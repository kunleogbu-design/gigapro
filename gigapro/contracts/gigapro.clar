;; QuantumProposal - Advanced Quadratic Voting DAO Governance Platform

;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-BALANCE (err u101))
(define-constant ERR-PROPOSAL-NOT-FOUND (err u102))
(define-constant ERR-VOTING-PERIOD-ENDED (err u103))
(define-constant ERR-INVALID-PROPOSAL-TYPE (err u104))
(define-constant ERR-ALREADY-VOTED (err u105))
(define-constant ERR-PROPOSAL-NOT-ACTIVE (err u106))
(define-constant ERR-INSUFFICIENT-VOTING-POWER (err u107))
(define-constant ERR-MILESTONE-NOT-FOUND (err u108))
(define-constant ERR-ORACLE-VERIFICATION-FAILED (err u109))
(define-constant ERR-SUPERMAJORITY-REQUIRED (err u110))
(define-constant ERR-TIME-LOCK-ACTIVE (err u111))
(define-constant ERR-INVALID-AMOUNT (err u112))
(define-constant ERR-REPUTATION-TOO-LOW (err u113))

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant MICRO-GRANT-THRESHOLD u1000)
(define-constant SUPERMAJORITY-THRESHOLD u75)
(define-constant TIME-LOCK-PERIOD u144) ;; blocks
(define-constant MAX-VOTING-PERIOD u1008) ;; blocks
(define-constant QUADRATIC-SCALING-FACTOR u10000)

;; Data variables
(define-data-var proposal-counter uint u0)
(define-data-var total-treasury-balance uint u0)
(define-data-var oracle-address principal CONTRACT-OWNER)
(define-data-var minimum-reputation-score uint u10)
(define-data-var ai-predictor-enabled bool true)

;; Proposal types
(define-constant PROPOSAL-TYPE-MICRO u1)
(define-constant PROPOSAL-TYPE-STANDARD u2)
(define-constant PROPOSAL-TYPE-MEGA u3)

;; Proposal status
(define-constant STATUS-ACTIVE u1)
(define-constant STATUS-PASSED u2)
(define-constant STATUS-REJECTED u3)
(define-constant STATUS-EXECUTED u4)

;; Data maps
(define-map proposals
    { proposal-id: uint }
    {
        proposer: principal,
        title: (string-ascii 100),
        description: (string-ascii 500),
        amount-requested: uint,
        proposal-type: uint,
        status: uint,
        votes-for: uint,
        votes-against: uint,
        voting-end-block: uint,
        time-lock-end: uint,
        ai-feasibility-score: uint,
        execution-block: uint,
        category: (string-ascii 50)
    }
)

(define-map user-votes
    { proposal-id: uint, voter: principal }
    {
        voting-power-used: uint,
        vote-direction: bool,
        tokens-committed: uint,
        conviction-weight: uint
    }
)

(define-map user-reputation
    { user: principal }
    {
        reputation-score: uint,
        successful-votes: uint,
        total-votes: uint,
        conviction-power: uint,
        category-expertise: (string-ascii 50)
    }
)

(define-map proposal-milestones
    { proposal-id: uint, milestone-id: uint }
    {
        description: (string-ascii 200),
        amount: uint,
        completed: bool,
        verified-by-oracle: bool,
        completion-block: uint
    }
)

(define-map escrow-accounts
    { proposal-id: uint }
    {
        total-amount: uint,
        released-amount: uint,
        milestones-count: uint,
        beneficiary: principal
    }
)

(define-map conviction-voting
    { user: principal, category: (string-ascii 50) }
    {
        accumulated-power: uint,
        last-activity-block: uint,
        consistency-score: uint
    }
)

(define-map cross-chain-resources
    { proposal-id: uint, chain-id: (string-ascii 20) }
    {
        contributed-amount: uint,
        chain-address: (string-ascii 100),
        verified: bool
    }
)

;; Token balance tracking
(define-map token-balances { user: principal } { balance: uint })

;; Administrative functions
(define-public (set-oracle-address (new-oracle principal))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set oracle-address new-oracle)
        (ok true)
    )
)

(define-public (set-minimum-reputation (new-min uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set minimum-reputation-score new-min)
        (ok true)
    )
)

(define-public (toggle-ai-predictor)
    (begin
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (var-set ai-predictor-enabled (not (var-get ai-predictor-enabled)))
        (ok true)
    )
)

;; Core proposal creation
(define-public (create-proposal 
    (title (string-ascii 100))
    (description (string-ascii 500))
    (amount-requested uint)
    (category (string-ascii 50)))
    (let (
        (proposal-id (+ (var-get proposal-counter) u1))
        (user-rep (get-user-reputation tx-sender))
        (proposal-type (determine-proposal-type amount-requested))
        (voting-period (calculate-voting-period proposal-type))
        (ai-score (if (var-get ai-predictor-enabled) 
                     (calculate-ai-feasibility-score amount-requested category)
                     u50))
    )
        (asserts! (>= (get reputation-score user-rep) (var-get minimum-reputation-score)) ERR-REPUTATION-TOO-LOW)
        (asserts! (> amount-requested u0) ERR-INVALID-AMOUNT)
        
        (map-set proposals { proposal-id: proposal-id }
            {
                proposer: tx-sender,
                title: title,
                description: description,
                amount-requested: amount-requested,
                proposal-type: proposal-type,
                status: STATUS-ACTIVE,
                votes-for: u0,
                votes-against: u0,
                voting-end-block: (+ block-height voting-period),
                time-lock-end: (if (is-eq proposal-type PROPOSAL-TYPE-MEGA)
                                  (+ block-height TIME-LOCK-PERIOD)
                                  block-height),
                ai-feasibility-score: ai-score,
                execution-block: u0,
                category: category
            }
        )
        
        ;; Create escrow account
        (map-set escrow-accounts { proposal-id: proposal-id }
            {
                total-amount: amount-requested,
                released-amount: u0,
                milestones-count: u0,
                beneficiary: tx-sender
            }
        )
        
        (var-set proposal-counter proposal-id)
        (ok proposal-id)
    )
)

;; Quadratic voting implementation
(define-public (vote-on-proposal 
    (proposal-id uint)
    (vote-for bool)
    (tokens-committed uint))
    (let (
        (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
        (user-rep (get-user-reputation tx-sender))
        (user-balance (default-to u0 (get balance (map-get? token-balances { user: tx-sender }))))
        (quadratic-power (calculate-quadratic-voting-power tokens-committed))
        (conviction-bonus (get-conviction-bonus tx-sender (get category proposal)))
        (total-voting-power (+ quadratic-power conviction-bonus))
    )
        (asserts! (is-eq (get status proposal) STATUS-ACTIVE) ERR-PROPOSAL-NOT-ACTIVE)
        (asserts! (<= block-height (get voting-end-block proposal)) ERR-VOTING-PERIOD-ENDED)
        (asserts! (>= user-balance tokens-committed) ERR-INSUFFICIENT-BALANCE)
        (asserts! (is-none (map-get? user-votes { proposal-id: proposal-id, voter: tx-sender })) ERR-ALREADY-VOTED)
        (asserts! (> total-voting-power u0) ERR-INSUFFICIENT-VOTING-POWER)
        
        ;; Record vote
        (map-set user-votes { proposal-id: proposal-id, voter: tx-sender }
            {
                voting-power-used: total-voting-power,
                vote-direction: vote-for,
                tokens-committed: tokens-committed,
                conviction-weight: conviction-bonus
            }
        )
        
        ;; Update proposal vote counts
        (if vote-for
            (map-set proposals { proposal-id: proposal-id }
                (merge proposal { votes-for: (+ (get votes-for proposal) total-voting-power) }))
            (map-set proposals { proposal-id: proposal-id }
                (merge proposal { votes-against: (+ (get votes-against proposal) total-voting-power) }))
        )
        
        ;; Update conviction voting
        (update-conviction-voting tx-sender (get category proposal))
        
        ;; Lock tokens
        (map-set token-balances { user: tx-sender }
            { balance: (- user-balance tokens-committed) }
        )
        
        (ok true)
    )
)

;; Execute approved proposals
(define-public (execute-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (map-get? proposals { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
        (escrow (unwrap! (map-get? escrow-accounts { proposal-id: proposal-id }) ERR-PROPOSAL-NOT-FOUND))
        (total-votes (+ (get votes-for proposal) (get votes-against proposal)))
        (approval-rate (if (> total-votes u0)
                          (/ (* (get votes-for proposal) u100) total-votes)
                          u0))
    )
        (asserts! (is-eq (get status proposal) STATUS-ACTIVE) ERR-PROPOSAL-NOT-ACTIVE)
        (asserts! (> block-height (get voting-end-block proposal)) ERR-VOTING-PERIOD-ENDED)
        (asserts! (> block-height (get time-lock-end proposal)) ERR-TIME-LOCK-ACTIVE)
        
        ;; Check approval thresholds based on proposal type
        (if (is-eq (get proposal-type proposal) PROPOSAL-TYPE-MICRO)
            (asserts! (>= approval-rate u51) ERR-INSUFFICIENT-VOTING-POWER)
            (if (is-eq (get proposal-type proposal) PROPOSAL-TYPE-MEGA)
                (asserts! (>= approval-rate SUPERMAJORITY-THRESHOLD) ERR-SUPERMAJORITY-REQUIRED)
                (asserts! (>= approval-rate u51) ERR-INSUFFICIENT-VOTING-POWER)
            )
        )
        
        ;; Update proposal status
        (map-set proposals { proposal-id: proposal-id }
            (merge proposal { 
                status: STATUS-EXECUTED,
                execution-block: block-height 
            })