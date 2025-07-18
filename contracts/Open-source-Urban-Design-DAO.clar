(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-PROPOSAL-EXISTS (err u101))
(define-constant ERR-NO-PROPOSAL (err u102))
(define-constant ERR-ALREADY-VOTED (err u103))
(define-constant ERR-PROPOSAL-EXPIRED (err u104))
(define-constant ERR-INSUFFICIENT-FUNDS (err u105))
(define-constant ERR-NOT-ACTIVE (err u106))

(define-data-var proposal-count uint u0)
(define-data-var min-proposal-amount uint u100)
(define-data-var voting-period uint u1440)

(define-map proposals 
    uint 
    {
        creator: principal,
        title: (string-ascii 100),
        description: (string-ascii 500),
        budget: uint,
        votes: uint,
        status: (string-ascii 20),
        deadline: uint,
        bounty: uint
    }
)

(define-map votes 
    { proposal-id: uint, voter: principal } 
    bool
)

(define-map user-tokens principal uint)

(define-public (initialize-token (amount uint))
    (begin
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set user-tokens tx-sender amount)
        (ok true)
    )
)

(define-public (create-proposal 
    (title (string-ascii 100))
    (description (string-ascii 500))
    (budget uint)
    (bounty uint)
)
    (let
        (
            (proposal-id (+ (var-get proposal-count) u1))
            (current-time burn-block-height)
            (deadline (+ burn-block-height (var-get voting-period)))
        )
        (asserts! (>= budget (var-get min-proposal-amount)) ERR-INSUFFICIENT-FUNDS)
        (try! (stx-transfer? budget tx-sender (as-contract tx-sender)))
        (map-set proposals proposal-id {
            creator: tx-sender,
            title: title,
            description: description,
            budget: budget,
            votes: u0,
            status: "active",
            deadline: deadline,
            bounty: bounty
        })
        (var-set proposal-count proposal-id)
        (ok proposal-id)
    )
)

(define-public (vote (proposal-id uint))
    (let
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR-NO-PROPOSAL))
            (has-voted (default-to false (map-get? votes {proposal-id: proposal-id, voter: tx-sender})))
            (user-balance (default-to u0 (map-get? user-tokens tx-sender)))
        )
        (asserts! (not has-voted) ERR-ALREADY-VOTED)
        (asserts! (> user-balance u0) ERR-INSUFFICIENT-FUNDS)
        (asserts! (< burn-block-height (get deadline proposal)) ERR-PROPOSAL-EXPIRED)
        (asserts! (is-eq (get status proposal) "active") ERR-NOT-ACTIVE)
        
        (map-set proposals proposal-id 
            (merge proposal {votes: (+ (get votes proposal) u1)})
        )
        (map-set votes {proposal-id: proposal-id, voter: tx-sender} true)
        (map-set user-tokens tx-sender (- user-balance u1))
        (ok true)
    )
)

(define-public (finalize-proposal (proposal-id uint))
    (let
        (
            (proposal (unwrap! (map-get? proposals proposal-id) ERR-NO-PROPOSAL))
        )
        (asserts! (>= burn-block-height (get deadline proposal)) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status proposal) "active") ERR-NOT-ACTIVE)
        
        (if (>= (get votes proposal) u10)
            (begin
                (try! (stx-transfer? 
                    (get bounty proposal)
                    (as-contract tx-sender)
                    (get creator proposal)
                ))
                (map-set proposals proposal-id 
                    (merge proposal {status: "completed"})
                )
                (ok true)
            )
            (begin
                (try! (stx-transfer? 
                    (get budget proposal)
                    (as-contract tx-sender)
                    (get creator proposal)
                ))
                (map-set proposals proposal-id 
                    (merge proposal {status: "rejected"})
                )
                (ok true)
            )
        )
    )
)

(define-read-only (get-proposal (proposal-id uint))
    (ok (unwrap! (map-get? proposals proposal-id) ERR-NO-PROPOSAL))
)

(define-read-only (get-user-balance (user principal))
    (ok (default-to u0 (map-get? user-tokens user)))
)