(use-trait nft-trait .sip009-nft-trait.sip009-nft-trait)


(define-constant hoa-president tx-sender)

;;NFT ERRORS
(define-constant err-owner-only (err u100))
(define-constant err-not-token-owner (err u101))


;; Errors

(define-constant err-already-locked (err u102))
(define-constant err-more-votes-than-members-required (err u103))
(define-constant err-not-a-member (err u104))
(define-constant err-votes-required-not-met (err u105))

;; Variables
(define-data-var members (list 100 principal) (list))
(define-data-var votes-required uint u1)

(define-data-var deposit-nonce uint u0)

;; map
(define-map votes {member: principal, recipient: principal} {decision: bool})
(define-map deposits uint {member-principal: principal, token-id: uint})


;;list declaration



;; NFT functions




(define-non-fungible-token hoa-nft uint)

(define-data-var last-token-id uint u0)

(define-read-only (get-last-token-id)
    (ok (var-get last-token-id))
)

(define-read-only (get-token-uri (token-id uint))
    (ok none)
)

(define-read-only (get-owner (token-id uint))
    (ok (nft-get-owner? hoa-nft token-id))
)

(define-public (transfer (token-id uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender sender) err-not-token-owner)
        (nft-transfer? hoa-nft token-id sender recipient)
		;; (let ((deposit-id (var-get deposit-nonce)))
        ;; (map-set deposits deposit-id (merge {member-principal: recipient, token-id: token-id}))
		;; (var-set listing-nonce (+ listing-id u1))
		;; (ok listing-id)

		;; (ok (map-set deposits {member: recipient, token-id: token-id}))
		

		
		
		
    )
)

(define-public (mint (recipient principal))
    (let
        (
            (token-id (+ (var-get last-token-id) u1))
        )
        (asserts! (is-eq tx-sender hoa-president) err-owner-only)
        (try! (nft-mint? hoa-nft token-id recipient))
        (var-set last-token-id token-id)
        (ok token-id)
    )
)


;; Manual testing steps

;; (contract-call? .hoa-nft mint tx-sender)

;; (contract-call? .hoa-nft transfer u1 tx-sender 'ST1J4G6RR643BCG8G8SR6M2D9Z9KXT2NJDRK3FBTK)

;; (contract-call? .hoa-nft get-owner u1)

;;::set_tx_sender ST1J4G6RR643BCG8G8SR6M2D9Z9KXT2NJDRK3FBTK





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;multi-sig start

;; Implementing start

(define-public (start (new-members (list 100 principal)) (new-votes-required uint))
	(begin
		(asserts! (is-eq tx-sender hoa-president) err-owner-only)
		(asserts! (is-eq (len (var-get members)) u0) err-already-locked)
		(asserts! (>= (len new-members) new-votes-required) err-more-votes-than-members-required)
		(var-set members new-members)
		(var-set votes-required new-votes-required)
		(ok true)
	)
)

;; Implementing Vote

;; make sure the tx-sender is one of the members.

(define-public (vote (recipient principal) (decision bool))
	(begin
		(asserts! (is-some (index-of (var-get members) tx-sender)) err-not-a-member)
		(ok (map-set votes {member: tx-sender, recipient: recipient} {decision: decision}))
	)
)


;; read-only function to retrieve a vote. If a member never voted for a specific principal before, we will default to a negative vote of false.

;; Use map-get? to retrieve the vote tuple. The function will return a some or a none.
;; get returns the value of the specified key in a tuple. If get is supplied with a (some tuple), it will return a (some value). If get is supplied none, it returns none.
;; default-to attempts to unwrap the result of get. If it is a some, it returns the wrapped value. If it is none, it returns the default value, in this case false.

(define-read-only (get-vote (member principal) (recipient principal))
	(default-to false (get decision (map-get? votes {member: member, recipient: recipient})))
)


;; Tallying the votes


;; (fold accumulator-function input-list initial-value)  <--- accumulator signature

;; fold will iterate over input-list, calling accumulator-function for every element in the list. The accumulator function receives two parameters: the next member in the list and the previous accumulator value. The value returned by the accumulator function is used as the input for the next accumulator call.

;; Since we want to count the number of positive votes, we should increment the accumulator value only when the vote for the principal is true. There is no built-in function that can do that so we have to create a custom accumulator as a private function.

(define-private (tally (member principal) (accumulator uint))
	(if (get-vote member tx-sender) (+ accumulator u1) accumulator)
)

(define-read-only (tally-votes)
	(fold tally (var-get members) u0)
)

;; The tally-votes function returns the result of folding over the members list. Our custom accumulator function tally calls the get-vote read-only function we created earlier with the current current member from the list and the tx-sender. The result of this call will be either true or false. If the result is true, then tally returns the accumulator incremented by one. Otherwise, it returns just the current accumulator value.





;; Implementing withdraw


;; We have everything we need to create the withdraw function. It will tally the votes for tx-sender and check if it is larger than or equal to the number of votes required. If the transaction sender passes the bar, the contract shall transfer all its holdings to the tx-sender.

(define-public (withdraw)
	(let
		(
			(recipient tx-sender)
			(total-votes (tally-votes))
		)
		(asserts! (>= total-votes (var-get votes-required)) err-votes-required-not-met)
		(try! (as-contract (stx-transfer? (stx-get-balance tx-sender) tx-sender recipient)))
		(ok total-votes)
	)
)

;; Deposit convenience function

;; Finally, we will add a convenience function to deposit tokens into the contract. It is definitely not required as users can transfer tokens to a contract principal directly. The function will be useful when writing unit tests later.

(define-public (deposit (amount uint))
	(stx-transfer? amount tx-sender (as-contract tx-sender))
)

