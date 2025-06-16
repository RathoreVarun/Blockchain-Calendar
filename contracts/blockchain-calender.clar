;; Store latest event per user
(define-map user-calendar
  {user: principal}  ;; key
  {date: uint, description: (string-ascii 100)}) ;; value

(define-constant err-empty-desc (err u100))
(define-constant err-invalid-date (err u101))

;; Add an event to user's calendar
(define-public (save-event (date uint) (description (string-ascii 100)))
  (begin
    (asserts! (> date u0) err-invalid-date)
    (asserts! (> (len description) u0) err-empty-desc)
    (map-set user-calendar {user: tx-sender} {date: date, description: description})
    (ok true)))

;; Read-only function to view latest user event
(define-read-only (get-my-event)
  (let ((entry (map-get? user-calendar {user: tx-sender})))
    (ok (match entry
         val (some {date: (get date val), description: (get description val)})
         none))))
