;; Blockchain Calendar Contract
;; Store and share events on the blockchain with immutable timestamps

;; Define event structure
(define-map events 
  {event-id: uint} 
  {
    creator: principal,
    title: (string-ascii 100),
    description: (string-ascii 500),
    event-date: uint,
    location: (string-ascii 200),
    is-public: bool,
    created-at: uint
  })

;; Track event counter and user events
(define-data-var event-counter uint u0)
(define-map user-events principal (list 50 uint))

;; Constants for error handling
(define-constant err-invalid-event-id (err u200))
(define-constant err-event-not-found (err u201))
(define-constant err-unauthorized (err u202))
(define-constant err-invalid-date (err u203))
(define-constant err-invalid-input (err u204))

;; Function 1: Create Event
;; Allows users to create and store events on the blockchain
(define-public (create-event 
  (title (string-ascii 100))
  (description (string-ascii 500))
  (event-date uint)
  (location (string-ascii 200))
  (is-public bool))
  (let (
    (new-event-id (+ (var-get event-counter) u1))
    (current-block-height block-height)
  )
  (begin
    ;; Validate inputs
    (asserts! (> (len title) u0) err-invalid-input)
    (asserts! (> event-date current-block-height) err-invalid-date)
    
    ;; Create the event
    (map-set events 
      {event-id: new-event-id}
      {
        creator: tx-sender,
        title: title,
        description: description,
        event-date: event-date,
        location: location,
        is-public: is-public,
        created-at: current-block-height
      })
    
    ;; Update user's event list
    (let ((current-events (default-to (list) (map-get? user-events tx-sender))))
      (map-set user-events tx-sender (unwrap! (as-max-len? (append current-events new-event-id) u50) err-invalid-input)))
    
    ;; Increment counter
    (var-set event-counter new-event-id)
    
    ;; Print event creation notification
    (print {
      action: "event-created",
      event-id: new-event-id,
      creator: tx-sender,
      title: title,
      event-date: event-date
    })
    
    (ok new-event-id))))

;; Function 2: Get Event Details
;; Retrieve event information with privacy controls
(define-read-only (get-event (event-id uint))
  (let ((event-data (map-get? events {event-id: event-id})))
    (match event-data
      event-info 
        (if (or 
              (get is-public event-info)
              (is-eq tx-sender (get creator event-info)))
          (ok (some event-info))
          (ok none)) ;; Return none if private and not creator
      (ok none)))) ;; Return none if event doesn't exist

;; Helper function: Get user's events
(define-read-only (get-user-events (user principal))
  (ok (default-to (list) (map-get? user-events user))))

;; Helper function: Get total events count
(define-read-only (get-total-events)
  (ok (var-get event-counter)))

;; Helper function: Check if event exists
(define-read-only (event-exists (event-id uint))
  (ok (is-some (map-get? events {event-id: event-id}))))