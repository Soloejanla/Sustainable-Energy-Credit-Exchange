;; Scholarly Research Validation Network: Decentralized academic paper review system
;; Enables researchers to submit papers, reviewers to evaluate, and institutions to certify quality

(define-data-var chief-editor principal tx-sender)

(define-map manuscript-registry
  { manuscript-id: uint }
  {
    author: principal,
    review-fee: uint,
    paper-title: (string-ascii 50),
    abstract-content: (string-ascii 500),
    review-period: uint,
    certified: bool
  })

(define-map review-submissions
  { manuscript-id: uint, submission-id: uint }
  {
    reviewer: principal,
    submission-date: uint,
    evaluation-status: (string-ascii 20)
  })

(define-data-var next-manuscript-id uint u1)

(define-map submission-tracker
  { manuscript-id: uint }
  { total-submissions: uint })

;; Submit a research paper for review
(define-public (submit-manuscript (title-input (string-ascii 50)) (abstract-input (string-ascii 500)) (period-input uint) (fee-input uint))
  (let
    (
      (manuscript-id (var-get next-manuscript-id))
      (submission-id u0)
      (title title-input)
      (abstract abstract-input)
      (period period-input)
      (fee fee-input)
    )
    ;; Input validation
    (asserts! (> fee u0) (err u1))
    (asserts! (> (len title) u0) (err u5))
    (asserts! (> (len abstract) u0) (err u6))
    (asserts! (> period u0) (err u7))
    
    (map-set manuscript-registry
      { manuscript-id: manuscript-id }
      {
        author: tx-sender,
        review-fee: fee,
        paper-title: title,
        abstract-content: abstract,
        review-period: period,
        certified: false
      })
    
    (map-set review-submissions
      { manuscript-id: manuscript-id, submission-id: submission-id }
      {
        reviewer: tx-sender,
        submission-date: manuscript-id,
        evaluation-status: "submitted"
      })
    
    (map-set submission-tracker
      { manuscript-id: manuscript-id }
      { total-submissions: u1 })
    
    (var-set next-manuscript-id (+ manuscript-id u1))
    (ok manuscript-id)
  ))

;; Review a submitted manuscript
(define-public (review-manuscript (manuscript-id-input uint))
  (let
    (
      (manuscript-id manuscript-id-input)
      (manuscript-info (unwrap! (map-get? manuscript-registry { manuscript-id: manuscript-id }) (err u2)))
      (fee (get review-fee manuscript-info))
      (author (get author manuscript-info))
      (tracker-data (default-to { total-submissions: u0 } (map-get? submission-tracker { manuscript-id: manuscript-id })))
      (submission-id (get total-submissions tracker-data))
      (new-submission-id (+ submission-id u1))
    )
    ;; Input validation
    (asserts! (> manuscript-id u0) (err u8))
    (asserts! (not (is-eq tx-sender author)) (err u3))
    
    (try! (stx-transfer? fee tx-sender author))
    
    (map-set review-submissions
      { manuscript-id: manuscript-id, submission-id: submission-id }
      {
        reviewer: tx-sender,
        submission-date: (var-get next-manuscript-id),
        evaluation-status: "reviewed"
      })
    
    (map-set submission-tracker
      { manuscript-id: manuscript-id }
      { total-submissions: new-submission-id })
    
    (ok true)
  ))

;; Certify manuscript quality (chief editor only)
(define-public (certify-manuscript (manuscript-id-input uint))
  (let
    (
      (manuscript-id manuscript-id-input)
      (manuscript-info (unwrap! (map-get? manuscript-registry { manuscript-id: manuscript-id }) (err u2)))
      (tracker-data (default-to { total-submissions: u0 } (map-get? submission-tracker { manuscript-id: manuscript-id })))
      (submission-id (get total-submissions tracker-data))
      (new-submission-id (+ submission-id u1))
    )
    ;; Input validation
    (asserts! (> manuscript-id u0) (err u8))
    (asserts! (is-eq tx-sender (var-get chief-editor)) (err u4))
    
    (map-set manuscript-registry
      { manuscript-id: manuscript-id }
      (merge manuscript-info { certified: true }))
    
    (map-set review-submissions
      { manuscript-id: manuscript-id, submission-id: submission-id }
      {
        reviewer: (get author manuscript-info),
        submission-date: (var-get next-manuscript-id),
        evaluation-status: "certified"
      })
    
    (map-set submission-tracker
      { manuscript-id: manuscript-id }
      { total-submissions: new-submission-id })
    
    (ok true)
  ))

;; Get manuscript details
(define-read-only (get-manuscript (manuscript-id uint))
  (map-get? manuscript-registry { manuscript-id: manuscript-id }))

;; Get review submission entry
(define-read-only (get-review-submission (manuscript-id uint) (submission-id uint))
  (map-get? review-submissions { manuscript-id: manuscript-id, submission-id: submission-id }))

;; Get total review submissions for a manuscript
(define-read-only (get-submission-count (manuscript-id uint))
  (let
    (
      (tracker-data (default-to { total-submissions: u0 } (map-get? submission-tracker { manuscript-id: manuscript-id })))
    )
    (get total-submissions tracker-data)
  ))
