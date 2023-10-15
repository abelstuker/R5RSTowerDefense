
(define (make-tower-picker-item tower-name tower-image)
  (let ((position #f)) ; The position of the tower picker item is only determined by drawing, and therefore set by the Screen ADT
    
  (define (set-position! new-position)
    (set! position new-position))

  (define (dispatch msg)
    (cond ((eq? msg 'get-name) tower-name)
          ((eq? msg 'get-image) tower-image)
          ((eq? msg 'get-position) position)
          ((eq? msg 'set-position!) set-position!)))
  dispatch))