(load "screen-adt.rkt")
(load "position-adt.rkt")

(define (make-pathcell entry exit position is-first? is-last?)
  (let ((slowed? #f)
        (active-obstacle? #f))

    ; Comparing two pathcells' positions
    (define (same-pathcell? other-pathcell)
      (((other-pathcell 'get-position) 'compare?) position))

    (define (set-slowed! staat)
      (set! slowed? staat))
    (define (unset-slowed!)
      (set! slowed? #f))

    (define (set-active-obstacle! bool)
      (set! active-obstacle? bool))
    
    (define (dispatch msg)
      (cond ((eq? msg 'get-position) position)
            ((eq? msg 'get-entry) entry)
            ((eq? msg 'get-exit) exit)
            ((eq? msg 'is-first?) is-first?)
            ((eq? msg 'is-last?) is-last?)
            ((eq? msg 'is-slowed?) slowed?)
            ((eq? msg 'set-slowed!) set-slowed!)
            ((eq? msg 'has-active-obstacle?) active-obstacle?)
            ((eq? msg 'set-active-obstacle!) set-active-obstacle!)
            (else (display "ERROR -- pathcell-adt dispatch message not understood: ") (display msg))))
    dispatch))