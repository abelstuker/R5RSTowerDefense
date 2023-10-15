(#%require (only racket/base error))
(load "timer-adt.rkt")

(define (make-dropped-item position)
  (let ((type (get-random-element droppable-items))
         (dimensions *UNINITIALISED*)
         (timer (make-timer drop-item-time)))

    (define (set-dimensions! dims)
      (set! dimensions dims))

    (define (update! ms level)
      (if ((timer 'ended?))
          ((level 'remove-dropped-item!) dispatch)
          ((timer 'update!) ms)))

    (define (dispatch msg)
      (cond ((eq? msg 'update!) update!)
            ((eq? msg 'get-type) type)
            ((eq? msg 'get-position) position)
            ((eq? msg 'set-dimensions!) set-dimensions!)
            ((eq? msg 'get-dimensions) dimensions)
            (else (error 'dropped-item "message not understood: " msg))))
    dispatch))