(load "constants-and-auxfunctions.rkt")

(define (make-obstacle pathcell orientation)
  (let ((position (pathcell 'get-position))
        (dimensions *UNINITIALISED*))

    (define (set-dimensions! dims)
      (set! dimensions dims))

    (define (dispatch msg)
      (cond ((eq? msg 'get-position) position)
            ((eq? msg 'get-orientation) orientation)
            ((eq? msg 'set-dimensions!) set-dimensions!)
            ((eq? msg 'get-dimensions) dimensions)
            (else (display msg) (display " ERROR -- message not understood by TEMPORARY OBSTACLE"))))
    dispatch))