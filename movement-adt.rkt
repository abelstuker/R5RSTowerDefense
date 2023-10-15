(load "constants-and-auxfunctions.rkt")

(define (make-movement speed total-x-distance total-y-distance)
  (let ((normalised *UNINITIALISED*)
        (x-distance *UNINITIALISED*)
        (y-distance *UNINITIALISED*))

    ; Normalising x- and y-values
    (define (normalise x y)
      (sqrt (+ (sqr x) (sqr y))))

    ; Updating the normalised, x- and y-distances needed for correct movement
    (define (update-movement!)
      (set! normalised (normalise total-x-distance total-y-distance))
      (set! x-distance (/ total-x-distance normalised))
      (set! y-distance (/ total-y-distance normalised)))

    (define (x-movement)
      (* x-distance speed))
    (define (y-movement)
      (* y-distance speed))

    ; Procedure to update the distances of movement. Also update-movement! is called to normalise them.
    (define (set-total-distances! new-x new-y)
      (set! total-x-distance new-x)
      (set! total-y-distance new-y)
      (update-movement!))

    ; Mirroring the current movement to the opposite direction
    (define (mirror-movement!)
      (when (and x-distance y-distance)
        (set! total-x-distance (- total-x-distance))
        (set! total-y-distance (- total-y-distance))
        (update-movement!)))

    ; Makes a new movement object where the movement is slowed down
    (define (slowed-down-movement)
      (make-movement (- speed slow-down-amount) total-x-distance total-y-distance))

    ; Makes a new movement object where the movement is sped up
    (define (sped-up-movement)
      (make-movement (+ speed speed-up-amount) total-x-distance total-y-distance))
    
    (define (dispatch msg)
      (cond
        ((eq? msg 'get-speed) speed)
        ((eq? msg 'get-x-movement) x-movement)
        ((eq? msg 'get-y-movement) y-movement)
        ((eq? msg 'set-total-distances!) set-total-distances!)
        ((eq? msg 'mirror-movement!) mirror-movement!)
        ((eq? msg 'slowed-down-movement) slowed-down-movement)
        ((eq? msg 'sped-up-movement) sped-up-movement)
        (else (display msg) (display "ERROR -- movement-adt dispatch message not understood"))))
    (update-movement!)
    dispatch))
