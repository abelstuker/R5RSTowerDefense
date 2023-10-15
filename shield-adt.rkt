(load "timer-adt.rkt")

(define (make-shield)
  (let ((active? #t)
        (timer (make-timer 0))
        (times-unshielded 0))

    (define (update! ms)
      (cond ((and (< times-unshielded 3)
                  (not active?)
                  ((timer 'ended?)))
             (set! active? #t))
            ((and (< times-unshielded 3)
                  (not active?))
             ((timer 'update!) ms))))

    (define (hit-shield!)
      (when active?
        (set! active? #f)
        (set! times-unshielded (+ times-unshielded 1))
        (if (< times-unshielded 3)
            ((timer 'set-time!) temporary-unshield-time))))

    (define (dispatch msg)
      (cond ((eq? msg 'update!) update!)
            ((eq? msg 'hit-shield!) hit-shield!)
            ((eq? msg 'is-active?) active?)
            (else (display msg) (display "ERROR message not understood by SHIELD"))))
    dispatch))