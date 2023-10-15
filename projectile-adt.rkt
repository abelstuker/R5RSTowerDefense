(#%require (only racket/base error))
(load "constants-and-auxfunctions.rkt")
(load "timer-adt.rkt")

(define (make-projectile level tower position movement type)
  (let ((dimensions *UNINITIALISED*)
        (distance-from-tower 0)
        (delay-timer (make-timer (cond ((= type 2) bomb-delay-time)
                                       ((= type 3) slow-down-time)
                                       (else 0))))
        (check-collision? (if (or (= type 0) (= type 1)) #t #f))
        (returning-to-tower? #f)
        (in-slowed-state? #f))

    ; Update function of the projectile
    (define (update! ms monsters screen)
      
      (cond ((or (= type 0) (= type 1))
             (if (>= distance-from-tower (tower-config-range type))
                 (delete-projectile!)
                 (move-projectile! movement ms screen monsters)))
            ((= type 2)
             (cond ((<= distance-from-tower 1) (move-projectile! movement ms screen monsters))
                   ((not ((delay-timer 'ended?))) ((delay-timer 'update!) ms))
                   (else (explode! monsters))))
            ((= type 3)
             (cond ((or (and (not returning-to-tower?)
                             (< distance-from-tower 1))
                        (and returning-to-tower?
                             (>= distance-from-tower 0)))  ; when the net is moving from or back to the tower
                    (move-projectile! movement ms screen monsters))
                   (returning-to-tower? ; when the net is fully returned to the tower
                    (delete-projectile!))
                   ((and (not in-slowed-state?)
                         (not returning-to-tower?)) ; when the net has arrived at the pathcell
                    (set-slowed-path!)
                    ((delay-timer 'update!) ms)
                    (set! in-slowed-state? #t))
                   ((not ((delay-timer 'ended?))) ; when the net is staying at the pathcell
                    ((delay-timer 'update!) ms))
                   (else ; when the net has stayed at the pathcell for long enough
                    ((delay-timer 'set-time!) ms)
                    (return-to-tower!))))
            (else (error "invalid tower type on update function"))))

    ; The collision? procedure checks, given a list of monsters, whether the current projectile collides with any of those monsters.
    ; In that case it returns the monster itself, otherwise it returns #f.
    (define (collision? monsters)
      (define (check-list monsters)
        (if (null? monsters)
            #f
            (let* ((monster (car monsters))
                   (monster-position (monster 'get-position))
                   (monster-x (monster-position 'get-x))
                   (monster-y (monster-position 'get-y))
                   (monster-dimensions (monster 'get-dimensions))
                   (monster-w (monster-dimensions 'get-w))
                   (monster-h (monster-dimensions 'get-h))
                   (projectile-position position)
                   (projectile-x (projectile-position 'get-x))
                   (projectile-y (projectile-position 'get-y)))
              (if (and (> (+ projectile-x (/ (dimensions 'get-w) 2))
                          monster-x) 
                       (< (- projectile-x (/ (dimensions 'get-h) 2))
                          (+ monster-w monster-x))
                       (> (+ projectile-y (/ (dimensions 'get-w) 2))
                          monster-y)
                       (< (- projectile-y (/ (dimensions 'get-h) 2))
                          (+ monster-h monster-y)))
                  monster
                  (check-list (cdr monsters))))))
      (if (eq? *UNINITIALISED* dimensions)
          #f
          (check-list monsters)))

    (define (explode! monsters)
      (for-each (lambda (monster) (if (<= ((position 'distance-to-other-position) (monster 'get-position)) bomb-explosion-range)
                                      ((monster 'hurt!) bomb-damage monsters #f #t)))
                monsters)
      (delete-projectile!))
    
    (define (delete-projectile!)
      ((level 'remove-projectile!) dispatch))
    
    (define (move-projectile! movement ms screen monsters)
      (when (or (not check-collision?)
                (let ((hit-monster (collision? monsters)))
                  (if hit-monster (collide! hit-monster monsters))  ; When a monster is hit, the collide! procedure is executed.
                  (not hit-monster)))
        (set! distance-from-tower ((if returning-to-tower? - +) distance-from-tower ((position 'move!) movement ms))) ; add the elapsed distance in the current update time interval to the total distance-from-tower
        ((screen 'draw-game-element!) 'projectile (list dispatch (tower-config-projectile-scale? type))))) ; ... and draw the projectile again


    ; Procedure called when a 'net' has arrived (i.e. distance-from-tower >= 1) and the underlying pathcell should be informed to slow down its monsters.
    (define (set-slowed-path!)
      (((tower 'get-nearest-pathcell) 'set-slowed!) #t))
    ; Procedure called when a 'net' disappears and therefore the underlying pathcell should stop slowing down its monsters.
    (define (unset-slowed-path!)
      (((tower 'get-nearest-pathcell) 'set-slowed!) #f))

    (define (return-to-tower!)
      (unset-slowed-path!) 
      ((movement 'mirror-movement!))
      (set! returning-to-tower? #t)
      (set! in-slowed-state? #f))
      
    ; The collide! executes the collision by hurting the monster and removing the projectile, and returns whether the projectile has to remain drawed.
    (define (collide! monster all-monsters)
      ((monster 'hurt!) 1 all-monsters (= type 1) #t) ;(tower-config-projectile-damage type))
      ; TODO check for slow down or move back
      (delete-projectile!))


    (define (set-dimensions! dims)
      (set! dimensions dims))

    (define (dispatch msg)
      (cond ((eq? msg 'update!) update!)
            ((eq? msg 'get-position) position)
            ((eq? msg 'get-type) type)
            ((eq? msg 'collide!) collide!)
            ((eq? msg 'get-scale) distance-from-tower)
            ((eq? msg 'set-dimensions!) set-dimensions!)
            (else (error msg "message not understood by projectile"))))
    dispatch))