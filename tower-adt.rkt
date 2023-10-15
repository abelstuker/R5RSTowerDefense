(load "projectile-adt.rkt")
(load "constants-and-auxfunctions.rkt")
(load "timer-adt.rkt")

(define (make-tower position level type) 
  (let* ((range (tower-config-range type))
         (frequency (tower-config-frequency type))
         (projectile-speed (tower-config-projectile-speed type))
         (projectile-timer (make-timer projectile-initial-delay)) ; timer initialised with projectile-initial-delay (constant): the time to wait before the 1st projectile is launched
         (nearest-pathcell #f)
         (center-position ((position 'make-center-position))))


    ; Update function of the tower
    (define (update! ms monsters path)
      (if ((projectile-timer 'ended?)) ; checking whether it is time to fire or not
          (begin (cond ((or (= type 0) (= type 1))
                        (normal-projectile-fire ms monsters))
                       ((= type 2) (bomb-and-net-fire ms path monsters))
                       ((= type 3) (bomb-and-net-fire ms path monsters))
                       (else (display ("ERROR -- invalid tower type on update function"))))
                 ((projectile-timer 'set-time!) frequency))
          ((projectile-timer 'update!) ms)))

    ; Procedure that fires normal projectiles (i.e. projectiles from tower type 0 and 1: "Stenentoren en kanonskogeltoren")
    (define (normal-projectile-fire ms monsters)
      (let ((closest-monster (monster-in-range? monsters)))
        (when closest-monster
          (let ((margin (collision-margin closest-monster)))
            (fire! (+ (- ((closest-monster 'get-position) 'get-x) (center-position 'get-x)) (if (pair? margin) (car margin) 0))
                   (+ (- ((closest-monster 'get-position) 'get-y) (center-position 'get-y)) (if (pair? margin) (cdr margin) 0)))))))
    
    ; Procedure that is responsible for throwing bombs to the nearest pathcell
    (define (bomb-and-net-fire ms path monsters)
      (if (not nearest-pathcell)
          (set! nearest-pathcell ((path 'get-nearest-pathcell) position)))
      (if (monster-in-range? monsters)
          (fire! (- ((((nearest-pathcell 'get-position) 'make-center-position)) 'get-x) (center-position 'get-x))
                 (- ((((nearest-pathcell 'get-position) 'make-center-position)) 'get-y) (center-position 'get-y)))))

    (define (fire! x-dist y-dist)
      ((level 'add-projectile!) (make-projectile level
                                                 dispatch
                                                 ((position 'make-center-position))
                                                 (make-movement (tower-config-projectile-speed type)
                                                                x-dist
                                                                y-dist)
                                                 type)))
      
    
    ; Procedure returns the closest monster in range
    (define (monster-in-range? monsters)
      (let ((closest-distance 99999)
            (closest-monster #f))
        (for-each (lambda (monster)
                    (let* ((monster-position (monster 'get-position))
                           (distance ((center-position 'distance-to-other-position) monster-position))
                           ; Line-up monsters are monsters that are still moving outside the game field (in front of the first path tile)
                           ;   and may therefore not be attacked.
                           (line-up-monster? (< (monster-position 'get-x) 0))) 
                      (when (and (not line-up-monster?)
                                 (< distance range)
                                 (< distance closest-distance))
                        (set! closest-distance distance)
                        (set! closest-monster monster))))
                  monsters)
        closest-monster))

    ; Procedure to calculate the collision margin:
    (define (collision-margin monster)
      (let* ((monster-bullet-speed-ratio (/ ((monster 'get-movement) 'get-speed) projectile-speed))
             (margin (* collision-margin-coefficient monster-bullet-speed-ratio))
             (exit-direction (monster 'get-exit-direction)))
        (cond ((eq? exit-direction 'down) (cons 0 margin))
              ((eq? exit-direction 'up) (cons 0 (- margin)))
              ((eq? exit-direction 'right) (cons margin 0))
              (else (display "no valid exit direction for calculating collision margin") (newline) 0))))

  

    (define (dispatch msg)
      (cond ((eq? msg 'update!) update!)
            ((eq? msg 'get-position) position)
            ((eq? msg 'get-type) type)
            ((eq? msg 'get-nearest-pathcell) nearest-pathcell)
            (else (display ("ERROR -- tower-adt dispatch message not understood: ")) (display msg))))
  
    dispatch))