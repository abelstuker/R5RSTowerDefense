(load "movement-adt.rkt")
(load "shield-adt.rkt")
(load "dropped-item-adt.rkt")

(define (make-monster initial-pathcell level type . args)
  (define gamefield-entry-position (make-position  0 
                                                   (+ ((initial-pathcell 'get-position) 'get-y)
                                                      (* (random) (- 1 (/ monster-height cell-height))))))
  (let ((position (make-position  (if (and (not (null? args)) (car args)) 0 (* (- monster-spread-at-start) (random)))
                                  (gamefield-entry-position 'get-y)))
        (movement (make-movement (monster-config-speed type) 1 0))
        (underlying-pathcell *UNINITIALISED*)
        (dimensions *UNINITIALISED*)
        (entry-direction 'left)
        (exit-position gamefield-entry-position)
        (exit-direction *UNINITIALISED*)
        (previous-turning-points '())
        (lives (monster-config-lives type))
        (path-index 0)
        (distance-to-pathcell-check 0)
        (shield (if (= type 2) (make-shield) #f))
        (going-back? #f)
        (overridden-tanks (make-object-list)))

    ; Update function for the monster
    (define (update! ms path screen obstacles)

      (cond ((and (< (position 'get-x) 0)
                  (not going-back?)
                  (or (< ((movement 'get-x-movement)) 0)
                      (not (= ((movement 'get-y-movement)) 0))))
             (set! movement (make-movement (monster-config-speed type) 1 0)))
            ((and (>= (position 'get-x) 0) (<= distance-to-pathcell-check 0))
             (update-underlying-pathcell! path going-back?)
             (set! going-back? #f))
            ((<= distance-to-pathcell-check 0) (set! going-back? #f)))

      (if (not (and obstacles (obstacle-collision? obstacles)))
          (set! distance-to-pathcell-check (- distance-to-pathcell-check ((position 'move!) (cond (going-back?
                                                                                                   ((movement 'sped-up-movement)))
                                                                                                  ((and underlying-pathcell
                                                                                                        (underlying-pathcell 'is-slowed?))
                                                                                                   ((movement 'slowed-down-movement)))
                                                                                                  (else movement))
                                                                                            ms)))) ; Moving and updating the distance until the underlying pathcell needs to be rechecked
      (if shield ((shield 'update!) ms))
      (if (< (position 'get-grid-x) horizontal-cells-amount)
          ((screen 'draw-game-element!) 'monster (list dispatch))
          (let ((game (level 'get-game))) 
            ((game 'set-lives!) (- (game 'get-lives) (monster-config-damage type)))
            ((level 'remove-monster!) dispatch))))

    (define (obstacle-collision? obstacles)
      (define (check-vec index)
        (if (<= (vector-length obstacles) index)
            #f
            (let* ((obstacle (vector-ref obstacles index))
                   (obstacle-position (obstacle 'get-position))
                   (obstacle-x (obstacle-position 'get-x))
                   (obstacle-y (obstacle-position 'get-y))
                   (obstacle-dimensions (obstacle 'get-dimensions))
                   (obstacle-w (obstacle-dimensions 'get-w))
                   (obstacle-h (obstacle-dimensions 'get-h))
                   (monster-position position)
                   (monster-x (monster-position 'get-x))
                   (monster-y (monster-position 'get-y)))
              (if (and (> (+ monster-x (dimensions 'get-w))
                          obstacle-x) 
                       (< monster-x
                          (+ obstacle-w obstacle-x))
                       (> (+ monster-y (dimensions 'get-h))
                          obstacle-y)
                       (< monster-y
                          (+ obstacle-h obstacle-y)))
                  obstacle
                  (check-vec (+ index 1))))))
      (if (eq? *UNINITIALISED* dimensions)
          #f
          (check-vec 0)))
            
  
    ; This procedure checks wheter the monster is moving over a new pathcell compared to the previous check.
    ; If the underlying pathcell has indeed changed, it sets the underlying-pathcell to the current pathcell and
    ;   and it calls generate-exit-position!
    (define (update-underlying-pathcell! path just-fired-back?)
      
      (let* ((detected-pathcell ((position 'object-in-position?) (path 'get-path-vector)))
             (target-pathcell (if detected-pathcell detected-pathcell ((path 'get-nearest-pathcell) position))))
        (when (and target-pathcell
                   (or just-fired-back?
                       (not underlying-pathcell)
                       (not (equal? underlying-pathcell target-pathcell))
                       (not exit-position)
                       (not exit-direction)))
          (set! path-index (+ path-index 1))
          (set! underlying-pathcell target-pathcell)
          (if (not just-fired-back?) (update-turning-points!))
          (generate-exit-position!))))

    (define (update-turning-points!)
      (if (not (eq? *UNINITIALISED* exit-position))
          (set! previous-turning-points (cons exit-position previous-turning-points))))
    
    ; Generating an exit position based on the underlying pathcell.
    (define (generate-exit-position!)
      (if underlying-pathcell
      (let* ((exit (underlying-pathcell 'get-exit))
             (entry (underlying-pathcell 'get-entry))
             (x (position 'get-x))
             (y (position 'get-y))
             (x-grid (position 'get-grid-x))
             (y-grid (position 'get-grid-y))
             (new-exit-position (cond
                                  ; from LEFT to UP
                                  ((and (eq? exit 'up)
                                        (eq? entry 'left))
                                   (make-position (+ x-grid (* (random) (- 1 (/ monster-width cell-width)))) y-grid))

                                  ; from DOWN to UP
                                  ((eq? exit 'up) 
                                   (make-position x y-grid))

                                  ; from LEFT to DOWN
                                  ((and (eq? exit 'down)
                                        (eq? entry 'left))
                                   (make-position (+ x-grid (* (random) (- 1 (/ monster-width cell-width)))) (+ y-grid 1)))

                                  ; from UP to DOWN
                                  ((eq? exit 'down) 
                                   (make-position x (+ y-grid 1)))

                                  ; from LEFT to RIGHT
                                  ((and (eq? exit 'right)
                                        (eq? entry 'left))
                                   (make-position (+ x-grid 1) y))

                                  ; from DOWN to RIGHT
                                  ((and (eq? exit 'right)
                                        (eq? entry 'down))
                                   (make-position (+ x-grid 1) (+ y-grid (* (random) (- 1 (/ monster-height cell-height))))))

                                  ; from UP to RIGHT
                                  ((and (eq? exit 'right)
                                        (eq? entry 'up))
                                   (make-position (+ x-grid 1) (+ y-grid (* (random) (- 1 (/ monster-height cell-height)))))) ;(+ 0.1 (* (random) (- 0.9 (/ monster-height cell-height))))))
                                      
                                  (else (display "ERROR -- generate-exit-position! unexpected exit: ") (display (symbol->string exit)) (newline)))))
        (set! entry-direction entry)
        (set! exit-direction exit)
        (set! exit-position new-exit-position)
        
        (set! distance-to-pathcell-check ((position 'distance-to-other-position) new-exit-position))
        ((movement 'set-total-distances!) (- (new-exit-position 'get-x) x) (- (new-exit-position 'get-y) y)))))

    (define (get-fired-back!)
      (if (not going-back?)
          (let* ((go-back-position (car previous-turning-points))
                 (self-x (position 'get-x))
                 (self-y (position 'get-y)))
            (set! going-back? #t)
            (set! exit-position go-back-position)
            (if (not (null? (cdr previous-turning-points))) (set! previous-turning-points (cdr previous-turning-points)))
            (set! distance-to-pathcell-check fired-back-distance)
            (cond ((< ((position 'distance-to-other-position) exit-position) 0.1)
                   ((movement 'set-total-distances!) (- (go-back-position 'get-x) self-x) (- (go-back-position 'get-y) self-y)))
                  ((and (eq? exit-direction 'right) (eq? entry-direction 'up))
                   ((movement 'set-total-distances!) 0 -1))
                  ((and (eq? exit-direction 'right) (eq? entry-direction 'down))
                   ((movement 'set-total-distances!) 0 1))
                  ((and (eq? exit-direction 'right) (eq? entry-direction 'left))
                   ((movement 'set-total-distances!) -1 0))
                  ((and (eq? exit-direction 'up) (eq? entry-direction 'left))
                   ((movement 'set-total-distances!) -1 0))
                  ((and (eq? exit-direction 'up) (eq? entry-direction 'down))
                   ((movement 'set-total-distances!) 0 1))
                  ((and (eq? exit-direction 'down) (eq? entry-direction 'left))
                   ((movement 'set-total-distances!) -1 0))
                  ((and (eq? exit-direction 'down) (eq? entry-direction 'up))
                   ((movement 'set-total-distances!) 0 -1))))))


    ; Losing lives (auw) and checking whether to die or not
    (define (hurt! damage monsters fired-back? gives-money?)
      (if (or (not shield) (not (shield 'is-active?)))
          (set! lives (- lives damage)))
      (if fired-back? (get-fired-back!))
      (cond ((<= lives 0) (die! monsters gives-money?))
            ((and (= 1 lives) (= type 1)) (set! movement ((movement 'slowed-down-movement))))
            (shield ((shield 'hit-shield!)))))

    (define (hurt-with-tank! tank monsters)
      (when (not ((overridden-tanks 'contains-object?) tank))
        ((overridden-tanks 'add-object!) tank)
        (hurt! tank-damage monsters #f #f)))

    ; Dying (RIP)
    (define (die! monsters gives-money?)
      (let ((game (level 'get-game)))
        (if (= type 3) (donate-lives! monsters))
        ((level 'remove-monster!) dispatch)
        ((game 'set-money!) (+ (game 'get-money) (monster-config-money type)))
        (if (< (random) drop-item-probability)
            ((level 'add-dropped-item!) (make-dropped-item position)))))

    (define (set-dimensions! dims)
      (set! dimensions dims))

    (define (donate-lives! monsters)
      (for-each (lambda (monster) (if (and (not (eq? dispatch monster))
                                           (<= ((position 'distance-to-other-position) (monster 'get-position)) donating-lives-range))
                                      ((monster 'add-single-life!))))
                monsters))
      

    (define (add-single-life!)
      (set! lives (+ lives 1)))
    
    (define (dispatch msg)
      (cond ((eq? msg 'update!) update!)
            ((eq? msg 'get-position) position)
            ((eq? msg 'get-movement) movement)
            ((eq? msg 'get-underlying-pathcell) underlying-pathcell)
            ((eq? msg 'get-lives) lives)
            ((eq? msg 'get-exit-direction) exit-direction)
            ((eq? msg 'get-type) type)
            ((eq? msg 'hurt!) hurt!)
            ((eq? msg 'hurt-with-tank!) hurt-with-tank!)
            ((eq? msg 'set-dimensions!) set-dimensions!)
            ((eq? msg 'get-dimensions) dimensions)
            ((eq? msg 'get-shield) shield)
            ((eq? msg 'add-single-life!) add-single-life!)
            (else (error "monster-adt dispatch message not understood:" msg))))
    dispatch))