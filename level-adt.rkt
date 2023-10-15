(#%require (only racket/base random error))
(load "constants-and-auxfunctions.rkt")
(load "screen-adt.rkt")
(load "position-adt.rkt")
(load "tower-adt.rkt")
(load "monster-adt.rkt")
(load "path-adt.rkt")
(load "object-list-adt.rkt")
(load "tank-adt.rkt")
(load "obstacle-delegate-adt.rkt")
(load "dropped-item-adt.rkt")


(define (make-level game screen level-number)
  (let ((path *UNINITIALISED*) 
        (towers (make-object-list))
        (monsters (make-object-list))
        (projectiles (make-object-list))
        (dropped-items (make-object-list))
        (wave-instructions *UNINITIALISED*)
        (wave 0))

    ; Mouse-click procedure for when a level is active
    (define (mouseclick key position)
      (cond ((and (eq? key 'left)
                  (collect-dropped-item! position))
             #t)
            ((and (eq? key 'left)
                  (can-place-tower? position #t))
             (add-tower! ((position 'make-anchored-position))))))


    ; Key-press procedure for when a level is active
    (define (keypress key)
      (cond ((eq? key #\t)
             (add-tank!))
            ((eq? key #\o)
             (add-obstacles!))
            ((eq? key #\r)
             ((screen 'draw-game-element!) 'monster (list (add-monster! 0))))
            ((eq? key #\b)
             ((screen 'draw-game-element!) 'monster (list (add-monster! 1))))
            ((eq? key #\g)
             ((screen 'draw-game-element!) 'monster (list (add-monster! 2))))
            ((eq? key #\p)
             ((screen 'draw-game-element!) 'monster (list (add-monster! 3))))
            ((and (eq? key #\space)
                  (= 0 (monsters 'get-count)))
             (if wave
                 (start-new-wave!)
                 (end-level! #f)))))

    ; Procedure that determines whether a tower can be placed on a given position
    (define (can-place-tower? position show-ui-errors?)
      (let ((selected-tower-type (game 'get-selected-tower-type)))
        (if (>= (game 'get-money) (tower-config-price selected-tower-type))
            (if (and (not ((position 'object-in-position?) (path 'get-path-vector)))
                     (not ((position 'object-in-position?) (towers 'get-objects)))
                     (or (= selected-tower-type 0)
                         (= selected-tower-type 1)
                         ((path 'get-nearest-pathcell) ((position 'make-anchored-position)))))
                #t
                (begin
                  (if show-ui-errors? ((screen 'update-ui-element!) 'error "Deze toren kan je hier niet plaatsen"))
                  #f))
            (begin
              (if show-ui-errors? ((screen 'update-ui-element!) 'error "Niet genoeg geld om deze toren te kopen"))
              #f))))
               
    
    ; Procedure updates all the game objects
    (define (update! ms)
      ; (if (not (= 0 (monsters 'get-count)))
      ;     ((screen 'write-text-under!) (string-append "Monsters to kill: " (number->string (monsters 'get-count))) (string-append "Towers on the field: " (number->string (towers 'get-count))))
      ;     ((screen 'write-text-under!) "Press the spacebar to start another wave, press r to restart the game."))
     
      ; updating the monsters
      (for-each (lambda (el) ((el 'update!) ms path screen (if ((game 'get-power-ups-delegate) 'get-active-obstacle-delegate) (((game 'get-power-ups-delegate) 'get-active-obstacle-delegate) 'get-obstacles) #f)))
                (monsters 'get-objects)) 
      ; updating the towers
      (for-each (lambda (el) ((el 'update!) ms (monsters 'get-objects) path))
                (towers 'get-objects))
      ; updating the projectiles
      (for-each (lambda (el) ((el 'update!) ms (monsters 'get-objects) screen))
                (projectiles 'get-objects))
      ; updating the power-ups
      (((game 'get-power-ups-delegate) 'update!) ms screen dispatch (monsters 'get-objects))
      (level-state-update!)
      ; updating the dropped-items
      (for-each (lambda (el) ((el 'update!) ms dispatch))
                (dropped-items 'get-objects)))
      

    ; Procedures to add and remove MONSTERS:
    (define (add-monster! . args)
      (let* ((first-pathcell ((path 'get-pathcell-from-index) 0))
             (type (if (null? args) (inexact->exact (floor (* 4 (random)))) (car args)))
             (new-monster (if (null? args)
                              (make-monster first-pathcell dispatch type #f)
                              (make-monster first-pathcell dispatch type #t))))
        ((monsters 'add-object!) new-monster)
        new-monster))

    (define (remove-monster! monster)
      ((screen 'clear-game-element!) 'monster monster)
      ((monsters 'remove-object!) monster))

    ; Procedures to add TOWERS (removing towers will be introduced in phase 2):
    (define (add-tower! position)
      (let* ((type (game 'get-selected-tower-type))
             (tower (make-tower position dispatch type)))
        ((game 'set-money!) (- (game 'get-money) (tower-config-price type)))
        ((towers 'add-object!) tower)
        ((screen 'draw-game-element!) 'tower (list tower))))

    ; Procedures to add and remove PROJECTILES:
    (define (add-projectile! projectile)
      ((projectiles 'add-object!) projectile))

    (define (remove-projectile! projectile)
      ((screen 'clear-game-element!) 'projectile projectile)
      ((projectiles 'remove-object!) projectile))

    ; Procedures to add and remove TANKS:
    (define (add-tank!)
      (let ((new-tank (((game 'get-power-ups-delegate) 'use-power-up!) 'tank path)))
        (if new-tank ((screen 'draw-game-element!) 'tank (list new-tank)))))

    (define (remove-power-up!)
      (((game 'get-power-ups-delegate) 'deactivate-power-up!) #f #f))

    ; Procedures to add and remove OBSTACLE DELEGATES:
    (define (add-obstacles!)
      (let ((new-obstacle-delegate (((game 'get-power-ups-delegate) 'use-power-up!) 'obstacle-delegate path)))
        (if new-obstacle-delegate (vector-for-each (lambda (obstacle) ((screen 'draw-game-element!) 'obstacle (list obstacle)))
                                                   (new-obstacle-delegate 'get-obstacles)))))



    ; Procedures to add and remove DROPPED ITEMS:
    (define (add-dropped-item! new-dropped-item)
      ((screen 'draw-game-element!) 'dropped-item (list new-dropped-item))
      ((dropped-items 'add-object!) new-dropped-item))

    (define (remove-dropped-item! item)
      ((screen 'clear-game-element!) 'dropped-item item)
      ((dropped-items 'remove-object!) item))

    (define (collect-dropped-item! pos)
      (let ((clicked-item ((pos 'object-at-position-with-dimensions?) (dropped-items 'get-objects)))
            (power-ups-delegate (game 'get-power-ups-delegate)))
        (when clicked-item
          (remove-dropped-item! clicked-item)
          ((power-ups-delegate 'collect!) (clicked-item 'get-type) #f))
        clicked-item))


    

    ; Procedure makes a number of monsters spawn:
    (define (monster-spawner! count)
      (if (> count 0)
          (begin
            ((screen 'draw-game-element!) 'monster (list (add-monster!)))
            (monster-spawner! (- count 1)))))

    ; Start procedure:
    (define (start!)
      (set! wave-instructions (get-wave-instruction level-number))
      (set! path (make-path (get-path-instruction level-number)))
      ((path 'for-each-pathcell) (lambda (pathcell) ((screen 'draw-game-element!) 'pathcell (list pathcell))))
      ((screen 'draw-game-element!) 'entry-exit-overlays (list path))
      (level-state-update!))

    ; Procedure starts a wave:
    (define (start-new-wave!)
      ((screen 'update-ui-element!) 'action "")
      (set! wave (+ wave 1))
      ((screen 'update-ui-element!) 'wave (string-append (number->string wave) "/" (number->string (vector-length wave-instructions))))
      (monster-spawner! (vector-ref wave-instructions (- wave 1))))

    ; Procedure that checks whether a new wave needs to be started, or a new level can be started
    (define (level-state-update!)
      (if (and wave (= 0 (monsters 'get-count)))
          (if (< wave (vector-length wave-instructions))
              (wait-for-next 'wave)
              (if (and (< level-number (vector-length wave-configurations))
                       (< level-number (vector-length path-configurations)))
                  (wait-for-next 'level)
                  (end-level! #t)))))

    ; Procedure that waits until the user presses the key to continue to the next level
    (define (wait-for-next type)
      (cond ((eq? type 'wave)
             ((screen 'update-ui-element!) 'action "Druk op de spatiebalk om de volgende ronde te starten"))
            ((eq? type 'level)
             ((screen 'update-ui-element!) 'action "Druk op de spatiebalk om het volgende level te starten")
             (set! wave #f))))

    ; Procedure that ends the level by removing all level-specific elements and initiating a new level by communication with the Game AD
    (define (end-level! end-game?)
      (if end-game? ((game 'end-game!) #t) ((game 'new-level!))))
    
    ; Procedure resets the current level:
    (define (restart!)
      (set! towers (make-object-list))
      ((screen 'clear-all-game-elements!))
      (set! monsters (make-object-list))
      (set! projectiles (make-object-list))
      (start!))
             

    (define (dispatch msg)
      (cond
        ((eq? msg 'start!) start!)
        ((eq? msg 'mouseclick) mouseclick)
        ((eq? msg 'keypress) keypress)
        ((eq? msg 'update!) update!)
        ((eq? msg 'get-game) game)
        ((eq? msg 'can-place-tower?) can-place-tower?)
        ((eq? msg 'remove-monster!) remove-monster!)
        ((eq? msg 'add-projectile!) add-projectile!)
        ((eq? msg 'remove-projectile!) remove-projectile!)
        ((eq? msg 'remove-power-up!) remove-power-up!)
        ((eq? msg 'add-dropped-item!) add-dropped-item!)
        ((eq? msg 'remove-dropped-item!) remove-dropped-item!)
        ((eq? msg 'get-path) path)
        (else (error msg "message not understood by level-adt"))))
    dispatch))