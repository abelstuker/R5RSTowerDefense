(#%require (only racket/base random error))
(load "constants-and-auxfunctions.rkt")
(load "screen-adt.rkt")
(load "position-adt.rkt")
(load "level-adt.rkt")
(load "power-ups-delegate-adt.rkt")

(define (make-game)
  (let* ((selected-tower 0)
         (screen (make-screen))
         (level-number 0)
         (level *UNINITIALISED*)
         (money init-money)
         (lives init-lives)
         (power-ups-delegate *UNINITIALISED*))

    (define (set-money! amount)
      (set! money amount)
      ((screen 'update-ui-element!) 'money money))
   
    (define (set-lives! amount)
      (set! lives (max 0 amount))
      ((screen 'update-ui-element!) 'lives lives)
      (if (= lives 0) (end-game! #f)))
    
    (define (set-selected-tower-type! new-type)
      (set! selected-tower new-type))

    ; mouse-click handler (forwarded to level)
    (define (mouseclick key position)
      (if level
          ((level 'mouseclick) key position)))


    ; key-press procedure for when a level is active
    (define (keypress key)
      (cond ((and (eq? key #\space)
                  (not level))
             (restart!))
            ((eq? key #\m)
             (set-money! (+ money 200))) ; for testing purposes (add 200 money when M is pressed)
            (else ((level 'keypress) key))))

    ; The procedure fill-screen fills the screen with background tiles
    (define (fill-screen)
      (define (field-background position)
        ((screen 'draw-game-element!) 'background (list position #f))
        (let* ((x-grid (position 'get-grid-x))
               (y-grid (position 'get-grid-y))
               (last-col? (= x-grid (- horizontal-cells-amount 1)))
               (last-row? (= y-grid (- vertical-cells-amount 1))))
          (cond ((and last-col? last-row?) #t)
                (last-col? (field-background (make-position 0 (+ y-grid 1))))
                (else (field-background (make-position (+ x-grid 1) y-grid))))))
      
      (define (buffer-background position)
        ((screen 'draw-game-element!) 'background (list position #t))
        (let* ((x-grid (position 'get-grid-x))
               (y-grid (position 'get-grid-y))
               (last-row? (= y-grid (- vertical-cells-amount 1))))
          
          (if (not last-row?)
              (buffer-background (make-position x-grid (+ y-grid 1))))))
      (field-background (make-position 0 0))
      (buffer-background (make-position horizontal-cells-amount 0)))
    
    ; This procedure paints the background and initiates the first level. It is called by main.rkt when the game is started.
    (define (start!)
      (set! power-ups-delegate (make-power-ups-delegate dispatch screen))
      ((screen 'initialise!) dispatch)
      (fill-screen)
      (new-level!))

    ; This procedure is called
    (define (restart!)
      (set! level-number 0)
      (set-money! init-money)
      (set-lives! init-lives)
      (set! power-ups-delegate (make-power-ups-delegate dispatch screen))
      (new-level!))
            

    ; This procedure starts the next level (previous level + 1)
    (define (new-level!)
      (set! level-number (+ level-number 1))
      (set! level (make-level dispatch screen level-number))
      (clear-screen!  #t #f)
      ((screen 'update-ui-element!) 'level level-number)
      ((level 'start!)))

    ; Procedure that clears the entire screen (incl. UI text removal and powerup deactivation)
    (define (clear-screen! new-level? end-game?)
      ((screen 'update-ui-element!) 'action "")
      ((screen 'clear-game-element!) 'path)
      ((screen 'clear-all-game-elements!))
      ((power-ups-delegate 'deactivate-power-up!) new-level? end-game?))

    ; Procedure that ends the game, makes sure the screen is cleared and the end text is displayed
    (define (end-game! win?)
      (set! level *UNINITIALISED*)
      (clear-screen! #f #t)
      ((screen 'update-ui-element!) 'action (string-append (if win? "Goed gespeeld! " "Dood. Game over! ") "Einde van het spel. Druk op de spatiebalk om het spel te herstarten.")))

    ; Update procedure: updates dynamic counters and the level
    (define (update! ms)
      ((screen 'update-dynamic-counters!) ms)
      (when level
        ((level 'update!) ms)))

        
    (define (dispatch msg)
      (cond ((eq? msg 'update!) update!)
            ((eq? msg 'mouseclick) mouseclick)
            ((eq? msg 'keypress) keypress)
            ((eq? msg 'start!) start!)
            ((eq? msg 'get-power-ups-delegate) power-ups-delegate)
            ((eq? msg 'get-level) level)
            ((eq? msg 'new-level!) new-level!)
            ((eq? msg 'end-game!) end-game!)
            ((eq? msg 'get-selected-tower-type) selected-tower)
            ((eq? msg 'set-selected-tower-type!) set-selected-tower-type!)
            ((eq? msg 'get-lives) lives)
            ((eq? msg 'set-lives!) set-lives!)
            ((eq? msg 'get-money) money)
            ((eq? msg 'set-money!) set-money!)
            (else (error "game-adt dispatch message not understood:" msg))))
    dispatch))