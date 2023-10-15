(#%require "GraphicalLibrary/Graphics.rkt")
(load "constants-and-auxfunctions.rkt")
(load "dimensions-adt.rkt")
(load "position-adt.rkt")
(load "ui-container-adt.rkt")
(load "ui-dynamic-text-adt.rkt")
(load "ui-selection-listener-adt.rkt")
(load "ui-click-listener-adt.rkt")

(define (make-screen)
  
  (define (ui-tower-picker-item i)
    (make-ui-container 'padding
                       (make-ui-container 'row
                                          (make-ui-container 'scaled-image 0.9 (tower-config-image i) "tower-mask")
                                          (make-ui-container 'column
                                                             (make-ui-container 'row 
                                                                                (make-ui-container 'static-text (tower-config-name i))
                                                                                (make-ui-container 'h-space 30))
                                                             (make-ui-container 'padding
                                                                                (make-ui-container 'row
                                                                                                   (make-ui-container 'image "money" "money-mask")
                                                                                                   (make-ui-container 'h-space 5)
                                                                                                   (make-ui-container 'static-text (number->string (tower-config-price i)))))
                                                             (make-ui-container 'padding
                                                                                (make-ui-container 'row
                                                                                                   (make-ui-container 'image "target" "target-mask")
                                                                                                   (make-ui-container 'h-space 5)
                                                                                                   (make-ui-container 'static-text (number->string (tower-config-range i)))))))))

  (define (ui-header text)
    (make-ui-container 'padding
                       (make-ui-container 'static-text text 'header #f #t)
                       vertical-padding
                       (* horizontal-padding 2)))

  (define (ui-power-up-item name key image dynamic-text buy-proc price)
    (make-ui-container 'row
                       (make-ui-container 'padding (make-ui-container 'scaled-image 0.9 image "dropped-item-mask"))
                       (make-ui-container 'v-space 10)
                      
                       (make-ui-container 'column
                                          (make-ui-container 'row
                                                             (make-ui-container 'static-text (string-append name ": "))
                                                             (make-ui-container 'dynamic-text dynamic-text 'normal #f #t))
                                          (make-ui-container 'static-text (string-append "druk " key " voor activatie") 'subtext))
                       ;(make-ui-container 'row
                       ;                   (make-ui-container 'static-text "inzetbaar: " 'subtext)
                       ;                   (make-ui-container 'dynamic-text dynamic-text 'subtext #f #t)))
                       (make-ui-container 'h-space 15)
                       (make-ui-container 'button
                                          buy-proc
                                          (make-ui-container 'padding
                                                             (make-ui-container 'column
                                                                                (make-ui-container 'static-text "Koop" 'normal #t)
                                                                                (make-ui-container 'row
                                                                                                   (make-ui-container 'image "money" "money-mask")
                                                                                                   (make-ui-container 'h-space 5)
                                                                                                   (make-ui-container 'static-text (number->string price))))))))
                                                                                    
  
  (let* ((window (make-window window-width window-height "Tower Defense (Abel Stuker)"))
         (background-layer ((window 'new-layer!)))
         (path-layer ((window 'new-layer!)))
         (towers-layer ((window 'new-layer!)))
         (monsters-layer ((window 'new-layer!)))
         (projectiles-layer ((window 'new-layer!)))
         (power-ups-layer ((window 'new-layer!)))
         (dropped-items-layer ((window 'new-layer!)))
         (top-layer ((window 'new-layer!)))
         (hover-layer ((window 'new-layer!)))
         (overlay-layer ((window 'new-layer!)))
         ; The following lists are associate lists with cons-cells, each containing the ADT itself in the car
         ;   and the corresponding tile in the cdr. To make deleting from these lists easier, the first value
         ;   of each list will always be a dummy variable.
         (tower-tiles (list 'tower-tiles)) 
         (monster-tiles (list 'monster-tiles))
         (healthbar-tiles (list 'healthbar-tiles))
         (projectile-tiles (list 'projectile-tiles))
         (obstacle-tiles (list 'obstacle-tiles))
         (tank-tile *UNINITIALISED*)
         (dropped-item-tiles (list 'dropped-item-tiles))
         ; ---

         (dynamic-text-level (make-ui-dynamic-text ""))
         (dynamic-text-wave (make-ui-dynamic-text ""))
         (dynamic-text-action (make-ui-dynamic-text ""))
         (dynamic-text-error (make-ui-dynamic-text "" error-message-time))
         (dynamic-text-money (make-ui-dynamic-text (number->string init-money)))
         (dynamic-text-lives (make-ui-dynamic-text (number->string init-lives)))
         (dynamic-text-tanks (make-ui-dynamic-text "0"))
         (dynamic-text-obstacles (make-ui-dynamic-text "0"))
         (dynamic-text-power-ups (make-ui-dynamic-text "inzetbaar"))
         (tower-selection-obsever (make-ui-selection-listener))
         (click-listener-tanks (make-ui-click-listener))
         (click-listener-obstacles (make-ui-click-listener))
         (bottombar-ui-configuration (make-ui-container 'padding
                                                        (make-ui-container 'column
                                                                           (make-ui-container 'row
                                                                                              (make-ui-container 'static-text "Level: ")
                                                                                              (make-ui-container 'dynamic-text dynamic-text-level)
                                                                                              (make-ui-container 'h-space 50)
                                                                                              (make-ui-container 'dynamic-text dynamic-text-error 'normal #f #f error-color))
                                                                           (make-ui-container 'row
                                                                                              (make-ui-container 'static-text "Ronde: ")
                                                                                              (make-ui-container 'dynamic-text dynamic-text-wave))
                                                                           (make-ui-container 'row
                                                                                              (make-ui-container 'static-text "TESTING KEYBINDINGS | spawn monsters: rood 'r', blauw 'b', geel 'g', paars 'p' | +200 coins: 'm'" 'subtext #f #f testing-info-color))
                                                                           (make-ui-container 'row
                                                                                              (make-ui-container 'dynamic-text dynamic-text-action)))
                                                        (* 2 vertical-padding)
                                                        (* 2 horizontal-padding)))
         (sidebar-ui-configuration (make-ui-container 'column
                                                      (make-ui-container 'padding
                                                                         (make-ui-container 'column
                                                                                            
                                                                                            (make-ui-container 'row
                                                                                                               (make-ui-container 'image "money" "money-mask")
                                                                                                               (make-ui-container 'h-space 5)
                                                                                                               (make-ui-container 'dynamic-text dynamic-text-money))
                                                                                            (make-ui-container 'v-space 5)
                                                                                            (make-ui-container 'row
                                                                                                               (make-ui-container 'image "lives" "lives-mask")
                                                                                                               (make-ui-container 'h-space 5)
                                                                                                               (make-ui-container 'dynamic-text dynamic-text-lives))
                                                                                            (make-ui-container 'v-space 10)))

                                                      (ui-header "Torens")
                                              
                                                      (make-ui-container 'selectable-column
                                                                         tower-selection-obsever
                                                                         (ui-tower-picker-item 0)
                                                                         (ui-tower-picker-item 1)
                                                                         (ui-tower-picker-item 2)
                                                                         (ui-tower-picker-item 3)
                                                                         (make-ui-container 'v-space 10))

                                                      (ui-header "Power-ups")

                                                      (make-ui-container 'padding
                                                                         (make-ui-container 'row
                                                                                            (make-ui-container 'static-text "Status: ")
                                                                                            (make-ui-container 'dynamic-text dynamic-text-power-ups))
                                                                         vertical-padding
                                                                         (* horizontal-padding 2))

                                                      (make-ui-container 'padding
                                                                         (make-ui-container 'column
                                                                                            (ui-power-up-item "Tank" "T" "dropped-item-tank" dynamic-text-tanks click-listener-tanks tank-price)
                                                                                            (ui-power-up-item "Obstakel" "O" "dropped-item-obstacles" dynamic-text-obstacles click-listener-obstacles obstacle-price)))))
                                              
                                              
         
         (current-hovering-position *UNINITIALISED*)
         (game *UNINITIALISED*))

    ((window 'set-background!) "black")

    (define (initialise! n-game)
      (set! game n-game)
      (draw-ui!))
      
      

    ; Drawing a tile to its corresponding position as defined in the object itself
    (define (draw-object! object tile center? . args)
      (let* ((draw-on-top? (if (null? args) #f (car args)))
             (position (object 'get-position))
             (x (position 'get-x))
             (y (position 'get-y))
             (x-px (* cell-width x))
             (y-px (- (* cell-height y) (if draw-on-top? ((tile 'get-h)) 0))))
        (set-tile-position! tile (if center? (- x-px (/ (tile 'get-w) 2)) x-px)  (if center? (- y-px (/ (tile 'get-h) 2)) y-px))))

    (define (set-tile-position! tile x-px y-px)
      ((tile 'set-x!) x-px)
      ((tile 'set-y!) y-px))

    ; Removing an object from the screen (i.e. from its corresponding layer and list)
    (define (remove-from-screen! object objects-list layer)
      (define (hulp lst previous)
        (cond ((null? lst) #f)
              ((symbol? (car lst)) (hulp (cdr lst) (cdr previous)))
              ((equal? (caar lst) object)
               ((layer 'remove-drawable!) (cdar lst))
               (if (null? (cdr lst))
                   (set-cdr! previous '())
                   (set-cdr! previous (cdr lst))))
              (else (hulp (cdr lst) (cdr previous)))))
      (hulp (cdr objects-list) objects-list))

    ; Retrieving the tile of an object from its corresponding associate list
    (define (get-object-tile object objects-list add-procedure)
      (let ((screen-object (assoc object (cdr objects-list))))
        (if screen-object
            (cdr screen-object) ; the car contains the actual adt, the cdr contains the tile
            (add-procedure object))))

    ; Adding a tile for an object
    (define (add-object-tile! set-dimensions? object objects-list objects-layer image-path . image-mask)
      (define smallest-w 99999)
      (define smallest-h 99999)

      (define (construct-single-tile path mask-path)
        (let ((new-tile (make-bitmap-tile (drawable-file path) (drawable-file mask-path))))
          (set! smallest-w (min smallest-w (/ (new-tile 'get-w) cell-width)))
          (set! smallest-h (min smallest-h (/ (new-tile 'get-h) cell-height)))
          
          new-tile))

      (define (construct-tile-sequence paths masks)
        (define (recursive-construction paths masks)
          (let ((mask (if (pair? masks) (car masks) masks))
                (next-mask (if (pair? masks) (cdr masks) masks)))
            (if (null? paths)
                '()
                (cons (construct-single-tile (car paths) mask) (recursive-construction (cdr paths) next-mask)))))
        (make-tile-sequence (recursive-construction paths masks)))

      (let* ((mask (if (null? image-mask) #f (car image-mask)))
             (constructed-tile (if (pair? image-path)
                                   (construct-tile-sequence image-path mask)
                                   (construct-single-tile image-path mask))))
        (if set-dimensions?
            ((object 'set-dimensions!) (make-dimensions smallest-w smallest-h)))
        ((objects-layer 'add-drawable!) constructed-tile)
        (set-cdr! objects-list (cons (cons object constructed-tile) (cdr objects-list)))
        constructed-tile))



    
    ; towers
    (define (draw-tower! tower)
      (draw-object! tower (get-tower-tile tower) #f))
    (define (get-tower-tile tower) (get-object-tile tower tower-tiles add-tower-tile!))
    (define (add-tower-tile! tower) (add-object-tile! #f tower tower-tiles towers-layer  (tower-config-image (tower 'get-type)) "tower-mask"))
    (define (clear-all-towers!)
      (set! tower-tiles (list 'tower-tiles)) 
      ((towers-layer 'empty!)))

    ; monsters
    (define (draw-monster! monster)
      (let* ((shield (monster 'get-shield))
             (monster-tile (get-monster-tile monster))
             (healthbar-tile (get-healthbar-tile monster))
             (max-monster-lives (monster-config-lives (monster 'get-type)))
             (current-monster-lives (monster 'get-lives))
             (health-bar-width (* (* ((monster 'get-dimensions) 'get-h) cell-width)
                                  (/ max-monster-lives 4))))
        (when shield
          (if (shield 'is-active?)
              ((monster-tile 'set-current!) 0)
              ((monster-tile 'set-current!) 1)))

        ((healthbar-tile 'set-current!) (cond ((> current-monster-lives 5) 4)
                                              ((< current-monster-lives 1) 0)
                                              (else (- current-monster-lives 1))))

        (draw-object! monster monster-tile #f)
        (draw-object! monster healthbar-tile #f #t)))
    (define (get-monster-tile monster) (get-object-tile monster monster-tiles add-monster-tile!))
    (define (get-healthbar-tile monster) (get-object-tile monster healthbar-tiles add-monster-tile!))
    (define (add-monster-tile! monster) 
      (add-object-tile! #t monster healthbar-tiles monsters-layer healthbar-images healthbar-masks)
      (add-object-tile! #t monster monster-tiles monsters-layer (monster-config-image (monster 'get-type)) "monster_mask"))
    (define (clear-monster! monster) 
      (remove-from-screen! monster healthbar-tiles monsters-layer)
      (remove-from-screen! monster monster-tiles monsters-layer))
    (define (clear-all-monsters!)
      (set! monster-tiles (list 'monster-tiles))
      (set! healthbar-tiles (list 'healthbar-tiles))
      ((monsters-layer 'empty!)))


    ; projectiles
    (define (draw-projectile! projectile scale?)
      (let ((tile (get-projectile-tile projectile)))
        (if scale? ((tile 'set-scale!) (projectile 'get-scale)))
        (draw-object! projectile tile #t)))
    
    (define (get-projectile-tile projectile) (get-object-tile projectile projectile-tiles add-projectile-tile!))
    (define (add-projectile-tile! projectile) 
      (add-object-tile! #t projectile projectile-tiles projectiles-layer (tower-config-projectile-image (projectile 'get-type)) (drawable-mask-title (tower-config-projectile-image (projectile 'get-type)))))
    (define (clear-projectile! projectile) 
      (remove-from-screen! projectile projectile-tiles projectiles-layer))
    (define (clear-all-projectiles!)
      ((projectiles-layer 'empty!))
      (set! projectile-tiles (list 'projectile-tiles)))

    ; tanks
    (define (draw-tank! tank)
      (let* ((tile (get-tank-tile))
             (exit (tank 'get-exit)))
        (cond ((eq? exit 'right)
               ((tile 'rotate!) 0))
              ((eq? exit 'up)
               ((tile 'rotate!) 90))
              ((eq? exit 'down)
               ((tile 'rotate!) -90)))
        
        (draw-object! tank tile #f)))
    
    (define (get-tank-tile) (if tank-tile tank-tile (add-tank-tile!)))
    (define (add-tank-tile!)
      (set! tank-tile (make-bitmap-tile (drawable-file "tank") (drawable-file "tank-mask")))
      ((power-ups-layer 'add-drawable!) tank-tile)
      tank-tile)
    

    ; obstacles
    (define (draw-obstacle! obstacle)
      (draw-object! obstacle (get-obstacle-tile obstacle) #f))
    (define (get-obstacle-tile obstacle) (get-object-tile obstacle obstacle-tiles add-obstacle-tile!))
    (define (add-obstacle-tile! obstacle)
      (let ((drawable-title (if (eq? (obstacle 'get-orientation) 'horizontal) "obstacle-h" "obstacle-v")))
        (add-object-tile! #t obstacle obstacle-tiles power-ups-layer drawable-title (drawable-mask-title drawable-title))))
    
    (define (clear-power-up!)
      ((power-ups-layer 'empty!))
      (set! tank-tile *UNINITIALISED*)
      (set! obstacle-tiles (list 'obstacle-tiles)))

    ; dropped-items
    (define (draw-dropped-item! dropped-item)
      (draw-object! dropped-item (get-dropped-item-tile dropped-item) #f))
    (define (get-dropped-item-tile dropped-item) (get-object-tile dropped-item dropped-item-tiles add-dropped-item-tile!))
    (define (add-dropped-item-tile! dropped-item)
      (add-object-tile! #t dropped-item dropped-item-tiles dropped-items-layer (string-append "dropped-item-"(symbol->string (dropped-item 'get-type))) "dropped-item-mask"))
    (define (clear-dropped-item! dropped-item)
      (remove-from-screen! dropped-item dropped-item-tiles dropped-items-layer))
    (define (clear-all-dropped-items!)
      (set! dropped-item-tiles (list 'dropped-item-tiles))
      ((dropped-items-layer 'empty!)))

    ; clearing all game elements
    (define (clear-all-game-elements!)
      (clear-all-monsters!)
      (clear-all-towers!)
      (clear-all-projectiles!)
      (clear-all-dropped-items!)
      (clear-power-up!))

    ; drawing a game element of specified type with given argslist
    (define (draw-game-element! type argslist)
      (case type
        ('tower (apply draw-tower! argslist))
        ('projectile (apply draw-projectile! argslist))
        ('monster (apply draw-monster! argslist))
        ('tank (apply draw-tank! argslist))
        ('obstacle (apply draw-obstacle! argslist))
        ('dropped-item (apply draw-dropped-item! argslist))
        ('pathcell (apply draw-pathcell! argslist))
        ('entry-exit-overlays (apply draw-entry-exit-overlays! argslist))
        ('background (apply draw-background! argslist))))

    ; clearing a single game element
    (define (clear-game-element! type . object)
      (case type
        ('projectile (clear-projectile! (car object)))
        ('monster (clear-monster! (car object)))
        ('power-up (clear-power-up!))
        ('dropped-item (clear-dropped-item! (car object)))
        ('path (clear-path!))))

    ; pixels to position converter
    (define (pixels-to-position x-pix y-pix)
      (make-position (/ x-pix cell-width) (/ y-pix cell-height)))

    ; determines whether an x and y coördinate (in px) are located within the gamefield
    (define (in-gamefield? x y)
      (and (< x (* horizontal-cells-amount cell-width))
           (> x 0)
           (< y (* vertical-cells-amount cell-height))
           (> y 0)))
    (define (in-sidebar? x y)
      (> x (* horizontal-cells-amount cell-width)))
      
    ; mousclick
    (define (mouseclick-delegate key status x y)
      (if (eq? status 'pressed)
          ; The following conditional checks wheter the click occured within or outside the gamefield, and calls the appropriate procedure
          (cond ((in-gamefield? x y)
                 ((game 'mouseclick) key (pixels-to-position x y)))
                ((in-sidebar? x y)
                 (mouseclick-in-sidebar key x y)))))
    (define (mouseclick-in-sidebar key x y)
      ((sidebar-ui-configuration 'click!) dispatch x y))

    ; hovering
    (define (hover-delegate x y)
      (if (in-gamefield? x y)
          (let ((level (if game (game 'get-level) #f))
                (anchored-position (((pixels-to-position x y) 'make-anchored-position))))
            (if (and level ((level 'can-place-tower?) anchored-position #f))
                (hovering-on-gamefield! anchored-position game)
                (remove-gamefield-hover!)))
          (remove-gamefield-hover!)))
    
    (define (hovering-on-gamefield! position game)
      (when (not (eq? position current-hovering-position))
        (set! current-hovering-position position)
        (let* ((active-tower-index (game 'get-selected-tower-type))
               (range-px (* cell-width (tower-config-range active-tower-index)))
               (range-diameter-px (inexact->exact (* 2 range-px)))
               (tower-tile (make-bitmap-tile (drawable-file (tower-config-image active-tower-index)) (drawable-file "tower-mask")))
               (range-tile (make-tile range-diameter-px range-diameter-px))
               (x-px (* cell-width (position 'get-x)))
               (y-px (* cell-height (position 'get-y))))
          (remove-gamefield-hover!)
          ; drawing the radius circle on the radius tile
          (if (or (= active-tower-index 0)
                  (= active-tower-index 1))
              ((range-tile 'draw-ellipse!) 0 0 range-diameter-px range-diameter-px (custom-color 255 255 255 0.1))
              ((range-tile 'draw-rectangle!) 0 0 cell-width cell-height (custom-color 255 255 255 0.1)))
          ; adding the drawables to the hover-layer and setting their correct position
          ((hover-layer 'add-drawable!) tower-tile)
          ((hover-layer 'add-drawable!) range-tile)
          ((tower-tile 'set-x!) x-px)
          ((tower-tile 'set-y!) y-px)
          (if (or (= active-tower-index 0)
                  (= active-tower-index 1))
              (begin
                ((range-tile 'set-x!) (- x-px (- range-px (/ (tower-tile 'get-w) 2))))
                ((range-tile 'set-y!) (- y-px (- range-px (/ (tower-tile 'get-h) 2)))))
              (let ((nearest-pos (((((game 'get-level) 'get-path) 'get-nearest-pathcell) ((position 'make-anchored-position))) 'get-position)))
                      ((range-tile 'set-x!) (* cell-width (nearest-pos 'get-x)))
                      ((range-tile 'set-y!) (* cell-height (nearest-pos 'get-y))))))))
    
    (define (remove-gamefield-hover!)
      ((hover-layer 'empty!)))

    ; keypress
    (define (keypress-delegate status key)
      (if (eq? status 'pressed)
          ((game 'keypress) key)))

    ; update
    (define (update-delegate ms)
      (if (< ms 1000) ((game 'update!) ms)))
    
    
    ; callbacks
    ((window 'set-update-callback!) update-delegate)
    ((window 'set-mouse-click-callback!) mouseclick-delegate)
    ((window 'set-mouse-move-callback!) hover-delegate)
    ((window 'set-key-callback!) keypress-delegate)

    (define (buy-tank!)
      (((game 'get-power-ups-delegate) 'collect!) 'tank #t))
    (define (buy-obstacle!)
      (((game 'get-power-ups-delegate) 'collect!) 'obstacles #t))

    ; UI drawing
    (define (draw-ui!)
      ((overlay-layer 'empty!))
      ((tower-selection-obsever 'add-updater!) (game 'set-selected-tower-type!))
      ((click-listener-tanks 'add-updater!) buy-tank!)
      ((click-listener-obstacles 'add-updater!) buy-obstacle!)
      ((sidebar-ui-configuration 'build!) dispatch gamefield-width 0)
      ((bottombar-ui-configuration 'build!) dispatch 0 gamefield-height))

    (define (draw-ui-element! type argslist)
      (case type
        ('text (apply draw-ui-text! argslist))
        ('image (apply draw-ui-image! argslist))
        ('selection-box (apply draw-ui-selection-box! argslist))))

    (define (draw-ui-text! x y text specifications . existing-tile) ; specifications is a list of optional decorations: font size (number), underlignment (boolean), bold (boolean)
      (let ((text-tile (if (null? existing-tile)
                           (make-tile 5 5)
                           (car existing-tile)))
            (font-size (cond ((null? specifications) text-size-normal)
                             ((eq? (car specifications) 'normal) text-size-normal)
                             ((eq? (car specifications) 'header) text-size-big)
                             ((eq? (car specifications) 'subtext) text-size-small)
                             (else text-size-normal)))
            (underligned? (and (not (null? specifications))
                               (not (null? (cdr specifications)))
                               (cadr specifications)))
            (bold? (and (not (null? specifications))
                        (not (null? (cdr specifications)))
                        (not (null? (cddr specifications)))
                        (caddr specifications)))
            (color (if (and (not (null? specifications))
                            (not (null? (cdr specifications)))
                            (not (null? (cddr specifications)))
                            (not (null? (cdddr specifications)))
                            (cadddr specifications))
                       (cadddr specifications)
                       text-color)))
        ((text-tile 'draw-tilewrap-text!) text font-size 0 0 color underligned? bold?)
        (if (null? existing-tile) ((overlay-layer 'add-drawable!) text-tile))
        ((text-tile 'set-x!) x)
        ((text-tile 'set-y!) y)
        text-tile))

    (define (draw-ui-image! x y image-name mask-name . args)
      (let ((image-tile (make-bitmap-tile (drawable-file image-name) (drawable-file mask-name))))
        ((overlay-layer 'add-drawable!) image-tile)
        ((image-tile 'set-x!) x)
        ((image-tile 'set-y!) y)
        ((image-tile 'set-scale!) (car args))
        image-tile))

    (define (draw-ui-selection-box! x y w h observer)
      (let* ((observer-tile (observer 'get-selection-tile))
             (selection-tile (if observer-tile
                                 observer-tile 
                                 ((observer 'set-selection-tile!) (make-tile w h)))))
        ((selection-tile 'clear!))
        ((selection-tile 'draw-line!) 0 0 0 h tower-item-selector-width "white")
        ((selection-tile 'draw-line!) 0 h w h tower-item-selector-width "white")
        ((selection-tile 'draw-line!) w h w 0 tower-item-selector-width "white")
        ((selection-tile 'draw-line!) w 0 0 0 tower-item-selector-width "white")
        ((overlay-layer 'add-drawable!) selection-tile)
        ((selection-tile 'set-x!) x)
        ((selection-tile 'set-y!) y)
        selection-tile))

    ; dynamic text updaters
    (define (update-ui-element! element new-value)
      (if (number? new-value)
          (set! new-value (number->string new-value)))
      (cond ((eq? element 'money)
             ((dynamic-text-money 'update-text!) new-value))
            ((eq? element 'lives)
             ((dynamic-text-lives 'update-text!) new-value))
            ((eq? element 'tanks)
             ((dynamic-text-tanks 'update-text!) new-value))
            ((eq? element 'obstacles)
             ((dynamic-text-obstacles 'update-text!) new-value))
            ((eq? element 'power-ups-status)
             ((dynamic-text-power-ups 'update-text!) new-value))
            ((eq? element 'level)
             ((dynamic-text-level 'update-text!) new-value))
            ((eq? element 'wave)
             ((dynamic-text-wave 'update-text!) new-value))
            ((eq? element 'action)
             ((dynamic-text-action 'update-text!) new-value))
            ((eq? element 'error)
             ((dynamic-text-error 'update-text!) new-value))))

    ; dynamic text counter updaters
    (define (update-dynamic-counters! ms)
      ((dynamic-text-action 'update!) ms)
      ((dynamic-text-error 'update!) ms))
    

    ; Procedure to draw a background tile (used by fill-screen)
    (define (draw-background! position black?)
      (let* ((background-tile (make-bitmap-tile (drawable-file (if black? "tile-black-background" "tile-background"))))
             (x-pos (position 'get-x))
             (y-pos (position 'get-y))
             (x-px (* cell-width x-pos))
             (y-px (* cell-height y-pos)))
        (((if black? top-layer background-layer) 'add-drawable!) background-tile)
        ((background-tile 'set-x!) x-px)
        ((background-tile 'set-y!) y-px)))
    
    ; Procedure to draw a single pathcell
    (define (draw-pathcell! pathcell)
      (let* ((position (pathcell 'get-position));(img-path (string-append "tile-" (symbol->string entry) "-" (symbol->string exit))) -> used when we need a custom pathcell for each possible clear-path and exit direction
             (is-first? (pathcell 'is-first?))
             (is-last? (pathcell 'is-last?))
             (pathcell-tile (cond (is-first? (make-bitmap-tile (drawable-file "tile-path-entry")))
                                  (is-last? (make-bitmap-tile (drawable-file "tile-path-exit")))
                                  (else (make-bitmap-tile (drawable-file "tile-path")))))
             (original-x (position 'get-x))
             (original-y (position 'get-y)))
        ((path-layer 'add-drawable!) pathcell-tile)
        ((pathcell-tile 'set-x!) (* cell-width original-x))
        ((pathcell-tile 'set-y!) (* cell-height original-y))))

    (define (clear-path!)
      ; ((top-layer 'empty!))
      ((path-layer 'empty!)))

    ; Procedure to draw the entry and exit overlay
    (define (draw-entry-exit-overlays! path)
      (let ((vec (path 'get-path-vector))
            (entry-tile (make-bitmap-tile (drawable-file "tile-entry") (drawable-mask-file "tile-entry")))
            (exit-tile (make-bitmap-tile (drawable-file "tile-exit") (drawable-mask-file "tile-exit"))))
        ((path-layer 'add-drawable!) entry-tile)
        ((path-layer 'add-drawable!) exit-tile)
        (draw-object! ((path 'get-pathcell-from-index) 0) entry-tile #f)
        (draw-object! ((path 'get-pathcell-from-index) (- (vector-length vec) 1)) exit-tile #f)))
                 

    (define (dispatch msg)
      (cond ((eq? msg 'update-dynamic-counters!) update-dynamic-counters!)
            ((eq? msg 'update-ui-element!) update-ui-element!)
            ((eq? msg 'clear-all-game-elements!) clear-all-game-elements!)
            ((eq? msg 'draw-game-element!) draw-game-element!)
            ((eq? msg 'clear-game-element!) clear-game-element!)
            ((eq? msg 'draw-ui-element!) draw-ui-element!)
            ((eq? msg 'initialise!) initialise!)
            (else (error "screen-adt dispatch message not understood:" msg))))
    dispatch))
