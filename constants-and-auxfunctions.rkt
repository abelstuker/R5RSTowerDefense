(#%require (only racket/base random))
(define *UNINITIALISED* #f)

;; initial money and lives values
(define init-money 1000)
(define init-lives 10)

;; specifications for the user interface
(define text-color "white")
(define error-color "red")
(define testing-info-color "gray")
(define text-size-normal 12)
(define text-size-small 8)
(define text-size-big 15)
(define padding 6)
(define vertical-padding 3)
(define horizontal-padding 5)
(define overlay-under-amount 2)
(define overlay-indent 10)

;; dimension specifications
(define cell-width 100)
(define cell-height cell-width)

(define horizontal-cells-amount 14)
(define vertical-cells-amount 7)

(define bottom-bar-height 50)
(define side-bar-width 267)
(define status-item-height 20)

(define window-width (+ (* horizontal-cells-amount cell-width) 300))
(define window-height (+ (* vertical-cells-amount cell-height) 100))

(define gamefield-width (* horizontal-cells-amount cell-width))
(define gamefield-height (* vertical-cells-amount cell-height))

;; path configurations
(define path-configurations
  (vector #(3 R U U R R R D D D R R R R U U R R R R R R)
          #(2 R D D D R R U U R R U U U R R R D D D R R R U R R R)
          #(5 R U R R R U U U U R R R D D R R D R D D D R R U U U R R)))
(define (get-path-instruction level-number)
  (if (<= level-number (vector-length path-configurations))
      (vector-ref path-configurations (- level-number 1))
      #f))

;; wave configurations
(define wave-configurations
  (vector (vector 5 8)
          (vector 12 18)
          (vector 20 28)))
(define (get-wave-instruction level-number)
  (if (<= level-number (vector-length wave-configurations))
      (vector-ref wave-configurations (- level-number 1))
      #f))

;; procedures to simplify retrieving drawable files
(define (drawable-file title)
  (string-append "images/tiles-" (number->string cell-width) "px/" title ".png"))
(define (drawable-mask-file title)
  (drawable-file (string-append title "-mask")))
(define (drawable-mask-title title)
  (string-append title "-mask"))


;; tower & projectile configurations
(define small-range 1.5)
(define long-range 2.5)
(define tower-configurations (vector (vector "Stenentoren" "tower-steen" 250 long-range 2000 "steen" 120 (cons 15 15) 1 #f)
                                     (vector "Kanonstoren" "tower-kanonskogel" 400 long-range 2000 "kanonskogel" 100 (cons 30 30) 1 #f)
                                     (vector "Bommentoren" "tower-bom" 500 small-range 5000 "bom" 50 (cons 30 40) 3 #f)
                                     (vector "Nettentoren" "tower-net" 300 small-range 8000 "net" 120 (cons 100 100) 0 #t)))
(define (tower-config-name type)  (vector-ref (vector-ref tower-configurations type) 0))
(define (tower-config-image type)  (vector-ref (vector-ref tower-configurations type) 1))
(define (tower-config-price type) (vector-ref (vector-ref tower-configurations type) 2))
(define (tower-config-range type)  (vector-ref (vector-ref tower-configurations type) 3))
(define (tower-config-frequency type)  (vector-ref (vector-ref tower-configurations type) 4))
(define (tower-config-projectile-image type)  (vector-ref (vector-ref tower-configurations type) 5))
(define (tower-config-projectile-speed type)  (vector-ref (vector-ref tower-configurations type) 6))
(define (tower-config-projectile-width type)  (car (vector-ref (vector-ref tower-configurations type) 7)))
(define (tower-config-projectile-height type)  (cdr (vector-ref (vector-ref tower-configurations type) 7)))
(define (tower-config-projectile-damage type)  (vector-ref (vector-ref tower-configurations type) 8))
(define (tower-config-projectile-scale? type)  (vector-ref (vector-ref tower-configurations type) 9))

(define projectile-dimensions 10)
(define projectile-initial-delay 1)
(define collision-margin-coefficient 0.8)


;; monster configurations
(define monster-configurations (vector (vector "monster_red" 1 50 2 50)
                                       (vector "monster_blue" 3 60 2 75)
                                       (vector (list "monster_yellow_shielded" "monster_yellow") 2 30 3 85)
                                       (vector "monster_purple" 4 40 1 100)))
(define (monster-config-image type) (vector-ref (vector-ref monster-configurations type) 0))
(define (monster-config-lives type) (vector-ref (vector-ref monster-configurations type) 1))
(define (monster-config-speed type) (vector-ref (vector-ref monster-configurations type) 2))
(define (monster-config-damage type) (vector-ref (vector-ref monster-configurations type) 3))
(define (monster-config-money type) (vector-ref (vector-ref monster-configurations type) 4))

(define slow-down-time 3000)
(define slow-down-amount 20)
(define speed-up-amount 60) ; used when fired back by bomb tower
(define fired-back-distance 0.5)
(define bomb-delay-time 1000)
(define temporary-unshield-time 3500)
(define donating-lives-range 1)
(define bomb-explosion-range 1)
(define bomb-damage 3)

(define monster-spread-at-start 2) ; the amount of cells over which the monsters are randomly spawned in front of the path

(define healthbar-images (list "hb_1" "hb_2" "hb_3" "hb_4" "hb_5")) ; note that the maximum number of healthbar lives is 5
(define healthbar-masks (map (lambda (image) (string-append image "-mask")) healthbar-images))

(define monster-height 35)
(define monster-width 35)

;; power up configurations
(define power-ups-cool-down-time 7000)

(define tank-speed 160)
(define tank-damage 1)
(define tank-price 500)

(define obstacle-time 3000)
(define obstacle-amount 4)
(define obstacle-price 500)

(define drop-item-probability 0.4)
(define drop-item-time 6000)
(define droppable-items (vector 'tank 'obstacles))


(define (get-tower-picker-image tower-image) (string-append tower-image "-item"))

(define tower-item-selector-width 4)
(define tower-radius-border-width 4)

(define error-message-time 3000)

;; additional auxilary procedures
(define (sqr a)
  (* a a))

(define (ms->rounded-string ms)
  (number->string (ceiling (/ ms 1000))))

(define (get-random-element vector)
  (if (vector? vector)
      (vector-ref vector (inexact->exact (floor (* (random) (vector-length vector)))))
      (display "ERROR - get-random-element: given argument is not a vector")))

(define (vector-for-each f vec)
  (define (hulp index)
    (when (< index (vector-length vec))
      (f (vector-ref vec index))
      (hulp (+ index 1))))
  (hulp 0))

(define-syntax when
  (syntax-rules ()
    ((when test body ...)
     (if test
         (begin body ...)))))
