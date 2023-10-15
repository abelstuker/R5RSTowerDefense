(#%require (only racket/base error))

; A position exists of an x and y value of their position in the grid, and of a relative x and y value of their
;   position within the grid cell (optional).
(define (make-position init-x init-y)
  (let* ((x-grid (floor init-x))
         (y-grid (floor init-y))
         (x-rel (- init-x x-grid))
         (y-rel (- init-y y-grid)))

    (define (set-x-grid! new-x)
      (set! x-grid new-x))
    (define (set-y-grid! new-y)
      (set! y-grid new-y))

    (define (set-x-rel! new-relative-x)
      (set! x-rel new-relative-x)
      (check-update-position!))
    (define (set-y-rel! new-relative-y)
      (set! y-rel new-relative-y)
      (check-update-position!))

    (define (check-update-position!)
      (if (>= x-rel 1)
          (begin
            (set! x-grid (+ x-grid 1))
            (set! x-rel (- x-rel 1))))
      (if (>= y-rel 1)
          (begin
            (set! y-grid (+ y-grid 1))
            (set! y-rel (- y-rel 1))))
      (if (< y-rel 0)
          (begin
            (set! y-grid (- y-grid 1))
            (set! y-rel (+ 1 y-rel)))))

    ; This procedure compares the own grid position with the grid position of the argument.
    (define (compare? other-pos)
      (and (= x-grid (other-pos 'get-grid-x))
           (= y-grid (other-pos 'get-grid-y))))

    ; This procedure checks whether an object in a given list or vector contains an object with the own position.
    ;   The optional parameter can be used to determine the index to start searching when searching in a vector.
    ;   This was added to improve performance of monsters searching for their underlying pathcell
    ;     (they keep the index of their last underlying pathcel, the index of the next underlying pathcell can't have a smaller index)
    (define (object-in-position? objects . start-from)
      (define (hulp-lst lst)
        (cond ((null? lst) #f)
              ((symbol? (car lst)) (hulp-lst (cdr lst)))
              ((compare? ((car lst) 'get-position)) (car lst))
              (else (hulp-lst (cdr lst)))))
      
      (define (hulp-vector counter)
        (cond ((null? objects) #f)
              ((>= counter (vector-length objects)) #f)
              ((compare? ((vector-ref objects counter) 'get-position)) (vector-ref objects counter))
              (else (hulp-vector (+ counter 1)))))

      (cond ((and (not (null? start-from)) (vector? objects)) (hulp-vector (car start-from)))
            ((vector? objects) (hulp-vector 0))
            ((list? objects) (hulp-lst objects))))

    (define (object-at-position-with-dimensions? objects)
      (define (hulp-lst lst)
        (if (null? lst)
            #f
            (let* ((first (car lst))
                   (dims (first 'get-dimensions))
                   (pos (first 'get-position)))
              (if (in-area? pos dims)
                  first
                  (hulp-lst (cdr lst))))))

      (if (list? objects)
          (hulp-lst objects)))

      (define (in-area? pos-area dim-area)
      (let ((x (+ x-grid x-rel))
            (y (+ y-grid y-rel))
            (area-x (pos-area 'get-x))
            (area-y (pos-area 'get-y))
            (area-w (dim-area 'get-w))
            (area-h (dim-area 'get-h)))
        (and (< area-x x)
             (< area-y y)
             (> (+ area-x area-w) x)
             (> (+ area-y area-h) y))))

    (define (distance-to-other-position other-position)
      (sqrt (+ (sqr (- (+ x-grid x-rel) (other-position 'get-x)))
               (sqr (- (+ y-grid y-rel) (other-position 'get-y))))))

    

    ; The move! procedure moves the current position according to a given movement and the elapsed time (ms) since the last update.
    (define (move! movement ms)
      (let ((x-move (/ (* ms ((movement 'get-x-movement))) 100000))
            (y-move (/ (* ms ((movement 'get-y-movement))) 100000)))
        (set-x-rel! (+ x-rel x-move))
        (set-y-rel! (+ y-rel y-move))
        (sqrt (+ (sqr x-move) (sqr y-move)))))
      
    ; Returing the grid-position right from the current one and the leftmost grid-position of the row below the current one.
    (define (get-right)
      (make-position (+ x-grid 1) y-grid))
    (define (get-begin-nextline)
      (make-position 0 (+ y-grid 1)))


    (define (make-copy-position)
      (make-position (+ x-grid x-rel) (+ y-grid y-rel))) ; makes an entirely new position (that can be manipulated without interference with the current position)

    (define (make-center-position)
      (make-position (+ x-grid 0.5) (+ y-grid 0.5)))
    
    (define (make-anchored-position)
      (make-position x-grid y-grid))
    
    (define (dispatch msg)
      (cond ((eq? msg 'move!) move!)
            ((eq? msg 'get-x) (+ x-grid x-rel))
            ((eq? msg 'get-y) (+ y-grid y-rel))
            ((eq? msg 'get-grid-x) x-grid)
            ((eq? msg 'get-grid-y) y-grid)
           ; ((eq? msg 'get-relative-x) x-rel)
           ; ((eq? msg 'get-relative-y) y-rel)
           ; ((eq? msg 'set-x!) set-x-grid!)
           ; ((eq? msg 'set-y!) set-y-grid!)
           ; ((eq? msg 'set-relative-x!) set-x-rel!)
           ; ((eq? msg 'set-relative-y!) set-y-rel!)
            ((eq? msg 'compare?) compare?)
            ((eq? msg 'object-at-position-with-dimensions?) object-at-position-with-dimensions?)
            ((eq? msg 'object-in-position?) object-in-position?)
            ((eq? msg 'distance-to-other-position) distance-to-other-position)
            ((eq? msg 'make-center-position) make-center-position)
            ((eq? msg 'make-anchored-position) make-anchored-position)
           ; ((eq? msg 'make-copy-position) make-copy-position)
            (else (error msg "position-adt dispatch message not understood"))))
    dispatch))

