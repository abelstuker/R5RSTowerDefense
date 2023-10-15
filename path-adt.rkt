(#%require (only racket/base error))
(load "constants-and-auxfunctions.rkt")
(load "position-adt.rkt")
(load "pathcell-adt.rkt")

(define (make-path instructions)
  
  (let ((path (make-vector (- (vector-length instructions) 1) #f))
        (start-row (vector-ref instructions 0)))
    
    (define (fill-path-vector idx prev row col)
                   
      (if (< idx (vector-length instructions))
          (let ((current (vector-ref instructions idx))
                (is-first? (and (eq? prev 'R) (= col 0)))
                (is-last? (= (+ idx 1) (vector-length instructions))))
            (cond ((and (eq? current 'R)
                        (eq? prev 'R))
                   (vector-set! path (- idx 1) (make-pathcell 'left
                                        'right
                                        (make-position col row)
                                        is-first?
                                        is-last?))
                         (fill-path-vector (+ idx 1) current row (+ col 1)))
                  ((and (eq? current 'R)
                        (eq? prev 'U))
                   (vector-set! path (- idx 1) (make-pathcell 'down
                                        'right
                                        (make-position col row)
                                        is-first?
                                        is-last?))
                         (fill-path-vector (+ idx 1) current row (+ col 1)))
                  ((and (eq? current 'R)
                        (eq? prev 'D))
                   (vector-set! path (- idx 1) (make-pathcell 'up
                                        'right
                                        (make-position col row)
                                        is-first?
                                        is-last?))
                         (fill-path-vector (+ idx 1) current row (+ col 1)))
                  ((and (eq? current 'U)
                        (eq? prev 'R))
                   (vector-set! path (- idx 1) (make-pathcell 'left
                                        'up
                                        (make-position col row)
                                        is-first?
                                        is-last?))
                         (fill-path-vector (+ idx 1) current (- row 1) col))
                  ((and (eq? current 'U)
                        (eq? prev 'U))
                   (vector-set! path (- idx 1) (make-pathcell 'down
                                        'up
                                        (make-position col row)
                                        is-first?
                                        is-last?))
                         (fill-path-vector (+ idx 1) current (- row 1) col))
                  ((and (eq? current 'D)
                        (eq? prev 'R))
                   (vector-set! path (- idx 1) (make-pathcell 'left
                                        'down
                                        (make-position col row)
                                        is-first?
                                        is-last?))
                         (fill-path-vector (+ idx 1) current (+ row 1) col))
                  ((and (eq? current 'D)
                        (eq? prev 'D))
                   (vector-set! path (- idx 1) (make-pathcell 'up
                                        'down
                                        (make-position col row)
                                        is-first?
                                        is-last?))
                         (fill-path-vector (+ idx 1) current (+ row 1) col))
                  (else (display "unknown path instruction"))))))
    
    (if (and (number? start-row)
             (< start-row vertical-cells-amount))
        (fill-path-vector 1 'R start-row 0) ; note that prev is initially 'R since the path is generated beginning from column 0
        (error "wrong formattation of instructions: first element should be the index of the row where the path starts"))

    (define (get-nearest-pathcell position)
      (define (hulp index)
        (cond ((< index 0) #f)
              ((<= ((position 'distance-to-other-position) ((vector-ref path index) 'get-position)) 1) (vector-ref path index))
              (else (hulp (- index 1)))))
      (hulp (- (vector-length path) 1)))
          
          

    ;  (define (generate-path)
    ;    (define (iter path )
    ;      (newline) (display "path iter: ")
    ;      (let* ((curr (if (null? path) #f ((car path) 'get-new-position)))
    ;             (last (if (or (not curr) (null? (cdr path))) #f ((cadr path) 'get-new-position))))
    ;          
    ;        (cond ((null? path) (display "1c ") (iter (cons (make-pathcell (vector 'right)
    ;                                                                       'left
    ;                                                                       (make-position 0 (+ 1 (random (- vertical-cells-amount 2))))
    ;                                                                       screen (length path))
    ;                                                        path)))
    ;              ((null? (cdr path)) (display "2c ") (iter (cons (make-pathcell (vector 'right 'down 'up)
    ;                                                                             'left
    ;                                                                             curr
    ;                                                                             screen (length path)) path))) ; 
    ;              ((equal? (curr 'get-x) (- horizontal-cells-amount 0)) (display "3c ") (reverse path))
    ;              ((< (curr 'get-y) 1) (display "4c ") 
    ;                                   (if (equal? (curr 'get-y) (- (last 'get-y) 1))
    ;                                       (iter (cons (make-pathcell (vector 'right) 'down curr screen (length path)) path))
    ;                                       (iter (cons (make-pathcell (vector 'right 'down) 'left curr screen (length path)) path))))
    ;              ((> (curr 'get-y) (- vertical-cells-amount 2)) (display "5c ") 
    ;                                                             (if (equal? (curr 'get-y) (+ (last 'get-y) 1))
    ;                                                                 (iter (cons (make-pathcell (vector 'right) 'up  curr screen (length path)) path))
    ;                                                                 (iter (cons (make-pathcell (vector 'right 'up) 'left curr screen (length path)) path))))
    ;              ((equal? (curr 'get-y) (- (last 'get-y) 1)) (display "6c ") (iter (cons (make-pathcell (vector 'right 'up) 'down curr screen (length path)) path)))
    ;              ((equal? (curr 'get-y) (+ (last 'get-y) 1)) (display "7c ") (iter (cons (make-pathcell (vector 'right 'down) 'up curr screen (length path)) path)))
    ;              (else (display "else c") (iter (cons (make-pathcell (vector 'right 'up 'down 'up 'down) 'left curr screen (length path)) path))))))
    ;    (iter '()))




    (define (get-pathcell-from-index index)
      (vector-ref path index))

    (define (for-each-pathcell f)
      (vector-for-each f path))

    (define (get-path-length)
      (vector-length path))
    
    (define (dispatch msg)
      (cond ((eq? msg 'get-path-vector) path)
            ((eq? msg 'for-each-pathcell) for-each-pathcell)
            ((eq? msg 'get-nearest-pathcell) get-nearest-pathcell)
            ((eq? msg 'get-pathcell-from-index) get-pathcell-from-index)
            ((eq? msg 'get-path-length) get-path-length)
            (else (display "ERROR -- path-adt dispatch message not understood: ") (display msg))))
    dispatch))
