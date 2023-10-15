(#%require (only racket/base error))
(load "constants-and-auxfunctions.rkt")

(define (make-ui-container type . args)
  (let ((x *UNINITIALISED*)
        (y *UNINITIALISED*)
        (w 0)
        (h 0))

    (define (build! screen init-x init-y)
      (set! x init-x)
      (set! y init-y)
      (cond ((or (eq? type 'row) (eq? type 'column))
             (build-children! screen args x y))
            ((eq? type 'selectable-column)
             (build-selectable-children! screen (car args) (cdr args) x y))
            ((eq? type 'image)
             (build-image! screen (car args) (cadr args)))
            ((eq? type 'scaled-image)
             (build-image! screen (cadr args) (caddr args) (car args)))
            ((eq? type 'static-text)
             (build-static-text! screen (car args) (cdr args)))
            ((eq? type 'dynamic-text)
             (build-dynamic-text! screen (car args) (cdr args)))
            ((eq? type 'padding)
             (build-child-with-padding! screen (car args) (cdr args)))
            ((eq? type 'button)
             (build-button! screen (cadr args)))
            ((eq? type 'v-space)
             (set! h (car args)))
            ((eq? type 'h-space)
             (set! w (car args)))
            (else (error 'build! "type not known"))))

    (define (click! screen click-x click-y)
      (cond ((or (eq? type 'row) (eq? type 'column))
             (click-children! screen args click-x click-y))
            ((eq? type 'padding)
             (click-child! screen (car args) click-x click-y))
            ((eq? type 'selectable-column)
             (select-from-children! screen (car args) (cdr args) click-x click-y))
            ((eq? type 'button)
             (execute-click! (car args)))))

    (define (get-clicked-index-and-child children click-x click-y)
      (define (iter index rest-children child-x child-y)
        (cond ((null? rest-children) #f)
              ((if (eq? type 'row)
                   (< click-x
                      (+ child-x ((car rest-children) 'get-w)))
                   (< click-y
                      (+ child-y ((car rest-children) 'get-h))))
               (cons index (car rest-children)))
              (else (iter (+ index 1) (cdr rest-children) (+ child-x ((car rest-children) 'get-w)) (+ child-y ((car rest-children) 'get-h))))))
      (iter 0 children x y))

    (define (click-children! screen children click-x click-y)
      (let ((index-and-child (get-clicked-index-and-child children click-x click-y)))
        (if index-and-child
            (click-child! screen (cdr index-and-child) click-x click-y))))

    (define (click-child! screen child click-x click-y)
      ((child 'click!) screen click-x click-y))

    (define (select-from-children! screen listener children click-x click-y)
      (let ((index-and-child (get-clicked-index-and-child children click-x click-y)))
        (if index-and-child
            (let ((index (car index-and-child))
                  (child (cdr index-and-child)))
              ((listener 'set-selected-index!) index)
              (select-child! screen child listener)))))

    (define (select-child! screen child listener)
      (let ((cc-x (child 'get-x))
            (cc-y (child 'get-y))
            (cc-h (child 'get-h)))
        ((screen 'draw-ui-element!) 'selection-box (list cc-x cc-y w cc-h listener))))

    (define (build-children! screen children child-x child-y)
      (for-each (lambda (child)
                  (build-child! screen child child-x child-y)
                  (let ((child-w (child 'get-w))
                        (child-h (child 'get-h)))
                    (cond ((eq? type 'row)
                           (set! child-x (+ child-x child-w))
                           (set! w (+ w child-w))
                           (set! h (max h child-h)))
                          ((or (eq? type 'column) (eq? type 'selectable-column))
                           (set! child-y (+ child-y child-h))
                           (set! h (+ h child-h))
                           (set! w (max w child-w))))))
                children))

    (define (build-selectable-children! screen listener children child-x child-y)
      (build-children! screen children child-x child-y) ; build the children
      (select-child! screen (car children) listener)); select the first child

    (define (build-image! screen image-path mask-path . args)
      (let ((image-tile ((screen 'draw-ui-element!) 'image (list x y image-path mask-path (if (null? args) 1 (car args))))))
        (set-w-h-from-tile! image-tile #f)))

    (define (build-child-with-padding! screen child optionals)
      (let ((v-padding (if (null? optionals) vertical-padding (car optionals)))
            (h-padding (if (or (null? optionals)
                               (null? (cdr optionals)))
                           horizontal-padding
                           (cadr optionals))))
        (build-child! screen child (+ x h-padding) (+ y v-padding))
        (set! h (+ (child 'get-h) (* 2 v-padding)))
        (set! w  (+ (child 'get-w) (* 2 h-padding)))))

    (define (build-static-text! screen text specifications)
      (let ((text-tile ((screen 'draw-ui-element!) 'text (list x y text specifications))))
        (set-w-h-from-tile! text-tile #f)))

    (define (build-dynamic-text! screen dynamic-text specifications)
      (let ((text-tile ((screen 'draw-ui-element!) 'text (list x y (dynamic-text 'get-text) specifications))))
        (set-w-h-from-tile! text-tile #f)
        ((dynamic-text 'add-updater!) update-dynamic-text! screen specifications text-tile)))

    (define (update-dynamic-text! text screen specifications text-tile)
      ((screen 'draw-ui-element!) 'text (list x y text specifications text-tile)))

    (define (build-child! screen child child-x child-y)
      ((child 'build!) screen child-x child-y))

    (define (build-button! screen child)
      (build-child! screen child x y)
      (set! h (+ (child 'get-h) (* 2 vertical-padding)))
      (set! w (+ (child 'get-w) (* 2 horizontal-padding))))

    (define (execute-click! listener)
      ((listener 'execute-click!)))
      

    (define (set-w-h-from-tile! tile padding?)
      (set! h (+ (tile 'get-h) (if padding? (* 2 vertical-padding) 0)))
      (set! w  (+ (tile 'get-w) (if padding? (* 2 horizontal-padding) 0))))

    (define (dispatch msg)
      (cond ((eq? msg 'build!) build!)
            ((eq? msg 'click!) click!)
            ((eq? msg 'get-w) w)
            ((eq? msg 'get-h) h)
            ((eq? msg 'get-x) x)
            ((eq? msg 'get-y) y)
            (else (error "message not understood by ui-stack" msg))))
    dispatch))

    

