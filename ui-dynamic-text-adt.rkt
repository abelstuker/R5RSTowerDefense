
(define (make-ui-dynamic-text text . args)
  (let* ((updaters '())
        (counter-default-value (if (null? args) #f (car args)))
        (counter 0))

    (define (update-text! new-text)
      (set! text new-text)
      (set! counter counter-default-value)
      (for-each (lambda (updater) ((vector-ref updater 0) text (vector-ref updater 1) (vector-ref updater 2) (vector-ref updater 3)))
                updaters))

    (define (update! ms)
      (when (and counter (< 0 counter))
        (set! counter (max 0 (- counter ms)))
        (if (= 0 counter) (update-text! ""))))

    (define (add-updater! updater screen specifications tile)
      (set! updaters (cons (vector updater screen specifications tile) updaters)))

    (define (dispatch msg)
      (cond ((eq? msg 'update!) update!)
            ((eq? msg 'update-text!) update-text!)
            ((eq? msg 'get-text) text)
            ((eq? msg 'add-updater!) add-updater!)))
    dispatch))