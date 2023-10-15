(#%require (only racket/base error))

(define (make-ui-selection-listener)
  (let ((selected-index 0)
        (selection-tile *UNINITIALISED*)
        (updaters '()))

    (define (set-selected-index! new-index)
      (for-each (lambda (updater) (updater new-index))
                updaters)
      (set! selected-index new-index))

    (define (set-selection-tile! new-tile)
      (set! selection-tile new-tile)
      new-tile)

    (define (add-updater! updater)
      (set! updaters (cons updater updaters)))
      

    (define (dispatch msg)
      (cond ((eq? msg 'get-selected-index) selected-index)
            ((eq? msg 'set-selected-index!) set-selected-index!)
            ((eq? msg 'set-selection-tile!) set-selection-tile!)
            ((eq? msg 'get-selection-tile) selection-tile)
            ((eq? msg 'add-updater!) add-updater!)
            (else (error msg "message not understood by selection-listener"))))
    dispatch))