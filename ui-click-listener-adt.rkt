(#%require (only racket/base error))

(define (make-ui-click-listener)
   (let ((updaters '()))

    (define (execute-click!)
      (for-each (lambda (updater) (updater))
                updaters))

    (define (add-updater! updater)
      (set! updaters (cons updater updaters)))
      

    (define (dispatch msg)
      (cond ((eq? msg 'execute-click!) execute-click!)
            ((eq? msg 'add-updater!) add-updater!)
            (else (error msg "message not understood by click-listener"))))
    dispatch))