
(define (make-timer time)

  (define (set-time! new-time)
    (set! time new-time))

  (define (update! ms)
    (set! time (- time ms)))

  (define (ended?)
    (<= time 0))

  (define (dispatch msg)
    (cond
      ((eq? msg 'set-time!) set-time!)
      ((eq? msg 'update!) update!)
      ((eq? msg 'ended?) ended?)
      ((eq? msg 'get-time) time)))
  dispatch)