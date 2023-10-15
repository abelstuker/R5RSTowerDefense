(#%require (only racket/base random))
(load "constants-and-auxfunctions.rkt")
(load "obstacle-adt.rkt")

(define (make-obstacle-delegate path)
  
  (define (spawn-obstacles amount)
    (define checked-indices '())
    (define vec (make-vector amount *UNINITIALISED*))
    (define (loop-until-found i)
      (if (< i 1000)
          (let* ((index (inexact->exact (floor (* (random) ((path 'get-path-length))))))
                 (pathcell ((path 'get-pathcell-from-index) index))
                 (entry (pathcell 'get-entry))
                 (exit (pathcell 'get-exit)))
            (if (or (member index checked-indices)
                    (= index 0))
                (loop-until-found (+ i 1))
                (begin
                  (set! checked-indices (cons index checked-indices))
                  (cond ((and (eq? entry 'left) (eq? exit 'right))
                         (make-obstacle pathcell 'vertical))
                        ((or (and (eq? entry 'up) (eq? exit 'down))
                             (and (eq? entry 'down) (eq? exit 'up)))
                         (make-obstacle pathcell 'horizontal))
                        (else (loop-until-found (+ i 1)))))))
          (begin (newline) (display "            ") (display vec) (newline))))
        
    (define (iter rest-amount)
      (if (< 0 rest-amount)
          (begin
            (vector-set! vec (- rest-amount 1) (loop-until-found 0))
            (iter (- rest-amount 1)))
          vec))
    (iter amount))
      
  (let* ((amount obstacle-amount);(inexact->exact (floor (+ 1 (* (random) 2.99)))))
         (obstacles (spawn-obstacles amount))
         (timer (make-timer obstacle-time)))

    (define (update! ms level)
      (if (not ((timer 'ended?)))
          ((timer 'update!) ms)
          ((level 'remove-power-up!))))
                       

    (define (dispatch msg)
      (cond ((eq? msg 'update!) update!)
            ((eq? msg 'get-obstacles) obstacles)
            ((eq? msg 'get-timer) timer)
            (else (display msg) (display " ERROR -- message not understood by TEMPORARY OBSTACLE DELEGATE"))))
    dispatch))




