(load "tower-picker-item-adt.rkt")

(define (make-tower-picker-menu selected-index)
  (let ((tower-picker-menu-items (make-vector (vector-length tower-configurations)))
        (active-tower-index 0))

    ; Procedure that makes a tower picker menu in the sidebar
    (define (make-tower-picker-menu)
      (define (loop i)
        (when (< i (vector-length tower-configurations))
          (vector-set! tower-picker-menu-items i
                       (make-tower-picker-item (tower-config-name i)
                                               (tower-config-image i)))
          (loop (+ i 1))))
      (loop 0))

    ; Procedure returns a tower-picker-item in a given position and sets the index of the selected tower
    (define (select-tower-item-from-position position)
      (define (hulp index)
        (if (< index 0)
            #f
            (let* ((item (vector-ref tower-picker-menu-items index))
                   (item-position (item 'get-position)))
              (if (and (>= (position 'get-x) (item-position 'get-x))
                       (>= (position 'get-y) (item-position 'get-y)))
                  (begin
                    (set! active-tower-index index)
                    item)
                  (hulp (- index 1))))))
      (hulp (- (vector-length tower-picker-menu-items) 1))) ; Iteration starting at the end of the vector, to reduce the amount of conditional checks (in 'and').

    
    (define (dispatch msg)
      (cond ((eq? msg 'get-picker-items) tower-picker-menu-items)
            ((eq? msg 'select-tower-item-from-position) select-tower-item-from-position)
            ((eq? msg 'get-active-tower-index) active-tower-index)
            (else (display ("ERROR -- tower-adt dispatch message not understood: ")) (display msg))))
    (make-tower-picker-menu)
    dispatch))