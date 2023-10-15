#lang racket

(define (search-in-file-lines file txt)
  (with-input-from-file file
    (λ ()
      (let loop ([count 0])
        (let ((line (read-line)))
          (cond ((eof-object? line) #f)
                ((string-contains? line txt) count)
                (else (loop (add1 count)))))))))

(define (find-string-in-rkt-files txt)
  (define files (filter-files-by-extension (directory-list ".") ".rkt"))
  (for-each (lambda (file) (display-search-info file txt)) files))
; (define matches (map search-in-file-lines files))
; (newline))

(define (display-search-info path txt)
  (displayln (format "~a\t~a"
                     (search-in-file-lines path txt)
                     (path->string path))))

(define (filter-files-by-extension paths extension)
  (filter (λ (path) (string-suffix? (path->string path) extension))
          paths))

    
(define (string-contains? str sub)
  (regexp-match? (regexp sub) str))

(define (loop)
  (let* ((symb (read))
         (txt (symbol->string symb)))
    (find-string-in-rkt-files txt)
    (loop)))

(loop)
