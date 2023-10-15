#lang racket

(define (file-lines file)
  (with-input-from-file file
    (λ ()
      (let loop ([count 0])
        (let ((line (read-line)))
          (if (eof-object? line)
              count
              (loop (add1 count))))))))

(define (count-lines-in-rkt-files)
  (define files (filter-files-by-extension (directory-list ".") ".rkt"))
  (for-each display-path-info files)
  (define counts (map file-lines files))
  (newline)
  (displayln (format "total lines: ~a"
                     (apply + counts))))

(define (display-path-info path)
  (displayln (format "~a\t~a"
                     (file-lines path)
                     (path->string path))))

(define (filter-files-by-extension paths extension)
  (filter (λ (path) (string-suffix? (path->string path) extension))
          paths))

(count-lines-in-rkt-files)
