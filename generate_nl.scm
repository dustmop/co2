#lang racket

(require racket/cmdline)
(require data/gvector)

(define (has-comment? line)
  (and (> (string-length line) 32) (char=? (string-ref line 32) #\;)))

(define (has-label? line)
  (and (> (string-length line) 32)
       (regexp-match #px"^[\\w]+:" (substring line 32))))

(define (get-label line)
  (cadr (regexp-match #px"^([\\w]+):" (substring line 32))))

(define (valid-address? text)
  (let ((c (string-ref text 0)))
    (or (char-alphabetic? c) (char-numeric? c))))

(define (generate-nl lines)
  (let ((count 0)
        (build (make-gvector)))
    (for ([line lines])
         (when (has-comment? line)
               (let ((address (substring line 1 5))
                     (text (substring line 33)))
                 (when (valid-address? address)
                       (gvector-add! build (format "$~a##~a" address text)))))
         (when (has-label? line)
               (let ((address (substring line 1 5))
                     (label (get-label line)))
                 (when (valid-address? address)
                       (gvector-add! build (format "$~a#~a#" address label))))))
    (for ([elem (gvector->list build)])
         (printf "~a\n" elem))))

(let* ((fname (command-line #:args (input) input)))
  (generate-nl (file->lines fname)))
