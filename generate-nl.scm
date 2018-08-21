#lang racket

(require racket/cmdline)
(require data/gvector)

(define (has-comment? line)
  (and (> (string-length line) 32) (char=? (string-ref line 32) #\;)))

(define (has-label? line)
  (and (> (string-length line) 32)
       (regexp-match #px"^[\\w]+:" (substring line 32))))

(define (has-org? line)
  (string-contains? line ".org $8000"))

(define (get-label line)
  (cadr (regexp-match #px"^([\\w]+):" (substring line 32))))

(define (valid-address? text)
  (let ((c (string-ref text 0)))
    (or (char-alphabetic? c) (char-numeric? c))))

(define (process-listing lines out-name)
  (let ((count 0)
        (build (make-hash))
        (address #f)
        (label #f)
        (comment #f))
    (for ([line lines])
         (set! address #f)
         (set! label #f)
         (set! comment #f)
         (if (has-org? line)
             ; Org resets the labels hash.
             (when (> (hash-count build) 0)
                   (flush-symbols-hash build out-name count)
                   (set! build (make-hash))
                   (set! count (+ 1 count)))
             ; Otherwise-parse.
             (begin
               (when (has-comment? line)
                     (set! address (substring line 1 5))
                     (set! comment (substring line 33)))
               (when (has-label? line)
                     (set! address (substring line 1 5))
                     (set! label (get-label line)))
               (when (and address (valid-address? address))
                     (when (not (hash-has-key? build address))
                           (hash-set! build address (vector #f #f)))
                     (when label
                           (vector-set! (hash-ref build address) 0 label))
                     (when comment
                           (vector-set! (hash-ref build address) 1 comment))))))
    ; Flush one last time.
    (when (> (hash-count build) 0)
          (flush-symbols-hash build out-name count))))


(define (flush-symbols-hash sym-hash template count)
  (let ((keys #f) (address #f) (label #f) (comment #f) (f #f) (out-name #f))
    (set! out-name (string-replace template "%d" (format "~a" count)))
    (printf "Writing ~a symbols to ~a\n" (hash-count sym-hash) out-name)
    (set! f (open-output-file out-name #:exists 'replace))
    (set! keys (sort (hash-keys sym-hash) string<?))
    (for [(k keys)]
         (set! address k)
         (set! label (or (vector-ref (hash-ref sym-hash k) 0) ""))
         (set! comment (or (vector-ref (hash-ref sym-hash k) 1) ""))
         (write-string (format "$~a#~a#~a" address label comment) f)
         (newline f))
    (close-output-port f)))


(define (generate-nl in-name out-name)
  (process-listing (file->lines in-name) out-name))


(provide generate-nl)
