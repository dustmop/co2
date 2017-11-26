#lang racket

(require rackunit "../compile.scm")

(define (compile-code-first-error code)
  (clear-errors)
  (make-variable! 'n)
  (process-form (datum->syntax #f code))
  (first-error))

(check-equal? (compile-code-first-error '(#x2))
              "ERROR @ #f: Invalid function or macro name `2`")

(check-equal? (compile-code-first-error '(lda #x10)) #f)

(check-equal? (compile-code-first-error '(lad #x10))
              "ERROR @ #f: Not defined `lad`")

(check-equal? (compile-code-first-error '(lda x))
              "ERROR @ #f: Variable not found `x`")

(check-equal? (compile-code-first-error '(lda))
              "ERROR @ #f: Invalid arguments to \"lda\": `()`")

(check-equal? (compile-code-first-error '(set!))
              (string-append "ERROR @ #f: Need 2 arguments, only got 0, "
                             "for `process-set-bang`"))

(check-equal? (compile-code-first-error '(set! n))
              (string-append "ERROR @ #f: Need 2 arguments, only got 1, "
                             "for `process-set-bang`"))

(check-equal? (compile-code-first-error '(set! 10 n))
              "ERROR @ #f: Cannot assign to `10`")

(check-equal? (compile-code-first-error '(if))
              "ERROR @ #f: Need 3 arguments, only got 0, for `if`")

(check-equal? (compile-code-first-error '(if 1))
              "ERROR @ #f: Need 3 arguments, only got 1, for `if`")

(check-equal? (compile-code-first-error '(if 1 n))
              "ERROR @ #f: Need 3 arguments, only got 2, for `if`")
