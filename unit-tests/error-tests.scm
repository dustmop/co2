#lang racket

(require rackunit "../compile.scm")

(define (compile-code-first-error code)
  (clear-errors)
  (process-form (datum->syntax #f code))
  (first-error))

(check-equal? (compile-code-first-error '(lda #x10)) #f)

(check-equal? (compile-code-first-error '(lad #x10))
              "ERROR @ #f: Not defined `lad`")

(check-equal? (compile-code-first-error '(lda x))
              "ERROR @ #f: Variable not found `x`")
