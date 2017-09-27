#lang racket

(require rackunit "../compile.scm")

(define (compile-code code)
  (clear-result)
  (assign-include-base!
    (path->complete-path (find-system-path 'run-file)))
  (process-form (datum->syntax #f code))
  (fetch-result))

(check-equal? (compile-code '()) '())

(check-equal? (compile-code '1) '("  lda #$1"))

(check-equal? (compile-code '(include "testdata/sub.co2"))
              '("  ldx #$4"
                "  ldy #$5"))
