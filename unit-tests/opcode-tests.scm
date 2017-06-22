#lang racket

(require rackunit "../compile.scm")

(define (compile-code code)
  (printf "Testing ~a\n" code)
  (clear-result)
  (process-form (datum->syntax #f code))
  (fetch-result))

(check-equal? (compile-code '(and 1)) '("  and #$1"))

(check-equal? (compile-code '(clc)) '("  clc"))

(check-equal? (compile-code '(cmp 1)) '("  cmp #$1"))

(check-equal? (compile-code '(cpx 1)) '("  cpx #$1"))

(check-equal? (compile-code '(cpy 1)) '("  cpy #$1"))

(check-equal? (compile-code '(eor 1)) '("  eor #$1"))

(check-equal? (compile-code '(lda 1)) '("  lda #$1"))

(check-equal? (compile-code '(ora 1)) '("  ora #$1"))

(check-equal? (compile-code '(sta "n")) '("  sta n"))

