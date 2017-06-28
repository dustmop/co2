#lang racket

(require rackunit "../compile.scm")

(define (compile-code code)
  (clear-result)
  (process-form (datum->syntax #f code))
  (fetch-result))

(check-equal? (compile-code '(lda #x10)) '("  lda #$10"))

(check-equal? (compile-code '(lda (addr #x100))) '("  lda $100"))

(check-equal? (compile-code '(lda (addr #x100) x)) '("  lda $100,x"))

(check-equal? (compile-code '(lda (addr #x100) y)) '("  lda $100,y"))

(check-equal? (compile-code '(sta (addr #x100))) '("  sta $100"))

(check-equal? (compile-code '(sta (addr #x100) x)) '("  sta $100,x"))

(check-equal? (compile-code '(sta (addr #x100) y)) '("  sta $100,y"))
