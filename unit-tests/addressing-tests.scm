#lang racket

(require rackunit "../compile.scm")

(define (compile-code code)
  (clear-result)
  (make-variable! 'n)
  (process-form (datum->syntax #f code))
  (fetch-result))

(define (compile-code-first-error code)
  (clear-errors)
  (process-form (datum->syntax #f code))
  (first-error))

(check-equal? (compile-code '(lda #x10)) '("  lda #$10"))

(check-equal? (compile-code '(lda (addr #x100))) '("  lda $100"))

(check-equal? (compile-code '(lda (addr #x100) x)) '("  lda $100,x"))

(check-equal? (compile-code '(lda (addr #x100) y)) '("  lda $100,y"))

(check-equal? (compile-code-first-error '(sta #x10))
              "ERROR @ #f: Not a valid lvalue `16`")

(check-equal? (compile-code-first-error '(sta #x100))
              "ERROR @ #f: Not a valid lvalue `256`")

(check-equal? (compile-code '(sta (addr #x100))) '("  sta $100"))

(check-equal? (compile-code '(sta (addr #x100) x)) '("  sta $100,x"))

(check-equal? (compile-code '(sta (addr #x100) y)) '("  sta $100,y"))

(check-equal? (compile-code '(inc n))
              '("  inc n"))

(check-equal? (compile-code '(inc (high n)))
              '("  inc n+1"))

