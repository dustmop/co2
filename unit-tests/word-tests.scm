#lang racket

(require rackunit "../compile.scm")

(define (compile-code code)
  (clear-result)
  (make-word! 'm)
  (make-word! 'n)
  (make-word! 'p)
  (make-variable! 's)
  (process-form (datum->syntax #f code))
  (fetch-result))

(define (compile-code-as-16-bit code)
  (clear-result)
  (make-word! 'm)
  (make-word! 'n)
  (make-word! 'p)
  (make-variable! 's)
  (process-form-as-16-bit (datum->syntax #f code))
  (fetch-result))

(check-equal? (compile-code '(set! m 17))
              '("  lda #17"
                "  ldx #0"
                "  sta m"
                "  stx m+1"))

(check-equal? (compile-code '(set! n 900))
              '("  lda #132"
                "  ldx #3"
                "  sta n"
                "  stx n+1"))

(check-equal? (compile-code '(set! p n))
              '("  lda n"
                "  ldx n+1"
                "  sta p"
                "  stx p+1"))

(check-equal? (compile-code '(set! n s))
              '("  lda s"
                "  ldx #0"
                "  sta n"
                "  stx n+1"))

(check-equal? (compile-code-as-16-bit '(+ m n))
              '("  lda m"
                "  ldx m+1"
                "  clc"
                "  adc n"
                "  sta _low_byte"
                "  txa"
                "  adc n+1"
                "  tax"
                "  lda _low_byte"))
