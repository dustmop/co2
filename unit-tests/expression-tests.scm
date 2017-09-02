#lang racket

(require rackunit "../compile.scm")

(define (compile-code code)
  (clear-result)
  (clear-label-id)
  (make-variable! 'n)
  (make-variable! 'm)
  (process-form (datum->syntax #f code))
  (when (has-errors?)
        (printf "--------------------------------\n~a\n" (first-error)))
  (fetch-result))

(check-equal? (compile-code '(adc #x80))
              '("  adc #$80"))

(check-equal? (compile-code '(adc n #x80))
              '("  lda n"
                "  adc #$80"))

(check-equal? (compile-code '(adc #x80 n))
              '("  lda #$80"
                "  adc n"))

(check-equal? (compile-code '(adc (eor m #x80) n))
              '("  lda m"
                "  eor #$80"
                "  adc n"))

(check-equal? (compile-code '(adc n (eor m #x80)))
              '("  lda n"
                "  pha"
                "  lda m"
                "  eor #$80"
                "  sta _tmp"
                "  pla"
                "  adc _tmp"))

(check-equal? (compile-code '(adc n m 4 5))
              '("  lda n"
                "  adc m"
                "  adc #$4"
                "  adc #$5"))

(check-equal? (compile-code '(adc (eor m #x80) (ora n #x10)))
              '("  lda m"
                "  eor #$80"
                "  pha"
                "  lda n"
                "  ora #$10"
                "  sta _tmp"
                "  pla"
                "  adc _tmp"))

(check-equal? (compile-code '(and (eor m #x80) (ora n #x10)))
              '("  lda m"
                "  eor #$80"
                "  pha"
                "  lda n"
                "  ora #$10"
                "  sta _tmp"
                "  pla"
                "  and _tmp"))

(check-equal? (compile-code '(sbc 4))
              '("  sbc #$4"))

