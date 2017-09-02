#lang racket

(require rackunit "../compile.scm")

(define (compile-code code)
  (clear-result)
  (process-form (datum->syntax #f code))
  (fetch-result))

(check-equal? (compile-code '(+ 1 #\A)) '("  lda #$1"
                                          "  clc"
                                          "  adc #$41"))

(check-equal? (compile-code '(+ 1 #t)) '("  lda #$1"
                                         "  clc"
                                         "  adc #$ff"))

(check-equal? (compile-code '(+ 1 #f)) '("  lda #$1"
                                         "  clc"
                                         "  adc #$00"))
