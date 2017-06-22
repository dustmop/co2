#lang racket

(require rackunit "../compile.scm")

(define (compile-code code)
  (clear-result)
  (process-form (datum->syntax #f code))
  (fetch-result))

; TODO: Implement constant folding, which should break most of these tests.
(check-equal? (compile-code '(+ 1 2)) '("  lda #$1"
                                        "  clc"
                                        "  adc #$2"))

(check-equal? (compile-code '(<< 1 2)) '("  lda #$1"
                                         "  asl a"
                                         "  asl a"))

(check-equal? (compile-code '(>> 1 2)) '("  lda #$1"
                                         "  lsr a"
                                         "  lsr a"))

(check-equal? (compile-code '(* 3 4)) '("  lda #$4"
                                        "  bne _mul_start_0001"
                                        "  lda #0"
                                        "  jmp _mul_done_0004"
                                        "_mul_start_0001:"
                                        "  sta _count"
                                        "  lda #$3"
                                        "  sta _tmp"
                                        "  ldx #8"
                                        "  lda #0"
                                        "_mul_loop_0002:"
                                        "  asl a"
                                        "  asl _count"
                                        "  bcc _mul_inc_0003"
                                        "  clc"
                                        "  adc _tmp"
                                        "_mul_inc_0003:"
                                        "  dex"
                                        "  bne _mul_loop_0002"
                                        "_mul_done_0004:"))
