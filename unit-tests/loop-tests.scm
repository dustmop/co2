#lang racket

(require rackunit "../compile.scm")

(define (compile-code code)
  (clear-result)
  (clear-label-id)
  (make-variable! 'n)
  (process-form (datum->syntax #f code))
  (fetch-result))

(check-equal? (compile-code '(loop-down-from y 8 (lda 1)))
              '("  ldy #8"
                "_loop_down_from_0001:"
                "  lda #$1"
                "  dey"
                "  bne _loop_down_from_0001"))

(check-equal? (compile-code '(loop-down-from n 8 (lda 1)))
              '("  lda #8"
                "  sta n"
                "_loop_down_from_0001:"
                "  lda #$1"
                "  dec n"
                "  bne _loop_down_from_0001"))

(check-equal? (compile-code '(loop-up-to y 0 8 (lda 1)))
              '("  lda #$8"
                "  sta _loop"
                "  ldy #0"
                "_loop_up_to_0001:"
                "  lda #$1"
                "  iny"
                "  cpy _loop"
                "  bne _loop_up_to_0001"))

(check-equal? (compile-code '(loop-up-to n 0 8 (lda 1)))
              '("  lda #$8"
                "  sta _loop"
                "  lda #$0"
                "  sta n"
                "_loop_up_to_0001:"
                "  lda #$1"
                "  inc n"
                "  lda n"
                "  cmp _loop"
                "  bne _loop_up_to_0001"))

(check-equal? (compile-code '(loop y 0 8 (lda 1)))
              '("  lda #$8"
                "  sta _loop"
                "  ldy #0"
                "_loop_up_to_0001:"
                "  lda #$1"
                "  tya"
                "  iny"
                "  cmp _loop"
                "  bne _loop_up_to_0001"))

(check-equal? (compile-code '(loop n 0 8 (lda 1)))
              '("  lda #$8"
                "  sta _loop"
                "  lda #$0"
                "  sta n"
                "_loop_up_to_0001:"
                "  lda #$1"
                "  lda n"
                "  inc n"
                "  cmp _loop"
                "  bne _loop_up_to_0001"))
