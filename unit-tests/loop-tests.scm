#lang racket

(require rackunit "../compile.scm")
(require rackunit "../casla.scm")

(define (compile-code code)
  (clear-result)
  (clear-label-id)
  (make-variable! 'b)
  (make-address! 'data 0)
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
                "  sta __global__n"
                "_loop_down_from_0001:"
                "  lda #$1"
                "  dec __global__n"
                "  bne _loop_down_from_0001"))

(check-equal? (compile-code '(loop-up-to y 0 8 (lda 1)))
              '("  lda #$8"
                "  sta __global___gen_0001"
                "  ldy #0"
                "_loop_up_to_0002:"
                "  lda #$1"
                "  iny"
                "  cpy __global___gen_0001"
                "  bne _loop_up_to_0002"))

(check-equal? (compile-code '(loop-up-to n 0 8 (lda 1)))
              '("  lda #$8"
                "  sta __global___gen_0001"
                "  lda #$0"
                "  sta __global__n"
                "_loop_up_to_0002:"
                "  lda #$1"
                "  inc __global__n"
                "  lda __global__n"
                "  cmp __global___gen_0001"
                "  bne _loop_up_to_0002"))

(check-equal? (compile-code '(loop y 0 8 (lda 1)))
              '("  lda #$8"
                "  sta __global___gen_0001"
                "  ldy #0"
                "_loop_up_to_0002:"
                "  lda #$1"
                "  iny"
                "  cpy __global___gen_0001"
                "  bne _loop_up_to_0002"))

(check-equal? (compile-code '(loop n 0 8 (lda 1)))
              '("  lda #$8"
                "  sta __global___gen_0001"
                "  lda #$0"
                "  sta __global__n"
                "_loop_up_to_0002:"
                "  lda #$1"
                "  inc __global__n"
                "  lda __global__n"
                "  cmp __global___gen_0001"
                "  bne _loop_up_to_0002"))

(check-equal? (compile-code '(defsub (func)
                               (loop x 0 8
                                     (loop y 0 8 (lda 1)))))
              '(""
                "func:"
                "  lda #$8"
                "  sta _func___gen_0001"
                "  ldx #0"
                "_loop_up_to_0002:"
                "  lda #$8"
                "  sta _func___gen_0003"
                "  ldy #0"
                "_loop_up_to_0004:"
                "  lda #$1"
                "  iny"
                "  cpy _func___gen_0003"
                "  bne _loop_up_to_0004"
                "  inx"
                "  cpx _func___gen_0001"
                "  bne _loop_up_to_0002"
                "  rts"))
(begin
  (check-equal? (casla->allocations)
                '((func (_gensym "_func___gen_0001") 0)
                  (func (_gensym "_func___gen_0003") 1)))
  (clear-result)
  (generate-func-memory-addresses (casla->allocations))
  (check-equal? (fetch-result)
                '(""
                  ""
                  "_func___gen_0001 = $11"
                  "_func___gen_0003 = $12")))

(check-equal? (compile-code '(defsub (func)
                               (loop m 0 8
                                     (loop n 0 8 (lda 1)))))
              '(""
                "func:"
                "  lda #$8"
                "  sta _func___gen_0001"
                "  lda #$0"
                "  sta _func__m"
                "_loop_up_to_0002:"
                "  lda #$8"
                "  sta _func___gen_0003"
                "  lda #$0"
                "  sta _func__n"
                "_loop_up_to_0004:"
                "  lda #$1"
                "  inc _func__n"
                "  lda _func__n"
                "  cmp _func___gen_0003"
                "  bne _loop_up_to_0004"
                "  inc _func__m"
                "  lda _func__m"
                "  cmp _func___gen_0001"
                "  bne _loop_up_to_0002"
                "  rts"))
(begin
  (check-equal? (casla->allocations)
                '((func (_gensym "_func___gen_0001") 0)
                  (func m 1)
                  (func (_gensym "_func___gen_0003") 2)
                  (func n 3)))
  (clear-result)
  (generate-func-memory-addresses (casla->allocations))
  (check-equal? (fetch-result)
                '(""
                  ""
                  "_func___gen_0001 = $11"
                  "_func__m = $12"
                  "_func___gen_0003 = $13"
                  "_func__n = $14")))

(check-equal? (compile-code '(repeat (i 8)
                                     (set! b i)))
              '("  lda #0"
                "  sta b"
                "  lda #1"
                "  sta b"
                "  lda #2"
                "  sta b"
                "  lda #3"
                "  sta b"
                "  lda #4"
                "  sta b"
                "  lda #5"
                "  sta b"
                "  lda #6"
                "  sta b"
                "  lda #7"
                "  sta b"))
