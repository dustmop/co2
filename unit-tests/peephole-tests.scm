#lang racket

(require rackunit "../compile.scm")

(define (compile-code code)
  (clear-result)
  (clear-label-id)
  (make-variable! 'n)
  (set-optimization! #t)
  (set-peephole-optimization! #t)
  (process-form (datum->syntax #f code))
  (fetch-result))

(check-equal? (compile-code '(do (lda 3) (lda 4) (lda 5) (lda 5)))
              '("  lda #$3"
                "  lda #$4"
                "  lda #$5"
                "; AReg:omit   lda #$5"))

(check-equal? (compile-code '(do (lda 3) (ldx 4) (ldy 5) (lda 3) (ldx 6)))
              '("  lda #$3"
                "  ldx #$4"
                "  ldy #$5"
                "; AReg:omit   lda #$3"
                "  ldx #$6"))

(check-equal? (compile-code '(when (and (>= n 10) (< n 20)) #t))
              '("  lda n"
                "  cmp #$a"
                "  bcc _long_jump_0004"
                "; AReg:omit   lda n"
                "  cmp #$14"
                "  bcs _long_jump_0004"
                "  jmp _truth_case_0001"
                "_long_jump_0004:"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$ff"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$0"
                "_if_done_0003:"))

(check-equal? (compile-code '(when (and (>= n 10) (< n #x80)) #t))
              '("  lda n"
                "  cmp #$a"
                "  bcc _long_jump_0004"
                "   lda n"
                "  bmi _long_jump_0004"
                "  jmp _truth_case_0001"
                "_long_jump_0004:"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$ff"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$0"
                "_if_done_0003:"))

(check-equal? (compile-code '(when (and (>= n 10) (>= n #x80)) #t))
              '("  lda n"
                "  cmp #$a"
                "  bcc _long_jump_0004"
                "   lda n"
                "  bpl _long_jump_0004"
                "  jmp _truth_case_0001"
                "_long_jump_0004:"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$ff"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$0"
                "_if_done_0003:"))
