#lang racket

(require rackunit "../compile.scm")

(define (compile-code code)
  (clear-result)
  (clear-label-id)
  (make-variable! 'm)
  (make-variable! 'n)
  (process-form (datum->syntax #f code))
  (fetch-result))

(check-equal? (compile-code '(> m n))
              '("  lda m"
                "  cmp n"
                "  beq _not_gt_0001"
                "  bcs _is_gt_0002"
                "_not_gt_0001:"
                "  lda #0"
                "  jmp _done_gt_0003"
                "_is_gt_0002:"
                "  lda #$ff"
                "_done_gt_0003:"))

(check-equal? (compile-code '(< m n))
              '("  lda m"
                "  cmp n"
                "  bcc _is_lt_0001"
                "  lda #0"
                "  jmp _done_lt_0002"
                "_is_lt_0001:"
                "  lda #$ff"
                "_done_lt_0002:"))

(check-equal? (compile-code '(<= m n))
              '("  lda m"
                "  cmp n"
                "  beq _is_lt_0001"
                "  bcc _is_lt_0001"
                "  lda #0"
                "  jmp _done_lt_0002"
                "_is_lt_0001:"
                "  lda #$ff"
                "_done_lt_0002:"))

(check-equal? (compile-code '(>= m n))
              '("  lda m"
                "  cmp n"
                "  bcs _is_gt_0001"
                "  lda #0"
                "  jmp _done_gt_0002"
                "_is_gt_0001:"
                "  lda #$ff"
                "_done_gt_0002:"))

(check-equal? (compile-code '(<s m n))
              '("  lda m"
                "  cmp n"
                "  bmi _is_lt_0001"
                "  lda #0"
                "  jmp _done_lt_0002"
                "_is_lt_0001:"
                "  lda #$ff"
                "_done_lt_0002:"))

(check-equal? (compile-code '(>s m n))
              '("  lda m"
                "  cmp n"
                "  beq _not_gt_0001"
                "  bpl _is_gt_0002"
                "_not_gt_0001:"
                "  lda #0"
                "  jmp _done_gt_0003"
                "_is_gt_0002:"
                "  lda #$ff"
                "_done_gt_0003:"
                ))

(check-equal? (compile-code '(<=s m n))
              '("  lda m"
                "  cmp n"
                "  beq _is_lt_0001"
                "  bmi _is_lt_0001"
                "  lda #0"
                "  jmp _done_lt_0002"
                "_is_lt_0001:"
                "  lda #$ff"
                "_done_lt_0002:"))
