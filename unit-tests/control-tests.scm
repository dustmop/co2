#lang racket

(require rackunit "../compile.scm")

(define (compile-code code)
  (clear-result)
  (clear-label-id)
  (make-variable! 'n)
  (set-optimization! #f)
  (process-form (datum->syntax #f code))
  (fetch-result))

(define (compile-code-with-optimizations code)
  (clear-result)
  (clear-label-id)
  (make-variable! 'n)
  (set-optimization! #t)
  (process-form (datum->syntax #f code))
  (fetch-result))

(check-equal? (compile-code '(rts)) '("  rts"))

(check-equal? (compile-code '(jsr "abc")) '("  jsr abc"))

(check-equal? (compile-code '(if n 1 2))
              '("  lda n"
                "  bne _truth_case_0001"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code-with-optimizations '(if n 1 2))
              '("  lda n"
                "  bne _truth_case_0001"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code '(if (< n 10) 1 2))
              '("  lda n"
                "  cmp #$a"
                "  bcc _is_lt_0004"
                "  lda #0"
                "  jmp _done_lt_0005"
                "_is_lt_0004:"
                "  lda #1"
                "_done_lt_0005:"
                "  bne _truth_case_0001"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code-with-optimizations '(if (< n 10) 1 2))
              '("  lda n"
                "  cmp #$a"
                "  bcc _is_lt_0004"
                "  jmp _false_case_0002"
                "_is_lt_0004:"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))
