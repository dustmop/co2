#lang racket

(require rackunit "../compile.scm")

(define (compile-code code)
  (clear-result)
  (clear-label-id)
  (make-variable! 'n)
  (make-variable! 'm)
  (make-variable! 'p)
  (set-optimization! #f)
  (process-form (datum->syntax #f code))
  (when (has-errors?)
        (printf "--------------------------------\n~a\n" (first-error)))
  (fetch-result))

(define (compile-code-with-optimizations code)
  (clear-result)
  (clear-label-id)
  (make-variable! 'n)
  (make-variable! 'm)
  (make-variable! 'p)
  (set-optimization! #t)
  (process-form (datum->syntax #f code))
  (when (has-errors?)
        (printf "--------------------------------\n~a\n" (first-error)))
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

(check-equal? (compile-code '(if (and n m) 1 2))
              '("  lda n"
                "  and m"
                "  bne _truth_case_0001"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code-with-optimizations '(if (and n m) 1 2))
              '("  lda n"
                "  and m"
                "  bne _truth_case_0001"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code '(if (and (< n 10) (< m 20)) 1 2))
              '("  lda n"
                "  cmp #$a"
                "  bcc _is_lt_0004"
                "  lda #0"
                "  jmp _done_lt_0005"
                "_is_lt_0004:"
                "  lda #1"
                "_done_lt_0005:"
                "  pha"
                "  lda m"
                "  cmp #$14"
                "  bcc _is_lt_0006"
                "  lda #0"
                "  jmp _done_lt_0007"
                "_is_lt_0006:"
                "  lda #1"
                "_done_lt_0007:"
                "  sta _tmp"
                "  pla"
                "  and _tmp"
                "  bne _truth_case_0001"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code-with-optimizations '(if (and (< n 10) (< m 20))
                                                    1 2))
              '("  lda n"
                "  cmp #$a"
                "  bcc _is_lt_0004"
                "  jmp _false_case_0002"
                "_is_lt_0004:"
                "  lda m"
                "  cmp #$14"
                "  bcc _is_lt_0006"
                "  jmp _false_case_0002"
                "_is_lt_0006:"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"
                ))

; No optimizations because, for now, `and` only optimizes with 3 parameters.
(check-equal? (compile-code-with-optimizations '(if (and (< n 10) (< m 20)
                                                         (< p 30)) 1 2))
              '("  lda n"
                "  cmp #$a"
                "  bcc _is_lt_0004"
                "  lda #0"
                "  jmp _done_lt_0005"
                "_is_lt_0004:"
                "  lda #1"
                "_done_lt_0005:"
                "  pha"
                "  lda m"
                "  cmp #$14"
                "  bcc _is_lt_0006"
                "  lda #0"
                "  jmp _done_lt_0007"
                "_is_lt_0006:"
                "  lda #1"
                "_done_lt_0007:"
                "  sta _tmp"
                "  pla"
                "  and _tmp"
                "  pha"
                "  lda p"
                "  cmp #$1e"
                "  bcc _is_lt_0008"
                "  lda #0"
                "  jmp _done_lt_0009"
                "_is_lt_0008:"
                "  lda #1"
                "_done_lt_0009:"
                "  sta _tmp"
                "  pla"
                "  and _tmp"
                "  bne _truth_case_0001"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))
