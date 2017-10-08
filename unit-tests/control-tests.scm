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
                "  lda #$ff"
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
                "  lda #$ff"
                "_done_lt_0005:"
                "  pha"
                "  lda m"
                "  cmp #$14"
                "  bcc _is_lt_0006"
                "  lda #0"
                "  jmp _done_lt_0007"
                "_is_lt_0006:"
                "  lda #$ff"
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
                "  lda #$ff"
                "_done_lt_0005:"
                "  pha"
                "  lda m"
                "  cmp #$14"
                "  bcc _is_lt_0006"
                "  lda #0"
                "  jmp _done_lt_0007"
                "_is_lt_0006:"
                "  lda #$ff"
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
                "  lda #$ff"
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

(check-equal? (compile-code '(cond
                              ((eq? n 10) 100)
                              ((eq? n 20) 2000)
                              ((eq? n 30) 30000)))
              '("  lda n"
                "  cmp #$a"
                "  beq _is_eq_0004"
                "  lda #0"
                "  jmp _done_eq_0005"
                "_is_eq_0004:"
                "  lda #$ff"
                "_done_eq_0005:"
                "  bne _truth_case_0001"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$64"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda n"
                "  cmp #$14"
                "  beq _is_eq_0009"
                "  lda #0"
                "  jmp _done_eq_000a"
                "_is_eq_0009:"
                "  lda #$ff"
                "_done_eq_000a:"
                "  bne _truth_case_0006"
                "  jmp _false_case_0007"
                "_truth_case_0006:"
                "  lda #$7d0"
                "  jmp _if_done_0008"
                "_false_case_0007:"
                "  lda n"
                "  cmp #$1e"
                "  beq _is_eq_000e"
                "  lda #0"
                "  jmp _done_eq_000f"
                "_is_eq_000e:"
                "  lda #$ff"
                "_done_eq_000f:"
                "  bne _truth_case_000b"
                "  jmp _false_case_000c"
                "_truth_case_000b:"
                "  lda #$7530"
                "  jmp _if_done_000d"
                "_false_case_000c:"
                "  lda #$0"
                "_if_done_000d:"
                "_if_done_0008:"
                "_if_done_0003:"))
