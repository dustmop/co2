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
                              ((eq? n 10) (set! m 1) 15)
                              ((eq? n 20) (set! m 2) 200)
                              (else       (set! m 3) 33)))

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
                "  lda #$1"
                "  sta m"
                "  lda #$f"
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
                "  lda #$2"
                "  sta m"
                "  lda #$c8"
                "  jmp _if_done_0008"
                "_false_case_0007:"
                "  lda #$3"
                "  sta m"
                "  lda #$21"
                "_if_done_0008:"
                "_if_done_0003:"
                ))

(check-equal? (compile-code '(cond
                              ((eq? n 2) (set! m 22) 200)
                              ((eq? n 3) (set! m 35) 33)
                              ((eq? n 4) (set! m 42) 104)
                              ((eq? n 9) (set! m 99) 199)
                              (else      (set! m #xfe) #xff)))
              '("  ldy n"
                "  cpy #$2"
                "  bcc _cond_not_jump_0003"
                "  cpy #$5"
                "  bcs _cond_not_jump_0003"
                "  lda _cond_lookup_high_0002,y"
                "  pha"
                "  lda _cond_lookup_low_0001,y"
                "  pha"
                "  rts"
                "_cond_not_jump_0003:"
                "  jmp _cond_cases_0004"
                "_cond_jump_0006:"
                "  lda #$16"
                "  sta m"
                "  lda #$c8"
                "  jmp _cond_done_0005"
                "_cond_jump_0007:"
                "  lda #$23"
                "  sta m"
                "  lda #$21"
                "  jmp _cond_done_0005"
                "_cond_jump_0008:"
                "  lda #$2a"
                "  sta m"
                "  lda #$68"
                "  jmp _cond_done_0005"
                "_cond_lookup_low_0001_data:"
                ".byte <(_cond_jump_0006 - 1)"
                ".byte <(_cond_jump_0007 - 1)"
                ".byte <(_cond_jump_0008 - 1)"
                "_cond_lookup_low_0001 = _cond_lookup_low_0001_data - 2"
                "_cond_lookup_high_0002_data:"
                ".byte >(_cond_jump_0006 - 1)"
                ".byte >(_cond_jump_0007 - 1)"
                ".byte >(_cond_jump_0008 - 1)"
                "_cond_lookup_high_0002 = _cond_lookup_high_0002_data - 2"
                "_cond_cases_0004:"
                "  lda n"
                "  cmp #$9"
                "  beq _is_eq_000c"
                "  lda #0"
                "  jmp _done_eq_000d"
                "_is_eq_000c:"
                "  lda #$ff"
                "_done_eq_000d:"
                "  bne _truth_case_0009"
                "  jmp _false_case_000a"
                "_truth_case_0009:"
                "  lda #$63"
                "  sta m"
                "  lda #$c7"
                "  jmp _if_done_000b"
                "_false_case_000a:"
                "  lda #$fe"
                "  sta m"
                "  lda #$ff"
                "_if_done_000b:"
                "_cond_done_0005:"))
