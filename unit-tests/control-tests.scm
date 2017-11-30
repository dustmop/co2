#lang racket

(require rackunit "../compile.scm")

(define (compile-code code)
  (clear-result)
  (clear-label-id)
  (make-variable! 'n)
  (make-variable! 'm)
  (make-variable! 'p)
  (make-pointer! 'q)
  (make-address! 'b 1)
  (make-address! 'c 2)
  (make-address! 'd 3)
  (make-address! 'e 4)
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

(define (cond-body-unwrap branch)
  (let ((a (car branch))
        (b (cadr branch))
        (c (caddr branch))
        (d (cadddr branch)))
    (set! c (syntax->datum c))
    (set! d (syntax->datum d))
    (append (list a b c d))))

(define (compile-answer-table branches)
  (let ((a (build-answer-table (datum->syntax #f branches))))
    (if a
        (begin (hash-update! a 'branches (lambda (x) (map cond-body-unwrap x)))
               (hash->list a))
        #f)))

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
                "_long_jump_0004:"
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
                "  bcc _truth_case_0001"
                "_long_jump_0004:"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code '(if (>= n 10) 1 2))
              '("  lda n"
                "  cmp #$a"
                "  bcs _is_gt_0004"
                "  lda #0"
                "  jmp _done_gt_0005"
                "_is_gt_0004:"
                "  lda #$ff"
                "_done_gt_0005:"
                "  bne _truth_case_0001"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code-with-optimizations '(if (>= n 10) 1 2))
              '("  lda n"
                "  cmp #$a"
                "  bcs _truth_case_0001"
                "_long_jump_0004:"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code '(if (eq? n 10) 1 2))
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
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code-with-optimizations '(if (eq? n 10) 1 2))
              '("  lda n"
                "  cmp #$a"
                "  beq _truth_case_0001"
                "_long_jump_0004:"
                "  jmp _false_case_0002"
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
                "_long_jump_0004:"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"
                ))

(check-equal? (compile-code '(if (and (< n m) p) 1 2))
              '("  lda n"
                "  cmp m"
                "  bcc _is_lt_0004"
                "  lda #0"
                "  jmp _done_lt_0005"
                "_is_lt_0004:"
                "  lda #$ff"
                "_done_lt_0005:"
                "  and p"
                "  bne _truth_case_0001"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

; Since `(< n m)` is a boolean, but `p` is not, compile without short-circuit
; optimiziation. Otherwise, treating `p` like a boolean would produce invalid
; behavior.
; TODO: Check whether each argument is a boolean, to allow for mix and matching.
(check-equal? (compile-code-with-optimizations '(if (and (< n m) p) 1 2))
              '("  lda n"
                "  cmp m"
                "  bcc _is_lt_0005"
                "  lda #0"
                "  jmp _done_lt_0006"
                "_is_lt_0005:"
                "  lda #$ff"
                "_done_lt_0006:"
                "  and p"
                "  bne _truth_case_0001"
                "_long_jump_0004:"
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
                "  bcs _long_jump_0004"
                "  lda m"
                "  cmp #$14"
                "  bcs _long_jump_0004"
                "  jmp _truth_case_0001"
                "_long_jump_0004:"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code-with-optimizations '(if (and (< n 10) (< m 20)
                                                         (< p 30)) 1 2))
              '("  lda n"
                "  cmp #$a"
                "  bcs _long_jump_0004"
                "  lda m"
                "  cmp #$14"
                "  bcs _long_jump_0004"
                "  lda p"
                "  cmp #$1e"
                "  bcs _long_jump_0004"
                "  jmp _truth_case_0001"
                "_long_jump_0004:"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code '(if (and (and n m) p) 1 2))
              '("  lda n"
                "  and m"
                "  and p"
                "  bne _truth_case_0001"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

; Inner `and` doesn't return a boolean, so the only correct thing to do is
; to binary-and each of the parameters.
; Optimizing to checking `p` is non-zero is incorrect.
(check-equal? (compile-code-with-optimizations '(if (and (and n m) p) 1 2))
              '("  lda n"
                "  and m"
                "  and p"
                "  bne _truth_case_0001"
                "_long_jump_0004:"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code '(if (and (and (< n 10) (< m 20)) (< p 30)) 1 2))

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

; TODO: Currently broken because `and` is not assumed to be a boolean.
(check-equal? (compile-code-with-optimizations '(if (and (and (< n 10) (< m 20))
                                                         (< p 30)) 1 2))
              '("  lda n"
                "  cmp #$a"
                "  bcc _is_lt_0005"
                "  lda #0"
                "  jmp _done_lt_0006"
                "_is_lt_0005:"
                "  lda #$ff"
                "_done_lt_0006:"
                "  pha"
                "  lda m"
                "  cmp #$14"
                "  bcc _is_lt_0007"
                "  lda #0"
                "  jmp _done_lt_0008"
                "_is_lt_0007:"
                "  lda #$ff"
                "_done_lt_0008:"
                "  sta _tmp"
                "  pla"
                "  and _tmp"
                "  pha"
                "  lda p"
                "  cmp #$1e"
                "  bcc _is_lt_0009"
                "  lda #0"
                "  jmp _done_lt_000a"
                "_is_lt_0009:"
                "  lda #$ff"
                "_done_lt_000a:"
                "  sta _tmp"
                "  pla"
                "  and _tmp"
                "  bne _truth_case_0001"
                "_long_jump_0004:"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-answer-table '(((eq? n 10) (set! m 1) 15)
                                      ((eq? n 20) (set! m 2) 200)
                                      (else       (set! m 3) 33)))
              #f)

(check-equal? (compile-answer-table '(((eq? n 2) (set! m 22) 200)
                                      ((eq? n 3) (set! m 35) 33)
                                      ((eq? n 4) (set! m 42) 104)
                                      ((eq? n 9) (set! m 99) 199)
                                      (else      (set! m #xfe) #xff)))
              '((min . 2) (max . 4) (action . #f) (place . #f)
                (branches (2  #f (eq? n 2) (do (set! m 22)   200))
                          (3  #f (eq? n 3) (do (set! m 35)   33))
                          (4  #f (eq? n 4) (do (set! m 42)   104))
                          (9  #f (eq? n 9) (do (set! m 99)   199))
                          (#f #f else      (do (set! m #xfe) #xff)))
                (key . n)))

(check-equal? (compile-answer-table '(((eq? n 2) 200)
                                      ((eq? n 3) 33)
                                      ((eq? n 4) 104)
                                      ((eq? n 5) 155)))
              '((min . 2) (max . 5) (action . lda) (place . #f)
                (branches (2 200 (eq? n 2) (do 200))
                          (3  33 (eq? n 3) (do 33))
                          (4 104 (eq? n 4) (do 104))
                          (5 155 (eq? n 5) (do 155)))
                (key . n)))

(check-equal? (compile-answer-table '(((eq? n 2) (set! m 22))
                                      ((eq? n 3) (set! m 35))
                                      ((eq? n 4) (set! m 42))
                                      ((eq? n 5) (set! m 55))))
              '((min . 2) (max . 5) (action . set!) (place . m)
                (branches (2 22 (eq? n 2) (do (set! m 22)))
                          (3 35 (eq? n 3) (do (set! m 35)))
                          (4 42 (eq? n 4) (do (set! m 42)))
                          (5 55 (eq? n 5) (do (set! m 55))))
                (key . n)))

(check-equal? (compile-answer-table '(((eq? n 2) (set-pointer! p data-a))
                                      ((eq? n 3) (set-pointer! p data-b))
                                      ((eq? n 4) (set-pointer! p data-c))
                                      ((eq? n 5) (set-pointer! p data-d))))
              '((min . 2) (max . 5) (action . set-pointer!) (place . p)
                (branches (2 data-a (eq? n 2) (do (set-pointer! p data-a)))
                          (3 data-b (eq? n 3) (do (set-pointer! p data-b)))
                          (4 data-c (eq? n 4) (do (set-pointer! p data-c)))
                          (5 data-d (eq? n 5) (do (set-pointer! p data-d))))
                (key . n)))

(check-equal? (compile-answer-table '(((eq? n 2) (func m 22))
                                      ((eq? n 3) (func m 35))
                                      ((eq? n 4) (func m 42))
                                      ((eq? n 5) (func m 55))))
              '((min . 2) (max . 5) (action . #f) (place . m)
                (branches (2 22 (eq? n 2) (do (func m 22)))
                          (3 35 (eq? n 3) (do (func m 35)))
                          (4 42 (eq? n 4) (do (func m 42)))
                          (5 55 (eq? n 5) (do (func m 55))))
                (key . n)))

(check-equal? (compile-answer-table
               '([(and (>= val lower-a) (<= val upper-a)) 2]
                 [(and (>= val lower-b) (<= val lower-b)) 4]))
              #f)

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
                "  lda _cond_jump_high_0002,y"
                "  pha"
                "  lda _cond_jump_low_0001,y"
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
                "_cond_jump_low_0001_data:"
                ".byte <(_cond_jump_0006 - 1)"
                ".byte <(_cond_jump_0007 - 1)"
                ".byte <(_cond_jump_0008 - 1)"
                "_cond_jump_low_0001 = _cond_jump_low_0001_data - 2"
                "_cond_jump_high_0002_data:"
                ".byte >(_cond_jump_0006 - 1)"
                ".byte >(_cond_jump_0007 - 1)"
                ".byte >(_cond_jump_0008 - 1)"
                "_cond_jump_high_0002 = _cond_jump_high_0002_data - 2"
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

(check-equal? (compile-code '(cond
                              ((eq? n 2) (set! m 22))
                              ((eq? n 3) (set! m 35))
                              ((eq? n 4) (set! m 104))
                              ((eq? n 5) (set! m 155))))
              '("  ldy n"
                "  cpy #$2"
                "  bcc _cond_cases_0002"
                "  cpy #$6"
                "  bcs _cond_cases_0002"
                "  lda _cond_lookup_val_0001,y"
                "  sta m"
                "  jmp _cond_done_0003"
                "_cond_lookup_val_0001_data:"
                ".byte 22"
                ".byte 35"
                ".byte 104"
                ".byte 155"
                "_cond_lookup_val_0001 = _cond_lookup_val_0001_data - 2"
                "_cond_cases_0002:"
                "  lda #$0"
                "_cond_done_0003:"))

(check-equal? (compile-code '(cond
                              ((eq? n 2) 200)
                              ((eq? n 3) 33)
                              ((eq? n 4) 104)
                              ((eq? n 5) 55)))
              '("  ldy n"
                "  cpy #$2"
                "  bcc _cond_cases_0002"
                "  cpy #$6"
                "  bcs _cond_cases_0002"
                "  lda _cond_lookup_val_0001,y"
                "  jmp _cond_done_0003"
                "_cond_lookup_val_0001_data:"
                ".byte 200"
                ".byte 33"
                ".byte 104"
                ".byte 55"
                "_cond_lookup_val_0001 = _cond_lookup_val_0001_data - 2"
                "_cond_cases_0002:"
                "  lda #$0"
                "_cond_done_0003:"))

(check-equal? (compile-code '(cond
                              ((eq? n 2) (set-pointer! m b))
                              ((eq? n 3) (set-pointer! m c))
                              ((eq? n 4) (set-pointer! m d))
                              ((eq? n 5) (set-pointer! m e))))
              '("  ldy n"
                "  cpy #$2"
                "  bcc _cond_cases_0002"
                "  cpy #$6"
                "  bcs _cond_cases_0002"
                "  lda _cond_lookup_val_0001,y"
                "  sta m"
                "  lda _cond_lookup_hi_val_0004,y"
                "  sta m+1"
                "  jmp _cond_done_0003"
                "_cond_lookup_val_0001_data:"
                ".byte <b"
                ".byte <c"
                ".byte <d"
                ".byte <e"
                "_cond_lookup_val_0001 = _cond_lookup_val_0001_data - 2"
                "_cond_lookup_hi_val_0004_data:"
                ".byte >b"
                ".byte >c"
                ".byte >d"
                ".byte >e"
                "_cond_lookup_hi_val_0004 = _cond_lookup_hi_val_0004_data - 2"
                "_cond_cases_0002:"
                "  lda #$0"
                "_cond_done_0003:"))

(check-equal? (compile-code '(cond
                              ((eq? n 2) 200)
                              ((eq? n 3) 33)
                              ((eq? n 4) 104)
                              ((eq? n 9) 199)
                              (else      #xff)))
              '("  ldy n"
                "  cpy #$2"
                "  bcc _cond_cases_0002"
                "  cpy #$5"
                "  bcs _cond_cases_0002"
                "  lda _cond_lookup_val_0001,y"
                "  jmp _cond_done_0003"
                "_cond_lookup_val_0001_data:"
                ".byte 200"
                ".byte 33"
                ".byte 104"
                "_cond_lookup_val_0001 = _cond_lookup_val_0001_data - 2"
                "_cond_cases_0002:"
                "  lda n"
                "  cmp #$9"
                "  beq _is_eq_0007"
                "  lda #0"
                "  jmp _done_eq_0008"
                "_is_eq_0007:"
                "  lda #$ff"
                "_done_eq_0008:"
                "  bne _truth_case_0004"
                "  jmp _false_case_0005"
                "_truth_case_0004:"
                "  lda #$c7"
                "  jmp _if_done_0006"
                "_false_case_0005:"
                "  lda #$ff"
                "_if_done_0006:"
                "_cond_done_0003:"))

(check-equal? (compile-code '(if (or n m) 1 2))
              '("  lda n"
                "  ora m"
                "  bne _truth_case_0001"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code-with-optimizations '(if (or n m) 1 2))
              '("  lda n"
                "  ora m"
                "  bne _truth_case_0001"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code '(if (or (< n 10) (< m 20)) 1 2))
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
                "  ora _tmp"
                "  bne _truth_case_0001"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code-with-optimizations '(if (or (< n 10) (< m 20)) 1 2))
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
                "  ora _tmp"
                "  bne _truth_case_0001"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code '(if (not (< n 10)) 1 2))
              '("  lda n"
                "  cmp #$a"
                "  bcc _is_lt_0004"
                "  lda #0"
                "  jmp _done_lt_0005"
                "_is_lt_0004:"
                "  lda #$ff"
                "_done_lt_0005:"
                "  cmp #1"
                "  lda #$ff"
                "  adc #0"
                "  bne _truth_case_0001"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code-with-optimizations '(if (not (< n 10)) 1 2))
              '("  lda n"
                "  cmp #$a"
                "  bcs _truth_case_0001"
                "_long_jump_0004:"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code '(if (not (>= n 10)) 1 2))
              '("  lda n"
                "  cmp #$a"
                "  bcs _is_gt_0004"
                "  lda #0"
                "  jmp _done_gt_0005"
                "_is_gt_0004:"
                "  lda #$ff"
                "_done_gt_0005:"
                "  cmp #1"
                "  lda #$ff"
                "  adc #0"
                "  bne _truth_case_0001"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code-with-optimizations '(if (not (>= n 10)) 1 2))
              '("  lda n"
                "  cmp #$a"
                "  bcc _truth_case_0001"
                "_long_jump_0004:"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code '(if (not (eq? n 10)) 1 2))
              '("  lda n"
                "  cmp #$a"
                "  beq _is_eq_0004"
                "  lda #0"
                "  jmp _done_eq_0005"
                "_is_eq_0004:"
                "  lda #$ff"
                "_done_eq_0005:"
                "  cmp #1"
                "  lda #$ff"
                "  adc #0"
                "  bne _truth_case_0001"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code-with-optimizations '(if (not (eq? n 10)) 1 2))
              '("  lda n"
                "  cmp #$a"
                "  bne _truth_case_0001"
                "_long_jump_0004:"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code '(if (and (not (eq? n 10)) (not (eq? m 20))) 1 2))
              '("  lda n"
                "  cmp #$a"
                "  beq _is_eq_0004"
                "  lda #0"
                "  jmp _done_eq_0005"
                "_is_eq_0004:"
                "  lda #$ff"
                "_done_eq_0005:"
                "  cmp #1"
                "  lda #$ff"
                "  adc #0"
                "  pha"
                "  lda m"
                "  cmp #$14"
                "  beq _is_eq_0006"
                "  lda #0"
                "  jmp _done_eq_0007"
                "_is_eq_0006:"
                "  lda #$ff"
                "_done_eq_0007:"
                "  cmp #1"
                "  lda #$ff"
                "  adc #0"
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

(check-equal? (compile-code-with-optimizations '(if (and (not (eq? n 10))
                                                         (not (eq? m 20))) 1 2))
              '("  lda n"
                "  cmp #$a"
                "  beq _long_jump_0004"
                "  lda m"
                "  cmp #$14"
                "  beq _long_jump_0004"
                "  jmp _truth_case_0001"
                "_long_jump_0004:"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code '(if (not (and (eq? n 10) (eq? m 20))) 1 2))
              '("  lda n"
                "  cmp #$a"
                "  beq _is_eq_0004"
                "  lda #0"
                "  jmp _done_eq_0005"
                "_is_eq_0004:"
                "  lda #$ff"
                "_done_eq_0005:"
                "  pha"
                "  lda m"
                "  cmp #$14"
                "  beq _is_eq_0006"
                "  lda #0"
                "  jmp _done_eq_0007"
                "_is_eq_0006:"
                "  lda #$ff"
                "_done_eq_0007:"
                "  sta _tmp"
                "  pla"
                "  and _tmp"
                "  cmp #1"
                "  lda #$ff"
                "  adc #0"
                "  bne _truth_case_0001"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code-with-optimizations '(if (not (and (eq? n 10)
                                                              (eq? m 20))) 1 2))
              '("  lda n"
                "  cmp #$a"
                "  bne _long_jump_0004"
                "  lda m"
                "  cmp #$14"
                "  bne _long_jump_0004"
                "  jmp _false_case_0002"
                "_long_jump_0004:"
                "  jmp _truth_case_0001"
                "_truth_case_0001:"
                "  lda #$1"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda #$2"
                "_if_done_0003:"))

(check-equal? (compile-code '(if (eq? n 10) (< m 20) (< p 30)))
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
                "  lda m"
                "  cmp #$14"
                "  bcc _is_lt_0006"
                "  lda #0"
                "  jmp _done_lt_0007"
                "_is_lt_0006:"
                "  lda #$ff"
                "_done_lt_0007:"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda p"
                "  cmp #$1e"
                "  bcc _is_lt_0008"
                "  lda #0"
                "  jmp _done_lt_0009"
                "_is_lt_0008:"
                "  lda #$ff"
                "_done_lt_0009:"
                "_if_done_0003:"))

; Optimizations should not be used in the truth-case or false-case of `if`.
(check-equal? (compile-code-with-optimizations '(if (eq? n 10)
                                                    (< m 20) (< p 30)))
              '("  lda n"
                "  cmp #$a"
                "  beq _truth_case_0001"
                "_long_jump_0004:"
                "  jmp _false_case_0002"
                "_truth_case_0001:"
                "  lda m"
                "  cmp #$14"
                "  bcc _is_lt_0005"
                "  lda #0"
                "  jmp _done_lt_0006"
                "_is_lt_0005:"
                "  lda #$ff"
                "_done_lt_0006:"
                "  jmp _if_done_0003"
                "_false_case_0002:"
                "  lda p"
                "  cmp #$1e"
                "  bcc _is_lt_0007"
                "  lda #0"
                "  jmp _done_lt_0008"
                "_is_lt_0007:"
                "  lda #$ff"
                "_done_lt_0008:"
                "_if_done_0003:"))
