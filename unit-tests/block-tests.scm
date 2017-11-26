#lang racket

(require rackunit "../compile.scm")

(define (compile-code code)
  (clear-result)
  (clear-label-id)
  (make-variable! 'm)
  (make-variable! 'n)
  (make-variable! 'e)
  (make-variable! 'f)
  (process-form (datum->syntax #f code))
  (fetch-result))

(check-equal? (compile-code '(block SomeLabel
                                    (dec n)
                                    (bne SomeLabel)))
              '("_SomeLabel_0001:"
                "  dec n"
                "  bne _SomeLabel_0001"
                "_SomeLabel_0002:"))

(check-equal? (compile-code '(block SomeLabel
                                    (dec n)
                                    (bne SomeLabel)
                                    (dec m)
                                    (bne SomeLabel)))
              '("_SomeLabel_0001:"
                "  dec n"
                "  bne _SomeLabel_0001"
                "  dec m"
                "  bne _SomeLabel_0001"
                "_SomeLabel_0002:"))

(check-equal? (compile-code '(block SomeLabel
                                    (inc n)
                                    (jmp SomeLabel)))
              '("_SomeLabel_0001:"
                "  inc n"
                "  jmp _SomeLabel_0001"
                "_SomeLabel_0002:"))

(check-equal? (compile-code '(block SomeLabel
                                    (lda n)
                                    (beq #:break)
                                    (inc n)))
              '("_SomeLabel_0001:"
                "  lda n"
                "  beq _SomeLabel_0002"
                "  inc n"
                "_SomeLabel_0002:"))

(check-equal? (compile-code '(block SomeLabel
                                    (lda n)
                                    (beq #:break)
                                    (inc n)
                                    (bne SomeLabel)))
              '("_SomeLabel_0001:"
                "  lda n"
                "  beq _SomeLabel_0002"
                "  inc n"
                "  bne _SomeLabel_0001"
                "_SomeLabel_0002:"))

(check-equal? (compile-code '(block SomeLabel
                                    (if (eq? m 3)
                                        (inc e)
                                        0)
                                    (if (eq? n 4)
                                        (inc f)
                                        0)))
              '("_SomeLabel_0001:"
                "  lda m"
                "  cmp #$3"
                "  beq _is_eq_0006"
                "  lda #0"
                "  jmp _done_eq_0007"
                "_is_eq_0006:"
                "  lda #$ff"
                "_done_eq_0007:"
                "  bne _truth_case_0003"
                "  jmp _false_case_0004"
                "_truth_case_0003:"
                "  inc e"
                "  jmp _if_done_0005"
                "_false_case_0004:"
                "_if_done_0005:"
                "  lda n"
                "  cmp #$4"
                "  beq _is_eq_000b"
                "  lda #0"
                "  jmp _done_eq_000c"
                "_is_eq_000b:"
                "  lda #$ff"
                "_done_eq_000c:"
                "  bne _truth_case_0008"
                "  jmp _false_case_0009"
                "_truth_case_0008:"
                "  inc f"
                "  jmp _if_done_000a"
                "_false_case_0009:"
                "  lda #$0"
                "_if_done_000a:"
                "_SomeLabel_0002:"))

(check-equal? (compile-code '(block SomeLabel
                                    (when (eq? m 3)
                                          (inc e))
                                    (when (eq? n 4)
                                          (inc f))))
              '("_SomeLabel_0001:"
                "  lda m"
                "  cmp #$3"
                "  beq _is_eq_0006"
                "  lda #0"
                "  jmp _done_eq_0007"
                "_is_eq_0006:"
                "  lda #$ff"
                "_done_eq_0007:"
                "  bne _truth_case_0003"
                "  jmp _false_case_0004"
                "_truth_case_0003:"
                "  inc e"
                "  jmp _if_done_0005"
                "_false_case_0004:"
                "_if_done_0005:"
                "  lda n"
                "  cmp #$4"
                "  beq _is_eq_000b"
                "  lda #0"
                "  jmp _done_eq_000c"
                "_is_eq_000b:"
                "  lda #$ff"
                "_done_eq_000c:"
                "  bne _truth_case_0008"
                "  jmp _false_case_0009"
                "_truth_case_0008:"
                "  inc f"
                "  jmp _if_done_000a"
                "_false_case_0009:"
                "  lda #$0"
                "_if_done_000a:"
                "_SomeLabel_0002:"))
