#lang racket

(require rackunit "../compile.scm")

(define (compile-code code)
  (clear-result)
  (clear-label-id)
  (make-variable! 'm)
  (make-variable! 'n)
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
