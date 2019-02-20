#lang racket

(require rackunit "../compile.scm")
(require rackunit "../casla.scm")

(let* ((test-code '(do (defsub (a s)
                         (b (+ s 1))
                         (c (+ s 2)))
                       (defsub (b t)
                         (set! m t))
                       (defsub (c u)
                         (set! n u))))
       (test-form (datum->syntax #f test-code)))
  (clear-result)
  (clear-var-allocation)
  (make-variable! 'm)
  (make-variable! 'n)
  (analyze-form test-form)
  (process-form test-form)
  (check-equal? (fetch-result)
                '(""
                  "a:"
                  "  sta _a__s"
                  "  lda _a__s"
                  "  clc"
                  "  adc #$1"
                  "  jsr b"
                  "  lda _a__s"
                  "  clc"
                  "  adc #$2"
                  "  jsr c"
                  "  rts"
                  ""
                  "b:"
                  "  sta _b__t"
                  "  lda _b__t"
                  "  sta m"
                  "  rts"
                  ""
                  "c:"
                  "  sta _c__u"
                  "  lda _c__u"
                  "  sta n"
                  "  rts"))
  (check-equal? (casla->allocations)
                '((a s 1)
                  (c u 0)
                  (b t 0)))
  (clear-result)
  (generate-func-memory-addresses (casla->allocations))
  (check-equal? (fetch-result)
                '(""
                  ""
                  "_a__s = $13"
                  "_c__u = $12"
                  "_b__t = $12"
                  "; max allocation = $13")))

(define (compile-code code)
  (clear-result)
  (clear-label-id)
  (make-variable! 'n)
  (make-variable! 'm)
  (process-form (datum->syntax #f code))
  (fetch-result))

(check-equal? (compile-code '(defsub (func s)
                               (inc s)
                               (dec s)
                               (lsr s)
                               (asl s)
                               (ror s)
                               (rol s)
                               (lda s)
                               (clc)
                               (adc s)
                               (sec)
                               (sbc s)
                               (sta n)))
              '(""
                "func:"
                "  sta _func__s"
                "  inc _func__s"
                "  dec _func__s"
                "  lsr _func__s"
                "  asl _func__s"
                "  ror _func__s"
                "  rol _func__s"
                "  lda _func__s"
                "  clc"
                "  adc _func__s"
                "  sec"
                "  sbc _func__s"
                "  sta n"
                "  rts"))
