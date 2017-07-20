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
                  "_a__s = $41"
                  "_c__u = $40"
                  "_b__t = $40")))

