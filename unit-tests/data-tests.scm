#lang racket

(require rackunit "../compile.scm")
(require rackunit "../casla.scm")

(define (compile-code code)
  (clear-result)
  (clear-var-allocation)
  (clear-data-segment)
  (make-address! 'out #x8000)
  (make-address! 'data 0)
  (make-const! 'd 10)
  (analyze-form (datum->syntax #f code))
  (process-form (datum->syntax #f code))
  (when (has-errors?)
        (printf "--------------------------------\n~a\n" (first-error)))
  (fetch-result))

(define (compile-code-first-error code)
  (clear-errors)
  (process-form (datum->syntax #f code))
  (first-error))

(check-equal? (compile-code '(bytes 1 2 3))
              '(".byte $1,$2,$3"))

(check-equal? (compile-code '(bytes "abc"))
              '(".byte \"abc\""))

(check-equal? (compile-code '(bytes "test" #\newline))
              '(".byte \"test\",10"))

(check-equal? (compile-code '(bytes "a" #x42 67))
              '(".byte \"a\",$42,$43"))

(check-equal? (compile-code '(bytes d))
              '(".byte d"))

(check-equal? (compile-code '(bytes data))
              '(".byte <data,>data"))

(check-equal? (compile-code '(bytes (1 2)))
              '(".byte $1,$2"))

(check-equal? (compile-code '(bytes (d 3)))
              '(".byte d,$3"))

(check-equal? (compile-code '(defvar s))
              '(""
                "s = $10"))

(check-equal? (compile-code '(do (defvar s) (defvar t)))
              '(""
                "s = $10"
                ""
                "t = $11"))

(check-equal? (compile-code '(do (defbuffer s #x10) (defvar t)))
              '(""
                "s = $10"
                ""
                "t = $20"))

(check-equal? (compile-code '(do (defword s) (defword t)))
              '(""
                "s = $10"
                ""
                "t = $12"))

(check-equal? (compile-code '(include-binary graphics "gfx.dat"))
              '(""
                "graphics:"
                ".incbin \"gfx.dat\""))

(check-equal? (compile-code '(include-binary graphics "gfx.dat" :size #x1000))
              '(""
                "graphics:"
                ".incbin \"gfx.dat\",0,$1000"))

(check-equal? (compile-code '(defsub (func)
                               (let ((some-bytes '(#x10 #x22 #x3f)))
                                 (set! out (peek some-bytes 2)))))
              '("" "func:" "  lda some_bytes+2" "  sta out" "  rts"))
(check-equal? (get-data-segment)
              '(("some_bytes" (16 34 63))))

(check-equal? (compile-code '(defsub (func m)
                               (let ((n 0) (q))
                                 (set! n 1)
                                 (set! q 2)
                                 (set! out (+ (+ m n) q)))))
              '(""
                "func:"
                "  sta _func__m"
                "  lda #$0"
                "  sta _func__n"
                "  lda #$1"
                "  sta _func__n"
                "  lda #$2"
                "  sta _func__q"
                "  lda _func__m"
                "  clc"
                "  adc _func__n"
                "  clc"
                "  adc _func__q"
                "  sta out"
                "  rts"))
(check-equal? (casla->allocations)
              '((func m 0)
                (func n 1)
                (func q 2)))

(check-equal? (compile-code-first-error '(defsub (func m)
                                           (let ((n 0) (q))
                                             (set! n 1)
                                             (set! q 2))
                                           n))
              "ERROR @ #f: Variable not found `n`")
