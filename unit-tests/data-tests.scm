#lang racket

(require rackunit "../compile.scm")

(define (compile-code code)
  (clear-result)
  (clear-var-allocation)
  (make-const! 'd 10)
  (process-form (datum->syntax #f code))
  (fetch-result))

(check-equal? (compile-code '(bytes 1 2 3))
              '(".byte $1,$2,$3"))

(check-equal? (compile-code '(bytes "abc"))
              '(".byte \"abc\""))

(check-equal? (compile-code '(bytes "test" #x10))
              '(".byte \"test\",$10"))

(check-equal? (compile-code '(bytes d))
              '(".byte d"))

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
