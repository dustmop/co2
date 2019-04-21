#lang racket

(require rackunit "../compile.scm")

(define (compile-code code)
  (clear-result)
  (make-variable! 'n)
  (process-form (datum->syntax #f code))
  (fetch-result))

(check-equal? (compile-code '(asl))   '("  asl a"))

(check-equal? (compile-code '(asl a)) '("  asl a"))

(check-equal? (compile-code '(asl n)) '("  asl n"))

(check-equal? (compile-code '(inc n)) '("  inc n"))

(check-equal? (compile-code '(lsr n)) '("  lsr n"))

(check-equal? (compile-code '(rol))   '("  rol a"))

(check-equal? (compile-code '(rol n)) '("  rol n"))

(check-equal? (compile-code '(ror n)) '("  ror n"))
