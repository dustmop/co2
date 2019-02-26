#lang racket

(require rackunit "../compile.scm")

(define (compile-code code)
  (clear-result)
  (make-function! 'f '() #:ignore-redefine #t)
  (make-variable! 'n)
  (make-variable! 'm)
  (make-variable! 'p)
  (make-variable! 'q)
  (make-variable! 'r)
  (make-variable! 's)
  (process-form (datum->syntax #f code))
  (fetch-result))

(check-equal? (compile-code '(f)) '("  jsr f"))

(check-equal? (compile-code '(f 1)) '("  lda #$1"
                                      "  jsr f"))

(check-equal? (compile-code '(f 1 2 3)) '("  lda #$1"
                                          "  ldx #$2"
                                          "  ldy #$3"
                                          "  jsr f"))

(check-equal? (compile-code '(set! n (f))) '("  jsr f"
                                             "  sta n"))

(check-equal? (compile-code '(f 1 2 3 4 5)) '("  lda #$5"
                                              "  pha"
                                              "  lda #$4"
                                              "  pha"
                                              "  lda #$1"
                                              "  ldx #$2"
                                              "  ldy #$3"
                                              "  jsr f"
                                              "  sta _tmp"
                                              "  pla"
                                              "  pla"
                                              "  lda _tmp"))

(check-equal? (compile-code '(set! n (f 1 2 3 4 5))) '("  lda #$5"
                                                       "  pha"
                                                       "  lda #$4"
                                                       "  pha"
                                                       "  lda #$1"
                                                       "  ldx #$2"
                                                       "  ldy #$3"
                                                       "  jsr f"
                                                       "  sta _tmp"
                                                       "  pla"
                                                       "  pla"
                                                       "  lda _tmp"
                                                       "  sta n"))

(check-equal? (compile-code '(return)) '("  rts"))

(check-equal? (compile-code '(return 3)) '("  lda #$3"
                                           "  rts"))

(check-equal? (compile-code '(return n)) '("  lda n"
                                           "  rts"))

(check-equal? (compile-code '(return n m p)) '("  ldy p"
                                               "  ldx m"
                                               "  lda n"
                                               "  rts"))

(check-equal? (compile-code '(return (f n))) '("  lda n"
                                               "  jsr f"
                                               "  rts"))


(check-equal? (compile-code '(set-multiple! q (f))) '("  jsr f"
                                                      "  sta q"))

(check-equal? (compile-code '(set-multiple! q r (f))) '("  jsr f"
                                                        "  sta q"
                                                        "  stx r"))

(check-equal? (compile-code '(set-multiple! q r s (f))) '("  jsr f"
                                                          "  sta q"
                                                          "  stx r"
                                                          "  sty s"))
