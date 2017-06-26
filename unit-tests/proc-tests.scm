#lang racket

(require rackunit "../compile.scm")

(define (compile-code code)
  (clear-result)
  (make-function! 'f)
  (make-variable! 'n)
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
