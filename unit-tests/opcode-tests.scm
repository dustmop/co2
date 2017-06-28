#lang racket

(require rackunit "../compile.scm")

(define (compile-code code)
  (clear-result)
  (process-form (datum->syntax #f code))
  (fetch-result))

(check-equal? (compile-code '(asl a)) '("  asl a"))

(check-equal? (compile-code '(and 1)) '("  and #$1"))

(check-equal? (compile-code '(clc)) '("  clc"))

(check-equal? (compile-code '(cld)) '("  cld"))

(check-equal? (compile-code '(cli)) '("  cli"))

(check-equal? (compile-code '(clv)) '("  clv"))

(check-equal? (compile-code '(cmp 1)) '("  cmp #$1"))

(check-equal? (compile-code '(cpx 1)) '("  cpx #$1"))

(check-equal? (compile-code '(cpy 1)) '("  cpy #$1"))

(check-equal? (compile-code '(dex)) '("  dex"))

(check-equal? (compile-code '(dey)) '("  dey"))

(check-equal? (compile-code '(eor 1)) '("  eor #$1"))

(check-equal? (compile-code '(inx)) '("  inx"))

(check-equal? (compile-code '(iny)) '("  iny"))

(check-equal? (compile-code '(lda 1)) '("  lda #$1"))

(check-equal? (compile-code '(ldx 1)) '("  ldx #$1"))

(check-equal? (compile-code '(ldy 1)) '("  ldy #$1"))

(check-equal? (compile-code '(nop)) '("  nop"))

(check-equal? (compile-code '(ora 1)) '("  ora #$1"))

(check-equal? (compile-code '(sta "n")) '("  sta n"))

(check-equal? (compile-code '(stx "n")) '("  stx n"))

(check-equal? (compile-code '(sty "n")) '("  sty n"))

(check-equal? (compile-code '(tax)) '("  tax"))

(check-equal? (compile-code '(tay)) '("  tay"))

(check-equal? (compile-code '(txa)) '("  txa"))

(check-equal? (compile-code '(tya)) '("  tya"))



