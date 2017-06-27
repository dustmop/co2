#lang racket

(require rackunit "../compile.scm")

(define (compile-code code)
  (clear-result)
  (make-variable! 'm)
  (make-variable! 'n)
  (make-variable! 'array)
  (make-address! 'data 0)
  (process-form (datum->syntax #f code))
  (fetch-result))

(check-equal? (compile-code '(set! n 1)) '("  lda #$1"
                                           "  sta n"))

(check-equal? (compile-code '(set! n m)) '("  lda m"
                                           "  sta n"))

(check-equal? (compile-code '(set! m (set! n 1))) '("  lda #$1"
                                                    "  sta n"
                                                    "  sta m"))

(check-equal? (compile-code '(set-pointer! ptr addr))
              '("  lda #<addr"
                "  sta ptr"
                "  lda #>addr"
                "  sta ptr+1"))

(check-equal? (compile-code '(load-pointer ptr))
              '("  ldy #0"
                "  lda (ptr),y"))

(check-equal? (compile-code '(load-pointer ptr 2))
              '("  lda #$2"
                "  tay"
                "  lda (ptr),y"))

(check-equal? (compile-code '(memset sprites #xff))
              '("  lda #$ff"
                "  ldy #$00"
                "-  sta sprites,y"
                "  inx"
                "  bne -"))

(check-equal? (compile-code '(ppu-memcpy ppu-palette 0 0 #x20 data 0))
              '("  lda #$0"
                "  clc"
                "  adc #>ppu_palette"
                "  sta REG_PPU_ADDR"
                "  lda #$0"
                "  clc"
                "  adc #<ppu_palette"
                "  sta REG_PPU_ADDR"
                "  ldy #$0"
                "-  lda data,y"
                "  sta REG_PPU_DATA"
                "  iny"
                "  cpy #$20"
                "  bne -"
                "  lda #$00"
                "  sta REG_PPU_ADDR"
                "  lda #$00"
                "  sta REG_PPU_ADDR"))

(check-equal? (compile-code '(peek data #x10))
              '("  ldy #$10"
                "  lda data,y"))

(check-equal? (compile-code '(poke! array #x10 1))
              '("  ldy #$10"
                "  lda #$1"
                "  sta array,y"))

