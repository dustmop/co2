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
                "  iny"
                "  bne -"))

(check-equal? (compile-code '(memset render-buffer #x00 #x40))
              '("  lda #$0"
                "  ldy #$00"
                "-  sta render_buffer,y"
                "  iny"
                "  cpy #$40"
                "  bne -"))

(check-equal? (compile-code '(memcpy render-buffer data #x40))
              '("  ldy #<data"
                "  lda #>data"
                "  sty _pointer"
                "  sta _pointer+1"
                "  ldy #$00"
                "-  lda (_pointer),y"
                "  sta render_buffer,y"
                "  iny"
                "  cpy #$40"
                "  bne -"))

(check-equal? (compile-code '(memcpy render-buffer (scale16 data n #x20) #x40))
              '("  lda #0"
                "  sta _tmp"
                "  lda n"
                "  asl a"
                "  rol _tmp"
                "  asl a"
                "  rol _tmp"
                "  asl a"
                "  rol _tmp"
                "  asl a"
                "  rol _tmp"
                "  asl a"
                "  rol _tmp"
                "  clc"
                "  adc #<data"
                "  tay"
                "  lda _tmp"
                "  adc #>data"
                "  sty _pointer"
                "  sta _pointer+1"
                "  ldy #$00"
                "-  lda (_pointer),y"
                "  sta render_buffer,y"
                "  iny"
                "  cpy #$40"
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

(check-equal? (compile-code '(ppu-load #x2000 data #x800))
              '("  bit REG_PPU_STATUS"
                "  lda #>#$2000"
                "  sta REG_PPU_ADDR"
                "  lda #<#$2000"
                "  sta REG_PPU_ADDR"
                "  lda #<data"
                "  sta _pointer+0"
                "  lda #>data"
                "  sta _pointer+1"
                "  ldy #0"
                "  ldx #>#$800"
                "  jsr _ppu_load_by_pointer"))

(check-equal? (compile-code '(ppu-load #x3f00 data #x20))
              '("  bit REG_PPU_STATUS"
                "  lda #>#$3f00"
                "  sta REG_PPU_ADDR"
                "  lda #<#$3f00"
                "  sta REG_PPU_ADDR"
                "  lda #<(data+32)"
                "  sta _pointer+0"
                "  lda #>(data+32-$100)"
                "  sta _pointer+1"
                "  ldy #224"
                "  ldx #1"
                "  jsr _ppu_load_by_pointer"))

(check-equal? (compile-code '(ppu-load #x2000 0 #x800))
              '("  bit REG_PPU_STATUS"
                "  lda #>#$2000"
                "  sta REG_PPU_ADDR"
                "  lda #<#$2000"
                "  sta REG_PPU_ADDR"
                "  lda #$0"
                "  ldy #0"
                "  ldx #>#$800"
                "  jsr _ppu_load_by_val"))

(check-equal? (compile-code '(ppu-load #x3f00 #x0f #x20))
              '("  bit REG_PPU_STATUS"
                "  lda #>#$3f00"
                "  sta REG_PPU_ADDR"
                "  lda #<#$3f00"
                "  sta REG_PPU_ADDR"
                "  lda #$f"
                "  ldy #224"
                "  ldx #1"
                "  jsr _ppu_load_by_val"))

(check-equal? (compile-code '(peek data n))
              '("  ldy n"
                "  lda data,y"))

(check-equal? (compile-code '(peek data #x10))
              '("  lda data+16"))

(check-equal? (compile-code '(poke! array #x10 1))
              '("  ldy #$10"
                "  lda #$1"
                "  sta array,y"))

