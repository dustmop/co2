#lang racket

(require rackunit "../compile.scm")

(define (compile-code code)
  (clear-result)
  (clear-var-allocation)
  (make-variable! 'm)
  (make-variable! 'n)
  (make-variable! 'array)
  (make-pointer! 'ptr)
  (make-address! 'data 0)
  (make-const! 'some-field 4)
  (process-form (datum->syntax #f code))
  (fetch-result))

(define (compile-code-no-defs code)
  (clear-result)
  (clear-var-allocation)
  (analyze-form (datum->syntax #f code))
  (process-form (datum->syntax #f code))
  (fetch-result))

(check-equal? (compile-code '(set! n 1)) '("  lda #$1"
                                           "  sta n"))

(check-equal? (compile-code '(set! n m)) '("  lda m"
                                           "  sta n"))

(check-equal? (compile-code '(set! m (set! n 1))) '("  lda #$1"
                                                    "  sta n"
                                                    "  sta m"))

(check-equal? (compile-code-no-defs '(do (defpointer ptr) (defpointer qtr)))
              '(""
                "ptr = $10"
                ""
                "qtr = $12"))

(check-equal? (compile-code '(set-pointer! ptr addr))
              '("  lda #<addr"
                "  sta ptr"
                "  lda #>addr"
                "  sta ptr+1"))

(check-equal? (compile-code '(peek ptr))
              '("  ldy #0"
                "  lda (ptr),y"))

(check-equal? (compile-code '(peek ptr 2))
              '("  ldy #$2"
                "  lda (ptr),y"))

(check-equal? (compile-code '(load-pointer ptr))
              '("  ldy #0"
                "  lda (ptr),y"))

(check-equal? (compile-code '(load-pointer ptr 2))
              '("  ldy #$2"
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

(check-equal? (compile-code '(peek data x))
              '("  lda data,x"))

(check-equal? (compile-code '(peek data y))
              '("  lda data,y"))

(check-equal? (compile-code '(peek data some-field))
              '("  lda data+some_field"))

(check-equal? (compile-code '(repeat (i 2) (peek data i)))
              '("  lda data+0"
                "  lda data+1"))

(check-equal? (compile-code '(poke! array #x10 4))
              '("  lda #$4"
                "  sta array+16"))

(check-equal? (compile-code '(poke! array x 5))
              '("  lda #$5"
                "  sta array,x"))

(check-equal? (compile-code '(poke! array y 6))
              '("  lda #$6"
                "  sta array,y"))

(check-equal? (compile-code '(poke! array n 7))
              '("  lda #$7"
                "  ldy n"
                "  sta array,y"))

(check-equal? (compile-code '(poke! array (+ n 1) 8))
              '("  lda #$8"
                "  sta __global___gen_0001"
                "  lda n"
                "  clc"
                "  adc #$1"
                "  tay"
                "  lda __global___gen_0001"
                "  sta array,y"))

(check-equal? (compile-code '(poke! ptr 9))
              '("  lda #$9"
                "  ldy #0"
                "  sta (ptr),y"))

(check-equal? (compile-code '(poke! ptr #x10 10))
              '("  lda #$a"
                "  ldy #$10"
                "  sta (ptr),y"))

(check-equal? (compile-code '(poke! ptr y 11))
              '("  lda #$b"
                "  sta (ptr),y"))

(check-equal? (compile-code '(poke! ptr n 12))
              '("  lda #$c"
                "  ldy n"
                "  sta (ptr),y"))

(check-equal? (compile-code '(poke! ptr (+ n 1) 13))
              '("  lda #$d"
                "  sta __global___gen_0002"
                "  lda n"
                "  clc"
                "  adc #$1"
                "  tay"
                "  lda __global___gen_0002"
                "  sta (ptr),y"))
