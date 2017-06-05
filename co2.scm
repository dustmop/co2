#lang racket
;; co2 Copyright (C) 2016 Dave Griffiths
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require racket/cmdline)
(require data/gvector)

;; internal compiler register on zero page
(define working-reg "$ff")
(define stack-frame-h "$fe")
(define stack-frame-l "$fd")
(define rnd-reg "$fc")

(define reg-table
  ;; ppu registers
  '((REG-PPU-CTRL             #x2000)
    (REG-PPU-MASK             #x2001)
    (REG-PPU-STATUS           #x2002)
    (REG-OAM-ADDR             #x2003)
    (REG-OAM-DATA             #x2004)
    (REG-PPU-SCROLL           #x2005)
    (REG-PPU-ADDR             #x2006)
    (REG-PPU-DATA             #x2007)
    ;;  apu registers
    (REG-APU-PULSE1-CONTROL   #x4000)
    (REG-APU-PULSE1-RAMP      #x4001)
    (REG-APU-PULSE1-FT        #x4002)
    (REG-APU-PULSE1-CT        #x4003)
    (REG-APU-PULSE2-CONTROL   #x4004)
    (REG-APU-PULSE2-RAMP      #x4005)
    (REG-APU-PULSE2-FT        #x4006)
    (REG-APU-PULSE2-CT        #x4007)
    (REG-APU-TRI-CONTROL      #x4008)
    (REG-APU-TRI-FT           #x400a)
    (REG-APU-TRI-CT           #x400b)
    (REG-APU-NOISE-ENV        #x400c)
    (REG-APU-NOISE-FT         #x400e)
    (REG-APU-NOISE-CT         #x400f)
    (REG-APU-DMC-CONTROL      #x4010)
    (REG-APU-DMC-DAC          #x4011)
    (REG-APU-DMC-ADDR         #x4012)
    (REG-APU-DMC-SIZE         #x4013)
    (REG-OAM-DMA              #x4014)
    (REG-APU-CHANNEL          #x4015)
    ;; input
    (REG-JOYPAD-0             #x4016)
    (REG-JOYPAD-1             #x4017)))

(define ppu-flags
  ;; TODO: More
  '((PPU-CTRL-NMI                #x80)
    (PPU-MASK-SHOW-SPR           #x10)
    (PPU-MASK-SHOW-BG            #x08)))

(define reserved-zero-page
  '((ppu-ctrl                    #x00)
    (ppu-mask                    #x01)
    (frame-num                   #x02)
    (-count                      #x03)))

(define (reg-table-lookup x)
  (let ((lu (assoc x reg-table)))
    (if lu (cadr lu) #f)))

;;-------------------------------------------------------------
;; nes header

(define (emit-nes-header x)
  (let* ((num-prg-banks (cadr (assoc 'num-prg-banks x)))
         (num-chr-banks (cadr (assoc 'num-chr-banks x)))
         (mapper-num (cadr (assoc 'mapper-num x)))
         (mirroring (cadr (assoc 'mirroring x)))
         (third-byte (+ (* mapper-num #x10)
                        (if (eq? mirroring 'vertical) 1 0))))
    (emit-raw-lines
     ".byte \"NES\",$1a"
     (string-append ".byte $" (number->string num-prg-banks 16))
     (string-append ".byte $" (number->string num-chr-banks 16))
     (string-append ".byte $" (number->string third-byte 16))
     (string-append ".byte "
                    (string-join (build-list 9 (lambda (x) "$0")) ",")))))

(define (emit-init-system)
  (emit-raw-lines
   ;; disable interrupts while we set stuff up
   "sei"
   ;; make sure we're not using decimal mode
   "cld"
   ;; wait for 2 vblanks
   "- lda $2002"
   "bpl -"
   "- lda $2002"
   "bpl -"
   ;; clear out all ram
   "lda #$00"
   "ldx #$00"
   "- sta $000,x"
   "sta $100,x"
   "sta $200,x"
   "sta $300,x"
   "sta $400,x"
   "sta $500,x"
   "sta $600,x"
   "sta $700,x"
   "inx"
   "bne -"
   ;; reset the stack pointer.
   "ldx #$ff"
   "txs"
   ;; setup stack frame address high byte
   "lda #1"
   (string-append "sta " stack-frame-h)
   "lda #123"
   (string-append "sta " rnd-reg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Label generation

(define label-id 0)

(define (generate-label name)
  (set! label-id (+ label-id 1))
  (string-append "_" name "_" (left-pad (number->string label-id 16) #\0 4)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Symbol / label definitions

(define var-allocation #x10)

(define sym-label-defs (list (make-hash)))

(define data-segment '())

(struct sym-label (sym name address kind))

(define (make-variable! sym #:label [label #f])
  (let* ((name (normalize-name sym))
         (n var-allocation))
    (when label
          (set! name label))
    (when (not (hash-has-key? (car sym-label-defs) sym))
          (hash-set! (car sym-label-defs) sym (sym-label sym name n 'var))
          (set! var-allocation (+ 1 var-allocation)))
    (hash-ref (car sym-label-defs) sym)))

(define (make-address! sym addr)
  (let ((name (normalize-name sym)))
    (hash-set! (car sym-label-defs) sym (sym-label sym name addr 'addr))
    (hash-ref (car sym-label-defs) sym)))

(define (make-const! sym value)
  (let ((name (normalize-name sym)))
    (hash-set! (car sym-label-defs) sym (sym-label sym name value 'const))
    (hash-ref (car sym-label-defs) sym)))

(define (make-data! sym value)
  (let ((name (normalize-name sym)))
    (set! data-segment (cons (list name value) data-segment))
    (hash-set! (car sym-label-defs) sym (sym-label sym name 0 'data))
    (hash-ref (car sym-label-defs) sym)))

(define (variable? sym)
  (let ((lookup (sym-label-lookup)))
    (and lookup (eq? (sym-label-kind lookup) 'var))))

(define (address? sym)
  (let ((lookup (sym-label-lookup)))
    (and lookup (eq? (sym-label-kind lookup) 'addr))))

(define (const? sym)
  (let ((lookup (sym-label-lookup)))
    (and lookup (eq? (sym-label-kind lookup) 'const))))

(define (sym-label-lookup sym)
  (define (_ table parents)
    (if (hash-has-key? table sym)
        (hash-ref table sym)
        (if (null? parents)
            #f
            (_ (car parents) (cdr parents)))))
  (_ (car sym-label-defs) (cdr sym-label-defs)))

(define (sym-label-push-scope)
  (set! sym-label-defs (cons (make-hash) sym-label-defs)))

(define (sym-label-pop-scope)
  (set! sym-label-defs (cdr sym-label-defs)))

(define (get-data-segment)
  data-segment)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function definition

(define function-defs (make-hash))

(define (make-function! sym)
  (let* ((name (normalize-name sym))
         (n (hash-count function-defs)))
    (when (not (hash-has-key? function-defs name))
          (hash-set! function-defs name n))
    (hash-ref function-defs name)))

(define (function? name)
  (hash-has-key? function-defs (normalize-name name)))

;;------------------------------------------------------------------
;; store function args here - this is not a proper call stack
;; (considered far too bloaty!), so these get clobbered with function
;; calls within function calls - beware...

(define fnargs-start #x1)

(define (fnarg index)
  (string-append (number->string (+ fnargs-start index))))

(define fnarg-mapping '())

(define (set-fnarg-mapping! args)
  (set! fnarg-mapping
        (foldl
         (lambda (arg r)
           ;; map the argument name to the address
           (cons (list arg (fnarg (length r))) r))
         '()
         args)))

(define (clear-fnarg-mapping!)
  (set! fnarg-mapping '()))

(define (fnarg-lookup name)
  (let ((t (assoc name fnarg-mapping)))
    (if t (cadr t) #f)))

;; -----------------------

(define (is-fnarg? name)
  (let ((t (assoc name fnarg-mapping)))
    (if t #t #f)))

(define (emit-load-fnarg name)
  (append
   (emit "ldy" (string-append "#" (fnarg-lookup name)))
   (emit "lda" (string-append "(" stack-frame-l "),y"))))

;;----------------------------------------------------------------

(define (dash->underscore s)
  (foldl
   (lambda (c r)
     (if (eq? c #\-)
         (string-append r "_")
         (string-append r (string c))))
   ""
   (string->list s)))

;;---------------------------------------------------------------

(define (lookup x)
  #f)

(define (immediate-value x)
  (if (number? x)
      (string-append "#" (number->string x))
      (let ((lu (lookup x)))
        (if lu lu (symbol->string x)))))

;; is this an immediate value
(define (immediate? x)
  (or (number? x) (symbol? x)))

;; is this a primitive call?
(define (primcall? x)
  (and (list? x) (not (null? x)) (symbol? (car x))))

;--------------------------------------------------------------
;; code generation
;;
;; general rules:
;; * don't use registers across emit-expr as they can be clobbered
;; * use the stack (pha/pla) to store data in this case
;; * currently using internal "working-reg" as a 4th register on zero page
;;   for arithmetic stuff
;; * don't use shorthand branch labels ("-") across emit-expr either
;; * use (generate-label) in this case to use a unique one
;; * working register and returns all stored in a
;; * x/y are used for local optimisation
;; * results of the relevant subexpression should be left in a register as
;;   implicit return (e.g. loops, if, when etc)

(define (emit . args)
  (list
   (foldl
    (lambda (arg r)
      (if (equal? r "")
          (string-append r arg)
          (string-append r " " arg)))
    ""
    args)))

(define (emit-raw-lines . args)
  (emit-asm (cons "" args)))

;; append a bunch of expressions
(define (emit-expr-list l)
  (cond
    ((null? l) '())
    (else
     (append
      (emit-expr (car l))
      (emit-expr-list (cdr l))))))

(define (emit-asm x)
  (let ((r
         (foldl
          (lambda (str r)
            (if (equal? r "")
                str
                (string-append r "\n" str)))
          ""
          (cdr x))))
    (list r)))

(define (emit-label label)
  (emit (string-append label ":")))

(define (emit-load-variable x)
  (if (is-fnarg? x)
      (emit-load-fnarg x)
      (emit "lda" (immediate-value x))))

(define (emit-load-immediate x)
  (cond
    ((number? x) (emit "lda" (string-append "#" (number->string x))))
    ((symbol? x) (emit-load-variable x))))

(define (emit-defvar x)
  (make-variable! (cadr x))
  (append
   (emit-expr (caddr x))
   (emit "sta" (immediate-value (cadr x)))))

;; arguments are mapped to arg-n...
(define (emit-defun x)
  (set-fnarg-mapping! (cdr (cadr x)))
  (let ((r (append
            (emit (string-append
                   (dash->underscore
                    (symbol->string (car (cadr x)))) ":"))
            (emit-expr-list (cddr x))
            (emit "rts"))))
    (clear-fnarg-mapping!)
    r))

(define (emit-defint x)
  (append
   (emit (string-append
          (dash->underscore
           (symbol->string (car (cadr x)))) ":"))
   (emit-expr-list (cddr x))
   (emit "rti")))

(define (emit-fncall x)
  (append
   (emit "lda" stack-frame-l) ;; store previous stack location
   (emit "pha")
   ;; push the arguments on the stack
   (foldl
    (lambda (arg r)
      (append
       r
       (emit-expr arg)
       (emit "pha")))
    '()
    (reverse (cdr x))) ;; in reverse order
   (emit "tsx") ;; sture current top as stack frame
   (emit "stx" stack-frame-l) ;; to find arguments later
   ;; call the function
   (emit "jsr" (dash->underscore (symbol->string (car x))))
   (emit "sta" working-reg) ;; temp store return value
   ;; remove arguments from the stack
   (foldl
    (lambda (arg r)
      (append r (emit "pla")))
    '()
    (cdr x))
   (emit "pla")
   (emit "sta" stack-frame-l) ;; reinstate previous stack frame
   (emit "lda" working-reg) ;; load return value
   ))

(define (emit-set! x)
  (if (is-fnarg? (cadr x))
      (append
       (emit-expr (caddr x))
       (emit "ldy" (string-append "#" (fnarg-lookup (cadr x))))
       (emit "sta" (string-append "(" stack-frame-l "),y")))
      (append
       (emit-expr (caddr x))
       (emit "sta" (immediate-value (cadr x))))))

;; takes an address literal
(define (emit-set16! x)
  (if (is-fnarg? (cadr x))
      (begin
        (display "ERROR: trying to set fn arg to 16bit value...")
        (newline))
      (append
       (emit "lda" (string-append "#<" (symbol->string (caddr x))))
       (emit "sta" (immediate-value (cadr x)))
       (emit "ldx" "#1")
       (emit "lda" (string-append "#>" (symbol->string (caddr x))))
       (emit "sta" (immediate-value (cadr x)) ",x"))))

(define (emit-write! x)
  (append
   (emit-expr (list-ref x 3))
   (emit "ldy" (immediate-value (list-ref x 2)))
   (emit "sta" (immediate-value (list-ref x 1)) ",y")))

(define (emit-poke! x)
  ;; address offset is optional
  (if (eq? (length x) 4)
      (append
       (emit-expr (list-ref x 3)) ;; value
       (emit "pha")
       (emit-expr (list-ref x 2)) ;; address offset
       (emit "tay")
       (emit "pla")
       (emit "sta" (immediate-value (list-ref x 1)) ",y"))
      (append
       (emit-expr (list-ref x 2)) ;; value
       (emit "sta" (immediate-value (list-ref x 1))))))

(define (emit-peek x)
  ;; address offset is optional
  (if (eq? (length x) 3)
      (append
       (emit-expr (list-ref x 2)) ;; address offset
       (emit "tay")
       (emit "lda" (immediate-value (list-ref x 1)) ",y"))
      (append
       (emit "lda" (immediate-value (list-ref x 1))))))

(define (emit-peek16 x)
  ;; address offset is optional
  (if (eq? (length x) 3)
      (append
       (emit-expr (list-ref x 2)) ;; address offset
       (emit "tay")
       (emit "lda" (string-append "(" (immediate-value (list-ref x 1)) ")")
             ",y"))
      (append
       (emit "ldy" "#0")
       (emit "lda" (string-append "(" (immediate-value (list-ref x 1)) ")")
             ",y"))))


;; sets blocks of 256 bytes
;; (set-page variable/value expr)
(define (emit-memset x)
  (append
   (emit-expr (caddr x))
   (emit "ldx" "#$00")
   (emit "- sta" (immediate-value (cadr x)) ",x")
   (emit "inx")
   (emit "bne -")))

; (memset base-symbol high-offset low-offset length value)
(define (emit-ppu-memset x)
  (append
   (emit-expr (list-ref x 2)) ;; dest offset high
   (emit "clc")
   (emit "adc" (car (immediate-value (list-ref x 1))))
   (emit "sta" (reg-table-lookup 'reg-ppu-addr))
   (emit-expr (list-ref x 3)) ;; dest offset low
   (emit "clc")
   (emit "adc" (cadr (immediate-value (list-ref x 1))))
   (emit "sta" (reg-table-lookup 'reg-ppu-addr))
   (emit-expr (list-ref x 5)) ;; value
   (emit "pha")
   (emit-expr (list-ref x 4)) ;; length
   (emit "tax")
   (emit "pla")
   (emit "- sta" (reg-table-lookup 'reg-ppu-data))
   (emit "dex")
   (emit "bne -")
   ;; reset ppu addr
   (emit "lda" "#$00")
   (emit "sta" (reg-table-lookup 'reg-ppu-addr))
   (emit "lda" "#$00")
   (emit "sta" (reg-table-lookup 'reg-ppu-addr))))


; (memcpy base-symbol high-offset low-offset prg-end prg-base prg-start)
(define (emit-ppu-memcpy x)
  (append
   (emit-expr (list-ref x 2)) ;; dest offset high
   (emit "clc")
   (emit "adc" (car (immediate-value (list-ref x 1))))
   (emit "sta" (reg-table-lookup 'reg-ppu-addr))
   (emit-expr (list-ref x 3)) ;; dest offset low
   (emit "clc")
   (emit "adc" (cadr (immediate-value (list-ref x 1))))
   (emit "sta" (reg-table-lookup 'reg-ppu-addr))
   (emit "ldx" (immediate-value (list-ref x 6))) ;; start addr
   (emit "- lda" (immediate-value (list-ref x 5)) ",x") ;; base addr
   (emit "sta" (reg-table-lookup 'reg-ppu-data))
   (emit "inx")
   (emit "cpx" (immediate-value (list-ref x 4))) ;; end addr
   (emit "bne -")
   ;; reset ppu addr
   (emit "lda" "#$00")
   (emit "sta" (reg-table-lookup 'reg-ppu-addr))
   (emit "lda" "#$00")
   (emit "sta" (reg-table-lookup 'reg-ppu-addr))))

; (memcpy base-symbol high-offset low-offset prg-end prg-base-h prg-base-l)
(define (emit-ppu-memcpy16 x)
  (display (immediate-value (list-ref x 5)))(newline)
  (append
   (emit-expr (list-ref x 2)) ;; dest offset high
   (emit "clc")
   (emit "adc" (car (immediate-value (list-ref x 1))))
   (emit "sta" (reg-table-lookup 'reg-ppu-addr))
   (emit-expr (list-ref x 3)) ;; dest offset low
   (emit "clc")
   (emit "adc" (cadr (immediate-value (list-ref x 1))))
   (emit "sta" (reg-table-lookup 'reg-ppu-addr))
   (emit "ldy #0")
   (emit "- lda" (string-append "(" (immediate-value (list-ref x 5)) ")")
         ",y") ;; base addr
   (emit "sta" (reg-table-lookup 'reg-ppu-data))
   (emit "iny")
   (emit "cpy" (immediate-value (list-ref x 4))) ;; end addr
   (emit "bne -")
   ;; reset ppu addr
   (emit "lda" "#$00")
   (emit "sta" (reg-table-lookup 'reg-ppu-addr))
   (emit "lda" "#$00")
   (emit "sta" (reg-table-lookup 'reg-ppu-addr))))


;; optimised version of poke for sprites
(define (emit-set-sprite! n x)
  ;; address offset is optional
  (append
   (emit-expr (list-ref x 2)) ;; value
   (emit "pha")
   (emit-expr (list-ref x 1)) ;; sprite num offset
   (emit "asl") ;; *2
   (emit "asl") ;; *4
   (emit "clc")
   (emit "adc" (immediate-value n)) ;; byte offset
   (emit "tay")
   (emit "pla")
   (emit "sta" "$200,y")))

;; optimised version of peek for sprites
(define (emit-get-sprite n x)
  ;; address offset is optional
  (append
   (emit-expr (list-ref x 1)) ;; sprite num offset
   (emit "asl") ;; *2
   (emit "asl") ;; *4
   (emit "clc")
   (emit "adc" (immediate-value n)) ;; byte offset
   (emit "tay")
   (emit "lda" "$200,y")))

;; optimised version of poke for sprites
(define (emit-zzz-sprites! n zzz x)
  (let ((label (generate-label "sprite_range")))
    (append
     (emit-expr (list-ref x 3)) ;; value
     (emit "pha")
     (emit-expr (list-ref x 2)) ;; sprite count
     (emit "pha")
     (emit-expr (list-ref x 1)) ;; sprite num offset
     (emit "asl") ;; *2
     (emit "asl") ;; *4
     (emit "clc")
     (emit "adc" (immediate-value n)) ;; byte offset
     (emit "tay") ;; put offset in y
     (emit "pla") ;; pull count out
     (emit "tax") ;; put sprite count in x
     (emit "pla") ;; value
     (emit "sta" working-reg)
     (emit-label label)
     (emit "lda" "$200,y") ;; load previous
     (cond
      ((equal? zzz "adc") (emit "clc")) ;; clear carry
      ((equal? zzz "sbc") (emit "sec")) ;; set carry
      (else '()))
     (emit zzz working-reg)
     (emit "sta" "$200,y")
     (emit "iny") ;; skip
     (emit "iny") ;; to
     (emit "iny") ;; the next
     (emit "iny") ;; sprite
     (emit "dex")
     (emit "bne" label))))

;; optimised version of poke for sprites
(define (emit-set-sprites-x-2x2! x)
  (append
   (emit-expr (list-ref x 2)) ;; value
   (emit "pha")
   (emit-expr (list-ref x 1)) ;; sprite addr
   (emit "tay") ;; put offset in y
   (emit "pla") ;; value
   (emit "sta" "$203,y")
   (emit "sta" "$20b,y")
   (emit "clc")
   (emit "adc" "#8")
   (emit "sta" "$207,y")
   (emit "sta" "$20f,y")))

(define (emit-set-sprites-y-2x2! x)
  (append
   (emit-expr (list-ref x 2)) ;; value
   (emit "pha")
   (emit-expr (list-ref x 1)) ;; sprite addr
   (emit "tay") ;; put offset in y
   (emit "pla") ;; value
   (emit "sta" "$200,y")
   (emit "sta" "$204,y")
   (emit "clc")
   (emit "adc" "#8")
   (emit "sta" "$208,y")
   (emit "sta" "$20c,y")))

;; optimised version of poke for sprites
(define (emit-animate-sprites-2x2! x)
  (append
   (emit-expr (list-ref x 2)) ;; value
   (emit "pha")
   (emit-expr (list-ref x 1)) ;; sprite num offset
   (emit "asl") ;; *2
   (emit "asl") ;; *4
   (emit "tay") ;; put offset in y
   (emit "iny") ;; id byte offset
   (emit "pla") ;; value
   (emit "sta" "$200,y") ;; sprite 1
   ;;(emit "clc")
   (emit "adc" "#$01")
   (emit "sta" "$204,y") ;; sprite 2
   (emit "adc" "#$0f")
   (emit "sta" "$208,y") ;; sprite 3
   (emit "adc" "#$01")
   (emit "sta" "$20c,y"))) ;; sprite 4

;; ;; (sprite-check-collide-1x1v1x1 a b)
;; (define (emit-sprite-check-collide x)
;;   (append
;;    (emit-expr (list-ref x 2)) ;; sprite b
;;    (emit "pha")
;;    (emit-expr (list-ref x 1)) ;; sprite a num offset
;;    (emit "asl") ;; *2
;;    (emit "asl") ;; *4
;;    (emit "tax") ;; put offset in x
;;    (emit "pla") ;; load sprite b offset
;;    (emit "asl") ;; *2
;;    (emit "asl") ;; *4
;;    (emit "tay") ;; put offset in y
;;    ;; check x
;;    (emit "lda" "$203,x") ;; sprite 1 x coord
;;    (emit "sta" working-reg)
;;    (emit "lda" "$203,y") ;; sprite 2 x coord


;; (loop var from to expr)
;; todo: fix branch limit
(define (emit-loop x)
  (let ((label-start (generate-label "loop_start"))
        (label-end (generate-label "loop_end")))
    (append
     (emit-expr (list-ref x 2))
     (emit "sta" (immediate-value (list-ref x 1)))
     (emit-label label-start)
     (emit-expr-list (cddddr x))
     (emit "sta" working-reg) ;; store return in case we need it
     (emit "inc" (immediate-value (list-ref x 1)))
     (emit-expr (list-ref x 3))
     (emit "cmp" (immediate-value (list-ref x 1)))
     (emit "bcs" label-start)
     (emit "lda" working-reg)))) ;; retrieve return

;; (if pred then else)
(define (emit-if x)
  (let ((false-label (generate-label "if_false"))
        (true-label (generate-label "if_true"))
        (end-label (generate-label "if_end")))
    (append
     (emit-expr (list-ref x 1))
     (emit "cmp" "#0")
     (emit "bne" true-label)
     (emit "jmp" false-label)
     (emit-label true-label)
     (emit-expr (list-ref x 2)) ;; true block
     (emit "jmp" end-label)
     (emit-label false-label)
     (emit-expr (list-ref x 3)) ;; false block
     (emit-label end-label))))

;; (when pred then)
(define (emit-when x)
  (let ((end-label (generate-label "when_end"))
        (do-label (generate-label "when_do")))
    (append
     (emit-expr (list-ref x 1))
     (emit "cmp" "#0")
     (emit "bne" do-label)
     (emit "jmp" end-label)
     (emit-label do-label)
     (emit-expr-list (cddr x)) ;; true block
     (emit-label end-label))))

;; (while pred then)
(define (emit-while x)
  (let ((loop-label (generate-label "while_loop"))
        (end-label (generate-label "while_loop_end"))
        (next-label (generate-label "while_loop_next"))
        (pred-label (generate-label "while_loop_pred")))
    (append
     (emit "jmp" pred-label) ;; check first
     (emit-label loop-label)
     (emit-expr-list (cddr x)) ;; loop block
     (emit-label pred-label)
     (emit-expr (list-ref x 1)) ;; predicate
     (emit "bne" next-label)
     (emit "jmp" end-label)
     (emit-label next-label)
     (emit "jmp" loop-label)
     (emit-label end-label))))

;; predicate stuff, I think these are stupidly long

(define (emit-eq? x)
  (let ((true-label (generate-label "eq_true"))
        (end-label (generate-label "eq_end")))
    (append
     (emit-expr (list-ref x 1))
     (emit "pha")
     (emit-expr (list-ref x 2))
     (emit "sta" working-reg)
     (emit "pla")
     (emit "cmp" working-reg)
     (emit "beq" true-label)
     (emit "lda" "#0")
     (emit "jmp" end-label)
     (emit-label true-label)
     (emit "lda" "#1")
     (emit-label end-label))))

;; correct, but hella slow
;; instr depends on signed or unsigned version
(define (emit-< x instr)
  (let ((true-label (generate-label "gt_true"))
        (end-label (generate-label "gt_end")))
    (append
     (emit-expr (list-ref x 1))
     (emit "pha")
     (emit-expr (list-ref x 2))
     (emit "sta" working-reg)
     (emit "pla")
     (emit "cmp" working-reg)
     (emit instr true-label)
     (emit "lda" "#0")
     (emit "jmp" end-label)
     (emit-label true-label)
     (emit "lda" "#1")
     (emit-label end-label))))

;; correct, but hella slow
(define (emit-<= x instr)
  (let ((true-label (generate-label "gt_true"))
        (end-label (generate-label "gt_end")))
    (append
     (emit-expr (list-ref x 1))
     (emit "pha")
     (emit-expr (list-ref x 2))
     (emit "sta" working-reg)
     (emit "pla")
     (emit "sbc" working-reg)
     (emit instr true-label) ;; branch on minus
     (emit "lda" "#0")
     (emit "jmp" end-label)
     (emit-label true-label)
     (emit "lda" "#1")
     (emit-label end-label))))

(define (emit-> x instr)
  (let ((true-label (generate-label "lt_true"))
        (end-label (generate-label "lt_end")))
    (append
     (emit-expr (list-ref x 1))
     (emit "pha")
     (emit-expr (list-ref x 2))
     (emit "sta" working-reg)
     (emit "pla")
     (emit "sbc" working-reg)
     (emit instr true-label) ;; branch on plus
     (emit "lda" "#0")
     (emit "jmp" end-label)
     (emit-label true-label)
     (emit "lda" "#1")
     (emit-label end-label))))

(define (emit-not x)
  (let ((one-label (generate-label "not_one"))
        (end-label (generate-label "not_end")))
    (append
     (emit-expr (list-ref x 1))
     (emit "beq" one-label) ;; branch on plus
     (emit "lda" "#0")
     (emit "jmp" end-label)
     (emit-label one-label)
     (emit "lda" "#1")
     (emit-label end-label))))

(define (emit-left-shift x)
  (append
   (emit-expr (list-ref x 1))
   (map append
        (build-list
         (list-ref x 2)
         (lambda (i) "asl")))))

(define (emit-right-shift x)
  (append
   (emit-expr (list-ref x 1))
   (map append
        (build-list
         (list-ref x 2)
         (lambda (i) "lsr")))))

(define (emit-sub x)
  (append
   (emit-expr (cadr x))
   (emit "pha")
   (emit-expr (caddr x))
   (emit "sta" working-reg)
   (emit "pla")
   (emit "sec")
   (emit "sbc" working-reg)))

(define (emit-add x)
  (append
   (emit-expr (cadr x))
   (emit "pha")
   (emit-expr (caddr x))
   (emit "sta" working-reg)
   (emit "pla")
   (emit "clc")
   (emit "adc" working-reg)))

(define (emit-mul x)
  (let ((label (generate-label "mul")))
    (append
     (emit-expr (cadr x))
     (emit "pha")
     (emit-expr (caddr x))
     (emit "sta" working-reg)
     (emit "dec" working-reg)
     (emit "pla")
     (emit "tax")
     (emit-label label)
     (emit "clc")
     (emit "adc" working-reg)
     (emit "dex")
     (emit "bne" label))))

;; add two 8 bit numbers to a 16 bit one
(define (emit-add16 x)
  (append
   (emit-expr (list-ref x 2)) ; high 8 bit num
   (emit "pha")
   (emit-expr (list-ref x 3)) ; low 8 bit num
   (emit "sta" working-reg)
   (emit "clc")
   (emit "lda" (string-append "(" (immediate-value (list-ref x 1)) ")") )
   (emit "adc" working-reg)
   (emit "sta" (string-append "(" (immediate-value (list-ref x 1)) ")"))
   (emit "pla")
   (emit "sta" working-reg)
   (emit "lda" (string-append "(" (immediate-value (list-ref x 1)) "+1)"))
   (emit "adc" working-reg)
   (emit "sta" (string-append "(" (immediate-value (list-ref x 1)) "+1)"))))

;; subtract two 8 bit numbers to a 16 bit one
(define (emit-sub16 x)
  (append
   (emit-expr (list-ref x 2)) ; high 8 bit num
   (emit "pha")
   (emit-expr (list-ref x 3)) ; low 8 bit num
   (emit "sta" working-reg)
   (emit "sec")
   (emit "lda" (string-append "(" (immediate-value (list-ref x 1)) ")") )
   (emit "sbc" working-reg)
   (emit "sta" (string-append "(" (immediate-value (list-ref x 1)) ")"))
   (emit "pla")
   (emit "sta" working-reg)
   (emit "lda" (string-append "(" (immediate-value (list-ref x 1)) "+1)"))
   (emit "sbc" working-reg)
   (emit "sta" (string-append "(" (immediate-value (list-ref x 1)) "+1)"))))


;; (define (emit-mod x)
;; Mod:
;; 		LDA $00  ; memory addr A
;; 		SEC
;; Modulus:	SBC $01  ; memory addr B
;; 		BCS Modulus
;; 		ADC $01

;; 		;division, rounds up, returns in reg A
;; Division:
;; 		LDA $00
;; 		LDX #0
;; 		SEC
;; Divide:		INX
;; 		SBC $01
;; 		BCS Divide
;; 		TXA      ;get result into accumulator

(define (emit-rnd x)
  (let ((label (generate-label "rnd")))
    (append
     (emit "lda" rnd-reg)
     (emit "asl")
     (emit "bcc" label)
     (emit "eor" "#$1d")
     (emit-label label)
     (emit "sta" rnd-reg))))

(define (unary-procedure proc x)
  (append
   (emit-expr (cadr x))
   (emit proc)))

(define (binary-procedure proc x)
  (append
   (emit-expr (cadr x))
   (emit "pha")
   (emit-expr (caddr x))
   (emit "sta" working-reg)
   (emit "pla")
   (emit proc working-reg)))

(define (emit-procedure x)
  (cond
   ((eq? (car x) 'asm) (emit-asm x))
   ((eq? (car x) 'byte) (list (string-append ".byte " (cadr x) "\n")))
   ((eq? (car x) 'text) (list (string-append ".byte \"" (cadr x) "\"\n")))
   ((eq? (car x) 'nes-header) (emit-nes-header (cdr x)))
   ((eq? (car x) 'defvar) (emit-defvar x))
   ((eq? (car x) 'defun) (emit-defun x))
   ((eq? (car x) 'defint) (emit-defint x))
   ((eq? (car x) 'defconst) #f)
   ((eq? (car x) 'defaddr) #f)
   ((eq? (car x) 'defimmed) #f)
   ((eq? (car x) 'set!) (emit-set! x))
   ((eq? (car x) 'set16!) (emit-set16! x))
   ;;        ((eq? (car x) 'let) (emit-let x))
   ((eq? (car x) 'if) (emit-if x))
   ((eq? (car x) 'when) (emit-when x))
   ((eq? (car x) 'while) (emit-while x))
   ((eq? (car x) 'loop) (emit-loop x))
   ((eq? (car x) 'do) (emit-expr-list (cdr x)))
   ((eq? (car x) 'eq?) (emit-eq? x))
   ((eq? (car x) '<) (emit-< x "bcc"))
   ((eq? (car x) '<=) (emit-<= x "bcc"))
   ((eq? (car x) '>) (emit-> x "bcs"))
   ((eq? (car x) '<s) (emit-< x "bmi"))
   ((eq? (car x) '<=s) (emit-<= x "bmi"))
   ((eq? (car x) '>s) (emit-> x "bpl"))
   ((eq? (car x) 'not) (emit-not x))
   ((eq? (car x) '+) (emit-add x))
   ((eq? (car x) '-) (emit-sub x))
   ((eq? (car x) '*) (emit-mul x))
   ((eq? (car x) 'and) (binary-procedure "and" x))
   ((eq? (car x) 'or) (binary-procedure "ora" x))
   ((eq? (car x) 'xor) (binary-procedure "eor" x))
   ((eq? (car x) 'inc) (emit "inc" (immediate-value (cadr x))))
   ((eq? (car x) 'dec) (emit "dec" (immediate-value (cadr x))))
   ((eq? (car x) '<<) (emit-left-shift x))
   ((eq? (car x) '>>) (emit-right-shift x))
   ((eq? (car x) 'high) (emit "lda" (string-append "#>"
                                                   (symbol->string (cadr x)))))
   ((eq? (car x) 'low) (emit "lda" (string-append "#<"
                                                  (symbol->string (cadr x)))))
   ((eq? (car x) '+16!) (emit-add16 x))
   ((eq? (car x) '-16!) (emit-sub16 x))
   ((eq? (car x) '_rnd) (emit-rnd x))
   ((eq? (car x) 'wait-vblank)
    (append (emit "- lda $2002")
            (emit "bpl -")))
   ((eq? (car x) 'org) (emit ".org" (immediate-value (cadr x))))
   ((eq? (car x) 'poke!) (emit-poke! x))
   ((eq? (car x) 'peek) (emit-peek x))
   ((eq? (car x) 'peek16) (emit-peek16 x))
   ((eq? (car x) 'memset) (emit-memset x))
   ((eq? (car x) 'ppu-memset) (emit-ppu-memset x))
   ((eq? (car x) 'ppu-memcpy) (emit-ppu-memcpy x))
   ((eq? (car x) 'ppu-memcpy16) (emit-ppu-memcpy16 x))
   ((eq? (car x) 'set-sprite-y!) (emit-set-sprite! 0 x))
   ((eq? (car x) 'set-sprite-id!) (emit-set-sprite! 1 x))
   ((eq? (car x) 'set-sprite-attr!) (emit-set-sprite! 2 x))
   ((eq? (car x) 'set-sprite-x!) (emit-set-sprite! 3 x))
   ((eq? (car x) 'get-sprite-y) (emit-get-sprite 0 x))
   ((eq? (car x) 'get-sprite-id) (emit-get-sprite 1 x))
   ((eq? (car x) 'get-sprite-attr) (emit-get-sprite 2 x))
   ((eq? (car x) 'get-sprite-x) (emit-get-sprite 3 x))
   ((eq? (car x) 'set-sprites-2x2-x!) (emit-set-sprites-x-2x2! x))
   ((eq? (car x) 'set-sprites-2x2-y!) (emit-set-sprites-y-2x2! x))
   ((eq? (car x) 'add-sprites-x!) (emit-zzz-sprites! 3 "adc" x))
   ((eq? (car x) 'add-sprites-y!) (emit-zzz-sprites! 0 "adc" x))
   ((eq? (car x) 'sub-sprites-x!) (emit-zzz-sprites! 3 "sbc" x))
   ((eq? (car x) 'sub-sprites-y!) (emit-zzz-sprites! 0 "sbc" x))
   ((eq? (car x) 'or-sprites-attr!) (emit-zzz-sprites! 2 "eor" x))
   ((eq? (car x) 'animate-sprites-2x2!) (emit-animate-sprites-2x2! x))
   ((eq? (car x) 'init-system) (emit-init-system))
   (else
    (emit-fncall x))))


(define debug #f)

(define histogram '())

(define (add-histogram name count hist)
  (cond
   ((null? hist) (list (list name count)))
   ((eq? (car (car hist)) name)
    (cons (list name (+ (cadr (car hist)) count)) (cdr hist)))
   (else
    (cons (car hist) (add-histogram name count (cdr hist))))))

(define (histogram-print histogram)
  (for-each
   (lambda (h)
     (display (car h))(display ": ")(display (cadr h))(newline))
   histogram))

(define (emit-expr x)
  (cond
   ((immediate? x)
    (emit-load-immediate x))
   ((primcall? x)
    (let ((r (emit-procedure x)))
     ;; (display x)(newline)
      (set! histogram (add-histogram (car x) (length r) histogram))
      (append
       ;;(emit ";; starting " (symbol->string (car x)))
       r
       ;;(emit ";; ending " (symbol->string (car x)))
       )))
   (else
    (display "don't understand ")(display x)(newline) '())))

;----------------------------------------------------------------

(define (preprocess-cond-to-if x)
  (define (_ l)
    (cond
      ((null? l) 0)
      ((eq? (pre-process (caar l)) 'else) (cons 'do
                                                (pre-process (cdr (car l)))))
      (else (list 'if (pre-process (caar l))
                  (cons 'do (pre-process (cdr (car l))))
                  (_ (cdr l))))))
  (_ (cdr x)))

;; (get-sprite-vflip sprite-num)
(define (preprocess-get-sprite-vflip x)
  (list '>> (list 'get-sprite-attr (cadr x)) 7))

(define (preprocess-get-sprite-hflip x)
  (list 'and (list '>> (list 'get-sprite-attr (cadr x)) 6) #x01))

(define (preprocess-set-sprite-vflip! x)
  (list 'set-sprite-attr! (list-ref x 1) (list '<< (list-ref x 2) 6)))

(define (preprocess-set-sprite-hflip! x)
  (list 'set-sprite-attr! (list-ref x 1) (list '<< (list-ref x 2) 7)))

;; basically diy-macro from the main tinyscheme stuff
(define (pre-process s)
  (cond
    ((null? s) s)
    ((list? s)
     (map
      (lambda (i)
        (if (and (list? i) (not (null? i)))
            ;; dispatch to macro processors
            (cond
             ((eq? (car i) 'cond) (preprocess-cond-to-if i))
             ((eq? (car i) 'get-sprite-vflip) (preprocess-get-sprite-vflip i))
             ((eq? (car i) 'get-sprite-hflip) (preprocess-get-sprite-hflip i))
             ((eq? (car i) 'set-sprite-vflip!) (preprocess-set-sprite-vflip! i))
             ((eq? (car i) 'set-sprite-hflip!) (preprocess-set-sprite-hflip! i))
             (else (pre-process i)))
            (pre-process i)))
      s))
    (else s)))

(define (compile-program x)
  ;(set! variables '())
  ;(set! constants '())
  (let ((done (emit-expr (pre-process x))))
    (display "size: ")(display (length done))(newline)
    (histogram-print (sort histogram (lambda (a b) (> (cadr a) (cadr b)))))
    done))

(define (output fn x)
  (let ((f (open-output-file fn #:exists 'replace)))
    (for-each
     (lambda (line)
       (display line f)(newline f))
     (compile-program x))
    (close-output-port f)
    ;;(histogram-print histogram)
    ))

(define (assert fn x)
  (when (not x)
    (display "assert failed: ")(display fn)(newline)))

(define (dbg x)
  (display x)(newline) x)

(define (test)
  (assert "emit" (equal? (emit "1" "2" "3") (list "1 2 3")))
  (assert "reg-table-lookup 1" (equal? (reg-table-lookup
                                        'reg-apu-pulse1-control) "$4000"))
  (assert "reg-table-lookup 2" (not (reg-table-lookup 'nonsense)))
  (assert "emit-load-variable 1"
          (equal? (emit-load-variable 'reg-apu-pulse1-control)
                  (list "lda $4000")))
  (make-variable! 'poodle)
  (assert "emit-load-variable 2"
          (equal? (emit-load-variable 'poodle) (list "lda $00")))
  (assert "emit-load-immediate 1"
          (equal? (emit-load-immediate 'reg-oam-dma) (list "lda $4014")))
  ;;(assert "emit-load-immediate 2"
  ;;        (equal? (emit-load-immediate 'poodle) (list "lda $00")))
  ;;(assert "emit-defvar" (equal? (emit-defvar '(defvar poodle2 30)) '("lda #30" "sta $01")))
  )

;(test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Utilities

(define (find-keyword keyword elems)
  (if (or (empty? elems) (empty? (cdr elems)))
      #f
      (if (eq? keyword (car elems))
          (cadr elems)
          (find-keyword keyword (cddr elems)))))

(define (->string x)
  (call-with-output-string
   (lambda (out)
     (display x out))))

(define (left-pad text pad len)
  (if (>= (string-length text) len)
      text
      (string-append (make-string (- len (string-length text)) pad) text)))

(define (normalize-name name)
  (dash->underscore (symbol->string name)))

(define (atom? obj)
  (and (not (null? obj))
       (not (pair? obj))))

(define (list->byte-string ls)
  (string-join (map (lambda (x)
                      (string-append "$" (left-pad (format "~x" x) #\0 2))) ls)
               ","))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Source file context information for debugging / compiler metadata

(define *co2-source-form* (make-parameter #f))

(define (co2-source-context)
  (let* ((form (*co2-source-form*))
         (fname (syntax-source form))
         (line-num (syntax-line form))
         (source (->string (syntax->datum form)))
         (len (min 40 (string-length source))))
    (format ";~a:~a ~a" fname line-num (substring source 0 len))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Built-in functions

(define (built-in-nes-header num-prg num-chr mapper mirroring)
  (let ((third-byte (+ (* mapper #x10) (if (eq? mirroring 'vertical) 1 0))))
    (printf "\n~a\n" (co2-source-context))
    (printf ".byte \"NES\",$1a\n")
    (printf ".byte $~x\n" (or num-prg 1))
    (printf ".byte $~x\n" (or num-chr 0))
    (printf ".byte $~x\n" third-byte)
    (printf ".byte ~a\n" (string-join (build-list 9 (lambda (x) "$0")) ","))
    (printf "\n")
    (printf ".org $c000\n")
    (printf "\n")))

(define (built-in-init-system)
  ;; disable interrupts while we set stuff up
  (printf "  sei\n")
  ;; make sure we're not using decimal mode
  (printf "  cld\n")
  ;; wait for 2 vblanks
  (printf "  - lda $2002\n")
  (printf "  bpl -\n")
  (printf "  - lda $2002\n")
  (printf "  bpl -\n")
  ;; clear out all ram
  (printf "  ldx #$00\n")
  (printf "- lda #$00\n")
  (printf "  sta $000,x\n")
  (printf "  sta $100,x\n")
  (printf "  sta $300,x\n")
  (printf "  sta $400,x\n")
  (printf "  sta $500,x\n")
  (printf "  sta $600,x\n")
  (printf "  sta $700,x\n")
  (printf "  lda #$ff\n")
  (printf "  sta $200,x\n")
  (printf "  inx\n")
  (printf "  bne -\n")
  ;; reset the stack pointer.
  (printf "  ldx #$ff\n")
  (printf "  txs\n"))

(define func-nodes (make-hash))

(struct func-node (name params calls [memory #:mutable]))

(define (make-func-node! name params calls)
  (hash-set! func-nodes name (func-node name params calls #f)))

(define *invocations* (make-parameter #f))

(define (process-defvar name)
  (let* ((def (normalize-name name))
         (sym-label (make-variable! name))
         (addr (sym-label-address sym-label)))
    (printf "\n~a\n" (co2-source-context))
    (printf "~a = $~a\n" def (left-pad (number->string addr 16) #\0 2))))

(define (process-defconst name value)
  (let* ((def (normalize-name name))
         (sym-label (make-const! name value))
         (value (sym-label-address sym-label)))
    (printf "\n~a\n" (co2-source-context))
    (printf "~a = $~a\n" def (left-pad (number->string value 16) #\0 2))))

(define (process-proc type context-decl body)
  (assert type symbol?)
  (assert context-decl syntax?)
  (assert body list?)
  (let* ((decl (syntax->datum context-decl))
         (name (car decl))
         (args (cdr decl)))
    (parameterize ([*invocations* (make-gvector)])
      ; TODO: Add number of parameters to table, to check when called.
      (make-function! name)
      (printf "\n~a\n" (co2-source-context))
      (printf "~a:\n" (normalize-name name))
      ; Push scope for local variables.
      (sym-label-push-scope)
      ; Fastcall parameters
      (for ([sym args] [i (in-naturals)])
           (let ((label (format "_~a__~a" (normalize-name name)
                                (normalize-name sym))))
             (make-variable! sym #:label label)
             (cond
              ; TODO: Get name from the symbol table.
              ([= i 0] (printf "  sta ~a\n" (as-arg sym)))
              ([= i 1] (printf "  stx ~a\n" (as-arg sym)))
              ([= i 2] (printf "  sty ~a\n" (as-arg sym))))))
      ; Additional parameters
      (when (> (length args) 3)
            (printf "  tsx\n")
            (let ((params (cdddr args)))
              (for ([p params] [i (in-naturals)])
                   ; TODO: Get parameter from the stack, store in local frame.
                   (printf "  lda $~x,x\n" (+ #x103 i))
                   (printf "  sta ~a\n" (as-arg p)))))
      ; Process body.
      (for ([stmt body])
           (process-statement stmt))
      ; Pop scope.
      (sym-label-pop-scope)
      ; Return from function.
      (cond
       [(eq? type 'sub) (printf "  rts\n")]
       [(eq? type 'vector) (printf "  rti\n")])
      ; Store inner function calls for building call tree.
      (let ((calls (gvector->list (*invocations*))))
        (make-func-node! name args calls)))))

(define (process-set-bang target expr)
  (assert target syntax?)
  (assert expr syntax?)
  (let ((place (syntax->datum target))
        (result-need-context (process-expression expr))) ; Result left in A
    (when result-need-context
      (printf "~a\n" (co2-source-context)))
    ; TODO: Assert that place is a valid lvalue for set!
    (printf "  sta ~a\n" (as-arg place))))

(define (process-instruction-expression instr lhs rhs more)
  ; If these have one operand:
  ;  If that is an expression: evaluate it, apply this instruction to A
  ;  If that is an atom: apply this instruction to the atom
  ; If two operands:
  ;  Evaluate first expression / Load it with "lda"
  ;  If second is an expression:
  ;   Push A to the stack, evaluate the expression
  ;  ...
  (assert instr symbol?)
  (assert lhs syntax?)
  (when (and rhs (not (null? rhs))) (assert rhs syntax?))
  (when (and more (not (null? more))) (assert more syntax?))
  (let ((left (syntax->datum lhs))
        (right (if (and rhs (not (null? rhs))) (syntax->datum rhs) '()))
        (extra (if (and more (not (null? more))) (syntax->datum more) '())))
    (cond
     ; Single argument, which is an atom.
     ([and (atom? left) (null? right)]
      (begin (printf "  ~a ~a\n" instr (as-arg left))))
     ; Two arguemnts, second is an index register.
     ([and (atom? left) (index-register? right)]
      (begin (printf "  ~a ~a,~a\n" instr (as-arg left) (->register right))))
     ; TODO: (indirect),y
     ;...
     ; Two arguments, both atoms.
     ([and (atom? left) (atom? right) (null? extra)]
      (begin (printf "  lda ~a\n" (as-arg left))
             (printf "  ~a ~a\n" instr (as-arg right))))
     ; Two arguments, first is expression and second is atom.
     ([and (list? left) (atom? right) (null? extra)]
      (begin (process-expression lhs)
             (printf "  ~a ~a\n" instr (as-arg right))))
     ; Three arguments.
     ; TODO: Only implemented for ora
     ([and (atom? left) (atom? right) (atom? extra) (eq? instr 'ora)]
      (begin (printf "  lda ~a\n" (as-arg left))
             (printf "  ~a ~a|~a\n" instr (as-arg right) (as-arg extra))))
     (else (error (format "ERROR expression: ~a ~a ~a\n" instr lhs rhs))))))

(define (process-instruction-accumulator instr lhs)
  (assert instr symbol?)
  (assert lhs syntax?)
  (let ((left (syntax->datum lhs)))
    (cond
     ; Single argument, which is an atom.
     ([and (atom? left)]
      (begin (printf "  ~a ~a\n" instr (as-arg left))))
     ; Single argument, an expression.
     ([and (list? left)]
      (begin (process-expression lhs)
             (printf "  ~a a\n" instr)))
     (else (error (format "ERROR accumulator: ~a ~a\n" instr lhs))))))

(define (index-register? arg)
  (and (list? arg) (eq? (car arg) 'quote)
       (or (eq? (cadr arg) 'x) (eq? (cadr arg) 'y))))

(define (->register arg)
  (symbol->string (cadr arg)))

(define (as-arg arg)
  (cond
   ([string? arg] arg)
   ([symbol? arg] (let ((lookup (sym-label-lookup arg)))
                    (if (not lookup)
                        ; TODO: Throw an error, use syntax object
                        (format "\"not found ~a\"" arg)
                        (if (eq? (sym-label-kind lookup) 'const)
                            (format "#~a" (sym-label-name lookup))
                            (sym-label-name lookup)))))
   ([number? arg] (format "#$~x" arg))
   (else (error (format "ERROR as-arg: ~a\n" arg)))))

(define (process-instruction-standalone instr operand)
  ;TODO: Rhs being an expression is an error
  ;(printf ";****** alone ~a ~a\n" instr expr)
  (assert instr symbol?)
  (assert operand syntax?)
  (let ((value (syntax->datum operand)))
    (printf "~a\n" (co2-source-context))
    (cond
     ([symbol? value] (printf "  ~a ~a\n" instr (normalize-name value)))
     (else (error (format "ERROR standalone: ~a\n" value))))))

(define (process-instruction-branch instr target)
  (assert instr symbol?)
  (assert target syntax?)
  (let ((value (syntax->datum target)))
    (assert value symbol?)
    (printf "~a\n" (co2-source-context))
    (printf "  ~a ~a\n" instr (cadr (assoc value (lexical-scope))))))

(define (process-instruction-implied instr)
  (assert instr symbol?)
  (printf "~a\n" (co2-source-context))
  (printf "  ~a\n" instr))

(define (process-expression expr)
  (assert expr syntax?)
  (let* ((value (syntax->datum expr)))
    (cond
     ([list? value] (process-statement expr) #t)
     ([number? value] (begin
                        ; Output context here, then load value.
                        (printf "~a\n" (co2-source-context))
                        (printf "  lda #$~x\n" value)
                        #f))
     ([symbol? value] (let* ((lookup (sym-label-lookup value))
                             (name (sym-label-name lookup)))
                        (printf "  lda ~a\n" name)
                        #t))
     (else (error (format "ERROR: ~a\n" value))))))

(define lexical-scope (make-parameter '()))

(define (process-block context-label body)
  (assert context-label syntax?)
  (assert body list?)
  (let* ((label (syntax->datum context-label))
         (gen-label (generate-label (symbol->string label))))
    (printf "~a:\n" gen-label)
    (parameterize ([lexical-scope (cons (list label gen-label)
                                        (lexical-scope))])
      (for ([stmt body])
           (process-statement stmt)))))

(define (process-loop-down context-reg context-start body)
  (assert context-reg syntax?)
  (assert context-start syntax?)
  (assert body list?)
  (printf "~a\n" (co2-source-context))
  (let ((reg (syntax->datum context-reg))
        (start (syntax->datum context-start))
        (initial-loop-value #f))
    (cond
     ([= start 0] (error "Cannot start loop at 0"))
     ([< start #x100] (set! initial-loop-value start))
     ([= start #x100] (set! initial-loop-value 0))
     (else (error "Initial loop value invalid")))
    (printf "  ld~a #~a\n" reg initial-loop-value)
    (let ((loop-label (generate-label "loop_down_from")))
      (printf "~a:\n" loop-label)
      ; TODO: Disallow `reg` changes within `body`
      (for ([stmt body])
           (process-statement stmt))
      (printf "  de~a\n" reg)
      (printf "  bne ~a\n" loop-label))))

(define (process-loop-up context-reg context-start context-end body)
  (assert context-reg syntax?)
  (assert context-start syntax?)
  (assert context-end syntax?)
  (assert body list?)
  (printf "~a\n" (co2-source-context))
  (let* ((reg (syntax->datum context-reg))
         (start (syntax->datum context-start))
         ; TODO: Assuming end is a list like (length const-static-array).
         (end (syntax->datum context-end))
         (initial-loop-value start)
         ; TODO: Value reserved by implemention. Ensure `body` doesn't modify it
         (sentinal-value "_count"))
    (when (not (= initial-loop-value 0))
          (error "Start must be 0, other values not supported yet"))
    (printf "  lda #~a_~a\n"
            (normalize-name (cadr end)) (normalize-name (car end)))
    (printf "  sta ~a\n" sentinal-value)
    (printf "  ld~a #~a\n" reg initial-loop-value)
    (let ((loop-label (generate-label "loop_up_to")))
      (printf "~a:\n" loop-label)
      ; TODO: Disallow `reg` changes within `body`
      (for ([stmt body])
           (process-statement stmt))
      (printf "  in~a\n" reg)
      (printf "  cp~a ~a\n" reg sentinal-value)
      (printf "  bne ~a\n" loop-label))))

(define (process-let context-bindings body)
  ; TODO: This c(a|d)*r calls are awful.
  (let* ((bindings (syntax->datum context-bindings))
         (label (caar bindings))
         (value (cadr (cadar bindings))))
    (make-data! label value)
    (for ([stmt body])
         (process-statement stmt))))

(define (process-if condition truth-case false-case)
  (let ((truth-label (generate-label "truth_case"))
        (false-label (generate-label "false_case"))
        (if-done-label (generate-label "if_done")))
    (printf "  ; condition begin\n")
    (if (list? (syntax->datum condition))
        (process-statement condition)
        #f)
    (printf "  bne ~a\n" false-label)
    (printf "~a:\n" truth-label)
    (if (list? (syntax->datum truth-case))
        (process-statement truth-case)
        (printf "  lda ~a\n" (as-arg (syntax->datum truth-case))))
    (printf "  jmp ~a\n" if-done-label)
    (printf "~a:\n" false-label)
    (if (list? (syntax->datum false-case))
        (process-statement false-case)
        (printf "  lda ~a\n" (as-arg (syntax->datum false-case))))
    (printf "~a:\n" if-done-label)
    (printf "  ; condition done\n")
    ))

(define (process-math operator lhs rhs)
  (assert operator symbol?)
  (assert lhs syntax?)
  (assert rhs syntax?)
  (let ((left (syntax->datum lhs))
        (right (syntax->datum rhs)))
    (if (list? left)
        (process-expression lhs)
        (printf "  lda ~a\n" left))
    (when (list? right)
          (printf "  pha\n")
          (process-expression rhs)
          (printf "  sta _count\n")
          (printf "  pla\n")
          (set! right "_count"))
    (case operator
      ([+]
       (begin (printf "  clc\n")
              (printf "  adc ~a\n" (as-arg right)))))))

(define (process-stack action registers)
  (assert action symbol?)
  (assert registers list?)
  ; TODO: Only push/pull what's in the `registers` parameter
  (printf "~a\n" (co2-source-context))
  (cond
   [(eq? action 'push) (begin (printf "  pha\n")
                              (printf "  txa\n")
                              (printf "  pha\n")
                              (printf "  tya\n")
                              (printf "  pha\n"))]
   [(eq? action 'pull) (begin (printf "  pla\n")
                              (printf "  tay\n")
                              (printf "  pla\n")
                              (printf "  tax\n")
                              (printf "  pla\n"))]))

(define (process-jump-subroutine fname params)
  (let* ((pop-count 0))
    (printf "~a\n" (co2-source-context))
    (when (> (length params) 3)
          (for ([elem (reverse (cdddr params))])
               (let ((data (syntax->datum elem)))
                 (set! pop-count (+ 1 pop-count))
                 (printf "  lda ~a\n" (as-arg data))
                 (printf "  pha\n"))))
    (for ([elem params] [i (in-naturals)])
         (let ((data (syntax->datum elem)))
           (cond
            ; TODO: Evaluate expressions.
            ([= i 0] (printf "  lda ~a\n" (as-arg data)))
            ([= i 1] (printf "  ldx ~a\n" (as-arg data)))
            ([= i 2] (printf "  ldy ~a\n" (as-arg data))))))
    (printf "  jsr ~a\n" (normalize-name fname))
    (for ([i (in-range pop-count)])
         (printf "  pla\n"))
    (gvector-add! (*invocations*) fname)))

(define (process-statement stmt)
  ; TODO: Rename to process-inner-form
  (assert stmt syntax?)
  (let* ((inner (syntax-e stmt))
         (first (car inner))
         (rest (cdr inner))
         (symbol (syntax->datum first)))
    (parameterize ([*co2-source-form* stmt])
      (if (function? symbol)
        (process-jump-subroutine symbol rest)
        (case symbol
          ; Main expression walker.
          [(init-system) (built-in-init-system)]
          [(set!) (process-set-bang (car rest) (cadr rest))]
          [(block) (process-block (car rest) (cdr rest))]
          [(loop-down-from) (process-loop-down (car rest) (cadr rest)
                                               (cddr rest))]
          [(loop-up-to) (process-loop-up (car rest) (cadr rest)
                                         (caddr rest) (cdddr rest))]
          [(let) (process-let (car rest) (cdr rest))]
          [(if) (process-if (car rest) (cadr rest) (caddr rest))]
          [(push pull) (process-stack symbol rest)]
          [(adc and cmp cpx cpy eor lda ora sta)
           (process-instruction-expression symbol
                                           (car rest)
                                           (if (not (null? (cdr rest)))
                                               (cadr rest) '())
                                           (if (and (not (null? (cdr rest)))
                                                    (not (null? (cddr rest))))
                                               (caddr rest) '()))]
          [(asl lsr rol ror)
           (process-instruction-accumulator symbol (car rest))]
          [(bit dec inc)
           (process-instruction-standalone symbol (car rest))]
          [(beq bne jmp)
           (process-instruction-branch symbol (car rest))]
          [(clc)
           (process-instruction-implied symbol)]
          [(+)
           (process-math symbol
                         (car rest)
                         (if (not (null? (cdr rest)))
                             (cadr rest) '()))]
          [else (process-todo symbol)])))))

(define (process-unknown symbol)
  (printf "\n~a\n" (co2-source-context))
  (printf "Unknown: ~a\n" symbol))

(define (process-todo symbol)
  (printf "\n~a\n" (co2-source-context))
  (printf ";;;;;;;; TODO: ~a\n" symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Syntax tree walker

(define (process-keys args allowed-keywords)
  (for/list ([k allowed-keywords])
    (find-keyword k (map syntax->datum args))))

(define (process-args args num-required num-optional)
  ; TODO: Implement num-required and num-optional
  (map syntax-e args))

(define (process-top-level-form form)
  (let* ((inner (syntax-e form))
         (first (car inner))
         (rest (cdr inner))
         (symbol (syntax->datum first)))
    (parameterize ([*co2-source-form* form])
      (case symbol
        [(nes-header) (apply built-in-nes-header
                             (process-keys rest '(#:num-prg #:num-chr
                                                  #:mapper #:mirroring)))]
        [(defconst) (apply process-defconst (process-args rest 2 0))]
        [(defvar) (apply process-defvar (process-args rest 1 1))]
        [(defsub) (process-proc 'sub (car rest) (cdr rest))]
        [(defvector) (process-proc 'vector (car rest) (cdr rest))]
        [else (process-unknown symbol)]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (resolve-func-node-memory func-nodes n)
  (let* ((f (hash-ref func-nodes n))
         (name (func-node-name f))
         (params (func-node-params f))
         (calls (func-node-calls f))
         (memory (func-node-memory f)))
    (if (number? memory)
        memory ; return early
        (begin (let ((total 0)
                     (curr 0))
                 (for ([c calls])
                   (set! curr (resolve-func-node-memory func-nodes c))
                   (when (> curr total)
                     (set! total curr)))
                 (set! total (+ total (length params)))
                 (set-func-node-memory! f total)
                 total)))))

(define (traverse-func-nodes)
  (let ((names (hash-keys func-nodes)))
    (for ([n names])
      (resolve-func-node-memory func-nodes n))))

(define (display-func-nodes)
  (printf "\n\n")
  (let ((names (hash-keys func-nodes)))
    (for ([n names])
         (let* ((f (hash-ref func-nodes n))
                (name (func-node-name f))
                (params (func-node-params f))
                (calls (func-node-calls f))
                (memory (func-node-memory f))
                (k (- memory (length params))))
           (for ([p params] [i (in-naturals)])
                (printf "_~a__~a = $~x\n" (normalize-name name)
                        (normalize-name p) (+ k i #x40)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Entry point

; source symbol, assembly label, address / const

(define (define-built-ins)
  ;; Memory-mapped addresses
  (for ([elem reg-table])
       (let ((symbol (car elem))
             (value (cadr elem)))
         (make-address! symbol value)))
  ;; PPU flags
  (for ([elem ppu-flags])
       (let ((symbol (car elem))
             (value (cadr elem)))
         (make-const! symbol value)))
  ;; Reserved zeropage variables for internal use
  (for ([elem reserved-zero-page])
       (let ((symbol (car elem))
             (value (cadr elem)))
         (make-address! symbol value))))

(define (output-prefix)
  ;; Memory-mapped addresses
  (for ([elem reg-table])
       (let ((symbol (car elem))
             (value (cadr elem)))
         (printf "~A = $~x\n" (normalize-name symbol) value)))
  ;; PPU flags
  (for ([elem ppu-flags])
       (let ((symbol (car elem))
             (value (cadr elem)))
         (printf "~A = $~x\n" (normalize-name symbol) value)))
  ;; Reserved zeropage variables for internal use
  (for ([elem reserved-zero-page])
       (let ((symbol (car elem))
             (value (cadr elem)))
         (printf "~A = $~x\n" (normalize-name symbol) value)))
  (printf "\n"))

(define (output-suffix)
  (printf "\n\n")
  (let ((data (get-data-segment)))
    (for/list ([elem data])
      (let ((label (car elem))
            (value (cadr elem)))
        (printf "~a:\n" label)
        (printf "  .byte ~a\n" (list->byte-string value))
        (printf "~a_length = ~a\n" label (length value))
        (printf "\n"))))
  (printf ".pad $fffa\n")
  ; TODO: Only output vectors that are defined.
  (printf ".word nmi, reset, 0\n"))

(define (process-co2 fname out-filename f)
  (define-built-ins)
  (output-prefix)
  (define (loop)
    (let ((top-level-form (read-syntax fname f)))
      (when (not (eof-object? top-level-form))
            (process-top-level-form top-level-form)
            (loop))))
  (loop)
  (output-suffix))

(let* ((fname (command-line #:args (input) input))
       (f (open-input-file fname)))
  (port-count-lines! f)
  (process-co2 fname "out.asm" f)
  (traverse-func-nodes)
  (display-func-nodes)
  (close-input-port f))
