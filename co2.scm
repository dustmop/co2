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

;; internal compiler register on zero page
(define working-reg "$ff")
(define stack-frame-h "$fe")
(define stack-frame-l "$fd")
(define rnd-reg "$fc")

(define reg-table
  ;; ppu registers
  '((reg-ppu-ctl              "$2000")
    (reg-ppu-mask             "$2001")
    (reg-ppu-status           "$2002")
    (reg-oam-addr             "$2003")
    (reg-oam-data             "$2004")
    (reg-ppu-scroll           "$2005")
    (reg-ppu-addr             "$2006")
    (reg-ppu-data             "$2007")
    ;;  apu registers
    (reg-apu-pulse1-control   "$4000")
    (reg-apu-pulse1-ramp      "$4001")
    (reg-apu-pulse1-ft        "$4002")
    (reg-apu-pulse1-ct        "$4003")
    (reg-apu-pulse2-control   "$4004")
    (reg-apu-pulse2-ramp      "$4005")
    (reg-apu-pulse2-ft        "$4006")
    (reg-apu-pulse2-ct        "$4007")
    (reg-apu-tri-control      "$4008")
    (reg-apu-tri-ft           "$400a")
    (reg-apu-tri-ct           "$400b")
    (reg-apu-noise-env        "$400c")
    (reg-apu-noise-ft         "$400e")
    (reg-apu-noise-ct         "$400f")
    (reg-apu-dmc-control      "$4010")
    (reg-apu-dmc-dac          "$4011")
    (reg-apu-dmc-addr         "$4012")
    (reg-apu-dmc-size         "$4013")
    (reg-oam-dma              "$4014")
    (reg-apu-channel          "$4015")
    ;; input
    (reg-joypad-0             "$4016")
    (reg-joypad-1             "$4017")
    ;; ppu vram addresses
    (ppu-name-table-0 ("#$20" "#$00"))
    (ppu-attr-table-0 ("#$23" "#$c0"))
    (ppu-name-table-1 ("#$24" "#$00"))
    (ppu-attr-table-1 ("#$27" "#$c0"))
    (ppu-name-table-2 ("#$28" "#$00"))
    (ppu-attr-table-2 ("#$2b" "#$c0"))
    (ppu-name-table-3 ("#$2c" "#$00"))
    (ppu-attr-table-3 ("#$2f" "#$c0"))
    (ppu-palette      ("#$3f" "#$00"))
    (ppu-bg-palette   ("#$3f" "#$00"))
    (ppu-sprite-palette ("#$3f" "#$10"))
    ;; how many times to read reg-joypad-x to get the button
    (joypad-a "#0")
    (joypad-b "#1")
    (joypad-select "#2")
    (joypad-start "#3")
    (joypad-up "#4")
    (joypad-down "#5")
    (joypad-left "#6")
    (joypad-right "#7")
    ;; debug
    (rnd-reg "$fb")
    ))

(define (reg-table-lookup x)
  (let ((lu (assoc x reg-table)))
    (if lu (cadr lu) #f)))

;;-------------------------------------------------------------
;; a label generator

(define label-id 99)

(define (generate-label name)
  (set! label-id (+ label-id 1))
  (string-append name "_" (number->string label-id)))

;;----------------------------------------------------------------
;; variables are just an address lookup table to the zero page

(define variables '())

(define (make-variable! name)
  (when (not (memq name variables))
        (set! variables (append variables (list name)))))

(define (byte->string byte)
  (string-upcase (string-append
                  (number->string (quotient byte 16) 16)
                  (number->string (remainder byte 16) 16))))

(define (variable-lookup name)
  (define (_ l c)
    (cond
     ((null? l) (display "cant find variable ")(display name)(newline) #f)
     ((equal? name (car l)) (string-append "$" (byte->string c)))
     (else (_ (cdr l) (+ c 1)))))
  (_ variables 0))

;;----------------------------------------------------------------
;; constants lookup
(define constants '())

(define (make-constant! name value)
  (set! constants (cons (list name value) constants)))

(define (constant-lookup name)
  (let ((t (assoc name constants)))
    (if t (cadr t) #f)))

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

;; lookup a symbol everywhere, in order...
(define (lookup name)
  ;; check registers first
  (let ((reg (reg-table-lookup name)))
    (if reg reg
        ;; then constants
        (let ((const (constant-lookup name)))
          (if const const
	      ;; finally variables
	      (variable-lookup name))))))

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
   (emit "clc")
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
(define (emit-loop x)
  (let ((label (generate-label "loop")))
    (append
     (emit-expr (list-ref x 2))
     (emit "sta" (immediate-value (list-ref x 1)))
     (emit-label label)
     (emit-expr-list (cddddr x))
     (emit "sta" working-reg) ;; store return in case we need it
     (emit "inc" (immediate-value (list-ref x 1)))
     (emit-expr (list-ref x 3))
     (emit "cmp" (immediate-value (list-ref x 1)))
     (emit "bcs" label)
     (emit "lda" working-reg)))) ;; retrieve return

;; (if pred then else)
(define (emit-if x)
  (let ((false-label (generate-label "if_false"))
        (end-label (generate-label "if_end")))
    (append
     (emit-expr (list-ref x 1))
     (emit "cmp" "#0")
     (emit "beq" false-label)
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
	(next-label (generate-label "while_loop_next")))
    (append
     (emit-label loop-label)
     (emit-expr-list (cddr x)) ;; loop block
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
(define (emit-< x)
  (let ((true-label (generate-label "gt_true"))
        (end-label (generate-label "gt_end")))
    (append
     (emit-expr (list-ref x 1))
     (emit "pha")
     (emit-expr (list-ref x 2))
     (emit "sta" working-reg)
     (emit "pla")
     (emit "clc")
     (emit "adc" "#1")
     (emit "sbc" working-reg)
     (emit "bmi" true-label) ;; branch on minus
     (emit "lda" "#0")
     (emit "jmp" end-label)
     (emit-label true-label)
     (emit "lda" "#1")
     (emit-label end-label))))

;; correct, but hella slow
(define (emit-<= x)
  (let ((true-label (generate-label "gt_true"))
        (end-label (generate-label "gt_end")))
    (append
     (emit-expr (list-ref x 1))
     (emit "pha")
     (emit-expr (list-ref x 2))
     (emit "sta" working-reg)
     (emit "pla")
     (emit "sbc" working-reg)
     (emit "bmi" true-label) ;; branch on minus
     (emit "lda" "#0")
     (emit "jmp" end-label)
     (emit-label true-label)
     (emit "lda" "#1")
     (emit-label end-label))))

(define (emit-> x)
  (let ((true-label (generate-label "lt_true"))
        (end-label (generate-label "lt_end")))
    (append
     (emit-expr (list-ref x 1))
     (emit "pha")
     (emit-expr (list-ref x 2))
     (emit "sta" working-reg)
     (emit "pla")
     (emit "sbc" working-reg)
     (emit "bpl" true-label) ;; branch on plus
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
   ((eq? (car x) 'defvar) (emit-defvar x))
   ((eq? (car x) 'defun) (emit-defun x))
   ((eq? (car x) 'defint) (emit-defint x))
   ((eq? (car x) 'defconst) (make-constant! (cadr x) (caddr x)) '())
   ((eq? (car x) 'set!) (emit-set! x))
   ;;        ((eq? (car x) 'let) (emit-let x))
   ((eq? (car x) 'if) (emit-if x))
   ((eq? (car x) 'when) (emit-when x))
   ((eq? (car x) 'while) (emit-while x))
   ((eq? (car x) 'loop) (emit-loop x))
   ((eq? (car x) 'do) (emit-expr-list (cdr x)))
   ((eq? (car x) 'eq?) (emit-eq? x))
   ((eq? (car x) '<) (emit-< x))
   ((eq? (car x) '<=) (emit-<= x))
   ((eq? (car x) '>) (emit-> x))
   ((eq? (car x) 'not) (emit-not x))
   ((eq? (car x) '+) (append (emit "clc") (binary-procedure "adc" x)))
   ((eq? (car x) '-) (append (emit "sec") (binary-procedure "sbc" x)))
   ((eq? (car x) '*) (append (emit "clc") (emit-mul x)))
   ((eq? (car x) 'and) (binary-procedure "and" x))
   ((eq? (car x) 'or) (binary-procedure "ora" x))
   ((eq? (car x) 'xor) (binary-procedure "eor" x))
   ((eq? (car x) 'inc) (emit "inc" (immediate-value (cadr x))))
   ((eq? (car x) 'dec) (emit "dec" (immediate-value (cadr x))))
   ((eq? (car x) '<<) (emit-left-shift x))
   ((eq? (car x) '>>) (emit-right-shift x))
   ((eq? (car x) '_rnd) (emit-rnd x))
   ((eq? (car x) 'wait-vblank)
    (append (emit "- lda $2002")
            (emit "bpl -")))
   ((eq? (car x) 'org) (emit ".org" (immediate-value (cadr x))))
   ((eq? (car x) 'poke!) (emit-poke! x))
   ((eq? (car x) 'peek) (emit-peek x))
   ((eq? (car x) 'memset) (emit-memset x))
   ((eq? (car x) 'ppu-memset) (emit-ppu-memset x))
   ((eq? (car x) 'ppu-memcpy) (emit-ppu-memcpy x))
   ((eq? (car x) 'set-sprite-y!) (emit-set-sprite! 0 x))
   ((eq? (car x) 'set-sprite-id!) (emit-set-sprite! 1 x))
   ((eq? (car x) 'set-sprite-attr!) (emit-set-sprite! 2 x))
   ((eq? (car x) 'set-sprite-x!) (emit-set-sprite! 3 x))
   ((eq? (car x) 'get-sprite-y) (emit-get-sprite 0 x))
   ((eq? (car x) 'get-sprite-id) (emit-get-sprite 1 x))
   ((eq? (car x) 'get-sprite-attr) (emit-get-sprite 2 x))
   ((eq? (car x) 'get-sprite-x) (emit-get-sprite 3 x))
   ((eq? (car x) 'add-sprites-x!) (emit-zzz-sprites! 3 "adc" x))
   ((eq? (car x) 'add-sprites-y!) (emit-zzz-sprites! 0 "adc" x))
   ((eq? (car x) 'sub-sprites-x!) (emit-zzz-sprites! 3 "sbc" x))
   ((eq? (car x) 'sub-sprites-y!) (emit-zzz-sprites! 0 "sbc" x))
   ((eq? (car x) 'or-sprites-attr!) (emit-zzz-sprites! 2 "eor" x))

   ((eq? (car x) 'animate-sprites-2x2!) (emit-animate-sprites-2x2! x))
   ((eq? (car x) 'init-system)
    (append
     ;; disable interrupts while we set stuff up
     (emit "sei")
     ;; make sure we're not using decimal mode
     (emit "cld")     
     ;; wait for 2 vblanks
     (emit "- lda $2002")
     (emit "bpl -")
     (emit "- lda $2002")
     (emit "bpl -")
     ;; clear out all ram
     (emit "lda #$00")
     (emit "ldx #$00")
     (emit "- sta $000,x")
     (emit "sta $100,x")
     (emit "sta $200,x")
     (emit "sta $300,x")
     (emit "sta $400,x")
     (emit "sta $500,x")
     (emit "sta $600,x")
     (emit "sta $700,x")
     (emit "inx")
     (emit "bne -")
     ;; reset the stack pointer.
     (emit "ldx #$ff")
     (emit "txs")
     ;; setup stack frame address high byte
     (emit "lda #1")
     (emit "sta" stack-frame-h)
     (emit "lda #123")
     (emit "sta" rnd-reg)))

   (else
    (emit-fncall x)
    )))


(define debug #t)

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
      (set! histogram (add-histogram (car x) (length r) histogram))
      (append
       (emit ";; starting " (symbol->string (car x)))
       r
       (emit ";; ending " (symbol->string (car x)))
       )))
   (else
    (display "don't understand ")(display x)(newline) '())))

;----------------------------------------------------------------

(define (preprocess-cond-to-if x)
  (define (_ l)
    (cond
      ((null? l) 0)
      ((eq? (pre-process (caar l)) 'else) (cons 'do (pre-process (cdr (car l)))))
      (else (list 'if (pre-process (caar l)) (cons 'do (pre-process (cdr (car l))))
                  (_ (cdr l))))))
  (_ (cdr x)))

;; (get-sprite-vflip sprite-num)
(define (preprocess-get-sprite-vflip x)
  (list '>> (list 'get-sprite-attr (cadr x)) 7))

(define (preprocess-get-sprite-hflip x)
  (list 'and (list '>> (list 'get-sprite-attr (cadr x)) 6) #x01))

(define (preprocess-set-sprite-vflip! x)
  (list 'or-sprite-attr! (list-ref x 1) (list '<< (list-ref x 2) 6)))

(define (preprocess-set-sprite-hflip! x)
  (list 'or-sprite-attr! (list-ref x 1) (list '<< (list-ref x 2) 7)))

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
  (set! variables '())
  (set! constants '())
  (emit-expr (pre-process x)))

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
  (assert "reg-table-lookup 1" (equal? (reg-table-lookup 'reg-apu-pulse1-control) "$4000"))
  (assert "reg-table-lookup 2" (not (reg-table-lookup 'nonsense)))
  (assert "emit-load-variable 1"
          (equal? (emit-load-variable 'reg-apu-pulse1-control) (list "lda $4000")))
  (make-variable! 'poodle)
  (assert "emit-load-variable 2"
          (equal? (emit-load-variable 'poodle) (list "lda $00")))
  (assert "emit-load-immediate 1"
          (equal? (emit-load-immediate 'reg-oam-dma) (list "lda $4014")))
  ;;(assert "emit-load-immediate 2"
  ;;        (equal? (emit-load-immediate 'poodle) (list "lda $00")))
  ;;(assert "emit-defvar" (equal? (emit-defvar '(defvar poodle2 30)) '("lda #30" "sta $01")))
  )

(test)

(let ((f (open-input-file (command-line #:args (input) input))))
  (output "out.asm" (read f))
  (close-input-port f))
