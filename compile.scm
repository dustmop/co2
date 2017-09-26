#lang racket
;; co2 Copyright (C) 2016 Dave Griffiths, 2017 Dustin Long
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

(require data/gvector)
(require "casla.scm")

;; internal compiler register on zero page
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
    (PPU-CTRL-1000-BG            #x10)
    (PPU-CTRL-1000-SPR           #x08)
    (PPU-MASK-SHOW-SPR           #x10)
    (PPU-MASK-SHOW-BG            #x08)
    (PPU-MASK-NOCLIP-SPR         #x04)
    (PPU-MASK-NOCLIP-BG          #x02)))

(define reserved-zero-page
  '((ppu-ctrl                    #x00)
    (ppu-mask                    #x01)
    (frame-num                   #x02)
    (-count                      #x03)
    (-tmp                        #x04)
    (-loop                       #x05)
    (-pointer                    #x06)
    (-pointer-1                  #x07)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Label generation

(define *gen-id* 0)

(define (clear-label-id)
  (set! *gen-id* 0))

(define (generate-label name)
  (set! *gen-id* (+ *gen-id* 1))
  (string-append "_" name "_" (left-pad (number->string *gen-id* 16) #\0 4)))

;;----------------------------------------------------------------

(define (literal-address? obj)
  (and (list? obj)
       (eq? (car obj) 'addr)))

(define (literal-address-number obj)
  (cadr obj))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Symbol / label definitions. Stores vars, consts, addresses, data.

(define var-allocation #x10)

(define sym-label-defs (list (make-hash)))

(define *data-segment* '())

(define (clear-var-allocation)
  (set! var-allocation #x10)
  (set! sym-label-defs (list (make-hash))))

(define (clear-data-segment)
  (set! *data-segment* '()))

(struct sym-label (sym name address kind))

(define (make-variable! sym #:label [label #f] #:global [global #f])
  (let* ((name (normalize-name sym))
         (n var-allocation)
         (table (car sym-label-defs)))
    (define (get-global ls)
      (if (null? (cdr ls))
          (car ls)
          (get-global (cdr ls))))
    (when global
          (set! table (get-global sym-label-defs)))
    (when label
          (set! name label))
    (when (not (hash-has-key? table sym))
          (hash-set! table sym (sym-label sym name n 'var))
          (set! var-allocation (+ 1 var-allocation)))
    (hash-ref table sym)))

(define (make-local! sym scope)
  (let ((label (format "_~a__~a" (normalize-name scope) (normalize-name sym))))
    (make-variable! sym #:label label)))

(define (make-local-gensym! scope)
  (set! *gen-id* (+ *gen-id* 1))
  (let* ((uniq (format "_gen_~a" (left-pad (number->string *gen-id* 16) #\0 4)))
         (label (format "_~a__~a" (normalize-name scope) uniq)))
    (make-variable! '_gensym #:label label)
    (when (*local-vars*)
          (gvector-add! (*local-vars*) (list '_gensym label)))
    label))

(define (make-buffer! sym length)
  (let ((name (normalize-name sym))
        (n var-allocation)
        (table (car sym-label-defs)))
    (when (not (hash-has-key? table sym))
          (hash-set! table sym (sym-label sym name n 'buffer))
          (set! var-allocation (+ length var-allocation)))
    (hash-ref table sym)))

(define (make-address! sym addr)
  (let ((name (normalize-name sym)))
    (hash-set! (car sym-label-defs) sym (sym-label sym name addr 'addr))
    (hash-ref (car sym-label-defs) sym)))

(define (make-label! sym lbl)
  (let ((name (format "~a" sym)))
    (hash-set! (car sym-label-defs) sym (sym-label sym name lbl 'label))
    (hash-ref (car sym-label-defs) sym)))

(define (make-const! sym value)
  (let ((name (normalize-name sym)))
    (hash-set! (car sym-label-defs) sym (sym-label sym name value 'const))
    (hash-ref (car sym-label-defs) sym)))

(define (make-metavar! sym value)
  (let ((name (normalize-name sym)))
    (hash-set! (car sym-label-defs) sym (sym-label sym name value 'metavar))
    (hash-ref (car sym-label-defs) sym)))

(define (make-data! sym value)
  (let ((name (normalize-name sym)))
    (set! *data-segment* (cons (list name value) *data-segment*))
    (hash-set! (car sym-label-defs) sym (sym-label sym name 0 'data))
    (hash-ref (car sym-label-defs) sym)))

(define (variable? sym)
  (let ((lookup (sym-label-lookup sym)))
    (and lookup (eq? (sym-label-kind lookup) 'var))))

(define (address? sym)
  (let ((lookup (sym-label-lookup sym)))
    (and lookup (eq? (sym-label-kind lookup) 'addr))))

(define (const? sym)
  (let ((lookup (sym-label-lookup sym)))
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
  *data-segment*)

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

(define (macro? name)
  (member name '(cond when set-sprite-y! set-sprite-id! set-sprite-attr!
                      set-sprite-x! get-sprite-y get-sprite-id
                      get-sprite-attr get-sprite-x)))

;;----------------------------------------------------------------
; Emit

(define *result* (make-gvector))

(define (clear-result)
  (set! *result* (make-gvector)))

(define (fetch-result)
  (gvector->list *result*))

(define (emit . args)
  (let ((build ""))
    ; Optional label.
    (when (and (not (null? args)) (string? (car args)))
          (set! build (car args))
          (set! args (cdr args)))
    ; Symbol for opcode...
    (when (not (null? args))
          (set! build (string-append build "  " (symbol->string (car args))))
          ; ...followed by optional string.
          (when (not (null? (cdr args)))
                (if (string=? build "")
                    (set! build (cadr args))
                    (set! build (string-append build " " (cadr args))))))
    (gvector-add! *result* build)))

(define (emit-blank)
  (gvector-add! *result* ""))

(define (emit-context)
  (let ((c (co2-source-context)))
    (when c (gvector-add! *result* c))))

(define (emit-label label)
  (gvector-add! *result* (format "~a:" label)))

(define *errors* (make-gvector))

(define (clear-errors)
  (set! *errors* (make-gvector)))

(define (add-error msg value)
  (let ((c (co2-source-err-context)))
    (gvector-add! *errors* (format "ERROR @ ~a: ~a `~a`" c msg value))))

(define (has-errors?)
  (> (gvector-count *errors*) 0))

(define (display-errors)
  (for [(e *errors*)]
       (printf "~a\n" e)))

(define (first-error)
  (if (has-errors?)
      (gvector-ref *errors* 0)
      #f))

;;----------------------------------------------------------------

(define (lvalue? arg)
  (cond
   [(string? arg) #t]
   [(symbol? arg) (let ((lookup (sym-label-lookup arg)))
                    (if (not lookup)
                        #f
                        (if (eq? (sym-label-kind lookup) 'const)
                            #f
                            (if (eq? (sym-label-kind lookup) 'metavar)
                                #f
                                #t))))]
   ; TODO: More cases. Tests.
   [(number? arg) #f]
   [(char? arg) #f]
   [(literal-address? arg) #t]
   [else (error (format "ERROR: Unknown ~a" arg))]))

(define (index-register? arg)
  (or (eq? arg 'x) (eq? arg 'y)))

(define (->register arg)
  (symbol->string arg))

(define (->unsigned num)
  (if (>= num 0)
      num
      (+ #x100 num)))

(define (datum-ref list index)
  (if (>= index (length list))
      #f
      (syntax->datum (list-ref list index))))

(define (lref list index)
  (if (>= index (length list))
      #f
      (list-ref list index)))

(define (arg->str arg)
  (cond
   [(string? arg) arg]
   [(symbol? arg) (let ((lookup (sym-label-lookup arg)))
                    (if (not lookup)
                        (begin (add-error "Variable not found" arg)
                               (format ";Not found: ~a" arg))
                        (if (eq? (sym-label-kind lookup) 'const)
                            (format "#~a" (sym-label-name lookup))
                            (if (eq? (sym-label-kind lookup) 'metavar)
                                (format "#~a" (sym-label-address lookup))
                                (sym-label-name lookup)))))]
   [(number? arg) (format "#$~x" (->unsigned arg))]
   [(char? arg) (format "#$~x" (->unsigned (char->integer arg)))]
   [(eq? arg #f) "#$00"]
   [(eq? arg #t) "#$ff"]
   [(literal-address? arg) (format "$~x" (literal-address-number arg))]
   [else (error (format "ERROR arg->str: ~a" arg))]))

;;----------------------------------------------------------------

(define (process-set-pointer! context-place context-value)
  (emit-context)
  (let ((place (syntax->datum context-place))
        (value (syntax->datum context-value)))
   (emit 'lda (format "#<~a" (normalize-name value)))
   (emit 'sta (normalize-name place))
   (emit 'lda (format "#>~a" (normalize-name value)))
   (emit 'sta (format "~a+1" (normalize-name place)))))

(define (process-load-pointer context-pointer context-index)
  (emit-context)
  (let ((pointer (syntax->datum context-pointer)))
    (if context-index
        (begin (process-argument context-index #:skip-context #t)
               (emit 'tay)
               (emit 'lda (format "(~a),y" (normalize-name pointer))))
        (begin (emit 'ldy "#0")
               (emit 'lda (format "(~a),y" (normalize-name pointer)))))))

(define (process-memset context-address context-value optional-limit)
  (let ((address (syntax->datum context-address))
        (limit (if optional-limit
                   (syntax->datum optional-limit) #f)))
   (process-argument context-value)
   (emit 'ldy "#$00")
   (emit "-" 'sta (format "~a,y" (normalize-name address)))
   (emit 'iny)
   (when limit
         ; TODO: Currently only supports numbers for `limit`.
         (emit 'cpy (arg->str limit)))
   (emit 'bne "-")))

(define (process-memcpy context-address context-source context-limit)
  (let ((address (syntax->datum context-address))
        (source (syntax->datum context-source))
        (limit (syntax->datum context-limit)))
    ; TODO: This is really bad. Needs to be better generalized.
    (process-argument context-source #:as 16)
    (emit 'sty "_pointer")
    (emit 'sta "_pointer+1")
    (emit 'ldy "#$00")
    (emit "-" 'lda "(_pointer),y")
    (emit 'sta (format "~a,y" (normalize-name address)))
    (emit 'iny)
    (emit 'cpy (arg->str limit))
    (emit 'bne "-")))

(define *need-ppu-load-functions* #f)

(define (process-ppu-load context-ppu-addr context-address context-num)
  (let ((ppu-addr (syntax->datum context-ppu-addr))
        (address (syntax->datum context-address))
        (num (syntax->datum context-num)))
    (set! *need-ppu-load-functions* #t)
    (emit-context)
    (emit 'bit "REG_PPU_STATUS")
    (emit 'lda (format "#>~a" (arg->str ppu-addr)))
    (emit 'sta "REG_PPU_ADDR")
    (emit 'lda (format "#<~a" (arg->str ppu-addr)))
    (emit 'sta "REG_PPU_ADDR")
    (cond
     [(and (number? address) (< num #x100))
      (begin (emit 'lda (arg->str address))
             (emit 'ldy (format "#~a" (- #x100 num)))
             (emit 'ldx "#1")
             (emit 'jsr "_ppu_load_by_val"))]
     [(number? address)
      (begin (emit 'lda (arg->str address))
             (emit 'ldy "#0")
             (emit 'ldx (format "#>~a" (arg->str num)))
             (emit 'jsr "_ppu_load_by_val"))]
     [(< num #x100)
      (begin (emit 'lda (format "#<(~a+~a)" (arg->str address) num))
             (emit 'sta "_pointer+0")
             (emit 'lda (format "#>(~a+~a-$100)" (arg->str address) num))
             (emit 'sta "_pointer+1")
             (emit 'ldy (format "#~a" (- #x100 num)))
             (emit 'ldx "#1")
             (emit 'jsr "_ppu_load_by_pointer"))]
     [else
      (begin (emit 'lda (format "#<~a" (arg->str address)))
             (emit 'sta "_pointer+0")
             (emit 'lda (format "#>~a" (arg->str address)))
             (emit 'sta "_pointer+1")
             (emit 'ldy "#0")
             (emit 'ldx (format "#>~a" (arg->str num)))
             (emit 'jsr "_ppu_load_by_pointer"))])))

(define (process-ppu-memset context-ppu-base context-dest-high
                            context-dest-low context-length context-value)
  (let ((ppu-base (syntax->datum context-ppu-base)))
   (emit-context)
   (process-argument context-dest-high)
   (emit 'clc)
   (emit 'adc (format "#>~a" (normalize-name ppu-base)))
   (emit 'sta "REG_PPU_ADDR")
   (process-argument context-dest-low)
   (emit 'clc)
   (emit 'adc (format "#<~a" (normalize-name ppu-base)))
   (emit 'sta "REG_PPU_ADDR")
   (process-argument context-value)
   (emit 'pha)
   (process-argument context-length)
   (emit 'tax)
   (emit 'pla)
   (emit "-" 'sta "REG_PPU_DATA")
   (emit 'dex)
   (emit 'bne "-")
   ;; reset ppu addr
   (emit 'lda "#$00")
   (emit 'sta "REG_PPU_ADDR")
   (emit 'lda "#$00")
   (emit 'sta "REG_PPU_ADDR")))

(define (process-ppu-memcpy context-ppu-base context-dest-high
                            context-dest-low context-end-index
                            context-prg-base context-start-index)
  (let ((ppu-base (syntax->datum context-ppu-base))
        (end-index (syntax->datum context-end-index))
        (prg-base (syntax->datum context-prg-base))
        (start-index (syntax->datum context-start-index)))
   (emit-context)
   (process-argument context-dest-high)
   (emit 'clc)
   (emit 'adc (format "#>~a" (normalize-name ppu-base)))
   (emit 'sta "REG_PPU_ADDR")
   (process-argument context-dest-low)
   (emit 'clc)
   (emit 'adc (format "#<~a" (normalize-name ppu-base)))
   (emit 'sta "REG_PPU_ADDR")
   (emit 'ldy (arg->str start-index))
   (emit "-" 'lda (format "~a,y" (arg->str prg-base)))
   (emit 'sta "REG_PPU_DATA")
   (emit 'iny)
   (emit 'cpy (arg->str end-index))
   (emit 'bne "-")
   ;; reset ppu addr
   (emit 'lda "#$00")
   (emit 'sta "REG_PPU_ADDR")
   (emit 'lda "#$00")
   (emit 'sta "REG_PPU_ADDR")))

(define (process-ppu-memcpy16 context-ppu-base context-dest-high
                              context-dest-low context-length
                              context-pointer)
  (let ((ppu-base (syntax->datum context-ppu-base))
        (pointer (syntax->datum context-pointer))
        (length (syntax->datum context-length)))
   (emit-context)
   (process-argument context-dest-high)
   (emit 'clc)
   (emit 'adc (format "#>~a" (normalize-name ppu-base)))
   (emit 'sta "REG_PPU_ADDR")
   (process-argument context-dest-low)
   (emit 'clc)
   (emit 'adc (format "#<~a" (normalize-name ppu-base)))
   (emit 'sta "REG_PPU_ADDR")
   (emit 'ldy "#0")
   (emit "-" 'lda (format "(~a),y" (normalize-name pointer)))
   (emit 'sta "REG_PPU_DATA")
   (emit 'iny)
   (emit 'cpy (arg->str length))
   (emit 'bne "-")
   ;; reset ppu addr
   (emit 'lda "#$00")
   (emit 'sta "REG_PPU_ADDR")
   (emit 'lda "#$00")
   (emit 'sta "REG_PPU_ADDR")))

(define (process-set-sprite! context-sprite-id context-value context-field)
  (let ((field (syntax->datum context-field)))
   (process-argument context-value)
   (emit 'pha)
   (process-argument context-sprite-id)
   (emit 'asl) ;; *2
   (emit 'asl) ;; *4
   (emit 'tax)
   (emit 'pla)
   (emit 'sta (format "$~x,x" (+ field #x200)))))

(define (process-get-sprite context-sprite-id context-field)
  (let ((field (syntax->datum context-field)))
   (process-argument context-sprite-id)
   (emit 'asl) ;; *2
   (emit 'asl) ;; *4
   (emit 'tax)
   (emit 'lda (format "$~x,x" (+ field #x200)))))

(define (process-sprites-apply-to-field! context-sprite-id context-num-sprites
                                         context-attr-bits field instr)
  (let ((label (generate-label "sprites_apply")))
     (process-argument context-attr-bits)
     (emit 'pha)
     (process-argument context-num-sprites)
     (emit 'pha)
     (process-argument context-sprite-id)
     (emit 'asl) ;; *2
     (emit 'asl) ;; *4
     (emit 'clc)
     (emit 'adc (arg->str field)) ;; field
     (emit 'tax) ;; put offset in x
     (emit 'pla) ;; pull count out
     (emit 'tay) ;; put sprite count in y
     (emit 'pla) ;; value
     (emit 'sta "_tmp")
     (emit-label label)
     (emit 'lda "$200,x") ;; load previous
     (cond
      [(eq? instr 'adc) (emit 'clc)]
      [(eq? instr 'sbc) (emit 'sec)])
     (emit instr "_tmp")
     (emit 'sta "$200,x")
     (emit 'inx) ;; skip
     (emit 'inx) ;; to
     (emit 'inx) ;; the next
     (emit 'inx) ;; sprite
     (emit 'dey)
     (emit 'bne label)))

(define (process-underscore-rnd)
  (let ((label (generate-label "rnd"))
        (okay (generate-label "okay")))
     (emit 'lda (arg->str rnd-reg))
     (emit 'bne okay)
     (emit 'lda "#$c7")
     (emit-label okay)
     (emit 'asl)
     (emit 'bcc label)
     (emit 'eor "#$1d")
     (emit-label label)
     (emit 'sta (arg->str rnd-reg))))

(define (power-of-2? num)
  (eq? (bitwise-and num (- num 1)) 0))

(define (log-base-2 num)
  (inexact->exact (/ (log num) (log 2))))

(define (process-mul-by-10 context-left)
  (process-argument context-left)
  (emit 'asl "a")
  (emit 'sta "_count")
  (emit 'asl "a")
  (emit 'asl "a")
  (emit 'clc)
  (emit 'adc "_count"))

(define (process-mul-left-shift context-left num-right)
  (process-argument context-left)
  (for [(n num-right)]
       (emit 'asl "a")))

(define (process-div-right-shift context-left num-right)
  (process-argument context-left)
  (for [(n num-right)]
       (emit 'lsr "a")))

(define (process-mod-bitmask context-left num-right)
  (process-argument context-left)
  (emit 'and (format "#~a" num-right)))

(define (process-mul context-left context-right)
  (let ((right (syntax->datum context-right)))
    (if (and (number? right) (power-of-2? right))
        (process-mul-left-shift context-left (log-base-2 right))
        (if (and (number? right) (= right 10))
            (process-mul-by-10 context-left)
            (let ((start-label (generate-label "mul_start"))
                  (loop-label (generate-label "mul_loop"))
                  (inc-label (generate-label "mul_inc"))
                  (done-label (generate-label "mul_done")))
              (emit-context)
              (process-argument context-right #:skip-context #t)
              (emit 'bne start-label)
              (emit 'lda "#0")
              (emit 'jmp done-label)
              (emit-label start-label)
              (emit 'sta "_count")
              (process-argument context-left #:skip-context #t)
              (emit 'sta "_tmp")
              (emit 'ldy "#8")
              (emit 'lda "#0")
              (emit-label loop-label)
              (emit 'asl "a")
              (emit 'asl "_count")
              (emit 'bcc inc-label)
              (emit 'clc)
              (emit 'adc "_tmp")
              (emit-label inc-label)
              (emit 'dey)
              (emit 'bne loop-label)
              (emit-label done-label))))))

(define (process-div symbol context-left context-right)
  (let ((right (syntax->datum context-right)))
    (if (and (number? right) (power-of-2? right))
        (if (eq? symbol '/)
            (process-div-right-shift context-left (log-base-2 right))
            (process-mod-bitmask context-left (- right 1)))
        (add-error "Cannot divide by non-power-of-2" right))))

(define (process-scale16 context-base context-offset context-scale)
  (let ((base (syntax->datum context-base))
        (offset (syntax->datum context-offset))
        (scale (syntax->datum context-scale)))
    (when (not (eq? scale #x20))
          (error "Only scale of #x20 allowed"))
    (emit-context)
    (emit 'lda "#0")
    (emit 'sta "_tmp")
    (emit 'lda (arg->str offset))
    (emit 'asl "a")
    (emit 'rol "_tmp")
    (emit 'asl "a")
    (emit 'rol "_tmp")
    (emit 'asl "a")
    (emit 'rol "_tmp")
    (emit 'asl "a")
    (emit 'rol "_tmp")
    (emit 'asl "a")
    (emit 'rol "_tmp")
    (emit 'clc)
    (emit 'adc (format "#<~a" (normalize-name base)))
    (emit 'tay)
    (emit 'lda "_tmp")
    (emit 'adc (format "#>~a" (normalize-name base)))))

;; add two 8 bit numbers to a 16 bit one
(define (process-add16 context-place context-high context-low)
  (let ((place (syntax->datum context-place)))
   (process-argument context-high)
   (emit 'pha)
   (process-argument context-low #:skip-context #t)
   (emit 'sta "_tmp")
   (emit 'clc)
   (emit 'lda (normalize-name place))
   (emit 'adc "_tmp")
   (emit 'sta (normalize-name place))
   (emit 'pla)
   (emit 'sta "_tmp")
   (emit 'lda (format "~a+1" (normalize-name place)))
   (emit 'adc "_tmp")
   (emit 'sta (format "~a+1" (normalize-name place)))))

;; subtract two 8 bit numbers to a 16 bit one
(define (process-sub16 context-place context-high context-low)
  (let ((place (syntax->datum context-place)))
   (process-argument context-high)
   (emit 'pha)
   (process-argument context-low #:skip-context #t)
   (emit 'sta "_tmp")
   (emit 'sec)
   (emit 'lda (normalize-name place))
   (emit 'sbc "_tmp")
   (emit 'sta (normalize-name place))
   (emit 'pla)
   (emit 'sta "_tmp")
   (emit 'lda (format "~a+1" (normalize-name place)))
   (emit 'sbc "_tmp")
   (emit 'sta (format "~a+1" (normalize-name place)))))

;----------------------------------------------------------------

(define (m-expand-cond-to-if x)
  (define (_ l)
    (cond
      ((null? l) 0)
      ((eq? (macro-expand (caar l)) 'else) (cons 'do
                                                 (macro-expand (cdr (car l)))))
      [else (list 'if (macro-expand (caar l))
                  (cons 'do (macro-expand (cdr (car l))))
                  (_ (cdr l)))]))
  (_ (cdr x)))

(define (m-expand-when-to-if x)
  (list 'if (cadr x) (append (list 'do) (cddr x)) 0))

;; (get-sprite-vflip sprite-num)
(define (m-expand-get-sprite-vflip x)
  (list '>> (list 'get-sprite-attr (cadr x)) 7))

(define (m-expand-get-sprite-hflip x)
  (list 'and (list '>> (list 'get-sprite-attr (cadr x)) 6) #x01))

(define (m-expand-set-sprite-vflip! x)
  (list 'set-sprite-attr! (list-ref x 1) (list '<< (list-ref x 2) 6)))

(define (m-expand-set-sprite-hflip! x)
  (list 'set-sprite-attr! (list-ref x 1) (list '<< (list-ref x 2) 7)))

(define (m-expand-set-sprite! x f)
  (list 'set-sprite! (list-ref x 1) (list-ref x 2) f))

(define (m-expand-get-sprite x f)
  (list 'get-sprite (list-ref x 1) f))

;; basically diy-macro from the main tinyscheme stuff
(define (macro-expand s)
  (cond
   [(null? s) s]
   [(list? s)
    (map
     (lambda (i)
       (if (and (list? i) (not (null? i)))
           ;; dispatch to macro processors
           (cond
            [(eq? (car i) 'cond) (m-expand-cond-to-if i)]
            [(eq? (car i) 'when) (m-expand-when-to-if i)]
            [(eq? (car i) 'get-sprite-vflip) (m-expand-get-sprite-vflip i)]
            [(eq? (car i) 'get-sprite-hflip) (m-expand-get-sprite-hflip i)]
            [(eq? (car i) 'set-sprite-vflip!) (m-expand-set-sprite-vflip! i)]
            [(eq? (car i) 'set-sprite-hflip!) (m-expand-set-sprite-hflip! i)]
            [(eq? (car i) 'get-sprite-y) (m-expand-get-sprite i 0)]
            [(eq? (car i) 'get-sprite-id) (m-expand-get-sprite i 1)]
            [(eq? (car i) 'get-sprite-attr) (m-expand-get-sprite i 2)]
            [(eq? (car i) 'get-sprite-x) (m-expand-get-sprite i 3)]
            [(eq? (car i) 'set-sprite-y!) (m-expand-set-sprite! i 0)]
            [(eq? (car i) 'set-sprite-id!) (m-expand-set-sprite! i 1)]
            [(eq? (car i) 'set-sprite-attr!) (m-expand-set-sprite! i 2)]
            [(eq? (car i) 'set-sprite-x!) (m-expand-set-sprite! i 3)]
            [else (macro-expand i)])
           (macro-expand i)))
     s)]
   [else s]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Utilities

(define (assert val fn)
  (when (not (fn val))
    (error (format "assert failed: ~a ~a" val fn))))

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
  (string-replace (string-replace (symbol->string name) "-" "_") "!" "_ex_"))

(define (atom? obj)
  (and (not (null? obj))
       (not (pair? obj))))

(define (list->byte-string ls)
  (string-join (map (lambda (x)
                      (string-append "$" (left-pad (format "~x" x) #\0 2))) ls)
               ","))

(define (string-last text)
  (substring text (- (string-length text) 1)))

(define (register? text)
  (when (symbol? text)
        (set! text (symbol->string text)))
  (or (string=? text "a") (string=? text "x") (string=? text "y")))

(define (immediate? obj)
  (or (number? obj)
      (and (symbol? obj)
           (let ((lookup (sym-label-lookup obj)))
             (and lookup (eq? (sym-label-kind lookup) 'const))))))

(define (every-first-in-tree? pred tree)
  (cond
   [(not tree) #t]
   [(null? tree) #t]
   [(not (list? tree)) #t]
   [(pred (car tree))
      (for/and ([item tree])
               (every-first-in-tree? pred item))]
   [else #f]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Source file context information for debugging / compiler metadata

(define *co2-source-context* (make-parameter #f))

(define (co2-source-context)
  (let* ((form (vector-ref (*co2-source-context*) 0))
         (fname (syntax-source form))
         (line-num (syntax-line form))
         (source (->string (syntax->datum form)))
         (len (min 40 (string-length source))))
    (if fname
        (format ";~a:~a ~a" fname line-num (substring source 0 len))
        #f)))

(define (co2-source-err-context)
  (let* ((form (vector-ref (*co2-source-context*) 0))
         (fname (syntax-source form))
         (line-num (syntax-line form)))
    (if fname
        (format "~a:~a" fname line-num)
        #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Built-in functions

(define (built-in-nes-header num-prg num-chr mapper mirroring)
  (let* ((mbit (cond
                [(eq? mirroring #f) 0]
                [(eq? mirroring 'horizontal) 0]
                [(eq? mirroring 'vertical) 1]
                [(eq? mirroring 'quote)
                 (begin (add-error "Do not quote mirroring" 'quote) 0)]
                [else
                 (begin (add-error "Unknown mirroring" mirroring) 0)]))
         (third-byte (+ (* mapper #x10) mbit)))
    (emit-context)
    (emit ".byte \"NES\",$1a")
    (emit (format ".byte $~x" (or num-prg 1)))
    (emit (format ".byte $~x" (or num-chr 0)))
    (emit (format ".byte $~x" third-byte))
    (emit (format ".byte ~a"
                  (string-join (build-list 9 (lambda (x) "$0")) ",")))
    (emit "")
    (when (not *user-specified-org*)
      (emit ".org $c000")
      (emit ""))))

(define (built-in-init-system)
  ;; disable interrupts while we set stuff up
  (emit 'sei)
  ;; make sure we're not using decimal mode
  (emit 'cld)
  ;; wait for 2 vblanks
  (emit "-" 'lda "$2002")
  (emit 'bpl "-")
  (emit "-" 'lda "$2002")
  (emit 'bpl "-")
  ;; clear out all ram
  (emit 'ldx "#$00")
  (emit "-" 'lda "#$00")
  (emit 'sta "$000,x")
  (emit 'sta "$100,x")
  (emit 'sta "$300,x")
  (emit 'sta "$400,x")
  (emit 'sta "$500,x")
  (emit 'sta "$600,x")
  (emit 'sta "$700,x")
  (emit 'lda "#$ff")
  (emit 'sta "$200,x")
  (emit 'inx)
  (emit 'bne "-")
  ;; reset the stack pointer.
  (emit 'ldx "#$ff")
  (emit 'txs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Keep track of function calls to build a call tree. Used to determine
; memory addresses for local vars and function arguments.

(define *invocations* (make-parameter #f))

(define *entry-points* '())

(define *local-vars* (make-parameter #f))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *scope-name* (make-parameter #f))

(define (current-scope-name)
  (or (*scope-name*) '_global))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Optimization setting.

(define *opt-enabled* #f)

(define *opt-mode* (make-parameter #f))

(define (set-optimization! opt)
  (set! *opt-enabled* opt))

(define (optimization-enabled? kind)
  (and *opt-enabled* (eq? kind 'if)))

(define (optimization-using-mode? kind)
  (and *opt-enabled* (vector? (*opt-mode*))
       (eq? kind (vector-ref (*opt-mode*) 0))))

(define (optimization-successful?)
  (and *opt-enabled* (vector? (*opt-mode*))
       (or (vector-ref (*opt-mode*) 1) (vector-ref (*opt-mode*) 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Main processor.

(define (can-fold-const? instr accum arg)
  (and (immediate? arg)
       (or (eq? instr 'or) (eq? instr 'ora))))

(define (do-fold-const instr accum arg)
  (if (not accum)
      (arg->str arg)
      (cond
       [(or (eq? instr 'or) (eq? instr 'ora))
          (format "~a|~a" accum (arg->str arg))])))

(define (process-defvar name [value 0])
  (let* ((def (normalize-name name))
         (sym-label (make-variable! name #:global #t))
         (addr (sym-label-address sym-label)))
    (emit-blank)
    (emit-context)
    (emit (format "~a = $~a" def (left-pad (number->string addr 16) #\0 2)))
    (when (not (= value 0))
          (emit 'lda (format "#$~x" value))
          (emit 'sta def))))

(define (process-defaddr name value)
  (let* ((def (normalize-name name))
         (sym-label (make-address! name value))
         (value (sym-label-address sym-label)))
    (emit-blank)
    (emit-context)
    (emit (format "~a = $~a" def (left-pad (number->string value 16) #\0 2)))))

(define (process-defconst name value)
  (let* ((def (normalize-name name))
         (sym-label (make-const! name value))
         (value (sym-label-address sym-label)))
    (emit-blank)
    (emit-context)
    (emit (format "~a = $~a" def (left-pad (number->string value 16) #\0 2)))))

(define (process-deflabel context-name)
  (let* ((name (syntax->datum context-name))
         (def (normalize-name name)))
    (emit-blank)
    (emit-context)
    (emit (format "~a:" def))))

(define (process-defbuffer name length)
  (let* ((def (normalize-name name))
         (sym-label (make-buffer! name length))
         (addr (sym-label-address sym-label)))
    (emit-blank)
    (emit-context)
    (emit (format "~a = $~a" def (left-pad (number->string addr 16) #\0 2)))))

(define (process-program-begin context-address)
  (let* ((address (syntax->datum context-address)))
    (emit (format ".org $~x" address))))

(define (process-program-complete)
  (generate-suffix))

(define (process-proc type context-decl body)
  (assert type symbol?)
  (assert context-decl syntax?)
  (assert body list?)
  (let* ((decl (syntax->datum context-decl))
         (name (car decl))
         (args (cdr decl)))
    ; Add vector entry points.
    (when (eq? type 'vector)
          (set! *entry-points* (cons name *entry-points*)))
    (parameterize [(*invocations* (make-gvector)) (*local-vars* (make-gvector))]
      (emit-blank)
      (emit-context)
      (emit-label (normalize-name name))
      ; Push scope for local variables.
      (sym-label-push-scope)
      ; Fastcall parameters
      (for [(sym args) (i (in-naturals))]
           (make-local! sym name)
           (cond
            [(= i 0) (emit 'sta (arg->str sym))]
            [(= i 1) (emit 'stx (arg->str sym))]
            [(= i 2) (emit 'sty (arg->str sym))]))
      ; Additional parameters
      (when (> (length args) 3)
            (emit 'tsx)
            (let ((params (cdddr args)))
              (for [(p params) (i (in-naturals))]
                   (emit 'lda (format "$~x,x" (+ #x103 i)))
                   (emit 'sta (arg->str p)))))
      ; Set scope name.
      (parameterize [(*scope-name* name)]
        ; Process body.
        (for [(stmt body)]
             (process-form stmt)))
      ; Pop scope.
      (sym-label-pop-scope)
      ; Return from function.
      (cond
       [(eq? type 'sub) (emit 'rts)]
       [(eq? type 'vector) (emit 'rti)])
      ; Store inner function calls for building call tree.
      (let ((calls (gvector->list (*invocations*)))
            (vars (gvector->list (*local-vars*))))
        (make-func-node! name (append args vars) calls)))))

(define (process-set-bang target expr)
  (assert target syntax?)
  (assert expr syntax?)
  (let ((place (syntax->datum target)))
    (if (immediate? place)
        (add-error "Cannot assign to" place)
        (begin
          (process-argument expr)
          (emit 'sta (arg->str place))))))

(define (process-cond-or-expression instr args)
  (if (or (not (optimization-using-mode? 'if))
          (> (length args) 2))
      ; Not in a conditional, process as a normal instruction.
      (parameterize [(*opt-mode* #f)]
        (process-instruction-transitive instr args))
      ; TODO: More than 2 arguments.
      (let ((context-first (car args))
            (context-second (cadr args)))
        (process-argument context-first)
        (if (not (optimization-successful?))
            ; No optimizations within arguments, just output normal instruction.
            (let ((rhs (process-argument (cadr args) #:preserve '(a) #:as 'rhs
                                         #:skip-context #t)))
              (emit instr (arg->str rhs))
              ; TODO: vector-set! (*opt-mode*) ???
              )
            ;
            (let* ((optimization-result (*opt-mode*))
                   (truth-inner (vector-ref optimization-result 1))
                   (false-inner (vector-ref optimization-result 2))
                   (false-outer (vector-ref optimization-result 3)))
              ; TODO: Handle cases where optimized code has a false-inner.
              (assert false-inner not)
              (emit 'jmp false-outer)
              (emit-label truth-inner)
              (process-argument context-second))))))

; Used for instructions that can "chain" arguments in such a way that they can
; implemented by running that instruction on each argument after the 1st.
; For example:
;  (and #xfe a b c)
;  > lda #fe
;  > and a
;  > and b
;  > and c
(define (process-instruction-transitive instr args)
  (assert instr symbol?)
  (assert args list?)
  (let ((first (datum-ref args 0))
        (second (datum-ref args 1))
        (const #f))
    (cond
     ; Single argument to instruction just outputs the instruction.
     [(= (length args) 1)
      (begin
        (emit-context)
        (emit instr (arg->str first)))]
     ; Single argument with an index register.
     ; TODO: Should validate that the instruction can use indexed addressing.
     [(and (= (length args) 2) (index-register? second))
      (begin
        (emit-context)
        (emit instr (format "~a,~a" (arg->str first) (->register second))))]
     ; Two arguments, evaluate any expressions, load the first arg into A,
     ; and output the instruction using the second arg as the operand.
     [(= (length args) 2)
      (begin
        (let* ((lhs (process-argument (car args)))
               (rhs (process-argument (cadr args) #:preserve '(a) #:as 'rhs
                                      #:skip-context #t)))
          (emit instr (arg->str rhs))))]
     ; More than two arguments. Load first, generate code for others, folding
     ; constants along the way.
     [(> (length args) 2)
      (begin
        (process-argument (car args))
        (for [(elem (cdr args))]
             (let ((e (syntax->datum elem)))
               (if (can-fold-const? instr const e)
                   ; Fold constant
                   (set! const (do-fold-const instr const e))
                   ; Flush constant
                   (begin
                     (when const
                           (emit instr const)
                           (set! const #f))
                     (let ((val (process-argument elem #:preserve '(a) #:as 'rhs
                                                  #:skip-context #t)))
                       (emit instr val))))))
        ; Flush any remaining constants.
        (when const
              (emit instr const)
              (set! const #f)))]
     [else (add-error (format "Invalid arguments to \"~a\":" instr) args)])))

; Use for load or store instructions.
(define (process-instruction-mov need-lvalue? instr args)
  (assert instr symbol?)
  (assert args list?)
  (let ((first (datum-ref args 0))
        (second (datum-ref args 1)))
    (cond
     [(= (length args) 0)
        (add-error (format "Invalid arguments to \"~a\":" instr) args)]
     [(and need-lvalue? (not (lvalue? first)))
        (add-error "Not a valid lvalue" first)]
     [(and (= (length args) 2) (index-register? second))
      (begin
        (emit-context)
        (emit instr (format "~a,~a" (arg->str first) (->register second))))]
     [(> (length args) 1)
      (add-error (format "Only one argument allowed for ~a, got" instr) args)]
     [else
      (emit instr (arg->str first))])))

(define (process-instruction-accumulator instr context-arg)
  (assert instr symbol?)
  (assert context-arg syntax?)
  (let ((arg (syntax->datum context-arg)))
    (if (eq? arg 'a)
        (emit instr "a")
        (let ((value (process-argument context-arg #:as 'rhs)))
          (emit instr value)))))

;Generate code to get a single value into the desired return value, which
; defaults to the accumulator. Preserve registers to the stack if needed.
; Output context if loading an atomic value, unless context should be skipped.
(define (process-argument context-arg #:preserve [preserve #f] #:as [as #f]
                          #:skip-context [skip-context #f])
  (let ((ret "a")
        (arg (syntax->datum context-arg)))
    (if (list? arg)
        ; Evaluate the expression, preserving registers if needed.
        (parameterize [(*co2-source-context* (vector context-arg #t))]
          (when preserve
                (process-stack 'push preserve #:skip-context #t))
          (process-form context-arg)
          (when preserve
                (set! ret "_tmp")
                (emit 'sta ret)
                (process-stack 'pull preserve #:skip-context #t)))
        ; Load an atomic value.
        (let ((val (arg->str arg)))
          (when (and (not skip-context) (vector-ref (*co2-source-context*) 1))
                (emit-context)
                (vector-set! (*co2-source-context*) 1 #f))
          (cond
           [(not as) (emit 'lda val)]
           [(eq? as 'rhs) (set! ret val)]
           [(eq? as 16) (begin (emit 'ldy (format "#<~a" val))
                               (emit 'lda (format "#>~a" val)))]
           [else (begin (emit as val)
                        (set! ret (string-last (symbol->string as))))])))
    (when (and (not skip-context) (vector-ref (*co2-source-context*) 1))
          (emit-context)
          (vector-set! (*co2-source-context*) 1 #f))
    (when (and (not (eq? as 'rhs)) (not (register? ret)))
          (emit 'lda ret))
    ret))

(define (process-operand context-operand)
  (emit-context)
  (let ((operand (syntax->datum context-operand)))
    (cond
     [(and (list? operand) (eq? (car operand) 'high))
        ; TODO: Check type of operand, make sure it's a pointer.
        (format "~a+1" (arg->str (cadr operand)))]
     [(symbol? operand)
        (arg->str operand)]
     [else
        (add-error "ERROR NOT IMPLEMENTED:" operand)])))

(define (process-instruction-standalone instr operand)
  (assert instr symbol?)
  (assert operand syntax?)
  (let ((val (process-operand operand)))
    (emit instr val)))

(define (process-instruction-branch instr target)
  (assert instr symbol?)
  (assert target syntax?)
  (let* ((value (syntax->datum target))
         (lookup (sym-label-lookup value)))
    (when (not (and lookup (eq? (sym-label-kind lookup) 'label)))
          (error (format "Invalid target ~a" value)))
    (emit-context)
    (emit instr (format "~a" (sym-label-address lookup)))))

(define (process-instruction-implied instr)
  (assert instr symbol?)
  (emit-context)
  (emit instr))

(define (process-block context-label body)
  (assert context-label syntax?)
  (assert body list?)
  (let* ((label (syntax->datum context-label))
         (start-label (generate-label (symbol->string label)))
         (break-label (generate-label (symbol->string label))))
    ; Label for the start of the block.
    (emit-label start-label)
    ; Push scope for local label.
    (sym-label-push-scope)
    (make-label! label start-label)
    (make-label! '#:break break-label)
    ; Process body.
    (for [(stmt body)]
         (process-form stmt))
    ; Pop scope.
    (sym-label-pop-scope)
    ; Label that #:break goes to.
    (emit-label break-label)))

(define (process-bytes args)
  (let ((build (make-gvector)))
    (define (add-elem-to-vector elem)
      (cond
       [(number? elem) (gvector-add! build (format "$~x" elem))]
       [(string? elem) (gvector-add! build (format "~s" elem))]
       [(symbol? elem)
        (let ((lookup (sym-label-lookup elem))
              (normal (normalize-name elem)))
          (cond
           [(const? elem) (gvector-add! build normal)]
           [(address? elem) (begin
                              (gvector-add! build (format "<~a" normal))
                              (gvector-add! build (format ">~a" normal)))]
           [else (add-error "Cannot output bytes" elem)]))]
       [(list? elem) (for [(item elem)]
                          (add-elem-to-vector item))]
       [else (add-error "Cannot output bytes" elem)]))
    (for [(context-elem args)]
         (let ((elem (syntax->datum context-elem)))
           (add-elem-to-vector elem)))
    (emit (string-append ".byte " (string-join (gvector->list build) ",")))))

(define (process-include-binary context-label context-path optional-key
                                optional-size)
  (let* ((label (syntax->datum context-label))
         (name (normalize-name label))
         (path (syntax->datum context-path))
         (size (if optional-size
                   (syntax->datum optional-size) #f)))
    (emit-blank)
    (emit-context)
    (emit (format "~a:" name))
    (if size
        (emit (format ".incbin \"~a\",0,$~x" path size))
        (emit (format ".incbin \"~a\"" path)))))

(define (process-loop-down context-reg context-start body)
  (assert context-reg syntax?)
  (assert context-start syntax?)
  (assert body list?)
  (emit-context)
  (let ((iter (syntax->datum context-reg))
        (start (syntax->datum context-start))
        (initial-loop-value #f))
    (cond
     [(= start 0) (error "Cannot start loop at 0")]
     [(< start #x100) (set! initial-loop-value start)]
     [(= start #x100) (set! initial-loop-value 0)]
     [else (error "Initial loop value invalid")])
    ; Push scope.
    (sym-label-push-scope)
    ; Define the local var, or use a register.
    (if (register? iter)
        (emit (format "  ld~a #~a" iter initial-loop-value))
        (begin
          (make-local! iter (current-scope-name))
          (when (*local-vars*)
                (gvector-add! (*local-vars*) iter))
          (emit 'lda (format "#~a" initial-loop-value))
          (emit 'sta (arg->str iter))))
    (let ((loop-label (generate-label "loop_down_from")))
      (emit-label loop-label)
      ; TODO: Disallow `reg` changes within `body`
      (for [(stmt body)]
           (process-form stmt))
      (emit-context)
      (if (register? iter)
          (emit (format "  de~a" iter))
          (emit 'dec (arg->str iter)))
      (emit 'bne loop-label))
    ; Pop scope.
    (sym-label-pop-scope)))

(define (process-loop-up context-iter context-start context-end body
                         #:inclusive [inclusive #f])
  (assert context-iter syntax?)
  (assert context-start syntax?)
  (assert context-end syntax?)
  (assert body list?)
  (emit-context)
  ; Push scope.
  (sym-label-push-scope)
  ;
  (let* ((iter (syntax->datum context-iter))
         (start (syntax->datum context-start))
         (end (syntax->datum context-end))
         (initial-loop-value start)
         (sentinal-value (make-local-gensym! (current-scope-name)))
         (loop-label (generate-label "loop_up_to")))
    ; Store the sentinal value into memory.
    (if (and (list? end) (eq? (car end) 'length))
        (emit 'lda (format "#~a_~a"
                           (normalize-name (cadr end))
                           (normalize-name (car end))))
        (emit 'lda (arg->str end)))
    (emit 'sta sentinal-value)
    ; Setup iteration value.
    (if (register? iter)
        (emit (format "  ld~a #~a" iter initial-loop-value))
        (begin (make-local! iter (current-scope-name))
               (when (*local-vars*)
                     (gvector-add! (*local-vars*) iter))
               (emit 'lda (arg->str initial-loop-value))
               (emit 'sta (arg->str iter))))
    (emit-label loop-label)
    ; TODO: Disallow `reg` changes within `body`
    (for [(stmt body)]
         (process-form stmt))
    (emit-context)
    ; Increment and compare to sentinal value.
    (if inclusive
         (if (register? iter)
             (begin (emit (format "  t~aa" iter))
                    (emit (format "  in~a" iter))
                    (emit (format "  cmp ~a" sentinal-value)))
             (begin (emit 'lda (arg->str iter))
                    (emit 'inc (arg->str iter))
                    (emit 'cmp (arg->str sentinal-value))))
         (if (register? iter)
             (begin (emit (format "  in~a" iter))
                    (emit (format "  cp~a ~a" iter sentinal-value)))
             (begin (emit 'inc (arg->str iter))
                    (emit 'lda (arg->str iter))
                    (emit 'cmp (arg->str sentinal-value)))))
    ; Loop back to start.
    (emit 'bne loop-label)
    ; Pop scope.
    (sym-label-pop-scope)))

(define (process-repeat context-bindings body)
  (let* ((bindings (syntax->datum context-bindings))
         (label (car bindings))
         (target (cadr bindings)))
    (for [(count target)]
         (sym-label-push-scope)
         (make-metavar! label count)
         (for [(stmt body)]
              (process-form stmt))
         (sym-label-pop-scope))))

(define (process-let context-bindings body)
  ; TODO: This c(a|d)*r calls are awful.
  (let* ((bindings (syntax->datum context-bindings))
         (new-syms '()))
    ; Push scope for local variables.
    (sym-label-push-scope)
    ; Define local variables, or data arrays.
    (for [(bind bindings)]
         (let ((label (car bind))
               (value (if (not (null? (cdr bind))) (cadr bind) #f)))
           (cond
            [(and (list? value) (eq? (car value) 'quote))
               (make-data! label (cadr value))]
            [(list? value)
               (add-error "NOT IMPLEMENTED: bind" value)]
            [(number? value)
               (make-local! label (current-scope-name))
               (when (*local-vars*)
                     (gvector-add! (*local-vars*) label))
               (emit 'lda (arg->str value))
               (emit 'sta (arg->str label))]
            [(not value)
               (make-local! label (current-scope-name))
               (when (*local-vars*)
                     (gvector-add! (*local-vars*) label))]
            [else
               (add-error "NOT IMPLEMENTED: bind" value)])))
    ; Process body.
    (for [(stmt body)]
         (process-form stmt))
    ; Pop scope.
    (sym-label-pop-scope)))

(define (is-optimizable? symbol)
  (or (eq? symbol '<) (eq? symbol 'and)))

(define (can-optimize-tree? tree)
  (every-first-in-tree? is-optimizable? tree))

(define (process-if context-condition context-truth context-false)
  (let ((truth-label (generate-label "truth_case"))
        (false-label (generate-label "false_case"))
        (if-done-label (generate-label "if_done"))
        (if-cond (syntax->datum context-condition))
        (optimization-result #f))
    (emit-context)
    ; Check the condition of the `if`, with optimizations if enabled.
    (if (and (optimization-enabled? 'if) (can-optimize-tree? if-cond))
        (parameterize [(*opt-mode* (vector 'if #f #f false-label))]
          (begin
            (process-argument context-condition #:skip-context #t)
            (when (optimization-successful?)
                  (set! optimization-result (*opt-mode*)))))
        (process-argument context-condition #:skip-context #t))
    ;
    (if optimization-result
        (let ((truth-case (vector-ref optimization-result 1))
              (false-case (vector-ref optimization-result 2)))
          ; TODO: Handle cases where optimized code has a false-case.
          (assert false-case not)
          (emit 'jmp false-label)
          (emit-label truth-case))
        (begin
          ; TODO: Optimize to the negative case `beq`, but only if the branch is
          ; within range (< 128 bytes away).
          (emit 'bne truth-label)
          (emit 'jmp false-label)))
    ; Truth case of the `if`.
    (emit-label truth-label)
    (process-argument context-truth #:skip-context #t)
    (emit 'jmp if-done-label)
    ; False case of the `if`.
    (emit-label false-label)
    (process-argument context-false #:skip-context #t)
    (emit-label if-done-label)))

(define (process-while context-condition body)
  (let ((start-label (generate-label "while_start"))
        (body-label (generate-label "while_body"))
        (done-label (generate-label "while_done")))
    (emit-context)
    ; Check the condition of the `while`.
    (emit-label start-label)
    (process-argument context-condition #:skip-context #t)
    (emit 'bne body-label)
    (emit 'jmp done-label)
    ; Truth case of the `while`.
    (emit-label body-label)
    (for [(stmt body)]
         (process-form stmt))
    (emit 'jmp start-label)
    (emit-label done-label)))

(define (process-math operator context-left context-right)
  (assert operator symbol?)
  (assert context-left syntax?)
  (assert context-right syntax?)
  (let* ((lhs (process-argument context-left))
         (rhs (process-argument context-right #:preserve '(a) #:as 'rhs
                                #:skip-context #t))
         (right (syntax->datum context-right)))
    (case operator
      [(+) (begin (emit 'clc)
                  (emit 'adc (arg->str rhs)))]
      [(-) (begin (emit 'sec)
                  (emit 'sbc (arg->str rhs)))]
      [(eq?) (let ((is-label (generate-label "is_eq"))
                   (done-label (generate-label "done_eq")))
               (emit 'cmp (arg->str rhs))
               (emit 'beq is-label)
               ; not equal
               (emit 'lda "#0")
               (emit 'jmp done-label)
               ; is equal
               (emit-label is-label)
               (emit 'lda "#$ff")
               (emit-label done-label))]
      [(>) (let ((not-label (generate-label "not_gt"))
                 (is-label (generate-label "is_gt"))
                 (done-label (generate-label "done_gt")))
             (emit 'cmp (arg->str rhs))
             (emit 'beq not-label)
             (emit 'bcs is-label)
             ; not gt
             (emit-label not-label)
             (emit 'lda "#0")
             (emit 'jmp done-label)
             ; is gt
             (emit-label is-label)
             (emit 'lda "#$ff")
             (emit-label done-label))]
      [(<) (let ((is-label (generate-label "is_lt"))
                 (done-label (generate-label "done_lt")))
             (emit 'cmp (arg->str rhs))
             (emit 'bcc is-label)
             (if (optimization-using-mode? 'if)
                 (begin
                   (vector-set! (*opt-mode*) 1 is-label)
                   (vector-set! (*opt-mode*) 2 #f))
                 (begin
                   ; not lt
                   (emit 'lda "#0")
                   (emit 'jmp done-label)
                   ; is lt
                   (emit-label is-label)
                   (emit 'lda "#$ff")
                   (emit-label done-label))))]
      [(>s) (let ((not-label (generate-label "not_gt"))
                  (is-label (generate-label "is_gt"))
                  (done-label (generate-label "done_gt")))
              (emit 'cmp (arg->str rhs))
              (emit 'beq not-label)
              (emit 'bpl is-label)
              ; not gt
              (emit-label not-label)
              (emit 'lda "#0")
              (emit 'jmp done-label)
              ; is gt
              (emit-label is-label)
              (emit 'lda "#$ff")
              (emit-label done-label))]
      [(<s) (let ((is-label (generate-label "is_lt"))
                  (done-label (generate-label "done_lt")))
              (emit 'cmp (arg->str rhs))
              (emit 'bmi is-label)
              ; not lt
              (emit 'lda "#0")
              (emit 'jmp done-label)
              ; is lt
              (emit-label is-label)
              (emit 'lda "#$ff")
              (emit-label done-label))]
      [(>=) (let ((not-label (generate-label "not_gt"))
                 (is-label (generate-label "is_gt"))
                 (done-label (generate-label "done_gt")))
             (emit 'cmp (arg->str rhs))
             (emit 'bcs is-label)
             ; not gt
             (emit-label not-label)
             (emit 'lda "#0")
             (emit 'jmp done-label)
             ; is gt
             (emit-label is-label)
             (emit 'lda "#$ff")
             (emit-label done-label))]
      [(<=) (let ((is-label (generate-label "is_lt"))
                  (done-label (generate-label "done_lt")))
              (emit 'cmp (arg->str rhs))
              (emit 'beq is-label)
              (emit 'bcc is-label)
              ; not lt
              (emit 'lda "#0")
              (emit 'jmp done-label)
              ; is lt
              (emit-label is-label)
              (emit 'lda "#$ff")
              (emit-label done-label))]
      [(<=s) (let ((is-label (generate-label "is_lt"))
                   (done-label (generate-label "done_lt")))
               (emit 'cmp (arg->str rhs))
               (emit 'beq is-label)
               (emit 'bmi is-label)
               ; not lt
               (emit 'lda "#0")
               (emit 'jmp done-label)
               ; is lt
               (emit-label is-label)
               (emit 'lda "#$ff")
               (emit-label done-label))]
      [(>>) (begin (assert right number?)
                   (for [(i (in-range right))]
                        (emit 'lsr "a")))]
      [(<<) (begin (assert right number?)
                   (for [(i (in-range right))]
                        (emit 'asl "a")))])))

(define (process-not operator context-arg)
  (assert operator symbol?)
  (assert context-arg syntax?)
  (process-argument context-arg)
  (emit 'cmp "#1")
  (emit 'lda "#$ff")
  (emit 'adc "#0"))

(define (process-peek context-address context-index)
  (let ((address (syntax->datum context-address))
        (index (if context-index (syntax->datum context-index) #f)))
    (emit-context)
    (cond
     [(not index) (emit 'lda (arg->str address))]
     [(number? index) (emit 'lda (format "~a+~a" (arg->str address) index))]
     [(eq? index 'x) (emit 'lda (format "~a,x" (arg->str address)))]
     [(eq? index 'y) (emit 'lda (format "~a,y" (arg->str address)))]
     [else (when (string=? (process-argument context-index #:skip-context #t
                                             #:as 'ldy) "a")
                 (emit 'tay))
           (emit 'lda (format "~a,y" (arg->str address)))])))

(define (process-poke! context-address context-arg0 context-arg1)
  (if context-arg1
      (let* ((address (syntax->datum context-address))
             (context-index context-arg0)
             (context-value context-arg1)
             (arg #f))
        (when (string=? (process-argument context-index #:as 'ldy) "a")
              (emit 'tay))
        (process-argument context-value #:preserve '(y))
        (emit 'sta (format "~a,y" (arg->str address))))
      (let* ((address (syntax->datum context-address))
             (context-value context-arg0))
        (process-argument context-value)
        (emit 'sta (arg->str address)))))

(define (process-address-specifier specifier context-address)
  (let ((address (syntax->datum context-address)))
    (if (eq? specifier 'low)
        (emit 'lda (format "#<~a" (normalize-name address)))
        (emit 'lda (format "#>~a" (normalize-name address))))))

(define (process-stack action registers #:skip-context [skip-context #f])
  (assert action symbol?)
  (assert registers list?)
  (when (not skip-context)
        (emit-context))
  (cond
   [(eq? action 'push) (begin (when (member 'a registers)
                                    (emit 'pha))
                              (when (member 'x registers)
                                    (emit 'txa)
                                    (emit 'pha))
                              (when (member 'y registers)
                                    (emit 'tya)
                                    (emit 'pha)))]
   [(eq? action 'pull) (begin (when (member 'y registers)
                                    (emit 'pla)
                                    (emit 'tay))
                              (when (member 'x registers)
                                    (emit 'pla)
                                    (emit 'tax))
                              (when (member 'a registers)
                                    (emit 'pla)))]))

(define (process-raw symbol args)
  (let ((value (syntax->datum (car args))))
    (cond
     [(eq? symbol 'asm) (for [(elem args)] (emit (syntax->datum elem)))]
     [(eq? symbol 'jsr) (emit (format "  jsr ~a" value))]
     [(eq? symbol 'byte) (emit (format ".byte ~a" value))]
     [(eq? symbol 'text) (emit (format ".byte \"~a\"" value))])))

(define (process-jump-subroutine fname params)
  (let* ((pop-count 0))
    (emit-context)
    (when (> (length params) 3)
          (for [(elem (reverse (cdddr params)))]
               (set! pop-count (+ 1 pop-count))
               (process-argument elem #:skip-context #t)
               (emit 'pha)))
    (for [(elem params) (i (in-naturals))]
         (cond
          [(= i 0)
           (process-argument elem #:skip-context #t)]
          [(= i 1)
           (emit 'ldx (process-argument elem #:preserve '(a) #:as 'rhs
                                        #:skip-context #t))]
          [(= i 2)
           (emit 'ldy (process-argument elem #:preserve '(a x) #:as 'rhs
                                        #:skip-context #t))]))
    (emit 'jsr (normalize-name fname))
    (when (> pop-count 0)
          (emit 'sta "_tmp"))
    (for [(i (in-range pop-count))]
         (emit 'pla))
    (when (> pop-count 0)
          (emit 'lda "_tmp"))
    (when (*invocations*)
          (gvector-add! (*invocations*) fname))))

(define (process-invocation context-original symbol rest)
  (cond
   [(null? (syntax->datum context-original)) #f]
   [(not symbol) (process-argument context-original)]
   [(not (symbol? symbol)) (add-error "Invalid function or macro name" symbol)]
   [(function? symbol) (process-jump-subroutine symbol rest)]
   [(macro? symbol) (let* ((forms (list (syntax->datum context-original)))
                           (expanded (car (macro-expand forms)))
                           (wrapped (datum->syntax context-original
                                                   expanded
                                                   context-original)))
                      (process-form wrapped))]))

(define (process-with-args func args num-needed)
  (if (< (length args) num-needed)
      (add-error (format "Need ~a arguments, only got ~a, for"
                         num-needed (length args)) (object-name func))
      (apply func args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Syntax tree walker

(define (process-keys args allowed-keywords)
  (for/list [(k allowed-keywords)]
    (find-keyword k (map syntax->datum args))))

(define (unwrap-args args num-required num-optional)
  ; TODO: Implement num-required and num-optional
  (map syntax-e args))

(define (process-form form)
  (assert form syntax?)
  (let* ((inner (syntax-e form))
         (first (if (and (list? inner) (not (null? inner))) (car inner) #f))
         (rest (if (and (list? inner) (not (null? inner))) (cdr inner) #f))
         (symbol (if first (syntax->datum first) #f)))
    (parameterize [(*co2-source-context* (vector form #t))]
      (if (or (not symbol) (not (symbol? symbol))
              (function? symbol) (macro? symbol))
          (process-invocation form symbol rest)
          (case symbol
            ; Main expression walker.
            [(nes-header) (apply built-in-nes-header
                                 (process-keys rest '(#:num-prg #:num-chr
                                                      #:mapper #:mirroring)))]
            [(init-system) (built-in-init-system)]
            [(defconst) (apply process-defconst (unwrap-args rest 2 0))]
            [(defaddr) (apply process-defaddr (unwrap-args rest 2 0))]
            [(defvar) (apply process-defvar (unwrap-args rest 1 1))]
            [(defsub) (process-proc 'sub (car rest) (cdr rest))]
            [(defvector) (process-proc 'vector (car rest) (cdr rest))]
            [(deflabel) (process-deflabel (car rest))]
            [(defbuffer) (apply process-defbuffer (unwrap-args rest 2 0))]
            [(program-begin) (process-program-begin (lref rest 0))]
            [(program-complete) (process-program-complete)]
            [(push pull) (process-stack symbol (unwrap-args rest 0 3))]
            [(set!) (process-with-args process-set-bang rest 2)]
            [(block) (process-block (car rest) (cdr rest))]
            [(bytes) (process-bytes rest)]
            [(include-binary) (process-include-binary (lref rest 0)
                                                      (lref rest 1)
                                                      (lref rest 2)
                                                      (lref rest 3))]
            [(loop-down-from) (process-loop-down (car rest) (cadr rest)
                                                 (cddr rest))]
            [(loop-up-to loop) (process-loop-up (car rest) (cadr rest)
                                                (caddr rest) (cdddr rest))]
            [(repeat) (process-repeat (car rest) (cdr rest))]
            [(let) (process-let (car rest) (cdr rest))]
            [(if) (process-with-args process-if rest 3)]
            [(while) (process-while (car rest) (cdr rest))]
            [(do) (for [(elem rest)]
                       (process-form elem))]
            [(peek) (process-peek (lref rest 0) (lref rest 1))]
            [(poke!) (process-poke! (lref rest 0) (lref rest 1) (lref rest 2))]
            [(ppu-load) (process-ppu-load (lref rest 0) (lref rest 1)
                                          (lref rest 2))]
            [(ppu-memset) (process-ppu-memset
                           (lref rest 0) (lref rest 1) (lref rest 2)
                           (lref rest 3) (lref rest 4))]
            [(ppu-memcpy) (process-ppu-memcpy
                           (lref rest 0) (lref rest 1) (lref rest 2)
                           (lref rest 3) (lref rest 4) (lref rest 5))]
            [(ppu-memcpy16) (process-ppu-memcpy16
                             (lref rest 0) (lref rest 1) (lref rest 2)
                             (lref rest 3) (lref rest 4))]
            [(set-sprite!) (process-set-sprite! (lref rest 0) (lref rest 1)
                                                (lref rest 2))]
            [(get-sprite) (process-get-sprite (lref rest 0) (lref rest 1))]
            [(high low) (process-address-specifier symbol (lref rest 0))]
            [(memset) (process-memset (lref rest 0) (lref rest 1)
                                      (lref rest 2))]
            [(memcpy) (process-memcpy (lref rest 0) (lref rest 1)
                                      (lref rest 2))]
            [(scale16) (process-scale16 (lref rest 0) (lref rest 1)
                                        (lref rest 2))]
            [(load-pointer) (process-load-pointer (lref rest 0) (lref rest 1))]
            [(set-pointer!) (process-set-pointer! (lref rest 0) (lref rest 1))]
            [(or-sprites-attr!)
             (process-sprites-apply-to-field! (lref rest 0) (lref rest 1)
                                              (lref rest 2) 2 'eor)]
            [(add-sprites-y!)
             (process-sprites-apply-to-field! (lref rest 0) (lref rest 1)
                                              (lref rest 2) 0 'adc)]
            [(add-sprites-x!)
             (process-sprites-apply-to-field! (lref rest 0) (lref rest 1)
                                              (lref rest 2) 3 'adc)]
            [(sub-sprites-y!)
             (process-sprites-apply-to-field! (lref rest 0) (lref rest 1)
                                              (lref rest 2) 0 'sbc)]
            [(sub-sprites-x!)
             (process-sprites-apply-to-field! (lref rest 0) (lref rest 1)
                                              (lref rest 2) 3 'sbc)]
            [(_rnd) (process-underscore-rnd)]
            [(adc cmp cpx cpy eor sbc)
             (process-instruction-transitive symbol rest)]
            [(and ora) (process-cond-or-expression symbol rest)]
            [(or) (process-cond-or-expression 'ora rest)]
            [(xor) (process-instruction-transitive 'eor rest)]
            [(lda ldx ldy)
             (process-instruction-mov #f symbol rest)]
            [(sta stx sty)
             (process-instruction-mov #t symbol rest)]
            [(asl lsr rol ror)
             (process-instruction-accumulator symbol (lref rest 0))]
            [(bit dec inc)
             (process-instruction-standalone symbol (lref rest 0))]
            [(beq bcc bcs bne bmi bpl bvc bvs jmp)
             (process-instruction-branch symbol (lref rest 0))]
            [(clc cld cli clv dex dey inx iny nop pha pla rts sec)
             (process-instruction-implied symbol)]
            [(tax tay tsx txa txs tya)
             (process-instruction-implied symbol)]
            [(asm jsr) (process-raw symbol rest)]
            [(+ - eq? > < >> << <s >= <= >s <=s)
             (process-math symbol (lref rest 0) (lref rest 1))]
            [(*)
             (process-mul (lref rest 0) (lref rest 1))]
            [(/ mod)
             (process-div symbol (lref rest 0) (lref rest 1))]
            [(+16!)
             (process-add16 (lref rest 0) (lref rest 1) (lref rest 2))]
            [(-16!)
             (process-sub16 (lref rest 0) (lref rest 1) (lref rest 2))]
            [(not) (process-not symbol (lref rest 0))]
            [else (add-error "Not defined" symbol)
                  (format ";Unknown: ~a ~a" symbol rest)])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *user-specified-org* #f)

(define (analyze-proc context-decl body)
  (assert context-decl syntax?)
  (assert body list?)
  (let* ((decl (syntax->datum context-decl))
         (name (car decl))
         (args (cdr decl)))
    ; TODO: Add number of parameters to table, to check when called.
    (make-function! name)))

(define (analyze-label context-name)
  (assert context-name syntax?)
  (let* ((name (syntax->datum context-name)))
    (make-address! name 0)))

(define (analyze-var context-name)
  (assert context-name syntax?)
  (let* ((name (syntax->datum context-name)))
    (make-variable! name #:global #t)))

(define (analyze-const context-name)
  (assert context-name syntax?)
  (let* ((name (syntax->datum context-name)))
    (make-const! name 0)))

(define (analyze-program-begin)
  (set! *user-specified-org* #t))

(define (analyze-form form)
  (assert form syntax?)
  (let* ((inner (syntax-e form))
         (first (if (list? inner) (car inner) #f))
         (rest (if (list? inner) (cdr inner) #f))
         (symbol (if first (syntax->datum first) #f)))
    (parameterize [(*co2-source-context* (vector form #t))]
      (if (or (not symbol) (function? symbol) (macro? symbol))
          #f
          (case symbol
            ; Main expression walker.
            [(defsub) (analyze-proc (car rest) (cdr rest))]
            [(defvector) (analyze-proc (car rest) (cdr rest))]
            [(defvar) (analyze-var (car rest))]
            [(deflabel) (analyze-label (car rest))]
            [(defconst) (analyze-const (car rest))]
            [(include-binary) (analyze-label (car rest))]
            [(program-begin) (analyze-program-begin)]
            [(do) (for [(elem rest)]
                       (analyze-form elem))])))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (generate-func-memory-addresses results)
  (emit "")
  (emit "")
  (for [(elem results)]
       (let* ((name (list-ref elem 0))
              (param (list-ref elem 1))
              (addr (list-ref elem 2)))
         (if (list? param)
             (emit (format "~a = $~x" (cadr param) (+ addr var-allocation)))
             (emit (format "_~a__~a = $~x" (normalize-name name)
                           (normalize-name param) (+ addr var-allocation)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Entry point

; source symbol, assembly label, address / const

(define (define-built-ins)
  ;; Memory-mapped addresses
  (for [(elem reg-table)]
       (let ((symbol (car elem))
             (value (cadr elem)))
         (make-address! symbol value)))
  ;; PPU flags
  (for [(elem ppu-flags)]
       (let ((symbol (car elem))
             (value (cadr elem)))
         (make-const! symbol value)))
  ;; Reserved zeropage variables for internal use
  (for [(elem reserved-zero-page)]
       (let ((symbol (car elem))
             (value (cadr elem)))
         (make-address! symbol value))))

(define (generate-prefix)
  ;; Memory-mapped addresses
  (for [(elem reg-table)]
       (let ((symbol (car elem))
             (value (cadr elem)))
         (emit (format "~a = $~x" (normalize-name symbol) value))))
  ;; PPU flags
  (for [(elem ppu-flags)]
       (let ((symbol (car elem))
             (value (cadr elem)))
         (emit (format "~a = $~x" (normalize-name symbol) value))))
  ;; Reserved zeropage variables for internal use
  (for [(elem reserved-zero-page)]
       (let ((symbol (car elem))
             (value (cadr elem)))
         (emit (format "~a = $~x" (normalize-name symbol) value))))
  (emit ""))

(define *has-generated-suffix* #f)

(define (generate-suffix)
  (when (not *has-generated-suffix*)
    (set! *has-generated-suffix* #t)
    (emit "") (emit "")
    ;; Pre-defined
    (when *need-ppu-load-functions*
      (emit "_ppu_load_by_pointer:")
      (emit 'lda "(_pointer),y")
      (emit 'sta "REG_PPU_DATA")
      (emit 'iny)
      (emit 'bne "_ppu_load_by_pointer")
      (emit 'inc "_pointer+1")
      (emit 'dex)
      (emit 'bne "_ppu_load_by_pointer")
      (emit 'rts)
      (emit "")
      (emit "_ppu_load_by_val:")
      (emit 'sta "REG_PPU_DATA")
      (emit 'iny)
      (emit 'bne "_ppu_load_by_val")
      (emit 'dex)
      (emit 'bne "_ppu_load_by_val")
      (emit 'rts)
      (emit ""))
    ;; Data segment
    (let ((data (get-data-segment)))
      (for/list [(elem data)]
                (let ((label (car elem))
                      (value (cadr elem)))
                  (emit (format "~a:" label))
                  (emit (format "  .byte ~a" (list->byte-string value)))
                  (emit (format "~a_length = ~a" label (length value)))
                  (emit ""))))
    (emit ".pad $fffa")
    ; Output the vectors, entry points defined by hardware.
    (let ((build '()))
      (for [(entry '(irq reset nmi))]
           (if (member entry *entry-points*)
               (set! build (cons (symbol->string entry) build))
               (set! build (cons "0" build))))
      (emit (string-append ".word " (string-join build ","))))))

(define (analyze-func-defs fname)
  (let ((f (open-input-file fname)))
    (define (loop)
      (let ((top-level-form (read-syntax fname f)))
        (when (not (eof-object? top-level-form))
              (analyze-form top-level-form)
              (loop))))
    (loop)
    (close-input-port f)))

(define (generate-assembly fname)
  (let ((f (open-input-file fname)))
    (port-count-lines! f)
    (define-built-ins)
    (generate-prefix)
    (define (loop)
      (let ((top-level-form (read-syntax fname f)))
        (when (not (eof-object? top-level-form))
              (process-form top-level-form)
              (loop))))
    (loop)
    (generate-suffix)
    (close-input-port f)))

(define (compile-co2 fname out-filename)
  (set-optimization! #t)
  (analyze-func-defs fname)
  (generate-assembly fname)
  (generate-func-memory-addresses (casla->allocations))
  (let ((f (open-output-file out-filename #:exists 'replace)))
    (for [(line *result*)]
         (write-string line f)
         (newline f))
    (close-output-port f))
  (when (has-errors?)
        (display-errors)
        (exit 1)))


(provide compile-co2)
(provide process-form)
(provide analyze-form)
(provide clear-result)
(provide clear-label-id)
(provide clear-var-allocation)
(provide clear-data-segment)
(provide fetch-result)
(provide make-variable!)
(provide make-function!)
(provide make-address!)
(provide make-const!)
(provide has-errors?)
(provide first-error)
(provide clear-errors)
(provide set-optimization!)
(provide generate-func-memory-addresses)
(provide get-data-segment)
