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
(require racket/hash)
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
  '((PPU-CTRL-NMI                #x80)
    (PPU-CTRL-SPRITE-8x16        #x20)
    (PPU-CTRL-1000-BG            #x10)
    (PPU-CTRL-1000-SPR           #x08)
    (PPU-CTRL-VRAM-DOWN          #x04)
    (PPU-MASK-TINT-BLUE          #x80)
    (PPU-MASK-TINT-GREEN         #x40)
    (PPU-MASK-TINT-RED           #x20)
    (PPU-MASK-SHOW-SPR           #x10)
    (PPU-MASK-SHOW-BG            #x08)
    (PPU-MASK-NOCLIP-SPR         #x04)
    (PPU-MASK-NOCLIP-BG          #x02)
    (PPU-MASK-GREYSCALE          #x01)
    (PPU-MASK-NO-SHOW-SPR        #xef)))

(define reserved-zero-page
  '((ppu-ctrl                    #x00)
    (ppu-mask                    #x01)
    (frame-num                   #x02)
    (-count                      #x03)
    (-tmp                        #x04)
    (-loop                       #x05)
    (-pointer                    #x06)
    (-pointer-1                  #x07)
    (-aux                        #x08)
    (-aux-1                      #x09)))

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

; List of symbol definitions, representing a stack. Front of list is global.
(define sym-label-defs (list (make-hash)))

(define *data-segment* '())

(define (clear-var-allocation)
  (set! var-allocation #x10)
  (set! sym-label-defs (list (make-hash))))

(define (clear-data-segment)
  (set! *data-segment* '()))

(define *resource-bank* (make-gvector))

(define (add-resource filename label)
  (let ((id #f))
    (set! id (format "_imported_resource_~x"
                     (gvector-count *resource-bank*)))
    (gvector-add! *resource-bank* (list filename label))
    id))

(define (count-resources)
  (gvector-count *resource-bank*))

(define (has-resources)
  (> (gvector-count *resource-bank*) 0))

(define (get-all-resources)
  (gvector->list *resource-bank*))

(struct sym-label (sym name address kind))

(define (make-variable! sym #:label [label #f] #:global [global #f]
                            #:addr [addr #f])
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
    (if addr
        (hash-set! table sym (sym-label sym name addr 'var))
        (when (not (hash-has-key? table sym))
              (hash-set! table sym (sym-label sym name n 'var))
              (when (not label)
                    (set! var-allocation (+ 1 var-allocation)))))
    (hash-ref table sym)))

(define (make-local! sym scope)
  (let ((label (format "_~a__~a" (normalize-name scope) (normalize-name sym))))
    (make-variable! sym #:label label)))

(define (make-word! sym)
  (let ((name (normalize-name sym))
        (n var-allocation))
    (hash-set! (car sym-label-defs) sym (sym-label sym name n 'word))
    (set! var-allocation (+ 2 var-allocation))
    (hash-ref (car sym-label-defs) sym)))

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

(define (make-pointer! sym)
  (let ((name (normalize-name sym))
        (n var-allocation))
    (hash-set! (car sym-label-defs) sym (sym-label sym name n 'pointer))
    (set! var-allocation (+ 2 var-allocation))
    (hash-ref (car sym-label-defs) sym)))

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

(define (word? sym)
  (let ((lookup (sym-label-lookup sym)))
    (and lookup (eq? (sym-label-kind lookup) 'word))))

(define (address? sym)
  (let ((lookup (sym-label-lookup sym)))
    (and lookup (eq? (sym-label-kind lookup) 'addr))))

(define (const? sym)
  (let ((lookup (sym-label-lookup sym)))
    (and lookup (eq? (sym-label-kind lookup) 'const))))

(define (metavar? sym)
  (let ((lookup (sym-label-lookup sym)))
    (and lookup (eq? (sym-label-kind lookup) 'metavar))))

(define (num-like? arg)
  (or (number? arg) (const? arg) (metavar? arg)))

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
  (member name '(when load-pointer
                 set-sprite-y! set-sprite-id! set-sprite-attr!
                 set-sprite-x! get-sprite-y get-sprite-id
                 get-sprite-attr get-sprite-x)))

;;----------------------------------------------------------------
; Emit

(define *result* (make-gvector))

(define *result-stack* (make-gvector))

(define *result-target-bank* #f)
(define *result-seen-banks* #f)

(define *result-bank-base-addr* 0)
(define *result-bank-depend-sym* #f)
(define *result-bank-defined-here* #f)

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

(define (lref elems index)
  (if (not (list? elems))
      #f
      (if (>= index (length elems))
          #f
          (list-ref elems index))))

(define (arg->str arg)
  (cond
   [(string? arg) arg]
   [(symbol? arg) (let* ((lookup (sym-label-lookup arg))
                         (kind (if lookup (sym-label-kind lookup) #f)))
                    (cond
                     [(not lookup) (add-error "Variable not found" arg)
                                   (format ";Not found: ~a" arg)]
                     [(eq? kind 'const)
                        (format "#~a" (sym-label-name lookup))]
                     [(eq? kind 'metavar)
                        (format "#~a" (sym-label-address lookup))]
                     [(eq? kind 'var)
                        (sym-label-name lookup)]
                     [(eq? kind 'word)
                        (sym-label-name lookup)]
                     [(eq? kind 'addr)
                        (sym-label-name lookup)]
                     [(eq? kind 'pointer)
                        (sym-label-name lookup)]
                     [(eq? kind 'data)
                        (sym-label-name lookup)]
                     [else
                        (add-error (format "Unknown arg type for ~a" arg) kind)
                        (format ";type ~s" kind)]))]
   [(number? arg)
      (if (< arg #x100)
          (format "#$~x" (->unsigned arg))
          (begin (add-error "Overflow " arg)
                 (format ";overflow ~s" arg)))]
   [(char? arg) (format "#$~x" (->unsigned (char->integer arg)))]
   [(eq? arg #f) "#$00"]
   [(eq? arg #t) "#$ff"]
   [(literal-address? arg) (format "$~x" (literal-address-number arg))]
   [else (error (format "ERROR arg->str: ~a" arg))]))

(define (arg16->str arg)
  (cond
   [(string? arg)
      (let ((parts (string-split arg "!")))
        (when (< (length parts) 2)
              (set! parts (list (car parts) "#0")))
        parts)]
   [(symbol? arg) (let* ((lookup (sym-label-lookup arg))
                         (kind (if lookup (sym-label-kind lookup) #f)))
                    (cond
                     [(not lookup) (add-error "Variable not found" arg)
                                   (list (format ";Not found: ~a" arg)
                                         (format ";Not found: ~a" arg))]
                     [(eq? kind 'var)
                        (list (sym-label-name lookup) "#0")]
                     [(eq? kind 'word)
                        (list (sym-label-name lookup)
                              (format "~a+1" (sym-label-name lookup)))]
                     [else
                        (add-error "Unknown arg type" arg)
                        (list (format ";type ~s" kind)
                              (format ";type ~s" kind))]))]
   [(number? arg)
      (list (format "#~a" (modulo arg #x100))
            (format "#~a" (quotient arg #x100)))]
   [else (error (format "ERROR arg16->str: ~s" arg))]))

(define (resolve-arg arg #:even-const [even-const #f])
  (cond
   [(number? arg) arg]
   [(symbol? arg) (let* ((lookup (sym-label-lookup arg))
                         (kind (if lookup (sym-label-kind lookup) #f)))
                    (cond
                     [(eq? kind 'const)
                        (if even-const
                            (sym-label-address lookup)
                            (normalize-name arg))]
                     [(eq? kind 'metavar)
                        (sym-label-address lookup)]
                     [(eq? kind 'addr)
                        (sym-label-name lookup)]
                     [else (error (format "ERROR resolve-arg: ~a" arg))]))]
   [else (error (format "ERROR resolve-arg: ~a" arg))]))

;;----------------------------------------------------------------

(define (process-set-pointer! context-place context-value)
  (emit-context)
  (let ((place (syntax->datum context-place))
        (value (syntax->datum context-value))
        (ptr-name #f))
   (cond
    [(symbol? value)
       (emit 'lda (format "#<~a" (normalize-name value)))
       (emit 'sta (format "~a+0" (normalize-name place)))
       (emit 'lda (format "#>~a" (normalize-name value)))
       (emit 'sta (format "~a+1" (normalize-name place)))]
    [(and (list? value) (eq? (car value) 'resource-address))
       (set! ptr-name (cadr value))
       (emit 'lda (format "~a+1" (normalize-name ptr-name)))
       (emit 'sta (format "~a+0" (normalize-name place)))
       (emit 'lda (format "~a+2" (normalize-name ptr-name)))
       (emit 'sta (format "~a+1" (normalize-name place)))]
    [(and (list? value) (eq? (car value) 'addr))
       (set! ptr-name (cadr value))
       (emit 'lda (format "#$~x" (modulo (resolve-arg ptr-name) #x100)))
       (emit 'sta (format "~a+0" (normalize-name place)))
       (emit 'lda (format "#$~x" (quotient (resolve-arg ptr-name) #x100)))
       (emit 'sta (format "~a+1" (normalize-name place)))]
    [(number? value)
       (add-error "set-pointer! needs (addr ...)" value)]
    [else
       (add-error "Could not set-pointer! from" value)])))

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
    (emit 'lda (format "#$~x" (quotient (resolve-arg ppu-addr) #x100)))
    (emit 'sta "REG_PPU_ADDR")
    (emit 'lda (format "#$~x" (modulo (resolve-arg ppu-addr) #x100)))
    (emit 'sta "REG_PPU_ADDR")
    (cond
     [(and (number? address) (< num #x100))
        (emit 'lda (arg->str address))
        (emit 'ldy (format "#~a" (- #x100 num)))
        (emit 'ldx "#1")
        (emit 'jsr "_ppu_load_by_val")]
     [(number? address)
        (emit 'lda (arg->str address))
        (emit 'ldy "#0")
        ; Only using div, not also mod. This seems like a bug.
        (emit 'ldx (format "#~a" (quotient (resolve-arg num) #x100)))
        (emit 'jsr "_ppu_load_by_val")]
     [(and (list? address) (eq? (car address) 'resource-address))
        (emit 'lda (format "~a+1" (arg->str (cadr address))))
        (emit 'sec)
        (emit 'sbc (arg->str (- #x100 num)))
        (emit 'sta "_pointer+0")
        (emit 'lda (format "~a+2" (arg->str (cadr address))))
        (emit 'sbc (arg->str 0))
        (emit 'sta "_pointer+1")
        (emit 'ldy (format "#~a" (- #x100 num)))
        (emit 'ldx "#1")
        (emit 'jsr "_ppu_load_by_pointer")]
     [(< num #x100)
        (emit 'lda (format "#<(~a+~a)" (arg->str address) num))
        (emit 'sta "_pointer+0")
        (emit 'lda (format "#>(~a+~a-$100)" (arg->str address) num))
        (emit 'sta "_pointer+1")
        (emit 'ldy (format "#~a" (- #x100 num)))
        (emit 'ldx "#1")
        (emit 'jsr "_ppu_load_by_pointer")]
     [else
        (emit 'lda (format "#<~a" (arg->str address)))
        (emit 'sta "_pointer+0")
        (emit 'lda (format "#>~a" (arg->str address)))
        (emit 'sta "_pointer+1")
        (emit 'ldy "#0")
        ; Only using div, not also mod. This seems like a bug.
        (emit 'ldx (format "#~a" (quotient (resolve-arg num) #x100)))
        (emit 'jsr "_ppu_load_by_pointer")])))

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
  (if (eq? (curr-bit-size) 8)
      (begin (process-argument context-left)
             (for [(n num-right)]
                  (emit 'asl "a")))
      (begin (process-argument context-left #:bit-16 #t)
             (emit 'stx "_high_byte")
             (for [(i (in-range num-right))]
                  (emit 'asl "a")
                  (emit 'rol "_high_byte"))
             (emit 'ldx "_high_byte"))))

(define (process-div-right-shift context-left num-right)
  (if (eq? (curr-bit-size) 8)
      (begin (process-argument context-left)
             (for [(n num-right)]
                  (emit 'lsr "a")))
      (begin (process-argument context-left #:bit-16 #t)
             (emit 'stx "_high_byte")
             (for [(i (in-range num-right))]
                  (emit 'lsr "_high_byte")
                  (emit 'ror "a"))
             (emit 'ldx "_high_byte"))))

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
    (cond
     [(symbol? base)
        (emit 'adc (format "#<~a" (normalize-name base)))
        (emit 'tay)
        (emit 'lda "_tmp")
        (emit 'adc (format "#>~a" (normalize-name base)))]
     [(and (list? base) (eq? (car base) 'resource-address))
        (emit 'adc (format "~a+1" (normalize-name (cadr base))))
        (emit 'tay)
        (emit 'lda "_tmp")
        (emit 'adc (format "~a+2" (normalize-name (cadr base))))])))

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

(define (m-expand-load-pointer x)
  (append (list 'peek) (cdr x)))

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
            [(eq? (car i) 'set-sprite-x!) (m-expand-set-sprite! i 3)]
            [(eq? (car i) 'load-pointer) (m-expand-load-pointer i)]
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

(define (string-startswith subject find)
  (and (>= (string-length subject) (string-length find))
       (string=? (substring subject 0 (string-length find)) find)))

(define (string-endswith subject find)
  (and (>= (string-length subject) (string-length find))
       (string=? (substring subject (- (string-length subject)
                                       (string-length find)))
                 find)))

(define (dollar-hex->number text)
  (string->number (substring text 1) 16))

(define (register? text)
  (when (symbol? text)
        (set! text (symbol->string text)))
  (or (string=? text "a") (string=? text "x") (string=? text "y")))

(define (immediate? obj)
  (or (number? obj)
      (and (symbol? obj)
           (let ((lookup (sym-label-lookup obj)))
             (and lookup (eq? (sym-label-kind lookup) 'const))))))

(define (undefined? obj)
  (and (symbol? obj) (not (sym-label-lookup obj))))

(define (pointer? obj)
  (and (symbol? obj)
       (let ((lookup (sym-label-lookup obj)))
         (and lookup (eq? (sym-label-kind lookup) 'pointer)))))

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
         (len (min 40 (string-length source)))
         (orig-code (substring source 0 len)))
    (set! orig-code (string-replace orig-code "\n" "#\\newline"))
    (if fname
        (format ";~a:~a ~a" fname line-num orig-code)
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

(define *num-prg-banks* #f)

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
    (set! *num-prg-banks* num-prg)
    (emit-context)
    (emit ".byte \"NES\",$1a")
    (emit (format ".byte $~x" (or num-prg 1)))
    (emit (format ".byte $~x" (or num-chr 0)))
    (emit (format ".byte $~x" third-byte))
    (emit (format ".byte ~a"
                  (string-join (build-list 9 (lambda (x) "$0")) ",")))
    (emit "")))

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

(define (is-optimizable? symbol)
  (member symbol '(< >= eq? not and)))

(define (can-optimize-tree? tree)
  (every-first-in-tree? is-optimizable? tree))

(struct opt-codegen (oper [truth-case #:mutable] [false-case #:mutable]
                          [branch-instr #:mutable]))

; TODO: Change this, name is too similar to set-optimization.
(define (set-optimize! key value)
  (when (*opt-mode*)
        (cond
         [(eq? key 'branch-instr)
            (set-opt-codegen-branch-instr! (*opt-mode*) value)]
         [else (error (format "Don't know how to set-optimize! ~a" key))])))

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

(define (invert-condition instr)
  (cond
   [(eq? instr 'bne) 'beq]
   [(eq? instr 'beq) 'bne]
   [(eq? instr 'bcc) 'bcs]
   [(eq? instr 'bcs) 'bcc]
   [(eq? instr 'bmi) 'bpl]
   [(eq? instr 'bpl) 'bmi]
   [else (error (format "Don't know how to invert condition ~a" instr))]))

(define (boolean? obj)
  (if (list? obj)
      (member (car obj) '(< >= eq? bool not))
      #f))

(define (all-boolean-args args)
  (let ((result #t))
    (for [(item args)]
         (let ((obj (syntax->datum item)))
           (when (not (boolean? obj))
                 (set! result #f))))
    result))

(define (process-defvar name [value 0])
  ; Created by analyze-defvar, get the sym-label.
  (let* ((def (normalize-name name))
         (sym-label (sym-label-lookup name))
         (addr #f))
    (when (not sym-label)
          (set! sym-label (make-variable! name #:global #t)))
    (set! addr (sym-label-address sym-label))
    (emit-blank)
    (emit-context)
    (emit (format "~a = $~a" def (left-pad (number->string addr 16) #\0 2)))
    (when (not (= value 0))
          (emit 'lda (format "#$~x" value))
          (emit 'sta def))))

(define (process-defvarmem name addr)
  ; Created by analyze-defvarmem, get the sym-label.
  (let* ((def (normalize-name name))
         (sym-label (sym-label-lookup name))
         (addr (sym-label-address sym-label)))
    (emit-blank)
    (emit-context)
    (emit (format "~a = $~a" def (left-pad (number->string addr 16) #\0 2)))))

(define (process-defword name)
  ; Created by analyze-defword, get the sym-label.
  (let* ((def (normalize-name name))
         (sym-label (sym-label-lookup name))
         (addr (sym-label-address sym-label)))
    (emit-blank)
    (emit-context)
    (emit (format "~a = $~a" def (left-pad (number->string addr 16) #\0 2)))))

(define (process-defpointer name)
  ; Created by analyze-defpointer, get the sym-label.
  (let* ((def (normalize-name name))
         (sym-label (sym-label-lookup name))
         (addr (sym-label-address sym-label)))
    (emit-blank)
    (emit-context)
    (emit (format "~a = $~a" def (left-pad (number->string addr 16) #\0 2)))))

(define (process-defaddr name value)
  ; Created by analyze-defaddr, get the sym-label.
  (let* ((def (normalize-name name))
         (sym-label (sym-label-lookup name))
         (value (sym-label-address sym-label)))
    (emit-blank)
    (emit-context)
    (emit (format "~a = $~a" def (left-pad (number->string value 16) #\0 2)))))

(define (process-defconst name value)
  ; Created by analyze-defconst, get the sym-label.
  (let* ((def (normalize-name name))
         (sym-label (sym-label-lookup name))
         (value (sym-label-address sym-label)))
    (emit-blank)
    (emit-context)
    (cond
     [(number? value)
        (let ((repr #f))
          (set! repr (left-pad (number->string value 16) #\0 2))
          (emit (format "~a = $~a" def repr)))]
     [(and (list? value) (eq? (first value) '+))
        (let ((left #f) (right #f))
          ; TODO: Error handling.
          (set! left (resolve-arg (second value) #:even-const #t))
          (set! right (resolve-arg (third value) #:even-const #t))
          (set! value (+ left right))
          ; Override the old value.
          (make-const! name value)
          (emit (format "~a = ~a" def value)))]
     [else
        (error (format "Cannot define constant using ~a" value))])))

(define (process-defenum context-name syms)
  (let ((name (syntax->datum context-name)))
    (for [(context-sym syms) (i (in-naturals))]
         (let* ((name (syntax->datum context-sym))
                (def (normalize-name name))
                (repr (left-pad (number->string i 16) #\0 2)))
           (make-const! name i)
           (emit (format "~a = $~a" def repr))))))

(define (process-deflabel context-name)
  (let* ((name (syntax->datum context-name))
         (def (normalize-name name)))
    (emit-blank)
    (emit-context)
    (emit (format "~a:" def))
    (when *result-bank-defined-here*
          (hash-set! *result-bank-defined-here* name #t))))

(define (process-defbuffer name length)
  (let* ((def (normalize-name name))
         (sym-label (sym-label-lookup name))
         (addr (sym-label-address sym-label)))
    (emit-blank)
    (emit-context)
    (emit (format "~a = $~a" def (left-pad (number->string addr 16) #\0 2)))))

(define (process-defresource context-name resource-filename)
  (let ((name (syntax->datum context-name)))
    (make-const! (string->symbol (format "res-~a" name)) (count-resources))
    (emit (format "res_~a = $~x" (normalize-name name) (count-resources)))
    (add-resource (syntax->datum resource-filename) (normalize-name name))))

(define (process-resource-access context-name)
  (let* ((name (syntax->datum context-name))
         (label (normalize-name name)))
    (emit 'lda (format "#~a__attr__bank" label))
    (emit 'ldx (format "#~a__attr__low"  label))
    (emit 'ldy (format "#~a__attr__high" label))))

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
        (process-body-statements body))
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

(define *bit-size* (make-parameter #f))

(define (curr-bit-size)
  (if (and (*bit-size*) (eq? (vector-ref (*bit-size*) 0) 16)) 16 8))

(define (process-form-as-16-bit form)
  (parameterize [(*bit-size* (vector 16))]
    (process-form form)))

(define (process-set-bang target expr)
  (assert target syntax?)
  (assert expr syntax?)
  (let ((place (syntax->datum target)))
    (cond
     [(undefined? place)
        (add-error "Undefined var" place)]
     [(immediate? place)
        (add-error "Cannot assign to" place)]
     [(variable? place)
        (process-argument expr)
        (emit 'sta (arg->str place))]
     [(address? place)
        (process-argument expr)
        (emit 'sta (arg->str place))]
     [(word? place)
        (parameterize [(*bit-size* (vector 16))]
          (process-argument expr #:bit-16 #t)
          (emit 'sta (car (arg16->str place)))
          (emit 'stx (cadr (arg16->str place))))]
     [else
        (error (format "Don't know how to set! ~a to ~a" place expr))])))

(define (ensure-place place)
  (cond
   [(undefined? place)
      (add-error "Undefined var" place)
      "_"]
   [(immediate? place)
      (add-error "Cannot assign to" place)
      "_"]
   [(variable? place)
      place]
   [(address? place)
      place]
   [else
      (error (format "Don't know how to set! ~a" place))]))

(define (process-set-multiple context-one context-two context-three
                              context-four)
  (let ((context-expr #f) (place #f))
    (cond
     [context-four  (set! context-expr context-four)
                    (set! context-four #f)]
     [context-three (set! context-expr context-three)
                    (set! context-three #f)]
     [context-two   (set! context-expr context-two)
                    (set! context-two #f)])
    (process-argument context-expr)
    (when context-one
          (emit 'sta (arg->str (ensure-place (syntax->datum context-one)))))
    (when context-two
          (emit 'stx (arg->str (ensure-place (syntax->datum context-two)))))
    (when context-three
          (emit 'sty (arg->str (ensure-place (syntax->datum context-three)))))))

(define (process-cond-or-expression instr args)
  (if (or (not (*opt-mode*)) (not (all-boolean-args args)))
      ; Not in a conditional, process as a normal instruction.
      (parameterize [(*opt-mode* #f)]
        (process-instruction-transitive instr args))
      ; Optimized.
      (cond
       [(eq? instr 'and)
          (let ((optimizer #f)
                (all-truth-label (opt-codegen-truth-case (*opt-mode*)))
                (any-false-label (opt-codegen-false-case (*opt-mode*)))
                (branch-instr #f)
                (fail-instr #f))
            (for [(context-test args)]
                 (set! optimizer (opt-codegen 'if
                                              (generate-label "truth_case")
                                              any-false-label
                                              'bne))
                 (parameterize [(*opt-mode* optimizer)]
                   (process-argument context-test)
                   ; AND - if any test is false, go to false-case
                   (set! branch-instr (opt-codegen-branch-instr (*opt-mode*)))
                   (when (not (eq? branch-instr 'fall-through-to-true))
                      (set! fail-instr (invert-condition branch-instr))
                      (emit fail-instr any-false-label))))
            (set-optimize! 'branch-instr 'fall-through-to-true))]
       [else (parameterize [(*opt-mode* #f)]
               (process-instruction-transitive instr args))])))

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
                          #:skip-context [skip-context #f] #:bit-16 [bit-16 #f])
  (let ((ret "a")
        (arg (syntax->datum context-arg)))
    (cond
     [(list? arg)
        ; Evaluate the expression, preserving registers if needed.
        (parameterize [(*co2-source-context* (vector context-arg #t))]
          (when preserve
                (process-stack 'push preserve #:skip-context #t))
          (process-form context-arg)
          (when preserve
                (set! ret "_tmp")
                (emit 'sta ret)
                (process-stack 'pull preserve #:skip-context #t)))]
     [(not bit-16)
        ; Load an atomic value in 8-bit mode.
        (let ((val (arg->str arg)))
          (when (and (not skip-context) (vector-ref (*co2-source-context*) 1))
                (emit-context)
                (vector-set! (*co2-source-context*) 1 #f))
          (cond
           [(not as)
              (emit 'lda val)]
           [(eq? as 'none)
              #f]
           [(eq? as 'rhs)
              (set! ret val)]
           [(eq? as 16)
              (emit 'ldy (format "#<~a" val))
              (emit 'lda (format "#>~a" val))]
           [else
              (emit as val)
              (set! ret (string-last (symbol->string as)))]))]
     [bit-16
        ; Load a 16-bit value into A:X
        (let* ((pair (arg16->str arg))
               (low-val  (car pair))
               (high-val (cadr pair)))
          (when (and (not skip-context) (vector-ref (*co2-source-context*) 1))
                (emit-context)
                (vector-set! (*co2-source-context*) 1 #f))
          (cond
           [(not as)
              (emit 'lda low-val)
              (emit 'ldx high-val)]
           [(eq? as 'rhs)
              (set! ret (format "~a!~a" low-val high-val))]
           [else
              (emit (format "; Dunno ~a ~a" low-val high-val))]))])
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

(define *opt-no-retval-form* (make-parameter #f))

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
    (process-body-statements body)
    ; Pop scope.
    (sym-label-pop-scope)
    ; Label that #:break goes to.
    (emit-label break-label)))

(define (process-bytes args)
  (let ((build (make-gvector))
        (depend (make-hash)))
    (define (add-elem-to-vector elem)
      (cond
       [(number? elem) (gvector-add! build (format "$~x" elem))]
       [(string? elem) (gvector-add! build (format "~s" elem))]
       [(char? elem)   (gvector-add! build (format "~s" (char->integer elem)))]
       [(symbol? elem)
        (let ((lookup (sym-label-lookup elem))
              (normal (normalize-name elem)))
          (hash-set! depend elem #t)
          (cond
           [(const? elem) (gvector-add! build normal)]
           [(metavar? elem) (gvector-add! build
                                          (format "$~x" (resolve-arg elem)))]
           [(address? elem) (begin
                              (gvector-add! build (format "<~a" normal))
                              (gvector-add! build (format ">~a" normal)))]
           [(function? elem) (begin
                               (gvector-add! build (format "<~a" normal))
                               (gvector-add! build (format ">~a" normal)))]
           [else (add-error "Cannot output bytes" elem)]))]
       [(list? elem) (for [(item elem)]
                          (add-elem-to-vector item))]
       [else (add-error "Cannot output bytes" elem)]))
    (for [(context-elem args)]
         (let ((elem (syntax->datum context-elem)))
           (add-elem-to-vector elem)))
    (emit (string-append ".byte " (string-join (gvector->list build) ",")))
    ; Add symbols in `depend` into the list of those the resource bank needs.
    (when *result-bank-depend-sym*
      (hash-union! *result-bank-depend-sym* depend
                   #:combine/key (lambda (k v1 v2) v1)))))

(define (process-include context-filename)
  (when (not *include-base*)
        (error "ERROR: Include base not assigned"))
  (let ((filename (syntax->datum context-filename)))
    (set! filename (string-append *include-base* filename))
    (process-file filename)))

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

(define (process-resource-bank context-resource)
  (let ((res (syntax->datum context-resource)))
    (emit 'lda (format "~a+0" (normalize-name res)))))

(define (process-resource-bank-begin context-target-bank)
  (gvector-add! *result-stack* *result*)
  (set! *result* (make-gvector))
  (set! *result-bank-base-addr* #f)
  (set! *result-bank-depend-sym* (make-hash))
  (set! *result-bank-defined-here* (make-hash))
  (set! *result-target-bank* (syntax->datum context-target-bank)))

(require "assemble.scm")

(define (read-listing-file filename)
  (let ((result (make-gvector))
        (lines #f) (address #f) (label #f) (m #f))
    (set! lines (file->lines filename))
    (for ([line lines])
         (set! address #f)
         (set! label #f)
         (if (has-label? line)
             (begin (set! address (substring line 1 5))
                    (set! label (get-label line))
                    (gvector-add! result (list label address)))
             (begin (set! m (match-equ line))
                    (when m
                      (set! label (list-ref m 1))
                      (set! address (list-ref m 2))
                      (gvector-add! result (list label address))))))
    (gvector->list result)))

(define (read-extern-labels)
  (let ((lst-filename #f)
        (result (make-hash))
        (inner #f))
    (when *result-seen-banks*
          (for [(num *result-seen-banks*)]
               (set! lst-filename (format "~a~a.lst" *res-out-file* num))
               (set! inner (read-listing-file lst-filename))
               (for [(pair inner)]
                    (hash-set! result (first pair)
                               (format "$~a" (second pair))))))
    result))

(define (process-resource-bank-complete)
  (let ((out-filename #f) (rom-filename #f) (lst-filename #f) (dat-filename #f)
        (lines #f) (inner #f) (address #f) (label #f) (m #f) (extern-labels #f))
    ; TODO: Refactor
    (when (has-errors?)
          (display-errors)
          (exit 1))
    (set! out-filename (format "~a~a.asm" *res-out-file* *result-target-bank*))
    (set! rom-filename (format "~a~a.nes" *res-out-file* *result-target-bank*))
    (set! lst-filename (format "~a~a.lst" *res-out-file* *result-target-bank*))
    (set! dat-filename (format "~a~a"     *res-out-file* *result-target-bank*))
    ; Output inner results to a file.
    (let ((f (open-output-file out-filename #:exists 'replace)))
      (when (not *result-bank-base-addr*)
        (set! *result-bank-base-addr* #x8000))
      (write-string (format ".org $~x\n" *result-bank-base-addr*) f)
      (for [(line *result*)]
           (write-string line f)
           (newline f))
      ; Handled depended symbols.
      (for [(sym (hash-keys *result-bank-depend-sym*))]
           (let ((lookup (sym-label-lookup sym))
                 (norm (normalize-name sym))
                 (val #f))
             (cond
              [(const? sym) (set! val (sym-label-address lookup))]
              [(metavar? sym) (set! val (sym-label-address lookup))]
              [(address? sym) (when (not extern-labels)
                                    (set! extern-labels (read-extern-labels)))
                              (if (hash-has-key? extern-labels norm)
                                  (set! val (hash-ref extern-labels norm))
                                  (set! val "$0000"))]
              [else (error (format "Don't know how to output ~a" sym))])
             (when (hash-has-key? *result-bank-defined-here* sym)
                   (set! val #f))
             (when val
                   (write-string (format "~a = ~a\n" norm val) f))))
      (close-output-port f))
    ; Assemble file.
    (assemble out-filename rom-filename)
    ; Pad out the bank.
    (let ((fin  (open-input-file rom-filename))
          (fout (open-output-file dat-filename #:exists 'replace))
          (data #f)
          (next #f))
      (set! data (read-bytes #x4000 fin))
      (write-bytes data fout)
      (set! next (read-bytes 1 fin))
      (when (not (eof-object? next))
            (let ((last-data #f))
              (set! last-data (subbytes data #x3ff0))
              (add-error (format "Overflow (ending ~s) in bank" last-data)
                         *result-target-bank*)))
      (set! data (make-bytes (- #x4000 (bytes-length data)) 0))
      (write-bytes data fout)
      (close-input-port fin)
      (close-output-port fout))
    ; Process listing file.
    (set! inner (read-listing-file lst-filename))
    (when (not *result-seen-banks*) (set! *result-seen-banks* (list)))
    (set! *result-seen-banks* (cons *result-target-bank* *result-seen-banks*))
    (set! *result* (gvector-remove-last! *result-stack*))
    (set! *result* (list->gvector (append (gvector->list *result*)
                                          (map pair-to-equ inner))))
    (set! *result-bank-base-addr* #f)
    (set! *result-bank-depend-sym* #f)
    (set! *result-bank-defined-here* #f)))

(define (process-resource-base-address addr)
  (set! *result-bank-base-addr* (syntax->datum addr)))

(define (has-label? line)
  (and (> (string-length line) 32)
       (regexp-match #px"^[\\w]+:" (substring line 32))))

(define (match-equ line)
  (if (> (string-length line) 32)
      (regexp-match #px"^([\\w]+) = \\$([\\w]+)$" (substring line 32))
      #f))

(define (get-label line)
  (cadr (regexp-match #px"^([\\w]+):" (substring line 32))))

(define (pair-to-equ pair)
  (format "~a = $~a" (first pair) (second pair)))

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
      (process-body-statements body)
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
    (process-body-statements body)
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
    (process-body-statements body)
    ; Pop scope.
    (sym-label-pop-scope)))

(define (process-if context-root context-condition context-truth context-false)
  (let ((truth-label (generate-label "truth_case"))
        (false-label (generate-label "false_case"))
        (done-label (generate-label "if_done"))
        (long-label #f)
        (if-cond (syntax->datum context-condition))
        (branch-instr 'bne)
        (optimizer #f))
    (emit-context)
    ; Enable optimizer, if possible.
    (when (and (optimization-enabled? 'if) (can-optimize-tree? if-cond))
      (set! long-label (generate-label "long_jump"))
      (set! optimizer (opt-codegen 'if truth-label long-label branch-instr)))
    (parameterize [(*opt-mode* optimizer)]
      ; Evaluate the conditional.
      (process-argument context-condition #:skip-context #t)
      ;
      (when (*opt-mode*)
         (set! truth-label (opt-codegen-truth-case (*opt-mode*)))
         (set! long-label  (opt-codegen-false-case (*opt-mode*)))
         (set! branch-instr (opt-codegen-branch-instr (*opt-mode*))))
      (cond
       ; TODO: An intelligent assembly could "know" the length of jumps, and
       ; emit proper control instructions to both be efficient and avoid
       ; branch overflows.
       [(and (eq? branch-instr 'fall-through-to-true) long-label)
          ; This case is used when the condition is an `and` clause of boolean
          ; expressions that compiled as short-circuited tests. Such as:
          ; (if (and (>= n 10) (< n 20)) ...)
          (emit 'jmp truth-label)
          (emit-label long-label)
          (emit 'jmp false-label)]
       [(and (eq? branch-instr 'fall-through-to-false) long-label)
          ; This case is used when the condition is a `not` command wrapping
          ; the type of condition described above. For example:
          ; (if (not (and (>= n 10) (< n 20))) ...)
          (emit 'jmp false-label)
          (emit-label long-label)
          (emit 'jmp truth-label)]
       [(not (eq? branch-instr 'fall-through-to-true))
          ; This case is used most of the time.
          ; Estimate the size of the truth-case, and see if a short branch
          ; can be used to skip the truth-case.
          (if (can-use-short-branch-to-false context-truth)
              (emit (invert-condition branch-instr) false-label)
              (begin (emit branch-instr truth-label)
                     (when long-label
                           (emit-label long-label))
                     (emit 'jmp false-label)))]))
    ; Truth case.
    (emit-label truth-label)
    (process-argument context-truth #:skip-context #t)
    ; TODO: If the argument can return how it ended, use that for a branch
    ; instead of a jump.
    (emit 'jmp done-label)
    ; False case.
    (emit-label false-label)
    (if (eq? (*opt-no-retval-form*) context-root)
        (process-argument context-false #:skip-context #t #:as 'none)
        (process-argument context-false #:skip-context #t))
    (emit-label done-label)))

(define (process-cond context-cond branches)
  (when (not (maybe-process-cond-as-table context-cond branches))
        (process-cond-as-ifs context-cond branches)))

(define (unpack-cond-body elem count single)
  (set! elem (map syntax->datum elem))
  (if (not (= (length elem) 1))
      #f
      (let ((inner (car elem)))
        (cond
         [(not (list? inner)) (if (eq? single 'immed)
                                  inner
                                  single)]
         [(< count (length inner)) (lref inner count)]
         [else #f]))))

(define (can-use-short-branch-to-false context-body)
  (if (optimization-enabled? 'if)
      (let ((body (syntax->datum context-body)))
        (let ((size (estimate-size body)))
          (< size 5)))
      #f))

(define (estimate-size body)
  (if (null? body)
      0
      (if (not (list? body))
          1
          (let ((head (car body))
                (tail (cdr body)))
            (+ (estimate-size head) (estimate-size tail))))))

(define (build-answer-table context-branches)
  (let ((min #f) (max #f) (key #f) (done #f) (action #f) (place #f)
        (branches '()) (unit #f))
    (when (eq? (syntax->datum context-branches) #f)
          (set! context-branches (datum->syntax context-branches '())))
    (set! action '())
    (for [(context-branch (syntax-e context-branches))]
         (let* ((context-condition (car (syntax-e context-branch)))
                (condition (syntax->datum context-condition))
                (op (lref condition 0))
                (lhs (lref condition 1))
                (rhs (lref condition 2))
                (body (cdr (syntax-e context-branch)))
                (context-body (datum->syntax context-branch (cons (datum->syntax context-branch 'do) body))))
           (if (and (eq? op 'eq?) (symbol? lhs) (num-like? rhs) (not done))
               (begin
                 (set! rhs (resolve-arg rhs #:even-const #t))
                 ; Match the value in the condition.
                 (cond
                  [(not min) (set! key lhs)
                             (set! min rhs)
                             (set! max rhs)]
                  [(not (eq? lhs key)) (set! done #t)]
                  [(not (eq? (+ max 1) rhs)) (set! done #t)]
                  [(eq? (+ max 1) rhs) (set! max rhs)])
                 ; Match the type of action on the rhs branch.
                 (let ((this-action #f) (this-place #f) (this-val #f))
                   (set! this-action (unpack-cond-body body 0 'lda))
                   (set! this-place (unpack-cond-body body 1 #f))
                   (set! this-val (unpack-cond-body body 2 'immed))
                   ; Action 'resource-access only takes 1 argument, not 2, so
                   ; we need to move the `place` to `val`, and nullify `place`.
                   (when (eq? this-action 'resource-access)
                         (set! this-val this-place)
                         (set! this-place #t))
                   (cond
                    [(null? action) (set! action this-action)
                                    (set! place this-place)]
                    [(not (eq? action this-action)) (set! action #f)]
                    [(not (eq? place this-place)) (set! action #f)])
                   (set! unit (list rhs this-val
                                    context-condition context-body))
                   (set! branches (append branches (list unit)))))
               (begin
                 (set! done #t)
                 (set! unit (list #f #f context-condition context-body))
                 (set! branches (append branches (list unit)))))))
    ; Only allowed actions.
    (when (not (member action '(lda set! set-pointer! resource-access)))
          (set! action #f))
    ; Determine if a jump table should be used.
    (if (and min (>= (- (+ max 1) min) 3))
        (make-hash (list (cons 'key key) (cons 'min min) (cons 'max max)
                         (cons 'action action) (cons 'place place)
                         (cons 'branches branches)))
        #f)))

(define (maybe-process-cond-as-table context-cond branches)
  (let ((answer-table #f) (lookup '()) (jumps '()) (cases '())
        (min #f) (max #f) (key #f) (action #f) (place #f))
    ; Observe each branch, figure out how possible a jump table is.
    (set! answer-table (build-answer-table
                        (datum->syntax context-cond branches)))
    (if (not answer-table)
        #f
        (begin
          ; Yes, build list of jumps/lookups and list of other branch cases.
          (set! branches (hash-ref answer-table 'branches))
          (set! min (hash-ref answer-table 'min))
          (set! max (hash-ref answer-table 'max))
          (set! key (hash-ref answer-table 'key))
          (set! action (hash-ref answer-table 'action))
          (set! place (hash-ref answer-table 'place))
          (for [(branch branches)]
               (let ((source       (lref branch 0))
                     (val          (lref branch 1))
                     (context-cond (lref branch 2))
                     (context-body (lref branch 3))
                     (a-case #f))
                 (set! a-case (datum->syntax context-cond
                                             (list context-cond context-body)))
                 (cond
                  [(or (not source) (< source min) (> source max))
                     ; Source value doens't fit in the table, regular `if`.
                     (set! cases (append cases (list a-case)))]
                  [(and action val)
                     ; Have an action to execute directly on the value.
                     (set! lookup (append lookup (list val)))]
                  [else
                     ; Code body to put in jump table.
                     (set! jumps (append jumps (list context-body)))])))
          (if action
              (process-cond-lookup-table context-cond lookup cases min max key
                                         action place)
              (process-cond-jump-table context-cond jumps cases min max key))
          #t))))

(define (process-cond-jump-table context-cond jumps cases min max source)
  (let ((jump-low-label (generate-label "cond_jump_low"))
        (jump-high-label (generate-label "cond_jump_high"))
        (not-jump-label (generate-label "cond_not_jump"))
        (cases-label (generate-label "cond_cases"))
        (done-label (generate-label "cond_done")))
    ; Check if index is in range of jump table.
    (emit-context)
    (emit 'ldy (arg->str source))
    (when (not (eq? min 0))
          (emit 'cpy (arg->str min))
          (emit 'bcc not-jump-label))
    (emit 'cpy (arg->str (+ max 1)))
    (emit 'bcs not-jump-label)
    ; Load jump address from table, push to the stack.
    (emit 'lda (format "~a,y" jump-high-label))
    (emit 'pha)
    (emit 'lda (format "~a,y" jump-low-label))
    (emit 'pha)
    (emit 'rts)
    ; Trampoline to non-jump cases.
    (emit-label not-jump-label)
    (emit 'jmp cases-label)
    ; Jumps to branches reachable by jump table.
    (let ((data-table (make-gvector)))
      (for [(jump-branch jumps)]
           (let ((jump-label (generate-label "cond_jump")))
             (gvector-add! data-table jump-label)
             (emit-label jump-label)
             (process-form jump-branch)
             (emit 'jmp done-label)))
      ; Add jump table.
      (let ((jump-low-data (format "~a_data" jump-low-label))
            (jump-high-data (format "~a_data" jump-high-label)))
        ; Low bytes.
        (emit-label jump-low-data)
        (for [(dat data-table)]
             (emit (format ".byte <(~a - 1)" dat)))
        (emit (format "~a = ~a - ~a" jump-low-label jump-low-data min))
        ; High bytes.
        (emit-label jump-high-data)
        (for [(dat data-table)]
             (emit (format ".byte >(~a - 1)" dat)))
        (emit (format "~a = ~a - ~a" jump-high-label jump-high-data min))))
    ; Other cases not handled by jump table.
    (emit-label cases-label)
    (process-cond-as-ifs context-cond cases)
    (emit-label done-label)))

; Instead of a table that contains pointers to code, here we have a table
; that contains direct values. Load those values into the relevant places.
(define (process-cond-lookup-table context-cond table cases min max
                                   source action place)
  (let ((lookup-label (generate-label "cond_lookup_val"))
        (lookup-low-label #f)
        (lookup-hi-label #f)
        (cases-label (generate-label "cond_cases"))
        (done-label (generate-label "cond_done"))
        (num-bytes 1))
    ; Check if index is in range of lookup table.
    (emit-context)
    (emit 'ldy (arg->str source))
    (when (not (eq? min 0))
          (emit 'cpy (arg->str min))
          (emit 'bcc cases-label))
    (emit 'cpy (arg->str (+ max 1)))
    (emit 'bcs cases-label)
    ; Index is in range. Load value from the table.
    (emit 'lda (format "~a,y" lookup-label))
    ; Determine where to store the value based upon the action type.
    (cond
     [(eq? action 'lda)
        ; Value stored in A, already true.
        #f]
     [(eq? action 'set!)
        ; Value stored in some place.
        (emit 'sta (arg->str place))]
     [(eq? action 'set-pointer!)
        ; Value stored in a pointer. Load additional high byte.
        (set! num-bytes 2)
        (set! lookup-hi-label (generate-label "cond_lookup_hi_val"))
        (emit 'sta (arg->str place))
        (emit 'lda (format "~a,y" lookup-hi-label))
        (emit 'sta (format "~a+1" (arg->str place)))]
     [(eq? action 'resource-access)
        ; Value is a resource triple, store in A:X:Y. Load two more bytes.
        (set! num-bytes 3)
        (set! lookup-low-label (generate-label "cond_lookup_low_val"))
        (set! lookup-hi-label  (generate-label "cond_lookup_hi_val"))
        (emit 'pha)
        (emit 'lda (format "~a,y" lookup-low-label))
        (emit 'tax)
        (emit 'lda (format "~a,y" lookup-hi-label))
        (emit 'tay)
        (emit 'pla)]
     [else
        (error (format "Unknown lookup table action ~a" action))])
    ; Jump past the data table(s).
    (emit 'jmp done-label)
    ; Convert branches into a vector, then iterate the vector, outputing data.
    (let ((data-table (make-gvector)))
      (for [(value-branch table)]
           (gvector-add! data-table value-branch))
      ; Actual lookup table. For single byte values, this is the only table.
      (let ((lookup-data (format "~a_data" lookup-label)))
        ; Value bytes.
        (emit-label lookup-data)
        (for [(dat data-table)]
             (cond
              ((eq? num-bytes 1)
                 (emit (format ".byte ~a" (resolve-arg dat))))
              ((eq? num-bytes 2)
                 (emit (format ".byte <~a" (resolve-arg dat))))
              ((eq? num-bytes 3)
                 (emit (format ".byte ~a__attr__bank"
                               (normalize-name dat))))))
        ; Size of the lookup table.
        (emit (format "~a = ~a - ~a" lookup-label lookup-data min)))
      ; Low bytes, if it exists. Only used by `resource-access`.
      (when lookup-low-label
        (let ((lookup-low-data (format "~a_data" lookup-low-label)))
          ; Value bytes.
          (emit-label lookup-low-data)
          (for [(dat data-table)]
               (if (eq? num-bytes 3)
                   (emit (format ".byte ~a__attr__low"
                                 (normalize-name dat)))
                   (emit (format ".byte >~a" (resolve-arg dat)))))
          ; Size of the lookup table.
          (emit (format "~a = ~a - ~a" lookup-low-label lookup-low-data min))))
      ; High bytes, if it exists.
      (when lookup-hi-label
        (let ((lookup-hi-data (format "~a_data" lookup-hi-label)))
          ; Value bytes.
          (emit-label lookup-hi-data)
          (for [(dat data-table)]
               (if (eq? num-bytes 3)
                   (emit (format ".byte ~a__attr__high"
                                 (normalize-name dat)))
                   (emit (format ".byte >~a" (resolve-arg dat)))))
          ; Size of the lookup table.
          (emit (format "~a = ~a - ~a" lookup-hi-label lookup-hi-data min)))))
    ; Other cases not handled by lookup table.
    (emit-label cases-label)
    (process-cond-as-ifs context-cond cases)
    (emit-label done-label)))

(define (process-cond-as-ifs context-root branches)
  (let ((accum (datum->syntax context-root 0)))
    (for [(context-branch (reverse branches))]
         (let* ((context-condition (car (syntax-e context-branch)))
                (true-case (cons (datum->syntax context-branch 'do)
                                 (cdr (syntax-e context-branch)))))
           (if (eq? (syntax->datum context-condition) 'else)
               (begin
                 (when (not (eq? (syntax->datum accum) 0))
                       (add-error "`else` can only appear as last branch"
                                  branches))
                 (set! accum true-case))
               (begin
                 (set! accum (list (datum->syntax context-branch 'if)
                                   context-condition
                                   (datum->syntax context-branch true-case)
                                   accum))))
           (set! accum (datum->syntax context-branch accum))))
    (process-form accum)))

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
    (process-body-statements body)
    (emit 'jmp start-label)
    (emit-label done-label)))

(define (process-return context-one context-two context-three)
  (when context-three
        (process-argument context-three #:preserve '(y) #:as 'ldy))
  (when context-two
        (process-argument context-two #:preserve '(x y) #:as 'ldx))
  (when context-one
        (process-argument context-one))
  (emit 'rts))

(define (process-arithmetic operator context-args)
  (let* ((context-left (car context-args))
         (bit-16 #f)
         (arg #f))
    (set! context-args (cdr context-args))
    (if (eq? (curr-bit-size) 8)
        ; 8 bit mode
        (let ((lhs (process-argument context-left)))
          (for [(context-arg context-args)]
               (set! arg (process-argument context-arg #:preserve '(a) #:as 'rhs
                                           #:skip-context #t))
               (case operator
                [(+) (emit 'clc)
                     (emit 'adc (arg->str arg))]
                [(-) (emit 'sec)
                     (emit 'sbc (arg->str arg))]
                [(>>) (let ((right (syntax->datum (car context-args))))
                        (assert right number?)
                        (for [(i (in-range right))]
                             (emit 'lsr "a")))]
                [(<<) (let ((right (syntax->datum (car context-args))))
                        (assert right number?)
                        (for [(i (in-range right))]
                             (emit 'asl "a")))])))
        ; 16 bit mode
        (let ((lhs (process-argument context-left #:bit-16 #t)))
          (for [(context-arg context-args)]
               (set! arg (process-argument context-arg #:preserve '(a) #:as 'rhs
                                           #:skip-context #t #:bit-16 #t))
               (case operator
                [(+) (emit 'clc)
                     (emit 'adc (car (arg16->str arg)))
                     (emit 'sta "_low_byte")
                     (emit 'txa)
                     (emit 'adc (cadr (arg16->str arg)))
                     (emit 'tax)
                     (emit 'lda "_low_byte")]
                [(-) (emit 'sec)
                     (emit 'sbc (car (arg16->str arg)))
                     (emit 'sta "_low_byte")
                     (emit 'txa)
                     (emit 'sbc (cadr (arg16->str arg)))
                     (emit 'tax)
                     (emit 'lda "_low_byte")]
                [(>>) (let ((right (syntax->datum (car context-args))))
                        (assert right number?)
                        (emit 'stx "_high_byte")
                        (for [(i (in-range right))]
                             (emit 'lsr "_high_byte")
                             (emit 'ror "a"))
                        (emit 'ldx "_high_byte"))]
                [(<<) (let ((right (syntax->datum (car context-args))))
                        (assert right number?)
                        (emit 'stx "_high_byte")
                        (for [(i (in-range right))]
                             (emit 'asl "a")
                             (emit 'rol "_high_byte"))
                        (emit 'ldx "_high_byte"))]))))))

(define (process-math operator context-left context-right)
  (assert operator symbol?)
  (assert context-left syntax?)
  (assert context-right syntax?)
  (let* ((lhs (process-argument context-left))
         (rhs (process-argument context-right #:preserve '(a) #:as 'rhs
                                #:skip-context #t))
         (right (syntax->datum context-right)))
    (case operator
      [(eq?) (emit 'cmp (arg->str rhs))
             (if (*opt-mode*)
                 (set-optimize! 'branch-instr 'beq)
                 (let ((is-label (generate-label "is_eq"))
                       (done-label (generate-label "done_eq")))
                   (emit 'beq is-label)
                   ; Not less-than
                   (emit 'lda "#0")
                   (emit 'jmp done-label)
                   ; Is less-than
                   (emit-label is-label)
                   (emit 'lda "#$ff")
                   (emit-label done-label)))]
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
      [(<) (if (and (*opt-mode*) (eq? right #x80))
               (set-optimize! 'branch-instr 'bpl)
               (begin (emit 'cmp (arg->str rhs))
                      (if (*opt-mode*)
                          (set-optimize! 'branch-instr 'bcc)
                          (let ((is-label (generate-label "is_lt"))
                                (done-label (generate-label "done_lt")))
                            (emit 'bcc is-label)
                            ; Not less-than
                            (emit 'lda "#0")
                            (emit 'jmp done-label)
                            ; Is less-than
                            (emit-label is-label)
                            (emit 'lda "#$ff")
                            (emit-label done-label)))))]
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
      [(>=) (if (and (*opt-mode*) (eq? right #x80))
                (set-optimize! 'branch-instr 'bmi)
                (begin (emit 'cmp (arg->str rhs))
                       (if (*opt-mode*)
                           (set-optimize! 'branch-instr 'bcs)
                           (let ((is-label (generate-label "is_gt"))
                                 (done-label (generate-label "done_gt")))
                             (emit 'bcs is-label)
                             ; Not greater-than
                             (emit 'lda "#0")
                             (emit 'jmp done-label)
                             ; Is greater-than
                             (emit-label is-label)
                             (emit 'lda "#$ff")
                             (emit-label done-label)))))]
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
               (emit-label done-label))])))

(define (process-not operator context-arg)
  (assert operator symbol?)
  (assert context-arg syntax?)
  (if (*opt-mode*)
      (begin (process-argument context-arg)
             (let ((branch-instr #f)
                   (fail-instr #f))
               (set! branch-instr (opt-codegen-branch-instr (*opt-mode*)))
               (if (eq? branch-instr 'fall-through-to-true)
                   (set-optimize! 'branch-instr 'fall-through-to-false)
                   (begin
                     (set! fail-instr (invert-condition branch-instr))
                     (set-optimize! 'branch-instr fail-instr)))))
      (begin (process-argument context-arg)
             (emit 'cmp "#1")
             (emit 'lda "#$ff")
             (emit 'adc "#0"))))

(define (mem-optimize-index index)
  ; TODO: Add comments to this function explaining what it's doing.
  ; TODO: Add more tests.
  (if (and (list? index) (eq? (length index) 3) (eq? (car index) '+))
      (let ((first (cadr index))
            (second (caddr index))
            (resolved #f)
            (var #f)
            (pass 0))
        (if (or (number? first) (const? first) (metavar? first))
            (begin (set! pass (+ pass 1))
                   (set! first (resolve-arg first))
                   (set! resolved first))
            (set! var first))
        (if (or (number? second) (const? second) (metavar? second))
            (begin (set! pass (+ pass 1))
                   (set! second (resolve-arg second))
                   (if resolved
                       (set! resolved (format "~a+~a" resolved second))
                       (set! resolved second)))
            (set! var second))
        (when (and (list? second) (eq? (car second) 'and))
          (set! pass 0))
        (if (>= pass 1)
            (list resolved var)
            #f))
      #f))

(define (process-peek context-address context-index)
  (let ((address (syntax->datum context-address))
        (index (if context-index (syntax->datum context-index) #f)))
    (emit-context)
    (if (pointer? address)
        (cond
         [(not index) (emit 'ldy "#0")
                      (emit 'lda (format "(~a),y" (arg->str address)))]
         [else (when (string=? (process-argument context-index #:skip-context #t
                                                 #:as 'ldy) "a")
                     (emit 'tay))
               (emit 'lda (format "(~a),y" (arg->str address)))])
        (cond
         [(not index) (emit 'lda (arg->str address))]
         [(number? index) (emit 'lda (format "~a+~a" (arg->str address) index))]
         [(const? index) (emit 'lda (format "~a+~a" (arg->str address)
                                            (resolve-arg index)))]
         [(metavar? index) (emit 'lda (format "~a+~a" (arg->str address)
                                              (resolve-arg index)))]
         [(eq? index 'x) (emit 'lda (format "~a,x" (arg->str address)))]
         [(eq? index 'y) (emit 'lda (format "~a,y" (arg->str address)))]
         [(mem-optimize-index index)
            (let* ((result (mem-optimize-index index))
                   (resolved (car result))
                   (var      (cadr result)))
              (if var
                  (begin (emit 'ldy (arg->str var))
                         (emit 'lda (format "~a+~a,y" (arg->str address)
                                            resolved)))
                  (begin (emit 'lda (format "~a+~a" (arg->str address)
                                            resolved)))))]
         [else (when (string=? (process-argument context-index #:skip-context #t
                                                 #:as 'ldy) "a")
                     (emit 'tay))
               (emit 'lda (format "~a,y" (arg->str address)))]))))

(define (process-poke! context-address context-arg0 context-arg1)
  (let ((address (syntax->datum context-address))
        (context-index (if context-arg0 context-arg0 #f))
        (context-value (if context-arg1 context-arg1 #f))
        (index #f))
    (when (not context-value)
          (set! context-value context-arg0)
          (set! context-index #f))
    (when context-index
          (set! index (syntax->datum context-index)))
    (if (pointer? address)
        (begin
          (process-argument context-value)
          (cond
           [(not index) (emit 'ldy "#0")
                        (emit 'sta (format "(~a),y" (arg->str address)))]
           [(eq? index 'y) (emit 'sta (format "(~a),y" (arg->str address)))]
           [(not (list? index))
              (when (string=? (process-argument context-index
                                                #:skip-context #t
                                                #:as 'ldy) "a")
                    (emit 'tay))
              (emit 'sta (format "(~a),y" (arg->str address)))]
           [else (let ((gensym (make-local-gensym! (current-scope-name))))
                   (emit 'sta gensym)
                   (when (string=? (process-argument context-index
                                                     #:skip-context #t
                                                     #:as 'ldy) "a")
                         (emit 'tay)
                         (emit 'lda gensym))
                   (emit 'sta (format "(~a),y" (arg->str address))))]))
        (begin
          (process-argument context-value)
          (cond
           [(not index) (emit 'sta (arg->str address))]
           [(number? index) (emit 'sta (format "~a+~a"
                                               (arg->str address) index))]
           [(eq? index 'x) (emit 'sta (format "~a,x" (arg->str address)))]
           [(eq? index 'y) (emit 'sta (format "~a,y" (arg->str address)))]
           [(mem-optimize-index index)
              (let* ((result (mem-optimize-index index))
                     (resolved (car result))
                     (var      (cadr result)))
                (if var
                    (begin (emit 'ldy (arg->str var))
                           (emit 'sta (format "~a+~a,y" (arg->str address)
                                              resolved)))
                    (begin (emit 'sta (format "~a+~a" (arg->str address)
                                              resolved)))))]
           [(not (list? index))
              (when (string=? (process-argument context-index
                                                #:skip-context #t
                                                #:as 'ldy) "a")
                    (emit 'tay))
              (emit 'sta (format "~a,y" (arg->str address)))]
           [else (let ((gensym (make-local-gensym! (current-scope-name))))
                   (emit 'sta gensym)
                   (when (string=? (process-argument context-index
                                                     #:skip-context #t
                                                     #:as 'ldy) "a")
                         (emit 'tay)
                         (emit 'lda gensym))
                   (emit 'sta (format "~a,y" (arg->str address))))])))))

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
                      (if (eq? (*opt-no-retval-form*) context-original)
                          (parameterize [(*opt-no-retval-form* wrapped)]
                                        (process-form wrapped))
                          (process-form wrapped)))]))

(define (process-body-statements body)
  (let ((final (if (null? body) #f (last body))))
    (for [(stmt body)]
         (if (eq? stmt final)
             (parameterize [(*opt-no-retval-form* #f)]
                           (process-form stmt))
             (parameterize [(*opt-no-retval-form* stmt)]
                           (process-form stmt))))))

(define (process-with-args func args num-needed)
  (if (< (length args) num-needed)
      (add-error (format "Need ~a arguments, only got ~a, for"
                         num-needed (length args)) (object-name func))
      (apply func args)))

(define (ensure-num-args func args num-needed)
  (if (< (length args) num-needed)
      (begin (add-error (format "Need ~a arguments, only got ~a, for"
                                num-needed (length args)) func)
             #f)
      #t))

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
            [(defenum) (process-defenum (car rest) (cdr rest))]
            [(defaddr) (apply process-defaddr (unwrap-args rest 2 0))]
            [(defvar) (apply process-defvar (unwrap-args rest 1 1))]
            [(defvarmem) (apply process-defvarmem (unwrap-args rest 2 0))]
            [(defword) (apply process-defword (unwrap-args rest 1 1))]
            [(defpointer) (apply process-defpointer (unwrap-args rest 1 0))]
            [(defsub) (process-proc 'sub (car rest) (cdr rest))]
            [(defvector) (process-proc 'vector (car rest) (cdr rest))]
            [(deflabel) (process-deflabel (car rest))]
            [(defbuffer) (apply process-defbuffer (unwrap-args rest 2 0))]
            [(defresource) (process-defresource (car rest) (cadr rest))]
            [(resource-access) (process-resource-access (car rest))]
            [(program-begin) (process-program-begin (lref rest 0))]
            [(program-complete) (process-program-complete)]
            [(push pull) (process-stack symbol (unwrap-args rest 0 3))]
            [(set!) (process-with-args process-set-bang rest 2)]
            [(set-multiple!) (process-set-multiple (lref rest 0) (lref rest 1)
                                                   (lref rest 2) (lref rest 3))]
            [(set-pointer!) (process-set-pointer! (lref rest 0) (lref rest 1))]
            [(block) (process-block (car rest) (cdr rest))]
            [(bytes) (process-bytes rest)]
            [(include) (process-include (lref rest 0))]
            [(include-binary) (process-include-binary (lref rest 0)
                                                      (lref rest 1)
                                                      (lref rest 2)
                                                      (lref rest 3))]
            [(resource-bank) (process-resource-bank (lref rest 0))]
            [(resource-bank-begin) (process-resource-bank-begin (lref rest 0))]
            [(resource-bank-complete) (process-resource-bank-complete)]
            [(resource-base-address) (process-resource-base-address
                                       (lref rest 0))]
            [(loop-down-from) (process-loop-down (car rest) (cadr rest)
                                                 (cddr rest))]
            [(loop-up-to loop) (process-loop-up (car rest) (cadr rest)
                                                (caddr rest) (cdddr rest))]
            [(repeat) (process-repeat (car rest) (cdr rest))]
            [(let) (process-let (car rest) (cdr rest))]
            [(if) (when (ensure-num-args 'if rest 3)
                        (process-if form (lref rest 0) (lref rest 1)
                                    (lref rest 2)))]
            [(cond) (process-cond form rest)]
            [(while) (process-while (car rest) (cdr rest))]
            [(do) (process-body-statements rest)]
            [(return) (process-return (lref rest 0) (lref rest 1)
                                      (lref rest 2))]
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
            [(+ - << >>)
             (process-arithmetic symbol rest)]
            [(eq? > < <s >= <= >s <=s)
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

(define (analyze-deflabel context-name)
  (assert context-name syntax?)
  (let* ((name (syntax->datum context-name)))
    (make-address! name 0)))

(define (analyze-defpointer context-name)
  (assert context-name syntax?)
  (let* ((name (syntax->datum context-name)))
    (make-pointer! name)))

(define (analyze-defvar context-name)
  (assert context-name syntax?)
  (let* ((name (syntax->datum context-name)))
    (make-variable! name #:global #t)))

(define (analyze-defvarmem context-name context-addr)
  (assert context-name syntax?)
  (assert context-addr syntax?)
  (let* ((name (syntax->datum context-name))
         (addr (syntax->datum context-addr)))
    (make-variable! name #:addr addr)))

(define (analyze-defword context-name)
  (assert context-name syntax?)
  (let* ((name (syntax->datum context-name)))
    (make-word! name)))

(define (analyze-defconst context-name context-value)
  (assert context-name syntax?)
  (let* ((name (syntax->datum context-name))
         (value (syntax->datum context-value)))
    (make-const! name value)))

(define (analyze-defenum context-name rest)
  (assert context-name syntax?)
  (let* ((name (syntax->datum context-name)))
    (for [(context-sym rest) (i (in-naturals))]
         (let* ((sym (syntax->datum context-sym)))
           (make-const! sym i)))))

(define (analyze-defaddr context-name context-value)
  (assert context-name syntax?)
  (let* ((name (syntax->datum context-name))
         (value (syntax->datum context-value)))
    (make-address! name value)))

(define (analyze-defbuffer context-name context-length)
  (assert context-name syntax?)
  (let* ((name (syntax->datum context-name))
         (length (syntax->datum context-length)))
    (make-buffer! name length)))

(define (analyze-defresource context-name context-resource-filename)
  (assert context-name syntax?)
  (let* ((name (syntax->datum context-name)))
    (make-address! name 0)))

(define (analyze-include context-filename)
  (assert context-filename syntax?)
  (when (not *include-base*)
        (error "ERROR: Include base not assigned"))
  (let* ((filename (syntax->datum context-filename)))
    (set! filename (string-append *include-base* filename))
    (analyze-file filename)))

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
            [(defvar) (analyze-defvar (car rest))]
            [(defvarmem) (analyze-defvarmem (car rest) (cadr rest))]
            [(defpointer) (analyze-defpointer (car rest))]
            [(defword) (analyze-defword (car rest))]
            [(deflabel) (analyze-deflabel (car rest))]
            [(defconst) (analyze-defconst (car rest) (cadr rest))]
            [(defenum) (analyze-defenum (car rest) (cdr rest))]
            [(defaddr) (analyze-defaddr (car rest) (cadr rest))]
            [(defbuffer) (analyze-defbuffer (car rest) (cadr rest))]
            [(defresource) (analyze-defresource (car rest) (cadr rest))]
            [(include) (analyze-include (car rest))]
            [(include-binary) (analyze-deflabel (car rest))]
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
  (emit "")
  (when (not *user-specified-org*)
    ; TODO: output default iNES header
    (emit ".org $c000")
    (emit "")))

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
    ;; Resources.
    (when (and (has-resources) *num-prg-banks*)
      (let ((fout (open-output-file *res-out-file* #:exists 'replace)))
        (append-resources (get-all-resources) (- *num-prg-banks* 2) fout)
       (close-output-port fout)))
    (emit ".pad $fffa")
    ; Output the vectors, entry points defined by hardware.
    (let ((build '()))
      (for [(entry '(irq reset nmi))]
           (if (member entry *entry-points*)
               (set! build (cons (symbol->string entry) build))
               (set! build (cons "0" build))))
      (emit (string-append ".word " (string-join build ","))))))

(define (analyze-file fname)
  (let ((f (open-input-file fname)))
    (define (loop)
      (let ((top-level-form (read-syntax fname f)))
        (when (not (eof-object? top-level-form))
              (analyze-form top-level-form)
              (loop))))
    (loop)
    (close-input-port f)))

(define (process-file filename)
  (let ((f (open-input-file filename)))
    (port-count-lines! f)
    (define (loop)
      (let ((top-level-form (read-syntax filename f)))
        (when (not (eof-object? top-level-form))
              (process-form top-level-form)
              (loop))))
    (loop)
    (close-input-port f)))

(define (generate-assembly filename)
  (define-built-ins)
  (generate-prefix)
  (process-file filename)
  (generate-suffix))

(define (read-assembly-bytes fin)
  (let ((line #f)
        (data #f)
        (result (make-vector 0)))
    (define (loop)
      (set! line (read-line fin))
      (when (not (eof-object? line))
            (when (string-startswith line ".byte ")
                  (set! data (substring line 6))
                  (set! data (string-split data ","))
                  (set! data (map dollar-hex->number data))
                  (set! data (list->vector data))
                  (set! result (vector-append result data)))
            (loop)))
    (loop)
    result))

(define (read-resource-bytes filename)
  (let ((f (open-input-file filename)) (result #f))
    (set! result (if (string-endswith filename ".asm")
                     (read-assembly-bytes f)
                     (list->vector (bytes->list (port->bytes f)))))
    (close-input-port f)
    result))

(define (flush-resource-file data fout)
  (let ((padding (make-vector (- #x4000 (vector-length data)) 0)))
    (write-bytes (list->bytes (vector->list data)) fout)
    (write-bytes (list->bytes (vector->list padding)) fout)))

(define (append-resources resources number-of-banks fout)
  (let ((bank-num 0)
        (avail #x4000)
        (res-label #f)
        (res-filename #f)
        (bytes #f)
        (bank-data (make-vector 0))
        (bank-addr #f))
    (emit "resource_segment:")
    (for [(res resources)]
         (set! res-filename (car res))
         (set! res-label (cadr res))
         (set! bytes (read-resource-bytes res-filename))
         (when (> (vector-length bytes) avail)
               ; Flush the current bank.
               (flush-resource-file bank-data fout)
               (set! bank-num (+ bank-num 1))
               (set! bank-data (make-vector 0))
               (set! avail #x4000))
         (set! bank-addr (+ #x8000 (vector-length bank-data)))
         (emit (format "~a:" res-label))
         (emit (format ".byte ~a" bank-num))
         (emit (format ".word $~x" bank-addr))
         (emit (format "res_~a__attr__bank = ~a" res-label bank-num))
         (emit (format "res_~a__attr__low  = <~a" res-label bank-addr))
         (emit (format "res_~a__attr__high = >~a" res-label bank-addr))
         (emit "")
         (set! bank-data (vector-append bank-data bytes))
         (set! avail (- avail (vector-length bytes))))
    (flush-resource-file bank-data fout)
    (set! bank-num (+ bank-num 1))
    (for [(i (in-range (- number-of-banks bank-num)))]
         (flush-resource-file (make-vector 0) fout))))

(define *include-base* #f)
(define *res-out-file* #f)

(define (assign-include-base! filename)
  (let ((p (path-only filename)))
    (if p
        (set! *include-base* (path->string p))
        (set! *include-base* ""))))

(define (compile-co2 fname out-filename)
  (set-optimization! #t)
  (set! *res-out-file* (string-replace out-filename ".asm" ".res"))
  (assign-include-base! (string->path fname))
  (analyze-file fname)
  (generate-assembly fname)
  (generate-func-memory-addresses (casla->allocations))
  (when (has-errors?)
        (display-errors)
        (exit 1))
  (let ((f (open-output-file out-filename #:exists 'replace)))
    (for [(line *result*)]
         (write-string line f)
         (newline f))
    (close-output-port f)))

(provide compile-co2)
(provide process-form)
(provide process-form-as-16-bit)
(provide analyze-form)
(provide assign-include-base!)
(provide clear-result)
(provide clear-label-id)
(provide clear-var-allocation)
(provide clear-data-segment)
(provide fetch-result)
(provide make-variable!)
(provide make-pointer!)
(provide make-word!)
(provide make-function!)
(provide make-address!)
(provide make-const!)
(provide has-errors?)
(provide first-error)
(provide clear-errors)
(provide set-optimization!)
(provide generate-func-memory-addresses)
(provide get-data-segment)
(provide build-answer-table)
