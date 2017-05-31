#lang racket

(define (leaf-asm-gen? elem)
  (member elem '(asm byte text org)))

(define (data-definition? elem)
  (member elem '(nes-header defaddr defimmed defvar)))

(define (func-definition? elem)
  (member elem '(defun defint)))

(define (built-ins? elem)
  (member elem '(if when while loop + - +16! -16! < > << >>
                    and or eq? not inc dec _rnd
                    or-sprites-attr! animate-sprites-2x2!
                    get-sprite-x get-sprite-y
                    set-sprites-2x2-x! set-sprites-2x2-y!
                    set-sprite-x! set-sprite-y! set-sprite-id! set-sprite-attr!
                    set! set16! peek poke! peek16
                    memset ppu-memset ppu-memcpy ppu-memcpy16)))

(define (walk-func-def name params body)
  (printf "Defining ~a {\n" name)
  (walk-list body)
  (printf "}\n\n"))

(define (walk-list ls)
  (for ([elem ls])
    (walk-form elem)))

(define (walk-cond ls)
  (for ([elem ls])
    (let ((check (car elem))
          (stmt  (if (null? (cdr elem)) '() (cadr elem))))
      (walk-form check)
      (walk-form stmt))))

(define (walk-form form)
  (if (or (not (list? form)) (null? form))
      #f
      (let ((first (car form))
            (rest (cdr form)))
        (cond
         ([eq? first 'do] (walk-list rest))
         ([eq? first 'cond] (walk-cond rest))
         ([data-definition? first] #f)
         ([func-definition? first] (walk-func-def (caar rest)
                                                  (cdar rest)
                                                  (cdr rest)))
         ([built-ins? first] (walk-list rest))
         ([leaf-asm-gen? first] #f)
         (else
          (printf "Call ~a\n" first))))))

(define (analyze-file f)
  (define (loop)
    (let ((top-level-form (read f)))
      (when (not (eof-object? top-level-form))
            (walk-form top-level-form)
            (loop))))
  (loop))

(let* ((fname (command-line #:args (input) input))
       (f (open-input-file fname)))
  (port-count-lines! f)
  (analyze-file f)
  (close-input-port f))
