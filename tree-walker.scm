#lang racket

(require data/gvector)

(define func-nodes (make-hash))

(struct func-node (name params calls [memory #:mutable]))

(define (make-func-node name params calls)
  (hash-set! func-nodes name (func-node name params calls #f)))

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
                    memset ppu-memset ppu-memcpy ppu-memcpy16
                    init-system)))

(define *invocations* (make-parameter #f))

(define (walk-func-def name params body)
  (parameterize ([*invocations* (make-gvector)])
    (walk-list body)
    (let ((calls (gvector->list (*invocations*))))
      (make-func-node name params calls))))

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
          (gvector-add! (*invocations*) first))))))

(define (analyze-file f)
  (define (loop)
    (let ((top-level-form (read f)))
      (when (not (eof-object? top-level-form))
            (walk-form top-level-form)
            (loop))))
  (loop))

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

(define (display-single-func-node func-nodes n depth)
  (let* ((f (hash-ref func-nodes n))
         (name (func-node-name f))
         (params (func-node-params f))
         (calls (func-node-calls f))
         (memory (func-node-memory f)))
    (printf "~a~a ~a ~a\n" (make-string depth #\space) name params memory)
    (for ([c calls])
      (display-single-func-node func-nodes c (+ 1 depth)))))

(define (display-func-nodes)
  (let ((names (hash-keys func-nodes)))
    (for ([n names])
      (display-single-func-node func-nodes n 0))))

(let* ((fname (command-line #:args (input) input))
       (f (open-input-file fname)))
  (print-as-expression 1)
  (port-count-lines! f)
  (analyze-file f)
  (traverse-func-nodes)
  (display-func-nodes)
  (close-input-port f))
