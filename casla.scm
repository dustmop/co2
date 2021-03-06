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


; Call-graph Associated Static Lexical Allocation

; During compilation, for each function body, collect the names of functions
; that get called. Use this list of function calls to build a call graph.
;
; Traverse the graph, and starting from the leaves, assign memory addresses
; to local variables and parameters. Each caller's starting memory address
; is equal to the maximum of the address used by its callees.

(define *func-nodes* (make-hash))

(struct func-node (name locals calls [memory #:mutable]
                        [in-call #:mutable] [under-call #:mutable]
                        [data-dep #:mutable]))

; Keep track of function name, its locals and parameters, and its callees.
; Called by compiler after it finishes processing each function body.
(define (make-func-node! name locals calls)
  (hash-set! *func-nodes* name (func-node name locals calls #f 0 #f #f)))

; Recursively resolve each function and its callees. Implicitly builds a total
; call graph of the entire program.
(define (resolve-func-node-memory n)
  (let* ((f (hash-ref *func-nodes* n))
         (name (func-node-name f))
         (locals (func-node-locals f))
         (calls (func-node-calls f))
         (memory (func-node-memory f)))
    (if (number? memory)
        memory ; return early
        (begin (let ((total 0)
                     (curr 0))
                 (for [(c calls)]
                   (set! curr (resolve-func-node-memory c))
                   (when (> curr total)
                     (set! total curr)))
                 (set! total (+ total (length locals)))
                 (set-func-node-memory! f total)
                 total)))))

(define (casla->allocations)
  (let ((names (hash-keys *func-nodes*))
        (result (make-gvector)))
    ; Traverse the call-graph, assigning memory spaces.
    (for [(n names)]
      (resolve-func-node-memory n))
    ; For each function, allocate addresses to its locals and parameters.
    (for [(n names)]
         (let* ((f (hash-ref *func-nodes* n))
                (name (func-node-name f))
                (locals (func-node-locals f))
                (calls (func-node-calls f))
                (memory (func-node-memory f))
                (k (- memory (length locals))))
           (for [(l locals) (i (in-naturals))]
                (gvector-add! result (list name l (+ k i))))))
    (gvector->list result)))

(define (show-call-graph func-name indent)
  (printf "~a~a" (make-string indent #\Space) func-name)
  (let* ((f (hash-ref *func-nodes* func-name))
         (name (func-node-name f))
         (locals (func-node-locals f))
         (calls (func-node-calls f))
         (memory (func-node-memory f)))
    (if (> (length calls) 4)
        (printf " -> (~a ~a ~a ~a ...)\n" (list-ref calls 0) (list-ref calls 1)
                (list-ref calls 2) (list-ref calls 3))
        (printf " -> ~a\n" calls))
    (for [(fcall calls)]
         (show-call-graph fcall (+ indent 1)))))

; Count number of calls to each function
; Count cummulative calls for each function (sum of children)
; size in bytes? (no way to do this right now)
; data-dependency needs to be considered

(define (analyze-call-graph)
  (calc-in-calls 'reset (make-hash))
  (calc-under-calls 'reset)
  (calc-data-dep 'reset (make-hash))
  (show-analyzed 'reset 0 (make-hash)))

;
(define (calc-in-calls func-name seen)
  (when (not (hash-has-key? seen func-name))
    (hash-set! seen func-name #t)
    (let* ((f (hash-ref *func-nodes* func-name))
           (calls (func-node-calls f))
           (child #f))
      (for [(fcall calls)]
           (set! child (hash-ref *func-nodes* fcall))
           (set-func-node-in-call! child (+ 1 (func-node-in-call child)))
           (calc-in-calls fcall seen)))))

;
(define (calc-under-calls func-name)
  (let* ((f (hash-ref *func-nodes* func-name))
         (calls (func-node-calls f))
         (total (func-node-under-call f)))
    (when (not total)
      (set! total 0)
      (for [(fcall calls)]
           (set! total (+ total (calc-under-calls fcall))))
      (set! total (+ total (length calls)))
      (set-func-node-under-call! f total))
    total))

(define (calc-data-dep func-name seen)
  (when (not (hash-has-key? seen func-name))
    (hash-set! seen func-name #t)
    (let* ((f (hash-ref *func-nodes* func-name))
           (calls (func-node-calls f))
           (child #f)
           (depends-on-data #f)
           (depends-load-far-pointer #f))
      (for [(fcall calls)]
           (calc-data-dep fcall seen)
           (when (eq? fcall 'load-far-pointer)
                 (set! depends-load-far-pointer #t))
           (when (eq? fcall 'mmc1-prg-bank)
                 (set! depends-on-data #t))
           (set! child (hash-ref *func-nodes* fcall))
           ;(when (eq? (func-node-data-dep child) 1)
           ;      (set! depends-on-data #t))
           )
      (when depends-load-far-pointer
            (set! depends-on-data #f))
      (set-func-node-data-dep! f (if depends-on-data 1 0)))))

;
(define (show-analyzed func-name indent seen)
  (when (not (hash-has-key? seen func-name))
    (hash-set! seen func-name #t)
    (let* ((f (hash-ref *func-nodes* func-name))
           (calls (func-node-calls f))
           (in-call (func-node-in-call f))
           (under-call (func-node-under-call f))
           (data-dep (func-node-data-dep f)))
      (printf "~a in: ~a under: ~a~a\n" func-name in-call
              under-call (if (eq? data-dep 1) " DEPENDS-ON-DATA" ""))
      (for [(fcall calls)]
           (show-analyzed fcall (+ indent 1) seen)))))

(provide make-func-node!)
(provide casla->allocations)
(provide show-call-graph)
(provide analyze-call-graph)
