#lang racket

(require racket/cmdline)
(require "compile.scm")
(require "assemble.scm")
(require "generate-nl.scm")
(require "casla.scm")


(define set-out (make-parameter #f))
(define set-aout (make-parameter #f))
(define set-assembler (make-parameter #f))
(define set-call-graph (make-parameter #f))
(define set-analyze-calls (make-parameter #f))

(let* ((in-file (command-line
                 #:program "co2"
                 #:once-each
                 [("-o" "--output") out "output ROM filename" (set-out out)]
                 [("-a" "--assembly") aout "assembly generated" (set-aout aout)]
                 [("--asm") asm "assembly generated" (set-assembler asm)]
                 [("--call-graph") "call graph" (set-call-graph #t)]
                 [("--analyze-calls") "analyze calls" (set-analyze-calls #t)]
                 #:args (input) input))
       (asm-file (set-aout))
       (out-file (set-out))
       (assembler (set-assembler))
       (show-cg (set-call-graph))
       (analyze-calls (set-analyze-calls))
       (lst-file #f)
       (nl-file #f))
  (when (not out-file)
        (error "Expected, -o <output>"))
  (when (not asm-file)
        (set! asm-file (format ".b/~a"
                               (file-name-from-path
                                (string-replace in-file ".co2" ".asm"))))
        (when (not (directory-exists? ".b"))
              (make-directory ".b")))
  (set! lst-file (string-replace asm-file ".asm" ".lst"))
  (set! nl-file (string-replace out-file ".nes" ".nes.0.nl"))
  ; Compile
  (printf "co2: compile ~a => ~a\n" in-file asm-file)
  (compile-co2 in-file asm-file)
  ; Maybe show the call graph, then exit.
  (when show-cg
        (printf "========================================\n")
        (show-call-graph 'reset 0)
        (exit 1))
  ; Maybe analyze the call graph.
  (when analyze-calls
        (printf "========================================\n")
        (analyze-call-graph)
        (exit 1))
  ; Assemble
  (printf "co2: assemble ~a => ~a\n" asm-file out-file)
  (if assembler
      (assemble asm-file out-file #:assembler assembler)
      (assemble asm-file out-file))
  ; Generate nl file
  (printf "co2: gen-nl ~a => ~a\n" lst-file nl-file)
  (generate-nl lst-file nl-file)
  (printf "co2 done\n"))
