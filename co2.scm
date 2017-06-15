#lang racket

(require racket/cmdline)
(require "compile.scm")
(require "assemble.scm")
(require "generate-nl.scm")


(define set-out (make-parameter #f))
(define set-asm (make-parameter #f))


(let* ((in-file (command-line
                 #:program "co2"
                 #:once-each
                 [("-o" "--output") out "output ROM filename" (set-out out)]
                 [("-a" "--assembly") asm "assembly generated" (set-asm asm)]
                 #:args (input) input))
       (asm-file (set-asm))
       (out-file (set-out))
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
  (printf "*** start ~a => ~a\n" in-file asm-file)
  (compile-co2 in-file asm-file)
  (printf "*** compiled ~a => ~a\n" in-file asm-file)
  ; Assemble
  (printf "*** next ~a => ~a\n" asm-file out-file)
  (assemble asm-file out-file)
  (printf "*** assembled ~a => ~a\n" asm-file out-file)
  ; Generate nl file
  (generate-nl lst-file nl-file)
  (printf "*** done\n")
  )
