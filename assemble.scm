#lang racket

(define (assemble source-name [rom-name #f] #:assembler [assembler #f])
  (let ((stdout #f)
        (ret-val #f))
    (when (not rom-name)
          (set! rom-name (string-replace source-name ".asm" ".nes")))
    (when (not assembler)
          (set! assembler "asm6"))
    (set! stdout (with-output-to-string
                   (lambda ()
                     (set! ret-val (system* (find-executable-path assembler)
                                            source-name rom-name "-l")))))
    (when (not ret-val)
          (error "Error:\n~a" stdout))))

(provide assemble)
