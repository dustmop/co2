# co2: Lisp on NES/Famicom

A lisp for building NES/Famicom games.

![](shot.png)

More impressive screenshots to follow.

Generates 6502 assembly tested with ![asm6](https://github.com/freem/asm6f) and ![nestopia](http://nestopia.sourceforge.net/).

More documentation to follow.

Supports rough function calling (proper call stack considered too
bloaty!) so arguments work as expected until you call a function within
a function, whereapon they become clobbered. [todo: fix this]

No garbage collection, variables and direct memory peek/poke!

Everything 8 bit at the moment.

The plan is to support a bunch of NES specific calls for sprites,
backgrounds and sound.

Small example code, for reading the joypad button state:

    (defun (read-joypad button-type)
      ;; need to 'strobe' register before reading
      (set! reg-joypad-0 1)
      (set! reg-joypad-0 0)
      ;; read multiple times until we get to the button we want
      ;; function returns the result of the last expression
      (loop n 0 button-type
            (peek reg-joypad-0)))
            
See example.co2 for more info.

Memory use

    $00 - $ef : defvar reserves addresses here
    $f0 - $fe : function arguments
    $ff       : compiler working register
    $200 - $300 : sprite control data
    $300 - $7ff? : user data

## Fundamental stuff

### (defvar name value)

define and initialise a variable:

    (defvar num-trees 45)

### (defun (name args ...) code)

defines a function:

    (deffun (square x) (* x x))

### (defint (name) code)

defines a function used as an interrupt handler (ends with "rts" opcode)

    (defint (vblank) 
        (do-game-things))

### (defconst name value)

defines a constant, compiler only - so no associated memory overhead

     (defconst sprite-data "$200") ;; where the sprite control data is

### (if expr true-expr false-expr)

if this then that else the other

    (if (pressed joypad-up)
        (set! up 1)
        (set! up 0))
        
as a proper scheme it returns it's expression result, so you can also do this:

    (set! up (if (pressed joypad-up) 1 0))

### (when expr true-expr-list)

if this then that

    (when (pressed joypad-up)
        (go-up))

### (cond ((expr then-expr-list) ... (else else-expr-list)))

helper to concatenate ifs (implemented as a macro)

    (cond
      ((pressed joypad-up) (go-up)
      ((pressed joypad-down) (go-down))
      ((pressed joypad-left) (go-left))
      ((pressed joypad-right) (go-right))
      (else 0))

also returns the expression result so:

    (set! direction
        (cond
          ((pressed joypad-up) 1)
          ((pressed joypad-down) 2)
          ((pressed joypad-left) 3)
          ((pressed joypad-right) 4)
          (else 0))

works...

### (eq? a b)

returns true (1) if two bytes are equal otherwise returns false (0):

    (when (eq? (sprite-x player-sprite) 100) 
        (do-something))

### (< a b)

returns true (1) if a byte is less than another otherwise returns false
(0):

    (when (< (sprite-x player-sprite) 100) 
        (do-something))

### (> a b)

returns true (1) if a byte is greater than another otherwise returns
false (0):

    (when (> (sprite-x player-sprite) 100) 
        (do-something))

### (not a)

returns true if given false or vice versa

    (when (not (> (sprite-x player-sprite) 100))
        (do-something))

### (loop index-variable start end expr-list)

a fairly optimal loop construct for ranges (much faster than while)

    (defvar n 0)
    (loop n 0 10
        (set-sprite-x! n 100))

### (while expr expr-list)

general purpose looping

    (defvar n 0)
    (while (< n 10)
        (set-sprite-x! n (* n 10))
        (inc n))

### (do expr-list)

collect a bunch of expression together, returns result of the last one 

    (do 
      (something)
      (something-else))

### (asm assembly-string)

insert raw assembly code 

    (asm
      ".byte \"NES\",$1a" ;; number of prg-rom blocks
      ".byte $01" ;; number of chr-rom blocks
      ".byte $01" ;; rom control bytes: horizontal mirroring, no sram or trainer, mapper #0
      ".byte $00,$00" ;; filler
      ".byte $00,$00,$00,$00,$00,$00,$00,$00")

### (set! variable value)

assignments

    (defvar num-poodles 0)
    (set! num-poodles 100)

### (poke! base-addr [offset] value)

write to memory. base address is 
 
- peek
- +
- -
- *
- and
- or
- xor
- inc
- dec
- <<
- >>
- wait-vblank
- org
- memset (block writes entire page - 256 bytes)

# Experimental

while I figure out how the architecture works

## PPU commands

- ppu-memset
- ppu-memset-carry-on
- ppu-memcpy

## OAM commands

- set-sprite-x!
- set-sprite-y!
- set-sprite-id!
- set-sprite-attr!
- get-sprite-x
- get-sprite-y
- get-sprite-id
- get-sprite-attr
- add-sprite-x!
- add-sprite-y!
- sub-sprite-x!
- sub-sprite-y!
- or-sprite-attr!

