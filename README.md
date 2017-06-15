# CO2

A Lisp-like language for creating NES / Famicom software. Version 2.0 is a partial rewrite of co2 by Dave Griffiths.

![](shot.png)

CO2 takes Lispy source files and compiles them into 6502 ROMs meant to run in an NES emulator. High-level control structures like subroutines, loop, and conditionals are provided, as are low-level access to direct memory and individual 6502 CPU instructions. Minimal support for 16-bit math exists, and more will be added in the future.

Small example code, for reading the joypad button state:

    (defun (read-joypad)
      ;; need to 'strobe' register before reading
      (set! reg-joypad-0 1)
      (set! reg-joypad-0 0)
      ;; need to read multiple times until we get to the button we want
      ;; functions implicitly return the result of the last expression
      (loop n 0 8
         (poke! pad-data n (and (peek reg-joypad-0) #x1))))

See example/example.co2 for full program.

## Quick start

Requires racket, asm6 and nestopia

    $ ./build.sh example/example.co2

## Memory use

    $000 - $00f : used internally
    $010 - $03f : defvar reserves it's addresses here
    $040 - $0fb : function parameters
    $0fc        : random state register
    $100 - $1ff : stack
    $200 - $2ff : sprite control data
    $300 - ...  : free

## Fundamental stuff

General purpose 6502 code. All numbers
follow racket conventions in terms of representation: `38` is decimal,
`#xff` is hex, `#b11011011` is binary.

### (defvar name value)

define and initialise a variable:

    (defvar num-poodles 45)

### (defsub (name args ...) code)

defines a subroutine:

    (defsub (square x) (* x x))

### (defvector (name) code)

defines a system entry point (ends with "rti" opcode)

    (defvector (nmi)
        (do-game-things))

### (defconst name value)

defines a constant for use as an immediate value

     (defconst num-players 2)

### (defaddr name address)

defines a label for a specific memory location

     (defconst sprite-data #x200) ;; where the sprite control data is

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

### (byte data)

insert raw bytes into the PRG-ROM

     (asm "palette:")
     (byte "$0d,$00,$00,...")

### (text string)

insert raw text into the PRG-ROM

    (asm "mystring:")
    (text "hello world")

### (set! variable value)

assignments

    (defvar num-poodles 0)
    (set! num-poodles 100)

### (poke! base-addr [offset] value)

write to memory. base address can be a const or register 16bit address,
offset and value are normal 8 bit expressions. offset is optional,
reduces instruction count if you don't need it.

     ;; store button status
     (loop n 0 8
         (poke! pad-data n (and (peek reg-joypad-0) #x1))

### (peek base-addr [offset])

returns the contents of memory at the specified address. offset is optional, reduces instruction count if you don't need it.

     ;; store button status
     (loop n 0 8
         (poke! pad-data n (and (peek reg-joypad-0) #x1))

### (+ a b)

8 bit addition with carry

    (+ (- 1 2) (* 2 3) 8)

### (- a b)

8 bit subtraction

    (+ (- 1 2) (* 2 3) 8)

### (* a b)

8 bit multiplication

    (+ (- 1 2) (* 2 3) 8)

### (and a b)

8 bit and - can be used for masking or logical operations.

    (when (and (peek reg-joypad-0) #x1)
        (do-something))

### (or a b)

8 bit or - can be used for masking or logical operations.

    (when (or (pressed joypad-a) (pressed joypad-b))
        (do-something))

### (xor a b)

8 bit xor (eor in asm) - can be used for bit flipping, eg:

    (define (toggle-bit-zero a)
        (xor a #b00000001))

### (inc a)

increment a variable by one - maps to a single instruction

    (defvar n 0)
    (while (< n 10)
        (set-sprite-x! n (* n 10))
        (inc n))

## (dec a)

decrement a variable by one - maps to a single instruction

    (defvar n 20)
    (while (> n 10)
        (set-sprite-x! n (* n 10))
        (dec n))

## (<< a num-bits)

8 bit left shift

    (defvar num-poodles 10)
    (set! num-poodles (<< num-poodles 2)) ;; 40 = 10*4

## (>> a num-bits)

8 bit right shift

    (defvar num-poodles 10)
    (set! num-poodles (>> num-poodles 1)) ;; 5 = 10/2

# 16 bit commands

some stuff to simplify 16bit operations - very unpolished

### (set16! variable value)

a special two byte assignment from 16bit address label

    ;; must be contiguous
    (defvar addr-l 0)
    (defvar addr-h 0)

    (set16! addr-l mydata)

    ...

    (asm "mydata:")
    (bytes "0,1,2,3,4,5")

### (peek16 addr-l offset)

returns memory at address specified by two bytes

    (defvar addr-l 0)
    (defvar addr-h 0)
    (set16! addr-l mydata)

    (peek16 addr-l 4) ;; returns 4

    ...

    (asm "mydata:")
    (bytes "0,1,2,3,4,5")

### (high label)

returns the high byte of an address

    (set! addr-h (high mydata))

    ...

    (asm "mydata:")
    (bytes "0,1,2,3,4,5")

### (low label)

returns the low byte of an address

    (set! addr-l (low mydata))

    ...

    (asm "mydata:")
    (bytes "0,1,2,3,4,5")

### (+16! val-h h l)

two byte in-place addition for 16bit maths

    (defvar h 0)
    (defvar l 255)
    (+16! h 0 1)
    ;; h is now 1, l is 0

### (-16! val-h h l)

two byte in-place subtraction for 16bit maths

    (defvar h 1)
    (defvar l 9)
    (-16! h 0 10)
    ;; h is now 0, l is 255

# Experimental

NES/Famicom specific commands. These are subject to much change, while
we figure out how the architecture works and the best approach to game
programming.

## (init-system)

clears memory, resets stack pointer and stack frame, initialises
random number generator etc. needs to be called at the start of
your reset interrupt.

## (org addr)

set location of following code/data

    (org #xc000)

## (memset address value)

block writes an entire page of PRG-RAM - 256 bytes to a 16bit address

    ;; clear sprite data
    (memset sprite-data 0)



## PPU DMA commands

## (ppu-memset base-ppuaddr ppu-offset-h ppu-offset-l length value)

block writes a single value into ppu memory

    ;; write a load of tile ids to background memory
    (ppu-memset ppu-name-table-1 0 0 #x2f tile-id 0)

note: should only be called when the ppu is disabled or at the start
of vblank.

## (ppu-memcpy base-ppuaddr ppu-offset-h ppu-offset-l prg-end-offset prg-addr prg-start-offset)

copy a load of prg bytes to the ppu. dst and src addresses need to be 16bit constants/registers.

    ;; load a palette
    (asm "palette: .incbin \"example.pal\"")
    ...
    ;; copy all 32bytes of bg/sprite palette into the ppu
    (ppu-memcpy ppu-palette 0 0 #x20 palette 0))

## OAM commands

these commands write into sprite shadow ram, which is dma-ed to the
PPU every frame

### (set-sprite-x! sprite-id val)
### (set-sprite-y! sprite-id val)
### (set-sprite-id! sprite-id val)
### (set-sprite-attr! sprite-id val)

sets sprite values for the specified sprite

### (get-sprite-x sprite-id)
### (get-sprite-y sprite-id)
### (get-sprite-id sprite-id)
### (get-sprite-attr sprite-id)

gets sprite values for the specified sprite

### (get-sprite-vflip sprite-id)
### (get-sprite-hflip sprite-id)
### (set-sprite-vflip sprite-id value)
### (set-sprite-hflip sprite-id value)

macros for setting v/hflip on a sprite

## multiple sprite handling

often we are dealing with large collections of sprites, or metasprites.
these commands optimise for quickly dealing with contiguous groups of
sprites (see below for 2x2 metasprite commands)

### (add-sprites-x! sprite-id sprite-count value)
### (add-sprites-y! sprite-id sprite-count value)
### (sub-sprites-x! sprite-id sprite-count value)
### (sub-sprites-y! sprite-id sprite-count value)

add or subtract from the current sprite location

### (or-sprites-attr sprite-id sprite-count value)

binary or-s the value to the current set of sprites

## 2x2 metasprite handling

the most common size of sprites are 2x2 square, these commands produce
code optimised for this type of metasprite

## (animate-sprites-2x2! sprite-id pattern-location)

sets the 4 sprite metasprite starting at sprite-id to the patterns
starting at pattern-location. the patterns are arranged to form a
visible block for the sprite pattern data in a 16x16 grid for easier
drawing/editing. the first is the top left, second top right
(pattern-location+1), third bottom left (pattern-location+16), fourth
bottom right (pattern-location+17).

## (set-sprites-2x2-x! sprite-id x-value)

sets the top left x coordinate of the metasprite, keeping them 'stuck'
together.

## (set-sprites-2x2-y! sprite-id y-value)

sets the top left y coordinate of the metasprite, keeping them 'stuck'
together.

# registers

these are defined as constants for your convenience and enjoyment.

## ppu/oam registers

- REG-PPU-CTRL
- REG-PPU-MASK
- REG-PPU-STATUS
- REG-OAM-ADDR
- REG-OAM-DATA
- REG-PPU-SCROLL
- REG-PPU-ADDR
- REG-PPU-DATA
- REG-OAM-DMA

### apu registers

- REG-APU-PULSE1-CONTROL
- REG-APU-PULSE1-RAMP
- REG-APU-PULSE1-FT
- REG-APU-PULSE1-CT
- REG-APU-PULSE2-CONTROL
- REG-APU-PULSE2-RAMP
- REG-APU-PULSE2-FT
- REG-APU-PULSE2-CT
- REG-APU-TRI-CONTROL
- REG-APU-TRI-FT
- REG-APU-TRI-CT
- REG-APU-NOISE-ENV
- REG-APU-NOISE-FT
- REG-APU-NOISE-CT
- REG-APU-DMC-CONTROL
- REG-APU-DMC-DAC
- REG-APU-DMC-ADDR
- REG-APU-DMC-SIZE
- REG-APU-CHANNEL

### input

- REG-JOYPAD-0
- REG-JOYPAD-1

- JOYPAD-A
- JOYPAD-B
- JOYPAD-SELECT
- JOYPAD-START
- JOYPAD-UP
- JOYPAD-DOWN
- JOYPAD-LEFT
- JOYPAD-RIGHT

### ppu vram addresses

- PPU-NAME-TABLE-0
- PPU-ATTR-TABLE-0
- PPU-NAME-TABLE-1
- PPU-ATTR-TABLE-1
- PPU-NAME-TABLE-2
- PPU-ATTR-TABLE-2
- PPU-NAME-TABLE-3
- PPU-ATTR-TABLE-3
- PPU-PALETTE
- PPU-BG-PALETTE
- PPU-SPRITE-PALETTE

# program structure

follows normal NES/Famicom behaviour

    (do
      (nes-header ...)
      (org #xc000) ;; code start
      (defun ...)
      (defun ...)
      ...
      (defint (nmi) ...)
      (defint (reset)
         (init-system)
         ...)
      (defint (irq) ...)
      data
      ;; set up the interrupt vectors
      (asm ".word nmi, reset, irq")
      more data
      )

# Version 2.0

The rewrite is mostly backwards-compatible with Version 1.0, with a few superficial changes: `defconst` takes integers to define immediate values, `defaddr` is used for addresses, built-in NES registers are capitalized, `defsub` is used for subroutines and `defvector` is used for `reset` / `nmi` / `irq`. More breaking changes are coming too:

1) Moving some functions out of the core implementation and into a stdlib

2) Loop not being inclusive by default

3) Sprite support changes

4) Replacing the commonly used display-list with a better supported feature.

5) Single command-line invocation to produce a ROM with debugging info.

For semantics, CO2 treats the `A` register as the "current value" being operated on, whether it's a value to be set! into a RAM location, the conditional for an if statement, the return value from a subroutine, or the evaluation of an expression to be passed to a function call. Parameters passed to functions used fastcall, A for the 0th arg, X for the 1st, Y for the 2nd, and the system stack for more (evaluated and pushed in reverse order). There is also the option to directly manipulation X and Y with raw 6502 instructions, in which case it is up to the programmer to not evaluate any instrucitons that could clobber these registers.

Recursion is not supported. Functions store their parameter into statically determined memory addresses (calculated by looking at the total program call tree). Because these addresses are not stack based, recursion won't work.
