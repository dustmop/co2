# CO2

__WARNING, THIS CODE IS EXPERIMENTAL AND UNSTABLE. USE WITH CAUTION__

A lispy language for creating NES / Famicom software. Based upon original work by [Dave Griffiths](https://gitlab.com/nebogeo/co2).

![](shot.png)

CO2 takes Lispy source files and compiles them into 6502 ROMs meant to run in an NES emulator. It includes high-level control structures like subroutines, loops, and conditionals, as well as low-level access to memory and individual 6502 CPU instructions.

Though it was used for (and developed along with) the game What Remians, CO2 should be considered __highly experimental__. It is lacking many features, has poor errors, may rarely generate invalid code, lacks many standard language abstractions, and in general is somewhat unstable. Nevertheless, it still has some real benefits if you are willing to deal with its rough spots.

Example:

```
  (defun (read-joypad)
    ; Latch the controller
    (set! REG-JOYPAD-0 1)
    (set! REG-JOYPAD-0 0)
    ;; Read controller 8 times, put each bit into `joypad-data`.
    (loop-down-from x 8
      (lsr (lda REG-JOYPAD-0))
      (rol joypad-data)))
```

## Quick start

Requires `racket` and `asm6`

    $ racket co2.scm -o rom.nes example/example.co2

## Philosophy

CO2 is meant to enable high-level structured programming, while still providing access to low level facilities. It maintains some useful knowledge of ROM banks, in order to make it easier to develope large scale games. It emphasizes writing fairly performant code, at the expense of some safety. In some cases, it has slightly leaky abstractions (such as putting state into the X and Y registers) that the compiler does not insulate you from. Basically: "you have to know what you're doing".

The goal is to allow development of software that doens't absolutely require the performance of raw assembly, in a manner that's convenient for experimentation, and understanding.

## Features and benefits

### local var allocation

Variables declared as subroutine parameters or by using `let` are statically allocated using a "compiled stack", calculated by analyzing the program's entire call graph. This means scopes will not use memory locations used by any inner scopes, but are free to use them from sibling scopes. This ensures efficient variables lookups, while also not wasting RAM. However, it does mean that recursion is not supported.

### farcall

Functions "know" what bank they exist in. This enables conveniently calling functions in other banks, though of course this much less efficient than a normal function call.

### cross bank safety

Similarily, it is an error to call a function in a non-accessible bank, detected statically by the compiler. This saves deleopment time by catching errors early.

### resources

Resources provide a simple mechanism to include binary data, such that it's cheap and efficient to load a pointer and bank number to that data using a zero-cost (compile-time only) handle. No need to manually keep track of where data is stored in ROM.

### source-level debugging

When using fceux, CO2 produces *.nl files that easily enable source level debugging of the original high-level co2 files within the debugger window.

### efficient cond

The `cond` form can compile properly structured conditionals into lookup tables and jump tables, while still looking like high-level conditionals.

### optimizations

A very simple peephole optimizer and tail call optimizer help squeek out ROM bytes and CPU cycles without any extra work.

## Partially working features

### macros

Macros are available, but lack many features, as they were implemented late in development. They take advantage of racket's `namespaces` and `eval`, yielding a very simple implementation.

### 16-bit math

Some 16-bit math exists, but support is lacking. Ideally, types would automatically promote and demote, which proper errors from the compiler when the user is doing something wrong.

### cond

Though `cond` will sometimes compile to efficient tables, it could use more work. Ideally, it would always use binary search over sorted values, and tables with small gaps whereever possible.

## Future features

### timing aware code blocks

Similar to the [Dollhouse Demo](https://ahefner.livejournal.com/20528.html), it would be nice to have a construct that declares blocks which are aware of their own timing. 

### pluggable modules

Would be beneficial to have an easier way to include third-party code, like Famitracker's NSF Driver, or the TV Detection Code.

# Memory usage

```
  $000 - $007 : used internally
  $010 - $0ff : defvar, function parameters, local vars
  $100 - $1ff : stack
  $200 - $2ff : sprite data
  $300 - ...  : available
```

# Documentation

TODO(dustmop): Fill the rest of this documentation out.

## Defining functions & data

### defsub

```
(defsub (my-func param-a param-b)
  ; body goes here
  )
```

Defines a subroutine. Can be called with `(my-func val-a val-b)`

### defvector

Same as `defsub`, but for defining `reset` and `nmi`.

### deflabel

```
(deflabel my-table)
```

Define a label, such as a data table in ROM.

### bytes

```
(bytes 1 2 3)
```

Define raw bytes in ROM.

### words

```
(words 6502 1234)
```

Define word sized (16-bit) values in ROM.

### defconst

```
(defconst starting-health 30)
```

Define a constant.

### defenum

```
(defenum 'days-of-week monday tuesday wednesday)
```

Define constants that increase monotomically in representation.

### defmacro

TODO

## Memory definitions

### defvar

```
(defvar my-var)
```

Define a byte sized var. Defaults to zeropage.

### defvarmem

```
(defvarmem another-var #x310)
```

Define a byte sized var at an arbitrary memory location.

### defword

```
(defword my-word)
```

Define a word sized (16-bit) var.

### defpointer

```
(defpointer my-pointer)
```

Define a pointer. Must be used in zeropage. See `set-pointer!` and `peek`.

### defbuffer
### defaddr

## NES specific directives

### nes-header
### memory-map-global-state
### init-system

## Resources and banks

### defresource
### resource-access
### program-bank
### program-complete
### resource-bank
### resource-bank-complete

## Assignments

### let

```
(let ((n) (m))
  ; body ...
  )
```

Define local variables that are only visible in this scope. Initial values are undefined.

### set!

```
(set! n 7)
```

Assign a value to a variable.

### set-multiple!

```
(set-multiple! n m p (func-with-multiple-returns))
```

Assigns values from a multiple-return-value function to multiple variables. See `return`.

### set-pointer!
### push
### pull

## Control structures

### if
### when
### cond
### while
### do
### block
### loop
### loop-down-from
### loop-up-to
### repeat
### return

```
(return 3)
```

Return from the current subroutine. The argument will be used as the return value from this subroutine. Up to three values can be returned at a time, these should be handled by the caller by using `set-multiple!`.

```
(defsub (func-with-multiple-returns)
  (return 23 45 67))
```

### catch
### unwind
### farcall

## Low level memory

### peek
### poke
### high
### low

## Math

### +
### -
### <<
### >>
### eq?
### >
### <
### >=
### <=
### *
### /
### mod
### not

## Includes

### include
### include-binary

## PPU utilities

### ppu-load
### ppu-memset
### ppu-memcpy
### ppu-memcpy16

## Sprite utilities

### set-sprite!
### get-sprite

## Memory helpers    

### memset
### memcpy
### scale16

## Raw assembly

### asm

```
(asm "lda #1")
```

Emit raw assembly.
                        
# 6502 Instructions

```
adc cmp cpx cpy eor sbc and ora xor
lda ldx ldy sta stx sty asl lsr rol ror
bit beq bcc bcs bne bmi bpl bvc bvs jmp
clc cld cli clv dex dey inx iny nop pha
pla rts sec tax tay tsx txa txs tya jsr
```

All standard 6502 instructions can be used directly. Expressions are not allowed for these usages.

## Addressing modes:

```
(lsr a)
(lda 7)
(lda n)
(lda (addr #x421))
(lda (addr #x421) x)
```
