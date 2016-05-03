# co2: Lisp on NES/Famicom

A lisp for building NES/Famicom games.

![](shot.png)

More impressive screenshots to follow.

Generates 6502 assembly tested with ![asm6](https://github.com/freem/asm6f) and ![nestopia](http://nestopia.sourceforge.net/).

More documentation to follow.

Supports rough function calling (proper call stack considered too
bloaty!) so arguments work as expected until you call a function within
a function, whereapon they become clobbered.

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

Commands:

- defvar
- defun
- defint (interrupt)
- defconst 
- if
- when
- cond
- eq?
- <
- >
- not
- loop
- do
- asm
- set!
- poke!
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
- memset
- ppu-memset
- ppu-memset-carry-on
- ppu-memcpy
- set-sprite-x!
- set-sprite-y!
- set-sprite-id!
- set-sprite-attr!

