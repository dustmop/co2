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
- wait-vblank
- org
- memset
- ppu-memset
- ppu-memset-carry-on
- ppu-memcpy
