JST is a program which understands JST/whdload slaves and loads its data from hard drive.

It's been around since 1996 but was revived a few years ago, with 1.3 compat. & better whdload compatibility. Now focuses on

- whdload compatibility
- low spec machines
- keyboardless machines (using CD32 joypad redirection to keys)

So it's ideal for:

- CD32 machines with HD & fastmem (Terrible Fire boards with CF cards)
- Emulated hard disk files (on Raspberry Pi through HappiGA, where the keys are a nuisance sometimes)
- Kickstart 1.3 machines with HD & fastmem (where it's the only option to run whdload slaves)
- CDTV stock machines (where it's the only option ATM to run some whdload slaves). There's a big memory
  restriction and you can't quit without rebooting, but a lot of low-memory & small games can be run
  (examples: Arkanoid 1 & 2, Silkworm), running the games from a burned bootable CD-ROM

Howto:

- install "JST" in C subdir of the HD
- copy your whdload installs in directories: make sure that no file is XPK packed (checked at runtime)

Features:

- CUSTOMx tooltypes supported, CUSTOM, BUTTONWAIT, PAL, NTSC, NOVBRMOVE, NOCACHE, DATA
- can run multi-disk 1Meg games without flashes (Assassin)
- (*) tooltypes to redirect joypad buttons (green,blue,yellow,pause,fwd,reverse) to raw keycodes !
  ex: JOY1GREEN=0x40 (or $40) will issue "SPACE" key event when green is pressed
- (*) Mouse emulation with joypad (Earok), see virtual_mouse.txt for more details
- (*) Virtual keyboard (Earok), see virtual_keyboard_doc.txt for more details
- Kickstart 1.3 compatible !!
- quit by CD32/Amiga using the quitkey on an external keyboard, or mapping the quitkey on a joypad button (dangerous!)
- detects joystick vs cd32 joypad so no wrong "all buttons" readings when plugging a joystick
- CDTV compatible (stops CD drive before starting games to avoid unwanted interrupts)
- source code included

(*) features not available on a non-expanded A600 (needs 68010+) unless JOYPAD is explicitly set and game is suitable

Run it (through your favourite launcher):

example: JST assassin.slave CUSTOM1=1 JOY1GREEN=0x40

pressing green CD32 controller will issue rawkey 0x40 (space). Not sure of the usefulness in Assassin...

Basic options:

- SLAVE: mandatory: provide whdload slave to run
- DATA: like whdload, specify game files directory. Caution: if files aren't there, CD32Load can crash/abort
- BUTTONWAIT: if implemented in slave, will wait on title screens, loading screens...
- JOYPAD=0,1,2,3: 0: no remap, 1: remap only on port 0, 2: remap only on port 1, 3: remap both ports (2 player games)
- VK: enable virtual keyboard
- VM: enable virtual mouse
- JOYx<color/direction>: assign a raw keycode to a joypad button
- VMMODIFY,VMMODIFYBUT: see virtual mouse doc
- NOVBRMOVE: if game crashes, has strange behaviour with inputs try that (try JOYPAD=0 first!)
- NTSC/PAL: force display either in NTSC or PAL

About joypad to keyboard redirection:

This mechanism uses VBLANK redirected interrupt to scan joypad(s) and send key events if buttons or/and directions pressed.
This mechanism is reserved to 68010+ machines ATM. I'll add 68000 support later (like in CD32load)

The joypad read routine may conflict with existing slave/game read routine, specially if game/slave supports 2nd button/joypad.

- Default is JOYPAD=0: means no redirection
- JOYPAD=3 turns both joyports on for redirection
- JOYPAD=1 means that only port 0 has button redirection
 (useful when conflicts with game controls. Use JOYPAD=1 JOY0BLUE=0x19 JOY0RED=0x40 to enable P on RMB & spc on LMB on a 1-player game)
- JOYPAD=0 turns it off on both ports (required when slave/game already supports joypad buttons / 2 player mode & control conflicts)

- By default, joypad port 1 mapping is enabled like this:
  * blue => space
  * green => return
  * yellow => left ALT
  * play => P
  * bwd => F1
  * fwd => F2
  * fwd+bwd => ESC
  
- By default, joypad port 0 mapping is enabled like this:
  * blue => 2
  * green => 1
  * yellow => backspace
  * play => P
  * bwd => F3
  * fwd => F4
  * fwd+bwd => ESC

To disable a given default remapping, just set it to 0: JOY1BLUE=0x00

Note that you can quit the game by pressing all color buttons + play button simultaneously
(avoids knocking off the beer when getting up to press quitkey)

Also note that if a joystick is plugged instead of a joypad, it is detected as such, and you cannot use pause & esc
features from that joystick (hot swapping not supported). But you can still use the 2nd joypad (port 0) to pause & quit
(defaults to P and ESC for both, which work for most games, else use remap)

Check out other documentation files in the "docs" folder:

- outdated but exhaustive (but outdated) english & french guides for old versions of the program
- history.txt for changes
- bugs.txt for what's remaining to fix/code
- whdlist.txt: not exhaustive list of whdload slaves which run on JST
  (maintaining a non-working list at https://docs.google.com/spreadsheets/d/13c52fR_DKk3sX69TK6uGXLaL9UC4xLLR85iFenWYN-A/edit?pli=1#gid=0)
- virtual mouse/keyboard for those cool features that earok implemented when you don't have a mouse

Kickemu support & limitations:

- kickstart emulation (A500 1.2/1.3 and A1200/A4000/A600 3.1 ROMs) is supported quite well.
- the resload_ExNext function is not supported (yet): directory read won't work in games

68060 limitations:

- not all integer instructions are supported (only basic MOVEP stuff)
- needs 68040 dummy library + 68060.library to be able to detect the 68060 processor properly

If you have issues, try using NOCACHE (cache system is way more simplistic than whdload one)

Contributors:

- JOTD: main source code, bits & pieces integration of all parts & whdload emulation
- Ralf Huvendiek: various fixes at the time, MMU code (now discontinued), invaluable help on a program like this
- Earok: joystick directions remapping, virtual keyboard, virtual mouse ported from CD32load
- Toni Wilen: for WinUAE, keyboard interrupt generation code, 
  and insight that made this program possible and widely useable. BIG THANKS!!
- Wepl & asman & others: ReadJoyPad.s routines to safely read joypad
- Wepl: for the greatest of all: WHDLoad
- WHDLoad team: for writing so many slaves that work with JST :)
- Peter Pettersson: for fixing a small bug in a recent version
- malko from EAB for his extensive testing with kickstart 1.3, and various ways of running JST (WB/CLI, options...)
- all members of EAB for their kind replies to my sometimes noob posts (after 25 years owning an Amiga!)
  and for sometimes excellent suggestions


enjoy!
