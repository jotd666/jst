
	include	"jst_crc16.asm"

; Memory Copy
; This is not optimized, but it works
; Even if memory regions overlap

; < A0: start pointer
; < A1: dest pointer
; < D0: length

RelFun_CopyMem:
	movem.l d0-d3/a0/a1,-(a7)

	cmp.l	A0,A1
	beq.b	.exit		; same regions: out
	bcs.b	.copyfwd	; A1 < A0: copy from start

	tst.l	D0
	beq.b	.exit		; length 0: out

	; here A0 > A1, copy from end

	add.l	D0,A0		; adds length to A0
	cmp.l	A0,A1
	bcc.b	.cancopyfwd	; A0+D0<=A1: can copy forward (optimized)
	add.l	D0,A1		; adds length to A1 too

.copybwd:
	move.b	-(A0),-(A1)
	subq.l	#1,D0
	bne.b	.copybwd

.exit
	movem.l (a7)+,d0-d3/a0/a1
	rts

.cancopyfwd:
	sub.l	D0,A0		; restores A0 from A0+D0 operation
.copyfwd:
	move.l	A0,D1
	btst	#0,D1
	bne.b	.fwdbytecopy	; src odd: byte copy
	move.l	A1,D1
	btst	#0,D1
	bne.b	.fwdbytecopy	; dest odd: byte copy

	move.l	D0,D2
	lsr.l	#4,D2		; divides by 16
	move.l	D2,D3
	beq.b	.fwdbytecopy	; < 16: byte copy

.fwd4longcopy
	move.l	(A0)+,(A1)+
	move.l	(A0)+,(A1)+
	move.l	(A0)+,(A1)+
	move.l	(A0)+,(A1)+
	subq.l	#1,D2
	bne.b	.fwd4longcopy

	lsl.l	#4,D3		; #of bytes*16 again
	sub.l	D3,D0		; remainder of 16 division

.fwdbytecopy:
	tst.l	D0
	beq.b	.exit
.fwdbytecopy_loop:
	move.b	(A0)+,(A1)+
	subq.l	#1,D0
	bne.b	.fwdbytecopy_loop
	bra.b	.exit


; < D1: file/dir name
; < D2 (modified): basename of D1

RelFun_Basename:
	STORE_REGS	D0-D2/A0-A1
	move.l	d2,A1
	clr.b	(A1)	; default: nothing
	
	move.l	D1,D0
	bsr	RelFun_StrlenAsm
	tst.l	D0
	beq.w	.out
	
	move.l	d1,a1
	add.l	d0,a1
.loop
	subq.l	#1,a1
	cmp.l	d1,a1
	beq.b	.found2	; dirname is empty
	cmp.b	#'/',(a1)
	beq.b	.found
	cmp.b	#':',(a1)
	beq.b	.found
	bra.b	.loop
.found
	subq.l	#1,a1
.found2
	; a1 points to basename
	move.l	a1,d0
	move.l	d2,d1
	bsr	RelFun_StrcpyAsm
.out
	RESTORE_REGS	D0-D2/A0-A1
	rts
	
; < D1: dirname (modified)
; < D2: filename
; < D3: max buffer size
; > D0: !=0 if ok, =0 if buffer overflow

RelFun_AddPart:
	STORE_REGS	D1-A6
	
	move.l	D2,D0
	bsr	RelFun_StrlenAsm
	tst.l	D0
	beq.w	.nofile

	move.l	D1,D0
	bsr	RelFun_StrlenAsm
	tst.l	D0
	beq.b	.nodir

	move.l	D1,A0
	move.b	-1(A0,D0),D4	; last char of directory

	add.l	D0,D1		; end of directory string

	cmp.b	#":",D4
	beq.b	.skipslash

	cmp.b	#"/",D4
	beq.b	.skipslash

	; simple concatenation, with "/"

	move.l	D1,A0
	move.b	#"/",(A0)
	addq.l	#1,D1
.skipslash:
	move.l	D2,D0
	bsr	RelFun_StrcpyAsm

.exit
	moveq.l	#1,D0		; ok
	RESTORE_REGS	D1-A6
	rts
	
.nofile:
	bra.b	.exit
	
.nodir:
	move.l	D2,D0
	bsr	RelFun_StrcpyAsm
	bra.b	.exit
		
; *** Converts a hex number to a ascii string (size 4 $xxxx)
; in: D0: number
; in: A1: pointer to destination buffer
; out: nothing

RelFun_ShortHexToString:
	STORE_REGS
	swap	D0
	moveq.l	#3,D4		; 4 digits
	bsr	HexToString
	RESTORE_REGS
	rts

; *** Copies 2 strings

; D0: pointer on first string
; D1: pointer on second string

; out: nothing

RelFun_StrcpyAsm:
	STORE_REGS	D2
	move.l	#$FFFF,D2
	bsr	RelFun_StrncpyAsm
	RESTORE_REGS	D2
	rts

; *** Copies 2 strings

; D0: pointer on first string
; D1: pointer on second string
; D2: max length of the string

; out: nothing

RelFun_StrncpyAsm:
	STORE_REGS  d2/A0-A1

	move.l	D0,A0
	move.l	D1,A1
    subq.l  #1,d2
.copy
	move.b	(A0)+,(A1)+
	beq.b	.exit
	dbf	D2,.copy

	; terminates if end reached

	clr.b	(A1)
.exit
	RESTORE_REGS    d2/A0-A1
	rts

; *** Sets buffer in upper case

; D0: pointer on string

RelFun_ToUpperAsm:
	STORE_REGS
	move.l	D0,A0
	clr.l	D0
.loop: 
	move.b	(A0,D0.L),D1		; gets char
	beq.b	.exit

	cmp.b	#'a',D1
	bcs	.skip
	cmp.b	#'z'+1,D1
	bcc	.skip

	add.b	#'A'-'a',D1		; converts to upper
	move.b	D1,(A0,D0.L)
.skip
	addq.l	#1,D0
	bra	.loop
.exit:
	RESTORE_REGS
	rts

; *** Compares 2 strings (uc=lc)

; D0: pointer on first string
; D1: pointer on second string

; out: D0=0 if OK, -1 elsewhere

	cnop	0,4
RelFun_StrcmpAsm:
	STORE_REGS	D1-A6

	move.l	D0,D6
	move.l	D1,D7

	; *** Test String Lengths

	move.l	D6,D0
	bsr	RelFun_StrlenAsm
	move.l	D0,D2

	move.l	D7,D0
	bsr	RelFun_StrlenAsm

	cmp.l	D0,D2
	bne	.wrong

	tst.l	D2
	beq	.right	; empty -> match

	; *** Test String Contents

	move.l	D6,A0
	move.l	D7,A1
	subq.l	#1,D2

	bsr	intern_strncmp

.exit
	RESTORE_REGS	D1-A6
	rts

.right:
	moveq.l	#0,D0
	bra	.exit

.wrong:
	moveq.l	#-1,D0
	bra	.exit

; *** Compares 2 strings beginnings (uc=lc)

; D0: pointer on first string
; D1: pointer on second string
; D2: length

; out: D0=0 if OK, -1 elsewhere

	cnop	0,4
RelFun_StrncmpAsm:
	STORE_REGS	D1-A6

	move.l	D0,A0
	move.l	D1,A1

	subq.l	#1,D2

	; *** Test String Contents

	bsr	intern_strncmp

.exit
	RESTORE_REGS	D1-A6
	rts

.right:
	moveq.l	#0,D0
	bra	.exit

.wrong:
	moveq.l	#-1,D0
	bra	.exit

intern_strncmp:
.cmploop:
	move.b	(A0),D0
	cmp.b	(A1),D0
	beq	.match		; exact match

	move.b	(A0),D0
	sub.b	#'A',D0
	bcs	.wrong		; not a letter -> wrong

	move.b	(A0),D0
	sub.b	#'z'+1,D0
	bcc	.wrong		; not a letter -> wrong

	; between 'A' and 'z'

	move.b	(A0),D0
	sub.b	#'Z'+1,D0
	bcc	.lower		; a lower case

	; between 'A' and 'Z' : compare

	move.b	(A0),D0
	add.b	#'a'-'A',D0
	cmp.b	(A1),D0
	beq	.match		; case unsensitive match
	bra	.wrong		; false

.lower
	move.b	(A0),D0
	sub.b	#'a'-'A',D0
	cmp.b	(A1),D0
	beq	.match		; case unsensitive match
	bra	.wrong		; false

.match
	lea	1(A0),A0
	lea	1(A1),A1
	dbf	D2,.cmploop
	moveq.l	#0,D0
.exit
	rts

.wrong:
	moveq.l	#-1,D0
	bra	.exit

; *** Returns the length of a NULL-terminated string

; in: D0: string pointer
; out:D0: number of chars before '/0'

RelFun_StrlenAsm:
	STORE_REGS	A0
	move.l	D0,A0
	clr.l	D0
.loop: 
	tst.b	(A0,D0.L)
	beq.b	.exit
	addq.l	#1,D0
	bra	.loop
.exit:
	RESTORE_REGS	A0
	rts

; *** Converts a hex number to a ascii string (size 9 $xxxxxxxx)
; in: D0: number
; in: A1: pointer to destination buffer
; out: nothing

AbsFun_HexToString:
RelFun_HexToString:
	STORE_REGS
	moveq.l	#7,D4		; 8 digits
	bsr	HexToString
	RESTORE_REGS
	rts

; internal HexToString

HexToString:
	move.l	#$F0000000,D3
	moveq.l	#4,D2
	move.b	#'$',(A1)+	
.loop
	move.l	D0,D1
	and.l	D3,D1
	rol.l	D2,D1
	cmp.b	#9,D1
	bgt	.letter
	add.b	#'0',D1
	move.b	D1,(A1)+
	bra	.loopend
.letter
	add.b	#'A'-10,D1
	move.b	D1,(A1)+
.loopend
	addq.l	#4,D2
	lsr.l	#4,D3
	dbf	D4,.loop

	rts

; *** Converts a hex number to a ascii string of the decimal value
; in: D0: number (-655350:655350), if overflow, Nan or -Nan is returned
; in: A1: pointer to destination buffer (must be at least of size 8)
; out: nothing

RelFun_HexToDecString:
	STORE_REGS

	tst.l	D0
	bpl.b	.positive
	neg.l	D0	; D0 = -D0
	move.b	#'-',(A1)+
.positive

	cmp.l	#655351,D0
	bcs.b	.ok

	; overflow

	move.l	A1,A3
	move.b	#'N',(A3)+
	move.b	#'a',(A3)+
	move.b	#'N',(A3)+
	bra	.end
.ok
	move.l	A1,A2	; store user buffer pointer

	move.l	D0,D2
	moveq.l	#0,D3

.loop
	divu	#10,D2
	swap	D2	
	move.w	D2,D3	; D3=remainder

	add.b	#'0',D3
	move.b	D3,(A2)+	; store the number in reverse

	clr.w	D2
	swap	D2	; D2=result
	tst.l	D2
	bne.b	.loop

	; division over, now reverse the number in the buffer

	move.l	A2,A3	; store end of string

	move.l	A2,D0
	sub.l	A1,D0	; D0: number of digits
	lsr	#1,D0	; only swap on half!
	beq.b	.end	; 1 char: no swap
	subq.l	#1,D0
.reverse
	move.b	-(A2),D2
	move.b	(A1),(A2)
	move.b	D2,(A1)+
	dbf	D0,.reverse
.end
	clr.b	(A3)


	RESTORE_REGS
	rts

;*** Search and replace a longword
;
; < D0	longword to search for
; < D1	longword to replace
; < A0	start address
; < A1	end address

RelFun_HexReplaceLong:
	STORE_REGS	A0-A1/D0-D1
.srch
	cmp.l	(A0),D0
	beq.b	.found
.next
	addq.l	#2,A0
	cmp.l	A1,A0
	bcc.b	.exit
	bra.b	.srch
.found
	move.l	D1,(A0)+
	bra	.next

.exit
	RESTORE_REGS	A0-A1/D0-D1
	rts

;*** Search and replace a word
;
; < D0	word to search for
; < D1	word to replace
; < A0	start address
; < A1	end address

RelFun_HexReplaceWord:
	STORE_REGS
.srch
	cmp.w	(A0),D0
	beq.b	.found
.next
	addq.l	#2,A0
	cmp.l	A1,A0
	bcc.b	.exit
	bra.b	.srch
.found
	move.w	D1,(A0)+
	bra	.next

.exit
	RESTORE_REGS
	rts


;*** Skips colon in file name
; < A0 string
; > A0 string without ':'

RelFun_SkipColon:
	STORE_REGS	A2
	move.l	A0,A2

.loop
	cmp.b	#0,(A2)
	beq.b	.nocolon	; no colon: unchanged

	cmp.b	#':',(A2)
	beq.b	.colon
	addq.l	#1,A2
	bra.b	.loop
	
.colon
	move.l	A2,A0
	addq.l	#1,A0
.nocolon
	RESTORE_REGS	A2
.exit
	rts

;*** Search for hex data
;
; < A0	start address
; < A1	end address
; < A2	start address for hex string
; < D0  length
; > A0  address where the string was found, 0 if error

RelFun_HexSearch:
	STORE_REGS	D1/D3/A1-A2
	
.addrloop:
	moveq.l	#0,D3
.strloop
	move.b	(A0,D3.L),D1	; gets byte
	cmp.b	(A2,D3.L),D1	; compares it to the user string
	bne.b	.notok		; nope
	addq.l	#1,D3
	cmp.l	D0,D3
	bcs.b	.strloop

	; pattern was entirely found!

	bra.b	.exit
.notok:
	addq.l	#1,A0	; next byte please
	cmp.l	A0,A1
	bcc.b	.addrloop	; end?
	sub.l	A0,A0
.exit:
	RESTORE_REGS	D1/D3/A1-A2
	rts

; *** Waits for LMB while the screen is full of colors
; *** Useful to see if a point is reached

RelFun_WaitMouse:
	STORE_REGS	D0
.1
	move.w	d0,$dff180
	addq.w	#3,D0
	btst	#6,$bfe001
	bne	.1

.2
	btst	#6,$bfe001
	beq	.2		; waits for release

	RESTORE_REGS	D0
	rts



; *** Waits for LMB while the screen is full of colors
; *** Useful to see if a point is reached
; *** interrupts are enabled

RelFun_WaitMouseInterrupt:
	STORE_REGS

	lea	$DFF000,A6
	move.w	SR,D1
	move.w	intenar(A6),D2
	or.w	#$8000,D2

	move.w	#$2000,SR
	move.w	#$C028,intena(A6)


.1
	move.w	d0,$dff180
	addq.w	#7,D0
	btst	#6,$bfe001
	bne	.1

.2
	btst	#6,$bfe001
	beq	.2		; waits for release

	move.w	D1,SR
	move.w	D2,intena(A6)

	RESTORE_REGS
	rts

; *** returns the length of the file once decrunched
; in: A0: pointer on the memory zone
; out: D0: decrunched length (-1 if not rnc)

RelFun_RNCLength:
	moveq.l	#-1,D0
	CMPI.L         #$524E4301,(A0)	; RNC\01 tag.
	beq	.ok			; not a rnc01 file
	CMPI.L         #$524E4302,(A0)	; RNC\02 tag.
	bne	.exit			; not a rnc file
.ok
	move.l	4(A0),D0
.exit
	rts


; *** returns the length of the file once decrunched
; in: A0: pointer on the memory zone
; out: D0: decrunched length (-1 if not rnc)

RelFun_ATNLength:
	moveq.l	#-1,D0
	CMPI.L         #'ATN!',(A0)	; ATN! tag.
	beq	.ok			; not a rnc01 file
	CMPI.L         #'IMP!',(A0)	; IMP! tag.
	bne	.exit			; not a rnc file
.ok
	move.l	4(A0),D0
.exit
	rts

; *** Decrunch a TPWM crunched file (TPWM)
; <   A0 start of source/dest buffer
; >   D0=0 if OK, -1 else

RelFun_TPWMDecrunch:
	INCLUDE	"TPWMDecrunch.asm"


; *** Decrunch a ATN! file
; in: A0: crunched buffer
;     A1: decrunched dest (may be the same)

; out: D0=0 if not a ATN file

; *** Decrunch a Imploder crunched file (IMP!)
; >   D0=0 if OK, -1 else
; in: A0: crunched/decrunched buffer
    
RelFun_ImploderDecrunch:
	move.l	A0,A1
RelFun_ATNDecrunch:
	STORE_REGS
	bsr	.dec
	RESTORE_REGS
	rts

.dec:
	include	"atndecrunch.asm"


; *** Decrunches a RNC type 1 file (Rob Northen Cruncher, header: RNC\01)
; *** and decrypts it
; *** Ripped from Walker game

; in: A0: crunched buffer start
; in: A1: destination (may be the same !!)
; in: D0: 16-32 bit key

RNCDecrunchEncrypted:
	include "RNC1Decrunch.asm"


; *** Decrunches a RNC type 1/2 file (Rob Northen Cruncher, header: RNC\01, RNC\02)
; *** Ripped from SSBOOT/HOOK (Cannon Fodder2, SWOS)

; in: A0: crunched buffer start
; in: A1: destination (may be the same !!)

; This type of cruncher is very heavily used in lots of games
; from EOA, Team 17, Renegade, Akklaim, and lots of others.

RNCDecrunch:
	include	"RNC12Decrunch.asm"
	
; *** waits using the beam register (thanks Harry)

RelFun_BeamDelay:
.loop1
	tst.w	d0
	beq.b	.exit	; don't wait
	move.w  d0,-(a7)
        move.b	$dff006,d0
.loop2
	cmp.b	$dff006,d0
	beq.s	.loop2
	move.w	(a7)+,d0
	dbf	d0,.loop1
.exit
	RTS

; *** waits for blitter operation to finish

RelFun_WaitBlit:
	TST.B	dmaconr+$DFF000
	BTST	#6,dmaconr+$DFF000
	BNE.S	.wait
	RTS
.wait
	TST.B	$BFE001
	TST.B	$BFE001
	BTST	#6,dmaconr+$DFF000
	BNE.S	.wait
	TST.B	dmaconr+$DFF000
	RTS

; *** Reset the CIAs for the keyboard
; *** Thanks to Alain Malek for this piece of code

RelFun_ResetCIAs:
	move.b  #$c0,$bfd200            ;reinit CIAs
	move.b  #$ff,$bfd300            ;for
	move.b  #$03,$bfe201            ;KB
;	move.b  #$7f,$bfec01
	move.b  #$00,$bfee01
	move.b  #$88,$bfed01

	tst.b	$bfdd00			; acknowledge CIA-B Timer A interrupt
	tst.b	$bfed01			; acknowledge CIA-A interrupts

	bsr	AckKeyboard

	rts

AckKeyboard:
	STORE_REGS	D0
	bset	#$06,$BFEE01
	moveq.l	#3,D0
	bsr	RelFun_BeamDelay
	bclr	#$06,$BFEE01
	RESTORE_REGS	D0
	rts

; parse integer from string
; < A0: pointer on C string
; > D0: value
; > D1: -1 if ok, position of the string if error
parse_integer:
    movem.l  d2/d3,-(a7)
    ; go to end of string
    moveq.l #-1,d1
.loop1
    addq.l  #1,d1
    tst.b   (a0,d1.w)
    bne.b   .loop1
    ; d1 is the number of chars
    moveq.l #0,d0
    moveq.l #0,d2
    subq.l  #2,d1   ; 10th power minus 1
.loop2
    move.b  (a0)+,d2
    beq.b   .out
    
    cmp.b   #' ',d2
    beq.b   .skip
    sub.b   #'0',d2
    bcs.b   .error
    cmp.b   #10,d2
    bcc.b   .error
    move.w  d1,d3
    bmi.b   .doadd
.muloop
    mulu    #10,d2
    dbf d3,.muloop
.doadd
    add.l   d2,d0
.skip
    subq.l  #1,d1
    bra.b   .loop2
.out    
    movem.l  (a7)+,d2/d3
    rts
.error
    moveq.l #0,d0
    bra.b   .error
    
