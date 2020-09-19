;==========================================================================
; read current state of joystick & joypad
;
; original code by asman with timing fix by wepl
; "getting joypad keys (cd32)"
; http://eab.abime.net/showthread.php?t=29768
;
; adapted to single read of both joysticks thanks to Girv
;
; added detection of joystick/joypad by JOTD, thanks to EAB thread:
; http://eab.abime.net/showthread.php?p=1175551#post1175551
;
; > d0.l = port number (0,1)
;
; < d0.l = state bits set as follows
;        JPB_JOY_R	= $00
;        JPB_JOY_L 	= $01
;        JPB_JOY_D	= $02
;        JPB_JOY_U	= $03
;        JPB_BTN_PLAY	= $11
;        JPB_BTN_REVERSE	= $12
;        JPB_BTN_FORWARD	= $13
;        JPB_BTN_GRN	= $14
;        JPB_BTN_YEL	= $15
;        JPB_BTN_RED	= $16
;        JPB_BTN_BLU	= $17
; < d1.l = raw joy[01]dat value read from input port
;

	IFND	EXEC_TYPES_I
	INCLUDE	exec/types.i
	ENDC
	INCLUDE	hardware/cia.i
	INCLUDE	hardware/custom.i

	BITDEF	JP,BTN_RIGHT,0
	BITDEF	JP,BTN_LEFT,1
	BITDEF	JP,BTN_DOWN,2
	BITDEF	JP,BTN_UP,3
	BITDEF	JP,BTN_PLAY,$11
	BITDEF	JP,BTN_REVERSE,$12
	BITDEF	JP,BTN_FORWARD,$13
	BITDEF	JP,BTN_GRN,$14
	BITDEF	JP,BTN_YEL,$15
	BITDEF	JP,BTN_RED,$16
	BITDEF	JP,BTN_BLU,$17

POTGO_RESET = $FF00  	; was $FFFF but changed, thanks to robinsonb5@eab

; optional call to differentiate 2-button joystick from CD32 joypad
; default is "all joypads", but when using 2-button joystick second button,
; the problem is that all bits are set (which explains that pressing 2nd button usually
; triggers pause and/or both shoulder buttons => quit game)
;
; drawback:
; once detected, changing controller type needs game restart
;
; advantage:
; less CPU time consumed while trying to read ghost buttons
;

; the OS compliant version tries to use lowlevel.library first
; (seems to be more reliable)
_detect_controller_types_lowlevel_lib:
	move.l	$4,A6
	moveq.l	#0,d0
	lea	.lowlevel_name(pc),a1
	jsr	_LVOOpenLibrary(a6)
	tst.l	d0
	beq.b	.nolow
	; note that lowlevel was available, no need to re-detect
	lea	detected_by_os(pc),a0
	st.b	(a0)
	; lowlevel is available: use it to detect joytypes
	move.l	d0,a6
	; TODO
	move.l	a6,a1
	move.l	$4,A6
	jsr	_LVOCloseLibrary(a6)
.nolow:
	rts
.lowlevel_name
	dc.b	"lowlevel.library",0
	even
	

_detect_controller_types:
	move.b	detected_by_os(pc),d0
	beq.b	.do_detect
	; don't detect if OS-compliant routine was successfully called
	rts
.do_detect
	moveq	#0,d0
	bsr		.detect
	; ignore first read
	bsr	.wvbl
	moveq	#0,d0
	bsr		.detect
	
	lea	controller_joypad_0(pc),a0
	move.b	D0,(A0)
	
	bsr	.wvbl
	
	moveq	#1,d0
	bsr		.detect
	lea	controller_joypad_1(pc),a0
	move.b	D0,(A0)
	rts

.wvbl:
	; wait for VBL
	lea	$DFF000,a0
	move.w	#$7FFF,intreq(a0)
.wait
	move.w	intreqr(a0),d0
	btst	#5,d0
	beq.b	.wait
	rts
.detect
		movem.l	d1-d6/a0-a1,-(a7)
	
		tst.l	d0
		bne.b	.port1

		moveq	#CIAB_GAMEPORT0,d3	; red button ( port 0 )
		moveq	#10,d4			; blue button ( port 0 )
		move.w	#$f600,d5		; for potgo port 0
		moveq	#joy0dat,d6		; port 0
		bra.b	.buttons
.port1
		moveq	#CIAB_GAMEPORT1,d3	; red button ( port 1 )
		moveq	#14,d4			; blue button ( port 1 )
		move.w	#$6f00,d5		; for potgo port 1
		moveq	#joy1dat,d6		; port 1

.buttons
		lea	$DFF000,a0
		lea	$BFE001,a1
		
		bset	d3,ciaddra(a1)	;set bit to out at ciapra
		bclr	d3,ciapra(a1)	;clr bit to in at ciapra

		move.w	d5,potgo(a0)

		moveq	#0,d0
		moveq	#10-1,d1	; read 9 times instead of 7. Only 2 last reads interest us
		bra.b	.gamecont4

.gamecont3	tst.b	ciapra(a1)
		tst.b	ciapra(a1)
.gamecont4	tst.b	ciapra(a1)	; wepl timing fix
		tst.b	ciapra(a1)
		tst.b	ciapra(a1)
		tst.b	ciapra(a1)
		tst.b	ciapra(a1)
		tst.b	ciapra(a1)

		move.w	potinp(a0),d2

		bset	d3,ciapra(a1)
		bclr	d3,ciapra(a1)
	
		btst	d4,d2
		bne.b	.gamecont5

		bset	d1,d0

.gamecont5	dbf	d1,.gamecont3

		bclr	d3,ciaddra(a1)		;set bit to in at ciapra
		move.w	#POTGO_RESET,potgo(a0)	;changed from ffff to ff00, according to robinsonb5@eab
		
		or.b	#$C0,ciapra(a1)	;reset port direction

		; test only last bits
		and.w	#03,D0
		
		movem.l	(a7)+,d1-d6/a0-a1
		rts
		
_joystick:

	moveq	#0,d0
	bsr	_read_joystick

		movem.l	a0,-(a7)	; put input 0 output in joy0
		lea	joy0(pc),a0
		move.l	d0,(a0)		
		movem.l	(a7)+,a0

	moveq	#1,d0
	bsr	_read_joystick

		movem.l	a0,-(a7)	; put input 1 output in joy1
		lea	joy1(pc),a0
		move.l	d0,(a0)		
	
		movem.l	(a7)+,a0

	rts	

; reads directions too, but clobbers them (who cares?)
_read_joystick:
		movem.l	d1-d7/a0-a1,-(a7)
	
		tst.l	d0
		bne.b	.port1

		moveq	#CIAB_GAMEPORT0,d3	; red button ( port 0 )
		moveq	#10,d4			; blue button ( port 0 )
		move.w	#$f600,d5		; for potgo port 0
		moveq	#joy0dat,d6		; port 0
		move.b	controller_joypad_0(pc),d2
		bra.b	.direction
.port1
		moveq	#CIAB_GAMEPORT1,d3	; red button ( port 1 )
		moveq	#14,d4			; blue button ( port 1 )
		move.w	#$6f00,d5		; for potgo port 1
		moveq	#joy1dat,d6		; port 1
		move.b	controller_joypad_1(pc),d2

.direction
		lea	$DFF000,a0
		lea	$BFE001,a1

		moveq	#0,d7

		move.w	0(a0,d6.w),d0		;get joystick direction
		move.w	d0,d6

		move.w	d0,d1
		lsr.w	#1,d1
		eor.w	d0,d1

		btst	#8,d1	;check joystick up
		sne	d7
		add.w	d7,d7

		btst	#0,d1	;check joystick down
		sne	d7
		add.w	d7,d7

		btst	#9,d0	;check joystick left
		sne	d7
		add.w	d7,d7

		btst	#1,d0	;check joystick right
		sne	d7
		add.w	d7,d7

		swap	d7
		
	;two buttons

		btst	d4,potinp(a0)	;check button blue (normal fire2)
		seq	d7
		add.w	d7,d7

		btst	d3,ciapra(a1)	;check button red (normal fire1)
		seq	d7
		add.w	d7,d7

		and.w	#$0300,d7	;calculate right out for
		asr.l	#2,d7		;above two buttons
		swap	d7		;like from lowlevel
		asr.w	#6,d7

	; read buttons from CD32 pad only if CD32 pad detected

		moveq	#0,d0
		tst.b	d2
		beq.b	.no_further_button_test
		
		bset	d3,ciaddra(a1)	;set bit to out at ciapra
		bclr	d3,ciapra(a1)	;clr bit to in at ciapra

		move.w	d5,potgo(a0)

		moveq	#8-1,d1
		bra.b	.gamecont4

.gamecont3	tst.b	ciapra(a1)
		tst.b	ciapra(a1)
.gamecont4	tst.b	ciapra(a1)	; wepl timing fix
		tst.b	ciapra(a1)
		tst.b	ciapra(a1)
		tst.b	ciapra(a1)
		tst.b	ciapra(a1)
		tst.b	ciapra(a1)

		move.w	potinp(a0),d2

		bset	d3,ciapra(a1)
		bclr	d3,ciapra(a1)
	
		btst	d4,d2
		bne.b	.gamecont5

		bset	d1,d0

.gamecont5	dbf	d1,.gamecont3

		bclr	d3,ciaddra(a1)		;set bit to in at ciapra

.no_further_button_test
		move.w	#POTGO_RESET,potgo(a0)	;changed from ffff, according to robinsonb5@eab

		swap	d0		; d0 = state
		or.l	d7,d0

		moveq	#0,d1		; d1 = raw joydat
		move.w	d6,d1
		
		or.b	#$C0,ciapra(a1)	;reset port direction

		movem.l	(a7)+,d1-d7/a0-a1
		rts

;==========================================================================

;==========================================================================

joy0		dc.l	0		
joy1		dc.l	0
controller_joypad_0:
	dc.b	$FF	; set: joystick 0 is a joypad, else joystick
controller_joypad_1:
	dc.b	$FF
detected_by_os
		dc.b	0