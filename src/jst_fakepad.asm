
;;;; fake joypad initialization

InitFakePad:
	STORE_REGS
	SET_VAR_CONTEXT

	GETVAR_L	fakepad_flag,D0
	beq	.exit

	cmp.l	#4,D0
	bcs.b	.confok

	PRINT_MSG	msg_FAKEPAD_c_wrong_conf
	JMPABS	CloseAll

.confok:
	move.l	_SysBase,A6

	lea	lowname,A1
	moveq.l	#0,D0
	JSRLIB	OpenLibrary
	move.l	D0,lowbase
	bne.b	.next

	PRINT_MSG	msg_Unable_to_open_lowle
	JMPABS	CloseAll

.next

	move.l	lowbase,A6
	add.l	#_LVOReadJoyPort+2,A6
	move.l	(A6),A1
	move.l	(2,A1),D0
	cmp.l	#'JOYP',D0
	beq	.exit			; already there (fakepad program?)

	; interrupts disabled

	move.l	_SysBase,A6
	JSRLIB	Disable

	move.l	lowbase,A1
	move.l	#_LVOReadJoyPort,A0
	move.l	#pad_tsr,D0
	JSRLIB	SetFunction

	move.l	D0,oldreadjoyport

	; interrupts enabled

	move.l	_SysBase,A6
	JSRLIB	Enable

.exit:
	RESTORE_REGS
	rts

oldreadjoyport:
	dc.l	0	; must stay at offset $A
lowbase:
	dc.l	0

;;;;; relocatable section

pad_tsr:
	bra.b	tsr_startup
	dc.b	'JOYP'	; must stay at offset $2 (patch ID)
tsr_startup:

; reads joypads
; < D0
; > D0

READJOYPORT:
	cmp.w	#0,D0
	bne	.port1

	; mouse port bypassed

	move.l	oldreadjoyport,-(A7)
	rts

.port1

	STORE_REGS	D1-A6

	cmp.w	#1,D0
	beq	.joy1

	move.l	#JP_TYPE_NOTAVAIL,D0
	bra	.mainexit

; joystick port 1

.joy1
	move.l	#JP_TYPE_GAMECTLR,D0	; joypad connected

	btst	#7,$bfe001
	bne	.nob1_1

	bset	#JPB_BUTTON_RED,D0	; fire/lmb
	
.nob1_1:

	btst	#6,potinp+$DFF000
	bne	.nob2_1

	bset	#JPB_BUTTON_BLUE,D0	; fire 2/rmb
	move.w	#$CC01,potgo+$DFF000	; reset ports
.nob2_1:

	; joystick moves

	lea	joy1dat+$DFF000,A6
	bsr	.joy_test

	STORE_REGS	D0

	move.l	old_buttonmask(pc),D0
	bsr	.button_test

	lea	old_buttonmask(pc),A0
	move.l	D0,(A0)			; relocatable memory write

	or.l	(A7),D0
	move.l	D0,(A7)
	RESTORE_REGS	D0	

.mainexit
	RESTORE_REGS	D1-A6
	rts

; other joypad buttons by keyboard emulation
; even a real joypad does not work properly on a real amiga!
; (I don't really know why!)

.button_test:
	STORE_REGS	D1/A0-A1/A4
	bsr	.btproc
	RESTORE_REGS	D1/A0-A1/A4
	rts

.btproc
	SET_VAR_CONTEXT

	; reads keyboard current pressed key (maybe it won't work)

	GETVAR_L	fakepad_flag,D0
	add.l	D0,D0
	add.l	D0,D0
	lea	qk_table(pc),A0
	move.l	(A0,D0),A0
	STORE_REGS	A0

	move.l	lowbase(pc),A6
	moveq.l	#6,D1
	JSRLIB	QueryKeys

	RESTORE_REGS	A0

	moveq.l	#0,D0

	; F1: Blue
		
	tst.w	2(A0)
	beq	.noblue
	bset	#JPB_BUTTON_BLUE,D0	; button pressed
.noblue
	; F2: Green

	addq.l	#4,A0
	tst.w	2(A0)
	beq	.nogreen
	bset	#JPB_BUTTON_GREEN,D0	; button pressed
.nogreen:
	; F3: Yellow

	addq.l	#4,A0
	tst.w	2(A0)
	beq	.noyellow
	bset	#JPB_BUTTON_YELLOW,D0	; button pressed
.noyellow:
	; F4: Play/pause

	addq.l	#4,A0
	tst.w	2(A0)
	beq	.noplay
	bset	#JPB_BUTTON_PLAY,D0	; button pressed
.noplay:
	; F5: Reverse (left ear)

	addq.l	#4,A0
	tst.w	2(A0)
	beq	.nolear
	bset	#JPB_BUTTON_REVERSE,D0	; button pressed
.nolear:
	; F6: right ear
	addq.l	#4,A0
	tst.w	2(A0)
	beq	.norear
	bset	#JPB_BUTTON_FORWARD,D0	; button pressed

.norear:
	rts

; tests joystick moves
; < A6: custom reg. of the selected joystick
; > D0: joystick bits set

.joy_test:
	STORE_REGS	D4-D6

	move.w	(A6),D4
	move.w	D4,D5
	btst	#1,D4
	beq.b	.left_off
	bset	#JPB_JOY_RIGHT,D0
	bra.b	.vert_test
.left_off:
	btst	#9,D4
	beq.b	.vert_test
	bset	#JPB_JOY_LEFT,D0
.vert_test
	lsr.w	#1,D4
	eor.w	D5,D4
	btst	#0,D4
	beq.b	.back_off
	bset	#JPB_JOY_DOWN,D0
	bra.b	.exit
.back_off
	btst	#8,D4
	beq.b	.exit
	bset	#JPB_JOY_UP,D0
.exit

	RESTORE_REGS	D4-D6
	rts

old_buttonmask:
	dc.l	0


fakepad_str_state:
	dc.l	0

qk_conf_1:
	dc.w	$50,$0,$51,0,$52,$0,$53,0,$54,0,$55,0
qk_conf_2:
	dc.w	$56,$0,$57,0,$58,$0,$59,0,$5C,0,$5F,0
qk_conf_3:
	dc.w	18,0,19,0,20,0,21,0,22,0,23,0
qk_table:
	dc.l	0,qk_conf_1,qk_conf_2,qk_conf_3

lowname:
	dc.b	"lowlevel.library",0
	even

RemoveFakePad:
	STORE_REGS
	move.l	oldreadjoyport,D0
	beq.b	.exit

	move.l	_SysBase,A6
	JSRLIB	Disable

	; restores original joypad read routine

	move.l	lowbase,A1
	move.l	#_LVOReadJoyPort,A0
	move.l	oldreadjoyport,D0
	JSRLIB	SetFunction

	move.l	_SysBase,A6
	JSRLIB	Enable

	move.l	lowbase,A1
	JSRLIB	CloseLibrary	; close lowlevel lib

.exit:
	RESTORE_REGS
	rts

; labels

msg_FAKEPAD_c_wrong_conf:	
	dc.b	"** FAKEPAD: wrong configuration",10,13,0
msg_Unable_to_open_lowle:	
	dc.b	"** Unable to open lowlevel.library!",10,13,0
	even
