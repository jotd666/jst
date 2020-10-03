; *** JOTD HD Startup utility library
; *** Copyright Jean-Francois Fabre 1996-1998
; 
; *** Sprite degrade part Copyright Bert Jahn 1996

	XDEF	AbsFun_Enhance
	XDEF	AbsFun_Priv_EnhanceCpu
	XDEF	AbsFun_Priv_EnhanceGfx
	XDEF	AbsFun_DegradeCpu
	XDEF	AbsFun_DegradeGfx
	XDEF	AbsFun_Degrade
	XDEF	AbsFun_FlushCachesSys
	XDEF	AbsFun_Kick37Test
	XDEF	AbsFun_KickVerTest
	XDEF	AbsFun_Priv_SetClockLoad
	XDEF	AbsFun_Priv_GetVBR
	XDEF	AbsFun_Priv_SendCDTVCommand

	XDEF	user_cacheflags
	XDEF	user_cachemask

	XREF	_GfxBase
	XREF	_DosBase
	XREF	RelocErr
	XREF	gene_variables
	XREF	AbsFun_AllocateTheMemory
	XREF	AbsFun_FreeTheMemory
	XREF	TaskCheck
	XREF	RelFun_ResetCIAs
	XREF	RelFun_WaitMouse

HDSTARTUP=1

;; Code below in jst_util.asm
;;ResetCIAs:
;;	move.b  #$c0,$bfd200            ;reinit CIAs
;;	move.b  #$ff,$bfd300            ;for
;;	move.b  #$03,$bfe201            ;KB
;;	move.b  #$00,$bfee01
;;	move.b  #$88,$bfed01

;;	tst.b	$bfdd00			; acknowledge CIA-B Timer A interrupt
;;	tst.b	$bfed01			; acknowledge CIA-A interrupts

;;	bsr	AckKeyboard

;;	rts

;;AckKeyboard:
;;	bset	#$06,$BFEE01
;;	STORE_REGS	D0
;;	moveq.l	#1,D0
;;	bsr	RelFun_BeamDelay
;;	RESTORE_REGS	D0
;;	bclr	#$06,$BFEE01
;;	rts

	; *** this code is on its own (which is the case :-) )
	
_SysBase = 4


	include	"jst_libs.i"
	include	"gp_macros.i"
	include	"jstabs_macros.i"
	include	"jst_rel.i"


; *** Restore degraded things: Display, Caches, VBR...

; internal use (in InGameExitAbs)

	move.l	D0,A1

AbsFun_Enhance:
	; the cpu stuff

	bsr	AbsFun_Priv_EnhanceCpu

	; now the gfx stuff

	bsr	AbsFun_Priv_EnhanceGfx

	rts

AbsFun_Priv_EnhanceCpu:
	STORE_REGS
	SET_VAR_CONTEXT

	tst.b	degraded_cpu
	beq	.skipec

	TSTVAR_L	execute_flag
	beq.b	.skipvbr
	TSTVAR_L	system_vbr
	beq.b	.skipvbr	; don't touch if already at 0

	move.l	_SysBase,A6
	JSRLIB	Disable

	; restores VBR value

	GETVAR_L	system_vbr,D0
	bsr	SetVBR

	; restores the zero page contents from the buffer

	lea	zeropage_buffer(pc),A0
	lea	$8.W,A1
	move.l	#$F8,D0
	JSRGEN	CopyMem

	; flushes caches

	JSRABS	FlushCachesSys

	move.l	_SysBase,A6
	JSRLIB	Enable

.skipvbr
	; restores caches

	move.l	oldcacheset(pc),D0
	moveq.l	#-1,D1
	bsr	EnaCaches

	; ok not degraded anymore

	clr.b	degraded_cpu

.skipec
	RESTORE_REGS
	rts


AbsFun_Priv_EnhanceGfx:
	STORE_REGS
	SET_VAR_CONTEXT

	
	tst.b	degraded_gfx
	beq	.exit

	move.l	_GfxBase,D0
	beq	.1

	lea	$DFF000,A5
	move.l	my_copinit(PC),cop1lc(a5) ; adresse du début de la liste

	; now that the copperlist is OK we restore the custom registers
	; but not in the case of the EXECUTE option

	TSTVAR_L	execute_flag
	bne.b	.skip_custom_restore

	JSRGEN	RestoreCustomRegs
.skip_custom_restore:

	move.l	D0,A6

	bsr	EnhanceBandWidth

	move.l	my_actiview(PC),D0
	beq.b	.1
	move.l	D0,A1
	JSRLIB	LoadView
	JSRLIB	WaitTOF
	JSRLIB	WaitTOF

.1
	move.l	_IntuitionBase,D0
	beq	.skip
	move.l	D0,A6
	move.l	TheScreen,D0
	beq	.skip

	bsr	EnhanceSprites

	move.l	TheScreen,D0
	move.l	D0,A0
	move.l	TheScreen,A0
	JSRLIB	CloseScreen

	bsr	CloseIntuiLib
.skip
	bsr	CloseGFXLib

	clr.b	degraded_gfx
.exit

	RESTORE_REGS
	rts

DegradeBandWidth:
	STORE_REGS
	SET_VAR_CONTEXT
	clr.b	degraded_bdw
	move.l	(_GfxBase),a6
	SETVAR_W	(gb_system_bplcon0,A6),system_bplcon0
	SETVAR_B	(gb_ChipRevBits0,A6),system_chiprev_bits
	cmp.l	#39,(LIB_VERSION,a6)
	blo	.gfxold
	btst	#GFXB_AA_ALICE,(gb_ChipRevBits0,a6)
	beq	.noaga
	move.b	(gb_MemType,a6),oldbandwidth
	move.b	#BANDWIDTH_1X,(gb_MemType,a6)	;auf ECS Wert setzen
	move.l	#SETCHIPREV_A,D0
	JSRLIB	SetChipRev
	move.l	D0,oldchiprev
	st	degraded_bdw
.noaga
.gfxold
	RESTORE_REGS
	rts

EnhanceBandWidth:
	STORE_REGS
	tst.b	degraded_bdw
	beq	.exit

	move.l	(_GfxBase),a6

	move.l	oldchiprev,D0
	JSRLIB	SetChipRev

	move.b	oldbandwidth,(gb_MemType,a6)
	clr.b	degraded_bdw
.exit

	RESTORE_REGS
	rts

AbsFun_DegradeCpu:
_DegradeCpu:
	STORE_REGS
	SET_VAR_CONTEXT

	; don't touch VBR if execute is set

	TSTVAR_L	execute_flag
	beq	.skipzerovbr
	TSTVAR_L	system_vbr
	beq	.skipzerovbr	; VBR is already at zero or 68000, skip all VBR part

	; no interrupts please

	move.l	_SysBase,A6
	JSRLIB	Disable

	; save the zero page vectors in a buffer

	sub.l	A0,A0
	addq.l	#$8,A0		; starts at $8!
	lea	zeropage_buffer(pc),A1
	move.l	#$F8,D0
	JSRGEN	CopyMem

	; copy VBR data to page zero
	; (forget to copy $0 and $4 as FASTEXEC could mess it up!)

	GETVAR_L	system_vbr,A0
	addq.l	#8,A0
	sub.l	A1,A1
	addq.l	#8,A1
	move.l	#$F8,D0
	JSRGEN	CopyMem

	; set the VBR to zero

	bsr	ZeroVBR

	; cache flush

	JSRABS	FlushCachesSys

	; interrupts again

	move.l	_SysBase,A6
	JSRLIB	Enable

.skipzerovbr:
	RESTORE_REGS

	bsr	DisCaches
	move.l	D0,oldcacheset
	move.b	#$00,$DE0000		; DTack (A3000/A4000 users)
	st.b	degraded_cpu
	rts

zeropage_buffer:
	BLKDECL	b,$F8,0

AbsFun_DegradeGfx:
_DegradeGfx:
	bsr	OpenGFXLib
	beq	D_Exit

	bsr	OpenIntuiLib	; will fail on kickstart 1.3

	move.l	_IntuitionBase,D0
	beq	.skip
	move.l	D0,A6
	sub.l	a0,a0
	lea	ScreenTags,a1
	JSRLIB	OpenScreenTagList
	move.l	d0,TheScreen

	; set sprite to common OCS resolution

	move.l	_GfxBase,A6
	cmp.w	#39,(LIB_VERSION,a6)
	blo	.skip
	bsr	DegradeSprites	
.skip
	move.l	_DosBase,A6
	move.l	#20,D1
	JSRLIB	Delay		; wait 2/5 second before launching

	move.l	_GfxBase,A6
	move.l	gb_ActiView(A6),my_actiview
	move.l	gb_copinit(A6),my_copinit
	sub.l	A1,A1
	JSRLIB	LoadView
	JSRLIB	WaitTOF
	JSRLIB	WaitTOF

.wav
	tst.l	(gb_ActiView,a6)
	bne.b	.wav
	JSRLIB	WaitTOF

	bsr	DegradeBandWidth

	JSRGEN	ResetDisplay

	; now adjust custom chips so AGA does not mess it all up

	lea	$DFF000,A5

	; Panza Kick Boxing display was too large (some pixels missing)

	move.w	#$0,(bplcon1,A5)	; new from JST v4.7


	move.w	#$1200,(bplcon0,A5)
	move.w	#$0024,(bplcon2,A5)
	move.w	#$0,(bplcon3,A5)
	move.w	#$0,(bpl1mod,A5)
	move.w	#$0,(bpl2mod,A5)

	; set blitter masks to all ones (Lemmings)

	move.w	#$FFFF,(bltafwm,A5)	
	move.w	#$FFFF,(bltalwm,A5)

D_Exit:
	st.b	degraded_gfx
	rts

; *** degrade the sprites
; *** thanks to Bert Jahn for the routine!!

DegradeSprites:
	move.l	TheScreen,A0
	move.l	(sc_ViewPort+vp_ColorMap,a0),a0
	clr.l	-(a7)			;TAG_DONE
	clr.l	-(a7)
	move.l	#VTAG_SPRITERESN_GET,-(a7)
	move.l	a7,a1
	move.l	_GfxBase,a6
	JSRLIB	VideoControl
	addq.l	#4,a7
	lea	oldspriteres,a0
	move.l	(a7)+,(a0)
	addq.l	#4,a7
	
	move.l	TheScreen,a0
	move.l	(sc_ViewPort+vp_ColorMap,a0),a0
	clr.l	-(a7)			;TAG_DONE
	move.l	#SPRITERESN_140NS,-(a7)
	move.l	#VTAG_SPRITERESN_SET,-(a7)
	move.l	a7,a1
	JSRLIB	VideoControl
	lea	(12,a7),a7

	move.l	TheScreen,a0
	move.l	_IntuitionBase,a6
	JSRLIB	MakeScreen	;force the changes
	JSRLIB	RethinkDisplay

	st.b	degraded_spr
	rts

EnhanceSprites:
	tst.b	degraded_spr
	beq	.exit

	move.l	_GfxBase,A6
	move.l	TheScreen,a0
	move.l	(sc_ViewPort+vp_ColorMap,a0),a0
	clr.l	-(a7)			;TAG_DONE
	move.l	oldspriteres,-(a7)
	move.l	#VTAG_SPRITERESN_SET,-(a7)
	move.l	a7,a1
	JSRLIB	VideoControl
	lea	(12,a7),a7

	move.l	TheScreen,a0
	move.l	_IntuitionBase,a6
	JSRLIB	MakeScreen	;force the changes

	clr.b	degraded_spr
.exit
	rts

; *** Degrades everything
; in: D0,D1: value and mask for caches control
; (the same format as in CacheControl())

AbsFun_Degrade:
	STORE_REGS

	move.l	D0,user_cacheflags
	move.l	D1,user_cachemask

	bsr	_DegradeCpu		; degrades caches, VBR, dtack
	bsr	_DegradeGfx		; degrades sprites and screen
	jsr	RelFun_ResetCIAs		; re-init keyboard

	RESTORE_REGS
	rts

DisCaches:
	STORE_REGS	D1-A6
	SET_VAR_CONTEXT

	move.l	D0,D6
	move.l	D1,D7

	JSRABS	Kick37Test
	tst.l	D0
	bne	.3		; Don't touch if KS 1.x

	move.l	D6,D0
	move.l	D7,D1

	TSTVAR_L	leavecaches_flag
	bne	.3		; Don't touch the caches
	TSTVAR_L	nocaches_flag
	beq	.5		; Disable ALL caches

	moveq.l	#0,D0
	moveq.l	#-1,D1
.5
	move.l	_SysBase,A6
	JSRLIB	CacheControl
	move.l	D0,D7

	move.l	_SysBase,A6
	move.b	AttnFlags+1(A6),D0
	btst	#AFB_68060,D0
	beq	.2

	; *** remove data cache, branch cache, write cache, superscalar mode

	lea	DisCacheSup,A5
	bsr	MySupervisor
.2
	move.l	D7,D0
	RESTORE_REGS	D1-A6
	rts

.3
	moveq.l	#0,D0
	bra	.2

EnaCaches:
	STORE_REGS	D1-A6
	SET_VAR_CONTEXT

	TSTVAR_L	leavecaches_flag
	bne	.3		; Don't touch the caches

	move.l	D0,D7
	JSRABS	Kick37Test
	tst.l	D0
	bne	.3		; Don't touch if KS 1.x
	move.l	D7,D0
	move.l	_SysBase,A6
	JSRLIB	CacheControl

	move.l	D0,D7

	move.l	_SysBase,A6
	move.b	AttnFlags+1(A6),D0
	btst	#AFB_68060,D0
	beq	.2

	lea	EnaCacheSup,A5
	bsr	MySupervisor
.2
	move.l	D7,D0
	RESTORE_REGS	D1-A6
	rts

.3
	moveq.l	#0,D0
	bra	.2

; *** Disable some '060 caches
; *** Left intact by CacheControl

; internal, only useful (and executed) for 68060 cpus

DisCacheSup:
	MC68020
	ori.w	#$700,sr
	movec	cacr,D0
	andi.l	#$A0800000,D0
	move.l	D0,old060cacr

	movec	cacr,D0
	move.l	#$A0800000,D1		; mask for specific 68060 caches

	not.l	D1
	and.l	D1,D0
	movec	D0,cacr

	nop

	; *** remove superscalar

	bra	.nossh
	MC68060
	movec	pcr,D0
	move.l	D0,old060pcr
	andi.l	#$04300100,D0
	movec	D0,pcr
.nossh

	; *** flush

	CPUSHA	BC
	MC68000
	rte

; *** Enable some '060 caches
; *** Left intact by CacheControl

EnaCacheSup:
	MC68060
	ori.w	#$700,sr
	movec	cacr,D0
	or.l	old060cacr,D0
	movec	D0,cacr

	bra	.nossh
	movec	pcr,D0
	or.l	old060pcr,D0
	movec	D0,pcr
.nossh

	CPUSHA	BC
	MC68000
	rte

; *** Gets VBR value
; out: D0: VBR value

; VBR is the vector base register (interrupts, traps, exceptions)
; It exists on 68010, 68020, 68030, 68040, 68060
; Most of the games don't like it to be different from zero,
; others are originally HD-Installable :-)
; Called from a 68000, this function will return 0 in any case (no VBR)

AbsFun_Priv_GetVBR:
	STORE_REGS	A0
	lea	GetVBRSup(pc),A0
	bsr	OperateVBR_SYS
	RESTORE_REGS	A0
	rts

; *** Sets VBR value
; in: D0: new VBR value

; Called from a 68000, this function will do nothing (no VBR)
; It will do nothing if the LEAVEVBR tooltype is set too (debug)

SetVBR:
	STORE_REGS	A0
	lea	SetVBRSup(pc),A0
	bsr	OperateVBR_HW
	RESTORE_REGS	A0
	rts
	
; *** Sets VBR to zero
; It will do nothing if the LEAVEVBR tooltype is set (debug)

ZeroVBR:
	STORE_REGS	D1-A6
	moveq.l	#0,D0
	lea	SetVBRSup(pc),A0
	bsr	OperateVBR_SYS
	RESTORE_REGS	D1-A6
	rts

; *** VBR operation
; < D0: optional param
; > D0: optional out
; < A0: routine to be called in supervisor mode
; VBR must be at 0 for this one

OperateVBR_HW:
	STORE_REGS	D1-D7/A0-A6

	move.l	_SysBase,A6
	move.b	AttnFlags+1(A6),D1
	btst	#AFB_68010,D1		; At least 68010
	beq	.error
	
	move.l	$80.W,D4
	move.l	A0,$80.W
	TRAP	#0
	move.l	D4,$80.W
.exit
	RESTORE_REGS	D1-D7/A0-A6
	rts
.error
	moveq.l	#0,D0
	bra	.exit



; *** VBR operation
; < D0: optional param
; > D0: optional out
; < A0: routine to be called in supervisor mode

OperateVBR_SYS:
	STORE_REGS	D1-D7/A0-A6
	move.l	A0,A5			; supervisor function

	move.l	_SysBase,A6
	move.b	AttnFlags+1(A6),D1
	btst	#AFB_68010,D1		; At least 68010
	beq	.error

	bsr	MySupervisor
.exit
	RESTORE_REGS	D1-D7/A0-A6
	rts
.error
	moveq.l	#0,D0
	bra	.exit

; *** Supervisor call to set the VBR

SetVBRSup:
	STORE_REGS	A4
	SET_VAR_CONTEXT
	TSTVAR_L	leavevbr_flag
	bne	.exit

	CHANGEVBR	D0
.exit
	RESTORE_REGS	A4
	rte

; *** Supervisor call to get the VBR

GetVBRSup:
	MC68010
	movec	VBR,D0
	MC68000
	rte


; *** open graphics library

OpenGFXLib:
	STORE_REGS	D1-A6
	lea	grname,A1
	move.l	_SysBase,A6
	moveq.l	#0,D0
	JSRLIB	OpenLibrary
	move.l	D0,_GfxBase
	RESTORE_REGS	D1-A6
	rts

; *** close graphics library

CloseGFXLib:
	STORE_REGS	D0-A6
	move.l	_SysBase,A6
	move.l	_GfxBase,D0
	beq	.skip
	move.l	D0,A1
	JSRLIB	CloseLibrary
	clr.l	_GfxBase
.skip
	RESTORE_REGS	D0-A6
	rts

; *** open intuition library

OpenIntuiLib:
	STORE_REGS	D1-A6
	lea	intname,A1
	move.l	_SysBase,A6
	moveq.l	#36,D0
	JSRLIB	OpenLibrary
	move.l	D0,_IntuitionBase
	RESTORE_REGS	D1-A6
	rts

; *** close intuition library

CloseIntuiLib:
	STORE_REGS	D0-A6
	move.l	_SysBase,A6
	move.l	_IntuitionBase,D0
	beq	.skip
	move.l	D0,A1
	JSRLIB	CloseLibrary
	clr.l	_IntuitionBase
.skip
	RESTORE_REGS	D0-A6
	rts


; *** Flushes the caches

AbsFun_FlushCachesSys:
	STORE_REGS
	JSRABS	Kick37Test
	tst.l	D0
	bne	.2		; Don't touch if KS 1.x
	moveq.l	#0,D0
	moveq.l	#0,D1
	move.l	_SysBase,A6
	JSRLIB	CacheControl
.2
	RESTORE_REGS
	rts

; *** Test ROM version
; out: D0=0  if KS>=37
;      D0!=0 if KS<37

AbsFun_Kick37Test:
	move.l	#37,D0
	JSRABS	KickVerTest
	rts	

; *** Test if ROM version > a given version
; in: D0: kickstart version number
; out:D0=0 newer !=0 older
; This was broken since v4.5d, but now it should work

AbsFun_KickVerTest:
	STORE_REGS	D1/A5/A6

	move.l	_SysBase,A6
	move.w	LIB_VERSION(A6),D1	; exec library version
	cmp.w	D0,D1
	bcc.s	.newer
	moveq.l	#-1,D0
.exit
	RESTORE_REGS	D1/A5/A6
	rts
.newer
	moveq.l	#0,D0
	bra.b	.exit

; ** update system time from battery backed up clock


AbsFun_Priv_SendCDTVCommand:
	STORE_REGS
    move.l  d0,d5
    
	; alloc some mem for IORequest

	MOVEQ	#40,D0			
	MOVE.L	#MEMF_CLEAR|MEMF_PUBLIC,D1
	move.l	_SysBase,A6
	JSRABS	AllocateTheMemory
	move.l	D0,io_request
	beq	.Quit

	; open cdtv.device

	MOVEA.L	D0,A1
	LEA	cdtvname(PC),A0	; name
	MOVEQ	#0,D0			; unit 0
	MOVE.L	D0,D1			; flags
	JSRLIB	OpenDevice
	move.l	D0,D6
	ext	D6
	ext.l	D6
	bne	.Quit		; unable to open

	; prepare the IORequest structure

	MOVEQ	#0,D0
	MOVEA.L	io_request(pc),A0
	MOVE.B	D0,8(A0)
	MOVE.B	D0,9(A0)
	SUBA.L	A1,A1
	MOVE.L	A1,10(A0)
	MOVE.L	A1,14(A0)
	CLR.L	36(A0)

	move.l	io_request(pc),A0

	move.l	A0,A1
	move.w	d5,(IO_COMMAND,a1)
	move.l	_SysBase,A6
	JSRLIB	DoIO

.Quit:
	; close timer.device if open

	tst.l	D6
	bne	.Free
	MOVE.L	io_request(pc),D1
	beq	.End
	move.l	D1,A1
	move.l	_SysBase,A6
	JSRLIB	CloseDevice

.Free:		
	; free the memory

	MOVEQ	#40,D0
	move.l	io_request(pc),A1
	move.l	_SysBase,A6
	JSRABS	FreeTheMemory
.End:
	RESTORE_REGS
	rts
    

AbsFun_Priv_SetClockLoad:
	STORE_REGS
	moveq.l	#0,D6
	not.l	D6

	; open battclock.resource

	lea	battname(pc),A1
	move.l	_SysBase,A6
	JSRLIB	OpenResource
	move.l	D0,BattBase
	beq	SCL_Quit

	; alloc some mem for IORequest

	MOVEQ	#40,D0			
	MOVE.L	#MEMF_CLEAR|MEMF_PUBLIC,D1
	move.l	_SysBase,A6
	JSRABS	AllocateTheMemory
	move.l	D0,io_request
	beq	SCL_Quit

	; open timer.device

	MOVEA.L	D0,A1
	LEA	timername(PC),A0	; name
	MOVEQ	#0,D0			; unit 0
	MOVE.L	D0,D1			; flags
	JSRLIB	OpenDevice
	move.l	D0,D6
	ext	D6
	ext.l	D6
	bne	SCL_Quit		; unable to open

	; prepare the IORequest structure

	MOVEQ	#0,D0
	MOVEA.L	io_request(pc),A0
	MOVE.B	D0,8(A0)
	MOVE.B	D0,9(A0)
	SUBA.L	A1,A1
	MOVE.L	A1,10(A0)
	MOVE.L	A1,14(A0)
	CLR.L	36(A0)

	move.l	BattBase(pc),A6
	JSRLIB	ReadBattClock
	move.l	io_request(pc),A0
	move.l	D0,32(A0)	; D0 was read from battclock
	move.w	#$B,28(A0)
	move.l	A0,A1
	move.l	_SysBase,A6
	JSRLIB	DoIO

SCL_Quit:
	; close timer.device if open

	tst.l	D6
	bne	SCL_Free
	MOVE.L	io_request(pc),D1
	beq	SCL_End
	move.l	D1,A1
	move.l	_SysBase,A6
	JSRLIB	CloseDevice

SCL_Free:		
	; free the memory

	MOVEQ	#40,D0
	move.l	io_request(pc),A1
	move.l	_SysBase,A6
	JSRABS	FreeTheMemory
SCL_End:
	RESTORE_REGS
	rts

; < A5: user routine to call in supervisor mode

MySupervisor:
	move.l	TaskCheck,-(A7)
	move.l	_SysBase,A6
	JSRLIB	Supervisor
	move.l	(A7)+,TaskCheck
	rts

io_request:
	dc.l	0
my_actiview:
	dc.l	0
my_copinit:
	dc.l	0
oldcacheset:
	dc.l	0
old060cacr:
	dc.l	0
old060pcr:
	dc.l	0

cache_value:
	dc.l	0
cache_mask:
	dc.l	0

BattBase:
	dc.l	0

_IntuitionBase:
	dc.l	0

TheScreen:
	dc.l	0

ScreenTags:
		dc.l	SA_DisplayID,135168	; PAL lores
		dc.l	SA_Width,320
		dc.l	SA_Height,200
		dc.l	SA_Depth
		dc.l	2			; 4 colors
		dc.l	SA_Quiet,-1
		dc.l	SA_AutoScroll,0
		dc.l	0,0

oldspriteres:
	dc.l	0
oldbandwidth:
	dc.l	0
oldchiprev:
	dc.l	0

user_cacheflags:
	dc.l	0
user_cachemask:
	dc.l	0

grname:
	dc.b	"graphics.library",0
intname:
	dc.b	"intuition.library",0
cdtvname:
	dc.b	"cdtv.device",0
timername:
	dc.b	"timer.device",0
battname:
	dc.b	"battclock.resource",0

degraded_bdw:
	dc.b	0
degraded_cpu:
	dc.b	0
degraded_gfx:
	dc.b	0
degraded_spr:
	dc.b	0
	cnop	0,4
