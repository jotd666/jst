	; *** Relocated routines. always call with JSRGEN (see macros.i)
	; *** from user program. It works with JSR but if the OS is killed
	; *** JSRGEN is safer as it jumps in the allocated block
	; *** which is in the top of memory (MEMF_REVERSE) if kick > 38

	; if loader tries to write a file bigger than below, then exception

MAX_FILE_WRITE = $400000



		include	"jst_libs.i"
		include	"osemu.i"
		include	"jstrel_macros.i"
		include	"jst_rel.i"
		include	"vectors.i"

	; defines

	XDEF	JstRelStart

OSEMU_BASEADDRESS = $400

; hardcoded sysbase

ABSEXECBASE = 4

;	SECTION	"Relocatable",CODE

EXCEPTION_ROUTINE:MACRO
Exception\1:
	IFD	BARFLY
		IFNE	NARG-3
			DEF_EXCEPTION	<\1>
			DEF_EXCEPTION_HANDLER	<\1>,<\2>
		ELSE
			DEF_EXCEPTION_HANDLER	<\1>,<\2>,<\3>
		ENDC
	ELSE
		IFNE	NARG-3
			DEF_EXCEPTION	\1
			DEF_EXCEPTION_HANDLER	\1,\2
		ELSE
			DEF_EXCEPTION_HANDLER	\1,\2,\3
		ENDC
	ENDC
	ENDM

DEF_EXCEPTION:MACRO
	STORE_REGS	D0/A0

	; to avoid infinite calls...

	LEA	Exception\1(PC),A0
	MOVE.L	$\1.W,D0
	CMP.L	D0,A0
	BEQ.S	HandleException\1

	RESTORE_REGS	D0/A0
	MOVE.L	$\1.W,-(A7)
	RTS
	ENDM

DEF_EXCEPTION_HANDLER:MACRO
HandleException\1:
	IFNE	NARG-3
		RESTORE_REGS	D0/A0
	ENDIF
	
	STORE_REGS	A4-A5
	SET_VAR_CONTEXT
	lea	ExcMess\1(pc),A5
	SETVAR_L	A5,message_ptr
	RESTORE_REGS	A4-A5
	bra	HandleException
ExcMess\1:
	dc.b	10,"Exception"
	dc.b	": "
	IFD	MAXON_ASM
		dc.b	"\2"
	ELSE
		IFD	BARFLY
			dc.b	"\2"
		ELSE
			DC.B	\2	; phxass
		ENDIF
	ENDIF
	
	dc.b	10,0
	even
	ENDM
	
JstRelStart:
	dc.l	JstRelEnd-JstRelStart
	dc.l	-1

	; space for functions

	blk.b	Rel_ENDFUNC-Rel_STARTFUNC,0

reloc_RelFunTable:	; macro-expanded here, corrected by abs section
	MAKE_REL_REFS	ADDR
	dc.l	-1		; end of list

	; code

	include	"jst_hunk.asm"

	INCLUDE	"Emulate_MOVEP.asm"	;Added by Ralf, MOVEP Emulator
	include	"jst_cop.asm"		;Copper patch utilities
	include	"jst_util.asm"		;String & patch utilities
	include	"jst_whd.asm"		;Added by Jeff, WHDLoad Emulator

RelFun_FlushCachesHard_proxy:
		bra 	RelFun_FlushCachesHard
; > A0: osemu base address

RelFun_GetOSEmuBase:
	lea	OSEMU_BASEADDRESS,A0
	rts

; < A0: new function or 0 to disable
; > D0: old function

RelFun_SetAfterSwitchFunction:
	STORE_REGS	A4
	SET_VAR_CONTEXT

	GETVAR_L	after_switch_cb,D0
	SETVAR_L	A0,after_switch_cb

	RESTORE_REGS	A4
	RTS

; < A0: file/dir name
; < D0: flags (swap Y/N)
; > D0: OBJ_FILE,OBJ_DIR,OBJ_NONE

RelFun_GetObjectType:
	STORE_REGS	A4/A5
	bsr	RelFun_Priv_SearchDirEntry
	tst.l	(A5)
	bne.b	.exists_in_cache
	move.l	#OBJ_NONE,D0
.exit:
	RESTORE_REGS	A4/A5
	RTS

.exists_in_cache:
	tst.w	8(A5)
	beq.b	.afile
	move.l	#OBJ_DIR,D0
	bra.b	.exit
.afile
	move.l	#OBJ_FILE,D0
	bra.b	.exit

; < A0: address to watch (byte)
; < A1: routine to call when value has changed
; < D0: leave it to zero, future use
; can exit because too many watchpoints

;RelFun_AddWatch:
;RelFun_RemWatch:

; < D0: 0: off
;       1: on

RelFun_TraceControl:
	STORE_REGS	D1/A0/A4
	SET_VAR_CONTEXT

	SETVAR_W	D0,trace_control

	GETVAR_L	current_vbr,A0
	lea	($80,A0),A0
	MOVE.L	(A0),-(A7)

	PEA	.toggle_trace(PC)
	MOVE.L	(A7)+,(A0)
	TRAP	#0

	MOVE.L	(A7)+,(A0)
	RESTORE_REGS	D1/A0/A4
	RTS

.toggle_trace:
	move.w	(A7),D1
	bclr	#15,D1	; trace off
	tst.l	D0
	beq.b	.skip
	bset	#15,D1
.skip	
	move.w	D1,(A7)
	RTE		; SR | $8000 -> goto trace mode on return

; Called once at startup or when RESUME is called
; Freezes system and saves CIA & custom registers


RelFun_Priv_RegisterBusErrorRoutine:
	STORE_REGS	A0/A4
	SET_VAR_CONTEXT
	CMPVAR_L	#2,mmunumstate_flag
	BNE.S	.exit

	lea	Exception08(pc),A0
	CALL_MMUCode	RegisterBUSErrorRoutine
.exit
	RESTORE_REGS	A0/A4
	RTS


RelFun_GetAttnFlags:
	STORE_REGS	A4
	SET_VAR_CONTEXT
	GETVAR_L	attnflags,D0
	RESTORE_REGS	A4
	rts

RelFun_EnableMMU
	STORE_REGS	A0/A4
	SET_VAR_CONTEXT

	CMPVAR_L	#2,mmunumstate_flag
	BNE.S	.NoActivate

	GETVAR_L	current_vbr,A0
	lea	($80,A0),A0
	MOVE.L	(A0),-(A7)

	PEA	.EnableMMUCode(PC)
	MOVE.L	(A7)+,(A0)
	TRAP	#0

	MOVE.L	(A7)+,(A0)
.NoActivate
	RESTORE_REGS	A0/A4
	RTS

.EnableMMUCode
	JMP_MMUCode	EnableMMU
	RTE				; In case of no MMU or MMU disabled

RelFun_DisableMMU
	STORE_REGS	A0/A4
	SET_VAR_CONTEXT

	GETVAR_L	current_vbr,A0
	lea	($80,A0),A0
	MOVE.L	(A0),-(A7)

	PEA	.DisableMMUCode(PC)
	MOVE.L	(A7)+,(A0)
	TRAP	#0

	MOVE.L	(A7)+,(A0)

	RESTORE_REGS	A0/A4
	RTS

.DisableMMUCode
	JMP_MMUCode	DisableMMU
	RTE				; In case of no MMU or MMU disabled

RelFun_RestoreMMU
	STORE_REGS	A0/A4
	SET_VAR_CONTEXT

	GETVAR_L	current_vbr,A0
	lea	($80,A0),A0
	MOVE.L	(A0),-(A7)
	
	PEA	.RestoreMMUCode(PC)
	MOVE.L	(A7)+,(A0)
	TRAP	#0

	MOVE.L	(A7)+,(A0)
	RESTORE_REGS	A0/A4
	RTS

.RestoreMMUCode
	JMP_MMUCode	RestoreMMU
	RTE				; In case of no MMU or MMU disabled

; installs OS emulation by Harry

RelFun_InstallHarryOSEmu:
	STORE_REGS	A0/A2-A5/D0-D7
	
	SET_VAR_CONTEXT

	GETVAR_L	osemu_ptr,D0
	beq	.exit		; no OS emu installed

	GETVAR_L	osemu_len,D1
	beq	.exit		; no OS emu installed

	move.l	D0,A0
	lea	OSEMU_BASEADDRESS,A1
.copy
	move.b	(A0)+,(A1)+
	subq.l	#1,D1
	bne.b	.copy

	; initialize a fake WHDload structure

	lea	fake_ws_basememsize(pc),A0
	GETVAR_L	maxchip,(A0)		; base memory!

	lea	fake_ws_quitkey(pc),A0
	GETVAR_B	quitkeynum,D0
	beq	.noqk
	move.b	D0,(A0)				; user-defined quit key
.noqk

	; configures OSEmu if v >= 1.0

	bsr	OSEmu_Config

	JSRGEN	FlushCachesHard

	; initialize Harry's code

	lea	fake_resload(pc),A0
	lea	fake_slbase(pc),A1

	JSR	OSEMU_BASEADDRESS

	; store trackdisk diskio

	lea	fake_diskio(pc),A0
	move.l	A1,(A0)

	; return trackdisk diskio and ExecBase

	move.l	$4.W,A6			; puts fake ExecBase in A6

	; properly sets Attention flags (WHDTags not supported by JST emulation)

	JSRGEN	GetAttnFlags
	move.w	D0,AttnFlags(A6)
	
.exit
	RESTORE_REGS	A0/A2-A5/D0-D7
	
	rts

OSEmu_Config:
	lea	OSEMU_BASEADDRESS,A0
	cmp.l	#"OSEM",(OSM_ID,A0)
	bne.b	.tooold		; old version, before I add the ID string
	
	JSRGEN	GetUserFlags
	move.l	D0,(OSM_JSTFLAGS,A0)	; sets user flags into OSEmu

	cmp.w	#3,(OSM_RELEASE,A0)
	bcs.b	.below13		; before release 1.3, expansion mem was not supported

	; store extension memory size and base address in the OSEmu structure
	; using 24 bit expansion mem will have no effect, only fastmem will be
	; taken in consideration.

	GETVAR_L	extbuf,OSM_EXPMEM(A0)
	GETVAR_L	extsize,OSM_EXPSIZE(A0)

.below13:
	cmp.w	#4,(OSM_RELEASE,A0)
	bcs.b	.below14		; before release 1.4, call to debugger was not available
	
	lea	RelFun_EnterDebugger(pc),A2
	move.l	A2,OSM_DEBUGENTRY(A0)
.below14:
.tooold:
	rts

; *** Checks for AGA chipset
; out: D0=0 if AGA, -1 else

RelFun_CheckAGA:
	move.w	$DFF07C,D0
	cmp.b	#$F8,D0
	blt.b	.noaga
	moveq.l	#0,D0
	rts
.noaga
	moveq.l	#-1,D0
	rts


; *** Patch exceptions

; called at startup anyway, but it may be necessary to call it
; again if the game overwrites the values, and then crashes

; There's a trick to avoid trapping ILLEGAL/LINE-A/etc... set
; by the game: when the exception occurs, JST checks that the
; handler is different from the one it installed. If that's the
; case, it calls the handler, else, it exits with the exception message

RelFun_PatchExceptions:
	STORE_REGS

	SET_VAR_CONTEXT

	TSTVAR_L	chipmirror
	beq	.exit

;Added by Ralf

	LEAVAR	new_vbr,A0
	move.l	#'JST!',(A0)	; mark VBR table as JST's

	PATCH_EXCEPT	08	; patches in zero page & in the current VBR

	bsr	InstallBusErrorVector

	PATCH_EXCEPT	0C
	PATCH_EXCEPT	10
	PATCH_EXCEPT	14
	PATCH_EXCEPT	18
	PATCH_EXCEPT	1C
	PATCH_EXCEPT	20	; special case
	PATCH_EXCEPT	24
	PATCH_EXCEPT	28
	PATCH_EXCEPT	2C

	PATCH_EXCEPT	30

	PATCH_EXCEPT	34
	PATCH_EXCEPT	38
	PATCH_EXCEPT	3C

	PATCH_EXCEPT	40
	PATCH_EXCEPT	44
	PATCH_EXCEPT	48
	PATCH_EXCEPT	4C
	PATCH_EXCEPT	50
	PATCH_EXCEPT	54
	PATCH_EXCEPT	58
	PATCH_EXCEPT	5C

; Added by Ralf
	BSR	PATCH_060INTEGER	

; JOTD: patch zero page $100-$400 (maybe the explanation of some PPC crashes is there!)

;	lea	$100.W,A0
;	move.w	#$BF,D0
;	lea	RunTime_ZPException(pc),A1
.pzp
;	move.l	A1,(A0)+
;	dbf	D0,.pzp
.exit
	RESTORE_REGS
	rts

;	RUNTIME_ERROR_ROUTINE	ZPException,"Unlisted Zero page exception $100-$400"


InstallBusErrorVector
	STORE_REGS	D0/A0
	; exception will be overwritten by SetBUSError if MMU is found

	MOVEQ	#0,D0
	CALL_MMUCode	SetBUSError
	TST.L	D0
	BEQ.S	.No_MMU
	
	LEAVAR	new_vbr,A0
	MOVE.L	D0,8(A0)	; copies bus error handler in the JST VBR table too
	
.No_MMU
	RESTORE_REGS	D0/A0
	rts


; < D0: file error allowed / or trigger runtime error

RelFun_SetRTFileError:
	STORE_REGS	A4
	SET_VAR_CONTEXT
	SETVAR_L	D0,fatal_fileerror
	RESTORE_REGS	A4
	rts

; returns 1 if the version is registered

RelFun_IsRegistered:
	MOVEQ.l	#1,D0
	rts

RelFun_PatchZeroPage:
	STORE_REGS
	SET_VAR_CONTEXT

	TSTVAR_L	chipmirror
	beq	.exit
	lea	.zeropage_installed(pc),A0
	tst.l	(A0)
	bne	.exit	; zero page already patched

	move.l	#-1,(A0)	; sets zero page patch flag for next call

	lea	$8.W,A0
        LEAVAR	new_vbr+8,A2
	lea	ExceptionXX(pc),A1
	move.w	#$3D,D0

	; ** initialize exceptions to a default error trap
.init
	move.l	A1,(A0)+
	MOVE.L	A1,(A2)+
	dbf	D0,.init

	; ** exceptions triggered by wrong instructions

	JSRGEN	PatchExceptions

	; ** interrupts

; Added by Ralf
	LEAVAR	new_vbr,A0

	PATCH_EXCEPT	60	; spurious interrupt

	PATCH_INTVEC	64
	PATCH_INTVEC	68
	PATCH_INTVEC	6C
	PATCH_INTVEC	70
	PATCH_INTVEC	74
	PATCH_INTVEC	78
	PATCH_INTVEC	7C
	bra	.traps

	; old "deadly" mode, not really useful
	IFEQ	1
	PATCH_EXCEPT	64
	PATCH_EXCEPT	68
	PATCH_EXCEPT	6C
	PATCH_EXCEPT	70
	PATCH_EXCEPT	74
	PATCH_EXCEPT	78
	PATCH_EXCEPT	7C
	ENDC
.traps
	; ** traps

	PATCH_EXCEPT	80
	PATCH_EXCEPT	84
	PATCH_EXCEPT	88
	PATCH_EXCEPT	8C
	PATCH_EXCEPT	90
	PATCH_EXCEPT	94
	PATCH_EXCEPT	98
	PATCH_EXCEPT	9C
	PATCH_EXCEPT	A0
	PATCH_EXCEPT	A4	; the $A4 address, not the register!
	PATCH_EXCEPT	A8
	PATCH_EXCEPT	AC
	PATCH_EXCEPT	B0
	PATCH_EXCEPT	B4
	PATCH_EXCEPT	B8
	PATCH_EXCEPT	BC

	; *** MMU exceptions

	PATCH_EXCEPT	E4
	PATCH_EXCEPT	E8

	; *** MOVEP (060) exception -> MOVEP emulation

	BSR	PATCH_060INTEGER	

	; very tricky function:
	;
	; 1: sets the JST VBR driver to intercept interrupts
	; before they reach the proggy
	;
	; 2: if a HRTMon is installed, tell it not to change
	; the vectors by putting fake_resload in the location
	; reserved for WHDLoad (and JST now:)) by Bert/Hornet
	; so HRTMon is aware of a tool like JST or WHDLoad
	; and the 'wq' command works

	bsr	RalfVBRSet

.exit
	RESTORE_REGS
	rts

.zeropage_installed:
	dc.l	0

; Modifies HRTMon base, which allows HRTMon to recognize a
; WHDLoad-like tool is running, so don't do strange vector
; relocations when entering the debugger.

ShutupHRTMon:
	STORE_REGS	A0/A4
	SET_VAR_CONTEXT

	GETVAR_L	debugger,A0
	CMP.L	#HRTMon,A0
	BNE.S	.skip_sigdebug

	GETVAR_L	debuggerbase,A0

; check disabled. HRTMon can be at 0 (but this is not recommended!)
;	cmp.l	#0,A0
;	beq	.skip_sigdebug
; end of check disabled

	PEA	fake_resload(pc)
	MOVE.L	(A7)+,(60,A0)		; resload, to avoid crashes on exit (thanks Ralf & Bert!)

.skip_sigdebug:
	RESTORE_REGS	A0/A4
	rts

; Set up the VBR with the new emulation routines
; And PatchZeroPage if not done (RESUME)
; Added by Ralf

RalfVBRSet:
	STORE_REGS	D0/A0/A1/A4
	SET_VAR_CONTEXT

	bsr	ShutupHRTMon

	JSRGEN	GetAttnFlags

	BTST	#AFB_68010,D0		; At least 68010
	BEQ.S	.NO_VBRSET

	TSTVAR_L	novbrmove_flag
	BNE.S	.NO_VBRSET
	
	GETVAR_L	current_vbr,A1
	lea	($80,A1),A1
	move.l	(A1),D0		; save TRAP #0

	LEA	Set_JSTVBR(PC),A0
	MOVE.L	A0,(A1)
	TRAP	#0

	MOVE.L	D0,(A1)

.NO_VBRSET
	RESTORE_REGS	D0/A0/A1/A4
	rts


; Set the VBR to system value
; Can only be used in supervisor mode!
; Added by Jeff

RalfVBRRestore:
	STORE_REGS	D0/A4
	SET_VAR_CONTEXT

	JSRGEN	GetAttnFlags

	BTST	#AFB_68010,D0		; At least 68010
	BEQ.S	.NO_VBRRESET

	GETVAR_L	system_vbr,D0
	CHANGEVBR	D0
	
.NO_VBRRESET
	RESTORE_REGS	D0/A4
	rts

; installs JST VBR driver
; A4 is supposed to be set (SET_VAR_CONTEXT)

Set_JSTVBR:
	STORE_REGS	D0

	PEAVAR	new_vbr
	RESTORE_REGS	D0

	CHANGEVBR	D0

	RESTORE_REGS	D0
	RTE

; loader newer than JST

RelFun_Unsupported1:
RelFun_Unsupported2:
RelFun_Unsupported3:
RelFun_Unsupported4:
RelFun_Unsupported5:
RelFun_Unsupported6:
RelFun_Unsupported7:
RelFun_Unsupported8:
RelFun_Unsupported9:
RelFun_UnsupportedA:
RelFun_UnsupportedB:
RelFun_UnsupportedC:
	RUNTIME_ERROR_ROUTINE	Outdated,"Version of JOTD Startup is outdated for this object!"


; *** WaitBOF, hardware made

RelFun_WaitBOF:
	IFD	WAIT_BOF_NOT_BROKEN
	STORE_REGS

	; first save intena+intreq

	lea	$DFF000,A6
	move.w	(intenar,A6),D0
	move.w	(intreqr,A6),D1

	; then freeze and clear interrupts

	move.w	#$7FFF,(intreq,A6)
	move.w	#$7FFF,(intena,A6)

	; then TRAP to be sure we're in supervisor more

	move.l	$BC.W,-(A7)
	STORE_REGS	D0-D1/A6
	lea	waitbof_sup(pc),A0
	move.l	A0,$BC.W
	TRAP	#$F
	RESTORE_REGS	D0-D1/A6
	move.l	(A7)+,$BC.W

	; then restore intena+intreq

	bset	#15,D0	; activate
	bset	#15,D1
	move.w	D1,(intreq,A6)
	move.w	D0,(intena,A6)

	RESTORE_REGS
	rts

waitbof_sup:
	move	#$2700,SR	
	lea	$DFF000,A6

	move.l	$6C.W,-(A7)	; save old VBL interrupt

	lea	waitbof_int(pc),A0
	move.l	A0,$6C.W
	lea	waitbof_flag(pc),A0
	
	move.w	#$C020,(intena,A6)	; only VBL interrupt
	move	#$2000,SR		; activate VBL interrupt

	; waits for 2 frames to pass...

	clr.w	(A0)
.wait1
	tst.w	(A0)
	beq.b	.wait1
	clr.w	(A0)
.wait2
	tst.w	(A0)
	beq.b	.wait2

	move.l	(A7)+,$6C.W

	; freeze again and return

	move.w	#$7FFF,(intreq,A6)
	move.w	#$7FFF,(intena,A6)
	RTE

waitbof_int:
	STORE_REGS	A0
	lea	waitbof_flag(pc),A0
	move.w	#-1,(A0)		; sets flag Ok frame passed
	RESTORE_REGS	A0
	move.w	#$0020,$DFF000+intreq	; acknowledge interrupt
	RTE

waitbof_flag:
	dc.l	0
	ELSE
	RTS
	ENDC

; *** Get supervisor stack

RelFun_GetSSP:
	JSRGEN	GetSR
	btst	#13,D0
	beq.b	.tricky
	move.l	A7,D0
	addq.l	#4,D0	; stack when we'll return to caller
	rts

	; harder here: we are in user mode
	; we cannot just call TRAP, get A7, and add #6 (or #8)
	; because of stackframe, it would not be very clean

.tricky
	STORE_REGS	D1/A0/A5

	move.l	$BC.W,A5
	lea	.gosupervisor(pc),A0
	move.l	A0,$BC.W
	TRAP	#$F
	move.l	A5,$BC.W

	; we're in supervisor mode. Just get the stack

	move.l	A7,D0

	; now switch to user mode again

	move	SR,D1
	bclr	#13,D1
	move	D1,SR

	RESTORE_REGS	D1/A0/A5
	rts

.gosupervisor
	move.w	(A7),D0
	bset	#13,D0
	move.w	D0,(A7)
	rte		; when we return we're in supervisor mode

; *** Get status register

RelFun_GetSR:
	STORE_REGS	D1/A0/A5
	move.l	$BC.W,A5
	lea	.GetSRSup(pc),A0
	move.l	A0,$BC.W
	TRAP	#$F
	move.l	A5,$BC.W
	RESTORE_REGS	D1/A0/A5
	rts

.GetSRSup:
	moveq.l	#0,D0
	move.w	(A7),D0		; SR in the stack
	rte

; *** Go ECS, obsolete now

RelFun_GoECS:
	rts

; *** Go 15KHz display

RelFun_ResetDisplay:
	STORE_REGS	A4/A5
	SET_VAR_CONTEXT

	lea	$DFF000,A5

	TSTVAR_L	ntsc_flag
	bne.b	.1	; Force NTSC mode

	TSTVAR_L	pal_flag
	bne.b	.0	; Force PAL mode

	bra.b	.2
.0
	; force PAL
	move.w	#$0020,beamcon0(A5)	; go PAL
	bra	.2
.1
	; force NTSC
	move.w	#$0000,beamcon0(A5)	; go NTSC
.2	
	;;MOVE.W	#$0,fmode(A5)		; disable AGA-fetch rate why ???

	RESTORE_REGS	A4/A5
	rts

; *** LORES sprites (obsolete now)

RelFun_ResetSprites:
	rts


; *** custom registers and CIA routines

	include	"jst_cus.asm"

	include	"ReadJoyPad.s"
	include	"virtual_keyboard.asm"
	include	"virtual_mouse.asm"
	include	"send_key_event.asm"
	
; *** An error occurred: call InGameExit

ExitFromError:
	STORE_REGS	D0/A4
	SET_VAR_CONTEXT

	TSTVAR_L	ostrashed
	bne.b	.dead		; the os was down: call InGameExit

.alive
	CLRVAR_L	debug_flag	; do not display the coredump message
	RESTORE_REGS	D0/A4
	JMPABS	CloseAllWithError

.dead
	RESTORE_REGS	D0/A4
	bra	RelFun_InGameExit

	RUNTIME_ERROR_ROUTINE	DiskRead,"Disk read error"
	RUNTIME_ERROR_ROUTINE	FileRead,"File read error"
	RUNTIME_ERROR_ROUTINE	LowmemGDP,"GetDiskPointer called while in LOWMEM mode!"
	RUNTIME_ERROR_ROUTINE	WrongId,"Wrong virtual disk ID"
	RUNTIME_ERROR_ROUTINE	OutOfBounds,"Diskfile update out of bounds!"

RTStoreMessage:
	STORE_REGS	A4
	SET_VAR_CONTEXT
	SETVAR_L	A5,message_ptr
	RESTORE_REGS	A4
	bra	ExitFromError

; *** InGameExit: Exits and restores everything from _SaveOsData
; *** (chipmem, caches, VBR, display...)

; Do not call this function before having called SaveOSData
; or the SAVE_OSDATA macro from the user program.
; Call it whenever you want during the game, even if the OS
; is totally killed. This function will resurrect the OS.

; This function never returns to the caller
; It just quits the program


RelFun_InGameExit:
	STORE_REGS	D0/A0/A4
	SET_VAR_CONTEXT

	TSTVAR_L	ostrashed
	beq.b	.quit
	TSTVAR_L	chipmirror
	beq.b	.return

	RESTORE_REGS	D0/A0/A4
	bra	DoExit

.return
    ; jst slaves JSR to this routine so it will just ignore InGameExit
	RESTORE_REGS	D0/A0/A4
	rts
.quit:
	RESTORE_REGS	D0/A0/A4
	
	; jumps to CloseAll without error (new in JST v4.6)

	GETABSOFFSET	CloseAllQuiet,A0
	STORE_REGS	A0
	RTS



; *** Exception occured (illegal instruction, LineA, LineF...)

; Tries to exit with an error message

	IFND	BARFLY

	EXCEPTION_ROUTINE	XX,"Unlisted exception!!","No check"

;	EXCEPTION_ROUTINE	08,"Bus error"
	EXCEPTION_ROUTINE	0C,"Address error"
	EXCEPTION_ROUTINE	10,"Illegal instruction"
	EXCEPTION_ROUTINE	14,"Division by zero"
	EXCEPTION_ROUTINE	18,"CHK, CHK2 instruction"
	EXCEPTION_ROUTINE	1C,"TRAPV or TRAPcc instruction"
	DEF_EXCEPTION_HANDLER	20,"Privilege violation"	; handled by JST
	EXCEPTION_ROUTINE	24,"Uninitialized trace"
	EXCEPTION_ROUTINE	28,"LINE-A emulation"
	EXCEPTION_ROUTINE	2C,"LINE-F emulation"

	EXCEPTION_ROUTINE	30,"Unlisted exception $30"

	EXCEPTION_ROUTINE	34,"Coprocessor protocol violation"
	EXCEPTION_ROUTINE	38,"Format error"
	EXCEPTION_ROUTINE	3C,"Uninitialized interrupt"

	EXCEPTION_ROUTINE	40,"Unlisted exception $40"
	EXCEPTION_ROUTINE	44,"Unlisted exception $44"
	EXCEPTION_ROUTINE	48,"Unlisted exception $48"
	EXCEPTION_ROUTINE	4C,"Unlisted exception $4C"
	EXCEPTION_ROUTINE	50,"Unlisted exception $50"
	EXCEPTION_ROUTINE	54,"Unlisted exception $54"
	EXCEPTION_ROUTINE	58,"Unlisted exception $58"
	EXCEPTION_ROUTINE	5C,"Unlisted exception $5C"

	EXCEPTION_ROUTINE	60,"Spurious interrupt"
	EXCEPTION_ROUTINE	64,"Unitialized level 1 interrupt"
	EXCEPTION_ROUTINE	68,"Unitialized level 2 interrupt (Kb, CIA)"
	EXCEPTION_ROUTINE	6C,"Unitialized level 3 interrupt (VBL, Copper)"
	EXCEPTION_ROUTINE	70,"Unitialized level 4 interrupt (Audio)"
	EXCEPTION_ROUTINE	74,"Unitialized level 5 interrupt"
	EXCEPTION_ROUTINE	78,"Unitialized level 6 interrupt"

	EXCEPTION_ROUTINE	7C,"level 7 interrupt (NMI)"
	EXCEPTION_ROUTINE	80,"Uninitialized TRAP #$0"
	EXCEPTION_ROUTINE	84,"Uninitialized TRAP #$1"
	EXCEPTION_ROUTINE	88,"Uninitialized TRAP #$2"
	EXCEPTION_ROUTINE	8C,"Uninitialized TRAP #$3"
	EXCEPTION_ROUTINE	90,"Uninitialized TRAP #$4"
	EXCEPTION_ROUTINE	94,"Uninitialized TRAP #$5"
	EXCEPTION_ROUTINE	98,"Uninitialized TRAP #$6"
	EXCEPTION_ROUTINE	9C,"Uninitialized TRAP #$7"
	EXCEPTION_ROUTINE	A0,"Uninitialized TRAP #$8"
	EXCEPTION_ROUTINE	A4,"Uninitialized TRAP #$9"
	EXCEPTION_ROUTINE	A8,"Uninitialized TRAP #$A"
	EXCEPTION_ROUTINE	AC,"Uninitialized TRAP #$B"
	EXCEPTION_ROUTINE	B0,"Uninitialized TRAP #$C"
	EXCEPTION_ROUTINE	B4,"Uninitialized TRAP #$D"
	EXCEPTION_ROUTINE	B8,"Uninitialized TRAP #$E"
	EXCEPTION_ROUTINE	BC,"Uninitialized TRAP #$F"

	EXCEPTION_ROUTINE	E4,"MMU illegal operation error"
	EXCEPTION_ROUTINE	E8,"MMU access violation error"
	ELSE
	EXCEPTION_ROUTINE	XX,<Unlisted exception!!>,<No check>

	EXCEPTION_ROUTINE	08,<Illegal instruction>
	EXCEPTION_ROUTINE	0C,<Address error>
	EXCEPTION_ROUTINE	10,<Illegal instruction>
	EXCEPTION_ROUTINE	14,<Division by zero>
	EXCEPTION_ROUTINE	18,<CHK, CHK2 instruction>
	EXCEPTION_ROUTINE	1C,<TRAPV or TRAPcc instruction>
	DEF_EXCEPTION_HANDLER	20,<Privilege violation>	; handled by JST
	EXCEPTION_ROUTINE	24,<Uninitialized trace>
	EXCEPTION_ROUTINE	28,<LINE-A emulation>
	EXCEPTION_ROUTINE	2C,<LINE-F emulation>

	EXCEPTION_ROUTINE	30,<Unlisted exception $30>

	EXCEPTION_ROUTINE	34,<Coprocessor protocol violation>
	EXCEPTION_ROUTINE	38,<Format error>
	EXCEPTION_ROUTINE	3C,<Uninitialized interrupt>

	EXCEPTION_ROUTINE	40,<Unlisted exception $40>
	EXCEPTION_ROUTINE	44,<Unlisted exception $44>
	EXCEPTION_ROUTINE	48,<Unlisted exception $48>
	EXCEPTION_ROUTINE	4C,<Unlisted exception $4C>
	EXCEPTION_ROUTINE	50,<Unlisted exception $50>
	EXCEPTION_ROUTINE	54,<Unlisted exception $54>
	EXCEPTION_ROUTINE	58,<Unlisted exception $58>
	EXCEPTION_ROUTINE	5C,<Unlisted exception $5C>

	EXCEPTION_ROUTINE	60,<Spurious interrupt>
	EXCEPTION_ROUTINE	64,<Unitialized level 1 interrupt>
	EXCEPTION_ROUTINE	68,<Unitialized level 2 interrupt (Kb, CIA)>
	EXCEPTION_ROUTINE	6C,<Unitialized level 3 interrupt (VBL, Copper)>
	EXCEPTION_ROUTINE	70,<Unitialized level 4 interrupt (Audio)>
	EXCEPTION_ROUTINE	74,<Unitialized level 5 interrupt>
	EXCEPTION_ROUTINE	78,<Unitialized level 6 interrupt>

	EXCEPTION_ROUTINE	7C,<level 7 interrupt (NMI)>
	EXCEPTION_ROUTINE	80,<Uninitialized TRAP #$0>
	EXCEPTION_ROUTINE	84,<Uninitialized TRAP #$1>
	EXCEPTION_ROUTINE	88,<Uninitialized TRAP #$2>
	EXCEPTION_ROUTINE	8C,<Uninitialized TRAP #$3>
	EXCEPTION_ROUTINE	90,<Uninitialized TRAP #$4>
	EXCEPTION_ROUTINE	94,<Uninitialized TRAP #$5>
	EXCEPTION_ROUTINE	98,<Uninitialized TRAP #$6>
	EXCEPTION_ROUTINE	9C,<Uninitialized TRAP #$7>
	EXCEPTION_ROUTINE	A0,<Uninitialized TRAP #$8>
	EXCEPTION_ROUTINE	A4,<Uninitialized TRAP #$9>
	EXCEPTION_ROUTINE	A8,<Uninitialized TRAP #$A>
	EXCEPTION_ROUTINE	AC,<Uninitialized TRAP #$B>
	EXCEPTION_ROUTINE	B0,<Uninitialized TRAP #$C>
	EXCEPTION_ROUTINE	B4,<Uninitialized TRAP #$D>
	EXCEPTION_ROUTINE	B8,<Uninitialized TRAP #$E>
	EXCEPTION_ROUTINE	BC,<Uninitialized TRAP #$F>

	EXCEPTION_ROUTINE	E4,<MMU illegal operation error>
	EXCEPTION_ROUTINE	E8,<MMU access violation error>
	ENDC

Exception20:
	STORE_REGS	D0/A0
	LEA	Exception20(PC),A0
	MOVE.L	$20.W,D0
	CMP.L	D0,A0
	RESTORE_REGS	D0/A0
	BEQ.S	TryPatch20

	MOVE.L	$20.W,-(A7)
	RTS

; We will try to see if the opcode which triggered
; the exception is known, and if we can fix it
; by replacing movesr by moveccr
; this was necessary to allow MeGaLoMania to run
; but can be useful in the JST degrader mode (PRIVILEGE function)
; (orsr, andsr not supported at the moment)

TryPatch20:
	STORE_REGS	A0
	move.l	6(A7),A0	; reads PC
	STORE_REGS	D0-D1

	move.w	(A0),D0		; gets opcode
	move.w	D0,D1		; full opcode
	and.w	#$FFF0,D0	; opcode without register number

	cmp.w	#$40C0,D0	; move sr,Dx
	beq	.movesrdx

	cmp.w	#$40F8,D1
	beq	.movesrw

	cmp.w	#$40F9,D1
	bne	HandleException

.movesrw
.movesrdx
	or.w	#$0200,D1	; move sr -> move ccr
	move.w	D1,(A0)		; corrects the code
	JSRGEN	FlushCachesHard	; cache flush

	RESTORE_REGS	D0-D1
	RESTORE_REGS	A0
	rte

Exception08
;In:
;	D0.L = Faulted PC
;	D1.L = Accessed Memory
;	D2.W = Access Mode
;	D3.W = Size

	JSRGEN	EnterDebugger	; access fault

	SET_VAR_CONTEXT

	LEA	.PCBuffer(PC),A1
	MOVE.L	D0,(A1)
	LEA	.MemoryBuffer(PC),A1
	MOVE.L	D1,(A1)
	LEA	.SizeAndAccessMode(PC),A1
	MOVE	D2,(A1)
	LEA	.Size(PC),A1
	MOVE	D3,(A1)

	LEA	.PCBuffer(PC),A1
	MOVE.L	(A1),D0
	JSR	HEX2ASCII(pc)

	LEA	.MemoryBuffer(PC),A1
	MOVE.L	(A1),D0
	JSR	HEX2ASCII(pc)

	MOVE	.SizeAndAccessMode(PC),D0
	LEA	.SizeAndAccessMode(PC),A0

	CMP	#MMU_ACCESS_READ,D0
	BNE.S	.TestWrite
	
	LEA	.ReadStr(PC),A1
	BRA.S	.GetSize

.TestWrite
	CMP	#MMU_ACCESS_WRITE,D0
	BNE.S	.ReadModifyWrite
	
	LEA	.WriteStr(PC),A1
	BRA.S	.GetSize

.ReadModifyWrite
	LEA	.ReadMWriteStr(PC),A1
	
.GetSize
	MOVE	.Size(PC),D0
	CMP	#MMU_SIZE_BYTE,D0
	BNE.S	.TestWord

	LEA	.ByteStr(PC),A2
	BRA.S	.CopyStrings

.TestWord
	CMP	#MMU_SIZE_WORD,D0
	BNE.S	.TestLong

	LEA	.WordStr(PC),A2
	BRA.S	.CopyStrings

.TestLong
	CMP	#MMU_SIZE_LONG,D0
	BNE.S	.DPMOVE16

	LEA	.LongStr(PC),A2
	BRA.S	.CopyStrings

.DPMOVE16
	LEA	.DPMOVE16Str(PC),A2

.CopyStrings
.COPY_LEN
	MOVE.B	(A2)+,(A0)+
	TST.B	(A2)
	BNE.S	.COPY_LEN

	ADDQ.L	#1,A0
	
.COPY_ACCESS
	MOVE.B	(A1)+,(A0)+
	TST.B	(A1)
	BNE.S	.COPY_ACCESS
	
	LEAVAR	message_ptr,A0
	LEA	.MMUErrorMessage(PC),A1
	MOVE.L	A1,(A0)
	
	RESTORE_REGS	D0-A6
	BRA	HandleException

.MMUErrorMessage
	DC.B	"Illegal memory access: >"
.SizeAndAccessMode
	DCB.B	41," "
	DC.B	10,10
	DC.B	"PC:              "
.PCBuffer
	DCB.L	2,0
	DC.B	10,"memory accessed: "
.MemoryBuffer
	DCB.L	2,0
	DC.B	10,10,0
	CNOP	0,4

.Size
	DC.W	0

.LongStr
	DC.B	"long",0
	CNOP	0,4
.WordStr
	DC.B	"word",0
	CNOP	0,4
.ByteStr
	DC.B	"byte",0
	CNOP	0,4
.DPMOVE16Str
	DC.B	"double precision MOVE16",0
	CNOP	0,4

.ReadStr
	DC.B	"read",0
.WriteStr
	DC.B	"write",0
.ReadMWriteStr
	DC.B	"read modify write",0
	CNOP	0,4

HEX2ASCII
	STORE_REGS	D0-D2/A0-A1
	
	LEA	.ASCIITable(PC),A0
	MOVEQ	#7,D1
	MOVEQ	#0,D2
	
.CONVERT
	ROL.L	#4,D0
	MOVE.B	D0,D2
	AND.B	#$0F,D2
	MOVE.B	(A0,D2.W),(A1)+
	DBF	D1,.CONVERT

	RESTORE_REGS	D0-D2/A0-A1
	RTS

.ASCIITable
	DC.B	"0123456789ABCDEF"
	CNOP	0,4


IGNORE_VECTOR:MACRO
	STORE_REGS	A0/A4

	SET_VAR_CONTEXT
	SETVAR_W	#$\1,last_interrupt

	LEA	IgnoreVector\1(PC),A0
	CMP.L	$\1.W,A0
	RESTORE_REGS	A0/A4
	BEQ.S	.IGNORE
	
	MOVE.L	$\1.W,-(A7)
	RTS
.IGNORE
	ENDM

; Disk interrupt (level1)

IgnoreVector64:
;	BSR	TEST_QUIT
;	BSR	TEST_ICONIFY
;	BSR	TEST_FREEZE
	IGNORE_VECTOR	64
	move.w	#0007,intreq+$DFF000
	rte

; Keyboard interrupt and other shit

IgnoreVector68:
	BSR	TEST_QUIT
	BSR	TEST_ICONIFY
	BSR	TEST_FREEZE

	IGNORE_VECTOR	68

	BSET	#$06,$BFEE01
	STORE_REGS	D0
	MOVEQ	#3,D0
	JSRGEN	BeamDelay
	RESTORE_REGS	D0
	BCLR	#$06,$BFEE01
	move.w	#0008,intreq+$DFF000	; acknowledge interrupt
	TST.B	$BFED01
	rte

; VBlank vector

; read the joypad here and inject keyboard instead
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

HANDLE_BUTTON:MACRO
	GETVAR_B	joy\3_\2_keycode,D2	; keycode=0: don't test
	beq.b	.no\2\3twice
	
	btst	#JPB_BTN_\1,d0
	beq.b	.no\2\3
	btst	#JPB_BTN_\1,d3
	bne.b	.no\2\3twice
	; press
	bset	#JPB_BTN_\1,d3	; set "previous" flag
	pea	.no\2\3twice(pc)
	TSTVAR_L	verbose_flag
	beq	.send\3		; no bsr, return address has been pushed
	move.w	#\4,$DFF180
	bra	.send\3
.no\2\3
	btst	#JPB_BTN_\1,d3
;	beq.b	.no\2\3twice
	; released
	beq.b	.no\2\3twice		; 0: not set
	bset	#7,d2
	bclr	#JPB_BTN_\1,d3
	bsr	.send\3
.no\2\3twice
	ENDM
	
HANDLE_FWDBWD:MACRO
	GETVAR_B	joy\1_fwdbwd_keycode,D2
	beq			.endfwdbwd\1
	
	TSTVAR_B	joy\1_fwdbwd_active
	bne			.fwdbwdactive\1
	
	BTST		#JPB_BTN_REVERSE,D0
	beq.b			.endfwdbwd\1
	BTST		#JPB_BTN_FORWARD,D0
	beq.b			.endfwdbwd\1
	
	SETVAR_B	#1, joy\1_fwdbwd_active
	moveq.l	#\1,d0		; joy0 or joy1
	pea	.endfwdbwd\1(pc)
	TSTVAR_L	verbose_flag
	beq	.send\1
	move.w	#\2,$DFF180
	bra	.send\1		
.fwdbwdactive\1
	
	BTST		#JPB_BTN_REVERSE,D0
	bne.b			.endfwdbwd\1
	BTST		#JPB_BTN_FORWARD,D0
	bne.b			.endfwdbwd\1
	
	SETVAR_B	#0, joy\1_fwdbwd_active
	moveq.l	#\1,d0		; joy0 or joy1
	bset		#7,D2
	bsr			.send\1
.endfwdbwd\1
	ENDM
	
ALL_FRONT_BUTTONS_MASK = JPF_BTN_RED|JPF_BTN_BLU|JPF_BTN_PLAY|JPF_BTN_GRN|JPF_BTN_YEL

; VBlank vector

IgnoreVector6C:
	BSR	TEST_QUIT
	BSR	TEST_ICONIFY
	BSR	TEST_FREEZE
	; now check if joypad is enabled so we can emulate keys with joypad
	STORE_REGS	A4
	SET_VAR_CONTEXT
	TSTVAR_B	joypad_type
	beq	.nojoypad
	STORE_REGS	D0-D4
	
	GETVAR_B	joypad_type,D1
	btst	#0,D1
	beq		.joy1


	; port 0: (mouse port)
	moveq.l	#0,D0
	bsr	_read_joystick
	
	move.l	d0,d1
	and.l	#ALL_FRONT_BUTTONS_MASK,d1
	cmp.l	#ALL_FRONT_BUTTONS_MASK,d1
	beq	.quit

	GETVAR_L	previous_joy0_state,D3

	move.b		#0,D1	
	bsr vm	
	bsr	vk
	TSTVAR_B	vk_wason
	bne			.out
	
	HANDLE_BUTTON	RED,red,0,$800
	HANDLE_BUTTON	PLAY,play,0,$808
	HANDLE_BUTTON	BLU,blue,0,$008
	HANDLE_BUTTON	YEL,yellow,0,$880
	HANDLE_BUTTON	GRN,green,0,$080
	HANDLE_BUTTON	RIGHT,right,0,$0
	HANDLE_BUTTON	LEFT,left,0,$0
	HANDLE_BUTTON	UP,up,0,$0
	HANDLE_BUTTON	DOWN,down,0,$0
	
	HANDLE_FWDBWD	0,$F08
	
	HANDLE_BUTTON	REVERSE,bwd,0,$80F
	HANDLE_BUTTON	FORWARD,fwd,0,$088

	
.joy1
	GETVAR_B	joypad_type,D1
	btst	#1,D1
	beq		.out

	; port 1 (joystick port)

	moveq.l	#1,D0

	bsr	_read_joystick
	move.l	d0,d1
	and.l	#ALL_FRONT_BUTTONS_MASK,d1
	cmp.l	#ALL_FRONT_BUTTONS_MASK,d1
	beq		.quit

	GETVAR_L	previous_joy1_state,D3

	move.b		#1,D1
	bsr vm
	bsr	vk
	TSTVAR_B	vk_wason
	bne			.out

	
	HANDLE_BUTTON	RED,red,1,$F00
	HANDLE_BUTTON	PLAY,play,1,$F0F
	
	HANDLE_BUTTON	BLU,blue,1,$00f
	HANDLE_BUTTON	YEL,yellow,1,$ff0
	HANDLE_BUTTON	GRN,green,1,$0f0
	
	HANDLE_BUTTON	RIGHT,right,1,0
	HANDLE_BUTTON	LEFT,left,1,0
	HANDLE_BUTTON	UP,up,1,0
	HANDLE_BUTTON	DOWN,down,1,0	
	
	HANDLE_FWDBWD	1,$0AA

	HANDLE_BUTTON	REVERSE,bwd,1,$0ff
	HANDLE_BUTTON	FORWARD,fwd,1,$0Af

.out	
	RESTORE_REGS	D0-D4
.nojoypad
	RESTORE_REGS	A4
	IGNORE_VECTOR	6C
	move.w	#$0070,intreq+$DFF000
	rte
	
.quit
	JSRGEN	InGameExit
; < d2: keycode to send
.send0
;Upon sending key event, update the joy previous state
	move.b	d2,d0
	SETVAR_L	D3,previous_joy0_state
	bra	send_key_event
;Upon sending key event, update the joy previous state
.send1
	move.b	d2,d0
	SETVAR_L	D3,previous_joy1_state
	bra	send_key_event

; Copper vector

IgnoreVector70:
	IGNORE_VECTOR	70
	move.w	#$0780,intreq+$DFF000
	rte

; Audio vector

IgnoreVector74:
	IGNORE_VECTOR	74
	move.w	#$1800,intreq+$DFF000
	rte

; CIA vector

IgnoreVector78:
	IGNORE_VECTOR	78
	move.w	#$2000,intreq+$DFF000
	btst.b	#0,$BFDD00		; acknowledge CIA-B Timer A interrupt
	RTE

IgnoreVector7C:
;	BSR	TEST_QUIT
;	BSR	TEST_ICONIFY
	BSR	TEST_FREEZENMI

	STORE_REGS	D0/A0

	LEA	IgnoreVector7C(PC),A0
	MOVE.L	$7C.W,D0
	CMP.L	D0,A0
	BEQ.S	.IGNORE
	
	RESTORE_REGS	D0/A0
	MOVE.L	$7C.W,-(A7)
	RTS

.IGNORE
	RESTORE_REGS	D0/A0
	
	move.w	#$7FFF,intreq+$DFF000	; ???
	RTE

; Right mouse button freeze test

TEST_FREEZE_RMB:
	STORE_REGS	D0/A4
	SET_VAR_CONTEXT

	TSTVAR_L	freezermb_flag
	beq.b	.NO_FREEZE
	TSTVAR_L	debugger
	BEQ.S	.NO_DEBUGGER


	MOVE.W	potinp+$DFF000,D0
	BTST	#10,D0		; right mouse button
	bne.b	.NO_FREEZE
	MOVE	#$CC01,potgo+$DFF000	; resets for next time

	RESTORE_REGS	D0/A4

	GETVAR_L	debugger_nmi,(A7)
	RTS
	
.NO_FREEZE
.NO_DEBUGGER
	RESTORE_REGS	D0/A0/A4
	RTS

; test to see if freezekey was pressed
; JOTD improved the routine, now it only uses D0
; and is statistically less jumping/branching

TEST_FREEZE:
	STORE_REGS	A4
	
	SET_VAR_CONTEXT

	TSTVAR_L	debugger
	BNE.S	.DEBUGGER

	; always exit there for better maintainability

.NO_FREEZE
	RESTORE_REGS	A4
	RTS

; debugger is installed, we'll check the keyboard, then

.DEBUGGER:
	STORE_REGS	D0

	MOVE.B	$BFEC01,D0
	ROR.B	#1,D0
	NOT.B	D0
	
	SUBVAR_B	freezekeynum,D0
	BEQ.S	.FREEZE

	; key not pressed, check right mouse freeze enable flag

.NO_FREEZE_KB
	TSTVAR_L	freezermb_flag
	bne.b	.TRY_RMB_FREEZE

	; nothing pressed, exit
.NO_FREEZE2
	RESTORE_REGS	D0
	bra.b	.NO_FREEZE

	; right mouse freeze enable, check right mouse pressed

.TRY_RMB_FREEZE:
	MOVE.W	potinp+$DFF000,D0
	BTST	#10,D0		; right mouse button
	bne.b	.NO_FREEZE2	; not there
	MOVE	#$CC01,potgo+$DFF000	; resets for next time

	; freezekey or right mouse pressed: freeze
.FREEZE
	RESTORE_REGS	D0

	; modifies return address

	GETVAR_L	debugger_nmi,4(A7)
	RESTORE_REGS	A4
	RTS
	
TEST_FREEZENMI
	STORE_REGS	A4
	SET_VAR_CONTEXT

	TSTVAR_L	debugger
	BEQ.S	.NO_DEBUGGER

	GETVAR_L	debugger_nmi,4(A7)
	RESTORE_REGS	A4
	RTS

.NO_FREEZE
.NO_DEBUGGER
	RESTORE_REGS	D0-D1/A4
	RTS

TEST_QUIT:
	STORE_REGS	D0/A4
	SET_VAR_CONTEXT

	GETVAR_B	quitkeynum,D0
	BEQ.S	.NO_QUIT
	
	MOVE.B	$BFEC01,D0
	ROR.B	#1,D0
	NOT.B	D0

	SUBVAR_B	quitkeynum,D0
	BNE.S	.NO_QUIT

	MOVE.L	USER_QUIT_CODE(PC),D0
	BEQ.S	.DO_QUIT
	
	STORE_REGS	A0
	MOVE.L	D0,A0
	MOVEQ	#0,D0
	JSR	(A0)
	RESTORE_REGS	A0

	TST.L	D0
	BNE.S	.NO_QUIT
	
.DO_QUIT
	JSRGEN	InGameExit
	
.NO_QUIT
	RESTORE_REGS	D0/A4
	RTS
	
TEST_ICONIFY
	STORE_REGS	D0-D1/A0/A4
	SET_VAR_CONTEXT

	GETVAR_B	iconifykeynum,D1
	BEQ.S	.NO_ICONIFY
	
	MOVE.B	$BFEC01,D0
	ROR.B	#1,D0
	NOT.B	D0
	
	SUB.B	D0,D1
	BNE.S	.NO_ICONIFY
		
	MOVE.L	USER_ICONIFY_CODE(PC),D0
	BEQ.S	.DO_ICONIFY
	
	MOVE.L	D0,A0
	MOVEQ	#0,D0
	JSR	(A0)
	
	TST.L	D0
	BNE.S	.NO_ICONIFY
	
.DO_ICONIFY
	JSRGEN	InGameIconify
	
	MOVE.L	USER_ICONIFY_CODE(PC),D0
	BEQ.S	.NO_USER_CODE
	
	MOVE.L	D0,A0
	MOVEQ	#1,D0
	JSR	(A0)

.NO_USER_CODE
.NO_ICONIFY
	RESTORE_REGS	D0-D1/A0/A4

	RTS

HandleException:
	; *** save cpu registers properly in relocated area
	; *** (heavy routine)

	; *** save status register and PC

	STORE_REGS	A4
	SET_VAR_CONTEXT
	SETVAR_W	4(A7),game_sr
	SETVAR_L	6(A7),game_pc
	RESTORE_REGS	A4

	; *** registers D0-A6 (tricky)

	STORE_REGS	A0
	STORE_REGS	A4
	SET_VAR_CONTEXT
	LEAVAR	cpuregs,A0
	RESTORE_REGS	A4
	movem.l	D0-A6,(A0)
	RESTORE_REGS	A0

	SET_VAR_CONTEXT


	STORE_REGS	A1
	LEAVAR	cpuregs,A1
	move.l	A0,$20(A1)	; A0 was stored wrong
	RESTORE_REGS	A1

	; changed in v4.1: already disable MMU here

	JSRGEN	DisableMMU

	; *** stack (tricky)

	JSRGEN	GetAttnFlags
	btst	#AFB_68020,D0
	beq	.sk1
	addq.l	#2,A7		; removes last stackframe word on 68020+ !!
.sk1

	addq.l	#6,A7		; skips SR and PC for 680x0

	LEAVAR	cpuregs,A1
	move.l	A7,$3C(A1)	; update stack value in cpuregs string

	LEAVAR	crash_stack,A0
	moveq.l	#7,D0
.stkcpy
	move.l	(A7)+,(A0)+
	dbf	D0,.stkcpy

	lea	(-$20,A7),A7

	GETVAR_L	game_pc,A0
	LEAVAR	pc8,A1
	moveq.l	#7,D0
.pc8cpy
	move.l	(A0)+,(A1)+
	dbf	D0,.pc8cpy

	; *** custom registers

	bsr	LogCustomRegs

TryQuit:
	SET_VAR_CONTEXT

	TSTVAR_L	chipmirror
	bne	UrgentExit
	
	; There is no memory to restore chipmem
	; We display a screen characteristic of a crash
	; save the registers in $150000 (or $50000 if 1MB chipmem)
	; and loop forever

.forever
	LEAVAR	cpuregs,A0
	movem.l	(A0),D0-A7
	movem.l	D0-A7,$150000	; some poor information about the crash

	move.w	#$F00,$DFF180
	move.w	#$0F0,$DFF180
	move.w	#$00F,$DFF180
	bra.b	.forever


DoExit:
	STORE_REGS	D0-A6

	SET_VAR_CONTEXT

	move.l	#14,D0
	LEAVAR	cpuregs,A0

.stk2buf:
	move.l	(sp)+,(A0)+
	dbf	D0,.stk2buf

	lea	(-$3C,sp),sp

	JSRGEN	GetSR
	SETVAR_W	D0,game_sr
	btst	#13,D0
	beq.b	.user
	move.w	#$2700,SR
.user
	bsr	LogCustomRegs

	; *** acknowledge keyboard

	bsr	AckKeyboard

	; *** resets system LED/Filter

	bsr	RelFun_Priv_SetLed

	; *** test if system chipmem was saved

	TSTVAR_L	chipmirror
	bne	UrgentExit

	rts			; don't quit if chip mirror has not been allocated

LogCustomRegs:
	STORE_REGS	D0/D1/A4/A5
	SET_VAR_CONTEXT
	lea	$DFF000,A5
	move.w	#$8000,D1

	move.w	intenar(A5),D0
	or.w	D1,D0
	SETVAR_W	D0,game_intena
	move.w	intreqr(A5),D0
	or.w	D1,D0
	SETVAR_W	D0,game_intreq
	move.w	dmaconr(A5),D0
	or.w	D1,D0
	SETVAR_W	D0,game_dmacon
	move.w	adkconr(A5),D0
	or.w	D1,D0
	SETVAR_W	D0,game_adkcon
	RESTORE_REGS	D0/D1/A4/A5
	rts

UrgentExit:
	; trashes A1 and A0 and sets context in A4
	TRAP2SUPERVISOR

	; temporary stack will be used from now
	
	LEAVAR	tmpstack,A7

	; no interrupts please

	move.w	#$2700,SR

	; re-disable MMU here

	JSRGEN	DisableMMU
	
	GETVAR_L	debugger,A0
	CMP.L	#HRTMon,A0
	BNE.S	.NotHRTMon
	
	GETVAR_L	debuggerbase,A0
	CLR.L	(60,A0)		; resets resload to zero

.NotHRTMon
	; choose between closing all without error
	; and with error

	TSTVAR_L	message_ptr
	beq.b	.noerr
	GETABSOFFSET	CloseAllWithError,A2
	bra.b	.exit_chosen
.noerr
	GETABSOFFSET	CloseAllQuiet,A2
.exit_chosen
	lea	.closeall_routine(pc),A0
	move.l	A2,(A0)

	; *** waits blitter operations to complete

	JSRGEN	WaitBlit

	; *** freezes everything

	JSRGEN	FreezeAll

	; *** screen becomes black

	JSRGEN	BlackScreen

	; *** sound stopped (again)

	STOP_SOUND

	GETVAR_L	maxchip,D7
	beq	.nomem
	TSTVAR_L	chipmirror
	beq	.nomem

	GETVAR_L	chipmirror,A1
	lea	$0.W,A0

	TSTVAR_L	debug_flag
	bne.b	.chipswap	; debug on: makes a chip image of the memory

	GETVAR_L	maxchip,D7
	GETVAR_L	saved_mem_ptr,D2
	GETVAR_L	endsaved_mem_ptr,D3

.chipcopy
	tst.l	D2
	beq.b	.nosave		; normal copy operation

	cmp.l	A0,D2
	bcc.b	.nosave		; A0 < allocated buff : copy memory

	cmp.l	A0,D3
	bcs.b	.nosave		; A0 > end allocated : copy memory

	addq.l	#4,A0		; next address
	bra.b	.next

.nosave
	move.l	(A1)+,(A0)+	; copy buffer into chipmem
.next
	cmp.l	D7,A0
	bne	.chipcopy

	bra	.chipend

.chipswap
	move.l	(A1),D0
	move.l	(A0),(A1)+
	move.l	D0,(A0)+
	cmp.l	D7,A0
	bne	.chipswap

.chipend
	JSRGEN	RestoreMMU

	; *** the OS is up now, and forever ;-)

	CLRVAR_L	ostrashed
	CLRVAR_L	ostotrash

	; now that the system memory is restored,
	; restore the old user stack

	; stops floppy disk drive activity if any

	move.b	#$7F,$BFD100

	; restore custom & CIA registers but not the DMA yet

	JSRGEN	RestoreCustomNoDMA

	; *** resets some timer impossible to restore
	; *** any other way (v2.7d, thanks OSEmu code, Mr Larmer!)

	move.b	#$1f,$bfed01	; clears all interrupts
	MOVE.B	#$8F,$BFED01	; sets all interrupts but FLAG
	tst.B	$BFED01		; reads and clears ICR

	JSRGEN	RestoreCIARegs

	; clears all color registers

	JSRGEN	BlackScreen

	; *** waits some time, acknowledges keyboard

	bsr	AckKeyboard
	move.l	#500,D0
	JSRGEN	BeamDelay
	bsr	AckKeyboard

	; sets system VBR again for 68010+

	move.w	#$2700,SR

	bsr	RalfVBRRestore

	; we'll use the system userstack from now

	GETVAR_L	system_userstack,A7

	; disables interrupts using the OS

	move.l	ABSEXECBASE.W,A6
	JSRLIB	Disable

	; switches to user state, with the definite user & super stackpointers

	move.w	#$2000,SR
	JSRABS	UserMode
    
	; enhance CPU caches, restore VBR

	JSRABS	EnhanceCpu

	; re-enables interrupts
	; now that the user stack / super stack / VBR / VBR vectors are up

	move.l	ABSEXECBASE.W,A6
	JSRLIB	Enable

	; *** enhance graphics shit & DMA

	JSRABS	EnhanceGfx

	; *** acknowledge keyboard

;;;	bsr	AckKeyboard

	JSRABS	SetClockLoad
	TSTVAR_L	debug_flag
	beq.b	.free

	JSRABS	LogChipMirror
	JSRABS	LogCustomMirror
	JSRABS	LogRegisters
	JSRABS	LogExtMemory

.free
	SET_VAR_CONTEXT

	GETVAR_L	chipsize,D0
	beq.b	.nomem
	GETVAR_L	chipmirror,A1

	JSRABS	FreeTheMemory

.nomem
	GETVAR_L	saved_mem_size,D0
	beq.b	.nochipfree
	GETVAR_L	saved_mem_ptr,A1
	JSRABS	FreeTheMemory
	CLRVAR_L	saved_mem_size
.nochipfree

	move.l	.closeall_routine(pc),-(A7)
	RTS	; jumps to either CloseAll or CloseAllQuiet

.closeall_routine:
	dc.l	0
; *** In-Game call to the OS

; in: A5: pointer on the OS routine to be called
; out:D0:0 if success, -1 else

RelFun_InGameOSCall:
	STORE_REGS	D2-A6
	STORE_REGS	A4
	SET_VAR_CONTEXT
	LEAVAR	cpuregs,A6
	RESTORE_REGS	A4
	movem.l	D0-A5,(A6)	; save registers for OS call

	SET_VAR_CONTEXT

	; *** save the old ostrashed flag

	GETVAR_L	ostrashed,D0

	; *** and check if we are not in a OS alive state

	SETVAR_L	D0,ostotrash
	beq	IGOC_OsAlive 

	; *** the OS is dead

	; *** check if this is possible

	TSTVAR_L	chipmirror
	beq	IGOC_Error

	; *** save game SR

	JSRGEN	GetSR
	SETVAR_W	D0,game_sr

	; *** save game supervisor stack pointer

	JSRGEN	GetSSP
	SETVAR_L	D0,game_superstack

	; *** switches in supervisor mode (A0/A1 trashed)

	TRAP2SUPERVISOR

	; *** save game stack pointers : user and supervisor

	move.l	USP,A0
	SETVAR_L	A0,game_userstack

	; *** from now we use the temporary stack

	LEAVAR	tmpstack,A7

	; *** save in-game custom registers

	LEAVAR	game_hwregs,A1
	bsr	GetCustomRegs
	LEAVAR	game_ciaregs,A1
	bsr	GetCiaRegs

	; *** acknowledge keyboard

	bsr	AckKeyboard

	; *** wait for blitter operation to conclude

	JSRGEN	WaitBlit

	; *** freeze all interrupts, sound and DMA
	; *** (cos all the chip will be replaced)

	JSRGEN	FreezeAll

	; *** update VBR table
	
	lea	$DFF000,A5
;	move.w	#$4000,intena(A5)		; no interrupts
;	move.w	#$01FF,dmacon(A5)		; no display, no copper DMA...
;	move.w	#$7FFF,intreq(A5)
	move.w	#$0000,color(A5)		; black screen

	STOP_SOUND

	; *** waits

	move.l	#40,D0
	JSRGEN	BeamDelay

	JSRGEN	DisableMMU
GotoOS:
	; *** swap buffer and chip memory (goto OS)

	GETVAR_L	chipmirror,A1
	lea	$0.W,A0
	GETVAR_L	maxchip,D7
	GETVAR_L	saved_mem_ptr,D2
	GETVAR_L	endsaved_mem_ptr,D3
.chipswap
	tst.l	D2
	beq.b	.nosave		; normal copy operation

	cmp.l	A0,D2
	bcc.b	.nosave		; A0 < allocated buff : copy memory

	cmp.l	A0,D3
;	bcs.b	.nosave		; A0 > end allocated : copy memory
	BLE.S	.nosave

	addq.l	#4,A0		; next address
	bra.b	.next

.nosave
	move.l	(A1),D0
	move.l	(A0),(A1)+
	move.l	D0,(A0)+
.next

	cmp.l	D7,A0
	bne.b	.chipswap
	
	; *** restore MMU translation register

	JSRGEN	RestoreMMU

	; *** flushes caches

	JSRGEN	FlushCachesHard

	; *** resets system LED/Filter

	bsr	RelFun_Priv_SetLed

	; *** restores custom & CIA
	; *** no display, no sprites, no sound... please

	JSRGEN	RestoreCIARegs
	JSRGEN	RestoreCustomNoDMA

	; *** sets VBR to system value (we are in supervisor mode, so O.K.)

	bsr	RalfVBRRestore

	; *** all interrupts allowed from now
	
	move	#$2000,SR

	; *** switches to user mode 
	; *** (restores system user & supervisor) stack pointer

	JSRABS	UserMode

	; *** do the OS stuff (in user state, anyway)

	CLRVAR_L	ostrashed

	; *** we're now OK to execute any system-friendly
	; *** procedure, in user mode

	; *** calls the user routine

	LEAVAR	cpuregs,A6
	movem.l (A6),D0-A5	; restores user function context
	
	jsr	(A4)		; JUMPS TO OS ACTIVE ROUTINE!! <------------------------

	SET_VAR_CONTEXT		; restores variables context
	LEAVAR	cpuregs,A6
	movem.l	D0-D1,(A6)	; D0 and D1 are return values, saved

;;	JSRGEN	SaveCustomRegs	;Save the new
;;	JSRGEN	SaveCIARegs	;Added by Ralf
				;Removed by Jeff, crashed :)

	CALL_MMUCode	SaveMMU

;;;;TEMP	JSRABS	MarkNewEntryValid	; new file added: mark in in the MMU table

	LEAVAR	tmpstack,A7

	; *** switches in SuperState again (warning: stack change here, A0/A1 trashed)

	TRAP2SUPERVISOR

	; *** temporary stack used from now

	LEAVAR	tmpstack,A7

;	TSTVAR_W just_resumed
;	beq.b	.noresume
;	JSRGEN	Priv_SaveSysCustomCIARegs
;	CLRVAR_W	just_resumed
;.noresume

	; *** freeze, motherfucker

	JSRGEN	FreezeAll

	; *** re-installs bus error handler, but forget about zero page

	move.l	$8.W,-(A7)
	bsr	InstallBusErrorVector
	move.l	(A7)+,$8.W


	; *** sets JST VBR handler
	
	bsr	RalfVBRSet

	; *** No MMU translations..., not needed, already disabled

	JSRABS	DisableMMU

GotoGAME:
	SETVAR_L	#1,ostrashed

	; *** waits

	move.l	#40,D0
	JSRGEN	BeamDelay

	; *** swap buffer and chip memory (goto GAME)

	GETVAR_L	chipmirror,A1
	lea	$0.W,A0
	GETVAR_L	maxchip,D7
	GETVAR_L	saved_mem_ptr,D2
	GETVAR_L	endsaved_mem_ptr,D3

.chipswap
	tst.l	D2
	beq.b	.nosave		; normal copy operation

	cmp.l	A0,D2
	bcc.b	.nosave		; A0 < allocated buff : copy memory

	cmp.l	A0,D3
	bcs.b	.nosave		; A0 > end allocated : copy memory

	addq.l	#4,A0		; next chip address
	bra.b	.next
.nosave
	move.l	(A1),D0
	move.l	(A0),(A1)+
	move.l	D0,(A0)+

.next
	cmp.l	D7,A0
	bne.b	.chipswap

	; *** flushes caches

	JSRGEN	FlushCachesHard

	; *** sets PAL or NTSC again/restore gfx

	JSRGEN	ResetDisplay

	; *** restore in-game custom registers

	LEAVAR	game_ciaregs,A1
	bsr	SetCiaRegs
	LEAVAR	game_hwregs,A1
	bsr	SetCustomRegs

	STOP_SOUND

	JSRGEN	EnableMMU

	; *** find unchanged VBR vectors and set JST handler vectors
	; *** into unchanged locations

	; *** restores game supervisor stack

	GETVAR_L	game_superstack,A7

	; *** restores game user stack pointer

	GETVAR_L	game_userstack,A0
	move.l	A0,USP

	; *** restores old SR

	GETVAR_W	game_sr,D1
	move	D1,sr

IGOC_Exit:
	SET_VAR_CONTEXT

	; calls user after switch function

	GETVAR_L	after_switch_cb,D0
	beq.b	.nousercb
	pea	.nousercb(pc)
	move.l	(A7),A0
	move.l	D0,(A7)
	RTS
.nousercb

	LEAVAR	cpuregs,A6
	movem.l	(A6),D0-D1	; set return values and exit In-Game OS Call
IGOC_RestoreRegsRts:
	bsr	CustomRegsAdjust
	RESTORE_REGS	D2-A6
	rts

IGOC_Error:
	SET_VAR_CONTEXT
	LEAVAR	cpuregs,A6
	move.l	#-1,(A6)	; appears in D0
	bra.b	IGOC_Exit

	; the OS is alive, just call the OS function

IGOC_OsAlive:
	SET_VAR_CONTEXT
	LEAVAR	cpuregs,A6	; gets cpuregs in A6

	movem.l	(A6),D0-A5	; restore registers for OS call
	jsr	(A4)		; function address (exception of A4 use)

	bra.b	IGOC_RestoreRegsRts

; *** Initialize trackdisk.device emulation

RelFun_InitTrackDisk:
	lea	diskio(PC),A1
	rts

; use _FlushCachesSys whenever possible to avoid trouble

RelFun_FlushCachesHard:
	
	STORE_REGS	D0/A4/A5
	SET_VAR_CONTEXT

	JSRGEN	GetAttnFlags	; leave JSRGEN here
	BTST	#AFB_68020,D0
	beq.b	.exit		; no 68020: no cache flush


	lea	SupCode(pc),A5
	bsr	call_as_supervisor
.exit
	RESTORE_REGS	D0/A4/A5
	RTS	

; < A5 routine to call
call_as_supervisor:
	; freeze interrupts, using INTENA (not SR as we don't know if we're running in supervisor mode)
	; just in case some interrupt code the vector in $B8
	SAVE_INTENA_AND_FREEZE
	move.l	$B8.W,-(sp)
	move.l	A5,$B8.W	; $E vector of trap
	TRAP	#$E
	move.l	(sp)+,$B8.W
	RESTORE_INTENA
	rts

; *** Flushes the caches
; *** called from supervisor state only

SupCode
	STORE_REGS	D0/D1/A5/A6
	BSR.B	FlushCachesSup
	RESTORE_REGS	D0/D1/A5/A6
	RTE

; *** Flushes the caches

FlushCachesSup:
	ori	#$700,SR
	JSRGEN	GetAttnFlags	; leave JSRGEN here
	BTST	#AFB_68020,D0
	BEQ.B	.no020			; tested outside the function but better safe than sorry
	MC68020
	MOVEC	CACR,D1		: gets current CACR register
	MC68000
	BSET	#CACRB_ClearI,D1
	BTST	#AFB_68030,D0
	BEQ.B	.no030
	BSET	#CACRB_ClearD,D1
.no030:
	MC68020
	MOVEC	D1,CACR
	MC68000
	BTST	#AFB_68040,D0
	BEQ.B	.no040

	MC68040
	CPUSHA	BC
	MC68000
.no020
.no040:
	RTS	

; *** Replaces DoIO function to read from a virtual floppy

RelFun_TrackLoadFast:
RelFun_TrackLoad:
	STORE_REGS	D0-D3/A0-A1
	lea	diskio(PC),A1	; removed

	move.w	$1C(A1),D0
	bclr	#$F,D0		; sometimes $800x as a command

	cmp.w	#2,D0
	bne	TL_End		; Not read -> not supported

	move.l	$24(A1),D2	; Length
	move.l	$2C(A1),D1	; Offset
	move.l	$28(A1),A0	; Buffer

	lsr.l	#8,D2
	lsr.l	#1,D2		; D2 / 512
	lsr.l	#8,D1
	lsr.l	#1,D1		; D1 / 512

	move.l	tdunit(pc),D0	; Drive
	moveq.l	#0,D3		; Read data
	JSRGEN	ReadRobSectors

	clr.b	$1F(A1)		; All is OK

TL_End:
	RESTORE_REGS	D0-D3/A0-A1
	moveq.l	#0,D0
	rts


; *** Gets disk pointer address

; in: D0: disk number (from 0 to ...)
; out:D0: disk pointer

RelFun_GetDiskPointer:
	STORE_REGS	A4
	SET_VAR_CONTEXT

	and.l	#$F,D0
	TSTVAR_L	lowmem_flag
	bne	RunTime_LowmemGDP
	TSTVAR_L	hdload_flag
	beq.b	.ramload		; always return 0 (same disks)	
	moveq	#0,D0
.ramload
	add.w	D0,D0
	add.w	D0,D0

	LEAVAR	diskbuffers,A4
	move.l	(A4,D0.W),D0

	RESTORE_REGS	A4
	rts


; *** Sets user routine to be executed on exit
; in: A0: pointer (null can be passed, nothing will be done on exit)

RelFun_SetExitRoutine:
	STORE_REGS	A4
	SET_VAR_CONTEXT
	SETVAR_L	A0,doonexit
	RESTORE_REGS	A4
	rts

; *** Sets trackdisk.device emulation unit
; in: D0: unit (0 to number of disks). Not limited to 3

; if this routine is not called, the unit will be 0 (default)

RelFun_SetTDUnit:
	STORE_REGS	D0/A0
	lea	tdunit(pc),A0
	and.l	#$F,D0
	move.l	D0,(A0)
	RESTORE_REGS	D0/A0
	rts

RelFun_ReadFileFromImage
; In:
;	D0 = Len or -1 to read all
;	D1 = Disk NR
;	A0 = *FileName
;	A1 = *Buffer
; Out:
;	D0 = Read length
;	D0 = -1 Error occured

;--------------------------------------------------------------------------
; Macros and defines

BLOCK_SIZE	EQU	512
ROOT_BLOCK	EQU	880
BOOT_BLOCK	EQU	0

ROOT_IDENT1	EQU	2
ROOT_IDENT2	EQU	1
FILE_IDENT1	EQU	2
FILE_IDENT2	EQU	-3
FILEL_IDENT1	EQU	16
FILEL_IDENT2	EQU	-3
DIR_IDENT1	EQU	2
DIR_IDENT2	EQU	2

READ_DISK	MACRO
	IFNE	NARG-4
		FAIL	"READ_DISK: Too few arguments"
	ENDIF
	
	STORE_REGS	D0-D2/A0
	
	MOVE.L	\1,D0
	MOVE.L	\2,D1
	MOVE.L	\3,D2
	LEA	\4(PC),A0
	JSRGEN	ReadDiskPart
	
	RESTORE_REGS	D0-D2/A0
	ENDM

READ_BLOCK	MACRO
	IFNE	NARG-2
		FAIL	"READ_BLOCK: Too few arguments"
	ENDIF
	
	READ_DISK	\1,#BLOCK_SIZE,\2,BlockBuffer
	
	ENDM

;--------------------------------------------------------------------------
	STORE_REGS	D1-A6

	MOVE.L	D0,D7
	MOVE.L	D1,D6
	MOVE.L	A0,A5
	MOVE.L	A1,A4	; ReadFileFromImage does not use A4 context

	bsr	RegisterNameA0

	LEA	BlockBuffer(PC),A1
	
	READ_BLOCK	D6,#BOOT_BLOCK*BLOCK_SIZE

	CMP.L	#"DOS"<<8+0,(A1)
	BNE	.NO_DOS

	LEA	(A1),A2

	READ_BLOCK	D6,#ROOT_BLOCK*BLOCK_SIZE

	CMP.L	#ROOT_IDENT1,(A2)
	BNE	.NO_DOS
	CMP.L	#ROOT_IDENT2,508(A2)
	BNE	.NO_DOS

	MOVE.L	20(A2),D0
	CLR.L	20(A2)
	LEA	(A2),A3
	MOVEQ	#0,D1
	MOVEQ	#BLOCK_SIZE/4-1,D2

.ROOT_CHECKSUM
	SUB.L	(A3)+,D1
	DBF	D2,.ROOT_CHECKSUM

	MOVE.L	D0,20(A2)

	CMP.L	D0,D1	
	BNE	.NO_DOS

	MOVE.L	D6,D0
	LEA	(A5),A0
	BSR	LocateFile
	TST.L	D0
	BMI	.NO_DOS

	MOVE.L	D0,D2
	MULU	#BLOCK_SIZE,D2
	LEA	(A1),A2

	READ_BLOCK	D6,D2
	
	MOVE.L	324(A2),D1

	TST.L	D7
	BMI	.SIZE_OKAY

	CMP.L	D7,D1
	BLT	.SIZE_OKAY
	
	MOVE.L	D7,D1

.SIZE_OKAY
	MOVE.L	D1,D7
	
.BLOCK_OKAY
	MOVE.L	#71*4,D2

.COPY_DATA
	MOVE.L	24(A2,D2.L),D3
	MULU	#BLOCK_SIZE,D3

	MOVE.L	#488,D4

	READ_DISK	D6,#BLOCK_SIZE,D3,DataBuffer

	LEA	DataBuffer+24(PC),A3
	CMP.L	#488,D1
	BGT.S	.NO_SIZE_SWAP

	MOVE.L	D1,D4
	
.NO_SIZE_SWAP
.COPY_BLOCK
	MOVE.B	(A3)+,(A4)+	; ReadFileFromImage
	SUBQ.L	#1,D1
	SUBQ.L	#1,D4
	BNE	.COPY_BLOCK

	TST.L	D1
	BEQ	.FILE_COPIED

	SUBQ.L	#4,D2
	BPL	.COPY_DATA

	MOVE.L	504(A2),D2
	MULU	#BLOCK_SIZE,D2

	READ_BLOCK	D6,D2

	CMP.L	#FILEL_IDENT1,(A2)
	BNE	.ERROR
	
	CMP.L	#FILEL_IDENT2,508(A2)
	BNE	.ERROR

	BRA	.BLOCK_OKAY

.FILE_COPIED
	MOVE.L	D7,D0
	RESTORE_REGS	D1-A6
	RTS
.ERROR
.NO_DOS
	RESTORE_REGS	D1-A6
	MOVEQ	#-1,D0
	RTS

LocateFile
; IN:
;	D0 = Disk NR
;	A0 = *FileName
; OUT:
;	D0 = FileHeaderBlock
;	D0 = -1 <- Error

	STORE_REGS	D1-A6

	MOVE.L	D0,D6
	
	BSR	CountDirectories
	TST.L	D0
	BMI	.ERROR

	MOVE.L	#ROOT_BLOCK,D2
	MOVE.L	NAME_START(PC),D1
	LEA	(A0,D1.L),A2
	
.DIR_LOOP
	TST.L	D0
	BEQ	.DIR_LOOP_FINISHED
	
	STORE_REGS	D0/A0
	LEA	(A2),A0
	BSR	BuildHash
	MOVE.L	D0,D3
	RESTORE_REGS	D0/A0

	ADDQ.L	#6,D3
	LSL.L	#2,D3
	MULU	#BLOCK_SIZE,D2

	LEA	BlockBuffer(PC),A3

	READ_BLOCK	D6,D2
	MOVE.L	(A3,D3.L),D2

.FIND_DIR
	MOVE.L	D2,D3
	MULU	#BLOCK_SIZE,D3

	READ_BLOCK	D6,D3

	CMP.L	#DIR_IDENT1,(A3)
	BNE	.NO_DIR
	CMP.L	#DIR_IDENT2,508(A3)
	BNE	.NO_DIR

	STORE_REGS	D0/A0
	
	LEA	(A3),A0
	BSR	BuildBlockCheckSum
	MOVE.L	D0,D3
	
	RESTORE_REGS	D0/A0
	TST.L	D3
	BMI	.ERROR
	
	STORE_REGS	D0/D1/A0/A1
	LEA	(A2),A0
	BSR	CountStringLen
	LEA	433(A3),A1
	MOVEQ	#0,D1
	MOVE.B	432(A3),D1
	CMP.L	D1,D0
	BCC.S	.COMPARE_DIR_NAMES
	MOVE.L	D1,D0

.COMPARE_DIR_NAMES
	BSR	CompareNames
	MOVE.L	D0,D3
	RESTORE_REGS	D0/D1/A0/A1
	
	TST.L	D3
	BEQ	.FOUND_DIR

.NO_DIR
	TST.L	496(A3)
	BEQ	.ERROR
	MOVE.L	496(A3),D2
	BRA	.FIND_DIR

.FOUND_DIR
.SKIP_NAME
	CMP.B	#"/",(A2)+
	BNE.S	.SKIP_NAME

	SUBQ.L	#1,D0
	BRA	.DIR_LOOP

.DIR_LOOP_FINISHED
	STORE_REGS	D0/A0
	LEA	(A2),A0
	BSR	BuildHash
	MOVE.L	D0,D3
	RESTORE_REGS	D0/A0

	ADDQ.L	#6,D3
	LSL.L	#2,D3
	MULU	#BLOCK_SIZE,D2

	LEA	BlockBuffer(PC),A3
	
	READ_BLOCK	D6,D2
	
	MOVE.L	(A3,D3.L),D2

.FIND_FILE
	MOVE.L	D2,D3
	MULU	#BLOCK_SIZE,D3

	READ_BLOCK	D6,D3

	CMP.L	#FILE_IDENT1,(A3)
	BNE	.NO_FILE
	CMP.L	#FILE_IDENT2,508(A3)
	BNE	.NO_FILE

	STORE_REGS	D0/A0
	
	LEA	(A3),A0
	BSR	BuildBlockCheckSum
	MOVE.L	D0,D3
	
	RESTORE_REGS	D0/A0
	TST.L	D3
	BMI	.ERROR
	
	STORE_REGS	D0/D1/A0/A1
	LEA	(A2),A0
	BSR	CountStringLen
	LEA	433(A3),A1
	MOVEQ	#0,D1
	MOVE.B	432(A3),D1
	CMP.L	D1,D0
	BCC.S	.COMPARE_FILE_NAMES
	MOVE.L	D1,D0

.COMPARE_FILE_NAMES
	BSR	CompareNames
	MOVE.L	D0,D3
	RESTORE_REGS	D0/D1/A0/A1
	
	TST.L	D3
	BEQ	.FOUND_FILE

.NO_FILE
	TST.L	496(A3)
	BEQ	.ERROR
	MOVE.L	496(A3),D2
	BRA	.FIND_FILE

.FOUND_FILE
	MOVE.L	D2,D0	
	RESTORE_REGS	D1-A6
	
	bsr	RegisterNameA0
	RTS

.ERROR
	MOVEQ	#-1,D0
	RESTORE_REGS	D1-A6
	RTS

CountStringLen
; IN:
;	A0 = *Name, null or "/" terminated
; OUT:
;	D0 = = Len

	MOVEQ	#-1,D0

.COUNT_LOOP
	ADDQ.L	#1,D0
	
	TST.B	(A0,D0.L)
	BEQ.S	.ALL_DONE
	CMP.B	#"/",(A0,D0.L)
	BNE.S	.COUNT_LOOP

.ALL_DONE
	RTS

BuildBlockCheckSum
; IN:
;	A0 = *Block
; OUT:
;	D0 = 0 Okay
;	D0 = -1 Error

	STORE_REGS	D1/D2/A0/A1
	
	MOVE.L	20(A0),D0
	CLR.L	20(A0)
	LEA	(A0),A1
	MOVEQ	#0,D1
	MOVEQ	#BLOCK_SIZE/4-1,D2

.ROOT_CHECKSUM
	SUB.L	(A1)+,D1
	DBF	D2,.ROOT_CHECKSUM

	MOVE.L	D0,20(A0)

	SUB.L	D1,D0
	BEQ.S	.OKAY

	MOVEQ	#-1,D0

.OKAY
	RESTORE_REGS	D1/D2/A0/A1
	RTS

CompareNames
; IN:
;	A0 = *Name1
;	A1 = *Name2
;	D0 = Len
; OUT:
;	D0 = 0 Names are equal
;	D0 = -1 Names differ

	STORE_REGS	D1/D2/A0/A1

	MOVE.L	D0,D2

.COMPARE_LOOP
	MOVE.B	(A0)+,D0
	MOVE.B	(A1)+,D1

	CMP.B	#"a",D0
	BLT.S	.CHECK_D1
	CMP.B	#"z",D0
	BGT.S	.CHECK_D1
	BCLR	#5,D0

.CHECK_D1
	CMP.B	#"a",D1
	BLT.S	.COMPARE
	CMP.B	#"z",D1
	BGT.S	.COMPARE
	BCLR	#5,D1

.COMPARE
	CMP.B	D0,D1
	BNE.S	.NAMES_DIFFER

	SUBQ.L	#1,D2
	BNE.S	.COMPARE_LOOP

.NAMES_EQUAL
	MOVEQ	#0,D0

.EXIT
	RESTORE_REGS	D2/D1/A0/A1
	RTS

.NAMES_DIFFER
	MOVEQ	#-1,D0
	BRA.S	.EXIT

BuildHash
; IN:
;	A0 = *Name
; OUT:
;	D0 = Hashvalue
;
	STORE_REGS	D1/A0/A1

	LEA	(A0),A1
	MOVEQ	#0,D0

.COUNT_LOOP
	TST.B	(A1)
	BEQ.S	.FINISHED_LEN
	CMP.B	#"/",(A1)+
	BEQ.S	.FINISHED_LEN
	ADDQ.L	#1,D0
	BRA.S	.COUNT_LOOP

.FINISHED_LEN
.HASH_LOOP
	TST.B	(A0)
	BEQ.S	.FINISHED_HASH
	CMP.B	#"/",(A0)
	BEQ.S	.FINISHED_HASH
	
	MOVE.B	(A0)+,D1
	CMP.B	#"z",D1
	BGT.S	.OKAY
	CMP.B	#"a",D1
	BLT.S	.OKAY
	BCLR	#5,D1
	
.OKAY
	MULU	#13,D0
	ADD.L	D1,D0
	AND.L	#$7FF,D0

	BRA.S	.HASH_LOOP
	
.FINISHED_HASH
	DIVU	#72,D0
	SWAP	D0
	AND.L	#$FFFF,D0
	RESTORE_REGS	D1/A0/A1
	RTS
	
CountDirectories
; IN:
;	A0 = *FileName
; OUT:
;	D0 = Num of Directories in name
;	D0 = -1 No file in name

	STORE_REGS	A0-A1
	
	LEA	NAME_START(PC),A1
	MOVEQ	#0,D0

.FIND_VOLUME
	CMP.B	#":",(A0,D0.L)
	BEQ.S	.VOLUME_SKIPPED
	ADDQ.L	#1,D0
	TST.B	(A0,D0.L)
	BNE.S	.FIND_VOLUME

	CLR.L	(A1)
	BRA.S	.VOLUME_DONE
	
.VOLUME_SKIPPED
	ADDQ.L	#1,D0
	MOVE.L	D0,(A1)
	
.VOLUME_DONE
	MOVE.L	(A1),D0
	LEA	(A0,D0.L),A0
	MOVEQ	#0,D0
	
.SEARCH_DIRS
	TST.B	(A0)
	BEQ.S	.ALL_DONE
	
	CMP.B	#"/",(A0)+
	BNE.S	.SEARCH_DIRS
	
	ADDQ.L	#1,D0
	BRA.S	.SEARCH_DIRS

.ALL_DONE
	CMP.B	#"/",-1(A0)
	BNE.S	.NAME_OKAY

	MOVEQ	#-1,D0

.NAME_OKAY
	RESTORE_REGS	A0-A1
	RTS


NAME_START
	DC.L	0

BlockBuffer
	DCB.B	BLOCK_SIZE,0

DataBuffer
	DCB.B	BLOCK_SIZE,0


	IFD	XXXXXXX

; < D0: address looked for

FindReadAccess:
	STORE_REGS
	lea	varcontext(pc),A4
	SETVAR_L	D0,SearchAdr
	lea	FRA_Trace(pc),A0
	JSRGEN	SetTraceVector	
	RESTORE_REGS
	RTS

FRA_Trace:
	; save registers

	STORE_REGS	A4

	lea	varcontext(pc),A4
	movem.l	D0-D7,RelVar_Dn(A4)
	MOVEM.L	A0-A6,RelVar_An(A4)	; wrong value of A4 stored (context)
	move.l	(A7),(RelVar_An+16,A4)	; finaly saves correct A4

	MOVE.L	6(A7),A0	; gets current PC
	SETVAR_L	A0,PCAdr
	CLR.L D0
	MOVE.B	(A0),D0
	AND.B	#$F0,D0
	LSR.B	#4,D0
	TST.W	D0
	BEQ	.notMove
	CMP.B	#3,D0
	BGT	.notMove

	MOVE.W	(A0),D0
	MOVE.W	#12,D1
	BSR	.get2Bits
	BSR	.getSize
	SETVAR_W	D1,Size

.getRest:
	MOVE.W	#3,D1
	BSR	.get3Bits
	SETVAR_W	D1,AdrMode

	MOVE.W	#0,D1
	BSR	.get3Bits
	SETVAR_W	D1,RegNumber

	BRA	.switchadrmode

.notMove

	CMP.B   #%1011,D0
	BNE	.out
	MOVE.W	(A0),D0
	BTST	#8,D0
	BNE	.out		  ; securite

	MOVE.W	#6,D1
	BSR	.get2Bits
	MOVEQ #1,D0
	ASL.W D1,D0
	SETVAR_W	D0,Size

	BRA	.getRest

.switchadrmode

	GETVAR_W	AdrMode,D1
	add.w	D1,D1
	add.w	D1,D1
	JSR	.CaseAdr(PC,D1.W)
	BRA	.Interpret

	cnop	0,4
.CaseAdr
	BRA	.popout
	cnop	0,4
	BRA	.popout
	cnop	0,4
	BRA	.adrIAn
	cnop	0,4
	BRA	.adrIAn
	cnop	0,4
	BRA	.adrIAnM
	cnop	0,4
	BRA	.adrIAnD
	cnop	0,4
	BRA	.adrIAnDX

	GETVAR_W	RegNumber,D1
	add.w	D1,D1
	add.w	D1,D1
	JMP	.CaseReg(PC,D1.W)

	cnop	0,4
.CaseReg
	BRA .adrabsW
	cnop	0,4
	BRA .adrabsL
	cnop	0,4
	BRA	.adrpCD
	cnop	0,4
	BRA	.adrpCDX
	cnop	0,4
	BRA	.out

.adrImm
.Interpret
	GETVAR_L	BaseAdr,A0
	GETVAR_L	SearchAdr,A1
	CMP.L A1,A0
	BHI	.out
	CLR.L D0
	GETVAR_W	Size,D0
	ADD.L D0,A0
	CMP.L A1,A0
	BHI	.ok	; found
.out       		; exit, restore all and RTE
	movem.l	RelVar_Dn(A4),D0-D7
	movem.l	RelVar_An(A4),A0-A6
	RESTORE_REGS	A4
	RTE
.popout
	addq.l	#4,A7
	bra.b	.out

.ok
	GETVAR_L	PCAdr,D0
	JSRGEN	EnterDebugger
	nop
	nop
	nop
	bra.b	.out

.adra	GETVAR_W	RegNumber,D1
	ASL.W #2,D1
	LEAVAR	An,A0
	MOVE.L	(A0,D1.W),A1
	RTS

.adrIAn	BSR	.adra
	SETVAR_L	A1,BaseAdr
	RTS

.adrIAnM	BSR	.adra
	CLR.L D0
	GETVAR_W	Size,D0
	SUB.L D0,A1
	SETVAR_L	A1,BaseAdr
	RTS

.adrIAnD
	BSR	.adra
	GETVAR_L	PCAdr,A0
	MOVE.W	2(A0),D0
	EXT.L D0
	ADD.L D0,A1
	SETVAR_L	A1,BaseAdr
	RTS

.adrx	
	STORE_REGS	D1-D2/A0-A1

	GETVAR_L	PCAdr,A0
	MOVE.B	2(A0),D0
	MOVE.W	#4,D1
	BSR	.get3Bits
	BTST	#7,D0 			 ; D0 = Extention byte
	BEQ	.XD

	LEAVAR	An,A1
	ASL	#2,D1
	MOVE.L	(A1,D1.W),D2
	BRA	.adrIAnDX_1

.XD 
	LEAVAR	Dn,A1
	ASL	#2,D1
	MOVE.L	(A1,D1.W),D2

.adrIAnDX_1
	BTST	#3,D0
	BNE	.ExtL				 ; B si size = Long
	AND.L #$FFFF,D2
.ExtL
	MOVE.L	D2,A1

	MOVE.B	3(A0),D0
	EXT.W D0
	EXT.L D0
	ADD.L D0,A1

	MOVE.L	A1,D0
	RESTORE_REGS	D1-D2/A0-A1
	RTS

.adrIAnDX
	BSR	.adra
	BSR	.adrx
	ADD.L D0,A1
	SETVAR_L	A1,BaseAdr
	RTS

.adrabsW	
	GETVAR_L	PCAdr,A0
	MOVE.L	#0,A1
	MOVE.W	2(A0),A1
	SETVAR_L	A1,BaseAdr
	RTS

.adrabsL
	GETVAR_L	PCAdr,A0
	MOVE.L	2(A0),A1
	SETVAR_L	A1,BaseAdr
	RTS

.adrpCD	
	GETVAR_L	PCAdr,A0
	CLR.L D0
	MOVE.W	2(A0),D0
	EXT.L D0
	ADD.L D0,A0
	ADD.L #2,A0
	SETVAR_L	A0,BaseAdr
	RTS

.adrpCDX
	GETVAR_L	PCAdr,A0
	BSR	.adrx
	ADD.L D0,A0
	ADD.L #2,A0
	SETVAR_L	A0,BaseAdr
	RTS

.get2Bits
	MOVEM.L	D0/D2,-(A7)
	MOVE.W	D0,D2
	LSR.W D1,D2
	ANDI.W	#$3,D2
	MOVE.W	D2,D1
	MOVEM.L	(A7)+,D0/D2
	RTS

.get3Bits
	MOVEM.L	D0/D2,-(A7)
	MOVE.W	D0,D2
	LSR.W D1,D2
	ANDI.W	#$7,D2
	MOVE.W	D2,D1
	MOVEM.L	(A7)+,D0/D2
	RTS

.getSize
	CMP.W #1,D1
	BNE	.not_byte
	MOVE.W	#1,D1
	RTS

.not_byte
	CMP.W #3,D1
	BNE	.not_word
	MOVE.W	#2,D1
	RTS

.not_word
	MOVE.W	#4,D1
	RTS

	STRUCTURE	vars,0
	ULONG	RelVar_PCAdr
	ULONG	RelVar_BaseAdr
	ULONG	RelVar_SearchAdr
	STRUCT	RelVar_Dn,32
	STRUCT	RelVar_An,32
	UWORD	RelVar_Size
	UWORD	RelVar_AdrMode
	UWORD	RelVar_RegNumber
	LABEL	Vars_Size

varcontext:
	blk.b	Vars_Size,0
	ENDC
CustomRegsAdjust:
	STORE_REGS	A6

	lea	$DFF000,A6


	; change display start/stop/width/height
	; to fit what the A500 games expect (new from v4.7)
	; after OS swap, Fire & Ice display is trashed

	; this setup was copied from HRTMon sources (now GPL) by A. Malek

;	move.w	#$3081,$8e(a6)
;	move.w	#$30c1,$90(a6)
;	move.w	#$0038,$92(a6)
;	move.w	#$00d0,$94(a6)
;	move.w	#0,$108(a6)
;	move.w	#0,$10a(a6)
;	move.w	#0,$106(a6)
;	move.w	#0,$1fc(a6)
;	move.w	#$9200,$100(a6)


;	MOVE.W #$C40,$DFF104
;	MOVE.W #$0,$DFF106
;	MOVE.W #$0,$DFF10C
;	MOVE.W #$0,$DFF1E2
;	MOVE.W #$0,$DFF1FC
;	MOVE.W #$0,$DFF1C4
;	MOVE.W #$0,$DFF1C6

;	move.w	#$0,(bplcon1,A6)	; new from JST v4.7
;	move.w	#$0,(bplcon2,A6)
;	move.w	#$0,(bplcon3,A6)

	RESTORE_REGS	A6
	rts

; Stores name to the last file loaded name buffer
; (debug purposes)
; < A0: name to copy

RegisterNameA0:
	STORE_REGS	D0-D2/A1/A4
	SET_VAR_CONTEXT
	move.l	A0,D0
	LEAVAR	lastfile_buffer,A1
	move.l	A1,D1
	move.l	#100,D2
	JSRGEN	StrncpyAsm
	RESTORE_REGS	D0-D2/A1/A4
	RTS

; *** Read Disk Part
;     Reads a part of a diskfile

; < D0.W disk unit
; < D1.L length in bytes
; < D2.L offset in bytes
; < A0   output buffer

; > D0   0 if everything went OK

RelFun_ReadDiskPart:
	STORE_REGS	D1-D3/A0/A1/A4

	SET_VAR_CONTEXT

	tst.l	D1
	beq	.exit		; nothing to read

	TSTVAR_L	lowmem_flag
	beq.b	.fromfast

	; LOWMEM: we'll need to swap the os

	move.l	A0,A1		; destination

	bsr	SetDiskNumber

	LEAVAR	fname,A0
	JSRGEN	ReadFilePartHD	; partially read diskfile from HD
	tst.l	D0
	bne	RunTime_DiskRead	; diskfile read error (quit JST)	
	
	bra	.exit

.fromfast
	STORE_REGS	D0
	JSRGEN	SetDisk	; in case of HDLOAD, os swap will be done

	SETVAR_W	#1,iconify_lock	; forbid iconify during loading
					; (the diskfile location may have changed)

	RESTORE_REGS	D0

	JSRGEN	GetDiskPointer

	tst.l	D0
	bne.b	.disk_found

	TSTVAR_L	fatal_fileerror
	bne	RunTime_WrongId	; Is the disk allocated? (added this after Ralf report)

	moveq.l	#-1,D0
	bra.b	.exit		; disk problem, but non fatal

.disk_found:
	add.l	D2,D0		; offset
	move.l	D0,A1

	MOVEQ	#0,D3

	MOVE.L	A1,D2		; optimization
	BCLR	#0,D2		; added by Ralf
	BEQ.S	.NEXT

	ADDQ.L	#1,D3
	
.NEXT
	MOVE.L	A0,D2
	BCLR	#0,D2
	BEQ.S	.CHECK
	
	ADDQ.L	#1,D3
	
.CHECK
	BCLR	#0,D3
	BNE.S	.ramcp
	
	BCLR	#0,D1
	BEQ.S	.START_COPY
	
	MOVE.B	(A1)+,(A0)+

.START_COPY
	MOVE.L	D1,D2
	AND.L	#$F,D2
	LSR.L	#4,D1
	BRA.S	.START_UNROLLED_COPY

.UNROLLED_COPY
	MOVE.L	(A1)+,(A0)+
	MOVE.L	(A1)+,(A0)+
	MOVE.L	(A1)+,(A0)+
	MOVE.L	(A1)+,(A0)+
.START_UNROLLED_COPY
	DBF	D1,.UNROLLED_COPY
	
	ADDQ	#1,D1
	SUBQ.L	#1,D1
	BPL.S	.UNROLLED_COPY
	
	MOVE.L	D2,D1
	BEQ.S	.READ_DONE

.ramcp
	move.b	(A1)+,(A0)+
	subq.l	#1,D1		; length
	bne.b	.ramcp

.READ_DONE
	moveq.l	#0,D0

.exit
	
	CLRVAR_W	iconify_lock	; allow iconify again

	RESTORE_REGS	D1-D3/A0/A1/A4
	rts

; SetDiskNumber:
; < D0: number of the disk
; > fname updated
; needs A4 loaded with variables segment
; trashes A0 and D0

SetDiskNumber:
	and.b	#$F,D0
	add.b	#'1',D0
	ADDVAR_L	diskbias,D0

	LEAVAR	fname,A0
	STORE_REGS	D0
	move.l	A0,D0
	JSRGEN	StrlenAsm
	add.l	D0,A0
	RESTORE_REGS	D0
	subq.l	#1,A0			; last char = index
	move.b	D0,(A0)
	rts

; ReadFile. generic version
; < A0: filename
; < A1: buffer
; < D0: command (0: read, 5 length)
; > D0: 0 if OK
; > D1: file length

RelFun_ReadFile:
	STORE_REGS	D4/A0/A4
	move.l	D0,D4		; save D0 for later use
	SET_VAR_CONTEXT

	JSRGEN	ReadFileFast
	tst.l	D0
	bpl.b	.exit			; loaded ok, or size command

	move.l	D4,D0			; restores D0

	LEAVAR	lastfile_buffer,A0	; set by ReadFileFast, relocated filename
	JSRGEN	ReadFileHD		; not found in fast, try from HD
.exit
	RESTORE_REGS	D4/A0/A4
	rts


; ReadFile. Hard Disk version
; Reads a file in the chipmirror or in fast during game

; user directory version

RelFun_ReadUserFileHD:
	STORE_REGS	D2-A6

	SET_VAR_CONTEXT

	CLRVAR_L	fileoffset	; read from offset 0

	bsr	RelocName
	INGAMEOSCALL	ReadUserFileHD

.exit
	RESTORE_REGS	D2-A6
	rts

; game version

RelFun_ReadFileHD:
	STORE_REGS	D2-A6

	SET_VAR_CONTEXT

	CLRVAR_L	fileoffset	; read from offset 0

	bsr	RelocName		; store the filename in a relocated area

	INGAMEOSCALL	ReadFileHD

.exit
	RESTORE_REGS	D2-A6
	rts

.os_error
	moveq.l	#-1,D0
	bra	.exit


; Read user directory
; in D0,D1,A0,A1
; out D0

RelFun_ReadUserDir:
	STORE_REGS	D1-A6

	INGAMEOSCALL	ReadUserDir

.exit
	RESTORE_REGS	D1-A6
	rts

.os_error
	moveq.l	#-1,D0
	bra	.exit



; ReadFilePart. Universal version. Reads partially a file
; Reads a file in the chipmirror or in fast during game

; < D1: length to read (-1: till the end)
; < D2: offset to read from
; < A0: name
; < A1: destination

; > D0: 0 if OK
; > D1: length read if OK

RelFun_ReadFilePart:
	STORE_REGS	A4
	SET_VAR_CONTEXT
	tst.l	D1
	beq	.exit		; nothing to read

	; LOWMEM set -> from HD

	TSTVAR_L	lowmem_flag
	bne.b	.fromhd

.fromfast
	STORE_REGS	D1
	moveq.l	#0,D0			; read
	moveq.l	#0,D1			; 0 bytes
	JSRGEN	ReadFileFast
	RESTORE_REGS	D1
	tst.l	D0
	bne.b	.fromhd			; file read error: try from disk

	STORE_REGS	D1
	moveq.l	#8,D0
	move.l	D2,D1			; offset
	JSRGEN	ReadFileFast	; set offset

	RESTORE_REGS	D1
	moveq.l	#7,D0			; and D1 is the length to read
	JSRGEN	ReadFileFast	; read with offset

	moveq.l	#0,D0		; bugfix (my whdload slaves don't like it)
	bra.b	.exit

.fromhd
	TSTVAR_L	fatal_fileerror
	beq.b	.error

	; LOWMEM/HDLOAD or file not found in cache: we'll need to swap the os

	JSRGEN	ReadFilePartHD	; partially read diskfile from HD
	tst.l	D0
	bne	RunTime_FileRead	; file read error (quit JST)	
	
.exit
	RESTORE_REGS	A4
	rts
.error
	moveq	#1,D0
	bra.b	.exit

; ReadFilePart. Hard Disk version. Reads partially a file
; Reads a file in the chipmirror or in fast during game

; < D1: length to read (-1: till the end)
; < D2: offset to read from
; < A0: name
; < A1: destination

; > D0: 0 if OK
; > D1: length read if OK

RelFun_ReadFilePartHD:
	STORE_REGS	D2-A6

	SET_VAR_CONTEXT

	SETVAR_L	D2,fileoffset	; read from offset 0

	bsr	RelocName		; store the filename in a relocated area

	moveq.l	#0,D0			; don't try to get the length!
	INGAMEOSCALL	ReadFileHD

.exit
	RESTORE_REGS	D2-A6
	rts


; < D1: length written
; < D2: offset
; < A0: file name
; < A1: data source

update_normfile:
	STORE_REGS
	SET_VAR_CONTEXT

	SETVAR_W	#1,iconify_lock	; forbid iconify during diskfile updating

	move.l	D1,D5
	move.l	D2,D6


;;;	SETVAR_L	#0,fileoffset	; read from offset 0

	; we MUST update the file if the file is likely to be
	; loaded again (and still in the memory cache)

	TSTVAR_L	lowmem_flag
	bne.b	.exit		; lowmem is set: no need to update anything

	
	JSRGEN	Priv_SearchDirEntry
	tst.l	(A5)
	beq.b	.exit		; not in the memory: no need to update

	move.l	(A5),A0
	add.l	(4,A5),A0

	; invalidates file: file is marked as dirty
	; the file will be loaded from disk next time
	; and updated in the idbuffer

	st.b	(FNAME_SIZE-1,A0)

.exit
	CLRVAR_W	iconify_lock

	RESTORE_REGS
	rts

; < D1: length written
; < D2: offset
; < A0: file name
; < A1: data source

update_diskfile:
	STORE_REGS
	SET_VAR_CONTEXT

	SETVAR_W	#1,iconify_lock	; forbid iconify during diskfile updating


	move.l	D1,D5
	move.l	D2,D6

	; we MUST update the file/diskfile if the file is likely to be
	; loaded again (and still in the memory cache)

	TSTVAR_L	lowmem_flag
	bne.b	.exit		; lowmem is set: no need to update anything

	; compares both strings, without the last character

	LEAVAR	fname_base,A2
	tst.b	(A2)
	beq.b	.exit		; no diskfiles: exit!

	move.l	A2,D0
	JSRGEN	StrlenAsm
	move.l	D0,D2		; length to compare
	move.l	A0,D0		; just saved filename
	move.l	A2,D1		; diskfile base

	JSRGEN	StrncmpAsm
	tst.l	D0
	bne.b	.exit		; not the diskfile prefix, don't update

	move.l	A0,D0
	JSRGEN	StrlenAsm
	subq.l	#1,D0
	cmp.l	D2,D0
	bne.b	.exit		; could be same prefix but not a diskfile (disk. - disk.info) !

	moveq.l	#0,D0
	move.b	(A0,D2),D0
	sub.b	#'1',D0		; D0: disk index
	bmi.b	.exit		; not a digit after disk prefix (below 0)
	cmp.b	#8,D0
	bcc.b	.exit		; if not a digit, out

	; if HDLOAD is set, only update the memory
	; if the disk index matches current loaded disk

	TSTVAR_L	hdload_flag
	beq.b	.doupdate

	GETVAR_L	current_disk,D3
	cmp.l	D3,D0
	bne.b	.exit		; different: no need to update, will be done on swap
	
.doupdate
	; we NEED to update the diskfile
	
	JSRGEN	GetDiskPointer
	add.l	D6,D0
	move.l	D0,A0		

	GETVAR_L	filesize,D4
	sub.l	D6,D4	; for boundary check
	tst.l	D4
	bmi	RunTime_OutOfBounds	; offset > filesize!
	beq	RunTime_OutOfBounds	; offset = filesize!

	; copy to diskfile: A0=dest buffer, A1=src buffer, D1=length
.copy	
	move.b	(A1)+,(A0)+
	subq.l	#1,D5
	beq.b	.exit			; end of copy
	subq.l	#1,D4
	beq	RunTime_OutOfBounds	; we went over filesize limit!
	bra.b	.copy

.exit
	CLRVAR_W	iconify_lock

	RESTORE_REGS
	rts
	
; < A0: filename to relocate
; > A0: filename, relocated area
RelocName:
	STORE_REGS	A4
	SET_VAR_CONTEXT
	bsr	RegisterNameA0
	LEAVAR	lastfile_buffer,A0
	RESTORE_REGS	A4
	rts

	
; < A0: filename to relocate
; > A0: filename, relocated area, with directory (DATA) dir prepended if needed
RelocNameWithDir:
	STORE_REGS	D0-D1/A1/A4
	SET_VAR_CONTEXT
	bsr	RegisterNameA0
	lea	.relocated_name(pc),A1
	
	LEAVAR_D	fpath,D0
	move.l	A1,D1
	jsr	RelFun_StrcpyAsm

	move.l	A1,D1
	move.l	A0,D2
	jsr	RelFun_AddPart
	
	move.l	A1,A0

	RESTORE_REGS	D0-D1/A1/A4
	rts
.relocated_name
	ds.b	$100
	
; WriteFilePart. Hard Disk version. Writes partially a file

; < D1: length to write (be careful)
; < D2: offset to move to before writing
; < A0: name
; < A1: data source

; > D0: 0 if OK
; > D1: length written if OK

RelFun_WriteFilePartHD:
	STORE_REGS	D2-A6

	SET_VAR_CONTEXT

	; if the file is in JST file cache, tell the WriteFilePartHD to update it

	bsr	update_normfile

	
	SETVAR_L	D2,fileoffset

	bsr	RelocName		; store the filename in a relocated area
	INGAMEOSCALL	WriteFilePartHD

	tst.l	D0
	bne.b	.exit	; error: go out!

	; if the file is a diskfile, update the JST diskfile cache

	bsr	update_diskfile
.exit
	RESTORE_REGS	D2-A6
	rts


; DeleteFile

; in: A0: filename

; game data version. should not be used, but...

RelFun_DeleteFileHD:
	STORE_REGS	D2-A6

	bsr	RelocName		; store the filename in a relocated area

	INGAMEOSCALL	DeleteFileHD

.exit
	RESTORE_REGS	D2-A6
	rts


; game data version. should not be used, but...

RelFun_DeleteUserFileHD:
	STORE_REGS	D2-A6

	bsr	RelocName		; store the filename in a relocated area

	INGAMEOSCALL	DeleteUserFileHD

	RESTORE_REGS	D2-A6
	rts

; WriteFile. Hard Disk version
; Writes a file from the chipmirror or from fast during game

; in: A0: filename
;     A1: buffer
;     D1: length to write

; User data version

RelFun_WriteUserFileHD:
	STORE_REGS	D2-A6

	SET_VAR_CONTEXT

;;	tst.l	D1
;;	beq	.exit	; Jeff: removed the 0 size check (OSEmu uses it)


	cmp.l	#MAX_FILE_WRITE,D1
	bcc	RunTime_FileTooBig

	CLRVAR_L	fileoffset			; from offset 0

	bsr	update_normfile


	
	bsr	RelocName		; store the filename in a relocated area

	INGAMEOSCALL	WriteUserFileHD

.exit
	RESTORE_REGS	D2-A6
	rts



; Game data version (should not be used, but...)

RelFun_WriteFileHD:
	STORE_REGS	D2-A6
	
	SET_VAR_CONTEXT

;;	tst.l	D1
;;	beq	.exit	; Jeff: removed the 0 size check (OSEmu uses it)

	cmp.l	#MAX_FILE_WRITE,D1
	bcc	RunTime_FileTooBig

	CLRVAR_L	fileoffset

	bsr	update_normfile

	
	bsr	RelocName		; store the filename in a relocated area (debug)
	
	INGAMEOSCALL	WriteTheFileHD

.exit
	RESTORE_REGS	D2-A6
	rts



	RUNTIME_ERROR_ROUTINE	FileTooBig,"Write(User)FileHD(): File too big"

; ReadFile. RAMDisk version
; Historically uses the Rob Northern file interface.
; which is really stupid since it overcomplexifies the generic loader

; used in:

; Cannon Fodder 2, Darkmere, Sensible Golf, Sensible Soccer, Road Rash...

; Different versions may exist: D1 can be meaningless. In this case, set it to -1.
; Some games only use commands 0 and 5
;
; Darkmere uses 0, 5, 6, 7, 8 extensively.

; <A0: Filename
; <A1: Buffer
; <D0: command:
; <D1: misc
; <D0:

; >D0:
; >D1: various, depending on command

;  0: Read		
;		in : D1: # bytes (-1: all), A0: filename, A1: buffer
;		out: D0: 0 if OK,  D1: length read

;  1: Write (disabled)
;		in: D1: # bytes          , A0: filename, A1: buffer
;		out: D0: 0 if OK,  D1: length written

;  2: ???
;  3: Read directory (disabled)	                       A0: 
;  4: Format floppy (disabled)	
;  5: Get Length
;		in : A0: filename
;		out: D0: 1 if found, 0 else, D1: length

;  6: ???

;  7: Read last file w/ offset
;		in : D1: # bytes          , A0: scratch  ,A1: buffer
; 		out: D0: ???

;  8: Set offset on last file
;		in : D1: offset           , A0: scratch  ,A1: scratch
;		out: D0: offset


RelFun_ReadFileFast:
	STORE_REGS	D1-A6
	SET_VAR_CONTEXT

	SETVAR_W	#1,iconify_lock	; forbid iconify during diskfile updating

	bsr	RegisterNameA0

	move.w	D0,D7		; command
	move.l	D1,D6		; length info

	; *** check for special commands without name lookup

	cmp.b	#$6,D7	; command 6: ???
	beq	.op6

	cmp.b	#$7,D7	; command 7: read with offset
	beq	.offsetread

	cmp.b	#$8,D7	; command 8: set offset
	beq	.setoffset

	tst.b	D7
	bne	.2

;;	tst.l	D6
;;	beq	.nullread

.2
	
	; search the file in the fileid struct
	
	STORE_REGS	D0
	moveq.l	#0,D0
	JSRGEN	Priv_SearchDirEntry
	RESTORE_REGS	D0

	tst.l	(A5)
	beq	.notfound	; file not found
	tst.w	8(A5)
	bne	.notfound	; directory: not found
	
	; we note down the file reference for commands 7 and 8

	lea	openedfile(pc),A6
	move.l	A5,(A6)
	lea	currentoffset(pc),A6
	clr.l	(A6)

	; ** now we test the commands

	tst.w	D7	; command
	beq	.read

	cmp.b	#$5,D7	; command 5: check file
	beq	.cmdfilelen

	bra	.wrongcmd


; *** operation 7: read with offset

.offsetread
	move.l	openedfile(pc),A3	; pointer on the fileid -> opened file
	cmp.l	#0,A3
	beq	.exit			; problem

	clr.l	(A7)

	move.l	(A3),A3			; pointer on the file data
	move.l	currentoffset(pc),D2
	beq	.5
	move.l	#$7C,(A7)	; magic value ????
.5
	add.l	D2,A3			; add offset
	move.l	A3,A6			; calculates

	tst.l	D6
	bpl.b	.lengthok

	; v3.1: D6 was $FFFFFFFF, bugfix!

	move.l	openedfile(pc),A5	; pointer on the fileid -> opened file
	move.l	(4,A5),D6
	sub.l	D2,D6			; length = total length - current offset
	move.l	(A5),A5
.lengthok:
	add.l	D6,A6			; end address
	bra	.copy

; *** operation 8: set offset

.setoffset
	RELOC_MOVEL	D6,currentoffset
	bra	.quit
	
; *** read data

.read
	; ** Copy it in the buffer

	move.l	(A5),A3		; buffer
	tst.l	4(A5)
	beq.b	.exit
	tst.l	D6
	bpl	.limitlen

	move.l	4(A5),A6	; length
	add.l	A3,A6		; end address
	bra	.copy
.limitlen
	move.l	(A5),A6		; buffer
	add.l	D6,A6		; end address
.copy
	STORE_REGS	D0

	move.l	A6,D0
	sub.l	A3,D0		; length to read

	move.l	A3,A0		; source. A1 is already set properly

	JSRGEN	CopyMem
	RESTORE_REGS	D0
.exit
	tst.l	D6
	bmi	.filelen		; returns the file length
	move.l	D6,(A7)			; returns actually read length in D1
.quit
	CLRVAR_W	iconify_lock
	RESTORE_REGS	D1-A6
	rts

.notfound:
	moveq.l	#-1,D0
	bra.b	.quit

.nullread
	moveq.l	#0,D0
	bra.b	.quit

; *** command 5

.cmdfilelen
	move.l	#1,D0		; file found, in D0

; *** and end of command 0 with -1 as length

.filelen
	move.l	4(A5),(A7)
	bra	.quit

; *** operation 6.

.op6
	bra	.nullread

;	RESTORE_REGS	D1-A6
;	moveq.l	#0,D0
;	rts

.wrongcmd:
	; *** should not happen

	RUNTIME_ERROR_ROUTINE	RNReadWrongCommand,"ReadFile: unknown command"

; *** Search the file in the fileid struct

; *** fileid struct: 

; *** 4 bytes: pointer on the data
; *** 4 bytes: length
; *** 2 bytes: type 0 file 1 dir
; *** ....
; *** NULL terminated list

; *** data: file data (length) + 108 chars for filename (1 mem block per file)

; < A0: required filename
; > A5: pointer on file handle (if (A5)=0 then file not found)
; > D0: 0 if file is up to date, !=0 otherwise (update needed)

RelFun_Priv_SearchDirEntry:
	STORE_REGS	D1/A1/A4
	SET_VAR_CONTEXT
	GETVAR_L	fileidbuffer,A5
	cmp.l	#0,A5
	beq.b	.nofiles		; no files loaded, no fileidbuffer loaded

.search
	tst.l	(A5)
	beq.b	.exit		; not found: out with 0 in (A5)
	bmi.b	.skip		; invalidated file

	move.l	(A5),D0
	add.l	4(A5),D0	; pointer on the name
	move.l	A0,D1		; the filename we want to look for

	JSRGEN	StrcmpAsm
	tst.l	D0
	beq.b	.found		; found: out with the found struct in (A5),(4,A5)
.skip
	add.l	#10,A5		; next file
	bra.b	.search

	; ** File found, check if clean

.found
	move.l	(A5),A1
	add.l	4(A5),A1	; pointer on the name
	tst.b	(FNAME_SIZE-1,A1)
	beq.b	.exit
	moveq.l	#-1,D0
	bra.b	.exit2		; file needs to be updated in JST file cache
.exit
	moveq.l	#0,D0		; if not found, let's say that the file is clean
.exit2

	RESTORE_REGS	D1/A1/A4
	rts

.nofiles
	LEAVAR	fileidbuffer,A5	; trick: pointer on 0L
	bra.b	.exit

; *** Set disk: load diskfile from HD if not in memory
; *** in: D0: disk number
; *** out: D0=0 if HDLOAD, D0 unchanged if no HDLOAD

RelFun_SetDisk:
	STORE_REGS	D1/A0/A1/A4

	SET_VAR_CONTEXT

	TSTVAR_L	lowmem_flag
	bne	.exit			; do nothing if lowmem on
	TSTVAR_L	hdload_flag
	beq	.exit			; do nothing if hdload off

	; *** see if the disk is cached

	cmp.b	(RelVar_current_disk+3,A4),D0
	beq	.cached

	; *** the disk was not in memory, set it

	and.l	#$FF,D0
	SETVAR_L	D0,current_disk

	; *** and load it

	bsr	SetDiskNumber

	SETVAR_W	#1,iconify_lock	; lock (we never know...)

	moveq.l	#0,D0
	JSRGEN	GetDiskPointer
	move.l	D0,A1			; destination buffer
	GETVAR_L	filesize,D1	; disk size
	moveq.l	#0,D0			; command: read
	LEAVAR	fname,A0		; diskfile name
	JSRGEN	ReadFileHD
	tst.l	D0
	bne	RunTime_DiskRead	; disk read error (quit JST)

	CLRVAR_W	iconify_lock

.cached
	moveq.l	#0,D0		; always unit 0
.exit

	RESTORE_REGS	D1/A0/A1/A4
	rts


; ReadSectors (Rob Northen). RAM/HD version

; This routine is used as-is in lots of games and can be used provided
; an offset correction or a disk choice is done in many games.
; I noticed it and replaced it in:

; Assassin, BodyBlows, AlienBreed, Warzone, Magic Pockets, Cadaver,
; Chaos Engine, Cannon Fodder 2, Project-X, Mortal Kombat I & II,
; Gods, Rodland, Desert Strike, Qwak, Addams Family, SpeedBall II,
; ATR, Project-X SE, Donk, Jurrassic Park, Out To Lunch, Overdrive,
; and for sure others...

; D1 = Offset / 512 depart
; D2 = nb of 512 bytes blocks to read
; D0 = Drive Number - sometimes matches disk number
; D3 = 0: Read data, !=0 Writes (treated separately)
; A0 = Start address

RelFun_ReadRobSectorsFast:
RelFun_ReadRobSectors:
	STORE_REGS	D1/D2/D4/A0/A1
	SET_VAR_CONTEXT

	and.l	#$FFFF,D1
	and.l	#$FFFF,D2

	and.w	#$F,D0

	move.l	D2,D4
	beq	.RS_End
	bmi	.RS_End

	; File offset calculation

	lsl.l	#8,D1
	add.l	D1,D1		; offset in bytes

	TSTVAR_L	lowmem_flag
	bne.b	.RS_HDRead

	lsl.l	#5,D4
	subq.l	#1,D4		; length in longs-1 for the dbf
	
	JSRGEN	SetDisk
	JSRGEN	GetDiskPointer

	move.l	D0,A1

	cmp.l	#0,A1
	beq	RunTime_WrongId	; Is the disk allocated?
	add.l	D1,A1		; Sector offset

.RS_Copy:
	move.l	(A1)+,(A0)+
	move.l	(A1)+,(A0)+
	move.l	(A1)+,(A0)+
	move.l	(A1)+,(A0)+
	dbf	D4,.RS_Copy

.RS_End:
	RESTORE_REGS	D1/D2/D4/A0/A1
	moveq.l	#0,D0
	tst.l	D0
	rts

.RS_HDRead:
	move.l	D1,D2	; offset
	move.l	D4,D1	; length
	lsl.l	#8,D1
	add.l	D1,D1	; in bytes, please
	JSRGEN	ReadDiskPart
	tst.l	D0
	bne	RunTime_DiskRead
	bra.b	.RS_End

; *** returns userstring

; out: A0: UserData string

RelFun_GetUserData:
	STORE_REGS	A4
	SET_VAR_CONTEXT
	LEAVAR	custom_str,A0
	tst.b	(A0)
	bne.b	.notempty
	sub.l	A0,A0
.notempty
	RESTORE_REGS	A4
	rts

RelFun_GetLoadDir:
	STORE_REGS	A4
	SET_VAR_CONTEXT
	LEAVAR	loaddata_dir,A0
	tst.b	(A0)
	bne.b	.notempty
	sub.l	A0,A0
.notempty
	RESTORE_REGS	A4
	rts

; *** returns user flags (v2.3)

; out: D0: user flags

RelFun_GetUserFlags:
	STORE_REGS	A0/A4
	SET_VAR_CONTEXT

	moveq.l	#0,D0

	TSTVAR_L	pal_flag
	beq.b	.0
	bset	#AFB_PAL,D0
.0
	TSTVAR_L	ntsc_flag
	beq.b	.1
	bset	#AFB_NTSC,D0
.1
	TSTVAR_L	lowmem_flag
	beq.b	.2
	bset	#AFB_LOWMEM,D0
.2
	TSTVAR_L	hdload_flag
	beq.b	.3
	bset	#AFB_HDLOAD,D0
.3
	TSTVAR_L	trainer_flag
	beq.b	.4
	bset	#AFB_TRAINER,D0
.4
	TSTVAR_B	joypad_type
	beq.b	.6
	bset	#AFB_JOYPAD,D0		; legacy JST interface
.6

	RESTORE_REGS	A0/A4
	rts

; < A0: entry of the new interrupt
; > A0: old entry

RelFun_SetTraceVector:
	STORE_REGS	A1/A2/A4
	SET_VAR_CONTEXT
	LEAVAR	new_vbr,A1
	lea	(TRACE_VECTOR,A1),A1
	move.l	(A1),A2
	move.l	A0,(A1)
	move.l	A2,A0		; old interrupt
	RESTORE_REGS	A1/A2/A4
	rts

; < A0: entry of the new interrupt
; > A0: old entry

RelFun_SetBusErrorVector:
	STORE_REGS	A1/A2/A4
	SET_VAR_CONTEXT
	LEAVAR	new_vbr,A1
	lea	(BUS_ERR_VECTOR,A1),A1
	move.l	(A1),A2
	move.l	A0,(A1)
	move.l	A2,A0		; old interrupt
	RESTORE_REGS	A1/A2/A4
	rts

; *** notify the patch so JST cn write a patch report on exit
; < D0.W: patched size (bytes)
; < A0: patched address

RelFun_LogPatch:
	STORE_REGS	D0-D2/A0-A1
	SET_VAR_CONTEXT

	and.l	#$FFFF,D0		; limit to 65536 bytes at a time
	beq	.exit			; zero length: out!

	GETVAR_L	logpatch_buf,D1	; start of the buffer
	beq	.exit
	GETVAR_L	logpatch_ptr,A1	; current area
	move.l	D1,D2
	add.l	#LOGPATCH_SIZE,D2
	add.l	D0,D2			; the bytes to be stored
	add.l	#8,D2			; the extra information
	cmp.l	D2,A1
	bcc	.exit			; too many patches, overflow

	move.l	A0,(A1)+		; location
	move.l	D0,(A1)+		; store length
	subq	#1,D0
.store
	move.b	(A0)+,(A1)+		; store contents
	dbf	D0,.store

	SETVAR_L	A1,logpatch_ptr			; update current area pointer
.exit
	RESTORE_REGS	D0-D2/A0-A1
	rts

; *** Zeroes the color registers -> black screen if not copperlist is active

RelFun_BlackScreen:
	STORE_REGS	D0/A5
	lea	$DFF000+color,A5
	move.l	#31,D0
.cloop
	move.w	#0,(A5)+
	dbf	D0,.cloop

	RESTORE_REGS	D0/A5
	rts

; *** Checks for different joypad buttons
; < D0: controller id (0-1)
; > D0: button bits

RelFun_JoypadState:
	STORE_REGS	D1-D2/A0-A1
	move.l	D0,D1

	moveq.l	#0,D0
	lea	$DFF000,A1

	tst.b	D1
	bne	JBS_Joy1

	; ** check port 0

	BTST	#6,$BFE001
	beq	JBS_Fire1Pressed

	BSET	#6,$BFE201	; bit 6 now in input
	BCLR	#6,$BFE001	; CIA PRA
	MOVE	#$F600,(potgo,A1)
	MOVEQ	#0,D0
	MOVEQ	#6,D1
	BRA.S	.JBS_0001
.JBS_0000:
	TST.B	$BFE001
	TST.B	$BFE001
	TST.B	$BFE001
.JBS_0001:
	TST.B	$BFE001
	TST.B	$BFE001
	TST.B	$BFE001
	TST.B	$BFE001		
	TST.B	$BFE001

	MOVE.W	(potinp,A1),D2
	BSET	#6,$BFE001
	BCLR	#6,$BFE001
	BTST	#10,D2		; button 2
	BNE.S	.JBS_0002
	BSET	D1,D0
.JBS_0002:
	DBF	D1,.JBS_0000

	BCLR	#6,$BFE201	; bit 6 now in output
	MOVE	#$FFFF,(potgo,A1)
	bra	JBS_EXIT

	; ** check port 1

JBS_Joy1:
	BTST	#7,$BFE001
	beq	JBS_Fire1Pressed

	BSET	#7,$BFE201	; bit 7 now in input
	BCLR	#7,$BFE001	; CIA PRA
	MOVE	#$6F00,(potgo,A1)
	MOVEQ	#0,D0
	MOVEQ	#6,D1
	BRA.S	.JBS_0001
.JBS_0000:
	TST.B	$BFE001
	TST.B	$BFE001
	TST.B	$BFE001
.JBS_0001:
	TST.B	$BFE001
	TST.B	$BFE001
	TST.B	$BFE001
	TST.B	$BFE001		
	TST.B	$BFE001

	MOVE.W	(potinp,A1),D2
	BSET	#7,$BFE001
	BCLR	#7,$BFE001
	BTST	#14,D2		; button 2
	BNE.S	.JBS_0002
	BSET	D1,D0
.JBS_0002:
	DBF	D1,.JBS_0000

	BCLR	#7,$BFE201	; bit 7 now in output
JBS_EXIT:
	MOVE	#$FFFF,(potgo,A1)
	RESTORE_REGS	D1-D2/A0-A1		;88: 4CDF0107
	RTS				;8C: 4E75

; *** if normal fire button pressed, then special case
; *** as this one interferes with the others buttons read

JBS_Fire1Pressed:
	bset	#AFB_FIRE1,D0
	bra	JBS_EXIT

; check if address is in memory range useable by the slave
; < A0: address
; > D0: 0 if OK, -1 else

MemoryInRange:
	STORE_REGS	D1-A6
	SET_VAR_CONTEXT

	moveq.l	#0,D0

	; first check for chipmem range

	GETVAR_L	maxchip,D2
	moveq.l	#0,D1
	move.l	A0,D0
	bsr	.chkbounds
	tst.l	D0
	beq.b	.exit	; in chipmem, in mirrored area

	; then check for 32 bit expansion mem range
	
	GETVAR_L	extbuf,D1
	beq.b	.noext	; no 32bit expansion memory, will try 24bit memory
	move.l	D1,D2
	ADDVAR_L	extsize,D2
	move.l	A0,D0
	bsr	.chkbounds
	tst.l	D0
	beq.b	.exit	; in expansion mem

	; last chance: check for 24 bit expansion mem range
	
.noext:
	GETVAR_L	bit24buf,D1
	beq.b	.illegalmem	; no 24bit expansion memory neither: error
	move.l	D1,D2
	ADDVAR_L	bit24size,D2
	move.l	A0,D0
	bsr	.chkbounds
.exit:
	RESTORE_REGS	D1-A6
	rts

.illegalmem:
	moveq.l	#-1,D0
	bra.b	.exit

; < D0: address
; < D1: lower limit
; < D2: upper limit
; > D0: 0 if OK, -1 else

.chkbounds:
	cmp.l	D1,D0
	bcs.b	.rangeerr	; D0 lower than D1!

	cmp.l	D2,D0
	bcc.b	.rangeerr	; D0 higher than D2!

	moveq.l	#0,D0		; in D1-D2 range, ok
	rts
.rangeerr:
	moveq.l	#-1,D0
	rts

; Enter the debugger
; Caution: if you need more than A0 & A4 to save,
; then add 4 to values in lines noted "Stackdependent"

RelFun_EnterDebugger:
	STORE_REGS	A0/A4
	SET_VAR_CONTEXT
	
	lea	.debugger_nmi(pc),A0
	GETVAR_L	debugger_nmi,(A0)
	RESTORE_REGS	A0/A4
	BNE.S	.DEBUGGER
.NO_DEBUGGER
	RTS

.DEBUGGER:
	STORE_REGS	A0/A1
	bsr	.getvbrA1

	LEA	.oldTRAP0(PC),A0
	MOVE.L	($80,A1),(A0)		; saves old trap vector

	LEA	.oldPC(PC),A0
	MOVE.L	8(A7),(A0)		; saves return address (Stackdependent)
	RESTORE_REGS	A0/A1

	PEA	.GO_DEBUGGER(PC)
	STORE_REGS	A1
	bsr.b	.getvbrA1
	MOVE.L	4(A7),($80,A1)		; sets TRAP #0 with .GO_DEBUGGER
	RESTORE_REGS	A1
	ADDQ.L	#8,A7			; pops up return address + .GO_DEBUGGER
	TRAP	#0

	; from here supervisor state
.GO_DEBUGGER
	STORE_REGS	A1
	bsr.b	.getvbrA1
	MOVE.L	.oldTRAP0(PC),($80,A1)	; restores TRAP #0
	RESTORE_REGS	A1

	MOVE.L	.oldPC(PC),2(A7)	; changes return address (for the next RTE)
	MOVE.L	.debugger_nmi(PC),-(A7)
	RTS				; enters in the debugger interface

; > A1: VBR to use for TRAP

.getvbrA1:
	sub.l	A1,A1	; VBase is considered to be zero at first

	STORE_REGS	A4
	SET_VAR_CONTEXT

	TSTVAR_L	ostrashed
	bne.b	.ostrash

	; os is active: get absolute oldvbr address

	GETVAR_L	system_vbr,A1
	JSRGEN	WaitMouse

.ostrash:
	RESTORE_REGS	A4
	rts

.oldTRAP0
	DC.L	0
.oldPC
	DC.L	0
.debugger_nmi:
	DC.L	0

		
; End SaveOsData: called by SaveOSData in the end
; to be able to clear chipmem even if the absolute JST
; code is loaded in the game chipmem range (v4.0)

RelFun_Priv_EndSaveOsData:

	SET_VAR_CONTEXT

	
	GETVAR_L	maxchip,A1
	cmp.l	A7,A1
	bcs.b	.skipclr	; should not happen but...

	; clear chip memory

	move.l	A7,A1
	moveq.l	#0,D0
.loop:
	move.l	D0,-(A1)
	cmp.l	#$400,A1
	bcc.b	.loop

.skipclr

	move.w	#$2000,SR		; Jeff: no more interrupt freeze
							; Still intena blocks the interrupts

	; *** Install OSEmu and AllocAbs() stack if needed

	TSTVAR_L	osemu_ptr
	beq.b	.noosemu

	; *** install OS emu if present

	JSRGEN	InstallHarryOSEmu
	
	; don't clear A1/A6

	bra	.regclr

	; clear all registers (except A7 of course!!)
.noosemu
	; clear registers A1 & A6

	sub.l	A6,A6
	sub.l	A1,A1

	; clear all registers except A1 (diskio pointer) and A6 (execbase pointer)
	
.regclr
	; detect joystick types, twice
	bsr	_detect_controller_types
	bsr	_detect_controller_types
	; read both joys until not all buttons are pressed: this is an attempt to try to fix
	; the "first time VBL is called, game exits" when JOYPAD is 2
	moveq.l	#1,d2
.joyloop
	TSTVAR_L	verbose_flag
	beq.b	.nocolor
	move.w	#$00F,$dff180
.nocolor
	move.l	D2,D0
	bsr	_read_joystick
	and.l	#ALL_FRONT_BUTTONS_MASK,d0
	cmp.l	#ALL_FRONT_BUTTONS_MASK,d0
	beq.b	.joyloop
	dbf		d2,.joyloop
    

	moveq.l	#0,D0
	moveq.l	#0,D1
	moveq.l	#0,D2
	moveq.l	#0,D3
	moveq.l	#0,D4
	moveq.l	#0,D5
	moveq.l	#0,D6
	moveq.l	#0,D7

	; clear registers to avoid a bug trashes some fast memory
	; (sort of weak memory protection, how will the game know
	; how to trash fastmem if we give only null pointers? :) )
	; another advantage to this: the OSEmu loaders start programs
	; using LoadSeg and the registers are supposed to be zeroed
	; (I think this applies only to D2-D7/A2-A5)

	move.l	D0,A0
	move.l	D0,A2
	move.l	D0,A3
	move.l	D0,A4
	move.l	D0,A5

	; sets the system custom registers in a more compatible state
	; (the games expect some values to be present)

	bsr	CustomRegsAdjust


	rts				; and return (to EnableMMU)


RelFun_SetQuitKey:
;In:
;	D0 = Key to quit on
;	A0 = Custom code which will be called *before* InGameExit
;
;Notes:
;	If the Custom code returns with D0 != 0 no quit will be performed
;	If A0 is 0 no custom code will be called
;	If D0 is 0 no quit will be attempted

	STORE_REGS	A1/A4/D1
	SET_VAR_CONTEXT

	TSTVAR_B	quitkeynum
	bne.b	.userpri
	SETVAR_B	D0,quitkeynum
.userpri

	LEA	USER_QUIT_CODE(PC),A1
	MOVE.L	A0,(A1)
	
	RESTORE_REGS	A1/A4/D1

	RTS

RelFun_TryUnpackedSupport:
; < D0: leave to 0
;
	STORE_REGS	A4
	SET_VAR_CONTEXT
	SETVAR_W	#1,unpacked_support
	RESTORE_REGS	A4
	rts

RelFun_RNCDecrunchEncrypted
	STORE_REGS	A2
	lea	RNCDecrunchEncrypted(pc),A2
	bsr	RNCDecrunchGeneric
	RESTORE_REGS	A2
	rts
RelFun_RNCDecrunch
	STORE_REGS	A2
	lea	RNCDecrunch(pc),A2
	bsr	RNCDecrunchGeneric
	RESTORE_REGS	A2
	rts

RNCDecrunchGeneric:
	STORE_REGS	A4
	SET_VAR_CONTEXT
	
	TSTVAR_W	unpacked_support
	beq.b	.decrunch

	move.l	(A0),D0			; first 4 bytes
	CMP.L	#$524E4301,D0
	beq.b	.decrunch
	CMP.L	#$524E4302,D0
	bne.b	.nodecrunch

.decrunch:
	jsr	(A2)
.exit
	RESTORE_REGS	A4
	rts

	; not RNC signature & unpacked support activated: activate RNC fake decrunch

.nodecrunch:

; A4 is set there
; < A0: "RNC" file, in fact unpacked already
; < A1: destination
	STORE_REGS	D1-A6
	cmp.l	A0,A1
	beq.b	.end		; already at the proper location: exit

	STORE_REGS	A0
	LEAVAR	lastfile_buffer,A0
	moveq.l	#5,D0
	moveq.l	#0,D1
	JSRGEN	ReadFile	; gets file length
	RESTORE_REGS	A0

	moveq.l	#0,D0		; error?
	tst.l	D1
	bmi.b	.end		; yes
	beq.b	.end		; yes

	move.l	D1,D0	; decrunched length, D0 used for return value too

	; simple memory copy

	JSRGEN	CopyMem

.end
	RESTORE_REGS	D1-A6
	bra.b	.exit

RelFun_SetIconifyKey:
;In:
;	D0 = Key to quit on
;	A0 = Custom code which will be called:
;		 *before* InGameIconify with D0 = 0
;		 *after* InGameIconify with D0 = 1
;
;Notes:
;	If the Custom code returns with D0 != 0 no iconify will be performed
;	If A0 is 0 no custom code will be called
;	If D0 is 0 no iconify will be attempted

	STORE_REGS	A1/A4
	
	TSTVAR_B	iconifykeynum
	bne.b	.userpri
	SETVAR_B	D0,iconifykeynum
.userpri

	LEA	USER_ICONIFY_CODE(PC),A1
	MOVE.L	A0,(A1)
	
	RESTORE_REGS	A1/A4

	RTS

USER_ICONIFY_CODE:
	DC.L	0
	
USER_QUIT_CODE:
	DC.L	0

FreezeFlag
	DC.W	0

tdunit:		; the virtual trackdisk.device unit
	dc.l	0

diskio:		; the diskio emulation structure
	blk.l	20,0

	; list of valid CRCs
	
openedfile:
	dc.l	0
currentoffset:
	dc.l	0


	cnop	0,4

	blk.l	10,0

JstRelEnd:

