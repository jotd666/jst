; *** JOTD Startup program
; *** Copyright Jean-Francois Fabre 1997-2018
; *** Copyright Ralf Huvendiek 1999


;;DEBUG_LOADTHEFILES = 1
;;SKELETON_TEST = 1

;MAXON_ASM	EQU	1

	; from jst_rel

	XREF	JstRelStart
	XREF	RNCDecrunch

	; LVOs

	XREF	_GetFunctionName
	
	XREF	_ConName
	XREF	_CliVersionMess

	; from jst_deg.asm

	XREF	AbsFun_Enhance
	XREF	AbsFun_Priv_EnhanceCpu
	XREF	AbsFun_Priv_EnhanceGfx
	XREF	AbsFun_HexToString
	XREF	AbsFun_Degrade
	XREF	AbsFun_FlushCachesSys
	XREF	AbsFun_Kick37Test
	XREF	AbsFun_KickVerTest
	XREF	AbsFun_Priv_SetClockLoad
	XREF	AbsFun_Priv_SendCDTVCommand
	XREF	AbsFun_Priv_GetVBR
	XREF	RelFun_CopyMem
	XREF	RelFun_AddPart
	XREF	RelFun_CRC16
	XREF	RelFun_StrcmpAsm
	XREF	RelFun_StrlenAsm
	XREF	RelFun_WaitMouse
	XREF	RelFun_StrcpyAsm
	XREF	RelFun_StrncpyAsm
	XREF	msg_Loading_kickstart
	XREF	msg_Can_t_lock_directory
	XREF	msg_xpk_packed_not_supp
	XREF	msg_allocating_memory
	XREF	msg_done
	XREF	msg_scanning_files
	
	XREF	user_cacheflags
	XREF	user_cachemask


	XDEF	_GfxBase
	XDEF	_DosBase
	XDEF	_SysBase
	XDEF	RelocErr
	XDEF	gene_variables
	XDEF	AbsFun_AllocateTheMemory
	XDEF	AbsFun_FreeTheMemory
	XDEF	AbsFun_NewLine
	XDEF	AbsFun_Display

	; keychecks/misc

	XDEF	LockErr
	XDEF	MemErr
	XDEF	TaskCheck

	; variables

	XDEF	verbose_flag 




;EMU_KICK13 = 1		; to test 1.3 arguments parsing

;;USE_STACKCHECK = 1

; store regs macro, with stack check

	IFD	USE_STACKCHECK

STORE_REGS: MACRO
	IFLE	NARG
	STORE_REGS	D0-D7/A0-A6
	ELSE
	STORE_REGS	\1
	ENDC

	jsr	CheckStack

	ENDM
	ENDC


*## Default includes:


	include	"jst_libs.i"
	include	"osemu.i"
	include	"jstabs_macros.i"
	include	"jst_msgs.i"
	include	"jst_rel.i"
	;include	"jst_util.i"
	INCLUDE	"workbench/workbench.i"
	INCLUDE	"workbench/startup.i"
	INCLUDE	"MMU.I"
	include	"whdmacros.i"
	
StartFrom:	Macro
	move.l	RtnMess,d0		;if started from WB, d0<>0
	EndM

NextArg:	Macro
	move.l	zyxArgP(pc),d0		;get address to argument string
;	beq.b	*+8			;none? (from WB) then skip
	beq.b	.exit_nextarg\@
	move.l	d0,a0			;use pointer
	bsr.w	zyxGArg			;go to our internal routine
	move.l	a0,zyxArgP		;update argument pointer
.exit_nextarg\@
	tst.l	d0			;set/unset Z flag
	EndM

; startup code

EntryPoint:
	move.l	$4.w,_SysBase		;store execbase in fastmem
	move.l	_SysBase,a6		;exec base
	IFND	SKELETON_TEST
	move.l	A0,TaskArgsPtr
	move.l	D0,TaskArgsLength
	JSRLIB	Forbid
	sub.l	A1,A1
	JSRLIB	FindTask		;find ourselves
	move.l	D0,TaskPointer
	move.l	D0,A0
	move.l	(TC_SPLOWER,A0),D0
	add.l	#$100,D0	; 256 bytes for safety
	move.l	D0,TaskLower	; for task check
	move.l	#TaskName,LN_NAME(a0)	; sets task name
	move.l	#-1,pr_WindowPtr(A0)	; no more system requesters (insert volume...)
	JSRLIB	Permit

	move.l	TaskPointer,A4
	tst.l	pr_CLI(A4)
	bne.b	.fromcli

	; gets wb message

	lea	pr_MsgPort(A4),A0
	JSRLIB	WaitPort
	lea	pr_MsgPort(A4),A0
	JSRLIB	GetMsg
	move.l	D0,RtnMess
	
.fromcli
	ENDC
	
	jsr	init
	
	; replies to workbench

	StartFrom
	tst.l	D0
	beq.b	.cliend

	move.l	_SysBase,A6
	JSRLIB	Forbid
	move.l	D0,A1
	JSRLIB	ReplyMsg
	; no permit here??
	;move.l	_SysBase,A6
	;JSRLIB	Permit


.cliend
	; exit

	moveq.l	#0,D0
	rts

CheckStack
	rts
	
	tst.b	TaskCheck
	beq.b	.ok
	cmp.l	TaskLower,A7
	bcc.b	.ok

	move.l	oldstack,A7
	clr.b	TaskCheck	; disable task check now!
	jmp	TaskOverflowErr
.ok
	rts

	include "jst_fakepad.asm"

; *** JOTD Startup program (part 1)
; *** Copyright Jean-Francois Fabre 1997-2001
; *** Copyright Ralf Huvendiek 1999


SAFETY_MARGIN = $10

init:
	STORE_REGS
	move.l	A7,oldstack

	; *** Open DOS Library

	lea	dosname,A1
	move.l	_SysBase,A6
	moveq.l	#0,D0
	JSRLIB	OpenLibrary
	move.l	D0,_DosBase
	bne.b	.ok
	JMPABS	CloseAll
.ok

	; *** if started from CLI, open output there

	StartFrom		; gets wb message if from wb
	tst.l	D0
	bne.b	.fromwb
	
	move.l	_DosBase,a6
	JSRLIB	Output
	move.l	D0,conhandle

.fromwb

	
	; *** Allocate memory for relocated variables

	move.l	#Rel_ENDVARS-Rel_STARTVARS,D0
	JSRABS	GetMemFlag
	or.l	#MEMF_CLEAR,D1
	JSRABS	AllocateTheMemory
	move.l	D0,gene_variables
	beq	MemErr

	; *** installs variable context in address register 4

	SET_VAR_CONTEXT

	; *** Store modified AttnFlags

	move.l	_SysBase,A6
	moveq.l	#0,D0
	move.b	AttnFlags+1(A6),D0
	btst	#AFB_68040,D0
	beq.b	.noclr6888x

	; remove 6888x coprocessors declaration if 68040+ is set

	bclr	#AFB_68881,D0
	bclr	#AFB_68882,D0
.noclr6888x
	SETVAR_L	D0,attnflags	; save attnflags in fastmem

	; gets system VBR

	jsr	AbsFun_Priv_GetVBR
	SETVAR_L	D0,system_vbr
	SETVAR_L	D0,current_vbr

	; sets -1 in copper_pointer

	SETVAR_L	#-1,copper_pointer


	jsr	GetEnvVariables		; get some env. variables (FREEZEKEY, SAVEDIR)

	jsr	GetArgumentsAndCD	; read tooltypes/arguments

	; *** saves old stack also into relocated variables block

	SETVAR_L	oldstack,system_userstack

	IFND	SKELETON_TEST
	TSTVAR_L	quiet_flag
	beq.b	.noquiet
	jsr	CloseOutput
	bra.b	.quiet
.noquiet
	jsr	OpenOutput		; from there from WB, the console is available

.quiet

	jsr	ResumeFrozen		; if RESUME set try to resume session

	; *** end of temporary ***

	jsr	SearchDebugger		; lookup for HRTMon

	TSTVAR_L	mmunumstate_flag
	BEQ.S	.No_LoadMMU
	TSTVAR_L	execute_flag
	BNE.S	.No_LoadMMU

	JSR	LoadMMUCode


.No_LoadMMU
	CALL_MMUCodeABS	InitializeMMUFunctions
	CALL_MMUCodeABS	SaveMMU

	SETVAR_L	#1,snapshot_version
	ENDC	; SKELETON_TEST
	
	jsr	RelocateRoutines

	bsr	InstallMMUCodePlugins

	tst.l	verbose_flag
	beq.b	.do

	jsr	DisplayOptions
.do

	IFND	SKELETON_TEST
	jsr	AddLoadDir		; loaddir set

	jsr	ExecutePreScript

	TSTVAR_L	execute_flag
	bne	RunDegraded	; it's an executable that we want to run degraded

	jsr	LoadObject

	jsr	PatchObject
	ENDC
	
	JSRABS	FlushCachesSys
    
	; sets loader stack to start stack, we don't need it anymore

	SET_VAR_CONTEXT
	SETVAR_L	oldstack,system_userstack

	tst.w	is_whdload
	beq	.runjst
.runwhd

	jmp	InitWHDSlave

.runjst
	TSTVAR_L	test_flag
	beq	.nodo
	JMPABS	CloseAll
.nodo
	TSTVAR_L	resume_flag
	beq.b	.skipresume
	SETVAR_W	#1,just_resumed
	PRINT_MSG	msg_Resuming
.skipresume:

	GETVAR_L	custom1_flag,D0
	moveq.l		#0,D1
	moveq.l		#0,D2
	GETVAR_L	hdload_flag,D3
	GETVAR_L	buttonwait_flag,D4
	GETVAR_L	lowmem_flag,D5

	move.l	object_entry(pc),A0

	add.l	HDP_ENTRY(A0),A0

	jsr	(A0)
	jmp	AbsFun_CloseAll

InstallMMUCodePlugins:
	STORE_REGS
	SET_VAR_CONTEXT

	TSTVAR_L	mmucode_ptr
	beq.b	.exit

	; sets Display function if verbose is on

	tst.l	verbose_flag
	beq.b	.nodisplay_plug

	GETVAR_L	mmucode_ptr,A0
	lea	(DisplayOffset,A0),A0
	cmp.l	#'DISP',(A0)
	bne.b	.obsolete		; obsolete version, cannot plug Display
	move.l	#AbsFun_Display,(A0)

.nodisplay_plug:

;	removed, did not work
;	GETVAR_L	mmucode_ptr,A0
;	lea	(DebugOffset,A0),A0
;	cmp.l	#'DBUG',(A0)
;	bne.b	.obsolete		; obsolete version, cannot plug EnterDebugger
;	move.l	jstrel_ptr,A1
;	add.l	#Rel_RelFunTable+RelOff_EnterDebugger,A1
;	move.l	(A1),(A0)		; EnterDebugger function plugged

.obsolete:
.exit:
	RESTORE_REGS
	rts

file_in_cache:
	dc.b	0
	cnop	0,4
complete_filename:	
	BLKDECL	b,108,0	; dir/file or just file
	cnop	0,4

; run the program in DOS mode, degraded

RunDegraded:	; procedure start

	SET_VAR_CONTEXT

	jsr	InitFakePad

	PRINT_MSG	msg_JST_running_executab

	lea	object_name(pc),A0
	move.l	A0,D0
	jsr	RelFun_StrlenAsm
	move.b	#'"',(A0,D0.W)	; code for "

	LEAVAR	custom_str,A1
	tst.b	(A1)
	beq	.nullt

	move.b	#' ',1(A0,D0.W)	; space for args
	lea	2(A0,D0.W),A0

	move.l	A1,D0		; userdata str
	move.l	A0,D1		; args for executable
	move.l	#80,D2
	jsr	RelFun_StrncpyAsm	; appends args to executable

	bra	.cnt
.nullt
	clr.b	1(A0,D0.W)	; NULL terminate
.cnt
	JSRGEN	Priv_GetLed

	moveq.l	#0,D0
	moveq.l	#0,D1
	JSRABS	Degrade

	moveq.l	#0,D2
	moveq.l	#0,D3
	move.l	#object_name_quotes,D1
	move.l	_DosBase,A6
	JSRLIB	Execute
	STORE_REGS	D0		; save return code

	SET_VAR_CONTEXT
	TSTVAR_L	noquit_flag
	beq.b	.quit
.loop
	move.l	#$10000,D1
	move.l	_DosBase,A6
	JSRLIB	Delay
	bra	.loop		; infinite loop

.quit
	JSRABS	Enhance

	JSRGEN	Priv_SetLed

	jsr	RemoveFakePad

	RESTORE_REGS	D0	; restore return code
	tst.l	D0
	bne	.ok
	PRINT_MSG	msg_Could_not_execute_pr
	jmp	AbsFun_CloseAll
.ok
	jmp	AbsFun_Priv_CloseAllQuiet

DISPLAY_FLAG:MACRO
	lea	\1ToolType,A1
	JSRABS	Display
	PRINT_MSG	msg_colon
	GETVAR_L	\2_flag,D0
	bsr	disp_flag
	ENDM

DisplayOptions:
	SET_VAR_CONTEXT
	PRINT_MSG	msg_JOTD_Startup_verbose_mode
	lea	object_name,A1
	tst.b	(A1)
	bne	.nameok
	lea	noname_text,A1
.nameok
	JSRABS	Display
	JSRABS	NewLine


	lea	LoadDirToolType,A1
	JSRABS	Display
	PRINT_MSG	msg_colon
	LEAVAR	loaddata_dir,A1
	tst.b	(A1)
	bne.b	.udirok
	lea	noname_text(pc),A1
.udirok
	JSRABS	Display
	JSRABS	NewLine

	lea	QuitKeyToolType,A1
	JSRABS	Display
	PRINT_MSG	msg_colon
	
	lea	quitkey_str,A1
	tst.b	(A1)
	bne.b	.uquitkeyok
	lea	noname_text(pc),A1
.uquitkeyok
	JSRABS	Display
	JSRABS	NewLine

	lea	FreezeKeyToolType,A1
	JSRABS	Display
	PRINT_MSG	msg_colon
	lea	freezekey_str,A1
	tst.b	(A1)
	bne.b	.ufreezekeyok
	lea	noname_text(pc),A1
.ufreezekeyok
	JSRABS	Display
	JSRABS	NewLine

	lea	IconifyKeyToolType,A1
	JSRABS	Display
	PRINT_MSG	msg_colon
	lea	iconifykey_str,A1
	tst.b	(A1)
	bne.b	.uiconifykeyok
	lea	noname_text(pc),A1

.uiconifykeyok
	JSRABS	Display
	JSRABS	NewLine

	lea	ForceClistToolType,A1
	JSRABS	Display
	PRINT_MSG	msg_colon
	lea	forceclist_str,A1
	tst.b	(A1)
	bne.b	.forceclistok
	lea	noname_text(pc),A1

.forceclistok
	JSRABS	Display
	JSRABS	NewLine


	lea	UserDataToolType,A1
	JSRABS	Display
	PRINT_MSG	msg_colon
	LEAVAR	custom_str,A1
	tst.b	(A1)
	bne	.ustrok
	lea	noname_text,A1
.ustrok
	JSRABS	Display
	JSRABS	NewLine

	lea	MMUToolType,A1
	JSRABS	Display
	PRINT_MSG	msg_colon
	move.l	mmu_str_state,A1
	JSRABS	Display
	JSRABS	NewLine

	
	DISPLAY_FLAG	Execute,execute
	DISPLAY_FLAG	Noquit,noquit
	DISPLAY_FLAG	Ntsc,ntsc
	DISPLAY_FLAG	Pal,pal
	DISPLAY_FLAG	FilterOff,filteroff
	DISPLAY_FLAG	Quiet,quiet
	DISPLAY_FLAG	NoCaches,nocaches
	DISPLAY_FLAG	NoFast,nofast
	DISPLAY_FLAG	HdLoad,hdload
	DISPLAY_FLAG	LowMem,lowmem
	DISPLAY_FLAG	Resume,resume
	DISPLAY_FLAG	Delay,delay
	DISPLAY_FLAG	ButtonWait,buttonwait
	DISPLAY_FLAG	LeaveCaches,leavecaches
	DISPLAY_FLAG	Debug,debug
	DISPLAY_FLAG	FreezeRmb,freezermb
	DISPLAY_FLAG	LeaveVBR,leavevbr
	DISPLAY_FLAG	NoVBRMove,novbrmove
	DISPLAY_FLAG	Test,test
	DISPLAY_FLAG	Verbose,verbose
	
	Mac_print	"JOYPAD: "
	moveq.l	#0,D0
	GETVAR_B	joypad_type,D0
	Mac_printh	D0
	JSRABS	NewLine
	TSTVAR_B	joypad_type
	beq		.skipcontrols
	
	moveq.l	#0,d0
	Mac_print	"JOY1BLUE: "
	GETVAR_B	joy1_blue_keycode,D0
	Mac_printh	D0
	Mac_print	"JOY1YELLOW: "
	GETVAR_B	joy1_yellow_keycode,D0
	Mac_printh	D0
	Mac_print	"JOY1GREEN: "
	GETVAR_B	joy1_green_keycode,D0
	Mac_printh	D0
	Mac_print	"JOY1RED: "
	GETVAR_B	joy1_red_keycode,D0
	Mac_printh	D0
	Mac_print	"JOY1PLAY: "
	GETVAR_B	joy1_play_keycode,D0
	Mac_printh	D0
	Mac_print	"JOY1FWD: "
	GETVAR_B	joy1_fwd_keycode,D0
	Mac_printh	D0
	Mac_print	"JOY1BWD: "
	GETVAR_B	joy1_bwd_keycode,D0
	Mac_printh	D0


	Mac_print	"JOY0BLUE: "
	GETVAR_B	joy0_blue_keycode,D0
	Mac_printh	D0
	Mac_print	"JOY0YELLOW: "
	GETVAR_B	joy0_yellow_keycode,D0
	Mac_printh	D0
	Mac_print	"JOY0GREEN: "
	GETVAR_B	joy0_green_keycode,D0
	Mac_printh	D0
	Mac_print	"JOY0RED: "
	GETVAR_B	joy0_red_keycode,D0
	Mac_printh	D0
	Mac_print	"JOY0PLAY: "
	GETVAR_B	joy0_play_keycode,D0
	Mac_printh	D0
	Mac_print	"JOY0FWD: "
	GETVAR_B	joy0_fwd_keycode,D0
	Mac_printh	D0
	Mac_print	"JOY0BWD: "
	GETVAR_B	joy0_bwd_keycode,D0
	Mac_printh	D0
	
.skipcontrols
	lea	FakePadToolType,A1
	JSRABS	Display
	PRINT_MSG	msg_colon
	GETVAR_L	fakepad_flag,D0
	Mac_printh	D0
	JSRABS	NewLine

	STORE_REGS	D0/A1

	MOVEQ	#0,D0
	GETVAR_L	debugger,D0
	LSL.L	#2,D0
	JSR	FoundDebuggerMSG(pc,D0)

	RESTORE_REGS	D0/A1
	rts


FoundDebuggerMSG
	JMP	NoDebuggerFoundMSG(PC)
	JMP	HRTMonFoundMSG(PC)
	JMP	COPFoundMSG(PC)
	JMP	ThrillKillFoundMSG(PC)
	JMP	ActionReplayFoundMSG(PC)

NoDebuggerFoundMSG
	PRINT_MSG	msg_No_debugger_found
	RTS
HRTMonFoundMSG
	PRINT_MSG	msg_HRTMon_found
	RTS
COPFoundMSG
	PRINT_MSG	msg_COP_found
	RTS
ThrillKillFoundMSG
	PRINT_MSG	msg_ThrillKill_found
	RTS
ActionReplayFoundMSG
	PRINT_MSG	msg_ActionReplay_found
	RTS


disp_flag:
	tst.l	D0
	beq.b	.no
	PRINT_MSG	msg_ON
	rts
.no
	PRINT_MSG	msg_OFF
	rts

GetEnvVariables:	; procedure start
	STORE_REGS

	; check the FREEZEKEY environment variable

	move.l	#FreezeKeyToolType,d1
	move.l	#freezekey_str,D2
	move.l	#GVF_GLOBAL_ONLY,D4
	move.l	#10,D3
	bsr	GetVar

	; check the QUITKEY environment variable

	move.l	#QuitKeyToolType,d1
	move.l	#quitkey_str,D2
	bsr	GetVar
	
	RESTORE_REGS
	rts

; 1.3/2.0 compatible version
GetVar:
	STORE_REGS	D1-D4/A0-A1/A6
	move.l	_DosBase,A6
	JSRABS	Kick37Test
	tst.l	D0
	bne.b	.13
	JSRLIB	GetVar
.exit
	RESTORE_REGS	D1-D4/A0-A1/A6
	RTS
.13
	move.l	d1,d0	; source
	move.l	#.envn,d1
	move.l	d2,-(a7)	; d2 is used for 2 following calls but we need it
	move.l	#49,d2	; max env.var name len
	jsr	RelFun_StrncpyAsm
	
	move.l	#.envfp,d1	
	move.l	#MODE_OLDFILE,d2
	JSRLIB	Open
	move.l	(a7)+,d2
	move.l	d0,d4	; store handle & test
	beq.b	.notfound
	
	move.l	d0,d1	; handle
	; D2 holds the dest buffer: OK
	; D3 holds the dest buffer len: OK
	JSRLIB	Read
	; D0 holds the read length
	move.l	d0,d3
	move.l	d4,d1
	JSRLIB	Close
	move.l	d3,d0	; read length is returned
	bra	.exit

.notfound
	moveq.l	#-1,d0	; mock GetVar return code in case of an error
	bra	.exit
	
.envfp:
	dc.b	"ENV:"
.envn:
	blk.b	50,0
	even
	
ExecutePreScript:	; procedure start
	
	STORE_REGS

	jsr	TestTempDir
	tst.l	d0
	bne	.exit	; T: is not there: quit
	
	move.l	#exeprescr_file,D0
	JSRABS	TestFileAbs
	tst.l	D0
	bne	.exit	; script is not there: quit
	move.l	#exeprescr_command,D1
	moveq	#0,D2
	moveq	#0,D3
	move.l	_DosBase,A6
	TSTVAR_L	test_flag
	bne	.exit	; TEST mode: quit
	JSRLIB	Execute
.exit
    move.l  #CMD_STOP,d0
    jsr AbsFun_Priv_SendCDTVCommand
	RESTORE_REGS
	rts

ExecutePostScript:	; procedure start
	
	STORE_REGS
	
	SET_VAR_CONTEXT
    move.l  #CMD_START,d0
    jsr AbsFun_Priv_SendCDTVCommand
    
	jsr	TestTempDir
	tst.l	d0
	bne	.exit	; T: is not there: quit

	
	move.l	#exepostscr_file,D0
	JSRABS	TestFileAbs
	tst.l	D0
	bne	.exit	; script is not there: quit
	move.l	#exepostscr_command,D1
	moveq	#0,D2
	moveq	#0,D3
	move.l	_DosBase,A6
	TSTVAR_L	test_flag
	bne	.exit	; TEST mode: quit
	JSRLIB	Execute
.exit
	RESTORE_REGS
	rts

    
    
TestTempDir:	; procedure start
	move.l	#tempdirname,D0
	bsr	AbsFun_TestAssign
	rts
; Close output if any

CloseOutput:	; procedure start
	STORE_REGS	D0-D1/A0-A1/A6
	StartFrom
	tst.l	D0
	beq.b	.fromcli	; from CLI, don't close!
	move.l	_DosBase,A6
	move.l	conhandle,D1
	beq.b	.skip
	JSRLIB	Close
.fromcli
	;;clr.l	conhandle	; no more output
.skip
	RESTORE_REGS	D0-D1/A0-A1/A6
	RTS

; *** open window/get console output handle

OpenOutput:
	SET_VAR_CONTEXT
	TSTVAR_L	quiet_flag		; output or not?
	bne	.go			; no output (quiet)

	tst.l	conhandle
	bne.b	.go			; already open

	move.l	_DosBase,a6
	JSRLIB	Output
	move.l	D0,conhandle
	bne	.go		; Output? Ok.

	move.l	_DosBase,A6
	lea	_ConName,A0
	tst.b	(A0)		; Maybe we don't want to open a console
	beq	.nowin
	move.l	a0,D1
	move.l	#MODE_NEWFILE,D2
	JSRLIB	Open
.exit
	move.l	D0,conhandle
.go
	rts

.nowin
	moveq.l	#0,D0
	bra	.exit

; *** load the MMU Code
LoadMMUCode
	STORE_REGS	D0-A6
	SET_VAR_CONTEXT

	MOVE.L	_DosBase,A6
	MOVE.L	#MMUCode_FileName,D1
	JSRLIB	LoadSeg
	
	TST.L	D0
	BEQ	.NO_Exe

	MOVE.L	D0,D7

	JSRABS	FlushCachesSys

	MOVE.L	D7,D0
	ASL.L	#2,D0
	ADDQ.L	#4,D0

	MOVE.L	D0,A0
	MOVE.L	IdentOffset(A0),D1
	CMP.L	#MMUCode_Ident1,(A0,D1.l)
	BNE	.NO_Ident
	CMP.L	#MMUCode_Ident2,4(A0,D1.l)
	BNE	.NO_Ident

	SETVAR_L	D0,mmucode_ptr
	MOVE.L	D0,D6

	CALL_MMUCodeABS	GetLength
	SETVAR_L	D0,mmucode_len

	tst.l	verbose_flag
	beq	.stay_quiet

	VERBOSE_MSG	msg_MMUCode_length_c_
	Mac_printh	D0

	
.stay_quiet:	

	JSRABS	GetMemFlag		; reverse if possible
	or.l	#MEMF_FAST|MEMF_CLEAR,D1
	GETVAR_L	mmucode_len,D0
	MOVE.L	_SysBase,A6
	ALLOC_ABS_OR_DYN	mmucode_ptr
	SETVAR_L	D0,mmucode_ptr
	BEQ	.NO_MMUMem

	MOVE.L	D6,A0
	MOVE.L	D0,A1
	GETVAR_L	mmucode_len,D0
	
.COPY_MMUCode
	MOVE.B	(A0)+,(A1)+
	SUBQ.L	#1,D0
	BNE	.COPY_MMUCode

	; cache flush
	
	JSRABS	FlushCachesSys

;	TSTVAR_L	snoopcustom_flag
;	BEQ.S	.NoSnoopMem

	JSRABS	GetMemFlag		; reverse if possible
	OR.L	#MEMF_CLEAR,D1
	MOVE.L	#SNOOP_BUFFER_SIZE,D0
	JSRABS	AllocateTheMemory	; no alloc mem fix
	MOVE.L	D0,CustomMirror
	BEQ.S	.NoSnoopMem

	add.l	#SNOOP_BUFFER_SIZE/2,D0		; real $DFF000 location
	SETVAR_L	D0,custom_mirror	; store custom mirror pointer
.NoSnoopMem
.NO_Ident
.NO_MMUMem
	MOVE.L	_DosBase,A6
	MOVE.L	D7,D1
	JSRLIB	UnLoadSeg

.NO_Exe
.NO_MMUCode
	RESTORE_REGS	D0-A6
	RTS

; frees all MMU resources

FreeMMUCode
	STORE_REGS
	SET_VAR_CONTEXT

	GETVAR_L	mmucode_ptr,D0
	BEQ.S	.NO_MMUCode

	CALL_MMUCodeABS	Cleanup
	
	GETVAR_L	mmucode_ptr,A1
	GETVAR_L	mmucode_len,D0
	JSRABS	FreeTheMemory

	CLRVAR_L	mmucode_ptr

	MOVE.L	CustomMirror,D0
	BEQ.S	.NoSnoop

	MOVE.L	D0,A1
	MOVE.L	#SNOOP_BUFFER_SIZE,D0
	JSRABS	FreeTheMemory
	clr.l	CustomMirror
	
.NoSnoop
.NO_MMUCode
	RESTORE_REGS
	RTS


; *** load the object file

LoadObject:	; procedure start
	STORE_REGS
	SET_VAR_CONTEXT

	move.l	#object_name,D0
	JSRABS	GetFileLengthAbs
	tst.l	D0			; 1.2 bugfix
	bmi	ObjectErr		; not found: error
	beq	ObjectErr		; empty: error

	SETVAR_L	D0,object_len
	JSRABS	GetMemFlag
	
;	ALIGN_ON_MMU	D0
	
	ALLOC_ABS_OR_DYN	object_ptr
	SETVAR_L	D0,object_ptr
	bne.b	.ok
	jsr	MemErr
	bra	.nov
.ok

	move.l	#object_name,D1
	move.l	#MODE_OLDFILE,D2
	move.l	_DosBase,A6
	JSRLIB	Open
	move.l	D0,D6
	beq	ObjectErr

	move.l	D6,D1
	GETVAR_L	object_ptr,D2
	GETVAR_L	object_len,D3
	JSRLIB	Read

	move.l	D6,D1
	JSRLIB	Close

	; compute object CRC

	GETVAR_L	object_ptr,A0
	GETVAR_L	object_len,D0

	jsr	RelFun_CRC16

	; if RESUME, check object CRC against snapshot data

	jsr	SetObjectCRC


	tst.l	verbose_flag
	beq.b	.nov
	VERBOSE_MSG	msg_Object_loaded_at_loc
	GETVAR_L	object_ptr,A0
	Mac_printh	A0
.nov

	RESTORE_REGS
	rts


; *** check object against rules (executable and no reloc hunk)

PatchObject:	; procedure start
	SET_VAR_CONTEXT

	; executable?

	GETVAR_L	object_ptr,A0
	cmp.l	#$3F3,(A0)
	bne	InvalidObjErr

	cmp.l	#1,(8,A0)
	bne	MoreSectionsErr

	cmp.l	#$3E9,($18,A0)
	bne	InvalidObjErr

	move.l	A0,object_entry
	add.l	#$20,object_entry

	move.l	($1C,A0),D0	; length in longwords
	add.l	D0,D0
	add.l	D0,D0

	move.l	object_entry,A0	
	move.l	(A0,D0.L),D1
	cmp.l	#$3EC,D1
	bne	relocok
	move.l	4(A0,D0.L),D0	; number of non relocatable variables
	bra	NonRelocErr
relocok
	cmp.l	#$3F2,D1
	bne	InvalidObjErr

	; *** check if valid jotd object

	move.l	HDP_MAGIC(A0),D0
	cmp.l	#'JOTD',D0
	beq	JSTNativeObject
	cmp.l	#'WHDL',D0
	bne	NoMagicObjErr
	move.l	4+HDP_MAGIC(A0),D0
	cmp.l	#'OADS',D0
	bne	NoMagicObjErr

	; WHDLoad object

	bra	WHDNativeObject
		
JSTNativeObject:	; procedure start
	move.l	HDP_VERSION(A0),D0
	cmp.l	#CURRENT_VERSION_ID,D0
	beq	.ok
	bcc	LoaderVerErr
.ok

	; *** user data

	SETVAR_L	HDP_FILESIZE(A0),filesize
	move.l	HDP_NBDISKS(A0),nbdisks

	move.l	HDP_FNAME(A0),D0	; filename/datadir
	add.l	object_entry(pc),D0
	LEAVAR	fname,A1
	move.l	A1,D1
	jsr	RelFun_StrcpyAsm

	move.l	#fname_base,D1
	jsr	RelFun_StrcpyAsm	; also keeps basename

	jsr	ComputeDiskfileName

	lea	AbsFunTable(pc),A1
	move.l	A1,HDP_ABSTABLE(A0)
	GETVAR_L	gene_patchbuffer,A1
	add.l	#Rel_RelFunTable,A1
	move.l	A1,HDP_RELTABLE(A0)

	move.l	_SysBase,HDP_SYSBASE(A0)
	move.l	_DosBase,HDP_DOSBASE(A0)

	rts

WHDNativeObject:	; procedure start
	SET_VAR_CONTEXT
	
;;	SETVAR_W	#1,is_whdload
	move.w		#1,is_whdload


	; *** user data

;;	move.l	HDP_FILESIZE(A0),filesize
;;	move.l	HDP_NBDISKS(A0),nbdisks

	move.l	#whddiskname,D0
	LEAVAR	fname,A1
	move.l	A1,D1
	jsr	RelFun_StrcpyAsm

	move.l	#fname_base,D1
	jsr	RelFun_StrcpyAsm	; also keeps basename

	jsr	ComputeDiskfileName


	
	rts

GET_CUSTOM_OPT:MACRO
	move.l		(A0)+,D0	; CUSTOM1
	beq.b		.skipcustom\1
	move.l		D0,A1
	move.l		(a1),d0
	SETVAR_L	d0,custom\1_flag
.skipcustom\1
	ENDM
	
FIND_OPTION:MACRO
	lea	\1ToolType,A1
	bsr	findoption
	beq.b	.notfound\@
	SETVAR_L	#-1,\2_flag
.notfound\@
	ENDM
	
PARSEHEXARG:MACRO
	move.l	(A0)+,D0
	beq.b	.sk\@
	move.l	D0,A1
	bsr	HexStringToNum
	;tst.b	D0
	;beq.b	.sk\@
	SETVAR_B	D0,\1_keycode
	; force joypad bit when one joykey option is set
	GETVAR_B	joypad_type,D0
	bset	#\2,d0
	SETVAR_B	D0,joypad_type
.sk\@
	ENDM
; *** Gets arguments passed by the user

GetArgumentsAndCD:	; procedure start
	STORE_REGS

	SET_VAR_CONTEXT
	SETVAR_L	#1,verbose_flag	; verbose mode
	; default joypad controls
	SETVAR_B	#$19,joy1_play_keycode	; P
	SETVAR_B	#$50,joy1_bwd_keycode	; F1
	SETVAR_B	#$51,joy1_fwd_keycode	; F2
	
	SETVAR_B	#$44,joy1_green_keycode	; RETURN
	SETVAR_B	#$40,joy1_blue_keycode	; SPACE
	SETVAR_B	#$64,joy1_yellow_keycode	; left-ALT
	SETVAR_B	#$45,joy1_fwdbwd_keycode	; ESC
	
	; default values for joypad 0
	SETVAR_B	#$19,joy0_play_keycode	; P	;(same as player 1)
	SETVAR_B	#$52,joy0_bwd_keycode	; F3
	SETVAR_B	#$53,joy0_fwd_keycode	; F4
	
	SETVAR_B	#$01,joy0_green_keycode	; 1
	SETVAR_B	#$02,joy0_blue_keycode	; 2
	SETVAR_B	#$41,joy0_yellow_keycode	; backspace
	SETVAR_B	#$45,joy0_fwdbwd_keycode	; ESC
	
	move.l	_DosBase,A6

	StartFrom		; gets wb message if from wb
	tst.l	D0
	beq	.fromcli

	; ** Started from workbench

	move.l	D0,a0
	move.l	sm_NumArgs(a0),D0	; number of arguments
	move.l	sm_ArgList(a0),a0	; list of arguments

	cmp.l	#1,D0
	beq	.itself			; program was clicked
	addq.l	#8,A0			; second wb argument/icon default tool
.itself
	move.l	A0,icon_arglist

	move.l	wa_Lock(a0),D1	; lock the dir of the icon
	move.l	D1,dirlock
	beq	LockErr

	move.l	dirlock,D1
	JSRLIB	CurrentDir	; icon dir becomes the current directory
	move.l	D0,old_dirlock

	lea	iconname,A1
	moveq.l	#0,D0
	move.l	_SysBase,A6
	JSRLIB	OpenLibrary
	move.l	D0,iconbase
	beq	.exit
	move.l	iconbase,A6

	move.l	icon_arglist(pc),A0
	move.l	wa_Name(a0),a0		; pointer on first arg (program)

	move.l	A0,D0
	move.l	#object_name,D1
	jsr	RelFun_StrcpyAsm	; copy default name (project)

	JSRLIB	GetDiskObject		; reads icon
	tst.l	d0
	beq	NoIconErr

	move.l	d0,a2

	; ** Check for the objname/slave tooltype (compulsory)

	lea	ObjnameToolType,a1

	move.l	a2,a0
	move.l	do_ToolTypes(a0),a0
	JSRLIB	FindToolType
	tst.l	d0
	bne	.copyname

	lea	SlaveToolType,a1

	move.l	a2,a0
	move.l	do_ToolTypes(a0),a0
	JSRLIB	FindToolType
	tst.l	d0

	beq.b	.noname

.copyname:
	move.l	#object_name,D1
	jsr	RelFun_StrcpyAsm	; copy name
.noname

	; ** Check for the userdata tooltype

	lea	UserDataToolType,a1

	move.l	a2,a0
	move.l	do_ToolTypes(a0),a0
	JSRLIB	FindToolType
	tst.l	d0
	beq.b	.nouserdata
	STORE_REGS	A3
	LEAVAR	custom_str,A3
	move.l	A3,D1
	move.l	#80,D2
	jsr	RelFun_StrncpyAsm	; copy userdata up to 80 chars
	RESTORE_REGS	A3
.nouserdata

	; ** Check for the execute tooltype

	FIND_OPTION	Execute,execute

	; ** Check for the noquit tooltype

	FIND_OPTION	Noquit,noquit

	; ** Check for the ntsc tooltype

	FIND_OPTION	Ntsc,ntsc

	; ** Check for the ntsc tooltype

	FIND_OPTION	Pal,pal

	; ** Check for the quiet tooltype (internal option)

	FIND_OPTION	Quiet,quiet

	; ** Check for the nocaches tooltype

	FIND_OPTION	NoCaches,nocaches
	bne.b	.skipnc		; NOCACHES found

	; ** Check for the nocache tooltype

	FIND_OPTION	NoCache,nocaches

.skipnc
	; ** Check for the nofast tooltype

	FIND_OPTION	NoFast,nofast

	; ** Check for the joypad tooltype

	lea	JoyPadToolType,A1
	bsr	findoption
	beq.b	.notfound
	SETVAR_L	#3,joypad_type
.notfound

	; ** Check for the hdload tooltype

	FIND_OPTION	HdLoad,hdload

	; ** Check for the lowmem tooltype

	FIND_OPTION	LowMem,lowmem

	; ** Check for the delay tooltype

	FIND_OPTION	Delay,delay

	; ** Check for the debug tooltype

	FIND_OPTION	Debug,debug

	; ** Check for the buttonwait tooltype

	FIND_OPTION	ButtonWait,buttonwait

	; ** Check for the leavecaches tooltype

	FIND_OPTION	LeaveCaches,leavecaches

	; ** MMU tooltype

	bsr	ReadMMUToolType

	; ** Check for the debug tooltype

	tst.l	conhandle
	beq	.skipdebug

	FIND_OPTION	Debug,debug

.skipdebug
	; ** Check for the leavevbr tooltype (internal option)

	FIND_OPTION	LeaveVBR,leavevbr
	bne	.skiplvbr		; LEAVEVBR found

	; ** Check for the notraphandler tooltype

	FIND_OPTION	NoTrapHandler,leavevbr
.skiplvbr
	; ** Check for the test tooltype (internal option)

	FIND_OPTION	Test,test

	; ** Check for the verbose tooltype (internal option)

	FIND_OPTION	Verbose,verbose
	GETVAR_L	verbose_flag,verbose_flag

	; ** Check for the novbrmove tooltype

	FIND_OPTION	NoVBRMove,novbrmove

	; ** Check for the freezermb tooltype

	FIND_OPTION	FreezeRmb,freezermb

	; fakepad 

	LEA	FakePadToolType,A1

	move.l	a2,a0
	move.l	do_ToolTypes(a0),a0
	JSRLIB	FindToolType
	tst.l	d0
	beq	.nofakepad
	move.l	D0,A1
	moveq.l	#0,D0
	move.b	(A1),D0
	beq.b	.nofakepad
	sub.b	#'0',D0
	SETVAR_L	D0,fakepad_flag
.nofakepad
	; end fakepad

	; ** Check for the filteroff tooltype

	FIND_OPTION	FilterOff,filteroff


	; check for the RESUME tooltype

	; ** Check for the verbose tooltype (internal option)

	FIND_OPTION	Resume,resume


	; ** Check for the loaddir tooltype (registered only)

	lea	LoadDirToolType,a1

	move.l	a2,a0
	move.l	do_ToolTypes(a0),a0
	JSRLIB	FindToolType
	tst.l	d0
	beq	.noloaddir
	LEAVAR	loaddata_dir,A1
	move.l	A1,D1
	jsr	RelFun_StrcpyAsm	; copy name
.noloaddir

	; ** Check for the quitkey tooltype (registered only)

	lea	QuitKeyToolType,a1
	lea	quitkey_str,a3
	bsr	find_key_tooltype

	; ** Check for the iconifykey tooltype (registered only)

	lea	IconifyKeyToolType,a1
	lea	iconifykey_str,a3
	bsr	find_key_tooltype

	; ** Check for the iconifykey tooltype (registered only)

	lea	ForceClistToolType,a1
	lea	forceclist_str,a3
	bsr	find_key_tooltype

	; ** Check for the freezekey tooltype (registered & shareware version)

	lea	FreezeKeyToolType,a1
	lea	freezekey_str,a3
	bsr	find_key_tooltype
.noregtts

	; *** End of tooltypes check

	move.l	iconbase,A6
	move.l	a2,a0
	JSRLIB	FreeDiskObject
	move.l	iconbase,D0
	beq	.exit
	move.l	D0,A1
	move.l	_SysBase,A6
	JSRLIB	CloseLibrary
	clr.l	iconbase
	bra	.noregargs

	; ** Started from CLI

.fromcli

.usereadargs
	move.l	#36,D0
	JSRABS	KickVerTest
	tst.b	D0
	beq	.readargs_v36
	
	; a1 = pointer to result array. Must be LONG aligned!
	; a0 = formatting string. BSTR!
	; d0 = size of result array (number of LONGs)

	
	lea	Template_V34(pc),a0
	lea	ProgArgs(pc),a1
	move.l	#ProgArgsEnd-ProgArgs,d0
	lsr.l	#2,d0		; number of longs
	
	bsr	get_args_BCPL
	
	lea	ProgArgs,A0
	move.l	(A0)+,D0
	beq.b	.ska
	; Arg 1: SLAVE
	;;blitz
	;;nop
	
	move.l	#object_name,D1
	lsl.l	#2,d0
	moveq.l	#0,d2
	move.b	d0,d2	; size
	add.l	#1,d0	; contents
	jsr	RelFun_StrncpyAsm
.ska
	; Arg 2: CUSTOM (aka USERDATA)
	move.l	(A0)+,D0
	beq.b	.ska2
	lsl.l	#2,d0
	moveq.l	#0,d2
	move.b	d0,d2	; size
	add.l	#1,d0	; contents
	LEAVAR	custom_str,A1
	move.l	A1,D1
	jsr	RelFun_StrncpyAsm
.ska2
	; Arg 3: DATA
	move.l	(A0)+,D0
	beq.b	.skb
	lsl.l	#2,d0
	moveq.l	#0,d2
	move.b	d0,d2	; size
	add.l	#1,d0	; contents
	LEAVAR	loaddata_dir,A1
	move.l	A1,D1
	jsr	RelFun_StrncpyAsm

.skb
	bsr	.get_common_switches
	
	move.l	(A0)+,D0			; CHANGE IT IF ADD OTHER TOOLTYPES BETWEEN
	beq.b	.skc
	lsl.l	#2,d0
	moveq.l	#0,d2
	move.b	d0,d2	; size
	add.l	#1,d0	; contents

	move.l	#quitkey_str,D1
	jsr	RelFun_StrncpyAsm
.skc

	bsr	.get_custom_switches
	bra	.noregargs
.readargs_v36:
	move.l	#Template_V36,d1
	move.l	#ProgArgs,d2
	moveq.l	#0,d3
	move.l	_DosBase,A6

	IFND	SKELETON_TEST
	JSRLIB	ReadArgs

	move.l	d0,.rdargs_struct		;NULL is OK
	beq	ReadArgsErr
	ELSE
	; test: hardcode slave name
	lea	ProgArgs,A0
	move.l	#.foo,(a0)
	bra.b	.sk
.foo
	dc.b	"agony.slave",0
	even
.sk
	ENDC
	
	; ** copy the object name in a buffer

	lea	ProgArgs,A0
	move.l	(A0)+,D0
	beq.b	.skn
	move.l	#object_name,D1
	jsr	RelFun_StrcpyAsm
.skn
	move.l	(A0)+,D0
	beq.b	.sku
	LEAVAR	custom_str,A1
	move.l	A1,D1
	jsr	RelFun_StrcpyAsm
.sku

	move.l	(A0)+,D0			; CHANGE IT IF ADD OTHER TOOLTYPES BETWEEN
	beq.b	.skl

	LEAVAR	loaddata_dir,A1
	move.l	A1,D1
	move.l	#256,D2
	jsr	RelFun_StrncpyAsm

.skl

.exit
	lea	ProgArgs,A0
	add.l	#12,A0			; name/userdata/data were filled
					; by either args or tooltypes
	; *** users flags
	
	
	MOVE.L	(A0)+,D0
	bsr	MatchMMUParam	

	; *** developpment flags

	SETVAR_L	(A0)+,debug_flag	; write coredumps on exit/crash
	SETVAR_L	(A0)+,freezermb_flag	; Freeze using Right Mouse Button (v2.7)
	MOVE.L	(A0)+,A1
	cmp.l	#0,A1
	beq.b	.fpnullptr
	SETVAR_L	(A1),fakepad_flag	; Joypad emulation (v2.8b)
.fpnullptr
	SETVAR_L	(A0)+,resume_flag	; resume old saved data (JST native only)

	SETVAR_L	#709379,eclock_freq	; PAL forced ATM for whdload tags


	move.l	(A0)+,D0			; CHANGE IT IF ADD OTHER TOOLTYPES BETWEEN
	beq.b	.nofreeze

	move.l	#freezekey_str,D1
	moveq.l	#10,D2
	jsr	RelFun_StrncpyAsm

.nofreeze
	move.l	(A0)+,D0			; CHANGE IT IF ADD OTHER TOOLTYPES BETWEEN
	beq.b	.noiconify

	move.l	#iconifykey_str,D1
	moveq.l	#10,D2
	jsr	RelFun_StrncpyAsm

.noiconify
	move.l	(A0)+,D0			; CHANGE IT IF ADD OTHER TOOLTYPES BETWEEN
	beq.b	.noforceclist

	move.l	#forceclist_str,D1
	moveq.l	#10,D2
	jsr	RelFun_StrncpyAsm

.noforceclist
	move.l  (A0)+,D0	; VK
	tst.b	D0
	beq.b	.skipvk
	move.l	D0,A1
	bsr	HexStringToNum
	tst.b	D0
	beq.b	.skipvk
	SETVAR_B	D0,vk_button
	
.skipvk
	move.l  (A0)+,D0	; VM
	tst.b	D0
	beq.b	.skipvm
	move.l	D0,A1
	bsr	HexStringToNum
	tst.b	D0
	beq.b	.skipvm
	SETVAR_B	D0,vm_button
.skipvm

	move.l		(A0)+,D0	; VMDELAY
	beq.b		.skipvmdelay
	move.l		D0,A1
	SETVAR_L	(a1),vm_delay
	SETVAR_B	#1,vm_enabled
	
.skipvmdelay
	move.l		(A0)+,D0	; VMMODDELAY
	beq.b		.skipvmoddelay
	move.l		D0,A1
	SETVAR_L	(a1),vm_modifierdelay
	SETVAR_B	#1,vm_enabled
	
.skipvmoddelay
	move.l  (A0)+,D0
	tst.b	D0
	beq.b	.skipvmodifybutton
	move.l	D0,A1
	bsr	HexStringToNum
	tst.b	D0
	beq.b	.skipvmodifybutton
	SETVAR_B	D0,vm_modifierbutton
	
.skipvmodifybutton
	move.l		(A0)+,D0	; JOYPAD
	beq.b		.skipjoypad
	move.l		D0,A1
	move.l		(a1),d0
	SETVAR_B	d0,joypad_type	; 0: no joy, 1: joy 1, 2: joy 2, 3: joy 1+2
.skipjoypad
	
	PARSEHEXARG	joy1_red,1
	PARSEHEXARG	joy1_green,1
	PARSEHEXARG	joy1_yellow,1
	PARSEHEXARG	joy1_blue,1
	PARSEHEXARG	joy1_fwd,1
	PARSEHEXARG	joy1_bwd,1
	PARSEHEXARG	joy1_play,1
	PARSEHEXARG	joy1_fwdbwd,1
	
	PARSEHEXARG joy1_right,1
	PARSEHEXARG joy1_left,1
	PARSEHEXARG joy1_up,1
	PARSEHEXARG joy1_down,1
	
	PARSEHEXARG	joy0_red,0
	PARSEHEXARG	joy0_green,0
	PARSEHEXARG	joy0_yellow,0
	PARSEHEXARG	joy0_blue,0
	PARSEHEXARG	joy0_fwd,0
	PARSEHEXARG	joy0_bwd,0
	PARSEHEXARG	joy0_play,0
	PARSEHEXARG	joy0_fwdbwd,0
	
	PARSEHEXARG joy0_right,0
	PARSEHEXARG joy0_left,0
	PARSEHEXARG joy0_up,0
	PARSEHEXARG joy0_down,0
	
	bsr	.get_common_switches
	; quitkey
	move.l	(A0)+,D0			; CHANGE IT IF ADD OTHER TOOLTYPES BETWEEN
	beq.b	.skk

	move.l	#quitkey_str,D1
	moveq.l	#10,D2
	jsr	RelFun_StrncpyAsm
.skk
	; CUSTOM1-5 values
	bsr	.get_custom_switches
.noregargs

	; *** check for coherence

	TSTVAR_L	lowmem_flag
	beq.b	.7
	SETVAR_L	#-1,hdload_flag		; if lowmem then hdload (compat. problem)
.7

	TSTVAR_L	execute_flag
	beq.b	.9
	CLRVAR_L	resume_flag		; no RESUME in execute mode
	bra.b	.10	
.9
	CLRVAR_L	leavevbr_flag		; LEAVEVBR can be set only if EXECUTE is on
.10

	GETVAR_L	mmunumstate_flag,D0
	cmp.l	#2,D0
;;	BEQ	.mmu_full

	;CLRVAR_L	snoopcustom_flag  ; who needs that when whdload does the job

;;.mmu_full


	lea	quitkey_str(pc),A0
	tst.b	(a0)
	beq.b	.skipqk
	bsr	hexstr_to_num
	tst.l	D1
	bmi.b	.error_qk
	SETVAR_B	D0,quitkeynum
.skipqk
	lea	freezekey_str(pc),A0
	bsr	hexstr_to_num
	tst.l	D1
	bmi.b	.error3
	SETVAR_B	D0,freezekeynum
.error3
	lea	iconifykey_str(pc),A0
	bsr	hexstr_to_num
	tst.l	D1
	bmi.b	.error4
	SETVAR_B	D0,iconifykeynum
.error4

	lea	forceclist_str(pc),A0
	bsr	hexstr_to_num
	tst.l	D1
	bmi.b	.error5
	SETVAR_L	D0,forceclistnum
.error5

	StartFrom		; gets wb message if from wb
	tst.l	D0
	bne.b	.noargsfree
	move.l	.rdargs_struct(pc),D1
	beq.b	.noargsfree		; already freed/not parsed
	move.l	_DosBase,A6
	JSRLIB	FreeArgs
	clr.l	.rdargs_struct
.noargsfree:
	RESTORE_REGS
	rts

.rdargs_struct:
	dc.l	0
.error_qk
	Mac_printf	"Invalid QUITKEY value"
	JMPABS	CloseAll
	
.default13:
	SETVAR_L	#-1,lowmem_flag
	SETVAR_L	#-1,buttonwait_flag
	bra	.noregargs
	
.get_custom_switches:
	GET_CUSTOM_OPT	1
	GET_CUSTOM_OPT	2
	GET_CUSTOM_OPT	3
	GET_CUSTOM_OPT	4
	GET_CUSTOM_OPT	5
	rts
	
	; since there's macro which allows to share V34 or V36 args
	; we can also share the code (and the order) of the shared args :)
.get_common_switches:
	SETVAR_L	(A0)+,nocaches_flag	; no caches
	SETVAR_L	(A0)+,nofast_flag	; no fastmem
	SETVAR_L	(A0)+,ntsc_flag		; forces NTSC
	SETVAR_L	(A0)+,pal_flag		; forces PAL
	SETVAR_L	(A0)+,hdload_flag	; load from HD instead of RAM
	SETVAR_L	(A0)+,lowmem_flag	; load from HD instead of RAM even for diskfiles
	SETVAR_L	(A0)+,buttonwait_flag	; waits fire after loads to be able to read
	tst.l	(A0)+	; PRELOAD
	bne.b	.nopreload
	CLRVAR_L	hdload_flag
	CLRVAR_L	lowmem_flag
.nopreload
	SETVAR_L	(A0)+,quiet_flag	; display nothing
	SETVAR_L	(A0)+,delay_flag	; wait between OS swaps
	SETVAR_L	(A0)+,verbose_flag	; verbose mode
	SETVAR_L	(A0)+,novbrmove_flag	; no vbr move (don't relocate VBR)
	SETVAR_L	(A0)+,filteroff_flag	; turn off filter at start
	SETVAR_L	(A0)+,test_flag		; just test / don't start game
	SETVAR_L	(A0)+,execute_flag	; Treat as a normal program
	SETVAR_L	(A0)+,noquit_flag	; if EXECUTE on, JST will never return
	SETVAR_L	(A0)+,leavevbr_flag	; when a normal program (EXECUTE), don't zero VBR
	SETVAR_L	(A0)+,d_flag	; Treat as a normal program
	
	IFD	SKELETON_TEST
	SETVAR_L	#1,test_flag
	ENDC


	; propagate to absolute variable
	GETVAR_L	verbose_flag,verbose_flag
	
	rts

; < A1: string
; > D0: value (or 0 if wrong)
; this is a wrapper to be able to copy/paste CD32load
; arg parsing directly (also, A0 is used during arg parsing, 
; whereas A1 isn't)

HexStringToNum:
	STORE_REGS	A0/D1
	move.l	A1,A0
	bsr	hexstr_to_num
	tst.l	d1
	beq.b	.end
	moveq.l	#0,D0
.end
	RESTORE_REGS	A0/D1
	rts
	
; *** Converts a ascii string to number
; both formats $ABCD and 0xABCD are accepted
; in: A0: pointer to source buffer
; out: D0: number
; out: D1: 0 if OK

hexstr_to_num:
	STORE_REGS	D2-D3/A0
	moveq.l	#0,D0
	moveq.l	#-1,D3
	tst.b	(a0)
	beq.b	.out
		
	cmp.b	#'$',(a0)
	beq.b	.hexok
	cmp.b	#'0',(a0)+
	bne.b	.out
	cmp.b	#'X',(a0)
	beq.b	.hexok
	cmp.b	#'x',(a0)
	bne.b	.out	
.hexok
	addq.l	#1,a0
	moveq.l	#0,d1
	moveq.l	#0,d2
.loop
	move.b	(a0)+,d1
	beq.b	.end
	cmp.b	#'9'+1,d1
	bcs.b	.digit
	
	cmp.b	#'G',d1
	bcs.b	.ucletter
	cmp.b	#'g',d1
	bcs.b	.lcletter
	bra.b	.out

.lcletter
	sub.b	#'a',d1
	bmi.b	.out
	add.b	#10,d1
	bra.b	.store

.ucletter
	sub.b	#'A',d1
	bmi.b	.out
	add.b	#10,d1
	bra.b	.store
.digit
	sub.b	#'0',d1
	bmi.b	.out
.store	
	lsl.l	#4,d2
	or.b	d1,d2
	bra.b	.loop
.end
	move.l	d2,d0
	moveq.l	#0,D3

.out
	move.l	d3,d1
	RESTORE_REGS	D2-D3/A0
	rts
	
hexstr_to_num_old:
	STORE_REGS	A0
	moveq.l	#0,D1
	move.b	(A0)+,D0
	cmp.b	#'$',D0
	bne.b	.syntaxerr
.loop:
	move.b	(A0)+,D0
	beq.b	.end		; end OK
	lsl.l	#4,D1
	bsr.b	str_to_num
	tst.b	D0
	bmi.b	.syntaxerr
	or.b	D0,D1
	bra.b	.loop
.end
	move.l	D1,D0		; result
	moveq.l	#0,D1		; OK
	RESTORE_REGS	A0
	RTS
.syntaxerr
	moveq.l	#-1,D1		; error
	RESTORE_REGS	A0
	clr.b	(A0)
	rts

; < D0: digit
; > D0: -1 or 0-15 value

str_to_num:
	cmp.b	#'0',D0
	bcs.b	.wrong
	cmp.b	#'9'+1,D0
	bcs.b	.digit

	cmp.b	#'A',D0
	bcs.b	.wrong
	cmp.b	#'F'+1,D0
	bcs.b	.upper

	cmp.b	#'a',D0
	bcs.b	.wrong
	cmp.b	#'f'+1,D0
	bcs.b	.lower

	; wrong format!
.wrong
	moveq.l	#-1,D0
	rts
	

.digit
	sub.b	#'0',D0		; it's a digit
	rts

.upper
	sub.b	#'A'-10,D0	; it's a uppercase letter
	rts

.lower
	sub.b	#'a'-10,D0	; it's a lowercase letter
	rts

; < a1: tooltype string
; < a2: wb tooltype struct
; < a3: output buffer

find_key_tooltype:
	move.l	a2,a0
	move.l	do_ToolTypes(a0),a0
	JSRLIB	FindToolType
	tst.l	d0
	beq	.nokey
	move.l	a3,D1
	move.l	#10,D2
	jsr	RelFun_StrncpyAsm	; copy (quit|iconify|freeze)key (string)
.nokey
	rts

; < a2: wb tooltype struct

findoption:
	move.l	a2,a0
	move.l	do_ToolTypes(a0),a0
	JSRLIB	FindToolType
	tst.l	d0
	rts

BCPL_RdArgs = 78
	
	; a1 = pointer to result array. Must be LONG aligned!
	; a0 = formatting string. BSTR!
	; d0 = size of result array (number of LONGs)
get_args_BCPL:
	movem.l d2/d3/d4,-(sp)
	move.l d0,d3
	moveq #BCPL_RdArgs,d0
	move.l a0,d1
	lsr.l #2,d1
	move.l a1,d2
	lsr.l #2,d2
	moveq #0,d4
	bsr.s call_bcpl
	movem.l (sp)+,d2/d3/d4
	rts

	; d0 = gv index
	; d1-d4 = bcpl parms

call_bcpl:
	movem.l d2-d7/a2-a6,-(sp)

	move.l d0,d6
	move.l d1,d5

	move.l 4.w,a6
	move.l	_DosBase(pc),a5


	sub.l a1,a1
	JSRLIB	FindTask
	move.l d0,a4

	; allocate BCPL stack
	move.l #1500,d0
	move.l #65536+1,d1
	JSRLIB	AllocMem
	move.l d0,d7
	beq.s .nomem
	
	movem.l d7/a5/a6,-(sp)

	moveq #0,d0
	move.l d5,d1
	sub.l a0,a0
	move.l d7,a1
	lea 3*4(a1),a1
	move.l 136(a4),a2
	lsl.w #2,d6
	move.l 0(a2,d6.w),a4
	movem.l 46(a5),a5/a6
	jsr (a5) ; call bcpl!
	
	movem.l (sp)+,d7/a5/a6

.nomem:
	move.l d7,a1
	move.l #1500,d0
	JSRLIB	FreeMem


	movem.l (sp)+,d2-d7/a2-a6
	rts
; read only MMU tooltype

ReadMMUToolType:

	lea	MMUToolType,a1
	move.l	a2,a0
	move.l	do_ToolTypes(a0),a0
	JSRLIB	FindToolType

	bsr	MatchMMUParam
	rts

mmu_str_state:
	dc.l	0

; < D0: pointer on text to recognize: NONE, LAXIST or FULL (FULL can be anything, actually)
; > mmunumstate_flag, mmu_str_state updated

MatchMMUParam:
	STORE_REGS
	SET_VAR_CONTEXT

	lea	.mmu_str_none(pc),A1

	GETVAR_L	attnflags,D1
	btst	#AFB_68030,D1
	beq.b	.nommu		; 68000, 68010 or 68020: no MMU available

	move.l	#.mmu_str_full,mmu_str_state
	SETVAR_L	#2,mmunumstate_flag

	tst.l	D0
	beq	.exit

	move.l	D0,A0		; pointer on MMU string (FULL|LAXIST|NONE)

	lea	.mmu_str_none(pc),A1	; test MMU=NONE
	move.l	A1,D1
	jsr	RelFun_StrcmpAsm
	tst.l	D0
	bne.b	.trylax
.nommu
	SETVAR_L	#0,mmunumstate_flag
	move.l	A1,mmu_str_state
	bra	.exit
.trylax
	move.l	A0,D0
	lea	.mmu_str_laxist(pc),A1
	move.l	A1,D1
	jsr	RelFun_StrcmpAsm
	tst.l	D0
	bne.b	.exit

	SETVAR_L	#1,mmunumstate_flag
	move.l	A1,mmu_str_state
.exit
	RESTORE_REGS
	rts

.mmu_str_none:
	dc.b	"NONE",0
.mmu_str_full:
	dc.b	"FULL",0
.mmu_str_laxist:
	dc.b	"LAXIST",0
	even


AbsFun_NewLine:		STORE_REGS	A1
	lea	.newlinebuf(pc),A1
	JSRABS	Display
	RESTORE_REGS	A1
	rts

.newlinebuf:	dc.b	10,13,0

; *** system function table

	cnop	0,4

AbsFunTable:	MAKE_ABS_REFS	ADDR

; Loads OSEmu. This is a shitty routine

AbsFun_UseHarryOSEmu:
	STORE_REGS

	SET_VAR_CONTEXT

	JSRABS	NewLine
	PRINT_MSG	msg_This_loader_uses_OSE
	
	PRINT_MSG	msg_OSEmu_is_Harry_Wepl_

	; first try to load OSEmu in the game directory

	lea	local_osemuname,A0
	moveq.l	#5,D0
	jsr	AbsFun_Priv_ReadFileHD

	exg	D1,D0		; file length
	tst.l	D1
	beq.b	.global
	bpl.b	.loadosemu
.global
	move.l	#1,osemu_global	; global (JST PROGDIR:)

	move.l	#osemuname,D0
	move.l	D0,A0
	JSRABS	GetFileLengthAbs
	tst.l	D0
	bmi	OSEmuError

	; *** allocate the big chunk of memory
.loadosemu
	SETVAR_L	D0,osemu_len

	STORE_REGS	A0
	JSRABS	GetMemFlag
	JSRABS	AllocateTheMemory		; no alloc mem fix
	RESTORE_REGS	A0
	SETVAR_L	D0,osemu_ptr
	bne.b	.ok2
	jsr	MemErr
.ok2

	; *** read the file into the allocated memory

	GETVAR_L	osemu_ptr,A1
	GETVAR_L	osemu_len,D1
	tst.l	osemu_global
	beq.b	.1

	; Absolute path

	moveq.l	#-1,D0		; from offset 0
	jsr	ReadTheFileHD
	bra.b	.2
.1
	; Game path

	moveq.l	#-1,D0		; offset 0
	jsr	AbsFun_Priv_ReadFileHD
.2
	tst.l	D0
	bmi	OSEmuError

	NEWLINE

	; *** displays OSEmu release/version if available

	GETVAR_L	osemu_ptr,A1
	cmp.l	#"OSEM",(OSM_ID,A1)
	bne	.oldosemu

	PRINT_MSG	msg_Version

	moveq.l	#0,D0
	move.w	(OSM_VER,A1),D0
	lea	.decbuffer(pc),A1
	JSRGEN	HexToDecString
	JSRABS	Display
	PRINT_MSG	msg_Dot

	GETVAR_L	osemu_ptr,A1
	move.w	(OSM_RELEASE,A1),D0
	lea	.decbuffer(pc),A1
	JSRGEN	HexToDecString
	JSRABS	Display
	PRINT_MSG	msg_Of_osemu_loaded
	bra.b	.exit
.oldosemu
	PRINT_MSG	msg_Obsolete_version_of_

.exit
	; *** disables chipmem gap for 2Megs mirror

	JSRABS	DisableChipmemGap

	RESTORE_REGS
	rts

.decbuffer:
	BLKDECL	l,2,0

OSEmuError:
	PRINT_MSG	msg_Unable_to_find_OSEmu
	jmp	AbsFun_CloseAll

RelocErr:	
	PRINT_MSG	msg_Reloc_routines_not_relocated
	jmp	AbsFun_CloseAll
	
AbsFun_Unsupported1
AbsFun_Unsupported2
AbsFun_Unsupported3
AbsFun_Unsupported4
AbsFun_Unsupported5
AbsFun_Unsupported6
AbsFun_Unsupported7
AbsFun_Unsupported8
AbsFun_Unsupported9
	bra	LoaderVerErr
is_whdload:
	dc.w	0
savedir_forced:	dc.w	0	; the user has set SAVEDIR in the tooltypes or on the CLI
quitkey_str:
	BLKDECL	b,10,0
iconifykey_str:
	BLKDECL	b,10,0
freezekey_str:
	BLKDECL	b,10,0
forceclist_str:
	BLKDECL	b,10,0

	
; Readdir, WHDLoad compliant version
; IN :	d0 = DEF_WHDOFFSET  buffer size (a1)
;	a0 = CPTR   name of directory to scan (relative)
;	a1 = APTR   buffer (MUST reside in Slave !!!)
; OUT :	d0 = DEF_WHDOFFSET  amount of listed names
;	d1 = DEF_WHDOFFSET  dos errcode (0 if all went ok)

AbsFun_Priv_WHDReadDir:
	STORE_REGS	A0

	jsr	SetGamePath	; proper path for relative search

	moveq.l	#-1,D1
	neg.l	D0
	bsr.b	AbsFun_Priv_ReadDir
	moveq.l	#0,D1
	tst.l	D0
	bpl.b	.exit
	moveq.l	#-1,D1	; error
.exit
	RESTORE_REGS	A0
	rts

; *** Reads a directory on disk
; *** The filenames are equally spaced by D1+1
; *** If D1=-1, then the filename length will be
; *** used instead (WHDLoad ListFiles compatibility)

; < A0: pointer on directory
; < A1: output buffer
; < D0: maxentries (negative means it's a bytecount instead)
; < D1: maxnamelen (0 means 20 spaces, -1 means depending on the name)

; > D0: number of files (<0 : error)

AbsFun_Priv_ReadDir:
	STORE_REGS	D1-A6
	
	STORE_REGS	D0
	jsr	AllocInfoBlock
	move.l	D0,D7
	RESTORE_REGS	D0

	SET_VAR_CONTEXT

	cmp.l	(RelVar_maxchip,A4),A1
	bcc.b	.noswap

	; *** buffer is swapped at the moment: relocate it in the swap zone

	
	add.l	(RelVar_chipmirror,A4),A1
.noswap

	move.l	D0,D4
	move.l	D1,D5
	move.l	A1,A5		; A5 is the current buffer pointer
	move.l	A1,A3		; A3 keeps buffer pointer start all the time

	move.l	A0,D1

	jsr	nonfatal_lockdir
	move.l	D0,D6		; tmp lock
	beq	.error

	moveq	#0,D3		; entries

	move.l	D7,D2	; infoblock
	move.l	D6,D1		; lock
	move.l	_DosBase,A6
	JSRLIB	Examine
	tst.l	D0
	beq	.done		; 0 files found

.loop
	move.l	D6,D1
	move.l	D7,D2
	move.l	_DosBase,A6
	JSRLIB	ExNext
	tst.l	D0
	beq	.done

	addq.l	#1,D3		; 1 more file

;	move.l	D7,A0
;	tst.l	fib_DirEntryType(A0)
;	bpl	.itsdir			; it's a directory: ignore

	move.l	D7,A0
	add.l	#fib_FileName,A0	; filename

	jsr	.getspaceamount

	subq.l	#1,D1
	bmi.b	.nextentry	; if null name!!
	move.l	A5,A1

	tst.l	D4
	bpl	.nonamesizecheck

.nonamesizecheck

	; negative: check buffer length

	move.l	A5,D0
	sub.l	A3,D0	; buffer current maxoffset
	neg.l	D4	; -D4
	add.l	D1,D0	; length of name about to be copied
	cmp.l	D0,D4
	bcs.b	.done	; D0>D4: buffer overflow: out!
	neg.l	D4	; -D4 (restore original value)

.copyname
	move.b	(A0)+,(A1)+			; copy in the dirbuffer

	dbeq	D1,.copyname			; stops if 0 or maxlen chars

	clr.b	(-1,A1)		; NULL terminated

.nextentry
	jsr	.getspaceamount
	add.l	D1,A5
	tst.l	D5
	bne	.next
	add.l	#20,A5		; default

.itsdir
	subq.l	#1,D3		; we counted a directory
.next
	tst.l	D4
	bpl.b	.checkentries

	; negative: check buffer length instead of entries

;	move.l	A5,D0
;	sub.l	A3,D0	; buffer current maxoffset
;	neg.l	D4	; -D4
;	cmp.l	D0,D4
;	bcs.b	.done	; D0>D4: buffer overflow: out!
;	neg.l	D4	; -D4 (restore original value)

	bra.b	.loop

.checkentries:
	dbf	D4,.loop

.done
	move.l	D6,D1
	jsr	unlockdir		; unlock directory

	move.l	D3,D0


.exit
	STORE_REGS	D0
	move.l	D7,D0
	jsr	FreeInfoBlock
	RESTORE_REGS	D0

	RESTORE_REGS	D1-A6
	rts

.error
	moveq	#-1,D0
	bra.b	.exit


; Calculates the space between 2 filenames
;
; < D7: infoblock
; < D5: space (if <0, will be calculated depending on the filename)
; > D1: space calculated or =D5 if D5>0

.getspaceamount:
	STORE_REGS	A0/D0
	move.l	D7,A0
	add.l	#fib_FileName,A0	; filename

	moveq.l	#0,D1
	move.w	D5,D1
	bpl.b	.fixedlen

	; whdload mode

	move.l	A0,D0
	JSRGEN	StrlenAsm	; compute filename length
	move.l	D0,D1
	addq.l	#1,D1		; counts null termination
.fixedlen
	RESTORE_REGS	A0/D0
	rts



; Data section

; Moved to the middle. Otherwise access like x(PC) is in sometimes
; impossible. Added by Ralf

charbuf:	blk.l	10,0

osemu_global:	dc.l	0
old_dirlock:	dc.l	0
dirlock:	dc.l	0
fileslock:	dc.l	0
subfileslock:	dc.l	0
exloop_fileslock:	dc.l	0
read_subdir_flag:	dc.l	0
maxfilesize:	dc.l	0
conhandle:	dc.l	0
fhandle:	dc.l	0
debughandle:	dc.l	0

oldstack:	dc.l	0


gene_variables:		dc.l	0

bit24buf_abs:	dc.l	0
extbuf_abs:	dc.l	0
object_ptr_abs:	dc.l	0
mmucode_ptr_abs:	dc.l	0

nbdisks:	dc.l	0
object_entry:	dc.l	0

thisdir:
	dc.b	0	; string : current directory
whddiskname:
	dc.b	"disk.",0
progdir:
	dc.b	"PROGDIR:",0
osemuname:
	dc.b	"PROGDIR:OSEmu.400",0
local_osemuname:
	dc.b	"OSEmu.400",0
currdir:
	dc.b	$22,$22,0	; "" string
	cnop	0,4
MMUCode_FileName:
	dc.b	"PROGDIR:MMUCode",0

object_name_quotes:	dc.b	34	; code for "
object_name:	ds.b	108,0	; object name
	ds.b	90,0	; extra args

fname_base:	ds.b	108,0

	cnop	0,4

nname:	dc.l	0
NoWaitReturn:	dc.l	0
icon_arglist:	dc.l	0
returncode:	dc.l	0
returnaddr:	dc.l	0
Delayaddr:	dc.l	0
diskindex:	dc.w	0
dosname:	dc.b	"dos.library",0
corename:	dc.b	"chipmem.dat",0
extname:	dc.b	"extmem.dat",0
regname:	dc.b	"registers.dat",0
tempdirname:	dc.b	"T:",0
patchlogname:	dc.b	"patch.log",0

CoreMess:	dc.b	"   Coredump written",0
WrongArgsMess:
	dc.b	"Error: No or wrong arguments given",$0A,0

; template for read args
SHAREDOPTS:MACRO
	dc.b	"NOCACHE/S,EXPCHIP/S,NTSC/S,PAL/S,HDLOAD/S,LOWMEM/S,"
	dc.b	"BUTTONWAIT/S,PRELOAD/S,QUIET/S,DELAY/S,"
	dc.b	"VERBOSE/S,NOVBRMOVE/S,FILTEROFF/S,TEST/S,EXECUTE/S,NOQUIT/S,LVBR/S,D/S,"
	dc.b	"QUITKEY/K,CUSTOM1/K/N,CUSTOM2/K/N,CUSTOM3/K/N,CUSTOM4/K/N,CUSTOM5/K/N"	
	ENDM

Template_V36:
	dc.b	"SLAVE/A,CUSTOM=USERDATA/K,DATA/K,"
	dc.b	"MMU/K,DEBUG/S,"
	dc.b	"FREEZERMB/S,FAKEPAD/K/N,"
	dc.b	"RESUME/S,FREEZEKEY/K,ICONIFYKEY/K,"
	dc.b	"FORCECLIST/K,VK/K,VM/K,VMDELAY/K/N,VMMODDELAY/K/N,VMMODBUT/K,"
	dc.b	"JOYPAD/K/N,JOY1RED/K,JOY1GREEN/K,JOY1YELLOW/K,JOY1BLUE/K,JOY1FWD/K,JOY1BWD/K,JOY1PLAY/K,JOY1FWDBWD/K,"
	dc.b	"JOY1RIGHT/K,JOY1LEFT/K,JOY1UP/K,JOY1DOWN/K,"
	dc.b	"JOY0RED/K,JOY0GREEN/K,JOY0YELLOW/K,JOY0BLUE/K,JOY0FWD/K,JOY0BWD/K,JOY0PLAY/K,JOY0FWDBWD/K,"
	dc.b	"JOY0RIGHT/K,JOY0LEFT/K,JOY0UP/K,JOY0DOWN/K,"
	SHAREDOPTS
	dc.b	0
	
	cnop	0,4	; BCPL string, leave that align thing
Template_V34:
	dc.b	.end-Template_V34
	dc.b	"SLAVE,CUSTOM/K,DATA/K,"
	SHAREDOPTS
.end

emptyname:	dc.b	0

exeprescr_command:	dc.b	"execute "
exeprescr_file:	dc.b	"s:jstpre.sh",0
exepostscr_command:	dc.b	"execute "
exepostscr_file:	dc.b	"s:jstpost.sh",0

iconname:	dc.b	"icon.library",0
noname_text:	dc.b	"*NONE SPECIFIED*",0

; some tooltypes have been removed. Not sure if JST is used with icons
ObjnameToolType:	dc.b	"OBJNAME",0
SlaveToolType:	dc.b	"SLAVE",0
SaveDirToolType:	dc.b	"SAVEDIR",0
LoadDirToolType:	dc.b	"DATA",0
QuitKeyToolType:	dc.b	"QUITKEY",0
FreezeKeyToolType:	dc.b	"FREEZEKEY",0
IconifyKeyToolType:	dc.b	"ICONIFYKEY",0
FreezeRmbToolType:	dc.b	"FREEZERMB",0
FilterOffToolType:	dc.b	"FILTEROFF",0
ExecuteToolType:	dc.b	"EXECUTE",0
NtscToolType:	dc.b	"NTSC",0
PalToolType:	dc.b	"PAL",0
QuietToolType:	dc.b	"QUIET",0
NoCachesToolType:	dc.b	"NOCACHE",0
UserDataToolType:	dc.b	"CUSTOM",0  ; was USERDATA
NoCacheToolType:	dc.b	"NOCACHE",0
NoFastToolType:	dc.b	"EXPCHIP",0	; was NOFAST
JoyPadToolType:	dc.b	"JOYPAD",0
HdLoadToolType:	dc.b	"HDLOAD",0
LowMemToolType:	dc.b	"LOWMEM",0
ResumeToolType:	dc.b	"RESUME",0
ForceClistToolType:	dc.b	"FORCECLIST",0
DelayToolType:	dc.b	"DELAY",0
ButtonWaitToolType:	dc.b	"BUTTONWAIT",0
FakePadToolType:	dc.b	"FAKEPAD",0
LeaveVBRToolType:	dc.b	"LEAVEVBR",0
NoVBRMoveToolType:	dc.b	"NOVBRMOVE",0
NoTrapHandlerToolType:	dc.b	"NOTRAPHANDLER",0
LeaveCachesToolType:	dc.b	"LEAVECACHES",0
DebugToolType:	dc.b	"DEBUG",0
TestToolType:	dc.b	"TEST",0
VerboseToolType:	dc.b	"VERBOSE",0
NoquitToolType:	dc.b	"NOQUIT",0
MMUToolType:	dc.b	"MMU",0



	cnop	0,4		; leave this long word alignment
ProgArgs:	blk.l	256,0
ProgArgsEnd:
iconbase:	dc.l	0

nofree_flag:	dc.l	0

;End of data section


; *** Read a file from HD in the directory specified by SAVEDIR
; < A0: filename
; < A1: buffer
; < D1: length to read

AbsFun_Priv_ReadUserFileHD
	STORE_REGS	A0
	VERBOSE_MSG	msg_Reading_user_file_
	bsr	ReadTheFileHD
	RESTORE_REGS	A0
	RTS

; *** Read a file from HD in the directory specified (game data)
; < A0: filename
; < A1: buffer
; < D1: length to read

AbsFun_Priv_ReadFileHD
	STORE_REGS	A0
	jsr	SetGamePath
	VERBOSE_MSG	msg_Reading_file_
	bsr	ReadTheFileHD
	RESTORE_REGS	A0
	RTS

; *** Read a file from HD (used during OSSWAP)
; < A0: filename
; < A1: buffer
; < D1: length to read
; < D0: Command: 0: read, 5: length, -1: read (ignores offset)
; > D0: 0 if OK, other value if error
; > D1: length read
; < fileoffset contains the offset to move to

AbsFun_Priv_ReadTheFileHD:
ReadTheFileHD:
	STORE_REGS	D2-A6

	SET_VAR_CONTEXT

	jsr	VerboseNameA0

	move.l	D0,D6			; command
	moveq	#0,D7			; offset 0

	cmp.l	#-1,D0			; command 1: no offset
	beq.b	.skipoff

	GETVAR_L	fileoffset,D7			; offset

.skipoff
;;	cmp.l	reloc_maxchip,A0
;;	bcc	.noswap1

	; *** buffer is swapped at the moment: read the filename in the swap zone
	; *** this is also done in SetGamePath and SetUserPath, but
	; *** doing it twice is no harm, and I think this is useful
	; *** for ReadFilePart

;;	add.l	reloc_chipmirror,A0
;;.noswap1

	cmp.b	#5,D6			; getlength command?
	beq.b	.getlen

	tst.l	D1
	bpl	.readapart		; length specified: don't get the filesize

.getlen

	move.l	A0,D0
	JSRABS	GetFileLengthAbs
	cmp.b	#5,D6			; getlength command?
	bne	.reallyread	
	move.l	D0,D1
	bmi.b	.nofile
	moveq.l	#1,D0
	VERBOSE_MSG	msg_OK_po_length_pf_
	bra	.exit

.nofile
	VERBOSE_MSG	msg_FAIL_po_length_pf_
	moveq.l	#0,D0	
	bra	.exit

.reallyread
	move.l	D0,D1
	sub.l	D7,D1			; if there's an offset, substract it
.readapart
	move.l	A1,D4			; buffer
	cmp.l	(RelVar_maxchip,A4),D4
	bcc.b	.noswap2

	; *** buffer is swapped at the moment: copy it in the swap zone

	add.l	(RelVar_chipmirror,A4),D4
.noswap2
	
	move.l	D1,D6			; size to read

	move.l	A0,D1			; filename

	move.l	#MODE_OLDFILE,D2	; read
	move.l	_DosBase,A6
	JSRLIB	Open
	move.l	D0,D5
	beq	.error

	tst.l	D7
	beq.b	.noseek

	move.l	D5,D1			; filehandle
	move.l	D7,D2			; position
	move.l	#OFFSET_BEGINNING,D3
	JSRLIB	Seek

.noseek
	move.l	D5,D1			; filehandle
	move.l	D4,D2			; buffer
	move.l	D6,D3			; size
	beq.b	.read_zero

	JSRLIB	Read

	move.l	D0,D6			; size read
.read_zero:
	move.l	D5,D1
	move.l	_DosBase,A6
	JSRLIB	Close
	
	TSTVAR_L	delay_flag
	beq.b	.nowait
	move.l	_DosBase,A6
	move.l	#30,D1
	JSRLIB	Delay		; wait 3/5 second before restarting (slow media)
.nowait

	moveq.l	#0,D0
	VERBOSE_MSG	msg_OK

	move.l	D6,D1			; file length
.exit
	RESTORE_REGS	D2-A6
	rts

.error
	VERBOSE_MSG	msg_FAIL
	moveq.l	#-1,D0
	bra	.exit

;.filelock:	dc.l	0

; *** Write a file (partially) on HD in the game directory
; < A0: filename
; < A1: buffer
; < D1: length to read
; < reloc_offset contains the offset to move to
; (the file is opened in MODE_READWRITE)

AbsFun_Priv_WriteFilePartHD:
	STORE_REGS	D2-A6
	SET_VAR_CONTEXT
	; first locate file in filecache and  if found
	; else create a new entry, empty/dirty
	; store entry in "found_dir_entry" whatever the result is
	bsr	CreateFileInCache
	
    ; sets path in game directory: note this destroys the "relative" name
	; that's why storing "found_dir_entry" is useful
	jsr	SetGamePath		

	VERBOSE_MSG	msg_Modifying_file_
	jsr	VerboseNameA0

	move.l	A1,D4			; buffer


	move.l	D1,D6			; size to write

	move.l	A0,D1			; filename
	move.l	A0,A3
	move.l	A1,A5

	move.l	#MODE_READWRITE,D2	; read/write
	move.l	_DosBase,A6
	JSRLIB	Open
	move.l	D0,D5
	beq	WFPHD_error

	GETVAR_L	fileoffset,D7			; offset
	beq.b	.noseek			; if 0, useless

	move.l	D5,D1			; filehandle
	move.l	D7,D2			; position
	move.l	#OFFSET_BEGINNING,D3
	move.l	_DosBase,A6
	JSRLIB	Seek

.noseek
	cmp.l	(RelVar_maxchip,A4),D4
	bcc.b	.noswap

	; *** buffer is swapped at the moment: copy it in the swap zone

	add.l	(RelVar_chipmirror,A4),D4
.noswap		

	move.l	D5,D1			; filehandle
	move.l	D4,D2			; buffer
	move.l	D6,D3			; size
	beq.b	.write_zero
	move.l	_DosBase,A6
	JSRLIB	Write

	move.l	D0,D6			; size written
.write_zero

	TSTVAR_L	ostotrash
	beq	.skipsafe		; no need to re-read and wait
					; since the OS was up before the call!

	; the file is dirty i.e. the data on disk does not
	; match the data held in JST file cache
	; since it's just been written to! (that was a long time bug, hopefully
	; fixed in 6.1)

	bsr	update_file_from_disk

	; position to start of file

	move.l	D5,D1			; filehandle
	moveq.l	#0,D2			; start position
	move.l	#OFFSET_BEGINNING,D3
	move.l	_DosBase,A6
	JSRLIB	Seek

	bra.b	.noseek2

.out
	; Seek to written part again

	tst.l	D7
	beq	.noseek2			; if 0, useless

	move.l	D5,D1			; filehandle
	move.l	D7,D2			; position
	move.l	#OFFSET_BEGINNING,D3
	move.l	_DosBase,A6
	JSRLIB	Seek

.noseek2
	; read the written part again: safer to make disk buffers flush

	move.l	D5,D1			; filehandle
	move.l	D4,D2			; buffer returned by update_file_from_disk
	move.l	D6,D3			; size returned by update_file_from_disk
	beq	.read_zero
	move.l	_DosBase,A6
	JSRLIB	Read			; read the part again in the same buffer/fileid buffer

.read_zero
	move.l	#100,D1			; 2 seconds wait (buffer flush)
	move.l	_DosBase,A6
	JSRLIB	Delay

.skipsafe
	move.l	D5,D1
	move.l	_DosBase,A6
	JSRLIB	Close

	moveq.l	#0,D0
	move.l	D6,D1			; file length
.exit
	RESTORE_REGS	D2-A6
	rts

.error
	moveq.l	#-1,D0
	bra.b	.exit

; *** Write a file on HD in the directory specified by SAVEDIR
; < A0: filename
; < A1: buffer
; < D1: length to read

AbsFun_Priv_WriteUserFileHD:
	STORE_REGS	A0
	bsr	CreateFileInCache
    ; sets path in game directory: note this destroys the "relative" name
	; that's why storing "found_dir_entry" is useful
	jsr	SetGamePath		

	VERBOSE_MSG	msg_Writing_user_file_
	jsr	VerboseNameA0

	bsr	WriteTheFileHD
	RESTORE_REGS	A0
	RTS

; *** Write a file on HD in the game directory
; < A0: filename
; < A1: buffer
; < D1: length to read

WriteFileHD:	; procedure start
	bsr	CreateFileInCache
    ; sets path in game directory: note this destroys the "relative" name
	; that's why storing "found_dir_entry" is useful
	jsr	SetGamePath		

	VERBOSE_MSG	msg_Writing_file_
	jsr	VerboseNameA0
	JSRABS	NewLine

	bra.b	WriteTheFileHD


; *** Write a file from HD (used during OSSWAP)
; *** Warning: do not use a copyback partition
; < A0: filename
; < A1: buffer
; < D1: length to read

AbsFun_Priv_WriteTheFileHD:	; procedure start
	bsr	CreateFileInCache
    ; sets path in game directory: note this destroys the "relative" name
	; that's why storing "found_dir_entry" is useful
	jsr	SetGamePath		

WriteTheFileHD:
	SET_VAR_CONTEXT

	move.l	A1,D4			; buffer

	move.l	D1,D6			; size to write

	move.l	A0,D1			; filename
	move.l	A0,A3			; save filename
	move.l	A1,A5			; buffer

	move.l	#MODE_NEWFILE,D2	; write
	move.l	_DosBase,A6
	JSRLIB	Open
	move.l	D0,D5
	beq	WFPHD_error

	; also a continue point for AbsFun_Priv_WriteFilePartHD

	; *** ooops! moved the add swapbuffer there for AbsFun_Priv_WriteFilePartHD

	cmp.l	(RelVar_maxchip,A4),D4
	bcc.b	.noswap

	; *** buffer is swapped at the moment: get the data from the swap zone

	add.l	(RelVar_chipmirror,A4),D4
.noswap		

	move.l	D5,D1			; filehandle
	move.l	D4,D2			; buffer
	move.l	D6,D3			; size
	beq	.write_zero
	move.l	_DosBase,A6
	JSRLIB	Write

	move.l	D0,D6			; size written
.write_zero
	move.l	D5,D1
	move.l	_DosBase,A6
	JSRLIB	Close

	TSTVAR_L	ostotrash
	beq	.skipsafe

	; the file is dirty i.e. the data on disk does not
	; match the data held in JST file cache since the file just has
	; been written to
	; we need to fix this, or big trouble can happen
	; V3.4 and older versions of JST solved the problem
	; by destroying the filename in the JST file cache,
	; -> the file was no longer referenced. This solution
	; actually works, but the file is re-read all the time
	; which can be inacceptable for some loaders
	; (Operation Stealth), that's why something better must be done

	bsr	update_file_from_disk

.out:
	; we re-read the file written, just to be sure
	; that the data has been written to disk
	; (and also to update the ram buffer)
	; then we wait 2 seconds

	STORE_REGS

	move.l	_DosBase,A6
	move.l	A3,D1			; filename
	move.l	#MODE_OLDFILE,D2	; read
	JSRLIB	Open

	move.l	D0,D5
	beq.b	.wait		; not really possible, but...

	move.l	D5,D1			; filehandle
	move.l	D4,D2			; buffer returned from update_file_from_disk
	move.l	D6,D3			; size returned from update_file_from_disk
	beq.b	.close
	JSRLIB	Read		; read the written data at the same location

.close
	move.l	D5,D1
	JSRLIB	Close		; close the file

	; removed because caused crashes, probably due to the nested call
	; of ReadFileHD here. We should have done a simple Read

;	move.l	A5,A1			; same buffer as above
;	move.l	D6,D1			; same size as above
;	moveq.l	#0,D0
;	jsr	ReadFileHD

.wait
	move.l	#100,D1			; 2 seconds wait (buffer flush)
	move.l	_DosBase,A6
	JSRLIB	Delay

	RESTORE_REGS

.skipsafe
	VERBOSE_MSG	msg_File_written

	moveq.l	#0,D0
	move.l	D6,D1			; file length
	rts


WFPHD_error
	moveq.l	#-1,D0
	rts


; <A3: full path filename
; >D4: current written buffer
; >D6: current written size
; uses "found_dir_entry" global variable

update_file_from_disk:
	tst.l	found_dir_entry
	bne.b	.do
	; safety in case it's called out of context
	rts
.do
	STORE_REGS	D0/D2/A4-A6

	move.l	A3,D0
	JSRABS	GetFileLengthAbs
	move.l	d0,d2		; backup file size in D2
	
	lea	(-$100,A7),A7	; allocate some stack space

	SET_VAR_CONTEXT

	move.l	found_dir_entry,A5

	move.l	(4,A5),D0	; file size
	cmp.l	d0,d2
	beq	.samesize	; same size: no need to free/realloc

	; first, backup the filename in a temporary buffer

	move.l	(A5),D0
	add.l	(4,A5),D0
	move.l	A7,D1
	JSRGEN	StrcpyAsm

	; second, free the diskfile buffer
	
	; don't forget the filename size !!
	; huge bug here (fixed in 6.1) since a long time probably can guru 81000005
	; since size of the freed block is incorrect
	
	move.l	(4,A5),D0
	add.l	#FULLNAME_SIZE,D0
	move.l	(A5),A1
	JSRABS	FreeTheMemory
	clr.l	(A5)		; reset to zero
.nofree

	; allocate memory again, with the right size

	move.l	d2,d0
	move.l	d0,(4,A5)		; new file size

	add.l	#FULLNAME_SIZE,D0
	JSRABS	GetMemFlag
	JSRABS	AllocateTheMemory	; MMU aligned please
	move.l	d0,(A5)		; new buffer (or 0!)
	beq	.aargh		; new size is not avail!!

	; copy back the name in the buffer (at the end)

	move.l	A7,D0
	move.l	(A5),D1
	add.l	(4,A5),D1	; adds filesize
	JSRGEN	StrcpyAsm

	; we need to register our new allocated block to the MMU
	; but we cannot do it now. We store the A5 register
	; in a special variable. During the OS swap, JST will
	; mark the new block valid

	move.l	A5,new_directory_entry
.samesize
	; we've got the new buffer properly allocated
	; just change D4 & D6 values so the safety Read
	; will update the fileid buffer for us (tricky but good)

	move.l	(A5),D4		; buffer
	move.l	(4,A5),D6	; size
.exit
	lea	($100,A7),A7	; free the stack space
	RESTORE_REGS	D0/D1/A4-A6
	rts

		IFEQ	1
.invalidate:
	; trash the filename so file has to be reread from disk

	move.l	(A5),A4
	add.l	(4,A5),A4

	tst.l	verbose_flag
	beq.b	.skipv
	VERBOSE_MSG	msg_uncacheing_file
	STORE_REGS	A1
	move.l	A4,A1
	JSRABS	Display
	RESTORE_REGS	A1
.skipv

	; trash the name

	move.b	#1,(A4)+
	move.b	#2,(A4)+
	move.b	#3,(A4)+
	move.b	#4,(A4)+
	bra.b	.exit
	ENDC
	
	
.aargh:
	; could not allocate mem for the new filesize
	; ok the situation is serious but not desperate
	; just put -1 in the pointer, JST will know
	; there's a problem and will ignore the pointer
	; (hopefully)

	move.l	found_dir_entry,A5
	move.l	#-1,(A5)
	bra.b	.exit

; mark directory entry that was just added by Write.*FileHD

AbsFun_Priv_MarkNewEntryValid:
	SET_VAR_CONTEXT
	CMPVAR_L	#2,mmunumstate_flag
	BNE.b	.nodirupdate
	tst.l	new_directory_entry
	beq.b	.nodirupdate		; =0: no new dir entry

	STORE_REGS
	move.l	new_directory_entry(pc),A5
	MOVE.L	(A5)+,D0	; start
	MOVE.L	(A5),D1
	add.l	D0,D1		; end of data
	add.l	#FULLNAME_SIZE,D1	; end of data + filename (nasty bug fixed in 6.1)
	CALL_MMUCodeABS	MarkBlockValid
	clr.l	new_directory_entry
	RESTORE_REGS
.nodirupdate:
	rts

new_directory_entry:
	dc.l	0

; prints the file name on the output if VERBOSE is on
; < A0: text

VerboseNameA0:
	tst.l	verbose_flag
	beq.b	.exit
	STORE_REGS	A1
	move.l	A0,A1
	JSRABS	Display
	JSRABS	NewLine
	RESTORE_REGS	A1
.exit
	rts

VerboseNewLine:
	tst.l	verbose_flag
	beq.b	.exit
	JSRABS	NewLine
.exit
	rts

; *** Checks if a file is in JST file cache
; *** Result: no registers modified, but sets the file_in_cache variable to
;
; 0: file not in cache
; 1: file in cache, clean
; 2: file in cache, dirty
;
CreateFileInCache:
	STORE_REGS	D0
	moveq.l	#1,d0
	bsr	CheckOrCreateFileInCache
	RESTORE_REGS	D0
	rts
	
CheckFileInCache:
	STORE_REGS	D0
	moveq.l	#0,d0	; don't create if missing
	bsr	CheckOrCreateFileInCache
	RESTORE_REGS	D0
	rts
	
; and sets fileid struct pointer in found_dir_entry variable
;
; <A0: filename (without path add-ons)
; <D0: 1: create entry if not found, 0: don't (when deleting)

CheckOrCreateFileInCache:
	STORE_REGS	D0/D2/A4/A5
	SET_VAR_CONTEXT
	move.l	D0,D2	; store create if not found flag
	clr.l	found_dir_entry
	move.b	#FIC_NO,file_in_cache	; default: no
	TSTVAR_L	gene_patchbuffer
	beq.b	.goout

	JSRGEN	Priv_SearchDirEntry
	move.l	A5,found_dir_entry	; store found dir entry
	tst.l	(A5)
	beq.b	.notincache		; not in cache

	move.b	#FIC_CLEAN,file_in_cache ; file in cache
	tst.l	D0
	beq.b	.goout
	move.b	#FIC_DIRTY,file_in_cache ; file in cache, but dirty
.goout:
	RESTORE_REGS	D0/D2/A4/A5
	rts
.notincache:
	tst.l	D2
	beq.b	.goout	; requested not to create an entry for the file
	
	; create a new entry dynamically
	STORE_REGS
	GETVAR_L	fileidlen,A2	; current size
	ADDVAR_L	fileidbuffer,A2
	sub.l	#10,A2	; last (nul-terminating) entry
	move.l	A0,D2	; filename (backuped)
	move.l	#FULLNAME_SIZE,D0	 ; just for the file name (create empty file entry)
	JSRABS	GetMemFlag
	JSRABS	AllocateTheMemory
	tst.l	d0
	bne.b	.memok
	bsr	MemErr		; never returns
.memok
	move.l	D0,(A2)+
	clr.l	(A2)+	; zero-file size for now
	clr.w	(A2)	; file
	move.l	d0,d1	; dest of string copy
	move.l	d2,d0	; source of string copy
	JSRGEN	StrcpyAsm
	
; < D0: size
; < D1: buffer (contents+name)
; < D2: type: 0: file, 1: dir

	moveq.l	#0,d0
	moveq.l	#0,d1
	moveq.l	#0,d2
	; nul-terminate again
	bsr	push_buffer_and_size
	; store the new entry (possibly moved) in found_dir_entry
	GETVAR_L	fileidlen,A2	; current size
	ADDVAR_L	fileidbuffer,A2
	sub.l	#20,A2	; skip last (nul-terminating) entry to point to the last entry
	move.l	A2,found_dir_entry
	move.b	#FIC_DIRTY,file_in_cache ; file in cache, but dirty (actually empty)
	RESTORE_REGS
	bra	.goout
	
; *** Delete a file from HD
; *** from the user directory

AbsFun_Priv_DeleteUserFileHD
	; first locate file in filecache and store entry in "found_dir_entry" if found
	bsr	CheckFileInCache

	jsr	SetGamePath		; sets path in game directory

	VERBOSE_MSG	msg_Deleting_user_file_
	bra.b	DeleteTheFileHD

; *** Delete a file from HD (used during OSSWAP)
; *** Warning: do not use a copyback partition

AbsFun_Priv_DeleteFileHD
	bsr	CheckFileInCache

	jsr	SetGamePath
	VERBOSE_MSG	msg_Deleting_file_
DeleteTheFileHD:
	jsr	VerboseNameA0
	jsr	VerboseNewLine

	STORE_REGS	D1-D6
	move.l	A0,D1		; filename
	move.l	_DosBase,A6
	JSRLIB	DeleteFile

	move.l	D0,D5		; store return value in D5
	beq.b	.error

	; check if file in JST cache

	cmp.b	#FIC_NO,file_in_cache
	beq.b	.out

	; file is in cache, let's trash the name
	; note that this isn't optimal as if the file is re-created
	; a new block will be allocated, this one won't be freed until
	; the game is quitted

	move.l	found_dir_entry,A5
	cmp.l	#0,a5
	beq.b	.out
	move.l	(A5),A0
	add.l	(4,A5),A0

	move.b	#$DE,(A0)+
	move.b	#$AD,(A0)+
	move.b	#$BE,(A0)+
	move.b	#$EF,(A0)+
	clr.l	found_dir_entry

.error:	
.out:
	move.l	#100,D1			; 2 seconds wait (buffer flush)
	move.l	_DosBase,A6
	JSRLIB	Delay

	move.l	D5,D0			; error flag
	tst.l	D0
	bne.b	.okay
	; gets error from IoErr()

	move.l	_DosBase,A6
	JSRLIB	IoErr
	move.l	D0,D1	; error code in D1
	moveq.l	#0,D0	; error flag set
.okay
	RESTORE_REGS	D1-D6

	rts

; *** memory allocation for 512K expansion
; in: D0 size
; out:D0: allocated location

AbsFun_AllocExtMem:	; procedure start	
	STORE_REGS	D1-A6
	tst.l	D0
	beq	.exit
	ALIGN_ON_MMU	D0


	SET_VAR_CONTEXT

	TSTVAR_L	extbuf
	bne	AEMTwiceErr

	move.l	D0,D6		; bytesize
	JSRABS	GetMemFlag	; reverse if possible
	or.l	#MEMF_CLEAR,D1	; filled with zeroes

	TSTVAR_L	nofast_flag
	beq	.skipchip
	or.l	#MEMF_CHIP,D1	; force chipmem, but not $80000 location

.skipchip
	ALLOC_ABS_OR_DYN	extbuf
	SETVAR_L	D0,extbuf

	beq	.lowchip

	SETVAR_L	D6,extsize

	tst.l	verbose_flag
	beq	.1
	VERBOSE_MSG	msg_Extension_memory_at_
	Mac_printh	D0
.1
	CMPVAR_L	#$80000,extbuf
	bcs	.lowchip

	move.l	D0,A0

	move.l	#'EXTF',(A0)+
	clr.l	(A0)+
	move.l	#'JOTD',(A0)+
	clr.l	(A0)+
.exit
	RESTORE_REGS	D1-A6
	rts

; *** allocated chip is below $80000: no extension

.lowchip
	tst.l	verbose_flag
	beq	.skipv
	PRINT_MSG	msg_No_extension_memory_	
.skipv
	jsr	FreeExtMem
	moveq.l	#0,D0		; returns no memory for extension
	bra.b	.exit

; *** memory allocation for 512K expansion (24 bit only)
; in: D0 size
; out:D0: allocated location

AbsFun_Alloc24BitMem:	; procedure start
	STORE_REGS	D1-A6
	tst.l	D0
	beq	.exit
	ALIGN_ON_MMU	D0

	SET_VAR_CONTEXT

	TSTVAR_L	bit24buf
	bne	A2MTwiceErr

	move.l	D0,D6		; bytesize
	JSRABS	GetMemFlag	; reverse if possible
	or.l	#MEMF_CLEAR|MEMF_24BITDMA,D1	; filled with zeroes

	TSTVAR_L	nofast_flag
	beq.b	.skipchip
	or.l	#MEMF_CHIP,D1	; force chipmem, but not $80000 location
.skipchip
	move.l	_SysBase,A6

	ALLOC_ABS_OR_DYN	bit24buf

	SETVAR_L	D0,bit24buf
	beq	.lowchip
	SETVAR_L	D6,bit24size

	tst.l	verbose_flag
	beq	.1
	PRINT_MSG	msg_bit_Extension_memory
	Mac_printh	D0
.1
	CMPVAR_L	#$80000,bit24buf
	bcs	.lowchip

	move.l	D0,A0

	move.l	#'EXTC',(A0)+
	clr.l	(A0)+
	move.l	#'JOTD',(A0)+
	clr.l	(A0)+
.exit
	RESTORE_REGS	D1-A6
	rts

; *** allocated chip is below $80000: no extension

.lowchip
	tst.l	verbose_flag
	beq	.skipv
	PRINT_MSG	msg_No_bit_extension_mem	
.skipv
	jsr	Free24BitMem
	moveq.l	#0,D0		; returns no memory for extension
	rts

; *** Free object memory

FreeObjMem:	; procedure start
	SET_VAR_CONTEXT

	GETVAR_L	object_len,D0
	beq	.exit
	GETVAR_L	object_ptr,A1
	JSRABS	FreeTheMemory
	CLRVAR_L	object_len
	CLRVAR_L	object_ptr	; cleanup
.exit
	rts

; *** Free both extension memory chunks

FreeExtMem:	; procedure start
	STORE_REGS
	SET_VAR_CONTEXT

	GETVAR_L	extbuf,A1
	GETVAR_L	extsize,D0

	JSRABS	FreeTheMemory
	CLRVAR_L	extsize
	CLRVAR_L	extbuf

	RESTORE_REGS

	rts

Free24BitMem:	; procedure start
	STORE_REGS
	SET_VAR_CONTEXT

	GETVAR_L	bit24buf,D0
	beq.b	.exit

	move.l	D0,A1
	GETVAR_L	bit24size,D0
	beq.b	.exit

	JSRABS	FreeTheMemory
	CLRVAR_L	bit24size
	CLRVAR_L	bit24buf
.exit

	RESTORE_REGS

	rts

AbsFun_OpenFakeExec:	; procedure start
	PRINT_MSG	msg_OpenFakeExec_po_pf_c
	JMPABS	CloseAll


AbsFun_DisableChipmemGap:	; procedure start
	move.l	#1,chipmemgap_disabled
	rts

; *** Saves chipmem data (OSDATA)

AbsFun_SaveOSData:

	clr.b	TaskCheck
	SET_VAR_CONTEXT

	SETVAR_L	A7,system_userstack	; %ADDED

	move.l	(A7),returnaddr

	tst.l	D0
	bne.S	.NOTNULL

	move.l	_SysBase,A6
	MOVE.L	MaxLocMem(A6),D0	; gets maximum chipmem

.NOTNULL
	CMP.L	#$1FE000,D0
	BGT.S	.SAVE2MB

	ALIGN_ON_MMU	D0

	BRA.S	.SAVE_DATA

.SAVE2MB
	MOVEQ	#$20,D0
	SWAP	D0

.SAVE_DATA
	move.l	D0,D7			; max chip addr

	cmp.l	#$200000,D7	; save 2MB of chipmem?
	bne	.skip2MB

	TSTVAR_L	debug_flag	; disabled if debug
	bne	.skip2MB

	tst.l	chipmemgap_disabled	; disabled if coder requests it (v2.2)
	bne	.skip2MB

	TSTVAR_L	resume_flag	; disabled if RESUME
	bne	.skip2MB

	JSRABS	CheckFastMem
	move.l	D0,D1			; store result in D1
	move.l	D7,D0			; restore max chip addr
	cmp.l	#$200000,D1
	bcc	.skip2MB	; we've got enough mem to live without gap

	; *** the user requested 2MB of chip to be saved
	; *** this makes too much for us (swap+memory)
	; *** try to reduce this by allocating a large block
	; *** of chip memory that won't be swapped

	TSTVAR_L	resume_flag
	bne	ResumeErr

	move.l	#-1,snapshot_forbidden		; no way to make a snapshot

	move.l	#MEMF_LARGEST|MEMF_CHIP,D1
	move.l	_SysBase,A6
	JSRLIB	AvailMem			; no alloc mem fix

	SUB.L	#PAGE_SIZE_C*2,D0
	SETVAR_L	D0,saved_mem_size

	; *** allocate the big chunk of memory

	move.l	#MEMF_LARGEST|MEMF_CHIP,D1
	JSRABS	AllocateTheMemory		; no alloc mem fix
	SETVAR_L	D0,saved_mem_ptr

	move.l	D0,D1
	beq.b	.skip2MB		; impossible, but...
	ADDVAR_L	saved_mem_size,D1
	SETVAR_L	D1,endsaved_mem_ptr	; start to swap again

	; *** change the size from $200000 to $200000-MaxAlloc

	move.l	#$200000,D0
	SUBVAR_L	saved_mem_size,D0
	add.l	#$10,D0			; safety...

.skip2MB
	SET_VAR_CONTEXT	; now address register 4 is loaded with reloc vars

	SETVAR_L	D0,chipsize

	JSRABS	GetMemFlag
	or.l	#MEMF_CLEAR,D1
	move.l	_SysBase,A6
	JSRABS	AllocateTheMemory	; no alloc mem fix
	cmp.l	D7,D0			; is D0 < D7 (max_chip_addr??)

	; if branch: no chance to save osdata
	; no OS swap and no quit, but the game will run

	bcs.b	.mirrortoolow		
	
	; alloc OK, saves chip mirror pointer and top chip

	SETVAR_L	D0,chipmirror

	SETVAR_L	D7,maxchip	; = $80000, $100000 or $200000 (typically)

	TSTVAR_L	resume_flag
	beq.b	.nochipload

	jsr	ResumeFrozen_2		; load saved memory

;	move.l	_DosBase,A6
;	move.l	#10,D1
;	JSRLIB	Delay			: waits 1/5 second

.nochipload
;;StartResumed
.mirrortoolow

	move.l	_SysBase,A6
	JSRLIB	Disable

	; switches to supervisor mode, using system user stack

	JSRABS	Priv_SupervisorMode
	
	; from now use temporary stack because some patches
	; relocate SSP to speed things up (because SSP is
	; generally in chipmem)

	LEAVAR	tmpstack,A7

	; freeze interrupts from now
	
	move.w	#$2700,SR

	move.b	AttnFlags+1(A6),D0
	btst	#AFB_68010,D0
	beq.b	.novbrzero
	moveq	#0,D0
	CHANGEVBR	D0
.novbrzero

	; defines MMU valid areas (if MMU=FULL is selected)

	bsr	MMUSetup

;;;;;	; switches to supervisor mode, using system user stack

;;;;;	JSRABS	Priv_SupervisorMode
	
	move.l	_SysBase,A6
	JSRLIB	Enable		; enable, but SR is still at $27xx so no interrupts

	; now we save the system custom registers

	bsr	custom_regs_save

	; register bus error/access faults routine to MMUCode

	JSRGEN	Priv_RegisterBusErrorRoutine

	; difference between resumed and fresh start

	TSTVAR_L	resume_flag
	beq	.fresh_start
	JSRABS	UserMode
	bra	ReturnToGame

.fresh_start
	; disables MMU at the moment
	; because we're still in the absolute section

	JSRABS	Priv_DisableMMU

	GETVAR_L	chipmirror,D0
	beq.b	.nomem			; no memory for OS swap
	move.l	D0,A1
	lea	$0.W,A0
	GETVAR_L	maxchip,D7
	GETVAR_L	saved_mem_ptr,D2
	GETVAR_L	endsaved_mem_ptr,D3

.chipcopy
	tst.l	D2
	beq.b	.nosave		; normal copy operation

	cmp.l	A0,D2
	bcc.b	.nosave		; A0 < allocated buff : copy memory

	cmp.l	A0,D3
;	bcs.b	.nosave		; A0 >= end allocated : copy memory
	BLE.S	.nosave

	addq.l	#4,A0		; next address
	bra.b	.next

.nosave
	move.l	(A0)+,(A1)+	; copy memory in the FAST buffer
.next
	cmp.l	D7,A0
	bne.b	.chipcopy

.nomem


EndSaveOSData:


	; installs the exception handler:
	; - no more gurus
	; - VBR relocated if NOVBRMOVE is off
	; - JST exceptions/interrupts activated (autoquit, freezekey, ...)

	JSRGEN	PatchZeroPage

	; clears longword 0

	clr.l	$0.W
	lea	$4.W,A1	; Kills ExecBase anyway

.nodead
	TSTVAR_L	osemu_ptr
	beq.b	.6

	; *** OSEmu copper pointer in $3FC

	lea	$3FC.W,A1
.6
	; *** sets copperlist in a zero page zone
	; *** prevents WHDLoad version of Gods from crashing (for instance)

	lea	$DFF000,A6
	move.l	#$FFFFFFFE,(A1)
	move.l	A1,cop1lc(A6)

	SETVAR_L	#1,ostrashed

	; black screen

	JSRGEN	BlackScreen

	; flushes caches

	JSRGEN	FlushCachesHard

	pea	.SetStackCode(PC)
	MOVE.L	(a7)+,$80.W
	TRAP	#0


; relocates stack and clears the chip memory
; (some games expect it to be cleared)

.SetStackCode:
	GETVAR_L	maxchip,A7; problem if no swap memory
;;;	move.l	D7,A7		; chip size requested to save

	MOVE.L	A7,A0
	sub.l	#$12,A7		; safety (like whdload start)
	
; goes into user mode

	MOVE	#$700,SR

; sets user stack location

	LEA	-$400(A0),A7	; $400->$800 in JST v3.3 -> $400

; switches in supervisor mode again
	
	pea	.supcode(PC)
	MOVE.L	(A7)+,$80.W
	TRAP	#0

.supcode
;	MOVE	#$C008,$DFF09A		; added by Ralf


.No_IRQ_Activate

	; Jeff: moved InstallHarryOSEmu below

	move.l	returnaddr(pc),-(A7)	; push return address on stack

;	CMPVAR_L	#2,mmunumstate_flag
;	BNE.S	.No_MMUActivate
;	JSRGEN	Priv_RegisterBusErrorRoutine

	; will jump to EnableMMU before returning to the loader

	GETVAR_L	gene_patchbuffer,-(A7)
	ADD.L	#Rel_RelFunTable+RelOff_EnableMMU,(A7)
	STORE_REGS	A0
	MOVE.L	4(A7),A0
	MOVE.L	(A0),4(A7)
	RESTORE_REGS	A0


.No_MMUActivate
	; jump to relocated section: clear chipmem, install OSEmu if needed

	JMPGEN	Priv_EndSaveOsData

custom_regs_save:
	STORE_REGS	A6
	lea	$DFF000,A6
	move.w	#$4000,intena(A6)	; disable interrupts, freeze the system state

	JSRGEN	SaveCustomRegs
	JSRGEN	SaveCIARegs

	lea	$DFF000,A6
	move.w	#$7FFF,intreq(A6)	; disable interrupt requests
	move.w	#$0020,dmacon(A6)	; remove sprite dma

	RESTORE_REGS	A6
	RTS


MMUSetup:
	STORE_REGS
	SET_VAR_CONTEXT

	CMPVAR_L	#2,mmunumstate_flag
	BNE	.No_MMUProtect

	CALL_MMUCodeABS	BuildRootTable

	MOVEQ	#0,D0
	MOVE.L	D7,D1
	CALL_MMUCodeABS	MarkBlockValid

	GETVAR_L	object_ptr,D0
	beq.b	.skipobj
	GETVAR_L	object_len,D1
	ADD.L	D0,D1
	CALL_MMUCodeABS	MarkBlockValid
.skipobj

	MOVE.L	#$BFD000,D0
	MOVE.L	#$BFEFFF,D1
	CALL_MMUCodeABS	MarkBlockValid

	; mark jst relocated variables as valid

	move.l	A4,D0	; relocated variables
	move.l	D0,D1
	add.l	#Rel_ENDVARS-Rel_STARTVARS,D1
	CALL_MMUCodeABS	MarkBlockValid

	; mark jstrel code as valid

	GETVAR_L	gene_patchbuffer,A0
	move.l	A0,D0
	move.l	d0,d1
	add.l	(RelVar_jstrel_length,A0),D1	; length of jst_rel section
	CALL_MMUCodeABS	MarkBlockValid

	; if patch buffer is defined, mark as valid

	GETVAR_L	logpatch_buf,D0
	beq.b	.dodisks		; already allocated

	move.l	#LOGPATCH_SIZE,D1
	add.l	D0,D1
	CALL_MMUCodeABS	MarkBlockValid

.dodisks
	LEAVAR	diskbuffers,A0

.DiskLoop
	MOVE.L	(A0)+,D0
	BEQ.S	.DisksDone
	
	GETVAR_L	filesize,D1
	ADD.L	D0,D1
	CALL_MMUCodeABS	MarkBlockValid

	BRA.S	.DiskLoop
	
.DisksDone
	
	; mark files

	GETVAR_L	fileidbuffer,D0
	beq.b	.FilesDone
	MOVE.L	D0,D1
	ADDVAR_L	fileidlen,D1
	CALL_MMUCodeABS	MarkBlockValid

	; for each file

	GETVAR_L	fileidbuffer,A5
.MFLoop
	move.l	(A5)+,D0	; buffer - start of zone
	beq.b	.FilesDone
	move.l	(A5)+,D1	; length
	add.l	#FULLNAME_SIZE,D1	; + filename buffer
	add.l	D0,D1		; end of zone
	CALL_MMUCodeABS	MarkBlockValid
	bra.b	.MFLoop

.FilesDone

	GETVAR_L	extbuf,d0
	BEQ.S	.NoExtmem
	GETVAR_L	extsize,D1
	ADD.L	D0,D1
	CALL_MMUCodeABS	MarkBlockValid

.NoExtmem

	; ralf forgot to include this fix

	GETVAR_L	bit24buf,d0
	BEQ.S	.No24Bitmem
	GETVAR_L	bit24size,D1
	ADD.L	D0,D1
	CALL_MMUCodeABS	MarkBlockValid

.No24Bitmem
	MOVE.L	DebuggerStart,D0
	BEQ.S	.NoDebugger

	GETVAR_L	debuggerbase,D0
	MOVE.L	DebuggerSize,D1
	ADD.L	D0,D1
	CALL_MMUCodeABS	MarkBlockValid

.NoDebugger
	; mirror custom memory to a real memory zone
	; can be REALLY useful to check illegal dffxxx
	; pokes, or to track down copperlists...
	;
	; 10000x thanks Ralf for this !!!

	MOVE.L	CustomMirror,D0
	BEQ.S	.NoSnoopCustom
	MOVE.L	#SNOOP_BUFFER_SIZE,D1
	ADD.L	D0,D1
	CALL_MMUCodeABS	MarkBlockValid

	;TSTVAR_L	snoopcustom_flag
	;BEQ.S	.NoSnoopCustom

	;MOVE.L	#$DFF000,D0
	;MOVE.L	#$DFFFFF,D1

	;MOVE.L	CustomMirror,A0
	;CALL_MMUCodeABS	MirrorBlock
	;BRA.S	.CheckOSEmu

.NoSnoopCustom
	; mark custom registers zone as valid

	MOVE.L	#$DFF000,D0
	MOVE.L	#$DFFFFF,D1
	CALL_MMUCodeABS	MarkBlockValid
	
.CheckOSEmu
	GETVAR_L	osemu_ptr,D0
	BEQ.S	.NoOSEmu
	GETVAR_L	osemu_len,D1
	BEQ.S	.NoOSEmu

	ADD.L	D0,D1
	CALL_MMUCodeABS	MarkBlockValid

.NoOSEmu	

.No_MMUProtect
	RESTORE_REGS
	rts

SearchDebugger
	STORE_REGS

	SET_VAR_CONTEXT

	MOVE.L	$4.W,A6
	move.b	AttnFlags+1(A6),D0

	BTST	#AFB_68010,D0		; At least 68010
	BEQ	.NO_DEBUGGERCHECK

	MOVE.L	$4.W,A6
	
	LEA	GetVBRRoutine(PC),A5
	JSRLIB	Supervisor

	MOVE.L	SystemVBR(PC),D1
;;	BEQ	.CHECK_COP		; Jeff removed this check

	MOVE.L	D1,A0

	move.l	($7C,A0),A0
	move.l	A0,D0

	; Jeff removed loads of extra checks which did not work
	; on his system. Now everything is OK

	CMP.L	#"HRT!",-4(A0)
	BNE.S	.CHECK_COP

	MOVE.L	-8(A0),A0
	CMP.L	#"HRT!",4(A0)
	BNE.S	.CHECK_COP	
	
	SETVAR_L	A0,debuggerbase

	MOVE.L	20(A0),DebuggerSize
	MOVE.L	D0,DebuggerStart
	SETVAR_L	D0,debugger_nmi
	
	SETVAR_L	#HRTMon,debugger

	BRA.S	.DEBUGGER_FOUND

.CHECK_COP
	MOVE.L	4.W,A6
	
	JSRLIB	Forbid
	
	LEA	COPPortName(PC),A1
	JSRLIB	FindPort
	
	STORE_REGS	D0
	JSRLIB	Permit
	RESTORE_REGS	D0

	TST.L	D0
	BRA.S	.CHECK_THRILLKILL

	MOVE.L	SystemVBR(PC),A0
	SETVAR_L	$7C(A0),debugger_nmi

	SETVAR_L	#COP,debugger
	tst.l	verbose_flag

	BRA	.DEBUGGER_FOUND


.CHECK_THRILLKILL
.DEBUGGER_FOUND
.NO_DEBUGGERCHECK
.ExitSearch
	RESTORE_REGS
	RTS

GetVBRRoutine:
	STORE_REGS	D0/A0

	LEA	SystemVBR(PC),A0

	MC68010
	MOVEC	VBR,D0
	MC68000

	MOVE.L	D0,(A0)

	RESTORE_REGS	D0/A0
	RTE

SystemVBR
	DC.L	0
COPPortName
	DC.B	"COP",0
	CNOP	0,4

DebuggerSize
	DC.L	0
DebuggerStart
	DC.L	0

; *** Saves the chip image in a file

AbsFun_Priv_LogChipMirror:	; procedure start
	STORE_REGS
	VERBOSE_MSG	msg_Logging_chip_memory_

	SET_VAR_CONTEXT

	TSTVAR_L	saved_mem_size
	bne	.exit			; unavailable ATM

	; ** Open the file for writing

	move.l	#corename,D1
	move.l	#MODE_NEWFILE,D2
	move.l	_DosBase,A6
	bsr	OpenInUserDir
	move.l	D0,debughandle
	beq.b	.exit

	; ** Write the file

	move.l	debughandle(pc),D1
	GETVAR_L	chipmirror,D2
	GETVAR_L	maxchip,D3
	move.l	_DosBase,A6
	JSRLIB	Write

	; ** Close the file

	move.l	debughandle(pc),D1
	JSRLIB	Close
.exit
	RESTORE_REGS
	rts

; *** Saves the memory extension (if exists)

AbsFun_Priv_LogExtMemory:	; procedure start
	STORE_REGS
	SET_VAR_CONTEXT

	VERBOSE_MSG	msg_Logging_extension_me

	GETVAR_L	extsize,D3
	beq	.exit
	CMPVAR_L	#$200000,extbuf
	bcs	.exit			; extension in chipmem

	; ** Open the file for writing

	move.l	#extname,D1
	move.l	#MODE_NEWFILE,D2
	move.l	_DosBase,A6
	bsr	OpenInUserDir
	move.l	D0,debughandle
	beq.b	.exit

	; ** Write the file

	move.l	debughandle(pc),D1
	GETVAR_L	extbuf,D2
	GETVAR_L	extsize,D3
	move.l	_DosBase,A6
	JSRLIB	Write

	; ** Close the file

	move.l	debughandle(pc),D1
	JSRLIB	Close
.exit
	RESTORE_REGS
	rts

; *** Saves the custom mirror

AbsFun_Priv_LogCustomMirror:	; procedure start

	STORE_REGS

	tst.l	CustomMirror
	beq.b	.exit

	VERBOSE_MSG	msg_Logging_custom_memor

	; ** Open the file for writing

	move.l	#customname,D1
	move.l	#MODE_NEWFILE,D2
	move.l	_DosBase,A6
	bsr	OpenInUserDir
	move.l	D0,debughandle
	beq.b	.exit

	; ** Write the file

	move.l	debughandle(pc),D1
	move.l	CustomMirror,D2
	move.l	#SNOOP_BUFFER_SIZE/2,D3
	add.l	D3,D2			; Jeff: don't write first $1000 bytes, useless
	move.l	_DosBase,A6
	JSRLIB	Write

	; ** Close the file

	move.l	debughandle(pc),D1
	JSRLIB	Close
.exit
	RESTORE_REGS
	rts

; *** Switches in supervisor mode (dummy)

AbsFun_SupervisorMode:
	rts

AbsFun_Priv_SupervisorMode:
	bsr	TestSuperState
	bne.b	.exit			; if so, don't go in supervisor mode again


	STORE_REGS	D0/D1/A0/A1/A4/A6
	SET_VAR_CONTEXT
	move.l	_SysBase,A6
	JSRLIB	SuperState
	SETVAR_L	D0,system_superstack
	RESTORE_REGS	D0/D1/A0/A1/A4/A6

	; now we're in supervisor mode
.exit
	rts

; *** Switches in user mode

AbsFun_UserMode:
AbsFun_Priv_UserMode:

	bsr	TestSuperState
	beq.b	.userstate		; not in superstate now: out

	STORE_REGS	D0/D1/A0/A1/A4/A6
	SET_VAR_CONTEXT
	move.l	_SysBase,A6
	GETVAR_L	system_superstack,D0
	JSRLIB	UserState
	RESTORE_REGS	D0/D1/A0/A1/A4/A6
.userstate:
	rts

TestSuperState:
	STORE_REGS	D0/D1/A0/A1/A6
	move.l	_SysBase,A6
	moveq.l	#0,D0
	moveq.l	#0,D1			; read only
	JSRLIB	SetSR
	btst	#13,D0			; check if already in supervisor mode
	RESTORE_REGS	D0/D1/A0/A1/A6
	rts

; *** Load Disk from disk name
; in: D0: filename pointer

AbsFun_LoadDiskFromName:	; procedure start
	STORE_REGS
	SET_VAR_CONTEXT

	TSTVAR_L	lowmem_flag
	bne	LD_Exit			; load nothing if lowmem is on

	move.l	D0,D6

	LEAVAR	diskbuffers,A3
.zsearch
	tst.l	(A3)
	beq	.zfound
	addq.l	#4,A3
	bra	.zsearch
.zfound
	; *** we found a zero entry

	; *** allocate memory

	JSRABS	GetMemFlag				; reverse if possible
	GETVAR_L	filesize,D0
	move.l	_SysBase,A6
	JSRABS	AllocateTheMemory		; no alloc mem fix
	move.l	D0,(A3)
	bne.b	.ok
	jsr	MemErr
.ok


	; *** Open the file

	move.l	D6,D1
	move.l	#MODE_OLDFILE,D2	; Read Only
	move.l	_DosBase,A6
	JSRLIB	Open
	move.l	D6,A1			; in case of an error
	move.l	D0,fhandle
	beq	FileErr

	; ** Read the file

	move.l	fhandle(PC),D1
	GETVAR_L	filesize,D3
	move.l	(A3),D2
	move.l	_DosBase,A6
	JSRLIB	Read

	; ** Close the file

	move.l	fhandle(PC),D1
	JSRLIB	Close
	lea	fhandle(PC),A0
	clr.l	(A0)

	bra	LD_Exit


; *** Load Disks in memory
; in: D0: 0: starts from 1 for the filename
;         1:   ""    ""  2 and so on

AbsFun_LoadDisksIndex:	; procedure start
	STORE_REGS
	move.w	D0,diskindex
	and.w	#$FFFF,D0
	SETVAR_L	D0,diskbias
	bra	LD_Entry2

; *** Load Disks in memory

; Load the disk from the HD_PARAMS macro parameters in the user program
; no parameters are returned. The routine will exit with an error message
; if something fails (no memory, file not found...)
; else, it will return to the caller.

AbsFun_LoadDisks:	; procedure start
	VERBOSE_MSG	msg_Loading_disk_po_s_pf

	STORE_REGS
	clr.w	diskindex

	; ** Allocate memory for diskfiles
LD_Entry2
	SET_VAR_CONTEXT

	add.w	#'0'-1,diskindex
	TSTVAR_L	lowmem_flag
	bne	LD_Exit			; load nothing if lowmem is on

	LEAVAR	diskbuffers,A3
	moveq.l	#0,D4
	TSTVAR_L	hdload_flag
	bne.b	LD_Alloc		; only 1 allocation
	move.l	nbdisks,D4
	beq	LD_Exit			; nbdisks = 0: no read
	subq.l	#1,D4	; -1
LD_Alloc
	JSRABS	GetMemFlag		; reverse if possible
	GETVAR_L	filesize,D0
	move.l	_SysBase,A6
	JSRABS	AllocateTheMemory	; no alloc mem fix
	tst.l	D0
	bne.b	.ok
	jsr	MemErr
.ok
	; mark the disk buffer valid for MMU

	STORE_REGS	D0-D1
	move.L	D0,D1
	ADDVAR_L	filesize,D1
	CALL_MMUCodeABS	MarkBlockValid
	RESTORE_REGS	D0-D1

	move.l	D0,(A3)+
	tst.l	verbose_flag
	beq	.nov
	PRINT_MSG	msg_Disk_memory_allocate
	Mac_printh	D0

.nov
	dbf	D4,LD_Alloc

	move.l	nbdisks,D7

	moveq.l	#2,D5
	add.l	D5,D7

	LEAVAR	fname,A1		; store in A1 in case of error
	move.l	A1,D0
	jsr	RelFun_StrlenAsm
	tst.l	D0
	beq	FileErr			; no file name
	LEAVAR	fname,A0
	add.l	D0,A0
	move.l	A0,nname
	move.b	#'0',(A0)+
	clr.b	(A0)

	LEAVAR	diskbuffers,A3

.2
	; ** Calculate file name and open

	move.l	#MODE_OLDFILE,D2	; Read Only

	move.w	D5,D6
	add.w	diskindex(pc),D6
	move.l	nname(pc),A0
	move.b	D6,(A0)			; index for disk

	LEAVAR	fname,A1
	move.l	A1,D1
	move.l	_DosBase,A6
	JSRLIB	Open
	LEAVAR	fname,A1
	move.l	D0,fhandle
	beq	FileErr

	; ** Read the file

	move.l	D0,D1
	GETVAR_L	filesize,D3
	move.l	(A3)+,D2
	move.l	_DosBase,A6
	JSRLIB	Read

	; ** test if file is packed
	LEAVAR	fname,A1
	move.l	D2,A0
	move.l	(A0),D0
	cmp.l	#'XPKF',D0
	beq	PackErr
	
	; ** Close the file

	move.l	fhandle(PC),D1
	JSRLIB	Close
	clr.l	fhandle

	TSTVAR_L	hdload_flag
	bne	LD_Exit			; read only 1 disk if set

	addq.l	#1,D5
	cmp.b	D7,D5
	bcs	.2

LD_Exit:
	; sets path to "" (HDLOAD bug)

	lea	emptyname(pc),A0
	JSRABS	SetFilesPath

	RESTORE_REGS
	rts

MakeRawTable:
	STORE_REGS
	SET_VAR_CONTEXT
	move.l	_SysBase,A6
	moveq	#0,D0
	lea	.kbname(pc),A1
	; probably kick 2.0+ only
	JSRLIB	OpenLibrary
	tst.l	D0
	beq	.exit
	move.l	D0,A6

	JSRLIB	AskKeyMapDefault
	move.l	D0,A2

	LEAVAR	raw_ascii_table,A5

	; create a fake event

	lea	(-22,A7),A7
	move.l	A7,A3
	
	moveq.l	#10,D0
.clrloop
	clr.w	(A3)+
	dbf	D0,.clrloop

	move.l	A7,A3

	move.b	#IECLASS_RAWKEY,(ie_Class,A3)

	clr.w	(ie_Qualifier,A3)
	bsr	.convloop

	move.w	#IEQUALIFIER_LSHIFT,(ie_Qualifier,A3)
	bsr	.convloop

	move.w	#IEQUALIFIER_LALT,(ie_Qualifier,A3)
	bsr	.convloop

	move.w	#IEQUALIFIER_CONTROL,(ie_Qualifier,A3)
	bsr	.convloop

	lea	(22,A7),A7

	; close library

	move.l	A6,A1
	move.l	_SysBase,A6
	JSRLIB	CloseLibrary
	
.exit
	RESTORE_REGS
	RTS

; <A3: event
; <A5: buffer
; <A2: keymap

.convloop:
	moveq.l	#0,D2
	lea	-4(A7),A7
.loop
	move.l	A7,A1
	moveq.l	#4,D1
	move.l	A3,A0
	move.w	D2,(ie_Code,A0)
	JSRLIB	MapRawKey

	cmp.l	#1,D0
	beq.b	.onechar
	clr.l	(A7)
.onechar
	move.b	(A7),(A5)+	; only first char
	addq.l	#1,D2
	cmp.w	#$80,D2
	bne.b	.loop

	lea	4(A7),A7
	rts

.kbname:
	dc.b	"keymap.library",0
	even

AbsFun_LoadFiles:
	STORE_REGS
	moveq.l	#-1,D0
	jsr	LoadTheFiles
	RESTORE_REGS
	rts

; *** Load all the files in the specified directory which length <D1
;     D0: size limit

AbsFun_LoadSmallFiles:
	STORE_REGS
	SET_VAR_CONTEXT
	
	TSTVAR_L	lowmem_flag
	bne	.exit

	TSTVAR_L	hdload_flag
	bne	.ok
	moveq.l	#-1,D0			; read all the files
.ok
	jsr	LoadTheFiles

.exit
	RESTORE_REGS
	rts



; This function will exit with an error message if an error occurs
; else, it will return to the caller

LoadTheFiles:
	IFND	SKELETON_TEST
	VERBOSE_MSG	msg_Loading_files_p_p_p_
	SET_VAR_CONTEXT

	move.l	D0,maxfilesize	; if file > maxsize, don't read (HD load)

	; *** allocates the fileidbuffer, small at first
	
	CLRVAR_L	fileidlen
	move.l	#200,D0		; start size: ~20 items
	move.l	D0,fileidlen_alloc
	JSRABS	GetMemFlag
	VERBOSE_MSG	msg_allocating_memory
	JSRABS	AllocateTheMemory
	VERBOSE_MSG	msg_done
	SETVAR_L	D0,fileidbuffer
	bne.b	.ok
	jsr	MemErr
	bra.b	.exit
.ok
	; trick calculate the length of the prefix
	; this way it's possible to calculate the
	; relative path given the absolute path
	; by adding the prefix length to the absolute path

	LEAVAR_D	fpath,D0
	move.l	#temp_filename_buffer,D1
	jsr	RelFun_StrcpyAsm

	move.l	#temp_filename_buffer,D1
	move.l	#.fakefname,D2
	jsr	RelFun_AddPart

	move.l	#temp_filename_buffer,D0
	jsr	RelFun_StrlenAsm
	subq.l	#1,D0
	move.l	D0,prefix_length

	; call our precursive program
	; to scan all the directories below fpath

	LEAVAR	fpath,A0
	IFND	SKELETON_TEST
	jsr	recursive_loop
	ENDC
	
	
	; terminate buffer

	moveq.l	#0,D0
	moveq.l	#0,D1
	jsr	push_buffer_and_size

	; load all the files

	jsr	load_files

	; relocate filebuffer variables

.exit
	ENDC
	rts

.fakefname:	dc.b	"A",0
temp_filename_buffer:	
	BLKDECL	b,$100,0
	even
prefix_length:	dc.l	0
fileidlen_alloc:	dc.l	0

load_files:
	STORE_REGS
	SET_VAR_CONTEXT

	move.l	_DosBase,A6

	GETVAR_L	fileidbuffer,A5
.loop:
	move.l	(A5)+,D4	; buffer
	beq	.end
	move.l	(A5)+,D5	; size

	; directory: don't read anything
	tst.w	(A5)+
	bne	.loop
	
	move.l	D4,A0
	add.l	D5,A0		; filename

	; recompose the absolute filename

	LEAVAR_D	fpath,D0
	move.l	#temp_filename_buffer,D1
	jsr	RelFun_StrcpyAsm

	move.l	#temp_filename_buffer,D1
	move.l	A0,D2
	move.l	#FULLNAME_SIZE,D3	; max buffer size
	jsr	RelFun_AddPart

	VERBOSE_MSG	msg_Loading_file_

	tst.l	verbose_flag
	beq.b	.skip_loadaddr
	lea	temp_filename_buffer,A1
	JSRABS	Display
	Mac_print	" at "
	Mac_printx	D4
.skip_loadaddr:

	move.l	#temp_filename_buffer,D1
	move.l	#MODE_OLDFILE,D2
	JSRLIB	Open
	move.l	D0,D6
	beq	.cantopen
	tst.l	verbose_flag
	beq.b	.skip_open
	Mac_print	" [opened] "
.skip_open

	move.l	D6,D1
	move.l	D4,D2		; buffer
	move.l	D5,D3		; size
	JSRLIB	Read

	tst.l	verbose_flag
	beq.b	.skip_read
	Mac_print	" [read] "
.skip_read

	move.l	D6,D1
	JSRLIB	Close
	
	tst.l	verbose_flag
	beq.b	.skip_close
	Mac_printf	" [closed] "
.skip_close

	move.l	d2,a0
	move.l	(a0),d0
	lea		temp_filename_buffer,A1
	cmp.l	#'XPKF',D0
	beq	PackErr

	bra	.loop
.cantopen	
	PRINT_MSG	msg_Unable_to_open_,v
	move.l	A0,A1
	JSRABS	Display
	JSRABS	NewLine		
	JMPABS	CloseAll

.end
	RESTORE_REGS
	rts

; < D0: size
; < D1: buffer (contents+name)
; < D2: type: 0: file, 1: dir
	
push_buffer_and_size:
	STORE_REGS
	SET_VAR_CONTEXT

	GETVAR_L	fileidlen,D3	; current size
	move.l	D3,A2		; A2: current pointer

	add.l	#10,D3		; 10 bytes are going to be added
	cmp.l	fileidlen_alloc,D3
	bcs.b	.noresize

	; fileidlen_alloc <= D2: needs resize, add $100

	STORE_REGS

	move.l	fileidlen_alloc,D1	; old size
	add.l	#200,fileidlen_alloc
	move.l	fileidlen_alloc,D2	; new size
	GETVAR_L	fileidbuffer,D0
	JSRABS	ReAllocFastMemory
	tst.l	D0
	bne	.allocok		

	; not really probable, but...

	CLRVAR_L	fileidbuffer	; because we don't know where the problem occured
	clr.l	fileidlen_alloc		; never mind if some regions are not freed
	jsr	MemErr			; never returns

.allocok
	SETVAR_L	D0,fileidbuffer
	RESTORE_REGS
.noresize:
	ADDVAR_L	fileidbuffer,A2
	move.l	D1,(A2)+
	move.l	D0,(A2)+
	move.w	D2,(A2)		; store type: dir or file
	add.l	#10,(RelVar_fileidlen,A4)

	RESTORE_REGS
	rts

; < A1: current couple (buffer,size)

pop_buffer_ptr:
	SET_VAR_CONTEXT

	GETVAR_L	fileidbuffer,A1
	ADDVAR_L	fileidlen,A1
	sub.l	#10,A1
	rts

NAME_BUFFER_SIZE = $100

; < A0: dirname to scan

recursive_loop:

	
	STORE_REGS
	sub.l	#NAME_BUFFER_SIZE,A7
	move.l	A7,D4
	
	move.l	A0,A5		; dirname (relative)
	; allocates infoblock

	jsr	AllocInfoBlock
	move.l	D0,D6		; infoblock
	VERBOSE_MSG	msg_scanning_files

	; lock directory

	move.l	A5,D1
	jsr	lockdir
	move.l	D0,D5		; fileslock
	beq	.end

	move.l	D5,D1
	move.l	D6,D2
	jsr	examine
	tst.l	D0
	beq	.end	; no files

.loop:	
	; cleanup D4 buffer

	move.l	D4,A2
	clr.b	(A2)

	; examine next entry

	move.l	D5,D1
	move.l	D6,D2
	jsr	exnext	
	tst.l	D0
	beq	.end	; no more files

	move.l	D6,A2
	lea	fib_FileName(A2),A3

	; build an absolute directory name
	; to the allocated buffer in D4

	move.l	A5,D0
	move.l	A3,D1
	jsr	.absdirname

	tst.l	fib_DirEntryType(A2)
	bmi		.itsfile
	moveq.l	#1,d2	; directory
	moveq.l	#0,d0	; no size
	
	bsr	.store_entry
	
	; call the program with the calculated dir

	move.l	D4,A0
	jsr	recursive_loop

	bra	.loop

.itsfile:
	; regular file: get size

	move.l	fib_Size(A2),D0

	; compare to maxfilesize

	tst.l	maxfilesize
	bmi	.storelen		; no max size

	cmp.l	maxfilesize,D0
	bcc	.loop			; file too big -> skip

.storelen
	moveq.l	#0,d2	; file
	; allocates memory for the file + store entry
	bsr	.store_entry
	
	bra	.loop


.end:
	; unlock directory
	
	move.l	D5,D1
	jsr	unlockdir

	; frees info block

	move.l	D6,D0
	jsr	FreeInfoBlock

	; free name buffer
	add.l	#NAME_BUFFER_SIZE,A7
	RESTORE_REGS
	rts

.lockitself:
	PRINT_MSG	msg_Empty_name_found_in_
	JMPABS	CloseAll

	; > D0: file size or 0 (if empty file or dir), D2: 0 file 1 dir
	; store them in the vector buffer (fileidbuffer)
.store_entry
	STORE_REGS	D0		; save length
	add.l	#FULLNAME_SIZE,D0		; add space for the filename
	JSRABS	GetMemFlag
	;JSRABS	AllocateTheMemory
	move.l	_SysBase,A6
	JSRLIB	AllocMem
	move.l	D0,D1	; D1: buffer
	bne.b	.ok
	jsr	MemErr
.ok
	RESTORE_REGS	D0

	jsr	push_buffer_and_size

	; get the current pointer position in A1

	jsr	pop_buffer_ptr
	move.l	(A1),D1
	add.l	(4,A1),D1		; D1 -> name buffer
	move.l	D4,D0			; source
	add.l	prefix_length,D0	; make it relative by adding prefix length
	jsr	RelFun_StrcpyAsm	; gets the filename
	rts
	
; < D0: dirname (absolute path)
; < D1: filename (relative path)
; > (D4): result buffer

.absdirname:
	STORE_REGS

	MOVE.l	D1,D2	; filename

	; copy dirname into outbuffer
	move.l	D4,D1
	jsr	RelFun_StrcpyAsm

	move.l	D4,D1
	move.l	#FULLNAME_SIZE,D3
	jsr	RelFun_AddPart
	RESTORE_REGS
	rts


; < D1: 1/50 secs to wait

Delay
	STORE_REGS
	move.l	_DosBase,A6
	JSRLIB	Delay
	RESTORE_REGS
	rts


newline_and_quit
	JSRABS	NewLine
	jmp	AbsFun_CloseAll



ResumeErr:
	JSRABS	Enhance
	PRINT_MSG	msg_RESUME_not_available
	bra	newline_and_quit

MemErr:	
	PRINT_MSG	msg_Cant_Allocate_Mem
	SET_VAR_CONTEXT
	TSTVAR_L	resume_flag
	beq	.skip
	PRINT_MSG	msg_Or_abs_memory_requir

.skip
	bra	newline_and_quit

NofastErr:
	PRINT_MSG	msg_NOFAST_set_while_bot
	bra	newline_and_quit


AEMTwiceErr:
	PRINT_MSG	msg_AllocExtMem_po_pf_ca
	bra	newline_and_quit

A2MTwiceErr:
	PRINT_MSG	msg_Alloc_BitMem_po_pf_c
	bra	newline_and_quit


FastErr:	; procedure start
	PRINT_MSG	msg_Not_enough_memory_to
	bra	newline_and_quit

BaddirErr:	; procedure start
	PRINT_MSG	msg_Please_CD_to_the_cor
	bra	newline_and_quit


ReadArgsErr:	; procedure start
	SET_VAR_CONTEXT
	CLRVAR_L	quiet_flag		; force output, even if quiet is set
	jsr	OpenOutput		; opens output if not already done

	PUTS	_CliVersionMess
	PUTS	WrongArgsMess
	bra	newline_and_quit


UnexpectedErr:
	PRINT_MSG	msg_Unexpected_error_
	bra	newline_and_quit

TaskOverflowErr:
	PRINT_MSG	msg_StackOverflow_error
	bra	newline_and_quit

DirEmptyErr:
	PRINT_MSG	msg_The_directory_is_emp
	bra	newline_and_quit

LockErr:
	PRINT_MSG	msg_Can_t_lock_specified
	bra	newline_and_quit

NoIconErr:	; procedure start
	jsr	OpenOutput
	PRINT_MSG	msg_Can_t_open_icon_
	bra	newline_and_quit

disp_objerr:
	PRINT_MSG	msg_Object_
	PUTS	object_name
	rts

InvalidObjErr:
	jsr	disp_objerr
	PRINT_MSG	msg_has_got_an_invalid_s
	bra	newline_and_quit

NoMagicObjErr:
	jsr	disp_objerr
	PRINT_MSG	msg_is_executable_but_no
	PRINT_MSG	msg_Check_for_HD_PARAMS_
	bra	newline_and_quit

WHDVerErr:
	PRINT_MSG	msg_JOTDStartup_does_not
	bra	VerErr

LoaderVerErr
	PRINT_MSG	msg_Version_of_JOTDStart
VerErr:
	bra	newline_and_quit

MoreSectionsErr:
	jsr	disp_objerr
	PRINT_MSG	msg_is_made_of_more_than
	bra	newline_and_quit

NonRelocErr:
	move.l	D0,D6			; number of relocatable addresses
	jsr	disp_objerr
	PRINT_MSG	msg_is_not_relocatable_p
	Mac_printh	D6
	JSRABS	NewLine
	PRINT_MSG	msg_Please_check_addr_p_
	bra	newline_and_quit

ObjectErr:	; procedure start
	jsr	disp_objerr
	PRINT_MSG	msg_not_found_or_empty_
	bra	newline_and_quit

; *** File Error. NEEDS A1 <- filename

FileErr:	; procedure start
	PRINT_MSG	msg_Can_t_find_file_
	JSRABS	Display
	PRINT_MSG	msg_exclamation
	bra	newline_and_quit

PackErr:
	PRINT_MSG	msg_xpk_packed_not_supp
	JSRABS	Display
	PRINT_MSG	msg_exclamation
	bra	newline_and_quit

; resumes a saved game

;StartResumed:
;	jsr	ResumeFrozen_2		; load saved memory;

;	move.l	_DosBase,A6
;	move.l	#10,D1
;	JSRLIB	Delay			: waits 1/5 second

;	JSRABS	Priv_SupervisorMode
;	JSRABS	UserMode


;	bra	ReturnToGame

; AbsFun_Priv_InGameIconify (absolute call)

AbsFun_Priv_InGameIconify
	move.l	(A7),D0		; save return PC
	SET_VAR_CONTEXT

	SUBVAR_L	gene_patchbuffer,D0		; gets PC offset (InGameOSCall)
	SETVAR_L	D0,return_offset	; save it for later use

	JSRGEN	RestoreCustomNoDMA		; restore custom without the DMA
	JSRABS	Enhance				; restore system caches, display...

	jsr	ExecutePostScript	; run the post-script

	JSRABS	NewLine

	PRINT_MSG	msg_Loader_stopped_p_Typ
	JSRGEN	TellCopperPointer
	Mac_printh	D0,v
	PRINT_MSG	msg_pf_

CommandRetry:	; procedure start
	PRINT_MSG	msg_x_po_RETURN_pf_to_re

	tst.l	snapshot_forbidden	; gap in chipmem 2MB image
	bne.b	.skip
	PRINT_MSG	msg_s_po_RETURN_pf_to_sa

.skip
	SET_VAR_CONTEXT
	TSTVAR_L	resume_flag
	beq	.skip2
	PRINT_MSG	msg_l_po_RETURN_pf_to_re

.skip2

	PRINT_MSG	msg_q_po_RETURN_pf_to_qu
	JSRABS	WaitReturn

	move.b	charbuf,D0

	jsr	CheckCommand

	lea	FuncTable(pc),A0
	add.l	D1,D1
	add.l	D1,D1
	move.l	(A0,D1.L),A0

	jmp	(A0)

	; return to the game

ReturnCommand:	; procedure start
	jsr	ExecutePreScript	; run the pre-script again

	move.l	user_cacheflags,D0
	move.l	user_cachemask,D1
	JSRABS	Degrade			; degrade again

; count significant characters (in the non crypted string)

ReturnToGame:
	SET_VAR_CONTEXT

	GETVAR_L	return_offset,D0
	ADDVAR_L	gene_patchbuffer,D0	; computes return address from rel section start + offset
	move.l	D0,(A7)
	rts			; and return to end of InGameOSCall()

	cnop	0,4
FuncTable:	dc.l	InvalidCommand
	dc.l	QuitCommand
	dc.l	SaveCommand
	dc.l	ReturnCommand
	dc.l	ReloadCommand

QuitCommand:	; procedure start
	jmp	AbsFun_Priv_CloseAllQuiet

InvalidCommand:	; procedure start
	JSRABS	NewLine
	PRINT_MSG	msg_Invalid_command_p_Co
	bra	CommandRetry

CheckCommand:	; procedure start
	move.l	#INVALID_COMMAND,D1

	cmp.b	#'X',D0
	beq	.return
	cmp.b	#'x',D0
	beq	.return

	cmp.b	#'q',D0
	beq	.quit
	cmp.b	#'Q',D0
	beq	.quit
	cmp.b	#'s',D0
	beq	.save
	cmp.b	#'S',D0
	beq	.save
	cmp.b	#'l',D0
	beq	.load
	cmp.b	#'L',D0
	beq	.load

	rts

.return
	move.l	#RETURN_COMMAND,D1
	rts
.quit
	move.l	#QUIT_COMMAND,D1
	rts
.save
	move.l	#SAVE_COMMAND,D1
	rts
.load
	move.l	#RELOAD_COMMAND,D1
	rts

; Snapshot functions


	include	"jst_sna.asm"

; *** Loads and decrunches a RNC file from the Hard Drive,
; *** allocating memory at the same time (cool, isn't it)

; in:  D0: filename
; out: D0: error _flag (0 if OK)
;      A0: beginning of allocated buffer
;      D1: allocated length (useful if the file space must be freed with FreeMem())

AbsFun_LoadRNCFile
	STORE_REGS	D2-D7/A1-A6
	move.l	D0,rncname
	move.l	#MODE_OLDFILE,D2	; Read Only
	move.l	rncname,D1
	move.l	_DosBase,A6
	JSRLIB	Open
	move.l	D0,tmphandle
	beq	.notfound

	move.l	tmphandle,D1
	move.l	#minibuffer,D2
	move.l	#12,D3
	JSRLIB	Read

	move.l	tmphandle,D1
	JSRLIB	Close

	move.l	#minibuffer,A2
	cmp.l	#$524E4301,(A2)
	bne	.notrnc

	move.l	4(A2),declen	; decrunched len
	move.l	8(A2),crulen	; crunched len

	move.l	declen,D0
	moveq.l	#0,D1
	move.l	_SysBase,A6
	JSRABS	AllocateTheMemory		; no alloc mem fix
	move.l	D0,rncbuffer
	beq	.nomem

	move.l	#MODE_OLDFILE,D2	; Read Only
	move.l	rncname,D1
	move.l	_DosBase,A6
	JSRLIB	Open
	move.l	D0,tmphandle
	beq	.notfound

	move.l	tmphandle,D1
	move.l	rncbuffer,D2
	move.l	crulen,D3
	add.l	#$12,D3
	JSRLIB	Read

	move.l	tmphandle,D1
	JSRLIB	Close
	
	move.l	rncbuffer,A0
	move.l	rncbuffer,A1
	jsr	RNCDecrunch

	move.l	rncbuffer,A0
	moveq.l	#0,D0
	move.l	declen,D1
.exit
	RESTORE_REGS	D2-D7/A1-A6
	rts

.notfound
	moveq.l	#-1,D0
	bra	.exit

.notrnc
	moveq.l	#-2,D0
	bra	.exit

.nomem
	moveq.l	#-3,D0
	bra	.exit

; *** Some variables for loading and decrunching

minibuffer:
	BLKDECL	l,$10,0

tmphandle:	dc.l	0
declen:	dc.l	0
crulen:	dc.l	0
rncbuffer:	dc.l	0
rncname:	dc.l	0
	cnop	0,4

; *** Transfer the relocatable routines
; *** Obsolete now

AbsFun_TransfRoutines
	rts

RelocateRoutines:
	STORE_REGS
	SET_VAR_CONTEXT

	; ** first allocate some memory

	jsr	AllocObjectMem

	tst.l	D0
	bne.b	.ok
	jsr	MemErr
.ok:

	
	; *** then copy the routines in the reloc buffer

	lea	JstRelStart,A0

	; *** link variable space to relocatable section

	move.l	A4,(RelVar_variables_offset,A0)

	; *** relocate the relocated functions

	MOVE.L	D0,A1	; gene_patchbuffer, aligned

	move.l	(RelVar_jstrel_length,A0),D0
	lsr.l	#2,D0
	subq.l	#1,D0
.2
	move.l	(A0)+,(A1)+
	dbf	D0,.2
	
	; *** update reloc section addresses

	jsr	AbsFun_Priv_GetVBR				; supervisor call to get the VBR
	SETVAR_L	D0,system_vbr	; current system VBR
	
	; *** sets absolute functions used in relocatable section

	GETVAR_L	gene_patchbuffer,A0
	RELOCATE_ABSOFFSET_A0	InGameIconify
	RELOCATE_ABSOFFSET_A0	WHDReadDir
	RELOCATE_ABSOFFSET_A0	ReadDir
	RELOCATE_ABSOFFSET_A0	WriteTheFileHD
	RELOCATE_ABSOFFSET_A0	WriteFilePartHD
	RELOCATE_ABSOFFSET_A0	WriteUserFileHD
	RELOCATE_ABSOFFSET_A0	ReadTheFileHD
	RELOCATE_ABSOFFSET_A0	ReadFileHD
	RELOCATE_ABSOFFSET_A0	ReadUserFileHD
	RELOCATE_ABSOFFSET_A0	DeleteFileHD
	RELOCATE_ABSOFFSET_A0	DeleteUserFileHD
	RELOCATE_ABSOFFSET_A0	CloseAllWithError
	RELOCATE_ABSOFFSET_A0	LogChipMirror
	RELOCATE_ABSOFFSET_A0	LogCustomMirror
	RELOCATE_ABSOFFSET_A0	LogExtMemory
	RELOCATE_ABSOFFSET_A0	LogRegisters
	RELOCATE_ABSOFFSET_A0	DisableMMU
	RELOCATE_ABSOFFSET_A0	SetClockLoad
	RELOCATE_ABSOFFSET_A0	EnhanceGfx
	RELOCATE_ABSOFFSET_A0	EnhanceCpu
	RELOCATE_ABSOFFSET_A0	FreeTheMemory
	RELOCATE_ABSOFFSET_A0	CloseAllQuiet
	RELOCATE_ABSOFFSET_A0	Display
	RELOCATE_ABSOFFSET_A0	MarkNewEntryValid
	RELOCATE_ABSOFFSET_A0	UserMode
	RELOCATE_ABSOFFSET_A0	SupervisorMode

	; relocated function table offsets
	
	GETVAR_L	gene_patchbuffer,A0
	lea	(Rel_RelFunTable,A0),A1	; relocated functions table

	; convert the offsets to real addresses
	; by adding gene_patchbuffer to the offsets

.loop
	move.l	(A1),D0
	bmi.b	.exit		; ok, we met -1, end of list, quit
	add.l	A0,D0
	move.l	D0,(A1)+	; relocate function entries in relocated section
	bra.b	.loop
.exit

	; *** cache flush

	JSRABS	FlushCachesSys

	; don't forget the data is not relocated yet, since
	; the user code has not been loaded

	RESTORE_REGS
	rts

; allocates memory for relocated routines

AllocObjectMem:	; procedure start
	JSRABS	GetMemFlag				; reverse if possible

	STORE_REGS	A0
	lea	JstRelStart,A0
	move.l	(RelVar_jstrel_length,A0),D0	
	RESTORE_REGS	A0

	add.l	#SAFETY_MARGIN,D0				; Safety

	SETVAR_L	d0,gene_patchbuffer_size
	ALLOC_ABS_OR_DYN	gene_patchbuffer
	SETVAR_L	D0,gene_patchbuffer
	beq	MemErr

	RTS

; *** Blocks fastmem

; in: D0: # of bytes to leave unallocated (roughly)

; 0 in D0 means the routine will try to allocate all
; fast memory available.

; useful for some games which want chipmem but forget
; to specify it in AllocMem()
; do not use it if not necessary (impossible to return to the OS
; after that). You can use it when the TUDE NOFASTMEMORY BOOT=HIGHCHIP
; is necessary to boot the wanted game from floppy without problems
; TUDE is copyright N.O.M.A.D 1995. It can be found on aminet.

AbsFun_BlockFastMem:
	PRINT_MSG	msg_BlockFastMem_c_Don_t
	JMPABS	CloseAll

; *** Displays a message in the opened console or CLI

; in: A1: message pointer

; generally called from the Mac_printf macro (more convenient)

AbsFun_Display:
AbsFun_Priv_Display:
	STORE_REGS
	clr.l	d3

.aff_count:
	tst.b	(A1,D3)		
	beq.b	.aff_ok
	addq	#1,D3
	bra.b	.aff_count
.aff_ok
	move.l	A1,D2
	move.l	conhandle,D1
	beq	.1		; If no console, write nothing
	move.l	_DosBase,a6
	JSRLIB	Write
.1
	RESTORE_REGS
	rts

; *** Free files allocated in RAM by LoadFiles
; *** internal. No effect if no files were loaded.

FreeRamFiles:
	STORE_REGS
	SET_VAR_CONTEXT

	GETVAR_L	fileidbuffer,A5
	cmp.l	#0,A5
	beq	.fb

	GETVAR_L	fileidlen,D5
	beq	.fb
.loop
	move.l	(A5),D1	; buffer
	beq	.fb
	bmi	.skip		; -1 was put as pointer: invalidate
	
	tst.l	verbose_flag
	beq	.skipd
	Mac_print	"--> Freeing "
	move.l	(4,A5),d4
	move.l	d4,a1
	add.l	d1,a1
	JSRABS	Display
	Mac_print	" size "
	Mac_printx	D4
	Mac_print	" at "
	Mac_printh	D1
.skipd
	move.l	(4,A5),D0	; length
	add.l	#FULLNAME_SIZE,D0	; length of the name
	move.l	D1,A1
	JSRABS	FreeTheMemory
	clr.l	(A5)
.skip
	add.l	#10,A5
	bra	.loop

.fb
	; ** Frees the fileid buffer if any

	GETVAR_L	fileidbuffer,D0
	beq.b	.ff
	move.l	D0,A1
	move.l	fileidlen_alloc,D0	; allocated size, not used size
	JSRABS	FreeTheMemory
	CLRVAR_L	fileidbuffer
.ff
	RESTORE_REGS

	rts

; *** Free everything, and closes the window

AbsFun_CloseAllQuiet:
AbsFun_Priv_CloseAllQuiet:
	
	; *** the user specified a procedure to call
	; *** after the OS was restored (save hiscores, etc...)

	SET_VAR_CONTEXT
	TSTVAR_L	doonexit
	beq.b	.nothing

	STORE_REGS
	GETVAR_L	doonexit,A0
	JSR	(A0)
	RESTORE_REGS

.nothing
	move.l	verbose_flag,NoWaitReturn
	bra	DoClose

; *** Free everything, and displays an error message

AbsFun_Priv_CloseAllWithError:
	SET_VAR_CONTEXT

	GETVAR_L	message_ptr,A1
	JSRABS	Display

	; if OSEmu error, try to display the function name
	; to avoid to the coder to lookup in the RKRM

	TSTVAR_L	whd_sysoffset
	beq	.closeall

	LEAVAR	whd_sysunit,A1
.loop
	move.b	(A1)+,D1
	beq.b	.lexit
	cmp.b	#".",D1
	bne.b	.loop
	clr.b	-1(A1)
.lexit
	LEAVAR	whd_sysunit,A1
	GETVAR_L	whd_sysoffset,D0
	jsr	_GetFunctionName

	cmp.l	#0,A1
	beq.b	.checkdbg

	PRINT_MSG	msg_Function_c_
	JSRABS	Display
	JSRABS	NewLine

.checkdbg
	TSTVAR_L	debug_flag
	beq.b	.closeall
	PUTS	CoreMess
	JSRABS	NewLine
.closeall
	JSRABS	NewLine

	PRINT_MSG	msg_Maybe_try_with_NOCAC

	LEAVAR	lastfile_buffer,A1
	tst.b	(A1)
	beq.b	.nolastfile

	JSRABS	NewLine
	PRINT_MSG	msg_Last_file_loaded
	JSRABS	Display
	JSRABS	NewLine

.nolastfile
	GETVAR_W	last_interrupt,D0
	beq	.nointerrupt
	JSRABS	NewLine
	PRINT_MSG	msg_Last_interrupt_vector

	lea	-8(A7),A7
	clr.b	(5,A7)
	move.l	A7,A1
	JSRGEN	ShortHexToString
	JSRABS	Display
	lea	8(A7),A7

	JSRABS	NewLine
	JSRABS	NewLine

.nointerrupt
	jmp	AbsFun_CloseAll

; *** Free everything, and wait for RETURN

AbsFun_CloseAll:
	move.l	#-1,NoWaitReturn
DoClose:

	SET_VAR_CONTEXT

	jsr	ExecutePostScript

	VERBOSE_MSG	msg_Freeing_loader
	jsr	FreeObjMem
	VERBOSE_MSG	msg_Freeing_expansion_me
	jsr	FreeExtMem
	VERBOSE_MSG	msg_Freeing_bit_expansio
	jsr	Free24BitMem
	VERBOSE_MSG	msg_Freeing_game_files
	jsr	FreeRamFiles
	VERBOSE_MSG	msg_Freeing_logpatch_buf
	jsr	FreeLogPatch
	VERBOSE_MSG	msg_Freeing_MMU_code
	JSR	FreeMMUCode
	
	; ** returns to old directory (WB only)

	move.l	old_dirlock(pc),D1
	beq.b	.skipcd
	move.l	_DosBase,A6
	JSRLIB	CurrentDir

	; no need to unlock dirlock since the WB passed the lock
	; and is responsible for freeing it

	clr.l	dirlock

.skipcd

	; ** Free the diskfiles memory (RAM version)

	move.l	nbdisks,D4
	beq	.cw

	LEAVAR	diskbuffers,A3
	subq.l	#1,D4
.dmfree
	move.l	(A3)+,D0
	beq.b	.empty

	move.l	_SysBase,A6
	move.l	D0,A1
	GETVAR_L	filesize,D0
	beq.b	.cw
	JSRABS	FreeTheMemory
	clr.l	(-4,a3)
.empty
	dbf	D4,.dmfree

	; ** Closes the window after a return if any

.cw:
	move.l	conhandle,D1
	beq.b	.closeimm		; no conhandle
	StartFrom
	tst.l	D0
	beq.b	.closeimm		; from CLI, no need to wait for a key,
	tst.l	NoWaitReturn	; nor to close the window
	beq.b	.closeimm

	PRINT_MSG	msg_Hit_RETURN_to_exit

	JSRABS	WaitReturn
.closeimm
	SET_VAR_CONTEXT

	; free osemu memory if any

	GETVAR_L	osemu_ptr,A1
	GETVAR_L	osemu_len,D0
	JSRABS	FreeTheMemory

	CLRVAR_L	osemu_ptr

	GETVAR_L	gene_patchbuffer,D0
	beq.b	.cs5

	move.l	_SysBase,A6
	move.l	D0,A1
	GETVAR_L	gene_patchbuffer_size,d0

	JSRABS	FreeTheMemory
	CLRVAR_L	gene_patchbuffer

.cs5
	; free relocatable vars

	MOVE.L	gene_variables,D0
	beq.b	.cs7

	move.l	_SysBase,A6
	move.l	D0,A1
	move.l	#Rel_ENDVARS-Rel_STARTVARS,D0

	JSRABS	FreeTheMemory
	clr.l	gene_variables

.cs7
	;;Mac_printf	"Closing output & doslib"
	jsr	CloseOutput

	; ** Close DOS library

	move.l	_DosBase,D0
	beq.b	.cs8
	move.l	D0,A1
	move.l	_SysBase,A6
	JSRLIB	CloseLibrary
	clr.l	_DosBase

.cs8

	; ** restore stack and registers and exit
	
	move.l	oldstack,A7
	RESTORE_REGS
	moveq	#0,D0
	rts

; *** Writes the registers in a file called 'registers.dat'

AbsFun_Priv_LogRegisters:	; procedure start
	STORE_REGS
	VERBOSE_MSG	msg_Logging_registers_p_

	SET_VAR_CONTEXT

	move.l	_DosBase,A6
	move.l	#regname,D1
	move.l	#MODE_NEWFILE,D2
	bsr	OpenInUserDir
	move.l	D0,debughandle		; opens output file
	beq.b	.fileerr

	; calculates extmem offset

	GETVAR_L	game_pc,D1
	GETVAR_L	extbuf,D0
	sub.l	D0,D1
	SETVAR_L	D1,extbuf_offset

	; use exec library

	move.l	_SysBase,A6

	; write dump data to a text file

	LEAVAR	datastream,A1
	lea	msg_datadump_format,A0
	lea	.putchar(pc),A2
	JSRLIB	RawDoFmt

	move.l	debughandle,D1
	move.l	_DosBase,A6
	JSRLIB	Close			; closes the file
.fileerr
	RESTORE_REGS
	rts

.putchar:
	STORE_REGS
	move.b	D0,.outbuf
	move.l	_DosBase,A6
	move.l	debughandle,D1
	move.l	#.outbuf,D2
	moveq.l	#1,D3
	JSRLIB	Write
	RESTORE_REGS
	rts

.outbuf:
	dc.l	0
; < D1: filename
; < D2: open mode
; > D0: file handle

OpenInUserDir:
	STORE_REGS	D1-A6

	; copies directory name

	STORE_REGS	D1/A0
	lea	.temp_dir(pc),A0
	move.l	A0,D0
	move.l	#temp_filename_buffer,D1
	jsr	RelFun_StrcpyAsm
	RESTORE_REGS	D1/A0

	; cats dirname + filename

	STORE_REGS	D2
	move.l	D1,D2
	move.l	#temp_filename_buffer,D1
	move.l	#$100,D3
	jsr	RelFun_AddPart
	RESTORE_REGS	D2

	; opens the file in user directory

	move.l	_DosBase,A6
	JSRLIB	Open

	RESTORE_REGS	D1-A6
	rts
.temp_dir
	dc.b	"T:",0
	even
	
; *** Waits for the user to press
; *** the return key

AbsFun_WaitReturn:	; procedure start
	STORE_REGS
	move.l	_DosBase,A6
	move.l	conhandle(pc),D1
	JSRLIB	Flush

	clr.b	charbuf
	move.l	conhandle(pc),D1
	move.l	#charbuf,D2
	move.l	#40,D3		; size of the buffer, to flush it
	JSRLIB	Read
	RESTORE_REGS
	rts


; *** Return MEMF_REVERSE if kick 39+
; out: D1: 0 or MEMF_REVERSE

AbsFun_GetMemFlag:	; procedure start
	move.l	#MEMF_PUBLIC,D1
	STORE_REGS	D0
	moveq.l	#0,D1
	move.l	#38,D0
	JSRABS	KickVerTest
	tst.l	D0
	bne	.reversebroken
	or.l	#MEMF_REVERSE,D1		; Any mem, reverted, if KS 39 or higher
.reversebroken
	RESTORE_REGS	D0
	rts

; initialize patch zone (if PATCH_LOGGED is defined)

AbsFun_InitLogPatch:	; procedure start
	STORE_REGS
	SET_VAR_CONTEXT

	TSTVAR_L	logpatch_buf
	bne	.exit		; already allocated

	move.l	#LOGPATCH_SIZE,D0
	JSRABS	GetMemFlag
	or.l	#MEMF_CLEAR,D1
	move.l	_SysBase,A6
	JSRABS	AllocateTheMemory		; no alloc mem fix
	SETVAR_L	D0,logpatch_buf
	beq	.exit

	SETVAR_L	D0,logpatch_ptr

	PRINT_MSG	msg_Logged_patch_memory

.exit
	RESTORE_REGS
	rts

; save patch and deallocate patch zone (if PATCH_LOGGED is defined)
; private function

FreeLogPatch:	; procedure start
	STORE_REGS
	SET_VAR_CONTEXT

	TSTVAR_L	logpatch_buf
	beq	.exit		; not allocated

	; first save the data

	GETVAR_L	logpatch_buf,A1
	GETVAR_L	logpatch_ptr,D1	; need to get the relocated data
	sub.l	A1,D1	; D1: length

	beq	.exit	; empty log
	bmi	.exit	; can't happen, but...

	lea	patchlogname(pc),A0
	moveq	#0,D0
	JSRGEN	WriteUserFileHD

	GETVAR_L	logpatch_buf,A1
	move.l	A1,A5
	move.l	#LOGPATCH_SIZE,D0
	JSRABS	FreeTheMemory

	CLRVAR_L	logpatch_buf	; set to NULL
.exit
	RESTORE_REGS
	rts

; < A0 start of object local variables
; < A1 end of object local variables

AbsFun_SetLocalVarZone:	; procedure start
	STORE_REGS
	SET_VAR_CONTEXT

	; check for bounds

	cmp.l	A1,A0
	bcc	SLV_Error1	; start >= end
	GETVAR_L	object_ptr,D0

	cmp.l	D0,A0
	bcs	SLV_Error1	; below the object memory

				; no check for above object mem

	; check for already set

	tst.l	uservar_start
	bne	SLV_Error2	; already set

	move.l	A0,uservar_start
	move.l	A1,uservar_end
	
	RESTORE_REGS
	rts

SLV_Error1:	; procedure start
	PRINT_MSG	msg_SetLocalVarZone_c_in
	bra	newline_and_quit

SLV_Error2:	; procedure start
	PRINT_MSG	msg_SetLocalVarZone_c_bo
	bra	newline_and_quit

; *** encapsulated Absolute Allocation

AbsFun_AllocateAbsMemory
	move.l	_SysBase,A6
	JSRLIB	AllocAbs


	tst.l	verbose_flag
	beq.b	.exit

	STORE_REGS
	PRINT_MSG	msg_Absolute_memory_allo
	Mac_printh	D0
	JSRABS	NewLine
	RESTORE_REGS
.exit
	rts

; *** realloc emulation, only for fast memory
; *** used for resizeable arrays

; < D0: allocated block (can be zero)
; < D1: old size (can be zero)
; < D2: new required size

; > D0: newly allocated block, or 0 if it failed

AbsFun_ReAllocFastMemory:
	STORE_REGS	D1-A6

	tst.l	D0
	beq.b	.exit

	move.l	D0,A5	; source block
	move.l	D1,D5	; old size

	; allocate memory

	move.l	D2,D0	; new size
	JSRABS	GetMemFlag
	ori.l	#MEMF_CLEAR,D1
	JSRABS	AllocateTheMemory
	tst.l	D0
	beq.b	.exit

	move.l	D0,A4		; save new block pointer

	; allocation worked, move the memory

	move.l	A5,A0	; source block
	move.l	A4,A1	; dest block
	move.l	D5,D0	; old size
	jsr	RelFun_CopyMem

	; free the old block's memory

	move.l	A5,D0
	beq.b	.nomem	; there were no old memory
	move.l	D0,A1
	move.l	D5,D0	; idem
	beq.b	.nomem
	JSRABS	FreeTheMemory
.nomem
	; return new block's pointer

	move.l	A4,D0
.exit:
	RESTORE_REGS	D1-A6
	rts

; *** encapsulated AllocMem with MMU boundary alignment

; < D0: required size
; < D1: flags
; > D0: allocated block (0 if error)

AbsFun_AllocateTheMemory:
	STORE_REGS	D1-A6
	move.l	_SysBase,A6
	SET_VAR_CONTEXT
	cmp.l	#0,A4
	beq.b	.withmmu	; no context: alloc aligned
	TSTVAR_L	mmucode_ptr
	bne.b	.withmmu

	or.l	#MEMF_CLEAR,D1	; always clear memory to avoid problems

	JSRLIB	AllocMem	; *** no MMU: no page boundary alignment (A1200/nofast)
.exit:
	RESTORE_REGS	D1-A6
	rts

.withmmu:
	STORE_REGS	D2

	MOVE.L	#PAGE_SIZE_C,D2
	JSR	AllocMMUMem(PC)

	RESTORE_REGS	D2

	bra	.exit


AllocMMUMem
; IN:
;	D0 = Len
;	D1 = Memory flags
;	D2 = Alloc bound
; OUT:
;	D0 = *Memory

	STORE_REGS	D1-A6
	
	MOVE.L	D0,D7
	MOVE.L	D2,D6
	MOVE.L	D2,D5
	NEG.L	D5
	MOVE.L	D1,D4

	MOVE.L	_SysBase,A6
	JSRLIB	Disable

	MOVE.L	D7,D0
	ADD.L	D6,D0
	MOVE.L	D4,D1
	JSRLIB	AllocMem	; *** AllocMMUMem, aligned

	TST.L	D0
	BEQ.S	.EXIT
	
	STORE_REGS	D0

	MOVE.L	D0,A1
	MOVE.L	D7,D0
	ADD.L	D6,D0
	JSRLIB	FreeMem		; *** Free the memory (MMU)
	
	RESTORE_REGS	D0

	ADD.L	D6,D0
	AND.L	D5,D0
	MOVE.L	D0,A1
	MOVE.L	D7,D0

	JSRLIB	AllocAbs	; *** Allocate the right size, MMU page aligned!

	MOVE.L	D0,D6
	
	JSRLIB	Enable
	MOVE.L	D6,D0

	RESTORE_REGS	D1-A6
	RTS
	
.EXIT
	JSRLIB	Enable
	
	RESTORE_REGS	D1-A6
	MOVEQ	#0,D0
	RTS

; *** FreeMem but with size and location check to avoid
; *** to meet our old friend the guru
; *** also preserves all registers

AbsFun_FreeTheMemory:
AbsFun_Priv_FreeTheMemory:
	tst.l	D0
	beq.b	.exit		;safety
	cmp.l	#0,A1
	beq.b	.exit

	; could be a nice idea to fill memory with trash values
	STORE_REGS	D0-D1/A0-A1/A6
	move.l	_SysBase,A6
	JSRLIB	FreeMem
	RESTORE_REGS	D0-D1/A0-A1/A6
.exit
	rts

; ***** MMU disable

AbsFun_Priv_DisableMMU
	STORE_REGS	A4-A6
	SET_VAR_CONTEXT

	LEA	.DisableMMUCode(PC),A5
	MOVE.L	_SysBase,A6
	JSRLIB	Supervisor
	
	RESTORE_REGS	A4-A6
	RTS
	
.DisableMMUCode
	JMP_MMUCodeABS	DisableMMU
	RTE					; In case that no MMUCode is loaded

; *** Degrading standard routines

; those routines are used both by this program and by other degraders

	include	"jst_whdinit.asm"

	cnop	0,4

; buffer for the directory list

dirname_ptr:	dc.l	0
complete_ptr:	dc.l	0
chipmemgap_disabled:	dc.l	0
found_dir_entry:	dc.l	0

CustomMirror
	DC.L	0

customname
	DC.B	"custom_memory.dat",0
	cnop	0,4


	
	XREF	RelocErr
	XREF	MemErr
	XREF	LockErr

	XREF	_SysBase
	XREF	_DosBase
	XREF	gene_variables
	XREF	verbose_flag



; check for a assign existence
; < D0: assign name (with trailing :)
; > D0=0: exists else does not
; used to use LockDosList in v2.0 but it locked up so
; I replaced it by a simple dir test and it works without requesters

AbsFun_TestAssign
	bra	AbsFun_TestDirectoryAbs
	

; check for a dir existence
; < D0: dir name
; > D0=0: exists else does not

AbsFun_TestDirectory:
	STORE_REGS	D1-A6
	SET_VAR_CONTEXT
	LEAVAR	fpath,A0
	tst.b	(A0)
	beq	.nopath

	move.l	D0,D6	; save filename pointer

	move.l	A0,D0
	move.l	#tmpstring,D1
	jsr	RelFun_StrcpyAsm	; temp copy of the path

	move.l	#tmpstring,D1	; dir
	move.l	D6,D2		; file
	move.l	#256,D3		; length
	jsr	RelFun_AddPart

	move.l	#tmpstring,D0	; complete filename
.nopath

	JSRABS	TestDirectoryAbs

	RESTORE_REGS	D1-A6
	rts

; check for a dir existence
; < D0: dir name
; > D0=0: exists else does not

AbsFun_TestDirectoryAbs:
	STORE_REGS	D1/D6
	move.l	D0,D1
	jsr	nonfatal_lockdir
	move.l	D0,D6		; lock handle
	beq	.error

	move.l	D6,D1
	jsr	unlockdir	; unlock directory

.error
	moveq	#-1,D0
	tst.l	D6
	beq.b	.exit		; error
	moveq	#0,D0		; ok
.exit
	RESTORE_REGS	D1/D6
	rts

; *** User Callable routines

; *** Reboots the computer

AbsFun_Reboot
	move.l	_SysBase,A6
	JSRLIB	ColdReboot
	rts	; never reached

; *** check for fastmem
; out: D0= bytes free in fastmem

AbsFun_CheckFastMem
	STORE_REGS	D1-A6
	move.l	_SysBase,A6
	move.l	#MEMF_FAST|MEMF_LARGEST,D1
	JSRLIB	AvailMem
	RESTORE_REGS	D1-A6
	rts

; *** Test for 1MB chip memory
; out: D0=0 if 1MB chip (at least)

AbsFun_Test1MBChip:
	STORE_REGS	D1-A6
	move.l	_SysBase,A6

	MOVE.L	MaxLocMem(A6),D0
	SUB.L	#$100000,D0
	SMI	D0
	EXT	D0
	EXT.L	D0
	
	RESTORE_REGS	D1-A6
	RTS


; *** Test for 2MB chip memory
; out: D0=0 if 2MB chip (at least)

AbsFun_Test2MBChip:
	STORE_REGS	D1-A6
	move.l	_SysBase,A6

	MOVE.L	MaxLocMem(A6),D0
	SUB.L	#$200000,D0
	SMI	D0
	EXT	D0
	EXT.L	D0
	
	RESTORE_REGS	D1-A6
	RTS


; *** SetFilesPath changes the path to load files

; < A0: pointer on path

AbsFun_SetFilesPath:
	STORE_REGS
	SET_VAR_CONTEXT

	move.l	A0,D0
	LEAVAR	fpath,A0
	move.l	A0,D1

	jsr	RelFun_StrcpyAsm

	jsr	AddLoadDir	; add the LOADDIR part
	
	RESTORE_REGS
	rts

; *** SetGamePath concatenates data dir with current filename or directory
; < A0: pointer on short filename
; > A0: pointer on long filename

SetGamePath:
	STORE_REGS	D0-D1/A0-A1/A4
	SET_VAR_CONTEXT

	LEAVAR	fpath,A1
	tst.b	(A1)
	beq	.nogdir		; empty dirname

	; join the directory and the file name together

	move.l	A1,D0
	move.l	#tmpstring,D1
	jsr	RelFun_StrcpyAsm
	move.l	A0,D2		; short filename
	move.l	#256,D3
	jsr	RelFun_AddPart
	tst.l	D0

.nogdir
	RESTORE_REGS	D0-D1/A0-A1/A4
	beq	.skip

	lea	tmpstring(pc),A0	; change filename
.skip
	rts

AddLoadDir:
	STORE_REGS
	SET_VAR_CONTEXT

	LEAVAR	loaddata_dir,A0
	tst.b	(A0)
	beq	.noldir		; empty dirname

	; copy the loaddata path in another buffer

	lea	tmpstring(pc),A1
	move.l	A0,D0
	move.l	A1,D1
	jsr	RelFun_StrcpyAsm

	; then add the file path

	LEAVAR	fpath,A1
	tst.b	(A1)
	beq	.nogdir		; empty dirname

	; concatenate LOADDIR dir and the current load dir

	move.l	#tmpstring,D1
	move.l	A1,D2
	move.l	#256,D3
	jsr	RelFun_AddPart

.nogdir
	; now copy the concatenated path in the fpath buffer again

	move.l	#tmpstring,D0
	LEAVAR_D	fpath,D1
	jsr	RelFun_StrcpyAsm

.noldir

	tst.l	verbose_flag
	beq	.exit

	PRINT_MSG	msg_Load_disks_files_fro
	LEAVAR	fpath,A1
	JSRABS	Display
	JSRABS	NewLine
	JSRABS	NewLine
.exit
	RESTORE_REGS
	rts

ComputeDiskfileName:
	STORE_REGS
	SET_VAR_CONTEXT

	; compute the name of the diskfile

	LEAVAR	fpath,A0
	tst.b	(A0)
	beq	.nopath
	
	; add the path (LOADDIR/dir specified)

	move.l	A0,D0
	move.l	#tmpstring,D1
	jsr	RelFun_StrcpyAsm
	LEAVAR	fname,A1
	move.l	A1,D2
	move.l	#256,D3
	jsr	RelFun_AddPart

	; copy the complete name in the fname buffer

	move.l	#tmpstring,D0
	LEAVAR	fname,A1
	move.l	A1,D1
	JSRGEN	StrcpyAsm

.nopath
	RESTORE_REGS
	rts

; *** unlock a directory/file
; < D1: lock

unlockdir:
	STORE_REGS
	tst.l	verbose_flag
	beq.b	.nov2
	Mac_print	"-> Unlocking "
	Mac_printh	d1
.nov2
	move.l	_DosBase,A6
	tst.l	D1
	beq.b	.exit
	JSRLIB	UnLock		; unlock subdirectory
.exit
	RESTORE_REGS
	rts

; *** Lock a directory/file
; < D1: dir name
; > D0: lock

nonfatal_lockdir
	IFD	SKELETON_TEST
	moveq.l	#0,D0
	rts
	ENDC
	
	STORE_REGS	D1-A6

	tst.l	verbose_flag
	beq.b	.noprint

	Mac_print	"-> Locking '"
	move.l	D1,A1
	JSRABS	Display
.noprint

	move.l	#ACCESS_READ,D2
	move.l	_DosBase,A6
	JSRLIB	Lock

	tst.l	verbose_flag
	beq.b	.noprintr
	Mac_print	"' lockid="
	Mac_printh	D0
.noprintr


	RESTORE_REGS	D1-A6
	rts

; *** Lock a directory (quit if error)
; < D1: dir name
; > D0: lock

lockdir
	
	jsr	nonfatal_lockdir
	tst.l	D0
	bne.b	.ok
	; print error message & the directory that we tried to lock
	PRINT_MSG	msg_Can_t_lock_directory
	move.l	D1,A1
	JSRABS	Display
	bra	newline_and_quit
.ok
	rts


; *** examine a stat'd file

examine:
	STORE_REGS	D1-A6

	tst.l	verbose_flag
	beq.b	.nov
	Mac_print	"-> Examine "
	Mac_printx	D1
	Mac_printx	D2
.nov

	move.l	_DosBase,A6
	JSRLIB	Examine
	tst.l	verbose_flag
	beq.b	.nov2
	Mac_print	" => "
	Mac_printh	d0
.nov2
	RESTORE_REGS	D1-A6
	rts

; *** exnext a stat'd file

exnext:
	STORE_REGS	D1-A6
	tst.l	verbose_flag
	beq.b	.nov
	Mac_print	"-> ExNext "
.nov	move.l	_DosBase,A6
	JSRLIB	ExNext
	tst.l	verbose_flag
	beq.b	.nov2
	Mac_print	" => "
	Mac_printh	d0
.nov2
	RESTORE_REGS	D1-A6
	rts


; *** Gets the length (bytes) of a file on the hard drive
; in: D0: filename
; out: D0: length in bytes (-1 if not found!)

AbsFun_GetFileLength
	STORE_REGS	D1-A6

	SET_VAR_CONTEXT
	LEAVAR	fpath,A0
	tst.b	(A0)
	beq	.nopath

	move.l	D0,D6	; save filename pointer

	move.l	A0,D0
	move.l	#tmpstring,D1
	jsr	RelFun_StrcpyAsm	; temp copy of the path

	move.l	#tmpstring,D1	; dir
	move.l	D6,D2		; file
	move.l	#256,D3		; length
	jsr	RelFun_AddPart

	move.l	#tmpstring,D0	; complete filename

.nopath
	JSRABS	GetFileLengthAbs
	RESTORE_REGS	D1-A6
	rts

; *** Gets the length (bytes) of a file on the hard drive
; in: D0: filename
; out: D0: length in bytes (-1 if not found!)

AbsFun_GetFileLengthAbs
	STORE_REGS	D1-A6
	moveq.l	#-1,D6

	move.l	D0,D1
	move.l	D0,D1
	jsr	nonfatal_lockdir	; stat the file

	tst.l	D0
	beq.b	.end

	move.l	D0,D5		; D5=File Lock
	
	bsr	AllocInfoBlock
	move.l	D0,D7

	move.l	D5,D1
	move.l	D7,D2	; infoblock
	jsr	examine

	move.l	D7,A0
	move.l	fib_Size(A0),D6	; file size

	move.l	D5,D1
	jsr	unlockdir

	move.l	D7,D0
	bsr	FreeInfoBlock
.end
	move.l	D6,D0
	RESTORE_REGS	D1-A6
	rts

; *** Test if a file exists on the hard drive (gamedir path)
; in: D0 filename
; out:D0=0 if exists
;
; LOADDIR + filespath + filename
;

AbsFun_TestFile
	STORE_REGS	D1-A6
	SET_VAR_CONTEXT
	
	LEAVAR	fpath,A0
	tst.b	(A0)
	beq	.nopath

	move.l	D0,D6	; save filename pointer

	move.l	A0,D0
	move.l	#tmpstring,D1
	jsr	RelFun_StrcpyAsm	; temp copy of the path

	move.l	#tmpstring,D1	; dir
	move.l	D6,D2		; file
	move.l	#256,D3		; length
	jsr	RelFun_AddPart

	move.l	#tmpstring,D0	; complete filename

.nopath
	JSRABS	TestFileAbs	; test the file presence

	RESTORE_REGS	D1-A6
	rts


; *** Test if a file exists on the hard drive (absolute path)
; in: D0 filename
; out:D0=0 if exists

AbsFun_TestFileAbs:
	STORE_REGS	D1-A6
	move.l	D0,D1
	jsr	nonfatal_lockdir

	tst.l	D0
	beq.b	.err

	move.l	D0,D1
	jsr	unlockdir
	moveq.l	#0,D0
.exit:
	RESTORE_REGS	D1-A6
	rts

.err:
	moveq.l	#-1,D0
	bra.b	.exit

	; kickstart 1.3 compatible
AllocInfoBlock:
	STORE_REGS	D1/D2/A0/A1
	move.l	#fib_SIZEOF,D0
	move.l	#MEMF_PUBLIC|MEMF_CLEAR,d1
	JSRABS	AllocateTheMemory
	tst.l	D0
	beq	.error

	RESTORE_REGS	D1/D2/A0/A1
	rts

.error:
	PRINT_MSG	msg_Cannot_allocate_file
	jsr	MemErr
	rts

FreeInfoBlock
	STORE_REGS
	tst.l	D0
	beq.b	.exit	; safety
	move.l	D0,A1
	move.l	#fib_SIZEOF,D0
	JSRABS	FreeTheMemory
.exit
	RESTORE_REGS
	rts

	
	IFEQ	1		; kickstart v2.0+ version, not needed
AllocInfoBlock:
	STORE_REGS	D1/D2/A0/A1/A6
	move.l	#DOS_FIB,D1
	moveq.l	#0,D2
	move.l	_DosBase,A6
	JSRLIB	AllocDosObject
	tst.l	D0
	beq	.error
	RESTORE_REGS	D1/D2/A0/A1/A6
	rts

.error:
	PRINT_MSG	msg_Cannot_allocate_file
	jsr	MemErr
	rts

FreeInfoBlock
	STORE_REGS
	tst.l	D0
	beq.b	.exit	; safety
	move.l	D0,D2
	move.l	#DOS_FIB,D1
	move.l	_DosBase,A6
	JSRLIB	FreeDosObject
.exit
	RESTORE_REGS
	rts
	ENDC
	


tmpstring:	ds.b	256,0

msg_arrow:
	dc.b	"-> ",0
TaskName:
	dc.b	"JOTDStartup",0
	even

TaskCheck:
	IFD	USE_STACKCHECK
	dc.b	1
	ELSE
	dc.b	0
	ENDC
	dc.b	0,0,0
TaskPointer:
	dc.l	0
TaskArgsPtr:
	dc.l	0
TaskArgsLength:
	dc.l	0
RtnMess:
	dc.l	0
TaskLower:
	dc.l	0
ProcedureContext:	dc.l	0
gene_patchbuffer_abs
	dc.l	0
verbose_flag:
	dc.l	0
_SysBase:
	dc.l	0
_GfxBase:
	dc.l	0
_DosBase:
	dc.l	0
