	IFND	JSTREL_MACROS_I
JSTREL_MACROS_I	EQU	1

; macros used in absolute location programs

	include	"gp_macros.i"
	include "jst_macros.i"
	include	"LVOs.i"
	include	"MMU.i"

SAVE_INTENA:MACRO
	move.w	intenar+$dff000,-(a7)
	ori.w	#$8000,(a7)		; so interrupts are restored with bit set
	ENDM
	
SAVE_INTENA_AND_FREEZE:MACRO
	SAVE_INTENA
	move.w	#$4000,intena+$dff000	; no more interrupts
	ENDM
	
RESTORE_INTENA:MACRO
	move.w	(a7)+,intena+$dff000
	ENDM
	
; jump to relocatable JST routine (identical as the one used in a slave)

JSRGEN:MACRO
	IFNE	NARG-1
		FAIL	arguments "JSRGEN"
	ENDC

	jsr	RelFun_\1(pc)

	ENDM

GETABSOFFSET:MACRO
	IFNE	NARG-2
		FAIL	arguments "GETABSOFFSET"
	ENDC

	; gets relocatable function address

	move.l	JstRelStart+Rel_\1(pc),\2

	ENDM

JSRABS:MACRO
	IFNE	NARG-1
		FAIL	arguments "JSRABS"
	ENDC

	PEA	.END_JSRABS\@(PC)

	GETABSOFFSET	\1,-(A7)

	; trick with the stack to push the address and JSR to it

;	MOVE.L	A0,-(A7)
;	MOVE.L	4(A7),A0
;	MOVE.L	(A0),4(A7)
;	MOVE.L	(A7)+,A0
	RTS

.END_JSRABS\@
	ENDM

JMPABS:MACRO
	IFNE	NARG-1
		FAIL	arguments "JMPABS"
	ENDC

	GETABSOFFSET	\1,-(A7)

;	MOVE.L	A0,-(A7)
;	MOVE.L	4(A7),A0
;	MOVE.L	(A0),4(A7)
;	MOVE.L	(A7)+,A0

	RTS
	ENDM


INGAMEOSCALL:MACRO
	move.l	A4,-(A7)
	GETABSOFFSET	\1,A4
	JSRGEN	InGameOSCall		; executed with the OS restored
	move.l	(A7)+,A4
	ENDM


WHDLOAD_UNSUPPORTED_CALL:MACRO
WUC_\1
	lea	WHDMessUnsupported_Arg(pc),A5
	move.l	A5,D1
	lea	RTMess\1(pc),A5
	move.l	A5,D0
	JSRGEN	StrcpyAsm
	lea	WHDMessUnsupported(pc),A5
	bra	RTStoreMessage
RTMess\1
	IFD	BARFLY
	dc.b	"\2"
	ELSE
		IFD	MAXON_ASM
		dc.b	"\2"
		ELSE
		dc.b	\2	; phxass
		ENDC
	ENDC

	dc.b	10,0
	even
	ENDM

RUNTIME_ERROR_ROUTINE:MACRO
RunTime_\1:
	lea	RTMess\1(pc),A5
	bra	RTStoreMessage
RTMess\1:
	dc.b	10,"Run-Time Error: "
	IFD	BARFLY
	dc.b	"\2"
	ELSE
		IFD	MAXON_ASM
		dc.b	"\2"
		ELSE
		dc.b	\2	; phxass
		ENDC
	ENDC

	dc.b	10,0
	even
	ENDM


PATCH_EXCEPT:MACRO
	PEA	Exception\1(pc)
	MOVE.L	(A7),$\1(A0)		;Patch into the new VBR too ;
	move.l	(A7)+,$\1.W
	ENDM

PATCH_INTVEC:MACRO
	PEA	IgnoreVector\1(pc)
	MOVE.L	(A7),$\1(A0)		;Patch into the new VBR too;
	move.l	(A7)+,$\1.W
	ENDM

; address definition for relative address table

DEF_ADDR_REL:MACRO
RelAdd_\1:
	dc.l	RelFun_\1-JstRelStart
	ENDM

; jumps to MMU code function
; To use this macro, the variables context must be correct, or else *BING*

JMP_MMUCode	MACRO
	TSTVAR_L	mmucode_ptr
	BEQ.S	.NO_MMUCode\@	
	GETVAR_L	mmucode_ptr,-(A7)
	ADD.L	#FunctionOffs_\1,(A7)
	RTS
.NO_MMUCode\@
	ENDM

; calls to MMU code
; To use this macro, the variables context must be correct, or else *BING*

CALL_MMUCode	MACRO
	TSTVAR_L	mmucode_ptr
	BEQ.S	.NO_MMUCode\@
	
	PEA	.NO_MMUCode\@(PC)
	GETVAR_L	mmucode_ptr,-(A7)
	ADD.L	#FunctionOffs_\1,(A7)
	RTS

.NO_MMUCode\@
	ENDM	


;; for last-chance debugging...

DEBUG150000:MACRO
	move.l	#$DEADBE\1,$150000
	ENDM


; A4 based variables macros

SET_VAR_CONTEXT:MACRO
	move.l	JstRelStart+RelVar_variables_offset(pc),A4
	ENDM

Mac_printhr:MACRO
	STORE_REGS	D0/A1
	move.l	\1,D0
	lea	.text\@(PC),A1
	JSRGEN	HexToString
	JSRABS	Display
	bra.b	.ftext\@

.text\@
	dc.b	"$00000000",0
	even
.ftext\@
	RESTORE_REGS	D0/A1
	ENDM

	ENDC

TRAP2SUPERVISOR:MACRO
	SET_VAR_CONTEXT

	; switches in supervisor mode if not already done

	GETVAR_L	current_vbr,A1
	MOVE.L	($80,A1),A0
	PEA	.Supervisor(PC)
	MOVE.L	(A7)+,($80,A1)
	TRAP	#0
.Supervisor
	MOVE.L	A0,($80,A1)
	ENDM
