	IFND	JST_MACROS_I
JST_MACROS_I	EQU	1


; macros used in absolute location programs

	include	"gp_macros.i"
	include	"jst_macros.i"
	include	"LVOs.i"
	include	"MMU.i"

; useful to track down enforcer hits by setting
; harmless ones around them to see between which
; and which one the nasty ones are occuring.

ENFORCER_HIT:MACRO
	tst.b	$100+\1		; harmless page zero read but 100% enforcer readhit guaranteed :)
	ENDM

; jump to relocatable JST routine (identical as the one used in a slave)

JSRGEN:MACRO
	IFNE	NARG-1
		FAIL	arguments "JSRGEN"
	ENDC

	PEA	.END_JSRGEN\@(PC)

	; gets relocatable function table base address

	pea	0.W
	STORE_REGS	A4
	SET_VAR_CONTEXT

	; gets relocatable function table base address

	GETVAR_L	gene_patchbuffer,4(A7)	; Barfly doesn't like ,(4,A7) !!
	RESTORE_REGS	A4

	tst.l	(A7)
	bne.b	.RelocOK\@
	jsr	RelFun_WaitMouse
	jmp	RelocErr
.RelocOK\@
	; adds function table offset + function offset

	ADD.L	#Rel_RelFunTable+RelOff_\1,(A7)

	MOVE.L	A0,-(A7)
	MOVE.L	4(A7),A0
	MOVE.L	(A0),4(A7)

	MOVE.L	(A7)+,A0
	RTS

.END_JSRGEN\@
	ENDM

JMPGEN:MACRO
	IFNE	NARG-1
		FAIL	arguments "JMPGEN"
	ENDC

	pea	0.W
	STORE_REGS	A4
	SET_VAR_CONTEXT

	; gets relocatable function table base address

	GETVAR_L	gene_patchbuffer,4(A7)
	RESTORE_REGS	A4

	tst.l	(A7)
	bne.b	.RelocOK\@
	jsr	RelFun_WaitMouse
	jmp	RelocErr
.RelocOK\@
	; adds function table offset + function offset

	ADD.L	#Rel_RelFunTable+RelOff_\1,(A7)

	MOVE.L	A0,-(A7)
	MOVE.L	4(A7),A0
	MOVE.L	(A0),4(A7)

	MOVE.L	(A7)+,A0
	RTS
	ENDM

JSRABS:MACRO
	jsr	AbsFun_\1
	ENDM

JMPABS:MACRO
	jmp	AbsFun_\1
	ENDM

ALIGN_ON_MMU:MACRO			; added by Jeff
;;	ADD.L	#PAGE_SIZE_C-1,D0	; Align memory to MMU page size
;;	AND.L	#~(PAGE_SIZE_C-1),D0
	ENDM

;GETRELADDR:MACRO
;	move.l	gene_patchbuffer(pc),D0
;
;	bne.b	.RelocOK\@
;	jsr	RelFun_WaitMouse
;	jmp	RelocErr
;.RelocOK\@
;	add.l	#\1,D0
;	sub.l	#JstRelStart,D0
;	ENDM

;GETRELLONG:MACRO
;	move.l	A0,-(sp)
;	GETRELADDR	\1
;	move.l	D0,A0
;	move.l	(A0),D0
;	move.l	(sp)+,A0
;	ENDM


PRINT_MSG:MACRO
	STORE_REGS	A1
	lea	\1,A1
	JSRABS	Display
	RESTORE_REGS	A1
	ENDM

VERBOSE_MSG:MACRO
	tst.l	verbose_flag
	beq.b	.skip\@
	PRINT_MSG	msg_arrow
	PRINT_MSG	\1
.skip\@
	ENDM


ALLOC_ABS_OR_DYN:MACRO
	SET_VAR_CONTEXT
	tst.l	\1_abs
	beq.b	.dynalloc\@
	move.l	\1_abs,A1

	JSRABS	AllocateAbsMemory
	bra	.endalloc\@
.dynalloc\@
	TSTVAR_L	nofast_flag
	beq	.doalloc\@
	or.l	#MEMF_CHIP,D1
.doalloc\@
	JSRABS	AllocateTheMemory
.endalloc\@
	ENDM


; address definition for absolute address table

DEF_ADDR_ABS:MACRO
AbsAdd_\1:
	dc.l	AbsFun_\1
	ENDM

RELOCATE_ABSOFFSET_A0:MACRO
	move.l	#AbsFun_Priv_\1,Rel_\1(A0)
	ENDM

;; for last-chance debugging...

DEBUG150000:MACRO
	move.l	#$DEADBE\1,$150000
	ENDM

; MMUCode interface

CALL_MMUCodeABS	MACRO
	TSTVAR_L	mmucode_ptr
	BEQ.S	.NO_MMUCode\@
	
	PEA	.Continue\@(PC)
	GETVAR_L	mmucode_ptr,-(A7)
	ADD.L	#FunctionOffs_\1,(A7)
	RTS

.NO_MMUCode\@

.Continue\@
	ENDM	


JMP_MMUCodeABS	MACRO
	SET_VAR_CONTEXT
	TSTVAR_L	mmucode_ptr
	BEQ.S	.NO_MMUCode\@
	
	GETVAR_L	mmucode_ptr,-(A7)
	ADD.L	#FunctionOffs_\1,(A7)
	RTS

.NO_MMUCode\@

.Continue\@
	ENDM	

SET_VAR_CONTEXT:MACRO
	move.l	gene_variables,A4
	ENDM

; enums for cache state

FIC_NO = 0
FIC_CLEAN = 1	; file is OK
FIC_DIRTY = 2	; file must be reloaded
FIC_SHADOW = 3	; only size information is available

; do not change the order or the value of the following!!

INVALID_COMMAND = 0
QUIT_COMMAND = 1
SAVE_COMMAND = 2
RETURN_COMMAND = 3
RELOAD_COMMAND = 4

	ENDC
