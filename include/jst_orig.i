	IFND	JST_I_INCLUDED
JST_I_INCLUDED	=	1

	include	"gp_macros.i"

	IFND	REDEFINED_REGISTERS
REDEFINED_REGISTERS = 1
Ax EQUR A5
Ay EQUR A6
	ENDC

; wait for blitter operations to finish
; simple but effective

WAIT_BLIT:MACRO
.wait\@
	tst.b	$BFE001
	BTST	#6,dmaconr+$DFF000
	BNE.B	.wait\@
	ENDM

; save local variables

SET_VARZONE:MACRO
	IFNE	NARG-2
		FAIL	arguments "SET_VARZONE"
	ENDC
	STORE_REGS
	lea	\1(pc),A0
	lea	\2(pc),A1
	JSRABS	SetLocalVarZone
	RESTORE_REGS
	ENDM

; wait using Vertical beam delay (thanks to Harry for the routine)

BEAM_DELAY:MACRO
	IFNE	NARG-1
		FAIL	arguments "BEAM_DELAY"
	ENDC

	move.w  d0,-(a7)
	move.w	\1,D0
	beq.b	.exit\@		; don't wait

.loop1\@
	move.w  d0,-(a7)
        move.b	$dff006,d0	; VPOS

.loop2\@
	cmp.b	$dff006,d0
	beq.s	.loop2\@
	move.w	(a7)+,d0
	dbf	d0,.loop1\@

.exit\@
	move.w	(a7)+,d0
	ENDM

; jump to relocatable JST routine

JSRGEN:MACRO
	IFNE	NARG-1
		FAIL	arguments "JSRGEN"
	ENDC

	PEA	.END_JSRGEN\@(PC)

	MOVE.L	RelTable(PC),-(A7)
	ADD.L	#RelOff_\1,(A7)

	MOVE.L	A0,-(A7)
	MOVE.L	4(A7),A0
	MOVE.L	(A0),4(A7)

	MOVE.L	(A7)+,A0
	RTS

.END_JSRGEN\@
	ENDM

; jump to relocatable JST routine, with interrupts disabled

JSRGEN_FREEZE:MACRO
	IFNE	NARG-1
		FAIL	arguments "JSRGEN_FREEZE"
	ENDC
	move.w	SR,-(A7)
	move.w	#$2700,SR	; freeze, bastard
	JSRGEN	\1
	move.w	(A7)+,SR
	ENDM

; jump to absolute JST routines (only when OS is active!!)

JSRABS:MACRO
	IFNE	NARG-1
		FAIL	arguments "JSRABS"
	ENDC

	PEA	.END_JSRABS\@(PC)

	MOVE.L	AbsTable(PC),-(A7)
	ADD.L	#AbsOff_\1,(A7)

	MOVE.L	A0,-(A7)
	MOVE.L	4(A7),A0
	MOVE.L	(A0),4(A7)

	MOVE.L	(A7)+,A0
	RTS

.END_JSRABS\@
	ENDM

JMPABS:MACRO
	IFNE	NARG-1
		FAIL	arguments "JMPABS"
	ENDC

	MOVE.L	AbsTable(PC),-(A7)
	ADD.L	#AbsOff_\1,(A7)

	MOVE.L	A0,-(A7)
	MOVE.L	4(A7),A0
	MOVE.L	(A0),4(A7)

	MOVE.L	(A7)+,A0
	RTS

	ENDM

GO_SUPERVISOR:MACRO
	JSRABS	SupervisorMode
	ENDM

GO_USER:MACRO
	JSRABS	UserMode
	ENDM

SAVE_OSDATA:MACRO
	IFLT	NARG-1
		FAIL	arguments "SAVE_OSDATA"
	ENDC

	IFGE	NARG-2
		STORE_REGS	D0/A0
		
		MOVEQ	#0,D0
		MOVE.B	\2,D0
	
		IFEQ	NARG-3
			LEA	\3(PC),A0
		ELSE
			SUB.L	A0,A0
		ENDIF
		
		JSRGEN	SetQuitKey

		RESTORE_REGS	D0/A0
	ENDIF

;;	move.l	D0,-(sp)
	move.l	#\1,D0
	JSRABS	SaveOSData
;;	move.l	(sp)+,D0	; stack changes anyway!
				; Registers are zeroed too ;-) [Ralf]
	ENDM

; obsolete now, but can be useful to load a data register
; with a relative address

GETUSRADDR:MACRO
	IFNE	NARG-1
		FAIL	arguments "PATCHUSRADDR"
	ENDC
	move.l	Ax,-(sp)
	lea	\1(pc),Ax
	move.l	Ax,D0
	move.l	(sp)+,Ax
	ENDM

GETGENADDR:MACRO
	IFNE	NARG-1
		FAIL	arguments "GETGENADDR"
	ENDC
	move.l	Ax,-(sp)
	move.l	RelTable(pc),Ax
	add.l	#RelOff_\1,Ax
	move.l	(Ax),D0
	move.l	(sp)+,Ax
	ENDM

; puts a RTS and notifies

PATCH_RTS:MACRO
	IFNE	NARG-1
		FAIL	arguments "PATCH_RTS"
	ENDC

	REGISTER_PATCH	\1,#8	; 6 bytes ahead to allow to find the pattern
	move.w	#$4E75,\1
	ENDM

; puts a RTE and notifies

PATCH_RTE:MACRO
	IFNE	NARG-1
		FAIL	arguments "PATCH_RTE"
	ENDC

	REGISTER_PATCH	\1,#8	; 8 bytes ahead to allow to find the pattern
	move.w	#$4E73,\1
	ENDM

; puts a NOP and notifies

PATCH_NOP:MACRO
	IFNE	NARG-1
		FAIL	arguments "PATCH_NOP"
	ENDC

	REGISTER_PATCH	\1,#8	; 8 bytes ahead to allow to find the pattern
	move.w	#$4E71,\1
	ENDM

; puts 2 NOPs and notifies

PATCH_NOPNOP:MACRO
	IFNE	NARG-1
		FAIL	arguments "PATCH_NOPNOP"
	ENDC

	REGISTER_PATCH	\1,#8	; 8 bytes ahead to allow to find the pattern
	move.l	#$4E714E71,\1
	ENDM

; allows to notify JST about a patch
; if PATCH_LOGGED is defined

REGISTER_PATCH:MACRO
	IFNE	NARG-2
		FAIL	arguments "REGISTER_PATCH"
	ENDC

	IFD	PATCH_LOGGED
	STORE_REGS	D0-A0
	lea	\1,A0
	move.l	\2,D0
	JSRGEN	LogPatch
	RESTORE_REGS	D0-A0
	ENDC
	ENDM

; obsolete now

PATCHABSJMP:MACRO
	PATCHUSRJMP	\1,\2
	ENDM

; obsolete now

PATCHABSJSR:MACRO
	PATCHUSRJSR	\1,\2
	ENDM


PATCHUSRJMP:MACRO
	IFNE	NARG-2
		FAIL	arguments "PATCHUSRJMP"
	ENDC
	PATCHUSRXXX	\1,\2,$4EF9
	ENDM

PATCHUSRJSR:MACRO
	IFNE	NARG-2
		FAIL	arguments "PATCHUSRJSR"
	ENDC
	PATCHUSRXXX	\1,\2,$4EB9
	ENDM

PATCHGENJMP:MACRO
	IFNE	NARG-2
		FAIL	arguments "PATCHGENJMP"
	ENDC
	PATCHGENXXX	\1,\2,$4EF9
	ENDM

PATCHGENJSR:MACRO
	IFNE	NARG-2
		FAIL	arguments "PATCHGENJSR"
	ENDC
	PATCHGENXXX	\1,\2,$4EB9
	ENDM

PATCHUSRXXX:MACRO
	REGISTER_PATCH	\1,#6

	STORE_REGS	Ax/Ay
	lea	\2(pc),Ax
	lea	\1,Ay
	move.w	#\3,(Ay)+
	move.l	Ax,(Ay)
	RESTORE_REGS	Ax/Ay
	ENDM

PATCHGENXXX:MACRO
	REGISTER_PATCH	\1,#6

	STORE_REGS	Ax/Ay

	move.l	RelTable(pc),Ax
	add.l	#RelOff_\2,Ax
	move.l	(Ax),Ax
	lea	\1,Ay
	move.w	#\3,(Ay)+
	move.l	Ax,(Ay)

	RESTORE_REGS	Ax/Ay
	ENDM

TESTFILE:MACRO
	IFNE	NARG-1
		FAIL	arguments "TESTFILE"
	ENDC
	move.l	Ax,-(sp)
	lea	\1(pc),Ax
	move.l	Ax,D0
	JSRABS	TestFile
	move.l	(sp)+,Ax
	ENDM

HD_PARAMS:MACRO
	IFNE	NARG-3
		FAIL	arguments "HD_PARAMS"
	ENDC
HDP_Start:
	dc.l	$70004E75			; 00 if the user tries to start the loader
	dc.b	"JOTD"				; 04 magic number
	dc.l	CURRENT_VERSION_ID		; 09 version of JST identifier
AbsTable:
	dc.l	0				; 0C pointer on general absolute table
RelTable:
	dc.l	0				; 10 pointer on general relocate table
	dc.l	\2				; 14 diskfile size
	dc.l	\3				; 18 number of disks
	dc.l	HDP_End-HDP_Start		; 1C entrypoint offset
	dc.l	HDP_FnameStart-HDP_Start	; 20 offset of fname
_SysBase:
	dc.l	0				; 24 sysbase
_DosBase:
	dc.l	0				; 28 dosbase

HDP_FnameStart:
	IFD	MAXON_ASM
		DC.B	"\1",0
	ELSE
		dc.b	\1,0				; used for diskfiles
	ENDIF
	cnop	0,4
HDP_End:
	; start of the loader
	; initialize patch zone if PATCH_LOGGED set

	IFD	PATCH_LOGGED
	JSRABS	InitLogPatch
	ENDC
	ENDM

; if hardware not already defined, do it

	IFND	dmacon

	include	"hardware/custom.i"

	ENDC

; copied caches flag from execbase.i to avoid including
; system libs only for those ones

	IFND	CACRF_EnableI

CACRF_EnableI	EQU    (1<<0)  ; Enable instruction cache
CACRF_FreezeI	EQU    (1<<1)  ; Freeze instruction cache
CACRF_ClearI	EQU    (1<<3)  ; Clear instruction cache 
CACRF_IBE	EQU    (1<<4)  ; Instruction burst enable
CACRF_EnableD	EQU    (1<<8)  ; 68030 Enable data cache 
CACRF_FreezeD	EQU    (1<<9)  ; 68030 Freeze data cache 
CACRF_ClearD	EQU    (1<<11) ; 68030 Clear data cache	
CACRF_DBE	EQU    (1<<12) ; 68030 Data burst enable
CACRF_WriteAllocate EQU (1<<13) ; 68030 Write-Allocate mode
CACRF_EnableE	EQU    (1<<30) ; Master enable for external caches
			     ; External caches should track the
			     ; state of the internal caches
			     ; such that they do not cache anything
			     ; that the internal cache turned off; for.
CACRF_CopyBack	EQU    (1<<31) ; Master enable for copyback caches
	ENDC
	ENDC
