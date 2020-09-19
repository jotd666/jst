	IFND	JST_I_INCLUDED
JST_I_INCLUDED	=	1

		IFD	BARFLY
;;;		BOPT	O+ OG+			;enable optimizing
		BOPT	ODd- ODe-		;disable mul optimizing
		BOPT	w4-			;disable 64k warnings
		BOPT	wo-			;disable optimizer warnings
		SUPER				;disable supervisor warnings
		ENDC


	include	"gp_macros.i"

	IFD	WHDLOADSLAVE
	include	"whdload.i"
	include	"whdmacros.i"
	ENDC

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

	IFD	WHDLOADSLAVE
WRONG_VERSION:MACRO
	pea	TDREASON_WRONGVER
	move.l	_resload(pc),-(a7)
	addq.l	#resload_Abort,(a7)
	rts
	ENDM

FILE_NOT_FOUND:MACRO
	pea	\1(PC)
	pea	205
	pea	TDREASON_DOSREAD
	move.l	(_resload,pc),-(a7)
	add.l	#resload_Abort,(a7)
	rts
	ENDM

	ENDC

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

	IFD	WHDLOADSLAVE
;-------------------------------------
	IFC	\1,'InGameExit'
	pea	TDREASON_OK
	move.l	(_resload,pc),-(a7)
	add.l	#resload_Abort,(a7)
	rts
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'FlushCachesHard'
	move.l	a0,-(A7)
	move.l	_resload(PC),a0
	jsr	resload_FlushCache(A0)	;preserves all registers
	move.l	(A7)+,a0
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'GetUserFlags'
	move.l	D1,-(A7)
	moveq.l	#0,D0
	move.l	_whdi_custom4(pc),D1
	tst.l	D1
	beq.b	.skip1\@
	bset	#AFB_JOYPAD,D0
.skip1\@
	move.l	_whdi_custom5(pc),D1
	tst.l	D1
	beq.b	.skip2\@
	bset	#AFB_TRAINER,D0
.skip2\@
	move.l	(A7)+,D1
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'PatchExceptions'
				;ignored atm
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'StoreCopperPointer'
				;ignored atm
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'TellCopperPointer'
				;ignored atm
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'WaitBlit'
	TST.B	dmaconr+_custom
	BTST	#6,dmaconr+_custom
	BNE.S	.wait\@
	bra.s	.end\@
.wait\@
	TST.B	$BFE001
	TST.B	$BFE001
	BTST	#6,dmaconr+_custom
	BNE.S	.wait\@
	TST.B	dmaconr+_custom
.end\@
	MEXIT
	ENDC
;-------------------------------------
	IFC	\1,'SkipColon'
	movem.l	A2,-(A7)
	move.l	A0,A2

.loop\@
	cmp.b	#0,(A2)
	beq.b	.nocolon\@	; no colon: unchanged

	cmp.b	#':',(A2)
	beq.b	.colon\@
	addq.l	#1,A2
	bra.b	.loop\@
	
.colon\@
	move.l	A2,A0
	addq.l	#1,A0
.nocolon\@
	movem.l	(A7)+,A2
.exit\@
	MEXIT
	ENDC
;-------------------------------------
	IFC	\1,'RNCDecrunch'
	movem.l	D0-D1/A0-A2,-(A7)
	MOVE.L	_resload(PC),A2
	JSR	(resload_Decrunch,a2)
	movem.l	(A7)+,D0-D1/A0-A2
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'CopyMem'
	movem.l	D0-A6,-(A7)
	cmp.l	A0,A1
	beq.b	.exit\@		; same regions: out
	bcs.b	.copyfwd\@	; A1 < A0: copy from start

	tst.l	D0
	beq.b	.exit\@		; length 0: out

	; here A0 > A1, copy from end

	add.l	D0,A0		; adds length to A0
	cmp.l	A0,A1
	bcc.b	.cancopyfwd\@	; A0+D0<=A1: can copy forward (optimized)
	add.l	D0,A1		; adds length to A1 too

.copybwd\@:
	move.b	-(A0),-(A1)
	subq.l	#1,D0
	bne.b	.copybwd\@

.exit\@
	movem.l	(A7)+,D0-A6
	bra	.end\@
	
.cancopyfwd\@:
	sub.l	D0,A0		; restores A0 from A0+D0 operation
.copyfwd\@:
	move.l	A0,D1
	btst	#0,D1
	bne.b	.fwdbytecopy\@	; src odd: byte copy
	move.l	A1,D1
	btst	#0,D1
	bne.b	.fwdbytecopy\@	; dest odd: byte copy

	move.l	D0,D2
	lsr.l	#4,D2		; divides by 16
	move.l	D2,D3
	beq.b	.fwdbytecopy\@	; < 16: byte copy

.fwd4longcopy\@
	move.l	(A0)+,(A1)+
	move.l	(A0)+,(A1)+
	move.l	(A0)+,(A1)+
	move.l	(A0)+,(A1)+
	subq.l	#1,D2
	bne.b	.fwd4longcopy\@

	lsl.l	#4,D3		; #of bytes*16 again
	sub.l	D3,D0		; remainder of 16 division

.fwdbytecopy\@:
	tst.l	D0
	beq.b	.exit\@
.fwdbytecopy_loop\@:
	move.b	(A0)+,(A1)+
	subq.l	#1,D0
	bne.b	.fwdbytecopy_loop\@
	bra.b	.exit\@
.end\@:
	MEXIT
	ENDC
;-------------------------------------
	IFC	\1,'StrcpyAsm'
	movem.l	D0-D1/A0-A1,-(A7)

	move.l	D0,A0
	move.l	D1,A1
.copy\@
	move.b	(A0)+,(A1)+
	bne.b	.copy\@

	; terminates if end reached

	clr.b	(A1)
.exit\@
	movem.l	(A7)+,D0-D1/A0-A1
	MEXIT
	ENDC
;-------------------------------------
; not supported yet!
	IFC	\1,'EnterDebugger'
	ILLEGAL
	MEXIT
	ENDC

;-------------------------------------
; not supported yet!
;	IFC	\1,'FungusDecrunch'
;	movem.l	A2,-(A7)
;	MOVE.L	_resload(PC),A2
;	JSR	(resload_Decrunch,a2)
;	movem.l	(A7)+,A2
;	MEXIT
;	ENDC	
;-------------------------------------
	IFC	\1,'ATNDecrunch'
	movem.l	D1/A0-A2,-(A7)
	MOVE.L	_resload(PC),A2
	JSR	(resload_Decrunch,a2)
	movem.l	(A7)+,D1/A0-A2
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'ImploderDecrunch'
	movem.l	D0/A0-A2,-(A7)
	move.l	A0,A1
	MOVE.L	_resload(PC),A2
	JSR	(resload_Decrunch,a2)
	movem.l	(A7)+,D0/A0-A2
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'CRC16'
	movem.l	D1/A0-A2,-(A7)
	MOVE.L	_resload(PC),A2
	JSR	(resload_CRC16,a2)
	movem.l	(A7)+,D1/A0-A2
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'InGameIconify'
				;ignored atm
	MEXIT
	ENDC
;-------------------------------------
	IFC	\1,'BlackScreen'
	movem.l	D0/A0,-(A7)
	lea	_custom+color,A0
	move.l	#31,D0
.cloop\@
	move.w	#0,(A0)+
	dbf	D0,.cloop\@
	movem.l	(A7)+,D0/A0
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'FreezeAll'
	move.w	#$7fff,$dff09a
	move.w	#$7fff,$dff09c
	move.w	#$7fff,$dff096
	move.w	#$7fff,$dff09e
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'GoECS'
;;	move.w	#$0c20,$dff106
;;	move.w	#0,$dff1fc		; ignored ATM like in JST
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'BeamDelay'
.bd_loop1\@
	move.w  d0,-(a7)
        move.b	$dff006,d0	; VPOS
.bd_loop2\@
	cmp.b	$dff006,d0
	beq.s	.bd_loop2\@
	move.w	(a7)+,d0
	dbf	d0,.bd_loop1\@
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'WaitMouse'
.waitmousem_\@
	move.w	$dff006,$dff180
	btst	#6,$bfe001
	bne.s	.waitmousem_\@
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'WaitReturn'
.waitmousem_\@
	move.w	$dff006,$dff180
	btst	#6,$bfe001
	bne.s	.waitmousem_\@
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'SetDisk'
				;ignored atm
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'GetAttnFlags'
	move.l	_whdi_attnflags(pc),D0
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'InitTrackDisk'

	IFND	WHDM_INITTRACKDISK_CALLED
WHDM_INITTRACKDISK_CALLED EQU 1
	bra.s	.whdm_itd1\@
.whdm_id_table\@
WHDM_INITTRACKDISK_TABLE EQU .whdm_id_table\@
	DC.L	0,0,0,0,0,0,0,0,0,0,0,0
.whdm_itd1\@
	ENDC

	lea.l	WHDM_INITTRACKDISK_TABLE(PC),A1
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'TrackLoad'
	movem.l	d0-d1/a0-a2,-(A7)
	cmp.w	#2,$1c(A1)
	beq.s	.whdm_trackload1\@
	cmp.w	#9,$1c(A1)
	beq.s	.whdm_okay\@
	illegal				; unsupported
.whdm_trackload1\@
	move.l	$24(A1),d0
	move.l	$2c(A1),d1
	move.l	$28(A1),a1
	lea.l	HDP_FnameDiskNo(PC),a0
	move.b	#$31,(A0)		;only disk 0 atm
	lea.l	HDP_FnameStart(PC),a0
	MOVE.L	_resload(PC),A2
	JSR	(resload_LoadFileOffset,a2)
	tst.l	D0
	bmi.b	.whdm_okay\@
	movem.l	(A7)+,d0-d1/a0-a2
	pea	HDP_FnameStart(PC)
	pea	205
	pea	TDREASON_DOSREAD
	move.l	(_resload,pc),-(a7)
	add.l	#resload_Abort,(a7)
	rts
.whdm_okay\@
	movem.l	(A7)+,d0-d1/a0-a2
	move.b	#0,$1f(A1)		;no error
	moveq	#0,D0			;no error
	MEXIT
	ENDC	

;-------------------------------------
	IFC	\1,'RNCLength'
	moveq.l	#-1,D0
	CMPI.L         #$524E4301,(A0)	; RNC\01 tag.
	beq	.ok\@			; not a rnc01 file
	CMPI.L         #$524E4302,(A0)	; RNC\02 tag.
	bne	.exit\@			; not a rnc file
.ok\@
	move.l	4(A0),D0
.exit\@
	MEXIT
	ENDC
;-------------------------------------
	IFC	\1,'HexReplaceLong'
	movem.l	A0-A1/D0-D1,-(A7)
.srch\@
	cmp.l	(A0),D0
	beq.b	.found\@
.next\@
	addq.l	#2,A0
	cmp.l	A1,A0
	bcc.b	.exit\@
	bra.b	.srch\@
.found\@
	move.l	D1,(A0)+
	bra	.next\@
.exit\@
	movem.l	(A7)+,A0-A1/D0-D1
	MEXIT
	ENDC
;-------------------------------------
	IFC	\1,'HexSearch'
	movem.l	D1/D3/A1-A2,-(A7)
.addrloop\@:
	moveq.l	#0,D3
.strloop\@:
	move.b	(A0,D3.L),D1	; gets byte
	cmp.b	(A2,D3.L),D1	; compares it to the user string
	bne.b	.notok\@		; nope
	addq.l	#1,D3
	cmp.l	D0,D3
	bcs.b	.strloop\@

	; pattern was entirely found!

	bra.b	.exit\@
.notok\@:
	addq.l	#1,A0	; next byte please
	cmp.l	A0,A1
	bcc.b	.addrloop\@	; end?
	sub.l	A0,A0
.exit\@:
	movem.l	(A7)+,D1/D3/A1-A2
	MEXIT
	ENDC
;-------------------------------------
	IFC	\1,'ReadRobSectors'
	movem.l	d1-d2/a0-a2,-(A7)
;	tst.w	d3
;	beq.s	.whdm_rrs_load1\@
;	cmp.w	#$8000,d3
;	beq.s	.whdm_rrs_load1\@
;	illegal				;doesnt save yet
;.whdm_rrs_load1\@
	swap	D1
	clr.w	D1
	swap	D1
	swap	D2
	clr.w	D2
	swap	D2
	tst.w	D2
	beq.b	.exit\@		; length=0: out

	move.l	a0,a1			;address
	lea.l	HDP_FnameDiskNo(PC),a0
	add.b	#$31,d0			;only up to 9 disks
	move.b	d0,(A0)
	lea.l	HDP_FnameStart(PC),a0

	move.l	D2,D0			;len to read
	ext.l	d0
	lsl.l	#7,d0
	lsl.l	#2,d0
	ext.l	d1
	lsl.l	#7,d1			;diskoffset
	lsl.l	#2,d1
	MOVE.L	_resload(PC),A2
	JSR	(resload_LoadFileOffset,a2)
	tst.l	D0
	bmi.b	.exit\@
	pea	HDP_FnameStart(PC)
	pea	205
	pea	TDREASON_DOSREAD
	move.l	(_resload,pc),-(a7)
	add.l	#resload_Abort,(a7)
	rts
.exit\@
	movem.l	(A7)+,d1-d2/a0-a2
	not.l	d0		;KNOWN BUG: RETURNCODE UNSET!
	MEXIT
	ENDC	
;-------------------------------------
; d1 len
; d2 offset
	IFC	\1,'ReadDiskPart'
	movem.l	d1/a0-a2,-(A7)
	move.l	a0,a1			;address
	lea.l	HDP_FnameDiskNo(PC),a0
	add.b	#'1',d0			;only up to 9 disks
	move.b	d0,(A0)
	lea.l	HDP_FnameStart(PC),a0
	move.l	D1,D0			;len to read
	move.l	d2,d1			;diskfile offset
	MOVE.L	_resload(PC),A2
	JSR	(resload_LoadFileOffset,a2)
	tst.l	D0
	bmi.b	.ok\@
	pea	HDP_FnameStart(PC)
	pea	205
	pea	TDREASON_DOSREAD
	move.l	(_resload,pc),-(a7)
	add.l	#resload_Abort,(a7)
	rts
.ok\@
	movem.l	(A7)+,d1/a0-a2
	not.l	d0		;KNOWN BUG: RETURNCODE UNSET!
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'ReadFile'
			; can understand command 0 and 5
	movem.l	d1-d3/a0-a2,-(A7)
	move.l	D0,D3
	MOVE.L	_resload(PC),A2
	JSR	(resload_GetFileSize,a2)
	cmp.w	#5,D3
	beq.b	.exit\@
	movem.l	(A7),d1-d3/a0/a1
	cmp.l	d0,d1
	bls.s	.whdm_loadfile\@
;.usefilesize\@
	move.l	d0,d1
	move.l	d1,(A7)
.whdm_loadfile\@
	MOVE.L	D1,D0			;len to read
	MOVEQ.L	#0,D1			;start with begin of file
	;name and address match jst-function
	JSR	(resload_LoadFileOffset,a2)
		;result: D0=0 if success, d1-len if success
	not.l	d0
.exit\@
	movem.l	(A7)+,d1-d3/a0-a2
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'ReadFilePart'
					;ignores command (d0)
	movem.l	d1/a0-a2,-(A7)
	MOVE.L	_resload(PC),A2
	JSR	(resload_GetFileSize,a2)
	movem.l	(A7),d1/a0/a1
	cmp.l	d0,d1
	bls.s	.whdm_loadfile\@
	move.l	d0,d1
	move.l	d1,(A7)
.whdm_loadfile\@
	MOVE.L	D1,D0			;len to read
	MOVE.L	D2,D1			;start with begin of file
	;name and address match jst-function
	JSR	(resload_LoadFileOffset,a2)
	movem.l	(A7)+,d1/a0-a2
	not.l	d0
			;result: D0=0 if success, d1-len if success
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'StrlenAsm'
	movem.l	A0,-(A7)
	move.l	D0,A0
	clr.l	D0
.loop\@: 
	tst.b	(A0,D0.L)
	beq.b	.exit\@
	addq.l	#1,D0
	bra.b	.loop\@
.exit\@:
	movem.l	(A7)+,A0
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'ReadUserFileHD'
					;ignores command (d0)
	movem.l	d1/a0-a2,-(A7)
	MOVE.L	_resload(PC),A2
	JSR	(resload_GetFileSize,a2)
	movem.l	(A7),d1/a0/a1
	tst.l	d0
	beq.b	.exit\@
	cmp.l	d0,d1
	bls.s	.whdm_loadfile\@
	move.l	d0,d1
	move.l	d1,(A7)
.whdm_loadfile\@
	MOVE.L	D1,D0			;len to read
	MOVEQ.L	#0,D1			;start with begin of file
	;name and address match jst-function
	JSR	(resload_LoadFileOffset,a2)
.exit\@
	movem.l	(A7)+,d1/a0-a2
	not.l	d0
			;result: D0=0 if success, d1-len if success
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'WriteUserFileHD'
					;ignores command (d0)
	movem.l	d1/a0-a2,-(A7)
	MOVE.L	_resload(PC),A2
	MOVE.L	D1,D0			;len to save
	MOVEQ.L	#0,D1			;start with begin of file
	;name and address match jst-function
	JSR	(resload_SaveFileOffset,a2)
	movem.l	(A7)+,d1/a0-a2
	not.l	d0
			;result: D0=0 if success, d1-len if success
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'StrcmpAsm'
	movem.l	d1/a0-a2,-(A7)
	move.l	d0,a0
	move.l	d1,a1
.contstrcmpasm\@
	move.b	(A0)+,d0
	beq.s	.termstrcmpasm\@
	move.b	(A1)+,d1
	beq.s	.failstrcmpasm\@
	bsr.s	.letterstrcmpasm\@
	exg	d0,d1
	bsr.s	.letterstrcmpasm\@
	cmp.b	d0,d1
	bne.s	.failstrcmpasm\@
	bra.s	.contstrcmpasm\@

.termstrcmpasm\@
	tst.b	(A1)+
	bne.s	.failstrcmpasm\@
	moveq.l	#0,d0
	bra.s	.endstrcmpasm\@

.letterstrcmpasm\@
	cmp.b	#$60,d0
	bls.s	.letter1strcmpasm\@
	cmp.b	#$7a,d0
	bhi.s	.letter1strcmpasm\@
	sub.b	#$20,d0
.letter1strcmpasm\@
	rts

.failstrcmpasm\@
	moveq.l	#-1,d0
.endstrcmpasm\@
	movem.l	(A7)+,d1/a0-a2
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'DeleteUserFileHD'
	movem.l	D1/A0/A1/A2,-(A7)
	move.l	(_resload,pc),a2
	jsr	(resload_DeleteFile,a2)
	movem.l	(A7)+,D1/A0/A1/A2
	not.l	D0
	MEXIT
	ENDC

;-------------------------------------
	IFC	\1,'ToUpperAsm'
	movem.l	D0/D1/A0,-(A7)
	move.l	D0,A0
	clr.l	D0
.loop\@ 
	move.b	(A0,D0.L),D1		; gets char
	beq.b	.exit\@

	cmp.b	#'a',D1
	bcs	.skip\@
	cmp.b	#'z'+1,D1
	bcc	.skip\@

	add.b	#'A'-'a',D1		; converts to upper
	move.b	D1,(A0,D0.L)
.skip\@
	addq.l	#1,D0
	bra	.loop\@
.exit\@
	movem.l	(A7)+,D0/D1/A0
	MEXIT
	ENDC
;-------------------------------------
	IFC	\1,'LogPatch'
	; dummy
	MEXIT
	ENDC	

;-------------------------------------
	FAIL	'JSRGEN-WHDLOAD'
	ELSE

	PEA	.END_JSRGEN\@(PC)

	MOVE.L	RelTable(PC),-(A7)
	ADD.L	#RelOff_\1,(A7)

	MOVE.L	A0,-(A7)
	MOVE.L	4(A7),A0
	MOVE.L	(A0),4(A7)

	MOVE.L	(A7)+,A0
	RTS

.END_JSRGEN\@
	ENDC
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

	IFD	WHDLOADSLAVE
;-------------------------------------
	IFC	\1,'Display'
				;ignore display
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'NewLine'
				;ignore newline
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'UseHarryOSEmu'
OSEMU_BASE=$400
	lea	_custom+color,A0
	move.l	#31,D0
.cloop\@
	move.w	#0,(A0)+
	dbf	D0,.cloop\@


	lea	.osemuname\@(pc),a0
	lea	OSEMU_BASE,a1
	move.l	(_resload,pc),a2
	jsr	(resload_LoadFileDecrunch,a2)	;this allows to
						;compress the osemu
	tst.l	D0
	bne.b	.loadok\@
	pea	.osemuname\@(pc)
	pea	205
	pea	TDREASON_DOSREAD
	move.l	(_resload,pc),-(a7)
	add.l	#resload_Abort,(a7)
	rts
.loadok\@
	move.l	(_resload,pc),a0	;the resload base
	lea	(_whdinit_base,pc),a1		;the slave structure

	move.l	(ws_ExpMem,A1),(OSEMU_BASE+$24)
	beq.b	.nofast\@
	move.l	#FASTMEMSIZE,(OSEMU_BASE+$28)
.nofast\@
	IFD	OSEMU_USES_LISTFILES
	move.l	A3,-(a7)
	lea	.buffer\@(pc),a3
	move.l	A3,(OSEMU_BASE+76)
	move.l	#$400,(OSEMU_BASE+80)
	move.l	(A7)+,A3
	bra	.initemu\@
.buffer\@:
	ds.l	$400		; 4K buffer for listfiles
	ENDC
.initemu\@
	jsr	OSEMU_BASE
	
	sub.l	A2,A2
	sub.l	A3,A3
	sub.l	A4,A4
	sub.l	A5,A5
	moveq.l	#0,D0
	moveq.l	#0,D1
	moveq.l	#0,D2
	moveq.l	#0,D3
	moveq.l	#0,D4
	moveq.l	#0,D5
	moveq.l	#0,D6
	moveq.l	#0,D7
	move.l	$4.W,A6
	bra.b	.out\@
.osemuname\@:
	dc.b	"OSEmu.400",0
	even
.out\@
	MEXIT
	ENDC
;-------------------------------------
	IFC	\1,'LoadSmallFiles'
				;ignore LoadSmallFiles (whdload uses preload)
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'LoadFiles'
				;ignore LoadFiles (whdload uses preload)
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'Degrade'
	IFD	WHDM_DEGRADE_CALLED
	FAIL	'Degrade called twice'
	ENDC
WHDM_DEGRADE_CALLED SET	1
				;map Degrade on SetCACR
	movem.l	d0-d1/a0-a2,-(A7)
	move.l	_resload(PC),a2
	and.l	#CACRF_EnableI|CACRF_EnableD,d0	;mask bits allowed in whdload
	and.l	#CACRF_EnableI|CACRF_EnableD,d1	;mask bits allowed in whdload
	jsr	(resload_SetCACR,a2)
	movem.l	(A7)+,d0-d1/a0-a2

	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'SupervisorMode'
	IFD	WHDM_SUPERVISORMODE_CALLED
	FAIL	'SupervisorMode called twice'
	ENDC
WHDM_SUPERVISORMODE_CALLED SET	1
				;ignore SupervisorMode once
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'SaveOSData'
	IFD	WHDM_SAVEOSDATA_CALLED
	FAIL	'SaveOSData called twice'
	ELSE
WHDM_SAVEOSDATA_CALLED SET	1
				;ignore SaveOSData once
	sub.l	#$100,D0
	move.l	D0,A7	;change default stack location, more compatible with JST
	ENDC
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'SetLocalVarZone'
				;ignored atm
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'AllocExtMem'
	IFD	WHDM_ALLOCEXTMEM_CALLED
	FAIL	'AllocExtMem called twice'
	ENDC
WHDM_ALLOCEXTMEM_CALLED SET	1
	cmp.l	#FASTMEMSIZE,D0
	beq.s	.whdm_allocextmem1\@
	illegal
.whdm_allocextmem1\@
	move.l	_whd_fmemaddr(PC),d0
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'Alloc24BitMem'
	IFD	WHDM_ALLOCEXTMEM_CALLED
	FAIL	'Alloc24BitMem called twice'
	ENDC
WHDM_ALLOCEXTMEM_CALLED SET	1
	cmp.l	#FAKEFMEMSIZE,D0
	beq.s	.whdm_alloc24bitmem1\@
	illegal
.whdm_alloc24bitmem1\@
	move.l	#CHIPMEMSIZE,d0
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'CloseAll'
	pea	TDREASON_DEBUG
	move.l	(_resload,pc),-(a7)
	add.l	#resload_Abort,(a7)
	rts
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'TestFile'
	movem.l	d1/a0-a2,-(A7)
	move.l	_resload(PC),a2
	move.l	d0,a0
	jsr	(resload_GetFileSize,a2)
	movem.l	(A7)+,d1/a0-a2
	tst.l	d0
	bne.s	.whdm_testfile1\@
	moveq.l	#-1,d0
	bra.s	.whdm_testfile2\@

.whdm_testfile1\@
	moveq.l	#0,d0
.whdm_testfile2\@
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'LoadDisks'
				;ignore loaddisks
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'WaitMouse'
.waitmousem_\@
	move.w	$dff006,$dff180
	btst	#6,$bfe001
	bne.s	.waitmousem_\@
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'WaitReturn'
.waitmousem_\@
	move.w	$dff006,$dff180
	btst	#6,$bfe001
	bne.s	.waitmousem_\@
	MEXIT
	ENDC	
;-------------------------------------
				;unsupported functions cause errors
	FAIL	'JSRABS-WHDLOAD'
		;end whdload-part
	ELSE


	PEA	.END_JSRABS\@(PC)

	MOVE.L	AbsTable(PC),-(A7)
	ADD.L	#AbsOff_\1,(A7)

	MOVE.L	A0,-(A7)
	MOVE.L	4(A7),A0
	MOVE.L	(A0),4(A7)

	MOVE.L	(A7)+,A0
	RTS

.END_JSRABS\@
	ENDC
	ENDM

JMPABS:MACRO
	IFNE	NARG-1
		FAIL	arguments "JMPABS"
	ENDC

	IFD	WHDLOADSLAVE
;-------------------------------------
	IFC	\1,'Display'
	rts			;ignore display
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'NewLine'
	rts			;ignore newline
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'LoadFiles'
	rts			;ignore LoadFiles (whdload uses preload)
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'Degrade'
	IFD	WHDM_DEGRADE_CALLED
	FAIL	'Degrade called twice'
	ENDC
WHDM_DEGRADE_CALLED SET	1
				;map Degrade on SetCACR
	movem.l	d0-d1/a0-a2,-(A7)
	move.l	_resload(PC),a2
	and.l	#CACRF_EnableI|CACRF_EnableD,d0	;mask bits allowed in whdload
	and.l	#CACRF_EnableI|CACRF_EnableD,d1	;mask bits allowed in whdload
	jsr	(resload_SetCACR,a2)
	movem.l	(A7)+,d0-d1/a0-a2
	rts
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'SupervisorMode'
	IFD	WHDM_SUPERVISORMODE_CALLED
	FAIL	'SupervisorMode called twice'
	ENDC
WHDM_SUPERVISORMODE_CALLED SET	1
	rts			;ignore SupervisorMode once
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'SaveOSData'
	IFD	WHDM_SAVEOSDATA_CALLED
	FAIL	'SaveOSData called twice'
	ENDC
WHDM_SAVEOSDATA_CALLED SET	1
	rts			;ignore SaveOSData once
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'SetLocalVarZone'
	rts			;ignored atm
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'AllocExtMem'
	IFD	WHDM_ALLOCEXTMEM_CALLED
	FAIL	'AllocExtMem called twice'
	ENDC
WHDM_ALLOCEXTMEM_CALLED SET	1
	cmp.l	#FASTMEMSIZE,D0
	beq.s	.whdm_allocextmem1\@
	illegal
.whdm_allocextmem1\@
	move.l	_whd_fmemaddr(PC),d0
	rts
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'CloseAll'
	pea	TDREASON_DEBUG
	move.l	(_resload,pc),-(a7)
	add.l	#resload_Abort,(a7)
	rts
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'TestFile'
	movem.l	d1/a0-a2,-(A7)
	move.l	_resload(PC),a2
	move.l	d0,a0
	jsr	(resload_GetFileSize,a2)
	movem.l	(A7)+,d1/a0-a2
	tst.l	d0
	bne.s	.whdm_testfile1\@
	moveq.l	#-1,d0
	bra.s	.whdm_testfile2\@

.whdm_testfile1\@
	moveq.l	#0,d0
.whdm_testfile2\@
	rts
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'LoadDisks'
	rts			;ignore loaddisks
	MEXIT
	ENDC
;-------------------------------------
	IFC	\1,'WaitMouse'
.waitmousem_\@
	move.w	$dff006,$dff180
	btst	#6,$bfe001
	bne.s	.waitmousem_\@
	rts
	MEXIT
	ENDC	
;-------------------------------------
	IFC	\1,'WaitReturn'
.waitmousem_\@
	move.w	$dff006,$dff180
	btst	#6,$bfe001
	bne.s	.waitmousem_\@
	rts
	MEXIT
	ENDC	
;-------------------------------------
	FAIL	'JMPABS-WHDLOAD'
	ELSE

	MOVE.L	AbsTable(PC),-(A7)
	ADD.L	#AbsOff_\1,(A7)

	MOVE.L	A0,-(A7)
	MOVE.L	4(A7),A0
	MOVE.L	(A0),4(A7)

	MOVE.L	(A7)+,A0
	RTS

	ENDC
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
		MOVEM.L	D0/A0,-(A7)
		
		MOVEQ	#0,D0
		MOVE.B	\2,D0
	
		IFEQ	NARG-3
			LEA	\3(PC),A0
		ELSE
			SUB.L	A0,A0
		ENDIF
		
		JSRGEN	SetQuitKey

		MOVEM.L	(A7)+,D0/A0
	ENDIF

	IFD	WHDLOADSLAVE
_whd_chipused	SET	_whd_chipused+\1
_saveosdata_tempvar\@	SET	CHIPMEMSIZE+FAKEFMEMSIZE
	IFLT	_saveosdata_tempvar\@-_whd_chipused
	FAIL	'Too few memory set in slave'
	ENDC

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
	movem.l	D0-A0,-(A7)
	lea	\1,A0
	move.l	\2,D0
	JSRGEN	LogPatch
	movem.l	(A7)+,D0-A0
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

	movem.l	Ax/Ay,-(sp)
	lea	\2(pc),Ax
	lea	\1,Ay
	move.w	#\3,(Ay)+
	move.l	Ax,(Ay)
	movem.l	(sp)+,Ax/Ay
	ENDM

PATCHGENXXX:MACRO
	REGISTER_PATCH	\1,#6

	movem.l	Ax/Ay,-(sp)

	move.l	RelTable(pc),Ax
	add.l	#RelOff_\2,Ax
	move.l	(Ax),Ax
	lea	\1,Ay
	move.w	#\3,(Ay)+
	move.l	Ax,(Ay)

	movem.l	(sp)+,Ax/Ay
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
	IFD	WHDLOADSLAVE
	IFLT	NARG-3
		FAIL	arguments "HD_PARAMS"
	ENDC


_whdinit_base
		SLAVE_HEADER		;ws_Security + ws_ID
		dc.w	11		;ws_Version
	IFND	WHDLOADSLAVEFLAGS
		dc.w	WHDLF_ClearMem|WHDLF_EmulTrap|WHDLF_NoDivZero|WHDLF_NoError	;ws_flags
	ELSE
		dc.w	WHDLF_ClearMem|WHDLF_EmulTrap|WHDLF_NoDivZero|WHDLOADSLAVEFLAGS
	ENDC
		dc.l	CHIPMEMSIZE+FAKEFMEMSIZE ;ws_BaseMemSize
		dc.l	0		;ws_ExecInstall
		dc.w	HDP_End-_whdinit_base	;ws_GameLoader
	IFND	WHDLOADSLAVESUBDIR
		dc.w	0		;ws_CurrentDir
	ELSE
		dc.w	_whddata-_whdinit_base
	ENDC
		dc.w	0		;ws_DontCache
		dc.b	$00		;ws_keydebug = none
_whd_keyexit	dc.b	$5D		;ws_keyexit = *
_whd_fmemaddr	DC.L	FASTMEMSIZE	;fastmem
		dc.w	_whdi_name-_whdinit_base
		dc.w	_whdi_copyright-_whdinit_base
		dc.w	_whdi_installer-_whdinit_base

_whdi_name	dc.b	'\4',0	
_whdi_copyright	dc.b	'\5',0
_whdi_installer	dc.b	'\6'
	; extra arg to the macro: extra comment
	IFC	\#,7
	dc.b	10,10,'\7',10
	ENDC

	IFD	MAJOR_VERSION
;	dc.b	10,'V',MAJOR_VERSION+'0',".",MINOR_VERSION/10+'0'
;	dc.b	(MINOR_VERSION-(MINOR_VERSION/10)*10)+'0',' '
	dc.b	10,'V',MAJOR_VERSION+'0',"."
	IFGE	MINOR_VERSION-10
	dc.b	MINOR_VERSION/10+'0'
	ENDC
	dc.b	(MINOR_VERSION-(MINOR_VERSION/10)*10)+'0',' '
	DOSCMD	"WDate >T:date"
	INCBIN	"T:date"
	ENDC
	dc.b	0
	EVEN
HDP_FnameStart:
	IFD	MAXON_ASM
		DC.B	"\1"
HDP_FnameDiskNo	dc.b	0,0
	ELSE
		dc.b	\1
HDP_FnameDiskNo	dc.b	0,0			; used for diskfiles
	ENDIF
	cnop	0,4
HDP_End:
;function to init slave
;atm only for loading regs in whdload as overgiven by jst
	lea	_resload(pc),a1
	move.l	a0,(a1)			;save for later use
	bra.s	_whdinit_slave1

_resload	dc.l	0

_tags		dc.l	WHDLTAG_CUSTOM5_GET	;trainer
_whdi_custom5	dc.l	0
		dc.l	WHDLTAG_CUSTOM4_GET	;joypad
_whdi_custom4	dc.l	0
		dc.l	WHDLTAG_BUTTONWAIT_GET	;buttonwait
_whdi_buttonwait dc.l	0
		dc.l	WHDLTAG_ATTNFLAGS_GET	;attnflags
_whdi_attnflags dc.l	0
		dc.l	0

_whdinit_slave1
	;get tags
	lea	(_tags,pc),a0
	move.l	_resload(PC),a2
	jsr	(resload_Control,a2)


	move.l	_whdi_custom5(PC),d0
	beq.b	.noc5
	moveq.l	#-1,D0		; force to -1 (according to Harry)
.noc5
	moveq.l	#0,d1
	move.l	_whdi_custom4(PC),d2	; joypad
	beq.b	.noc4
	moveq.l	#-1,D2		; force to -1 (according to Harry)
.noc4
	moveq.l	#0,d3
	move.l	_whdi_buttonwait(PC),d4
	moveq.l	#0,d5

_whd_chipused	set	0	;needed for verification of memoryroutines
_whd_fastused	set	0
_whd_fakefused	set	0

	ELSE

	IFLT	NARG-3
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
	ENDC	;ENDC jst/whdload
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
