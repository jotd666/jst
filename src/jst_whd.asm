; WHDLoad emulation source code
; relocatable routines

DEF_WHDOFFSET:MACRO
fake_\1:
	bra.w	jst_\1
	CNOP	0,4
	ENDM	

waitvb	MACRO
.1\@		btst	#0,(vposr+1,\1)
		beq	.1\@
.2\@		btst	#0,(vposr+1,\1)
		bne	.2\@

	ENDM
	***** flash the screen and wait for LMB
blitz		MACRO
		movem.l	d0/a0,-(a7)
		lea	$dff000,a0
	;	move	#DMAF_SETCLR!DMAF_RASTER,(dmacon,a0)
.lpbl\@
	IFNE NARG&1
		move	#$4200,(bplcon0,a0)
	ENDC
		move.w	d0,(color,a0)
		subq.w	#1,d0
		btst	#6,$bfe001
		bne	.lpbl\@
		bsr	.waitvb\@			;entprellen
		bsr	.waitvb\@			;entprellen
.lp2bl\@
	IFNE NARG&1
		move	#$4200,(bplcon0,a0)
	ENDC
		move.w	d0,(color,a0)
		subq.w	#1,d0
		btst	#6,$bfe001
		beq	.lp2bl\@
		bsr	.waitvb\@			;entprellen
		bsr	.waitvb\@			;entprellen
		clr.w	(color,a0)
		movem.l	(a7)+,d0/a0
		bra	.end\@
.waitvb\@	waitvb	a0
		rts
.end\@
		ENDM

RelFun_Priv_WHDStart:
	lea	is_whdload(pc),A0
	move.w	#1,(A0)					; tell JST we're emulating WHDLoad

	SET_VAR_CONTEXT
	GETVAR_L	object_entry,A1

	move.l	#$CCCCCCCC,d1
	move.w	ws_Flags(A1),D0
	btst	#WHDLB_ClearMem,D0
	beq.b	.noclr
	moveq.l	#0,d1
.noclr
	lea	$0.W,A0	; zero page
	bsr		RelFun_GetAttnFlags
	btst	#AFB_68020,d0
	bne.b	.fullzp	; do nothing on 68000/68010
	lea	$100.W,A0	; preserve zero page
.fullzp
	GETVAR_L	maxchip,A2
.zapchip
	move.l	D1,(A0)+
	cmp.l	A2,A0
	bcs	.zapchip
	
	move.l	ws_ExpMem(A1),D0
	beq.b	.skipexp
	; don't zap kick memory
	ADDVAR_L	kicksize,D0
	move.l	D0,A0
	GETVAR_L	extsize,D1
	SUBVAR_L	kicksize,D1
	beq.b	.skipexp		; only expmem: don't clear
	move.l	D1,A2
	add.l	A0,A2
.zapext
	move.l	D1,(A0)+
	cmp.l	A2,A0
	bcs	.zapext
.skipexp
	
	move.l	#$F0000001,$4.W	; fixed wasn't the same value as whdload
	clr.l	$0.W
	
	moveq.l	#0,D0
	move.w	ws_GameLoader(A1),D0
	add.l	D0,A1			; loader entry address

	JSRGEN	FreezeAll
	; Here we return to the user loader
	; start WHDLoad/JST slave
	TSTVAR_L	d_flag
	beq.b	.go
	blitz
.go
	lea	fake_resload(pc),A0			; fake WHD library MUST be in A0
	; zap the rest of registers. Only A0 is valid when entering slave
	pea	(A1)
	move.l	#$D0D0D0D0,D0
	move.l	#$D1D1D1D1,D1
	move.l	#$D2D2D2D2,D2
	move.l	#$D3D3D3D3,D3
	move.l	#$D4D4D4D4,D4
	move.l	#$D5D5D5D5,D5
	move.l	#$D6D6D6D6,D6
	move.l	#$D7D7D7D7,D7
	move.l	#$A1A1A1A1,A1
	move.l	#$A2A2A2A2,A2
	move.l	#$A3A3A3A3,A3
	move.l	#$A4A4A4A4,A4
	move.l	#$A5A5A5A5,A5
	move.l	#$A6A6A6A6,A6
	nop
	nop
	rts
	nop
	nop

	; inserted a CNOP there

	CNOP	0,4
fake_resload:

	DEF_WHDOFFSET	resload_Install		;(private)
	DEF_WHDOFFSET	resload_Abort
		; return to operating system
		; IN: (a7) = DEF_WHDOFFSET  success (one of TDREASON_xxx)
		;   (4,a7) = DEF_WHDOFFSET  primary error code
		;   (8,a7) = DEF_WHDOFFSET  secondary error code
		; OUT :	-
		; DANGER this routine must called via JMP ! (not JSR)
	DEF_WHDOFFSET	resload_LoadFile
		; load to BaseMem
		; IN :	a0 = CPTR   name of file
		;	a1 = APTR   address
		; OUT :	d0 = BOOL   success (size of file)
		;	d1 = DEF_WHDOFFSET  dos errcode (0 if all went ok)
	DEF_WHDOFFSET	resload_SaveFile
		; save from BaseMem
		; IN :	d0 = LONG   length to save
		;	a0 = CPTR   name of file
		;	a1 = APTR   address
		; OUT :	d0 = BOOL   success
		;	d1 = DEF_WHDOFFSET  dos errcode (0 if all went ok)
	DEF_WHDOFFSET	resload_SetCACR
		; sets the CACR (also ok with 68000's and from user-state)
		; IN :	d0 = DEF_WHDOFFSET  new cacr
		;	d1 = DEF_WHDOFFSET  mask (bits to change)
		; OUT :	d0 = DEF_WHDOFFSET  old cacr
	DEF_WHDOFFSET	resload_ListFiles
		; list files in dir to buffer
		; IN :	d0 = DEF_WHDOFFSET  buffer size (a1)
		;	a0 = CPTR   name of directory to scan (relative)
		;	a1 = APTR   buffer (MUST reside in Slave !!!)
		; OUT :	d0 = DEF_WHDOFFSET  amount of listed names
		;	d1 = DEF_WHDOFFSET  dos errcode (0 if all went ok)
	DEF_WHDOFFSET	resload_Decrunch
		; decrunch memory
		; IN :	a0 = APTR   source
		;	a1 = APTR   destination (can be equal to source)
		; OUT :	d0 = BOOL   success (size of file unpacked)
	DEF_WHDOFFSET	resload_LoadFileDecrunch
		; IN :	a0 = CPTR   name of file (anywhere)
		;	a1 = APTR   address (MUST inside BaseMem !!!)
		; OUT :	d0 = BOOL   success (size of file)
		;	d1 = DEF_WHDOFFSET  dos errcode (0 if all went ok)
	DEF_WHDOFFSET	resload_FlushCache
		; flush all caches
		; IN :	-
		; OUT :	-
	DEF_WHDOFFSET	resload_GetFileSize
		; IN :	a0 = CPTR   name of file
		; OUT :	d0 = LONG   size of file or 0 if doesn't exist (or empty)
	DEF_WHDOFFSET	resload_DiskLoad
		; IN :	d0 = DEF_WHDOFFSET  offset
		;	d1 = DEF_WHDOFFSET  size
		;	d2 = DEF_WHDOFFSET  disk number
		;	a0 = APTR   destination
		; OUT :	d0 = BOOL   success
		;	d1 = DEF_WHDOFFSET  dos errorcode (if failed)

******* the following functions require ws_Version >= 2

	DEF_WHDOFFSET	resload_DiskLoadDev
		; IN :	d0 = DEF_WHDOFFSET  offset
		;	d1 = DEF_WHDOFFSET  size
		;	a0 = APTR   destination
		;	a1 = STRUCT taglist
		; OUT :	d0 = BOOL   success
		;	d1 = DEF_WHDOFFSET  trackdisk errorcode (if failed)

******* the following functions require ws_Version >= 3

	DEF_WHDOFFSET	resload_CRC16
		; IN :	d0 = DEF_WHDOFFSET  length
		;	a0 = APTR   address
		; OUT :	d0 = UWORD  crc checksum

******* the following functions require ws_Version >= 5

	DEF_WHDOFFSET	resload_Control
		; IN :	a0 = STRUCT taglist
		; OUT :	d0 = BOOL   success
	DEF_WHDOFFSET	resload_SaveFileOffset
		; save from BaseMem
		; IN :	d0 = DEF_WHDOFFSET  length to save
		;	d1 = DEF_WHDOFFSET  offset
		;	a0 = CPTR   name of file
		;	a1 = APTR   address
		; OUT :	d0 = BOOL   success
		;	d1 = DEF_WHDOFFSET  dos errcode (if failed)

******* the following functions require ws_Version >= 6

	DEF_WHDOFFSET	resload_ProtectRead
		; IN :	d0 = DEF_WHDOFFSET  length
		;	a0 = CPTR   address
		; OUT :	-
	DEF_WHDOFFSET	resload_ProtectReadWrite
		; IN :	d0 = DEF_WHDOFFSET  length
		;	a0 = CPTR   address
		; OUT :	-
	DEF_WHDOFFSET	resload_ProtectWrite
		; IN :	d0 = DEF_WHDOFFSET  length
		;	a0 = CPTR   address
		; OUT :	-
	DEF_WHDOFFSET	resload_ProtectRemove
		; IN :	d0 = DEF_WHDOFFSET  length
		;	a0 = CPTR   address
		; OUT :	-
	DEF_WHDOFFSET	resload_LoadFileOffset
		; IN :	d0 = DEF_WHDOFFSET  offset
		;	d1 = DEF_WHDOFFSET  size
		;	a0 = CPTR   name of file
		;	a1 = APTR   destination
		; OUT :	d0 = BOOL   success
		;	d1 = DEF_WHDOFFSET  dos errorcode (if failed)

******* the following functions require ws_Version >= 8

	DEF_WHDOFFSET	resload_Relocate
		; IN :	a0 = APTR   address of executable (source/destination)
		;	a1 = STRUCT taglist
		; OUT :	d0 = DEF_WHDOFFSET  size of relocated executable
		; will *never* be emulated properly
	DEF_WHDOFFSET	resload_Delay
		; IN :	d0 = DEF_WHDOFFSET  time to wait in 1/10 seconds
		; OUT :	-
	DEF_WHDOFFSET	resload_DeleteFile
		; IN :	a0 = CPTR   name of file
		; OUT :	d0 = BOOL   success
		;	d1 = DEF_WHDOFFSET  dos errorcode (if failed)

;******* the following functions require ws_Version >= 10 (totally unsupported!)

	DEF_WHDOFFSET	resload_ProtectSMC
		; detect self modifying code
		; IN :	d0 = ULONG  length
		;	a0 = CPTR   address
		; OUT :	-
	DEF_WHDOFFSET	resload_SetCPU
		; control CPU setup
		; IN :	d0 = ULONG  properties
		;	d1 = ULONG  mask
		; OUT :	d0 = ULONG  old properties
	DEF_WHDOFFSET	resload_Patch
		; apply patchlist
		; IN :	a0 = APTR   patchlist
		;	a1 = APTR   destination address
		; OUT :	-
	DEF_WHDOFFSET	resload_LoadKick
		; load kickstart image
		; IN :	d0 = ULONG  length of image
		;	d1 = UWORD  crc16 of image
		;	a0 = CPTR   basename of image
		; OUT :	-
	DEF_WHDOFFSET	resload_Delta
		; apply wdelta
		; IN :	a0 = APTR   src data
		;	a1 = APTR   dest data
		;	a2 = APTR   wdelta data
		; OUT :	-
	DEF_WHDOFFSET	resload_GetFileSizeDec
		; get size of a packed file
		; IN :	a0 = CPTR   filename
		; OUT :	d0 = ULONG  size of file
;******* the following functions require ws_Version >= 15 
	DEF_WHDOFFSET	resload_PatchSeg
	DEF_WHDOFFSET	resload_Examine
	DEF_WHDOFFSET	resload_ExNext
	DEF_WHDOFFSET	resload_GetCustom
  ;success = resload_GetCustom(buflen, reserved, buffer)
  ;        D0                    D0       D1       A0
  ;       ULONG                       ULONG    ULONG    CPTR
 ;Get the string which has been specified via the Custom/S option.
 ;       The buffer is filled as far as possible. If buffer was not large
 ;       enough false will be returned.


fake_resload_end:

jst_resload_Install:
	RUNTIME_ERROR_ROUTINE	WHD_Install,"WHDLoad install called!"
jst_resload_Abort:
	bra	WHD_Abort
jst_resload_GetFileSizeDec
	STORE_REGS	D1-D2/A1/A4

	; read the start of the file in the buffer

	; cannot load files in resload zone (there are checks in CD32Load - crazy memory copy)
	move.l	$100.W,-(A7)
	move.l	$104.W,-(A7)
	lea	$100.W,A1
	moveq.l	#0,D2
	moveq.l	#8,D1
	SET_VAR_CONTEXT
	;;CLRVAR_L	last_io_error
	JSRGEN	ReadFilePart
	tst.l	D0
	beq.b	.ok
	SETVAR_L	#ERROR_OBJECT_NOT_FOUND,last_io_error	; set any error code: file is not found
	; allows to distinguish between file not found and file not crunched
	moveq.l	#0,d0
	bra.b	.exit
.ok
	move.l	(A1)+,d0
	cmp.l	#"IMP!",d0
	beq.b	.read
	cmp.l	#"TPWM",d0
	beq.b	.read
	cmp.l	#"ATN!",d0
	beq.b	.read
	move.b	#'C',d0
	cmp.l	#"RNCC",d0
	beq.b	.read
	; file isn't compressed: get its actual size
	bsr	jst_resload_GetFileSize
	bra.b	.exit
.read	
	move.l	(A1)+,d0
.exit
	move.l	(A7)+,$104.W
	move.l	(A7)+,$100.W
	RESTORE_REGS	D1-D2/A1/A4
	tst.l	d0		; some slaves test the CCR flags without performing a TST.L !!
	rts
jst_resload_Delta
	WHDLOAD_UNSUPPORTED_CALL	WHD_Delta,"Delta"

;   success,errorcode = resload_Examine(name, FileInfoBlock)
;    D0        D1                       A0         A1
;   BOOL     ULONG                     CPTR       APTR
; limitation: if file isn't preloaded that doesn't work (file is always "not found")

jst_resload_Examine
	STORE_REGS	D2-A5
	;;blitz
	SET_VAR_CONTEXT
	; init to trash
	move.l	a1,a2
	move.w	#fib_Reserved,D2
	lsr.w	#1,D2
	subq	#1,D2
.clr
	move.w	#$eeee,(a2)+
	dbf 	d2,.clr
	clr.l	(a1)		; zero key
	
	tst.b	(a0)
	bne		.norootdir
	bsr	.setdirparams
	lea	.rootname(pc),a2
	move.l	a2,d0
	lea	(fib_FileName,a1),a2
	move.l	a2,d1
	JSRGEN	StrcpyAsm
	
	bra	.oknodk
.norootdir

; < A0: required filename
; > A5: pointer on file handle (if (A5)=0 then file not found)
; > D0: 0 if file is up to date, !=0 otherwise (update needed)

	bsr	RelFun_Priv_SearchDirEntry
	
	tst.l	(A5)		; CDLERR_DIRNOTFOUND
	beq.b	.notfound
	tst.w	(8,A5)
	beq.b	.setfileparams

	bsr		.setdirparams
	bra		.ok
	
.setfileparams
	move.l	(4,A5),d0
	move.l	D0,(fib_Size,A1)
	lsr.l	#8,D0
	lsr.l	#1,D0
	addq.l	#1,D0
	move.l	D0,(fib_NumBlocks,A1)
	move.l 	#-3,(fib_DirEntryType,A1)	; file
	move.l 	#-3,(fib_EntryType,A1)	; file
	move.l	#$F,(fib_Protection,A1)		; ??? F=RWED
	bra.b	.ok
.notfound:
	move.l	#DOSFALSE,d0
	move.l	#ERROR_OBJECT_NOT_FOUND,d1
	bra.b	.out
.ok
	; compute a diskkey: not needed kickfs uses it to store filename
	
	;move.l	a0,d0
	;bsr	StrlenAsm
	;bsr	CRC16
	;move.l	d0,(fib_DiskKey,a1)
	
	; copy basename
	move.l	a0,d1
	lea		(fib_FileName,a1),a2
	move.l	a2,d2
	
; < D1: file/dir name
; < D2 (modified): basename of D1
	JSRGEN	Basename
.oknodk
	move.l	#$3670,(fib_DateStamp,A1)
	move.l	#$0256,(fib_DateStamp+4,A1)
	move.l	#$0280,(fib_DateStamp+8,A1)
	clr.b	(fib_Comment,A1)	
	move.l	#DOSTRUE,d0	; DOSTRUE
	moveq.l	#0,d1
.out	
	RESTORE_REGS	D2-A5
	RTS
.setdirparams:
	move.l 	#2,(fib_DirEntryType,A1)	; dir
	move.l 	#2,(fib_EntryType,A1)	; dir
	move.l	#$10,(fib_Protection,A1)		; ??? F=RWED
	moveq.l	#0,d1
	move.l	D1,(fib_Size,A1)
	move.l	D1,(fib_NumBlocks,A1)
	rts
	
.rootname
		dc.b	"WHDLoad",0
		even
		
jst_resload_ExNext
	WHDLOAD_UNSUPPORTED_CALL	WHD_ExNext,"ExNext"
	
jst_resload_GetCustom:
	STORE_REGS	D1/A1/A4
	move.l	A0,D1
	SET_VAR_CONTEXT
	LEAVAR	custom_str,A1
	move.l	A1,D0
	JSRGEN	StrcpyAsm
	moveq.l	#-1,d0	; always successful (we don't test buffer length)
	RESTORE_REGS	D1/A1/A4
	rts
	
		; load kickstart image
		; IN :	d0 = ULONG  length of image
		;	d1 = UWORD  crc16 of image
		;	a0 = CPTR   basename of image
		; OUT :	-

jst_resload_LoadKick
	STORE_REGS	A3/A4
	; copy basename to name buffer
	STORE_REGS	D0-D1
	move.l	A0,D0
	lea	.kickext(pc),A0
	move.l	A0,D1
	JSRGEN	StrcpyAsm
	RESTORE_REGS	D0-D1

	STORE_REGS	D2/D3
	move.w	D1,D2		; CRC saved in D2


	move.l	D0,D1		; length
	move.l	D0,D3		; save length in D3
	moveq.l	#-1,D0
	
	lea	.kickname(pc),A0
	SET_VAR_CONTEXT
	GETVAR_L	extbuf,A1
	INGAMEOSCALL	ReadTheFileHD
	tst.l	D0
	bne	.kickloaderr

	STORE_REGS	A0

	GETVAR_L	extbuf,A0
	move.l	D3,D0	; length to check
	JSRGEN	CRC16
	cmp.w	D2,D0	; checks with provided CRC
	bne	RunTime_WrongKickCRC
	RESTORE_REGS	A0

	move.l	A0,D0
	JSRGEN	StrlenAsm
	lea	.kickname(pc),A0
	move.b	#'.',(A0,D0)	
	move.b	#'R',(1,A0,D0)	
	move.b	#'T',(2,A0,D0)	
	move.b	#'B',(3,A0,D0)	

	lea	.kickname(pc),A0
	SET_VAR_CONTEXT
	moveq.l	#-1,D1
	GETVAR_L	extbuf,A1
	add.l	D3,A1
	INGAMEOSCALL	ReadTheFileHD
	tst.l	D0
	bne	.kickloaderr

; < extbuf: memory with kickstart to reloc
; < D3 kicksize
; < A3 pointer to RTB
	GETVAR_L	extbuf,A3
	add.l	D3,A3		; points to RTB
	
	bsr	.relockick
	RESTORE_REGS	D2/D3
	RESTORE_REGS	A3/A4
	rts
	
.relockick:
	include		"reloc_kickstart.asm"


.kickloaderr:
	lea	.kickerrmsg(pc),A5
	bra	RTStoreMessage
	
.kickerrmsg
	dc.b	"LoadKick: Cannot open "
.kickname:
	dc.b	"DEVS:Kickstarts/Kick"
.kickext:
	blk.b	20,0
	even


;length,errorcode = resload_LoadFile(name, address)
; D0      D1                         A0      A1
;ULONG    ULONG                      CPTR    APTR

jst_resload_LoadFile:
	moveq.l	#-1,D1
	moveq.l	#0,D0

    
	JSRGEN	ReadFile
	exg	D0,D1	; swap D0 and D1 registers
	tst.l	D1	; D1=0: okay
	bne.b	.error

	; no error, size in D0, 0 in D1

	rts
.error
	moveq.l	#0,D0	; size=0
	moveq.l	#-1,D1	; error
	rts

; new since v4.0: removes /'s in front of filename
; (WHDLoad uses this to go to parent dir, but because
; of a different implementation and for safety reasons
; I removed it)

jst_resload_SaveFile:
	STORE_REGS	A0
	move.l	D0,D1	; length

;;	cmp.b	#'/',(A0)
;;	bne.b	.skip		; skips '/' on write
;;	addq.l	#1,A0
;;.skip

	moveq.l	#0,D0	; command: save
	JSRGEN	WriteFileHD
	moveq.l	#0,D1
	tst.l	D0
	beq.b	.ok
	move.l	#205,D1
.ok
	neg.l	D0
	RESTORE_REGS	A0
	rts

; < D0: new status
; < D1: mask (is the same)
; partially implemented to remove caches or set instruction caches
; mask is ignored as whdload does a thing with MMU I don't get, as Bert says:

;; I assume WHDLoad is using the MMU. So this works as intended.
;; Please check http://whdload.de/docs/en/cache.html
;; Default setup is Chip not cacheable, Slave/Exp cacheable (I+D).
;; If you enable ICache using resload_SetCACR Chip becomes cachable.
;; DCache will be disabled because you didn't wanted it and MMU allows
;; not to separate between D and I.
;; If MMU is not used initial CACR would be 0.

jst_resload_SetCACR:
	movem.l	d2,-(a7)
	JSRGEN	GetAttnFlags
	STORE_REGS	A5
	lea	set_cacr_sup(pc),a5
	bsr	call_as_supervisor
	RESTORE_REGS	A5
	movem.l	(a7)+,d2
	rts

; < D2: attnflags
set_cacr_sup:
	btst	#AFB_68020,d0
	beq.b	.exit	; do nothing on 68000/68010
	MC68020
	movec	CACR,D1
	or.w	d0,d1
	BTST	#AFB_68030,D0
	BEQ.B	.denabled
	btst	#CACRB_EnableD,d0
	bne.b	.denabled
	bclr	#CACRB_EnableD,d1
	bset	#CACRB_FreezeD,d1
	BTST	#AFB_68040,D0
	BEQ.B	.denabled
	bclr	#CACRB_CopyBack,d1
.denabled
	btst	#CACRB_EnableI,d0
	bne.b	.ienabled
	bclr	#CACRB_EnableI,d1
	bset	#CACRB_FreezeI,d1
.ienabled

	movec	d1,CACR
	MC68000
.exit
	rte
	
jst_resload_ListFiles:
	STORE_REGS	D2-A6
	bsr	RelocNameWithDir
	INGAMEOSCALL	WHDReadDir
	RESTORE_REGS	D2-A6
	rts

jst_resload_Decrunch:
	STORE_REGS	D6
	moveq.l	#0,D6
	
	JSRGEN	RNCLength
	tst.l	D0
	bmi.b	.nornc		; not a RNC file

	move.l	D0,D6		; decrunched length

	JSRGEN	RNCDecrunch
	bra	.exit
.nornc
	JSRGEN	ATNLength
	tst.l	D0
	bmi.b	.noatn		; not a RNC file

	move.l	D0,D6		; decrunched length

	JSRGEN	ATNDecrunch	; ATN! and IMP! files
	tst.l	D0
	bmi.b	.noatn		; not a RNC file

	
	bra.b	.exit
.noatn
	; last chance: try fungus decrunch
	cmp.b	#'T',(A0)
	bne.b	.notpwm
	cmp.b	#'P',1(A0)
	bne.b	.notpwm
	cmp.b	#'W',2(A0)
	bne.b	.notpwm
	cmp.b	#'M',3(A0)
	bne.b	.notpwm
	JSRGEN	TPWMDecrunch
	moveq.l	#0,D6		; len not supported
	bra.b	.exit
.notpwm
	moveq.l	#0,D0		; not crunched: returns 0

.exit
	move.l	D6,D0		; decrunched length
	RESTORE_REGS	D6
	JSRGEN	FlushCachesHard
	tst.l	d0
	rts

	
jst_resload_LoadFileDecrunch:
	bsr	jst_resload_LoadFile
	tst.l	D1
	bne.b	.error
	STORE_REGS	D2-A6

	move.l	D0,D6

	move.l	A1,A0
	bsr	jst_resload_Decrunch

	tst.l	D0
	bne.b	.waspacked

	move.l	D6,D0	; file was not packed: restore real length
.waspacked
	RESTORE_REGS	D2-A6
	moveq.l	#0,D1	; no error, anyway
	rts
.error:
	moveq.l	#0,D0	; whdload API specifies that
	tst.l	d1
	rts
	
jst_resload_FlushCache:
	bra	RelFun_FlushCachesHard
	
; < A0: filename
; > D0: size (or 0 if empty or not found, setting the error flag if not found)
; limitation: if file isn't preloaded that doesn't work (file is always not found)
jst_resload_GetFileSize:
	STORE_REGS	D1/A4/A5

	SET_VAR_CONTEXT
	CLRVAR_L	last_io_error
	
; < A0: required filename
; > A5: pointer on file handle (if (A5)=0 then file not found)
	bsr	RelFun_Priv_SearchDirEntry
	
	tst.l	(A5)
	beq.b	.notfound
	tst.w	(8,A5)
	bne.b	.is_a_dir

	move.l	(4,A5),d0
.exit
	RESTORE_REGS	D1/A4/A5
	tst.l	d0		; some slaves test the CCR flags without performing a TST.L !!
	rts
.notfound
	moveq.l	#0,d0
	SETVAR_L	#205,last_io_error	; file not found
	bra.b	.exit
.is_a_dir
	moveq.l	#0,d0
	SETVAR_L	#211,last_io_error	; invalid lock?
	bra.b	.exit

	
; IN :	d0 = DEF_WHDOFFSET  offset
;	d1 = DEF_WHDOFFSET  size
;	d2 = DEF_WHDOFFSET  disk number
;	a0 = APTR   destination
; OUT :	d0 = BOOL   success
;	d1 = DEF_WHDOFFSET  dos errorcode (if failed)

; interrupts are frozen here, which allows SWIV to work
; SWIV probably works out of luck in WHDLoad (and CD32load!!) because some game interrupt
; is probably reading/writing on registers that are expected to be fixed.
; if the interrupt occurs while loading, mayhem can occur. Freezing the interrupts
; when loading should be imperceptible (maybe the music??) and safe, so doing it for
; all games.

jst_resload_DiskLoad:
	STORE_REGS	D2-D3
	SAVE_INTENA_AND_FREEZE
	
	move.l	a0,a1	; dest
	lea	.disknum(pc),a0
	add.b	#'0',d2
	move.b	d2,(a0)
	
	move.l	d0,d2	; offset in D2
	
	; name in A0
	; address in A1
	; D2	; offset
	; D1	; length
	
	lea	.diskname(pc),a0
	JSRGEN	ReadFilePart

	JSRGEN	FlushCachesHard		; as WHDLoad flushes the caches, we do the same
	RESTORE_INTENA

	RESTORE_REGS	D2-D3
	tst.l	D0
	bmi.b	.disk_error

	moveq.l	#0,D1	; errcode: 0 : OK
	moveq.l	#-1,D0	; returns TRUE
	rts

.disk_error:
	moveq.l	#0,D0
	moveq.l	#1,D1
	rts
.diskname:
	dc.b	"disk."
.disknum:
	dc.b	"1",0
	even
	
jst_resload_DiskLoadDev:
	WHDLOAD_UNSUPPORTED_CALL	WHD_DiskLoadDev,"DiskLoadDev"

jst_resload_CRC16:
	bra	RelFun_CRC16

jst_resload_Control:
	STORE_REGS	A0/A1/A4
	SET_VAR_CONTEXT

.loop
	move.l	(A0)+,D0
	beq	.exit
	cmp.l	#WHDLTAG_ATTNFLAGS_GET,D0
	bne.b	.sk1
	JSRGEN	GetAttnFlags
	bra	.storetag
.sk1
	cmp.l	#WHDLTAG_MONITOR_GET,D0
	bne.b	.sk2

	move.l	#PAL_MONITOR_ID,D0
	TSTVAR_L	ntsc_flag
	beq	.storetag
	move.l	#NTSC_MONITOR_ID,D0
	bra	.storetag

.sk2
	cmp.l	#WHDLTAG_BUTTONWAIT_GET,D0
	bne.b	.sk3
	moveq.l	#0,D0
	TSTVAR_L	buttonwait_flag
	beq	.storetag
	moveq.l	#-1,D0
	bra	.storetag

.sk3
	cmp.l	#WHDLTAG_Private3,D0	; registered, I guess?
	bne.b	.sk4
	moveq.l	#1,D0			; yes, always,
	bra	.storetag


.sk4:
	cmp.l	#WHDLTAG_CBAF_SET,D0	; access fault hook
	bne.b	.sk5
	move.l	(A0)+,D0		; gets function address
	STORE_REGS	A0
	move.l	D0,A0
	JSRGEN	SetBusErrorVector
	RESTORE_REGS	A0
	bra	.loop


.sk5:

	cmp.l	#WHDLTAG_KEYTRANS_GET,D0
	bne.b	.sk6
	; raw/ascii table

	LEAVAR	raw_ascii_table,A1
	move.l	A1,D0
	bra	.storetag
.sk6
	cmp.l	#WHDLTAG_CBSWITCH_SET,D0
	bne.b	.sk7
	move.l	(A0)+,D0		; gets function address
	STORE_REGS	A0
	move.l	D0,A0
	JSRGEN	SetAfterSwitchFunction
	RESTORE_REGS	A0
	bra	.loop
	
	; after switch to game callback

.sk7
	cmp.l	#WHDLTAG_CUSTOM1_GET,D0
	bne.b	.sk8

	GETVAR_L	custom1_flag,D0
	bra.b	.storetag
.sk8
	cmp.l	#WHDLTAG_CUSTOM2_GET,D0
	bne.b	.sk9

	GETVAR_L	custom2_flag,D0
	bra.b	.storetag
.sk9
	cmp.l	#WHDLTAG_CUSTOM3_GET,D0
	bne.b	.sk10

	GETVAR_L	custom3_flag,D0
	bra.b	.storetag
.sk10
	cmp.l	#WHDLTAG_CUSTOM4_GET,D0
	bne.b	.sk11

	GETVAR_L	custom4_flag,D0
	bra.b	.storetag
.sk11
	cmp.l	#WHDLTAG_CUSTOM5_GET,D0
	bne.b	.sk12

	GETVAR_L	custom5_flag,D0
	bra.b	.storetag
.sk12
	cmp.l	#WHDLTAG_VERSION_GET,D0	;get WHDLoad major version number
	bne.b	.sk13
	move.l	#18,D0		; highest possible
	bra.b	.storetag
.sk13
	cmp.l	#WHDLTAG_REVISION_GET,D0	;get WHDLoad major version number
	bne.b	.sk14
	moveq.l	#2,D0		; highest possible
	bra	.storetag
.storetag:
	move.l	D0,(A0)+
	bra	.loop
.skiptag:
	addq.l	#4,A0
	bra	.loop

.sk14
	cmp.l	#WHDLTAG_BUILD_GET,D0	;get WHDLoad major version number
	bne.b	.sk15
	move.l	#156,D0		; random
	bra.b	.storetag
.sk15
	cmp.l	#WHDLTAG_ECLOCKFREQ_GET,D0
	bne.b	.sk16
	GETVAR_L	eclock_freq,D0
	bra.b	.storetag
.sk16
	cmp.l	#WHDLTAG_CHIPREVBITS_GET,D0
	bne.b	.sk17
	moveq.l	#0,D0
	GETVAR_B	system_chiprev_bits,d0
	bra.b	.storetag
.sk17
	cmp.l	#WHDLTAG_IOERR_GET,D0
	bne.b	.sk18
	GETVAR_L	last_io_error,D0
	bra.b	.storetag
.sk18
	cmp.l	#WHDLTAG_TIME_GET,D0
	bne.b	.sk19
	lea	current_time(pc),A1
	; fill with fixed but acceptable values
    move.l	#365*38,whdlt_days(A1)	;days since 1978-01-01
	move.l	#500,whdlt_mins(A1)	;minutes since last day
	move.l	#500,whdlt_ticks(A1)	;1/50 seconds since last minute
	move.b	#16,whdlt_year(A1)
	move.b	#1,whdlt_month(A1)
	move.b	#1,whdlt_day(A1)
	move.b	#12,whdlt_hour(A1)
	move.b	#0,whdlt_min(A1)
	move.b	#0,whdlt_sec(A1)
	move.l	A1,D0
	bra	.storetag
.sk19
	cmp.l	#WHDLTAG_LANG_GET,D0
	bne.b	.sk20
	moveq.l	#0,D0	; english?
	bra	.storetag
.sk20
	cmp.l	#WHDLTAG_BPLCON0_GET,D0
	bne	.sk21
	moveq.l	#0,D0
	GETVAR_W	system_bplcon0,D0
	bra	.storetag
.sk21
	cmp.l	#WHDLTAG_DBGADR_SET,D0
	beq	.skiptag		; ignore
	cmp.l	#WHDLTAG_DBGSEG_SET,D0
	beq	.skiptag		; ignore
	bra	.skiptag		; ignore others
.exit
	RESTORE_REGS	A0/A1/A4
	moveq.l	#0,D0		; success!
	rts

current_time:
	blk.b	whdlt_SIZEOF,0


	; IN :	d0 = DEF_WHDOFFSET  length to save
	;	d1 = DEF_WHDOFFSET  offset
	;	a0 = CPTR   name of file
	;	a1 = APTR   address
	; OUT :	d0 = BOOL   success
	;	d1 = DEF_WHDOFFSET  dos errcode (if failed)

jst_resload_SaveFileOffset:
	STORE_REGS	D1/D2
	move.l	D1,D2	; offset
	move.l	D0,D1	; length

	JSRGEN	WriteFilePartHD

	move.l	D0,D1		; "errorcode"
	RESTORE_REGS	D1/D2	; does not affect CCR
	beq	.ok

	moveq.l	#0,D0		; not OK
	rts
.ok
	moveq.l	#-1,D0		; TRUE
	rts
	

jst_resload_ProtectRead:
jst_resload_ProtectReadWrite:
jst_resload_ProtectWrite:
jst_resload_ProtectRemove:
	WHDLOAD_UNSUPPORTED_CALL	WHD_Protect,"Protect"

;;    success,error = resload_LoadFileOffset(size, offset, name, address)
;;          D0     D1                             D0     D1     A0      A1
;;        BOOL   ULONG                          ULONG  ULONG  CPTR    APTR

jst_resload_LoadFileOffset:
	movem.l	D2,-(A7)
	; name in A0
	; address in A1
	move.l	D1,D2	; offset
	move.l	D0,D1	; length

	JSRGEN	ReadFilePart
	movem.l	(A7)+,D2
	move.l	D0,D1		; "errorcode"
	beq.b	.ok

	moveq.l	#0,D0		; not OK
	rts
.ok
	moveq.l	#-1,D0		; TRUE
	rts

******* the following functions require ws_Version >= 8

jst_resload_Relocate:
	JSRGEN	RelocateExecutable
	rts

jst_resload_Delay:
	STORE_REGS
	SET_VAR_CONTEXT
	move.l	#16000,D0	; PAL mode
	TSTVAR_L	ntsc_flag
	beq.b	.pal
	move.l	#19200,D0	; PAL mode
.pal
	JSRGEN	BeamDelay
	subq.l	#1,D1
	beq.b	.exit
	btst	#6,$BFE001
	beq.b	.exit
	btst	#7,$BFE001
	bne.b	.pal
.exit
	RESTORE_REGS
	RTS

; < A0: filename to delete
; > D0: success/error
; > D1: error code from IoErr()

jst_resload_DeleteFile:
	JSRGEN	DeleteFileHD
	rts



;******* the following functions require ws_Version >= 10 (totally unsupported!)

jst_resload_ProtectSMC:
		; detect self modifying code
		; IN :	d0 = ULONG  length
		;	a0 = CPTR   address
		; OUT :	-
	WHDLOAD_UNSUPPORTED_CALL	WHD_ProtectSMC,"ProtectSMC"
jst_resload_SetCPU:
		; control CPU setup
		; IN :	d0 = ULONG  properties
		;	d1 = ULONG  mask
		; OUT :	d0 = ULONG  old properties
		JSRGEN	FlushCachesHard
		rts

jst_resload_PatchSeg
	;LOG_WHD_CALL	"PatchSeg"
	STORE_REGS
	move.l	A0,A2		; store patchlist
	move.l	A1,A6		; store seglist
	add.l	A6,A6
	add.l	A6,A6	; CPTR
	lea	patch_jumptable(pc),A5
	moveq.l	#0,D5		; condition mask: all bits must be at 0 or patch won't apply
	moveq.l	#0,D6		; nest counter

.loop:
	move.l	A6,A1	; reset segment pointers
	
	move.w	(a0)+,D0	; command
	cmp.w	#PLCMD_END,D0
	beq.w	.exit

	
	bclr	#14,D0
	bne.b	.noaddr		; command: no address argument
	bclr	#15,D0
	beq.b	.bit32
	moveq.l	#0,D1
	move.w	(a0)+,D1	; D1.W: offset
	bra.b	.bit16
.bit32
	move.l	(a0)+,D1	; D1.L: address
	; compute the correct A1 (not fixed like in resload_Patch)
	; depending on D1
.bit16
	moveq.l	#0,D3	; segment absolute offset
	
.find_a1:
	move.l	(A1)+,D2
	add.l	D2,D2
	add.l	D2,D2	; D2=next segment address
	move.l	d3,D7	; save previous accumulated segment size
	add.l	-8(a1),D3	; accumulate segment size
	subq.l	#8,d3	; minus size+next segment info
	cmp.l	D3,D1
	bcs.b	.a1_found
	; D1 is above D3: look in next segment

	tst.l	d2
	beq.b	.notfound	; end of seglist found, and offset still too high

	move.l	D2,A1
	bra.b	.find_a1
.a1_found
	sub.l	D7,D1	; make D1 offset relative to current segment
.noaddr:
	cmp.w	#45,D0
	bcc		unsupported_patch_instruction
	add.w	D0,D0
	move.w	(A5,D0.W),D0
	jsr	(A5,D0.W)
	bra.b	.loop
.notfound
	move.l	d1,d0
	bra	patchseg_offset_not_found

.exit
	JSRGEN	FlushCachesHard
	RESTORE_REGS
	;END_WHD_CALL

	RTS

patchseg_offset_not_found
	WHDLOAD_UNSUPPORTED_CALL	PatchSeg,"PatchSeg - offset not found"

	
SKIPIFFALSE	MACRO
		tst.l	d5
		beq.b	.cont\@
		rts
.cont\@
	ENDM
jst_resload_Patch:

		; apply patchlist
		; IN :	a0 = APTR   patchlist
		;	a1 = APTR   destination address
		; OUT :	-
	STORE_REGS
	move.l	A0,A2		; store patchlist
	lea	patch_jumptable(pc),A5
	moveq.l	#0,D5		; condition mask: all bits must be at 0 or patch won't apply
	moveq.l	#0,D6		; nest counter
	SET_VAR_CONTEXT
.loop:
	move.w	(a0)+,D0	; command
	cmp.w	#PLCMD_END,D0
	bne.w	.cont

	; exit
	JSRGEN	FlushCachesHard
	RESTORE_REGS
	RTS
.cont
	bclr	#14,D0
	bne.b	.noaddr		; command: no address argument
	bclr	#15,D0
	beq.b	.bit32
	moveq.l	#0,D1
	move.w	(a0)+,D1	; D1.W: offset
	bra.b	.noaddr
.bit32
	move.l	(a0)+,D1	; D1.L: address
.noaddr:
	cmp.w	#46,D0
	bcs.b	.instok
	bra	unsupported_patch_instruction
.instok
	add.w	D0,D0
	move.w	(A5,D0.W),D2
	jsr	(A5,D2.W)
	bra	.loop
	
patch_jumptable:
	dc.w	.exit-patch_jumptable	; not reached??	0
	dc.w	.R-patch_jumptable		; 1
	dc.w	.P-patch_jumptable			; 2 set JMP
	dc.w	.PS-patch_jumptable		; 3 set JSR
	dc.w	.S-patch_jumptable			; 4 set BRA (skip)
	dc.w	.I-patch_jumptable			; 5 set ILLEGAL
	dc.w	.B-patch_jumptable			; 6 write byte to specified address
	dc.w	.W-patch_jumptable			; 7 write word to specified address
	dc.w	.L-patch_jumptable			; 8 write long to specified address
; version 11
	dc.w	.UNSUP-patch_jumptable			; 9 (A) write address which is calculated as
					;base + arg to specified address
; version 14
	dc.w	.PA-patch_jumptable		; $A write address given by argument to
					;specified address
	dc.w	.NOP-patch_jumptable		; $B fill given area with NOP instructions
; version 15
	dc.w	.CZ-patch_jumptable			; $C (C) clear n bytes
	dc.w	.CB-patch_jumptable		; $D clear one byte
	dc.w	.CW-patch_jumptable		; $E clear one word
	dc.w	.CL-patch_jumptable		; $F clear one long
; version 16
	dc.w	.PSS-patch_jumptable		; $11 set JSR + NOP..
	dc.w	.NEXT-patch_jumptable		;continue with another patch list
	dc.w	.AB-patch_jumptable		;add byte to specified address
	dc.w	.AW-patch_jumptable		;add word to specified address
	dc.w	.AL-patch_jumptable		;add long to specified address
	dc.w	.DATA-patch_jumptable		;write n data bytes to specified address
; version 16.5
	dc.w	.ORB-patch_jumptable		;or byte to specified address
	dc.w	.ORW-patch_jumptable		;or word to specified address
	dc.w	.ORL-patch_jumptable		;or long to specified address
; version 16.6
	dc.w	.GA-patch_jumptable		; (GA) get specified address and store it in the slave
; version 16.9
	dc.w	.UNSUP-patch_jumptable		;call freezer
	dc.w	.UNSUP-patch_jumptable		;show visual bell
; version 17.2
	dc.w	.IFBW-patch_jumptable		;condition if ButtonWait/S
	dc.w	.IFC1-patch_jumptable		;condition if Custom1/N
	dc.w	.IFC2-patch_jumptable		;condition if Custom2/N
	dc.w	.IFC3-patch_jumptable		;condition if Custom3/N
	dc.w	.IFC4-patch_jumptable		;condition if Custom4/N
	dc.w	.IFC5-patch_jumptable		;condition if Custom5/N
	dc.w	.IFC1X-patch_jumptable		;condition if bit of Custom1/N
	dc.w	.IFC2X-patch_jumptable		;condition if bit of Custom2/N
	dc.w	.IFC3X-patch_jumptable		;condition if bit of Custom3/N
	dc.w	.IFC4X-patch_jumptable		;condition if bit of Custom4/N
	dc.w	.IFC5X-patch_jumptable		;condition if bit of Custom5/N
	dc.w	.ELSE-patch_jumptable		;condition alternative
	dc.w	.ENDIF-patch_jumptable		;end of condition block


.IFBW:
	GETVAR_L	buttonwait_flag,D0
	bra	.IFXXX
.IFC1:
	GETVAR_L	custom1_flag,D0
	bra	.IFXXX
.IFC2:
	GETVAR_L	custom2_flag,D0
	bra	.IFXXX
.IFC3:
	GETVAR_L	custom3_flag,D0
	bra	.IFXXX
.IFC4:
	GETVAR_L	custom4_flag,D0
	bra	.IFXXX
.IFC5:
	GETVAR_L	custom5_flag,D0
	bra	.IFXXX
.ELSE:
		bchg	D6,D5	; invert condition
		rts
		
.ENDIF:
		bclr	D6,D5
		subq.l	#1,D6	; unnest
		rts	
.IFC1X:
	GETVAR_L	custom1_flag,D0
	bra	.IFBITXXX
.IFC2X:
	GETVAR_L	custom2_flag,D0
	bra	.IFBITXXX
.IFC3X:
	GETVAR_L	custom3_flag,D0
	bra	.IFBITXXX
.IFC4X:
	GETVAR_L	custom4_flag,D0
	bra	.IFBITXXX
.IFC5X:
	GETVAR_L	custom5_flag,D0


.IFBITXXX
	move.w	(a0)+,d2	; get argument: bit number
	; must be between 0 and 31
	btst	d2,d0
	sne	d0
	ext.w	d0
	ext.l	d0
.IFXXX
	addq.w	#1,d6	; increase nest
	tst.l	d0
	bne	.skif
	bset	d6,d5	; failed condition: set D5 so nothing is patched anymore until ELSE/ENDIF
.skif
	rts
	
.R
	SKIPIFFALSE
	move.w	#$4E75,(A1,D1.L)
	rts
.P
	bsr	.get_slave_address
	SKIPIFFALSE
	move.w	#$4EF9,(A1,D1.L)
	move.l	A3,2(A1,D1.L)
	rts
.PA
	bsr	.get_slave_address
	SKIPIFFALSE
	move.l	A3,(A1,D1.L)
	rts
.PS
	bsr	.get_slave_address
	SKIPIFFALSE
	move.w	#$4EB9,(A1,D1.L)
	move.l	A3,2(A1,D1.L)
	rts
.GA

	bsr	.get_slave_address
	SKIPIFFALSE
	; copy program address (A1+D1) into location
	move.l	A1,(A3)
	add.l	D1,(A3)
	rts
	
.PSS
	bsr	.get_slave_address
	move.w	(a0)+,d2
	SKIPIFFALSE
	move.w	#$4EB9,(A1,D1.L)
	move.l	A3,2(A1,D1.L)
	addq.l	#6,D1
	bra	.NOP_from_PSS
.ORB
	clr.w	d0
	move.w	(A0)+,D0
	SKIPIFFALSE
	or.b	D0,(A1,D1.L)
	rts
.ORW
	move.w	(A0)+,D0
	SKIPIFFALSE
	or.w	D0,(A1,D1.L)
	rts
.ORL
	move.l	(A0)+,D0
	SKIPIFFALSE
	or.l	D0,(A1,D1.L)
	rts
.AB
	clr.w	d0
	move.w	(A0)+,D0
	SKIPIFFALSE
	add.b	D0,(A1,D1.L)
	rts
.AW
	move.w	(A0)+,D0
	SKIPIFFALSE
	add.w	D0,(A1,D1.L)
	rts
.AL
	move.l	(A0)+,D0
	SKIPIFFALSE
	add.l	D0,(A1,D1.L)
	rts
.CZ
	move.w	(A0)+,D2
	SKIPIFFALSE
	subq.l	#1,D2
.czl
	clr.b	(A1,D1.L)
	addq.l	#1,D1
	dbf	D2,.czl
	rts
.CL
	SKIPIFFALSE
	clr.l	(A1,D1.L)
	rts
.CW
	SKIPIFFALSE
	clr.w	(A1,D1.L)
	rts
.CB
	SKIPIFFALSE
	clr.b	(A1,D1.L)
	rts
.S
	move.w	(A0)+,D2
	SKIPIFFALSE
	move.w	#$6000,(A1,D1.L)
	move.w	D2,2(A1,D1.L)
	rts
.NOP
	move.w	(A0)+,D2
	SKIPIFFALSE
.NOP_from_PSS
	lsr.w	#1,d2
	bne	.ncont
	rts	; safety
.ncont
	subq.w	#1,d2
.noploop
	move.w	#$4E71,(A1,D1.L)
	addq.l	#2,d1
	dbf		d2,.noploop
	rts
.I
	SKIPIFFALSE
	move.w	#$4AFC,(A1,D1.L)
	rts
.B
	move.w	(A0)+,D2
	SKIPIFFALSE
	move.b	D2,(A1,D1.L)
	rts

.W
	move.w	(A0)+,D2
	SKIPIFFALSE
	move.w	D2,(A1,D1.L)
	rts
.L
	move.l	(A0)+,D2
	SKIPIFFALSE
	move.l	D2,(A1,D1.L)
	rts
.DATA
	move.w	(A0)+,D0	; size
	beq.b	.exit
	SKIPIFFALSE
	move.w	d0,d2	
	subq.l	#1,D0
.dataloop
	move.b	(A0)+,(A1,D1.L)
	addq.l	#1,D1
	dbf		D0,.dataloop
	btst	#0,d2	; odd?
	beq.b	.exit
	addq.l	#1,a0
	rts
.NEXT	; V16 patchlist support
	bsr	.get_slave_address
	move.l	a3,a0		; next patchlist
	move.l	a3,a2		; store patchlist start
	rts
.UNSUP
	lsr.w	#1,D0
	bra	unsupported_patch_instruction
.exit
	rts

; <> A0: patch buffer (+=2 on exit)
; < A2: patch start
; > A3: real address of the routine in the slave
; D2 trashed

.get_slave_address:
	move.w	(A0)+,D2
	lea	(A2,D2.W),A3
	rts
	
unsupported_patch_instruction
	WHDLOAD_UNSUPPORTED_CALL	Patch,"Patch - unsupported command"

	; fake slave header, for OSEmu loaders

fake_slbase:
		dc.l	0		;security...
		dc.b	"WHDLOADS"	;id
		dc.w	8		;ws_Version (version 7 OK!)
		dc.w	0		;ws_flags (unused here)
fake_ws_basememsize:
		dc.l	$0		;ws_BaseMemSize	(the maxchip of SaveOSData)
		dc.l	$00		;ws_ExecInstall (unused here)
		dc.w	0		;ws_GameLoader (unused here)
		dc.w	0		;ws_CurrentDir (unused here)
		dc.w	0		;ws_DontCache (unused here)
 		dc.b	0
fake_ws_quitkey:
		dc.b	$5D		;quitkey (used here!)
fake_ws_expmemsize:
		dc.l	0		;ws_ExpMem (size of expmem required,
					; overwritten by JST with the expmem location)

WHD_Abort:
	RESTORE_REGS	D0
	cmp.l	#TDREASON_OK,D0		; normal termination
	beq	WHD_Exit
	cmp.l	#TDREASON_OSEMUFAIL,D0		; OSEMU FAIL
	beq	WHD_OsEmuFail

	cmp.l	#TDREASON_DOSREAD,D0
	beq	RunTime_FileRead

;;	cmp.l	#TDREASON_REQ68020,D0
;;	cmp.l	#TDREASON_REQAGA,D0
;;	cmp.l	#TDREASON_MUSTNTSC,D0

	cmp.l	#TDREASON_MUSTPAL,D0
	beq	RunTime_PalRequired

;;	cmp.l	#TDREASON_DOSWRITE,D0
;;	cmp.l	#TDREASON_DOSLIST,D0
;;	cmp.l	#TDREASON_DELETEFILE,D0

	cmp.l	#TDREASON_WRONGVER,D0
	beq	RunTime_WrongCRC

	cmp.l	#TDREASON_DISKLOAD,D0
	beq	RunTime_DiskRead

	cmp.l	#TDREASON_DEBUG,D0
	beq	RunTime_ExitDebug

	RUNTIME_ERROR_ROUTINE	UnknownExit,"WHDLoad abort (unknown reason)"

	RUNTIME_ERROR_ROUTINE	ExitDebug,"WHDLoad abort (debug)"

	RUNTIME_ERROR_ROUTINE	WrongCRC,"Unsupported game version"

	RUNTIME_ERROR_ROUTINE	PalRequired,"Needs PAL video mode"

	RUNTIME_ERROR_ROUTINE	WrongKickCRC,"Kickfile CRC mismatch"

WHD_Exit
	SET_VAR_CONTEXT
	TSTVAR_L	chipmirror
	bne RelFun_InGameExit       ; able to quit, memory has been mirrored
    ; can't quit, can't return, just reset
    ; reset the machine when quitting
    pea .sup(pc)
    move.l  (a7)+,$80
    trap    #0
.sup
	lea 2.W,A0
	RESET
	jmp (a0)    
    
WHD_OsEmuFail:
	RESTORE_REGS	D0
	SET_VAR_CONTEXT

	lea	WHDMessContext(pc),A5
	move.l	A5,D1
	JSRGEN	StrcpyAsm	; get the library which triggered
	LEAVAR	whd_sysunit,A5
	move.l	A5,D1
	JSRGEN	StrcpyAsm	; copy it elsewhere

	lea	WHDMessContext(pc),A5
	move.l	A5,D0
	JSRGEN	StrlenAsm
	move.b	#' ',(A5,D0.L)		; don't end the string
	move.b	#' ',1(A5,D0.L)		; don't end the string

	RESTORE_REGS	D1
	moveq.l	#0,D0
	sub.l	D1,D0			; -D1

	SETVAR_L	D1,whd_sysoffset

	lea	WHDMessOffset(pc),A1
	JSRGEN	ShortHexToString

	; store message pointer and return to OS with an error

	lea	WHDMessAbort(pc),A5
	bra	RTStoreMessage

;; fake disk io zone, allocated by OSEmu

fake_diskio:
	dc.l	0
is_whdload:
	dc.w	0

WHDMessUnsupported:
	dc.b	10,"Run-Time Error: Unsupported WHDLoad call: "
WHDMessUnsupported_Arg:
	blk.b	30,0
WHDMessAbort:
	dc.b	"WHDLoad abort: ",10,13,"Library: "
WHDMessContext:
	dc.b	"                         ",10,13
	dc.b	"Offset: -"
WHDMessOffset:
	dc.b	"                  ",10,13
	dc.b	0
	cnop	0,4

