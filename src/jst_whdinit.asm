DisplayWHDInfo:
	STORE_REGS
	move.l	#object_name,D0
	move.l	D0,A0
	move.l	D0,A1
	move.l	#tmpstring2,D1
	JSRGEN	StrcpyAsm

.rmdot
	move.b	(A0)+,D0
	beq	.out
	cmp.b	#'.',D0
	bne	.rmdot
	clr.b	-1(A0)
	bra	.rmdot
.out		
	JSRABS	Display
	PRINT_MSG	msg_HD_loader_po_WHDLoad

	RESTORE_REGS
	rts

;; check against 68020/AGA requirements

CheckHardwareReqs:
	btst	#WHDLB_NoError,D0
	beq.b	.nonfatal

	move.l	D0,-(A7)		; fixed a long time bug in whdload emulation in 2015 :)
	moveq.l	#-1,D0
	JSRGEN	SetRTFileError
	move.l	(A7)+,D0
.nonfatal
	SET_VAR_CONTEXT
;	TSTVAR_L	forcewhd_flag		; ignore all checks for badly written slaves
;	bne	.noagachk
	
	btst	#WHDLB_Req68020,D0
	beq	.no020chk

	JSRGEN	GetAttnFlags
	btst	#AFB_68020,D0
	beq	WHDCpuError
.no020chk

	btst	#WHDLB_ReqAGA,D0
	beq	.noagachk
	
	JSRGEN	CheckAGA
	tst.l	D0
	bne	WHDAgaError
.noagachk
	rts

; init and call WHDLoad slave

InitWHDSlave:
	SET_VAR_CONTEXT

	; build table for raw/ascii equivalents

	jsr	MakeRawTable

	TSTVAR_L	quiet_flag
	bne.b	.skip_whdinfo
	bsr	DisplayWHDInfo
.skip_whdinfo

	; do the stuff necessary for WHDLoad loaders

	move.l	object_entry(pc),A1
	moveq.l	#0,D0
	move.w	ws_Flags(A1),D0

	bsr	CheckHardwareReqs

	move.w	ws_Version(A1),D0
	cmp.w	#8,D0
	bcs.b	.noext				; not supported before version 8!

	move.l	ws_ExpMem(A1),D0
	beq.b	.noext				; no expansion memory required
	; optional memory not supported, since it would eat a lot of mem
	; maybe too much for preloading files afterwards
	; I think expmem should be allocated in the end, after file preloading
	; but right now that's not gonna happen.
	bmi	.noext			
	
	JSRABS	AllocExtMem
	move.l	D0,ws_ExpMem(A1)		; ext memory replaces ext size!
	beq	FastErr				; no extension memory!
	; now check to see if a kickstart must be loaded
	
	moveq.l	#0,D0
	move.w	ws_Version(A1),D0
	cmp.w	#16,D0
	bcs.b	.noext				; not supported before version 16

	bsr	LoadKick

.noext
	; set load directory
	
	LEAVAR	loaddata_dir,a0
	tst.b	(a0)
	bne.b	.skipsfp	; already set through command line: skip
	move.l	A1,A0	; object pointer
	moveq.l	#0,d0
	move.w	ws_CurrentDir(A1),D0
	beq	.skipsfp
	add.l	D0,A0

	JSRABS	SetFilesPath
.skipsfp
	
.loadfiles
	move.l	#10000,D0
	JSRABS	LoadSmallFiles

.rungame
	TSTVAR_L	test_flag
	beq	.nodo
	Mac_printf	"Test mode, exiting"
	JMPABS	CloseAll
.nodo
	moveq.l	#0,D0
	moveq.l	#0,D1
	JSRABS	Degrade


	move.l	object_entry(pc),A1

	move.w	ws_Version(A1),D0
	cmp.w	#4,D0
	bcs	.noautokey		; not supported before version 4!

	GETVAR_B	quitkeynum,D0
	bne.b	.setk			; auto quit key defined
	move.b	ws_keyexit(A1),D0	; bugfix in v2.4
.setk
	move.b	D0,ws_keyexit(A1)	; bugfix in v2.4
	SUB.L	A0,A0
	JSRGEN	SetQuitKey		; installs auto quit key

	
	tst.b	ws_keydebug(A1)
	bne.b	.noautokey

	move.b	#$7F,ws_keydebug(A1)		; sets to $7F (v4.0, Cannon Fodder 2)
.noautokey:

	SETVAR_L	object_entry(pc),object_entry

	GETVAR_L	gene_patchbuffer,A0

	; adds function table offset + function offset

	ADD.L	#Rel_RelFunTable+RelOff_Priv_WHDStart,A0

	move.l	(A0),A0
	move.l	A0,-(A7)		; generic slave init start


	move.l	ws_BaseMemSize(A1),D0
	JMPABS	SaveOSData		; save OS data & jump to slave
	
RelocKickstart:	
	include		"reloc_kickstart.asm"

	; < A1: whdload base
	
LoadKick:
	STORE_REGS
	move.l	A1,A5
	
	moveq.l	#0,D0
	move.w	ws_kickname(A5),D0
	beq	.nokick

	add.l	A5,D0

	move.w	#$FFFF,d1
	cmp.w	ws_kickcrc(A5),D1
	bne		.single_kick
	
	; new "multi-kick" system introduced to support A1200/A4000/A600 kick 3.1 files
	; D0 points to a table CRC,kicksuffix ... 0
	; we have to loop until we find an existing file
	move.l	d0,a2	; table

.kloop
	move.l	#kickstart_suffix,D1
	move.w	(a2),d2	; check CRC=0: end of list
	beq	.notfound
	moveq.l	#0,d0
	move.w	2(a2),d0
	add.l	A5,d0		; absolute pointer to the name
	jsr	RelFun_StrcpyAsm		; compose full filename
	; now test if file exists
	tst.l	verbose_flag
	beq.b	.tk
	Mac_print	"-> Scan kickfile "
	move.l	#kickstart_filename,A1
	JSRABS	Display
	Mac_print	", CRC16="
	Mac_printh	D2

.tk
	move.l	#kickstart_filename,D0
	JSRABS	TestFileAbs
	tst.l	d0
	beq.b	.found
	; not that one
	addq.l	#4,a2
	bra	.kloop
.notfound
; no matching kick file: error
	Mac_printf	"Could not load any kick3.1 file from DEVS:Kickstarts"
	JMPABS	CloseAll
.found
; the name is copied ok, copy the CRC
	move.w	(a2),ws_kickcrc(A5)
	bra.b	.all_kick	; continue
.single_kick

	SET_VAR_CONTEXT
	move.l	#kickstart_suffix,D1
	jsr	RelFun_StrcpyAsm		; compose full filename
.all_kick
	tst.l	verbose_flag
	beq.b	.nov
	Mac_print	"-> Loading kickstart file "
	move.l	#kickstart_filename,A1
	JSRABS	Display
.nov
	move.l	D1,D0
	jsr	RelFun_StrlenAsm
	move.l	D1,A0
	add.l	D0,A0
	move.l	A0,rtb_suffix_ptr
	; add .RTB suffix
	move.b	#'.',(a0)+
	move.b	#'R',(a0)+
	move.b	#'T',(a0)+
	move.b	#'B',(a0)+
	move.b	#0,(a0)
	move.l	#kickstart_filename,D0
	; get RTB size
	JSRABS	GetFileLengthAbs
	move.l	D0,D4
	bmi	.nortb
	
	move.l	ws_kicksize(A5),D1
	SETVAR_L	D1,kicksize
	tst.l	verbose_flag
	beq.b	.nov2
	Mac_print " size="
	Mac_printh	D1
.nov2

	; use allocated memory for .RTB file only
	moveq.l	#0,D1
	move.l	D4,D0	; alloc kicksize
	move.l	_SysBase,A6
	JSRLIB	AllocMem
	move.l	D0,A3		; save to D2 
	tst.l	d0
	; TODO if 0 alloc error
	
	; now read the .RTB file
	move.l	#kickstart_filename,D1
	move.l	#MODE_OLDFILE,D2
	move.l	_DosBase,A6
	JSRLIB	Open
	move.l	D0,D6
	beq	.nortb	; very unlikely!

	; ** Read the RTB file first
	move.l	D6,D1
	move.l	A3,D2	; allocated buffer
	move.l	D4,D3	; read all file
	JSRLIB	Read
	; ** Close the .RTB file
	move.l	D6,D1
	JSRLIB	Close

	; now read kickstart file into expansion memory (remove .RTB suffix)
	move.l	rtb_suffix_ptr(pc),A0
	clr.b	(A0)
	move.l	#kickstart_filename,D1
	move.l	#MODE_OLDFILE,D2
	move.l	_DosBase,A6
	JSRLIB	Open
	move.l	D0,D6
	beq	.nortb
	; ** Read the file
	move.l	D6,D1
	GETVAR_L	kicksize,D3	; read all file
	GETVAR_L	extbuf,D2
	JSRLIB	Read
	; ** Close the kickstart file
	move.l	D6,D1
	JSRLIB	Close

	; perform CRC
	
	GETVAR_L	extbuf,A0
	GETVAR_L	kicksize,d0
	JSRGEN	CRC16
	moveq.l	#0,d5
	cmp.w	ws_kickcrc(A5),D0  ; checks with provided CRC
	bne	.wrong_kick_crc

	; now relocate (A3 already contains RTB pointer)
	GETVAR_L	kicksize,d3
	bsr	RelocKickstart

	GETVAR_L	extbuf,A0

	
	; now free RTB memory
	move.l	A3,A1
	move.l	D4,D0
	move.l	_SysBase,A6
	JSRLIB	FreeMem

.nokick	
	RESTORE_REGS
	rts
	
.kickerr
	LEA	kickstart_filename(pc),A1
	JSRABS	Display
	Mac_printf	"'"
	bra	AbsFun_CloseAll
.nortb
	Mac_print	"RTB file not found: '"
	bra.b	.kickerr
.wrong_kick_crc:
	Mac_print	"Wrong kickstart CRC. Expected "
	Mac_printh	D5
	Mac_print   " found "
	Mac_printh	D0
	Mac_print " for file "
	bra	.kickerr

rtb_suffix_ptr:
	dc.l	0
kickstart_filename:
	dc.b	"DEVS:Kickstarts/Kick"
kickstart_suffix:
	blk.b	24,0

	
ComputeDiskData:
	STORE_REGS	
	SET_VAR_CONTEXT

	move.l	#whddiskname,D0
	move.l	#dskstring,D1
	JSRGEN	StrcpyAsm

	clr.l	nbdisks	
	lea	dskstring(pc),A2
	lea	5(A2),A3			; disk. length

.loop
	move.l	nbdisks,D0
	add.l	#'1',D0
	move.b	D0,(A3)
	clr.b	1(A3)

	move.l	A2,D0
	JSRABS	GetFileLength
	tst.l	D0
	bmi.b	.nomore

	add.l	#1,nbdisks

	cmp.l	(RelVar_filesize,A4),D0
	bcs.b	.nosup
	SETVAR_L	D0,filesize

.nosup
	bra.b	.loop
.nomore
	RESTORE_REGS
	rts

dskstring:
	blk.l	10,0


WHDCpuError:
	PRINT_MSG	msg_At_least_a_is_requir
	JMPABS	CloseAll

WHDAgaError:
	PRINT_MSG	msg_A_AGA_computer_is_re
	JMPABS	CloseAll

tmpstring2:
	blk.b	100,0

; labels

msg_HD_loader_po_WHDLoad:	
	dc.b	" HD loader (WHDLoad)",10
	dc.b	"JST Using WHDLoad emulation interface",10,0

msg_At_least_a_is_requir:	
	dc.b	"** At least a 68020 is required to run this WHDLoad loader (use FORCEWHD to override)",10,0
msg_A_AGA_computer_is_re:	
	dc.b	"** A AGA computer is required to run this WHDLoad loader (use FORCEWHD to override)",10,0
	even
