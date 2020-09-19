MISC_LEN = $80+$100

READFILE:MACRO
	bsr	ReadSnapshotFile
	ENDM

; MOVABS: transfer pointers to previously allocated memory
; (i.e. during the session where the snapshot was made)
; into a buffer, and clears the pointer
; The ALLOC_ABS_OR_DYN macro will check if \1_abs is != 0 and
; if so, will try to AllocAbs the zone
; if \1_abs is == 0, then, a normal AllocMem() will be performed

MOVABS:MACRO
	GETVAR_L	\1,\1_abs
	CLRVAR_L	\1
	ENDM

; < A0: filename
; < A1: buffer
; < D1: length

WriteSnapshotFile:
	STORE_REGS	D1-D7/A0-A6
	SET_VAR_CONTEXT

	; the OS won't be trashed at once (disable reread and delay)

	CLRVAR_L	ostotrash

	bsr	AppendObjectName
	moveq.l	#0,D0
	bsr	AbsFun_Priv_WriteUserFileHD
	RESTORE_REGS	D1-D7/A0-A6
	rts

ReadSnapshotFile:
	STORE_REGS	D1-D7/A0-A6
	bsr	AppendObjectName
	bsr	AbsFun_Priv_ReadUserFileHD
	RESTORE_REGS	D1-D7/A0-A6
	rts

AppendObjectName:
	STORE_REGS	D0-D1
	move.l	#object_name,D0		; specific loader name
	move.l	#snapfile_name,D1	; buffer for new filename
	jsr	RelFun_StrcpyAsm	; prepare prefix
	move.l	#object_name,D0
	jsr	RelFun_StrlenAsm	; get length

	move.l	#snapfile_name,D1	; buffer for new filename
	add.l	D0,D1			; end of buffer
	move.l	A0,D0			; suffix

	jsr	RelFun_StrcpyAsm	; copy suffix - name done

	lea	snapfile_name(pc),A0	; change name

	RESTORE_REGS	D0-D1
	rts

; < D0: object CRC
; < A4: variables block
; > nothing, but can fail if CRC does not match (RESUME!)

SetObjectCRC:
	TSTVAR_L	resume_flag
	beq.b	.exit		; RESUME not set, exit

	; RESUME is set: check current object CRC against
	; CRC stored in the snapshot file: they MUST match

	STORE_REGS	D1
	GETVAR_W	object_crc,D1
	cmp.w	D0,D1
	bne	sna_ObjectError	; object CRC does not match
	RESTORE_REGS	D1

.exit
	; sets object CRC

	SETVAR_W	D0,object_crc
	rts

ResumeFrozen:
	SET_VAR_CONTEXT

	TSTVAR_L	resume_flag
	beq	.exit		; RESUME not set!

	STORE_REGS
	bsr	load_snapshot_1	; load snapshot, first part
	RESTORE_REGS

.exit
	rts

ResumeFrozen_2:
	SET_VAR_CONTEXT

	TSTVAR_L	resume_flag
	beq	.exit		; RESUME not set!

	STORE_REGS

	bsr	load_snapshot_2	; load snapshot, second part

	RESTORE_REGS

.exit
	rts

savecall:
	dc.l	0

ReloadCommand:
	bsr	ResumeFrozen
	bsr	ResumeFrozen_2
	jmp	CommandRetry	

SaveCommand:
	tst.l	snapshot_forbidden
	bne	.exit

	bsr	save_snapshot
.exit
	jmp	CommandRetry	
	

save_snapshot:
	SET_VAR_CONTEXT

	; the OS won't be trashed at once (disable reread and delay)

	CLRVAR_L	ostotrash

	SETVAR_L	snapshot_version,snapshot_version

	; 1: save the hardware information (and misc info)

	LEAVAR	start_snapshot,A1	; start address
	LEAVAR	end_snapshot,A0
	sub.l	A1,A0
	move.l	A0,D0			; length

	move.l	D0,D1
	lea	SysInfo_Name(pc),A0
	bsr	WriteSnapshotFile
	tst.l	D0
	bne	sna_WriteError

	; 2: save chipmem block

	GETVAR_L	chipsize,D1
	beq	sna_NoChipError

	GETVAR_L	chipmirror,A1
	lea	ChipMem_Name(pc),A0
	bsr	WriteSnapshotFile

	; 3: save fastmem block (if any)

	GETVAR_L	extsize,D1
	beq.b	.noext

	GETVAR_L	extbuf,A1
	lea	ExtMem_Name(pc),A0
	bsr	WriteSnapshotFile

.noext
	; 3: save fastmem block (if any)

	GETVAR_L	bit24size,D1
	beq.b	.no24b

	GETVAR_L	bit24buf,A1
	lea	Ext24Mem_Name(pc),A0
	bsr	WriteSnapshotFile

.no24b
	; save user variables (if any)

	tst.l	uservar_start
	beq	.nousrvar

	move.l	uservar_start(pc),A1
	move.l	uservar_end(pc),D1
	sub.l	A1,D1			; length
	lea	UserMem_Name(pc),A0
	bsr	WriteSnapshotFile
	tst.l	D0
	bne	sna_WriteError

.nousrvar
	; the OS will be trashed soon (just after the last (small) write)
	
	SETVAR_L	#1,ostotrash

	rts

sna_WriteError:
	PRINT_MSG	msg_Write_error_
	rts

	; impossible but...
sna_NoChipError:
	PRINT_MSG	msg_No_chipmem_
	rts

load_snapshot_1:
	SET_VAR_CONTEXT

	; 1: load the hardware information (and misc info)

	LEAVAR	start_snapshot,A1
	LEAVAR	end_snapshot,A0
	move.l	A0,D1
	sub.l	A1,D1			; length to read
	moveq.l	#-1,D0			; command: read without offset
	lea	SysInfo_Name(pc),A0
	READFILE
	tst.l	D0
	bne	sna_ReadError

	GETVAR_L	snapshot_version,D1
	cmp.l		snapshot_version,D1	; version test (forget regged/limited info)
	bne	sna_VersionError

	TSTVAR_W	snapmem_allocated
	bne.b	.skipthis

	SETVAR_W	#1,snapmem_allocated

	; clear some allocated buffers (save them in _abs pointers)
	
	MOVABS	bit24buf
	MOVABS	gene_patchbuffer
	MOVABS	object_ptr
	MOVABS	mmucode_ptr
	MOVABS	extbuf
.skipthis
	rts


load_snapshot_2:
	SET_VAR_CONTEXT

	; 2: load chipmem block

	GETVAR_L	chipsize,D1
	beq	sna_NoChipError

	GETVAR_L	chipmirror,A1
	lea	ChipMem_Name(pc),A0
	moveq	#-1,d0
	READFILE

	; 3: load fastmem block (if any)

	GETVAR_L	extsize,D1
	beq	.noext

	GETVAR_L	extbuf,A1
	lea	ExtMem_Name(pc),A0
	moveq	#-1,d0
	READFILE

.noext
	; 4: load 24 bit mem block (if any)

	GETVAR_L	bit24size,D1
	beq.b	.no24b

	GETVAR_L	bit24buf,A1
	lea	Ext24Mem_Name(pc),A0
	moveq	#-1,d0
	READFILE
	tst.l	D0
	bne	sna_ReadError

.no24b

	; 5: load user variables (if any)

	tst.l	uservar_start
	beq	.nousrvar

	move.l	uservar_start(pc),A1
	move.l	uservar_end(pc),D1
	sub.l	A1,D1			; length
	moveq.l	#-1,D0			; command: read without offset
	lea	UserMem_Name(pc),A0
	READFILE
	tst.l	D0
	bne	sna_ReadError

.nousrvar
	rts

sna_ReadError:
	PRINT_MSG	msg_Cannot_find_one_of_t
	bra	sna_CommonError

sna_VersionError:
	PRINT_MSG	msg_Snapshot_is_incompat
	bra	sna_CommonError

sna_ObjectError:
	PRINT_MSG	msg_Snapshot_isn_t_compa
	bra	sna_CommonError

sna_CommonError:
	PRINT_MSG	msg_RESUME_failed_
	JSRABS	Enhance
	JMPABS	CloseAll

	; system/hardware info buffer

SysInfo_Name:
	dc.b	"_sna.sys",0

	; chipmem buffer

ChipMem_Name:
	dc.b	"_sna.chp",0

	; extension mem buffer

ExtMem_Name:
	dc.b	"_sna.fst",0

	; 24 bit memory buffer

Ext24Mem_Name:
	dc.b	"_sna.24b",0

	; absolute memory configuration

MiscMem_Name:
	dc.b	"_sna.cfg",0

UserMem_Name:
	dc.b	"_sna.usr",0

	; buffer for filename

snapfile_name:
	blk.b	108,0

	cnop	0,4
snapshot_forbidden:
	dc.l	0

uservar_start:
	dc.l	0

uservar_end:
	dc.l	0

snapshot_version
	dc.b	"4.0  "	; V4.0
	even
; labels

msg_Write_error_:	
	dc.b	"** Write error!",10,13,0
msg_No_chipmem_:	
	dc.b	"** No chipmem??????",10,13,0
msg_Cannot_find_one_of_t:	
	dc.b	"** Cannot find one of the snapshot files!",10,13,0
msg_Snapshot_is_incompat:	
	dc.b	"** Snapshot is incompatible with this version of JST!",10,13,0
msg_Snapshot_isn_t_compa:	
	dc.b	"** Snapshot is has not been made with this version of the object!",10,13,0
msg_RESUME_failed_:	
	dc.b	"   RESUME failed!",10,13,0
	even

MiscBuffer:
	blk.l	MISC_LEN,0
