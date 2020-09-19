	IFND	GP_MACROS_I_INCLUDED
GP_MACROS_I_INCLUDED	=	1

	;MC68000

; ** JST version identifier (to avoid the use of obsolete versions)

CURRENT_VERSION_ID = 20

; ** currently used disk sizes

STD_DISK_SIZE = 901120	; standard dos copiable disks
B12_DISK_SIZE = 970752	; 12 sectored 79 tracks disks, dos bootblock
S12_DISK_SIZE = 983040	; 12 sectored 79 tracks disks, dos bootblock

; ** Tooltype/argument definitions

; bits

AFB_NTSC = 0
AFB_LOWMEM = 1
AFB_HDLOAD = 2
AFB_TRAINER = 3
AFB_NOOSSWAP = 4
AFB_JOYPAD = 5
AFB_PAL = 6

; ** joypad button definitions (for JoyButtonsState)

; ** masks

AFF_FIRE1 = $20		; red joypad button
AFF_START = $01		; start/pause joypad button
AFF_FIRE2 = $40		; blue joypad button
AFF_FIRE4 = $08		; green joypad button
AFF_FIRE3 = $10		; yellow joypad button
AFF_FORWD = $04		; forward joypad key
AFF_BACWD = $02		; back joypad key

; ** bits

AFB_START = $0		; start/pause joypad button
AFB_BACWD = $1		; back joypad key
AFB_FORWD = $2		; forward joypad key
AFB_FIRE4 = $3		; green joypad button
AFB_FIRE3 = $4		; yellow joypad button
AFB_FIRE1 = $5		; red joypad button
AFB_FIRE2 = $6		; blue joypad button


STORE_REGS: MACRO
	IFLE	NARG
	movem.l	D0-D7/A0-A6,-(A7)
	ELSE
	movem.l	\1,-(A7)
	ENDC
	ENDM

RESTORE_REGS: MACRO
	IFLE	NARG
	movem.l	(A7)+,D0-D7/A0-A6
	ELSE
	movem.l	(A7)+,\1
	ENDC
	ENDM

WAIT_LMB: MACRO
.wl\@
	btst	#6,$BFE001
	bne	.wl\@
	ENDM

WAIT_JOY: MACRO
.wj\@
	btst	#7,$BFE001
	bne	.wj\@
	ENDM

GETLVO:MACRO
	move.l	#_LVO\1,D0
	ENDM

; ******* Print/Printf ********
; Because of buggy Barfly macro argument count,
; I had to make 2 functions for BARFLY for each Mac_printf and Mac_printh
;
; Mac_printf : same as before
; Mac_print  : no linefeed (same as Mac_printf "string",*anything* , except
; Barfly does not work properly with it, and as I now use Barfly for JST
; it's annoying)
;
; Mac_printh : same as before
; Mac_printx : Mac_printh without linefeed

	IFD	BARFLY

	IFND	WHDLOADSLAVE

Mac_print: MACRO

	move.l	A1,-(A7)
	lea	.text\@(PC),A1
	JSRABS	Display

	bra.b	.ftext\@
.text\@
	dc.b	'\1',0
	even

.ftext\@
	move.l	(A7)+,A1
	ENDM

Mac_printf: MACRO
	Mac_print	<\1>
	NEWLINE
	ENDM

Mac_printh: MACRO
	Mac_printx	\1
	NEWLINE
	ENDM

Mac_printx:MACRO
	movem.l	D0/A1,-(A7)
	move.l	\1,D0
	lea	.text\@(PC),A1
	JSRABS	HexToString
	JSRABS	Display
	bra.b	.ftext\@

.text\@
	dc.b	"$00000000",0
	even
.ftext\@
	movem.l	(A7)+,D0/A1
	ENDM

	ELSE	
	; whdload slave: no messages

Mac_printf:MACRO
	ENDM
Mac_print:MACRO
	ENDM
Mac_printh:MACRO
	ENDM



	ENDC

	ELSE

; *** macro definitions for other assemblers than Barfly

; ******* Macro Printf ********
; Mac_printf "text"   -> text + linefeed
; Mac_print "text" -> text without linefeed

Mac_printf:MACRO
	move.l	A1,-(A7)
	lea	.text\@(PC),A1
	JSRABS	Display
	bra.b	.ftext\@
.text\@
	IFD	MAXON_ASM
		dc.b	"\1"
	ELSE
		IFD	BARFLY
;;			dc.b	"\1"
		ELSE
			DC.B	\1
		ENDIF
	ENDIF

	dc.b	10,13
	dc.b	0
	even

.ftext\@
	move.l	(A7)+,A1
	ENDM

Mac_print:MACRO
	move.l	A1,-(A7)
	lea	.text\@(PC),A1
	JSRABS	Display
	bra.b	.ftext\@
.text\@
	IFD	MAXON_ASM
	dc.b	"\1"
	ELSE
		IFD	BARFLY
;;			dc.b	"\1"
		ELSE
			dc.b	\1
		ENDIF
	ENDIF
	dc.b	0
	even
.ftext\@
	move.l	(A7)+,A1
	ENDM

; ******* Macro Printh ********
; Mac_printh <expr>   -> hex number + linefeed
; Mac_printh <expr>,*any argument* -> without linefeed

Mac_printh: MACRO
	Mac_printx	\1
	NEWLINE
	ENDM

Mac_printx:MACRO
	movem.l	D0/A1,-(A7)
	move.l	\1,D0
	lea	.text\@(PC),A1
	JSRABS	HexToString
	JSRABS	Display
	bra.b	.ftext\@

.text\@
	dc.b	"$00000000",0
	even
.ftext\@
	movem.l	(A7)+,D0/A1
	ENDM

	ENDC

PUTS: MACRO
	move.l	A1,-(A7)
	lea	\1,A1
	JSRABS	Display
	move.l	(A7)+,A1
	ENDM

NEWLINE: MACRO
	JSRABS	NewLine
	ENDM



STOP_SOUND:MACRO
	move.w	#$000F,dmacon+$DFF000
	move.w	#$0000,aud0+ac_len+$DFF000
	move.w	#$0000,aud1+ac_len+$DFF000
	move.w	#$0000,aud2+ac_len+$DFF000
	move.w	#$0000,aud3+ac_len+$DFF000
	ENDM

	; *** Non-relocated routines. call normally with JSR
	; *** don't call them with JSRGEN, this would not compile (better than crash)

MAKE_ABS_REFS:MACRO

	DEF_\1_ABS	CloseAll,0		; quit program (while the OS is up)
	DEF_\1_ABS	CloseAllQuiet,1		; quit program and close window
	DEF_\1_ABS	LoadDisks,2		; load diskfiles
	DEF_\1_ABS	LoadDiskFromName,3	; load a disk
	DEF_\1_ABS	LoadDisksIndex,4	; load a disk, starting from number D0
	DEF_\1_ABS	LoadFiles,5		; load files in the directory
	DEF_\1_ABS	LoadSmallFiles,6	; load small files (<D0)
	DEF_\1_ABS	Kick37Test,7		; check if Kickstart version <37
	DEF_\1_ABS	KickVerTest,8		; check against a given kick version
	DEF_\1_ABS	GetMemFlag,9		; returns MEMF_REVERSE flag isf available
	DEF_\1_ABS	FlushCachesSys,10	; flush caches using CacheClearU()
	DEF_\1_ABS	Enhance,11		; restores everything (caches, display)
	DEF_\1_ABS	Degrade,12		; degrades everything
	DEF_\1_ABS	Unsupported1,13		; future use
	DEF_\1_ABS	Unsupported2,14		; future use
	DEF_\1_ABS	TransfRoutines,15	; *** obsolete ***
	DEF_\1_ABS	SaveOSData,16		; saves OS data (mem,CIAs,custom...)
	DEF_\1_ABS	TestFile,17		; check if a file is here (game path)
	DEF_\1_ABS	Display,18		; display a string pointed by A1
	DEF_\1_ABS	LoadRNCFile,19		; load a RNC file and unpack it
	DEF_\1_ABS	AllocExtMem,20		; allocate extension memory
	DEF_\1_ABS	Test1MBChip,21		; check against 1MB of chip (at least)
	DEF_\1_ABS	Test2MBChip,22		; check against 2MB of chip
	DEF_\1_ABS	Reboot,23		; reboots :-), only works when OS is alive!
	DEF_\1_ABS	WaitReturn,24		; waits for the user to press return key
	DEF_\1_ABS	BlockFastMem,25		; * allocates all fastmem. Do not use :-)
	DEF_\1_ABS	CheckFastMem,26		; checks if the computer has got fast memory
	DEF_\1_ABS	SupervisorMode,27	; goes into supervisor mode
	DEF_\1_ABS	UserMode,28		; goes into user mode
	DEF_\1_ABS	Alloc24BitMem,29	; allocate extension memory, 24 bit area only
	DEF_\1_ABS	OpenFakeExec,30		; allocate table for fake exec (obsolete, will exit)
	DEF_\1_ABS	SetFilesPath,31		; change default file path
	DEF_\1_ABS	TestDirectory,32	; test directory presence (game path)
	DEF_\1_ABS	TestDirectoryAbs,33	; test directory presence (absolute path)
	DEF_\1_ABS	TestFileAbs,34		; test file presence (absolute path)
	DEF_\1_ABS	SetLocalVarZone,35	; save start-end of local object variables
	DEF_\1_ABS	InitLogPatch,36		; initialize patch logging (private function)
	DEF_\1_ABS	TestAssign,37		; test assign presence
	DEF_\1_ABS	AllocateTheMemory,38	; AllocMem with ressource-tracking
	DEF_\1_ABS	FreeTheMemory,39	; AllocMem with ressource-tracking
	DEF_\1_ABS	HexToString,40		; HexToString, absolute call
	DEF_\1_ABS	UseHarryOSEmu,41	; Tell JST to use Harry's great OS emu
	DEF_\1_ABS	NewLine,42		; Prints a newline
	DEF_\1_ABS	DisableChipmemGap,43	; Will allow LOWMEM with 2MB chip
	DEF_\1_ABS	GetFileLength,44	; Returns the length of a file on disk

	ENDM
	
	; *** Relocated routines. always call with JSRGEN (see macros.i)
	; *** from user program. It works with JSR but if the OS is killed
	; *** JSRGEN is safer as it jumps in the allocated block
	; *** which is in the top of memory (MEMF_REVERSE) if kick > 38
	; *** fast memory is used when found, else chipmem is used
	; ***
	; *** I also included a short description of the routines
	; *** Please read the autodocs to get more details
	
	; *** The functions marked with a * should not be used anymore

MAKE_REL_REFS:MACRO
	DEF_\1_REL	GetSR,0			; returns SR in D0
	DEF_\1_REL	GoECS,1			; useless - resets sprite aspect/playfield/goes 15KHz
	DEF_\1_REL	GetAttnFlags,2		; gets system backuped AttnFlags, at any time
	DEF_\1_REL	ResetDisplay,3		; switches in PAL or NTSC if specified
	DEF_\1_REL	ResetSprites,4		; resets sprite aspect, useless, dummy!
	DEF_\1_REL	BlackScreen,5		; sets all color registers to black
	DEF_\1_REL	JoypadState,6		; to check the state of joystick/joypad
	DEF_\1_REL	InitTrackDisk,7		; returns a fake disk IO pointer in A1
	DEF_\1_REL	TrackLoad,8		; simulates DoIO()
	DEF_\1_REL	TrackLoadFast,9		; * same routine. Please use the TrackLoad name
	DEF_\1_REL	SetTDUnit,10		; sets drive unit in D0 (for DoIO emulation)
	DEF_\1_REL	SetDisk,11		; sets current disk (useful with HDLOAD)
	DEF_\1_REL	ReadRobSectors,12	; reads sectors, rob northen interface
	DEF_\1_REL	ReadRobSectorsFast,13	; * same routine. Use ReadRobSectors instead
	DEF_\1_REL	ReadFile,14		; reads file, rob northen interface, from RAM or HD
	DEF_\1_REL	ReadFileFast,15		; reads file, rob northen interface, from RAM only
	DEF_\1_REL	ReadFilePartHD,16	; partially reads file, modified rob northen interface, from HD only
	DEF_\1_REL	ReadDiskPart,17		; reads parts of a disk, from RAM or HD (LOWMEM decides)
	DEF_\1_REL	ReadFileHD,18		; reads file, rob northen interface, from HD only
	DEF_\1_REL	WriteFileHD,19		; writes file on HD during game
	DEF_\1_REL	DeleteFileHD,20		; deletes file from HD during game (careful :-))
	DEF_\1_REL	Unsupported3,21		; decrunches PowerPacker files/blocks
	DEF_\1_REL	ImploderDecrunch,22	; decrunches Imploder files/blocks
	DEF_\1_REL	ATNDecrunch,23		; decrunches ATN! files/blocks
	DEF_\1_REL	RNCDecrunch,24		; decrunches RNC\01,\02 files/blocks
	DEF_\1_REL	Unsupported4,25	; decrunches Gremlins packer files/blocks
	DEF_\1_REL	RNCDecrunchEncrypted,26	; decrunches RNC/01 encrypted files/blocks
	DEF_\1_REL	RNCLength,27		; gives the length of decunched RNC file
	DEF_\1_REL	FlushCachesHard,28	; flushes all caches
	DEF_\1_REL	WaitBlit,29		; waits till blitter operation is complete
	DEF_\1_REL	BeamDelay,30		; waits using $DFF006 register (beam)
	DEF_\1_REL	InGameOSCall,31		; calls a user OS routine during the game
	DEF_\1_REL	PatchExceptions,32	; private - install JOTD exception handler (only exceptions)
	DEF_\1_REL	StrcmpAsm,33		; compares 2 strings (null termintated). Not case sensitive
	DEF_\1_REL	StrcpyAsm,34		; copies a string (null terminated)
	DEF_\1_REL	StrlenAsm,35		; returns length of a string (ends with null)
	DEF_\1_REL	ToUpperAsm,36		; converts a string in upper case
	DEF_\1_REL	WaitMouse,37		; waits for LMB to be pressed. Colors fill the screen
	DEF_\1_REL	WaitMouseInterrupt,38	; same thing but interrupts are enabled (HRTMon)
	DEF_\1_REL	GetDiskPointer,39	; gets start of the specified diskfile (D0) (can be avoided)
	DEF_\1_REL	CheckAGA,40		; checks if the computer is AGA using DeniseId
	DEF_\1_REL	SetExitRoutine,41	; to call a user routine just before exit
	DEF_\1_REL	InGameExit,42		; exits to WB from game at any time
	DEF_\1_REL	SaveCustomRegs,43	; private - saves important custom registers
	DEF_\1_REL	RestoreCustomRegs,44	; private - restores them
	DEF_\1_REL	SaveCIARegs,45		; private - saves all CIA info possible and LED state
	DEF_\1_REL	RestoreCIARegs,46	; private - restores them
	DEF_\1_REL	FreezeAll,47		; clears all ints, dma...
	DEF_\1_REL	ReadUserFileHD,48	; reads file, from HD in user SAVEDIR directory
	DEF_\1_REL	WriteUserFileHD,49	; writes file on HD in user SAVEDIR directory
	DEF_\1_REL	DeleteUserFileHD,50	; removes file from HD in user SAVEDIR directory
	DEF_\1_REL	TPWMDecrunch,51		; decrunches TPWM files/blocks (experimental)
	DEF_\1_REL	WaitBOF,52		; waits bottom of frame, hardware coded. not working, useless
	DEF_\1_REL	ReadUserDir,53		; reads user directory in a robdir structure
	DEF_\1_REL	PatchZeroPage,54	; private - install JOTD exception handler (done in SaveOSData)
	DEF_\1_REL	Unsupported5,55		; decrunch FIRE packer files/blocks
	DEF_\1_REL	HexReplaceLong,56	; search/replace a longword in a zone
	DEF_\1_REL	HexReplaceWord,57	; search/replace a word in a zone
	DEF_\1_REL	HexToDecString,58	; converts hexadecimal to decimal string
	DEF_\1_REL	GetUserData,59		; returns userdata string
	DEF_\1_REL	StrncpyAsm,60		; strncpy, D2 limits string length
	DEF_\1_REL	PatchMoveCList_Idx,61	; patches move.l #adr,($80,Ax) 
	DEF_\1_REL	StoreCopperPointer,62	; stores manually copper address
	DEF_\1_REL	TellCopperPointer,63	; returns previously stored copper address
	DEF_\1_REL	InGameIconify,64	; iconifies the game
	DEF_\1_REL	PatchMoveCList_Abs,65	; patches move.l #adr,($DFF080) 
	DEF_\1_REL	PatchMoveCList_Ind,66	; patches move.l adr,($DFF080) 
	DEF_\1_REL	LogPatch,67		; store before-patch information
	DEF_\1_REL	PatchMoveBlit_Idx,68	; patches move.w #blit,($58,Ax)
	DEF_\1_REL	HexToString,69		; hex to string conversion
	DEF_\1_REL	IsRegistered,70		; check to see if JST is registered
	DEF_\1_REL	SetQuitKey,71		; set the key for auto quit
	DEF_\1_REL	SetIconifyKey,72	; set the key for auto iconify
	DEF_\1_REL	ReadFilePart,73		; partially reads file, modified rob northen interface
	DEF_\1_REL	StrncmpAsm,74		; partially compares 2 strings UC=LC
	DEF_\1_REL	CRC16,75		; calculates CRC16 for a block
	DEF_\1_REL	Unsupported6,76		; decrunches a TSM! file, not working!
	DEF_\1_REL	EnterDebugger,77	; enter your Debugger/Freezer if any present
	DEF_\1_REL	ReadFileFromImage,78	; Load a file from a "DOS\0" image
	DEF_\1_REL	EnableMMU,79		; private - Enable the MMU translation
	DEF_\1_REL	DisableMMU,80		; private - Disable the MMU translation
	DEF_\1_REL	SetTraceVector,81	; Modifies trace vector ($24.W+VBR)
	DEF_\1_REL	GetUserFlags,82		; Gets some of the user tooltypes
	DEF_\1_REL	InstallHarryOSEmu,83	; private - do not use
	DEF_\1_REL	AddPart,84		; concatenates dirname[/:]filename
	DEF_\1_REL	CopyMem,85		; copies memory
	DEF_\1_REL	RelocateExecutable,86	; relocates executable
	DEF_\1_REL	Unsupported7,87	; calculates executable filesize
	DEF_\1_REL	Unsupported8,88	; calculates expanded size in memory
	DEF_\1_REL	Unsupported9,89	; relocates executable, with allocation - deprecated
	DEF_\1_REL	HexSearch,90		; memory search
	DEF_\1_REL	SkipColon,91		; skips ':' in file names
	DEF_\1_REL	RestoreCustomNoDMA,92	; private
	DEF_\1_REL	SetRTFileError,93	; sets/disables runtime file error
	DEF_\1_REL	ATNLength,94		; ATN/IMP length

	DEF_\1_REL	Priv_SetLed,95		; private
	DEF_\1_REL	Priv_GetLed,96		; private

	DEF_\1_REL	Priv_SearchDirEntry,97	; private
	DEF_\1_REL	Priv_WHDStart,98	; private
	DEF_\1_REL	SetBusErrorVector,99	; Modifies bus error vector ($8.W+VBR)
	DEF_\1_REL	Priv_EndSaveOsData,100	; private
	DEF_\1_REL	TryUnpackedSupport,101	; Try to support unpacked files
	DEF_\1_REL	Priv_RegisterBusErrorRoutine,102	; private
	DEF_\1_REL	ShortHexToString,103	; word hex to string conversion
	DEF_\1_REL	SetAfterSwitchFunction,104	; Called after OS swap
	DEF_\1_REL	GetLoadDir,105		; returns LOADDIR string or NULL in A0
	DEF_\1_REL	TraceControl,106	; sets/disables trace mode
	DEF_\1_REL	GetObjectType,107	; gets object type
	DEF_\1_REL	GetOSEmuBase,108	; returns OSEmu base in A0 (usually $400)
	DEF_\1_REL	UnsupportedA,109	; future use
	DEF_\1_REL	UnsupportedB,110	; future use
	DEF_\1_REL	UnsupportedC,111	; future use
	
	ENDM

DUMMY_MACRO:MACRO
	;dummy due asm-one-problem
	dc.b	\1
	ENDM

DEF_OFFSET_ABS:MACRO
AbsOff_\1 = \2*4
	ENDM

DEF_OFFSET_REL:MACRO
RelOff_\1 = \2*4
	ENDM


RELOC_CLRL:MACRO
	IFNE	NARG-1
		FAIL	arguments "RELOC_CLRL"
	ENDC

	RELOC_MOVEL	#0,\1
	ENDM

RELOC_CLRW:MACRO
	IFNE	NARG-1
		FAIL	arguments "RELOC_CLRW"
	ENDC

	RELOC_MOVEW	#0,\1
	ENDM

RELOC_CLRB:MACRO
	IFNE	NARG-1
		FAIL	arguments "RELOC_CLRB"
	ENDC

	RELOC_MOVEB	#0,\1
	ENDM

RELOC_STL:MACRO
	IFNE	NARG-1
		FAIL	arguments "RELOC_STL"
	ENDC

	RELOC_MOVEL	#-1,\1
	ENDM

RELOC_STW:MACRO
	IFNE	NARG-1
		FAIL	arguments "RELOC_STW"
	ENDC

	RELOC_MOVEW	#-1,\1
	ENDM

RELOC_STB:MACRO
	IFNE	NARG-1
		FAIL	arguments "RELOC_STB"
	ENDC

	RELOC_MOVEB	#-1,\1
	ENDM

RELOC_MOVEL:MACRO
	IFNE	NARG-2
		FAIL	arguments "RELOC_MOVEL"
	ENDC

	movem.l	D0/A6,-(sp)
	lea	\2(pc),A6
	move.l	\1,(A6)
	movem.l	(sp)+,D0/A6	; movem preserves flags
	ENDM

RELOC_MOVEW:MACRO
	IFNE	NARG-2
		FAIL	arguments "RELOC_MOVEW"
	ENDC

	movem.l	A6/D0,-(sp)
	lea	\2(pc),A6
	move.w	\1,(A6)
	movem.l	(sp)+,D0/A6
	ENDM

RELOC_MOVEB:MACRO
	IFNE	NARG-2
		FAIL	arguments "RELOC_MOVEB"
	ENDC

	movem.l	A6/D0,-(sp)
	lea	\2(pc),A6
	move.b	\1,(A6)
	movem.l	(sp)+,D0/A6
	ENDM

RELOC_ADDL:MACRO
	IFNE	NARG-2
		FAIL	arguments "RELOC_ADDL"
	ENDC

	movem.l	D0/A6,-(sp)
	lea	\2(pc),A6
	add.l	\1,(A6)
	movem.l	(sp)+,D0/A6	; movem preserves flags
	ENDM

RELOC_ADDW:MACRO
	IFNE	NARG-2
		FAIL	arguments "RELOC_ADDW"
	ENDC

	movem.l	A6/D0,-(sp)
	lea	\2(pc),A6
	add.w	\1,(A6)
	movem.l	(sp)+,D0/A6
	ENDM

RELOC_ADDB:MACRO
	IFNE	NARG-2
		FAIL	arguments "RELOC_ADDB"
	ENDC

	movem.l	A6/D0,-(sp)
	lea	\2(pc),A6
	add.b	\1,(A6)
	movem.l	(sp)+,D0/A6
	ENDM


RELOC_SUBL:MACRO
	IFNE	NARG-2
		FAIL	arguments "RELOC_SUBL"
	ENDC

	movem.l	D0/A6,-(sp)
	lea	\2(pc),A6
	sub.l	\1,(A6)
	movem.l	(sp)+,D0/A6	; movem preserves flags
	ENDM

RELOC_SUBW:MACRO
	IFNE	NARG-2
		FAIL	arguments "RELOC_SUBW"
	ENDC

	movem.l	A6/D0,-(sp)
	lea	\2(pc),A6
	sub.w	\1,(A6)
	movem.l	(sp)+,D0/A6
	ENDM

RELOC_SUBB:MACRO
	IFNE	NARG-2
		FAIL	arguments "RELOC_SUBB"
	ENDC

	movem.l	A6/D0,-(sp)
	lea	\2(pc),A6
	sub.b	\1,(A6)
	movem.l	(sp)+,D0/A6
	ENDM

RELOC_TSTL:MACRO
	IFNE	NARG-1
		FAIL	arguments "RELOC_TSTL"
	ENDC

	movem.l	D0/A0,-(sp)
	lea	\1(pc),A0
	tst.l	(A0)
	movem.l	(sp)+,D0/A0
	ENDM

RELOC_TSTW:MACRO
	IFNE	NARG-1
		FAIL	arguments "RELOC_TSTW"
	ENDC

	movem.l	D0/A0,-(sp)
	lea	\1(pc),A0
	tst.w	(A0)
	movem.l	(sp)+,D0/A0
	ENDM

RELOC_TSTB:MACRO
	IFNE	NARG-1
		FAIL	arguments "RELOC_TSTB"
	ENDC

	movem.l	D0/A0,-(sp)
	lea	\1(pc),A0
	tst.b	(A0)
	movem.l	(sp)+,D0/A0
	ENDM

	; *** builds the function offsets

	MAKE_ABS_REFS	OFFSET
	MAKE_REL_REFS	OFFSET


HDP_SAFETY = $00
HDP_MAGIC = $04
HDP_VERSION = $08
HDP_ABSTABLE = $0C
HDP_RELTABLE = $10
HDP_FILESIZE = $14
HDP_NBDISKS = $18
HDP_ENTRY = $1C
HDP_FNAME = $20
HDP_SYSBASE = $24
HDP_DOSBASE = $28

OBJ_NONE = 0
OBJ_FILE = 1
OBJ_DIR = 2


	ENDC

