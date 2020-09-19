DECL_LABEL:MACRO
	XDEF	msg_\1
msg_\1:
	ENDM

	IFD	BARFLY
	OUTPUT	//objs/jst_msgs.o
	ENDC

	SECTION	"labels",DATA

	DECL_LABEL	JST_running_executab	
	dc.b	"JST running executable program...",10,0

	DECL_LABEL	Could_not_execute_pr
	dc.b	"** Could not execute program",10,0

	DECL_LABEL	JOTD_Startup_verbose_mode
	dc.b	"JOTD Startup by JF. Fabre & R. Huvendiek",10
	dc.b	"Test/Verbose mode.",10,10
	dc.b	"OBJECT NAME: ",0

	DECL_LABEL	colon
	dc.b	":",9,0

	DECL_LABEL	No_debugger_found	
	dc.b	"No debugger found",10,0

	DECL_LABEL	HRTMon_found	
	dc.b	"HRTMon found",10,0

	DECL_LABEL	COP_found	
	dc.b	"COP found",10,0

	DECL_LABEL	StackOverflow_error
	dc.b	"Stack overflow",10,0

	DECL_LABEL	ThrillKill_found	
	dc.b	"ThrillKill found",10,0

	DECL_LABEL	ActionReplay_found	
	dc.b	"ActionReplay found",10,0

	DECL_LABEL	ON	
	dc.b	"ON",10,0

	DECL_LABEL	OFF	
	dc.b	"OFF",10,0

	DECL_LABEL	MMUCode_length_c_	
	dc.b	"MMUCode length: ",0

	DECL_LABEL	Object_loaded_at_loc	
	dc.b	"Object loaded at location ",0

	DECL_LABEL	Load_disks_files_fro	
	dc.b	"Load disks/files from: ",0

	DECL_LABEL	This_loader_uses_OSE	
	dc.b	"This loader uses OSEmu",10,0

	DECL_LABEL	OSEmu_is_Harry_Wepl_	
	dc.b	"OSEmu is © Harry/Wepl/Mr Larmer/JOTD",10,0

	DECL_LABEL	Write_to_disk_disabl	
	dc.b	"Write to disk disabled (NOOSSWAP)",10,0

	DECL_LABEL	xpk_packed_not_supp
	dc.b	"XPK packed files aren't supported",10,0
	DECL_LABEL	Obsolete_version_of_
	dc.b	"Obsolete version of OSEmu loaded",10,0

	DECL_LABEL	Unable_to_find_OSEmu
	dc.b	"** Unable to find the file OSEmu.400",10
	dc.b	"   in the game dir or in JST PROGDIR:!",10,0

	DECL_LABEL	Reloc_routines_not_relocated
	dc.b	"** Internal: reloc routines are not relocated yet!",10,0

	DECL_LABEL	Resuming
	dc.b	"Resuming ",0

	DECL_LABEL	Version
	dc.b	"Version ",0

	DECL_LABEL	Dot
	dc.b	".",0

	DECL_LABEL	Of_osemu_loaded
	dc.b	" of OSEmu loaded",10,0


; labels 2


	DECL_LABEL	Cannot_allocate_file	
	dc.b	"   Cannot allocate file info block",10,0

	DECL_LABEL	Reading_user_file_	
	dc.b	"Reading user file ",0

	DECL_LABEL	Reading_file_	
	dc.b	"Reading file ",0

	DECL_LABEL	OK_po_length_pf_	
	dc.b	"OK (length)",10,0

	DECL_LABEL	FAIL_po_length_pf_	
	dc.b	"FAIL (length)",10,0

	DECL_LABEL	OK	
	dc.b	"OK",10,0

	DECL_LABEL	FAIL	
	dc.b	"FAIL",10,0

	DECL_LABEL	Modifying_file_	
	dc.b	"Modifying file ",0

	DECL_LABEL	Writing_user_file_	
	dc.b	"Writing user file ",0

	DECL_LABEL	Writing_file_	
	dc.b	"Writing file ",0

	DECL_LABEL	Deleting_user_file_	
	dc.b	"Deleting user file ",0

	DECL_LABEL	Deleting_file_	
	dc.b	"Deleting file ",0

	DECL_LABEL	Extension_memory_at_	
	dc.b	"Extension memory at ",0

	DECL_LABEL	No_extension_memory_	
	dc.b	"No extension memory, not enough mem",10,0	

	DECL_LABEL	bit_Extension_memory	
	dc.b	"24 bit Extension memory at ",0

	DECL_LABEL	No_bit_extension_mem	
	dc.b	"No 24 bit extension memory, not enough mem",10,0	

	DECL_LABEL	OpenFakeExec_po_pf_c	
	dc.b	"** OpenFakeExec(): Feature removed. Ask for a loader update",10,0

	DECL_LABEL	Logging_chip_memory_	
	dc.b	"Logging chip memory...",10,0

	DECL_LABEL	Logging_extension_me	
	dc.b	"Logging extension memory...",10,0

	DECL_LABEL	Logging_custom_memor	
	dc.b	"Logging custom memory...",10,0

	DECL_LABEL	Loading_disk_po_s_pf	
	dc.b	"Loading disk(s)...",10,0

	DECL_LABEL	Disk_memory_allocate	
	dc.b	"Disk memory allocated at ",0


	DECL_LABEL	allocating_memory
	dc.b	"Allocating memory...",10,0
	DECL_LABEL	done
	dc.b	"Done",10,0

	DECL_LABEL	Loading_files_p_p_p_	
	dc.b	"Loading files...",10,0
	DECL_LABEL	scanning_files
	dc.b	"Scanning files...",10,0

	DECL_LABEL	Loading_file_	
	dc.b	"Loading file ",0

	DECL_LABEL	Unable_to_open_	
	dc.b	"** Unable to open ",0

	DECL_LABEL	Empty_name_found_in_	
	dc.b	"** Empty name found in dir: disk error",10,0

	DECL_LABEL	Examine_po_	
	dc.b	"Examine (",0

	DECL_LABEL	v_	
	dc.b	",",0

	DECL_LABEL	pf_
	dc.b	")",10,0

	DECL_LABEL	Unlocking_	
	dc.b	"Unlocking ",0

	DECL_LABEL	Locking_	
	dc.b	"Locking ",0

	DECL_LABEL	po_	
	dc.b	" (",0

	DECL_LABEL	FAIL_pf_	
	dc.b	"*FAIL*)",10,0

	DECL_LABEL	RESUME_not_available	
	dc.b	"** RESUME not available, not enough memory!",10,0

	DECL_LABEL	Or_abs_memory_requir	
	dc.b	"   Or abs memory required by RESUME not available",10,0

	DECL_LABEL	NOFAST_set_while_bot	
	dc.b	"** NOFAST set while both ExtMem and 24BitMem on !",10,0

	DECL_LABEL	AllocExtMem_po_pf_ca	
	dc.b	"** AllocExtMem() called twice !",10,0

	DECL_LABEL	Alloc_BitMem_po_pf_c	
	dc.b	"** Alloc24BitMem() called twice !",10,0

	DECL_LABEL	Not_enough_memory_to	
	dc.b	"** Not enough expansion memory to boot game!",10,0

	DECL_LABEL	Please_CD_to_the_cor	
	dc.b	"** Please CD to the correct directory !",10,0

	DECL_LABEL	Cant_Allocate_Mem
	dc.b	"** Can't allocate enough memory for files !",10,0

	DECL_LABEL	Unexpected_error_	
	dc.b	"** Unexpected error !",10,0

	DECL_LABEL	The_directory_is_emp	
	dc.b	"** The directory is empty !",10,0

	DECL_LABEL	Can_t_lock_specified	
	dc.b	"** Can't lock specified directory !",10,0

	DECL_LABEL	Can_t_lock_directory	
	dc.b	"** Can't lock directory: ",0

	DECL_LABEL	Can_t_open_icon_	
	dc.b	"** Can't open icon !!!!",10,0

	DECL_LABEL	Object_	
	dc.b	"** Object ",0

	DECL_LABEL	has_got_an_invalid_s	
	dc.b	" has got an invalid structure",10,0

	DECL_LABEL	is_executable_but_no	
	dc.b	" is executable but not a JOTD loader",10,0

	DECL_LABEL	Check_for_HD_PARAMS_	
	dc.b	"   Check for HD_PARAMS location in the source",10,0

	DECL_LABEL	JOTDStartup_does_not	
	dc.b	"** JST does not support this WHDLoad slave yet (try FORCEWHD)",10,0

	DECL_LABEL	Version_of_JOTDStart	
	dc.b	"** Version of JST is outdated for this object",10,0

	DECL_LABEL	is_made_of_more_than	
	dc.b	" is made of more than 1 section",10,0

	DECL_LABEL	is_not_relocatable_p	
	dc.b	" is not relocatable.",10,"Non reloc addresses count: ",0

	DECL_LABEL	Please_check_addr_p_	
	dc.b	"Please check addr. modes and/or use optimize option",10,0

	DECL_LABEL	not_found_or_empty_	
	dc.b	" not found or empty!!",10,0

	DECL_LABEL	Can_t_find_file_	
	dc.b	"** Can't find file ",0

	DECL_LABEL	exclamation
	dc.b	" !",10,0

	DECL_LABEL	Loader_stopped_p_Typ	
	dc.b	"Loader stopped (copper: ",0

	DECL_LABEL	x_po_RETURN_pf_to_re	
	dc.b	"Type: ",10,"  x (+RETURN) to return to the loader",10,0

	DECL_LABEL	s_po_RETURN_pf_to_sa	
	dc.b	"  s (+RETURN) to save snapshot",10,0

	DECL_LABEL	l_po_RETURN_pf_to_re	
	dc.b	"  l (+RETURN) to re-load snapshot",10,0

	DECL_LABEL	q_po_RETURN_pf_to_qu	
	dc.b	"  q (+RETURN) to quit the current loader",10,0

	DECL_LABEL	Invalid_command_p_Co	
	dc.b	"** Invalid command. Commands are:",10,0

	DECL_LABEL	Loading_kickstart
	dc.b	"Loading kickstart: ",0
	
	DECL_LABEL	BlockFastMem_c_Don_t	
	dc.b	"** BlockFastMem: Don't use this shit anymore!",10,0

	DECL_LABEL	Function_c_	
	dc.b	"Function: ",0

	DECL_LABEL	Maybe_try_with_NOCAC	
	dc.b	"Maybe try with NOCACHES set or/and",10
	dc.b	"MMU=NONE or MMU=LAXIST",10,0

	DECL_LABEL	Freeing_loader	
	dc.b	"Freeing loader",10,0

	DECL_LABEL	Freeing_expansion_me	
	dc.b	"Freeing expansion memory",10,0

	DECL_LABEL	Freeing_bit_expansio	
	dc.b	"Freeing 24-bit expansion memory",10,0

	DECL_LABEL	Freeing_game_files	
	dc.b	"Freeing game files",10,0

	DECL_LABEL	Freeing_logpatch_buf	
	dc.b	"Freeing logpatch buffer",10,0

	DECL_LABEL	Freeing_MMU_code	
	dc.b	"Freeing MMU code",10,0

	DECL_LABEL	Hit_RETURN_to_exit	
	dc.b	"** Hit RETURN to exit",10,0

	DECL_LABEL	Logging_registers_p_	
	dc.b	"Logging registers...",10,0

	DECL_LABEL	Logged_patch_memory
	dc.b	"** Logged patch memory allocated",10
	dc.b	"   Loader in development stage!!",10,10,0

	DECL_LABEL	SetLocalVarZone_c_in	
	dc.b	"** SetLocalVarZone: incoherency in bounds!",10,0

	DECL_LABEL	SetLocalVarZone_c_bo	
	dc.b	"** SetLocalVarZone: bounds already set!",10,0

	DECL_LABEL	Absolute_memory_allo	
	dc.b	"Absolute memory allocation: ",0

	DECL_LABEL	Last_file_loaded
	dc.b	"Last file loaded by ReadFile...: ",0

	DECL_LABEL	Last_interrupt_vector
	dc.b	"Last interrupt vector: ",0

	DECL_LABEL	File_written
	dc.b	"File written.",10,0

	DECL_LABEL	datadump_format
	dc.b	"JOTD Startup register dump",10,10
	dc.b	"Data: $%08lx $%08lx $%08lx $%08lx $%08lx $%08lx $%08lx $%08lx",10
	dc.b	"Addr: $%08lx $%08lx $%08lx $%08lx $%08lx $%08lx $%08lx $%08lx",10,10
	dc.b	"Stak: $%08lx $%08lx $%08lx $%08lx $%08lx $%08lx $%08lx $%08lx",10
	dc.b	"PC-8: $%08lx $%08lx $%08lx $%08lx $%08lx $%08lx $%08lx $%08lx",10,10
	dc.b	"PC",9,9,": $%08lx",9,9,9,"ExtOffset",9,9,": $%08lx",10,"ExtFast",9,9
	dc.b	": $%08lx",9,9,9,"Ext24Bit",9,9,": $%08lx",10,10
	dc.b	"SR",9,9,": $%04x",10,10,"INTENA: $%04x, INTREQ: $%04x",10
	dc.b	"DMACON: $%04x, ADKCON: $%04x",10,0

	DECL_LABEL	uncacheing_file
	dc.b	"Uncacheing file ",0

	DECL_LABEL	obsolete_rom
	dc.b	"JST needs Kickstart V37 or higher",10,0
	even
