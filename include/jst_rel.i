	IFND	JST_REL_I_INCLUDED
JST_REL_I_INCLUDED	=	1

	include	"exec/types.i"

TMP_STACK_SIZE = 1000 ; in longwords

; offset for jst_rel.o structure
; add the patch buffer address to it, and you get the data

	STRUCTURE	AbsOffsets,0

	; initialized variables

	ULONG	RelVar_jstrel_length
	ULONG	RelVar_variables_offset
;;	ULONG	RelVar_exception08_offset

	; uninitialized function offsets

	LABEL	Rel_STARTFUNC

	ULONG	Rel_InGameIconify
	ULONG	Rel_WHDReadDir
	ULONG	Rel_ReadDir
	ULONG	Rel_WriteTheFileHD
	ULONG	Rel_WriteFilePartHD
	ULONG	Rel_WriteUserFileHD
	ULONG	Rel_ReadFileHD
	ULONG	Rel_ReadUserFileHD
	ULONG	Rel_ReadTheFileHD
	ULONG	Rel_DeleteFileHD
	ULONG	Rel_DeleteUserFileHD
	ULONG	Rel_ReadUserDir
	ULONG	Rel_CloseAllWithError
	ULONG	Rel_LogChipMirror
	ULONG	Rel_LogCustomMirror
	ULONG	Rel_LogExtMemory
	ULONG	Rel_LogRegisters
	ULONG	Rel_DisableMMU
	ULONG	Rel_SetClockLoad
	ULONG	Rel_EnhanceCpu
	ULONG	Rel_EnhanceGfx
	ULONG	Rel_FreeTheMemory
	ULONG	Rel_CloseAllQuiet
	ULONG	Rel_Display
	ULONG	Rel_MarkNewEntryValid
	ULONG	Rel_UserMode
	ULONG	Rel_SupervisorMode
	LABEL	Rel_ENDFUNC

	ULONG	Rel_RelFunTable	; last entry of the structure


	STRUCTURE	RelVariables,0

	LABEL	Rel_STARTVARS

	; unitialized variables

	ULONG	RelVar_chipmirror
	ULONG	RelVar_chipsize
	ULONG	RelVar_mmucode_len
	ULONG	RelVar_object_len
	ULONG	RelVar_osemu_ptr
	ULONG	RelVar_osemu_len
	ULONG	RelVar_fileoffset
	ULONG	RelVar_fileidbuffer
	ULONG	RelVar_fileidlen
	ULONG	RelVar_message_ptr
	ULONG	RelVar_whd_sysoffset

	; option flags

	LABEL	Rel_STARTFLAGS
        ULONG   RelVar_quiet_flag
        ULONG   RelVar_freezermb_flag
        ULONG   RelVar_verbose_flag
        ULONG   RelVar_fakepad_flag
        ULONG   RelVar_lowmem_flag
        ULONG   RelVar_hdload_flag
        ULONG   RelVar_buttonwait_flag
		; those are dummies ATM
		ULONG	RelVar_custom1_flag
		ULONG	RelVar_custom2_flag
		ULONG	RelVar_custom3_flag
		ULONG	RelVar_custom4_flag
		ULONG	RelVar_custom5_flag
		ULONG	RelVar_eclock_freq
		
        ULONG   RelVar_trainer_flag
        ULONG   RelVar_test_flag
        ULONG   RelVar_execute_flag
        ULONG   RelVar_nofast_flag
        ULONG   RelVar_noquit_flag
        ULONG   RelVar_resume_flag
        ULONG   RelVar_nocaches_flag
        ULONG   RelVar_delay_flag
        ULONG   RelVar_debug_flag
        ULONG   RelVar_leavecaches_flag
        ULONG   RelVar_leavevbr_flag
        ULONG   RelVar_filteroff_flag
        ULONG   RelVar_ntsc_flag
        ULONG   RelVar_pal_flag
        ULONG   RelVar_novbrmove_flag
        ULONG   RelVar_d_flag
		ULONG	RelVar_last_io_error
	ULONG	RelVar_mmunumstate_flag
	STRUCT	RelVar_custom_str,80
	LABEL	Rel_ENDFLAGS

	; end option flags

	; old "reloc" variables

	ULONG	RelVar_debugger_nmi
	ULONG	RelVar_debugger_base
	ULONG	RelVar_debugger
	ULONG	RelVar_filesize		; size of a disk file
	ULONG	RelVar_diskbias		; first disk loaded (LoadDisksIndex)
	ULONG	RelVar_ostrashed	; tells if the os is down or not
	ULONG	RelVar_ostotrash	; tells if the os is going to be down after the call

	UBYTE	RelVar_freezekeynum
	UBYTE	RelVar_iconifykeynum
	UBYTE	RelVar_quitkeynum
	UBYTE	pad0

	ULONG	RelVar_forceclistnum

	ULONG	RelVar_maxchip		; max chip address of transfered
	ULONG	RelVar_keylength
	ULONG	RelVar_keycrc16
	ULONG	RelVar_extsize
	ULONG	RelVar_fatal_fileerror
	ULONG	RelVar_debuggerbase
	ULONG	RelVar_saved_mem_size	; size of mem (minus saved by the Alloc chip)
	ULONG	RelVar_saved_mem_ptr	; pointer on start this mem (have to save before)
	ULONG	RelVar_endsaved_mem_ptr	; pointer on end of this mem (have to save after)
	ULONG	RelVar_attnflags
	ULONG	RelVar_logpatch_buf	; pointer on start of logpatch buffer
	ULONG	RelVar_logpatch_ptr	; pointer on current log
	ULONG	RelVar_object_entry
	ULONG	RelVar_custom_mirror	; pointer on custom memory buffer
	; end old "reloc" variables

	; whdload variables
	


	; end whdload variables

	; snapshot variables

	LABEL	RelVar_start_snapshot
	ULONG	RelVar_snapshot_version	; current version of the snapshot (0: unavail because unregistered)
	UWORD	RelVar_object_crc	; CRC of the slave when the snapshot was made
	ULONG	RelVar_return_offset	; offset (base=jstrel start) to return to
	LABEL	RelVar_datastream
	STRUCT	RelVar_cpuregs,$40
	STRUCT	RelVar_crash_stack,$20	; $20 bytes of stack when the crash occurred
	STRUCT	RelVar_pc8,$20		; $20 bytes of memory after PC

	ULONG	RelVar_game_pc
	ULONG	RelVar_extbuf_offset
	ULONG	RelVar_extbuf
	ULONG	RelVar_bit24buf
	UWORD	RelVar_game_sr
	LABEL	RelVar_game_hwregs
	UWORD	RelVar_game_intreq
	UWORD	RelVar_game_intena
	UWORD	RelVar_game_dmacon
	UWORD	RelVar_game_adkcon
	STRUCT	RelVar_game_ciaregs,4*6

	ULONG	RelVar_bit24size
	ULONG	RelVar_current_disk
	ULONG	RelVar_game_userstack
	ULONG	RelVar_game_superstack
	STRUCT	RelVar_new_vbr,$100	; saved game JST VBR

	ULONG	RelVar_doonexit
	ULONG	RelVar_copper_pointer
	ULONG	RelVar_gene_patchbuffer
	ULONG	RelVar_gene_patchbuffer_size
	ULONG	RelVar_object_ptr
	ULONG	RelVar_mmucode_ptr
	ULONG	RelVar_after_switch_cb
	LABEL	RelVar_end_snapshot	; snapshot end here

	ULONG	RelVar_system_userstack
	ULONG	RelVar_system_superstack
	ULONG	RelVar_system_vbr
	ULONG	RelVar_current_vbr

	LABEL	RelVar_sys_hwregs
	UWORD	RelVar_sys_intreq
	UWORD	RelVar_sys_intena
	UWORD	RelVar_sys_dmacon
	UWORD	RelVar_sys_adkcon
	STRUCT	RelVar_sys_ciaregs,4*6

	LABEL	RelVar_sysnodisp_hwregs
	UWORD	RelVar_sysnodisp_intreq
	UWORD	RelVar_sysnodisp_intena
	UWORD	RelVar_sysnodisp_dmacon
	UWORD	RelVar_sysnodisp_adkcon

	ULONG	RelVar_os_switch_cb
	UWORD	RelVar_last_interrupt
	UWORD	RelVar_ledstate
	UWORD	RelVar_just_resumed
	UWORD	RelVar_unpacked_support
	UWORD	RelVar_iconify_lock
	UWORD	RelVar_snapmem_allocated
	UWORD	RelVar_trace_control
	ULONG	RelVar_kicksize
	UWORD	RelVar_system_bplcon0
	UBYTE	RelVar_system_chiprev_bits	; only 1 byte is used
	UBYTE	pad1
	
	STRUCT	RelVar_loaddata_dir,256
	STRUCT	RelVar_raw_ascii_table,512
	STRUCT	RelVar_whd_sysunit,40
	STRUCT	RelVar_diskbuffers,40
	STRUCT	RelVar_fname,256
	STRUCT	RelVar_lastfile_buffer,FNAME_SIZE
	STRUCT	RelVar_fname_base,FNAME_SIZE
	STRUCT	RelVar_fpath,FNAME_SIZE


	; virtual mouse stuff, courtesy to Earok
	ULONG	RelVar_vm_modifierdelay
	ULONG	RelVar_vm_delay
	ULONG	RelVar_vm_currentdelay
	ULONG	RelVar_previous_joy0_state
	ULONG	RelVar_previous_joy1_state
    UBYTE   RelVar_joypad_type
	UBYTE	RelVar_joy1_play_keycode
	UBYTE	RelVar_joy1_fwd_keycode
	UBYTE	RelVar_joy1_bwd_keycode
	UBYTE	RelVar_joy1_green_keycode
	UBYTE	RelVar_joy1_blue_keycode
	UBYTE	RelVar_joy1_yellow_keycode
	UBYTE	RelVar_joy1_fwdbwd_keycode
	UBYTE	RelVar_joy1_fwdbwd_active
	UBYTE	RelVar_joy1_red_keycode
	UBYTE	RelVar_joy1_right_keycode
	UBYTE	RelVar_joy1_left_keycode
	UBYTE	RelVar_joy1_up_keycode
	UBYTE	RelVar_joy1_down_keycode
	UBYTE	RelVar_joy0_play_keycode
	UBYTE	RelVar_joy0_fwd_keycode
	UBYTE	RelVar_joy0_bwd_keycode
	UBYTE	RelVar_joy0_green_keycode
	UBYTE	RelVar_joy0_blue_keycode
	UBYTE	RelVar_joy0_yellow_keycode
	UBYTE	RelVar_joy0_fwdbwd_keycode
	UBYTE	RelVar_joy0_fwdbwd_active
	UBYTE	RelVar_joy0_red_keycode
	UBYTE	RelVar_joy0_right_keycode
	UBYTE	RelVar_joy0_left_keycode
	UBYTE	RelVar_joy0_up_keycode
	UBYTE	RelVar_joy0_down_keycode
	UBYTE	RelVar_vk_on
	UBYTE	RelVar_vk_wason
	UBYTE	RelVar_vk_selected_character
	UBYTE	RelVar_vk_queued
	UBYTE	RelVar_vk_key_delay
	UBYTE	RelVar_vk_button
	UBYTE	RelVar_vk_keyup
	UBYTE	RelVar_vm_button
	UBYTE	RelVar_vm_enabled
	UBYTE	RelVar_vm_modifierbutton
	UBYTE	RelVar_vbl_redirect
	
	STRUCT	tmpstack_zone,TMP_STACK_SIZE*4
	LABEL	RelVar_tmpstack
	ULONG	RelVar_safety1
	ULONG	RelVar_safety2
	ULONG	RelVar_safety3
	ULONG	RelVar_safety4


	LABEL	Rel_ENDVARS

	ENDC
