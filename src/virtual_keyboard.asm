;Macros
vk_test_button_down:MACRO
		btst		\1,D0
		beq			\2
;		btst		\1,D3
;		bne			\2
	ENDM
	
;constants
VK_MAX				equ		36		;Index of the last character
VK_KEYWAITFRAMES	equ		30		;Number of frames between key down and key up events

; already in reloc zone, no need to copy it again and again
keycode_table:	
	dc.b  $20 ;a
	dc.b  $35  ;b
	dc.b  $33  ;c
	dc.b  $22  ;d
	dc.b  $12  ;e
	dc.b  $23  ;f
	dc.b  $24  ;g
	dc.b  $25  ;h
	dc.b  $17  ;i
	dc.b  $26  ;j
	dc.b  $27  ;k
	dc.b  $28  ;l
	dc.b  $37  ;m
	dc.b  $36  ;n
	dc.b  $18  ;o
	dc.b  $19  ;p
	dc.b  $10  ;q
	dc.b  $13  ;r
	dc.b  $21  ;s
	dc.b  $14  ;t
	dc.b  $16  ;u
	dc.b  $34  ;v
	dc.b  $11  ;w
	dc.b  $32  ;x
	dc.b  $16  ;y
	dc.b  $31  ;z
	dc.b  $40 	;space
	dc.b  $01  ;1
	dc.b  $02  ;2
	dc.b  $03  ;3
	dc.b  $04  ;4
	dc.b  $05  ;5
	dc.b  $06  ;6
	dc.b  $07  ;7
	dc.b  $08  ;8
	dc.b  $09  ;9
	dc.b  $0a  ;0
	even

vk:
		;If this isn't assigned to a button, do nothing
		TSTVAR_B	vk_button
		beq			vk_done	
		
		;Are we waiting for a delay to end?
		TSTVAR_B	vk_key_delay
		beq			vk_nokeydelay
		
		;Count down on the key delay
		GETVAR_B	vk_key_delay,D1
		sub.b		#1,D1
		SETVAR_B	D1,vk_key_delay
		TSTVAR_B	vk_key_delay
		bne			vk_done
		
		;Now that the delay is over, are there any key up events scheduled?
		TSTVAR_B	vk_keyup
		beq			vk_nokeydelay
		
		;Finally, send the key up event
		GETVAR_B	vk_keyup,D0
		SETVAR_B	#0,vk_keyup
		bset		#7,d0
		bra			vk_safesendkeyevent
		
vk_nokeydelay:

		;Was a key change changed?
		TSTVAR_B	vk_queued
		beq			vk_notqueued
		
		GETVAR_B	vk_queued,d1
		sub.b		#1,d1
		SETVAR_B	d1,vk_queued
		TSTVAR_B	vk_queued
		bne			vk_done
		
		;Finished waiting, so now punch the new character
		bra			vk_punchcharacter
	
vk_notqueued:

		TSTVAR_B 	vk_on
		bne			vk_testleft
		
		;Not active, if so activate if the keycode is up
		SETVAR_B	#0,vk_wason
		GETVAR_B	vk_button,D1
		vk_test_button_down D1,vk_done
		
		;Player has just released key, so set active plus punch selected character
		SETVAR_B 	#$1,vk_on
		SETVAR_B	#$1,vk_wason
		bra			vk_punchcharacter
		
vk_testleft:

		;Test for backspace
		vk_test_button_down #JPB_BTN_LEFT,vk_testright		
		bra			vk_backspace
		
vk_testright:

		;Test for insert character
		vk_test_button_down #JPB_BTN_RIGHT,vk_testup		
		bra			vk_punchcharacter

vk_testup:

		;Test for decrease character code
		vk_test_button_down #JPB_BTN_UP,vk_testdown
		
		GETVAR_B	vk_selected_character,D1
		tst.b		D1
		bne			vk_decrease ;If the key is not equal to zero, decrease by one. Else wrap
		move.b		#VK_MAX,D1
		bra			vk_swapcharacter

vk_decrease:

		sub.b		#1,D1
		bra			vk_swapcharacter
		
vk_testdown:

		;Test for increase character code
		vk_test_button_down #JPB_BTN_DOWN,vk_testquit

		GETVAR_B	vk_selected_character,D1
		cmp.b		#VK_MAX,D1
		blt			vk_increase ;Key is less than maximum, so increase. Else wrap
		clr.b		D1
		bra			vk_swapcharacter
		
vk_increase
		add.b		#1,D1
		bra			vk_swapcharacter

vk_testquit:

		;Disable virtual keyboard if reverse has been hit, otherwise we're done for now
		GETVAR_B	vk_button,D1
		vk_test_button_down D1,vk_done
		
		;Player has just exited virtual keyboard, so finish by sending enter
		SETVAR_B 	#0,vk_on
		SETVAR_B	#0,vk_selected_character
		move.b		#$44,D0
		bra 		vk_presskey
		
vk_done:
		rts		
		
vk_swapcharacter:
		SETVAR_B	D1,vk_selected_character
		SETVAR_B	#1,vk_queued
		bra			vk_backspace

vk_punchcharacter:

		clr.l		d0
		lea		keycode_table(pc),A5
		GETVAR_B	vk_selected_character,d0
		add.l		d0,A5
		move.b		(A5),d0
		bra 		vk_presskey

vk_backspace:
		move.b		#$41,D0
		
vk_presskey:
		SETVAR_B	D0,vk_keyup
		
vk_safesendkeyevent:
		SETVAR_B	#VK_KEYWAITFRAMES,vk_key_delay ;ALL events contain a delay
		and.l		#$FF,D0 ;Make sure it's only the lower byte
		bsr			send_key_event
		rts
		
		