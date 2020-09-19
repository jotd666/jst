;Macros
vm_test_button_down:MACRO
		btst		\1,D0
		beq			\2
		btst		\1,D3
		bne			\2
	ENDM

;Constants
MOVEAMOUNT		EQU $4 ;Bottom two bits of each byte aren't recognised by JOYTEST, hence you can't make smaller moves than 4 pixels
JOY0DAT			EQU $DFF00A
JOYTEST			EQU	$dff036
CIAADDRA		EQU $BFE201
CIAAPRA  		EQU $BFE001
FIRE1			EQU 6
	
;Erik's virtual mouse test
;Trashes D1,D2
;D1 is used as the lower byte (horizontal)
;D2 is used as the upper byte (vertical)
;Assumes D0 is populated with CD32 joypad read data
;Assumes D1 is set to #1 if port 1, #0 if port 0

vm

	;Do nothing if no button has been configured
	TSTVAR_B				vm_button
	beq						vm_quit

	;Is this enabled?
	TSTVAR_B				vm_enabled
	bne						vm_testdisable

	;Test that the select button has been pressed
	GETVAR_B				vm_button,d2
	vm_test_button_down		d2,vm_quit
	
	;Button has been pressed, so set active to 1
	SETVAR_B				#1,vm_enabled
	CLRVAR_L				vm_currentdelay
	bra						vm_quit
	
vm_testdisable
	
	GETVAR_B				vm_button,d2
	vm_test_button_down		d2,vm_test_leftbutton
	
	;Button has been lifted, so set active to 0
	CLRVAR_B				vm_enabled	
	bra						vm_quit
	
vm_test_leftbutton 

	tst.b		D1
	beq			vm_testdelay ;If the gamepad is in port 2, try to map the gamepad red/blue buttons to mouse left/right
	btst		#JPB_BTN_RED,d0
	beq			vm_leftbutton_up
	bset		#FIRE1,CIAADDRA	;set to output
	bclr		#FIRE1,CIAAPRA
	bra			vm_testdelay
	
vm_leftbutton_up
	bclr		#FIRE1,CIAADDRA ;Set back to input

vm_testdelay

	;Are we currently waiting to test the gamepad?
	TSTVAR_L	vm_currentdelay
	beq			vm_setdelay
	
	;If we got to here, we're still waiting
	GETVAR_L	vm_currentdelay,d2
	sub.l		#1,d2
	SETVAR_L	d2,vm_currentdelay
	rts

vm_setdelay

	;Is the player holding down the modify key?
	GETVAR_B	vm_modifierbutton,d2 
	btst		d2,d0
	bne			vm_setmodifieddelay
	
	;Use the regular delay
	GETVAR_L	vm_delay,d2
	SETVAR_L	d2,vm_currentdelay
	bra			vm_testright
	
vm_setmodifieddelay ;Use the modified delay

	GETVAR_L	vm_modifierdelay,d2
	SETVAR_L	d2,vm_currentdelay
		
vm_testright
	;Get the current mouse data
	move.w		JOY0DAT,D1
	move.w		D1,D2
	lsr			#8,D2

	BTST		#JPB_BTN_RIGHT,D0
	beq			vm_testleft
	add.b		#MOVEAMOUNT,D1

vm_testleft
	BTST		#JPB_BTN_LEFT,D0
	beq			vm_testup
	sub.b		#MOVEAMOUNT,D1
	
vm_testup
	BTST		#JPB_BTN_UP,D0
	beq			vm_testdown
	sub.b		#MOVEAMOUNT,D2

vm_testdown
	BTST		#JPB_BTN_DOWN,D0
	beq			vm_done
	add.b		#MOVEAMOUNT,D2
	
vm_done
	lsl			#8,D2
	move.b		D1,D2
	move.w		D2,JOYTEST
	
vm_quit
	rts