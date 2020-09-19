; written by Toni Wilen on EAB!!
; < D0: keycode to write
; trashes D0, D1, D2


send_key_event
	; Timer value = 10 (Just randonly chosen value)
	move.b #10,$bfe401
	move.b #0,$bfe501
	; OUTMODE and START
	move.b #$41,$bfee01
	
	; Write keycode (keypress only)
	rol.b	#1,d0
	not.b	d0
	move.b d0,$bfec01

	; Delay until keycode transmit is complete
	; (Can't poll CIA interrupt register because it also clears it)
	moveq #10-1,d2
l4
	move.w $dff006,d0
	clr.b d0
l3	move.w $dff006,d1
	clr.b d1
	cmp.w d0,d1
	beq.s l3
	dbf d2,l4


	; Stop timer
	bclr #0,$bfee01
	
	rts

;keycodes
	;dc.b ~($20<<1+0),~($20<<1+1) ;A press,A relase
	;dc.b ~($35<<1+0),~($35<<1+1) ;B
	;dc.b ~($33<<1+0),~($33<<1+1) ;C
	;dc.b $ff
