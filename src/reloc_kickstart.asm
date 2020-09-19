; < extbuf: memory with kickstart to reloc
; < D3 kicksize
; < A3 pointer to RTB
	STORE_REGS
	SET_VAR_CONTEXT
	GETVAR_L	extbuf,D4	; reloc base
	move.l	D4,A2		; kick current address	
;	move.l	D4,A3
;	add.l	D3,A3		; reloc table
	addq.l	#4,A3		; skip CRC long
	sub.l	#$F80000,D4	; original ROM start
	; offsets work different depending on the kick size
	; seems to work with 256k & 512k kicks, so let it stay that way
	; I probably set original ROM start wrong at first cos 256k roms start at $FC0000
	cmp.l	#$80000,D3
	beq.b	.relocloop
	sub.l	D3,D4		; D4: offset to add
.relocloop:
	moveq.l	#0,D0
	move.b	(A3)+,D0
	bne.b	.reloc1byte
	move.b	(A3)+,D0
	bne.b	.reloc2bytes
	move.b	(A3)+,D0
	bne.b	.reloc3bytes

	move.b	(A3)+,D0
	bne.b	.reloc1byte
	bra.b	.reloc1_end			; 4 zeros: out

.reloc3bytes
.reloc2bytes
	lsl	#8,D0
	move.b	(A3)+,D0	
.reloc1byte
	; add D0 to kick pointer and patch
	add.l	D0,A2
	add.l	D4,(A2)

	bra.b	.relocloop
.reloc1_end:
	add.l	#4,A3	; skip $FFFFFFFF
	; now BCPL, just longword relocs (much simpler)
	sub.l	#$20000,D4	; kludge for 256K rom
	asr.l	#2,D4
	add.l	#$8000,D4	; kludge (I'm tired of trying to get the proper values)
	GETVAR_L	extbuf,A2	; reloc base
.relocbcpl
	move.l	(A3)+,D0
	beq.b	.reloc2_end
	; add D0 to kick pointer and patch
	add.l	D4,(A2,D0.L)
	bra.b	.relocbcpl
.reloc2_end
	RESTORE_REGS
	rts