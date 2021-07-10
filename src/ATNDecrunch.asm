
; *** Decrunch a ATN! file
; in: A0: crunched buffer
;     A1: decrunched dest (may be the same)

; out: D0=0 if not a ATN file
	movem.w	D7,-(a7)
	; save header
	move.l	(A0),-(A7)
	move.l	4(A0),-(A7)
	cmp.l	A0,A1
	sne	D7	; D7 != 0 if source != dest
	MOVEM.L	A0-A1,-(A7)
	bsr	.do_atn
	MOVEM.L	(A7)+,A0-A1
	tst.b	D7
	beq.b	.same
	; restore decrunch header if source != dest
	move.l	(A7)+,4(A0)
	move.l	(A7)+,(A0)
	movem.w	(a7)+,D7
	RTS
	
.same
	add.l	#8,A7
	movem.w	(a7)+,D7
	RTS
	
.do_atn
	MOVEM.L	D2-D5/A2-A5,-(A7)
	CMPI.L	#'ATN!',(A0)
	BEQ.B   .decrunch
	CMPI.L	#'IMP!',(A0)
	bne	LB_BA8A
    
.decrunch
    ; ripped & adapted from Body Blows Galactic ECS
    ; apparently, previous version (ripped from somewhere else)
    ; fails to unpack some BBG ECS files and crashes
    ; probably because it isn't made to handle the case where A1 != A0
    
	MOVEA.L	A0,A3
	MOVEA.L	A1,A4
	MOVEA.L	A1,A5
    cmp.l   A1,A0
    beq.b   in_place    ; no need to copy if in-place
    addq.l  #4,a0   ; fourcc has already been checked
    
	MOVE.L	A1,-(A7)
	MOVE.L	(A0)+,D0    ; length
LAB_02D0:
	MOVE.B	(A3)+,(A1)+   ; copy source to dest
	SUBQ.L	#1,D0
	TST.L	D0
	BPL.S	LAB_02D0
	MOVE.L	(A7)+,A1
in_place    
	LEA	4(A1),A0
	MOVEA.L	A1,A3
	ADDA.L	(A0)+,A4
	ADDA.L	(A0)+,A3
	MOVEA.L	A3,A2
	MOVE.L	(A2)+,-(A0)
	MOVE.L	(A2)+,-(A0)
	MOVE.L	(A2)+,-(A0)
	MOVE.L	(A2)+,D2
	MOVE.W	(A2)+,D3
	BMI.S	LAB_02D1
	SUBQ.L	#1,A3
LAB_02D1:
	LEA	-28(A7),A7
	MOVEA.L	A7,A1
	MOVEQ	#6,D0
LAB_02D2:
	MOVE.L	(A2)+,(A1)+
	DBF	D0,LAB_02D2
	MOVEA.L	A7,A1
	MOVEQ	#0,D4
LAB_02D3:
	TST.L	D2
	BEQ.S	LAB_02D5
LAB_02D4:
	MOVE.B	-(A3),-(A4)
	SUBQ.L	#1,D2
	BNE.S	LAB_02D4
LAB_02D5:
	CMPA.L	A4,A5
	BCS.S	LAB_02D8
	LEA	28(A7),A7
	MOVEQ	#-1,D0
	CMPA.L	A3,A0
	BEQ.S	LAB_02D7
LB_BA8A ; error
LAB_02D6:
	MOVEQ	#0,D0
LAB_02D7:

LB_BA8C	MOVEM.L	(A7)+,D2-D5/A2-A5
	TST.L	D0
	RTS


LAB_02D8:
	ADD.B	D3,D3
	BNE.S	LAB_02D9
	MOVE.B	-(A3),D3
	ADDX.B	D3,D3
LAB_02D9:
	BCC.S	LAB_02E5
	ADD.B	D3,D3
	BNE.S	LAB_02DA
	MOVE.B	-(A3),D3
	ADDX.B	D3,D3
LAB_02DA:
	BCC.S	LAB_02E4
	ADD.B	D3,D3
	BNE.S	LAB_02DB
	MOVE.B	-(A3),D3
	ADDX.B	D3,D3
LAB_02DB:
	BCC.S	LAB_02E3
	ADD.B	D3,D3
	BNE.S	LAB_02DC
	MOVE.B	-(A3),D3
	ADDX.B	D3,D3
LAB_02DC:
	BCC.S	LAB_02E2
	ADD.B	D3,D3
	BNE.S	LAB_02DD
	MOVE.B	-(A3),D3
	ADDX.B	D3,D3
LAB_02DD:
	BCC.S	LAB_02DE
	MOVE.B	-(A3),D4
	MOVEQ	#3,D0
	BRA.S	LAB_02E6
LAB_02DE:
	ADD.B	D3,D3
	BNE.S	LAB_02DF
	MOVE.B	-(A3),D3
	ADDX.B	D3,D3
LAB_02DF:
	ADDX.B	D4,D4
	ADD.B	D3,D3
	BNE.S	LAB_02E0
	MOVE.B	-(A3),D3
	ADDX.B	D3,D3
LAB_02E0:
	ADDX.B	D4,D4
	ADD.B	D3,D3
	BNE.S	LAB_02E1
	MOVE.B	-(A3),D3
	ADDX.B	D3,D3
LAB_02E1:
	ADDX.B	D4,D4
	ADDQ.B	#6,D4
	MOVEQ	#3,D0
	BRA.S	LAB_02E6
LAB_02E2:
	MOVEQ	#5,D4
	MOVEQ	#3,D0
	BRA.S	LAB_02E6
LAB_02E3:
	MOVEQ	#4,D4
	MOVEQ	#2,D0
	BRA.S	LAB_02E6
LAB_02E4:
	MOVEQ	#3,D4
	MOVEQ	#1,D0
	BRA.S	LAB_02E6
LAB_02E5:
	MOVEQ	#2,D4
	MOVEQ	#0,D0
LAB_02E6:
	MOVEQ	#0,D5
	MOVE.W	D0,D1
	ADD.B	D3,D3
	BNE.S	LAB_02E7
	MOVE.B	-(A3),D3
	ADDX.B	D3,D3
LAB_02E7:
	BCC.S	LAB_02EA
	ADD.B	D3,D3
	BNE.S	LAB_02E8
	MOVE.B	-(A3),D3
	ADDX.B	D3,D3
LAB_02E8:
	BCC.S	LAB_02E9
	MOVE.B	LAB_02F4(PC,D0.W),D5
	ADDQ.B	#8,D0
	BRA.S	LAB_02EA
LAB_02E9:
	MOVEQ	#2,D5
	ADDQ.B	#4,D0
LAB_02EA:
	MOVE.B	LAB_02F5(PC,D0.W),D0
LAB_02EB:
	ADD.B	D3,D3
	BNE.S	LAB_02EC
	MOVE.B	-(A3),D3
	ADDX.B	D3,D3
LAB_02EC:
	ADDX.W	D2,D2
	SUBQ.B	#1,D0
	BNE.S	LAB_02EB
	ADD.W	D5,D2
	MOVEQ	#0,D5
	MOVEA.L	D5,A2
	MOVE.W	D1,D0
	ADD.B	D3,D3
	BNE.S	LAB_02ED
	MOVE.B	-(A3),D3
	ADDX.B	D3,D3
LAB_02ED:
	BCC.S	LAB_02F0
	ADD.W	D1,D1
	ADD.B	D3,D3
	BNE.S	LAB_02EE
	MOVE.B	-(A3),D3
	ADDX.B	D3,D3
LAB_02EE:
	BCC.S	LAB_02EF
	MOVEA.W	8(A1,D1.W),A2
	ADDQ.B	#8,D0
	BRA.S	LAB_02F0
LAB_02EF:
	MOVEA.W	0(A1,D1.W),A2
	ADDQ.B	#4,D0
LAB_02F0:
	MOVE.B	16(A1,D0.W),D0
LAB_02F1:
	ADD.B	D3,D3
	BNE.S	LAB_02F2
	MOVE.B	-(A3),D3
	ADDX.B	D3,D3
LAB_02F2:
	ADDX.L	D5,D5
	SUBQ.B	#1,D0
	BNE.S	LAB_02F1
	ADDQ.W	#1,A2
	ADDA.L	D5,A2
	ADDA.L	A4,A2
LAB_02F3:
	MOVE.B	-(A2),-(A4)
	SUBQ.B	#1,D4
	BNE.S	LAB_02F3
	BRA.W	LAB_02D3
LAB_02F4:
	DC.W	$060a
	DC.W	$0a12
LAB_02F5:
	dc.w     $0101
	dc.w     $0101
	DC.W	$0203
	dc.w    $0304
	DC.W	$0405
LAB_02F6:
	dc.w    $070e
    