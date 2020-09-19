

; *** Decrunches a RNC type 1/2 file (Rob Northen Cruncher, header: RNC\01, RNC\02)
; *** Ripped from SSBOOT/HOOK (Cannon Fodder2, SWOS)

; in: A0: crunched buffer start
; in: A1: destination (may be the same !!)

; This type of cruncher is very heavily used in lots of games
; from EOA, Team 17 (when not using ATN!), Renegade, Akklaim, and lots of others.
; ex: Mortal Kombat

	movem.l	D0/D1/A0,-(sp)
	bsr	get_longword
	cmp.l	#$524E4302,D0
	movem.l	(sp)+,D0/D1/A0
	beq	RNC2Dec
	movem.l	D0/D1/A0,-(sp)
	bsr	get_longword
	cmp.l	#$524E4301,D0
	movem.l	(sp)+,D0/D1/A0
	beq.b	RNC1Dec_select

	rts			; exit

RNC1Dec_select:
	movem.l	D0-D1/A0-A1,-(a7)

	; compute CRC16 and see if matches, if doesn't, it's the old RN format
	lea	(8,a0),a0
	bsr	get_longword		; D0 = packed len
	move.w	#0,D1
	lea	(2,A0),A0		; skip unpacked CRC
	move.b	(a0)+,d1
	lsl.w	#8,D1
	move.b	(a0)+,d1	; expected crunched CRC
	
	lea	(2,A0),A0	; skip 2 misc bytes, start packed
	bsr	RelFun_CRC16
	cmp.w	D0,D1
	movem.l	(a7)+,D0-D1/A0-A1
	bne	RNC1DecOld
	
RNC1Dec:
	MOVEM.L        D0-D7/A0-A6,-(SP)

	lea		_rnc_savestack(pc),a2
	move.l	A7,(A2)
	lea	_rnc_stack_top(pc),A7
	LEA            -$180(SP),SP
	MOVEA.L        SP,A2
	BSR            get_longword	; RNC\01 signature
	MOVEQ          #$0,D1
	CMPI.L         #$524E4301,D0	; RNC\01 tag.
	BNE            L706
	BSR            get_longword
	MOVE.L         D0,$180(SP)	; decrunched length
	LEA            $A(A0),A3	; skip CRC
	MOVEA.L        A1,A5
	LEA            $0(A5,D0.L),A6
	BSR            get_longword
	LEA            $0(A3,D0.L),A4
	CLR.W          -(SP)
	CMPA.L         A4,A5
	BCC.S          L67C
	MOVEQ          #$0,D0
	MOVE.B         -$2(A3),D0
	LEA            $0(A6,D0.L),A0
	CMPA.L         A4,A0
	BLS.S          L67C
	ADDQ.W         #$2,SP
	MOVE.L         A4,D0
	BTST           #$0,D0
	BEQ.S          L64C
	ADDQ.W         #$1,A4
	ADDQ.W         #$1,A0
L64C
	MOVE.L         A0,D0
	BTST           #$0,D0
	BEQ.S          L656
	ADDQ.W         #$1,A0
L656    
	MOVEQ          #$0,D0
L658    
	CMPA.L         A0,A6
	BEQ.S          L664
	MOVE.B         -(A0),D1
	MOVE.W         D1,-(SP)
	ADDQ.B         #$1,D0
	BRA.S          L658
L664    
	MOVE.W         D0,-(SP)
	ADDA.L         D0,A0
L668    
	LEA            -$20(A4),A4
	MOVEM.L        (A4),D0-D7
	MOVEM.L        D0-D7,-(A0)
	CMPA.L         A3,A4
	BHI.S          L668
	SUBA.L         A4,A3
	ADDA.L         A0,A3
L67C    
	MOVEQ          #$0,D7
	MOVE.B         $1(A3),D6
	ROL.W          #$8,D6
	MOVE.B         (A3),D6
	MOVEQ          #$2,D0
	MOVEQ          #$2,D1
	BSR            L74A
L68E    
	MOVEA.L        A2,A0
	BSR            L77A
	LEA            $80(A2),A0
	BSR            L77A
	LEA            $100(A2),A0
	BSR            L77A
	MOVEQ          #-$1,D0
	MOVEQ          #$10,D1
	BSR            L74A
	MOVE.W         D0,D4
	SUBQ.W         #$1,D4
	BRA.S          L6CE
L6B2    
	LEA            $80(A2),A0
	MOVEQ          #$0,D0
	BSR.S          L714
	NEG.L          D0
	LEA            -$1(A5,D0.L),A1
	LEA            $100(A2),A0
	BSR.S          L714
	MOVE.B         (A1)+,(A5)+
L6C8    
	MOVE.B         (A1)+,(A5)+
	DBRA           D0,L6C8
L6CE    
	MOVEA.L        A2,A0
	BSR.S          L714
	SUBQ.W         #$1,D0
	BMI.S          L6F0
L6D6    
	MOVE.B         (A3)+,(A5)+
	DBRA           D0,L6D6
	MOVE.B         $1(A3),D0
	ROL.W          #$8,D0
	MOVE.B         (A3),D0
	LSL.L          D7,D0
	MOVEQ          #$1,D1
	LSL.W          D7,D1
	SUBQ.W         #$1,D1
	AND.L          D1,D6
	OR.L           D0,D6
L6F0    
	DBRA           D4,L6B2
	CMPA.L         A6,A5
	BCS.S          L68E
	MOVE.W         (SP)+,D0
	BEQ.S          L704
L6FC    
	MOVE.W         (SP)+,D1
	MOVE.B         D1,(A5)+
	SUBQ.B         #$1,D0
	BNE.S          L6FC
L704    
	BRA.S          L70A
L706    
	MOVE.L         D1,$180(SP)
L70A    
	move.l	_rnc_savestack(pc),A7	; restore stack
	move.l	_rnc_stack_top(pc),(A7)	; copy value so restored in D0
	MOVEM.L        (SP)+,D0-D7/A0-A6
	RTS

L714
	MOVE.W         (A0)+,D0
	AND.W          D6,D0
	SUB.W          (A0)+,D0
	BNE.S          L714
	MOVE.B         $3C(A0),D1
	SUB.B          D1,D7
	BGE.S          L726
	BSR.S          L756
L726
	LSR.L          D1,D6
	MOVE.B         $3D(A0),D0
	CMPI.B         #$2,D0
	BLT.S          L748
	SUBQ.B         #$1,D0
	MOVE.B         D0,D1
	MOVE.B         D0,D2
	MOVE.W         $3E(A0),D0
	AND.W          D6,D0
	SUB.B          D1,D7
	BGE.S          L744
	BSR.S          L756
L744
	LSR.L          D1,D6
	BSET           D2,D0
L748
	RTS

L74A
	AND.W          D6,D0
	SUB.B          D1,D7
	BGE.S          L752
	BSR.S          L756
L752
	LSR.L          D1,D6
	RTS

L756
	ADD.B          D1,D7
	LSR.L          D7,D6
	SWAP           D6
	ADDQ.W         #$4,A3
	MOVE.B         -(A3),D6
	ROL.W          #$8,D6
	MOVE.B         -(A3),D6
	SWAP           D6
	SUB.B          D7,D1
	MOVEQ          #$10,D7
	SUB.B          D1,D7
	RTS

;get_longword
;	MOVEQ          #$3,D1
;L770
;	LSL.L          #$8,D0
;	MOVE.B         (A0)+,D0
;	DBRA           D1,L770
;	RTS

L77A
	MOVEQ          #$1F,D0
	MOVEQ          #$5,D1
	BSR            L74A
	SUBQ.W         #$1,D0
	BMI.S          L800
	MOVE.W         D0,D2
	MOVE.W         D0,D3
	LEA            -$10(SP),SP
	MOVEA.L        SP,A1
L78E
	MOVEQ          #$F,D0
	MOVEQ          #$4,D1
	BSR.S          L74A
	MOVE.B         D0,(A1)+
	DBRA           D2,L78E
	MOVEQ          #$1,D0
	ROR.L          #$1,D0
	MOVEQ          #$1,D1
	MOVEQ          #$0,D2
	MOVEM.L        D5-D7,-(SP)
L7A6
	MOVE.W         D3,D4
	LEA            $C(SP),A1
L7AC
	CMP.B          (A1)+,D1
	BNE.S          L7EA
	MOVEQ          #$1,D5
	LSL.W          D1,D5
	SUBQ.W         #$1,D5
	MOVE.W         D5,(A0)+
	MOVE.L         D2,D5
	SWAP           D5
	MOVE.W         D1,D7
	SUBQ.W         #$1,D7
L7C0
	ROXL.W         #$1,D5
	ROXR.W         #$1,D6
	DBRA           D7,L7C0
	MOVEQ          #$10,D5
	SUB.B          D1,D5
	LSR.W          D5,D6
	MOVE.W         D6,(A0)+
	MOVE.B         D1,$3C(A0)
	MOVE.B         D3,D5
	SUB.B          D4,D5
	MOVE.B         D5,$3D(A0)
	MOVEQ          #$1,D6
	SUBQ.B         #$1,D5
	LSL.W          D5,D6
	SUBQ.W         #$1,D6
	MOVE.W         D6,$3E(A0)
	ADD.L          D0,D2
L7EA
	DBRA           D4,L7AC
	LSR.L          #$1,D0
	ADDQ.B         #$1,D1
	CMPI.B         #$11,D1
	BNE.S          L7A6
	MOVEM.L        (SP)+,D5-D7
	LEA            $10(SP),SP
L800
	RTS

RNC2Dec:
	MOVEM.L	D0-D7/A0-A6,-(A7)
	BSR	get_longword		
	MOVEQ	#0,D1			
	CMPI.L	#$524E4302,D0
	BNE	RNC2_0035		
	BSR	get_longword		
	MOVE.L	D0,(A7)			
	LEA	10(A0),A3		
	MOVEA.L	A1,A5			
	LEA	0(A5,D0.L),A6		
	BSR	get_longword		
	LEA	0(A3,D0.L),A4		
	CLR	-(A7)			
	CMPA.L	A4,A5			
	BCC.S	RNC2_0005		
	MOVEQ	#0,D0			
	MOVE.B	-2(A3),D0		
	LEA	0(A6,D0.L),A0		
	CMPA.L	A4,A0			
	BLS.S	RNC2_0005		
	ADDQ	#2,A7			
	MOVE.L	A4,D0			
	BTST	#0,D0			
	BEQ.S	RNC2_0000		
	ADDQ	#1,A4			
	ADDQ	#1,A0			
RNC2_0000:
	MOVE.L	A0,D0			
	BTST	#0,D0			
	BEQ.S	RNC2_0001		
	ADDQ	#1,A0			
RNC2_0001:
	MOVEQ	#0,D0			
RNC2_0002:
	CMPA.L	A0,A6			
	BEQ.S	RNC2_0003		
	MOVE.B	-(A0),D1		
	MOVE	D1,-(A7)		
	ADDQ.B	#1,D0			
	BRA.S	RNC2_0002		
RNC2_0003:
	MOVE	D0,-(A7)		
	ADDA.L	D0,A0			
RNC2_0004:
	LEA	-32(A4),A4		
	MOVEM.L	(A4),D0-D7		
	MOVEM.L	D0-D7,-(A0)		
	CMPA.L	A3,A4			
	BHI.S	RNC2_0004		
	SUBA.L	A4,A3			
	ADDA.L	A0,A3			
RNC2_0005:
	MOVEQ	#-128,D7		
	ADD.B	D7,D7			
	MOVE.B	(A3)+,D7		
	ADDX.B	D7,D7			
	ADD.B	D7,D7			
	BRA	RNC2_0023		
RNC2_0006:
	MOVE.B	(A3)+,D7		
	ADDX.B	D7,D7			
	BRA.S	RNC2_0010		
RNC2_0007:
	MOVE.B	(A3)+,D7		
	ADDX.B	D7,D7			
	BRA.S	RNC2_0013		
RNC2_0008:
	MOVE.B	(A3)+,D7		
	ADDX.B	D7,D7			
	BRA.S	RNC2_0014		
RNC2_0009:
	MOVE.B	(A3)+,D7		
	ADDX.B	D7,D7			
	BRA.S	RNC2_0015		
RNC2_000A:
	MOVE.B	(A3)+,D7		
	ADDX.B	D7,D7			
	BRA.S	RNC2_0017		
RNC2_000B:
	MOVE.B	(A3)+,D7		
	ADDX.B	D7,D7			
	BRA.S	RNC2_0018		
RNC2_000C:
	MOVE.B	(A3)+,D7		
	ADDX.B	D7,D7			
	BRA.S	RNC2_0019		
RNC2_000D:
	MOVE.B	(A3)+,D7		
	ADDX.B	D7,D7			
	BRA.S	RNC2_001B		
RNC2_000E:
	MOVEQ	#3,D5			
RNC2_000F:
	ADD.B	D7,D7			
	BEQ.S	RNC2_0006		
RNC2_0010:
	ADDX	D6,D6			
	DBF	D5,RNC2_000F		
	ADDQ	#2,D6			
RNC2_0011:
	MOVE.B	(A3)+,(A5)+		
	MOVE.B	(A3)+,(A5)+		
	MOVE.B	(A3)+,(A5)+		
	MOVE.B	(A3)+,(A5)+		
	DBF	D6,RNC2_0011		
	BRA.S	RNC2_0023		
RNC2_0012:
	ADD.B	D7,D7			
	BEQ.S	RNC2_0007		
RNC2_0013:
	ADDX	D5,D5			
	ADD.B	D7,D7			
	BEQ.S	RNC2_0008		
RNC2_0014:
	BCC.S	RNC2_0016		
	SUBQ	#1,D5			
	ADD.B	D7,D7			
	BEQ.S	RNC2_0009		
RNC2_0015:
	ADDX	D5,D5			
	CMPI.B	#$09,D5			
	BEQ.S	RNC2_000E		
RNC2_0016:
	ADD.B	D7,D7			
	BEQ.S	RNC2_000A		
RNC2_0017:
	BCC.S	RNC2_001D		
	ADD.B	D7,D7			
	BEQ.S	RNC2_000B		
RNC2_0018:
	ADDX	D6,D6			
	ADD.B	D7,D7			
	BEQ.S	RNC2_000C		
RNC2_0019:
	BCS.S	RNC2_0029		
	TST	D6			
	BNE.S	RNC2_001C		
	ADDQ	#1,D6			
RNC2_001A:
	ADD.B	D7,D7			
	BEQ.S	RNC2_000D		
RNC2_001B:
	ADDX	D6,D6			
RNC2_001C:
	ROL	#8,D6			
RNC2_001D:
	MOVE.B	(A3)+,D6		
	MOVEA.L	A5,A0			
	SUBA	D6,A0			
	SUBQ	#1,A0			
	LSR	#1,D5			
	BCC.S	RNC2_001E		
	MOVE.B	(A0)+,(A5)+		
RNC2_001E:
	SUBQ	#1,D5			
	TST	D6			
	BNE.S	RNC2_0020		
	MOVE.B	(A0),D6			
RNC2_001F:
	MOVE.B	D6,(A5)+		
	MOVE.B	D6,(A5)+		
	DBF	D5,RNC2_001F		
	BRA.S	RNC2_0023		
RNC2_0020:
	MOVE.B	(A0)+,(A5)+		
	MOVE.B	(A0)+,(A5)+		
	DBF	D5,RNC2_0020		
	BRA.S	RNC2_0023		
RNC2_0021:
	MOVE.B	(A3)+,D7		
	ADDX.B	D7,D7			
	BCS.S	RNC2_0025		
RNC2_0022:
	MOVE.B	(A3)+,(A5)+		
RNC2_0023:
	ADD.B	D7,D7			
	BCS.S	RNC2_0024		
	MOVE.B	(A3)+,(A5)+		
	ADD.B	D7,D7			
	BCC.S	RNC2_0022		
RNC2_0024:
	BEQ.S	RNC2_0021		
RNC2_0025:
	MOVEQ	#2,D5			
	MOVEQ	#0,D6			
	ADD.B	D7,D7			
	BEQ.S	RNC2_002C		
RNC2_0026:
	BCC	RNC2_0012		
	ADD.B	D7,D7			
	BEQ.S	RNC2_002D		
RNC2_0027:
	BCC.S	RNC2_001D		
	ADDQ	#1,D5			
	ADD.B	D7,D7			
	BEQ.S	RNC2_002E		
RNC2_0028:
	BCC.S	RNC2_0016		
	MOVE.B	(A3)+,D5		
	BEQ.S	RNC2_0031		
	ADDQ	#8,D5			
	BRA.S	RNC2_0016		
RNC2_0029:
	ADD.B	D7,D7			
	BEQ.S	RNC2_002F		
RNC2_002A:
	ADDX	D6,D6			
	ORI	#$0004,D6		
	ADD.B	D7,D7			
	BEQ.S	RNC2_0030		
RNC2_002B:
	BCS.S	RNC2_001C		
	BRA.S	RNC2_001A		
RNC2_002C:
	MOVE.B	(A3)+,D7		
	ADDX.B	D7,D7			
	BRA.S	RNC2_0026		
RNC2_002D:
	MOVE.B	(A3)+,D7		
	ADDX.B	D7,D7			
	BRA.S	RNC2_0027		
RNC2_002E:
	MOVE.B	(A3)+,D7		
	ADDX.B	D7,D7			
	BRA.S	RNC2_0028		
RNC2_002F:
	MOVE.B	(A3)+,D7		
	ADDX.B	D7,D7			
	BRA.S	RNC2_002A		
RNC2_0030:
	MOVE.B	(A3)+,D7		
	ADDX.B	D7,D7			
	BRA.S	RNC2_002B		
RNC2_0031:
	ADD.B	D7,D7			
	BNE.S	RNC2_0032		
	MOVE.B	(A3)+,D7		
	ADDX.B	D7,D7			
RNC2_0032:
	BCS.S	RNC2_0023		
	MOVE	(A7)+,D0		
	BEQ.S	RNC2_0034		
RNC2_0033:
	MOVE	(A7)+,D1		
	MOVE.B	D1,(A5)+		
	SUBQ.B	#1,D0			
	BNE.S	RNC2_0033		
RNC2_0034:
	BRA.S	RNC2_0036		
RNC2_0035:
	MOVE.L	D1,(A7)			
RNC2_0036:
	MOVEM.L	(A7)+,D0-D7/A0-A6
	RTS				
get_longword:
	MOVEQ	#3,D1			
RNC2_0038:
	LSL.L	#8,D0			
	MOVE.B	(A0)+,D0		
	DBF	D1,RNC2_0038		
	RTS				

; in: A0: crunched buffer start
; in: A1: destination
; (old RNC decrunch ripped from Rodland)

RNC1DecOld:

	MOVEM.L	D1-D7/A0-A6,-(A7)	;00142: 48e77ffe
	BSR.S	.lb_000D		;00148: 6158
	CMP.L	#$524e4301,D0		;0014a: b0bc524e4301
	BNE.S	.lb_000B		;00150: 664a
	BSR.S	.lb_000D		;00152: 614e
	LEA	4(A0),A4		;00154: 49e80004
	LEA	0(A4,D0.L),A2		;00158: 45f40800	; end of decrunched data
	ADDA.W	#$0100,A2		;0015c: d4fc0100
	MOVEA.L	A2,A3			;00160: 264a
	BSR.S	.lb_000D		;00162: 613e
	LEA	0(A4,D0.L),A6		;00164: 4df40800
	MOVE.B	-(A6),D3		;00168: 1626
.lb_0008:
	BSR.S	.lb_000F		;0016a: 6142
	ADDQ.W	#1,D5			;0016c: 5245
	CMPA.L	A4,A6			;0016e: bdcc
	BLE.S	.lb_000A		;00170: 6f22
	BSR.W	.lb_001A		;00172: 61000090
	BSR.W	.lb_0021		;00176: 610000c0
	SUBQ.W	#1,D6			;0017a: 5346
	LEA	0(A3,D7.W),A0		;0017c: 41f37000
	EXT.L	D6			;00180: 48c6
	ADDA.L	D6,A0			;00182: d1c6
	TST.W	D7			;00184: 4a47
	BNE.S	.lb_0009		;00186: 6604
	LEA	1(A3),A0		;00188: 41eb0001
.lb_0009:
	MOVE.B	-(A0),-(A3)		;0018c: 1720
	DBF	D6,.lb_0009		;0018e: 51cefffc
	BRA.S	.lb_0008		;00192: 60d6
.lb_000A:
	MOVE.L	A2,D0			;00194: 200a
	SUB.L	A3,D0			;00196: 908b
	MOVEA.L	A3,A0			;00198: 204b
	BRA.S	.lb_000C		;0019a: 6002
.lb_000B:
	MOVEQ	#0,D0			;0019c: 7000
.lb_000C:
	BRA.W	.lb_0029		;0019e: 600000e6
.lb_000D:
	MOVEQ	#3,D1			;001a2: 7203
.lb_000E:
	LSL.L	#8,D0			;001a4: e188
	MOVE.B	(A0)+,D0		;001a6: 1018
	DBF	D1,.lb_000E		;001a8: 51c9fffa
	RTS				;001ac: 4e75
.lb_000F:
	MOVEQ	#-1,D5			;001ae: 7aff
	BSR.S	.lb_0018		;001b0: 6148
	BCC.S	.lb_0015		;001b2: 643c
	MOVEQ	#0,D5			;001b4: 7a00
	BSR.S	.lb_0018		;001b6: 6142
	BCC.S	.lb_0013		;001b8: 642c
	MOVEQ	#3,D1			;001ba: 7203
.lb_0010:
	CLR.W	D5			;001bc: 4245
	MOVE.B	.lb_0016(PC,D1.W),D0	;001be: 103b1032
	EXT.W	D0			;001c2: 4880
	MOVEQ	#-1,D2			;001c4: 74ff
	LSL.W	D0,D2			;001c6: e16a
	NOT.W	D2			;001c8: 4642
	SUBQ.W	#1,D0			;001ca: 5340
.lb_0011:
	BSR.S	.lb_0018		;001cc: 612c
	ROXL.W	#1,D5			;001ce: e355
	DBF	D0,.lb_0011		;001d0: 51c8fffa
	TST.W	D1			;001d4: 4a41
	BEQ.S	.lb_0012		;001d6: 6706
	CMP.W	D5,D2			;001d8: b445
	DBNE	D1,.lb_0010		;001da: 56c9ffe0
.lb_0012:
	MOVE.B	.lb_0017(PC,D1.W),D0	;001de: 103b1016
	EXT.W	D0			;001e2: 4880
	ADD.W	D0,D5			;001e4: da40
.lb_0013:
	MOVE.W	D5,-(A7)		;001e6: 3f05
.lb_0014:
	MOVE.B	-(A6),-(A3)		;001e8: 1726
	DBF	D5,.lb_0014		;001ea: 51cdfffc
	MOVE.W	(A7)+,D5		;001ee: 3a1f
.lb_0015:
	RTS				;001f0: 4e75
.lb_0016:
	DC.W	$0a03			;001f2
	DC.W	$0202			;001f4
.lb_0017:
	DC.W	$0e07			;001f6
	DC.W	$0401			;001f8
.lb_0018:
	LSL.B	#1,D3			;001fa: e30b
	BNE.S	.lb_0019		;001fc: 6604
	MOVE.B	-(A6),D3		;001fe: 1626
	ROXL.B	#1,D3			;00200: e313
.lb_0019:
	RTS				;00202: 4e75
.lb_001A:
	MOVEQ	#3,D0			;00204: 7003
.lb_001B:
	BSR.S	.lb_0018		;00206: 61f2
	BCC.S	.lb_001C		;00208: 6404
	DBF	D0,.lb_001B		;0020a: 51c8fffa
.lb_001C:
	CLR.W	D6			;0020e: 4246
	ADDQ.W	#1,D0			;00210: 5240
	MOVE.B	.lb_001F(PC,D0.W),D1	;00212: 123b001a
	BEQ.S	.lb_001E		;00216: 670c
	EXT.W	D1			;00218: 4881
	SUBQ.W	#1,D1			;0021a: 5341
.lb_001D:
	BSR.S	.lb_0018		;0021c: 61dc
	ROXL.W	#1,D6			;0021e: e356
	DBF	D1,.lb_001D		;00220: 51c9fffa
.lb_001E:
	MOVE.B	.lb_0020+1(PC,D0.W),D1	;00224: 123b000d
	EXT.W	D1			;00228: 4881
	ADD.W	D1,D6			;0022a: dc41
	RTS				;0022c: 4e75
.lb_001F:
	DC.W	$0a02			;0022e
	dc.w	$0100
.lb_0020:
	DC.W	$000a			;00232
	DC.W	$0604			;00234
	dc.w    $0302
.lb_0021:
	MOVEQ	#0,D7			;00238: 7e00
	CMP.W	#$0002,D6		;0023a: bc7c0002
	BEQ.S	.lb_0025		;0023e: 6722
	MOVEQ	#1,D0			;00240: 7001
.lb_0022:
	BSR.S	.lb_0018		;00242: 61b6
	BCC.S	.lb_0023		;00244: 6404
	DBF	D0,.lb_0022		;00246: 51c8fffa
.lb_0023:
	ADDQ.W	#1,D0			;0024a: 5240
	MOVE.B	.lb_0027(PC,D0.W),D1	;0024c: 123b002c
	EXT.W	D1			;00250: 4881
.lb_0024:
	BSR.S	.lb_0018		;00252: 61a6
	ROXL.W	#1,D7			;00254: e357
	DBF	D1,.lb_0024		;00256: 51c9fffa
	LSL.W	#1,D0			;0025a: e348
	ADD.W	.lb_0028(PC,D0.W),D7	;0025c: de7b0020
	RTS				;00260: 4e75
.lb_0025:
	MOVEQ	#5,D0			;00262: 7005
	CLR.W	D1			;00264: 4241
	BSR.S	.lb_0018		;00266: 6192
	BCC.S	.lb_0026		;00268: 6404
	MOVEQ	#8,D0			;0026a: 7008
	MOVEQ	#64,D1			;0026c: 7240
.lb_0026:
	BSR.S	.lb_0018		;0026e: 618a
	ROXL.W	#1,D7			;00270: e357
	DBF	D0,.lb_0026		;00272: 51c8fffa
	ADD.W	D1,D7			;00276: de41
	RTS				;00278: 4e75
.lb_0027:
	dc.w  $0b04
	dc.w  $0700
.lb_0028:
	dc.w	$0120
	dc.l 	$00000020
	DC.W	$0000			;00284
.lb_0029:
	MOVE.L	A0,D2			;00286: 2408
	SUB.L	A1,D2			;00288: 9489
	MOVE.L	D0,D1			;0028a: 2200
	BEQ.S	.lb_002C		;0028c: 670c
.lb_002A:
	MOVE.B	(A0)+,(A1)+		;0028e: 12d8
	SUBQ.L	#1,D1			;00290: 5381
	BNE.S	.lb_002A		;00292: 66fa
.lb_002B:
	CLR.B	(A1)+			;00294: 4219
	SUBQ.L	#1,D2			;00296: 5382
	BNE.S	.lb_002B		;00298: 66fa
.lb_002C:
	MOVEM.L	(A7)+,D1-D7/A0-A6	;0029a: 4cdf7ffe
	RTS				;0029e: 4e75
.lb_002D:
	dc.l	0,0
	dc.l    $19BC0,$1420
	
