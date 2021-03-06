
; *** Decrunches a RNC type 1 file (Rob Northen Cruncher, header: RNC\01)
; *** and decrypts it
; *** Ripped from Walker game

; in: A0: crunched buffer start
; in: A1: destination (may be the same !!)
; in: D0: 16-32 bit key

	MOVEM.L	D0-D7/A0-A6,-(A7)	;0000: 48E7FFFE
	lea		_rnc_savestack(pc),a2
	move.l	A7,(A2)
	lea	_rnc_stack_top(pc),A7
	LEA	-384(A7),A7		;0004: 4FEFFE80
	MOVEA.L	A7,A2			;0008: 244F
	MOVE	D0,D5			;000A: 3A00
	BSR	.lb_0017		;000C: 61000176
	MOVEQ	#0,D1			;0010: 7200
	CMP.L	#$524E4301,D0		;0012: B0BC524E4301
	BNE	.lb_000E		;0018: 66000102
	BSR	.lb_0017		;001C: 61000166
	MOVE.L	D0,384(A7)		;0020: 2F400180
	LEA	10(A0),A3		;0024: 47E8000A
	MOVEA.L	A1,A5			;0028: 2A49
	LEA	0(A5,D0.L),A6		;002A: 4DF50800
	BSR	.lb_0017		;002E: 61000154
	LEA	0(A3,D0.L),A4		;0032: 49F30800
	CLR	-(A7)			;0036: 4267
	CMPA.L	A4,A5			;0038: BBCC
	BCC.S	.lb_0005		;003A: 6450
	MOVEQ	#0,D0			;003C: 7000
	MOVE.B	-2(A3),D0		;003E: 102BFFFE
	LEA	0(A6,D0.L),A0		;0042: 41F60800
	CMPA.L	A4,A0			;0046: B1CC
	BLS.S	.lb_0005		;0048: 6342
	ADDQ	#2,A7			;004A: 544F
	MOVE.L	A4,D0			;004C: 200C
	BTST	#0,D0			;004E: 08000000
	BEQ.S	.lb_0000		;0052: 6704
	ADDQ	#1,A4			;0054: 524C
	ADDQ	#1,A0			;0056: 5248
.lb_0000:
	MOVE.L	A0,D0			;0058: 2008
	BTST	#0,D0			;005A: 08000000
	BEQ.S	.lb_0001		;005E: 6702
	ADDQ	#1,A0			;0060: 5248
.lb_0001:
	MOVEQ	#0,D0			;0062: 7000
.lb_0002:
	CMPA.L	A0,A6			;0064: BDC8
	BEQ.S	.lb_0003		;0066: 6708
	MOVE.B	-(A0),D1		;0068: 1220
	MOVE	D1,-(A7)		;006A: 3F01
	ADDQ.B	#1,D0			;006C: 5200
	BRA.S	.lb_0002		;006E: 60F4
.lb_0003:
	MOVE	D0,-(A7)		;0070: 3F00
	ADDA.L	D0,A0			;0072: D1C0
	MOVE	D5,-(A7)		;0074: 3F05
.lb_0004:
	LEA	-32(A4),A4		;0076: 49ECFFE0
	MOVEM.L	(A4),D0-D7		;007A: 4CD400FF
	MOVEM.L	D0-D7,-(A0)		;007E: 48E0FF00
	CMPA.L	A3,A4			;0082: B9CB
	BHI.S	.lb_0004		;0084: 62F0
	SUBA.L	A4,A3			;0086: 97CC
	ADDA.L	A0,A3			;0088: D7C8
	MOVE	(A7)+,D5		;008A: 3A1F
.lb_0005:
	MOVEQ	#0,D7			;008C: 7E00
	MOVE.B	1(A3),D6		;008E: 1C2B0001
	ROL	#8,D6			;0092: E15E
	MOVE.B	(A3),D6			;0094: 1C13
	MOVEQ	#2,D0			;0096: 7002
	MOVEQ	#2,D1			;0098: 7202
	BSR	.lb_0014		;009A: 610000C4
.lb_0006:
	MOVEA.L	A2,A0			;009E: 204A
	BSR	.lb_0019		;00A0: 610000EE
	LEA	128(A2),A0		;00A4: 41EA0080
	BSR	.lb_0019		;00A8: 610000E6
	LEA	256(A2),A0		;00AC: 41EA0100
	BSR	.lb_0019		;00B0: 610000DE
	MOVEQ	#-1,D0			;00B4: 70FF
	MOVEQ	#16,D1			;00B6: 7210
	BSR	.lb_0014		;00B8: 610000A6
	MOVE	D0,D4			;00BC: 3800
	SUBQ	#1,D4			;00BE: 5344
	BRA.S	.lb_0009		;00C0: 601C
.lb_0007:
	LEA	128(A2),A0		;00C2: 41EA0080
	MOVEQ	#0,D0			;00C6: 7000
	BSR.S	.lb_0010		;00C8: 6160
	NEG.L	D0			;00CA: 4480
	LEA	-1(A5,D0.L),A1		;00CC: 43F508FF
	LEA	256(A2),A0		;00D0: 41EA0100
	BSR.S	.lb_0010		;00D4: 6154
	MOVE.B	(A1)+,(A5)+		;00D6: 1AD9
.lb_0008:
	MOVE.B	(A1)+,(A5)+		;00D8: 1AD9
	DBF	D0,.lb_0008		;00DA: 51C8FFFC
.lb_0009:
	MOVEA.L	A2,A0			;00DE: 204A
	BSR.S	.lb_0010		;00E0: 6148
	SUBQ	#1,D0			;00E2: 5340
	BMI.S	.lb_000B		;00E4: 6B20
.lb_000A:
	MOVE.B	(A3)+,(A5)+		;00E6: 1ADB
	EOR.B	D5,-1(A5)		;00E8: BB2DFFFF
	DBF	D0,.lb_000A		;00EC: 51C8FFF8
	ROR	#1,D5			;00F0: E25D
	MOVE.B	1(A3),D0		;00F2: 102B0001
	ROL	#8,D0			;00F6: E158
	MOVE.B	(A3),D0			;00F8: 1013
	LSL.L	D7,D0			;00FA: EFA8
	MOVEQ	#1,D1			;00FC: 7201
	LSL	D7,D1			;00FE: EF69
	SUBQ	#1,D1			;0100: 5341
	AND.L	D1,D6			;0102: CC81
	OR.L	D0,D6			;0104: 8C80
.lb_000B:
	DBF	D4,.lb_0007		;0106: 51CCFFBA
	CMPA.L	A6,A5			;010A: BBCE
	BCS.S	.lb_0006		;010C: 6590
	MOVE	(A7)+,D0		;010E: 301F
	BEQ.S	.lb_000D		;0110: 6708
.lb_000C:
	MOVE	(A7)+,D1		;0112: 321F
	MOVE.B	D1,(A5)+		;0114: 1AC1
	SUBQ.B	#1,D0			;0116: 5300
	BNE.S	.lb_000C		;0118: 66F8
.lb_000D:
	BRA.S	.lb_000F		;011A: 6004
.lb_000E:
	MOVE.L	D1,384(A7)		;011C: 2F410180
.lb_000F:
	move.l	_rnc_savestack(pc),A7
	move.l	_rnc_stack_top(pc),(A7)	; copy value so restored in D0
	movem.l	(A7)+,D0-A6
	rts
.lb_0010:
	MOVE	(A0)+,D0		;012A: 3018
	AND	D6,D0			;012C: C046
	SUB	(A0)+,D0		;012E: 9058
	BNE.S	.lb_0010		;0130: 66F8
	MOVE.B	60(A0),D1		;0132: 1228003C
	SUB.B	D1,D7			;0136: 9E01
	BGE.S	.lb_0011		;0138: 6C02
	BSR.S	.lb_0016		;013A: 6130
.lb_0011:
	LSR.L	D1,D6			;013C: E2AE
	MOVE.B	61(A0),D0		;013E: 1028003D
	CMP.B	#$02,D0			;0142: B03C0002
	BLT.S	.lb_0013		;0146: 6D16
	SUBQ.B	#1,D0			;0148: 5300
	MOVE.B	D0,D1			;014A: 1200
	MOVE.B	D0,D2			;014C: 1400
	MOVE	62(A0),D0		;014E: 3028003E
	AND	D6,D0			;0152: C046
	SUB.B	D1,D7			;0154: 9E01
	BGE.S	.lb_0012		;0156: 6C02
	BSR.S	.lb_0016		;0158: 6112
.lb_0012:
	LSR.L	D1,D6			;015A: E2AE
	BSET	D2,D0			;015C: 05C0
.lb_0013:
	RTS				;015E: 4E75
.lb_0014:
	AND	D6,D0			;0160: C046
	SUB.B	D1,D7			;0162: 9E01
	BGE.S	.lb_0015		;0164: 6C02
	BSR.S	.lb_0016		;0166: 6104
.lb_0015:
	LSR.L	D1,D6			;0168: E2AE
	RTS				;016A: 4E75
.lb_0016:
	ADD.B	D1,D7			;016C: DE01
	LSR.L	D7,D6			;016E: EEAE
	SWAP	D6			;0170: 4846
	ADDQ	#4,A3			;0172: 584B
	MOVE.B	-(A3),D6		;0174: 1C23
	ROL	#8,D6			;0176: E15E
	MOVE.B	-(A3),D6		;0178: 1C23
	SWAP	D6			;017A: 4846
	SUB.B	D7,D1			;017C: 9207
	MOVEQ	#16,D7			;017E: 7E10
	SUB.B	D1,D7			;0180: 9E01
	RTS				;0182: 4E75
.lb_0017:
	MOVEQ	#3,D1			;0184: 7203
.lb_0018:
	LSL.L	#8,D0			;0186: E188
	MOVE.B	(A0)+,D0		;0188: 1018
	DBF	D1,.lb_0018		;018A: 51C9FFFA
	RTS				;018E: 4E75
.lb_0019:
	MOVEQ	#31,D0			;0190: 701F
	MOVEQ	#5,D1			;0192: 7205
	BSR	.lb_0014		;0194: 61CA
	SUBQ	#1,D0			;0196: 5340
	BMI.S	.lb_001F		;0198: 6B7C
	MOVE	D0,D2			;019A: 3400
	MOVE	D0,D3			;019C: 3600
	LEA	-16(A7),A7		;019E: 4FEFFFF0
	MOVEA.L	A7,A1			;01A2: 224F
.lb_001A:
	MOVEQ	#15,D0			;01A4: 700F
	MOVEQ	#4,D1			;01A6: 7204
	BSR.S	.lb_0014		;01A8: 61B6
	MOVE.B	D0,(A1)+		;01AA: 12C0
	DBF	D2,.lb_001A		;01AC: 51CAFFF6
	MOVEQ	#1,D0			;01B0: 7001
	ROR.L	#1,D0			;01B2: E298
	MOVEQ	#1,D1			;01B4: 7201
	MOVEQ	#0,D2			;01B6: 7400
	MOVEM.L	D5-D7,-(A7)		;01B8: 48E70700
.lb_001B:
	MOVE	D3,D4			;01BC: 3803
	LEA	12(A7),A1		;01BE: 43EF000C
.lb_001C:
	CMP.B	(A1)+,D1		;01C2: B219
	BNE.S	.lb_001E		;01C4: 663A
	MOVEQ	#1,D5			;01C6: 7A01
	LSL	D1,D5			;01C8: E36D
	SUBQ	#1,D5			;01CA: 5345
	MOVE	D5,(A0)+		;01CC: 30C5
	MOVE.L	D2,D5			;01CE: 2A02
	SWAP	D5			;01D0: 4845
	MOVE	D1,D7			;01D2: 3E01
	SUBQ	#1,D7			;01D4: 5347
.lb_001D:
	ROXL	#1,D5			;01D6: E355
	ROXR	#1,D6			;01D8: E256
	DBF	D7,.lb_001D		;01DA: 51CFFFFA
	MOVEQ	#16,D5			;01DE: 7A10
	SUB.B	D1,D5			;01E0: 9A01
	LSR	D5,D6			;01E2: EA6E
	MOVE	D6,(A0)+		;01E4: 30C6
	MOVE.B	D1,60(A0)		;01E6: 1141003C
	MOVE.B	D3,D5			;01EA: 1A03
	SUB.B	D4,D5			;01EC: 9A04
	MOVE.B	D5,61(A0)		;01EE: 1145003D
	MOVEQ	#1,D6			;01F2: 7C01
	SUBQ.B	#1,D5			;01F4: 5305
	LSL	D5,D6			;01F6: EB6E
	SUBQ	#1,D6			;01F8: 5346
	MOVE	D6,62(A0)		;01FA: 3146003E
	ADD.L	D0,D2			;01FE: D480
.lb_001E:
	DBF	D4,.lb_001C		;0200: 51CCFFC0
	LSR.L	#1,D0			;0204: E288
	ADDQ.B	#1,D1			;0206: 5201
	CMP.B	#$11,D1			;0208: B23C0011
	BNE.S	.lb_001B		;020C: 66AE
	MOVEM.L	(A7)+,D5-D7		;020E: 4CDF00E0
	LEA	16(A7),A7		;0212: 4FEF0010
.lb_001F:
	RTS				;0216: 4E75

; replaces stack for games not supporting it (ex: when using LoadFileDecrunch in a game
; where stack usage should be low)
_rnc_savestack
	dc.l	0
	blk.b	$280,0
_rnc_stack_top
	dc.l	0	; routine stores return value for D0 to be restored