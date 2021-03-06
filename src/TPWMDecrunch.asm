
; *** Decrunch a TPWM crunched file (TPWM)
; <   A0 start of source/dest buffer
; >   D0=0 if OK, -1 else

	CMPI.L	#$5450574D,(A0)		;00: 0C905450574D
	BNE	TPWM_0006		;06: 6600008E
	MOVE.L	4(A0),D1		;0A: 22280004
	LEA	0(A0,D0.L),A1		;0E: 43F00800
	LEA	0(A0,D1.L),A0		;12: 41F01800
	ADDA	#$0200,A0		;16: D0FC0200
	SUBI.L	#$00000008,D0		;1A: 048000000008
TPWM_0000:
	MOVE.B	-(A1),-(A0)		;20: 1121
	SUBQ.L	#1,D0			;22: 5380
	BGT.S	TPWM_0000		;24: 6EFA
	LEA	-8(A1),A1		;26: 43E9FFF8
	MOVE.L	D1,-(A7)		;2A: 2F01
	MOVEA.L	A1,A2			;2C: 2449
	ADDA.L	D1,A2			;2E: D5C1
TPWM_0001:
	MOVE.B	(A0)+,D0		;30: 1018
	MOVEQ	#7,D7			;32: 7E07
TPWM_0002:
	ADD.B	D0,D0			;34: D000
	BCS.S	TPWM_0003		;36: 6510
	MOVE.B	(A0)+,(A1)+		;38: 12D8
	CMPA.L	A1,A2			;3A: B5C9
	DBLS	D7,TPWM_0002		;3C: 53CFFFF6
	BGT	TPWM_0001		;40: 6E00FFEE
	BRA	TPWM_0005		;44: 6000004C
TPWM_0003:
	CLR	D1			;48: 4241
	MOVE.B	(A0)+,D1		;4A: 1218
	MOVE	D1,D2			;4C: 3401
	ANDI	#$000F,D1		;4E: 0241000F
	LSL	#4,D2			;52: E94A
	MOVE.B	(A0)+,D2		;54: 1418
	MOVEA.L	A1,A3			;56: 2649
	SUBA	D2,A3			;58: 96C2
	ADD	D1,D1			;5A: D241
	NEG	D1			;5C: 4441
	JMP	TPWM_0004(PC,D1.W)	;5E: 4EFB1022
	MOVE.B	(A3)+,(A1)+		;62: 12DB
	MOVE.B	(A3)+,(A1)+		;64: 12DB
	MOVE.B	(A3)+,(A1)+		;66: 12DB
	MOVE.B	(A3)+,(A1)+		;68: 12DB
	MOVE.B	(A3)+,(A1)+		;6A: 12DB
	MOVE.B	(A3)+,(A1)+		;6C: 12DB
	MOVE.B	(A3)+,(A1)+		;6E: 12DB
	MOVE.B	(A3)+,(A1)+		;70: 12DB
	MOVE.B	(A3)+,(A1)+		;72: 12DB
	MOVE.B	(A3)+,(A1)+		;74: 12DB
	MOVE.B	(A3)+,(A1)+		;76: 12DB
	MOVE.B	(A3)+,(A1)+		;78: 12DB
	MOVE.B	(A3)+,(A1)+		;7A: 12DB
	MOVE.B	(A3)+,(A1)+		;7C: 12DB
	MOVE.B	(A3)+,(A1)+		;7E: 12DB
	MOVE.B	(A3)+,(A1)+		;80: 12DB
TPWM_0004:
	MOVE.B	(A3)+,(A1)+		;82: 12DB
	MOVE.B	(A3)+,(A1)+		;84: 12DB
	MOVE.B	(A3)+,(A1)+		;86: 12DB
	CMPA.L	A1,A2			;88: B5C9
	DBLS	D7,TPWM_0002		;8A: 53CFFFA8
	BGT	TPWM_0001		;8E: 6E00FFA0
TPWM_0005:
	MOVE.L	(A7)+,D0		;92: 201F
	BRA.S	TPWM_0007		;94: 6002
TPWM_0006:
	MOVEQ	#-1,D0			;96: 70FF
TPWM_0007:
	RTS				;98: 4E75

