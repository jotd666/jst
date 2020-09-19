; CRC16 algorithm
; Converted from the C program CRCPPC, found on aminet.

;;M16 = $A001

crctab:
	dc.w		$0000, $C0C1, $C181, $0140, $C301, $03C0, $0280, $C241
	dc.w		$C601, $06C0, $0780, $C741, $0500, $C5C1, $C481, $0440
	dc.w		$CC01, $0CC0, $0D80, $CD41, $0F00, $CFC1, $CE81, $0E40 
	dc.w		$0A00, $CAC1, $CB81, $0B40, $C901, $09C0, $0880, $C841
	dc.w		$D801, $18C0, $1980, $D941, $1B00, $DBC1, $DA81, $1A40 
	dc.w		$1E00, $DEC1, $DF81, $1F40, $DD01, $1DC0, $1C80, $DC41 
	dc.w		$1400, $D4C1, $D581, $1540, $D701, $17C0, $1680, $D641
	dc.w		$D201, $12C0, $1380, $D341, $1100, $D1C1, $D081, $1040
	dc.w		$F001, $30C0, $3180, $F141, $3300, $F3C1, $F281, $3240
	dc.w		$3600, $F6C1, $F781, $3740, $F501, $35C0, $3480, $F441 
	dc.w		$3C00, $FCC1, $FD81, $3D40, $FF01, $3FC0, $3E80, $FE41 
	dc.w		$FA01, $3AC0, $3B80, $FB41, $3900, $F9C1, $F881, $3840
	dc.w		$2800, $E8C1, $E981, $2940, $EB01, $2BC0, $2A80, $EA41 
	dc.w		$EE01, $2EC0, $2F80, $EF41, $2D00, $EDC1, $EC81, $2C40
	dc.w		$E401, $24C0, $2580, $E541, $2700, $E7C1, $E681, $2640 
	dc.w		$2200, $E2C1, $E381, $2340, $E101, $21C0, $2080, $E041
	dc.w		$A001, $60C0, $6180, $A141, $6300, $A3C1, $A281, $6240 
	dc.w		$6600, $A6C1, $A781, $6740, $A501, $65C0, $6480, $A441 
	dc.w		$6C00, $ACC1, $AD81, $6D40, $AF01, $6FC0, $6E80, $AE41
	dc.w		$AA01, $6AC0, $6B80, $AB41, $6900, $A9C1, $A881, $6840 
	dc.w		$7800, $B8C1, $B981, $7940, $BB01, $7BC0, $7A80, $BA41
	dc.w		$BE01, $7EC0, $7F80, $BF41, $7D00, $BDC1, $BC81, $7C40
	dc.w		$B401, $74C0, $7580, $B541, $7700, $B7C1, $B681, $7640 
	dc.w		$7200, $B2C1, $B381, $7340, $B101, $71C0, $7080, $B041 
	dc.w		$5000, $90C1, $9181, $5140, $9301, $53C0, $5280, $9241 
	dc.w		$9601, $56C0, $5780, $9741, $5500, $95C1, $9481, $5440
	dc.w		$9C01, $5CC0, $5D80, $9D41, $5F00, $9FC1, $9E81, $5E40 
	dc.w		$5A00, $9AC1, $9B81, $5B40, $9901, $59C0, $5880, $9841
	dc.w		$8801, $48C0, $4980, $8941, $4B00, $8BC1, $8A81, $4A40
	dc.w		$4E00, $8EC1, $8F81, $4F40, $8D01, $4DC0, $4C80, $8C41
	dc.w		$4400, $84C1, $8581, $4540, $8701, $47C0, $4680, $8641
	dc.w		$8201, $42C0, $4380, $8341, $4100, $81C1, $8081, $4040

; < A0: pointer on zone
; < D0: length in bytes
; > D0: CRC 16

RelFun_CRC16:
	STORE_REGS	D1-A6
	move.l	D0,D7
	moveq.l	#0,D0	; initialize to 0
	tst.l	D7
	beq	.exit	; 0 length: out!

	lea	crctab(pc),A2
.loop

	moveq.l	#0,D1
	move.b	(A0)+,D1	; gets one char
	bsr		updctcr
	subq.l	#1,D7
	bne.b	.loop

.exit
	RESTORE_REGS	D1-A6
	rts

updctcr:
	move.w	D0,D4	; current CRC (tmp)
	move.w	D0,D3	; current CRC
	eor.w	D1,D4	; tmp=crc^c
	and.w	#$FF,D4	; limited to 0xFF
	add.w	D4,D4	; *2
	lsr.w	#8,D3	; crc>>8
	move.w	(A2,D4.W),D0	; crc16tab[tmp & 0xff]
	eor.w	D3,D0	; crc=(crc>>8)^crc16tab[tmp & 0xff];
	rts
