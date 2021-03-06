adkcon_off = RelVar_sys_adkcon-RelVar_sys_hwregs
dmacon_off = RelVar_sys_dmacon-RelVar_sys_hwregs
intena_off = RelVar_sys_intena-RelVar_sys_hwregs
intreq_off = RelVar_sys_intreq-RelVar_sys_hwregs

; *** Save readable custom registers
; *** in the custom regs system buffer

RelFun_SaveCustomRegs:
	STORE_REGS	A1/A4
	SET_VAR_CONTEXT
	LEAVAR	sys_hwregs,A1
	bsr	GetCustomRegs
	RESTORE_REGS	A1/A4
	rts

; *** Store custom registers in the given buffer
; in: A1: buffer

GetCustomRegs:
	STORE_REGS
	lea	$DFF000,A5
	move.l	A1,A3
	MOVE.W	intreqr(A5),(intreq_off,A3)	; and intreq of the running game
	ORI.W	#$8000,(intreq_off,A3)
	MOVE.W	intenar(A5),(intena_off,A3)	; saves the old values of intena
	ORI.W	#$8000,(intena_off,A3)
	MOVE.W	dmaconr(A5),(dmacon_off,A3)	; and dmacon too
	ORI.W	#$8000,(dmacon_off,A3)
	MOVE.W	adkconr(A5),(adkcon_off,A3)	; and akdcon too
	ORI.W	#$8000,(adkcon_off,A3)
	RESTORE_REGS
	rts


; *** Sets custom registers from given buffer
; in: A1: buffer

SetCustomRegs:
	STORE_REGS
	
	; remove old values

	bsr	RelFun_FreezeAll

    ; wait for VPOS to get to the bottom of screen
	move.l	#$12F00,d1
.wait
	move.l	$DFF004,d0
	cmp.l	d1,d0
	bcs.b	.wait

	; set new values

	lea	$DFF000,A5		
	move.l	A1,A3
	MOVE.W	(intreq_off,A3),intreq(A5)
	MOVE.W	(intena_off,A3),intena(A5)
	MOVE.W	(dmacon_off,A3),dmacon(A5)
	MOVE.W	(adkcon_off,A3),adkcon(A5)
	RESTORE_REGS
	rts

; *** Restore previously saved custom registers

RelFun_RestoreCustomRegs:
	STORE_REGS	A1/A4
	SET_VAR_CONTEXT
	LEAVAR	sys_hwregs,A1
	bsr	SetCustomRegs
	RESTORE_REGS	A1/A4
	rts

; *** Restore previously saved custom registers, but without DMA
; *** to avoid graphics corruption (on Shadow Fighter, for instance)

RelFun_RestoreCustomNoDMA:
	STORE_REGS	D0/A1/A2/A4
	SET_VAR_CONTEXT
	LEAVAR	sys_hwregs,A1
	LEAVAR	sysnodisp_hwregs,A2
	move.w	(A1)+,(A2)+	; copy intreq
	move.w	(A1)+,(A2)+	; copy intena
	move.w	(A1)+,D0	; dmacon
;	and.w	#$FC70,D0	; all but bitplane, copper and sound things :-)
	moveq.l	#0,D0		; finally, no DMA at all
	move.w	D0,(A2)+
	move.w	(A1)+,(A2)+	; copy adkcon
	
	LEAVAR	sysnodisp_hwregs,A1
	bsr	SetCustomRegs
	RESTORE_REGS	D0/A1/A2/A4
	rts

; *** CIA-A/B code (store/restore)

; *** timer storage offsets

TimerAA = $0
TimerAB = $5
TimerBA = $A
TimerBB = $F

CR = 0
THI = 1
TLO = 2
LHI = 3
LLO = 4


; *** Save CIA registers in a buffer
; in: A1: buffer

GetCiaRegs:
	STORE_REGS
	LEA	$BFE001,A2
	lea	$BFD000,a4

	move.l	A1,A5		; buffer for timers, base

	lea	$1E01(a4),a0
	lea	$1401(a4),a1
	lea	$1501(a4),a2
	lea	TimerAA(A5),A3	; offset
	bsr	GetTimer

	lea	$1F01(a4),a0
	lea	$1601(a4),a1
	lea     $1701(a4),a2
	lea	TimerAB(A5),A3	; offset
	bsr	GetTimer

	lea	$E00(a4),a0
	lea	$400(a4),a1
	lea	$500(a4),a2
	lea	TimerBA(A5),A3	; offset
	bsr	GetTimer

	lea	$F00(a4),a0
	lea	$600(a4),a1
	lea	$700(a4),a2
	lea	TimerBB(A5),A3	; offset
	bsr	GetTimer

	clr.b	$1E01(a4)
	clr.b	$1F01(a4)
	clr.b	$E00(a4)
	clr.b	$F00(a4)

	RESTORE_REGS
	rts

; *** Restore CIA regs from a buffer
; in: A1: buffer

SetCiaRegs:
	STORE_REGS
	lea     $BFD000,a4
	move.l	A1,A5		; timer base

	lea     $1E01(a4),a0
	lea     $1401(a4),a1
	lea     $1501(a4),a2
	lea     TimerAA(A5),a3
	bsr	SetTimer

	lea     $1F01(a4),a0
	lea     $1601(a4),a1
	lea     $1701(a4),a2
	lea     TimerAB(A5),a3
	bsr   SetTimer

	lea     $E00(a4),a0
	lea     $400(a4),a1
	lea     $500(a4),a2
	lea	TimerBA(A5),a3
	bsr   SetTimer

	lea     $F00(a4),a0
	lea     $600(a4),a1
	lea     $700(a4),a2
	lea	TimerBB(A5),a3
	bsr   SetTimer
	RESTORE_REGS
	rts


; *** get timer values
; *** thanks to Alain Malek for the source code

GetTimer:
	move.b  (a0),CR(a3)             ;store state of control register
	bclr    #0,(a0)                 ;stop the timer
	nop
	move.b  (a1),TLO(a3)            ;store the actual timer values
	move.b  (a2),THI(a3)
	bclr    #3,(a0)                 ;set continuous mode
	nop
	bclr    #1,(a0)                 ;clear PB operation mode
	nop
	bset    #4,(a0)                 ;force load latch->timer
	nop
	move.b  (a1),LLO(a3)            ;store latch values
	move.b  (a2),LHI(a3)

	;;bsr	SetTimer	; WTF: removed!!

	rts

; *** set timer values
; *** thanks to Alain Malek for the source code

SetTimer:
	clr.b   CR(a0)                  ;clear all CR values
	nop
	move.b  TLO(a3),(a1)            ;set latch to original timer value
	move.b  THI(a3),(a2)
	nop
	bset    #4,(a0)                 ;move latch->timer
	nop
	move.b  LLO(a3),(a1)            ;set latch to original latch value
	move.b  LHI(a3),(a2)
	nop
	move.b  CR(a3),(a0)             ;restore the timer's work
	rts


; *** Save CIA registers

RelFun_SaveCIARegs
	STORE_REGS	D0/A1/A4
	SET_VAR_CONTEXT			; JOTD 2016, was missing, maybe that was a problem
	LEAVAR	sys_ciaregs,A1
	bsr	GetCiaRegs
	bsr	RelFun_Priv_GetLed
	RESTORE_REGS	D0/A1/A4
	rts


RelFun_Priv_GetLed:
	STORE_REGS	D0/A4
	SET_VAR_CONTEXT
	move.b	$BFE001,D0	; CIAPRA
	lsr.b	#1,D0
	and.w	#1,D0		; changed from .b to .w, could store trash in MSB!!
	SETVAR_W	D0,ledstate		; store LED value

	; if filter off is set, then removes audio filter

	TSTVAR_L	filteroff_flag
	beq.b	.noforceoff
	bset.b	#1,$BFE001
.noforceoff
	RESTORE_REGS	D0/A4
	rts

; *** Restore CIA registers

RelFun_RestoreCIARegs
	STORE_REGS	D0/A1/A4
	SET_VAR_CONTEXT
	LEAVAR	sys_ciaregs,A1
	bsr	SetCiaRegs
	JSRGEN	ResetCIAs
	RESTORE_REGS	A1/D0/A4
	rts

RelFun_Priv_SetLed:
	STORE_REGS	A4

	; ** reset LED/Filter
	
	SET_VAR_CONTEXT

	TSTVAR_W	ledstate
	bne.b	1$
	bclr.b	#1,$BFE001
	bra.b	2$
1$
	bset.b	#1,$BFE001
2$
	RESTORE_REGS	A4
	rts

; *** Freeze DMA and interrupts

; be sure to save the custom registers before
; with _SaveCustomRegs

RelFun_FreezeAll:
	MOVE.W	#$7FFF,intena+$DFF000
	MOVE.W	#$7FFF,dmacon+$DFF000
	MOVE.W	#$7FFF,intreq+$DFF000
	MOVE.W	#$7FFF,adkcon+$DFF000
	rts
