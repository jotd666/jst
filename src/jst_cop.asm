
; *** Search for move.w #blit,($58,Ax) (6 bytes)

; < A0: start address
; < A1: end address
; < D0: register number (0-6)
; < D1: trap to use to do the patch operation (0-15)

RelFun_PatchMoveBlit_Idx:
	STORE_REGS
	cmp.l	#7,D0
	bcc	.exit		; >6: out!

	cmp.l	#$10,D1
	bcc	.exit		; >15: out!

	move.w	#$317C,D2	; start of move.w #xxxx,($58,A0)
	add.w	D0,D0		; D0*2
	lsl.w	#8,D0		; D0*$100

	or.w	D0,D2		; compose the move.w #xxxx,($58,Ax)

	move.l	D1,A3
	ori.w	#$4E40,D1	; compose the TRAP #x

	add.l	A3,A3
	add.l	A3,A3		; A3 * 4

	lea	DoAndWaitBlit_6(pc),A2
	move.l	A2,($80,A3)	; sets trap in trap table

.loop
	move.w	(A0)+,D3
	cmp.w	D2,D3		; first part
	beq.b	.found1

.next
	cmp.l	A0,A1
	bcc.b	.loop
	bra.b	.exit

.found1
	move.w	(2,A0),D3
	cmp.w	#$0058,D3	; second part
	bne.b	.next
.found2
	move.w	D1,(-2,A0)	; patch
	bra.b	.next		; process next occurences
.exit

	; the code has changed: we flush the caches

	bsr	RelFun_FlushCachesHard_proxy
	RESTORE_REGS
	rts


DoAndWaitBlit_6:
	STORE_REGS	A0
	move.l	(6,A7),A0		; return PC
	bsr	RelFun_WaitBlit		; waits for blitter operation end
	move.w	(A0),$DFF058		; do the blit
	RESTORE_REGS	A0
	ADD.L	#4,(2,A7)		; skip 4 bytes (remainder of move.w instruction)
	RTE

; *** Search for move.l #coplist,($80,Ax) (8 bytes)

; < A0: start address
; < A1: end address
; < D0: register number (0-6)
; < D1: trap to use to do the patch operation (0-15)

RelFun_PatchMoveCList_Idx:
	STORE_REGS
	cmp.l	#7,D0
	bcc	.exit		; >6: out!

	cmp.l	#$10,D1
	bcc	.exit		; >15: out!

	move.w	#$217C,D2	; start of move.l #xxxx,($80,A0)
	add.w	D0,D0		; D0*2
	lsl.w	#8,D0		; D0*$100

	or.w	D0,D2		; compose the move.l #xxxx,($80,Ax)

	move.l	D1,A3
	ori.w	#$4E40,D1	; compose the TRAP #x

	add.l	A3,A3
	add.l	A3,A3		; A3 * 4

	lea	StoreAndSetCopper(pc),A2
	move.l	A2,($80,A3)	; sets trap in trap table

.loop
	move.w	(A0)+,D3
	cmp.w	D2,D3		; first part
	beq.b	.found1

.next
	cmp.l	A0,A1
	bcc.b	.loop
	bra.b	.exit

.found1
	move.w	(4,A0),D3
	cmp.w	#$0080,D3	; second part
	bne.b	.next
.found2
	cmp.l	#$200000,(A0)	; in chipmem?
	bcc.b	.next		; no, don't patch (would not harm, though)
	move.w	D1,(-2,A0)	; patch
	bra.b	.next		; process next occurences
.exit

	; the code has changed: we flush the caches

	bsr	RelFun_FlushCachesHard_proxy
	RESTORE_REGS
	rts


; *** Search for move.l #coplist,$DFF080 (10 bytes)

; < A0: start address
; < A1: end address
; < D1: trap to use to do the patch operation (0-15)

RelFun_PatchMoveCList_Abs:
	STORE_REGS
	cmp.l	#$10,D1
	bcc	.exit		; >15: out!

	move.w	#$23FC,D2	; start of move.l #xxxx,$DFF080
	move.l	#$DFF080,D0	; end of that instruction

	move.l	D1,A3
	ori.w	#$4E40,D1	; compose the TRAP #x

	add.l	A3,A3
	add.l	A3,A3		; A3 * 4

	lea	StoreAndSetCopper(pc),A2
	move.l	A2,($80,A3)	; sets trap in trap table

.loop
	move.w	(A0)+,D3
	cmp.w	D2,D3		; first part
	beq.b	.found1

.next
	cmp.l	A0,A1
	bcc.b	.loop
	bra.b	.exit

.found1
	move.l	(4,A0),D3
	cmp.l	D0,D3		; second part
	bne.b	.next
.found2
	cmp.l	#$200000,(A0)	; in chipmem?
	bcc.b	.next		; no, don't patch (would not harm, though)
	move.w	D1,(-2,A0)	; patch
	move.l	#$4E71,(4,A0)	; NOP on last word (to keep same storecop trap)
	bra.b	.next		; process next occurences
.exit

	; the code has changed: we flush the caches

	bsr	RelFun_FlushCachesHard_proxy
	RESTORE_REGS
	rts

; *** Search for move.l coplist,$DFF080 (10 bytes)

; < A0: start address
; < A1: end address
; < D1: trap to use to do the patch operation (0-15)

RelFun_PatchMoveCList_Ind:
	STORE_REGS
	cmp.l	#$10,D1
	bcc	.exit		; >15: out!

	move.w	#$23F9,D2	; start of move.l #xxxx,$DFF080
	move.l	#$DFF080,D0	; end of that instruction

	move.l	D1,A3
	ori.w	#$4E40,D1	; compose the TRAP #x

	add.l	A3,A3
	add.l	A3,A3		; A3 * 4

	lea	StoreAndSetCopper_Ind(pc),A2
	move.l	A2,($80,A3)	; sets trap in trap table

.loop
	move.w	(A0)+,D3
	cmp.w	D2,D3		; first part
	beq.b	.found1

.next
	cmp.l	A0,A1
	bcc.b	.loop
	bra.b	.exit

.found1
	move.l	(4,A0),D3
	cmp.l	D0,D3		; second part
	bne.b	.next
.found2
	cmp.l	#$200000,(A0)	; in chipmem?
	bcc.b	.next		; no, don't patch (would not harm, though)
	move.w	D1,(-2,A0)	; patch
	move.l	#$4E71,(4,A0)	; NOP on last word (to keep same storecop trap)
	bra.b	.next		; process next occurences
.exit

	; the code has changed: we flush the caches

	bsr	RelFun_FlushCachesHard_proxy
	RESTORE_REGS
	rts

; *** Store copperlist pointer in memory and in the copper register

StoreAndSetCopper:
	STORE_REGS
	move.l	($3E,A7),A0	; return PC
	move.l	(A0),D0		; address of the copperlist
	bsr	RelFun_StoreCopperPointer	; store address
	move.l	D0,$DFF080	; sets pointer
	RESTORE_REGS
	ADD.L	#6,(2,A7)	; skip 6 bytes (remainder of move.l instruction)
	RTE

; *** Store copperlist pointer in memory and in the copper register
; (indirect addressing)

StoreAndSetCopper_Ind:
	STORE_REGS
	move.l	($3E,A7),A0	; return PC
	move.l	(A0),A0		; address of the copperlist
	move.l	(A0),D0		; value
	bsr	RelFun_StoreCopperPointer	; store address
	move.l	D0,$DFF080	; sets pointer
	RESTORE_REGS
	ADD.L	#6,(2,A7)	; skip 6 bytes (remainder of move.l instruction)
	RTE

; *** Store copperlist pointer in memory

; < D0: value

RelFun_StoreCopperPointer:
	STORE_REGS	A4
	SET_VAR_CONTEXT
	SETVAR_L	D0,copper_pointer
	RESTORE_REGS	A4
	rts

; *** Store copperlist pointer in memory

; > D0: value

RelFun_TellCopperPointer:
	STORE_REGS	A4
	SET_VAR_CONTEXT
	GETVAR_L	copper_pointer,D0
	bmi.b	.trysnoop
.exit
	RESTORE_REGS	A4
	rts

; the copperlist pointer was not set "by hand"
; but it is possible to read into custom snoop buffer
; if SNOOPCUSTOM is activated
;
; Priority:
;
; 1) Copper set by hand by the slave
; 2) SNOOPCUSTOM buffer
; 3) FORCECLIST tooltype value

.trysnoop
	TSTVAR_L	custom_mirror
	beq.b	.tryforce			; no custom mirror, try FORCECLIST
	STORE_REGS	A0
	GETVAR_L	custom_mirror,A0
	move.l	($80,A0),D0
	RESTORE_REGS	A0
	tst.l	D0
	bne.b	.exit			; copperlist found
.nocopper:
	moveq.l	#-1,D0
	bra.b	.exit

.tryforce:
	GETVAR_L	forceclistnum,D0
	beq.b	.nocopper
	bra.b	.exit

; *** Iconify the game

RelFun_InGameIconify:
	STORE_REGS
	SET_VAR_CONTEXT

	TSTVAR_L	quiet_flag
	bne.b	.exit			; operation not allowed with QUIET flag
	TSTVAR_W	iconify_lock
	bne.b	.exit			; operation not allowed during RAM loads
	bsr	RelFun_TellCopperPointer
	tst.l	D0
	bmi.b	.exit			; pointer = -1: no iconify

	INGAMEOSCALL	InGameIconify

	bsr	RelFun_TellCopperPointer
	move.l	D0,$DFF080		; modify COPJMP1
.exit
	RESTORE_REGS
	rts
