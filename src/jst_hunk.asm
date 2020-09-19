RelFun_RelocateExecutable:
	STORE_REGS	D1-A6
	bsr		.reloc
	RESTORE_REGS	D1-A6
	rts
	
.reloc:
	incbin	"relocate.bin"