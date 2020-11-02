	XDEF	_GetFunctionName
	XREF	RelFun_StrcmpAsm
	XDEF	_JstVersion
	XDEF	_ConName
	XDEF	_CliVersionMess

	SECTION	"LVODummy",DATA

JOTDNAME:MACRO
	dc.b	"JOTD Startup "
	ENDM

	dc.b	0,"$VER: "
	JOTDNAME
	;incbin	"T:jst_date"
	dc.b	"99/99/9999"
	dc.b	" © JF Fabre, R Huvendiek.",0
	cnop	0,4

DECL_VERSION:MACRO
	dc.b	"6.5"
	ENDM
	
_JstVersion:
	DECL_VERSION
	dc.b	0

_ConName:
	dc.b	"CON:20/20/350/200/"
	JOTDNAME
	DECL_VERSION
	dc.b	" by JFFabre & RHuvendiek/CLOSE",0

_CliVersionMess:
	JOTDNAME
	DECL_VERSION
	dc.b	" by Jean-Francois Fabre & Ralf Huvendiek",$0A,$0D,0
	even

; < D0: function offset
; < A1: pointer on system part (without the dot)

; > A1: pointer on the function (or 0 if not found)

_GetFunctionName:
	lea	dummyname(pc),A1
	rts

dummyname:
	dc.b	"Not avail",0
