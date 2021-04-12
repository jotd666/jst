LINK = vlink 
ASM = vasmm68k_mot
# no symbols: makes executable smaller (and this software is undebuggable with a symbolic debugger anyway)
ASMOPTS =  -x -no-opt -IK:/jff/AmigaHD/amiga39_JFF_OS/include -I../include -Fhunk -nosym -kick1hunks





INCDIR = ../include
OBJDIR = ../obj
EXEDIR = ../bin

JST_OBJS = $(OBJDIR)/jst_abs.o $(OBJDIR)/jst_rel.o $(OBJDIR)/jst_deg.o \
  $(OBJDIR)/jst_msgs.o $(OBJDIR)/jst_util.o

all : $(EXEDIR)/jst 


dist: $(EXEDIR)/jst
	copy $(EXEDIR)/jst PROJETS:JOTDHD/JOTDSTARTUP/bin
clean:
	delete $(EXEDIR)/#? $(OBJDIR)/#?.o FORCE QUIET

$(EXEDIR)/jst	: $(JST_OBJS) $(MAKEFILE) $(OBJDIR)/lvo_dummy.o
	$(LINK) -o $(EXEDIR)/jst $(JST_OBJS) $(OBJDIR)/lvo_dummy.o -Z 


# automatic date insertion

$(OBJDIR)/jst_abs.o : jst_abs.asm jst_fakepad.asm reloc_kickstart.asm \
			jst_whdinit.asm $(INCDIR)/jst_rel.i \
			$(INCDIR)/jst_macros.i $(INCDIR)/jstabs_macros.i \
			$(INCDIR)/gp_macros.i $(INCDIR)/osemu.i \
			$(INCDIR)/jst_msgs.i
	$(ASM) $(ASMOPTS) jst_abs.asm -o $(OBJDIR)/jst_abs.o

$(OBJDIR)/jst_deg.o : jst_deg.asm $(INCDIR)/jst_rel.i \
			$(INCDIR)/jst_macros.i $(INCDIR)/jstabs_macros.i \
			$(INCDIR)/gp_macros.i
	$(ASM) $(ASMOPTS) jst_deg.asm -o $(OBJDIR)/jst_deg.o

$(OBJDIR)/jst_util.o: jst_util.asm jst_util_ro.asm jst_crc16.asm
	$(ASM) $(ASMOPTS) jst_util_ro.asm -o $(OBJDIR)/jst_util.o

$(OBJDIR)/jst_rel.o: jst_rel.asm ReadJoyPad.s reloc_kickstart.asm  send_key_event.asm virtual_mouse.asm virtual_keyboard.asm \
			jst_util.asm jst_cop.asm jst_hunk.asm Emulate_MOVEP.asm \
			jst_whd.asm jst_cus.asm jst_sna.asm RNC1Decrunch.asm ATNDecrunch.asm TPWMDecrunch.asm RNC12Decrunch.asm  $(INCDIR)/jst_macros.i \
			$(INCDIR)/jstrel_macros.i $(INCDIR)/jst_rel.i \
			$(INCDIR)/gp_macros.i $(INCDIR)/osemu.i
	$(ASM) $(ASMOPTS) jst_rel.asm -o $(OBJDIR)/jst_rel.o

$(OBJDIR)/lvo_dummy.o: lvo_dummy.asm 
	$(ASM) $(ASMOPTS) lvo_dummy.asm -o $(OBJDIR)/lvo_dummy.o


$(OBJDIR)/jst_msgs.o : jst_msgs.asm
	$(ASM) $(ASMOPTS) jst_msgs.asm -o $(OBJDIR)/jst_msgs.o
