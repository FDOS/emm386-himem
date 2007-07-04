#
# makefile for EMM386,EXE and HIMEM.EXE.
# Should be called from BUILD.BAT.
#

!include "common.mak"

all: ..\..\emm386.exe emmtest.exe ..\..\himem.exe

# EMM386 ###############################################################

..\..\emm386.exe: emm386.exe
	copy emm386.exe ..\..
	$(XPACK) $@

emm386.exe: $(TARGET).lib emm386.obj emm386c.obj prf.obj useful.obj
	-$(RM) $*.lnk
	$(ECHOTO) $*.lnk emm386.obj emm386c.obj prf.obj useful.obj
	$(ECHOTO) $*.lnk $*
	$(ECHOTO) $*.lnk $*
	$(ECHOTO) $*.lnk $(TARGET).lib
	$(XLINK) @$*.lnk;

emm386.obj: emm386.asm				
emm386c.obj: emm386c.c useful.h
emmtest.exe: emmtest.c $(TARGET).lib

# HIMEM ################################################################

..\..\himem.exe: himem.exe
	copy himem.exe ..\..
	$(XPACK) $@

himem.exe: $(TARGET).lib himem64.obj himemc.obj xmstest.obj prf.obj useful.obj
	-$(RM) $*.lnk
	$(ECHOTO) $*.lnk himem64.obj himemc.obj xmstest.obj prf.obj useful.obj
	$(ECHOTO) $*.lnk $*
	$(ECHOTO) $*.lnk $*
	$(ECHOTO) $*.lnk $(TARGET).lib
	$(XLINK) @$*.lnk;

himem64.obj: himem64.asm
himemc.obj: himemc.c useful.h
xmstest.obj: xmstest.c useful.h

# Libraries ############################################################

prf.obj: prf.c portab.h
useful.obj: useful.c useful.h

$(TARGET).lib: $(CLIB) $(DEPENDS)
	-$(RM) *.lib
	$(XLIB) $(CLIB) $(RTL_EXTRACT) $(LIBCMDTAIL)
	$(XLIB) $* $(RTL_INSERT) $(LIBCMDTAIL)
	-$(RM) *.obj

# Cleaning #############################################################

clean:
	-$(RM) *.bak *.cod *.crf *.err *.las *.lib *.lnk *.lst *.map *.obj *.xrf

clobber: clean
	-$(RM) *.exe
