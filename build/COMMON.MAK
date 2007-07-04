# These are generic definitions

!if $(XCPU)0 == 0
XCPU=86
!endif
CPUOPT=
!if $(XCPU) == 186
CPUOPT=-1
!endif
!if $(XCPU) == 386
CPUOPT=-3
!endif

#AFLAGS=-D$(COMPILER) -DXCPU=$(XCPU) $(AFLAGS)

BINPATH=$(BASE)\bin
INCLUDEPATH=$(BASE)\include
LIBPATH=$(BASE)\lib

!if $(__MAKE__)0 == 0	# NMAKE/WMAKE
!if "$(XPACK)" == ""	# TC doesn't supports this
XPACK=rem		# NMAKE doesn't supports @ in macro
!endif
!else			# TC/BC MAKE
!if !$d(XPACK)		# NMAKE/WMAKE doesn't supports $d()
XPACK=@rem
!endif
!endif

RM=rmfiles
ECHOTO=echoto

DEPENDS=build.bat config.bat defaults.bat makefile common.mak

!include "$(COMPILER).mak"

CFLAGSCOM=$(CFLAGS0) $(CFLAGSCOM) $(CFLAGS)
CFLAGSEXE=$(CFLAGS0) $(CFLAGSEXE) $(CFLAGS)
CFLAGS=$(CFLAGS0) $(CFLAGS)

# Implicit Rules #######################################################

.asm.obj:
	$(XASM) $(AFLAGS) $<

.c.obj:
	$(CC) $(CFLAGS) $<

.cpp.obj:
	$(CC) $(CFLAGS) $<

.c.com:
	$(CL) $(CFLAGSCOM) $<

.c.exe:
	$(CL) $(CFLAGSEXE) $<
