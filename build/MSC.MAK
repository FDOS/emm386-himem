#
# MSC.MAK - kernel compiler options for MS VC 1.5x (MS CL 8.x)
#

TARGET=MS$(XCPU)
DEPENDS=$(DEPENDS) msc.mak msc.cfg

CC=$(BINPATH)\cl -c
CL=$(BINPATH)\cl

# used for building the library

CLIB=$(LIBPATH)\slibce.lib
RTL_EXTRACT=*aflmul *aFlshl *aFNauldi *aFulrem *aFulshr *aFuldiv *aFlrem *aFldiv *aFNaulsh
RTL_INSERT =+aflmul +aFlshl +aFNauldi +aFulrem +aFulshr +aFuldiv +aFlrem +aFldiv +aFNaulsh

#

!if $(XCPU) == 186
CPUOPT=-G1
!endif
!if $(XCPU) == 386
CPUOPT=-G3
!endif

CFLAGS0=@msc.cfg -I$(INCLUDEPATH) $(CPUOPT)
CFLAGSCOM=/Fm /AT /Os /Zp1
CFLAGSEXE=/Fm /AL /Os /Zp1
