#
# WATCOM.MAK - kernel compiler options for WATCOM C/OpenWatcom
#

TARGET=WC$(XCPU)
DEPENDS=$(DEPENDS) watcom.mak wc.cfg

BINPATH=$(BASE)\binw
INCLUDEPATH=$(BASE)\h
LIBPATH=$(BASE)\lib286

CC=$(BINPATH)\wcc
CL=$(BINPATH)\wcl

# used for building the library

CLIB=$(LIBPATH)\dos\clibm.lib
RTL_EXTRACT=*i4m
RTL_INSERT =+i4m

#
# Compiler options for Watcom
# ---------------------------
#
# -e=<num>      set limit on number of error messages
# -w=<num>      set warning level number
# -we           treat all warnings as errors
# -zq           operate quietly
#
# -j            change char default from unsigned to signed
# -ms           small memory model (small code/small data)
# -os           -> favor code size over execution time in optimizations
# -s            remove stack overflow checks
# -ze           enable extensions (i.e., near, far, export, etc.)
# -zl           remove default library information
# -zp=<num>     pack structure members with alignment {1,2,4,8,16}
#
# -3		optimization for 386 - given in $(CPUOPT)
# -g=<id>       set code group name
# -nc=<id>      set code class name
# -nd=<id>      set data segment name
# -nm=<file>    set module name
# -nt=<id>      set name of text segment
#

CFLAGS0=@wc.cfg -I$(INCLUDEPATH) $(CPUOPT)
CFLAGSCOM=-mt
CFLAGSEXE=-ms
