#
# TCPP.MAK - kernel compiler options for Turbo C++ 1.01
#

TARGET=TP$(XCPU)
DEPENDS=$(DEPENDS) tcpp.mak turboc.cfg

CC=$(BINPATH)\tcc -c
CL=$(BINPATH)\tcc

# used for building the library

CLIB=$(LIBPATH)\cs.lib
RTL_EXTRACT=*H_LDIV *H_LLSH *H_LURSH *F_LXMUL *N_LXMUL
RTL_INSERT =+H_LDIV +H_LLSH +H_LURSH +F_LXMUL +N_LXMUL

#
# Compiler options for Turbo/Borland C
# ------------------------------------
#
#  -zAname       ¦ ¦ Code class
#  -zBname       ¦ ¦ BSS class
#  -zCname       ¦ ¦ Code segment
#  -zDname       ¦ ¦ BSS segment
#  -zEname       ¦ ¦ Far segment
#  -zFname       ¦ ¦ Far class
#  -zGname       ¦ ¦ BSS group
#  -zHname       ¦ ¦ Far group
#  -zPname       ¦ ¦ Code group
#  -zRname       ¦ ¦ Data segment
#  -zSname       ¦ ¦ Data group
#  -zTname       ¦ ¦ Data class
#  -zX           ¦«¦ Use default name for "X"

#
# Common options specified in turboc.cfg instead CFLAGS0
#

CFLAGS0=-I$(INCLUDEPATH) $(CPUOPT)
CFLAGSCOM=-L$(LIBPATH) -mt -lt
CFLAGSEXE=-L$(LIBPATH)
