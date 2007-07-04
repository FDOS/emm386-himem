#
# TC.MAK - kernel compiler options for Turbo C 2.01
#

BINPATH=$(BASE)

!include "tcpp.mak"

TARGET=TC$(XCPU)
DEPENDS=$(DEPENDS) tc.mak

RTL_EXTRACT=*LDIV *LXMUL *LURSH *LLSH *LRSH
RTL_INSERT =+LDIV +LXMUL +LURSH +LLSH +LRSH
