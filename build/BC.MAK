#
# BC.MAK - kernel compiler options for Borland C++
#

!include "tcpp.mak"

TARGET=BC$(XCPU)
DEPENDS=$(DEPENDS) bc.mak

CC=$(BINPATH)\bcc -c
CL=$(BINPATH)\bcc

ECHOTO=echo>>
