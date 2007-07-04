;*****************************************************************************
;**                  c't  --  magazin fuer computer technik                 **
;*****************************************************************************
;**                     Release   08/90   from page  214                    **
;**                                                                         **
;** original author and copyright                                           **
;**   (c) 1990       c't/Harald Albrecht                                    **
;**   (c) 2001-2004  tom ehlert                                             **
;**                                                                         **
;**	Licensed under the Artistic License version                             **
;**	                                                                        **
;**	please see LICENSE.TXT for details                                      **
;**                                                                         **
;*****************************************************************************
;**                                                                         **
;** original author and copyright                                           **
;**   (c) 1990 c't/Harald Albrecht                                          **
;**                                                                         **
;** created the v86 monitor part and EMS functions                                                                        **
;**                                                                         **
;**                                                                         **
;**        1990 Thomas Gloeckler - Rechenzentrum FH Ulm                     **
;** significant enhancements (DMA+fixes)                                    **
;**                                                                         **
;**                                                                         **
;** put into current shape as a potential EMM386 (EMM386-VCPI-DPMI-...)     **
;**   (c)  2001  junk@drivesnapshot.de                                      **
;**                                                                         **
;** implemented VCPI                                                        **
;**        2004 michael devore                                              **
;**                                                                         **
;**                                                                         **
;**  for better understanding of the code history                           **                                     **
;**                                                                         **
;**  lowercase                                                              **
;**  	mov ax,bx                                                           **
;**  and english comments are my (tom ehlert) contribution                  **
;**                                                                         **
;**  UPPER case                                                             **
;**     MOV AX,BX                                                           **
;**  and german comments are generally the original (c't / Harald Albrecht) **
;**  where however                                                          **
;**  I may have cut/copy/pasted ... things around                           **
;**                                                                         **
;**                                                                         **
;**  11/23/2001  tom ehlert                                                 **
;**                                                                         **
;**																			**
;**  30/10/2003	  Imre Leber												**
;**																			**
;**				   All German strings finally translated to English			**
;**				   Merged code from Eric Auer								**
;**																			**
;**  12/2003      Michael Devore                                            **
;**                                                                         **
;** - Modified for >64M and VCPI support
;** - Updated for EMS 4.0, some extended API returns unsupported, this will
;**   change as requested or needed by other software requirements
;** - Documented EMS 4.0 only supports up to 32M (0x800 pages), but some EMS
;**   drivers allow more than 32M.  The EMS 4.0 spec could be extended to 1G-32K
;**   (map value of 0xFFFF means unmap) without breaking the spec API,
;**   but unfortunately breaking unsophisticated programs which get confused.
;**   I have arbitrarily decided to allow up to 32M of EMS allocations,
;**   leaving the high bit of page allocation alone since specific drivers
;**   may make use of high bit values other than 0xFFFF for their own purposes,
;**   or may only check high bit for unmap -- as FreeDOS EMM386 does.
;**	- Minor English corrections where useful for comprehension              **
;**                                                                         **
;**   Michael Devore's changes are not copyrighted and are released
;**   to the public domain.
;**   This does not affect copyright on the rest of the code.
;**                                                                         **
;**																			**
;*****************************************************************************
                TITLE   V86 - Virtual 8086-Monitor for 80386 PCs
		NAME    V86
;
;                          V 8 6        Version 1.34


;
; (c) 1990 c't/Harald Albrecht
; (c) 2001 tom.ehlert@ginko.de
;
;     T A S M 1.0 / 1.01
;     TC 2.01
;
; Virtual 8086-Monitor, that puts the pc after booting into virtual
; 8086-working mode.
; **** #### Changes of V1.4 against (magazine version) V1.3 are
; **** #### marked by this, there are also:
; **** #### additional questions/questioning with the installation on EMMXXXX0,
; **** #### because some BIOS's take ownership of INT67
; **** #### Failure when asking after the mark 'ct' means (eliminated)
; **** #### that Extended Memory doesn't use now the full capacity,
; **** #### using EXT you can provide the rest of the extended memory
; **** #### STack as Classname for Stack-Segmenr for TLINK 3.0
;
; As a result of using the appropriate extensions in the places marked with (* - 1 - *)
; and so on arises:
;
;**** ####  E M M    Version 1.4
;           LIM-EMS-Driver for 386 PCs (EMS Version 3.2)
;
; (c) 1989 Harald Albrecht
;
;(#-0-#)
; By utilising the extensions (#-1-#) and on, DMA support is installed.
;
;       DMA-support &  The savage thirteen     Version 1.0
;
; (c) 1990 Harald Albrecht
;(#-0-#)
;
		.486P
        .SEQ  		                       ; Absolutely keep row-order/sequence of the Segment-
                                             ; Definitions!!!

		public _MAXPAGES
		public _PAGESAVAIL
		public _FRAME
		public _NoVCPI
		public _FlagNOEMS
		public _NoPageFrame
		public _SB
		public _NoAltBoot
		public _NoDisableA20
		public _startup_verbose

		LOCALS

LF              EQU 0AH
CR              EQU 0DH

V86_TOS         EQU 200H             ; Size of monitor stack

; **** ##### changed to ggf keep space free in Extended Memory
EXT             EQU 14*1024                  ; here you can enter the
                                             ; optional size in KB

MAX_EMS_PAGES_ALLOWED	EQU	800h	; 32M max mem (16K each page), always even
; keep this value low for buggy VCPI clients that fail with large free amounts
MAXMEM16K_DEFAULT	EQU	1E00h	; 120M in 16K blocks

UNIMPLEMENTED_EMS_DEBUG	EQU	0
VCPI_DEBUG	EQU	0


PORT_A          EQU 60H              ; Data-port of the 8042
STATUS_PORT     EQU 64H              ; Statusport of the 8042

@KB_FLAG        EQU 417H             ; Status SHIFT/CTRL/ALT etcetera.
@KB_FLAG_3      EQU 496H             ; u.a. 0E0h/0E1h
@RESET_FLAG     EQU 472H             ; Flag for Warmboot (=1234h)

; GDT - Selectors in the Global Descriptor Table GDT
NULL_SEL        EQU 00H              ; Null-Descriptor
V86_LDT_SEL     EQU 08H              ; Local Descriptor Table Selector
V86_TSS_SEL     EQU 10H              ; Task State Segment of the Monitor
TMP_TSS_SEL     EQU 18H              ; Shorttime needed TSS (0th TSS, will
		; be filled by waste data when activating the 1st real TSS)
REAL_SEL        EQU 20H              ; CS for return to real mode
REAL_DATA_SEL   EQU 28H              ; DS for return to real mode
UNIVERSE_SEL    EQU 30H              ; 4-GByte-Data-segment
; GDT for stack segment makes debugging a lot easier
V86_STACK_SEL   EQU 38H              ; dto. Stack-Segment (STACK) ; = same?

; LDT - Selectors in the Local Descriptor Table
V86_CODE_SEL    EQU 0CH              ; Code-Segment virtual 86-Monitor
V86_DATA_SEL    EQU 14H              ; dto. Data-Segment (DATA) ; dto=ditto
;V86_STACK_SEL   EQU 1CH              ; dto. Stack-Segment (STACK) ; = same?
V86_ALIAS_SEL    EQU 1CH             ; Code-Segment virtual 86-Monitor alias

;(#-1-#)
DMA_BUFF_SIZE   EQU     64              ; DMA-Buffer size in kBytes

BasePrgrd       EQU      0              ; 0 - programmed Address
LenPrgrd        EQU      1              ; 0 - programmed Block length
PagePrgrd       EQU      2              ; 0 - programmed Page register
ModePrgrd       EQU      3              ; 0 - programmed Mode-register

INT13Activ      EQU      0              ; INT 13h is active
INT40Activ      EQU      1              ; INT 40h is active
NeedBuffer      EQU      2              ; DMA-Transfer via Buffer
HiLoFlag1       EQU     14              ; adressing the Hi-Byte , DMA #1
HiLoFlag2       EQU     15              ; Same for DMA controller #2

REPCMD  MACRO CMD,OP                     ;; Call Command with different
                IRP     OPERAND,<&OP>    ;; operands back
		CMD     OPERAND
		ENDM
		ENDM
;(#-1-#)

; Macro for bypassing the 386-Bug with 32-Bit-Stringoperations
; only needed for Mask-version B3
; AddressSize - Prefix + NOP attached

BIG_NOP MACRO
		DB 67h    ;  32-Bit-Prefix
		NOP
		ENDM
;
;
; Macro for describing the Descriptors
;
SELECTOR MACRO BEGIN,LIMIT,ACCESS,GRANULARITY
                DW      LIMIT                   ;; Size of Segment (15..0)
                DW      BEGIN                   ;; Start of Segment (15..0)
                DB      0                       ;; Beginning (23..16)
		DB      ACCESS
                DB      GRANULARITY             ;; u.a. Size (19..16)
                DB      0                       ;; Beginning (31..24)
		ENDM

; VCPI switch from V86 to protected mode data structure
SWSTR	STRUC
	SW_CR3		DD	?	; client's CR3 value
	SW_GDTOFFS	DD	?	; offset of client's GDTR value
	SW_IDTOFFS	DD	?	; offset of client's IDTR value
	SW_LDTR		DW	?	; client's LDTR value
	SW_TR		DW	?	; client's TR value
	SW_EIP		DD	?	; entry point in client
	SW_CS		DW	?
SWSTR	ENDS


DRIVERCODE SEGMENT PARA USE16
DRIVERCODE ENDS


; The stack remains in any case resident, because it's needed during the return
; to REAL-Mode for the termination of the errorsolving
; process(es).

; no return to realmode ever wanted, no res_stack
RES_STACK  SEGMENT  PARA USE16
        DB      V86_TOS DUP (?)  ; space for Stack
;TOS     LABEL   WORD
RES_STACK  ENDS


RESCODE SEGMENT PARA USE16
RESCODE ENDS

; some versions TLINK place this out of order if simply named DATA
MONDATA    SEGMENT PARA USE16
MONDATA    ENDS

CODE    SEGMENT PARA USE16
CODE 	ENDS

V86     SEGMENT PARA USE16
V86 	ENDS

_TEXT	segment	PARA public 'CODE' use16
_TEXT	ends

_DATA	segment word public 'DATA' use16
_DATA	ends

_BSS	segment word public 'BSS'  use16
_BSS	ends

_STACK	segment STACK  'STACK' use16
_STACK	ends

DGROUP	group	_DATA,_BSS,_STACK


TMP_STACK SEGMENT STACK PARA USE16
TMP_STACK ENDS


DRIVERCODE SEGMENT PARA USE16
		ASSUME  CS:DRIVERCODE,DS:NOTHING,ES:NOTHING,FS:NOTHING,GS:NOTHING

public interrupt,strategy,device_header,_EMM_Driver_Name

;******************************************************************************
; device driver header

device_header:
		dd  -1			    ; last driver in list
		dw  0c000h		    ; driver flags :
					    ; 8000 - character device
					    ; 4000 - supports IOCTL - like EMM386
		dw  offset strategy	    ; pointer to strategy routine
		dw  offset interrupt	    ; pointer to interrupt handler
_EMM_Driver_Name:
		db  'EMMXXXX0'		    ; device driver name



;******************************************************************************
; strategy routine. is called by DOS to initialize the driver once.
; only thing to be done here is to store the address of the device driver
; request block.
; In:   ES:BX - address of request header
; Out:  nothing

request_ptr dd  0			    ; pointer to request header

strategy:
	mov word ptr cs:[request_ptr+2],es  ; store segment addr
	mov word ptr cs:[request_ptr],bx    ; store offset addr
	retf				    ; far return here!

interrupt:
	;int 3			; trigger driver entry for debugging purpose

					; this driver assumes, it is called once
					; with cmd = INIT
					;
					; this one request is fullfilled, then
					; every other denied
	push es
	push edi

	les di,cs:[request_ptr]          ; load address of request header

	mov          es:[di+0eh+2],cs    ; set end address
	mov word ptr es:[di+0eh  ],0	 ;
	mov word ptr es:[di+3	 ],0800h ; STATUS_OK


	call far ptr go_driver_entry	 ; this will be patched
					 ; away after init
					 ; so ALL following request are simply OK'ed

	pop edi
	pop es
	retf

;*********************************************************
; an UMB handler
; good enough for FreeDOS=UMB, for nothing else :-)
;*********************************************************

public _UMBhandler, _UMBoldhandler

_UMBhandler:

	jmp short TheUMBHandler			    ; standard XMS link chain
	nop					    ; with 3 required NOPs
	nop
	nop
TheUMBHandler:
	cmp ah,4				    ; global disable A20
	je  global_disable_a20
	cmp ah,6				    ; local disable A20
	je  local_disable_a20
	cmp	ah,3
	je	global_enable_a20
	cmp	ah,5
	je	local_enable_a20

	cmp ah,010h				    ; UMBallocate
	je  UMBallocate				    ;
	cmp	ah,11h				; UMB free
	je	UMBfree
						    ; else let the others return
						    ; 'not implemented'


not_for_us:
	db 0eah					    ; jmp far  UMBoldhandler
_UMBoldhandler dd 0



; dull and stupid - we neither reallocate nor free

;UMBfree:
;UMBreallocate:
;	mov bl,080h			; not implemented
;	mov ax,0			; failure
;	retf




;
;	REALLY dull and stupid - we manage exactly four UMB blocks
;   no UMBfree
;   no UMBrealloc
;   no largest size UMB size in alloc
;
;	but it's
;   feel free to do it better
;   this is filled in by C code
;
;
;extern struct {
;	ushort segment;
;	ushort size;
;	ulong  linearaddress_bottom;
;	ulong  linearaddress_top;
;	ulong  physical_offset;
;	} far UMBsegments[8];	/* UMB block 'array' :-) */


; allow up to 8 allocations of UMB's
public _UMBsegments
; VDS info removed
;_UMBsegments	dd 0,0,0,0	; segment/size, linearaddress bottom, linear address 
                            ; top, phys. offset
;		dd 0,0,0,0
;		dd 0,0,0,0
;		dd 0,0,0,0
;		dd 0,0,0,0
;		dd 0,0,0,0
;		dd 0,0,0,0
;		dd 0,0,0,0
_UMBsegments		dd	0,0,0,0,0,0,0,0	; segment/size entry each
VDS_retcode	DB	0
A20_localcount	DW	0

UMBallocate:
	push	cx
	push	si

					; find first available memory block
	lea	bx, cs:[_UMBsegments]
	mov	cx,8			; ch flags any UMB found (in case too small), cl is entry count
	xor	si,si		; holds largest too-small block size

@@UMBloop:
	cmp	WORD PTR cs:[bx],0	; see if valid UMB
	je	@@UMBnext			; no
	test	BYTE PTR cs:[bx+3],80h	; see if UMB already allocated (high bit size set)
	jne	@@UMBnext			;  yes
	cmp	dx,cs:[bx+2]		; dx = requested block size (high bit of UMB size known reset)
	jbe	@@UMBfound			; enough memory available in UMB
	mov	ch,1				; flag UMB was found, although too small
	cmp	si,cs:[bx+2]
	ja	@@UMBnext
	mov	si,cs:[bx+2]		; update largest too-small block size

@@UMBnext:
;	add	bx,16
	add	bx,4
	dec	cl
	jne	@@UMBloop
	or	ch,ch
	jne	umb_too_small
	jmp	no_umbs_available

@@UMBfound:

; see if actual UMB size exceeds request size by >=2K
	mov	ax,80h				; 128 paras == 2K
	add	ax,dx
	cmp	ax,cs:[bx+2]
	ja	good_umb			; can't split it, just use it

;  2K or over would be unused, see if we can split the block
	mov	cl,8
	lea	si,cs:[_UMBsegments]

@@splitloop:
	cmp	WORD PTR cs:[si],0
	jne	@@splitnext

; split the block
	mov	ax,dx
	add	ax,7fh
	and	ax,0ff80h			; round up allocation to next 2K in paras
	mov	cx,cs:[bx]
	add	cx,ax
	mov	cs:[si],cx			; new block has segment offset of old block+allocation
	mov	cx,cs:[bx+2]		; get original UMB block size, in paras
	sub	cx,ax				; subtract allocation
	mov	cs:[si+2],cx		; update new block with old block size minus allocation
	mov	cs:[bx+2],ax		; update original UMB block size to allocation

	jmp	good_umb

@@splitnext:
;	add	si,16
	add	si,4
	dec	cl
	jne	@@splitloop

good_umb:
	mov	dx,cs:[bx+2]		; actual block size to dx
	or	BYTE PTR cs:[bx+3],80h	; flag UMB allocated
	mov	bx,cs:[bx]			; get UMB address in bx
	mov	ax,1
	pop	si
	pop	cx
	retf

umb_too_small:
	mov	bl,0B0h	; only smaller UMB available
	mov	dx,si

umb_badalloc:
	xor	ax,ax			; flag failure
	pop	si
	pop	cx
	retf

no_umbs_available:
	mov	bl,0B1h	; no UMB's are available
	xor	dx,dx
	jmp	umb_badalloc

UMBfree:
	push	bx
	push	cx
	mov	cl,8
	lea	bx,cs:[_UMBsegments]

@@freeloop:
	cmp	cs:[bx],dx		; see if matches existing UMB allocation
	jne	@@freenext
	and	BYTE PTR cs:[bx+3],7fh	; flag UMB not allocated
	mov	ax,1			; flag success
	pop	cx
	pop	bx
	retf

@@freenext:
;	add	bx,16
	add	bx,4
	dec	cl
	jne	@@freeloop

	xor	ax,ax			; flag failure
	mov	bl,0b2h			; invalid UMB segment number error code
	pop	cx
	add	sp,2			; consume original bx on stack
	retf

; disable and enable are now supported
; emulate this by remapping memory at HMA to point to appropriate memory location
;
;; we do not want to have A20 disabled - we need it !
;; so we don't do anything - and even say SUCCESS - like WinNT DOSBOX
;

local_enable_a20:
	mov	ax,[cs:A20_localcount]
	inc	ax
	jz	global_enable_a20	; overflow -> skip counter increment
	mov	[cs:A20_localcount],ax
global_enable_a20:
	mov	al,1	; enable A20 flag to EMM
	jmp	disen_share

global_disable_a20:
	xor	ax,ax	; disable A20 flag to EMM (and set ZF)
	jmp	dis_share
local_disable_a20:
	mov	ax,[cs:A20_localcount]
	sub	ax,1
	jc	global_disable_a20	; underflow -> skip counter decrement
dis_share:
	mov	[cs:A20_localcount],ax
	jnz	global_enable_a20
;	mov	al,0	; disable A20 flag to EMM

disen_share:
	push	cx
	push	dx
	mov	cx,4652h	; "FR"
	mov	dx,4545h	; "EE"
	mov	ah,3fh		; special EMM function (QEMM does it, so can we)
	int	67h

	pop	dx
	pop	cx
	mov ax,1
	mov bl,0
	retf


if 1
;;******************************************************
;; absolute braindamaged VDS implementation - october 2002
;;******************************************************
; slightly less braindamaged VDS implementation - november 2004-jul 2005
public _int4b_handler
public _int4b_oldhandler

_int4b_handler:
	cmp ah,081h
	je  vds_handler

_int4b_old:
	db 0eah
_int4b_oldhandler:
	dd 0
	
vds_reserved:
	popf
	jmp	_int4b_old

vds_handler:
	pushf				; save entry flags (interrupt/carry of interest)
	cmp	al,1		; check if reserved/unused VDS function 0 or 1
	jbe	vds_reserved	; could be stupid SCSI BIOS

	mov	cs:VDS_retcode,0
	cmp	al,2
	je	vds_version
	cmp al,3
	je	vds_lock
	cmp	al,5
	je	vds_scatterlock
	cmp al,7		; request DMA buffer
	je	vds_reqbuff
	cmp al,0bh		; disable DMA translation, unsupported, don't give feedback
	je	vds_unsupnoshow

; functions 4, 6, and 0ch are no-ops
	cmp	al,6
	je	vds_nada
	cmp	al,4
	je	vds_nada
	cmp	al,0ch		; enable DMA translation (never disabled)
	je	vds_nada
	jb	vds_unsup	; valid VDS functions that are unsupported, give feedback
	jmp	vds_reserved

vds_nada:
	popf				; restore entry flags
	clc					; flag no-op success

; do iret, but only update CY flag status
;	retf	2			; iret, eat flags on stack
iret_with_new_CY:
	push	bp
	push	ax
	pushf
	pop	ax
	and	al,1		; CY flag
	mov	bp,sp
	and	BYTE PTR [bp+2+2+4],0feh	; bp is ss: relative, strip old CY
	or	BYTE PTR [bp+2+2+4],al
	pop	ax
	pop	bp
	iret

vds_unsup:
	push	ax
	push	bx
	push	cx
	mov	bx,OFFSET which_vds
	mov	ch,2

vds_outloop:
	mov	cl,2
	push	ax
	and	al,0f0h
	shr	al,4

vds_inloop:
	cmp	al,9
	jbe	vds2
	add	al,7
vds2:
	add	al,30h
	mov	BYTE PTR cs:[bx],al
	inc	bx
	pop	ax
	and	al,0fh
	dec	cl
	push	ax
	jne	vds_inloop
	pop	ax

	inc	bx
	mov	ax,dx
	dec	ch
	jne	vds_outloop

	pop	cx
	pop	bx

	push si
	push bx
	call print
	db 'unimplemented VDS function '
which_vds:
	db	'xx-xx',0dh,0ah,0
	pop  bx
	pop si
	pop	ax

vds_unsupnoshow:
	mov al,0fh			; function not supported
vds_failret:
	popf				; restore entry flags
	stc					; flag failure
;	retf 2				; iret, eat flags on stack
	jmp	iret_with_new_CY

;   DDS                   STRUCT
;     Region_Size         DWORD     ?    ; offset 0
;     Offset              DWORD     ?    ; offset 4
;     Seg_or_Select       WORD      ?    ; offset 8
;     Buffer_ID           WORD      ?    ; offset A
;     Physical_Address    DWORD     ?    ; offset C
;   DDS                   ENDS

; entry ax = linear address 4K frame
; exit edx = physical address
GetPhysAddr	PROC	NEAR
	push	eax
	push	ecx
	mov	cx,ax
	mov	ax,0de06h	; get physical address of 4K page in 1st megabyte
	int	67h
	pop	ecx
	pop	eax
	ret
GetPhysAddr	ENDP

vds_version:
	mov	ax,100h		; major/minor spec version
	mov	bx,1		; product number
	mov	cx,1		; product revision
	xor	di,di		; si:di == maximum dma buffer size requestable
	mov	si,di
	xor	dx,dx		; flags
	popf
	clc
;	retf 2
	jmp	iret_with_new_CY

vds_reqbuff:
	mov	al,4		; no buffer available, ever
	jmp	vds_failret

vds_lock:

;	call print
;	db 'VDS lock',0dh,0ah,0

	push	eax
	push	ebx
	push	ecx
	push	edx

	xor     eax,eax
	mov	ebx,es:[di]	; region size
	cmp	ebx,eax
	je	vds_locksuccess		; zero byte-sized region always works

	mov 	ax, es:[di+8];	segment
	shl	eax,4
	add	eax, es:[di+4]; offset
	mov	ecx,eax		; ecx == start linear address
	shr	eax,12		; convert start to 4K frame
	cmp	eax,256
	jb	vds_inside	; inside UMB remapping range (<1M)

	mov	eax,ecx		; restore physical/linear address to eax

; see if boundary alignment check
vds_checksuccess:
	pop	dx			; bit flags in dx
	push	dx
	test	dl,30h
	je	vds_locksuccess

	mov	ecx,eax
	mov	ebx,eax
	add	ebx,es:[di]
	dec	ebx			; ebx == last value
	shr	ebx,16		; convert start/end to alignment 64K frames
	shr	ecx,16
	test	dl,10h	; see if 64K alignment
	jne	vds_aligncheck	; yes, if 64K works, then 128K will too

; 128K alignment check
	shr	ecx,1
	shr	ebx,1

vds_aligncheck:
	cmp	ebx,ecx
	je	vds_locksuccess	; alignment okay

	inc	ecx			; ecx == starting alignment frame+1
	shl	ecx,16		; convert to next alignment frame address
	test	dl,10h
	jne	vds_check2
	shl	ecx,1

vds_check2:
	sub	ecx,eax		; get bytes to next alignment frame address from start
	mov	cs:VDS_retcode,2	; region crossed alignment boundary error code
	mov	edx,eax		; edx == start physical address
	mov	eax,ecx
	jmp	vds_lenfail	; update maximum byte count before boundary and fail

vds_inside:
	push	ecx		; save start linear address
	add	ebx,ecx		; start+size
	dec	ebx			; ebx == final linear address
	shr	ebx,12		; convert to 4K frame

	call	GetPhysAddr
	mov	ecx,edx		; ecx == initial page physical address frame
	push	edx		; save initial page physical address frame

vds_checkend:
	cmp	eax,ebx		; see if checked 4K frame matches end
	jb	vds_checknext

vds_compstart:
	pop	eax			; eax == initial page physical address frame
	pop	ebx			; ebx == start linear address
	and	ebx,0fffh	; get 4K block offset
	add	eax,ebx		; eax == physical address
	jmp	vds_checksuccess

vds_checknext:
	cmp	ax,255
	jb	vds_nextok	; not at end of first 1M
	cmp	ecx,0ff000h	; end of 1M, see if final 4K block was identity mapped
	jne	vds_noncont	; no, failed contiguous test
	jmp	vds_compstart

vds_nextok:
	inc	ax			; next linear page frame
	call	GetPhysAddr
	add	ecx,1000h
	cmp	edx,ecx		; see if physical is 4K more than last time
	je	vds_checkend

; physical memory is noncontiguous, fail with error 1
;  return maximum contiguous length
vds_noncont:
	mov	cs:VDS_retcode,1
	mov	ebx,eax		; ebx == current failed page frame

	pop	edx			; edx == initial page physical address frame
	mov	eax,edx
	pop	eax			; eax == start linear address
	mov	ecx,eax
	and	ecx,0fffh	; get 4K block offset
	add	edx,ecx		; edx == physical address

	shr	eax,12		; convert to init linear 4K frame
	sub	ebx,eax		; get count of successful 4K blocks checked+1
	dec	ebx			; adjust to actual block count
	mov	cx,es:[di+8]	; segment
	shl	cx,4
	add	cx,es:[di+4]	; offset, we don't care about high word
	and	cx,0fffh
	mov	eax,1000h
	sub	ax,cx		; [e]ax == count of partial first block bytes
	shl	ebx,12		; convert successful 4K blocks to bytes
	add	eax,ebx		; eax == successful byte count

vds_lenfail:
	mov	es:[di],eax	; update maximum contiguous length
	mov	es:[di+0ch],edx ; update physical address
	mov	WORD PTR es:[di+0ah],0	; zero buffer id

vds_fail:
;	popf
	stc				; flag error
	jmp	vds_done

COMMENT $
	push cx
	push bx
	mov  cx,4
	lea bx, _UMBsegments

vds_lock_loop:
	cmp eax, cs:[bx+4]
	jb	vds_lock_done

	cmp eax, cs:[bx+8]
	jb	vds_lock_found

	loop vds_lock_loop

vds_lock_found:
	add 	eax, cs:[bx+12]
vds_lock_done:
	mov	es:[di+0ch],eax ; physical address


	pop	bx
	pop	cx
END COMMENT $

vds_locksuccess:
	mov	es:[di+0ch],eax ; physical address
	mov	WORD PTR es:[di+0ah],0	; zero buffer id

vds_flagsuccess:
;	popf
	clc

vds_done:
	pop	edx			; cannot change carry flag status through ret
	pop	ecx
	movzx	eax,sp
	mov	bx,ss:[eax+4]	; original ax value
	mov	ss:[eax+8],bx	; store over original flag
	pop	ebx
	pop	eax
	pop	ax			; original flag value changes to original ax, clean stack
	mov	al,cs:VDS_retcode
;	retf 2
	jmp	iret_with_new_CY

vds_scatterlock:
	test	dl,40h	; see if page table entries flagged
	jne	vds_unsup	; yes, unsupported

	push	eax
	push	ebx
	push	ecx
	push	edx

	mov	ebx,es:[di]	; ebx == region size
	xor	ecx,ecx		; cx holds entries used
	mov	eax,ecx		; zero eax for later calcs
	mov	es:[di+0eh],cx	; EDDS number entries used
	cmp	ebx,ecx
	je	vds_flagsuccess	; zero sized region always successful

	mov	ax,es:[di+8]	; segment
	shl	eax,4
	add	eax,es:[di+4]	; offset
	mov	edx,eax			; edx == start linear address
	shr	eax,12			; convert start to 4K frame
	cmp	eax,256
	jb	vds_checklock	; inside of UMB remapping range (<1M)

; outside of UMB remapping range, assuming linear == physical
	mov	WORD PTR es:[di+0eh],1	; one region used/needed
	cmp	cx,es:[di+0ch]	; see if nonzero available entries
	jne	vds_onereg
	mov	cs:VDS_retcode,9
	jmp	vds_fail

vds_onereg:
	mov	es:[di+10h],edx	; region 0 physical address
	mov	es:[di+14h],ebx	; region 0 size
	jmp	vds_flagsuccess	; single region outside of UMB, success

vds_checklock:
	push	esi
	push	edi
	push	ebp
	movzx	edi,di

	mov	esi,edx		; esi == start linear address
	add	ebx,edx		; start+size
	dec	ebx			; ebx == final linear address
	shr	ebx,12		; convert to 4K frame
	call	GetPhysAddr
	mov	ebp,edx		; ebp == current physical address of 4K page

	cmp	cx,es:[di+0ch]	; see if any available entries (nonzero)
	je	vds_entryloop	; no
	and	esi,0fffh
	add	ebp,esi		; add back in offset within 4K page
	mov	edx,1000h
	sub	dx,si		; compute initial 4K frame region size
	mov	es:[edi+ecx*8+14h],edx
	mov	es:[edi+ecx*8+10h],ebp
	jmp	vds_bumpused

vds_entryloop:
	cmp	cx,es:[di+0ch]	; entries available count
	jb	vds_entryok		; cx count is relative zero
	mov	cs:VDS_retcode,9
	jmp	vds_bumpused

vds_entryok:
	mov	es:[edi+ecx*8+10h],ebp
	mov	DWORD PTR es:[edi+ecx*8+14h],1000h	; init region size

vds_bumpused:
	inc	WORD PTR es:[di+0eh]	; bump count of used/needed entries

vds_nextframe:
	inc	ax			; next linear page frame
	cmp	ax,bx
	ja	vds_scatdone	; no more regions to map
	cmp	ax,256
	jb	vds_next2	; not at end of first 1M
	cmp	ax,257
	jae	vds_scatdone	; finishing off final region entry
	cmp	ebp,0ff000h	; end of 1M, see if final 4K block was identity mapped
	je	vds_scatdone	; yes

; start new region for final
	inc	cx
	mov	ebp,100000h
	jmp	vds_entryloop

vds_next2:
	add	ebp,1000h
	call	GetPhysAddr
	cmp	edx,ebp
	je	vds_samereg

; have to move to next region/entry
	inc	cx
	mov	ebp,edx		; update current physical address
	jmp	vds_entryloop

vds_samereg:
	cmp	cs:VDS_retcode,0
	jne	vds_nextframe	; no longer updating regions
	add	DWORD PTR es:[edi+ecx*8+14h],1000h
	jmp	vds_nextframe

; calculate final region byte size or maximum allowed
vds_scatdone:
	xor	ebx,ebx
	mov	edx,ebx
	mov	cx,es:[di+0eh]	; number regions used
	jcxz	vds_scatdone2
	cmp	cs:VDS_retcode,0
	je	vds_finalloop
	mov	cx,es:[di+0ch]	; only count up to minimum of available/used

vds_finalloop:
	mov	esi,edx			; keep previous, last known valid value
	add	edx,es:[edi+ebx*8+14h]
	inc	ebx
	loop	vds_finalloop
	cmp	cs:VDS_retcode,0
	jne	vds_scatdone2	; not all regions represented, return max count

; all regions represented, update final region byte count
	mov	edx,es:[di]
	sub	edx,esi
	dec	ebx
	shl	ebx,3		; ebx*8
	add	ebx,14h
	add	edi,ebx		; edi -> final region byte count

vds_scatdone2:
	mov	es:[edi],edx
	pop	ebp
	pop	edi
	pop	esi
	cmp	cs:VDS_retcode,0
	je	vds_flagsuccess
	jmp	vds_fail

endif
;;******************************************************
;; end of absolute braindamaged VDS implementation - october 2002
;;******************************************************
; end of slightly less braindamaged VDS implementation - november 2004







;*******************************************
; prints text after call
; we rely on DOS to satisfy INT 29
;*******************************************

__print proc near

print_1char:
	int 29h						  ; int29:  DOS 2+ - FAST CONSOLE OUTPUT
							  ; * al = char to display
							  ; * destroys bx

print2:	pop   bx
	pop   si                       ; this is the first character
	mov   al,cs:[si]               ; get token
	inc   si
	push  si			; stack up potential return address
	push  bx

	cmp   al, 0                    ; end of string?
	jne   print_1char              ; until done
	ret


print:
	call print2
	ret


;*******************************************
; printing routines
; we rely on DOS to satisfy INT 29
;
; print
;   db 'hello world'
;
; printdh,printdx - what the name implies
;
; printhex, printhexd
;   'ds=',0    'eax=',0
;
;*******************************************

printdx:
	call printdh
	ror  dx,8

printdh:
	call printnibble
printnibble:

	ror dh,4


	mov al,dh

	and al,0fh
	add al,'0'
	cmp al,'9'
	jbe nohex
	add al,'A'-'0' - 10
nohex:

	int 29h
	ret


printhex:
	call print2

	mov al,'='
	int 29h

printhex2:
	call printdx

	mov al,' '
	int 29h

	ret                            ; and jump to it


printhexd:
	call print2

	mov al,'='
	int 29h

        ror edx,16

	call printdx

        ror edx,16

        jmp printhex2


__print endp


;*********************************************************************
;
; illegal instruction executed
; print some diagnostic
; end program
;
; if int21/4c returns (during device init or more regularily
; for command.com), there is nothing left do to. show 'I am alive'
; and wait for '3FingerSalut'
;*********************************************************************

	public	_IllegalOpcodeHandler
_IllegalOpcodeHandler proc FAR

	; flags 36
	; CS    34
	; IP    32

	push es ;30
	push ds ;28
	push ebp ;24
	push edi ;20
	push esi ;16
	push edx ;12
	push ecx ;8
	push ebx ;4
	push eax ;0

	mov bp,sp
	push cs
	pop  ds


	call print
	db CR,LF,LF,'Illegal Instruction occurred',CR,LF,0


	mov dx,[bp+34]
	call printhex
	db 'CS',0

	mov dx,[bp+32]
	call printhex
	db 'IP',0

	mov dx,ss
	call printhex
	db 'SS',0

	mov dx,bp
	add dx, 38
	call printhex
	db 'SP',0

	mov dx,[bp+26]
	call printhex
	db 'DS',0

	mov dx,[bp+28]
	call printhex
	db 'ES',0


	mov edx,[bp]
	call printhexd
	db CR,LF,'EAX',0

	mov edx,[bp+4]
	call printhexd
	db 'EBX',0

	mov edx,[bp+8]
	call printhexd
	db 'ECX',0

	mov dx,[bp+12]
	call printhexd
	db 'EDX',0

	mov dx,[bp+16]
	call printhexd
	db CR,LF,'ESI',0

	mov dx,[bp+20]
	call printhexd
	db 'EDI',0


	mov dx,[bp+24]
	call printhexd
	db 'EBP',0

	call print
	db CR,LF,'Opcodes @CS:IP ',0

	les di,	[bp+32]		; CS:IP

	mov cx,8
hexdloop:
	push cx
	mov dh, es:[di]
	inc di
	call printdh
	call print
	db ' ',0
	pop cx
	loop hexdloop


	call print
	db CR,LF,'Aborting program',CR,LF,0

	sti
	mov ax,04c7fh
	int 21h

					; and if we tried to abort command.com
					; it will come back here
					; there is nothing left we can do, so
					; we do a dynamic halt, waiting for
					; Ctrl-Alt-Delete
	sub dx,dx
stopit: sti
	call printhex
	db CR,'EMM386 - unable to continue - Please reboot',0
	inc dx
	jmp stopit			; should never be executed

_IllegalOpcodeHandler	endp

DRIVERCODE ENDS

;*********************************************************************

RESCODE SEGMENT
		ASSUME  CS:RESCODE,DS:NOTHING,ES:NOTHING,FS:NOTHING,GS:NOTHING

RETURN_OF_THE_86 PROC FAR
        CLI								 ; No disturbance of the peace !
        MOV     AX,REAL_DATA_SEL         ; Load the On-Chip-Registercache of the
        MOV     DS,AX                    ; Segmentregister with the attributes
        MOV     ES,AX                    ; of REAL-Mode (who from now on
        MOV     FS,AX                    ; on wants to write MOV CS,AX
        MOV     GS,AX                    ; should be ashamed!)
	    MOV     SS,AX

        MOV     EAX,CR0                  ; read from CR0-Register, to
        AND     EAX,7FFFFFFEH            ; disable the PE- Bit (Protected Mode Enable)
        MOV     CR0,EAX                  ; and any possibly Paging.
        XOR     EAX,EAX                  ; Because of safety reasons
        MOV     CR3,EAX                  ; only erase the TLB  !
        JMP     FAR PTR FLUSH            ; Flush the Prefetch-Queue and
FLUSH   LABEL   FAR                      ; set the Attributes for CS newly
        MOV     AX,SEG MONDATA              ; Load DS newly, with that
        MOV     DS,AX                    ; of the Interrupt-Table so it's
        ASSUME  DS:MONDATA                  ; size can be
        LIDT    FWORD PTR [INTS_86]      ; set newly.
;        MOV     AX,SEG RES_STACK        ; The stack has to be recreated
;        MOV     SS,AX                   ; (in resident part
;        MOV     SP,OFFSET TOS           ; below 1 MB)
		JMP     FAR PTR BACK_TO_REALITY
RETURN_OF_THE_86 ENDP
;
; (Continuation of the return from Protected Mode)
; Generate a failure message (Number in DX)
;
BACK_TO_REALITY PROC FAR
	db 0eah,0,0,0ffh,0ffh		; jmp ffff:0
BACK_TO_REALITY endp

RESCODE ENDS


;
; The Data-area of the Monitor- & EMS-Program
;

MONDATA    SEGMENT PARA USE16
		ASSUME  CS:MONDATA
;(*-2-*)
		DB      10 DUP (0)
Kennung DB      'EMMXXXX0'                ; Identification of the EMM-Driver
sig2	DB		'EMMQXXX0'		; ID if NOEMS specified
        DB      'c''t_EMM'                ; Identification for ct_EMM

DUMMY_CALL:			          ; In case software (among them TURBO Pascal
        INT     67H		          ; Programs) gets the idea, by
        IRET				  ; calling CS:IP from the Interrupt-table
                                          ; of the EMM, to in fact call the
                                          ; EMM!


;******************************************************************************
; INT15 handler:
;    everything forwarded to old handler, except int15/87 = move_memory
;
;******************************************************************************
;(*-2-*)
	public NEW15
NEW15 PROC FAR
	CMP     AH,87H               	; is it a blockmove ?
	JZ      SHORT @@do_int1587 	 	;
	JMP     CS:DWORD PTR [OLDINT15] ; else forward to default handler
@@NO_MOVE:

;
; suggested by eric auer - and I like the idea (it's easy to implement)
;	it's easiest (for the moment), to change
;   the int15/87 into a magic, undocumented int67/87 function,
;   which is handled by the 'normal' int67 handler
;
; 	although not 100% efficient, as this will require 2 transitions
; 	DOS int15 --> V86 --> this code --> int67 --> done
;
;
;INT 15 - SYSTEM - COPY EXTENDED MEMORY
;
;        AH = 87h
;        CX = number of words to copy (max 8000h)
;        ES:SI -> global descriptor table (see #0403)
;Return: CF set on error
;        CF clear if successful
;        AH = status (see #0402)

@@do_int1587:
	MOV     AH,087h
	int 067h

	test ah,ah
	jz @@ok
	STC
@@ok:
;	STI
;	RET 2
	jmp updated_iret
NEW15 ENDP
;******************************************************************************

;MSG00   DB      CR,LF,'Left 8086-workingmode and returned to REAL-'
;        DB      'Mode.',CR,LF,'$'
;MSG01   DB      CR,LF,'$'
;MSG08   DB      'INT 08h: Doppelfehler (Gates - Wozniak 5-3 6-3 2-5 6-2)$'
;MSG09   DB      'INT 09h: x87 - Limit-ueberschreitung by Operand$'
;MSG0A   DB      'INT 0Ah: Invalid Task State Segment$'
;MSG0B   DB      'INT 0Bh: Segment not physically available - present$'
;MSG0C   DB      'INT 0Ch: Stack-Failure$'
;MSG0D   DB      'INT 0Dh: General Sicherheitsverletzung$'
;MSG0E   DB      'INT 0Eh: Page not physically available - present$'
;MSG0F   DB      'INT 0Fh: hoppla, gibt''s eigentlich nicht$'
; MSG99   DB      CR,LF,'A20 can't be closed - blocked!$'

;(#-3-#)
	public BaseAdr,BlockLen,PageReg,ChanFlags,Flags,TargetAdr,BuffStart,BuffLen
ALIGN 4
BaseAdr         DW      8 DUP (?)       ; Starting Block of the DMA-Channel
BlockLen        DW      8 DUP (?)       ; dto. Length of the Block
PageReg         DB      8 DUP (?)       ; Pageregister
ChanFlags       DW      8 DUP (?)       ; div. flags for Channels
Flags           DW      0               ; global flags

TargetAdr       DD      ?               ; Original adress for DMA
BuffStart       DD      ?               ; Beginning of the DMA-Buffer
BuffLen         DD      ?               ; Utilized part of the Buffer

PageLookUp      DB      0,2,3,1,0,0,0,0,0,6,7,5,0,0,0,0
PageXLat        DB      87H,83H,81H,82H,0,8BH,89H,8AH

OLD13           DD      ?               ; Address of old INT 13h
OLD40           DD      ?               ; Address of old INT 40h




;
; Control of INT 13h and also INT 40h because of DMA-support
;
NEW13 PROC FAR
        BTS     CS:[Flags],INT13Activ           ; The beginning.
        BTR     CS:[Flags],NeedBuffer           ; Rather disable always.
		PUSH    ECX
        MOV     ECX,8                           ; The Statusbits partly
@@Loop: MOV     CS:[ChanFlags+ECX*2-2],1100B	; = ModePrgrd+PagePrgrd
        LOOP    @@Loop				            ; initialise
	POP     ECX
        PUSHF					; Now call the old INT
        CALL    DWORD PTR CS:[OLD13]		; If an error occurs,
        PUSHF					            ; ... there is nothing !
	JC      @@Bye
        BTR     CS:[Flags],NeedBuffer		; Is it necessary to copy
        JNC     @@Bye				        ; back the buffer?
HLT13:  HLT                                 ; Hello, 32-Bit World !
@@Bye:  BTR     CS:[Flags],INT13Activ       ; Bye bye (Flags are being saved,
        POPF				                ; the BTR CY-flag erased !)

; only update carry flag status, keep rest of original flags
;        RETF    2		                    ; delete flags on stack
updated_iret:
	push	bp
	push	ax
	pushf
	pop	ax
	and	al,1		; CY flag
	mov	bp,sp
	and	BYTE PTR [bp+2+2+4],0feh	; bp is ss: relative, strip old CY
	or	BYTE PTR [bp+2+2+4],al
	pop	ax
	pop	bp
	iret

NEW13 ENDP

NEW40 PROC FAR
        BTS     CS:[Flags],INT40Activ		; Here we go.
        BTR     CS:[Flags],NeedBuffer		; Rather disable always.
	PUSH    ECX
        MOV     ECX,8				; The statusbits partly
@@Loop: MOV     CS:[ChanFlags+ECX*2-2],1100B	; = ModePrgrd+PagePrgrd
        LOOP    @@Loop				; initialize.
		POP     ECX
        PUSHF					; Now call the old INT
        CALL    DWORD PTR CS:[OLD40]		; In case an error occurs,
        PUSHF					; ... nothing is !
		JC      @@Bye
        BTR     CS:[Flags],NeedBuffer		; Still necessary to copy
        JNC     @@Bye				; back the buffer ?
HLT40:  HLT                                     ; Hello, 32-Bit World !
@@Bye:  BTR     CS:[Flags],INT40Activ           ; Bye bye (Flags are being
        POPF					; saved, the BTR CY-Flag emptied !)

;        RETF    2				; Back with the flags
	jmp	updated_iret

NEW40 ENDP
;(#-3-#)

	ALIGN   4
;MSGTAB  DW      MSG08,MSG09,MSG0A,MSG0B,MSG0C,MSG0D,MSG0E,MSG0F

PSP		DW      ?            ; PSP of the program
OLDINT15        DD      ?            ; Address of the old INT 15h

INTS_86         DW      3FFH	     ; Size and state of the Interrupttable
                DD      0            ; (IDT) in REAL-Mode
IDT_PTR         DW      7FFH	     ; Size and state IDT in Pro
                DW      OFFSET IDT,0 ; (Will be adjusted later again !)
GDT_PTR         DW      GDT_LEN-1	 ; Size and state GDT in Pro
		DW      OFFSET GDT,0

;(*-3-*)
OLDINT67        DD      ?            ; Alter the EMM-Vector
;EXTMEM         DW      ?            ; Extended Memory in kBytes

public	_XMS_CONTROL_HANDLE
_XMS_CONTROL_HANDLE	DW	0				; initial XMS handle allocated for EMM386 control/fixed allocations

public 	_MONITOR_ADDR
_MONITOR_ADDR   DD  100000H +EXT*1024   ; Beginning of the Monitor-Code

public _EMM_MEMORY_END              	; end of memory for EMM
_EMM_MEMORY_END dd 0

public _TOTAL_MEMORY		    ; highest physical address in machine
_TOTAL_MEMORY	dd      0

public PAGEDIR
PAGEDIR         DD      ?	    ; ^ up Page Directory (later CR3)

public _FIRSTPAGE
_FIRSTPAGE       DD      ?	    ; ^ up first available EMS-page

public	_POTENTIAL_EMSVCPI_MEMORY
_POTENTIAL_EMSVCPI_MEMORY	DD	0	; total XMS or user-set

public	_MAXMEM16K
_MAXMEM16K		DD	MAXMEM16K_DEFAULT	; maximum amount of VCPI memory in 16K

STATUSTABLE     DD      ?           ; ^ statustable for EMM Handles
EMSPAGETABLE    DD      ?	    ; ^ up allocation table der
EMSNAMETABLE	DD      ?	    ; storage for EMS handle names
_MAXPAGES       DW      0	    ; EMS-pages Maximal available
_PAGESAVAIL     DW      0	    ; EMS-pages currently available
_FRAME          DW      0D000H	    ; EMS-page Segmentaddress of the
FRAMEANCHOR     DD      ?	    ; EMS-"Frame" ^ up entry in
PHYSPAGETABLE   DW      4 DUP (-1)  ; Page Table up-to-date faded in
				    ; pages

TSS_Address		DD	0	; TSS address after moved to high memory
VCPI_Tracking	DD	0	; address of VCPI tracking control page
PageMapSave		DW	2 DUP (?)	; first two EMS physical page maps store
RegionSource	DD	0	; function 57h region source
RegionDest		DD	0	; function 57h region destination
RegionCount		DD	?	; function 57h region count
CR3_Save		DD	?	; temporary store of CR3 value not EMM386's
CX_Save			DW	0	; temporary store of cx value
ScratchSource	DD	0	; start of scratch source page tables
ScratchDest		DD	0	; start of scratch destination page tables
ScratchDirS1	DD	0	; scratch source page directory entry pointer
ScratchDirS2	DD	0	; scratch source page directory entry pointer #2
ScratchDirD1	DD	0	; scratch destination page directory entry pointer
ScratchDirD2	DD	0	; scratch destination page directory entry pointer #2

; boolean flags
_NoVCPI			DB	0	; flags no VCPI services
_FlagNOEMS		DB	0	; flags no EMS services
_NoPageFrame	DB	0	; flags FRAME=NONE
_SB				DB	0	; flags SB (soundblaster drivers)
_NoAltBoot		DB	1	; flags NoAltBoot (don't check INT 9 for Ctrl-Alt-Del)
Xchg_Flag		DB	0	; EMS 4.0 func. 57h move/exchange memory region flag
VCPI_PM_call	DB	0	; flags call to VCPI via protected mode rather than INT 67h
_NoDisableA20	DB	0	; flag that EMM386 can't disable A20

				    ; Flag to enable/disable special
				    ; handling of INT67/44 (MAP_PAGE)
				    ; during init phase, abused to map UMB everywhere
	public _INIT_DONE
_INIT_DONE		db		0			; reset after initialization

public _MEMCHECK		    ; check and allow access outside of RAM address space 0-4G
_MEMCHECK	db      0

public _NOCHECK		    ; do not check and allow access outside of RAM address space
_NOCHECK	db      0

public _ENDALLOC		   	; allocate EMS/VCPI from end of memory blocks
_ENDALLOC	db      0

_startup_verbose db 0		    ; more (debug) output in Init Phase


    public _FlagKILLAbove64M	
_FlagKILLAbove64M db 0          ; this driver supports > 64M memory, no need to kill

; number of bytes for system info in EMS/VCPI pool allocation block,
;  must be >= sizeof POOL_SYSTEM_INFO_STRUC or bad things will happen quickly
POOLBLOCK_SYSTEM_SPACE	EQU	16

; number of bytes for allocation in a pool allocation block
POOLBLOCK_ALLOCATION_SPACE	EQU	48

POOLBLOCK_TOTAL_SPACE	EQU	POOLBLOCK_SYSTEM_SPACE+POOLBLOCK_ALLOCATION_SPACE

POOL_SYSTEM_INFO_STRUC	struc
	psi_addressK	DD	?	; base address in K (may not be XMS handle base if handle size changed later)
	psi_descptr	DD	?	; pointer to XMS handle descriptor array entry/pseudo-handle value
	psi_startadj	DB	?	; unused K from XMS handle base for 4K alignment (0-3)
	psi_endadj	DB	?	; unused K at XMS handle end as 32K chunks (0-31)
	psi_16kmax	DB	?	; maximum number of 16K allocations (used allocation bytes*2), always even
	psi_16kfree	DB	?	; free number of 16K slots
	psi_4kfree	DW	?	; free number of 4K slots (>psi_16kfree*4 if any partials)
	psi_flags	DB	?	; various flag values
	psi_unused	DB	?
POOL_SYSTEM_INFO_STRUC	ends

POOLBLOCK_FLAG_DONTEXPAND	EQU	1	; don't try to expand or release this pool allocation block

XMS_ARRAY_STRUC	struc
	xas_flag			DB	?
	xas_lockcount	DB	?
	xas_addressK	DD	?
	xas_sizeK		DD	?
XMS_ARRAY_STRUC	ends

XMS_HANDLE_TABLE_STRUC	struc
	xht_sig			DB	?
	xht_sizeof		DB	?
	xht_numhandle	DW	?
	xht_handleptr	DD	?
XMS_HANDLE_TABLE_STRUC	ends
XMS_Handle_Table	XMS_HANDLE_TABLE_STRUC	<0,0,0,0>

	public	_PtrXMShandleTable
_PtrXMShandleTable	DD	0		; pointer to XMS handle table

	public	_IsXMSTableNotFixedEMS
; flags presence of XMS handle table via INT 2f function 4309h and
;  not fixed EMS/VCPI allocation
; default on condition
_IsXMSTableNotFixedEMS	DB	1	

XMSHandleArray	DD	0
EMSPageAllocationPtrs	DD	0	; address of pointers to EMS page allocations within allocation blocks
PoolAllocationTable	DD	0	; address of EMS/VCPI allocation block start
EMSPageAllocationEnd	DD	0
PoolAllocationEnd		DD	0
LastBlockAllocator	DD	0	; pointer to last block allocated from
LastBlockFreed			DD	0	; pointer to last block freed from
XMSBlockSize		DD	0	; current size of XMS block to allocate for sharing, in K
XMSPoolBlockCount	DW	0	; count of XMS handles allocated for pool blocks (not counting initial UMB block)

ZeroPTE	DD	00007h,01007h,02007h,03007h,04007h,05007h,06007h,07007h
		DD	08007h,09007h,0a007h,0b007h,0c007h,0d007h,0e007h,0f007h
HMAPTE	DD	100007h,101007h,102007h,103007h
		DD	104007h,105007h,106007h,107007h
		DD	108007h,109007h,10a007h,10b007h
		DD	10c007h,10d007h,10e007h,10f007h
;(*-3-*)
;
; GDT - Global Descriptor Table. This table holds the descriptions
; of all  public Data-, Code-, LDT- and Task-Segments
;
		ALIGN   4
GDT     LABEL BYTE
  DQ            0                               ; NULL-Entry
  SELECTOR <OFFSET LDT>,LDT_LEN,82H,0           ; LDT-Descriptor
  SELECTOR <OFFSET TSS>,TSS_LEN,89H,0           ; TSS V86
  SELECTOR <OFFSET TMP_TSS>,TMP_TSS_LEN,89H,0   ; Temporary TSS
  SELECTOR 0,0FFFFH,9AH,0                       ; "Normal" Codesegment (64k)
  SELECTOR 0,0FFFFH,92H,0                       ; "Normal" Datasegment with
                                                ; REAL-Attributes
server_GDT_Universe:
  DW 0FFFFH,0                                   ; 4-GByte-Adress-space
  DB 0,92H,0C0H+0FH,0

; put V86 stack segment in GDT to help out debugger
  SELECTOR 0,V86_TOS,92H,0			 ; Stack-Segment V86 (16 Bit)

; NULL entries for debugger purposes
  DQ	0, 0, 0, 0, 0, 0, 0, 0
GDT_LEN EQU     $-GDT
;
; LDT - Local Descriptor Table. This table holds the Code-, Data-
; and Stack-Segment exclusiv(-ely) for the virtual Monitor (thus DPL = 0).
;
LDT     LABEL BYTE
  DQ            0                                ; NULL-Entry
server_GDT_Code:
  SELECTOR 0,V86_LEN,09AH,0			 ; Code-Segment V86  (16 Bit) use32
server_GDT_Data:
  SELECTOR		0,WHOLE_DATA_LEN,92H,0	 ; Data-Segment V86 (16 Bit)
;  SELECTOR 0,V86_TOS,92H,0			 ; Stack-Segment V86 (16 Bit)

; use as a code alias selector
alias_selector:
  SELECTOR 0,V86_LEN,092H,0			 ; Data-Segment V86  (16 Bit)
LDT_LEN EQU     $-LDT
		PURGE   SELECTOR

public DATA_END
DATA_END        EQU     $			 ; End of resident part


;
; Now the Interrupttable (IDT) follow, these contain the "Gates" for
; Exceptions or Interrupts in Protected Mode. Beware: So that the processing can take
; place duly, the condition that DPL = 3 must be valid !

IDT_ENTRY_O MACRO SEQNR				;; Unfortunately this is the only way
        DW      OFFSET INT_&SEQNR		;; the Symbolname INTxx is correctly
		ENDM				;; produced.

IDT_ENTRY MACRO FROM,TO				;; Interrupt Gates producer
		LOCAL   ENTRY
ENTRY   =       FROM
		REPT    TO-FROM+1
		IDT_ENTRY_O %(ENTRY)		;; Offset 15..0 of target
             DW      V86_CODE_SEL		;; Code-Segment-Selector of Target
		DB      0,0EEH                  ;; Interrupt Gate (DPL=3, 32 Bit)
		DW      0			;; Offset 31..16
ENTRY   =       ENTRY+1
		ENDM
		ENDM

IDT     LABEL BYTE
                IDT_ENTRY 0,0FFH               ; Interrupt-Table
IDT_LEN EQU     $-IDT
		PURGE   IDT_ENTRY,IDT_ENTRY_O
;
; TSS - priry Task State Segment; is needed during the initialisation,
; to activate the 86-Monitor by Taskswitching.
; (Strictly not a single byte is needed, only the 386 is already happy
; with the sheer existence of it.)
; * When the first real TSS / task is activated, the 386 stores parts
; * of the current state to this "zeroth TSS" which can then be discarded.
;
TMP_TSS LABEL BYTE
                DW      0,0                  ; Back Link for verschachtelte Tasks
                DD      V86_TOS              ; Level 0 Stack
		DW      V86_STACK_SEL,0
		DD      0,0                  ; Level 1 Stack
		DD      0,0                  ; Level 2 Stack
                DD      0                    ; CR3 (for Paging)
		DW      OFFSET V86_START,0   ; EIP
		DD      0                    ; EFLAGS (IF = 0)
		DD      0,0,0,0              ; EAX/EBX/ECX/EDX
		DD      V86_TOS              ; ESP
		DD      0,0,0                ; EBP/ESI/EDI
		DW      V86_DATA_SEL,0       ; ES
		DW      V86_CODE_SEL,0       ; CS
		DW      V86_STACK_SEL,0	     ; SS
		DW      V86_DATA_SEL,0       ; DS
		DW      V86_DATA_SEL,0       ; FS
		DW      V86_DATA_SEL,0       ; GS
		DW      V86_LDT_SEL,0        ; LDT
		DW      0                    ; Debug-Bit 0
		DW      $+2-TMP_TSS          ; Offset I/O-Erlaubnis-Bitmap rel.
                                             ; TSS No I/O is permitted
TMP_TSS_LEN     EQU     $-TMP_TSS
;
; TSS - Task State Segment of the Monitor-Program (at the same time here ends
; the resident part of the program, the rest is shifted part-by-part in the
; range, to clean up MSDOS' memory space.)
;
DATA_LEN        EQU     OFFSET $
;
; This TSS is surely really needed!
;
TSS: ;     LABEL BYTE
        DW      0,0		     ; Back Link for interlocked Tasks
	DD      V86_TOS              ; Level 0 Stack
	DW      V86_STACK_SEL,0
	DD      0,0                  ; Level 1 Stack
	DD      0,0                  ; Level 2 Stack
	DD      0		     ; CR3 (for Paging)
	DW      OFFSET V86_START,0   ; EIP
	DD      0                    ; EFLAGS (IF = 0)
	DD      0,0,0,0              ; EAX/EBX/ECX/EDX
	DD      V86_TOS              ; ESP
	DD      0,0,0                ; EBP/ESI/EDI
	DW      V86_DATA_SEL,0       ; ES
	DW      V86_CODE_SEL,0       ; CS
	DW      V86_STACK_SEL,0      ; SS
	DW      V86_DATA_SEL,0       ; DS
	DW      V86_DATA_SEL,0       ; FS
	DW      V86_DATA_SEL,0       ; GS
	DW      V86_LDT_SEL,0	     ; LDT (not used!)
	DW      0                    ; Debug-Bit 0
	DW      $+2-TSS              ; Offset I/O-Erlaubnis-Bitmap rel.

;**********************************************************************

;(#-4-#)
Comment $
;(#-4a-#)
DB      10000H/8 DUP (0)        ; TSS all I/O-Addresses allowed, without 
DMA!
;(#-4a-#)
$
;*********************************************************************
;       Changed for I/O-control of the DMA-port
; The line "DB 10000H/8 DUP (0)" directly before this new change/adaptation
; has to be remarked or deleted!

		DB      11111111B,00011000B		; DMA-Controller #1
;DB	0,0
		; * trap ports 0..7, b, c ?
		DB      14 DUP (0)
		DB      10001110B,00001110B		; page register
;DB	0,0
		; * trap ports 81..83, 87, 89..8b ?
		DB      6 DUP (0)
		DB      11111111B,11111111B,01000000B	; DMA-Controller #2
;DB	0,0,0
		; * trap ports c0..cf, d6 ?
		DB      00000001B			; dto.
;DB	0
		; * trap port d8 ?
		DB      (10000H-0E0H)/8 DUP (0)
		; * allow all other ports
;(#-4-#)
                DB      0FFH                 ; wg. Readmechanism of the 80386
TSS_LEN EQU     $-TSS

MSGF			DB      ' Byte remaining resident, EMM386 returning to DOS',CR,LF,'$'
MSGFail			DB      ' something failed - driver aborted',CR,LF,'$'
msg_already_installed	DB	'EMM already installed',CR,LF,'$'

WHOLE_DATA_LEN  EQU     OFFSET $

MONDATA    ENDS
;
; Switch the processor into Protected-Mode and start the virtual working
; mode of the 80386. Start follows in REAL-Mode, and afterwards
; a Task-change is forced, through which the virtual 8086-Modus
; is started.
CODE    SEGMENT PARA USE16
	ASSUME  CS:CODE,DS:MONDATA

STORE MACRO ADDRESS
        MOV     EAX,EDI			    ;; Load the by <ADDRESS> described
        MOV     WORD PTR FS:[ADDRESS+2],AX  ;; Descriptor with the value
        SHR     EAX,16			    ;; from EDI.
	MOV     BYTE PTR FS:[ADDRESS+4],AL
	MOV     BYTE PTR FS:[ADDRESS+7],AH	; 32-bit extension
	ENDM

REPMOVSB MACRO
        MOVZX   ECX,CX				; use 32 Bit-Adresses, so
        REP MOVS BYTE PTR [ESI],BYTE PTR [EDI]	; the entire Address-space
	BIG_NOP;
        ADD     EDI,3				; can be addressed.
        AND     DI,NOT 3			; Addresses in Longword
        ENDM					; round border, clear Bits 1 & 0

_pmessage macro line,col,c1,c2
	push ds
	push ax
	mov ax,UNIVERSE_SEL
	mov ds,ax
	mov byte ptr ds:[0b0000h+line*160+col*2+0],c1
	mov byte ptr ds:[0b0000h+line*160+col*2+1],070h
	mov byte ptr ds:[0b0000h+line*160+col*2+2],c2
	mov byte ptr ds:[0b0000h+line*160+col*2+3],070h
	inc byte ptr ds:[0b0000h+line*160+col*2+4]
	mov byte ptr ds:[0b0000h+line*160+col*2+5],070h
	pop ax
	pop ds
	endm

_pause macro line,col,c1,c2
	push ebx

	mov ebx, (line*80 + col) * 2
	call pauser

	pop ebx
	endm



GO_PROTECTED PROC FAR
	CLI 	    			    ; do not disturb !
	CLD
	LIDT    FWORD PTR [IDT_PTR]	    ; Initialise pointers to IDT and GDT
	LGDT    FWORD PTR [GDT_PTR]
        MOV     BX,TMP_TSS_SEL		    ; Temporarily set temporary Task
        MOV     EAX,CR0			    ; Set the Protected-Mode-Enable-Bit in
        OR      EAX,1			    ; CR0 (?).
        MOV     CR0,EAX			    ; In Protected-Mode !
        JMP     SHORT $+2		    ; Prefetch-Queue flush
        LTR     BX			    ; Only select current TSS.
        MOV     AX,V86_LDT_SEL		    ; Also initialise LDT (only
	LLDT    AX			    ; now !)

	ASSUME  FS:MONDATA
        MOV     AX,UNIVERSE_SEL		    ; Addressing everything
	MOV     DS,AX
	MOV     ES,AX
	MOV     AX,V86_DATA_SEL		    ; GDT/LDT & Co.
	MOV     FS,AX

    MOV     EDI,FS:[_MONITOR_ADDR]      ; Start of Monitor-Code

; rearrange things so that the TSS gets a 4K boundary to satisfy
;  any possibility of crossing page border and causing problems with
;  rumored early CPUs that can fail in such circumstance
	ADD     EDI,4095
	AND     DI,NOT 4095
	mov	fs:[TSS_Address],edi	; save new location of TSS
	mov	esi,SEG MONDATA
	shl	esi,4
	mov	eax,OFFSET TSS
	add	esi,eax
	STORE   <OFFSET GDT+(V86_TSS_SEL AND 0F8H)>
	mov	cx,TSS_LEN
	REPMOVSB				; move the TSS to extended memory

        MOV     SI,SEG V86		    ; Calculate situation of the Monitor-
        MOVZX   ESI,SI			    ; Code in REAL-Mode
	SHL     ESI,4

; para align DMA buffer for better transfer performance
	ADD     EDI,15                  ; Round to the next page border
	AND     DI,NOT 15

;(#-6-#)
; Reserve the space for the DMA-Buffer
;
	MOV     FS:[BuffStart],EDI	    ; Store pointers.
        ADD     EDI,DMA_BUFF_SIZE*1024	    ; insert space for Buffer
;(#-6-#)

	STORE   <OFFSET LDT+(V86_CODE_SEL AND 0F8H)>
					     ; New situation of Code-Segments
        MOV     CX,V86_LEN		     ; Length of the code
	REPMOVSB

        MOV     SI,SEG MONDATA		     ; The Interrupt-Table dissapears
        MOVZX   ESI,SI			     ; also in Extended-range
	SHL     ESI,4
        MOV     EBP,ESI                      ; to keep it (for) later
	MOV     AX,OFFSET IDT
	MOVZX   EAX,AX
	ADD     ESI,EAX

; can no longer use STORE macro since 32-bit extension is different for LxDT
;  and segment descriptors
;        STORE   <IDT_PTR>		     ; Reset pointer to IDT
	mov	DWORD PTR FS:[IDT_PTR+2],EDI

	MOV     CX,7FFH
	REPMOVSB

	DB	66h					; make 32-bit 6-byte load
        LIDT    FWORD PTR FS:[IDT_PTR]	     ; load IDT-Base-register newly

comment $	; moved
        MOV     ESI,EBP                      ; Because the TSS hardly takes
        MOV     AX,OFFSET TSS	             ; litle space (over 8 kByte),it's
        MOVZX   EAX,AX			     ; being moved.
	ADD     ESI,EAX
	STORE   <OFFSET GDT+(V86_TSS_SEL AND 0F8H)>
	MOV     CX,TSS_LEN
	REPMOVSB
$

;(*-5-*)
; Now likewise initialize the variables of the Expanded MEMORY of manager (EMM).
; First comes the virtual store management 80386.

	ADD     EDI,4095                   ; Round to the next page border
;	AND     EDI,NOT 4095
	AND     DI,NOT 4095


;        MOV     AX,FS:[__EXTMEM]           ; Determine for the order
;        ADD     AX,1024                    ; standing kBytes (inc. DOS = 1

	mov eax, FS:[_TOTAL_MEMORY]	    ; memory in byte
	shr eax, 10			    ; in kbyte

	SHR     eAX,2			    ; MByte) recalculate in pages (… 4 kBytes)
        SHR     eAX,10			    ; ... gives the number
        MOV     BX,1024                     ; of needed entries in the Page
	INC     AX                          ;  Directory
        SUB     BX,AX			    ; Set the rest later to 0

	MOV     ESI,111B                    ; R/W=1, U/S=1, P=1
        MOV     FS:[PAGEDIR],EDI	    ; Beginning of the Tabels (later in CR3)

	mov	edx,fs:[TSS_Address]
	mov	[edx+1ch],edi			; save (future) CR3 value to TSS

        MOV     EDX,EDI			    ; 1. Page Table start directly beyond
        ADD     EDX,4096                    ; the Page Directory 
@@FILL_PAGEDIR:
        MOV     [EDI],EDX	            ; physical address of the corresponding
	OR      BYTE PTR [EDI],111B	    ; page (Page Table) &
	ADD     EDI,4                       ;  Statusbits
	MOV     CX,1024			    ; In each case fill Page Table
@@FILL_PAGETABLE:
        MOV     [EDX],ESI		    ; Calculate situation of page in the physical
        ADD     ESI,4096                    ; Address-space and store in Page
        ADD     EDX,4                       ; Table
	LOOP    @@FILL_PAGETABLE
	DEC     AX
	JNZ     SHORT @@FILL_PAGEDIR
        MOVZX   ECX,BX			    ; Set rest of Page Directory to 0
	XOR     EAX,EAX                     ; ausnullen
	REP     STOS DWORD PTR [EDI]
        BIG_NOP				    ; For the old 386 (B3-Mask)

	cmp	FS:[_NOCHECK],0
	jne	@@nomemcheck
	mov	FS:[ScratchSource],edx
	add	edx,8192				; allocate 2 scratch page tables for source
	mov	FS:[ScratchDest],edx
	add	edx,8192				; allocate 2 scratch page tables for destination
@@nomemcheck:

        MOV     EDI,EDX			    ; EDI always points to the first
                                            ; free byte in Extended

; initial setup for XMS handle array work
	cmp	fs:_IsXMSTableNotFixedEMS,0	; no XMS handle table info available
	je	@@noarray
	movzx	eax,WORD PTR fs:[_PtrXMShandleTable]
	movzx	ecx,WORD PTR fs:[_PtrXMShandleTable+2]
	shl	ecx,4			; convert seg to absolute
	add	ecx,eax			; ecx -> XMS table

; transfer XMS table info to fixed memory location, assume two dwords
	mov	eax,[ecx]
	mov	DWORD PTR fs:XMS_Handle_Table,eax
	mov	eax,[ecx+4]
	mov	DWORD PTR fs:XMS_Handle_Table+4,eax

	movzx	eax,WORD PTR [ecx].xht_handleptr
	movzx	ecx,WORD PTR [ecx].xht_handleptr+2
	shl	ecx,4			; convert seg to absolute
	add	eax,ecx			; eax -> XMS handle array
	mov	fs:XMSHandleArray,eax
@@noarray:

	add	edi,15
	and	di,NOT 15

; allow with NOVCPI for UMBs
; 2K status table (8 bytes * 256 handles)
;  each entry is 4 words, first word is either
;  -1 == system handle
;  -2 == free handle
;  -3 == occupied handle
; != the above values, first page number of saved mapping
; 2-4 words are 2nd-4th page number of saved mapping
        MOV     FS:[STATUSTABLE],EDI	    ; Here starts the status table of the
        MOV     AX,-1			    ; handles. Handle 0 is reserved
        STOS    WORD PTR [EDI]		    ; for the System, all others are
        BIG_NOP				    ; prefix-adapted
        MOV     AX,-2			    ; -free
        MOV     ECX,256*4-1		    ; Entries still to fill for 255
	REP     STOS WORD PTR [EDI]	    ; Handles (REP STOSW, addr. size =
	BIG_NOP

	cmp	fs:[_NoVCPI],0	; see if VCPI allowed
	je	@@namesetup			; yes
	mov	ecx,POOLBLOCK_ALLOCATION_SPACE * 2	; allow one block (1.5M) for NOVCPI UMBs
	jmp	@@novcpi

; allocate room for handle names (8*256 = 2K) and zero them
@@namesetup:
	mov	fs:[EMSNAMETABLE],EDI
	mov	eax,'TSYS'		; store SYSTEM as first handle name
	mov	[edi],eax
	mov	eax,'ME'
	mov	[edi+4],eax
	add	edi,8
	mov	ecx,8*255/4
	xor	eax,eax
	rep	stos DWORD PTR [edi]
	BIG_NOP

; allocate and -1 store space for EMS page allocation pointer/descriptors
;  dword per page, points into EMS/VCPI block which controls page allocation
; allocate 4 bytes per 16K XMS total, 128K-4 max (no more than 32K-1 pages controlled)
;  page descriptor high word == 64-byte EMS/VCPI allocation block count from start, RELATIVE 1!
;  page descriptor low word ==  half-byte offset of allocation page (2 * 48)
;   within allocation block, not including system bytes
	mov	ecx,fs:_POTENTIAL_EMSVCPI_MEMORY
	shr	ecx,14		; convert each 16K to pointer count
	add	ecx,4		; allow for available computed versus actual rounding errors
	mov	dx,cx		; save as page count
	add	ecx,3			; round up to next paragraph (dword-based) boundary
	and	cl,NOT 3
	cmp	ecx,MAX_EMS_PAGES_ALLOWED
	jb	@@zeropaps		; even if equal fall through to page count adjustment
	mov	ecx,MAX_EMS_PAGES_ALLOWED
	mov	dx,cx		; update page count to valid

@@zeropaps:
	cmp	ecx,fs:_MAXMEM16K	; throttle EMS if MAX= setting is less than 32M (MAX_EMS_PAGES_ALLOWED)
	jb	@@zero2
	mov	ecx,fs:_MAXMEM16K

@@novcpi:
	mov	dx,cx

@@zero2:
	mov	fs:EMSPageAllocationPtrs,edi
	mov	eax,-1
	rep	stos DWORD PTR [edi]
	BIG_NOP;

	mov	fs:EMSPageAllocationEnd,edi

; allow with NOVCPI for UMBs
; page count-sized table flagging owner handle, if any, of EMS page
@@ownersetup:
	        MOV     FS:[EMSPAGETABLE],EDI	    ; 32) Here the allocation of the

	movzx	ecx,dx			; page count
	and	dl,NOT 1			; mask to even count for byte control
	mov	fs:[_MAXPAGES],dx	; set maximum pages for pool-sharing, changed for fixed

        MOV     AL,FREEPAGE_ID		    ; all currently free
	REP     STOS BYTE PTR [EDI]	    ; REP STOSB (addr. size = 32)
	BIG_NOP;

; allocate and zero ((XMS total / 1.5M) + 128) * 64 bytes for pool
;  allocation table entries
; 1.5M is pool allocation maximum memory control, 128 is max number XMS handles,
;  64 is pool block size
	add	edi,63
	and	di,NOT 63		; 64-byte align tables for proper location by count
	mov	fs:PoolAllocationTable,edi

	cmp	fs:[_NoVCPI],0	; see if VCPI allowed
	je	@@poolsetup			; yes
	mov	ecx,POOLBLOCK_TOTAL_SPACE / 4	; one pool block of dwords
	jmp	@@zeropool

@@poolsetup:
	xor	edx,edx
	mov	eax,fs:_POTENTIAL_EMSVCPI_MEMORY
	mov	ecx,1536*1024
	div	ecx
	add	eax,128
	shl	eax,6-2			; 64 bytes/16 dwords each
	mov	ecx,eax

@@zeropool:
	xor	eax,eax
	rep	stos DWORD PTR [edi]
	BIG_NOP;

	mov	fs:PoolAllocationEnd,edi

; force 4K alignment for EMS/VCPI fixed pages and UMB's
@@4kalign:
	ADD     EDI,4095                   ; Round to the next page border
	AND     DI,NOT 4095

        MOV     FS:[_FIRSTPAGE],EDI	    ; Mark the beginning of first EMS-page

	mov eax, FS:[_EMM_MEMORY_END]
	sub eax, EDI
	jnc	initEMS

; ran out of memory, shouldn't happen, avoid disaster by shutting off vcpi,
;  pool-sharing, and leaving max/available memory at 0
@@killvcpi:
	mov	fs:[_NoVCPI],1	
	mov	fs:_IsXMSTableNotFixedEMS,0
	jmp	compute_anchor		; bypass EMS/VCPI pool stuff

; init EMS globals
; even NOVCPI needs room for UMBs
initEMS:
	shr	eax,14				; compute 16K pages of memory available
	jz	@@killvcpi			; no free memory at startup for EMS/VCPI allocations

; if pool sharing, first pool allocation block points to remainder of initial
;  XMS allocated memory so that UMB code still has linear==physical memory
;  the remainder will never exceed UMB size + slop, so always <300K if sharing
	and	al,NOT 1		; mask off odd-16K since each byte controls 32K
	cmp	fs:_IsXMSTableNotFixedEMS,0	; check if pool sharing
	jne	@@setblocks			; yes, set the first blocks to remainder

	push	eax
	cmp	eax,MAX_EMS_PAGES_ALLOWED
	jb	@@adj1
	mov	eax,MAX_EMS_PAGES_ALLOWED
@@adj1:
	cmp	eax,fs:_MAXMEM16K	; throttle if MAX= setting is < MAX_EMS_PAGES_ALLOWED
	jb	@@adj2
	mov	eax,fs:_MAXMEM16K
@@adj2:

	mov	fs:[_MAXPAGES],ax
	mov	fs:[_PAGESAVAIL],ax
	pop	eax

; setup control blocks (pool allocation blocks which are fixed) for the
;  fixed EMS/VCPI allocations
; start and end adjustments leave as initialized zero value since the memory
;  is originally 4K aligned, via edi alignment above
@@setblocks:
	mov	ecx,eax			; count of available 16K blocks
	mov	esi,fs:PoolAllocationTable
	movzx	edx,fs:[_XMS_CONTROL_HANDLE]

@@fixblkloop:
	mov	[esi].psi_descptr,edx
	mov	eax,ecx
	cmp	eax,POOLBLOCK_ALLOCATION_SPACE * 2	; each allocation byte covers 32K
	jb	@@fix2
	mov	eax,POOLBLOCK_ALLOCATION_SPACE * 2
@@fix2:
	mov	[esi].psi_16kmax,al
	mov	[esi].psi_16kfree,al
	sub	ecx,eax
	shl	ax,2
	mov	[esi].psi_4kfree,ax
	mov	ebx,edi
	shr	ebx,10			; convert byte address to K
	mov	[esi].psi_addressK,ebx
	shl	eax,12			; convert 4K count to bytes
	add	edi,eax			; okay to increment edi free memory ptr, it's not used further

; never expand this block, the descriptor holds a real XMS handle rather than
;  an XMS pseudo-handle/pointer taken from the XMS handle array
	or	[esi].psi_flags,POOLBLOCK_FLAG_DONTEXPAND

	or	ecx,ecx
	je	compute_anchor	; no more memory to distribute to allocation blocks
	add	esi,POOLBLOCK_TOTAL_SPACE
	cmp	esi,fs:PoolAllocationEnd
	jb	@@fixblkloop

; In addition the situation of the entries in PAGE the Tables for EMS the window is to be
; determined. (Saves the need for doing later returning calculations)
;
compute_anchor:
        MOVZX   EAX,FS:[_FRAME]		    ; D000 Segmentaddress of the Frame
        SHL     EAX,4			    ; D0000 ... recalculate in absolute Adress
	MOV     ESI,FS:[PAGEDIR]
	SHR     EAX,10
	PUSH    EAX
	SHR     EAX,10			    ; Determine the pointers in the page table
	AND     EAX,111111111100B
	AND     ESI,NOT 111111111111B	    ; Throw statusbits out
	MOV     ESI,[ESI+EAX]		    ; read ^ of Page Table
	POP     EAX
        AND     EAX,111111111100B	    ; Same game again
	AND     ESI,NOT 111111111111B
	ADD     ESI,EAX
	MOV     FS:[FRAMEANCHOR],ESI
;(*-5-*)
; Small note about the following command: the Descriptor-Cache of the FS-
; Segmentregister contains after the execution again the old Segment-
; border! (Here that is insignificant, because no further
; accesses follows.)
	MOV     WORD PTR FS:[LDT+(V86_DATA_SEL AND 0F8H)],DATA_LEN
;
; Now finally all is prepared, so with a Task-switch the
; virtual 8086-Monitor can be started.
;
        JMP     DWORD PTR CS:[@@V86_ENTRY_ADDR]; force Task-change
@@V86_ENTRY_ADDR:
	DW      0,V86_TSS_SEL        ; "Address" Task State Segment
GO_PROTECTED ENDP

CODE    ENDS
;
; The actual Monitor for the virtual 8086-Modus. This range
; is paged out in the Extended Memory at 1 MByte.
;
V86     SEGMENT
	ASSUME  CS:V86,ES:NOTHING,FS:NOTHING,GS:NOTHING
;
; Within this routine the interrupts and exceptions handled as simply the
; interrupt call into the virtual 8086-Modus is reflected. Exceptions form
; only the error INTs 08h to 0Fh, which must be examined for it, if they are
; generated from hardware.
;
V86_MONITOR PROC NEAR
	PUSH    EAX
	PUSH    EBX
	PUSH    ECX

	movzx   esp,sp

					; ECX is a pointer into iterrupt table
					; or intno*4
	MOV     cX,[ESP+12]		; the return address = from where we were called
	SUB     cX,OFFSET INT_TABLE	; return address as unique identifier
	AND     ecX,0fffch		; for the corresponding IDT entry!
									; (ECX/4 = interrupt number)

; With the interrupts 08h - 0Fh is to be examined whether they were released by INT
; xx-command or hardware, or whether it concerns an exception interrupt .
; In addition it can also be, that the Monitor is being called recursively,
; because a HLT-command must be executed.
;
        CMP     ESP,V86_TOS-36H				; What did actually happen?!
	JZ      @@V86_ABORT				; an exception interrupt
        JB      @@PROTECTED			; Monitor is being interrupted

@@REENTRY:
        MOV     BX,UNIVERSE_SEL				; use 4-GByte-range for full access
        MOV     DS,BX					; to all Bytes.

	SUB     WORD PTR [ESP+14+12],6			; Create space for INT data

	MOVZX   EBX,WORD PTR [ESP+14+16]		; linear address of 86er-Stacks
	SHL     EBX,4					;
	movzx   eax,word ptr [ESP+14+12]		; use ONLY low portion SP,
	add 	ebx,eax					; as ESP may be undefined !


							; copy Interrupt frame down
	MOV     AX,[ESP+14]			        ; IP
	MOV     [EBX],AX
	MOV     AX,[ESP+14+4]				; CS
	MOV     [EBX+2],AX
	MOV     AX,[ESP+14+8]				; Flags
	MOV     [EBX+4],AX

;
; With the hardware interrupts 08h to 0Fh as well as 70h to 77h the interrupt flag is
; to be blocked

; always clear interrupt flag, irrespective of whether hardware IRQ
;; reflecting an IRQ to whichever handler the V86 task "IDT" offers)...
;
;	CMP     CX,08H*4                   ; range 08h to 0Fh ?
;	JB      SHORT @@V86_CONT	   ; Otherwise, then further
;	CMP     CX,0FH*4
;	JBE     SHORT @@CLR_IF
;	CMP     CX,70H*4                   ; Only within the range 70h to 77h
;	JB      SHORT @@V86_CONT
;	CMP     CX,77H*4
;	JA      SHORT @@V86_CONT

@@CLR_IF:
	AND     WORD PTR [ESP+14+8],NOT 200H; Clear IF: stop Interrupts
;				; clear IF if trigger was an hardware IRQ
;				; * decided by int number, so reprogramming 8259
;				; * would break this! Maybe an error code given to the
;				; * handler could tell in more reliable way?
@@V86_CONT:
	AND     WORD PTR [ESP+14+8],NOT 100H; Clear TF: single step!
		    				; clear TF in any case

        MOV     BX,[ECX]			; Write new adress CS:IP from the 8086-
        MOV     [ESP+14],BX			; Tabel in the jumpback-address
        MOV     BX,[ECX+2]		        ; on the stack
	MOV     [ESP+18],BX

@@Clean:
        POP     ECX				; Clean everything properly
        POP     EBX
	POP     EAX

        INC     ESP				; Correct old call-adress
	INC     ESP

        IRETD		                        ; Return to virtual 86-Modus
                                                ; (only) via IRETD !
;
; Remove EFLAG, CS, IP, that are saved by the Interrupt again
; * This is called when an exception is generated by the monitor itself,
; * which we recognize by how much context is on stack (v86
;

@@PROTECTED:
        MOV     AX,[ESP+12]          ; Put new Interrupt-Nr (= return-
        MOV     [ESP+24],AX          ; address) in the correct Pos.
        MOV     EAX,[ESP+8]          ; move EAX, EBX, ECX
        MOV     [ESP+20],EAX         ; to above,
        MOV     EAX,[ESP+4]          ; so the stack for the virtual
        MOV     [ESP+16],EAX         ; Registers EFLAG, CS, IP can be cleared.
        MOV     EAX,[ESP]
	MOV     [ESP+12],EAX
	ADD     ESP,12               ; throw away EFLAG, CS, IP Protected Mode
	JMP     @@REENTRY            ; and go on.


@@V86_ABORT:
	CMP     CX,0DH*4             ; Privilegviolation ?
        JNZ     @@V86_ABORT_IT	     ; Everything else lies below it after all!

; see if SoundBlaster INT 3 forced to 1ah error code GPF
	CMP	DWORD PTR [ESP+14],1ah
	jne	notsb
	mov	ax,V86_DATA_SEL
;	push	ds
	mov	ds,ax
	cmp	_SB,0
;	pop	ds
	je	notsb					; SB option not turned on
	mov	ax,3*4+OFFSET INT_TABLE	; set configuration as if INT 3 occurred
	mov	[ESP+16],ax				; emulate call from INT 3 call address
	pop	ecx
	pop	ebx
	pop	eax
	add	sp,4					; discard excess GPF error code
	inc	DWORD PTR [ESP+2]		; increment EIP past INT 3 instruction
	jmp	V86_MONITOR
notsb:

        MOV     AX,UNIVERSE_SEL      ; Hello World, hello Universe !
	MOV     DS,AX

        MOVZX   EAX,WORD PTR [ESP+18+4]; examine whether a HLT-command the
	SHL     EAX,4                  ; has caused the Privilege fault
	ADD     EAX,[ESP+18]

					; EAX = linear CS:IP

;
; examination of the errorreleasing instruction
;
; check which command triggered the GPF: Special treatment for
; I/O commands (I/O only causes GPF for masked ports, as we only
; simulate the DMA chip) and HLT (if at magical locations, it is
; a wink by the V86 part of EMM386 to trigger DMA processing after
; int 13h / 40h). Non-magical HLT is handled by doing HLT for the
; user in the monitor. No other handling yet (e.g. privileged commands,
; LOCK and the like are just translated to an int 6, illegal opcode,
; which is then reflected to the V86 task!). The DMA chip has no 32bit
; registers, so only 8/16bit IN/OUT is trapped. String IN/OUT is not
; trapped either, but makes no sense with DMA chip anyway.
;
	MOV     BL,[EAX]                ; check opcode

	cmp	bl,0fh				; check if potentially mov <reg>,cr#
	je	@@ExtendedOp

	CMP     BL,0F4H                 ; HLT-
        JZ      @@Is_Hlt                ; command ?

	CMP     BL,0E4H                 ; IN/OUT ??
	JB      @@V86_ABORT_IT                   ;
	CMP     BL,0E7H                 ;
	JBE     @@DoIO_Im               ;
	CMP     BL,0ECH                 ;
	JB      @@V86_ABORT_IT                   ;
	CMP     BL,0EFH
	JBE     @@DoIO_DX

					; no handling for 32bit IN/OUT or other potential GPF causes

@@NIX:
  JMP     @@V86_ABORT_IT          ; else complain!

@@ExtendedOp:
	cmp	BYTE PTR [eax+1],9
	je	@@wbinvd
	cmp	BYTE PTR [eax+1],8
	je	@@invd
	cmp	BYTE PTR [eax+1],32h
	je	@@rdmsr
	cmp	BYTE PTR [eax+1],30h
	je	@@wrmsr
	cmp	BYTE PTR [eax+1],31h
	je	@@rdtsc

	cmp	BYTE PTR [eax+1],20h
	jb	@@NIX		; not an opcode we emulate
	cmp	BYTE PTR [eax+1],23h
	ja	@@NIX

; opcodes 0F 20 xx to 0F 23 xx emulated via self-modifying code
	pop	ecx
	push	fs
	mov	bx,V86_DATA_SEL
	mov	fs,bx
	mov	ebx,DWORD PTR fs:[server_GDT_Code]
	mov	DWORD PTR fs:[alias_selector],ebx
	mov	ebx,DWORD PTR fs:[server_GDT_Code+4]
	mov	DWORD PTR fs:[alias_selector+4],ebx
	mov	BYTE PTR fs:[alias_selector+5],92h	; 16-bit r/w data segment
	mov	bx,V86_ALIAS_SEL
	mov	fs,bx

	mov	bl,[eax+2]	; get third byte of opcode
	mov	fs:[EmuInstr+2],bl
	mov	bx,[eax]	; first/second opcode bytes
	mov	WORD PTR fs:[EmuInstr],bx
	jmp	nextinstr	; feeble cache flush

nextinstr:
	pop	fs
	pop	ebx
	pop	eax
EmuInstr	DB	90h,90h,90h

	add	WORD PTR [esp+6],3	; jump over emulated instruction
	jmp	@@Extret

@@wbinvd:
	wbinvd				; 0f 09 is wbinvd opcode
invdshare:
	pop	ecx				; restore registers
	pop	ebx
	pop	eax
	jmp	@@twoeat

@@invd:
	invd				; 0f 08 is invd opcode
	jmp	invdshare

@@wrmsr:
	mov	ecx,[esp]		; get original ecx value
	mov	eax,[esp+8]		; get original eax value
;	wrmsr				; 0f 30 is rdmsr opcode
	DB	0fh,30h			; not all assemblers know RDMSR, so force it in there
	jmp	invdshare		; restore and exit

; early pentiums and such will throw an exception on rdtsc instruction in V86
;  regardless of CR4 setting, later CPU versions won't
@@rdtsc:
;	rdtsc					; 0f 31 is rdtsc opcode
	DB	0fh,31h			; not all assemblers know RDTSC, including this one
	jmp	rdmsrshare

@@rdmsr:
	mov	ecx,[esp]		; get original ecx value
;	rdmsr				; 0f 32 is rdmsr opcode
	DB	0fh,32h			; not all assemblers know RDMSR

rdmsrshare:
	pop	ecx				; restore registers
	pop	ebx
	add	esp,4			; eat original eax value

@@twoeat:
	add	WORD PTR [esp+6],2	; jump over instruction

@@Extret:
	add	esp,6			; eat return address and error code
	iretd

@@Is_Hlt:

        CMP     WORD PTR [ESP+18+4],SEG MONDATA ; Was a special HLT-command
        JNZ     @@Do_Hlt		; the cause for the


	MOV     AX,[ESP+18]             ; General Protection Fault ?
	CMP     AX,OFFSET HLT13		; * magical HLT in our disk / int 13 handler
        JZ      TRANSFER_BUFF		; copy buffer back
	CMP     AX,OFFSET HLT40		; * magical HLT in our disk / int 40 handler
	JZ      TRANSFER_BUFF
@@Do_Hlt:
;(#-7-#)

	INC     WORD PTR [ESP+18]   ; Jump over the HLT instruction
        POP     ECX                 ; restore Register
	POP     EBX
	POP     EAX
        ADD     ESP,6               ; throw away errorcode & returnaddress.
        STI                         ; give Interrupts free and then wait,
        HLT                         ; wait, wait........
	iretd		; but will it ever hit this?



;*************************************************************
; this driver isn't abortable (it holds UMBs,...)
;
; so we generate an INT 6 (invalid opcode) interrupt, and hope,
; that DOS aborts us
;
;*************************************************************
@@V86_ABORT_IT:                      ; Now conclusion is however final  !

						;>> by mov ax,[ffff]
		;>> by AX,0FFF1h		; otherwise provoke privilege errors
		;>> LMSW	 AX		; at the uninstall

						; ESP+22 = cs
						; ESP+18 = ip


        MOV     BX,UNIVERSE_SEL			; 4-GByte-range for to utilize full access
        MOV     DS,BX			        ; to all Bytes .
        MOVZX   EBX,WORD PTR [ESP+18+16]	; Calculate the absolute state
        SHL     EBX,4			        ; of the 86-Stack
	ADD     EBX,[ESP+18+12]			; (ESP 86er-Stack in addition)

	SUB     DWORD PTR [ESP+18+12],6		; room for IP,CS,FLAGS

        MOV     AX,[ESP+18]			; copy IP
	MOV     [EBX-6],AX
        MOV     AX,[ESP+18+4]			; dto. save CS
	MOV     [EBX-4],AX
        MOV     AX,[ESP+18+8]			; edit Flags
	MOV     [EBX-2],AX
;


                                    ; simulate invalid opcode interrupt
        MOV     BX,[ds:6*4]			; Write new address CS:IP of the 8086-
        MOV     [ESP+18],BX			; Table in the returnaddress
        MOV     BX,[ds:6*4+2]			; on the stack
	MOV     [ESP+18+4],BX

	AND     WORD PTR [ESP+18+8],NOT 100H	; clear TF (single step off)


        POP     ECX				; Clean everything again
	POP     EBX
	POP     EAX

        INC     ESP			        ; Correct old calling-address
	INC     ESP

	add     ESP,4   			; remove error word

        IRETD					; Return to virtual 86-Modus
                                                ; (only) via IRETD !

;***************************************


;        MOV     DX,CX				; Calculate number of the Interrupt
;        SHR     DX,2
;        JMP     DWORD PTR CS:[@@V86_ABORT_ADDR]
;@@V86_ABORT_ADDR:
;        DW      OFFSET RETURN_OF_THE_86,REAL_SEL

;(#-8-#)
;
; an IN/OUT-command now HAS to be emulated and the data has to be checked
;
@@DoIO_Im:
        REPCMD  PUSH,<EDX,ESI,EDI>		; save yourselves!
        INC     EAX				; turn into Opcode and read
        MOVZX   DX,BYTE PTR [EAX]	        ; the I/O-Adresse
	ADD     WORD PTR [ESP+18+12],2		; jump over instruction
@@WithDX:
	MOV     AX,V86_DATA_SEL
	MOV     DS,AX
        TEST    BL,00000010B			; Is it a IN-command ?
	JNZ     @@Im_Out			; Noe ... 'n oller OUT.
        TEST    BL,00000001B			; Width Word or Byte ?
	JNZ     @@Im_Word
        IN      AL,DX				; Read the date from Port
        MOV     [ESP+8+12],AL			; and return it.
@@Bye:  REPCMD  POP,<EDI,ESI,EDX,ECX,EBX,EAX>
	ADD     ESP,6
	IRETD

@@Im_Word:
        IN      AX,DX				; Same action like above but
        MOV     [ESP+8+12],AX			; now with/for 16 Bit
	JMP     @@Bye

@@Im_Out:
	MOV     AL,[ESP+8+12]			; Save opcode because of Byte/Word-Flag.
	REPCMD  PUSH,<BX,DX>			; First send Low Byte to the
	CALL    @@Do_IO				; port.
        REPCMD  POP,<DX,BX>			; (call back) Repeat Opcode and test,
        TEST    BL,00000001B			; if the Hi-Byte still can be send
        JZ      @@Bye
	INC     DX
	MOV     AL,[ESP+8+1+12]
	CALL    @@Do_IO
	JMP     @@Bye

@@Do_IO:
        CMP     DX,80H				; Has a Page-register
        JB      LIKE_DMA			; been adressed, or one
        CMP     DX,8FH				; of both DMA-
	JBE     LIKE_PAGE			; Controllers ?
	JMP     LIKE_DMA

@@DoIO_DX:
        REPCMD  PUSH,<EDX,ESI,EDI>	    	; save Register  .
	INC     WORD PTR [ESP+18+12]		; Jump over instruction
	JMP     @@WithDX

; Copy the DMA-buffercontents after termination of the DISK-I/O to the
; wanted target/location
; * This is triggered by an HLT at a magical location in our own int 13
; * and int 40 handlers. First the original int 13 / 40 is done, and
; * then things are copied to / from where the mapped memory -really- is.
; * Should only trigger if a matching DMA is pending. Tricky!
;
TRANSFER_BUFF PROC NEAR
        INC     WORD PTR [ESP+18]       ; Jump over HLT-command
	REPCMD  PUSH,<DS,ES,ESI,EDI>
	MOV     AX,V86_DATA_SEL
	MOV     DS,AX
	MOV     ESI,[BuffStart]         ; Get the basic data of the block which
	MOV     EDI,[TargetAdr]         ; can be shifted. Read currently
					; simulated DMA source / dest. / length

	MOV     ECX,[BuffLen]
	MOV     AX,UNIVERSE_SEL         ; The large liberty  ... every
	MOV     DS,AX                   ; wall fall simetimes!
	MOV     ES,AX
	CLD
	MOV     EAX,ECX
	AND     ECX,3
	REP     MOVS BYTE PTR [ESI],BYTE PTR [EDI]
	BIG_NOP
	MOV     ECX,EAX
	SHR     ECX,2
	REP     MOVS DWORD PTR [ESI], DWORD PTR [EDI]
	BIG_NOP
	REPCMD  POP,<EDI,ESI,ES,DS,ECX,EBX,EAX>
        ADD     ESP,6                   ; throw away return-address.
        IRETD                           ; back to virtual 8086-
TRANSFER_BUFF ENDP			; Mode.
;
; Access to a pageregister
;
; In: DX : Port-address; AL : Value
;
LIKE_PAGE PROC NEAR
        OUT     DX,AL				; release data
        MOV     BX,DX				; Look up the number that corresponds to the port
        MOVZX   EDI,BYTE PTR PageLookUp[BX-80H]
	MOV     PageReg[EDI],AL			; Buffer Bits 24-16
	BTR     ChanFlags[EDI*2],PagePrgrd	; Program page register
	JMP     READY?
LIKE_PAGE ENDP
;
; Supervise certain registers of the DMA (direct memory access) components
;
LIKE_DMA PROC NEAR
	OUT     DX,AL                   ; Pass on only once ...
        CMP     DX,0C0H                 ; Should the 2nd Controller
        JB      @@What?                 ; be programmed ?
        SUB     DX,0C0H-20H		; Then convert I/O-Adress in the range
	SHR     DX,1                    ; 10h - 1Fh
@@What?:
	CMP     DX,7                    ; Program addresses and/or length of the blocks
	JBE     @@BlockSet              ;
	CMP     DX,10H                  ; dto. 2nd DMA-Controller ?
	JB      @@Mode?
	CMP     DX,17H
	JBE     @@BlockSet
@@Mode?:
	CMP     DX,0BH                  ; "Mode Register" responded ?
	JZ      @@Mode8                 ; DMA-Controller #1 ?
	CMP     DX,1BH                  ; dto. #2 ?
	JZ      @@Mode16
	CMP     DX,0CH                  ; Soll das Hi/Lo-FlipFlop ge-
	JZ      @@Clear8                ; loescht werden ?
	CMP     DX,1CH
	JZ      @@Clear16
	JMP     @@Bye                   ; If everything fails ...
;
; Access to Startadress and/or Blocklength of a DMA-Channel
;
@@BlockSet:
        MOVZX   EDI,DX			; Compute the DmaChannel-number from the
	AND     DI,0110B                ; I/O-Adress (likewise already transformed)
	SHR     DI,1
        MOV     BX,HiLoFlag1            ; ... currently Controller #1
	CMP     DX,10H                  ; Something nevertheless the second CONTROLLER
	JB      @@Len?                  ; addressed ?
	ADD     DI,4                    ; Starting from that 4. Channel  (because of
	MOV     BX,HiLoFlag2            ; Private...)
@@Len?: TEST    DX,0001H                ; meant block length  ?
	JNZ     @@Length

        BTC     [Flags],BX			; toggle the Hi/Lo-Flag. Is
	JNC     @@AdrLo				; the Lo-Byte programmed ?
	MOV     BYTE PTR BaseAdr[EDI*2+1],AL	; Set the bits with high order
	BTR     ChanFlags[EDI*2],BasePrgrd	; 8-15 and clear Flag there
	JMP     READY?				; completely programmed.
@@AdrLo:
	MOV     BYTE PTR BaseAdr[EDI*2],AL	; Program Lo-Byte,
	BTS     ChanFlags[EDI*2],BasePrgrd	; so set Flag, while
	JMP     @@Bye				; still 8 Bits follow.

@@Length:
        BTC     [Flags],BX			; toggle Hi/Lo-Flag again.
	JNC     @@LenLo				; Otherwise nearly the same
	MOV     BYTE PTR BlockLen[EDI*2+1],AL   ; takes place here as before.
	BTR     ChanFlags[EDI*2],LenPrgrd	;
	JMP     READY?
@@LenLo:
        MOV     BYTE PTR BlockLen[EDI*2],AL	; programm Lo-Byte
	BTS     ChanFlags[EDI*2],LenPrgrd
	JMP     @@Bye

READY?: TEST    ChanFlags[EDI*2],1111B
						; = BasePrgrd+LenPrgrd+PagePrgrd+ModePrgrd
        JNZ     @@Bye			        ; All Registers initialised ?
	TEST    [Flags],11B	                ; INT13Active+INT40Active.
						; Is currently a Disk-
        JZ      @@Bye				; I/O-INT currently active ?
						; If not, (keep an eye wide shut and continue..)
						; Eyes too and through...
        CALL    CHK_CHANNEL			; Then check value
@@Bye:  RET
;
; Monitor "Mode Register" for the Transferdirection DMA <--> I/O
;
@@Mode8:
        MOVZX   EDI,AL				; Mask away the number of the DMA-Channel
	AND     DI,0011B
@@Mode8@16:
	SHL     AL,2				; Mask away the transfer direction from the
	AND     AL,00110000B			; remaining data
        AND     ChanFlags[EDI*2],NOT 00110000B	; and wrote it in the Statusdata
        OR      BYTE PTR ChanFlags[EDI*2],AL	; of the corresponding Channel
        BTR     ChanFlags[EDI*2],ModePrgrd
	JMP     READY?
@@Mode16:
	MOVZX   EDI,AL
	AND     DI,0011B			; Likewise out-mask the number of the DMA
						; (direct memory access) channel.
        ADD     DI,4				; It's about a 16bit channel
	JMP     @@Mode8@16
;
; Clear the Hi/Lo flip-flop of the appropriate DMA (direct memory access) CONTROLLER
;
@@Clear8:
        BTR     [Flags],HiLoFlag1		; It was like that already
	JMP     @@Bye
@@Clear16:
        BTR     [Flags],HiLoFlag2		; Number 2's turn..
	JMP     @@Bye
LIKE_DMA ENDP
;
; Check a memory-area, whether it lies in continuous physical memory
; in pages of 4K each
;
; In:  ESI: linear Startadress
;      ECX: Length of the range
; Out: CY-Flag: reset, ESI = physical adress
;               set, not constantly
;
CONTINUOUS? PROC NEAR
        PUSH    ES                      ; We need here immediately a bit
        MOV     AX,UNIVERSE_SEL         ; more space in Adress-space,
	MOV     ES,AX                   ; daher nun aufpusten...
	PUSH    ECX
        MOV     EAX,ESI                 ; Act like the Block begins,
        AND     EAX,4095                ; at a pageborder

	or	ecx,ecx
	setz	cl		; make count relative zero with 0- same as 1-byte check
	dec	ecx

        ADD     ECX,EAX                 ; for that, change Length
        PUSH    ESI                     ; to save later
	MOV     EAX,ESI
        SHR     ESI,20                  ; Determine the pointer of the corresponding
	AND     ESI,111111111100B	; entry in the page table.
	MOV     EBX,CR3
	ADD     ESI,EBX
	MOV     ESI,ES:[ESI]
	AND     ESI,NOT 4095
	SHR     EAX,10
	AND     EAX,111111111100B
	ADD     ESI,EAX
	MOV     EAX,ES:[ESI]		; Retreive state of the page in the phys. Adress
	AND     EAX,NOT 4095		; range
        PUSH    EAX                     ; save Bits 31-12
        SHR     ECX,12                  ; Calculate number of
        JCXZ    @@Ok                    ; overlapping pages. Zero is OK!
@@Loop: ADD     EAX,4096                ; Go one page and
        ADD     ESI,4                   ; one entry further
        MOV     EBX,ES:[ESI]
        AND     EBX,NOT 4095            ; no memory in between?
	CMP     EAX,EBX
        JNZ     @@Fail                  ; ...Below!
	LOOP    @@Loop
@@Ok:   REPCMD  POP,<EAX,ESI,ECX,ES>    ; The physical address results
	AND     ESI,4095                ; from the bits 31-12 of the page
	OR      ESI,EAX                 ; and the offset 11-0
	CLC                             ; ...its ok.
	RET
@@Fail: REPCMD  POP,<EAX,ESI,ECX,ES>    ; restore Register.
        STC			        ; ...not possible  !
	RET
CONTINUOUS? ENDP
;
; A DMA-Channel is completely with data about beginning and length
; supplied, so that a check can (and has to) take place.
;
; In: EDI: Channelnumber 0..7
;
CHK_CHANNEL PROC NEAR
        MOVZX   ECX,BlockLen[EDI*2]     ; Calculate length of the blocks
        INC     ECX                     ; which are transferred
        CMP     EDI,4                   ; In case we're dealing with a
        JB      @@Only8                 ; 16bit DMA-channel , words are transferred.
        ADD     ECX,ECX
        MOVZX   ESI,PageReg[EDI]        ; The base-adress always lies on a
        SHL     ESI,15                  ; Word-border and Bit 0 of the
        MOV     SI,BaseAdr[EDI*2]       ; pageregister is being ignored
	SHL     ESI,1
	JMP     @@Chk
@@Only8:
        MOVZX   ESI,PageReg[EDI]        ; For 8-Bit-DMA-Channels the
        SHL     ESI,16                  ; Adress-calculation is a bit
        MOV     SI,BaseAdr[EDI*2]       ; more easy...
@@Chk:
        BTR     [Flags],NeedBuffer      ; Initialise.
	MOV     [TargetAdr],ESI		; For savety save the data about the block
	MOV     [BuffLen],ECX
        CALL    CONTINUOUS?             ; Is the block occupying directly-(each other)following pages?
        JNC     @@Set

        MOV     AL,BYTE PTR ChanFlags[EDI*2]	; If a verify is wanted
        AND     AL,00110000B			; the buffer is completely unneeded
	JZ      @@Bye
	MOV     ESI,[BuffStart]			; if not, go over Buffer (and draw 4000 clocks...)
	BTS     [Flags],NeedBuffer
@@Set:
	MOV     DX,DI                   ; Calculate I/O-address from the
        ADD     DX,DX                   ; channelnumber
        CMP     EDI,4                   ; 8-bit or 16-bit channel ?
	JB      @@Set8

	BTR     [Flags],HiLoFlag2       ; DMA #2, HiLo-FlipFlop-Flag
        ADD     DX,DX                   ; The 2nd DMA-Controller
        ADD     DX,0C0H                 ; is located at 0C0h
	OUT     [0D8H],AL               ; Clear Hi/Lo-FlipFlop
	JMP     $+2                     ; ... leave some time.
	SHR     ESI,1                   ; The Baseadress is now
	MOV     AX,SI                   ; newly written in the Controller
	OUT     DX,AL                   ; Lo before Hi-Byte
	MOV     AL,AH                   ; in the usual Intel way.
	JMP     $+2
	OUT     DX,AL
	MOVZX   DX,PageXLat[DI]         ; Set I/O-Adress of the page-
	SHR     ESI,15                  ; registers.
	MOV     AX,SI                   ; Ship still the remaining 8 Bit of
	JMP     $+2                     ; the 24-Bit-Adress in the page register.
	OUT     DX,AL
	JMP     @@Cont
@@Set8:
	BTR     [Flags],HiLoFlag1       ; Clear DMA #1, HiLo-FlipFlop-Flag
	OUT     [0CH],AL                ; Hi/Lo-FlipFlop ...
	JMP     $+2                     ; ... and the remaining.
        MOV     AX,SI                   ; Program the base-address anew
        OUT     DX,AL                   ;
	MOV     AL,AH
	JMP     $+2
	OUT     DX,AL
        MOVZX   DX,PageXLat[DI]         ; Like above, but now that the
        SHR     ESI,16                  ; Adress-calculation appears to be a bit different
        MOV     AX,SI                   ; (and easier) with 8-Bit-DMA-
        JMP     $+2                     ; Channels.
	OUT     DX,AL
@@Cont:
; Now check, if the data (because of "Save") still needs to be copied first
; in the buffer

        BT      [Flags],NeedBuffer      ; Should a transfer happen via buffer at all?
        JNC     @@Bye
        MOV     AL,BYTE PTR ChanFlags[EDI*2] ; With a  Save (Memory
        AND     AL,00110000B            ; -> I/O) first initialize
        CMP     AL,00100000B            ; the buffer with the data
	JNZ     @@Bye

        MOV     ESI,[TargetAdr]         ; First copy the needed
        MOV     EDI,[BuffStart]         ; memoryblock in the
        MOV     ECX,[BuffLen]           ;  DMA-Buffer
	REPCMD  PUSH,<DS,ES>
	MOV     AX,UNIVERSE_SEL         ; ...a bit Universum
	MOV     DS,AX                   ; looks in now (Greetings also at
	MOV     ES,AX                   ; ' boundless ' !)
	CLD
	MOV     EAX,ECX
	AND     ECX,3
	REP     MOVS BYTE PTR [ESI],BYTE PTR [EDI]
	BIG_NOP
	MOV     ECX,EAX
	SHR     ECX,2
	REP     MOVS DWORD PTR [ESI], DWORD PTR [EDI]
	BIG_NOP
	REPCMD  POP,<ES,DS>
        BTR     [Flags],NeedBuffer      ; can go...
@@Bye:
	RET
CHK_CHANNEL ENDP
;(#-8-#)

;
; PAUSE :
;         wait a little bit, incrementing some screen memory -
;         so you see some little flashing before the program
;         looses control of itself :-)
;


pauser proc near

	push ds
	push ax
	mov ax,UNIVERSE_SEL
	mov ds,ax

	add ebx, 0b0000h

	mov ax,0ffffh

@@loop:
	inc word ptr ds:[ebx]
	inc word ptr ds:[ebx+2]
	inc word ptr ds:[ebx+4]
	dec ax
	jne @@loop

	add ebx, 08000h

	mov ax,0ffffh

@@loop1:
	inc word ptr ds:[ebx]
	inc word ptr ds:[ebx+2]
	inc word ptr ds:[ebx+4]
	dec ax
	jne @@loop1


	pop ax
	pop ds
	ret

pauser endp

V86_MONITOR     ENDP

; this handler is always present unless NOALTBOOT option is on.
; the following behavior remarks are no longer true:
;
;; Special Ctrl-Alt-Del handler (should be off by default and only
;; be enabled by the ALTBOOT option!). Translates Ctrl-Alt-Del into
;; cold (out 64,fe) reboot to make sure that mapping / protected mode
;; is not blocking the proper reboot process.
;
KBOARD PROC NEAR
        PUSH    EAX                     ; Everyone for himself! Evacuate or be wiped out!

;		; * should directly jump to @@CONT here unless ALTBOOT is enabled!

;	mov	ax,V86_DATA_SEL
;	mov	ds,ax
;	cmp	_NoAltBoot,0
;	jne	@@CONT

;	MOV     AX,UNIVERSE_SEL		  ; flat 4 GB data segment
;	MOV     DS,AX

	movzx   esp,sp
	movzx	eax,WORD PTR [esp+6+4]	; get return CS
	shl	eax,4			; convert segment to address
	add	eax,[esp+6]		; add in return EIP
	cmp	eax,0ffff2h		; could only have come from FFFF:2 address
	je	kb_portreset

	mov	ax,V86_DATA_SEL
	mov	ds,ax
	cmp	_NoAltBoot,0
	jne	@@CONT

	MOV     AX,UNIVERSE_SEL		  ; flat 4 GB data segment
	MOV     DS,AX

kb_keycheck:
        IN      AL,[PORT_A]	          ; get key-scancode
        CMP     AL,53H                    ; DEL ?
	JNZ     SHORT @@CONT
        MOV     AL,DS:[@KB_FLAG]          ; Have the keys CTRL & ALT
        AND     AL,1100B                  ;  been pressed ?
        CMP     AL,1100B                  ; If not,  continue working
	JNZ     SHORT @@CONT


					; why don't restart if E0 ??
					; TE 01 jul 02
					; these 2 changes help toshiba notebooks reboot.

;		TEST    BYTE PTR DS:[@KB_FLAG_3],10B; 0E0h sent (special key)
;		JNZ     SHORT @@CONT			; ? then never restart !

;							; and a warm start isn't a good idea ...

		MOV     WORD PTR DS:[@RESET_FLAG],1234H; Otherwise one


kb_portreset:
;	mov     al,0feh			; reset, using the keyboard
;;	out	dx,al			; (pulse hardware reset line)
;	out	64h,al			; (pulse hardware reset line)


					; if that doesn't work,
					; do a more conventional
					; jmp ffff:0 from real mode


	JMP     DWORD PTR CS:[@@ABORT_ADDR]; not Warmstart and
@@ABORT_ADDR:                        ; back to REAL-Mode
	DW      OFFSET RETURN_OF_THE_86,REAL_SEL


@@CONT: POP     EAX
	JMP     V86_MONITOR		; JMP, not CALL, as the address
					; where the call comes from must
					; stay on stack for the handler.
KBOARD ENDP
;
; Entrypoint because of the task swap after the switch into
; protected mode to start the virtual 8086 -working
; Note: we're currently in a 16-bit segment!
;
V86_START PROC NEAR
	PUSH    0
	PUSH    0                    ; GS  The Stackframe fo the
	PUSH    0                    ;     switch to 8086-mode
	PUSH    0                    ; FS
	PUSH    0                    ;
	PUSH    SEG MONDATA             ; DS
	PUSH    0
	PUSH    0                    ; ES
	PUSH    0
	PUSH    SEG TMP_STACK        ; SS
	PUSH    0
	PUSH    OFFSET TMP_TOS       ; ESP
	PUSH    0002H                ; EFLAGS: VM, IOPL=3, IF
	PUSH    3200H
	PUSH    0
	PUSH    SEG _TEXT            ; CS
	PUSH    0
	PUSH    OFFSET KEEP          ; EIP
	CLTS                         ; TS-Flag Clear (Task Switch) absolutely
                                     ; thus the next
                                     ; x87-command without INT 7 is being executed

;(*-6-*)
        MOV     AX,V86_DATA_SEL	     ; Throw out still the "Anchor" for the
        MOV     FS,AX                ; virtual memory-administration needed
        MOV     EAX,FS:[PAGEDIR]     ; Tables
	MOV     CR3,EAX
        MOV     EAX,CR0              ; Now enable Paging
	OR      EAX,80000000H
	MOV     CR0,EAX
;(*-6-*)
        IRETD                        ; With this single command the 8086-workingmode
                                     ; is being started.
V86_START ENDP
;
; here starts the interrupt table of the virtual 8086 Monitor.
; now only these table addresses are validm the tables from 0:0 not anymore

INTENTRY_O MACRO SEQNR               ;; Unfortunaly the symbol name INTxx is only
INT_&SEQNR:                           ;; correctly produced in this way.
		ENDM

INTENTRY MACRO VON,BIS               ;; Generate entrypoint  for Interrupts in the
                LOCAL   ENTRY        ;; indicated range (for
ENTRY   =       VON                  ;; Mathematicians: Interval)
                REPT    BIS-VON+1    ;;
		INTENTRY_O %(ENTRY)  ;; Call the virtual 86-Monitor with
		CALL    V86_MONITOR  ;; the return address as reference.
		NOP                  ;; Align (at 4 Bytes)
ENTRY   =       ENTRY+1
		ENDM
		ENDM

INTENTRYNR MACRO INTRNR,ADDRESS      ;; Purposefully call a routine
		INTENTRY_O %(INTRNR)
		ADDRESS
		NOP                  ;; Align
		ENDM

INT_TABLE:
		INTENTRY 0,8                 ; Redirect All Interrupts, up to the
		INTENTRYNR 9,<CALL KBOARD> ; keyboard- Interrupt to the
				             ; Monitor

;********************************************************************
;(*-7-*)
comment $
;(*-7a-*)
	   INTENTRY 10,255  ; This line is faded out with EMM
;(*-7a-*)
$
; The line INTENTRY 10,255 directly above (*-7-*) needs to deleted!!!
;********************************************************************
		INTENTRY 10,66H
                INTENTRYNR 103,<JMP  EMM_ENTRY>; EMM-Driver
		INTENTRY 104,255
;(*-7-*)

		PURGE   INTENTRY,INTENTRY_O,INTENTRYNR

;(*-8-*)
;
; Here starts the Expanded Memory Manager (EMM) Version 4.0
;
; The definitions of the situation of the Segmentregister can of course be changed,
; when in Dispatcher EMM_ENTRY following registers are saved
;
V8086_ES        EQU     <ESP+34>          ; ES on the Level-0-Stack
V8086_DS        EQU     <ESP+38>          ; DS on the Level-0-Stack

MAX_HANDLES     EQU     255               ; There are only 255 handles to give away (lt.
                                          ; Documentation  LIM EMS 4.0)

FREEPAGE_ID     EQU     255               ; Owner Handle of a free page
EMM_ENTRY PROC NEAR
		ASSUME  FS:MONDATA
		PUSH    ESI               ; In any case save the endangered
		PUSH    EDI               ; registers
		PUSH    ECX
		CLD

                MOV     CX,UNIVERSE_SEL   ; address everything
		MOV     DS,CX
		MOV     ES,CX
		MOV     CX,V86_DATA_SEL
		MOV     FS,CX

		pop	ecx	; restore original [e]cx value
		push	ecx

		mov	fs:[VCPI_PM_call],0	; flag this is an INT 67h
		movzx	esp,sp

					  ;
					  ; hack: handle undocumented
					  ; int67/87 function = simulated int15/87
					  ;
		cmp     ah,087h
		jne	normal_emm_functions

;		pop     ecx

		CALL    SIMULATE_INT1587
;		jmp     @@BYECX
		jmp     BYEEMS


normal_emm_functions:

; EMS 4.0 functions may need CX value
;		pop	ecx
		mov	fs:[CX_Save],cx
;		push	ecx

		MOVZX   ECX,AH		    ; Set the adress of the wanted Routine.

		cmp	cl,0deh			; see if VCPI function
		jne	not_vcpi_api
		movzx	ecx,al

IF	VCPI_DEBUG	EQ	1
		cmp	al,077h
		jne	debug2
		call	VCPI_Debugging
		jmp	BYEEMS
debug2:
ENDIF

		cmp	cl,0ch
		ja	emm_INV_CALL	; invalid VCPI call
		cmp	fs:[_NoVCPI],0	; check if VCPI turned off
		jne	emm_INV_CALL	; yes, return invalid code, flags VCPI not present for 0de00h
		shl	cx,1
		call	WORD PTR cs:[OFFSET VCPI_CALL_TABLE+ECX]
		jmp	BYEEMS

not_vcpi_api:
		cmp	cl,3fh		; see if special handling function
		je	emm_special
		SUB     CL,40H		    ; If outside of the permitted range,
		JB      SHORT emm_INV_CALL    ; then report
                CMP     CL,1DH              ; failure/error
		JA      SHORT emm_INV_CALL

		cmp	fs:[_INIT_DONE],0	; see if still initializing
		je	@@makecall		; yes, don't turn off EMS functions yet

		cmp	fs:[_FlagNOEMS],0
		jne	@@limitedEMS
		cmp	fs:[_NoPageFrame],0
		je	@@makecall

; allow non-mapping EMS functions with NOEMS or no page frame
@@limitedEMS:
		cmp	cl,1
		je	@@noems_fail	; function 41h
		cmp	cl,3
		jbe	@@makecall		; function 40h,42h-43h
		cmp	cl,4
		je	@@noems_fail	; function 44h
		cmp	cl,6
		jbe	@@makecall		; function 45h-46h
		cmp	cl,0ah
		jbe	@@noems_fail	; function 47h-4ah
		cmp	cl,0dh
		jbe	@@makecall		; function 4bh-4dh
		cmp	cl,10h
		jbe	@@noems_fail	; function 4eh-50h
		cmp	cl,14h
		jbe	@@makecall		; function 51h-54h
		cmp	cl,17
		je	@@makecall		; function 57h
		cmp	cl,18h
		jbe	@@noems_fail	; function 55h-56h,58h
		cmp	cl,1ah
		jbe	@@makecall		; function 59h-5ah

@@noems_fail:
		mov	ah,91h			; feature not supported error code
		jmp	BYEEMS

@@makecall:
		SHL     CX,1
		CALL    WORD PTR CS:[OFFSET CALL_TABLE+ECX]

BYEEMS:  POP     ECX
;@@BYECX:
		POP     EDI
		POP     ESI                  ; restore Register again
		cmp	fs:[VCPI_PM_call],0
		jne	PM_ret		; don't IRETD from protected mode call

		IRETD                        ; 32-Bit-Return !!!
emm_INV_CALL:
                MOV     AH,84H               ; "Invalid Functioncode in AH"
		JMP     SHORT BYEEMS
EMM_ENTRY ENDP

; entry for EMM386 routines called from protected mode
PM_ENTRY	PROC
	push	eax
	mov	ax,ss
	lar	eax,eax
	test	eax,400000h
	jnz	stack32
	movzx	esp,sp
stack32:
	pop	eax

	cmp	ax,0de0ch		; see if switch from protected mode to V86 mode
	je	VCPI_PMtoV86	; yes, give it special handling

; in client address space
; on client stack
	pushfd
	cli					; don't allow interruptions
	push	ds			; have to save segments for p-mode entry
	push	es
	push	fs

	push	ecx
	push	esi
	mov	cx,cs
	add	cx,8			; UNIVERSE_SEL value
	mov	ds,cx
	mov	es,cx
	add	cx,8			; V86_DATA_SEL
	mov	fs,cx

	mov	ecx,cr3
;	push	ecx
;	mov	ecx,ss:[esp+4]	; original value back to ecx
	mov	fs:[CR3_Save],ecx
	mov	fs:[CX_Save],ss	; temp store client ss

	xor	ecx,ecx
	mov	cx,SEG RES_STACK
	shl	ecx,4
	add	ecx,V86_TOS
	mov	si,ds
	mov	ss,si
	xchg	esp,ecx		; ecx == client esp

; on server internal stack, in both address spaces
	mov	esi,fs:[PAGEDIR]
	mov	cr3,esi			; switch to server linear address space

; in server address space
	push	fs:[CX_Save]	; client ss to host stack
	push	ecx			; client esp to host stack

	push	esi			; setup same as dispatch routine since we're jumping in the middle
	push	edi
	push	ecx
	cld

;	mov	ecx,fs:[PAGEDIR]
;	mov	cr3,ecx			; switch to server linear address space

	mov	fs:[VCPI_PM_call],1	; flag this is a PM call, not an INT 67h
	cmp	ah,0deh
	jne	vcpi_INV_CALL	; only allow VCPI calls from protected mode interface

;	cmp	al,1			; don't allow 0de01h from protected mode
;	je	vcpi_INV_CALL
; other than de0ch, don't allow anything other than de03h,de04h,de05h from PM
	cmp	al,3
	jb	vcpi_INV_CALL
	cmp	al,5
	ja	vcpi_INV_CALL

	jmp	normal_emm_functions

vcpi_INV_CALL:
	mov	ah,8fh			; use bad subfunction code, per VCPI spec
	jmp	BYEEMS

PM_ret:
;	pop	ecx				; restore client cr3
;	mov	cr3,ecx
;	pop	ecx

	mov	ecx,fs:[CR3_Save]
	mov	cr3,ecx			; restore client cr3

; in client address space
	lss	esp,[esp]		; restore client stack

; on client stack
	pop	esi
	pop	ecx

	pop	fs
	pop	es
	pop	ds
	popfd
	DB	66h				; make 32-bit retf
	retf
PM_ENTRY	ENDP

; special EMM handler flagged by function 3fh, if signatures match
emm_special:
	cmp	fs:[CX_Save],4652h
	jne	emm_INV_CALL	; must match signature
	cmp	dx,4545h
	jne	emm_INV_CALL
	cmp	al,1	; al must be 0 (a20 disable emulate) or 1 (enable)
	ja	emm_INV_CALL
	je	spec_enable

; emulate a20 disable
	cmp	fs:[_NoDisableA20],0	; see if we're allowed to disable it
	jne	spec_done		; nope
	mov	esi,OFFSET ZeroPTE
	jmp	spec_share

spec_enable:
	mov	esi,OFFSET HMAPTE

spec_share:
	xor	ecx,ecx
	mov	edi,fs:[PAGEDIR]
spec_loop:
	mov	eax,fs:[esi+ecx*4]
	mov	ds:[edi+4*ecx+(256*4)+4096],eax	; HMA PTEs
	inc	cx
	cmp	cl,16
	jb	spec_loop

; flush TLB to update page maps
	mov	esi,fs:[PAGEDIR]
	mov	CR3,esi

spec_done:
	mov	ah,0
	jmp	BYEEMS


;************************************************************
; simulate INT15/87
;
;INT 15 - SYSTEM - COPY EXTENDED MEMORY (by RBIL)
;
;        AH = 87h
;        CX = number of words to copy (max 8000h)
;        ES:SI -> global descriptor table (see #0403)
;Return: CF set on error
;        CF clear if successful
;        AH = status
;
;Values for extended-memory copy status:
; 00h    source copied into destination
; 01h    parity error
; 02h    interrupt error
; 03h    address line 20 gating failed
; 80h    invalid command (PC,PCjr)
; 86h    unsupported function (XT,PS30)
;
;Format of global descriptor table:
;Offset  Size    Description     (Table 0403)
; 00h 16 BYTEs   zeros (used by BIOS)
; 10h    WORD    source segment length in bytes (2*CX-1 or greater)
; 12h  3 BYTEs   24-bit linear source address, low byte first
; 15h    BYTE    source segment access rights (93h)
; 16h    BYTE    more rights
; 17h    BYTE    8 bit  linear source adress, high
; 18h    WORD    destination segment length in bytes (2*CX-1 or greater)
; 1Ah  3 BYTEs   24-bit linear destination address, low byte first
; 1Dh    BYTE    destination segment access rights (93h)
; 1eh    byte    more rights
; 1fh    BYTE    8 bit  linear source adress, high
;************************************************************

SIMULATE_INT1587 proc near

;        ES:SI -> global descriptor table (see #0403)

	movzx	ecx,cx				; verify size

;	jecxz	@@ok				; we are done
	or	cx,cx
	je	@@ok			; done

	MOVZX   edi,WORD PTR [V8086_ES]; make edi = linear address of command
	SHL     edi,4
	MOVZX   esi,si
	add     edi,esi

	shl	ecx,1				; verify, source and destination
						; descriptors are ok
	dec cx				; 2*cx-1
	cmp	cx, word ptr [edi+10h]	; 16-bit overflow not an issue (0->ffff)
	ja	@@invalid_command

	cmp	cx, word ptr [edi+18h]
	ja	@@invalid_command

	inc	cx				; restore original cx
	shr	ecx,1
						; we don't care about segment access rights


						; load the source/destination
						; adresses

; 12h  3 BYTEs   24-bit linear source address, low byte first
; 17h    BYTE    8 bit  linear source adress, high
; 1Ah  3 BYTEs   24-bit linear destination address, low byte first
; 1fh    BYTE    8 bit  linear source adress, high

	push edx				; must be protected


	mov dl,[edi+17h]			; get linear source address
	shl edx,8
	mov dl,[edi+14h]
	shl edx,16
	mov dx,[edi+12h]

	mov esi,edx

	mov dl,[edi+1fh]			; get destination source address
	shl edx,8
	mov dl,[edi+1ch]
	shl edx,16
	mov dx,[edi+1ah]

	mov edi,edx

	cmp	fs:[_NOCHECK],0
	jne	@@accessok
	cmp	fs:[_MEMCHECK],0
	jne	@@memcheck

; neither MEMCHECK nor NOCHECK, only allow outside RAM 3G-4G address accesses, default
	cmp	esi,0c0000000h
	jae	@@memcheck
	cmp	edi,0c0000000h
	jb	@@accessok

; adjust for end past current memory by creating page directory entry
;  and page table entries on the fly with linear == physical mapping
@@memcheck:
	push	eax
	push	ebx
	push	edx
	push	ebp

	mov	edx,edi
	add	edx,ecx
	dec	edx				; edx == final byte of destination
	mov	eax,edi
	shr	eax,22
	shr	edx,22
	xor	bp,bp

	mov	ebx,fs:[PAGEDIR]
	cmp	eax,edx
	je	@@dentry1

	lea	edx,[ebx+edx*4]	; edx -> destination page directory entry
	cmp	DWORD PTR ds:[edx],0
	jne	@@dentry1		; page directory entry already set

; clear previous destination page directory entry
	mov	ebp,fs:[ScratchDirD2]
	or	ebp,ebp
	je	@@dupdir2
	mov	DWORD PTR ds:[ebp],0

@@dupdir2:
	mov	fs:[ScratchDirD2],edx	; update scratch destination page directory entry pointer
	mov	ebp,fs:[ScratchDest]
	add	ebp,1000h		; second scratch page table following first
	mov	ds:[edx],ebp	; page dir entry -> scratch 
	or	BYTE PTR ds:[edx],3
	mov	bp,1			; flag that second entry updated

@@dentry1:
	lea	ebx,[ebx+eax*4]	; ebx -> destination page directory entry
	cmp	DWORD PTR ds:[ebx],0
	je	@@dclear1		; no previous setting
	or	bp,bp
	je	@@checksrc		; page directory entry already set, second entry not used
	mov	eax,fs:[ScratchDest]	; eax -> start of scratch page tables
	jmp	@@dsetpages		; set page table entries

; clear previous destination page directory entry
@@dclear1:
	mov	eax,fs:[ScratchDirD1]
	or	eax,eax
	je	@@dupdir1
	mov	DWORD PTR ds:[eax],0

@@dupdir1:
	mov	fs:[ScratchDirD1],ebx	; update scratch destination page directory entry pointer
	mov	eax,fs:[ScratchDest]
	mov	ds:[ebx],eax	; page dir entry -> scratch 
	or	BYTE PTR ds:[ebx],3

; set up two scratch page tables for identity mapping
@@dsetpages:
	push	cx
	mov	cx,2048			; 2*1K dword entries
	mov	ebx,edi
	and	ebx,0ffc00000h
	or	bl,3			; set status bits
@@dscratchloop:
	mov	ds:[eax],ebx
	add	eax,4
	add	ebx,1000h
	loop	@@dscratchloop
	pop	cx

@@checksrc:
	mov	edx,esi
	add	edx,ecx
	dec	edx				; edx == final byte of source
	mov	eax,esi
	shr	eax,22
	shr	edx,22
	xor	bp,bp

	mov	ebx,fs:[PAGEDIR]
	cmp	eax,edx
	je	@@sentry1

	lea	edx,[ebx+edx*4]	; edx -> source page directory entry
	cmp	DWORD PTR ds:[edx],0
	jne	@@sentry1		; page directory entry already set

; clear previous source page directory entry
	mov	ebp,fs:[ScratchDirS2]
	or	ebp,ebp
	je	@@supdir2
	mov	DWORD PTR ds:[ebp],0

@@supdir2:
	mov	fs:[ScratchDirS2],edx	; update scratch source page directory entry pointer
	mov	ebp,fs:[ScratchSource]
	add	ebp,1000h		; second scratch page table following first
	mov	ds:[edx],ebp	; page dir entry -> scratch 
	or	BYTE PTR ds:[edx],3
	mov	bp,1			; flag that second entry updated

@@sentry1:
	lea	ebx,[ebx+eax*4]	; ebx -> source page directory entry
	cmp	DWORD PTR ds:[ebx],0
	je	@@sclear1		; no previous setting
	or	bp,bp
	je	@@checkdone		; page directory entry already set, second entry not used
	mov	eax,fs:[ScratchSource]	; eax -> start of scratch page tables
	jmp	@@ssetpages		; set page table entries

; clear previous source page directory entry
@@sclear1:
	mov	eax,fs:[ScratchDirS1]
	or	eax,eax
	je	@@supdir1
	mov	DWORD PTR ds:[eax],0

@@supdir1:
	mov	fs:[ScratchDirS1],ebx	; update scratch source page directory entry pointer
	mov	eax,fs:[ScratchSource]
	mov	ds:[ebx],eax	; page dir entry -> scratch 
	or	BYTE PTR ds:[ebx],3

; set up two scratch page tables for identity mapping
@@ssetpages:
	push	cx
	mov	cx,2048			; 2*1K dword entries
	mov	ebx,esi
	and	ebx,0ffc00000h
	or	bl,3			; set status bits
@@sscratchloop:
	mov	ds:[eax],ebx
	add	eax,4
	add	ebx,1000h
	loop	@@sscratchloop
	pop	cx

@@checkdone:
	mov	eax,cr3
	mov	cr3,eax			; ensure TLB is updated if holding old scratch mapping
	pop	ebp
	pop	edx
	pop	ebx
	pop	eax

@@accessok:
	mov dx,es				; get es=ds
	push ds
	pop es

	cld

	shr ecx,1
	REP MOVS DWORD PTR [ESI],DWORD PTR [EDI];
	BIG_NOP;
	adc ecx,ecx
	REP MOVS WORD PTR [ESI],WORD PTR [EDI];
	BIG_NOP;

	mov es,dx

	pop edx

@@ok:
	mov   AH,0                    ; everything OK and finished
	RET

@@abort:
	mov   AH,1                    ; everything OK and finished
	RET


@@invalid_command:
	mov ah,80h
	ret

SIMULATE_INT1587 endp


CALL_TABLE DW OFFSET GET_STATUS
		   DW OFFSET GET_PAGE_FRAME_ADDRESS
		   DW OFFSET GET_UNALLOCATED_PAGE_COUNT
		   DW OFFSET ALLOCATE_PAGES
		   DW OFFSET MAP_HANDLE_PAGE
		   DW OFFSET DEALLOCATE_PAGES
		   DW OFFSET GET_VERSION
		   DW OFFSET SAVE_PAGES
		   DW OFFSET RESTORE_PAGES
		   DW OFFSET NOT_IMPL
		   DW OFFSET NOT_IMPL
		   DW OFFSET GET_OPEN_HANDLES_COUNT
		   DW OFFSET GET_NR_OF_ALLOCATED_PAGES
		   DW OFFSET GET_ALLOCATED_PAGES
		   DW OFFSET SET_GET_PAGE_MAP
		   DW OFFSET NOT_IMPL	; 4fh
		   DW OFFSET ems4_map_multi	; 50h
		   DW OFFSET ems4_realloc	; 51h
		   DW ems4_attribute	; 52h
		   DW ems4_handle_names	; 53h
		   DW ems4_get_handle_info	; 54h
		   DW OFFSET NOT_IMPL	; 55h
		   DW OFFSET NOT_IMPL	; 56h
		   DW ems4_memory_region	; 57h
		   DW ems4_get_mappable_info	; 58h
		   DW ems4_get_config	; 59h
		   DW ems4_allocate_pages	; 5ah
		   DW OFFSET NOT_IMPL	; 5bh
		   DW OFFSET NOT_IMPL	; 5ch
		   DW OFFSET NOT_IMPL	; 5dh


VCPI_CALL_TABLE DW OFFSET VCPI_Presence	; 0
		   DW OFFSET VCPI_GetInterface
		   DW OFFSET VCPI_GetMax	; 2
		   DW OFFSET VCPI_GetFree
		   DW OFFSET VCPI_Allocate4K	; 4
		   DW OFFSET VCPI_Free4K
		   DW OFFSET VCPI_GetAddress	; 6
		   DW OFFSET VCPI_GetCR0
		   DW OFFSET VCPI_ReadDR	; 8
		   DW OFFSET VCPI_WriteDR
		   DW OFFSET VCPI_GetMappings	; 0ah
		   DW OFFSET VCPI_SetMappings
		   DW OFFSET VCPI_V86toPM	; 0ch

;
; AH = 40h: return the actual state of the EMM-driver.
;
GET_STATUS PROC NEAR
        XOR     AH,AH                    ; Allways everything OK.
	RET
GET_STATUS ENDP
;
; AH = 41h: request the segment address of the EMS-window
;
GET_PAGE_FRAME_ADDRESS PROC NEAR
        XOR     AH,AH                    ; No error occurred
        MOV     BX,FS:[_FRAME]           ; Segment address of EMS-Window/Frame
	RET
GET_PAGE_FRAME_ADDRESS ENDP
;
; AH = 42h: Request number of ( also maximum) available EMS-pages
;
GET_UNALLOCATED_PAGE_COUNT PROC NEAR

	call	DynamicEMSPageCount

        MOV     BX,FS:[_PAGESAVAIL]	; Amount of currently available pages
        MOV     DX,FS:[_MAXPAGES]       ; Amount of maximum available pages

; follow MS-DOS EMM386 7.x lead and don't throttle pages on NOEMS
;	cmp	fs:[_FlagNOEMS],0
;	je	unalloc_ret
;	or	bx,bx
;	je	unalloc_ret
;	mov	bx,1			; only show maximum of 1 EMS page if NOEMS set

unalloc_ret:
	XOR     AH,AH
	RET
GET_UNALLOCATED_PAGE_COUNT ENDP
;
; AH = 43h: Reserve Memoryspace of the EMS-add-on-card (ahemm..extinct...)
;
ALLOCATE_PAGES PROC NEAR
	MOV     AH,89H			; "Request, to reserve null pages"
	AND     BX,BX
	JZ      SHORT @@BYE

allocate_pages_plus_zero:
        MOV     AH,87H                  ; "Not enough pages available"
	CMP     BX,FS:[_MAXPAGES]
	JA      SHORT @@BYE

	call	DynamicEMSPageCount	; compute pages available if dynamic

@@nomoreerror:
        MOV     AH,88H                  ; "Not enough pages available anymore"
        CMP     BX,FS:[_PAGESAVAIL]
	JA      SHORT @@BYE

;allocate_pages_plus_zero:
        MOV     ESI,FS:[STATUSTABLE]	; Now search for a free Handle in the table
        MOV     ECX,MAX_HANDLES		;
@@SEARCH:
        CMP     WORD PTR [ESI],-2       ; Is there one free ... ?
	JZ      SHORT @@FOUND
	ADD     ESI,8
	LOOP    @@SEARCH
        MOV     AH,85H                  ; "No more free Handles"
@@BYE:  RET

@@FOUND:
;	PUSH    BX
        MOV     WORD PTR [ESI],-3        ; mark Handle as occupied

; zero page allocations allowed, so test and bypass code if found
	or	bx,bx
	je	allocate_exit
	push	bx
        SUB     FS:[_PAGESAVAIL],BX	 ; Take pages from the pool
	MOV     DX,MAX_HANDLES	         ; Set in CX now the actual Handle-
        SUB     DX,CX			 ; number, so the page
        MOV     EDI,FS:[EMSPAGETABLE]	 ; can be marked in the
        MOVZX   ECX,FS:[_MAXPAGES]	 ; page-usage-table as used
	MOV     AL,FREEPAGE_ID
@@SEARCH_PAGES:
        REPNZ   SCAS BYTE PTR [EDI]	; After searching for a free page

	jnz	@@nofind			; free page could not be located
	call	GetEMSPage			; allocate the page
	jnc	@@assign				; allocation worked
@@nofind:
	pop	bx
	jmp	@@nomoreerror		; couldn't get a page

@@assign:
	MOV     [EDI-1],DL		; Assign the handle
	DEC     BX			; Continue until all desired pages are occupied
	JNZ     SHORT @@SEARCH_PAGES
	POP     BX

allocate_exit:
	XOR     AH,AH
	RET
ALLOCATE_PAGES ENDP
;
; AH = 44h: mirror logical page in the EMS-window
;
MAP_HANDLE_PAGE PROC NEAR
        CMP     AL,4			; not" Only pages 0..3
        JAE     SHORT @@PAGE_TO_LARGE   ; are allowed!

	CALL    TEST_HANDLE		; test remaining Handle
	PUSH    BX			; save BX  (since it is changed)
	AND     BX,BX			; In case the log. page number
	JS      SHORT @@MAP		; negative is, don't search, there
	INC     BX			; fade out
	MOVZX   ECX,FS:[_MAXPAGES]	; the range to search
	PUSH    AX			; AL cannot be disturbed
	MOV     AL,DL			; The handle to search
	MOV     EDI,FS:[EMSPAGETABLE]	; Search the allocation table by the pages
@@LOOP: REPNZ   SCAS BYTE PTR [EDI]	; occupied by the handle
	JNZ     SHORT @@NIX		; Out and past...
	DEC     BX			; The logical page is not yet
	JNZ     SHORT @@LOOP		; reached, so further !
        MOV     BX,FS:[_MAXPAGES]       ; Only calculate the absolute
        SUB     BX,CX                   ; logical pagenumber for MAP_PAGE
	DEC     BX
        POP     AX			; BI "cleans the stack"
@@MAP:  CALL    MAP_PAGE
	POP     BX
	XOR     AH,AH
@@BYE:  RET
@@NIX:  POP     AX			; restore Register again
	POP     BX
        MOV     AH,8AH                  ; "logical page out of
        RET				; reserved area"

;*******************************************************************************
; here comes the funny part (by Tom)
;		during initialization phase, calling EMM_MAP_PAGE (0x44)
;		with pysical page (AL) > 3 is possible.
;		meaning:
;
;		AL = highest 8 bits of logical adress, AL=E4 --> address E4000
;
;		initialization phase is terminated with AL=9F
;*******************************************************************************

; since this is used to map UMB's and that first block of memory is set
;  linear==physical even with pool sharing blocks, we shouldn't
;  need to modify any code in here to make it work with pool sharing
@@PAGE_TO_LARGE:
        MOV     AH,8BH                    ; "Indicated physical page does not exist
        cmp     FS:[_INIT_DONE],0         ; still in init phase ?
	jne     @@BYE

	cmp	al,09fh			; AL=9f finishes init phase
	jne	@@TomsFunnyMapPage

	mov	FS:[_INIT_DONE],1	; finish initialization
	mov	ah,0			; success
	jmp @@BYE

	; the fun part - map in page DX:BX at address AL

@@TomsFunnyMapPage:

        MOV     AH,8BH                    ; "Indicated physical page doesn't exist
	AND     BX,BX                     ; In case the log. page number
	JS      SHORT @@BYE               ; negative is, no search

					  ; code stolen above
			                  ; find the memory for handle/page


	CALL    TEST_HANDLE		  ; test handed over Handle
	PUSH    BX			  ; save BX (since it is changed)

; convert 4K-based logical page to 16K-based
	shr	bx,2

	INC     BX			  ; Fade out
        MOVZX   ECX,FS:[_MAXPAGES]        ; Area to search within
	PUSH    AX			  ; AL may not be disturbed
	MOV     AL,DL                     ; Handle to search for
	MOV     EDI,FS:[EMSPAGETABLE]	  ; Search the allocation table for the page
@@XLOOP: REPNZ   SCAS BYTE PTR [EDI]	  ; corresponding to the handle
        JNZ     SHORT @@NIX		  ; Over and out...
        DEC     BX			  ; The logical page hasn't been
        JNZ     SHORT @@XLOOP		  ; reached yet, so go on !
        MOV     BX,FS:[_MAXPAGES]         ; Now calculate theabsolute logical
        SUB     BX,CX                     ; pagenumber for MAP_PAGE
	DEC     BX
        POP     AX	                  ; BI "cleans the stack"

;****
;**** stolen from MAP_PAGE
;**** with insertions from frameanchor calculation
;****
					; calculation of 'frame anchor'

		push	ax		; save segment frame
        MOVZX   EAX,AL                  ;  00E4 Segmentadresse of the Frame (Fensters)
        shl     EAX,8                   ; usual format,

        SHL     EAX,4                   ; D0000 ... calculate into absolute Adress
	MOV     ESI,FS:[PAGEDIR]
	SHR     EAX,10
	PUSH    EAX
	SHR     EAX,10			; Set pointer in the Page Directory
	AND     EAX,111111111100B
	AND     ESI,NOT 111111111111B	; throw out Statusbits
	MOV     ESI,[ESI+EAX]		; ^ read from Page Table
	POP     EAX
        AND     EAX,111111111100B       ; Same game all over again
	AND     ESI,NOT 111111111111B
	ADD     ESI,EAX

	; MOV     FS:[FRAMEANCHOR],ESI   ; esi = 'frame anchor'


        MOVZX   EDI,BX                  ; Now calculate, from the pagenumber
        SHL     EDI,14			; (absolute) the situation of the EMS-page
        ADD     EDI,FS:[_FIRSTPAGE]      ; in Extended Memory .
        ADD     DI,111B                 ; Statusbits: R/W=1,U/S=1,P=1

;        MOV     CX,4                    ; 1 EMS page 4 (virtual) pages
					; <VDS> support (haha - october 2002)
					; we return the start of physical memory
					; through dx:bx
					;
	pop	ax			; restore segment frame to ax
	POP     BX
	mov	cl,bl
	and	cl,3		; get 4K multiplier offset into 16K page
	movzx	ecx,cl
	shl	ecx,12		; convert multiplier offset to true 4K offset
	add	edi,ecx

; if segment frame value is 0ffh, then we know we're shadowing the ROM into RAM
;  to catch jumps to FFFF:0, so copy ROM image of block to RAM
	cmp	al,0ffh
	jne	noshadow
	pushf
	cli				; no interrupts while BIOS goes away
	push	esi
	push	edi
	and	di,0f000h	; strip off status bits of physical address, enforce 4K alignment
	push	edi
	movzx	esi,al
	shl	esi,12		; convert segment frame to absolute address
	mov	cx,1000h/4	; map 4K block in dwords
shadowloop:
	mov	eax,[esi]
	mov	[edi],eax
	add	edi,4
	add	esi,4
	dec	cx
	jne	shadowloop
	pop	edi
	add	edi,0ff0h
	mov	WORD PTR [edi],09cdh	; force INT 9 (to KBOARD) at FFFF:0
	pop	edi
	pop	esi
	popf

noshadow:
	push	edi

	mov	bx,di
	shr	edi,16
	mov	dx,di

	pop	edi
					; </VDS>


;	MOV     CX,4                    ; 1 EMS entspr. 4 (virtual) pages
;	MOVZX   ECX,CX
@@X2LOOP: MOV     [ESI],EDI             ; put in the new physical address of the
;        ADD     ESI,4                   ; Framepage
;       ADD     EDI,4096                ; Next 4K-page
;	LOOP    @@X2LOOP

        MOV     ESI,FS:[PAGEDIR]        ; To avoid failure through the TLB,
        MOV     CR3,ESI                 ; Load the TLB

	XOR     AH,AH
	RET

;*******************************************************************************
; end of fun :-)
;*******************************************************************************


MAP_HANDLE_PAGE ENDP



;
; AH = 45h: Release reserved memoryspace again
;
DEALLOCATE_PAGES PROC NEAR
        CALL    TEST_HANDLE		; First...
        MOV     AH,86H                  ; "A saved state still
        CMP     WORD PTR [ESI],-3       ; exists" ?
	JNZ     SHORT @@BYE
	PUSH    AX
        MOVZX   ECX,FS:[_MAXPAGES]	; All pages in the allocation table
        MOV     AL,DL                   ; have to be marked again as free (= 0)
        MOV     EDI,FS:[EMSPAGETABLE]   ;
@@LOOP: REPNZ   SCAS BYTE PTR [EDI]     ; after searching pages with Handlenumber,
        JNZ     SHORT @@OK              ;  Everything has been done - ,Alles abgearbeitet ?

	call	ReleaseEMSPage

        MOV     BYTE PTR [EDI-1],FREEPAGE_ID; Mark page as free
	INC     FS:[_PAGESAVAIL]
	JMP     SHORT @@LOOP
@@OK:   MOV     WORD PTR [ESI],-2	; release Handle

; zero handle name on free
	mov	edi,fs:[EMSNAMETABLE]
	movzx	ecx,dx	; handle (index)
	mov	DWORD PTR [edi+ecx*8],0
	mov	DWORD PTR [edi+ecx*8+4],0

	POP     AX
        XOR     AH,AH                   ; Function executed duly
@@BYE:  RET
DEALLOCATE_PAGES ENDP
;
; AH = 46h: determine Version-number of EMM
;
GET_VERSION PROC NEAR
;	MOV     AX,0032H                ; Currently version 3.2
	MOV     AX,0040H                ; Currently version 4.0
	RET
GET_VERSION ENDP
;
; AH = 47h: Save number of the fit-in page in EMS-window
SAVE_PAGES PROC NEAR
	CALL    TEST_HANDLE
        MOV     AH,8DH                 ; "State for Handle already
        CMP     WORD PTR [ESI],-3      ;     saved"
	JNZ     SHORT @@BYE
SAVE_TO_ESI:                           ; Re-entry point from functions  4Exxh
	PUSH    AX                     ; so that AL is saved
	MOV     AX,FS:[PHYSPAGETABLE]  ; the current logical page
	MOV     [ESI],AX               ; place within the action status range
	MOV     AX,FS:[PHYSPAGETABLE+2]
	MOV     [ESI+2],AX
	MOV     AX,FS:[PHYSPAGETABLE+4]
	MOV     [ESI+4],AX
	MOV     AX,FS:[PHYSPAGETABLE+6]
	MOV     [ESI+6],AX
	POP     AX
        XOR     AH,AH                    ; report 'everything OK'
@@BYE:  RET
SAVE_PAGES ENDP
;
; AH = 48h: Restore saved state of the EMS-window
;
RESTORE_PAGES PROC NEAR
	CALL    TEST_HANDLE
        MOV     AH,8EH                    ; "A saved stated does
        CMP     WORD PTR [ESI],-3         ; not exist"
	JZ      SHORT @@BYE
	CALL    RESTORE_FROM_ESI          ; Today ' times indirectly
	MOV     WORD PTR [ESI],-3         ; Nothing stored for Handle
        XOR     AH,AH                     ; report 'everything OK'
@@BYE:  RET

RESTORE_FROM_ESI:                         ; Re-entry for functions 4Exxh
        PUSH    AX		          ; save everything! Data- and System-
        PUSH    BX		          ; registers first!
        XOR     AL,AL                     ; fade in page 0 again
	MOV     BX,[ESI]
    	PUSH    ESI
	CALL    MAP_PAGE
	POP     ESI
	INC     AL			   ; Fade in page 1
	MOV     BX,[ESI+2]
	PUSH    ESI
	CALL    MAP_PAGE
	POP     ESI
	INC     AL			    ; page 2
	MOV     BX,[ESI+4]
	PUSH    ESI
	CALL    MAP_PAGE
	POP     ESI
	INC     AL			    ; page 3
	MOV     BX,[ESI+6]
	PUSH    ESI
	CALL    MAP_PAGE
	POP     ESI
	POP     BX
	POP     AX
        XOR     AH,AH                    ; report OK (because
        RET                              ;  of functions $4E01/$4E02)
RESTORE_PAGES ENDP
;
; report the failure so that we can maybe support it in the future
NOT_IMPL PROC NEAR

IF	UNIMPLEMENTED_EMS_DEBUG	EQ	1
	push	ax
	push	bx
	push	cx
	push	dx
	push	edi

	mov	bx,ax
	mov	edi,0b8000h
	mov	al,bh
	shr	al,4
	call	ni_post
	mov	al,bh
	and	al,0fh
	call	ni_post
	mov	al,bl
	shr	al,4
	call	ni_post
	mov	al,bl
	and	al,0fh
	call	ni_post

	pop	edi
	pop	dx
	pop	cx
	pop	bx
	pop	ax
ENDIF

        MOV     AH,84H                    ; "Invalid function code"
	RET

IF	UNIMPLEMENTED_EMS_DEBUG	EQ	1
ni_post:
	cmp	al,9
	jbe	ni_set
	add	al,7
ni_set:
	add	al,30h
	mov	ds:[edi],al
	add	di,2
	ret
ENDIF

NOT_IMPL ENDP
;
; AH = 4Bh: Number of open Handles
;
GET_OPEN_HANDLES_COUNT PROC NEAR
        MOV     ESI,FS:[STATUSTABLE] ; Search Handle-status-table for
        MOV     CX,MAX_HANDLES           ; assigned/given handles
	XOR     BX,BX
@@LOOP: CMP     WORD PTR [ESI],-2        ; Free ?
	JZ      SHORT @@CLOSED
	INC     BX
@@CLOSED:
        ADD     ESI,8                    ; Next entry.
	LOOP    @@LOOP
	XOR     AH,AH
	RET
GET_OPEN_HANDLES_COUNT ENDP
;
; AH = 4Ch: Determine number of reserved pages for a Handle
;
GET_NR_OF_ALLOCATED_PAGES PROC NEAR
	CALL    TEST_HANDLE
	PUSH    AX
	MOV     AL,DL                    ; After this handle search through the complete
	MOVZX   ECX,FS:[_MAXPAGES]	 ; page-allocation-table
	MOV     EDI,FS:[EMSPAGETABLE]
	XOR     BX,BX
@@LOOP: REPNZ   SCAS BYTE PTR [EDI]	 ; Search ...
        JNZ     SHORT @@OK		 ; No more found, so done
        INC     BX			 ; one page more
	JMP     SHORT @@LOOP
@@OK:   POP     AX
	XOR     AH,AH                    ; Ok.
	RET
GET_NR_OF_ALLOCATED_PAGES ENDP
;
; AH = 4Dh: determine Number of reserved pages for all Handles
;
GET_ALLOCATED_PAGES PROC NEAR
	MOVZX   ESI,WORD PTR [V8086_ES]	    ; ES from the Level-0-Stack
	SHL     ESI,4			    ; ES:DI ^ from the storage area
	MOVZX   EDI,DI
	ADD     ESI,EDI
	PUSH    AX
	PUSH    DX
	MOV     EDI,FS:[STATUSTABLE]
        XOR     AX,AX                    ; actual/current Handle-Number
        XOR     BX,BX                    ; sofar no Handle open
@@NEXT_HANDLE:
	CMP     WORD PTR [EDI],-2        ; Assign handle at all ? If
	JZ      SHORT @@NEXT		 ; not, then jump over
        INC     BX                       ; One more Handle is open...
        MOV     [ESI],AX                 ; Place handle
	INC     ESI
	INC     ESI
	PUSH    EDI
        MOV     EDI,FS:[EMSPAGETABLE]    ; Determine amount of reserved pages
        XOR     DX,DX                    ; for the handle
	MOVZX   ECX,FS:[_MAXPAGES]
@@LOOP: REPNZ   SCAS BYTE PTR [EDI]	 ; Search through Page-Allocation-Table
        JNZ     SHORT @@NOPE             ; No more found
	INC     DX
	JMP     SHORT @@LOOP
@@NOPE: MOV     [ESI],DX                 ; Set the number of determined pages
	INC     ESI
	INC     ESI
	POP     EDI
@@NEXT: ADD     EDI,8                    ; next Handle, please !
	INC     AX
	CMP     AX,MAX_HANDLES           ; All Handles processed ?
	JB      SHORT @@NEXT_HANDLE
	POP     DX
	POP     AX
	XOR     AH,AH                    ; Everything ok.
	RET
GET_ALLOCATED_PAGES ENDP
;
; AH = 4Eh: Get & Set Map
;
CHKSUM  MACRO   REG
	MOV     AX,[REG+2]           ; Calculate checksum
	ADD     AX,[REG+4]
	ADD     AX,[REG+6]
	ADD     AX,[REG+8]
	ENDM

SET_GET_PAGE_MAP PROC NEAR
        CMP     AL,3                    ; Subfunction 0 to 3 ?
	JA      SHORT @@NIX_IS
        JZ      SHORT @@SUBF_3          ; Size of field
	DEC     AL
	JZ      SHORT @@SUBF_1          ; Set Page Map
; Subf. 2: Get & Set Page Map
; Subf. 0: Get Page Map - save Hardwarestatus
@@SUBF_0:
	MOVZX   ESI,WORD PTR [V8086_ES] ; ES:DI ^ convert from statusfield in
	SHL     ESI,4                   ; usual REAL-Mode-Format
	MOVZX   EDI,DI
	ADD     ESI,EDI
        PUSH    AX			; save Subfunctioncode
        ADD     ESI,2                   ; Currently skip checsum
	CALL    SAVE_TO_ESI
	SUB     ESI,2
	CHKSUM  ESI                     ; Calculate checksum and ...
	MOV     [ESI],AX                ; ... store
        POP     AX                      ; restore and examen subfunctioncode
        CMP     AL,2                    ; if subfuction 2
        JZ      SHORT @@SUBF_1          ; is wanted , since then also
	XOR     AH,AH                   ; Subf. 1 still needs be done.
        RET                             ; Everything OK.

; Subf. 1: Set Page Map - restore Hardwarestatus
@@SUBF_1:
	MOVZX   EDI,WORD PTR [V8086_DS]; DS:SI ^ convert from statusfield in
	SHL     EDI,4                    ; usual REAL-Mode-Format
	MOVZX   ESI,SI
	ADD     ESI,EDI
	CHKSUM  ESI                      ; Calculate checksum and check it
	CMP     [ESI],AX
	JNZ     SHORT @@CHKERR
	ADD     ESI,2                    ; Jump over checksum
	JMP     RESTORE_FROM_ESI

; Checksum is incorrect !
@@CHKERR:
	MOV     AH,0A3H                  ; data is destroyed !
	RET

; Subf. 3: Size of the field
@@SUBF_3:
	MOV     AL,4*2+2                 ; 4 entries … 2 Byte + 2 Bytes
	XOR     AH,AH                    ; ChkSum
	RET			         ; That was it then.

bad_subfunc:
@@NIX_IS:
        MOV     AH,8FH                   ; Invalid subfunctioncode !
	RET
SET_GET_PAGE_MAP ENDP

;
; Paste a logical page into EMS-frame.
; AL = physical page 0 - 3, BX = logical page (absolute) or -1
;
MAP_PAGE PROC NEAR
	MOVZX   ESI,AL
	MOV     FS:[PHYSPAGETABLE+ESI*2],BX ; Place faded in page
	SHL     ESI,4			    ; Store the pointe of the entry
	ADD     ESI,FS:[FRAMEANCHOR]	    ; in the page table
	AND     BX,BX			    ; log. page < 0, then fade out
	JS      SHORT @@UNMAP
	MOVZX   EDI,BX			    ; Calculate now the (absolute) pagenumber

	cmp	fs:_IsXMSTableNotFixedEMS,0	; check if pool sharing
	je	@@fixed					; no

	push	esi
	shl	edi,2					; convert absolute page to dword entry offset
	add	edi,fs:[EMSPageAllocationPtrs]	; edi -> EMS page descriptor entry
	movzx	esi,WORD PTR [edi+2]	; pool allocation block count relative 1
	dec	si
	shl	esi,6					; convert to 64-byte block offset
	add	esi,fs:PoolAllocationTable	; esi -> pool allocation block for page
	movzx	ecx,WORD PTR [edi]	; 16K offset from pool block base
	shl	ecx,14				; 16K to bytes
	mov	edi,[esi].psi_addressK
	shl	edi,10				; K address to bytes
	add	edi,ecx				; edi -> page memory
	pop	esi
	jmp	@@SET

@@fixed:
	SHL     EDI,14			    ; from the situation of the EMS-page in Extended
	ADD     EDI,FS:[_FIRSTPAGE]	    ; Memory
@@SET:  ADD     DI,111B			    ; Statusbits: R/W=1,U/S=1,P=1
        MOV     CX,4			    ; 1 EMS entspr. 4 (virtual) pages
	MOVZX   ECX,CX
@@LOOP: MOV     [ESI],EDI                  ; Register the new physical Address of
	ADD     ESI,4                      ; window
        ADD     EDI,4096                   ; Process next 4K-page
	LOOP    @@LOOP
        MOV     ESI,FS:[PAGEDIR]	   ; To avoid error by the TLB,
        MOV     CR3,ESI                    ; , empty the TLB by loading of
        RET                                ; CR3
@@UNMAP:
        MOV     ECX,ESI                    ; save ESI!
        MOVZX   EDI,FS:[_FRAME]            ; During fade out applies "logical =
        SHL     EDI,4			   ; physical address"

	sub	esi,fs:[FRAMEANCHOR]	; adjust esi -> physical offset in frame/K

	SHL     ESI,10
	ADD     EDI,ESI
	MOV     ESI,ECX
	JMP     SHORT @@SET
MAP_PAGE ENDP

;
; AH = 50h: EMS 4.0 map multiple pages
;  remember we must explicitly load cx value from its saved spot in
;  fs:[CX_Save] since EMM API dispatch code uses cx during the dispatch
;
ems4_map_multi PROC NEAR
	cmp	al,1	; only subfunction 0 supported
	ja	bad_subfunc	; this is an invalid subfunction
	je	NOT_IMPL

; perform handle check here so that stack return address isn't blown
;  on invalid handle MAP_HANDLE_PAGE call which directly manipulates it
	movzx	edi,si		; save critical entry register value
	call	TEST_HANDLE

	movzx	esi,WORD PTR [V8086_DS]	; DS from the Level-0-Stack
	shl	esi,4
	add	esi,edi	; esi -> map address buffer
	mov	cx,fs:[CX_Save]	; load EMM entry CX value

	push	bx

multi_loop:
	push	cx
	mov	bx,[esi]
	mov	ax,[esi+2]
	add	esi,4
	push	esi
	call	MAP_HANDLE_PAGE
	pop	esi
	pop	cx
	test	ah,ah
	jne	multi_out	; error occurred
	loop	multi_loop
	xor	ah,ah	; no error return

multi_out:
	pop	bx
	ret
ems4_map_multi ENDP

;
; AH = 51h: EMS 4.0 reallocate pages for handle
;
ems4_realloc PROC NEAR
	call	TEST_HANDLE
	call	DynamicEMSPageCount

	push	bx		; save new pages
	call	GET_NR_OF_ALLOCATED_PAGES	; get current page allocation
	mov	di,bx		; save original page count
	mov	cx,bx
	add	cx,FS:[_PAGESAVAIL]	; get current pages for handle + available
	pop	bx			; restore new page request amount
	cmp	bx,cx
	ja	toomany

	movzx	ecx,FS:[_MAXPAGES]
	push	bx
	cmp	bx,di		; check new page count against original
	jb	shrinkalloc
	je	realloc_success	; no change needed

; growing the allocation of pages
	sub	bx,di		; get count of new pages needed
	mov	edi,FS:[EMSPAGETABLE]
	mov	al,FREEPAGE_ID
growpages:
	repnz	scas BYTE PTR [edi]

	jnz	nofind					; couldn't find new page
	call	GetEMSPage			; allocate the page
	jnc	@@assign				; allocation successful
nofind:
	pop	bx
	jmp	toomany					; couldn't get a page

@@assign:
	mov	[edi-1],dl
	dec	FS:[_PAGESAVAIL]
	dec	bx
	jnz	growpages
	jmp	realloc_success

; trim off the top pages in case something stupid depends on page order
shrinkalloc:
	xchg	di,bx
	sub	bx,di		; get count of pages to reduce
	mov	edi,FS:[EMSPAGETABLE]
	add	edi,ecx
	dec	edi			
	mov	al,dl
	std				; scan backwards
shrinkpages:
	repnz	scas BYTE PTR [edi]

	add	edi,2		; ReleaseEMSPage expects edi-1 ptr, not edi+1
	call	ReleaseEMSPage
	sub	edi,2

	mov	BYTE PTR [edi+1],FREEPAGE_ID
	inc	FS:[_PAGESAVAIL]
	dec	bx
	jnz	shrinkpages

	cld				; restore normalcy

realloc_success:
	pop	bx
	xor	ah,ah		; no error occurred
	ret

toomany:
	mov	ah,88h
	mov	bx,di		; return original pages owned by handle
	ret
ems4_realloc ENDP

;
; AH = 52h: EMS 4.0 attribute related
;
ems4_attribute PROC NEAR
	cmp	al,2
	jb	@@get_set_attribute
	ja	bad_subfunc	; this is an invalid subfunction

@@is_volatile:
	xor	ax,ax	; al == 0, volatile attribute only, ah == successful call
	ret

@@get_set_attribute:
	mov	cl,al
	call TEST_HANDLE	; only valid handles please
	or cl,cl		; 0 is get, 1 is set
	jz @@is_volatile	; only volatile here (none survive warm reboot)
	or bl,bl		; 0 is "make volatile" (true anyway)
	jnz @@cannot_make_nonvolatile
	mov ah,0		; be happy
	ret

@@cannot_make_nonvolatile:
	mov ah,91h		; feature not supported
	ret
ems4_attribute ENDP

;
; AH = 53h: EMS 4.0 get/set handle name
;
ems4_handle_names PROC NEAR
	cmp	al,1
	ja	bad_subfunc	; this is an invalid subfunction
	mov	cl,al
	push	si
	push	di
	pop	edi				; edi high word holds original si

; perform handle check here so that stack return address isn't blown
;  on invalid handle MAP_HANDLE_PAGE call which directly manipulates it
	call	TEST_HANDLE

	or	cl,cl
	jne	ems4_setname

	movzx	esi,WORD PTR [V8086_ES]	; ES from the Level-0-Stack
	shl	esi,4
	movzx	edi,di	; clear out original si save in high word
	add	edi,esi		; edi -> handle name buffer address (dest)
	mov	esi,fs:[EMSNAMETABLE]

ems4_doname:
	movzx	ecx,dx	; handle (index)
	mov	eax,[ecx*8+esi]	; transfer handle name to es:di
	mov	[edi],eax
	mov	eax,[ecx*8+esi+4]
	mov	[edi+4],eax

	xor	ah,ah	; no error return
	ret

ems4_setname:
	movzx	esi,WORD PTR [V8086_DS]	; DS from the Level-0-Stack
	shl	esi,4
	shr	edi,16	; original si into [e]di
	add	esi,edi	; esi -> handle name (source)
	mov	edi,fs:[EMSNAMETABLE]
	movzx	ecx,dx	; handle (index)
	mov	eax,[esi]	; transfer handle name to es:di
	mov	[ecx*8+edi],eax
	mov	eax,[esi+4]
	mov	[ecx*8+edi+4],eax

	xor	ah,ah	; no error return
	ret

ems4_handle_names ENDP

;
; AH = 54h: EMS 4.0 get various handle info
;
ems4_get_handle_info PROC NEAR
	or	al,al
	je	getallhand
	cmp	al,1
	je	@@find_handle_by_name
	cmp	al,2
	ja	bad_subfunc	; this is an invalid subfunction

	mov	bx,MAX_HANDLES
	mov	ah,0	; zero ah, no error return
	ret

; write handle directory to caller buffer
getallhand:
	movzx	esi,WORD PTR [V8086_ES]	; ES from the Level-0-Stack
	shl	esi,4
	movzx	edi,di
	add	esi,edi
	mov	edi,FS:[STATUSTABLE]
	xor	ax,ax		; AL will be count of open handles
	mov	cx,MAX_HANDLES
@@scan_handles:
	cmp	WORD PTR [edi],-2
	jz	@@free_handle
	inc	ax		; count that open handle
	push	eax

;	mov	ax,[edi]	; handle number
	mov	eax,edi
	sub	eax,FS:[STATUSTABLE]
	shr	eax,3

	mov	[esi],ax
	movzx	eax,ax
	shl	eax,3		; convert to table offset
	add	eax,fs:[EMSNAMETABLE]
	push	DWORD PTR [eax]	; copy handle name
	pop	DWORD PTR [esi+2]
	push	DWORD PTR [eax+4]
	pop	DWORD PTR [esi+6]
	add	esi,10
	pop	eax
@@free_handle:
	add	edi,8
	loop	@@scan_handles
	mov	ah,0
	ret

@@find_handle_by_name:
	movzx	edi,WORD PTR [V8086_DS]	; DS from the Level-0-Stack
	shl	edi,4
	movzx	esi,si
	add	edi,esi
	mov	eax,[edi]		; fetch to-be-searched name
	mov	ecx,[edi+4]		; (8 byte binary string)
	or	eax,eax
	jnz	@@valid_search_term
;	or	ecx,ecx
;	jz	@@invalid_search_term
@@valid_search_term:
	xor	edx,edx			; handle number
;	mov	ecx,fs:[EMSNAMETABLE]
	mov	edi,fs:[EMSNAMETABLE]
	mov	esi,fs:[STATUSTABLE]
@@scan_for_name:
	cmp	WORD PTR [esi],-2	; closed, auto-fail
	jz	@@another_handle
	cmp	[edi+edx*8],eax
	jnz	@@another_handle
	cmp	[edi+edx*8+4],ecx	; Note that open handles do not have
	jz	@@found_handle		; to have a name.
@@another_handle:
	add	esi,8
	inc	dx					; never exceeds 16-bit value (8-bit actually)
	cmp	dx,MAX_HANDLES
	jb	@@scan_for_name
	mov	ah,0a0h			; "no handle could be found"
	ret

@@found_handle:
	mov	ah,0
@@bye:	ret

@@invalid_search_term:			; The error code descr. is misleading:
	mov	ah,0a1h			; "handle found had no name"
 	ret
ems4_get_handle_info ENDP

;
; AH = 57h: EMS 4.0 move/exchange memory region
;
ems4_memory_region PROC NEAR
	movzx	edi,WORD PTR [V8086_DS]	; DS from the Level-0-Stack
	cmp	al,1
	ja	bad_subfunc	; this is an invalid subfunction
	sete	fs:[Xchg_Flag]

; save original EMS page mapping of first two physical pages
	mov	eax,DWORD PTR fs:[PHYSPAGETABLE]
	mov	DWORD PTR fs:[PageMapSave],eax

	push	ebx
	push	edx

	xor	dx,dx		; dh holds counter, dl will bitcode flag src/dest conv/ems
	shl	edi,4
	movzx	esi,si
	add	edi,esi		; edi -> EMS region buffer address
	mov	ecx,[edi]	; get move count
	test	ecx,ecx
	je	mr_good		; always succeeds if no bytes are moved
	mov	ah,96h		; preload error code, region length exceeds 1M
	cmp	ecx,65536*16
	ja	mr_out
	mov	fs:[RegionCount],ecx

; process region destination information
	movzx	ecx,WORD PTR [edi+14]	; offset
	mov	al,[edi+11]	; destination memory type, 0 conv, 1 (nonzero) expanded
	or	al,al
	movzx	ebx,WORD PTR [edi+16]	; keep flag status, preload destination segment if conv
	je	mr_calc_dest

; EMS destination
	mov	dl,2		; bitcode flag EMS destination
	mov	ah,95h		; preload error code, specified offset is outside logical page
	cmp	ecx,4000h-1	; can't have a page offset >16K-1
	ja	mr_out

	add	ecx,4000h	; page 1 is offset 16K from base
	movzx	ebx,fs:[_FRAME]

mr_calc_dest:
	shl	ebx,4		; convert seg to memory offset
	add	ebx,ecx		; ebx -> destination address
	mov	fs:[RegionDest],ebx

; process region source information
	movzx	ecx,WORD PTR [edi+7]	; offset
	mov	al,[edi+4]	; source memory type, 0 conv, 1 (nonzero) expanded
	or	al,al
	movzx	esi,WORD PTR [edi+9]	; keep flag status, preload source segment if conv
	je	mr_calc_src

; EMS source
	or	dl,1		; bitcode flag EMS source
	mov	ah,95h		; preload error code to specified offset is outside logical page
	cmp	ecx,4000h-1	; can't have a page offset >16K-1
	ja	mr_out

	movzx	esi,fs:[_FRAME]

mr_calc_src:
	shl	esi,4		; convert seg to memory offset
	add	esi,ecx		; esi -> source address
	mov	fs:[RegionSource],esi

mr_proc_loop:
	mov	ecx,fs:[RegionCount]
	or	dl,dl		; see if anything is EMS
	je	mr_trans_setup	; nope, bypass EMS calcs
	test	dl,1	; see if source EMS
	je	mr_test_dest	; no

; source boundary check
	movzx	eax,WORD PTR fs:[RegionSource]
	and	ax,3fffh
	mov	ebx,4000h
	sub	ebx,eax		; get bytes available to move/xfer on this page
	cmp	ecx,ebx
	jbe	mr_test_dest
	mov	ecx,ebx		; can't go past page boundary

mr_test_dest:
	test	dl,2	; see if destination EMS
	je	mr_map

; destination boundary check
	movzx	eax,WORD PTR fs:[RegionDest]
	and	ax,3fffh
	mov	ebx,4000h
	sub	ebx,eax		; get bytes available to move/xfer on this page
	cmp	ecx,ebx
	jbe	mr_map
	mov	ecx,ebx		; can't go past page boundary

; now map in the EMS pages, if necessary
mr_map:
	push	ecx
	push	edx
	push	edi
	test	dl,1	; see if source page mapping
	je	mr_map_dest
	movzx	ax,dh	; adjust for pages already transferred
	mov	bx,[edi+9]
	add	bx,ax
	mov	dx,[edi+5]
	mov	al,0
	push	OFFSET bad_map	; offset for code branch if TEST_HANDLE fails
	call	MAP_HANDLE_PAGE
	pop	ax			; remove error code branch offset from stack
	pop	edi			; MAP_HANDLE_PAGE modifies critical edi and edx, restore
	pop	edx
	push	edx
	push	edi

mr_map_dest:
	test	dl,2	; see if destination page mapping
	je	mr_ems_restore
	movzx	ax,dh	; adjust for pages already transferred
	mov	bx,[edi+16]
	add	bx,ax
	mov	dx,[edi+12]
	mov	al,1
	push	OFFSET bad_map	; offset for code branch if TEST_HANDLE fails
	call	MAP_HANDLE_PAGE
	pop	ax			; remove error code branch offset from stack

mr_ems_restore:
	pop	edi
	pop	edx
	pop	ecx			; restore count of bytes to transfer this block
	inc	dh			; increase count of pages transferred

; at this point:
;  dh == page count transferred, if EMS used (+1 from current status)
;  dl == bitcoded flags for source/dest conv/EMS
;  ecx == byte count to transfer
;  edi -> entry buffer
;  RegionSource == address of transfer source
;  RegionDest == address of transfer destination
mr_trans_setup:
	push	ecx
	mov	ebx,edi		; keep pointer to entry buffer
	mov	esi,fs:[RegionSource]
	mov	edi,fs:[RegionDest]
	cmp	fs:[Xchg_Flag],0
	jne	mr_xchg

; use original EMM386 move code instruction block
	MOV     EAX,ECX
	AND     ECX,3
	REP     MOVS BYTE PTR [ESI],BYTE PTR [EDI]
	BIG_NOP
	MOV     ECX,EAX
	SHR     ECX,2
	REP     MOVS DWORD PTR [ESI], DWORD PTR [EDI]
	BIG_NOP

; this block transfer is done, ready for next, if any
mr_trans_done:
	pop	ecx
	mov	eax,fs:[RegionCount]
	sub	eax,ecx
	jz	mr_good		; done
	mov	fs:[RegionCount],eax
	mov	ecx,eax
	cmp	ecx,4000h
	jbe	mr_trans2
	mov	ecx,4000h

mr_trans2:
	mov	fs:[RegionSource],esi	; update source address
	test	dl,1	; see if source EMS
	je	mr_trans3

; subsequent transfer from EMS page base
	movzx	eax,fs:[_FRAME]
	shl	eax,4
	mov	fs:[RegionSource],eax

mr_trans3:
	mov	fs:[RegionDest],edi	; update destination address
	mov	edi,ebx		; restore edi -> entry buffer
	test	dl,2	; see if destination EMS
	je	mr_map

; subsequent transfers to EMS page base+1 page
	movzx	eax,fs:[_FRAME]
	shl	eax,4
	add	eax,4000h
	mov	fs:[RegionDest],eax
	jmp	mr_map

mr_good:
	mov	ah,0	; zero ah, no error return

; restore original page mapping of first two pages and exit
mr_out:
	push	ax
	xor	ax,ax
	mov	bx,fs:[PageMapSave]
	call	MAP_PAGE
	inc	ax
	mov	bx,fs:[PageMapSave+2]
	call	MAP_PAGE
	pop	ax

	pop	edx
	pop	ebx
	ret

mr_xchg:
	push	edx
	mov	edx,ecx
	and	ecx,3
	je	mr_fullx
mr_xsmall:
	mov	al,[esi]
	xchg	al,[edi]
	mov	[esi],al
	inc	edi
	inc	esi
	loop	mr_xsmall
mr_fullx:
	mov	ecx,edx
	shr	ecx,2
mr_xbig:
	mov	eax,[esi]
	xchg	eax,[edi]
	mov	[esi],eax
	add	esi,4
	add	edi,4
	loop	mr_xbig
	pop	edx
	jmp	mr_trans_done

; clear all interim saved registers on stack, info won't be used
bad_map:
	pop	edx
	pop	edx
	pop	edx
	jmp	mr_out

ems4_memory_region ENDP

;
; AH = 58h: EMS 4.0 get addresses of mappable pages, number of mappable pages
;
ems4_get_mappable_info PROC NEAR
	cmp	al,1
	ja	bad_subfunc	; this is an invalid subfunction
	je	nummap

	movzx	esi,WORD PTR [V8086_ES]	; ES from the Level-0-Stack
	shl	esi,4
	movzx	edi,di
	add	edi,esi	; edi -> handle name buffer address
	mov	si,fs:[_FRAME]
	xor	ax,ax
	mov	cx,4

mapinfo_loop:
	mov	[edi],si	; base address
	mov	[edi+2],ax	; physical page
	add	si,400h		; next base address (16384/16 paragraphs) 
	add	edi,4		; next dword entry
	inc	ax			; next physical page
	loop	mapinfo_loop

nummap:
;	mov	cx,4	; four mappable pages
	mov	WORD PTR ss:[esp+2],4	; have to save cx value to stack storage
	xor	ah,ah	; no error return
	ret

ems4_get_mappable_info ENDP

;
; AH = 59h: EMS 4.0 get hardware config/get number of raw pages
;
ems4_get_config PROC NEAR
	cmp	al,1	; only subfunction 1 supported
	ja	bad_subfunc
	jne	NOT_IMPL

	jmp	GET_UNALLOCATED_PAGE_COUNT

ems4_get_config ENDP

;
; AH = 5ah: EMS 4.0 allocate handle and standard/raw pages
;
ems4_allocate_pages PROC NEAR
	cmp	al,1	; subfunction must be 0 or 1, we don't care if either
	ja	bad_subfunc

	jmp	allocate_pages_plus_zero

ems4_allocate_pages ENDP

;
; AX=DE00: VCPI presence detection
;  return BH = 1 (major version), BL = 0 (minor version)
;
VCPI_Presence	PROC	NEAR
	mov	bx,100h
	xor	ah,ah
	ret
VCPI_Presence	ENDP

;
; AX=DE01: VCPI get protected mode interface
;  entry es:di -> client zero page table (to fill)
;   ds:si -> three descriptor table entries in client's GDT (to fill)
;  return di -> first uninitialized page table entry (advanced by 4K)
;   ebx == offset to server's protect mode code segment
;
VCPI_GetInterface	PROC	NEAR
	movzx	ebx,WORD PTR [V8086_DS]	; DS from the Level-0-Stack
	shl	ebx,4
	movzx	esi,si
	add	esi,ebx			; esi -> client GDT entries
	mov	ebx,OFFSET server_GDT_Code

	push	eax
	mov	eax,fs:[ebx]
	mov	[esi],eax
	mov	eax,fs:[ebx+4]
	mov	[esi+4],eax
	mov	ebx,OFFSET server_GDT_Universe
	mov	eax,fs:[ebx]
	mov	[esi+8],eax
	mov	eax,fs:[ebx+4]
	mov	[esi+12],eax
	mov	ebx,OFFSET server_GDT_Data
	mov	eax,fs:[ebx]
	mov	[esi+16],eax
	mov	eax,fs:[ebx+4]
	mov	[esi+20],eax
	pop	eax

	movzx	esi,WORD PTR [V8086_ES]	; ES from the Level-0-Stack
	shl	esi,4
	movzx	edi,di
	add	edi,esi			; edi -> client zero page table
	mov	esi,fs:[PAGEDIR]
	mov	esi,[esi]		; edx -> page table for first 1M
	and	esi,0fffff000h	; mask to page frame address
;	mov	ecx,1000h/4
	mov	ecx,700h/4		; only map first 1.75M so debugger can stuff in
						; its own PTE's (only ~1.3M needed anyway)
	push	eax

vgiloop:
	mov	eax,[esi]
	and	ax,0F1FFh		; clear bits 9-11
	mov	es:[edi],eax
	add	esi,4
	add	edi,4
	loop	vgiloop
	pop	eax

	REP     MOVS DWORD PTR [ESI], DWORD PTR [EDI]
	BIG_NOP

;	add	DWORD PTR [esp+6],1000h	; advance edi value stored on stack
	add	DWORD PTR [esp+6],700h	; advance edi value stored on stack

	mov	ebx,OFFSET PM_ENTRY
	xor	ah,ah
	ret

VCPI_GetInterface	ENDP

; AX=DE02: VCPI get maximum physical memory address
;  return edx == physical address of highest 4K page available
;
VCPI_GetMax	PROC	NEAR
	mov	edx,fs:[_TOTAL_MEMORY]
	dec	edx
	and	edx,NOT 0fffh

	xor	ah,ah			; flag success
	ret
VCPI_GetMax	ENDP

;
; AX=DE03: VCPI get number of free pages
;  return edx == number of free pages
;
VCPI_GetFree	PROC	NEAR
	push	eax
	call	GetFreeXMS4KPageCount
	mov	ecx,edx		; ecx == XMS free 4K pages

	call	GetCurrent4KPageCount	; current free in eax, current total in edx
	add	ecx,eax		; ecx == all free pages
	sub	edx,eax		; edx == current allocated (total - free)
	jc	@@nofree			; this shouldn't happen, but return no free it if does
	xchg	edx,ecx		; free pages count in edx, allocated count in ecx

; total free must be <= MAXMEM16K * 4 - current allocated
;  otherwise set total free = MAXMEM16K * 4 - current allocated
	mov	eax,fs:_MAXMEM16K
	shl	eax,2			; convert to maximum 4K blocks
	sub	eax,ecx
	jc	@@nofree
	cmp	eax,edx
	jae	@@done
	mov	edx,eax

@@done:
	pop	eax
	xor	ah,ah			; flag success
	ret

@@nofree:
	xor	edx,edx
	jmp	@@done

VCPI_GetFree	ENDP

;
; AX=DE04: VCPI allocate a 4K page
;  return edx == physical address of 4K page allocated
;
VCPI_Allocate4K	PROC	NEAR
	push	eax		; save high word of eax

	call	GetCurrent4KPageCount	; current free in eax, current total in edx

; fail if current total - current free (allocated) >= MAXMEM16K * 4
	sub	edx,eax		; edx == current allocated (total - free)
	jc	@@fail			; shouldn't happen, but fail if it does
	mov	eax,fs:_MAXMEM16K
	shl	eax,2			; convert to maximum 4K blocks
	cmp	eax,edx
	jbe	@@fail

	call	Allocate4KPageFromPoolBlock
	or	edx,edx
	jne	@@success

	call	ExpandAnyPoolBlock
	or	edx,edx			; see if any pool block has 4K free
	je	@@tryxms

; pool block was expanded, so an allocation should succeed
	call	Allocate4KPageFromPoolBlock
	or	edx,edx
	jne	@@success

; shouldn't reach this point since pool expansion complete,
;  fall through to trying XMS
@@bad1:

@@tryxms:
	call	AllocateXMSForPool
	jc	@@fail
	call	Allocate4KPageFromPoolBlock	; this should always succeed
	or	edx,edx
	je	@@bad2			; failed due to internal fault, so fail allocation

@@success:
	xor	cl,cl			; flag no errors

@@allocret:
	pop	eax
	mov	ah,cl
	ret

@@bad2:

@@fail:
	xor	edx,edx
	mov	cl,88h
	jmp	@@allocret

VCPI_Allocate4K	ENDP

;
; AX=DE05: VCPI free a 4K page
;  entry edx == physical address of 4K page to free
;
VCPI_Free4K	PROC	NEAR
	call Free4KPage
	jc	@@bad
	xor	ah,ah			; flag success
	ret

@@bad:
	mov	ah,8ah
	ret

VCPI_Free4K	ENDP

;
; AX=DE06: VCPI get physical address of 4K page in first megabyte
;  entry CX_Save = page number (original cx)
;  return edx == physical address of 4K page
;
VCPI_GetAddress	PROC	NEAR
	movzx	ecx,fs:[CX_Save]
	cmp	cx,256
	jae	vga_bad			; page outside of first megabyte

	mov	edx,fs:[PAGEDIR]
	mov	edx,[edx]		; edx -> page table for first 1M
	and	edx,0fffff000h	; mask to page frame address
	mov	edx,[edx+ecx*4]	; edx == page table entry for page number
	and	edx,0fffff000h	; mask to page frame address
	xor	ah,ah			; flag success
	ret

vga_bad:
	xor	edx,edx
	mov	ah,8bh
	ret

VCPI_GetAddress	ENDP

;
; AX=DE07: VCPI read CR0
;  return EBX == CR0
;
VCPI_GetCR0	PROC	NEAR
	mov	ebx,cr0
	xor	ah,ah			; flag success
	ret
VCPI_GetCR0	ENDP
 
;
; AX=DE08: VCPI read debug registers
;  call with ES:DI buffer pointer. Returns with buffer filled.
;  (8 dwords, dr0 first, dr4/5 undefined)
;
VCPI_ReadDR	PROC	NEAR
	movzx	esi,WORD PTR [V8086_ES]	; ES from the Level-0-Stack
	shl	esi,4
	movzx	edi,di
	add	esi,edi
	mov	edi,dr0
	mov	[esi],edi
	mov	edi,dr1
	mov	[esi+4],edi
	mov	edi,dr2
	mov	[esi+8],edi
	mov	edi,dr3
	mov	[esi+12],edi
;	mov	edi,dr4
;	mov	[esi+16],edi
;	mov	edi,dr5
;	mov	[esi+20],edi
	mov	edi,dr6
	mov	[esi+24],edi
	mov	edi,dr7
	mov	[esi+28],edi
	xor	ah,ah			; flag success
	ret
VCPI_ReadDR	ENDP

;
; AX=DE09: VCPI write debug registers
;  call with ES:DI buffer pointer. Updates debug registers.
;  (8 dwords, dr0 first, dr4/5 ignored)
;
VCPI_WriteDR	PROC	NEAR
	movzx	esi,WORD PTR [V8086_ES]	; ES from the Level-0-Stack
	shl	esi,4
	movzx	edi,di
	add	esi,edi
	mov	edi,[esi]
	mov	dr0,edi
	mov	edi,[esi+4]
	mov	dr1,edi
	mov	edi,[esi+8]
	mov	dr2,edi
	mov	edi,[esi+12]
	mov	dr3,edi
;	mov	edi,[esi+16]
;	mov	dr4,edi
;	mov	edi,[esi+20]
;	mov	dr5,edi
	mov	edi,[esi+24]
	mov	dr6,edi
	mov	edi,[esi+28]
	mov	dr7,edi
	xor	ah,ah			; flag success
	ret
VCPI_WriteDR	ENDP

;
; AX=DE0A: VCPI get 8259A interrupt vector mappings
;  return bx == 1st vector mapping for master 8259A (IRQ0-IRQ7)
;    cx == 1st vector mapping for slave 8259A (IRQ8-IRQ15)
;
VCPI_GetMappings	PROC	NEAR
	mov	bx,8			; use default
	mov	cx,70h
	mov	[esp+2],cx		; have to save cx value to stack storage
	xor	ah,ah			; flag success
	ret
VCPI_GetMappings	ENDP

; *** NOT IMPLEMENTED
; AX=DE0B: VCPI set 8259A interrupt vector mappings
;  entry bx == 1st vector mapping for master 8259A (IRQ0-IRQ7)
;    cx == 1st vector mapping for slave 8259A (IRQ8-IRQ15)
;
VCPI_SetMappings	PROC	NEAR
;	cli					; ensure interrupts disabled

	jmp	NOT_IMPL

VCPI_SetMappings	ENDP

; V86-entry only
; AX=DE0C: VCPI switch from V86 mode to protected mode
;  entry esi -> entry linear address of data structure
;  exit GDTR, IDTR, LDTR, TR loaded for client
;    transfer control to client
;    modify only eax, esi, DS, ES, FS, GS
;
VCPI_V86toPM	PROC	NEAR
	cli					; ensure interrupts disabled
	add	esp,2			; eat dispatch return address
	pop	ecx				; restore stack saved ecx and edi value
	pop	edi
	add	esp,4			; clear esi value off of stack

;	mov	eax,cr3

; actually, reloading cr3 with same value doesn't make any difference
;	cmp	eax,[esi].SW_CR3
;	je	vvp2			; avoid reloading cr3 if same since it hurts performance

	mov	eax,[esi].SW_CR3	; load cr3 to switch to client's linear address space
;	and	ax,0f018h		; clear out all reserved bits
	mov	cr3,eax

vvp2:
	mov	eax,[esi].SW_GDTOFFS	; set up client's GDT
	DB	66h				; make 32-bit 6-byte load
	lgdt	[eax]
	mov	eax,[esi].SW_IDTOFFS	; set up client's IDT
	DB	66h					; make 32-bit 6-byte load
	lidt	[eax]
	lldt	[esi].SW_LDTR		; set up client's LDT
	mov	eax,[esi].SW_GDTOFFS
	mov	eax,[eax+2]		; EAX == linear base address of client's GDT
	push	ebx
	mov	bx,[esi].SW_TR
	and	ebx,0fff8h
	add	eax,ebx
	pop	ebx
	and	BYTE PTR [eax+5],NOT 2	; clear task busy bit in TSS descriptor
	ltr	[esi].SW_TR				; set up client's TSS
	jmp	PWORD PTR [esi].SW_EIP	; jump to client's entry point

VCPI_V86toPM	ENDP

; protected mode-entry only
; AX=DE0C: VCPI switch from protected mode to V86 mode
;  entry SS:ESP set for IRETD to V86+PM far call return address to discard
;   ds -> linear address space from 0de01h call
;  exit GDTR, IDTR, LDTR, TR loaded for server,
;    SS:ESP and all segment registers loaded with values on stack
;    transfer control to client
;    modify only eax
;
VCPI_PMtoV86	PROC	NEAR
	cli					; client should have disabled interrupts, but not all do

	add	esp,8			; eat far call return address
	mov	DWORD PTR [esp+8],23002h	; set EFLAGS for VM and IOPL=3

	mov	ax,cs
	add	ax,16			; V86_DATA_SEL
	mov	fs,ax
ASSUME fs:MONDATA

	mov	eax,fs:[PAGEDIR]
	mov	cr3,eax			; flush TLB

; set up server system registers
	mov	eax,OFFSET IDT_PTR
	DB	66h					; make 32-bit 6-byte load
	lidt	fs:[eax]
	mov	eax,OFFSET GDT_PTR
	DB	66h					; make 32-bit 6-byte load
	lgdt	fs:[eax]
	mov	ax,V86_LDT_SEL
	lldt	ax

	mov	eax,OFFSET GDT_PTR
	mov	eax,fs:[eax+2]		; EAX == linear base address of server's GDT
	push	ebx
	mov	bx,V86_TSS_SEL
	and	ebx,0fff8h
	add	eax,ebx
	pop	ebx
	and	BYTE PTR [eax+5],NOT 2	; clear task busy bit in TSS descriptor
	mov	ax,V86_TSS_SEL
	ltr	ax

	mov	eax,cr0
	and	al,NOT 8			; ensure TS bit not set
	mov	cr0,eax
	pushfd
	pop	eax
	and	ah,NOT 40h			; ensure NT bit not set
	push	eax
	popfd

	iretd

VCPI_PMtoV86	ENDP

IF	VCPI_DEBUG	EQ	1

VCPI_Debugging	PROC	NEAR
	call	GetFreeXMS4KPageCount
	mov	ebp,edx		; ebp == XMS free 4K pages
	call	GetCurrent4KPageCount	; current free in eax, current total in edx
	mov	ebx,eax
	ret
VCPI_Debugging	ENDP

ENDIF

;
; Check a given Handle for validness.
; In case the Handle is invalid the returnaddress is thrown away and afterwards through
; RET returned to dispatcher.

TEST_HANDLE PROC NEAR
        MOV     AH,83H                    ; "Handed over Handle is unknown"
        CMP     DX,MAX_HANDLES            ; Out of area ?
	JAE     SHORT @@BYE
        MOVZX   ESI,DL                    ; Form the pointer from the Handle-Status-
        SHL     ESI,3                     ; Table and ...
	ADD     ESI,FS:[STATUSTABLE]
        CMP     WORD PTR [ESI],-2         ; ... examine, if the Handle has
        JZ      SHORT @@BYE               ; been marked as free
	RET
@@BYE:  POP     SI                        ; Throw away call(ing)-address
		RET
TEST_HANDLE ENDP


; EMS/VCPI memory management routines

; dynamically compute EMS pages, store to _PAGESAVAIL
;  if fixed allocation, check only current EMS store
; destroy no registers
DynamicEMSPageCount	PROC
	push	eax
	push	ecx
	push	edx
	push	edi

	xor	ecx,ecx			; init count of free 16K XMS pool blocks
	cmp	fs:_IsXMSTableNotFixedEMS,0	; check if pool sharing
	je	@@pastxms

	call	GetFreeXMS4KPageCount
	shr	edx,2			; convert 4K count to 16K pages
	mov	ecx,edx

; current EMS page count in dx, available in eax, VCPI/EMS used in edi
@@pastxms:
	call	GetCurrentEMSPageCount
	cmp	fs:_IsXMSTableNotFixedEMS,0	; check if pool sharing
	je	@@ret
	add	eax,ecx			; eax == potential pages available

; total free must be <= MAXMEM16K - current VCPI/EMS allocated/used
;  otherwise set total free = MAXMEM16K - allocated
	mov	ecx,fs:_MAXMEM16K
	sub	ecx,edi			; amount that can still be allocated (total - used)
	jc	@@nofree
	cmp	eax,ecx
	jbe	@@pagemax		; available less than amount allowed for allocation
	mov	eax,ecx

; limit possible EMS pages to maximum allowed
@@pagemax:
	cmp	eax,10000h
	jae	@@adj1
	cmp	ax,fs:[_MAXPAGES]
	jb	@@noadj1
@@adj1:
	mov	ax,fs:[_MAXPAGES]
@@noadj1:
	cmp	ax,dx			; ensure no overflow from improper current value
	ja	@@noadj2
	mov	ax,dx
@@noadj2:
	sub	ax,dx			; max - used == max available

@@ret:
	mov	fs:[_PAGESAVAIL],ax
	pop	edi
	pop	edx
	pop	ecx
	pop	eax
	ret

@@nofree:
	xor	ax,ax
	jmp	@@ret

DynamicEMSPageCount	ENDP


; allocate an EMS page
; upon entry [edi-1] -> EMS page table entry for page
; return carry if fail, due to internal failure
; destroy no registers
GetEMSPage	PROC
	push	eax
	push	ebx
	push	edx

	mov	ebx,edi
	dec	ebx
	sub	ebx,FS:[EMSPAGETABLE]	; absolute offset in table
	shl	ebx,2				; dword/entry
	add	ebx,fs:[EMSPageAllocationPtrs]	; ebx -> EMS page descriptor entry

	cmp	ebx,fs:[EMSPageAllocationEnd]
	jae	@@bad2			; out of range

	call	Allocate16KPageFromPoolBlock
	or	edx,edx
	jne	@@success

	call	ExpandAnyPoolBlock
	or	edx,edx			; see if any pool block has 16K free
	je	@@tryxms

; pool block was expanded, so an allocation should succeed
	call	Allocate16KPageFromPoolBlock
	or	edx,edx
	jne	@@success

; shouldn't reach this point since pool expansion complete,
;  fall through to trying XMS
@@bad1:

@@tryxms:
	call	AllocateXMSForPool
	jc	@@fail
	call	Allocate16KPageFromPoolBlock	; this should always succeed
	or	edx,edx
	je	@@bad2			; failed due to internal fault, so fail allocation

@@success:
	clc					; flag no errors

@@allocret:
	pop	edx
	pop	ebx
	pop	eax
@@ret:
	ret

@@bad2:

@@fail:
	stc
	jmp	@@allocret

GetEMSPage	ENDP


; release EMS page, [edi-1] -> EMS page table entry for page
; destroy no registers
ReleaseEMSPage	PROC
	push	edx
	mov	edx,edi
	dec	edx
	sub	edx,FS:[EMSPAGETABLE]	; absolute offset in table
	shl	edx,2				; dword/entry
	add	edx,fs:[EMSPageAllocationPtrs]	; edx -> EMS page descriptor entry

	cmp	edx,fs:[EMSPageAllocationEnd]
	jae	@@ret				; out of range

	call	Free16KPage

@@ret:
	pop	edx
	ret
ReleaseEMSPage	ENDP


; find count of available XMS 4K-aligned 4K pages in 32K chunks
;  return count in edx
;  destroy no other registers
GetFreeXMS4KPageCount	PROC
	pushf			; directly accessing system tables, don't interrupt
	cli
	push	esi
	push	eax
	push	ebx
	push	cx
	xor	edx,edx
	cmp	fs:_IsXMSTableNotFixedEMS,0	; no XMS handle table info available
	je	@@countdone
	mov	cx,fs:XMS_Handle_Table.xht_numhandle
	jcxz	@@countdone
	mov	esi,fs:XMSHandleArray
	or	esi,esi
	je	@@countdone

@@hanloop:
	cmp	[esi].xas_flag,2	; see if used
	je	@@next			; yes, don't count
	xor	eax,eax
	cmp	eax,[esi].xas_addressK
	je	@@next				; can't count blank or zero-sized handle
	cmp	eax,[esi].xas_sizeK
	je	@@next

	mov	eax,[esi].xas_addressK
	mov	ebx,eax
	add	ebx,3			; round up
	add	eax,[esi].xas_sizeK
	and	al,0fch		; align to 4K boundary
	and	bl,0fch
	sub	eax,ebx		; compute size of block after alignments
	jbe	@@next
	and	al,NOT 1fh	; mask to 32K
	shr	eax,2			; convert 1K to 4K
	add	edx,eax		; update total count

@@next:
	movzx	eax,fs:XMS_Handle_Table.xht_sizeof
	add	esi,eax	; move to next handle descriptor
	dec	cx
	jne	@@hanloop

@@countdone:
	pop	cx
	pop	ebx
	pop	eax
	pop	esi
	popf
	ret
GetFreeXMS4KPageCount	ENDP


; get total and free 4K page count in pool
; return total in edx, free in eax
; destroy no other registers
GetCurrent4KPageCount	PROC
	push	esi
	push	ecx
	mov	esi,fs:PoolAllocationTable
	xor	eax,eax
	mov	edx,eax

@@findblkloop:
	cmp	[esi].psi_addressK,0
	je	@@nextblock		; unused/deallocated block
	movzx	ecx,[esi].psi_16kmax
	shl	cx,2			; convert to 4K count, known 16-bit quantity
	add	edx,ecx
	movzx	ecx,[esi].psi_4kfree
	add	eax,ecx

@@nextblock:
	add	esi,POOLBLOCK_TOTAL_SPACE
	cmp	esi,fs:PoolAllocationEnd
	jb	@@findblkloop

	pop	ecx
	pop	esi
	ret

GetCurrent4KPageCount	ENDP


; get used 16K (EMS) page count in pool
; return used EMS page count in dx, available in eax, VCPI/EMS used in edi
; destroy no other registers
GetCurrentEMSPageCount	PROC
	push	esi
	push	ecx

	xor	eax,eax
	mov	edi,eax
	mov	dx,ax
	mov	esi,fs:[EMSPageAllocationPtrs]
	mov	cx,fs:[_MAXPAGES]
	jcxz	@@ret

@@countloop:
	cmp	DWORD PTR [esi],-1
	je	@@nextcount
	inc	dx

@@nextcount:
	add	esi,4
	loop	@@countloop

	mov	esi,fs:[PoolAllocationTable]

@@findblkloop:
	cmp	[esi].psi_addressK,0
	je	@@nextfind
	movzx	ecx,[esi].psi_16kmax
	sub	cl,[esi].psi_16kfree
	add	edi,ecx			; update total pages used
	mov	cl,[esi].psi_16kfree	; high words known zero
	add	eax,ecx

@@nextfind:
	add	esi,POOLBLOCK_TOTAL_SPACE
	cmp	esi,fs:PoolAllocationEnd
	jb	@@findblkloop

@@ret:
	pop	ecx
	pop	esi
	ret

GetCurrentEMSPageCount	ENDP


; locate any adjacent free XMS block to current EMS/VCPI pool allocation block
; if found, try consuming 32K of it for pool allocation block
; the adjacent XMS block must be at the end of current pool block,
;  since the pool block base cannot be changed once set
; pool block base+block size == owner XMS handle base+handle size (end match end)
; ends must match since you can't span noncontiguous sub-blocks of an owner XMS
;  block with a single EMS/VCPI pool allocation block
; upon entry edx -> current EMS/VCPI block
; return carry clear if success, set if fail
; destroy no other registers
ExpandCurrentPoolBlock	PROC
	pushf			; directly accessing system tables, don't interrupt
	cli
	push	eax
	push	ebx
	push	ecx
	push	esi
	push	edi

	cmp	fs:_IsXMSTableNotFixedEMS,0	; no XMS handle table info available
	je	@@locfail
	cmp	[edx].psi_16kmax,2*POOLBLOCK_ALLOCATION_SPACE
	jae	@@locfail				; current block is full
	test	[edx].psi_flags,POOLBLOCK_FLAG_DONTEXPAND
	jne	@@locfail				; can't expand this block

	mov	edi,[edx].psi_descptr
	mov	ebx,[edi].xas_addressK
	add	ebx,[edi].xas_sizeK	; ebx -> end of current pool block owner XMS

; see if owner XMS for EMS/VCPI allocation block end matches
;  end of pool allocation block
	movzx	ecx,[edx].psi_startadj
	mov	eax,[edx].psi_addressK
	sub	eax,ecx					; true XMS start when pool allocation block created
	movzx	ecx,[edx].psi_16kmax
	shl	ecx,4						; convert to K
	add	eax,ecx
	movzx	ecx,[edx].psi_endadj
	add	eax,ecx					; true XMS end when block created
	cmp	eax,ebx
	jne	@@locfail				; owner XMS end no longer matches initial pool block owner XMS end

	mov	cx,fs:XMS_Handle_Table.xht_numhandle
	or	cx,cx
	je	@@locfail
	mov	esi,fs:XMSHandleArray
	test	esi,esi
	je	@@locfail

; esi -> test XMS block
@@hanloop:
	cmp	[esi].xas_flag,2		; see if used
	je	@@next						; yes, don't check
	mov	eax,[esi].xas_addressK
	or	eax,eax				; test for valid
	je	@@next

	cmp	eax,ebx			; see if test block immediately follows current block	
	jne	@@next			; no

	movzx	eax,[edx].psi_endadj
	add	eax,[esi].xas_sizeK
	cmp	eax,32		; free block plus unused end overlap must be >=32K
	jb	@@next

; transfer 32K of following block to current block - unused end K in current
	mov	eax,32
	movzx	ecx,[edx].psi_endadj
	sub	eax,ecx					; adjust amount to change preceding block
	add	[esi].xas_addressK,eax	; move changed block address ahead
	sub	[esi].xas_sizeK,eax	; and adjust size
	mov	edi,[edx].psi_descptr
	add	[edi].xas_sizeK,eax	; increase EMS/VCPI associated XMS block size
	mov	[edx].psi_endadj,0	; no end overlap

	add	[edx].psi_16kmax,2	; adjust allocation tracking bytes
	add	[edx].psi_16kfree,2
	add	[edx].psi_4kfree,8
	movzx	eax,[edx].psi_16kmax
	shr	eax,1						; byte offset in allocation space (32K/byte)
	dec	eax						; relative 0
	mov	BYTE PTR [edx+eax+POOLBLOCK_SYSTEM_SPACE],0	; zero tracking allocation byte

; see if changed contiguous XMS block size went to <32K,
;  if so, transfer any remainder to pool block and zero XMS block
	mov	eax,[esi].xas_sizeK
	cmp	eax,31
	ja	@@loc2
	mov	[edx].psi_endadj,al

	xor	eax,eax
	mov	[esi].xas_addressK,eax
	mov	[esi].xas_sizeK,eax
	mov	[esi].xas_lockcount,al
	mov	[esi].xas_flag,1			; flag free

@@loc2:
	mov	fs:LastBlockFreed,edx	; expanding block size is same as freeing space
	pop	edi
	pop	esi
	pop	ecx
	pop	ebx
	pop	eax
	popf
	clc					; flag success
	ret

@@next:
	movzx	eax,fs:XMS_Handle_Table.xht_sizeof
	add	esi,eax		; move to next handle descriptor
	dec	cx
	jne	@@hanloop

@@locfail:
	pop	edi
	pop	esi
	pop	ecx
	pop	ebx
	pop	eax
	popf
	stc				; flag failure
	ret

ExpandCurrentPoolBlock	ENDP


; expand any available allocation pool block by 32K, if possible
;  return edx -> expanded allocation pool block, zero if none
; destroy no other registers
ExpandAnyPoolBlock	PROC
	push	esi
	cmp	fs:_IsXMSTableNotFixedEMS,0	; no XMS handle table info available
	je	@@fail

	mov	esi,fs:PoolAllocationTable

@@findblkloop:
	cmp	[esi].psi_addressK,0	; unused/deallocated block
	je	@@nextblock
	cmp	[esi].psi_16kmax,2*POOLBLOCK_ALLOCATION_SPACE
	jae	@@nextblock				; current block is full

	mov	edx,esi
	call	ExpandCurrentPoolBlock
	jc	@@nextblock			; couldn't expand this block

@@done:
	pop	esi
	ret

@@nextblock:
	add	esi,POOLBLOCK_TOTAL_SPACE
	cmp	esi,fs:PoolAllocationEnd
	jb	@@findblkloop

@@fail:
	xor	edx,edx			; failure
	jmp	@@done

ExpandAnyPoolBlock	ENDP


; find and allocate free 4K (VCPI) block in pool blocks
; return edx == physical address, zero if none found
; destroy ecx,esi,edi
Allocate4KPageFromPoolBlock	PROC
	push	eax

@@begin:
	xor	edx,edx

; first try last block allocated, to avoid searching full blocks if possible
	mov	esi,fs:LastBlockAllocator
	or	esi,esi
	je	@@nolastalloc
	cmp	[esi].psi_addressK,edx
	je	@@nolastalloc
	cmp	[esi].psi_4kfree,dx
	jne	@@searchbytes

; try last freed chunk
@@nolastalloc:
	mov	esi,fs:LastBlockFreed
	or	esi,esi
	je	@@nolastfreed
	cmp	[esi].psi_addressK,edx
	je	@@nolastfreed
	cmp	[esi].psi_4kfree,dx
	jne	@@searchbytes

@@nolastfreed:
	mov	esi,fs:PoolAllocationTable

@@findblkloop:
	cmp	[esi].psi_addressK,0	; unused/deallocated block
	je	@@nextblock
	cmp	[esi].psi_4kfree,0
	je	@@nextblock

@@searchbytes:
	movzx	cx,[esi].psi_16kmax
	shr	cx,1			; count of allocation bytes in block
	xor	edi,edi

@@findbyteloop:
	mov	al,[esi+edi+POOLBLOCK_SYSTEM_SPACE]
	xor	al,-1			; unallocated 4K areas show as bit set
	jne	@@freebyte	; at least one unallocated area
	inc	edi
	loop	@@findbyteloop

; nothing free, although block indicated there was
	call	CheckBlockIntegrity
	jmp	@@begin

@@freebyte:
	mov	ecx,edi
	shl	ecx,15		; each byte covers 32K
	mov	edx,[esi].psi_addressK
	shl	edx,10		; convert from K to bytes
	add	edx,ecx		; compute base address of block addressed by byte

	mov	ah,1

; al holds bitcodes of allocations, set if available due to xor
@@bitloop:
	shr	al,1
	jc	@@found
	add	edx,4096
	shl	ah,1
	jmp	@@bitloop

@@found:
	or	[esi+edi+POOLBLOCK_SYSTEM_SPACE],ah	; flag that 4K area has been allocated
	dec	[esi].psi_4kfree
	mov	al,[esi+edi+POOLBLOCK_SYSTEM_SPACE]
	cmp	ah,0fh		; see if allocated from low nybble or high
	ja	@@highnyb

; low nybble
	and	al,0fh
	jmp	@@nybshared

@@highnyb:
	and	al,0f0h

; see if first allocation in that nybble
;  if so, then reduce free 16K region count since it was partially consumed
@@nybshared:
	mov	fs:LastBlockAllocator,esi	; update last block allocated from

	xor	al,ah			; turn off bit we turned on
	jne	@@finddone	; not the first bit allocated in 16K region (nybble)
	dec	[esi].psi_16kfree
	jnc	@@finddone

	call	CheckBlockIntegrity
	jmp	@@finddone

@@nextblock:
	add	esi,POOLBLOCK_TOTAL_SPACE
	cmp	esi,fs:PoolAllocationEnd
	jb	@@findblkloop

@@finddone:
	pop	eax
	ret

Allocate4KPageFromPoolBlock	ENDP


; find and allocate free 16K (EMS) block in pool blocks
; upon entry ebx -> EMS page descriptor entry
; return edx == physical address, zero if none found
;  set page descriptor high word == 64-byte EMS/VCPI allocation block count from start, RELATIVE 1!
;  set page descriptor low word ==  half-byte offset of allocation page (2 * 48)
;   within allocation block, not including system bytes
; destroy no other registers
Allocate16KPageFromPoolBlock	PROC
	push	eax
	push	ecx
	push	esi
	push	edi

@@begin:
	xor	edx,edx

; first try last block allocated, to avoid searching full blocks if possible
	mov	esi,fs:LastBlockAllocator
	or	esi,esi
	je	@@nolastalloc
	cmp	[esi].psi_addressK,edx
	je	@@nolastalloc
	cmp	[esi].psi_16kfree,dl
	jne	@@searchbytes

; try last freed chunk
@@nolastalloc:
	mov	esi,fs:LastBlockFreed
	or	esi,esi
	je	@@nolastfreed
	cmp	[esi].psi_addressK,edx
	je	@@nolastfreed
	cmp	[esi].psi_16kfree,dl
	jne	@@searchbytes

@@nolastfreed:
	mov	esi,fs:PoolAllocationTable

@@findblkloop:
	cmp	[esi].psi_addressK,0	; unused/deallocated block
	je	@@nextblock
	cmp	[esi].psi_16kfree,0
	je	@@nextblock

@@searchbytes:
	movzx	cx,[esi].psi_16kmax
	shr	cx,1			; count of allocation bytes in block
	xor	edi,edi

@@findbyteloop:
	mov	al,[esi+edi+POOLBLOCK_SYSTEM_SPACE]
	xor	al,-1			; unallocated 4K areas show as bit set
	mov	ah,al
	and	al,0fh
	cmp	al,0fh
	je	@@lowfree		; low nybble unallocated, free 16K area

	and	ah,0f0h
	cmp	ah,0f0h
	je	@@highfree		; high nybble unallocated, free 16K area

; no free 16K area
	inc	edi
	loop	@@findbyteloop

	call	CheckBlockIntegrity
	jmp	@@begin

@@lowfree:
	or	BYTE PTR [esi+edi+POOLBLOCK_SYSTEM_SPACE],0fh
	xor	cx,cx
	jmp	@@freeshared	; edx == 0

@@highfree:
	or	BYTE PTR [esi+edi+POOLBLOCK_SYSTEM_SPACE],0f0h
	mov	cx,1
	mov	edx,16		; nybble offset is four 4K pages, 16K

@@freeshared:
	mov	eax,edi
	shl	eax,15		; each byte covers 32K
	add	edx,[esi].psi_addressK	; add in base value
	shl	edx,10		; convert K to bytes
	add	edx,eax		; edx == 16k page memory address
	dec	[esi].psi_16kfree
	jnc	@@valid1
	call	CheckBlockIntegrity	; force valid value

@@valid1:
	sub	[esi].psi_4kfree,4
	jnc	@@valid2
	call	CheckBlockIntegrity	; force valid value

; update ebx pointer
@@valid2:
	mov	eax,esi
	sub	eax,fs:PoolAllocationTable
	shr	eax,6		; convert to 64-byte offset (block count)
	inc	ax			; make count relative 1
	mov	[ebx+2],ax
	mov	ax,cx		; get even/odd offset
	shl	di,1		; half-byte offset within block, not counting system info bytes
	add	ax,di		; ax holds half-byte offset within block
	mov	[ebx],ax

	mov	fs:LastBlockAllocator,esi	; update last block allocated from

@@alloc16ret:
	pop	edi
	pop	esi
	pop	ecx
	pop	eax
	ret

@@nextblock:
	add	esi,POOLBLOCK_TOTAL_SPACE
	cmp	esi,fs:PoolAllocationEnd
	jb	@@findblkloop
	jmp	@@alloc16ret

Allocate16KPageFromPoolBlock	ENDP


; find an unused allocation block
; return edx -> allocation block, or zero if none and no space available
; no other registers modified
GetUnusedAllocationBlock	PROC
	mov	edx,fs:PoolAllocationTable

@@findblkloop:
	cmp	[edx].psi_addressK,0	; unused/deallocated block
	je	@@getret

@@nextblock:
	add	edx,POOLBLOCK_TOTAL_SPACE
	cmp	edx,fs:PoolAllocationEnd
	jb	@@findblkloop

	xor	edx,edx		; failed

@@getret:
	ret

GetUnusedAllocationBlock	ENDP


; prepare allocation block for use
; upon entry:
;  edx -> pool allocation block
;  ecx == raw size in K before alignment
;  edi == raw address in K before alignment
;  esi == owner XMS handle psi_descptr value, do NOT use XMS handle values for
;   size and address since this call may be part of a multi-pool block span
; destroys no registers
PrepAllocationBlock	PROC
	push	eax
	push	ebx
	push	ecx
	push	edi
	mov	[edx].psi_descptr,esi

	mov	eax,edi			; raw address
	mov	bl,al
	and	bl,3
	mov	bh,4
	sub	bh,bl
	and	bh,3
	mov	[edx].psi_startadj,bh
	add	eax,3			; round up address
	and	al,NOT 3
	mov	[edx].psi_addressK,eax

; block size = (raw size - start adjustment) rounded down to 32K boundary
;  since each allocation byte covers 32K
	mov	eax,ecx			; raw size
	movzx	ebx,bh		; start adustment
	sub	eax,ebx
	and	al,NOT 31
	shr	ax,4			; 16K count, known 16-bit value going to 8-bit

	cmp	ax,POOLBLOCK_ALLOCATION_SPACE*2
	jbe	@@setmax
	mov	ax,POOLBLOCK_ALLOCATION_SPACE*2

@@setmax:
	mov	[edx].psi_16kmax,al
	mov	[edx].psi_16kfree,al
	shl	ax,2			; 8-bit value potentially going to 16-bit
	mov	[edx].psi_4kfree,ax

; compute end adjustment, raw size - (block size + start adjustment)
	movzx	ebx,[edx].psi_16kmax
	shl	ebx,4			; convert to K
	movzx	eax,[edx].psi_startadj
	add	eax,ebx
	sub	ecx,eax			; ecx == raw size
	mov	[edx].psi_endadj,cl

; zero allocation entries
	xor	eax,eax
	mov	edi,edx
	add	edi,POOLBLOCK_SYSTEM_SPACE
	mov	cx,POOLBLOCK_ALLOCATION_SPACE/4

@@zeroloop:
	mov	[edi],eax
	add	edi,4
	loop	@@zeroloop

	pop	edi
	pop	ecx
	pop	ebx
	pop	eax
	ret

PrepAllocationBlock	ENDP


; populate empty pool allocation blocks with XMS owner info
; upon entry esi -> owner XMS (pseudo-)handle, ecx == size, edi == address
;  NOTE: ecx and edi may not match owner XMS size/address
; return carry set if insufficient number of empty blocks to cover XMS handle range
;  reset otherwise
; destroy eax,ecx,edx
;  update edi
GetBlocksForXMSHandle	PROC
	push	ebx
	mov	ebx,ecx

@@allocloop:
	call	GetUnusedAllocationBlock
	or	edx,edx
	je	@@exhausted			; no more blocks, remainder of XMS is effectively discarded

	mov	ax,di				; compute size of candidate block/offset to new
	and	al,3
	mov	ah,4
	sub	ah,al
	and	ah,3
	movzx	eax,ah
	add	eax,1536			; 1.5M (in K) plus alignment adjustment size
	cmp	eax,ebx
	jbe	@@sizeok
	mov	eax,ebx

@@sizeok:
	mov	ecx,eax
	call	PrepAllocationBlock	; uses esi entry condition
	sub	ebx,eax			; update size left to allocate
	je	@@allocsuccess
	cmp	ebx,32			; see if should remainder what's left
	jb	@@remainder
	add	edi,eax			; update pool allocation block address
	jmp	@@allocloop

@@exhausted:
	stc
	jmp	@@getret

@@remainder:
	mov	[edx].psi_endadj,bl

@@allocsuccess:
	clc

@@getret:
	pop	ebx
	ret

GetBlocksForXMSHandle	ENDP


; walk XMS blocks, find largest XMS block which is sized 1.5M or smaller >=32K
;  after 4K alignment and allocate it for new EMS/VCPI pool allocation block
; if all XMS blocks >1.5M, then pick smallest and try to put remainder
;  into a free handle.
; If no free handle, allocate sufficient new EMS/VCPI pool blocks to cover full range.
; If not enough free EMS/VCPI blocks available, then remaining allocation is lost
;  until handle is freed.  This could only happen under very bad XMS fragmentation,
;  if at all.
; return carry clear if success, set if fail
; destroy no other registers
AllocateXMSForPool	PROC
	pushf			; directly accessing system tables, don't interrupt
	cli
	push	eax
	push	ebx
	push	ecx
	push	edx
	push	esi
	push	edi

	cmp	fs:_IsXMSTableNotFixedEMS,0	; check if pool sharing
	je	@@allocfail

	mov	esi,fs:XMSHandleArray
	or	esi,esi
	je	@@allocfail
	mov	cx,fs:XMS_Handle_Table.xht_numhandle
	or	cx,cx
	je	@@allocfail
	call	GetUnusedAllocationBlock	; test only, don't keep pointer
	jc	@@allocfail			; unable to make initial pool block allocation
	xor	edx,edx			; edx -> largest block <= 1.5M or smallest if none <=1.5M

@@hanloop:
	cmp	[esi].xas_flag,2	; see if used
	je	@@next				; yes, don't check
	cmp	[esi].xas_addressK,0
	je	@@next				; can't check blank or zero-sized handle
	mov	eax,[esi].xas_sizeK
	or	eax,eax
	je	@@next
	or	edx,edx
	je	@@newcandidate		; auto-match if first xms block available

; adjust for alignment loss (1-3K) in test
	mov	bl,BYTE PTR [esi].xas_addressK
	and	bl,3
	mov	bh,4
	sub	bh,bl
	and	bh,3
	movzx	ebx,bh
	sub	eax,ebx

; adjust for alignment lost in candidate
	cmp	[edx].xas_sizeK,32	; ensure candidate size isn't so small that adjustment will underflow
	jb	@@next			; doesn't even meet minimum requirements
	mov	bl,BYTE PTR [edx].xas_addressK
	and	bl,3
	mov	bh,4
	sub	bh,bl
	and	bh,3
	movzx	ebx,bh
	neg	ebx
	add	ebx,[edx].xas_sizeK

; eax holds test value size, ebx holds current candidate, both alignment adjusted
	cmp	eax,ebx
	je	@@next
	ja	@@larger

; test XMS block smaller than candidate block
	cmp	ebx,1536			; in K
	jbe	@@next			; current candidate closer to match size
	cmp	eax,32
	jb	@@next				; test too small
	jmp	@@newcandidate

; test XMS block larger than candidate block
@@larger:
	cmp	ebx,1536
	jae	@@next			; current candidate closer to match size
	cmp	eax,1536
	ja	@@next				; test too large

@@newcandidate:
	cmp	[esi].xas_sizeK,32
	jb	@@next			; candidate doesn't even meet unadjusted requirements
	mov	edx,esi			; new best candidate

@@next:
	movzx	eax,fs:XMS_Handle_Table.xht_sizeof
	add	esi,eax			; move to next handle descriptor
	dec	cx
	jne	@@hanloop
	or	edx,edx
	je	@@allocfail

; candidate must be at least 32K, after 4K alignment adjustment
	mov	bl,BYTE PTR [edx].xas_addressK
	and	bl,3
	mov	bh,4
	sub	bh,bl
	and	bh,3
	movzx	ebx,bh
	neg	ebx
	add	ebx,[edx].xas_sizeK
	cmp	ebx,32
	jb	@@allocfail				; candidate too small

	mov	[edx].xas_flag,2	; flag candidate as used so it doesn't show up as free
	mov	[edx].xas_lockcount,1	; and show as locked

	mov	fs:[XMSBlockSize],1536	; default allocation maximum size
	mov	ax,fs:[XMSPoolBlockCount]
	cmp	ax,1
	jbe	@@trailadj			; use standard 1.5M size for first two blocks
	dec	ax					; should never overflow before we hit 4G total allocated

@@noadj:
	and	al,0fh				; but ensure that overflow doesn't happen anyway
	mov	cl,al				; shift the block size higher by factor of two
	shl	fs:[XMSBlockSize],cl

; default is same as a MAX setting
;	cmp	fs:[_MAXMEM16K],MAXMEM16K_DEFAULT
;	je	@@trailadj			; no MAX setting

; MAX setting, don't overallocate XMS we can't use
	push	edx
	call	GetCurrent4KPageCount	; current free in eax, current total in edx
	sub	edx,eax		; edx == current allocated (total - free)
	jc	@@adjusted		; shouldn't happen, continue without adjustment if it does

; if XMSBlockSize >= MAXMEM16K * 4 - allocated, then reduce XMSBlockSize
	mov	eax,fs:[_MAXMEM16K]
	shl	eax,2			; convert to maximum 4K blocks
	sub	eax,edx
	jc	@@adjusted		; shouldn't happen, continue without adjustment
	shl	eax,2			; convert to 1K blocks

@@checksize:
	cmp	eax,fs:[XMSBlockSize]
	jae	@@adjusted
	cmp	fs:[XMSBlockSize],1536	; see if XMSBlockSize is at minimum default
	jbe	@@adjusted		; yes, can't reduce it any further
	shr	fs:[XMSBlockSize],1	; reduce block size by one shift and try again
	jmp	@@checksize

@@adjusted:
	pop	edx

; allow up to 31K trailing bytes
@@trailadj:
	mov	eax,fs:[XMSBlockSize]
	add	eax,31				; adjust for possible trail
	cmp	ebx,eax
	jbe	@@setblock			; no need to split XMS handle allocation

; search for a free handle, zero in either address or size
	mov	edi,fs:XMSHandleArray
	mov	cx,fs:XMS_Handle_Table.xht_numhandle

@@freeloop:
	cmp	[edi].xas_flag,2	; see if used
	je	@@nextfree			; yes, don't check
	cmp	[edi].xas_addressK,0
	je	@@gotfree
	cmp	[edi].xas_sizeK,0
	je	@@gotfree

@@nextfree:
	movzx	eax,fs:XMS_Handle_Table.xht_sizeof
	add	edi,eax			; move to next handle descriptor
	dec	cx
	jne	@@freeloop

; no free handle found, try to allocate multiple blocks, discarding excess
	mov	ecx,[edx].xas_sizeK
	mov	edi,[edx].xas_addressK
	mov	esi,edx			; esi -> owner XMS pseudo-handle
	call	GetBlocksForXMSHandle	; ignore return status, one block minimum allocated
	jmp	@@allocsuccess

@@gotfree:
	mov	cl,BYTE PTR [edx].xas_addressK	; compute size of candidate block/offset to new
	and	cl,3
	mov	ch,4
	sub	ch,cl
	and	ch,3
	movzx	ecx,ch

;	add	ecx,1536				; 1.5M (in K) plus alignment adjustment size
	add	ecx,fs:[XMSBlockSize]	; maximum size (exceeded) plus alignment adjustment size

; edx -> candidate block being allocated, edi -> new block receiving remainder
; update candidate XMS block size
	mov	eax,[edx].xas_sizeK		; keep original size for updating new block
	mov	[edx].xas_sizeK,ecx

; update new XMS block info
	sub	eax,ecx					; new block size == old block original size - old block new size
	mov	[edi].xas_sizeK,eax
	mov	[edi].xas_flag,1	; explicitly flag free
	mov	[edi].xas_lockcount,0
	mov	eax,[edx].xas_addressK
	add	eax,ecx
	mov	[edi].xas_addressK,eax	; new block start == old block start + old block new size

; edx -> owner XMS handle for new pool allocation block(s)
; may be multiple blocks due to XMSBlockCount shifter
@@setblock:
	mov	esi,edx
;	call	GetUnusedAllocationBlock	; assume success, since entry test worked

;; update pool allocation block system info
;; edx -> pool allocation block, esi -> owner XMS handle
	mov	ecx,[esi].xas_sizeK
	mov	edi,[esi].xas_addressK
;	call	PrepAllocationBlock

	call	GetBlocksForXMSHandle

@@allocsuccess:
	inc	fs:[XMSPoolBlockCount]
	pop	edi
	pop	esi
	pop	edx
	pop	ecx
	pop	ebx
	pop	eax
	popf
	clc
	ret

@@allocfail:
	pop	edi
	pop	esi
	pop	edx
	pop	ecx
	pop	ebx
	pop	eax
	popf
	stc
	ret

AllocateXMSForPool	ENDP


; upon entry edx -> 4K page absolute address to free
; return carry clear on success, set on fail
; destroy ecx,esi,edi
Free4KPage	PROC
	push	eax
	push	ebx
	push	edx

	mov	ebx,edx
	shr	ebx,10		; convert bytes to K
	and	bl,NOT 3		; ensure 4K alignment

	mov	esi,fs:LastBlockFreed
	or	esi,esi
	je	@@notlastfreed
	mov	eax,[esi].psi_addressK
	or	eax,eax
	je	@@notlastfreed	; unused/deallocated block

; ebx == start of 4K page in K after alignment adjustment
	cmp	ebx,eax
	jb	@@notlastfreed	; pool block starts after page
	movzx	ecx,[esi].psi_16kmax
	shl	ecx,4			; convert 16K to 1K
	add	ecx,eax			; ecx == end of range holding 4K pages
	cmp	ebx,ecx
	jb	@@rightblock	; after start, before end

@@notlastfreed:
	mov	esi,fs:LastBlockAllocator
	or	esi,esi
	je	@@notlastalloc
	mov	eax,[esi].psi_addressK
	or	eax,eax
	je	@@notlastalloc	; unused/deallocated block

	cmp	ebx,eax
	jb	@@notlastalloc	; pool block starts after page
	movzx	ecx,[esi].psi_16kmax
	shl	ecx,4			; convert 16K to 1K
	add	ecx,eax			; ecx == end of range holding 4K pages
	cmp	ebx,ecx
	jb	@@rightblock	; after start, before end

@@notlastalloc:
	mov	esi,fs:PoolAllocationTable

@@findblkloop:
	mov	eax,[esi].psi_addressK
	or	eax,eax
	je	@@nextblock		; unused/deallocated block

	cmp	ebx,eax
	jb	@@nextblock	; pool block starts after page
	movzx	ecx,[esi].psi_16kmax
	shl	ecx,4			; convert 16K to 1K
	add	ecx,eax		; ecx == end of range holding 4K pages
	cmp	ebx,ecx
	jb	@@rightblock	; page to free within pool block

@@nextblock:
	add	esi,POOLBLOCK_TOTAL_SPACE
	cmp	esi,fs:PoolAllocationEnd
	jb	@@findblkloop

@@fail:
	call	CheckBlockIntegrity
	stc
	jmp	@@ret

; this is the proper pool allocation block
@@rightblock:
	sub	ebx,eax		; 4K offset from block base in K
	mov	eax,ebx
	shr	eax,2			; K to 4K page
	mov	cl,al			; keep bit offset
	shr	eax,3			; 4K page to 32K byte offset
	and	cl,7
	mov	bl,1
	rcl	bl,cl			; get mask bit

	test	[esi+eax+POOLBLOCK_SYSTEM_SPACE],bl	; see if bit set (was allocated)
	je	@@fail			; no

	not	bl				; turn on all bits except current allocation's
	and	[esi+eax+POOLBLOCK_SYSTEM_SPACE],bl

	inc	[esi].psi_4kfree

; check if this frees up a 16K chunk
	mov	al,[esi+eax+POOLBLOCK_SYSTEM_SPACE]
	and	bl,0f0h
	cmp	bl,0f0h		; see if high nybble was freed
	je	@@nothigh		; all bits set, freed portion was low

	test	al,0f0h		; see if all high bits of nybble cleared
	jne	@@success	; no
	jmp	@@inc16

@@nothigh:
	test	al,0fh		; see if all low bits of nybble cleared
	jne	@@success		; no

@@inc16:
	inc	[esi].psi_16kfree

@@success:
	mov	fs:LastBlockFreed,esi
	call	TryFreeToXMS	; free empty pool allocation block to XMS if appropriate
	clc

@@ret:
	pop	edx
	pop	ebx
	pop	eax
	ret

Free4KPage	ENDP


; upon entry edx -> EMS page descriptor pointer
;  upon return set page descriptor pointer to -1 (unused)
; destroy no registers
Free16KPage	PROC
	push	esi
	push	eax
	push	ecx

	movzx	esi,WORD PTR [edx+2]	; [e]si == 64-byte block count, relative 1
	cmp	si,-1
	je	@@fail			; bad pointer
	dec	si				; make relative 0
	shl	esi,6			; convert 64-byte count to byte offset
	add	esi,fs:PoolAllocationTable	; esi -> pool allocation block

	mov	cl,[edx]		; half-byte offset
	shr	cl,1			; byte offset
	movzx	ecx,cl
	mov	al,[esi+ecx+POOLBLOCK_SYSTEM_SPACE]
	test	BYTE PTR [edx],1	; see if odd/even nybble
	jne	@@oddbyte

	and	al,0fh
	and	BYTE PTR [esi+ecx+POOLBLOCK_SYSTEM_SPACE],0f0h	; reset all expected bits
	cmp	al,0fh
	jne	@@fail		; not all expected bits were set
	jmp	@@success

@@oddbyte:
	and	al,0f0h
	and	BYTE PTR [esi+ecx+POOLBLOCK_SYSTEM_SPACE],0fh	; reset all expected bits
	cmp	al,0f0h
	jne	@@fail

@@success:
	inc	[esi].psi_16kfree
	add	[esi].psi_4kfree,4
	mov	fs:LastBlockFreed,esi
	call	TryFreeToXMS	; free empty pool allocation block to XMS if appropriate
	clc

@@ret:
	mov	DWORD PTR [edx],-1
	pop	ecx
	pop	eax
	pop	esi
	ret

@@fail:
	call	CheckBlockIntegrity
	stc
	jmp	@@ret

Free16KPage	ENDP


; upon entry esi -> pool allocation block to check if freeable to XMS
;  perform the free if possible
; destroys eax,ecx,esi
TryFreeToXMS	PROC
	pushf			; directly accessing system tables, don't interrupt
	cli
	push	edx
	push	edi

	cmp	fs:_IsXMSTableNotFixedEMS,0	; check if pool sharing
	je	@@checkdone		; no

	test	[esi].psi_flags,POOLBLOCK_FLAG_DONTEXPAND
	jne	@@checkdone		; can't free this block

@@proccheck:
	mov	al,[esi].psi_16kfree
	cmp	al,[esi].psi_16kmax
	ja	@@bad			; free more than max, try to fix
	jne	@@checkdone		; free is less than maximum, used

	mov	ax,[esi].psi_4kfree
	shr	ax,2
	or	ah,ah
	jne	@@bad
	cmp	al,[esi].psi_16kmax
	ja	@@bad
	jne	@@checkdone		; free less than max

; walk all pool blocks, see if all blocks for XMS handle are empty or nonexistent
;  if so, then mark XMS handle as free
	mov	esi,[esi].psi_descptr
	mov	edx,fs:PoolAllocationTable

@@checkblkloop:
	cmp	[edx].psi_addressK,0
	je	@@checknext			; unused block

	cmp	esi,[edx].psi_descptr
	jne	@@checknext

	test	[edx].psi_flags,POOLBLOCK_FLAG_DONTEXPAND
	jne	@@checkdone		; can't free this block

; see if block empty
@@check:
	movzx	cx,[edx].psi_16kmax
	cmp	cl,[edx].psi_16kfree
	jne	@@checkdone

	shl	cx,2				; convert to 4K max
	cmp	cx,[edx].psi_4kfree
	jne	@@checkdone

@@checknext:
	add	edx,POOLBLOCK_TOTAL_SPACE
	cmp	edx,fs:PoolAllocationEnd
	jb	@@checkblkloop

; checked all blocks as empty, go through them again and mark unused
	mov	edx,fs:PoolAllocationTable

@@freeblkloop:
	cmp	[edx].psi_addressK,0
	je	@@freenext			; unused block

	cmp	esi,[edx].psi_descptr
	jne	@@freenext

; zero the block
	mov	cx,POOLBLOCK_TOTAL_SPACE/4
	xor	eax,eax
	mov	edi,edx

@@zeroloop:
	mov	[edi],eax
	add	edi,4
	loop	@@zeroloop

@@freenext:
	add	edx,POOLBLOCK_TOTAL_SPACE
	cmp	edx,fs:PoolAllocationEnd
	jb	@@freeblkloop

	mov	[esi].xas_lockcount,0
	mov	[esi].xas_flag,1			; flag free
	dec	fs:[XMSPoolBlockCount]

@@defragger:
	call	DefragXMS
	jc	@@defragger

@@checkdone:
	pop	edi
	pop	edx
	popf
	ret

@@bad:
	call	CheckBlockIntegrity
	jmp	@@proccheck

TryFreeToXMS	ENDP

; try to defrag XMS blocks
; return when two blocks merged
; carry set if merge occurred, reset otherwise
; destroys eax,ecx,edx,esi,edi
DefragXMS	PROC
	push	ebx
	mov	esi,fs:XMSHandleArray
	or	esi,esi
	je	@@nodefrag
	mov	cx,fs:XMS_Handle_Table.xht_numhandle
	or	cx,cx
	je	@@nodefrag
	mov	ebx,[esi].xas_addressK
	add	ebx,[esi].xas_sizeK	; ebx -> end address of first block (highest+1)

@@defragloop:
	cmp	[esi].xas_flag,2	; see if used
	je	@@defragnext		; yes, don't check
	cmp	[esi].xas_addressK,0
	je	@@defragnext		; can't check blank handle
	mov	edi,fs:XMSHandleArray
	mov	dx,fs:XMS_Handle_Table.xht_numhandle

@@checkloop:
	cmp	esi,edi				; don't merge into self
	je	@@checknext

	cmp	[edi].xas_flag,2	; see if used
	je	@@checknext		; yes, don't check
	cmp	[edi].xas_addressK,0
	je	@@checknext		; can't check blank handle

; check if end of first block matches beginning of test block
	cmp	ebx,[edi].xas_addressK
	je	@@merge

; check if start of first block matches end of test block
	mov	eax,[edi].xas_addressK
	add	eax,[edi].xas_sizeK	; eax -> end address of block (highest+1)
	cmp	eax,[esi].xas_addressK
	jne	@@checknext
	xchg	esi,edi		; switch block pointer references for merging
	jmp	@@merge

@@checknext:
	movzx	eax,fs:XMS_Handle_Table.xht_sizeof
	add	edi,eax			; move to next handle descriptor
	dec	dx
	jne	@@checkloop

@@defragnext:
	movzx	eax,fs:XMS_Handle_Table.xht_sizeof
	add	esi,eax			; move to next handle descriptor
	dec	cx
	jne	@@defragloop

@@nodefrag:
	clc

@@defragret:
	pop	ebx
	ret

; merge blocks, esi->merger block, edi->merged block
@@merge:
	mov	eax,[edi].xas_sizeK
	add	[esi].xas_sizeK,eax	; update merger block size
	mov	[esi].xas_lockcount,0	; ensure not showing as locked, shouldn't be

	mov	[edi].xas_flag,1	; flag merged block free
	mov	[edi].xas_lockcount,0
	mov	[edi].xas_addressK,0
	mov	[edi].xas_sizeK,0

	stc
	jmp	@@defragret

DefragXMS	ENDP

; hook left for debugging, no current actions taken
;; upon entry esi -> pool allocation block to perform integrity check upon
;;  update with valid information if allocation counts mismatch
;; destroy no registers
CheckBlockIntegrity	PROC
COMMENT $
	push	ax
	push	bx
	push	cx
	push	edx

	cmp	[esi].psi_addressK,0	; unused/deallocated block
	je	@@checkret

	movzx	cx,[esi].psi_16kmax
	or	cx,cx
	je	@@checkret		; can't fix zero block
	shr	cl,1			; allocation byte count
	je	@@checkret		; can't fix zero block
	jnc	@@checkoverflow
	and	BYTE PTR [esi].psi_16kmax,NOT 1	; odd maximum is wrong, even it

@@checkoverflow:
	cmp	cl,POOLBLOCK_ALLOCATION_SPACE
	jbe	@@walkbits

; true error, max is too large
	mov	cl,POOLBLOCK_ALLOCATION_SPACE
	mov	[esi].psi_16kmax,POOLBLOCK_ALLOCATION_SPACE * 2

; walk pool block allocation bits, counting free 4K and 16K chunks
;  then update the free allocation fields with proper amounts, if incorrect
@@walkbits:
	mov	edx,POOLBLOCK_SYSTEM_SPACE	; edx -> byte within pool block
	xor	bx,bx			; count of allocated 4K blocks
	mov	ch,bl			; count of allocated 16K blocks

@@checkloop:
	mov	al,[esi+edx]
	mov	ah,al
	and	al,0fh
	je	@@checkhigh
	cmp	al,0fh
	jne	@@getlow
	inc	ch				; bump count of allocated 16K blocks

@@getlow:
	shr	al,1
	adc	bx,0			; bump count of allocated 4K blocks if bit set
	shr	al,1
	adc	bx,0
	shr	al,1
	adc	bx,0
	shr	al,1
	adc	bx,0

@@checkhigh:
	mov	al,ah
	and	al,0f0h
	je	@@next
	cmp	al,0f0h
	jne	@@gethigh
	inc	ch				; bump count of allocated 16K blocks

@@gethigh:
	shr	al,1
	adc	bx,0			; bump count of allocated 4K blocks if bit set
	shr	al,1
	adc	bx,0
	shr	al,1
	adc	bx,0
	shr	al,1
	adc	bx,0

@@next:
	inc	edx
	dec	cl
	jne	@@checkloop

; ch == count of allocated 16K blocks
; bx == count of allocated 4K blocks
	mov	al,[esi].psi_16kmax
	sub	al,ch
	mov	[esi].psi_16kfree,al

	movzx	ax,[esi].psi_16kmax
	shl	ax,2
	sub	ax,bx
	mov	[esi].psi_4kfree,ax

@@checkret:
	pop	edx
	pop	cx
	pop	bx
	pop	ax
END COMMENT $

	ret

CheckBlockIntegrity	ENDP


;(*-8-*)
V86_LEN         EQU     OFFSET $          ; End of Code-range

V86     ENDS
;
; Installationpart of the virtual Monitor, that later is given back again
; to the system.
;

_TEXT SEGMENT
		extrn _startup_driver:far
		extrn _startup_exe:far
		extrn _finishing_touches:far
_TEXT ENDS


_TEXT SEGMENT
		ASSUME  CS:_TEXT,DS:MONDATA

;*********************************************
; startpoint when executing as EXE
;*********************************************


GO:
	MOV     AX,SEG MONDATA          ; Set up Data segment
	MOV     DS,AX
        MOV     [PSP],ES             ; save PSP for later

	mov     ax, DGROUP
	mov     ds,ax
	mov     ss,ax
	mov     sp, offset DGROUP:exe_stacktop


	push  es		    ; startup_exe(commandline);
	mov   ax,080h

	push  ax
	call  _TEXT:_startup_exe
	add sp,4

				; exit
        mov     ah,04ch         ; that was all
	int 	21h

;***** exe done



;**********************************************
; driver init part
;**********************************************

go_driver_entry proc far

	push eax
	push ebx
	push ecx
	push edx
	push esi
	push ebp
	push ds
	push fs
	push gs

	mov word ptr es:[di+3	 ],1000h	; STATUS_BAD

	cmp byte ptr es:[di+2],0 		; command == 0 : do we have to initialize?
	jne driver_done


	push es					; request_ptr, needed later
	push di

	call far ptr  go_driver

	pop di
	pop es					; reload address of request header


	or ax,ax				; ax = size of resident code
	jz driver_done

	mov          es:[di+0eh  ],ax   ;
	mov word ptr es:[di+3	 ],0800h ; STATUS_OK


driver_done:
						; we are done,
						; patch the code, calling this
						; driver entry, so we NEVER get called again
	mov bp,sp				;
						; stack frame:
;	lds bx, [bp + 14]			; 7*regs, [cs:ip]
	lds bx, [bp + 30]			; 6*regs+3*sregs, [cs:ip]

	sub bx,5				; call far

	mov byte ptr [bx],0ebh			; JMP $+3
	mov byte ptr [bx+1],03h

	pop gs
	pop fs
	pop ds
	pop ebp
	pop esi
	pop edx
	pop ecx
	pop ebx
	pop eax
	retf
go_driver_entry ENDP




;struc	request_hdr
;  req_size	db	?		; number of bytes stored
;  unit_id	db	?		; unit ID code
;  cmd		db	?		; command code
;  status	dw	?		; status word
;  rsvd		db	8 dup (?)	; reserved
;ends	request_hdr

;struc	init_strc
;  init_hdr	db	size request_hdr dup (?)
;  units		db	?	; number of supported units
;  end_addr	dd	?		; end address of resident part
;  cmd_line	dd	?		; address of command line
;ends	init_strc

driverss dw 0
driversp dw 0
driverret dw 0

go_driver PROC FAR


	mov cs:[driverret],0
	mov cs:[driverss],ss
	mov cs:[driversp],sp

	mov     ax, DGROUP
	mov     ds,ax
	mov     ss,ax
	mov     sp, offset DGROUP:exe_stacktop

	les ax, es:[di+18]	; fetch driver commandline

	push es                 ; startup_driver(char far *cmdLine)
	push ax
	call _startup_driver
	add sp,4

				;some bugs ??
	or ax,ax
	jnz fail_driver


        XOR     AX,AX           ; next, restore (set back?)
        MOV     DS,AX           ; Reset-Flag
        MOV     AX,00AAh        ; usual OK-message
	MOV     WORD PTR DS:[@RESET_FLAG],AX
	MOV     AX,SEG MONDATA      ; Set data segment
	MOV     DS,AX


;(*-9-*)

;        MOV     AH,88H               ; determine size of Extended Memory
;        INT     15H
;        MOV     [EXTMEM],AX          ; (in kBytes)

        MOV     AX,3567H              ; save old contents of EMM-Vector
	INT     21H
	MOV     WORD PTR [OLDINT67],BX
	MOV     WORD PTR [OLDINT67+2],ES
	MOV     AX,ES                  ; EMM already installed ?
	OR      AX,BX                  ;
        JZ      Neu_inst               ; if not, the new
;   **** #### **** zusaetzliche Abfrage nach EMMXXXX0 ********
	MOV     DI,10
	MOV     SI,OFFSET Kennung
	CLD
	MOV     CX,8
	REPZ    CMPSB

;	JNZ     Neu_inst
	je	installed		; matched 1st ID string
	mov	di,10			; didn't match, check 2nd ID (NOEMS version)
	mov	si,OFFSET sig2
	mov	cx,8
	repz	cmpsb
	jnz	Neu_inst		; didn't match 2nd ID string

;   **** #### ***********************************************

installed:
	mov 	dx, OFFSET msg_already_installed
	jmp 	ERR_EX

Neu_inst:


        MOV     DX,OFFSET DUMMY_CALL ; install EMM-Vector
	MOV     AX,2567H
	INT     21H
;(*-9-*)
;(#-9-#)
        MOV     AX,3513H                ; Save address of the "old" Disk-
        INT     21H                     ; or Platten-I/O-Interrupts
        MOV     WORD PTR [OLD13],BX     ; and instead of these
        MOV     WORD PTR [OLD13+2],ES   ; hang on the new routine
	MOV     DX,OFFSET NEW13
	MOV     AX,2513H
	INT     21H
	MOV     AX,3540H                ; The pure diskette interrupt
	INT     21H                     ; will not be forgetten for safety reasons!
	MOV     WORD PTR [OLD40],BX
	MOV     WORD PTR [OLD40+2],ES
	MOV     DX,OFFSET NEW40
	MOV     AX,2540H
	INT     21H
;(#-9-#)

        MOV     AX,3515H		    ; save INT 15h (together) with
        INT     21H                 ; Move Block and Extended Size  ...
	MOV     WORD PTR [OLDINT15],BX
	MOV     WORD PTR [OLDINT15+2],ES
        MOV     DX,OFFSET NEW15             ; ... hang up instead new routine
        MOV     AX,2515H
	INT     21H
;
; Adjust table (this programpart takes over the "grateful" task
; to play DDL (Dynamic Linking Loader)

        MOV     CX,4                    ; Totally 4 GDT-Entries have to
        MOV     SI,OFFSET GDT           ; be processed.
        MOV     DX,SEG MONDATA             ; Calculate beginning of DATA-Segment
	MOVZX   EDX,DX
	SHL     EDX,4
GDT_LOOP:
        MOVZX   EBX,WORD PTR [SI+2]	; Read base-address of the Segment
	AND     EBX,EBX                 ; Null ? Then turn into
	JZ      SHORT GDT_LOOP_CONT
	ADD     EBX,EDX
        MOV     [SI+2],BX		; write the Beginning (15..0)
	SHR     EBX,16
        MOV     [SI+4],BL		; Write Bits 23..16
GDT_LOOP_CONT:
        ADD     SI,8                    ; Process next/following entry
	LOOP    GDT_LOOP

	ADD     DWORD PTR [IDT_PTR+2],EDX; Situation of IDT & GDT must
	ADD     DWORD PTR [GDT_PTR+2],EDX; in any case be adjusted.

	MOV     WORD PTR [OFFSET LDT+(V86_DATA_SEL AND 0F8H)+2],DX
	SHR     EDX,16
	MOV     BYTE PTR [OFFSET LDT+(V86_DATA_SEL AND 0F8H)+4],DL

	MOV     DX,SEG RESCODE           ; Register the state of the residenten Code-
	MOVZX   EDX,DX			 ; segments.
	SHL     EDX,4
	MOV     WORD PTR [OFFSET GDT+(REAL_SEL AND 0F8H)+2],DX
	SHR     EDX,16
	MOV     BYTE PTR [OFFSET GDT+(REAL_SEL AND 0F8H)+4],DL

	MOV     DX,SEG V86
	MOVZX   EDX,DX
	SHL     EDX,4
	MOV     WORD PTR [OFFSET LDT+(V86_CODE_SEL AND 0F8H)+2],DX
	SHR     EDX,16
	MOV     BYTE PTR [OFFSET LDT+(V86_CODE_SEL AND 0F8H)+4],DL

        MOV     DX,SEG RES_STACK           ; In any case adapt Stack-Segment
	MOVZX   EDX,DX
	SHL     EDX,4
;	MOV     WORD PTR [OFFSET LDT+(V86_STACK_SEL AND 0F8H)+2],DX
	MOV     WORD PTR [OFFSET GDT+(V86_STACK_SEL AND 0F8H)+2],DX
	SHR     EDX,16
;	MOV     BYTE PTR [OFFSET LDT+(V86_STACK_SEL AND 0F8H)+4],DL
	MOV     BYTE PTR [OFFSET GDT+(V86_STACK_SEL AND 0F8H)+4],DL

	JMP     FAR PTR GO_PROTECTED ; It goes !

ERR_EX: MOV     AH,9
	INT     21H
	jmp fail_driver

;
; In case the switch was succesfull, the remaining commands are
; already executed in virtual 8086-mode

	public KEEP
KEEP:
	mov     ax, DGROUP
	mov     ds,ax
	mov     ss,ax
	mov     sp, offset DGROUP:exe_stacktop


	call  _finishing_touches		; do some postprocessing work

@@IGNORE:
;(*-10-*)
        MOV     AX,OFFSET DRIVERCODE:DATA_END   ; End of resident data range

        add ax,200h                             ; TOM writes -  this is complete bullshit
						; however EXTENSIVE debugging shows, that
						; some bytes after this are later used,
						; probably in the disk transfer area
						; to reproduce, remove line and start with
						; SHELL=
						; you want be very happy %-)
						; this was caused by having not
						; V86_TOS bytes for resident stack


	mov cs:[driverret],ax			; we want that many bytes remaining


						; 1500 byte remaining, return to DOS
;		call PRINT_DEZ

;		MOV     AX,SEG DATA          ; Install data segment
;		MOV     DS,AX

;		MOV     DX,OFFSET MSGF           ; give error message...
;		MOV     AH,9
;		INT     21H


driver_exit:
	mov ss,cs:[driverss]
	mov sp,cs:[driversp]

	mov ax,cs:[driverret]
	retf

fail_driver:
	MOV     AX,SEG MONDATA          ; Install data segment
	MOV     DS,AX

        MOV     DX,OFFSET MSGFail    ; Give/Show/Return errormessage
	MOV     AH,9
	INT     21H

	jmp driver_exit

go_driver ENDP


;
; check, if this program runs after all on a 386-Computer (o.ae.)
; (... you never know)
;
public _IS386
_IS386 PROC NEAR
        PUSHF                        ; save Flags
        XOR     AX,AX                ; first ueberpruefen, if a8086/88
        PUSH    AX                   ; this program abarbeitet, since
        POPF                         ; the PUSHF-command always sets
        PUSHF                        ; the bits 15..12 to 1, the 386
        POP     AX                   ; sets Bit 15 always to Null (0?).
	TEST    AH,80H
	JNZ     SHORT @@NO_386
        MOV     AX,7000H             ; The IOPL-value remains on a 80386
        PUSH    AX                   ; also kept after a POPF
        POPF                         ; a 286 always sets it to Null
	STI
	PUSHF
	POP     AX
	TEST    AX,7000H
	JZ      SHORT @@NO_386
	POPF
	mov ax,1					; OK
	RET
@@NO_386:
	popf
	xor ax,ax
	ret
_IS386 ENDP

public _ISPROTECTEDMODE
_ISPROTECTEDMODE proc near
	SMSW    AX
        AND    AX,0001H             ; PE-Bit (Protect Enable) set ?
	ret
_ISPROTECTEDMODE ENDP

;(*-11-*)
;
; Print decimal in AX
;
PRINT_DEZ PROC NEAR
        MOV     BX,10                ; Base (is) 10
        XOR     CX,CX                ; Counter for the amount of digits
@@DIV:  XOR     DX,DX                ; by division & Modulo determine
        DIV     BX                   ; the digits backwards ...
        PUSH    DX                   ; ... and put them on the stack
        AND     AX,AX                ; Something left somewhere?
	LOOPNZ  @@DIV
        NEG     CX                   ; Gives the number of digits
@@PRINT:
        POP     DX                   ; get the single Ziffern again
        ADD     DL,'0'               ; from Stack (now in the
        MOV     AH,2                 ; correct row order)
        INT     21H                  ; and print
	LOOP    @@PRINT
	RET
PRINT_DEZ ENDP
;(*-11-*)

_TEXT ENDS
;
; Stack that's needed shortly after the Installation
;
TMP_STACK SEGMENT PARA USE16
                DB      100H DUP (0)         ; Temporary stack
TMP_TOS EQU     $
TMP_STACK ENDS

_STACK	segment
	my_stack db 1024 dup(?)       ; stack for the C - things

	public exe_stacktop
exe_stacktop dw 0

_STACK	ends


		END     GO
