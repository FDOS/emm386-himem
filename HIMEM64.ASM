;******************************************************************************
;       F R E E - D O S     X M S - D R I V E R
;******************************************************************************
; Written by Till Gerken for the Free-DOS project.
;
; major rework by tom ehlert
; modified for >64M support, Michael Devore
;
; If you would like to use parts of this driver in one of your projects, please
; check up with me first.
;
; I can be reached at:  Till.Gerken@ngosub0.ngo.ol.ni.schule.de (Internet)
;           2:2426/2190.16 (FidoNet)
;
; For questions concerning Free-DOS, mail the coordinator
; Morgan "Hannibal" Toal <hannibal@iastate.edu>
;
; Comments and bug reports are always appreciated.
;
; Copyright (c) 1995, Till Gerken
;******************************************************************************
; -- ORIGINAL IMPLEMENTATION NOTES --
;
; - The Protected Mode handling is very simple, it fits only for the least
;   needs
; - I didn't care about reentrancy. If this thing should be used in
;   Multitasking Environments, well, somebody got to take a look at the code.
; - INT15h, Func. 87h (Move Block) has been re-implemented to preserve
;   the state of A20. (very slow!)
; - INT15h, Func. 88h (Extended Memory Size) has been re-implemented to
;   return 0.
; - Function 0Bh (Move Block) uses it's own Protected Mode handling.
;   It doesn't provide Interrupt windows.
; - The code is not very optimised, I just wrote it down for now. Later, when
;   everything is tested, I'm going to see what can be done
; - Some ideas were taken from the original XMS driver written by
;   Mark E. Huss (meh@bis.adp.com), but everything has been completely
;   rewritten, so if there are bugs, they are mine, not his. ;)
;******************************************************************************
; -- NEW IMPLEMENTATION NOTES --
;
; modified for >64M support, Michael Devore, Nov 2003 - Apr 2004
;  added support for API functions 88h, 89h, 8eh, and 8fh
;  fixed various bugs
;  added /NOABOVE16 support to match Microsoft's HIMEM.SYS
;  added /X support to match Microsoft's HIMEM.SYS
;  Michael Devore's changes are not copyrighted and are released to the public
;   domain. This does not affect copyright on the rest of the code.
;
; - major rework done by tom ehlert, fixed
;
; - reported XMS version  reflects current implementation - 2.0
; - reported XMS internal version reflects driver version - 0.5
;
; - xms_free_handle now actually works
; - added support for protected mode operation
; - many more checks for valid requests (like valid handles,valid offsets)
; - major code cleaning
; - has chances to work in multitasking environments (not tested)
;
; - although the code is based on Till gerkens, much has been changed.
;   so the bugs are no longer due to Till Gerkens, but due to
;   tom.ehlert (tom.ehlert@ginko.de)
;
; - Changes 2004/24/9 (Aitor SANTAMARIA MERINO)
;   Minor changes to commandline (HMAMIN, TESTMEM, /?, suffixed values),
;   Rebuild the package with the HIMEM64, the HELP file, history, etc
;   Test less verbose for emulating MS-HIMEM
;   New test_a20 proc based on Undocumented PC (saves 12 bytes)
;
; still missing
;    support for 80286 (mostly due to long arith with 32 bit registers)
;      (no such support is planned)
;
;******************************************************************************
;
;        fixed bug in xms_realloc_xms that caused lst handles
; version 0.6 - nov 2001
;
;        if someone ever decides to disable A20, EMM386 will crash
;        nearly immediately. so the code to disable A20 was disabled itself.
;        as Bart disagreed ( tom still thinks A20 is bullshit, referring to
;        nonexisting software written 20 years ago)
;        it was reenabled and marked with ALLOWDISABLEA20. this costs 
;        60 bytes of precious memory ;-)
;        EMM386 will void this call himself
;
;       added "C" commandline parsing
;       compatibility with free EMM386 (lmsw ax <--> mov eax,cr0)
;       added EXE capability
;       merged TEST functionality from XMSTEST
;       
;******************************************************************************


ALLOWDISABLEA20 equ 1



_TEXT   segment byte public 'CODE'
_TEXT   ends

_DATA   segment byte public 'DATA'
_DATA   ends


_BSS    segment word public 'BSS'
_BSS    ends


_STACK  segment STACK 'STACK'
    my_stack db 1024 dup(?)       ; stack for the C - things
            
      public driver_stacktop      
;label driver_stacktop  ; not all TASMs like this
driver_stacktop:        ;  but they think this is okay
_STACK  ends


DGROUP  group   _TEXT,_DATA, _BSS,_STACK

assume  cs:DGROUP,ds:DGROUP


        extrn _startup_driver:far
        extrn _startup_exe:far




_TEXT   segment


ideal                   ; switch on ideal mode syntax
P386                    ; 386 instructions this time
;jumps


; MANIFEST constants

; XMS driver is now version 3.00 for >64M support
;DRIVER_VERSION      equ '2.07'   ; revision number
;DRIVER_VER          =   0207h   ; some (EMU86) software wants it that way
;INTERFACE_VERSION   equ '2.00'  ; like HIMEM 2.77 - we hope :-)
;INTERFACE_VER       =   200h
DRIVER_VERSION      equ '3.26'
DRIVER_VER          =   0326h 
INTERFACE_VERSION   equ '3.00'
INTERFACE_VER       =   300h

XMS_START           =   1088    ; XMS starts at 1088k after HMA

CMD_INIT            =   0   ; init command (used when installed)

STATUS_OK           =   0100h   ; driver is initialized and ok
STATUS_BAD          =   8000h   ; driver couldn't be installed

VDISK_IDSTR         equ "VDISK"
VDISK_IDLEN         =   5
VDISK_IDOFS         =   13h

XMS_NOT_IMPLEMENTED             =   80h
XMS_VDISK_DETECTED              =   81h
XMS_A20_FAILURE                 =   82h
XMS_DRIVER_FAILURE              =   8eh
XMS_DRIVER_FATAL                =   8fh
XMS_HMA_NOT_THERE               =   90h
XMS_HMA_IN_USE                  =   91h
XMS_HMAREQ_TOO_SMALL            =   92h
XMS_HMA_NOT_USED                =   93h
XMS_A20_STILL_ENABLED           =   94h
XMS_ALREADY_ALLOCATED           =   0a0h
XMS_NO_HANDLE_LEFT              =   0a1h
XMS_INVALID_HANDLE              =   0a2h
XMS_INVALID_SOURCE_HANDLE       = 0a3h
XMS_INVALID_SOURCE_OFFSET       = 0a4h
XMS_INVALID_DESTINATION_HANDLE  = 0a5h
XMS_INVALID_DESTINATION_OFFSET  = 0a6h
XMS_INVALID_LENGTH              =   0a7h
XMS_INVALID_OVERLAP             =   0a8h
XMS_PARITY_ERROR                =   0a9h
XMS_BLOCK_NOT_LOCKED            =   0aah
XMS_BLOCK_LOCKED                =   0abh
XMS_LOCK_COUNT_OVERFLOW         =   0ach
XMS_LOCK_FAILED                 =   0adh
XMS_ONLY_SMALLER_UMB            =   0b0h
XMS_NO_UMB_AVAILABLE            =   0b1h
XMS_UMB_SEGMENT_NR_INVALID      =    0b2h


struc   request_hdr
  req_size  db  ?       ; number of bytes stored
  unit_id   db  ?       ; unit ID code
  cmd       db  ?       ; command code
  status    dw  ?       ; status word
  rsvd      db  8 dup (?)   ; reserved
ends    request_hdr

struc   init_strc
  init_hdr  db  size request_hdr dup (?)
  units     db  ?       ; number of supported units
  end_addr  dd  ?       ; end address of resident part
  cmd_line  dd  ?       ; address of command line
ends    init_strc

struc   desc
  limit     dw  0ffffh      ; segment limit
  base0_15  dw  ?       ; low word of base address
  base16_23 db  ?       ; high byte of base address
  flags     db  93h     ; std read/write segment
  reserved  db  0
ends    desc

; >64M support
struc   xms_handle
;  xbase     dw  ?       ; base address in kbytes
;  xsize     dw  ?       ; size in kbytes
  used      db  ?       ; 2 if used, 1 if not
  locks     db  ?       ; lock count
  xbase     dd  ?       ; base address in kbytes
  xsize     dd  ?       ; size in kbytes
ends    xms_handle

struc	stxms_handle_table
xtid		DB	?		; identifier byte?, 1 for MS-DOS HIMEM, 0FDH for us
desc_size	DB	?		; size of handle descriptor (xms_handle)
num_handles	DW	?		; number of handles
ptr_handles	DD	?		; pointer to XMS handles array
ends	stxms_handle_table

struc   xms_move_strc
  len       dd  ?       ; block length in bytes
  src_handle    dw  ?       ; source handle
  src_offset    dd  ?       ; offset into source
  dest_handle   dw  ?       ; destination handle
  dest_offset   dd  ?       ; offset into destination
ends    xms_move_strc


SMAP    =    534d4150h
struc e820map_struc
    baselow dd  ?
    basehigh    dd  ?
    lenlow  dd  ?
    lenhigh dd  ?
    type    dd  ?
ends


;******************************************************************************
; 16-bit resident code and data
;******************************************************************************


;******************************************************************************
; device driver header

        dd  -1          ; last driver in list
        dw  8000h           ; driver flags
        dw  offset strategy     ; pointer to strategy routine
        dw  offset interrupt    ; pointer to interrupt handler
        db  'XMSXXXX0'      ; device driver name

;******************************************************************************
; global data

request_ptr dd  ?           ; pointer to request header


;xms_size    dw  ?           ; size of XMS in kbytes
xms_size    dd  ?           ; size of XMS in kbytes

gdt32       dw  gdt_size,dummy,0
dummy       dq  0
code16dsc   db  0ffh,0ffh,0,0,0,9ah,0,0 ; 16-bit execute/read code, 64K
core32dsc   db  0ffh,0ffh,0,0,0,92h,0cfh,0  ; 32-bit read/write data, 4G
gdt_size=$-(offset dummy)

code16idx   =   08h
core32idx   =   10h

old_int15   dd  ?           ; old INT15h vector
old_int2f   dd  ?           ; old INT2fh vector

hma_used    db  0           ; set if HMA is used
	public  _hma_min
_hma_min     dw  0           ; minimal space in HMA that
                            ; has to be requested
a20_locks   dw  0           ; internal A20 lock count

xms_handle_start dw normal_driver_end

    public _xms_num_handles 
_xms_num_handles dw  72      ; number of available handles

xms_handle_table	stxms_handle_table	<0fdh, size xms_handle, 72, 0>

	public _xms_logging_enabled
_xms_logging_enabled db  0      ; debugging output 

	public _x2max32
_x2max32		db  1			; maximum XMS 2.0 free/available 32M

;delay2ptr	DW	(OFFSET delay2)


;******************************************************************************
; strategy routine. is called by DOS to initialize the driver once.
; only thing to be done here is to store the address of the device driver
; request block.
; In:   ES:BX - address of request header
; Out:  nothing

proc    strategy    far
    mov [word cs:request_ptr+2],es  ; store segment addr
    mov [word cs:request_ptr],bx    ; store offset addr
    ret                 ; far return here!
endp    strategy

;******************************************************************************
; interrupt routine. called by DOS right after the strategy routine to
; process the incoming job. also used to initialize the driver.

proc    interrupt   far

label init_patch  byte           ; will be overwritten by
                                ; NOP's after init
    call near init_interrupt

    ret                 ; far return here!
endp    interrupt

;******************************************************************************
; just delays a bit


MASM	; have to switch to masm mode to use COMMENT blocks
COMMENT !
; *** OLD CODE ***
;proc   delay
;   jmp short $+2
;   jmp short $+2
;   ret
;endp   delay
; *** OLD CODE ***

proc delay
delay_start:
     in al, 64h
     jmp delay_check
delay_check:
     and al, 2
     jnz delay_start
     ret
endp delay


;******************************************************************************
; empties the keyboard processor's command queue

; *** OLD CODE ***
;proc   empty_8042
;   call    delay           ; delay a bit
;   in  al,64h
;   test    al,1            ; is there something to be read?
;   jz  no_output       ; no, go on
;   call    delay           ; yes, first delay a bit
;   in  al,60h          ; then read the output buffer
;   jmp short empty_8042    ; and try again
;no_output:
;   test    al,2            ; has it finished processing?
;   jnz empty_8042      ; no, try again
;   ret             ; yes, done
;endp   empty_8042
; *** OLD CODE ***

;******************************************************************************
; enables the A20 address line

proc enable_a20
	push ax
	mov  ah,2
	jmp short disable_enable_a20

disable_a20:
	push ax
	mov  ah,0

disable_enable_a20:
     mov al,0d1h
     out 64h,al
     call delay
     mov al,0ddh		; or df=dd+2
     or  al,ah
     out 60h,al
     call delay
     mov al,0ffh
     out 64h,al
     call delay
     pop ax
     ret
endp enable_a20
DB	40 DUP (?)

disable_enable_a20_end:

;******************************************************************************
; disables the A20 address line

;proc disable_a20
;
;	 push ax
;     mov al,0d1h
;     out 64h,al
;     call delay
;     mov al,0ddh
;     out 60h,al
;     call delay
;     mov al,0ffh
;     out 64h,al
;     call delay
;     pop ax
;     ret
;endp disable_a20
disable_a20end:

;******************************************************************************
; tests if the A20 address line is enabled.
; compares 256 bytes at 0:0 with ffffh:10h
; Out:  ZF=0 - A20 enabled
;   ZF=1 - A20 disabled

proc    test_a20
    push    cx si di ds es

    xor si,si
    mov ds,si

    mov di,0ffffh
    mov es,di
    mov di,10h

    mov cx,100h/4
    rep cmpsd

    pop es ds di si cx
    ret
endp    test_a20

;
; new method to test A20
;
; idea: compare my own memory with something which might be 
; at 100000:xxxx if A20 is enabled
; or  00000:xxxx if A20 is disabled
;
; if not equal --> A20 enabled
; else 
;     increment low memory
;     compare again with high memory
;     if not equal --> A20 enabled
;     else         -->
;
; assumes CS:offset  is lies in memory 0:FFFF
; AND code is loaded below e00:0
; both only true for kernel or driver code
;


; FFFF:10  == 0:0
; FFFF+400:10 == 400:0
; F800+400:7FF0+10 == 400:0
; F800+seg mylowmemoryval: 7ff0+10+offset mylowmemoryval == cs:[mylowmemoryval]
; F200+seg mylowmemoryval: e000+offset mylowmemoryval == cs:[mylowmemoryval]
           
;public lowmemoryval           
;lowmemoryval  dw 0
;highmemoryseg dw 0f200h + seg lowmemoryval

;testa20_new:

;   push ds

;   lds ax, [cs:offset lowmemoryval]    
    
;   cmp ax, [offset lowmemoryval + 0e000h]
;   jne enabled

;   inc [cs:lowmemoryval]
;   inc ax
    
;   cmp ax, [offset lowmemoryval + 0e000h]

;enabled:
;   pop ds
;   ret

proc delay2
	jmp	delay2_wait
delay2_discard:
	in	al,60h		; read data register, throw away info
	jmp	delay2_wait	; very short delay
delay2_wait:
	in	al,64h		; read status register
	test	al,1	; check if output buffer full
	jne	delay2_discard	; yes, read info and throw away
	and	al,2		; check if input buffer full
	jnz	delay2_wait	; yes, keep waiting
	ret
endp delay2
END COMMENT!       
IDEAL

;******************************************************************************
; enables the A20 address line
; currently dummy/always on, code replaced as A20 tests indicate

proc enable_a20
	push ax
	mov  ah,2
	jmp short disable_enable_a20

disable_a20:
	push ax
	mov  ah,0

disable_enable_a20:

	pop	ax
	ret
endp enable_a20

; since this is replaceable, we need to bulk up the space allocated for it
;  for larger replacement routines
DB	66 DUP (?)

disable_enable_a20_end:

;******************************************************************************
; simpler and more accurate A20 enabled test
;  borrowed from public domain code
; Out:  ZF=0 - A20 enabled
;   ZF=1 - A20 disabled

proc    test_a20
	push	ax
	push	dx
	push	ds
	push	es
	pushf			; save original flags (IF is important)

	cli				; ensure interrupts are off while testing
	xor	ax,ax
	mov	ds,ax
	dec	ax
	mov	es,ax		; es->FFFF seg
	mov	ax,[es:10h]	; read word at FFFF:10h, the 1M limit
	not	ax			; ~1M word
	push	[WORD 0]	; save word we're changing (INT 0 offset)
	mov	[0],ax		; save ~1M word to 0:0 (and FFFF:10h if A20 disabled)
	mov	ax,[0]		; read back, may be unnecessary (forced memory access?)
	cmp	ax,[es:10h]	; compare 0:0 ~1M word to 1M word, only equal if A20 is disabled
	pop	[WORD 0]	; restore INT 0 offset

	pushf
	pop	ax			; get new flags in ax
	and	al,40h		; only interested in ZF
	pop	dx			; dx holds original flags
	and	dl,NOT 40h	; clear original ZF
	or	dl,al		; dx holds original flags except new ZF
	push	dx
	popf			; original flags reflect new ZF

	pop	es
	pop	ds
	pop	dx
	pop	ax
	ret
endp    test_a20


;******************************************************************************
; (ASM: 2004/09/24: yet another test)
; This is based on similar ideas than the above test, but it's shorter (12 bytes)
; borrowed from "The Undocumented PC", Frank Van Gilluwe
; (a little bit adapted)
; Out:  ZF=0 - A20 enabled
;   ZF=1 - A20 disabled
;
;
;proc    test_a20
;	push	ax
;	push	di
;	push	si
;	push	ds
;	push	es
;	pushf			; save original flags (IF is important)
;
;	cli				; ensure interrupts are off while testing
;	mov	di,10
;	mov	ax,0ffffh
;	mov	es,ax
;	xor	ax,ax
;	mov	si,ax
;	mov	ds,ax
;	not	word ptr ds:[si]	; TASM doesn't like this (issues warning)
;	cld
;	cmpsw
;	pushf			; save the result
;	not	word ptr ds:[si]	; TASM doesn't like this (issues warning)
;
;	pop	si
;	and	si,0040h
;	pop	ax
;	or	ax,si
;	push	ax
;	popf
;	
;	pop	es
;	pop	ds
;	pop	si
;	pop	di
;	pop	ax
;	ret
;endp    test_a20



;******************************************************************************
; Interrupt handlers
;******************************************************************************
;******************************************************************************
; new INT15h handler
;
; this externally preserves A20 state on function 87h
;

proc    int15_handler
	cmp	ah,87h
	je	do_move
    cmp ah,88h              ; is it a ext. mem size req.?
    je  ext_mem_size
    jmp [cs:old_int15]          ; jump to old handler

ext_mem_size:
    xor ax,ax               ; no memory available
    clc                 ; no error
    iret

do_move:
	push    bx              ; use bx because ax carries return value
	call	test_a20
        setz    bl              ; save ZF in bl

call_old_mover:
	pushf
	call	[cs:old_int15]	; simulated INT

	and     bl,bl
	jne	do_disable
	call	enable_a20
	jmp	move_done

do_disable:
	call	disable_a20

move_done:
        pop     bx
        iret


endp    int15_handler

;******************************************************************************
; new INT2Fh handler. Catches Func. 4300h+4310h

public  int2f_handler
proc    int2f_handler
    pushf
    cmp ah,43h
    je maybe_my2f
jmp_old2f:
    popf
    jmp [cs:old_int2f]          ; jump to old handler

maybe_my2f:
    cmp al,00h            ; is it "Installation Check"?
    jne get_driver_address
    mov al,80h              ; yes, we are installed ;)
    popf
    iret
get_driver_address:
    cmp al,10h            ; is it "Get Driver Address"?
    jne get_xms_handle_table
    mov bx,offset xms_dispatcher
shared2f:
    push cs
    pop  es
    popf
    iret
get_xms_handle_table:
    cmp al,9			; is it get xms handle table?
    jne jmp_old2f
	mov	al,43h
	mov	bx,OFFSET xms_handle_table
	jmp	shared2f

endp    int2f_handler

;******************************************************************************
; XMS functions
;******************************************************************************
; returns XMS version number
; In:   AH=0
; Out:  AX=XMS version number
;   BX=internal revision number
;   DX=1 if HMA exists, 0 if not

proc    xms_get_version
    mov ax,INTERFACE_VER
    mov bx,DRIVER_VER
    mov dx,1                ; HMA is always available
    ret
endp    xms_get_version

;******************************************************************************
; requests HMA
; In:   AH=1
;   DX=space needed in HMA (0ffffh if application tries to request HMA)
; Out:  AX=1 if successful
;   AX=0 if not successful
;     BL=80h -> function not implemented (implemented here ;) )
;     BL=81h -> VDISK is detected
;     BL=90h -> HMA does not exist
;     BL=91h -> HMA already in use
;     BL=92h -> DX less than HMA_MIN

proc    xms_request_hma
    cmp [hma_used],0            ; is HMA already used?
    mov bl,XMS_HMA_IN_USE
    jnz xrh_err
    cmp dx,[_hma_min]            ; is request big enough?
    mov bl,XMS_HMAREQ_TOO_SMALL
    jb  xrh_err
    mov [hma_used],1            ; assign HMA to caller
    mov ax,1
    xor bl,bl
    ret;return_success
xrh_err:
    xor ax,ax
    ret;return_failure
endp    xms_request_hma

;******************************************************************************
; releases HMA
; In:   AH=2
; Out:  AX=1 if successful
;   AX=0 if not successful
;     BL=80h -> function not implemented
;     BL=81h -> VDISK is detected
;     BL=90h -> HMA doesn't exist
;     BL=93h -> HMA wasn't allocated

proc    xms_release_hma
    cmp [hma_used],0            ; is HMA used?
    mov bl,XMS_HMA_NOT_USED
    jz  xrh_err
    mov [hma_used],0            ; now release it
    mov ax,1
    xor bl,bl
    ret;return_success
endp    xms_release_hma

;******************************************************************************
; global A20 address line enable
; In:   AH=3
; Out:  AX=1 if successful
;   AX=0 if not successful
;     BL=80h -> function is not implemented
;     BL=81h -> VDISK is detected
;     BL=82h -> A20 failure

proc    xms_global_enable_a20
    call    enable_a20          ; enable A20
    call    test_a20            ; is it really enabled?
    jz  xge_a20_err

xge_success:
    mov ax,1
    xor bl,bl
    ret;return_success
xge_a20_err:
    xor ax,ax
    mov bl,XMS_A20_FAILURE
    ret;return_failure
endp    xms_global_enable_a20

;******************************************************************************
; global A20 address line disable
; In:   AH=4
; Out:  AX=1 if successful
;   AX=0 if not successful
;     BL=80h -> function is not implemented
;     BL=81h -> VDISK is detected
;     BL=82h -> A20 failure
;     BL=84h -> A20 still enabled

proc    xms_global_disable_a20

IF ALLOWDISABLEA20
    call    disable_a20         ; disable A20

    call    test_a20            ; is it really disabled?
    jnz xge_a20_err
    mov ax,1
    xor bl,bl
    ret;return_success
endif    

        xor ax,ax
        mov bl,84h                  ; A20 still enabled
                                    ; or function not implemented ??        
        ret;return_failure



endp    xms_global_disable_a20

;******************************************************************************
; enables A20 locally
; In:   AH=5
; Out:  AX=1 if A20 is enabled, 0 otherwise
;   BL=80h -> function not implemented
;   BL=81h -> VDISK is detected
;   BL=82h -> A20 failure

proc    xms_local_enable_a20
    inc [a20_locks]             ; increase lock counter
    call    test_a20            ; test if it's really enabled

;    jnz local_enable_was_enabled
    jnz xge_success
	jmp	xms_global_enable_a20

;    call    enable_a20          ; enable it
;    call    test_a20            ; test if it's really enabled
;    jz  xge_a20_err
;local_enable_was_enabled:
;    mov ax,1
;    xor bl,bl
;    ret;return_success

endp    xms_local_enable_a20

;******************************************************************************
; disables A20 locally
; In:   AH=6
; Out:  AX=1 if A20 is disabled, 0 otherwise
;   BL=80h -> function not implemented
;   BL=81h -> VDISK is detected
;   BL=82h -> A20 failure

proc    xms_local_disable_a20
    dec [cs:a20_locks]          ; decrease lock counter
    jnz xld_dont_disable        ; disable only if needed

IF ALLOWDISABLEA20    
    call    disable_a20         ; disable it
ENDIF    

    call    test_a20            ; test if it's really disabled
    jnz xge_a20_err
xld_dont_disable:
    mov ax,1
    xor bl,bl
    ret;return_success

endp    xms_local_disable_a20

;******************************************************************************
; returns the state of A20
; In:   AH=7
; Out:  AX=1 if A20 is physically enabled, AX=0 if not
;   BL=00h -> function was successful
;   BL=80h -> function is not implemented
;   BL=81h -> VDISK is detected

proc    xms_query_a20
    xor ax,ax           ; suppose A20 is disabled
    call    test_a20
    jz  xqa_a20dis
    mov ax,1
xqa_a20dis:
    xor bl,bl
    ret;return_success
endp    xms_query_a20

;******************************************************************************
; searches a/next free XMS memory block
;; In:   DS=CS -- no longer true
;   BX - offset of start handle (if search is continued)
;   CX - remaining handles (if search is continued)
; Out:  CY=1 - no free block
;     BX - offset of end of handle table
;   CY=0 - free block found
;     BX - offset of free handle
;     CX - number of remaining handles

proc    xms_find_free_block
    mov bx,[xms_handle_start]    ; start at the beginning of the table
    mov cx,[_xms_num_handles]    ; check all handles
find_free_block:
    cmp [bx+xms_handle.used],2  ; is it used?
    jz xms_find_next_free_block	; yes, go on
    cmp [bx+xms_handle.xbase],0 ; assigned memory block or just blank?
    jnz found_block     ; assigned, return it
xms_find_next_free_block:
    add bx,size xms_handle  ; skip to next handle
    loop    find_free_block     ; check next handle
    stc             ; no free block found, error
    ret
found_block:
    clc             ; no error, return
    ret
endp    xms_find_free_block

;******************************************************************************
; searches a/next free XMS memory handle
; In:   DS=CS
;   BX - offset of start handle (if search is continued)
;   CX - remaining handles (if search is continued)
; Out:  CY=1 - no free handle
;     BX - offset of end of handle table
;   CY=0 - free handle found
;     BX - offset of free handle
;     CX - number of remaining handles

proc    xms_find_free_handle
    mov bx,[xms_handle_start]    ; start at the beginning of the table
    mov cx,[_xms_num_handles]    ; check all handles
find_free_handle:
    cmp [bx+xms_handle.used],2      ; is it used?
    jz xms_find_next_free_handle    ; yes, go on
    cmp [bx+xms_handle.xbase],0     ; really blank handle?
    jz  found_handle            ; found a blank handle
xms_find_next_free_handle:
    add bx,size xms_handle  ; skip to next handle
    loop    find_free_handle    ; check next handle
    stc             ; no free block found, error
    ret
found_handle:
    clc             ; no error, return
    ret
endp    xms_find_free_handle

;******************************************************************************
; xms_check_handle
; In:   DS=CS
;   DX - handle to check
;
; Out:  CY=1     - no valid handle
;         BL=0a2h  - XMS_INVALID_HANDLE
;         AX=0     - usual error return
;
;       CY=0     - no error
;
; registers destroyed - AX
;

proc    xms_check_handle
    push dx
    push si

    mov si,dx

    mov ax,dx
    sub ax,[xms_handle_start]    ; start at the beginning of the table
    jb  xms_no_valid_handle
    xor dx,dx

    push bx                     ; what syntax does TASM support to do
    mov  bx,size xms_handle     ; div DX:AX,3 ??
    div bx
    pop bx

    or  dx,dx
    jnz xms_no_valid_handle

    cmp ax,[_xms_num_handles]    ; less then last handle ??
    jae xms_no_valid_handle

    cmp [si+xms_handle.used],2             ; is it in use ??

    jne xms_no_valid_handle

    pop si
    pop dx

    ret

xms_no_valid_handle:
    pop si
    pop dx

    xor ax,ax
    mov bl,XMS_INVALID_HANDLE
    stc
    ret

endp    xms_check_handle



;******************************************************************************
; query any free extended memory
; In:   AH=88h
; Out:  EAX=size of largest free XMS block in kbytes
;   ECX=highest ending address of any memory block
;   EDX=total amount of free XMS in kbytes
;   BL=0 if ok
;   BL=080h -> function not implemented
;   BL=081h -> VDISK is detected
;   BL=0a0h -> all XMS is allocated

proc    xms_ext_query_free_xms

; find highest address, doesn't matter if about block allocation status
ext_findhigh:
    xor edi,edi     ; highest ending address of any memory block
    mov bx,[xms_handle_start]    ; start at the beginning of the table
    mov cx,[_xms_num_handles]    ; check all handles

ext_highloop:
    test [bx+xms_handle.used],7	; check if flagged free or in use
	je	ext_nexthigh	; nope
    mov esi,[bx+xms_handle.xbase]
    add esi,[bx+xms_handle.xsize]
    cmp edi,esi
    jae ext_nexthigh
    mov edi,esi     ; higher address, update
ext_nexthigh:
    add bx,size xms_handle  ; skip to next handle
    loop    ext_highloop	; check next handle

    xor eax,eax               ; contains largest free block
    xor edx,edx               ; contains total free XMS

    call    xms_find_free_block     ; search free block
    jc  ext_no_free_xms

ext_check_next:
    mov esi,[bx+xms_handle.xsize]    ; get size

ext_check_update:
    add edx,esi               ; update total amount
    jnc ext_check_larger
    mov edx,-1              ; overflowed, set to highest

ext_check_larger:
    cmp esi,eax               ; check if larger than largest
    jbe ext_not_larger
    mov eax,esi               ; larger, update
ext_not_larger:
    call    xms_find_next_free_block
    jnc ext_check_next

    xor bl,bl

ext88_ret:
    mov ecx,edi     ; highest address to ecx return value
	shl	ecx,10		; convert to bytes
	dec	ecx			; relative zero
	
    ret         ; success

ext_no_free_xms:
    mov bl,XMS_ALREADY_ALLOCATED
;   ret;return_failure
	jmp	ext88_ret

endp    xms_ext_query_free_xms


;******************************************************************************
; returns free XMS
; In:   AH=8
; Out:  AX=size of largest free XMS block in kbytes
;       DX=total amount of free XMS in kbytes
;   BL=0 if ok
;   BL=0a0h -> all XMS is allocated

proc    xms_query_free_xms
	       
	       		; protect high parts 
	push eax
	pop ax
	push ecx
	push edx
	pop dx

	call xms_ext_query_free_xms	
							; returns:
							;   EAX=size of largest free XMS block in kbytes
							;   ECX=highest ending address of any memory block
							;   EDX=total amount of free XMS in kbytes


	cmp edx,0fbc0h			; dx = min(edx,0fbc0)
	jb  edx_not_larger
	mov dx,0fbc0h
edx_not_larger:

	cmp eax,0fbc0h			; ax = min(eax,0fbc0)
	jb  eax_not_larger
	mov ax,0fbc0h
eax_not_larger:

	cmp	[_x2max32],0	; see if limiting XMS 2.0 functions to 32M-1
	je	eax_no_throttle
	cmp edx,7fffh			; dx = min(edx,7fff)
	jb  edx_no_throttle
	mov dx,7fffh
edx_no_throttle:
	cmp eax,7fffh			; ax = min(eax,7fff)
	jb  eax_no_throttle
	mov ax,7fffh
eax_no_throttle:

	       		; restore high parts 
	push dx
	pop edx
	
	pop ecx
	
	push ax
	pop  eax
            
	ret            
endp    xms_query_free_xms

;******************************************************************************
; allocates an XMS block
; In:   AH=9
;   DX=amount of XMS being requested in kbytes
; Out:  AX=1 if successful
;     DX=handle
;   AX=0 if not successful
;     BL=080h -> function not implemented
;     BL=081h -> VDISK is detected
;     BL=0a0h -> all XMS is allocated
;     BL=0a1h -> no free handles left

proc    xms_alloc_xms
    push    edx
    movzx   edx,dx  ; extend alloc request to 32-bits
    jmp xms_alloc2

; 32-bit entry for function 89h, just uses full edx value
xms_ext_alloc_xms:
    push    edx

xms_alloc2:
    push    eax
    push    cx
    push    bx

    call    xms_find_free_block ; see if there's a free block
    jnc check_size      ; if it is, go on

no_free_handle:
    pop bx
    mov bl,XMS_NO_HANDLE_LEFT

alloc_fail:
    pop cx
    pop eax
    pop edx
    xor ax,ax
    ret ; failure

no_free_mem:
    pop bx
    mov bl,XMS_ALREADY_ALLOCATED
    jmp alloc_fail

get_next_block:
    call    xms_find_next_free_block
    jc  no_free_mem
check_size:
    cmp edx,[bx+xms_handle.xsize]    ; check if it's large enough
    ja  get_next_block              ; no, get next block

    mov si,bx           ; save handle address
    mov [bx+xms_handle.used],2  ; this block is used from now on

    call    xms_find_free_handle    ; see if there's a blank handle
    jc  perfect_fit     ; no, there isn't, alloc all mem left
    mov eax,[si+xms_handle.xsize]    ; get size of old block
    sub eax,edx               ; calculate resting memory
    jz  perfect_fit         ; if it fits perfectly, go on
    mov [bx+xms_handle.xsize],eax    ; store sizes of new blocks
    mov [si+xms_handle.xsize],edx

    mov eax,[si+xms_handle.xbase]    ; get base address of old block
    add eax,edx               ; calculate new base address

    mov [bx+xms_handle.xbase],eax    ; store it in new handle
    mov [bx+xms_handle.locks],0     ; no locks on this block

perfect_fit:
    mov [si+xms_handle.locks],0     ; no locks on this block

    pop bx
    xor bl,bl
    pop cx
    pop eax
    mov ax,1
    pop edx
    mov dx,si               ; return handle in DX

    ret;return_success
endp    xms_alloc_xms

;******************************************************************************
; frees an XMS block
; In:   AH=0ah
;   DX=handle to allocated block that should be freed
; Out:  AX=1 if successful
;   AX=0 if not successful
;     BL=080h -> function not implemented
;     BL=081h -> VDISK is detected
;     BL=0a2h -> handle is invalid
;     BL=0abh -> handle is locked

proc    xms_free_xms

    call xms_check_handle       ; check if dx holds handle
    jnc free_valid_handle
    ret;return_failure

free_valid_handle:

    mov si,dx

    cmp [si+xms_handle.locks],0     ; is the block locked?
    jz  not_locked          ; no, go on
    xor ax,ax
    mov bl,XMS_BLOCK_LOCKED
    ret;return_failure


not_locked:
    push    eax
    push    bx
    push    cx
    push    edx

    cmp [si+xms_handle.xsize],0     ; is it a zero-length handle?
    jnz normal_handle
    mov [si+xms_handle.xbase],0     ; blank handle
    jmp xms_free_done

normal_handle:

                                    ; 1) see if the following block is
                                    ;    free, too

    mov eax,[si+xms_handle.xbase]    ; get base address
    add eax,[si+xms_handle.xsize]    ; calculate end-address

    call    xms_find_free_block     ; check free blocks
    jc  xms_free_done_1             ; no, was last handle
try_concat:
    cmp eax,[bx+xms_handle.xbase]    ; is it adjacent to old block?
    jne not_adjacent
    mov edx,[bx+xms_handle.xsize]    ; concat
;    add ax,dx  ; unused?
    add [si+xms_handle.xsize],edx
    mov [bx+xms_handle.xbase],0     ; blank handle
    mov [bx+xms_handle.xsize],0
not_adjacent:
    call    xms_find_next_free_block    ; see if there are other blks
    jnc try_concat


xms_free_done_1:
                                    ; 2) see if the previous block is
                                    ;    free, too

xms_free_loop_2:
    call    xms_find_free_block     ; check free blocks
    jc  xms_free_done               ; no, was last handle


try_concat_2:
    mov eax,[bx+xms_handle.xbase]    ; is it adjacent to old block?
    add eax,[bx+xms_handle.xsize]    ;
    cmp eax,[si+xms_handle.xbase]    ;
    jne not_adjacent_2

    mov eax,[si+xms_handle.xsize]    ; concat
    add [bx+xms_handle.xsize],eax
    mov [si+xms_handle.xbase],0     ; blank handle
    mov [si+xms_handle.xsize],0
    mov [si+xms_handle.used],1      ; handle isn't used anymore
    mov si,bx
    jmp xms_free_loop_2             ; restart
not_adjacent_2:
    call    xms_find_next_free_block    ; see if there are other blks
    jnc try_concat_2

xms_free_done:
    mov [si+xms_handle.used],1      ; handle isn't used anymore
    pop edx
    pop cx
    pop bx
    pop eax
    mov ax,1
    xor bl,bl
    ret;return_success
endp    xms_free_xms

;******************************************************************************
; calculates the move address
; In:   BX - handle (0 if EDX should be interpreted as seg:ofs value)
;   EDX - offset
; Out:  EBX - absolute move address
; Modifies: ECX, EDX

proc    xms_get_move_addr
    or  bx,bx           ; translate address in EDX?
    jnz dont_translate  

                        ; its segment:offset in EDX
    
                        ; ebx = 16*(edx high) + dx
    movzx   ebx,dx          ; save offset
    xor dx,dx           ; clear lower word
    shr edx,12          ; convert segment to absolute address
    add ebx,edx         ; add offset


    mov eax,ebx                 ; check that ebx(address) + ecx (length) is <= 10fff0
    add eax,ecx                 ; 
    jc wrong_size2              ; negative length might wrap
    cmp eax,10fff0h
    ja wrong_size2

    clc
    ret

    
dont_translate:         ; its a handle:offset pair
    push dx
    mov  dx,bx
    call xms_check_handle
    pop  dx
    jnc  get_move_addr_1
                        ; no valid handle
    ret                 ; return with carry set

get_move_addr_1:
    push ecx            ; contains length

    add ecx,edx         ; assert length + offset < size    
    jc  wrong_size      ; probably negative length
    add ecx,1024-1      ;
    jc  wrong_size      ; probably negative length

    shr ecx,10          
    cmp ecx,[bx+xms_handle.xsize]    ; compare with max offset
    ja wrong_size
    pop ecx

    mov ebx,[bx+xms_handle.xbase]   ; get block base address
    shl ebx,10              ; convert from kb to absolute

    add ebx,edx             ; add offset into block
    ret

wrong_size:
    pop ecx
wrong_size2:

    mov bl,XMS_INVALID_LENGTH
    xor ax,ax
    stc
    ret
endp    xms_get_move_addr

;******************************************************************************
; moves an XMS block
; In:   AH=0bh
;   DS:SI=pointer to XMS move structure
; Out:  AX=1 if successful
;   AX=0 if not successful
;     BL=080h -> function not implemented
;     BL=081h -> VDISK is detected
;     BL=082h -> A20 failure
;     BL=0a3h -> source handle is invalid
;     BL=0a4h -> source offset is invalid
;     BL=0a5h -> destination handle is invalid
;     BL=0a6h -> destination offset is invalid
;     BL=0a7h -> length is invalid
;     BL=0a8h -> move has invalid overlap
;     BL=0a9h -> parity error

proc    xms_move_xms
    push    ecx edx
    push    eax ebx



    call    test_a20            ; get A20 state
    pushf                   ; save it for later
    jnz was_enabled
    call    enable_a20          ; now enable it! - if it was disabled
was_enabled:

    mov ecx,[es:si+xms_move_strc.len]   ; get length
    test    cl,1                ; is it even?
    jnz move_invalid_length

    mov bx,[es:si+xms_move_strc.dest_handle]
    mov edx,[es:si+xms_move_strc.dest_offset]
    call    xms_get_move_addr       ; get move address
    jc  copy_dest_is_wrong
    mov edi,ebx             ; store in destination index

    mov bx,[es:si+xms_move_strc.src_handle]
    mov edx,[es:si+xms_move_strc.src_offset]
    call    xms_get_move_addr       ; get move address
    jc  copy_source_is_wrong

    mov esi,ebx                 ; store in source index

;**************************************************
; setup finished with
;   ESI = source
;   EDI = destination
;   ECX = number of words to move
;
; now we must check for potential overlap
;**************************************************

    or  ecx,ecx                 ; nothing to do ??
    jz  xms_exit_copy

    cmp esi,edi                 ; nothing to do ??
    jz  xms_exit_copy


;
; if source is greater the destination, it's ok
;     ( at least if the BIOS, too, does it with CLD)

    ja xms_move_ok_to_start

;
; no, it's less
; if (source + length > destination)
;    return ERROR_OVERLAP

    mov eax, esi
    add eax, ecx
    cmp eax, edi
    ja  move_invalid_overlap

;
; we might be able to handle that, but are not yet
; so better don't copy
;



;   jmp use_int15               ; allways BIOS


xms_move_ok_to_start:
    SMSW    AX                  ; don't use mov eax,cr0
                                ; this is a priviledged instruction
    test    al,1                ; are we already in PM?
    jnz use_int15               ; yes, use BIOS (or EMM386...)


;------------------------------------------------------------
; we do very interesting protected mode stuff to copy things

    cli                     ; no interrupts when doing protected mode


    lgdt    [fword cs:gdt32]        ; load GDTR
    mov eax,cr0
    or  al,1                ; set PE bit
    mov cr0,eax             ; shazamm!
    db  0eah                ; JMP FAR
    dw  offset to_pm,code16idx      ; flush IPQ and load CS sel.
to_pm:

    mov ax,core32idx
    mov ds,ax
    mov es,ax

    shr ecx,2               ; get number of DWORDS to move
    jnc dword_boundary          ; is length a DWORD multiple?
    movs    [word esi],[word edi]       ; no, move first word to adjust
dword_boundary:
    rep movs [dword esi],[dword edi]    ; now move the main block

                            ; and because not all 386's were OK,
    db      67h             ; don't remove - some x386's were buggy
    nop                     ; don't remove - some x386's were buggy


    mov eax,cr0
    and al,not 1            ; clear PE bit
    mov cr0,eax             ; shazomm!

    db  0eah                ; JMP FAR
    dw  offset to_rm
code_seg dw ?               ; flush IPQ and load CS sel.

to_rm:
    jmp xms_exit_copy

;------------------------------------------------------------------------
; we are in protected mode, use int15, ah=87 to copy things around

BIOSGDT:
            db 0,0,0,0,0,0,0,0  ; dummy GDT entry
            db 0,0,0,0,0,0,0,0  ; dummy GDT entry

            dw 0ffffh           ; source segment length
GDTsrclow   dw 0              ; 24 bit src address
GDTsrcmiddle db 0
            db 093h             ; source access rights == 94
            db  0fh         ; More type bits and bits 16-19 of source segment length.
GDTsrchigh  db  0           ; Bits 24-31 of source address.

            dw 0ffffh           ; dest segment length
GDTdstlow   dw 0                ; 24 bit src address
GDTdstmiddle db 0
            db 093h             ; dest access rights == 94
            db  0fh         ; More type bits and bits 16-19 of dest segment length.
GDTdsthigh  db   0           ; Bits 24-31 of dest address.

            db 0,0,0,0,0,0,0,0  ; dummy GDT entry
            db 0,0,0,0,0,0,0,0  ; dummy GDT entry

; entry ESI = src linear adress
; entry EDI = dst linear adress
; entry ECX = length

public use_int15
use_int15:
    mov edx,ecx

use_int15_loop:
    mov ecx,edx
    cmp ecx,1000h
    jle nomax1000
    mov ecx,1000h
nomax1000:


    cli                     ; protect BIOSGDT for reentrancy

    mov eax,esi
    mov [GDTsrclow],ax
    shr eax,010h
    mov [GDTsrcmiddle],al
    mov [GDTsrchigh],ah

    mov eax,edi
    mov [GDTdstlow],ax
    shr eax,010h
    mov [GDTdstmiddle],al
    mov [GDTdsthigh],ah

    push ecx                 ; later used again
    push esi
    push es
    push cs
    pop  es

;    lea si, [BIOSGDT]
    mov si, OFFSET BIOSGDT
    shr cx,1                ; number of words

    clc
    mov ah,87h
    int 15h

    sti

    pop es
    pop esi
    pop ecx                 ; get length back

    jc move_a20_failure


    add edi,ecx             ; buff += copied length
    add esi,ecx

    sub edx,ecx             ; count -= copied length

    jnz use_int15_loop


;   jmp xms_exit_copy
;-------------------------------------------------------------------------




xms_exit_copy:
    popf                    ; get A20 state

    pop ebx
    pop eax

    mov ax,1                ; success


move_a20_exit:
IF ALLOWDISABLEA20
    jnz move_a20_was_enabled  ; if A20 was enabled, don't disable
    call    disable_a20       ; it was disabled, so restore state
    move_a20_was_enabled:
ENDIF

    pop edx ecx

    ret;return_success_or_failure

                            ; common exit rouitne
move_a20_failure:
    mov bl,XMS_A20_FAILURE

xms_exit_copy_failure:
    popf                    ; get back A20 state

    mov al,bl               ; save errorcode BL , but restore EBX
    pop ebx
    mov bl,al
    pop eax
    mov ax,0                ; failure
    jmp move_a20_exit


move_invalid_overlap:
    mov bl,XMS_INVALID_OVERLAP
    jmp xms_exit_copy_failure

move_invalid_length:
    mov bl,XMS_INVALID_LENGTH
    jmp xms_exit_copy_failure


copy_source_is_wrong:
    cmp bl,XMS_INVALID_LENGTH
    je xms_exit_copy_failure

    mov bl,XMS_INVALID_SOURCE_HANDLE
    jmp xms_exit_copy_failure

copy_dest_is_wrong:
    cmp bl,XMS_INVALID_LENGTH
    je xms_exit_copy_failure

    mov bl,XMS_INVALID_DESTINATION_HANDLE
    jmp xms_exit_copy_failure


endp    xms_move_xms

;******************************************************************************
; locks an XMS block
; In:   AH=0ch
;   DX=XMS handle to be locked
; Out:  AX=1 if block is locked
;     DX:BX=32-bit linear address of block
;   AX=0 if not successful
;     BL=080h -> function not implemented
;     BL=081h -> VDISK is detected
;     BL=0a2h -> handle is invalid
;     BL=0ach -> lock count overflow
;     BL=0adh -> lock fails

public  xms_lock_xms
proc    xms_lock_xms
    call xms_check_handle       ; check if dx holds handle
    jnc lock_valid_handle
    ret;return_failure

lock_valid_handle:
    mov si,dx
    inc [si+xms_handle.locks]   ; increase lock counter
    jnz locked_successful       ; go on if no overflow
    dec [si+xms_handle.locks]   ; decrease lock counter
    xor ax,ax
    mov bl,XMS_LOCK_COUNT_OVERFLOW  ; overflow, return with error
    ret;return_failure

locked_successful:

; real 32 bit addresses this way
;    push    eax             ; save EAX
;    movzx   eax,[si+xms_handle.xbase]   ; get block base address
;    shl eax,10              ; calculate linear address
;    mov bx,ax               ; store LSW
;    shr eax,16
;    mov dx,ax               ; store MSW
;    pop eax                 ; restore EAX
;
;; 16 bit handling of .xbase like that
;    mov bx,[si+xms_handle.xbase]   ; get block base address
;    xor dx,dx
;    shld dx,bx,10           ; calculate linear address
;    shl  bx,10

    push    eax
    mov eax,[si+xms_handle.xbase]   ; get block base address
    shl eax,10              ; calculate linear address
    mov bx,ax               ; store LSW
    shr eax,16
    mov dx,ax               ; store MSW
    pop eax

    mov ax,1

    ret;return_success
endp    xms_lock_xms

;******************************************************************************
; unlocks an XMS block
; In:   AH=0dh
;   DX=XMS handle to unlock
; Out:  AX=1 if block is unlocked
;   AX=0 if not successful
;     BL=080h -> function not implemented
;     BL=081h -> VDISK is detected
;     BL=0a2h -> handle is invalid
;     BL=0aah -> block is not locked

public  xms_unlock_xms
proc    xms_unlock_xms
    call xms_check_handle       ; check if dx holds handle
    jnc unlock_valid_handle
    ret;return_failure

unlock_valid_handle:

    mov si,dx
    cmp [si+xms_handle.locks],0 ; check if block is locked
    jnz is_locked           ; go on if true
    xor ax,ax
    mov bl,XMS_BLOCK_NOT_LOCKED
    ret;return_failure
is_locked:
    dec [si+xms_handle.locks]   ; decrease lock counter
    mov ax,1
    xor bl,bl
    ret;return_success
endp    xms_unlock_xms

;******************************************************************************
; get extended EMB handle information
; In:   AH=8eh
;   DX=XMS block handle
; Out:  AX=1 if successful
;     BH=block's lock count
;     CX=number of free XMS handles
;     EDX=block's length in kbytes

proc    xms_ext_get_handle_info
    call xms_check_handle   ; check handle validity
    jnc  ext_handle_info_1
    ret;return_failure

ext_handle_info_1:
    push    eax
    mov     si,dx

    xor dx,dx               ; reset free handle counter
    call    xms_find_free_handle        ; chk if there's a blank handle
    jc  ext_nothing_free
ext_find_next_free:
    inc dx              ; increase handle counter
    call    xms_find_next_free_handle   ; and check if there's another
    jnc ext_find_next_free

ext_nothing_free:
    mov cx,dx               ; store number of free handles
    mov bh,[si+xms_handle.locks]    ; store lock count
    mov edx,[si+xms_handle.xsize]    ; store block size

    pop eax
    mov ax,1
    ret;return_success
endp    xms_ext_get_handle_info


;********************************************************************
; returns XMS handle information
; In:   AH=0eh
;   DX=XMS block handle
; Out:  AX=1 if successful
;     BH=block's lock count
;     BL=number of free XMS handles
;     DX=block's length in kbytes
;   AX=0 if not successful
;     BL=080h -> function not implemented
;     BL=081h -> VDISK is detected
;     BL=0a2h -> handle is invalid

public  xms_get_handle_info
proc    xms_get_handle_info

	push cx
	push edx
	
	call xms_ext_get_handle_info
	or ax,ax
	jz xms_get_handle_info_err

	cmp cx,0ffh					; bl = min(cx,0xff)
	jbe handle_count_ok
	mov cl,0ffh
handle_count_ok:
	mov bl,cl
    
    
	cmp edx,010000h				; dx = min(edx,0xffff);
	jbe handle_size_ok
	mov dx,0ffffh
handle_size_ok:

	mov cx,dx
	pop edx
	mov dx,cx
	pop cx
	
	ret
	
xms_get_handle_info_err:
	pop edx
	pop cx
	ret
	
	
endp    xms_get_handle_info


;******************************************************************************
;;  reallocates an XMS block. only supports shrinking.
;  reallocates an XMS block. shrinking and growing supported
; In:   AH=0fh
;   BX=new size for the XMS block in kbytes
;   DX=unlocked XMS handle
;
; EXTENDED:
; In:   AH=8fh
;   EBX=new size for the XMS block in kbytes
;   DX=unlocked XMS handle
; Out:  AX=1 if successful
;   AX=0 if not successful
;     BL=080h -> function not implemented
;     BL=081h -> VDISK is detected
;     BL=0a0h -> all XMS is allocated
;     BL=0a1h -> all handles are in use
;     BL=0a2h -> invalid handle
;     BL=0abh -> block is locked

proc    xms_ext_realloc_xms
    call xms_check_handle   ; check handle validity
    jnc  ext_realloc_xms_handle_valid
    ret;return_failure

ext_realloc_xms_handle_valid:
    push    eax
    push    ebx
    push    edx

    xchg    ebx,edx

; fail if block is locked
	cmp	[bx+xms_handle.locks],0
	jne	ext_xms_locked

    cmp edx,[bx+xms_handle.xsize]
    jbe ext_shrink_it

; growing, try to allocate a new block
	call	xms_ext_alloc_xms
	cmp	ax,1
	je	ext_gotblock

; failed to allocate a new block
	pop	edx				; proper error code already in bl
	mov	al,bl
	pop	ebx
	mov	bl,al			; restore error code
	jmp	ext_failed

xms_move_data	xms_move_strc	<>

; got new block, copy info from old block to new block
; dx == new handle
ext_gotblock:
	pop	ebx				; bx == old handle
	push	ebx
	movzx	edx,dx		; edx == new handle

	mov	eax,[bx+xms_handle.xsize]
	shl	eax,0ah			; K to byte
	mov	[xms_move_data.len],eax		; transfer old handle data to new location
	xor	eax,eax
	mov	[xms_move_data.src_offset],eax
	mov	[xms_move_data.dest_offset],eax
	mov	[xms_move_data.src_handle],bx
	mov	[xms_move_data.dest_handle],dx

	mov	si,OFFSET xms_move_data
	push	bx			; save old handle ptr
	push	ds
	pop	es				; es -> "original" ds for xms_move_xms
	call	xms_move_xms
	push	cs			; xms_move_xms eats critical ds value
	pop	ds
	pop	bx

; swap handle data so handle pointers remain valid
;  handle data is 10 bytes long
	mov	eax,[bx]
	xchg	eax,[edx]
	mov	[bx],eax
	mov	eax,[bx+4]
	xchg	eax,[edx+4]
	mov	[bx+4],eax
	mov	ax,[bx+8]
	xchg	ax,[edx+8]
	mov	[bx+8],ax

; free newly allocated handle with old handle data in it
; dx == new handle
	call	xms_free_xms
	jmp	ext_grow_success

ext_no_xms_handles_left:
    pop edx
    pop ebx
    mov bl,XMS_NO_HANDLE_LEFT
	jmp	ext_failed

ext_xms_locked:
    pop edx
    pop ebx
    mov bl,XMS_BLOCK_LOCKED

ext_failed:
    pop eax
    xor ax,ax
    ret ; failure

ext_shrink_it:
    mov si,bx
    call    xms_find_free_handle        ; get blank handle
    jc  ext_no_xms_handles_left     ; return if there's an error
    mov eax,[si+xms_handle.xsize]    ; get old size
    mov [si+xms_handle.xsize],edx
    sub eax,edx               ; calculate what's left over
    jz  ext_dont_need_handle        ; skip if we don't need it
    add edx,[si+xms_handle.xbase]    ; calculate new base address

    mov [bx+xms_handle.xbase],edx    ; store it
    mov [bx+xms_handle.xsize],eax    ; store size
    mov [bx+xms_handle.locks],0     ; block is not locked...

    mov [bx+xms_handle.used],2      ; ...and not used - for a mikrosecond

    mov dx,bx                       ; and FREE it again -
    call xms_free_xms               ; to merge it with free block list

ext_dont_need_handle:
ext_grow_success:
    pop edx
    pop ebx
    pop eax
    mov ax,1
    xor bl,bl
    ret;return_success
endp    xms_ext_realloc_xms


public  xms_realloc_xms
proc    xms_realloc_xms

	push ebx    					; protect high part of ebx
	pop  bx
	
	movzx ebx,bx					; clear top 16 bit
	
	call xms_ext_realloc_xms
	
									; recover top 16 bit of ebx
	push bx
	pop  ebx
	
	ret									

endp    xms_realloc_xms




;******************************************************************************
; requests an UMB block
; In:   AH=10h
;   DX=size of requested memory block in paragraphs
; Out:  AX=1 if successful
;     BX=segment number of UMB
;     DX=actual size of the allocated block in paragraphs
;   AX=0 if not successful
;     DX=size of largest available UMB in paragraphs
;     BL=080h -> function not implemented
;     BL=0b0h -> only a smaller UMB are available
;     BL=0b1h -> no UMBs are available


;******************************************************************************
; releases an UMB block
; In:   AH=11h
;   DX=segment of UMB
; Out:  AX=1 if successful
;   AX=0 if not successful
;     BL=080h -> function not implemented
;     BL=0b2h -> UMB segment number is invalid


;******************************************************************************
; reallocates an UMB
; In:   AH=12h
;   BX=new size for UMB in paragraphs
;   DX=segment of UMB to reallocate
; Out:  AX=1 if successful
;   AX=0 if not successful
;     BL=080h -> function not implemented
;     BL=0b0h -> no UMB large enough to satisfy request
;       DX=size of largest UMB in paragraphs
;     BL=0b2h -> UMB segment is invalid

;xms_realloc_umb:
;xms_request_umb:
;xms_release_umb:
;    xor ax,ax
;    mov bl,XMS_NOT_IMPLEMENTED
;    ret;return_failure



proc    xms_not_supported

    xor ax,ax               ; everything else fails
    mov bl,XMS_NOT_IMPLEMENTED
    ret
endp    xms_not_supported


;******************************************************************************
; XMS dispatcher
;******************************************************************************
; XMS dispatcher
; In:   AH - function number
; Out:  AX=0 -> function not supported
;   else see appr. routine

xms_table   dw  xms_get_version,xms_request_hma,xms_release_hma
            dw  xms_global_enable_a20,xms_global_disable_a20
            dw  xms_local_enable_a20,xms_local_disable_a20
            dw  xms_query_a20,xms_query_free_xms,xms_alloc_xms
            dw  xms_free_xms,xms_move_xms,xms_lock_xms,xms_unlock_xms
            dw  xms_get_handle_info,xms_realloc_xms

			dw xms_ext_query_free_xms	; 88            
			dw xms_ext_alloc_xms		; 89
            dw xms_not_supported		; 8a
            dw xms_not_supported		; 8b
            dw xms_not_supported		; 8c
            dw xms_not_supported		; 8d
		    dw xms_ext_get_handle_info  ; 8e
		    dw xms_ext_realloc_xms		; 8f
		    

proc    xms_dispatcher
    jmp short dispatcher_entry
    nop                 ;
    nop                 ; guarantee hookability
    nop                 ;
dispatcher_entry:

label dispatcher_log_entry byte
	call log_entry  	; this might get patched


    pushf                       ; save flags

    cld

    cmp ah,0fh                  ; is it a supported function?
                                ; UMB functions not implemented
    jbe  supported_api


								; test for 88..8f
	cmp  ah,88h
	jb   not_supported
    cmp  ah,8fh
    ja   not_supported
    
    sub ah, 88h-10h				; map functions from 88..8f to
    							;                    10..17

;
;real dispatcher
;
; save ds,es,si,di
; set es = USERds
; set ds = CS
supported_api:
    push ds         ; protect registers
    push es
    push esi        ; might get used 32 bit wide
    push edi        ;

    push ds         ; set up segment registers for internal use
    pop  es
    push cs
    pop  ds

    movzx   di,ah       ; is nowhere used as input
    shl di,1
    call [xms_table+di] ; call the handler here

api_call_complete:
    pop edi         ; restore saved registers
    pop esi

    pop es
    pop ds

dispatcher_end:    
    popf
    
label dispatcher_log_exit byte
	call log_exit       ; this might get patched
    
    retf


not_supported:
    xor ax,ax               ; everything else fails
    mov bl,XMS_NOT_IMPLEMENTED
    jmp short dispatcher_end




endp    xms_dispatcher

;******************************************************************************
; mark for the driver end. above has to be the resident part, below the
; transient.

;******************************************************************************
; mark for the normal driver end. above has to be the resident part, 

 public normal_driver_end
normal_driver_end:



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
proc __print

print_1char:
				int 29h

print2:			pop   bx
	   		    pop   si                       ; this is the first character
	   		    mov   al,[cs:si]               ; get token
	   		    inc   si
				push  si               			; stack up potential return address	
				push  bx
				                       
				cmp   al, 0                    ; end of string?
				jne   print_1char              ; until done
				ret

	
print:
	call print2
	ret



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
           
           
printhexd: 		call print2

				mov al,'='
				int 29h
                
                ror edx,16

				call printdx

                ror edx,16
                
                jmp printhex2


endp __print 


;************************* ;returns NZ, if we shall log NOW
lognow:	
							; this will LOG stuff to screen only, if 
							; SCROLL_LOCK is locked
	push es
	push ax
	
	mov ax,40h
	mov es,ax
	mov al,[es:17h]
	test al,10h
	pop ax
	pop es
	ret




;
; to be added by YOU (not be me as I'm happy as it is) 
;
; display funktion string
; display input and output registers
; enable trace only for certain functions
; ...
;


log_entry:
	pushf
	pusha

	call lognow
	jz log_done	

     call print
     db 'XMS enter:',0
    
log_done:	       
	popa
	popf
	ret



log_exit:
	pushf
	pusha 

	call lognow
	jz log_done	


	call print
    db 'XMS leave',13,10,0
	
	popa
	popf
	ret




;******************************************************************************
; mark for the trace log mode driver end. above has to be the resident part, 
trace_driver_end:




DB  120 DUP (0) ; make transient portion bigger


;******************************************************************************
; call an external commandline handler
; although it doesn't do anything yet, its present.
;
; feel free to implement:
;
;   NumHandles=
;   MaxMem=
;   ...
; tom
;******************************************************************************

    public _no_above_16
	public _x_option
; these matches Microsoft's HIMEM.SYS /X /NOABOVE16 command
_no_above_16    DB  0       ; don't use 0e801h extended memory check
_x_option       DB  0       ; don't use 0e820h extended memory check

; forced A20 method switches
	public	_alwayson_set
	public	_bios_set
	public	_ps2_set,_fast_set,_port92_set
	public	_kbc_set
_alwayson_set	DB	0
_bios_set		DB	0
_ps2_set		DB	0
_fast_set		DB	0
_port92_set		DB	0
_kbc_set		DB	0

	public	_xms_max
_xms_max		DD	0		; maximum XMS controlled by HIMEM in 1K
;	public  _hma_min
;_hma_min		DW	0



proc DoCommandline near

                            ; install a stack, the C part will require it
    mov ax,ss
    mov bx,sp

    mov dx,cs
    mov ss,dx
    mov sp,offset DGROUP:driver_stacktop

    pusha
    push es

    les bx,[cs:request_ptr]   ; pointer to request header


    les ax, [es:di+init_strc.cmd_line]

    push es                     ; startup_driver(char far *cmdLine)
    push ax
    call _startup_driver

    add sp,4

    pop es
    popa

                            ; restore original stack, DOS requires it

    mov ss,ax
    mov sp,bx


    ret

endp DoCommandline

;******************************************************************************
; checks if VDISK is already installed
; note: HMA check is skipped because of speed and some other (weird) reasons.
; In:   nothing
; Out:  ZF=0 -> VDISK is installed
;   ZF=1 -> VDISK not installed
;
; tom:it's absolute unclear, if [13] or [12] should be checked.
;     HIMEM verifies [13], so we do that as well.
;     goto HELL, dear VDISK
;     verify only 4 bytes, should do as well
;


proc    _install_check_vdisk
    push    bx ds

    xor bx,bx           ; get interrupt vector 19h
    mov ds,bx
    lds bx,[19h*4]

        
    cmp [dword bx],053494456h; 'VDIS'

    pop ds bx
    ret
endp    _install_check_vdisk



;******************************************************************************
; 16-bit transient code and data. only used once.
;******************************************************************************
; checks if CPU is a 386
; In:   nothing
; Out:  CY=0 - processor is a 386 or higher
;   CY=1 - processor lower than 386

proc    check_cpu
    pushf
    xor ax,ax
    push    ax
    popf
    pushf
    pop ax
    and ah,0fh
    cmp ah,0fh
    je  not386
    mov ah,7
    push    ax
    popf
    pushf
    pop ax
    and ah,7
    je  not386
    popf
    clc
    ret
not386:
    popf
    stc
    ret
endp    check_cpu

;******************************************************************************
; checks if A20 can be enabled and disabled
; Out:  CF=0 - A20 switching works
;   CF=1 - A20 failure

cant_disable_message db 'Can',27h,'t disable A20 - ignored',0dh,0ah,'$'

proc    check_a20
    call    enable_a20
    call    test_a20            ; TEST_A20 should return ZF=0
    jz  a20failed
IF ALLOWDISABLEA20    
    call    disable_a20
    call    test_a20            ; TEST_A20 should return ZF=1
    jz a20_ok
                                ; we can't disable A20.
                                ; so what ?
                                ; these guys are crazy anyway,
                                ; and we (nearly) ignore that 
                                    
    mov ah,9                    ; print msg
    mov dx,offset cant_disable_message
    int 21h
    
a20_ok:
ENDIF    
    clc
    ret
a20failed:
    stc
    ret
endp    check_a20


; the so-called 'fast' A20 method replacement code
; entry: ah == 0 A20 turn off, ah == 2 turn on, ax on stack
;
disable_enable_a20_fast:
    in  al,92h
	or	ah,ah
	jne	deaf_on		; turning on A20
	test	al,2
	je	deaf_done	; already flagged off, don't do it again, might upset something
	and	al,NOT 2	; set A20 bit off
	jmp	deaf_out

; ah == 2
deaf_on:
	test	al,ah
	jne	deaf_done	; already flagged on
	or	al,ah		; set A20 bit on

deaf_out:
	out	92h,al

; wait until it gets on or off, possibly superfluous, code opinion differs
	push	cx
    xor cx,cx
deaf_wait:
    in  al,92h
    and al,2
    cmp al,ah
    loopne deaf_wait
	pop	cx

deaf_done:
	pop	ax
    ret

disable_enable_a20_fast_end:

; check if BIOS flags port 92h fast method supported
proc detect_and_handle_fast
	stc
	mov	ax,2403h
	int	15h
	jc	fail_test
	or	ah,ah
	jne	fail_test
	test	bl,2
	je	fail_test

	mov	si,OFFSET disable_enable_a20_fast
	call	detect_and_handle_test
	jc	fail_test

flag_fast:
	mov	dx,OFFSET MsgFastA20
	mov	ah,9
	int	21h

fast_success:
        mov si, offset disable_enable_a20_fast
        mov cx, offset disable_enable_a20_fast_end - offset disable_enable_a20_fast

trans_handler:
	push	es	; es,di critical registers
	push	di
	push cs
	pop  es
	mov di, offset disable_enable_a20
	rep movsb

	clc			; flag success
	pop	di
	pop	es
	ret

fail_test:
	stc			; flag failure
	ret
endp detect_and_handle_fast

; check if BIOS flags PS/2 present, to try port 92h fast method used by PS/2's
;  shares enable/disable code with fast
proc detect_and_handle_PS2
	mov	ah,0c0h		; get system description vector
	stc
	int	15h
	jc	fail_test	; not a PS/2

; test feature information byte 1, micro channel implemented bit
	test	[BYTE es:bx+5],2
	jz	fail_test	; not micro channel

	mov	si,OFFSET disable_enable_a20_fast
	call	detect_and_handle_test
	jc	fail_test

flag_ps2:
	mov	dx,OFFSET MsgPS2FastA20
	mov	ah,9
	int	21h

	jmp	fast_success

endp detect_and_handle_PS2

; check if port 92h fast method supported without BIOS or PS/2 test
;  shares enable/disable code with fast and PS/2
proc detect_and_handle_port92
	mov	si,OFFSET disable_enable_a20_fast
	call	detect_and_handle_test
	jc	fail_test

flag_port92:
	mov	dx,OFFSET MsgPort92A20
	mov	ah,9
	int	21h

	jmp	fast_success

endp detect_and_handle_port92


; BIOS A20 method
; entry: ah == 0 A20 turn off, ah == 2 turn on, ax on stack
; don't check for errors, assume BIOS works more than once on same call,
;  if it doesn't, not much we can do about it anyway
;
disable_enable_a20_BIOS:
	pushf
	sub	sp,10	; give buggy BIOS some stack to chew on without causing problems
				; one word might suffice, but let's be really safe
	cli
	shr	ah,1	; ah to 0 or 1
	mov	al,24h
	xchg	ah,al	; ax == 2400h to turn off, 2401h to turn on
	int	15h

	add	sp,10	; restore potentially gnawed-on stack
	popf
	pop	ax
    ret
disable_enable_a20_BIOS_end:

proc detect_and_handle_BIOS
	stc				; preset carry flag
	mov	ax,2402h	; get gate status
	int	15h
	jc	fail_test
	or	ah,ah
	jne	fail_test
	mov	cl,al	; save status

	mov	si,OFFSET disable_enable_a20_BIOS
	call	detect_and_handle_test
	jc	fail_test

flag_bios:
	mov	dx,OFFSET MsgBIOSA20
	mov	ah,9
	int	21h

	mov	si,offset disable_enable_a20_BIOS
	mov	cx,offset disable_enable_a20_BIOS_end - offset disable_enable_a20_BIOS
	jmp	trans_handler

endp detect_and_handle_BIOS


; try turning A20 on or off from current to see if it works
; KBC HIMEM method
; entry: ah == 0 A20 turn off, ah == 2 turn on, ax on stack
;
disable_enable_a20_KBC:
	push	cx
	pushf
	cli				; shut off interrupts while we twiddle

	call	Sync8042		; check keyboard controller ready
	mov	al,0D1h		; Send D1h
	out	64h,al
	call	Sync8042
	mov	al,0ddh			; or df=dd+2
	or	al,ah			; disable/enable A20 command (DDh/DFh)
	out	60h,al
	call	Sync8042

; wait up to 20 microseconds for A20 line to settle
	mov	al,0FFh			; pulse output port NULL
	out	64h,al
	call	Sync8042
	popf
	pop	cx
	pop	ax
	ret

Sync8042:
	xor	cx,cx
InSync:
	in	al,64h
	and	al,2
	loopnz InSync
	ret

disable_enable_a20_KBC_end:

proc detect_and_handle_KBC
	mov	si,OFFSET disable_enable_a20_KBC
	call	detect_and_handle_test
	jc	fail_test

flag_kbc:
	mov	dx,OFFSET MsgKBCA20
	mov	ah,9
	int	21h

	mov	si,offset disable_enable_a20_KBC
	mov	cx,offset disable_enable_a20_KBC_end - offset disable_enable_a20_KBC
	jmp	trans_handler

endp detect_and_handle_KBC

; upon entry si->disable/enable routine for a20 method being tested
; return carry set if failed, reset if success
;
proc	detect_and_handle_test
	xor	cx,cx
	call	test_a20
	jz	dah_2		; A20 disabled on entry

	mov	cl,1		; A20 enabled on entry

; try to disable A20
dah_disable:
	push	OFFSET dah_2
	push ax
	mov  ah,0
	jmp	si

dah_2:
	call	test_a20
	jnz	dah_fail		; A20 not disabled

; try to enable A20 (always disabled at this point)
	push	OFFSET dah_3
	push ax
	mov  ah,2
	jmp	si

dah_3:
	call	test_a20
	jz	dah_fail		; A20 not enabled
	or	cl,cl
	jne	dah_success		; A20 was enabled on entry, done
	push	OFFSET dah_4	; disable to entry state
	push ax
	mov  ah,0
	jmp	si

dah_4:
	call	test_a20
	jnz	dah_fail		; A20 not disabled

dah_success:
	clc
	ret

dah_fail:
	stc
	ret
endp	detect_and_handle_test

; reserve size of routine checks
IF disable_enable_a20_BIOS_end - disable_enable_a20_BIOS gt disable_enable_a20_end-disable_enable_a20
    this is an error! reserve some space
ENDIF

IF disable_enable_a20_fast_end - disable_enable_a20_fast gt disable_enable_a20_end-disable_enable_a20
    this is an error! reserve some space
ENDIF

IF disable_enable_a20_KBC_end - disable_enable_a20_KBC gt disable_enable_a20_end-disable_enable_a20
    this is an error! reserve some space
ENDIF

; method feedback text
MsgAlwaysA20	db 'HIMEM - Always On A20 method used',0dh,0ah,'$'
MsgBIOSA20	db 'HIMEM - BIOS A20 method used',0dh,0ah,'$'
MsgFastA20	db 'HIMEM - Fast A20 method used',0dh,0ah,'$'
MsgPS2FastA20	db 'HIMEM - PS/2 Fast A20 method used',0dh,0ah,'$'
MsgPort92A20	db 'HIMEM - Port 92h A20 method used',0dh,0ah,'$'
MsgKBCA20	db 'HIMEM - KBC A20 method used',0dh,0ah,'$'
MsgUnknownA20	db 'HIMEM - No Supported A20 method detected',0dh,0ah,'$'


MASM	; have to switch to masm mode to use COMMENT blocks
COMMENT #
; KBC method
; entry: ah == 0 A20 turn off, ah == 2 turn on, ax on stack

disable_enable_a20_KBC:
	pushf
	cli				; shut off interrupts while we twiddle

	call	[cs:delay2ptr]
	mov	al,0d0h		; 8042 read output port
	out	64h,al		; issue to command register

deaa_loop:
	in	al,64h		; read status register
	test	al,1	; check if output buffer full
	jz	deaa_loop

	in	al,60h		; read data register
	or	ah,ah		; check enable/disable request
	jne	deaa_enable
	and	al,NOT 2	; turn off A20 bit to disable
	jmp	deaa_write

deaa_enable:
	or	al,ah		; turn on A20 bit to enable

deaa_write:
	push	ax		; save bit status
	call	[cs:delay2ptr]
	mov	al,0d1h		; 8042 write output port
	out	64h,al		; issue to command register
	call	[cs:delay2ptr]
	pop	ax			; restore bit status
	out	60h,al		; issue to data register
	call	[cs:delay2ptr]

	mov	al,0ffh		; pulse output port (delay for A20)
	out	64h,al
	call	[cs:delay2ptr]

	popf

	pop	ax
	ret

disable_enable_a20_KBC_end:

;DisableA20PS2:
;    in  al,92h ; 
;    and al,not 2   ; switch off
;    out 92h,al
;                ; wait until it gets off
;    xor cx,cx
;disableps2wait:
;    in  al,92h
;    test al,2
;    loopnz disableps2wait
;    ret

;DisableA20PS2End:


MsgPS2Detected        db 'HIMEM - trying PS/2 maschine',0dh,0ah,'$'

proc detect_and_handle_PS2
;        mov     ah,0C0h   
;        stc
;        int     15h
;        jc      noPS2            
        
;        or         ah,ah
;        jnz        noPS2

;        test   [byte es:bx+5],2 ; feature byte 1, bus is microchannel
;        jz      short NoPS2

        
        mov dx,offset MsgPS2Detected        
        mov ah,9
        int 21h

        cld

                                    ; copy PS2 handler into place       

IF disable_enable_a20_PS2_end - disable_enable_a20_PS2 gt disable_enable_a20_end-disable_enable_a20
    this is an error! reserve some space
ENDIF                                                        
        push cs
        pop  es

        mov di, offset disable_enable_a20
        mov si, offset disable_enable_a20_PS2
        mov cx, offset disable_enable_a20_PS2_end - offset disable_enable_a20_PS2
        rep movsb

		clc			; flag success
        ret

noPS2:
		stc			; flag failure
		ret

endp detect_and_handle_PS2
END COMMENT #
IDEAL

;******************************************************************************
; initializes the driver. called only once!
; may modify DI
; In:   ES:DI - pointer to init structure
;   DS - initialized with code segment

    public _init_message,_vinit_message,_copyright

_init_message  db  'FreeDOS HIMEM64 ',DRIVER_VERSION,0
_copyright     db  '(c) 1995, Till Gerken 2001-2006 tom ehlert',0
_vinit_message db  'Interface : XMS ',INTERFACE_VERSION,' 80386 4G ',0


old_dos         db  'XMS needs at least DOS version 3.00.$'
xms_twice       db  'XMS is already installed.$'
vdisk_detected  db  'VDISK has been detected.$'
no_386          db  'At least a 80386 is required.$'
a20_error       db  'Unable to switch A20 address line.$'
xms_sizeerr     db  'Unable to determine size of extended memory.$'
xms_toosmall    db  'Extended memory is too small or not available.$'

error_msg       db  ' Driver won''t be installed.',7,13,10,'$'

init_finished       db  80 dup ('Ä'),13,10,'$'

    public _startup_verbose
_startup_verbose db  0      ; more (debugging) output in 
                            ; startup phase 


e820map e820map_struc   <0,0,0,0,0>
e820_current_base   DD  0
cfstore DB  ?
zfstore DB  ?


proc    initialize
    pushf
    push    eax ebx ecx edx esi edi

    cld

;    mov ah,9                ; first, welcome the user!
;    mov dx,offset init_message
;    int 21h

    mov ax,3000h            ; get DOS version number
    int 21h
    xchg    ah,al               ; convert to BSD
    cmp ax,300h             ; we need at least 3.00
    mov dx,offset old_dos
    jb  error_exit

    mov ax,4300h            ; check if XMS is already
    int 2fh             ; installed
    cmp al,80h
    mov dx,offset xms_twice
    je  error_exit

    call    check_cpu           ; do we have at least a 386?
    mov dx,offset no_386
	jnc	have_386
    jmp	error_exit   

have_386:
    call DoCommandline          ; call some C initialization
                                ; to interpret commandline,...

; process forced methods
	cmp	[_alwayson_set],0
	jne	flag_alwayson
	cmp	[_bios_set],0
	je	forced_2
	call	flag_bios
	jmp	got_type
forced_2:
	cmp	[_fast_set],0
	je	forced_3
	call	flag_fast
	jmp	got_type
forced_3:
	cmp	[_ps2_set],0
	je	forced_4
	call	flag_ps2
	jmp	got_type
forced_4:
	cmp	[_kbc_set],0
	je	forced_5
	call	flag_kbc
	jmp	got_type
forced_5:
	cmp	[_port92_set],0
	je	forced_done
	call	flag_port92
	jmp	got_type
forced_done:

; check if the A20 line is on, if so assume it's always on
	call	test_a20
;	jz	check_BIOS_method		; not on, try other methods
	jz	check_A20_method		; not on, try other methods

; use A20 always on code (dummy enable/disable A20 routine)
flag_alwayson:
	mov	dx,OFFSET MsgAlwaysA20
	mov	ah,9
	int	21h
	jmp	got_type

check_A20_method:
;check_BIOS_method:
;    call detect_and_handle_BIOS	; see if BIOS A20 handler supported
;	jnc	got_type

; see if fast/port 92h/PS/2 handler supported with 2403h BIOS call
    call detect_and_handle_fast	
	jnc	got_type

; see if fast/port 92h/PS/2 handler supported with PS/2 signature
	push	es		; consumes critical register
    call detect_and_handle_PS2
	pop	es
	jnc	got_type

; KBC used to be called KBC-2 until original KBC method was turned off
    call detect_and_handle_KBC	; see if KBC handler supported
	jnc	got_type

; try BIOS here, demoted from first in line because unreliable BIOS
;  versions of A20 control exist
    call detect_and_handle_BIOS	; see if BIOS A20 handler supported
	jnc	got_type

; see if fast port 92h handler supported without BIOS or PS/2 signature
;  leave this test until last because messing with port 92h is
;  reported to crash some machines which don't support that method
    call detect_and_handle_port92
	jnc	got_type

; out of options to try
unknown_a20:
	mov	dx,OFFSET MsgUnknownA20
	mov	ah,9
	int	21h
    mov dx,offset a20_error
    jmp  error_exit

got_type:
    call    _install_check_vdisk   ; is VDISK installed?
    mov dx,offset vdisk_detected
    jz  error_exit

                                ; get extended memory size
;    clc                         ; MUST be reset,
                                ; some BIOS'es don't set/reset it.

; look for extended memory the Linux approved way, 0e820h -> 0e801h -> 88h
;  could always add other calls like 0c7h and 8ah to catch oddball cases

    cmp [_x_option],0
    jne e801_check		; cannot use 0e820h, per user /X command

; try 0e820h first
    push    es
    push    di

    xor ebx,ebx
    mov esi,ebx
    mov [e820_current_base],ebx
    push    ds
    pop es

; ebx offset is updated with each successive int 15h
e820_loop:
    mov edx,SMAP
    mov ecx,20
    mov di,offset e820map
    xor eax,eax
    mov [e820map.baselow],eax   ; insurance against buggy BIOS
    mov [e820map.type],eax
    mov [e820map.lenlow],eax
    mov ax,0e820h
    int 15h
    setc    [cfstore]   ; keep carry flag status
    cmp eax,SMAP
    jne e820_bad    ; failure
    cmp [cfstore],1
    je  e820_done   ; CF doesn't have to signal fail, can just mean done

    cmp ebx,0
    je  e820_done   ; finished
    cmp ecx,20      ; didn't return all the info needed, assume done
    jb  e820_done

    cmp [e820map.type],1    ; memory available to OS
    jne e820_loop
    mov eax,[e820map.baselow]
    cmp eax,100000h ; has to live in extended memory
    setz    [zfstore]
    jb  e820_loop

    cmp esi,0
    jne e820_checkhole

; we're not able to handle extended base start not exactly at 100000h
;  not big deal to add support later (does this happen, though?)
    cmp [zfstore],1
    jne e820_done
    mov [e820_current_base],eax
    jmp e820_matchcrit

; check that there isn't a hole in memory, stop at the hole if detected
;  this presumes the map will return contiguous addresses rather than a spray
e820_checkhole:
    mov eax,[e820_current_base]
    add eax,esi
    cmp eax,[e820map.baselow]
    jne e820_done   ; current base plus memory length not equal to this base

; matched all the criteria, add to the memory count
e820_matchcrit:
    add esi,[e820map.lenlow]
    jnc e820_loop
    mov esi,-1  ; wow, we overflowed a 4G counter, force a limit
    jmp e820_done

e820_bad:
    xor esi,esi     ; force failure

e820_done:
    pop di
    pop es
    mov eax,esi
    shr eax,10      ; convert from bytes to 1K blocks
    cmp eax,64      ; only use if useful amount
    ja  check_small

; try 0e801h, but set up the registers to fail status because not
;  all BIOS's properly return the carry flag on failure
e801_check:
    cmp [_no_above_16],0
    jne try_88h     ; cannot use 0e801h, per user /NOABOVE16 command

    xor ax,ax
    mov bx,ax
    mov cx,ax
    mov dx,ax
    mov ax,0e801h
    int 15h
    jc  try_88h
    mov ax,cx
    or  ax,dx
    je  try_88h

; if dx is > 0, then cx should be 3c00h since that's full 1-16M range
;  if cx != 3c00h use cx and not dx
    cmp cx,3c00h
    je  e801_compute
    cmp dx,0
    je  e801_compute
    xor dx,dx

e801_compute:
    movzx   edx,dx
    shl edx,6           ; convert 64K blocks to 1K
    movzx   eax,cx
    add eax,edx
    cmp eax,64      ; only use if useful amount
    ja  check_small

; e801h didn't do the trick, fall back to old 88h with 64M max
try_88h:
    clc
    mov ah,88h
    int 15h
    mov dx,offset xms_sizeerr
    jc  error_exit

    movzx   eax,ax

check_small:
    mov dx,offset xms_toosmall
    sub eax,64                   ; save HIMEM area
    jc  error_exit              ; if there aren't 64k,
                                ; there's nothing to do
	mov	edx,[_xms_max]
	or	edx,edx
	je	save_size			; no maximum XMS set
	cmp	eax,edx
	jbe	save_size			; at or below maximum
	mov	eax,edx				; above max, limit to maximum

save_size:
    mov [xms_size],eax           ; save size

;    push    eax                 ; save EAX

    mov ax,cs                   ; setup descriptors
    mov [code_seg],ax           ; eliminate relocation entry
    movzx   eax,ax
    shl eax,4
    or  [dword code16dsc+2],eax
    add [dword gdt32+2],eax

;    pop eax                     ; restore EAX

    push    es

    xor ax,ax                   ; get INT2Fh vector
    mov es,ax
    les bx,[es:2fh*4]
    mov [word old_int2f+2],es
    mov [word old_int2f],bx

    mov ax,252fh            ; install own INT2Fh
    mov dx,offset int2f_handler
    int 21h
    
                            ; install own INT15h
    mov ax,3515h            ; getvect --> es:bx
    int 21h

    mov [word old_int15+2],es
    mov [word old_int15],bx

    mov ax,2515h            ; setvect -->ds:dx
    mov dx,offset int15_handler
    int 21h
    

    pop es     
    
 	; *****************  handle LOG mode
	cmp [_xms_logging_enabled],0
	jne enable_logging
	                                    
	mov  [dispatcher_log_entry+0],090h	; patch call to NOP
	mov  [dispatcher_log_entry+1],090h	; patch call to NOP
	mov  [dispatcher_log_entry+2],090h	; patch call to NOP

	mov  [dispatcher_log_exit +0],0cbh	; patch call to RETF
	
	jmp logging_end    							

enable_logging:    							
	mov [xms_handle_start], offset trace_driver_end    							
    
    
logging_end:
 	; *****************  handle LOG mode
    


                                ; driver init done    

    mov [word es:di+2+init_strc.end_addr],cs    ; set end address
    xor dx,dx
    mov ax,size xms_handle
    mul [_xms_num_handles]
    add ax,[xms_handle_start]
    mov [word es:di+init_strc.end_addr],ax
    mov [es:di+request_hdr.status],STATUS_OK    ; we're alright
    jmp short exit

error_exit:
    mov [word es:di+2+init_strc.end_addr],cs    ; set end address
    mov [word es:di+init_strc.end_addr],0   ; now, we don't use
                            ; any space
    mov [es:di+request_hdr.status],STATUS_BAD   ; waaaah!
    mov ah,9                    ; print msg
    int 21h
    mov dx,offset error_msg
    int 21h

exit:
;               no longer nice ------------- message for noise reduction
;    mov ah,9
;    mov dx,offset init_finished
;    int 21h


;
; here, AFTER ALL MESSAGES,...
; we clear the handle table, as this may overwrite part of the code above
; but must not erase itself
; 

handle_table_end:                   ; handle_table_end - driver_end
                                    ; must be >= max_handles*sizeof(handle)

;**                
;** make sure the reserved handle space is large enough
;** 

IF $ - trace_driver_end le 128 *size xms_handle 
                                    ; reserve enough space for the handle_table
    this is an error! serve some space after driver end ~!!                         
ENDIF                
;******                
                                    ;
                                    ; clean up and init the handle table
                                    ;    
    
;   push    eax
    mov cx,[_xms_num_handles]        ; get number of handles
    mov bx,[xms_handle_start]        ; get start of handle table
clear_table:
    xor eax,eax
    mov [bx+xms_handle.xbase],eax     ; blank handle
    mov [bx+xms_handle.xsize],eax     ; blk doesn't occupy any space
    mov [bx+xms_handle.locks],al      ; clear locks
    mov [bx+xms_handle.used],1        ; handle not used
    add bx,size xms_handle
    loop    clear_table

	mov	bx,OFFSET xms_handle_table
	mov	ax,[_xms_num_handles]
	mov	[bx+stxms_handle_table.num_handles],ax
	mov	ax,ds
	shl	eax,16
	mov ax,[xms_handle_start]
	mov	[bx+stxms_handle_table.ptr_handles],eax

    mov bx,[xms_handle_start]
    mov [bx+xms_handle.xbase],XMS_START ; init first block and give
    mov eax,[xms_size]           ; it all available memory
    mov [bx+xms_handle.xsize],eax
;   pop eax

    pop edi esi edx ecx ebx eax
    popf
    ret
endp    initialize




;******************************************************************************
; init_interrupt routine. called by DOS right after the strategy routine to
; process the incoming job. also used to initialize the driver.

public  init_interrupt
proc    init_interrupt   near
    push    di es

    push cs                     ; DS == CS
    pop  ds

    les di,[request_ptr]        ; load address of request header

    cmp [es:di+request_hdr.cmd],CMD_INIT; do we have to initialize?
    jne done

    call    initialize          ; no, do it now!

    mov al,090h                     ; NOP
    mov [cs:init_patch], al         ; NOP out init routine
    mov [cs:init_patch+1], al       ;
    mov [cs:init_patch+2], al       ;


done:
    lds si,[request_ptr]        ; return this to DOS

    pop es di
    ret                 ; far return here!
endp    init_interrupt




;------------------------------------------------------------------------------

;*********************************************
; startpoint when executing as EXE
;*********************************************


ASMSTART_EXE:
        mov     ax, DGROUP
        mov     ds,ax

        mov     ss,ax
        mov     sp,offset DGROUP:driver_stacktop


        push  es                        ; startup_exe(commandline);
        mov   ax,080h
        push  ax
        call  _startup_exe
        add sp,4

                                        ; exit
        mov     ah,04ch                 ; und tschuess
        int     21h


;***** exe done


    public _install_end
_install_end:
    
ends    _TEXT  





;******************************************************************************

end ASMSTART_EXE
