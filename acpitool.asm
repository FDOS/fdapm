; ### This file is part of FDAPM, a tool for APM power management and
; ### energy saving. (c) by Eric Auer <eric #@# coli.uni-sb.de> 2005.
; FDAPM is free software; you can redistribute it and/or modify it
; under the terms of the GNU General Public License as published
; by the Free Software Foundation; either version 2 of the License,
; or (at your option) any later version.
; ### FDAPM is distributed in the hope that it will be useful, but
; ### WITHOUT ANY WARRANTY; without even the implied warranty of
; ### MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; ### GNU General Public License for more details.
; You should have received a copy of the GNU General Public License
; along with FDAPM; if not, write to the Free Software Foundation,
; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
; (or try http://www.gnu.org/licenses/ at http://www.gnu.org).

	; ACPI throttle / off functions for FDAPM by Eric Auer 2005
	; to be placed LAST in FDAPM binary (uses buffers after code)

	; supported functions: THROTTLE - call with AX=0..8 to get
	; S1 HLT, 1..8/8 speed, and ACPIOFF - call, after flushing
	; buffers, to soft-off the system (S4/S5 state).

	; Sn states: 0 on, 1 stopped (press power button to wake up),
	; (2 CPU off*), 3 suspend to RAM*, 4 suspend to disk*, 5 off*
	; * system will reboot on wake up, but S2-S4 let you install
	; a quick boot handler which restores the system state which
	; you had to save yourself first (too complicated for DOS!).

%define STANDALONE_ACPITOOL 0
%define CURIOUS_ACPI 0		; set to 1 to enable some debug code

%if STANDALONE_ACPITOOL		; create some test cases
	org 100h
start:	mov ax,4		; half speed
	call throttle
	jc failed
	mov ax,0		; stopped
	call throttle
	jc failed
	mov ax,8		; full speed
	call throttle
	jc failed
	call acpioff		; power off
	; (never returns if power off worked)
failed:	mov ax,4cffh
	int 21h
%endif	; STANDALONE_ACPITOOL

	; -------------

throttle:			; call with AX=1..8 for 1..8/8 speed
	push ax			; or with AX=0 for S1 stop or AX=9 to read speed.
					; returns old speed in AX, destroys registers.
	call getacpi		; collect ACPI data
	pop ax
	jc c_ret2
	push ax
	call read_cpu_speed	; fetch previous speed (5/2005)
	mov bp,ax
	pop ax
	cmp ax,9
	jz just_read_speed
	cmp ax,8
	ja c_ret2		; out of range
	jz no_tht
	or ax,ax		; special case: S1 (CPU stop)
	jnz norm_tht
s1_sys:	; *** S1 ***		; "stop" case: caller should flush caches
	mov al,[s1mode]		; etc. before selecting this "speed".

sn_sys:	cmp al,-1		; trigger S1 sleep or S5 soft-off etc.
	jz c_ret2		; selected Sn state not supported
	;
	push ax
	mov dx,[pm1aEvt]	; base for pm1aStatus and pm1aEnable
	in ax,dx		; pm1aStatus
	or ax,8700h		; clear (by writing 1) pending "waking up",
	out dx,ax		; "RTC ring" "sleep button", "power button".
	add dx,2		; *** more flexible: "pm1aEvt block len / 2"
	in ax,dx		; pm1aEnable
	or ax,300h		; enable "sleep button" and "power button".
	out dx,ax
	pop ax
	;
	shl ax,10		; 400h * number (CPU will be 386+)
	or ax,2000h		; "enter new mode" flag
	mov dx,[pm1aCnt]	; port number for PM 1a control
	out dx,ax		; Ta-Daa!
	sti
; -	mov dx,[pblk_io]	; would have to check if latency below 100ys
; -	add dx,4		; before we may use the p_lvl2 trigger port:
; -	in al,dx		; reading the port triggers CPU C2 sleep
	hlt			; HLT triggers CPU C1 sleep
	clc			; we could read pm1aStatus until MSB is set
just_read_speed:
	mov ax,bp
	ret

c_ret2:	stc
	ret

no_tht:	; *** no THROTTLE ***
	xor ebx,ebx		; to-be-set value
some_tht:
	mov dx,[pblk_io]
	in eax,dx
	and al,0efh		; disable throttling
	out dx,eax		; must be disabled while modifying
	;
	push edx	; and eax,"not the throttle value for 7/8 speed"
	mov cx,[thtbits]	; low: bit offset, high: bit count
	xor edx,edx
	inc edx
	xchg cl,ch
	shl edx,cl
	dec edx	; maximum value
	xchg cl,ch
	shl edx,cl	; shift to right place
	not edx
	and eax,edx	; turn off all duty cycle bits
	pop edx
	;
	or eax,ebx		; possibly enable new throttling
	out dx,eax		; Ta-Daa!
	mov ax,bp		; previous speed
	clc
	ret

norm_tht:			; throttle to 1..7/8 speed
	movzx eax,al
	mov cx,[thtbits]	; low: bit offset, high: bit count
	cmp ch,3
	jb c_ret		; must be at least 8 steps
	sub ch,3		; if more steps, just use every Nth
	add cl,ch
	shl eax,cl
	or al,10h		; bit "enable throttling"
	mov ebx,eax		; to-be-set value
	jmp short some_tht

c_ret:	stc
	ret

acpioff:			; Just ACPI soft-off the system
	call getacpi		; collect ACPI data
	jc c_ret
	mov al,[s5mode]
	jmp sn_sys		; Enter that mode of no return!

read_cpu_speed:		; internal (must be called after getacpi) - 5/2005
	push dx		; returns current CPU throttle value (8 if full speed) in EAX
	push cx		; other registers preserved
	mov dx,[pblk_io]
	in eax,dx
	test al,10h		; was throttle enabled at all?
	jz full_read_speed
	and al,0efh		; remove throttle enable bit
	mov cx,[thtbits]	; low: bit offset, high: bit count
	sub ch,3
	jbe normalized_read_speed	; assume "less than 3 means 3"
	add cl,ch		; will shift away the extra bits
normalized_read_speed:
	shr eax,cl
done_read_speed:
	movzx eax,al		; return AX value in (0) 1..7 range now
	clc
	pop cx
	pop dx
	ret
full_read_speed:
	mov al,8		; assume full speed
	jmp short done_read_speed

	; -------------

getacpi:	; fetch ACPI tables and values (destroys registers)
	mov ax,[facpptr]	; FACP pointer as "already done" flag
	or ax,[facpptr+2]
	jz no_acpi_yet	; "did not try at all yet" value
	cmp ax,1	; "already tried but failed" value
	jz no_acpi_retry
	clc		; ... then we can just return, as
	ret		; all values are already known :-)
	;
no_acpi_retry:
	stc		; we already know that it will not work
	ret
	;
no_acpi_yet:
	mov ax,3306h	; get internal DOS version BL:BH.DL/DH (DOS 5+)
	xor bx,bx	; (DOS 4/older returns AL=-1)
	int 21h
	cmp bx,3205h
	jz inntdosbox
	cmp al,-1
	jz inntdosbox
	;
	mov ax,40h	; BIOS data area
	mov ds,ax
	mov ax,[ds:0eh]	; EBDA segment
	or ax,ax
	jz noebda
	cmp ax,200h	; 200..3fc probably is just an LPT I/O port
	jb hasebda
	cmp ax,3fch
	jb noebda
hasebda:
	mov cx,1024/16	; scan one kb
	mov ds,ax	; EBDA segment
	xor bx,bx
scan_e:	call check_rsdp	; check if DS:BX has RSD PTR structure
	jz found_rsdp
	add bx,16
	loop scan_e
noebda:	mov ax,0f000h
	mov ds,ax
	xor bx,bx
scan_r:	call check_rsdp	; check if DS:BX has RSD PTR structure
	jz found_rsdp
	add bx,16
	cmp bx,0ffe0h
	jnz scan_r	; check 64k BIOS (COULD do even 128k from e000:0)
noacpi:	push cs
	pop ds
	mov ah,9
	mov dx,noacpimsg
	int 21h		; show complaint message
altacpi:
	mov ah,9
	mov dx,contactmsg	; show ad: contact info, THROTTLE, PCISLEEP.
	int 21h		; THROTTLE works w/o ACPI BIOS, using a PCI table.
			; PCISLEEP can find ACPI port w/o BIOS: most chips
			; use "3 bits starting with bit 1" at ACPI base+16.
	mov word [facpptr],1	; flag to avoid trying again later
	stc		; no ACPI found
	ret

inntdosbox:
	mov ah,9
	mov dx,dosboxmsg
	int 21h		; complain if tried to run in WinNT family DOS box
	jmp short altacpi		; ... and show blatant ad
					; (THROTTLE can do WinNT)

found_rsdp:				; assume 386 present from here on
	mov eax,[ds:bx+9]		; ignore checksum [8] and copy OEM
	mov [cs:rsdoem],eax		; name [9] (6 bytes + trailing 00)
	mov ax,[ds:bx+13]
	mov [cs:rsdoem+4],ax
	mov eax,[ds:bx+10h]		; linear RSDT location in RAM
	push cs
	pop ds
	call fetch_high			; load RSDT to thebuffer
	jc noacpi
	mov ax,[thebuffer+4]		; size
	cmp ax,24h			; ... without header ...
	jbe noacpi			; ACPI 0.9 not supported
	mov bx,ax
	sub bx,4			; last pointer
	; header: dd RSDT, size / db revision, checksum / 6db OEMname /
	; 8db OEMtabname / 4db OEMrevbcd / 4db compilerid / 4db compilerrev
	mov eax,[thebuffer+10h]		; copy OEMtabname: is manufacture
	mov [rsdmodel],eax		; model ID in RSDT case...
	mov eax,[thebuffer+14h]
	mov [rsdmodel+4],eax
	xor eax,eax			; NULL item as end marker
	push eax
more_r:	mov eax,[thebuffer+bx]	; linear address of a table
	push eax			; store on stack
	sub bx,4
	cmp bx,24h			; only header left?
	jnb more_r			; else keep collecting
find_facp:
	pop eax
	or eax,eax
	jz near noacpi			; end marker and no FACP yet. Bad.
	mov [facpptr],eax	; might get updated later
	call fetch_high			; ignore headers, load some table
	jc find_facp			; if fetch failed, keep searching
	cmp dword [thebuffer],'FACP'	; FACP found?
	jz facp_found
	jmp short find_facp		; walk through stacked pointers

facp_found:
	pop eax				; consume unused pointers on stack
	or eax,eax			; end marker?
	jnz facp_found			; else stack is not empty yet
	xor ax,ax
	or ax,[thebuffer+32h]		; high word of SMI port
	or ax,[thebuffer+3ah]		; high word of PM1a event port
	or ax,[thebuffer+3ch]		; any PM1b event port?
	or ax,[thebuffer+3eh]		; any PM1b event port?
	or ax,[thebuffer+42h]		; high word of PM1a control reg block
	or ax,[thebuffer+44h]		; any PM1b control port?
	or ax,[thebuffer+46h]		; any PM1b control port?
	; we ignore: 48 dd PM2 control port, 4c dd PM timer port
	; 50 dd GPE0 reg block (port base), 54 dd GPE1 ...
	; ... and block sizes: 58..5d of pm1a/b evt, pm1a/b ctrl,
	; pm2 ctrl, pm timer, gpe0, gpe1. 5e is gpe0->1 event number offset
	;
	; MAYBE useful: 60 dw C2 latency, 62 dw C3 latency: C2 / C3 above
	; 100 / 1000 resp. means "not supported". Problem: C3 usage can
	; require some CPU state save / restore work by the Op.Sys.!?
	; C2 saves more energy than normal STI/HLT sequence (C1 state).
	; S1 state is defined to involve STPCLK#, so CPU goes Cn, n > 0.
	;
	; 64/66 dw for cache spill (used if WBINVD is broken)
	jz good_acpi			; any disturbing non-zero fields
	mov ah,9			; which would need extra handling?
	mov dx,badacpimsg
	int 21h				; then give up and complain!
	mov dword [facpptr],1		; flag to avoid trying again later
	stc
	ret

good_acpi:
	mov eax,[thebuffer+24h]		; FACS address
	mov [facsptr],eax
	mov eax,[thebuffer+28h]		; DSDT address
	mov [dsdtptr],eax
%if CURIOUS_ACPI
	mov ax,[thebuffer+2eh]
	mov [smi_irq],ax		; SMI IRQ (triggered by ACPI/SMI)
	call showax
	mov ax,[thebuffer+30h]		; (high word checked later)
	mov [smi_io],ax			; SMI command port (to control SMI)
	call showax
	mov ax,[thebuffer+34h]
	mov [smi_flg],ax		; low/high: SMI command ACPI/SMI mode
	call showax
	; 36: SMI command for S4BIOS (triggers BIOS assisted S3->S4...)
%endif
	mov ax,[thebuffer+38h]		; (high word checked later)
	mov [pm1aEvt],ax		; PowerMgmt 1a event reg block
	add ax,10h			; normal pm1a_evt -> p_blk offset :-)
	mov [pblk_io],ax		; THROTTLE port base
	mov ax,[thebuffer+40h]		; (high word checked later)
	mov [pm1aCnt],ax		; PowerMgmt 1a control reg block
	mov ax,[thebuffer+68h]
	cmp ah,3		; duty cycle setting defined?
	jae tht_okay
	push ax
	mov ah,9
	mov dx,thtwarnmsg
	int 21h
	pop ax
	call showax
	mov ah,9
	mov dx,thtwarnmsg2
	int 21h
	mov ax,301h	; IGNORE BIOS duty cycle info and use "3 bits, shl 1"!
tht_okay:
	mov [thtbits],ax		; low: duty cycle shift value
tht_ign:
					; high: duty cycle bit count
	; 6a/6b/6c: if nonzero, RTC alarm reg numbers for day/month/century
	; 5f/6d reserved, 6e dd flags: bit 0 "wbinvd supported",
	; 1 "wbinvd needs cache cheat", 2 "all CPUs can do C1 sleep"
	; 3 "C2 configured for multi-CPU" (4..6 differ for ACPI 0.9...)
	; 4 power button as ctrl method , 5 sleep button: on = ctrl method
	; or none, else fixed feature... 6 no rtc wake support in fixed
	; reg space, 7 rtc can wake from S4, 8 timer is 32 (not 24) bits.
	mov eax,[dsdtptr]		; fetch DSDT pointer
	or eax,eax			; null pointer check
	jz nodsdt
	call fetch_high
	jc nodsdt			; access failed, bad luck
	cmp dword [thebuffer],'DSDT'
	jnz nodsdt		; next are dd size, db version, db checksum
; --- 	mov eax,[thebuffer+10]		; usually e.g. chipset name
; --- 	mov [dsdtoem],eax		; ... often equal to RSDT OEM name
; --- 	mov ax,[thebuffer+14]
; --- 	mov [dsdtoem+4],ax
	call analyze_dsdt		; find Sn state codes for poweroff
					; ... and allow pblk_io override
nodsdt:	mov ah,9
	mov dx,acpimsg
	int 21h				; show some names which we found
	mov ax,[pm1aEvt]	; usually general ACPI port base
	call showax
	mov dx,sizeDSDTmsg
	mov ah,9
	int 21h
	mov ax,[dsdtsz]		; yes, we ignore the high 16 bits.
	call showax		; only low 16 bits displayed... 64 kB max.
	mov ah,9
	mov dx,dotcrlfmsg	; add ".",13,10,"$"
	int 21h
	clc				; everything okay
	ret

	; -------------

check_rsdp:				; returns ZF if "RSD PTR " at ds:bx
	cmp word [ds:bx],'RS'
	jnz no_rsdp
	cmp word [ds:bx+2],'D '
	jnz no_rsdp
	cmp word [ds:bx+4],'PT'
	jnz no_rsdp
	cmp word [ds:bx+6],'R '
	jnz no_rsdp
no_rsdp:
	ret

	; -------------

analyze_dsdt:				; DSDT is a very complex byte stream
	; you would need a virtual machine and name space tree model and
	; other stuff (Intel talks about a 70k library) to handle all, but
	; we only look for _S1_ and _S5_ in static definition block, which
	; contain 2-3 byte packages starting with the PM1a byte constant
	; for the hardware state number for S1 (or S5). Just grabbing a
	; constant from the "program" is quite easy ;-).
	;
	; Names can be: "abcd", ".abcdefgh", "/"+nn+"nn times 4 chars",
	; where 4 char blocks use A-Z 0-9 _, _ padded, must start with A-Z _
	; ... can be prefixed by "\" (root) or 1 or more "^" (parent).
	; We just assume that "\_Sn_" is used, not "^_Sn_" or plain "_Sn_".
	; Sizes: 00..3f or 4x yy or 8x yyyy or cx yyyyyy
	;
	mov eax,[thebuffer+4]		; DSDT size including header
	mov [dsdtsz],eax		; store for later
small_dsdt:
	mov cx,ax			; scan size
	mov al,'1'
	mov di,s1mode
	push cx
	call find_Sn			; scan for \_S1_ (standby) and store s1mode
	pop cx	; *** now scan for \_S3_ (suspend to RAM) if no _S1_??? ***
	mov al,'5'
	mov di,s5mode
	push cx
	call find_Sn			; scan for \_S5_ (power off) and store s5mode
	pop cx
	mov di,pblk_io
	call find_pblk	; scan for ProcessorOp(s)...
	;
	mov al,[s5mode]			; mode found? (needed for ACPIOFF)
	or al,[s1mode]			; mode found? (S1 mode only needed for SPEED0)
	cmp al,-1			; if not both found, AL is -1
	jnz s1s5_found
	mov ax,[dsdtsz]		; ignoring high part - max 64k supported.
	mov dx,noSnDSDTmsg
	mov ah,9
	int 21h
	mov ah,9
	mov dx,contactmsg
	int 21h
	mov dword [facpptr],1		; flag to avoid trying again later
	clc			;  if we got that far, throttle/1..8 should
	ret			; work, but acpioff and throttle/0 will fail.

s1s5_found:
	mov al,[s1mode]
	add al,'0'
	mov [s1msg],al
	mov al,[s5mode]
	add al,'0'
	mov [s5msg],al
	mov ah,9
	mov dx,s1s5msg
	int 21h
	clc
	ret

	; -------------

find_Sn:				; find \_S?_ in CX size thebuffer
	; with ? being AL from user. Store first byte item to var at DI...
	; _Sn info block: (if size were > 3F, it would use n-byte encoding)
	; 08 (DefName) "\_Sn_" 12 (PackageOp) nn (size) 02 (item count)
	; ITEM ITEM, where ITEM can be: 0, 1, -1 constants or 0A xx for
	; "byte xx" (0B xxxx / 0C xxxxxxxx would be word/dword...)
	push cx
	push si
	push eax
	mov si,thebuffer+24h	; base (header skipped)
	sub cx,24h+10		; consider header and target size
	jc no_Sn
%if 0		; already checking in fetch_high anyway
	push si		; rough check for buffer overflow...
	add si,1024		; desired left-over stack size
	sub si,sp		; add (64k-sp), already in-use stack size
	add si,cx		; buffer size
	pop si
	jc no_Sn		; buffer would overflow stack
%endif
	shl ax,8
	mov al,'S'	; "Sn" now
	shl eax,16
	mov ax,'\_'	; EAX is "\_Sn" now
scan_S:	cmp [si],eax
	jz maybe_Sn
half_Sn:
	inc si
	loop scan_S
	jmp short no_Sn
maybe_Sn:
	cmp word [si+4],'_'+1200h	; Package at \_Sn_?
	jnz half_Sn	; looked good, but was not the real thing
	cmp byte [si+6],3fh
	ja half_Sn	; that is implausibly big
;	cmp byte [si+7],2
;	jnz half_Sn	; _Sn should always contain 2 items
		; (items are PM1a and PM1b state codes)
	mov ax,[si+8]	; first 2 bytes should be enough
		; (we only want to know the PM1a state code)
	or al,al	; "zero"?
	jz got_Sn
	cmp al,1	; "one"?
	jz got_Sn
	cmp al,-1	; "minus one"?
	jz got_Sn_min1
	cmp al,0ah	; "byte constant"?
	jnz no_Sn	; we cannot parse other variants
	mov al,ah	; constant is in next byte
	jmp short got_Sn
got_Sn_min1:
	mov al,7	; Sn values must be 3 bit values
got_Sn:	mov [di],al	; YES, we found a value!
no_Sn:
	pop eax
	pop si
	pop cx
	ret

find_pblk:				; find \_PR_ in CX size thebuffer
	; and store Processor p_blk to word at DI if not 0
	; *** TODO: pblk_io override based on parsing _PR_ information ***
	; Processor block info, size by1, P_BLK at dd, size by2 (0 or 4..6) ->
	; "\_PR_" 5B (prefix) 83 (ProcessorOp) by1 "\._PR_CPU0" 01 dd by2
	; *** TODO *** TODO *** TODO ***
	ret

	; -------------

fetch_high:				; copy ??? bytes from EAX to thebuffer
	push eax			; ??? value is taken from copied data [4]
	mov [gdt_src+2],ax		; low part
	shr eax,16
	mov [gdt_src+4],al		; mid part
	mov word [gdt_src+5],93h	; data segment
	mov [gdt_src+7],ah		; high part
	xor eax,eax
	mov ax,cs
	shl eax,4
	add eax,thebuffer		; linear buffer address
	mov [gdt_dst+2],ax		; low part
	shr eax,16
	mov [gdt_dst+4],al		; mid part
	mov word [gdt_dst+5],93h	; data segment
	mov [gdt_dst+7],ah		; high part
	push cx
	push si
	mov cx,16	; FIRST, we only fetch the header
	mov word [gdt_src],cx	; in theory, cx+cx-1, but with emm386,
	mov word [gdt_dst],cx	; we need one more (cx+cx) here!?
	shr cx,1			; copy BUFSIZE/2 words
	mov si,gdt			; structure offset (in ES)
	mov ah,87h			; high memory copy
	int 15h				; misc BIOS services
	jc failed_fetch
	;
	mov cx,[thebuffer+4]	; read table size from header
	inc cx	; SECOND step: we fetch ALL the data (round up to words)
	jz toobig_fetch
	and cx,0fffeh
	xor ax,ax
	cmp ax,[thebuffer+6]
	jz acceptable_fetch
toobig_fetch:
	stc
	jmp short failed_fetch
acceptable_fetch:
	mov ax,cx
	add ax,thebuffer
	jc toobig_fetch
	add ax,1024		; keep some stack space untouched
	jc toobig_fetch
	cmp sp,ax		; SP marks end of useable memory
	jb toobig_fetch
	;
	mov word [gdt_src],cx	; in theory, cx+cx-1, but with emm386,
	mov word [gdt_dst],cx	; we need one more (cx+cx) here!?
	shr cx,1			; copy BUFSIZE/2 words
	mov si,gdt			; structure offset (in ES)
	mov ah,87h			; high memory copy
	int 15h				; misc BIOS services
failed_fetch:
	pop si
	pop cx
	pop eax
	ret				; returns CY set if error

	; -------------

	; *** useful constants taken from FACP ***
facpptr	dd 0				; location of FACP itself
facsptr	dd 0				; not really used for FDAPM
dsdtptr	dd 0	; for S1/S5 prep	; has to be parsed for poweroff
%if CURIOUS_ACPI
smi_irq	dw 0				; not yet used
smi_io	dw 0				; not yet used
smi_flg	dw 0				; not yet used
%endif
pm1aEvt	dw 0				; PowerMgmt 1a event port base
	; contains 2 words (or dwords, if block size is 8 bytes, rare!)
	; 1. "status" 2. "enable"
pm1aCnt	dw 0	; for S1/S5...		; PowerMgmt 1a control port base
thtbits	dw 301h	; for throttle		; CPU duty cycle: low offs, high size
pblk_io	dw 0	; for throttle		; CPU port base (default pm1aEvt+10)

	; *** useful constants taken from DSDT ***
dsdtsz	dd 0	; size of DSDT (used for "too big" message)
	; s0mode is not used, as wake-from-S1 should mean S0 anyway
s1mode	db -1	; pm1aCnt mode number for S1 sleep - you SHOULD first
	; put all PCI/AGP devices to sleep, too, as far as supported.
	; s2mode is optional, s3mode is only used if s3 differs from
	; "suspend to RAM by shutting down all devices and go to S1"
	; s4mode is often equal to S4 (suspend to disk) mode...
s5mode	db -1	; pm1aCnt mode number for S5 soft-off
	; (at least one of S1...S5 must be possible)
	; DSDT can also override pblk_io

acpimsg		db "ACPI for '"
rsdoem		db "??????' found: "
rsdmodel	db "????????"	; --- (DSDT for '"
; --- dsdtoem		db "??????'), port base $"
		db ", port base 0x$"
sizeDSDTmsg	db ", DSDT size 0x$"
dotcrlfmsg	db "."
crlfmsg		db 13,10,"$"

s1s5msg		db "ACPI mode codes from DSDT: S1 (sleep) is "
s1msg		db "?, S5 (soft-off) is "
s5msg		db "?.",13,10,"$"

dosboxmsg	db "True DOS version 5.50 - WinNT/2k/XP DOS box: No ACPI."
		db 13,10,"$"

noSnDSDTmsg	db "ACPI S1 / S5 info not in DSDT, check BIOS CMOS setup."
		db 13,10,"$"
badacpimsg	db "I/O base must be 16bit, no PM1b.",13,10,"$"
thtwarnmsg	db "WARNING: BIOS throttle info '$"
thtwarnmsg2	db "' rejected, SPEEDn will use '301' instead.",13,10,"$"
noacpimsg	db "No ACPI (?) - check BIOS CMOS setup.",13,10,"$"
contactmsg	db "Try PCISLEEP or try www.oldskool.org/pc/throttle/"
		db " by Jeff Leyda,",13,10
		db "or contact: eric at coli.uni-sb.de",13,10,"$"


	align 8
gdt:	dw 0, 0, 0, 0			; NULL descriptor
	dw 0, 0, 0, 0			; for BIOS (GDT alias?)
gdt_src	dw 0, 0, 0, 0			; descriptor for source
gdt_dst	dw 0, 0, 0, 0			; descriptor for destination
	; NOTE: first word should be LIMIT, but some int 15.87 want SIZE!?
	; So we use the value (CX*2) instead of (CX*2)-1 here...
	dw 0, 0, 0, 0			; for BIOS (CS?)
	dw 0, 0, 0, 0			; for BIOS (SS?)

thebuffer:		; label for (8kb) buffer, must be at end of code!


