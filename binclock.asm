; binclock for msx2~
; setup asm: https://github.com/teedjay/msx-graphics/blob/main/teedjay.asm
;==============================================================
; WLA-DX bank and slot setup
; BIOS starts at $0000
; ROM starts at $4000 in a real MSX (or $8000)
; RAM should be available at $C000 (MSX has at least 16kb)
; VRAM has 16kb and is accessible thru I/O ports only
;
; wla-z80 binclock.asm
; ;create binclock.link
; wlalink binclock.link binclock.rom
; openmsx -machine Panasonic_FS-A1WX -cart binclock.rom
;============================================================== 
.memorymap 
    defaultslot 0 
    slotsize $4000 
    slot 0 $4000
.endme

.rombankmap 
    bankstotal 1 
    banksize $4000 
    banks 1 
.endro

;HOOK ENTRY
.DEFINE H_TIMI $FD9F ; the VDP interrupt hook (called from HKEYI routine)
;BIOS ENTRY
.DEFINE WRTVRM $004D
.DEFINE FILVRM $0056
.DEFINE LDIRVM $005C
.DEFINE CHGMOD $005F
.DEFINE CLS    $00C3
.DEFINE SNSMAT $0141
.DEFINE KILBUF $0156
.DEFINE EXTROM $015F    ;msx2~
.DEFINE REDCLK $01F5    ;msx2~
	;C=xxbbaaaa
    ;bb=00 BLOCK
    ;aaaa=register
    ;aaaa=5 HOUR10
    ;aaaa=4 HOUR1
    ;aaaa=3 MIN10
    ;aaaa=2 MIN1
    ;aaaa=1 SEC10
    ;aaaa=0 SEC1
;SYSTEM WORKAREA
.DEFINE FORCLR $F3E9
.DEFINE BAKCLR $F3EA
.DEFINE BDRCLR $F3EB
.DEFINE RG1SAV $F3E0
; program specific
.EQU COLAREA $200C
.EQU COLSIZE 18     ; BBUFH+BBUFM+BBUFS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RAM
.ENUM $C000
    OLDHOK  dsb 5
    buf     db
    buf2    db
    buf3    db
    buf4    db
    BUFESC  db
    bufst   db
    BBUFH   dsb 6
    BBUFM   dsb 6
    BBUFS   dsb 6
.ENDE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


.bank 0 slot 0 
.org $0000 ; ROM binary starts at $0000 (inserted ROM on MSX will be placed at $4000)

; MSX 16 byte header
; +0	ID	Put these first two bytes at 041H and 042H ("AB") to indicate that it is an additional ROM.
; +2	INIT	Address of the routine to call to initialize a work area or I/O ports, or run a game, etc.
; +4	STATEMENT	Runtime address of a program whose purpose is to add instructions to the MSX-Basic using CALL.
; +6	DEVICE	Execution address of a program used to control a device built into the cartridge. For example, a disk interface.
; +8	TEXT	Pointer of the tokenizen Basic program contained in ROM.
; +10	Reserved	6 bytes reserved for future updates.

MSX_ROM:
    .db "AB"
    .dw START
    .dw 0
    .dw 0
    .dw 0
    .db 0,0,0,0,0,0
START:
    call    SCR
    call    CLS
    call    SETFONT
    call    SETCLOCKFACE
    call    HOOKTIMI
    call    INIT
LOOP:
    jr      LOOP
    ret
    ;xxx
SCR:
    ; COLOR 15,4,7
    ; COLOR 15,14,1
    ; COLOR 15,1,14
    ld		a,15
    ld		(FORCLR), a
    ld		a,14
    ld		(BAKCLR), a
    ld      a,1
    ld		(BDRCLR), a
    ; SCREEN 1
    ; VDP(1) = (VDP(1) AND &HFC) OR 0x00		※スプライトサイズ 8x8, ２倍拡大無し
    ld		a, (RG1SAV)
    and     $FC
    or		$00
    ld		(RG1SAV), a
    ld		a, 1			; SCREEN 1
    call	CHGMOD
    ret

HOOKTIMI:
	ld hl,H_TIMI
    ld de,OLDHOK
    ld bc,5
    LDIR
    
    DI
    LD A,0c3h
    ld (H_TIMI),a
    ld de,INTVAL
    ld (H_TIMI+1),de
    EI
    ret

UNHOOKTIMI:
    DI
    ld hl,OLDHOK
    ld de,H_TIMI
    ld bc,5
    LDIR
    EI
	RET

;set font
; FORI=0to17:a=&h300+i*64:forj=0to7:vpokea+j,255:next:next
SETFONT:
    ld      A,18
    ld      DE,$300
SETFONTLP:
    ld      HL,FONTDEF
    ld      BC,8
    push    AF
    push    DE
    call    LDIRVM
    pop     DE
    pop     AF
    ld      HL,64
    add     HL,DE
    ex      DE,HL   ; ld DE,HL
    dec     a
    or      a ;cp 0
    jr      nz,SETFONTLP
    ;font color initialize
    ld      HL,COLAREA
    ld      BC,COLSIZE
    ld      a,$11 
    call    FILVRM
    ret

SETCLOCKFACE:
    ;ld      HL,CLOCKFACE
    ld      HL,CLOCKFACE2
    ld      DE,$1800
    ld      BC,$300
    call    LDIRVM
    ret

INIT:
    ;initialize RAM workarea
    XOR A
    ld  B,14
    ld  HL,buf
INITLP:
    ld  (HL),A
    inc HL
    djnz INITLP
    ; for values neq 0
    ld a,255
    ld (BUFESC),a
    LD A,255
    ld (bufst),A
MAINLP:    
    LD A,(BUFESC)
    OR A ;...CP 0
    JP NZ,MAINLP
    ;end
    CALL KILBUF
    JP UNHOOKTIMI

; called from H.TIMI
INTVAL:
	PUSH AF
    ;PUSH BC
    PUSH DE
    PUSH HL
	CALL KEYESC
    ;
    CALL GETTIME
	;to vram
    LD HL,BBUFH
    LD DE,COLAREA
    LD BC,COLSIZE
    CALL LDIRVM
    ;
	POP HL
    POP DE
    ;POP BC
    POP AF
    call OLDHOK
    RET

;
; gettime
;
GETTIME:
	ld b,60
    ;timer1
	ld HL,buf
	CALL TIMER
    OR A ;...CP 0
    RET NZ
    ;
    ;READ BIT COLOR
 	ld DE,(BITCOL)

	;block1 reg10
	;12H/24H,AM/PM
    LD C,%00011010
	LD IX,REDCLK
    CALL EXTROM
    LD C,D ;24HorAM
    LD B,A
    AND %01
    JR NZ,MODE24
    ;AM/PM bit
    LD A,B
    AND %010
    JR Z,MODE24
    LD C,E ;12HandPM
MODE24:
	LD HL,BBUFH
    LD (HL),C

	;hour:blk0 reg4,5
    LD C,%0000101
    CALL GETCLK
;	LD (TBUFH),A    ;dbg
    LD HL,BBUFH+5
    LD B,5
	CALL ATOBIN

	;minute:blk0 reg2,3
    LD C,%0000011
    CALL GETCLK
;	LD (TBUFM),A    ;dbg
    LD HL,BBUFM+5
    LD B,6
	CALL ATOBIN

	;second:blk0 reg0,1
    LD C,%0000001
    CALL GETCLK
;	LD (TBUFS),A    ;dbg
    LD HL,BBUFS+5
    LD B,6
	CALL ATOBIN

    ; gettime end
    ret

; read two 4-bit reg from clock ic(C and C-1) and convert to a byte
; IN C - ClockIC param
; OUT A
; USES A,BC
GETCLK:
	LD IX,REDCLK
    CALL EXTROM
    LD B,A
	;read next address
	DEC C
    CALL EXTROM
    ;don't add 10 if B=0
    INC B
    JR GETCLKLE
GETCLKLP:
	ADD A,10
GETCLKLE:
	DJNZ GETCLKLP
	RET

;timer60
; IN HL - counter wk
;    B - counter max
; OUT A - counter
; USES A
TIMER:
    ld a,(HL)
    INC A
    CP B
    JR NZ,TIMERUPD
	XOR A ;...LD A,0
TIMERUPD:
    ld (HL),A
    RET

;check esc
; OUT Z (0 if pressed)
KEYESC:
	LD A,7
    CALL SNSMAT
    ;esc key
    AND %00000100
    LD (BUFESC),A
	RET

; puts A converted  to binary to (HL) backwords
; bit6..210=>HL-6,..,HL-2,HL-1,HL
; IN B - num of bits
;    D - color data for bit off
;    E - color data for bit on
;    HL - end adr
; MOD AF,BC,HL
ATOBIN: 
    LD C,D
	RRCA
	JR NC,ABZERO
    LD C,E
ABZERO:
	LD (HL),C
    DEC HL
    DJNZ ATOBIN
	RET

FONTDEF:
    .db $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF

CLOCKFACE:
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$60,$60,$60,$20,$68,$68,$68,$20,$70,$70,$70,$20
    .db $78,$78,$78,$20,$80,$80,$80,$20,$88,$88,$88,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$60,$60,$60,$20,$68,$68,$68,$20,$70,$70,$70,$20
    .db $78,$78,$78,$20,$80,$80,$80,$20,$88,$88,$88,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$60,$60,$60,$20,$68,$68,$68,$20,$70,$70,$70,$20
    .db $78,$78,$78,$20,$80,$80,$80,$20,$88,$88,$88,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$60,$60,$60,$20,$68,$68,$68,$20,$70,$70,$70,$20
    .db $78,$78,$78,$20,$80,$80,$80,$20,$88,$88,$88,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$90,$90,$90,$20,$98,$98,$98,$20,$A0,$A0,$A0,$20
    .db $A8,$A8,$A8,$20,$B0,$B0,$B0,$20,$B8,$B8,$B8,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$90,$90,$90,$20,$98,$98,$98,$20,$A0,$A0,$A0,$20
    .db $A8,$A8,$A8,$20,$B0,$B0,$B0,$20,$B8,$B8,$B8,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$90,$90,$90,$20,$98,$98,$98,$20,$A0,$A0,$A0,$20
    .db $A8,$A8,$A8,$20,$B0,$B0,$B0,$20,$B8,$B8,$B8,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$90,$90,$90,$20,$98,$98,$98,$20,$A0,$A0,$A0,$20
    .db $A8,$A8,$A8,$20,$B0,$B0,$B0,$20,$B8,$B8,$B8,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$C0,$C0,$C0,$20,$C8,$C8,$C8,$20,$D0,$D0,$D0,$20
    .db $D8,$D8,$D8,$20,$E0,$E0,$E0,$20,$E8,$E8,$E8,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$C0,$C0,$C0,$20,$C8,$C8,$C8,$20,$D0,$D0,$D0,$20
    .db $D8,$D8,$D8,$20,$E0,$E0,$E0,$20,$E8,$E8,$E8,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$C0,$C0,$C0,$20,$C8,$C8,$C8,$20,$D0,$D0,$D0,$20
    .db $D8,$D8,$D8,$20,$E0,$E0,$E0,$20,$E8,$E8,$E8,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$C0,$C0,$C0,$20,$C8,$C8,$C8,$20,$D0,$D0,$D0,$20
    .db $D8,$D8,$D8,$20,$E0,$E0,$E0,$20,$E8,$E8,$E8,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20

CLOCKFACE2:
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$41,$4D,$2F,$50,$4D,$20,$48,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$60,$60,$60,$20,$68,$68,$68,$20,$70,$70,$70,$20,$78,$78,$78,$20,$80,$80,$80,$20,$88,$88,$88,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$60,$60,$60,$20,$68,$68,$68,$20,$70,$70,$70,$20,$78,$78,$78,$20,$80,$80,$80,$20,$88,$88,$88,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$60,$60,$60,$20,$68,$68,$68,$20,$70,$70,$70,$20,$78,$78,$78,$20,$80,$80,$80,$20,$88,$88,$88,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$60,$60,$60,$20,$68,$68,$68,$20,$70,$70,$70,$20,$78,$78,$78,$20,$80,$80,$80,$20,$88,$88,$88,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$4D,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$90,$90,$90,$20,$98,$98,$98,$20,$A0,$A0,$A0,$20,$A8,$A8,$A8,$20,$B0,$B0,$B0,$20,$B8,$B8,$B8,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$90,$90,$90,$20,$98,$98,$98,$20,$A0,$A0,$A0,$20,$A8,$A8,$A8,$20,$B0,$B0,$B0,$20,$B8,$B8,$B8,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$90,$90,$90,$20,$98,$98,$98,$20,$A0,$A0,$A0,$20,$A8,$A8,$A8,$20,$B0,$B0,$B0,$20,$B8,$B8,$B8,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$90,$90,$90,$20,$98,$98,$98,$20,$A0,$A0,$A0,$20,$A8,$A8,$A8,$20,$B0,$B0,$B0,$20,$B8,$B8,$B8,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$53,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$C0,$C0,$C0,$20,$C8,$C8,$C8,$20,$D0,$D0,$D0,$20,$D8,$D8,$D8,$20,$E0,$E0,$E0,$20,$E8,$E8,$E8,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$C0,$C0,$C0,$20,$C8,$C8,$C8,$20,$D0,$D0,$D0,$20,$D8,$D8,$D8,$20,$E0,$E0,$E0,$20,$E8,$E8,$E8,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$C0,$C0,$C0,$20,$C8,$C8,$C8,$20,$D0,$D0,$D0,$20,$D8,$D8,$D8,$20,$E0,$E0,$E0,$20,$E8,$E8,$E8,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$C0,$C0,$C0,$20,$C8,$C8,$C8,$20,$D0,$D0,$D0,$20,$D8,$D8,$D8,$20,$E0,$E0,$E0,$20,$E8,$E8,$E8,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20
    .db $20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$20,$FF

BITCOL:
	.db	$81 ; on
    .db	$11	; off

