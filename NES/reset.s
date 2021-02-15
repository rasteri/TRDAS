; startup code for cc65/ca65

.export __STARTUP__:absolute=1

; linker-generated symbols

.include "zeropage.inc"

; definitions
PPU_CTRL      = $2000
PPU_MASK      = $2001
PPU_STATUS    = $2002
OAM_ADDRESS   = $2003
PPU_ADDRESS     = $2006
PPU_DATA        = $2007
PPU_SCROLL       = $2005
OAM_DMA       = $4014
APU_DMC       = $4010
APU_STATUS    = $4015
APU_FRAME_CTR = $4017
CNTRL1          = $4016
CNTRL2          = $4017

.segment "ZEROPAGE"

P1D0err:       .res 1
P1D3err:       .res 1
P2D0err:       .res 1
P2D3err:       .res 1
p1d0:              .res 1
p1d3:               .res 1
p2d0:               .res 1
p2d3:               .res 1
p1d0next:              .res 1
p1d3next:               .res 1
p2d0next:               .res 1
p2d3next:               .res 1
p1d0errs:              .res 1
p1d3errs:               .res 1
p2d0errs:               .res 1
p2d3errs:               .res 1
RPF:               .res 1
temp:    .res 1


; no variables

.segment "HEADER"

; iNES header
; see http://wiki.nesdev.com/w/index.php/INES

.byte $4e, $45, $53, $1a ; "NES" followed by MS-DOS EOF
.byte $01                ; size of PRG ROM in 16 KiB units
.byte $01                ; size of CHR ROM in 8 KiB units
.byte $00                ; horizontal mirroring, mapper 000 (NROM)
.byte $00                ; mapper 000 (NROM)
.byte $00                ; size of PRG RAM in 8 KiB units
.byte $00                ; NTSC
.byte $00                ; unused
.res 5, $00              ; zero-filled

.segment "STARTUP"

; initialize RAM and jump to C main()
start:
    sei ; ignore IRQs
    cld ; disable decimal mode

    ; disable APU frame IRQs
    ldx #$40
    stx APU_FRAME_CTR

    ; setup stack
    ldx #$ff
    txs

    inx ; x = $00
    stx PPU_CTRL ; disable NMI
    stx PPU_MASK ; disable rendering
    stx APU_DMC  ; disable DMC IRQs

    ; If the user presses reset during vblank, the PPU may reset
    ; with the vblank flag still true. This has about a 1 in 13
    ; chance of happening on NTSC or 2 in 9 on PAL. Clear the
    ; flag now so the @vblankwait1 loop sees an actual vblank.
    bit PPU_STATUS

    ; First of two waits for vertical blank to make sure that the
    ; PPU has stabilized
@vblank_wait_1:
    bit PPU_STATUS
    bpl @vblank_wait_1

    ; We now have about 30,000 cycles to burn before the PPU stabilizes.

    stx APU_STATUS ; disable music channels

    ; We'll fill RAM with $00.
    txa
@clear_ram:
    sta $00,   x
    sta $0100, x
    sta $0300, x
    sta $0400, x
    sta $0500, x
    sta $0600, x
    sta $0700, x ; Remove this if you're storing reset-persistent data

    ; We skipped $0200, x on purpose. Usually, RAM page 2 is used for the
    ; display list to be copied to OAM. OAM needs to be initialized to
    ; $ef-$ff, not 0, or we'll get a bunch of garbage sprites at (0, 0).

    inx
    bne @clear_ram

	; Initialize OAM data in $0200 to have all y coordinates off-screen
	; (e.g. set every fourth byte starting at $0200 to $ef)
	lda #$ef
@clear_oam:
	sta $0200, x

	inx
    inx
	inx
	inx
	bne @clear_oam

    ; Second of two waits for vertical blank to make sure that the
    ; PPU has stabilized
@vblank_wait_2:
    bit PPU_STATUS
    bpl @vblank_wait_2

	; initialize PPU OAM
    stx OAM_ADDRESS ; $00
    lda #$02 ; use page $0200-$02ff
    sta OAM_DMA


	lda PPU_STATUS ; reset the PPU latch

    ;

    ; set palette to black background and white everything else
	lda     #$3F
	sta     PPU_ADDRESS
	lda     #$00
	sta     PPU_ADDRESS

    lda     #$0f
    sta     PPU_DATA

    lda     #$20    ; fill other 15 palette entries with white
    ldy     #15         
@loopp:
    sta     PPU_DATA
    dey
    bne     @loopp


    ; draw text
    lda     #$21
	sta     PPU_ADDRESS
    lda     #$64
	sta     PPU_ADDRESS
    ldy #$00
@loopt:
    lda TEXT, Y
    beq @tfin
    sta PPU_DATA
    iny
    jmp @loopt
@tfin:


    ; enable PPU rendering and NMI
    lda     #$80
	sta     PPU_CTRL
    lda     #$1E
	sta     PPU_MASK

    lda    #$00
    sta     PPU_SCROLL
    sta     PPU_SCROLL


@loop4eva:
    ;jmp @loop4eva
    jmp mainloop


readcntrl:
    ; strobe controllers
    ldx #$01
    stx CNTRL1
    dex
    stx CNTRL1

    ldy #08         ; loop over all 8 buttons
@loop:
    lda CNTRL1     ; read button state
    and #$03        ; mask lowest 2 bits
    cmp #$01        ; set carry bit to button state
    rol p1d0         ; rotate carry bit into button var
    asl p1d0        ; rotate vars
    asl p1d3
    asl p2d0
    asl p2d3
    lda CNTRL1      ; read button state
    sta temp        ; store in temp var

    lda #$01        ; see if P1d0 is set
    bit temp
    beq @z1         
    inc p1d0
@z1:

    lda #$08        ; see if P1d3 is set
    bit temp
    beq @z2         
    inc p1d3
@z2:

    lda CNTRL2     ; repeat for second controller
    sta temp        ; store in temp var

    lda #$01        ; see if P1d0 is set
    bit temp
    beq @z3         
    inc p2d0
@z3:

    lda #$08        ; see if P1d3 is set
    bit temp
    beq @z4         
    inc p2d3
@z4:
    dey
    bne @loop

rts


mainloop:
    @loopage:
    jsr readcntrl

    ldx p1d0next    ; load next expected reading
    cpx p1d0        ; is current reading as expected?
    beq @z1         ; if yes, branch to z1
    inc P1D0err  ; else increment error counter
    ldx p1d0        ; and set next expected reading to current
    @z1:
    inx             ; increment and store next expected reading
    cpx #$ff        ; if value is FF loop back to zero
    bne @z12
    ldx #$0
    @z12:
    stx p1d0next

    ldx p1d3next    ; load next expected reading
    cpx p1d3        ; is current reading as expected?
    beq @z2         ; if yes, branch to z1
    inc P1D3err  ; else increment error counter
    ldx p1d3        ; and set next expected reading to current
    @z2:
    inx             ; increment and store next expected reading
    cpx #$ff        ; if value is FF loop back to zero
    bne @z22
    ldx #$0
    @z22:
    stx p1d3next

    ldx p2d0next    ; load next expected reading
    cpx p2d0        ; is current reading as expected?
    beq @z3         ; if yes, branch to z1
    inc P2D0err  ; else increment error counter
    ldx p2d0        ; and set next expected reading to current
    @z3:
    inx             ; increment and store next expected reading
    cpx #$ff        ; if value is FF loop back to zero
    bne @z32
    ldx #$0
    @z32:
    stx p2d0next

    ldx p2d3next    ; load next expected reading
    cpx p2d3        ; is current reading as expected?
    beq @z4         ; if yes, branch to z1
    inc P2D3err  ; else increment error counter
    ldx p2d3        ; and set next expected reading to current
    @z4:
    inx             ; increment and store next expected reading
    cpx #$ff        ; if value is FF loop back to zero
    bne @z42
    ldx #$0
    @z42:
    stx p2d3next


    inc RPF


    jmp @loopage

_StartLoop:
    jmp mainloop

printhex: ; print hex representation of value in X register

    txa
    lsr     a
	lsr     a
	lsr     a
	lsr     a
    clc
    adc #$30
    sta PPU_DATA
    txa
    and #$0f
    clc
    adc #$30
    sta PPU_DATA


    rts


; interrupt handler
nmi:
	pha
	txa
	pha
	tya
	pha

    ; print reads per frame count
    lda #$21
    sta PPU_ADDRESS
    lda #$6B
    sta PPU_ADDRESS
    ldx RPF
    jsr printhex

    ; print P1D0 error count
    lda #$21
    sta PPU_ADDRESS
    lda #$8B
    sta PPU_ADDRESS
    ldx P1D0err
    jsr printhex

    ; print P1D3 error count
    lda #$21
    sta PPU_ADDRESS
    lda #$AB
    sta PPU_ADDRESS
    ldx P1D3err
    jsr printhex

    ; print P2D0 error count
    lda #$21
    sta PPU_ADDRESS
    lda #$CB
    sta PPU_ADDRESS
    ldx P2D0err
    jsr printhex

    ; print P2D3 error count
    lda #$21
    sta PPU_ADDRESS
    lda #$EB
    sta PPU_ADDRESS
    ldx P2D3err
    jsr printhex

    ; zero RPF
    lda #$00
    sta RPF



    lda    #$00
    sta     PPU_SCROLL
    sta     PPU_SCROLL

    pla
	tay
	pla
	tax
	pla
irq:
    rti

.segment "RODATA"

TEXT: .asciiz   "RPF  -                          P1D0 -                          P1D3 -                          P2D0 -                          P2D3 -"

.segment "VECTORS"

; set interrupt vectors to point to handlers
.word nmi   ;$fffa NMI
.word start ;$fffc Reset
.word irq   ;$fffe IRQ


.segment "CHARS"

; include CHR ROM data
.incbin "sprites.chr"

