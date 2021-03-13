; compile with -
; 	asar main.asm

hirom
org    $808000
base $7E0000
optimize dp always
optimize address mirrors
incsrc mmio.asm

base $7E0000
	RPF:	skip 2
	P1D0err:	skip 2
	P1D1err:	skip 2
	P2D0err:	skip 2
	P2D1err:	skip 2
	
	
	p1d0next:	skip 1
	p1d1next:	skip 1
	p2d0next:	skip 1
	p2d1next:	skip 1
	
	p1d0:	skip 2
	p1d1:	skip 1
	p2d0:	skip 1
	p2d1:	skip 1
	temp:	skip 1
	
base $7E0F80			;go here if not already here
	stack_end: skip $7F
	stack:
	print pc
base $7F0000
	output_buffer:
base off

org    $808000

macro DMA_to_CGRAM(srcbank, srcaddr, destaddr, datasize)
	rep #$20		; 16-bit A
	sep #$10		; 8-bit XY
	ldy <destaddr>		;
	sty PPU.cgram_address	;
	lda #$2200		;
	sta DMA[2].settings	; 1 reg
	lda <srcaddr>		;
	sta DMA[2].source_word	; set the lower two bytes of the destination address
	ldy.b <srcbank>		;
	sty DMA[2].source_bank	;
	lda <datasize>		; number of bytes to transfer
	sta DMA[2].size		;
	ldy #$04		; DMA channel 2
	sty CPU.enable_dma	;
	rep #$30		;
endmacro


reset:
	sei 	  
	clc 	  
	xce 	  
	stz CPU.enable_interrupts
	jml +
	+
 	
 	;setup stack/direct page/clear decimal
 	rep #$38 
 	lda #stack
 	tcs 	 
 	lda #$0000 
 	tcd
 	
 	pea $8080
 	plb
 	plb
 	
 	jsr register_init
	jsr clear_RAM
	jsr setup_font
	
	;leave interrupts off by default
	;only update the screen when a new byte has been written
	sep #$30
	stz CPU.enable_interrupts
	
	jsr execute
	- bra -
	
register_init:  
 	sep #$10
 	rep #$20
 	ldx #$80 		;\ enable force blank
 	stx PPU.screen		;/
 	stz PPU.sprite_select	; use 16x16 and 8x8 sprites, graphics at vram $0000
 	stz PPU.oam_address	; reset oam address
 	
 	ldx #$01	
 	stx PPU.layer_mode			; Mode 1
 	stz PPU.mosaic				; disable mosaic
 	stz PPU.layer_0_1_tilemap_select	; clear 0/1 map VRAM location
 	stz PPU.layer_2_3_tilemap_select	; clear 2/3 map VRAM location
 	stz PPU.layer_all_tiledata_select	; clear 0/1/2/3 Tile data location
 	
 	dex
 	stx PPU.layer_0_scroll_x	;\ layer 0 X
 	stx PPU.layer_0_scroll_x	;/
 	stx PPU.layer_1_scroll_x	;\ layer 1 X
 	stx PPU.layer_1_scroll_x	;/
 	stx PPU.layer_2_scroll_x	;\ layer 2 X
 	stx PPU.layer_2_scroll_x	;/
 	stx PPU.layer_3_scroll_x	;\ layer 3 X
 	stx PPU.layer_3_scroll_x	;/
 	
 	ldy #$07			;\ 1 pixel down so you can see the top
 	dex				;/
 	stx PPU.layer_0_scroll_y	;\ layer 0 Y
 	stx PPU.layer_0_scroll_y	;/
 	stx PPU.layer_1_scroll_y	;\ layer 1 Y
 	stx PPU.layer_1_scroll_y	;/
 	stx PPU.layer_2_scroll_y	;\ layer 2 Y
 	stx PPU.layer_2_scroll_y	;/
 	stx PPU.layer_3_scroll_y	;\ layer 3 Y
 	stx PPU.layer_3_scroll_y	;/

 	stz PPU.window_layer_all_settings	; clear window masks
 	stz PPU.window_sprite_color_settings	; clear color masks
 	stz PPU.window_1			; Clear window 1 left/right positions
 	stz PPU.window_2			; Clear window 2 left/right positions
 	stz PPU.window_logic			; Clear windowing logic
 	
 	lda #$0013 		;\ enable sprites, layer 0, layer 1 on main screen, clear subscreen
 	sta PPU.screens		;/
 	
 	stz PPU.window_masks	; Window mask for Main Screen
 	lda #$0030
 	sta PPU.color_math	; Disable color math
 	
 	lda #$00E0	
 	sta PPU.display_control	; reset color intensity and clear screen mode
 	rts

clear_RAM:
	jsr clear_wram
	jsr clear_sram
	jsr clear_vram
	jsr clear_cgram
	jsr clear_oam
	rts

clear_wram:
	stz WRAM.word
	stz WRAM.high
	
	lda #$8008
	sta DMA[0].settings
	lda #.fill_byte
	sta DMA[0].source_word
	ldx #.fill_byte>>16
	stx DMA[0].source_bank
	lda #stack-4	;Clear up to the last 4 bytes of stack
	sta DMA[0].size
	ldx #$01
	stx CPU.enable_dma

	lda #$2000
	sta WRAM.word
	stz WRAM.high
		
	lda #$E000
	sta DMA[0].size
	stx CPU.enable_dma
	
	stz DMA[0].size
	stx CPU.enable_dma
	
	rts
	
.fill_byte
	db $00
	
clear_sram:
	rep #$30
	;phb
	;lda #$0000
	;sta output_buffer
	;ldx.w #output_buffer
	;ldy.w #output_buffer+1
	;lda.w #$FFFF
	;mvn output_buffer>>16,output_buffer>>16
	;plb
	sep #$10
	rts

clear_vram:
	ldx #$80
	stx PPU.vram_control
	lda #$1809
	sta DMA[0].settings
	stz PPU.vram_address
	stz DMA[0].source_word
	stz DMA[0].source_bank
	stz DMA[0].size  
	
	ldx #$01
	stx CPU.enable_dma
	rts

clear_cgram:
	stz PPU.cgram_address
	ldy #$00
	ldx #$00
	-
		sty PPU.cgram_write
		sty PPU.cgram_write
		dex
	bne -
	rts
	
clear_oam:
	stz PPU.oam_address
	ldx #$80
	ldy #$F0
	-
		stz PPU.oam_write
		sty PPU.oam_write
		stz PPU.oam_write
		stz PPU.oam_write
		dex
	bne -
	
	ldx #$20
	-
		stz PPU.oam_write
		dex
	bne -
	rts
	
setup_font:
	sep #$10
	lda #$0002
	sta PPU.screens

	stz PPU.sprite_select   ; 8x8/16x16 sprites using 0000 for base tile data
	
	lda #$0001  
	sta PPU.layer_all_tiledata_select   ;layer 1 tile data at 1000, layer 2 at 0000
	
	ldx #$01
	stx PPU.layer_mode   ;background mode 1
	
	ldx #$40
	stx PPU.layer_1_tilemap_select   ;layer 2 tilemap at 4000 no mirroring
	
	;reset layer positions
	
	ldx #$00
	stx PPU.layer_1_scroll_x
	stx PPU.layer_1_scroll_x

	ldx #$FE
	stx PPU.layer_1_scroll_y
	ldx #$80
	stx PPU.layer_1_scroll_y
	
	stx PPU.vram_control	
	stz PPU.vram_address
	
	lda #$1841
	sta DMA[6].settings
	;upload font tiles
	;transfer source
	
	lda #font_tiles
	sta DMA[6].source_word
	ldx.b #font_tiles>>16
	stx DMA[6].source_bank
	
	;transfer size
	lda #font_tiles-font_tilemap
	sta DMA[6].size
	
	ldx #$40
	stx CPU.enable_dma
		
	%DMA_to_CGRAM(#font_palette>>16, #font_palette, #$00, #$00A0)
	
	sep #$30
	stz PPU.cgram_address
	;lda #$73
	stz PPU.cgram_write
	;lda #$4E
	stz PPU.cgram_write
	;lda #$E0
 	stz PPU.fixed_color
 	
 	lda #$0F
 	sta PPU.screen
 	
	rep #$30
	rts
	
update_screen:
	jml +
	+
	rep #$30
	pha
	phx
	phy
	
	lda CPU.nmi_flag
	;dma tilemap here
	;lda output_index
	sep #$30
	
	ldy #$00
	ldx #$FF
	--
		inx
		;-
		;	inx
		;	lda text,x
		;	sta output_buffer,x
		;bne -
		lda RPF,y
		lsr     a
		lsr     a
		lsr     a
		lsr     a
		clc
		adc #'0'
		sta output_buffer,x
		inx
		lda RPF,y
		and #$0f
		clc
		adc #'0'
		sta output_buffer,x
		
		iny
		cpy #$0A
	bne --
	
	stz RPF
	stz RPF+1
	
	rep #$30
	sep #$10
	ldx #$00
	stx PPU.vram_control
	lda #$4164
	sta PPU.vram_address
	
	lda #$1800
	sta DMA[6].settings
	;upload font tiles
	;transfer source

	lda #output_buffer
	sta DMA[6].source_word
	ldx.b #output_buffer>>16
	stx DMA[6].source_bank
	
	;transfer size
	lda.w #32*28
	sta DMA[6].size
	
	ldx #$40
	stx CPU.enable_dma
	
	lda #$4164
	sta PPU.vram_address
	
	ldx #$80
	stx PPU.vram_control
	
	lda #$1908
	sta DMA[6].settings
	
	lda #palette_byte
	sta DMA[6].source_word
	ldx.b #palette_byte>>16
	stx DMA[6].source_bank
	
	lda.w #32*28
	sta DMA[6].size
	
	ldx #$40
	stx CPU.enable_dma
	
	
	sep #$20
	;stz CPU.enable_interrupts
	rep #$30
	
	ply
	plx
	pla
	rti
	
palette_byte:
	db $0C

execute:
	lda #$80
	sta CPU.enable_interrupts
	jsr mainloop
	rts
;lazy text method
text:
db "RPF  -  ", $00
db "                       YEAP -  ", $00
db "                       YEAP -  ", $00
db "                       YEAP -  ", $00
db "                       YEAP -  ", $00

mainloop:
    jsr readcntrl

    


    jmp mainloop


readcntrl:
	rep #$20		; 16-bit A
	lda #$0000
    ; strobe controllers
    ldx #$01
    stx joypad.port_0
    dex
    stx joypad.port_0

	lda joypad.port_0
    ldy #03         ; loop over all 3 bit positions
-
	asl a
	asl a
	eor joypad.port_0
    dey
    bne -
	
	sta P1D0err
	
	lda joypad.port_0
	ldy #03         ; loop over all 3 bit positions
-
	asl a
	asl a
	eor joypad.port_0
    dey
    bne -
	
	sta P1D1err

	lda joypad.port_0
	ldy #03         ; loop over all 3 bit positions
-
	asl a
	asl a
	eor joypad.port_0
    dey
    bne -
	
	sta P2D0err
	
	lda joypad.port_0
    ldy #03         ; loop over all 3 bit positions
-
	asl a
	asl a
	eor joypad.port_0
    dey
    bne -
	
	sta P2D1err
	inc RPF
	sep #$20

rts


youscrewedup:
- bra -

print pc
warnpc $80FFAF

org $00FFB0
        db "FF"				;maker code.
        db "FFFF"			;game code.
        db $00,$00,$00,$00,$00,$00,$00	;fixed value, must be 0
        db $00				;expansion RAM size. SRAM size. 128kB
        db $00				;special version, normally 0
        db $00				;cartridge sub number, normally 0s

        db "TRDAS                "	;ROM NAME
        db $31				;MAP MODE. Mode 30 = fastrom
        db $02				;cartridge type. ROM AND RAM AND SRAM
        db $09				;3-4 MBit ROM        
        db $07				;128K RAM        
        db $00				;Destination code: Japan
        db $33				;Fixed Value    
        db $00				;Mask ROM. This ROM is NOT revised.
        dw $0000			;Complement Check.
        dw $0000			;Checksum

        ;emulation mode
        dw $FFFF			;Unused
        dw $FFFF			;Unused
        dw youscrewedup			;COP
        dw youscrewedup			;BRK
        dw youscrewedup			;ABORT
        dw update_screen		;NMI
        dw $FFFF			;Unused
        dw $FFFF			;IRQ

        ;native mode
        dw $FFFF			;Unused
        dw $FFFF			;Unused
        dw youscrewedup			;COP
        dw youscrewedup			;BRK
        dw youscrewedup			;ABORT
        dw $FFFF			;NMI
        dw reset			;RESET
        dw $FFFF			;IRQ

org $C50000
	font_tilemap:
		incbin font_tilemap.bin
	font_tiles:
		incbin font_tiles.bin
	font_palette:
		rep 16 : incbin font_palette.bin

org $CFFFFF
db $00
