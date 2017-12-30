;compile to this filename
!to "jmain.prg",cbm

;define constants here

;placeholder for various temp parameters
PARAM1                  = $03
PARAM2                  = $04
PARAM3                  = $05
PARAM4                  = $06
PARAM5                  = $07

;placeholder for zero page pointers
ZEROPAGE_POINTER_1      = $17
ZEROPAGE_POINTER_2      = $19
ZEROPAGE_POINTER_3      = $21
ZEROPAGE_POINTER_4      = $23

VIC_SCREENCTRL2         = $d016
VIC_BORDER_COLOR        = $d020
VIC_BACKGROUND_COLOR    = $d021
VIC_MEMORY_CONTROL      = $d018

CIA_PRA                 = $dd00

;address of the screen buffer
SCREEN_CHAR             = $CC00

;address of color ram
SCREEN_COLOR            = $D800

;address of the screen backup memory
SCREEN_BACK_CHAR        = $2000

;address of the screen backbuffer
SCREEN_BACK_COLOR       = $C800

;this creates a basic start
*=$0801

		;SYS 2064
		!byte $0C,$08,$0A,$00,$9E,$20,$32,$30,$36,$34,$00,$00,$00,$00,$00

		;block interrupts 
		;since we turn ROMs off this would result in crashes if we didn't
		sei
          
		;save old configuration
		lda $01
		sta PARAM1
          
		;to copy under the IO rom and expose character generator rom
		;lda #%00110000
		lda #%00110011
		sta $01
          
		;take source address from CHARSET
		lda #$00
		sta ZEROPAGE_POINTER_1
		lda #$D0
		sta ZEROPAGE_POINTER_1 + 1
          
		;now copy
		jsr CopyCharSet

		;restore ROMs
		lda PARAM1
		sta $01
          
		cli
		
		;set charset to 12K from start of VIC (12*1024=$3000)
		;and screen memory to %0011*1024=3*1024=$CC00
		lda #$3c
		sta VIC_MEMORY_CONTROL

		;VIC bank set to bank 3 meaning $C000 to $FFFF
		;character set to be found at $C000+$3000=$F000
		lda CIA_PRA
		and #$fc
		sta CIA_PRA
		
		; set scroll delay to 0
		ldx  #$00
		stx  SCROLL_DELAY
		
		; clear the screen with spaces
spaceloop		
		lda	#$20
		sta	SCREEN_CHAR,x
		sta	SCREEN_CHAR+256,x
		sta	SCREEN_CHAR+512,x
		sta	SCREEN_CHAR+768,x
		inx
		bne	spaceloop
		
		;write message to the screen
		lda	#<TICKER
		sta	ZEROPAGE_POINTER_1
		lda	#>TICKER
		sta	ZEROPAGE_POINTER_1 + 1
		
		lda #$01
		sta PARAM1
		sta PARAM2
		jsr DisplayText

		; empty the character shapes in the ticker
		ldy #$00
nextchar	lda #<TICKER
		sta ZEROPAGE_POINTER_2
		lda #>TICKER
		sta ZEROPAGE_POINTER_2+1
		jsr CalculateCharPos
		sty PARAM1
		
		ldy #$00
		lda #0
wipeloop	sta (ZEROPAGE_POINTER_1),y
		iny
		cpy #$08
		bne wipeloop
		
		ldy PARAM1
		iny
		cpy #$0b
		bne nextchar
		
hold		nop
		jmp hold
				
		

		
;------------------------------------------------------------
;
;    GameLoop
;
;------------------------------------------------------------
!zone GameLoop
GameLoop  
		jsr waitFrame

		;lda #1
		;sta VIC_BORDER_COLOR

		jsr runTicker

		;lda #0
		;sta VIC_BORDER_COLOR

		jmp  GameLoop         

;---------------------------------------
;
;    waitFrame
;
;---------------------------------------
!zone waitFrame
		;wait for the raster to reach line $f8
		;this is keeping our timing stable
		  
		;are we on line $F8 already? if so, wait for the next full screen
		;prevents mistimings if called too fast
waitFrame 
		lda $d012
		cmp #$F8
		beq waitFrame

		;wait for the raster to reach line $f8 (should be closer to the start of this line this way)
.WaitStep2
		lda $d012
		cmp #$F8
		bne .WaitStep2
		  
		rts

;------------------------------------------------------------
; runTicker
;------------------------------------------------------------
		; loop through the message
		; copy the shape of the character at the cursor to the input buffer character
		;  for 8 lines do
		;   rol line one pixel
		;  8 pixels scrolled?
		;  no: keep scrolling
		;  yes: advance the message pointer
		;  message pointer at end? yes: set to 0
		; 
!zone runTicker
runTicker	
		
;------------------------------------------------------------
; CalculateCharPos
; parameters:
; 	ZEROPAGE_POINTER_2=points to string of characters
; 	Y=index into ZEROPAGE_POINTER_2
; returns:
; 	ZEROPAGE_POINTER_1 address of character shape in memory
; changes:
; 	ZEROPAGE_POINTER_3
;------------------------------------------------------------
!zone CalculateCharPos
CalculateCharPos		
		; calculate memory position of a character
		; get the character we are targeting
		lda (ZEROPAGE_POINTER_2),y
		
		; multiply its value by 8
		sta ZEROPAGE_POINTER_3	; store it in result lsb
		lda #$0				; set result msb to 0
		sta ZEROPAGE_POINTER_3+1
		asl ZEROPAGE_POINTER_3	; multiply by 2
		rol ZEROPAGE_POINTER_3+1	; transfer result to msb
		asl ZEROPAGE_POINTER_3	; repeat three times
		rol ZEROPAGE_POINTER_3+1
		asl ZEROPAGE_POINTER_3
		rol ZEROPAGE_POINTER_3+1
		; now add the result to $F000
		clc					; clear carry
		lda ZEROPAGE_POINTER_3	; load first number lsb
		adc #$00				; add to lsb of $f000
		sta ZEROPAGE_POINTER_1	; store sum of LSBs
		lda ZEROPAGE_POINTER_3+1	; load first number msb	
		adc #$F0				; add the MSBs using carry from
		sta ZEROPAGE_POINTER_1+1	; the previous calculation
		rts		
		
;------------------------------------------------------------
;copies charset from ZEROPAGE_POINTER_1 to ZEROPAGE_POINTER_2
;------------------------------------------------------------

!zone CopyCharSet
CopyCharSet
          ;set target address ($F000)
          lda #$00
          sta ZEROPAGE_POINTER_2
          lda #$F0
          sta ZEROPAGE_POINTER_2 + 1

          ldx #$00
          ldy #$00
          lda #0
          sta PARAM2

.NextLine
          lda (ZEROPAGE_POINTER_1),Y
          sta (ZEROPAGE_POINTER_2),Y
          inx
          iny
          cpx #$08
          bne .NextLine
          cpy #$00
          bne .PageBoundaryNotReached
          
          ;we've reached the next 256 bytes, inc high byte
          inc ZEROPAGE_POINTER_1 + 1
          inc ZEROPAGE_POINTER_2 + 1

.PageBoundaryNotReached

          ;only copy 254 chars to keep irq vectors intact
          inc PARAM2
          lda PARAM2
          cmp #254
          beq .CopyCharsetDone
          ldx #$00
          jmp .NextLine

.CopyCharsetDone
          rts	
		
;------------------------------------------------------------
;displays a line of text
;ZEROPAGE_POINTER_1 = pointer to text
;PARAM1 = X
;PARAM2 = Y;
;modifies ZEROPAGE_POINTER_2 and ZEROPAGE_POINTER_3
;------------------------------------------------------------
!zone DisplayText
DisplayText
          ldx PARAM2
          lda SCREEN_LINE_OFFSET_TABLE_LO,x
          sta ZEROPAGE_POINTER_2
          sta ZEROPAGE_POINTER_3
          lda SCREEN_LINE_OFFSET_TABLE_HI,x
          sta ZEROPAGE_POINTER_2 + 1
          clc
          adc #( ( SCREEN_COLOR - SCREEN_CHAR ) & 0xff00 ) >> 8
          sta ZEROPAGE_POINTER_3 + 1

          lda ZEROPAGE_POINTER_2
          clc
          adc PARAM1
          sta ZEROPAGE_POINTER_2
          lda ZEROPAGE_POINTER_2 + 1
          adc #0
          sta ZEROPAGE_POINTER_2 + 1
          lda ZEROPAGE_POINTER_3
          clc
          adc PARAM1
          sta ZEROPAGE_POINTER_3
          lda ZEROPAGE_POINTER_3 + 1
          adc #0
          sta ZEROPAGE_POINTER_3 + 1

          ldy #0
.InLineLoop
          lda (ZEROPAGE_POINTER_1),y
          cmp #$2A
          beq .EndMarkerReached
          cmp #45
          beq .LineBreak
          sta (ZEROPAGE_POINTER_2),y
          lda #1
          sta (ZEROPAGE_POINTER_3),y
          iny
          jmp .InLineLoop
        
.LineBreak
          iny
          tya
          clc
          adc ZEROPAGE_POINTER_1
          sta ZEROPAGE_POINTER_1
          lda #0
          adc ZEROPAGE_POINTER_1 + 1
          sta ZEROPAGE_POINTER_1 + 1

          inc PARAM2
          jmp DisplayText
            
.EndMarkerReached
          rts
		
;---------------------------------------
;
;	Game data goes here
;
;---------------------------------------

; the delay counter for scrolling
SCROLL_DELAY	!byte	0
		 
MESSAGE	!text "HELLO THIS IS MY MESSAGE*", $00
TICKER	!byte 64,65,66,67,68,69,70,71,72,73,74,$2A
;TICKER !byte 0,1,2,3,4,5,6,7,8,9,10,11,$2a

; tables of address of first character on each line of base and backup screens (low and high parts)
SCREEN_LINE_OFFSET_TABLE_LO
		!byte ( SCREEN_CHAR +   0 ) & 0x00ff
		!byte ( SCREEN_CHAR +  40 ) & 0x00ff
		!byte ( SCREEN_CHAR +  80 ) & 0x00ff
		!byte ( SCREEN_CHAR + 120 ) & 0x00ff
		!byte ( SCREEN_CHAR + 160 ) & 0x00ff
		!byte ( SCREEN_CHAR + 200 ) & 0x00ff
		!byte ( SCREEN_CHAR + 240 ) & 0x00ff
		!byte ( SCREEN_CHAR + 280 ) & 0x00ff
		!byte ( SCREEN_CHAR + 320 ) & 0x00ff
		!byte ( SCREEN_CHAR + 360 ) & 0x00ff
		!byte ( SCREEN_CHAR + 400 ) & 0x00ff
		!byte ( SCREEN_CHAR + 440 ) & 0x00ff
		!byte ( SCREEN_CHAR + 480 ) & 0x00ff
		!byte ( SCREEN_CHAR + 520 ) & 0x00ff
		!byte ( SCREEN_CHAR + 560 ) & 0x00ff
		!byte ( SCREEN_CHAR + 600 ) & 0x00ff
		!byte ( SCREEN_CHAR + 640 ) & 0x00ff
		!byte ( SCREEN_CHAR + 680 ) & 0x00ff
		!byte ( SCREEN_CHAR + 720 ) & 0x00ff
		!byte ( SCREEN_CHAR + 760 ) & 0x00ff
		!byte ( SCREEN_CHAR + 800 ) & 0x00ff
		!byte ( SCREEN_CHAR + 840 ) & 0x00ff
		!byte ( SCREEN_CHAR + 880 ) & 0x00ff
		!byte ( SCREEN_CHAR + 920 ) & 0x00ff
		!byte ( SCREEN_CHAR + 960 ) & 0x00ff
		  
SCREEN_LINE_OFFSET_TABLE_HI
		!byte ( ( SCREEN_CHAR +   0 ) & 0xff00 ) >> 8
		!byte ( ( SCREEN_CHAR +  40 ) & 0xff00 ) >> 8
		!byte ( ( SCREEN_CHAR +  80 ) & 0xff00 ) >> 8
		!byte ( ( SCREEN_CHAR + 120 ) & 0xff00 ) >> 8
		!byte ( ( SCREEN_CHAR + 160 ) & 0xff00 ) >> 8
		!byte ( ( SCREEN_CHAR + 200 ) & 0xff00 ) >> 8
		!byte ( ( SCREEN_CHAR + 240 ) & 0xff00 ) >> 8
		!byte ( ( SCREEN_CHAR + 280 ) & 0xff00 ) >> 8
		!byte ( ( SCREEN_CHAR + 320 ) & 0xff00 ) >> 8
		!byte ( ( SCREEN_CHAR + 360 ) & 0xff00 ) >> 8
		!byte ( ( SCREEN_CHAR + 400 ) & 0xff00 ) >> 8
		!byte ( ( SCREEN_CHAR + 440 ) & 0xff00 ) >> 8
		!byte ( ( SCREEN_CHAR + 480 ) & 0xff00 ) >> 8
		!byte ( ( SCREEN_CHAR + 520 ) & 0xff00 ) >> 8
		!byte ( ( SCREEN_CHAR + 560 ) & 0xff00 ) >> 8
		!byte ( ( SCREEN_CHAR + 600 ) & 0xff00 ) >> 8
		!byte ( ( SCREEN_CHAR + 640 ) & 0xff00 ) >> 8
		!byte ( ( SCREEN_CHAR + 680 ) & 0xff00 ) >> 8
		!byte ( ( SCREEN_CHAR + 720 ) & 0xff00 ) >> 8
		!byte ( ( SCREEN_CHAR + 760 ) & 0xff00 ) >> 8
		!byte ( ( SCREEN_CHAR + 800 ) & 0xff00 ) >> 8
		!byte ( ( SCREEN_CHAR + 840 ) & 0xff00 ) >> 8
		!byte ( ( SCREEN_CHAR + 880 ) & 0xff00 ) >> 8
		!byte ( ( SCREEN_CHAR + 920 ) & 0xff00 ) >> 8
		!byte ( ( SCREEN_CHAR + 960 ) & 0xff00 ) >> 8
 



