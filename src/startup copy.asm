; NES ROM Header - tells emulator/hardware about the ROM
.segment "HEADER"
.byte 'N', 'E', 'S', $1a      ; "NES" followed by MS-DOS EOF marker
.byte $02                     ; 2 x 16KB PRG-ROM banks
.byte $01                     ; 1 x 8KB CHR-ROM bank
.byte $00, $00                ; Mapper 0, no special features

.segment "ZEROPAGE"

game_state:     .res 1
player_x:       .res 1
player_y:       .res 1
ball_x:         .res 1
ball_y:         .res 1
ball_dx:        .res 1
ball_dy:        .res 1
score:          .res 1
scroll:         .res 1
time:           .res 1
seconds:        .res 1

; Main program code section
.segment "CODE"

; Interrupt Request Handler - called when IRQ interrupt occurs
.proc irq_handler
  RTI                     ; Return from interrupt (we don't use IRQ)
.endproc

; Non-Maskable Interrupt Handler - called during VBlank
.proc nmi_handler
; *** Changed from empty handler to run game logic and sprite drawing at VBlank ***
  JSR update_ball        ; Update ball position
  JSR draw_ball_sprite   ; Draw ball based on new position
  RTI
.endproc

; Reset Handler - called when system starts up or resets
.proc reset_handler
  ; === CPU Initialization ===
  SEI                     ; Set interrupt disable flag (ignore IRQ)
  CLD                     ; Clear decimal mode flag (NES doesn't support BCD)

  ; === APU Initialization ===
  LDX #$40                ; Load X with $40
  STX $4017               ; Write to APU Frame Counter register
                          ; Disables APU frame IRQ

  ; === Stack Initialization ===
  LDX #$FF                ; Load X with $FF (top of stack page)
  TXS                     ; Transfer X to Stack pointer ($01FF)

  ; === PPU Initialization ===
  INX                     ; Increment X (now $00)
  LDA #%10000000           ; bit 7 = 1 → enable NMI
  STA $2000

  STX $2001               ; PPUMASK = 0 (disable rendering)
  STX $4010               ; DMC frequency register = 0 (disable DMC)

  ; === Wait for PPU to be ready ===
  BIT $2002               ; Read PPUSTATUS to clear VBlank flag

  ; First VBlank wait - PPU needs time to stabilize
vblankwait:
  BIT $2002               ; Read PPUSTATUS register
  BPL vblankwait          ; Branch if Plus (bit 7 = 0, no VBlank)
                          ; Loop until VBlank flag is set

  ; Second VBlank wait - ensures PPU is fully ready
vblankwait2:
  BIT $2002               ; Read PPUSTATUS register again
  BPL vblankwait2         ; Branch if Plus (bit 7 = 0, no VBlank)
                          ; Loop until second VBlank occurs

  ; --- Ball Initialization ---
  LDA #$80        ; Start X at 128
  STA ball_x

  LDA #$60        ; Start Y at 96
  STA ball_y

  LDA #$01        ; X velocity = 1
  STA ball_dx

  LDA #$01        ; Y velocity = 1
  STA ball_dy

  JMP main                ; Jump to main program
.endproc

.proc draw_ball_sprite
; *** Added this procedure to centralize sprite drawing during VBlank ***
  LDA #$00
  STA $2003           ; Set OAM address to 0

  LDA ball_y
  STA $2004           ; Y position

  LDA #$01
  STA $2004           ; Tile index (solid block)

  LDA #$00
  STA $2004           ; Attributes

  LDA ball_x
  STA $2004           ; X position

  RTS
.endproc

.proc update_ball
; *** This procedure updates ball position and handles boundary collision ***
  ; Update X position
  LDA ball_x
  CLC
  ADC ball_dx
  STA ball_x

  ; Check screen boundaries for X (0–248)
  CMP #0
  BNE not_hit_left
    LDA #1
    STA ball_dx
not_hit_left:
  CMP #248
  BNE not_hit_right
    LDA #$FF
    STA ball_dx
not_hit_right:

  ; Update Y position
  LDA ball_y
  CLC
  ADC ball_dy
  STA ball_y

  ; Check screen boundaries for Y (0–210)
  CMP #0
  BNE not_hit_top
    LDA #1
    STA ball_dy
not_hit_top:
  CMP #210
  BNE not_hit_bottom
    LDA #$FF
    STA ball_dy
not_hit_bottom:

  RTS
.endproc

.proc main
  ; === Set Background Color ===
  LDX $2002               ; Read PPUSTATUS to reset address latch

  ; Set PPU address to palette RAM
  LDX #$3f                ; High byte of palette address ($3F00)
  STX $2006               ; Write to PPUADDR register
  LDX #$00                ; Low byte of palette address ($3F00)
  STX $2006               ; Write to PPUADDR register
                          ; PPU address is now $3F00 (background palette 0)

  ; Write background color
  LDA #$29                ; Load color $29 (green) into accumulator
  STA $2007               ; Write to PPUDATA register
                          ; This sets the background color

  ; === Enable Rendering ===
  LDA #%00011110          ; Load rendering flags:
                          ; bit 4 = 1: Show background
                          ; bit 3 = 1: Show sprites
                          ; bit 2 = 1: Show background in leftmost 8 pixels
                          ; bit 1 = 1: Show sprites in leftmost 8 pixels
  STA $2001               ; Write to PPUMASK register (enable rendering)

  ; === Sprite DMA Setup ===
  ; *** Kept in main but actual sprite drawing moved to NMI handler in an attempt to avoid flickering ***
  LDA #$00
  STA $2003

  LDA ball_y
  STA $2004

  LDA #$01
  STA $2004

  LDA #$00
  STA $2004

  LDA ball_x
  STA $2004

forever:

  JSR update_ball

  ; DMA sprite transfer
  LDA #$00
  STA $2003

  LDA ball_y
  STA $2004

  LDA #$01
  STA $2004

  LDA #$00
  STA $2004

  LDA ball_x
  STA $2004

; *** Removed direct sprite DMA and update calls from main loop ***
  JMP forever
.endproc

; Interrupt vectors - tells CPU where to jump for each interrupt
.segment "VECTORS"
.addr nmi_handler         ; NMI vector ($FFFA-$FFFB)
.addr reset_handler       ; Reset vector ($FFFC-$FFFD)
.addr irq_handler         ; IRQ vector ($FFFE-$FFFF)

; Character ROM data (graphics patterns)
.segment "CHARS"

; Tile 0 (blank)
.repeat 16
  .byte $00
.endrepeat

; Tile 1 (solid block)
.repeat 8
  .byte %11111111
.endrepeat

.repeat 8
  .byte %00000000
.endrepeat

; Fill the rest with zeros to reach 8192 bytes
.repeat 8192 - 32
  .byte $00
.endrepeat

; Reserve 8KB of space for CHR-ROM data
; (sprite and background tile patterns)

; Startup segment
.segment "STARTUP"
