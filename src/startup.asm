; === NES HEADER ===
.segment "HEADER"
.byte 'N','E','S',$1A
.byte $02       ; 2 x 16KB PRG-ROM
.byte $01       ; 1 x 8KB CHR-ROM
.byte $00, $00  ; Mapper 0, no mirroring

; === ZEROPAGE VARIABLES ===
.segment "ZEROPAGE"
player_x:     .res 1
player_y:     .res 1
controller:   .res 1
nmi_flag:     .res 1
score_low:    .res 1
score_high:   .res 1
frame_counter: .res 1
rain_spawn_timer: .res 1

; Rain drops (4 drops max)
rain1_x:      .res 1
rain1_y:      .res 1
rain1_active: .res 1
rain2_x:      .res 1
rain2_y:      .res 1
rain2_active: .res 1
rain3_x:      .res 1
rain3_y:      .res 1
rain3_active: .res 1
rain4_x:      .res 1
rain4_y:      .res 1
rain4_active: .res 1

; === STARTUP SEGMENT ===
.segment "STARTUP"

.proc reset_handler
  SEI
  CLD
  LDX #$FF
  TXS
  INX

  ; Clear RAM
  LDA #$00
clear_ram:
  STA $0000, X
  STA $0100, X
  STA $0200, X
  STA $0300, X
  STA $0400, X
  STA $0500, X
  STA $0600, X
  STA $0700, X
  INX
  BNE clear_ram

  ; Initialize PPU
  LDA #%10000000
  STA $2000
  LDA #$00
  STA $2001
  STA $4010

  ; Wait for PPU to stabilize
  BIT $2002
vblank_wait1:
  BIT $2002
  BPL vblank_wait1

vblank_wait2:
  BIT $2002
  BPL vblank_wait2

  ; Load palette
  LDA #$3F
  STA $2006
  LDA #$00
  STA $2006

  ; Background palette
  LDA #$0F  ; Black
  STA $2007
  LDA #$30  ; White
  STA $2007
  LDA #$00
  STA $2007
  LDA #$00
  STA $2007

  ; Sprite palette
  LDA #$0F  ; Black
  STA $2007
  LDA #$30  ; White
  STA $2007
  LDA #$16  ; Red
  STA $2007
  LDA #$27  ; Orange
  STA $2007

  ; Initialize variables
  LDA #$00
  STA nmi_flag
  STA score_low
  STA score_high
  STA frame_counter
  STA rain_spawn_timer

  ; Initialize player position
  LDA #120
  STA player_x
  LDA #200
  STA player_y

  ; Initialize rain drops as inactive
  LDA #$00
  STA rain1_active
  STA rain2_active
  STA rain3_active
  STA rain4_active

  ; Setup nametable for score display
  JSR setup_score_display

  ; Enable NMI and sprites
  LDA #%10000000
  STA $2000
  LDA #%00011000  ; Enable sprites and background
  STA $2001

  JMP main
.endproc

; === SETUP SCORE DISPLAY ===
.proc setup_score_display
  ; Set PPU address to top right area for score
  LDA #$20
  STA $2006
  LDA #$D6  ; Position for "SCORE:"
  STA $2006

  ; Write "SCORE:" text (using basic tiles)
  LDA #$01  ; Use solid block for letters (simplified)
  STA $2007
  STA $2007
  STA $2007
  STA $2007
  STA $2007
  LDA #$00  ; Space
  STA $2007

  RTS
.endproc

; === NMI HANDLER (Render Here) ===
.proc nmi_handler
  ; Preserve registers
  PHA
  TXA
  PHA
  TYA
  PHA

  ; Set NMI flag
  LDA #$01
  STA nmi_flag

  ; Set OAM address
  LDA #$00
  STA $2003

  ; --- Draw Player ---
  LDA player_y
  STA $2004
  LDA #$01
  STA $2004
  LDA #$00
  STA $2004
  LDA player_x
  STA $2004

  ; --- Draw Rain Drops ---
  LDX #$00

  ; Rain drop 1
  LDA rain1_active
  BEQ skip_rain1
  LDA rain1_y
  STA $2004
  LDA #$01
  STA $2004
  LDA #$01  ; Different palette
  STA $2004
  LDA rain1_x
  STA $2004
  INX
skip_rain1:

  ; Rain drop 2
  LDA rain2_active
  BEQ skip_rain2
  LDA rain2_y
  STA $2004
  LDA #$01
  STA $2004
  LDA #$01
  STA $2004
  LDA rain2_x
  STA $2004
  INX
skip_rain2:

  ; Rain drop 3
  LDA rain3_active
  BEQ skip_rain3
  LDA rain3_y
  STA $2004
  LDA #$01
  STA $2004
  LDA #$01
  STA $2004
  LDA rain3_x
  STA $2004
  INX
skip_rain3:

  ; Rain drop 4
  LDA rain4_active
  BEQ skip_rain4
  LDA rain4_y
  STA $2004
  LDA #$01
  STA $2004
  LDA #$01
  STA $2004
  LDA rain4_x
  STA $2004
  INX
skip_rain4:

  ; Clear remaining sprites
clear_remaining:
  CPX #63  ; 64 sprites total - 1 (player)
  BCS sprites_done
  LDA #$F0  ; Off-screen Y
  STA $2004
  STA $2004
  STA $2004
  STA $2004
  INX
  JMP clear_remaining

sprites_done:
  ; Update score display
  JSR update_score_display

  ; Restore registers
  PLA
  TAY
  PLA
  TAX
  PLA

  RTI
.endproc

; === UPDATE SCORE DISPLAY ===
.proc update_score_display
  ; Set PPU address to score number area
  LDA #$20
  STA $2006
  LDA #$DD  ; Position after "SCORE:"
  STA $2006

  ; Display score (simple decimal conversion)
  LDA score_low
  ; Convert to two digits
  LDX #$00
tens_loop:
  CMP #10
  BCC show_tens
  SBC #10
  INX
  JMP tens_loop

show_tens:
  ; Show tens digit (tiles 2-9 are numbers 0-7)
  TXA
  CLC
  ADC #$02  ; Convert to tile number (0-7 become tiles $02-$09)
  STA $2007

  ; Show ones digit
  PHA
  CLC
  ADC #$02
  STA $2007
  PLA

  RTS
.endproc

; === IRQ HANDLER (Unused) ===
.proc irq_handler
  RTI
.endproc

; === MAIN LOOP ===
.proc main
forever:
  ; Wait for NMI
  LDA nmi_flag
  BEQ forever

  ; Clear NMI flag
  LDA #$00
  STA nmi_flag

  ; Increment frame counter
  INC frame_counter

  JSR read_input
  JSR update_player
  JSR update_rain
  JSR spawn_rain
  JSR check_collisions
  JMP forever
.endproc

; === CONTROLLER INPUT ===
.proc read_input
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016

  LDX #$08
read_loop:
  LDA $4016
  LSR A
  ROL controller
  DEX
  BNE read_loop
  RTS
.endproc

; === UPDATE PLAYER (2x speed) ===
.proc update_player
  ; Check right (D-pad right)
  LDA controller
  AND #%00000001
  BEQ check_left
  LDA player_x
  CMP #240
  BCS check_left
  INC player_x
  INC player_x  ; Double speed

check_left:
  ; Check left (D-pad left)
  LDA controller
  AND #%00000010
  BEQ check_up
  LDA player_x
  CMP #8
  BCC check_up
  DEC player_x
  DEC player_x  ; Double speed

check_up:
  ; Check up (D-pad up)
  LDA controller
  AND #%00001000
  BEQ check_down
  LDA player_y
  CMP #16
  BCC check_down
  DEC player_y
  DEC player_y  ; Double speed

check_down:
  ; Check down (D-pad down)
  LDA controller
  AND #%00000100
  BEQ done
  LDA player_y
  CMP #220
  BCS done
  INC player_y
  INC player_y  ; Double speed

done:
  RTS
.endproc

; === SPAWN RAIN ===
.proc spawn_rain
  ; Increment spawn timer
  INC rain_spawn_timer

  ; Spawn every 30 frames (about 0.5 seconds)
  LDA rain_spawn_timer
  CMP #30
  BCC done

  ; Reset timer
  LDA #$00
  STA rain_spawn_timer

  ; Find inactive rain drop
  LDA rain1_active
  BEQ spawn_rain1
  LDA rain2_active
  BEQ spawn_rain2
  LDA rain3_active
  BEQ spawn_rain3
  LDA rain4_active
  BEQ spawn_rain4
  JMP done  ; All active

spawn_rain1:
  LDA #$01
  STA rain1_active
  LDA frame_counter  ; Use frame counter for pseudo-random X
  AND #%11110000
  CLC
  ADC #16
  STA rain1_x
  LDA #16
  STA rain1_y
  JMP done

spawn_rain2:
  LDA #$01
  STA rain2_active
  LDA frame_counter
  AND #%01111000
  CLC
  ADC #32
  STA rain2_x
  LDA #16
  STA rain2_y
  JMP done

spawn_rain3:
  LDA #$01
  STA rain3_active
  LDA frame_counter
  AND #%00111100
  CLC
  ADC #48
  STA rain3_x
  LDA #16
  STA rain3_y
  JMP done

spawn_rain4:
  LDA #$01
  STA rain4_active
  LDA frame_counter
  AND #%11100000
  CLC
  ADC #64
  STA rain4_x
  LDA #16
  STA rain4_y

done:
  RTS
.endproc

; === UPDATE RAIN ===
.proc update_rain
  ; Update rain drop 1
  LDA rain1_active
  BEQ check_rain2
  INC rain1_y
  LDA rain1_y
  CMP #230
  BCC check_rain2
  LDA #$00
  STA rain1_active

check_rain2:
  LDA rain2_active
  BEQ check_rain3
  INC rain2_y
  LDA rain2_y
  CMP #230
  BCC check_rain3
  LDA #$00
  STA rain2_active

check_rain3:
  LDA rain3_active
  BEQ check_rain4
  INC rain3_y
  LDA rain3_y
  CMP #230
  BCC check_rain4
  LDA #$00
  STA rain3_active

check_rain4:
  LDA rain4_active
  BEQ done
  INC rain4_y
  LDA rain4_y
  CMP #230
  BCC done
  LDA #$00
  STA rain4_active

done:
  RTS
.endproc

; === CHECK COLLISIONS ===
.proc check_collisions
  ; Check collision with rain drop 1
  LDA rain1_active
  BEQ check_rain2
  JSR check_collision_rain1

check_rain2:
  LDA rain2_active
  BEQ check_rain3
  JSR check_collision_rain2

check_rain3:
  LDA rain3_active
  BEQ check_rain4
  JSR check_collision_rain3

check_rain4:
  LDA rain4_active
  BEQ done
  JSR check_collision_rain4

done:
  RTS
.endproc

; === COLLISION DETECTION HELPERS ===
.proc check_collision_rain1
  ; Check if player and rain1 overlap
  LDA player_x
  CMP rain1_x
  BCC no_collision

  LDA rain1_x
  CLC
  ADC #8
  CMP player_x
  BCC no_collision

  LDA player_y
  CMP rain1_y
  BCC no_collision

  LDA rain1_y
  CLC
  ADC #8
  CMP player_y
  BCC no_collision

  ; Collision detected!
  LDA #$00
  STA rain1_active
  JSR add_score

no_collision:
  RTS
.endproc

.proc check_collision_rain2
  LDA player_x
  CMP rain2_x
  BCC no_collision

  LDA rain2_x
  CLC
  ADC #8
  CMP player_x
  BCC no_collision

  LDA player_y
  CMP rain2_y
  BCC no_collision

  LDA rain2_y
  CLC
  ADC #8
  CMP player_y
  BCC no_collision

  LDA #$00
  STA rain2_active
  JSR add_score

no_collision:
  RTS
.endproc

.proc check_collision_rain3
  LDA player_x
  CMP rain3_x
  BCC no_collision

  LDA rain3_x
  CLC
  ADC #8
  CMP player_x
  BCC no_collision

  LDA player_y
  CMP rain3_y
  BCC no_collision

  LDA rain3_y
  CLC
  ADC #8
  CMP player_y
  BCC no_collision

  LDA #$00
  STA rain3_active
  JSR add_score

no_collision:
  RTS
.endproc

.proc check_collision_rain4
  LDA player_x
  CMP rain4_x
  BCC no_collision

  LDA rain4_x
  CLC
  ADC #8
  CMP player_x
  BCC no_collision

  LDA player_y
  CMP rain4_y
  BCC no_collision

  LDA rain4_y
  CLC
  ADC #8
  CMP player_y
  BCC no_collision

  LDA #$00
  STA rain4_active
  JSR add_score

no_collision:
  RTS
.endproc

; === ADD SCORE ===
.proc add_score
  LDA score_low
  CLC
  ADC #1  ; Add 1 point per raindrop (can change to 10 if desired)
  STA score_low

  ; Check for overflow
  CMP #100
  BCC no_overflow

  SBC #100
  STA score_low
  INC score_high

no_overflow:
  RTS
.endproc

; === VECTORS ===
.segment "VECTORS"
.addr nmi_handler
.addr reset_handler
.addr irq_handler

; === CHR-ROM (Tiles) ===
.segment "CHARS"
; Tile 0 = blank
.repeat 16
  .byte $00
.endrepeat

; Tile 1 = solid block
.repeat 8
  .byte %11111111
.endrepeat
.repeat 8
  .byte %00000000
.endrepeat

; Tiles 2-9 = Numbers 0-7 (basic font)
; Number 0 (tile 2)
.byte %00111100
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %01100110
.byte %00111100
.byte %00000000
.repeat 8
  .byte %00000000
.endrepeat

; Number 1 (tile 3)
.byte %00001100
.byte %00011100
.byte %00001100
.byte %00001100
.byte %00001100
.byte %00001100
.byte %00111111
.byte %00000000
.repeat 8
  .byte %00000000
.endrepeat

; Number 2 (tile 4)
.byte %00111100
.byte %01100110
.byte %00000110
.byte %00001100
.byte %00110000
.byte %01100000
.byte %01111110
.byte %00000000
.repeat 8
  .byte %00000000
.endrepeat

; Number 3 (tile 5)
.byte %00111100
.byte %01100110
.byte %00000110
.byte %00011100
.byte %00000110
.byte %01100110
.byte %00111100
.byte %00000000
.repeat 8
  .byte %00000000
.endrepeat

; Number 4 (tile 6)
.byte %00001100
.byte %00011100
.byte %00101100
.byte %01001100
.byte %01111110
.byte %00001100
.byte %00001100
.byte %00000000
.repeat 8
  .byte %00000000
.endrepeat

; Number 5 (tile 7)
.byte %01111110
.byte %01100000
.byte %01111100
.byte %00000110
.byte %00000110
.byte %01100110
.byte %00111100
.byte %00000000
.repeat 8
  .byte %00000000
.endrepeat

; Number 6 (tile 8)
.byte %00111100
.byte %01100000
.byte %01111100
.byte %01100110
.byte %01100110
.byte %01100110
.byte %00111100
.byte %00000000
.repeat 8
  .byte %00000000
.endrepeat

; Number 7 (tile 9)
.byte %01111110
.byte %00000110
.byte %00001100
.byte %00011000
.byte %00110000
.byte %00110000
.byte %00110000
.byte %00000000
.repeat 8
  .byte %00000000
.endrepeat

; Fill rest of CHR-ROM to exactly 8192 bytes
.repeat 8192 - 160
  .byte $00
.endrepeat
