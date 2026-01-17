ROW_BYTES    equ 160        ; bytes per text row (80 cols × 2 bytes)
LAST_ROW_OFF equ ROW_BYTES * 24   ; offset of start of bottom row (line 24)
LAST_COL_OFF equ 158        ; offset of col 79 within a row (79×2)

.model small                
.stack 100h                
.data
    score dw 0                  ; Player score
    next_dot_value db 'A'       ; Start with 'A'
    dot_offset dw ?             ; Current dot position
    dot_age dw 0                ; Aging counter for current dot
    dot_base_attr db 0          ; Original dot attribute (color)
    dot_expired db 0            ; Flag for expired dot (1 = needs removal)
    msg_wall db 'You hit the wall',0
    msg_score db 'Your Score is ',0
    score_buffer db 6 dup(0)    ; Buffer for score string
    
    ; Movement variables
    current_direction db 4      ; 0=none, 1=W, 2=A, 3=S, 4=D (start right)
    tick_counter db 0           ; Counter for timer ticks
    player_offset dw 2000       ; Player position (center)
    
    ; Interrupt handling
    old_int1c_offset dw ?       ; Original INT 1Ch vector
    old_int1c_segment dw ?
    old_int09_offset dw ?       ; Original INT 09h vector
    old_int09_segment dw ?
    
    ; Game state
    game_active db 1            ; 1=active, 0=game over
    key_pressed db 0            ; Keyboard buffer

.code
;----------------------------------------------------------
; New timer interrupt handler (INT 1Ch) with dot aging
;----------------------------------------------------------
new_int1c_handler proc far
    pushf
    push ax
    push bx
    push cx
    push dx
    push di
    push ds
    push es
    
    ; Only process if game active
    cmp game_active, 1
    jne skip_dot_aging
    
    mov ax, @data
    mov ds, ax
    
    ; Only move if direction set
    cmp current_direction, 0
    je check_dot_aging
    
    ; Count ticks (4 ticks = ~220ms)
    inc tick_counter
    cmp tick_counter, 4
    jb check_dot_aging
    
    ; Reset counter and move
    mov tick_counter, 0
    call MovePlayerAuto
    
check_dot_aging:
    ; Age the dot if exists
    cmp dot_offset, 0
    je skip_dot_aging
    inc dot_age
    
    ; Check for color change (60 ticks = 3.3 seconds)
    cmp dot_age, 60
    jne check_dot_expiry
    call AgeDotColor
    
check_dot_expiry:
    ; Check for removal (120 ticks = 6.6 seconds)
    cmp dot_age, 120
    jne skip_dot_aging
    mov dot_expired, 1
    
skip_dot_aging:
    pop es
    pop ds
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    popf
    iret
new_int1c_handler endp

;----------------------------------------------------------
; Change dot color to yellow background (aging)
;----------------------------------------------------------
AgeDotColor proc
    push ax
    push bx
    push es
    
    mov ax, 0B800h
    mov es, ax
    mov bx, dot_offset
    
    ; Get current character
    mov al, es:[bx]
    
    ; Set yellow background (0xE0) while keeping original foreground
    mov ah, dot_base_attr
    and ah, 0Fh           ; Keep only foreground 
    or ah, 0E0h           ; Yellow background
    mov es:[bx], ax
    
    pop es
    pop bx
    pop ax
    ret
AgeDotColor endp

;----------------------------------------------------------
; Remove expired dot
;----------------------------------------------------------
RemoveExpiredDot proc
    push ax
    push di
    push es
    
    mov ax, 0B800h
    mov es, ax
    mov di, dot_offset
    
    ; Clear dot (space with black background)
    mov word ptr es:[di], 0020h
    
    ; Reset dot tracking
    mov dot_offset, 0
    mov dot_age, 0
    mov dot_expired, 0
    
    pop es
    pop di
    pop ax
    ret
RemoveExpiredDot endp

;----------------------------------------------------------
; Auto-movement based on current direction
;----------------------------------------------------------
MovePlayerAuto proc
    push ax
    cmp current_direction, 1
    je move_up_auto
    cmp current_direction, 2
    je move_left_auto
    cmp current_direction, 3
    je move_down_auto
    cmp current_direction, 4
    je move_right_auto
    jmp end_auto_move
    
move_up_auto:
    call MoveUp
    jmp end_auto_move
move_left_auto:
    call MoveLeft
    jmp end_auto_move
move_down_auto:
    call MoveDown
    jmp end_auto_move
move_right_auto:
    call MoveRight
    
end_auto_move:
    pop ax
    ret
MovePlayerAuto endp

;----------------------------------------------------------
; Movement procedures
;----------------------------------------------------------
MoveUp proc
    push ax
    push bx
    push cx
    push di
    
    mov bx, player_offset
    cmp bx, ROW_BYTES
    jb WallHit
    mov di, bx
    sub di, ROW_BYTES
    cmp byte ptr es:[di], '#'
    je WallHit
    
    mov ax, word ptr es:[bx]
    push ax
    mov al, 20h
    mov word ptr es:[bx], ax
    pop ax
    sub bx, ROW_BYTES
    mov word ptr es:[bx], ax
    mov player_offset, bx
    call CheckDotCollision
    
    pop di
    pop cx
    pop bx
    pop ax
    ret
MoveUp endp

MoveLeft proc
    push ax
    push bx
    push cx
    push dx
    push di
    
    mov bx, player_offset
    mov ax, bx
    xor dx, dx
    mov cx, ROW_BYTES
    div cx
    cmp dx, 0
    je WallHit
    
    mov di, bx
    sub di, 2
    cmp byte ptr es:[di], '#'
    je WallHit
    
    mov ax, word ptr es:[bx]
    push ax
    mov al, 20h
    mov word ptr es:[bx], ax
    pop ax
    sub bx, 2
    mov word ptr es:[bx], ax
    mov player_offset, bx
    call CheckDotCollision
    
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret
MoveLeft endp

MoveDown proc
    push ax
    push bx
    push cx
    push di
    
    mov bx, player_offset
    cmp bx, LAST_ROW_OFF
    jae WallHit
    
    mov di, bx
    add di, ROW_BYTES
    cmp byte ptr es:[di], '#'
    je WallHit
    
    mov ax, word ptr es:[bx]
    push ax
    mov al, 20h
    mov word ptr es:[bx], ax
    pop ax
    add bx, ROW_BYTES
    mov word ptr es:[bx], ax
    mov player_offset, bx
    call CheckDotCollision
    
    pop di
    pop cx
    pop bx
    pop ax
    ret
MoveDown endp

MoveRight proc
    push ax
    push bx
    push cx
    push dx
    push di
    
    mov bx, player_offset
    mov ax, bx
    xor dx, dx
    mov cx, ROW_BYTES
    div cx
    cmp dx, LAST_COL_OFF
    je WallHit
    
    mov di, bx
    add di, 2
    cmp byte ptr es:[di], '#'
    je WallHit
    
    mov ax, word ptr es:[bx]
    push ax
    mov al, 20h
    mov word ptr es:[bx], ax
    pop ax
    add bx, 2
    mov word ptr es:[bx], ax
    mov player_offset, bx
    call CheckDotCollision
    
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret
MoveRight endp

;----------------------------------------------------------
; Game events
;----------------------------------------------------------
WallHit:
    mov game_active, 0
    
    ; Print messages
    mov di, 12*160 + 30*2
    lea si, msg_wall
    call PrintString
    
    mov di, 13*160 + 30*2
    lea si, msg_score
    call PrintString
    
    ; Print score
    mov ax, score
    lea di, score_buffer
    call ConvertWordToString
    mov di, 13*160 + 30*2 + 14*2
    lea si, score_buffer
    call PrintString
    
    ; Wait for key
    mov ah, 00h
    int 16h
    jmp ExitProgram

;----------------------------------------------------------
; Dot collision and placement
;----------------------------------------------------------
CheckDotCollision PROC NEAR
    push ax
    push bx
    
    mov bx, player_offset
    mov ax, dot_offset
    cmp bx, ax
    jne no_collision
    
    ; ======================= CORRECTED CODE BLOCK STARTS HERE =======================
    ; Handle collision
    inc score

    ; Update player appearance as per requirements
    ; 1. Take the new score and convert it to an ASCII character
    mov ax, score
    add al, '0'             ; AL now holds the digit's character (e.g., '1')

    ; 2. Set the player's color: red (4) on a black (0) background
    mov ah, 04h             ; AH now holds the color attribute

    ; 3. Draw the updated player (character and color) at its current location
    mov bx, player_offset
    mov word ptr es:[bx], ax ; Write char and attribute, overwriting the dot

    ; Continue game logic
    ; Update the value for the next dot to be placed
    inc next_dot_value
    cmp next_dot_value, 'J'
    jb no_reset_final
    mov next_dot_value, 'A'
no_reset_final:

    ; Reset the state of the collected dot and place a new one
    mov dot_offset, 0
    mov dot_age, 0
    mov dot_expired, 0
    
    call PlaceNumberedDot
    ; ======================= CORRECTED CODE BLOCK ENDS HERE =======================
    
no_collision:
    pop bx
    pop ax
    ret
CheckDotCollision ENDP

PlaceNumberedDot PROC NEAR
    push ax
    push bx
    push cx
    push dx
    push di
    
    ; Reset dot tracking
    mov dot_age, 0
    mov dot_expired, 0
    
    mov cx, 100         ; Max attempts

try_again:
    ; Random row (0-24)
    call GetRandom
    mov ax, dx
    xor dx, dx
    mov bx, 25
    div bx
    mov di, dx

    ; Random column (0-79)
    call GetRandom
    mov ax, dx
    xor dx, dx
    mov bx, 80
    div bx

    ; Calculate offset
    push cx
    mov cx, dx 
    mov ax, di
    mov bx, ROW_BYTES
    mul bx
    shl cx, 1
    add ax, cx
    pop cx
    mov di, ax

    ; Validate position
    cmp di, 2000        ; Not on player start
    je invalid
    cmp di, player_offset ; Not on player
    je invalid
    cmp byte ptr es:[di], 20h  ; Must be empty
    jne invalid
    cmp byte ptr es:[di], '#'  ; Not on wall
    je invalid
    
    jmp place_dot
    
invalid:
    loop try_again
    mov di, 0
        
place_dot:
    mov al, next_dot_value
    mov ah, 0Fh         ; White by default
    test al, 1
    jz not_odd
    mov ah, 02h         ; Green for odd letters
not_odd:
    mov dot_base_attr, ah  ; Store base color
    mov word ptr es:[di], ax
    mov dot_offset, di
    
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret
PlaceNumberedDot ENDP

;----------------------------------------------------------
; Utility functions
;----------------------------------------------------------
GetRandom PROC NEAR
    push ax
    mov ah, 00h
    int 1Ah
    pop ax
    ret
GetRandom ENDP

PlaceRandomWall PROC NEAR
    push ax
    push bx
    push dx
    push di
    push cx

    mov cx, 100

RetryRandomWall:
    call GetRandom
    mov ax, dx
    xor dx, dx
    mov bx, 25
    div bx
    mov di, dx

    call GetRandom
    mov ax, dx
    xor dx, dx
    mov bx, 80
    div bx
    mov bx, dx

    ; Avoid center position
    cmp di, 12
    jne CheckEmpty
    cmp bx, 40
    je AttemptFailed

CheckEmpty:
    mov ax, di
    mov dx, ROW_BYTES
    mul dx
    push bx
    shl bx, 1
    add ax, bx
    pop bx
    mov di, ax

    cmp byte ptr es:[di], 20h
    jne AttemptFailed

    ; Place wall (red '#')
    mov ax, 0F23h
    mov word ptr es:[di], ax
    jmp EndProc

AttemptFailed:
    loop RetryRandomWall

EndProc:
    pop cx
    pop di
    pop dx
    pop bx
    pop ax
    ret
PlaceRandomWall ENDP

SimpleDelay PROC NEAR
    push cx
    push dx
    mov cx, 20
OuterLoop:
    mov dx, 0FFFFh
InnerLoop:
    dec dx
    jne InnerLoop
    loop OuterLoop
    pop dx
    pop cx
    ret
SimpleDelay ENDP

PrintString PROC NEAR
    push ax
    push si
    push di
    mov ah, 0Fh
next_char:
    lodsb
    cmp al, 0
    je end_print
    stosw
    jmp next_char
end_print:
    pop di
    pop si
    pop ax
    ret
PrintString ENDP

ConvertWordToString PROC NEAR
    push ax
    push bx
    push cx
    push dx
    push di
    
    test ax, ax
    jnz not_zero
    mov byte ptr [di], '0'
    inc di
    jmp null_terminate
    
not_zero:
    mov bx, 10
    xor cx, cx
        
div_loop:
    xor dx, dx
    div bx
    add dl, '0'
    push dx
    inc cx
    test ax, ax
    jnz div_loop
        
pop_loop:
    pop dx
    mov [di], dl
    inc di
    loop pop_loop
        
null_terminate:
    mov byte ptr [di], 0
    
    pop di
    pop dx
    pop cx
    pop bx
    pop ax
    ret
ConvertWordToString ENDP

;----------------------------------------------------------
; Main program
;----------------------------------------------------------
START: 
    mov ax, @data
    mov ds, ax
    
    ; Save original interrupt vectors
    cli
    ; Keyboard (INT 09h)
    mov ax, 3509h
    int 21h
    mov old_int09_offset, bx
    mov old_int09_segment, es
    
    ; Timer (INT 1Ch)
    mov ax, 351Ch
    int 21h
    mov old_int1c_offset, bx
    mov old_int1c_segment, es
    
    ; Set new interrupt vectors
    ; Keyboard
    mov ax, 2509h
    mov dx, offset new_int09_handler
    push ds
    push cs
    pop ds
    int 21h
    pop ds
    
    ; Timer
    mov ax, 251Ch
    mov dx, offset new_int1c_handler
    push ds
    push cs
    pop ds
    int 21h
    pop ds
    
    ; Enable interrupts
    in al, 21h
    and al, 0FCh     ; Unblock IRQ0 (timer) and IRQ1 (keyboard)
    out 21h, al
    sti
    
    ; Initialize game
    mov game_active, 1
    mov current_direction, 4    ; Start moving right
    
    ; Clear screen
    mov ax, 0B800h
    mov es, ax
    mov cx, 2000
    mov bx, 0
    mov ax, 0420h    ; Red background, space (this sets the whole background to red)
    
clear_loop:
    mov word ptr es:[bx], ax
    add bx, 2
    loop clear_loop
    
    ; Place walls
    call PlaceRandomWall
    call SimpleDelay
    call PlaceRandomWall
    call SimpleDelay
    call PlaceRandomWall
    call SimpleDelay
    
    ; Draw player
    mov bx, player_offset
    mov ax, 0430h    ; Red-on-Black '0' 
    mov word ptr es:[bx], ax
    
    ; Place first dot
    call PlaceNumberedDot
    
    ; Main game loop
MainLoop:
    ; Check for expired dot
    cmp dot_expired, 1
    jne CheckKeyboard
    
    ; Remove expired dot and place new one
    call RemoveExpiredDot
    call PlaceNumberedDot
    
CheckKeyboard:
    ; Check keyboard buffer
    cmp key_pressed, 0
    je NoKey
    
    ; Process key press
    mov al, key_pressed
    mov key_pressed, 0  ; Clear buffer
    
    ; Check for direction keys
    cmp al, 11h      ; W
    je W_pressed
    cmp al, 1Eh      ; A
    je A_pressed
    cmp al, 1Fh      ; S
    je S_pressed
    cmp al, 20h      ; D
    je D_pressed
    cmp al, 14h      ; T
    je DisplayScoreAndExit
    jmp NoKey
    
W_pressed:
    mov current_direction, 1
    jmp NoKey
A_pressed:
    mov current_direction, 2
    jmp NoKey
S_pressed:
    mov current_direction, 3
    jmp NoKey
D_pressed:
    mov current_direction, 4
    jmp NoKey
    
DisplayScoreAndExit:
    mov game_active, 0
    mov di, 12*160 + 30*2
    lea si, msg_score
    call PrintString
    mov ax, score
    lea di, score_buffer
    call ConvertWordToString
    mov di, 12*160 + 30*2 + 14*2
    lea si, score_buffer
    call PrintString
    mov ah, 00h
    int 16h
    jmp ExitProgram
    
NoKey:
    ; Check if game still active
    cmp game_active, 1
    jne ExitLoop
    
    ; Small delay to prevent CPU hogging
    mov cx, 1000
DelayLoop:
    nop
    loop DelayLoop
    
    jmp MainLoop
    
ExitLoop:
    jmp ExitProgram

;----------------------------------------------------------
; New keyboard interrupt handler (INT 09h)
;----------------------------------------------------------
new_int09_handler proc far
    pushf
    push ax
    push bx
    push ds
    
    ; Read scan code
    in al, 60h
    
    ; Store in buffer
    mov bx, @data
    mov ds, bx
    mov [key_pressed], al
    
    ; Send EOI to keyboard controller
    in al, 61h
    mov ah, al
    or al, 80h
    out 61h, al
    mov al, ah
    out 61h, al
    
    ; Send EOI to PIC
    mov al, 20h
    out 20h, al
    
    pop ds
    pop bx
    pop ax
    popf
    iret
new_int09_handler endp

;----------------------------------------------------------
; Clean exit
;----------------------------------------------------------
ExitProgram:
    cli
    
    ; Restore original interrupt vectors
    ; Keyboard
    mov dx, old_int09_offset
    mov ds, old_int09_segment
    mov ax, 2509h
    int 21h
    
    ; Timer
    mov dx, old_int1c_offset
    mov ds, old_int1c_segment
    mov ax, 251Ch
    int 21h
    
    ; Restore PIC
    in al, 21h
    or al, 03h
    out 21h, al

    sti ; Re-enable interrupts before exiting
    
    ; Exit to DOS
    mov ax, 4C00h
    int 21h
END START