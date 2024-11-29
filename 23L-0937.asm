[org 0x0100]

jmp start

positionArr: times 80 dw 0
prevPosition: times 80 dw 0xFFFF
alphabetsArr: times 80 dw 0 
colorArr: times 80 db 01h, 02h, 03h, 04h, 05h, 06h, 07h, 08h, 09h, 0Ah
rand: dw 0
randnum: dw 0
alphabetNum: db 5
arr: times 2000 dw 0
oldisr: dd 0
oldtimer: dd 0
win: db 0

AlphaCatched: dw 0
AlphaDropped: dw 0

boxAddr: dw 3920
prevBoxAddr: dw 3920

boxAddr2: dw 3880
prevBoxAddr2: dw 3880

sta1: db "________  ________  _________  _______  ___  ___  ___  ________   ______        "
		db "\   ____\|\   __  \|\___   __\|\  ____\|\  \|\  \|\  \|\   ___  \|\  ___\       "
		db "\ \  \___|\ \  \|\  \|___ \  \ \ \ \___|\ \  \\\  \ \  \ \  \\ \  \ \ \__|      "
		db "  \ \  \    \ \   __  \   \ \  \ \ \ \    \ \   __  \ \  \ \  \\ \  \ \ \  __   "
		db "    \ \  \__  \ \   __  \   \ \  \ \ \ \ __ \ \   __  \ \  \ \  \\ \  \ \ \  _\ "
		db "      \ \_______\ \__\ \__\   \ \__\ \ \______\ \__\ \__\ \__\ \__\\ \__\ \____ "
		db "        \|_______|\|__|\|__|    \|__|  \|______|\|__|\|__|\|__|\|__| \|__|\|____"
		db " ________  ___  ___  ________  ________  ________                               "
		db "|\   ____\|\  \|\  \|\   __  \|\   __  \|\   ____\                              "
		db "\ \  \___|\ \  \\\  \ \  \|\  \ \  \|\  \ \  \___|_                             "
		db "  \ \  \    \ \   __  \ \   __  \ \   _  _\ \_____  \                           "
		db "    \ \  \____\ \  \ \  \ \  \ \  \ \  \\  \\|____|\  \                         "
		db "      \ \_______\ \__\ \__\ \__\ \__\ \__\\ _\ ____\_\  \                       "
		db "        \|_______|\|__|\|__|\|__|\|__|\|__|\|__|\_________\                     "
		db "                                                \|_________|                    "

sta16: db "PRESS ANY KEY TO START!"
dropText: db "Dropped :" , 0
catchedText: db "Catched :" , 0
singleplayerText:db "SinglePlayer", 0
multiplayerText:db "MultiPlayer", 0
playAgainText:db "Play Again", 0
exitText:db "Exit", 0
lostText: db "You lost!", 0
winText: db "You Win!", 0
chooseText: db "Choose Your Mode", 0
timerText: db "Timer   :", 0
scoreText: db "Your Score :", 0


rightUp: db 1
leftUp: db 1
aKeyUp: db 1
dKeyUp: db 1
lShiftUp: db 1
rShiftUp: db 1

endFlag: db 0
keyPressed: db 0
modType: db 1

speed: dw 0
timerCount: db 5

timerScreen: dw 0

kbisr:
	push ax
	push es
	push bx
	

	in al, 0x60
	cmp al, 0x01
	jne rDown
	
	mov byte [endFlag], 1
	jmp decide_key_presses
	
	rDown:
		cmp al, 0x4D
		jne rUp
		mov byte [rightUp], 0
		jmp decide_key_presses
	rUp:
		cmp al, 0xCD
		jne lDown
		mov byte [rightUp], 1
		jmp decide_key_presses
		
	lDown:
		cmp al, 0x4B
		jne lUp
		mov byte [leftUp], 0
		jmp decide_key_presses
		
	lUp:
		cmp al, 0xCB
		jne aDown
		mov byte [leftUp], 1
		jmp decide_key_presses
		
	rShiftKeyUp:
		cmp al, 0xB6
		jne rShiftKeyDown
		mov byte [rShiftUp], 1
		jmp decide_key_presses
	
	
	rShiftKeyDown:
		cmp al, 0x36
		jne mod_for_key
		mov byte [rShiftUp], 0
		jmp decide_key_presses
	
mod_for_key:
	cmp byte [modType], 1
	je decide_key_presses
	
	
		
	aDown:
		cmp al, 0x1E
		jne aUp
		mov byte [aKeyUp], 0
		jmp decide_key_presses
	aUp:
		cmp al, 0x9E
		jne dDown
		mov byte [aKeyUp], 1
		jmp decide_key_presses
		
	dDown:
		cmp al, 0x20
		jne dkUp
		mov byte [dKeyUp], 0
		jmp decide_key_presses
		
	dkUp:
		cmp al, 0xA0
		jne lShiftKeyUp
		mov byte [dKeyUp], 1
		jmp decide_key_presses
		
	lShiftKeyUp:
		cmp al, 0xAA
		jne lShiftKeyDown
		mov byte [lShiftUp], 1
		jmp decide_key_presses
		
	lShiftKeyDown:
		cmp al, 0x2A
		jne decide_key_presses
		mov byte [lShiftUp], 0
		
		
decide_key_presses:
	
		cmp byte [rightUp], 0
		jne left_arrow_key
	
		cmp word [boxAddr], 3996
		jge left_arrow_key
	
		mov bx, [boxAddr]
		mov [prevBoxAddr], bx
	
		add word [boxAddr], 2
		
		cmp byte [rShiftUp], 0
		jne left_arrow_key
		
		cmp word [boxAddr], 3996
		jge left_arrow_key
	
		mov bx, [boxAddr]
		mov [prevBoxAddr], bx
	
		add word [boxAddr], 2
	
		
	left_arrow_key:
		cmp byte [leftUp], 0
		jne mod_for_decide
		
		cmp word [boxAddr], 3840
		jle mod_for_decide
		
		mov bx, [boxAddr]
		mov [prevBoxAddr], bx
		sub word [boxAddr], 2
		
		cmp byte [rShiftUp], 0
		jne mod_for_decide
		
		cmp word [boxAddr], 3840
		jle mod_for_decide
		
		mov bx, [boxAddr]
		mov [prevBoxAddr], bx
		sub word [boxAddr], 2
	
mod_for_decide:
	cmp byte [modType], 1
	je kb_end
	
	d_key:
		cmp byte [dKeyUp], 0
		jne a_key
	
		cmp word [boxAddr2], 3996
		jge a_key
	
		mov bx, [boxAddr2]
		mov [prevBoxAddr2], bx
	
		add word [boxAddr2], 2
		
		cmp byte [lShiftUp], 0
		jne a_key
		
		cmp word [boxAddr2], 3996
		jge a_key
	
		mov bx, [boxAddr2]
		mov [prevBoxAddr2], bx
	
		add word [boxAddr2], 2
		
	

	a_key:
		cmp byte [aKeyUp], 0
		jne kb_end
		
		cmp word [boxAddr2], 3840
		jle kb_end
		mov bx, [boxAddr2]
		mov [prevBoxAddr2], bx
		sub word [boxAddr2], 2
		
		cmp byte [lShiftUp], 0
		jne kb_end
		
		cmp word [boxAddr2], 3840
		jle kb_end
		
		mov bx, [boxAddr2]
		mov [prevBoxAddr2], bx
		sub word [boxAddr2], 2
		
	
	
kb_end:
	call boxPrint

	mov al, 20h
	out 20h, al
	mov byte [keyPressed], 1
	
	pop bx
	pop es
	pop ax
iret


menu_kbisr:
	push ax
	push es
	push bx
	
	in al, 0x60
	
	cmp al, 0x1C ; enter
	jne menu_right_arrow_key
	mov byte [endFlag], 1
	jmp menu_kbend
	
menu_right_arrow_key:
	cmp al, 0x4D
	jne menu_left_arrow_key
	
	mov byte [modType], 2
	
	jmp kb_end
menu_left_arrow_key:
	cmp al, 0x4B
	jne menu_kbend
	
	mov byte [modType], 1
	
menu_kbend:
	
	mov al, 20h
	out 20h, al

	pop bx
	pop es
	pop ax
iret

strlen:
	push bp
	mov bp, sp
	push ax
	push cx
	push si
	push di
	push es
	push ds
	
	push ds
	pop es
	
	mov di, [bp+4]
	
	xor ax, ax
	mov cx, 0xFFFF
	
	repne scasb
	mov ax, 0xFFFF
	sub ax, cx
	dec ax
	mov  [bp+6], ax
	
	pop ds
	pop es
	pop di
	pop si
	pop cx
	pop ax
	pop bp

	ret 2
	
	
printStr:
	push bp
	mov bp, sp
	push ax
	push cx
	push si
	push di
	push es
	push ds
	
	sub sp, 2
	push word [bp+4]
	call strlen
	pop cx
	
	push 0xb800
	pop es
	
	mov ax, [bp+8]	
	mov di, [bp+6]
	mov si, [bp+4]
	
	cld
	next_char_print:
		lodsb
		stosw
		loop next_char_print
		
	pop ds
	pop es
	pop di
	pop si
	pop cx
	pop ax
	pop bp
	ret 6

boxPrint:

	push bx
	push es
	push di
	push ax
	push cx
	
	push 0xb800
	pop es
	
	mov cx, 160
	
	mov ax, 0x0720
	mov di, 3840
	
	rep stosw
	
	mov di, [prevBoxAddr]
	mov word [es:di], 0x0720
	add di, 2
	mov word [es:di], 0x0720
	
	mov di, [boxAddr]
	mov word [es:di], 0x04DF
	add di, 2
	mov word [es:di], 0x04DF
	
	cmp byte [modType], 2
	jne end_box_print
	
	mov di, [prevBoxAddr2]
	mov word [es:di], 0x0720
	add di, 2
	mov word [es:di], 0x0720
	
	mov di, [boxAddr2]
	mov word [es:di], 0x03DF
	add di, 2
	mov word [es:di], 0x03DF
end_box_print:

	pop cx
	pop ax
	pop di
	pop es
	pop bx
ret

timer:
	push bp
	mov bp, sp
	push ax
	push es
	push cx
	push dx
	push bx
	push ds
	
	
	
	add byte [speed], 1
	cmp word [speed], 0xffff
	jl endInt8
	
	
	mov cx, 0xFFFF
	timer_delay: loop timer_delay
	
	add word [timerScreen], 1
	
	mov word [speed], 0
	add byte [timerCount], 1
	cmp byte [timerCount], 80
	jle noInt8Round
	
	mov byte [timerCount], 0	
noInt8Round:
	
	mov cl, [alphabetNum]
	xor bx, bx
	
Int8increment_pos_next:
	shr bx, 1
	add bx, 1
	
	sub sp, 2
	push bx
	push word [timerCount]
	call divides
	pop ax
	sub bx, 1
	shl bx, 1
	cmp ax, 0
	jne Int8no_incre
	
	mov ax, [positionArr+bx]
	mov [prevPosition+bx], ax
	; catch logic

	sub sp, 2
	push ax
	call checkForCatch
	pop dx
	
	cmp dx, 1
	je createNew
	
notCatched:	

	add ax, 160
	cmp ax, 4160
	
	jle Int8no_new_alphabet
	add  word [AlphaDropped], 1
createNew:

	sub sp, 2
	push bx

	call create_new_alphabet
	pop ax
	
Int8no_new_alphabet:
	mov [positionArr+bx], ax
	
Int8no_incre:
	add bx, 2
		
	loop Int8increment_pos_next
	
	call printScreen
	
endInt8:
	
	call boxPrint
	
	mov al, 0x20
	out 0x20, al
	
	pop ds
	pop bx
	pop dx
	pop cx
	pop es
	pop ax
	pop bp
iret


checkForCatch:
	push bp
	mov bp, sp
	push dx
	push ax
	
	mov dx, [AlphaCatched]
	mov ax, [bp+4]
	
	cmp ax, [boxAddr]
	jne next_catch_check
		add word [AlphaCatched], 1
		
	next_catch_check:
		sub ax, 2
		cmp ax, [boxAddr]
		jne catch_check_for_2
			add word [AlphaCatched], 1
			add ax, 2
			jmp catch_check_for_2

	catch_check_for_2:
		
		cmp byte [modType], 1
		je decide_if_catched
		
		cmp ax, [boxAddr2]
		jne next_catch_check_2
			add word [AlphaCatched], 1
		
	next_catch_check_2:
		add ax, 2
		cmp ax, [boxAddr2]
		jne decide_if_catched
			add word [AlphaCatched], 1
			
decide_if_catched:	
	
	cmp dx,  [AlphaCatched]
	je not_need_for_change
		mov word [bp+6], 1     ; changed
not_need_for_change:
		mov word [bp+6], 0          ; not changed
		
	pop ax
	pop dx
	pop bp
ret 2

clrscreen:
	push bp
	mov bp, sp
	push es
	push ax
	push di
	push cx

	mov ax, 0xb800
	mov es, ax

	xor di, di
	mov cx, 2000
	mov ax, 0x0720

	rep stosw

	pop cx
	pop di
	pop ax
	pop es
	pop bp
	ret

sleep:
	push cx
	mov cx, 10
	
delay:
	push cx
	mov cx, 0XFFFF
delay_next:
	loop delay_next
	pop cx
	loop delay
	
	pop cx
	ret
	
randG:
	push bp
	mov bp, sp
	pusha
	cmp word [rand], 0
	jne next

	MOV     AH, 00h   
	INT     1AH
	inc word [rand]
	mov     [randnum], dx
	jmp next1

next:
	mov ax, 25173          
	mul     word  [randnum]     
	add ax, 13849         
	mov     [randnum], ax        

	next1:xor dx, dx
	mov ax, [randnum]
	mov cx, [bp+4]
	inc cx
	div cx
 
	mov [bp+6], dx
	popa
	pop bp
ret 2
   
init:
	push bp
	mov bp, sp
	push ax
	push es
	push cx
	push dx
	push bx
	push ds
	
	mov cl, [alphabetNum]
	xor bx, bx
	
init_nextPos:
	
	sub sp, 2
	push word 79
	call randG
	pop ax
	
	shl ax, 1
	
	mov [positionArr+ bx], ax
	
	add bx, 2
	loop init_nextPos
	
	mov cl, [alphabetNum]
	xor bx, bx
	xor si, si
	
init_nextAlphabet:
	xor ax, ax
	sub sp, 2
	push word 25
	call randG
	pop ax
	add al, 65
	mov ah, [colorArr+si]
	mov [alphabetsArr+bx], ax
	
	inc si
	add bx, 2
	loop init_nextAlphabet
	
	pop ds
	pop bx
	pop dx
	pop cx
	pop es
	pop ax
	pop bp
ret

printNum:
	push bp
	mov bp, sp
	push ax
	push es
	push cx
	push dx
	push bx
	push ds
	
	mov ax, 0xb800
	mov es,  ax
	
	
	mov di, [bp+6]
	
	mov ax, [bp+4]
	mov bx, 10
	
getDigit:
	mov dx, 0
	div bx
	add dl, 30h
	push dx
	inc cx
	cmp ax, 0
	jnz getDigit
	
	mov ax, [bp+8]
	
print_digit:
	pop dx
	mov dh, ah
	mov [es:di], dx
	add di, 2
	loop print_digit
	
	pop ds
	pop bx
	pop dx
	pop cx
	pop es
	pop ax
	pop bp
	
	ret 6

printScreen:
	push bp
	mov bp, sp
	push ax
	push es
	push cx
	push dx
	push bx
	push ds
	
	push 0xb800
	pop es
	
	mov si, alphabetsArr
	xor bx, bx
	
	mov cl, [alphabetNum]
	
print_next:
	
	mov di, [prevPosition+bx]
	cmp di, 0xFFFF
	
	je noPrevRemove
	
	cmp di, [positionArr+bx]
	je print_no
	
	
	mov ax, 0x0720
	stosw 		;ax->ds:di

noPrevRemove:
	mov di, [positionArr+bx]
	lodsw		; ds:si->ax
	stosw 		;ax->es:di
	jmp printed
	
print_no:
	add si, 2
printed:
	add bx, 2
	
	loop print_next
	
	call boxPrint
	
	push 0707
	push 130
	push catchedText
	call printStr
	
	push 0707
	push 290
	push dropText
	call printStr
	
	push 0707
	push 450
	push timerText
	call printStr
	
	push 0707
	push 154
	push word [AlphaCatched]
	call printNum
	
	push 0707
	push 314
	push word [AlphaDropped]
	call printNum
	
	push 0707
	push 474
	push word [timerScreen]
	call printNum
	
	
	pop ds
	pop bx
	pop dx
	pop cx
	pop es
	pop ax
	pop bp
ret



divides:
	push bp
	mov bp, sp
	push ax
	push es
	push cx
	push dx
	push bx
	push ds
	
	xor ax, ax
	mov es, ax
	
	
	xor dx, dx
	mov ax, [bp+4]
	mov bx, [bp+6]
	inc bx
	div bx

	mov [bp+8], dx
	
	pop ds
	pop bx
	pop dx
	pop cx
	pop es
	pop ax
	pop bp
ret 4

create_new_alphabet:
	push bp
	mov bp, sp
	push ax
	push es
	push cx
	push dx
	push bx
	push ds
	
	mov bx, [bp+4]

	xor ax, ax
	sub sp, 2
	push word 25
	call randG
	pop ax
	add al, 65
	shr bx, 1
	mov ah, [colorArr+bx]
	shl bx, 1
	mov [alphabetsArr+bx], ax
	mov word [prevPosition+bx], 0xFFFF
	
	
	xor ax, ax
	sub sp, 2
	push word 79
	call randG
	pop ax
	shl ax, 1
	mov [bp+6], ax
	
	pop ds
	pop bx
	pop dx
	pop cx
	pop es
	pop ax
	pop bp
ret 2



game:
	push bp
	push bx
	
	mov bx, 10
	cmp byte [modType], 2
	jne only_10_score
		mov bx, 20
	only_10_score:

	game_loop:
		cmp byte [endFlag], 1
		je game_end
		
		cmp bx, [AlphaCatched]
		je game_end
		
		cmp word [AlphaCatched], 8
		jne not_increase_game_speed
			mov ax, 32767
			out 0x40, al
			mov al, ah
			out 0x40, al
	not_increase_game_speed:
		
		cmp  word  [AlphaDropped], 10
		je game_end
		
		jmp game_loop
	
	game_end:

	mov byte [endFlag], 0
	
	cmp  word  [AlphaDropped], 10
	je game_end_end
	mov byte [win], 1
	
game_end_end:
	pop bx
	pop bp
ret


startScreen:
	push bp
	mov bp, sp
	push ax
	push es
	push cx
	push dx
	push bx
	push ds
	
	push 0xb800
	pop es
	
	mov di, 320
	mov ah, 0x07
	

	mov cx, 1200
	mov si, sta1
start_next_line:
	lodsb
	stosw
	loop start_next_line
	
	
	mov si, sta16
	mov di, 3096
	mov cx, 23
	mov ah, 10000111b
start_message:
	lodsb
	stosw
	loop start_message
	
	mov ah, 0
	int 16h
	
;start_wait:
	;cmp byte [keyPressed], 1
	;je start_wait_stop
	;jmp start_wait
;start_wait_stop:

	;mov byte [keyPressed], 0
	
	pop ds
	pop bx
	pop dx
	pop cx
	pop es
	pop ax
	pop bp
ret

drawRectangle:
	push bp
	mov bp, sp
	push ax
	push es
	push cx
	push dx
	push bx
	push ds
	
	push 0xb800
	pop es
	
	mov cx, 3
	mov di, [bp+4]
	;0x87FE
	mov bx, 0
drawLine:
	push cx
	mov cx, [bp+6]
	mov ax, [bp+8]
	rep stosw
	
	pop cx
	
	mov di, [bp+4]
	
	mov ax, 160
	inc bx
	xor dx, dx
	mul bx
	add di, ax
	
	loop drawLine


	pop ds
	pop bx
	pop dx
	pop cx
	pop es
	pop ax
	pop bp
ret 6

menuScreen:
	push bp
	mov bp, sp
	push ax
	push es
	push cx
	push dx
	push bx
	push ds
	
	call clrscreen
	

	push 0
	pop es
	
	cli
	mov word [es:9*4], menu_kbisr
	mov word [es:9*4+2], cs
	sti
	
	mov word [endFlag], 0
menu_loop:
	call clrscreen
	
	cmp byte [modType], 1
	jne select_for_mutlti
	push 0x87FE
	push 14
	push 1798
	call drawRectangle
	
	;push 0x0720
	;push 13
	;push 1858
	;call drawRectangle
	jmp end_menu_rect
	
	select_for_mutlti:

		;push 0x0720
		;push 14
		;push 1798
		;call drawRectangle
	
		push 0x87FE
		push 13
		push 1858
		call drawRectangle
	end_menu_rect:
	
		push 0x0303
		push 1504
		push chooseText
		call printStr
	
		push 0x0202
		push 1960
		push singleplayerText
		call printStr
	
		push 0x0404
		push 2020
		push multiplayerText
		call printStr
	

	
	mov cx, 0xFFFF
	menu_sleep:
		cmp byte [endFlag], 1
		je end_menu
		loop menu_sleep
		
	
	
	jmp menu_loop
	
	
end_menu:

	cmp byte [modType], 1
	je no_multi_menu
		mov word [boxAddr], 3940
		mov word [prevBoxAddr], 3940
	jmp end_menu_address
	
no_multi_menu:
	mov word [boxAddr], 3920
	mov word [prevBoxAddr], 3920
end_menu_address:
	mov word [endFlag], 0

	push 0
	pop es
	
	
	cli
	mov word [es:9*4], kbisr
	mov [es:9*4+2], cs
	sti
	
	
	pop ds
	pop bx
	pop dx
	pop cx
	pop es
	pop ax
	pop bp
ret


exitScreen:
	push bp
	mov bp, sp
	push ax
	push es
	push cx
	push dx
	push bx
	push ds
	
	call clrscreen
	

	push 0
	pop es
	
	cli
	mov word [es:9*4], menu_kbisr
	mov word [es:9*4+2], cs
	sti
	
	mov word [endFlag], 0
exit_loop:
	call clrscreen
	
	cmp byte [modType], 1
	jne select_for_mutlti_exit
	push 0x87FE
	push 12
	push 1798
	call drawRectangle
	
	;push 0x0720
	;push 13
	;push 1858
	;call drawRectangle
	jmp end_exit_rect
	
	select_for_mutlti_exit:

		;push 0x0720
		;push 14
		;push 1798
		;call drawRectangle
	
		push 0x87FE
		push 6
		push 1858
		call drawRectangle
	end_exit_rect:
	
	cmp byte [win], 0
	jne exit_win
		
		call lost_screen_text
		
		jmp exit_other_text
	exit_win:
		push 0x0303
		push 1504
		push winText
		call printStr
	exit_other_text:
		push 0x0202
		push 1960
		push playAgainText
		call printStr
	
		push 0x0404
		push 2020
		push exitText
		call printStr
	

	
	mov cx, 0xFFFF
	exit_sleep:
		cmp byte [endFlag], 1
		je end_exit
		loop exit_sleep
		
	
	
	jmp exit_loop
	
	
end_exit:

	mov word [endFlag], 0

	push 0
	pop es
	
	
	cli
	mov word [es:9*4], kbisr
	mov [es:9*4+2], cs
	sti
	
	
	pop ds
	pop bx
	pop dx
	pop cx
	pop es
	pop ax
	pop bp
ret

lost_screen_text:

		push 0x0303
		push 1504
		push lostText
		call printStr
		
		push 0x0303
		push 1654
		push scoreText
		call printStr
		
		;push 0707
		;push 1668
		;push word [AlphaCatched]
		;call printNum	
		
		
		mov di, 1690
		push 0xb800
		pop es
		mov ax, 0x0730
		mov [es:di], ax
		add di, 2
		
		mov bx, 10
		xor cx, cx
		mov ax, [AlphaCatched]
	getDigit_lost:
		xor dx, dx
		div bx
		add dl, 30h
		push dx
		inc cx
		cmp ax, 0
		jnz getDigit_lost
	
	mov ax, 0x0707
	
	print_digit_lost:
		pop dx
		mov dh, ah
		mov [es:di], dx
		add di, 2
		loop print_digit_lost
		

ret

saveScreen:
	push bp
	mov bp, sp
	push es
	push ax
	push di
	push cx
	push ds
	
	xor si, si
	mov di, arr
	
	push ds ;ds:si->es:di
	pop es
	
	push word 0xb800
	pop ds
	
	mov cx, 2000
	
	rep movsw

	pop ds
	pop cx
	pop di
	pop ax
	pop es
	pop bp
ret 
	
	
restoreScreen:
	push bp
	mov bp, sp
	push es
	push ax
	push di
	push cx
	push ds
	
	mov cx, 2000
	
	mov si, arr
	xor di, di
	
	push word 0xb800 
	pop es
	
	rep movsw

	pop ds
	pop cx
	pop di
	pop ax
	pop es
	pop bp
ret 


start:
	call saveScreen

	xor ax, ax
	mov es, ax
	
	mov ax, [es:9*4]
	mov [oldtimer], ax
	mov ax, [es:9*4+2]
	mov [oldtimer+2], ax
	
	mov ax, [es:8*4]
	mov [oldtimer], ax
	mov ax, [es:8*4+2]
	mov [oldtimer+8], ax

	call clrscreen
	
	call startScreen
playGameAgain:
	
	mov word [AlphaCatched],  0
	mov word [AlphaDropped],  0
	mov word [timerScreen], 0
	mov word [boxAddr2], 3880
	mov word [prevBoxAddr2], 3880
	
	call menuScreen
	call clrscreen
	
	call init
	
	cli
	mov word [es:8*4], timer
	mov [es:8*4+2], cs
	
	mov ax, 0xFFFF
	out 0x40, al
	mov al, ah
	out 0x40, al
	sti
	
	call game
	
	
	cli
	
	mov bx, [oldtimer]
	
	mov [es:8*4], bx
	mov bx, [oldtimer+2]
	mov  [es:8*4+2], bx
	
	mov bx, [oldisr]
	sti
	
	call exitScreen
	cmp byte [modType], 1
	je playGameAgain
	
	
	call restoreScreen
	cli
	mov [es:9*4], bx
	mov bx, [oldisr+2]
	mov  [es:9*4+2], bx
	sti

mov ax, 0x4c00
int 21h