; Restaurant Billing System

; Restaurant Billing system is a program that allows people to choose 
; from a list of menu items until they
; have everything they've wanted to order and then calculate the total
; bill when they are finished selecting from a list.

Include Irvine32.inc
INCLUDE macros.inc

.386
.model flat,stdcall
.stack 4096
ExitProcess proto,dwExitCode:dword
max = 500
maxb = 50000
sizeb = 500
BUFFER_SIZE = 500000
.data
     bill DWORD 0
	string BYTE '           WELCOME TO RESTAURANT',0dh,0ah,0
     string1 BYTE ' Menu:',0dh,0ah
             BYTE '      Enter 1 - Oriental food ',0dh,0ah
             BYTE '      Enter 2 - Chinese food ',0dh,0ah
             BYTE '      Enter 3 - Fast food ',0dh,0ah
             BYTE '      Enter 4 - Drinks ',0dh,0ah
             BYTE '      Enter 5 - Dessert ',0dh,0ah
             BYTE '      Enter 6 - Exit ',0dh,0ah,0

    string2  BYTE '      Enter 1- To continue',0dh,0ah
             BYTE '      Enter 2 - Exit ',0dh,0ah,0

    string3  BYTE '      Enter 1 - Naan   = Rs 15',0dh,0ah
             BYTE '      Enter 2 - Roti   = Rs 10',0dh,0ah
             BYTE '      Enter 3 - Exit ',0dh,0ah,0

    buffer3 DWORD 15,10

    string4  BYTE '      Enter 1 - Chicken Briyani  = Rs 100 per plate',0dh,0ah
             BYTE '      Enter 2 - Chicken Karahi   = Rs 90 per plate',0dh,0ah
             BYTE '      Enter 3 - Chicken Tikka    = Rs 70 per plate',0dh,0ah
             BYTE '      Enter 4 - Murgh Haleem     = Rs 80 per plate',0dh,0ah
             BYTE '      Enter 5 - Exit ',0dh,0ah,0

    buffer4 DWORD 100,90,70,80
    buffer41 byte "Chicken Briyani",0
    buffer42 byte "Chicken Karahi",0
    buffer43 byte "Chicken Tikka",0
    buffer44 byte "Murgh Haleem",0


    string5  BYTE '      Enter 1 - Egg Fried Rice               = Rs 150 per plate ',0dh,0ah
             BYTE '      Enter 2 - Chicken Manchurian with rice = Rs 165 per plate ',0dh,0ah
             BYTE '      Enter 3 - Chicken Macroni              = Rs 95 per plate ',0dh,0ah
             BYTE '      Enter 4 - Chicken Shahlik              = Rs 80 per plate ',0dh,0ah
             BYTE '      Enter 5 - Exit ',0dh,0ah,0

   buffer5 DWORD 150,165,95,80

    string6  BYTE '      Enter 1 - Zinger Burger    = Rs 100 ',0dh,0ah
             BYTE '      Enter 2 - Chicken Pizza    = Rs 150',0dh,0ah
             BYTE '      Enter 3 - French Fries     = Rs 50',0dh,0ah
             BYTE '      Enter 4 - Chicken Shawarma = Rs 95',0dh,0ah
             BYTE '      Enter 5 - Exit ',0dh,0ah,0

    buffer6 DWORD 100,150,50,95

    string7  BYTE '      Enter 1 - Coca Cola      = Rs 90 (1.5 litre)',0dh,0ah
             BYTE '      Enter 2 - Sprite         = Rs 90 (1.5 litre)',0dh,0ah
             BYTE '      Enter 3 - Mint Margarita = Rs 65',0dh,0ah
             BYTE '      Enter 4 - Pineapple Juice= Rs 70',0dh,0ah
             BYTE '      Enter 5 - Exit ',0dh,0ah,0

    buffer7 DWORD 90,90,65,70

    string8  BYTE '      Enter 1 - Choclate Cake     = Rs 155',0dh,0ah
             BYTE '      Enter 2 - Brownie           = Rs 145',0dh,0ah
             BYTE '      Enter 3 - Cocunut Cream Pie = Rs 75',0dh,0ah
             BYTE '      Enter 4 - Vanilla Ice-cream = Rs 60',0dh,0ah
             BYTE '      Enter 5 - Exit ',0dh,0ah,0

    buffer8 DWORD 155,145,75,60
    
    spaces BYTE '              ',0

    errorMsg BYTE '      Please follow instructions correctly ',0dh,0ah,0

    Quantity BYTE '      Quantity:     ',0

    billing BYTE  '      Total Bill:   Rs ',0    

    buffer BYTE sizeb dup(0)
    bufSize DWORD ($-buffer)
    errMsg BYTE "Cannot open file",0dh,0ah,0
    filename     BYTE  100 DUP(0)
    fileHandle   HANDLE ?       ; handle to output file
    bytesWritten DWORD ?    			; number of bytes written
    space byte ",",0
    next byte "          ",0
    next1 byte " ",0
    total dword 1024
    strOutput BYTE 100 dup(0)
    result BYTE 100 dup(0)
    nextline byte 0dh,0ah
    
    names BYTE 30 DUP(0)

    nameString BYTE "Please Enter Your Name: ",0

    phone BYTE 30 DUP(0)

    subQuantity DWORD 0
    
    subTotal DWORD 0

    itemName Byte 100 DUP(0)

    billText BYTE 500 DUP(0)

    nill BYTE 500 DUP(0)

    customerIDS BYTE '      Do You Have a Customer Id?',0dh,0ah
                BYTE '      Enter 1- YES',0dh,0ah
                BYTE '      Enter 2- NO',0dh,0ah,0

    billFile byte "newfile.txt",0

    customerIDF byte "customerid.txt",0

    counter byte "counter.txt",0

    bufferRead BYTE BUFFER_SIZE DUP(?)
.code
main proc
     call Crlf
     call Crlf
	 mov edx,OFFSET string
     call WriteString
     L1:                         
       mov edx,OFFSET string1
       call WriteString
       mov edx,OFFSET spaces
       call WriteString
       call ReadDec
       call Checkerror    ; check whether user enter the number in given range

       cmp eax,1      ; comparison b/w what user enter with each item of list
       je L2
       cmp eax,2
       je L3
       cmp eax,3
       je L4
       cmp eax,4
       je L5
       cmp eax,5
       je L6
       jmp last

     L2: call Oriental       ; calling procedures depends on what user enters
         jmp L7
     L3: call Chinese
         jmp L7
     L4: call FastFood
         jmp L7
     L5: call Drinks
         jmp L7
     L6: call Dessert
         jmp L7
     L7: mov edx,offset buffer
         call strlength
         mov esi,eax
         mov edx,offset billText
         call strlength
         mov ecx,eax
         mov edx,offset billText
         call cont2
         mov ecx,lengthof billText
         mov esi,0
         aa1:
            mov al,0
            mov billText[esi],al
            inc esi
            loop aa1
         mov edx,OFFSET string2
         call WriteString
         mov edx,OFFSET spaces
         call WriteString
         call ReadDec
         call Checkerror1
         cmp eax,1           ; if user want to continue then jump to L1
         jmp L1
     last:
         INVOKE Str_copy,
            ADDR billFile,
            ADDR filename
         call write
         mov edx,offset buffer
         call writestring
         mov edx,offset bufferRead
         call writestring
         call Crlf
         call Crlf
         mov edx,OFFSET billing
         call WriteString
         mov eax,bill        
         call WriteDec       ; prints the bill
         call Crlf   ; next line
         call Crlf
         call WaitMsg
         
	invoke ExitProcess,0
main endp

name1 PROC
    
    mov edx,offset nameString
    call writestring
    mov edx,offset names
    mov ecx,30
    call readstring

    ret

name1 ENDP

customer PROC

    mov edx,offset customerIDS
    call writestring
    call ReadDec
    call Checkerror1
    cmp eax,1
    jne c2
    c2:
        mov edx,offset names
        call strlength
        mov esi,eax
        
    ret
customer ENDP

Oriental PROC

; print the oriental menu and add prices into bill according to which item of what quantity user selects 
; and call another func(NaanRoti) according to requirment
; Receives: string4, buffer4
; Returns: return updated bill
;-----------------------------------------------
         
         mov edx,OFFSET string4
         call WriteString
         mov edx,OFFSET spaces
         call WriteString
         call ReadDec
         call Crlf
         call Checkerror3
         cmp eax,1
         je L1
         cmp eax,2
         je L2
         cmp eax,3
         je L3
         cmp eax,4
         je L4
         cmp eax,5
         jmp last
 L1: 
    mov subQuantity,0
    mov subTotal,0
    INVOKE Str_copy,
            ADDR buffer41,
            ADDR itemName
     mov edx,OFFSET Quantity
     call WriteString
     call ReadDec
     call Crlf
     mov subQuantity,eax
     mov ecx,eax
     mov ebx,[buffer4]; buffer4 is array contains price of oriental foods
     L11:                   ; quantity times a loop L11 runs           
        add subTotal,ebx
        add bill,ebx        ; add price into bill
        loop L11
     jmp last
 L2: mov subQuantity,0
     mov subTotal,0
     INVOKE Str_copy,
            ADDR buffer42,
            ADDR itemName
     mov edx,OFFSET Quantity
     call WriteString
     call ReadDec
     call Crlf
     mov subQuantity,eax
     mov ecx,eax
     mov ebx,[buffer4 + 4]
     L22:
         add subTotal,ebx
         add bill,ebx
         loop L22
     call NaanRoti
     jmp last
 L3: mov subQuantity,0
     mov subTotal,0
     INVOKE Str_copy,
            ADDR buffer43,
            ADDR itemName
     mov edx,OFFSET Quantity
     call WriteString
     call ReadDec
     call Crlf
     mov subQuantity,eax
     mov ecx,eax
     mov ebx,[buffer4 + 8]
     L33:
         add subTotal,ebx
         add bill,ebx
         loop L33
     call NaanRoti
     jmp last
 L4: mov subQuantity,0
     mov subTotal,0
     INVOKE Str_copy,
            ADDR buffer44,
            ADDR itemName
     mov edx,OFFSET Quantity
     call WriteString
     call ReadDec
     call Crlf
     mov subQuantity,eax
     mov ecx,eax
     mov ebx,[buffer4 + 12]
     L44:
         add subTotal,ebx
         add bill,ebx
         loop L44
     call NaanRoti
last:
    INVOKE Str_copy,
            ADDR itemName,
            ADDR billText
    mov edx,offset itemName
    call strlength
    mov ecx,25
    mov esi,eax
    sub ecx,eax
    ab1:
        mov al,next1
        mov billText[esi],al
        inc esi
        loop ab1
    mov eax,subQuantity
    call numb
    mov edx,offset billText
    call strlength
    mov esi,eax
    mov edx,offset result
    call strlength
    mov ecx,eax
    mov edx,offset result
    call cont
    mov edx,offset billText
    call strlength
    mov esi,eax
    mov edx,offset next
    call strlength
    mov ecx,eax
    mov edx,offset next
    call cont
    mov eax,subTotal
    call numb
    mov edx,offset billText
    call strlength
    mov esi,eax
    mov edx,offset result
    call strlength
    mov ecx,eax
    mov edx,offset result
    call cont
    mov edx,offset billText
    call strlength
    mov esi,eax
    mov edx,offset nextline
    call strlength
    mov ecx,eax
    mov edx,offset nextline
    call cont
    ret
Oriental ENDP

NaanRoti PROC


; print the menu(naan,roti) and add prices into bill according to which item of what quantity user selects 
; Receives: string3, buffer3
; Returns: return updated bill
;-----------------------------------------------
 
        mov edx,OFFSET string3
        call WriteString
        mov edx,OFFSET spaces
        call WriteString
        call ReadDec
        call Checkerror2
        cmp eax,1
        je L1
        cmp eax,2
        je L2
        jmp last
     L1:
        mov ebx,[buffer3]    ; buuffer3 is array contains price of Naan and roti
        mov edx,OFFSET Quantity
        call WriteString
        call ReadDec
        call Crlf
        mov ecx,eax
        L11:
          add bill,ebx
          loop L11
       jmp last
     L2:
        mov ebx,[buffer3 + 4]
        mov edx,OFFSET Quantity
        call WriteString
        call ReadDec
        call Crlf
        mov ecx,eax
        L22:
          add bill,ebx
          loop L22
last:
ret
NaanRoti ENDP


Chinese PROC


; print the chinese menu and add prices into bill according to which item of what quantity user selects 
; Receives: string5, buffer5
; Returns: return updated bill
;-----------------------------------------------
         mov edx,OFFSET string5
         call WriteString
         mov edx,OFFSET spaces
         call WriteString
         call ReadDec
         call Crlf
         call Checkerror3       ; check for error
         cmp eax,1
         je L1
         cmp eax,2
         je L2
         cmp eax,3
         je L3
         cmp eax,4
         je L4
         cmp eax,5
         jmp last

 ; price of 1st,2nd,.. item of Chinese menu is on 1st,2nd.. index of buffer5 respectively
 ; same for all other menus

 L1:
     mov edx,OFFSET Quantity
     call WriteString
     call ReadDec
     mov ecx,eax
     mov ebx,[buffer5]   
     L11:
        add bill,ebx
        loop L11
     jmp last
 L2: mov edx,OFFSET Quantity
     call WriteString
     call ReadDec
     mov ecx,eax
     mov ebx,[buffer5 + 4]
     L22:
         add bill,ebx
         loop L22
     jmp last
 L3: mov edx,OFFSET Quantity
     call WriteString
     call ReadDec
     call Crlf
     mov ecx,eax
     mov ebx,[buffer5 + 8]
     L33:
         add bill,ebx
         loop L33
     jmp last
 L4: mov edx,OFFSET Quantity
     call WriteString
     call ReadDec
     call Crlf
     mov ecx,eax
     mov ebx,[buffer5 + 12]
     L44:
         add bill,ebx
         loop L44
last:

ret
Chinese ENDP
         
FastFood PROC


; print the fastfood menu and add prices into bill according to which item of what quantity user selects 
; Receives: string6, buffer6
; Returns: return updated bill
;-----------------------------------------------
         mov edx,OFFSET string6
         call WriteString
         mov edx,OFFSET spaces
         call WriteString
         call ReadDec
         call Crlf
         call Checkerror3
         cmp eax,1
         je L1
         cmp eax,2
         je L2
         cmp eax,3
         je L3
         cmp eax,4
         je L4
         cmp eax,5
         jmp last
 L1: mov edx,OFFSET Quantity
     call WriteString
     call ReadDec
     call Crlf
     mov ecx,eax
     mov ebx,[buffer6]
     L11:
        add bill,ebx
        loop L11
     jmp last
 L2: mov edx,OFFSET Quantity
     call WriteString
     call ReadDec
     call Crlf
     mov ecx,eax
     mov ebx,[buffer6 + 4]
     L22:
         add bill,ebx
         loop L22
     jmp last
 L3: mov edx,OFFSET Quantity
     call WriteString
     call ReadDec
     call Crlf
     mov ecx,eax
     mov ebx,[buffer6 + 8]
     L33:
         add bill,ebx
         loop L33
     jmp last
 L4: mov edx,OFFSET Quantity
     call WriteString
     call ReadDec
     call Crlf
     mov ecx,eax
     mov ebx,[buffer6 + 12]
     L44:
         add bill,ebx
         loop L44
last:
ret
FastFood ENDP
         
Drinks PROC


; print the drinks menu and add prices into bill according to which item of what quantity user selects 
; Receives: string7, buffer7
; Returns: return updated bill
;-----------------------------------------------
         mov edx,OFFSET string7
         call WriteString
         mov edx,OFFSET spaces
         call WriteString
         call ReadDec
         call Crlf
         call Checkerror3
         cmp eax,1
         je L1
         cmp eax,2
         je L2
         cmp eax,3
         je L3
         cmp eax,4
         je L4
         cmp eax,5
         jmp last
 L1: mov edx,OFFSET Quantity
     call WriteString
     call ReadDec
     call Crlf
     mov ecx,eax
     mov ebx,[buffer7]
     L11:
        add bill,ebx
        loop L11
     jmp last
 L2: mov edx,OFFSET Quantity
     call WriteString
     call ReadDec
     call Crlf
     mov ecx,eax
     mov ebx,[buffer7 + 4]
     L22:
         add bill,ebx
         loop L22
     jmp last
 L3: mov edx,OFFSET Quantity
     call WriteString
     call ReadDec
     call Crlf
     mov ecx,eax
     mov ebx,[buffer7 + 8]
     L33:
         add bill,ebx
         loop L33
     jmp last
 L4: mov edx,OFFSET Quantity
     call WriteString
     call ReadDec
     call Crlf
     mov ecx,eax
     mov ebx,[buffer7 + 12]
     L44:
         add bill,ebx
         loop L44
last:
ret
Drinks ENDP
         
Dessert PROC


; print the dessert menu and add prices into bill according to which item of what quantity user selects 
; Receives: string8, buffer8
; Returns: return updated bill
;-----------------------------------------------
         mov edx,OFFSET string8
         call WriteString
         mov edx,OFFSET spaces
         call WriteString
         call ReadDec
         call Crlf
         call Checkerror3
         cmp eax,1
         je L1
         cmp eax,2
         je L2
         cmp eax,3
         je L3
         cmp eax,4
         je L4
         cmp eax,5
         jmp last
 L1: mov edx,OFFSET Quantity
     call WriteString
     call ReadDec
     call Crlf
     mov ecx,eax
     mov ebx,[buffer8]
     L11:
        add bill,ebx
        loop L11
     jmp last
 L2: mov edx,OFFSET Quantity
     call WriteString
     call ReadDec
     call Crlf
     mov ecx,eax
     mov ebx,[buffer8 + 4]
     L22:
         add bill,ebx
         loop L22
     jmp last
 L3: mov edx,OFFSET Quantity
     call WriteString
     call ReadDec
     mov ecx,eax
     call Crlf
     mov ebx,[buffer8 + 8]
     L33:
         add bill,ebx
         loop L33
     jmp last
 L4: mov edx,OFFSET Quantity
     call WriteString
     call ReadDec
     call Crlf
     mov ecx,eax
     mov ebx,[buffer8 + 12]
     L44:
         add bill,ebx
         loop L44

last:
ret
Dessert ENDP

Checkerror PROC

; check whether eax contains value in range 1-6
; Receives: eax
; Returns: eax contains value in range 1-6 according to user's choice
;-----------------------------------------------
L1:
   cmp eax,1
   jl L2
   cmp eax,6
   jg L2
   jmp last

L2:
   mov edx, OFFSET errorMsg
   call WriteString
   call ReadDec
   jmp L1
last:
ret
Checkerror ENDP

Checkerror1 PROC

; check whether eax contains value either 1 or 2
; Receives: eax
; Returns: eax contains value either 1 or 2 according to user's choice
;-----------------------------------------------
L1:
   cmp eax,1
   jl L2
   cmp eax,2
   jg L2
   jmp last

L2:
   mov edx, OFFSET errorMsg
   call WriteString
   call ReadDec
   jmp L1
last:
ret
Checkerror1 ENDP

Checkerror2 PROC

; check whether eax contains value in range 1-3
; Receives: eax
; Returns: eax contains value in range 1-3 according to user's choice
;-----------------------------------------------
L1:
   cmp eax,1
   jl L2
   cmp eax,3
   jg L2
   jmp last

L2:
   mov edx, OFFSET errorMsg
   call WriteString
   call ReadDec
   jmp L1
last:
ret
Checkerror2 ENDP

Checkerror3 PROC

; check whether eax contains value in range 1-5
; Receives: eax
; Returns: eax contains value in range 1-6 according to user's choice
;-----------------------------------------------
L1:
   cmp eax,1
   jl L2
   cmp eax,5
   jg L2
   jmp last

L2:
   mov edx, OFFSET errorMsg
   call WriteString
   call ReadDec
   jmp L1
last:
ret
Checkerror3 ENDP

numb PROC
    INVOKE Str_copy,
            ADDR nill,
            ADDR strOutput
    INVOKE Str_copy,
            ADDR nill,
            ADDR result
	mov edx,0
	mov ebx,10
	mov esi,0
	a1:
		div ebx
		add dl,"0"
		mov strOutput[esi],dl
		mov dl,0
		inc esi
		cmp eax,0
		jne a1
	mov ecx,esi
	dec esi
	mov ebx,esi
	mov esi,0
	a2:
		mov al,strOutput[ebx]
		mov result[esi],al
		inc esi
		dec ebx
		loop a2
	mov ecx,100
	sub ecx,esi
	a3:
		mov al,0
		mov result[esi],al
		inc esi
		loop a3
ret
numb ENDP
cont PROC
	mov ebx,0
	L1:
		mov al,[edx+ebx]
		mov billText[esi],al
		inc esi
		inc ebx
		loop L1
	ret
cont ENDP
cont2 PROC
	mov ebx,0
	L1:
		mov al,[edx+ebx]
		mov buffer[esi],al
		inc esi
		inc ebx
		loop L1
	ret
cont2 ENDP
copy PROC
	mov esi,0
	l1:
	mov al,[edx+esi]
	mov buffer[esi],al
	inc esi
	loop l1
	mov eax,500
	sub eax,esi
	mov ecx,eax
	l2:
	mov al,0
	mov buffer[esi],al
	inc esi
	loop l2
	ret
copy ENDP
write PROC
INVOKE CreateFile,
	  ADDR filename, GENERIC_WRITE, DO_NOT_SHARE, NULL,
	  OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0

	mov fileHandle,eax			; save file handle
	.IF eax == INVALID_HANDLE_VALUE
	  mov  edx,OFFSET errMsg		; Display error message
	  call WriteString
	  jmp _exit
	.ENDIF

	; Move the file pointer to the end of the file
	INVOKE SetFilePointer,
	  fileHandle,0,0,FILE_END

	; Append text to the file
	INVOKE WriteFile,
	    fileHandle, ADDR buffer, bufSize,
	    ADDR bytesWritten, 0
INVOKE CloseHandle, fileHandle
_exit:
ret
write ENDP
read PROC
; Open the file for input.
	mov	edx,OFFSET filename
	call	OpenInputFile
	mov	filehandle,eax

; Check for errors.
	cmp	eax,INVALID_HANDLE_VALUE		; error opening file?
	jne	file_ok					; no: skip
	mWrite <"Cannot open file",0dh,0ah>
	jmp	quit						; and quit
file_ok:

; Read the file into a buffer.
	mov	edx,OFFSET bufferRead
	mov	ecx,BUFFER_SIZE
	call	ReadFromFile
	jnc	check_buffer_size			; error reading?
	mWrite "Error reading file. "		; yes: show error message
	call	WriteWindowsMsg
	jmp	close_file
	
check_buffer_size:
	cmp	eax,BUFFER_SIZE			; buffer large enough?
	jb	buf_size_ok				; yes
	mWrite <"Error: Buffer too small for the file",0dh,0ah>
	jmp	quit						; and quit
	
buf_size_ok:	
	mov	bufferRead[eax],0		; insert null terminator

; Display the buffer.
	mWrite <"Buffer:",0dh,0ah,0dh,0ah>
	mov	edx,OFFSET bufferRead	; display the buffer
	call	WriteString
	call	Crlf
	call crlf

close_file:
	mov	eax,filehandle
	call	CloseFile


quit:
	ret
read ENDP

end main