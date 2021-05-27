;----Macros----

%macro pushRegisters 0
    pushad
    pushfd
%endmacro

%macro popRegisters 0
    popfd
    popad
%endmacro

%macro printCalcMsg 0
    push calc_msg
    call print_string
%endmacro

%macro readInput 0
    push dword [stdin]
    push 80
    push dword buffer
    call fgets
    add esp, 12
%endmacro

%macro popMyStack 0
    mov ebx, [stackIndex]
    mov eax, [stack]
    sub ebx, 1 ;EBX = stackIndex-1
    shl ebx, 2 ;EBX = stackIndex * 4
    add eax, ebx ; stack[stackIndex]
    dec dword[stackIndex]
%endmacro


;----Debug Mode Macros-----;

%macro debugAddNumber 0
    pushRegisters
    mov eax, [debugModeOn]
    cmp eax, 0
    je %%exitDebug
    push buffer
    push insert_num_debug
    call printf
    add esp, 8
    %%exitDebug:
        popRegisters
%endmacro



section	.rodata			; we define (global) read-only variables in .rodata section
	format_string_enter: db "%s",10, 0	; format string
    format_string: db "%s", 0	; format string
    format_oct_enter: db "%o",10, 0	; format string
    insert_num_debug: db 'Number read from user: %s', 10
    overflowError: db 'Error: Operand Stack Overflow', 10, 0
    lackOfArgumentsError: db 'Error: Need at least two arguments on stack', 10, 0
    calc_msg:  db  'calc: ', 0 ;our dear string


section .data
    zero: dd 0
    debugModeOn: dd 0
    stackSize: dd 5
    stackIndex: dd 0
    operations: dd 0
    numOfDigits: dd 0
    oddNumOfDigits: dd 0
    bufferIndex: dd 0
    num: dd 0
    startOfNodeList: dd 0
    currentNode: dd 0
    nextNode: dd 0
    listNum1: dd 0
    listNum2: dd 0
    carry: dd 0
    doneWithNum1: dd 0
    doneWithNum2: dd 0
    firstNodeCreated: dd 0
    stringToPrint: dd 0
    

section .bss			; we define (global) uninitialized variables in .bss section
	buffer: resb 80		; enough to store 80 chars
    stack: resd 1       ; Hold stack pointer



section .text
  align 16
  global main
  extern printf
  extern fprintf 
  extern fflush
  extern malloc 
  extern calloc 
  extern free 
  extern gets 
  extern getchar 
  extern fgets 
  extern stdout
  extern stdin
  extern stderr

; -------- DEBUGGING METHODS ---------------------------------------------------- ;

print_string:
    pop ebx ; ret value
    pop eax ; string to print
    push ebx
    push eax
    push format_string_enter
    call printf
    add esp, 8
    ret

print_int:
    pop ebx ; ret value
    pop eax ; int to print
    push ebx
    push eax
    push format_oct_enter
    call printf
    add esp, 8
    ret

; dec_to_ecta:
;     mov eax, [esp+4] ; hold in eax the first argument
;     mov ecx, 1

;     .loop:
;         inc ecx     ; ecx count log 8
;         shr eax, 3
;         cmp eax, 0      
;         jnz .loop

;     push ecx
;     call malloc ; get buffer of number of digits
;     mov ecx, [esp]
;     sub ecx, 1
;     add esp, 4
;     push eax    ; push pointer to malloc*

;     .loop2:
;         mov eax, [esp+12]
;         sub ecx, 1
;         cmp ecx, 0
;         jne .loop2

;     call free
;     add esp, 4
;     ret

; stoi:
;     pop ecx ; ecx holds the return value

;      mov ebx, 0 ; initiate ebx (which will hold the sum)
;      pop edx ; hold in edx the first argument which is (char*)

;     .loop:
;         shl ebx, 3
;         mov eax, [edx]  ; we get the value of the char in 4 bytes (eax is 4 bytes)
;         sub eax, '0'   ; we delete the ASCII value of 0
;         and eax, 0xFF ; we remove all the irrelevant bytes (first 3 bytes)
;         add ebx, eax    ; add the current digit to the sum
;         inc edx         ; go to the next digit
        
;         mov al, [edx]   ; check if the current value is EOF
;         cmp al, 20      
;         jg .loop

;     push ebx
;     push ecx
;     ret

    debugPrint:     
        pushad
  
        ;mov ebx, [currentNode]
        ;mov eax, [ebx]
    
        ; mov eax, [numOfDigits]
        ;mov eax, [num]
        ;mov eax, dword[eax]
       
        push eax	; call printf with 2 arguments - 
        call print_int  
        popad
        ret
          
        

    debugPrint2:
        pushad
       movzx eax, byte[eax+2]
        ;and ebx, 63
        push eax	; call printf with 2 arguments - 
        call print_int   
        popad
        ret 

; ---------------------------------------------------------------------------------;




main:
    mov eax, [esp+4] ; esp+4 holds argc as int
    mov ebx, dword [esp+8] ; argv
    cmp eax, 1        ; compare argc<=1
    jbe start
    mov esi, 1
    argsLoop:
    cmp esi,eax
    je start
    mov ecx, dword[ebx+4*esi] ; get argv[1] will be '-d' or size of buffer
    cmp word[ecx], '-d' ; check for debug mode 
    jne getStackSize

    debugMode:
        mov edx , 1
        mov [debugModeOn], edx ;Turn on debug mode
        inc esi         
        jmp argsLoop
    getStackSize:
        movzx edx, byte [ecx] ; Move first number to edx
        sub edx, 48         ; Remove 48 to get "int" value
        mov [stackSize], edx; Add first number to stackSize
        inc ecx             ; Increment ecx and get the next value of number (if it exists)
        movzx edx, byte [ecx] ; Move next value of number to edx
        cmp edx, 0q0        ; Compare 2nd number to 0
        je oneNumber
        sub edx, 48
        mov ebx, [stackSize]; Move first number to ebx
        shl ebx, 3  ; Multiply stack size by 8
        add ebx, edx; Add numbers together
        mov [stackSize], ebx
        inc esi
        jmp argsLoop
    
        oneNumber:
            inc esi
            jmp argsLoop

    ; mov eax, [esp+8] ; esp+8 holds argv**
    ; mov edx, [eax +4] ; [eax+4] is the location of argv[1]
    ; push edx
    ; call stoi       ; convert edx string into number
    ; call print_int  ; show the number in base 8 

    start:
        pushRegisters ;pushad & pushfd macro
        mov eax, 4    ;Size of pointer
        push eax
        mov eax, dword[stackSize] ;Size of stack
        push eax
        call calloc  ;Allocate memory using calloc
        add esp, 8   ;Ignore double push
        mov [stack], eax
        popRegisters
        jmp calculate
    
    calculate:
        pushRegisters
        printCalcMsg ; Prints calculator input message
        popRegisters
        readInput
        checkOperation:
            cmp byte[buffer], 'q'
            je quitProgram
            cmp byte[buffer], '+'
            je addition
            cmp byte[buffer], '&'
            je bitwiseAND
            cmp byte[buffer], 'p'
            je popAndPrint
            cmp byte[buffer], 'd'
            je Duplicate
            cmp byte[buffer], 'n'
            je countNumOfBytes
            jmp addNumberToStack


    quitProgram:
        mov eax, [stackIndex]
        cmp eax, 0
        je endOfQuit
        popMyStack
        mov eax, [eax]
        call freeNumberList
        jmp quitProgram

        endOfQuit:
        mov  eax, [stack]
        push eax
        call free
        add esp, 4

        mov eax, dword[operations]
        push eax
        call print_int
        mov     eax, 1
        mov     ebx, 0
        int     0x80
        

    addNumberToStack:
        debugAddNumber
        mov eax, [stackIndex]
        mov ebx, [stackSize]
        cmp ebx, eax ; make sure that stackIndex <= stackSize
        jne continueAddNum ;If it isn't, print overflow error and restart calc loop
        push overflowError
        call print_string
        jmp calculate
        continueAddNum: mov ecx, buffer
        mov edx, 0
        mov [numOfDigits], edx
        mov [bufferIndex], edx
            getNumOfDigits:
                movzx eax, byte[ecx] ;Move current char from buffer into eax
                cmp eax, 10 ; Check if we finished going over number
                je createNumber
                inc dword[numOfDigits] ; numOfDigits ++
                inc ecx ; Go to next char in buffer
                jmp getNumOfDigits

            createNumber:
                mov eax, [numOfDigits]
                and eax, 1
                cmp eax, 0 ;Check if numOfDigits is even or odd
                je handleNumberInput
                mov eax, 1
                mov [oddNumOfDigits], eax ; Flag that tells us if the current numOfDigits is odd
                handleNumberInput:
                    mov edx, [bufferIndex] 
                    mov eax, buffer
                    add eax, edx ;Jumps to index EDX of buffer
                    movzx edx, byte[eax] ; buffer[bufferIndex]
                    cmp edx, 10 ; Check if we recieved end of line
                    je calculate
                    inc dword[bufferIndex]; bufferIndex++
                    sub edx, 48 ;Recieve int value of char
                    createFirstDigit: ; EDX & EAX are occupied
                        mov ebx, [oddNumOfDigits]
                        cmp ebx, 1
                        je handleOddNumber
                        shl edx, 3 ;Multiply number by 8
                        mov [num], edx 
                        mov eax, buffer
                        mov edx, [bufferIndex]
                        add eax, edx
                        movzx edx, byte [eax] ; buffer[bufferIndex]
                        sub edx, 48 ;Recieve int value of 2nd char 
                        mov eax, [num]
                        add eax, edx
                        mov [num], eax
                        inc dword[bufferIndex]
                        jmp createFirstNode

                    handleOddNumber:
                        mov [num], edx
                        jmp createFirstNode ; 

                    createFirstNode:
                        mov ebx, 5 ;Size of memory block
                        push ebx
                        call malloc ;Allocate 5 bytes of memory
                        add esp, 4
                        mov [currentNode], eax ;Return value is always in eax
                        mov ebx, [num] ; 2 digits of current number to be added to node
                        mov [eax], ebx ; We will put num in the first byte of EAX
                        mov dword[eax+4], 0 ; Next node is currently NULL
                        jmp getRestOfDigits

                    getRestOfDigits:
                        mov ecx, [bufferIndex]
                        mov edx, buffer
                        add edx, ecx ; pointer to buffer[bufferIndex]
                        movzx ecx, byte[edx] ;Mov byte value of buffer[bufferIndex] to ecx
                        cmp ecx, 10
                        je pushToStack
                        inc dword[bufferIndex]
                        sub ecx, 48
                        jmp createSecondDigit
                    
                    createSecondDigit: ;ECX contains buffer[bufferIndex]
                        shl ecx, 3
                        mov ebx, [bufferIndex]
                        mov eax, buffer
                        add eax, ebx ; Go to buffer[bufferIndex]
                        movzx ebx, byte[eax] ;Get byte value of buffer[bufferIndex]
                        sub ebx, 48
                        inc dword [bufferIndex]
                        add ecx, ebx
                        mov [num], ecx
                        jmp handleRestOfNodes

                    handleRestOfNodes: ;All registers are free to use
                        mov eax, 5
                        push eax
                        call malloc
                        add esp, 4
                        mov ebx, [currentNode]
                        mov ecx, [num]
                        mov [eax], ecx
                        mov dword[eax+4], ebx ;The new node will point to currentNode
                        mov [currentNode], eax ;Point to newly created node
                        jmp getRestOfDigits

                    pushToStack: ; All registers are free to use
                        mov ebx, 4 ;This may be wrong
                        mov eax, [stackIndex]
                        mul ebx ;Multiplies 4*stackIndex, stores result in eax
                        mov edx, [stack]
                        add edx, eax ; Point to stack[stackIndex]
                        mov eax, [currentNode]
                        mov [edx], eax ;stack[stackIndex] points to currentNode
                        inc dword[stackIndex]
                        mov byte[oddNumOfDigits], 0
                        jmp calculate




        

                        
                        




;---------OPERATIONS----------------------------------------------------------------;
                      


;------------------- Addition ----------------------------------;   
    addition:
        
        mov eax, [stackIndex]
        cmp eax, 2
        jge startAddition
        push lackOfArgumentsError
        call printf
        add esp, 4
        jmp calculate

        startAddition:
            inc dword[operations]
            popMyStack ;EAX = pointer to "popped number", can't save on EBX
            mov [listNum1], eax ;Pointer to popped number
            mov ecx, [eax] ;First digits of popped number
            popMyStack
            mov [listNum2], eax ;2nd popped number
            mov edx, [eax] ;First digits of 2nd popped number
            jmp addTwoNumbers

        additionConditions:
            mov eax, dword[ecx] 
            mov ebx, dword[edx]
            or eax, ebx ;Check if we finished summing up everything
            cmp eax, 0
            je finishAddingNumbers
            cmp dword[ecx], 0
            je finishNum1
            jmp continueAdditionConditions
            finishNum1: ;If we are here, we need to only sum num2
            mov byte[doneWithNum1], 1 
            jmp addOneNumber

            continueAdditionConditions:
            cmp dword[edx], 0
            je finishNum2
            mov eax, dword[ecx]
            mov ecx, eax
            mov ebx, dword[edx]
            mov edx, ebx
            jmp addTwoNumbers

            finishNum2: ;If we are here, we need to only sum num1
            mov byte[doneWithNum2], 1
            jmp addOneNumber

        addOneNumber:
            ;Check if num1 = 0, if yes, eax = 0, else eax = [ecx]

            cmp byte[doneWithNum1], 1
            je addOnlyEDX
            mov ecx, dword[ecx]
            mov eax, [ecx]      ; eax holds ecx.data
            add eax, [carry]
            add ecx, 4 ;Point to next link
            jmp finishAddition
            addOnlyEDX:
            mov edx, dword[edx] 
            mov eax, [edx]      ; eax holds ebx.data
            add eax, [carry]
            add edx, 4 ;Point to next link
            finishAddition:
            movzx esi, al ;Move last 8 bits of eax to esi
            cmp esi, 63
            jg handleCarry2
            jmp handleNoCarry2
            handleCarry2:
            mov byte[carry], 1
            jmp continueAddition4

            handleNoCarry2:
            mov byte[carry], 0
            jmp continueAddition4

            continueAddition4:

            and esi, 63 ;Will leave us with two numbers, carry is handled
            mov [num], esi ;EDX[i] will be the result
            jmp createNewNode




        addTwoNumbers: ;ECX contains pointer to num1, EDX contains pointer to num2
            mov eax, [ecx]
            mov ebx, [edx]
            movzx esi, byte[carry]
            cmp esi, 1
            je storeCarryFlag
            clc
            jmp continueAddition
            storeCarryFlag: stc
            continueAddition: 
            adc eax,ebx
            movzx esi, al ;Move last 8 bits of ecx to esi
            cmp esi, 63
            jg handleCarry
            jmp handleNoCarry
            handleCarry:
            mov byte[carry], 1
            jmp continueAddition2

            handleNoCarry:
            mov byte[carry], 0
            jmp continueAddition2

            continueAddition2:
            and esi, 63 ;Will leave us with two numbers, carry is handled
            mov [num], esi
            ;mov [edx], esi ;EDX[i] will be the result
            add ecx, 4 ;Point to next link
            add edx, 4 ;Point to next link
            jmp createNewNode

        createNewNode: ;num to add is in edx
            push ecx
            push edx
            mov eax, 5
            push eax
            call malloc
            add esp, 4
            pop edx
            pop ecx
            mov ebx, [firstNodeCreated] ;Checks if the first link was created
            cmp ebx, 1
            je createAnotherLink
            mov byte[firstNodeCreated], 1
            mov [currentNode], eax
            mov ebx, [num]
            mov [eax], ebx
            mov dword[eax+4], 0 ;New node currenly points to nothing
            mov ebx, [currentNode]
            mov [startOfNodeList], ebx
            jmp additionConditions
        
        createAnotherLink:
            mov [nextNode], eax
            mov ebx, [num]
            mov [eax], ebx
            mov dword[eax+4], 0
            mov eax, [currentNode]
            mov ebx, [nextNode]
            mov dword[eax+4], ebx
            mov [currentNode], ebx
            jmp additionConditions




        finishAddingNumbers:
            ;Reset variables
            mov byte[doneWithNum1], 0
            mov byte[doneWithNum2], 0
            mov byte[firstNodeCreated], 0

            movzx esi, byte[carry]
            mov byte[carry], 0

            cmp esi, 0
            je dontAddNewLink
            mov eax, 5
            push eax
            call malloc
            add esp, 4
            mov [nextNode], eax
            mov ebx, [num]
            mov [eax], ebx
            mov dword[eax+4], 0
            mov eax, [currentNode]
            mov ebx, dword[nextNode]
            mov [eax+4], ebx
            mov ebx, [nextNode]
            mov [currentNode], ebx
            ; Check if last node is bigger than 77, and split it if needed
            mov ebx, [num]
            cmp ebx, 0
            jne dontAddNewLink   ; If it is less or equal 77, finish
            and ebx, 63     ; keep in ebx only last 2 digits
            mov eax, [currentNode]
            mov [eax], ebx   ; now currentNode.data holds only 2 digits
            mov eax, 5
            push eax
            call malloc
            add esp, 4
            mov [nextNode], eax
            mov byte[eax], 1
            mov dword[eax+4], 0
            mov ecx, [currentNode]
            mov [ecx+4], eax
            mov [currentNode], eax
            
            dontAddNewLink:
            mov ebx, [startOfNodeList]
            mov [currentNode], ebx
            mov eax, dword[listNum1]
            mov eax, dword[eax]
            call freeNumberList
            mov eax, dword[listNum2]
            mov eax, dword[eax]
            call freeNumberList
            jmp pushToStack
            

; --------------------- AND --------------------------------;
    bitwiseAND:
        
        mov eax, [stackIndex]
        cmp eax, 2
        jge startBitwiseAND
        push lackOfArgumentsError
        call printf
        add esp, 4
        jmp calculate

        startBitwiseAND:
            inc dword[operations]
            popMyStack ;EAX = pointer to "popped number", can't save on EBX
            mov [listNum1], eax ;Pointer to popped number
            mov ecx, [eax] ;First digits of popped number
            popMyStack
            mov [listNum2], eax ;2nd popped number
            mov edx, [eax] ;First digits of 2nd popped number
            jmp andTwoNumbers    

        andConditions:
            mov eax, [ecx]
            cmp eax, 0
            je finishAND
            mov eax, [edx]
            cmp eax, 0
            je finishAND
            mov eax, dword[ecx]
            mov ecx, eax
            mov ebx, dword[edx]
            mov edx, ebx
            jmp andTwoNumbers
             
        andTwoNumbers:
            mov eax, [ecx]
            mov ebx, [edx]
            and eax, ebx ;EAX will hold the result
            mov [num], eax
            add ecx, 4
            add edx, 4
            jmp createNewADDNode

        createNewADDNode: ;num to add is in edx
            push ecx
            push edx
            mov eax, 5
            push eax
            call malloc
            add esp, 4
            pop edx
            pop ecx
            mov ebx, [firstNodeCreated] ;Checks if the first link was created
            cmp ebx, 1
            je createAnotherADDLink
            mov byte[firstNodeCreated], 1
            mov [currentNode], eax
            mov ebx, [num]
            mov [eax], ebx
            mov dword[eax+4], 0 ;New node currenly points to nothing
            mov ebx, [currentNode]
            mov [startOfNodeList], ebx
            jmp andConditions
        
        createAnotherADDLink:
            mov [nextNode], eax
            mov ebx, [num]
            mov [eax], ebx
            mov dword[eax+4], 0
            mov eax, [currentNode]
            mov ebx, dword[nextNode]
            mov [eax+4], ebx
            mov ebx, [nextNode]
            mov [currentNode], ebx
            jmp andConditions

        finishAND:
            mov byte[firstNodeCreated], 0
            mov ebx, [startOfNodeList]
            mov [currentNode], ebx
            mov eax, dword[listNum1]
            mov eax, dword[eax]
            call freeNumberList
            mov eax, dword[listNum2]
            mov eax, dword[eax]
            call freeNumberList
            jmp pushToStack

;------------------- Pop & Print ---------------------------;
    popAndPrint:
            
            mov eax, [stackIndex]
            cmp eax, 1
            jge popNumberFromStack
            push lackOfArgumentsError
            call printf
            add esp, 4
            jmp calculate   

            popNumberFromStack:  
                inc dword[operations] 
                popMyStack ;EAX = pointer to "popped number", can't save on EBX
                mov eax, [eax] ;Pointer to popped number
                mov [listNum1], eax

            printListnum1:
                mov edx, 0
                mov [numOfDigits], edx
        
            loopOverList:
                cmp edx, dword [eax+4]
                je handleEndOfLoop
                inc byte[numOfDigits]
                inc byte[numOfDigits]
                mov ebx, dword [eax+4]
                mov eax, ebx
                jmp loopOverList

            handleEndOfLoop:
                inc byte[numOfDigits] ; There will always be at least 1 digit
                mov eax, [eax]
                cmp eax, 8
                jb continueToMalloc
                inc byte[numOfDigits]
                jmp continueToMalloc

            continueToMalloc:
                inc byte[numOfDigits] ; If the digit is X numbers, we need last char to be \n
                mov eax, [numOfDigits]
                push eax
                call malloc
                add esp, 4
                mov [stringToPrint], eax
                mov ebx, [listNum1]
                mov edx, 10
                dec byte[numOfDigits]
                add eax, [numOfDigits] ; Jump to the last byte in string
                mov [eax], edx ; End of line will be in the end of string
                
                jmp iterateOverLinkedList

                iterateOverLinkedList:  ; EAX hold the current byte in string
                dec eax      ; eax now points to previous byte
                mov ecx, dword[ebx+4]   ;ecx holds now "NEXT" value of node
                cmp ecx, 0
                je lastNodeOfString
                jmp parseNodeNumber

                parseNodeNumber:
                mov ecx, [ebx] ; ecx now holds the number of node
                and ecx, 7  ; ecx will hold the first digit
                add ecx, 48   ; to get the char value of the digit
                mov [eax], cl
                dec eax
                mov ecx, [ebx]
                and ecx, 56
                shr ecx, 3  ; ecx will hold the second digit
                add ecx, 48 
                mov [eax], cl
                mov ebx, dword[ebx+4]   ; move ebx to next node
                jmp iterateOverLinkedList

                lastNodeOfString:
                mov ecx, [numOfDigits]
                and ecx, 1
                cmp ecx, 0  ; check if numOfDigits is even
                je oddLastNodeOfString
                jmp evenLastNodeOfString

                evenLastNodeOfString:
                mov ecx, [ebx] ; ecx now holds the number of node
                and ecx, 7  ; ecx will hold the first digit
                add ecx, 48   ; to get the char value of the digit
                mov [eax], cl
                jmp endPopAndPrint

                oddLastNodeOfString:
                mov ecx, [ebx] ; ecx now holds the number of node
                and ecx, 7  ; ecx will hold the first digit
                add ecx, 48   ; to get the char value of the digit
                mov [eax], cl
                dec eax
                mov ecx, [ebx]
                and ecx, 56
                shr ecx, 3  ; ecx will hold the second digit
                add ecx, 48 
                mov [eax], cl
                mov ebx, dword[ebx+4]   ; move ebx to next node
                jmp endPopAndPrint

                endPopAndPrint:
                push format_string
                push eax
                call printf
                add esp, 8
                mov eax, [listNum1]
                call freeNumberList
                mov eax, [stringToPrint]
                push eax
                call free
                add esp, 4
                jmp calculate

; ------------------------- Duplicate ---------------------- ;

    Duplicate: 
        
        mov eax, [stackIndex]
        cmp eax, 1
        jge startDuplicate
        push lackOfArgumentsError
        call printf
        add esp, 4
        jmp calculate
        startDuplicate:
        inc dword[operations]
        mov dword[currentNode], 0
        mov ebx, [stackIndex]
        mov ecx, [stack]
        sub ebx, 1 ;EBX = stackIndex-1
        shl ebx, 2 ;EBX = stackIndex * 4
        add ecx, ebx ; ecx holds stack top number
        push ecx
        mov ebx, 5 ;Size of memory block
        push ebx
        call malloc ;Allocate 5 bytes of memory
        add esp, 4
        mov [startOfNodeList], eax
        mov [currentNode], eax
        
        pop ecx ; ecx holds stack top number
        mov ecx, [ecx] ; ecx holds beginning of list    
        loopOfDuplicate:
       
        mov ebx, [ecx]
        mov [eax], bl ; move to new Node the data of the other one
        mov ebx, dword[ecx+4]
        cmp ebx, 0  ; check if ecx was last node
        je endOfDuplicate
        mov ecx, ebx
        push ecx
        mov ebx, 5 ;Size of memory block
        push ebx
        call malloc ;Allocate 5 bytes of memory
        add esp, 4
        pop ecx     ; ecx still holds the node of original list
        mov ebx, [currentNode]
        mov dword[ebx+4], eax   ;ebx.setNext(eax)
        mov ebx, dword[ebx+4] ; ebx.next()
        mov [currentNode], ebx  ; update currentNode
        jmp loopOfDuplicate


        endOfDuplicate:
         
        mov eax, [currentNode]
        mov dword[eax+4], 0
        mov eax, [startOfNodeList]
        mov [currentNode], eax
        mov dword[startOfNodeList], 0
        jmp pushToStack


; ----------------- Count number of bytes ----------------;

    countNumOfBytes:
    
    mov eax, [stackIndex]
    cmp eax, 1
    jge startCountOfBytes
    push lackOfArgumentsError
    call printf
    add esp, 4
    jmp calculate
    startCountOfBytes:
    inc dword[operations]
    mov byte[num], 0    ; num will hold the counter of nodes
    popMyStack
    mov [listNum1], eax
    mov eax, [eax]  ; eax holds the first Node of popped list
    
    
    loopOverNumOfBytes:
    mov ebx, dword[eax+4]   ;ebx holds eax.next value
    cmp ebx, 0
    je endOfNumOfBytes
    inc byte[num]
    mov eax, dword[eax+4]   ; eax.next()
    jmp loopOverNumOfBytes

    endOfNumOfBytes:    ; when we get here eax is the last node
    push eax
    mov eax, [num]
    mov ebx, 6
    mul ebx
    mov [num], al
    pop eax
    mov ebx, [eax]

    getlastbytes:
    cmp ebx, 0
    je lastsec
    shr ebx, 1
    inc byte[num]
    jmp getlastbytes

    lastsec:    ; here [num] holds the num of bits we need
    mov ecx, [num]
    shr ecx, 3
    mov ebx, [num]
    and ebx, 7
    cmp ebx, 0
    je lsec1
    inc ecx
    lsec1:
    mov [num], ecx ; divide [num] by 8
    mov ebx, 5
    push ebx
    call malloc
    add esp, 4
    movzx ebx, byte[num]
    mov [eax], ebx
    mov dword[eax+4], 0
    mov [currentNode], eax
    mov eax, dword[listNum1]
    mov eax, dword[eax]
    call freeNumberList

    jmp pushToStack





    ;----------------freeNumberList------------------;
    ;   This function will free the list starting at eax    ;
    freeNumberList:
    mov edx, dword[eax+4]   ; edx = eax.next
    cmp edx, 0
    je FreeLastNode
    push edx
    push eax
    call free
    pop eax
    pop edx
    mov eax, edx
    jmp freeNumberList

    FreeLastNode:
     push eax
     call free
     add esp, 4
     ret
    


; 1000 lines of assembly!!!!!