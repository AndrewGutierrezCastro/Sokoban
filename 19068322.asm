printENTER MACRO
    push ax
    push dx

    mov dl, 10
    mov ah, 02h
    int 21h
    mov dl, 13
    int 21h

    pop dx
    pop ax
ENDM

printAcercaDe macro
  PUSH ax
  PUSH dx
  MOV ah, 09h
  lea dx, AcerdaDe
  int 21h
  POP dx
  POP ax
endm printAcercaDe

pushRegisters macro 
  ;push de la mayoria de registros
  PUSH ax
  PUSH bx
  PUSH cx
  PUSH dx
  PUSH di
  PUSH si

  PUSH ds
  PUSH es 
endm pushRegisters
  
popRegisters macro 
  POP es 
  POP ds
  POP si
  POP di
  POP dx
  POP cx
  POP bx
  POP ax
endm popRegisters

datos segment

    AcerdaDe db "Solo debe ejecutar el programa y jugar.",10,13,7,"Para moverse presione las flechas y para salir ESC",10,13,7,'$'
    var dw ?
    msgErrorLectura db "Error en la lectura del archivo de origen$"
    errorLect db 0 ; 1 error lectura Nivel
    
    numNivelRaw db 000
    numNivelStr db "0000", '$'

    indiceExterno db 20
    buffer db 1000 dup('.');para leer un nivel de 20 fila y 50 columnas
    handleLectura dw ? ;uso un buffer de 50 bytes, 20 veces
    bytesBufferLeidos dw 0
    termineDeLeer db 0


    pathEstandar db "\NIVELES\SOKO000.txt", '$' 
    pathLectura db 73 DUP(0), '$'
    directory db 50 dup('$')
    nombreDrive db "A"

    ;matriz de juego
    matrizSokoban db 1000 dup('*')
    N db 20
    M db 50

datos ends

pila segment stack 'stack'

    dw 2048 dup (?)

pila ends

codigo segment

  assume  cs:codigo, ds:datos, ss:pila

pressEnterContinueEco proc near
  PUSH ax
  XOR ax, ax
  MOV ah, 01h
  noPressEnter:
  int 21h
  CMP al, 13
  JNE noPressEnter

  POP ax  
  ret
pressEnterContinueEco endp

getDirectory proc near
  PUSH ax
  PUSH dx
  PUSH si
  ;GET CURRENT DIRECTORY NAME.
  mov ah, 47h
  XOR dl, dl             ;DRIVE, 0 = CURRENT DRIVE.
  mov si, offset directory ;VARIABLE TO STORE DIRECTORY NAME.
  int 21h
  POP si
  POP dx
  POP ax
  ret
getDirectory endp

getPathNivel proc near
  ;este procedimiento crea el path de los niveles dado el estandar
  ;definido y el numeroNivel de 3 digitos
  PUSH ax
  PUSH bx
  PUSH cx
  PUSH dx
  PUSH di
  PUSH si
  PUSH ds
  PUSH es
    ;mover el numero
	;primero obtener el numero del nivel en forma de string
	lea si, numNivelStr;en el si un vector de 4 bytes
	MOV al, numNivelRaw;numero a convertir en el ax
	XOR ah, ah
	MOV bx, 10; la base en el bx
	CALL convertirnumBaseN
    ; de esta manera obtengo 15 en '0015'

	MOV bl, nombreDrive
	MOV byte ptr pathLectura[0], bl
	MOV byte ptr pathLectura[1], ':'
	MOV byte ptr pathLectura[2], '\'

	CALL getDirectory
	XOR bx, bx

	ciclopathNivel:
		MOV al, byte ptr directory[bx]
		MOV byte ptr pathLectura[bx+3], al
		INC bx
	CMP byte ptr directory[bx], 0
	JE seguirCicloPathNivel
	JMP ciclopathNivel
	seguirCicloPathNivel:
	ADD bx, 3

    push ds
    pop es
    cld
    ;Mover el Niveles\sokoXXX.txt
    lea si, pathEstandar
    lea di, pathLectura
    ADD di, bx
    MOV cx, 20
    REP MOVSB
    
  	push ds
    pop es
    cld 
    lea si, numNivelStr
    inc si
    lea di, pathLectura
    ADD di, 13
    ADD di, bx
    MOV cx, 3
    REP MOVSB
  POP es
  POP ds
  POP si
  POP di
  POP dx
  POP cx
  POP bx
  POP ax
  ret
getPathNivel endp

lecturaNiveles proc near

  
  PUSH ax
  PUSH bx
  PUSH cx
  PUSH dx

  lea dx, pathLectura
  MOV ax, 3D00h
  int 21h    
  JNC sinErrorAbrirFichero
  MOV errorLect, 1
  JMP errorLectura
  sinErrorAbrirFichero:
  MOV handleLectura, ax

  MOV indiceExterno, 20 ;20 * 50 = 10000
  
    cicloLectura:
      
      MOV ax, 3F00h
      MOV bx, handleLectura
      MOV cx, 1000 ; leer todo el buffer
      lea dx, buffer
      int 21h
      JNC bufferLeyoBien
      MOV errorLect, 1; =1 error de lectura
      JMP errorLectura
      bufferLeyoBien:

      MOV bytesBufferLeidos, ax ; mover la cantidad de bytes que leyo        
      MOV var, ax
      CALL copyBufferToMatrix

    condicionCicloLectura:
      INC byte ptr[indiceExterno]
      CMP byte ptr[indiceExterno], 20
      JAE cerrarLecturaArhivo
      JMP cicloLectura
        
    cerrarLecturaArhivo:

      MOV ah, 3Eh
      MOV bx, handleLectura
      int 21h  
      JNC sinErrorCerrarLectura
      JMP errorLecturaMsgPrint

      sinErrorCerrarLectura:
      JMP finalLecturaArchivo

    errorLectura:
      
      CMP errorLect, 1
      JE errorLecturaMsgPrint
      JMP finalLecturaArchivo
      errorLecturaMsgPrint:
        MOV ah, 09h
        lea dx, msgErrorLectura
        int 21h
        JMP finalLecturaArchivo

  finalLecturaArchivo:

    POP dx
    POP cx
    POP bx
    POP ax
  ret
lecturaNiveles endp

copyBufferToMatrix proc near
  
  pushRegisters 
  ;este metodo recibe el numero de fila en indiceExterno
  ;en el buffer el array a copiar
  
  push ds
  pop es
  cld
  lea si, buffer
  lea di, matrizSokoban
  ;calcular el numero de fila
  MOV cx, bytesBufferLeidos
  REP MOVSB

  popRegisters
copyBufferToMatrix endp

convertirnumBaseN proc near
  ;recibe en el ax el numero a convertir
  ;en el bx la base a convertir 
  ;la salida se da en el si
  ;el buffer es de 4bytes

  push ax            
  push bx
  push cx
  push dx

  ADD si, 4
  convert:
    xor dx, dx         
    div bx             
    add dl, '0'        
    cmp dl, '9'       
    jbe store         
    add dl, 'A'-'0'-10 
  store:
    dec si             
    mov [si], dl       
    and ax, ax        
    jnz convert

  ;lea si, bufferHexPrint
  ;MOV ah, 02h
  ;MOV cx, 4
  ;cicloPrintHex:
  ;CMP byte ptr [si], 0
  ;JE seguirCicloPrintHex
  ;MOV dl, [si]
  ;int 21h
  ;seguirCicloPrintHex:
  ;INC si
  ;LOOP cicloPrintHex

     
  pop dx
  pop cx            
  pop bx
  pop ax

  ret
convertirnumBaseN endp

printMatrix proc near
  pushRegisters
  XOR bx, bx
  cicloPrintMatrix:
  PUSH bx
    
    MOV cx, 50
    cicloPrintMatrixInterno:
      PUSH cx
      MOV ch, bl
      ;En el cl esta el index interno
      CALL axezador
      MOV dl, al
      MOV ah, 02h
      int 21h

      POP cx 
    LOOP cicloPrintMatrixInterno
    printENTER
  POP bx
  INC bx
  CMP bx, 20
  JB cicloPrintMatrix
  popRegisters
  ret
printMatrix endp

axezador proc near
    ; supone los indices en cl y ch (fil y col)
    ; retorna en el ax el contenido
    ; supone en [si] un ptr a la matriz.
    ; supone los tamaños en dh y dl
    MOV dh, N
    MOV dl, M
    lea si, matrizSokoban
    push bx
    push cx
    push dx
    push si

    mov al, cl
    mul dh
    mov bl, ch
    xor bh, bh
    add ax, bx
    shl ax, 1
    add si, ax
    mov ax, word ptr [si]    

    pop SI
    pop DX
    pop CX
    pop BX
    ret
axezador endp

copyRowToMatrix proc near
  pushRegisters 
  ;este metodo recibe el numero de fila en indiceExterno
  ;en el buffer el array a copiar
  
  push ds
  pop es
  cld
  lea si, buffer
  lea di, matrizSokoban
  ;calcular el numero de fila
  MOV al, indiceExterno
  MOV bl, 50
  MUL bl
  ADD di, ax; sumar el numero de fila correspondiente
  MOV cx, bytesBufferLeidos
  REP MOVSB

  popRegisters
  ret
copyRowToMatrix endp

printAX proc
  ; imprime a la salida estándar un número que supone estar en el AX
  ; supone que es un número positivo y natural en 16 bits.
  ; lo imprime en decimal.  
    
    push AX
    push BX
    push CX
    push DX

    xor cx, cx
    mov bx, 10
  ciclo1PAX: xor dx, dx
      div bx
      push dx
      inc cx
      cmp ax, 0
      jne ciclo1PAX
      mov ah, 02h
  ciclo2PAX: pop DX
      add dl, 30h
      int 21h
      loop ciclo2PAX

    pop DX
    pop CX
    pop BX
    pop AX
    ret
printAX endP


	inicio: 
	  
	mov ax, datos
	mov ds, ax
	mov ax, pila
	mov ss, ax
	

	printAcercaDe
	CALL pressEnterContinueEco

	CALL getPathNivel

	CALL lecturaNiveles

  CALL printMatrix

  printENTER
  MOV ax, var
  CALL printAX
	
	mov  ax,4C00h
	int  21h

codigo ends

end inicio