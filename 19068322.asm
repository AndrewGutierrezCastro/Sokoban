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
    msgLimiteNiveles db "Se buscaron mas niveles y no se pudo cargar ninguno, desea salir?",10,13,7,"Presione ESC",10,13,7,"Si quiere volver a empezar desde 0 presione cualquier otra tecla",10,13,7,'$'
    errorLect db 0 ; 1 error lectura Nivel, 0 sin errores
    
    numNivelRaw db 000
    numNivelStr db "0000", '$'

    indiceExterno db 20
    buffer db 50 dup('.');para leer un nivel de 20 fila y 50 columnas
    handleLectura dw ? ;uso un buffer de 50 bytes, 20 veces
    bytesBufferLeidos dw 0
    termineDeLeer db 0
    punteroLectura dw 0

    pathEstandar db "\NIVELES\SOKO000.txt", '$' 
    pathLectura db 73 DUP(0), '$'
    directory db 50 dup('$')
    nombreDrive db "A"

    ;matriz de juego
    matrizSokoban db 1040 dup('!')
    numFilasCargadas db 0
    kirstein db 0,0
    N db 20
    M db 52

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
  MOV errorLect, 0
  lea dx, pathLectura
  MOV ax, 3D00h
  int 21h    
  JNC sinErrorAbrirFichero
  MOV errorLect, 1
  JMP errorLectura
  sinErrorAbrirFichero:
  MOV handleLectura, ax
  MOV punteroLectura, 0
  MOV indiceExterno, 0 ;20 * 50 = 10000
  
    cicloLectura:
      mov ax, 4200h ; mover el filepointer
      mov bx, handleLectura; desde el inico del arhcivo
      mov dx, punteroLectura
      xor cx,cx
      int 21h

      MOV ax, 3F00h
      MOV bx, handleLectura
      MOV cx, 50 ; leer 50 bytes en el buffer
      lea dx, buffer
      int 21h
      
       
      JNC bufferLeyoBien
      MOV errorLect, 1; =1 error de lectura
      JMP errorLectura
      bufferLeyoBien:

        CMP ax, 0
        JNE leiAlgo
        JMP cerrarLecturaArhivo
      leiAlgo:
        MOV bytesBufferLeidos, ax ; mover la cantidad de bytes que leyo  
        CALL revisarFilaLeida  
        MOV ax, bytesBufferLeidos
        CALL copyRowToMatrix
        ;MOV al, indiceExterno
        ;CALL printRowMatrix

    condicionCicloLectura:
      INC byte ptr[indiceExterno]
      CMP byte ptr[indiceExterno], 20
      JAE cerrarLecturaArhivo
      JMP cicloLectura
        
    cerrarLecturaArhivo:
      MOV ah, indiceExterno
      MOV numFilasCargadas, ah
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

revisarFilaLeida proc near
  ;este procedimiento revisa si el buffer leyo mas de una fila
  ;y ajusta el contador de bytesBufferLeidos para copiar Solo
  ;los caracteres correspondientes a esa fila.
  pushRegisters
  XOR bx, bx
  MOV cx, bytesBufferLeidos
  ;MOV ah, 02h
  cicloRevisarFilaLeida:

    ;MOV dl, byte ptr buffer[bx]
    ;int 21h
    CMP byte ptr buffer[bx], 'K'
    JE encontreAKirstein2
    CMP byte ptr buffer[bx], 'k'
    JE encontreAKirstein2
    JMP noEncontreAKirstein
    encontreAKirstein2:
    MOV al, indiceExterno
    MOV byte ptr kirstein[0], al ;fila
    MOV byte ptr kirstein[1], bl ;columna
    INC byte ptr kirstein[1] ;al inicio esta el tamaño
    noEncontreAKirstein:
    CMP byte ptr buffer[bx], 13; revisar si leyo mas de una fila
    JE leyoMasDeUnaFila
    INC bx
    LOOP cicloRevisarFilaLeida
    JMP finalRevisarFilaLeida
  
  leyoMasDeUnaFila:
  MOV bytesBufferLeidos, bx ; mover la cantidad de bytes hasta el cambio 
  INC bx                    ;de linea 
  finalRevisarFilaLeida:
  MOV bx, bytesBufferLeidos
  INC bx
  INC bx 
  ADD punteroLectura, bx; sumarle al filepointer la cantidad
  ;de bytes leidos, para que la proxima fila empiece a leer
  ;desde el lugar correcto
  popRegisters
  ret
revisarFilaLeida endp

copyBufferToMatrix proc near
  pushRegisters 
  ;este metodo copia el buffer a la matriz
  ;antiguo. No se recomienda usar
  
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
  
    MOV al, bl
    CALL printRowMatrix
    printENTER
  INC bl
  CMP bl, N
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
  
  ;CALL printBuffer

  pushRegisters 
  ;este metodo recibe el numero de fila en indiceExterno
  ;en el buffer el array a copiar
  
  lea si, buffer
  lea di, matrizSokoban
  ;calcular el numero de fila
  XOR ah, ah
  MOV al, indiceExterno
  MOV bl, M; cantidad de columnas
  MUL bl
  ADD di, ax; sumar el numero de fila correspondiente
  MOV cx, bytesBufferLeidos
  MOV byte ptr [di], cl
  INC di
  push ds
  pop es
  cld
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

printBuffer proc near
  pushRegisters
  MOV cx, bytesBufferLeidos
  MOV ah, 02h
  XOR bx, bx
  cicloPrintBuffer:
    MOV dl, byte ptr buffer[bx]
    INC bx
    int 21h
  LOOP cicloPrintBuffer

  popRegisters
  ret
printBuffer endp

printRowMatrix proc near
  ;este procedimiento imprime una fila de la matriz 
  ;el indice externo se da en el al
  pushRegisters
  lea di, matrizSokoban
  ;calcular el numero de fila
  ;viene dado en el al
  MOV bl, M ; tamaño de columnas
  MUL bl
  ADD di, ax; sumar el numero de fila correspondiente
  INC di
  ;en el di tengo el puntero la fila correcta

  XOR ch, ch
  MOV cl, M ; imprime toda la fila o cuando llegue a un salto
  ; de linea, lo que suceda primero
  MOV ah, 02h
  cicloPrintRowMatrix:
    MOV dl, [di]
    int 21h
    inc di
    CMP byte ptr [di], '!'; revisar si leyo mas de una fila
    JE printRowFinal
    LOOP cicloPrintRowMatrix
    JMP finalRevisarFilaLeida
  printRowFinal:

  popRegisters
  ret
printRowMatrix endp

printRowSize proc near
  ;imprime los tamaños de las filas, el tamaño es la cantidad de bytesBufferLeidos
  ;esta frente del arreglo like pascal
  pushRegisters
  XOR ch, ch
  MOV cl, N
  cicloPrintRowSize:
    lea di, matrizSokoban
    ;calcular el numero de fila
    MOV al, N
    SUB al, cl
    MOV bl, M ; tamaño de columnas
    MUL bl
    ADD di, ax; sumar el numero de fila correspondiente
    MOV al, byte ptr[di]
    XOR ah, ah
    CALL printAX
    printENTER
  LOOP cicloPrintRowSize

  popRegisters
  ret
printRowSize endp

cargaDeNiveles proc near
  ;este procedimiento carga niveles hasta 100, sino encuentra pregunta si desea salir
  ;sino vuelve al 0
  pushRegisters
  cicloCargaNiveles:
    CALL getPathNivel
    MOV ah, 09h
    lea dx, pathLectura
    int 21h
    CALL pressEnterContinueEco
    CALL lecturaNiveles

    CMP errorLect, 0
    JNE noSeCargoNivel
    JMP finalCargaNiveles

    noSeCargoNivel:
      INC numNivelRaw
      JMP condicionCicloCargaNiveles

    condicionCicloCargaNiveles:
      CMP numNivelRaw, 100
      JA limiteNiveles
  JMP cicloCargaNiveles

  limiteNiveles:
    MOV ah, 09h
    lea dx, msgLimiteNiveles
    int 21h
    MOV ah, 01h
    int 21h
    CMP al, 27 ; si dio escape
    JE finalCargaNiveles
    MOV numNivelRaw, 0
    JMP cicloCargaNiveles
  finalCargaNiveles:
  popRegisters
  ret
cargaDeNiveles endp

imprimirFilaVideo proc near
  ;se recibe en el si el puntero a la fila
  pushRegisters

  MOV al, 80
  MUL indiceExterno
  shl ax, 1
  mov di, ax ;posicion en la pantalla

  mov  ax,0B800h
  mov  es,ax

  mov  cl, byte ptr[si]
  xor ch, ch
  inc si

  cicloPrintFilaVideo:    ;Bucle que se encargara de pintar la string
      mov  al, [si]        ;caracteres de la pantalla para limpiarla
      mov  ah, 0          ;Fondo azul, letras blancas
      OR ah, 07h
      mov  es:[di], ax
      inc  si             ;Pasamos a apuntar a la siguiente letra del saludo
      inc  di
      inc  di

      LOOP cicloPrintFilaVideo
  popRegisters
  ret
imprimirFilaVideo endp

imprimirMatrizVideo proc near
  CALL limpiarPantalla
  pushRegisters
  XOR cx, cx
  cicloPrintMatrizVideo:
  
    lea si, matrizSokoban
    XOR ax, ax;calcular el numero de fila
    or al, cl
    MOV bl, M ; tamaño de columnas
    MUL bl
    ADD si, ax; sumar el numero de fila correspondiente
    MOV indiceExterno, cl
    INC indiceExterno
    CALL imprimirFilaVideo
  
  INC cl
  CMP cl, numFilasCargadas
  JB cicloPrintMatrizVideo

  popRegisters
  ret
imprimirMatrizVideo endp

limpiarPantalla proc near
  PUSH ax
  PUSH bx
  PUSH cx
  PUSH dx
  PUSH di
  PUSH si
  mov  ax,0B800h      ;En esta direccion comienza la memoria de video
  mov  es,ax          ;Lo cargamos en el segmento extra
  xor  di,di          ;Ponemos DI=0. Esto equivale a mov di,0, pero
                     ;xor di,di consume 3 ciclos de reloj y con mov 4
  mov  cx,80*25       ;El tamaño total es 2000 (80 lineas x 25 columnas)

  _clear:            ;Bucle que se encargara de recorrer los 2000
                     ;caracteres de la pantalla para limpiarla
      mov  al,20h    ;20h=" "   Rellenar la pantalla con espacios
      mov  ah,00h    ;Fondo azul, letras blancas
      mov  es:[di],ax
      inc  di
      inc  di
      loop _clear
  POP si
  POP di
  POP dx
  POP cx
  POP bx
  POP ax
  ret
limpiarPantalla endp

buscarAKirstein proc near
  ;este procedimiento busca al crack de kirstein en la matriz
  pushRegisters
  XOR cx, cx
  cicloBuscarKirstein:

    MOV al, cl
    CALL buscarAKirsteinFila
    JC finalBuscarAKirstein
  INC cl
  CMP cl, numFilasCargadas
  JB cicloBuscarKirstein
  finalBuscarAKirstein:

  popRegisters
  ret
buscarAKirstein endp

buscarAKirsteinFila proc near
  ; busca a kirstein en las filas
  ; si lo encuentra se activa el carry
  pushRegisters

  lea di, matrizSokoban
  ;calcular el numero de fila
  ;viene dado en el al
  MOV bl, M ; tamaño de columnas
  MUL bl
  ADD di, ax; sumar el numero de fila correspondiente
  MOV cl, [di] ;como es like pascal el tamaño viene al frente
  inc di
  cicloBuscarKirsteinFila:
    CMP byte ptr[di], 'K'
    JE encontreAKirstein
    CMP byte ptr[di], 'k'
    JE encontreAKirstein
    inc di
  LOOP cicloBuscarKirsteinFila
  JMP finalBuscarAKirsteinFila
  encontreAKirstein:
    MOV byte ptr kirstein[0], al ;fila
    MOV byte ptr kirstein[1], cl ;columna
    INC byte ptr kirstein[1] ;al inicio esta el tamaño
    STC
  finalBuscarAKirsteinFila:
  popRegisters
  ret
buscarAKirsteinFila endp

	inicio: 
	  
	mov ax, datos
	mov ds, ax
	mov ax, pila
	mov ss, ax
	
	printAcercaDe
	CALL pressEnterContinueEco

	CALL cargaDeNiveles
  
  CALL printMatrix
  ;CALL pressEnterContinueEco
  ;CALL imprimirMatrizVideo
	;CALL buscarAKirstein
  XOR ah, ah
  MOV al, kirstein[0]
  CALL printAX
  printENTER
  XOR ah, ah
  MOV al, kirstein[1]
  CALL printAX
  printENTER
	mov  ax,4C00h
	int  21h

codigo ends

end inicio