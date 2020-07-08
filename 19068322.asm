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

printSpace macro 
  PUSH ax
  PUSH dx
  
  MOV ah, 02h
  MOV dl, ' '
  int 21h

  POP dx
  POP ax
endm printSpace

datos segment

    AcerdaDe db "Solo debe ejecutar el programa y jugar.",10,13,7
    db "Para moverse presione las flechas y para salir ESC",10,13,7,'$'
    msgDigiteDrive db "Digite la letra del nombre de su drive de DOSBOX: ",10,13,7,'$'
    var dw ?
    msgErrorLectura db "Error en la lectura del archivo de origen$"
    msgLimiteNiveles db "Se buscaron mas niveles y no se pudo cargar ninguno, desea salir?",10,13,7
                    db "Presione ESC",10,13,7,"Si quiere volver a empezar desde 0 presione cualquier otra tecla",10,13,7,'$'
    msgTeclaPresionada db "Se presiono la tecla",10,13,7,'$'
    msgNivel db 11,"Nivel: ","    ",'$'
    msgPasos db 11,"Pasos: ","    ",'$'
    msgPush db 13,"Empujes: ","    ",'$'
    msgBlankContadores db "    "
    msgNivelesResueltos db 23,"Niveles Resueltos: ","    ",'$'
    contadores dw 0,0,0;pasos, push, niveles      
    ;Mensajes de teclado
    msgTecladoAyuda db 8,"Ayuda: H"
    msgTecladoReset db 8,"Reset: R"
    msgTecladoSigNivel db 18,"Siguiente Nivel: S"
    msgTecladoAntNivel db 17,"Anterior Nivel: A"
    msgTecladoHighScore db 13,"HighScores: N"
    msgTecladoSalir db 10,"Salir: ESC"
    msgTecladoAcerdaDe db 11,"AcercaDe: M"


    msgVideoAcercaDe db 43," ========================================= "
                     db 43,"| Acerca De:            Sokoban TEC Arqui |"
                     db 43,"| Intstituto Tecnologico de Costa Rica    |"
                     db 43,"| Sokoban  :                              |"
                     db 43,"|    +Juego en Ensamblador con video      |"
                     db 43,"|    +Desarrollado por Andrew JGC         |"
                     db 43,"|    +Uso directo del teclado             |"
                     db 43,"|    +Carga de niveles automatica         |"
                     db 43,"|    +Carpeta Niveles necesaria con el    |"
                     db 43,"|     ejecutable                          |"
                     db 43,"| Semestre I   -*ENTER PARA SEGUIR*- 2020 |"
                     db 43," ========================================= " 

    msgVideoAyuda    db 43," ========================================= "
                     db 43,"| Instrucciones:        Sokoban TEC Arqui |"
                     db 43,"| Intstituto Tecnologico de Costa Rica    |"
                     db 43,"| Sokoban  :                              |"
                     db 43,"|    +Para jugar use las flechas del te-  |"
                     db 43,"|     clado                               |"
                     db 43,"|    +Para salir presione la tecla ESC    |"
                     db 43,"|    +Resetear el nivel:R HighScores: H   |"
                     db 43,"|     Siguiente y anterior nivel: S, A    |"
                     db 43,"|                                         |"
                     db 43,"| Semestre I   -*ENTER PARA SEGUIR*- 2020 |"
                     db 43," ========================================= "

    msgVideoErrNivel db 43," ========================================= "
                     db 43,"| Error Lectura:        Sokoban TEC Arqui |"
                     db 43,"| Error con la lectura de un nivel, tiene |"
                     db 43,"| diferente numero de cajas y entregas o  |"
                     db 43,"| el nivel no existe.....                 |"
                     db 43,"|    +Presione Enter para intentar leer   |"
                     db 43,"|     el siguiente nivel                  |"
                     db 43,"|    +Error con el nivel:                 |"
                     db 43,"|                                         |"
                     db 43,"|                                         |"
                     db 43,"| Semestre I   -*ENTER PARA SEGUIR*- 2020 |"
                     db 43," ========================================= "
    ;pasos, push, nivelesConcluidos
    errorLect db 0 ; 1 error lectura Nivel, 0 sin errores

    numNivelRaw dw 000
    numNivelStr db "0000", '$'
    numMaxNivel dw 99
    numNivelBlank db "0000"

    indiceExterno db 20
    buffer db 50 dup('.');para leer un nivel de 20 fila y 50 columnas
    handleLectura dw ? ;uso un buffer de 50 bytes, 20 veces
    bytesBufferLeidos dw 0
    termineDeLeer db 0
    punteroLectura dw 0

    punteroPathLectura dw 0
    pathEstandar db "\NIVELES\SOKO000.txt", '$' 
    ;pathLectura db "A:\ENSAMB~1\VIDEOS~1\SOKOBAN\NIVELES\soko139.txt" , '$'
    pathLectura db 73 DUP(0), '$'
    
    directory db 64 dup('0'), '$'
    nombreDrive db "A"
    teclaGuardada db 0
    ;matriz de juego
    matrizSokoban db 1040 dup('!')
    matrizSoloNivel DB 1040 dup('!')
    numFilasCargadas db 0
    kirstein db 0,0,'K'
    N db 20
    M db 52
    muro db 'M'
    espacio db ' '
    caja db 'X'
    zonaEntrega db 'O'
    cajasEntregadas db 0
    numCajas db 0
    numZonasDeEntrega db 0

    cajasEnPosicion db 0
    ;movimientoSokoban
    x db 0 
    y db 0
    salir dw 0
datos ends

pila segment stack 'stack'

    dw 2048 dup (?)

pila ends

codigo segment

  assume  cs:codigo, ds:datos, ss:pila

pressEnterContinueEco proc near
  PUSH ax
  MOV teclaGuardada, 0
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
	MOV ax, numNivelRaw;numero a convertir en el ax
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
  MOV punteroPathLectura, bx
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
  MOV numCajas, 0
  MOV numZonasDeEntrega, 0
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
      MOV al, numZonasDeEntrega
      CMP numCajas, al
      JE igualNumeroCajazZonas
      MOV errorLect, 1
      JMP errorLectura
      igualNumeroCajazZonas:
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
      ; CALL printAX
      ; printSpace
      ; CMP errorLect, 1
      ; JE errorLecturaMsgPrint
      ; JMP finalLecturaArchivo
      errorLecturaMsgPrint:
        ; MOV ah, 09h
        ; lea dx, msgErrorLectura
        ; int 21h
        ; JMP finalLecturaArchivo

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

    CMP byte ptr buffer[bx], 'X';Conteo de Cajas y zonas de entrega
      JNE noSumarCaja
      INC numCajas
      noSumarCaja:
        CMP byte ptr buffer[bx], 'O'
          JNE noSumarZonaEntrega
          INC numZonasDeEntrega
          noSumarZonaEntrega:
    
    CMP byte ptr buffer[bx], '.'
    JNE noEsElEspacio
    MOV al, espacio
    MOV byte ptr buffer[bx], al
    noEsElEspacio:
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
    ; no supone los tamaños en dh y dl
    
    push bx
    push cx
    push dx
    push si

    MOV al, ch
    MOV bl, M ; tamaño de columnas
    MUL bl
    XOR ch, ch
    ADD ax, cx 
    ADD si, ax; sumar el numero de fila correspondiente
    ;shl ax, 1
    mov al, byte ptr[si]    

    pop SI
    pop DX
    pop CX
    pop BX
    ret
axezador endp

almacenador proc near
  ; supone los indices en cl y ch (fil y col)
  ; retorna en el ax el contenido
  ; supone en [si] un ptr a la matriz.
  ; se supone en el al el dato a guardar
  
  push bx
  push cx
  push dx
  push si
  MOV dl, al
  MOV al, ch
  MOV bl, M ; tamaño de columnas
  MUL bl
  XOR ch, ch
  ADD ax, cx 
  ADD si, ax; sumar el numero de fila correspondiente
  ;shl ax, 1
  mov byte ptr[si], dl    

  pop SI
  pop DX
  pop CX
  pop BX
  ret

  ret
almacenador endp

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

printAxToSi proc near
  push AX
  push BX
  push CX
  push DX
  push SI

    xor cx, cx
    mov bx, 10
  ciclo1PAXtSI: xor dx, dx
      div bx
      push dx
      inc cx
      cmp ax, 0
      jne ciclo1PAXtSI
      ;mov ah, 02h
  ciclo2PAXtSI: pop DX
      add dl, 30h
      mov byte ptr[si], dl
      INC si      
      loop ciclo2PAXtSI

  pop SI
  pop DX
  pop CX
  pop BX
  pop AX

  ret
printAxToSi endp

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
  ;lea di, matrizSokoban
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
    ; MOV ah, 09h
    ; lea dx, pathLectura
    ; int 21h
    ; CALL pressEnterContinueEco
    CALL lecturaNiveles

    CMP errorLect, 0
    JNE noSeCargoNivel
    JMP finalCargaNiveles

    noSeCargoNivel:

      CALL printErrorNivelVideo  
      INC numNivelRaw
      JMP condicionCicloCargaNiveles

    condicionCicloCargaNiveles:
      lea si,numNivelBlank
      lea di, numNivelStr
      MOV cx, 4
      CALL copyStringSIDI
      MOV ax, numNivelRaw
      CMP ax, numMaxNivel    
      JA limiteNiveles
  JMP cicloCargaNiveles

  limiteNiveles:
    ; MOV ah, 09h
    ; lea dx, msgLimiteNiveles
    ; int 21h
    ; MOV ah, 01h
    ; int 21h
    ; CMP al, 27 ; si dio escape
    ; JE finalCargaNiveles
    XOR ax, ax
    MOV numNivelRaw, ax
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
      mov  ah, 15          ;Fondo azul, letras blancas
      OR ah, 01h
      CMP al, byte ptr kirstein[2]
      JNE noEsKirstein
        MOV ah, 4
      noEsKirstein:
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

    CALL imprimirFilaVideo
  
  INC cl
  CMP cl, numFilasCargadas
  JB cicloPrintMatrizVideo

  popRegisters
  ret
imprimirMatrizVideo endp

retardadorPantalla proc near
     
  PUSH cx
  MOV cx, 8192
  retardadorPantallaCiclo:
      PUSH cx
      MOV cx, 1
      retardadorPantallaCiclo2:

      loop retardadorPantallaCiclo2
      POP CX
  loop retardadorPantallaCiclo
  POP CX

  ret
retardadorPantalla endp

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
    MOV ax, cx
    ;CALL printAX
    ;printSpace
    CALL buscarAKirsteinFila
    JC finalBuscarAKirstein    
    
  INC cl
  CMP cl, numFilasCargadas
  JNG cicloBuscarKirstein
  finalBuscarAKirstein:
  printENTER
  popRegisters
  ret
buscarAKirstein endp

buscarAKirsteinFila proc near
  ; busca a kirstein en las filas
  ; si lo encuentra se activa el carry
  pushRegisters
  MOV dl, al
  lea di, matrizSokoban
  ;calcular el numero de fila
  ;viene dado en el al
  MOV bl, M ; tamaño de columnas
  MUL bl
  ADD di, ax; sumar el numero de fila correspondiente
  MOV cl, [di] ;como es like pascal el tamaño viene al frente
  MOV dh, cl
  inc di
  cicloBuscarKirsteinFila:
    CMP byte ptr[di], 'K'
    JE encontreAKirstein
    CMP byte ptr[di], 'k'
    JE encontreAKirstein
    inc di
  LOOP cicloBuscarKirsteinFila
  clc
  JMP finalBuscarAKirsteinFila
  encontreAKirstein:
    MOV byte ptr kirstein[0], dl ;fila
    SUB dh, cl 
    MOV byte ptr kirstein[1], dh ;columna
    INC byte ptr kirstein[1] ;al inicio esta el tamaño
    STC
  finalBuscarAKirsteinFila:
  popRegisters
  ret
buscarAKirsteinFila endp

getKeyPress proc near
  ;revisa si se ha presionado una tecla, la cual se retorna en el al
  MOV AH,01H
  INT 16H
  JZ noHayTecla
  XOR AH,AH
  INT 16H
  stc
  JMP finalGetKeyPress
  noHayTecla:
  XOR ax, ax
  clc
  finalGetKeyPress:
  ret
getKeyPress endp

movimientoSokoban proc near
  ;este procedimiento se encarga del movimiento del matrizSokoban
  ;se espera del teclado que se presione 
  pushRegisters

  cicloMovimientoSokoban:
  CALL retardadorPantalla
  CALL getKeyPress
  JNC noPresionoTecla
  ;ah ->
  ;arriba  = 72
  ;derecha = 77
  ;abajo = 80
  ;izquierda = 75
  MOV teclaGuardada, 0
    CMP ah, 72
    JNE noPresionoArriba
    JMP presionoArriba
    noPresionoArriba:
      CMP ah, 75
      JNE noPresionoIzquierda
      JMP presionoIzquierda
      noPresionoIzquierda:
        CMP ah, 77
        JNE noPresionoDerecha
        JMP presionoDerecha
        noPresionoDerecha:    
          CMP ah, 80
          JNE noPresionoAbajo
          JMP presionoAbajo
          noPresionoAbajo:
            JMP noPresionoTecla
    ;dependiendo de la tecla la direccion cambia
    ;Arriba es    x:-1, y: 0
    ;Izquierda es x: 0, y:-1
    ;Derecha es   x: 0, y:+1
    ;Abajo es     x:+1, y: 0
    ;De esta forma puedo hacer un procedimiento que busque colisiones
    ;pero sumando siempre.
    presionoArriba:
      MOV x, -1
      MOV y, 0
      JMP finalMovimientoSokoban
    presionoIzquierda:
      MOV x, 0
      MOV y, -1
      JMP finalMovimientoSokoban
    presionoDerecha:
      MOV x, 0
      MOV y, 1
      JMP finalMovimientoSokoban
    presionoAbajo:
      MOV x, 1
      MOV y, 0
      JMP finalMovimientoSokoban
  noPresionoTecla:
  CMP al, 27
  JE salirJuego
  MOV teclaGuardada, al
  JMP finalMovimientoSokoban
  salirJuego:
    MOV ax, 1
    MOV salir, ax

  finalMovimientoSokoban:


  popRegisters
  ret
movimientoSokoban endp

verificarColisiones proc near
  ;este metodo busca colisiones segun la direccion en la que 
  ;que quiere mover el usuario al sokoban
  pushRegisters
    ;posicion del sokoban en kirstein
    MOV ch, kirstein[0]
    MOV cl, kirstein[1]
    lea si, matrizSokoban
    CALL axezador
    MOV kirstein[2], al

    ADD ch, x
    ADD cl, y
    lea si, matrizSokoban
    CALL axezador
    CMP al, muro
    JNE noEsMuro
    JMP esMuro
    noEsMuro:
      CMP al, espacio
      JNE noEsEspacio
      JMP esEspacio
      noEsEspacio:
        CMP al, zonaEntrega
        JNE noEsZonaEntrega
        JMP esZonaEntrega
        noEsZonaEntrega:
          CMP al, caja
          JNE noEsCaja
          JMP quiereEmpujarCaja
          noEsCaja:
            JMP esMuro
    
    esMuro: 
      JMP finalVerificarColisiones   
    esZonaEntrega:
    esEspacio:
      ; el ch y cl estan apuntando a donde quiero ir
      MOV kirstein[0], ch
      MOV kirstein[1], cl
      MOV al, kirstein[2]
      lea si, matrizSokoban
      CALL almacenador

      SUB ch, x; ir a la posicion anterior
      SUB cl, y;traer el valor de la matriz Solo nivel
      lea si, matrizSoloNivel
      CALL axezador
      lea si, matrizSokoban
      CALL almacenador
      XOR ax, ax 
      MOV x, ah
      MOV y, al
      INC word ptr contadores[0]
      ;pasos, push, nivelesConcluidos
      JMP finalVerificarColisiones
    quiereEmpujarCaja:
      ADD ch, x
      ADD cl, y
      lea si, matrizSokoban
      CALL axezador
      CMP al, espacio
      JE sePuedeMoverCaja
      CMP al, zonaEntrega
      JE sePuedeMoverCaja
      JMP noSePuedeMovercaja
      sePuedeMoverCaja:
        lea si, matrizSokoban
        MOV al, caja
        ;en el ch y cl ya estan los indices
        CALL almacenador
        ;ya movi la caja, ahora voy a mover a kirstein
        SUB ch, x
        SUB cl, y
        MOV kirstein[0], ch
        MOV kirstein[1], cl 
        lea si, matrizSokoban
        MOV al, kirstein[2]
        CALL almacenador
        ;ahora voy a poner un espacio en blanco donde estaba
        ;kirstein
        SUB ch, x
        SUB cl, y
        lea si, matrizSoloNivel
        CALL axezador
        lea si, matrizSokoban
        CALL almacenador
        XOR ax, ax 
        MOV x, ah
        MOV y, al
        INC word ptr contadores[0]
        INC word ptr contadores[2]
        ;pasos, push, nivelesConcluidos
        JMP finalVerificarColisiones
      noSePuedeMovercaja:
        JMP finalVerificarColisiones
  finalVerificarColisiones:

  popRegisters
  ret
verificarColisiones endp

hacerMatrizSinObjetosMoviles proc near
  ;este procedimiento hace una copia de la matriz
  ;del nivel pero elimina los objetos moviles 
  ;dejando solo los muros, zonas de entrega y espacios

  PUSH ax
  PUSH di
  PUSH es
  PUSH si

  PUSH ds
  POP es
  lea si, matrizSokoban
  lea di, matrizSoloNivel
  cld
  XOR cx, cx
  MOV al, N
  MUL M
  MOV cx, ax
  ciclo_HacerMatriz:
    LODSB
    CMP al, caja
    JE cambiarElementoMovilPorEspacio
    CMP al, kirstein[2]
    JE cambiarElementoMovilPorEspacio
    JMP condicionCiclo_HacerMatriz
    cambiarElementoMovilPorEspacio:
      MOV al, espacio
    condicionCiclo_HacerMatriz:
      STOSB
    LOOP ciclo_HacerMatriz
  POP si
  POP es
  POP di
  POP ax

  ret
hacerMatrizSinObjetosMoviles endp

cicloGeneral proc near
  pushRegisters
    ciclo_CicloGeneral:
		CALL movimientoSokoban
		CMP salir, 1
		JE finalCicloGeneral
    CALL capturadorOpcionesMenu
    
		CALL verificarColisiones
		CALL imprimirMatrizVideo
    CALL retardadorPantalla
    CALL printEstadisticas
    CALL retardadorPantallaEstadisticas
    CALL revisarGaneNivel
		JMP ciclo_CicloGeneral
  finalCicloGeneral:
  popRegisters
  ret
cicloGeneral endp

resetNivel proc near
  pushRegisters
  XOR ax, ax 
  
  MOV word ptr contadores[0], ax
  MOV word ptr contadores[2], ax
  ;MOV word ptr contadores[4], ax

  MOV cx, 4
    lea si, msgBlankContadores
    lea di, msgNivel
    ADD di, 8
    CALL copyStringSIDI
    lea si, msgBlankContadores
    lea di, msgPasos
    ADD di, 8
    CALL copyStringSIDI
    lea si, msgBlankContadores
    lea di, msgPush
    ADD di, 10
    CALL copyStringSIDI
    lea si, msgBlankContadores
    lea di, msgNivelesResueltos
    ADD di, 20
    CALL copyStringSIDI

  CALL cargaDeNiveles  

 ; CALL cambiarMatrizLeida

  CALL buscarAKirstein

  CALL hacerMatrizSinObjetosMoviles

  
  finalResetNivel:

  popRegisters
  ret
resetNivel endp

makeEstadisticas proc near
  pushRegisters
    ;[+7],  [+9],   [+19] donde poner los numeros en str
  ;pasos, push, nivelesConcluidos
  ;contadores mismo orden
  XOR ax, ax
  lea si, msgNivel
  ADD si, 8
  MOV ax, numNivelRaw
  CALL printAxToSi

  XOR ax, ax
  lea si, msgPasos
  ADD si, 8
  MOV ax, word ptr contadores[0]
  CALL printAxToSi

  XOR ax, ax
  lea si, msgPush
  ADD si, 10
  MOV ax, word ptr contadores[2]
  CALL printAxToSi

  XOR ax, ax
  lea si, msgNivelesResueltos
  ADD si, 20
  MOV ax, word ptr contadores[4]
  CALL printAxToSi

  popRegisters
  ret
makeEstadisticas endp

printEstadisticas proc near
	pushRegisters
  CALL makeEstadisticas
  ;dh, dl fila y columna
  ;si puntero a la str
  lea si, msgPasos
  MOV dh, 1
  MOV dl, 51
  CALL imprimirStrVideo
  
  lea si, msgPush
  MOV dh, 2
  MOV dl, 51
  CALL imprimirStrVideo
  
  lea si, msgNivel
  MOV dh, 3
  MOV dl, 51
  CALL imprimirStrVideo

  lea si, msgNivelesResueltos
  MOV dh, 4
  MOV dl, 51
  CALL imprimirStrVideo

  lea si, msgTecladoSigNivel
  MOV dh, 6
  MOV dl, 51
  CALL imprimirStrVideo
  
  lea si, msgTecladoAntNivel
  MOV dh, 8
  MOV dl, 51
  CALL imprimirStrVideo

  lea si, msgTecladoHighScore
  MOV dh, 10
  MOV dl, 51
  CALL imprimirStrVideo

  lea si, msgTecladoAcerdaDe
  MOV dh, 12
  MOV dl, 51
  CALL imprimirStrVideo

  lea si, msgTecladoAyuda
  MOV dh, 21
  MOV dl, 0
  CALL imprimirStrVideo
  
  lea si, msgTecladoReset
  MOV dh, 21
  MOV dl, 15
  CALL imprimirStrVideo

  lea si, msgTecladoSalir
  MOV dh, 21
  MOV dl, 30
  CALL imprimirStrVideo

	popRegisters
	ret
printEstadisticas endp

imprimirStrVideo proc near
  ;este metodo imprime un string en una parte especifica de
  ;la pantalla, se recibe la fila y columna en dh, dl
  ;se recibe en el si el puntero al string like PASCAL
  pushRegisters

  MOV al, 80
  MUL dh
  shl ax, 1
  XOR dh, dh
  shl dx, 1
  ADD ax, dx
  mov di, ax ;posicion en la pantalla

  mov  ax,0B800h
  mov  es,ax

  mov  cl, byte ptr[si]
  xor ch, ch
  inc si

  cicloImprimirStrVideo:    ;Bucle que se encargara de pintar la string
      mov  al, [si]        ;caracteres de la pantalla para limpiarla
      mov  ah, 3         ;Fondo azul, letras blancas
      ;
      mov  es:[di], ax
      inc  si             ;Pasamos a apuntar a la siguiente letra del saludo
      inc  di
      inc  di

      LOOP cicloImprimirStrVideo
  popRegisters
  ret
  ret
imprimirStrVideo endp

retardadorPantallaEstadisticas proc near
  pushRegisters
  MOV cx, 10
  ciclo_RetardadorPantallaEstadisticas:
    PUSH cx
      MOV cx, 16144
      retardadorPantallaEstadisticasCiclo2:

      loop retardadorPantallaEstadisticasCiclo2
      POP CX
  LOOP ciclo_RetardadorPantallaEstadisticas
  popRegisters
  ret
retardadorPantallaEstadisticas endp

capturadorOpcionesMenu proc near
  pushRegisters
  CMP teclaGuardada, 0
  JNE hayTeclaGuardada
  JMP noPresionoTeclaAyudas
  hayTeclaGuardada:
  MOV al, teclaGuardada
  ;al ->
  ;ayuda  = H
  ;acercaDe = M
  ;reset = R
  ;highscores = N
  ;sig y ant nivel = S, A
    OR al, 32; hacer mayuscula
    CMP al, 'h'
    JNE noPresionoAyuda
    JMP presionoAyuda
    noPresionoAyuda:
      CMP al, 'm'
      JNE noPresionoAcercaDe
      JMP presionoAcercaDe
      noPresionoAcercaDe:
        CMP al, 'r'
        JNE noPresionoReset
        JMP presionoReset
        noPresionoReset:    
          CMP al, 'n'
          JNE noPresionoHighScores
          JMP presionoHighScores
          noPresionoHighScores:
            CMP al, 's'
            JNE noPresionoSigNivel
            JMP presionoSigNivel 
            noPresionoSigNivel:
              CMP al, 'a'
              JNE noPresionoAntNivel
              JMP presionoAntNivel 
              noPresionoAntNivel:
                JMP finalCapturadorOpcionesMenu
    
    presionoAyuda:
      ;se recibe en el si el puntero a la matriz 
      ;en el dh y dl se recibe filas y columnas 
      ;en el CH se recibe la cantidad de filas
      ;en el CL la cantidad de columnas
      MOV dx, 0403h
      MOV ch, 12
      MOV cl, 44
      lea si, msgVideoAyuda
      call printMatrizEnVideoGeneric
      call pressEnterContinueEco
      JMP finalCapturadorOpcionesMenu
    presionoAcercaDe:
      MOV dx, 0403h
      MOV ch, 12
      MOV cl, 44
      lea si, msgVideoAcercaDe
      call printMatrizEnVideoGeneric
      call pressEnterContinueEco
      JMP finalCapturadorOpcionesMenu
    presionoReset:
      CALL resetNivel
      JMP finalCapturadorOpcionesMenu
    presionoHighScores:
      
      JMP finalCapturadorOpcionesMenu
    presionoSigNivel:
      INC numNivelRaw
      CALL resetNivel
      JMP finalCapturadorOpcionesMenu
    presionoAntNivel:
      CMP numNivelRaw, 0
      JE nivelEsCero
      DEC numNivelRaw
      CALL resetNivel
      nivelEsCero:
      JMP finalCapturadorOpcionesMenu
  noPresionoTeclaAyudas:
  
  finalCapturadorOpcionesMenu:

  popRegisters
  ret
capturadorOpcionesMenu endp

printMatrizEnVideoGeneric proc near
  ;se recibe en el si el puntero a la matriz 
  ;en el dh y dl se recibe filas y columnas 
  ;en el CH se recibe la cantidad de filas
  ;en el CL la cantidad de columnas
  pushRegisters
  XOR bx, bx
  MOV var, si;guardar el inicio de la matriz
  ciclo_PrintMatrizEnVideoGeneric:
    MOV si, var
    XOR ax, ax;calcular el numero de fila
    or al, bl
    MUL cl ; tamaño de columnas
    ADD si, ax; sumar el numero de fila correspondiente
    MOV ah, bl
    ADD ah, dh
    
    pushRegisters
      ;al, cantidad de columnas
      ;ah, numero de fila
      ;dl, numero de columna
      MOV al, 80
      MUL ah
      XOR dh, dh
      ADD ax, dx
      shl ax, 1
      mov di, ax ;posicion en la pantalla

      mov  ax,0B800h
      mov  es,ax

      mov  cl, byte ptr[si]
      xor ch, ch
      inc si

      cicloPrintFilaEnVideoGeneric:    ;Bucle que se encargara de pintar la string
          mov  al, [si]        ;caracteres de la pantalla para limpiarla
          mov  ah, 14          ;Fondo azul, letras blancas
          OR ah, 10h
          mov  es:[di], ax
          inc  si             ;Pasamos a apuntar a la siguiente letra del saludo
          inc  di
          inc  di

      LOOP cicloPrintFilaEnVideoGeneric
      popRegisters
    INC bl
  CMP bl, ch
  JB ciclo_PrintMatrizEnVideoGeneric


  popRegisters
  ret
printMatrizEnVideoGeneric endp

printErrorNivelVideo proc near
  pushRegisters
  CALL limpiarPantalla
  ;Mover a la fila num 8 de msgVideoErrorNivel
  ;el nivel con error 
  push ds
  pop es
  cld
  ;Mover el Niveles\sokoXXX.txt
  lea si, pathLectura
  ADD si, punteroPathLectura
  lea di, msgVideoErrNivel
  MOV ah, 8
  MOV al, 44
  MUL ah
  ADD di, ax
  ADD di, 7
  ADD di, 2
  mov dh, nombreDrive
  MOV byte ptr [di], dh
  MOV byte ptr [di+1], ':'
  MOV byte ptr [di+2], '\'
  MOV byte ptr [di+3], '.'
  MOV byte ptr [di+4], '.'
  ADD di, 5
  MOV cx, 20
  REP MOVSB

  MOV dx, 0403h
  MOV ch, 12
  MOV cl, 44
  lea si, msgVideoErrNivel
  call printMatrizEnVideoGeneric
  call pressEnterContinueEco
  popRegisters
  ret
printErrorNivelVideo endp

getDriveName proc near
  pushRegisters
  cicloGetDriveName:
    printENTER
    MOV ah, 09h
    lea dx, msgDigiteDrive
    int 21h
    MOV ah, 01H
    int 21h
    JZ cicloGetDriveName
    XOR al, 20h
    CMP al, 'A'
    JB cicloGetDriveName
    CMP al, 'Z'
    JA cicloGetDriveName
    MOV nombreDrive, al
  popRegisters
  ret
getDriveName endp

copyStringSIDI proc near
  ;en el di y si vienen los string y en el cx el largo
  pushRegisters
  push ds
  pop es
  cld
  
  REP MOVSB
  popRegisters
  ret
copyStringSIDI endp

revisarGaneNivel proc near
  pushRegisters
  MOV cajasEntregadas, 0
  cicloExternoRevisarGaneNivel:
    lea si, matrizSokoban
    MOV al, bl
    MUL M
    ADD si, ax
    MOV cl, byte ptr[si]
    XOR ch, ch
    cicloInternoRevisarGaneNivel:
      PUSH cx
      lea si, matrizSoloNivel
      MOV ch, bl
      DEC cl ;decrementar porque esta basado en 0 el index
      CALL axezador ;tenemos el elemento de la matriz sin limpia
      CMP al, zonaEntrega;si es una zona de entrega entonces revisar
      JE revisarSiHayCaja;si tiene una caja en esa posicion en la matrizSokoban
      JMP condicionCicloInternoRevisarGaneNivel
      revisarSiHayCaja:
        lea si, matrizSokoban
        CALL axezador
        CMP al, caja
        JE sumarCajaEnPos
        JMP noHayCaja
        sumarCajaEnPos:
          INC cajasEntregadas
        noHayCaja:
      condicionCicloInternoRevisarGaneNivel:
      POP cx
      LOOP cicloInternoRevisarGaneNivel
  INC bl
  CMP bl, numFilasCargadas
  JB cicloExternoRevisarGaneNivel
  MOV al, numCajas
  CMP cajasEntregadas, al
  JNE noHaGanado
    INC word ptr contadores[4]
    INC numNivelRaw
    CALL resetNivel
  noHaGanado:
  popRegisters
  ret
revisarGaneNivel endp

cambiarMatrizLeida proc near
  ;este procedimiento cambia la matriz leida a una
  ;matriz adaptada
  pushRegisters
  XOR bx, bx
  cicloExternoCambiarMatrizLeida:
    lea si, matrizSokoban
    MOV al, bl
    MUL M
    ADD si, ax
    MOV cl, byte ptr[si]
    XOR ch, ch
    cicloInternoCambiarMatrizLeida:
      PUSH cx
      lea si, matrizSokoban
      MOV ch, bl
      DEC cl ;decrementar porque esta basado en 0 el index
      CALL axezador 
      CMP al, 'M'
      JE cambiarMuro
      JMP noCambiarMuro
      cambiarMuro:
        JMP condicionCicloInternoRevisarGaneNivel
        noCambiarMuro:
        CMP al, '.'
        JE cambiarEspacio
        JMP noCambiarEspacio
        cambiarEspacio:
          
          MOV al, espacio
          lea si, matrizSokoban
          CALL almacenador

          JMP condicionCicloInternoRevisarGaneNivel
          noCambiarEspacio:
          CMP al, 'X'
          JE cambiarCaja
          JMP noCambiarCaja
          cambiarCaja:
          JMP condicionCicloInternoRevisarGaneNivel
          noCambiarCaja:
            CMP al, 'O'
            JE cambiarZonaEntrega
            JMP noCambiarZonaEntrega
            cambiarZonaEntrega:
            JMP condicionCicloInternoRevisarGaneNivel
            noCambiarZonaEntrega:

      POP cx
      LOOP cicloInternoCambiarMatrizLeida
  INC bl
  CMP bl, numFilasCargadas
  JB cicloExternoCambiarMatrizLeida

  popRegisters
  ret
  ret
cambiarMatrizLeida endp

  inicio: 
	  
	mov ax, datos
	mov ds, ax
	mov ax, pila
	mov ss, ax
	

  CALL getDriveName
	CALL resetNivel
  CALL cicloGeneral
  mov  ax,4C00h
	int  21h

codigo ends

end inicio