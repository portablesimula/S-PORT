NAME       EDRAW
; ---------------------------------------------------------------
; ---  COPYRIGHT 1989 by                                      ---
; ---  Simula a.s.                                            ---
; ---  Oslo, Norway                                           ---
; ---                                                         ---
; ---             P O R T A B L E     S I M U L A             ---
; ---                   F O R    I B M   P C                  ---
; ---                                                         ---
; ---       T  H  E     E  N  V  I  R  O  N  M  E  N  T       ---
; ---------------------------------------------------------------
INCLUDE  macro.def

IF iAPX286
   IF WITH87
   ELSE
             EXTRN    E@SC2LR:FAR
             EXTRN    E@LRSUB:FAR
   ENDIF
ENDIF


; ************************************************
; ******    L  O  C  A  L     D  A  T  A    ******
; ************************************************
_DATA        SEGMENT
REPS         DD  00800000H             ;  addeps(0.0&0)
REPN         DD  80800000H             ;  subeps(0.0&0)
DEPS         DQ  0010000000000000H     ;  addeps(0.0&&0)
DEPN         DQ  8010000000000000H     ;  subeps(0.0&&0)
_DATA        ENDS


; ******************************************************
; ******    C  O  D  E     S  E  C  T  I  O  N    ******
; ******************************************************
IF iAPX286
S@ENV_TEXT SEGMENT
      ASSUME  CS:S@ENV_TEXT
      ASSUME  DS:NOTHING
ELSE ; I.E. iAPX386
_TEXT SEGMENT
ENDIF


; **********************************************************
; ************    B A S I C    D R A W I N G    ************
; **********************************************************

IF iAPX286
  SIMROUTINE E@DRAWRP
; **** DS:DI <-- Address of Seed
       MOV    AX,FSTARG+4
       MOV    DS,AX
       ASSUME DS:NOTHING
       MOV    AX,FSTARG
       ADD    AX,FSTARG+2
       MOV    DI,AX
; **** Test Normal or Antithetic Drawing
       MOV    AX,[DI]+2
       OR     AX,AX
       JS     ANTI
; ****                                 ****
; **** Normal Drawing -- Positive Seed ****
; ****                                 ****
; **** Calculate:  DX:AX:BX:CX <-- Seed * 455470314
; **** Note: 455470314 = 1B25ECEA Hex
       MOV    AX,0ECEAH        ; Const'LOW
       MOV    SI,WORD PTR [DI] ; Seed'LOW
       MUL    SI
       MOV    CX,AX
       MOV    BX,DX
       MOV    AX,01B25H        ; Const'HIGH
       MUL    SI
       ADD    BX,AX
       ADC    DX,0
       MOV    SI,DX
       MOV    AX,0ECEAH        ; Const'LOW
       MUL    WORD PTR [DI]+2  ; Seed'HIGH
       ADD    BX,AX
       ADC    SI,DX
       MOV    AX,01B25H        ; Const'HIGH
       MUL    WORD PTR [DI]+2  ; Seed'HIGH
       ADD    AX,SI
       ADC    DX,0
; **** DX:AX <-- DX:AX * 2
       CLC
       RCL    AX,1
       RCL    DX,1
; **** DX:AX <-- DX:AX:BX:CX mod (2**31)-1
       OR     BX,BX
       JGE    NL1
       ADD    AX,1
       ADC    DX,0
NL1:   AND    BX,7FFFH
       SUB    CX,0FFFFH
       SBB    BX,7FFFH
       ADD    AX,CX
       ADC    DX,BX
       OR     DX,DX
       JGE    NL2
       ADD    AX,0FFFFH
       ADC    DX,7FFFH
NL2:
; **** Store Next Seed
       MOV    [DI]+2,DX
       MOV    [DI],AX
; **** Convert to Floating
   IF WITH87
       MOV    AX,-31
       PUSH   AX
       FILD   WORD PTR [BP]-2
       FILD   DWORD PTR [DI]
       FSCALE
       POP    SI
       FSTP   ST(1)
     IFNDEF X87
       FSTP   QWORD PTR FSTARG+6   ; Export value
       WAIT
     ENDIF
   ELSE
       CALL   NEAR PTR E@SC2LR
       MOV    FSTARG+12,BX
       MOV    FSTARG+10,CX
       MOV    FSTARG+8,DX
       MOV    FSTARG+6,AX
   ENDIF

; **** Exit
       MOV    SP,BP
       POP    BP
       RET    6
; ****                                     ****
; **** Antithetic Drawing -- Negative Seed ****
; ****                                     ****
; **** Seed <--  - Seed
ANTI:  MOV    DX,[DI]+2
       MOV    AX,[DI]
       NEG    AX
       ADC    DX,0
       NEG    DX
       MOV    [DI],AX
       MOV    [DI]+2,DX
; **** Calculate:  DX:AX:BX:CX <-- Seed * 455470314
; **** Note: 455470314 = 1B25ECEA Hex
       MOV    AX,0ECEAH        ; Const'LOW
       MOV    SI,WORD PTR [DI] ; Seed'LOW
       MUL    SI
       MOV    CX,AX
       MOV    BX,DX
       MOV    AX,01B25H        ; Const'HIGH
       MUL    SI
       ADD    BX,AX
       ADC    DX,0
       MOV    SI,DX
       MOV    AX,0ECEAH        ; Const'LOW
       MUL    WORD PTR [DI]+2  ; Seed'HIGH
       ADD    BX,AX
       ADC    SI,DX
       MOV    AX,01B25H        ; Const'HIGH
       MUL    WORD PTR [DI]+2  ; Seed'HIGH
       ADD    AX,SI
       ADC    DX,0
; **** DX:AX <-- DX:AX * 2
       CLC
       RCL    AX,1
       RCL    DX,1
; **** DX:AX <-- DX:AX:BX:CX mod (2**31)-1
       OR     BX,BX
       JGE    AL1
       ADD    AX,1
       ADC    DX,0
AL1:   AND    BX,7FFFH
       SUB    CX,0FFFFH
       SBB    BX,7FFFH
       ADD    AX,CX
       ADC    DX,BX
       OR     DX,DX
       JGE    AL2
       ADD    AX,0FFFFH
       ADC    DX,7FFFH
AL2:
; **** Store Next Seed
       NEG    AX
       ADC    DX,0
       NEG    DX
       MOV    [DI]+2,DX
       MOV    [DI],AX
; **** Convert to Floating
   IF WITH87
       MOV    AX,-31
       PUSH   AX
       FILD   WORD PTR [BP]-2
       FILD   DWORD PTR [DI]
       FABS
       FSCALE
       FLD1
       FSUBRP ST(1),ST
       POP    SI
       FSTP   ST(1)
     IFNDEF X87
       FSTP   QWORD PTR FSTARG+6   ; Export value
       WAIT
     ENDIF
   ELSE
       NEG    AX
       ADC    DX,0
       NEG    DX
       CALL   NEAR PTR E@SC2LR
       MOV    SI,3FF0H  ; Push  1.0&&0
       PUSH   SI
       MOV    SI,0
       PUSH   SI
       PUSH   SI
       PUSH   SI
       PUSH   BX       ; Push abs(NewSeed)*(2** -31)
       PUSH   CX
       PUSH   DX
       PUSH   AX
       CALL   NEAR PTR E@LRSUB
       MOV    FSTARG+12,BX
       MOV    FSTARG+10,CX
       MOV    FSTARG+8,DX
       MOV    FSTARG+6,AX
   ENDIF
  ENDROUTINE E@DRAWRP,6

ELSE ; I.E. iAPX386
  SIMROUTINE E@DRAWRP
; **** EDI <-- Address of Seed
       MOV    EAX,FSTARG+4
       ADD    EAX,FSTARG
       MOV    EDI,EAX
; **** Test Normal or Antithetic Drawing
       MOV    EAX,[EDI]
       OR     EAX,EAX
       JS     SHORT ANTI
; ****                                 ****
; **** Normal Drawing -- Positive Seed ****
; ****                                 ****
; **** Calculate:  EDX:EAX <-- Seed * 455470314
; **** Note: 455470314 = 1B25ECEA Hex
       MOV    ECX,455470314
       MUL    ECX
; **** EDX <-- EDX * 2
       SHL    EDX,1
; **** EDX:EAX <-- EDX:EAX mod (2**31)-1
       OR     EAX,EAX
       JGE    SHORT NL1
       ADD    EDX,1
NL1:   AND    EAX,7FFFFFFFH
       SUB    EAX,7FFFFFFFH
       ADD    EAX,EDX
       OR     EAX,EAX
       JGE    SHORT NL2
       ADD    EAX,7FFFFFFFH
NL2:
; **** Store Next Seed
       MOV    [EDI],EAX
; **** Convert to Floating
   IF WITH87
       PUSH   -31
       FILD   DWORD PTR [EBP]-4
       FILD   DWORD PTR [EDI]
       FSCALE
       POP    ESI
       FSTP   ST(1)
       FSTP   QWORD PTR FSTARG+8   ; Export value
       WAIT
   ELSE
      IF WTL167
       MOV    EAX,DWORD PTR [EDI]
       WFLOAT <wd2>,EAX
       WFST   EAX,<ws2>
       SUB    EAX,01F00000H ; 31 SHL 20
       WFLD   <ws2>,EAX
       ; ***
       WFST   EAX,<ws2>
       MOV    DWORD PTR FSTARG+12,EAX  ; Export value'HI
       WFST   EAX,<ws2>
       MOV    DWORD PTR FSTARG+8,EAX   ; Export value'LO
       ; ***
      ELSE
          .ERR
      ENDIF
   ENDIF
; **** Exit
       MOV    ESP,EBP
       POP    EBP
       RET    8
; ****                                     ****
; **** Antithetic Drawing -- Negative Seed ****
; ****                                     ****
; **** Seed <--  - Seed
ANTI:  NEG    EAX
       MOV    [EDI],EAX
; **** Calculate:  EDX:EAX <-- Seed * 455470314
; **** Note: 455470314 = 1B25ECEA Hex
       MOV    ECX,455470314
       MUL    ECX
; **** EDX <-- EDX * 2
       SHL    EDX,1
; **** EDX:EAX <-- EDX:EAX mod (2**31)-1
       OR     EAX,EAX
       JGE    SHORT AL1
       ADD    EDX,1
AL1:   AND    EAX,7FFFFFFFH
       SUB    EAX,7FFFFFFFH
       ADD    EAX,EDX
       OR     EAX,EAX
       JGE    SHORT AL2
       ADD    EAX,7FFFFFFFH
AL2:
; **** Store Next Seed
       NEG    EAX
       MOV    [EDI],EAX
; **** Convert to Floating
   IF WITH87
       PUSH   -31
       FILD   DWORD PTR [EBP]-4
       FILD   DWORD PTR [EDI]
       FABS
       FSCALE
       FLD1
       FSUBRP ST(1),ST
       POP    ESI
       FSTP    ST(1)
       FSTP   QWORD PTR FSTARG+8   ; Export value
       WAIT
   ELSE
      IF WTL167
       MOV    EAX,DWORD PTR [EDI]
       WFLOAT <wd2>,EAX
       WFABS  <wd2>,<wd2>
       WFST   EAX,<ws2>
       SUB    EAX,01F00000H ; 31 SHL 20
       WFLD   <ws2>,EAX
       WFLOAT <wd4>,1
       WFSUBR <wd2>,<wd4>
       ; ***
       WFST   EAX,<ws2>
       MOV    DWORD PTR FSTARG+12,EAX  ; Export value'HI
       WFST   EAX,<ws2>
       MOV    DWORD PTR FSTARG+8,EAX   ; Export value'LO
       ; ***
      ELSE
       .ERR
      ENDIF
   ENDIF
  ENDROUTINE E@DRAWRP,8
E@DRAWRP  ENDP
ENDIF



; ---------------------------------------------------------- real addeps
; Visible known("RADDEP") RADDEPS;
; import real arg; export real val;
; begin  val := arg + <small number>  end;
;
; Result returned on Stack.

  SIMROUTINE E@RADDEP
IF iAPX286
          MOV     DX,FSTARG+2
          OR      DX,DX
          JNZ     RADD1
          MOV     AX,WORD PTR SS:REPS
          MOV     DX,WORD PTR SS:REPS+2 ; 0+eps = 2 ** -23
          JMP     SHORT RADDE
RADD1:    MOV     AX,FSTARG          ; DX:AX = short argument
          JS      SHORT RADD2
          ADD     AX,1
          ADC     DX,0              ; 32-bit add
          JMP     SHORT RADD3
RADD2:    SUB     AX,1
          SBB     DX,0              ; 32-bit sub
RADD3:    MOV     DI,DX
          AND     DI,7F80H
          CMP     DI,7F80H          ; Check for NAN etc.
          JNE     SHORT RADD4
          MOV     AX,FSTARG
          MOV     DX,FSTARG+2
          JMP     SHORT RADDE
RADD4:    CMP     DI,0              ; Check for denormal.
          JNE     SHORT RADDE
          XOR     AX,AX             ; Res:= 0.0
          XOR     DX,DX
RADDE:    MOV     FSTARG+4,AX
          MOV     FSTARG+6,DX
ELSE ; I.E. iAPX386
          MOV     EAX,FSTARG
          OR      EAX,EAX
          JNZ     SHORT RADD1
          MOV     EAX,DWORD PTR REPS ; 0+eps = 2 ** -23
          JMP     SHORT RADDE
RADD1:    JS      SHORT RADD2
          ADD     EAX,1              ; 32-bit add
          JMP     SHORT RADD3
RADD2:    SUB     EAX,1              ; 32-bit sub
RADD3:    MOV     EDI,EAX
          AND     EDI,7F800000H
          CMP     EDI,7F800000H      ; Check for NAN etc.
          JNE     SHORT RADD4
          MOV     EAX,FSTARG
          JMP     SHORT RADDE
RADD4:    CMP     EDI,0              ; Check for denormal.
          JNE     SHORT RADDE
          XOR     EAX,EAX            ; Res:= 0.0
RADDE:    MOV     FSTARG+4,EAX
ENDIF
  ENDROUTINE E@RADDEP,4

; ----------------------------------------------------- long real addeps
; Visible known("DADDEP") DADDEPS;
; import long real arg; export long real val;
; begin  val := arg + <small number>  end;
;
; Result returned on Stack.

  SIMROUTINE E@DADDEP
IF iAPX286
          MOV     DX,FSTARG+6
          OR      DX,DX
          JNZ     DADD1
          MOV     DX,WORD PTR SS:DEPS+6
          MOV     AX,WORD PTR SS:DEPS+4
          MOV     BX,WORD PTR SS:DEPS+2
          MOV     CX,WORD PTR SS:DEPS   ; 0+eps = 2 ** -52
          JMP     SHORT DADDE
DADD1:    MOV     AX,FSTARG+4
          MOV     BX,FSTARG+2
          MOV     CX,FSTARG         ; DX:AX:BX:CX = long argument
          JS      SHORT DADD2
          ADD     CX,1
          ADC     BX,0
          ADC     AX,0
          ADC     DX,0              ; 64-bit add
          JMP     SHORT DADD3
DADD2:    SUB     CX,1
          SBB     BX,0
          SBB     AX,0
          SBB     DX,0              ; 64-bit sub
DADD3:    MOV     DI,DX
          AND     DI,7FF0H
          CMP     DI,7FF0H          ; Check for NAN etc.
          JNE     SHORT DADD4
          MOV     DX,FSTARG+6
          MOV     AX,FSTARG+4
          MOV     BX,FSTARG+2
          MOV     CX,FSTARG         ; DX:AX:BX:CX = long argument
          JMP     SHORT DADDE
DADD4:    CMP     DI,0              ; Check for denormal.
          JNE     SHORT DADDE
          XOR     AX,AX             ; Res := 0.0
          XOR     BX,BX
          XOR     CX,CX
          XOR     DX,DX
DADDE:    MOV     FSTARG+8,CX
          MOV     FSTARG+10,BX
          MOV     FSTARG+12,AX
          MOV     FSTARG+14,DX
ELSE ; I.E. iAPX386
          MOV     EDX,FSTARG+4
          OR      EDX,EDX
          JNZ     SHORT DADD1
          MOV     EDX,DWORD PTR SS:DEPS+4
          MOV     EAX,DWORD PTR SS:DEPS ; 0+eps = 2 ** -52
          JMP     SHORT DADDE
DADD1:    MOV     EAX,FSTARG         ; EDX:EAX = long argument
          OR      EDX,EDX
          JS      SHORT DADD2
          ADD     EAX,1
          ADC     EDX,0              ; 64-bit add
          JMP     SHORT DADD3
DADD2:    SUB     EAX,1
          SBB     EDX,0              ; 64-bit sub
DADD3:    MOV     EDI,EDX
          AND     EDI,7FF00000H
          CMP     EDI,7FF00000H      ; Check for NAN etc.
          JNE     SHORT DADD4
          MOV     EDX,FSTARG+4
          MOV     EAX,FSTARG         ; EDX:EAX = long argument
          JMP     SHORT DADDE
DADD4:    CMP     EDI,0              ; Check for denormal.
          JNE     SHORT DADDE
          XOR     EDX,EDX            ; Res:= 0.0
          XOR     EAX,EAX
DADDE:    MOV     FSTARG+8,EAX
          MOV     FSTARG+12,EDX
ENDIF
  ENDROUTINE E@DADDEP,8


; ---------------------------------------------------------- real subeps
; Visible known("RSUBEP") RSUBEPS;
; import real arg; export real val;
; begin  val := arg - <small number>  end;
;
; Result returned on Stack.

  SIMROUTINE E@RSUBEP
IF iAPX286
          MOV     DX,FSTARG+2
          OR      DX,DX
          JNZ     RSUB1
          MOV     AX,WORD PTR SS:REPN
          MOV     DX,WORD PTR SS:REPN+2 ; 0+eps = -(2 ** -23)
          JMP     SHORT RSUBE
RSUB1:    MOV     AX,FSTARG         ; DX:AX = short argument
          JNS     SHORT RSUB2
          ADD     AX,1
          ADC     DX,0              ; 32-bit add
          JMP     SHORT RSUB3
RSUB2:    SUB     AX,1
          SBB     DX,0              ; 32-bit sub
RSUB3:    MOV     DI,DX
          AND     DI,7F80H
          CMP     DI,7F80H          ; Check for NAN etc.
          JNE     SHORT RSUB4
          MOV     DX,FSTARG+2
          MOV     AX,FSTARG         ; DX:AX = short argument
          JMP     SHORT RSUBE
RSUB4:    CMP     DI,0              ; Check for denormal
          JNE     SHORT RSUBE
          XOR     AX,AX             ; Res:= 0.0
          XOR     DX,DX
RSUBE:    MOV     FSTARG+4,AX
          MOV     FSTARG+6,DX
ELSE ; I.E. iAPX386
          MOV     EAX,FSTARG
          OR      EAX,EAX
          JNZ     SHORT RSUB1
          MOV     EAX,DWORD PTR REPN ; 0-eps = -(2 ** -23)
          JMP     SHORT RSUBE
RSUB1:    JNS     SHORT RSUB2
          ADD     EAX,1              ; 32-bit add
          JMP     SHORT RSUB3
RSUB2:    SUB     EAX,1              ; 32-bit sub
RSUB3:    MOV     EDI,EAX
          AND     EDI,7F800000H
          CMP     EDI,7F800000H      ; Check for NAN etc.
          JNE     SHORT RSUB4
          MOV     EAX,FSTARG
          JMP     SHORT RSUBE
RSUB4:    CMP     EDI,0              ; Check for denormal
          JNE     SHORT RSUBE
          XOR     EAX,EAX            ; RES:= 0.0
RSUBE:    MOV     FSTARG+4,EAX
ENDIF
  ENDROUTINE E@RSUBEP,4


; ----------------------------------------------------- long real subeps
; Visible known("DSUBEP") DSUBEPS;
; import long real arg; export long real val;
; begin  val := arg - <small number>  end;
;
; Result returned on Stack.

  SIMROUTINE E@DSUBEP
IF iAPX286
          MOV     DX,FSTARG+6
          OR      DX,DX
          JNZ     DSUB1
          MOV     DX,WORD PTR SS:DEPN+6
          MOV     AX,WORD PTR SS:DEPN+4
          MOV     BX,WORD PTR SS:DEPN+2
          MOV     CX,WORD PTR SS:DEPN   ; 0+eps = -(2 ** -52)
          JMP     SHORT DSUBE
DSUB1:    MOV     AX,FSTARG+4
          MOV     BX,FSTARG+2
          MOV     CX,FSTARG         ; DX:AX:BX:CX = long argument
          JNS     SHORT DSUB2
          ADD     CX,1
          ADC     BX,0
          ADC     AX,0
          ADC     DX,0              ; 64-bit add
          JMP     SHORT DSUB3
DSUB2:    SUB     CX,1
          SBB     BX,0
          SBB     AX,0
          SBB     DX,0              ; 64-bit sub
DSUB3:    MOV     DI,DX
          AND     DI,7FF0H
          CMP     DI,7FF0H          ; Check for NAN etc.
          JNE     SHORT DSUB4
          MOV     DX,FSTARG+6
          MOV     AX,FSTARG+4
          MOV     BX,FSTARG+2
          MOV     CX,FSTARG         ; DX:AX:BX:CX = long argument
          JMP     SHORT DSUBE
DSUB4:    CMP     DI,0              ; Check for denormal
          JNE     SHORT DSUBE
          XOR     DX,DX             ; Res:= 0.0
          XOR     AX,AX
          XOR     BX,BX
          XOR     CX,CX
DSUBE:    MOV     FSTARG+8,CX
          MOV     FSTARG+10,BX
          MOV     FSTARG+12,AX
          MOV     FSTARG+14,DX
ELSE ; I.E. iAPX386
          MOV     EDX,FSTARG+4
          OR      EDX,EDX
          JNZ     SHORT DSUB1
          MOV     EDX,DWORD PTR DEPN+4
          MOV     EAX,DWORD PTR DEPN   ; 0+eps = -(2 ** -52)
          JMP     SHORT DSUBE
DSUB1:    MOV     EAX,FSTARG
          OR      EDX,EDX
          JNS     SHORT DSUB2
          ADD     EAX,1
          ADC     EDX,0              ; 64-bit add
          JMP     SHORT DSUB3
DSUB2:    SUB     EAX,1
          SBB     EDX,0              ; 64-bit sub
DSUB3:    MOV     EDI,EDX
          AND     EDI,7FF00000H
          CMP     EDI,7FF00000H      ; Check for NAN etc.
          JNE     SHORT DSUB4
          MOV     EDX,FSTARG+4
          MOV     EAX,FSTARG         ; EDX:EAX = long argument
          JMP     SHORT DSUBE
DSUB4:    CMP     EDI,0              ; Check for denormal.
          JNE     SHORT DSUBE
          XOR     EDX,EDX            ; Res:= 0.0
          XOR     EAX,EAX
DSUBE:    MOV     FSTARG+8,EAX
          MOV     FSTARG+12,EDX
ENDIF
  ENDROUTINE E@DSUBEP,8

          EVEN
IF iAPX286
S@ENV_TEXT ENDS
ELSE ; I.E. iAPX386
_TEXT ENDS
ENDIF
      END
