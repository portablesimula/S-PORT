NAME       EMUFP
; ---------------------------------------------------------------
; ---  COPYRIGHT 1989 by                                      ---
; ---  Simula a.s.                                            ---
; ---  Oslo, Norway                                           ---
; ---                                                         ---
; ---                                                         ---
; ---             P O R T A B L E     S I M U L A             ---
; ---                                                         ---
; ---              F O R    i A P X    8 0 x 8 6              ---
; ---                                                         ---
; ---                                                         ---
; ---       T  H  E     E  N  V  I  R  O  N  M  E  N  T       ---
; ---                                                         ---
; ---------------------------------------------------------------

INCLUDE   macro.def

MOVWD     MACRO  P0,P1,P2
          MOV    AX,P0+P1
          MOV    P0+P2,AX
          ENDM

SHLONCE   MACRO  P0                     ; Shift area one position left
          PUSH   AX
          RCL    P0+6,1
          RCL    P0+4,1
          RCL    P0+2,1
          RCL    P0,1
          LAHF
          AND    AH,1
          MOV    SHFLC,AH               ; Save shift carry for testing
          POP    AX
          ENDM

SHRONCE   MACRO  P0                     ; Shift area one position right
          PUSH   AX
          CLC
          RCR    P0,1
          RCR    P0+2,1
          RCR    P0+4,1
          RCR    P0+6,1
          LAHF
          AND    AH,1
          MOV    SHFLC,AH               ; Save shift carry for testing
          POP    AX
          ENDM

SHFWL     MACRO  P0,P1,P2               ; Shift area CL positions left
          MOV    DX,P0+P1               ; CH asumed as 16-CL.
          SHL    DX,CL
          XCHG   CH,CL
          MOV    AX,P0+P2
          SHR    AX,CL
          OR     AX,DX
          MOV    P0+P1,AX
          XCHG   CH,CL
          ENDM

SHFWR     MACRO  P0,P1,P2               ; Shift area CL positions right
          MOV    DX,P0+P1               ; CH asumed as 16-CL.
          SHR    DX,CL
          XCHG   CH,CL
          MOV    AX,P0+P2
          SHL    AX,CL
          OR     AX,DX
          MOV    P0+P1,AX
          XCHG   CH,CL
          ENDM

INCADD    MACRO  P0,REG                 ; inc0:= inc0*4 + SHR(P0,14);
          MOV    REG,INC0
          SHL    REG,1
          SHL    REG,1
          MOV    CL,14
          MOV    AX,P0
          SHR    AX,CL
          ADD    REG,AX
          MOV    INC0,REG
          ENDM

MANDD     MACRO  MN,REG                 ; manx:= manx*4;
          MOV    REG,MN
          SHL    REG,1
          SHL    REG,1
          MOV    MN,REG
          ENDM

N0M1DA1   MACRO                         ; n0:= (n0-1)*2+1;
          DEC    N0
          SHL    N0,1                   ; NB: N0 EQU SI
          INC    N0
          ENDM

          EXTRN  K@ERROR:FAR
BIAS      EQU 3FFFH
SBIAS     EQU 3F80H
DBIAS     EQU 3FF0H
MAXRE2IN  EQU 4EFFH
MAXLR2IN  EQU 41DFH
MAXLR2RE  EQU 47EFH

; ************************************************
; ******    L  O  C  A  L     D  A  T  A    ******
; ************************************************
_DATA     SEGMENT       WORD PUBLIC 'DATA'
F1EXP     DW 0
F1MAN     DW 4 DUP(0)
F2EXP     DW 0
F2MAN     DW 4 DUP(0)
F3EXP     DW 0
F3MAN     DW 4 DUP(0)
F1SIGN    DB 0
F2SIGN    DB 0
F3SIGN    DB 0
SHFLC     DB 0
ISIGN     DB 0
          EVEN
; INC0       DW 0
INC0      EQU DI
INC1      DW 0
INC2      DW 0
INC3      DW 0
; N0        DW 0
N0        EQU SI
N1        DW 0
N2        DW 0
N3        DW 0
CNT       DW 0
EXP       DW 0
MAN0      DW 0
MAN1      DW 0
MAN2      DW 0
MAN3      DW 0
RMN1      DW 0
RMN2      DW 0
RMN3      DW 0
RMN4      DW 0
RTMP0      DW 0
RTMP1     DW 0
RTMP2     DW 0
RTMP3     DW 0

_DATA     ENDS


; ******************************************************
; ******    C  O  D  E     S  E  C  T  I  O  N    ******
; ******************************************************
S@ENV_TEXT SEGMENT
           ASSUME  CS:S@ENV_TEXT
           ASSUME  DS:NOTHING

  SIMROUTINE E@RENEG ; (DX:AX) := - (DX:AX)
           OR      DX,DX
           JE      RPG     ; No negative zero
           XOR     DH,80H  ; Change Sign-bit
RPG:
  ENDROUTINE E@RENEG

  SIMROUTINE E@LRNEG            ; (BX:CX:DX:AX) := - QWORD PTR FSTARG
           MOV     AX,FSTARG
           MOV     DX,FSTARG+2
           MOV     CX,FSTARG+4
           MOV     BX,FSTARG+6
           OR      BX,BX   ; No negative zero
           JE      LPG
           XOR     BH,80H  ; Change Sign-bit
LPG:
  ENDROUTINE E@LRNEG,8

  SIMROUTINE E@READD            ; (DX:AX) := (DX:AX) + (BX:CX)
           OR     BX,BX
           JE     F2ZAS
           OR     DX,DX
           JNE    F1NZAS
           MOV    DX,BX
           MOV    AX,CX
           JMP    SHORT F2ZAS
F1NZAS:    CALL   S2PARM        ; Move in 2nd parameter, short
           CALL   S1PARM        ; Move in 1st parameter, short
           CALL   ADDFL         ; Addition in 64 bits mantissa
           CALL   SRES          ; Store short result
F2ZAS:
  ENDROUTINE E@READD

  SIMROUTINE E@LRADD
; ***      (BX:CX:DX:AX) := QWORD PTR FSTARG+8 + QWORD PTR FSTARG
           CALL   D2PARM        ; Move in 2nd parameter, double
           CALL   D1PARM        ; Move in 1st parameter, double
           MOV    AX,F2EXP
           OR     AX,AX
           JE     F2ZAD
           MOV    AX,F1EXP
           OR     AX,AX
           JNE    F1NZAD
           CALL   MOVF2F1       ; F1:= F2
           JMP    SHORT F2ZAD
F1NZAD:    CALL   ADDFL         ; Addition in 64 bits mantissa
F2ZAD:     CALL   DRES          ; Store double result
  ENDROUTINE E@LRADD,16

  SIMROUTINE E@RESUB
; ***      (DX:AX) := (DX:AX) - (BX:CX)
           OR     BX,BX
           JZ     F2ZSS
           OR     DX,DX
           JNE    F1NZSS
           MOV    DX,BX
           MOV    AX,CX
           XOR    DH,80H
           JMP    SHORT F2ZSS
F1NZSS:    CALL   S2PARM        ; Move in 2nd parameter, short
           CALL   S1PARM        ; Move in 1st parameter, short
           MOV    AL,F2SIGN
           XOR    AL,80H
           MOV    F2SIGN,AL     ; F2SIGN:= - F2SIGN
           CALL   ADDFL         ; Addition in 64 bits mantissa
           CALL   SRES          ; Store short result
F2ZSS:
  ENDROUTINE E@RESUB

  SIMROUTINE E@LRSUB
; ***      (BX:CX:DX:AX) := QWORD PTR FSTARG+8 - QWORD PTR FSTARG
           CALL   D2PARM        ; Move in 2nd parameter, short
           CALL   D1PARM        ; Move in 1st parameter, short
           MOV    AX,F2EXP
           OR     AX,AX
           JE     F2ZSD
           MOV    AL,F2SIGN
           XOR    AL,80H
           MOV    F2SIGN,AL     ; F2SIGN:= - F2SIGN
           MOV    AX,F1EXP
           OR     AX,AX
           JNE    F1NZSD
           CALL   MOVF2F1       ; F1:= F2
           JMP    SHORT F2ZSD
F1NZSD:    CALL   ADDFL         ; Addition in 64 bits mantissa
F2ZSD:     CALL   DRES          ; Store short result
  ENDROUTINE E@LRSUB,16

  SIMROUTINE E@REMUL            ; (DX:AX) := (DX:AX) * (BX:CX)
           OR     DX,DX
           JZ     F1MZS
           OR     BX,BX
           JNE    F1NZMS
           MOV    DX,BX
           MOV    AX,CX
           JMP    SHORT F1MZS
F1NZMS:    CALL   S2PARM        ; Move in 2nd parameter, short
           CALL   S1PARM        ; Move in 1st parameter, short
           CALL   MULFL         ; Multiply F1 and F2
           CALL   SRES          ; Store short result
F1MZS:
  ENDROUTINE E@REMUL

  SIMROUTINE E@LRMUL
; ***      (BX:CX:DX:AX) := QWORD PTR FSTARG+8 * QWORD PTR FSTARG
           CALL   D2PARM        ; Move in 2nd parameter, double
           MOV    AX,F2EXP
           OR     AX,AX
           JE     F1MZD
           CALL   D1PARM        ; Move in 1st parameter, double
           MOV    AX,F1EXP
           OR     AX,AX
           JNE    F1NZMD
F1MZD:     XOR    BX,BX         ; F1:= 0
           XOR    CX,CX
           XOR    DX,DX
           XOR    AX,AX
           JMP    SHORT F2ZMD
F1NZMD:    CALL   MULFL         ; Multiply in 64 bits mantissa
           CALL   DRES          ; Store double result
F2ZMD:
  ENDROUTINE E@LRMUL,16

  SIMROUTINE E@REDIV            ; (DX:AX) := (DX:AX) / (BX:CX)
           OR     BX,BX
           JNE    F2NZDS
           ERROR  2             ; Division by zero
F2NZDS:    OR     DX,DX
           JZ     F1ZDS
           CALL   S2PARM        ; Move in 2nd parameter, short
           CALL   S1PARM        ; Move in 1st parameter, short
           CALL   DIVFL         ; Division in 64 bits mantissa
           CALL   SRES          ; Store short result
F1ZDS:
  ENDROUTINE E@REDIV

  SIMROUTINE E@LRDIV
; ***      (BX:CX:DX:AX) := QWORD PTR FSTARG+8 / QWORD PTR FSTARG
           CALL   D2PARM        ; Move in 2nd parameter, double
           MOV    AX,F2EXP
           OR     AX,AX
           JNE    F2NZDD
           ERROR  2             ; Division by zero
F2NZDD:    CALL   D1PARM        ; Move in 1st parameter, double
           MOV    AX,F1EXP
           OR     AX,AX
           JNE    F1NZDD
           XOR    BX,BX         ; F1:= 0
           XOR    CX,CX
           XOR    DX,DX
           XOR    AX,AX
           JMP    SHORT F1ZDD
F1NZDD:    CALL   DIVFL         ; Division in 64 bits mantissa
           CALL   DRES          ; Store double result
F1ZDD:
  ENDROUTINE E@LRDIV,16

  SIMROUTINE E@RECMP            ; AH := (DX:AX) <cmp> (BX:CX)
           MOV     DI,AX
           MOV     AH,DH
           XOR     AH,BH
           AND     AH,80H       ; Same sign in DX and BX?
           MOV     AX,DI
           JZ      RSM
           OR      DX,DX        ; Sign different.
           MOV     BX,2
           JS      RDN
           MOV     DX,3         ; BX negative
           JMP     SHORT RD
RDN:       MOV     DX,1         ; DX negative
           JMP     SHORT RD
RSM:       OR      DX,DX
           JNS     RD
           AND     DH,7FH       ; Remove sign
           AND     BH,7FH
           XCHG    DX,BX        ; Swap registers
           XCHG    AX,CX
RD:        CMP     DX,BX        ; DX<>BX, they give relation
           JNE     RCFIN        ; remember signed test afterwards!
           CMP     AX,CX        ; if not, unsigned compare
           JE      RCFIN
           JA      RT
           DEC     DX           ; manipulate DX & BX to perform
           INC     BX           ; new test above
           JMP     SHORT RD
RT:        INC     DX
           DEC     BX        
           JMP     SHORT RD
RCFIN:     LAHF
           MOV     AL,AH        ; Simulate return from -87:
           MOV     CL,7
           SHR     AL,CL        ; Check if sign
           AND     AH,40H       ; Check if zero
           OR      AH,AL
  ENDROUTINE E@RECMP

  SIMROUTINE E@LRCMP
; ***      AH:= QWORD PTR FSTARG+8 <cmp> QWORD PTR FSTARG
           MOV     DX,FSTARG+14
           MOV     BX,FSTARG+6
           MOV     AH,DH
           XOR     AH,BH
           AND     AH,80H       ; Same sign in DX and BX?
           JZ      LSM
           OR      DX,DX        ; Sign different.
           MOV     BX,2
           JS      LDN
           MOV     DX,3         ; BX negative
           JMP     SHORT LD
LDN:       MOV     DX,1         ; DX negative
           JMP     SHORT LD
LSM:       OR      DX,DX
           JNS     LD
           AND     DH,7FH       ; Remove sign
           AND     BH,7FH
           XCHG    DX,BX        ; Swap registers
           MOV     AX,FSTARG+12
           XCHG    AX,FSTARG+4
           MOV     FSTARG+12,AX
           MOV     AX,FSTARG+10
           XCHG    AX,FSTARG+2
           MOV     FSTARG+10,AX
           MOV     AX,FSTARG+8
           XCHG    AX,FSTARG
           MOV     FSTARG+8,AX
LD:        CMP     DX,BX        ; DX<>BX, they give relation
           JNE     LCFIN
           MOV     AX,FSTARG+12
           MOV     CX,FSTARG+4
           CMP     AX,CX        ; if not, unsigned compare
           JE      LCEQ2
           JA      LT1
           DEC     DX           ; manipulate DX & BX to perform
           INC     BX           ; new test above
           JMP     SHORT LD
LT1:       INC     DX
           DEC     BX        
           JMP     SHORT LD
LCEQ2:     MOV     AX,FSTARG+10
           MOV     CX,FSTARG+2
           CMP     AX,CX        ; compare next part
           JE      LCEQ3
           JA      LT2
           DEC     DX           ; manipulate both DX and BX to cover
           INC     BX           ; case with maxint/minint
           JMP     SHORT LD
LT2:       INC     DX
           DEC     BX
           JMP     SHORT  LD
LCEQ3:     MOV     AX,FSTARG+8
           MOV     CX,FSTARG
           CMP     AX,CX
           JE      LCFIN
           JA      LT3
           DEC     DX
           INC     BX
           JMP     SHORT LD
LT3:       INC     DX
           DEC     BX
           JMP     SHORT LD
LCFIN:     LAHF
           MOV     AL,AH        ; Simulate return from -87:
           MOV     CL,7
           SHR     AL,CL        ; Check if sign
           AND     AH,40H       ; Check if zero
           OR      AH,AL
  ENDROUTINE E@LRCMP,16

  SIMROUTINE E@IN2RE            ; (DX:AX) := Integer_to_Real(DX:AX)
           MOV     ISIGN,0
           OR      DX,DX
           JNS     INABZ
           MOV     ISIGN,80H
           NEG2    DX,AX
INABZ:     CALL    IN2RLR
           CALL    SRES
           OR      DH,ISIGN
  ENDROUTINE E@IN2RE

  SIMROUTINE E@IN2LR
; ***      (BX:CX:DX:AX) := Integer_to_LongReal(DX:AX)
           MOV     ISIGN,0
           OR      DX,DX
           JNS     INABZL
           MOV     ISIGN,80H
           NEG2    DX,AX
INABZL:    CALL    IN2RLR
           CALL    DRES
  ENDROUTINE E@IN2LR

           EVEN
IN2RLR     PROC    NEAR         ; F1:= Integer_to_TempReal(DX:AX)
           MOV     F1MAN,DX
           MOV     F1MAN+2,AX
           XOR     CX,CX
           MOV     F1MAN+4,CX
           MOV     F1MAN+6,CX
           MOV     CX,BIAS
           ADD     CX,30
           MOV     F1EXP,CX
           MOV     AH,ISIGN
           MOV     F1SIGN,AH
           CALL    NORMF1
           RET
IN2RLR     ENDP

  SIMROUTINE E@RE2IN            ; (DX:AX) := Real_to_Integer(DX:AX)
           OR      DX,DX
           JE      ISIGOK       ; Result zero as well
           MOV     DI,DX
           AND     DH,80H
           MOV     ISIGN,DH
           MOV     DX,DI
           AND     DX,7FFFH
           CMP     DX,MAXRE2IN
           JNA     RE2INOK
           ERROR   6            ; Integer overflow
RE2INOK:   CALL    S1PARM
           CALL    R2IN
           CMP     ISIGN,0
           JE      ISIGOK
           NEG2    DX,AX        ; Reinstall sign
ISIGOK:
  ENDROUTINE E@RE2IN

           EVEN
R2IN       PROC    NEAR         ; (DX:AX):= Real_to_Integer(F1)
           MOV     CX,F1EXP
           CMP     CX,0
           MOV     DX,CX
           MOV     AX,CX
           JE      R2ZR
           SUB     CX,BIAS
           SUB     CX,30
           NEG     CX
           MOV     BX,OFFSET DGROUP:F1MAN
CLSF:      CALL    SHFR
           ADD     F1MAN+4,8000H
           ADC     F1MAN+2,0
           ADC     F1MAN,0
           MOV     AX,F1MAN+2
           MOV     DX,F1MAN
R2ZR:      RET
R2IN       ENDP

  SIMROUTINE E@RE2LR
; ***      (BX:CX:DX:AX) := Real_to_LongReal(DX:AX)
           OR      DX,DX
           JNE     R2LCNZ
           MOV     BX,DX
           MOV     CX,DX
           JMP     SHORT R2LCZ  ; Zero case
R2LCNZ:    MOV     DI,DX
           AND     DH,80H
           MOV     ISIGN,DH
           MOV     DX,DI
           AND     DX,7FFFH
           CALL    S1PARM
           CALL    DRES
           OR      BH,ISIGN     ; Reinstall sign
R2LCZ:
  ENDROUTINE E@RE2LR

  SIMROUTINE E@LR2IN
; ***      (DX:AX) := LongReal_to_Integer(QWORD PTR FSTARG)
           MOV     DX,FSTARG+6
           MOV     DI,DX
           AND     DH,80H
           MOV     ISIGN,DH
           MOV     DX,DI
           AND     DX,7FFFH
           CMP     DX,MAXLR2IN
           JNA     LR2INOK
           ERROR   6            ; Integer overflow
LR2INOK:   MOV     FSTARG+6,DX
           CALL    D2PARM
           CALL    MOVF2F1
           CALL    R2IN
           CMP     ISIGN,0
           JE      ISIGLOK
           NEG2    DX,AX        ; Reinstall sign
ISIGLOK:
  ENDROUTINE E@LR2IN,8

  SIMROUTINE E@LR2RE
; ***      (DX:AX) := LongReal_to_Real( QWORD PTR FSTARG )
           MOV     DX,FSTARG+6
           MOV     DI,DX
           AND     DH,80H
           MOV     ISIGN,DH
           MOV     DX,DI
           AND     DX,7FFFH
           CMP     DX,MAXLR2RE
           JNA     LR2REOK
           ERROR   3            ; Floating overflow
LR2REOK:   CALL    D2PARM
           CALL    MOVF2F1
           CALL    SRES
           OR      DH,ISIGN     ; Reinstall sign
  ENDROUTINE E@LR2RE,8

  SIMROUTINE E@SC2LR
; ***       Scaled Integer to long real (Used by DRAWRP)
; ******** (BX:CX:DX:AX) := DX:AX * 1.0*(2**-31)
; ***       Note: DX:AX must be positive
            MOV     ISIGN,0     ; Clear sign for IN2RLR
            CALL    IN2RLR
            SUB     F1EXP,31
            CALL    DRES
  ENDROUTINE E@SC2LR

  SIMROUTINE E@RSQROO
           MOV    AX,FSTARG+2
           OR     AX,AX
           JG     RSQ44                 ; if arg <= 0.0 then
           JNL    RSQ45                 ;    if arg < 0.0 then
           ERROR  128                   ;       RTERR(128,0);
RSQ45:     XOR    AX,AX
           MOV    FSTARG+4,AX           ;    endif:
           MOV    FSTARG+6,AX           ;    res:= 0.0; goto RSQexit;
           JMP    NEAR PTR RSQEX        ; endif;
RSQ44:     MOV    AX,FSTARG+2
           MOV    CL,7
           SHR    AX,CL
           SUB    AX,127
           MOV    EXP,AX                ; exp:= SHR(arg(1),7) - 127;
           MOV    AX,FSTARG+2
           AND    AX,127
           MOV    CL,9
           SHL    AX,CL
           MOV    MAN1,AX               ; man1:= SHL(AND(arg(1),127),9);
           MOV    AX,FSTARG
           MOV    MAN0,AX               ; man0:= arg(0);
           MOV    CL,15
           SHR    AX,CL
           MOV    CL,8
           SHL    AX,CL
           OR     AX,MAN1
           MOV    MAN1,AX               ; man1:= OR(man1,SHL(SHR(man0,15),8));
           MOV    AX,MAN0
           SHL    AX,1
           MOV    MAN0,AX               ; man0:= SHL(man0,1);
           MOV    AX,EXP
           AND    AX,1
           JNZ    RSQ47                 ; if AND(exp,1)=0 then
           MOV    RTMP0,AX
           MOV    RTMP1,AX              ;    rtmp:= 1.0; NB !!
           JMP    NEAR PTR RSQ48        ; else
RSQ47:     MOV    WORD PTR RTMP0,004F4H
           MOV    WORD PTR RTMP1,03FB5H ;    rtmp:= 1.4142136; (sqrt(2).
RSQ48:     CMP    EXP,0                 ; endif;
           JGE    RSQ49                 ; if exp < 0 then
           MOV    BX,RTMP1
           OR     BX,BX
           JE     RNODIV
           MOV    CX,RTMP0
           XOR    AX,AX
           MOV    DX,03F80H
           CALL   NEAR PTR E@REDIV      ;    rtmp:= 1.0/rtmp;
           MOV    RTMP0,AX
           MOV    RTMP1,DX
RNODIV:    INC    EXP                   ;    exp:= exp+1; (Due to SAR)
RSQ49:     MOV    AX,EXP                ; endif;
           SAR    AX,1
           ADD    AX,127
           MOV    EXP,AX
           XOR    AX,AX
           MOV    RMN1,AX               ; rmn1:= 0;
           MOV    RMN2,AX               ; rmn2:= 0;
           MOV    INC0,AX               ; inc0:= 0;
           MOV    INC1,AX               ; inc1:= 0;
           MOV    CNT,AX                ; cnt:= 0;
           MOV    N0,3                  ; n0= 3;
           MOV    N1,AX                 ; n1:= 0;
RCNT4:     CMP    CNT,4
           JGE    RCNT9                 ; repeat while cnt<4 do
           INCADD MAN1,DX               ;    inc0:= inc0*4+SHR(man1,14);
           MANDD  MAN1,CX               ;    man1:= man1*4;
           N0M1DA1                      ;    n0:= (n0-1)*2+1;
           XOR    CX,CX                 ;    t:= 0;
           SUB    DX,N0
           JB     RSQ51                 ;    if inc0>=n0 then
           MOV    INC0,DX               ;       inc0:= inc0-n0;
           ADD    N0,2                  ;       n0:= n0+2;
           INC    CX                    ;       t:= 1;
RSQ51:     SHL    RMN1,1                ;    endif;
           ADD    RMN1,CX               ;    rmn1:= SHL(rmn1,1)+t;
           INC    CNT                   ;    cnt:= cnt+1;
           JMP    NEAR PTR RCNT4        ; endrepeat;
RCNT9:     CMP    CNT,9
           JGE    RCNT12                ; repeat while cnt<9 do
           INCADD MAN0,DX               ;    inc0:= inc0*4+SHR(man0,14);
           MANDD  MAN0,CX               ;    man0:= SHL(man0,2);
           N0M1DA1                      ;    n0:= (n0-1)*2+1;
           XOR    CX,CX                 ;    t:= 0;
           SUB    DX,N0
           JB     RSQ54                 ;    if inc0>=n0 then
           MOV    INC0,DX               ;       inc0:= inc0-n0;
           ADD    N0,2                  ;       n0:= n0+2;
           INC    CX                    ;       t:= 1;
RSQ54:     SHL    RMN1,1                ;    endif;
           ADD    RMN1,CX               ;    rmn1:= SHL(rmn1,1)+t;
           INC    CNT                   ;    cnt:= cnt+1;
           JMP    NEAR PTR RCNT9        ; endrepeat;
RCNT12:    CMP    CNT,12
           JGE    RCNT23                ; repeat while cnt<12 do
           INCADD MAN0,DX               ;    inc0:= inc0*4+SHR(man0,14);
           MANDD  MAN0,CX               ;    man0:= SHL(man0,2);
           N0M1DA1                      ;    n0:= (n0-1)*2+1;
           XOR    CX,CX                 ;    t:= 0;
           SUB    DX,N0         
           JB     RSQ57                 ;    if inc0>=n0 then
           MOV    INC0,DX               ;       inc0:= inc0-n0;
           ADD    N0,2                  ;       n0:= n0+2;
           INC    CX                    ;       t:= 1;
RSQ57:     SHL    RMN2,1                ;    endif;
           ADD    RMN2,CX               ;    rmn2:= SHL(rmn2,1)+t;
           INC    CNT                   ;    cnt:= cnt+1;
           JMP    NEAR PTR RCNT12       ; endrepeat;
RCNT23:    CMP    CNT,23                ; repeat while cnt<23 do
           JGE    RSQ59
           SHL    INC0,1
           RCL    INC1,1
           SHL    INC0,1
           RCL    INC1,1                ;    (inc0,inc1):= ()*4;
           SUB    N0,1
           SBB    N1,0
           SHL    N0,1
           RCL    N1,1
           ADD    N0,1
           ADC    N1,0                  ;    (n0,n1):= ((n0,n1)-1)*2+1;
           MOV    AX,INC0
           MOV    DX,INC1
           XOR    CX,CX                 ;    t:= 0;
           SUB    AX,N0
           SBB    DX,N1
           JL     RSQ63                 ;    if inc1 >= n1 then
           SUB    INC0,N0
           MOV    AX,N1
           SBB    INC1,AX               ;       inc1:= inc1 - n1;
           ADD    N0,2
           ADC    N1,0                  ;       n1:= n1 + 2;
           INC    CX                    ;       t:= 1;
RSQ63:     SHL    RMN2,1                ;    endif;
           ADD    RMN2,CX               ;    rmn2:= SHL(rmn2,1)+t;
           INC    CNT                   ;    cnt:= cnt+1;
           JMP    NEAR PTR RCNT23       ; endrepeat;
RSQ59:     MOV    CL,14
           MOV    AX,RMN1
           SHL    AX,CL
           OR     RMN2,AX               ; rmn2:= OR(rmn2,SHL(rmn1,14));
           MOV    DX,RMN1
           SHR    DX,1
           SHR    DX,1
           MOV    AX,EXP
           AND    AX,255
           MOV    CL,7
           SHL    AX,CL
           OR     DX,AX                 ; rmn1:= OR(SHR(rmn1,2),
           MOV    AX,RMN2               ;        SHL(AND(exp,255),7));
           MOV    BX,RTMP1
           OR     BX,BX
           JZ     RNOMUL
           MOV    CX,RTMP0
           CALL   NEAR PTR E@REMUL      ; res:= rtmp*rmn.AsReal;
RNOMUL:    MOV    FSTARG+4,AX
           MOV    FSTARG+6,DX
RSQEX:
  ENDROUTINE E@RSQROO,4

  SIMROUTINE E@SQROOT
           MOV    AX,FSTARG+6
           OR     AX,AX
           JG     DSQ44                 ; if arg <= 0.0 then
           JNL    DSQ45                 ;    if arg < 0.0 then
           ERROR  128                   ;       RTERR(128,0);
DSQ45:     XOR    AX,AX                 ;    endif:
           MOV    FSTARG+8,AX
           MOV    FSTARG+10,AX
           MOV    FSTARG+12,AX
           MOV    FSTARG+14,AX          ;    res:= 0.0; goto DSQexit;
           JMP    NEAR PTR DSQEX        ; endif;
DSQ44:     MOV    AX,FSTARG+6
           MOV    CL,4
           SHR    AX,CL
           SUB    AX,1023
           MOV    EXP,AX                ; exp:= SHR(arg(3),4) - 1023;
           MOV    AX,FSTARG+6
           MOV    CL,12
           SHL    AX,CL
           MOV    BX,FSTARG+4
           MOV    CL,4
           SHR    BX,CL
           OR     AX,BX                 ; man3:= OR(SHL(arg(3),12),
           MOV    MAN3,AX               ;           SHR(arg(2),4));
           MOV    AX,FSTARG+4
           MOV    CL,12
           SHL    AX,CL
           MOV    BX,FSTARG+2
           MOV    CL,4
           SHR    BX,CL
           OR     AX,BX                 ; man2:= OR(SHL(arg(2),12),
           MOV    MAN2,AX               ;           SHR(arg(1),4));
           MOV    AX,FSTARG+2
           MOV    CL,12
           SHL    AX,CL
           MOV    BX,FSTARG
           MOV    CL,4
           SHR    BX,CL
           OR     AX,BX                 ; man1:= OR(SHL(arg(1),12),
           MOV    MAN1,AX               ;           SHR(arg(0),4));
           MOV    AX,FSTARG
           MOV    CL,12
           SHL    AX,CL
           MOV    MAN0,AX               ; man0:= OR(SHL(arg(0),12);
           MOV    AX,EXP
           AND    AX,1
           JNZ    DSQ47                 ; if AND(exp,1)=0 then
           MOV    RTMP0,AX
           MOV    RTMP1,AX
           MOV    RTMP2,AX
           MOV    RTMP3,AX              ;    rtmp:= 1.0; NB !!
           JMP    NEAR PTR DSQ48        ; else
DSQ47:     MOV    WORD PTR RTMP0,03BCDH
           MOV    WORD PTR RTMP1,0667FH
           MOV    WORD PTR RTMP2,0A09EH
           MOV    WORD PTR RTMP3,03FF6H ;    rtmp:= 1.4142136;
DSQ48:     CMP    WORD PTR EXP,0        ; endif;
           JGE    DSQ49                 ; if exp < 0 then
           MOV    BX,RTMP3
           OR     BX,BX
           JE     DNODIV
           XOR    AX,AX
           PUSH   AX
           PUSH   AX
           PUSH   AX
           MOV    AX,03FF0H             ; 1.0
           PUSH   AX
           MOV    AX,RTMP3
           PUSH   AX
           MOV    AX,RTMP2
           PUSH   AX
           MOV    AX,RTMP1
           PUSH   AX
           MOV    AX,RTMP0
           PUSH   AX
           CALL   NEAR PTR E@LRDIV      ;    rtmp:= 1.0/rtmp;
           MOV    RTMP0,AX
           MOV    RTMP1,DX
           MOV    RTMP2,CX
           MOV    RTMP3,BX
DNODIV:    INC    EXP                   ;    exp:= exp+1; (Due to SAR)
DSQ49:     MOV    AX,EXP                ; endif;
           SAR    AX,1
           ADD    AX,1023
           MOV    EXP,AX                ; exp:= exp/2+1023;
           XOR    AX,AX
           MOV    RMN1,AX               ; rmn1:= 0;
           MOV    RMN2,AX               ; rmn2:= 0;
           MOV    RMN3,AX               ; rmn3:= 0;
           MOV    RMN4,AX               ; rmn4:= 0;
           MOV    INC0,AX               ; inc0:= 0;
           MOV    INC1,AX               ; inc1:= 0;
           MOV    INC2,AX               ; inc2:= 0;
           MOV    INC3,AX               ; inc3:= 0;
           MOV    CNT,AX                ; cnt:= 0;
           MOV    N0,3                  ; n0:= 3;
           MOV    N1,AX                 ; n1:= 0;
           MOV    N2,AX                 ; n2:= 0;
           MOV    N3,AX                 ; n3:= 0;
DCNT8:     CMP    CNT,8
           JGE    DCNT13                ; repeat while cnt<8 do
           INCADD MAN3,DX               ;    inc0:= inc0*4 + SHR(man3,14);
           MANDD  MAN3,CX               ;    man3:= SHL(man3,2);
           N0M1DA1                      ;    n0:= (n0-1)*2 + 1;
           XOR    CX,CX                 ;    t:= 0;
           SUB    DX,N0
           JB     DSQ51                 ;    if inc0>=n0 then
           MOV    INC0,DX               ;       inc0:= inc0-n0;
           ADD    N0,2                  ;       n0:= n0+2;
           INC    CX                    ;       t:= 1;
DSQ51:     SHL    RMN1,1                ;    endif;
           ADD    RMN1,CX               ;    rmn1:= SHL(rmn1,1)+t;
           INC    CNT                   ;    cnt:= cnt+1;
           JMP    NEAR PTR DCNT8        ; endrepeat;
DCNT13:    CMP    CNT,13
           JGE    DCNT16                ; repeat while cnt<13 do
           INCADD MAN2,DX               ;    inc0:= inc0*4 + SHR(man2,14);
           MANDD  MAN2,CX               ;    man2:= SHL(man2,2);
           N0M1DA1                      ;    n0:= (n0-1)*2 + 1;
           XOR    CX,CX                 ;    t:= 0;
           SUB    DX,N0
           JB     DSQ510                ;    if inc0>=n0 then
           MOV    INC0,DX               ;       inc0:= inc0-n0;
           ADD    N0,2                  ;       n0:= n0+2;
           INC    CX                    ;       t:= 1;
DSQ510:    SHL    RMN1,1                ;    endif;
           ADD    RMN1,CX               ;    rmn1:= SHL(rmn1,1)+t;
           INC    CNT                   ;    cnt:= cnt+1;
           JMP    NEAR PTR DCNT13       ; endrepeat;
DCNT16:    CMP    CNT,16                ; repeat while cnt<16 do
           JGE    DCNT24
           MOV    DX,INC0
           MOV    BX,INC1
           SHL    DX,1
           RCL    BX,1
           SHL    DX,1
           RCL    BX,1
           MOV    AX,MAN2
           MOV    CL,14
           SHR    AX,CL
           ADD    DX,AX
           ADC    BX,0
           MOV    INC0,DX               ;    inc1:= inc1*4 + SHR(man2,14);
           MOV    INC1,BX
           MANDD  MAN2,CX               ;    man2:= SHL(man2,2);
           MOV    AX,N0
           MOV    CX,N1
           SUB    AX,1
           SBB    CX,0
           SHL    AX,1
           RCL    CX,1
           ADD    AX,1
           ADC    CX,0
           MOV    N0,AX
           MOV    N1,CX                 ;    n1:= (n1-1)*2 + 1;
           XOR    CX,CX                 ;    t:= 0;
           SUB    DX,N0
           SBB    BX,N1
           JB     DSQ54                 ;    if inc1>=n1 then
           MOV    INC0,DX               ;       inc1:= inc1-n1;
           MOV    INC1,BX
           ADD    N0,2                  ;       n1:= n1+2;
           ADC    N1,0
           INC    CX                    ;       t:= 1;
DSQ54:     SHL    RMN1,1                ;    endif;
           ADD    RMN1,CX               ;    rmn1:= SHL(rmn1,1)+t;
           INC    CNT                   ;    cnt:= cnt+1;
           JMP    NEAR PTR DCNT16       ; endrepeat;
DCNT24:    CMP    CNT,24                ; repeat while cnt<24 do
           JL     DCNT32X
           JMP    NEAR PTR DCNT32
DCNT32X:   MOV    DX,INC0
           MOV    BX,INC1
           SHL    DX,1
           RCL    BX,1
           SHL    DX,1
           RCL    BX,1
           MOV    CL,14
           MOV    AX,MAN1
           SHR    AX,CL
           ADD    DX,AX
           ADC    BX,0
           MOV    INC0,DX               ;    inc2:= inc2*4+SHR(man1,14);
           MOV    INC1,BX
           MANDD  MAN1,CX               ;    man1:= SHL(man1,2);
           MOV    AX,N0
           MOV    BX,N1
           SUB    AX,1
           SBB    BX,0
           SHL    AX,1
           RCL    BX,1
           ADD    AX,1
           ADC    BX,0
           MOV    N0,AX                 ;    n2:= (n2-1)*2+1;
           MOV    N1,BX
           MOV    AX,INC0
           MOV    BX,INC1
           XOR    CX,CX                 ;    t:= 0;
           SUB    AX,N0
           SBB    BX,N1
           JB     DSQ57                 ;    if inc2>=n2 then
           MOV    INC0,AX               ;       inc2:= inc2-n2;
           MOV    INC1,BX
           ADD    N0,2                  ;       n0:= n0+2;
           ADC    N1,0
           INC    CX                    ;       t:= 1;
DSQ57:     SHL    RMN2,1                ;    endif;
           ADD    RMN2,CX               ;    rmn2:= SHL(rmn2,1)+t;
           INC    CNT                   ;    cnt:= cnt+1;
           JMP    NEAR PTR DCNT24       ; endrepeat;
DCNT32:    CMP    CNT,32                ; repeat while cnt<32 do
           JL     DSQ59X
           JMP    DCNT48
DSQ59X:    MOV    AX,INC0
           MOV    DX,INC1
           MOV    BX,INC2
           SHL    AX,1
           RCL    DX,1
           RCL    BX,1
           SHL    AX,1
           RCL    DX,1
           RCL    BX,1
           PUSH   BX
           MOV    CL,14
           MOV    BX,MAN0
           SHR    BX,CL
           ADD    AX,BX
           POP    BX
           ADC    DX,0
           ADC    BX,0
           MOV    INC0,AX                
           MOV    INC1,DX
           MOV    INC2,BX               ;    inc3:= inc3*4+SHR(man0,14);
           MANDD  MAN0,CX               ;    man0:= SHL(man0,2);
           MOV    AX,N0
           MOV    DX,N1
           MOV    BX,N2
           SUB    AX,1
           SBB    DX,0
           SBB    BX,0
           SHL    AX,1
           RCL    DX,1
           RCL    BX,1
           ADD    AX,1
           ADC    DX,0
           ADC    BX,0
           MOV    N0,AX
           MOV    N1,DX
           MOV    N2,BX                 ;    n3:= (n3-1)*2+1;
           XOR    CX,CX                 ;    t:= 0;
           MOV    AX,INC0
           MOV    DX,INC1
           MOV    BX,INC2
           SUB    AX,N0
           SBB    DX,N1
           SBB    BX,N2
           JB     DSQ64                 ;    if inc1 >= n1 then
           MOV    INC0,AX
           MOV    INC1,DX
           MOV    INC2,BX               ;       inc3:= inc3 - n3;
           ADD    N0,2
           ADC    N1,0
           ADC    N2,0                  ;       n3:= n3 + 2;
           INC    CX                    ;       t:= 1;
DSQ64:     SHL    RMN2,1                ;    endif;
           ADD    RMN2,CX               ;    rmn4:= SHL(rmn4,1)+t;
           INC    CNT                   ;    cnt:= cnt+1;
           JMP    NEAR PTR DCNT32       ; endrepeat;
DCNT48:    CMP    CNT,48                ; repeat while cnt<48 do
           JL     DSQ19X
           JMP    DCNT52
DSQ19X:    MOV    AX,INC0
           MOV    DX,INC1
           MOV    BX,INC2
           MOV    CX,INC3
           SHL    AX,1
           RCL    DX,1
           RCL    BX,1
           RCL    CX,1
           SHL    AX,1
           RCL    DX,1
           RCL    BX,1
           RCL    CX,1
           MOV    INC0,AX                
           MOV    INC1,DX
           MOV    INC2,BX
           MOV    INC3,CX               ;    inc3:= inc3*4;
           MOV    AX,N0
           MOV    DX,N1
           MOV    BX,N2
           MOV    CX,N3
           SUB    AX,1
           SBB    DX,0
           SBB    BX,0
           SBB    CX,0
           SHL    AX,1
           RCL    DX,1
           RCL    BX,1
           RCL    CX,1
           ADD    AX,1
           ADC    DX,0
           ADC    BX,0
           ADC    CX,0
           MOV    N0,AX
           MOV    N1,DX
           MOV    N2,BX
           MOV    N3,CX                 ;    n3:= (n3-1)*2+1;
           XOR    CX,CX
           PUSH   CX                    ;    t:= 0;
           MOV    AX,INC0
           MOV    DX,INC1
           MOV    BX,INC2
           MOV    CX,INC3
           SUB    AX,N0
           SBB    DX,N1
           SBB    BX,N2
           SBB    CX,N3
           JB     DSQ642                ;    if inc2 >= n2 then
           MOV    INC0,AX
           MOV    INC1,DX
           MOV    INC2,BX
           MOV    INC3,CX               ;       inc3:= inc3 - n3;
           ADD    N0,2
           ADC    N1,0
           ADC    N2,0
           ADC    N3,0                  ;       n3:= n3 + 2;
           POP    CX
           INC    CX
           PUSH   CX                    ;       t:= 1;
DSQ642:    SHL    RMN3,1                ;    endif;
           POP    CX
           ADD    RMN3,CX               ;    rmn3:= SHL(rmn3,1)+t;
           INC    CNT                   ;    cnt:= cnt+1;
           JMP    NEAR PTR DCNT48       ; endrepeat;
DCNT52:    CMP    CNT,52                ; repeat while cnt<52 do
           JL     DSQ18X
           JMP    DSQ59
DSQ18X:    MOV    AX,INC0
           MOV    DX,INC1
           MOV    BX,INC2
           MOV    CX,INC3
           SHL    AX,1
           RCL    DX,1
           RCL    BX,1
           RCL    CX,1
           SHL    AX,1
           RCL    DX,1
           RCL    BX,1
           RCL    CX,1
           MOV    INC0,AX                
           MOV    INC1,DX
           MOV    INC2,BX
           MOV    INC3,CX               ;    inc3:= inc3*4;
           MOV    AX,N0
           MOV    DX,N1
           MOV    BX,N2
           MOV    CX,N3
           SUB    AX,1
           SBB    DX,0
           SBB    BX,0
           SBB    CX,0
           SHL    AX,1
           RCL    DX,1
           RCL    BX,1
           RCL    CX,1
           ADD    AX,1
           ADC    DX,0
           ADC    BX,0
           ADC    CX,0
           MOV    N0,AX
           MOV    N1,DX
           MOV    N2,BX
           MOV    N3,CX                 ;    n3:= (n3-1)*2+1;
           XOR    CX,CX
           PUSH   CX                    ;    t:= 0;
           MOV    AX,INC0
           MOV    DX,INC1
           MOV    BX,INC2
           MOV    CX,INC3
           SUB    AX,N0
           SBB    DX,N1
           SBB    BX,N2
           SBB    CX,N3
           JB     DSQ643                ;    if inc3 >= n3 then
           MOV    INC0,AX
           MOV    INC1,DX
           MOV    INC2,BX
           MOV    INC3,CX               ;       inc3:= inc3 - n3;
           ADD    N0,2
           ADC    N1,0
           ADC    N2,0
           ADC    N3,0                  ;       n3:= n3 + 2;
           POP    CX
           INC    CX
           PUSH   CX                    ;       t:= 1;
DSQ643:    SHL    RMN4,1                ;    endif;
           POP    CX
           ADD    RMN4,CX               ;    rmn3:= SHL(rmn3,1)+t;
           INC    CNT                   ;    cnt:= cnt+1;
           JMP    NEAR PTR DCNT52       ; endrepeat;
DSQ59:     MOV    CL,4
           MOV    AX,RMN3
           SHL    AX,CL
           OR     RMN4,AX       ; rmn4:= OR(SHL(rmn3,4),SHR(rmn4,12));
           MOV    CL,4
           MOV    AX,RMN2
           SHL    AX,CL
           MOV    DX,RMN3
           MOV    CL,12
           SHR    DX,CL
           OR     DX,AX
           MOV    RMN3,DX       ; rmn3:= OR(SHL(rmn2,4),SHR(rmn3,12));
           MOV    CL,4
           MOV    AX,RMN1
           SHL    AX,CL
           MOV    BX,RMN2
           MOV    CL,12
           SHR    BX,CL
           OR     AX,BX
           MOV    RMN2,AX       ; rmn2:= OR(SHL(rmn1,4),SHR(rmn2,12));
           MOV    CL,12
           MOV    BX,RMN1
           SHR    BX,CL
           MOV    AX,EXP
           MOV    CL,4
           SHL    AX,CL
           OR     BX,AX
           CMP    RTMP3,0
           MOV    CX,RMN2
           MOV    AX,RMN4
           JZ     DNOMUL
           PUSH   BX
           PUSH   CX
           PUSH   DX
           PUSH   AX
           MOV    BX,RTMP3
           PUSH   BX
           MOV    BX,RTMP2
           PUSH   BX
           MOV    BX,RTMP1
           PUSH   BX
           MOV    BX,RTMP0              ; Push OR(SHR(rmn1,4),SHL(exp,4));
           PUSH   BX
           CALL   NEAR PTR E@LRMUL      ; res:= rtmp*rmn.AsReal;
DNOMUL:    MOV    FSTARG+8,AX
           MOV    FSTARG+10,DX
           MOV    FSTARG+12,CX
           MOV    FSTARG+14,BX
DSQEX:
  ENDROUTINE E@SQROOT,8

           EVEN
S1PARM     PROC   NEAR          ; F1:= First Parameter, DWORD
           MOV    DI,DX
           AND    DH,80H
           MOV    F1SIGN,DH     ; Save sign
           MOV    DX,DI
           AND    DX,7FFFH      ; Remove sign bit
           SUB    DX,SBIAS
           PUSH   CX
           MOV    CL,7
           SAR    DX,CL
           ADD    DX,BIAS       ; Internal Bias
           MOV    F1EXP,DX      ; Save exponent
           MOV    DX,DI
           MOV    DH,DL
           MOV    DL,AH
           SHR    DX,1
           OR     DH,40H        ; Insert implicit bit
           AND    DH,7FH        ; First bits must be 01
           MOV    F1MAN,DX
           MOV    CX,7
           SHL    AX,CL
           MOV    F1MAN+2,AX
           XOR    DX,DX
           MOV    F1MAN+4,DX
           MOV    F1MAN+6,DX    ; Mantissa saved
           POP    CX
           RET
S1PARM     ENDP

           EVEN
S2PARM     PROC   NEAR          ; F2:= Second parameter, DWORD
           MOV    DI,BX
           AND    BH,80H
           MOV    F2SIGN,BH     ; Save sign
           MOV    BX,DI
           AND    BX,7FFFH      ; Remove sign bit
           SUB    BX,SBIAS
           PUSH   CX
           MOV    CL,7
           SAR    BX,CL
           POP    CX
           ADD    BX,BIAS       ; Internal bias
           MOV    F2EXP,BX      ; Save exponent
           MOV    BX,DI
           MOV    BH,BL
           MOV    BL,CH
           SHR    BX,1
           OR     BH,40H        ; Insert implicit bit
           AND    BH,7FH        ; First bits must be 01
           MOV    F2MAN,BX
           MOV    BX,CX
           MOV    CX,7
           SHL    BX,CL
           MOV    F2MAN+2,BX
           XOR    BX,BX
           MOV    F2MAN+4,BX
           MOV    F2MAN+6,BX    ; Mantissa saved
           RET
S2PARM     ENDP

           EVEN
D1PARM     PROC   NEAR          ; F1:= First parameter, QWORD
           MOV    AX,FSTARG+14  ; Sign first parameter
           OR     AX,AX
           JNE    NZD1          ; if zero then
           MOV    F1EXP,AX      ; F1EXP:= 0
           JMP    SHORT ZD1
NZD1:      AND    AH,80H
           MOV    F1SIGN,AH     ; Save sign
           MOV    AX,FSTARG+14  ; Exponent first parameter
           AND    AX,7FFFH      ; Remove sign bit
           SUB    AX,DBIAS
           MOV    CL,4
           SAR    AX,CL
           ADD    AX,BIAS       ; Internal Bias
           MOV    F1EXP,AX      ; Save exponent
           MOV    AX,FSTARG+14
           MOV    F1MAN,AX
           MOV    AX,FSTARG+12
           MOV    F1MAN+2,AX
           MOV    AX,FSTARG+10
           MOV    F1MAN+4,AX
           MOV    AX,FSTARG+8
           MOV    F1MAN+6,AX
           MOV    CX,10         ; sign, exp + implicit 1
           MOV    BX,OFFSET DGROUP:F1MAN
           CALL   SHFL          ; Shift F1MAN left 10 times
           MOV    AX,F1MAN
           OR     AH,40H        ; Insert implicit bitt
           AND    AH,7FH        ; First bits must be 01
           MOV    F1MAN,AX
ZD1:       RET
D1PARM     ENDP

           EVEN
D2PARM     PROC   NEAR          ; F2:= Second parameter, QWORD
           MOV    AX,FSTARG+6   ; Sign second parameter
           OR     AX,AX
           JNE    NZD2          ; if zero then
           MOV    F2EXP,AX      ; F1EXP:= 0
           JMP    SHORT ZD2
NZD2:      AND    AH,80H
           MOV    F2SIGN,AH     ; Save sign
           MOV    AX,FSTARG+6   ; Exponent second parameter
           AND    AX,7FFFH      ; Remove sign bit
           SUB    AX,DBIAS
           MOV    CL,4
           SAR    AX,CL
           ADD    AX,BIAS       ; Internal bias
           MOV    F2EXP,AX      ; Save exponent
           MOV    AX,FSTARG+6
           MOV    F2MAN,AX
           MOV    AX,FSTARG+4
           MOV    F2MAN+2,AX
           MOV    AX,FSTARG+2 
           MOV    F2MAN+4,AX
           MOV    AX,FSTARG 
           MOV    F2MAN+6,AX    ; Mantissa saved
           MOV    CX,10         ; sign, exp + implicit 1
           MOV    BX,OFFSET DGROUP:F2MAN
           CALL   SHFL          ; Shift F2MAN left 10 times
           MOV    AX,F2MAN
           OR     AH,40H        ; Insert implicit bit
           AND    AH,7FH        ; First bits must be 01
           MOV    F2MAN,AX
ZD2:       RET
D2PARM     ENDP

           EVEN
SRES       PROC   NEAR          ; (DX:AX):= F1, DWORD
           MOV    CX,7          ; Initial right shift count
           MOV    AX,F1MAN+2    ; Check for rounding
           AND    AX,40H
           JE     NOSR          ; No increment
           MOV    AX,F1MAN+2    ; Round bit set, check rest
           AND    AX,3FH
           OR     AX,F1MAN+4    ; Additional bits: increment
           OR     AX,F1MAN+6
           JNE    SINC
           MOV    AX,F1MAN+2    ; No additional bits, round to even
           AND    AX,80H
           JE     NOSR
SINC:      ADD    F1MAN+2,80H   ; Increment least significant digit
           ADC    F1MAN,0
           MOV    AX,F1MAN      ; Check overflowbit
           AND    AX,8000H
           JE     NOSR
           INC    F1EXP
           INC    CX            ; Shift F1MAN right once more
NOSR:      MOV    CH,16
           SUB    CH,CL
           SHFWR  F1MAN,2,0
           MOV    AX,F1MAN
           SHR    AX,CL
           AND    AX,007FH      ; Remove explicit bit
           MOV    F1MAN,AX
           MOV    DX,F1EXP
           OR     DX,DX
           MOV    AX,DX
           JE     NOSS
           SUB    DX,BIAS       ; Check overflow relative to DWORD
           JNS    DXPD
           NEG    DX
DXPD:      CMP    DX,80H
           JL     ESOK
           ERROR  3             ; Floating overflow
ESOK:      MOV    DX,F1EXP
           SUB    DX,BIAS
           MOV    CL,7
           SAL    DX,CL
           ADD    DX,SBIAS
           OR     DX,F1MAN
           OR     DH,F1SIGN     ; Result in (DX:AX)
           MOV    AX,F1MAN+2
NOSS:      RET
SRES       ENDP

           EVEN
DRES       PROC   NEAR          ; (BX:CX:DX:AX):= F1, QWORD
           MOV    CX,10         ; Initial right shift count
           MOV    AX,F1MAN+6    ; Check for rounding
           AND    AX,200H
           JE     NOLR          ; No increment
           MOV    AX,F1MAN+6    ; Round bit set, check rest
           AND    AX,1FFH
           JNE    LINC          ; Additional bits: increment
           MOV    AX,F1MAN+6    ; No additional bits, round to even
           AND    AX,400H
           JE     NOLR
LINC:      ADD    F1MAN+6,400H  ; Increment least significant digit
           ADC    F1MAN+4,0
           ADC    F1MAN+2,0
           ADC    F1MAN,0
           MOV    AX,F1MAN      ; Check overflowbit
           AND    AX,8000H
           JE     NOLR
           INC    F1EXP
           INC    CX            ; Shift F1MAN right once more
NOLR:      MOV    BX,OFFSET DGROUP:F1MAN
           CALL   SHFR          ; Shift F1MAN right 10 times, final
           AND    F1MAN,000FH   ; Remove explicit bit
           MOV    AX,F1EXP
           OR     AX,AX
           MOV    BX,AX
           MOV    CX,AX
           MOV    DX,AX
           JE     NODS
           SUB    AX,BIAS       ; Check overflow relative to QWORD
           JNS    AXPD
           NEG    AX
AXPD:      CMP    AX,0400H
           JL     EDOK
           ERROR  3             ; Floating overflow
EDOK:      MOV    BX,F1EXP
           SUB    BX,BIAS
           MOV    CL,4
           SAL    BX,CL
           ADD    BX,DBIAS
           OR     BX,F1MAN
           OR     BH,F1SIGN     ; Result in (BX:CX:DX:AX)
           MOV    CX,F1MAN+2
           MOV    DX,F1MAN+4
           MOV    AX,F1MAN+6
NODS:      RET
DRES       ENDP

           EVEN
MOVF2F1    PROC   NEAR          ; F1:= F2
           MOV    AX,F2EXP
           MOV    F1EXP,AX
           MOV    AX,F2MAN
           MOV    F1MAN,AX
           MOV    AX,F2MAN+2
           MOV    F1MAN+2,AX
           MOV    AX,F2MAN+4
           MOV    F1MAN+4,AX
           MOV    AX,F2MAN+6
           MOV    F1MAN+6,AX
           MOV    AL,F2SIGN
           MOV    F1SIGN,AL
           RET
MOVF2F1    ENDP

           EVEN
ADDFL      PROC   NEAR          ; F1:= F1 + F2
           MOV    AX,F1EXP
           CMP    AX,F2EXP
           JNL    F1EXPGT       ; if F1EXP < F2EXP then
           MOV    SI,8          ; exchange F1 and F2
F1GTL:     MOV    AX,F1EXP[SI]
           MOV    BX,F2EXP[SI]
           MOV    F1EXP[SI],BX
           MOV    F2EXP[SI],AX
           DEC    SI
           DEC    SI
           JNL    F1GTL
           MOV    AL,F1SIGN
           MOV    AH,F2SIGN
           MOV    F1SIGN,AH
           MOV    F2SIGN,AL
F1EXPGT:   MOV    CX,F1EXP      ; Largest exponent in F1
           SUB    CX,F2EXP
           CMP    CX,62         ; CX = bits to shift
           JG     NOADD         ; Difference too large, no addition
           MOV    AL,F1SIGN
           CMP    AL,F2SIGN
           JE     CXLP          ; if F1SIGN <> F2SIGN then
           MOV    BX,OFFSET DGROUP:F1MAN
           CALL   NEGMAN        ; negate F1MAN
CXLP:      MOV    BX,OFFSET DGROUP:F2MAN
           CALL   SHFR          ; Shift F2MAN right CX times
           MOV    BX,OFFSET DGROUP:F1MAN
           CALL   ADF2TOBX      ; F1MAN:= F1MAN + F2MAN
           MOV    AL,F1SIGN
           CMP    AL,F2SIGN
           JE     NOSGCH        ; if F1SIGN <> F2SIGN and
           MOV    AX,F1MAN
           AND    AX,8000H
           JE     SGCH          ; bit 79 of F1MAN = 1 then begin
           MOV    BX,OFFSET DGROUP:F1MAN
           CALL   NEGMAN        ; Negate F1MAN
           JMP    SHORT NOSGCH
SGCH:      CMP    SHFLC,0
           JE     NOSGCH
           MOV    AL,F2SIGN     
           MOV    F1SIGN,AL     ; F1SIGN:= F2SIGN end
NOSGCH:    CALL   NORMF1
NOADD:     RET
ADDFL      ENDP

           EVEN
MULFL      PROC   NEAR          ; F1:= F1 * F2
           MOV    AL,F2SIGN
           XOR    F1SIGN,AL     ; F1SIGN:= F1SIGN XOR F2SIGN
           MOV    AX,F1EXP
           ADD    AX,F2EXP
           SUB    AX,BIAS
           MOV    F1EXP,AX      ; F1EXP:= F1EXP + F2EXP - (BIAS+1)
           XOR    AX,AX
           MOV    F3MAN,AX
           MOV    F3MAN+2,AX
           MOV    F3MAN+4,AX
           MOV    F3MAN+6,AX    ; Clear F3MAN
           MOV    SI,63
SREPM:     SHRONCE F3MAN        ;    Shift F3MAN right
           SHRONCE F1MAN        ;    Shift F1MAN right
           CMP    SHFLC,0
           JE     SMNOAD        ;    if CARRY then
           MOV    BX,OFFSET DGROUP:F3MAN
           CALL   ADF2TOBX      ;    F3MAN:= F3MAN + F2MAN
SMNOAD:    DEC    SI
           JG     SREPM         ; end
           MOV    AX,F3MAN
           MOV    F1MAN,AX
           MOV    AX,F3MAN+2
           MOV    F1MAN+2,AX
           MOV    AX,F3MAN+4
           MOV    F1MAN+4,AX
           MOV    AX,F3MAN+6
           MOV    F1MAN+6,AX    ; F1MAN:= F3MAN
           CALL   NORMF1        ; Normalise F1
           RET
MULFL      ENDP

           EVEN
DIVFL      PROC   NEAR          ; F1:= F1 / F2
           MOV    AX,F2EXP
           OR     AX,AX
           JNE    SDOK
           ERROR  2             ; Message division by zero
SDOK:      MOV    AL,F2SIGN
           XOR    F1SIGN,AL     ; F1SIGN:= F1SIGN XOR F2SIGN
           MOV    AX,F1EXP
           SUB    AX,F2EXP
           ADD    AX,BIAS
           DEC    AX
           MOV    F1EXP,AX      ; F1EXP:= F1EXP - F2EXP + BIAS
           XOR    AX,AX
           MOV    F3MAN,AX
           MOV    F3MAN+2,AX
           MOV    F3MAN+4,AX
           MOV    F3MAN+6,AX    ; Clear F3MAN
           MOV    BX,OFFSET DGROUP:F1MAN
           CALL   SBF2TOBX      ; F1MAN:= F1MAN - F2MAN
           MOV    SI,64
DREPCX:    SHLONCE F3MAN        ;    Shift F3MAN left once
           SHLONCE F1MAN        ;    Shift F1MAN left once
           CMP    SHFLC,0       ;    if CARRY then
           JE     CAS0
           MOV    BX,OFFSET DGROUP:F1MAN
           CALL   ADF2TOBX      ;    F1MAN:= F1MAN + F2MAN
           JMP    SHORT CAS1    ;    else begin
CAS0:      MOV    BX,OFFSET DGROUP:F1MAN
           CALL   SBF2TOBX      ;    F1MAN:= F1MAN - F2MAN
           OR     F3MAN+6,1     ;    Insert bit; end
CAS1:      DEC    SI
           JG     DREPCX        ; end;
           MOV    AX,F3MAN
           MOV    F1MAN,AX
           MOV    AX,F3MAN+2
           MOV    F1MAN+2,AX
           MOV    AX,F3MAN+4
           MOV    F1MAN+4,AX
           MOV    AX,F3MAN+6
           MOV    F1MAN+6,AX    ; F1MAN:= F3MAN
           CALL   NORMF1        ; Normalize F1
           RET
DIVFL      ENDP

           EVEN
NORMF1     PROC   NEAR          ; Normalize F1
           CMP    F1MAN,0       ; Sjekk F1MAN = 0
           JNE    F1NZR
           CMP    F1MAN+2,0
           JNE    F1NZR
           CMP    F1MAN+4,0
           JNE    F1NZR
           CMP    F1MAN+6,0
           JNE    F1NZR         ; if F1MAN = 0 then begin
           XOR    AX,AX
           MOV    F1SIGN,AL     ;    F1SIGN:= 0
           MOV    F1EXP,AX      ;    F1EXP:= 0
           JMP    F1RET         ; end else begin
F1NZR:     MOV    AX,F1MAN
           AND    AX,8000H
           JE     N79
           SHRONCE F1MAN        ;       Shift F1MAN right 1
           INC    F1EXP         ;       F1EXP:= F1EXP + 1; end
N79:       MOV    AX,F1MAN      ;    while bit 78 <> 1 do begin
           AND    AX,4000H
           JNE    NZ78
           SHLONCE F1MAN        ;       Shift F1MAN left once
           DEC    F1EXP         ;       F1EXP:= F1EXP - 1
           JMP    SHORT N79     ;    end;
NZ78:      MOV    AX,F1EXP
           AND    AX,8000H
           JE     OVFOK
           ERROR  3             ;    Message exponent overflow
OVFOK:     MOV    AX,F1EXP
           OR     AX,AX
           JNE    F1RET
           MOV    F1MAN,AX      ;    Result zero, underflow occurred
           MOV    F1MAN+2,AX
           MOV    F1MAN+4,AX
           MOV    F1MAN+6,AX
F1RET:     RET                  ; end
NORMF1     ENDP

           EVEN
SHFR       PROC   NEAR          ; Shift right CX times the QWORD area
R2RPO:     CMP    CL,16         ; identified by BX
           JL     SXLP          ; Shift whole words first.
           MOVWD  SS:[BX],4,6
           MOVWD  SS:[BX],2,4
           MOVWD  SS:[BX],0,2
           XOR    AX,AX
           MOV    SS:[BX],AX
           SUB    CL,16
           JMP    SHORT R2RPO
SXLP:      CMP    CL,0
           JNG    NOCXLP        ; No extra shift.
           MOV    CH,16
           SUB    CH,CL
           SHFWR  SS:[BX],6,4
           SHFWR  SS:[BX],4,2
           SHFWR  SS:[BX],2,0
           MOV    AX,SS:[BX]
           SHR    AX,CL
           MOV    SS:[BX],AX
NOCXLP:    RET
SHFR       ENDP

           EVEN
SHFL       PROC   NEAR          ; Shift left CX times the QWORD area
R2LPO:     CMP    CL,16         ; identified by BX
           JL     SLLP
           MOVWD  SS:[BX],2,0
           MOVWD  SS:[BX],4,2
           MOVWD  SS:[BX],6,4
           XOR    AX,AX
           MOV    SS:[BX]+6,AX
           SUB    CL,16
           JMP    SHORT R2LPO
SLLP:      CMP    CL,0
           JNG    NLCXLP
           MOV    CH,16
           SUB    CH,CL
           SHFWL  SS:[BX],0,2
           SHFWL  SS:[BX],2,4
           SHFWL  SS:[BX],4,6
           MOV    AX,SS:[BX]+6
           SHL    AX,CL
           MOV    SS:[BX]+6,AX
NLCXLP:    RET
SHFL       ENDP

           EVEN
ADF2TOBX   PROC   NEAR          ; [BX]:= [BX] + F2MAN (QWORD)
           MOV    AX,SS:[BX]+6
           ADD    AX,F2MAN+6
           MOV    SS:[BX]+6,AX
           MOV    AX,SS:[BX]+4
           ADC    AX,F2MAN+4
           MOV    SS:[BX]+4,AX
           MOV    AX,SS:[BX]+2
           ADC    AX,F2MAN+2
           MOV    SS:[BX]+2,AX
           MOV    AX,SS:[BX]
           ADC    AX,F2MAN
           MOV    SS:[BX],AX
           LAHF
           AND    AH,1
           MOV    SHFLC,AH      ; Save add carry for external testing
           RET
ADF2TOBX   ENDP

           EVEN
SBF2TOBX   PROC   NEAR          ; [BX]:= [BX] - F2MAN (QWORD)
           CLC
           MOV    AX,SS:[BX]+6
           SBB    AX,F2MAN+6
           MOV    SS:[BX]+6,AX
           MOV    AX,SS:[BX]+4
           SBB    AX,F2MAN+4
           MOV    SS:[BX]+4,AX
           MOV    AX,SS:[BX]+2
           SBB    AX,F2MAN+2
           MOV    SS:[BX]+2,AX
           MOV    AX,SS:[BX]
           SBB    AX,F2MAN
           MOV    SS:[BX],AX
           LAHF
           AND    AH,1
           MOV    SHFLC,AH      ; Save subtract carry for external testing
           RET
SBF2TOBX   ENDP

           EVEN
NEGMAN     PROC   NEAR          ; Negate Mantissa in BX (QWORD)
           NOT    WORD PTR SS:[BX]
           NOT    WORD PTR SS:[BX]+2
           NOT    WORD PTR SS:[BX]+4
           NOT    WORD PTR SS:[BX]+6
           ADD    WORD PTR SS:[BX]+6,1
           ADC    WORD PTR SS:[BX]+4,0
           ADC    WORD PTR SS:[BX]+2,0
           ADC    WORD PTR SS:[BX],0
           RET
NEGMAN     ENDP

           EVEN
S@ENV_TEXT ENDS
           END
