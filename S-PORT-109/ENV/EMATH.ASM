NAME       EMATH
; ---------------------------------------------------------------
; ---  COPYRIGHT 1989 by                                      ---
; ---  Simula a.s.                                            ---
; ---  Oslo, Norway                                           ---
; ---                                                         ---
; ---                                                         ---
; ---             P O R T A B L E     S I M U L A             ---
; ---                                                         ---
; ---                   F O R    I B M   P C                  ---
; ---                                                         ---
; ---                                                         ---
; ---       T  H  E     E  N  V  I  R  O  N  M  E  N  T       ---
; ---                                                         ---
; ---                                                         ---
; --- Formulas Used:                                          ---
; ---                                                         ---
; ---          e ** x    =  2 ** (x * log2(e))   , x real     ---
; ---          x ** y    =  2 ** (y * log2(x))                ---
; ---          10 ** x   =  2 ** (x * log2(10))               ---
; ---                                                         ---
; ---          ln(x)     =  log2(x) / log2(e)                 ---
; ---          log10(x)  =  log2(x) / log2(10)                ---
; ---                                                         ---
; ---          sinh(x)   =  e**x - e**-x                      ---
; ---          cosh(x)   =  e**x + e**-x                      ---
; ---          tanh(x)   =  sinh(x) / cosh(x)                 ---
; ---                                                         ---
; ---          asin(x)   =  atan( x / sqrt(1-x*x) )           ---
; ---          acos(x)   =  atan( sqrt(1-x*x) / x )           ---
; ---          asin(-x)  =  - asin(x)                         ---
; ---          acos(-x)  =  - acos(x)                         ---
; ---          asin(x)   =  pi/2 - acos(x)                    ---
; ---          asin(0)   =  acos(1)  =  0                     ---
; ---          asin(1)   =  acos(0)  =  pi/2                  ---
; ---          atan(-x)  =  - atan(x)                         ---
; ---          atan(1/x) =  pi/2 - atan(x)                    ---
; ---          atan(0)   =  0                                 ---
; ---          atan(1)   =  pi/4                              ---
; ---                                                         ---
; ---------------------------------------------------------------

; ---------------------------------------------------------------
; ---   C O M P A B I L I T Y   W I T H   E M U L A T O R S   ---
; ---------------------------------------------------------------
; ---  1) Only use 6 floating registers                       ---
; ---  2) Never use FFREE st(i) when st(i) is empty           ---
; ---  3) Never use FLDxx when the floating stack is full     ---
; ---  4) Never use FSTxx st(i) when st(i) is empty           ---
; ---  5) Never use FPREM, FXTRACT, FXAM, FSAVE, FSTENV       ---
; ---------------------------------------------------------------

; ---------------------------------------------------------------
; ---   A S S E M B L E R    P R O B L E M S                  ---
; ---------------------------------------------------------------
; ---  1) Use UPPER case for all x87-operations               ---
; ---     (asm may generate erroneous code otherwise!!!)      ---
; ---  2) Use macroes when indicated (i.e. for FLDT)          ---
; ---------------------------------------------------------------

INCLUDE  macro.def

   IF iAPX286
FDOWN2I  MACRO ; round down always
         FLDCW    SS:CW87POWR ; =0730H
         FRNDINT
         FLDCW    SS:CW87SIMU ; =0330H
         ENDM
   ELSE ; I.E. iAPX386
FDOWN2I  MACRO ;
         FLDCW    CW87POWR ; =0730H
         FRNDINT
         FLDCW    CW87SIMU ; =0330H
         ENDM
   ENDIF

         EXTRN   G@TMP8687:WORD
         EXTRN   G@MAXREA:DWORD
         EXTRN   G@MAXLRL:QWORD
         EXTRN   G@MINREA:DWORD
         EXTRN   G@MINLRL:QWORD
IF iAPX286
         EXTRN   G@INIERR:WORD
         EXTRN   K@ERROR:FAR
ELSE ; I.E. iAPX386
         EXTRN   G@INIERR:DWORD
         EXTRN   K@ERROR:NEAR
         EXTRN   _write:NEAR
ENDIF


; ************************************************
; ******    L  O  C  A  L     D  A  T  A    ******
; ************************************************
_DATA      SEGMENT
MINRED   DW       10 ; used to REDUCE big trig. arguments

; PIQUART  DT  3FFEC90FDAA22168C235H  ; PI/4
PIQUART    DB  35H,0C2H,68H,21H,0A2H,0DAH,0FH,0C9H,0FEH,3FH  ; PI/4
; PIHALF   DT  3FFFC90FDAA22168C235H  ; PI/2
PIHALF     DB  35H,0C2H,68H,21H,0A2H,0DAH,0FH,0C9H,0FFH,3FH  ; PI/2
INDEFINITE DD  0FFC00000H             ; indefinite special value
LIMREXP    DD  042B17218H             ; ln(maxreal)
; LIMDEXP  DQ  040862E42FEFA39F0H     ; ln(maxlongreal)
LIMDEXP    DB  0F0H,39H,0FAH,0FEH,42H,2EH,86H,40H  ; ln(maxlongreal)
FPREMTAB   DB  00H,02H,40H,42H,01H,03H,41H,43H
CW87POWR   DW 0732H   ; ****** 8087 / 80287 Control Word ******
CW87SIMU   DW 0332H   ; ****** 8087 / 80287 Control Word ******
;          <xxx>3<IC>1<RC>2<PC>2<xx>2<PM>1<UM>1<OM>1<ZM>1<DM>1<IM>1
;           000    0   00   11   00    1    1    0    0    1    0
;           IC - Infinity Control  ( 0: Projective)
;           RC - Rounding Control  (00: Round to nearest)
;           PC - Precision Control (11: 64-bit significand)
;           PM - Exception Mask - Precision
;           UM - Exception Mask - Underflow
;           OM - Exception Mask - Overflow
;           ZM - Exception Mask - Zero Divide
;           DM - Exception Mask - Denormalized Operand
;           IM - Exception Mask - Invalid Operation
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


IF WITH87
; *********************************************
; ******   U T I L I T Y : E @ P O W 2   ******
; *********************************************
       EVEN
E@POW2 PROC    NEAR   ; TOS = x * log2(base) -- To calculate  base ** x
       FLD1
       FCHS
       FLD     ST(1)
       FDOWN2I 
       FXCH    ST(2)
       FSUB    ST,ST(2)
       FSCALE
       F2XM1
       FSUBR
       FMUL    ST,ST(0)
       FSCALE
       FSTP    ST(1)
       RET
E@POW2 ENDP
ENDIF



  SIMROUTINE E@RSQROO ; ------------------------------------- sqrt(real)
; Visible Routine RSQROO;  --- rsqrt;
; import real arg; export real val;
; begin rsqrt := sqrt(arg qua long real) qua real end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
   IF iAPX286
          MOV   AX,FSTARG+2
          OR    AX,AX
   ELSE ; I.E. iAPX386
          MOV   EAX,FSTARG
          OR    EAX,EAX
   ENDIF
          JGE   SHORT RSQ1
          ERROR 128
RSQ1:     FLD   DWORD PTR FSTARG
          FSQRT
   IFNDEF X87
          FSTP  DWORD PTR FSTARG+4   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@RSQROO,4


  SIMROUTINE E@SQROOT ; -------------------------------- sqrt(long real)
; Visible Routine SQROOT;  --- sqrt;
; import long real arg; export long real val;
; begin  ...  end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
   IF iAPX286
          MOV   AX,FSTARG+6
          OR    AX,AX
   ELSE ; I.E. iAPX386
          MOV   EAX,FSTARG+4
          OR    EAX,EAX
   ENDIF
          JGE   SHORT DSQ1
          ERROR 128
DSQ1:     FLD   QWORD PTR FSTARG
          FSQRT
   IFNDEF X87
          FSTP  QWORD PTR FSTARG+8   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@SQROOT,8


  SIMROUTINE E@REXPON ; -------------------------------------- exp(real)
; Visible Routine REXPON;  --- rexp;
; import real arg; export real val;
; begin val := exp(arg qua long real) qua real end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD     DWORD PTR FSTARG     ; arg
   IF iAPX286
          FCOM    SS:LIMREXP           ; ln(maxreal)
   ELSE ; I.E. iAPX386
          FCOM    LIMREXP              ; ln(maxreal)
   ENDIF
          FSFLG
          JBE     SHORT REXP1
          FSTP    ST(0)                ; remove argument
          ERROR   26                   ; argument too big
REXP1:    FLDL2E
          FMUL
          FABS
          CALL    E@POW2  ; TOS= arg * log2(e), result: 2 ** TOS
   IF iAPX286
          MOV     AX,FSTARG+2
          OR      AX,AX
   ELSE ; I.E. iAPX386
          MOV     EAX,FSTARG
          OR      EAX,EAX
   ENDIF
          JNS     SHORT REX1
          FLD1
          FDIVR
REX1:
   IFNDEF X87
          FSTP  DWORD PTR FSTARG+4   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@REXPON,4


  SIMROUTINE E@EXPONE ; --------------------------------- exp(long real)
; Visible Routine EXPONE;  --- exp;
; import long real arg; export long real val;
; begin  ...  end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD     QWORD PTR FSTARG     ; arg
   IF iAPX286
          FCOM    SS:QWORD PTR LIMDEXP ; ln(maxlongreal)
   ELSE ; I.E. iAPX386
          FCOM    QWORD PTR LIMDEXP    ; ln(maxlongreal)
   ENDIF
          FSFLG
          JBE     SHORT DEXP1
          FSTP    ST(0)                ; remove argument
          ERROR   26                   ; argument too big
DEXP1:    FLDL2E
          FMUL
          FABS
          CALL    E@POW2  ; TOS= arg * log2(e), result: 2 ** TOS
   IF iAPX286
          MOV     AX,FSTARG+6
          OR      AX,AX
   ELSE ; I.E. iAPX386
          MOV     EAX,FSTARG+4
          OR      EAX,EAX
   ENDIF
          JNS     SHORT DEX1
          FLD1
          FDIVR
DEX1:
   IFNDEF X87
          FSTP  QWORD PTR FSTARG+8   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@EXPONE,8


  SIMROUTINE E@LOGARI ; -------------------------------------- ln(real)
; Visible Routine LOGARI;  --- ln;
; import long real arg; export long real val;
; begin  ...  end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
   IF iAPX286
          MOV   AX,FSTARG+6
          OR    AX,AX
   ELSE ; I.E. iAPX386
          MOV   EAX,FSTARG+4
          OR    EAX,EAX
   ENDIF
          JG    SHORT DLN1
          ERROR 128
DLN1:     FLD1
          FLDL2E
          FDIVP   ST(1), ST
          FLD     QWORD PTR FSTARG
          FYL2X
          WAIT
   IFNDEF X87
          FSTP  QWORD PTR FSTARG+8   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@LOGARI,8


  SIMROUTINE E@RLOGAR ; --------------------------------- ln(long real)
; Visible Routine RLOGAR;  --- rln;
; import real arg; export real val;
; begin val := ln(arg qua long real) qua real end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
   IF iAPX286
          MOV   AX,FSTARG+2
          OR    AX,AX
   ELSE ; I.E. iAPX386
          MOV   EAX,FSTARG
          OR    EAX,EAX
   ENDIF
          JG    SHORT RLN1
          ERROR 128
RLN1:     FLD1
          FLDL2E
          FDIVP   ST(1), ST
          FLD     DWORD PTR FSTARG
          FYL2X
          WAIT
   IFNDEF X87
          FSTP  DWORD PTR FSTARG+4   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@RLOGAR,4



  SIMROUTINE E@RLOG10 ; -------------------------------------- log(real)
; Visible known("RLOG10") envir_RLOG;
; import real arg; export real val;
; begin val:=envir_RLN(arg) * 0.4342944819;      -- log(e)=0.4342944819
;       --- checking of status must be done at calling place.
; end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
   IF iAPX286
          MOV   AX,FSTARG+2
          OR    AX,AX
   ELSE ; I.E. iAPX386
          MOV   EAX,FSTARG
          OR    EAX,EAX
   ENDIF
          JG    SHORT RLG1
          ERROR 128
RLG1:     FLD1
          FLDL2T
          FDIVP   ST(1), ST
          FLD     DWORD PTR FSTARG
          FYL2X
          WAIT
   IFNDEF X87
          FSTP  DWORD PTR FSTARG+4   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@RLOG10,4


  SIMROUTINE E@DLOG10 ; --------------------------------- log(long real)
; Visible known("DLOG10") envir_DLOG;
; import long real arg; export long real val;
; begin val:=envir_DLN(arg) * 0.434294481903252&&0;
;       ---  log(e)=0.434294481903252
;       ---  checking of status must be done at calling place.
; end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
   IF iAPX286
          MOV   AX,FSTARG+6
          OR    AX,AX
   ELSE ; I.E. iAPX386
          MOV   EAX,FSTARG+4
          OR    EAX,EAX
   ENDIF
          JG    SHORT DLG1
          ERROR 128
DLG1:     FLD1
          FLDL2T
          FDIVP   ST(1), ST
          FLD     QWORD PTR FSTARG
          FYL2X
          WAIT
   IFNDEF X87
          FSTP  QWORD PTR FSTARG+8   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@DLOG10,8



  SIMROUTINE E@RIPOWR ; -------------------------------------------- RIPOWR
; Visible known("RIPOWR") RIPOWER; --- res:=b**x ---
; import real b; integer x; export real res;
; begin if x=0 then if b=0.0 then ERROR(ENO_SYS_2) else res:=1.0 endif
;       elsif b=0.0 then res:=0.0
;       else if x<0 then x:=-x; res:=1.0; b:=res:=res/b; else res:=b endif;
;            repeat x := x-1 while x > 0 do res:=res*b endrepeat;
;       endif;
; end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
   IF iAPX286
          MOV    CX,WORD PTR FSTARG
          MOV    DX,WORD PTR FSTARG+2 ; DX:CX=x
   ELSE ; I.E. iAPX386
          MOV    ECX,FSTARG           ; ECX:= x
   ENDIF
          FLD    DWORD PTR FSTARG+4   ; ST(0)=b
          CALL   FIPOWR               ; b**x, r/l binary
   IFNDEF X87
          FSTP  DWORD PTR FSTARG+8   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@RIPOWR,8


  SIMROUTINE E@DIPOWR ; -------------------------------------------- DIPOWR
; Visible known("DIPOWR") DIPOWER; --- res:=b**x ---
; import long real b; integer x; export long real res;
; begin if x=0 then if b=0.0&&0 then ERROR(ENO_SYS_2) else res:=1.0&&0 endif
;       elsif b=0.0&&0 then res:=0.0&&0;
;       else if x < 0 then x:=-x; res:=1.0&&0; b:=res:=res/b else res:=b endif;
;            repeat  x := x-1  while  x > 0  do  res := res * b  endrepeat;
;       endif;
; end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
   IF iAPX286
          MOV    CX,WORD PTR FSTARG
          MOV    DX,WORD PTR FSTARG+2 ; DX:CX=x
   ELSE ; I.E. iAPX386
          MOV    ECX,FSTARG           ; ECX:= x
   ENDIF
          FLD    QWORD PTR FSTARG+4   ; ST(0)=b
          CALL   FIPOWR               ; b**x, r/l binary
   IFNDEF X87
          FSTP  QWORD PTR FSTARG+12  ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@DIPOWR,12


IF WITH87
       EVEN
FIPOWR    PROC   NEAR          ; TOS = a, DX:CX/ECX = b, result = a**b
   IF iAPX286
          OR     DX,DX
   ELSE ; I.E. iAPX386
          OR     ECX,ECX
   ENDIF
          JS     SHORT FIPOW2          ; Negative exponent
          JNZ    SHORT FIPOW3          ; Positive, Nonzero exponent
   IF iAPX286
          OR     CX,CX
          JNZ    SHORT FIPOW3          ; Positive, Nonzero exponent
   ENDIF
          FLDZ
          FCOMPP
          FSFLG
          JNE    SHORT FIPOW1          ; b <> 0.0
          ERROR  128                   ; Zero to Zero power => undefined
FIPOW1:   FLD1
          JMP    SHORT FIPOWE          ; Nonzero to Zero power => 1.0
FIPOW2:
   IF iAPX286
          NEG    CX
          ADC    DX,0
          NEG    DX
   ELSE ; I.E. iAPX386
          NEG    ECX
   ENDIF
FIPOW3:   FTST
          FSFLG
          JE     SHORT FIPOWE          ; Zero to Nonzero power => 0.0
          FLD1                         ; y:= 1
          FXCH   ST(1)                 ; z to ST(0)
FIPOW4: 
   IF iAPX286
          SHR    DX,1
          RCR    CX,1                  ; DX:CX 32-bit Shift right 1
   ELSE ; I.E. iAPX386
          SHR    ECX,1                 ; ECX 32-bit Shift right 1
   ENDIF
          JC     SHORT FIPOW6
FIPOW5:   FMUL   ST,ST(0)              ; DX:CX even, z:= z*z
          JMP    FIPOW4
FIPOW6:   FMUL   ST(1),ST              ; DX:CX odd, y:= y*z
   IF iAPX286
          OR     CX,CX
          JNZ    FIPOW5
          OR     DX,DX
   ELSE ; I.E. iAPX386
          OR     ECX,ECX
   ENDIF
          JNZ    FIPOW5
          FSTP   ST(0)                 ; POP ST(0), result = y
   IF iAPX286
          MOV    AX,FSTARG+2           ; HI-Word of x
          OR     AX,AX
   ELSE ; I.E. iAPX386
          MOV    EAX,FSTARG            ; Whole x
          OR     EAX,EAX
   ENDIF
          JNS    SHORT FIPOWE          ; x > 0
          FLD1
          FDIVR                        ; res := 1.0 / res
FIPOWE:   RET
FIPOWR    ENDP

ENDIF



  SIMROUTINE E@RRPOWR ; -------------------------------------------- RRPOWR
; Visible known("RRPOWR") RRPOWER; --- res:=b**x ---
; import real b,x; export real res;
; begin if b=0.0 then
;       if x<=0.0 then ERROR(ENO_SYS_2) else res:=0.0 endif
;       elsif b<0.0 then ERROR(ENO_SYS_2)
;       elsif b=0.0 then res:=0.0
;       else res:=envir_RLN(b); if status > 0 then ERROR(ENO_SYS_2) endif;
;            res:=envir_REXP(res*x); if status > 0 then ERROR(ENO_SYS_2) endif;
;       endif;
; end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD    DWORD PTR FSTARG+4   ; b
          FTST
          FSFLG
          JNE    SHORT RRPOW3         ; b <> 0.0
          FLD    DWORD PTR FSTARG     ; x
          FTST
          FSFLG
          JA     SHORT RRPOW2         ; X > 0.0 ( C3=C0=0 )
          FSTP    ST(0)               ; remove argument
RRPOW1:   FSTP    ST(0)               ; remove argument
          ERROR  128                  ; result undefined
RRPOW2:   FSTP   ST(0)
          JMP    SHORT RRPOWE         ; x<>0.0 and b=0.0 => res:= 0.0
RRPOW3:   JB     SHORT RRPOW1         ; b < 0.0 ( C3=0, C0=1 )
          FLD    DWORD PTR FSTARG     ; x
          FTST
          FSFLG
          JNE    SHORT RRPOW4         ; x <> 0.0 and b <> 0.0
          FSTP   ST(0)
          FSTP   ST(0)
          FLD1
          JMP    SHORT RRPOWE         ; x=0.0 and b<>0.0 => res:= 1.0
RRPOW4:   FXCH   ST(1)                ; ST(0) = b, ST(1) = x
          FYL2X
          CALL   E@POW2 ; TOS = x * log2(b), result: 2 ** TOS
   IF iAPX286
          MOV    AX,WORD PTR FSTARG+2  ; HI-Word of x
          OR     AX,AX
   ELSE ; I.E. iAPX386
          MOV    EAX,FSTARG           ;  x
          OR     EAX,EAX
   ENDIF
          JNS    SHORT RRPOWE
          FLD1
          FDIV                        ; res := 1.0 / res
RRPOWE:
   IFNDEF X87
          FSTP  DWORD PTR FSTARG+8   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@RRPOWR,8


  SIMROUTINE E@RDPOWR ; -------------------------------------------- RDPOWR
; Visible known("RDPOWR") RDPOWER; --- res:=b**x ---
; import real b; long real x; export long real res;
; begin if b=0.0 then
;       if x<=0.0&&0 then ERROR(ENO_SYS_2) else res:=0.0&&0 endif
;       elsif b<0.0 then ERROR(ENO_SYS_2)
;       elsif b=0.0 then res:=0.0&&0;
;       else res:=envir_DLN(b qua long real);
;            if status > 0 then ERROR(ENO_SYS_2) endif;
;            res:=envir_DEXP(res*x); if status > 0 then ERROR(ENO_SYS_2) endif;
;    endif;
; end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD    DWORD PTR FSTARG+8   ; b
          FTST
          FSFLG
          JNE    SHORT RDPOW3         ; b <> 0.0
          FLD    QWORD PTR FSTARG     ; x
          FTST
          FSFLG
          JA     SHORT RDPOW2         ; X > 0.0 ( C3=C0=0 )
          FSTP    ST(0)               ; remove argument
RDPOW1:   FSTP    ST(0)               ; remove argument
          ERROR  128                  ; result undefined
RDPOW2:   FSTP   ST(0)
          JMP    SHORT RDPOWE         ; x<>0.0 and b=0.0 => res:= 0.0
RDPOW3:   JB     RDPOW1               ; b < 0.0 ( C3=0, C0=1 )
          FLD    QWORD PTR FSTARG     ; x
          FTST
          FSFLG
          JNE    SHORT RDPOW4         ; x <> 0.0 and b <> 0.0
          FSTP   ST(0)
          FSTP   ST(0)
          FLD1
          JMP    SHORT RDPOWE         ; x=0.0 and b<>0.0 => res:= 1.0
RDPOW4:   FXCH   ST(1)                ; ST(0) = b, ST(1) = x
          FYL2X
          CALL   E@POW2 ; TOS = x * log2(b), result: 2 ** TOS
   IF iAPX286
          MOV    AX,WORD PTR FSTARG+6  ; HI-Word of x
          OR     AX,AX
   ELSE ; I.E. iAPX386
          MOV    EAX,FSTARG+4         ; HI-part of x
          OR     EAX,EAX
   ENDIF
          JNS    SHORT RDPOWE
          FLD1
          FDIV                        ; res := 1.0 / res
RDPOWE:
   IFNDEF X87
          FSTP  QWORD PTR FSTARG+12  ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@RDPOWR,12


  SIMROUTINE E@DRPOWR ; -------------------------------------------- DRPOWR
; Visible known("DRPOWR") drpower; --- res:=b**x ---
; import long real b; real x; export real res;
; begin if b=0.0 then
;       if x<=0.0 then ERROR(ENO_SYS_2) else res:=0.0 endif
;       elsif b<0.0&&0 then ERROR(ENO_SYS_2)
;       elsif b=0.0&&0 then res:=0.0&&0;
;       else b:=envir_DLN(b);
;            if status > 0 then ERROR(ENO_SYS_2) endif;
;            b:=envir_DEXP(b * (x qua long real)) qua real;
;            if status > 0 then ERROR(ENO_SYS_2) endif;
;       endif;
; end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD    QWORD PTR FSTARG+4   ; b
          FTST
          FSFLG
          JNE    SHORT DRPOW3         ; b <> 0.0
          FLD    DWORD PTR FSTARG     ; x
          FTST
          FSFLG
          JA     SHORT DRPOW2         ; X > 0.0 ( C3=C0=0 )
          FSTP    ST(0)               ; remove argument
DRPOW1:   FSTP    ST(0)               ; remove argument
          ERROR  128                  ; result undefined
DRPOW2:   FSTP   ST(0)
          JMP    SHORT DRPOWE         ; x<>0.0 and b=0.0 => res:= 0.0
DRPOW3:   JB     DRPOW1               ; b < 0.0 ( C3=0, C0=1 )
          FLD    DWORD PTR FSTARG     ; x
          FTST
          FSFLG
          JNE    SHORT DRPOW4         ; x <> 0.0 and b <> 0.0
          FSTP   ST(0)
          FSTP   ST(0)
          FLD1
          JMP    SHORT DRPOWE         ; x=0.0 and b<>0.0 => res:= 1.0
DRPOW4:   FXCH   ST(1)                ; ST(0) = b, ST(1) = x
          FYL2X
          CALL   E@POW2 ; TOS = x * log2(b), result: 2 ** TOS
   IF iAPX286
          MOV    AX,WORD PTR FSTARG+2  ; HI-Word of x
          OR     AX,AX
   ELSE ; I.E. iAPX386
          MOV    EAX,FSTARG           ;  x
          OR     EAX,EAX
   ENDIF
          JNS    SHORT DRPOWE
          FLD1
          FDIV                        ; res := 1.0 / res
DRPOWE:
   IFNDEF X87
          FSTP  DWORD PTR FSTARG+12  ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@DRPOWR,12



  SIMROUTINE E@DDPOWR ; -------------------------------------------- DDPOWR
; Visible known("DDPOWR") DDPOWER; --- res:=b**x ---
; import long real b,x; export long real res;
; begin if b=0.0&&0
;       if x<=0.0&&0 then ERROR(ENO_SYS_2) else res:=0.0&&0 endif
;       elsif b<0.0&&0 then ERROR(ENO_SYS_2)
;       elsif x=0.0&&0 then res:= 1.0&&0
;       else res:=envir_DLN(b); if status > 0 then ERROR(ENO_SYS_2) endif;
;            res:=envir_DEXP(res*x); if status > 0 then ERROR(ENO_SYS_2) endif;
;       endif;
; end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD    QWORD PTR FSTARG+8    ; b
          FTST
          FSFLG
          JNE    SHORT DDPOW3          ; b <> 0.0
          FLD    QWORD PTR FSTARG      ; x
          FTST
          FSFLG
          JA     SHORT DDPOW2          ; X > 0.0 ( C3=C0=0 )
          FSTP    ST(0)                ; remove argument
DDPOW1:   FSTP    ST(0)                ; remove argument
          ERROR  128
DDPOW2:   FSTP   ST(0)
          JMP    SHORT DDPOWE          ; x<>0.0 and b=0.0 => res:= 0.0
DDPOW3:   JB     DDPOW1                ; b < 0.0 ( C3=0, C0=1 )
          FLD    QWORD PTR FSTARG      ; x
          FTST
          FSFLG
          JNE    SHORT DDPOW4          ; x <> 0.0 and b <> 0.0
          FSTP   ST(0)
          FSTP   ST(0)
          FLD1
          JMP    SHORT DDPOWE          ; x=0.0 and b<>0.0 => res:= 1.0
DDPOW4:   FXCH   ST(1)                 ; ST(0) = b, ST(1) = x
          FYL2X
          CALL   E@POW2 ; TOS = x * log2(b), result: 2 ** TOS
   IF iAPX286
          MOV    AX,WORD PTR FSTARG+6  ; HI-Word of x
          OR     AX,AX
   ELSE ; I.E. iAPX386
          MOV    EAX,FSTARG+4          ; HI-Part of x
          OR     EAX,EAX
   ENDIF
          JNS    SHORT DDPOWE          ; x > 0.0
          FLD1
          FDIV                         ; res := 1.0 / res
DDPOWE:
   IFNDEF X87
          FSTP  QWORD PTR FSTARG+16  ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@DDPOWR,16


  SIMROUTINE E@RSINH ; -------------------------------------- sinh(real)
; Visible known("RSINH") envir_RSINH;
; import real arg; export real val;
; begin val := (envir_REXP(arg) - envir_REXP(-arg)) / 2.0 end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD     DWORD PTR FSTARG     ; arg
          FABS
   IF iAPX286
          FCOM    SS:LIMREXP           ; ln(maxreal)
   ELSE ; I.E. iAPX386
          FCOM    LIMREXP              ; ln(maxreal)
   ENDIF
          FSFLG
          JBE     SHORT RSH0
          FSTP    ST(0)                ; remove argument
          ERROR   26                   ; argument too big
RSH0:     FLDL2E
          FMUL
          CALL    E@POW2  ; TOS= arg * log2(e), result: 2 ** TOS
          FLD1
          FDIV    ST,ST(1)  ; St(0)=e**-abs(arg), St(1)=e**abs(arg)
   IF iAPX286
          MOV     AX,FSTARG+2
          OR      AX,AX
   ELSE ; I.E. iAPX386
          MOV     EAX,FSTARG
          OR      EAX,EAX
   ENDIF
          JNS     SHORT RSH1
          FSUBR
          JMP     SHORT RSH2
RSH1:     FSUB
RSH2:     FLD1
          FCHS
          FXCH
          FSCALE
          FSTP    ST(1)
   IFNDEF X87
          FSTP  DWORD PTR FSTARG+4   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@RSINH,4


  SIMROUTINE E@SINH ; ---------------------------------- sinh(long real)
; Visible known("SINH") envir_DSINH;
; import long real arg; export long real val;
; begin val := (envir_DEXP(arg) - envir_DEXP(-arg)) / 2.0&&0 end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD     QWORD PTR FSTARG     ; arg
          FABS
   IF iAPX286
          FCOM    QWORD PTR SS:LIMDEXP ; ln(maxlongreal)
   ELSE ; I.E. iAPX386
          FCOM    QWORD PTR LIMDEXP    ; ln(maxlongreal)
   ENDIF
          FSFLG
          JBE     SHORT DSH0
          FSTP    ST(0)                ; remove argument
          ERROR   26                   ; argument too big
DSH0:     FLDL2E
          FMUL
          CALL    E@POW2  ; TOS= arg * log2(e), result: 2 ** TOS
          FLD1
          FDIV    ST,ST(1)  ; St(0)=e**-abs(arg), St(1)=e**abs(arg)
   IF iAPX286
          MOV     AX,FSTARG+6
          OR      AX,AX
   ELSE ; I.E. iAPX386
          MOV     EAX,FSTARG+4
          OR      EAX,EAX
   ENDIF
          JNS     SHORT DSH1
          FSUBR
          JMP     SHORT DSH2
DSH1:     FSUB
DSH2:     FLD1
          FCHS
          FXCH
          FSCALE
          FSTP    ST(1)
   IFNDEF X87
          FSTP  QWORD PTR FSTARG+8   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@SINH,8


  SIMROUTINE E@RCOSH ; ------------------------------------ cosh(real)
; Visible known("RCOSH") envir_RCOSH;
; import real arg; export real val;
; begin val := (envir_REXP(arg) + envir_REXP(-arg)) / 2.0 end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD     DWORD PTR FSTARG     ; arg
          FABS
   IF iAPX286
          FCOM    SS:LIMREXP           ; ln(maxreal)
   ELSE ; I.E. iAPX386
          FCOM    LIMREXP              ; ln(maxreal)
   ENDIF
          FSFLG
          JBE     SHORT RCH0
          FSTP    ST(0)                ; remove argument
          ERROR   26                   ; argument too big
RCH0:     FLDL2E
          FMUL
          CALL    E@POW2  ; TOS= arg * log2(e), result: 2 ** TOS
          FLD1
          FDIV    ST,ST(1)  ; St(0)=e**-abs(arg), St(1)=e**abs(arg)
          FADD
          FLD1
          FCHS
          FXCH
          FSCALE
          FSTP    ST(1)
   IFNDEF X87
          FSTP  DWORD PTR FSTARG+4   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@RCOSH,4


  SIMROUTINE E@COSH ; -------------------------------- cosh(long real)
; Visible known("COSH") envir_DCOSH;
; import long real arg; export long real val;
; begin val := (envir_DEXP(arg) + envir_DEXP(-arg)) / 2.0&&0 end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD     QWORD PTR FSTARG     ; arg
          FABS
   IF iAPX286
          FCOM    QWORD PTR SS:LIMDEXP ; ln(maxlongreal)
   ELSE ; I.E. iAPX386
          FCOM    QWORD PTR LIMDEXP    ; ln(maxlongreal)
   ENDIF
          FSFLG
          JBE     SHORT DCH0
          FSTP    ST(0)                ; remove argument
          ERROR   26                   ; argument too big
DCH0:     FLDL2E
          FMUL
          CALL    E@POW2  ; TOS= arg * log2(e), result: 2 ** TOS
          FLD1
          FDIV    ST,ST(1)  ; St(0)=e**-abs(arg), St(1)=e**abs(arg)
          FADD
          FLD1
          FCHS
          FXCH
          FSCALE
          FSTP    ST(1)
   IFNDEF X87
          FSTP  QWORD PTR FSTARG+8   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@COSH,8


  SIMROUTINE E@RTANH ; ------------------------------------ tanh(real)
; Visible known("RTANH") envir_RTANH;
; import real arg; export real val;
; begin real x,y; x:=envir_REXP(arg); y:=envir_REXP(-arg);
;       val := (x-y)/(x+y);
; end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD1                         ; out-of-range result= +/-1.0
          FLD     DWORD PTR FSTARG     ; arg
          FABS
   IF iAPX286
          FCOM    SS:LIMREXP           ; ln(maxreal)
   ELSE ; I.E. iAPX386
          FCOM    LIMREXP              ; ln(maxreal)
   ENDIF
          FSFLG
          JBE     SHORT RTH0
          FSTP    ST(0)                ; remove argument
          JMP     SHORT RTHe           ; arg too big, result= +/-1.0
RTH0:     FLDL2E
          FMUL
          CALL    E@POW2  ; TOS= arg * log2(e), result: 2 ** TOS
          FLD1
          FDIV    ST,ST(1)  ; St(0)=e**-abs(arg), St(1)=e**abs(arg)
          FLD     ST(0)     ; St(0)=e**x, St(1)=e**x, St(2)=e**-x
          FSUBR   ST,ST(2)
          FSTP    ST(3)     ; St(0)=e**x, St(1)=e**-x, St(2)=sinh(x)
          FADDP   ST(1),ST  ; St(0)=2*cosh(x), St(1)=2*sinh(x)
          FDIV              ; St(0)=sinh(x) / cosh(x)
   IF iAPX286
RTHe:     MOV     AX,FSTARG+2
          OR      AX,AX
   ELSE ; I.E. iAPX386
RTHe:     MOV     EAX,FSTARG
          OR      EAX,EAX
   ENDIF
          JNS     SHORT RTH1
          FCHS
RTH1:
   IFNDEF X87
          FSTP  DWORD PTR FSTARG+4   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@RTANH,4


  SIMROUTINE E@TANH ; -------------------------------- tanh(long real)
; Visible known("TANH") envir_DTANH;
; import long real arg; export long real val;
; begin long real x,y; x:=envir_DEXP(arg); y:=envir_DEXP(-arg);
;       val := (x-y)/(x+y);
; end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD1                         ; out-of-range result= +/-1.0
          FLD     QWORD PTR FSTARG     ; arg
          FABS
   IF iAPX286
          FCOM    QWORD PTR SS:LIMDEXP ; ln(maxlongreal)
   ELSE ; I.E. iAPX386
          FCOM    QWORD PTR LIMDEXP    ; ln(maxlongreal)
   ENDIF
          FSFLG
          JBE     SHORT DTH0
          FSTP    ST(0)                ; remove argument
          JMP     SHORT DTHe           ; arg too big, result= +/-1.0
DTH0:     FLDL2E
          FMUL
          CALL    E@POW2  ; TOS= arg * log2(e), result: 2 ** TOS
          FLD1
          FDIV    ST,ST(1)  ; St(0)=e**-abs(arg), St(1)=e**abs(arg)
          FLD     ST(0)     ; St(0)=e**x, St(1)=e**x, St(2)=e**-x
          FSUBR   ST,ST(2)
          FSTP    ST(3)     ; St(0)=e**x, St(1)=e**-x, St(2)=sinh(x)
          FADDP   ST(1),ST  ; St(0)=2*cosh(x), St(1)=2*sinh(x)
          FDIV              ; St(0)=sinh(x) / cosh(x)
   IF iAPX286
DTHe:     MOV     AX,FSTARG+6
          OR      AX,AX
   ELSE ; I.E. iAPX386
DTHe:     MOV     EAX,FSTARG+4
          OR      EAX,EAX
   ENDIF
          JNS     SHORT DTH1
          FCHS
DTH1:
   IFNDEF X87
          FSTP  QWORD PTR FSTARG+8   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@TANH,8




  SIMROUTINE E@RSINUS ; ----------------------------------- sin(real)
; Visible Routine RSINUS;  --- rsinus;
; import real arg; export real val;
; begin val := sinus(arg qua long real) qua real end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD   DWORD PTR FSTARG
          CALL  SINE
   IFNDEF X87
          FSTP  DWORD PTR FSTARG+4   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@RSINUS,4


  SIMROUTINE E@SINUSR ; ------------------------------ sin(long real)
; Visible Routine SINUSR;  --- sinus;
; import long real arg; export long real val;
; begin  ...  end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD   QWORD PTR FSTARG
          CALL  SINE
   IFNDEF X87
          FSTP  QWORD PTR FSTARG+8   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@SINUSR,8


  SIMROUTINE E@RCOSIN ; ----------------------------------- cos(real)
; Visible known("RCOSIN") envir_RCOS;
; import real arg; export real val;
; begin val:= envir_RSIN(arg + 1.5707963268) end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD   DWORD PTR FSTARG
          CALL  COSINE
   IFNDEF X87
          FSTP  DWORD PTR FSTARG+4   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@RCOSIN,4


  SIMROUTINE E@COSINU ; ------------------------------ cos(long real)
; Visible known("COSINU") envir_DCOS;
; import long real arg; export long real val;
; begin val:= envir_DSIN(arg + 1.5707963267949&&0) end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD   QWORD PTR FSTARG
          CALL  COSINE
   IFNDEF X87
          FSTP  QWORD PTR FSTARG+8   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@COSINU,8


  SIMROUTINE E@RTANGN ; ----------------------------------- tan(real)
; Visible known("RTANGN") envir_RTAN;
; import real arg; export real val;
; begin real h; h:=envir_RSIN(arg);
;       if status=0 then val:=h/envir_RSQRT(1.0-(h*h)) endif; -- h = 1 ?
;       --- checking of status must be done at calling place.
; end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD   DWORD PTR FSTARG
          CALL  tangent
   IFNDEF X87
          FSTP  DWORD PTR FSTARG+4   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@RTANGN,4


  SIMROUTINE E@TANGEN ; ------------------------------ tan(long real)
; Visible known("TANGEN") envir_DTAN;
; import long real arg; export long real val;
; begin long real h; h:=envir_DSIN(arg);
;       if status=0 then val:=h/envir_DSQRT(1.0&&0-(h*h)) endif; -- h=1?
;       --- checking of status must be done at calling place.
; end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD   QWORD PTR FSTARG
          CALL  tangent
   IFNDEF X87
          FSTP  QWORD PTR FSTARG+8   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@TANGEN,8


  SIMROUTINE E@RCOTAN ; ----------------------------------- cot(real)
; Visible known("RCOTAN") envir_RCOTAN; -- NOTE  cot(x) = 1 / tan(x)
; import real arg; export real val;
; begin real h; h:=envir_RCOS(arg);
;       if status=0 then val:=h/envir_RSQRT(1.0-(h*h)) endif; -- h = 1 ?
;       --- checking of status must be done at calling place.
; end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD1
          FLD   DWORD PTR FSTARG
          CALL  tangent
          FDIV
   IFNDEF X87
          FSTP  DWORD PTR FSTARG+4   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@RCOTAN,4


  SIMROUTINE E@COTANG ; ------------------------------ cot(long real)
; Visible known("COTANG") envir_DCOTAN; -- NOTE  cot(x) = 1 / tan(x)
; import long real arg; export long real val;
; begin long real h; h:=envir_DCOS(arg);
;       if status=0 then val:=h/envir_DSQRT(1.0&&0-(h*h)) endif; -- h=1?
;       --- checking of status must be done at calling place.
; end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD1
          FLD   QWORD PTR FSTARG
          CALL  tangent
          FDIV
   IFNDEF X87
          FSTP  QWORD PTR FSTARG+8   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@COTANG,8




  SIMROUTINE E@RARSIN ; ----------------------------------- asin(real)
; Visible known("RARSIN") envir_RARCSIN;
; import real arg; export real val;
; begin real h; h:= envir_RSQRT(1.0 - (arg*arg));
;       if status = 0 then val:=envir_RARCTAN(arg/(h+1.0))*2.0 endif;
;       --- checking of status must be done at calling place.
; end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD   DWORD PTR FSTARG
          CALL  ARCSIN
   IFNDEF X87
          FSTP  DWORD PTR FSTARG+4   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@RARSIN,4


  SIMROUTINE E@ARCSIN ; ------------------------------ asin(long real)
; Visible known("ARCSIN") envir_DARCSIN;
; import long real arg; export long real val;
; begin long real h; h:=envir_DSQRT(1.0&&0 - (arg*arg));
;       if status=0 then val:=envir_DARCTAN(arg/(h+1.0&&0))*2.0&&0 endif;
;       --- checking of status must be done at calling place.
; end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD   QWORD PTR FSTARG
          CALL  ARCSIN
   IFNDEF X87
          FSTP  QWORD PTR FSTARG+8   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@ARCSIN,8


  SIMROUTINE E@RARCOS ; ----------------------------------- acos(real)
; Visible known("RARCOS") envir_RARCCOS;
; import real arg; export real val;
; begin val:=1.5707963268 - envir_RARCSIN(arg);
;       --- checking of status must be done at calling place.
; end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD   DWORD PTR FSTARG
          CALL  ARCCOS
   IFNDEF X87
          FSTP  DWORD PTR FSTARG+4   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@RARCOS,4


  SIMROUTINE E@ARCCOS ; ------------------------------ acos(long real)
; Visible known("ARCCOS") envir_DARCCOS;
; import long real arg; export long real val;
; begin val:=1.5707963267949&&0 - envir_DARCSIN(arg);
;       --- checking of status must be done at calling place.
; end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD   QWORD PTR FSTARG
          CALL  ARCCOS
   IFNDEF X87
          FSTP  QWORD PTR FSTARG+8   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@ARCCOS,8


  SIMROUTINE E@RARTAN ; ----------------------------------- atan(real)
; Visible Routine RARTAN;  --- rarctan;
; import real arg; export real val;
; begin val := arctan(arg qua long real) qua real end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD   DWORD PTR FSTARG
          CALL  ARCTAN
   IFNDEF X87
          FSTP  DWORD PTR FSTARG+4   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@RARTAN,4


  SIMROUTINE E@ARCTAN ; ------------------------------ atan(long real)
; Visible Routine ARCTAN;  --- arctan;
; import long real arg; export long real val;
; begin  ...  end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD   QWORD PTR FSTARG
          CALL  ARCTAN
   IFNDEF X87
          FSTP  QWORD PTR FSTARG+8   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@ARCTAN,8


  SIMROUTINE E@RATAN2 ; ------------------------------ atan2(real,real)
; Visible known("RATAN2") envir_RATAN2;
; import real y,x; export real val;
; begin if x = 0.0
;       then if y=0.0 then status:=27; goto E endif;
;            if y>0.0 then val:=1.5707963268 else val:= -1.5707963268 endif;
;       else val:=envir_RARCTAN(y/x) endif;
;       if y > 0.0 then
;          if val < 0.0 then val := val + 3.1415926536 endif;
;       elsif y < 0.0 then
;          if val > 0.0 then val := val - 3.1415926536 endif; endif;
; E:end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD   DWORD PTR FSTARG       ; x
          FTST
          FSFLG
          FLD   DWORD PTR FSTARG+4     ; y
          JNE   SHORT RATA3            ; x <> 0.0
          FTST
          FSFLG
          JNE   SHORT RATA1            ; y <> 0.0
          FSTP  ST(0)                  ; remove argument
          FSTP  ST(0)                  ; remove argument
          ERROR 128
RATA1:
          FLDT  PIHALF
          JNB   SHORT RATA2            ; y < 0.0
          FCHS                         ; val:= -val
RATA2:    JMP   SHORT RATA4
RATA3:    JNA   SHORT RATA3N
          FTST
          FSFLG
          JNZ   SHORT RATA40
          FXCH
          FSTP  ST(0)                  ; y=0 and x>0: val:=y (=0)
          JMP   SHORT RATAE
RATA3N:   FTST
          FSFLG
          JNZ   SHORT RATA40
          FSTP  ST(0)
          FSTP  ST(0)
          FLDPI                        ; y=0 and x<0: val:=pi
          JMP   SHORT RATAE
          
RATA40:   FXCH
          FDIV                         ; val:= y/x
          CALL  ARCTAN                 ; val:= arctan(val)
RATA4:    FLD   DWORD PTR FSTARG+4     ; y
          FTST
          FSFLG
          FSTP  ST(0)
          JNA   SHORT RATA5            ; y <= 0.0
          FTST
          FSFLG
          JNB   SHORT RATAE            ; val >= 0.0
          FLDPI
          FADDP ST(1),ST               ; val:= val + pi
          JMP   SHORT RATAE
RATA5:    JNB   SHORT RATAE            ; y >= 0.0
          FTST
          FSFLG
          JNA   SHORT RATAE            ; val <= 0.0
          FLDPI
          FSUBP ST(1),ST               ; val:= val - pi
RATAE:
   IFNDEF X87
          FSTP  DWORD PTR FSTARG+8   ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@RATAN2,8


  SIMROUTINE E@ATAN2 ; --------------------- atan2(long real,long real)
; Visible known("ATAN2") envir_DATAN2;
; import long real y,x; export long real val;
; begin if x = 0.0&&0
;       then if y=0.0&&0 then status:=27; goto E endif;
;            if y>0.0&&0 then val:=  1.5707963267949&&0
;                        else val:= -1.5707963267949&&0 endif;
;       else val:=envir_DARCTAN(y/x) endif;
;       if y > 0.0 then
;          if val < 0.0 then val := val + 3.1415926535898&&0 endif;
;       elsif y < 0.0 then
;          if val > 0.0 then val := val - 3.1415926535898&&0 endif; endif;
; E:end;
;
; Result returned on 80x87-stack.         -- WITH87
;                 in WEITEK register.     -- WTL167
;                 on 80x86-stack.         -- else
IF WITH87
          FLD   QWORD PTR FSTARG       ; x
          FTST
          FSFLG
          FLD   QWORD PTR FSTARG+8     ; y
          JNE   SHORT DATA3            ; x <> 0.0
          FTST
          FSFLG
          JNE   SHORT DATA1            ; y <> 0.0
          FSTP  ST(0)                  ; remove argument
          FSTP  ST(0)                  ; remove argument
          ERROR 128
DATA1:
          FLDT  PIHALF
          JNB   SHORT DATA2            ; y < 0.0
          FCHS                         ; val:= -val
DATA2:    JMP   SHORT DATA4
DATA3:    JNA   SHORT DATA3N
          FTST
          FSFLG
          JNZ   SHORT DATA40
          FXCH
          FSTP  ST(0)                  ; y=0 and x>0: val:=y (=0)
          JMP   SHORT DATAE
DATA3N:   FTST
          FSFLG
          JNZ   SHORT DATA40
          FSTP  ST(0)
          FSTP  ST(0)
          FLDPI                        ; y=0 and x<0: val:=pi
          JMP   SHORT DATAE
          
DATA40:   FXCH
          FDIV                         ; val:= y/x
          CALL  ARCTAN                 ; val:= arctan(val)
DATA4:    FLD   QWORD PTR FSTARG+8     ; y
          FTST
          FSFLG
          FSTP  ST(0)
          JNA   SHORT DATA5            ; y <= 0.0
          FTST
          FSFLG
          JNB   SHORT DATAE            ; val >= 0.0
          FLDPI
          FADDP ST(1),ST               ; val:= val + pi
          JMP   SHORT DATAE
DATA5:    JNB   SHORT DATAE            ; y >= 0.0
          FTST
          FSFLG
          JNA   SHORT DATAE            ; val <= 0.0
          FLDPI
          FSUBP ST(1),ST               ; val:= val - pi
DATAE:
   IFNDEF X87
          FSTP  QWORD PTR FSTARG+16  ; Export value
          WAIT
   ENDIF
ELSE
   IF WTL167
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
      ; NY KODE INN HER !!!
   ELSE
      .ERR
   ENDIF
ENDIF
  ENDROUTINE E@ATAN2,16




IF WITH87

       EVEN
REDUCE   PROC     NEAR  ; -- Argument in ST(0)
; reduce by emulating fprem - note that very large arguments MUST be
; reduced by very large amounts first before computing modulo PIQUART!!!
         FABS
   IF iAPX286
         XCHG     AX,SI                ; Save ax
         FICOM    WORD PTR SS:MINRED
   ELSE ; I.E. iAPX386
         XCHG     EAX,ESI              ; Save eax
         FICOM    WORD PTR MINRED
   ENDIF
         FSFLG
         JB       SHORT MICRORED
         FLD      ST(0)
         FLDPI
         FADD     ST,ST(0)
         FDIV     ST(1),ST(0)
         FXCH
         FDOWN2I 
         FMUL     ST,ST(1)
         FSUBP    ST(2),ST
         FADD
MICRORED:
         FLDT     PIQUART
         FXCH
IF iAPX286
         XOR      BX,BX                ; *** EMULATE 'fprem' ***
FPREM1:  FCOM     ST(1)                ; *** EMULATE 'fprem' ***
      ;  WAIT                          ; *** EMULATE 'fprem' ***
         FSFLG                         ; *** EMULATE 'fprem' ***
         JB       SHORT FPREM1x        ; *** EMULATE 'fprem' ***
         FSUB     ST,ST(1)             ; *** EMULATE 'fprem' ***
         INC      BX                   ; *** EMULATE 'fprem' ***
         JMP      SHORT FPREM1         ; *** EMULATE 'fprem' ***
FPREM1x: AND      BX,7                 ; *** EMULATE 'fprem' ***
         MOV      BH,SS:FPREMTAB[BX]   ; *** EMULATE 'fprem' ***
         XCHG     AX,SI                ; restore ax
ELSE ; I.E. iAPX386
         XOR      EBX,EBX              ; *** EMULATE 'fprem' ***
FPREM1:  FCOM     ST(1)                ; *** EMULATE 'fprem' ***
      ;  WAIT                          ; *** EMULATE 'fprem' ***
         FSFLG                         ; *** EMULATE 'fprem' ***
         JB       SHORT FPREM1x        ; *** EMULATE 'fprem' ***
         FSUB     ST,ST(1)             ; *** EMULATE 'fprem' ***
         INC      EBX                  ; *** EMULATE 'fprem' ***
         JMP      SHORT FPREM1         ; *** EMULATE 'fprem' ***
FPREM1x: AND      EBX,7                ; *** EMULATE 'fprem' ***
         MOV      BH,FPREMTAB[EBX]     ; *** EMULATE 'fprem' ***
         XCHG     EAX,ESI              ; restore eax
ENDIF
         ret
REDUCE   ENDP
ENDIF



IF WITH87
       EVEN
COSINE   PROC     NEAR  ; -- Argument in ST(0)
         ftst
         FSFLG
         mov      cl,1                 ; Signal COSINE
         jpe      SHORT SCERR
         jnz      SHORT enter_sine
         fstp     st(0)
         fld1
         ret      ; cos(0)=1
SCERR:   FSTP     ST(0)               ; remove argument
         ERROR    128
         ret

SINE:    ; *****  ENTRY  -- Argument in ST(0)
         ftst
         FSFLG
         mov      cl,0                 ; Signal SINE
         jpe      SHORT SCERR
         jnz      SHORT enter_sine
         ret      ; sin(0)=0
enter_sine:
         call near ptr REDUCE
         or       cl,cl
         jz       SHORT sine_select
    shl ah,1  ; NY NY NY
         and      ah,0FDH ; not high(mask cond1)
         or       bh,80H
         add      bh,40H ; high(mask cond3)
         mov      al,0
         rcl      al,1
         xor      bh,al
sine_select:
         test     bh,02H ; high(mask cond1)
         jz       SHORT no_sine_reverse
         fsub
         jmp      short do_sine_fptan
no_sine_reverse:
         fstp     st(1)
do_sine_fptan:
         fptan
         test     bh,42H ; high(mask cond3 + mask cond1)
         jpo      SHORT X_numerator
         FLD      ST(1)
         jmp      short finish_sine
X_numerator:
         FLD      ST(0)
         fxch     st(2)
finish_sine:
         fmul     st,st(0)
         fxch
         fmul     st,st(0)
         fadd
         fsqrt
         and      bh,01H ; high(mask cond0)
    shl ah,1  ; NY NY NY
         and      ah,02H ; high(mask cond1)
         or       bh,ah
         jpe      SHORT positive_sine
         fchs
positive_sine:
         fdiv
         ret
COSINE   ENDP
ENDIF



IF WITH87
       EVEN
tangent  PROC     NEAR
         ftst
         FSFLG
         jpe      SHORT TANERR
         jnz      SHORT enter_tan
         ret      ; tan(0)=0
TANERR:  FSTP     ST(0)               ; remove argument
         ERROR    128
         ret

enter_tan:
         call near ptr REDUCE
         test     bh,02H        ; high(mask cond1)
         jz       SHORT no_tan_reverse
         fsub
         jmp      short do_tangent
no_tan_reverse:
         fstp     st(1)
do_tangent:
         fptan                  ; Evaluate tan(arg), 0<arg<pi/4
         mov      al,bh         ; Find which quadrant, and modify
IF iAPX286
         and      ax,0140H      ; Mask cond1 + high(mask cond3)
ELSE ; I.E. iAPX386
         and      eax,0140H     ; Mask cond1 + high(mask cond3)
ENDIF
         test     bh,42H        ; high(mask cond1 + mask cond3)
         jpo      SHORT reverse_divide
         or       al,ah
         jpe      SHORT positive_divide
         fchs
positive_divide:
         fdiv                   ; Make inverse
         ret
reverse_divide:
         or       al,ah
         jpe      SHORT positive_r_divide
         fchs
positive_r_divide:
         fdivr
         ret
tangent  ENDP
ENDIF




IF WITH87
       EVEN
ARCTAN  PROC  NEAR  ; Argument on 87-Stack
        FTST
        FSFLG
        JNE   SHORT ATN0
; ****  Argument = 0
        RET                      ;  atan(0)=0
ATN0:   LAHF
        MOV   CL,AH              ;  save flags of argument | zero
        FABS
        FLD1
        FCOMP
        FSFLG
        JNE   SHORT ATN1
; ****  Argument = 1
        FSTP  ST(0)
        FLDT  PIQUART
        JMP   SHORT ATNE
ATN1:   JA    SHORT ATN2
; ****  Argument > 1
        FLD1
        FXCH   ST(1)
        FPATAN                   ; atan(1/abs(x))
        FLDT   PIHALF
        FSUBRP ST(1),ST          ; atan(abs(x)) = PI/2 - atan(1/abs(x))
        JMP    SHORT ATNE
; ****  Argument < 1
ATN2:   FLD1
        FPATAN                   ; atan(abs(x)/1)
ATNE:   MOV    AH,CL
        SAHF
        JNB  SHORT ATNX
        FCHS                     ; atan(-x) = - atan(x)
ATNX:   RET
ARCTAN  ENDP
ENDIF




IF WITH87
       EVEN
ARCSIN  PROC  NEAR  ; Argument on 87-Stack
        FTST
        FSFLG
        JNE   SHORT ASN0
; ****  Argument = 0
        RET                      ;  asin(0)=0
ASN0:   LAHF
        MOV   DL,AH              ;  save flags of argument | zero
IF iAPX286
        MOV   CX,0               ;  signal: Arcsin
ELSE ; I.E. iAPX286
        MOV   ECX,0              ;  signal: Arcsin
ENDIF
        FABS
        FLD1
        FCOMP
        FSFLG
        JA    SHORT ASC1
        JE    SHORT API2
        FSTP  ST(0)               ; remove argument
        ERROR 128
; ****  asin(1)=acos(0)=pi/2
API2:   FSTP  ST(0)
        FLDT  PIHALF
        JMP   SHORT ASCE

ARCCOS: ; ****** Entry
        FTST
        FSFLG
        LAHF
        MOV   DL,AH              ;  save flags of argument | zero
        MOV   CL,255             ;  signal: Arccos
        MOV   CH,255             ;  signal: Arccos
        JE    SHORT ASCE ; API2  ;  acos(0)=pi/2
        FABS
        FLD1
        FCOMP
        FSFLG
        JA    SHORT ASC1
        JE    SHORT AZRO
        FSTP  ST(0)               ; remove argument
        ERROR 128
; ****  Argument = 1
AZRO:   FSTP  ST(0)
        MOV   AH,DL
        SAHF
        JA    SHORT ASCONE
        FLDPI                    ;  acos(-1)=pi
        RET
ASCONE: FLDZ                     ;  acos(1)=0
        RET

; ****  Common Code  ****

ASC1:   FLD   ST
        FMUL  ST,ST(1)
        FLD1
        FSUBRP ST(1),ST
        FSQRT                    ; St(0)=sqrt(1-x*x), St(1)=abs(x)
        FCOM
        FSFLG
        JA    SHORT ASC2
; ****  Argument > 1 to FPATAN
        FXCH
        NOT   CL
        FPATAN                   ; atan( sqrt(1-x*x) / abs(x) )
        JMP   SHORT ASCE
; ****  Argument < 1 to FPATAN
ASC2:
        FPATAN                   ; atan( abs(x) / sqrt(1-x*x) )
ASCE:   OR    CL,CL
        JZ    SHORT ASCF
        FLDT  PIHALF
        FSUBRP ST(1),ST         ; arcsin(abs(x)) = PI/2 - arccos(abs(x))
ASCF:   MOV   AH,DL
        SAHF
        JNB   SHORT ASCX
        OR    CH,CH
        JZ    SHORT ASCS
        FLDPI
        FSUBRP ST(1),ST          ; arccos(-x) = PI - arccos(x)
        JMP   SHORT ASCX
ASCS:   FCHS                     ; arcsin(-x) = - arcsin(x)
ASCX:   RET
ARCSIN  ENDP
ENDIF


        EVEN
IF iAPX286
S@ENV_TEXT ENDS
ELSE ; I.E. iAPX386
_TEXT ENDS
ENDIF
      END
