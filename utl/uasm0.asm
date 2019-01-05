NAME       ENVIR
; ---------------------------------------------------------------
; ---  COPYRIGHT 1989 by                                      ---
; ---  Simula a.s.                                            ---
; ---  Oslo, Norway                                           ---
; ---                                                         ---
; ---                                                         ---
; ---             P O R T A B L E     S I M U L A             ---
; ---                                                         ---
; ---            F O R    i A P X    2 8 6 / 3 8 6            ---
; ---                                                         ---
; ---                                                         ---
; ---       T  H  E     E  N  V  I  R  O  N  M  E  N  T       ---
; ---                                                         ---
; ---------------------------------------------------------------

INCLUDE macro.def

IF iAPX286
        EXTRN   SIM_MAIN:FAR
   IF OS2
        EXTRN   DOSEXIT:FAR
        EXTRN   DOSWRITE:FAR
   ENDIF
   IF XENIX
        EXTRN   _exit:FAR
        EXTRN   _errno:WORD
        EXTRN   _write:FAR
   ENDIF
ELSE ; I.E. iAPX386
        EXTRN   SIM_MAIN:NEAR
        EXTRN   _exit:NEAR
        EXTRN   _errno:DWORD
        EXTRN   _write:NEAR
ENDIF


; ***********************************************************
; ******     S  T  A  C  K     S  E  C  T  I  O  N     ******
; ***********************************************************
; ******  A stack segment is defined for all iAPX286   ******
; ******  systems except DOS without without Banking.  ******
; ******  This is done due to an error in LINK.EXE     ******
; ***********************************************************
IFDEF BANKING
PURE_DOS EQU FALSE
ELSE
PURE_DOS EQU DOS
ENDIF

IF iAPX286 AND NOT PURE_DOS
STACK     SEGMENT
          DW STKHED DUP(?)
           DW 500 DUP(?)
  GLOBAL  G@STKBEG DB <STKLNG DUP(?)>
  GLOBAL  G@STKEND DB <100    DUP(?)>
           DW 500 DUP(?)
STACK     ENDS
ENDIF


; ******************************************************
; ******    D  A  T  A     S  E  C  T  I  O  N    ******
; ******************************************************
_DATA      SEGMENT
   GLOBAL  G@OSSTAT  DB <AllignFac DUP(0)>
;  GLOBAL  G@TMPQNT  DB <8*AllignFac DUP(0)>
;  GLOBAL  G@TMPCALL DB <4 DUP(0)>
;  GLOBAL  G@TMP8687 DB <80 DUP(0)>
   GLOBAL  G@PSPREF  DB <4 DUP(0)>
   GLOBAL  G@ENVSEG  DB <4 DUP(0)>
   GLOBAL  G@XNXREF  DB <4 DUP(0)>
IF PURE_DOS
; ******************************************************
; ******  Runtime Stack Under Non-Banking MS-DOS  ******
; ******************************************************
; ******  Old versions of LINK.EXE does not treat ******
; ******  DGROUP _DATA,STACK correctly.           ******
; ******  Therefore we have to define DOS-STACK   ******
; ******  as part of the DATA SECTION             ******
; ******************************************************
           DW STKHED DUP(?)
           DW 500 DUP(?)
   GLOBAL  G@STKBEG DB <STKLNG DUP(?)>
   GLOBAL  G@STKEND DB <100    DUP(?)>
           DW 500 DUP(?)
ENDIF
_DATA      ENDS




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


IF DOS
; *************************************************************
; ******   M A I N   E N T R Y   F R O M   M S - D O S   ******
; *************************************************************
_main PROC    FAR
      PUBLIC  _main
      ASSUME  DS:NOTHING
      MOV     SI,SEG _DATA
      MOV     SS,SI
      MOV     SS:G@PSPREF+2,DS
      XOR     AX,AX
      MOV     SS:G@PSPREF,AX
      MOV     SP,OFFSET G@STKEND+4
      XOR     BP,BP
; *** Initiate and Go User Program
      CALL    FAR PTR SIM_MAIN
      INT     2   ; Impossible to reach this instruction
_main ENDP
ENDIF
  

IF OS2
; *******************************************************************
; ***********   M A I N   E N T R Y   F R O M   O S / 2   ***********
; *******************************************************************
; *** CS:IP Points to the program's initial entry point           ***
; ***       as specified in the .exe header                       ***
; *** SS:SP Points to the stack specified in the .exe header      ***
; *** DS    Points to the automatic data segment specified        ***
; ***       in the .exe header                                    ***
; *** ES    Contains zero                                         ***
; *** AX    The Environment segment selector                      ***
; *** BX    Offset of the command line in the Environment Segment ***
; *** CX    The size of the automatic data segment (0=65536)      ***
; *** BP    Contains zero                                         ***
; *******************************************************************
_main PROC    FAR
      PUBLIC  _main
      ASSUME  DS:NOTHING
      MOV     SS:G@ENVSEG+2,AX
      MOV     SS:G@ENVSEG,BX
      MOV     SP,OFFSET G@STKEND+4
      XOR     BP,BP
      CALL    FAR PTR SIM_MAIN
      INT     2   ; Impossible to reach this instruction
_main ENDP
ENDIF


IF XENIX AND iAPX286
; *******************************************************************
; ******   M A I N   E N T R Y   F R O M   X E N I X / 2 8 6   ******
; *******************************************************************
_main PROC    FAR
      PUBLIC  _main
      ASSUME  DS:NOTHING
      PUSH    BP
      MOV     BP,SP
;  ....
      MOV     G@XNXREF+2,SS
      LEA     AX,FSTARG
      MOV     G@XNXREF,AX
      XOR     BP,BP
      CALL    FAR PTR SIM_MAIN
      INT     2   ; Impossible to reach this instruction
_main ENDP
ENDIF
 

IF iAPX386
; *****************************************************************************
; ******   M A I N   E N T R Y   F R O M   X E N I X / U N I X / 3 8 6   ******
; *****************************************************************************
_main PROC    NEAR
      PUBLIC  _main
      PUSH    EBP
      MOV     EBP,ESP
;  ....
      LEA     EAX,FSTARG
      MOV     G@XNXREF,EAX
      XOR     EBP,EBP
      CALL    SIM_MAIN
      INT     2   ; Impossible to reach this instruction
_main ENDP
ENDIF


IF XENIX OR UNIX
; *************************************************
; ******    S E T   U N I X   S T A T U S    ******
; *************************************************
   IF iAPX286
E@SSTAT PROC    FAR            ; Set UNIX/XENIX Status
        PUBLIC  E@SSTAT
        ASSUME  DS:NOTHING
        POP     AX
        PUSH    CS
        PUSH    AX
        MOV     AX,_errno
        MOV     SS:G@OSSTAT,AX
        RET
E@SSTAT ENDP
   ELSE ; I.E. iAPX386
E@SSTAT PROC    NEAR           ; Set UNIX/XENIX Status
        PUBLIC  E@SSTAT
        MOV     EAX,_errno
        MOV     G@OSSTAT,EAX
        RET
E@SSTAT ENDP
   ENDIF
ENDIF


IF DOS
; ***************************************************
; ******    X Q T    D O S    C O M M A N D    ******
; ***************************************************
SAV_SS   DW  0
SAV_SP   DW  0
PARBLK   DW  0  ; No Redefinition of Environment
         DW  0  ; Command Line'Offset
         DW  0  ; Command Line'Segment
         DD -1  ; No FCB #1
         DD -1  ; No FCB #2
  SIMROUTINE E@EXEC
;  ****  Update PARBLK  ****
         MOV    AX,FSTARG+2
         ADD    AX,FSTARG
         MOV    CS:PARBLK+2,AX
         MOV    AX,FSTARG+4
         MOV    CS:PARBLK+4,AX
;  ****  ES:BX <-- Pointer to PARBLK
         PUSH   CS
         POP    ES
         MOV    BX,OFFSET PARBLK
;  ****  DS:DX <-- Pointer to File-Name
         LDS    DX,FSTARG+8
         ADD    DX,FSTARG+6
;  ****  Save SS and SP
         MOV    CS:SAV_SS,SS
         MOV    CS:SAV_SP,SP
;  ****  Do DOS-Call EXEC
         MOV    AL,0
         MOV    AH,4BH
         INT    21H
;  ****  Restore SS and SP  ****
         MOV    SS,CS:SAV_SS
         MOV    SP,CS:SAV_SP
;  ****  Test For ERROR  ****
         JNC    LL2
         MOV    SS:G@OSSTAT,AX
;  ****  Get Return Code  ****
LL2:     MOV    AH,4DH
         INT    21H           ; AX := Return Code
         MOV    FSTARG+12,AX
  ENDROUTINE E@EXEC,12
ENDIF


IF iAPX286
; *************************************************************************
; ******  E M U L A T E   3 2 - B I T   M U L T I P L I C A T I O N  ******
; *************************************************************************
  SIMROUTINE E@IMUL ; (DX:AX) := (DX:AX) * (BX:CX)
           XOR     DI,DI    ; Sign flag
           OR      DX,DX
           JGE     LMUL1
           NOT     DI
           NEG2    DX,AX
LMUL1:     OR      BX,BX
           JGE     LMUL2
           NOT     DI
           NEG2    BX,CX     ; Corr 12/8. GS
LMUL2:     PUSH    DX
           XCHG    AX,BX
           XCHG    BX,CX
           MUL     CX
           XCHG    AX,BX
           MOV     SI,AX
           MUL     CX
           ADD     BX,DX
           XCHG    AX,SI
           POP     CX
           MUL     CX
           ADD     BX,AX
           MOV     DX,BX
           MOV     AX,SI
           OR      DI,DI
           JE      LMULE
           NEG2    DX,AX
LMULE:
  ENDROUTINE E@IMUL
ENDIF


IF iAPX286
; *************************************************************
; ******  E M U L A T E   3 2 - B I T   D I V I S I O N  ******
; *************************************************************
  SIMROUTINE E@IDIV ; (DX:AX) := (DX:AX) / (BX:CX)
           MOV     BP,SP
           XOR     DI,DI    ; Sign flag
           OR      DX,DX
           JGE     LDIV1
           NOT     DI
           NEG2    DX,AX
LDIV1:     OR      BX,BX
           JGE     LDIV2
           NOT     DI
           NEG2    BX,CX     ; Corr 12/8. GS
LDIV2:     PUSH    BX
           PUSH    CX
           PUSH    DX
           PUSH    AX
           MOV     AX,[BP]-2

           OR      AX,AX
           JNE     LDIV3
           MOV     CX,[BP]-4
           MOV     AX,[BP]-6
           XOR     DX,DX
           DIV     CX
           MOV     BX,AX
           MOV     AX,[BP]-8
           DIV     CX
           MOV     DX,BX
           JMP     NEAR PTR LDIV7
LDIV3:     MOV     BX,AX
           MOV     CX,[BP]-4
           MOV     DX,[BP]-6
           MOV     AX,[BP]-8
LDIV4:     SAR     BX,1
           RCR     CX,1
           SAR     DX,1
           RCR     AX,1
           OR      BX,BX
           JNE     LDIV4
           DIV     CX
           MOV     SI,AX
; ------------------------------------------------------
           MOV     BX,AX
           MUL     WORD PTR [BP]-2
           XCHG    AX,BX
           MUL     WORD PTR [BP]-4
           ADD     DX,BX
; ------------------------------------------------------
           CMP     DX,[BP]-6
           JA      LDIV5
           JB      LDIV6
           CMP     AX,[BP]-8
           JBE     LDIV6
LDIV5:     SUB     SI,1
LDIV6:     XOR     DX,DX
           MOV     AX,SI
LDIV7:     OR      DI,DI      ; Test sign
           JE      LDIVE
           NEG2    DX,AX
LDIVE:
  ENDROUTINE E@IDIV
ENDIF


IF iAPX286
; ***************************************************************
; ******  E M U L A T E   3 2 - B I T   R E M A I N D E R  ******
; ***************************************************************
  SIMROUTINE E@IREM ; (DX:AX) := (DX:AX) rem (BX:CX)
           XOR     DI,DI    ; Sign flag
           OR      DX,DX
           JGE     LREM1
           NOT     DI
           NEG2    DX,AX
LREM1:     OR      BX,BX
           JGE     LREM2
           NEG2    BX,CX     ; Corr 12/8. GS
LREM2:     PUSH    BX
           PUSH    CX
           PUSH    DX
           PUSH    AX
           MOV     AX,[BP]-2

           OR      AX,AX
           JNE     LREM3
           MOV     CX,[BP]-4
           MOV     AX,[BP]-6
           XOR     DX,DX
           DIV     CX
           MOV     AX,[BP]-8
           DIV     CX
           MOV     AX,DX
           XOR     DX,DX
           OR      DI,DI
           JNE     LREM7
           JMP     NEAR PTR LREME
LREM3:     MOV     BX,AX
           MOV     CX,[BP]-4
           MOV     DX,[BP]-6
           MOV     AX,[BP]-8
LREM4:     SAR     BX,1
           RCR     CX,1
           SAR     DX,1
           RCR     AX,1
           OR      BX,BX
           JNE     LREM4
           DIV     CX
; ------------------------------------------------------
           MOV     BX,AX
           MUL     WORD PTR [BP]-2
           XCHG    AX,BX
           MUL     WORD PTR [BP]-4
           ADD     DX,BX
; ------------------------------------------------------
           CMP     DX,[BP]-6
           JA      LREM5
           JB      LREM6
           CMP     AX,[BP]-8
           JBE     LREM6
LREM5:     SUB     AX,[BP]-4
           SBB     DX,[BP]-2
LREM6:     SUB     AX,[BP]-8
           SBB     DX,[BP]-6
           OR      DI,DI
           JNE     LREME
LREM7:     NEG2    DX,AX
LREME:
  ENDROUTINE E@IREM
ENDIF


IF iAPX286
S@ENV_TEXT ENDS
ELSE ; I.E. iAPX386
_TEXT ENDS
ENDIF

       END   _main
