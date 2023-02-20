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
;
; ---------------------------------------------------------------
; ---  Simula a.s sept. 1992
; ---  Entry/exit code (in addition to DOSSEG.ASM for DOS/Os2)
; ---
; ---  Provided in source to allow local modifications to these
; ---  two files (IF you know what you are doing!!!)
;
; ---------------------------------------------------------------
; ---  The following MUST be done in this module:
; ---	init of XNXREF and INITSP
; ---   push 2 zeroes on stack before 
; ---   JMP SIM_MAIN   with BP zero!!!
; ---   DO NOT CHANGE GLOBAL declarations, not even their sequence
; ---	WARNING: Simula treatment of stack is in conflict with C's.
; ---		 Sometimes, RTS will set SP to it's initial value
; ---            and thereby destroying whatever (assumed) garbage
; ---		 is on top ... this makes callback somewhat risky!!
; ---            See e.g. EGOTO
; ---------------------------------------------------------------


INCLUDE macro.def

IF iAPX286
        EXTRN   SIM_MAIN:FAR
        EXTRN   E@TRAPH:FAR
        EXTRN   K@ERROR:FAR
   IF DOS
     IFDEF VOPTV
        EXTRN   _exit:FAR   ; in DOSSEG or C
     ENDIF
     IFDEF BANKING
        EXTRN   E@BEGBNK:NEAR
        EXTRN   E@LDBNK:NEAR
        EXTRN   S@BNKTAB:WORD
     ENDIF
   ENDIF
   IF OS2
        EXTRN   DOSEXIT:FAR
        EXTRN   DOSWRITE:FAR
        EXTRN   DOSSETVEC:FAR
        EXTRN   KBDCHARIN:FAR
   ENDIF
   IF XENIX
        EXTRN   _exit:FAR
        EXTRN   _errno:WORD
        EXTRN   _signal:FAR
        EXTRN   _write:FAR
;         EXTRN   _getchar:FAR
   ENDIF
ELSE ; I.E. iAPX386
        EXTRN   SIM_MAIN:NEAR
        EXTRN   E@TRAPH:NEAR
        EXTRN   K@ERROR:NEAR
        EXTRN   _exit:NEAR
        EXTRN   _errno:DWORD
        EXTRN   _signal:NEAR
        EXTRN   _write:NEAR
;         EXTRN   _getchar:NEAR
   IF UNIX
        EXTRN   _fcntl:NEAR
   ENDIF
ENDIF




; ***********************************************************
; ******     S  T  A  C  K     S  E  C  T  I  O  N     ******
; ***********************************************************
IF iAPX286
STACK     SEGMENT
          DW STKHED DUP(?)
  GLOBAL  G@STKBEG DB <STKLNG DUP(?)>
  GLOBAL  G@STKEND DB <100    DUP(?)>
STACK     ENDS
ENDIF

; ******************************************************
; ******    D  A  T  A     S  E  C  T  I  O  N    ******
; ******************************************************
_DATA      SEGMENT
   GLOBAL  G@CURINS  DB <4 DUP(0)>
   GLOBAL  G@BIOREF  DB <4 DUP(0)>
   GLOBAL  G@INIERR  DB <4 DUP(0)>
   GLOBAL  G@TROUTM  DD 0                    ; Trap's OuterMost
   GLOBAL  G@ALLOCO  DB <4 DUP(0)>           ; NEW
   GLOBAL  G@FREEOB  DB <4 DUP(0)>           ; NEW
     GLOBAL  G@ITSIZE  DB <AllignFac DUP(0)>
     GLOBAL  G@EXMODE  DB <AllignFac DUP(0)>
   GLOBAL  G@STATUS  DB <AllignFac DUP(0)>
   GLOBAL  G@OSSTAT  DB <AllignFac DUP(0)>
   GLOBAL  G@TMPQNT  DB <8*AllignFac DUP(0)>
;  GLOBAL  G@TMPCALL DB <4 DUP(0)>
   GLOBAL  G@TMP8687 DB <80 DUP(0)>
IFNDEF VOPTV
   GLOBAL  G@PSPREF  DB <4 DUP(0)>
ENDIF
   GLOBAL  G@ENVSEG  DB <4 DUP(0)>
   GLOBAL  G@XNXREF  DB <4 DUP(0)>
   GLOBAL  G@MAXINT  DD 07FFFFFFFH          ; 32 bit max integer
   GLOBAL  G@MININT  DD 080000000H          ; 32 bit min integer
   GLOBAL  G@MAXRNK  DD 255
   GLOBAL  G@MAXREA  DD 07F7FFFFFH          ; 32 bit max real
   GLOBAL  G@MINREA  DD 0FF7FFFFFH          ; 32 bit min real
   GLOBAL  G@MAXLRL  DQ 07FEFFFFFFFFFFFFFH  ; 64 bit max real
   GLOBAL  G@MINLRL  DQ 0FFEFFFFFFFFFFFFFH  ; 64 bit min real
;    GLOBAL  G@ITSIZE  DB <AllignFac DUP(0)>  ; - moved for 4-byte allign
;    GLOBAL  G@EXMODE  DB <AllignFac DUP(0)>  ; - moved for 4-byte allign
   GLOBAL  G@MAXLEN  DD 200
   GLOBAL  G@INPLTH  DD 132
   GLOBAL  G@OUTLTH  DD 132
   GLOBAL  G@PRGINF  DD 0                   ; ModuleInfo, none:Not Available
;  GLOBAL  G@INITSP  DB <AllignFac DUP(0)>  ; Initial value of SP
   GLOBAL  G@INITSP  DB <4 DUP(0)>          ; Initial value of SP +- allign
IF DOS
   IFDEF BANKING
        GLOBAL  G@BNKINF DD S@BNKTAB
        GLOBAL  G@BNKBUF DD G@FRMTAB
        GLOBAL  G@FRMTAB DB <256 DUP(0)>
        GLOBAL  G@FRMBAS DB <256 DUP(0)>
   ELSE
        GLOBAL  G@BNKINF DB <4 DUP(0)>
        GLOBAL  G@BNKBUF DB <4 DUP(0)>
        GLOBAL  G@FRMTAB DB <4 DUP(0)>
        GLOBAL  G@FRMBAS DB <4 DUP(0)>
   ENDIF
ENDIF
IF iAPX286
 IF DOS
   GLOBAL  G@STATE   DW <14 DUP(0)>  ; -- TRAP PACKET UNDER MS-DOS
;  G@STATE+00: range(0:127) OsCode;
;  G@STATE+01: range(0:127) SimCode;    -- S-Port trap code
;  G@STATE+02: range(0:MaxWord) rSP;    -- Saved register SP
;  G@STATE+04: range(0:MaxWord) rDS;    -- Saved register DS
;  G@STATE+06: range(0:MaxWord) rES;    -- Saved register ES
;  G@STATE+08: range(0:MaxWord) rDI;    -- Saved register DI
;  G@STATE+10: range(0:MaxWord) rSI;    -- Saved register SI
;  G@STATE+12: range(0:MaxWord) rBX;    -- Saved register BX
;  G@STATE+14: range(0:MaxWord) rDX;    -- Saved register DX
;  G@STATE+16: range(0:MaxWord) rCX;    -- Saved register CX
;  G@STATE+18: range(0:MaxWord) rAX;    -- Saved register AX
;  G@STATE+20: range(0:MaxWord) OldBP;  -- Old BP
;  G@STATE+22: infix(AnyAddr) TADR;     -- Trap address (I.e. CS:IP)
;  G@STATE+26: range(0:MaxWord) FLAGS;  -- Saved flags
   ENDIF
   IF OS2
   GLOBAL  G@STATE   DW <14 DUP(0)>  ; -- TRAP PACKET UNDER OS/2
;  G@STATE+00: range(0:127) OsCode;
;  G@STATE+01: range(0:127) SimCode;    -- S-Port trap code
;  G@STATE+02: range(0:MaxWord) rSP;    -- Saved register SP
;  G@STATE+04: range(0:MaxWord) rDS;    -- Saved register DS
;  G@STATE+06: range(0:MaxWord) rES;    -- Saved register ES
;  G@STATE+08: range(0:MaxWord) rDI;    -- Saved register DI
;  G@STATE+10: range(0:MaxWord) rSI;    -- Saved register SI
;  G@STATE+12: range(0:MaxWord) rBX;    -- Saved register BX
;  G@STATE+14: range(0:MaxWord) rDX;    -- Saved register DX
;  G@STATE+16: range(0:MaxWord) rCX;    -- Saved register CX
;  G@STATE+18: range(0:MaxWord) rAX;    -- Saved register AX
;  G@STATE+20: range(0:MaxWord) OldBP;  -- Old BP
;  G@STATE+22: infix(AnyAddr) TADR;     -- Trap address (I.e. CS:IP)
;  G@STATE+26: range(0:MaxWord) FLAGS;  -- Saved flags
 ENDIF
 IF XENIX
   GLOBAL  G@STATE   DW <18 DUP(0)>  ; -- TRAP PACKET UNDER XENIX/286
;  G@STATE+00: range(0:MaxWord) OsCode;  -- Signal number
;  G@STATE+02: range(0:MaxWord) SimCode; -- S-Port trap code
;  G@STATE+04: range(0:MaxWord) rSP;     -- Saved register SP
;  G@STATE+06: range(0:MaxWord) rDS;     -- Saved register DS
;  G@STATE+08: range(0:MaxWord) rES;     -- Saved register ES
;  G@STATE+10: range(0:MaxWord) rDI;     -- Saved register DI
;  G@STATE+12: range(0:MaxWord) rSI;     -- Saved register SI
;  G@STATE+14: range(0:MaxWord) rBX;     -- Saved register BX
;  G@STATE+16: range(0:MaxWord) rDX;     -- Saved register DX
;  G@STATE+18: range(0:MaxWord) rCX;     -- Saved register CX
;  G@STATE+20: range(0:MaxWord) rAX;     -- Saved register AX
;  G@STATE+22: range(0:MaxWord) FLAGS;   -- Saved flags
;  G@STATE+24: infix(AnyAddr) CONT;      -- Continuation address (or +0)
;  G@STATE+28: range(0:MaxWord) OldBP;   -- Old BP
;  G@STATE+30: range(0:MaxWord) UserBP;  -- User's BP
;  G@STATE+32: infix(AnyAddr) TADR;      -- Trap address
 ENDIF
ELSE ; I.E. iAPX386
   GLOBAL  G@STATE   DD <14 DUP(0)>  ; -- TRAP PACKET UNDER XENIX/UNIX/386
;  G@STATE+00: integer OsCode;        -- Signal number
;  G@STATE+04: integer SimCode;       -- S-Port trap code
;  G@STATE+08: integer rESP;          -- Saved register ESP
;  G@STATE+12: integer rEDI;          -- Saved register EDI
;  G@STATE+16: integer rESI;          -- Saved register ESI
;  G@STATE+20: integer rEBX;          -- Saved register EBX
;  G@STATE+24: integer rEDX;          -- Saved register EDX
;  G@STATE+28: integer rECX;          -- Saved register ECX
;  G@STATE+32: integer rEAX;          -- Saved register EAX
;  G@STATE+36: integer FLAGS;         -- Saved flags
;  G@STATE+40: infix(AnyAddr) CONT;   -- Continuation address (or +0)
;  G@STATE+44: integer        OldBP;  -- Old EBP
;  G@STATE+48: integer      UserBP;   -- User's EBP
;  G@STATE+52: infix(AnyAddr) TADR;   -- Trap address
ENDIF
; ******    L  O  C  A  L     D  A  T  A    ******
IF WITH87
NPXSTAT    DW 0       ; ****** 8087 / 80287 Status  Word ******
CW87SIM    DW 0332H   ; ****** 8087 / 80287 Control Word ******
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
ENDIF

IF WTL167
CTX167     DD 01320000H   ; ****** WEITEK x167 Context Register ******
;
; CTX = <MDSEL>4<MD>4<EM>8<CC>8<AE>8
;          0      1   32   00   00     -- MINE SETTING
;          0      3   32   80   00     -- UNIX/386 DEFAULT SETTING
;
;
; MDSEL = 12 = C = 1100B -- Update EM, CC and AE fields only
;       =  0 = 0 = 0000B -- Update MD, EM, CC and AE fields
;
;    MD = <RND>2<IRND>1<+1>1
;           00     0     1      = 1 =
;           00     1     1      = 3 =  -- UNIX/386 DEFAULT
;
;           RND = 00 - Round toward Nearest Value 
;                 01 - Round toward Zero
;                 10 - Round toward Positive Infinity
;                 11 - Round toward Negative Infinity
;
;          IRND = 0  - Integer Rounding based on RND
;                 1  - Integer Rounding always towards Zero
;
;    EM = <DM>1<UOM>1<PM>1<UM>1<OM>1<ZM>1<+1>1<IM>1
;           0    0     1    1    0    0    1    0     = 32 --
;           0    0     1    1    0    0    1    0     = 32 -- UNIX/386 DEFAULT
;
;          DM - Data Chain exception mask
;         UOM - Undefined Opcode exception mask
;          PM - Exception Mask - Precision
;          UM - Exception Mask - Underflow
;          OM - Exception Mask - Overflow
;          ZM - Exception Mask - Zero Divide
;          IM - Exception Mask - Invalid Operation
ENDIF

IF iAPX286
IISIGN     DW  0   ; USED BY: IIPOWR
ENDIF
STKMS0     DB 'sim: CPU-Stack Overflow',10,13,'$'
IF DOS OR OS2
STKMS1     DB 'sim: CPU stack underflow',10,13,'$'
   IF WITH87
NPXHERE    DW 0FFH    ; ****** 8087 / 80287 With us <> 0 ******
STKMS2     DB 'sim: x87 stack not empty',10,13,'$'
NO87MS     DB '*** NOTE: No Numeric Co-Processor',10,13,'$'
   ELSE
NPXHERE    DW 000H    ; ****** no 8087 / 80287 With us = 0 ******
   ENDIF
ENDIF

IF OS2
PRVADDR    DD 0   ; ****** Result from DosSetVec     ******
RetCode    DW 0   ; Return Value from OS/2-Call
KBDASC     DB 0   ; KBD-Packet - ASCII Character Code from Keyboard
KBDSCAN    DB 0   ; KBD-Packet - Scan Code from Keyboard
KBDSTAT    DB 0   ; KBD-Packet - NLSStatus from Keyboard
KBDSHFT    DB 0   ; KBD-Packet - NLShiftStatus from Keyboard
KBDSHST    DW 0   ; KBD-Packet - ShiftStates from Keyboard
KBDTIME    DD 0   ; KBD-Packet - TimeStamp from Keyboard
ScanCode   DB 0   ; Saved ScanCode for INBT
O@87CW   DW 0    ;     87 Memory Layout:  Control Word
O@87SW   DW 0    ;                        Status Word
O@87TW   DW 0    ;                        Tag Word
O@87IPO  DW 0    ;                        Instruction Pointer Offset
O@87IPS  DW 0    ;                        Instruction Pointer Segment
O@87DPO  DW 0    ;                        Data Pointer Offset
O@87DPS  DW 0    ;                        Data Pointer Segment
ENDIF

IF UNIX OR XENIX
TrapBP     DD 0
TrapSP     DD 0
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
      NOP
      NOP
      NOP
      NOP

; *************************************************************
; ******                                                 ******
; ******   entered from DOSSEG or a C init module:       ******
; ******      SS:SP set                                  ******
; ******      call far ptr _main( argc, argv, envp )     ******
; ******           argc: #arguments to program           ******
; ******           argv: argument vector                 ******
; ******           envp: envir vector (zero term.)       ******
; ******                                                 ******
; *************************************************************
  
  IFDEF VOPTV
      PUSH    BP
      MOV     BP,SP
      MOV     SI,SEG DGROUP	; to be sure!!!
      MOV     DS,SI		; to be sure!!!
      ASSUME  DS:DGROUP
      MOV     G@XNXREF+2,SS
      LEA     AX,FSTARG
      MOV     G@XNXREF,AX
  ELSE
      MOV     SI,SEG DGROUP
      CLI
      MOV     SS,SI
      MOV     SP,OFFSET DGROUP:G@STKEND+4
      STI
      MOV     SS:G@PSPREF+2,DS
      XOR     AX,AX
      MOV     SS:G@PSPREF,AX
  ENDIF
      XOR     BP,BP
      MOV     AX,SP
      SUB     AX,4
      MOV     SS:G@INITSP,AX   ; Set Initial SP before traps set
      CALL    FAR PTR E@ETRH ; Enable Trap Handlers
; *** Initiate and Go User Program
; === CALL    FAR PTR SIM_MAIN
      PUSH    BP
;     MOV     AX,100H  ; Single step
      PUSH    BP       ; Not Single step
      JMP     FAR PTR SIM_MAIN
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
      MOV     SP,OFFSET DGROUP:G@STKEND+4
      XOR     BP,BP
      MOV     AX,SP
      SUB     AX,4
      MOV     SS:G@INITSP,AX   ; Set Initial SP
      CALL    FAR PTR E@ETRH ; Enable Trap Handlers
; === CALL    FAR PTR SIM_MAIN
      PUSH    BP
      PUSH    BP
      JMP     FAR PTR SIM_MAIN
      INT     2   ; Impossible to reach this instruction
_main ENDP
ENDIF


IF XENIX AND iAPX286
; *******************************************************************
; ******   M A I N   E N T R Y   F R O M   X E N I X / 2 8 6   ******
; *******************************************************************
  
_main PROC    FAR
      PUBLIC  _main
      ASSUME  DS:DGROUP
      PUSH    BP
      MOV     BP,SP
      MOV     G@XNXREF+2,SS
      LEA     AX,FSTARG
      MOV     G@XNXREF,AX
;  ....
      XOR     BP,BP
      MOV     AX,SP
      SUB     AX,4
      MOV     SS:G@INITSP,AX   ; Set Initial SP
      CALL    FAR PTR E@ETRH ; Enable Trap Handlers
; === CALL    FAR PTR SIM_MAIN
      PUSH    BP
;     PUSH    100H     ; Single step
      PUSH    BP       ; =0: Not Single step
      JMP     FAR PTR SIM_MAIN
      INT     2   ; Impossible to reach this instruction
;     NOP         ; *** FILLER FOR SCO-286 LINKER ***
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
      MOV     EAX,ESP
      SUB     EAX,4
      MOV     G@INITSP,EAX   ; Set Initial ESP
      CALL    E@ETRH ; Enable Trap Handlers
; === CALL    SIM_MAIN
;     PUSH    100H     ; Single step
      PUSH    0        ; Not Single step
      JMP     SIM_MAIN
      INT     2   ; Impossible to reach this instruction
_main ENDP
ENDIF


; ****************************************
; ******   T  R  A  P    E  N  D    ******
; ****************************************
  SIMROUTINE  E@TRAPEND
       PUBLIC  E@TRAPEND
IF iAPX286
       ASSUME  DS:NOTHING
   IF DOS   ;  *** RESTORE ALL REGISTERS UNDER MS-DOS ***
       MOV     AX,FSTARG
       MOV     BX,FSTARG+2
       MOV     SP,SS:G@STATE+02;  -- Saved register SP
       MOV     BP,SS:G@STATE+20;  -- Old BP
       PUSH    BX
       PUSH    AX
       MOV     DS,SS:G@STATE+04;  -- Saved register DS
       MOV     ES,SS:G@STATE+06;  -- Saved register ES
       MOV     DI,SS:G@STATE+08;  -- Saved register DI
       MOV     SI,SS:G@STATE+10;  -- Saved register SI
       MOV     BX,SS:G@STATE+12;  -- Saved register BX
       MOV     DX,SS:G@STATE+14;  -- Saved register DX
       MOV     CX,SS:G@STATE+16;  -- Saved register CX
       MOV     AX,SS:G@STATE+18;  -- Saved register AX
   ENDIF
   IF OS2   ;  *** RESTORE ALL REGISTERS UNDER OS/2 ***
       MOV     AX,FSTARG
       MOV     BX,FSTARG+2
       MOV     SP,SS:G@STATE+02;  -- Saved register SP
       MOV     BP,SS:G@STATE+20;  -- Old BP
       PUSH    BX
       PUSH    AX
       MOV     DS,SS:G@STATE+04;  -- Saved register DS
       MOV     ES,SS:G@STATE+06;  -- Saved register ES
       MOV     DI,SS:G@STATE+08;  -- Saved register DI
       MOV     SI,SS:G@STATE+10;  -- Saved register SI
       MOV     BX,SS:G@STATE+12;  -- Saved register BX
       MOV     DX,SS:G@STATE+14;  -- Saved register DX
       MOV     CX,SS:G@STATE+16;  -- Saved register CX
       MOV     AX,SS:G@STATE+18;  -- Saved register AX
   ENDIF
   IF XENIX ;  *** RESTORE ALL REGISTERS UNDER XENIX/286 ***
       MOV     AX,FSTARG
       MOV     BX,FSTARG+2
       MOV     SP,SS:G@STATE+04;  -- Saved register SP
       MOV     BP,SS:G@STATE+30;  -- User's BP
       PUSH    BX
       PUSH    AX
       MOV     DS,SS:G@STATE+06;  -- Saved register DS
       MOV     ES,SS:G@STATE+08;  -- Saved register ES
       MOV     DI,SS:G@STATE+10;  -- Saved register DI
       MOV     SI,SS:G@STATE+12;  -- Saved register SI
       MOV     BX,SS:G@STATE+14;  -- Saved register BX
       MOV     DX,SS:G@STATE+16;  -- Saved register DX
       MOV     CX,SS:G@STATE+18;  -- Saved register CX
       MOV     AX,SS:G@STATE+20;  -- Saved register AX
   ENDIF
ELSE ; I.E. iAPX386
;  *** RESTORE ALL REGISTERS UNDER XENIX/UNIX/386 ***
       MOV     EAX,FSTARG
       MOV     ESP,G@STATE+08; -- Saved register ESP
       MOV     EBP,G@STATE+48; -- User's EBP
       PUSH    EAX
       MOV     EDI,G@STATE+12; -- Saved register EDI
       MOV     ESI,G@STATE+16; -- Saved register ESI
       MOV     EBX,G@STATE+20; -- Saved register EBX
       MOV     EDX,G@STATE+24; -- Saved register EDX
       MOV     ECX,G@STATE+28; -- Saved register ECX
       MOV     EAX,G@STATE+32; -- Saved register EAX
ENDIF
       RET
E@TRAPEND ENDP


; **********************************************
; ******   F  I  N  A  L    E  X  I  T    ******
; **********************************************
  SIMROUTINE  E@EXIT
IF iAPX286
   IF DOS
      CALL    NEAR PTR E@DTRH ; Disable Trap Handlers
   ENDIF
ENDIF
       EXIT    FSTARG ; import range(0:MaxWord) ExitCode
       INT     2      ; Impossible to reach this instruction
E@EXIT ENDP


IF iAPX386
; ***************************************************************
; ******   R  E  A  D    C  O  D  E    M  E  M  O  R  Y    ******
; ***************************************************************
  SIMROUTINE  E@GTCODE
       MOV     EAX,FSTARG
       ADD     EAX,FSTARG+4
       MOV     AL,CS:[EAX]
       AND     EAX,0FFH
       MOV     FSTARG+8,EAX
  ENDROUTINE  E@GTCODE,8
ENDIF


IF iAPX286
   IFDEF BANKING
; **********************************************
; ******   U T I L I T Y :  E @ G O T O   ******
; **********************************************
        EVEN
E@GOTO PROC    FAR  ; BX:AX = Address
       PUBLIC  E@GOTO
       ASSUME  DS:NOTHING
; ???  MOV     SP,OFFSET DGROUP:G@STKEND ; Restore Simula's Stack-Pointer
       MOV     SP,SS:G@INITSP ; Restore Simula's Stack-Pointer
       XOR     BP,BP
       PUSH    BX
       PUSH    AX
       RET
E@GOTO ENDP
   ENDIF
ENDIF


IF iAPX286
; **********************************************
; ******   U T I L I T Y :  E @ C A L L   ******
; **********************************************
        EVEN
E@CALL PROC    FAR  ; BX:AX = Address
       PUBLIC  E@CALL
       ASSUME  DS:NOTHING
       POP     SI   ; Near Entry
       PUSH    CS
       PUSH    SI
       PUSH    BX
       PUSH    AX
       RET
E@CALL ENDP
ENDIF



IFDEF BANKING
; ----------------------------------------------------------------
; -- GOTO Visible Label:                                        --
; --                                                            --
; -- q-Code:  qJMPF  <address>                                  --
; --                                                            --
; -- i-Code:  iJMPF  <offset>2<segm>2+F_POINTER(address)        --
; --                                                            --
; --   ===>   iINT  xJMPF  frmx  <offset>2                      --
; --                                                            --
; ----------------------------------------------------------------
T@JMPF DD 0
xJMPF  PROC    FAR
       ASSUME  DS:NOTHING
       POP     DI
       POP     ES                   ; ES:DI = INT Return Address
       POP     SI
       MOV     BL,ES:[DI]
       ADD     BX,SEG S@BNKTAB
       MOV     BH,0                 ; BX <-- Frmx + Relocation
       MOV     CL,BYTE PTR SS:G@FRMTAB[BX]
       MOV     CH,0
       MOV     BH,BYTE PTR SS:G@FRMBAS[BX]
       PUSH    BX
       PUSH    ES:[DI]+1
       JCXZ    $+3
       RET                          ; OK: Frame in Memory
       POP     AX
       POP     BX
       PUSH    BX
       PUSH    AX
       PUSH    BX
       PUSH    AX
       CALL    NEAR PTR E@LDBNK     ; Load Bank and Jump
       RET
xJMPF  ENDP


; ----------------------------------------------------------------
; -- CALL Visible Routine:                                      --
; --                                                            --
; -- q-Code:  qCALL  <address>                                  --
; --                                                            --
; -- i-Code:  iCALL  <offset>2<segm>2+F_POINTER(address)        --
; --                                                            --
; --   ===>   iINT  xCALF  frmx  <offset>2                      --
; --                                                            --
; ----------------------------------------------------------------
T@CALF DD 0
       EVEN
xCALF  PROC    FAR
       ASSUME  DS:NOTHING
       POP     DI
       POP     ES                   ; ES:DI = INT Return Address
       POP     SI
       MOV     BL,ES:[DI]
       ADD     BX,SEG S@BNKTAB
       MOV     BH,0                 ; BX <-- Frmx + Relocation
       MOV     AX,ES:[DI]+1
       INC     DI
       INC     DI
       INC     DI
       PUSH    ES
       PUSH    DI                   ; Modified Return Address
       MOV     CL,BYTE PTR SS:G@FRMTAB[BX]
       MOV     CH,0
       MOV     BH,BYTE PTR SS:G@FRMBAS[BX]
       PUSH    BX
       PUSH    AX
       JCXZ    $+3
       RET                          ; OK: Frame in Memory
       POP     AX
       POP     BX
       PUSH    BX
       PUSH    AX
       PUSH    BX
       PUSH    AX
       CALL    NEAR PTR E@LDBNK     ; Load Bank and Jump
       RET
xCALF  ENDP


; ----------------------------------------------------------------
; -- GOTO Evaluated Label:    BX:AX = Address                   --
; --                                                            --
; -- q-Code:  qJMPF   E@GOTO                                    --
; --                                                            --
; -- i-Code:  iJMPF  <offset>2<segm>2+F_POINTER(E@GOTO)         --
; --                                                            --
; --   ===>   iINT  xGOTO  iNOP  iNOP  iNOP                     --
; --                                                            --
; ----------------------------------------------------------------
T@GOTO DD 0
       EVEN
xGOTO  PROC    FAR
       ASSUME  DS:NOTHING
; ???  MOV     SP,OFFSET DGROUP:G@STKEND ; Restore Simula's Stack-Pointer
       MOV     SP,SS:G@INITSP ; Restore Simula's Stack-Pointer
       PUSH    BX
       PUSH    AX
       MOV     BH,0                 ; BX <-- Frmx
       MOV     CL,BYTE PTR SS:G@FRMTAB[BX]
       MOV     CH,0
       JCXZ    $+3
       RET                          ; OK: Frame in Memory
       POP     AX
       POP     BX
       PUSH    BX
       PUSH    AX
       PUSH    BX
       PUSH    AX
       CALL    NEAR PTR E@LDBNK     ; Load Bank and Jump
       RET
xGOTO  ENDP


; ----------------------------------------------------------------
; -- CALL Evaluated Routine:  BX:AX = Address                   --
; --                                                            --
; -- q-Code:  qCALL   (far) E@CALL                              --
; --                                                            --
; -- i-Code:  iCALLF <offset>2<segm>2+F_POINTER(E@CALL)         --
; --                                                            --
; --   ===>   iINT  xCALL  iNOP  iNOP  iNOP                     --
; --                                                            --
; ----------------------------------------------------------------
T@CALL DD 0
       EVEN
xCALL  PROC    FAR
       ASSUME  DS:NOTHING
       POP     DI
       POP     ES                   ; ES:DI = INT Return Address
       POP     SI
       PUSH    ES
       PUSH    DI                   ; UnModified Return Address
       PUSH    BX
       PUSH    AX
       MOV     BH,0                 ; BX <-- Frmx
       MOV     CL,BYTE PTR SS:G@FRMTAB[BX]
       MOV     CH,0
       JCXZ    $+3
       RET                          ; OK: Frame in Memory
       POP     AX
       POP     BX
       PUSH    BX
       PUSH    AX
       PUSH    BX
       PUSH    AX
       CALL    NEAR PTR E@LDBNK     ; Load Bank and Jump
       RET
xCALL  ENDP
ENDIF


; *********************************************
; ******    G E T   O U T E R M O S T    ******
; *********************************************
  SIMROUTINE E@GTOUTM
; FSTARG   --  export label res;
IF iAPX286
; ???  MOV     AX,WORD PTR SS:G@TROUTM
; ???  MOV     BX,WORD PTR SS:G@TROUTM+2
; ???  OR      BX,BX
; ???  JNZ     L2
       MOV     CX,BP
L1:    MOV     BX,CX
       MOV     CX,SS:[BX]
       INC     CX
       LOOP    L1
       MOV     AX,SS:[BX]+2  ; Ofst
       DEC     AX
       DEC     AX
       MOV     BX,SS:[BX]+4  ; Segm
L2:    MOV     FSTARG,AX
       MOV     FSTARG+2,BX
ELSE ; I.E. iAPX386
; ???  MOV     EAX,G@TROUTM
; ???  OR      EAX,EAX
; ???  JNZ     GL2
       MOV     ECX,EBP
GL1:   MOV     EBX,ECX
       MOV     ECX,[EBX]
       INC     ECX
       LOOP    GL1
       MOV     EAX,[EBX]+4  ; Ofst
; ???  SUB     EAX,5        ; Previous instruction ?????????
       SUB     EAX,2        ; Previous instruction ?????????
GL2:   MOV     FSTARG,EAX
ENDIF
  ENDROUTINE E@GTOUTM


; **********************************************
; ******    R U N T I M E    E R R O R    ******
; **********************************************
; FSTARG   --  import ref(file) filref;
; FSTARG+4 --  import range(0:MaxByte) eno;
IF iAPX286
        EVEN
E@RTERR PROC  FAR ; **** Runtime Error ****
        PUBLIC  E@RTERR
        ASSUME  DS:NOTHING
        POP     AX
        PUSH    CS
        PUSH    AX
        MOV     AX,WORD PTR SS:G@INIERR+2
        PUSH    AX
        MOV     AX,WORD PTR SS:G@INIERR
        PUSH    AX
ELSE ; I.E. iAPX386
E@RTERR PROC  NEAR ; **** Runtime Error ****
        PUBLIC  E@RTERR
        PUSH    DWORD PTR G@INIERR
ENDIF
        RET
E@RTERR ENDP


; ****************************************************************
; ******    W R I T E :  ' S T A C K   O V E R F L O W '    ******
; ****************************************************************
IF iAPX286
  SIMROUTINE  E@STKOFL ; Write STACK OVERFLOW
           PUSH    SS
           POP     DS
           ASSUME  DS:DGROUP
           PRT     25,<OFFSET DGROUP:STKMS0>
  ENDROUTINE E@STKOFL
ENDIF



; *******************************************
; ******    G E T   S T A C K P T R    ******
; *******************************************
  SIMROUTINE E@GETSP
; FSTARG   --  export unsigned (E)SP; NOTE: SP inside routine,
;          but this doesn't matter as long as it is consistent
IF iAPX286
       MOV     DX,SP
       MOV     FSTARG,DX
ELSE ; i.e. 386
       MOV     EDX,ESP
       MOV     FSTARG,EDX
ENDIF
  ENDROUTINE E@GETSP

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
         CLI
         MOV    SS,CS:SAV_SS
         MOV    SP,CS:SAV_SP
         STI
;  ****  Test For ERROR  ****
         JNC    LL2
         MOV    SS:G@OSSTAT,AX
;  ****  Get Return Code  ****
LL2:     MOV    AH,4DH
         INT    21H           ; AX := Return Code
         MOV    FSTARG+12,AX
  ENDROUTINE E@EXEC,12
ENDIF


; *****************************************************
; ******    C H E C K   S T A C K   E M P T Y    ******
; *****************************************************
;
IF iAPX286
CHKRET     DD 0  ; *** Save previous return of CHKSTK
E@CHKSTK PROC    FAR         ; ****  Check 86 and 87 Stack Empty  ******
         PUBLIC  E@CHKSTK    ; ****  On Return: Restore all regs  ******
         ASSUME  DS:NOTHING
         POP     AX
         PUSH    CS
         PUSH    AX
   IF DOS ;  ****** MS-DOS Specific code ******
         CMP     BP,0
         JNE     EX1
;        ***     Check 86-Stack Empty     ***
         CMP     SP,OFFSET DGROUP:G@STKEND-4
; ###### CMP     SP,(G@INITSP)-4   dvs: AX:=G@INITSP,AX:=AX-1,CMP SP,AX
         JE      E86
         PUSH    SS
         POP     DS
         ASSUME  DS:DGROUP
         MOV     DX,OFFSET DGROUP:STKMS1
         MOV     AH,9
         INT     21H       ; Write: CHKSTK failed - 86
         XOR     CX,CX
         IDIV    CX        ; Division by zero --> TRAP
E86:  ;  ***     Check 87-Stack Empty     ***
         PUSH    AX
      IF WITH87
         MOV     AX,SS:NPXHERE
         CMP     AX,0        ; Do we have a NPX (8087 or 80287)
         JE      EX2
         PUSH    BP          ; Yes: Check Tag Word
         MOV     BP,SP
         CLI                 ; Disable Host Interrupt
         FNSTENV [BP]-14
         WAIT
         FLDCW   SS:CW87SIM  ; Set Coprocessor Control Word
         STI                 ; Enable Host Interrupt
         MOV     AX,[BP]-10  ; Tag Word
         POP     BP
         CMP     AX,0FFFFH
         JE      EX2
         PUSH    SS
         POP     DS
         ASSUME  DS:DGROUP
         MOV     DX,OFFSET DGROUP:STKMS2
         MOV     AH,9
         INT     21H       ; Write: CHKSTK failed - 87
         XOR     CX,CX
         IDIV    CX        ; Division by zero --> TRAP
      ENDIF
;        ...     ......
EX2:     PUSH    BX
         PUSH    DX
         MOV     BX,SP
         MOV     AX,SS:[Bx]+6
         MOV     DX,SS:[Bx]+8
         MOV     CS:WORD PTR CHKRET,AX
         MOV     CS:WORD PTR CHKRET+2,DX
         POP     DX
         POP     BX
         POP     AX
   ENDIF   ;  ****** MS-DOS Specific code ******
EX1:     RET
E@CHKSTK ENDP

ENDIF


IF XENIX OR UNIX
; *************************************************
; ******    S E T   U N I X   S T A T U S    ******
; *************************************************
   IF iAPX286
         EVEN
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


; ; ***************************************************************
; ; ******   R E A D   B Y T E   F R O M   K E Y B O A R D   ******
; ; ***************************************************************
;   ROUTINE X@INBT ; Read byte from keyboard.
; IF DOS
;        MOV     AL,0
;        MOV     AH,7          ; Direct Console input.
;        INT     21H
;        MOV     AH,0
;        MOV     FSTARG+4,AX    ; Store result as character.
; ENDIF
; IF OS2
;        MOV     AL,ScanCode  ;  Check if we have a saved ScanCode
;        MOV     AH,0
;        MOV     ScanCode,AH  ;  Clear ScanCode
;        OR      AL,AL
;        JNE     BTRET
;        PUSH    SS
;        POP     DS
;        PUSH    DS                   ; KeyBoardBuffer'Segment
;        PUSH    OFFSET DGROUP:KBDASC ; KeyBoardBuffer'Offset
;        PUSH    0                    ; WAIT Until Char available
;        PUSH    0                    ; Keyboard Handle
;        CALL    KBDCHARIN
;        MOV     AL,KBDASC
;        MOV     AH,0
;        OR      AL,AL
;        JNE     BTRET
;        MOV     AL,KBDSCAN
;        MOV     ScanCode,AL   ; Save ScanCode for next call
;        XOR     AL,AL
; BTRET: MOV     FSTARG+4,AX ; Return Ascii/Scan Code
; ENDIF
; IF XENIX AND iAPX286
;        PUSH    SS
;        POP     DS
;        CALL    FAR PTR _getchar
;        MOV     AH,0
;        MOV     FSTARG+4,AX   ; Return character read.
; ENDIF
; IF iAPX386
;        CALL    _getchar
;        MOVZX   EAX,AL
;        MOV     FSTARG+4,EAX   ; Return character read.
; ENDIF
;   ENDROUTINE X@INBT,4


IF UNIX
; ***************************************************************
; ******   UNIX: Check to see if there is data to be read  ******
; ***************************************************************
; ******   Sysroutine("X?rdchk") XNX_RDCHK;                ******
; ******   import integer fildes;                          ******
; ******   export integer res;    ! 0:Not ready, 1:Ready;  ******
; ******   end;                                            ******
; ***************************************************************
  ROUTINE rdchk  ; Check Read byte possible
       PUSH  0       ; Arg
       PUSH  6001H   ; Cmnd -- F_RDCHK UNIX:386ix specific
       PUSH  FSTARG  ; Fildes
       CALL  _fcntl
       ADD   ESP,12
  ENDROUTINE rdchk,0
; ***************************************************************
; ******   UNIX: Change the size of a file                 ******
; ***************************************************************
; ******   Sysroutine("X?chsize") XNX_CHSIZE;              ******
; ******   import integer lng; -- New file size (in bytes) ******
; ******          integer fildes;  -- File Descriptor      ******
; ******   end;                                            ******
; ***************************************************************
  ROUTINE chsize  ; Change the size of a file
       PUSH  FSTARG+4 ; Arg  -- New file size
       PUSH  6000H    ; Cmnd -- F_CHSIZE UNIX:386ix specific
       PUSH  FSTARG   ; Fildes
       CALL  _fcntl
       ADD   ESP,12
  ENDROUTINE chsize,0
ENDIF


IF iAPX286
; *************************************************************************
; ******  E M U L A T E   3 2 - B I T   M U L T I P L I C A T I O N  ******
; *************************************************************************
;   SIMROUTINE E@IMUL ; (DX:AX) := (DX:AX) * (BX:CX)
;            XOR     DI,DI    ; Sign flag
;            OR      DX,DX
;            JGE     LMUL1
;            NOT     DI
;            NEG2    DX,AX
; LMUL1:     OR      BX,BX
;            JGE     LMUL2
;            NOT     DI
;            NEG2    BX,CX     ; Corr 12/8. GS
; LMUL2:     PUSH    DX
;            XCHG    AX,BX
;            XCHG    BX,CX
;            MUL     CX
;            XCHG    AX,BX
;            MOV     SI,AX
;            MUL     CX
;            ADD     BX,DX
;            XCHG    AX,SI
;            POP     CX
;            MUL     CX
;            ADD     BX,AX
;            MOV     DX,BX
;            MOV     AX,SI
;            OR      DI,DI
;            JE      LMULE
;            NEG2    DX,AX
; LMULE:
;   ENDROUTINE E@IMUL


  SIMROUTINE E@IMUL ; (DX:AX) := (DX:AX) * (BX:CX) , overflow check
           XOR     DI,DI    ; Sign flag
           OR      DX,DX
           JNS     LMUL1
           NOT     DI
           NEG2    DX,AX
LMUL1:     OR      BX,BX
           JZ      LMULZ
           JNS     LMUL2
           NOT     DI
           NEG2    BX,CX     ; Corr 12/8. GS
LMUL2:     OR      DX,DX
           JNZ     LMULOF    ; both DX and BX nonzero: overflow
           XCHG    DX,BX
           XCHG    AX,CX
LMULZ: ; BX zero
           PUSH    DX
           MUL     CX
           MOV     BX,DX
           MOV     SI,AX
           POP     AX
           MUL     CX
           JO      LMULOF ; overflow ??
           MOV     DX,AX
           MOV     AX,SI
           ADD     DX,BX
           JO      LMULOF
           OR      DI,DI
           JZ      LMULOT
           NEG2    DX,AX
LMULOT:    XOR     DI,DX
           JNS     LMULE  ; if DI and DX has diff. sign: overflow
           OR      DX,DX  ; -- except if DX:AX ZERO!!!
           JNZ     LMULOF
           OR      AX,AX
           JZ      LMULE
LMULOF:    ERROR   6
LMULE:
  ENDROUTINE E@IMUL
ENDIF


IF iAPX286
; *************************************************************
; ******  E M U L A T E   3 2 - B I T   D I V I S I O N  ******
; *************************************************************
; NOTE - minint is not treated correct!!!
  SIMROUTINE E@IDIV ; (DX:AX) := (DX:AX) / (BX:CX)
           MOV     BP,SP
           XOR     DI,DI    ; Sign flag
           OR      DX,DX
           JGE     LDIV1
           NOT     DI
           NEG2    DX,AX ; doesn't work for minint
LDIV1:     OR      BX,BX
           JE      LDIVZ
           JGE     LDIV2
           NOT     DI
           NEG2    BX,CX     ; Corr 12/8. GS ; doesn't work for minint
LDIV2:     PUSH    BX   ; [BP]-2
           PUSH    CX   ; [BP]-4
           PUSH    DX   ; [BP]-6
           PUSH    AX   ; [BP]-8
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
           JMP     NEAR PTR LDIV7
LDIVZ:     XCHG    AX,BX ; BX is zero
           XCHG    AX,DX
           DIV     CX
           XCHG    AX,BX
           DIV     CX
           MOV     DX,BX
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
; NOTE - minint not treated correct!!!
  SIMROUTINE E@IREM ; (DX:AX) := (DX:AX) rem (BX:CX)
           XOR     DI,DI    ; Sign flag
           OR      DX,DX
           JGE     LREM1
           NOT     DI
           NEG2    DX,AX ; doesn't work for minint
LREM1:     OR      BX,BX
           JE      LREMZ
           JG      LREM2
           NEG2    BX,CX     ; Corr 12/8. GS ; doesn't work for minint
LREM2:     PUSH    BX   ; [BP]-2
           PUSH    CX   ; [BP]-4
           PUSH    DX   ; [BP]-6
           PUSH    AX   ; [BP]-8
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
           JE      LREM7          ; NOTE! inverted test
           JMP     NEAR PTR LREME
LREMZ: ; BX is zero
           XCHG    AX,BX ; BX is zero
           XCHG    AX,DX
           DIV     CX
           XCHG    AX,BX
           DIV     CX
           MOV     AX,DX
           XOR     DX,DX
           OR      DI,DI
           JE      LREME
LREM7:     NEG2    DX,AX
LREME:
  ENDROUTINE E@IREM
ENDIF




  SIMROUTINE E@IIPOWR ; -------------------------------------------- IIPOWR
; Visible known("IIPOWR") IIPOWER; --- res:=b**x ---
; import integer b,x; export integer res;
; begin if x=0 then if b=0 then ERROR(ENO_SYS_2) else res:=1 endif
;       elsif x < 0 then ERROR(ENO_SYS_2) elsif b=0 then res:=1
;       else res:=b; repeat x:=x-1 while x>0 do res:=res*b endrepeat;
;       endif;
; end;
IF iAPX286
          PUSH   SS
          POP    DS
          ASSUME DS:DGROUP

;     MOV     AX,100H     ; Single step
;     PUSH    AX       ; Single step
;     MOV     AX,1111H
;     MOV     CX,2222H
;     MOV     DX,3333H
;     MOV     BX,4444H
;     MOV     SI,5555H
;     MOV     DI,6666H
;     MOV     BP,7777H
;     INT     4
;     POPF
;     PUSHF
;     POP     SI
;     XOR     BP,BP

          MOV    BX,WORD PTR FSTARG+6 ; z: BX:CX <-- b
          MOV    CX,WORD PTR FSTARG+4

          MOV    DX,WORD PTR FSTARG+2 ;    DX:DI <-- x
          OR     DX,DX
          JGE    IIPOW1
          ERROR  128                  ; Negative exponent
IIPOW1:   MOV    DI,WORD PTR FSTARG
          JA     IIPOW3
          OR     DI,DI
          JNE    IIPOW3
          OR     BX,BX
          JNE    IIPOW2
          OR     CX,CX
          JNE    IIPOW2
          ERROR  128                  ; Zero raised to Zero power
IIPOW2:   XOR    DX,DX
          MOV    AX,1
          JMP    IIPOWE               ; Nonzero to Zero power = 1
IIPOW3:   OR     BX,BX
          JNE    IIPOW4
          OR     CX,CX 
          JZ     IIPOW2               ; Zero to Nonzero power = 1
      ;   JNE    IIPOW4
      ;   XOR    DX,DX
      ;   XOR    AX,AX
      ;   JMP    SHORT IIPOWE         ; Zero to Nonzero power = 0
IIPOW4:   MOV    IISIGN,0
          OR     BX,BX
          JGE    IIPOW5
          MOV    IISIGN,1
          NEG2   BX,CX                ; z: BX:CX <-- abs(BX:CX)
IIPOW5:   OR     BX,BX
          JNE    IIPOW6
          CMP    CX,2                 ; if z < 2
          JAE    IIPOW6               ; then 
          AND    DI,1
          OR     DI,2                 ;      DI <-- (DI and 1) or 2
          JMP    SHORT IIPOW7         ; else
IIPOW6:   OR     DX,DX                ;      if DX <> 0
          JZ     IIPOW7
          ERROR  6                    ;      then Too large exponent
IIPOW7:                               ; endif
          MOV    AX,DI
          AND    AX,1
          JNE    IIPOW8
          MOV    IISIGN,0             ; if DI and 1 = 0 then IISIGN := 0
IIPOW8:   XOR    DX,DX
          MOV    AX,1                 ; y: DX:AX <-- 1
IIXEXP1:  SHR    DI,1
          JC     IIXEXP3
IIXEXP2:; ****   DI even, z:= z*z
; this will overflow if BX <> 0 i.e. 
          OR     BX,BX
          JZ     IIXP
          ERROR  6
IIXP:     PUSH   AX
          PUSH   DX
      ;   MOV    DX,BX
          MOV    AX,CX
          MUL    CX
      ;   CALL   IIMUL         ; (DX:AX) := (DX:AX) * (BX:CX)
          MOV    BX,DX
          MOV    CX,AX
          POP    DX
          POP    AX
          JMP    IIXEXP1
IIXEXP3:; ****   DI odd, y:= y*z
          PUSH   BX
          PUSH   CX
          CALL   IIMUL         ; (DX:AX) := (DX:AX) * (BX:CX)
          POP    CX
          POP    BX
          OR     DI,DI
          JNZ    IIXEXP2
          CMP    IISIGN,0
          JZ     IIPOWE
          NEG2   DX,AX
IIPOWE:   MOV    FSTARG+8,AX    ; y: DX:AX --> Result
          MOV    FSTARG+10,DX
ELSE ; I.E. iAPX386

;     MOV     EAX,11111111H
;     MOV     ECX,22222222H
;     MOV     EDX,33333333H
;     MOV     EBX,44444444H
;     MOV     ESI,55555555H
;     MOV     EDI,66666666H
;     MOV     EBP,77777777H
;     PUSH    100H     ; Single step
;     INT     2
;     POPF
;     PUSHF
;     POP     ESI
;     XOR     EBP,EBP

          MOV    EBX,WORD PTR FSTARG+4 ; z: EBX <-- b
          MOV    ECX,WORD PTR FSTARG   ;    ECX <-- x
          OR     ECX,ECX
          JGE    SHORT IIPOW1
ERR128:   ERROR  128                  ; Negative exponent or Overflow
ERR6:     ERROR  6                    ; Overflow
IIPOW1:   MOV    EDI,1                ; y: EDI <-- 1
          JNZ    SHORT IIPOW3
          OR     EBX,EBX
          JNZ    SHORT IIPOWE         ; Nonzero to Zero power = 1
          JMP    ERR128               ; Zero raised to Zero power
IIPOW3:   SHR    ECX,1
          JC     SHORT IIXEXP3
IIXEXP2:; ****   ECX even, z:= z*z
          MOV    EAX,EBX
          IMUL   EAX           ; (EDX:EAX) := EAX * EAX
          JO     ERR6
          MOV    EBX,EAX
          JMP    IIPOW3
IIXEXP3:; ****   ECX odd, y:= y*z
          MOV    EAX,EDI
          IMUL   EBX           ; (EDX:EAX) := EAX * EBX
          JO     ERR6
          MOV    EDI,EAX
          OR     ECX,ECX
          JNZ    IIXEXP2
IIPOWE:   MOV    FSTARG+8,EDI  ; y: EDI --> Result
ENDIF
  ENDROUTINE E@IIPOWR,8


IF iAPX286
;        EVEN
; SETO  PROC   NEAR  ; set overflow flag
;       ASSUME DS:NOTHING
;       PUSH   AX
;       PUSHF
;       POP    AX
;       OR     AH,08H ; set overflow bit
;       PUSH   AX
;       POPF
;       POP    AX
;       RET
; SETO  ENDP

       EVEN
IIMUL PROC   NEAR  ; (DX:AX) := (DX:AX) * (BX:CX)
      ASSUME DS:NOTHING
      PUSH   DX
      XCHG   AX,BX
      XCHG   BX,CX
      MUL    CX
      XCHG   AX,BX
      MOV    SI,AX
      MUL    CX
      ADD    BX,DX
      XCHG   AX,SI
      POP    CX
      MUL    CX
      ADD    BX,AX
      MOV    DX,BX
      MOV    AX,SI
      RET
IIMUL ENDP
ENDIF



; *********************************************************************
; ******  R E P O R T   T R A P S   T O   E N V I R O N M E N T  ******
; *********************************************************************
IF iAPX286
  SIMROUTINE  E@RTRAP
       MOV     AX,FSTARG     ; Get Code
;      ...     .........
;      ...     .........
;      ...     .........
;      ...     .........
       XOR     AX,AX
       MOV     FSTARG+2,AX   ; Result:=false
  ENDROUTINE E@RTRAP,2
ELSE ; I.E. iAPX386
  SIMROUTINE  E@RTRAP
       MOV     EAX,FSTARG     ; Get Code
;      ...     .........
;      ...     .........
;      ...     .........
;      ...     .........
       XOR     EAX,EAX
       MOV     FSTARG+4,EAX  ; Result:=false
  ENDROUTINE E@RTRAP,4
ENDIF



; *******************************************************
; ******  E N A B L E   T R A P - H A N D L E R S  ******
; *******************************************************
  ROUTINE E@ETRH ; Enable Trap Handlers
IF DOS
           PUSH    CS
           POP     DS
           ASSUME  DS:S@ENV_TEXT
;  ****    Enable DOS Trap Handlers ****
           GTRH    00H,T@DIVZRO  ; Divide Error Exception
           GTRH    01H,T@SNGSTP  ; Single Step Interrupt
   IF WITH87
           GTRH    02H,T@NMIINT  ; NMI Interrupt
   ENDIF
           GTRH    03H,T@BRKPNT  ; Breakpoint Interrupt
           GTRH    04H,T@INTOVF  ; INTO Detected Overflow Exception
;          GTRH    05H,T@BOUNDE  ; BOUND Range Exceeded Exception
           GTRH    06H,T@ILLOPC  ; Invalid Opcode Exception
           GTRH    07H,T@NONE87  ; Numeric Coprocessor not available
;          GTRH    08H,T@xxxxxx  ; Double Exception Detected
;          GTRH    09H,T@PROT87  ; Coprocessor Segment Overrun Interrupt
;          GTRH    0AH,T@xxxxxx  ; Invalid Task State Segment
;          GTRH    0BH,T@xxxxxx  ; Segment Not Present
;          GTRH    0CH,T@xxxxxx  ; Stack Segment Overrun or Not Present
;          GTRH    0DH,T@PROT86  ; General Protection
;          GTRH    0EH,T@xxxxxx
;          GTRH    0FH,T@xxxxxx
;          GTRH    10H,T@INTR87  ; Coprocessor Error Report
           GTRH    23H,T@BREAK   ; Contol-C Handler Address

           PUSH    CS
           POP     DS
           ASSUME  DS:S@ENV_TEXT
           ETRH    00H,I@DIVZRO
           ETRH    01H,I@SNGSTP
           ETRH    03H,I@BRKPNT
           ETRH    04H,I@INTOVF
;          ETRH    05H,I@BOUNDE
           ETRH    06H,I@ILLOPC
           ETRH    07H,I@NONE87
;          ETRH    08H,I@xxxxxx
;          ETRH    09H,I@PROT87
;          ETRH    0AH,I@xxxxxx
;          ETRH    0BH,I@xxxxxx
;          ETRH    0CH,I@xxxxxx
;          ETRH    0DH,I@PROT86
;          ETRH    0EH,I@xxxxxx
;          ETRH    0FH,I@xxxxxx
           ETRH    23H,I@BREAK
   IF WITH87
           FNINIT
           XOR     AH,AH
           MOV     SS:BYTE PTR NPXSTAT+1,AH
           WAIT
           FNSTCW  SS:NPXSTAT
           MOV     AH,SS:BYTE PTR NPXSTAT+1
           CMP     AH,03H
           JE      GOT87
;  ****    No 8087 / 80287 Present
           PUSH    SS
           POP     DS
         ASSUME  DS:DGROUP
           MOV     DX,OFFSET DGROUP:NO87MS
           MOV     AH,9
           INT     21H       ; Write: 'NOTE: No Numeric Co-Processor   '
           XOR     AX,AX
           MOV     SS:NPXHERE,AX  ; Mark: No NPX
           JMP     SHORT CONT
;  ****    8087 / 80287 is Present in System
GOT87:
           FLDCW   SS:CW87SIM  ; Set Coprocessor Control Word
   ENDIF
           PUSH    CS
           POP     DS
           ASSUME  DS:S@ENV_TEXT
   IF WITH87
           ETRH    02H,I@NMIINT
   ENDIF
CONT:
   IFDEF BANKING
      CALL    NEAR PTR E@BEGBNK  ; NOTE: Must be near !!!!
; *** Redefine INT xGOTO/xJMPF/xCALL/xCALF
      MOV     SI,SEG S@BNKTAB
      MOV     DS,SI
      ASSUME  DS:NOTHING
      GTRH    DS:8,T@GOTO
      GTRH    DS:9,T@JMPF
      GTRH    DS:10,T@CALL
      GTRH    DS:11,T@CALF
      PUSH    CS
      POP     DS
      ASSUME  DS:S@ENV_TEXT
      MOV     ES,SI
      ASSUME  ES:NOTHING
      ETRH    ES:8,xGOTO
      ETRH    ES:9,xJMPF
      ETRH    ES:10,xCALL
      ETRH    ES:11,xCALF
   ENDIF
ENDIF

IF OS2
;  ****    Enable OS/2 Trap Handlers ****
           ETRH    00H,I@DIVZRO  ; Divide Error Exception
;          ETRH    01H,I@SNGSTP  ; Single Step Interrupt
;          ETRH    02H,I@xxxxxx  ; NMI Interrupt
;          ETRH    03H,I@BRKPNT  ; Breakpoint Interrupt
           ETRH    04H,I@INTOVF  ; INTO Detected Overflow Exception
;          ETRH    05H,I@BOUNDE  ; BOUND Range Exceeded Exception
           ETRH    06H,I@ILLOPC  ; Invalid Opcode Exception
           ETRH    09H,I@PROT87  ; Coprocessor Segment Overrun Interrupt
           ETRH    10H,I@INTR87  ; Coprocessor Error Report
           ETRH    0DH,I@PROT86  ; Segment Overrun Interrupt
           FLDCW   SS:CW87SIM    ; Set Coprocessor Control Word
ENDIF
IF XENIX OR UNIX
   IF WITH87
       FLDCW   CW87SIM  ; Set Coprocessor Control Word
   ENDIF
   IF WTL167
       WFSTCTX EAX         ; TEMP  TEMP  TEMP  TEMP  TEMP  ?????????
       MOV     EAX,CTX167
       WFLDCTX EAX         ; Load Weitek Context Register
   ENDIF
   IF iAPX286
       PUSH    SS
       POP     DS
       ASSUME  DS:DGROUP
   ENDIF
;  ****    Enable XENIX OR UNIX Signal Handlers ****
;      ESIG    1  ; Enable signal  1: Hangup
       ESIG    2  ; Enable signal  2: Interrupt
;      ESIG    3  ; Enable signal  3: Quit
       ESIG    4  ; Enable signal  4: Illegal Instruction
       ESIG    5  ; Enable signal  5: Trace Trap
;      ESIG    6  ; Enable signal  6: I/O Trap
;      ESIG    7  ; Enable signal  7: Emulator Trap Instruction
       ESIG    8  ; Enable signal  8: Floating-point Exception
;      ESIG    9  ; Enable signal  9: Kill (can't be caught)
       ESIG    10 ; Enable signal 10: Bus Error
       ESIG    11 ; Enable signal 11: Segment Violation
;      ESIG    12 ; Enable signal 12: Bad Arg. to System Call
;      ESIG    13 ; Enable signal 13: Write pipe none reads
;      ESIG    14 ; Enable signal 14: Alarm Clock 
;      ESIG    15 ; Enable signal 15: Software term signal
;      ESIG    16 ; Enable signal 16: User-Defined signal 1
;      ESIG    17 ; Enable signal 17: User-Defined signal 2
;      ESIG    18 ; Enable signal 18: Death of a Child
;      ESIG    19 ; Enable signal 19: Power fail
ENDIF
  ENDROUTINE E@ETRH



IF DOS
; *****************************************************************
; ******  D I S A B L E   D O S   T R A P - H A N D L E R S  ******
; *****************************************************************
  SIMROUTINE  E@DTRH
           PUSH    CS
           POP     DS
           ASSUME  DS:S@ENV_TEXT
           MOV     AX,CS:WORD PTR T@DIVZRO+2
           OR      AX,AX
           JNZ     DTRH0
   IF WITH87
           JMP     DRET
   ELSE
           JMP     SHORT DRET
   ENDIF
DTRH0:     DTRH    00H,T@DIVZRO
           DTRH    01H,T@SNGSTP
   IF WITH87
           MOV     AX,SS:NPXHERE
           OR      AX,AX       ; Do we have a NPX (8087 or 80287)
           JE      DTRH3
           DTRH    02H,T@NMIINT
DTRH3:
   ENDIF
           DTRH    03H,T@BRKPNT
           DTRH    04H,T@INTOVF
;          DTRH    05H,T@BOUNDE
           DTRH    06H,T@ILLOPC
           DTRH    07H,T@NONE87
;          DTRH    08H,T@xxxxxx
;          DTRH    09H,T@PROT87
;          DTRH    0AH,T@xxxxxx
;          DTRH    0BH,T@xxxxxx
;          DTRH    0CH,T@xxxxxx
;          DTRH    0DH,T@PROT86
;          DTRH    0EH,T@xxxxxx
;          DTRH    0FH,T@xxxxxx
;          DTRH    10H,T@INTR87
           DTRH    23H,T@BREAK
DRET:
   IFDEF BANKING ; *** Restore INT xGOTO/xJMPF/xCALL/xCALF
       MOV     SI,SEG S@BNKTAB
       MOV     ES,SI
       ASSUME  ES:NOTHING
       DTRH    ES:8,T@GOTO
       DTRH    ES:9,T@JMPF
       DTRH    ES:10,T@CALL
       DTRH    ES:11,T@CALF
   ENDIF
  ENDROUTINE E@DTRH
ENDIF


IF DOS
; ********************************************************************
; **************   D O S   T R A P   H A N D L E R S   ***************
; ********************************************************************
         ASSUME  DS:NOTHING
T@DIVZRO DD 0  ; *** Division by Zero
I@DIVZRO LABEL   FAR            ; *** Integer Divide by zero
         PUSH    BP
         MOV     BP,SP
         PUSH    AX
         MOV     AX,0700H       ; SimCode=07H, DosCode=00H
         JMP     SHORT I@TRAPH
T@SNGSTP DD 0  ; *** Single Step Interupt
I@SNGSTP LABEL   FAR            ; *** Single Step Interupt
         PUSH    BP
         MOV     BP,SP
         PUSH    AX
         MOV     AX,0001H       ; SimCode=00H=0, DosCode=01H
         JMP     SHORT I@TRAPH
T@BRKPNT DD 0  ; *** Breakpoint Interupt
I@BRKPNT LABEL   FAR            ; *** Breakpoint Interupt
         PUSH    BP
         MOV     BP,SP
         PUSH    AX
         MOV     AX,0A03H       ; SimCode=0AH=10, DosCode=03H
         JMP     SHORT I@TRAPH
T@INTOVF DD 0  ; *** Integer Overflow
I@INTOVF LABEL   FAR            ; *** Integer Overflow
         PUSH    BP
         MOV     BP,SP
         PUSH    AX
         MOV     AX,0604H       ; SimCode=06H, DosCode=04H
         JMP     SHORT I@TRAPH
T@BOUNDE DD 0  ; *** BOUND Range
I@BOUNDE LABEL   FAR            ; *** BOUND Range
         PUSH    BP
         MOV     BP,SP
         PUSH    AX
         MOV     AX,0805H       ; SimCode=08H, DosCode=05H
         JMP     SHORT I@TRAPH
T@ILLOPC DD 0  ; *** Invalid Opcode
I@ILLOPC LABEL   FAR            ; *** Invalid Opcode
         PUSH    BP
         MOV     BP,SP
         PUSH    AX
         MOV     AX,0906H       ; SimCode=09H, DosCode=06H
         JMP     SHORT I@TRAPH
T@NONE87 DD 0  ; *** Numeric Co-Processor not Available
I@NONE87 LABEL   FAR            ; *** Numeric Co-Processor not Available
         PUSH    BP
         MOV     BP,SP
         PUSH    AX
         MOV     AX,0007H       ; SimCode=00H, DosCode=07H
         JMP     SHORT I@TRAPH
T@PROT87 DD 0  ; *** Segment Overrun in87
I@PROT87 LABEL   FAR            ; *** Segment Overrun in87
         PUSH    BP
         MOV     BP,SP
         PUSH    AX
         MOV     AX,0809H       ; SimCode=08H, DosCode=09H
         JMP     SHORT I@TRAPH
T@PROT86 DD 0  ; *** Segment Overrun in86
I@PROT86 LABEL   FAR            ; *** Segment Overrun in86
         PUSH    BP
         MOV     BP,SP
         PUSH    AX
         MOV     AX,080DH       ; SimCode=08H, DosCode=0DH = 13
         JMP     SHORT I@TRAPH
T@BREAK  DD 0  ; *** Control-C Handler Address
I@BREAK  LABEL   FAR            ; *** Control-C Handler Address
         PUSH    BP
         MOV     BP,SP
         PUSH    AX
         MOV     AX,0B23H       ; SimCode=0BH, DosCode=23H = 35
         JMP     SHORT I@TRAPH

I@TRAPH: PUSH    CX
         PUSH    DX
         PUSH    BX
         PUSH    SI
         PUSH    DI
         PUSH    ES
         PUSH    DS
         MOV     SI,BP
         ADD     SI,8
         PUSH    SI   ; User's SP
         PUSH    AX   ; SimCode,DosCode
         CALL    FAR  PTR E@TRAPH+3
         POP     SI   ; SimCode,DosCode
         POP     SI   ; Saved user's SP
         POP     DS
         POP     ES
         POP     DI
         POP     SI
         POP     BX
         POP     DX
         POP     CX
         POP     AX
         MOV     SP,BP
         POP     BP
         IRET

   IF WITH87
; ****** Handler for Interrupt 02H *** NMI or Co-Processor Error Report
I@NMIINT LABEL   FAR
         PUSH    AX
;        FNDISI                 ; 8087: Disable interrupt, 80287: NOOP
         FNSTSW  CS:T@NPXSTAT
         MOV     AX,CS:T@NPXSTAT
         AND     AX,3FH
         JNZ     ERR87
;        FNENI                  ; 8087: Enable interrupt, 80287: NOOP
         POP     AX
         DB 0EAH ; JMP FAR <Dos-Handler-for-NMI>
T@NMIINT DD 0    ; *** Link to DOS-Handler
T@87CW   DW 0    ;     87 Memory Layout:  Control Word
T@87SW   DW 0    ;                        Status Word
T@87TW   DW 0    ;                        Tag Word
T@87IP   DW 0    ;                        Instruction Pointer
T@87IO   DW 0    ;                        IP + Instruction OP-code
T@87DP   DW 0    ;                        Data Pointer
         DW 0    ;                        Data Pointer
T@NPXSTAT DW 0
ERR87: ; ****** Numeric Co-Processor Error Report ******
       ; ****** AX=<+0>10<precision>1<ufl>1<ofl>1<divz>1<dnor>1<ill-op>1
         POP     CS:T@NPXSTAT ; NOTE: User's AX
; Underflow is handled here, other interrupt situations result in
; a run-time error, which is handled by giving control to Xhandlerp
;
; Major problem that at NMI-interrupt the stack is frequently changed,
; since the NMI is passed to out routine through several levels of
; interrupts.
         PUSH    BP
         MOV     BP,SP
         PUSH    CS:T@NPXSTAT ; NOTE: User's AX
         MOV     CX,AX
         AND     CX,10H
         JZ      FER1
         MOV     AX,0410H       ; SimCode=04H, DosCode=10H = 16
         PUSH    ES             ; 4 - Floating Point Underflow
         PUSH    BX
         FNSTENV CS:T@87CW
         WAIT
         MOV     BH,CS:BYTE PTR T@87DP+3
         MOV     CL,4
         SHR     BH,CL
         MOV     BL,CS:BYTE PTR T@87DP+1
         MOV     CL,4
         SHL     BX,CL
         MOV     ES,BX          ; Segment address established
         MOV     BL,CS:BYTE PTR T@87DP
         XOR     BH,BH          ; ES:BX is data address
         MOV     AX,CS:T@87IO
         MOV     CL,9
         SHR     AX,CL
         AND     AX,3           ; Look at MF, memory format
         XOR     CX,CX
         CMP     AX,3
         JE      BL16
         CMP     AX,2
         JNE     BL32
         MOV     ES:[BX]+6,CX   ; Set 64-bit real to zero
         MOV     ES:[BX]+4,CX
BL32:    MOV     ES:[BX]+2,CX   ; Set 32-bit Real or Int to zero
BL16:    MOV     ES:[BX],CX     ; Set 16-bit Int to zero
         FNCLEX
         POP     BX
         POP     ES
         POP     AX
         POP     BP
         IRET
;        JMP     SHORT I@TRAPH  ; 4 - Floating Point Underflow
         JMP     NEAR PTR I@TRAPH  ; 4 - Floating Point Underflow
FER1:    MOV     CX,SEG DGROUP
         MOV     SS,CX          ; NOTE: interrupt disabled here
         MOV     SP,SS:G@INITSP ; Reset SS and SP to initial SIMULA stack
         FNSTENV CS:T@87CW
         WAIT
         XOR     CX,CX
         PUSH    CX             ; Pretend flags zero
         MOV     BH,CS:BYTE PTR T@87IO+1
         MOV     CL,4
         SHR     BH,CL
         MOV     BL,CS:BYTE PTR T@87IP+1
         MOV     CL,4
         SHL     BX,CL
         PUSH    BX             ; Segment instruction address surrogate
         MOV     BL,CS:BYTE PTR T@87DP
         XOR     BH,BH          ; Offset instruction address surrogate
         PUSH    BX
         XOR     CX,CX
         PUSH    CX             ; Assume old BP empty
         MOV     BP,SP
         PUSH    CS:T@NPXSTAT   ; NOTE: User's AX
         FNCLEX
         STI                    ; Enable interrupt
         MOV     CX,AX
         AND     CX,08H
         JZ      FER2
         MOV     AX,0310H       ; SimCode=03H, DosCode=10H = 16
;        JMP     SHORT I@TRAPH  ; 3 - Floating Point Overflow
         JMP     NEAR PTR I@TRAPH  ; 3 - Floating Point Overflow
FER2:    MOV     CX,AX
         AND     CX,04H
         JZ      FER3
         MOV     AX,0210H       ; SimCode=02H, DosCode=10H = 16
;        JMP     SHORT I@TRAPH  ; 2 - Floating Point Division by zero
         JMP     NEAR PTR I@TRAPH  ; 2 - Floating Point Division by zero
FER3:    MOV     CX,AX
         AND     CX,01H
         JZ      FER4
         MOV     AX,0110H       ; SimCode=01H, DosCode=10H = 16
;        JMP     SHORT I@TRAPH  ; 1 - Invalid Floating Point Operation
         JMP     NEAR PTR I@TRAPH  ; 1 - Invalid Floating Point Operation
FER4:    MOV     AX,0210H       ; SimCode=05H, DosCode=10H = 16
         JMP     NEAR PTR I@TRAPH  ; 5 - Inexact Result
   ENDIF

ENDIF   ;  ****** MS-DOS Specific code ******




IF OS2
; **********************************************************************
; **************   O S / 2   T R A P   H A N D L E R S   ***************
; **********************************************************************
         ASSUME  DS:NOTHING
I@DIVZRO LABEL   FAR            ; *** Integer Divide by zero
         PUSH    BP
         MOV     BP,SP
         PUSH    AX
         MOV     AX,0700H       ; SimCode=07H, DosCode=00H
         JMP     SHORT I@TRAPH
I@SNGSTP LABEL   FAR            ; *** Single Step Interupt
         PUSH    BP
         MOV     BP,SP
         PUSH    AX
         MOV     AX,0001H       ; SimCode=00H=0, DosCode=01H
         JMP     SHORT I@TRAPH
I@BRKPNT LABEL   FAR            ; *** Breakpoint Interupt
         PUSH    BP
         MOV     BP,SP
         PUSH    AX
         MOV     AX,0A03H       ; SimCode=0AH=10, DosCode=03H
         JMP     SHORT I@TRAPH
I@INTOVF LABEL   FAR            ; *** Integer Overflow
         PUSH    BP
         MOV     BP,SP
         PUSH    AX
         MOV     AX,0604H       ; SimCode=06H, DosCode=04H
         JMP     SHORT I@TRAPH
I@BOUNDE LABEL   FAR            ; *** BOUND Range
         PUSH    BP
         MOV     BP,SP
         PUSH    AX
         MOV     AX,0805H       ; SimCode=08H, DosCode=05H
         JMP     SHORT I@TRAPH
I@ILLOPC LABEL   FAR            ; *** Invalid Opcode
         PUSH    BP
         MOV     BP,SP
         PUSH    AX
         MOV     AX,0906H       ; SimCode=09H, DosCode=06H
         JMP     SHORT I@TRAPH
I@PROT87 LABEL   FAR            ; *** Segment Overrun in87
         PUSH    BP
         MOV     BP,SP
         PUSH    AX
         MOV     AX,0809H       ; SimCode=08H, DosCode=09H
         JMP     SHORT I@TRAPH
I@PROT86 LABEL   FAR            ; *** Segment Overrun in86
         PUSH    BP
         MOV     BP,SP
         PUSH    AX
         MOV     AX,080DH       ; SimCode=08H, DosCode=0DH = 13
         JMP     SHORT I@TRAPH

I@TRAPH: PUSH    CX
         PUSH    DX
         PUSH    BX
         PUSH    SI
         PUSH    DI
         PUSH    ES
         PUSH    DS
         MOV     SI,BP
         ADD     SI,8
         PUSH    SI   ; User's SP
         PUSH    AX   ; SimCode,DosCode
         CALL    FAR  PTR E@TRAPH+3
         POP     SI   ; SimCode,DosCode
         POP     SI   ; Saved user's SP
         POP     DS
         POP     ES
         POP     DI
         POP     SI
         POP     BX
         POP     DX
         POP     CX
         POP     AX
         MOV     SP,BP
         POP     BP
         IRET
I@INTR87 LABEL   FAR            ; *** Co-Processor Error Report
         PUSH    BP
         MOV     BP,SP
         PUSH    ES             ; Compensate for SS damaged from NMI
         PUSH    CX
         MOV     CX,SEG DGROUP
         MOV     ES,CX          ; New segment selector established
         PUSH    AX             ; Save User's AX
         MOV     AX,SS:[BP]+2   ; Pick up NPX IDT Selector error code
         MOV     ES:NPXSTAT,AX  ; Treat it as NPX Status Word
         AND     AX,3FH
; ****** Numeric Co-Processor Error Report ******
; ****** AX=<+0>10<precision>1<ufl>1<ofl>1<divz>1<dnor>1<ill-op>1
         MOV     CX,AX
         AND     CX,10H
         JZ      FER1
         PUSH    BX
         PUSH    DS
         FSTENV  ES:O@87CW      ; Store NPX Environment
         WAIT
         MOV     AX,ES:O@87IPS
         MOV     DS,AX
         MOV     BX,ES:O@87IPO  ; ES:BX is Instruction address
         MOV     AX,DS:[BX]     ; Pick up instruction byte
         SHR     AX,1
         AND     AX,3           ; Look at MF in instruction byte
         MOV     BX,ES:O@87DPS
         MOV     DS,BX
         MOV     BX,ES:O@87DPO  ; ES:BX is Data address
         XOR     CX,CX
         CMP     AX,3
         JE      BL16
         CMP     AX,2
         JNE     BL32
         MOV     DS:[BX]+6,CX
         MOV     DS:[BX]+4,CX
BL32:    MOV     DS:[BX]+2,CX
BL16:    MOV     DS:[BX],CX
         FNCLEX
         POP     DS
         POP     BX
         POP     AX
         POP     CX
         POP     ES
         MOV     SP,BP
         POP     BP
         ADD     SP,2           ; Remove Error Code
         IRET                   ; Underflow handling completed
         MOV     AX,0410H       ; SimCode=04H, DosCode=10H = 16
         JMP     NEAR PTR I@TRAPH  ; 4 - Floating Point Underflow
FER1:    MOV     CX,ES
         MOV     SS,CX
         MOV     SP,ES:G@INITSP ; Reestablish SS:SP as in SIMULA
         XOR     CX,CX
         PUSH    CX             ; Pretend flags zero
         MOV     CX,ES:O@87IPS
         PUSH    CX             ; Segment to instruction
         MOV     CX,ES:O@87IPO
         PUSH    CX             ; Offset to instruction
         XOR     CX,CX
         PUSH    CX             ; Pretend old BP empty
         MOV     BP,SP
         PUSH    CX             ; Pretend user's AX
         FNCLEX
         MOV     CX,AX
         AND     CX,08H
         JZ      FER2
         MOV     AX,0310H       ; SimCode=03H, DosCode=10H = 16
         JMP     NEAR PTR I@TRAPH  ; 3 - Floating Point Overflow
FER2:    MOV     CX,AX
         AND     CX,04H
         JZ      FER3
         MOV     AX,0210H       ; SimCode=02H, DosCode=10H = 16
         JMP     NEAR PTR I@TRAPH  ; 2 - Floating Point Division by zero
FER3:    MOV     CX,AX
         AND     CX,01H
         JZ      FER4
         MOV     AX,0110H       ; SimCode=01H, DosCode=10H = 16
         JMP     NEAR PTR I@TRAPH  ; 1 - Invalid Floating Point Operation
FER4:    MOV     AX,0210H       ; SimCode=05H, DosCode=10H = 16
         JMP     NEAR PTR I@TRAPH  ; 5 - Inexact Result
ENDIF


IF XENIX OR UNIX
; **********************************************************************
; *************   U N I X   S I G N A L   H A N D L E R   **************
; **********************************************************************
   IF iAPX286
I@SIGF PROC    FAR          ; ***
       ASSUME  DS:NOTHING
; ..............................
       PUSH    BP
       MOV     BP,SP
       MOV     AX,FSTARG+24  ; Trap Address'segment   XENIX/286
       MOV     DX,FSTARG+22  ; Trap Address'offset    XENIX/286
;      MOV     AX,FSTARG+90  ; Trap Address'segment   XENIX/286 under XENIX/386
;      MOV     DX,FSTARG+86  ; Trap Address'offset    XENIX/286 under XENIX/386
;      MOV     AX,FSTARG+124 ; Trap Address'segment   XENIX/286 under UNIX/386
;      MOV     DX,FSTARG+120 ; Trap Address'offset    XENIX/286 under UNIX/386
       MOV     CX,FSTARG     ; Signal number
; ..............................
       PUSH    AX    ; Trap Address'segment
       PUSH    DX    ; Trap Address'offset
       PUSH    0     ; User's BP to make GTOUTM work
       MOV     SI,SP
       PUSH    BP    ; Old BP
; ???  MOV     BP,SI
       PUSH    0     ; Continuation Address'segment
       PUSH    0     ; Continuation Address'offset

       PUSH    FSTARG+26     ; User's Flag register
       PUSH    FSTARG+16     ; User's Register AX
       PUSH    FSTARG+12     ; User's Register CX
       PUSH    FSTARG+10     ; User's Register DX
       PUSH    FSTARG+14     ; User's Register BX
       PUSH    FSTARG+8      ; User's Register SI
       PUSH    FSTARG+6      ; User's Register DI
       PUSH    FSTARG+4      ; User's Register ES
       PUSH    FSTARG+2      ; User's Register DS
       PUSH    0             ; Space for User's Register SP

       PUSH    0     ; Space for SimCode
       PUSH    CX    ; Signal number
; ..............................
       MOV     BP,SI
       CALL    FAR PTR E@TRAPH+3
       POP     SI    ; Signal number
       POP     SI    ; SimCode

       POP     SI    ; User's Register SP
       POP     SI    ; User's Register DS
       POP     SI    ; User's Register ES
       POP     SI    ; User's Register DI
       POP     SI    ; User's Register SI
       POP     SI    ; User's Register BX
       POP     SI    ; User's Register DX
       POP     SI    ; User's Register CX
       POP     SI    ; User's Register AX
       POP     SI    ; User's Flag register

       POP     DX    ; Continuation Address'offset
       POP     CX    ; Continuation Address'segment
       JCXZ    SL1
       MOV     SP,SS:G@INITSP ; Restore Simula's Stack-Pointer
       XOR     BP,BP
       PUSH    CX    ; Continuation Address'segment
       PUSH    DX    ; Continuation Address'offset
       RET
SL1:
       MOV     SP,BP
       POP     BP    ; Old BP
       POP     SI    ; User's BP !!!!!
       POP     SI    ; Trap Address'offset
       POP     SI    ; Trap Address'segment
       RET
I@SIGF ENDP
   ELSE ; I.E. iAPX386
I@SIGF PROC    NEAR         ; *** Signal from XENIX/UNIX-386
       PUBLIC  I@SIGF
       PUSH    EBP
       MOV     EBP,ESP
;      WAIT
;      FSTSW   AX
;      WAIT
; ..........................
       MOV     EAX,FSTARG+64 ; Trap Address'segment -- NOT USED
       MOV     EDX,FSTARG+60 ; Trap Address'offset
       MOV     ECX,FSTARG    ; Trap Code
; ..........................
       PUSH    EDX           ; Trap Address'offset
       PUSH    0             ; Space for User's EBP to make E@GTOUTM work
       MOV     ESI,ESP       ; Just to make E@GTOUTM work
       PUSH    EBP           ; Old EBP
       PUSH    0             ; Continuation Address'offset

       PUSH    FSTARG+68     ; User's Flag register
       PUSH    FSTARG+48     ; User's Register EAX
       PUSH    FSTARG+44     ; User's Register ECX
       PUSH    FSTARG+40     ; User's Register EDX
       PUSH    FSTARG+36     ; User's Register EBX
       PUSH    FSTARG+24     ; User's Register ESI
       PUSH    FSTARG+20     ; User's Register EDI
       PUSH    FSTARG+72     ; User's Register ESP

       PUSH    0             ; Space for SimCode
       PUSH    ECX           ; Signal number
       MOV     EBP,ESI       ; Just to make E@GTOUTM work
       CALL    E@TRAPH
       POP     ESI           ; Signal number
       POP     ESI           ; SimCode

       POP     ESI           ; User's Register ESP
       POP     ESI           ; User's Register EDI
       POP     ESI           ; User's Register ESI
       POP     ESI           ; User's Register EBX
       POP     ESI           ; User's Register EDX
       POP     ESI           ; User's Register ECX
       POP     ESI           ; User's Register EAX
       POP     ESI           ; User's Flag register

       POP     ECX           ; Continuation Address'offset
       JCXZ    SHORT SL1
       MOV     ESP,G@INITSP  ; Restore Simula's Stack-Pointer
       XOR     EBP,EBP
       PUSH    ECX           ; Continuation Address'offset
       RET
SL1:
       MOV     ESP,EBP
       POP     EBP           ; Old EBP
       POP     ESI           ; User's EBP !!!!!
       POP     ESI           ; Trap Address'offset
       RET
I@SIGF ENDP
   ENDIF
ENDIF


     EVEN
IF iAPX286
S@ENV_TEXT ENDS
ELSE ; I.E. iAPX386
_TEXT ENDS
ENDIF

       END
