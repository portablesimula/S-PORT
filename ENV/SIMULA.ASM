NAME       SIMULA
; ---------------------------------------------------------------
; ---  COPYRIGHT 1990 by                                      ---
; ---  Simula a.s.                                            ---
; ---  Oslo, Norway                                           ---
; ---                                                         ---
; ---                                                         ---
; ---             P O R T A B L E     S I M U L A             ---
; ---                                                         ---
; ---            F O R    i A P X    2 8 6 / 3 8 6            ---
; ---                                                         ---
; ---                                                         ---
; ---            M  A  I  N    P  R  O  G  R  A  M            ---
; ---                                                         ---
; ---------------------------------------------------------------
INCLUDE  versn.def ; Defines SREL=S-PORT Release, SVER=SIMULA Version
INCLUDE  macro.def

IF DOS
; ******  NOTE! do not use EXIT macro as is under DOS !!!!   pje
	PURGE	EXIT
EXIT	MACRO	CODE
  IFDIF <AL>,<CODE>
	MOV	AL,CODE
  ENDIF
	MOV	AH,4CH
	INT	21H
	ENDM
ENDIF

; ***********   L O C A L   M A C R O D E F S   **************

MOVESTR  MACRO  DST,SRC,LNG
   IF iAPX286
         MOV    SI,OFFSET SRC
         MOV    DI,OFFSET DST
         MOV    CX,LNG
         REP    MOVSB
   ELSE ; i.e. 386
         MOV    ESI,OFFSET SRC
         MOV    EDI,OFFSET DST
         MOV    ECX,LNG
         REP    MOVSB
   ENDIF
         ENDM

IF OS2
 ; ***  not implemented user program call
ELSE

OPEN_HANDLE    MACRO PATH,HDL,OERR ; *** access=0 always
IF DOS
         MOV   DX,OFFSET PATH
      ;  MOV   AL,ACCESS
      ;  MOV   AH,3DH
         MOV   AX,3D00H
         INT   21H
         JC    OERR
         MOV   HDL,AX
ELSE ; XENIX - UNIX 
         PUSH  0666       ; rw-rw-rw-  
         PUSH  4          ; nobuffer
     IF iAPX286 AND XENIX
         PUSH  DS         ; = PATH SEG
         PUSH  OFFSET PATH
         CALL  FAR PTR _open      ; AX = -1 or handle
         ADD   SP,8
         MOV   HDL,AX
         INC   AX
         JZ    OERR ; note - must be near
     ELSE ; i.e. iAPX386
         PUSH  OFFSET PATH
         CALL  _open      ; AX = -1 or handle
         ADD   ESP,12
         MOV   HDL,EAX
         INC   EAX
         JZ    SHORT OERR
     ENDIF
ENDIF
         ENDM

CLOSE_HANDLE   MACRO HANDLE
IF DOS
         MOV   BX,HANDLE
         MOV   AH,3EH
         INT   21H
ELSE ; XENIX - UNIX 
     IF iAPX286 AND XENIX
         PUSH  HANDLE
         CALL  FAR PTR _close
         ADD   SP,2
     ELSE ; i.e. iAPX386
         PUSH  HANDLE
         CALL  _close
         ADD   ESP,4
     ENDIF
ENDIF
         ENDM

DELETE   MACRO  PATH
IF DOS
         MOV    DX,OFFSET PATH
         MOV    AH,41H
         INT    21H   ; delete and ignore error
ELSE ; XENIX - UNIX
     IF iAPX286 AND XENIX
         PUSH  DS         ; = PATH SEG
         PUSH  OFFSET PATH
         CALL  FAR PTR _unlink
         ADD   SP,4
     ELSE ; i.e. iAPX386
         PUSH  OFFSET PATH
         CALL  _unlink
         ADD   ESP,4
     ENDIF
ENDIF
         ENDM

ENDIF ; not OS2

; *******   DOS macros, mainly for redirection   *******

IF DOS

READ_HANDLE    MACRO HDL
 ; DX and CX set before call
         MOV   BX,HDL
      ;  MOV   DX,OFFSET BUF
      ;  MOV   CX,BYTES
         MOV   AH,3FH
         INT   21H
         ENDM

CREATE_HANDLE  MACRO  PATH,ATTR
         MOV   DX,OFFSET PATH
         MOV   CX,ATTR
         MOV   AH,3CH
         INT   21H
         ENDM

XDUP     MACRO HANDLE
         MOV   BX,HANDLE
         MOV   AH,45H
         INT   21H
         ENDM

XDUP2    MACRO HANDLE1,HANDLE2
         MOV   BX,HANDLE1
         MOV   CX,HANDLE2
         MOV   AH,46H
         INT   21H
         ENDM

; *** redirect a standard device to some file ***
REDIR    MACRO SAVEOLD,DEVICE,NEWHDL,ELABEL,NEWPATH
              ; SAVEOLD - saved handle of ...
              ; DEVICE  - device number to be redirected into ...
              ; NEWHDL  - saved handle of redirection file
              ; ELABEL  - "cannot redirect"-label
              ; NEWPATH - path of redirection file

         CREATE_HANDLE  NEWPATH,0
         JC    ELABEL
         MOV   NEWHDL,AX
         XDUP  DEVICE
         JC    ELABEL
         MOV   SAVEOLD,AX
         XDUP2 NEWHDL,DEVICE
         JC    ELABEL
         ENDM

; *** reset standard device after redirection ***
RERED    MACRO SAVEOLD,DEVICE,NEWHDL,ELABEL
 ; - see REDIR for parameters
         XDUP2 SAVEOLD,DEVICE
IFDIF    <ELABEL>,<IGNORE>
         JC    ELABEL
ENDIF
         CLOSE_HANDLE   SAVEOLD
IFDIF    <ELABEL>,<IGNORE>
         JC    ELABEL
ENDIF
         CLOSE_HANDLE   NEWHDL
IFDIF    <ELABEL>,<IGNORE>
         JC    ELABEL
ENDIF
         ENDM


_DATA    SEGMENT
 ; *********************  D O S   V A R   A R E A   ******************
 ;
 ; inform2.xcg contains user program call in format:
 ;       <progname.length:byte><progname>  -- must be non-empty
 ;       <arglist.length:byte><arglist>    -- length=0: no arglist
 ;       <redirfil.length:byte><redirfil>  -- length=0: no redir
 ;       progname and redir are ASCIIZERO, arglist is CR-terminated

INFORM2  DB     'inform2.xcg',0,'$'
         DB     0 ; alignment
HANDLE   DW     0 ; 16 bytes alligned
 ; ****  redirection variables
REDIRPATH  DB   72 DUP(0)     ; 4-byte aligned
SAV_OUT  DW     0 ; saved std out handle
SAV_NEW  DW     0 ; saved redir handle
REDIRFLAG  DB   0 ; <>0: redirected output
EXESTAT  DB     0
         DB     2 DUP(0); 16 bytes alignment

PDESCR1  DB     '????s',SREL,'.exe',0,'$' ; - for restoring PDESCR
         DB     2 DUP('$') ; 16 bytes alignment
PATREF   DD     0
PARREF   DD     0
PATSPEC  DB     'PATH='
LSTCHR   DB     0
SUBPOS   DW     0
PREFIX   DB     127 DUP(' '),'\'           ; 16-bytes aligned
PDESCR   DB     '????s',SREL,'.exe',0,'$'  ; 14 bytes length
         DB     70 DUP('$')
PARAM    DB     4,'/P'
POPTN    DB     '1 '
OLDARG   DB     127 DUP('$') ; parameters from original call
         DB     4 DUP('$')

RUNARG   DB     127 DUP('$') ; parameters for user program
         DB     5 DUP('$')   ; 16 bytes aligned

         EVEN ; ******  BE SURE!!! stack must be at even address

         DB  204 DUP(?) ; STACK AREA
STKEND   DW  0
SaveSP   DW  0             ; 16 bytes allign

PARBLK   DW  0             ; No Redefinition of Environment
         DW  OFFSET PARAM  ; Command Line'Offset
         DW  SEG PARAM     ; Command Line'Segment
         DD -1             ; No FCB #1
         DD -1             ; No FCB #2

ENDIF


; *************   O S 2   **********************
IF OS2
         EXTRN    DOSWRITE:FAR
         EXTRN    DOSEXIT:FAR
         EXTRN    DOSEXECPGM:FAR
STACK    SEGMENT
         DB       800 DUP(?)
STACK    ENDS
_DATA    SEGMENT
PDESCR1  DB     '????s',SREL,'.exe',0,'/P2'
NPAR     DW     0
PDESCR   DB     '????s',SREL,'.exe',0,'/P'
POPTN    DB     '1 '
OLDARG   DB     127 DUP(0)
         DB     0
SAVARG   DB     127 DUP(0)
         DB     0
ENDIF

IF iAPX286 AND XENIX

         EXTRN    _errno:WORD
         EXTRN    _open:FAR
         EXTRN    _read:FAR
         EXTRN    _close:FAR
         EXTRN    _unlink:FAR
         EXTRN    _write:FAR
         EXTRN    _system:FAR
         EXTRN    _exit:FAR
_DATA    SEGMENT
INFORM2  DB     './inform2.xcg',0,'$' ; 4-byte aligned
         DB     0 ; alignment
HANDLE   DW     0
PDESCR1  DB     '????s',SREL,'.x16 /P2'
NPAR     DW     0
PDESCR   DB     '????s',SREL,'.x16 /P'
POPTN    DB     '1'
OLDARG   DB     127 DUP(0)
         DB     0
SAVARG   DB     127 DUP(0)
         DB     0
ENDIF

IF iAPX386

         EXTRN    _errno:WORD
         EXTRN    _open:NEAR
         EXTRN    _read:NEAR
         EXTRN    _close:NEAR
         EXTRN    _unlink:NEAR
         EXTRN    _write:NEAR
         EXTRN    _system:NEAR
         EXTRN    _exit:NEAR
_DATA    SEGMENT
INFORM2  DB     './inform2.xcg',0,0,0          ; 4-byte aligned
HANDLE   DD     0
   IF UNIX
PDESCR1  DB     '????s',SREL,'.unx /P2'
NPAR     DD     0
PDESCR   DB     '????s',SREL,'.unx /P'       ; 4-byte aligned
   ELSE ; I.E. XENIX
PDESCR1  DB     '????s',SREL,'.x32 /P2'
NPAR     DD     0
PDESCR   DB     '????s',SREL,'.x32 /P'       ; 4-byte aligned
   ENDIF
POPTN    DB     '1'
OLDARG   DB     127 DUP(0)
         DB     0
SAVARG   DB     127 DUP(0)
         DB     0
ENDIF

IFDEF INTERIM
CVER     DB    'z' ; Initial version
;  MESS1    DB    'ZIMULA'
ELSE
CVER     DB    's' ; Initial version
;  MESS1    DB    'SIMULA'
ENDIF
;           DB    ' Version ',SREL,'.',SVER
;           DB    ' under ',OSID,10,13
;           DB    'Copyright (C) Simula a.s. 1991.'
;           DB    10,10,13,13,13,13,13,13,13,'$'
EMSG1    DB    'Cannot execute "'
EDESCR   DB    'xxxxxrrr.xxx", '
         DB    'Simula may not be installed properly',10,13,'$'
EMSG2    DB    'Warning: no PATH found',10,13,'$'
EMSG3    DB    'Cannot read/interprete INFORM2.XCG',10,13,'$'
EMSG4    DB    '- may be insufficient memory',10,13,'$'
IF DOS   ; no use for temp var
ELSE
EXITCOD  DB    0 ; saved return code from EXEC
ENDIF

TAB      LABEL    DWORD
         DB       'mtr0'  ; Component no. 0
         DB       'fec0'  ; Component no. 1
         DB       'sml0'  ; Component no. 2
         DB       'edt0'  ; Component no. 3
         DB       '???0'  ; Component no. 4
         DB       '???0'  ; Component no. 5
         DB       '???0'  ; Component no. 6
         DB       '???0'  ; Component no. 7
         DB       '???0'  ; Component no. 8
         DB       '???0'  ; Component no. 9
         DB       'bc86'  ; Component no. 10
         DB       'b386'  ; Component no. 11
         DB       'bcn5'  ; Component no. 12
         DB       'b??0'  ; Component no. 13
         DB       'lnk0'  ; Component no. 14
         DB       'lnk1'  ; Component no. 15
         DB       'lnk2'  ; Component no. 16
         DB       'lnk3'  ; Component no. 17
         DB       'lnk4'  ; Component no. 18
         DB       'lnk5'  ; Component no. 19

IFDEF TRACE
TRC1     DB       'Execute xx: $'
TRC2     DB       ' as Child.',10,13,'$'
TRC3     DB       'TRY: $'
TRC4     DB       10,13,'$'
TRC5     DB       'her var den: AL=8',10,13,'$'
ENDIF


IF OS2
RetCode  DD       0 ; Return value from OS2-Call
BUF      DB       80 DUP(?)
ENDIF


_DATA    ENDS

         ASSUME   SS:DGROUP,DS:DGROUP

_TEXT    SEGMENT
         ASSUME   CS:_TEXT
_main    PROC
         PUBLIC _main

IF UNIX OR XENIX
   IF iAPX286

;  *************************************************************
;  ****  M A I N   E N T R Y   F R O M   X E N I X / 2 8 6  ****
;  *************************************************************

         PUSH   SS
         POP    DS
         MOV    BX,SP         ; DS:[BX] Pointer to nArg,Argv,Envp
         MOV    CX,DS:4[BX]   ; nArg
         LDS    BX,DS:6[BX]   ; DS:[BX] Points to ArgumentVector
MPAR:    DEC    CX
         JCXZ   EPAR               ; No more parameters
         ADD    BX,4
;  ***   Include 'old' parameter
     ;   MOV    AL,'x'
     ;   MOV    BYTE PTR SS:TAB+2,AL
         LES    DI,DS:[BX]         ; ES:[DI] Old parameter
         MOV    AX,0

         MOV    AL,ES:[DI]
         CMP    AX,'-'
         JNE    PLOOP1
         INC    DI
         MOV    AL,ES:[DI]
         CMP    AX,'w'
         JE     FOUND
         DEC    DI
         JMP    SHORT PLOOP1
FOUND:   MOV    AL,'x'
         MOV    BYTE PTR SS:TAB+2,AL
         JMP    SHORT MPAR

PLOOP1:  MOV    AL,' '
         MOV    SI,SS:NPAR
         MOV    SS:OLDARG[SI],AL
         INC    SI
PLOOP:
         MOV    AL,ES:[DI]
         INC    DI
         CMP    AX,0
         JE     MPAR               ; Go take next parameter
         MOV    SS:OLDARG[SI],AL
         INC    SI
         MOV    SS:NPAR,SI
         JMP    SHORT PLOOP
EPAR:    PUSH   SS
         POP    DS
   ELSE ; I.E. iAPX386

;  ***********************************************************************
;  ****  M A I N   E N T R Y   F R O M   X E N I X / U N I X / 3 8 6  ****
;  ***********************************************************************

         MOV    EBX,ESP      ; [EBX] Pointer to nArg,Argv,Envp
         MOV    ECX,4[EBX]   ; nArg
         MOV    EBX,8[EBX]   ; [EBX] Points to ArgumentVector
MPAR:    DEC    ECX
         JCXZ   EPAR               ; No more parameters
         ADD    EBX,4
;  ***   Include 'old' parameter
    ;    MOV    AL,'x'
    ;    MOV    BYTE PTR TAB+2,AL
         MOV    EDI,[EBX]         ; [EDI] Old parameter
         MOV    EAX,0

     ; text for '-w' as first characters of param
         MOV    AL,[EDI]
         CMP    AL,'-'
         JNE    SHORT PLOOP1
         INC    EDI
         MOV    AL,[EDI]
         CMP    AL,'w'
         JE     SHORT FOUND
         DEC    EDI
         JMP    SHORT PLOOP1
FOUND:   ; -w found, call mtx
         MOV    AL,'x'
         MOV    BYTE PTR TAB+2,AL
         JMP    MPAR               ; Go take next parameter
PLOOP1:  MOV    AL,' '
         MOV    ESI,NPAR
         MOV    OLDARG[ESI],AL
         INC    ESI

         MOV    AL,' '
         MOV    ESI,NPAR
         MOV    OLDARG[ESI],AL
         INC    ESI
PLOOP:
         MOV    AL,[EDI]
         INC    EDI
         CMP    EAX,0
         JE     MPAR               ; Go take next parameter
         MOV    OLDARG[ESI],AL
         INC    ESI
         MOV    NPAR,ESI
         JMP    SHORT PLOOP
EPAR:
   ENDIF
ENDIF
IF OS2

;  ***************************************************
;  ****  M A I N   E N T R Y   F R O M   O S / 2  ****
;  ***************************************************

;        AX = Segment'addr of Environment Segment
;        BX = Offset of the command line in Environment Segment
         MOV    ES,AX
         MOV    DI,BX              ; ES:[DI] Old parameter
         ASSUME ES:NOTHING
;  ***   Include 'old' OS/2 parameters
         MOV    AX,0
PL1:     MOV    AL,ES:[DI]
         INC    DI
         CMP    AX,0
         JNE    PL1                ; Skip Program-ident
         JMP    SHORT PL3
PL2:     INC    DI
PL3:     MOV    AL,ES:[DI]
         CMP    AX,0
         JE     EPAR               ; No parameter
         CMP    AX,' '
         JE     PL2                ; Skip Leading Blanks

         CMP    AX,'-'
         JNE    PLOOP1
         INC    DI
         MOV    AL,ES:[DI]
         CMP    AX,'w'
         JE     FOUND
         DEC    DI
         JMP    SHORT PLOOP1
FOUND:   MOV    AL,'x'
         MOV    BYTE PTR TAB+2,AL  ; Use other Monitor
SKIP:    INC    DI
         MOV    AL,ES:[DI]
         CMP    AX,0
         JE     EPAR               ; No parameter
         CMP    AX,' '
         JNE    SKIP               ; Skip until blank or nomore
         JMP    SHORT PL2

PLOOP1:  MOV    SI,NPAR
PL4:     MOV    AL,ES:[DI]
         INC    DI
         CMP    AX,0
         JE     EPAR               ; End parameters
         MOV    OLDARG[SI],AL
         INC    SI
         MOV    NPAR,SI
         JMP    SHORT PL4
EPAR:    PUSH   DS
         POP    ES
ENDIF
IF DOS

;  *************************************************
;  ****  M A I N   E N T R Y   F R O M   D O S  ****
;  *************************************************

;        DS = Segment'addr of ProgramSegmentPrefix (PSP)
         ASSUME DS:NOTHING
         MOV    SI,44
         MOV    AX,DS:[SI]
         MOV    ES,AX
         ASSUME ES:NOTHING
         XOR    DI,DI            ; ES:[DI] is address of EnvStrings
   IFDEF TRACE
         PUSH   DS
         PUSH   ES
         POP    DS
         ASSUME DS:NOTHING
         PRT    160,DI          ;  *** Print EnvStrings ***
         MOV    AX,SEG STKEND
         MOV    DS,AX
         ASSUME DS:DGROUP
         PRT    2,<OFFSET TRC4> ;  *** Print newline ***
         POP    DS
         ASSUME DS:NOTHING
   ENDIF
         MOV    BX,DS
         MOV    AX,SEG STKEND
         MOV    DS,AX
         ASSUME DS:DGROUP
         MOV    CX,OFFSET STKEND
         AND    CL,0FEH            ; make SURE SP is even !!!
         CLI
         MOV    SS,AX
         MOV    SP,CX
         STI
         ASSUME SS:DGROUP,DS:DGROUP
         MOV    WORD PTR PARREF+2,BX
         MOV    SI,128
         MOV    WORD PTR PARREF,SI ; PARREF is address of nPar,ParString
         CLD
;  ***   SEARCH FOR PATH=string   ***
LOOP:    MOV    CX,5
         MOV    SI,OFFSET PATSPEC
         MOV    BX,DI
         REPE   CMPSB    ; Test against 'PATH='
         JE     FOUND
         MOV    DI,BX
         XOR    AX,AX
         MOV    CX,255
         REPNE  SCASB    ; Find next Zero-byte
         MOV    AX,ES:[DI]
         OR     AX,AX
         JNZ    LOOP
         PRT    24,<OFFSET EMSG2> ; *** Print Warning ***
         JMP    SHORT EPATH
FOUND:   MOV    AX,ES
         MOV    WORD PTR PATREF+2,AX
         MOV    WORD PTR PATREF,DI       ; PATREF is address of PATH'String
EPATH:   PUSH   DS
         POP    ES
         ASSUME ES:DGROUP
;  ***   Include 'old' DOS parameters
         MOV    DI,OFFSET OLDARG
         LDS    SI,PARREF
         ASSUME DS:NOTHING
	MOV	DL,' '
	MOV	DH,'w'             ; <w-flag,separator>

	CALL	COPYARG

	OR	DX,DX
	JZ 	NOWIN
         MOV    AL,'x'             ; '-w' found: call windowMtr
         MOV    BYTE PTR ES:TAB+2,AL
NOWIN:
PL4:     ADD    CL,BYTE PTR ES:PARAM 
         MOV    BYTE PTR ES:PARAM,CL ; nPar:=nPar+Old'nPar.frontstrip
EPAR:    PUSH   ES
         POP    DS
         ASSUME DS:DGROUP
ENDIF

;  ***************************************************************************
;  ****  C O M M O N   F O R   A L L   O P E R A T I N G   S Y S T E M S  ****
;  ***************************************************************************

    IF   DOS
      ;  nothing
    ELSE
         MOVESTR SAVARG,OLDARG,128
    ENDIF

;  ****  Print Messages  ****
;           PRT    83,<OFFSET MESS1> ;  ****  Print Messages  ****
EXEMON:  ;  ****   Execute: MONITOR /P?  (?=1 at first call, else 2)
         XOR    AL,AL             ; Component 0
         MOV    CL,CVER           ; Current Version
         CALL   SETUP
         CALL   EXEC              ; AL <-- Return Code
;  ****  Update P-Option to /P2  ****
         MOV    CL,'2'
         MOV    POPTN,CL
EXECOMP: ;  ****   Execute component /P2 as defined by AL (=returnCode)
;  ****  ReturnCode >= 50  ==> Vers:='z', ReturnCode:=ReturnCode-40
;  ****  ReturnCode >= 30  ==> Vers:='t', ReturnCode:=ReturnCode-20
;  ****  Else              ==> Vers:='s'
         MOV    CL,'s'            ; Standard Version
         CMP    AL,50
         JB     SHORT LL1b
         MOV    CL,'z'            ; Interim Version
         SUB    AL,40
LL1b:    CMP    AL,30
         JB     SHORT LL1c
         MOV    CL,'t'            ; Tracing Version
         SUB    AL,20
LL1c: ;  ****   If (ReturnCode < 10) or (ReturnCode > 29) then EXIT
         CMP    AL,10
         JB     SHORT LL2
         CMP    AL,30
         JB     SHORT LL3
LL2:
IF DOS
         EXIT   AL
ELSE
         EXIT   0                 ; TEMP!
ENDIF
LL3:     MOV    CVER,CL           ; Set New Current Version
      ;  ****   Execute Component no. AL
         SUB    AL,10
IF OS2
 ; not implemented
ELSE
         CMP    AL,3             ; is this user program call ?
         JE     SHORT USR        ; - yes
ENDIF
         CALL   SETUP
         CALL   EXEC              ;    AL <-- Return Code
      ;  ****   If (ReturnCode < 10) or (ReturnCode > 80) then MNTR next
      ;  ***    Else execute component
;        CMP    AL,10
;        JB     SHORT EXEMON
;        CMP    AL,80
;        JB     SHORT EXECOMP ; *** execute component
;        JMP    SHORT EXEMON
         CMP    AL,10
         JB     SHORT XTOLL1
         CMP    AL,80
         JB     SHORT EXECOMP
IF OS2
XTOLL1:  JMP    SHORT EXEMON
 ; call user program/redirection not implemented
ELSE
XTOLL1:  JMP    TOLL1       ; execute component WITHOUT resetting redir!!!
USR:  ;  ****   Execute user program specified in INFORM2.XCG
     ;   ****   open INFORM2.XCG
         OPEN_HANDLE INFORM2,HANDLE,USRERR
     ;   ****   read program name
      IF iAPX286
         MOV    DX,OFFSET PDESCR
      ELSE
         MOV    EDX,OFFSET PDESCR
      ENDIF
         CALL   READSTR
         JCXZ   USRERR
IF DOS
     ;   ****   read parameter string
         MOV    DX,OFFSET RUNARG
         CALL   READSTR
         JCXZ   USRERR
         MOV    PARBLK+2,OFFSET RUNARG
     ;   ****   perform redirection (if any and not done before)
         CMP    REDIRFLAG,0
         JNZ    CL1
         MOV    DX,OFFSET REDIRPATH
         CALL   READSTR
         MOV    REDIRFLAG,CL
         JCXZ   CL1
     ;   ****   perhaps later: redirect std error as well
         REDIR  SAV_OUT,1,SAV_NEW,USRERR,REDIRPATH
ENDIF
         JMP    SHORT CL1

USRERR:  PRT    37,<OFFSET EMSG3> ; ****  Print Messages  ****
         EXIT   5                 ; don't START, just error-exit
CL1: ;   ****   close inform2.xcg (ignore OS error codes)
         CLOSE_HANDLE HANDLE
         DELETE INFORM2
         CALL   EXEC              ;    AL <-- Return Code
     ;   ****   Restore overwritten PDESCR and PARBLK
         MOVESTR  PDESCR,PDESCR1,16
    IF   DOS
         MOV    PARBLK+2,OFFSET PARAM
      ;  nothing
    ELSE
         MOVESTR OLDARG,SAVARG,128
    ENDIF
      ;  ****   If (ReturnCode < 10) or (ReturnCode >= 80) then MNTR next
      ;  ***    Else execute component
         CMP    AL,10
         JB     SHORT TOLL1
         CMP    AL,80
         JNB    SHORT TOLL1
         JMP    EXECOMP ; execute component WITHOUT resetting redir!!!
TOLL1:
   IF DOS
     ;   ****   if redirection, stop it
         CMP    REDIRFLAG,0
         JZ     LM1
         XOR    AX,AX
         MOV    REDIRFLAG,AL
         RERED  SAV_OUT,1,SAV_NEW,IGNORE
   ENDIF

       ; XOR    AX,AX ; execute monitor next
LM1:     JMP    EXEMON

ENDIF ; not OS2

_main    ENDP

IF OS2
; *** not implemented
ELSE

READSTR     PROC  NEAR
;  read a string (or possibly nothing) from HANDLE
;  CX flags whether anything read (<>0 at exit)
;  ****  destination in DX, HANDLE set
   IF XENIX OR UNIX
      IF iAPX286
         PUSH  128        ; nBytes, at most 128
         PUSH  DS         ; buffer SEGM
         PUSH  DX         ; buffer OFFSET
         PUSH  HANDLE
         CALL  FAR PTR _read
         ADD   SP,8
         CMP   AX,0       ; AX is -1 or nBytes
         JS    EEXIT ; note - must be near
         MOV   CX,AX       ; result in CX
OEXIT:   RET
EEXIT:   MOV    CX,0       ; i.e. nothing read
         RET
      ELSE ; i.e. iAPX386
         PUSH  128         ; nBytes, at most 128
         PUSH  EDX         ; buffer
         PUSH  HANDLE
         CALL  _read
         ADD   ESP,12
         CMP   EAX,0 ; AX is -1 or nBytes
         JS    SHORT EEXIT
         MOV   ECX,EAX       ; result in CX
OEXIT:   RET
EEXIT:   MOV    ECX,0
         RET
      ENDIF
   ENDIF
   IF DOS
         XOR    AX,AX
         PUSH   DX
         MOV    DI,DX
         MOV    DS:[DI],AL
         MOV    CX,1
         READ_HANDLE HANDLE ; DOS: read length byte
         XOR    CX,CX
         POP    DX
         JC     OEXIT
         MOV    DI,DX
         MOV    CL,DS:[DI]  ; length byte
         JCXZ   OEXIT
         READ_HANDLE HANDLE ; - overwriting length i.e. same dest
         JNC    OEXIT
         XOR    CX,CX      ; i.e. nothing read
OEXIT:   RET
    ENDIF
READSTR  ENDP

ENDIF


SETUP    PROC   NEAR
;  ****  AL  =  Component Number
;  ****  CL  =  Version Letter
IF iAPX286
         XOR    AH,AH
         MOV    BX,AX
         SHL    BX,1
         SHL    BX,1
         MOV    DI,WORD PTR TAB[BX]
         MOV    WORD PTR PDESCR,DI
         MOV    DI,WORD PTR TAB+2[BX]
         MOV    WORD PTR PDESCR+2,DI
ELSE ; I.E. iAPX386
         AND    EAX,0FFH
         MOV    EDI,TAB[4*EAX]
         MOV    DWORD PTR PDESCR,EDI
ENDIF
IFDEF INTERIM
         MOV    CL,'z'            ; Interim Version
ENDIF
         MOV    PDESCR+4,CL       ; set Version Letter
IFDEF TRACE
;        AL     Component Number
         AND    AL,0FH
         ADD    AL,48
         CMP    AL,57
         JB     SHORT TT1
         ADD    AL,7
TT1:     MOV    TRC1+9,AL
         MOV    AL,AH
         SHR    AL,1
         SHR    AL,1
         SHR    AL,1
         SHR    AL,1
         AND    AL,0FH
         ADD    AL,48
         CMP    AL,57
         JB     SHORT TT2
         ADD    AL,7
TT2:     MOV    TRC1+8,AL
         PRT    12,<OFFSET TRC1>   ;  ****  Print Messages  ****
         PRT    36,<OFFSET PDESCR> ;  ****  Print Messages  ****
IF DOS
         PRT    130,<OFFSET PARAM+1> ;  ****  Print Messages  ****
ENDIF
         PRT    12,<OFFSET TRC2>   ;  ****  Print Messages  ****
ENDIF
         RET
SETUP    ENDP


IF DOS

INCLUDE	copyarg.inc

DOEXEC   PROC   NEAR ; to save some bytes (DX set before call)
         MOV    SaveSP,SP        ; ****  Save SP
         MOV    BX,OFFSET PARBLK ; ES:BX <-- Pointer to PARBLK
         MOV    AX,4B00H         ; ****  Do DOS-Call EXEC
         INT    21H
         MOV    BX,SEG STKEND
         MOV    DS,BX            ; Restore DS  WITHOUT CHANGING THE FLAGS ****
         MOV    ES,BX            ; Restore ES  WITHOUT CHANGING THE FLAGS ****
         CLI
         MOV    SS,BX            ; Restore SS  WITHOUT CHANGING THE FLAGS ****
         MOV    SP,SaveSP        ; Restore SP  WITHOUT CHANGING THE FLAGS ****
         STI
         JNC    DORET
         CMP    AX,8
         JNE    NOTMRY
IFDEF TRACE
         PUSH   AX
         PRT    19,<OFFSET TRC5>
         POP    AX
ENDIF
         MOV    EXESTAT,AL
NOTMRY:  STC
DORET:   RET
DOEXEC   ENDP
ENDIF

EXEC     PROC   NEAR
IF DOS
;  *********************************************************
;  ****  E X E C U T E   D O S   S U B - P R O G R A M  ****
;  *********************************************************

         MOV    DX,OFFSET PDESCR ; DS:DX <-- Pointer to File-Name
         CALL   DOEXEC
         JNC    OKRETUR
         MOV    AX,WORD PTR PATREF
         OR     AX,AX
         JZ     ERR1             ; ****  Component not found  ****
         MOV    SUBPOS,AX
NXTRY:   MOV    SI,SS:SUBPOS
         MOV    AX,WORD PTR SS:PATREF+2
         MOV    DS,AX
         ASSUME DS:NOTHING
PS1:     LODSB
         CMP    AL,';'
         JE     PS2
         OR     AL,AL
         JNZ    PS1
PS2:     MOV    SS:LSTCHR,AL
         MOV    AX,SS:SUBPOS
         MOV    SS:SUBPOS,SI
         MOV    CX,SI
         SUB    CX,AX
         DEC    CX
         MOV    SI,AX
;  ****  DS:[SI]   Start of current sub-path
;  ****  CX        Length
         MOV    DI,OFFSET PDESCR-1
         SUB    DI,CX
         MOV    DX,DI
         REP    MOVSB

         PUSH   SS
         POP    DS
   IFDEF TRACE
         PUSH DX
         PRT  5,<OFFSET TRC3> ;  ****  Print 'TRY: '  ****
         POP  DX
         PUSH DX
         PRT  0,DX
         PRT  2,<OFFSET TRC4> ;  ****  Print newline  ****
         POP  DX
   ENDIF
         CALL   DOEXEC
         JNC    OKRETUR
         MOV    AL,LSTCHR
         OR     AL,AL
; !!!    JNE    NXTRY
         JZ     ERR1          ; ****  Component not found  ****
         JMP    NXTRY
OKRETUR: 
         XOR    AL,AL
         MOV    EXESTAT,AL
         MOV    AH,4DH           ; ****  Get Return Code  ****
         INT    21H              ; AX := Return Code
         RET
ENDIF

IF OS2
;  ***********************************************************
;  ****  E X E C U T E   O S / 2   S U B - P R O G R A M  ****
;  ***********************************************************

         PUSH   SS                ; BUF'Segment
         PUSH   OFFSET BUF        ; BUF'Offset
         PUSH   80                ; BUF'Length
         PUSH   0                 ; Execute Synchronously
         PUSH   SS                ; Argument String'Segment
         PUSH   OFFSET PDESCR     ; Argument String'Offset
         PUSH   0                 ; Environment String (none)'Segment
         PUSH   0                 ; Environment String (none)'Offset
         PUSH   SS                ; ReturnCode'Segment
         PUSH   OFFSET RetCode    ; ReturnCode'Offset
         PUSH   SS                ; FileName'Segment
         PUSH   OFFSET PDESCR     ; FileName'Offset
         CALL   FAR PTR DOSEXECPGM
         OR     AX,AX             ; *** Test For ERROR (AX <> 0) ***
         JNZ    ERR1              ; *** Component not found      ***
         MOV    AL,BYTE PTR SS:RetCode+2   ; AL := ReturnCode'Word2.LOW
         MOV    EXITCOD,AL
         RET
ENDIF

IF XENIX OR UNIX
;  ***********************************************************************
;  ****  E X E C U T E   X E N I X / U N I X   S U B - P R O G R A M  ****
;  ***********************************************************************

   IF iAPX286
   IFDEF TRACE
         PRT  5,<OFFSET TRC3> ;  ****  Print 'TRY: '  ****
         PRT  12,<OFFSET PDESCR>
         PRT  2,<OFFSET TRC4> ;  ****  Print newline  ****
   ENDIF
         PUSH   DS ; SS:BX Points to <FileName> <Param>00
         PUSH   OFFSET PDESCR
         CALL   FAR PTR _system  ; AH <-- Return Code
         ADD    SP,4
         PUSH   SS
         POP    DS
   ELSE ; I.E. iAPX386
         PUSH   OFFSET PDESCR ; <FileName> <Param>00
         CALL   _system  ; AH <-- Return Code
         ADD    ESP,4
   ENDIF
         MOV    AL,AH
         MOV    EXITCOD,AL
         RET
ENDIF

ERR1:
         MOVESTR EDESCR,PDESCR,12
         PRT    69,<OFFSET EMSG1> ; ****  Print Messages  ****
IF DOS   ; test whether insuff. mem.
         CMP    EXESTAT,8
         JNE    NOTMEM
         PRT    30,<OFFSET EMSG4> ; *** Print 'insuff. memory' ***
NOTMEM:
ENDIF
         EXIT   4                 ; don't START, just error-exit
EXEC     ENDP


_TEXT    ENDS
         END    _main
