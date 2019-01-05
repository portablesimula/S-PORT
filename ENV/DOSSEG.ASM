NAME       DOSSEG
; ---------------------------------------------------------------
; ---  COPYRIGHT 1989 by                                      ---
; ---  Simula a.s.                                            ---
; ---  Oslo, Norway                                           ---
; ---                                                         ---
; ---             P O R T A B L E     S I M U L A             ---
; ---                                                         ---
; ---             D  O  S     S  E  G  M  E  N  T             ---
; ---                                                         ---
; ---             D  E  F  I  N  I  T  I  O  N  S             ---
; ---------------------------------------------------------------
; ---------------------------------------------------------------
; ---  Simula a.s sept. 1992
; ---  Initialisation file - MUST BE FIRST IN LINK
; ---
; ---  Provided in source to allow local modifications
; ---  (IF you know what you are doing!!!)
; ---  provided as is, no responsibilities assumed, Simula does
; ---  not provide support on this file
; ---  WARNING: DO NOT CHANGE if it says so down there !!!
; ---  The following MUST be done in this module:
; ---	initialisation of SS and SP as shown
; ---	call _main as shown
; ---------------------------------------------------------------


S@ENV_TEXT SEGMENT WORD PUBLIC 'FAR_CODE' ; BYTE -> WORD pj
S@ENV_TEXT ENDS
LINENUMBER SEGMENT BYTE PUBLIC 'LIN_CODE'
LINENUMBER ENDS
_TEXT      SEGMENT PARA PUBLIC 'CODE'
_TEXT      ENDS
FAR_DATA   SEGMENT PARA PUBLIC 'FAR_DATA'
FAR_DATA   ENDS
FAR_BSS    SEGMENT PARA PUBLIC 'FAR_BSS'
FAR_BSS    ENDS
_DATA      SEGMENT WORD PUBLIC 'DATA'

IFDEF DOS
   IFDEF VOPTV
  ; *** local data for argvector init
ARGCOPY	DB	128 DUP(0)	; command line parameter copy
     	DW	0		; 4/byte allign
ARG0	DB	'S',0		; default program name under DOS 2.x
PSP	DW	0		; PSP segment
ARG0REF DW	OFFSET DGROUP:ARG0	; arg(0) address, may be changed below
	DW	SEG ARG0
ARGV	DW	0		; addr in stack of argvector
ENVP	DW	0		; addr in stack of envir vector
ARGC	DW	0		; number of command line parameters
			; paragraph aligned here
   ENDIF ; VOPTV
ENDIF ;; DOS

_DATA      ENDS
CONST      SEGMENT WORD PUBLIC 'CONST'
CONST      ENDS
_BSS       SEGMENT WORD PUBLIC 'BSS'
_BSS       ENDS
STACK      SEGMENT PARA STACK  'STACK'
STACK      ENDS
DGROUP     GROUP  _DATA,CONST,_BSS,STACK

; *************************************************************
; ******   M A I N   E N T R Y   F R O M   M S - D O S   ******
; *************************************************************
; ++++++ changes (version 9.4.8) under DOS to accomodate C interface:
; ++++++    abort if DOS version less than 2
; ++++++    set SS and SP
; ++++++    scan arguments and environment block,
; ++++++    set up vectors on stack and
; ++++++    call far _main( argc, argv, envp )
; ++++++ adds _exit entry point, so that Simula may exit thru
; ++++++ C _exit if this module is replaced by e.g. MS-C crt0.
; ++++++ NOTE: this may create trouble with C routines, se README
; ++++++       on the installation diskettes
 
           EXTRN   _main:FAR
IFDEF DOS
   IFDEF VOPTV
           EXTRN   G@STKEND:WORD ;; do not change!!!
   ENDIF
ENDIF
S@ENV_TEXT SEGMENT
           ASSUME  CS:S@ENV_TEXT
           ASSUME  DS:NOTHING
_start     PROC    FAR
           PUBLIC  _start
           ASSUME  DS:NOTHING
IFDEF DOS
   IFDEF VOPTV
; check that version is at least 2.0
	MOV	AH,30H
	INT	21H
	CMP	AL,2
        JAE	DOSOK
BADENV:
	INT	20H	; terminate if DOS 1.x
DOSOK:
; set SS and SP
; DO NOT CHANGE ==================================
	MOV	SI,SEG DGROUP
	MOV	BX,OFFSET DGROUP:G@STKEND
	AND	BL,0FEH	; **** make SURE that SP is even !!! ****
	CLI
	MOV	SS,SI
	MOV	SP,BX
        STI
; DO NOT CHANGE END ==============================
; set PSP, DS, DOS version and Simula direction
	MOV	SI,DS
	PUSH	SS
	POP	DS
	ASSUME	DS:DGROUP
	MOV	PSP,SI
	CLD

; *********  envp initialisation  *************
; scan environment block and set up vector terminated by a null pointer
;      NB: the block is not copied
	MOV	ES,PSP
	MOV	DI,2CH		; offset env block segment
	MOV	DI,ES:[DI]
	MOV	ES,DI
	XOR	DI,DI
	MOV	CX,8000H	; max size of envblock 32K
	XOR	BX,BX
  ; now ES:DI addresses environment strings
  ;     AX is zero always
  ;     BX counts #strings
  ;     CX counts down in envblock (just in case)
  ; first set null pointer (not counted)
	XOR	AX,AX
	PUSH	AX
	PUSH	AX		; null pointer terminate envp
ENVSTR:
	INC	BX		; note: there's allways PATH!?!
	REPNZ	SCASB
	JCXZ	BADENV		; env block destroyed ???
	CMP	BYTE PTR ES:[DI],AL
	JNE	ENVSTR		; not double zero - more strings
	SHL	BX,1
	SHL	BX,1		; 4 * count bytes for envp
	SUB	SP,BX		; reserve space
	MOV	BP,SP		; SS:BP dest for env string pointer
	MOV	SS:ENVP,BP	; envp address in stack
	XOR	DI,DI		; prepare for restart scan
	MOV	CX,8000H	; max size of envblock 32K
ENVLOOP:
	MOV	[BP],DI
	MOV	[BP]+2,ES
	ADD	BP,4
	REPNZ	SCASB
	CMP	BYTE PTR ES:[DI],AL
	JNE	ENVLOOP		; not double zero - more strings
	
; *********  argv initialisation  *********
; copy command line params into local area with quote/backslash treat
;           but no wildcard handling !!!
; argvector points into local area, terminated by pointer to null string
; zero'th arg is program name, or string "s" (DOS 2.x)

	MOV	AH,30H
	INT	21H
	CMP	AL,2
	JE	DOS2
	ADD	DI,3		; skip last zero + count word
	MOV	SS:ARG0REF,DI
	MOV	SS:ARG0REF+2,ES
DOS2:
	PUSH	DS
	POP	ES
	ASSUME	ES:DGROUP
	MOV	DI,OFFSET DGROUP:ARGCOPY
	MOV	SI,PSP
	MOV	DS,SI
	ASSUME	DS:NOTHING
	XOR	DX,DX

  ; now ES:DI addresses copy area for arguments
  ;     DS is PSP segment, DX is zero (no -w scan, zero term)

	CALL	COPYARG

  ; ES:DI addresses terminal CR
  ; BX = number of parameters found + 1
  ; CX = number of bytes stored in ARGCOPY excl. term CR

  ; zero-terminate last arg (instead of CR)
	XOR	AX,AX
	MOV	BYTE PTR ES:[DI],AL

	MOV	SS:ARGC,BX	; true count incl. program name, excl:
  ; set terminating null pointer
	PUSH	ES		; ES:DI -> term zero of last arg
	PUSH	DI		; - i.e. pointer to null string
  ; reserve space on stack for argvector and set argv
	SHL	BX,1
	SHL	BX,1		; 4 * count bytes for argv
	SUB	SP,BX		; reserve space
	MOV	BP,SP
	MOV	SS:ARGV,BP
  ; argv(0):=program name
	MOV	AX,SS:ARG0REF
	MOV	[BP],AX
	MOV	AX,SS:ARG0REF+2
	MOV	[BP]+2,AX
  ; prepare for restart scan
	MOV	DI,OFFSET DGROUP:ARGCOPY
	INC	CX
	XOR	AX,AX
ARGSET:
	ADD	BP,4
	SUB	BX,4
	OR	BX,BX
	JZ	AZSET
	MOV	[BP],DI
	MOV	[BP]+2,ES
	REPNZ	SCASB
	JCXZ	AZSET
	JMP	SHORT ARGSET
AZSET:

; _main( argc, argv, envp )
	PUSH	SS
	PUSH	SS:ENVP		; envp pushed (offset,segment=DGROUP)
	PUSH	SS
	PUSH	SS:ARGV		; argv pushed (offset,segment=DGROUP)
	PUSH	SS:ARGC
	CALL	FAR PTR _main
	MOV	AL,255		; error code 255 if return from main
        MOV     AH,4CH
        INT     21H
  ELSE ; i.e. old (no Voptn)
           JMP     FAR PTR _main
  ENDIF
ELSE ; i.e. OS2
           JMP     FAR PTR _main
ENDIF
_start     ENDP
           EVEN
IFDEF DOS
  IFDEF VOPTV
INCLUDE	copyarg.inc

	EVEN

; external C procedure exit is exit(code); short integer code; ;
; -- to enable Simula to exit thru C exit if interfacing to C
;    libraries/programs that must have C init file instead of this
; ----------   D O S   O N L Y   -------------------------------

_exit	PROC	FAR  ;; must be called FAR !!!
	PUBLIC	_exit 
	ASSUME	DS:NOTHING
	NOP          ;; - to catch _exit+3 entries
	NOP
	NOP
	NOP
	POP	AX
	POP	AX   ; forget return address
	POP	AX
	MOV	AH,4CH
	INT	21H
_exit      ENDP
   ENDIF
ENDIF ;; DOS

           EVEN

S@ENV_TEXT ENDS

           END   _start
