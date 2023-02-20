; ---------------------------------------------------------------
; ---  Simula a.s sept. 1992
; ---  Include file for ENVIR.ASM and DOSSEG.ASM
; ---
; ---  Provided in source to allow local modifications to these
; ---  two files (IF you know what you are doing!!!)
; ---------------------------------------------------------------

; this routine copies the command line parameters from PSP to
; ES:DI. during copying leading whitespace (SP and TAB) is skipped,
; and all parameters are terminated with the character in DL.
; (because the routine is intended to be used both to set up a local
; command line copy, and to produce cmdline for subprogram).
;
; if DH <> 0 at entry, the return value of DH will indicate:
;  - 0: '-w' not found
;  - 1: parameter '-w' found (where w is the char in DH)
;       this parameter is NOT copied
; (used by Simula monitor to decide between monitors)
;
; does backslash/quote treatment after C conventions:
;  - quoted strings are copied without quotes
;  - backslashes followed by a quote are treated as literal backslash:
;    an even number are copied as half that number, and " is treated
;    an odd number is also copied as half that number, but the last is
;    interpreted to mean 'take " literal' i.e. the quote is copied:
;
;	mov	DS,<PSP segment>
;	mov	ES,<dest segment>
;	mov	DI,<dest offset>
;	mov	DL,<separator>	  either zero or space
;	mov	DH,<w>            either null or some char (e.g. w)
;	call	moveargs
;	return:	BX = number of parameters
;		CX = length of copy excl. final CR
;		DX <> 0: -w found (and skipped)
;		ES:DI address of terminating CR

GOSPACE	MACRO	DEST ;; skip whitespace
	CMP	AL,' '
	JE	DEST
	CMP	AL,09H	;; TAB
	JE	DEST
	ENDM

GOTERM	MACRO	DEST ;; jump if terminator
  	CMP	AL,0DH	;; CR
  	JE	DEST
	OR	AL,AL
	JE	DEST	;; null terminates too
	ENDM

GETNXT	MACRO		;; next char and backslash handler
	LOCAL	L1
	LODSB
	CMP	AL,'\'
	JNE	L1
	CALL	BSLASH
L1:
	ENDM

COPYARG	PROC	NEAR
;
;     ES:DI addresses copy area for arguments
;     DS:SI addresses arguments in PSP block (max 128 bytes)
;     BL    counts parameters (for argc)
;     DL    contains the separator to be set between args in copy (NULL or SP)
;
 	PUSH	DI		; used to compute #chars stored at end
	MOV	SI,81H		; offset(command line param)
	XOR	BX,BX
	PUSH	BX		; no arg '-w' found (yet)
	INC	BX		; - always one param: prog name
NEXTARG:			; repeat
	LODSB			;    repeat nextchar
	GOSPACE	NEXTARG		;    while separator do endrepeat
	GOTERM	ARGLAST		;       if terminator then goto ARGLAST endif
	CMP	AL,'-'		;    -- -w fudge:
	JNE	ARGNW		;    -- check for '-' and check w-flag
	CALL	ARG_W		;    -- both true, check it out
	JCXZ	ARGNW		;    -- no, wasn't it
	POP	AX
	PUSH	CX		;    -- yes, set flag
	JMP	SHORT NEXTARG   ;    -- and skip this one
ARGNW:	INC	BL		;    argc:=argc+1
	DEC	SI		;    backupChar -- i.e. read again
ARGLOOP:			;    repeat
	GETNXT			;       nextchar
				;       if char='\' then char:=bslash endif
	CMP	AL,'"'		;       if char='"'
	JNE	ARGL1
	CALL	COPYQ		;       then char:=copyq endif
ARGL1:				;       --- char<>'\' and char<>'"'
	GOTERM	ARGLAST		;       if terminator then goto ARGLAST endif
	GOSPACE	ARGSEPR		;    while not separator
	STOSB			;    do store(char) endrepeat;
	JMP	SHORT ARGLOOP
ARGSEPR:
	LODSB
	GOSPACE	ARGSEPR
	GOTERM	ARGLAST		;       if terminator then goto ARGLAST endif
	MOV 	AL,DL		; 
	STOSB			;    store(newsep);
	DEC	SI		;    backupChar -- i.e. read again
	JMP	NEXTARG		; while true do endrepeat
ARGLAST:			;ARGLAST:
	MOV	BYTE PTR ES:[DI],0DH	; store(CR)
	POP	DX
 	POP	AX
 	MOV	CX,DI
 	SUB	CX,AX
	RET
COPYARG	ENDP


COPYQ	PROC	NEAR
; copy quoted string, always return next char (<> '\' always)
				; repeat
CQ1:	GETNXT			;    repeat nextchar
				;       if char='\' then char:=bslash endif;
CQ2:	GOTERM	STOPQ		;       if terminator then goto STOPQ endif
	CMP	AL,'"'		;
	JE	STOPQ1		;    while char<>'"'
	STOSB			;    do store(char) endrepeat;
	JMP	SHORT CQ1	;
STOPQ1:				;    nextchar;
	GETNXT			;    if char='\' then char:=bslash endif
	CMP	AL,'"'
	JE	CQ1		; while char='"' do endrepeat
STOPQ:	RET			; STOPQ: copyq:=char
COPYQ	ENDP


BSLASH	PROC	NEAR
; backslash treatment, always return next char (<> '\' always)
BSL0:	XOR	CX,CX		; repeat count:=0;
BSL1:	INC	CX		;    repeat count:=count+1
	LODSB			;       nextchar
	CMP	AL,'\'
	JE	BSL1		;    while char='\' do endrepeat
	CMP	AL,'"'		;	
	JE	BSQ		;    if char<>'"'
	XCHG	AH,AL		;    then -- copy all slashes and return char
	MOV	AL,'\'		;         repeat while count>0 do count:=count-1
	REP	STOSB		;            store('\') endrepeat
	XCHG	AH,AL		;         bslash:=char; RETURN
	RET			;    endif
BSQ:				;    -- quote follows, copy half of slashes
	MOV	AL,'\'		;
	SHR	CX,1		;    repeat while count>1
	REP	STOSB		;    do store('\'); count:=count-2 endrepeat
	MOV	AL,'"'
	JC	BSODD		;    if count=0
	RET			;    then bslash:='"'; RETURN endif
BSODD:	STOSB			;    store('"'); -- odd number of slashes
	LODSB			;    nextchar
	CMP	AL,'\'
	JE	BSL0		; while char='\' do endrepeat
	RET			; bslash:=char
BSLASH	ENDP


ARG_W	PROC	NEAR
	XOR	CX,CX			; CX flags result
	OR	DH,DH
	JE	ARG_WX
	CMP	BYTE PTR DS:[SI],DH	; next = DH??
	JNE	ARG_WX
	MOV	AL,DS:[SI]+1		; yes - sep/term follows??
	GOSPACE	ARGW_F
	GOTERM	ARGW_F
ARG_WX:	RET
ARGW_F:					; yes, set w-flag to one
	MOV	CX,1
	ADD	SI,2			; '-w', reread sep/term
	RET
ARG_W	ENDP


