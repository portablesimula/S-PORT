NAME       SIMDRAW
; ---------------------------------------------------------------
; ---  COPYRIGHT 1992 by                                      ---
; ---  Simula a.s.                                            ---
; ---  Postboks 138 Voksenlia                                 ---
; ---  0325 Oslo 3, Norway                                    ---
; ---                                                         ---
; ---                                                         ---
; ---             P O R T A B L E     S I M U L A             ---
; ---                F O R    i A P X   2 8 6                 ---
; ---                       D O S                             ---
; ---                  Drawing utilities                      ---
; ---                                                         ---
; ---------------------------------------------------------------

ROUTINE    MACRO  RR            ; Standard LIBRARY entry, public routine
           EVEN
RR         PROC   FAR
           PUBLIC RR
           PUSH   BP
           MOV    BP,SP
           ENDM

ENDROUTINE MACRO  RR,NN         ; Standard routine exit
           MOV    SP,BP
           POP    BP
           RET    NN
RR         ENDP
           ENDM
                                
GTOFFS     MACRO  X,Y           ; Compute offset in case 14
           MOV    AX,Y          ; offset = 80*y + x/8
           MOV    CX,SS:RASTINC
           MUL    CX
           MOV    BX,X          ; + x/8
           MOV    CL,SS:DIVBY8
           SHR    BX,CL
           ADD    AX,BX
           ENDM

OUTMASK    MACRO  R,BTM         ; Select or set mask
           MOV    DX,GRAFCTL
           MOV    AL,BTM        ; Select BIT MASK register
           OUT    DX,AL
           INC    DX            ; Set MASK register
           MOV    AL,R
           OUT    DX,AL
           ENDM

PSCHK      MACRO  RG,LB         ; Check legal PS-argument
           CMP    RG,SS:PSLIM
           JG     LB
           OR     RG,RG
           JS     LB
           
           ENDM

PSCHKL     MACRO  RG,LB         ; Check legal PS-argument
           LOCAL  L1,L2
           CMP    RG,SS:PSLIM
           JG     L1
           OR     RG,RG
           JNS    L2
L1:        JMP    LB
L2:
           ENDM

PSCLIP     MACRO  RG            ; ensure legal PS-argument
           LOCAL  L1,L2
           CMP    RG,SS:PSLIM
           JLE    L1
           MOV    RG,SS:PSLIM
L1:        OR     RG,RG
           JNS    L2
           XOR    RG,RG
L2:
           ENDM

LNCHK      MACRO  RG,LB         ; Check legal LN-argument
           CMP    RG,SS:LNLIM
           JG     LB
           OR     RG,RG
           JS     LB
           ENDM

LNCHKL     MACRO  RG,LB         ; Check legal LN-argument
           LOCAL  L1,L2
           CMP    RG,SS:LNLIM
           JG     L1
           OR     RG,RG
           JNS    L2
L1:        JMP    LB
L2:
           ENDM

LNCLIP     MACRO  RG            ; ensure legal LN-argument
           LOCAL  L1,L2
           CMP    RG,SS:LNLIM
           JLE    L1
           MOV    RG,SS:LNLIM
L1:        OR     RG,RG
           JNS    L2
           XOR    RG,RG
L2:
           ENDM

SHR3       MACRO  REG
           SHR    REG,1
           SHR    REG,1
           SHR    REG,1
           ENDM

_DATA        SEGMENT WORD  PUBLIC  'DATA'

; table of 65536 * SIN( v * 6.283185482&-003 ), v=0..250

SINUS      DW         0 ;    0     65536
           DW       412 ;    1     65535
           DW       824 ;    2     65531
           DW      1235 ;    3     65524
           DW      1647 ;    4     65515
           DW      2059 ;    5     65504
           DW      2470 ;    6     65489
           DW      2881 ;    7     65473
           DW      3293 ;    8     65453
           DW      3704 ;    9     65431
           DW      4115 ;   10     65407
           DW      4526 ;   11     65380
           DW      4937 ;   12     65350
           DW      5347 ;   13     65317
           DW      5757 ;   14     65283
           DW      6167 ;   15     65245
           DW      6577 ;   16     65205
           DW      6987 ;   17     65162
           DW      7396 ;   18     65117
           DW      7805 ;   19     65070
           DW      8214 ;   20     65019
           DW      8622 ;   21     64966
           DW      9030 ;   22     64911
           DW      9438 ;   23     64853
           DW      9845 ;   24     64792
           DW     10252 ;   25     64729
           DW     10659 ;   26     64663
           DW     11065 ;   27     64595
           DW     11470 ;   28     64524
           DW     11876 ;   29     64451
           DW     12280 ;   30     64375
           DW     12684 ;   31     64297
           DW     13088 ;   32     64216
           DW     13491 ;   33     64132
           DW     13894 ;   34     64046
           DW     14296 ;   35     63958
           DW     14698 ;   36     63867
           DW     15099 ;   37     63773
           DW     15499 ;   38     63677
           DW     15899 ;   39     63578
           DW     16298 ;   40     63477
           DW     16697 ;   41     63373
           DW     17095 ;   42     63267
           DW     17492 ;   43     63159
           DW     17888 ;   44     63047
           DW     18284 ;   45     62934
           DW     18679 ;   46     62818
           DW     19073 ;   47     62699
           DW     19467 ;   48     62578
           DW     19860 ;   49     62454
           DW     20252 ;   50     62328
           DW     20643 ;   51     62200
           DW     21033 ;   52     62069
           DW     21423 ;   53     61936
           DW     21812 ;   54     61800
           DW     22200 ;   55     61662
           DW     22587 ;   56     61521
           DW     22973 ;   57     61378
           DW     23358 ;   58     61232
           DW     23742 ;   59     61084
           DW     24125 ;   60     60934
           DW     24508 ;   61     60781
           DW     24889 ;   62     60626
           DW     25270 ;   63     60468
           DW     25649 ;   64     60308
           DW     26027 ;   65     60146
           DW     26405 ;   66     59981
           DW     26781 ;   67     59814
           DW     27157 ;   68     59645
           DW     27531 ;   69     59473
           DW     27904 ;   70     59299
           DW     28276 ;   71     59122
           DW     28647 ;   72     58943
           DW     29017 ;   73     58762
           DW     29385 ;   74     58579
           DW     29753 ;   75     58393
           DW     30119 ;   76     58205
           DW     30484 ;   77     58015
           DW     30848 ;   78     57822
           DW     31211 ;   79     57627
           DW     31572 ;   80     57430
           DW     31932 ;   81     57230
           DW     32291 ;   82     57028
           DW     32649 ;   83     56824
           DW     33005 ;   84     56618
           DW     33361 ;   85     56410
           DW     33714 ;   86     56199
           DW     34067 ;   87     55986
           DW     34418 ;   88     55771
           DW     34768 ;   89     55553
           DW     35116 ;   90     55334
           DW     35463 ;   91     55112
           DW     35809 ;   92     54888
           DW     36153 ;   93     54662
           DW     36495 ;   94     54434
           DW     36837 ;   95     54204
           DW     37177 ;   96     53971
           DW     37515 ;   97     53736
           DW     37852 ;   98     53500
           DW     38187 ;   99     53261
           DW     38521 ;  100     53020
           DW     38853 ;  101     52777
           DW     39184 ;  102     52531
           DW     39514 ;  103     52284
           DW     39841 ;  104     52035
           DW     40167 ;  105     51784
           DW     40492 ;  106     51530
           DW     40815 ;  107     51275
           DW     41136 ;  108     51017
           DW     41456 ;  109     50758
           DW     41774 ;  110     50496
           DW     42091 ;  111     50233
           DW     42405 ;  112     49967
           DW     42719 ;  113     49700
           DW     43030 ;  114     49431
           DW     43340 ;  115     49159
           DW     43648 ;  116     48886
           DW     43954 ;  117     48611
           DW     44259 ;  118     48334
           DW     44561 ;  119     48055
           DW     44862 ;  120     47774
           DW     45162 ;  121     47491
           DW     45459 ;  122     47206
           DW     45755 ;  123     46920
           DW     46049 ;  124     46631
           DW     46341 ;  125     46341
           DW     46631 ;  126     46049
           DW     46920 ;  127     45755
           DW     47206 ;  128     45459
           DW     47491 ;  129     45162
           DW     47774 ;  130     44862
           DW     48055 ;  131     44561
           DW     48334 ;  132     44259
           DW     48611 ;  133     43954
           DW     48886 ;  134     43648
           DW     49159 ;  135     43340
           DW     49431 ;  136     43030
           DW     49700 ;  137     42719
           DW     49967 ;  138     42405
           DW     50233 ;  139     42091
           DW     50496 ;  140     41774
           DW     50758 ;  141     41456
           DW     51017 ;  142     41136
           DW     51275 ;  143     40815
           DW     51530 ;  144     40492
           DW     51784 ;  145     40167
           DW     52035 ;  146     39841
           DW     52284 ;  147     39514
           DW     52531 ;  148     39184
           DW     52777 ;  149     38853
           DW     53020 ;  150     38521
           DW     53261 ;  151     38187
           DW     53500 ;  152     37852
           DW     53736 ;  153     37515
           DW     53971 ;  154     37177
           DW     54204 ;  155     36837
           DW     54434 ;  156     36495
           DW     54662 ;  157     36153
           DW     54888 ;  158     35808
           DW     55112 ;  159     35463
           DW     55334 ;  160     35116
           DW     55553 ;  161     34768
           DW     55771 ;  162     34418
           DW     55986 ;  163     34067
           DW     56199 ;  164     33714
           DW     56410 ;  165     33361
           DW     56618 ;  166     33005
           DW     56824 ;  167     32649
           DW     57028 ;  168     32291
           DW     57230 ;  169     31932
           DW     57430 ;  170     31572
           DW     57627 ;  171     31211
           DW     57822 ;  172     30848
           DW     58015 ;  173     30484
           DW     58205 ;  174     30119
           DW     58393 ;  175     29753
           DW     58579 ;  176     29385
           DW     58762 ;  177     29017
           DW     58943 ;  178     28647
           DW     59122 ;  179     28276
           DW     59299 ;  180     27904
           DW     59473 ;  181     27531
           DW     59645 ;  182     27157
           DW     59814 ;  183     26781
           DW     59981 ;  184     26405
           DW     60146 ;  185     26027
           DW     60308 ;  186     25649
           DW     60468 ;  187     25270
           DW     60626 ;  188     24889
           DW     60781 ;  189     24508
           DW     60934 ;  190     24125
           DW     61084 ;  191     23742
           DW     61232 ;  192     23358
           DW     61378 ;  193     22973
           DW     61521 ;  194     22587
           DW     61662 ;  195     22200
           DW     61800 ;  196     21812
           DW     61936 ;  197     21423
           DW     62069 ;  198     21033
           DW     62200 ;  199     20643
           DW     62328 ;  200     20252
           DW     62454 ;  201     19860
           DW     62578 ;  202     19467
           DW     62699 ;  203     19073
           DW     62818 ;  204     18679
           DW     62934 ;  205     18284
           DW     63047 ;  206     17888
           DW     63159 ;  207     17492
           DW     63267 ;  208     17095
           DW     63373 ;  209     16697
           DW     63477 ;  210     16298
           DW     63578 ;  211     15899
           DW     63677 ;  212     15499
           DW     63773 ;  213     15099
           DW     63867 ;  214     14698
           DW     63958 ;  215     14296
           DW     64046 ;  216     13894
           DW     64132 ;  217     13491
           DW     64216 ;  218     13088
           DW     64297 ;  219     12684
           DW     64375 ;  220     12280
           DW     64451 ;  221     11875
           DW     64524 ;  222     11470
           DW     64595 ;  223     11065
           DW     64663 ;  224     10659
           DW     64729 ;  225     10252
           DW     64792 ;  226      9845
           DW     64853 ;  227      9438
           DW     64911 ;  228      9030
           DW     64966 ;  229      8622
           DW     65019 ;  230      8214
           DW     65070 ;  231      7805
           DW     65117 ;  232      7396
           DW     65162 ;  233      6987
           DW     65205 ;  234      6577
           DW     65245 ;  235      6167
           DW     65283 ;  236      5757
           DW     65317 ;  237      5347
           DW     65350 ;  238      4937
           DW     65380 ;  239      4526
           DW     65407 ;  240      4115
           DW     65431 ;  241      3704
           DW     65453 ;  242      3293
           DW     65473 ;  243      2881
           DW     65489 ;  244      2470
           DW     65504 ;  245      2059
           DW     65515 ;  246      1647
           DW     65524 ;  247      1235
           DW     65531 ;  248       824
           DW     65535 ;  249       412
;          DW     65536 ;  250         0

STR        DQ 0                 ; String for OUTST
           DQ 0
           DQ 0
           DQ 0
           DQ 0
           DQ 0
           DQ 0
           DQ 0
           DQ 0
           DQ 80
PSLIM      DW 319
LNLIM      DW 199
MEMB       DW 0A000H            ; Segment of display buffer
RASTINC    DW 80                ; Raster increment 
COLR       DB 1
GRM        DB 1
DIVBY8     DB 3
GRAFCTL    EQU 3CEH             ; Address of graphics controller
SEQCTL     EQU 3C4H             ; Address of sequencer
_DATA        ENDS

STACK        SEGMENT PARA STACK 'STACK'
STACK        ENDS

DGROUP       GROUP   _DATA,STACK
             ASSUME  SS:DGROUP

MS_TEXT SEGMENT WORD  PUBLIC  'FAR_CODE'
           ASSUME  CS:MS_TEXT
           ASSUME  DS:DGROUP

; external library procedure INBT is character procedure INBT; ;
; console input, no echo, no Ctrl-C, WAIT ;
   ROUTINE MS@INBT
           XOR    AX,AX
           MOV    AH,7          ; Direct console input.
           INT    21H
           MOV    [BP]+6,AX    ; Store result as character.
   ENDROUTINE MS@INBT,0

; external library procedure CINBT is character procedure CINBT; ;
; console input, no echo, no Ctrl-C, noWAIT (NUL signals no char) ;
   ROUTINE MS@CINBT
           XOR    AX,AX
           MOV    AH,0BH        ; Direct console input.
           INT    21H
           OR     AL,AL
           JNZ    CHROK
           XOR    AX,AX         ; Result zero, No char available
           JMP    SHORT RTRN
CHROK:     XOR    AX,AX
           MOV    AH,7          ; Direct console input.
           INT    21H
           XOR    AH,AH
RTRN:      MOV    [BP]+6,AX    ; Store result as character.
   ENDROUTINE MS@CINBT,0

; external library procedure OUTBT is procedure OUBT(ch); character ch; ;
; output to std output ;
   ROUTINE MS@OUTBT
           MOV    DL,[BP]+6     ; Get import parameter.
           XOR    AX,AX
           MOV    AH,2          ; Display character in DL.
           INT    21H
   ENDROUTINE MS@OUTBT,2

; external library procedure OUTST is procedure OUTST(str); text str; ;
; output to std output ;
   ROUTINE MS@OUTST                     ; Outstring(str);
           MOV    AX,[BP]+14
           SUB    AX,[BP]+12
           MOV    CX,AX                 ; Lengde iorden.
           MOV    SI,WORD PTR [BP]+12   ; Start peker
           LDS    BX,DWORD PTR [BP]+6   ; DS:seg, BX:offs
           LEA    SI,[BX+SI]            ; Samle start og addr
           ADD    SI,10
           PUSH   SS
           POP    ES
           ASSUME ES:DGROUP
           MOV    DI,OFFSET STR ; Start STR
           MOV    BX,CX
           ADD    BX,DI
           MOV    BYTE PTR ES:[BX],'$'
           CLD
           REP    MOVSB
  ;     DS:DX : String to be displayed, must end with $
           MOV    SI,SEG _DATA
           MOV    DS,SI
           ASSUME DS:_DATA
           MOV    DX,OFFSET STR
           MOV    AH,9
           INT    21H
           ASSUME DS:NOTHING
   ENDROUTINE MS@OUTST,10

; external library procedure STPOS is procedure STPOS(Ln,Ps); char Ln,Ps; ;
; moves cursor to Ln,Ps - may also be decl. short integer 
   ROUTINE MS@STPOS             ; Setpos(Line, Pos);
           MOV    DL,BYTE PTR [BP]+6    ; Line number
           MOV    DH,BYTE PTR [BP]+8    ; Position
           XOR    BH,BH
           MOV    AH,2          ; Set Cursor Position in dl,dh
           INT    10H
   ENDROUTINE MS@STPOS,4

; external library procedure STCOL is procedure STCOL(cl); char cl; ;
; set colour asttribute - may also be decl. short int;
   ROUTINE MS@STCOL             ; Setcolor(cl), save for later;
           MOV    DX,[BP]+6     ; Legal value checked externally
           MOV    SS:COLR,DL       ; Set Colour as specified
   ENDROUTINE MS@STCOL,2

; external library procedure CLS is procedure CLS; ;
; blank screen, set normal vid.attr., set cursor at top;
   ROUTINE MS@CLS               ; Clear Screen;
;          MOV    DL,80         ; DX lower right corner
;          MOV    DH,24
           XOR    DX,DX
           XOR    CX,CX         ; CX upper left corner
           XOR    AX,AX
           MOV    AH,6          ; Initialize entire window
           MOV    BH,7          ; Normal video attribute
           INT    10H
           XOR    DX,DX
           XOR    BH,BH
           MOV    AH,2          ; Set Cursor Position in dl,dh
           INT    10H
           MOV    AX,3
           INT    10H
   ENDROUTINE MS@CLS,0

; external library procedure SETVDM is
; procedure SETVDM(mode,lnlim,pslim,Div,VidMem); short int ALL; ;
   ROUTINE MS@SETVDM  ; -- Set video(mode,lnlim,pslim,Div,VidMem);
                      ; This is entirely the user's responsibility!!!
           MOV    SI,SEG _DATA
           MOV    DS,SI
           ASSUME DS:_DATA
           MOV    AX,[BP]+14    ; Graphic mode
           XOR    AH,AH         ; mod 256
           MOV    GRM,AL
           CMP    AL,127        ; if > 127
           JNA    VDMOK
           SUB    AL,128
VDMOK:     MOV    BL,[BP]+8
           MOV    DIVBY8,BL     ; Set Right shift control
           MOV    BX,[BP]+6     ; Base segment address
           MOV    MEMB,BX
           MOV    BX,[BP]+10    ; Pslim
           MOV    PSLIM,BX
           INC    BX
           MOV    CL,DIVBY8
           SHR    BX,CL
           MOV    RASTINC,BX      
           MOV    BX,[BP]+12    ; Lnlim
           MOV    LNLIM,BX
           MOV    AL,GRM
           INT    10H
           ASSUME DS:NOTHING
   ENDROUTINE MS@SETVDM,10

; external library procedure INITGRM is INITGRM(grm); short int grm; ;
; set graphic mode
   ROUTINE MS@INITGR  ; -- Set Graph mode(mode);
           MOV    SI,SEG _DATA
           MOV    DS,SI
           ASSUME DS:_DATA
           MOV    AX,199
           MOV    LNLIM,AX
           MOV    AX,319
           MOV    PSLIM,AX
           MOV    AX,[BP]+6     ; Graphic mode
           XOR    AH,AH
           MOV    [BP]+6,AX     ; modulo 256
           CMP    AL,127        ; if > 127
           JNA    GRMOK
           SUB    AL,128
GRMOK:     MOV    GRM,AL
           MOV    BL,2
           CMP    AL,6
           JL     SHORT DIV8OK
           MOV    BL,3
           CMP    AL,19
           JNE    SHORT DIV8OK
           XOR    BL,BL
DIV8OK:    MOV    DIVBY8,BL     ; Set Right shift control
           MOV    BX,0B800H     ; Base segment address (CGA)
           CMP    AL,7
           JL     SHORT MEMBOK
           MOV    BX,0A000H     ; Base segment address (EGA/VGA)
MEMBOK:    MOV    MEMB,BX
           CMP    AL,6
           JE     INCPS
           CMP    AL,13
           JNG    NOINCLN
           CMP    AL,19
           JE     NOINCLN
INCPS:     MOV    BX,639
           MOV    PSLIM,BX
           CMP    AL,15
           JL     NOINCLN
           MOV    BX,349
           MOV    LNLIM,BX
           CMP    AL,17
           JL     NOINCLN
           MOV    BX,399
           MOV    LNLIM,BX
           CMP    AL,94
           JE     NOINCLN
           MOV    BX,479
           MOV    LNLIM,BX
           CMP    AL,19
           JL     NOINCLN
           MOV    BX,599
           MOV    LNLIM,BX
           MOV    BX,799
           MOV    PSLIM,BX
NOINCLN:   MOV    AX,PSLIM
           INC    AX
           MOV    CL,DIVBY8
           SHR    AX,CL
           MOV    RASTINC,AX
           MOV    AX,[BP]+6     ; Graphic mode
           INT    10H
           ASSUME DS:NOTHING
   ENDROUTINE MS@INITGR,2

; external library procedure DRPT is procedure DRPT(Ln,Ps); short int Ln,Ps; ;
; plot pixel w/colour
   ROUTINE MS@DRPT              ; Draw Point(Ln,Ps) with COLR
           MOV    BX,[BP]+6     ; X-coordinate
           PSCHK  BX,NOPTE
           MOV    AX,[BP]+8     ; Y-coordinate (line number)
           LNCHK  AX,NOPTE
           MOV    CX,SS:RASTINC    ; Fetch bytes per raster
           MUL    CX            ; Compute offset past y raster
           MOV    CH,BL         ; Keep copy of X for later
           MOV    CL,SS:DIVBY8
           SHR    BX,CL
           ADD    BX,AX         ; Add offset together and keep in BX
           MOV    AX,SS:MEMB       ; Fetch segment and copy it into ES
           MOV    ES,AX
           MOV    CL,CH
           AND    CL,7          ; Compute which bit in a byte
           MOV    AL,80H        ; and use it to rotate mask into position
           ROR    AL,CL
           MOV    CL,AL         ; Keep mask in CL
           CALL   SETRES
           OUTMASK CL,08H
           MOV    AL,ES:[BX]    ; Must latch to preserve old bits
           MOV    ES:[BX],AL    ; Set pixel color to new value
           OUTMASK 00H,01H      ; select ENABLE RESET
           OUTMASK 0FFH,08H     ; Select BIT MASK register, enable 8 bits
NOPTE:
   ENDROUTINE MS@DRPT,4

; external library procedure
;    ROUTINE MS@DRPTVGA           ; Draw Point(Ln,Ps) with COLR
;            MOV    BX,[BP]+6     ; X-coordinate
;            PSCHK  BX,NOPTEV
;            MOV    AX,[BP]+8     ; Y-coordinate (line number)
;            LNCHK  AX,NOPTEV
;            MOV    CX,SS:RASTINC    ; Fetch bytes per raster
;            MUL    CX            ; Compute offset past y raster
;            MOV    CL,SS:DIVBY8
;            SHR    BX,CL
;            ADD    BX,AX         ; Add offset together and keep in BX
;            MOV    AX,SS:MEMB       ; Fetch segment and copy it into ES
;            MOV    ES,AX
;            MOV    CL,0FFH        ; All bits
;            CALL   SETRES
;            OUTMASK CL,08H
;            MOV    AL,ES:[BX]    ; Must latch to preserve old bits
;            MOV    ES:[BX],AL    ; Set pixel color to new value
;            OUTMASK 00H,01H      ; select ENABLE RESET
;            OUTMASK 0FFH,08H     ; Select BIT MASK register, enable 8 bits
; NOPTEV:
;    ENDROUTINE MS@DRPTVGA,4

; external library procedure
   ROUTINE MS@DRPTCGA           ; Draw Point(Ln,Ps);
           MOV    DX,[BP]+8     ; Line number
           LNCHK  DX,NOPT       ; Check legal line number
           MOV    CX,[BP]+6     ; Start position
           PSCHK  CX,NOPT       ; Check legal position
           MOV    AL,SS:COLR       ; Pixel value
           MOV    AH,12         ; Write graphics pixel
           XOR    BX,BX         ;   into page 0
           INT    10H
NOPT:
   ENDROUTINE MS@DRPTCGA,4

; external library procedure
   ROUTINE MS@DRP4CGA           ; Draw 4 Points(Ln,Ps,rl,rp);
           MOV    AL,SS:COLR       ; Pixel value
           MOV    AH,12         ; Write graphics pixel
           XOR    BX,BX         ;   into page 0
           MOV    DX,[BP]+12    ; Line number
           MOV    SI,[BP]+8     ; line relative
           ADD    DX,SI         ; + line relative
           MOV    DI,[BP]+6     ; pos relative
           MOV    CX,[BP]+10    ; Start position
           LNCHK  DX,NOTL1      ; if not legal line, skip two points
           ADD    CX,DI
           PSCHK  CX,NOT1       ; if not legal pos, skip point
           INT    10H           ; write (ln+rl,ps+ph);
NOT1:      SUB    CX,DI
           SUB    CX,DI
           PSCHK  CX,NOT2       ; if not legal pos, skip point
           INT    10H           ; write (ln+rl,ps-ph);
NOT2:      ADD    CX,DI         ; - back to ps
NOTL1:     SUB    DX,SI         ; Line number
           SUB    DX,SI         ; - line relative
           LNCHK  DX,NOT4       ; if not legal line, skip two points
           ADD    CX,DI
           PSCHK  CX,NOT3       ; if not legal pos, skip point
           INT    10H           ; write (ln-rl,ps+ph);
NOT3:      SUB    CX,DI         ; Pos
           SUB    CX,DI         ; - pos relative
           PSCHK  CX,NOT4       ; if not legal pos, skip point
           INT    10H           ; write (ln-rl,ps-ph);
NOT4:
   ENDROUTINE MS@DRP4CGA,8

; external library procedure
   ROUTINE MS@DRBARCGA          ; Draw Hor.Line(Ln,Ps,Rp);
           MOV    DX,[BP]+10    ; Line number
           LNCHK  DX,BOPT       ; Check legal line number once
           MOV    CX,[BP]+8     ; Start position
           MOV    DI,CX
           PSCLIP DI            ; ensure legal position start
           ADD    CX,[BP]+6     
           PSCLIP CX            ; ensure legal position finish
           MOV    AL,SS:COLR       ;   Pixel value
           MOV    AH,12         ;   Write graphics pixel
           XOR    BX,BX         ;   into page 0
           CMP    CX,DI
           JE     BOPT
           JG     BARNXT
           XCHG   CX,DI
BARNXT:                         ; repeat write pixel
           INT    10H
           CMP    CX,DI         ; while not start pixel
           LOOPNE BARNXT        ;   count down incr
           OR     DI,DI         ; treat: start=0
           JNZ    BOPT
           INT    10H
BOPT:
   ENDROUTINE MS@DRBARCGA,6


; **************   read pixels etc  (picture file support)  *************

; *** read scan line into txt, from (Ln,Ps), rP+1 pixels ***
; external PASCAL procedure scanBar = "MS@RDBAR" is
; short integer procedure scanBar(buf,Ln,Ps,rP);
;               name buf; value Ln,Ps,rP;
;               text buf; short integer Ln,Ps,rP; ;
; RESULT:  <2 byte rp =pixel count-1><(rp+2)//2 bytes>

; NB! NB!  BIOS entry (int.10H,13) doesn't work, and too slow anyway !!!!!!!!
;          ABSOLUTELY NO CHECKING ON PARAMETERS

; use DS:SI for display mem and ES:DI for dest
   ROUTINE MS@RDBAR
           SUB    SP,8          ; 4 locals:
                                ;   BP-2: last point
                                ;   BP-4: part byte flag (packing)
                                ;   BP-6: raster offset
                                ;   BP-8: next point (x coord.)
           LES    DI,[BP]+12
           ASSUME ES:NOTHING
           MOV    AX,[BP]+10    ; Ln
           MOV    CX,SS:RASTINC ; Fetch bytes per raster
           MUL    CX            ; Compute offset past y raster
           ADD    DX,SS:MEMB    ; Fetch segment and copy it into DS
           MOV    DS,DX         ; - taking care if segment overrun
           ASSUME DS:NOTHING
           MOV    [BP-6] ,AX    ; save offset
           MOV    BX,[BP]+8     ; Ps
           MOV    DX,[BP]+6     ;    + rP:
           MOV    ES:[DI],DX
           INC    DI
           INC    DI
           ADD    DX,BX
           MOV    [BP-2],DX     ; BP-2: last point
           MOV    BYTE PTR [BP-4],1      ; BP-4<0: part byte empty
           MOV    DX,GRAFCTL    ; DX holds controller addr thruout
           CLD    ; TO BE SURE !!!
           MOV    AL,4
           OUT    DX,AL
           INC    DX
           DEC    BX
RDNEXT:
           INC    BX
           MOV    [BP-8],BX     ; save Ps for next time around
           MOV    CH,BL
           MOV    CL,SS:DIVBY8
           SHR    BX,CL
           ADD    BX,[BP-6]     ; Add offset together and keep in BX
           MOV    SI,BX
           MOV    CL,CH
           AND    CL,7          ; Compute which bit in a byte
           MOV    BH,80H        ; and use it to rotate mask into position
           ROR    BH,CL
           XOR    BL,BL
           MOV    CX,4
           MOV    AL,CL
RDNP:      SHL    BL,1
           DEC    AL
           OUT    DX,AL
           MOV    AH,[SI]
           AND    AH,BH
           JZ     RDZR
           OR     BL,1
RDZR:      LOOP   RDNP
           XCHG   AX,BX             ; for STOSB
           NEG    BYTE PTR [BP-4]
           JS     RDPACK
           OR     AL,ES:[DI]
           STOSB
           MOV    BX,[BP-8]
           CMP    BX,[BP-2]
           JB     RDNEXT
           JMP    SHORT RDFIN
RDPACK:    SHL    AL,1
           SHL    AL,1
           SHL    AL,1
           SHL    AL,1
           STOSB
           DEC    DI
           MOV    BX,[BP-8]
           CMP    BX,[BP-2]
           JB     RDNEXT
           INC    DI
RDFIN:  
           CALL   ENA8W         ; Enable all 8bits in a byte for write
           MOV    AX,DI
           SUB    AX,[BP]+12       ; result (pascal: in AX) = length
   ENDROUTINE MS@RDBAR,10


;  *** write scan line from txt, from (Ln,Ps) and length pixels
;  external PASCAL procedure paintBar="MS@PAINTBAR" is
;  short integer procedure paintBar(txt,Ln,Ps); name txt; value Ln,Lp;
;                text txt; short integer Ln,Lp; ;
;  uses BIOS entry to save space - but a bit slow !!!

   ROUTINE MS@PAINTBAR          ; paint(txt)
           MOV    DI,1          ; <>0 - error flag
           LDS    SI,[BP]+10
           ASSUME DS:NOTHING
           MOV    DX,[BP]+8     ; Ln
           LNCHK  DX,PAINTX
           MOV    CX,[BP]+6     ; Ps
           MOV    AX,[SI]       ; rp
           ADD    AX,CX         ; Ps + rP
           CMP    AX,CX
           JG     PPOK
           XCHG   AX,CX
PPOK: ; now AX (first point) >= CX (last point
           PSCHK  CX,PAINTX     ; start point must be within screen
           OR     CX,CX
           JS     PAINTX
           MOV    DI,SS:PSLIM
           CMP    AX,DI
           JLE    PLOK
           MOV    AX,DI
           CMP    AX,CX
           JL     PAINTX
PLOK:
           MOV    [BP]+8,AX    ; save last point
           MOV    DI,AX
           CMP    CX,DI
           JAE    PAINTLAST
           XOR    AX,CX
           RCR    AX,1
           JC     EVENDIST
           DEC    DI
EVENDIST:
           MOV    AH,12         ; write
           XOR    BX,BX
           CLD    ; TO BE SURE !!!
           INC    SI
           INC    SI
PAINTNXT:
           LODSB
           MOV    BL,AL
           SHR    AL,1
           SHR    AL,1
           SHR    AL,1
           SHR    AL,1
           INT    10H
        ;  CMP    CX,DI        ; no need since dist is even
        ;  JAE    POK
           XCHG   AL,BL
           INC    CX
           AND    AL,0FH
           INT    10H
           INC    CX
           CMP    CX,DI
           JBE    PAINTNXT
PAINTLAST:
           CMP    [BP]+8,DI    ; last point painted?
           JE     POK
           LODSB               ; no, do it
           SHR    AL,1
           SHR    AL,1
           SHR    AL,1
           SHR    AL,1
           INT    10H
POK:
           XOR    DI,DI            ; result OK
PAINTX:
           MOV    AX,DI            ; PASCAL - result in AX!!!
   ENDROUTINE MS@PAINTBAR,8


; external library procedure
   ROUTINE MS@DRBOXCGA          ; FillSquare(Ln,Ps,Rl,Rp);
     ; will draw if some (L,P) is on screen (rl,rp positive, checked ext.)
           MOV    DX,[BP]+12    ; Line number Ln
           MOV    DI,DX
           LNCLIP DI
           ADD    DX,[BP]+8
           LNCLIP DX
           CMP    DX,DI
           JG     LNBOXOK
           JE     BOXT          ; degenerate squares not drawn
           XCHG   DI,DX
LNBOXOK:   MOV    AL,SS:COLR       ;   Pixel value
           MOV    AH,12         ;   Write graphics pixel function
           XOR    BX,BX         ;   into page 0
           MOV    CX,[BP]+10    ; Start position Ps
           MOV    SI,CX
           PSCLIP SI
           ADD    CX,[BP]+6     
           PSCLIP CX
           CMP    CX,SI
           JG     BOXSET
           JE     BOXT          ; degenerate squares not drawn
           XCHG   CX,SI
BOXSET:    MOV    [BP]+6,CX     ; save start pos
; now   DX > DI >= 0,  CX > SI >= 0, write first line
BOXLIN1:   INT    10H
           CMP    CX,SI
           LOOPNE BOXLIN1
           OR     SI,SI         ; treat: start=0
           JNZ    BOXNX1
           INT    10H
BOXNX1:    DEC    DX            ; now  DX >= DI, not test here
BOXNXT:    MOV    CX,[BP]+6     ; - start pos of current line
BOXBAR:                         ; repeat write pixel
           INT    10H
           CMP    CX,SI         ; while not start pixel
           LOOPNE BOXBAR        ; do count down pos
           OR     SI,SI         ; treat: start=0
           JNZ    BOXB2
           INT    10H
BOXB2:     DEC    DX
           CMP    DX,DI         ; DX>=DI>=0
           JGE    BOXNXT
BOXT:
   ENDROUTINE MS@DRBOXCGA,8

           EVEN
ENA4       PROC   NEAR
           MOV    DX,SEQCTL     ; enable all four planes for writing
           MOV    AL,2
           OUT    DX,AL
           INC    DX
           MOV    AL,0FH
           OUT    DX,AL
           RET
ENA4       ENDP

; external library procedure
   ROUTINE MS@DRGLN             ; Draw Gen Line(x0,y0,x1,y1)
; note: if any point is outside then no draw (i.e. no clipping)
           SUB    SP,6          ; Three local variables
           MOV    AX,[BP]+12    ; Check x1 >= x0
           PSCHKL AX,NOPT0
           MOV    CX,[BP]+8     ; x1
           PSCHKL CX,NOPT0
 ; ???     CMP    CX,AX
 ; ???     JGE    GETOFFS
           MOV    BX,[BP]+10    ; y0
           LNCHKL BX,NOPT0
           MOV    DX,[BP]+6     ; y1
           LNCHKL DX,NOPT0
           CMP    CX,AX
           JG     GETOFFS
           JNE    GLOK
           CMP    DX,BX
           JNE    GLOK
           JMP    NOPT0         ; after clipping (x0,y0)=(x1,y1): no line
GLOK:      MOV    [BP]+12,CX    ; x0
           MOV    [BP]+10,DX    ; y0
           MOV    [BP]+8,AX     ; x1
           MOV    [BP]+6,BX     ; y1
GETOFFS:   GTOFFS [BP]+12,[BP]+10 ; Compute offset and save stack
           PUSH   AX            ; save offset on stack, later pop to DI
           MOV    CX,[BP]+12    ; compute which bit (x mod 8) to modify
           AND    CL,7
           MOV    BX,80H
           SHR    BX,CL
           PUSH   BX            ; save mask on stack, later pop to SI
           OUTMASK BL,08H       ; enable only the bit (within a byte)
           CALL   SETRES        ; move color into reset register
           CALL   ENA4
           MOV    DX,SS:MEMB       ; Load segment register
           MOV    DS,DX
           MOV    ES,DX
           MOV    AX,SS:RASTINC    ; set raster increment
           MOV    [BP-6],AX
           MOV    SI,[BP]+8     ; compute dx, reg SI
           SUB    SI,[BP]+12
           MOV    DI,[BP]+6     ; compute dy, reg DI
           SUB    DI,[BP]+10
           JGE    DYISPOS
           NEG    AX
           MOV    [BP-6],AX
           NEG    DI
DYISPOS:   CMP    SI,0
           JZ     VERT
           CMP    DI,0
           JZ     HORIZ
           JMP    DIAG
VERT:      MOV    CX,DI         ; set up counter
           INC    CX
           MOV    BX,[BP-6]
           POP    SI            ; fetch mask
           MOV    DX,GRAFCTL    ; set mask
           MOV    AL,08H
           OUT    DX,AL
           INC    DX
           MOV    AX,SI
           OUT    DX,AL
           POP    DI            ; fetch offset
LOOPV:     MOV    AL,[DI]       ; latch data (to preserve other 7 bits)
           MOV    [DI],AL       ; write new data, only one bit modified
           ADD    DI,BX
           LOOP   LOOPV
           JMP    LINEDN
HORIZ:     MOV    CX,SI         ; set counter of pixels
           POP    SI            ; fetch mask
           POP    DI            ; fetch offset
           MOV    AX,[BP]+12    ; draw pixels from leading partial byte
           AND    AX,7          ; check for partial byte
           JZ     FULLB
           MOV    BX,0FFH       ; compute the mask
           PUSH   CX
           MOV    CX,AX
           SHR    BX,CL
           POP    CX
           ADD    CX,AX         ; update counter
           SUB    CX,08H
           JGE    MASKSET       ; modify mask if only one byte
           NEG    CX
           SHR    BX,CL
           SHL    BX,CL
           XOR    CX,CX         ; restore counter
MASKSET:   OUTMASK BL,08H       ; set the mask
           MOV    AH,[DI]       ; latch data
           MOV    [DI],AL       ; write new data
           INC    DI
FULLB:     MOV    BX,CX         ; check if any bytes to be set
           CMP    BX,8                  ; !!!!!
           JL     TRAILB
           SHR3   CX
           OUTMASK 0FFH,08H     ; set the mask
           CLD
           REP    STOSB         ; fill complete bytes
TRAILB:    AND    BX,7
           JZ     HORIZD
           MOV    AX,0FFFFH     ; compute mask
           MOV    CX,BX
           SHR    AX,CL
           XOR    AH,0FFH       ; set the mask
           OUTMASK AH,08H
           MOV    AL,[DI]       ; latch data
           MOV    [DI],AL       ; set new data
HORIZD:    JMP    SHORT LINEDN
DIAG:      CMP    SI,DI         ; is dy > dx
           JLE    OCT12         ; yes, do processing in octants 1 and 2
OCT03:     MOV    CX,SI         ; set counter to dx+1
           INC    CX
           SAL    DI,1          ; d1 = dy*2
           MOV    BX,DI         ; d = dy*2 - dx in reg-BX
           SUB    BX,SI
           NEG    SI            ; d2 = dy*2-dx-dx in reg SI
           ADD    SI,BX
           MOV    [BP-2],DI     ; save d1
           MOV    [BP-4],SI     ; save d2
           MOV    DX,GRAFCTL    ; select bit mask register
           MOV    AL,08H
           OUT    DX,AL
           INC    DX
           POP    AX            ; fetch mask
           POP    DI            ; fetch address
NEXT0:     OUT    DX,AL         ; enable a bit in a byte
           MOV    AH,[DI]       ; latch old data
           MOV    [DI],AH       ; modify (enable bits)
           ROR    AL,1          ; update mask
           ADC    DI,0          ; update byte address
           TEST   BX,8000H      ; if d >= 0 then ...
           JNZ    DNEG0
           ADD    BX,[BP-4]     ; ... d = d+d2
           ADD    DI,[BP-6]     ; update offset to next scan line
           LOOP   NEXT0
           JMP    SHORT LINEDN
DNEG0:     ADD    BX,[BP-2]     ; if d < 0 then d = d+d1
           LOOP   NEXT0
           JMP    SHORT LINEDN
OCT12:     MOV    CX,DI         ; set counter to dy+1
           INC    CX
           SAL    SI,1          ; d1 = dx*2
           MOV    BX,SI         ; d = dx*2 - dy
           SUB    BX,DI
           NEG    DI            ; d2 = dx*2 - dy - dy
           ADD    DI,BX
           MOV    [BP-4],DI     ; save d2
           MOV    [BP-2],SI     ; save d1
           MOV    DX,GRAFCTL    ; select BIT MASK register
           MOV    AL,08H
           OUT    DX,AL
           INC    DX
           POP    AX            ; fetch mask
           POP    DI            ; fetch address
           OUT    DX,AL         ; enable a bit in a byte
NEXT1:     MOV    AH,[DI]       ; latch old data
           MOV    [DI],AH       ; modify (enable bits)
           ADD    DI,[BP-6]     ; update offset (y = y+1)
           TEST   BX,8000H      ; if d >= 0 then ...
           JNZ    DNEG1
           ADD    BX,[BP-4]     ; ... d = d + d2
           ROR    AL,1          ; ... update mask (x = x+1)
           ADC    DI,0          ; ... update offset
           OUT    DX,AL         ; enable next bit within a byte
           LOOP   NEXT1
           JMP    SHORT LINEDN
DNEG1:     ADD    BX,[BP-2]     ; if d < 0 then d = d+d1
           LOOP   NEXT1
LINEDN:    CALL   ENA8W         ; Enable all 8bits in a byte for write
NOPT0:
   ENDROUTINE MS@DRGLN,8

; external library procedure
   ROUTINE MS@DRBOX             ; -- Draw Filled Rectangle(x0,y0,x1,y1)
           MOV    AX,[BP]+8     ; x1
           MOV    BX,[BP]+12    ; Check x1 >= x0
           CMP    AX,BX
           JG     XFINE
           XCHG   AX,BX
XFINE: ; AX >= BX
           OR     AX,AX
           JS     ERRSTEP
           CMP    BX,LNLIM
           JG     ERRSTEP
           CMP    AX,LNLIM
           JLE    X1OK
           MOV    AX,LNLIM
X1OK:      OR     BX,BX
           JNS    X2OK
           XOR    BX,BX
X2OK:
           MOV    AX,[BP]+6     ; x1
           MOV    BX,[BP]+10    ; Check x1 >= x0
           CMP    AX,BX
           JG     YFINE
           XCHG   AX,BX
YFINE: ; AX >= BX
           OR     AX,AX
           JS     ERRSTEP
           CMP    AX,LNLIM
           JLE    Y1OK
           MOV    AX,LNLIM
Y1OK:      OR     BX,BX
           JNS    Y2OK
           XOR    BX,BX
Y2OK:      CMP    BX,LNLIM
           JLE    YSTOR
ERRSTEP:   JMP    NEAR PTR NOPT1
YSTOR:     MOV    [BP]+6,AX
           MOV    [BP]+10,BX
           GTOFFS [BP]+12,[BP]+10 ; Compute offset and save stack
           MOV    DI,AX         ; save offset in register DI
           CALL   SETRES        ; move color into reset register
           CALL   ENA4
           MOV    DX,SS:MEMB       ; Load segment register
           MOV    DS,DX                                 ; !!! DX,DX !!!
           MOV    ES,DX
           MOV    CX,[BP]+12    ; check for partial leading byte by
           AND    CX,7          ; checking that X0 is not a multiple
           JZ     ENDLEAD       ; of 8
           MOV    AX,[BP]+12    ; update X0 to start in the Middle strip
           SUB    AX,CX
           ADD    AX,8
           MOV    [BP]+12,AX    ; ... and save it for later
           MOV    BX,0FFH
           SHR    BX,CL
           SUB    AX,[BP]+8     ; check if whole rectangle is in this byte
           DEC    AX
           JLE    BITMASK       ; modify mask if it is the only byte
           MOV    CX,AX
           SHR    BX,CL
           SHL    BX,CL
BITMASK:   OUTMASK BL,08H       ; set the mask in the control register
           MOV    CX,[BP]+6     ; Loop to fill the whole strip, set count
           SUB    CX,[BP]+10    ; over rasters
           INC    CX
           MOV    BX,SS:RASTINC    ; byte in next raster is 80 bytes further
           PUSH   DI
LOOPLEAD:  MOV    AH,[DI]       ; latch data, preserve unused bits in byte
           MOV    [DI],AL       ; write new data, mask register does r bits
           ADD    DI,BX         ; address of byte in next raster
           LOOP   LOOPLEAD
           POP    DI
           INC    DI            ; address next to first byte in this strip
ENDLEAD:   MOV    CX,[BP]+8     ; check if any more to do
           SUB    CX,[BP]+12
           JL     ENDTRAIL
           INC    CX
           CMP    CX,8
           JL     ENDFULL
           MOV    BX,[BP]+6     ; set counter of raster
           SUB    BX,[BP]+10
           INC    BX
           SHR3   CX            ; Converts bits fill into bytes to fill
           OUTMASK 0FFH,08H     ; set the mask
           MOV    DX,SS:RASTINC    ; byte in next raster is 80 bytes further
           PUSH   DI
LOOPFULL:  PUSH   CX            ; loop over rasters
           PUSH   DI
           CLD
           REP    STOSB         ; set all full bytes in the same raster
           POP    DI            ; address of first byte in next raster
           ADD    DI,DX
           POP    CX
           DEC    BX
           JG     LOOPFULL
           POP    DI
           ADD    DI,CX
ENDFULL:   MOV    CX,[BP]+8
           INC    CX
           AND    CX,7
           JZ     ENDTRAIL
           MOV    AX,00FFH      ; computes mask
           ROR    AX,CL
           OUTMASK AH,08H       ; select bit mask register
           MOV    CX,[BP]+6     ; set counters
           SUB    CX,[BP]+10
           INC    CX
           MOV    BX,SS:RASTINC
LOOPTRAIL: MOV    AL,[DI]       ; latch data (to preserve other bits)
           MOV    [DI],AL       ; set new data
           ADD    DI,BX
           LOOP   LOOPTRAIL
ENDTRAIL:  CALL   ENA8W         ; Enable all 8bits in a byte for write
NOPT1:
   ENDROUTINE MS@DRBOX,8

; external library procedure
   ROUTINE MS@GETGR   ; -- Get Graph mode;
           MOV    AH,0FH
           INT    10H
           MOV    [BP]+10,AX    ; return (columns,mode)
   ENDROUTINE MS@GETGR,0


; *********** GTPOINT suppl. *********************

REDUCV:    MOV    DI,1000       ; - reduce angle to 0-999
TSTVLO:    OR     BX,BX
           JNS    TSTVHI
           ADD    BX,DI
           JMP    SHORT TSTVLO
TSTVHI:    CMP    BX,DI
           JB     VOK
           SUB    BX,DI
           JMP    TSTVHI
VOK:
           XOR    DI,DI
           CMP    BX,250
           JB     QOK1
           CMP    BX,500
           JAE    INQ34
           INC    DI
           NEG    BX
           ADD    BX,500
           JMP    SHORT QOK1
INQ34:     MOV    DI,2
           CMP    BX,750
           JB     INQ3
           INC    DI
           NEG    BX
           ADD    BX,1000
           JMP    SHORT QOK1
INQ3:      SUB    BX,500
QOK1:      CMP    BX,250
           JE     Q250
           CMP    BX,0
           JNE    QOK
      ; angle=0: cos overflows
           XOR    CX,CX           ; sin:=0
           MOV    AX,[SI]+26      ; cos:=point.radius
           JMP    SHORT MULCOS
Q250: ; angle=250: sine overflows
           XOR    AX,AX           ; cos:=0
           MOV    CX,[SI]+26      ; sin:=point.radius
           JMP    SHORT MULCOS

           JMP    SHORT QOK

; external ASSEMBLY procedure !!!
   ROUTINE MS@GTPOINT ;
;  *** NBNB attribute of class Point in drawing *****;
;  class point; begin
;     short integer cx,cy,r,v,quad,scalex,scaley;
;     external assembly procedure GTPOINT is ... (this)
;  end
;  -- set cx:=r*cos(v*acon)*scaleX/scaleY, cy:=-r*sin(v*acon),
;  --     quad:=0..3 acc. to quadrant
;  -- assume r positive, scaleX/Y positive small values
;  -- optimised for first quadrant
           LDS    SI,[BP]+6     ; DS:SI :- Point
           ASSUME DS:NOTHING
           MOV    BX,[SI]+28    ; BX:=Point.angle
           XOR    DI,DI         ; DI(+1) quadrant
           CMP    BX,250
           JGE    REDUCV
           CMP    BX,0
           JLE    REDUCV     
QOK:                            ; 0 < BX < 250, DI indicates quadrant
           ADD    BX,BX
           MOV    AX,SS:SINUS[BX]
           NEG    BX
           ADD    BX,500        ; cos(v) = sin(250-v)
           MOV    CX,SS:SINUS[BX]
; now AX=sine, CX=cosine - note: AX>0, CX>0
           MOV    BX,[SI]+26      ; BX:=point.radius
           OR     BX,BX
           JNS    RADOK
           NEG    BX
RADOK:     MUL    BX
           ;      DIV    65536  ==  take upper rounded
           RCL    AX,1
           ADC    DX,0
           MOV    AX,CX
           MOV    CX,DX
           MUL    BX
           ;      DIV    65536  ==  take upper rounded
           RCL    AX,1
           ADC    DX,0
           MOV    AX,DX
MULCOS:    MUL    WORD PTR [SI]+32
           DIV    WORD PTR [SI]+34 ; NB - no rounding here!!!!
; now AX=r*cosine(scaled), CX=r*sine - adjust sign acc. to quadrant and result
; quad 1: sine neg, cosine ok          NOTE: result:= -r*sin(...)
; quad 2: sine neg, cosine neg
; quad 3: sine ok,  cosine neg
; quad 4: sine ok,  cosine ok
           CMP    DI,1
           JAE    Q234            ; first quad:  neg sin, cos ok
INVSIN:    NEG    CX              ; fourth quad: neg sin, cos ok
Q4:        MOV    [SI]+24,CX      ; Point.cy:=CX
           MOV    [SI]+22,AX      ; Point.cx:=r*cos(..)*scalex/scaley
           MOV    [SI]+30,DI      ; point.quadrant:=DI
   ENDROUTINE MS@GTPOINT,4        ; pop ref(Point)

Q234:      JA     Q34
           NEG    AX     ; quad 2
           JMP    SHORT INVSIN
Q34:       CMP    DI,2
           JNE    Q4
           NEG    AX     ; quad 3
           JMP    SHORT Q4
;***********  end code of GTPOINT  *************

           EVEN
SETRES     PROC   NEAR
; *********************************************************************
; NOTE: shortcut below - tricky output of 16 bits to two 8 bits ports
;       If in trouble, remove semicolons and the four indented lines!!!
; *********************************************************************
           MOV    DX,GRAFCTL    ; move color into reset register
           XOR    AL,AL
       ;   OUT    DX,AL
       ;   INC    DX
       ;   MOV    AL,SS:COLR
       ;   OUT    DX,AL
       ;   DEC    DX            ; enable use of reset register
            MOV    AH,SS:COLR
            OUT    DX,AX
           MOV    AL,1
       ;   OUT    DX,AL
       ;   INC    DX
       ;   MOV    AL,0FH
       ;   OUT    DX,AL
            MOV    AH,0FH
            OUT    DX,AX
           RET
SETRES     ENDP

           EVEN
ENA8W      PROC   NEAR
           OUTMASK 0FFH,08H     ; Enable all 8bits in a byte for write
           DEC    DX            ; Disable SET/RESET function
           MOV    AL,1
           OUT    DX,AL
           INC    DX
           XOR    AX,AX
           OUT    DX,AL
           RET
ENA8W      ENDP

MS_TEXT ENDS
             END
