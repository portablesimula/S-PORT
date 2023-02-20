NAME       TESTW
INCLUDE  macro.def
;  .LALL

; ************************************************
; ******    L  O  C  A  L     D  A  T  A    ******
; ************************************************
_DATA        SEGMENT
VB DB 1
VW DW 2
VD DD 3
VQ DQ 4
VT DT 5
_DATA        ENDS


; ******************************************************
; ******    C  O  D  E     S  E  C  T  I  O  N    ******
; ******************************************************
_TEXT SEGMENT
;      DD TYPE 44
;      DD TYPE EAX
;      DD TYPE VB
;      DD TYPE VW
;      DD TYPE VD
;      DD TYPE VQ
;      DD TYPE VT
;      DD TYPE ws1
;      DD TYPE wd2

;      DD .TYPE 44
;      DD .TYPE EAX
;      DD .TYPE VB
;      DD .TYPE VW
;      DD .TYPE VD
;      DD .TYPE VQ
;      DD .TYPE VT
;      DD .TYPE ws1
;      DD .TYPE wd2

      WFLD   <ws3>,<ws1>
      WFLD   <wd2>,<wd4>
      WFLD   <ws3>,44
      WFLD   <ws5>,EAX

NOP

      WFLDCTX EAX

NOP

      WFPOP  <ws3>
      WFPOP  <wd6>

NOP

      WFLDSD <ws1>,VW,10

NOP

      WFST   EAX,<ws3>
      WFST   EAX,<ws7>,AND

NOP

      WFSTCTX   EAX
      WFSTCTX   EAX,AND

NOP

      WFPUSH <ws3>
      WFPUSH <wd6>

NOP

      WFSTSD <ws1>,VW,10

NOP

; ========================================================================

      WFLOAT  <ws1>,<ws3>
      WFLOAT  <wd2>,<ws3>
      WFLOAT  <ws3>,123
      WFLOAT  <ws5>,EAX
      WFLOAT  <wd4>,123
      WFLOAT  <wd6>,EAX

NOP

      WFIX    <ws1>,<ws3>
      WFIX    <ws3>,<wd2>
      WFIX    <ws3>,123
      WFIX    <ws5>,EAX

NOP

      WFCVT   <ws3>,<wd2>
      WFCVT   <wd4>,<ws7>
      WFCVT   <ws3>,123
      WFCVT   <ws5>,EAX
      WFCVT   <wd6>,123
      WFCVT   <wd8>,EAX

NOP

; ========================================================================

      WFADD  <ws3>,<ws1>
      WFADD  <wd2>,<wd4>
      WFADD  <ws3>,44
      WFADD  <ws5>,EAX
      WFADD  <wd4>,44
      WFADD  <wd8>,EAX

NOP

      WFSUBR <ws3>,<ws1>
      WFSUBR <wd2>,<wd4>
      WFSUBR <ws3>,44
      WFSUBR <ws5>,EAX
      WFSUBR <wd4>,44
      WFSUBR <wd8>,EAX

NOP

      WFMUL  <ws3>,<ws1>
      WFMUL  <wd2>,<wd4>
      WFMUL  <ws3>,44
      WFMUL  <ws5>,EAX
      WFMUL  <wd4>,44
      WFMUL  <wd8>,EAX

NOP

      WFMULN <ws3>,<ws1>
      WFMULN <wd2>,<wd4>
      WFMULN <ws3>,44
      WFMULN <ws5>,EAX
      WFMULN <wd4>,44
      WFMULN <wd8>,EAX

NOP

      WFAMUL <ws3>,<ws1>
      WFAMUL <wd2>,<wd4>
      WFAMUL <ws3>,44
      WFAMUL <ws5>,EAX
      WFAMUL <wd4>,44
      WFAMUL <wd8>,EAX

NOP

      WFMAC  <ws3>,<ws1>
      WFMAC  <ws3>,44
      WFMAC  <ws5>,EAX

NOP

      WFMACD <ws3>,<ws1>
      WFMACD <ws3>,44
      WFMACD <ws5>,EAX

NOP

      WFDIVR <ws3>,<ws1>
      WFDIVR <wd2>,<wd4>
      WFDIVR <ws3>,44
      WFDIVR <ws5>,EAX
      WFDIVR <wd4>,44
      WFDIVR <wd8>,EAX

NOP

      WFCMPR <ws3>,<ws1>
      WFCMPR <wd2>,<wd4>
      WFCMPR <ws3>,44
      WFCMPR <ws5>,EAX
      WFCMPR <wd4>,44
      WFCMPR <wd8>,EAX

NOP

      WFCMPRT <ws3>,<ws1>
      WFCMPRT <wd2>,<wd4>
      WFCMPRT <ws3>,44
      WFCMPRT <ws5>,EAX
      WFCMPRT <wd4>,44
      WFCMPRT <wd8>,EAX

NOP

      WFTST  <ws3>
      WFTST  <wd2>
      WFTST  44
      WFTST  EAX
      WFTST  44,<ws1>
      WFTST  EAX,<ws1>

NOP

      WFTSTT <ws3>
      WFTSTT <wd2>
      WFTSTT 44
      WFTSTT EAX
      WFTSTT 44,<ws1>
      WFTSTT EAX,<ws1>

NOP

      WFNEG  <ws3>,<ws1>
      WFNEG  <wd2>,<wd4>
      WFNEG  <ws3>,44
      WFNEG  <ws5>,EAX
      WFNEG  <wd4>,44
      WFNEG  <wd8>,EAX

NOP

      WFABS  <ws3>,<ws1>
      WFABS  <wd2>,<wd4>
      WFABS  <ws3>,44
      WFABS  <ws5>,EAX
      WFABS  <wd4>,44
      WFABS  <wd8>,EAX

NOP

      MOV     WORD PTR VW,-5
      PUSH    DWORD PTR VW
      POP     EAX
      MOV     WORD PTR VD,AX
      MOVSX   EAX,WORD PTR VD
      WFLOAT  <ws2>,EAX
      WFST    EAX,<ws2>

_TEXT ENDS
      END
