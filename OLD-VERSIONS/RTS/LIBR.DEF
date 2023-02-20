 Module libr("RTS");
 begin sysinsert rt,sysr,knwn,util,strg,cent,arr;

       -----------------------------------------------------------
       ---  COPYRIGHT 1989 by                                  ---
       ---  Simula a.s.                                        ---
       ---  Oslo, Norway                                       ---
       ---                                                     ---
       ---           P O R T A B L E     S I M U L A           ---
       ---            R U N T I M E     S Y S T E M            ---
       ---                                                     ---
       ---         S t a n d a r d   P r o c e d u r e s       ---
       ---                                                     ---
       -----------------------------------------------------------


 routine ERR_STD;
 begin
 ---   if status = 27 then
                           status:=0; ERROR(ENO_STD_3);
 ---   else IERR_E endif; -- internal error in ENVIR
 end;


 Visible routine ARCOSR; import real arg; export real val;
 begin bio.errmsg:="ARCCOS"; val:=RARCOS(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine ARCOSD; import long real arg; export long real val;
 begin bio.errmsg:="ARCCOS"; val:=ARCCOS(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine ARSINR; import real arg; export real val;
 begin bio.errmsg:="ARCSIN"; val:=RARSIN(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine ARSIND; import long real arg; export long real val;
 begin bio.errmsg:="ARCSIN"; val:=ARCSIN(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine ARTANR; import real arg; export real val;
 begin bio.errmsg:="ARCTAN"; val:=RARTAN(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine ARTAND; import long real arg; export long real val;
 begin bio.errmsg:="ARCTAN"; val:=ARCTAN(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine RATAN2; import real y,x; export real val;
 begin bio.errmsg:="ARTAN2"; val:=ATAN2R(y,x);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine DATAN2; import long real y,x; export long real val;
 begin bio.errmsg:="ARTAN2"; val:=ATAN2(y,x);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine RCOS; import real arg; export real val;
 begin bio.errmsg:="COS"; val:=RCOSIN(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine DCOS; import long real arg; export long real val;
 begin bio.errmsg:="COS"; val:=COSINU(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine REXP; import real arg; export real val;
 begin bio.errmsg:="EXP"; val:=REXPON(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine DEXP; import long real arg; export long real val;
 begin bio.errmsg:="EXP"; val:=EXPONE(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine HISTO; import ref(reaAr1) a,b; real c,d;
 begin integer i,nelt; i:=0;
       if a.sort<>S_ARENT1 then ERROR(ENO_STD_1) endif;
       if b.sort<>S_ARENT1 then ERROR(ENO_STD_1) endif;
       nelt:=b.ub - b.lb + 1;
       if nelt >= (a.ub-a.lb+1) then ERROR(ENO_STD_2) endif;
       repeat if b.elt(i) >= c then goto EX endif;  i:=i+1;
       while i < nelt do endrepeat;
EX:    a.elt(i):=a.elt(i)+d;
 end;

 Visible routine RLN; import real arg; export real val;
 begin bio.errmsg:="LN"; val:=RLOGAR(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine DLN; import long real arg; export long real val;
 begin bio.errmsg:="LN"; val:=LOGARI(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine RLOG; import real arg; export real val;
 begin bio.errmsg:="LOG"; val:=RLOG10(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine DLOG; import long real arg; export long real val;
 begin bio.errmsg:="LOG"; val:=DLOG10(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine RSIN; import real arg; export real val;
 begin bio.errmsg:="SIN"; val:=RSINUS(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine DSIN; import long real arg; export long real val;
 begin bio.errmsg:="SIN"; val:=SINUSR(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine RSQRT; import real arg; export real val;
 begin bio.errmsg:="SQRT"; val:=RSQROO(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine DSQRT; import long real arg; export long real val;
 begin bio.errmsg:="SQRT"; val:=SQROOT(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine RTAN; import real arg; export real val;
 begin bio.errmsg:="TAN"; val:=RTANGN(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine DTAN; import long real arg; export long real val;
 begin bio.errmsg:="TAN"; val:=TANGEN(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine RCOTAN; import real arg; export real val;
 begin bio.errmsg:="COTAN"; val:=COTANR(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine DCOTAN; import long real arg; export long real val;
 begin bio.errmsg:="COTAN"; val:=COTANG(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine RSINH; import real arg; export real val;
 begin bio.errmsg:="SINH"; val:=SINHR(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine DSINH; import long real arg; export long real val;
 begin bio.errmsg:="SINH"; val:=SINH(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine RCOSH; import real arg; export real val;
 begin bio.errmsg:="COSH"; val:=COSHR(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine DCOSH; import long real arg; export long real val;
 begin bio.errmsg:="COSH"; val:=COSH(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine RTANH; import real arg; export real val;
 begin bio.errmsg:="TABH"; val:=TANHR(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

 Visible routine DTANH; import long real arg; export long real val;
 begin bio.errmsg:="TABH"; val:=TANH(arg);
       if status<>0 then ERR_STD endif; bio.errmsg.nchr:=0 end;

%title ***   R a n d o m   D r a w i n g   ***

 Visible routine discrR;
 import ref(reaAr1) a; name(integer) u; export integer i;
 begin integer j,nelt; real v;
       if a.sort<>S_ARENT1 then ERROR(ENO_DRW_1) endif;
       v:=DRAWRP(u) qua real; nelt:=a.ub-a.lb+1; i:=a.ub+1; j:=0;
       repeat if a.elt(j) > v then i:=a.lb+j; nelt:=0 endif;
              j:= j + 1;
       while j < nelt do endrepeat;
 end;


 Visible routine DRAW;
 import long real a; name(integer) u; export boolean val;
 -- begin if a >= 1.0&&0 then val:= true;
 --    elsif a <= 0.0&&0 then val:= false;
 --    else val:= DRAWRP(u) <= a endif;
 -- end;
 begin val:= DRAWRP(u) <= a; -- NB! DRAWRP must always be called
       if a >= 1.0&&0 then val:= true;
    elsif a <= 0.0&&0 then val:= false; endif;
 end;


 Visible routine ERLANG;
 import long real a,b; name(integer) u; export long real val;
 begin integer c; long real bc,ab,z,v;
       if (a <= 0.0&&0) or (b <= 0.0&&0) then ERROR(ENO_DRW_7) endif;
       val:= 0.0&&0; c:= DENTI(b);
       bc:= b - (c qua long real); ab:= a * b;
       repeat c:= c - 1;
       while c >= 0
       do v:= DRAWRP(u); z:= LOGARI(v);
          if status<>0 then ERROR(ENO_DRW_9)  endif;
          val:= val - (z/ab);
       endrepeat;
       if bc > 0.0&&0
       then v:= DRAWRP(u); z:= LOGARI(v);
            if status<>0 then ERROR(ENO_DRW_9)  endif;
            val:= val - ((bc*z) / ab );
       endif;
 end;


 Visible routine HISTD;
 import ref(reaAr1) a; name(integer) u; export integer i;
 begin integer j;         --  Array index.
       integer nelt;      --  Number of array elements.
       real sum;          --  Sum of all array element values.
       real wsum;         --  Weighted sum of all array element values.
       real tmp;          --  Temporary variabel.
       if a.sort<>S_ARENT1 then ERROR(ENO_DRW_1) endif;
       nelt:= a.ub + a.lb - 1; j:= 0; sum:= 0.0;
       repeat tmp:= a.elt(j);
              if tmp < 0.0 then ERROR(ENO_DRW_2) endif;
              sum:= sum + tmp; j:= j + 1;
       while j < nelt do endrepeat;
       wsum:= (DRAWRP(u) qua real) * sum;  --  Make  0 <= wsum < sum
       j:= 0; sum:= 0.0;
       repeat sum:= sum + a.elt(j);
              if sum >= wsum         --  We will take the then-branch
              then i:= a.lb+j; nelt:=0 endif;  --  once and only once.
              j:= j + 1;
       while j < nelt do endrepeat;
 end;


 Visible routine LINEAR;
 import ref(reaAr1) a,b; name(integer) u; export long real val;
 begin integer i,nelt; real a_val,a_lag,a_dif,b_val,b_lag;
       long real v;
       if a.sort<>S_ARENT1 then ERROR(ENO_DRW_1) endif;
       if b.sort<>S_ARENT1 then ERROR(ENO_DRW_1) endif;
       nelt:= a.ub - a.lb + 1;
       if nelt<>(b.ub-b.lb+1) then ERROR(ENO_DRW_3) endif;
       v:= DRAWRP(u); i:= 0;
       repeat while (a.elt(i) qua long real) < v do i:=i+1 endrepeat;
       if i <= 0
       then if ((v qua real) = 0.0) and (a.elt = 0.0) then i:= 1;
            else ERROR(ENO_DRW_4) endif;
       elsif i >= nelt then ERROR(ENO_DRW_4) endif;

       a_val:= a.elt(i); a_lag:= a.elt(i-1); a_dif:= a_val - a_lag;
       if a_dif = 0.0 then val:= b.elt(i-1) qua long real
       else b_val:= b.elt(i); b_lag:= b.elt(i-1);
            val:= ((((b_val-b_lag) qua long real)/(a_dif qua long real))
                  * (v-(a_lag qua long real))) + (b_lag qua long real);
       endif;
 end;


 Visible routine  NEGEXP;
 import long real a; name(integer) u; export real val;
 begin long real v;
       if a <= 0.0&&0 then ERROR(ENO_DRW_5) endif;
       v:= DRAWRP(u); val:= (- LOGARI(v) / a) qua real;
       if status<>0 then ERROR(ENO_DRW_9) endif;
 end;


 Visible routine  NORMAL;
 import long real a,b; name(integer) u; export long real val;
 begin long real t,p,q,v,x; boolean z;
       if b < 0.0&&0 then ERROR(ENO_DRW_8) endif;
       v:= DRAWRP(u);
       if v > 0.5&&0 then z:=true; v:=1.0&&0-v else z:= false endif;
       t:= LOGARI(v);      if status<>0 then ERROR(ENO_DRW_9) endif;
       t:= SQROOT(-t-t);   if status<>0 then ERROR(ENO_DRW_9) endif;
       p:= 2.515517&&0 + (t * (0.802853&&0 + (t*0.010328&&0)));
       q:= 1.0&&0 +
           (t * (1.432788&&0 + (t * (0.189269&&0 + (t*0.001308&&0)))));
       x:= b * (t-(p/q));
       val:= a + if z then x else -x ;
 end;


 Visible routine  poissn;
 import long real a; name(integer) u; export long real val;
 begin long real acc,xpa,sqa;
       if a <= 0.0&&0 then val:= 0.0&&0 elsif a > 20.0&&0
       then sqa:= SQROOT(a); if status<>0 then ERROR(ENO_DRW_9) endif;
            val:= normal(a,sqa,u);
       else acc:= 1.0&&0; val:= 0.0&&0;
            xpa:= EXPONE(-a); if status<>0 then ERROR(ENO_DRW_9) endif;
            repeat acc:= DRAWRP(u) * acc;
            while acc >= xpa do val:= val + 1.0&&0 endrepeat;
      endif;
 end;


 Visible routine  RANDI;
 import integer a,b; name(integer) u; export integer val;
 begin if b < a then ERROR(ENO_DRW_6) endif;
       val:=DENTI
            (DRAWRP(u) * ((b qua long real)-(a qua long real)+1.0&&0))
            + a;
 end;


 Visible routine UNIFRM;
 import long real a,b; name(integer) u; export long real val;
 begin if b < a then ERROR(ENO_DRW_6) endif;
       val:=(DRAWRP(u)*(b-a))+a;
 end;


 Visible routine discrD;
 import ref(lrlAr1) a; name(integer) u; export integer i;
 begin integer j,nelt; long real v;
       if a.sort<>S_ARENT1 then ERROR(ENO_DRW_1) endif;
       v:=DRAWRP(u); nelt:=a.ub-a.lb+1; i:=a.ub+1; j:=0;
       repeat if a.elt(j) > v then i:=a.lb+j; nelt:=0 endif;
              j:= j + 1;
       while j < nelt do endrepeat;
 end;


 Visible routine DHISTD;
 import ref(lrlAr1) a; name(integer) u; export integer i;
 begin integer j;         --  Array index.
       integer nelt;      --  Number of array elements.
       long real sum;     --  Sum of all array element values.
       long real wsum;    --  Weighted sum of all array element values.
       long real tmp;     --  Temporary variabel.
       if a.sort<>S_ARENT1 then ERROR(ENO_DRW_1) endif;
       nelt:= a.ub + a.lb - 1; j:= 0; sum:= 0.0&&0;
       repeat tmp:= a.elt(j);
              if tmp < 0.0&&0 then ERROR(ENO_DRW_2) endif;
              sum:= sum + tmp; j:= j + 1;
       while j < nelt do endrepeat;
       wsum:= DRAWRP(u) * sum;             --  Make  0 <= wsum < sum
       j:= 0; sum:= 0.0&&0;
       repeat sum:= sum + a.elt(j);
              if sum >= wsum            --  We will take the then-branch
              then i:= a.lb+j; nelt:=0 endif;  --  once and only once.
              j:= j + 1;
       while j < nelt do endrepeat;
 end;


 Visible routine LINEAD;
 import ref(lrlAr1) a,b; name(integer) u; export long real val;
 begin integer i,nelt; long real a_val,a_lag,a_dif,b_val,b_lag,v;
       if a.sort<>S_ARENT1 then ERROR(ENO_DRW_1) endif;
       if b.sort<>S_ARENT1 then ERROR(ENO_DRW_1) endif;
       nelt:= a.ub + a.lb - 1;
       if nelt<>(b.ub+b.lb-1) then ERROR(ENO_DRW_3) endif;
       v:= DRAWRP(u); i:= 0;
       repeat while a.elt(i) < v do i:=i+1 endrepeat;
       if i <= 0
       then if (v = 0.0&&0) and (a.elt = 0.0&&0) then i:= 1;
            else ERROR(ENO_DRW_4) endif;
       elsif i >= nelt then ERROR(ENO_DRW_4) endif;

       a_val:= a.elt(i); a_lag:= a.elt(i-1); a_dif:= a_val - a_lag;
       if a_dif = 0.0&&0 then val:= b.elt(i-1)
       else b_val:= b.elt(i); b_lag:= b.elt(i-1);
            val:= (((b_val-b_lag)/a_dif) * (v-a_lag)) + b_lag;
       endif;
 end;

end;
