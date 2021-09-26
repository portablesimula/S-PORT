module XOUT("xout");
begin

external("c") outstring;
import ref() str;
end;

record entity; info "DYNAMIC";
begin ref(entity) sl;
      range(0:255) sort,misc;
      variant ref(entity) ptp;
      variant range(0:32000) ncha;
      variant size lng;
end;

record txtent: entity;
begin character cha(0); end;

record txtqnt; info "TYPE";
begin ref(txtent) ent;
      range(0:32000) cp,sp,lp;
end;

visible routine XOUT;
import infix(txtqnt) tq;
begin outstring(tq.ent + size(entity)) end;

end
