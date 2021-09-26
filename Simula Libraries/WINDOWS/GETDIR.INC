%+f   text procedure getdir(template,resName); text template,resName;
%+f   !*** performs 'dir/ls template' and returns the name of a
%+f        file containing the resulting list (sorted), stripped of
%+f        blank lines etc. (DOS), and with concatenated
%+f        file names (XENIX/UNIX eg: ls */*.sim);
%+f   begin text fname,t,v; ref(infile) tmp; ref(outfile) res;
%+f         character ch; text prefix;

%+f         if resName==notext then resName:-TempFile;
%+f         res:-new outfile(resName);
%+f         if not res.open(blanks(72)) then goto ERR;
%+f         nFiles:=0; fname:-TempFile;
%+M %+f     System ("dir " & Template & " | sort >" & fname);
%+O %+f     System ("dir " & Template & " >" & fname);
%+X %+f     System ("ls >" & fname & " 2>/dev/null " & Template);
%+f         tmp:-new infile(fname);
%+f         if not tmp.open(blanks(72)) then goto ERR1;
%+f         getdir:-resName; goto LOOP;

%+f            while not tmp.endfile do begin
%+f               t:-tmp.image;
%+f               ch:=loadchar(t,0); if ch = ' ' then goto LOOP;
%-X %+f           if ch='.' or else digit(ch) then goto LOOP;
%-X %+f           lowcase(t);
%-X %+f           v:-t.sub(10,3).strip;
%-X %+f           res.outtext(t.sub(1,8).strip);
%-X %+f           if v=/=notext then res.outtext("." & v);
%+X %+f           t:-t.Strip;
%+X %+f           if loadchar(t,t.length-2)=':'
%+X %+f           then begin prefix:-t.sub(1,t.length-1) & "/"; goto LOOP end;
%+X %+f           res.outtext(prefix); res.outtext(t);
%+f               res.outimage; !outtext("!13!!10!");
%+f               nFiles:=nFiles+1;
%+f      LOOP:    tmp.inimage;
%+f            end;

%+f   ERR1: tmp.close; DeleteFile(fname);
%+f   ERR:  res.close;
%+f   end getdir;

