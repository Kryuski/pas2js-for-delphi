uses JS;

Var
 R : TJSRegexp;
 T : TStringDynArray;
 i : integer;
 
begin
  r:=TJSRegexp.New('m(.)','g');
  writeln('source: ',r.source);
  writeln('toString: ',r.toString);
  writeln('Multiline: ',r.multiline);
  writeln('global: ',r.global);
  writeln('ignoreCase: ',r.ignoreCase);
{$IFDEF FIREFOX}  
  writeln('sticky: ',r.sticky);
{$ENDIF}  
  t:=r.exec('memamimomu');
  While t<>nil do
    begin
    Writeln(r.toString,' -> exec(''memamimomu'') : length ',length(t),' lastIndex:',r.lastIndex);
    for I:=0 to Length(t)-1 do
       Writeln('Match[',i,'] : ',t[i]); 
    t:=r.exec('memamimomu');
    end    
end.