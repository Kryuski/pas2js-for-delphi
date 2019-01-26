uses types, js;

Var
  O : TJSObject;
  S : TStringDynArray;
  I : integer;
  V : JSValue;
    
begin
  O:=new(['a','text','b',123,'c',true]);
  S:=TJSObject.getOwnPropertyNames(O);
  Writeln('Object has ',Length(S),' own properties');
  for I:=0 to Length(S)-1 do
    begin
    Writeln(i);
    V:=O.Properties[S[i]];
    Writeln('Property ',S[i],' : ',V);
    end;
  S:=TJSObject.Keys(O);
  Writeln('Object has ',Length(S),' keys');
  for I:=0 to Length(S)-1 do
    Writeln('Property ',S[i],' : ',O.Properties[S[i]]);
  Writeln('Manual : ');  
  Writeln('a: ',O['a']);
  Writeln('b: ',O['b']);
  Writeln('c: ',O['c']);
end.