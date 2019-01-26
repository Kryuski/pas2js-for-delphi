{$mode objfpc}
{$H+}
uses browserconsole,sysutils,classes;

Var
  L : TStringList;
  I : Integer;
  //S : TJSString;
  S : String;

begin
  L:=TStringList.Create;
  for I:=0 to 10 do
    L.Add(IntToStr(I));
  for I:=0 to L.Count-1 do
    Writeln(I,' : ',L[i]);
  writeln('in one swoop: ',L.text);
  L.Delete(4);
  Writeln('Index 4 deleted.',L.Text);
  Writeln('Commatext : ',L.CommaText);
  Writeln('IndexOf(5) : ',L.INdexOf('5'));
  L.Clear;
  Writeln('Clear : "',L.Text,'"');
  L.CommaText:='3,4,5';
  Writeln('After set commatext:  ',L.CommaText);
  L.exchange(2,0);
  Writeln('After exchange : ',L.CommaText);
  L.Sort;
  Writeln('After sort : ',L.CommaText);
  //S:=TJSString.new('abc');
  //Writeln(S.toUpperCase);
  Writeln('For in loop:');
  for S in L do
    Writeln(S);
  
end.  
