
{$codepage UTF8}

uses types,JS;

Var
  S,C,R : TJSString;
  t : TStringDynArray;
  i : integer;
  Q : String;
  
begin
  S:=TJSString.New('memamomu');
  C:=TJSString.New('你好!');
  R:=TJSString.New('привет!');
  writeln(s,' -> upper: ',s.toUpperCase);
  Writeln(s,' -> lower: ',s.toLowerCase);
  Writeln(s,' -> startsWith(''me''): ',s.startsWith('me'));
  Writeln(s,' -> startsWith(''ma''): ',s.startsWith('ma'));
  For I:=0 to s.length-1 do
    Writeln('S[',I,']: ',s.charAt(i),
    ', Charcode S[',I,']: ',s.charCodeAt(i),
    ', codePoint S[',I,']: ',s.codePointAt(i));
  For I:=0 to C.length-1 do
    Writeln('C[',I,']: ',C.charAt(i),
    ', Charcode C[',I,']: ',C.charCodeAt(i),
    ', codePoint C[',I,']: ',C.codePointAt(i));
  For I:=0 to R.length-1 do
    Writeln('R[',I,']: ',R.charAt(i),
    ', Charcode R[',I,']: ',R.charCodeAt(i),
    ', codePoint R[',I,']: ',R.codePointAt(i));
  Writeln(R,' -> indexOf(''вет''): ',R.indexOf('вет'));
  Writeln(C,' -> indexOf(''вет''): ',c.indexOf('вет'));
  Writeln(S,' -> lastIndexOf(''m''): ',S.lastIndexOf('m'));
  Writeln(S,' -> link(''freepascal.org''): ',S.link('freepascal.org'));
  t:=S.Split('m');
  writeln(s,' -> split length: ',length(t),', elements:');
  for i:=0 to length(t)-1 do
    writeln(i,' : ',t[i]);
  Writeln(s,' -> substr(6): ',s.substr(6));
  Writeln(s,' -> substr(5,2): ',s.substr(5,2));
  Q:='abcde';
  writeln(Q,', typecast to TJSString, uppercase: ',TJSString(Q).toUpperCase);
  t:=S.match(TJSRegexp.new('m.','g'));
  writeln(s,' -> match(/m./g): ',length(t),', elements:');
  for i:=0 to length(t)-1 do
    writeln(i,' : ',t[i]);
  Writeln(S,' -> replace(/m(.)/g/,''n$1''): ',S.replace(TJSRegexp.new('m(.)','g'),'n$1f'));
  
end.