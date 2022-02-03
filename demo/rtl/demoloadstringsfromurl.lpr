program demoloadstringsfromurl;

{$mode objfpc}

uses
  browserconsole, Classes;

Procedure DumpList(s : Tstrings);

Var
  I : Integer;

begin
  Writeln('----------');
  For I:=0 to S.Count-1 do
    Writeln('Line[',I,'] : ',S[I]);
  Writeln('----------');
end;

Var
  Lst,lst2,lst3 : TStrings;

begin
  Writeln('Loading synchronously');
  Lst:=TStringList.Create;
  try
    Lst.LoadFromFile('bytes.txt');
    DumpList(Lst);
  finally
    Lst.Free;
  end;
  Writeln('Loading asynchronously');
  // We can't free the stringlist, because of the async nature
  Lst2:=TStringList.Create;
  Lst2.LoadFromURL('bytes.txt',True,procedure(Sender: tobject)
    begin
    DumpList(Lst2);
    end
  );
  Writeln('Loading non-existing file');
  // We can't free the stringlist, because of the async nature
  Lst3:=TStringList.Create;
  Lst3.LoadFromURL('bytesnonexist.txt',True,procedure(Sender: tobject)
  begin
    DumpList(Lst3);
  end
  ,
  procedure(Sender: tobject; Const aError : string)
  begin
  Writeln('Load error: ',aError);
  end
  );
end.
