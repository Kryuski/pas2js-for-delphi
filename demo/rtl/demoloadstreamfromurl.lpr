program demoloadstreamfromurl;

{$mode objfpc}

uses
  browserconsole, Classes;

Var
  SS,SS2,SS3 : TStringStream;

begin
  Writeln('Loading synchronously');
  SS:=TStringStream.Create('');
  try
    SS.LoadFromFile('bytes.txt');
    Writeln('Loaded : ',SS.DataString);
  finally
    SS.Free;
  end;
  Writeln('Loading asynchronously');
  SS2:=TStringStream.Create('');
  SS2.LoadFromURL('bytes.txt',True,procedure(Sender: tobject)
    begin
      Writeln('Loaded 2: ',SS2.DataString);
    end
    );
  Writeln('Loading non-existing file');
  SS3:=TStringStream.Create('');
  SS3.LoadFromURL('bytesnonexist.txt',True,procedure(Sender: tobject)
    begin
      Writeln('Loaded 3: ',SS3.DataString);
    end
  ,
    procedure(Sender: tobject; Const aError : string)
    begin
      Writeln('Load error: ',aError);
    end
  );
end.
