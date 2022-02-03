program consoledemo;

{$mode objfpc}

{$R help.txt}

uses
   types, p2jsres, nodejs;


Var
  RL : TStringDynArray;
  S : String;
  aInfo : TResourceInfo;


begin
  Writeln('Javascript source:');
  SetResourceSource(rsJS);
  RL:=GetResourceNames;
  For S in RL do
    begin
    Writeln('--- Found resource ',S,' : ');
    if not GetResourceInfo(S,aInfo) then
      Writeln('No extra information for resource ',S,' available')
    else
      begin
      Writeln('Name: ',aInfo.Name);
      Writeln('Format: ',aInfo.Format);
      Writeln('encoding: ',aInfo.Encoding);
      Writeln('unit: ',aInfo.resourceunit);
      Writeln('data length: ',Length(aInfo.data));
      end;
    end;
  if not GetResourceInfo('help',aInfo) then
    Writeln('resource help not found !')
  else
    begin
    Writeln('Usage:');
    Writeln(TNJSBuffer.from(ainfo.Data,'base64').toString);
    end;
end.
