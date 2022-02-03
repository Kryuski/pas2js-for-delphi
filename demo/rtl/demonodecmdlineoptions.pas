program demonodecmdlineoptions;

{$mode objfpc}

uses
  nodejsapp, JS, Classes, SysUtils, nodeJS;

type

  { TMyApplication }

  TMyApplication = class(TNodeJSApplication)
    procedure doRun; override;
  private
    procedure Usage(Msg: string);
  end;

procedure TMyApplication.Usage(Msg : string);

begin
  if Msg<>'' then
    Writeln('Error :',Msg);
  Writeln('Usage:');
  Writeln(ExeName,' [options]');
  Writeln('Where options is one or more of');
  Writeln('-h --help    this help message');
  Writeln('-e --echo    echo option');
  ExitCode:=Ord(Msg<>'');
end;

procedure TMyApplication.doRun;

Var
  S : String;
begin
  S:=CheckOptions('he:',['help','echo']);
  if (S<>'') or HasOption('h','help') then
    Usage(S);
  if HasOption('e','echo') then
    Writeln(GetoptionValue('e','echo'));
  for S in GetNonOptions('he:',['help','echo']) do
    Writeln(s);
  Terminate;
end;

var
  Application : TMyApplication;

begin
  Application:=TMyApplication.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
