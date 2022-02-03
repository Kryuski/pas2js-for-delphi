program demowasiapp;

{$mode objfpc}

uses
  browserconsole, browserapp, JS, Classes, SysUtils, Web, WebAssembly, types,
  wasienv, wasihostapp;

Type

  { TMyApplication }

  TMyApplication = class(TWASIHostApplication)
  Private
    procedure DoWrite(Sender: TObject; const aOutput: String);
  Public
    Constructor Create(aOwner : TComponent); override;
    procedure doRun; override;
  end;


procedure TMyApplication.DoWrite(Sender: TObject; const aOutput: String);
begin
  Writeln(aOutput);
end;

constructor TMyApplication.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  WasiEnvironment.OnStdErrorWrite:=@DoWrite;
  WasiEnvironment.OnStdOutputWrite:=@DoWrite;
end;

procedure TMyApplication.doRun;

begin
  // Your code here
  Terminate;
  StartWebAssembly('helloworld.wasm');
end;

var
  Application : TMyApplication;

begin
  Application:=TMyApplication.Create(nil);
  Application.Initialize;
  Application.Run;
end.
