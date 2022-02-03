program designdemo;

{$mode objfpc}
{ $DEFINE USEIDE}

uses
  browserapp, JS, Classes, SysUtils, Web, designer, webideclient;

type
  TMyApplication = class(TBrowserApplication)
  Public
    FDemo : TDesignDemo;
    {$IFDEF USEIDE}
    FIDEIntf : TIDEClient;
    {$ENDIF}
    procedure doRun; override;
  end;

procedure TMyApplication.doRun;

begin
  FDemo:=TDesignDemo.Create(Self);
  {$IFDEF USEIDE}
  FIDEIntf:=TIDEClient.Create(Self);
  FDemo.IDEClient:=FIDEintf;
  FIDEIntf.RegisterClient;
  {$ENDIF}
end;

var
  Application : TMyApplication;

begin
  Application:=TMyApplication.Create(nil);
  Application.Initialize;
  Application.Run;
end.
