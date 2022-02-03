program pacman;

{$mode objfpc}

uses
  browserapp, JS, Classes, SysUtils, Web, upacman;

type
  TMyApplication = class(TBrowserApplication)
    FPacMan : TPacMan;
    procedure doRun; override;
  end;

procedure TMyApplication.doRun;

begin
  FPacMan:=TPacMan.Create(Self);
  FPacMan.Start;
  // Your code here
  Terminate;
end;

var
  Application : TMyApplication;

begin
  Application:=TMyApplication.Create(nil);
  Application.Initialize;
  Application.Run;
//  Application.Free;
end.
