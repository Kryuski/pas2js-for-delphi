program tetris;

{$mode objfpc}

uses
  browserapp, JS, Classes, SysUtils, Web, utetris;

type
  TTetrisApplication = class(TBrowserApplication)
    FTetris : TTetris;
    procedure doRun; override;
  end;

procedure TTetrisApplication.doRun;

begin
  FTetris:=TTetris.Create(Self);
  FTetris.Start;
end;

var
  Application : TTetrisApplication;

begin
  Application:=TTetrisApplication.Create(nil);
  Application.Initialize;
  Application.Run;
end.
