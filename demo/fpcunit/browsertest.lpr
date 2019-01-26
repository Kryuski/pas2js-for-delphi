program browsertest;

uses
  SysUtils, BrowserTestRunner, demotests, frmrunform;

Var
  Application : TTestRunner;

begin
  Application:=TTestRunner.Create(Nil);
  Application.RunFormClass:=TTestRunForm;
  Application.Initialize;
  Application.Run;
  Application.Free;
end.

