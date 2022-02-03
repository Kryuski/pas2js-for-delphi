program webwidgetsdemo;

{$mode objfpc}

uses
  browserconsole, browserapp, JS, Classes, SysUtils, Web, widgetdemo, frmDemo, Rtl.TemplateLoader,
  demohtmlwidgets, democonsts, webrouter, demobootstrap;

type
  TMyApplication = class(TBrowserApplication)
    FForm : TDemoForm;
    procedure doRun; override;
  end;

procedure TMyApplication.doRun;

begin
  Router.InitHistory(hkHash);
  FForm:=TDemoForm.Create(Self);
  FForm.Show;
  Router.RouteFromURL;
  Terminate;
end;

var
  Application : TMyApplication;

begin
  MaxConsoleLines:=15;
  ConsoleStyle:=DefaultCRTConsoleStyle;
  InitConsole;
  Application:=TMyApplication.Create(nil);
  Application.Initialize;
  Application.Run;
end.
