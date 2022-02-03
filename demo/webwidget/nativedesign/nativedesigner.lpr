program nativedesigner;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF LINUX}
  InitSubProcess, // On Linux this unit must be used *before* the "interfaces" unit.
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, frmmain, webideintf
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

