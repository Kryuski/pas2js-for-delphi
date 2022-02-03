unit frmrtlrun;

{$mode objfpc}

interface

uses
  Classes, fpcunitreport, BrowserConsole, web;

Type

  { TConsoleRunner }

  TConsoleRunner = Class(TRunForm)
  Private
    FRun : TJSHTMLButtonElement;
    function DoRunTest(aEvent: TJSMouseEvent): boolean;
  public
    procedure initialize; override;
  end;

implementation

{ TConsoleRunner }

function TConsoleRunner.DoRunTest(aEvent: TJSMouseEvent): boolean;
begin
  Result:=False;
  ResetConsole;
  If Assigned(OnRun) then
    OnRun(Self);
end;

procedure TConsoleRunner.initialize;
begin
  FRun:=TJSHTMLButtonElement(document.getElementById('RunTest'));
  FRun.onClick:=@DoRunTest;
  ResetConsole;
end;

end.

