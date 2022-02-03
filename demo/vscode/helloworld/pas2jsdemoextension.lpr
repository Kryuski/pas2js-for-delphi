program pas2jsdemoextension;

{$mode objfpc}

uses
  JS, Types, Classes, SysUtils, libvscode, vscodeapp;


Type
  { TMyVSCodeExtension }

  TMyVSCodeExtension = Class(TVSCodeApplication)
  Private
    function DoHello(args:TJSValueDynArray) : JSValue;
  Protected
    procedure DoActivate; override;
    procedure DoDeactivate; override;
  Public
    // Add function handlers here
  end;


// Do not change the name of this procedure, the Javascript glue code depends on it.
// If you do want to change it, change the glue code as well.
Procedure InitVSCode(aVSCode : TVSCodeEnvironment; aCallBacks : TVSCodeExtensionCallBacks);

begin
  If Application=Nil then
    Application:=TMyVSCodeExtension.Create(Nil);
  Application.SaveVSCodeEnvironment(aVSCode,aCallBacks);
end;

{ TMyVSCodeExtension }

procedure TMyVSCodeExtension.DoActivate;

Var
  disp : TVSDisposable;

begin
  inherited DoActivate;
  disp:=vscode.commands.registerCommand('pas2jsdemoextension.helloWorld',@doHello);
  TJSArray(ExtensionContext.subscriptions).push(disp);
end;

function TMyVSCodeExtension.DoHello(args:TJSValueDynArray) : JSValue;
begin
  vscode.window.showInformationMessage('Hello World from Pas2js!');
end;

procedure TMyVSCodeExtension.DoDeactivate();
begin
  inherited DoDeactivate();
end;

// This code is needed to prevent the pas2js compiler from removing the InitAtom call.
var
  dummy : JSValue;

begin
  Application:=TMyVSCodeExtension.Create(Nil);
  dummy:=@InitVSCode;
end.

