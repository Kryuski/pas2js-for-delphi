{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2017-2020 by the Pas2JS development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit vscodeapp;

{$mode objfpc}

interface

uses
  Classes, SysUtils, JS, libvscode, custapp;

Type
  TVSCodeEnvironment = class (TJSObject)
    vscodeGlobal : TVSCode;
    extensionContext : TVSExtensionContext;
  end;
 
  TDeactivateCallback = reference to procedure; 
  TVSCodeExtensionCallBacks = Class (TJSObject)
    onDeactivate : TDeactivateCallback;
  end;
  { TVSCodeApplication }

  TVSCodeApplication = class(TCustomApplication)
  private
    FVSCode: TVSCode;
    FContext : TVSExtensionContext;
    procedure Activate;
    procedure Deactivate;
  Protected
    procedure DoActivate; virtual;
    procedure DoDeactivate(); virtual;
    procedure DoRun; override;
    function GetConsoleApplication: boolean; override;
    function GetLocation: String; override;
  Public
    procedure GetEnvironmentList(List: TStrings; NamesOnly: Boolean); override;
    procedure SaveVSCodeEnvironment(aEnv : TVSCodeEnvironment; aCallBacks : TVSCodeExtensionCallBacks);
    procedure ShowException(E: Exception); override;
    Property VSCode : TVSCode Read FVSCode;
    Property ExtensionContext : TVSExtensionContext read FContext;
  end;

Var
  Application : TVSCodeApplication;

implementation

{ TVSCodeApplication }

procedure TVSCodeApplication.DoActivate;
begin
  // Do nothing, override in descendents
end;

procedure TVSCodeApplication.DoDeactivate;
begin
 // Do nothing, override in descendents
end;

procedure TVSCodeApplication.DoRun;
begin
  // Do nothing
end;

function TVSCodeApplication.GetConsoleApplication: boolean;
begin
  Result:=False;
end;

function TVSCodeApplication.GetLocation: String;
begin
  result:=''; // VSCode.AppName;
end;

procedure TVSCodeApplication.GetEnvironmentList(List: TStrings; NamesOnly: Boolean);
begin
  // None
end;

procedure TVSCodeApplication.SaveVSCodeEnvironment(aEnv: TVSCodeEnvironment;
  aCallBacks: TVSCodeExtensionCallBacks);
begin
  FVSCode:=aEnv.vscodeGlobal;
  FContext:=aEnv.extensionContext;
  aCallBacks.onDeactivate:=@DeActivate;
  Activate;
end;

procedure TVSCodeApplication.ShowException(E: Exception);
begin
  VSCode.window.showErrorMessage(E.Message);
end;

procedure TVSCodeApplication.Activate;
begin
  DoActivate;
end;

procedure TVSCodeApplication.Deactivate;
begin
  DoDeActivate;
end;


end.

