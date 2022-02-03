{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2017-2020 by the Pas2JS development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit atomapp;

{$mode objfpc}

interface

uses
  Classes, SysUtils, JS, libatom, custapp;

Type
  TAtomEnvironment = class (TJSObject)
    atomGlobal : TAtom;
    subscriptions : TAtomCompositeDisposable;
    state : TJSObject;
  end;

  TSerializeCallback = Reference to Procedure (aEnv : TAtomEnvironment;aState : TJSObject);
  TDeactivateCallback = Reference to  Procedure (aEnv : TAtomEnvironment);

  TAtomPackageCallBacks = Class (TJSObject)
    onDeactivate : TDeactivateCallback;
    onSerialize : TSerializeCallback;
  end;

  { TAtomApplocation }

  { TAtomApplication }

  TAtomApplication = class(TCustomApplication)
  private
    FAtom: TAtom;
    FSubScriptions: TAtomCompositeDisposable;
    procedure Activate(aState : TJSObject);
    procedure Deactivate(aEnv : TAtomEnvironment);
    procedure Serialize(aEnv : TAtomEnvironment;aState : TJSObject);
  Protected
    procedure DoActivate(aState : TJSObject); virtual;
    procedure DoDeactivate(); virtual;
    procedure DoRun; override;
    procedure DoSerialize(aState : TJSObject); virtual;
    function GetConsoleApplication: boolean; override;
    function GetLocation: String; override;
  Public
    procedure GetEnvironmentList(List: TStrings; NamesOnly: Boolean); override;
    procedure SaveAtomEnvironment(aEnv : TAtomEnvironment; aCallBacks : TAtomPackageCallBacks);
    procedure ShowException(E: Exception); override;
    property Subscriptions : TAtomCompositeDisposable Read FSubScriptions;
    Property Atom : TAtom Read FAtom;
  end;

Var
  Application : TAtomApplication;

implementation

{ TAtomApplication }

procedure TAtomApplication.DoActivate(aState: TJSObject
  );
begin
  // Do nothing, override in descendents
end;

procedure TAtomApplication.DoDeactivate();
begin
 // Do nothing, override in descendents
end;

procedure TAtomApplication.DoRun;
begin
  // Do nothing
end;

procedure TAtomApplication.DoSerialize(aState: TJSObject);
begin
  // Do nothing, override in descendents
end;

function TAtomApplication.GetConsoleApplication: boolean;
begin
  Result:=False;
end;

function TAtomApplication.GetLocation: String;
begin
  result:=Atom.AppName;
end;

procedure TAtomApplication.GetEnvironmentList(List: TStrings; NamesOnly: Boolean);
begin
  // None
end;

procedure TAtomApplication.SaveAtomEnvironment(aEnv: TAtomEnvironment;
  aCallBacks: TAtomPackageCallBacks);
begin
  FAtom:=aEnv.atomGlobal;
  FSubscriptions:=aEnv.subscriptions;
  aCallBacks.onDeactivate:=@DeActivate;
  aCallbacks.onSerialize:=@Serialize;
  Activate(aEnv.state);
end;

procedure TAtomApplication.ShowException(E: Exception);
begin
  Atom.notifications.addError(E.Message);
end;

procedure TAtomApplication.Activate(aState: TJSObject);
begin
  DoActivate(aState);
end;

procedure TAtomApplication.Deactivate(aEnv : TAtomEnvironment);
begin
  DoDeActivate();
end;

procedure TAtomApplication.Serialize(aEnv: TAtomEnvironment; aState: TJSObject
  );
begin
  DoSerialize(aState);
end;

end.

