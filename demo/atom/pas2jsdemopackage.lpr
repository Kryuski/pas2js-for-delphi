program pas2jsdemopackage;

{$mode objfpc}

uses
  JS, Classes, SysUtils, libAtom, atomapp, Web, demoview;


Type
  { TMyAtomApplication }

  TMyAtomApplication = Class(TAtomApplication)
  Private
    FOurView: TPas2jsdemopackageView;
    FModalPanel : TAtomPanel;
    procedure DoDismiss(Sender: TObject);
    procedure DoToggle;
  Protected
    procedure DoActivate(aState : TJSObject); override;
    procedure DoDeactivate; override;
    procedure DoSerialize(aState : TJSObject); override;
  Public
    // Add function handlers here
  end;


// Do not change the name of this procedure, the Javascript glue code depends on it.
// If you do want to change it, change the glue code as well.
Procedure InitAtom(aAtom : TAtomEnvironment; aCallBacks : TAtomPackageCallBacks);

begin
  If Application=Nil then
    Application:=TMyAtomApplication.Create(Nil);
  Application.SaveAtomEnvironment(aAtom,aCallBacks);
end;

{ TMyAtomApplication }
procedure TMyAtomApplication.DoDismiss(Sender : TObject);

begin
  FModalPanel.hide;
  atom.notifications.addInfo('Panel dismissed from pas2js');
end;

procedure TMyAtomApplication.DoActivate(aState: TJSObject);

Var
  cmds : TJSObject;
  opts : TAtomPanelOptions;
  VState: TJSObject;

begin
  inherited DoActivate(aState);
  if Assigned(aState) then
    VState:=TJSObject(aState['yourNameWordCountViewState'])
  else
    VState:=nil;
  FOurView:=TPas2jsdemopackageView.Create(VState);
  FOurView.OnDismiss:=@DoDismiss;
  opts:=TAtomPanelOptions.New;
  opts.item:=FourView.getElement;
  opts.visible:=False;
  FModalPanel:=atom.workspace.addModalPanel(opts);
  cmds:=TJSObject.New;
  cmds['pas2jsdemopackage:toggle']:=@DoToggle;
  subscriptions.add(atom.commands.add('atom-workspace', cmds));
end;

procedure TMyAtomApplication.DoToggle;
begin
  if FModalPanel.isVisible then
    fModalPanel.hide
  else
   fModalPanel.show;
end;

procedure TMyAtomApplication.DoDeactivate();
begin
  FModalpanel.destroy;
  FModalpanel:=Nil;
  inherited DoDeactivate();
end;

procedure TMyAtomApplication.DoSerialize(aState: TJSObject);
begin
  inherited DoSerialize(aState);
  if Assigned(FourView) then
    aState['yourNameWordCountViewState']:=FourView.Serialize;
end;

// This code is needed to prevent the pas2js compiler from removing the InitAtom call.
var
  dummy : JSValue;

begin
  Application:=TMyAtomApplication.Create(Nil);
  dummy:=@InitAtom;
end.

