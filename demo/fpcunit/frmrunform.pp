unit frmrunform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, js, web, hotreloadclient, browserTestRunner, fpcunitreport;

Type

  { TTestRunForm }

  TTestRunForm = Class(TRunForm)
  private
    FElementID: String;
    BOutput : TJSHTMLInputElement;
    BRebuild : TJSHTMLInputElement;
    BRun : TJSHTMLInputElement;
    BuildID : TJSHTMLElement;
    FLastReq : TJSXMLHttpRequest;
    POutput: TJSElement;
    function RecompileClick(Event{%H-}: TJSMouseEvent): boolean;
    procedure CreatePanel(aParent: TJSElement);
    function DoReloadChange(Event{%H-}: TEventListenerEvent): boolean;
    function doResponse(Event{%H-}: TEventListenerEvent): boolean;
    function RunClick(aEvent{%H-}: TJSMouseEvent): boolean;
    function ToggleOutput(aEvent{%H-}: TJSMouseEvent): boolean;
  Public
    Constructor create(AOwner : TComponent); override;
    Procedure Initialize; override;
    // Where to place the control. Default fpcunit-control
    Property ElementID : String Read FElementID Write FElementID;
  end;

implementation

function TTestRunForm.doResponse(Event: TEventListenerEvent): boolean;

Var
  Data : TJSObject;
  S : String;

begin
  Result:=True;
  console.warn('Compile request response received');
  try
     Data:=TJSJSON.parseObject(FLastReq.responseText);
     if data['success'] then
       begin
       if isInteger(data['compileID']) then
         begin
         S:=String(data['compileID']);
         BuildID.innerText:=S;
         end;
       end
     else
       BuildID.innerText:='no build yet';
  except
    console.error('Compile request response received non-json response: '+FLastReq.ResponseText);
  end;
  FreeAndNil(FLastReq);
end;

function TTestRunForm.RunClick(aEvent: TJSMouseEvent): boolean;
begin
  Result:=True;
  if Assigned(OnRun) then
    OnRun(Self);
end;

function TTestRunForm.ToggleOutput(aEvent: TJSMouseEvent): boolean;

Var
  S : String;
  P : Integer;

begin
  Result:=True;
  S:=POutput.className;
  P:=Pos(' in',S);
  if P>0 then
    S:=Copy(S,1,P-1)
  else
    S:=S+' in';
  POutput.className:=S;
end;

function TTestRunForm.RecompileClick(Event: TJSMouseEvent): boolean;

Var
  Req : TJSXMLHttpRequest;

begin
  Result:=True;
   console.info('Recompile requested');
   Req:=TJSXMLHttpRequest.new;
   Req.addEventListener('load',@DoResponse);
   Req.open('POST','$sys/compile');
   Req.send;
   FLastReq:=Req;
end;


Procedure TTestRunForm.CreatePanel(aParent : TJSElement);

Var
  Panel : TJSElement;
  BG,D,PanelContent : TJSElement;
  CB : TJSHTMLInputElement;

begin
  Panel:=aParent;
  // attrs are default array property...
  Panel['class']:='panel panel-default';
  // Innner panel
  PanelContent:=document.createElement('div');
  Panel.appendChild(PanelContent);
  PanelContent['class']:='panel-heading';
  BG:=document.createElement('div');
  BG['class']:='btn-group';
  PanelContent.appendChild(BG);
  // Run Button
  BRun:=TJSHTMLInputElement(document.createElement('button'));
  BRun['id']:=ElementID+'-BRun';
  BRun['type']:='button';
  BRun['class']:='btn btn-primary';
  BRun.appendChild(document.createTextNode('Re-Run'));
  BRun.onclick:=@RunClick;
  BG.appendChild(BRun);

  // Rebuild Button
  BRebuild:=TJSHTMLInputElement(document.createElement('button'));
  BRebuild['id']:=ElementID+'-BRebuild';
  BRebuild['type']:='button';
  BRebuild['class']:='btn btn-primary';
  BRebuild.appendChild(document.createTextNode('Recompile'));
  BRebuild.onclick:=@RecompileClick;
  BG.appendChild(BRebuild);
  // Output Button
  BOutput:=TJSHTMLInputElement(document.createElement('button'));
  BOutput['id']:=ElementID+'-BRebuild';
  BOutput['type']:='button';
  BOutput['class']:='btn btn-primary';
  BOutput.appendChild(document.createTextNode('Output'));
  BOutput.onclick:=@ToggleOutput;
  BG.appendChild(BOutput);
  // Span for build ID
  D:=Document.createElement('span');
  D.id:='buildlabel';
  D['style']:='margin-left: 15px; margin-right: 15px';
  D.appendChild(Document.createTextNode('Build ID:'));

  PanelContent.appendChild(D);
  BuildID:=TJSHTMLElement(document.createElement('span'));
  BuildID['id']:='recompileID';
  BuildID['class']:='badge';
  BuildID.appendChild(document.createTextNode('no build yet'));
  PanelContent.appendChild(BuildID);
  // Combo box to trigger onload
  D:=Document.createElement('label');
  D['class']:='checkbox-inline';
  D['style']:='margin-left: 15px';
  cb:=TJSHTMLInputElement(document.createElement('input'));
  cb['id']:='cbautoreload';
  cb['type']:='checkbox';
  cb.onchange:=@DoReloadChange;
  // Sync with hotreload
  cb.checked:=THotReload.getglobal.options.Reload;
  D.AppendChild(cb);
  D.appendChild(Document.createTextNode('Reload page on build/sync'));
  PanelContent.appendChild(D);
  // Collapsible panel For compiler output
  POutput:=document.createElement('div');
  POutput['class']:='panel-collapse collapse';
  D:=Document.createElement('div');
  D['class']:='panel-body';
  D['id']:=ElementID+'-output';
  // Tell it to hotreload.
  THotReload.getglobal.options.OverlayElementID:=ElementID+'-output';
  POutput.appendChild(D);
  Panel.appendChild(POutput);
  // Hint
end;

constructor TTestRunForm.Create(aOwner : TComponent);
begin
  inherited;
  ElementID:='fpcunit-controller'
end;

procedure TTestRunForm.Initialize;

Var
  Panel : TJSElement;
begin
  inherited Initialize;
  THotReload.StartHotReload;
  Panel:=document.getElementById(Self.ElementID);
  if Panel<>Nil then
    CreatePanel(Panel);
  // Start hotreload
end;

function TTestRunForm.DoReloadChange(Event: TEventListenerEvent): boolean;

begin
  Result:=True;
  THotReload.getglobal.options.Reload:=not THotReload.getglobal.options.Reload;
end;

end.

