unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, webideintf, Forms, Controls, Graphics, Dialogs, EditBtn,
  ExtCtrls, ComCtrls, StdCtrls, ActnList, LazFileUtils, GlobalCefApplication,
  {$IFDEF DARWIN}  uCEFLazarusCocoa,  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows, Messages,
  {$ENDIF}
  uCEFChromium, uCEFWindowParent, uCEFChromiumWindow, uCEFTypes, uCEFInterfaces,
  uCEFWinControl, uCEFApplication, uCEFWorkScheduler, uCEFBrowserWindow, fpJSON, uCEFChromiumEvents;

type

  { TMainForm }

  TMainForm = class(TForm)
    AOpenDev: TAction;
    AGoExternal: TAction;
    AGo: TAction;
    ALWidgets: TActionList;
    BrowserWindow1: TBrowserWindow;
    FEProject: TFileNameEdit;
    ILWidgets: TImageList;
    BLog: TMemo;
    MLog: TMemo;
    Panel1: TPanel;
    PnlLog: TPanel;
    PnlBLog: TPanel;
    PCDesigner: TPageControl;
    Project: TLabel;
    PBottom: TPanel;
    BrowserLog: TTabSheet;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    TBExternalGo: TToolButton;
    TBExternalGo1: TToolButton;
    TSInspector: TTabSheet;
    TSBrowser: TTabSheet;
    TSLog: TTabSheet;
    TBWidgets: TToolBar;
    TBGo: TToolButton;
    ToolButton1: TToolButton;
    procedure AGoExecute(Sender: TObject);
    procedure AGoExternalExecute(Sender: TObject);
    procedure AGoUpdate(Sender: TObject);
    procedure AOpenDevExecute(Sender: TObject);
    procedure BrowserWindow1BrowserClosed(Sender: TObject);
    procedure BrowserWindow1BrowserCreated(Sender: TObject);
    procedure ChromiumConsoleMessage(Sender: TObject;
      const browser: ICefBrowser; level: TCefLogSeverity; const message,
      source: ustring; line: Integer; out Result: Boolean);
    procedure cwOnBeforePopup(Sender: TObject;
      const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
      targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
      userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
      var windowInfo: TCefWindowInfo; var client: ICefClient;
      var settings: TCefBrowserSettings;
      var extra_info: ICefDictionaryValue;
      var noJavascriptAccess: Boolean;
      var Result: Boolean);
    procedure DEProjectEditingDone(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
  private
    FFormClosing: Boolean;
    FClientID : Int64; // Just one for now
    FDesignCaption : String;
    FWebIDEIntf : TIDEServer;
    FWidgetCount : Integer;
    FWidgets : Array of String;
    FAllowGo: Boolean;
{$IFDEF WINDOWS}
    procedure WMEnterMenuLoop(var aMessage: TMessage); message WM_ENTERMENULOOP;
    procedure WMExitMenuLoop(var aMessage: TMessage); message WM_EXITMENULOOP;
{$ENDIF}
    function GetProjectURL: String;
    procedure DoAddWidget(Sender: TObject);
    procedure DoAction(Sender: TObject; aExchange: TIDEExchange);
    procedure DoClientCame(Sender: TObject; aClient: TIDEClient);
    procedure DoClientLeft(Sender: TObject; aClient: TIDEClient);
    procedure DoLogRequest(Sender: TObject; aURL: String);
    procedure IsWidgetEnabled(Sender: TObject);
    Procedure RegisterWidgets;
    Procedure RegisterWidget(aWidget: String; aImageIndex : Integer);
  public
    Procedure Log(Msg : String);
    Procedure Log(Fmt : String; Args : Array of const);
  end;

var
  MainForm: TMainForm;

implementation

uses lclintf, fpmimetypes;

{$R *.lfm}

type

  { TLogMsg }

  TLogMsg = class
  private
    FMsg: String;
  public
    constructor Create(AMsg: String);
    procedure DoBrowserMsg(Data: PtrInt);
    procedure DoLog(Data: PtrInt);
  end;


{ TLogMsg }

constructor TLogMsg.Create(AMsg: String);
begin
  FMsg := AMsg;
end;

procedure TLogMsg.DoBrowserMsg(Data: PtrInt);
begin
  if MainForm <> nil then
    MainForm.BLog.Append(FMsg);
  Free;
end;

procedure TLogMsg.DoLog(Data: PtrInt);
begin
  if MainForm <> nil then
    MainForm.Log(FMsg);
  Free;
end;

{ TMainForm }

procedure TMainForm.DEProjectEditingDone(Sender: TObject);
begin
  FWebIDEIntf.ProjectDir:=ExtractFilePath(FEProject.FileName);
end;

procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  FWebIDEIntf.Active:=False;
  FFormClosing := True;
  BrowserWindow1.CloseBrowser(True);
  CanClose:=BrowserWindow1.IsClosed;
  Visible  := False;
end;

Function TMainForm.GetProjectURL : String;

begin
  Result:=Format('http://localhost:%d/Project/%s',[FWebIDEIntf.Port,ExtractFileName(FEProject.FileName)]);
end;

procedure TMainForm.AGoExecute(Sender: TObject);
Var
  URL : String;

begin
  URL:=GetProjectURL;
  Log('Going to URL: %s',[URL]);
  BrowserWindow1.LoadURL(URL);
end;

procedure TMainForm.AGoExternalExecute(Sender: TObject);
Var
  URL : String;

begin
  URL:=GetProjectURL;
  Log('Going to URL: %s',[URL]);
  OpenURL(URL);
end;

procedure TMainForm.AGoUpdate(Sender: TObject);
begin
  (Sender as Taction).Enabled:=FAllowGo;
end;

procedure TMainForm.AOpenDevExecute(Sender: TObject);
var
  p: TPoint;
begin
  p.X := 0;
  p.Y := 0;
  BrowserWindow1.Chromium.ShowDevTools(p,nil);
end;

procedure TMainForm.BrowserWindow1BrowserClosed(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.BrowserWindow1BrowserCreated(Sender: TObject);
begin
  // Now the browser is fully initialized we can load the initial web page.
  FAllowGo:=True;
end;

procedure TMainForm.ChromiumConsoleMessage(Sender: TObject;
  const browser: ICefBrowser; level: TCefLogSeverity; const message,
  source: ustring; line: Integer; out Result: Boolean);
var
  m: TLogMsg;
begin
  if FFormClosing then
    exit;
  m := TLogMsg.Create(Format('%s [%s %d]', [message, source, line]));
  Application.QueueAsyncCall(@m.DoBrowserMsg, 0);
  // Issue https://gitlab.com/freepascal.org/fpc/source/-/issues/39367
  //Application.QueueAsyncCall(@TLogMsg.Create(Format('%s [%s %d]', [message, source, line])).DoBrowserMsg, 0);
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  s: String;
begin
  FAllowGo:=False;
  FDesignCaption:=Caption;
  MimeTypes.LoadKnownTypes;
  s := ExtractFilePath(Paramstr(0));
  if pos('nativedesign', s) > 0 then
    s := StringReplace(s, 'nativedesign', 'designdemo', [rfReplaceAll, rfIgnoreCase])
  else
    s := s+'designdemo';
  s := AppendPathDelim(s)+'designdemo.html';
  FEProject.FileName:=s;
  FWebIDEIntf:=TIDEServer.Create(Self);
  FWebIDEIntf.ProjectDir:=ExtractFilePath(FEProject.FileName);
  FWebIDEIntf.OnClientAdded:=@DoClientCame;
  FWebIDEIntf.OnClientRemoved:=@DoClientLeft;
  FWebIDEIntf.OnRequest:=@DoLogRequest;
  FWebIDEIntf.OnAction:=@DoAction;
  FWebIDEIntf.Active:=True;
  TSInspector.TabVisible:=False;
  RegisterWidgets;
end;

procedure TMainForm.Panel1Resize(Sender: TObject);
begin
  //if not Visible then
  //  exit;
  if Width = 0 then begin
    if MLog.Parent = PnlLog then begin
      MLog.Parent := TSLog;
      TSLog.TabVisible := True;
    end;
    if BLog.Parent = PnlBLog then begin
      BLog.Parent := BrowserLog;
      BrowserLog.TabVisible := True;
    end;
  end
  else begin
    if MLog.Parent = TSLog then begin
      MLog.Parent := PnlLog;
      TSLog.TabVisible := False;
    end;
    if BLog.Parent = BrowserLog then begin
      BLog.Parent := PnlBLog;
      BrowserLog.TabVisible := False;
    end;
  end;
end;

{$IFDEF WINDOWS}
procedure TMainForm.WMEnterMenuLoop(var aMessage: TMessage);

begin
  inherited;
  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then
    GlobalCEFApp.OsmodalLoop := True;
end;

procedure TMainForm.WMExitMenuLoop(var aMessage: TMessage);

begin
  inherited;
  if (aMessage.wParam = 0) and (GlobalCEFApp <> nil) then
    GlobalCEFApp.OsmodalLoop := False;
end;
{$ENDIF}

procedure TMainForm.cwOnBeforePopup(Sender: TObject;
  const browser: ICefBrowser; const frame: ICefFrame; const targetUrl,
  targetFrameName: ustring; targetDisposition: TCefWindowOpenDisposition;
  userGesture: Boolean; const popupFeatures: TCefPopupFeatures;
  var windowInfo: TCefWindowInfo; var client: ICefClient;
  var settings: TCefBrowserSettings;
  var extra_info: ICefDictionaryValue;
  var noJavascriptAccess: Boolean;
  var Result: Boolean);
begin
  // For simplicity, this demo blocks all popup windows and new tabs
  Result := (targetDisposition in [WOD_NEW_FOREGROUND_TAB, WOD_NEW_BACKGROUND_TAB, WOD_NEW_POPUP, WOD_NEW_WINDOW]);
end;


procedure TMainForm.DoAction(Sender: TObject; aExchange: TIDEExchange);

var
  PayJSON : TJSONObject;

begin
  payJSON:=Nil;
  if Not (aExchange.Payload is TJSONObject) then
    begin
    Log('Payload is not JSON Object');
    exit;
    end;
  payJSON:=aExchange.Payload as TJSONObject;
  with aExchange do
    case Name of
    'create':
      Log('Browser created widget of class %s, name %s',[PayJSON.Get('class',''),PayJSON.Get('widget','')]);
    'select':
      begin
      Log('Browser selected widget of class %s, name %s',[PayJSON.Get('class',''),PayJSON.Get('widget','')]);
      Log('Selected widget state: '+PayJSON.Get('state',''));
      end;
    end;
end;

procedure TMainForm.DoClientCame(Sender: TObject; aClient: TIDEClient);
begin
  if FClientID>0 then
    Log('Ignoring second client (id: %d) attachment.',[aClient.ID])
  else
    begin
    FClientID:=aClient.ID;
    Caption:=FDesignCaption+Format(' [Client: %d]',[FClientID]);
    end;
end;

procedure TMainForm.DoAddWidget(Sender: TObject);

Var
  Cmd : TIDECommand;
  aName : String;

begin
  aName:=FWidgets[(Sender as TAction).Tag];
  Cmd:=TIDECommand.Create;
  Cmd.NeedsConfirmation:=True;
  Cmd.ClientID:=FClientID;
  Cmd.name:='addWidget';
  Cmd.PayLoad:=TJSONObject.Create(['class','T'+aName+'Widget']);
  FWebIDEIntf.SendCommand(cmd);
end;

procedure TMainForm.DoClientLeft(Sender: TObject; aClient: TIDEClient);
begin
  if (aClient.ID=FClientID) then
    begin
    FClientID:=-1;
    Caption:=FDesignCaption;
    end;
end;

procedure TMainForm.DoLogRequest(Sender: TObject; aURL: String);
var
  m: TLogMsg;
begin
  if FFormClosing then
    exit;
  m := TLogMsg.Create('Internal server request received: '+aURL);
  Application.QueueAsyncCall(@m.DoLog, 0);
  // Issue https://gitlab.com/freepascal.org/fpc/source/-/issues/39367
  //Application.QueueAsyncCall(@TLogMsg.Create('Internal server request received: '+FURL).DoLog, 0);
end;

procedure TMainForm.IsWidgetEnabled(Sender: TObject);
begin
  (Sender as TAction).Enabled:=(FClientID<>-1);
end;

procedure TMainForm.RegisterWidgets;
begin
  SetLength(FWidgets,9);
  FWidgetCount:=0;
  RegisterWidget('Button',2);
  RegisterWidget('CheckBoxInput',3);
  RegisterWidget('RadioInput',4);
  RegisterWidget('TextInput',5);
  RegisterWidget('Image',6);
  RegisterWidget('TextArea',7);
  RegisterWidget('Select',8);
  RegisterWidget('Container',9);
  RegisterWidget('Jumbo',10);
end;

procedure TMainForm.RegisterWidget(aWidget: String; aImageIndex: Integer);

Var
  A : TAction;
  B : TToolButton;
  L,i : Integer;

begin
  FWidgets[FWidgetCount]:=aWidget;
  A:=TAction.Create(Self);
  A.ActionList:=ALWidgets;
  A.Name:='AAdd'+aWidget;
  A.Hint:='Add '+aWidget;
  A.Caption:='Add '+aWidget;
  A.ImageIndex:=aImageIndex;
  A.Tag:=FWidgetCount;
  A.OnExecute:=@DoAddWidget;
  A.OnUpdate:=@IsWidgetEnabled;
  L:=0;
  For I:=0 to TBWidgets.ControlCount-1 do
    if TBWidgets.Controls[i].BoundsRect.Right>L then
      L:=TBWidgets.Controls[i].BoundsRect.Right;
  B:=TToolButton.Create(Self);
  B.Parent:=TBWidgets;
  B.Left:=L;
  B.Height:=32;
  B.Action:=A;
  inc(FWidgetCount);
//  TBWidgets.AddControl;

end;

procedure TMainForm.Log(Msg: String);
begin
  MLog.Lines.Add(Msg);
end;

procedure TMainForm.Log(Fmt: String; Args: array of const);
begin
  Log(Format(Fmt,Args));
end;


initialization
  {$IFDEF DARWIN}
  AddCrDelegate;
  {$ENDIF}
  if GlobalCEFApp = nil then begin
    CreateGlobalCEFApp;
    if not GlobalCEFApp.StartMainProcess then begin
      DestroyGlobalCEFApp;
      DestroyGlobalCEFWorkScheduler;
      halt(0); // exit the subprocess
    end;
  end;

finalization
  (* Destroy from this unit, which is used after "Interfaces". So this happens before the Application object is destroyed *)
  if GlobalCEFWorkScheduler <> nil then
    GlobalCEFWorkScheduler.StopScheduler;
  DestroyGlobalCEFApp;
  DestroyGlobalCEFWorkScheduler;

end.

