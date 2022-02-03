program demojitsimeet;

{$mode objfpc}
{$modeswitch externalclass}

uses
  Types, TypInfo, JS, Classes, SysUtils, Web, browserapp, libjitsimeet;

Type

  { TJitsiClient }

  TJitsiClient = Class(TBrowserApplication)
  Private
    returnURL : string;
    jitsicontainer,jitsinode,options : TJSHTMLElement;
    svr,room,email,nick : TJSHTMLInputElement;
    BtnConnect : TJSHtmlButtonElement;
    BtnLeave : TJSHtmlButtonElement;
    API : TJMExternalAPI;
    GUIElements : Array[TJMUIElement] of TJSHTMLInputElement;
    procedure BindElements;
    procedure ConnectToJitsi;
    function DoConnectClick(aEvent: TJSMouseEvent): boolean;
    function DoDisConnectClick(aEvent: TJSMouseEvent): boolean;
    procedure doHangup(Arg: JSValue);
    function GetServer: String;
  Protected

    Procedure DoRun; override;
  end;

Var
  Application : TJitsiClient;

{ TJitsiClient }

function TJitsiClient.DoConnectClick(aEvent: TJSMouseEvent): boolean;

begin
  Result:=True;
  if room.value='' then
    Window.alert('No room specified')
  else
    begin
    ConnectToJitsi;
    Options.style.cssText:='display: none;';
    BtnLeave.disabled:=False;
    BtnConnect.Disabled:=True;
    end;
end;

function TJitsiClient.DoDisConnectClick(aEvent: TJSMouseEvent): boolean;
begin
  API.Hangup;
end;

procedure TJitsiClient.doHangup(Arg : JSValue);

begin
  BtnLeave.disabled:=True;
  BtnConnect.Disabled:=False;
  Options.style.cssText:='';
  jitsinode.innerHTML:='';
end;

Function TJitsiClient.GetServer : String;

begin
  Result:=svr.value;
  if Result='' then
    Result:='meet.jit.si';
end;

Procedure TJitsiClient.ConnectToJitsi;

var
  opts : TJMMeetOptions;
  user : TJMUserInfo;
  cfg : TJMInterfaceConfig;
  el : TJMUIElement;
  els : TJMUIElements;

begin
  els:=[];
  for el in TJMUIElement do
    if GUIElements[el].checked then
      Include(els,el);
  opts:=TJMMeetOptions.New;
  opts.noSSL:=False;
  opts.parentNode:=jitsinode;
  opts.roomName:=room.Value;
  if (email.value<>'') then
    begin
    user:=TJMUserInfo.New;
    user.email:=email.value;
    opts.userInfo:=user;
    end;
  cfg:=TJMInterfaceConfig.New;
  cfg.ToolbarButtons:=UIElementsStrings(Els);
  opts.interfaceConfigOverwrite:=cfg;
  API:=TJMExternalAPI.New(GetServer,opts);
  API.on_(EventReadyToClose,@DoHangup);
  if nick.value<>'' then
    API.SetDisplayName(nick.value);
end;

procedure TJitsiClient.BindElements;

Var
  E : TJMUIElement;

begin
  jitsinode:=GetHTMLElement('jitsi');
  svr:=TJSHTMLInputElement(GetHTMLElement('edtServer'));
  room:=TJSHTMLInputElement(GetHTMLElement('edtRoom'));
  email:=TJSHTMLInputElement(GetHTMLElement('edtEmail'));
  nick:=TJSHTMLInputElement(GetHTMLElement('edtNick'));
  jitsicontainer:=GetHTMLElement('jitsi-container');
  btnConnect:=TJSHtmlButtonElement(GetHTMLElement('btnConnect'));
  BtnLeave:=TJSHtmlButtonElement(GetHtmlElement('btnDisconnect'));
  options:=GetHTMLElement('Options');
  For E in TJMUIElement do
    GUIElements[E]:=TJSHTMLInputElement(GetHTMLElement(GetEnumName(TypeInfo(TJMUIElement),Ord(E))));
end;

procedure TJitsiClient.DoRun;

begin
  Terminate;
  BindElements;
  ReturnURL:=window.location.href;
  btnConnect.onclick:=@DoConnectClick;
  btnLeave.onclick:=@DoDisConnectClick;
end;


begin
  Application:=TJitsiClient.Create(Nil);
  Application.Initialize;
  Application.Run;
end.
