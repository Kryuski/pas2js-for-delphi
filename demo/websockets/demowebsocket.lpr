program demowebsocket;

{$mode objfpc}
{$modeswitch externalclass}

uses
  browserconsole, browserapp, JS, Classes, SysUtils, Web, strutils;



type
  TServerConfig = Class External name 'Object' (TJSObject)
    url : string;
  end;

Var
  ServerConfig : TServerConfig; external name 'serverConfig';


Type
  { TMyApplication }
  TMsgKind = (mkIncoming,mkOutgoing,mkSystem);

  TMyApplication = class(TBrowserApplication)
    btnSend: TJSHTMLButtonElement;
    EdtSender : TJSHTMLInputElement;
    EdtMessage : TJSHTMLInputElement;
    EdtRecipient : TJSHTMLInputElement;
    divMessages : TJSHTMLDivElement;
    btnConnect : TJSHTMLButtonElement;
    WS : TJSWebSocket;
    procedure doRun; override;
  private
    function AppendIcon(aParent: TJSHTMLElement; aName: String): TJSHTMLElement;
    function CreateMessageEl(aKind : TMsgKind): TJSHTMLElement;
    procedure DisplayClose;
    procedure DisplayOpen;
    function DoClosed(Event: TEventListenerEvent): boolean;
    procedure doConnect;
    function DoIncomingMessage(Event: TEventListenerEvent): boolean;
    procedure DisplayMessage(Sender, Msg: String; Incoming: Boolean=True);
    function DoOpen(Event: TEventListenerEvent): boolean;
    function DoReconnect(aEvent: TJSMouseEvent): boolean;
    function DoSendClick(aEvent: TJSMouseEvent): boolean;
    function DoToggleConnectClick(aEvent: TJSMouseEvent): boolean;
    procedure SendMessage(const aRecipient, aMessage: String);
  end;



procedure TMyApplication.doRun;

begin
  edtRecipient:=TJSHTMLInputElement(GetHTMLElement('edtRecipient'));
  edtSender:=TJSHTMLInputElement(GetHTMLElement('edtSender'));
  edtMessage:=TJSHTMLInputElement(GetHTMLElement('edtMessage'));
  btnConnect:=TJSHTMLButtonElement(GetHTMLElement('btnConnect'));
  btnConnect.onclick:=@DoToggleConnectClick;
  btnSend:=TJSHTMLButtonElement(GetHTMLElement('btnSend'));
  btnSend.onclick:=@DoSendClick;
  divMessages:=TJSHTMLDivElement(GetHTMLElement('messages'));
  DoConnect;
  Terminate;
end;

procedure TMyApplication.doConnect;

Var
  URL,aHost : string;

begin
  URL:='';
  if Assigned(ServerConfig) and isString(serverConfig.URL) then
    URL:=serverConfig.URL;
  if URL='' then
    begin
    aHost:=window.location.host;
    aHost:=ExtractWord(1,aHost,[':']);
    URL:='ws://'+aHost+':8080/';
    end;
  try
    WS:=TJSWebsocket.New(url);
    WS.onmessage:=@DoIncomingMessage;
    WS.onclose:=@DoClosed;
    WS.onopen:=@DoOpen;
  except
    on TJSError do
      Window.Alert('Could not connect to websocket server at '+URL);
  end;
end;

function TMyApplication.DoOpen(Event: TEventListenerEvent): boolean;
begin
  btnSend.disabled:=False;
  DisplayOpen;
  btnConnect.InnerText:='Disconnect';
end;

function TMyApplication.DoClosed(Event: TEventListenerEvent): boolean;
begin
  btnSend.disabled:=true;
  btnConnect.InnerText:='Connect';
  DisplayClose;
end;

function TMyApplication.DoIncomingMessage(Event: TEventListenerEvent): boolean;

Var
  Msg: TJSMessageEvent absolute Event;
  JS : TJSObject;

begin
   if isString(Msg.Data) then
     begin
     JS:=TJSJSON.parseObject(String(Msg.Data));
     DisplayMessage(String(JS['from']),String(JS['msg']),True)
     end
   else
     DisplayMessage('','<<unknown data arrived>>',True);
end;

function TMyApplication.CreateMessageEl(aKind : TMsgKind): TJSHTMLElement;

Var
  ImgDiv,ImgEl,msgEl : TJSHTMLElement;

begin
  MsgEl:=TJSHTMLElement(Document.createElement('DIV'));
  if aKind=mkSystem then
    begin
    MsgEl.className:='text-center my-2';
    end
  else
    begin
    msgEl.className:='d-flex align-items-center'+Ifthen(aKind=mkIncoming,'',' text-right justify-content-end');
    ImgDiv:=TJSHTMLElement(Document.createElement('DIV'));
    ImgDiv.ClassName:='text-left pr-1';
    ImgEl:=TJSHTMLElement(Document.createElement('IMG'));
    ImgEl['Src']:=IfThen(aKind=mkIncoming,'guest','you')+'.png';
    ImgDiv.AppendChild(ImgEl);
    if aKind=mkIncoming then
      msgEl.AppendChild(ImgDiv);
    end;
  Result:=TJSHTMLElement(Document.createElement(IfThen(aKind=mkSystem,'span','div')));
  if aKind=mkSystem then
    Result.className:='between'
  else
    Result.className:='pr-2'+IfThen(aKind=mkIncoming,' pl-1','');
  msgEl.AppendChild(Result);
  if aKind=mkOutgoing then
    msgEl.AppendChild(ImgDiv);
  divMessages.appendChild(msgEl);
  msgEl.scrollIntoView;
end;

function TMyApplication.AppendIcon(aParent: TJSHTMLElement; aName: String
  ): TJSHTMLElement;

begin
  Result:=TJSHTMLElement(Document.createElement('i'));
  Result.className:='fas fa-'+aName+' mr-3';
  aParent.AppendChild(Result);
end;

procedure TMyApplication.DisplayClose;

Var
  iEl,pEl : TJSHTMLElement;

begin
  Pel:=CreateMessageEl(mkSystem);
  iEl:=AppendIcon(Pel,'plug');
  iEl.onclick:=@DoReconnect;
  pEl.AppendChild(Document.createTextNode('Connection closed, click icon to reconnect'));
end;

procedure TMyApplication.DisplayOpen;

Var
  pEl : TJSHTMLElement;

begin
  Pel:=CreateMessageEl(mkSystem);
  AppendIcon(Pel,'link');
  pEl.AppendChild(Document.createTextNode('Connection open, you can start messaging'));
end;



procedure TMyApplication.DisplayMessage(Sender,Msg: String; Incoming: Boolean = True);

Const
   kinds : Array[Boolean] of TMsgKind = (mkOutgoing,mkIncoming);

Var
  pEl,pEl2 : TJSHTMLElement;

begin
  pEl:=CreateMessageEl(Kinds[Incoming]);
  if (Sender<>'') then
    begin
    pEl2:=TJSHTMLElement(Document.createElement('span'));
    pEl2.className:='name';
    pEl2.innerText:=Sender;
    pEl.appendChild(pEl2);
    end;
  pEl2:=TJSHTMLElement(Document.createElement('p'));
  PEL2.className:='msg';
  PEL2.AppendChild(Document.createTextNode(Msg));
  pEl.AppendChild(pEL2);
end;

function TMyApplication.DoReconnect(aEvent: TJSMouseEvent): boolean;
begin
  DoConnect;
end;

procedure TMyApplication.SendMessage(const aRecipient,aMessage: String);

Var
  JS : TJSObject;

begin
  JS:=New(['from',EdtSender.Value,'msg',aMessage,'to',aRecipient]);
  WS.send(TJSJSON.Stringify(JS));
  DisplayMessage('you',aMessage,False);
end;

function TMyApplication.DoSendClick(aEvent: TJSMouseEvent): boolean;
begin
  SendMessage(EDTRecipient.Value, EdtMessage.Value);
end;

function TMyApplication.DoToggleConnectClick(aEvent: TJSMouseEvent): boolean;
begin
  if btnConnect.InnerText='Connect' then
    doConnect
  else
    begin
    WS.close;
    WS:=nil;
    end;
end;

var
  Application : TMyApplication;

begin
  Application:=TMyApplication.Create(nil);
  Application.Initialize;
  Application.Run;
end.
