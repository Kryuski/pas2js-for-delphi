program demoopentok;

{$mode objfpc}

uses
  JS, Classes, SysUtils, Web, libopentok, browserapp;

Type

  { TOpenTokApplication }

  TOpenTokApplication = Class(TBrowserApplication)
    EdtAPIKey : TJSHTMLInputElement;
    EdtSession : TJSHTMLInputElement;
    EdtToken : TJSHTMLInputElement;
    EdtName : TJSHTMLInputElement;
    BtnJoin : TJSHTMLButtonElement;
    BtnLeave : TJSHTMLButtonElement;
    OptionsEl : TJSHTMLElement;
    cbPictureInPicture : TJSHTMLInputElement;
    VideosEl : TJSHTMLElement;
    PublisherEl : TJSHTMLElement;
    SubscriberEl : TJSHTMLElement;
  private
    Fsession : TOTSession;
    FPublisher : TOTPublisher;
    function CheckInput: Boolean;
    function DoJoinClick(aEvent: TJSMouseEvent): boolean;
    function DoLeaveClick(aEvent: TJSMouseEvent): boolean;
    function DoPictureInPicture(Event: TEventListenerEvent): boolean;
    procedure handleError(error: TOTError);
    procedure InitElements;
    procedure initializeSession;
  Protected
    Procedure DoRun; override;
  end;

{ TOpenTokApplication }

procedure TOpenTokApplication.InitElements;

begin
  VideosEl:=GetHTMLElement('videos');
  PublisherEl:=GetHTMLElement('publisher');
  SubscriberEl:=GetHTMLElement('subscriber');
  cbPictureInPicture:=TJSHTMLInputElement(GetHTMLElement('cbPictureInPicture'));
  EdtAPIKey :=TJSHTMLInputElement(GetHTMLElement('edtAPIKey'));
  EdtSession := TJSHTMLInputElement(GetHTMLElement('edtSession'));
  EdtToken := TJSHTMLInputElement(GetHTMLElement('edtToken'));
  EdtName := TJSHTMLInputElement(GetHTMLElement('edtName'));
  BtnJoin := TJSHTMLButtonElement(GetHTMLElement('btnJoin'));
  BtnLeave := TJSHTMLButtonElement(GetHTMLElement('btnLeave'));
  OptionsEl := GetHTMLElement('Options');
  BtnJoin.OnClick:=@DoJoinClick;
  BtnLeave.OnClick:=@DoLeaveClick;
  cbPictureInPicture.onChange:=@DoPictureInPicture;
end;

procedure TOpenTokApplication.DoRun;
begin
  Terminate;
  InitElements;
end;

Procedure TOpenTokApplication.handleError(error : TOTError);

begin
  if Assigned(error) then
    begin
    window.alert(error.message);
    writeln('Error :',error.message);
    end;
end;

function TOpenTokApplication.CheckInput : Boolean;

begin
  Result:=(EdtAPIKey.Value<>'') and (EdtSession.Value<>'') and (edtToken.Value<>'');
  if not Result then
    window.alert('Please fill in APIKey, Session and Token');

end;

function TOpenTokApplication.DoJoinClick(aEvent: TJSMouseEvent): boolean;
begin
  Result:=False;
  if Not CheckInput then exit;
  BtnJoin.disabled:=True;
  OptionsEl.style.cssText:='display: none;';
  InitializeSession;
  BtnLeave.disabled:=False;
end;

function TOpenTokApplication.DoLeaveClick(aEvent: TJSMouseEvent): boolean;
begin
  Result:=False;
  BtnJoin.disabled:=False;
  OptionsEl.style.cssText:='';
  FSession.disconnect;
  FSession:=Nil;
  FPublisher:=Nil;
  BtnLeave.disabled:=True;
end;

function TOpenTokApplication.DoPictureInPicture(Event: TEventListenerEvent): boolean;
begin
  Result:=False;
  if cbPictureInPicture.Checked then
    begin
    PublisherEl.className:='publisher';
    SubscriberEl.className:='subscriber';
    end
  else
    begin
    PublisherEl.className:='col-6';
    SubscriberEl.className:='col-6';
    end
end;

Procedure TOpenTokApplication.initializeSession;

  Procedure DoConnect(error : TOTError);

  begin
    if Assigned(error) then
      handleError(error)
    else
      FSession.publish(FPublisher, @handleError);
  end;

  procedure DoStreamCreated(event : TJSEvent);

  Var
    S : TOTStream;
    initSub : TOTInitSubscriberOptions;

  begin
    With TOTStreamEvent(event) do
      S:=Stream;
    initSub:=TOTInitSubscriberOptions.new;
    With initSub do
      begin
      insertMode:='append';
      widthstring:='100%';
      heightString:='100%';
      end;
    FSession.subscribe(S, 'subscriber', InitSub,@HandleError);
  end;

var
  initPub : TOTInitPublisherOptions;
  n : string;

begin
  Fsession:=OpenTok.initSession(edtAPIKey.Value, edtSession.Value);
  Fsession.on_('streamCreated', @doStreamCreated);
  initPub:=TOTInitPublisherOptions.New;
  initPub.insertmode:='append';
  initPub.widthString:='100%';
  initPub.heightString:='100%';
  N:=edtName.Value;
  if N='' then
    N:='You';
  initPub.Name:=N;
  Fpublisher:=OpenTok.initPublisher('publisher',initPub,@HandleError);
  // Connect to the session
  Fsession.connect(edtToken.Value, @DoConnect);
end;

begin
  With TOpenTokApplication.Create(Nil) do
    begin
    Title:='OpenTok API Demo';
    Initialize;
    Run;
    end;
end.
