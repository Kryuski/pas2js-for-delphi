unit webideclient;

{$mode objfpc}

interface

uses
  Classes, SysUtils, js, web;

type
  TIDEClient = Class;

  TIDEResponseHandler = Procedure (aCode : Integer; aCodeText : String; aPayload : TJSObject) of object;

  { TIDERequest }

  TIDERequest = Class(TObject)
  Private
    FXHR : TJSXMLHttpRequest;
    FOnResponse: TIDEResponseHandler;
    Procedure ProcessResponse;
    procedure DoStateChange;
  Public
    Constructor Create(aMethod, aURl: String; aPayLoad: TJSObject; aOnResponse: TIDEResponseHandler);
  end;

  { TIDEClient }
  TCommandEvent = Procedure (Sender : TObject; aCommands : TJSArray) of object;
  TActionEvent  = Procedure (Sender : TObject; aID : nativeint; aName : String; aPayload : TJSObject) of object;

  TIDEClient = Class(TComponent)
  private
    FActionID : NativeInt;
    FOnActionResponse: TActionEvent;
    FPollID : NativeInt;
    FCommandPollInterval : Integer;
    FClientID: NativeInt;
    FIDEURL: String;
    FOnCommands: TCommandEvent;
    FLastPoll : TIDERequest;
    FStartPolling : Boolean;
    procedure DoCommandPoll;
    procedure OnActionSent(aCode: Integer; aCodeText: String; aPayload: TJSObject);
    procedure OnClientRegistered(aCode: Integer; aCodeText: String; aPayload: TJSObject);
    procedure OnCommandsReceived(aCode: Integer; aCodeText: String; aPayload: TJSObject);
  Public
    Constructor Create(aOwner : TComponent); override;
    Procedure RegisterClient;
    Procedure UnRegisterClient;
    Procedure StartCommandPolling;
    Procedure StopCommandPolling;
    Function GetNextID : NativeInt;
    procedure SendAction(Const aName : String; aPayLoad : TJSObject);
    Property IDEURL : String read FIDEURL Write FIDEURL;
    Property ClientID : nativeint read FClientID Write FClientID;
    Property CommandPollInterval : Integer Read FCommandPollInterval Write FCommandPollInterval;
    Property OnCommands : TCommandEvent Read FOnCommands Write FOnCommands;
    Property OnActionResponse : TActionEvent Read FOnActionResponse Write FOnActionResponse;
  end;

implementation

{ TIDEClient }


procedure TIDEClient.DoCommandPoll;

begin
  if Not Assigned(FLastPoll) then
    FLastPoll:=TIDERequest.Create('Get',IDEURL+'Command/'+IntToStr(ClientID)+'/',Nil,@OnCommandsReceived);
end;

procedure TIDEClient.OnActionSent(aCode: Integer; aCodeText: String; aPayload: TJSObject);

Var
  aID : NativeInt;
  aName : string;
  aActionPayload : TJSObject;

begin
  if ((aCode div 100)=2) and Assigned(aPayload) and Assigned(OnActionResponse) then
    begin
    aID:=NativeInt(aPayLoad['id']);
    aName:=String(aPayLoad['name']);
    aActionPayLoad:=TJSObject(aPayLoad['payload']);
    OnActionResponse(Self,aID,aName,aActionPayload);
    end;
end;

procedure TIDEClient.OnClientRegistered(aCode: Integer; aCodeText: String; aPayload: TJSObject);

begin
  if (aCode div 100)=2 then
    begin
    FClientID:=NativeInt(aPayload['id']);
    if FStartPolling then
      StartCommandPolling;
    end
  else
    FClientID:=0;
end;

procedure TIDEClient.OnCommandsReceived(aCode: Integer; aCodeText: String; aPayload: TJSObject);

Var
  A: TJSArray;

begin
  FLastPoll:=Nil;
  if (aCode div 100)<>2 then
    exit;
  if Assigned(aPayload) and isArray(aPayload['commands']) then
    begin
    A:=TJSArray(aPayload['commands']);
    if (A.Length>0) then
      OnCommands(Self,A);
    end;
end;

constructor TIDEClient.Create(aOwner: TComponent);
begin
  Inherited;
  FLastPoll:=Nil;
  IDEURL:='http://'+Window.location.hostname+':'+Window.location.port+'/IDE/';
end;

procedure TIDEClient.RegisterClient;

Var
  P : TJSObject;
  Req : TIDERequest;

begin
  P:=New(['url',window.locationString]);
  req:=TIDERequest.Create('POST',IDEURL+'Client',P,@OnClientRegistered);
end;

procedure TIDEClient.UnRegisterClient;

Var
  Req : TIDERequest;

begin
  Req:=TIDERequest.Create('DELETE',IDEURL+'Client/'+IntToStr(ClientID),Nil,@OnClientRegistered);
end;

procedure TIDEClient.StartCommandPolling;
begin
  if ClientID<>0 then
    FPollID:=Window.setInterval(@DoCommandPoll,FCommandPollInterval)
  else
    FStartPolling:=True;
end;

procedure TIDEClient.StopCommandPolling;
begin
  FStartPolling:=False;
  if (FPollID>0) then
    Window.clearInterval(FPollID);
end;

function TIDEClient.GetNextID: NativeInt;
begin
  Inc(FActionID);
  Result:=FActionID;
end;

procedure TIDEClient.SendAction(const aName: String; aPayLoad: TJSObject);

Var
  aAction : TJSObject;
  aID : NativeInt;
  req: TIDERequest;

begin
  aID:=GetNextID;
  aAction:=New(['id',aID,
                'name',aName,
                'payload',aPayLoad]);
  req:=TIDERequest.Create('POST',IDEURL+'Action/'+IntToStr(ClientID)+'/'+IntToStr(aID),aAction,@OnActionSent);
end;

{ TIDERequest }

procedure TIDERequest.ProcessResponse;

var
  P : TJSObject;

begin
  if ((FXHR.Status div 100)=2) and (FXHR.ResponseHeaders['Content-Type']='application/json') then
    P:=TJSJSON.parseObject(FXHR.responseText)
  else
    P:=Nil;
  if Assigned(FOnResponse) then
    FOnResponse(FXHR.Status,FXHR.StatusText,P);
end;

procedure TIDERequest.DoStateChange;
begin
  case FXHR.readyState of
    TJSXMLHttpRequest.DONE :
      begin
      if Assigned(FOnResponse) then
        ProcessResponse;
      Free;
      end;
  end;
end;

constructor TIDERequest.Create(aMethod, aURl: String; aPayLoad: TJSObject; aOnResponse: TIDEResponseHandler);

Var
  S : String;

begin
  FOnResponse:=aOnResponse;
  FXHR:=TJSXMLHttpRequest.New;
  FXHR.open(aMethod,aURL);
  if assigned(aPayload) then
    S:=TJSJSON.Stringify(aPayload)
  else
    S:='';
  FXHR.setRequestHeader('Content-Type','application/json');
  FXHR.onreadystatechange:=@DoStateChange;
  FXHR.send(S);
end;


end.

