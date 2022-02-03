unit webideintf;

{$mode objfpc}{$H+}

interface

uses
  fpMimeTypes, Classes, SysUtils, StrUtils, httpdefs, fphttpclient,custhttpapp, fpjson, jsonparser, httproute;

Const
  SFilesURL = '/Project/';
  SIDEURL = '/IDE/';

Type
  TClientObject = Class(TObject)
  Private
    FID: Int64;
  public
    Procedure FromJSON(aJSON : TJSONObject); virtual; abstract;
    Procedure ToJSON(aJSON : TJSONObject); virtual; abstract;
    Property ID : Int64 Read FID Write FID;
  end;
  { TIDEClient }

  TIDEClient = Class(TClientObject)
  private
    FURL: String;
  Public
    Procedure FromJSON(aJSON : TJSONObject); override;
    Procedure ToJSON(aJSON : TJSONObject); override;
    Property URL : String Read FURL Write FURL;
  end;
  { TIDEExchange }

  TIDEExchange = Class(TClientObject)
  private
    FClientID: Int64;
    FName: String;
    FPayLoad: TJSONData;
  Public
    Destructor Destroy; override;
    Procedure FromJSON(aJSON : TJSONObject); override;
    Procedure ToJSON(aJSON : TJSONObject); override;
    Property ClientID : Int64 Read FClientID Write FClientID;
    Property Name : String Read FName Write FName;
    Property PayLoad : TJSONData Read FPayLoad Write FPayLoad;
  end;

  TIDEAction = Class(TIDEExchange)
  end;

  { TClientObjectList }

  TClientObjectList = Class(TThreadList)
  Public
    Function FindID(aID : int64) : TClientObject;
  end;

  { TIDECommand }

  TIDECommand = Class(TIDEExchange)
  private
    FConfirmed: Boolean;
    FNeedsConfirmation: Boolean;
    FSent: Boolean;
  Public
    Property NeedsConfirmation : Boolean Read FNeedsConfirmation Write FNeedsConfirmation;
    Property Sent : Boolean Read FSent Write FSent;
    Property Confirmed : Boolean Read FConfirmed Write FConfirmed;
  end;

  { TIDEThread }

  TIDEThread = Class(TThread)
  Private
    FHandler : TFPHTTPServerHandler;
    FExceptionClass : String;
    FExceptionMessage : String;
  Public
    Constructor Create(aHandler : TFPHTTPServerHandler);
    Procedure Execute; override;
  end;


  TIDENotification = Procedure(Sender : TObject; aExchange : TIDEExchange) of object;
  TIDEClientNotification = Procedure(Sender : TObject; aClient : TIDEClient) of object;
  TIDERequestNotification = Procedure(Sender : TObject; aURL : String) of object;

  { TIDEServer }

  TIDEServer = Class(TComponent)
  private
    FOnRequest: TIDERequestNotification;
    FQuitting : Boolean;
    FClients,
    FCommands,
    FActions : TClientObjectList;
    FIDCounter: Int64;
    FOnAction: TIDENotification;
    FOnClient: TIDEClientNotification;
    FOnClientRemoved: TIDEClientNotification;
    FOnConfirmCommand: TIDENotification;
    FProjectDir: String;
    FWebHandler : TFPHTTPServerHandler;
    FThread : TIDEThread;
    FLastAction : TIDEAction;
    FLastCommand : TIDECommand;
    FLastClient : TIDEClient;
    function CheckClient(aRequest: TRequest): INt64;
    procedure DeActivatedThread(Sender: TObject);
    function Do404(is404: boolean; aResponse: TResponse): Boolean;
    procedure DoEvent(aProc: TThreadMethod);
    procedure DoQuit(ARequest: TRequest; AResponse: TResponse);
    procedure DoRouteRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse);
    function GetAction(Index : Integer): TIDEAction;
    function GetActionCount: Integer;
    function GetPort: Integer;
    function GetActive: Boolean;
    procedure SetActive(AValue: Boolean);
    procedure SetPort(AValue: Integer);
    procedure SetProjectDir(AValue: String);
  Protected
    procedure RegisterRoutes; virtual;
    // HTTP request extraction
    procedure GetClientObjectFromRequest(ARequest: TRequest; AObject: TClientObject);
    function GetActionFromRequest(ARequest: TRequest): TIDEAction;
    function GetCommandFromRequest(ARequest: TRequest): TIDECommand;
    function GetClientFromRequest(ARequest: TRequest): TIDEClient;
    function GetJSONFromRequest(ARequest: TRequest): TJSONObject;
    // Sending responses
    procedure SendClientObjectResponse(AObject: TClientObject; AResponse: TResponse);
    Procedure SendJSONResponse(aJSON : TJSONObject; aResponse : TResponse);
    // HTTP route handlers
    procedure DoDeleteAction(ARequest: TRequest; AResponse: TResponse); virtual;
    procedure DoDeleteClient(ARequest: TRequest; AResponse: TResponse); virtual;
    procedure DoGetCommand(ARequest: TRequest; AResponse: TResponse);virtual;
    procedure DoGetFile(ARequest: TRequest; AResponse: TResponse);virtual;
    procedure DoPostAction(ARequest: TRequest; AResponse: TResponse);virtual;
    procedure DoPostClient(ARequest: TRequest; AResponse: TResponse);virtual;
    procedure DoPutCommand(ARequest: TRequest; AResponse: TResponse);virtual;
    // Event handler synchronisation. Rework this to objects
    Procedure DoOnAction;
    Procedure DoOnConfirmCommand;
    Procedure DoOnClientAdded;
    Procedure DoOnClientRemoved;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Function GetNextCounter : Int64;
    // Public API to communicate with browser
    Function SendCommand(aCommand : TIDECommand) : Int64;
    Procedure GetClientActions(aClientID : Int64; aList : TFPList);
    Function DeleteAction(aID: Int64; Const aClientID : Int64 = -1): Boolean;
    // Public properties
    Property ProjectDir : String Read FProjectDir Write SetProjectDir;
    Property Port : Integer Read GetPort Write SetPort;
    Property Active : Boolean read GetActive write SetActive;
    Property ActionCount : Integer Read GetActionCount;
    Property Action[Index : Integer] : TIDEAction Read GetAction;
    // Events
    Property OnRequest : TIDERequestNotification Read FOnRequest Write FOnRequest;
    Property OnConfirmCommand : TIDENotification Read FOnConfirmCommand Write FOnConfirmCommand;
    Property OnAction : TIDENotification Read FOnAction Write FOnAction;
    Property OnClientAdded : TIDEClientNotification Read FOnClient Write FOnClient;
    Property OnClientRemoved : TIDEClientNotification Read FOnClientRemoved Write FOnClientRemoved;
  end;

implementation

{ TClientObjectList }


function TClientObjectList.FindID(aID: int64): TClientObject;

Var
  L : TList;
  I : integer;

begin
  Result:=Nil;
  L:=LockList;
  try
    I:=L.Count-1;
    While (Result=Nil) and (I>=0) do
      begin
      Result:=TClientObject(L[i]);
      if Result.ID<>aID then
        Result:=nil;
      Dec(I);
      end;
  finally
    UnlockList;
  end;
end;

{ TIDEClient }

procedure TIDEClient.FromJSON(aJSON: TJSONObject);
begin
  FID:=aJSON.Get('id',Int64(-1));
  FURL:=aJSON.Get('url','');
end;

procedure TIDEClient.ToJSON(aJSON: TJSONObject);
begin
  aJSON.Add('id',ID);
  aJSON.Add('url',url);
end;

{ TIDEExchange }

destructor TIDEExchange.Destroy;
begin
  FreeAndNil(FPayload);
  Inherited;
end;

procedure TIDEExchange.FromJSON(aJSON: TJSONObject);

Var
  P : TJSONObject;

begin
  ID:=aJSON.Get('id',Int64(0));
  Name:=aJSON.Get('name','');
  P:=aJSON.Get('payload',TJSONObject(Nil));
  if Assigned(P) then
    Payload:=aJSON.Extract('payload');
end;

procedure TIDEExchange.ToJSON(aJSON: TJSONObject);
begin
  aJSON.Add('id',ID);
  aJSON.Add('name',name);
  if Assigned(Payload) then
    aJSON.Add('payload',Payload.Clone);
end;

{ TIDEThread }


constructor TIDEThread.Create(aHandler: TFPHTTPServerHandler);
begin
  FHandler:=AHandler;
  FreeOnTerminate:=True;
  Inherited Create(False);
end;

procedure TIDEThread.Execute;
begin
  try
    FHandler.Run;
    FHandler:=nil;
  except
    On E : Exception do
      begin
      FExceptionClass:=E.ClassName;
      FExceptionMessage:=E.Message;
      end;
  end;
end;

{ TIDEServer }

function TIDEServer.GetAction(Index : Integer): TIDEAction;

Var
  L : TList;

begin
  L:=FActions.LockList;
  try
    Result:=TIDEAction(L.Items[Index]);
  finally
    FActions.UnlockList;
  end;
end;

procedure TIDEServer.DeActivatedThread(Sender: TObject);
begin
  FThread:=Nil;
end;

function TIDEServer.GetActionCount: Integer;

Var
  L : TList;

begin
  L:=FActions.LockList;
  try
    Result:=L.Count;
  finally
    FActions.UnlockList;
  end;
end;

function TIDEServer.GetActive: Boolean;
begin
  Result:=Assigned(FThread);
end;

function TIDEServer.GetPort: Integer;
begin
  Result:=FWebHandler.Port;
end;

procedure TIDEServer.SetActive(AValue: Boolean);
begin
  if Active=AValue then Exit;
  if AValue then
    begin
    FThread:=TIDEThread.Create(FWebHandler);
    FThread.OnTerminate:=@DeActivatedThread;
    end
  else
    begin
    FWebHandler.Terminate; // will cause thread to stop.
    try
      // Send a Quit request just in case. Normally this should fail.
      FQuitting:=True;
      TFPHTTPClient.SimpleGet(Format('http://localhost:%d/Quit',[Port]));
    except
      FQuitting:=False;
    end;
    end;
end;

procedure TIDEServer.SetPort(AValue: Integer);
begin
  FWebHandler.Port:=aValue;
end;

procedure TIDEServer.SetProjectDir(AValue: String);
begin
  if FProjectDir=AValue then Exit;
  FProjectDir:=IncludeTrailingPathDelimiter(AValue);
end;

procedure TIDEServer.DoOnAction;
begin
  If Assigned(FOnAction) then
    FonAction(Self,FLastAction);
  FLastAction:=Nil;
end;

procedure TIDEServer.DoOnConfirmCommand;
begin
  If Assigned(FOnAction) then
    FonAction(Self,FLastCommand);
  FLastCommand:=Nil;
end;

procedure TIDEServer.DoOnClientAdded;
begin
  if Assigned(FOnClient) then
    FOnClient(Self,FLastClient);
  FLastClient:=Nil;
end;

procedure TIDEServer.DoOnClientRemoved;
begin
  if Assigned(FOnClientRemoved) then
    FOnClientRemoved(Self,FLastClient);
  FLastClient:=Nil;
end;

procedure TIDEServer.DoGetCommand(ARequest: TRequest; AResponse: TResponse);

Var
  L : TList;
  I : integer;
  J,C : TJSONObject;
  A :TJSONArray;
  Cmd : TIDECommand;
  L2 : TFPList;
  aClient : Int64;

begin
  aClient:=CheckClient(aRequest);
  J:=nil;
  A:=nil;
  L:=FCommands.LockList;
  try
    L2:=TFPList.Create;
    J:=TJSONObject.Create;
    A:=TJSONArray.Create;
    J.Add('commands',A);
    For I:=0 to L.Count-1 do
      begin
      CMD:=TIDECommand(L[i]);
      if Not Cmd.Sent and (Cmd.ClientID=aClient) then
        begin
        C:=TJSONObject.Create;
        Cmd.ToJSON(C);
        A.Add(C);
        L2.Add(CMD);
        end;
      end;
    SendJSONResponse(J,aResponse);
    // Remove sent from list
    for I:=0 to L2.Count-1 do
      begin
      Cmd:=TIDECommand(L2[i]);
      if Cmd.NeedsConfirmation then
        Cmd.Sent:=True
      else
        begin
        Cmd.Free;
        L.Remove(Cmd);
        end;
      end;
  finally
    J.Free;
    FCommands.UnLockList;
    l2.Free;
  end;
end;

procedure TIDEServer.DoPutCommand(ARequest: TRequest; AResponse: TResponse);

Var
  cmd,oCmd : TIDECommand;
  aID,aClient : Int64;

begin
  aClient:=CheckClient(aRequest);
  aID:=StrToIntDef(aRequest.RouteParams['ID'],-1);
  cmd:=TIDECommand.Create;
  try
    GetClientObjectFromRequest(aRequest,Cmd);
    cmd.ClientID:=aClient;
    oCmd:=TIDECommand(FCommands.FindID(aID));
    if Do404((oCmd=Nil) or (oCmd.ClientID<>aClient),aResponse) then
       exit;
    // Later on we can add more modifications
    oCmd.Confirmed:=True;
    aResponse.Code:=204;
    aResponse.CodeText:='OK';
    aResponse.SendResponse;
    FLastCommand:=oCmd;
    DoEvent(@DoOnConfirmCommand);
    FCommands.Remove(oCmd);
  Finally
    cmd.Free;
  end;
end;

procedure TIDEServer.DoQuit(ARequest: TRequest; AResponse: TResponse);
begin
  if FQuitting then
    aResponse.Code:=200
  else
    aResponse.Code:=401;
  aResponse.SendResponse;
end;

procedure TIDEServer.DoRouteRequest(Sender: TObject; ARequest: TRequest; AResponse: TResponse);
begin
  If Assigned(FonRequest) then
    FOnRequest(Self,aRequest.URI);
end;

function TIDEServer.GetJSONFromRequest(ARequest: TRequest): TJSONObject;

var
  D : TJSONData;

begin
  if ARequest.ContentType<>'application/json' then
    Raise Exception.Create('Not valid JSON payload: content type must be application/json');
  D:=GetJSON(ARequest.Content);
  if Not (D is TJSONObject) then
    begin
    FreeAndNil(D);
    Raise EJSON.Create('Payload is valid JSON but not a JSON object');
    end;
  Result:=D as TJSONObject;
end;

procedure TIDEServer.SendJSONResponse(aJSON: TJSONObject; aResponse: TResponse);

Var
  JS : TJSONStringType;

begin
  JS:=aJSON.AsJSON;
  aResponse.FreeContentStream:=True;
  aResponse.ContentStream:=TMemoryStream.Create;
  aResponse.ContentStream.WriteBuffer(JS[1],Length(JS));
  aResponse.ContentLength:=Length(JS);
  aResponse.ContentType:='application/json';
  aResponse.SendResponse;
end;

procedure TIDEServer.GetClientObjectFromRequest(ARequest: TRequest; AObject: TClientObject);

Var
  J : TJSONObject;

begin
  J:=GetJSONFromRequest(aRequest);
  try
    AObject.FromJSON(J);
  finally
    J.Free;
  end;
end;

procedure TIDEServer.SendClientObjectResponse(AObject: TClientObject; AResponse: TResponse);

Var
  J : TJSONObject;

begin
  J:=TJSONObject.Create;
  try
    aObject.ToJSON(J);
    SendJSONResponse(J,aResponse);
  finally
    J.Free;
  end;
end;

function TIDEServer.GetActionFromRequest(ARequest: TRequest): TIDEAction;

begin
  Result:=TIDEAction.Create;
  try
    GetClientObjectFromRequest(aRequest,Result);
  except
    Result.Free;
    raise;
  end;
end;

function TIDEServer.GetCommandFromRequest(ARequest: TRequest): TIDECommand;

begin
  Result:=TIDECommand.Create;
  try
    GetClientObjectFromRequest(aRequest,Result);
  except
    Result.Free;
    Raise;
  end;
end;

function TIDEServer.GetClientFromRequest(ARequest: TRequest): TIDEClient;
begin
  Result:=TIDEClient.Create;
  try
    GetClientObjectFromRequest(aRequest,Result);
  except
    Result.Free;
    Raise;
  end;
end;

procedure TIDEServer.DoPostAction(ARequest: TRequest; AResponse: TResponse);

var
  A : TIDEAction;
  aId,aClient : Int64;

begin
  aClient:=CheckClient(aRequest);
  aID:=StrToInt64Def(aRequest.RouteParams['ID'],-1);
  Try
    A:=GetACtionFromRequest(aRequest);
    A.ClientID:=aClient;
    if A.ID=0 then
      a.ID:=aID;
    FActions.Add(A);
    FLastAction:=A;
    DoEvent(@DoOnAction);
    AResponse.Code:=201;
    AResponse.Codetext:='Created';
  except
    On E: Exception do
      begin
      AResponse.Code:=400;
      AResponse.Codetext:='Invalid Param';
      AResponse.Content:='Invalid data ('+E.ClassName+'): '+E.Message;
      end;
  end;
  aResponse.SendResponse;
end;

function TIDEServer.CheckClient(aRequest: TRequest): INt64;

Var
  S : String;

begin
  S:=ARequest.RouteParams['Client'];
  if (S='') then
    Raise EJSON.Create('Missing client ID in request');
  if Not TryStrToInt64(S,Result) then
    Raise EJSON.CreateFmt('Invalid client ID: %s',[S]);
end;

procedure TIDEServer.DoDeleteAction(ARequest: TRequest; AResponse: TResponse);

var
  SID : String;
  ID,aClient : Int64;

begin
  Try
    aClient:=CheckClient(ARequest);
    SID:=ARequest.RouteParams['ID'];
    ID:=StrtoInt64Def(SID,-1);
    if Do404((ID=-1) or not (DeleteAction(ID,aClient)),aResponse) then
      exit;
    AResponse.Code:=204;
    AResponse.Codetext:='No content';
    aResponse.SendResponse;
  except
    On E: Exception do
      begin
      AResponse.Code:=400;
      AResponse.Codetext:='Invalid Param';
      AResponse.Content:='Invalid data ('+E.ClassName+'): '+E.Message;
      end;
  end;
end;


procedure TIDEServer.DoGetFile(ARequest: TRequest; AResponse: TResponse);

Var
  FN : String;

begin
  FN:=ARequest.URL;
  if AnsiStartsText(SFilesURL,FN) then
    Delete(FN,1,Length(SFilesURL));
  FN:=ExpandFileName(FProjectDir+FN);
  if Pos('..',ExtractRelativepath(FProjectDir,FN))<>0 then
    begin
    aResponse.Code:=401;
    aResponse.CodeText:='Forbidden';
    aResponse.Content:='<H1>Forbidden</H1>';
    end
  else if Do404(Not FileExists(FN),aResponse) then
    exit;
  aResponse.FreeContentStream:=True;
  aResponse.ContentStream:=TFileStream.Create(FN,fmOpenRead or fmShareDenyWrite);
  aResponse.ContentLength:=aResponse.ContentStream.Size;
  aResponse.ContentType:=MimeTypes.GetMimeType(ExtractFileExt(FN));
  if aResponse.ContentType='' then
    aResponse.ContentType:='text/html';
  aResponse.SendResponse;
end;


constructor TIDEServer.Create(aOwner: TComponent);
begin
  Inherited;
  FProjectDir:=ExtractFilePath(Paramstr(0));
  FActions:=TClientObjectList.Create;
  FCommands:=TClientObjectList.Create;
  FClients:=TClientObjectList.Create;
  FWebHandler:=TFPHTTPServerHandler.Create(Self);
  FWebHandler.Port:=8080;
  RegisterRoutes;
end;

procedure TIDEServer.DoEvent(aProc : TThreadMethod);

begin
  if Assigned(FThread) then
    FThread.Synchronize(aProc)
  else
    aProc;
end;

procedure TIDEServer.DoPostClient(ARequest: TRequest; AResponse: TResponse);

Var
  aClient : TIDEClient;

begin
  aClient:=GetClientFromRequest(aRequest);
  aClient.FID:=GetNextCounter;
  FClients.Add(aClient);
  SendClientObjectResponse(aClient,aResponse);
  FLastClient:=aClient;
  DoEvent(@DoOnClientAdded);
end;

function TIDEServer.Do404(is404: boolean; aResponse: TResponse): Boolean;

begin
  Result:=is404;
  if Result then
    begin
    aResponse.Code:=404;
    aResponse.Codetext:='Not found';
    aResponse.SendResponse;
    end;
end;

procedure TIDEServer.DoDeleteClient(ARequest: TRequest; AResponse: TResponse);

Var
  aClientID : Int64;
  aClient : TIDEClient;

begin
  aClientID:=CheckClient(aRequest);
  aClient:=TIDEClient(FClients.FindID(aClientID));
  if Do404(not Assigned(aClient),aResponse) then
    exit;
  FLastClient:=aClient;
  DoEvent(@DoOnClientRemoved);
  FClients.Remove(aClient);
end;


procedure TIDEServer.RegisterRoutes;

begin
  // get command
  HTTPRouter.RegisterRoute(SIDEURL+'Quit',rmGet,@DoQuit);
  HTTPRouter.RegisterRoute(SIDEURL+'Client/',rmPost,@DoPostClient);
  HTTPRouter.RegisterRoute(SIDEURL+'Client/:Client',rmDelete,@DoDeleteClient);
  HTTPRouter.RegisterRoute(SIDEURL+'Command/:Client/',rmGet,@DoGetCommand);
  // PUT command for confirm.
  HTTPRouter.RegisterRoute(SIDEURL+'Command/:Client/:ID',rmPut,@DoPutCommand);
  // POST action
  HTTPRouter.RegisterRoute(SIDEURL+'Action/:Client/:ID',rmPost,@DoPostAction);
  HTTPRouter.RegisterRoute(SIDEURL+'Action/:Client/:ID',rmDelete,@DoDeleteAction);
  // GET file
  HTTPRouter.RegisterRoute(SFilesURL+'*',rmGet,@DoGetFile,true);
  HTTPRouter.BeforeRequest:=@DoRouteRequest;
end;

destructor TIDEServer.Destroy;
begin
  Active:=False;
  While Active do
    Sleep(20);
  FreeAndNil(FActions);
  FreeAndNil(FCommands);
  FreeAndNil(FClients);
  inherited Destroy;
end;

function TIDEServer.GetNextCounter: Int64;
begin
  Inc(FIDCounter);
  Result:=FIDCounter;
end;

function TIDEServer.SendCommand(aCommand: TIDECommand): Int64;
begin
  Result:=GetNextCounter;
  aCommand.ID:=Result;
  FCommands.Add(aCommand);
end;

function TIDEServer.DeleteAction(aID: Int64; const aClientID: Int64): Boolean;

Var
  P : TIDEAction;
  L : TList;
  I : Integer;

begin
  P:=nil;
  L:=FActions.LockList;
  try
    I:=L.Count-1;
    While (I>=0) and (P=Nil) do
      begin
      P:=TIDEAction(L[i]);
      if P.ID<>AID then P:=Nil;
      Dec(i)
      end;
  finally
    L.Free;
  end;
  Result:=(P<>Nil) and ((aClientID=-1) or (P.ClientID=aClientID));
  if Result then
    FActions.Remove(P);
end;

procedure TIDEServer.GetClientActions(aClientID: Int64; aList: TFPList);

Var
  P : TIDEAction;
  L : TList;
  I : Integer;
begin
  P:=nil;
  L:=FActions.LockList;
  try
    I:=L.Count-1;
    While (I>=0) and (P=Nil) do
      begin
      P:=TIDEAction(L[i]);
      if P.ClientID=aClientID then
        begin
        aList.Add(P);
        L.Delete(I);
        end;
      Dec(i);
      end;
  finally
    L.Free;
  end;
end;

end.

