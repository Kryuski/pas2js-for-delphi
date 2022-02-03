unit fprpcclient;

{$mode ObjFPC}
{$modeswitch advancedrecords}

interface

uses
  Classes, SysUtils, JS;

Const
  DefaultJSONRPCversion = '2.0';

Type
  ERPCClient = Class(Exception);

  { TRPCRequestParamsBuilder }

  TRPCRequestParamsBuilder = class
  Protected
    Procedure DoAddArg(const aName : String; aValue : JSValue); virtual; abstract;
    Function DoGetArgs : JSValue; virtual; abstract;
  Public
    Procedure AddArg(const aName : string; aValue : NativeInt);
    Procedure AddArg(const aName : string; aValue : String);
    Procedure AddArg(const aName : string; aValue : Boolean);
    Procedure AddArg(const aName : string; aValue : Double);
    Procedure AddArg(const aName : string; aValue : TJSArray);
    Procedure AddArg(const aName : string; aValue : TJSObject);
  end;

  { TRPCArrayRequestParamsBuilder }

  TRPCArrayRequestParamsBuilder = class (TRPCRequestParamsBuilder)
  private
    FParams: TJSArray;
  Protected
    Function DoGetArgs : JSValue; override;
    Procedure DoAddArg(const aName : String; aValue : JSValue); override;
  Public
    Constructor Create(aParams : TJSArray);
    Property Params : TJSArray Read FParams;
  end;

  { TRPCObjectRequestParamsBuilder }

  TRPCObjectRequestParamsBuilder = class (TRPCRequestParamsBuilder)
  private
    FParams: TJSObject;
  Protected
    Procedure DoAddArg(const aName : String; aValue : JSValue); override;
    Function DoGetArgs : JSValue; override;
  Public
    Constructor Create(aParams : TJSObject);
    Property Params : TJSObject Read FParams;
  end;

  { TRPCError }

  TRPCError = record
    ID : NativeInt;
    Code : NativeInt;
    Message : String;
    ErrorClass : String;
    Procedure FromValue(Err : JSValue);
  end;

  { TRPCResponse }

  TRPCResponse = Record
    isOK : Boolean;
    ID : NativeInt;
    Error : TRPCError;
    HasError : Boolean;
    Result : JSValue;
    Version : String;
    Procedure FromObject(Obj : TJSObject);
  end;

  TRPCFailureCallBack = reference to Procedure (Sender : TObject; const aError : TRPCError);
  TRPCResultCallBack = reference to Procedure (Sender : TObject; const aResult : JSValue);
  TRPCUnexpectedErrorCallback = Procedure (Sender : TObject; Const aStage : String; E : Exception) of object;

  TRPCOption = (roParamsAsObject,roFullMethodName,roUseBatch,roAutoBatch,roForceArray);
  TRPCOptions = Set of TRPCOption;

  TRPCRequest = Record
    IsNotification : Boolean;
    ClassName : String;
    MethodName : String;
    ID : NativeInt;
    Params : JSValue;
    OnFailure : TRPCFailureCallBack;
    OnSuccess : TRPCResultCallBack;
  end;

  { TRPCBatch }

  TRPCBatch = Record
    Requests : Array of TRPCRequest;
    ID : NativeInt;
    Function GetRequest(aID : NativeInt; DoRemove : Boolean) : TRPCRequest;
  end;

  TRPCConfigRequest = procedure (sender : TObject; aConfig : TJSObject) of object;
  TRPCHeadersRequest = procedure (sender : TObject; aHeaders : TStrings) of object;

  { TRPCClient }
  TRPCClient = Class(TComponent)
  private
    FBatchTimeout: Integer;
    FCustomHeaders: TStrings;
    FJSONRPCversion: String;
    FOnConfigRequest: TRPCConfigRequest;
    FOnCustomHeaders: TRPCHeadersRequest;
    FOnUnexpectedError: TRPCUnexpectedErrorCallback;
    FOptions: TRPCoptions;
    FPendingBatches : TJSObject;
    FURL: String;
    FBatch : TRPCBatch;
    FCurrentBatchTimeout : Integer;
    FRequestID : NativeInt;
    FBatchID: NativeInt;
    procedure SetCustomHeaders(AValue: TStrings);
    procedure SetOptions(AValue: TRPCoptions);
    procedure SetURL(AValue: String);
  Protected
    // Handle unexpected error during callbacks
    procedure HandleUnexpectedError(const aStage: String; E: Exception);
    // Find batch with ID equal to aBatch in list of pending batches. If DoRemove, the record is removed from the list.
    function GetBatch(aBatchID: NativeInt; DoRemove: Boolean): TRPCBatch;
    // Convert JS value to TRPCError. Calls TRPCError.FromValue
    function ValueToError(Err: JSValue): TRPCError; virtual;
    // Convert JS object to TRPResponse. Calls TRPCResponse.FromObject
    function ResponseFromObject(aObj: TJSObject): TRPCResponse; virtual;
    // Remove batch from pending batches, calling each OnFailure with aError
    procedure RemoveFromPending(aBatchID: NativeInt; aError: TRPCError); virtual; overload;
    // Depending on results, call requests handlers with result/error.  Remove batch from pending batches if batch is empty.
    procedure RemoveFromPending(aBatchID: NativeInt; Res: TJSArray); virtual; overload;
    // Collect headers for request.
    procedure GetHeaders(Headers : TStrings); virtual;
    // Send batch to server
    procedure DoSendBatch(aBatch: TRPCBatch); virtual;
    // Configure FETCH request init object
    procedure ConfigRequest(init: TJSObject); virtual;
    // Start new request batch. If current batch was not empty, sends it first.
    Procedure StartRequestBatch; virtual;
    // Send started request batch. Will stop timer.
    Procedure SendRequestBatch; virtual;
    // Add request to current batch. Starts timer if roAutoBatch is in options and the batch is empty.
    Procedure AddToRequestBatch(aRequest : TRPCRequest); virtual;
    // Overload for ease of use.
    Function AddToRequestBatch(aID : NativeInt; const aClassName,aMethodName : String; aParams : JSValue; aOnSuccess : TRPCResultCallBack; aOnFailure: TRPCFailureCallBack) : TRPCRequest; virtual;
    // perform HTTP request.
    procedure DoSendHTTPRequest(const aJSON: String; aBatchID : NativeInt); virtual;
    // For use in service
    Function DoExecuteRequest(const aClassName,aMethodName : String; aParams : JSValue; aOnSuccess : TRPCResultCallBack = Nil; aOnFailure: TRPCFailureCallBack = nil) : NativeInt;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    // you are responsible for freeing the request params builder.
    Function CreateRequestParamsBuilder : TRPCRequestParamsBuilder;
    // Execute a request. Params can be passed as object or array
    Function ExecuteRequest(const aClassName,aMethodName : String; aParams : TJSArray; aOnSuccess : TRPCResultCallBack = Nil; aOnFailure: TRPCFailureCallBack = nil) : NativeInt;
    Function ExecuteRequest(const aClassName,aMethodName : String; aParams : TJSObject; aOnSuccess : TRPCResultCallBack = Nil; aOnFailure: TRPCFailureCallBack = nil) : NativeInt;
    // Close current batch
    Procedure CloseBatch;
  Published
    // URL for RPC server.
    Property URL : String Read FURL Write SetURL;
    // Options.
    Property Options : TRPCoptions Read FOptions Write SetOptions;
    // If roAutoBatch is in options, this is the timeout between first request and the time the batch is created and sent. Default 100 ms.
    Property BatchTimeout: Integer Read FBatchTimeout Write FBatchTimeout;
    // JSON RPC version to send, default: 2.0
    Property JSONRPCversion : String Read FJSONRPCversion Write FJSONRPCversion;
    // Custom headers to be sent with each request. NameValueSeparator is colon (:) so add Name:value
    Property CustomHeaders : TStrings Read FCustomHeaders Write SetCustomHeaders;
    // Called when configuring a FETCH request
    Property OnConfigRequest : TRPCConfigRequest Read FOnConfigRequest Write FOnConfigRequest;
    // Called when collecting headers for a request.
    Property OnCustomHeaders : TRPCHeadersRequest Read FOnCustomHeaders Write FOnCustomHeaders;
    // Called when an unexpected error occurs during success/failure callbacks
    Property OnUnexpectedError : TRPCUnexpectedErrorCallback Read FOnUnexpectedError Write FOnUnexpectedError;
  end;

  { TRPCCustomService }

  // Result callback types for all supported types
  TEmptyResultHandler = reference to procedure;
  TBooleanResultHandler = reference to procedure (aResult : Boolean);
  TNativeIntResultHandler = reference to procedure (aResult : NativeInt);
  TDoubleResultHandler = reference to procedure (aResult : Double);
  TStringResultHandler = reference to procedure (aResult : String);
  TArrayResultHandler = reference to procedure (aResult : TJSArray);
  TObjectResultHandler = reference to procedure (aResult : TJSObject);
  TJSValueResultHandler = reference to procedure (aResult : JSValue);

  TRPCCustomService = class(TComponent)
  private
    FClient: TRPCClient;
    FParamBuilder: TRPCRequestParamsBuilder;
    procedure SetClient(AValue: TRPCClient);
  protected
    Procedure AddParam(const aName : string; aValue : NativeInt);
    Procedure AddParam(const aName : string; aValue : String);
    Procedure AddParam(const aName : string; aValue : Boolean);
    Procedure AddParam(const aName : string; aValue : Double);
    Procedure AddParam(const aName : string; aValue : TJSArray);
    Procedure AddParam(const aName : string; aValue : TJSObject);
    Procedure StartParams;
    Function EndParams : JSValue;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Function RPCClassName : String ; virtual;
    Function ExecuteRequest(const aClassName,aMethodName : String; aParams : JSValue; aOnSuccess : TRPCResultCallBack = Nil; aOnFailure: TRPCFailureCallBack = nil) : NativeInt;
    Property ParamBuilder : TRPCRequestParamsBuilder Read FParamBuilder;
  Published
    Property RPCClient : TRPCClient Read FClient Write SetClient;
  end;

implementation

uses web;

{ TRPCCustomService }

procedure TRPCCustomService.SetClient(AValue: TRPCClient);
begin
  if FClient=AValue then Exit;
  if Assigned(FClient) then
    FClient.RemoveFreeNotification(Self);
  FClient:=AValue;
  if Assigned(FClient) then
    FClient.FreeNotification(Self);
end;

procedure TRPCCustomService.AddParam(const aName: string; aValue: NativeInt);
begin
  ParamBuilder.AddArg(aName,aValue);
end;

procedure TRPCCustomService.AddParam(const aName: string; aValue: String);
begin
  ParamBuilder.AddArg(aName,aValue);
end;

procedure TRPCCustomService.AddParam(const aName: string; aValue: Boolean);
begin
  ParamBuilder.AddArg(aName,aValue);
end;

procedure TRPCCustomService.AddParam(const aName: string; aValue: Double);
begin
  ParamBuilder.AddArg(aName,aValue);
end;

procedure TRPCCustomService.AddParam(const aName: string; aValue: TJSArray);
begin
  ParamBuilder.AddArg(aName,aValue);
end;

procedure TRPCCustomService.AddParam(const aName: string; aValue: TJSObject);
begin
  ParamBuilder.AddArg(aName,aValue);
end;

procedure TRPCCustomService.StartParams;
begin
  if Assigned(FParamBuilder) then
    Raise ERPCClient.Create('Parameter building already in progress');
  if Not Assigned(RPCClient) then
    Raise ERPCClient.Create('Parameter building cannot be started without RPCClient');
  FParamBuilder:=RPCClient.CreateRequestParamsBuilder;
end;

function TRPCCustomService.EndParams: JSValue;
begin
  if not Assigned(FParamBuilder) then
    Raise ERPCClient.Create('No parameter builder was started. Call StartParams first');
  Result:=ParamBuilder.DoGetArgs;
  FreeAndNil(FParamBuilder);
end;

procedure TRPCCustomService.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
    if AComponent=FClient then
      FClient:=Nil;
end;

function TRPCCustomService.RPCClassName: String;
begin
  Result:='';
end;

function TRPCCustomService.ExecuteRequest(const aClassName,
  aMethodName: String; aParams: JSValue; aOnSuccess: TRPCResultCallBack;
  aOnFailure: TRPCFailureCallBack): NativeInt;
begin
  if Not Assigned(RPCClient) then
    Raise ERPCClient.Create('ExecuteRequest cannot be called without RPCClient');
  Result:=RPCClient.DoExecuteRequest(aClassName,aMethodName,aParams,aOnSuccess,aOnFailure);
end;

{ TRPCBatch }

function TRPCBatch.GetRequest(aID: NativeInt; DoRemove: Boolean): TRPCRequest;

Var
  Len,Idx : Integer;

begin
  Idx:=0;
  Len:=Length(Requests);
  While (Idx<Len) and (Requests[Idx].ID<>aID) do
    Inc(Idx);
  if (Idx<Len) then
    begin
    Result:=Requests[Idx];
    if DoRemove then
      Delete(Requests,Idx,1);
    end
  else
    Result:=Default(TRPCRequest);
end;

{ TRPCError }

procedure TRPCError.FromValue(Err: JSValue);
Var
  aErrJS : TJSError absolute Err;
  aErrEx : Exception absolute Err;
  aErrObj : TJSObject absolute Err;

begin
   Code:=0;
    if isObject(Err) then
      begin
      if Err is TJSError then
        begin
        Self.Code:=-2;
        Self.Message:=aErrJS.Message;
        if aErrJS.hasOwnProperty('status') and (isNumber(aErrJS['status'])) then
          Self.Code:=Integer(aErrJS['status']);
        Self.ErrorClass:='Error';
        end
      else if Err is Exception then
        begin
        Self.Code:=-3;
        Self.Message:=aErrEx.Message;
        Self.ErrorClass:=aErrEx.ClassName;
        end
      else // TJSObject
        begin
        Self.Code:=-4;
        if aErrObj.hasOwnProperty('code') and (isNumber(aErrJS['code'])) then
          Self.Code:=Integer(aErrJS['code']);
        if aErrJS.hasOwnProperty('message') and (isString(aErrJS['message'])) then
          Self.Message:=String(aErrJS['message']);
        Self.ErrorClass:='Object';
        end
      end;
    if Self.Code=0 then
      begin
      Self.Code:=-1;
      Self.Message:='Unknown error';
      end

end;

{ TRPCResponse }

procedure TRPCResponse.FromObject(Obj: TJSObject);


begin
  IsOK:=Obj.hasOwnProperty('id') and isNumber(Obj['id']);
  if ISOK then
    ID:=NativeInt(Obj['id']);
  HasError:=Obj.hasOwnProperty('error') and isObject(Obj['error']);
  if HasError then
    Error.FromValue(Obj['error'])
  else
    begin
    Result:=Obj['result'];
    if Obj.hasOwnProperty('jsonrpc') and isString(Obj['jsonrpc']) then
      Version:=String(Obj['jsonrpc']);
    end;
end;

{ TRPCRequestParamsBuilder }


procedure TRPCRequestParamsBuilder.AddArg(const aName: string; aValue: NativeInt);
begin
  DoAddArg(aName,aValue);
end;

procedure TRPCRequestParamsBuilder.AddArg(const aName: string; aValue: String);
begin
  DoAddArg(aName,aValue);
end;

procedure TRPCRequestParamsBuilder.AddArg(const aName: string; aValue: Boolean);
begin
  DoAddArg(aName,aValue);
end;

procedure TRPCRequestParamsBuilder.AddArg(const aName: string; aValue: Double);
begin
  DoAddArg(aName,aValue);
end;

procedure TRPCRequestParamsBuilder.AddArg(const aName: string; aValue: TJSArray);
begin
  DoAddArg(aName,aValue);
end;

procedure TRPCRequestParamsBuilder.AddArg(const aName: string; aValue: TJSObject);
begin
  DoAddArg(aName,aValue);
end;

{ TRPCObjectRequestParamsBuilder }

procedure TRPCObjectRequestParamsBuilder.DoAddArg(const aName: String; aValue: JSValue
  );
begin
  FParams.Properties[aName]:=aValue;
end;

function TRPCObjectRequestParamsBuilder.DoGetArgs: JSValue;
begin
  Result:=FParams;
end;

constructor TRPCObjectRequestParamsBuilder.Create(aParams: TJSObject);
begin
  FParams:=aParams;
end;

{ TRPCArrayRequestParamsBuilder }

function TRPCArrayRequestParamsBuilder.DoGetArgs: JSValue;
begin
  Result:=FParams;
end;

procedure TRPCArrayRequestParamsBuilder.DoAddArg(const aName: String; aValue: JSValue
  );
begin
  FParams.push(aValue);
  if aName='' then;
end;

constructor TRPCArrayRequestParamsBuilder.Create(aParams: TJSArray);
begin
  FParams:=AParams;
end;

{ TRPCClient }

procedure TRPCClient.SetOptions(AValue: TRPCoptions);

begin
  if FOptions=AValue then Exit;
  FOptions:=AValue;
end;

procedure TRPCClient.SetURL(AValue: String);
begin
  if FURL=AValue then Exit;
  FURL:=AValue;
end;

procedure TRPCClient.StartRequestBatch;
begin
  if Length(FBatch.Requests)>0 then
    SendRequestBatch
  else
    begin
    SetLength(FBatch.Requests,0);
    Inc(FBatchID);
    FBatch.ID:=FBatchID;
    end;
end;

procedure TRPCClient.CloseBatch;

begin
  if FCurrentBatchTimeout>0 then
     begin
     window.clearTimeout(FCurrentBatchTimeout);
     FCurrentBatchTimeout:=0;
     end;
  SendRequestBatch;
end;

function TRPCClient.GetBatch(aBatchID: NativeInt; DoRemove: Boolean): TRPCBatch;


Var
  BID : String;

begin
  BID:=IntToStr(aBatchID);
  if FPendingBatches.hasOwnProperty(BID) then
    begin
    Result:=TRPCBatch(FPendingBatches[BID]);
    if DoRemove then
      FPendingBatches[BID]:=undefined;
    end
  else
    Result:=Default(TRPCBatch);
end;

function TRPCClient.ResponseFromObject(aObj: TJSObject): TRPCResponse;

begin
  Result:=Default(TRPCResponse);
  Result.FromObject(aObj);
end;

procedure TRPCClient.HandleUnexpectedError(const aStage : String; E : Exception);

begin
  if Assigned(FOnUnexpectedError) then
    FOnUnexpectedError(Self,aStage,E);
end;

procedure TRPCClient.RemoveFromPending(aBatchID : NativeInt; Res: TJSArray);

var
  aReq : TRPCRequest;
  aResp : TRPCResponse;
  I : Integer;
  aBatch : TRPCBatch;

begin
  aBatch:=GetBatch(aBatchID,False);
  For I:=0 to Res.Length-1 do
    if isObject(Res[i]) then
      begin
      aResp:=ResponseFromObject(TJSObject(Res[i]));
      if aResp.IsOK then
        begin
        aReq:=aBatch.getRequest(aResp.ID,True);
        if (aReq.ID=aResp.ID) then
          if aResp.HasError then
            begin
            If Assigned(aReq.OnFailure) then
              try
                aReq.OnFailure(Self,aResp.Error);
              except
                On E : exception do
                  HandleUnexpectedError('OnFailure',E);
              end;
            end
          else
            begin
            If Assigned(aReq.OnSuccess) then
              try
                aReq.OnSuccess(Self,aResp.Result);
              except
                On E : exception do
                  HandleUnexpectedError('OnSuccess',E);
              end;
            end;
        end;
      end;
  // Remove if all requests treated
  if Length(aBatch.Requests)=0 then
    aBatch:=GetBatch(aBatchID,True);
end;

procedure TRPCClient.RemoveFromPending(aBatchID : NativeInt; aError : TRPCError);

Var
  aBatch : TRPCBatch;
  aReq : TRPCRequest;

begin
  aBatch:=GetBatch(aBatchID,True);
  For aReq in aBatch.Requests do
    if Assigned(aReq.OnFailure) then
      Try
        aReq.OnFailure(Self,aError);
      except
        On E : Exception do
          HandleUnexpectedError('OnFailure',E);
      end;
  SetLength(aBatch.Requests,0);
end;

function TRPCClient.ValueToError(Err: JSValue): TRPCError;

begin
  Result:=Default(TRPCError);
  Result.FromValue(Err);
end;

procedure TRPCClient.DoSendHTTPRequest(const aJSON : String; aBatchID : NativeInt);

    function dofail(aValue: JSValue): JSValue;

    Var
      Err : TRPCError;

    begin
      Result:=undefined;
      Err:=ValueToError(aValue);
      RemoveFromPending(aBatchID, Err)
    end;

    function processresponse (J : JSValue) : jsvalue;

    begin
      Result:=undefined;
      if isArray(J) then
        RemoveFromPending(aBatchID,TJSArray(J))
      else
        RemoveFromPending(aBatchID,TJSArray.New(J));
    end;


    function doOK(aValue: JSValue): JSValue;

    Var
      Req : TJSResponse absolute aValue;
      Err : TRPCError;

    begin
      Result:=Null;
      if not Req.ok then
        begin
        Err.Code:=Req.status;
        Err.Message:=Req.statusText;
        Err.ErrorClass:='HTTP';
        RemoveFromPending(aBatchID,Err);
        end
      else
        Req.json._then(@processresponse,@DoFail);
    end;


Var
  init,Headers : TJSObject;
  lheaders : TStringList;
  I : Integer;
  N,V : String;

begin
  init:=New([
    'method','POST',
    'cache','no-cache',
    'body',aJSON
  ]);
  Headers:=TJSObject.New;
  lheaders:=TStringList.Create;
  try
    GetHeaders(lHeaders);
    for I:=0 to lHeaders.Count-1 do
      begin
      lheaders.GetNameValue(I,N,V);
      headers[N]:=V;
      end;
    init['headers']:=Headers;
  finally
    lHeaders.Free;
  end;
  ConfigRequest(init);
  window.fetch(URL,init)._then(@doOK,@dofail);
end;

function TRPCClient.DoExecuteRequest(const aClassName, aMethodName: String;
  aParams: JSValue; aOnSuccess: TRPCResultCallBack;
  aOnFailure: TRPCFailureCallBack): NativeInt;
begin
  If isArray(AParams) then
    Result:=ExecuteRequest(aClassName,aMethodName,TJSArray(aParams),aOnSuccess,aOnFailure)
  else if isObject(AParams) then
    Result:=ExecuteRequest(aClassName,aMethodName,TJSObject(aParams),aOnSuccess,aOnFailure)
  else if Not (isUndefined(AParams) or isNull(aParams)) then
    Result:=ExecuteRequest(aClassName,aMethodName,TJSArray.New(aParams),aOnSuccess,aOnFailure)
  else
    Result:=ExecuteRequest(aClassName,aMethodName,TJSArray.New(),aOnSuccess,aOnFailure)

end;

procedure TRPCClient.GetHeaders(Headers: TStrings);

begin
  Headers.AddStrings(FCustomHeaders);
end;

procedure TRPCClient.ConfigRequest(init : TJSObject);

begin
  if Assigned(FOnConfigRequest) then
    FOnConfigRequest(Self,init);
end;

procedure TRPCClient.DoSendBatch(aBatch : TRPCBatch);

Var
  aRequests : TJSArray;
  aRequest : TRPCRequest;
  aSerialized : TJSObject;
  N : String;
  aJSON : String;

begin
  aRequests:=TJSArray.New;
  For aRequest in aBatch.Requests do
    begin
    aSerialized:=TJSObject.New;
    if Not aRequest.IsNotification then
      aSerialized['id']:=aRequest.ID;
    aSerialized['jsonrpc']:=JSONRPCversion;
    if Assigned(aRequest.Params) then
      aSerialized['params']:=aRequest.Params;
    N:=aRequest.MethodName;
    if roFullMethodName in FOptions then
      begin
      if aRequest.ClassName<>'' then
        N:=aRequest.ClassName+'.'+N;
      end
    else
      begin
      if aRequest.ClassName<>'' then
        aSerialized['class']:=aRequest.ClassName;
      end;
    aSerialized['method']:=N;
    aRequests.Push(aSerialized);
    end;
  if (aRequests.Length=1) and not (roForceArray in FOptions) then
    aJSON:=TJSJSON.stringify(aRequests[0])
  else
    aJSON:=TJSJSON.stringify(aRequests);
  For aRequest in aBatch.Requests do
    FPendingBatches[IntToStr(aBatch.Id)]:=JSValue(aBatch);
  try
    DoSendHTTPRequest(aJSON,aBatch.ID);
  finally
    aRequests:=nil;
  end;
end;

procedure TRPCClient.SetCustomHeaders(AValue: TStrings);
begin
  if FCustomHeaders=AValue then Exit;
  FCustomHeaders.Assign(AValue);
end;


procedure TRPCClient.SendRequestBatch;

Var
  aBatch : TRPCBatch;

begin
  aBatch:=FBatch;
  SetLength(FBatch.Requests,0);
  FBatch.ID:=0;
  if (Length(aBatch.Requests)>0) then
    DoSendBatch(aBatch);
  if FCurrentBatchTimeout>0 then
    begin
    Window.ClearTimeout(FCurrentBatchTimeout);
    FCurrentBatchTimeout:=0;
    end;
end;

procedure TRPCClient.AddToRequestBatch(aRequest: TRPCRequest);

Var
  Idx : Integer;

begin
  // Send pending, if any..
  if Not (roUseBatch in Options) then
    SendRequestBatch;
  if FBatch.ID=0 then
    begin
    Inc(FBatchID);
    FBatch.ID:=FBatchID;
    end;
  Idx:=Length(FBatch.Requests);
  SetLength(FBatch.Requests,Idx+1);
  FBatch.Requests[Idx]:=aRequest;
  if Not (roUseBatch in Options) then
    SendRequestBatch
  else
    if (roAutoBatch in FOptions) and (FCurrentBatchTimeout=0) then
      FCurrentBatchTimeout:=window.SetTimeout(@SendRequestBatch);
end;

function TRPCClient.AddToRequestBatch(aID: NativeInt; const aClassName,
  aMethodName: String; aParams: JSValue; aOnSuccess: TRPCResultCallBack;
  aOnFailure: TRPCFailureCallBack): TRPCRequest;

begin
  Result:=Default(TRPCRequest);
  Result.ID:=aID;
  Result.ClassName:=aClassName;
  Result.MethodName:=aMethodName;
  Result.Params:=aParams;
  Result.OnFailure:=aOnFailure;
  Result.OnSuccess:=aOnSuccess;
  AddToRequestBatch(Result);
  if not (roUseBatch in Options) then
    SendRequestBatch;
end;

constructor TRPCClient.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FPendingBatches:=TJSObject.New;
  FBatchTimeOut:=100;
  JSONRPCVersion:=DefaultJSONRPCversion;
  FCustomHeaders:=TStringList.Create;
  FCustomHeaders.NameValueSeparator:=':';
end;

destructor TRPCClient.Destroy;
begin
  FreeAndNil(FCustomHeaders);
  FPendingBatches:=Nil;
  inherited Destroy;
end;

function TRPCClient.CreateRequestParamsBuilder: TRPCRequestParamsBuilder;
begin
  if roParamsAsObject in Options then
    Result:=TRPCObjectRequestParamsBuilder.Create(TJSObject.New)
  else
    Result:=TRPCArrayRequestParamsBuilder.Create(TJSArray.New);
end;

function TRPCClient.ExecuteRequest(const aClassName, aMethodName: String;
  aParams: TJSArray; aOnSuccess: TRPCResultCallBack; aOnFailure: TRPCFailureCallBack): NativeInt;

Var
  Req : TRPCRequest;

begin
  Inc(FRequestID);
  Req:=AddToRequestBatch(FRequestID,aClassName,aMethodName,aParams,aOnSuccess,aOnFailure);
  Result:=Req.ID;
end;

function TRPCClient.ExecuteRequest(const aClassName, aMethodName: String;
  aParams: TJSObject; aOnSuccess: TRPCResultCallBack;
  aOnFailure: TRPCFailureCallBack): NativeInt;
Var
  Req : TRPCRequest;

begin
  Inc(FRequestID);
  Req:=AddToRequestBatch(FRequestID,aClassName,aMethodName,aParams,aOnSuccess,aOnFailure);
  Result:=Req.ID;
end;

end.

