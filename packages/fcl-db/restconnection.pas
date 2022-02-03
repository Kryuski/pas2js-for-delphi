{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by Michael Van Canneyt, member of the
    Free Pascal development team

    Simple REST connection component for use with Datasets.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit RestConnection;

{$mode objfpc}

interface

uses
  Classes, SysUtils, Web, DB;

Type
  

  { TRESTConnection }
  TRestGetURLEvent = Procedure (Sender : TComponent; aRequest : TDataRequest; Var aURL : String) of Object;
  TRestUpdateURLEvent = Procedure (Sender : TComponent; aRequest : TRecordUpdateDescriptor; Var aURL : String) of Object;
  TRESTConnection = Class(TComponent)
  private
    FBaseURL: String;
    FDataProxy : TDataProxy;
    FOnGetURL: TRestGetURLEvent;
    FOnUpdateURL: TRestUpdateURLEvent;
    FPageParam: String;
    function GetDataProxy: TDataProxy;
  Protected
    Procedure SetupRequest(aXHR : TJSXMLHttpRequest); virtual;
    Function GetUpdateBaseURL(aRequest: TRecordUpdateDescriptor) : String; virtual;
    Function GetReadBaseURL(aRequest: TDataRequest) : String; virtual;
    Function GetPageURL(aRequest : TDataRequest) : String; virtual;
    Function GetRecordUpdateURL(aRequest : TRecordUpdateDescriptor) : String;
  Public
    Function DoGetDataProxy : TDataProxy; virtual;
  Public
    Property DataProxy : TDataProxy Read GetDataProxy;
    Property BaseURL : String Read FBaseURL Write FBaseURL;
    Property PageParam : String Read FPageParam Write FPageParam;
    Property OnGetURL : TRestGetURLEvent Read FOnGetURL Write FOnGetURL;
    Property OnUpdateURL : TRestUpdateURLEvent Read FOnUpdateURL Write FOnUpdateURL;
  end;

  { TRESTDataProxy }

  TRESTDataProxy = class(TDataProxy)
  private
    FConnection: TRESTConnection;
  protected
    Procedure CheckBatchComplete(aBatch : TRecordUpdateBatch); virtual;
  Public
    Function GetUpdateDescriptorClass : TRecordUpdateDescriptorClass; override;
    Function ProcessUpdateBatch(aBatch : TRecordUpdateBatch): Boolean; override;
    Function DoGetData(aRequest: TDataRequest): Boolean; override;
    Function GetDataRequest(aOptions: TLoadOptions; aAfterRequest: TDataRequestEvent; aAfterLoad: TDatasetLoadEvent) : TDataRequest; override;
    Constructor Create(AOwner: TComponent);  override;
    Property Connection : TRESTConnection Read FConnection;
  end;

  { TRESTDataRequest }

  TRESTDataRequest = Class(TDataRequest)
  Private
    FXHR : TJSXMLHttpRequest;
  protected
    function onLoad(Event{%H-}: TEventListenerEvent): boolean; virtual;
    function TransformResult: JSValue; virtual;
  end;

  { TRESTUpdateRequest }

  TRESTUpdateRequest = Class(TRecordUpdateDescriptor)
  Private
    FXHR : TJSXMLHttpRequest;
    FBatch : TRecordUpdateBatch;
  protected
    function onLoad(Event{%H-}: TEventListenerEvent): boolean; virtual;
  end;

implementation

uses js;

{ TRESTUpdateRequest }

function TRESTUpdateRequest.onLoad(Event: TEventListenerEvent): boolean;
begin
  if (FXHR.Status div 100)=2 then
    begin
    Resolve(FXHR.response);
    Result:=True;
    end
  else
    ResolveFailed(FXHR.StatusText);
  (Proxy as TRestDataProxy).CheckBatchComplete(FBatch);
end;

{ TRESTDataRequest }

function TRESTDataRequest.TransformResult : JSValue;

begin
  Result:=FXHR.responseText;
end;

function TRESTDataRequest.onLoad(Event: TEventListenerEvent): boolean;
begin
  if (FXHR.Status=200) then
    begin
    Data:=TransformResult;
    Success:=rrOK;
    end
  else
    begin
    Data:=Nil;
    if (loAtEOF in LoadOptions) and (FXHR.Status=404) then
      Success:=rrEOF
    else
      begin
      Success:=rrFail;
      ErrorMsg:=FXHR.StatusText;
      end;
    end;
  DoAfterRequest;
  Result:=True;
end;

{ TRESTConnection }

function TRESTConnection.GetDataProxy: TDataProxy;
begin
  if (FDataProxy=Nil) then
    FDataProxy:=DoGetDataProxy;
  Result:=FDataProxy;
end;

procedure TRESTConnection.SetupRequest(aXHR: TJSXMLHttpRequest);
begin
  // Do nothing
  if aXHR=nil then ;
end;

function TRESTConnection.GetUpdateBaseURL(aRequest: TRecordUpdateDescriptor): String;
begin
  Result:=BaseURL;
  if aRequest=nil then ;
end;

function TRESTConnection.GetReadBaseURL(aRequest: TDataRequest): String;
begin
  Result:=BaseURL;
  if aRequest=nil then ;
end;

function TRESTConnection.GetPageURL(aRequest: TDataRequest): String;

Var
  URL : String;

begin
  URL:=GetReadBaseURL(aRequest);
  if (PageParam<>'') then
    begin
    if Pos('?',URL)<>0 then
      URL:=URL+'&'
    else
      URL:=URL+'?';
    URL:=URL+PageParam+'='+IntToStr(ARequest.RequestID-1);
    end;
  if Assigned(FOnGetURL) then
    FOnGetURL(Self,aRequest,URL);
  Result:=URL;
end;

function TRESTConnection.GetRecordUpdateURL(aRequest: TRecordUpdateDescriptor): String;

Var
  I : integer;
  Base,KeyField : String;

begin
  KeyField:='';
  Result:='';
  Base:=GetUpdateBaseURL(aRequest);
  if aRequest.Status in [usModified,usDeleted] then
    begin
    I:=aRequest.Dataset.Fields.Count-1;
    While (KeyField='') and (I>=0) do
      begin
      if pfInKey in aRequest.Dataset.Fields[i].ProviderFlags then
        KeyField:=aRequest.Dataset.Fields[i].FieldName;
      Dec(I);
      end;
    if (KeyField='') then
      DatabaseError('No key field',aRequest.Dataset);
    end;
  if (KeyField<>'') and (Base<>'') and (Base[Length(Base)]<>'/') then
    Base:=Base+'/';
  Case aRequest.Status of
    usModified,
    usDeleted: Result:=Base+TJSJSON.stringify(TJSObject(aRequest.Data)[KeyField]);
    usInserted : Result:=Base;
  end;
  If Assigned(FOnUpdateURL) then
    FOnUpdateURL(Self,aRequest,Result);
end;

function TRESTConnection.DoGetDataProxy: TDataProxy;
begin
  Result:=TRESTDataProxy.Create(Self);
end;

{ TRESTDataProxy }

procedure TRESTDataProxy.CheckBatchComplete(aBatch: TRecordUpdateBatch);

Var
  BatchOK : Boolean;
  I : Integer;

begin
  BatchOK:=True;
  I:=aBatch.List.Count-1;
  While BatchOK and (I>=0) do
    begin
    BatchOK:=aBatch.List[I].ResolveStatus in [rsResolved,rsResolveFailed];
    Dec(I);
    end;
  If BatchOK and Assigned(aBatch.OnResolve) then
    aBatch.OnResolve(Self,aBatch);
end;

function TRESTDataProxy.GetUpdateDescriptorClass: TRecordUpdateDescriptorClass;
begin
  Result:=TRESTUpdateRequest;
end;

function TRESTDataProxy.ProcessUpdateBatch(aBatch: TRecordUpdateBatch): Boolean;

Var
  R : TRESTUpdateRequest;
  i : Integer;
  Method,URl : String;

begin
  Result:=False;
  For I:=0 to aBatch.List.Count-1 do
    begin
    R:=aBatch.List[i] as TRESTUpdateRequest;
    R.FBatch:=aBatch;
    R.FXHR:=TJSXMLHttpRequest.New;
    R.FXHR.AddEventListener('load',@R.onLoad);
    URL:=FConnection.GetRecordUpdateURL(R);
    Case R.Status of
      usInserted :
        Method:='POST';
      usModified:
        Method:='PUT';
      usDeleted:
        Method:='DELETE';
    end;
    R.FXHR.open(Method,URL);
    R.FXHR.setRequestHeader('content-type','application/json');
    Connection.SetupRequest(R.FXHR); 
    if R.Status in [usInserted,usModified] then
      R.FXHR.Send(TJSJSON.Stringify(R.Data))
    else
      R.FXHR.Send;
    end;
  Result:=True;
end;

function TRESTDataProxy.DoGetData(aRequest: TDataRequest): Boolean;

Var
  R : TRestDataRequest;
  URL : String;

begin
  Result:=False;
  R:=aRequest as TRestDataRequest;
  R.FXHR:=TJSXMLHttpRequest.New;
  R.FXHR.AddEventListener('load',@R.onLoad);
  URL:=Connection.GetPageURL(aRequest);
  if (URL='') then
    begin
    if loAtEOF in R.LoadOptions then
      R.Success:=rrEOF
    else
      begin
      R.Success:=rrFail;
      R.ErrorMsg:='No URL to get data';
      R.DoAfterRequest; // This will free request !
      end;
    end
  else
    begin
    if (loAtEOF in R.LoadOptions) and (Connection.PageParam='') then
      R.Success:=rrEOF
    else
      begin
      R.FXHR.open('GET',URL,true);
      Connection.SetupRequest(R.FXHR);
      R.FXHR.send;
      Result:=True;
      end;
    end;
end;

function TRESTDataProxy.GetDataRequest(aOptions: TLoadOptions; aAfterRequest: TDataRequestEvent; aAfterLoad: TDatasetLoadEvent): TDataRequest;

begin
  Result:=TRestDataRequest.Create(Self,aOptions, aAfterRequest,aAfterLoad);
end;

constructor TRESTDataProxy.Create(AOwner: TComponent);
begin
  Inherited;
  If AOwner is TRestConnection then
    FConnection:=TRestConnection(aOwner);
end;

end.

