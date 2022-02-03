{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2018 by Michael Van Canneyt, member of the
    Free Pascal development team

    Dataset which talks to Remobjects Data Abstract server.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit dadataset;

{$mode objfpc}
{$modeswitch externalclass}


interface

uses Types, Classes, DB, jsonDataset, JS, rosdk, da, dasdk;

Type
  EDADataset = Class(EDatabaseError);
  TDAConnection = Class;

  { TDAWhereClauseBuilder }

  TDAWhereClauseBuilder = class
  public
    class function NewBinaryExpression(aLeft, aRight: TDAExpression; anOp: TDABinaryOperator): TDAExpression;overload;
    class function NewBinaryExpression(aLeft: TDAExpression; anOp: TDABinaryOperator;const aValue: JSValue): TDAExpression;overload;
    class function NewBinaryExpression(aLeft: TDAExpression; anOp: TDABinaryOperator;const aValue: JSValue; aType: TDADataType): TDAExpression;overload;
    class function NewBinaryExpression(const aTableName,aFieldName: string; anOp: TDABinaryOperator; const aJSValue: JSValue; aType: TDADataType): TDAExpression; overload;
    class function NewBinaryExpression(const aTableName,aFieldName: string; anOp: TDABinaryOperator; const aJSValue: JSValue): TDAExpression; overload;
    class function NewBinaryExpression(const aTableName,aFieldName: string; const aParameterName: string; aParameterType: TDADataType; anOp: TDABinaryOperator): TDAExpression; overload;
    class function NewBinaryExpressionList(const aExpressions: array of TDAExpression; anOp: TDABinaryOperator): TDAExpression;
    class function NewUnaryExpression(anExpression: TDAExpression; anOp: TDAUnaryOperator): TDAExpression;
    class function NewConstant(const aValue: jsValue): TDAExpression; overload;
    class function NewConstant(const aValue: jsValue; aType: TDADataType): TDAExpression; overload;
    class function NewDateTimeConstant(const aValue: TJSDate): TDAExpression; overload;
    class function NewDateTimeConstant(const aValue: TDateTime): TDAExpression; overload;
    class function NewList(const aValues: array of TDAExpression): TDAExpression;
    class function NewParameter(const aParameterName: string; aParameterType: TDADataType = datUnknown): TDAExpression;
    class function NewField(const aTableName,aFieldName: string): TDAExpression;
    class function NewNull: TDAExpression;
    class function NewIsNotNull: TDAExpression; overload;
    class function NewIsNotNull(const aTableName,aFieldName: string): TDAExpression; overload;
    class function NewIsNull(const aTableName,aFieldName: string): TDAExpression; overload;
    class function NewMacro(const aName: string): TDAExpression; overload;
    class function NewMacro(const aName: string; const aValues: array of TDAExpression): TDAExpression; overload;
    class function NewBetweenExpression(aExpression, aLower, aUpper: TDAExpression): TDAExpression; overload;
    class function NewBetweenExpression(const aExprTableName, aExprFieldName: string; aLower, aUpper: TDAExpression): TDAExpression; overload;
    class function NewBetweenExpression(const aExprTableName, aExprFieldName: string; aLowerValue, aUpperValue: JSValue; aValuesDataType: TDADataType): TDAExpression; overload;
    class function GetWhereClause (aExpression : TDAExpression) : String;
  end;

Type
  TDADataRow = Class external name 'Object' (TJSObject)
    _new,
    _old : TJSValueDynArray;
  end;
  TDaDataRowArray = Array of TDADataRow;

  TResolvedRow = Class external name 'Object' (TJSObject)
    changes : TDAChange;
    fields : TLogFieldArray;
  end;

  { TDAArrayFieldMapper }

  TDAArrayFieldMapper = Class(TJSONArrayFieldMapper)
  Public
    Procedure RemoveField(Const FieldName : String; FieldIndex : Integer; Row : JSValue); override;
    procedure SetJSONDataForField(Const FieldName{%H-} : String; FieldIndex : Integer; Row,Data : JSValue); override;
    Function GetJSONDataForField(Const FieldName{%H-} : String; FieldIndex : Integer; Row : JSValue) : JSValue; override;
    Function CreateRow : JSValue; override;
  end;


  { TDADataset }
  TDADatasetOption = (doRefreshAllFields);
  TDADatasetOptions = Set of TDADatasetOption;

  TDADataset = class(TBaseJSONDataset)
  private
    FDAOptions: TDADatasetOptions;
    FParams: TParams;
    FTableName: String;
    FDAConnection: TDAConnection;
    FWhereClause: String;
    FWhereClauseBuilder : TDAWhereClauseBuilder;
    function DataTypeToFieldType(s: String): TFieldType;
    procedure SetParams(AValue: TParams);
  Protected
    function DoResolveRecordUpdate(anUpdate : TRecordUpdateDescriptor): Boolean; override;
    Procedure MetaDataToFieldDefs; override;
    Procedure InternalEdit; override;
    Procedure InternalDelete; override;
    // These operate on metadata received from DA.
    function GetDAFields: TDAFieldArray;
    function GetExcludedFields : TNativeIntDynArray;
    Function GetLoggedFields : TLogFieldArray;
    function GetKeyFields: TStringDynArray;
  Public
    constructor create(aOwner : TComponent); override;
    Destructor Destroy; override;
    function ConvertToDateTime(aField : TField; aValue : JSValue; ARaiseException : Boolean) : TDateTime; override;
    function ConvertDateTimeToNative(aField : TField; aValue : TDateTime) : JSValue; override;
    function DoGetDataProxy: TDataProxy; override;
    Function ParamByName(Const aName : string) : TParam;
    Function FindParam(Const aName : string) : TParam;
    Property WhereClauseBuilder : TDAWhereClauseBuilder Read FWhereClauseBuilder;
    // DA is index based. So create array field mapper.
    function CreateFieldMapper : TJSONFieldMapper; override;
    Procedure CreateFieldDefs(a : TJSArray);
    Property TableName : String Read FTableName Write FTableName;
    Property DAConnection : TDAConnection Read FDAConnection Write FDAConnection;
    Property Params : TParams Read FParams Write SetParams;
    Property WhereClause : String Read FWhereClause Write FWhereClause;
    Property DAOptions : TDADatasetOptions Read FDAOptions Write FDAOptions;
  end;



  TDADataRequest = Class(TDataRequest)
  Public
    Procedure doSuccess(res : JSValue) ;
    Procedure DoFail(response: TROMessage; fail: TjsError) ;
  End;

  { TDADataProxy }

  TDADataProxy = class(TDataProxy)
  private
    FConnection: TDAConnection;
    function ConvertParams(DADS: TDADataset): TDADataParameterDataArray;
    procedure ProcessUpdateResult(Res: JSValue; aBatch: TRecordUpdateBatch);
  Protected
    Function GetDataRequestClass : TDataRequestClass; override;
  Public
    Function DoGetData(aRequest : TDataRequest) : Boolean; override;
    Function ProcessUpdateBatch(aBatch : TRecordUpdateBatch): Boolean; override;
    Property Connection : TDAConnection Read FConnection Write FConnection;
  end;

  TDAMessageType = (mtAuto, // autodetect from URL
                    mtBin,  // use BinMessage
                    mtJSON); // Use JSONMessage.
  TDAStreamerType = (stJSON,stBin);

  { TDAConnection }
  TLoginFailedEvent = Reference to procedure (Msg : TROMessage; Err : String);
  TDAConnection = class(TComponent)
  private
    FDataService: TDADataAbstractService;
    FDataserviceName: String;
    FLoginService: TDASimpleLoginService;
    FLoginServiceName: String;
    FMessageType: TDAMessageType;
    FMessage : TROmessage;
    FChannel : TROHTTPClientChannel;
    FOnLoginFailed: TLoginFailedEvent;
    FOnLogin: TDALoginSuccessEvent;
    FOnLogout: TDASuccessEvent;
    FOnLogoutailed: TDAFailedEvent;
    FOnLogoutFailed: TDAFailedEvent;
    FStreamerType: TDAStreamerType;
    FURL: String;
    procedure ClearConnection;
    function GetChannel: TROHTTPClientChannel;
    function GetClientID: String;
    Function GetDataService : TDADataAbstractService;
    function GetLoginService: TDASimpleLoginService;
    function GetMessage: TROMessage;
    procedure SetDataserviceName(AValue: String);
    procedure SetLoginServiceName(AValue: String);
    procedure SetMessageType(AValue: TDAMessageType);
    procedure SetURL(AValue: String);
    procedure DoLoginFailed(Msg: TROMessage; aErr: TJSError);
  Protected
    Procedure CreateChannelAndMessage; virtual;
    function DetectMessageType(Const aURL: String): TDAMessageType; virtual;
    Function CreateDataService : TDADataAbstractService; virtual;
    Function CreateLoginService : TDASimpleLoginService; virtual;
    Function CreateStreamer : TDADataStreamer;
    Function InterpretMessage(aRes : JSValue) : String;
  Public
    Constructor create(aOwner : TComponent); override;
    Destructor Destroy; override;
    // Returns a non-auto MessageType, but raises exception if it cannot be determined;
    Function EnsureMessageType : TDAMessageType;
    // Returns DataService, but raises exception if it is nil;
    Function EnsureDataservice : TDADataAbstractService;
    // Returns SimpleLoginService, but raises exception if it is nil;
    Function EnsureLoginservice : TDASimpleLoginService;
    // Call this to login. This is an asynchronous call, check the result using OnLoginOK and OnLoginFailed calls.
    Procedure Login(aUserName, aPassword : String);
    Procedure LoginEx(aLoginString : String);
    Procedure Logout;
    // You can set this. If you didn't set this, and URL is filled, an instance will be created.
    Property DataService : TDADataAbstractService Read GetDataService Write FDataService;
    //  You can set this. If you didn't set this, and URL is filled, an instance will be created.
    Property LoginService : TDASimpleLoginService Read GetLoginService Write FLoginService;
    // You can get this to use in other service constructors
    Property Channel : TROHTTPClientChannel Read GetChannel;
    Property Message : TROMessage Read GetMessage;
    // Get client ID
    Property ClientID : String Read GetClientID;
  Published
    // If set, this is the message type that will be used when auto-creating the service. Setting this while dataservice is Non-Nil will remove the reference
    Property MessageType : TDAMessageType Read FMessageType Write SetMessageType;
    // if set, URL is used to create a DataService. Setting this while dataservice is Non-Nil will remove the reference
    Property URL : String Read FURL Write SetURL;
    // DataServiceName is used to create a DataService. Setting this while dataservice is Non-Nil will remove the reference
    Property DataserviceName : String Read FDataserviceName Write SetDataserviceName;
    // LoginServiceName is used to create a login service. Setting this while loginservice is Non-Nil will remove the reference
    Property LoginServiceName : String read FLoginServiceName write SetLoginServiceName;
    // Called when login call is executed.
    Property OnLogin : TDALoginSuccessEvent Read FOnLogin Write FOnLogin;
    // Called when login call failed. When call was executed but user is wrong OnLogin is called !
    Property OnLoginCallFailed : TLoginFailedEvent Read FOnLoginFailed Write FOnLoginFailed;
    // Called when logout call is executed.
    Property OnLogout : TDASuccessEvent Read FOnLogout Write FOnLogout;
    // Called when logout call failed.
    Property OnLogOutCallFailed : TDAFailedEvent Read FOnLogoutailed Write FOnLogoutFailed;
    // Streamertype : format of the data package in the message.
    Property StreamerType : TDAStreamerType Read FStreamerType Write FStreamerType;
  end;


implementation

uses strutils, sysutils;

resourcestring
  SErrInvalidDate = '%s is not a valid date value for %s';

{ TDAArrayFieldMapper }

procedure TDAArrayFieldMapper.RemoveField(const FieldName: String; FieldIndex: Integer; Row: JSValue);

begin
  Inherited RemoveField(FieldName,FieldIndex,TDADataRow(Row)._new);
end;

procedure TDAArrayFieldMapper.SetJSONDataForField(const FieldName: String; FieldIndex: Integer; Row, Data: JSValue);
begin
  Inherited SetJSONDataForField(FieldName,FieldIndex,TDADataRow(Row)._new,Data);
end;

function TDAArrayFieldMapper.GetJSONDataForField(const FieldName: String; FieldIndex: Integer; Row: JSValue): JSValue;
begin
  Result:=Inherited GetJSONDataForField(FieldName,FieldIndex,TDADataRow(Row)._new);
end;

function TDAArrayFieldMapper.CreateRow: JSValue;

begin
  Result:=TDADataRow.New;
  With TDADataRow(Result) do
    begin
    _new:=[];
    _old:=[];
    end;
end;

{ TDAWhereClauseBuilder }

class function TDAWhereClauseBuilder.NewBinaryExpression(aLeft, aRight: TDAExpression; anOp: TDABinaryOperator): TDAExpression;
begin
  Result:=TDABinaryExpression.New(aLeft,aRight,BinaryOperatorNames[anOp]);
end;

class function TDAWhereClauseBuilder.NewBinaryExpression(aLeft: TDAExpression; anOp: TDABinaryOperator; const aValue: JSValue
  ): TDAExpression;
begin
  Result:=TDABinaryExpression.New(aLeft,NewConstant(aValue),BinaryOperatorNames[anOp]);
end;

class function TDAWhereClauseBuilder.NewBinaryExpression(aLeft: TDAExpression; anOp: TDABinaryOperator; const aValue: JSValue;
  aType: TDADataType): TDAExpression;
begin
  Result:=TDABinaryExpression.New(aLeft,NewConstant(aValue,aType),BinaryOperatorNames[anOp]);
end;

class function TDAWhereClauseBuilder.NewBinaryExpression(const aTableName, aFieldName: string; anOp: TDABinaryOperator;
  const aJSValue: JSValue; aType: TDADataType): TDAExpression;

begin
  Result:=TDABinaryExpression.New(NewField(aTableName,aFieldName),NewConstant(aJSValue,aType),BinaryOperatorNames[anOp])
end;

class function TDAWhereClauseBuilder.NewBinaryExpression(const aTableName, aFieldName: string; anOp: TDABinaryOperator;
  const aJSValue: JSValue): TDAExpression;

begin
  Result:=TDABinaryExpression.New(NewField(aTableName,aFieldName),NewConstant(aJSValue),BinaryOperatorNames[anOp])
end;

class function TDAWhereClauseBuilder.NewBinaryExpression(const aTableName, aFieldName: string; const aParameterName: string;
  aParameterType: TDADataType; anOp: TDABinaryOperator): TDAExpression;

begin
  Result:=TDABinaryExpression.New(NewField(aTableName,aFieldName),NewParameter(aParameterName,aParameterType),BinaryOperatorNames[anOp])
end;

class function TDAWhereClauseBuilder.NewBinaryExpressionList(const aExpressions: array of TDAExpression; anOp: TDABinaryOperator): TDAExpression;

var
  i, len: integer;
begin
  len:=Length(aExpressions);
  Case Len of
    0: Result:=nil;
    1: Result:=aExpressions[0];
  else
    Result:=NewBinaryExpression(aExpressions[0],aExpressions[1],anOp);
    for i := 2 to Len-1 do
      Result:=NewBinaryExpression(Result,aExpressions[i],anOp);
  end;
end;

class function TDAWhereClauseBuilder.NewUnaryExpression(anExpression: TDAExpression; anOp: TDAUnaryOperator): TDAExpression;
begin
  Result:=TDAUnaryExpression.New(anExpression,UnaryOperatorNames[anOp]);
end;

class function TDAWhereClauseBuilder.NewConstant(const aValue: jsValue): TDAExpression;
begin
  Result:=TDAConstantExpression.New(JSValueToDataTypeName(aValue),aValue,Ord(IsNull(aValue)));
end;

class function TDAWhereClauseBuilder.NewDateTimeConstant(const aValue: TJSDate): TDAExpression; overload;

begin
  Result:=TDAConstantExpression.New(DataTypeNames[datDateTime],Trunc(aValue.Time/1000),0);
end;

class function TDAWhereClauseBuilder.NewDateTimeConstant(const aValue: TDateTime): TDAExpression; overload;

begin
  Result:=NewDateTimeConstant(DateTimeToJSDate(aValue));
//  Result:=TDAConstantExpression.New(DataTypeNames[datDateTime],DateTimeToJSDate(('yyyy"-"mm"-"dd"T"hh":"nn":"ss',aValue),0);
end;

class function TDAWhereClauseBuilder.NewConstant(const aValue: jsValue; aType: TDADataType): TDAExpression;
begin
  Result:=TDAConstantExpression.New(JSValueToDataTypeName(aValue),aValue,Ord(IsNull(aValue)));
end;

class function TDAWhereClauseBuilder.NewList(const aValues: array of TDAExpression): TDAExpression;
begin
  Result:=TDAListExpression.New(aValues);
end;

class function TDAWhereClauseBuilder.NewParameter(const aParameterName: string; aParameterType: TDADataType): TDAExpression;
begin
  Result:=TDAParameterExpression.New(aParameterName,DataTypeNames[aParameterType],0);
end;

class function TDAWhereClauseBuilder.NewField(const aTableName, aFieldName: string): TDAExpression;
var
  aName : String;

begin
  aName:=aFieldName;
  if aTableName<>'' then
    aName:=aTableName+'.'+aName;
  Result:=TDAFieldExpression.New(aName);
end;

class function TDAWhereClauseBuilder.NewNull: TDAExpression;
begin
  Result:=TDANullExpression.new;
end;

class function TDAWhereClauseBuilder.NewIsNotNull: TDAExpression;
begin
  Result:=NewUnaryExpression(TDANullExpression.new,duoNot);
end;

class function TDAWhereClauseBuilder.NewIsNotNull(const aTableName, aFieldName: string): TDAExpression;
begin
  Result:=NewBinaryExpression(NewField(aTableName,aFieldName),NewIsNotNull,dboEqual);
end;

class function TDAWhereClauseBuilder.NewIsNull(const aTableName,
  aFieldName: string): TDAExpression;
begin
  Result:=NewBinaryExpression(NewField(aTableName,aFieldName),TDANullExpression.new,dboEqual);
end;

class function TDAWhereClauseBuilder.NewMacro(const aName: string): TDAExpression;
begin
  Result:=TDAMacroExpression.New(aName);
end;

class function TDAWhereClauseBuilder.NewMacro(const aName: string; const aValues: array of TDAExpression): TDAExpression;
begin
  Result:=TDAMacroExpression.New(aName); // ??
end;

class function TDAWhereClauseBuilder.NewBetweenExpression(aExpression, aLower, aUpper: TDAExpression): TDAExpression;
begin
  Result:=TDABetweenExpression.New(aExpression,aLower,aUpper);
end;

class function TDAWhereClauseBuilder.NewBetweenExpression(const aExprTableName, aExprFieldName: string; aLower,
  aUpper: TDAExpression): TDAExpression;
begin
  Result:=NewBetweenExpression(NewField(aExprTableName,aExprFieldName),aLower,aUpper);
end;

class function TDAWhereClauseBuilder.NewBetweenExpression(const aExprTableName, aExprFieldName: string; aLowerValue,
  aUpperValue: JSValue; aValuesDataType: TDADataType): TDAExpression;
begin
  Result:=NewBetweenExpression(NewField(aExprTableName,aExprFieldName),
                               NewConstant(aLowerValue,aValuesDataType),
                               NewConstant(aUpperValue,aValuesDataType));
end;

class function TDAWhereClauseBuilder.GetWhereClause(aExpression: TDAExpression): String;

Var
  DW : TDADynamicWhere;

begin
  DW:=TDADynamicWhere.New(aExpression);
  try
    Result:=dw.toXml
  Finally
    DW:=Nil;
  end;
end;

{ TDAConnection }


function TDAConnection.GetDataService: TDADataAbstractService;
begin
  if (FDataservice=Nil) then
    FDataservice:=CreateDataService;
  Result:=FDataService;
end;

function TDAConnection.GetLoginService: TDASimpleLoginService;
begin
  if (FLoginService=Nil) then
    FLoginService:=CreateLoginService;
  Result:=FLoginService;
end;

function TDAConnection.GetMessage: TROMessage;
begin
  CreateChannelAndMessage;
  Result:=FMessage;
end;

procedure TDAConnection.SetDataserviceName(AValue: String);
begin
  if FDataserviceName=AValue then Exit;
  ClearConnection;
  FDataserviceName:=AValue;
end;

procedure TDAConnection.SetLoginServiceName(AValue: String);
begin
  if FLoginServiceName=AValue then Exit;
  FLoginServiceName:=AValue;
end;

procedure TDAConnection.SetMessageType(AValue: TDAMessageType);
begin
  if FMessageType=AValue then Exit;
  ClearConnection;
  FMessageType:=AValue;
end;

procedure TDAConnection.ClearConnection;

begin
  FDataservice:=Nil;
  FChannel:=Nil;
  FMessage:=Nil;
end;

function TDAConnection.GetChannel: TROHTTPClientChannel;
begin
  CreateChannelAndMessage;
  Result:=FChannel;
end;

function TDAConnection.GetClientID: String;
begin
  if Assigned(FMessage) then
    Result:=FMessage.ClientID
  else
    Result:='';
end;

procedure TDAConnection.SetURL(AValue: String);
begin
  if FURL=AValue then Exit;
  ClearConnection;
  FURL:=AValue;
end;

procedure TDAConnection.CreateChannelAndMessage;


begin
  if (FChannel=Nil) then
    FChannel:=TROHTTPClientChannel.New(URL);
  if (FMessage=Nil) then
    Case EnsureMessageType of
      mtBin : fMessage:=TROBINMessage.New;
      mtJSON : fMessage:=TROJSONMessage.New;
    end;
end;

function TDAConnection.DetectMessageType(const aURL: String): TDAMessageType;

Var
  S : String;

begin
  S:=aURL;
  Delete(S,1,RPos('/',S));
  case lowercase(S) of
    'bin' : Result:=mtBin;
    'json' : Result:=mtJSON;
  else
    Raise EDADataset.Create(Name+': Could not determine message type from URL: '+aURL);
  end;
end;


function TDAConnection.CreateDataService: TDADataAbstractService;

begin
  Result:=Nil;
  if URL='' then exit;
  CreateChannelAndMessage;
  Result:=TDADataAbstractService.New(FChannel,FMessage,DataServiceName);
end;

function TDAConnection.CreateLoginService: TDASimpleLoginService;
begin
  Result:=Nil;
  if URL='' then exit;
  CreateChannelAndMessage;
  Result:=TDASimpleLoginService.New(FChannel,FMessage,LoginServiceName);
end;

function TDAConnection.CreateStreamer: TDADataStreamer;
begin
  Case StreamerType of
    stJSON : Result:=TDAJSONDataStreamer.new;
    stBIN: Result:=TDABIN2DataStreamer.new;
  end;
end;

function TDAConnection.InterpretMessage(aRes: JSValue): String;
begin
  Result:=String(aRes);
  if (EnsureMessageType=mtJSON) then
    Result:=TROUtil.Frombase64(Result);
end;

constructor TDAConnection.create(aOwner: TComponent);
begin
  inherited create(aOwner);
  FDataServiceName:='DataService';
  FLoginServiceName:='LoginService';
end;

destructor TDAConnection.Destroy;
begin
  ClearConnection;
  inherited Destroy;
end;

function TDAConnection.EnsureMessageType: TDAMessageType;
begin
  Result:=MessageType;
  if Result=mtAuto then
    Result:=DetectMessageType(URL);
end;

function TDAConnection.EnsureDataservice: TDADataAbstractService;

begin
  Result:=Dataservice;
  if (Result=Nil) then
    Raise EDADataset.Create('No data service available. ');
end;

function TDAConnection.EnsureLoginservice: TDASimpleLoginService;

begin
  Result:=LoginService;
  if (Result=Nil) then
    Raise EDADataset.Create('No login service available. ');
end;

procedure TDAConnection.DoLoginFailed(Msg : TROMessage; aErr : TJSError);

Var
  ErrMsg : String;

begin
  if Assigned(FonLoginFailed) then
    begin
    if IsObject(aErr) then
      if TJSObject(aErr).HasOwnProperty('message') then
        ErrMsg:=aErr.Message
      else
        ErrMsg:='Error object: '+TJSJSON.Stringify(aErr)
    else if IsString(aErr) then
      ErrMsg:=String(JSValue(aErr))
    else
      ErrMsg:='Unknown error';
    FOnLoginFailed(Msg,errMsg);
    end;
end;


procedure TDAConnection.Login(aUserName, aPassword: String);

begin
  EnsureLoginService.Login(aUserName,aPassword,FOnLogin,@DoLoginFailed);
end;

procedure TDAConnection.LoginEx(aLoginString: String);
begin
  EnsureLoginService.LoginEx(aLoginString,FOnLogin,@DoLoginFailed);
end;

procedure TDAConnection.Logout;
begin
  EnsureLoginService.Logout(FOnLogout,FOnLogoutFailed);
end;

{ TDADataset }

function TDADataset.DataTypeToFieldType(s : String) : TFieldType;

Const
  FieldStrings : Array [TFieldType] of string = (
    '','String', 'Integer', 'LargeInt', 'Boolean', 'Float', 'Date',
    'Time', 'DateTime',  'AutoInc', 'Blob', 'Memo', 'FixedChar',
    'Variant','Dataset');


begin
  if (Copy(S,1,3)='dat') then
    system.Delete(S,1,3);
  Result:=High(TFieldType);
  While (Result>ftUnknown) and Not SameText(FieldStrings[Result],S) do
    Result:=Pred(Result);
  if Result=ftUnknown then
    case LowerCase(s) of
     'widememo',
     'widestring' : result:=ftString;
     'currency' : result:=ftFloat;
     'decimal' : result:=ftFloat;
     'smallint' : result:=ftInteger;
     'largeautoinc' : result:=ftLargeInt;
    else
      writeln('Unknown field type:',S)
    end;
end;

procedure TDADataset.SetParams(AValue: TParams);
begin
  if FParams=AValue then Exit;
  FParams.Assign(AValue);
end;

function TDADataset.DoResolveRecordUpdate(anUpdate: TRecordUpdateDescriptor): Boolean;

Var
  rIdx,I : Integer;
  Fld : TField;
  ResRow : TResolvedRow;
  aRow : JSValue;

begin
  Result:=True;
  if Assigned(anupDate.ServerData) and (anUpdate.Status<>usDeleted) then
     begin
     rIdx:=NativeInt(anUpdate.Bookmark.Data);
     if Not (rIdx>=0) and (rIdx<Rows.Length) then
        exit;
     // Apply new values
     aRow:=Rows[rIdx];
     ResRow:=TResolvedRow(anUpdate.ServerData);
     With ResRow do
       for I:=0 to Length(Fields)-1 do
         if (doRefreshAllFields in DAOptions) or (Changes.old[I]<>Changes.new_[I]) then
            begin
            Fld:=FieldByName(Fields[i].Name);
            FieldMapper.SetJSONDataForField(Fld,aRow,Changes.New_[i]);
            end;
     TDADataRow(aRow)._old:=[];
     end;
end;

function TDADataset.ConvertToDateTime(aField: TField; aValue: JSValue; ARaiseException: Boolean): TDateTime;
begin
  Result:=0;
  if isDate(aValue) then
    Result:=JSDateToDateTime(TJSDate(aValue))
  else if isString(aValue) then
    Result:=Inherited  ConvertToDateTime(afield,aValue,ARaiseException)
  else
    if aRaiseException then
      DatabaseErrorFmt(SErrInvalidDate,[String(aValue),aField.FieldName],Self);
end;

function TDADataset.ConvertDateTimeToNative(aField: TField; aValue: TDateTime): JSValue;
begin
  Result:=DateTimeToJSDate(aValue);
end;

procedure TDADataset.MetaDataToFieldDefs;

begin
  if Not isArray(Metadata['fields']) then
    exit;
  CreateFieldDefs(TJSArray(Metadata['fields']));
end;

procedure TDADataset.InternalEdit;

Var
  D : TDADataRow;

begin
  Inherited;
  D:=TDADataRow(ActiveBuffer.Data);
  if Length(D._old)=0 then
    begin
    asm
    this.FEditRow._old=this.FEditRow._new.slice();
    end;
    end;
  if Not isDefined(D._new) then
    ActiveBuffer.Data:=FieldMapper.CreateRow
end;

procedure TDADataset.InternalDelete;
begin
  inherited InternalDelete;
  asm
    var len=this.FDeletedRows.length-1;
    this.FDeletedRows[len]._old=this.FDeletedRows[len]._new.slice();
  end;
end;

function TDADataset.GetDAFields: TDAFieldArray;

begin
  if Assigned(metadata) and Metadata.HasOwnProperty('fields') and isArray(MetaData['fields']) then
    Result:=TDAFieldArray(Metadata['fields'])
  else
    Result:=nil;
end;

function TDADataset.GetExcludedFields: TNativeIntDynArray;

Var
  Flds : TDAFieldArray;
  I : Integer;

begin
  Result:=[];
  Flds:=GetDaFields;
  For I:=0 to Length(Flds)-1 do
    if not Flds[i].logChanges then
      TJSArray(Result).Push(i);
end;

function TDADataset.GetKeyFields: TStringDynArray;

Var
  Flds : TDAFieldArray;
  I,aLen : Integer;
begin
  Result:=[];
  Flds:=GetDaFields;
  aLen:=0;
  SetLength(Result,Length(Flds));
  For I:=0 to Length(Flds)-1 do
    begin
    if Flds[i].HasOwnProperty('inPrimaryKey') and  SameText(Flds[i].inPrimaryKey,'True') then
      begin
      Result[aLen]:=Flds[i].Name;
      Inc(aLen);
      end;
    end;
  SetLength(Result,aLen);
end;


function TDADataset.GetLoggedFields: TLogFieldArray;

Var
  Flds : TDAFieldArray;
  I,aLen : Integer;
begin
  Result:=[];
  Flds:=GetDaFields;
  aLen:=0;
  SetLength(Result,Length(Flds));
  For I:=0 to Length(Flds)-1 do
    begin
    if Flds[i].logChanges then
      begin
      Result[aLen].name:=Flds[i].Name;
      Result[aLen].datatype:=Flds[i].type_;
      Inc(aLen);
      end;
    end;
  SetLength(Result,aLen);
end;

function TDADataset.DoGetDataProxy: TDataProxy;
begin
  Result:=TDADataProxy.Create(Self);
  TDADataProxy(Result).Connection:=DAConnection;
end;

function TDADataset.ParamByName(const aName: string): TParam;
begin
  Result:=FParams.ParamByname(aName);
end;

function TDADataset.FindParam(const aName: string): TParam;
begin
  Result:=FParams.FindParam(aName);
end;

constructor TDADataset.create(aOwner: TComponent);
begin
  inherited;
  DataProxy:=nil;
  FParams:=TParams.Create(Self);
  FWhereClauseBuilder:=TDAWhereClauseBuilder.Create;
end;

destructor TDADataset.Destroy;
begin
  FreeAndNil(FWhereClauseBuilder);
  FreeAndNil(FParams);
  Inherited;
end;

procedure TDADataset.CreateFieldDefs(a: TJSArray);

Var
  I : Integer;
  F : TDAField;
  FO : TJSObject absolute F;
  fn,dt : string;
  fs : Integer;
  FT : TFieldType;
  req : boolean;

begin
  FieldDefs.Clear;
  For I:=0 to A.length-1 do
    begin
    F:=TDAField(A.Elements[i]);
    fn:=F.Name;
    // The JSON streamer does not create all properties :(
    if FO.hasOwnProperty('size') then
      begin

      if isString(FO['size']) then
        fs:=StrToInt(String(FO['size']))
      else if isNumber(FO['size']) then
        fs:=F.Size
      else
        fs:=0;
      end
    else
      fs:=0;
    if FO.hasOwnProperty('type') then
      dt:=F.type_
    else
      dt:='string';
    if FO.hasOwnProperty('required') then
      req:=F.Required
    else
      Req:=false;
    Ft:=DataTypeToFieldType(dT);
    if (ft=ftBlob) and (fs=0) then
      fs:=1;
//    Writeln('FieldDef : ',fn,', ',ft,', ',fs);
    FieldDefs.Add(fn,ft,fs,Req);
    end;
end;

function TDADataset.CreateFieldMapper: TJSONFieldMapper;
begin
  Result := TDAArrayFieldMapper.Create;
end;

{ TDADataProxy }

function TDADataProxy.ConvertParams(DADS : TDADataset) : TDADataParameterDataArray;

Var
  I : integer;
begin
  Result:=Nil;
  if DADS.Params.Count=0 then
     Exit;
  SetLength(Result,DADS.Params.Count);
  for I:=0 to DADS.Params.Count-1 do
    begin
    Result[i].Name:=DADS.Params[i].Name;
    Result[i].Value:=DADS.Params[i].Value;
//    Writeln('Set param ',Result[i].Name,' to ',Result[i].Value);
    end;
end;

function TDADataProxy.DoGetData(aRequest: TDataRequest): Boolean;

Var
  TN : TDAStringArray;
  TIA : TDATableRequestInfoArray;
  TID : TDATableRequestInfoV5Data;
  TI : TDATableRequestInfoV5;
  Srt : TDAColumnSortingData;
  R : TDADataRequest;
  DADS : TDADataset;
  PA : TDADataParameterDataArray;
  DS : TDADataAbstractService;

begin
  // DA does not support this option...
  if loAtEOF in aRequest.LoadOptions then
    exit(False);
  DADS:=aRequest.Dataset as TDADataset;
  R:=aRequest as TDADatarequest;
  if (Connection=Nil) then
    Raise EDADataset.Create(DADS.Name+': Cannot get data without connection');
  if (DADS.TableName='') then
    Raise EDADataset.Create(DADS.Name+': Cannot get data without tablename');
  DS:=Connection.EnsureDataservice;
  TN:=TDAStringArray.New;
  TN.fromObject([DADS.TableName]);
  TID.maxRecords:=-1;
  TID.IncludeSchema:=True;
  Srt.FieldName:='';
  Srt.SortDirection:='Ascending';
  TID.Sorting:=Srt;
  TID.UserFilter:='';
  if DADS.WhereClause<>'' then
    TID.WhereClause:=DADS.WhereClause;
  PA:=ConvertParams(DADS);
  if Length(PA)>0 then
    TID.Parameters:=Pa;
  TIA:=TDATableRequestInfoArray.new;
  // We need to manually fill the array
  TI:=TDATableRequestInfoV5.New;
  TI.FromObject(TID);
  TJSArray(TIA.items).push(TI);
  DS.GetData(TN,TIA,@R.doSuccess,@R.doFail);
  Result:=True;
end;

function TDADataProxy.GetDataRequestClass: TDataRequestClass;
begin
  Result:=TDADataRequest;
end;


procedure TDADataProxy.ProcessUpdateResult(Res: JSValue;aBatch: TRecordUpdateBatch);

Var
  I : Integer;
  aDelta : TDADelta;
  aDeltas : TDADeltas;
  aStreamer : TDADataStreamer;
  C : TDaChange;
  ResolvedRow : TResolvedRow;

begin
  aStreamer:=Connection.CreateStreamer;
  aStreamer.Stream:=Connection.InterpretMessage(Res);
  aStreamer.initializeRead;
  aDeltas:=aStreamer.ReadDelta;
  if Length(aDeltas.deltas)>1 then
    begin
    For I:=0 to aBatch.List.Count-1 do
      aBatch.List[i].ResolveFailed('More than 1 delta in result');
    Exit;
    end;
  aDelta:=aDeltas.Deltas[0];
  For C in aDelta.data do
    begin
    Case C.Status of
     'failed' :
        aBatch.List[C.recid].ResolveFailed(C.Message);
     'resolved':
        begin
        ResolvedRow:=TResolvedRow.new;
        ResolvedRow.changes:=C;
        ResolvedRow.fields:=aDelta.LoggedFields;
        aBatch.List[C.recid].Resolve(ResolvedRow);
        end;
    end;
    end;
  For I:=0 to aBatch.List.Count-1 do
    if aBatch.List[i].ResolveStatus=rsResolving then
      aBatch.List[i].Resolve(Null);
  If Assigned(aBatch.OnResolve) then
    ABatch.OnResolve(Self,aBatch);
end;


function TDADataProxy.ProcessUpdateBatch(aBatch: TRecordUpdateBatch): Boolean;

  Procedure UpdateSuccess(res : JSValue);

  begin
    ProcessUpdateResult(Res,aBatch)
  end;

  Procedure UpdateFailure(response : TROMessage; Err : TJSError);

  var
     I : Integer;
     aDesc : TRecordUpdateDescriptor;

  begin
    For I:=0 to aBatch.List.Count-1 do
      begin
      aDesc:=aBatch.List[i];
      aDesc.ResolveFailed(extractErrorMsg(Err));
      end;
    If Assigned(aBatch.OnResolve) then
      ABatch.OnResolve(Self,aBatch);
  end;

  Procedure ExcludeItems(aList : TNativeIntDynArray; aValue : TJSValueDynArray);

  Var
     I : Integer;

  begin
    // Backwards or index will shift with each delete!
    for I:=Length(aList)-1 downto 1 do
       TJSArray(aValue).Splice(aList[I],1);
  end;



Const
  ChangeTypes : Array[TUpdateStatus] of String = ('update','insert','delete');

Var
  lDataset : TDaDataset;
  excludedFields : TNativeIntDynArray;
  I : Integer;
  aDesc : TRecordUpdateDescriptor;
  aDelta : TDADelta;
  aDeltas : TDADeltas;
  aChange : TDAChange;
  DS : TDADataAbstractService;
  DStr : TDADataStreamer;
  S : String;


begin
  lDataset:=TDADataset(aBatch.Dataset);
  aDeltas:=TDADeltas.New;
  aDelta:=TDADelta.New;
  aDelta.Name:=lDataset.TableName;
  aDelta.keyFields:=lDataset.GetKeyFields;
  aDelta.loggedFields:=lDataset.GetLoggedFields;
  excludedFields:=lDataset.GetExcludedFields;
  TJSArray(aDeltas.deltas).Push(aDelta);
  For I:=0 to aBatch.List.Count-1 do
    begin
    aDesc:=aBatch.List[i];
    aChange:=TDaChange.New;
    aChange.Status:='pending';
    if aDesc.Status=usInserted then
      aChange.old:=TJSValueDynArray(TJSArray(TDADataRow(aDesc.Data)._new).Slice())
    else
      aChange.old:=TJSValueDynArray(TJSArray(TDADataRow(aDesc.Data)._old).Slice());
    aChange.new_:=TJSValueDynArray(TJSArray(TDADataRow(aDesc.Data)._new).Slice());
    excludeItems(ExcludedFields,aChange.new_);
    excludeItems(ExcludedFields,aChange.old);
    aChange.changeType:=ChangeTypes[aDesc.Status];
    aChange.recid:=I;
    TJSArray(aDelta.data).push(aChange);
    end;
  DStr:=Connection.CreateStreamer;
  DStr.initializeWrite;
  DStr.writeDelta(aDeltas);
  DStr.finalizeWrite;
  S:=DStr.Stream;
  DS:=Connection.EnsureDataservice;
  DS.UpdateData(S,@UpdateSuccess,@UpdateFailure);
  Result:=True;
end;

{ TDADataRequest }

procedure TDADataRequest.DoFail(response: TROMessage; fail: TJSError);

Var
  O : TJSOBject;
  S : TStringDynArray;
  Msg : String;
  I : Integer;

begin
  if isObject(fail) then
    begin
    O:=TJSOBject(JSValue(fail));
    S:=TJSObject.getOwnPropertyNames(O);
    for I:=0 to Length(S)-1 do
      begin
      msg:=Msg+sLineBreak+S[i];
      Msg:=Msg+' : '+String(O[S[i]]);
      end;
    end
  else
    Msg:=TJSJSON.Stringify(Fail);
  Success:=rrFail;
  ErrorMsg:=Msg;
  DoAfterRequest;
end;



procedure TDADataRequest.doSuccess(res: JSValue);

Var
  S : String;
  Rows : TDADataRowArray;
  aRow : TDADataRow;
  DADS : TDADataset;
  DStr : TDADataStreamer;
  DT : TDADatatable;
  I : Integer;

begin
//  Writeln('Data loaded, dataset active: ',Dataset.Active);
  DADS:=Dataset as TDADataset;
  if not Assigned(DADS.DAConnection) then
    Raise EDADataset.Create(DADS.Name+': Cannot process response, connection not available');
  DStr:=DADS.DAConnection.CreateStreamer;
  S:=DADS.DAConnection.InterpretMessage(Res);
  DStr.Stream:=S;
  DStr.initializeRead;
  DT:=TDADataTable.New;
  DT.name:=DADS.TableName;
  DStr.ReadDataset(DT);
  // Writeln('Row count : ',Length(DT.rows));
  SetLength(Rows,Length(DT.rows));
  for I:=0 to length(DT.rows)-1 do
      begin
      aRow:=TDADataRow.New;
      aRow._new:=TJSValueDynArray(DT.Rows[i].__newValues);
      aRow._old:=[];
      Rows[i]:=aRow;
      end;
  (Dataset as TDADataset).Metadata:=New(['fields',TJSArray(DT.Fields)]);
  // Data:=aJSON['data'];
  (Dataset as TDADataset).Rows:=TJSArray(Rows);
  Success:=rrOK;
  DoAfterRequest;
end;

end.
