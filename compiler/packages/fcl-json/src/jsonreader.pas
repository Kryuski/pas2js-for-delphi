{
    This file is part of the Free Component Library

    JSON SAX-like Reader
    Copyright (c) 2007 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$h+}
unit jsonreader;

interface

uses
  Classes, SysUtils, fpJSON, jsonscanner, FPCTypes;
  
Type

  { TBaseJSONReader }

  TBaseJSONReader = Class(TObject)
  Private
    FScanner : TJSONScanner;
    function GetOptions: TJSONOptions; inline;
    procedure SetOptions(AValue: TJSONOptions);
  Protected
    procedure DoError(const Msg: String);
    procedure DoParse(AtCurrent,AllowEOF: Boolean);
    function GetNextToken: TJSONToken;
    function CurrentTokenString: String;
    function CurrentToken: TJSONToken; inline;

    procedure KeyValue(const AKey : TJSONStringType); virtual; abstract;
    procedure StringValue(const AValue : TJSONStringType);virtual; abstract;
    procedure NullValue; virtual; abstract;
    procedure FloatValue(const AValue : Double); virtual; abstract;
    procedure BooleanValue(const AValue : Boolean); virtual; abstract;
    procedure NumberValue(const AValue : TJSONStringType); virtual; abstract;
    procedure IntegerValue(const AValue : integer); virtual; abstract;
    procedure Int64Value(const AValue : int64); virtual; abstract;
    procedure QWordValue(const AValue : QWord); virtual; abstract;
    procedure StartArray; virtual; abstract;
    procedure StartObject; virtual; abstract;
    procedure EndArray; virtual; abstract;
    procedure EndObject; virtual; abstract;

    procedure ParseArray;
    procedure ParseObject;
    procedure ParseNumber;
    procedure DoExecute;
    Property Scanner : TJSONScanner read FScanner;
  Public
    Constructor Create(Source : TStream; AUseUTF8 : Boolean = True); overload;deprecated 'use options form instead';
    Constructor Create(Source : TJSONStringType; AUseUTF8 : Boolean = True); overload;deprecated 'use options form instead';
    constructor Create(Source: TStream; AOptions: TJSONOptions); overload;
    constructor Create(const Source: String; AOptions: TJSONOptions); overload;
    destructor Destroy();override;
    // Parsing options
    Property Options : TJSONOptions Read GetOptions Write SetOptions;
  end;

  TOnJSONBoolean = procedure (Sender : TObject; const AValue : Boolean) of object;
  TOnJSONFloat = procedure (Sender : TObject; const AValue : TJSONFloat) of object;
  TOnJSONInt64 = procedure (Sender : TObject; const AValue : Int64) of object;
  TOnJSONQWord = procedure (Sender : TObject; const AValue : QWord) of object;
  TOnJSONInteger = procedure (Sender : TObject; const AValue : Integer) of object;
  TOnJSONString = procedure (Sender : TObject; const AValue : TJSONStringType) of Object;
  TOnJSONKey = procedure (Sender : TObject; const AKey : TJSONStringType) of Object;


  { TJSONEventReader }

  TJSONEventReader = Class(TBaseJSONReader)
  Private
    FOnBooleanValue: TOnJSONBoolean;
    FOnEndArray: TNotifyEvent;
    FOnEndObject: TNotifyEvent;
    FOnFloatValue: TOnJSONFloat;
    FOnInt64Value: TOnJSONInt64;
    FOnIntegerValue: TOnJSONInteger;
    FOnKeyName: TOnJSONKey;
    FOnNullValue: TNotifyEvent;
    FOnNumberValue: TOnJSONString;
    FOnQWordValue: TOnJSONQWord;
    FOnStartArray: TNotifyEvent;
    FOnStartObject: TNotifyEvent;
    FOnStringValue: TOnJSONString;
  Protected
    procedure KeyValue(const AKey : TJSONStringType); override;
    procedure StringValue(const AValue : TJSONStringType);override;
    procedure NullValue; override;
    procedure FloatValue(const AValue : Double); override;
    procedure BooleanValue(const AValue : Boolean); override;
    procedure NumberValue(const AValue : TJSONStringType); override;
    procedure IntegerValue(const AValue : integer); override;
    procedure Int64Value(const AValue : int64); override;
    procedure QWordValue(const AValue : QWord); override;
    procedure StartArray; override;
    procedure StartObject; override;
    procedure EndArray; override;
    procedure EndObject; override;
  Public
    procedure Execute;
    Property OnNullValue : TNotifyEvent Read FOnNullValue Write FOnNullValue;
    Property OnBooleanValue : TOnJSONBoolean Read FOnBooleanValue Write FOnBooleanValue;
    Property OnNumberValue : TOnJSONString Read FOnNumberValue Write FOnNumberValue;
    Property OnFloatValue : TOnJSONFloat Read FOnFloatValue Write FOnFloatValue;
    Property OnIntegerValue : TOnJSONInteger Read FOnIntegerValue Write FOnIntegerValue;
    Property OnInt64Value : TOnJSONInt64 Read FOnInt64Value Write FOnInt64Value;
    Property OnQWordValue : TOnJSONQWord Read FOnQWordValue Write FOnQWordValue;
    Property OnStringValue : TOnJSONString Read FOnStringValue Write FOnStringValue;
    Property OnKeyName : TOnJSONKey Read FOnKeyName Write FOnKeyName;
    Property OnStartObject : TNotifyEvent Read FOnStartObject Write FOnStartObject;
    Property OnEndObject : TNotifyEvent Read FOnEndObject Write FOnEndObject;
    Property OnStartArray : TNotifyEvent Read FOnStartArray Write FOnStartArray;
    Property OnEndArray : TNotifyEvent Read FOnEndArray Write FOnEndArray;
  end;

  IJSONConsumer = Interface ['{60F9D640-2A69-4AAB-8EE1-0DB6DC614D27}']
    procedure NullValue;
    procedure BooleanValue (const AValue : Boolean);
    procedure NumberValue (const AValue : TJSONStringType);
    procedure FloatValue (const AValue : TJSONFloat);
    procedure Int64Value (const AValue : Int64);
    procedure QWordValue (const AValue : QWord);
    procedure IntegerValue(const AValue : Integer) ;
    procedure StringValue(const AValue : TJSONStringType) ;
    procedure KeyName(const AKey : TJSONStringType);
    procedure StartObject;
    procedure EndObject;
    procedure StartArray;
    procedure EndArray;
  end;

  { TJSONConsumerReader }

  TJSONConsumerReader = Class(TBaseJSONReader)
  Private
    FConsumer: IJSONConsumer;
  Protected
    procedure KeyValue(const AKey : TJSONStringType); override;
    procedure StringValue(const AValue : TJSONStringType);override;
    procedure NullValue; override;
    procedure FloatValue(const AValue : Double); override;
    procedure BooleanValue(const AValue : Boolean); override;
    procedure NumberValue(const AValue : TJSONStringType); override;
    procedure IntegerValue(const AValue : integer); override;
    procedure Int64Value(const AValue : int64); override;
    procedure QWordValue(const AValue : QWord); override;
    procedure StartArray; override;
    procedure StartObject; override;
    procedure EndArray; override;
    procedure EndObject; override;
  Public
    procedure Execute;
    Property Consumer : IJSONConsumer Read FConsumer Write FConsumer;
  end;

  EJSONParser = Class(EParserError);
  
implementation

Resourcestring
  SErrUnexpectedEOF   = 'Unexpected EOF encountered.';
  SErrUnexpectedToken = 'Unexpected token (%s) encountered.';
  SErrExpectedColon   = 'Expected colon (:), got token "%s".';
  //SErrEmptyElement = 'Empty element encountered.';
  SErrExpectedElementName    = 'Expected element name, got token "%s"';
  SExpectedCommaorBraceClose = 'Expected , or ], got token "%s".';
  SErrInvalidNumber          = 'Number is not an integer or real number: %s';
  SErrNoScanner = 'No scanner. No source specified ?';
  
{ TBaseJSONReader }


procedure TBaseJSONReader.DoExecute;

begin
  if (FScanner=Nil) then
    DoError(SErrNoScanner);
  DoParse(False,True);
end;

{
  Consume next token and convert to JSON data structure.
  If AtCurrent is true, the current token is used. If false,
  a token is gotten from the scanner.
  If AllowEOF is false, encountering a tkEOF will result in an exception.
}

function TBaseJSONReader.CurrentToken: TJSONToken;

begin
  Result:=FScanner.CurToken;
end;

function TBaseJSONReader.CurrentTokenString: string;
begin
  If CurrentToken in [tkString,tkIdentifier,tkNumber,tkComment] then
    Result := FScanner.CurTokenString
  else
    Result := TokenInfos[CurrentToken];
end;

procedure TBaseJSONReader.DoParse(AtCurrent, AllowEOF: Boolean);
var
  T: TJSONToken;
begin
  If not AtCurrent then
    T:=GetNextToken
  else
    T:=FScanner.CurToken;
  Case T of
    tkEof : If Not AllowEof then
              DoError(SErrUnexpectedEOF);
    tkNull  : NullValue;
    tkTrue,
    tkFalse : BooleanValue(t=tkTrue);
    tkString : if (joUTF8 in Options) and (DefaultSystemCodePage<>CP_UTF8) then
                 StringValue(TJSONStringType(UTF8ToString(RawByteString(CurrentTokenString))))
               else
                 StringValue(CurrentTokenString);
    tkCurlyBraceOpen :
        ParseObject;
    tkCurlyBraceClose :
        DoError(SErrUnexpectedToken);
    tkSQuaredBraceOpen :
        ParseArray;
    tkSQuaredBraceClose :
        DoError(SErrUnexpectedToken);
    tkNumber :
        ParseNumber;
    tkComma :
        DoError(SErrUnexpectedToken);
    tkIdentifier :
        DoError(SErrUnexpectedToken);
  end;
end;

// Creates the correct JSON number type, based on the current token.
procedure TBaseJSONReader.ParseNumber;
var
  I : Integer;
  I64 : Int64;
  QW  : QWord;
  F : TJSONFloat;
  S : String;
begin
  S:=CurrentTokenString;
  NumberValue(S);
  I:=0;
  if TryStrToQWord(S,QW) then
    begin
    if QW>qword(high(Int64)) then
      QWordValue(QW)
    else
      if QW>MaxInt then
      begin
        I64 := QW;
        Int64Value(I64);
      end
      else
      begin
        I:=QW;
        IntegerValue(I);
      end
    end
  else
    begin
    If TryStrToInt64(S,I64) then
      if (I64>Maxint) or (I64<-MaxInt) then
        Int64Value(I64)
      Else
        begin
        I:=I64;
        IntegerValue(I);
        end
    else
      begin
      I:=0;
      Val(S,F,I);
      If (I<>0) then
        DoError(SErrInvalidNumber);
      FloatValue(F);
      end;
    end;
end;

function TBaseJSONReader.GetOptions: TJSONOptions;
begin
  Result:=FScanner.Options
end;

procedure TBaseJSONReader.SetOptions(AValue: TJSONOptions);
begin
  FScanner.Options:=AValue;
end;


// Current token is {, on exit current token is }
procedure TBaseJSONReader.ParseObject;

var
  T : TJSONtoken;
  LastComma : Boolean;

begin
  LastComma:=False;
  StartObject;
  T:=GetNextToken;
  While T<>tkCurlyBraceClose do
    begin
    If (T<>tkString) and (T<>tkIdentifier) then
      DoError(SErrExpectedElementName);
    KeyValue(CurrentTokenString);
    T:=GetNextToken;
    If (T<>tkColon) then
      DoError(SErrExpectedColon);
    DoParse(False,False);
    T:=GetNextToken;
    If Not (T in [tkComma,tkCurlyBraceClose]) then
      DoError(SExpectedCommaorBraceClose);
    If T=tkComma then
      begin
      T:=GetNextToken;
      LastComma:=(t=tkCurlyBraceClose);
      end;
    end;
  If LastComma and ((joStrict in Options) or not (joIgnoreTrailingComma in Options))  then // Test for ,} case
    DoError(SErrUnExpectedToken);
  EndObject;
end;

// Current token is [, on exit current token is ]
procedure TBaseJSONReader.ParseArray;
var
  T : TJSONtoken;
  LastComma : Boolean;
  S : TJSONOPTions;

begin
  StartArray;
  LastComma:=False;
  Repeat
    T:=GetNextToken;
    If (T<>tkSquaredBraceClose) then
      begin
      DoParse(True,False);
      T:=GetNextToken;
      If Not (T in [tkComma,tkSquaredBraceClose]) then
        DoError(SExpectedCommaorBraceClose);
      LastComma:=(t=TkComma);
      end;
  Until (T=tkSquaredBraceClose);
  S:=Options;
  If LastComma and ((joStrict in S) or not (joIgnoreTrailingComma in S))  then // Test for ,] case
    DoError(SErrUnExpectedToken);
  EndArray;
end;

// Get next token, discarding whitespace
function TBaseJSONReader.GetNextToken: TJSONToken;
begin
  Repeat
    Result:=FScanner.FetchToken;
  Until (Not (Result in [tkComment,tkWhiteSpace]));
end;

procedure TBaseJSONReader.DoError(const Msg: String);
var
  S: String;
begin
  S:=Format(Msg,[CurrentTokenString]);
  S:=Format('Error at line %d, Pos %d:',[FScanner.CurRow,FSCanner.CurColumn])+S;
  Raise EJSONParser.Create(S);
end;

constructor TBaseJSONReader.Create(Source: TStream; AUseUTF8 : Boolean = True);
begin
  Inherited Create;
  FScanner:=TJSONScanner.Create(Source,[joUTF8]);
  if AUseUTF8 then
   Options:=Options + [joUTF8];
end;

constructor TBaseJSONReader.Create(Source: TJSONStringType; AUseUTF8 : Boolean = True);
begin
  Inherited Create;
  FScanner:=TJSONScanner.Create(Source,[joUTF8]);
  if AUseUTF8 then
   Options:=Options + [joUTF8];
end;

constructor TBaseJSONReader.Create(Source: TStream; AOptions: TJSONOptions);
begin
  FScanner:=TJSONScanner.Create(Source,AOptions);
end;

constructor TBaseJSONReader.Create(const Source: String; AOptions: TJSONOptions);
begin
  FScanner:=TJSONScanner.Create(Source,AOptions);
end;

destructor TBaseJSONReader.Destroy();
begin
  FreeAndNil(FScanner);
  inherited Destroy();
end;

{ TJSONReader }

procedure TJSONEventReader.KeyValue(const AKey: TJSONStringType);
begin
  if Assigned(FOnKeyName) then
    FOnKeyName(Self,AKey);
end;

procedure TJSONEventReader.StringValue(const AValue: TJSONStringType);
begin
  if Assigned(FOnStringValue) then
    FOnStringValue(Self,AValue);
end;

procedure TJSONEventReader.NullValue;
begin
  if Assigned(FOnNullValue) then
    FOnNullValue(Self);
end;

procedure TJSONEventReader.FloatValue(const AValue: Double);
begin
  if Assigned(FOnFloatValue) then
    FOnFloatValue(Self,AValue);
end;

procedure TJSONEventReader.BooleanValue(const AValue: Boolean);
begin
  if Assigned(FOnBooleanValue) then
    FOnBooleanValue(Self,AValue);
end;

procedure TJSONEventReader.NumberValue(const AValue: TJSONStringType);
begin
  if Assigned(FOnNumberValue) then
    FOnNumberValue(Self,AValue);
end;

procedure TJSONEventReader.IntegerValue(const AValue: integer);
begin
  if Assigned(FOnIntegerValue) then
    FOnIntegerValue(Self,AValue);

end;

procedure TJSONEventReader.Int64Value(const AValue: int64);
begin
  if Assigned(FOnInt64Value) then
    FOnInt64Value(Self,AValue);

end;

procedure TJSONEventReader.QWordValue(const AValue: QWord);
begin
  if Assigned(FOnQWordValue) then
    FOnQWordValue(Self,AValue);
end;

procedure TJSONEventReader.StartArray;
begin
  If Assigned(FOnStartArray) then
    FOnStartArray(Self);
end;

procedure TJSONEventReader.StartObject;
begin
  if Assigned(FOnStartObject) then
    FOnStartObject(Self);
end;

procedure TJSONEventReader.EndArray;
begin
  If Assigned(FOnEndArray) then
    FOnEndArray(Self);
end;

procedure TJSONEventReader.EndObject;
begin
  If Assigned(FOnEndObject) then
   FOnEndObject(Self);
end;

procedure TJSONEventReader.Execute;
begin
  DoExecute;
end;

{ TJSONConsumerReader }

procedure TJSONConsumerReader.KeyValue(const AKey: TJSONStringType);
begin
  If Assigned(FConsumer) then
    FConsumer.KeyName(Akey)
end;

procedure TJSONConsumerReader.StringValue(const AValue: TJSONStringType);
begin
  If Assigned(FConsumer) then
    FConsumer.StringValue(AValue);
end;

procedure TJSONConsumerReader.NullValue;
begin
  If Assigned(FConsumer) then
    FConsumer.NullValue;
end;

procedure TJSONConsumerReader.FloatValue(const AValue: Double);
begin
  If Assigned(FConsumer) then
    FConsumer.FloatValue(AValue);
end;

procedure TJSONConsumerReader.BooleanValue(const AValue: Boolean);
begin
  If Assigned(FConsumer) then
    FConsumer.BooleanValue(AValue);
end;

procedure TJSONConsumerReader.NumberValue(const AValue: TJSONStringType);
begin
  If Assigned(FConsumer) then
    FConsumer.NumberValue(AValue);
end;

procedure TJSONConsumerReader.IntegerValue(const AValue: integer);
begin
  If Assigned(FConsumer) then
    FConsumer.IntegerValue(AValue);
end;

procedure TJSONConsumerReader.Int64Value(const AValue: int64);
begin
  If Assigned(FConsumer) then
    FConsumer.Int64Value(AValue);
end;

procedure TJSONConsumerReader.QWordValue(const AValue: QWord);
begin
  If Assigned(FConsumer) then
    FConsumer.QWordValue(AValue);
end;

procedure TJSONConsumerReader.StartArray;
begin
  if Assigned(FConsumer) then
    FConsumer.StartArray;
end;

procedure TJSONConsumerReader.StartObject;
begin
  if Assigned(FConsumer) then
    FConsumer.StartObject;
end;

procedure TJSONConsumerReader.EndArray;
begin
  if Assigned(FConsumer) then
    FConsumer.EndArray;
end;

procedure TJSONConsumerReader.EndObject;
begin
  if Assigned(FConsumer) then
    FConsumer.EndObject;
end;

procedure TJSONConsumerReader.Execute;
begin
  DoExecute;
end;


end.

