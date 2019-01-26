{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2017 by Mattias Gaertner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit SysUtils;

{$mode objfpc}

interface

uses
  RTLConsts, js;

procedure FreeAndNil(var Obj);

type
  TProcedure = procedure;

 {*****************************************************************************
                              Various types
  *****************************************************************************}
Const
  FloatRecDigits = 19;

type
  { TFloatRec }
  TFloatRec = Record
     Exponent: Integer;
     Negative: Boolean;
     Digits: Array[0..FloatRecDigits-1] Of Char;
  End;
  TEndian = (Little,Big);
  TFileName = String;
  TByteArray = array [0..32767] of Byte;
  TWordArray = array [0..16383] of Word;
  TBytes = Array of byte;
  TStringArray = array of string;

  TMonthNameArray = array [1..12] of string;
  TDayTable = array [1..12] of Word;
  TWeekNameArray = array [1..7] of string;
  TMonthNames = TMonthNameArray;
  TDayNames = array[0..6] of string;

{*****************************************************************************
                            Exception handling
*****************************************************************************}

  { Exception }

  Exception = class(TObject)
  private
    fMessage: String;
    fHelpContext: Integer;
    {$ifdef NodeJS}
    FNodeJSError: TJSError;
    {$endif}
  public
    constructor Create(const Msg: String); reintroduce;
    constructor CreateFmt(const Msg: string; const Args: array of jsvalue);
    constructor CreateHelp(const Msg: String; AHelpContext: Integer);
    constructor CreateFmtHelp(const Msg: string; const Args: array of jsvalue; AHelpContext: Integer);
    function ToString: String; override;
    property HelpContext: Integer read fHelpContext write fHelpContext;
    property Message: String read fMessage write fMessage;
    {$ifdef NodeJS}
    property NodeJSError: TJSError read FNodeJSError write FNodeJSError;
    {$endif}
  end;

  ExceptClass = class of Exception;

  EExternal = class(Exception);

  { General math errors }
  EMathError  = class(EExternal);
  EInvalidOp  = class(EMathError);
  EZeroDivide = class(EMathError);
  EOverflow   = class(EMathError);
  EUnderflow  = class(EMathError);

  EAbort           = class(Exception);
  EInvalidCast     = class(Exception);
  EAssertionFailed = class(Exception);
  EObjectCheck     = class(Exception);

  { String conversion errors }
  EConvertError = class(Exception);
  EFormatError = class(Exception);


  { integer math exceptions }
  EIntError    = Class(EExternal);
  EDivByZero   = Class(EIntError);
  ERangeError  = Class(EIntError);
  EIntOverflow = Class(EIntError);

  { General math errors }

  { Run-time and I/O Errors }
  EInOutError = class(Exception)
  public
    ErrorCode : Integer;
  end;

  EHeapMemoryError = class(Exception);
  EHeapException = EHeapMemoryError;

  EExternalException = class(EExternal);
  EInvalidPointer  = Class(EHeapMemoryError);
  EOutOfMemory     = Class(EHeapMemoryError);

  { EVariantError }

  EVariantError = Class(Exception)
    ErrCode : longint;
    Constructor CreateCode(Code : Longint);
  end;

  EAccessViolation = Class(EExternal);
  EBusError = Class(EAccessViolation);
  EPrivilege = class(EExternal);
  EStackOverflow = class(EExternal);
  EControlC = class(EExternal);

  { String conversion errors }

  { Other errors }
  EAbstractError   = Class(Exception);

  EPropReadOnly = class(Exception);
  EPropWriteOnly = class(Exception);

  EIntfCastError = class(Exception);
  EInvalidContainer = class(Exception);
  EInvalidInsert = class(Exception);

  EPackageError = class(Exception);

  EOSError = class(Exception)
  public
    ErrorCode: Longint;
  end;

  ESafecallException = class(Exception);
  ENoThreadSupport = Class(Exception);
  ENoWideStringSupport = Class(Exception);
  ENotImplemented = class(Exception);

  EArgumentException = class(Exception);
  EArgumentOutOfRangeException = class(EArgumentException);
  EArgumentNilException = class(EArgumentException);

  EPathTooLongException = class(Exception);
  ENotSupportedException = class(Exception);
  EDirectoryNotFoundException = class(Exception);
  EFileNotFoundException = class(Exception);
  EPathNotFoundException = class(Exception);

  ENoConstructException = class(Exception);


//function GetTickCount: Integer;


{*****************************************************************************
                            String function
*****************************************************************************}

Const
   EmptyStr = '';
   EmptyWideStr = ''; // No difference here.
   HexDisplayPrefix: string = '$';
   LeadBytes = [] unimplemented;

Function CharInSet(Ch: Char;Const CSet : array of char) : Boolean;

function LeftStr(const S: string; Count: Integer): String; assembler;
function RightStr(const S: string; Count: Integer): String; assembler;

function Trim(const S: String): String; assembler;
function TrimLeft(const S: String): String; assembler;
function TrimRight(const S: String): String; assembler;

function UpperCase(const s: String): String; assembler; overload;
function LowerCase(const s: String): String; assembler; overload;

function CompareStr(const s1, s2: String): Integer; assembler;
function SameStr(const s1, s2: String): Boolean; assembler;
function CompareText(const s1, s2: String): Integer; assembler;
function SameText(const s1, s2: String): Boolean; assembler;
function AnsiCompareText(const s1, s2: String): Integer; assembler;
function AnsiSameText(const s1, s2: String): Boolean; assembler;
function AnsiCompareStr(const s1, s2: String): Integer;
procedure AppendStr(var Dest: String; const S: string);

function Format(const Fmt: String; const Args: array of JSValue): String;

function BytesOf(const AVal: string): TBytes;
function StringOf(const ABytes: TBytes): string;

// JavaScript built-in functions
function LocaleCompare(const s1, s2, locales: String): Boolean; assembler; overload;
function NormalizeStr(const S: String; const Norm: String = 'NFC'): String; assembler; overload; // not in IE

function IsValidIdent(const Ident: string; AllowDots: Boolean = False; StrictDots: Boolean = False): Boolean;

Type
  TStringReplaceFlag = (rfReplaceAll, rfIgnoreCase);
  TReplaceFlag = TStringReplaceFlag;
  TStringReplaceFlags = set of TStringReplaceFlag;
  TReplaceFlags = TStringReplaceFlags;

function StringReplace(aOriginal, aSearch, aReplace: string; Flags: TStringReplaceFlags): String;
function QuoteString(aOriginal: String; AQuote: Char): String;
function QuotedStr(const s: string; QuoteChar: Char = ''''): string;
function DeQuoteString(aQuoted: String; AQuote: Char): String;
function IsDelimiter(const Delimiters, S: string; Index: Integer): Boolean;
function AdjustLineBreaks(const S: string): string;
function AdjustLineBreaks(const S: string; Style: TTextLineBreakStyle): string;
function WrapText(const Line, BreakStr: string; const BreakChars: Array of char;  MaxCol: Integer): string;
function WrapText(const Line: string; MaxCol: Integer): string;

{ *****************************************************************************
  Integer conversions
  *****************************************************************************}

function IntToStr(const Value: Integer): string;
Function TryStrToInt(const S : String; Out res : Integer) : Boolean;
Function TryStrToInt(const S : String; Out res : NativeInt) : Boolean;
Function StrToIntDef(const S : String; Const aDef : Integer) : Integer;
Function StrToIntDef(const S : String; Const aDef : NativeInt) : NativeInt;
Function StrToInt(const S : String) : Integer;
Function StrToNativeInt(const S : String) : NativeInt;
// For compatibility
Function StrToInt64(const S : String) : NativeLargeInt;
Function StrToInt64Def(const S : String; ADefault : NativeLargeInt) : NativeLargeInt;
Function TryStrToInt64(const S : String; Out res : NativeLargeInt) : Boolean;
Function StrToQWord(const S : String) : NativeLargeUInt;
Function StrToQWordDef(const S : String; ADefault : NativeLargeUInt) : NativeLargeUInt;
Function TryStrToQWord(const S : String; Out res : NativeLargeUInt) : Boolean;
Function StrToUInt64(const S : String) : NativeLargeUInt;
Function StrToUInt64Def(const S : String; ADefault : NativeLargeUInt) : NativeLargeUInt;
Function TryStrToUInt64(const S : String; Out res : NativeLargeUInt) : Boolean;
Function StrToDWord(const S : String) : DWord;
Function StrToDWordDef(const S : String; ADefault : DWord) : DWord;
Function TryStrToDWord(const S : String; Out res : DWord) : Boolean;

function IntToHex(Value: NativeInt; Digits: integer): string;

{ *****************************************************************************
  Float conversions
  *****************************************************************************}

const
  // Note: Currency is internally a double, multiplied by 10000 and truncated.
  // The below values are the safe limits, within every step exists.
  // Since currency is a double it can take much larger values, but the result
  // may differ from Delphi/FPC
  MaxCurrency: Currency =  450359962737.0495; // fpc: 922337203685477.5807;
  MinCurrency: Currency = -450359962737.0496; // fpc: -922337203685477.5808;

Type
  TFloatFormat = (ffFixed,ffGeneral,ffExponent,ffNumber,ffCurrency);

Function FloatToDecimal(Value : double; Precision, Decimals : integer) :  TFloatRec;
Function FloatToStr(Value: Double): String;
Function FloatToStrF(const Value : double; format: TFloatFormat; Precision, Digits: Integer): String;
Function TryStrToFloat(const S : String; Out res : Double) : Boolean;
Function StrToFloatDef(const S : String; Const aDef : Double) : Double;
Function StrToFloat(const S : String) : Double;
Function FormatFloat (Fmt : String; aValue : Double) : String;

{ *****************************************************************************
  Boolean conversions
  *****************************************************************************}

Var
  TrueBoolStrs, FalseBoolStrs : Array of String;

function StrToBool(const S: String): Boolean;
function BoolToStr(B: Boolean; UseBoolStrs:Boolean=False): string;
function BoolToStr(B: Boolean; const TrueS, FalseS: String): string;
function StrToBoolDef(const S: String; Default: Boolean): Boolean;
function TryStrToBool(const S: String; out Value: Boolean): Boolean;


{*****************************************************************************
                              OS/Environment
*****************************************************************************}

Const
  ConfigExtension : String = '.cfg';
  SysConfigDir    : String = '';

type
  TOnGetEnvironmentVariable = function(Const EnvVar: String): String;
  TOnGetEnvironmentString = function(Index: Integer): String;
  TOnGetEnvironmentVariableCount = function: Integer;
var
  OnGetEnvironmentVariable: TOnGetEnvironmentVariable;
  OnGetEnvironmentString: TOnGetEnvironmentString;
  OnGetEnvironmentVariableCount: TOnGetEnvironmentVariableCount;

function GetEnvironmentVariable(Const EnvVar: String): String;
function GetEnvironmentVariableCount: Integer;
function GetEnvironmentString(Index: Integer): String;

procedure ShowException(ExceptObject: TObject; ExceptAddr: Pointer);
Procedure Abort;

{*****************************************************************************
                               Events
*****************************************************************************}
Type
  TEventType = (etCustom,etInfo,etWarning,etError,etDebug);
  TEventTypes = Set of TEventType;

{*****************************************************************************
                            Date and time
*****************************************************************************}
Type
  
  TSystemTime = record
     Year, Month, Day, DayOfWeek: word;
     Hour, Minute, Second, MilliSecond: word;
  end ;
  TTimeStamp = record
     Time: longint;   { Number of milliseconds since midnight }
     Date: longint;   { One plus number of days since 1/1/0001 }
  end ;


Var
  TimeSeparator : char = ':';
  DateSeparator : char = '-';
  ShortDateFormat : string = 'yyyy-mm-dd';
  LongDateFormat : string = 'ddd, yyyy-mm-dd';
  ShortTimeFormat : string = 'hh:nn';
  LongTimeFormat : string = 'hh:nn:ss';
  DecimalSeparator : string = '.';
  ThousandSeparator : string;
  TimeAMString : string = 'AM';
  TimePMString : string = 'PM';

const

  HoursPerDay = 24;
  MinsPerHour = 60;
  SecsPerMin  = 60;
  MSecsPerSec = 1000;
  MinsPerDay  = HoursPerDay * MinsPerHour;
  SecsPerDay  = MinsPerDay * SecsPerMin;
  MSecsPerDay = SecsPerDay * MSecsPerSec;
  MaxDateTime: TDateTime =  2958465.99999999;
  MinDateTime: TDateTime =  -693593.99999999;

  JulianEpoch = TDateTime(-2415018.5);
  UnixEpoch = JulianEpoch + TDateTime(2440587.5);

  DateDelta = 693594;        // Days between 1/1/0001 and 12/31/1899
  UnixDateDelta = 25569;

  { True=Leapyear }

Var
  MonthDays : array [Boolean] of TDayTable =
    ((31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31),
     (31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31));
  ShortMonthNames : TMonthNames = (
    'Jan',
    'Feb',
    'Mar',
    'Apr',
    'May',
    'Jun',
    'Jul',
    'Aug',
    'Sep',
    'Oct',
    'Nov',
    'Dec');
  LongMonthNames : TMonthNames = (
    'January',
    'February',
    'March',
    'April',
    'May',
    'June',
    'July',
    'August',
    'September',
    'October',
    'November',
    'December');
  ShortDayNames : TDayNames = (
    'Sun',
    'Mon',
    'Tue',
    'Wed',
    'Thu',
    'Fri',
    'Sat');

  LongDayNames : TDayNames = (
    'Sunday',
    'Monday',
    'Tuesday',
    'Wednesday',
    'Thursday',
    'Friday',
    'Saturday');

type
  // Stub, for easier porting of FPC/Delphi code.
  // Reading/Writing the properties will actually set the global variables

  { TFormatSettings }

  TFormatSettings = class(TObject)
  private
    function GetCurrencyDecimals: Byte;
    function GetCurrencyFormat: Byte;
    function GetCurrencyString: String;
    function GetDateSeparator: char;
    function GetDecimalSeparator: string;
    function GetLongDateFormat: string;
    function GetLongDayNames: TDayNames;
    function GetLongMonthNames: TMonthNames;
    function GetLongTimeFormat: string;
    function GetNegCurrFormat: Byte;
    function GetShortDateFormat: string;
    function GetShortDayNames: TDayNames;
    function GetShortMonthNames: TMonthNames;
    function GetShortTimeFormat: string;
    function GetThousandSeparator: string;
    function GetTimeAMString: string;
    function GetTimePMString: string;
    function GetTimeSeparator: char;
    procedure SetCurrencyFormat(AValue: Byte);
    procedure SetCurrencyString(AValue: String);
    procedure SetDateSeparator(const Value: char);
    procedure SetDecimalSeparator(const Value: string);
    procedure SetLongDateFormat(const Value: string);
    procedure SetLongDayNames(AValue: TDayNames);
    procedure SetLongMonthNames(AValue: TMonthNames);
    procedure SetLongTimeFormat(const Value: string);
    procedure SetNegCurrFormat(AValue: Byte);
    procedure SetShortDateFormat(const Value: string);
    procedure SetShortDayNames(AValue: TDayNames);
    procedure SetShortMonthNames(AValue: TMonthNames);
    procedure SetShortTimeFormat(const Value: string);
    procedure SetCurrencyDecimals(AValue: Byte);
    procedure SetThousandSeparator(const Value: string);
    procedure SetTimeAMString(const Value: string);
    procedure SetTimePMString(const Value: string);
    procedure SetTimeSeparator(const Value: char);
  public
    Property ShortMonthNames : TMonthNames Read GetShortMonthNames Write SetShortMonthNames;
    Property LongMonthNames : TMonthNames Read GetLongMonthNames Write SetLongMonthNames;
    Property ShortDayNames : TDayNames Read GetShortDayNames Write SetShortDayNames;
    Property LongDayNames : TDayNames Read GetLongDayNames Write SetLongDayNames;
    property TimeSeparator : char read GetTimeSeparator write SetTimeSeparator;
    property DateSeparator : char read GetDateSeparator write SetDateSeparator;
    property ShortDateFormat : string read GetShortDateFormat write SetShortDateFormat;
    property LongDateFormat : string read GetLongDateFormat write SetLongDateFormat;
    property ShortTimeFormat : string read GetShortTimeFormat write SetShortTimeFormat;
    property LongTimeFormat : string read GetLongTimeFormat write SetLongTimeFormat;
    property DecimalSeparator : string read GetDecimalSeparator write SetDecimalSeparator;
    property ThousandSeparator : string read GetThousandSeparator write SetThousandSeparator;
    property TimeAMString : string read GetTimeAMString write SetTimeAMString;
    property TimePMString : string read GetTimePMString write SetTimePMString;
    Property CurrencyFormat : Byte read GetCurrencyFormat Write SetCurrencyFormat;
    Property NegCurrFormat : Byte read GetNegCurrFormat Write SetNegCurrFormat;
    Property CurrencyDecimals : Byte Read GetCurrencyDecimals Write SetCurrencyDecimals;
    Property CurrencyString : String Read GetCurrencyString Write SetCurrencyString;
  end;

Var
  FormatSettings: TFormatSettings;
  TwoDigitYearCenturyWindow : word = 50;
                             { Threshold to be subtracted from year before age-detection.}


function DateTimeToJSDate(aDateTime : TDateTime) : TJSDate;
function JSDateToDateTime(aDate : TJSDate) : TDateTime;

function DateTimeToTimeStamp(DateTime: TDateTime): TTimeStamp;
function TimeStampToDateTime(const TimeStamp: TTimeStamp): TDateTime;
function MSecsToTimeStamp(MSecs: NativeInt): TTimeStamp;
function TimeStampToMSecs(const TimeStamp: TTimeStamp): NativeInt;
function TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean;
function TryEncodeTime(Hour, Min, Sec, MSec: Word; out Time: TDateTime): Boolean;
function EncodeDate(Year, Month, Day :word): TDateTime;
function EncodeTime(Hour, Minute, Second, MilliSecond:word): TDateTime;
function ComposeDateTime(Date,Time : TDateTime) : TDateTime;
procedure DecodeDate(Date: TDateTime; out Year, Month, Day: word);
function DecodeDateFully(const DateTime: TDateTime; out Year, Month, Day, DOW: Word): Boolean;
procedure DecodeTime(Time: TDateTime; out Hour, Minute, Second, MilliSecond: word);
procedure DateTimeToSystemTime(DateTime: TDateTime; out SystemTime: TSystemTime);
function SystemTimeToDateTime(const SystemTime: TSystemTime): TDateTime;
function DayOfWeek(DateTime: TDateTime): integer;
function Date: TDateTime;
function Time: TDateTime;
function Now: TDateTime;
function IncMonth(const DateTime: TDateTime; NumberOfMonths: integer = 1 ): TDateTime;
procedure IncAMonth(var Year, Month, Day: Word; NumberOfMonths: Integer = 1);
function IsLeapYear(Year: Word): boolean;
function DateToStr(Date: TDateTime): string;
// function DateToStr(Date: TDateTime; const FormatSettings: TFormatSettings): string;
function TimeToStr(Time: TDateTime): string;
// function TimeToStr(Time: TDateTime; const FormatSettings: TFormatSettings): string;
function DateTimeToStr(DateTime: TDateTime; ForceTimeIfZero : Boolean = False): string;
// function DateTimeToStr(DateTime: TDateTime; const FormatSettings: TFormatSettings; ForceTimeIfZero : Boolean = False): string;
function StrToDate(const S: String): TDateTime;
function StrToDate(const S: String; separator : char): TDateTime;
function StrToDate(const S: String; const useformat : string; separator : char): TDateTime;
//function StrToDate(const S: string; FormatSettings : TFormatSettings): TDateTime;
function StrToTime(const S: String): TDateTime;
function StrToTime(const S: String; separator : char): TDateTime;
// function StrToTime(const S: string; FormatSettings : TFormatSettings): TDateTime;
function StrToDateTime(const S: String): TDateTime;
//function StrToDateTime(const s: ShortString; const FormatSettings : TFormatSettings): TDateTime;
function FormatDateTime(const FormatStr: string; const DateTime: TDateTime): string;
// function FormatDateTime(const FormatStr: string; DateTime: TDateTime; const FormatSettings: TFormatSettings; Options : TFormatDateTimeOptions = []): string;
function TryStrToDate(const S: String; out Value: TDateTime): Boolean;
function TryStrToDate(const S: String; out Value: TDateTime; separator : char): Boolean;
function TryStrToDate(const S: String; out Value: TDateTime; const useformat : string; separator : char): Boolean;
// function TryStrToDate(const S: string; out Value: TDateTime; const FormatSettings: TFormatSettings): Boolean;
function TryStrToTime(const S: String; out Value: TDateTime): Boolean;
function TryStrToTime(const S: String; out Value: TDateTime; separator : char): Boolean;
function TryStrToDateTime(const S: String; out Value: TDateTime): Boolean;
// function TryStrToTime(const S: string; out Value: TDateTime; const FormatSettings: TFormatSettings): Boolean;
// function TryStrToDateTime(const S: string; out Value: TDateTime; const FormatSettings: TFormatSettings): Boolean;
function StrToDateDef(const S: String; const Defvalue : TDateTime): TDateTime;
function StrToDateDef(const S: String; const Defvalue : TDateTime; separator : char): TDateTime;
function StrToTimeDef(const S: String; const Defvalue : TDateTime): TDateTime;
function StrToTimeDef(const S: String; const Defvalue : TDateTime; separator : char): TDateTime;
function StrToDateTimeDef(const S: String; const Defvalue : TDateTime): TDateTime;
function CurrentYear:Word;
procedure ReplaceTime(var dati: TDateTime; NewTime : TDateTime);
procedure ReplaceDate(var DateTime: TDateTime; const NewDate: TDateTime);
Function FloatToDateTime (Const Value : Extended) : TDateTime;

{ *****************************************************************************
  Currency support
  *****************************************************************************}

Var
  CurrencyFormat : Byte = 0;
  NegCurrFormat : Byte = 0;
  CurrencyDecimals : Byte = 2;
  CurrencyString : String = '$';

Function FloattoCurr (Const Value : Extended) : Currency;
function TryFloatToCurr(const Value: Extended; var AResult: Currency): Boolean;
Function CurrToStr(Value: Currency): string;
//Function CurrToStr(Value: Currency; Const FormatSettings: TFormatSettings): string;
function StrToCurr(const S: string): Currency;
//function StrToCurr(const S: string; Const FormatSettings: TFormatSettings): Currency;
function TryStrToCurr(const S: string;Out Value : Currency): Boolean;
//function TryStrToCurr(const S: string;Out Value : Currency; Const FormatSettings: TFormatSettings): Boolean;
function StrToCurrDef(const S: string; Default : Currency): Currency;
//function StrToCurrDef(const S: string; Default : Currency; Const FormatSettings: TFormatSettings): Currency;

{*****************************************************************************
                               File Paths
*****************************************************************************}
type
  PathStr = String;
  TPathStrArray = Array of PathStr;

function ChangeFileExt(const FileName, Extension: PathStr): PathStr;
function ExtractFilePath(const FileName: PathStr): PathStr;
function ExtractFileDrive(const FileName: PathStr): PathStr;
function ExtractFileName(const FileName: PathStr): PathStr;
function ExtractFileExt(const FileName: PathStr): PathStr;
function ExtractFileDir(Const FileName : PathStr): PathStr;
function ExtractRelativepath (Const BaseName,DestName : PathStr): PathStr;
function IncludeTrailingPathDelimiter(Const Path : PathStr) : PathStr;
function ExcludeTrailingPathDelimiter(Const Path: PathStr): PathStr;
function IncludeLeadingPathDelimiter(Const Path : PathStr) : PathStr;
function ExcludeLeadingPathDelimiter(Const Path: PathStr): PathStr;
function IsPathDelimiter(Const Path: PathStr; Index: Integer): Boolean;
Function SetDirSeparators (Const FileName : PathStr) : PathStr;
Function GetDirs (DirName : PathStr) : TPathStrArray;
function ConcatPaths(const Paths: array of PathStr): PathStr;


{*****************************************************************************
                               Interfaces
*****************************************************************************}

const
  GUID_NULL: TGuid = '{00000000-0000-0000-0000-000000000000}';

function Supports(const Instance: IInterface; const AClass: TClass; out Obj): Boolean; overload;
function Supports(const Instance: IInterface; const IID: TGuid; out Intf): Boolean; overload;
function Supports(const Instance: TObject; const IID: TGuid; out Intf): Boolean; overload;
function Supports(const Instance: TObject; const IID: TGuidString; out Intf): Boolean; overload;

function Supports(const Instance: IInterface; const AClass: TClass): Boolean; overload;
function Supports(const Instance: IInterface; const IID: TGuid): Boolean; overload;
function Supports(const Instance: TObject; const IID: TGuid): Boolean; overload;
function Supports(const Instance: TObject; const IID: TGuidString): Boolean; overload;

function Supports(const AClass: TClass; const IID: TGuid): Boolean; overload;
function Supports(const AClass: TClass; const IID: TGuidString): Boolean; overload;

function TryStringToGUID(const s: string; out Guid: TGuid): Boolean;
function StringToGUID(const S: string): TGuid;
function GUIDToString(const guid: TGuid): string;
function IsEqualGUID(const guid1, guid2: TGuid): Boolean;
function GuidCase(const guid: TGuid; const List: array of TGuid): Integer;
Function CreateGUID(out GUID : TGUID) : Integer;

implementation

procedure ShowException(ExceptObject: TObject; ExceptAddr: Pointer);

Var
  S : String;

begin
  S:='Application raised an exception '+ExceptObject.ClassName;
  if ExceptObject is Exception then
    S:=S+' : '+Exception(ExceptObject).Message;
{$IFDEF BROWSER}
  asm
    window.alert(S);
  end;
{$ENDIF}
{$IFDEF NODEJS}
  Writeln(S);
{$ENDIF}
  if ExceptAddr=nil then;
end;

Const
  SAbortError = 'Operation aborted';

procedure Abort;
begin
  Raise EAbort.Create(SAbortError);
end;

Type
  TCharSet = Set of Char;
Function CharInSet(Ch: Char;Const CSet : TCharSet) : Boolean;

begin
  Result:=Ch in CSet;
end;

Function CharInSet(Ch: Char;Const CSet : array of char) : Boolean;

Var
  I : integer;

begin
  Result:=False;
  I:=Length(CSet)-1;
  While (Not Result) and (I>=0) do
    begin
    Result:=(Ch=CSet[i]);
    Dec(I);
    end;
end;

function LeftStr(const S: string; Count: Integer): String; assembler;
asm
  return (Count>0) ? S.substr(0,Count) : "";
end;

function RightStr(const S: string; Count: Integer): String; assembler;
asm
  var l = S.length;
  return (Count<1) ? "" : ( Count>=l ? S : S.substr(l-Count));
end;

function Trim(const S: String): String; assembler;
asm
  return S.trim();
end;

function TrimLeft(const S: String): String; assembler;
asm
  return S.replace(/^[\s\uFEFF\xA0\x00-\x1f]+/,'');
end;

function TrimRight(const S: String): String; assembler;
asm
  return S.replace(/[\s\uFEFF\xA0\x00-\x1f]+$/,'');
end;

function IntToStr(const Value: Integer): string;
begin
  Result:=str(Value);
end;

Function FloatToDecimal(Value : double; Precision, Decimals : integer) :  TFloatRec;

Const
  Rounds = '123456789:';

var
  Buffer: String;  //Though str func returns only 25 chars, this might change in the future
  InfNan: string;
  OutPos,Error, N, L, C: Integer;
  GotNonZeroBeforeDot, BeforeDot : boolean;

begin
  Result.Negative:=False;
  Result.Exponent:=0;
  For C:=0 to FloatRecDigits do
    Result.Digits[C]:='0';
  if Value=0 then
    exit;
  asm
    Buffer=Value.toPrecision(21); // Double precision
  end;
  // Writeln('Buffer :',Buffer);
  N := 1;
  L := Length(Buffer);
  while Buffer[N]=' ' do
    Inc(N);
  Result.Negative := (Buffer[N] = '-');
  if Result.Negative then
    Inc(N)
  else if (Buffer[N] = '+') then
    inc(N);
  { special cases for Inf and Nan }
  if (L>=N+2) then
    begin
      InfNan:=copy(Buffer,N,3);
      if (InfNan='Inf') then
        begin
          Result.Digits[0]:=#0;
          Result.Exponent:=32767;
          exit
        end;
      if (InfNan='Nan') then
        begin
          Result.Digits[0]:=#0;
          Result.Exponent:=-32768;
          exit
        end;
    end;
  //Start := N;  //Start of digits
  Outpos:=0;
  Result.Exponent := 0; BeforeDot := true;
  GotNonZeroBeforeDot := false;
  while (L>=N) and (Buffer[N]<>'E') do
    begin
      // Writeln('Examining : ',Buffer[N],'( output pos: ',outPos,')');
      if Buffer[N]='.' then
        BeforeDot := false
      else
        begin
        if BeforeDot then
          begin  // Currently this is always 1 char
            Inc(Result.Exponent);
            Result.Digits[Outpos] := Buffer[N];
            if Buffer[N] <> '0' then
              GotNonZeroBeforeDot := true;
          end
        else
          Result.Digits[Outpos] := Buffer[N];
        Inc(outpos);
        end;
      Inc(N);
    end;
  Inc(N); // Pass through 'E'
  if N<=L then
    begin
      Val(Copy(Buffer, N, L-N+1), C, Error); // Get exponent after 'E'
      Inc(Result.Exponent, C);
    end;
  // Calculate number of digits we have from str
  N:=OutPos;
  // Writeln('Number of digits: ',N,' requested precision : ',Precision);
  L:=Length(Result.Digits);
  While N<L do
    begin
    Result.Digits[N]:='0';  //Zero remaining space
    Inc(N);
    end;
  if Decimals + Result.Exponent < Precision Then //After this it is the same as in FloatToDecimal
    N := Decimals + Result.Exponent
  Else
    N := Precision;
  if N >= L Then
    N := L-1;
  // Writeln('Rounding on digit : ',N);
  if N = 0 Then
    begin
      if Result.Digits[0] >= '5' Then
        begin
          Result.Digits[0] := '1';
          Result.Digits[1] := #0;
          Inc(Result.Exponent);
        end
      Else
        Result.Digits[0] := #0;
    end  //N=0
  Else if N > 0 Then
    begin
      if Result.Digits[N] >= '5' Then
        begin
          Repeat
            Result.Digits[N] := #0;
            Dec(N);
            // Writeln(N,': ',Result.Digits[N],', Rounding to : ',Rounds[StrToInt(Result.Digits[N])]);
            Result.Digits[N]:=Rounds[StrToInt(Result.Digits[N])+1];
          Until (N = 0) Or (Result.Digits[N] < ':');
          If Result.Digits[0] = ':' Then
            begin
              Result.Digits[0] := '1';
              Inc(Result.Exponent);
            end;
        end
      Else
        begin
          Result.Digits[N] := '0';
          While (N > -1) And (Result.Digits[N] = '0') Do
            begin
              Result.Digits[N] := #0;
              Dec(N);
            end;
        end;
      end //N>0
  Else
    Result.Digits[0] := #0;
  if (Result.Digits[0] = #0) and
     not GotNonZeroBeforeDot then
    begin
      Result.Exponent := 0;
      Result.Negative := False;
    end;
end;


function FloatToStr(Value: Double): String;
begin
  Result:=FloatToStrF(Value,ffGeneral,15,0);
end;

Function TryStrToFloat(const S : String; Out res : Double) : Boolean;

Var
  J : JSValue;
  N : String;

begin
  N:=S;
  // Delocalize
  if (ThousandSeparator <>'') then
    N:=StringReplace(N,ThousandSeparator,'',[rfReplaceAll]);
  if (DecimalSeparator<>'.') then
    N:=StringReplace(N,DecimalSeparator,'.',[]);
  J:=parseFloat(N);
  Result:=Not jsIsNaN(J);
  if Result then
    Res:=Double(J);
end;

Function StrToFloatDef(const S : String; Const aDef : Double) : Double;

begin
  if not TryStrToFloat(S,Result) then
    Result:=aDef;
end;

Function StrToFloat(const S : String) : Double;
begin
  if not TryStrToFloat(S,Result) then
    Raise EConvertError.CreateFmt(SErrInvalidFloat,[S]);
end;

function FormatFloat(Fmt: String; aValue: Double): String;

Type
  TPosArray = Array of Integer;

const
  MaxPrecision = 18;  // Extended precision

var
  // Input in usable format
  E : Extended;              // Value as extended.
  FV: TFloatRec;             // Value as floatrec.
  Section : String;          // Format can contain 3 sections, semicolon separated: Pos;Neg;Zero. This is the one to use.
  SectionLength : Integer;   // Length of section.
  // Calculated based on section. Static during output
  ThousandSep: Boolean;      // Thousands separator detected in format ?
  IsScientific: Boolean;     // Use Scientific notation ? (E detected in format)
  DecimalPos: Integer;       // Position of decimal point in pattern.
  FirstDigit: Integer;       // First actual digit in input (# or 0), relative to decimal point
  LastDigit: Integer;        // Last required (0) digit, relative to decimal point
  RequestedDigits: Integer;  // Number of requested digits, # and 0 alike
  ExpSize : Integer;         // Number of digits in exponent
  Available: Integer;        // Available digits in FV.
  // These change during output loop
  Current: Integer;          // Current digit in available digits
  PadZeroes: Integer;        // Difference in requested digits before comma and exponent, needs to be padded with zeroes.
  DistToDecimal: Integer;    // Place of current digit, relative to decimal point taking in account PadZeroes!

  Procedure InitVars;

  begin
    E:=aValue;
    Section:='';
    SectionLength:=0;
    ThousandSep:=false;
    IsScientific:=false;
    DecimalPos:=0;
    FirstDigit:=MaxInt;
    LastDigit:=0;
    RequestedDigits:=0;
    ExpSize:=0;
    Available:=-1;
  end;

  procedure ToResult(const AChar: Char);
  begin
    Result:=Result+AChar;
  end;

  procedure AddToResult(const AStr: String);

  begin
    Result:=Result+AStr;
  end;

  procedure WriteDigit(ADigit: Char);

  // Write a digit to result, prepend with decimalseparator or append with 1000 separator

  begin
    if ADigit=#0 then exit;
    // Writeln('WriteDigit: ',ADigit,', DistToDecimal: ',DistToDecimal);
    Dec(DistToDecimal);
    // -1 -> we've arrived behind the decimal
    if (DistToDecimal=-1) then
      begin
      AddToResult(DecimalSeparator);
      ToResult(ADigit);
      end
    else
      begin
      // We're still before the decimal.
      ToResult(ADigit);
      if ThousandSep and ((DistToDecimal mod 3)=0) and (DistToDecimal>1) then
        AddToResult(ThousandSeparator);
      end;
  end;

  Function GetDigit : Char;

  // Return next digit from available digits.
  // May return #0 if none available.
  // Will return '0' if applicable.

  begin
    // Writeln(' DistToDecimal <= LastDigit : ',DistToDecimal,' <= ',LastDigit,' have digit: ',Current<=Available, ' (',Current,')');
    Result:=#0;
    if (Current<=Available) then
      begin
      Result:=FV.Digits[Current];
      Inc(Current);
      end
    else if (DistToDecimal <= LastDigit) then
      Dec(DistToDecimal)
    else
      Result:='0';
    // Writeln('GetDigit ->: ',Result);
  end;

  procedure CopyDigit;

  // Copy a digit (#, 0) to the output with the correct value

  begin
    // Writeln('CopyDigit: Padzeroes: ',PadZeroes,', DistToDecimal: ',DistToDecimal);
    if (PadZeroes=0) then
      WriteDigit(GetDigit) // No shift needed, just copy what is available.
    else if (PadZeroes<0) then
      begin
      // We must prepend zeroes
      Inc(PadZeroes);
      if (DistToDecimal<=FirstDigit) then
        WriteDigit('0')
      else
        Dec(DistToDecimal);
      end
    else
      begin
      // We must append zeroes
      while PadZeroes > 0 do
        begin
        WriteDigit(GetDigit);
        Dec(PadZeroes);
        end;
      WriteDigit(GetDigit);
      end;
  end;

  Function GetSections(Var SP : TPosArray) : Integer;

  var
    FL : Integer;
    i : Integer;
    C,Q : Char;
    inQuote : Boolean;

  begin
    Result:=1;
    SP[1]:=-1;
    SP[2]:=-1;
    SP[3]:=-1;
    inQuote:=False;
    Q:=#0;
    I:=1;
    FL:=Length(Fmt);
    while (I<=FL) do
      begin
      C:=Fmt[I];
      case C of
      ';':
        begin
        if not InQuote then
          begin
          if Result>3 then
            Raise Exception.Create('Invalid float format');
          SP[Result]:=I+1;
          Inc(Result);
          end;
        end;
      '"','''':
        begin
        if InQuote then
          InQuote:=C<>Q
        else
          begin
          InQuote:=True;
          Q:=C;
          end;
        end;
      end;
      Inc(I);
      end;
    if SP[Result]=-1 then
      SP[Result]:=FL+1;
  end;

  Procedure AnalyzeFormat;

  var
    I,Len: Integer;
    Q,C: Char;
    InQuote : Boolean;

  begin
    Len:=Length(Section);
    I:=1;
    InQuote:=False;
    Q:=#0;
    while (I<=Len) do
      begin
      C:=Section[i];
      if C in ['"',''''] then
        begin
        if InQuote then
          InQuote:=C<>Q
        else
          begin
          InQuote:=True;
          Q:=C;
          end;
        end
      else if not InQuote then
        case C of
        '.':
          if (DecimalPos=0) then
            DecimalPos:=RequestedDigits+1;
        ',':
            ThousandSep:=ThousandSeparator<>#0;
        'e', 'E':
            begin
            Inc(I);
            if (I<Len) then
              begin
              C:=Section[i];
              IsScientific:=C in ['-','+'];
              if IsScientific then
                while (I<Len) and (Section[i+1]='0') do
                  begin
                  Inc(ExpSize);
                  Inc(I);
                  end;
              if ExpSize>4 then
                ExpSize:=4;
              end;
            end;
        '#':
            Inc(RequestedDigits);
        '0':
            begin
            if RequestedDigits<FirstDigit then
              FirstDigit:=RequestedDigits+1;
            Inc(RequestedDigits);
            LastDigit:=RequestedDigits+1;
            end;
        end;
      Inc(I);
      end;
    if DecimalPos=0 then
      DecimalPos:=RequestedDigits+1;
    // Writeln('LastDigit: ',DecimalPos,'-',LastDigit);
    LastDigit:=DecimalPos-LastDigit;
    if LastDigit>0 then
      LastDigit:=0;
    // Writeln('FirstDigit: ',DecimalPos,'-',FirstDigit);
    FirstDigit:=DecimalPos-FirstDigit;
    if FirstDigit<0 then
      FirstDigit:=0;
  end;

  Function ValueOutSideScope : Boolean;
  begin
    With FV do
     Result:=((Exponent >= 18) and (not IsScientific)) or (Exponent = $7FF) or (Exponent = $800)
  end;

  Procedure CalcRunVars;

  Var
    D,P: Integer;

  begin
    if IsScientific then
      begin
      P:=RequestedDigits;
      D:=9999;
      end
    else
      begin
      P:=MaxPrecision;
      D:=RequestedDigits-DecimalPos+1;
      end;
    FV:=FloatToDecimal(aValue,P,D);
    // Writeln('Number of digits available : ',Length(FV.Digits));
    // For p:=0 to Length(FV.Digits)-1 do
    //   Writeln(P,': ',FV.Digits[p]);
    DistToDecimal:=DecimalPos-1;
    // Writeln('DistToDecimal : ',DistToDecimal);
    if IsScientific then
      PadZeroes:=0 // No padding.
    else
      begin
      PadZeroes:=FV.Exponent-(DecimalPos-1);
      if (PadZeroes>=0) then
        DistToDecimal:=FV.Exponent
      end;
    // Writeln('PadZeroes : ',PadZeroes, ', DistToDecimal : ',DistToDecimal);
    Available:=-1;
    while (Available<High(FV.Digits)) and (FV.Digits[Available+1]<>#0) do
      Inc(Available);
    // Writeln('Available: ',Available);
  end;

  Function FormatExponent(ASign: Char; aExponent: Integer) : String;

  begin
    Result:=IntToStr(aExponent);
    Result:=StringOfChar('0',ExpSize-Length(Result))+Result;
    if (aExponent<0) then
      Result:='-'+Result
    else if (aExponent>0) and (aSign='+') then
      Result:=aSign+Result;
  end;

var
  I,S : Integer;
  C,Q : Char;
  PA : TPosArray;
  InLiteral : Boolean;

begin
  SetLength(PA,4);
  Result:='';
  Initvars;
  // What section to use ?
  if (E>0) then
    S:=1
  else if (E<0) then
    S:=2
  else
    S:=3;
  PA[0]:=0;
  I:=GetSections(PA);
  if (I<S) or (PA[S]-PA[S-1]=0) then
    S:=1;
  // Extract correct section
  SectionLength:=PA[S]-PA[S-1]-1;
  Section:=Copy(Fmt,PA[S-1]+1,SectionLength);
  SetLength(Section,SectionLength);
  // Writeln('Section ',I,' : "',Section,'" ',SectionLength);
  AnalyzeFormat;
  // Writeln('RequestedDigits: ',RequestedDigits,', DecimalPos : ',DecimalPos,', LastDigit: ',LastDigit,', FirstDigit: ',FirstDigit);
  CalcRunVars;
  // If we cannot process value using current settings, fallback
  if (SectionLength=0) or ValueOutSideScope then
    begin
    asm
     Section=E.toPrecision(15);
    end;
    Result:=Section;
    end;
  // Get Started
  I:=1;
  Current:=0;
  Q:=' ';
  InLiteral:=False;
  if (FV.Negative) and (S=1) then
    ToResult('-');
  while (I<=SectionLength) do
    begin
    C:=Section[i];
    // Writeln('Analyzing pos ',I,': "',C,'"');
    If (C in ['"', '''']) then
      begin
      if InLiteral then
        InLiteral:=C<>Q
      else
        begin
        inLiteral:=True;
        Q:=C;
        end;
      end
    else if InLiteral then
      ToResult(C)
    else
      case C of
      '0', '#':
        CopyDigit;
      '.', ',':
        ; // Do nothing, handled by CopyDigit
      'e', 'E':
        begin
        ToResult(C); // Always needed
        Inc(I);
        if I<=Length(Section) then
          begin
          C:=Section[I];
          if (C in ['+','-']) then
            begin
            AddToResult(FormatExponent(C,FV.Exponent-DecimalPos+1));
            // Skip rest
            while (I<SectionLength) and (Section[i+1]='0') do
              Inc(I);
            end;
          end;
        end;
      else
        ToResult(C);
      end;
    Inc(i);
    end;
end;

function StrToBool(const S: String): Boolean;
begin
  if not(TryStrToBool(S,Result)) then
    raise EConvertError.CreateFmt(SInvalidBoolean,[S]);
end;

procedure CheckBoolStrs;
begin
  if Length(TrueBoolStrs)=0 then
  begin
    SetLength(TrueBoolStrs,1);
    TrueBoolStrs[0]:='True';
  end;
  if Length(FalseBoolStrs)=0 then
  begin
    SetLength(FalseBoolStrs,1);
    FalseBoolStrs[0]:='False';
  end;
end;

function BoolToStr(B: Boolean; UseBoolStrs: Boolean): string;
begin
  if UseBoolStrs Then
  begin
    CheckBoolStrs;
    if B then
      Result:=TrueBoolStrs[0]
    else
      Result:=FalseBoolStrs[0];
  end else
    if B then
      Result:='-1'
    else
      Result:='0';
end;

function BoolToStr(B: Boolean; const TrueS, FalseS: String): string;
begin
  if B then Result:=TrueS else Result:=FalseS;
end;

function StrToBoolDef(const S: String; Default: Boolean): Boolean;
begin
  if not TryStrToBool(S,Result) then
    Result:=Default;
end;

function TryStrToBool(const S: String; out Value: Boolean): Boolean;
Var
  Temp : String;
  I    : Longint;
  D : Double;
  Code: integer;
begin
  Temp:=uppercase(S);
  Val(Temp,D,code);
  Result:=true;
  If Code=0 then
    Value:=(D<>0.0)
  else
    begin
      CheckBoolStrs;
      for I:=low(TrueBoolStrs) to High(TrueBoolStrs) do
        if Temp=uppercase(TrueBoolStrs[I]) then
          begin
            Value:=true;
            exit;
          end;
      for I:=low(FalseBoolStrs) to High(FalseBoolStrs) do
        if Temp=uppercase(FalseBoolStrs[I]) then
          begin
            Value:=false;
            exit;
          end;
      Result:=false;
    end;
end;

function UpperCase(const s: String): String; assembler;
asm
  return s.toUpperCase();
end;

function LowerCase(const s: String): String; assembler;
asm
  return s.toLowerCase();
end;

function CompareStr(const s1, s2: String): Integer; assembler;
asm
  var l1 = s1.length;
  var l2 = s2.length;
  if (l1<=l2){
    var s = s2.substr(0,l1);
    if (s1<s){ return -1;
    } else if (s1>s){ return 1;
    } else { return l1<l2 ? -1 : 0; };
  } else {
    var s = s1.substr(0,l2);
    if (s<s2){ return -1;
    } else { return 1; };
  };
end;

function SameStr(const s1, s2: String): Boolean; assembler;
asm
  return s1 == s2;
end;

function CompareText(const s1, s2: String): Integer; assembler;
asm
  var l1 = s1.toLowerCase();
  var l2 = s2.toLowerCase();
  if (l1>l2){ return 1;
  } else if (l1<l2){ return -1;
  } else { return 0; }
end;

function SameText(const s1, s2: String): Boolean; assembler;
asm
  return s1.toLowerCase() == s2.toLowerCase();
end;

function AnsiCompareText(const s1, s2: String): Integer; assembler;
asm
  return s1.localeCompare(s2);
end;

function AnsiSameText(const s1, s2: String): Boolean; assembler;
asm
  return s1.localeCompare(s2) == 0;
end;

function AnsiCompareStr(const s1, s2: String): Integer;
begin
  {$IFDEF ECMAScript6}
  Result:=CompareText(TJSString(s1).normalize(),TJSString(s1).normalize());
  {$ELSE}
  Result:=CompareText(s1,s2);
  {$ENDIF}
end;

procedure AppendStr(var Dest: String; const S: string);

begin
  Dest:=Dest+S;
end;


Const
  feInvalidFormat   = 1;
  feMissingArgument = 2;
  feInvalidArgIndex = 3;

Procedure DoFormatError (ErrCode : Longint;const fmt: String);


begin
  //!! must be changed to contain format string...
  Case ErrCode of
   feInvalidFormat : raise EConvertError.Createfmt(SInvalidFormat,[Fmt]);
   feMissingArgument : raise EConvertError.Createfmt(SArgumentMissing,[Fmt]);
   feInvalidArgIndex : raise EConvertError.Createfmt(SInvalidArgIndex,[Fmt]);
  end;
end;

Const
  maxdigits = 15;

Function ReplaceDecimalSep(S: String; Const DS : string) : string;

Var
  P : Integer;

begin
  P:=Pos('.',S);
  if P>0 then
    Result:=Copy(S,1,P-1)+DS+Copy(S,P+1,Length(S)-P)
  else
    Result:=S;
end;

function FormatGeneralFloat(Value : double; Precision : Integer; DS : String) : string;

Var
  P, PE, Q, Exponent: Integer;

Begin
  If (Precision = -1) Or (Precision > maxdigits) Then
     Precision := maxdigits;
  { First convert to scientific format, with correct precision }
  Str(Value:precision+7, Result);
  { Delete leading spaces }
  Result:=TrimLeft(Result);
  P:=Pos('.',Result);
  if P=0 then
    exit;
   { Consider removing exponent }
  PE:=Pos('E',Result);
  if PE=0 then
    begin
    Result:=ReplaceDecimalSep(Result,DS);
    exit;
    end;
  { Read exponent }
  Q:=PE+2;
  Exponent := 0;
  while (Q <= Length(Result)) do
    begin
    Exponent := Exponent*10 + Ord(Result[Q])-Ord('0');
    Inc(Q);
    end;
  if Result[PE+1] = '-' then
    Exponent := -Exponent;
  if (P+Exponent < PE) and (Exponent > -6) then
    begin
    { OK to remove exponent }
    SetLength(Result,PE-1); { Trim exponent }
    if Exponent >= 0 then
      begin
        { Shift point to right }
      for Q := 0 to Exponent-1 do
        begin
        Result[P] := Result[P+1];
        Inc(P);
        end;
      Result[P] := '.';
      P := 1;
      if Result[P] = '-' then
        Inc(P);
      while (Result[P] = '0') and (P < Length(Result)) and (Copy(Result,P+1,Length(DS))<>DS) do
          { Trim leading zeros; conversion above should not give any, but occasionally does
            because of rounding }
          System.Delete(Result,P,1);
      end
    else
      begin
      { Add zeros at start }
      Insert(Copy('00000',1,-Exponent),Result,P-1);
      Result[P-Exponent] := Result[P-Exponent-1]; { Copy leading digit }
      Result[P] := '.';
      if Exponent <> -1 then
        Result[P-Exponent-1] := '0';
      end;
    { Remove trailing zeros }
    Q := Length(Result);
    while (Q > 0) and (Result[Q] = '0') do
      Dec(Q);
    if Result[Q] = '.' then
      Dec(Q); { Remove trailing decimal point }
    if (Q = 0) or ((Q=1) and (Result[1] = '-')) then
      Result := '0'
    else
      SetLength(Result,Q);
    end
  else
    begin
    { Need exponent, but remove superfluous characters }
    { Delete trailing zeros }
    while Result[PE-1] = '0' do
      begin
      System.Delete(Result,PE-1,1);
      Dec(PE);
      end;
    { If number ends in decimal point, remove it }
    if Result[PE-1] = DS then
      begin
      System.Delete(Result,PE-1,1);
      Dec(PE);
      end;
    { delete superfluous + in exponent }
    if Result[PE+1]='+' then
      System.Delete(Result,PE+1,1)
    else
      Inc(PE);
    while Result[PE+1] = '0' do
      { Delete leading zeros in exponent }
      System.Delete(Result,PE+1,1)
    end;
  Result:=ReplaceDecimalSep(Result,DS)
end;

function FormatExponentFloat(Value : double; Precision,Digits : Integer;DS : String) : string;

Var
  P: Integer;
Begin
  DS:=DecimalSeparator;
  If (Precision = -1) Or (Precision > maxdigits) Then
    Precision := maxdigits;
  Str(Value:Precision+7, Result);
  { Delete leading spaces }
  while Result[1] = ' ' do
    System.Delete(Result, 1, 1);
  P:=Pos('E',Result);
  if P=0 then
    begin
    Result:=ReplaceDecimalSep(Result,DS);
    exit;
    end;
  Inc(P, 2);
  if Digits > 4 then
    Digits:=4;
  Digits:=Length(Result) - P - Digits + 1;
  if Digits < 0 then
    insert(copy('0000',1,-Digits),Result,P)
  else
    while (Digits > 0) and (Result[P] = '0') do
      begin
      System.Delete(Result, P, 1);
      if P > Length(Result) then
        begin
        System.Delete(Result, P - 2, 2);
        break;
        end;
      Dec(Digits);
      end;
  Result:=ReplaceDecimalSep(Result,DS);
End;

function FormatFixedFloat(Value : double; Digits : Integer; DS : String) : string;

Begin
  If Digits = -1 Then
    Digits := 2
  Else If Digits > 18 Then
    Digits := 18;
  Str(Value:0:Digits, Result);
  if (Result<>'') and (Result[1]=' ') then
    Delete(Result,1,1);
  Result:=ReplaceDecimalSep(Result,DS);
end;


function FormatNumberFloat(Value : double; Digits : Integer; DS,TS : String) : string;

Var
  P : integer;

Begin
  If Digits = -1 Then
    Digits := 2
  else If Digits > maxdigits Then
    Digits := maxdigits;
  Str(Value:0:Digits, Result);
  if (Result<>'') and (Result[1]=' ') then
    Delete(Result,1,1);
  P:=Pos('.',Result);
  Result:=ReplaceDecimalSep(Result,DS);
  Dec(P,3);
  if (TS<>'') and (TS<>#0) then
    While (P>1) Do
      Begin
      If (Result[P-1] <> '-')  Then
        Insert(TS, Result, P);
      Dec(P, 3);
      End;
End;

function RemoveLeadingNegativeSign(var AValue: String; DS : String): Boolean;

// removes negative sign in case when result is zero eg. -0.00

var
  i: PtrInt;
  TS: String;
  StartPos: PtrInt;

begin
  Result:=False;
  StartPos := 2;
  TS := ThousandSeparator;
  for i :=StartPos to length(AValue) do
    begin
    Result := (AValue[i] in ['0', DS, 'E', '+']) or (aValue[i]=TS);
    if not Result then
      break;
    end;
  if (Result) and (AValue[1]='-') then
    Delete(AValue, 1, 1);
end;

Function FormatNumberCurrency(const Value : Currency; Digits : Integer; DS,TS : String) : string;

Var
  Negative: Boolean;
  P : Integer;

Begin
  //  Writeln('Value ',D);
   If Digits = -1 Then
     Digits := CurrencyDecimals
   Else If Digits > 18 Then
     Digits := 18;
   Str(Value:0:Digits, Result);
   // Writeln('1. Result ',Result,' currencystring : ',CurrencyString);
   Negative:=Result[1] = '-';
   if Negative then
     System.Delete(Result, 1, 1);
   P := Pos('.', Result);
   // Writeln('2. Result ',Result,' currencystring : ',CurrencyString);
   If TS<>'' Then
     begin
     If P <> 0 Then
       Result:=ReplaceDecimalSep(Result,DS)
     else
       P := Length(Result)+1;
     Dec(P, 3);
     While (P > 1) Do
     Begin
         Insert(TS, Result, P);
     Dec(P, 3);
     End;
     end;
   // Writeln('3. Result ',Result,' currencystring : ',CurrencyString);
   if Negative then
     RemoveLeadingNegativeSign(Result,DS);
   // Writeln('4. Result ',Result,' currencystring : ',CurrencyString);
   // Writeln('CurrencyFormat:  ',CurrencyFormat,'NegcurrencyFormat: ',NegCurrFormat);
   If Not Negative Then
     Case CurrencyFormat Of
       0: Result := CurrencyString + Result;
       1: Result := Result + CurrencyString;
       2: Result := CurrencyString + ' ' + Result;
       3: Result := Result + ' ' + CurrencyString;
     end
   else
     Case NegCurrFormat Of
       0: Result := '(' + CurrencyString + Result + ')';
       1: Result := '-' + CurrencyString + Result;
       2: Result := CurrencyString + '-' + Result;
       3: Result := CurrencyString + Result + '-';
       4: Result := '(' + Result + CurrencyString + ')';
       5: Result := '-' + Result + CurrencyString;
       6: Result := Result + '-' + CurrencyString;
       7: Result := Result + CurrencyString + '-';
       8: Result := '-' + Result + ' ' + CurrencyString;
       9: Result := '-' + CurrencyString + ' ' + Result;
       10: Result := Result + ' ' + CurrencyString + '-';
       11: Result := CurrencyString + ' ' + Result + '-';
       12: Result := CurrencyString + ' ' + '-' + Result;
       13: Result := Result + '-' + ' ' + CurrencyString;
       14: Result := '(' + CurrencyString + ' ' + Result + ')';
       15: Result := '(' + Result + ' ' + CurrencyString + ')';
     end;
end;

Function FloatToStrF(const Value : double; format: TFloatFormat; Precision, Digits: Integer): String;

Var
  DS: string;

Begin
  DS:=DecimalSeparator;
  Case format Of
    ffGeneral:
      Result:=FormatGeneralFloat(Value,Precision,DS);
    ffExponent:
      Result:=FormatExponentFloat(Value,Precision,Digits,DS);
    ffFixed:
      Result:=FormatFixedFloat(Value,Digits,DS);
    ffNumber:
      Result:=FormatNumberFloat(Value,Digits,DS,ThousandSeparator);
    ffCurrency:
     Result:=FormatNumberCurrency(Value,Digits,DS,ThousandSeparator);
  end;
  if (Format<>ffCurrency) and (length(Result)>1) and (Result[1]='-') then
    RemoveLeadingNegativeSign(Result,DS);
end;

function Format (const Fmt: String; const Args: array of jsvalue): String;

Var ChPos,OldPos,ArgPos,DoArg,Len : SizeInt;
    Hs,ToAdd : String;
    Index : SizeInt;
    Width,Prec : Longint;
    Left : Boolean;
    Fchar : char;
    vq : nativeint;

  {
    ReadFormat reads the format string. It returns the type character in
    uppercase, and sets index, Width, Prec to their correct values,
    or -1 if not set. It sets Left to true if left alignment was requested.
    In case of an error, DoFormatError is called.
  }

  Function ReadFormat : Char;

  Var Value : NativeInt;

    Procedure ReadInteger;

    var
      Code: integer;
      ArgN: SizeInt;
    begin
      If Value<>-1 then exit; // Was already read.
      OldPos:=ChPos;
      While (ChPos<=Len) and
            (Fmt[ChPos]<='9') and (Fmt[ChPos]>='0') do inc(ChPos);
      If ChPos>len then
        DoFormatError(feInvalidFormat,Fmt);
      If Fmt[ChPos]='*' then
        begin

        if Index=-1 then
          ArgN:=Argpos
        else
        begin
          ArgN:=Index;
          Inc(Index);
        end;

        If (ChPos>OldPos) or (ArgN>High(Args)) then
          DoFormatError(feInvalidFormat,Fmt);

        ArgPos:=ArgN+1;

        if IsNumber(Args[ArgN]) and IsInteger(Args[ArgN]) then
          Value:=Integer(Args[ArgN])
        else
          DoFormatError(feInvalidFormat,Fmt);
        Inc(ChPos);
        end
      else
        begin
        If (OldPos<ChPos) Then
          begin
          Val (Copy(Fmt,OldPos,ChPos-OldPos),value,code);
          // This should never happen !!
          If Code>0 then DoFormatError (feInvalidFormat,Fmt);
          end
        else
          Value:=-1;
        end;
    end;

    Procedure ReadIndex;

    begin
      If Fmt[ChPos]<>':' then
        ReadInteger
      else
        value:=0; // Delphi undocumented behaviour, assume 0, #11099
      If Fmt[ChPos]=':' then
        begin
        If Value=-1 then DoFormatError(feMissingArgument,Fmt);
        Index:=Value;
        Value:=-1;
        Inc(ChPos);
        end;
    end;

    Procedure ReadLeft;

    begin
      If Fmt[ChPos]='-' then
        begin
        left:=True;
        Inc(ChPos);
        end
      else
        Left:=False;
    end;

    Procedure ReadWidth;

    begin
      ReadInteger;
      If Value<>-1 then
        begin
        Width:=Value;
        Value:=-1;
        end;
    end;

    Procedure ReadPrec;

    begin
      If Fmt[ChPos]='.' then
        begin
        inc(ChPos);
          ReadInteger;
        If Value=-1 then
         Value:=0;
        prec:=Value;
        end;
    end;


  begin
    Index:=-1;
    Width:=-1;
    Prec:=-1;
    Value:=-1;
    inc(ChPos);
    If Fmt[ChPos]='%' then
      begin
      Result:='%';
      exit;                           // VP fix
      end;
    ReadIndex;
    ReadLeft;
    ReadWidth;
    ReadPrec;
    Result:=Upcase(Fmt[ChPos]);
  end;


  function Checkarg (AT : TJSValueType; err:boolean):boolean;
  {
    Check if argument INDEX is of correct type (AT)
    If Index=-1, ArgPos is used, and argpos is augmented with 1
    DoArg is set to the argument that must be used.
  }
  begin
    result:=false;
    if Index=-1 then
      DoArg:=Argpos
    else
      DoArg:=Index;
    ArgPos:=DoArg+1;
    If (Doarg>High(Args)) or (GetValueTYpe(Args[Doarg])<>AT) then
     begin
       if err then
        DoFormatError(feInvalidArgindex,Fmt);
       dec(ArgPos);
       exit;
     end;
    result:=true;
  end;

begin
  Result:='';
  Len:=Length(Fmt);
  ChPos:=1;
  OldPos:=1;
  ArgPos:=0;
  While ChPos<=len do
    begin
    While (ChPos<=Len) and (Fmt[ChPos]<>'%') do
      inc(ChPos);
    If ChPos>OldPos Then
      Result:=Result+Copy(Fmt,OldPos,ChPos-Oldpos);
    If ChPos<Len then
      begin
      FChar:=ReadFormat;
{$ifdef fmtdebug}
      DumpFormat(FCHar);
{$endif}
      Case FChar of
        'D' : begin
              Checkarg(jvtinteger,true);
              toAdd:=IntToStr(NativeInt(Args[DoArg]));
              Width:=Abs(width);
              Index:=Prec-Length(ToAdd);
              If ToAdd[1]<>'-' then
                ToAdd:=StringOfChar('0',Index)+ToAdd
              else
                // + 1 to accomodate for - sign in length !!
                Insert(StringOfChar('0',Index+1),toadd,2);
              end;
        'U' : begin
              Checkarg(jvtinteger,True);
              if NativeInt(Args[Doarg])<0 then
                DoFormatError(feInvalidArgindex,Fmt);
              Toadd:=IntToStr(NativeInt(Args[Doarg]));
              Width:=Abs(width);
              Index:=Prec-Length(ToAdd);
              ToAdd:=StringOfChar('0',Index)+ToAdd
              end;
        'E' : begin
              if CheckArg(jvtFloat,false) or CheckArg(jvtInteger,True) then
                ToAdd:=FloatToStrF(Double(Args[doarg]),ffFixed,9999,Prec);
              end;
        'F' : begin
              if CheckArg(jvtFloat,false) or CheckArg(jvtInteger,True) then
                ToAdd:=FloatToStrF(Double(Args[doarg]),ffFixed,9999,Prec);
              end;
        'G' : begin
              if CheckArg(jvtFloat,false) or CheckArg(jvtInteger,True) then
                ToAdd:=FloatToStrF(Double(Args[doarg]),ffGeneral,Prec,3);
              end;
        'N' : begin
              if CheckArg(jvtFloat,false) or CheckArg(jvtInteger,True) then
                ToAdd:=FloatToStrF(Double(Args[doarg]),ffNumber,9999,Prec);
              end;
        'M' : begin
              if CheckArg(jvtFloat,false) or CheckArg(jvtInteger,True) then
                ToAdd:=FloatToStrF(Double(Args[doarg]),ffCurrency,9999,Prec);
              end;
        'S' : begin
              CheckArg(jvtString,true);
              hs:=String(Args[doarg]);
              Index:=Length(hs);
              If (Prec<>-1) and (Index>Prec) then
                Index:=Prec;
              ToAdd:=Copy(hs,1,Index);
              end;
        'P' : Begin
              CheckArg(jvtInteger,true);
              ToAdd:=IntToHex(NativeInt(Args[DoArg]),31);
              end;
        'X' : begin
              Checkarg(jvtinteger,true);
              vq:=nativeInt(Args[Doarg]);
              index:=31; // May need to adjust to NativeInt
              If Prec>index then
                ToAdd:=IntToHex(vq,index)
              else
                begin
                // determine minimum needed number of hex digits.
                Index:=1;
                While (NativeInt(1) shl (Index*4)<=vq) and (index<16) do
                  inc(Index);
                If Index>Prec then
                  Prec:=Index;
                ToAdd:=IntToHex(vq,Prec);
                end;
              end;
        '%': ToAdd:='%';
      end;
      If Width<>-1 then
        If Length(ToAdd)<Width then
          If not Left then
            ToAdd:=StringOfChar(' ',Width-Length(ToAdd))+ToAdd
          else
            ToAdd:=ToAdd+StringOfChar(' ',Width-Length(ToAdd));
      Result:=Result+ToAdd;
      end;
    inc(ChPos);
    Oldpos:=ChPos;
    end;
end;

function BytesOf(const AVal: string): TBytes;
var
  I: SizeUInt;
begin
  SetLength(Result, Length(AVal));
  for I := 0 to Length(AVal)-1 do
    Result[I] := Ord(AVal[I+1]);
end;

function StringOf(const ABytes: TBytes): string;
var
  I: Integer;
begin
  Result:='';
  for I := 0 to Length(ABytes)-1 do
    Result:=Result+Char(ABytes[I]);
end;

function LocaleCompare(const s1, s2, locales: String): Boolean; assembler;
asm
  return s1.localeCompare(s2,locales) == 0;
end;

function NormalizeStr(const S: String; const Norm: String): String; assembler;
asm
  return S.normalize(Norm);
end;

function IsValidIdent(const Ident: string; AllowDots: Boolean = False; StrictDots: Boolean = False): Boolean;
const
  Alpha = ['A'..'Z', 'a'..'z', '_'];
  AlphaNum = Alpha + ['0'..'9'];
  Dot = '.';
var
  First: Boolean;
  I, Len: Integer;
begin
  Len := Length(Ident);
  if Len < 1 then
    Exit(False);
  First := True;
  Result:=false;
  I:=1;
  While I<=len do
    begin
    if First then
    begin
      if not (Ident[I] in Alpha) then exit;
      First := False;
    end
    else if AllowDots and (Ident[I] = Dot) then
    begin
      if StrictDots then
      begin
        if I >= Len then exit;
        First := True;
      end;
    end
    else
      if not (Ident[I] in AlphaNum) then exit;
    I:=I+1;
    end;
  Result:=true;
end;

procedure FreeAndNil(var Obj);
var
  o: TObject;
begin
  o:=TObject(Obj);
  if o=nil then exit;
  TObject(Obj):=nil;
  o.Destroy;
end;

{ EVariantError }

constructor EVariantError.CreateCode(Code: Longint);
begin
  ErrCode:=Code;
end;

{ Exception }

constructor Exception.Create(const Msg: String);
begin
  fMessage:=Msg;
  {$ifdef nodejs}
  FNodeJSError:=TJSError.new;
  {$endif}
end;

constructor Exception.CreateFmt(const Msg: string; const Args: array of jsvalue
  );
begin
  //writeln('Exception.CreateFmt START ',ClassName,' "',Msg,'" Args=',Args);
  Create(Format(Msg,Args));
  //writeln('Exception.CreateFmt END ',ClassName,' "',Msg,'" fMessage=',fMessage);
end;

constructor Exception.CreateHelp(const Msg: String; AHelpContext: Integer);
begin
  Create(Msg);
  fHelpContext:=AHelpContext;
end;

constructor Exception.CreateFmtHelp(const Msg: string;
  const Args: array of jsvalue; AHelpContext: Integer);
begin
  Create(Format(Msg,Args));
  fHelpContext:=AHelpContext;
end;

function Exception.ToString: String;
begin
  Result:=ClassName+': '+Message;
end;

Const
  RESpecials = '([\[\]\(\)\\\.\*])';

Function StringReplace(aOriginal, aSearch, aReplace : string; Flags : TStringReplaceFlags) : String;

Var
  REFlags : String;
  REString : String;

begin
  REFlags:='';
  if rfReplaceAll in flags then
    ReFlags:='g';
  if rfIgnoreCase in flags then
    ReFlags:=ReFlags+'i';
  REString:=TJSString(aSearch).replace(TJSRegexp.new(RESpecials,'g'),'\$1');
  Result:=TJSString(aOriginal).replace(TJSRegexp.new(REString,REFlags),aReplace);
end;

Function QuoteString(aOriginal : String; AQuote : Char) : String;

begin
  Result:=AQuote+StringReplace(aOriginal,aQuote,aQuote+aQuote,[rfReplaceAll])+AQuote;
end;

function QuotedStr(const s: string; QuoteChar : Char = ''''): string;

begin
  Result:=QuoteString(S,QuoteChar);
end;

function DeQuoteString(aQuoted: String; AQuote: Char): String;
var
  i: Integer;
begin
  Result:=aQuoted;
  if TJSString(Result).substr(0,1)<>AQuote then exit;
  Result:=TJSString(Result).slice(1);
  i:=1;
  while i<=length(Result) do
    begin
    if Result[i]=AQuote then
      begin
      if (i=length(Result)) or (Result[i+1]<>AQuote) then
        begin
        Result:=TJSString(Result).slice(0,i-1);
        exit;
        end
      else
        Result:=TJSString(Result).slice(0,i-1)+TJSString(Result).slice(i);
      end
    else
      inc(i);
    end;
end;

function IsDelimiter(const Delimiters, S: string; Index: Integer): Boolean;
begin
  Result:=False;
  if (Index>0) and (Index<=Length(S)) then
    Result:=Pos(S[Index],Delimiters)<>0; // Note we don't do MBCS yet
end;

function AdjustLineBreaks(const S: string): string;

begin
  Result:=AdjustLineBreaks(S,DefaultTextLineBreakStyle);
end;

function AdjustLineBreaks(const S: string; Style: TTextLineBreakStyle): string;

var
  I,L: Longint;
  Res : String;

  Procedure Add(C  : Char);
  begin
    Res:=Res+C;
  end;

begin
  I:=0;
  L:=Length(S);
  Result:='';
  While (I<=L) do
    case S[I] of
      #10: begin
          if Style in [tlbsCRLF,tlbsCR] then
            Add(#13);
          if Style=tlbsCRLF then
            Add(#10);
          Inc(I);
          end;
     #13: begin
          if Style=tlbsCRLF then
            Add(#13);
          Add(#10);
          Inc(I);
          if S[I]=#10 then
            Inc(I);
          end;
    else
      Add(S[i]);
      Inc(I);
    end;
  Result:=Res;
end;

function WrapText(const Line, BreakStr: string; const BreakChars: Array of char;  MaxCol: Integer): string;

const
  Quotes = ['''', '"'];

Var
  L : String;
  C,LQ,BC : Char;
  P,BLen,Len : Integer;
  HB,IBC : Boolean;

begin
  Result:='';
  L:=Line;
  Blen:=Length(BreakStr);
  If (BLen>0) then
    BC:=BreakStr[1]
  else
    BC:=#0;
  Len:=Length(L);
  While (Len>0) do
    begin
    P:=1;
    LQ:=#0;
    HB:=False;
    IBC:=False;
    While ((P<=Len) and ((P<=MaxCol) or not IBC)) and ((LQ<>#0) or Not HB) do
      begin
      C:=L[P];
      If (C=LQ) then
        LQ:=#0
      else If (C in Quotes) then
        LQ:=C;
      If (LQ<>#0) then
        Inc(P)
      else
        begin
        HB:=((C=BC) and (BreakStr=Copy(L,P,BLen)));
        If HB then
          Inc(P,Blen)
        else
          begin
          If (P>=MaxCol) then
            IBC:=CharInSet(C,BreakChars);
          Inc(P);
          end;
        end;
//      Writeln('"',C,'" : IBC : ',IBC,' HB  : ',HB,' LQ  : ',LQ,' P>MaxCol : ',P>MaxCol);
      end;
    Result:=Result+Copy(L,1,P-1);
    Delete(L,1,P-1);
    Len:=Length(L);
    If (Len>0) and Not HB then
      Result:=Result+BreakStr;
    end;
end;

function WrapText(const Line: string; MaxCol: Integer): string;

begin
  Result:=WrapText(Line,sLineBreak, [' ', '-', #9], MaxCol);
end;


function GetEnvironmentVariable(const EnvVar: String): String;
begin
  if Assigned(OnGetEnvironmentVariable) then
    Result:=OnGetEnvironmentVariable(EnvVar)
  else
    Result:='';
end;

function GetEnvironmentVariableCount: Integer;
begin
  if Assigned(OnGetEnvironmentVariableCount) then
    Result:=OnGetEnvironmentVariableCount()
  else
    Result:=0;
end;

function GetEnvironmentString(Index: Integer): String;
begin
  if Assigned(OnGetEnvironmentString) then
    Result:=OnGetEnvironmentString(Index)
  else
    Result:='';
end;

{ Date/Time routines}

Function DoEncodeDate(Year, Month, Day: Word): longint;

Var
  D : TDateTime;

begin
  If TryEncodeDate(Year,Month,Day,D) then
    Result:=Trunc(D)
  else
    Result:=0;
end;

function DoEncodeTime(Hour, Minute, Second, MilliSecond: word): TDateTime;

begin
  If not TryEncodeTime(Hour,Minute,Second,MilliSecond,Result) then
    Result:=0;
end;

Function DateTimeToJSDate(aDateTime : TDateTime) : TJSDate;

Var
  Y,M,D,h,n,s,z : Word;

begin
  DecodeDate(Trunc(aDateTime),Y,M,D);
  DecodeTime(Frac(aDateTime),H,N,S,Z);
  Result:=TJSDate.New(Y,M,D,h,n,s,z);
end;

Function JSDatetoDateTime(ADate: TJSDate) : TDateTime;

begin
  Result:=EncodeDate(ADate.FullYear,ADate.Month+1,ADate.Date) +
          EncodeTime(ADate.Hours,ADate.Minutes,ADate.Seconds,ADate.Milliseconds);
end;


{   ComposeDateTime converts a Date and a Time into one TDateTime   }
function ComposeDateTime(Date,Time : TDateTime) : TDateTime;

begin
  if Date < 0 then
    Result := trunc(Date) - Abs(frac(Time))
  else
    Result := trunc(Date) + Abs(frac(Time));
end;

Function TryEncodeDate(Year,Month,Day : Word; Out Date : TDateTime) : Boolean;

var
  c, ya: LongWord;
begin
  Result:=(Year>0) and (Year<10000) and
          (Month >= 1) and (Month<=12) and
          (Day>0) and (Day<=MonthDays[IsleapYear(Year),Month]);
 If Result then
   begin
     if month > 2 then
      Dec(Month,3)
     else
      begin
        Inc(Month,9);
        Dec(Year);
      end;
     c:= Year DIV 100;
     ya:= Year - 100*c;
     Date := (146097*c) SHR 2 + (1461*ya) SHR 2 + (153*LongWord(Month)+2) DIV 5 + LongWord(Day);
     // Note that this line can't be part of the line above, since TDateTime is
     // signed and c and ya are not
     Date := Date - 693900;
   end
end;

function TryEncodeTime(Hour, Min, Sec, MSec:word; Out Time : TDateTime) : boolean;

begin
  Result:=(Hour<24) and (Min<60) and (Sec<60) and (MSec<1000);
  If Result then
    Time:=TDateTime(LongWord(Hour)*3600000+LongWord(Min)*60000+LongWord(Sec)*1000+MSec)/MSecsPerDay;
end;

{   EncodeDate packs three variables Year, Month and Day into a
    TDateTime value the result is the number of days since 12/30/1899   }

function EncodeDate(Year, Month, Day: word): TDateTime;

begin
  If Not TryEncodeDate(Year,Month,Day,Result) then
    Raise EConvertError.CreateFmt('%s-%s-%s is not a valid date specification',
                              [IntToStr(Year),IntToStr(Month),IntToStr(Day)]);
end;

{   EncodeTime packs four variables Hour, Minute, Second and MilliSecond into
    a TDateTime value     }

function EncodeTime(Hour, Minute, Second, MilliSecond:word):TDateTime;

begin
  If not TryEncodeTime(Hour,Minute,Second,MilliSecond,Result) then
    Raise EConvertError.CreateFmt('%s:%s:%s.%s is not a valid time specification',
                              [IntToStr(Hour),IntToStr(Minute),IntToStr(Second),IntToStr(MilliSecond)]);
end;


{   DecodeDate unpacks the value Date into three values:
    Year, Month and Day   }

procedure DecodeDate(Date: TDateTime; out Year, Month, Day: word);
var
  ly,ld,lm,j : LongWord;
begin
  if Date <= -datedelta then  // If Date is before 1-1-1 then return 0-0-0
    begin
    Year := 0;
    Month := 0;
    Day := 0;
    end
  else
    begin
    if Date>0 then
      Date:=(Date+(1/(msecsperday*2)))
    else
      Date:=Date-(1/(msecsperday*2));
    if Date>MaxDateTime then
      Date:=MaxDateTime;
//       Raise EConvertError.CreateFmt('%f is not a valid TDatetime encoding, maximum value is %f.',[Date,MaxDateTime]);
    j := ((Trunc(Date) + 693900) SHL 2)-1;
    ly:= j DIV 146097;
    j:= j - 146097 * LongWord(ly);
    ld := j SHR 2;
    j:=(ld SHL 2 + 3) DIV 1461;
    ld:= ((ld SHL 2) + 7 - 1461*j) SHR 2;
    lm:=(5 * ld-3) DIV 153;
    ld:= (5 * ld +2 - 153*lm) DIV 5;
    ly:= 100 * LongWord(ly) + j;
    if lm < 10 then
     inc(lm,3)
    else
      begin
        dec(lm,9);
        inc(ly);
      end;
    year:=ly;
    month:=lm;
    day:=ld;
    end;
end;


function DecodeDateFully(const DateTime: TDateTime; out Year, Month, Day, DOW: Word): Boolean;
begin
  DecodeDate(DateTime,Year,Month,Day);
  DOW:=DayOfWeek(DateTime);
  Result:=IsLeapYear(Year);
end;
  {   DateTimeToTimeStamp converts DateTime to a TTimeStamp   }

function DateTimeToTimeStamp(DateTime: TDateTime): TTimeStamp;

Var
  D : Double;
begin
  D:=DateTime * Double(MSecsPerDay);
  if D<0 then
    D:=D-0.5
  else
    D:=D+0.5;
  result.Time := Trunc(Abs(Trunc(D)) Mod MSecsPerDay);
  result.Date := DateDelta + (Trunc(D) div MSecsPerDay);
end;

{   TimeStampToDateTime converts TimeStamp to a TDateTime value   }

function TimeStampToDateTime(const TimeStamp: TTimeStamp): TDateTime;
begin
  Result := ComposeDateTime(TimeStamp.Date - DateDelta,TimeStamp.Time/MSecsPerDay)
end;

{   MSecsToTimeStamp   }

function MSecsToTimeStamp(MSecs: NativeInt): TTimeStamp;
begin
  result.Date := Trunc(msecs/msecsperday);
  msecs:= msecs-NativeInt(result.date)*msecsperday;
  result.Time := Round(MSecs);
end;

function TimeStampToMSecs(const TimeStamp: TTimeStamp): NativeInt;
begin
  result := TimeStamp.Time + (timestamp.date*msecsperday);
end ;



{   DecodeTime unpacks Time into four values:
    Hour, Minute, Second and MilliSecond    }

procedure DecodeTime(Time: TDateTime; out Hour, Minute, Second, MilliSecond: word);
Var
  l : LongWord;
begin
  l := DateTimeToTimeStamp(Time).Time;
  Hour   := l div 3600000;
  l := l mod 3600000;
  Minute := l div 60000;
  l := l mod 60000;
  Second := l div 1000;
  l := l mod 1000;
  MilliSecond := l;
end;

{   DateTimeToSystemTime converts DateTime value to SystemTime   }

procedure DateTimeToSystemTime(DateTime: TDateTime; out SystemTime: TSystemTime);
begin
  DecodeDateFully(DateTime, SystemTime.Year, SystemTime.Month, SystemTime.Day,SystemTime.DayOfWeek);
  DecodeTime(DateTime, SystemTime.Hour, SystemTime.Minute, SystemTime.Second, SystemTime.MilliSecond);
  Dec(SystemTime.DayOfWeek);
end ;

{   SystemTimeToDateTime converts SystemTime to a TDateTime value   }

function SystemTimeToDateTime(const SystemTime: TSystemTime): TDateTime;
begin
  result := ComposeDateTime(DoEncodeDate(SystemTime.Year, SystemTime.Month, SystemTime.Day),
                            DoEncodeTime(SystemTime.Hour, SystemTime.Minute, SystemTime.Second, SystemTime.MilliSecond));
end ;

function DayOfWeek(DateTime: TDateTime): integer;
begin
  Result := 1 + ((Trunc(DateTime) - 1) mod 7);
  If (Result<=0) then
    Inc(Result,7);
end;



function Now: TDateTime;
begin
  Result:=JSDateToDateTime(TJSDate.New());
end;

function Date: TDateTime;

begin
  Result:=Trunc(Now);
end;

function Time: TDateTime;
begin
  Result:=Now-Date;
end ;

{   IncMonth increments DateTime with NumberOfMonths months,
    NumberOfMonths can be less than zero   }

function IncMonth(const DateTime: TDateTime; NumberOfMonths: integer = 1 ): TDateTime;
var
  Year, Month, Day : word;
begin
  DecodeDate(DateTime, Year, Month, Day);
  IncAMonth(Year, Month, Day, NumberOfMonths);
  result := ComposeDateTime(DoEncodeDate(Year, Month, Day), DateTime);
end ;

{   IncAMonth is the same as IncMonth, but operates on decoded date  }

procedure IncAMonth(var Year, Month, Day: Word; NumberOfMonths: Integer = 1);
var
  TempMonth, S: Integer;
begin
  If NumberOfMonths>=0 then
    s:=1
  else
    s:=-1;
  inc(Year,(NumberOfMonths div 12));
  TempMonth:=Month+(NumberOfMonths mod 12)-1;
  if (TempMonth>11) or
     (TempMonth<0) then
   begin
     Dec(TempMonth, S*12);
     Inc(Year, S);
   end;
  Month:=TempMonth+1;          {   Months from 1 to 12   }
  If (Day>MonthDays[IsLeapYear(Year)][Month]) then
    Day:=MonthDays[IsLeapYear(Year)][Month];
end;

{  IsLeapYear returns true if Year is a leap year   }

function IsLeapYear(Year: Word): boolean;
begin
  Result := (Year mod 4 = 0) and ((Year mod 100 <> 0) or (Year mod 400 = 0));
end;

{  DateToStr returns a string representation of Date using ShortDateFormat   }

function DateToStr(Date: TDateTime): string;
begin
  Result:=FormatDateTime('ddddd', Date);
end ;

{  TimeToStr returns a string representation of Time using LongTimeFormat   }

function TimeToStr(Time: TDateTime): string;
begin
  Result:=FormatDateTime('tt',Time);
end ;

{   DateTimeToStr returns a string representation of DateTime using LongDateTimeFormat   }

Var
  DateTimeToStrFormat : Array[Boolean] of string = ('c','f');

function DateTimeToStr(DateTime: TDateTime; ForceTimeIfZero : Boolean = False): string;
begin
  Result:=FormatDateTime(DateTimeToStrFormat[ForceTimeIfZero], DateTime)
end ;


{   StrToDate converts the string S to a TDateTime value
    if S does not represent a valid date value
    an EConvertError will be raised   }

function IntStrToDate(Out ErrorMsg : String; const S: String; const useformat : string; separator : char): TDateTime;

Const
  WhiteSpace = ' '#8#9#10#12#13;
  Digits = '0123456789';

  procedure FixErrorMsg(const errmarg : String);

  begin
    ErrorMsg:=Format(SInvalidDateFormat,[errmarg]);
  end;

var
   df:string;
   d,m,y,ly,ld,lm:word;
   n,i,len:longint;
   c: integer;
   dp,mp,yp,which : Byte;
   s1:string;
   values: array of integer;
   YearMoreThenTwoDigits : boolean;

begin
  SetLength(values,4);
  Result:=0;
  Len:=Length(S);
  ErrorMsg:='';
  While (Len>0) and (Pos(S[Len],WhiteSpace)>0) do
    Dec(len);
  if (Len=0) then
    begin
    FixErrorMsg(S);
    exit;
    end;
  YearMoreThenTwoDigits := False;
  if separator = #0 then
    if (DateSeparator<>#0) then
      separator := DateSeparator
    else
      separator:='-';
  // Writeln('Separator: ',Separator);
  df := UpperCase(useFormat);
  { Determine order of D,M,Y }
  yp:=0;
  mp:=0;
  dp:=0;
  Which:=0;
  i:=0;
  while (i<Length(df)) and (Which<3) do
   begin
     inc(i);
     Case df[i] of
       'Y' :
         if yp=0 then
          begin
            Inc(Which);
            yp:=which;
          end;
       'M' :
         if mp=0 then
          begin
            Inc(Which);
            mp:=which;
          end;
       'D' :
         if dp=0 then
          begin
            Inc(Which);
            dp:=which;
          end;
     end;
   end;
  // Writeln('YP: ',Yp,', MP: ',Mp,', DP: ',DP);
  for i := 1 to 3 do
    values[i] := 0;
  s1 := '';
  n := 0;
  for i := 1 to len do
     begin
     if Pos(s[i],Digits)>0 then
       s1 := s1 + s[i];
     { space can be part of the shortdateformat, and is defaultly in slovak
       windows, therefor it shouldn't be taken as separator (unless so specified)
       and ignored }
     if (Separator <> ' ') and (s[i] = ' ') then
       Continue;
     if (s[i] = separator) or ((i = len) and (Pos(s[i],Digits)>0)) then
      begin
        inc(n);
        if n>3 then
          begin
//            Writeln('More than 3 separators');
            FixErrorMsg(S);
            exit;
          end;
         // Check if the year has more then two digits (if n=yp, then we are evaluating the year.)
        if (n=yp) and (length(s1)>2) then YearMoreThenTwoDigits := True;
        val(s1, values[n], c);
        if c<>0 then
          begin
//            Writeln('S1 not a number ',S1);
            FixErrorMsg(s);
            Exit;
          end;
        s1 := '';
      end
     else if (Pos(s[i],Digits)=0) then
       begin
//       Writeln('Not a number at pos ',I,' ',S[i]);
       FixErrorMsg(s);
       Exit;
       end;
   end ;
//   Writeln('Which : ',Which,' N : ',N);
   if (Which<3) and (N>Which) then
    begin
    FixErrorMsg(s);
    Exit;
    end;
  // Fill in values.
  DecodeDate(Date,Ly,LM,LD);
  If N=3 then
   begin
     y:=values[yp];
     m:=values[mp];
     d:=values[dp];
   end
  Else
  begin
    Y:=ly;
    If n<2 then
     begin
       d:=values[1];
       m := LM;
     end
    else
     If dp<mp then
      begin
        d:=values[1];
        m:=values[2];
      end
    else
      begin
        d:=values[2];
        m:=values[1];
      end;
  end;
  if (y >= 0) and (y < 100) and not YearMoreThenTwoDigits then
    begin
    ly := ly - TwoDigitYearCenturyWindow;
    Inc(Y, ly div 100 * 100);
    if (TwoDigitYearCenturyWindow > 0) and (Y < ly) then
      Inc(Y, 100);
    end;
  if not TryEncodeDate(y, m, d, result) then
    errormsg:=SErrInvalidDate;
end;

function StrToDate(const S: String; const useformat : string; separator : char): TDateTime;
Var
  MSg : String;
begin
  Result:=IntStrToDate(Msg,S,useFormat,Separator);
  If (Msg<>'') then
    Raise EConvertError.Create(Msg);
end;

function StrToDate(const S: String; separator : char): TDateTime;
begin
    result := StrToDate(S,ShortDateFormat,separator)
end;

function StrToDate(const S: String): TDateTime;
begin
  result := StrToDate(S,ShortDateFormat,#0);
end;

{   StrToTime converts the string S to a TDateTime value
    if S does not represent a valid time value an
    EConvertError will be raised   }


function IntStrToTime(Out ErrorMsg : String; const S: String; Len : integer; separator : char): TDateTime;

const
  AMPM_None = 0;
  AMPM_AM = 1;
  AMPM_PM = 2;
  tiHour = 0;
  tiMin = 1;
  tiSec = 2;
  tiMSec = 3;

type
  TTimeValues = array of Word;

var
   AmPm: integer;
   TimeValues: TTimeValues;


   function SplitElements(out TimeValues: TTimeValues; out AmPm: Integer): Boolean;
   //Strict version. It does not allow #32 as Separator, it will treat it as whitespace always
   const
     Digits = '0123456789';
   var
      Cur, Offset, ElemLen, Err, TimeIndex, FirstSignificantDigit: Integer;
      Value: Integer;
      DigitPending, MSecPending: Boolean;
      AmPmStr: String;
      CurChar: Char;
      I : Integer;
      allowedchars : string;

   begin
     Result := False;
     AmPm := AMPM_None; //No Am or PM in string found yet
     MSecPending := False;
     TimeIndex := 0; //indicating which TTimeValue must be filled next
     For I:=tiHour to tiMSec do
      TimeValues[i]:=0;
     Cur := 1;
     //skip leading blanks
     While (Cur < Len) and (S[Cur] =#32) do Inc(Cur);
     Offset := Cur;
     //First non-blank cannot be Separator or DecimalSeparator
     if (Cur > Len - 1) or (S[Cur] = Separator) or (S[Cur] = Decimalseparator) then
       begin
       // Writeln('Error in sep S[Cur]',S[Cur],' ',separator,' ',GetDecimalSeparator);
       Exit;
       end;
     DigitPending := (Pos(S[Cur],Digits)>0);
     While (Cur <= Len) do
     begin
       //writeln;
       // writeln('Main While loop:  Cur = ',Cur,' S[Cur] = "',S[Cur],'" Len = ',Len,' separator : ',Separator);
       CurChar := S[Cur];
       if Pos(CurChar,Digits)>0 then
       begin//Digits
         //HH, MM, SS, or Msec?
         // writeln('Digit: ', CurChar);
         //Digits are only allowed after starting Am/PM or at beginning of string or after Separator
         //and TimeIndex must be <= tiMSec
         //Uncomment "or (#32 = Separator)" and it will allllow #32 as separator
         if (not (DigitPending {or (#32 = Separator)})) or (TimeIndex > tiMSec) then
            begin
            // Writeln('DigitPending',ElemLen);
            Exit;
            end;
         OffSet := Cur;
         if (CurChar <> '0') then FirstSignificantDigit := OffSet else FirstSignificantDigit := -1;
         while (Cur < Len) and (Pos(S[Cur + 1],Digits)>0) do
         begin
           //Mark first Digit that is not '0'
           if (FirstSignificantDigit = -1) and (S[Cur] <> '0') then FirstSignificantDigit := Cur;
           Inc(Cur);
         end;
         if (FirstSignificantDigit = -1) then FirstSignificantDigit := Cur;
         ElemLen := 1+ Cur - FirstSignificantDigit;
         // writeln('  S[FirstSignificantDigit] = ',S[FirstSignificantDigit], ' S[Cur] = ',S[Cur],' ElemLen = ',ElemLen,' -> ', S[Offset], ElemLen);
         // writeln('  Cur = ',Cur);
         //this way we know that Val() will never overflow Value !
         if (ElemLen <= 2) or ((ElemLen <= 3) and (TimeIndex = tiMSec) ) then
         begin
           Val(Copy(S,FirstSignificantDigit, ElemLen), Value, Err);
           // writeln('  Value = ',Value,' HH = ',TimeValues[0],' MM = ',TimeValues[1],' SS = ',TimeValues[2],' MSec = ',Timevalues[3]);
           //This is safe now, because we know Value < High(Word)
           TimeValues[TimeIndex] := Value;
           Inc(TimeIndex);
           DigitPending := False;
         end
         else
           begin
           // Writeln('Wrong elemlen: ',ElemLen, ' timeIndex: ',timeindex);
           Exit; //Value to big, so it must be a wrong timestring
           end;
       end//Digits
       else if (CurChar = #32) then
       begin
         //writeln('#32');
         //just skip, but we must adress this, or it will be parsed by either AM/PM or Separator
       end
       else if (CurChar = Separator) then
       begin
         // writeln('Separator ',Separator);
         if DigitPending or (TimeIndex > tiSec) then
           begin
           // Writeln ('DigitPending ',DigitPending,' or (TimeIndex',Timeindex,' > tiSec,', tiSec,')');
           Exit;
           end;
         DigitPending := True;
         MSecPending := False;
       end
       else if (CurChar = DecimalSeparator) then
       begin
         //writeln('DecimalSeparator');
         if DigitPending or MSecPending or (TimeIndex <> tiMSec) then
            begin
            // Writeln('DigitPending ',DigitPending,' or MSecPending ',MSecPending,' (',TimeIndex,',Timeindex, >', tiMSec,'  tiSec)');
            Exit;
            end;
         DigitPending := True;
         MSecPending := True;
       end
       else
       begin//AM/PM?
         //None of the above, so this char _must_ be the start of AM/PM string
         //If we already have found AM/PM or we expect a digit then then timestring must be wrong at this point
         //writeln('AM/PM?');
         if (AmPm <> AMPM_None) or DigitPending then
           begin
           // Writeln('AmPm <> AMPM_None) or DigitPending');
           Exit;
           end;
         OffSet := Cur;
         allowedchars:=DecimalSeparator+' ';
         if Separator<>#0 then
           allowedchars:=allowedchars+Separator;
         while (Cur < Len -1) and (Pos(S[Cur + 1],AllowedChars)=0)
           and (Pos(S[Cur + 1],Digits)=0) do Inc(Cur);
         ElemLen := 1 + Cur - OffSet;
         // writeln('  S[Offset] = ',S[1+Offset], ' S[Cur] = ',S[Cur],' ElemLen = ',ElemLen,' -> ', S[1+Offset], ElemLen);
         // writeln('  Cur = ',Cur, 'S =',S);
         AmPmStr := Copy(S,1+OffSet, ElemLen);

         // writeln('AmPmStr = ',ampmstr,' (',length(ampmstr),')');
         //We must compare to TimeAMString before hardcoded 'AM' for delphi compatibility
         //Also it is perfectly legal, though insane to have TimeAMString = 'PM' and vice versa
         if (CompareText(AmPmStr, TimeAMString) = 0) then AmPm := AMPM_AM
         else if (CompareText(AmPmStr, TimePMString) = 0) then AmPm := AMPM_PM
         else if (CompareText(AmPmStr, 'AM') = 0) then AmPm := AMPM_AM
         else if (CompareText(AmPmStr, 'PM') = 0) then AmPm := AMPM_PM
         else
           begin
           // Writeln('No timestring ',AmPmStr);
           Exit; //If text does not match any of these, timestring must be wrong;
           end;
         //if AM/PM is at beginning of string, then a digit is mandatory after it
         if (TimeIndex = tiHour) then
         begin
           DigitPending := True;
         end
         //otherwise, no more TimeValues allowed after this
         else
         begin
           TimeIndex := tiMSec + 1;
           DigitPending := False;
         end;
       end;//AM/PM
       Inc(Cur)
     end;//while

     //If we arrive here, parsing the elements has been successfull
     //if not at least Hours specified then input is not valid
     //when am/pm is specified Hour must be <= 12 and not 0
     if (TimeIndex = tiHour) or ((AmPm <> AMPM_None) and ((TimeValues[tiHour] > 12) or (TimeValues[tiHour] = 0))) or DigitPending then
       Exit;
     Result := True;
   end;

begin
  setlength(timevalues,4);
  if separator = #0 then
     if (TimeSeparator<>#0) then
       separator := TimeSeparator
      else
       separator:=':';
  AmPm := AMPM_None;
  if not SplitElements(TimeValues, AmPm) then
  begin
    ErrorMsg:=Format(SErrInvalidTimeFormat,[S]);
    Exit;
  end;
  if (AmPm=AMPM_PM) and (TimeValues[tiHour]<>12) then Inc(TimeValues[tiHour], 12)
  else if (AmPm=AMPM_AM) and (TimeValues[tiHour]=12) then TimeValues[tiHour]:=0;
  // Writeln(      TimeValues[tiHour], TimeValues[tiMin], TimeValues[tiSec], TimeValues[tiMSec]);
  if not TryEncodeTime(TimeValues[tiHour], TimeValues[tiMin], TimeValues[tiSec], TimeValues[tiMSec], result) Then

    ErrorMsg:=Format(SErrInvalidTimeFormat,[S]);
end ;

function StrToTime(const s: String; separator : char): TDateTime;

Var
  Msg : String;

begin
  Result:=IntStrToTime(Msg,S,Length(S),Separator);
  If (Msg<>'') then
    Raise EConvertError.Create(Msg);
end;

function StrToTime(const s: String): TDateTime;
begin
   result:= StrToTime(s, TimeSeparator);
end;

{   StrToDateTime converts the string S to a TDateTime value
    if S does not represent a valid date and/or time value
    an EConvertError will be raised   }

function SplitDateTimeStr(DateTimeStr: String; out DateStr, TimeStr: String): Integer;

{ Helper function for StrToDateTime
  Pre-condition
    Date is before Time
    If either Date or Time is omitted then see what fits best, a time or a date (issue #0020522)
    Date and Time are separated by whitespace (space Tab, Linefeed or carriage return)
    FS.DateSeparator can be the same as FS.TimeSeparator (issue #0020522)
    If they are both #32 and TrimWhite(DateTimeStr) contains a #32 a date is assumed.
  Post-condition
    DateStr holds date as string or is empty
    TimeStr holds time as string or is empty
    Result = number of strings returned, 0 = error
}
const
  WhiteSpace = #9#10#13#32;

var
  p: Integer;
  DummyDT: TDateTime;
begin
  Result := 0;
  DateStr := '';
  TimeStr := '';
  DateTimeStr := Trim(DateTimeStr);
  if Length(DateTimeStr) = 0 then exit;
  if (DateSeparator = #32) and (TimeSeparator = #32) and (Pos(#32, DateTimeStr) > 0) then
    begin
    DateStr:=DateTimeStr;
    {
      Assume a date: dd [mm [yy]].
      Really fancy would be counting the number of whitespace occurrences and decide
      and split accordingly
    }
    Exit(1);
    end;
  p:=1;
  //find separator
  if (DateSeparator<>#32) then
    begin
    while (p<Length(DateTimeStr)) and (not (Pos(DateTimeStr[p+1],WhiteSpace)>0)) do
      Inc(p);
    end
  else
    begin
    p:=Pos(TimeSeparator, DateTimeStr);
    if (p<>0) then
      repeat
        Dec(p);
      until (p=0) or (Pos(DateTimeStr[p],WhiteSpace)>0);
    end;
  //Always fill DateStr, it eases the algorithm later
  if (p=0) then
    p:=Length(DateTimeStr);
  DateStr:=Copy(DateTimeStr,1,p);
  TimeStr:=Trim(Copy(DateTimeStr,p+1,100));
  if (Length(TimeStr)<>0) then
    Result:=2
  else
    begin
    Result:=1; //found 1 string
    // 2 cases when DateTimeStr only contains a time:
    // Date/time separator differ, and string contains a timeseparator
    // Date/time separators are equal, but transformation to date fails.
    if ((DateSeparator<>TimeSeparator) and (Pos(TimeSeparator,DateStr) > 0))
       or ((DateSeparator=TimeSeparator) and (not TryStrToDate(DateStr, DummyDT)))  then
      begin
      TimeStr := DateStr;
      DateStr := '';
      end;
    end;
end;

function StrToDateTime(const s: String): TDateTime;

var
  TimeStr, DateStr: String;
  PartsFound: Integer;
begin
  PartsFound := SplitDateTimeStr(S, DateStr, TimeStr);
  case PartsFound of
    0: Result:=StrToDate('');
    1: if (Length(DateStr) > 0) then
         Result := StrToDate(DateStr,ShortDateFormat,DateSeparator)
       else
         Result := StrToTime(TimeStr);
    2: Result := ComposeDateTime(StrTodate(DateStr,ShortDateFormat,DateSeparator),
                                  StrToTime(TimeStr));
  end;
end;

Function FormatDateTime(const FormatStr: string; const DateTime: TDateTime) : String;

  procedure StoreStr(APos,Len: Integer);
  begin
//    Writeln('StoreStr: ',Result,'+',Copy(FormatStr,APos,Len));
    Result:=Result+Copy(FormatStr,APos,Len);
  end;

  procedure StoreString(const AStr: string);

  begin
//    Writeln('StoreString: ',Result,'+',AStr);
    Result:=Result+AStr;
  end;

  procedure StoreInt(Value, Digits: Integer);
  var
    S: string;

  begin
    S:=IntToStr(Value);
    While (Length(S)<Digits) do
      S:='0'+S;
    StoreString(S);
  end;

var
  Year, Month, Day, DayOfWeek, Hour, Minute, Second, MilliSecond: word;

  procedure StoreFormat(const FormatStr: string; Nesting: Integer; TimeFlag: Boolean);
  var
    Token, lastformattoken, prevlasttoken: char;
    Count: integer;
    Clock12: boolean;
    tmp: integer;
    isInterval: Boolean;
    P,FormatCurrent,FormatEnd : Integer;
  begin
    if Nesting > 1 then  // 0 is original string, 1 is included FormatString
      Exit;
    FormatCurrent := 1;
    FormatEnd := Length(FormatStr);
    Clock12 := false;
    isInterval := false;
    // look for unquoted 12-hour clock token
    P:=1;
    while P<=FormatEnd do
      begin
      Token := FormatStr[P];
      case Token of
        '''', '"':
        begin
          Inc(P);
          while (P < FormatEnd) and (FormatStr[P]<>Token) do
            Inc(P);
        end;
        'A', 'a':
        begin
          if (CompareText(Copy(FormatStr,P,3),'A/P')=0) or
             (CompareText(Copy(FormatStr,P,4),'AMPM')=0) or
             (CompareText(Copy(FormatStr,P,5),'AM/PM')=0) then
          begin
            Clock12 := true;
            break;
          end;
        end;
      end;  // case
      Inc(P);
    end ;
    token := #255;
    lastformattoken := ' ';
    prevlasttoken := 'H';
    while FormatCurrent <= FormatEnd do
    begin
      Token := UpperCase(FormatStr[FormatCurrent])[1];
     //  Writeln('Treating token at pos ',FormatCurrent,', : ',Token,' (',FormatStr,')');
      Count := 1;
      P := FormatCurrent + 1;
      case Token of
        '''', '"':
        begin
          while (P < FormatEnd) and (FormatStr[P]<>Token) do
            Inc(P);
          Inc(P);
          Count := P - FormatCurrent;
          StoreStr(FormatCurrent + 1, Count - 2);
        end ;
        'A':
        begin
          if CompareText(Copy(FormatStr,FormatCurrent,4), 'AMPM') = 0 then
          begin
            Count := 4;
            if Hour < 12 then
              StoreString(TimeAMString)
            else
              StoreString(TimePMString);
          end
          else if CompareText(Copy(FormatStr,FormatCurrent,5), 'AM/PM') = 0 then
          begin
            Count := 5;
            if Hour < 12 then StoreStr(FormatCurrent, 2)
                         else StoreStr(FormatCurrent+3, 2);
          end
          else if CompareText(Copy(FormatStr,FormatCurrent,3), 'A/P') = 0 then
          begin
            Count := 3;
            if Hour < 12 then StoreStr(FormatCurrent, 1)
            else StoreStr(FormatCurrent+2, 1);
          end
          else
            raise EConvertError.Create('Illegal character in format string');
        end ;
        '/':
          begin
           //  Writeln('Detected date separator');
          StoreString(DateSeparator);
          end;
        ':': StoreString(TimeSeparator);
        ' ', 'C', 'D', 'H', 'M', 'N', 'S', 'T', 'Y', 'Z', 'F' :
        begin
          // Writeln(FormatCurrent,' Special Token: ',Token,', Count: ',Count,', P: ',P);
          while (P <= FormatEnd) and (UpperCase(FormatStr[P]) = Token) do
            Inc(P);
          Count := P - FormatCurrent;
          // Writeln(FormatCurrent,' Special Token: ',Token,', Count: ',Count,', P: ',P);
          case Token of
            ' ': StoreStr(FormatCurrent, Count);
            'Y': begin
              if Count > 2 then
                StoreInt(Year, 4)
              else
                StoreInt(Year mod 100, 2);
            end;
            'M': begin
	      if isInterval and ((prevlasttoken = 'H') or TimeFlag) then
	        StoreInt(Minute + (Hour + trunc(abs(DateTime))*24)*60, 0)
	      else
              if (lastformattoken = 'H') or TimeFlag then
              begin
                if Count = 1 then
                  StoreInt(Minute, 0)
                else
                  StoreInt(Minute, 2);
              end
              else
              begin
                case Count of
                  1: StoreInt(Month, 0);
                  2: StoreInt(Month, 2);
                  3: StoreString(ShortMonthNames[Month]);
                else
                  StoreString(LongMonthNames[Month]);
                end;
              end;
            end;
            'D': begin
              case Count of
                1: StoreInt(Day, 0);
                2: StoreInt(Day, 2);
                3: StoreString(ShortDayNames[DayOfWeek]);
                4: StoreString(LongDayNames[DayOfWeek]);
                5: StoreFormat(ShortDateFormat, Nesting+1, False);
              else
                StoreFormat(LongDateFormat, Nesting+1, False);
              end ;
            end ;
            'H':
	      if isInterval then
	        StoreInt(Hour + trunc(abs(DateTime))*24, 0)
	      else
	      if Clock12 then
              begin
                tmp := hour mod 12;
                if tmp=0 then tmp:=12;
                if Count = 1 then
                  StoreInt(tmp, 0)
                else
                  StoreInt(tmp, 2);
              end
              else begin
                if Count = 1 then
		  StoreInt(Hour, 0)
                else
                  StoreInt(Hour, 2);
              end;
            'N': if isInterval then
	           StoreInt(Minute + (Hour + trunc(abs(DateTime))*24)*60, 0)
		 else
		 if Count = 1 then
                   StoreInt(Minute, 0)
                 else
                   StoreInt(Minute, 2);
            'S': if isInterval then
	           StoreInt(Second + (Minute + (Hour + trunc(abs(DateTime))*24)*60)*60, 0)
	         else
	         if Count = 1 then
                   StoreInt(Second, 0)
                 else
                   StoreInt(Second, 2);
            'Z': if Count = 1 then
                   StoreInt(MilliSecond, 0)
                 else
		   StoreInt(MilliSecond, 3);
            'T': if Count = 1 then
		   StoreFormat(ShortTimeFormat, Nesting+1, True)
                 else
	           StoreFormat(LongTimeFormat, Nesting+1, True);
            'C': begin
                   StoreFormat(ShortDateFormat, Nesting+1, False);
                   if (Hour<>0) or (Minute<>0) or (Second<>0) then
                     begin
                      StoreString(' ');
                      StoreFormat(LongTimeFormat, Nesting+1, True);
                     end;
                 end;
            'F': begin
                   StoreFormat(ShortDateFormat, Nesting+1, False);
                   StoreString(' ');
                   StoreFormat(LongTimeFormat, Nesting+1, True);
                 end;
          end;
	  prevlasttoken := lastformattoken;
          lastformattoken := token;
        end;
        else
          StoreString(Token);
      end ;
      Inc(FormatCurrent, Count);
    end;
  end;

begin
  DecodeDateFully(DateTime, Year, Month, Day, DayOfWeek);
  DecodeTime(DateTime, Hour, Minute, Second, MilliSecond);
  // Writeln(DateTime,'->',Year,',', Month, ',',Day, ',',DayOfWeek,',',Hour, ',',Minute, ',',Second, ',',MilliSecond);
  if FormatStr <> '' then
    StoreFormat(FormatStr, 0, False)
  else
    StoreFormat('C', 0, False);
end ;



function CurrentYear: Word;

begin
  Result:=TJSDate.New().FullYear;
end;

function TryStrToDate(const S: String; out Value: TDateTime): Boolean;
begin
  Result:=TryStrToDate(S,Value,ShortDateFormat,#0);
end;

function TryStrToDate(const S: String; out Value: TDateTime; separator : char): Boolean;

begin
  Result:=TryStrToDate(S,Value,ShortDateFormat,Separator);
end;

function TryStrToDate(const S: String; out Value: TDateTime;
                    const useformat : string; separator : char = #0): Boolean;

Var
  Msg : String;

begin
  Result:=Length(S)<>0;
  If Result then
    begin
    Value:=IntStrToDate(Msg,S,useformat,Separator);
    Result:=(Msg='');
    end;
end;




function TryStrToTime(const S: String; out Value: TDateTime; separator : char): Boolean;
Var
  Msg : String;
begin
  Result:=Length(S)<>0;
  If Result then
    begin
      Value:=IntStrToTime(Msg,S,Length(S),Separator);
      Result:=(Msg='');
    end;
end;

function TryStrToTime(const S: String; out Value: TDateTime): Boolean;
begin
  result:=TryStrToTime(S,Value,#0);
end;

function TryStrToDateTime(const S: String; out Value: TDateTime): Boolean;

var
  I: integer;
  dtdate, dttime :TDateTime;
begin
  result:=false;
  I:=Pos(TimeSeparator,S);
  If (I>0) then
    begin
      While (I>0) and (S[I]<>' ') do
        Dec(I);
      If I>0 then
        begin
          if not TryStrToDate(Copy(S,1,I-1),dtdate) then
            exit;
          if not TryStrToTime(Copy(S,i+1, Length(S)-i),dttime) then
            exit;
          Value:=ComposeDateTime(dtdate,dttime);
          result:=true;
        end
      else
         result:=TryStrToTime(s,Value);
    end
  else
    result:=TryStrToDate(s,Value);
end;



function StrToDateDef(const S: String; const Defvalue : TDateTime): TDateTime;
begin
   result := StrToDateDef(S,DefValue,#0);
end;

function StrToTimeDef(const S: String; const Defvalue : TDateTime): TDateTime;
begin
   result := StrToTimeDef(S,DefValue,#0);
end;

function StrToDateTimeDef(const S: String; const Defvalue : TDateTime): TDateTime;
begin
  if not TryStrToDateTime(s,Result) Then
    result:=defvalue;
end;

function StrToDateDef(const S: String; const Defvalue : TDateTime; separator : char): TDateTime;
begin
  if not TryStrToDate(s,Result, separator) Then
    result:=defvalue;
end;

function StrToTimeDef(const S: String; const Defvalue : TDateTime; separator : char): TDateTime;
begin
  if not TryStrToTime(s,Result, separator) Then
    result:=defvalue;
end;

procedure ReplaceTime(var dati:TDateTime; NewTime : TDateTime);
begin
  dati:= ComposeDateTime(dati, newtime);
end;

procedure ReplaceDate(var DateTime: TDateTime; const NewDate: TDateTime);
var
  tmp : TDateTime;
begin
  tmp:=NewDate;
  ReplaceTime(tmp,DateTime);
  DateTime:=tmp;
end;

Function FloatToDateTime (Const Value : Extended) : TDateTime;
begin
  If (Value<MinDateTime) or (Value>MaxDateTime) then
    Raise EConvertError.CreateFmt (SInvalidDateTime,[FloatToStr(Value)]);
  Result:=Value;
end;

function FloattoCurr(const Value: Extended): Currency;
begin
  if not TryFloatToCurr(Value, Result) then
    Raise EConvertError.CreateFmt(SInvalidCurrency, [FloatToStr(Value)]);
end;

function TryFloatToCurr(const Value: Extended; var AResult: Currency): Boolean;
begin
  Result:=(Value>=MinCurrency) and (Value<=MaxCurrency);
  if Result then
    AResult := Value;
end;

function CurrToStr(Value: Currency): string;
begin
  Result:=FloatToStrF(Value,ffGeneral,-1,0);
end;

(*
function CurrToStr(Value: Currency; const FormatSettings: TFormatSettings): string;
begin

end;
*)

function StrToCurr(const S: string): Currency;

begin
  if not TryStrToCurr(S,Result) then
    Raise EConvertError.createfmt(SInvalidCurrency,[S]);
end;

(*
function StrToCurr(const S: string; const FormatSettings: TFormatSettings): Currency;
begin

end;
*)

function TryStrToCurr(const S: string; out Value: Currency): Boolean;

Var
  D : Double;

begin
  Result:=TryStrToFloat(S,D);
  if Result then
    Value:=D;
end;

(*
function TryStrToCurr(const S: string; out Value: Currency; const FormatSettings: TFormatSettings): Boolean;
begin

end;
*)

function StrToCurrDef(const S: string; Default: Currency): Currency;

Var
  R : Currency;

begin
  if TryStrToCurr(S,R) then
    Result:=R
  else
    Result:=Default;
end;

(*
function StrToCurrDef(const S: string; Default: Currency; const FormatSettings: TFormatSettings): Currency;
begin

end;
*)

{ ---------------------------------------------------------------------
  Interface related
  ---------------------------------------------------------------------}
function Supports(const Instance: IInterface; const AClass: TClass; out Obj
  ): Boolean;
begin
  Result := (Instance<>nil) and (Instance.QueryInterface(IObjectInstance,Obj)=S_OK)
     and (TObject(Obj).InheritsFrom(AClass));
end;

function Supports(const Instance: IInterface; const IID: TGUID; out Intf
  ): Boolean;
begin
  Result:=(Instance<>nil) and (Instance.QueryInterface(IID,Intf)=S_OK);
end;

function Supports(const Instance: TObject; const IID: TGUID; out Intf
  ): Boolean;
begin
  Result:=(Instance<>nil) and Instance.GetInterface(IID,Intf);
end;

function Supports(const Instance: TObject; const IID: TGuidString; out Intf
  ): Boolean;
begin
  Result:=(Instance<>nil) and Instance.GetInterfaceByStr(IID,Intf);
end;

function Supports(const Instance: IInterface; const AClass: TClass): Boolean;
var
  Temp: TObject;
begin
  Result:=Supports(Instance,AClass,Temp);
end;

function Supports(const Instance: IInterface; const IID: TGUID): Boolean;
var
  Temp: IInterface;
begin
  Result:=Supports(Instance,IID,Temp);
end;

function Supports(const Instance: TObject; const IID: TGUID): Boolean;
var
  Temp: TJSObject;
begin
  Result:=Supports(Instance,IID,Temp);
  asm
    if (Temp && Temp.$kind==='com') Temp._Release();
  end;
end;

function Supports(const Instance: TObject; const IID: TGuidString): Boolean;
var
  Temp: TJSObject;
begin
  Result:=Supports(Instance,IID,Temp);
  asm
    if (Temp && Temp.$kind==='com') Temp._Release();
  end;
end;

function Supports(const AClass: TClass; const IID: TGUID): Boolean;
var
  maps: JSValue;
begin
  if AClass=nil then exit(false);
  maps := TJSObject(AClass)['$intfmaps'];
  if not maps then exit(false);
  if TJSObject(maps)[GUIDToString(IID)] then exit(true);
  Result:=false;
end;

function Supports(const AClass: TClass; const IID: TGuidString): Boolean;
var
  maps: JSValue;
begin
  if AClass=nil then exit(false);
  maps := TJSObject(AClass)['$intfmaps'];
  if not maps then exit(false);
  if TJSObject(maps)[uppercase(IID)] then exit(true);
  Result:=false;
end;

function TryStringToGUID(const s: string; out Guid: TGUID): Boolean;
var
  re: TJSRegexp;
begin
  if Length(s)<>38 then Exit(False);
  re:=TJSRegexp.new('^\{[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}\}$');
  Result:=re.test(s);
  if not Result then
    begin
    Guid.D1:=0;
    exit;
    end;
  asm
    rtl.strToGUIDR(s,Guid);
  end;
  Result:=true;
end;

function StringToGUID(const S: string): TGUID;
begin
  if not TryStringToGUID(S, Result) then
    raise EConvertError.CreateFmt(SInvalidGUID, [S]);
end;

function GUIDToString(const guid: TGUID): string;
begin
  Result:=System.GUIDToString(guid);
end;

function IsEqualGUID(const guid1, guid2: TGUID): Boolean;
var
  i: integer;
begin
  if (guid1.D1<>guid2.D1) or (guid1.D2<>guid2.D2) or (guid1.D3<>guid2.D3) then
    exit(false);
  for i:=0 to 7 do if guid1.D4[i]<>guid2.D4[i] then exit(false);
  Result:=true;
end;

function GuidCase(const guid: TGUID; const List: array of TGuid): Integer;
begin
  for Result := High(List) downto 0 do
    if IsEqualGUID(guid, List[Result]) then
      Exit;
  Result := -1;
end;

Function CreateGUID(out GUID : TGUID) : Integer;

  Function R(B: Integer) : NativeInt;

  Var
    v : NativeInt;
  begin
    v:=Random(256);
    While B>1 do
      begin
      v:=v*256+Random(256);
      Dec(B);
      end;
    Result:=V;
  end;

Var
  I : Integer;

begin
  Result:=0;
  GUID.D1:= R(4);
  GUID.D2:= R(2);
  GUID.D3:= R(2);
  For I:=0 to 7 do
   GUID.D4[I]:=R(1);
end;
{ ---------------------------------------------------------------------
  Integer/Ordinal related
  ---------------------------------------------------------------------}

Function TryStrToInt(const S : String; Out res : Integer) : Boolean;

Var
  NI : NativeInt;

begin
  Result:=TryStrToInt(S,NI);
  if Result then
    res:=NI;
end;

Function TryStrToInt(const S : String; Out res : NativeInt) : Boolean;

Var
  Radix : Integer = 10;
  N : String;
  J : JSValue;

begin
  N:=S;
  case Copy(N,1,1) of
  '$': Radix:=16;
  '&': Radix:=8;
  '%': Radix:=2;
  end;
  If Radix<>10 then
    Delete(N,1,1);
  J:=parseInt(N,Radix);
  Result:=Not jsIsNan(j);
  if Result then
    res:=NativeInt(J);
end;

Function StrToIntDef(const S : String; Const aDef : Integer) : Integer;

Var
  R : NativeInt;

begin
  if TryStrToInt(S,R) then
    Result:=R
  else
    Result:=aDef;
end;

Function StrToIntDef(const S : String; Const aDef : NativeInt) : NativeInt;

Var
  R : NativeInt;

begin
  if TryStrToInt(S,R) then
    Result:=R
  else
    Result:=aDef;
end;

Function StrToInt(const S : String) : Integer;

Var
  R : NativeInt;

begin
  if not TryStrToInt(S,R) then
    Raise EConvertError.CreateFmt(SErrInvalidInteger,[S]);
  Result:=R;
end;

Function StrToNativeInt(const S : String) : NativeInt;

begin
  if not TryStrToInt(S,Result) then
    Raise EConvertError.CreateFmt(SErrInvalidInteger,[S]);
end;

Function StrToInt64(const S : String) : NativeLargeInt;

Var
  N : NativeInt;

begin
  if not TryStrToInt(S,N) then
    Raise EConvertError.CreateFmt(SErrInvalidInteger,[S]);
  Result:=N;
end;

Function TryStrToInt64(const S : String; Out res : NativeLargeInt) : Boolean;

Var
  R : nativeint;

begin
  Result:=TryStrToInt(S,R);
  If Result then
    Res:=R;
end;

Function StrToInt64Def(const S : String; ADefault : NativeLargeInt) : NativeLargeInt;


begin
  if TryStrToInt64(S,Result) then
    Result:=ADefault;
end;

Function StrToQWord(const S : String) : NativeLargeUInt;

Var
  N : NativeInt;

begin
  if (not TryStrToInt(S,N)) or (N<0) then
    Raise EConvertError.CreateFmt(SErrInvalidInteger,[S]);
  Result:=N;
end;

Function TryStrToQWord(const S : String; Out res : NativeLargeUInt) : Boolean;

Var
  R : nativeint;

begin
  Result:=TryStrToInt(S,R) and (R>=0);
  If Result then
    Res:=R;
end;

Function StrToQWordDef(const S : String; ADefault : NativeLargeUInt) : NativeLargeUInt;

begin
  if Not TryStrToQword(S,Result) then
    Result:=ADefault;
end;


Function StrToUInt64(const S : String) : NativeLargeUInt;

Var
  N : NativeInt;

begin
  if (not TryStrToInt(S,N)) or (N<0) then
    Raise EConvertError.CreateFmt(SErrInvalidInteger,[S]);
  Result:=N;
end;

Function TryStrToUInt64(const S : String; Out res : NativeLargeUInt) : Boolean;

Var
  R : nativeint;

begin
  Result:=TryStrToInt(S,R) and (R>=0);
  If Result then
    Res:=R;
end;

Function StrToUInt64Def(const S : String; ADefault : NativeLargeUInt) : NativeLargeUInt;


begin
  if Not TryStrToUInt64(S,Result) then
    Result:=ADefault;
end;

Function TryStrToDWord(const S : String; Out res : DWord) : Boolean;

Var
  R : nativeint;

begin
  Result:=TryStrToInt(S,R) and (R>=0) and (R<=DWord($FFFFFFFF));
  If Result then
    Res:=R;
end;

Function StrToDWord(const S : String) : DWord;

begin
  if not TryStrToDWord(S,Result) then
    Raise EConvertError.CreateFmt(SErrInvalidInteger,[S]);
end;


Function StrToDWordDef(const S : String; ADefault : DWord) : DWord;

begin
  if Not TryStrToDWord(S,Result) then
    Result:=ADefault;
end;


function IntToHex(Value: NativeInt; Digits: integer): string;
const
  HexDigits = '0123456789ABCDEF';
begin
  If Digits=0 then
    Digits:=1;
  Result:='';
  While Value>0 do
    begin
    result:=HexDigits[(value and 15)+1]+Result;
    value := value shr 4;
    end ;
  while (Length(Result)<Digits) do
    Result:='0'+Result;
end;


{ TFormatSettings }

function TFormatSettings.GetCurrencyDecimals: Byte;
begin
  Result:=Sysutils.CurrencyDecimals;
end;

function TFormatSettings.GetCurrencyFormat: Byte;
begin
  Result:=Sysutils.CurrencyFormat;
end;

function TFormatSettings.GetCurrencyString: String;
begin
  Result:=Sysutils.CurrencyString;
end;

function TFormatSettings.GetDateSeparator: char;
begin
  Result := SysUtils.DateSeparator;
end;

function TFormatSettings.GetDecimalSeparator: string;
begin
  Result := SysUtils.DecimalSeparator;
end;

function TFormatSettings.GetLongDateFormat: string;
begin
  Result := SysUtils.LongDateFormat;
end;

function TFormatSettings.GetLongDayNames: TDayNames;
begin
  Result:=Sysutils.LongDayNames;
end;

function TFormatSettings.GetLongMonthNames: TMonthNames;
begin
 Result:=Sysutils.LongMonthNames;
end;

function TFormatSettings.GetLongTimeFormat: string;
begin
  Result := SysUtils.LongTimeFormat;
end;

function TFormatSettings.GetNegCurrFormat: Byte;
begin
  Result:=Sysutils.NegCurrFormat;
end;

function TFormatSettings.GetShortDateFormat: string;
begin
  Result := SysUtils.ShortDateFormat;
end;

function TFormatSettings.GetShortDayNames: TDayNames;
begin
 Result:=Sysutils.ShortDayNames;
end;

function TFormatSettings.GetShortMonthNames: TMonthNames;
begin
 Result:=Sysutils.ShortMonthNames;
end;

function TFormatSettings.GetShortTimeFormat: string;
begin
  Result := SysUtils.ShortTimeFormat;
end;

function TFormatSettings.GetThousandSeparator: string;
begin
  Result := SysUtils.ThousandSeparator;
end;

function TFormatSettings.GetTimeAMString: string;
begin
  Result := SysUtils.TimeAMString;
end;

function TFormatSettings.GetTimePMString: string;
begin
  Result := SysUtils.TimePMString;
end;

function TFormatSettings.GetTimeSeparator: char;
begin
  Result := SysUtils.TimeSeparator;
end;

procedure TFormatSettings.SetCurrencyFormat(AValue: Byte);
begin
  Sysutils.CurrencyFormat:=AValue;
end;

procedure TFormatSettings.SetCurrencyString(AValue: String);
begin
  Sysutils.CurrencyString:=AValue;
end;

procedure TFormatSettings.SetDateSeparator(const Value: char);
begin
  SysUtils.DateSeparator := Value;
end;

procedure TFormatSettings.SetDecimalSeparator(const Value: string);
begin
  SysUtils.DecimalSeparator := Value;
end;

procedure TFormatSettings.SetLongDateFormat(const Value: string);
begin
  SysUtils.LongDateFormat := Value;
end;

procedure TFormatSettings.SetLongDayNames(AValue: TDayNames);
begin
  SysUtils.LongDayNames:=AValue;
end;

procedure TFormatSettings.SetLongMonthNames(AValue: TMonthNames);
begin
  SysUtils.LongMonthNames:=AValue;
end;

procedure TFormatSettings.SetLongTimeFormat(const Value: string);
begin
  SysUtils.LongTimeFormat := Value;
end;

procedure TFormatSettings.SetNegCurrFormat(AValue: Byte);
begin
  Sysutils.NegCurrFormat:=AValue;
end;

procedure TFormatSettings.SetShortDateFormat(const Value: string);
begin
  SysUtils.ShortDateFormat := Value;
end;

procedure TFormatSettings.SetShortDayNames(AValue: TDayNames);
begin
  SysUtils.ShortDayNames:=AValue;
end;

procedure TFormatSettings.SetShortMonthNames(AValue: TMonthNames);
begin
  SysUtils.ShortMonthNames:=AValue;
end;

procedure TFormatSettings.SetShortTimeFormat(const Value: string);
begin
  SysUtils.ShortTimeFormat := Value;
end;

procedure TFormatSettings.SetCurrencyDecimals(AValue: Byte);
begin
  Sysutils.CurrencyDecimals:=aValue;
end;

procedure TFormatSettings.SetThousandSeparator(const Value: string);
begin
  SysUtils.ThousandSeparator := Value;
end;

procedure TFormatSettings.SetTimeAMString(const Value: string);
begin
  SysUtils.TimeAMString := Value;
end;

procedure TFormatSettings.SetTimePMString(const Value: string);
begin
  SysUtils.TimePMString := Value;
end;

procedure TFormatSettings.SetTimeSeparator(const Value: char);
begin
  SysUtils.TimeSeparator := Value;
end;

{ ---------------------------------------------------------------------
  FileNames
  ---------------------------------------------------------------------}

function ChangeFileExt(const FileName, Extension: PathStr): PathStr;
var
  i : longint;
  EndSep : Set of Char;
  SOF : Boolean;

begin
  i := Length(FileName);
  EndSep:=AllowDirectorySeparators+AllowDriveSeparators+[ExtensionSeparator];
  while (I > 0) and not(FileName[I] in EndSep) do
    Dec(I);
  if (I = 0) or (FileName[I] <> ExtensionSeparator) then
    I := Length(FileName)+1
  else
    begin
        SOF:=(I=1) or (FileName[i-1] in AllowDirectorySeparators);
        if (SOF) and not FirstDotAtFileNameStartIsExtension then
          I:=Length(FileName)+1;
        end;
  Result := Copy(FileName, 1, I - 1) + Extension;
end;

function ExtractFilePath(const FileName: PathStr): PathStr;
var
  i : longint;
  EndSep : Set of Char;
begin
  i := Length(FileName);
  EndSep:=AllowDirectorySeparators+AllowDriveSeparators;
  while (i > 0) and not CharInSet(FileName[i],EndSep) do
    Dec(i);
  If I>0 then
    Result := Copy(FileName, 1, i)
  else
    Result:='';
end;

function ExtractFileDir(const FileName: PathStr): PathStr;
var
  i : longint;
  EndSep : Set of Char;
begin
  I := Length(FileName);
  EndSep:=AllowDirectorySeparators+AllowDriveSeparators;
  while (I > 0) and not CharInSet(FileName[I],EndSep) do
    Dec(I);
  if (I > 1) and CharInSet(FileName[I],AllowDirectorySeparators) and
     not CharInSet(FileName[I - 1],EndSep) then
    Dec(I);
  Result := Copy(FileName, 1, I);
end;

function ExtractFileDrive(const FileName: PathStr): PathStr;
var
  i,l: longint;
begin
  Result := '';
  l:=Length(FileName);
  if (l<2) then
    exit;
  If CharInSet(FileName[2],AllowDriveSeparators) then
    result:=Copy(FileName,1,2)
  else if CharInSet(FileName[1],AllowDirectorySeparators) and
          CharInSet(FileName[2],AllowDirectorySeparators) then
    begin
      i := 2;

      { skip share }
      While (i<l) and Not CharInSet(Filename[i+1],AllowDirectorySeparators) do
        inc(i);
      inc(i);

      While (i<l) and Not CharInSet(Filename[i+1],AllowDirectorySeparators) do
        inc(i);
      Result:=Copy(FileName,1,i);
    end;
end;

function ExtractFileName(const FileName: PathStr): PathStr;
var
  i : longint;
  EndSep : Set of Char;
begin
  I := Length(FileName);
  EndSep:=AllowDirectorySeparators+AllowDriveSeparators;
  while (I > 0) and not CharInSet(FileName[I],EndSep) do
    Dec(I);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function ExtractFileExt(const FileName: PathStr): PathStr;
var
  i : longint;
  EndSep : Set of Char;
  SOF : Boolean; // Dot at Start of filename ?

begin
  Result:='';
  I := Length(FileName);
  EndSep:=AllowDirectorySeparators+AllowDriveSeparators+[ExtensionSeparator];
  while (I > 0) and not CharInSet(FileName[I],EndSep) do
    Dec(I);
  if (I > 0) and (FileName[I] = ExtensionSeparator) then
    begin
        SOF:=(I=1) or (FileName[i-1] in AllowDirectorySeparators);
        if (Not SOF) or FirstDotAtFileNameStartIsExtension then
          Result := Copy(FileName, I, MaxInt);
        end
  else
    Result := '';
end;

function ExtractRelativepath (Const BaseName,DestName : PathStr): PathStr;

Var
  OneLevelBack,Source, Dest   : PathStr;
  Sc,Dc,I,J      : Longint;
  SD,DD          : TPathStrArray;


begin
  OneLevelBack := '..'+PathDelim;
  If Uppercase(ExtractFileDrive(BaseName))<>Uppercase(ExtractFileDrive(DestName)) Then
    begin
    Result:=DestName;
    exit;
    end;
  Source:=ExcludeTrailingPathDelimiter(ExtractFilePath(BaseName));
  Dest:=ExcludeTrailingPathDelimiter(ExtractFilePath(DestName));
  SD:=GetDirs (Source);
  SC:=Length(SD);
  DD:=GetDirs (Dest);
  DC:=Length(SD);
  I:=0;
  While (I<DC) and (I<SC) do
    begin
    If SameText(DD[i],SD[i]) then
      Inc(i)
    else
      Break;
    end;
  Result:='';
  For J:=I to SC do Result:=Result+OneLevelBack;
  For J:=I to DC do Result:=Result+DD[J]+PathDelim;
  Result:=Result+ExtractFileName(DestName);
end;

Function SetDirSeparators (Const FileName : PathStr) : PathStr;

Var
  I : integer;

begin
  Result:=FileName;
  For I:=1 to Length(Result) do
    If CharInSet(Result[I],AllowDirectorySeparators) then
      Result[i]:=PathDelim;
end;

Function GetDirs (DirName : PathStr) : TPathStrArray;

Var
  I,J,L : Longint;
  D : String;

begin
  I:=1;
  J:=0;
  L:=0;
  SetLength(Result,Length(DirName));
  While I<=Length(DirName) do
    begin
    If CharInSet(DirName[i],AllowDirectorySeparators) then
      begin
      D:=Copy(DirName,J+1,J-I);
      if (D<>'') then
        begin
        Result[L]:=D;
        Inc(L);
        end;
      J:=I;
      end;
    Inc(I);
    end;
  SetLength(Result,L);
end;

function IncludeTrailingPathDelimiter(Const Path : PathStr) : PathStr;

Var
  l : Integer;

begin
  Result:=Path;
  l:=Length(Result);
  If (L=0) or not CharInSet(Result[l],AllowDirectorySeparators) then
    Result:=Result+PathDelim;
end;

function ExcludeTrailingPathDelimiter(Const Path: PathStr): PathStr;

Var
  L : Integer;

begin
  L:=Length(Path);
  If (L>0) and CharInSet(Path[L],AllowDirectorySeparators) then
    Dec(L);
  Result:=Copy(Path,1,L);
end;

function IncludeLeadingPathDelimiter(Const Path : PathStr) : PathStr;

Var
  l : Integer;

begin
  Result:=Path;
  l:=Length(Result);
  If (L=0) or not CharInSet(Result[1],AllowDirectorySeparators) then
    Result:=PathDelim+Result;
end;

function ExcludeLeadingPathDelimiter(Const Path: PathStr): PathStr;

Var
  L : Integer;

begin
  Result:=Path;
  L:=Length(Result);
  If (L>0) and CharInSet(Result[1],AllowDirectorySeparators) then
    Delete(Result,1,1);
end;

function IsPathDelimiter(Const Path: PathStr; Index: Integer): Boolean;

begin
  Result:=(Index>0) and (Index<=Length(Path)) and CharInSet(Path[Index],AllowDirectorySeparators);
end;

function ConcatPaths(const Paths: array of PathStr): PathStr;
var
  I: Integer;
begin
  if Length(Paths) > 0 then
  begin
    Result := Paths[0];
    for I := 1 to Length(Paths) - 1 do
      Result := IncludeTrailingPathDelimiter(Result) + ExcludeLeadingPathDelimiter(Paths[I]);
  end else
    Result := '';
end;


initialization
  FormatSettings := TFormatSettings.Create;

end.

