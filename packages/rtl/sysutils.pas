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
{$modeswitch typehelpers}
{$modeswitch advancedrecords}
{$WARN 5078 off}

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
  TSysCharSet = Set of Char;

  { TFloatRec }
  TFloatRec = Record
    Exponent: Integer;
    Negative: Boolean;
    Digits: Array[0..FloatRecDigits-1] of Char;
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

type

  { TFormatSettings }
  TFormatSettings = record
    strict private
      class function GetJSLocale: string; assembler; static;
      class function GetLocaleShortDayName(const ADayOfWeek: Integer; const ALocale: string): string; static;
      class function GetLocaleDecimalSeparator(const ALocale: string): string; static;
      class function GetLocaleLongMonthName(const AMonth: Integer; const ALocale: string): string; static;
      class function GetLocaleLongDayName(const ADayOfWeek: Integer; const ALocale: string): string; static;
      class function GetLocaleShortMonthName(const AMonth: Integer; const ALocale: string): string; static;
    public
      CurrencyDecimals: Byte;
      CurrencyFormat: Byte;
      CurrencyString: string;
      DateSeparator: Char;
      DateTimeToStrFormat: array[Boolean] of string;
      DecimalSeparator: string;
      LongDateFormat: string;
      LongDayNames: TDayNames;
      LongMonthNames: TMonthNames;
      LongTimeFormat: string;
      NegCurrFormat: Byte;
      ShortDateFormat: string;
      ShortDayNames: TDayNames;
      ShortMonthNames: TMonthNames;
      ShortTimeFormat: string;
      ThousandSeparator: string;
      TimeAMString: string;
      TimePMString: string;
      TimeSeparator: Char;
      TwoDigitYearCenturyWindow: Word;
    public
      Type
         TLocaleInitCallback = Procedure(const aLocale : String; aInstance : TFormatSettings);
      class var InitLocaleHandler : TLocaleInitCallback;
      class function Create: TFormatSettings; overload; static;
      class function Create(const ALocale: string): TFormatSettings; overload; static;
    end;


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
    class var
      LogMessageOnCreate : Boolean;
  Public
    constructor Create(const Msg: String); reintroduce;
    constructor CreateFmt(const Msg: string; const Args: array of const);
    constructor CreateHelp(const Msg: String; AHelpContext: Integer);
    constructor CreateFmtHelp(const Msg: string; const Args: array of const; AHelpContext: Integer);
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

Function CharInSet(Ch: Char;Const CSet : array of char) : Boolean; overload;

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

function Format(const Fmt: String; const Args: array of const): String;
function Format(const Fmt: String; const Args: array of const; const aSettings : TFormatSettings): String;

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
Function LastDelimiter(const Delimiters, S: string): SizeInt;
function IsDelimiter(const Delimiters, S: string; Index: Integer): Boolean;
function AdjustLineBreaks(const S: string): string;
function AdjustLineBreaks(const S: string; Style: TTextLineBreakStyle): string;
function WrapText(const Line, BreakStr: string; const BreakChars: Array of char;  MaxCol: Integer): string;
function WrapText(const Line: string; MaxCol: Integer): string;

{ *****************************************************************************
  Integer conversions
  *****************************************************************************}

Function SwapEndian(W : Word) : Word;
Function SwapEndian(C : Cardinal) : Cardinal;
function IntToStr(const Value: Integer): string;
Function TryStrToInt(const S : String; Out res : Integer) : Boolean;
Function TryStrToInt(const S : String; Out res : NativeInt) : Boolean;
function TryStrToInt(const S: String; out res: NativeInt; Const aSettings : TFormatSettings): Boolean;
Function StrToIntDef(const S : String; Const aDef : Integer) : Integer;
Function StrToIntDef(const S : String; Const aDef : NativeInt) : NativeInt;
Function StrToInt(const S : String) : Integer;
Function StrToNativeInt(const S : String) : NativeInt;
function StrToUInt(const s: string): Cardinal;
function StrToUIntDef(const s: string; aDef : Cardinal): Cardinal;
function UIntToStr(Value: Cardinal): string; 
function TryStrToUInt(const s: string; out C: Cardinal): Boolean;
// For compatibility
Function StrToInt64(const S : String) : NativeLargeInt;
Function StrToInt64Def(const S : String; ADefault : NativeLargeInt) : NativeLargeInt;
Function TryStrToInt64(const S : String; Out res : NativeLargeInt) : Boolean; overload;
Function TryStrToInt64(const S : String; Out res : Int64) : Boolean; unimplemented; overload; // only 53 bits
Function StrToQWord(const S : String) : NativeLargeUInt;
Function StrToQWordDef(const S : String; ADefault : NativeLargeUInt) : NativeLargeUInt;
Function TryStrToQWord(const S : String; Out res : NativeLargeUInt) : Boolean; overload;
Function TryStrToQWord(const S : String; Out res : QWord) : Boolean; unimplemented; overload; // only 52 bits
Function StrToUInt64(const S : String) : NativeLargeUInt;
Function StrToUInt64Def(const S : String; ADefault : NativeLargeUInt) : NativeLargeUInt;
Function TryStrToUInt64(const S : String; Out res : NativeLargeUInt) : Boolean; overload;
Function TryStrToUInt64(const S : String; Out res : UInt64) : Boolean; unimplemented; overload; // only 52 bits
Function StrToDWord(const S : String) : DWord;
Function StrToDWordDef(const S : String; ADefault : DWord) : DWord;
Function TryStrToDWord(const S : String; Out res : DWord) : Boolean;

function IntToHex(Value: NativeInt; Digits: Integer): string; overload;

{ *****************************************************************************
  Float conversions
  *****************************************************************************}

const
  // Note: Currency is internally a double, multiplied by 10000 and truncated.
  // The below values are the safe limits, within every step exists.
  // Since currency is a double it can take much larger values, but the result
  // may differ from Delphi/FPC
  MaxCurrency: Currency =  900719925474.0991; // fpc: 922337203685477.5807;
  MinCurrency: Currency = -900719925474.0991; // fpc: -922337203685477.5808;

Type
  TFloatFormat = (ffFixed,ffGeneral,ffExponent,ffNumber,ffCurrency);

Function FloatToDecimal(Value : double; Precision, Decimals : integer) :  TFloatRec;
Function FloatToStr(Value: Double): String; overload;
Function FloatToStr(Value: Double; const aSettings : TFormatSettings): String; overload;
Function FloatToStrF(const Value : double; format: TFloatFormat; Precision, Digits: Integer): String; overload;
Function FloatToStrF(const Value : double; format: TFloatFormat; Precision, Digits: Integer;const aSettings : TFormatSettings): String; overload;
Function TryStrToFloat(const S : String; Out res : Extended) : Boolean; overload;
Function TryStrToFloat(const S : String; Out res : Extended; const aSettings : TFormatSettings) : Boolean; overload;
Function TryStrToFloat(const S : String; Out res : Double) : Boolean; overload;
Function TryStrToFloat(const S : String; Out res : Double; const aSettings : TFormatSettings) : Boolean; overload;
Function StrToFloatDef(const S : String; Const aDef : Double) : Double;
function StrToFloatDef(const S: String; const aDef: Double; const aSettings : TFormatSettings): Double;
Function StrToFloat(const S : String) : Double; overload;
Function StrToFloat(const S : String; const aSettings : TFormatSettings) : Double; overload;
Function FormatFloat (Fmt : String; aValue : Double) : String; overload;
Function FormatFloat (Fmt : String; aValue : Double; aSettings : TFormatSettings) : String; overload;


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
  TShowExceptionHandler = Procedure (Const Msg : String);
  TUncaughtPascalExceptionHandler = reference to Procedure(aObject : TObject);
  TUncaughtJSExceptionHandler = reference to Procedure(aObject : TJSObject);

var
  OnGetEnvironmentVariable: TOnGetEnvironmentVariable;
  OnGetEnvironmentString: TOnGetEnvironmentString;
  OnGetEnvironmentVariableCount: TOnGetEnvironmentVariableCount;
  // Handler to show an exception (used when showexception is called)
  OnShowException : TShowExceptionHandler = nil;

// Set handlers for uncaught exceptions. These will call HookUncaughtExceptions
// They return the old exception handler, if there was any.
Function SetOnUnCaughtExceptionHandler(aValue : TUncaughtPascalExceptionHandler) : TUncaughtPascalExceptionHandler;
Function SetOnUnCaughtExceptionHandler(aValue : TUncaughtJSExceptionHandler) : TUncaughtJSExceptionHandler;
// Hook the rtl handler for uncaught exceptions. If any exception handlers were set, they will be called.
// If none were set, the exceptions will be displayed using ShowException.
Procedure HookUncaughtExceptions;

function GetEnvironmentVariable(Const EnvVar: String): String;
function GetEnvironmentVariableCount: Integer;
function GetEnvironmentString(Index: Integer): String;


procedure ShowException(ExceptObject: TObject; ExceptAddr: Pointer = Nil);
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
  TimeSeparator : char deprecated;
  DateSeparator : char deprecated;
  ShortDateFormat : string deprecated;
  LongDateFormat : string deprecated;
  ShortTimeFormat : string deprecated;
  LongTimeFormat : string deprecated;
  DecimalSeparator : string deprecated;
  ThousandSeparator : string deprecated;
  TimeAMString : string deprecated;
  TimePMString : string deprecated;


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

Var
  ShortMonthNames : TMonthNames deprecated;
  LongMonthNames : TMonthNames deprecated;
  ShortDayNames : TDayNames deprecated;
  LongDayNames : TDayNames deprecated;


Var
  FormatSettings: TFormatSettings;
  TwoDigitYearCenturyWindow : word = 50;
                             { Threshold to be subtracted from year before age-detection.}


// Various conversions

function DateTimeToJSDate(aDateTime : TDateTime) : TJSDate;
function JSDateToDateTime(aDate : TJSDate) : TDateTime;
function DateTimeToTimeStamp(DateTime: TDateTime): TTimeStamp;
function TimeStampToDateTime(const TimeStamp: TTimeStamp): TDateTime;
function MSecsToTimeStamp(MSecs: NativeInt): TTimeStamp;
function TimeStampToMSecs(const TimeStamp: TTimeStamp): NativeInt;
procedure DateTimeToSystemTime(DateTime: TDateTime; out SystemTime: TSystemTime);
function SystemTimeToDateTime(const SystemTime: TSystemTime): TDateTime;
Function FloatToDateTime (Const Value : Extended) : TDateTime;

// Encode/Decode
function TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean; overload;
function TryEncodeTime(Hour, Min, Sec, MSec: Word; out Time: TDateTime): Boolean;
function EncodeDate(Year, Month, Day :word): TDateTime;
function EncodeTime(Hour, Minute, Second, MilliSecond:word): TDateTime;
procedure DecodeDate(Date: TDateTime; out Year, Month, Day: word);
procedure DecodeTime(Time: TDateTime; out Hour, Minute, Second, MilliSecond: word);
function DecodeDateFully(const DateTime: TDateTime; out Year, Month, Day, DOW: Word): Boolean;
function ComposeDateTime(Date,Time : TDateTime) : TDateTime;
procedure ReplaceTime(var dati: TDateTime; NewTime : TDateTime);
procedure ReplaceDate(var DateTime: TDateTime; const NewDate: TDateTime);

function Date: TDateTime;
function Time: TDateTime;
function Now: TDateTime;
function DayOfWeek(DateTime: TDateTime): integer;
function IncMonth(const DateTime: TDateTime; NumberOfMonths: integer = 1 ): TDateTime;
procedure IncAMonth(var Year, Month, Day: Word; NumberOfMonths: Integer = 1);
function IsLeapYear(Year: Word): boolean;
function CurrentYear:Word;

// Date <-> String conversion

function DateToStr(Date: TDateTime): string; overload;
function DateToStr(Date: TDateTime; const aSettings: TFormatSettings): string; overload;
function StrToDate(const S: String): TDateTime; overload;
function StrToDate(const S: String; separator : char): TDateTime; overload;
function StrToDate(const S: String; const useformat : string; separator : char): TDateTime; overload;
function StrToDate(const S: string; const aSettings : TFormatSettings): TDateTime; overload;
function TryStrToDate(const S: String; out Value: TDateTime): Boolean; overload;
function TryStrToDate(const S: String; out Value: TDateTime; const aSettings : TFormatSettings): Boolean; overload;
function TryStrToDate(const S: String; out Value: TDateTime; separator : char): Boolean; overload;
function TryStrToDate(const S: String; out Value: TDateTime; const useformat : string; separator : char): Boolean; overload;
function StrToDateDef(const S: String; const Defvalue : TDateTime): TDateTime; overload;
function StrToDateDef(const S: String; const Defvalue : TDateTime; separator : char): TDateTime; overload;

// Time <-> String conversion

function TimeToStr(Time: TDateTime): string;overload;
function TimeToStr(Time: TDateTime; const aSettings: TFormatSettings): string;overload;
function StrToTime(const S: String): TDateTime;overload;
function StrToTime(const S: String; separator : char): TDateTime;overload;
function StrToTime(const S: string; const aSettings : TFormatSettings): TDateTime;overload;
function TryStrToTime(const S: String; out Value: TDateTime): Boolean;overload;
function TryStrToTime(const S: String; out Value: TDateTime; aSettings : TFormatSettings): Boolean;overload;
function TryStrToTime(const S: String; out Value: TDateTime; separator : char): Boolean; overload;
function StrToTimeDef(const S: String; const Defvalue : TDateTime): TDateTime;overload;
function StrToTimeDef(const S: String; const Defvalue : TDateTime; separator : char): TDateTime;overload;
function StrToTimeDef(const AString: string; const ADefault: TDateTime; const aSettings: TFormatSettings): TDateTime;

// DateTime <-> String conversion

function DateTimeToStr(DateTime: TDateTime; ForceTimeIfZero : Boolean = False): string;overload;
function DateTimeToStr(DateTime: TDateTime; const aSettings: TFormatSettings; ForceTimeIfZero : Boolean = False): string;overload;
function StrToDateTime(const S: String): TDateTime;overload;
function StrToDateTime(const s: String; const aSettings : TFormatSettings): TDateTime;overload;
function TryStrToDateTime(const S: String; out Value: TDateTime): Boolean; overload;
function TryStrToDateTime(const S: string; out Value: TDateTime; const aSettings: TFormatSettings): Boolean; overload;
function StrToDateTimeDef(const S: String; const Defvalue : TDateTime): TDateTime; overload;
function StrToDateTimeDef(const S: String; const Defvalue : TDateTime; aSettings : TFormatSettings): TDateTime; overload;
function FormatDateTime(const FormatStr: string; const DateTime: TDateTime): string; overload;
function FormatDateTime(const FormatStr: string; const DateTime: TDateTime; const aSettings: TFormatSettings): string; overload;


{ *****************************************************************************
  Currency support
  *****************************************************************************}


Var
  CurrencyFormat : Byte deprecated;
  NegCurrFormat : Byte deprecated;
  CurrencyDecimals : Byte deprecated;
  CurrencyString : String deprecated;

Function FloattoCurr (Const Value : Extended) : Currency;
function TryFloatToCurr(const Value: Extended; var AResult: Currency): Boolean;
Function CurrToStr(Value: Currency): string;
Function CurrToStr(Value: Currency; Const aSettings: TFormatSettings): string;
function StrToCurr(const S: string): Currency;
function StrToCurr(const S: string; Const aSettings: TFormatSettings): Currency;
function TryStrToCurr(const S: string;Out Value : Currency): Boolean;
function TryStrToCurr(const S: string;Out Value : Currency; Const aSettings: TFormatSettings): Boolean;
function StrToCurrDef(const S: string; Default : Currency): Currency;
function StrToCurrDef(const S: string; Default : Currency; Const aSettings: TFormatSettings): Currency;

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

Function EncodeHTMLEntities (S : String) : String;

{ ---------------------------------------------------------------------
  Type Helpers
  ---------------------------------------------------------------------}

Type
  TCharArray = Array of char;

  TByteBitIndex = 0..7;
  TShortIntBitIndex = 0..7;
  TWordBitIndex = 0..15;
  TSmallIntBitIndex = 0..15;
  TCardinalBitIndex = 0..31;
  TIntegerBitIndex = 0..31;
  TLongIntBitIndex = TIntegerBitIndex;
  TQwordBitIndex = 0..52;
  TInt64BitIndex = 0..52;
  TNativeIntBitIndex = 0..52;
  TNativeUIntBitIndex = 0..52;

Const
  CPUEndian = {$IFNDEF FPC_LITTLE_ENDIAN}TEndian.Big{$ELSE}TEndian.Little{$ENDIF};

Type

  { TGuidHelper }

  TGuidHelper = record helper for TGUID
    class function Create(Src : TGUID; BigEndian: Boolean): TGUID; overload; static;
    class function Create(const Buf : TJSArrayBuffer; AStartIndex: Cardinal; BigEndian: Boolean): TGUID; overload; static;
    class function Create(const Data: array of Byte; AStartIndex: Cardinal; BigEndian: Boolean): TGUID; overload; static;
    Class Function Create(const B: TBytes; DataEndian: TEndian = CPUEndian): TGUID; overload; static; inline;
    Class Function Create(const B: TBytes; AStartIndex: Cardinal; DataEndian: TEndian = CPUEndian): TGUID; overload; static;
    Class Function Create(const S: string): TGUID; overload; static;
    Class Function Create(A: Integer; B: SmallInt; C: SmallInt; const D: TBytes): TGUID; overload; static;
//    Class Function Create(A: Integer; B: SmallInt; C: SmallInt; D, E, F, G, H, I, J, K: Byte): TGUID; overload; static;
    Class Function Create(A: Cardinal; B: Word; C: Word; D, E, F, G, H, I, J, K: Byte): TGUID; overload; static;
    Class Function NewGuid: TGUID; static;
    Function ToByteArray(DataEndian: TEndian = CPUEndian): TBytes;
    Function ToString(SkipBrackets: Boolean = False): string;
  end;


{$SCOPEDENUMS ON}
  TStringSplitOptions = (None, ExcludeEmpty);
{$SCOPEDENUMS OFF}

  { TStringHelper }

  TStringHelper = Type Helper for String
  Private
    Function GetChar(AIndex : SizeInt) : Char;
    Function GetLength : SizeInt;
  public
    const Empty = '';
    // Methods
    Class Function Compare(const A: string; const B: string): Integer; overload; static; //inline;
    Class Function Compare(const A: string; const B: string; IgnoreCase: Boolean): Integer; overload; static; //inline; //deprecated 'Use same with TCompareOptions';
    Class Function Compare(const A: string; const B: string; Options: TCompareOptions): Integer; overload; static; // inline;
    Class Function Compare(const A: string; IndexA: SizeInt; const B: string; IndexB: SizeInt; ALen: SizeInt): Integer; overload; static; // inline;
    Class Function Compare(const A: string; IndexA: SizeInt; const B: string; IndexB: SizeInt; ALen: SizeInt; IgnoreCase: Boolean): Integer; overload; static; // inline; //deprecated 'Use same with TCompareOptions';
    Class Function Compare(const A: string; IndexA: SizeInt; const B: string; IndexB: SizeInt; ALen: SizeInt; Options: TCompareOptions): Integer; overload; static;//  inline;
    Class Function CompareOrdinal(const A: string; const B: string): Integer; overload; static;
    Class Function CompareOrdinal(const A: string; IndexA: SizeInt; const B: string; IndexB: SizeInt; ALen: SizeInt): Integer; overload; static;
    Class Function CompareText(const A: string; const B: string): Integer; static; inline;
    Class Function Copy(const Str: string): string; inline; static;
    Class Function Create(AChar: Char; ACount: SizeInt): string; overload; inline; static;
    Class Function Create(const AValue: array of Char): string; overload; static;
    Class Function Create(const AValue: array of Char; StartIndex: SizeInt; ALen: SizeInt): string; overload; static;
    Class Function EndsText(const ASubText, AText: string): Boolean; static;
    Class Function Equals(const a: string; const b: string): Boolean; overload; static;
    Class Function Format(const AFormat: string; const args: array of const): string; overload; static;
    Class Function IsNullOrEmpty(const AValue: string): Boolean; static;
    Class Function IsNullOrWhiteSpace(const AValue: string): Boolean; static;
    Class Function Join(const Separator: string; const Values: array of const): string; overload; static;
    Class Function Join(const Separator: string; const Values: array of string): string; overload; static;
    Class Function Join(const Separator: string; const Values: array of string; StartIndex: SizeInt; ACount: SizeInt): string; overload; static;
    Class Function LowerCase(const S: string): string; overload; static; inline;
    Class Function Parse(const AValue: Boolean): string; overload; static; inline;
    Class Function Parse(const AValue: Extended): string; overload; static;inline;
    Class Function Parse(const AValue: NativeInt): string; overload; static; inline;
    Class Function Parse(const AValue: Integer): string; overload; static; inline;
    Class Function ToBoolean(const S: string): Boolean; overload; static; inline;
    Class Function ToDouble(const S: string): Double; overload; static; inline;
    Class Function ToExtended(const S: string): Extended; overload; static; inline;
    Class Function ToNativeInt(const S: string): NativeInt; overload; static; inline;
    Class Function ToInteger(const S: string): Integer; overload; static; inline;
    Class Function UpperCase(const S: string): string; overload; static; inline;
    Class Function ToCharArray(const S : String) : TCharArray; static;
    Function CompareTo(const B: string): Integer;
    Function Contains(const AValue: string): Boolean;
    Function CountChar(const C: Char): SizeInt;
    Function DeQuotedString: string; overload;
    Function DeQuotedString(const AQuoteChar: Char): string; overload;
    Function EndsWith(const AValue: string): Boolean; overload; inline;
    Function EndsWith(const AValue: string; IgnoreCase: Boolean): Boolean; overload;
    Function Equals(const AValue: string): Boolean; overload;
    Function Format(const args: array of const): string; overload;
    Function GetHashCode: Integer;
    Function IndexOf(AValue: Char): SizeInt; overload; inline;
    Function IndexOf(const AValue: string): SizeInt; overload; inline;
    Function IndexOf(AValue: Char; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOf(const AValue: string; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOf(AValue: Char; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function IndexOf(const AValue: string; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function IndexOfUnQuoted(const AValue: string; StartQuote, EndQuote: Char; StartIndex: SizeInt = 0): SizeInt; overload;
    Function IndexOfAny(const AnyOf: string): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of Char): SizeInt; overload;
    Function IndexOfAny(const AnyOf: String; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of Char; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: String; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of Char; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of String): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of String; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of String; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function IndexOfAny(const AnyOf: array of String; StartIndex: SizeInt; ACount: SizeInt; Out AMatch : SizeInt): SizeInt; overload;
    Function IndexOfAnyUnquoted(const AnyOf: array of Char; StartQuote, EndQuote: Char): SizeInt; overload;
    Function IndexOfAnyUnquoted(const AnyOf: array of Char; StartQuote, EndQuote: Char; StartIndex: SizeInt): SizeInt; overload;
    Function IndexOfAnyUnquoted(const AnyOf: array of Char; StartQuote, EndQuote: Char; StartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    function IndexOfAnyUnquoted(const AnyOf: array of string; StartQuote, EndQuote: Char; StartIndex: SizeInt; Out Matched: SizeInt): SizeInt; overload;
    Function Insert(StartIndex: SizeInt; const AValue: string): string;
    Function IsDelimiter(const Delimiters: string; Index: SizeInt): Boolean;
    Function IsEmpty: Boolean;
    Function LastDelimiter(const Delims: string): SizeInt;
    Function LastIndexOf(AValue: Char): SizeInt; overload;
    Function LastIndexOf(const AValue: string): SizeInt; overload;
    Function LastIndexOf(AValue: Char; AStartIndex: SizeInt): SizeInt; overload;
    Function LastIndexOf(const AValue: string; AStartIndex: SizeInt): SizeInt; overload;
    Function LastIndexOf(AValue: Char; AStartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function LastIndexOf(const AValue: string; AStartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function LastIndexOfAny(const AnyOf: array of Char): SizeInt; overload;
    Function LastIndexOfAny(const AnyOf: array of Char; AStartIndex: SizeInt): SizeInt; overload;
    Function LastIndexOfAny(const AnyOf: array of Char; AStartIndex: SizeInt; ACount: SizeInt): SizeInt; overload;
    Function PadLeft(ATotalWidth: SizeInt): string; overload; inline;
    Function PadLeft(ATotalWidth: SizeInt; PaddingChar: Char): string; overload; inline;
    Function PadRight(ATotalWidth: SizeInt): string; overload; inline;
    Function PadRight(ATotalWidth: SizeInt; PaddingChar: Char): string; overload; inline;
    Function QuotedString: string; overload;
    Function QuotedString(const AQuoteChar: Char): string; overload;
    Function Remove(StartIndex: SizeInt): string; overload; inline;
    Function Remove(StartIndex: SizeInt; ACount: SizeInt): string; overload; inline;
    Function Replace(OldChar: Char; NewChar: Char): string; overload;
    Function Replace(OldChar: Char; NewChar: Char; ReplaceFlags: TReplaceFlags): string; overload;
    Function Replace(const OldValue: string; const NewValue: string): string; overload;
    Function Replace(const OldValue: string; const NewValue: string; ReplaceFlags: TReplaceFlags): string; overload;
    Function Split(const Separators: String): TStringArray; overload;
    Function Split(const Separators: array of Char): TStringArray; overload;
    Function Split(const Separators: string; ACount: SizeInt): TStringArray; overload;
    Function Split(const Separators: array of Char; ACount: SizeInt): TStringArray; overload;
    Function Split(const Separators: string; Options: TStringSplitOptions): TStringArray; overload;
    Function Split(const Separators: array of Char; Options: TStringSplitOptions): TStringArray; overload;
    Function Split(const Separators: string; ACount: SizeInt; Options: TStringSplitOptions): TStringArray; overload;
    Function Split(const Separators: array of Char; ACount: SizeInt; Options: TStringSplitOptions): TStringArray; overload;
    Function Split(const Separators: array of string): TStringArray; overload;
    Function Split(const Separators: array of string; ACount: SizeInt): TStringArray; overload;
    Function Split(const Separators: array of string; Options: TStringSplitOptions): TStringArray; overload;
    Function Split(const Separators: array of string; ACount: SizeInt; Options: TStringSplitOptions): TStringArray; overload;
    Function Split(const Separators: String; AQuote: Char): TStringArray; overload;
    Function Split(const Separators: array of Char; AQuote: Char): TStringArray; overload;
    Function Split(const Separators: String; AQuoteStart, AQuoteEnd: Char): TStringArray; overload;
    Function Split(const Separators: array of Char; AQuoteStart, AQuoteEnd: Char): TStringArray; overload;
    Function Split(const Separators: string; AQuoteStart, AQuoteEnd: Char; Options: TStringSplitOptions): TStringArray; overload;
    Function Split(const Separators: array of Char; AQuoteStart, AQuoteEnd: Char; Options: TStringSplitOptions): TStringArray; overload;
    Function Split(const Separators: string; AQuoteStart, AQuoteEnd: Char; ACount: SizeInt): TStringArray; overload;
    Function Split(const Separators: array of Char; AQuoteStart, AQuoteEnd: Char; ACount: SizeInt): TStringArray; overload;
    Function Split(const Separators: string; AQuoteStart, AQuoteEnd: Char; ACount: SizeInt; Options: TStringSplitOptions): TStringArray; overload;
    Function Split(const Separators: array of Char; AQuoteStart, AQuoteEnd: Char; ACount: SizeInt; Options: TStringSplitOptions): TStringArray; overload;
    Function Split(const Separators: array of string; AQuote: Char): TStringArray; overload;
    Function Split(const Separators: array of string; AQuoteStart, AQuoteEnd: Char): TStringArray; overload;
    Function Split(const Separators: array of string; AQuoteStart, AQuoteEnd: Char; Options: TStringSplitOptions): TStringArray; overload;
    Function Split(const Separators: array of string; AQuoteStart, AQuoteEnd: Char; ACount: SizeInt): TStringArray; overload;
    Function Split(const Separators: array of string; AQuoteStart, AQuoteEnd: Char; ACount: SizeInt; Options: TStringSplitOptions): TStringArray; overload;
    Function StartsWith(const AValue: string): Boolean; overload; inline;
    Function StartsWith(const AValue: string; IgnoreCase: Boolean): Boolean; overload;
    Function Substring(AStartIndex: SizeInt): string; overload;
    Function Substring(AStartIndex: SizeInt; ALen: SizeInt): string; overload;
    Function ToBoolean: Boolean; overload; inline;
    Function ToInteger: Integer; overload; inline;
    Function ToNativeInt: NativeInt; overload; inline;
    Function ToDouble: Double; overload; inline;
    Function ToExtended: Extended; overload; inline;
    Function ToCharArray: TCharArray; overload;
    Function ToCharArray(AStartIndex: SizeInt; ALen: SizeInt): TCharArray; overload;
    Function ToLower: string; overload; inline;
    Function ToLowerInvariant: string;
    Function ToUpper: string; overload; inline;
    Function ToUpperInvariant: string; inline;
    Function Trim: string; overload;
    Function TrimLeft: string; overload;
    Function TrimRight: string; overload;
    Function Trim(const ATrimChars: array of Char): string; overload;
    Function TrimLeft(const ATrimChars: array of Char): string; overload;
    Function TrimRight(const ATrimChars: array of Char): string; overload;
    Function TrimEnd(const ATrimChars: array of Char): string; deprecated 'Use TrimRight';
    Function TrimStart(const ATrimChars: array of Char): string; deprecated 'Use TrimLeft';
    property Chars[AIndex: SizeInt]: Char read GetChar;
    property Length: SizeInt read GetLength;
  end;

  TDoubleHelper = Type Helper for Double
  private
    Function GetB(AIndex: Cardinal): Byte;
    Function GetW(AIndex: Cardinal): Word;
    Function GetE: NativeUInt; inline;
    Function GetF: NativeUInt; inline;
    Function GetS: Boolean; inline;
    Procedure SetS(aValue : Boolean); inline;
    procedure SetB(AIndex: Cardinal; const AValue: Byte);
    procedure SetW(AIndex: Cardinal; const AValue: Word);
  public
    const
    {$push}
    {$R-}
    {$Q-}
      Epsilon          : Double = 4.9406564584124654418e-324;
      MaxValue         : Double = 1.7976931348623157081e+308;
      MinValue         : Double = -1.7976931348623157081e+308;
      // PositiveInfinity : Double = 1.0/0.0;
      // NegativeInfinity : Double = -1.0/0.0;
      // NaN              : Double = 0.0/0.0;
    {$POP}
    Class Function IsInfinity(const AValue: Double): Boolean; overload; inline; static;
    Class Function IsNan(const AValue: Double): Boolean; overload; inline; static;
    Class Function IsNegativeInfinity(const AValue: Double): Boolean; overload; inline; static;
    Class Function IsPositiveInfinity(const AValue: Double): Boolean; overload; inline; static;
    Class Function Parse(const AString: string): Double; overload; inline; static;
    Class Function ToString(const AValue: Double): string; overload; inline; static;
    Class Function ToString(const AValue: Double; const AFormat: TFloatFormat; const APrecision, ADigits: Integer): string; overload; inline; static;
    Class Function TryParse(const AString: string; out AValue: Double): Boolean; overload; inline; static;

    Procedure BuildUp(const ASignFlag: Boolean; const AMantissa: NativeUInt; const AExponent: Integer);
    Function Exponent: Integer;
    Function Fraction: Extended;
    Function IsInfinity: Boolean; overload; inline;
    Function IsNan: Boolean; overload; inline;
    Function IsNegativeInfinity: Boolean; overload; inline;
    Function IsPositiveInfinity: Boolean; overload; inline;
    Function Mantissa: NativeUInt;

    Function ToString(const AFormat: TFloatFormat; const APrecision, ADigits: Integer): string; overload; inline;
    Function ToString: string; overload; inline;

    property Bytes[AIndex: Cardinal]: Byte read GetB write SetB;  // 0..7
    property Words[AIndex: Cardinal]: Word read GetW write SetW; // 0..3
    property Sign: Boolean read GetS Write SetS;
    property Exp: NativeUInt read GetE;
    property Frac: NativeUInt read GetF;
  end;

  TByteHelper = Type Helper for Byte
  public
    const
      MaxValue = 255;
      MinValue = 0;
  public
    Class Function Parse(const AString: string): Byte; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Byte): string; overload; inline; static;
    Class Function TryParse(const AString: string; out AValue: Byte): Boolean; inline; static;
  Public
    Function ToBoolean: Boolean; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function ToBinString:string; inline;
    Function ToHexString(const AMinDigits: Integer): string; overload; inline;
    Function ToHexString: string; overload; inline;
    Function ToString: string; overload; inline;
    Function SetBit(const Index: TByteBitIndex) : Byte; inline;
    Function ClearBit(const Index: TByteBitIndex) : Byte; inline;
    Function ToggleBit(const Index: TByteBitIndex) : Byte; inline;
    Function TestBit(const Index:TByteBitIndex):Boolean; inline;
  end;

  TShortIntHelper = Type Helper for ShortInt
  public
    const
      MaxValue = 127;
      MinValue = -128;
  public
    Class Function Parse(const AString: string): ShortInt; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: ShortInt): string; overload; inline; static;
    Class Function TryParse(const AString: string; out AValue: ShortInt): Boolean; inline; static;
  public
    Function ToBoolean: Boolean; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function ToBinString:string; inline;
    Function ToHexString(const AMinDigits: Integer): string; overload; inline;
    Function ToHexString: string; overload; inline;
    Function ToString: string; overload; inline;
    Function SetBit(const Index: TShortIntBitIndex): Shortint; inline;
    Function ClearBit(const Index: TShortIntBitIndex): Shortint; inline;
    Function ToggleBit(const Index: TShortIntBitIndex): Shortint; inline;
    Function TestBit(const Index:TShortIntBitIndex):Boolean; inline;
  end;

  TSmallIntHelper = Type Helper for SmallInt
  public
    const
      MaxValue = 32767;
      MinValue = -32768;
  public
    Class Function Parse(const AString: string): SmallInt; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: SmallInt): string; overload; inline; static;
    Class Function TryParse(const AString: string; out AValue: SmallInt): Boolean; inline; static;
  public
    Function ToString: string; overload; inline;
    Function ToBoolean: Boolean; inline;
    Function ToBinString:string; inline;
    Function ToHexString: string; overload; inline;
    Function ToHexString(const AMinDigits: Integer): string; overload; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function SetBit(const Index: TSmallIntBitIndex) : Smallint; inline;
    Function ClearBit(const Index: TSmallIntBitIndex) : Smallint; inline;
    Function ToggleBit(const Index: TSmallIntBitIndex) : Smallint; inline;
    Function TestBit(const Index:TSmallIntBitIndex):Boolean; inline;
  end;

  TWordHelper = Type Helper for Word
  public
    const
      MaxValue = 65535;
      MinValue = 0;
  Public
    Class Function Parse(const AString: string): Word; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Word): string; overload; inline; static;
    Class Function TryParse(const AString: string; out AValue: Word): Boolean; inline; static;
  Public
    Function ToBoolean: Boolean; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function ToBinString:string; inline;
    Function ToHexString(const AMinDigits: Integer): string; overload; inline;
    Function ToHexString: string; overload; inline;
    Function ToString: string; overload; inline;
    Function SetBit(const Index: TWordBitIndex) : Word; inline;
    Function ClearBit(const Index: TWordBitIndex) : Word; inline;
    Function ToggleBit(const Index: TWordBitIndex) : Word; inline;
    Function TestBit(const Index:TWordBitIndex):Boolean; inline;
  end;

  TCardinalHelper = Type Helper for Cardinal { for LongWord Type too }
  public
    const
      MaxValue = 4294967295;
      MinValue = 0;
  Public
    Class Function Parse(const AString: string): Cardinal; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Cardinal): string; overload; inline; static;
    Class Function TryParse(const AString: string; out AValue: Cardinal): Boolean; inline; static;
  Public
    Function ToBoolean: Boolean; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function ToBinString:string; inline;
    Function ToHexString(const AMinDigits: Integer): string; overload; inline;
    Function ToHexString: string; overload; inline;
    Function ToString: string; overload; inline;
    Function SetBit(const Index: TCardinalBitIndex) : Cardinal; inline;
    Function ClearBit(const Index: TCardinalBitIndex) : Cardinal; inline;
    Function ToggleBit(const Index: TCardinalBitIndex) : Cardinal; inline;
    Function TestBit(const Index:TCardinalBitIndex):Boolean; inline;
  end;

  TIntegerHelper = Type Helper for Integer { for LongInt Type too }
  public
    const
      MaxValue = 2147483647;
      MinValue = -2147483648;
  Public
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Integer): string; overload; inline; static;
    Class Function Parse(const AString: string): Integer; inline; static;
    Class Function TryParse(const AString: string; out AValue: Integer): Boolean; inline; static;
  Public
    Function ToBoolean: Boolean; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function ToBinString:string; inline;
    Function ToHexString(const AMinDigits: Integer): string; overload; inline;
    Function ToHexString: string; overload; inline;
    Function ToString: string; overload; inline;
    Function SetBit(const Index: TIntegerBitIndex) : Integer; inline;
    Function ClearBit(const Index: TIntegerBitIndex) : Integer; inline;
    Function ToggleBit(const Index: TIntegerBitIndex) : Integer; inline;
    Function TestBit(const Index:TIntegerBitIndex):Boolean; inline;
  end;

  TNativeIntHelper = Type Helper for NativeInt
  public
    const
      MaxValue = High(NativeInt);
      MinValue = Low(NativeInt);
  Public
    Class Function Parse(const AString: string): NativeInt; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: NativeInt): string; overload; inline; static;
    Class Function TryParse(const AString: string; out AValue: NativeInt): Boolean; inline; static;
  Public
    Function ToBoolean: Boolean; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function ToBinString:string; inline;
    Function ToHexString(const AMinDigits: Integer): string; overload; inline;
    Function ToHexString: string; overload; inline;
    Function ToString: string; overload; inline;
    Function SetBit(const Index: TNativeIntBitIndex) : NativeInt; inline;
    Function ClearBit(const Index: TNativeIntBitIndex) : NativeInt; inline;
    Function ToggleBit(const Index: TNativeIntBitIndex) : NativeInt; inline;
    Function TestBit(const Index:TNativeIntBitIndex):Boolean; inline;
  end;

  TNativeUIntHelper = Type Helper for NativeUInt
  public
    const
      MaxValue = High(NativeUInt);
      MinValue = 0;
  Public
    Class Function Parse(const AString: string): NativeUInt; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: NativeUInt): string; overload; inline; static;
    Class Function TryParse(const AString: string; out AValue: NativeUInt): Boolean; inline; static;
  Public
    Function ToBoolean: Boolean; inline;
    Function ToDouble: Double; inline;
    Function ToExtended: Extended; inline;
    Function ToBinString:string; inline;
    Function ToHexString(const AMinDigits: Integer): string; overload; inline;
    Function ToHexString: string; overload; inline;
    Function ToSingle: Single; inline;
    Function ToString: string; overload; inline;
    Function SetBit(const Index: TNativeUIntBitIndex) : NativeUint; inline;
    Function ClearBit(const Index: TNativeUIntBitIndex): NativeUint; inline;
    Function ToggleBit(const Index: TNativeUIntBitIndex) : NativeUint; inline;
    Function TestBit(const Index:TNativeUIntBitIndex) :Boolean; inline;
  end;

{$SCOPEDENUMS ON}
  TUseBoolStrs = (False,True);
{$SCOPEDENUMS OFF}

  TBooleanHelper = Type Helper for Boolean
  public
    Class Function Parse(const S: string): Boolean; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Boolean; UseBoolStrs: TUseBoolStrs = TUseBoolStrs.false): string; overload; inline; static;
    Class Function TryToParse(const S: string; out AValue: Boolean): Boolean; inline; static;
  Public
    Function ToInteger: Integer; inline;
    Function ToString(UseBoolStrs: TUseBoolStrs = TUseBoolStrs.False): string; overload; inline;
  end;

  TByteBoolHelper = Type Helper for ByteBool
  public
    Class Function Parse(const S: string): Boolean; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Boolean; UseBoolStrs : Boolean = False): string; overload; inline; static;
    Class Function TryToParse(const S: string; out AValue: Boolean): Boolean; inline; static;
  Public
    Function ToInteger: Integer; inline;
    Function ToString(UseBoolStrs: Boolean = False): string; overload; inline;
  end;

  TWordBoolHelper = Type Helper for WordBool
  public
    Class Function Parse(const S: string): Boolean; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Boolean; UseBoolStrs: Boolean = False): string; overload; inline; static;
    Class Function TryToParse(const S: string; out AValue: Boolean): Boolean; inline; static;
  Public
    Function ToInteger: Integer; inline;
    Function ToString(UseBoolStrs: boolean = False): string; overload; inline;
  end;

  TLongBoolHelper = Type Helper for LongBool
  public
    Class Function Parse(const S: string): Boolean; inline; static;
    Class Function Size: Integer; inline; static;
    Class Function ToString(const AValue: Boolean; UseBoolStrs: Boolean= False): string; overload; inline; static;
    Class Function TryToParse(const S: string; out AValue: Boolean): Boolean; inline; static;
  public
    Function ToInteger: Integer; inline;
    Function ToString(UseBoolStrs: Boolean = False): string; overload; inline;
  end;

  { TStringBuilder }

  TStringBuilder = class
  private
    const
      DefaultCapacity = 64;
  private
    Function  GetCapacity: Integer;
    Procedure SetCapacity(AValue: Integer);
    Function  GetC(Index: Integer): Char;
    Procedure SetC(Index: Integer; AValue: Char);
    Function  GetLength: Integer; inline;
    Procedure SetLength(AValue: Integer);
  protected
    FData: String;
    FMaxCapacity: Integer;
    // Raise error on range check.
    Procedure CheckRange(Idx,Count,MaxLen : Integer);inline;
    Procedure CheckNegative(Const AValue : Integer; Const AName: String); inline;
    // All appends/inserts pass through here.

    Procedure DoAppend(Const S : String);virtual;
    Procedure DoAppend(const AValue: Array of char; Idx, aCount: Integer); virtual;
    Procedure DoInsert(Index: Integer; const AValue: String); virtual;
    Procedure DoInsert(Index: Integer; const AValue: Array of char; StartIndex, aCharCount: Integer); virtual;
    Procedure DoReplace(Index: Integer; const Old, New: String); virtual;
    Procedure Grow;
    Procedure Shrink;
  public
    Constructor Create;
    Constructor Create(aCapacity: Integer);
    Constructor Create(const AValue: String);
    Constructor Create(aCapacity: Integer; aMaxCapacity: Integer);
    Constructor Create(const AValue: String; aCapacity: Integer);
    Constructor Create(const AValue: String; StartIndex: Integer; aLength: Integer; aCapacity: Integer);

    Function Append(const AValue: Boolean): TStringBuilder;
    Function Append(const AValue: Byte): TStringBuilder;
    Function Append(const AValue: Char): TStringBuilder;
    Function Append(const AValue: Currency): TStringBuilder;
    Function Append(const AValue: Double): TStringBuilder;
    Function Append(const AValue: Smallint): TStringBuilder;
    Function Append(const AValue: LongInt): TStringBuilder;
    Function Append(const AValue: Int64): TStringBuilder;
    Function Append(const AValue: TObject): TStringBuilder;
    Function Append(const AValue: Shortint): TStringBuilder;
    Function Append(const AValue: Single): TStringBuilder;
    Function Append(const AValue: UInt64): TStringBuilder;
    Function Append(const AValue: Array of char): TStringBuilder;
    Function Append(const AValue: Word): TStringBuilder;
    Function Append(const AValue: Cardinal): TStringBuilder;
    Function Append(const AValue: String): TStringBuilder;
    Function Append(const AValue: Char; RepeatCount: Integer): TStringBuilder;
    Function Append(const AValue: Array of char; StartIndex: Integer; SBCharCount: Integer): TStringBuilder;
    Function Append(const AValue: String; StartIndex: Integer; Count: Integer): TStringBuilder;

    Function Append(const Fmt: String; const Args: array of const): TStringBuilder;
    Function AppendFormat(const Fmt: String; const Args: array of const): TStringBuilder;
    Function AppendLine: TStringBuilder;
    Function AppendLine(const AValue: String): TStringBuilder;

    Procedure Clear;
    Procedure CopyTo(SourceIndex: Integer; Var Destination: Array of char; DestinationIndex: Integer; Count: Integer);
    Function EnsureCapacity(aCapacity: Integer): Integer;
    Function Equals(StringBuilder: TStringBuilder): Boolean; reintroduce;

    Function Insert(Index: Integer; const AValue: Boolean): TStringBuilder;
    Function Insert(Index: Integer; const AValue: Byte): TStringBuilder;
    Function Insert(Index: Integer; const AValue: Char): TStringBuilder;
    Function Insert(Index: Integer; const AValue: Currency): TStringBuilder;
    Function Insert(Index: Integer; const AValue: Double): TStringBuilder;
    Function Insert(Index: Integer; const AValue: Smallint): TStringBuilder;
    Function Insert(Index: Integer; const AValue: LongInt): TStringBuilder;
    Function Insert(Index: Integer; const AValue: Array of char): TStringBuilder;
    Function Insert(Index: Integer; const AValue: Int64): TStringBuilder;
    Function Insert(Index: Integer; const AValue: TObject): TStringBuilder;
    Function Insert(Index: Integer; const AValue: Shortint): TStringBuilder;
    Function Insert(Index: Integer; const AValue: Single): TStringBuilder;
    Function Insert(Index: Integer; const AValue: String): TStringBuilder;
    Function Insert(Index: Integer; const AValue: Word): TStringBuilder;
    Function Insert(Index: Integer; const AValue: Cardinal): TStringBuilder;
    Function Insert(Index: Integer; const AValue: UInt64): TStringBuilder;
    Function Insert(Index: Integer; const AValue: String; const aRepeatCount: Integer): TStringBuilder;
    Function Insert(Index: Integer; const AValue: Array of char; startIndex: Integer; SBCharCount: Integer): TStringBuilder;

    Function Remove(StartIndex: Integer; RemLength: Integer): TStringBuilder;

    Function Replace(const OldValue, NewValue: String): TStringBuilder;
    Function Replace(const OldValue, NewValue: String; StartIndex: Integer; Count: Integer): TStringBuilder;
    Function ToString: String; override;
    Function ToString(aStartIndex: Integer; aLength: Integer): String; reintroduce;
    property Chars[index: Integer]: Char read GetC write SetC; default;
    property Length: Integer read GetLength write SetLength;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property MaxCapacity: Integer read FMaxCapacity;
  end;



implementation

Const
  DefaultShortMonthNames : TMonthNames = (
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
  DefaultLongMonthNames : TMonthNames = (
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
  DefaultShortDayNames : TDayNames = (
    'Sun',
    'Mon',
    'Tue',
    'Wed',
    'Thu',
    'Fri',
    'Sat');

  DefaultLongDayNames : TDayNames = (
    'Sunday',
    'Monday',
    'Tuesday',
    'Wednesday',
    'Thursday',
    'Friday',
    'Saturday');


{ ---------------------------------------------------------------------
  Exception handling
  ---------------------------------------------------------------------}
Resourcestring
  SAbortError = 'Operation aborted';
  SApplicationException = 'Application raised an exception: ';
  SErrUnknownExceptionType = 'Caught unknown exception type : ';

procedure DoShowException(S : String);

begin
  if Assigned(OnShowException) then
    OnShowException(S)
  else
    begin
    {$IFDEF BROWSER}
      asm
        window.alert(S);
      end;
    {$ENDIF}
    {$IFDEF NODEJS}
      Writeln(S);
    {$ENDIF}
    end;
end;

procedure ShowException(ExceptObject: TObject; ExceptAddr: Pointer = Nil);

Var
  S : String;

begin
  S:=SApplicationException+ExceptObject.ClassName;
  if ExceptObject is Exception then
    S:=S+' : '+Exception(ExceptObject).Message;
  DoShowException(S);
  if ExceptAddr=nil then;
end;

Type
  TRTLExceptionHandler = procedure (aError : JSValue);

Var
  rtlExceptionHandler : TRTLExceptionHandler; External name 'rtl.onUncaughtException';
  rtlShowUncaughtExceptions : Boolean; External name 'rtl.showUncaughtExceptions';
  OnPascalException : TUncaughtPascalExceptionHandler;
  OnJSException : TUncaughtJSExceptionHandler;

Procedure RTLExceptionHook(aError : JSValue);

Var
  S : String;

begin
  if isClassInstance(aError) then
    begin
    if Assigned(OnPascalException) then
      OnPascalException(TObject(aError))
    else
      ShowException(TObject(aError),Nil);
    end
  else if isObject(aError) then
    begin
    if Assigned(OnJSException) then
      OnJSException(TJSObject(aError))
    else
      begin
      if TJSObject(aError).hasOwnProperty('message') then
        S:=SErrUnknownExceptionType+String(TJSObject(aError).Properties['message'])
      else
        S:=SErrUnknownExceptionType+TJSObject(aError).toString;
      DoShowException(S);
      end
    end
  else
    begin
    S:=SErrUnknownExceptionType+String(aError);
    DoShowException(S);
    end;
end;



Function SetOnUnCaughtExceptionHandler(aValue : TUncaughtPascalExceptionHandler) : TUncaughtPascalExceptionHandler;

begin
  Result:=OnPascalException;
  OnPascalException:=aValue;
  HookUncaughtExceptions;
end;

Function SetOnUnCaughtExceptionHandler(aValue : TUncaughtJSExceptionHandler) : TUncaughtJSExceptionHandler;

begin
  Result:=OnJSException;
  OnJSException:=aValue;
  HookUncaughtExceptions;
end;

Procedure HookUncaughtExceptions;

begin
  rtlExceptionHandler:=@RTLExceptionHook;
  rtlShowUncaughtExceptions:=True;
end;

procedure Abort;
begin
  Raise EAbort.Create(SAbortError);
end;

{$IFNDEF MAKESTUB}
Type
  TCharSet = Set of Char;

Function CharInSet(Ch: Char;Const CSet : TCharSet) : Boolean; overload;

begin
  Result:=Ch in CSet;
end;
{$ENDIF}

function CharInSet(Ch: Char; const CSet: array of char): Boolean; overload;

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
  return S.replace(/^[\s\uFEFF\xA0\x00-\x1f]+/,'').replace(/[\s\uFEFF\xA0\x00-\x1f]+$/,'');
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

function FloatToDecimal(Value: double; Precision, Decimals: integer): TFloatRec;

Const
  Rounds = '123456789:';

var
  Buffer: String;  //Though str func returns only 25 chars, this might change in the future
  InfNan: string;
  OutPos,Error, N, L, C: Integer;
  GotNonZeroBeforeDot, BeforeDot : boolean;

begin
  // Writeln('Precision ',Precision,' decimals: ',Decimals);
  Result.Negative:=False;
  Result.Exponent:=0;
  For C:=0 to FloatRecDigits do
    Result.Digits[C]:='0';
  if Value=0 then
    exit;
  Str(Value:24,Buffer); // Double precision
  // writeln('12345678901234567890123456789012345678901234567890');
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
          Result.Digits[Outpos-1] := Buffer[N];
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

Function FloatToStr(Value: Double; const aSettings : TFormatSettings): String; overload;

begin
  Result:=FloatToStrF(Value,ffGeneral,15,0,aSettings);
end;

function FloatToStr(Value: Double): String;
begin
  Result:=FloatToStr(Value,FormatSettings);
end;

function TryStrToFloat(const S: String; out res: Extended): Boolean;
begin
  Result:=TryStrToFloat(S,double(res));
end;

function TryStrToFloat(const S: String; out res: Double): Boolean;

begin
  Result:=TryStrToFloat(S,Res,FormatSettings);
end;

function TryStrToFloat(const S: String; out res: Extended; const aSettings : TFormatSettings): Boolean;

begin
  Result:=TryStrToFloat(S,double(res),aSettings);
end;

function TryStrToFloat(const S: String; out res: Double; const aSettings : TFormatSettings): Boolean;

Var
  J : JSValue;
  N : String;

begin
  N:=S;
  // Delocalize
  if (aSettings.ThousandSeparator <>'') then
    N:=StringReplace(N,aSettings.ThousandSeparator,'',[rfReplaceAll]);
  if (aSettings.DecimalSeparator<>'.') then
    N:=StringReplace(N,aSettings.DecimalSeparator,'.',[]);
  J:=parseFloat(N);
  Result:=Not jsIsNaN(J);
  if Result then
    Res:=Double(J);
end;

function StrToFloatDef(const S: String; const aDef: Double; const aSettings : TFormatSettings): Double;
begin
  if not TryStrToFloat(S,Result,aSettings) then
    Result:=aDef;
end;

function StrToFloatDef(const S: String; const aDef: Double): Double;
begin
  if not TryStrToFloat(S,Result,FormatSettings) then
    Result:=aDef;
end;

function StrToFloat(const S: String): Double;
begin
  Result:=StrToFloat(S,FormatSettings);
end;

function StrToFloat(const S: String; const aSettings : TFormatSettings): Double;
begin
  if not TryStrToFloat(S,Result,aSettings) then
    Raise EConvertError.CreateFmt(SErrInvalidFloat,[S]);
end;

Function FormatFloat (Fmt : String; aValue : Double) : String;
begin
  Result:=FormatFloat(Fmt,aValue,FormatSettings);
end;

function FormatFloat(Fmt: String; aValue: Double; aSettings : TFormatSettings): String;

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
      AddToResult(aSettings.DecimalSeparator);
      ToResult(ADigit);
      end
    else
      begin
      // We're still before the decimal.
      ToResult(ADigit);
      if ThousandSep and ((DistToDecimal mod 3)=0) and (DistToDecimal>1) then
        AddToResult(aSettings.ThousandSeparator);
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
          SP[Result]:=I;
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
    // writeln(len);
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
            ThousandSep:=aSettings.ThousandSeparator<>#0;
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
      // Writeln(RequestedDigits,'-',DecimalPos,'+1');
      D:=RequestedDigits-DecimalPos+1;
      end;
    FV:=FloatToDecimal(aValue,P,D);
    // Writeln('Number of digits available : ',Length(FV.Digits));
    //   For p:=0 to Length(FV.Digits)-1 do
    // Writeln(P,': ',FV.Digits[p]);
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
  return s1.toLowerCase() == s2.toLowerCase();
end;

function AnsiCompareStr(const s1, s2: String): Integer;
begin
  {$IFDEF ECMAScript6}
  Result:=CompareText(TJSString(s1).normalize(),TJSString(s2).normalize());
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
Function SwapEndian(W : Word) : Word;

begin
  Result:=((W and $FF) shl 8) or ((W shr 8) and $FF)
end;

Function SwapEndian(C : Cardinal) : Cardinal;

begin
  Result:=((C and $FF) shl 24)
           or ((C and $FF00) shl 8)
           or ((C shr 8) and $FF00)
           or ((C shr 24) and $FF);
end;

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
  DS:=FormatSettings.DecimalSeparator;
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
  if (P<=0) then
    P:=Length(Result)+1;
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

function RemoveLeadingNegativeSign(var AValue: String; DS : String; aThousandSeparator : String): Boolean;

// removes negative sign in case when result is zero eg. -0.00

var
  i: PtrInt;
  TS: String;
  StartPos: PtrInt;

begin
  Result:=False;
  StartPos := 2;
  TS := aThousandSeparator;
  for i :=StartPos to length(AValue) do
    begin
    Result := (AValue[i] in ['0', DS, 'E', '+']) or (aValue[i]=TS);
    if not Result then
      break;
    end;
  if (Result) and (AValue[1]='-') then
    Delete(AValue, 1, 1);
end;

Function FormatNumberCurrency(const Value : Currency; Digits : Integer; const aSettings: TFormatSettings) : string;

Var
  Negative: Boolean;
  P : Integer;
  CS,DS,TS : String;

Begin
   DS:=aSettings.DecimalSeparator;
   TS:=aSettings.ThousandSeparator;
   CS:=aSettings.CurrencyString;
  //  Writeln('Value ',D);
   If Digits = -1 Then
     Digits := aSettings.CurrencyDecimals
   Else If Digits > 18 Then
     Digits := 18;
   Str(Value:0:Digits, Result);
   // Writeln('1. Result ',Result,' currencystring : ',CS);
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
   // Writeln('3. Result ',Result,' currencystring : ',CS);
   if Negative then
     RemoveLeadingNegativeSign(Result,DS,TS);
   // Writeln('4. Result ',Result,' currencystring : ',CurrencyString);
   // Writeln('CurrencyFormat:  ',CurrencyFormat,'NegcurrencyFormat: ',NegCurrFormat);
   If Not Negative Then
     Case aSettings.CurrencyFormat Of
       0: Result := CS + Result;
       1: Result := Result + CS;
       2: Result := CS + ' ' + Result;
       3: Result := Result + ' ' + CS;
     end
   else
     Case aSettings.NegCurrFormat Of
       0: Result := '(' + CS + Result + ')';
       1: Result := '-' + CS + Result;
       2: Result := CS + '-' + Result;
       3: Result := CS + Result + '-';
       4: Result := '(' + Result + CS + ')';
       5: Result := '-' + Result + CS;
       6: Result := Result + '-' + CS;
       7: Result := Result + CS + '-';
       8: Result := '-' + Result + ' ' + CS;
       9: Result := '-' + CS + ' ' + Result;
       10: Result := Result + ' ' + CS + '-';
       11: Result := CS + ' ' + Result + '-';
       12: Result := CS + ' ' + '-' + Result;
       13: Result := Result + '-' + ' ' + CS;
       14: Result := '(' + CS + ' ' + Result + ')';
       15: Result := '(' + Result + ' ' + CS + ')';
     end;
end;


function FloatToStrF(const Value: double; format: TFloatFormat; Precision,
  Digits: Integer): String;
begin
  Result:=FloatToStrF(Value,Format,Precision,Digits,Formatsettings);
end;

function FloatToStrF(const Value: double; format: TFloatFormat; Precision,
  Digits: Integer ;const aSettings : TFormatSettings): String;

Var
  TS,DS: string;

Begin
  DS:=aSettings.DecimalSeparator;
  TS:=aSettings.ThousandSeparator;
  Case format Of
    ffGeneral:
      Result:=FormatGeneralFloat(Value,Precision,DS);
    ffExponent:
      Result:=FormatExponentFloat(Value,Precision,Digits,DS);
    ffFixed:
      Result:=FormatFixedFloat(Value,Digits,DS);
    ffNumber:
      Result:=FormatNumberFloat(Value,Digits,DS,TS);
    ffCurrency:
     Result:=FormatNumberCurrency(Value,Digits,aSettings);
  end;
  if (Format<>ffCurrency) and (length(Result)>1) and (Result[1]='-') then
    RemoveLeadingNegativeSign(Result,DS,TS);
end;

function Format(const Fmt: String; const Args: array of const): String;

begin
  Result:=Format(Fmt,Args,FormatSettings)
end;

function Format(const Fmt: String; const Args: array of Const; const aSettings : TFormatSettings): String;

Var ChPos,OldPos,ArgPos,DoArg,Len : SizeInt;
    Hs,ToAdd : String;
    Index : Byte;
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

        if Index=High(byte) then
          ArgN:=Argpos
        else
        begin
          ArgN:=Index;
          Inc(Index);
        end;

        If (ChPos>OldPos) or (ArgN>High(Args)) then
          DoFormatError(feInvalidFormat,Fmt);

        ArgPos:=ArgN+1;

        case Args[ArgN].Vtype of
           vtInteger: Value := Args[ArgN].VInteger;
           vtNativeInt: Value := Args[ArgN].VNativeInt;
        else
          DoFormatError(feInvalidFormat,Fmt);
        end;
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
    Index:=High(byte);
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


  function Checkarg (AT : Integer; err:boolean):boolean;
  {
    Check if argument INDEX is of correct type (AT)
    If Index=-1, ArgPos is used, and argpos is augmented with 1
    DoArg is set to the argument that must be used.
  }
  begin
    result:=false;
    if Index=High(Byte) then
      DoArg:=Argpos
    else
      DoArg:=Index;
    ArgPos:=DoArg+1;
    If (Doarg>High(Args)) or (Args[Doarg].VType<>AT) then
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
              if Checkarg(vtinteger,false) then
                toAdd:=IntToStr(Args[DoArg].VInteger)
              else if CheckArg(vtNativeInt,True) then
                toAdd:=IntToStr(Args[DoArg].VNativeInt);
              Width:=Abs(width);
              Index:=Prec-Length(ToAdd);
              If ToAdd[1]<>'-' then
                ToAdd:=StringOfChar('0',Index)+ToAdd
              else
                // + 1 to accomodate for - sign in length !!
                Insert(StringOfChar('0',Index+1),toadd,2);
              end;
        'U' : begin
              if Checkarg(vtinteger,false) then
                toAdd:=IntToStr(Cardinal(Args[DoArg].VInteger))
              else if CheckArg(vtNativeInt,True) then
                toAdd:=IntToStr(NativeUInt(Args[DoArg].VNativeInt));
              Width:=Abs(width);
              Index:=Prec-Length(ToAdd);
              ToAdd:=StringOfChar('0',Index)+ToAdd
              end;
        'E' : begin
              if CheckArg(vtCurrency,false) then
                ToAdd:=FloatToStrF(Args[doarg].VCurrency,ffExponent,3,Prec,aSettings)
              else if CheckArg(vtExtended,True) then
                ToAdd:=FloatToStrF(Args[doarg].VExtended,ffExponent,3,Prec,aSettings);
              end;
        'F' : begin
              if CheckArg(vtCurrency,false) then
                ToAdd:=FloatToStrF(Args[doarg].VCurrency,ffFixed,9999,Prec,aSettings)
              else if CheckArg(vtExtended,True) then
                ToAdd:=FloatToStrF(Args[doarg].VExtended,ffFixed,9999,Prec,aSettings);
              end;
        'G' : begin
              if CheckArg(vtCurrency,false) then
                ToAdd:=FloatToStrF(Args[doarg].VCurrency,ffGeneral,Prec,3,aSettings)
              else if CheckArg(vtExtended,True) then
                ToAdd:=FloatToStrF(Args[doarg].VExtended,ffGeneral,Prec,3,aSettings);
              end;
        'N' : begin
              if CheckArg(vtCurrency,false) then
                ToAdd:=FloatToStrF(Args[doarg].VCurrency,ffNumber,9999,Prec,aSettings)
              else if CheckArg(vtExtended,True) then
                ToAdd:=FloatToStrF(Args[doarg].VExtended,ffNumber,9999,Prec,aSettings);
              end;
        'M' : begin
              if CheckArg(vtCurrency,false) then
                ToAdd:=FloatToStrF(Args[doarg].VCurrency,ffCurrency,9999,Prec,aSettings)
              else if CheckArg(vtExtended,True) then
                ToAdd:=FloatToStrF(Args[doarg].VExtended,ffCurrency,9999,Prec,aSettings);
              end;
        'S' : begin
              if CheckArg(vtUnicodeString,false) then
                hs:=Args[doarg].VUnicodeString
              else if CheckArg(vtWideChar,True) then
                hs:=Args[doarg].VWideChar;
              Index:=Length(hs);
              If (Prec<>-1) and (Index>Prec) then
                Index:=Prec;
              ToAdd:=Copy(hs,1,Index);
              end;
        'P' : Begin
              if CheckArg(vtInteger,false) then
                ToAdd:=IntToHex(Args[DoArg].VInteger,8)
              else if CheckArg(vtInteger,true) then
                ToAdd:=IntToHex(Args[DoArg].VNativeInt,16);
              end;
        'X' : begin
              if Checkarg(vtinteger,false) then
                begin
                vq:=Args[Doarg].VInteger;
                Index:=16;
                end
              else if Checkarg(vtNativeint,True) then
                begin
                vq:=Args[Doarg].VNativeInt;
                index:=31; // May need to adjust to NativeInt
                end;
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
  if LogMessageOnCreate then
    Writeln('Created exception ',ClassName,' with message: ',Msg);
end;

constructor Exception.CreateFmt(const Msg: string; const Args: array of Const
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
  const Args: array of Const; AHelpContext: Integer);
begin
  Create(Format(Msg,Args));
  fHelpContext:=AHelpContext;
end;

function Exception.ToString: String;
begin
  Result:=ClassName+': '+Message;
end;

Const
  RESpecials = '([\$\+\[\]\(\)\\\.\*\^\?\|])';

function StringReplace(aOriginal, aSearch, aReplace: string;
  Flags: TStringReplaceFlags): String;

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

function QuoteString(aOriginal: String; AQuote: Char): String;

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

Function LastDelimiter(const Delimiters, S: string): SizeInt;

begin
  Result:=Length(S);
  While (Result>0) and (Pos(S[Result],Delimiters)=0) do
    Dec(Result);
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

function WrapText(const Line, BreakStr: string;
  const BreakChars: array of char; MaxCol: Integer): string;

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

function DateTimeToJSDate(aDateTime: TDateTime): TJSDate;

Var
  Y,M,D,h,n,s,z : Word;

begin
  DecodeDate(Trunc(aDateTime),Y,M,D);
  DecodeTime(Frac(aDateTime),H,N,S,Z);
  Result:=TJSDate.New(Y,M-1,D,h,n,s,z);
end;

function JSDateToDateTime(aDate: TJSDate): TDateTime;

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


function TryEncodeDate(Year, Month, Day: Word; out Date: TDateTime): Boolean;

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

function TryEncodeTime(Hour, Min, Sec, MSec: Word; out Time: TDateTime
  ): Boolean;

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
  Result:= 1+((Trunc(DateTime) - 1) mod 7);
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
  Result:=DateToStr(Date,FormatSettings);
end;

function DateToStr(Date: TDateTime; const aSettings : TFormatSettings): string;
begin
  Result:=FormatDateTime('ddddd', Date, aSettings);
end ;

{  TimeToStr returns a string representation of Time using LongTimeFormat   }

function TimeToStr(Time: TDateTime): string;
begin
  Result:=TimeToStr(Time,FormatSettings);
end;

function TimeToStr(Time: TDateTime; const aSettings : TFormatSettings): string;
begin
  Result:=FormatDateTime('tt',Time,aSettings);
end ;


{   DateTimeToStr returns a string representation of DateTime using LongDateTimeFormat   }

Var
  DateTimeToStrFormat : Array[Boolean] of string = ('c','f');


function DateTimeToStr(DateTime: TDateTime; ForceTimeIfZero : Boolean = False): string;

begin
  Result:=DateTimeToStr(DateTime,FormatSettings,ForceTimeIfZero);
end;

function DateTimeToStr(DateTime: TDateTime; Const aSettings : TFormatSettings; ForceTimeIfZero : Boolean = False): string;


begin
  Result:=FormatDateTime(DateTimeToStrFormat[ForceTimeIfZero], DateTime,aSettings)
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
    if (FormatSettings.DateSeparator<>#0) then
      separator := FormatSettings.DateSeparator
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

function StrToDate(const S: String; const aSettings : TFormatSettings): TDateTime;

begin
  Result:=StrToDate(S,aSettings.ShortDateFormat,aSettings.DateSeparator);
end;

function StrToDate(const S: String): TDateTime;
begin
  result:=StrToDate(S,FormatSettings);
end;

function StrToDate(const S: String; separator : char): TDateTime;
begin
  result := StrToDate(S,FormatSettings.ShortDateFormat,separator)
end;


{   StrToTime converts the string S to a TDateTime value
    if S does not represent a valid time value an
    EConvertError will be raised   }


function IntStrToTime(Out ErrorMsg : String; const S: String; Len : integer; Const aSettings : TFormatSettings): TDateTime;

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
     if (Cur > Len - 1) or (S[Cur] = aSettings.TimeSeparator) or (S[Cur] = aSettings.Decimalseparator) then
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
       else if (CurChar = aSettings.TimeSeparator) then
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
       else if (CurChar = aSettings.DecimalSeparator) then
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
         allowedchars:=aSettings.DecimalSeparator+' ';
         if aSettings.TimeSeparator<>#0 then
           allowedchars:=allowedchars+aSettings.TimeSeparator;
         while (Cur < Len) and (Pos(S[Cur + 1],AllowedChars)=0)
           and (Pos(S[Cur + 1],Digits)=0) do Inc(Cur);
         ElemLen := 1 + Cur - OffSet;
//         writeln('  S[Offset] = ',S[Offset], ' S[Cur] = ',S[Cur],' ElemLen = ',ElemLen,' -> ', S[1+Offset], ElemLen);
//         writeln('  Cur = ',Cur, ', S =',S);
         AmPmStr := Copy(S,OffSet, ElemLen);

         // writeln('AmPmStr = ',ampmstr,' (',length(ampmstr),')');
         //We must compare to TimeAMString before hardcoded 'AM' for delphi compatibility
         //Also it is perfectly legal, though insane to have TimeAMString = 'PM' and vice versa
         if (CompareText(AmPmStr, aSettings.TimeAMString) = 0) then AmPm := AMPM_AM
         else if (CompareText(AmPmStr, aSettings.TimePMString) = 0) then AmPm := AMPM_PM
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

function StrToTime(const S: String; const aSettings : TFormatSettings): TDateTime;

Var
  Msg : String;

begin
  Result:=IntStrToTime(Msg,S,Length(S),aSettings);
  If (Msg<>'') then
    Raise EConvertError.Create(Msg);
end;

function StrToTime(const S: String): TDateTime;
begin
   result:=StrToTime(S, FormatSettings);
end;


function StrToTime(const S: String; separator: char): TDateTime;

Var
  aSettings : TFormatSettings;

begin
  aSettings:=TFormatSettings.Create;
  aSettings.TimeSeparator:=Separator;
  Result:=StrToTime(S,aSettings);
end;


{   StrToDateTime converts the string S to a TDateTime value
    if S does not represent a valid date and/or time value
    an EConvertError will be raised   }

function SplitDateTimeStr(DateTimeStr: String; out DateStr, TimeStr: String; Const aSettings : TFormatSettings): Integer;

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
  if (aSettings.DateSeparator = #32) and (aSettings.TimeSeparator = #32) and (Pos(#32, DateTimeStr) > 0) then
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
  if (aSettings.DateSeparator<>#32) then
    begin
    while (p<Length(DateTimeStr)) and (not (Pos(DateTimeStr[p+1],WhiteSpace)>0)) do
      Inc(p);
    end
  else
    begin
    p:=Pos(aSettings.TimeSeparator, DateTimeStr);
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
    if ((aSettings.DateSeparator<>aSettings.TimeSeparator) and (Pos(aSettings.TimeSeparator,DateStr) > 0))
       or ((aSettings.DateSeparator=aSettings.TimeSeparator) and (not TryStrToDate(DateStr, DummyDT)))  then
      begin
      TimeStr := DateStr;
      DateStr := '';
      end;
    end;
end;

function StrToDateTime(const S: String): TDateTime;

begin
  Result:=StrToDateTime(S,FormatSettings);
end;

function StrToDateTime(const S: String; Const aSettings : TFormatSettings): TDateTime;

var
  TimeStr, DateStr: String;
  PartsFound: Integer;
begin
  PartsFound := SplitDateTimeStr(S, DateStr, TimeStr,aSettings);
  case PartsFound of
    0: Result:=StrToDate('');
    1: if (Length(DateStr) > 0) then
         Result := StrToDate(DateStr,aSettings.ShortDateFormat,aSettings.DateSeparator)
       else
         Result := StrToTime(TimeStr);
    2: Result := ComposeDateTime(StrTodate(DateStr,aSettings.ShortDateFormat,aSettings.DateSeparator),
                                  StrToTime(TimeStr));
  end;
end;

function FormatDateTime(const FormatStr: string; const DateTime: TDateTime): string;

begin
  Result:=FormatDateTime(FormatStr,DateTime,FormatSettings);
end;

function FormatDateTime(const FormatStr: string; const DateTime: TDateTime; const aSettings : TFormatSettings): string;

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
              StoreString(aSettings.TimeAMString)
            else
              StoreString(aSettings.TimePMString);
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
          StoreString(aSettings.DateSeparator);
          end;
        ':': StoreString(aSettings.TimeSeparator);
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
                  3: StoreString(aSettings.ShortMonthNames[Month]);
                else
                  StoreString(aSettings.LongMonthNames[Month]);
                end;
              end;
            end;
            'D': begin
              case Count of
                1: StoreInt(Day, 0);
                2: StoreInt(Day, 2);
                3: StoreString(aSettings.ShortDayNames[DayOfWeek-1]);
                4: StoreString(aSettings.LongDayNames[DayOfWeek-1]);
                5: StoreFormat(aSettings.ShortDateFormat, Nesting+1, False);
              else
                StoreFormat(aSettings.LongDateFormat, Nesting+1, False);
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
		   StoreFormat(aSettings.ShortTimeFormat, Nesting+1, True)
                 else
	           StoreFormat(aSettings.LongTimeFormat, Nesting+1, True);
            'C': begin
                   StoreFormat(aSettings.ShortDateFormat, Nesting+1, False);
                   if (Hour<>0) or (Minute<>0) or (Second<>0) then
                     begin
                      StoreString(' ');
                      StoreFormat(aSettings.LongTimeFormat, Nesting+1, True);
                     end;
                 end;
            'F': begin
                   StoreFormat(aSettings.ShortDateFormat, Nesting+1, False);
                   StoreString(' ');
                   StoreFormat(aSettings.LongTimeFormat, Nesting+1, True);
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
  Result:=TryStrToDate(S,Value,FormatSettings);
end;

function TryStrToDate(const S: String; out Value: TDateTime; separator : char): Boolean;

begin
  Result:=TryStrToDate(S,Value,FormatSettings.ShortDateFormat,Separator);
end;

function TryStrToDate(const S: String; out Value: TDateTime;  const useformat : string; separator : char): Boolean;

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

function TryStrToDate(const S: String; out Value: TDateTime; const aSettings : TFormatSettings): Boolean;

begin
  Result:=TryStrToDate(S,Value,aSettings.ShortDateFormat,aSettings.DateSeparator);
end;

function TryStrToTime(const S: String; out Value: TDateTime; aSettings : TFormatSettings): Boolean;

Var
  Msg : String;

begin
  Result:=Length(S)<>0;
  If Result then
    begin
      Value:=IntStrToTime(Msg,S,Length(S),aSettings);
      Result:=(Msg='');
    end;
end;

function TryStrToTime(const S: String; out Value: TDateTime; separator : char): Boolean;

Var
  Fmt : TFormatSettings;

begin
  fmt:=TFormatSettings.Create;
  fmt.TimeSeparator:=Separator;
  Result:=TryStrToTime(S,Value,Fmt);
end;

function TryStrToTime(const S: String; out Value: TDateTime): Boolean;
begin
  result:=TryStrToTime(S,Value,FormatSettings);
end;

function TryStrToDateTime(const S: String; out Value: TDateTime): Boolean;

begin
  Result:=TryStrToDateTime(S,Value,FormatSettings);
end;

function TryStrToDateTime(const S: String; out Value: TDateTime; Const aSettings : TFormatSettings): Boolean;

var
  I: integer;
  dtdate, dttime :TDateTime;
begin
  result:=false;
  I:=Pos(aSettings.TimeSeparator,S);
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

function StrToDateTimeDef(const S: String; const Defvalue : TDateTime; aSettings : TFormatSettings): TDateTime; overload;
begin
  if not TryStrToDateTime(s,Result,aSettings) Then
    result:=defvalue;
end;

function StrToDateTimeDef(const S: String; const Defvalue : TDateTime): TDateTime;
begin
  Result:=StrToDateTimeDef(s,DefValue,FormatSettings);
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

function StrToTimeDef(const AString: string; const ADefault: TDateTime;
  const aSettings: TFormatSettings): TDateTime;
begin
  if not TryStrToTime(AString, Result, aSettings) Then
    Result := ADefault;
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

function FloatToDateTime(const Value: Extended): TDateTime;
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
  Result:=FloatToStrF(Value,ffGeneral,-1,0,FormatSettings);
end;


function CurrToStr(Value: Currency; const aSettings: TFormatSettings): string;
begin
  Result:=FloatToStrF(Value,ffGeneral,-1,0,aSettings);
end;


function StrToCurr(const S: string): Currency;

begin
  if not TryStrToCurr(S,Result) then
    Raise EConvertError.createfmt(SInvalidCurrency,[S]);
end;

function StrToCurr(const S: string; const aSettings: TFormatSettings): Currency;
begin
  if not TryStrToCurr(S,Result,aSettings) then
    Raise EConvertError.createfmt(SInvalidCurrency,[S]);
end;

function TryStrToCurr(const S: string; out Value: Currency): Boolean;

Var
  D : Double;

begin
  Result:=TryStrToFloat(S,D,FormatSettings);
  if Result then
    Value:=D;
end;


function TryStrToCurr(const S: string; out Value: Currency; const aSettings: TFormatSettings): Boolean;
Var
  D : Double;

begin
  Result:=TryStrToFloat(S,D,aSettings);
  if Result then
    Value:=D;
end;


function StrToCurrDef(const S: string; Default: Currency): Currency;

Var
  R : Currency;

begin
  if TryStrToCurr(S,R,FormatSettings) then
    Result:=R
  else
    Result:=Default;
end;


function StrToCurrDef(const S: string; Default: Currency; const aSettings: TFormatSettings): Currency;
Var
  R : Currency;

begin
  if TryStrToCurr(S,R,aSettings) then
    Result:=R
  else
    Result:=Default;
end;


{ ---------------------------------------------------------------------
  Interface related
  ---------------------------------------------------------------------}
function Supports(const Instance: IInterface; const AClass: TClass; out Obj
  ): Boolean;
begin
  Result := (Instance<>nil) and (Instance.QueryInterface(IObjectInstance,Obj)=S_OK)
     and (TObject(Obj).InheritsFrom(AClass));
end;

function Supports(const Instance: IInterface; const IID: TGuid; out Intf
  ): Boolean;
begin
  Result:=(Instance<>nil) and (Instance.QueryInterface(IID,Intf)=S_OK);
end;

function Supports(const Instance: TObject; const IID: TGuid; out Intf): Boolean;
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

function Supports(const Instance: IInterface; const IID: TGuid): Boolean;
var
  Temp: IInterface;
begin
  Result:=Supports(Instance,IID,Temp);
end;

function Supports(const Instance: TObject; const IID: TGuid): Boolean;
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

function Supports(const AClass: TClass; const IID: TGuid): Boolean;
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

function TryStringToGUID(const s: string; out Guid: TGuid): Boolean;
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

function StringToGUID(const S: string): TGuid;
begin
  if not TryStringToGUID(S, Result) then
    raise EConvertError.CreateFmt(SInvalidGUID, [S]);
end;

function GUIDToString(const guid: TGuid): string;
begin
  Result:=System.GUIDToString(guid);
end;

function IsEqualGUID(const guid1, guid2: TGuid): Boolean;
var
  i: integer;
begin
  if (guid1.D1<>guid2.D1) or (guid1.D2<>guid2.D2) or (guid1.D3<>guid2.D3) then
    exit(false);
  for i:=0 to 7 do if guid1.D4[i]<>guid2.D4[i] then exit(false);
  Result:=true;
end;

function GuidCase(const guid: TGuid; const List: array of TGuid): Integer;
begin
  for Result := High(List) downto 0 do
    if IsEqualGUID(guid, List[Result]) then
      Exit;
  Result := -1;
end;

function CreateGUID(out GUID: TGUID): Integer;

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

function TryStrToInt(const S: String; out res: Integer): Boolean;

Var
  NI : NativeInt;

begin
  Result:=TryStrToInt(S,NI);
  Result:=Result and (-2147483648<=NI) and (NI<=2147483647);
  if Result then
    res:=NI;
end;


function IntTryStrToInt(const S: String; out res: NativeInt; Const aSep : string): Boolean;

Var
  Radix : Integer = 10;
  N : String;
  J : JSValue;

begin
  N:=S;
  // Javascript Parseint allows 1.0 or 1E0 to be an integer, so we must check for this to get the same behaviour as FPC/Delphi.
  if (Pos(aSep,N)<>0) or (Pos('.',N)<>0) then
    exit(False);
  case Copy(N,1,1) of
  '$': Radix:=16;
  '&': Radix:=8;
  '%': Radix:=2;
  end;
  // Check for E after we know radix
  if (Radix<>16) and (Pos('e',LowerCase(N))<>0) then
    exit(False);
  If Radix<>10 then
    Delete(N,1,1);
  J:=parseInt(N,Radix);
  Result:=Not jsIsNan(j);
  if Result then
    res:=NativeInt(J);
end;

function TryStrToInt(const S: String; out res: NativeInt): Boolean;

begin
  Result:=IntTryStrToInt(S,res,FormatSettings.DecimalSeparator);
end;

function TryStrToInt(const S: String; out res: NativeInt; Const aSettings : TFormatSettings): Boolean;

begin
  Result:=IntTryStrToInt(S,res,aSettings.DecimalSeparator);
end;

function StrToIntDef(const S: String; const aDef: Integer): Integer;

Var
  R : NativeInt;

begin
  if TryStrToInt(S,R) then
    Result:=R
  else
    Result:=aDef;
end;

function StrToIntDef(const S: String; const aDef: NativeInt): NativeInt;

Var
  R : NativeInt;

begin
  if TryStrToInt(S,R) then
    Result:=R
  else
    Result:=aDef;
end;

function StrToInt(const S: String): Integer;

Var
  R : NativeInt;

begin
  if not TryStrToInt(S,R) then
    Raise EConvertError.CreateFmt(SErrInvalidInteger,[S]);
  Result:=R;
end;

function StrToNativeInt(const S: String): NativeInt;

begin
  if not TryStrToInt(S,Result) then
    Raise EConvertError.CreateFmt(SErrInvalidInteger,[S]);
end;

function StrToUInt(const s: string): Cardinal;

begin
  If not TryStrToUint(S,Result) then
    Raise EConvertError.CreateFmt(SErrInvalidInteger,[S])
end;

function StrToUIntDef(const s: string; aDef : Cardinal): Cardinal;

begin
  If not TryStrToUint(S,Result) then
    Result:=aDef;
end;

function UIntToStr(Value: Cardinal): string; 

begin
  Result:=IntToStr(Value);
end;

function TryStrToUInt(const s: string; out C: Cardinal): Boolean;
Var
  N : NativeInt;
begin
  Result:=TryStrToInt(S,N);
  Result:=(N>=0) and (N<=high(longword));
  If Result then 
    C:=N;
end;

function StrToInt64(const S: String): NativeLargeInt;

Var
  N : NativeInt;

begin
  if not TryStrToInt(S,N) then
    Raise EConvertError.CreateFmt(SErrInvalidInteger,[S]);
  Result:=N;
end;

function TryStrToInt64(const S: String; out res: NativeLargeInt): Boolean;

Var
  R : nativeint;

begin
  Result:=TryStrToInt(S,R);
  If Result then
    Res:=R;
end;

function TryStrToInt64(const S: String; out res: Int64): Boolean;
begin
  Result:=TryStrToInt64(S,NativeLargeInt(res));
end;

function StrToInt64Def(const S: String; ADefault: NativeLargeInt
  ): NativeLargeInt;


begin
  if not TryStrToInt64(S,Result) then
    Result:=ADefault;
end;

function StrToQWord(const S: String): NativeLargeUInt;

Var
  N : NativeInt;

begin
  if (not TryStrToInt(S,N)) or (N<0) then
    Raise EConvertError.CreateFmt(SErrInvalidInteger,[S]);
  Result:=N;
end;

function TryStrToQWord(const S: String; out res: NativeLargeUInt): Boolean;

Var
  R : nativeint;

begin
  Result:=TryStrToInt(S,R) and (R>=0);
  If Result then
    Res:=R;
end;

function TryStrToQWord(const S: String; out res: QWord): Boolean;
begin
  Result:=TryStrToQWord(S,NativeLargeUInt(res));
end;

function StrToQWordDef(const S: String; ADefault: NativeLargeUInt
  ): NativeLargeUInt;

begin
  if Not TryStrToQword(S,Result) then
    Result:=ADefault;
end;

function StrToUInt64(const S: String): NativeLargeUInt;

Var
  N : NativeInt;

begin
  if (not TryStrToInt(S,N)) or (N<0) then
    Raise EConvertError.CreateFmt(SErrInvalidInteger,[S]);
  Result:=N;
end;

function TryStrToUInt64(const S: String; out res: NativeLargeUInt): Boolean;

Var
  R : nativeint;

begin
  Result:=TryStrToInt(S,R) and (R>=0);
  If Result then
    Res:=R;
end;

function TryStrToUInt64(const S: String; out res: UInt64): Boolean;
begin
  Result:=TryStrToUInt64(S,NativeLargeUInt(res));
end;

function StrToUInt64Def(const S: String; ADefault: NativeLargeUInt
  ): NativeLargeUInt;


begin
  if Not TryStrToUInt64(S,Result) then
    Result:=ADefault;
end;

function TryStrToDWord(const S: String; out res: DWord): Boolean;

Var
  R : nativeint;

begin
  Result:=TryStrToInt(S,R) and (R>=0) and (R<=DWord($FFFFFFFF));
  If Result then
    Res:=R;
end;

function StrToDWord(const S: String): DWord;

begin
  if not TryStrToDWord(S,Result) then
    Raise EConvertError.CreateFmt(SErrInvalidInteger,[S]);
end;


function StrToDWordDef(const S: String; ADefault: DWord): DWord;

begin
  if Not TryStrToDWord(S,Result) then
    Result:=ADefault;
end;


function IntToHex(Value: NativeInt; Digits: integer): string;

begin
//  Result:=HexStr(Value,Digits);     // TestNegLongintHelper  Failed: "ToHexString" expected: <FFFE0000> but was: <00-20000> !
  Result:='';
  if Value<0 then
    asm
    if (Value<0) Value = 0xFFFFFFFF + Value + 1;
    end;
  asm
  Result=Value.toString(16);
  end;
  Result:=UpperCase(Result);
  while (Length(Result)<Digits) do
    Result:='0'+Result;
end;



{ TFormatSettings }


{ TFormatSettings }

class function TFormatSettings.Create: TFormatSettings;
begin
  Result := Create(GetJSLocale);
end;


class function TFormatSettings.Create(const ALocale: string): TFormatSettings;

begin

  Result.LongDayNames:=DefaultLongDayNames;
  Result.ShortDayNames:=DefaultShortDayNames;
  Result.ShortMonthNames:=DefaultShortMonthNames;
  Result.LongMonthNames:=DefaultLongMonthNames;
  Result.DateTimeToStrFormat[False] := 'c';
  Result.DateTimeToStrFormat[True] := 'f';
  Result.DateSeparator := '-';
  Result.TimeSeparator := ':';
  Result.ShortDateFormat := 'yyyy-mm-dd';
  Result.LongDateFormat := 'ddd, yyyy-mm-dd';
  Result.ShortTimeFormat := 'hh:nn';
  Result.LongTimeFormat := 'hh:nn:ss';
  Result.DecimalSeparator := '.';
  Result.ThousandSeparator := ',';
  Result.TimeAMString := 'AM';
  Result.TimePMString := 'PM';
  Result.TwoDigitYearCenturyWindow := 50;
  Result.CurrencyFormat:=0;
  Result.NegCurrFormat:=0;
  Result.CurrencyDecimals:=2;
  Result.CurrencyString:='$';
  If Assigned(TFormatSettings.InitLocaleHandler) then
    TFormatSettings.InitLocaleHandler(UpperCase(aLocale),Result);
end;

class function TFormatSettings.GetJSLocale: string; assembler;
asm
  return Intl.DateTimeFormat().resolvedOptions().locale
end;

class function TFormatSettings.GetLocaleDecimalSeparator(const ALocale: string): string; assembler;
asm
  var lNumber = 1.1;
  lNumber = lNumber.toLocaleString(ALocale).substring(1, 2);
  return lNumber;
end;

class function TFormatSettings.GetLocaleLongDayName(const ADayOfWeek: Integer; const ALocale: string): string; assembler;
asm
  var lBaseDate = new Date(2017, 0, 1); // Sunday
  lBaseDate.setDate(lBaseDate.getDate() + ADayOfWeek - 1);
  return lBaseDate.toLocaleDateString(ALocale, { weekday: 'long' });
end;

class function TFormatSettings.GetLocaleLongMonthName(const AMonth: Integer; const ALocale: string): string; assembler;
asm
  var lBaseDate = new Date(2017, AMonth - 1, 1);
  return lBaseDate.toLocaleDateString(ALocale, { month: 'long' });
end;

class function TFormatSettings.GetLocaleShortDayName(const ADayOfWeek: Integer; const ALocale: string): string;

Var
  d : TJSDate;

begin
  d:=TJSDate.New(2017, 0, 1); // Sunday
  d.Date:=d.Date + ADayOfWeek - 1;
  Result:=d.toLocaleDateString(aLocale, new(['weekday','short']));
end;

class function TFormatSettings.GetLocaleShortMonthName(const AMonth: Integer; const ALocale: string): string;
Var
  d : TJSDate;

begin
  d:=TJSDate.New(2017, aMonth-1, 1); // Sunday
  Result:=d.toLocaleDateString(aLocale,new(['month','short']));
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

function ExtractRelativepath(const BaseName, DestName: PathStr): PathStr;

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

function SetDirSeparators(const FileName: PathStr): PathStr;

Var
  I : integer;

begin
  Result:=FileName;
  For I:=1 to Length(Result) do
    If CharInSet(Result[I],AllowDirectorySeparators) then
      Result[i]:=PathDelim;
end;

function GetDirs(DirName: PathStr): TPathStrArray;

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

function IncludeTrailingPathDelimiter(const Path: PathStr): PathStr;

Var
  l : Integer;

begin
  Result:=Path;
  l:=Length(Result);
  If (L=0) or not CharInSet(Result[l],AllowDirectorySeparators) then
    Result:=Result+PathDelim;
end;

function ExcludeTrailingPathDelimiter(const Path: PathStr): PathStr;

Var
  L : Integer;

begin
  L:=Length(Path);
  If (L>0) and CharInSet(Path[L],AllowDirectorySeparators) then
    Dec(L);
  Result:=Copy(Path,1,L);
end;

function IncludeLeadingPathDelimiter(const Path: PathStr): PathStr;

Var
  l : Integer;

begin
  Result:=Path;
  l:=Length(Result);
  If (L=0) or not CharInSet(Result[1],AllowDirectorySeparators) then
    Result:=PathDelim+Result;
end;

function ExcludeLeadingPathDelimiter(const Path: PathStr): PathStr;

Var
  L : Integer;

begin
  Result:=Path;
  L:=Length(Result);
  If (L>0) and CharInSet(Result[1],AllowDirectorySeparators) then
    Delete(Result,1,1);
end;

function IsPathDelimiter(const Path: PathStr; Index: Integer): Boolean;

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

Function EncodeHTMLEntities (S : String) : String;

begin
  Result:='';
  if S='' then exit;
  asm
   return S.replace(/[\u00A0-\u9999<>\&]/gim, function(i) {
     return '&#'+i.charCodeAt(0)+';';
   });
  end;
end;

{ ---------------------------------------------------------------------
  Type helpers implementation
  ---------------------------------------------------------------------}

{ ---------------------------------------------------------------------
  TGUIDHelper
  ---------------------------------------------------------------------}

Procedure NotImplemented(S : String);

begin
  Raise Exception.Create('Not yet implemented : '+S);
end;

class function TGuidHelper.Create(Src: TGUID; BigEndian: Boolean): TGUID;
begin
  Result:=Src;
  if Not Bigendian then
    begin
    Result.D1:=SwapEndian(Result.D1);
    Result.D2:=SwapEndian(Result.D2);
    Result.D3:=SwapEndian(Result.D3);
    end;
end;

class function TGuidHelper.Create(const Buf: TJSArrayBuffer; AStartIndex: Cardinal; BigEndian: Boolean): TGUID;

Var
  A : Cardinal;
  B,C : Word;
  V : TJSDataView;

begin
  V:=TJSDataView.New(Buf);
  // The get functions return by default correct endianness.
  if BigEndian then
    begin
    A:=V.getUint32(aStartIndex);
    B:=V.getUint16(AStartIndex+4);
    C:=V.getUint16(AStartIndex+6);
    end
  else
    begin
    A:=SwapEndian(V.getUint32(aStartIndex));
    B:=SwapEndian(V.getUint16(AStartIndex+4));
    C:=SwapEndian(V.getUint16(AStartIndex+6));
    end;
  Result:=Create(A,B,C,V.GetUint8(AStartIndex+8),V.GetUint8(AStartIndex+9),V.GetUint8(AStartIndex+10),V.GetUint8(AStartIndex+11),V.GetUint8(AStartIndex+12),V.GetUint8(AStartIndex+13),V.GetUint8(AStartIndex+14),V.GetUint8(AStartIndex+15));
end;


class function TGuidHelper.Create(const Data: array of Byte; AStartIndex: Cardinal; BigEndian: Boolean): TGUID;

Var
  D : TJSUint8Array;

begin
  if ((System.Length(Data)-AStartIndex)<16) then
    raise EArgumentException.CreateFmt('The length of a GUID array must be at least %d',[]);
  D:=TJSUint8Array.From(Data);
  Result:=Create(D.buffer,aStartIndex,BigEndian);
end;


class function TGuidHelper.Create(const B: TBytes; DataEndian: TEndian): TGUID;

begin
  Result:=Create(B,0,DataEndian);
end;

class function TGuidHelper.Create(const B: TBytes; AStartIndex: Cardinal; DataEndian: TEndian): TGUID;

begin
  if ((System.Length(B)-AStartIndex)<16) then
    raise EArgumentException.CreateFmt('The length of a GUID array must be at least %d',[]);
  Result:=Create(B,AStartIndex,DataEndian=TEndian.Big);
end;

class function TGuidHelper.Create(const S: string): TGUID;

begin
  Result:=StringToGUID(S);
end;

class function TGuidHelper.Create(A: Integer; B: SmallInt; C: SmallInt; const D: TBytes): TGUID;

begin
  if (System.Length(D)<>8) then
    raise EArgumentException.CreateFmt('The length of a GUID array must be %d',[]);
  Result:=Create(Cardinal(A),Word(B),Word(C),D[0],D[1],D[2],D[3],D[4],D[5],D[6],D[7]);
end;

(*
class function TGuidHelper.Create(A: Integer; B: SmallInt; C: SmallInt; D, E, F, G, H, I, J, K: Byte): TGUID;

begin
  Result:=Create(Cardinal(A),Word(B),Word(C),D,E,F,G,H,I,J,K);
end;
*)
class function TGuidHelper.Create(A: Cardinal; B: Word; C: Word; D, E, F, G, H, I, J, K: Byte): TGUID;

begin
  Result.D1 := Cardinal(A);
  Result.D2 := Word(B);
  Result.D3 := Word(C);
  Result.D4[0] := D;
  Result.D4[1] := E;
  Result.D4[2] := F;
  Result.D4[3] := G;
  Result.D4[4] := H;
  Result.D4[5] := I;
  Result.D4[6] := J;
  Result.D4[7] := K;
end;

class function TGuidHelper.NewGuid: TGUID;

begin
  CreateGUID(Result)
end;

function TGuidHelper.ToByteArray(DataEndian: TEndian): TBytes;

Var
  D : TJSUint8Array;
  V : TJSDataView;
  I : Integer;

begin
  D:=TJSUint8array.New(16);
  V:=TJSDataView.New(D.buffer);
  V.setUint32(0,D1,DataEndian=TEndian.Little);
  V.setUint16(4,D2,DataEndian=TEndian.Little);
  V.setUint16(6,D3,DataEndian=TEndian.Little);
  for I:=0 to 7 do
    V.setUint8(8+I,D4[i]);
  SetLength(Result, 16);
  for I:=0 to 15 do
    Result[i]:=V.getUint8(I);
end;

function TGuidHelper.ToString(SkipBrackets: Boolean): string;

begin
  Result:=GuidToString(Self);
  If SkipBrackets then
    Result:=Copy(Result,2,Length(Result)-2);
end;

{ ---------------------------------------------------------------------
  TStringHelper
  ---------------------------------------------------------------------}

Function HaveChar(AChar : Char; const AList: array of Char) : Boolean;

Var
  I : SizeInt;

begin
  I:=0;
  Result:=False;
  While (Not Result) and (I<Length(AList)) do
    begin
    Result:=(AList[i]=AChar);
    Inc(I);
    end;
end;

function TStringHelper.GetChar(AIndex: SizeInt): Char;
begin
  Result:=Self[AIndex+1];
end;


function TStringHelper.GetLength: SizeInt;

begin
  Result:=System.Length(Self);
end;


class function TStringHelper.Compare(const A: string; const B: string): Integer;
begin
  Result:=Compare(A,0,B,0,System.Length(B),[]);
end;


class function TStringHelper.Compare(const A: string; const B: string;
  IgnoreCase: Boolean): Integer; //deprecated 'Use same with TCompareOptions';
begin
  if IgnoreCase then
    Result:=Compare(A,B,[coIgnoreCase])
  else
    Result:=Compare(A,B,[]);
end;


class function TStringHelper.Compare(const A: string; const B: string;
  Options: TCompareOptions): Integer;
begin
  Result:=Compare(A,0,B,0,System.Length(B),Options);
end;


class function TStringHelper.Compare(const A: string; IndexA: SizeInt;
  const B: string; IndexB: SizeInt; ALen: SizeInt): Integer;
begin
  Result:=Compare(A,IndexA,B,IndexB,ALen,[]);
end;


class function TStringHelper.Compare(const A: string; IndexA: SizeInt;
  const B: string; IndexB: SizeInt; ALen: SizeInt; IgnoreCase: Boolean
  ): Integer; //deprecated 'Use same with TCompareOptions';
begin
  if IgnoreCase then
    Result:=Compare(A,IndexA,B,IndexB,ALen,[coIgnoreCase])
  else
    Result:=Compare(A,IndexA,B,IndexB,ALen,[])
end;


class function TStringHelper.Compare(const A: string; IndexA: SizeInt;
  const B: string; IndexB: SizeInt; ALen: SizeInt; Options: TCompareOptions
  ): Integer;

Var
  AL,BL : String;

begin
  AL:=System.Copy(A,IndexA+1,aLen);
  BL:=System.Copy(B,IndexB+1,aLen);
  if (coIgnoreCase in Options) then
    Result:=TJSString(UpperCase(AL)).localeCompare(UpperCase(BL))
  else
    Result:=TJSString(AL).localeCompare(BL)
end;


class function TStringHelper.CompareOrdinal(const A: string; const B: string
  ): Integer;

Var
  L : SizeInt;

begin
  L:=System.Length(B);
  if L>System.Length(A) then
    L:=System.Length(A);
  Result:=CompareOrdinal(A,0,B,0,L);
end;


class function TStringHelper.CompareOrdinal(const A: string; IndexA: SizeInt;
  const B: string; IndexB: SizeInt; ALen: SizeInt): Integer;

Var
  I,M : integer;

begin
  M:=System.Length(A)-IndexA;
  If M>(System.Length(B)-IndexB) then
    M:=(System.Length(B)-IndexB);
  if M>aLen then
    M:=aLen;
  I:=0;
  Result:=0;
  While (Result=0) and (I<M) do
    begin
    Result:=TJSString(A).charCodeAt(IndexA+I)-TJSString(B).charCodeAt(IndexB+I);
    Inc(I);
    end;
end;


class function TStringHelper.CompareText(const A: string; const B: string
  ): Integer;
begin
  Result:=Sysutils.CompareText(A,B);
end;


class function TStringHelper.Copy(const Str: string): string;
begin
  Result:=Str;
end;


class function TStringHelper.Create(AChar: Char; ACount: SizeInt): string;
begin
   Result:=StringOfChar(AChar,ACount);
end;


class function TStringHelper.Create(const AValue: array of Char): string;

begin
  Result:=Create(AValue,0,System.Length(AValue));
end;


class function TStringHelper.Create(const AValue: array of Char;
  StartIndex: SizeInt; ALen: SizeInt): string;
Var
  I : Integer;
begin
  SetLength(Result,ALen);
  For I:=1 to ALen do
    Result[I]:=AValue[StartIndex+I-1];
end;


class function TStringHelper.EndsText(const ASubText, AText: string): Boolean;
begin
  Result:=(ASubText<>'') and (sysutils.CompareText(System.Copy(AText,System.Length(AText)-System.Length(ASubText)+1,System.Length(ASubText)),ASubText)=0);
end;


class function TStringHelper.Equals(const a: string; const b: string): Boolean;
begin
  Result:=A=B;
end;


class function TStringHelper.Format(const AFormat: string; const args: array of const): string;
begin
  Result:=Sysutils.Format(AFormat,Args);
end;


class function TStringHelper.IsNullOrEmpty(const AValue: string): Boolean;
begin
  Result:=system.Length(AValue)=0;
end;


class function TStringHelper.IsNullOrWhiteSpace(const AValue: string): Boolean;
begin
  Result:=system.Length(sysutils.Trim(AValue))=0;
end;


class function TStringHelper.Join(const Separator: string; const Values: array of const): string;

begin
  Result:=TJSArray(Values).Join(Separator);
end;

class function TStringHelper.Join(const Separator: string;
  const Values: array of string): string;
begin
  Result:=TJSArray(Values).Join(Separator);
end;


class function TStringHelper.Join(const Separator: string;
  const Values: array of string; StartIndex: SizeInt; ACount: SizeInt): string;

Var
  VLen : SizeInt;

begin
  VLen:=High(Values);
  If (ACount<0) or ((StartIndex>0) and (StartIndex>VLen)) then
    raise ERangeError.Create(SRangeError);
  If (ACount=0) or (VLen<0) then
    Result:=''
  else
    Result:=TJSArray(Values).Slice(StartIndex,StartIndex+aCount).Join(Separator);
end;


class function TStringHelper.LowerCase(const S: string): string;
begin
  Result:=sysutils.Lowercase(S);
end;


class function TStringHelper.Parse(const AValue: Boolean): string;
begin
  Result:=BoolToStr(AValue);
end;


class function TStringHelper.Parse(const AValue: Extended): string;
begin
  Result:=FloatToStr(AValue);
end;


class function TStringHelper.Parse(const AValue: NativeInt): string;
begin
  Result:=IntToStr(AValue);
end;


class function TStringHelper.Parse(const AValue: Integer): string;
begin
  Result:=IntToStr(AValue);
end;


class function TStringHelper.ToBoolean(const S: string): Boolean;
begin
  Result:=StrToBool(S);
end;


class function TStringHelper.ToDouble(const S: string): Double;
begin
  Result:=StrToFloat(S);
end;


class function TStringHelper.ToExtended(const S: string): Extended;
begin
  Result:=StrToFloat(S);
end;


class function TStringHelper.ToNativeInt(const S: string): NativeInt;
begin
  Result:=StrToInt64(S);
end;


class function TStringHelper.ToInteger(const S: string): Integer;
begin
  Result:=StrToInt(S);
end;


class function TStringHelper.UpperCase(const S: string): string;
begin
  Result:=sysutils.Uppercase(S);
end;

class function TStringHelper.ToCharArray(const S: String): TCharArray;

Var
  I,Len: integer;

begin
  Len:=System.Length(S);
  SetLength(Result,Len);
  For I:=1 to Len do
    Result[I-1]:=S[I];
end;

function TStringHelper.CompareTo(const B: string): Integer;
begin
  // Order is important
  Result:=Compare(Self,B);
end;


function TStringHelper.Contains(const AValue: string): Boolean;
begin
  Result:=(AValue<>'') and (Pos(AValue,Self)>0);
end;


function TStringHelper.CountChar(const C: Char): SizeInt;

Var
  S : Char;
begin
  Result:=0;
  For S in Self do
    if (S=C) then
      Inc(Result);
end;


function TStringHelper.DeQuotedString: string;
begin
  Result:=DeQuotedString('''');
end;


function TStringHelper.DeQuotedString(const AQuoteChar: Char): string;

var
  L,I : SizeInt;
  Res : Array of Char;
  PS,PD : SizeInt;
  IsQuote : Boolean;

begin
  L:=System.Length(Self);
  if (L<2) or Not ((Self[1]=AQuoteChar) and (Self[L]=AQuoteChar)) then
    Exit(Self);
  SetLength(Res,L);
  IsQuote:=False;
  PS:=2;
  PD:=1;
  For I:=2 to L-1 do
    begin
    if (Self[PS]=AQuoteChar) then
      begin
      IsQuote:=Not IsQuote;
      if Not IsQuote then
        begin
        Result[PD]:=Self[PS];
        Inc(PD);
        end;
      end
    else
      begin
      if IsQuote then
        IsQuote:=false;
      Result[PD]:=Self[PS];
      Inc(PD);
      end;
    Inc(PS);
    end;
  SetLength(Result,PD-1);
end;


function TStringHelper.EndsWith(const AValue: string): Boolean;
begin
  Result:=EndsWith(AValue,False);
end;


function TStringHelper.EndsWith(const AValue: string; IgnoreCase: Boolean): Boolean;

Var
  L : SizeInt;
  S : String;

begin
  L:=system.Length(AVAlue);
  Result:=L=0;
  if Not Result then
    begin
    S:=system.Copy(Self,Length-L+1,L);
    Result:=system.Length(S)=L;
    if Result then
      if IgnoreCase then
        Result:=CompareText(S,AValue)=0
      else
        Result:=S=AValue;
    end;
end;


function TStringHelper.Equals(const AValue: string): Boolean;

begin
  Result:=(Self=AValue);
end;


function TStringHelper.Format(const args: array of const): string;

begin
  Result:=Sysutils.Format(Self,Args);
end;


function TStringHelper.GetHashCode: Integer;

// Taken from contnrs, fphash
var
  P,pmax : Integer;
  L : TJSString;

begin
{$push}
{$Q-}
  L:=TJSString(Self);
  Result:=0;

  P:=1;
  pmax:=length+1;
  while (p<pmax) do
    begin
    Result:=LongWord(LongInt(Result shl 5) - LongInt(Result)) xor L.CharCodeAt(P);
    Inc(p);
    end;
{$pop}
end;


function TStringHelper.IndexOf(AValue: Char): SizeInt;
begin
  Result:=IndexOf(AValue,0,Length);
end;


function TStringHelper.IndexOf(const AValue: string): SizeInt;
begin
  Result:=IndexOf(AValue,0,Length);
end;


function TStringHelper.IndexOf(AValue: Char; StartIndex: SizeInt): SizeInt;
begin
  Result:=IndexOf(AValue,StartIndex,Length);
end;


function TStringHelper.IndexOf(const AValue: string; StartIndex: SizeInt
  ): SizeInt;
begin
  Result:=IndexOf(AValue,StartIndex,Length);
end;


function TStringHelper.IndexOf(AValue: Char; StartIndex: SizeInt;
  ACount: SizeInt): SizeInt;

Var
  S : String;

begin
  S:=System.Copy(Self,StartIndex+1,ACount);
  Result:=Pos(AValue,S)-1;
  if Result<>-1 then
    Result:=Result+StartIndex;
end;


function TStringHelper.IndexOf(const AValue: string; StartIndex: SizeInt;
  ACount: SizeInt): SizeInt;

Var
  S : String;

begin
  S:=System.Copy(Self,StartIndex+1,ACount);
  Result:=Pos(AValue,S)-1;
  if Result<>-1 then
    Result:=Result+StartIndex;
end;

function TStringHelper.IndexOfUnQuoted(const AValue: string; StartQuote,
  EndQuote: Char; StartIndex: SizeInt = 0): SizeInt;

Var
  LV : SizeInt;
  S : String;

  Function MatchAt(I : SizeInt) : Boolean ; Inline;

  Var
    J : SizeInt;

  begin
    J:=1;
    Repeat
      Result:=(S[I+J-1]=AValue[j]);
      Inc(J);
    Until (Not Result) or (J>LV);
  end;

Var
  I,L,Q: SizeInt;

begin
  S:=Self;
  Result:=-1;
  LV:=system.Length(AValue);
  L:=Length-LV+1;
  if L<0 then
    L:=0;
  I:=StartIndex+1;
  Q:=0;
  if StartQuote=EndQuote then
    begin
    While (Result=-1) and (I<=L) do
      begin
      if (S[I]=StartQuote) then
        Q:=1-Q;
      if (Q=0) and MatchAt(i) then
        Result:=I-1;
      Inc(I);
      end;
    end
  else
    begin
    While (Result=-1) and (I<=L) do
      begin
      if S[I]=StartQuote then
        Inc(Q)
      else if (S[I]=EndQuote) and (Q>0) then
        Dec(Q);
      if (Q=0) and MatchAt(i) then
        Result:=I-1;
      Inc(I);
      end;
    end;
end;

function TStringHelper.IndexOfAny(const AnyOf: string): SizeInt;
begin
  Result:=IndexOfAny(AnyOf.ToCharArray);
end;


function TStringHelper.IndexOfAny(const AnyOf: array of Char): SizeInt;
begin
  Result:=IndexOfAny(AnyOf,0,Length);
end;

function TStringHelper.IndexOfAny(const AnyOf: String; StartIndex: SizeInt): SizeInt;
begin
  Result:=IndexOfAny(AnyOf.ToCharArray,StartIndex);
end;


function TStringHelper.IndexOfAny(const AnyOf: array of Char;
  StartIndex: SizeInt): SizeInt;
begin
  Result:=IndexOfAny(AnyOf,StartIndex,Length);
end;

function TStringHelper.IndexOfAny(const AnyOf: String; StartIndex: SizeInt; ACount: SizeInt): SizeInt;
begin
  Result:=IndexOfAny(AnyOf.ToCharArray,StartIndex,aCount);
end;

function TStringHelper.IndexOfAny(const AnyOf: array of Char;
  StartIndex: SizeInt; ACount: SizeInt): SizeInt;

Var
  i,L : SizeInt;

begin
  I:=StartIndex+1;
  L:=I+ACount-1;
  If L>Length then
    L:=Length;
  Result:=-1;
  While (Result=-1) and (I<=L) do
    begin
    if HaveChar(Self[i],AnyOf) then
      Result:=I-1;
    Inc(I);
    end;
end;

function TStringHelper.IndexOfAny(const AnyOf: array of String): SizeInt;
begin
  Result:=IndexOfAny(AnyOf,0,Length);
end;

function TStringHelper.IndexOfAny(const AnyOf: array of String;
  StartIndex: SizeInt): SizeInt;
begin
  Result:=IndexOfAny(AnyOf,StartIndex,Length-StartIndex);
end;

function TStringHelper.IndexOfAny(const AnyOf: array of String;
  StartIndex: SizeInt; ACount: SizeInt): SizeInt;

Var
  M : SizeInt;

begin
  Result:=IndexOfAny(AnyOf,StartIndex,ACount,M);
end;

function TStringHelper.IndexOfAny(const AnyOf: array of String;
  StartIndex: SizeInt; ACount: SizeInt; out AMatch: SizeInt): SizeInt;

Var
  L,I : SizeInt;

begin
  Result:=-1;
  For I:=0 to System.Length(AnyOf)-1 do
    begin
    L:=IndexOf(AnyOf[i],StartIndex,ACount);
    If (L>=0) and ((Result=-1) or (L<Result)) then
      begin
      Result:=L;
      AMatch:=I;
      end;
    end;
end;


function TStringHelper.IndexOfAnyUnquoted(const AnyOf: array of Char;
  StartQuote, EndQuote: Char): SizeInt;
begin
  Result:=IndexOfAnyUnquoted(AnyOf,StartQuote,EndQuote,0,Length);
end;


function TStringHelper.IndexOfAnyUnquoted(const AnyOf: array of Char;
  StartQuote, EndQuote: Char; StartIndex: SizeInt): SizeInt;
begin
  Result:=IndexOfAnyUnquoted(AnyOf,StartQuote,EndQuote,StartIndex,Length);
end;


function TStringHelper.IndexOfAnyUnquoted(const AnyOf: array of Char;
  StartQuote, EndQuote: Char; StartIndex: SizeInt; ACount: SizeInt): SizeInt;

Var
  I,L : SizeInt;
  Q : SizeInt;

begin
  Result:=-1;
  L:=StartIndex+ACount-1;
  if L>Length then
    L:=Length;
  I:=StartIndex+1;
  Q:=0;
  if StartQuote=EndQuote then
    begin
    While (Result=-1) and (I<=L) do
      begin
      if (Self[I]=StartQuote) then
        Q:=1-Q;
      if (Q=0) and HaveChar(Self[i],AnyOf) then
        Result:=I-1;
      Inc(I);
      end;
    end
  else
  begin
    While (Result=-1) and (I<=L) do
      begin
      if Self[I]=StartQuote then
        Inc(Q)
      else if (Self[I]=EndQuote) and (Q>0) then
        Dec(Q);
      if (Q=0) and HaveChar(Self[i],AnyOf) then
        Result:=I-1;
      Inc(I);
      end;
    end;

end;

function TStringHelper.IndexOfAnyUnquoted(const AnyOf: array of string;
  StartQuote, EndQuote: Char; StartIndex: SizeInt; out Matched: SizeInt
  ): SizeInt;

Var
  L,I : SizeInt;

begin
  Result:=-1;
  For I:=0 to System.Length(AnyOf)-1 do
    begin
    L:=IndexOfUnquoted(AnyOf[i],StartQuote,EndQuote,StartIndex);
    If (L>=0) and ((Result=-1) or (L<Result)) then
      begin
      Result:=L;
      Matched:=I;
      end;
    end;
end;


function TStringHelper.Insert(StartIndex: SizeInt; const AValue: string
  ): string;
begin
  system.Insert(AValue,Self,StartIndex+1);
  Result:=Self;
end;


function TStringHelper.IsDelimiter(const Delimiters: string; Index: SizeInt
  ): Boolean;
begin
  Result:=sysutils.IsDelimiter(Delimiters,Self,Index+1);
end;


function TStringHelper.IsEmpty: Boolean;
begin
  Result:=(Length=0)
end;


function TStringHelper.LastDelimiter(const Delims: string): SizeInt;
begin
  Result:=sysutils.LastDelimiter(Delims,Self)-1;
end;


function TStringHelper.LastIndexOf(AValue: Char): SizeInt;
begin
  Result:=LastIndexOf(AValue,Length-1,Length);
end;


function TStringHelper.LastIndexOf(const AValue: string): SizeInt;
begin
  Result:=LastIndexOf(AValue,Length-1,Length);
end;


function TStringHelper.LastIndexOf(AValue: Char; AStartIndex: SizeInt): SizeInt;
begin
  Result:=LastIndexOf(AValue,AStartIndex,Length);
end;


function TStringHelper.LastIndexOf(const AValue: string; AStartIndex: SizeInt
  ): SizeInt;
begin
  Result:=LastIndexOf(AValue,AStartIndex,Length);
end;


function TStringHelper.LastIndexOf(AValue: Char; AStartIndex: SizeInt;
  ACount: SizeInt): SizeInt;

Var
  Min : SizeInt;

begin
  Result:=AStartIndex+1;
  Min:=Result-ACount+1;
  If Min<1 then
    Min:=1;
  While (Result>=Min) and (Self[Result]<>AValue) do
    Dec(Result);
  if Result<Min then
    Result:=-1
  else
    Result:=Result-1;
end;


function TStringHelper.LastIndexOf(const AValue: string; AStartIndex: SizeInt; ACount: SizeInt): SizeInt;

begin
  Result:=TJSString(Self).lastIndexOf(aValue,aStartIndex);
  if (aStartIndex-Result)>aCount then
    Result:=-1;
end;


function TStringHelper.LastIndexOfAny(const AnyOf: array of Char): SizeInt;
begin
  Result:=LastIndexOfAny(AnyOf,Length-1,Length);
end;


function TStringHelper.LastIndexOfAny(const AnyOf: array of Char;
  AStartIndex: SizeInt): SizeInt;
begin
  Result:=LastIndexOfAny(AnyOf,AStartIndex,Length);
end;


function TStringHelper.LastIndexOfAny(const AnyOf: array of Char;
  AStartIndex: SizeInt; ACount: SizeInt): SizeInt;

Var
  Min : SizeInt;

begin
  Result:=AStartIndex+1;
  Min:=Result-ACount+1;
  If Min<1 then
    Min:=1;
  While (Result>=Min) and Not HaveChar(Self[Result],AnyOf) do
    Dec(Result);
  if Result<Min then
    Result:=-1
  else
    Result:=Result-1;
end;


function TStringHelper.PadLeft(ATotalWidth: SizeInt): string;
begin
  Result:=PadLeft(ATotalWidth,' ');
end;


function TStringHelper.PadLeft(ATotalWidth: SizeInt; PaddingChar: Char): string;
Var
  L : SizeInt;

begin
  Result:=Self;
  L:=ATotalWidth-Length;
  If L>0 then
    Result:=StringOfChar(PaddingChar,L)+Result;
end;


function TStringHelper.PadRight(ATotalWidth: SizeInt): string;
begin
  Result:=PadRight(ATotalWidth,' ');
end;


function TStringHelper.PadRight(ATotalWidth: SizeInt; PaddingChar: Char
  ): string;

Var
  L : SizeInt;

begin
  Result:=Self;
  L:=ATotalWidth-Length;
  If L>0 then
    Result:=Result+StringOfChar(PaddingChar,L);
end;


function TStringHelper.QuotedString: string;
begin
  Result:=QuotedStr(Self);
end;


function TStringHelper.QuotedString(const AQuoteChar: Char): string;
begin
  Result:=QuotedStr(Self,AQuoteChar);
end;


function TStringHelper.Remove(StartIndex: SizeInt): string;
begin
  Result:=Remove(StartIndex,Self.Length-StartIndex);
end;


function TStringHelper.Remove(StartIndex: SizeInt; ACount: SizeInt): string;
begin
  Result:=Self;
  System.Delete(Result,StartIndex+1,ACount);
end;


function TStringHelper.Replace(OldChar: Char; NewChar: Char): string;
begin
  Result:=Replace(OldChar,NewChar,[rfReplaceAll]);
end;


function TStringHelper.Replace(OldChar: Char; NewChar: Char;
  ReplaceFlags: TReplaceFlags): string;
begin
  Result:=StringReplace(Self,OldChar,NewChar,ReplaceFlags);
end;


function TStringHelper.Replace(const OldValue: string; const NewValue: string
  ): string;
begin
  Result:=Replace(OldValue,NewValue,[rfReplaceAll]);
end;


function TStringHelper.Replace(const OldValue: string; const NewValue: string;
  ReplaceFlags: TReplaceFlags): string;
begin
  Result:=StringReplace(Self,OldValue,NewValue,ReplaceFlags);
end;



function TStringHelper.Split(const Separators: String): TStringArray;
begin
  Result:=Split(Separators.ToCharArray);
end;


function TStringHelper.Split(const Separators: array of Char): TStringArray;
begin
  Result:=Split(Separators,#0,#0,Length+1,TStringSplitOptions.None);
end;

function TStringHelper.Split(const Separators: string; ACount: SizeInt): TStringArray;
begin
  Result:=Split(Separators.ToCharArray,aCount);
end;


function TStringHelper.Split(const Separators: array of Char; ACount: SizeInt
  ): TStringArray;
begin
  Result:=Split(Separators,#0,#0,ACount,TStringSplitOptions.None);
end;

function TStringHelper.Split(const Separators: string; Options: TStringSplitOptions): TStringArray;
begin
  Result:=Split(Separators.ToCharArray,Options);
end;


function TStringHelper.Split(const Separators: array of Char;
  Options: TStringSplitOptions): TStringArray;
begin
  Result:=Split(Separators,Length+1,Options);
end;

function TStringHelper.Split(const Separators: string; ACount: SizeInt; Options: TStringSplitOptions): TStringArray;
begin
  Result:=Split(Separators.ToCharArray,aCount,Options);
end;


function TStringHelper.Split(const Separators: array of Char; ACount: SizeInt;
  Options: TStringSplitOptions): TStringArray;
begin
  Result:=Split(Separators,#0,#0,ACount,Options);
end;


function TStringHelper.Split(const Separators: array of string): TStringArray;
begin
  Result:=Split(Separators,Length+1);
end;


function TStringHelper.Split(const Separators: array of string; ACount: SizeInt
  ): TStringArray;
begin
  Result:=Split(Separators,ACount,TStringSplitOptions.None);
end;


function TStringHelper.Split(const Separators: array of string;
  Options: TStringSplitOptions): TStringArray;
begin
  Result:=Split(Separators,Length+1,Options);
end;


function TStringHelper.Split(const Separators: array of string;
  ACount: SizeInt; Options: TStringSplitOptions): TStringArray;
begin
  Result:=Split(Separators,#0,#0,ACount,Options);
end;

function TStringHelper.Split(const Separators: String; AQuote: Char): TStringArray;
begin
  Result:=Split(Separators.ToCharArray,aQuote);
end;


function TStringHelper.Split(const Separators: array of Char; AQuote: Char
  ): TStringArray;
begin
  Result:=Split(Separators,AQuote,AQuote);
end;

function TStringHelper.Split(const Separators: String; AQuoteStart, AQuoteEnd: Char): TStringArray;
begin
  Result:=Split(Separators.ToCharArray,aQuoteStart,aQuoteEnd);
end;


function TStringHelper.Split(const Separators: array of Char; AQuoteStart,
  AQuoteEnd: Char): TStringArray;
begin
  Result:=Split(Separators,AQuoteStart,AQuoteEnd,TStringSplitOptions.None);
end;

function TStringHelper.Split(const Separators: string; AQuoteStart, AQuoteEnd: Char; Options: TStringSplitOptions): TStringArray;
begin
  Result:=Split(Separators.ToCharArray,aQuoteStart,aQuoteEnd,Options);
end;


function TStringHelper.Split(const Separators: array of Char; AQuoteStart,
  AQuoteEnd: Char; Options: TStringSplitOptions): TStringArray;
begin
  Result:=Split(Separators,AQuoteStart,AQuoteEnd,Length+1,Options);
end;

function TStringHelper.Split(const Separators: string; AQuoteStart, AQuoteEnd: Char; ACount: SizeInt): TStringArray;
begin
  Result:=Split(Separators.ToCharArray,aQuoteStart,aQuoteEnd,aCount);
end;


function TStringHelper.Split(const Separators: array of Char; AQuoteStart,
  AQuoteEnd: Char; ACount: SizeInt): TStringArray;
begin
  Result:=Split(Separators,AQuoteStart,AQuoteEnd,ACount,TStringSplitOptions.None);
end;

function TStringHelper.Split(const Separators: string; AQuoteStart, AQuoteEnd: Char; ACount: SizeInt; Options: TStringSplitOptions
  ): TStringArray;
begin
  Result:=Split(Separators.ToCharArray,aQuoteStart,aQuoteEnd,aCount,Options);
end;


function TStringHelper.Split(const Separators: array of Char; AQuoteStart,
  AQuoteEnd: Char; ACount: SizeInt; Options: TStringSplitOptions): TStringArray;


Const
  BlockSize = 10;

Var
  S : String;

  Function NextSep(StartIndex : SizeInt) : SizeInt;

  begin
    if (AQuoteStart<>#0) then
      Result:=S.IndexOfAnyUnQuoted(Separators,AQuoteStart,AQuoteEnd,StartIndex)
    else
      Result:=S.IndexOfAny(Separators,StartIndex);
  end;

  Procedure MaybeGrow(Curlen : SizeInt);

  begin
    if System.Length(Result)<=CurLen then
      SetLength(Result,System.Length(Result)+BlockSize);
  end;

Var
  Sep,LastSep,Len : SizeInt;
  T : String;

begin

  S:=Self;
  SetLength(Result,BlockSize);
  Len:=0;
  LastSep:=0;
  Sep:=NextSep(0);
  While (Sep<>-1) and ((ACount=0) or (Len<ACount)) do
    begin
    T:=SubString(LastSep,Sep-LastSep);
//    Writeln('Examining >',T,'< at pos ',LastSep,', till pos ',Sep);
    If (T<>'') or (not (TStringSplitOptions.ExcludeEmpty=Options)) then
      begin
      MaybeGrow(Len);
      Result[Len]:=T;
      Inc(Len);
      end;
    LastSep:=Sep+1;
    Sep:=NextSep(LastSep);
    end;
  if (LastSep<=Length) and ((ACount=0) or (Len<ACount)) then
    begin
    T:=SubString(LastSep);
//    Writeln('Examining >',T,'< at pos,',LastSep,' till pos ',Sep);
    If (T<>'') or (not (TStringSplitOptions.ExcludeEmpty=Options)) then
      begin
      MaybeGrow(Len);
      Result[Len]:=T;
      Inc(Len);
      end;
    end;
  SetLength(Result,Len);

end;


function TStringHelper.Split(const Separators: array of string; AQuote: Char
  ): TStringArray;
begin
  Result:=SPlit(Separators,AQuote,AQuote);
end;


function TStringHelper.Split(const Separators: array of string; AQuoteStart,
  AQuoteEnd: Char): TStringArray;
begin
  Result:=SPlit(Separators,AQuoteStart,AQuoteEnd,Length+1,TStringSplitOptions.None);
end;


function TStringHelper.Split(const Separators: array of string; AQuoteStart,
  AQuoteEnd: Char; Options: TStringSplitOptions): TStringArray;
begin
  Result:=SPlit(Separators,AQuoteStart,AQuoteEnd,Length+1,Options);
end;


function TStringHelper.Split(const Separators: array of string; AQuoteStart,
  AQuoteEnd: Char; ACount: SizeInt): TStringArray;
begin
  Result:=SPlit(Separators,AQuoteStart,AQuoteEnd,ACount,TStringSplitOptions.None);
end;


function TStringHelper.Split(const Separators: array of string; AQuoteStart,
  AQuoteEnd: Char; ACount: SizeInt; Options: TStringSplitOptions): TStringArray;

Const
  BlockSize = 10;

Var
  S : String;

  Function NextSep(StartIndex : SizeInt; out Match : SizeInt) : SizeInt;

  begin
    if (AQuoteStart<>#0) then
      Result:=S.IndexOfAnyUnQuoted(Separators,AQuoteStart,AQuoteEnd,StartIndex,Match)
    else
      Result:=S.IndexOfAny(Separators,StartIndex,Length,Match);
  end;

  Procedure MaybeGrow(Curlen : SizeInt);

  begin
    if System.Length(Result)<=CurLen then
      SetLength(Result,System.Length(Result)+BlockSize);
  end;

Var
  Sep,LastSep,Len,Match : SizeInt;
  T : String;

begin
  S:=Self;
  SetLength(Result,BlockSize);
  Len:=0;
  LastSep:=0;
  Sep:=NextSep(0,Match);
  While (Sep<>-1) and ((ACount=0) or (Len<ACount)) do
    begin
    T:=SubString(LastSep,Sep-LastSep);
    If (T<>'') or (not (TStringSplitOptions.ExcludeEmpty=Options)) then
      begin
      MaybeGrow(Len);
      Result[Len]:=T;
      Inc(Len);
      end;
    LastSep:=Sep+System.Length(Separators[Match]);
    Sep:=NextSep(LastSep,Match);
    end;
  if (LastSep<=Length) and ((ACount=0) or (Len<ACount)) then
    begin
    T:=SubString(LastSep);
//    Writeln('Examining >',T,'< at pos,',LastSep,' till pos ',Sep);
    If (T<>'') or (not (TStringSplitOptions.ExcludeEmpty=Options)) then
      begin
      MaybeGrow(Len);
      Result[Len]:=T;
      Inc(Len);
      end;
    end;
  SetLength(Result,Len);

end;


function TStringHelper.StartsWith(const AValue: string): Boolean;
begin
  Result:=StartsWith(AValue,False);
end;


function TStringHelper.StartsWith(const AValue: string; IgnoreCase: Boolean
  ): Boolean;
Var
  L : SizeInt;
  S : String;

begin
  L:=System.Length(AValue);
  Result:=L<=0;
  if not Result then
    begin
    S:=System.Copy(Self,1,L);
    Result:=(System.Length(S)=L);
    if Result then
      if IgnoreCase then
        Result:=SameText(S,aValue)
      else
        Result:=SameStr(S,AValue);
    end;
end;


function TStringHelper.Substring(AStartIndex: SizeInt): string;
begin
  Result:=Self.SubString(AStartIndex,Self.Length-AStartIndex);
end;


function TStringHelper.Substring(AStartIndex: SizeInt; ALen: SizeInt): string;
begin
  Result:=system.Copy(Self,AStartIndex+1,ALen);
end;


function TStringHelper.ToBoolean: Boolean;
begin
  Result:=StrToBool(Self);
end;


function TStringHelper.ToInteger: Integer;
begin
  Result:=StrToInt(Self);
end;


function TStringHelper.ToNativeInt: NativeInt;
begin
  Result:=StrToNativeInt(Self);
end;

function TStringHelper.ToDouble: Double;
begin
  Result:=StrToFLoat(Self);
end;


function TStringHelper.ToExtended: Extended;
begin
  Result:=StrToFLoat(Self);
end;


function TStringHelper.ToCharArray: TCharArray;

begin
  Result:=ToCharArray(0,Self.Length);
end;


function TStringHelper.ToCharArray(AStartIndex: SizeInt; ALen: SizeInt
  ): TCharArray;

Var
  I : SizeInt;

begin
  SetLength(Result,ALen);
  For I:=0 to ALen-1 do
    Result[I]:=Self[AStartIndex+I+1];
end;


function TStringHelper.ToLower: string;
begin
  Result:=LowerCase(Self);
end;


function TStringHelper.ToLowerInvariant: string;
begin
  Result:=LowerCase(Self);
end;


function TStringHelper.ToUpper: string;
begin
  Result:=UpperCase(Self);
end;


function TStringHelper.ToUpperInvariant: string;
begin
  Result:=UpperCase(Self);
end;


function TStringHelper.Trim: string;
begin
  Result:=SysUtils.Trim(self);
end;


function TStringHelper.TrimLeft: string;
begin
  Result:=SysUtils.TrimLeft(Self);
end;


function TStringHelper.TrimRight: string;
begin
  Result:=SysUtils.TrimRight(Self);
end;


function TStringHelper.Trim(const ATrimChars: array of Char): string;
begin
  Result:=Self.TrimLeft(ATrimChars).TrimRight(ATrimChars);
end;


function TStringHelper.TrimLeft(const ATrimChars: array of Char): string;

Var
  I,Len : SizeInt;

begin
  I:=1;
  Len:=Self.Length;
  While (I<=Len) and HaveChar(Self[i],ATrimChars) do Inc(I);
  if I=1 then
    Result:=Self
  else if I>Len then
    Result:=''
  else
    Result:=system.Copy(Self,I,Len-I+1);
end;


function TStringHelper.TrimRight(const ATrimChars: array of Char): string;

Var
  I,Len : SizeInt;

begin
  Len:=Self.Length;
  I:=Len;
  While (I>=1) and HaveChar(Self[i],ATrimChars) do Dec(I);
  if I<1 then
    Result:=''
  else if I=Len then
    Result:=Self
  else
    Result:=system.Copy(Self,1,I);
end;


function TStringHelper.TrimEnd(const ATrimChars: array of Char): string;
begin
  Result:=TrimRight(ATrimChars);
end;


function TStringHelper.TrimStart(const ATrimChars: array of Char): string;
begin
  Result:=TrimLeft(ATrimChars);
end;

{ ---------------------------------------------------------------------
  TDoubleHelper
  ---------------------------------------------------------------------}

Class Function TDoubleHelper.IsNan(const AValue: Double): Boolean; overload; inline;

begin
  Result:=JS.jsIsNaN(AValue);
end;

Class Function TDoubleHelper.IsInfinity(const AValue: Double): Boolean; overload; inline;

begin
  Result:=Not jsIsFinite(aValue);
end;

Class Function TDoubleHelper.IsNegativeInfinity(const AValue: Double): Boolean; overload; inline;

begin
  asm
    return (AValue=Number.NEGATIVE_INFINITY);
  end;
  Result:=aValue=0; // Fool compiler
end;

Class Function TDoubleHelper.IsPositiveInfinity(const AValue: Double): Boolean; overload; inline;

begin
  asm
    return (AValue=Number.POSITIVE_INFINITY);
  end;
  Result:=aValue=0;  // Fool compiler.
end;

Class Function TDoubleHelper.Parse(const AString: string): Double; overload; inline;

begin
  Result:=StrToFloat(AString);
end;


Class Function TDoubleHelper.ToString(const AValue: Double): string; overload; inline;

begin
  Result:=FloatToStr(AValue);
end;


Class Function TDoubleHelper.ToString(const AValue: Double; const AFormat: TFloatFormat; const APrecision, ADigits: Integer): string; overload; inline;

begin
  Result:=FloatToStrF(AValue,AFormat,APrecision,ADigits);
end;


Class Function TDoubleHelper.TryParse(const AString: string; out AValue: Double): Boolean; overload; inline;

begin
  Result:=TryStrToFloat(AString,AValue);
end;



Function TDoubleHelper.GetB(AIndex: Cardinal): Byte;

var
  F : TJSFloat64Array;
  B : TJSUInt8array;

begin
  F:=TJSFloat64Array.New(1);
  B:=TJSUInt8array.New(F.Buffer);
  F[0]:=Self;
  Result:=B[AIndex];
end;

Function TDoubleHelper.GetW(AIndex: Cardinal): Word;

var
  F : TJSFloat64Array;
  W : TJSUInt16array;

begin
  F:=TJSFloat64Array.New(1);
  W:=TJSUInt16array.New(F.Buffer);
  F[0]:=Self;
  Result:=W[AIndex];
end;

Type
  TFloatParts = Record
    sign : boolean;
    exp : integer;
    mantissa : double;
  end;

// See https://stackoverflow.com/questions/9383593/extracting-the-exponent-and-mantissa-of-a-javascript-number

Function FloatToParts(aValue : Double) : TFloatParts;

var
  F : TJSFloat64Array;
  B : TJSUInt8array;

begin
  F:=TJSFloat64Array.New(1);
  B:=TJSUInt8array.New(F.Buffer);
  F[0]:=aValue;
  Result.Sign:=(B[7] shr 7)=0;
  Result.exp:=(((B[7] and $7f) shl 4) or (B[6] shr 4))- $3ff;
  B[3]:=$3F;
  B[6]:=B[6] or $F0;
  Result.Mantissa:=F[0];
end;

Function TDoubleHelper.GetE: NativeUInt; inline;

begin
  Result:=FloatToParts(Self).Exp;
end;

Function TDoubleHelper.GetF: NativeUInt; inline;

begin
  Result:=0;
  NotImplemented('GetF');
end;

Function TDoubleHelper.GetS: Boolean; inline;

begin
  Result:=FloatToParts(Self).Sign;
end;

procedure TDoubleHelper.SetB(AIndex: Cardinal; const AValue: Byte);

var
  F : TJSFloat64Array;
  B : TJSUInt8array;

begin
  if (AIndex>=8) then
    raise ERangeError.Create(SRangeError);
  F:=TJSFloat64Array.New(1);
  B:=TJSUInt8array.New(F.Buffer);
  F[0]:=Self;
  B[AIndex]:=aValue;
  Self:=F[0];
end;

procedure TDoubleHelper.SetW(AIndex: Cardinal; const AValue: Word);

Var
  F : TJSFloat64Array;
  W : TJSUInt16array;

begin
  if (AIndex>=4) then
    raise ERangeError.Create(SRangeError);
  F:=TJSFloat64Array.New(1);
  W:=TJSUInt16array.New(F.Buffer);
  F[0]:=Self;
  W[AIndex]:=aValue;
  Self:=F[0];
end;



procedure TDoubleHelper.SetS(AValue: Boolean);


Var
  F : TJSFloat64Array;
  B : TJSUInt8array;

begin
  F:=TJSFloat64Array.New(1);
  B:=TJSUInt8array.New(F.Buffer);
  F[0]:=Self;
  if aValue then
    B[7]:=B[7] or (1 shr 7)
  else
    B[7]:=B[7] and not (1 shr 7);
  Self:=F[0];
end;


Procedure TDoubleHelper.BuildUp(const ASignFlag: Boolean; const AMantissa: NativeUInt; const AExponent: Integer);

begin
  NotImplemented('BuildUp');
  // Following statement is just to fool the compiler
  if ASignFlag and (AMantissa>0) and (AExponent<0) then exit;
//  TFloatRec(Self).BuildUp(ASignFlag, AMantissa, AExponent);
end;

Function TDoubleHelper.Exponent: Integer;

begin
  Result:=FloatToParts(Self).Exp;
end;

Function TDoubleHelper.Fraction: Extended;

begin
  Result:=system.Frac(Self);
end;

Function TDoubleHelper.IsInfinity: Boolean; overload; inline;

begin
  Result:=Double.IsInfinity(Self);
end;

Function TDoubleHelper.IsNan: Boolean; overload; inline;

begin
  Result:=Double.IsNan(Self);
end;

Function TDoubleHelper.IsNegativeInfinity: Boolean; overload; inline;

begin
  Result:=Double.IsNegativeInfinity(Self);
end;

Function TDoubleHelper.IsPositiveInfinity: Boolean; overload; inline;

begin
  Result:=Double.IsPositiveInfinity(Self);
end;

Function TDoubleHelper.Mantissa: NativeUInt;

begin
  Result:=Trunc(FloatToParts(Self).mantissa);
end;

Function TDoubleHelper.ToString(const AFormat: TFloatFormat; const APrecision, ADigits: Integer): string; overload; inline;

begin
  Result:=FloatToStrF(Self,AFormat,APrecision,ADigits);
end;

Function TDoubleHelper.ToString: string; overload; inline;

begin
  Result:=FloatToStr(Self);
end;

{ ---------------------------------------------------------------------
  TByteHelper
  ---------------------------------------------------------------------}

Class Function TByteHelper.Parse(const AString: string): Byte; inline;

begin
  Result:=StrToInt(AString);
end;

Class Function TByteHelper.Size: Integer; inline;

begin
  Result:=1;
end;

Class Function TByteHelper.ToString(const AValue: Byte): string; overload; inline;

begin
  Result:=IntToStr(AValue);
end;

Class Function TByteHelper.TryParse(const AString: string; out AValue: Byte): Boolean; inline;

Var
  C : Integer;

begin
  Val(AString,AValue,C);
  Result:=(C=0);
end;

Function TByteHelper.ToBoolean: Boolean; inline;

begin
  Result:=(Self<>0);
end;

Function TByteHelper.ToDouble: Double; inline;

begin
  Result:=Self;
end;

Function TByteHelper.ToExtended: Extended; inline;

begin
  Result:=Self;
end;

Function TByteHelper.ToBinString: string; inline;

begin
  Result:=BinStr(Self,Size*8);
end;

Function TByteHelper.ToHexString(const AMinDigits: Integer): string;
overload; inline;

begin
  Result:=IntToHex(Self,AMinDigits);
end;

Function TByteHelper.ToHexString: string; overload; inline;

begin
  Result:=IntToHex(Self,Size*2);
end;

Function TByteHelper.ToString: string; overload; inline;

begin
  Result:=IntToStr(Self);
end;

Function TByteHelper.SetBit(const index: TByteBitIndex) : Byte; inline;

begin
  Self := Self or (Byte(1) shl index);
  Result:=Self;
end;

Function TByteHelper.ClearBit(const index: TByteBitIndex) : Byte; inline;

begin
  Self:=Self and not Byte((Byte(1) shl index));
  Result:=Self;
end;

Function TByteHelper.ToggleBit(const index: TByteBitIndex) : Byte; inline;

begin
  Self := Self xor Byte((Byte(1) shl index));
  Result:=Self;
end;

Function TByteHelper.TestBit(const Index: TByteBitIndex):Boolean; inline;

begin
  Result := (Self and Byte((Byte(1) shl index)))<>0;
end;

{ ---------------------------------------------------------------------
  TShortintHelper
  ---------------------------------------------------------------------}

Class Function TShortIntHelper.Parse(const AString: string): ShortInt; inline;

begin
  Result:=StrToInt(AString);
end;

Class Function TShortIntHelper.Size: Integer; inline;

begin
  Result:=1;
end;

Class Function TShortIntHelper.ToString(const AValue: ShortInt): string; overload; inline;

begin
  Result:=IntToStr(AValue);
end;

Class Function TShortIntHelper.TryParse(const AString: string; out AValue: ShortInt): Boolean; inline;

Var
  C : Integer;

begin
  Val(AString,AValue,C);
  Result:=(C=0);
end;

Function TShortIntHelper.ToBoolean: Boolean; inline;

begin
  Result:=(Self<>0);
end;

Function TShortIntHelper.ToDouble: Double; inline;

begin
  Result:=Self;
end;

Function TShortIntHelper.ToExtended: Extended; inline;

begin
  Result:=Self;
end;

Function TShortIntHelper.ToBinString: string; inline;

begin
  Result:=BinStr(Self,Size*8);
end;

Function TShortIntHelper.ToHexString(const AMinDigits: Integer): string; overload; inline;

Var
  B : Word;
  U : TJSUInt8Array;
  S : TJSInt8array;

begin
  if Self>=0 then
    B:=Self
  else
    begin
    S:=TJSInt8Array.New(1);
    S[0]:=Self;
    U:=TJSUInt8Array.New(S);
    B:=U[0];
    if AMinDigits>2 then
      B:=$FF00+B;
    end;
  Result:=IntToHex(B,AMinDigits);
end;

Function TShortIntHelper.ToHexString: string; overload; inline;

begin
  Result:=ToHexString(Size*2);
end;

Function TShortIntHelper.ToString: string; overload; inline;

begin
  Result:=IntToStr(Self);
end;

Function TShortIntHelper.SetBit(const index: TShortIntBitIndex) : ShortInt; inline;

begin
  Self := Self or (ShortInt(1) shl index);
  Result:=Self;
end;

Function TShortIntHelper.ClearBit(const index: TShortIntBitIndex) : ShortInt; inline;

begin
  Self:=Self and not ShortInt((ShortInt(1) shl index));
  Result:=Self;
end;

Function TShortIntHelper.ToggleBit(const index: TShortIntBitIndex) : ShortInt; inline;

begin
  Self := Self xor ShortInt((ShortInt(1) shl index));
  Result:=Self;
end;

Function TShortIntHelper.TestBit(const Index: TShortIntBitIndex):Boolean; inline;

begin
  Result := (Self and ShortInt((ShortInt(1) shl index)))<>0;
end;

{ ---------------------------------------------------------------------
  TSmallintHelper
  ---------------------------------------------------------------------}

Class Function TSmallIntHelper.Parse(const AString: string): SmallInt; inline;

begin
  Result:=StrToInt(AString);
end;

Class Function TSmallIntHelper.Size: Integer; inline;

begin
  Result:=2;
end;

Class Function TSmallIntHelper.ToString(const AValue: SmallInt): string; overload; inline;

begin
  Result:=IntToStr(AValue);
end;

Class Function TSmallIntHelper.TryParse(const AString: string; out AValue: SmallInt): Boolean; inline;

Var
  C : Integer;

begin
  Val(AString,AValue,C);
  Result:=(C=0);
end;

Function TSmallIntHelper.ToBoolean: Boolean; inline;

begin
  Result:=(Self<>0);
end;

Function TSmallIntHelper.ToDouble: Double; inline;

begin
  Result:=Self;
end;

Function TSmallIntHelper.ToExtended: Extended; inline;

begin
  Result:=Self;
end;

Function TSmallIntHelper.ToBinString: string; inline;

begin
  Result:=BinStr(Self,Size*8);
end;

Function TSmallIntHelper.ToHexString(const AMinDigits: Integer): string; overload; inline;

Var
  B : Cardinal;
  U : TJSUInt16Array;
  S : TJSInt16array;


begin
  if Self>=0 then
    B:=Self
  else
    begin
    S:=TJSInt16Array.New(1);
    S[0]:=Self;
    U:=TJSUInt16Array.New(S);
    B:=U[0];
    if AMinDigits>6 then
      B:=$FFFF0000+B
    else if AMinDigits>4 then
      B:=$FF0000+B;
    end;
  Result:=IntToHex(B,AMinDigits);
end;

Function TSmallIntHelper.ToHexString: string; overload; inline;

begin
  Result:=ToHexString(Size*2);
end;


Function TSmallIntHelper.ToString: string; overload; inline;

begin
  Result:=IntToStr(Self);
end;

Function TSmallIntHelper.SetBit(const index: TSmallIntBitIndex) : SmallInt; inline;

begin
  Self := Self or (SmallInt(1) shl index);
  Result:=Self;
end;

Function TSmallIntHelper.ClearBit(const index: TSmallIntBitIndex) : SmallInt; inline;

begin
  Self:=Self and not SmallInt((SmallInt(1) shl index));
  Result:=Self;
end;

Function TSmallIntHelper.ToggleBit(const index: TSmallIntBitIndex) : SmallInt; inline;

begin
  Self := Self xor SmallInt((SmallInt(1) shl index));
  Result:=Self;
end;

Function TSmallIntHelper.TestBit(const Index: TSmallIntBitIndex):Boolean; inline;

begin
  Result := (Self and SmallInt((SmallInt(1) shl index)))<>0;
end;

{ ---------------------------------------------------------------------
  TWordHelper
  ---------------------------------------------------------------------}

Class Function TWordHelper.Parse(const AString: string): Word; inline;

begin
  Result:=StrToInt(AString);
end;

Class Function TWordHelper.Size: Integer; inline;

begin
  Result:=2;
end;

Class Function TWordHelper.ToString(const AValue: Word): string; overload; inline;

begin
  Result:=IntToStr(AValue);
end;

Class Function TWordHelper.TryParse(const AString: string; out AValue: Word): Boolean; inline;

Var
  C : Integer;

begin
  Val(AString,AValue,C);
  Result:=(C=0);
end;

Function TWordHelper.ToBoolean: Boolean; inline;

begin
  Result:=(Self<>0);
end;

Function TWordHelper.ToDouble: Double; inline;

begin
  Result:=Self;
end;

Function TWordHelper.ToExtended: Extended; inline;

begin
  Result:=Self;
end;

Function TWordHelper.ToBinString: string; inline;

begin
  Result:=BinStr(Self,Size*8);
end;

Function TWordHelper.ToHexString(const AMinDigits: Integer): string;
overload; inline;

begin
  Result:=IntToHex(Self,AMinDigits);
end;

Function TWordHelper.ToHexString: string; overload; inline;

begin
  Result:=IntToHex(Self,Size*2);
end;


Function TWordHelper.ToString: string; overload; inline;

begin
  Result:=IntToStr(Self);
end;

Function TWordHelper.SetBit(const index: TWordBitIndex) : Word; inline;

begin
  Self := Self or (Word(1) shl index);
  Result:=Self;
end;

Function TWordHelper.ClearBit(const index: TWordBitIndex) : Word; inline;

begin
  Self:=Self and not Word((Word(1) shl index));
  Result:=Self;
end;

Function TWordHelper.ToggleBit(const index: TWordBitIndex) : Word; inline;

begin
  Self := Self xor Word((Word(1) shl index));
  Result:=Self;
end;

Function TWordHelper.TestBit(const Index: TWordBitIndex):Boolean; inline;

begin
  Result := (Self and Word((Word(1) shl index)))<>0;
end;

{ ---------------------------------------------------------------------
  TCardinalHelper
  ---------------------------------------------------------------------}

Class Function TCardinalHelper.Parse(const AString: string): Cardinal; inline;

begin
  Result:=StrToInt(AString);
end;

Class Function TCardinalHelper.Size: Integer; inline;

begin
  Result:=4;
end;

Class Function TCardinalHelper.ToString(const AValue: Cardinal): string; overload; inline;

begin
  Result:=IntToStr(AValue);
end;

Class Function TCardinalHelper.TryParse(const AString: string; out AValue: Cardinal): Boolean; inline;

Var
  C : Integer;

begin
  Val(AString,AValue,C);
  Result:=(C=0);
end;

Function TCardinalHelper.ToBoolean: Boolean; inline;

begin
  Result:=(Self<>0);
end;

Function TCardinalHelper.ToDouble: Double; inline;

begin
  Result:=Self;
end;

Function TCardinalHelper.ToExtended: Extended; inline;

begin
  Result:=Self;
end;

Function TCardinalHelper.ToBinString: string; inline;

begin
  Result:=BinStr(Self,Size*8);
end;

Function TCardinalHelper.ToHexString(const AMinDigits: Integer): string;
overload; inline;

begin
  Result:=IntToHex(Self,AMinDigits);
end;

Function TCardinalHelper.ToHexString: string; overload; inline;

begin
  Result:=ToHexString(Size*2);
end;

Function TCardinalHelper.ToString: string; overload; inline;

begin
  Result:=IntToStr(Self);
end;

Function TCardinalHelper.SetBit(const index: TCardinalBitIndex) : Cardinal; inline;

begin
  Self := Self or (Cardinal(1) shl index);
  Result:=Self;
end;

Function TCardinalHelper.ClearBit(const index: TCardinalBitIndex) : Cardinal; inline;

begin
  Self:=Self and not Cardinal((Cardinal(1) shl index));
  Result:=Self;
end;

Function TCardinalHelper.ToggleBit(const index: TCardinalBitIndex) : Cardinal; inline;

begin
  Self := Self xor Cardinal((Cardinal(1) shl index));
  Result:=Self;
end;

Function TCardinalHelper.TestBit(const Index: TCardinalBitIndex):Boolean; inline;

begin
  Result := (Self and Cardinal((Cardinal(1) shl index)))<>0;
end;


{ ---------------------------------------------------------------------
  TIntegerHelper
  ---------------------------------------------------------------------}

Class Function TIntegerHelper.Parse(const AString: string): Integer; inline;

begin
  Result:=StrToInt(AString);
end;

Class Function TIntegerHelper.Size: Integer; inline;

begin
  Result:=4;
end;

Class Function TIntegerHelper.ToString(const AValue: Integer): string; overload; inline;

begin
  Result:=IntToStr(AValue);
end;

Class Function TIntegerHelper.TryParse(const AString: string; out AValue: Integer): Boolean; inline;

Var
  C : Integer;

begin
  Val(AString,AValue,C);
  Result:=(C=0);
end;

Function TIntegerHelper.ToBoolean: Boolean; inline;

begin
  Result:=(Self<>0);
end;

Function TIntegerHelper.ToDouble: Double; inline;

begin
  Result:=Self;
end;

Function TIntegerHelper.ToExtended: Extended; inline;

begin
  Result:=Self;
end;

Function TIntegerHelper.ToBinString: string; inline;

begin
  Result:=BinStr(Self,Size*8);
end;

Function TIntegerHelper.ToHexString(const AMinDigits: Integer): string;
overload; inline;


Var
  B : Word;
  U : TJSUInt32Array;
  S : TJSInt32array;

begin
  if Self>=0 then
    B:=Self
  else
    begin
    S:=TJSInt32Array.New(1);
    S[0]:=Self;
    U:=TJSUInt32Array.New(S);
    B:=U[0];
    end;
  Result:=IntToHex(B,AMinDigits);
end;

Function TIntegerHelper.ToHexString: string; overload; inline;

begin
  Result:=ToHexString(Size*2);
end;


Function TIntegerHelper.ToString: string; overload; inline;

begin
  Result:=IntToStr(Self);
end;

Function TIntegerHelper.SetBit(const index: TIntegerBitIndex) : Integer; inline;

begin
  Self := Self or (Integer(1) shl index);
  Result:=Self;
end;

Function TIntegerHelper.ClearBit(const index: TIntegerBitIndex) : Integer; inline;

begin
  Self:=Self and not Integer((Integer(1) shl index));
  Result:=Self;
end;

Function TIntegerHelper.ToggleBit(const index: TIntegerBitIndex) : Integer; inline;

begin
  Self := Self xor Integer((Integer(1) shl index));
  Result:=Self;
end;

Function TIntegerHelper.TestBit(const Index: TIntegerBitIndex):Boolean; inline;

begin
  Result := (Self and Integer((Integer(1) shl index)))<>0;
end;


{ ---------------------------------------------------------------------
  TNativeIntHelper
  ---------------------------------------------------------------------}

Class Function TNativeIntHelper.Parse(const AString: string): NativeInt; inline;

begin
  Result:=StrToInt(AString);
end;

Class Function TNativeIntHelper.Size: Integer; inline;

begin
  Result:=7;
end;

Class Function TNativeIntHelper.ToString(const AValue: NativeInt): string; overload; inline;

begin
  Result:=IntToStr(AValue);
end;

Class Function TNativeIntHelper.TryParse(const AString: string; out AValue: NativeInt): Boolean; inline;

Var
  C : Integer;

begin
  Val(AString,AValue,C);
  Result:=(C=0);
end;

Function TNativeIntHelper.ToBoolean: Boolean; inline;

begin
  Result:=(Self<>0);
end;

Function TNativeIntHelper.ToDouble: Double; inline;

begin
  Result:=Self;
end;

Function TNativeIntHelper.ToExtended: Extended; inline;

begin
  Result:=Self;
end;

Function TNativeIntHelper.ToBinString: string; inline;

begin
  Result:=BinStr(Self,Size*8);
end;

Function TNativeIntHelper.ToHexString(const AMinDigits: Integer): string;
overload; inline;

begin
  Result:=IntToHex(Self,AMinDigits);
end;

Function TNativeIntHelper.ToHexString: string; overload; inline;

begin
  Result:=IntToHex(Self,Size*2);
end;

Function TNativeIntHelper.ToString: string; overload; inline;

begin
  Result:=IntToStr(Self);
end;

Function TNativeIntHelper.SetBit(const index: TNativeIntBitIndex) : NativeInt; inline;

begin
  Self := Self or (NativeInt(1) shl index);
  Result:=Self;
end;

Function TNativeIntHelper.ClearBit(const index: TNativeIntBitIndex) : NativeInt; inline;

begin
  Self:=Self and not NativeInt((NativeInt(1) shl index));
  Result:=Self;
end;

Function TNativeIntHelper.ToggleBit(const index: TNativeIntBitIndex) : NativeInt; inline;

begin
  Self := Self xor NativeInt((NativeInt(1) shl index));
  Result:=Self;
end;

Function TNativeIntHelper.TestBit(const Index: TNativeIntBitIndex):Boolean; inline;

begin
  Result := (Self and NativeInt((NativeInt(1) shl index)))<>0;
end;


{ ---------------------------------------------------------------------
  TNativeUIntHelper
  ---------------------------------------------------------------------}

Class Function TNativeUIntHelper.Parse(const AString: string): NativeUInt; inline;

begin
  Result:=StrToInt(AString);
end;

Class Function TNativeUIntHelper.Size: Integer; inline;

begin
  Result:=7;
end;

Class Function TNativeUIntHelper.ToString(const AValue: NativeUInt): string; overload; inline;

begin
  Result:=IntToStr(AValue);
end;

Class Function TNativeUIntHelper.TryParse(const AString: string; out AValue: NativeUInt): Boolean; inline;

Var
  C : Integer;

begin
  Val(AString,AValue,C);
  Result:=(C=0);
end;

Function TNativeUIntHelper.ToBoolean: Boolean; inline;

begin
  Result:=(Self<>0);
end;

Function TNativeUIntHelper.ToDouble: Double; inline;

begin
  Result:=Self;
end;

Function TNativeUIntHelper.ToExtended: Extended; inline;

begin
  Result:=Self;
end;

Function TNativeUIntHelper.ToBinString: string; inline;

begin
  Result:=BinStr(Self,Size*8);
end;

Function TNativeUIntHelper.ToHexString(const AMinDigits: Integer): string;
overload; inline;

begin
  Result:=IntToHex(Self,AMinDigits);
end;

Function TNativeUIntHelper.ToHexString: string; overload; inline;

begin
  Result:=IntToHex(Self,Size*2);
end;

Function TNativeUIntHelper.ToSingle: Single; inline;

begin
  Result:=Self;
end;

Function TNativeUIntHelper.ToString: string; overload; inline;

begin
  Result:=IntToStr(Self);
end;

Function TNativeUIntHelper.SetBit(const index: TNativeUIntBitIndex) : NativeUInt; inline;

begin
  Self := Self or (NativeUInt(1) shl index);
  Result:=Self;
end;

Function TNativeUIntHelper.ClearBit(const index: TNativeUIntBitIndex) : NativeUInt; inline;

begin
  Self:=Self and not NativeUInt((NativeUInt(1) shl index));
  Result:=Self;
end;

Function TNativeUIntHelper.ToggleBit(const index: TNativeUIntBitIndex) : NativeUInt; inline;

begin
  Self := Self xor NativeUInt((NativeUInt(1) shl index));
  Result:=Self;
end;

Function TNativeUIntHelper.TestBit(const Index: TNativeUIntBitIndex):Boolean; inline;

begin
  Result := (Self and NativeUInt((NativeUInt(1) shl index)))<>0;
end;

{ ---------------------------------------------------------------------
  TBooleanHelper
  ---------------------------------------------------------------------}

Class Function TBooleanHelper.Parse(const S: string): Boolean; inline;

begin
  Result:=StrToBool(S);
end;

Class Function TBooleanHelper.Size: Integer; inline;

begin
  Result:=1;
end;

Class Function TBooleanHelper.ToString(const AValue: Boolean; UseBoolStrs: TUseBoolStrs = TUseBoolStrs.False): string; overload; inline;

begin
  Result:=BoolToStr(AValue,UseBoolStrs=TUseBoolStrs.True);
end;

Class Function TBooleanHelper.TryToParse(const S: string; out AValue: Boolean): Boolean; inline;

begin
  Result:=TryStrToBool(S,AValue);
end;

Function TBooleanHelper.ToInteger: Integer; inline;

begin
  Result:=Integer(Self);
end;

Function TBooleanHelper.ToString(UseBoolStrs: TUseBoolStrs = TUseBoolStrs.False): string; overload; inline;

begin
  Result:=BoolToStr(Self,UseBoolStrs=TUseBoolStrs.True);
end;

{ ---------------------------------------------------------------------
  TByteBoolHelper
  ---------------------------------------------------------------------}

Class Function TByteBoolHelper.Parse(const S: string): Boolean; inline;

begin
  Result:=StrToBool(S);
end;

Class Function TByteBoolHelper.Size: Integer; inline;

begin
  Result:=1;
end;

Class Function TByteBoolHelper.ToString(const AValue: Boolean; UseBoolStrs: Boolean = False): string; overload; inline;

begin
  Result:=BoolToStr(AValue,UseBoolStrs);
end;

Class Function TByteBoolHelper.TryToParse(const S: string; out AValue: Boolean): Boolean; inline;

begin
  Result:=TryStrToBool(S,AValue);
end;

Function TByteBoolHelper.ToInteger: Integer; inline;

begin
  Result:=Integer(Self);
end;

Function TByteBoolHelper.ToString(UseBoolStrs: Boolean = False): string; overload; inline;

begin
  Result:=BoolToStr(Self,UseBoolStrs);
end;

{ ---------------------------------------------------------------------
  TWordBoolHelper
  ---------------------------------------------------------------------}

Class Function TWordBoolHelper.Parse(const S: string): Boolean; inline;

begin
  Result:=StrToBool(S);
end;

Class Function TWordBoolHelper.Size: Integer; inline;

begin
  Result:=2;
end;

Class Function TWordBoolHelper.ToString(const AValue: Boolean; UseBoolStrs: boolean = False): string; overload; inline;

begin
  Result:=BoolToStr(AValue,UseBoolStrs);
end;

Class Function TWordBoolHelper.TryToParse(const S: string; out AValue: Boolean): Boolean; inline;

begin
  Result:=TryStrToBool(S,AValue);
end;

Function TWordBoolHelper.ToInteger: Integer; inline;

begin
  Result:=Integer(Self);
end;

Function TWordBoolHelper.ToString(UseBoolStrs: Boolean = False): string; overload; inline;

begin
  Result:=BoolToStr(Self,UseBoolStrs);
end;

{ ---------------------------------------------------------------------
  TLongBoolHelper
  ---------------------------------------------------------------------}


Class Function TLongBoolHelper.Parse(const S: string): Boolean; inline;

begin
  Result:=StrToBool(S);
end;

Class Function TLongBoolHelper.Size: Integer; inline;

begin
  Result:=4;
end;

Class Function TLongBoolHelper.ToString(const AValue: Boolean; UseBoolStrs: Boolean = False): string; overload; inline;

begin
  Result:=BoolToStr(AValue,UseBoolStrs);
end;

Class Function TLongBoolHelper.TryToParse(const S: string; out AValue: Boolean): Boolean; inline;

begin
  Result:=TryStrToBool(S,AValue);
end;

Function TLongBoolHelper.ToInteger: Integer; inline;

begin
  Result:=Integer(Self);
end;

Function TLongBoolHelper.ToString(UseBoolStrs: Boolean = False): string; overload; inline;

begin
  Result:=BoolToStr(Self,UseBoolStrs);
end;

{ TStringBuilder }

constructor TStringBuilder.Create;
begin
  Create(DefaultCapacity,Maxint);
end;

constructor TStringBuilder.Create(const AValue: String; aCapacity: Integer);
begin
  Create(aCapacity,Maxint);
  if (system.Length(AValue)>0) then
    Append(AValue);
end;


constructor TStringBuilder.Create(const AValue: String; StartIndex, Alength,
  aCapacity: Integer);
begin
  Create(Copy(AValue,StartIndex+1,Alength), aCapacity);
end;

constructor TStringBuilder.Create(aCapacity, aMaxCapacity: Integer);
begin
  FMaxCapacity:=aMaxCapacity;
  Capacity:=aCapacity;
end;

constructor TStringBuilder.Create(aCapacity: Integer);
begin
  Create(aCapacity,MaxInt);
end;

constructor TStringBuilder.Create(const AValue: String);
begin
  Create(aValue,DefaultCapacity);
end;


{ Property getter/setter }

function TStringBuilder.GetLength: Integer;
begin
  Result:=System.Length(FData);
end;

function TStringBuilder.GetCapacity: Integer;
begin
  Result:=System.Length(FData);
end;

function TStringBuilder.GetC(Index: Integer): Char;
begin
  CheckNegative(Index,'Index');
  CheckRange(Index,0,Length);
  Result:=FData[Index];
end;

procedure TStringBuilder.SetC(Index: Integer; AValue: Char);
begin
  CheckNegative(Index,'Index');
  CheckRange(Index,0,Length-1);
  FData[Index]:=AValue;
end;

procedure TStringBuilder.SetLength(AValue: Integer);

begin
  CheckNegative(AValue,'AValue');
  CheckRange(AValue,0,MaxCapacity);
  SetLength(FData,aValue);
end;

{ Check functions }



procedure TStringBuilder.CheckRange(Idx, Count, MaxLen: Integer);

begin
  if (Idx<0) or (Idx+Count>MaxLen) then
    Raise ERangeError.CreateFmt(SListIndexError,[Idx]);
end;


procedure TStringBuilder.CheckNegative(const AValue: Integer;
  const AName: String);

begin
  if (AValue<0) then
    Raise ERangeError.CreateFmt(SParamIsNegative,[AName])
end;

{  These do the actual Appending/Inserting }

procedure TStringBuilder.DoAppend(const S: String);

begin
  FData:=FData+S;
end;

procedure TStringBuilder.DoAppend(const AValue: Array of Char; Idx, aCount: Integer
  );

Var
  S : String;
  I : Integer;

begin
  S:='';
  CheckRange(Idx,aCount,System.Length(AValue));
  for I:=Idx to Idx+aCount-1 do
    S:=S+aValue[I];
  DoAppend(S);
end;

procedure TStringBuilder.DoInsert(Index: Integer; const AValue: String);

begin
  CheckRange(Index,0,Length-1);
  System.Insert(aValue,FData,Index+1);
end;

procedure TStringBuilder.DoInsert(Index: Integer; const AValue: Array of Char;
  StartIndex, aCharCount: Integer);

Var
  I : Integer;
  S : String;

begin
  CheckRange(Index,0,Length-1);
  CheckNegative(StartIndex,'StartIndex');
  CheckNegative(aCharCount,'SBCharCount');
  CheckRange(StartIndex,aCharCount,System.Length(AValue));
  S:='';
  for I:=StartIndex to StartIndex+aCharCount-1 do
    S:=S+aValue[I];
  DoInsert(Index,S);
end;

{ Public routines for appending }

function TStringBuilder.Append(const AValue: UInt64): TStringBuilder;
begin
  DoAppend(IntToStr(AValue));
  Result:=self;
end;

function TStringBuilder.Append(const AValue: Array of Char): TStringBuilder;

var
  I,L: Integer;

begin
  I:=-1;
  L:=System.Length(AValue);
  If L=0 then
    Exit(Self);
  Repeat
    Inc(I);
  Until (I>=L) or (AValue[I]=#0);
  DoAppend(AValue,0,I);
  Result:=Self;
end;

function TStringBuilder.Append(const AValue: Single): TStringBuilder;
begin
  DoAppend(FloatToStr(AValue));
  Result:=self;
end;

function TStringBuilder.Append(const AValue: Word): TStringBuilder;
begin
  Append(IntToStr(AValue));
  Result:=self;
end;

function TStringBuilder.Append(const AValue: Cardinal): TStringBuilder;
begin
  DoAppend(IntToStr(AValue));
  Result:=self;
end;

function TStringBuilder.Append(const AValue: Char; RepeatCount: Integer
  ): TStringBuilder;
begin
  DoAppend(StringOfChar(AValue,RepeatCount));
  Result:=Self;
end;


function TStringBuilder.Append(const AValue: Shortint): TStringBuilder;
begin
  DoAppend(IntToStr(AValue));
  Result:=Self;
end;

function TStringBuilder.Append(const AValue: Char): TStringBuilder;
begin
  DoAppend(AValue);
  Result:=Self;
end;

function TStringBuilder.Append(const AValue: Currency): TStringBuilder;
begin
  DoAppend(CurrToStr(AValue));
  Result:=Self;
end;

function TStringBuilder.Append(const AValue: Boolean): TStringBuilder;
begin
  DoAppend(BoolToStr(AValue, True));
  Result:=Self;
end;

function TStringBuilder.Append(const AValue: Byte): TStringBuilder;
begin
  DoAppend(IntToStr(AValue));
  Result:=Self;
end;

function TStringBuilder.Append(const AValue: Double): TStringBuilder;
begin
  DoAppend(FloatToStr(AValue));
  Result:=Self;
end;

function TStringBuilder.Append(const AValue: Int64): TStringBuilder;
begin
  DoAppend(IntToStr(AValue));
  Result:=Self;
end;

function TStringBuilder.Append(const AValue: TObject): TStringBuilder;
begin
  DoAppend(AValue.ToString);
  Result:=Self;
end;

function TStringBuilder.Append(const AValue: Smallint): TStringBuilder;
begin
  DoAppend(IntToStr(AValue));
  Result:=Self;
end;

function TStringBuilder.Append(const AValue: LongInt): TStringBuilder;
begin
  DoAppend(IntToStr(AValue));
  Result:=Self;
end;

Function TStringBuilder.Append(const AValue: Array of Char; StartIndex, SBCharCount: Integer): TStringBuilder;

begin
  DoAppend(AValue,StartIndex,SBCharCount);
  Result:=Self;
end;

Function TStringBuilder.Append(const AValue: String; StartIndex, Count: Integer): TStringBuilder;

begin
  CheckRange(StartIndex,Count,System.Length(AValue));
  DoAppend(Copy(AValue,StartIndex+1,Count));
  Result:=Self;
end;


function TStringBuilder.Append(const AValue: String): TStringBuilder;
begin
  DoAppend(AValue);
  Result:=Self;
end;


function TStringBuilder.AppendFormat(const Fmt: String;
  const Args: array of const): TStringBuilder;
begin
  DoAppend(Format(Fmt,Args));
  Result:=Self;
end;

function TStringBuilder.Append(const Fmt: String;
  const Args: array of const): TStringBuilder;
begin
  DoAppend(Format(Fmt,Args));
  Result:=Self;
end;

function TStringBuilder.AppendLine: TStringBuilder;
begin
  DoAppend(sLineBreak);
  Result:=Self;
end;

function TStringBuilder.AppendLine(const AValue: String): TStringBuilder;
begin
  DoAppend(AValue);
  Result:=AppendLine();
end;

procedure TStringBuilder.Clear;
begin
  Length:=0;
  Capacity:=DefaultCapacity;
end;


procedure TStringBuilder.CopyTo(SourceIndex: Integer;
  Var Destination: Array of Char; DestinationIndex: Integer; Count: Integer);

Var
  I : Integer;

begin
  CheckNegative(Count,'Count');
  CheckNegative(DestinationIndex,'DestinationIndex');
  CheckRange(DestinationIndex,Count,System.Length(Destination));
  if Count>0 then
    begin
    CheckRange(SourceIndex,Count,Length);
    For I:=SourceIndex+1 to SourceIndex+Count do
      Destination[DestinationIndex]:=FData[I];
    end;
end;


function TStringBuilder.EnsureCapacity(aCapacity: Integer): Integer;
begin
  CheckRange(aCapacity,0,MaxCapacity);
  if Capacity<aCapacity then
    Capacity:=aCapacity;
  Result:=Capacity;
end;

function TStringBuilder.Equals(StringBuilder: TStringBuilder): Boolean;
begin
  Result:=(StringBuilder<>nil);
  if Result then
    Result:=(Length=StringBuilder.Length)
             and (MaxCapacity=StringBuilder.MaxCapacity)
             and (FData=StringBuilder.FData[0]);
end;

procedure TStringBuilder.Grow;

var
  NewCapacity: SizeInt;

begin
  NewCapacity:=Capacity*2;
  if NewCapacity>MaxCapacity then
    NewCapacity:=MaxCapacity;
  Capacity:=NewCapacity;
end;

function TStringBuilder.Insert(Index: Integer; const AValue: TObject
  ): TStringBuilder;
begin
  DoInsert(Index,AValue.ToString());
  Result:=Self;
end;

function TStringBuilder.Insert(Index: Integer; const AValue: Int64
  ): TStringBuilder;
begin
  DoInsert(Index,IntToStr(AValue));
  Result:=Self;
end;

function TStringBuilder.Insert(Index: Integer; const AValue: Single
  ): TStringBuilder;
begin
  DoInsert(Index,FloatToStr(AValue));
  Result:=Self;
end;

function TStringBuilder.Insert(Index: Integer; const AValue: String
  ): TStringBuilder;

begin
  DoInsert(Index,AValue);
  Result:=Self;
end;

function TStringBuilder.Insert(Index: Integer; const AValue: Word
  ): TStringBuilder;
begin
  DoInsert(Index,IntToStr(AValue));
  Result:=Self;
end;

function TStringBuilder.Insert(Index: Integer; const AValue: Shortint
  ): TStringBuilder;
begin
  DoInsert(Index, IntToStr(AValue));
  Result:=Self;
end;


function TStringBuilder.Insert(Index: Integer; const AValue: Currency
  ): TStringBuilder;
begin
  DoInsert(Index,CurrToStr(AValue));
  Result:=Self;
end;

function TStringBuilder.Insert(Index: Integer; const AValue: Char
  ): TStringBuilder;
begin
  DoInsert(Index,AValue);
  Result:=Self;
end;

function TStringBuilder.Insert(Index: Integer; const AValue: Byte
  ): TStringBuilder;
begin
  DoInsert(Index,IntToStr(AValue));
  Result:=Self;
end;

function TStringBuilder.Insert(Index: Integer; const AValue: Double
  ): TStringBuilder;
begin
  DoInsert(Index,FloatToStr(AValue));
  Result:=Self;
end;

function TStringBuilder.Insert(Index: Integer; const AValue: LongInt
  ): TStringBuilder;
begin
  DoInsert(Index,IntToStr(AValue));
  Result:=Self;
end;

function TStringBuilder.Insert(Index: Integer; const AValue: Smallint
  ): TStringBuilder;
begin
  DoInsert(Index,IntToStr(AValue));
  Result:=Self;
end;

function TStringBuilder.Insert(Index: Integer; const AValue: Boolean
  ): TStringBuilder;
begin
  DoInsert(Index,BoolToStr(AValue,True));
  Result:=Self;
end;

function TStringBuilder.Insert(Index: Integer; const AValue: String;
  const aRepeatCount: Integer): TStringBuilder;
var
  I: Integer;
begin
  for I:=0 to aRepeatCount-1 do
    DoInsert(Index,AValue);
  Result:=Self;
end;

function TStringBuilder.Insert(Index: Integer; const AValue: Array of Char
  ): TStringBuilder;
begin
  DoInsert(Index,AValue,0,System.Length(AValue));
  Result:=Self;
end;

function TStringBuilder.Insert(Index: Integer; const AValue: Array of Char;
  startIndex: Integer; SBCharCount: Integer): TStringBuilder;
begin
  DoInsert(Index,AValue,StartIndex,SBCharCount);
  Result:=Self;
end;

function TStringBuilder.Insert(Index: Integer; const AValue: Cardinal
  ): TStringBuilder;
begin
  DoInsert(Index,IntToStr(AValue));
  Result:=self;
end;

function TStringBuilder.Insert(Index: Integer; const AValue: UInt64
  ): TStringBuilder;
begin
  DoInsert(Index,IntToStr(AValue));
  Result:=self;
end;

procedure TStringBuilder.Shrink;

begin
  if (Capacity div 4)>=Length then
    Capacity:=Capacity div 2;
end;

function TStringBuilder.Remove(StartIndex: Integer; RemLength: Integer
  ): TStringBuilder;

Var
  MoveIndex : Integer;

begin
  if (RemLength=0) then
    exit(Self);
  CheckNegative(RemLength,'RemLength');
  CheckRange(StartIndex,0,Length);
  MoveIndex:=StartIndex+RemLength;
  CheckRange(MoveIndex,0,Length);
  Delete(FData,StartIndex+1,RemLength);
  Shrink;
  Result:=Self;
end;

Function TStringBuilder.Replace(const OldValue, NewValue: String; StartIndex, Count: Integer): TStringBuilder;

var
  I : Integer;
  Start : String;

begin
  if Count=0 then
    Exit(Self);
  // Some checks.
  CheckNegative(StartIndex,'StartIndex');
  CheckNegative(Count,'Count');
  CheckRange(Startindex,Count,Length);
  // Init
  Start:=Copy(FData,1,StartIndex+1);
  FData:=Copy(FData,StartIndex+1);
  For I:=1 to Count do
    FData:=StringReplace(FData,OldValue,NewValue,[]);
  FData:=Start+FData;
  Result:=Self;
end;


Function TStringBuilder.Replace(const OldValue, NewValue: String): TStringBuilder;
begin
  Result:=Replace(OldValue,NewValue,0,Length);
end;

procedure TStringBuilder.SetCapacity(AValue: Integer);
begin
  if (AValue>FMaxCapacity) then
    Raise ERangeError.CreateFmt(SListCapacityError,[AValue]);
  if (AValue<Length) then
    Raise ERangeError.CreateFmt(SListCapacityError,[AValue]);
  // No-op
end;


function TStringBuilder.ToString: String;
begin
  Result:=ToString(0,Length);
end;

function TStringBuilder.ToString(aStartIndex: Integer; aLength: Integer
  ): String;
begin
  CheckNegative(aStartIndex,'aStartIndex');
  CheckNegative(aLength,'aLength');
  CheckRange(aStartIndex,aLength,Length);
  Writeln('FData : ',FData);
  Result:=Copy(FData,1+aStartIndex,aStartIndex+aLength);
end;

procedure TStringBuilder.DoReplace(Index: Integer; const Old, New: String);

var
  OVLen : Integer;

begin
  OVLen:=System.Length(Old);
  System.Delete(FData,Index+1,OVLen);
  System.Insert(New,FData,Index+1);
end;



initialization
  {$WARN 5043 off}
  ShortMonthNames:=DefaultShortMonthNames;
  LongMonthNames:=DefaultLongMonthNames;
  ShortDayNames:=DefaultShortDayNames;
  LongDayNames:=DefaultLongDayNames;
  FormatSettings:=TFormatSettings.Create;
  // So the defaults are taken from FormatSettings.
  TimeSeparator:=FormatSettings.TimeSeparator;
  DateSeparator:=FormatSettings.DateSeparator;
  ShortDateFormat:=FormatSettings.ShortDateFormat;
  LongDateFormat:=FormatSettings.LongDateFormat;
  ShortTimeFormat:=FormatSettings.ShortTimeFormat;
  LongTimeFormat:=FormatSettings.LongTimeFormat;
  DecimalSeparator:=FormatSettings.DecimalSeparator;
  ThousandSeparator:=FormatSettings.ThousandSeparator;
  TimeAMString:=FormatSettings.TimeAMString;
  TimePMString:=FormatSettings.TimePMString;
  CurrencyFormat:=FormatSettings.CurrencyFormat;
  NegCurrFormat:=FormatSettings.NegCurrFormat;
  CurrencyDecimals:=FormatSettings.CurrencyDecimals;
  CurrencyString:=FormatSettings.CurrencyString;

end.

