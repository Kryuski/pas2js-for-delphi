{
    This file is part of the Free Component Library

    JSON Data structures
    Copyright (c) 2007 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$h+}
unit fpjson;

{$I pas2js_defines.inc}

interface

uses
  {$ifdef fpc}
  variants,
  {$endif}
  {$ifdef pas2js}
  JS, RTLConsts, Types,
  {$endif}
  SysUtils,
  classes,
  contnrs,
  FPCTypes,
  Generics.Collections;

type
  TJSONtype = (jtUnknown, jtNumber, jtString, jtBoolean, jtNull, jtArray, jtObject);
  TJSONInstanceType = (
    jitUnknown,
    jitNumberInteger,
    {$ifdef fpc}
    jitNumberInt64,
    jitNumberQWord,
    {$endif}
    jitNumberFloat,
    jitString,
    jitBoolean,
    jitNull,
    jitArray,
    jitObject);
  TJSONFloat = Double;
  TJSONCharType = WideChar;
  TJSONStringType = Unicodestring;
  {$ifdef fpc}
  PJSONCharType = ^TJSONCharType;
  TJSONVariant = variant;
  TFPJSStream = TMemoryStream;
  {$else}
  TJSONVariant = jsvalue;
  TFPJSStream = TJSArray;
  {$endif}
  TFormatOption = (foSingleLineArray,   // Array without CR/LF: all on one line
                   foSingleLineObject,  // Object without CR/LF: all on one line
                   foDoNotQuoteMembers, // Do not quote object member names.
                   foUseTabchar,        // Use tab characters instead of spaces.
                   foSkipWhiteSpace,    // Do not use whitespace at all
                   foSkipWhiteSpaceOnlyLeading   //  When foSkipWhiteSpace is active, skip whitespace for object members only before :
                   );
  TFormatOptions = set of TFormatOption;

const
  DefaultIndentSize = 2;
  DefaultFormat     = [];
  AsJSONFormat      = [foSingleLineArray,foSingleLineObject]; // These options make FormatJSON behave as AsJSON
  AsCompressedJSON  = [foSingleLineArray,foSingleLineObject,foskipWhiteSpace]; // These options make FormatJSON behave as AsJSON with TJSONData.CompressedJSON=True
  AsCompactJSON     = [foSingleLineArray,foSingleLineObject,foskipWhiteSpace,foDoNotQuoteMembers]; // These options make FormatJSON behave as AsJSON with TJSONData.CompressedJSON=True and TJSONObject.UnquotedMemberNames=True
  ValueJSONTypes    = [jtNumber, jtString, jtBoolean, jtNull];
  ActualValueJSONTypes = ValueJSONTypes - [jtNull];
  StructuredJSONTypes  = [jtArray,jtObject];

Type
  TJSONData = Class;
  TFPObjectList = TObjectList;
  TFPHashObjectList = TDictionary<string, TJSONData>;

  { TBaseJSONEnumerator }

  TJSONEnum = Record
    Key: TJSONStringType;
    KeyNum: Integer;
    Value: TJSONData;
  end;

  TBaseJSONEnumerator = class
  public
    function GetCurrent: TJSONEnum; virtual; abstract;
    function MoveNext: Boolean; virtual; abstract;
    property Current: TJSONEnum read GetCurrent;
  end;

  { TJSONData }
  
  TJSONData = class(TObject)
  private
    const
      ElementSeps: array[Boolean] of TJSONStringType = (', ',',');
    Class var FCompressedJSON: Boolean;
    Class var FElementSep: TJSONStringType;
    class procedure DetermineElementSeparators;
    class function GetCompressedJSON: Boolean; {$ifdef fpc}static;{$endif}
    class procedure SetCompressedJSON(AValue: Boolean); {$ifdef fpc}static;{$endif}
  protected
    Class procedure DoError(const Msg: string); overload;
    Class procedure DoError(const Fmt: string; const Args: array of {$ifdef pas2js}jsvalue{$else}const{$endif}); overload;
    function DoFindPath(const APath: TJSONStringType; Out NotFound: TJSONStringType): TJSONdata; virtual;
    function GetAsBoolean: Boolean; virtual; abstract;
    function GetAsFloat: TJSONFloat; virtual; abstract;
    function GetAsInteger: Integer; virtual; abstract;
    {$ifdef fpc}
    function GetAsInt64: Int64; virtual; abstract;
    function GetAsQWord: QWord; virtual; abstract;
    {$endif}
    function GetIsNull: Boolean; virtual;
    procedure SetAsBoolean(const AValue: Boolean); virtual; abstract;
    procedure SetAsFloat(const AValue: TJSONFloat); virtual; abstract;
    procedure SetAsInteger(const AValue: Integer); virtual; abstract;
    {$ifdef fpc}
    procedure SetAsInt64(const AValue: Int64); virtual; abstract;
    procedure SetAsQword(const AValue: QWord); virtual; abstract;
    {$endif}
    function GetAsJSON: TJSONStringType; virtual; abstract;
    function GetAsString: TJSONStringType; virtual; abstract;
    procedure SetAsString(const AValue: TJSONStringType); virtual; abstract;
    function GetValue: TJSONVariant; virtual; abstract;
    procedure SetValue(const AValue: TJSONVariant); virtual; abstract;
    function GetItem(Index: Integer): TJSONData; virtual;
    procedure SetItem(Index: Integer; const AValue: TJSONData); virtual;
    function DoFormatJSON(Options: TFormatOptions; CurrentIndent, Indent: Integer): TJSONStringType; virtual;
    function GetCount: Integer; virtual;
  Public
    Class function JSONType: TJSONType; virtual;
    Class property CompressedJSON: Boolean Read GetCompressedJSON Write SetCompressedJSON;
  public
    Constructor Create; virtual;
    procedure Clear;  virtual; Abstract;
    procedure DumpJSON(S: TFPJSStream);
    // Get enumerator
    function GetEnumerator: TBaseJSONEnumerator; virtual;
    function FindPath(const APath: TJSONStringType): TJSONdata;
    function GetPath(const APath: TJSONStringType): TJSONdata;
    function Clone: TJSONData; virtual; abstract;
    function FormatJSON(Options: TFormatOptions = DefaultFormat; Indentsize: Integer = DefaultIndentSize): TJSONStringType;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TJSONData read GetItem write SetItem;
    property Value: TJSONVariant read GetValue write SetValue;
    property AsString: TJSONStringType Read GetAsString Write SetAsString;
    property AsFloat: TJSONFloat Read GetAsFloat Write SetAsFloat;
    property AsInteger: Integer Read GetAsInteger Write SetAsInteger;
    {$ifdef fpc}
    property AsInt64: Int64 Read GetAsInt64 Write SetAsInt64;
    property AsQWord: QWord Read GetAsQWord Write SetAsQword;
    {$endif}
    property AsBoolean: Boolean Read GetAsBoolean Write SetAsBoolean;
    property IsNull: Boolean Read GetIsNull;
    property AsJSON: TJSONStringType Read GetAsJSON;
  end;

  TJSONDataClass = Class of TJSONData;
  TJSONNumberType = (
    ntFloat,
    ntInteger
    {$ifdef fpc}
    ,ntInt64
    ,ntQWord
    {$endif}
    );

  TJSONNumber = class(TJSONData)
  protected
  public
    class function JSONType: TJSONType; override;
    class function NumberType: TJSONNumberType; virtual; abstract;
  end;

  { TJSONFloatNumber }

  TJSONFloatNumber = class(TJSONNumber)
  Private
    FValue: TJSONFloat;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsFloat: TJSONFloat; override;
    function GetAsInteger: Integer; override;
    {$ifdef fpc}
    function GetAsInt64: Int64; override;
    function GetAsQWord: QWord; override;
    {$endif}
    procedure SetAsBoolean(const AValue: Boolean); override;
    procedure SetAsFloat(const AValue: TJSONFloat); override;
    procedure SetAsInteger(const AValue: Integer); override;
    {$ifdef fpc}
    procedure SetAsInt64(const AValue: Int64); override;
    procedure SetAsQword(const AValue: QWord); override;
    {$endif}
    function GetAsJSON: TJSONStringType; override;
    function GetAsString: TJSONStringType; override;
    procedure SetAsString(const AValue: TJSONStringType); override;
    function GetValue: TJSONVariant; override;
    procedure SetValue(const AValue: TJSONVariant); override;
  public
    Constructor Create(AValue: TJSONFloat); reintroduce;
    class function NumberType: TJSONNumberType; override;
    procedure Clear;  override;
    function Clone: TJSONData; override;
  end;
  TJSONFloatNumberClass = Class of TJSONFloatNumber;

  { TJSONIntegerNumber }

  TJSONIntegerNumber = class(TJSONNumber)
  Private
    FValue: Integer;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsFloat: TJSONFloat; override;
    function GetAsInteger: Integer; override;
    {$ifdef fpc}
    function GetAsInt64: Int64; override;
    function GetAsQWord: QWord; override;
    {$endif}
    procedure SetAsBoolean(const AValue: Boolean); override;
    procedure SetAsFloat(const AValue: TJSONFloat); override;
    procedure SetAsInteger(const AValue: Integer); override;
    {$ifdef fpc}
    procedure SetAsInt64(const AValue: Int64); override;
    procedure SetAsQword(const AValue: QWord); override;
    {$endif}
    function GetAsJSON: TJSONStringType; override;
    function GetAsString: TJSONStringType; override;
    procedure SetAsString(const AValue: TJSONStringType); override;
    function GetValue: TJSONVariant; override;
    procedure SetValue(const AValue: TJSONVariant); override;
  public
    Constructor Create(AValue: Integer); reintroduce;
    class function NumberType: TJSONNumberType; override;
    procedure Clear;  override;
    function Clone: TJSONData; override;
  end;
  TJSONIntegerNumberClass = Class of TJSONIntegerNumber;

  {$ifdef fpc}
  { TJSONInt64Number }

  TJSONInt64Number = class(TJSONNumber)
  Private
    FValue: Int64;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsFloat: TJSONFloat; override;
    function GetAsInteger: Integer; override;
    function GetAsInt64: Int64; override;
    function GetAsQWord: QWord; override;
    procedure SetAsBoolean(const AValue: Boolean); override;
    procedure SetAsFloat(const AValue: TJSONFloat); override;
    procedure SetAsInteger(const AValue: Integer); override;
    procedure SetAsInt64(const AValue: Int64); override;
    procedure SetAsQword(const AValue: QWord); override;
    function GetAsJSON: TJSONStringType; override;
    function GetAsString: TJSONStringType; override;
    procedure SetAsString(const AValue: TJSONStringType); override;
    function GetValue: TJSONVariant; override;
    procedure SetValue(const AValue: TJSONVariant); override;
  public
    Constructor Create(AValue: Int64); reintroduce;
    class function NumberType: TJSONNumberType; override;
    procedure Clear;  override;
    function Clone: TJSONData; override;
  end;
  TJSONInt64NumberClass = Class of TJSONInt64Number;
  {$endif}

  {$ifdef fpc}
  { TJSONQWordNumber }

  TJSONQWordNumber = class(TJSONNumber)
  Private
    FValue: Qword;
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsFloat: TJSONFloat; override;
    function GetAsInteger: Integer; override;
    function GetAsInt64: Int64; override;
    function GetAsQWord: QWord; override;
    procedure SetAsBoolean(const AValue: Boolean); override;
    procedure SetAsFloat(const AValue: TJSONFloat); override;
    procedure SetAsInteger(const AValue: Integer); override;
    procedure SetAsInt64(const AValue: Int64); override;
    procedure SetAsQword(const AValue: QWord); override;
    function GetAsJSON: TJSONStringType; override;
    function GetAsString: TJSONStringType; override;
    procedure SetAsString(const AValue: TJSONStringType); override;
    function GetValue: TJSONVariant; override;
    procedure SetValue(const AValue: TJSONVariant); override;
  public
    Constructor Create(AValue: QWord); reintroduce;
    class function NumberType: TJSONNumberType; override;
    procedure Clear;  override;
    function Clone: TJSONData; override;
  end;
  TJSONQWordNumberClass = Class of TJSONQWordNumber;
  {$endif}

  { TJSONString }

  TJSONString = class(TJSONData)
  Private
    FValue: TJSONStringType;
  protected
    function GetValue: TJSONVariant; override;
    procedure SetValue(const AValue: TJSONVariant); override;
    function GetAsBoolean: Boolean; override;
    function GetAsFloat: TJSONFloat; override;
    function GetAsInteger: Integer; override;
    {$ifdef fpc}
    function GetAsInt64: Int64; override;
    function GetAsQWord: QWord; override;
    {$endif}
    procedure SetAsBoolean(const AValue: Boolean); override;
    procedure SetAsFloat(const AValue: TJSONFloat); override;
    procedure SetAsInteger(const AValue: Integer); override;
    {$ifdef fpc}
    procedure SetAsInt64(const AValue: Int64); override;
    procedure SetAsQword(const AValue: QWord); override;
    {$endif}
    function GetAsJSON: TJSONStringType; override;
    function GetAsString: TJSONStringType; override;
    procedure SetAsString(const AValue: TJSONStringType); override;
  Public
    Class var StrictEscaping: Boolean;
  public
    Constructor Create(const AValue: TJSONStringType); reintroduce; overload;
    class function JSONType: TJSONType; override;
    procedure Clear;  override;
    function Clone: TJSONData; override;
  end;
  TJSONStringClass = Class of TJSONString;

  { TJSONBoolean }

  TJSONBoolean = class(TJSONData)
  Private
    FValue: Boolean;
  protected
    function GetValue: TJSONVariant; override;
    procedure SetValue(const AValue: TJSONVariant); override;
    function GetAsBoolean: Boolean; override;
    function GetAsFloat: TJSONFloat; override;
    function GetAsInteger: Integer; override;
    {$ifdef fpc}
    function GetAsInt64: Int64; override;
    function GetAsQWord: QWord; override;
    {$endif}
    procedure SetAsBoolean(const AValue: Boolean); override;
    procedure SetAsFloat(const AValue: TJSONFloat); override;
    procedure SetAsInteger(const AValue: Integer); override;
    {$ifdef fpc}
    procedure SetAsInt64(const AValue: Int64); override;
    procedure SetAsQword(const AValue: QWord); override;
    {$endif}
    function GetAsJSON: TJSONStringType; override;
    function GetAsString: TJSONStringType; override;
    procedure SetAsString(const AValue: TJSONStringType); override;
  public
    Constructor Create(AValue: Boolean); reintroduce;
    class function JSONType: TJSONType; override;
    procedure Clear;  override;
    function  Clone: TJSONData; override;
  end;
  TJSONBooleanClass = Class of TJSONBoolean;

  { TJSONnull }

  TJSONNull = class(TJSONData)
  protected
    procedure Converterror(From: Boolean);
    function GetAsBoolean: Boolean; override;
    function GetAsFloat: TJSONFloat; override;
    function GetAsInteger: Integer; override;
    {$ifdef fpc}
    function GetAsInt64: Int64; override;
    function GetAsQWord: QWord; override;
    {$endif}
    function GetIsNull: Boolean; override;
    procedure SetAsBoolean(const AValue: Boolean); override;
    procedure SetAsFloat(const AValue: TJSONFloat); override;
    procedure SetAsInteger(const AValue: Integer); override;
    {$ifdef fpc}
    procedure SetAsInt64(const AValue: Int64); override;
    procedure SetAsQword(const AValue: QWord); override;
    {$endif}
    function GetAsJSON: TJSONStringType; override;
    function GetAsString: TJSONStringType; override;
    procedure SetAsString(const AValue: TJSONStringType); override;
    function GetValue: TJSONVariant; override;
    procedure SetValue(const AValue: TJSONVariant); override;
  public
    class function JSONType: TJSONType; override;
    procedure Clear;  override;
    function Clone: TJSONData; override;
  end;
  TJSONNullClass = Class of TJSONNull;

  TJSONArrayIterator = procedure(Item: TJSONData; Data: TObject; var Continue: Boolean) of object;

  { TJSONArray }
  TJSONObject = Class;

  TJSONArray = class(TJSONData)
  Private
    FList: TFPObjectList;
    function GetArrays(Index: Integer): TJSONArray;
    function GetBooleans(Index: Integer): Boolean;
    function GetFloats(Index: Integer): TJSONFloat;
    function GetIntegers(Index: Integer): Integer;
    {$ifdef fpc}
    function GetInt64s(Index: Integer): Int64;
    {$endif}
    function GetNulls(Index: Integer): Boolean;
    function GetObjects(Index: Integer): TJSONObject;
    {$ifdef fpc}
    function GetQWords(Index: Integer): QWord;
    {$endif}
    function GetStrings(Index: Integer): TJSONStringType;
    function GetTypes(Index: Integer): TJSONType;
    procedure SetArrays(Index: Integer; const AValue: TJSONArray);
    procedure SetBooleans(Index: Integer; const AValue: Boolean);
    procedure SetFloats(Index: Integer; const AValue: TJSONFloat);
    procedure SetIntegers(Index: Integer; const AValue: Integer);
    {$ifdef fpc}
    procedure SetInt64s(Index: Integer; const AValue: Int64);
    {$endif}
    procedure SetObjects(Index: Integer; const AValue: TJSONObject);
    {$ifdef fpc}
    procedure SetQWords(Index: Integer; AValue: QWord);
    {$endif}
    procedure SetStrings(Index: Integer; const AValue: TJSONStringType);
  protected
    function DoFindPath(const APath: TJSONStringType; Out NotFound: TJSONStringType): TJSONdata; override;
    procedure Converterror(From: Boolean);
    function GetAsBoolean: Boolean; override;
    function GetAsFloat: TJSONFloat; override;
    function GetAsInteger: Integer; override;
    {$ifdef fpc}
    function GetAsInt64: Int64; override;
    function GetAsQWord: QWord; override;
    {$endif}
    procedure SetAsBoolean(const AValue: Boolean); override;
    procedure SetAsFloat(const AValue: TJSONFloat); override;
    procedure SetAsInteger(const AValue: Integer); override;
    {$ifdef fpc}
    procedure SetAsInt64(const AValue: Int64); override;
    procedure SetAsQword(const AValue: QWord); override;
    {$endif}
    function GetAsJSON: TJSONStringType; override;
    function GetAsString: TJSONStringType; override;
    procedure SetAsString(const AValue: TJSONStringType); override;
    function GetValue: TJSONVariant; override;
    procedure SetValue(const AValue: TJSONVariant); override;
    function GetCount: Integer; override;
    function GetItem(Index: Integer): TJSONData; override;
    procedure SetItem(Index: Integer; const AValue: TJSONData); override;
    function DoFormatJSON(Options: TFormatOptions; CurrentIndent, Indent: Integer): TJSONStringType; override;
  public
    Constructor Create; reintroduce; overload;
    Constructor Create(const Elements: array of {$ifdef pas2js}jsvalue{$else}const{$endif}); reintroduce; overload;
    Destructor Destroy; override;
    class function JSONType: TJSONType; override;
    function Clone: TJSONData; override;
    // Examine
    procedure Iterate(Iterator: TJSONArrayIterator; Data: TObject);
    function IndexOf(obj: TJSONData): Integer;
    function GetEnumerator: TBaseJSONEnumerator; override;
    // Manipulate
    procedure Clear;  override;
    function Add(Item: TJSONData): Integer; overload;
    function Add(I: Integer): Integer; overload;
    {$ifdef fpc}
    function Add(I: Int64): Int64; overload;
    function Add(I: QWord): QWord; overload;
    {$endif}
    function Add(const S: string): Integer; overload;
    function Add: Integer; overload;
    function Add(F: TJSONFloat): Integer; overload;
    function Add(B: Boolean): Integer; overload;
    function Add(AnArray: TJSONArray): Integer; overload;
    function Add(AnObject: TJSONObject): Integer; overload;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function Extract(Item: TJSONData): TJSONData; overload;
    function Extract(Index: Integer): TJSONData; overload;
    procedure Insert(Index: Integer); overload;
    procedure Insert(Index: Integer; Item: TJSONData); overload;
    procedure Insert(Index: Integer; I: Integer); overload;
    {$ifdef fpc}
    procedure Insert(Index: Integer; I: Int64); overload;
    procedure Insert(Index: Integer; I: QWord); overload;
    {$endif}
    procedure Insert(Index: Integer; const S: string); overload;
    procedure Insert(Index: Integer; F: TJSONFloat); overload;
    procedure Insert(Index: Integer; B: Boolean); overload;
    procedure Insert(Index: Integer; AnArray: TJSONArray); overload;
    procedure Insert(Index: Integer; AnObject: TJSONObject); overload;
    procedure Move(CurIndex, NewIndex: Integer);
    procedure Remove(Item: TJSONData);
    procedure Sort(Compare: TListSortCompare);
    // Easy Access Properties.
    property Items;default;
    property Types[Index: Integer]: TJSONType Read GetTypes;
    property Nulls[Index: Integer]: Boolean Read GetNulls;
    property Integers[Index: Integer]: Integer Read GetIntegers Write SetIntegers;
    {$ifdef fpc}
    property Int64s[Index: Integer]: Int64 Read GetInt64s Write SetInt64s;
    property QWords[Index: Integer]: QWord Read GetQWords Write SetQWords;
    {$endif}
    property Strings[Index: Integer]: TJSONStringType Read GetStrings Write SetStrings;
    property Floats[Index: Integer]: TJSONFloat Read GetFloats Write SetFloats;
    property Booleans[Index: Integer]: Boolean Read GetBooleans Write SetBooleans;
    property Arrays[Index: Integer]: TJSONArray Read GetArrays Write SetArrays;
    property Objects[Index: Integer]: TJSONObject Read GetObjects Write SetObjects;
  end;
  TJSONArrayClass = Class of TJSONArray;

  TJSONObjectIterator = procedure(const AName: TJSONStringType; Item: TJSONData; Data: TObject; var Continue: Boolean) of object;

  { TJSONObject }

  TJSONObject = class(TJSONData)
  private
    const
      ElementStart  : array[Boolean] of TJSONStringType = ('"','');
      SpacedQuoted  : array[Boolean] of TJSONStringType = ('": ',': ');
      UnSpacedQuoted: array[Boolean] of TJSONStringType = ('":',':');
      ObjStartSeps  : array[Boolean] of TJSONStringType = ('{ ','{');
      ObjEndSeps    : array[Boolean] of TJSONStringType = (' }','}');
    Class var FUnquotedMemberNames: Boolean;
    Class var FObjStartSep,FObjEndSep,FElementEnd,FElementStart: TJSONStringType;
    procedure DoAdd(const AName: TJSONStringType; AValue: TJSONData; FreeOnError: Boolean=True);
    Class procedure DetermineElementQuotes;
  Private
    {$ifdef pas2js}
    FCount: integer;
    FHash: TJSObject;
    FNames: TStringDynArray;
    {$else}
    FHash: TFPHashObjectList; // Careful: Names limited to 255 chars.
    {$endif}
    function GetArrays(const AName: string): TJSONArray;
    function GetBooleans(const AName: string): Boolean;
    function GetElements(const AName: string): TJSONData;
    function GetFloats(const AName: string): TJSONFloat;
    function GetIntegers(const AName: string): Integer;
    {$ifdef fpc}
    function GetInt64s(const AName: string): Int64;
    {$endif}
    function GetIsNull(const AName: string): Boolean; reintroduce;
    function GetObjects(const AName: string): TJSONObject;
    {$ifdef fpc}
    function GetQWords(AName: string): QWord;
    {$endif}
    function GetStrings(const AName: string): TJSONStringType;
    function GetTypes(const AName: string): TJSONType;
    procedure SetArrays(const AName: string; const AValue: TJSONArray);
    procedure SetBooleans(const AName: string; const AValue: Boolean);
    procedure SetElements(const AName: string; const AValue: TJSONData);
    procedure SetFloats(const AName: string; const AValue: TJSONFloat);
    procedure SetIntegers(const AName: string; const AValue: Integer);
    {$ifdef fpc}
    procedure SetInt64s(const AName: string; const AValue: Int64);
    {$endif}
    procedure SetIsNull(const AName: string; const AValue: Boolean);
    procedure SetObjects(const AName: string; const AValue: TJSONObject);
    {$ifdef fpc}
    procedure SetQWords(AName: string; AValue: QWord);
    {$endif}
    procedure SetStrings(const AName: string; const AValue: TJSONStringType);
    class function GetUnquotedMemberNames: Boolean; {$ifdef fpc}static;{$endif}
    class procedure SetUnquotedMemberNames(AValue: Boolean); {$ifdef fpc}static;{$endif}
  protected
    function DoFindPath(const APath: TJSONStringType; Out NotFound: TJSONStringType): TJSONdata; override;
    procedure Converterror(From: Boolean);
    function GetAsBoolean: Boolean; override;
    function GetAsFloat: TJSONFloat; override;
    function GetAsInteger: Integer; override;
    {$ifdef fpc}
    function GetAsInt64: Int64; override;
    function GetAsQWord: QWord; override;
    {$endif}
    procedure SetAsBoolean(const AValue: Boolean); override;
    procedure SetAsFloat(const AValue: TJSONFloat); override;
    procedure SetAsInteger(const AValue: Integer); override;
    {$ifdef fpc}
    procedure SetAsInt64(const AValue: Int64); override;
    procedure SetAsQword(const AValue: QWord); override;
    {$endif}
    function GetAsJSON: TJSONStringType; override;
    function GetAsString: TJSONStringType; override;
    procedure SetAsString(const AValue: TJSONStringType); override;
    function GetValue: TJSONVariant; override;
    procedure SetValue(const AValue: TJSONVariant); override;
    function GetCount: Integer; override;
    function DoFormatJSON(Options: TFormatOptions; CurrentIndent, Indent: Integer): TJSONStringType; override;
  public
    constructor Create; reintroduce; overload;
    Constructor Create(const Elements: array of {$ifdef pas2js}jsvalue{$else}const{$endif}); reintroduce; overload;
    destructor Destroy; override;
    class function JSONType: TJSONType; override;
    Class property UnquotedMemberNames: Boolean Read GetUnquotedMemberNames Write SetUnquotedMemberNames;
    function Clone: TJSONData; override;
    // Examine
    function Find(const AName: string): TJSONData; overload;
    function Find(const AName: string; AType: TJSONType): TJSONData; overload;
    function Find(const key: TJSONStringType; out AValue: TJSONData): boolean; overload;
    function Find(const key: TJSONStringType; out AValue: TJSONObject): boolean; overload;
    function Find(const key: TJSONStringType; out AValue: TJSONArray): boolean; overload;
    function Find(const key: TJSONStringType; out AValue: TJSONString): boolean; overload;
    function Find(const key: TJSONStringType; out AValue: TJSONBoolean): boolean; overload;
    function Find(const key: TJSONStringType; out AValue: TJSONNumber): boolean; overload;
    function Get(const AName: string): TJSONVariant; overload;
    function Get(const AName: string; ADefault: TJSONFloat): TJSONFloat; overload;
    {$ifdef fpc}
    function Get(const AName: string; ADefault: Int64): Int64; overload;
    function Get(const AName: string; ADefault: QWord): QWord; overload;
    {$endif}
    function Get(const AName: string; ADefault: Boolean): Boolean; overload;
    function Get(const AName: string; ADefault: TJSONStringType): TJSONStringType; overload;
    function Get(const AName: string; ADefault: TJSONArray): TJSONArray; overload;
    function Get(const AName: string; ADefault: TJSONObject): TJSONObject; overload;
    // Manipulate
    procedure Clear;  override;
    procedure Add(const AName: TJSONStringType; AValue: TJSONData); overload;
    procedure Add(const AName: TJSONStringType; AValue: Boolean); overload;
    procedure Add(const AName: TJSONStringType; AValue: TJSONFloat); overload;
    procedure Add(const AName, AValue: TJSONStringType); overload;
    procedure Add(const AName: TJSONStringType; Avalue: Integer); overload;
    {$ifdef fpc}
    procedure Add(const AName: TJSONStringType; Avalue: Int64); overload;
    procedure Add(const AName: TJSONStringType; Avalue: QWord); overload;
    {$endif}
    procedure Add(const AName: TJSONStringType); overload;
    procedure Add(const AName: TJSONStringType; AValue: TJSONArray); overload;
    procedure Delete(const AName: string); overload;
    procedure Remove(Value: TJSONData);
    {$ifdef fpc}
    function Extract(const AName: string): TJSONData; overload;
    {$endif}

    // Easy access properties.
    property Elements[const AName: string]: TJSONData read GetElements write SetElements; default;

    property Types[const AName: string]: TJSONType Read GetTypes;
    property Nulls[const AName: string]: Boolean Read GetIsNull Write SetIsNull;
    property Floats[const AName: string]: TJSONFloat Read GetFloats Write SetFloats;
    property Integers[const AName: string]: Integer Read GetIntegers Write SetIntegers;
    {$ifdef fpc}
    property Int64s[const AName: string]: Int64 Read GetInt64s Write SetInt64s;
    property QWords[AName: string]: QWord Read GetQWords Write SetQWords;
    {$endif}
    property Strings[const AName: string]: TJSONStringType Read GetStrings Write SetStrings;
    property Booleans[const AName: string]: Boolean Read GetBooleans Write SetBooleans;
    property Arrays[const AName: string]: TJSONArray Read GetArrays Write SetArrays;
    property Objects[const AName: string]: TJSONObject Read GetObjects Write SetObjects;
  end;
  TJSONObjectClass = Class of TJSONObject;

  EJSON = Class(Exception);

  {$ifdef fpc}
  TJSONParserHandler = procedure(AStream: TStream; const AUseUTF8: Boolean; Out Data: TJSONData);
  {$endif}

function SetJSONInstanceType(AType: TJSONInstanceType; AClass: TJSONDataClass): TJSONDataClass;
function GetJSONInstanceType(AType: TJSONInstanceType): TJSONDataClass;

function StringToJSONString(const S: TJSONStringType; Strict: Boolean = False): TJSONStringType;
function JSONStringToString(const S: TJSONStringType): TJSONStringType;
function JSONTypeName(JSONType: TJSONType): string;

// These functions create JSONData structures, taking into account the instance types
function CreateJSON: TJSONNull; overload;
function CreateJSON(Data: Boolean): TJSONBoolean; overload;
function CreateJSON(Data: Integer): TJSONIntegerNumber; overload;
{$ifdef fpc}
function CreateJSON(Data: Int64): TJSONInt64Number; overload;
function CreateJSON(Data: QWord): TJSONQWordNumber; overload;
{$endif}
function CreateJSON(Data: TJSONFloat): TJSONFloatNumber; overload;
function CreateJSON(const Data: TJSONStringType): TJSONString; overload;
function CreateJSONArray(const Data: array of {$ifdef pas2js}jsvalue{$else}const{$endif}): TJSONArray;
function CreateJSONObject(const Data: array of {$ifdef pas2js}jsvalue{$else}const{$endif}): TJSONObject;

// These functions rely on a callback. If the callback is not set, they will raise an error.
// When the jsonparser unit is included in the project, the callback is automatically set.
{$ifdef fpc}
function GetJSON(const JSON: TJSONStringType; const UseUTF8: Boolean = True): TJSONData; overload;
function GetJSON(const JSON: TStream; const UseUTF8: Boolean = True): TJSONData; overload;
function SetJSONParserHandler(AHandler: TJSONParserHandler): TJSONParserHandler;
function GetJSONParserHandler: TJSONParserHandler;
{$endif}

implementation

uses
  TypInfo, Types;

resourcestring
  SErrCannotConvertFromNull = 'Cannot convert data from Null value';
  SErrCannotConvertToNull = 'Cannot convert data to Null value';
  SErrCannotConvertFromArray = 'Cannot convert data from array value';
  SErrCannotConvertToArray = 'Cannot convert data to array value';
  SErrCannotConvertFromObject = 'Cannot convert data from object value';
  SErrCannotConvertToObject = 'Cannot convert data to object value';
  SErrInvalidFloat = 'Invalid float value: %s';
  SErrCannotSetNotIsNull = 'IsNull cannot be set to False';
  SErrCannotAddArrayTwice = 'Adding an array object to an array twice is not allowed';
  SErrCannotAddObjectTwice = 'Adding an object to an array twice is not allowed';
  SErrUnknownTypeInConstructor = 'Unknown type in JSON%s constructor: %d';
  SErrNotJSONData = 'Cannot add object of type %s to TJSON%s';
  SErrOddNumber = 'TJSONObject must be constructed with name,value pairs';
  SErrNameMustBeString = 'TJSONObject constructor element name at pos %d is not a string';
  SErrNonexistentElement = 'Unknown object member: "%s"';
  SErrDuplicateValue = 'Duplicate object member: "%s"';
  SErrPathElementNotFound = 'Path "%s" invalid: element "%s" not found.';
  SErrWrongInstanceClass = 'Cannot set instance class: %s does not descend from %s.';
  {$ifdef fpc}
  SErrPointerNotNil = 'Cannot add non-nil pointer to JSON%s';
  SErrNoParserHandler = 'No JSON parser handler installed. Recompile your project with the jsonparser unit included';
  {$endif}

var
  DefaultJSONInstanceTypes :
    array [TJSONInstanceType] of TJSONDataClass = (
      TJSONData,
      TJSONIntegerNumber,
      {$ifdef fpc}
      TJSONInt64Number,
      TJSONQWordNumber,
      {$endif}
      TJSONFloatNumber,
      TJSONString,
      TJSONBoolean,
      TJSONNull,
      TJSONArray,
      TJSONObject);
const
  MinJSONInstanceTypes :
    array [TJSONInstanceType] of TJSONDataClass = (
      TJSONData,
      TJSONIntegerNumber,
      {$ifdef fpc}
      TJSONInt64Number,
      TJSONQWordNumber,
      {$endif}
      TJSONFloatNumber,
      TJSONString,
      TJSONBoolean,
      TJSONNull,
      TJSONArray,
      TJSONObject
      );

function SetJSONInstanceType(AType: TJSONInstanceType; AClass: TJSONDataClass): TJSONDataClass;
begin
  if AClass=nil then
    TJSONData.DoError(SErrWrongInstanceClass,['Nil',MinJSONInstanceTypes[AType].ClassName]);
  if not AClass.InheritsFrom(MinJSONINstanceTypes[AType]) then
    TJSONData.DoError(SErrWrongInstanceClass,[AClass.ClassName,MinJSONInstanceTypes[AType].ClassName]);
  Result := DefaultJSONInstanceTypes[AType];
  DefaultJSONINstanceTypes[AType] := AClass;
end;

function GetJSONInstanceType(AType: TJSONInstanceType): TJSONDataClass;
begin
  Result := DefaultJSONInstanceTypes[AType]
end;

function StringToJSONString(const S: TJSONStringType;
  Strict: Boolean = False): TJSONStringType;
var
  I,J,L: Integer;
  C: Char;
begin
  I := 1;
  J := 1;
  Result := '';
  L := Length(S);
  while I<=L do begin
    C := S[I];
    if (C in ['"','/','\',#0..#31]) then begin
      Result := Result+Copy(S,J,I-J);
      Case C of
        '\': Result := Result+'\\';
        '/': if Strict then
                Result := Result+'\/'
              else
                Result := Result+'/';
        '"': Result := Result+'\"';
        #8 : Result := Result+'\b';
        #9 : Result := Result+'\t';
        #10: Result := Result+'\n';
        #12: Result := Result+'\f';
        #13: Result := Result+'\r';
      else
        Result := Result+'\u'+HexStr(Ord(C),4);
      end;
      J := I+1;
    end;
    Inc(I);
  end;
  Result := Result + Copy(S, J, I-1);
end;

function JSONStringToString(const S: TJSONStringType): TJSONStringType;
var
  I, J, L, U1, U2: Integer;
  App, W: string;

  procedure MaybeAppendUnicode;
  var
    U: string;
  begin
    if U1 <> 0 then begin
      U := WideChar(U1);
      Result := Result + U;
      U1 := 0;
    end;
  end;

begin
  I := 1;
  J := 1;
  L := Length(S);
  Result := '';
  U1 := 0;
  while I<=L do begin
    if S[I]='\' then begin
      Result := Result+Copy(S,J,I-J);
      if I<L then begin
        Inc(I);
        App := '';
        case S[I] of
          '\','"','/'
             : App := S[I];
          'b': App := #8;
          't': App := #9;
          'n': App := #10;
          'f': App := #12;
          'r': App := #13;
          'u': begin
               W := Copy(S, I+1, 4);
               Inc(I, 4);
               u2 := StrToInt('$'+W);
               if U1 <> 0 then
                 App := WideChar(U1)+WideChar(U2)
               else
                 U1 := U2;
          end;
        end;
        if App <> '' then begin
          MaybeAppendUnicode;
          Result := Result+App;
        end;
      end;
      J := I+1;
    end else
      MaybeAppendUnicode;
    Inc(I);
  end;
  MaybeAppendUnicode;
  Result := Result + Copy(S,J,I-J+1);
end;

function JSONTypeName(JSONType: TJSONType): string;
begin
  Result := GetEnumName(TypeInfo(TJSONType),Ord(JSONType));
end;

function CreateJSON: TJSONNull;
begin
  Result := TJSONNullClass(DefaultJSONInstanceTypes[jitNull]).Create
end;

function CreateJSON(Data: Boolean): TJSONBoolean;
begin
  Result := TJSONBooleanClass(DefaultJSONInstanceTypes[jitBoolean]).Create(Data);
end;

function CreateJSON(Data: Integer): TJSONIntegerNumber;
begin
  Result := TJSONIntegerNumberCLass(DefaultJSONInstanceTypes[jitNumberInteger]).Create(Data);
end;

{$ifdef fpc}
function CreateJSON(Data: Int64): TJSONInt64Number;
begin
  Result := TJSONInt64NumberCLass(DefaultJSONInstanceTypes[jitNumberInt64]).Create(Data);
end;

function CreateJSON(Data: QWord): TJSONQWordNumber;
begin
  Result := TJSONQWordNumberClass(DefaultJSONInstanceTypes[jitNumberQWord]).Create(Data);
end;
{$endif}

function CreateJSON(Data: TJSONFloat): TJSONFloatNumber;
begin
  Result := TJSONFloatNumberCLass(DefaultJSONInstanceTypes[jitNumberFloat]).Create(Data);
end;

function CreateJSON(const Data: TJSONStringType): TJSONString;
begin
  Result := TJSONStringCLass(DefaultJSONInstanceTypes[jitString]).Create(Data);
end;

function CreateJSONArray(const Data: array of {$ifdef pas2js}jsvalue{$else}const{$endif}): TJSONArray;
begin
  Result := TJSONArrayCLass(DefaultJSONInstanceTypes[jitArray]).Create(Data);
end;

function CreateJSONObject(const Data: array of {$ifdef pas2js}jsvalue{$else}const{$endif}): TJSONObject;
begin
  Result := TJSONObjectClass(DefaultJSONInstanceTypes[jitObject]).Create(Data);
end;

{$ifdef fpc}
function GetJSON(const JSON: TJSONStringType; const UseUTF8: Boolean): TJSONData;

var
  SS: TStringStream;
begin
  SS := TStringStream.Create(JSON);
  try
    Result := GetJSON(SS,UseUTF8);
  finally
    SS.Free;
  end;
end;
{$endif}

{$ifdef fpc}
var
  JPH: TJSONParserHandler;

function GetJSON(const JSON: TStream; const UseUTF8: Boolean): TJSONData;

begin
  Result := nil;
  if not Assigned(JPH) then
    TJSONData.DoError(SErrNoParserHandler);
  JPH(JSON,UseUTF8,Result);
end;

function SetJSONParserHandler(AHandler: TJSONParserHandler): TJSONParserHandler;
begin
  Result := JPH;
  JPH := AHandler;
end;

function GetJSONParserHandler: TJSONParserHandler;
begin
  Result := JPH;
end;
{$endif}

Type
  { TJSONEnumerator }

  TJSONEnumerator = class(TBaseJSONEnumerator)
  Private
    FData: TJSONData;
  public
    Constructor Create(AData: TJSONData);
    function GetCurrent: TJSONEnum; override;
    function MoveNext: Boolean; override;
  end;

  { TJSONArrayEnumerator }

  TJSONArrayEnumerator = class(TBaseJSONEnumerator)
  Private
    FData: TJSONArray;
    FCurrent: Integer;
  public
    Constructor Create(AData: TJSONArray);
    function GetCurrent: TJSONEnum; override;
    function MoveNext: Boolean; override;
  end;

{$ifdef fpc}
{ TJSONQWordNumber }

function TJSONQWordNumber.GetAsBoolean: Boolean;
begin
  Result := FValue <> 0;
end;

function TJSONQWordNumber.GetAsFloat: TJSONFloat;
begin
  Result := FValue;
end;

function TJSONQWordNumber.GetAsInteger: Integer;
begin
  Result := FValue;
end;

function TJSONQWordNumber.GetAsInt64: Int64;
begin
  Result := FValue;
end;

function TJSONQWordNumber.GetAsQWord: QWord;
begin
  Result := FValue;
end;

procedure TJSONQWordNumber.SetAsBoolean(const AValue: Boolean);
begin
  FValue := Ord(AValue);
end;

procedure TJSONQWordNumber.SetAsFloat(const AValue: TJSONFloat);
begin
  FValue := Round(AValue);
end;

procedure TJSONQWordNumber.SetAsInteger(const AValue: Integer);
begin
  FValue := AValue;
end;

procedure TJSONQWordNumber.SetAsInt64(const AValue: Int64);
begin
  FValue := AValue;
end;

procedure TJSONQWordNumber.SetAsQword(const AValue: QWord);
begin
  FValue := AValue;
end;

function TJSONQWordNumber.GetAsJSON: TJSONStringType;
begin
  Result := AsString;
end;

function TJSONQWordNumber.GetAsString: TJSONStringType;
begin
  Result := IntToStr(FValue);
end;

procedure TJSONQWordNumber.SetAsString(const AValue: TJSONStringType);
begin
  FValue := StrToQWord(AValue);
end;

function TJSONQWordNumber.GetValue: TJSONVariant;
begin
  Result := FValue;
end;

procedure TJSONQWordNumber.SetValue(const AValue: TJSONVariant);
begin
  FValue := AValue;
end;

constructor TJSONQWordNumber.Create(AValue: QWord);
begin
  FValue := AValue;
end;

class function TJSONQWordNumber.NumberType: TJSONNumberType;
begin
  Result := ntQWord;
end;

procedure TJSONQWordNumber.Clear;
begin
  FValue := 0;
end;

function TJSONQWordNumber.Clone: TJSONData;
begin
  Result := TJSONQWordNumberClass(ClassType).Create(Self.FValue);
end;
{$endif}

{ TJSONArrayEnumerator }

constructor TJSONArrayEnumerator.Create(AData: TJSONArray);
begin
  FData := AData;
  FCurrent := -1;
end;

function TJSONArrayEnumerator.GetCurrent: TJSONEnum;
begin
  Result.KeyNum := FCurrent;
  Result.Key := IntToStr(FCurrent);
  Result.Value := FData.Items[FCurrent];
end;

function TJSONArrayEnumerator.MoveNext: Boolean;
begin
  Inc(FCurrent);
  Result := FCurrent<FData.Count;
end;

  { TJSONEnumerator }

constructor TJSONEnumerator.Create(AData: TJSONData);
begin
  FData := AData;
end;

function TJSONEnumerator.GetCurrent: TJSONEnum;
begin
  Result.Key := '';
  Result.KeyNum := 0;
  Result.Value := FData;
  FData := nil;
end;

function TJSONEnumerator.MoveNext: Boolean;
begin
  Result := FData <> nil;
end;

{ TJSONData }

function TJSONData.GetItem(Index: Integer): TJSONData;
begin
  Result := nil;
  if Index>0 then ;
end;

function TJSONData.GetCount: Integer;
begin
  Result := 0;
end;

constructor TJSONData.Create;
begin
  Clear;
end;

procedure TJSONData.DumpJSON(S: TFPJSStream);

  procedure W(T: string);
  var
    U: UTF8String;
  begin
    if T='' then Exit;
    {$ifdef pas2js}
    S.push(T);
    {$else}
    U := UTF8Encode(T);
    S.WriteBuffer(U[1],Length(U));
    {$endif}
  end;

var
  I: Integer;
  item: TPair<string, TJSONData>;
begin
  Case JSONType of
    jtObject: begin
      var o := TJSONObject(Self);
      W('{');
      var first := True;
      for item in o.FHash do begin
        if not first then
        begin
          W(',');
        end;
        first := False;
        W('"');
        W(StringToJSONString(item.Key, False));
        W('":');
        item.Value.DumpJSON(S);
      end;
      W('}');
    end;
    jtArray :
      begin
      W('[');
      for I := 0 to Count-1 do
        begin
        if (I>0) then
          W(',');
        Items[I].DumpJSON(S);
        end;
      W(']');
      end
  else
    W(AsJSON)
  end;
end;

class function TJSONData.GetCompressedJSON: Boolean;
begin
  Result := FCompressedJSON;
end;

class procedure TJSONData.DetermineElementSeparators;


begin
  FElementSep := ElementSeps[FCompressedJSON];
end;

class procedure TJSONData.SetCompressedJSON(AValue: Boolean);


begin
  if AValue=FCompressedJSON then Exit;
  FCompressedJSON := AValue;
  DetermineElementSeparators;
  TJSONObject.DetermineElementQuotes;
end;

class procedure TJSONData.DoError(const Msg: string);
begin
  Raise EJSON.Create(Msg);
end;

class procedure TJSONData.DoError(const Fmt: string;
  const Args: array of {$ifdef pas2js}jsvalue{$else}const{$endif});
begin
  Raise EJSON.CreateFmt(Fmt,Args);
end;

function TJSONData.DoFindPath(const APath: TJSONStringType; out
  NotFound: TJSONStringType): TJSONdata;
begin
  if APath <> '' then
    begin
    NotFound := APath;
    Result := nil;
    end
  else
    Result := Self;
end;

function TJSONData.GetIsNull: Boolean;
begin
  Result := False;
end;

class function TJSONData.JSONType: TJSONType;
begin
  JSONType := jtUnknown;
end;

function TJSONData.GetEnumerator: TBaseJSONEnumerator;
begin
  Result := TJSONEnumerator.Create(Self);
end;

function TJSONData.FindPath(const APath: TJSONStringType): TJSONdata;

var
  M: TJSONStringType;

begin
  Result := DoFindPath(APath,M);
end;

function TJSONData.GetPath(const APath: TJSONStringType): TJSONdata;

var
  M: TJSONStringType;
begin
  Result := DoFindPath(APath,M);
  if Result=nil then
    DoError(SErrPathElementNotFound,[APath,M]);
end;

procedure TJSONData.SetItem(Index: Integer; const AValue:
  TJSONData);
begin
  // Do Nothing
  if Index>0 then ;
  if AValue <> nil then ;
end;

function TJSONData.FormatJSON(Options: TFormatOptions; Indentsize: Integer
  ): TJSONStringType;

begin
  Result := DoFormatJSON(Options,0,IndentSize);
end;

function TJSONData.DoFormatJSON(Options: TFormatOptions; CurrentIndent,
  Indent: Integer): TJSONStringType;

begin
  Result := AsJSON;
  if Options=[] then ;
  if CurrentIndent=0 then ;
  if Indent>0 then ;
end;

{ TJSONnumber }

class function TJSONnumber.JSONType: TJSONType;
begin
  Result := jtNumber;
end;


{ TJSONstring }

class function TJSONString.JSONType: TJSONType;
begin
  Result := jtString;
end;

procedure TJSONString.Clear;
begin
  FValue := '';
end;

function TJSONString.Clone: TJSONData;

begin
  Result := TJSONStringClass(ClassType).Create(Self.FValue);
end;

function TJSONString.GetValue: TJSONVariant;
begin
  Result := FValue;
end;

procedure TJSONString.SetValue(const AValue: TJSONVariant);
begin
  FValue := {$ifdef pas2js}TJSONStringType(AValue){$else}AValue{$endif};
end;


function TJSONString.GetAsBoolean: Boolean;
begin
  Result := StrToBool(FValue);
end;

function TJSONString.GetAsFloat: TJSONFloat;

var
  C: Integer;

begin
  Val(FValue,Result,C);
  if (C <> 0) then
    if not TryStrToFloat(FValue,Result) then
      Raise EConvertError.CreateFmt(SErrInvalidFloat,[FValue]);
end;

function TJSONString.GetAsInteger: Integer;
begin
  Result := StrToInt(FValue);
end;

{$ifdef fpc}
function TJSONString.GetAsInt64: Int64;
begin
  Result := StrToInt64(FValue);
end;

function TJSONString.GetAsQWord: QWord;
begin
  Result := StrToQWord(FValue);
end;
{$endif}

procedure TJSONString.SetAsBoolean(const AValue: Boolean);
begin
  FValue := BoolToStr(AValue);
end;

procedure TJSONString.SetAsFloat(const AValue: TJSONFloat);
begin
  FValue := FloatToStr(AValue);
end;

procedure TJSONString.SetAsInteger(const AValue: Integer);
begin
  FValue := IntToStr(AValue);
end;

{$ifdef fpc}
procedure TJSONString.SetAsInt64(const AValue: Int64);
begin
  FValue := IntToStr(AValue);
end;

procedure TJSONString.SetAsQword(const AValue: QWord);
begin
  FValue := IntToStr(AValue);
end;
{$endif}

function TJSONString.GetAsJSON: TJSONStringType;
begin
  Result := '"'+StringToJSONString(FValue,StrictEscaping)+'"';
end;

function TJSONString.GetAsString: TJSONStringType;
begin
  Result := FValue;
end;

procedure TJSONString.SetAsString(const AValue: TJSONStringType);
begin
  FValue := AValue;
end;

constructor TJSONString.Create(const AValue: TJSONStringType);
begin
  FValue := AValue;
end;

{ TJSONboolean }

function TJSONBoolean.GetValue: TJSONVariant;
begin
  Result := FValue;
end;

class function TJSONBoolean.JSONType: TJSONType;
begin
  Result := jtBoolean;
end;

procedure TJSONBoolean.Clear;
begin
  FValue := False;
end;

function TJSONBoolean.Clone: TJSONData;
begin
  Result := TJSONBooleanClass(Self.ClassType).Create(Self.Fvalue);
end;


procedure TJSONBoolean.SetValue(const AValue: TJSONVariant);
begin
  FValue := boolean(AValue);
end;

function TJSONBoolean.GetAsBoolean: Boolean;
begin
  Result := FValue;
end;

function TJSONBoolean.GetAsFloat: TJSONFloat;
begin
  Result := Ord(FValue);
end;

function TJSONBoolean.GetAsInteger: Integer;
begin
  Result := Ord(FValue);
end;

{$ifdef fpc}
function TJSONBoolean.GetAsInt64: Int64;
begin
  Result := Ord(FValue);
end;

function TJSONBoolean.GetAsQWord: QWord;
begin
  Result := Ord(FValue);
end;
{$endif}

procedure TJSONBoolean.SetAsBoolean(const AValue: Boolean);
begin
  FValue := AValue;
end;

procedure TJSONBoolean.SetAsFloat(const AValue: TJSONFloat);
begin
  FValue := (AValue <> 0)
end;

procedure TJSONBoolean.SetAsInteger(const AValue: Integer);
begin
  FValue := (AValue <> 0)
end;

{$ifdef fpc}
procedure TJSONBoolean.SetAsInt64(const AValue: Int64);
begin
  FValue := (AValue <> 0)
end;

procedure TJSONBoolean.SetAsQword(const AValue: QWord);
begin
  FValue := (AValue <> 0)
end;
{$endif}

function TJSONBoolean.GetAsJSON: TJSONStringType;
begin
  if FValue then
    Result := 'true'
  else
    Result := 'false';
end;

function TJSONBoolean.GetAsString: TJSONStringType;
begin
  Result := BoolToStr(FValue, True);
end;

procedure TJSONBoolean.SetAsString(const AValue: TJSONStringType);
begin
  FValue := StrToBool(AValue);
end;


constructor TJSONBoolean.Create(AValue: Boolean);
begin
  FValue := AValue;
end;

{ TJSONnull }

procedure TJSONNull.Converterror(From: Boolean);
begin
  if From then
    DoError(SErrCannotConvertFromNull)
  else
    DoError(SErrCannotConvertToNull);
end;

{$warnings off}
function TJSONNull.GetAsBoolean: Boolean;
begin
  ConvertError(True);
  Result := false;
end;

function TJSONNull.GetAsFloat: TJSONFloat;
begin
  ConvertError(True);
  Result := 0.0;
end;

function TJSONNull.GetAsInteger: Integer;
begin
  ConvertError(True);
  Result := 0;
end;

{$ifdef fpc}
function TJSONNull.GetAsInt64: Int64;
begin
  ConvertError(True);
end;

function TJSONNull.GetAsQWord: QWord;
begin
  ConvertError(True);
end;
{$endif}

function TJSONNull.GetIsNull: Boolean;
begin
  Result := True;
end;

procedure TJSONNull.SetAsBoolean(const AValue: Boolean);
begin
  ConvertError(False);
  if AValue then ;
end;

procedure TJSONNull.SetAsFloat(const AValue: TJSONFloat);
begin
  ConvertError(False);
  if AValue>0 then ;
end;

procedure TJSONNull.SetAsInteger(const AValue: Integer);
begin
  ConvertError(False);
  if AValue>0 then ;
end;

{$ifdef fpc}
procedure TJSONNull.SetAsInt64(const AValue: Int64);
begin
  ConvertError(False);
  if AValue>0 then ;
end;

procedure TJSONNull.SetAsQword(const AValue: QWord);
begin
  ConvertError(False);
  if AValue>0 then ;
end;
{$endif}

function TJSONNull.GetAsJSON: TJSONStringType;
begin
  Result := 'null';
end;

function TJSONNull.GetAsString: TJSONStringType;
begin
  ConvertError(True);
  Result := '';
end;

procedure TJSONNull.SetAsString(const AValue: TJSONStringType);
begin
  ConvertError(True);
  if AValue='' then ;
end;


function TJSONNull.GetValue: TJSONVariant;
begin
  Result := {$ifdef pas2js}js.Null{$else}variants.Null{$endif};
end;

procedure TJSONNull.SetValue(const AValue: TJSONVariant);
begin
  ConvertError(False);
  {$ifdef pas2js}
  if AValue=0 then ;
  {$else}
  if VarType(AValue)=0 then ;
  {$endif}
end;

class function TJSONNull.JSONType: TJSONType;
begin
  Result := jtNull;
end;

procedure TJSONNull.Clear;
begin
  // Do nothing
end;

function TJSONNull.Clone: TJSONData;
begin
  Result := TJSONNullClass(Self.ClassType).Create;
end;

{$warnings on}



{ TJSONFloatNumber }

function TJSONFloatNumber.GetAsBoolean: Boolean;
begin
  Result := (FValue <> 0);
end;

function TJSONFloatNumber.GetAsFloat: TJSONFloat;
begin
  Result := FValue;
end;

function TJSONFloatNumber.GetAsInteger: Integer;
begin
  Result := Round(FValue);
end;

{$ifdef fpc}
function TJSONFloatNumber.GetAsInt64: Int64;
begin
  Result := Round(FValue);
end;

function TJSONFloatNumber.GetAsQWord: QWord;
begin
  Result := Round(FValue);
end;
{$endif}

procedure TJSONFloatNumber.SetAsBoolean(const AValue: Boolean);
begin
  FValue := Ord(AValue);
end;

procedure TJSONFloatNumber.SetAsFloat(const AValue: TJSONFloat);
begin
  FValue := AValue;
end;

procedure TJSONFloatNumber.SetAsInteger(const AValue: Integer);
begin
  FValue := AValue;
end;

{$ifdef fpc}
procedure TJSONFloatNumber.SetAsInt64(const AValue: Int64);
begin
  FValue := AValue;
end;

procedure TJSONFloatNumber.SetAsQword(const AValue: QWord);
begin
  FValue := AValue;
end;
{$endif}

function TJSONFloatNumber.GetAsJSON: TJSONStringType;
begin
  Result := AsString;
end;

function TJSONFloatNumber.GetAsString: TJSONStringType;
begin
  {$WARN IMPLICIT_STRING_CAST OFF}
  Str(FValue, Result);
  {$WARN IMPLICIT_STRING_CAST ON}
  // Str produces a ' ' in front where the - can go.
  if (Result <> '') and (Result[1] = ' ') then
    Delete(Result, 1, 1);
end;

procedure TJSONFloatNumber.SetAsString(const AValue: TJSONStringType);
var
  C: Integer;
begin
  Val(AValue,FValue,C);
  if (C <> 0) then
    Raise EConvertError.CreateFmt(SErrInvalidFloat,[AValue]);
end;


function TJSONFloatNumber.GetValue: TJSONVariant;
begin
  Result := FValue;
end;

procedure TJSONFloatNumber.SetValue(const AValue: TJSONVariant);
begin
  FValue := {$ifdef pas2js}TJSONFloat(AValue){$else}AValue{$endif};
end;

constructor TJSONFloatNumber.Create(AValue: TJSONFloat);
begin
  FValue := AValue;
end;

class function TJSONFloatNumber.NumberType: TJSONNumberType;
begin
  Result := ntFloat;
end;

procedure TJSONFloatNumber.Clear;
begin
  FValue := 0;
end;

function TJSONFloatNumber.Clone: TJSONData;

begin
  Result := TJSONFloatNumberClass(ClassType).Create(Self.FValue);
end;

{ TJSONIntegerNumber }

function TJSONIntegerNumber.GetAsBoolean: Boolean;
begin
  Result := FValue <> 0;
end;

function TJSONIntegerNumber.GetAsFloat: TJSONFloat;
begin
  Result := FValue;
end;

function TJSONIntegerNumber.GetAsInteger: Integer;
begin
  Result := FValue;
end;

{$ifdef fpc}
function TJSONIntegerNumber.GetAsInt64: Int64;
begin
  Result := FValue;
end;

function TJSONIntegerNumber.GetAsQWord: QWord;
begin
  result := FValue;
end;
{$endif}

procedure TJSONIntegerNumber.SetAsBoolean(const AValue: Boolean);
begin
  FValue := Ord(AValue);
end;

procedure TJSONIntegerNumber.SetAsFloat(const AValue: TJSONFloat);
begin
  FValue := Round(AValue);
end;

procedure TJSONIntegerNumber.SetAsInteger(const AValue: Integer);
begin
  FValue := AValue;
end;

{$ifdef fpc}
procedure TJSONIntegerNumber.SetAsInt64(const AValue: Int64);
begin
  FValue := AValue;
end;

procedure TJSONIntegerNumber.SetAsQword(const AValue: QWord);
begin
  FValue := AValue;
end;
{$endif}

function TJSONIntegerNumber.GetAsJSON: TJSONStringType;
begin
  Result := AsString;
end;

function TJSONIntegerNumber.GetAsString: TJSONStringType;
begin
  Result := IntToStr(FValue)
end;

procedure TJSONIntegerNumber.SetAsString(const AValue: TJSONStringType);
begin
  FValue := StrToInt(AValue);
end;


function TJSONIntegerNumber.GetValue: TJSONVariant;
begin
  Result := FValue;
end;

procedure TJSONIntegerNumber.SetValue(const AValue: TJSONVariant);
begin
  FValue := {$ifdef pas2js}Integer(AValue){$else}AValue{$endif};
end;

constructor TJSONIntegerNumber.Create(AValue: Integer);
begin
  FValue := AValue;
end;

class function TJSONIntegerNumber.NumberType: TJSONNumberType;
begin
  Result := ntInteger;
end;

procedure TJSONIntegerNumber.Clear;
begin
  FValue := 0;
end;

function TJSONIntegerNumber.Clone: TJSONData;

begin
  Result := TJSONIntegerNumberClass(ClassType).Create(Self.FValue);
end;

{$ifdef fpc}
{ TJSONInt64Number }

function TJSONInt64Number.GetAsInt64: Int64;
begin
  Result := FValue;
end;

function TJSONInt64Number.GetAsQWord: QWord;
begin
  Result := FValue;
end;

procedure TJSONInt64Number.SetAsInt64(const AValue: Int64);
begin
  FValue := AValue;
end;

procedure TJSONInt64Number.SetAsQword(const AValue: QWord);
begin
  FValue := AValue;
end;

function TJSONInt64Number.GetAsBoolean: Boolean;
begin
  Result := FValue <> 0;
end;

function TJSONInt64Number.GetAsFloat: TJSONFloat;
begin
  Result := FValue;
end;

function TJSONInt64Number.GetAsInteger: Integer;
begin
  Result := FValue;
end;

procedure TJSONInt64Number.SetAsBoolean(const AValue: Boolean);
begin
  FValue := Ord(AValue);
end;

procedure TJSONInt64Number.SetAsFloat(const AValue: TJSONFloat);
begin
  FValue := Round(AValue);
end;

procedure TJSONInt64Number.SetAsInteger(const AValue: Integer);
begin
  FValue := AValue;
end;

function TJSONInt64Number.GetAsJSON: TJSONStringType;
begin
  Result := AsString;
end;

function TJSONInt64Number.GetAsString: TJSONStringType;
begin
  Result := IntToStr(FValue)
end;

procedure TJSONInt64Number.SetAsString(const AValue: TJSONStringType);
begin
  FValue := StrToInt64(AValue);
end;

function TJSONInt64Number.GetValue: TJSONVariant;
begin
  Result := FValue;
end;

procedure TJSONInt64Number.SetValue(const AValue: TJSONVariant);
begin
  FValue := AValue;
end;

constructor TJSONInt64Number.Create(AValue: Int64);
begin
  FValue := AValue;
end;

class function TJSONInt64Number.NumberType: TJSONNumberType;
begin
  Result := ntInt64;
end;

procedure TJSONInt64Number.Clear;
begin
  FValue := 0;
end;

function TJSONInt64Number.Clone: TJSONData;

begin
  Result := TJSONInt64NumberClass(ClassType).Create(Self.FValue);
end;
{$endif}

{ TJSONArray }

function TJSONArray.GetBooleans(Index: Integer): Boolean;
begin
  Result := Items[Index].AsBoolean;
end;

function TJSONArray.GetArrays(Index: Integer): TJSONArray;
begin
  Result := Items[Index] as TJSONArray;
end;

function TJSONArray.GetFloats(Index: Integer): TJSONFloat;
begin
  Result := Items[Index].AsFloat;
end;

function TJSONArray.GetIntegers(Index: Integer): Integer;
begin
  Result := Items[Index].AsInteger;
end;

{$ifdef fpc}
function TJSONArray.GetInt64s(Index: Integer): Int64;
begin
  Result := Items[Index].AsInt64;
end;
{$endif}

function TJSONArray.GetNulls(Index: Integer): Boolean;
begin
  Result := Items[Index].IsNull;
end;

function TJSONArray.GetObjects(Index: Integer): TJSONObject;
begin
  Result := Items[Index] as TJSONObject;
end;

{$ifdef fpc}
function TJSONArray.GetQWords(Index: Integer): QWord;
begin
  Result := Items[Index].AsQWord;
end;
{$endif}

function TJSONArray.GetStrings(Index: Integer): TJSONStringType;
begin
  Result := Items[Index].AsString;
end;

function TJSONArray.GetTypes(Index: Integer): TJSONType;
begin
  Result := Items[Index].JSONType;
end;

procedure TJSONArray.SetArrays(Index: Integer; const AValue: TJSONArray);
begin
  Items[Index] := AValue;
end;

procedure TJSONArray.SetBooleans(Index: Integer; const AValue: Boolean);

begin
  Items[Index] := CreateJSON(AValue);
end;

procedure TJSONArray.SetFloats(Index: Integer; const AValue: TJSONFloat);
begin
  Items[Index] := CreateJSON(AValue);
end;

procedure TJSONArray.SetIntegers(Index: Integer; const AValue: Integer);
begin
  Items[Index] := CreateJSON(AValue);
end;

{$ifdef fpc}
procedure TJSONArray.SetInt64s(Index: Integer; const AValue: Int64);
begin
  Items[Index] := CreateJSON(AValue);
end;
{$endif}

procedure TJSONArray.SetObjects(Index: Integer; const AValue: TJSONObject);
begin
  Items[Index] := AValue;
end;

{$ifdef fpc}
procedure TJSONArray.SetQWords(Index: Integer; AValue: QWord);
begin
  Items[Index] := CreateJSON(AValue);
end;
{$endif}

procedure TJSONArray.SetStrings(Index: Integer; const AValue: TJSONStringType);
begin
  Items[Index] := CreateJSON(AValue);
end;

function TJSONArray.DoFindPath(const APath: TJSONStringType; out
  NotFound: TJSONStringType): TJSONdata;
var
  P,I: integer;
  E: string;
begin
  if (APath <> '') and (APath[1]='[') then
    begin
    P := Pos(']',APath);
    I := -1;
    if (P>2) then
      I := StrToIntDef(Copy(APath,2,P-2),-1);
    if (I>=0) and (I<Count) then
       begin
       E := APath;
       System.Delete(E,1,P);
       Result := Items[i].DoFindPath(E,NotFound);
       end
    else
       begin
       Result := nil;
       if (P>0) then
         NotFound := Copy(APath,1,P)
       else
         NotFound := APath;
       end;
    end
  else
    Result := inherited DoFindPath(APath, NotFound);
end;

procedure TJSONArray.Converterror(From: Boolean);
begin
  if From then
    DoError(SErrCannotConvertFromArray)
  else
    DoError(SErrCannotConvertToArray);
end;

{$warnings off}
function TJSONArray.GetAsBoolean: Boolean;
begin
  ConvertError(True);
  Result := false;
end;

function TJSONArray.GetAsFloat: TJSONFloat;
begin
  ConvertError(True);
  Result := 0.0;
end;

function TJSONArray.GetAsInteger: Integer;
begin
  ConvertError(True);
  Result := 0;
end;

{$ifdef fpc}
function TJSONArray.GetAsInt64: Int64;
begin
  ConvertError(True);
end;

function TJSONArray.GetAsQWord: QWord;
begin
  ConvertError(True);
end;
{$endif}

procedure TJSONArray.SetAsBoolean(const AValue: Boolean);
begin
  ConvertError(False);
  if AValue then ;
end;

procedure TJSONArray.SetAsFloat(const AValue: TJSONFloat);
begin
  ConvertError(False);
  if AValue>0 then ;
end;

procedure TJSONArray.SetAsInteger(const AValue: Integer);
begin
  ConvertError(False);
  if AValue>0 then ;
end;

{$ifdef fpc}
procedure TJSONArray.SetAsInt64(const AValue: Int64);
begin
  ConvertError(False);
  if AValue>0 then ;
end;

procedure TJSONArray.SetAsQword(const AValue: QWord);
begin
  ConvertError(False);
  if AValue>0 then ;
end;
{$endif}

{$warnings on}


function TJSONArray.GetAsJSON: TJSONStringType;

var
  I: Integer;
  Sep: string;
  D: TJSONData;
  V: TJSONStringType;

begin
  Sep := TJSONData.FElementSep;
  Result := '[';
  for I := 0 to Count-1 do
    begin
    D := Items[i];
    if D <> nil then
      V := D.AsJSON
    else
      V := 'null';
    Result := Result+V;
    if (I<Count-1) then
      Result := Result+Sep;
    end;
  Result := Result+']';
end;

function IndentString(Options: TFormatOptions; Indent: Integer): TJSONStringType;

begin
  if (foUseTabChar in Options) then
    Result := StringofChar(#9,Indent)
  else
    Result := StringOfChar(' ',Indent);
end;

function TJSONArray.DoFormatJSON(Options: TFormatOptions; CurrentIndent,
  Indent: Integer): TJSONStringType;

var
  I: Integer;
  MultiLine: Boolean;
  SkipWhiteSpace: Boolean;
  Ind: string;
  
begin
  Result := '[';
  MultiLine := not (foSingleLineArray in Options);
  SkipWhiteSpace := foSkipWhiteSpace in Options;
  Ind := IndentString(Options, CurrentIndent+Indent);
  if MultiLine then
    Result := Result+sLineBreak;
  for I := 0 to Count-1 do
    begin
    if MultiLine then
      Result := Result+Ind;
    if Items[i]=nil then
      Result := Result+'null'
    else
      Result := Result+Items[i].DoFormatJSON(Options,CurrentIndent+Indent,Indent);
    if (I<Count-1) then
      if MultiLine then
        Result := Result+','
      else
        Result := Result+ElementSeps[SkipWhiteSpace];
    if MultiLine then
      Result := Result+sLineBreak
    end;
  if MultiLine then
    Result := Result+IndentString(Options, CurrentIndent);
  Result := Result+']';
end;


{$warnings off}
function TJSONArray.GetAsString: TJSONStringType;
begin
  ConvertError(True);
  Result := '';
end;

procedure TJSONArray.SetAsString(const AValue: TJSONStringType);
begin
  ConvertError(False);
  if AValue='' then ;
end;

function TJSONArray.GetValue: TJSONVariant;
begin
  ConvertError(True);
  Result := 0;
end;

procedure TJSONArray.SetValue(const AValue: TJSONVariant);
begin
  ConvertError(False);
  {$ifdef pas2js}
  if AValue=0 then ;
  {$else}
  if VarType(AValue)=0 then ;
  {$endif}
end;
{$warnings on}

function TJSONArray.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TJSONArray.GetItem(Index: Integer): TJSONData;
begin
  Result := FList[Index] as TJSONData;
end;

procedure TJSONArray.SetItem(Index: Integer; const AValue: TJSONData);
begin
  if (Index=FList.Count) then
    FList.Add(AValue)
  else
    FList[Index] := AValue;
end;

constructor TJSONArray.Create;
begin
  Flist := TFPObjectList.Create(True);
end;

{$ifdef pas2js}
function VarRecToJSON(const Element: jsvalue; const SourceType: string): TJSONData;
var
  i: NativeInt;
  VObject: TObject;
begin
  Result := nil;
  if Element=nil then
    Result := CreateJSON // TJSONNull
  else if isBoolean(Element) then
    Result := CreateJSON(boolean(Element))
  else if isString(Element) then
    Result := CreateJSON(string(Element))
  else if isNumber(Element) then
    begin
    if isInteger(Element) then
      begin
      i := NativeInt(Element);
      if (i>=low(integer)) and (i<=high(integer)) then
        Result := CreateJSON(Integer(Element))
      else
        Result := CreateJSON(TJSONFloat(Element));
      end
    else
      Result := CreateJSON(TJSONFloat(Element));
    end
  else if isObject(Element) and (Element is TObject) then
    begin
    VObject := TObject(Element);
    if VObject is TJSONData then
      Result := TJSONData(VObject)
    else
      TJSONData.DoError(SErrNotJSONData,[VObject.ClassName,SourceType]);
    end
  else
    TJSONData.DoError(SErrUnknownTypeInConstructor,[SourceType,jsTypeOf(Element)]);
end;
{$else}
function VarRecToJSON(const Element: TVarRec; const SourceType: string): TJSONData;
begin
  Result := nil;
  With Element do
    case VType of
      vtInteger   : Result := CreateJSON(VInteger);
      vtBoolean   : Result := CreateJSON(VBoolean);
      vtWideChar  : Result := CreateJSON(VWideChar);
      vtExtended  : Result := CreateJSON(VExtended^);
      vtUnicodeString: Result := CreateJSON(UnicodeString(VUnicodeString^));
      vtAnsiString: Result := CreateJSON(TJSONStringType(AnsiString(vAnsiString)));
      vtPChar     : Result := CreateJSON(TJSONStringType(AnsiString(VPChar)));
      vtPointer   : if (VPointer <> nil) then
                       TJSONData.DoError(SErrPointerNotNil,[SourceType])
                     else
                       Result := CreateJSON();
      vtCurrency  : Result := CreateJSON(vCurrency^);
      vtInt64     : Result := CreateJSON(vInt64^);
      vtObject    : if (VObject is TJSONData) then
                       Result := TJSONData(VObject)
                     else
                       TJSONData.DoError(SErrNotJSONData,[VObject.ClassName,SourceType]);
      //vtVariant    :
    else
      TJSONData.DoError(SErrUnknownTypeInConstructor,[SourceType,VType])
    end;
end;
{$endif}

constructor TJSONArray.Create(const Elements: array of {$ifdef pas2js}jsvalue{$else}const{$endif});

var
  I: integer;
  J: TJSONData;

begin
  Create;
  for I := Low(Elements) to High(Elements) do
    begin
    J := VarRecToJSON(Elements[i],'Array');
    Add(J);
    end;
end;

destructor TJSONArray.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

class function TJSONArray.JSONType: TJSONType;
begin
  Result := jtArray;
end;

function TJSONArray.Clone: TJSONData;
var
  A: TJSONArray;
  I: Integer;
begin
  A := TJSONArrayClass(ClassType).Create;
  try
    for I := 0 to Count-1 do
      A.Add(Self.Items[I].Clone);
    Result := A;
  except
    A.Free;
    Raise;
  end;
end;

procedure TJSONArray.Iterate(Iterator: TJSONArrayIterator; Data: TObject);

var
  I: Integer;
  Cont: Boolean;
  
begin
  I := 0;
  Cont := True;
  While (I<FList.Count) and cont do
    begin
    Iterator(Items[i],Data,Cont);
    Inc(I);
    end;
end;

function TJSONArray.IndexOf(obj: TJSONData): Integer;
begin
  Result := FList.IndexOf(Obj);
end;

function TJSONArray.GetEnumerator: TBaseJSONEnumerator;
begin
  Result := TJSONArrayEnumerator.Create(Self);
end;

procedure TJSONArray.Clear;
begin
  FList.Clear;
end;

function TJSONArray.Add(Item: TJSONData): Integer;
begin
  Result := FList.Add(Item);
end;

function TJSONArray.Add(I: Integer): Integer;
begin
  Result := Add(CreateJSON(I));
end;

{$ifdef fpc}
function TJSONArray.Add(I: Int64): Int64;
begin
  Result := Add(CreateJSON(I));
end;

function TJSONArray.Add(I: QWord): QWord;
begin
  Result := Add(CreateJSON(I));
end;
{$endif}

function TJSONArray.Add(const S: string): Integer;
begin
  Result := Add(CreateJSON(S));
end;

function TJSONArray.Add: Integer;
begin
  Result := Add(CreateJSON);
end;

function TJSONArray.Add(F: TJSONFloat): Integer;
begin
  Result := Add(CreateJSON(F));
end;

function TJSONArray.Add(B: Boolean): Integer;
begin
  Result := Add(CreateJSON(B));
end;

function TJSONArray.Add(AnArray: TJSONArray): Integer;
begin
  if (IndexOf(AnArray) <> -1) then
    DoError(SErrCannotAddArrayTwice);
  Result := Add(TJSONData(AnArray));
end;

function TJSONArray.Add(AnObject: TJSONObject): Integer;
begin
  if (IndexOf(AnObject) <> -1) then
    DoError(SErrCannotAddObjectTwice);
  Result := Add(TJSONData(AnObject));
end;

procedure TJSONArray.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

procedure TJSONArray.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
end;

function TJSONArray.Extract(Item: TJSONData): TJSONData;
begin
  Result := TJSONData(FList.Extract(Item));
end;

function TJSONArray.Extract(Index: Integer): TJSONData;
begin
  Result := TJSONData(FList.Extract(FList.Items[Index]));
end;

procedure TJSONArray.Insert(Index: Integer);
begin
  Insert(Index,CreateJSON);
end;

procedure TJSONArray.Insert(Index: Integer; Item: TJSONData);
begin
  FList.Insert(Index, Item);
end;

procedure TJSONArray.Insert(Index: Integer; I: Integer);
begin
  FList.Insert(Index, CreateJSON(I));
end;

{$ifdef fpc}
procedure TJSONArray.Insert(Index: Integer; I: Int64);
begin
  FList.Insert(Index, CreateJSON(I));
end;

procedure TJSONArray.Insert(Index: Integer; I: QWord);
begin
  FList.Insert(Index, CreateJSON(I));
end;
{$endif}

procedure TJSONArray.Insert(Index: Integer; const S: string);
begin
  FList.Insert(Index, CreateJSON(S));
end;

procedure TJSONArray.Insert(Index: Integer; F: TJSONFloat);
begin
  FList.Insert(Index, CreateJSON(F));
end;

procedure TJSONArray.Insert(Index: Integer; B: Boolean);
begin
  FList.Insert(Index, CreateJSON(B));
end;

procedure TJSONArray.Insert(Index: Integer; AnArray: TJSONArray);
begin
  if (IndexOf(AnArray) <> -1) then
    DoError(SErrCannotAddArrayTwice);
  FList.Insert(Index, AnArray);
end;

procedure TJSONArray.Insert(Index: Integer; AnObject: TJSONObject);
begin
  if (IndexOf(AnObject) <> -1) then
    DoError(SErrCannotAddObjectTwice);
  FList.Insert(Index, AnObject);
end;

procedure TJSONArray.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

procedure TJSONArray.Remove(Item: TJSONData);
begin
  FList.Remove(Item);
end;

procedure TJSONArray.Sort(Compare: TListSortCompare);
begin
  FList.Sort(Compare);
end;

{ TJSONObject }

function TJSONObject.GetArrays(const AName: string): TJSONArray;
begin
  Result := GetElements(AName) as TJSONArray;
end;

function TJSONObject.GetBooleans(const AName: string): Boolean;
begin
  Result := GetElements(AName).AsBoolean;
end;

function TJSONObject.GetElements(const AName: string): TJSONData;
begin
  {$ifdef pas2js}
  if FHash.hasOwnProperty('%'+AName) then
    Result := TJSONData(FHash['%'+AName])
  else
    DoError(SErrNonexistentElement,[AName]);
  {$else}
  Result := TJSONData(FHash[AName]);
  if (Result=nil) then
    DoError(SErrNonexistentElement,[AName]);
  {$endif}
end;

function TJSONObject.GetFloats(const AName: string): TJSONFloat;
begin
  Result := GetElements(AName).AsFloat;
end;

function TJSONObject.GetIntegers(const AName: string): Integer;
begin
  Result := GetElements(AName).AsInteger;
end;

{$ifdef fpc}
function TJSONObject.GetInt64s(const AName: string): Int64;
begin
  Result := GetElements(AName).AsInt64;
end;
{$endif}

function TJSONObject.GetIsNull(const AName: string): Boolean;
begin
  Result := GetElements(AName).IsNull;
end;

function TJSONObject.GetObjects(const AName: string): TJSONObject;
begin
  Result := GetElements(AName) as TJSONObject;
end;

{$ifdef fpc}
function TJSONObject.GetQWords(AName: string): QWord;
begin
  Result := GetElements(AName).AsQWord;
end;
{$endif}

function TJSONObject.GetStrings(const AName: string): TJSONStringType;
begin
  Result := GetElements(AName).AsString;
end;

function TJSONObject.GetTypes(const AName: string): TJSONType;
begin
  Result := Getelements(Aname).JSONType;
end;

class function TJSONObject.GetUnquotedMemberNames: Boolean;
begin
  Result := FUnquotedMemberNames;
end;

procedure TJSONObject.SetArrays(const AName: string; const AValue: TJSONArray);

begin
  SetElements(AName,AVAlue);
end;

procedure TJSONObject.SetBooleans(const AName: string; const AValue: Boolean);
begin
  SetElements(AName,CreateJSON(AVAlue));
end;

procedure TJSONObject.SetElements(const AName: string; const AValue: TJSONData);
begin
{$ifdef pas2js}
  if not FHash.hasOwnProperty('%'+AName) then
    inc(FCount);
  FHash['%'+AName] := AValue;
  FNames := nil;
{$else}
  FHash.AddOrSetValue(AName, AValue);
end;
{$endif}

procedure TJSONObject.SetFloats(const AName: string; const AValue: TJSONFloat);
begin
  SetElements(AName,CreateJSON(AVAlue));
end;

procedure TJSONObject.SetIntegers(const AName: string; const AValue: Integer);
begin
  SetElements(AName,CreateJSON(AVAlue));
end;

{$ifdef fpc}
procedure TJSONObject.SetInt64s(const AName: string; const AValue: Int64);
begin
  SetElements(AName,CreateJSON(AVAlue));
end;
{$endif}

procedure TJSONObject.SetIsNull(const AName: string; const AValue: Boolean);
begin
  if not AValue then
    DoError(SErrCannotSetNotIsNull);
  SetElements(AName,CreateJSON);
end;

procedure TJSONObject.SetObjects(const AName: string; const AValue: TJSONObject);
begin
  SetElements(AName,AValue);
end;

{$ifdef fpc}
procedure TJSONObject.SetQWords(AName: string; AValue: QWord);
begin
  SetElements(AName,CreateJSON(AVAlue));
end;
{$endif}

procedure TJSONObject.SetStrings(const AName: string; const AValue: TJSONStringType);
begin
  SetElements(AName,CreateJSON(AValue));
end;

class procedure TJSONObject.DetermineElementQuotes;
begin
  FObjStartSep := ObjStartSeps[TJSONData.FCompressedJSON];
  FObjEndSep := ObjEndSeps[TJSONData.FCompressedJSON];
  if TJSONData.FCompressedJSON then
    FElementEnd := UnSpacedQuoted[FUnquotedMemberNames]
  else
    FElementEnd := SpacedQuoted[FUnquotedMemberNames];
  FElementStart := ElementStart[FUnquotedMemberNames]
end;

class procedure TJSONObject.SetUnquotedMemberNames(AValue: Boolean);
begin
  if FUnquotedMemberNames=AValue then Exit;
  FUnquotedMemberNames := AValue;
  DetermineElementQuotes;
end;

function TJSONObject.DoFindPath(const APath: TJSONStringType; out
  NotFound: TJSONStringType): TJSONdata;

var
  N: TJSONStringType;
  L,P,P2: Integer;

begin
  if (APath='') then
    Exit(Self);
  N := APath;
  L := Length(N);
  P := 1;
  While (P<L) and (N[P]='.') do
    inc(P);
  P2 := P;
  While (P2<=L) and (not (N[P2] in ['.','['])) do
    inc(P2);
   N := Copy(APath,P,P2-P);
   if (N='') then
     Result := Self
   else
     begin
     Result := Find(N);
     if Result=nil then
       NotFound := N+Copy(APath,P2,L-P2)
     else
       begin
       N := Copy(APath,P2,L-P2+1);
       Result := Result.DoFindPath(N,NotFound);
       end;
     end;
end;

procedure TJSONObject.Converterror(From: Boolean);
begin
  if From then
    DoError(SErrCannotConvertFromObject)
  else
    DoError(SErrCannotConvertToObject);
end;

{$warnings off}
function TJSONObject.GetAsBoolean: Boolean;
begin
  ConvertError(True);
  Result := false;
end;

function TJSONObject.GetAsFloat: TJSONFloat;
begin
  ConvertError(True);
  Result := 0.0;
end;

function TJSONObject.GetAsInteger: Integer;
begin
  ConvertError(True);
  Result := 0;
end;

{$ifdef fpc}
function TJSONObject.GetAsInt64: Int64;
begin
  ConvertError(True);
end;

function TJSONObject.GetAsQWord: QWord;
begin
  ConvertError(True);
end;
{$endif}

procedure TJSONObject.SetAsBoolean(const AValue: Boolean);
begin
  ConvertError(False);
  if AValue then ;
end;

procedure TJSONObject.SetAsFloat(const AValue: TJSONFloat);
begin
  ConvertError(False);
  if AValue>0 then ;
end;

procedure TJSONObject.SetAsInteger(const AValue: Integer);
begin
  ConvertError(False);
  if AValue>0 then ;
end;

{$ifdef fpc}
procedure TJSONObject.SetAsInt64(const AValue: Int64);
begin
  ConvertError(False);
  if AValue>0 then ;
end;

procedure TJSONObject.SetAsQword(const AValue: QWord);
begin
  ConvertError(False);
  if AValue>0 then ;
end;
{$endif}

{$warnings on}
function TJSONObject.GetAsJSON: TJSONStringType;
var
  sep: string;
  v: TJSONStringType;
  item: TPair<string, TJSONData>;
begin
  sep := TJSONData.FElementSep;
  Result := '';
  for item in FHash do begin
    if Result <> '' then
      Result := Result + sep;
    if Assigned(item.Value) then
      v := item.Value.AsJSON
    else
      v := 'null';
    Result := Result + FElementStart + StringToJSONString(item.Key) + FElementEnd + v;
  end;
  if (Result <> '') then
    Result := FObjStartSep+Result+FObjEndSep
  else
    Result := '{}';
end;
{$warnings off}

function TJSONObject.GetAsString: TJSONStringType;
begin
  ConvertError(True);
  Result := '';
end;

procedure TJSONObject.SetAsString(const AValue: TJSONStringType);
begin
  ConvertError(False);
  if AValue='' then ;
end;

function TJSONObject.GetValue: TJSONVariant;
begin
  ConvertError(True);
  Result := 0;
end;

procedure TJSONObject.SetValue(const AValue: TJSONVariant);
begin
  ConvertError(False);
  {$ifdef pas2js}
  if AValue=0 then ;
  {$else}
  if VarType(AValue)=0 then ;
  {$endif}
end;
{$warnings on}

function TJSONObject.GetCount: Integer;
begin
  {$ifdef pas2js}
  Result := FCount;
  {$else}
  Result := FHash.Count;
  {$endif}
end;

constructor TJSONObject.Create;
begin
  {$ifdef pas2js}
  FHash := TJSObject.new;
  {$else}
  FHash := TFPHashObjectList.Create;
  {$endif}
end;

constructor TJSONObject.Create(const Elements: array of {$ifdef pas2js}jsvalue{$else}const{$endif});
var
  I: integer;
  AName: string;
  J: TJSONData;
begin
  Create;
  if ((High(Elements)-Low(Elements)) mod 2)=0 then
    DoError(SErrOddNumber);
  I := Low(Elements);
  While I<=High(Elements) do
    begin
    {$ifdef pas2js}
    if isString(Elements[I]) then
      AName := string(Elements[I])
    else
      DoError(SErrNameMustBeString,[I+1]);
    {$else}
    With Elements[i] do
      Case VType of
        vtWideChar: AName := VWideChar;
        vtUnicodeString: AName := UnicodeString(VUnicodeString^);
        vtPWideChar: AName := UnicodeString(VPChar);
      else
        DoError(SErrNameMustBeString,[I+1]);
      end;
    {$endif}
    if (AName='') then
      DoError(SErrNameMustBeString,[I+1]);
    Inc(I);
    J := VarRecToJSON(Elements[i],'Object');
    Add(AName,J);
    Inc(I);
    end;
end;

destructor TJSONObject.Destroy;
begin
  {$ifdef pas2js}
  FHash := nil;
  {$else}
  FreeAndNil(FHash);
  {$endif}
  inherited Destroy;
end;

class function TJSONObject.JSONType: TJSONType;
begin
  Result := jtObject;
end;

function TJSONObject.Clone: TJSONData;
var
  o: TJSONObject;
  item: TPair<string, TJSONData>;
begin
  o := TJSONObjectClass(ClassType).Create;
  try
    for item in FHash do
      o.Add(item.Key, item.Value.Clone);
    Result := o;
  except
    FreeAndNil(o);
    Raise;
  end;
end;

function TJSONObject.DoFormatJSON(Options: TFormatOptions; CurrentIndent,
  Indent: Integer): TJSONStringType;
var
  s: TJSONStringType;
  multiLine, useQuotes, skipWhiteSpace, skipWhiteSpaceOnlyLeading: Boolean;
  nSep, sep, ind: string;
  v: TJSONStringType;
  item: TPair<string, TJSONData>;
  first: Boolean;
begin
  Result := '';
  useQuotes := not (foDoNotQuoteMembers in options);
  multiLine := not (foSingleLineObject in Options);
  skipWhiteSpace := foSkipWhiteSpace in Options;
  skipWhiteSpaceOnlyLeading := foSkipWhiteSpaceOnlyLeading in Options;
  CurrentIndent := CurrentIndent + Indent;
  ind := IndentString(Options, CurrentIndent);
  if skipWhiteSpace then begin
    if skipWhiteSpaceOnlyLeading then
      nSep := ': '
    else
      nSep := ':'
    end
  else
    nSep := ': ';
  if multiLine then
    sep := ','+SLineBreak+ind
  else if skipWhiteSpace then
    sep := ','
  else
    sep := ', ';
  first := True;
  for item in FHash do begin
    if first then begin
      Result := Result + sep;
      first := False
    end else if multiLine then
      Result := Result + ind;
    s := StringToJSONString(item.Key);
    if useQuotes then
      s := '"'+s+'"';
    if item.Value = nil then
      v := 'null'
    else
      v := item.Value.DoFormatJSON(Options, CurrentIndent, Indent);
    Result := Result + s + nSep + v;
  end;
  if Result <> '' then begin
    if multiLine then
      Result := '{' + sLineBreak + Result + sLineBreak + indentString(options,
        CurrentIndent-Indent)+'}'
    else
      Result := ObjStartSeps[skipWhiteSpace] + Result + ObjEndSeps[skipWhiteSpace]
  end else
    Result := '{}';
end;

procedure TJSONObject.Clear;
begin
  {$ifdef pas2js}
  FCount := 0;
  FHash := TJSObject.new;
  FNames := nil;
  {$else}
  FHash.Clear;
  {$endif}
end;

procedure TJSONObject.DoAdd(const AName: TJSONStringType; AValue: TJSONData;
  FreeOnError: Boolean = True);
begin
  if {$ifdef pas2js}FHash.hasOwnProperty('%'+AName)
    {$else}FHash.ContainsKey(aName){$endif}
  then begin
    if FreeOnError then
      FreeAndNil(AValue);
    DoError(SErrDuplicateValue,[aName]);
  end;
  {$ifdef pas2js}
  FHash['%'+AName] := AValue;
  FNames := nil;
  inc(FCount);
  Result := FCount;
  {$else}
  FHash.Add(AName, AValue);
  {$endif}
end;

procedure TJSONObject.Add(const AName: TJSONStringType; AValue: TJSONData);
begin
  DoAdd(aName,AValue,False);
end;

procedure TJSONObject.Add(const AName: TJSONStringType; AValue: Boolean);
begin
  DoAdd(AName,CreateJSON(AValue));
end;

procedure TJSONObject.Add(const AName: TJSONStringType; AValue: TJSONFloat);
begin
  DoAdd(AName,CreateJSON(AValue));
end;

procedure TJSONObject.Add(const AName, AValue: TJSONStringType);
begin
  DoAdd(AName,CreateJSON(AValue));
end;

procedure TJSONObject.Add(const AName: TJSONStringType; Avalue: Integer);
begin
  DoAdd(AName,CreateJSON(AValue));
end;

{$ifdef fpc}
procedure TJSONObject.Add(const AName: TJSONStringType; Avalue: Int64);
begin
  DoAdd(AName,CreateJSON(AValue));
end;

procedure TJSONObject.Add(const AName: TJSONStringType; Avalue: QWord);
begin
  DoAdd(AName,CreateJSON(AValue));
end;
{$endif}

procedure TJSONObject.Add(const AName: TJSONStringType);
begin
  DoAdd(AName,CreateJSON);
end;

procedure TJSONObject.Add(const AName: TJSONStringType; AValue: TJSONArray);
begin
  DoAdd(AName,TJSONData(AValue),False);
end;

procedure TJSONObject.Delete(const AName: string);
begin
{$ifdef pas2js}
  if not FHash.hasOwnProperty('%'+AName) then Exit;
  JSDelete(FHash,'%'+AName);
  FNames := nil;
  dec(FCount);
{$else}
  FHash.Remove(AName);
{$endif}
end;

procedure TJSONObject.Remove(Value: TJSONData);
{$ifdef pas2js}
var AName: string;
begin
  for AName in FHash do
    if FHash.hasOwnProperty(AName) and (FHash[AName]=Value) then
      begin
      JSDelete(FHash,AName);
      FNames := nil;
      dec(FCount);
      Exit;
      end;
end;
{$else}
var
  item: TPair<string, TJSONData>;
begin
  for item in FHash do
    if item.Value = Value then begin
      FHash.Remove(item.Key);
      Exit
    end;
end;
{$endif}

{$ifdef fpc}
function TJSONObject.Extract(const AName: string): TJSONData;
begin
  if not FHash.TryGetValue(AName, Result) then
    Result := nil
  else
    FHash.Remove(AName);
end;
{$endif}

function TJSONObject.Get(const AName: string): TJSONVariant;
{$ifdef pas2js}
begin
  if FHash.hasOwnProperty('%'+AName) then
    Result := TJSONData(FHash['%'+AName]).Value
  else
    Result := nil;
end;
{$else}
var
  d: TJSONData;
begin
  if FHash.TryGetValue(AName, d) then
    Result := d.Value
  else
    Result := Null
end;
{$endif}

function TJSONObject.Get(const AName: string; ADefault: TJSONFloat
  ): TJSONFloat;

var
  D: TJSONData;

begin
  D := Find(AName,jtNumber);
  if D <> nil then
    Result := D.AsFloat
  else
    Result := ADefault;
end;

{$ifdef fpc}
function TJSONObject.Get(const AName: string; ADefault: Int64): Int64;
var
  D: TJSONData;

begin
  D := Find(AName,jtNumber);
  if D <> nil then
    Result := D.AsInt64
  else
    Result := ADefault;
end;

function TJSONObject.Get(const AName: string; ADefault: QWord): QWord;
var
  D: TJSONData;

begin
  D := Find(AName,jtNumber);
  if D <> nil then
    Result := D.AsQWord
  else
    Result := ADefault;
end;
{$endif}

function TJSONObject.Get(const AName: string; ADefault: Boolean
  ): Boolean;
var
  D: TJSONData;

begin
  D := Find(AName,jtBoolean);
  if D <> nil then
    Result := D.AsBoolean
  else
    Result := ADefault;
end;

function TJSONObject.Get(const AName: string; ADefault: TJSONStringType
  ): TJSONStringType;
var
  D: TJSONData;

begin
  D := Find(AName,jtString);
  if (D <> nil) then
    Result := D.AsString
  else
    Result := ADefault;
end;

function TJSONObject.Get(const AName: string; ADefault: TJSONArray
  ): TJSONArray;
var
  D: TJSONData;

begin
  D := Find(AName,jtArray);
  if (D <> nil) then
    Result := TJSONArray(D)
  else
    Result := ADefault;
end;

function TJSONObject.Get(const AName: string; ADefault: TJSONObject
  ): TJSONObject;
var
  D: TJSONData;

begin
  D := Find(AName,jtObject);
  if (D <> nil) then
    Result := TJSONObject(D)
  else
    Result := ADefault;
end;

function TJSONObject.Find(const AName: string): TJSONData;
{$ifdef pas2js}
begin
  if FHash.hasOwnProperty('%'+AName) then
    Result := TJSONData(FHash['%'+AName])
  else
    Result := nil;
end;
{$else}
begin
  if not FHash.TryGetValue(AName, Result) then
    Result := nil
end;
{$endif}

function TJSONObject.Find(const AName: string; AType: TJSONType): TJSONData;
begin
  Result := Find(AName);
  if Assigned(Result) and (Result.JSONType <> AType) then
    Result := nil;
end;

function TJSONObject.Find(const key: TJSONStringType; out AValue: TJSONData): boolean;
begin
  AValue := Find(key);
  Result := assigned(AValue);
end;

function TJSONObject.Find(const key: TJSONStringType; out AValue: TJSONObject): boolean;
var
  v: TJSONData;
begin
  v := Find(key);
  Result := assigned(v) and (v.JSONType = jtObject);
  if Result then
    AValue := TJSONObject(v);
end;

function TJSONObject.Find(const key: TJSONStringType; out AValue: TJSONArray): boolean;
var
  v: TJSONData;
begin
  v := Find(key);
  Result := assigned(v) and (v.JSONType = jtArray);
  if Result then
    AValue := TJSONArray(v);
end;

function TJSONObject.Find(const key: TJSONStringType; out AValue: TJSONString): boolean;
var
  v: TJSONData;
begin
  v := Find(key);
  Result := assigned(v) and (v.JSONType = jtString);
  if Result then
    AValue := TJSONString(v);
end;

function TJSONObject.Find(const key: TJSONStringType; out AValue: TJSONBoolean): boolean;
var
  v: TJSONData;
begin
  v := Find(key);
  Result := assigned(v) and (v.JSONType = jtBoolean);
  if Result then
    AValue := TJSONBoolean(v);
end;

function TJSONObject.Find(const key: TJSONStringType; out AValue: TJSONNumber): boolean;
var
  v: TJSONData;
begin
  v := Find(key);
  Result := assigned(v) and (v.JSONType = jtNumber);
  if Result then
    AValue := TJSONNumber(v);
end;

initialization
  // Need to force initialization;
  TJSONData.DetermineElementSeparators;
  TJSONObject.DetermineElementQuotes;
end.

