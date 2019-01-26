{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2018 by Mattias Gaertner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit System;

{$mode objfpc}
{$modeswitch externalclass}

interface

{$IFDEF NodeJS}
var
  LineEnding: string = #10;
  sLineBreak: string = #10;
{$ELSE}
const
  LineEnding = #10;
  sLineBreak = LineEnding;
{$ENDIF}

Var
  PathDelim : Char = '/';
  AllowDirectorySeparators : Set of Char = ['/'];
  AllowDriveSeparators : Set of Char = [':'];
  ExtensionSeparator : Char = '.';

const
  MaxSmallint = 32767;
  MinSmallint = -32768;
  MaxShortInt = 127;
  MinShortInt = -128;
  MaxByte = $FF;
  MaxWord = $FFFF;
  MaxLongint  = $7fffffff;
  MaxCardinal = LongWord($ffffffff);

  Maxint = MaxLongint;
  IsMultiThread = false;

{*****************************************************************************
                               Base types
*****************************************************************************}
type
  Integer = LongInt;
  Cardinal = LongWord;
  DWord = LongWord;
  SizeInt = NativeInt;
  SizeUInt = NativeUInt;
  PtrInt = NativeInt;
  PtrUInt = NativeUInt;
  ValSInt = NativeInt;
  ValUInt = NativeUInt;
  ValReal = Double;
  Real = type Double;
  Extended = type Double;

  TDateTime = type double;
  TTime = type TDateTime;
  TDate = type TDateTime;

  Int64 = type NativeInt unimplemented; // only 53 bits at runtime
  UInt64 = type NativeUInt unimplemented; // only 52 bits at runtime
  QWord = type NativeUInt unimplemented; // only 52 bits at runtime
  Single = type Double unimplemented;
  Comp = type NativeInt unimplemented;
  NativeLargeInt = NativeInt;
  NativeLargeUInt = NativeUInt;

  UnicodeString = type String;
  WideString = type String;
  WideChar = char;
  UnicodeChar = char;

  TDynArrayIndex = NativeInt;
  TTextLineBreakStyle = (tlbsLF,tlbsCRLF,tlbsCR);

{*****************************************************************************
            TObject, TClass, IUnknown, IInterface, TInterfacedObject
*****************************************************************************}
type
  TGuid = record
    D1: DWord;
    D2: word;
    D3: word;
    D4: array[0..7] of byte;
  end;
  TGUIDString = type string;

  TClass = class of TObject;

  { TObject }

  TObject = class
  private
    class var FClassName: String; external name '$classname';
    class var FClassParent: TClass; external name '$ancestor';
    class var FUnitName: String; external name '$module.$name';
  public
    constructor Create;
    destructor Destroy; virtual;

    // Free is using compiler magic.
    // Reasons:
    // 1. In JS calling obj.Free when obj=nil would crash.
    // 2. In JS freeing memory requires to set all references to nil.
    // Therefore any obj.free call is replaced by the compiler with some rtl magic.
    procedure Free;

    class function ClassType: TClass; assembler;
    class property ClassName: String read FClassName;
    class function ClassNameIs(const Name: string): boolean;
    class property ClassParent: TClass read FClassParent;
    class function InheritsFrom(aClass: TClass): boolean; assembler;
    class property UnitName: String read FUnitName;

    procedure AfterConstruction; virtual;
    procedure BeforeDestruction; virtual;

    function GetInterface(const iid: TGuid; out obj): boolean;
    function GetInterface(const iidstr: String; out obj): boolean; inline;
    function GetInterfaceByStr(const iidstr: String; out obj): boolean;
    function GetInterfaceWeak(const iid: TGuid; out obj): boolean; // equal to GetInterface but the interface returned is not referenced

    function Equals(Obj: TObject): boolean; virtual;
    function ToString: String; virtual;
  end;

const
  { IInterface }
  S_OK          = 0;
  S_FALSE       = 1;
  E_NOINTERFACE = -2147467262; // FPC: longint($80004002)
  E_UNEXPECTED  = -2147418113; // FPC: longint($8000FFFF)
  E_NOTIMPL     = -2147467263; // FPC: longint($80004001)

type
  {$Interfaces COM}
  IUnknown = interface
    ['{00000000-0000-0000-C000-000000000046}']
    function QueryInterface(const iid: TGuid; out obj): Integer;
    function _AddRef: Integer;
    function _Release: Integer;
  end;
  IInterface = IUnknown;

  {$M+}
  IInvokable = interface(IInterface)
  end;
  {$M-}

  { Enumerator support }
  IEnumerator = interface(IInterface)
    function GetCurrent: TObject;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: TObject read GetCurrent;
  end;

  IEnumerable = interface(IInterface)
    function GetEnumerator: IEnumerator;
  end;

  { TInterfacedObject }

  TInterfacedObject = class(TObject,IUnknown)
  protected
    fRefCount: Integer;
    { implement methods of IUnknown }
    function QueryInterface(const iid: TGuid; out obj): Integer; virtual;
    function _AddRef: Integer; virtual;
    function _Release: Integer; virtual;
  public
    procedure BeforeDestruction; override;
    property RefCount: Integer read fRefCount;
  end;
  TInterfacedClass = class of TInterfacedObject;

  { TAggregatedObject - sub or satellite object using same interface as controller }

  TAggregatedObject = class(TObject)
  private
    fController: Pointer;
    function GetController: IUnknown;
  protected
    { implement methods of IUnknown }
    function QueryInterface(const iid: TGuid; out obj): Integer; virtual;
    function _AddRef: Integer; virtual;
    function _Release: Integer; virtual;
  public
    constructor Create(const aController: IUnknown); reintroduce;
    property Controller: IUnknown read GetController;
  end;

  { TContainedObject }

  TContainedObject = class(TAggregatedObject,IInterface)
  protected
    function QueryInterface(const iid: TGuid; out obj): Integer; override;
  end;

const
  { for safe as operator support }
  IObjectInstance: TGuid = '{D91C9AF4-3C93-420F-A303-BF5BA82BFD23}';

function GUIDToString(const GUID: TGUID): string; external name 'rtl.guidrToStr';

{*****************************************************************************
                            Init / Exit / ExitProc
*****************************************************************************}
var
  ExitCode: Integer; external name 'rtl.exitcode';
  IsConsole: Boolean = {$IFDEF NodeJS}true{$ELSE}false{$ENDIF};
  FirstDotAtFileNameStartIsExtension : Boolean = False;

type
  TOnParamCount = function: Longint;
  TOnParamStr = function(Index: Longint): String;
var
  OnParamCount: TOnParamCount;
  OnParamStr: TOnParamStr;

function ParamCount: Longint;
function ParamStr(Index: Longint): String;

{*****************************************************************************
                                 Math
*****************************************************************************}
const
  PI: Double; external name 'Math.PI';
  MathE: Double; external name 'Math.E'; // Euler's number
  MathLN10: Double; external name 'Math.LN10'; // ln(10)
  MathLN2: Double; external name 'Math.LN2'; // ln(2)
  MathLog10E: Double; external name 'Math.Log10E'; // log10(e)
  MathLog2E: Double; external name 'Math.LOG2E'; // log2(e)
  MathSQRT1_2: Double; external name 'Math.SQRT1_2'; // sqrt(0.5)
  MathSQRT2: Double; external name 'Math.SQRT2'; // sqrt(2)

function Abs(const A: integer): integer; overload; external name 'Math.abs';
function Abs(const A: NativeInt): integer; overload; external name 'Math.abs';
function Abs(const A: Double): Double; overload; external name 'Math.abs';
function ArcTan(const A, B: Double): Double; external name 'Math.atan';
function Cos(const A: Double): Double; external name 'Math.cos';
function Exp(const A: Double): Double; external name 'Math.exp';
function Frac(const A: Double): Double; assembler;
function Ln(const A: Double): Double; external name 'Math.log';
function Odd(const A: Integer): Boolean; assembler;
function Random(const Range: Integer): Integer; overload; assembler;
function Random: Double; overload; external name 'Math.random';
function Round(const A: Double): NativeInt; external name 'Math.round';
function Sin(const A: Double): Double; external name 'Math.sin';
function Sqr(const A: Integer): Integer; assembler; overload;
function Sqr(const A: Double): Double; assembler; overload;
function sqrt(const A: Double): Double; external name 'Math.sqrt';
function Trunc(const A: Double): NativeInt;

{*****************************************************************************
                          String functions
*****************************************************************************}
const
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsLF;

function Int(const A: Double): double;
function Copy(const S: string; Index, Size: Integer): String; assembler; overload;
function Copy(const S: string; Index: Integer): String; assembler; overload;
procedure Delete(var S: String; Index, Size: Integer); assembler; overload;
function Pos(const Search, InString: String): Integer; assembler; overload;
function Pos(const Search, InString: String; StartAt : Integer): Integer; assembler; overload;
procedure Insert(const Insertion: String; var Target: String; Index: Integer); overload;
function upcase(c : char) : char; assembler;
function HexStr(Val: NativeInt; cnt: byte): string; external name 'rtl.hexStr'; overload;

procedure val(const S: String; out NI : NativeInt; out Code: Integer); overload;
procedure val(const S: String; out NI : NativeUInt; out Code: Integer); overload;
procedure val(const S: String; out SI : ShortInt; out Code: Integer); overload;
procedure val(const S: String; out B : Byte; out Code: Integer); overload;
procedure val(const S: String; out SI : smallint; out Code: Integer); overload;
procedure val(const S: String; out W : word; out Code : Integer); overload;
procedure val(const S: String; out I : integer; out Code : Integer); overload;
procedure val(const S: String; out C : Cardinal; out Code: Integer); overload;
procedure val(const S: String; out d : double; out Code : Integer); overload;
procedure val(const S: String; out b : boolean; out Code: Integer); overload;
function StringOfChar(c: Char; l: NativeInt): String;

{*****************************************************************************
                          Other functions
*****************************************************************************}
procedure Write; varargs; // ToDo: should be compiler built-in function
procedure Writeln; varargs; // ToDo: should be compiler built-in function

Type
  TConsoleHandler = Procedure (S : JSValue; NewLine : Boolean);
Function SetWriteCallBack(H : TConsoleHandler) : TConsoleHandler;

function Assigned(const V: JSValue): boolean; assembler; overload;
function StrictEqual(const A: JSValue; const B): boolean; assembler;
function StrictInequal(const A: JSValue; const B): boolean; assembler;

implementation

type

  { TJSObj - simple access to JS Object }

  TJSObj = class external name 'Object'
  private
    function GetProperties(Name: String): JSValue; external name '[]';
    procedure SetProperties(Name: String; const AValue: JSValue); external name '[]';
  public
    //constructor new;
    //function hasOwnProperty(prop: String): boolean;
    property Properties[Name: String]: JSValue read GetProperties write SetProperties; default;
  end;

// function parseInt(s: String; Radix: NativeInt): NativeInt; external name 'parseInt'; // may result NaN
function isNaN(i: JSValue): boolean; external name 'isNaN'; // may result NaN

// needed by ClassNameIs, the real SameText is in SysUtils
function SameText(const s1, s2: String): Boolean; assembler;
asm
  return s1.toLowerCase() == s2.toLowerCase();
end;

function ParamCount: Longint;
begin
  if Assigned(OnParamCount) then
    Result:=OnParamCount()
  else
    Result:=0;
end;

function ParamStr(Index: Longint): String;
begin
  if Assigned(OnParamStr) then
    Result:=OnParamStr(Index)
  else if Index=0 then
    Result:='js'
  else
    Result:='';
end;


function Frac(const A: Double): Double; assembler;
asm
  return A % 1;
end;

function Odd(const A: Integer): Boolean; assembler;
asm
  return A&1 != 0;
end;

function Random(const Range: Integer): Integer; assembler;
asm
  return Math.floor(Math.random()*Range);
end;

function Sqr(const A: Integer): Integer; assembler;
asm
  return A*A;
end;

function Sqr(const A: Double): Double; assembler;
asm
  return A*A;
end;

function Trunc(const A: Double): NativeInt; assembler;
asm
  if (!Math.trunc) {
    Math.trunc = function(v) {
      v = +v;
      if (!isFinite(v)) return v;
      return (v - v % 1) || (v < 0 ? -0 : v === 0 ? v : 0);
    };
  }
  $mod.Trunc = Math.trunc;
  return Math.trunc(A);
end;

function Copy(const S: string; Index, Size: Integer): String; assembler;
asm
  if (Index<1) Index = 1;
  return (Size>0) ? S.substring(Index-1,Index+Size-1) : "";
end;

function Copy(const S: string; Index: Integer): String; assembler;
asm
  if (Index<1) Index = 1;
  return S.substr(Index-1);
end;

procedure Delete(var S: String; Index, Size: Integer);
var
  h: String;
begin
  if (Index<1) or (Index>length(S)) or (Size<=0) then exit;
  h:=S;
  S:=copy(h,1,Index-1)+copy(h,Index+Size);
end;

function Pos(const Search, InString: String): Integer; assembler;
asm
  return InString.indexOf(Search)+1;
end;

function Pos(const Search, InString: String; StartAt : Integer): Integer; assembler; overload;
asm
  return InString.indexOf(Search,StartAt-1)+1;
end;

procedure Insert(const Insertion: String; var Target: String; Index: Integer);
var
  t: String;
begin
  if Insertion='' then exit;
  t:=Target;
  if Index<1 then
    Target:=Insertion+t
  else if Index>length(t) then
    Target:=t+Insertion
  else
    Target:=copy(t,1,Index-1)+Insertion+copy(t,Index,length(t));
end;

var
  WriteBuf: String;
  JSArguments: array of JSValue; external name 'arguments';
  WriteCallBack : TConsoleHandler;

Function SetWriteCallBack(H : TConsoleHandler) : TConsoleHandler;

begin
  Result:=WriteCallBack;
  WriteCallBack:=H;
end;

procedure Write;
var
  i: Integer;
begin
  for i:=0 to length(JSArguments)-1 do
    if Assigned(WriteCallBack) then
      WriteCallBack(JSArguments[i],False)
    else
      WriteBuf:=WriteBuf+String(JSArguments[i]);
end;

procedure Writeln;

var
  i,l: Integer;
  s: String;

begin
  L:=length(JSArguments)-1;
  if Assigned(WriteCallBack) then
    begin
    for i:=0 to L do
      WriteCallBack(JSArguments[i],I=L);
    end
  else
    begin
    s:=WriteBuf;
    for i:=0 to L do
      s:=s+String(JSArguments[i]);
    asm
      console.log(s);
    end;
    WriteBuf:='';
    end;
end;

function Int(const A: Double): double;

begin
  // trunc contains fix for missing Math.trunc in IE
  Result:=Trunc(A);
end;

function Number(S: String): Double; external name 'Number';

function valint(const S: String; MinVal, MaxVal: NativeInt; out Code: Integer): NativeInt;
var
  x: double;
begin
  x:=Number(S);
  if isNaN(x) then
    case copy(s,1,1) of
    '$': x:=Number('0x'+copy(S,2));
    '&': x:=Number('0o'+copy(S,2));
    '%': x:=Number('0b'+copy(S,2));
    else
      Code:=1;
      exit;
    end;
  if isNaN(x) or (X<>Int(X)) then
    Code:=1
  else if (x<MinVal) or (x>MaxVal) then
    Code:=2
  else
    begin
    Result:=Trunc(x);
    Code:=0;
    end;
end;

procedure val(const S: String; out NI : NativeInt; out Code: Integer);
begin
  NI:=valint(S,low(NI),high(NI),Code);
end;

procedure val(const S: String; out NI: NativeUInt; out Code: Integer);
var
  x : double;
begin
  x:=Number(S);
  if isNaN(x) or (X<>Int(X)) or (X<0) then
    Code:=1
  else
    begin
    Code:=0;
    NI:=Trunc(x);
    end;
end;

procedure val(const S: String; out SI : ShortInt; out Code: Integer);
begin
  SI:=valint(S,low(SI),high(SI),Code);
end;

procedure val(const S: String; out SI: smallint; out Code: Integer);
begin
  SI:=valint(S,low(SI),high(SI),Code);
end;

procedure val(const S: String; out C: Cardinal; out Code: Integer);
begin
  C:=valint(S,low(C),high(C),Code);
end;

procedure val(const S: String; out B: Byte; out Code: Integer);
begin
  B:=valint(S,low(B),high(B),Code);
end;

procedure val(const S: String; out W: word; out Code: Integer);
begin
  W:=valint(S,low(W),high(W),Code);
end;

procedure val(const S : String; out I : integer; out Code : Integer);
begin
  I:=valint(S,low(I),high(I),Code);
end;

procedure val(const S : String; out d : double; out Code : Integer);
Var
  x: double;
begin
  x:=Number(S);
  if isNaN(x) then
    Code:=1
  else
    begin
    Code:=0;
    d:=x;
    end;
end;

procedure val(const S: String; out b: boolean; out Code: Integer);
begin
  if SameText(S,'true') then
    begin
    Code:=0;
    b:=true;
    end
  else if SameText(S,'false') then
    begin
    Code:=0;
    b:=false;
    end
  else
    Code:=1;
end;

function upcase(c : char) : char; assembler;
asm
  return c.toUpperCase();
end;

function StringOfChar(c: Char; l: NativeInt): String;
var
  i: Integer;
begin
  asm
    if ((l>0) && c.repeat) return c.repeat(l);
  end;
  Result:='';
  for i:=1 to l do Result:=Result+c;
end;

function Assigned(const V: JSValue): boolean; assembler;
asm
  return (V!=undefined) && (V!=null) && (!rtl.isArray(V) || (V.length > 0));
end;

function StrictEqual(const A: JSValue; const B): boolean; assembler;
asm
  return A === B;
end;

function StrictInequal(const A: JSValue; const B): boolean; assembler;
asm
  return A !== B;
end;

{ TContainedObject }

function TContainedObject.QueryInterface(const iid: TGuid; out obj): Integer;
begin
  if GetInterface(iid,obj) then
    Result:=S_OK
  else
    Result:=Integer(E_NOINTERFACE);
end;

{ TAggregatedObject }

function TAggregatedObject.GetController: IUnknown;
begin
  Result := IUnknown(fController);
end;

function TAggregatedObject.QueryInterface(const iid: TGuid; out obj): Integer;
begin
  Result := IUnknown(fController).QueryInterface(iid, obj);
end;

function TAggregatedObject._AddRef: Integer;
begin
  Result := IUnknown(fController)._AddRef;
end;

function TAggregatedObject._Release: Integer;
begin
  Result := IUnknown(fController)._Release;
end;

constructor TAggregatedObject.Create(const aController: IUnknown);
begin
  inherited Create;
  { do not keep a counted reference to the controller! }
  fController := Pointer(aController);
end;

{ TInterfacedObject }

function TInterfacedObject.QueryInterface(const iid: TGuid; out obj): Integer;
begin
  if GetInterface(iid,obj) then
    Result:=S_OK
  else
    Result:=Integer(E_NOINTERFACE);
end;

function TInterfacedObject._AddRef: Integer;
begin
  inc(fRefCount);
  Result:=fRefCount;
end;

function TInterfacedObject._Release: Integer;
begin
  dec(fRefCount);
  Result:=fRefCount;
  if fRefCount=0 then
    Destroy;
end;

procedure TInterfacedObject.BeforeDestruction;
begin
  if fRefCount<>0 then
    asm
    rtl.raiseE('EHeapMemoryError');
    end;
end;

{ TObject }

constructor TObject.Create;
begin

end;

destructor TObject.Destroy;
begin

end;

procedure TObject.Free;
begin
  Destroy;
end;

class function TObject.ClassType: TClass; assembler;
asm
  return this;
end;

class function TObject.ClassNameIs(const Name: string): boolean;
begin
  Result:=SameText(Name,ClassName);
end;

class function TObject.InheritsFrom(aClass: TClass): boolean; assembler;
asm
  return (aClass!=null) && ((this==aClass) || aClass.isPrototypeOf(this));
end;

procedure TObject.AfterConstruction;
begin

end;

procedure TObject.BeforeDestruction;
begin

end;

function TObject.GetInterface(const iid: TGuid; out obj): boolean;
begin
  asm
    var i = iid.$intf;
    if (i){
      // iid is the private TGuid of an interface
      i = rtl.getIntfG(this,i.$guid,2);
      if (i){
        obj.set(i);
        return true;
      }
    }
  end;
  Result := GetInterfaceByStr(GUIDToString(iid),obj);
end;

function TObject.GetInterface(const iidstr: String; out obj): boolean;
begin
  Result := GetInterfaceByStr(iidstr,obj);
end;

function TObject.GetInterfaceByStr(const iidstr: String; out obj): boolean;
begin
  if not TJSObj(IObjectInstance)['$str'] then
    TJSObj(IObjectInstance)['$str']:=GUIDToString(IObjectInstance);
  if iidstr = TJSObj(IObjectInstance)['$str'] then
    begin
    obj:=Self;
    exit(true);
    end;
  asm
    var i = rtl.getIntfG(this,iidstr,2);
    obj.set(i);
    return i!==null;
  end;
  Result:=false;
end;

function TObject.GetInterfaceWeak(const iid: TGuid; out obj): boolean;
begin
  Result:=GetInterface(iid,obj);
  asm
    if (Result){
      var o = obj.get();
      if (o.$kind==='com'){
        o._Release();
      }
    }
  end;
end;

function TObject.Equals(Obj: TObject): boolean;
begin
  Result:=Obj=Self;
end;

function TObject.ToString: String;
begin
  Result:=ClassName;
end;


initialization
  ExitCode:=0; // set it here, so that WPO does not remove it

end.

