{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2018 by Mattias Gaertner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit TypInfo;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  SysUtils, Types, RTLConsts, JS;

type
  // if you change the following enumeration type in any way
  // you also have to change the rtl.js in an appropriate way !
  TTypeKind = (
    tkUnknown,  // 0
    tkInteger,  // 1
    tkChar,     // 2 in Delphi/FPC tkWChar, tkUChar
    tkString,   // 3 in Delphi/FPC tkSString, tkWString or tkUString
    tkEnumeration, // 4
    tkSet,      // 5
    tkDouble,   // 6
    tkBool,     // 7
    tkProcVar,  // 8
    tkMethod,   // 9  proc var of object
    tkArray,    // 10 static array
    tkDynArray, // 11
    tkRecord,   // 12
    tkClass,    // 13
    tkClassRef, // 14
    tkPointer,  // 15
    tkJSValue,  // 16
    tkRefToProcVar, // 17
    tkInterface // 18
    //tkObject,
    //tkSString,tkLString,tkAString,tkWString,
    //tkVariant,
    //tkWChar,
    //tkInt64,
    //tkQWord,
    //tkInterfaceRaw,
    //tkUString,tkUChar,
    //tkHelper,
    //tkFile,
    );
  TTypeKinds = set of TTypeKind;

  // for compatibility with Delphi/FPC, ignored under pas2js
  TCallConv = (ccReg, ccCdecl, ccPascal, ccStdCall, ccSafeCall, ccCppdecl,
    ccFar16, ccOldFPCCall, ccInternProc, ccSysCall, ccSoftFloat, ccMWPascal);

const
  tkFloat = tkDouble; // for compatibility with Delphi/FPC
  tkProcedure = tkProcVar; // for compatibility with Delphi
  tkAny = [Low(TTypeKind)..High(TTypeKind)];
  tkMethods = [tkMethod];
  tkProperties = tkAny-tkMethods-[tkUnknown];

type
  { TTypeInfo }

  TTypeInfo = class external name 'rtl.tTypeInfo'
  public
    Name: String external name 'name';
    Kind: TTypeKind external name 'kind';
  end;
  TTypeInfoClassOf = class of TTypeInfo;

  PTypeInfo = Pointer; // for compatibility with Delphi/FPC, under pas2js it is a TTypeInfo

  TOrdType  = (
    otSByte,      // 0
    otUByte,      // 1
    otSWord,      // 2
    otUWord,      // 3
    otSLong,      // 4
    otULong,      // 5
    otSIntDouble, // 6 NativeInt
    otUIntDouble  // 7 NativeUInt
    );

  { TTypeInfoInteger - Kind = tkInteger }

  TTypeInfoInteger = class external name 'rtl.tTypeInfoInteger'(TTypeInfo)
  public
    MinValue: NativeInt external name 'minvalue';
    MaxValue: NativeInt external name 'maxvalue';
    OrdType : TOrdType external name 'ordtype';
  end;

  { TEnumType }

  TEnumType = class external name 'anonymous'
  private
    function GetIntToName(Index: NativeInt): String; external name '[]';
    function GetNameToInt(Name: String): NativeInt; external name '[]';
  public
    property IntToName[Index: NativeInt]: String read GetIntToName;
    property NameToInt[Name: String]: NativeInt read GetNameToInt;
  end;

  { TTypeInfoEnum - Kind = tkEnumeration }

  TTypeInfoEnum = class external name 'rtl.tTypeInfoEnum'(TTypeInfoInteger)
  public
    // not supported: BaseType: TTypeInfo
    EnumType: TEnumType external name 'enumtype';
  end;

  { TTypeInfoSet - Kind = tkSet }

  TTypeInfoSet = class external name 'rtl.tTypeInfoSet'(TTypeInfo)
  public
    // not supported: BaseType: TTypeInfo
    CompType: TTypeInfo external name 'comptype';
  end;

  { TTypeInfoStaticArray - Kind = tkArray }

  TTypeInfoStaticArray = class external name 'rtl.tTypeInfoStaticArray'(TTypeInfo)
  public
    Dims: TIntegerDynArray;
    ElType: TTypeInfo external name 'eltype';
  end;

  { TTypeInfoDynArray - Kind = tkDynArray }

  TTypeInfoDynArray = class external name 'rtl.tTypeInfoDynArray'(TTypeInfo)
  public
    DimCount: NativeInt external name 'dimcount';
    ElType: TTypeInfo external name 'eltype';
  end;

  TParamFlag     = (
    pfVar,     // 2^0 = 1
    pfConst,   // 2^1 = 2
    pfOut,     // 2^2 = 4
    pfArray    // 2^3 = 8
    //pfAddress,pfReference,
    );
  TParamFlags = set of TParamFlag;

  { TProcedureParam }

  TProcedureParam = class external name 'anonymous'
  public
    Name: String external name 'name';
    TypeInfo: TTypeInfo external name 'typeinfo';
    Flags: NativeInt external name 'flags'; // TParamFlags as bit vector
  end;

  TProcedureParams = array of TProcedureParam;

  TProcedureFlag = (
    pfStatic,   // 2^0 = 1
    pfVarargs,  // 2^1 = 2
    pfExternal  // 2^2 = 4  name may be an expression
    );
  TProcedureFlags = set of TProcedureFlag;

  { TProcedureSignature }

  TProcedureSignature = class external name 'anonymous'
  public
    Params: TProcedureParams external name 'params'; // can be null
    ResultType: TTypeInfo external name 'resulttype'; // can be null
    Flags: NativeInt external name 'flags'; // TProcedureFlags as bit vector
  end;

  { TTypeInfoProcVar - Kind = tkProcVar }

  TTypeInfoProcVar = class external name 'rtl.tTypeInfoProcVar'(TTypeInfo)
  public
    ProcSig: TProcedureSignature external name 'procsig';
  end;

  { TTypeInfoRefToProcVar - Kind = tkRefToProcVar }

  TTypeInfoRefToProcVar = class external name 'rtl.tTypeInfoRefToProcVar'(TTypeInfoProcVar)
  end;

  TMethodKind = (
    mkProcedure,     // 0  default
    mkFunction,      // 1
    mkConstructor,   // 2
    mkDestructor,    // 3
    mkClassProcedure,// 4
    mkClassFunction  // 5
    //mkClassConstructor,mkClassDestructor,mkOperatorOverload
    );
  TMethodKinds = set of TMethodKind;

  { TTypeInfoMethodVar - Kind = tkMethod }

  TTypeInfoMethodVar = class external name 'rtl.tTypeInfoMethodVar'(TTypeInfoProcVar)
  public
    MethodKind: TMethodKind external name 'methodkind';
  end;

  TTypeMemberKind = (
    tmkUnknown,  // 0
    tmkField,    // 1
    tmkMethod,   // 2
    tmkProperty  // 3
    );
  TTypeMemberKinds = set of TTypeMemberKind;

  { TTypeMember }

  TTypeMember = class external name 'rtl.tTypeMember'
  public
    Name: String external name 'name';
    Kind: TTypeMemberKind external name 'kind';
  end;
  TTypeMemberDynArray = array of TTypeMember;

  { TTypeMemberField - Kind = tmkField }

  TTypeMemberField = class external name 'rtl.tTypeMemberField'(TTypeMember)
  public
    TypeInfo: TTypeInfo external name 'typeinfo';
  end;

  { TTypeMemberMethod - Kind = tmkMethod }

  TTypeMemberMethod = class external name 'rtl.tTypeMemberMethod'(TTypeMember)
  public
    MethodKind: TMethodKind external name 'methodkind';
    ProcSig: TProcedureSignature external name 'procsig';
  end;
  TTypeMemberMethodDynArray = array of TTypeMemberMethod;

const
  pfGetFunction = 1; // getter is a function
  pfSetProcedure = 2; // setter is a procedure
  // stored is a 2-bit vector:
  pfStoredFalse = 4; // stored false, never
  pfStoredField = 8; // stored field, field name is in Stored
  pfStoredFunction = 12; // stored function, function name is in Stored
  pfHasIndex = 16; { if getter is function, append Index as last param
                     if setter is function, append Index as second last param }
type
  { TTypeMemberProperty - Kind = tmkProperty }

  TTypeMemberProperty = class external name 'rtl.tTypeMemberProperty'(TTypeMember)
  public
    TypeInfo: TTypeInfo external name 'typeinfo';
    Flags: NativeInt external name 'flags'; // bit vector, see pf constants above
    Params: TProcedureParams external name 'params'; // can be null or undefined
    Index: JSValue external name 'index'; // can be undefined
    Getter: String external name 'getter'; // name of field or function
    Setter: String external name 'setter'; // name of field or function
    Stored: String external name 'stored'; // name of field or function, can be undefined
    Default: JSValue external name 'Default'; // can be undefined
  end;
  TTypeMemberPropertyDynArray = array of TTypeMemberProperty;

  { TTypeMembers }

  TTypeMembers = class external name 'rtl.tTypeMembers'
  private
    function GetItems(Name: String): TTypeMember; external name '[]';
    procedure SetItems(Name: String; const AValue: TTypeMember); external name '[]';
  public
    property Members[Name: String]: TTypeMember read GetItems write SetItems; default;
  end;

  { TTypeInfoStruct }

  TTypeInfoStruct = class external name 'rtl.tTypeInfoStruct'(TTypeInfo)
  private
    FFieldCount: NativeInt external name 'fields.length';
    FMethodCount: NativeInt external name 'methods.length';
    FPropCount: NativeInt external name 'properties.length';
  public
    Members: TTypeMembers external name 'members';
    Names: TStringDynArray external name 'names'; // all member names with TTypeInfo
    Fields: TStringDynArray external name 'fields';
    Methods: TStringDynArray external name 'methods';
    Properties: TStringDynArray external name 'properties';
    property FieldCount: NativeInt read FFieldCount;
    function GetField(Index: NativeInt): TTypeMemberField; external name 'getField';
    function AddField(aName: String; aType: TTypeInfo; Options: TJSObject = nil
      ): TTypeMemberField; external name 'addField';
    property MethodCount: NativeInt read FMethodCount;
    function GetMethod(Index: NativeInt): TTypeMemberMethod; external name 'getMethod';
    function AddMethod(aName: String; MethodKind: TMethodKind = mkProcedure;
      Params: TJSArray = nil; ResultType: TTypeInfo = nil;
      Options: TJSObject = nil): TTypeMemberMethod; external name 'addMethod';
    property PropCount: NativeInt read FPropCount;
    function GetProp(Index: NativeInt): TTypeMemberProperty; external name 'getProperty';
    function AddProperty(aName: String; Flags: NativeInt; ResultType: TTypeInfo;
      Getter, Setter: String; Options: TJSObject = nil): TTypeMemberProperty; external name 'addProperty';
  end;

  { TTypeInfoRecord - Kind = tkRecord }

  TTypeInfoRecord = class external name 'rtl.tTypeInfoRecord'(TTypeInfoStruct)
  public
    RecordType: TJSObject external name 'record';
  end;

  { TTypeInfoClass - Kind = tkClass }

  TTypeInfoClass = class external name 'rtl.tTypeInfoClass'(TTypeInfoStruct)
  public
    ClassType: TClass external name 'class';
    Ancestor: TTypeInfoClass external name 'ancestor';
  end;

  { TTypeInfoClassRef - class-of, Kind = tkClassRef }

  TTypeInfoClassRef = class external name 'rtl.tTypeInfoClassRef'(TTypeInfo)
  public
    InstanceType: TTypeInfo external name 'instancetype';
  end;

  { TTypeInfoPointer - Kind = tkPointer }

  TTypeInfoPointer = class external name 'rtl.tTypeInfoPointer'(TTypeInfo)
  public
    RefType: TTypeInfo external name 'reftype'; // can be null
  end;

  { TTypeInfoInterface - Kind = tkInterface }

  TTypeInfoInterface = class external name 'rtl.tTypeInfoInterface'(TTypeInfoStruct)
  public
    InterfaceType: TJSObject external name 'interface';
    Ancestor: TTypeInfoInterface external name 'ancestor';
  end;

  EPropertyError  = class(Exception);

function GetClassMembers(aTIStruct: TTypeInfoStruct): TTypeMemberDynArray;
function GetClassMember(aTIStruct: TTypeInfoStruct; const aName: String): TTypeMember;
function GetInstanceMethod(Instance: TObject; const aName: String): Pointer;
function GetClassMethods(aTIStruct: TTypeInfoStruct): TTypeMemberMethodDynArray;
function CreateMethod(Instance: TObject; FuncName: String): Pointer; external name 'rtl.createCallback';

function GetInterfaceMembers(aTIInterface: TTypeInfoInterface): TTypeMemberDynArray;
function GetInterfaceMember(aTIInterface: TTypeInfoInterface; const aName: String): TTypeMember;
function GetInterfaceMethods(aTIInterface: TTypeInfoInterface): TTypeMemberMethodDynArray;

function GetPropInfos(aTIStruct: TTypeInfoStruct): TTypeMemberPropertyDynArray;
function GetPropList(aTIStruct: TTypeInfoStruct; TypeKinds: TTypeKinds; Sorted: boolean = true): TTypeMemberPropertyDynArray;
function GetPropList(aTIStruct: TTypeInfoStruct): TTypeMemberPropertyDynArray;
function GetPropList(AClass: TClass): TTypeMemberPropertyDynArray;
function GetPropList(Instance: TObject): TTypeMemberPropertyDynArray;

function GetPropInfo(TI: TTypeInfoStruct; const PropName: String): TTypeMemberProperty;
function GetPropInfo(TI: TTypeInfoStruct; const PropName: String; const Kinds: TTypeKinds): TTypeMemberProperty;
function GetPropInfo(Instance: TObject; const PropName: String): TTypeMemberProperty;
function GetPropInfo(Instance: TObject; const PropName: String; const Kinds: TTypeKinds): TTypeMemberProperty;
function GetPropInfo(aClass: TClass; const PropName: String): TTypeMemberProperty;
function GetPropInfo(aClass: TClass; const PropName: String; const Kinds: TTypeKinds): TTypeMemberProperty;

function FindPropInfo(Instance: TObject; const PropName: String): TTypeMemberProperty;
function FindPropInfo(Instance: TObject; const PropName: String; const Kinds: TTypeKinds): TTypeMemberProperty;
function FindPropInfo(aClass: TClass; const PropName: String): TTypeMemberProperty;
function FindPropInfo(aClass: TClass; const PropName: String; const Kinds: TTypeKinds): TTypeMemberProperty;

// Property information routines.
Function IsStoredProp(Instance: TObject; const PropInfo: TTypeMemberProperty): Boolean;
Function IsStoredProp(Instance: TObject; const PropName: string): Boolean;
function IsPublishedProp(Instance: TObject; const PropName: String): Boolean;
function IsPublishedProp(aClass: TClass; const PropName: String): Boolean;
function PropType(Instance: TObject; const PropName: string): TTypeKind;
function PropType(aClass: TClass; const PropName: string): TTypeKind;
function PropIsType(Instance: TObject; const PropName: string; const TypeKind: TTypeKind): Boolean;
function PropIsType(aClass: TClass; const PropName: string; const TypeKind: TTypeKind): Boolean;

function GetJSValueProp(Instance: TJSObject; TI: TTypeInfoStruct; const PropName: String): JSValue;
function GetJSValueProp(Instance: TJSObject; const PropInfo: TTypeMemberProperty): JSValue;
function GetJSValueProp(Instance: TObject; const PropName: String): JSValue;
function GetJSValueProp(Instance: TObject; const PropInfo: TTypeMemberProperty): JSValue;
procedure SetJSValueProp(Instance: TJSObject; TI: TTypeInfoStruct; const PropName: String; Value: JSValue);
procedure SetJSValueProp(Instance: TJSObject; const PropInfo: TTypeMemberProperty; Value: JSValue);
procedure SetJSValueProp(Instance: TObject; const PropName: String; Value: JSValue);
procedure SetJSValueProp(Instance: TObject; const PropInfo: TTypeMemberProperty; Value: JSValue);

function GetNativeIntProp(Instance: TObject; const PropName: String): NativeInt;
function GetNativeIntProp(Instance: TObject; const PropInfo: TTypeMemberProperty): NativeInt;
procedure SetNativeIntProp(Instance: TObject; const PropName: String; Value: NativeInt);
procedure SetNativeIntProp(Instance: TObject; const PropInfo: TTypeMemberProperty; Value: NativeInt);

function GetOrdProp(Instance: TObject; const PropName: String): longint;
function GetOrdProp(Instance: TObject; const PropInfo: TTypeMemberProperty): longint;
procedure SetOrdProp(Instance: TObject; const PropName: String; Value: longint);
procedure SetOrdProp(Instance: TObject; const PropInfo: TTypeMemberProperty; Value: longint);

function GetEnumProp(Instance: TObject; const PropName: String): String;
function GetEnumProp(Instance: TObject; const PropInfo: TTypeMemberProperty): String;
procedure SetEnumProp(Instance: TObject; const PropName: String; const Value: String);
procedure SetEnumProp(Instance: TObject; const PropInfo: TTypeMemberProperty; const Value: String);
// Auxiliary routines, which may be useful
function GetEnumName(TypeInfo: TTypeInfoEnum; Value: Integer): String;
function GetEnumValue(TypeInfo: TTypeInfoEnum; const Name: string): Longint;
function GetEnumNameCount(TypeInfo: TTypeInfoEnum): Longint;

function GetSetProp(Instance: TObject; const PropName: String): String; overload;
function GetSetProp(Instance: TObject; const PropInfo: TTypeMemberProperty): String; overload;
function GetSetPropArray(Instance: TObject; const PropName: String): TIntegerDynArray; overload;
function GetSetPropArray(Instance: TObject; const PropInfo: TTypeMemberProperty): TIntegerDynArray; overload;
procedure SetSetPropArray(Instance: TObject; const PropName: String; const Arr: TIntegerDynArray); overload;
procedure SetSetPropArray(Instance: TObject; const PropInfo: TTypeMemberProperty; const Arr: TIntegerDynArray); overload;

function GetStrProp(Instance: TObject; const PropName: String): String;
function GetStrProp(Instance: TObject; const PropInfo: TTypeMemberProperty): String;
procedure SetStrProp(Instance: TObject; const PropName: String; Value: String);
procedure SetStrProp(Instance: TObject; const PropInfo: TTypeMemberProperty; Value: String);

function GetStringProp(Instance: TObject; const PropName: String): String; deprecated; // use GetStrProp
function GetStringProp(Instance: TObject; const PropInfo: TTypeMemberProperty): String; deprecated; // use GetStrProp
procedure SetStringProp(Instance: TObject; const PropName: String; Value: String); deprecated; // use GetStrProp
procedure SetStringProp(Instance: TObject; const PropInfo: TTypeMemberProperty; Value: String); deprecated; // use GetStrProp

function GetBoolProp(Instance: TObject; const PropName: String): boolean;
function GetBoolProp(Instance: TObject; const PropInfo: TTypeMemberProperty): boolean;
procedure SetBoolProp(Instance: TObject; const PropName: String; Value: boolean);
procedure SetBoolProp(Instance: TObject; const PropInfo: TTypeMemberProperty; Value: boolean);

function GetObjectProp(Instance: TObject; const PropName: String): TObject;
function GetObjectProp(Instance: TObject; const PropName: String; MinClass : TClass): TObject;
function GetObjectProp(Instance: TObject; const PropInfo: TTypeMemberProperty):  TObject;
function GetObjectProp(Instance: TObject; const PropInfo: TTypeMemberProperty; MinClass : TClass):  TObject;
procedure SetObjectProp(Instance: TObject; const PropName: String; Value: TObject) ;
procedure SetObjectProp(Instance: TObject; const PropInfo: TTypeMemberProperty; Value: TObject);

Function  GetFloatProp(Instance: TObject; const PropName: string): Double;
Function  GetFloatProp(Instance: TObject; PropInfo : TTypeMemberProperty) : Double;
Procedure SetFloatProp(Instance: TObject; const PropName: string; Value: Double);
Procedure SetFloatProp(Instance: TObject; PropInfo : TTypeMemberProperty;  Value : Double);

implementation

function GetClassMembers(aTIStruct: TTypeInfoStruct): TTypeMemberDynArray;
var
  C: TTypeInfoStruct;
  i: Integer;
  PropName: String;
  Names: TJSObject;
begin
  Result:=nil;
  Names:=TJSObject.new;
  C:=aTIStruct;
  while C<>nil do
  begin
    for i:=0 to length(C.Names)-1 do
    begin
      PropName:=C.Names[i];
      if Names.hasOwnProperty(PropName) then continue;
      TJSArray(Result).push(C.Members[PropName]);
      Names[PropName]:=true;
    end;
    if not (C is TTypeInfoClass) then break;
    C:=TTypeInfoClass(C).Ancestor;
  end;
end;

function GetClassMember(aTIStruct: TTypeInfoStruct; const aName: String): TTypeMember;
var
  C: TTypeInfoStruct;
  i: Integer;
begin
  // quick search: case sensitive
  C:=aTIStruct;
  while C<>nil do
  begin
    if TJSObject(C.Members).hasOwnProperty(aName) then
      exit(C.Members[aName]);
    if not (C is TTypeInfoClass) then break;
    C:=TTypeInfoClass(C).Ancestor;
  end;
  // slow search: case insensitive
  C:=aTIStruct;
  while C<>nil do
  begin
    for i:=0 to length(C.Names)-1 do
      if CompareText(C.Names[i],aName)=0 then
        exit(C.Members[C.Names[i]]);
    if not (C is TTypeInfoClass) then break;
    C:=TTypeInfoClass(C).Ancestor;
  end;
  Result:=nil;
end;

function GetInstanceMethod(Instance: TObject; const aName: String): Pointer;
var
  TI: TTypeMember;
begin
  if Instance=nil then exit(nil);
  TI:=GetClassMember(TypeInfo(Instance),aName);
  if not (TI is TTypeMemberMethod) then exit(nil);
  Result:=CreateMethod(Instance,TI.Name); // Note: use TI.Name for the correct case!
end;

function GetClassMethods(aTIStruct: TTypeInfoStruct): TTypeMemberMethodDynArray;
var
  C: TTypeInfoStruct;
  i, Cnt, j: Integer;
begin
  Cnt:=0;
  C:=aTIStruct;
  while C<>nil do
  begin
    inc(Cnt,C.MethodCount);
    if not (C is TTypeInfoClass) then break;
    C:=TTypeInfoClass(C).Ancestor;
  end;
  SetLength(Result,Cnt);
  C:=aTIStruct;
  i:=0;
  while C<>nil do
  begin
    for j:=0 to C.MethodCount-1 do
    begin
      Result[i]:=TTypeMemberMethod(C.Members[C.Methods[j]]);
      inc(i);
    end;
    if not (C is TTypeInfoClass) then break;
    C:=TTypeInfoClass(C).Ancestor;
  end;
end;

function GetInterfaceMembers(aTIInterface: TTypeInfoInterface
  ): TTypeMemberDynArray;
var
  Intf: TTypeInfoInterface;
  i, Cnt, j: Integer;
begin
  Cnt:=0;
  Intf:=aTIInterface;
  while Intf<>nil do
  begin
    inc(Cnt,length(Intf.Names));
    Intf:=Intf.Ancestor;
  end;
  SetLength(Result,Cnt);
  Intf:=aTIInterface;
  i:=0;
  while Intf<>nil do
  begin
    for j:=0 to length(Intf.Names)-1 do
    begin
      Result[i]:=Intf.Members[Intf.Names[j]];
      inc(i);
    end;
    Intf:=Intf.Ancestor;
  end;
end;

function GetInterfaceMember(aTIInterface: TTypeInfoInterface;
  const aName: String): TTypeMember;
var
  Intf: TTypeInfoInterface;
  i: Integer;
begin
  // quick search: case sensitive
  Intf:=aTIInterface;
  while Intf<>nil do
  begin
    if TJSObject(Intf.Members).hasOwnProperty(aName) then
      exit(Intf.Members[aName]);
    Intf:=Intf.Ancestor;
  end;
  // slow search: case insensitive
  Intf:=aTIInterface;
  while Intf<>nil do
  begin
    for i:=0 to length(Intf.Names)-1 do
      if CompareText(Intf.Names[i],aName)=0 then
        exit(Intf.Members[Intf.Names[i]]);
    Intf:=Intf.Ancestor;
  end;
  Result:=nil;
end;

function GetInterfaceMethods(aTIInterface: TTypeInfoInterface
  ): TTypeMemberMethodDynArray;
var
  Intf: TTypeInfoInterface;
  i, Cnt, j: Integer;
begin
  Cnt:=0;
  Intf:=aTIInterface;
  while Intf<>nil do
  begin
    inc(Cnt,Intf.MethodCount);
    Intf:=Intf.Ancestor;
  end;
  SetLength(Result,Cnt);
  Intf:=aTIInterface;
  i:=0;
  while Intf<>nil do
  begin
    for j:=0 to Intf.MethodCount-1 do
    begin
      Result[i]:=TTypeMemberMethod(Intf.Members[Intf.Methods[j]]);
      inc(i);
    end;
    Intf:=Intf.Ancestor;
  end;
end;

function GetPropInfos(aTIStruct: TTypeInfoStruct): TTypeMemberPropertyDynArray;
var
  C: TTypeInfoStruct;
  i: Integer;
  Names: TJSObject;
  PropName: String;
begin
  Result:=nil;
  C:=aTIStruct;
  Names:=TJSObject.new;
  while C<>nil do
  begin
    for i:=0 to C.PropCount-1 do
    begin
      PropName:=C.Properties[i];
      if Names.hasOwnProperty(PropName) then continue;
      TJSArray(Result).push(TTypeMemberProperty(C.Members[PropName]));
      Names[PropName]:=true;
    end;
    if not (C is TTypeInfoClass) then
      break;
    C:=TTypeInfoClass(C).Ancestor;
  end;
end;

function GetPropList(aTIStruct: TTypeInfoStruct; TypeKinds: TTypeKinds;
  Sorted: boolean): TTypeMemberPropertyDynArray;

  function NameSort(a,b: JSValue): NativeInt;
  begin
    if TTypeMemberProperty(a).Name<TTypeMemberProperty(b).Name then
      Result:=-1
    else if TTypeMemberProperty(a).Name>TTypeMemberProperty(b).Name then
      Result:=1
    else
      Result:=0;
  end;

var
  C: TTypeInfoStruct;
  i: Integer;
  Names: TJSObject;
  PropName: String;
  Prop: TTypeMemberProperty;
begin
  Result:=nil;
  C:=aTIStruct;
  Names:=TJSObject.new;
  while C<>nil do
  begin
    for i:=0 to C.PropCount-1 do
    begin
      PropName:=C.Properties[i];
      if Names.hasOwnProperty(PropName) then continue;
      Prop:=TTypeMemberProperty(C.Members[PropName]);
      if not (Prop.TypeInfo.Kind in TypeKinds) then continue;
      TJSArray(Result).push(Prop);
      Names[PropName]:=true;
    end;
    if not (C is TTypeInfoClass) then
      break;
    C:=TTypeInfoClass(C).Ancestor;
  end;
  if Sorted then
    TJSArray(Result).sort(@NameSort);
end;

function GetPropList(aTIStruct: TTypeInfoStruct): TTypeMemberPropertyDynArray;
begin
  Result:=GetPropInfos(aTIStruct);
end;

function GetPropList(AClass: TClass): TTypeMemberPropertyDynArray;
begin
  Result:=GetPropInfos(TypeInfo(AClass));
end;

function GetPropList(Instance: TObject): TTypeMemberPropertyDynArray;
begin
  Result:=GetPropList(Instance.ClassType);
end;

function GetPropInfo(TI: TTypeInfoStruct; const PropName: String
  ): TTypeMemberProperty;
var
  m: TTypeMember;
  i: Integer;
  C: TTypeInfoStruct;
begin
  // quick search case sensitive
  C:=TI;
  while C<>nil do
  begin
    m:=C.Members[PropName];
    if m is TTypeMemberProperty then
      exit(TTypeMemberProperty(m));
    if not (C is TTypeInfoClass) then
      break;
    C:=TTypeInfoClass(C).Ancestor;
  end;

  // slow search case insensitive
  Result:=nil;
  repeat
    for i:=0 to TI.PropCount-1 do
      if CompareText(PropName,TI.Properties[i])=0 then
      begin
        m:=TI.Members[TI.Properties[i]];
        if m is TTypeMemberProperty then
          Result:=TTypeMemberProperty(m);
        exit;
      end;
    if not (TI is TTypeInfoClass) then
      break;
    TI:=TTypeInfoClass(TI).Ancestor;
  until TI=nil;
end;

function GetPropInfo(TI: TTypeInfoStruct; const PropName: String;
  const Kinds: TTypeKinds): TTypeMemberProperty;
begin
  Result:=GetPropInfo(TI,PropName);
  if (Kinds<>[]) and (Result<>nil) and not (Result.TypeInfo.Kind in Kinds) then
    Result:=nil;
end;

function GetPropInfo(Instance: TObject; const PropName: String
  ): TTypeMemberProperty;
begin
  Result:=GetPropInfo(TypeInfo(Instance),PropName,[]);
end;

function GetPropInfo(Instance: TObject; const PropName: String;
  const Kinds: TTypeKinds): TTypeMemberProperty;
begin
  Result:=GetPropInfo(TypeInfo(Instance),PropName,Kinds);
end;

function GetPropInfo(aClass: TClass; const PropName: String
  ): TTypeMemberProperty;
begin
  Result:=GetPropInfo(TypeInfo(AClass),PropName,[]);
end;

function GetPropInfo(aClass: TClass; const PropName: String;
  const Kinds: TTypeKinds): TTypeMemberProperty;
begin
  Result:=GetPropInfo(TypeInfo(AClass),PropName,Kinds);
end;

function FindPropInfo(Instance: TObject; const PropName: String
  ): TTypeMemberProperty;
begin
  Result:=GetPropInfo(TypeInfo(Instance), PropName);
  if Result=nil then
    raise EPropertyError.CreateFmt(SErrPropertyNotFound, [PropName]);
end;

function FindPropInfo(Instance: TObject; const PropName: String;
  const Kinds: TTypeKinds): TTypeMemberProperty;
begin
  Result:=GetPropInfo(TypeInfo(Instance), PropName, Kinds);
  if Result=nil then
    raise EPropertyError.CreateFmt(SErrPropertyNotFound, [PropName]);
end;

function FindPropInfo(aClass: TClass; const PropName: String
  ): TTypeMemberProperty;
begin
  Result:=GetPropInfo(TypeInfo(aClass), PropName);
  if Result=nil then
    raise EPropertyError.CreateFmt(SErrPropertyNotFound, [PropName]);
end;

function FindPropInfo(aClass: TClass; const PropName: String;
  const Kinds: TTypeKinds): TTypeMemberProperty;
begin
  Result:=GetPropInfo(TypeInfo(aClass), PropName, Kinds);
  if Result=nil then
    raise EPropertyError.CreateFmt(SErrPropertyNotFound, [PropName]);
end;

function IsStoredProp(Instance: TObject; const PropInfo: TTypeMemberProperty
  ): Boolean;
type
  TIsStored = function: Boolean of object;
begin
  case PropInfo.Flags and 12 of
  0: Result:=true;
  4: Result:=false;
  8: Result:=Boolean(TJSObject(Instance)[PropInfo.Stored]);
  else Result:=TIsStored(TJSObject(Instance)[PropInfo.Stored])();
  end;
end;

function IsStoredProp(Instance: TObject; const PropName: string): Boolean;
begin
  Result:=IsStoredProp(Instance,FindPropInfo(Instance,PropName));
end;

function IsPublishedProp(Instance: TObject; const PropName: String): Boolean;
begin
  Result:=GetPropInfo(Instance,PropName)<>nil;
end;

function IsPublishedProp(aClass: TClass; const PropName: String): Boolean;
begin
  Result:=GetPropInfo(aClass,PropName)<>nil;
end;

function PropType(Instance: TObject; const PropName: string): TTypeKind;
begin
  Result:=FindPropInfo(Instance,PropName).TypeInfo.Kind;
end;

function PropType(aClass: TClass; const PropName: string): TTypeKind;
begin
  Result:=FindPropInfo(aClass,PropName).TypeInfo.Kind;
end;

function PropIsType(Instance: TObject; const PropName: string;
  const TypeKind: TTypeKind): Boolean;
begin
  Result:=PropType(Instance,PropName)=TypeKind;
end;

function PropIsType(aClass: TClass; const PropName: string;
  const TypeKind: TTypeKind): Boolean;
begin
  Result:=PropType(aClass,PropName)=TypeKind;
end;

type
  TGetterKind = (
    gkNone,
    gkField,
    gkFunction,
    gkFunctionWithParams
  );

function GetPropGetterKind(const PropInfo: TTypeMemberProperty): TGetterKind;
begin
  if PropInfo.Getter='' then
    Result:=gkNone
  else if (pfGetFunction and PropInfo.Flags)>0 then
    begin
    if length(PropInfo.Params)>0 then
      Result:=gkFunctionWithParams
    else
      Result:=gkFunction;
    end
  else
    Result:=gkField;
end;

type
  TSetterKind = (
    skNone,
    skField,
    skProcedure,
    skProcedureWithParams
  );

function GetPropSetterKind(const PropInfo: TTypeMemberProperty): TSetterKind;
begin
  if PropInfo.Setter='' then
    Result:=skNone
  else if (pfSetProcedure and PropInfo.Flags)>0 then
    begin
    if length(PropInfo.Params)>0 then
      Result:=skProcedureWithParams
    else
      Result:=skProcedure;
    end
  else
    Result:=skField;
end;

function GetJSValueProp(Instance: TJSObject; TI: TTypeInfoStruct;
  const PropName: String): JSValue;
var
  PropInfo: TTypeMemberProperty;
begin
  PropInfo:=GetPropInfo(TI,PropName);
  if PropInfo=nil then
    raise EPropertyError.CreateFmt(SErrPropertyNotFound, [PropName]);
  Result:=GetJSValueProp(Instance,PropInfo);
end;

function GetJSValueProp(Instance: TJSObject;
  const PropInfo: TTypeMemberProperty): JSValue;
type
  TGetter = function: JSValue of object;
  TGetterWithIndex = function(Index: JSValue): JSValue of object;
var
  gk: TGetterKind;
begin
  gk:=GetPropGetterKind(PropInfo);
  case gk of
    gkNone:
      raise EPropertyError.CreateFmt(SCantReadPropertyS, [PropInfo.Name]);
    gkField:
      Result:=Instance[PropInfo.Getter];
    gkFunction:
      if (pfHasIndex and PropInfo.Flags)>0 then
        Result:=TGetterWithIndex(Instance[PropInfo.Getter])(PropInfo.Index)
      else
        Result:=TGetter(Instance[PropInfo.Getter])();
    gkFunctionWithParams:
      raise EPropertyError.CreateFmt(SIndexedPropertyNeedsParams, [PropInfo.Name]);
  end;
end;

function GetJSValueProp(Instance: TObject; const PropName: String): JSValue;
begin
  Result:=GetJSValueProp(Instance,FindPropInfo(Instance,PropName));
end;

function GetJSValueProp(Instance: TObject; const PropInfo: TTypeMemberProperty
  ): JSValue;
begin
  Result:=GetJSValueProp(TJSObject(Instance),PropInfo);
end;

procedure SetJSValueProp(Instance: TJSObject; TI: TTypeInfoStruct;
  const PropName: String; Value: JSValue);
var
  PropInfo: TTypeMemberProperty;
begin
  PropInfo:=GetPropInfo(TI,PropName);
  if PropInfo=nil then
    raise EPropertyError.CreateFmt(SErrPropertyNotFound, [PropName]);
  SetJSValueProp(Instance,PropInfo,Value);
end;

procedure SetJSValueProp(Instance: TJSObject;
  const PropInfo: TTypeMemberProperty; Value: JSValue);
type
  TSetter = procedure(Value: JSValue) of object;
  TSetterWithIndex = procedure(Index, Value: JSValue) of object;
var
  sk: TSetterKind;
begin
  sk:=GetPropSetterKind(PropInfo);
  case sk of
    skNone:
      raise EPropertyError.CreateFmt(SCantWritePropertyS, [PropInfo.Name]);
    skField:
      Instance[PropInfo.Setter]:=Value;
    skProcedure:
      if (pfHasIndex and PropInfo.Flags)>0 then
        TSetterWithIndex(Instance[PropInfo.Setter])(PropInfo.Index,Value)
      else
        TSetter(Instance[PropInfo.Setter])(Value);
    skProcedureWithParams:
      raise EPropertyError.CreateFmt(SIndexedPropertyNeedsParams, [PropInfo.Name]);
  end;
end;

procedure SetJSValueProp(Instance: TObject; const PropName: String;
  Value: JSValue);
begin
  SetJSValueProp(Instance,FindPropInfo(Instance,PropName),Value);
end;

procedure SetJSValueProp(Instance: TObject;
  const PropInfo: TTypeMemberProperty; Value: JSValue);
begin
  SetJSValueProp(TJSObject(Instance),PropInfo,Value);
end;

function GetNativeIntProp(Instance: TObject; const PropName: String): NativeInt;
begin
  Result:=GetNativeIntProp(Instance,FindPropInfo(Instance,PropName));
end;

function GetNativeIntProp(Instance: TObject; const PropInfo: TTypeMemberProperty
  ): NativeInt;
begin
  Result:=NativeInt(GetJSValueProp(Instance,PropInfo));
end;

procedure SetNativeIntProp(Instance: TObject; const PropName: String;
  Value: NativeInt);
begin
  SetJSValueProp(Instance,FindPropInfo(Instance,PropName),Value);
end;

procedure SetNativeIntProp(Instance: TObject;
  const PropInfo: TTypeMemberProperty; Value: NativeInt);
begin
  SetJSValueProp(Instance,PropInfo,Value);
end;

function GetOrdProp(Instance: TObject; const PropName: String): longint;
begin
  Result:=GetOrdProp(Instance,FindPropInfo(Instance,PropName));
end;

function GetOrdProp(Instance: TObject; const PropInfo: TTypeMemberProperty
  ): longint;
var
  o: TJSObject;
  Key: String;
  n: NativeInt;
begin
  if PropInfo.TypeInfo.Kind=tkSet then
  begin
    // a set is a JS object, with the following property: o[ElementDecimal]=true
    o:=TJSObject(GetJSValueProp(Instance,PropInfo));
    Result:=0;
    for Key in o do
    begin
      n:=parseInt(Key,10);
      if n<32 then
        Result:=Result+(1 shl n);
    end;
  end else
    Result:=longint(GetJSValueProp(Instance,PropInfo));
end;

procedure SetOrdProp(Instance: TObject; const PropName: String; Value: longint);
begin
  SetOrdProp(Instance,FindPropInfo(Instance,PropName),Value);
end;

procedure SetOrdProp(Instance: TObject; const PropInfo: TTypeMemberProperty;
  Value: longint);
var
  o: TJSObject;
  i: Integer;
begin
  if PropInfo.TypeInfo.Kind=tkSet then
  begin
    o:=TJSObject.new;
    for i:=0 to 31 do
      if (1 shl i) and Value>0 then
        o[str(i)]:=true;
    SetJSValueProp(Instance,PropInfo,o);
  end else
    SetJSValueProp(Instance,PropInfo,Value);
end;

function GetEnumProp(Instance: TObject; const PropName: String): String;
begin
  Result:=GetEnumProp(Instance,FindPropInfo(Instance,PropName));
end;

function GetEnumProp(Instance: TObject; const PropInfo: TTypeMemberProperty): String;
var
  n: NativeInt;
  TIEnum: TTypeInfoEnum;
begin
  TIEnum:=PropInfo.TypeInfo as TTypeInfoEnum;
  n:=NativeInt(GetJSValueProp(Instance,PropInfo));
  if (n>=TIEnum.MinValue) and (n<=TIEnum.MaxValue) then
    Result:=TIEnum.EnumType.IntToName[n]
  else
    Result:=str(n);
end;

procedure SetEnumProp(Instance: TObject; const PropName: String;
  const Value: String);
begin
  SetEnumProp(Instance,FindPropInfo(Instance,PropName),Value);
end;

procedure SetEnumProp(Instance: TObject; const PropInfo: TTypeMemberProperty;
  const Value: String);
var
  TIEnum: TTypeInfoEnum;
  n: NativeInt;
begin
  TIEnum:=PropInfo.TypeInfo as TTypeInfoEnum;
  n:=TIEnum.EnumType.NameToInt[Value];
  if not isUndefined(n) then
    SetJSValueProp(Instance,PropInfo,n);
end;

function GetEnumName(TypeInfo: TTypeInfoEnum; Value: Integer): String;
begin
  Result:=TypeInfo.EnumType.IntToName[Value];
end;

function GetEnumValue(TypeInfo: TTypeInfoEnum; const Name: string): Longint;
begin
  Result:=TypeInfo.EnumType.NameToInt[Name];
end;

function GetEnumNameCount(TypeInfo: TTypeInfoEnum): Longint;
var
  o: TJSObject;
  l, r: LongInt;
begin
  o:=TJSObject(TypeInfo.EnumType);
  // as of pas2js 1.0 the RTTI does not contain a min/max value
  // -> use exponential search
  // ToDo: adapt this once enums with gaps are supported
  Result:=1;
  while o.hasOwnProperty(String(JSValue(Result))) do
    Result:=Result*2;
  l:=Result div 2;
  r:=Result;
  while l<=r do
    begin
    Result:=(l+r) div 2;
    if o.hasOwnProperty(String(JSValue(Result))) then
      l:=Result+1
    else
      r:=Result-1;
    end;
  if o.hasOwnProperty(String(JSValue(Result))) then
    inc(Result);
end;

function GetSetProp(Instance: TObject; const PropName: String): String;
begin
  Result:=GetSetProp(Instance,FindPropInfo(Instance,PropName));
end;

function GetSetProp(Instance: TObject; const PropInfo: TTypeMemberProperty
  ): String;
var
  o: TJSObject;
  key, Value: String;
  n: NativeInt;
  TIEnum: TTypeInfoEnum;
  TISet: TTypeInfoSet;
begin
  Result:='';
  // get enum type if available
  TISet:=PropInfo.TypeInfo as TTypeInfoSet;
  TIEnum:=nil;
  if TISet.CompType is TTypeInfoEnum then
    TIEnum:=TTypeInfoEnum(TISet.CompType);
  // read value
  o:=TJSObject(GetJSValueProp(Instance,PropInfo));
  // a set is a JS object, where included element is stored as: o[ElementDecimal]=true
  for Key in o do
  begin
    n:=parseInt(Key,10);
    if (TIEnum<>nil) and (n>=TIEnum.MinValue) and (n<=TIEnum.MaxValue) then
      Value:=TIEnum.EnumType.IntToName[n]
    else
      Value:=str(n);
    if Result<>'' then Result:=Result+',';
    Result:=Result+Value;
  end;
  Result:='['+Result+']';
end;

function GetSetPropArray(Instance: TObject; const PropName: String
  ): TIntegerDynArray;
begin
  Result:=GetSetPropArray(Instance,FindPropInfo(Instance,PropName));
end;

function GetSetPropArray(Instance: TObject; const PropInfo: TTypeMemberProperty
  ): TIntegerDynArray;
var
  o: TJSObject;
  Key: string;
begin
  Result:=[];
  // read value
  o:=TJSObject(GetJSValueProp(Instance,PropInfo));
  // a set is a JS object, where included element is stored as: o[ElementDecimal]=true
  for Key in o do
    TJSArray(Result).push(parseInt(Key,10));
end;

procedure SetSetPropArray(Instance: TObject; const PropName: String;
  const Arr: TIntegerDynArray);
begin
  SetSetPropArray(Instance,FindPropInfo(Instance,PropName),Arr);
end;

procedure SetSetPropArray(Instance: TObject;
  const PropInfo: TTypeMemberProperty; const Arr: TIntegerDynArray);
var
  o: TJSObject;
  i: integer;
begin
  o:=TJSObject.new;
  for i in Arr do
    o[str(i)]:=true;
  SetJSValueProp(Instance,PropInfo,o);
end;

function GetStrProp(Instance: TObject; const PropName: String): String;
begin
  Result:=GetStrProp(Instance,FindPropInfo(Instance,PropName));
end;

function GetStrProp(Instance: TObject; const PropInfo: TTypeMemberProperty
  ): String;
begin
  Result:=String(GetJSValueProp(Instance,PropInfo));
end;

procedure SetStrProp(Instance: TObject; const PropName: String; Value: String
  );
begin
  SetStrProp(Instance,FindPropInfo(Instance,PropName),Value);
end;

procedure SetStrProp(Instance: TObject; const PropInfo: TTypeMemberProperty;
  Value: String);
begin
  SetJSValueProp(Instance,PropInfo,Value);
end;

function GetStringProp(Instance: TObject; const PropName: String): String;
begin
  Result:=GetStrProp(Instance,PropName);
end;

function GetStringProp(Instance: TObject; const PropInfo: TTypeMemberProperty
  ): String;
begin
  Result:=GetStrProp(Instance,PropInfo);
end;

procedure SetStringProp(Instance: TObject; const PropName: String; Value: String
  );
begin
  SetStrProp(Instance,PropName,Value);
end;

procedure SetStringProp(Instance: TObject; const PropInfo: TTypeMemberProperty;
  Value: String);
begin
  SetStrProp(Instance,PropInfo,Value);
end;

function GetBoolProp(Instance: TObject; const PropName: String): boolean;
begin
  Result:=GetBoolProp(Instance,FindPropInfo(Instance,PropName));
end;

function GetBoolProp(Instance: TObject; const PropInfo: TTypeMemberProperty
  ): boolean;
begin
  Result:=Boolean(GetJSValueProp(Instance,PropInfo));
end;

procedure SetBoolProp(Instance: TObject; const PropName: String; Value: boolean
  );
begin
  SetBoolProp(Instance,FindPropInfo(Instance,PropName),Value);
end;

procedure SetBoolProp(Instance: TObject; const PropInfo: TTypeMemberProperty;
  Value: boolean);
begin
  SetJSValueProp(Instance,PropInfo,Value);
end;

function GetObjectProp(Instance: TObject; const PropName: String): TObject;
begin
  Result:=GetObjectProp(Instance,FindPropInfo(Instance,PropName));
end;

function GetObjectProp(Instance: TObject; const PropName: String; MinClass : TClass): TObject;
begin
  Result:=GetObjectProp(Instance,FindPropInfo(Instance,PropName));
  if (MinClass<>Nil) and (Result<>Nil) Then
    if not Result.InheritsFrom(MinClass) then
      Result:=Nil;
end;

function GetObjectProp(Instance: TObject; const PropInfo: TTypeMemberProperty):  TObject;

begin
  Result:=GetObjectProp(Instance,PropInfo,Nil);
end;

function GetObjectProp(Instance: TObject; const PropInfo: TTypeMemberProperty; MinClass : TClass):  TObject;

Var
  O : TObject;

begin
  O:=TObject(GetJSValueProp(Instance,PropInfo));
  if (MinClass<>Nil) and not O.InheritsFrom(MinClass) then
    Result:=Nil
  else
    Result:=O;
end;

procedure SetObjectProp(Instance: TObject; const PropName: String; Value: TObject) ;

begin
  SetObjectProp(Instance,FindPropInfo(Instance,PropName),Value);
end;

procedure SetObjectProp(Instance: TObject; const PropInfo: TTypeMemberProperty; Value: TObject);

begin
  SetJSValueProp(Instance,PropInfo,Value);
end;

function GetFloatProp(Instance: TObject; PropInfo: TTypeMemberProperty): Double;
begin
  Result:=Double(GetJSValueProp(Instance,PropInfo));
end;

function GetFloatProp(Instance: TObject; const PropName: string): Double;

begin
  Result:=GetFloatProp(Instance,FindPropInfo(Instance,PropName));
end;

procedure SetFloatProp(Instance: TObject; const PropName: string; Value: Double
  );

begin
  SetFloatProp(Instance,FindPropInfo(Instance,PropName),Value);
end;

procedure SetFloatProp(Instance: TObject; PropInfo: TTypeMemberProperty;
  Value: Double);

begin
  SetJSValueProp(Instance,PropInfo,Value);
end;

end.

