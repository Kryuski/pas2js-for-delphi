{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2018 by Mattias Gaertner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit RTTI;

{$mode objfpc}

interface

uses
  SysUtils, Types, TypInfo, JS;

resourcestring
  SErrInvokeInvalidCodeAddr = 'CodeAddress is not a function';

type
  // will be changed to 'record' and improved as soon as the
  // operator overloading is implemented
  TValue = JSValue;

  EInvoke = EJS;

  TVirtualInterfaceInvokeEvent = function(const aMethodName: string;
    const Args: TJSValueDynArray): JSValue of object;

  { TVirtualInterface: A class that can implement any IInterface. Any method
    call is handled by the OnInvoke event. }
  TVirtualInterface = class(TInterfacedObject, IInterface)
  private
    FOnInvoke: TVirtualInterfaceInvokeEvent;
  public
    constructor Create(InterfaceTypeInfo: Pointer); overload; assembler;
    constructor Create(InterfaceTypeInfo: Pointer;
      const InvokeEvent: TVirtualInterfaceInvokeEvent); overload;
    property OnInvoke: TVirtualInterfaceInvokeEvent read FOnInvoke write FOnInvoke;
  end;

procedure CreateVirtualCorbaInterface(InterfaceTypeInfo: Pointer;
  const MethodImplementation: TVirtualInterfaceInvokeEvent; out IntfVar); assembler;

function Invoke(ACodeAddress: Pointer; const AArgs: TJSValueDynArray;
  ACallConv: TCallConv; AResultType: PTypeInfo; AIsStatic: Boolean;
  AIsConstructor: Boolean): TValue;

implementation

procedure CreateVirtualCorbaInterface(InterfaceTypeInfo: Pointer;
  const MethodImplementation: TVirtualInterfaceInvokeEvent; out IntfVar); assembler;
asm
  var IntfType = InterfaceTypeInfo.interface;
  var i = Object.create(IntfType);
  var o = { $name: "virtual", $fullname: "virtual" };
  i.$o = o;
  do {
    var names = IntfType.$names;
    if (!names) break;
    for (var j=0; j<names.length; j++){
      let fnname = names[j];
      i[fnname] = function(){ return MethodImplementation(fnname,arguments); };
    }
    IntfType = Object.getPrototypeOf(IntfType);
  } while(IntfType!=null);
  IntfVar.set(i);
end;

{ TVirtualInterface }

constructor TVirtualInterface.Create(InterfaceTypeInfo: Pointer); assembler;
asm
  var IntfType = InterfaceTypeInfo.interface;
  if (IntfType.$kind !== 'com') rtl.raiseE('EInvalidCast');
  var guid = IntfType.$guid;
  var i = Object.create(IntfType); // needed by IntfVar is IntfType
  i.$o = this;
  // copy IInterface methods: _AddRef, _Release, QueryInterface
  var iinterfaceguid = '{00000000-0000-0000-C000-000000000046}';
  var map = this.$intfmaps[iinterfaceguid];
  for (var key in map){
    var v = map[key];
    if (typeof(v)!=='function') continue;
    i[key] = map[key];
  }
  // all other methods call OnInvoke
  do {
    var names = IntfType.$names;
    if (!names) break;
    for (var j=0; j<names.length; j++){
      let fnname = names[j];
      if (i[fnname]) continue;
      i[fnname] = function(){ return this.$o.FOnInvoke(fnname,arguments); };
    }
    IntfType = Object.getPrototypeOf(IntfType);
  } while(IntfType!=null);
  // create a new list of interface map, supporting IInterface and IntfType
  this.$intfmaps = {};
  this.$intfmaps[iinterfaceguid] = map;
  this.$intfmaps[guid] = {};
  // store the implementation of IntfType (used by the as-operator)
  this.$interfaces = {};
  this.$interfaces[guid] = i;
end;

constructor TVirtualInterface.Create(InterfaceTypeInfo: Pointer;
  const InvokeEvent: TVirtualInterfaceInvokeEvent);
begin
  Create(InterfaceTypeInfo);
  OnInvoke:=InvokeEvent;
end;

function Invoke(ACodeAddress: Pointer; const AArgs: TJSValueDynArray;
  ACallConv: TCallConv; AResultType: PTypeInfo; AIsStatic: Boolean;
  AIsConstructor: Boolean): TValue;
begin
  if isFunction(ACodeAddress) then
    Result := TJSFunction(ACodeAddress).apply(nil, AArgs)
  else
    raise EInvoke.Create(SErrInvokeInvalidCodeAddr);
end;

end.

