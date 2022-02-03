{
    This file is part of the Free Component Library

    Use native parser for parsing JSON Data structures
    Copyright (c) 2020 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpjsonjs;

{$mode objfpc}

interface

uses
  Classes, Types, fpjson;

Function JSValueToJSONData(aValue : JSValue) : TJSONData;
Procedure HookGetJSONCallBack;
Procedure UnHookGetJSONCallBack;

implementation

Uses JS;

function JSValueToJSONData(aValue: JSValue): TJSONData;

Var
  Obj: TJSObject Absolute aValue;
  v : JSValue;
  S : String;

begin
  Case GetValueType(aValue) of
    jvtNull : Result:=CreateJSON;
    jvtBoolean : Result:=CreateJSON(Boolean(aValue));
    jvtString : Result:=CreateJSON(String(aValue));
    jvtFloat : Result:=CreateJSON(TJSONFloat(aValue));
    jvtInteger:
      if (NativeInt(aValue)>Maxint) or (NativeInt(aValue)<-MaxInt) then
        Result:=CreateJSON(NativeInt(aValue))
      else
        Result:=CreateJSON(NativeInt(aValue));
    jvtArray :
      begin
      Result:=CreateJSONArray([]);
      for v in TJSValueDynArray(aValue) do
        TJSONArray(Result).Add(JSValueToJSONData(v));
      end;
    jvtObject :
      begin
      Result:=CreateJSONObject(Nil);
      For S in TJSObject.getOwnPropertyNames(Obj) do
        TJSOnObject(Result).Add(S,JSValueToJSONData(Obj.Properties[S]));
      end;
  end;
end;


Procedure JSONFromString(Const aJSON : TJSONStringType; Const AUseUTF8 : Boolean; Out Data : TJSONData);

Var
  Msg : String;
  aValue : JSValue;

begin
  msg:='';
  try
    aValue:=TJSJSON.Parse(aJSON);
  except
    On ES : TJSSyntaxError do
      Msg:=ES.message;
    on E : TJSError do
      Msg:=E.Message ;
    on O : TJSObject do
      Msg:='Unknown error : '+TJSJSON.stringify(O);
    else
      asm
        var b = new SyntaxError;
        console.log(SyntaxError.prototype.isPrototypeOf(b));

        if ($e.hasOwnProperty('message')) {
          Msg = '' || $e.message;
        }
      end;
  end;
  if Msg<>'' then
    Raise EJSON.Create('Error parsing JSON: '+Msg);
  // We do this outside the try..except so that in case of errors, we get the original exception.
  Data:=JSValueToJSONData(aValue);
end;

Procedure JSONFromStream(AStream : TStream; Const AUseUTF8 : Boolean; Out Data : TJSONData);

Var
  SS : TStringStream;

begin
  SS:=TStringStream.Create('');
  try
    SS.CopyFrom(aStream,0);
    JSONFromString(SS.DataString,False,Data);
  finally
    SS.Free;
  end;
end;

procedure HookGetJSONCallBack;
begin
  SetJSONParserHandler(@JSONFromStream);
  SetJSONStringParserHandler(@JSONFromString);
end;

Procedure UnHookGetJSONCallBack;

begin
  SetJSONParserHandler(Nil);
  SetJSONStringParserHandler(Nil);
end;

initialization
  HookGetJSONCallBack;
end.

