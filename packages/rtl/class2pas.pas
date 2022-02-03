{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2018 Michael Van Canneyt

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit class2pas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, JS;

function ClassToPas(Const aName : string; Obj: TJSObject; aAncestor : string = ''; recurse : Boolean = False): string; overload;
function ClassToPas(Const aJSName,aPascalName : string; Obj: TJSObject; aAncestor : string = ''; recurse : Boolean = False): string; overload;
Procedure ClassToPas(Const aJSName,aPascalName,aAncestor : string; Obj: TJSObject; aDecl : TStrings; recurse : Boolean = False); overload;

implementation

function ClassToPas(Const aName : string; Obj: TJSObject; aAncestor : string = ''; recurse : Boolean = False): string;

begin
  Result:=ClassToPas('TJS'+aName,aName,Obj,aAncestor,Recurse);
end;

function ClassToPas(Const aJSName,aPascalName : string; Obj: TJSObject; aAncestor : string = '';recurse : Boolean = False): string;

Var
  S : TStringList;

begin
  S:=TStringList.Create;
  try
    ClassToPas(aJSName,aPascalName,aAncestor,Obj,S,Recurse);
    Result:=S.Text;
  finally
    S.Free;
  end;
end;

Procedure ClassToPas(Const aJSName,aPascalName,aAncestor : string; Obj: TJSObject; aDecl : TStrings; recurse : Boolean = False); overload;

var
  Names: TStringDynArray;
  i, j: Integer;
  ot,t: String;
  p: TJSArray;
  f: TJSFunction;
  Value: JSValue;

begin
  T:=aPascalName+' = Class external name '''+aJSName+'''';
  if aAncestor<>'' then
    T:=T+' ('+aAncestor+')';
  aDecl.Add(T);
  aDecl.Add('Public');
  p:=TJSArray.new;
  while Obj<>nil do
    begin
    Names:=TJSObject.getOwnPropertyNames(Obj);
    for i:=0 to length(Names)-1 do
      begin
      try
        Value:=Obj[Names[i]];
      except
        aDecl.Add('// not readable property "'+Names[i]+'"'+sLineBreak);
      end;
      ot:=jsTypeOf(Value);
      if ot='function' then
        begin
        f:=TJSFunction(Value);
        t:=f.name;
        if t='' then
          T:=Names[i];
        t:='function '+T+'(';
        for j:=1 to NativeInt(f['length']) do
          begin
          if j>1 then t:=t+';';
          t:=t+'arg'+IntToStr(j)+': JSValue';
          end;
        t:=t+') : JSValue;';
        end
      else if ot='string' then
        t:=Names[i]+' : string;'
      else if ot='number' then
        t:=Names[i]+' : double;'
      else if ot='boolean' then
        t:=Names[i]+' : boolean;'
      else if ot='object' then
        t:=Names[i]+' : TJSObject;';
      if p.indexOf(t)<0 then
        begin
        p.push(t);
        aDecl.Add('  '+t);
        end;
      end;
    if Recurse then
      Obj:=TJSObject.getPrototypeOf(Obj)
    else
      Obj:=Nil;
    if Obj<>nil then
      aDecl.Add('// next getPrototypeOf ...');
    end;
 aDecl.Add('end;');
end;

end.

