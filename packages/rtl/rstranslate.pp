{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2019 by Michael Van Canneyt

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************} 
unit rstranslate;

{$mode objfpc}

interface

uses
  SysUtils, JS, web;

Type

  { TResourceTranslator }
  TLoadFailEvent = Reference to Procedure (Sender : TObject; aCode : Integer; aError : String);
  TOnTranslatedEvent = Reference to Procedure (Sender : TObject; aURL : String);

  TResourceTranslator = Class
  Private
    FOnLoadFail: TLoadFailEvent;
    FOnURLTranslated: TOnTranslatedEvent;
    class var FInstance : TResourceTranslator;
    function GetUnitResources(const aUnit: string): TJSOBject;
  Protected
    procedure ResetTranslation(aUnitResources: TJSObject; AString: String); virtual;
    procedure Translate(aUnitResources: TJSObject; AString, aTranslation: String); virtual;
  Public
    Class Function Instance : TResourceTranslator;
    Procedure Translate(Const aUnit,aString,aTranslation : String); overload;
    Procedure Translate(Const aTranslations : TJSOBject); overload;
    Procedure ResetTranslation(Const aUnit : String; const aString : String = ''); overload;
    procedure Translate(const aURL: string; aOnTranslated : TOnTranslatedEvent = Nil); overload;
    Property OnLoadFail : TLoadFailEvent Read FOnLoadFail Write FOnLoadFail;
    Property OnURLTranslated : TOnTranslatedEvent Read FOnURLTranslated Write FOnURLTranslated;
  end;

Function ResourceTranslator : TResourceTranslator;
Procedure Translate(Const aURL : String; aOnTranslated : TOnTranslatedEvent = Nil);
Procedure Translate(Const aUnit,aString,aTranslation : String);
Procedure Translate(Const aTranslations : TJSOBject);
Procedure ResetTranslation(Const aUnit : String; Const aString : String = '');

implementation

Const
  SCurrent = 'current';

var
  Pas : TJSObject; external name 'pas';

{ TResourceTranslator }


procedure TResourceTranslator.Translate(aUnitResources: TJSObject; AString, aTranslation: String);

Var
  res: JSValue;

begin
  res:=aUnitResources[aString];
  if Assigned(Res) then
    TJSOBject(Res)[SCurrent]:=aTranslation;
end;

procedure TResourceTranslator.ResetTranslation(aUnitResources: TJSObject; AString: String);

Var
  res: JSValue;

begin
  res:=aUnitResources[aString];
  if Assigned(Res) then
    TJSOBject(Res)[SCurrent]:=undefined;
end;


function TResourceTranslator.GetUnitResources(const aUnit: string): TJSOBject;

var
  jsMod,res : JSValue;

begin
  Result:=Nil;
  jsMod:=Pas[aUnit];
  if assigned(jsMod) then
    begin
    res:=(TJSObject(jsMod)['$resourcestrings']);
    if Assigned(res) then
      Result:=TJSObject(res);
    end;
end;

procedure TResourceTranslator.Translate(const aTranslations: TJSOBject);

var
  aUnitName,aStringName : String;
  aUnit,Res : TJSObject;
  V : JSValue;

begin
  for aUnitName in TJSObject.getOwnPropertyNames(aTranslations) do
    if IsValidIdent(aUnitName,True) and isObject(aTranslations[aUnitName]) then
      begin
      aUnit:=TJSObject(aTranslations[aUnitName]);
      Res:=GetUnitResources(aUnitName);
      if Assigned(Res) then
        For aStringName in TJSObject.getOwnPropertyNames(aUnit) do
          begin
          V:=aUnit[aStringName];
          if IsString(V) then
            Translate(Res,aStringName,String(V));
          end;
      end;
end;

procedure TResourceTranslator.Translate(const aUnit, aString, aTranslation: String);

Var
  jsmod : TJSObject;

begin
  jsMod:=GetUnitResources(aUnit);
  if Assigned(jsMod) then
    Translate(jsMod, aString,aTranslation);
end;

procedure TResourceTranslator.ResetTranslation(const aUnit: String; const aString: String);

Var
  jsmod : TJSObject;
  S : String;

begin
  jsMod:=GetUnitResources(aUnit);
  if Assigned(jsMod) then
    if (aString<>'') then
      ResetTranslation(jsMod, aString)
    else
      for S in TJSOBject.getOwnPropertyNames(TJSObject(jsMod)) do
        ResetTranslation(jsMod, S);
end;

Type

  { TURLTranslator }

  TURLTranslator = Class
  Private
    FTranslator : TResourceTranslator;
    FURL : String;
    FXHR : TJSXMLHttpRequest;
    FOnTranslated : TOnTranslatedEvent;
    procedure DoStateChange;
  Public
    Constructor Create(aUrl : String; ATranslator : TResourceTranslator;aOnTranslated : TOnTranslatedEvent);
    Procedure Translate;
  end;

{ TURLTranslator }

procedure TURLTranslator.DoStateChange;

Var
  O : TJSObject;

begin
  if (FXHR.ReadyState=TJSXMLHttpRequest.DONE) then
    if ((FXHR.Status div 100)=2) then
      begin
      try
        O:=TJSJSON.parseObject(FXHR.responseText);
        FTranslator.Translate(O);
        if Assigned(FOnTranslated) then
          FOnTranslated(FTranslator,FURL);
        if Assigned(FTranslator.OnURLTranslated) then
          FTranslator.OnURLTranslated(FTranslator,FURL);
      except
        if Assigned(FTranslator.OnLoadFail) then
          FTranslator.OnLoadFail(FTranslator,0,'Invalid JSON')
      end
      end
    else
      if Assigned(FTranslator.OnLoadFail) then
        FTranslator.OnLoadFail(FTranslator,FXHR.Status,FXHR.StatusText);
end;

constructor TURLTranslator.Create(aUrl: String; ATranslator: TResourceTranslator;aOnTranslated : TOnTranslatedEvent);
begin
  FURL:=aURL;
  FTranslator:=ATranslator;
  FOnTranslated:=aOnTranslated;
end;

procedure TURLTranslator.Translate;
begin
  FXHR:=TJSXMLHttpRequest.new;
  FXHR.open('GET',fURL);
  FXHR.onreadystatechange:=@DoStateChange;
  FXHR.responseType:='text';
  FXHR.send;
end;

procedure TResourceTranslator.Translate(const aURL : string;aOnTranslated : TOnTranslatedEvent = Nil);

begin
  With TURLTranslator.Create(aURL,Self,aOnTranslated) do
    Translate;
end;

class function TResourceTranslator.Instance: TResourceTranslator;
begin
  if FInstance=Nil then
    FInstance:=TResourceTranslator.Create;
  Result:=FInstance;
end;

{ ---------------------------------------------------------------------
  Procedural access
  ---------------------------------------------------------------------}

Procedure Translate(Const aURL : String; aOnTranslated : TOnTranslatedEvent = Nil);

begin
  TResourceTranslator.Instance.Translate(aURL,aOnTranslated);
end;

Procedure Translate(Const aTranslations : TJSOBject);

begin
  TResourceTranslator.Instance.Translate(aTranslations);
end;

procedure Translate(const aUnit, aString, aTranslation: String);
begin
  TResourceTranslator.Instance.Translate(aUnit,aString,aTranslation);
end;

Procedure ResetTranslation(Const aUnit : String; Const aString : String = '');
begin
  TResourceTranslator.Instance.ResetTranslation(aUnit,Astring);
end;

Function ResourceTranslator : TResourceTranslator;
begin
  Result:=TResourceTranslator.Instance;
end;

end.

