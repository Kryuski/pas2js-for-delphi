{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2019 by Michael Van Canneyt

    This unit implements a HTML template loader.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Rtl.TemplateLoader;

{$mode objfpc}

interface

uses
  Classes, SysUtils, JS, web;

Type
   TFailData = Record
     message : string;
     code : Integer;
   end;

  TTemplateNotifyEvent = Reference to Procedure(Sender : TObject; Const aTemplate : String);
  TTemplateErrorNotifyEvent = Reference to Procedure(Sender : TObject; Const aTemplate,aError : String; aErrorcode : Integer);

  { TCustomTemplateLoader }

  TCustomTemplateLoader = Class(TComponent)
  Private
    FBaseURL: String;
    FOnLoad: TTemplateNotifyEvent;
    FOnLoadFail: TTemplateErrorNotifyEvent;
    FTemplates : TJSObject;
    function GetTemplate(aName : String): String;
    procedure SetTemplate(aName : String; AValue: String);
  Protected
    // Process an url before it is used to fetch data
    Function ProcessURL(const aURL : String) : String;
  Public
    Constructor Create (aOwner : TComponent); override;
    Destructor Destroy; override;
    // Remove a template
    Procedure RemoveRemplate(aName : String);
    // fetch a template using promise. Promise resolves to template name. On fail a TFailData record is passed on.
    // Note that the global OnLoad/OnLoadFail a
    Function FetchTemplate(Const aName,aURL : String) : TJSPromise;
    // procedural API.
    // If the aOnSuccess aOnFail event handlers are specified, they're called as well in addition to global handlers.
    Procedure LoadTemplate(Const aName,aURL : String; aOnSuccess : TTemplateNotifyEvent = Nil; AOnFail : TTemplateErrorNotifyEvent= Nil);
    // procedural API for multiple templates at once.
    // Form = name, URL, name URL.
    // If the aOnSuccess aOnFail event handlers are specified, they're called as well in addition to global handlers.
    Procedure LoadTemplates(Const Templates : Array of String; aOnSuccess : TTemplateNotifyEvent = Nil; AOnFail : TTemplateErrorNotifyEvent= nil);
    // URLs will be relative to this. Take care that you add a / at the end if needed !
    Property BaseURL : String Read FBaseURL Write FBaseURl;
    Property Templates[aName : String] : String Read GetTemplate Write SetTemplate; default;
    // Called when a template was loaded.
    Property OnLoad : TTemplateNotifyEvent Read FOnLoad Write FOnLoad;
    // Called when a template failed to load.
    Property OnLoadFail : TTemplateErrorNotifyEvent Read FOnLoadFail Write FOnLoadFail;
  end;

  TTemplateLoader = Class(TCustomTemplateLoader)
  Published
    Property BaseURL;
    Property OnLoad;
    Property OnLoadFail;
  end;

// Global instance, for ease of use.
Function GlobalTemplates : TCustomTemplateLoader;

implementation

{ TCustomTemplateLoader }

Var
  _loader : TCustomTemplateLoader;

Function GlobalTemplates : TCustomTemplateLoader;

begin
  if _loader=Nil then
    _loader:=TCustomTemplateLoader.Create(Nil);
  Result:=_Loader;
end;

Type
   { TURLLoader }

   TURLLoader = Class(TObject)
   private
     FLoader: TCustomTemplateLoader;
     FName: String;
     FURL: String;
     procedure dofetch(resolve, reject: TJSPromiseResolver);
   Public
     Constructor Create(aLoader : TCustomTemplateLoader; aName,aURL : String);
     Function fetch : TJSPromise;
     Property Name : String Read FName;
     Property URL : String Read FURL;
     Property Loader : TCustomTemplateLoader Read FLoader;
   end;


{ TURLLoader }

constructor TURLLoader.Create(aLoader: TCustomTemplateLoader; aName, aURL: String);
begin
  FLoader:=aLoader;
  FURL:=aURL;
  FName:=aName;
end;

procedure TURLLoader.dofetch(resolve,reject : TJSPromiseResolver);

  function doOK(response : JSValue) : JSValue;

  var
    Res : TJSResponse absolute response;
    F : TFailData;

  begin
    If (Res.status<>200) then
      begin
      F.Message:=res.StatusText;
      F.Code:=Res.Status;
      Result:=Reject(F);
      end
    else
      Res.text._then(
        function (value : JSValue) : JSValue
          begin
          Loader.Templates[FName]:=String(Value);
          if Assigned(Loader.FonLoad) then
            Loader.FOnLoad(FLoader,Name);
          Result:=Resolve(Name);
          end
      );
  end;

  function doFail(response : JSValue) : JSValue;

  Var
    F : TFailData;

  begin
    F.message:='unknown error';
    F.code:=999;
    Result:=Reject(F);
  end;

begin
  Window.Fetch(URl)._then(@DoOK).catch(@DoFail);
end;

function TURLLoader.fetch : TJSPromise;

begin
  Result:=TJSPromise.New(@Dofetch)
end;

function TCustomTemplateLoader.GetTemplate(aName : String): String;

Var
  V : jsValue;

begin
  V:=FTemplates[LowerCase(aName)];
  if isString(V) then
    Result:=String(V)
  else
    Result:='';
end;

procedure TCustomTemplateLoader.SetTemplate(aName : String; AValue: String);
begin
  FTemplates[LowerCase(aName)]:=AValue;
end;

function TCustomTemplateLoader.ProcessURL(const aURL: String): String;

Var
  R : TJSRegexp;

begin
  R:=TJSRegexp.New('^https?://|^/','i');
  if R.Test(aURL) then
    Result:=aURL
  else
    Result:=BaseURL+aURL;
end;

constructor TCustomTemplateLoader.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FTemplates:=TJSObject.New;
end;

destructor TCustomTemplateLoader.Destroy;
begin
  FTemplates:=nil;
  inherited Destroy;
end;

procedure TCustomTemplateLoader.RemoveRemplate(aName: String);
begin
  jsDelete(FTemplates,Lowercase(aName));
end;

function TCustomTemplateLoader.FetchTemplate(const aName, aURL: String): TJSPromise;

begin
  Result:=TURLLoader.Create(Self,aName,ProcessURL(aURL)).fetch;
end;

procedure TCustomTemplateLoader.LoadTemplate(const aName, aURL: String; aOnSuccess: TTemplateNotifyEvent;
  AOnFail: TTemplateErrorNotifyEvent);

  function doOK(aValue : JSValue) : JSValue;

  begin
    if Assigned(aOnSuccess) then
      aOnSuccess(Self,aName);
    Result:=nil;
  end;

  function doFail(aValue : JSValue) : JSValue;

  Var
    F : TFailData absolute aValue;
    S : String;
    C : Integer;

  begin
    S:=F.message;
    C:=F.Code;
    if Assigned(FonLoadFail) then
      FOnLoadFail(Self,aName,S,C);
    if Assigned(aOnFail) then
      aOnFail(Self,aName,S,C);
    Result:=nil;
  end;

begin
  FetchTemplate(aName,aURL)._then(@DoOK).catch(@doFail);
end;

procedure TCustomTemplateLoader.LoadTemplates(const Templates: array of String; aOnSuccess: TTemplateNotifyEvent;
  AOnFail: TTemplateErrorNotifyEvent);

Var
  I,L : Integer;

begin
  L:=Length(Templates);
  if (L mod 2)<>0 then
    Raise Exception.CreateFmt('Number of arguments (%d) must be even',[L]);
  I:=0;
  While I<L do
   begin
   LoadTemplate(Templates[I],Templates[I+1],aOnsuccess,aOnFail);
   Inc(I,2);
   end;
end;

end.

