unit p2jsres;

{$mode objfpc}
{$h+}
{$modeswitch externalclass}

interface

uses types;

Type
  TResourceSource = (rsJS,rsHTML);
  TResourceInfo = record
    name : string;
    encoding : string;
    resourceunit : string;
    format : string;
    data : string;
  end;
  TResourceEnumCallBack = Reference to function(const resName : string) : boolean;
  TResourcesLoadedEnumCallBack = Reference to Procedure(const LoadedResources : Array of String);
  TResourcesLoadErrorCallBack = Reference to Procedure(const aError : string);

Function SetResourceSource(aSource : TResourceSource) : TResourceSource;

Function GetResourceNames : TStringDynArray;
Function GetResourceNames(aSource : TResourceSource) : TStringDynArray;

Function EnumResources(aCallback : TResourceEnumCallBack) : Integer;
Function EnumResources(aSource : TResourceSource; aCallback : TResourceEnumCallBack) : Integer;

Function GetResourceInfo(Const aName : String; var aInfo : TResourceInfo) : Boolean;
Function GetResourceInfo(aSource : TResourceSource; Const aName : String; var aInfo : TResourceInfo) : Boolean;

Procedure LoadHTMLLinkResources(const aURL : String; OnLoad : TResourcesLoadedEnumCallBack = Nil; OnError : TResourcesLoadErrorCallBack = Nil);

implementation

uses sysutils, js, web;

var
  gMode: TResourceSource;

{ ---------------------------------------------------------------------
  Global entry points
  ---------------------------------------------------------------------}

Function SeTResourceSource(aSource : TResourceSource) : TResourceSource;
begin
  Result:=gMode;
  gMode:=aSource;
end;

Function GetResourceNames : TStringDynArray;
begin
  Result:=GetResourceNames(gMode);
end;

Function EnumResources(aCallback : TResourceEnumCallBack) : Integer;

begin
  Result:=EnumResources(gMode,aCallback);
end;

Function GetResourceInfo(Const aName : String; var aInfo : TResourceInfo) : Boolean;

begin
  Result:=GetResourceInfo(gMode,aName,aInfo);
end;

{ ---------------------------------------------------------------------
  JS resources
  ---------------------------------------------------------------------}

Type
  TRTLResourceInfo = class external name 'Object' (TJSObject)
    name : string;
    encoding : string;
    resourceunit : string; external name 'unit';
    format : string;
    data : string;
  end;

function rtlGetResourceList : TStringDynArray; external name 'rtl.getResourceList';
function rtlGetResource(const aName : string) : TRTLResourceInfo; external name 'rtl.getResource';

Function GetRTLResourceInfo(Const aName : String; var aInfo : TResourceInfo) : Boolean;

Var
  RTLInfo : TRTLResourceInfo;

begin
  RTLInfo:=rtlGetResource(lowercase(aName));
  Result:=Assigned(RTLInfo);
  if Result then
    begin
    aInfo.name:=RTLinfo.name;
    aInfo.encoding:=RTLinfo.encoding;
    aInfo.format:=RTLinfo.format;
    aInfo.resourceunit:=RTLinfo.resourceunit;
    aInfo.data:=RTLinfo.data;
    end;
end;

{ ---------------------------------------------------------------------
  HTML resources
  ---------------------------------------------------------------------}

Const
  IDPrefix = 'resource-';

Function IsResourceLink(L : TJSHTMLLinkElement) : Boolean;

begin
  Result:=(Copy(L.id,1,Length(IDPrefix))=IDPrefix) and (isDefined(L.Dataset['unit'])) and (Copy(L.href,1,4)='data')
end;

Function GetHTMLResources : TStringDynArray;


Var
  LC : TJSHTMLCollection;
  L : TJSHTMLLinkElement;
  I : Integer;
  ID : String;

begin
  SetLength(Result,0);
  if not isDefined(document) then // If called in Node...
    exit;
  // No cache, we do this dynamically: it's possible to add link nodes at runtime.
  LC:=document.getElementsByTagName('link');
  For I:=0 to LC.length-1 do
    begin
    L:=TJSHTMLLinkElement(LC[i]);
    ID:=L.ID;
    if IsResourceLink(L) then
      begin
      Delete(ID,1,Length(IDPrefix));
      if (ID<>'') then
        TJSArray(Result).Push(ID);
      end;
    end;
end;

Function GetHTMLResourceInfo(Const aName : String; var aInfo : TResourceInfo) : Boolean;

Var
  el : TJSElement;
  L : TJSHTMLLinkElement absolute el;
  S : String;
  I : Integer;

begin
  Result:=False;
  if not isDefined(document) then // If called in Node...
    exit;
  El:=document.getElementByID(IDPrefix+lowercase(aName));
  Result:=assigned(el) and SameText(el.tagName,'link');
  if not Result then
    exit;
  ainfo.name:=lowercase(aName);
  ainfo.Resourceunit:=String(L.Dataset['unit']);
  S:=L.href;
  S:=Copy(S,6,Length(S)-5); // strip data;
  I:=Pos(',',S);
  aInfo.data:=Copy(S,I+1,Length(S)-1);
  S:=copy(S,1,I-1);
  I:=Pos(';',S);
  if I=0 then
    aInfo.encoding:=''
  else
    begin
    aInfo.encoding:=Copy(S,I+1,Length(S)-1);
    S:=Copy(S,1,I-1);
    end;
  aInfo.Format:=S;
end;

Function HasTemplate : Boolean;

begin
  asm
   return ('content' in document.createElement('template'))
  end;
end;


Procedure LoadHTMLLinkResources(const aURL : String; OnLoad : TResourcesLoadedEnumCallBack = Nil; OnError : TResourcesLoadErrorCallBack = Nil);

  function FetchOK(Res : JSValue) : JSValue;
  var
    Response : TJSResponse absolute res;
  begin
    Result:=Nil;
    if not Response.ok then
      begin
      if Assigned(OnError) then
        Raise TJSError.New('HTTP Error for URL aURL, status = '+IntToStr(Response.status)+' : '+Response.statusText)
      end
    else
      Result:=Response.Text();
  end;

  function BlobOK(Res : JSValue) : JSValue;

  Var
    aText : String absolute res;
    ID : String;
    Tmpl : TJSHTMLTemplateElement;
    El : TJSHTMLElement;
    L : TJSHTMLLinkElement absolute El;
    Arr : TStringDynArray;
    aParent : TJSHTMLElement;

  begin
    Result:=Nil;
    aParent:=TJSHTMLElement(document.head);
    if aParent=Nil then
      aParent:=TJSHTMLElement(document.body);
    SetLength(Arr,0);
    Tmpl:=TJSHTMLTemplateElement(Document.createElement('template'));
    Tmpl.innerhtml:=TJSString(aText).trim;
    el:=TJSHTMLElement(Tmpl.Content.firstElementChild);
    While El<>Nil do
      begin
      if SameText(El.tagName,'link') and IsResourceLink(L) then
        begin
        aParent.Append(TJSHTMLElement(document.importNode(l,true)));
        ID:=L.ID;
        Delete(ID,1,Length(IDPrefix));
        if (ID<>'') then
          TJSArray(Arr).Push(ID);
        end;
      el:=TJSHTMLElement(el.nextElementSibling);
      end;
    if Assigned(OnLoad) then
      OnLoad(Arr);
  end;

  function DoError (aValue : JSValue) : JSValue;

  Var
    aErr : TJSError absolute aValue;

  begin
    Result:=Nil;
    if Assigned(OnError) then
      if aErr=Nil then
        OnError('Error: ' + aErr.message)
  end;

begin
  if not HasTemplate then
    begin
    if Assigned(OnError) then
      OnError('No template support in this browser')
    end
  else
    window.fetch(aURL)._then(@FetchOK)._then(@BlobOK).catch(@doError);
end;


{ ---------------------------------------------------------------------
  Global entries, specifying resource mode
  ---------------------------------------------------------------------}

Function GetResourceNames(aSource : TResourceSource) : TStringDynArray;

begin
  case aSource of
    rsJS : Result:=rtlGetResourceList;
    rsHTML : Result:=GetHTMLResources;
  end;
end;

Function EnumResources(aSource : TResourceSource; aCallback : TResourceEnumCallBack) : Integer;

Var
  RL : TStringDynArray;
  I : Integer;
  ContinueEnum : Boolean;

begin
  Result:=0;
  RL:=GetResourceNames(aSource);
  I:=0;
  Result:=Length(RL);
  ContinueEnum:=True;
  While (I<Result) and ContinueEnum do
    begin
    ContinueEnum:=aCallBack(RL[i]);
    Inc(I);
    end;
end;


Function GetResourceInfo(aSource : TResourceSource; Const aName : String; var aInfo : TResourceInfo) : Boolean;

begin
  case aSource of
    rsJS : Result:=GetRTLResourceInfo(aName,aInfo);
    rsHTML : Result:=GetHTMLResourceInfo(aName,aInfo);
  end;
end;


end.
