program convert;
{$mode objfpc}

uses typinfo, types, web, js, webwidget, browserapp;

var
  dtsfiles : TStringDynArray; external name 'dtsfiles';

Type

  TConversionOption = (coRaw,coGenericArrays,coUseNativeTypeAliases,coLocalArgumentTypes, coUntypedTuples, coDynamicTuples,
                    coExternalConst,coExpandUnionTypeArgs,coaddOptionsToheader,coInterfaceAsClass,coSkipImportStatements);

  { TConvertApplication }

  TConvertApplication = class(TBrowserApplication)
  Private 
    edtFileInput : TJSHTMLInputElement;
    edtUnitName : TJSHTMLInputElement;
    btnGo : TJSHTMLButtonElement;
    edtSource : TJSHTMLTextAreaElement;
    divMenu : TJSHTMLElement;
    cbOptions : Array[TConversionOption] of TJSHTMLInputElement;
    cbPrependLog : TJSHTMLInputElement;
    function GetQueryOptions: String;
  Protected
    Function LogGetElementErrors : Boolean; override;
    Procedure DoGoClick(aEvent : TJSEvent);
    Procedure DoAnchorClick(aEvent : TJSEvent);
    Procedure DoInputChange(aEvent : TJSEvent);
    Procedure DoInputExit(aEvent : TJSEvent);
    Procedure DoRun; override;
  end;
 
function TConvertApplication.GetQueryOptions: String;

  Procedure AddToRes(N,V : String);

  begin
    if V='' then
      exit;
    if Result<>'' then
      Result:=Result+'&';
    Result:=Result+N+'='+V;
  end;

Var
  T : TConversionOption;

begin
  AddToRes('file',edtFileInput.value);
  AddToRes('unit',edtUnitName.value);
  if cbPrependLog.Checked then
    AddToRes('prependlog','1');
  For T in TConversionoption do
    if cbOptions[T].checked then
      AddToRes(cbOptions[T].ID,'1');
end;

function TConvertApplication.LogGetElementErrors: Boolean;
begin
  Result:=true;
end;

procedure TConvertApplication.DoGoClick(aEvent: TJSEvent);
 
  function haveText(res : JSValue) : JSValue;
  
  begin
    edtSource.Value:=String(res);
  end; 
 
  function DoFetchOK (res : JSValue) : JSValue;
 
  var
    Resp : TJSResponse absolute res;
     
  begin
    Resp.Text()._then(@HaveText);
    Result:=True;
  end;
  
begin
  divMenu.style['display']:='none'; 
  window.fetch('convcgi.cgi/convert/?'+GetQueryOptions)._then(@DoFetchOK)
end;

procedure TConvertApplication.DoAnchorClick(aEvent: TJSEvent);

var
  a : TJSHTMLAnchorElement;

begin
  aEvent.preventDefault();
  a:=TJSHTMLAnchorElement(aEvent.target);
  edtFileInput.Value:=a.innertext;
  divmenu.style['display']:='none';
end;

procedure TConvertApplication.DoInputExit(aEvent: TJSEvent);

var
  FE: TJSFocusEvent absolute aEvent;

begin
  if (FE.relatedTarget=Nil) or (Pos('dropdown-item',FE.relatedTarget.className)=0) then
    divmenu.style['display']:= 'none'
end;

procedure TConvertApplication.DoInputChange(aEvent: TJSEvent);

var
  inp,S : String;
  a : TJSHTMLAnchorElement;
  aCount : Integer;
  
begin
  inp:=edtFileInput.Value;
  if length(inp)<2 then exit;
  divMenu.style['display']:='none';
  divMenu.innerHTML:='<div class="dropdown-content"></div>';
  aCount:=0;
  for S in dtsFiles do
    if Pos(Inp,S)<>0 then
      begin
      a:=TJSHTMLAnchorElement(Window.Document.CreateElement('a'));
      a.className:='dropdown-item';
      a.href:='#';
      a.InnerText:=S;
      a.addEventListener('click',@DoAnchorClick);
      divMenu.childNodes[0].appendChild(a);
      Inc(aCount);
      end;
  if aCount>0 then
    divMenu.style['display']:='block';    
end;

procedure TConvertApplication.DoRun;

Var
  T : TConversionOption;
  N : String;

begin
  edtFileInput:=TJSHTMLInputElement(GetHTMLElement('edtfilename'));
  edtFileInput.addEventListener('input',@DoInputChange);
  edtFileInput.addEventListener('focusout',@DoInputexit);
  edtUnitName:=TJSHTMLInputElement(GetHTMLElement('edtunitname'));
  btnGo:=TJSHTMLButtonElement(GetHTMLElement('btnGo'));
  btnGo.AddEventListener('click',@DoGoClick);
  edtSource:=TJSHTMLTextAreaElement(GetHTMLElement('edtSource'));
  cbPrependLog:=TJSHTMLInputElement(GetHTMLElement('cbPrependLog'));
  divMenu:=GetHTMLElement('file-menu');
  for T in TConversionOption do
    begin
    N:=GetEnumName(TypeInfo(TConversionOption),Ord(T));
    cbOptions[T]:=TJSHTMLInputElement(GetHTMLElement(N));
    end;
end;
 
Var
 Application : TConvertApplication;  

begin
  Application:= TConvertApplication.Create(Nil);
  Application.Initialize;
  Application.Run;
end.
