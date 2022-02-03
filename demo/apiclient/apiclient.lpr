program apiclient;

{$mode objfpc}

uses
  browserapp, JS, Classes, SysUtils, Web, fpjson, fpjsonjs, fprpccodegen;

type

  { TMyApplication }

  TMyApplication = class(TBrowserApplication)
    edtResult : TJSHTMLTextAreaElement;
    edtURL : TJSHTMLInputElement;
    edtUnit : TJSHTMLInputElement;
    cbPreferNativeInt : TJSHTMLInputElement;
    cbForceJSValueResult : TJSHTMLInputElement;
    btnGenerate : TJSHTMLButtonElement;
    procedure BindElements;
    procedure doRun; override;
  private
    function DoGenerateCode(aEvent: TJSMouseEvent): boolean;
    procedure GenerateAPI(const aJSON: String);
  end;

procedure TMyApplication.BindElements;
begin
  edtResult:=TJSHTMLTextAreaElement(GetHTMLElement('edtResult'));
  edtURL:=TJSHTMLInputElement(GetHTMLElement('edtURL'));
  edtUnit:=TJSHTMLInputElement(GetHTMLElement('edtUnit'));
  cbPreferNativeInt:=TJSHTMLInputElement(GetHTMLElement('cbPreferNativeInt'));
  cbForceJSValueResult:=TJSHTMLInputElement(GetHTMLElement('cbForceJSValueResult'));
  btnGenerate:=TJSHTMLButtonElement(GetHTMLElement('btnGenerate'));
  btnGenerate.OnClick:=@DoGenerateCode;
end;

procedure TMyApplication.doRun;

begin
  BindElements;
  Terminate;
end;

Procedure TMyApplication.GenerateAPI(const aJSON: String);

Var
  API : TJSONObject;
  Gen : TAPIClientCodeGen;
  Opts : TClientCodeOptions;

begin
  API:=GetJSON(aJSON) as TJSONObject;
  Opts:=[];
  if cbForceJSValueResult.checked then
    Include(Opts,ccoForceJSValueResult);
  if cbPreferNativeInt.Checked then
    Include(Opts,ccoPreferNativeInt);
  Gen:=TAPIClientCodeGen.Create(Self);
  try
    Gen.API:=API;
    Gen.Options:=Opts;
    Gen.OutputUnitName:=edtUnit.Value;
    Gen.Execute;
    edtResult.value:=Gen.Source.Text;
  finally
    Gen.Free;
  end;
end;

function TMyApplication.DoGenerateCode(aEvent: TJSMouseEvent): boolean;

  procedure GenAPI(Resp : TJSResponse); async;

  begin
    GenerateAPI(Await(Resp.text()));
  end;

  function DoOK(aValue: JSValue): JSValue;

  var
    Resp : TJSResponse absolute aValue;

  begin
    Result:=undefined;
    GenAPI(Resp)
  end;

  function DoFail(aValue: JSValue): JSValue;
  begin
    Result:=undefined;
    window.alert('Failed to fetch API description at URL '+edtURL.value)
  end;

begin
  Result:=True;
  window.fetch(edtURL.Value,TJSObject.New)._then(@DoOK,@DoFail);
end;

var
  Application : TMyApplication;

begin
  Application:=TMyApplication.Create(nil);
  Application.Initialize;
  Application.Run;
end.
