program democlasstopas;

uses Sysutils, Types, Web, Classes, JS, browserconsole, class2pas;

Type

  { TGenCodeApp }

  TGenCodeApp = Class
    elHead : TJSHTMLElement;
    btnGo : TJSHTMLElement;
    btnLoad : TJSHTMLElement;
    edtJSObject : TJSHTMLInputElement;
    edtScript : TJSHTMLInputElement;
    edtPascalClass : TJSHTMLInputElement;
    edtPascalParentClass : TJSHTMLInputElement;
    edtExternalName : TJSHTMLInputElement;
    edtClassDefinition : TJSHTMLTextAreaElement;
    Procedure Execute;
    procedure ShowRTLProps(aClassName,aParentClassName,aJSClassName : String; O : TJSObject);
  private
    function DoGenCode(aEvent: TJSMouseEvent): boolean;
    function DoLoad(aEvent: TJSMouseEvent): boolean;
    function FindObject(aPath: String): TJSObject;
  end;

procedure TGenCodeApp.ShowRTLProps(aClassName,aParentClassName,aJSClassName : String; O : TJSObject);

Var
  S : TStrings;

begin
  S:=TStringList.Create;
  try
    ClassToPas(aJSClassName,aClassName,aParentClassName,O,S,True);
    edtClassDefinition.value:=S.Text;
  finally
    S.Free;
  end;
end;

function TGenCodeApp.FindObject(aPath : String): TJSObject;

Var
  p : JSValue;
  O : TJSObject;
  Path : TStringDynArray;
  Done,S : String;

begin
  Path:=aPath.Split('.');
  Result:=nil;
  O:=Window;
  Done:='';
  for S in Path do
    begin
    if Done<>'' then
      Done:=Done+'.';
    Done:=Done+S;
    p:=O.Properties[S];
    if Not Assigned(P) then
      begin
      Window.Alert('No object found at : '+Done);
      exit;
      end;
    if Not isObject(P) then
      begin
      Window.Alert('Value at : '+Done+' is not an object');
      exit;
      end;
    O:=TJSObject(P);
    end;
  Result:=O;
end;

function TGenCodeApp.DoGenCode(aEvent: TJSMouseEvent): boolean;

var
  O : TJSObject;

begin
  Result:=False;
  if (edtPascalClass.value='') or (edtJSObject.Value='') or (edtExternalName.Value='') then
    begin
    Window.Alert('Please fill in all fields');
    exit;
    end;
  O:=FindObject(edtJSObject.Value);
  if Assigned(O) then
    ShowRTLProps(edtPascalClass.value,edtPascalParentClass.Value,edtExternalName.Value,O);
end;

function TGenCodeApp.DoLoad(aEvent: TJSMouseEvent): boolean;

Var
  El : TJSElement;

begin
  if (edtScript.Value='') then
    begin
    Window.Alert('Please fill in URL');
    exit;
    end;
  El:=Document.createElement('script');
  EL.Properties['src']:=edtScript.Value;
  elHead.appendChild(El);
end;

Procedure TGEncodeApp.Execute;

begin
  elHead:=TJSHTMLElement(Document.GetElementByID('head'));
  btnGo:=TJSHTMLButtonElement(Document.GetElementByID('go'));
  btnLoad:=TJSHTMLButtonElement(Document.GetElementByID('load'));
  edtJSObject:=TJSHTMLInputElement(Document.GetElementByID('edtJSObject'));
  edtScript:=TJSHTMLInputElement(Document.GetElementByID('edtScript'));
  edtPascalClass:=TJSHTMLInputElement(Document.GetElementByID('edtPascalClass'));
  edtPascalParentClass:=TJSHTMLInputElement(Document.GetElementByID('edtPascalClassAncestor'));
  edtExternalName:=TJSHTMLInputElement(Document.GetElementByID('edtExternalName'));
  edtClassDefinition:=TJSHTMLTextAreaElement(Document.GetElementByID('edtClassDefinition'));
  btnGo.onclick:=@DoGenCode;
  btnLoad.onclick:=@DoLoad;
end;


begin
  With TGenCodeApp.Create do
    Execute;
end.

