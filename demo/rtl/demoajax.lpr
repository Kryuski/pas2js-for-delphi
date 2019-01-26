program demoajax;

uses SysUtils, Web, ajax;

Type

  { TForm }

  TForm = Class
    function onLoad(Event: TEventListenerEvent): boolean;
    Constructor Create;
  end;


function TForm.onLoad(Event: TEventListenerEvent): boolean;
var
  lPanel: TJSElement;
  lStatus: longint;
begin
  lStatus := TJSXMLHttpRequest(event.target).Status;
  lPanel := document.createElement('div');
  if(lStatus = 404) then
    lPanel['style'] := 'width: 100px; height: 100px; border: 4px solid red;'
  else
    lPanel['style'] := 'width: 100px; height: 100px; border: 4px solid green;';
  document.body.appendChild(lPanel);
  console.log(TJSXMLHttpRequest(event.target).Status);
  Result := True;
end;

constructor TForm.Create;

var
  lAjax: TAjax;

begin
  lAjax := TAjax.Create;
  lAjax.OnLoad := @onLoad;
  lAjax.Open('GET','demoajax2.html');
end;

begin
  TForm.Create;
end.

