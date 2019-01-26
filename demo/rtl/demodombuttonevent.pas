program demodombuttonevent;

uses web, classes, libjquery;

Type
  TForm = Class
    function ButtonClick(Event: TJSMouseEvent): boolean;
    Constructor Create;
  end;

function TForm.ButtonClick(Event: TJSMouseEvent): boolean;
begin
  writeln('ButtonClick ',Event,' in ',className);
  window.alert('Hello world from Pascal!');
  Result:=true;
end;

constructor TForm.Create;
Var
  Panel,PanelContent : TJSElement;
  Button1:TJSElement;
begin
  Panel:=document.createElement('div');
  // attrs are default array property...
  Panel['class']:='panel panel-default';
  PanelContent:=document.createElement('div');
  PanelContent['class']:='panel-body';
  Button1:=document.createElement('input');
  Button1['id']:='Button1';
  Button1['type']:='submit';
  Button1['class']:='btn btn-default';
  Button1['value']:='Click me!';
  TJSHTMLElement(Button1).onclick:=@ButtonClick;
  document.body.appendChild(Panel);
  Panel.appendChild(PanelContent);
  PanelContent.appendChild(Button1);
end;

begin
  TForm.Create;
end.

