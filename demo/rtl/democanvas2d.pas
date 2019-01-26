program democanvas2d;

uses Web, Classes, JS, SysUtils;

Type
  TForm = Class
    Canvas2D : TJSCanvasRenderingContext2D;
    Ex : TJSHTMLInputElement;
    Ey : TJSHTMLInputElement;
    EHeight : TJSHTMLInputElement;
    EWidth  : TJSHTMLInputElement;
    ELineWidth  : TJSHTMLInputElement;
    function ButtonClick(Event{%H-}: TJSMouseEvent): boolean;
    Constructor Create; reintroduce;
  end;

function TForm.ButtonClick(Event: TJSMouseEvent): boolean;

Var
  X,Y,W,H : Double;

begin
  writeln('Drawing rectangle');
  X:=StrToFloat(EX.value);
  Y:=StrToFloat(Ey.value);
  W:=StrToFloat(EWidth.value);
  H:=StrToFloat(EHeight.value);
  Canvas2D.lineWidth:=ParseFloat(ELineWidth.value);
  Canvas2D.rect(X,Y, W,H);
  Canvas2D.stroke;
  Result:=true;
end;

constructor TForm.Create;

  Function CreateNumberEdit (aName : String) : TJSHTMLInputElement;

  begin
    Result:=TJSHTMLInputElement(document.createElement('input'));
    Result['type']:='text';
    Result.value:='100';
    Result.name:=aName;
    Result['style']:='width: 80px;';
  end;
Var
  Panel,PanelContent : TJSElement;
  Button1:TJSElement;
  Canvas : TJSHTMLCanvasElement;

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
  Button1['value']:='Draw rectangle';
  TJSHTMLElement(Button1).onclick:=@ButtonClick;
  EHeight:=CreateNumberEdit('Height');
  EWidth:=CreateNumberEdit('Width');
  ELineWidth:=CreateNumberEdit('LineWidth');
  ELineWidth['type']:='text';
  ELineWidth.Value:='0.5';
  EX:=CreateNumberEdit('X');
  EY:=CreateNumberEdit('Y');
  document.body.appendChild(Panel);
  Panel.appendChild(PanelContent);
  PanelContent.appendChild(Button1);
  PanelContent.appendChild(document.createTextNode('X'));
  PanelContent.appendChild(EX);
  PanelContent.appendChild(document.createTextNode('Y'));
  PanelContent.appendChild(EY);
  PanelContent.appendChild(document.createTextNode('Width'));
  PanelContent.appendChild(EWidth);
  PanelContent.appendChild(document.createTextNode('Height'));
  PanelContent.appendChild(EHeight);
  PanelContent.appendChild(document.createTextNode('LineWidth'));
  PanelContent.appendChild(ELineWidth);
  Panel.appendChild(PanelContent);
  Canvas:=TJSHTMLCanvasElement(document.createElement('canvas'));
  Canvas.width:=640;
  Canvas.height:=480;
  PanelContent.appendChild(canvas);
  Canvas2D:=Canvas.getContextAs2DContext('2d');
end;

begin
  TForm.Create;
end.

