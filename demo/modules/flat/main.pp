// filename export object alias
{$linklib ./modules/canvas.js canvas}
{$linklib ./modules/square.js square}
{$mode objfpc}
{$modeswitch externalclass}
uses js, web;

// canvas API

type
  TCreateCanvasResult = class external name 'Object' (TJSObject)
     ctx: TJSCanvasRenderingContext2D;
     id : string;    
  end;
  
function create (aID : String; AParent : TJSElement; aWidth,aHeight : Integer) : TCreateCanvasResult; external name 'canvas.create';
function createReportList(aWrapperID : String) : String;  external name 'canvas.createReportList';

Type
  TDrawResult = class external name 'Object' (TJSObject)
    length,x,y : Integer;
    color : string;
  end;

// Square API

var 
  name : string; external name 'square.name';
  
function randomsquare(aCtx : TJSCanvasRenderingContext2D) : TDrawResult; external name 'square.default';
function draw(aCtx : TJSCanvasRenderingContext2D; alength,x,y : Integer; color : string) : TDrawResult; external name 'square.draw';
procedure reportArea(length : Integer; ListID : String); external name 'square.reportArea';
procedure reportPerimeter(length : Integer; ListID : String); external name 'square.reportPerimeter';
  
Var
  MyCanvas : TCreateCanvasResult;
  MyReportList : string;
  MySquare2,MySquare1 :  TDrawResult;
  
begin
  MyCanvas:=create('myCanvas', document.body, 480, 320);
  MyreportList:=createReportList(myCanvas.id);
  MySquare1:=draw(myCanvas.ctx, 50, 50, 100, 'blue');
  reportArea(MySquare1.length, MyReportList);
  reportPerimeter(MySquare1.length,MyReportList);
  MySquare2:=randomSquare(myCanvas.ctx);
end.