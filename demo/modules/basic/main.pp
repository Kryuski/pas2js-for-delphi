// filename export object alias
{$linklib ./modules/canvas.js canvas}
{$linklib ./modules/square.js square}
{$mode objfpc}
{$modeswitch externalclass}
uses js, web;

type
  TCreateCanvasResult = class external name 'Object' (TJSObject)
     ctx: TJSCanvasRenderingContext2D;
     id : string;    
  end;
  
  TCanvasAPI = class external name 'Object' (TJSObject)
    function create (aID : String; AParent : TJSElement; aWidth,aHeight : Integer) : TCreateCanvasResult;
    function createReportList(aWrapperID : String) : String;  
  end;

  TDrawResult = class external name 'Object' (TJSObject)
    length,x,y : Integer;
    color : string;
  end;

  TSquareAPI = class external name 'Object' (TJSObject)
    name : string;
    // Default export
    function randomsquare(aCtx : TJSCanvasRenderingContext2D) : TDrawResult; external name 'default';
    function draw(aCtx : TJSCanvasRenderingContext2D; alength,x,y : Integer; color : string) : TDrawResult;
    procedure reportArea(length : Integer; ListID : String);
    procedure reportPerimeter(length : Integer; ListID : String);
  end;

var
  CanvasAPI : TCanvasAPI; external name 'canvas';
  SquareAPI : TSquareAPI; external name 'square';
  
Var
  MyCanvas : TCreateCanvasResult;
  MyReportList : string;
  MySquare2,MySquare1 :  TDrawResult;
  
begin
  MyCanvas:=CanvasAPI.create('myCanvas', document.body, 480, 320);
  MyreportList:=CanvasAPI.createReportList(myCanvas.id);
  MySquare1:=SquareAPI.draw(myCanvas.ctx, 50, 50, 100, 'blue');
  SquareAPI.reportArea(MySquare1.length, MyReportList);
  SQuareAPI.reportPerimeter(MySquare1.length,MyReportList);
  MySquare2:=SquareAPI.randomSquare(myCanvas.ctx);
end.