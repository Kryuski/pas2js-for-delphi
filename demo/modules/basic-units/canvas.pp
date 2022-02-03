{$mode objfpc}
{$modeswitch externalclass}
// filename export object alias
{$linklib ./modules/canvas.js canvas}

unit canvas;

interface

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

Var
  CanvasAPI : TCanvasAPI; external name 'canvas';

implementation

end.