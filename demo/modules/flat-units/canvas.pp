{$linklib ./modules/canvas.js canvas}
{$mode objfpc}
{$modeswitch externalclass}
unit canvas;

interface

uses js, web;

Type

  TCreateCanvasResult = class external name 'Object' (TJSObject)
     ctx: TJSCanvasRenderingContext2D;
     id : string;    
  end;
  
function create (aID : String; AParent : TJSElement; aWidth,aHeight : Integer) : TCreateCanvasResult; external name 'canvas.create';
function createReportList(aWrapperID : String) : String;  external name 'canvas.createReportList';

implementation

end.
