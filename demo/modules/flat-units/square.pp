{$linklib ./modules/square.js square}
{$mode objfpc}
{$modeswitch externalclass}
unit square;

interface

uses js, web;

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

implementation

end.
