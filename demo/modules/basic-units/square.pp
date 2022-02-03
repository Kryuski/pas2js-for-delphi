{$mode objfpc}
{$modeswitch externalclass}
// filename export object alias
{$linklib ./modules/square.js square}

unit square;

interface

uses js, web;

Type
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
  SquareAPI : TSquareAPI; external name 'square';

implementation

end.