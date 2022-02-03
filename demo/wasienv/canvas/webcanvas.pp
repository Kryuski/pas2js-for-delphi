unit webcanvas;

interface

// These types and constants could go in a unit shared between pas2js and webassembly !

Type
  TCanvasError = longint;
  TCanvasID = longint;
  PCanvasID = ^TCanvasID;

Const
  ECANVAS_SUCCESS     = 0;
  ECANVAS_NOCANVAS    = 1;
  ECANVAS_UNSPECIFIED = -1;

function __webcanvas_allocate(
  SizeX : Longint;
  SIzeY : Longint;
  aID: PCanvasID
): TCanvasError; external 'web_canvas' name 'allocate';

function __webcanvas_moveto(
  aID : TCanvasID;
  X : Longint;
  Y : Longint
):  TCanvasError; external 'web_canvas' name 'moveto';

function __webcanvas_lineto(
  aID : TCanvasID;
  X : Longint;
  Y : Longint
):  TCanvasError; external 'web_canvas' name 'lineto';

function __webcanvas_stroke(
  aID : TCanvasID
):  TCanvasError; external 'web_canvas' name 'stroke';

function __webcanvas_beginpath(
  aID : TCanvasID
):  TCanvasError; external 'web_canvas' name 'beginpath';

function __webcanvas_arc(
  aID : TCanvasID;
  X : Longint;
  Y : Longint;
  Radius : Longint;
  StartAngle : Double;
  EndAngle : Double
):  TCanvasError; external 'web_canvas' name 'arc';


function __webcanvas_fillrect(
  aID : TCanvasID;
  X : Longint;
  Y : Longint;
  Width : Longint;
  Height : Longint
):  TCanvasError; external 'web_canvas' name 'fillrect';

function __webcanvas_strokerect(
  aID : TCanvasID;
  X : Longint;
  Y : Longint;
  Width : Longint;
  Height : Longint
):  TCanvasError; external 'web_canvas' name 'strokerect';


function __webcanvas_clearrect(
  aID : TCanvasID;
  X : Longint;
  Y : Longint;
  Width : Longint;
  Height : Longint
):  TCanvasError; external 'web_canvas' name 'clearrect';

function __webcanvas_stroketext(
  aID : TCanvasID;
  X : Longint;
  Y : Longint;
  aText : PByte;
  aTextLen : Longint
):  TCanvasError; external 'web_canvas' name 'stroketext';

function __webcanvas_filltext(
  aID : TCanvasID;
  X : Longint;
  Y : Longint;
  aText : PByte;
  aTextLen : Longint
):  TCanvasError; external 'web_canvas' name 'filltext';


implementation

end.
