{$mode objfpc}
{$h+}
unit wacanvas;

interface

uses js, web, webassembly, wasienv;

Const
  ECANVAS_SUCCESS     = 0;
  ECANVAS_NOCANVAS    = 1;
  ECANVAS_UNSPECIFIED = -1;

Type
  TCanvasError = Longint;
  TCanvasID = Longint;
  
  { TWACanvas }

  TWACanvas = class(TImportExtension)
  Private 
    FCanvases : TJSObject;
    FCurrentID : Integer;
    FCanvasParent : TJSHTMLELement;
  Protected
    function GetCanvas(aID : TCanvasID) : TJSCanvasRenderingContext2D;
    function allocate(SizeX : Longint; SizeY : Longint; aID: Longint): TCanvasError;   
    function moveto(aID : TCanvasID; X : Longint;Y : Longint): TCanvasError;
    function lineto(aID : TCanvasID;X : Longint; Y : Longint ):  TCanvasError; 
    function stroke(aID : TCanvasID): TCanvasError; 
    function beginpath(aID : TCanvasID):  TCanvasError; 
    function arc(aID : TCanvasID;X : Longint;Y : Longint;Radius : Longint;StartAngle : Double;EndAngle : Double):  TCanvasError; 
    function fillrect(aID : TCanvasID;  X : Longint; Y : Longint;  Width : Longint; Height : Longint): TCanvasError; 
    function strokerect(aID : TCanvasID;X : Longint;Y : Longint; Width : Longint; Height : Longint ):  TCanvasError; 
    function clearrect(aID : TCanvasID;X : Longint;Y : Longint;Width : Longint; Height : Longint ):  TCanvasError; 
    function StrokeText(aID : TCanvasID;X : Longint;Y : Longint; aText : Longint; aTextLen : Longint ):  TCanvasError;
    function FillText(aID : TCanvasID;X : Longint;Y : Longint; aText : Longint; aTextLen : Longint ):  TCanvasError;
  Public
    Constructor Create(aEnv : TPas2JSWASIEnvironment); override;
    Procedure FillImportObject(aObject : TJSObject); override;
    Function ImportName : String; override;
    Property CanvasParent : TJSHTMLELement Read FCanvasParent Write FCanvasParent;
  end;

Implementation

uses sysutils;

constructor TWACanvas.Create(aEnv: TPas2JSWASIEnvironment);

begin
  Inherited Create(aEnv);
  FCanvases:=TJSObject.New();
end;

function TWACanvas.ImportName: String;

begin
  Result:='web_canvas';
end;


function TWACanvas.GetCanvas(aID : TCanvasID) : TJSCanvasRenderingContext2D;

Var
  JS : JSValue;
begin
  JS:=FCanvases[IntTostr(AID)];
  if IsObject(JS)  then
    Result:=TJSCanvasRenderingContext2D(JS)
  else
    Result:=Nil;  
end;

procedure TWACanvas.FillImportObject(aObject: TJSObject);

begin
  aObject['allocate']:=@allocate;
  aObject['moveto']:=@moveto;
  aObject['lineto']:=@LineTo;
  aObject['stroke']:=@stroke;
  aObject['beginpath']:=@beginpath;
  aObject['arc']:=@arc;
  aObject['fillrect']:=@fillrect;
  aObject['strokerect']:=@strokerect;
  aObject['clearrect']:=@clearrect;
  aObject['stroketext']:=@StrokeText;
  aObject['filltext']:=@FillText;

end;   

function TWACanvas.allocate(SizeX : Longint; SizeY : Longint; aID: Longint): TCanvasError;   

Var
  C : TJSElement;
  V : TJSDataView;
  
begin
  C:=window.document.createElement('CANVAS');
  CanvasParent.AppendChild(C);
  Inc(FCurrentID);
  V:=getModuleMemoryDataView; 
  FCanvases[IntToStr(FCurrentID)]:=TJSHTMLCanvasElement(c).getcontext('2d');
  v.setUint32(aID, FCurrentID, env.IsLittleEndian); 
  Result:=ECANVAS_SUCCESS;
end;

function TWACanvas.moveto(aID : TCanvasID; X : Longint;Y : Longint): TCanvasError;

Var
  C : TJSCanvasRenderingContext2D;

begin
  Result:=ECANVAS_NOCANVAS;
  C:=GetCanvas(aID);
  if Assigned(C) then
    begin
    C.moveto(X,Y);
    Result:=ECANVAS_SUCCESS;
    end;
end;

function TWACanvas.lineto(aID : TCanvasID;X : Longint; Y : Longint ):  TCanvasError; 

Var
  C : TJSCanvasRenderingContext2D;

begin
  Result:=ECANVAS_NOCANVAS;
  C:=GetCanvas(aID);
  if Assigned(C) then
    begin
    C.lineto(X,Y);
    Result:=ECANVAS_SUCCESS;
    end;
end;

function TWACanvas.stroke(aID : TCanvasID): TCanvasError; 

Var
  C : TJSCanvasRenderingContext2D;

begin
  Result:=ECANVAS_NOCANVAS;
  C:=GetCanvas(aID);
  if Assigned(C) then
    begin
    C.Stroke;
    Result:=ECANVAS_SUCCESS;
    end;
end;

function TWACanvas.beginpath(aID : TCanvasID):  TCanvasError; 

Var
  C : TJSCanvasRenderingContext2D;

begin
  Result:=ECANVAS_NOCANVAS;
  C:=GetCanvas(aID);
  if Assigned(C) then
    begin
    C.beginPath;
    Result:=ECANVAS_SUCCESS;
    end;
end;

function TWACanvas.arc(aID : TCanvasID;X : Longint;Y : Longint;Radius : Longint;StartAngle : Double;EndAngle : Double):  TCanvasError; 

Var
  C : TJSCanvasRenderingContext2D;

begin
  Result:=ECANVAS_NOCANVAS;
  C:=GetCanvas(aID);
  if Assigned(C) then
    begin
    C.arc(X,y,radius,Startangle,EndAngle);
    Result:=ECANVAS_SUCCESS;
    end;
end;

function TWACanvas.fillrect(aID : TCanvasID;  X : Longint; Y : Longint;  Width : Longint; Height : Longint): TCanvasError; 

Var
  C : TJSCanvasRenderingContext2D;

begin
  Result:=ECANVAS_NOCANVAS;
  C:=GetCanvas(aID);
  if Assigned(C) then
    begin
    C.FillRect(X,y,width,Height);
    Result:=ECANVAS_SUCCESS;
    end;
end;

function TWACanvas.strokerect(aID : TCanvasID;X : Longint;Y : Longint; Width : Longint; Height : Longint ):  TCanvasError; 

Var
  C : TJSCanvasRenderingContext2D;

begin
  Result:=ECANVAS_NOCANVAS;
  C:=GetCanvas(aID);
  if Assigned(C) then
    begin
    C.StrokeRect(X,Y,Width,Height);
    Result:=ECANVAS_SUCCESS;
    end;
end;

function TWACanvas.clearrect(aID : TCanvasID;X : Longint;Y : Longint;Width : Longint; Height : Longint ):  TCanvasError; 

Var
  C : TJSCanvasRenderingContext2D;

begin
  Result:=ECANVAS_NOCANVAS;
  C:=GetCanvas(aID);
  if Assigned(C) then
    begin
    C.ClearRect(X,Y,Width,Height);
    Result:=ECANVAS_SUCCESS;
    end;
end;

function TWACanvas.StrokeText(aID: TCanvasID; X: Longint; Y: Longint;
  aText: Longint; aTextLen: Longint): TCanvasError;

Var
  C : TJSCanvasRenderingContext2D;
  S : String;
begin
  Result:=ECANVAS_NOCANVAS;
  C:=GetCanvas(aID);
  if Assigned(C) then
    begin
    S:=Env.GetUTF8StringFromMem(aText,aTextLen);
    C.StrokeText(S,X,Y);
    Result:=ECANVAS_SUCCESS;
    end;
end;

function TWACanvas.FillText(aID: TCanvasID; X: Longint; Y: Longint;
  aText: Longint; aTextLen: Longint): TCanvasError;
Var
  C : TJSCanvasRenderingContext2D;
  S : String;
begin
  Result:=ECANVAS_NOCANVAS;
  C:=GetCanvas(aID);
  if Assigned(C) then
    begin
    S:=Env.GetUTF8StringFromMem(aText,aTextLen);
    C.FillText(S,X,Y);
    Result:=ECANVAS_SUCCESS;
    end;
end;

end.  
