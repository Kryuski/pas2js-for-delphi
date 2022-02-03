program canvasdraw;

{$mode objfpc}
{$h+}
uses
  sysutils, webcanvas;

Type
  ECanvas = class(Exception);

  { TWebCanvas }

  TWebCanvas = class(TObject)
  private
    FCanvasID : Longint;
    FHeight: Longint;
    FWidth: Longint;
  Protected
    Procedure Check(aError : TCanvasError; const aMsg : String = '');
  Public
    Constructor Create(aWidth,aHeight : Longint);
    Procedure moveto(X : Longint;Y : Longint);
    Procedure lineto(X : Longint; Y : Longint);
    Procedure stroke();
    Procedure beginpath();
    Procedure arc(X : Longint;Y : Longint;Radius : Longint;StartAngle : Double;EndAngle : Double);
    Procedure fillrect(X : Longint; Y : Longint;  Width : Longint; Height : Longint);
    Procedure strokerect(X : Longint;Y : Longint; Width : Longint; Height : Longint);
    Procedure clearrect(X : Longint;Y : Longint;Width : Longint; Height : Longint );
    Procedure StrokeText(X : Longint;Y : Longint;S : UTF8String);
    Procedure FillText(X : Longint;Y : Longint;S : UTF8String);
    Property CanvasID : Integer Read FCanvasID;
    Property Width : Longint Read FWidth;
    Property Height : Longint Read FHeight;
  end;

{ TWebCanvas }

procedure TWebCanvas.Check(aError: TCanvasError; const aMsg: String);
begin
  if aError<>ECANVAS_SUCCESS then
    if aMsg='' then
      Raise Exception.CreateFmt('Canvas Operation failed %d',[aError])
    else
      Raise Exception.CreateFmt('%s : Error code %d',[aMsg,aError]);

end;

constructor TWebCanvas.Create(aWidth, aHeight: Longint);
begin
  Check(__webcanvas_allocate(aWidth,aHeight,@FCanvasID),'Failed to create web canvas');
  FWidth:=aWidth;
  FHeight:=aHeight;
end;

procedure TWebCanvas.moveto(X: Longint; Y: Longint);
begin
  Check(__webcanvas_moveto(FCanvasID,X,Y));
end;

procedure TWebCanvas.lineto(X: Longint; Y: Longint);
begin
  Check(__webcanvas_lineto(FCanvasID,X,Y));
end;

procedure TWebCanvas.stroke;
begin
  Check(__webcanvas_stroke(FCanvasID));
end;

procedure TWebCanvas.beginpath;
begin
  Check(__webcanvas_beginpath(FCanvasID));
end;

procedure TWebCanvas.arc(X: Longint; Y: Longint; Radius: Longint;
  StartAngle: Double; EndAngle: Double);
begin
  Check(__webcanvas_arc(FCanvasID,X,Y,Radius,StartAngle,EndAngle));
end;

procedure TWebCanvas.fillrect(X: Longint; Y: Longint; Width: Longint;
  Height: Longint);
begin
  Check(__webcanvas_fillrect(FCanvasID,X,Y,Width,Height));
end;

procedure TWebCanvas.strokerect(X: Longint; Y: Longint; Width: Longint;
  Height: Longint);
begin
  Check(__webcanvas_strokerect(FCanvasID,X,Y,Width,Height));
end;

procedure TWebCanvas.clearrect(X: Longint; Y: Longint; Width: Longint;
  Height: Longint);
begin
  Check(__webcanvas_clearrect(FCanvasID,X,Y,Width,Height));
end;

procedure TWebCanvas.StrokeText(X: Longint; Y: Longint; S: UTF8String);
begin
  Check(__webcanvas_stroketext(FCanvasID,X,Y,PByte(PAnsichar(S)),Length(S)));
end;

procedure TWebCanvas.FillText(X: Longint; Y: Longint; S: UTF8String);
begin
  Check(__webcanvas_filltext(FCanvasID,X,Y,PByte(PAnsichar(S)),Length(S)));
end;

Var
  aCanvas : TWebCanvas;

begin
  aCanvas:=TWebCanvas.Create(150,150);
  With aCanvas do
    try
      Writeln('Filling rect');
      fillRect(25, 25, 100, 100);
      Writeln('Clearing rect');
      clearRect(45, 45, 60, 60);
      Writeln('Stroking rect');
      strokeRect(50, 50, 50, 50);
      Writeln('Drawing cross');
      BeginPath;
      MoveTo(25,25);
      Writeln('First line');
      LineTo(125,125);
      MoveTo(125,25);
      Writeln('Second line');
      LineTo(25,125);
      Stroke;
      Writeln('Writing text');
      FillText(8,8,'Greetings on the WebAssembly Canvas!');
    finally
      Free;
    end;
end.

