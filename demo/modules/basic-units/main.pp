{$mode objfpc}
{$modeswitch externalclass}
uses js, web, canvas, square;
 
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