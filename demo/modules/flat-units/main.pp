// filename export object alias
uses web,canvas, square;
  
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