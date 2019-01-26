uses js;

Const
  BSize = 10;
 
var
  B : TJSArrayBuffer;
  V : TJSDataView;
  I : Integer;

begin
  B:=TJSArrayBuffer.New(BSize);
  V:=TJSDataView.New(B);
  for I:=0 to v.byteLength-1 do
    Writeln('Byte ',I,': ',v.getUInt8(i));
  for I:=0 to v.byteLength-1 do
    v.setUInt8(i,i+1);
  Writeln('Writing bytes');
  for I:=0 to v.byteLength-1 do
     Writeln('Byte ',I,': ',v.getUInt8(i));
  Writeln('Reading as Words');
  for I:=0 to (v.byteLength-1) div 2 do
     Writeln('Word ',I,': ',v.getUInt16(i*2));
end.