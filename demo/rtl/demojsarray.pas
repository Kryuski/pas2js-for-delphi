uses js;

function FilterOdd(el : JSValue; Index : NativeInt; Arr: TJSArray) : boolean  ;

Var
  I : Integer;
begin
  I:=Integer(el);
  result:=((i mod 2)=1);
end;

function showElement(el : JSValue; Index : NativeInt; Arr: TJSArray) : boolean  ;

begin
  Writeln(Index,':',el);
  result:=true;
end;

Procedure ShowArray(Msg : string; a: TJSArray);
  
begin
  writeln(Msg,' : ');
  a.forEach(@ShowElement);
end;


var
  a,b : TJSArray;

begin
  a:=TJSArray._of(5,4,3,2,1,0);
  ShowArray('init',a);
  a:=TJSArray.new(5,4,3,2,1,0);
  ShowArray('init 2',a);
{$IFDEF ECMAScript6}
  // Note these change the array itself
  ShowArray('fill(-1,3)',a.fill(-1,3));
  ShowArray('fill(-1,1,1)',a.fill(-1,1,1));
  ShowArray('fill(-1)',a.fill(-1));
{$ENDIF}
  a:=TJSArray.new(5,4,3,2,1,2,3);
  Writeln(a.ToString,'.indexOf(3): ',a.indexOf(3));  
  Writeln(a.ToString,'.indexOf(2,4): ',a.indexOf(2,4));  
  ShowArray('Filter(odd)',a.filter(@FilterOdd));
  a:=TJSArray.new('alpha', 'bravo', 'charlie', 'delta');
  ShowArray('copyWithin(2,0)',a.copyWithin(2, 0));
end. 
