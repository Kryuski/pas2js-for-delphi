program sums;

Var
  A,B,C : Integer;

begin
  Writeln('Answer -1 to end the program');
  Repeat
    A:=Random(100);
    B:=Random(100);
    Write(A,' + ',B,' = ');
    Readln(C);
    if (C=(A+B)) then
      Writeln('Well done !')
    else if (C<>-1) then
      Writeln('Sorry, wrong. The correct answer is ',A+B);
  Until (C=-1);
end.

