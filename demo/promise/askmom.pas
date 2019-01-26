program askmom;

{$mode objfpc}
{
  Translated from
    https://scotch.io/tutorials/javascript-promises-for-dummies
}
uses
  browserconsole, JS, Web;

var
  isMomHappy : Boolean = False;

Procedure LetsAskMom;

  procedure MomDecides (resolve, reject : TJSPromiseResolver);

  begin
    if IsMomHappy then
      Resolve(New(['brand','Samsung','Color','Black']))
   else
      Reject(TJSError.New('Mom is not happy'));
  end;

  Function Disappointed(aValue : JSValue): JSValue;

  begin
    Writeln('No present because: ',aValue);
  end;

  Function Showpresent(aValue : JSValue): JSValue;

  begin
    Writeln('Received : ',aValue);
  end;

Var
  willIGetNewPhone : TJSPromise;

begin
  TJSPromise.New(@MomDecides).
    _Then(@ShowPresent).
    Catch(@Disappointed);
end;

begin
  Writeln('Did something bad, making mom unhappy');
  isMomHappy:=False;
  LetsAskMom();
  Writeln('Made up with mom, making her happy again');
  isMomHappy:=True;
  LetsAskMom();
end.
