program demoall;

{$mode objfpc}

uses
  JS, web, browserconsole;

Procedure demo;

  procedure DoTimeout(resolve, reject: TJSPromiseResolver);

    Procedure DoCallResolve;

    begin
      Resolve('foo');
    end;

  begin
    window.setTimeOut(@DoCallResolve,100);
  end;

  function ShowResult(aValue: JSValue): JSValue;

  Var
    I : Integer;
    A : TJSArray;

  begin
    A:=TJSArray(aValue);
    For I:=0 to A.Length-1 do
      Writeln(A[i]);
  end;

var
  p1,p2,p3 : JSValue;

begin
  p1:=TJSPromise.resolve(3);
  p2:=1337;
  p3:=TJSPromise.New(@DoTimeout);
  TJSPromise.all([p1, p2, p3])._then(@ShowResult);
end;

Procedure Demo2;

var
  p1,p2,p3 : TJSPromise;

  Procedure LogAll;

  begin
    writeln(p1);
    writeln(p2);
    writeln(p3);
  end;

begin
  // this will be counted as if the iterable passed is empty, so it gets fulfilled
  p1 :=TJSPromise.all([1,2,3]);
  // this will be counted as if the iterable passed contains only the resolved promise with value "444", so it gets fulfilled
  p2 := TJSPromise.all([1,2,3, TJSPromise.resolve(444)]);
  // this will be counted as if the iterable passed contains only the rejected promise with value "555", so it gets rejected
  p3 := TJSPromise.all([1,2,3, TJSPromise.reject(555)]);
  // using setTimeout we can execute code after the stack is empty
  window.setTimeout(@LogAll);
end;

Procedure Demo3;

var
  resolvedPromisesArray : TJSPromiseArray;
  p : TJSPromise;

  Procedure doLog;

  begin
    console.log('the stack is now empty');
    console.log(p);
  end;

begin
  // we are passing as argument an array of promises that are already resolved,
  // to trigger Promise.all as soon as possible
  SetLength(resolvedPromisesArray,2);
  resolvedPromisesArray[0]:=TJSPromise.resolve(33);
  resolvedPromisesArray[1]:=TJSPromise.resolve(44);
  p:=TJSPromise.all(resolvedPromisesArray);
  // immediately logging the value of p
  console.log(p);
  // using setTimeout we can execute code after the stack is empty
  window.setTimeout(@DoLog);
end;

begin
  Demo;
  Demo2;
  Demo3;
end.
