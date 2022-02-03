Program manualpromise;

uses browserconsole, JS, Web;

function ResolveAfter2Seconds: TJSPromise;
// returns immediately with a Promise,
// which after 2 seconds gets resolved
begin
  Result:=TJSPromise.new(procedure(resolve, reject : TJSPromiseResolver)
    begin
    window.setTimeout(procedure
      begin
      resolve('resolved');
      end,
      2000); // wait 2 seconds
    end);
end;

procedure AsyncCall; async;
var s: string;
begin
  writeln('calling');
  s := await(string,resolveAfter2Seconds());
  // the await pauses this procedure returning to the caller
  // when the Promise from resolveAfter2Seconds gets resolved 
  // this procedure is continued
  writeln(s); // expected output: 'resolved'
end;

begin
  AsyncCall;
  // calling AsyncCall returns immediately, while the Promise is waiting
  writeln('called');
end.
