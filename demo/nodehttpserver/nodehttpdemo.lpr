program nodehttpdemo;

{$mode objfpc}

uses
  nodejsapp, JS, Classes, SysUtils, nodeJS, node.http, node.net;

type

  { TMyApplication }

  TMyApplication = class(TNodeJSApplication)
    procedure doRun; override;
  private
    procedure doRequest(req: TNJSHTTPIncomingMessage; resp: TNJSHTTPServerResponse);
  end;

procedure TMyApplication.doRequest(req : TNJSHTTPIncomingMessage; resp : TNJSHTTPServerResponse);

Var
  S : string;
  h : JSValue;
begin
  resp.write('Hello World!'+sLineBreak);
  resp.write('You asked for: '+req.URL+sLineBreak);
  resp.write('You sent the following headers: '+sLineBreak);
  for s in TJSObject.getOwnPropertyNames(req.headers) do
    begin
    H:=req.headers[S];
    if jsTypeOf(H)='string' then
      resp.Write('Header "'+S+'": '+String(H)+sLineBreak);
    end;
  resp.end_(); //end the response
end;

procedure TMyApplication.doRun;

begin
  http.createServer(@DoRequest).listen(7770);
end;

var
  Application : TMyApplication;

begin
  Application:=TMyApplication.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
