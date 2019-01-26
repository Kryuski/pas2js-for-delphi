unit browserapp;

{$mode objfpc}

interface

uses
  Classes, SysUtils, Types, JS, web, CustApp;

type

  { TBrowserApplication }

  TBrowserApplication = class(TCustomApplication)
  protected
    procedure DoRun; override;
    function GetConsoleApplication: boolean; override;
    function GetLocation: String; override;
  public
    procedure GetEnvironmentList(List: TStrings; NamesOnly: Boolean); override;
    procedure ShowException(E: Exception); override;
    procedure HandleException(Sender: TObject); override;
  end;

procedure ReloadEnvironmentStrings;

implementation

var
  EnvNames: TJSObject;
  Params : TStringDynArray;

procedure ReloadEnvironmentStrings;

var
  I : Integer;
  S : String;
  A,P : TStringDynArray;
begin
  if Assigned(EnvNames) then
    FreeAndNil(EnvNames);
  EnvNames:=TJSObject.new;
  S:=Window.Location.search;
  S:=Copy(S,2,Length(S)-1);
  A:=TJSString(S).split('&');
  for I:=0 to Length(A)-1 do
    begin
    P:=TJSString(A[i]).split('=');
    if Length(P)=2 then
      EnvNames[decodeURIComponent(P[0])]:=decodeURIComponent(P[1])
    else if Length(P)=1 then
      EnvNames[decodeURIComponent(P[0])]:=''
    end;
end;

procedure ReloadParamStrings;

begin
  SetLength(Params,1);
  Params[0]:=Window.location.pathname;
end;


function GetParamCount: longint;
begin
  Result:=Length(Params)-1;
end;

function GetParamStr(Index: longint): String;
begin
  Result:=Params[Index]
end;

function MyGetEnvironmentVariable(Const EnvVar: String): String;

begin
  Result:=String(EnvNames[envvar]);
end;

function MyGetEnvironmentVariableCount: Integer;
begin
  Result:=length(TJSOBject.getOwnPropertyNames(envNames));
end;

function MyGetEnvironmentString(Index: Integer): String;
begin
  Result:=String(EnvNames[TJSOBject.getOwnPropertyNames(envNames)[Index]]);
end;

{ TBrowserApplication }

procedure TBrowserApplication.DoRun;
begin
  // Override in descendent classes.
end;

function TBrowserApplication.GetConsoleApplication: boolean;
begin
  Result:=true;
end;

function TBrowserApplication.GetLocation: String;
begin
  Result:=''; // ToDo ExtractFilePath(GetExeName);
end;

procedure TBrowserApplication.GetEnvironmentList(List: TStrings;
  NamesOnly: Boolean);
var
  Names: TStringDynArray;
  i: Integer;
begin
  Names:=TJSObject.getOwnPropertyNames(EnvNames);
  for i:=0 to length(Names)-1 do
  begin
    if NamesOnly then
      List.Add(Names[i])
    else
      List.Add(Names[i]+'='+String(EnvNames[Names[i]]));
  end;
end;

procedure TBrowserApplication.ShowException(E: Exception);

Var
  S : String;

begin
  if (E<>nil) then
    S:=E.ClassName+': '+E.Message
  else if ExceptObjectJS then
    s:=TJSObject(ExceptObjectJS).toString;
  window.alert('Unhandled exception caught:'+S);
end;

procedure TBrowserApplication.HandleException(Sender: TObject);
begin
  if ExceptObject is Exception then
    ShowException(ExceptObject);
  inherited HandleException(Sender);
end;

initialization
  IsConsole:=true;
  OnParamCount:=@GetParamCount;
  OnParamStr:=@GetParamStr;
  ReloadEnvironmentStrings;
  ReloadParamStrings;
  OnGetEnvironmentVariable:=@MyGetEnvironmentVariable;
  OnGetEnvironmentVariableCount:=@MyGetEnvironmentVariableCount;
  OnGetEnvironmentString:=@MyGetEnvironmentString;

end.

