{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2017 by the Free Pascal development team

    TNodeJSApplication class.

    Initial implementation by Mattias Gaertner mattias@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit NodeJSApp;

{$mode objfpc}

interface

uses
  NodeJS, Classes, SysUtils, Types, JS, CustApp;

type
  
  { TNodeJSApplication }

  TNodeJSApplication = class(TCustomApplication)
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
  EnvNames: TStringDynArray;

procedure ReloadEnvironmentStrings;
begin
  EnvNames:=TJSObject.getOwnPropertyNames(TNJSProcess.env);
end;

function GetParamCount: longint;
begin
  // argv contains as first element nodejs, and second the src.js
  Result:=length(TNJSProcess.argv)-2;
end;

function GetParamStr(Index: longint): String;
begin
  // ParamStr(0) is the exename = the js file, which is the second element argv[1]
  Result:=TNJSProcess.argv[Index+1];
end;

function MyGetEnvironmentVariable(Const EnvVar: String): String;
begin
  Result:=String(TNJSProcess.env[EnvVar]);
end;

function MyGetEnvironmentVariableCount: Integer;
begin
  Result:=length(EnvNames);
end;

function MyGetEnvironmentString(Index: Integer): String;
begin
  Result:=EnvNames[Index];
end;

{ TNodeJSApplication }

procedure TNodeJSApplication.DoRun;
begin
  // Override in descendent classes.
end;

function TNodeJSApplication.GetConsoleApplication: boolean;
begin
  Result:=true;
end;

function TNodeJSApplication.GetLocation: String;
begin
  Result:=''; // ToDo ExtractFilePath(GetExeName);
end;

procedure TNodeJSApplication.GetEnvironmentList(List: TStrings;
  NamesOnly: Boolean);
var
  Names: TStringDynArray;
  i: Integer;
begin
  Names:=TJSObject.getOwnPropertyNames(TNJSProcess.env);
  for i:=0 to length(Names)-1 do
  begin
    if NamesOnly then
      List.Add(Names[i])
    else
      List.Add(Names[i]+'='+String(TNJSProcess.env[Names[i]]));
  end;
end;

procedure TNodeJSApplication.ShowException(E: Exception);
begin
  if E<>nil then
    writeln(E.ClassName,': ',E.Message)
  else if ExceptObjectJS then
    writeln(ExceptObjectJS);
end;

procedure TNodeJSApplication.HandleException(Sender: TObject);
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
  OnGetEnvironmentVariable:=@MyGetEnvironmentVariable;
  OnGetEnvironmentVariableCount:=@MyGetEnvironmentVariableCount;
  OnGetEnvironmentString:=@MyGetEnvironmentString;

end.

