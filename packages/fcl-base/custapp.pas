{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    CustomApplication class.

    Port to pas2js by Mattias Gaertner mattias@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit CustApp;

{$mode objfpc}

interface

uses
  Classes, SysUtils, Types, JS;

Const
  SErrInvalidOption: String = 'Invalid option at position %s: "%s"';
  SErrNoOptionAllowed: String = 'Option at position %s does not allow an argument: %s';
  SErrOptionNeeded: String = 'Option at position %s needs an argument : %s';

Type
  TExceptionEvent = procedure (Sender : TObject; E : Exception) of object;
  TEventLogTypes = set of TEventType;

  { TCustomApplication }

  TCustomApplication = Class(TComponent)
  Private
    FEventLogFilter: TEventLogTypes;
    FExceptObjectJS: JSValue;
    FOnException: TExceptionEvent;
    FTerminated: Boolean;
    FTitle: String;
    FOptionChar: Char;
    FCaseSensitiveOptions: Boolean;
    FStopOnException: Boolean;
    FExceptionExitCode: Integer;
    FExceptObject: Exception;
  Protected
    function GetEnvironmentVar(VarName: String): String; virtual;
    function GetExeName: string; virtual;
    function GetLocation: String; virtual; abstract;
    function GetOptionAtIndex(AIndex: Integer; IsLong: Boolean): String;
    procedure SetTitle(const AValue: string); virtual;
    function GetConsoleApplication: boolean; virtual; abstract;
    procedure DoRun; virtual; abstract;
    function GetParams(Index: Integer): String; virtual;
    function GetParamCount: Integer; virtual;
    procedure DoLog(EventType: TEventType; const Msg: String); virtual;
  Public
    constructor Create(AOwner: TComponent); override;
    // Some Delphi methods.
    procedure HandleException(Sender: TObject); virtual;
    procedure Initialize; virtual;
    procedure Run;
    procedure ShowException(E: Exception); virtual; abstract;
    procedure Terminate; virtual;
    procedure Terminate(AExitCode: Integer); virtual;
    // Extra methods.
    function FindOptionIndex(Const S: String; var Longopt: Boolean; StartAt: Integer = -1): Integer;
    function GetOptionValue(Const S: String): String;
    function GetOptionValue(Const C: Char; Const S: String): String;
    function GetOptionValues(Const C: Char; Const S: String): TStringDynArray;
    function HasOption(Const S: String) : Boolean;
    function HasOption(Const C: Char; Const S: String): Boolean;
    function CheckOptions(Const ShortOptions: String; Const Longopts: TStrings;
      Opts,NonOpts: TStrings; AllErrors: Boolean = False): String;
    function CheckOptions(Const ShortOptions: String; Const Longopts: Array of string;
      Opts,NonOpts: TStrings; AllErrors: Boolean = False): String;
    function CheckOptions(Const ShortOptions: String; Const Longopts: TStrings;
      AllErrors: Boolean = False): String;
    function CheckOptions(Const ShortOptions: String; Const LongOpts: Array of string;
      AllErrors: Boolean = False): String;
    function CheckOptions(Const ShortOptions: String; Const LongOpts: String;
      AllErrors: Boolean = False): String;
    function GetNonOptions(Const ShortOptions: String; Const Longopts: Array of string): TStringDynArray;
    procedure GetNonOptions(Const ShortOptions: String; Const Longopts: Array of string;
      NonOptions: TStrings);
    procedure GetEnvironmentList(List: TStrings; NamesOnly: Boolean); virtual; abstract;
    procedure GetEnvironmentList(List: TStrings); virtual;
    procedure Log(EventType: TEventType; const Msg: String);
    procedure Log(EventType: TEventType; const Fmt: String; const Args: Array of string);
    // Delphi properties
    property ExeName: string read GetExeName;
    property Terminated: Boolean read FTerminated;
    property Title: string read FTitle write SetTitle;
    property OnException: TExceptionEvent read FOnException write FOnException;
    // Extra properties
    property ConsoleApplication: Boolean Read GetConsoleApplication;
    property Location: String Read GetLocation;
    property Params[Index: integer]: String Read GetParams;
    property ParamCount: Integer Read GetParamCount;
    property EnvironmentVariable[EnvName: String]: String Read GetEnvironmentVar;
    property OptionChar: Char Read FoptionChar Write FOptionChar;
    property CaseSensitiveOptions: Boolean Read FCaseSensitiveOptions Write FCaseSensitiveOptions;
    property StopOnException: Boolean Read FStopOnException Write FStopOnException;
    property ExceptionExitCode: Longint Read FExceptionExitCode Write FExceptionExitCode;
    property ExceptObject: Exception read FExceptObject write FExceptObject;
    property ExceptObjectJS: JSValue read FExceptObjectJS write FExceptObjectJS;
    property EventLogFilter: TEventLogTypes Read FEventLogFilter Write FEventLogFilter;
  end;

var CustomApplication: TCustomApplication = nil;

implementation

{ TCustomApplication }

function TCustomApplication.GetEnvironmentVar(VarName: String): String;
begin
  Result:=GetEnvironmentVariable(VarName);
end;

function TCustomApplication.GetExeName: string;
begin
  Result:=ParamStr(0);
end;

function TCustomApplication.GetOptionAtIndex(AIndex: Integer; IsLong: Boolean
  ): String;

Var
  P : Integer;
  O : String;

begin
  Result:='';
  If AIndex=-1 then
    Exit;
  If IsLong then
    begin // Long options have form --option=value
    O:=Params[AIndex];
    P:=Pos('=',O);
    If P=0 then
      P:=Length(O);
    Delete(O,1,P);
    Result:=O;
    end
  else
    begin // short options have form '-o value'
    If AIndex<ParamCount then
      if Copy(Params[AIndex+1],1,1)<>'-' then
        Result:=Params[AIndex+1];
    end;
end;

procedure TCustomApplication.SetTitle(const AValue: string);
begin
  FTitle:=AValue;
end;

function TCustomApplication.GetParams(Index: Integer): String;
begin
  Result:=ParamStr(Index);
end;

function TCustomApplication.GetParamCount: Integer;
begin
  Result:=System.ParamCount;
end;

procedure TCustomApplication.DoLog(EventType: TEventType; const Msg: String);
begin
  // Do nothing, override in descendants
  if EventType=etCustom then ;
  if Msg='' then ;
end;

constructor TCustomApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptionChar:='-';
  FCaseSensitiveOptions:=True;
  FStopOnException:=False;
end;

procedure TCustomApplication.HandleException(Sender: TObject);
begin
  ShowException(ExceptObject);
  if FStopOnException then
    Terminate(ExceptionExitCode);
end;

procedure TCustomApplication.Initialize;
begin
  FTerminated:=False;
end;

procedure TCustomApplication.Run;
begin
  Repeat
    ExceptObject:=nil;
    ExceptObjectJS:=nil;
    Try
      DoRun;
    except
      on E: Exception do
      begin
        ExceptObject:=E;
        ExceptObjectJS:=E;
        HandleException(Self);
      end
      else begin
        ExceptObject:=nil;
        ExceptObjectJS := JS.JSExceptValue;
      end;
    end;
    break;
  Until FTerminated;
end;

procedure TCustomApplication.Terminate;
begin
  Terminate(ExitCode);
end;

procedure TCustomApplication.Terminate(AExitCode: Integer);
begin
  FTerminated:=True;
  ExitCode:=AExitCode;
end;

function TCustomApplication.FindOptionIndex(const S: String;
  var Longopt: Boolean; StartAt: Integer): Integer;

Var
  SO,O : String;
  I,P : Integer;

begin
  If Not CaseSensitiveOptions then
    SO:=UpperCase(S)
  else
    SO:=S;
  Result:=-1;
  I:=StartAt;
  if I=-1 then
    I:=ParamCount;
  While (Result=-1) and (I>0) do
    begin
    O:=Params[i];
    // - must be seen as an option value
    If (Length(O)>1) and (O[1]=FOptionChar) then
      begin
      Delete(O,1,1);
      LongOpt:=(Length(O)>0) and (O[1]=FOptionChar);
      If LongOpt then
        begin
        Delete(O,1,1);
        P:=Pos('=',O);
        If (P<>0) then
          O:=Copy(O,1,P-1);
        end;
      If Not CaseSensitiveOptions then
        O:=UpperCase(O);
      If (O=SO) then
        Result:=i;
      end;
    Dec(i);
    end;
end;

function TCustomApplication.GetOptionValue(const S: String): String;
begin
  Result:=GetOptionValue(' ',S);
end;

function TCustomApplication.GetOptionValue(const C: Char; const S: String
  ): String;

Var
  B : Boolean;
  I : integer;

begin
  Result:='';
  I:=FindOptionIndex(C,B);
  If I=-1 then
    I:=FindOptionIndex(S,B);
  If I<>-1 then
    Result:=GetOptionAtIndex(I,B);
end;

function TCustomApplication.GetOptionValues(const C: Char; const S: String
  ): TStringDynArray;

Var
  I,Cnt : Integer;
  B : Boolean;

begin
  SetLength(Result,ParamCount);
  Cnt:=0;
  Repeat
    I:=FindOptionIndex(C,B,I);
    If I<>-1 then
      begin
      Inc(Cnt);
      Dec(I);
      end;
  Until I=-1;
  Repeat
    I:=FindOptionIndex(S,B,I);
    If I<>-1 then
      begin
      Inc(Cnt);
      Dec(I);
      end;
  Until I=-1;
  SetLength(Result,Cnt);
  Cnt:=0;
  I:=-1;
  Repeat
    I:=FindOptionIndex(C,B,I);
    If (I<>-1) then
      begin
      Result[Cnt]:=GetOptionAtIndex(I,False);
      Inc(Cnt);
      Dec(i);
      end;
  Until (I=-1);
  I:=-1;
  Repeat
    I:=FindOptionIndex(S,B,I);
    If I<>-1 then
      begin
      Result[Cnt]:=GetOptionAtIndex(I,True);
      Inc(Cnt);
      Dec(i);
      end;
  Until (I=-1);
end;

function TCustomApplication.HasOption(const S: String): Boolean;

Var
  B : Boolean;

begin
  Result:=FindOptionIndex(S,B)<>-1;
end;

function TCustomApplication.HasOption(const C: Char; const S: String): Boolean;

Var
  B : Boolean;

begin
  Result:=(FindOptionIndex(C,B)<>-1) or (FindOptionIndex(S,B)<>-1);
end;

function TCustomApplication.CheckOptions(const ShortOptions: String;
  const Longopts: TStrings; Opts, NonOpts: TStrings; AllErrors: Boolean
  ): String;

Var
  I,J,L,P : Integer;
  O,OV,SO : String;
  UsedArg,HaveArg : Boolean;

  Function FindLongOpt(S : String) : boolean;

  Var
    I : integer;

  begin
    Result:=Assigned(LongOpts);
    if Not Result then
      exit;
    If CaseSensitiveOptions then
      begin
      I:=LongOpts.Count-1;
      While (I>=0) and (LongOpts[i]<>S) do
        Dec(i);
      end
    else
      begin
      S:=UpperCase(S);
      I:=LongOpts.Count-1;
      While (I>=0) and (UpperCase(LongOpts[i])<>S) do
        Dec(i);
      end;
    Result:=(I<>-1);
  end;

  Procedure AddToResult(Const Msg : string);

  begin
    If (Result<>'') then
      Result:=Result+sLineBreak;
    Result:=Result+Msg;
  end;

begin
  If CaseSensitiveOptions then
    SO:=Shortoptions
  else
    SO:=LowerCase(Shortoptions);
  Result:='';
  I:=1;
  While (I<=ParamCount) and ((Result='') or AllErrors) do
    begin
    O:=Paramstr(I);
    If (Length(O)=0) or (O[1]<>FOptionChar) then
      begin
      If Assigned(NonOpts) then
        NonOpts.Add(O);
      end
    else
      begin
      If (Length(O)<2) then
        AddToResult(Format(SErrInvalidOption,[IntToStr(I),O]))
      else
        begin
        HaveArg:=False;
        OV:='';
        // Long option ?
        If (O[2]=FOptionChar) then
          begin
          Delete(O,1,2);
          J:=Pos('=',O);
          If J<>0 then
            begin
            HaveArg:=true;
            OV:=O;
            Delete(OV,1,J);
            O:=Copy(O,1,J-1);
            end;
          // Switch Option
          If FindLongopt(O) then
            begin
            If HaveArg then
              AddToResult(Format(SErrNoOptionAllowed,[IntToStr(I),O]));
            end
          else
            begin // Required argument
            If FindLongOpt(O+':') then
              begin
              If Not HaveArg then
                AddToResult(Format(SErrOptionNeeded,[IntToStr(I),O]));
              end
            else
              begin // Optional Argument.
              If not FindLongOpt(O+'::') then
                AddToResult(Format(SErrInvalidOption,[IntToStr(I),O]));
              end;
            end;
          end
        else // Short Option.
          begin
          HaveArg:=(I<ParamCount) and (Length(ParamStr(I+1))>0) and (ParamStr(I+1)[1]<>FOptionChar);
          UsedArg:=False;
          If Not CaseSensitiveOptions then
            O:=LowerCase(O);
          L:=Length(O);
          J:=2;
          While ((Result='') or AllErrors) and (J<=L) do
            begin
            P:=Pos(O[J],SO);
            If (P=0) or (O[j]=':') then
              AddToResult(Format(SErrInvalidOption,[IntToStr(I),O[J]]))
            else
              begin
              If (P<Length(SO)) and (SO[P+1]=':') then
                begin
                // Required argument
                If ((P+1)=Length(SO)) or (SO[P+2]<>':') Then
                  If (J<L) or not haveArg then // Must be last in multi-opt !!
                    begin
                    AddToResult(Format(SErrOptionNeeded,[IntToStr(I),O[J]]));
                    end;
                O:=O[j]; // O is added to arguments.
                UsedArg:=True;
                end;
              end;
            Inc(J);
            end;
          HaveArg:=HaveArg and UsedArg;
          If HaveArg then
            begin
            Inc(I); // Skip argument.
            OV:=Paramstr(I);
            end;
          end;
        If HaveArg and ((Result='') or AllErrors) then
          If Assigned(Opts) then
            Opts.Add(O+'='+OV);
        end;
      end;
    Inc(I);
    end;
end;

function TCustomApplication.CheckOptions(const ShortOptions: String;
  const Longopts: array of string; Opts, NonOpts: TStrings; AllErrors: Boolean
  ): String;
Var
  L : TStringList;
  I : Integer;
begin
  L:=TStringList.Create;
  try
    For I:=0 to High(LongOpts) do
      L.Add(LongOpts[i]);
    Result:=CheckOptions(ShortOptions,L,Opts,NonOpts,AllErrors);
  finally
    L.Destroy;
  end;
end;

function TCustomApplication.CheckOptions(const ShortOptions: String;
  const Longopts: TStrings; AllErrors: Boolean): String;
begin
  Result:=CheckOptions(ShortOptions,LongOpts,Nil,Nil,AllErrors);
end;

function TCustomApplication.CheckOptions(const ShortOptions: String;
  const LongOpts: array of string; AllErrors: Boolean): String;

Var
  L : TStringList;
  I : Integer;

begin
  L:=TStringList.Create;
  Try
    For I:=0 to High(LongOpts) do
      L.Add(LongOpts[i]);
    Result:=CheckOptions(ShortOptions,L,AllErrors);
  Finally
    L.Destroy;
  end;
end;

function TCustomApplication.CheckOptions(const ShortOptions: String;
  const LongOpts: String; AllErrors: Boolean): String;

Const
  SepChars = ' '#10#13#9;

Var
  L : TStringList;
  Len,I,J : Integer;

begin
  L:=TStringList.Create;
  Try
    I:=1;
    Len:=Length(LongOpts);
    While I<=Len do
      begin
      While Isdelimiter(SepChars,LongOpts,I) do
        Inc(I);
      J:=I;
      While (J<=Len) and Not IsDelimiter(SepChars,LongOpts,J) do
        Inc(J);
      If (I<=J) then
        L.Add(Copy(LongOpts,I,(J-I)));
      I:=J+1;
      end;
    Result:=CheckOptions(Shortoptions,L,AllErrors);
  Finally
    L.Destroy;
  end;
end;

function TCustomApplication.GetNonOptions(const ShortOptions: String;
  const Longopts: array of string): TStringDynArray;

Var
  NO : TStrings;
  I : Integer;

begin
  No:=TStringList.Create;
  try
    GetNonOptions(ShortOptions,LongOpts,No);
    SetLength(Result,NO.Count);
    For I:=0 to NO.Count-1 do
      Result[I]:=NO[i];
  finally
    NO.Destroy;
  end;
end;

procedure TCustomApplication.GetNonOptions(const ShortOptions: String;
  const Longopts: array of string; NonOptions: TStrings);

Var
  S : String;

begin
  S:=CheckOptions(ShortOptions,LongOpts,Nil,NonOptions,true);
  if (S<>'') then
    Raise EListError.Create(S);
end;

procedure TCustomApplication.GetEnvironmentList(List: TStrings);
begin
  GetEnvironmentList(List,False);
end;

procedure TCustomApplication.Log(EventType: TEventType; const Msg: String);
begin
  If (FEventLogFilter=[]) or (EventType in FEventLogFilter) then
    DoLog(EventType,Msg);
end;

procedure TCustomApplication.Log(EventType: TEventType; const Fmt: String;
  const Args: array of string);
begin
  try
    Log(EventType, Format(Fmt, Args));
  except
    On E: Exception do
      Log(etError,Format('Error formatting message "%s" with %d arguments: %s',
        [Fmt,IntToStr(Length(Args)),E.Message]));
  end
end;

end.

