{ Author: Mattias Gaertner  2018  mattias@freepascal.org

  Abstract:
    Command line interface for the pas2js compiler.
}
program pas2js;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  Classes,
  SysUtils,
  Pas2jsFileUtils in '..\..\packages\pastojs\src\Pas2jsFileUtils.pas',
  Pas2jsLogger in '..\..\packages\pastojs\src\Pas2jsLogger.pas',
  Pas2jsCompiler in '..\..\packages\pastojs\src\Pas2jsCompiler.pas',
  Pas2JSFSCompiler in '..\..\packages\pastojs\src\Pas2JSFSCompiler.pas',
  Pas2JSCompilerPP in '..\..\packages\pastojs\src\Pas2JSCompilerPP.pas',
  Pas2JSCompilerCfg in '..\..\packages\pastojs\src\Pas2JSCompilerCfg.pas',
  PasResolver in '..\..\packages\fcl-passrc\src\PasResolver.pas',
  pastree in '..\..\packages\fcl-passrc\src\pastree.pas',
  PScanner in '..\..\packages\fcl-passrc\src\PScanner.pas',
  PParser in '..\..\packages\fcl-passrc\src\PParser.pas',
  PasResolveEval in '..\..\packages\fcl-passrc\src\PasResolveEval.pas',
  FPPas2Js in '..\..\packages\pastojs\src\FPPas2Js.pas',
  jswriter in '..\..\packages\fcl-js\src\jswriter.pas',
  fppjssrcmap in '..\..\packages\pastojs\src\fppjssrcmap.pas',
  Pas2jsFileCache in '..\..\packages\pastojs\src\Pas2jsFileCache.pas';

type
  { TPas2jsCLI }
  TPas2jsCLI = class
  private
    FCompiler: TPas2JSFSCompiler;
    FWriteOutputToStdErr: Boolean;
  protected
    procedure DoRun;
  public
    constructor Create;
    destructor Destroy; override;
    property Compiler: TPas2JSFSCompiler read FCompiler;
    property WriteOutputToStdErr: Boolean read FWriteOutputToStdErr write FWriteOutputToStdErr;
  end;

procedure TPas2jsCLI.DoRun;
var
  ParamList: TStringList;
  i: Integer;
begin
  ParamList:=TStringList.Create;
  try
    for i:=1 to ParamCount do
      ParamList.Add(ParamStr(i));
    try
      Compiler.Run(ParamStr(0),GetCurrentDirPJ,ParamList);
    except
      on E: ECompilerTerminate do ;
      on E: Exception do
      begin
        {AllowWriteln}
        writeln(E.Message);
        {AllowWriteln-}
        if ExitCode=0 then
          ExitCode:=ExitCodeErrorInternal;
      end;
    end;
  finally
    ParamList.Free;
    Compiler.Log.CloseOutputFile;
  end;
end;

constructor TPas2jsCLI.Create;
begin
  inherited ;
  FCompiler := TPas2JSFSCompiler.Create;
  FCompiler.ConfigSupport := TPas2JSFileConfigSupport.Create(FCompiler);
  FCompiler.PostProcessorSupport := TPas2JSFSPostProcessorSupport.Create(FCompiler);
end;

destructor TPas2jsCLI.Destroy;
begin
  FreeAndNil(FCompiler);
  inherited Destroy;
end;

var
  Application: TPas2jsCLI;
begin
  Application := TPas2jsCLI.Create;
  Application.DoRun;
  Application.Free;
end.

