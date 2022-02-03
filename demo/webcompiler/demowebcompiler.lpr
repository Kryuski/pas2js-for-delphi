program demowebcompiler;

{$mode objfpc}

uses
  Classes, SysUtils, Web, webfilecache, pas2jswebcompiler;

Type

  { TWebCompilerDemo }

  TWebCompilerDemo = Class(TComponent)
  Private
    BCompile : TJSHTMLElement;
    BDefaultUnits : TJSHTMLElement;
    BThisUnit : TJSHTMLElement;
    EUnitName : TJSHTMLInputElement;
    BRun : TJSHTMLElement;
    MSource : TJSHTMLInputElement;
    MLog: TJSHTMLInputElement;
    MUnits: TJSHTMLInputElement;
    RFrame : TJSHTMLIFrameElement;
    PResult : TJSHTMLElement;
    FCompiler : TPas2JSWebCompiler;
    procedure ActivateTab(aTab: String);
    procedure ClearResult;
    procedure DoLog(Sender: TObject; const Msg: String);
    function LoadDefaultsClick(aEvent: TJSMouseEvent): boolean;
    function LoadSingleUnitClick(aEvent: TJSMouseEvent): boolean;
    procedure OnUnitLoaded(Sender: TObject; aFileName: String; aError: string);
    function RunClick(aEvent: TJSMouseEvent): boolean;
  Protected
    function CompileClick(aEvent: TJSMouseEvent): boolean;
    Procedure LinkElements;
    Property Compiler : TPas2JSWebCompiler Read FCompiler;
  Public
    Constructor Create(aOwner : TComponent); override;
    Procedure Execute;
  end;

Const
  // Default run HTML page, shown in IFrame.

  SHTMLHead =
    '<HTML>'+LineEnding+
    '<head>'+LineEnding+
    '  <meta charset="UTF-8">'+LineEnding+
    '  <Title>Pas2JS web compiler Program output</Title>'+LineEnding+
    '  <script type="application/javascript">'+LineEnding;

  SHTMLTail =
    '   </script>'+LineEnding+
    '  <link href="https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css" rel="stylesheet">'+LineEnding+
    '</head>'+LineEnding+
    '<body>'+LineEnding+
    '  <div class="container">'+LineEnding+
    '    <div class="panel panel-info">'+LineEnding+
    '      <div class="panel-heading">Run program output</div>'+LineEnding+
    '      <div class="panel-body">'+
    '        <div id="pasjsconsole" style="width: 640px; height: 200px;">'+LineEnding+
    '      </div>'+LineEnding+
    '    </div>'+LineEnding+
    '  </div>'+LineEnding+
    '</div>'+LineEnding+
    '<script>'+LineEnding+
    '  rtl.run();'+LineEnding+
    '</script>'+LineEnding+
    '</body>'+LineEnding+
    '</HTML>';


{ TWebCompilerDemo }

procedure TWebCompilerDemo.OnUnitLoaded(Sender: TObject; aFileName: String; aError: string);
begin
  if aError='' then
    MUnits.Value:=MUnits.Value+sLineBreak+'Loaded: '+aFileName
  else
    MUnits.Value:=MUnits.Value+sLineBreak+'Error Loading "'+aFileName+'": '+AError;
  if SameText(afilename,EUnitName.Value) then
    EUnitName.Value:='';
end;

procedure TWebCompilerDemo.LinkElements;
begin
  BCompile:=TJSHTMLElement(Document.getElementById('btn-compile'));
  BCompile.onclick:=@CompileClick;
  BRun:=TJSHTMLElement(Document.getElementById('btn-run'));
  BRun.onClick:=@RunClick;
  MSource:=TJSHTMLInputElement(Document.getElementById('memo-program-src'));
  MLog:=TJSHTMLInputElement(Document.getElementById('memo-compiler-output'));
  MUnits:=TJSHTMLInputElement(Document.getElementById('memo-loaded-units'));
  RFrame:=TJSHTMLIFrameElement(Document.getElementById('runarea'));
  BDefaultUnits:=TJSHTMLElement(Document.getElementById('btn-load-defaults'));
  BDefaultUnits.Onclick:=@LoadDefaultsClick;
  BThisUnit:=TJSHTMLElement(Document.getElementById('btn-load-unit'));
  BThisUnit.Onclick:=@LoadSingleUnitClick;
  EUnitName:=TJSHTMLInputElement(Document.getElementById('edt-load-unit-name'));
  PResult:=TJSHTMLElement(Document.getElementById('compile-result'));
end;

constructor TWebCompilerDemo.Create(aOwner : TComponent);
begin
  Inherited;
  FCompiler:=TPas2JSWebCompiler.Create;
  Compiler.Log.OnLog:=@DoLog;
  Compiler.WebFS.LoadBaseURL:='sources';
end;

function TWebCompilerDemo.RunClick(aEvent: TJSMouseEvent): boolean;

Var
  Src : String;

begin
  Result:=True;
  Src:=Compiler.WebFS.GetFileContent('main.js');
  if Src='' then
    begin
    Window.Alert('No source available');
    exit;
    end;
  Src:=SHTMLHead+Src+LineEnding+SHTMLTail;
  RFrame['srcdoc']:=Src;
end;

procedure TWebCompilerDemo.DoLog(Sender: TObject; const Msg: String);
begin
  MLog.Value:=MLog.Value+sLineBreak+Msg;
end;

function TWebCompilerDemo.LoadDefaultsClick(aEvent: TJSMouseEvent): boolean;
begin
  Result:=False;
  Compiler.WebFS.LoadFiles(['rtl.js','system.pas','sysutils.pas','types.pas','typinfo.pas','classes.pas','rtlconsts.pas','js.pas','web.pas','browserconsole.pas'],@OnUnitLoaded);
end;

function TWebCompilerDemo.LoadSingleUnitClick(aEvent: TJSMouseEvent): boolean;
begin
  Result:=False;
  Compiler.WebFS.LoadFile(EUnitName.Value,@OnUnitLoaded);
end;


Procedure TWebCompilerDemo.ActivateTab(aTab : String);

begin
  asm
   $("#act-"+aTab).tab('show');
  end;
end;


Procedure TWebCompilerDemo.ClearResult;

begin
  While PResult.firstElementChild<>Nil do
    PResult.removeChild(PResult.firstElementChild);
end;

function TWebCompilerDemo.CompileClick(aEvent: TJSMouseEvent): boolean;

  Procedure ShowResult(success : boolean);

  Const
    CloseLink = '<a href="#" class="close" data-dismiss="alert" aria-label="close">&times;</a>';

  Var
    E : TJSHTMLElement;

  begin
    ClearResult;
    E:=TJSHTMLElement(document.createElement('div'));
    if Success then
      begin
      E['class']:='alert alert-success alert-dismissible fade in';
      E.innerHTML:=CloseLink+'<strong>Succes!</strong> program compiled succesfully. You can run the program now.';
      end
    else
      begin
        E['class']:='alert alert-danger alert-dismissible fade in';
        E.innerHTML:=CloseLink+'<strong>Failure</strong> failed to compile program, please check error messages.';
      end;
    PResult.appendChild(E);
  end;

Var
  args : TStrings;
  Res : Boolean;

begin
  Result:=False;
  BRun['disabled']:='disabled';
  ClearResult;
  MLog.Value:='';
  Compiler.WebFS.SetFileContent('main.pp',MSource.value);
  args:=TStringList.Create;
  try
    Args.Add('-Tbrowser');
    Args.Add('-Jc');
    Args.Add('-Jirtl.js');
    Args.Add('main.pp');
    ActivateTab('output');
    RFrame.Src:='run.html';
    Compiler.Run('','',Args,True);
    Res:=Compiler.ExitCode=0;
    ShowResult(Res);
    if Res then
      BRun.removeAttribute('disabled');
  finally
   Args.Free;
  end;
end;

procedure TWebCompilerDemo.Execute;
begin
  LinkElements;
end;

begin
  With TWebCompilerDemo.Create(Nil) do
    Execute;
end.
