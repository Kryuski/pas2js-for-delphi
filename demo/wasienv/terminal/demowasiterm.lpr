program demowasiterm;

{$mode objfpc}

uses
  browserapp, JS, Classes, SysUtils, Web, WebAssembly, types,
  xterm, wasienv, strutils;

Type

  { TMyApplication }

  TMyApplication = class(TBrowserApplication)
  Private
    FTerminal : TXTerm.TTerminal;
    FTermEl : TJSHTMLElement;
    FWasiEnv : TPas2JSWASIEnvironment;
    FMemory : TJSWebAssemblyMemory; // Memory of webassembly
    FTable:TJSWebAssemblyTable; // Exported functions, directly callable
    FInputLine : String;
    Fidx : Integer;
    FinputLines : TStrings;
    function CreateWebAssembly(Path: string; ImportObject: TJSObject
      ): TJSPromise;
    procedure DoGetInputString(Sender: TObject; var AInput: string);
    function DoKey(keyData: TXTerm.TOnKeyCallbackDataType): Boolean;
    procedure DoWrite(Sender: TObject; const aOutput: String);
    function initEnv(aValue: JSValue): JSValue;
    procedure InitWebAssembly;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    procedure doRun; override;
  end;

function TMyApplication.InitEnv(aValue: JSValue): JSValue;

Var
  Module : TJSInstantiateResult absolute aValue;
  exps : TWASIExports;

begin
  Result:=True;
  Exps := TWASIExports(TJSObject(Module.Instance.exports_));
  FWasiEnv.Instance:=Module.Instance;
  Exps.Start;
end;

function TMyApplication.DoKey(keyData : TXTerm.TOnKeyCallbackDataType): Boolean;
{
  Experimental code to convert keystrokes to input.
  Unfortunately, it does not work because the webassembly is executed in the same thread as the javascript.
  Maybe using a web-worker this can be realized.
}
Var
  printable : Boolean;

begin
  Result:=true;
  With keyData do
    printable :=Not (domEvent.altKey or domEvent.metaKey or domEvent.ctrlKey or domEvent.metaKey);
  if keyData.domEvent.Key=TJSKeyNames.BackSpace then
    begin
    if (Length(FInputLine)>0) then
      begin
      FInputLine:=Copy(FInputLine,1,Length(FInputLine)-1);
      FTerminal.write(#8' '#8);
      end;
    end
  else if (keyData.domEvent.Key=TJSKeyNames.Enter) then
    begin
    FInputLine:=FInputLine+#10;
    FTerminal.writeln('');
    end
  else if Printable then
    begin
    FInputLine:=FInputLine+keyData.Key;
    FTerminal.write(keyData.Key);
    end;
end;


procedure TMyApplication.DoWrite(Sender: TObject; const aOutput: String);

Var
  S : TStringDynArray;
  L : String;
  I,aLen : Integer;

begin
  S:=SplitString(aOutput,#10);
  aLen:=length(S)-1;
  For I:=0 to aLen do
    begin
    L:=S[i];
    FTerminal.Write(L);
    if I<aLen then
      FTerminal.Writeln('')
    end;
end;

constructor TMyApplication.Create(aOwner: TComponent);
Var
  i : Integer;
begin
  inherited Create(aOwner);
  FWasiEnv:=TPas2JSWASIEnvironment.Create;
  FWasiEnv.OnStdErrorWrite:=@DoWrite;
  FWasiEnv.OnStdOutputWrite:=@DoWrite;
  FWasiEnv.OnGetConsoleInputString:=@DoGetInputString;
  FTermEl:=GetHTMLElement('xterm');
  FTerminal:=TXTerm.TTerminal.New;
  FTerminal.OnKey(@DoKey);
  FTerminal.open(FTermEl);
  FinputLine:='';
  FinputLines:=TStringList.Create;
  Fidx:=0;
  For I:=0 to 9 do
    FinputLines.Add(IntToStr(Random(200)));
  FinputLines.Add('-1');
end;

function TMyApplication.CreateWebAssembly(Path: string; ImportObject: TJSObject): TJSPromise;

begin
  Result:=window.fetch(Path)._then(Function (res : jsValue) : JSValue
    begin
      Result:=TJSResponse(Res).arrayBuffer._then(Function (res2 : jsValue) : JSValue
        begin
          Result:=TJSWebAssembly.instantiate(TJSArrayBuffer(res2),ImportObject);
        end,Nil)
    end,Nil
  );
 end;

procedure TMyApplication.DoGetInputString(Sender: TObject; var AInput: string);

Var
  S : String;

begin
  {
  // Experimental code for use with keyhandler.
  aInput:=FInputLine;
  FInputLine:='';
  }
  aInput:='';
  If FIdx<FInputLines.Count then
    begin
    S:=FInputLines[FIdx];
    aInput:=S+#10;
    FTerminal.writeln(#27'[31m'+S+#27'[37m');
    inc(FIdx);
    end;
end;

procedure TMyApplication.InitWebAssembly;

Var
  mDesc : TJSWebAssemblyMemoryDescriptor;
  tDesc: TJSWebAssemblyTableDescriptor;
  ImportObj : TJSObject;


begin
  //  Setup memory
  mDesc.initial:=256;
  mDesc.maximum:=256;
  FMemory:=TJSWebAssemblyMemory.New(mDesc);
  // Setup table
  tDesc.initial:=0;
  tDesc.maximum:=0;
  tDesc.element:='anyfunc';
  FTable:=TJSWebAssemblyTable.New(tDesc);
  // Setup ImportObject
  ImportObj:=new([
    'js', new([
      'mem', FMemory,
      'tbl', FTable
    ])
  ]);
  FWasiEnv.AddImports(ImportObj);
  CreateWebAssembly('sums.wasm',ImportObj)._then(@initEnv)
end;


destructor TMyApplication.Destroy;
begin
  FreeAndNil(FWasiEnv);
  inherited Destroy;
end;

procedure TMyApplication.doRun;

begin
  // Your code here
  Terminate;
  InitWebAssembly;
end;

var
  Application : TMyApplication;

begin
  Application:=TMyApplication.Create(nil);
  Application.Initialize;
  Application.Run;
end.
