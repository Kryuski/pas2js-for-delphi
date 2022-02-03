unit Pas2jsCompilerProxy;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{$IFDEF darwin}
{$DEFINE UseCDecl}
{$ENDIF}

interface

uses
  Classes, SysUtils, LibPas2jsIntf;

Type

   { TPas2JSCompilerProxy }

   TPas2JSCompilerProxy = class(TObject)
   Private
     FCompiler : PPas2JSCompiler;
     FPasFile: TFileStream;
   Protected
     Procedure WriteLog(Const S : AnsiString);
     Procedure WriteJS(Const AFileName,AFileData : AnsiString);
     Procedure StartReadPasFile(Const AFileName : AnsiString);
     Procedure ReadChunk(ABuffer : PAnsiChar; Var AChunkSize : Cardinal);
     Procedure DoneReadPasFile;
   Public
     Constructor Create; virtual;
     Destructor Destroy; override;
     Procedure Run(ACompilerExe, AWorkingDir : String; CommandLine : TStringList; DoReset : Boolean);
     Procedure Execute;
     Property PasFile : TFileStream Read FPasFile;
   end;

implementation

{$ifndef fpc}
const
  AllFilesMask = '*.*';
type
  TUnicodeSearchRec = TSearchRec;
{$endif}

Procedure DoLog(Data : Pointer; Msg : PansiChar; MsgLen : Integer); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
Var
  S : AnsiString;
begin
  SetLength(S{%H-},MsgLen);
  if MsgLen>0 then
    Move(Msg^,S[1],MsgLen);
  TPas2JSCompilerProxy(Data).WriteLog(S);
end;

Procedure DoWriteJS(Data : Pointer; AFileName: PAnsiChar; AFileNameLen : Integer;
  AFileData : PAnsiChar; AFileDataLen: Int32); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
Var
  Src,DestFileName : AnsiString;
begin
  SetLength(DestFileName{%H-},AFileNameLen);
  if AFileNameLen>0 then
    Move(AFileName^,DestFileName[1],AFileNameLen);
  SetLength(Src{%H-},AFileDataLen);
  if AFileDataLen>0 then
    Move(AFileData^,Src[1],AFileDataLen);
  TPas2JSCompilerProxy(Data).WriteJS(DestFileName,Src);
end;


procedure DoReadPasJS(Data: Pointer; AFileName: PAnsiChar; AFileNameLen: Integer;
  AFileData: PAnsiChar; Var AFileDataLen: Cardinal);
  {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
Var
  DestFileName : AnsiString;
  BytesToRead : Cardinal;
begin
  SetLength(DestFileName{%H-},AFileNameLen);
  if AFileNameLen>0 then
    Move(AFileName^,DestFileName[1],AFileNameLen);
  TPas2JSCompilerProxy(Data).StartReadPasFile(AFileName);
  BytesToRead:=AFileDatalen;
  TPas2JSCompilerProxy(Data).ReadChunk(AFileData,AFileDataLen);
  if AFileDatalen<BytesToRead then
    TPas2JSCompilerProxy(Data).DoneReadPasFile;
end;

function DoReadDir(Data: Pointer; P: PDirectoryCache; ADirPath: PAnsiChar): boolean;
  {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
var
  Info: TUnicodeSearchRec;
  Filename: String;
  Path: PAnsiChar;
begin
  if Data=nil then ;
  Path:=ADirPath;
  // Note: do not add a 'if not DirectoryExists then exit'.
  // This will not work on automounted directories. You must use FindFirst.
  if FindFirst(UnicodeString(Path+AllFilesMask),faAnyFile,Info)=0 then begin
    repeat
      // check if special file
      if (Info.Name='.') or (Info.Name='..') or (Info.Name='')
      then
        continue;
      // add file
      Filename:=AnsiString(Info.Name);
      AddPas2JSDirectoryEntry(P,PAnsiChar(Filename),Info.Time,Info.Attr,Info.Size);
    until FindNext(Info)<>0;
  end;
  FindClose(Info);
  Result:=true;
end;

Function DoUnitAlias(Data: Pointer; AUnitName: PAnsiChar;
  AUnitNameMaxLen: Integer): boolean; {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
var
  Old, New: AnsiString;
begin
  Result:=false;
  Old:=AUnitName;
  if Data=nil then ;
  New:='';

  if SameText(Old,'Test.Foo.Alias1') then
    New:='Bar'
  else if SameText(Old,'Test.Foo.Alias2') then
    New:='Test.Foo.SomeLongUnitName';

  if New<>'' then
    begin
    writeln('Info: DoUnitAlias Old="',Old,'" New="',New,'"');
    if AUnitNameMaxLen<length(New) then
      raise Exception.Create('unit alias too long');
    System.Move(New[1],AUnitName^,length(New)+1);
    Result:=true;
    end;
end;

{ TPas2JSCompilerProxy }

procedure TPas2JSCompilerProxy.WriteLog(const S: AnsiString);
begin
  Writeln('Log : ',S);
end;

procedure TPas2JSCompilerProxy.WriteJS(const AFileName, AFileData: AnsiString);
Var
  F : TFileStream;
begin
  F:=TFileStream.Create(AFileName,fmCreate);
  try
    F.WriteBuffer(AFileData[1],Length(AFileData));
  finally
    F.Free;
  end;
end;

procedure TPas2JSCompilerProxy.StartReadPasFile(const AFileName: AnsiString);
begin
  If Assigned(FPasFile) and SameFileName(AFileName,FPasFile.FileName) then
    exit;
  FreeAndNil(FPasFile);
  FPasFile:=TFileStream.Create(aFileName,fmOpenRead or fmShareDenyWrite);
end;

procedure TPas2JSCompilerProxy.ReadChunk(ABuffer: PAnsiChar; Var AChunkSize: Cardinal);
begin
  if Not assigned(FPasFile) then
    AChunkSize:=0
  else
    AChunkSize:=FPasFile.Read(ABuffer^,AChunkSize);
end;

procedure TPas2JSCompilerProxy.DoneReadPasFile;
begin
  FreeAndNil(FPasFile);
end;

constructor TPas2JSCompilerProxy.Create;
begin
  FCompiler:=GetPas2JSCompiler();
  SetPas2JSCompilerLogCallBack(FCompiler,@DoLog,Self);
  SetPas2JSWriteJSCallBack(FCompiler,@DoWriteJS,Self);
  SetPas2JSReadPasCallBack(FCompiler,@DoReadPasJS,Self,16*1024);
  SetPas2JSReadDirCallBack(FCompiler,@DoReadDir,Self);
  SetPas2JSUnitAliasCallBack(FCompiler,@DoUnitAlias,Self);
end;

destructor TPas2JSCompilerProxy.Destroy;
begin
  inherited Destroy;
end;

procedure TPas2JSCompilerProxy.Run(ACompilerExe, AWorkingDir: String; CommandLine: TStringList; DoReset: Boolean);
Var
  SCmdLn : Array Of AnsiString;
  CmdLn : Array Of PAnsiChar;
  Err,ErrClassname : AnsiString;
  I,ErrorLength,ErrorClassLength : Integer;
begin
  SetLength(SCmdLn{%H-},CommandLine.Count);
  SetLength(CmdLn{%H-},CommandLine.Count+1);
  For I:=0 to CommandLine.Count-1 do
    begin
    SCmdLn[i]:=CommandLine[i]; // CommandLine[i] might return a temporary string -> make sure it is valid during this proc
    CmdLn[i]:=PAnsiChar(SCmdLn[i]);
    end;
  CmdLn[CommandLine.Count]:=Nil;

  if not RunPas2JSCompiler(FCompiler,PAnsiChar(ACompilerExe),PAnsiChar(AWorkingDir),PPAnsiChar(Cmdln),DoReset) then
    begin
    ErrorLength:=1024;
    ErrorClassLength:=1024;
    SetLength(Err{%H-},ErrorLength);
    SetLength(ErrClassname{%H-},ErrorClassLength);
    GetPas2JSCompilerLastError(FCompiler,@Err[1],ErrorLength,@ErrClassname[1],ErrorClassLength);
    SetLength(Err,ErrorLength);
    SetLength(ErrClassname,ErrorClassLength);
    writeln(Format('Error of class "%s" raised when compiling : %s',[ErrClassname,Err]));
    ExitCode:=1;
    end;
end;

procedure TPas2JSCompilerProxy.Execute;
Var
  Cmd : TStringList;
  I : integer;
begin
  Cmd:=TStringList.Create;
  try
    for I:=1 to ParamCount do
      Cmd.Add(Paramstr(i));
    Run(ParamStr(0),GetCurrentDir,Cmd,False);
  finally
    Cmd.Free;
  end;
end;

end.

