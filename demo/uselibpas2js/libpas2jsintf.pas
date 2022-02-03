unit LibPas2jsIntf;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$endif}

interface

Const
{$IFDEF UNIX}
  {$IFDEF darwin}
  libpas2js = 'libpas2js.dylib';
  {$DEFINE UseCDecl}
  {$ELSE}
  libpas2js = 'libpas2js.so';
  {$ENDIF}
{$ELSE}
  libpas2js = 'libpas2js.dll';
{$ENDIF}

Type
  PPas2JSCompiler = Pointer;
  PDirectoryCache = Pointer;
  TPas2jsFileAgeTime = longint;
  TPas2jsFileAttr = longint;
  TPas2jsFileSize = int64;
  TLibLogCallBack = Procedure (Data : Pointer; Msg : PAnsiChar; MsgLen : Integer); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
  TWriteJSCallBack = Procedure (Data : Pointer; AFileName: PAnsiChar; AFileNameLen : Integer; AFileData : PAnsiChar; AFileDataLen: Int32); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
  TReadPasCallBack = Procedure (Data : Pointer; AFileName: PAnsiChar; AFileNameLen : Integer; AFileData : PAnsiChar; Var AFileDataLen: Cardinal); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
  TReadDirCallBack = Function (Data : Pointer; P: PDirectoryCache; ADirPath: PAnsiChar): boolean; {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
  TUnitAliasCallBack = Function (Data: Pointer; AUnitName: PAnsiChar; AUnitNameMaxLen: Integer): boolean; {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};

  TSetPas2JSWriteJSCallBack = Procedure (P : PPas2JSCompiler; ACallBack : TWriteJSCallBack; CallBackData : Pointer); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
  TSetPas2JSCompilerLogCallBack = Procedure (P : PPas2JSCompiler; ACallBack : TLibLogCallBack; CallBackData : Pointer); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
  TSetPas2JSReadPasCallBack = Procedure (P : PPas2JSCompiler; ACallBack : TReadPasCallBack;  CallBackData : Pointer; ABufferSize : Cardinal); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
  TSetPas2JSReadDirCallBack = Procedure (P : PPas2JSCompiler; ACallBack : TReadDirCallBack;  CallBackData : Pointer); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
  TAddPas2jsDirectoryEntry = procedure(P: PDirectoryCache; AFilename: PAnsiChar; AAge: TPas2jsFileAgeTime; AAttr: TPas2jsFileAttr; ASize: TPas2jsFileSize); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
  TSetPas2JSUnitAliasCallBack = Procedure (P : PPas2JSCompiler; ACallBack : TUnitAliasCallBack;  CallBackData : Pointer); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
  TRunPas2JSCompiler = Function (P : PPas2JSCompiler; ACompilerExe, AWorkingDir : PAnsiChar; CommandLine : PPAnsiChar; DoReset : Boolean) :Boolean; {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
  TFreePas2JSCompiler = Procedure (P : PPas2JSCompiler); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
  TGetPas2JSCompiler = Function () :  PPas2JSCompiler;{$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};
  TGetPas2JSCompilerLastError = Procedure(P : PPas2JSCompiler; AError : PAnsiChar; Var AErrorLength : Longint; AErrorClass : PAnsiChar; Var AErrorClassLength : Longint); {$IFDEF UseCDecl}cdecl{$ELSE}stdcall{$ENDIF};

Var
  SetPas2JSWriteJSCallBack : TSetPas2JSWriteJSCallBack;
  SetPas2JSCompilerLogCallBack : TSetPas2JSCompilerLogCallBack;
  SetPas2JSReadPasCallBack : TSetPas2JSReadPasCallBack;
  SetPas2JSReadDirCallBack : TSetPas2JSReadDirCallBack;
  AddPas2JSDirectoryEntry : TAddPas2jsDirectoryEntry;
  SetPas2JSUnitAliasCallBack : TSetPas2JSUnitAliasCallBack;
  RunPas2JSCompiler : TRunPas2JSCompiler;
  FreePas2JSCompiler : TFreePas2JSCompiler;
  GetPas2JSCompiler : TGetPas2JSCompiler;
  GetPas2JSCompilerLastError : TGetPas2JSCompilerLastError;

Function LoadPas2JsLibrary(ALibraryName : String = '') : Integer;
Function FreePas2jsLibrary : Integer;

implementation

uses
  {$ifdef fpc}
  {$ifdef fpc_fullversion<30101}
  dynlibs,
  {$endif}
  {$else}
  windows,
  {$endif}
  sysutils;

{$ifndef fpc}
type TLibHandle = THandle;
{$endif}

Var
  LibHandle : TLibHandle;
  RefCount : Integer;

Function LoadPas2JsLibrary(ALibraryName : String = '') : Integer;

  Function GPA(N : String) : Pointer ;

  var
    s: String;
  begin
    Result:=GetProcAddress(LibHandle,PChar(N));
    if Result<>nil then exit;
    s:=Format('Could not load Pas2JS library function "%s" from "%s"',[N,ALibraryName]);
    {$IF defined(Unix) and defined(FPC)}
    s:=s+': '+GetLoadErrorStr;
    {$ENDIF}
    Raise Exception.Create(s);
  end;

var
  s: String;
begin
  if RefCount<>0 then
    Inc(RefCount)
  else
    begin
    if ALibraryName='' then
      ALibraryName:=libpas2js;
    {$ifdef UseDL}
    LibHandle:=dlopen(Pchar(ALibraryName));
    {$ELSE}
    LibHandle:=LoadLibrary(PChar(ALibraryName));
    {$ENDIF}
    if (LibHandle=TLibHandle(0)) then
      begin
      s:=Format('Could not load Pas2JS library "%s"',[ALibraryName]);
      {$IF defined(Unix) and defined(FPC)}
      s:=s+': '+GetLoadErrorStr;
      {$ENDIF}
      Raise Exception.Create(s);
      end;
    // Compiler
    Pointer({$IFNDEF FPC}@{$ENDIF}SetPas2JSWriteJSCallBack):=GPA('SetPas2JSWriteJSCallBack');
    Pointer({$IFNDEF FPC}@{$ENDIF}SetPas2JSCompilerLogCallBack):=GPA('SetPas2JSCompilerLogCallBack');
    Pointer({$IFNDEF FPC}@{$ENDIF}SetPas2JSReadPasCallBack):=GPA('SetPas2JSReadPasCallBack');
    Pointer({$IFNDEF FPC}@{$ENDIF}SetPas2JSReadDirCallBack):=GPA('SetPas2JSReadDirCallBack');
    Pointer({$IFNDEF FPC}@{$ENDIF}AddPas2JSDirectoryEntry):=GPA('AddPas2JSDirectoryEntry');
    Pointer({$IFNDEF FPC}@{$ENDIF}SetPas2JSUnitAliasCallBack):=GPA('SetPas2JSUnitAliasCallBack');
    Pointer({$IFNDEF FPC}@{$ENDIF}RunPas2JSCompiler):=GPA('RunPas2JSCompiler');
    Pointer({$IFNDEF FPC}@{$ENDIF}FreePas2JSCompiler):=GPA('FreePas2JSCompiler');
    Pointer({$IFNDEF FPC}@{$ENDIF}GetPas2JSCompiler):=GPA('GetPas2JSCompiler');
    Pointer({$IFNDEF FPC}@{$ENDIF}GetPas2JSCompilerLastError):=GPA('GetPas2JSCompilerLastError');
    RefCount:=1;
    end;
  Result:=RefCount;  
end;

Function FreePas2jsLibrary : Integer;

begin
  If (RefCount>0) then
    begin
    Dec(RefCount);
    if RefCount=0 then
      begin
      {$IFNDEF FPC}
      FreeLibrary(Libhandle);
      {$else}
      UnloadLibrary(Libhandle);
      {$ENDIF}
      // Compiler
      Pointer({$IFNDEF FPC}@{$ENDIF}SetPas2JSWriteJSCallBack):=nil;
      Pointer({$IFNDEF FPC}@{$ENDIF}SetPas2JSCompilerLogCallBack):=nil;
      Pointer({$IFNDEF FPC}@{$ENDIF}SetPas2JSReadPasCallBack):=nil;
      Pointer({$IFNDEF FPC}@{$ENDIF}SetPas2JSReadDirCallBack):=nil;
      Pointer({$IFNDEF FPC}@{$ENDIF}AddPas2JSDirectoryEntry):=nil;
      Pointer({$IFNDEF FPC}@{$ENDIF}SetPas2JSUnitAliasCallBack):=nil;
      Pointer({$IFNDEF FPC}@{$ENDIF}RunPas2JSCompiler):=nil;
      Pointer({$IFNDEF FPC}@{$ENDIF}FreePas2JSCompiler):=nil;
      Pointer({$IFNDEF FPC}@{$ENDIF}GetPas2JSCompiler):=nil;
      Pointer({$IFNDEF FPC}@{$ENDIF}GetPas2JSCompilerLastError):=nil;
      end;
    end;
  Result:=RefCount;
end;

end.

