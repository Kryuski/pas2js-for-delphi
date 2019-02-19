{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2018  Mattias Gaertner  mattias@freepascal.org

    Pascal to Javascript converter class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

  Abstract:
    TPas2jsFileResolver extends TFileResolver and searches source files.
}
unit Pas2jsFileCache;

{$i pas2js_defines.inc}

interface

uses
  {$IFDEF Pas2js}
    {$IFDEF NodeJS}
    JS, NodeJSFS,
    {$ENDIF}
  {$ENDIF}
  Classes, SysUtils,
  fpjson,
  PScanner, PasResolver, PasUseAnalyzer,
  Pas2jsLogger, Pas2jsFileUtils, Pas2JSFS;


type
  EPas2jsFileCache = class(EPas2JSFS);

type
  TPas2jsFileAgeTime = TDateTime;
  TPas2jsFileAttr = longint;
  TPas2jsFileSize = TMaxPrecInt;
  TPas2jsSearchFileCase = (
    sfcDefault,
    sfcCaseSensitive,
    sfcCaseInsensitive
  );

  TPas2jsCachedDirectories = class;

  TPas2jsCachedDirectoryEntry = class
  public
    Name: string;
    Time: TPas2jsFileAgeTime; // modification time
    Attr: TPas2jsFileAttr;
    Size: TPas2jsFileSize;
  end;

  { TPas2jsCachedDirectory }

  TPas2jsCachedDirectory = class
  private
    FChangeStamp: TChangeStamp;
    FPath: string;
    FEntries: TFPList; // list of TPas2jsCachedDirectoryEntry
    FPool: TPas2jsCachedDirectories;
    FRefCount: integer;
    FSorted: Boolean;
    function GetEntries(Index: integer): TPas2jsCachedDirectoryEntry; inline;
    procedure SetSorted(const AValue: Boolean);
  protected
    procedure DoReadDir; virtual;
  public
    constructor Create(aPath: string; aPool: TPas2jsCachedDirectories);
    destructor Destroy; override;
    function Count: integer;
    procedure Clear;
    property ChangeStamp: TChangeStamp read FChangeStamp write FChangeStamp;// set on Update to Pool.ChangeStamp
    function NeedsUpdate: Boolean;
    procedure Update;
    procedure Reference;
    procedure Release;
    property RefCount: integer read FRefCount;
    function Add(const Name: string; Time: TPas2jsFileAgeTime;
      Attr: TPas2jsFileAttr; Size: TPas2jsFileSize): TPas2jsCachedDirectoryEntry;
    function FindFile(const ShortFilename: string;
                      const FileCase: TPas2jsSearchFileCase): string;
    function FileAge(const ShortFilename: string): TPas2jsFileAgeTime;
    function FileAttr(const ShortFilename: string): TPas2jsFileAttr;
    function FileSize(const ShortFilename: string): TPas2jsFileSize;
    function IndexOfFileCaseInsensitive(const ShortFilename: string): integer;
    function IndexOfFileCaseSensitive(const ShortFilename: string): integer;
    function IndexOfFile(const ShortFilename: string): integer; inline;
    function CountSameNamesCaseInsensitive(Index: integer): integer;
    procedure GetSameNamesCaseInsensitive(Index: integer; List: TStrings);
    property Entries[Index: integer]: TPas2jsCachedDirectoryEntry read GetEntries; default;
    procedure GetFiles(var Files: TStrings;
      IncludeDirs: Boolean = True // add faDirectory as well
      ); // returns relative file names
    procedure CheckConsistency;
    procedure WriteDebugReport;
    property Path: string read FPath; // with trailing path delimiter
    property Pool: TPas2jsCachedDirectories read FPool;
    property Sorted: Boolean read FSorted write SetSorted; // descending, sort first case insensitive, then sensitive
  end;

  TReadDirectoryEvent = function(Dir: TPas2jsCachedDirectory): Boolean of object;// True = skip default function

  { TPas2jsCachedDirectories }

  TPas2jsCachedDirectories = class
  private
    FChangeStamp: TChangeStamp;
    FDirectories: TPasAnalyzerKeySet;// set of TPas2jsCachedDirectory, key is Directory
    FWorkingDirectory: string;
  private
    FOnReadDirectory: TReadDirectoryEvent;
    type
      TFileInfo = record
        Filename: string;
        DirPath: string;
        ShortFilename: string;
        Dir: TPas2jsCachedDirectory;
      end;
    function GetFileInfo(var Info: TFileInfo): Boolean;
    procedure SetWorkingDirectory(const AValue: string);
  public
    constructor Create;
    destructor Destroy; override;
    property ChangeStamp: TChangeStamp read FChangeStamp;
    procedure Invalidate; inline;
    procedure Clear;
    function DirectoryExists(Filename: string): Boolean;
    function FileExists(Filename: string): Boolean;
    function FileExistsI(var Filename: string): integer; // returns number of found files
    function FileAge(Filename: string): TPas2jsFileAgeTime;
    function FileAttr(Filename: string): TPas2jsFileAttr;
    function FileSize(Filename: string): TPas2jsFileSize;
    function FindDiskFilename(const Filename: string;
                              {%H-}SearchCaseInsensitive: Boolean = False): string; // using Pascal case insensitivity, not UTF-8
    procedure GetListing(const aDirectory: string; var Files: TStrings;
        IncludeDirs: Boolean = True // add faDirectory as well
        ); // returns relative file names
    function GetDirectory(const Directory: string;
                      CreateIfNotExists: Boolean = True;
                      DoReference: Boolean = True): TPas2jsCachedDirectory;
    property WorkingDirectory: string read FWorkingDirectory write SetWorkingDirectory; // used for relative filenames, contains trailing path delimiter
    property OnReadDirectory: TReadDirectoryEvent read FOnReadDirectory write FOnReadDirectory;
  end;

type
  TPas2jsFilesCache = class;
  TPas2jsCachedFile = class;

  { TPas2jsFileResolver }

  TPas2jsFileResolver = class(TPas2JSFSResolver)
  private
    function GetCache: TPas2jsFilesCache;
  public
    constructor Create(aCache: TPas2jsFilesCache); reintroduce;
    // Redirect all calls to cache.
    property Cache: TPas2jsFilesCache read GetCache;
  end;

  { TPas2jsFileLineReader }

  TPas2jsFileLineReader = class(TSourceLineReader)
  private
    FCachedFile: TPas2jsCachedFile;
  Protected
    Procedure IncLineNumber; override;
    property CachedFile: TPas2jsCachedFile read FCachedFile;
  public
    constructor Create(const AFilename: string); overload; override;
    constructor Create(aFile: TPas2jsCachedFile); overload;
  end;

  { TPas2jsCachedFile }

  TPas2jsCachedFile = class(TPas2JSFile)
  private
    FChangeStamp: TChangeStamp;
    FFileEncoding: string;
    FLastErrorMsg: string;
    FLoaded: Boolean;
    FLoadedFileAge: TDateTime;
    FCacheStamp: TChangeStamp; // Cache.ResetStamp when file was loaded
    function GetCache: TPas2jsFilesCache;
    function GetIsBinary: Boolean; inline;
  Protected
    property IsBinary: Boolean read GetIsBinary;
    property FileEncoding: string read FFileEncoding;
    property Cache: TPas2jsFilesCache read GetCache;
    property ChangeStamp: TChangeStamp read FChangeStamp;// changed when Source changed
    property Loaded: Boolean read FLoaded; // Source valid, but may contain an old version
    property LastErrorMsg: string read FLastErrorMsg;
    property LoadedFileAge: TDateTime read FLoadedFileAge;// only valid if Loaded=True
  public
    constructor Create(aCache: TPas2jsFilesCache; const aFilename: string); reintroduce;
    function Load(RaiseOnError: Boolean; Binary: Boolean = False): Boolean; override;
    function CreateLineReader(RaiseOnError: Boolean): TSourceLineReader; override;
  end;

  TPas2jsReadFileEvent = function(aFilename: string; var aSource: RawByteString): Boolean of object;
  TPas2jsWriteFileEvent = procedure(aFilename: string; Source: RawByteString) of object;

  TPas2jsSearchPathKind = (
    spkPath,      // e.g. unitpaths, includepaths
    spkIdentifier // e.g. namespaces, trailing - means remove
    );

  { TPas2jsFilesCache }

  TPas2jsFilesCache = class (TPas2JSFS)
  private
    FBaseDirectory: string;
    FDirectoryCache: TPas2jsCachedDirectories;
    FFiles: TPasAnalyzerKeySet; // set of TPas2jsCachedFile, key is Filename
    FForeignUnitPaths: TStringList;
    FForeignUnitPathsFromCmdLine: integer;
    FIncludePaths: TStringList;
    FIncludePathsFromCmdLine: integer;
    FLog: TPas2jsLogger;
    FOnReadFile: TPas2jsReadFileEvent;
    FOnWriteFile: TPas2jsWriteFileEvent;
    FResetStamp: TChangeStamp;
    FUnitPaths: TStringList;
    FUnitPathsFromCmdLine: integer;
    FPCUPaths: TStringList;
    function FileExistsILogged(var Filename: string): integer;
    function FileExistsLogged(const Filename: string): Boolean;
    function GetOnReadDirectory: TReadDirectoryEvent;
    procedure RegisterMessages;
    procedure SetBaseDirectory(AValue: string);
    function AddSearchPaths(const Paths: string; Kind: TPas2jsSearchPathKind;
      FromCmdLine: Boolean; var List: TStringList; var CmdLineCount: integer): string;
    procedure SetOnReadDirectory(AValue: TReadDirectoryEvent);
  protected
    function FindSourceFileName(const aFilename: string): string; override;
    function GetHasPCUSupport: Boolean; virtual;
    function ReadFile(Filename: string; var Source: RawByteString): Boolean; virtual;
    procedure FindMatchingFiles(Mask: string; MaxCount: integer; Files: TStrings);// find files, matching * and ?
  public
    constructor Create(aLog: TPas2jsLogger); reintroduce; overload;
    destructor Destroy; override;
    procedure Reset; override;
    procedure WriteFoldersAndSearchPaths; override;
    procedure GetPCUDirs(aList: TStrings; const aBaseDir: string); override;
    function PCUExists(var aFileName: string): Boolean; override;
    Function SameFileName(Const File1,File2 : string) : Boolean;  override;
    Function File1IsNewer(const File1, File2: string): Boolean; override;
    function SearchLowUpCase(var Filename: string): Boolean;
    function FindCustomJSFileName(const aFilename: string): string; override;
    function FindUnitJSFileName(const aUnitFilename: string): string; override;
    function FindUnitFileName(const aUnitname, InFilename, ModuleDir: string; out IsForeign: boolean): String; override;
    function FindIncludeFileName(const aFilename: string): string; override;
    function AddIncludePaths(const Paths: string; FromCmdLine: Boolean; out ErrorMsg: string): Boolean;
    function AddUnitPaths(const Paths: string; FromCmdLine: Boolean; out ErrorMsg: string): Boolean;
    function AddSrcUnitPaths(const Paths: string; FromCmdLine: Boolean; out ErrorMsg: string): Boolean;
    function CreateResolver: TPas2jsFSResolver; override;
    function FormatPath(const aPath: string): string; override;
    Function DirectoryExists(Const Filename: string): Boolean; override;
    function FileExists(const Filename: string): Boolean; override;
    function FileExistsI(var Filename: string): integer; // returns number of found files
    function FileAge(const Filename: string): TPas2jsFileAgeTime; virtual;
    function FindFile(Filename: string): TPas2jsCachedFile;
    function LoadFile(Filename: string; Binary: Boolean = False): TPas2jsFile; override;
    function NormalizeFilename(const Filename: string; RaiseOnError: Boolean): string;
    procedure GetListing(const aDirectory: string; var Files: TStrings;
                         FullPaths: Boolean = True);
    procedure RaiseDuplicateFile(aFilename: string);
    procedure SaveToFile(ms: TFPJSStream; Filename: string); override;
    function ExpandDirectory(const Filename: string): string; override;
    function ExpandFileName(const Filename: string): string; override;
    function ExpandExecutable(const Filename: string): string; override;
    function HandleOptionPaths(C: Char; aValue: string; FromCmdLine: Boolean): string; override;
    Function AddForeignUnitPath(const aValue: string; FromCmdLine: Boolean): string; override;
    function TryCreateRelativePath(const Filename, BaseDirectory: string; UsePointDirectory: Boolean; out RelPath: string): Boolean; override;
  Protected
    property DirectoryCache: TPas2jsCachedDirectories read FDirectoryCache;
  public
    property BaseDirectory: string read FBaseDirectory write SetBaseDirectory; // includes trailing pathdelim
    property ForeignUnitPaths: TStringList read FForeignUnitPaths;
    property ForeignUnitPathsFromCmdLine: integer read FForeignUnitPathsFromCmdLine;
    property IncludePaths: TStringList read FIncludePaths;
    property IncludePathsFromCmdLine: integer read FIncludePathsFromCmdLine;
    property Log: TPas2jsLogger read FLog;
    property ResetStamp: TChangeStamp read FResetStamp;
    property UnitPaths: TStringList read FUnitPaths;
    property UnitPathsFromCmdLine: integer read FUnitPathsFromCmdLine;
    property OnReadDirectory: TReadDirectoryEvent read GetOnReadDirectory write SetOnReadDirectory;
    property OnReadFile: TPas2jsReadFileEvent read FOnReadFile write FOnReadFile;
    property OnWriteFile: TPas2jsWriteFileEvent read FOnWriteFile write FOnWriteFile;
  end;

{$IFDEF Pas2js}
function PtrStrToStr(StrAsPtr: Pointer): string;
function PtrFilenameToKeyName(FilenameAsPtr: Pointer): string;
function Pas2jsCachedFileToKeyName(Item: Pointer): string;
function Pas2jsCacheDirToKeyName(Item: Pointer): string;
{$ELSE}
function CompareFilenameWithCachedFile(Filename, CachedFile: Pointer): integer;
function CompareCachedFiles(File1, File2: Pointer): integer;
function ComparePas2jsCacheDirectories(Dir1, Dir2: Pointer): integer;
function CompareAnsiStringWithDirectoryCache(Path, DirCache: Pointer): integer;
{$ENDIF}
function ComparePas2jsDirectoryEntries(Entry1, Entry2: {$IFDEF Pas2js}jsvalue{$ELSE}Pointer{$ENDIF}): integer;
function CompareFirstCaseInsThenSensitive(const s, h: string): integer;

{$IFDEF FPC_HAS_CPSTRING}
function ConvertTextToUTF16(const Src: RawByteString; var SrcEncoding: string): string;
// UTF-8 helper functions
function ConvertTextToUTF8(const Src: RawByteString; var SrcEncoding: string): UTF8String;
function GuessEncoding(const Src: RawByteString): string;
function HasUTF8BOM(const s: RawByteString): Boolean;
function RemoveUTFBOM(const s: RawByteString): RawByteString;
{$ENDIF}

implementation

uses
  StrUtils, FPCTypes;

{$IFDEF pas2js}
function PtrStrToStr(StrAsPtr: Pointer): string;
var
  S: string absolute StrAsPtr;
begin
  Result := S;
end;

function PtrFilenameToKeyName(FilenameAsPtr: Pointer): string;
var
  Filename: string absolute FilenameAsPtr;
begin
  Result := FilenameToKey(Filename);
end;

function Pas2jsCachedFileToKeyName(Item: Pointer): string;
var
  aFile: TPas2jsCachedFile absolute Item;
begin
  Result := FilenameToKey(aFile.Filename);
end;

function Pas2jsCacheDirToKeyName(Item: Pointer): string;
var
  Dir: TPas2jsCachedDirectory absolute Item;
begin
  Result := FilenameToKey(Dir.Path);
end;

{$ELSE}
function CompareFilenameWithCachedFile(Filename, CachedFile: Pointer): integer;
var
  Cache: TPas2jsCachedFile absolute CachedFile;
begin
  Result := CompareFilenames(string(Filename), Cache.Filename);
end;

function CompareCachedFiles(File1, File2: Pointer): integer;
var
  Cache1: TPas2jsCachedFile absolute File1;
  Cache2: TPas2jsCachedFile absolute File2;
begin
  Result := CompareFilenames(Cache1.Filename,Cache2.Filename);
end;

function ComparePas2jsCacheDirectories(Dir1, Dir2: Pointer): integer;
var
  Directory1: TPas2jsCachedDirectory absolute Dir1;
  Directory2: TPas2jsCachedDirectory absolute Dir2;
begin
  Result := CompareFilenames(Directory1.Path,Directory2.Path);
end;

function CompareAnsiStringWithDirectoryCache(Path, DirCache: Pointer): integer;
var
  Directory: TPas2jsCachedDirectory absolute DirCache;
begin
  Result := CompareFilenames(string(Path), Directory.Path);
end;

{$ENDIF}

function ComparePas2jsDirectoryEntries(Entry1, Entry2: {$IFDEF Pas2js}jsvalue{$ELSE}Pointer{$ENDIF}): integer;
var
  E1: TPas2jsCachedDirectoryEntry absolute Entry1;
  E2: TPas2jsCachedDirectoryEntry absolute Entry2;
begin
  Result := CompareFirstCaseInsThenSensitive(E1.Name,E2.Name);
end;

function CompareFirstCaseInsThenSensitive(const s, h: string): integer;
begin
  Result := CompareText(s,h);
  if Result<>0 then Exit;
  Result := CompareStr(s,h);
end;

{$IFDEF FPC_HAS_CPSTRING}
function ConvertTextToUTF8(const Src: RawByteString; var SrcEncoding: string): UTF8String;
var
  p: PAnsiChar;
  NormSrcEncoding: string;
begin
  Result := Src;
  if SrcEncoding='' then
    SrcEncoding := GuessEncoding(Src);
  if Result='' then Exit;
  NormSrcEncoding := NormalizeEncoding(SrcEncoding);
  if NormSrcEncoding=NormalizeEncoding(EncodingUTF8) then begin
    p := PAnsiChar(Result);
    if (p^=#$EF) and (p[1]=#$BB) and (p[2]=#$BF) then
      // cut out UTF-8 BOM
      Delete(Result,1,3);
  end else if (NormSrcEncoding = EncodingSystem)
    or (NormSrcEncoding = GetDefaultTextEncoding)
  then
    Result := SystemCPToUTF8(Result)
  else
    EPas2jsFileCache.Create('invalid encoding "'+SrcEncoding+'"');
end;

function ConvertTextToUTF16(const Src: RawByteString; var SrcEncoding: string): string;
var
  NormSrcEncoding: string;
begin
  Result := '';
  if SrcEncoding = '' then
    SrcEncoding := GuessEncoding(Src);
  if Src = '' then
    Exit;
  NormSrcEncoding := NormalizeEncoding(SrcEncoding);
  if NormSrcEncoding = NormalizeEncoding(EncodingUTF8) then begin
    Result := UTF8ToString(Src);
    if (Length(Result) > 1)
      and (Result[1] = #$FEFF)
    then
      Delete(Result, 1, 1);
  end else if (NormSrcEncoding = EncodingSystem)
    or (NormSrcEncoding = GetDefaultTextEncoding)
  then
    Result := SystemCPToUTF16(Src)
  else
    EPas2jsFileCache.Create('invalid encoding "'+SrcEncoding+'"');
end;

function GuessEncoding(const Src: RawByteString): string;
var
  p: PAnsiChar;
  l: SizeInt;
  i: Integer;
begin
  if Src='' then Exit(EncodingUTF8);

  if HasUTF8BOM(Src) then
    // UTF-8 BOM
    Exit(EncodingUTF8);

  // try UTF-8 (this includes ASCII)
  l := Length(Src);
  p := PAnsiChar(Src);
  repeat
    if ord(p^)<128 then
    begin
      // ASCII
      if (p^=#0) and (p-PAnsiChar(Src)>=l) then
        Exit(EncodingUTF8);
      inc(p);
    end else begin
      i := UTF8CharacterStrictLength(p);
      if i=0 then
        break;
      inc(p,i);
    end;
  until False;

  // check binary
  p := PAnsiChar(Src);
  repeat
    case p^ of
    #0:
      if (p-PAnsiChar(Src)>=l) then
        break
      else
        Exit(EncodingBinary);
    #1..#8,#11,#14..#31:
      Exit(EncodingBinary);
    end;
    inc(p);
  until False;

  // use system encoding
  Result := GetDefaultTextEncoding;
end;

function HasUTF8BOM(const s: RawByteString): Boolean;
var
  p: PAnsiChar;
begin
  if s='' then Exit(False);
  p := PAnsiChar(s);
  Result := (p^=#$EF) and (p[1]=#$BB) and (p[2]=#$BF);
end;

function RemoveUTFBOM(const s: RawByteString): RawByteString;
begin
  Result := s;
  if not HasUTF8BOM(Result) then Exit;
  Delete(Result,1,3);
end;
{$ENDIF}


{ TPas2jsCachedDirectory }

// inline
function TPas2jsCachedDirectory.IndexOfFile(const ShortFilename: string
  ): integer;
begin
  {$IFDEF CaseInsensitiveFilenames}
  Result := IndexOfFileCaseInsensitive(ShortFilename);
  {$ELSE}
  Result := IndexOfFileCaseSensitive(ShortFilename);
  {$ENDIF}
end;

// inline
function TPas2jsCachedDirectory.GetEntries(Index: integer
  ): TPas2jsCachedDirectoryEntry;
begin
  Result := TPas2jsCachedDirectoryEntry(FEntries[Index]);
end;

// inline
function TPas2jsCachedDirectory.NeedsUpdate: Boolean;
begin
  Result := (Pool.ChangeStamp<>FChangeStamp) or (FChangeStamp=InvalidChangeStamp);
end;

procedure TPas2jsCachedDirectory.SetSorted(const AValue: Boolean);
begin
  if FSorted=AValue then Exit;
  FSorted := AValue;
  if not FSorted then Exit;
  FEntries.Sort(@ComparePas2jsDirectoryEntries); // sort descending
end;

procedure TPas2jsCachedDirectory.DoReadDir;
var
  Info: TSearchRec;
begin
  if Assigned(Pool.OnReadDirectory) then
    if Pool.OnReadDirectory(Self) then Exit;

  // Note: do not add a 'if not DirectoryExists then Exit'.
  // This will not work on automounted directories. You must use FindFirst.
  if FindFirst(Path+AllFilesMask, faAnyFile, Info)=0 then
    repeat
      // check if special file
      if (Info.Name='.') or (Info.Name='..') or (Info.Name='')
      then
        continue;
      // add file
      Add(Info.Name, Info.TimeStamp, Info.Attr, Info.Size);
    until FindNext(Info)<>0;
  FindClose(Info);
end;

constructor TPas2jsCachedDirectory.Create(aPath: string;
  aPool: TPas2jsCachedDirectories);
begin
  FRefCount := 1;
  FPath := IncludeTrailingPathDelimiter(aPath);
  FEntries := TFPList.Create;
  FPool := aPool;
  FChangeStamp := InvalidChangeStamp;
end;

destructor TPas2jsCachedDirectory.Destroy;
begin
  Clear;
  FreeAndNil(FEntries);
  inherited Destroy;
end;

function TPas2jsCachedDirectory.Count: integer;
begin
  Result := FEntries.Count;
end;

procedure TPas2jsCachedDirectory.Clear;
var
  i: Integer;
begin
  for i := 0 to FEntries.Count-1 do
    TObject(FEntries[i]).{$IFDEF Pas2js}Destroy{$ELSE}Free{$ENDIF};
  FEntries.Clear;
  FSorted := True;
end;

procedure TPas2jsCachedDirectory.Update;
begin
  if not NeedsUpdate then Exit;
  Clear;
  DoReadDir;
  FChangeStamp := Pool.ChangeStamp;
  Sorted := True;
  {$IFDEF VerbosePas2JSDirCache}
  writeln('TPas2jsCachedDirectories.Update "',Path,'" Count=',Count);
  CheckConsistency;
  {$ENDIF}
end;

procedure TPas2jsCachedDirectory.Reference;
begin
  inc(FRefCount);
end;

procedure TPas2jsCachedDirectory.Release;
begin
  if FRefCount<1 then
    raise Exception.Create('TPas2jsCachedDirectory.Release [20180126090800] "'+Path+'"');
  dec(FRefCount);
  if FRefCount=0 then Free;
end;

function TPas2jsCachedDirectory.Add(const Name: string;
Time: TPas2jsFileAgeTime; Attr: TPas2jsFileAttr; Size: TPas2jsFileSize
  ): TPas2jsCachedDirectoryEntry;
begin
  Result := TPas2jsCachedDirectoryEntry.Create;
  Result.Name := Name;
  Result.Time := Time;
  Result.Attr := Attr;
  Result.Size := Size;
  FEntries.Add(Result);
  FSorted := False;
end;

function TPas2jsCachedDirectory.FindFile(const ShortFilename: string;
  const FileCase: TPas2jsSearchFileCase): string;
var
  i: Integer;
begin
  case FileCase of
    sfcCaseSensitive: i := IndexOfFileCaseSensitive(ShortFilename);
    sfcCaseInsensitive: i := IndexOfFileCaseInsensitive(ShortFilename);
  else
    i := IndexOfFile(ShortFilename);
  end;
  if i>=0 then
    Result := Entries[i].Name
  else
    Result := '';
end;

function TPas2jsCachedDirectory.FileAge(const ShortFilename: string
  ): TPas2jsFileAgeTime;
var
  i: Integer;
begin
  i := IndexOfFile(ShortFilename);
  if i>=0 then
    Result := Entries[i].Time
  else
    Result := -1;
end;

function TPas2jsCachedDirectory.FileAttr(const ShortFilename: string
  ): TPas2jsFileAttr;
var
  i: Integer;
begin
  i := IndexOfFile(ShortFilename);
  if i>=0 then
    Result := Entries[i].Attr
  else
    Result := 0;
end;

function TPas2jsCachedDirectory.FileSize(const ShortFilename: string
  ): TPas2jsFileSize;
var
  i: Integer;
begin
  i := IndexOfFile(ShortFilename);
  if i>=0 then
    Result := Entries[i].Size
  else
    Result := -1;
end;

function TPas2jsCachedDirectory.IndexOfFileCaseInsensitive(
  const ShortFilename: string): integer;
var
  l, r, cmp, m: Integer;
  Entry: TPas2jsCachedDirectoryEntry;
begin
  Sorted := True;
  l := 0;
  r := Count-1;
  while l<=r do begin
    m := (l+r) shr 1;
    Entry := Entries[m];
    cmp := CompareText(Entry.Name,ShortFilename);
    if cmp>0 then
      r := m-1
    else if cmp<0 then
      l := m+1
    else
      Exit(m);
  end;
  Result := -1;
end;

function TPas2jsCachedDirectory.IndexOfFileCaseSensitive(
  const ShortFilename: string): integer;
var
  l, r, cmp, m: Integer;
  Entry: TPas2jsCachedDirectoryEntry;
begin
  Sorted := True;
  l := 0;
  r := Count-1;
  while l<=r do begin
    m := (l+r) shr 1;
    Entry := Entries[m];
    cmp := CompareFirstCaseInsThenSensitive(Entry.Name,ShortFilename);
    if cmp>0 then
      r := m-1
    else if cmp<0 then
      l := m+1
    else
      Exit(m);
  end;
  Result := -1;
end;

function TPas2jsCachedDirectory.CountSameNamesCaseInsensitive(Index: integer
  ): integer;
var
  i: Integer;
  Filename: string;
begin
  Filename := Entries[Index].Name;
  Result := 1;
  for i := Index-1 downto 0 do
  begin
    if CompareText(Entries[i].Name,Filename)<>0 then break;
    inc(Result);
  end;
  for i := Index+1 to Count-1 do
  begin
    if CompareText(Entries[i].Name,Filename)<>0 then break;
    inc(Result);
  end;
end;

procedure TPas2jsCachedDirectory.GetSameNamesCaseInsensitive(Index: integer;
  List: TStrings);
var
  i: Integer;
  Filename: string;
begin
  Filename := Entries[Index].Name;
  List.Add(Filename);
  for i := Index-1 downto 0 do
  begin
    if CompareText(Entries[i].Name,Filename)<>0 then break;
    List.Add(Entries[i].Name);
  end;
  for i := Index+1 to Count-1 do
  begin
    if CompareText(Entries[i].Name,Filename)<>0 then break;
    List.Add(Entries[i].Name);
  end;
end;

procedure TPas2jsCachedDirectory.GetFiles(var Files: TStrings;
  IncludeDirs: Boolean);
var
  i: Integer;
  Entry: TPas2jsCachedDirectoryEntry;
begin
  if Files=nil then
    Files := TStringList.Create;
  if (Self=nil) or (Path='') then Exit;
  Update;
  for i := 0 to Count-1 do begin
    Entry := Entries[i];
    if IncludeDirs or ((Entry.Attr and faDirectory)=0) then
      Files.Add(Entry.Name);
  end;
end;

procedure TPas2jsCachedDirectory.CheckConsistency;
{AllowWriteln}

  procedure E(Msg: string);
  begin
    WriteDebugReport;
    writeln('TPas2jsCachedDirectory.CheckConsistency Failed for "',Path,'": '+Msg);
  end;

var
  i, cmp, j: Integer;
  Entry, LastEntry: TPas2jsCachedDirectoryEntry;
begin
  if Path<>IncludeTrailingPathDelimiter(Path) then
    E('Path<>IncludeTrailingPathDelimiter(Path)');
  LastEntry := nil;
  for i := 0 to Count-1 do begin
    Entry := Entries[i];
    if (Entry.Name='') or (Entry.Name='.') or (Entry.Name='..') then
      E('invalid entry "'+Entry.Name+'"');
    if (Entry.Size<0) then
      E('invalid size "'+Entry.Name+'" '+IntToStr(Entry.Size));
    if Sorted then
    begin
      if (LastEntry<>nil) then
      begin
        if LastEntry.Name=Entry.Name then
          E('duplicate "'+Entry.Name+'"');
        cmp := CompareText(LastEntry.Name,Entry.Name);
        if cmp>0 then
          E('sorted wrong case insensitive "'+LastEntry.Name+'" "'+Entry.Name+'"');
        if (cmp=0) and (CompareStr(LastEntry.Name,Entry.Name)>0) then
          E('sorted wrong case sensitive "'+LastEntry.Name+'" "'+Entry.Name+'"');
      end;
      j := IndexOfFileCaseSensitive(Entry.Name);
      if i<>j then
        E('IndexOfFileCaseSensitive failed "'+Entry.Name+'" expected '+IntToStr(i)+', but was '+IntToStr(j));
    end;
    LastEntry := Entry;
  end;
  {AllowWriteln-}
end;

procedure TPas2jsCachedDirectory.WriteDebugReport;
var
  i: Integer;
  Entry: TPas2jsCachedDirectoryEntry;
begin
  {AllowWriteln}
  writeln('TPas2jsCachedDirectory.WriteDebugReport Count=',Count,' Path="',Path,'"');
  for i := 0 to Count-1 do begin
    Entry := Entries[i];
    writeln(i,' "',Entry.Name,'" Size=',Entry.Size,' Time=',DateTimeToStr(Entry.Time),' Dir=',faDirectory and Entry.Attr>0);
  end;
  {AllowWriteln-}
end;

{ TPas2jsCachedDirectories }

function TPas2jsCachedDirectories.GetFileInfo(var Info: TFileInfo): Boolean;
begin
  Info.Filename := ChompPathDelim(ResolveDots(Info.Filename));
  if Info.Filename='' then Exit(False);
  if not FilenameIsAbsolute(Info.Filename) then
    Info.Filename := WorkingDirectory+Info.Filename;
  Info.ShortFilename := ExtractFilename(Info.Filename);
  Info.DirPath := ExtractFilePath(Info.Filename);
  if (Info.ShortFilename<>'') and (Info.ShortFilename<>'.') and (Info.ShortFilename<>'..')
  then
  begin
    Info.Dir := GetDirectory(Info.DirPath,True,False);
  end else begin
    Info.Dir := nil;
  end;
  Result := True;
end;

procedure TPas2jsCachedDirectories.SetWorkingDirectory(const AValue: string);
begin
  FWorkingDirectory := IncludeTrailingPathDelimiter(ResolveDots(AValue));
end;

constructor TPas2jsCachedDirectories.Create;
begin
  IncreaseChangeStamp(FChangeStamp);
  FDirectories := TPasAnalyzerKeySet.Create(
    {$IFDEF pas2js}
    @Pas2jsCacheDirToKeyName,@PtrFilenameToKeyName
    {$ELSE}
    @ComparePas2jsCacheDirectories,@CompareAnsiStringWithDirectoryCache
    {$ENDIF});
end;

destructor TPas2jsCachedDirectories.Destroy;
begin
  Clear;
  FreeAndNil(FDirectories);
  inherited Destroy;
end;

procedure TPas2jsCachedDirectories.Invalidate;
begin
  IncreaseChangeStamp(FChangeStamp);
end;

procedure TPas2jsCachedDirectories.Clear;
var
  Dir: TPas2jsCachedDirectory;
  List: TFPList;
  i: Integer;
begin
  List := FDirectories.GetList;
  try
    for i := 0 to List.Count-1 do
    begin
      Dir := TPas2jsCachedDirectory(List[i]);
      if Dir.FRefCount<>1 then
        raise Exception.Create('TPas2jsCachedDirectories.Clear [20180126090807] "'+Dir.Path+'" '+IntToStr(Dir.FRefCount));
      Dir.Release;
    end;
  finally
    List.Free;
  end;
  FDirectories.Clear;
end;

function TPas2jsCachedDirectories.DirectoryExists(Filename: string): Boolean;
var
  Info: TFileInfo;
begin
  Info.Filename := Filename;
  if not GetFileInfo(Info) then Exit(False);
  if Info.Dir<>nil then
    Result := (Info.Dir.FileAttr(Info.ShortFilename) and faDirectory)>0
  else
    Result := {$IFDEF pas2js}NodeJSFS{$ELSE}SysUtils{$ENDIF}.DirectoryExists(Info.Filename);
end;

function TPas2jsCachedDirectories.FileExists(Filename: string): Boolean;
var
  Info: TFileInfo;
begin
  Info.Filename := Filename;
  if not GetFileInfo(Info) then Exit(False);
  if Info.Dir<>nil then
    Result := Info.Dir.IndexOfFile(Info.ShortFilename)>=0
  else
    Result := {$IFDEF pas2js}NodeJSFS{$ELSE}SysUtils{$ENDIF}.FileExists(Info.Filename);
end;

function TPas2jsCachedDirectories.FileExistsI(var Filename: string): integer;
var
  Info: TFileInfo;
  i: Integer;
begin
  Result := 0;
  Info.Filename := Filename;
  if not GetFileInfo(Info) then Exit;
  if Info.Dir=nil then
  begin
    if {$IFDEF pas2js}NodeJSFS{$ELSE}SysUtils{$ENDIF}.FileExists(Info.Filename) then
      Result := 1;
  end
  else
  begin
    i := Info.Dir.IndexOfFileCaseInsensitive(Info.ShortFilename);
    if i<0 then
      Exit;
    Filename := Info.Dir.Path+Info.Dir[i].Name;
    Result := Info.Dir.CountSameNamesCaseInsensitive(i);
  end;
end;

function TPas2jsCachedDirectories.FileAge(Filename: string): TPas2jsFileAgeTime;
var
  Info: TFileInfo;
begin
  Info.Filename := Filename;
  if GetFileInfo(Info) and (Info.Dir<>nil) then
    Result := Info.Dir.FileAge(Info.ShortFilename)
  else
    Result := -1;
end;

function TPas2jsCachedDirectories.FileAttr(Filename: string): TPas2jsFileAttr;
var
  Info: TFileInfo;
begin
  Info.Filename := Filename;
  if GetFileInfo(Info) and (Info.Dir<>nil) then
    Result := Info.Dir.FileAttr(Info.ShortFilename)
  else
    Result := 0;
end;

function TPas2jsCachedDirectories.FileSize(Filename: string): TPas2jsFileSize;
var
  Info: TFileInfo;
begin
  Info.Filename := Filename;
  if GetFileInfo(Info) and (Info.Dir<>nil) then
    Result := Info.Dir.FileSize(Info.ShortFilename)
  else
    Result := -1;
end;

function TPas2jsCachedDirectories.FindDiskFilename(const Filename: string;
  SearchCaseInsensitive: Boolean): string;
var
  ADirectory: string;
  Cache: TPas2jsCachedDirectory;
  DiskShortFilename: string;
begin
  Result := ChompPathDelim(ResolveDots(Filename));
  if Result='' then Exit;
  //debugln(['TPas2jsCachedDirectories.FindDiskFilename Filename=',Result]);
  {$IF defined(NotLiteralFilenames) or defined(CaseInsensitiveFilenames)}
  {$ELSE}
  if (not SearchCaseInsensitive) then Exit;
  {$ENDIF}
  ADirectory := ExtractFilePath(Result);
  if ADirectory=Result then
    Exit; // root directory, e.g. / under Linux or C: under Windows
  if SearchCaseInsensitive then
    // search recursively all directory parts
    ADirectory := IncludeTrailingPathDelimiter(FindDiskFilename(ADirectory,True));
  Cache := GetDirectory(ADirectory,True,False);
  //debugln(['TPas2jsCachedDirectories.FindDiskFilename Dir=',Cache.Directory]);
  Result := ExtractFileName(Result);
  DiskShortFilename := Cache.FindFile(Result,sfcCaseInsensitive);
  //debugln(['TPas2jsCachedDirectories.FindDiskFilename DiskShortFilename=',DiskShortFilename]);
  if DiskShortFilename<>'' then Result := DiskShortFilename;
  Result := Cache.Path+Result;
end;

procedure TPas2jsCachedDirectories.GetListing(const aDirectory: string;
  var Files: TStrings; IncludeDirs: Boolean);
begin
  GetDirectory(aDirectory,True,False).GetFiles(Files,IncludeDirs);
end;

function TPas2jsCachedDirectories.GetDirectory(const Directory: string;
  CreateIfNotExists: Boolean; DoReference: Boolean): TPas2jsCachedDirectory;
var
  Dir: string;
begin
  Dir := ResolveDots(Directory);
  if not FilenameIsAbsolute(Dir) then
    Dir := WorkingDirectory+Dir;
  Dir := IncludeTrailingPathDelimiter(Dir);
  Result := TPas2jsCachedDirectory(FDirectories.FindKey(Pointer(Dir)));
  if Result<>nil then
  begin
    if DoReference then
      Result.Reference;
    Result.Update;
  end else if DoReference or CreateIfNotExists then
  begin
    {$IFDEF VerbosePas2JSDirCache}
    writeln('TPas2jsCachedDirectories.GetDirectory "',Dir,'"');
    {$ENDIF}
    Result := TPas2jsCachedDirectory.Create(Dir,Self);
    FDirectories.Add(Result);
    if DoReference then
      Result.Reference;
    Result.Update;
  end else
    Result := nil;
end;

{ TPas2jsFileLineReader }

procedure TPas2jsFileLineReader.IncLineNumber;
begin
  if (CachedFile<>nil) and (CachedFile.Cache<>nil) then
    CachedFile.Cache.IncReadLineCounter;
  inherited IncLineNumber;
end;

constructor TPas2jsFileLineReader.Create(const AFilename: string);
begin
  raise Exception.Create('TPas2jsFileLineReader.Create [20180126090825] no cache "'+AFilename+'"');
end;

constructor TPas2jsFileLineReader.Create(aFile: TPas2jsCachedFile);
begin
  inherited Create(aFile.Filename,aFile.Source);
  FCachedFile := aFile;
end;


{ TPas2jsCachedFile }

// inline
function TPas2jsCachedFile.GetIsBinary: Boolean;
begin
  Result := FFileEncoding=EncodingBinary;
end;

function TPas2jsCachedFile.GetCache: TPas2jsFilesCache;
begin
  Result := TPas2jsFilesCache(FS);
end;

constructor TPas2jsCachedFile.Create(aCache: TPas2jsFilesCache;
  const aFilename: string);
begin
  inHerited Create(aCache,aFileName);
  FChangeStamp := InvalidChangeStamp;
  FCacheStamp := Cache.ResetStamp;
end;

function TPas2jsCachedFile.Load(RaiseOnError: Boolean; Binary: Boolean): Boolean;

  procedure Err(const ErrorMsg: string);
  begin
    {$IFDEF VerboseFileCache}
    writeln('TPas2jsCachedFile.Load.Err ErrorMsg="',ErrorMsg,'"');
    {$ENDIF}
    FLastErrorMsg := ErrorMsg;
    if RaiseOnError then
      raise EPas2jsFileCache.Create(FLastErrorMsg);
  end;

var
  buffer: RawByteString;
  b: Boolean;
begin
  {$IFDEF VerboseFileCache}
  writeln('TPas2jsCachedFile.Load START "',Filename,'" Loaded=',Loaded);
  {$ENDIF}
  if Loaded then begin
    // already loaded, check if it still valid
    if Cache.ResetStamp = FCacheStamp then begin
      // nothing changed
      Result := FLastErrorMsg='';
      if not Result and RaiseOnError then
        raise EPas2jsFileCache.Create(FLastErrorMsg);
      Exit;
    end;
    {$IFDEF VerboseFileCache}
    writeln('TPas2jsCachedFile.Load CHECK FILEAGE "',Filename,'"');
    {$ENDIF}
    if LoadedFileAge=Cache.DirectoryCache.FileAge(Filename) then
      Exit(True);
  end;
  {$IFDEF VerboseFileCache}
  writeln('TPas2jsCachedFile.Load FIRST or RELOAD ',Filename,' Loaded=',Loaded);
  {$ENDIF}
  // needs (re)load
  Result := False;
  if not Cache.FileExists(Filename) then begin
    Err('File not found "'+Filename+'"');
    Exit;
  end;
  if Cache.DirectoryExists(Filename) then begin
    Err('File is a directory "'+Filename+'"');
    Exit;
  end;
  buffer := '';
  if RaiseOnError then
    b := Cache.ReadFile(Filename, buffer)
  else try
    b := Cache.ReadFile(Filename, buffer);
  except
    b := False;
  end;
  if not b then begin
    Err('Read error "'+Filename+'"');
    Exit;
  end;
  {$IFDEF VerboseFileCache}
  writeln('TPas2jsCachedFile.Load ENCODE ',Filename,' FFileEncoding=',FFileEncoding);
  {$ENDIF}
  if Binary then begin
    SetBinary(buffer);
    FFileEncoding := EncodingBinary;
  end else begin
    {$IFDEF FPC_HAS_CPSTRING}
    SetSource(ConvertTextToUTF16(buffer, FFileEncoding));
    {$ELSE}
    SetSource(buffer);
    {$ENDIF}
  end;
  FLoaded := True;
  FCacheStamp := Cache.ResetStamp;
  FLoadedFileAge := Cache.DirectoryCache.FileAge(Filename);
  {$IFDEF VerboseFileCache}
  writeln('TPas2jsCachedFile.Load END ',Filename,' FFileEncoding=',FFileEncoding);
  {$ENDIF}
end;

function TPas2jsCachedFile.CreateLineReader(RaiseOnError: Boolean
  ): TSourceLineReader;
begin
  if not Load(RaiseOnError) then
    Exit(nil);
  Result := TPas2jsFileLineReader.Create(Self);
end;

{ TPas2jsFileResolver }

function TPas2jsFileResolver.GetCache: TPas2jsFilesCache;
begin
  Result := TPas2jsFilesCache(FS);
end;

constructor TPas2jsFileResolver.Create(aCache: TPas2jsFilesCache);
begin
  inherited Create(aCache);
end;

{ TPas2jsFilesCache }

procedure TPas2jsFilesCache.RegisterMessages;
begin
  Log.RegisterMsg(mtInfo,nIncludeSearch,sIncludeSearch);
  Log.RegisterMsg(mtInfo,nUnitSearch,sUnitSearch);
  Log.RegisterMsg(mtInfo,nSearchingFileFound,sSearchingFileFound);
  Log.RegisterMsg(mtInfo,nSearchingFileNotFound,sSearchingFileNotFound);
  Log.RegisterMsg(mtFatal,nDuplicateFileFound,sDuplicateFileFound);
  Log.RegisterMsg(mtFatal,nCustomJSFileNotFound,sCustomJSFileNotFound);
end;

function TPas2jsFilesCache.GetHasPCUSupport: Boolean;
begin
  Result := False;
end;

procedure TPas2jsFilesCache.SetBaseDirectory(AValue: string);
begin
  AValue := Pas2jsFileUtils.ExpandDirectory(AValue);
  if FBaseDirectory=AValue then Exit;
  FBaseDirectory := AValue;
  DirectoryCache.WorkingDirectory := BaseDirectory;
end;

function TPas2jsFilesCache.AddSearchPaths(const Paths: string;
  Kind: TPas2jsSearchPathKind; FromCmdLine: Boolean; var List: TStringList;
  var CmdLineCount: integer): string;
// cmd line paths are added in front of the cfg paths
// cmd line paths are added in order, cfg paths are added in reverse order
// multi paths separated by semicolon are added in order
// duplicates are removed
var
  Added: Integer;

  function Add(aPath: string): Boolean;
  var
    Remove: Boolean;
    i: Integer;
  begin
    Remove := False;
    // search duplicate
    case Kind of
      spkPath: begin
        i := List.Count-1;
        while (i>=0) and (CompareFilenames(aPath,List[i])<>0) do dec(i);
      end;
      spkIdentifier: begin
        if aPath[Length(aPath)]='-' then begin
          Delete(aPath,Length(aPath),1);
          Remove := True;
        end;
        if not IsValidIdent(aPath,True,True) then begin
          AddSearchPaths := aPath;
          Exit(False);
        end;
        i := List.Count-1;
        while (i>=0) and (CompareText(aPath,List[i])<>0) do dec(i);
      end;
      else
        Exit(False);
    end;
    if Remove then begin
      // remove
      if i>=0 then begin
        List.Delete(i);
        if CmdLineCount>i then dec(CmdLineCount);
      end;
      Exit(True);
    end;
    if FromCmdLine then begin
      // from cmdline: append in order to the cmdline params, in front of cfg params
      if i>=0 then begin
        if i<=CmdLineCount then Exit(True);
        List.Delete(i);
      end;
      List.Insert(CmdLineCount,aPath);
      inc(CmdLineCount);
    end else begin
      // from cfg: append in reverse order to the cfg params, behind cmdline params
      if i>=0 then begin
        if i<=CmdLineCount+Added then Exit(True);
        List.Delete(i);
      end;
      List.Insert(CmdLineCount+Added,aPath);
      inc(Added);
    end;
    Result := True;
  end;

var
  aPath: string;
  p, i: integer;
  aPaths: TStringList;
begin
  Result := '';
  p := 1;
  Added := 0;
  aPaths := TStringList.Create;
  try
    while p<=Length(Paths) do begin
      aPath := GetNextDelimitedItem(Paths,';',p);
      if aPath='' then continue;
      if Kind=spkPath then
      begin
        aPath := ExpandDirectory(aPath);
        if aPath='' then continue;
      end;
      aPaths.Clear;
      FindMatchingFiles(aPath,1000,aPaths);
      if aPaths.Count=0 then begin
        if not Add(aPath) then Exit;
      end else begin
        for i := 0 to aPaths.Count-1 do
          if not Add(aPaths[i]) then Exit;
      end;
    end;
  finally
    aPaths.Free;
  end;
end;

procedure TPas2jsFilesCache.SetOnReadDirectory(AValue: TReadDirectoryEvent);
begin
  DirectoryCache.OnReadDirectory := AValue;
end;

function TPas2jsFilesCache.ReadFile(Filename: string; var Source: RawByteString
  ): Boolean;
{$IFDEF Pas2js}
{$ELSE}
var
  ms: TMemoryStream;
{$ENDIF}
begin
  Result := False;
  try
    if Assigned(OnReadFile) then
      Result := OnReadFile(Filename, Source);
    if Result then
      Exit;
    {$IFDEF Pas2js}
    try
      Source := NJS_FS.readFileSync(Filename,new(['encoding','utf8']));
    except
      raise EReadError.Create(string(JSExceptValue));
    end;
    Result := True;
    {$ELSE}
    ms := TMemoryStream.Create;
    try
      ms.LoadFromFile(Filename);
      SetLength(Source,ms.Size);
      ms.Position := 0;
      if Source<>'' then
        ms.Read(Source[1],Length(Source));
      Result := True;
    finally
      ms.Free;
    end;
    {$ENDIF}
  except
    on E: Exception do begin
      EPas2jsFileCache.Create('Error reading file "'+Filename+'": '+E.Message);
    end;
  end;
end;

procedure TPas2jsFilesCache.FindMatchingFiles(Mask: string; MaxCount: integer;
  Files: TStrings);

  procedure TooMany(id: TMaxPrecInt);
  begin
    raise EListError.Create('found too many files "'+Mask+'". Max='+IntToStr(MaxCount)+' ['+IntToStr(id)+']');
  end;

  procedure Find(aMask: string; p: integer);
  var
    dir: TPas2jsCachedDirectory;
    startP, i: Integer;
    curMask, filename: string;
    entry: TPas2jsCachedDirectoryEntry;
  begin
    while p<=Length(aMask) do begin
      if aMask[p] in ['*','?'] then begin
        while (p>1) and not (aMask[p-1] in AllowDirectorySeparators) do dec(p);
        dir := DirectoryCache.GetDirectory(LeftStr(aMask,p-1),True,False);
        startP := p;
        while (p<=Length(aMask)) and not (aMask[p] in AllowDirectorySeparators) do
          inc(p);
        curMask := copy(aMask,startP,p-StartP);
        for i := 0 to dir.Count-1 do begin
          entry := dir.Entries[i];
          if (entry.Name='') or (entry.Name='.') or (entry.Name='..') then continue;
          if not MatchGlobbing(curMask,entry.Name) then continue;
          filename := dir.Path+entry.Name;
          if p>Length(aMask) then
          begin
            // e.g. /path/unit*.pas
            if Files.Count>=MaxCount then
              TooMany(20180126091916);
            Files.Add(filename);
          end else begin
            // e.g. /path/sub*path/...
            Find(filename+copy(aMask,p,Length(aMask)),Length(filename)+1);
          end;
        end;
        Exit;
      end;
      inc(p);
    end;
    // mask has no placeholder -> search directly
    if FileExists(aMask) then begin
      if Files.Count>=MaxCount then
        TooMany(20180126091913);
      Files.Add(aMask);
    end;
  end;

begin
  Mask := ResolveDots(Mask);
  Find(Mask,1);
end;

constructor TPas2jsFilesCache.Create(aLog: TPas2jsLogger);
begin
  inherited Create;
  FResetStamp := InvalidChangeStamp;
  FLog := aLog;
  FIncludePaths := TStringList.Create;
  FForeignUnitPaths := TStringList.Create;
  FUnitPaths := TStringList.Create;
  FFiles := TPasAnalyzerKeySet.Create(
    {$IFDEF Pas2js}
    @Pas2jsCachedFileToKeyName,@PtrFilenameToKeyName
    {$ELSE}
    @CompareCachedFiles,@CompareFilenameWithCachedFile
    {$ENDIF});
  FDirectoryCache := TPas2jsCachedDirectories.Create;
  RegisterMessages;
end;

destructor TPas2jsFilesCache.Destroy;
begin
  FLog := nil;
  FFiles.FreeItems;
  FreeAndNil(FDirectoryCache);
  FreeAndNil(FFiles);
  FreeAndNil(FIncludePaths);
  FreeAndNil(FForeignUnitPaths);
  FreeAndNil(FUnitPaths);
  FreeAndNil(FPCUPaths);
  inherited Destroy;
end;

procedure TPas2jsFilesCache.Reset;
begin
  Inherited;
  IncreaseChangeStamp(FResetStamp);
  FDirectoryCache.Invalidate;
  // FFiles: keep data, files are checked against LoadedFileAge
  FBaseDirectory := '';
  FForeignUnitPaths.Clear;
  FForeignUnitPathsFromCmdLine := 0;
  FUnitPaths.Clear;
  FUnitPathsFromCmdLine := 0;
  FIncludePaths.Clear;
  FIncludePathsFromCmdLine := 0;
  FreeAndNil(FPCUPaths);
  // FOnReadFile: TPas2jsReadFileEvent; keep
  // FOnWriteFile: TPas2jsWriteFileEvent; keep
end;

procedure TPas2jsFilesCache.WriteFoldersAndSearchPaths;

  procedure WriteFolder(aName, Folder: string);
  begin
    if Folder='' then Exit;
    Log.LogMsgIgnoreFilter(nUsingPath,[aName,Folder]);
    if not DirectoryExists(ChompPathDelim(Folder)) then
      Log.LogMsgIgnoreFilter(nFolderNotFound,[aName,QuoteStr(Folder)]);
  end;

var
  i: Integer;
begin
  for i := 0 to ForeignUnitPaths.Count-1 do
    WriteFolder('foreign unit path',ForeignUnitPaths[i]);
  for i := 0 to UnitPaths.Count-1 do
    WriteFolder('unit path',UnitPaths[i]);
  for i := 0 to IncludePaths.Count-1 do
    WriteFolder('include path',IncludePaths[i]);
  WriteFolder('unit output path',UnitOutputPath);
  WriteFolder('main output path',MainOutputPath);
end;

procedure TPas2jsFilesCache.GetPCUDirs(aList: TStrings; const aBaseDir: string);
var
  i: Integer;
begin
  if FPCUPaths=nil then
    begin
    FPCUPaths := TStringList.Create;
    inherited GetPCUDirs(FPCUPaths, aBaseDir);
    FPCUPaths.AddStrings(UnitPaths);
    for i := 0 to FPCUPaths.Count-1 do
      FPCUPaths[i] := IncludeTrailingPathDelimiter(FPCUPaths[i]);
    DeleteDuplicateFiles(FPCUPaths);
    end;
  aList.Assign(FPCUPaths);
end;

function TPas2jsFilesCache.PCUExists(var aFileName: string): Boolean;
begin
  Result := SearchLowUpCase(aFileName);
end;

function TPas2jsFilesCache.SameFileName(const File1, File2: string): Boolean;
begin
  Result := Pas2jsFileUtils.CompareFilenames(File1,File2)=0;
end;

function TPas2jsFilesCache.File1IsNewer(const File1, File2: string): Boolean;
begin
  Result := FileAge(File1)>FileAge(File2);
end;

function TPas2jsFilesCache.AddIncludePaths(const Paths: string;
  FromCmdLine: Boolean; out ErrorMsg: string): Boolean;
begin
  ErrorMsg := AddSearchPaths(Paths,spkPath,FromCmdLine,FIncludePaths,FIncludePathsFromCmdLine);
  Result := ErrorMsg='';
end;

function TPas2jsFilesCache.AddUnitPaths(const Paths: string;
  FromCmdLine: Boolean; out ErrorMsg: string): Boolean;
begin
  ErrorMsg := AddSearchPaths(Paths,spkPath,FromCmdLine,FUnitPaths,FUnitPathsFromCmdLine);
  Result := ErrorMsg='';
end;

function TPas2jsFilesCache.AddSrcUnitPaths(const Paths: string;
  FromCmdLine: Boolean; out ErrorMsg: string): Boolean;
begin
  ErrorMsg := AddSearchPaths(Paths,spkPath,FromCmdLine,FForeignUnitPaths,FForeignUnitPathsFromCmdLine);
  Result := ErrorMsg='';
end;

function TPas2jsFilesCache.CreateResolver: TPas2jsFSResolver;

begin
  Result := TPas2jsFileResolver.Create(Self);
  {$IFDEF HasStreams}
  Result.UseStreams := False;
  {$ENDIF}
  Result.BaseDirectory := BaseDirectory; // beware: will be changed by Scanner.OpenFile
end;

function TPas2jsFilesCache.FormatPath(const aPath: string): string;
begin
  Result := aPath;
  if (Result='') or (BaseDirectory='') then Exit;
  if FilenameIsAbsolute(aPath) then
  begin
    if not ShowFullPaths then
    begin
      if BaseDirectory=LeftStr(Result,Length(BaseDirectory)) then
        Delete(Result,1,Length(BaseDirectory));
    end;
  end else begin
    if ShowFullPaths then
      Result := BaseDirectory+Result;
  end;
end;



function TPas2jsFilesCache.DirectoryExists(const Filename: string): Boolean;
begin
  Result := DirectoryCache.DirectoryExists(FileName);
end;

function TPas2jsFilesCache.FileExists(const Filename: string): Boolean;
begin
  Result := DirectoryCache.FileExists(FileName);
end;

function TPas2jsFilesCache.FileExistsI(var Filename: string): integer;
begin
  Result := DirectoryCache.FileExistsI(FileName);
end;

function TPas2jsFilesCache.FileAge(const Filename: string): TPas2jsFileAgeTime;
begin
  Result := DirectoryCache.FileAge(FileName);
end;

function TPas2jsFilesCache.FindFile(Filename: string): TPas2jsCachedFile;
begin
  Filename := NormalizeFilename(Filename,True);
  Result := TPas2jsCachedFile(FFiles.FindKey(Pointer(Filename)));
end;

function TPas2jsFilesCache.LoadFile(Filename: string; Binary: Boolean
  ): TPas2jsFile;
begin
  Result := FindFile(FileName);
  if Result=nil then
  begin
    // new file
    Result := TPas2jsCachedFile.Create(Self,Filename);
    FFiles.Add(Result);
  end;
  Result.Load(True,Binary);
end;

function TPas2jsFilesCache.NormalizeFilename(const Filename: string;
  RaiseOnError: Boolean): string;
begin
  Result := Filename;
  if ExtractFilename(Result)='' then
    if RaiseOnError then
      raise EFileNotFoundError.Create('invalid file name "'+Filename+'"');
  Result := ExpandFileNamePJ(Result,BaseDirectory);
  if (ExtractFilename(Result)='') or not FilenameIsAbsolute(Result) then
    if RaiseOnError then
      raise EFileNotFoundError.Create('invalid file name "'+Filename+'"');
end;

procedure TPas2jsFilesCache.GetListing(const aDirectory: string;
  var Files: TStrings; FullPaths: Boolean);
begin
  DirectoryCache.GetDirectory(aDirectory,True,False).GetFiles(Files,FullPaths);
end;

procedure TPas2jsFilesCache.RaiseDuplicateFile(aFilename: string);

  procedure E(const File1, File2: string);
  begin
    raise EPas2jsFileCache.Create(SafeFormat(sDuplicateFileFound,[File1,File2]));
  end;

var
  Dir: TPas2jsCachedDirectory;
  i: Integer;
  List: TStringList;
  ShortFilename: string;
begin
  Dir := DirectoryCache.GetDirectory(ExtractFilePath(aFilename),True,False);
  ShortFilename := ExtractFilename(aFilename);
  i := Dir.IndexOfFileCaseSensitive(ShortFilename);
  if i<0 then
    E(aFilename,'?');
  List := TStringList.Create;
  try
    Dir.GetSameNamesCaseInsensitive(i,List);
    if List.Count<2 then
      E(aFilename,'?');
    E(Dir.Path+List[0],List[1]);
  finally
    List.Free;
  end;
end;

procedure TPas2jsFilesCache.SaveToFile(ms: TFPJSStream; Filename: string);
var
  s: RawByteString;
  {$IFDEF FPC}
  i: Integer;
  l: TMaxPrecInt;
  {$ENDIF}
begin
  if Assigned(OnWriteFile) then begin
    {$IFDEF Pas2js}
    s := ms.join('');
    {$ELSE}
    l := ms.Size-ms.Position;
    if l>0 then begin
      s := '';
      SetLength(s, l);
      ms.Read(s[1], l);
    end else
      s := '';
    {$ENDIF}
    OnWriteFile(Filename, s);
  end else begin
    {$IFDEF Pas2js}
    try
      s := ms.join('');
      NJS_FS.writeFileSync(Filename,s,new(['encoding','utf8']));
    except
      raise EWriteError.Create(string(JSExceptValue));
    end;
    {$ELSE}
    try
      ms.SaveToFile(Filename);
    except
      on E: Exception do begin
        i := GetLastError;
        if i<>0 then
          Log.LogPlain('Note: '+SysErrorMessage(i));
        if not DirectoryExists(ChompPathDelim(ExtractFilePath(Filename))) then
          Log.LogPlain('Note: file cache inconsistency: folder does not exist "'+ChompPathDelim(ExtractFilePath(Filename))+'"');
        if FileExists(Filename) and not FileIsWritable(Filename) then
          Log.LogPlain('Note: file is not writable "'+Filename+'"');
        raise;
      end;
    end;
    {$ENDIF}
  end;
end;

function TPas2jsFilesCache.ExpandDirectory(const Filename: string): string;
begin
  if Filename='' then Exit('');
  Result := ExpandFileNamePJ(Filename,BaseDirectory);
  if Result='' then Exit;
  Result := IncludeTrailingPathDelimiter(Result);
end;

function TPas2jsFilesCache.ExpandFileName(const Filename: string): string;
begin
  Result := ExpandFileNamePJ(Filename,BaseDirectory);
end;

function TPas2jsFilesCache.ExpandExecutable(const Filename: string): string;

  function TryFile(CurFilename: string): Boolean;
  begin
    Result := False;
    CurFilename := ResolveDots(CurFilename);
    if not FileExists(CurFilename) then Exit;
    ExpandExecutable := CurFilename;
    Result := True;
  end;

var
  PathVar, CurPath: string;
  p, StartPos: Integer;
begin
  if Filename='' then Exit('');
  if ExtractFilePath(Filename)='' then
  begin
    // no file path -> search
    {$IFDEF Windows}
    // search in BaseDir
    if BaseDirectory<>'' then
    begin
      if TryFile(IncludeTrailingPathDelimiter(BaseDirectory)+Filename) then Exit;
    end;
    {$ENDIF}
    // search in PATH
    PathVar := GetEnvironmentVariablePJ('PATH');
    p := 1;
    while p<=Length(PathVar) do
    begin
      while (p<=Length(PathVar)) and (PathVar[p]=PathSeparator) do inc(p);
      StartPos := p;
      while (p<=Length(PathVar)) and (PathVar[p]<>PathSeparator) do inc(p);
      CurPath := copy(PathVar,StartPos,p-StartPos);
      if CurPath='' then continue;
      CurPath := ExpandFileNamePJ(CurPath);
      if CurPath='' then continue;
      if TryFile(IncludeTrailingPathDelimiter(CurPath)+Filename) then Exit;
    end;
  end else
    Result := ExpandFileName(Filename);
end;

function TPas2jsFilesCache.HandleOptionPaths(C: Char; aValue: string; FromCmdLine: Boolean): string;
var
  ErrorMsg: string;
begin
  Result := '';
  case C of
    'E': MainOutputPath := aValue;
    'i': if not AddIncludePaths(aValue,FromCmdLine,ErrorMsg) then
           Result := 'invalid include path (-Fi) "'+ErrorMsg+'"';
    'u': if not AddUnitPaths(aValue,FromCmdLine,ErrorMsg) then
           Result := 'invalid unit path (-Fu) "'+ErrorMsg+'"';
    'U': UnitOutputPath := aValue;
  else
    Result := inherited HandleOptionPaths(C, aValue, FromCmdLine);
  end;
end;

function TPas2jsFilesCache.AddForeignUnitPath(const aValue: string; FromCmdLine: Boolean): string;
begin
  AddSrcUnitPaths(aValue,FromCmdLine,Result);
end;

function TPas2jsFilesCache.TryCreateRelativePath(const Filename, BaseDirectory: String;
  UsePointDirectory: boolean; out RelPath: String): Boolean;
begin
  Result:=Pas2jsFileUtils.TryCreateRelativePath(Filename, BaseDirectory,
    UsePointDirectory, true, RelPath);
end;

function TPas2jsFilesCache.FindIncludeFileName(const aFilename: string): string;

  function SearchCasedInIncPath(const Filename: string): string;
  var
    i: Integer;
  begin
    // file name is relative
    // first search in the same directory as the unit
    if BaseDirectory<>'' then
      begin
      Result := BaseDirectory+Filename;
      if SearchLowUpCase(Result) then Exit;
      end;
    // then search in include path
    for i := 0 to IncludePaths.Count-1 do begin
      Result := IncludeTrailingPathDelimiter(IncludePaths[i])+Filename;
      if SearchLowUpCase(Result) then Exit;
    end;
    Result := '';
  end;

var
  Filename : string;
begin
  Result := '';

  // convert pathdelims to system
  Filename := SetDirSeparators(aFilename);
  if ShowTriedUsedFiles then
    Log.LogMsgIgnoreFilter(nIncludeSearch,[Filename]);

  if FilenameIsAbsolute(Filename) then
  begin
    Result := Filename;
    if not SearchLowUpCase(Result) then
      Result := '';
    Exit;
  end;

  // search with the given file extension (even if no ext)
  Result := SearchCasedInIncPath(Filename);
  if Result<>'' then Exit;

  if ExtractFileExt(Filename)='' then
  begin
    // search with the default file extensions
    Result := SearchCasedInIncPath(Filename+'.inc');
    if Result<>'' then Exit;
    Result := SearchCasedInIncPath(Filename+'.pp');
    if Result<>'' then Exit;
    Result := SearchCasedInIncPath(Filename+'.pas');
    if Result<>'' then Exit;
  end;
end;

function TPas2jsFilesCache.FindSourceFileName(const aFilename: string): string;

Var
  Found: Boolean;
  i: Integer;

begin
  Result := aFilename;
  if StrictFileCase or SearchLikeFPC then
    Found := FileExists(Result)
  else
    begin
    i := FileExistsI(Result);
    Found := i=1;
    if i>1 then
      RaiseDuplicateFile(Result);
    end;
  if not Found then
    raise EFileNotFoundError.Create(aFilename)
end;


function TPas2jsFilesCache.FindUnitFileName(const aUnitname, InFilename,
  ModuleDir: string; out IsForeign: boolean): String;
var
  SearchedDirs: TStringList;

  function SearchInDir(Dir: string; var Filename: string): Boolean;
  // search in Dir for pp, pas, p times given case, lower case, upper case
  begin
    Dir := IncludeTrailingPathDelimiter(Dir);
    if IndexOfFile(SearchedDirs,Dir)>=0 then Exit(False);
    SearchedDirs.Add(Dir);
    Filename := Dir+aUnitname+'.pp';
    if SearchLowUpCase(Filename) then Exit(True);
    Filename := Dir+aUnitname+'.pas';
    if SearchLowUpCase(Filename) then Exit(True);
    Filename := Dir+aUnitname+'.p';
    if SearchLowUpCase(Filename) then Exit(True);
    Result := False;
  end;

var
  i: Integer;
  aFilename: string;
begin
  Result := '';
  IsForeign := False;
  SearchedDirs := TStringList.Create;
  try
    if InFilename<>'' then
    begin
      aFilename := SetDirSeparators(InFilename);
      Result := ResolveDots(aFilename);
      if FilenameIsAbsolute(Result) then
      begin
        if SearchLowUpCase(Result) then Exit;
      end else
      begin
        Result:=ResolveDots(ModuleDir+Result);
        if SearchLowUpCase(Result) then Exit;
      end;
      Exit('');
    end;

    // first search in foreign unit paths
    IsForeign := True;
    for i := 0 to ForeignUnitPaths.Count-1 do
      if SearchInDir(ForeignUnitPaths[i],Result) then
      begin
        IsForeign := True;
        Exit;
      end;

    // then in ModuleDir
    IsForeign:=false;
    if SearchInDir(ModuleDir,Result) then exit;

    // then in BaseDirectory
    IsForeign := False;
    if SearchInDir(BaseDirectory,Result) then Exit;

    // finally search in unit paths
    for i := 0 to UnitPaths.Count-1 do
      if SearchInDir(UnitPaths[i],Result) then Exit;
  finally
    SearchedDirs.Free;
  end;

  Result := '';
end;

function TPas2jsFilesCache.FindUnitJSFileName(const aUnitFilename: string): string;

begin
  Result := '';
  if aUnitFilename='' then Exit;
    begin
    if UnitOutputPath<>'' then
      Result := UnitOutputPath+ChangeFileExt(ExtractFileName(aUnitFilename),'.js')
    else if MainOutputPath<>'' then
      Result := MainOutputPath+ChangeFileExt(ExtractFileName(aUnitFilename),'.js')
    else
      Result := ChangeFileExt(aUnitFilename,'.js');
    end;
end;

function TPas2jsFilesCache.FindCustomJSFileName(const aFilename: string): string;

Var
  FN : string;

  function SearchInDir(Dir: string): Boolean;
  var
    CurFilename: string;
  begin
    Dir := IncludeTrailingPathDelimiter(Dir);
    CurFilename := Dir+FN;
    Result := FileExistsLogged(CurFilename);
    if Result then
      FindCustomJSFileName := CurFilename;
  end;

var
  i: Integer;
begin
  Result := '';
  FN := ResolveDots(aFileName);
  if FilenameIsAbsolute(FN) then
    begin
    Result := FN;
    if not FileExistsLogged(Result) then
      Result := '';
    Exit;
    end;

  if ExtractFilePath(FN)<>'' then
    begin
    Result := ExpandFileNamePJ(FN,BaseDirectory);
    if not FileExistsLogged(Result) then
      Result := '';
    Exit;
    end;

  // first search in foreign unit paths
  for i := 0 to ForeignUnitPaths.Count-1 do
    if SearchInDir(ForeignUnitPaths[i]) then
      Exit;

  // then in BaseDirectory
  if SearchInDir(BaseDirectory) then Exit;

  // finally search in unit paths
  for i := 0 to UnitPaths.Count-1 do
    if SearchInDir(UnitPaths[i]) then Exit;

  Result := '';
end;

function TPas2jsFilesCache.FileExistsLogged(const Filename: string): Boolean;
begin
  Result := FileExists(Filename);
  if ShowTriedUsedFiles then
    if Result then
      Log.LogMsgIgnoreFilter(nSearchingFileFound,[FormatPath(Filename)])
    else
      Log.LogMsgIgnoreFilter(nSearchingFileNotFound,[FormatPath(Filename)]);
end;

function TPas2jsFilesCache.GetOnReadDirectory: TReadDirectoryEvent;
begin
  Result := DirectoryCache.OnReadDirectory;
end;

function TPas2jsFilesCache.FileExistsILogged(var Filename: string): integer;
begin
  Result := DirectoryCache.FileExistsI(Filename);
  if ShowTriedUsedFiles then
    if Result>0 then
      Log.LogMsgIgnoreFilter(nSearchingFileFound,[FormatPath(Filename)])
    else
      Log.LogMsgIgnoreFilter(nSearchingFileNotFound,[FormatPath(Filename)]);
end;

function TPas2jsFilesCache.SearchLowUpCase(var Filename: string): Boolean;
var
  i: Integer;
{$IFNDEF CaseInsensitiveFilenames}
  CasedFilename: string;
{$ENDIF}
begin
  if StrictFileCase or SearchLikeFPC then begin
    if FileExistsLogged(Filename) then
      Exit(True);
    if StrictFileCase then
      Exit(False);
    {$IFNDEF CaseInsensitiveFilenames}
    // FPC like search:
    // first as written, then lowercase, then uppercase
    CasedFilename := ExtractFilePath(Filename)+LowerCase(ExtractFileName(Filename));
    if (Filename<>CasedFilename) and FileExistsLogged(CasedFilename) then
    begin
      Filename := CasedFilename;
      Exit(True);
    end;
    CasedFilename := ExtractFilePath(Filename)+UpperCase(ExtractFileName(Filename));
    if (Filename<>CasedFilename) and FileExistsLogged(CasedFilename) then
    begin
      Filename := CasedFilename;
      Exit(True);
    end;
    {$ENDIF}
  end else
  begin
    // search case insensitive
    i := FileExistsILogged(Filename);
    if i=1 then Exit(True);
    if i>1 then
      RaiseDuplicateFile(Filename);
  end;
  Result := False;
end;

end.

