{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2018 by Mattias Gaertner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit NodeJSFS;

{$mode objfpc}
{$ModeSwitch externalclass}

interface

uses
  JS, NodeJS, Types, SysUtils;

var
  DirectorySeparator: char = '/';
  DriveSeparator: string = '';
  ExtensionSeparator: char = '.';
  PathSeparator: char = ':';
  AllowDirectorySeparators: set of char = ['\','/'];
  AllowDriveSeparators: set of char = [];
  AllDirectorySeparators: set of char = ['\','/'];
  AllFilesMask: string = '*';
  MaxPathLen: integer = 4096;

  PathDelim: char = '/'; // FPC = DirectorySeparator
  DriveDelim: string = ''; // FPC = DriveSeparator
  PathSep: char = ':';// FPC = PathSeparator
  MAX_PATH: integer = 4096; // FPC = MaxPathLen;

const
  //faReadOnly      =  1;
  //faHidden        =  2;
  //faSystem        =  4;
  //faReserve       =  8;
  faDirectory     = 16;
  //faArchive       = 32;
  faAnyFile    = $000001FF;

function GetCurrentDir: string;
function FileExists(const Filename: string): boolean;
function DirectoryExists(const Filename: string): boolean;
function ExtractFilePath(const Filename: string): string;
function ExtractFileName(const Filename: string): string;
function ExtractFileExt(const Filename: string): string;
function SetDirSeparators(const Filename: string): string;
function ExpandFileName(const Filename: string): string;
function ExcludeTrailingPathDelimiter(const Filename: string): string;
function IncludeTrailingPathDelimiter(const Filename: string): string;
function ChangeFileExt(const Filename, NewExt: string): string;
function DeleteFile(const Filename: String): Boolean;
function RenameFile(const OldName, NewName: String): Boolean;

type
  TSearchRec = record
    Time : Longint;
    Size : nativeint;
    Attr : Longint;
    Name : String;
    ExcludeAttr : Longint;
    FindHandle : Pointer;
    //Mode : TMode;
    //FindData : TFindData;
  end;
  TUnicodeSearchRec = TSearchRec;

function FindFirst(const Path: String; Attr : Longint; out Rslt: TSearchRec): Longint;
function FindNext(var Rslt: TSearchRec): Longint;
procedure FindClose(var F: TSearchrec);

function FileDateToDateTime(Filedate : Longint): TDateTime;
function DateTimeToFileDate(DateTime: TDateTime): longint;

const
  S_IRUSR = &400; // read by owner
  S_IWUSR = &200; // write by owner
  S_IXUSR = &100; // execute/search by owner
  S_IRGRP =  &40; // read by group
  S_IWGRP =  &20; // write by group
  S_IXGRP =  &10; // execute/search by group
  S_IROTH =   &4; // read by others
  S_IWOTH =   &2; // write by others
  S_IXOTH =   &1; // execute/search by others

  F_OK: nativeint; external name 'fs.constants.F_OK'; // file visible
  R_OK: nativeint; external name 'fs.constants.R_OK'; // file readable
  W_OK: nativeint; external name 'fs.constants.W_OK'; // file writable
  X_OK: nativeint; external name 'fs.constants.X_OK'; // file executable

  COPYFILE_EXCL: nativeint; external name 'fs.constants.COPYFILE_EXCL';
  COPYFILE_FICLONE: NativeInt; external name 'fs.constants.COPYFILE_FICLONE';
  COPYFILE_FICLONE_FORCE: NativeInt; external name 'fs.constants.COPYFILE_FICLONE_FORCE';

type
  TNJSFileDesc = JSValue; // integer or nil
  TNJSFileMode = NativeInt;

  { TNJSDirEnt }

  TNJSDirEnt = class external name 'fs.Dirent'
  private
    FName: string; external name 'name';
  public
    function isBlockDevice: boolean;
    function isCharacterDevice: boolean;
    function isDirectory: boolean;
    function isFIFO: boolean;
    function isFile: boolean;
    function isSocket: boolean;
    function isSymbolicLink: boolean;
    property Name: string read FName;
  end;
  TNJSDirEntArray = array of TNJSDirEnt;

  { TNJSStats }

  TNJSStats = class external name 'fs.Stats'
  private
  public
    dev: NativeInt;
    ino: NativeInt;
    mode: TNJSFileMode;
    nlink: NativeInt;
    uid: NativeInt;
    gid: NativeInt;
    rdev: NativeInt;
    size: NativeInt;
    blksize: NativeInt;
    blocks: NativeInt;
    atimeMs: Double;
    mtimeMs: Double;
    ctimeMs: Double;
    birthtimeMs: Double;
    atime: TJSDate;
    mtime: TJSDate;
    ctime: TJSDate;
    birthtime: TJSDate;
    function isBlockDevice: boolean;
    function isCharacterDevice: boolean;
    function isDirectory: boolean;
    function isFIFO: boolean;
    function isFile: boolean;
    function isSocket: boolean;
    function isSymbolicLink: boolean;
  end;

  { TNJSStreamReadable }

  TNJSStreamReadable = class external name 'stream.Readable'
  private
    FReadableHighWaterMark: NativeInt external name 'readableHighWaterMark';
    FReadableLength: NativeInt external name 'readableLength';
  public
    function destroy(Error: TJSError): TNJSStreamReadable;
    function isPaused: boolean;
    function pause: TNJSStreamReadable;
    // pipe(destination[, options])
    function read: jsvalue; // string | buffer | nil | any
    function read(size: nativeint): jsvalue; // string | buffer | nil | any
    property ReadableHighWaterMark: NativeInt read FReadableHighWaterMark;
    property ReadableLength: NativeInt read FReadableLength;
    function resume: TNJSStreamReadable;
    function setEncoding(Encoding: string): TNJSStreamReadable;
    // unpipe([destination])
    // unshift(chunk)
    // wrap(stream)
  end;

  { TNJSReadStream }

  TNJSReadStream = class external name 'fs.ReadStream'(TNJSStreamReadable)
  private
    fBytesRead: NativeInt external name 'bytesRead';
    FPath: string external name 'path';
  public
    property BytesRead: NativeInt read fBytesRead;
    property Path: string read FPath;
  end;

  TNJSStreamWritableEndHandler = reference to procedure;

  { TNJSStreamWritable }

  TNJSStreamWritable = class external name 'stream.Writable'
  private
    FwritableHighWaterMark: nativeint external name 'writableHighWaterMark';
    FwritableLength: nativeint external name 'writableLength';
  public
    procedure cork;
    function destroy(Error: TJSError): TNJSStreamWritable;
    function _end(chunk: string): TNJSStreamWritable; external name 'end';
    function _end(chunk: string; encoding: string;
      callback: TNJSStreamWritableEndHandler = nil): TNJSStreamWritable; external name 'end';
    function setDefaultEncoding(Encoding: string): TNJSStreamWritable;
    procedure uncork;
    property writableHighWaterMark: NativeInt read FwritableHighWaterMark;
    property writableLength: nativeint read FwritableLength;
    function write(chunk: string): boolean;
    function write(chunk: string; encoding: string;
      callback: TNJSStreamWritableEndHandler): boolean;
  end;

  { TNJSWriteStream }

  TNJSWriteStream = class external name 'fs.WriteStream'(TNJSStreamWritable)
  private
    FBytesWritten: NativeInt external name 'bytesWritten';
    FPath: string external name 'path';
  public
    property BytesWritten: NativeInt read FBytesWritten;
    property Path: string read FPath;
  end;

  { TNJSAppendFileOpts }

  TNJSAppendFileOpts = record
    Encoding: string external name 'encoding'; // default nil
    Mode: TNJSFileMode external name 'mode'; // default &666
    Flag: string external name 'flag'; // default 'a'
  end;

  { TNJSReadStreamOpts }

  TNJSReadStreamOpts = record
    Flags: string external name 'flags'; // default 'r'
    Encoding: string external name 'encoding'; // default nil
    FD: TNJSFileDesc external name 'fd'; // default nil
    Mode: TNJSFileMode external name 'mode'; // default &666
    AutoClose: boolean external name 'autoclose'; // default true
    StartByte: NativeInt external name 'start';
    EndByte: NativeInt external name 'end'; // default Infinity
    HighWaterMark: NativeInt external name 'highWaterMark'; // default 64*1024
  end;

  { TNJSWriteStreamOpts }

  TNJSWriteStreamOpts = record
    Flags: string external name 'flags'; // default 'w'
    Encoding: string external name 'encoding'; // default 'utf8'
    FD: TNJSFileDesc external name 'fd'; // default nil
    Mode: TNJSFileMode external name 'mode'; // default &666
    AutoClose: boolean external name 'autoclose'; // default true
    Start: NativeInt external name 'start';
  end;

  { TNJSStatOpts }

  TNJSStatOpts = record
    bigint: boolean;
  end;

  { TNJSMkdirOpts }

  TNJSMkdirOpts = record
    recursive: boolean; // default false
    mode: nativeint; // default &777
  end;

  { TNJSReadDirOpts }

  TNJSReadDirOpts = record
    encoding: string; // default 'utf8'
    withFileTypes: boolean; // default false
  end;

  { TNJSReadFileOpts }

  TNJSReadFileOpts = record
    encoding: string; // default nil
    flag: string; // default 'r'
  end;

  { TNJSReadLinkOpts }

  TNJSReadLinkOpts = record
    encoding: string; // default 'utf8'
  end;

  TNJSWriteFileOpts = record
    encoding: string; // default 'utf8'
    mode: nativeint; // default &666
    flag: string; // default 'w'
  end;

type
  { TNJSFS - nodejs filesystem }

  TNJSFS = class external name 'fs'
  public
    procedure accessSync(Path: string; Mode: TNJSFileMode = F_OK); // throws Error if access is not granted
    procedure appendFileSync(Path: string; Data: string);
    procedure appendFileSync(Path: string; Data: string; const Options: TJSObject{TNJSAppendFileOpts});
    procedure chmodSync(Path: string; Mode: TNJSFileMode);
    procedure chownSync(Path: string; uid, gid: NativeInt);
    procedure closeSync(fd: TNJSFileDesc);
    procedure copyFileSync(Src, Dest: string;
      Flags: NativeInt = 0 // see COPYFILE_EXCL etc
      );
    function createReadStream(Path: string): TNJSWriteStream;
    function createReadStream(Path: string; const Options: TJSObject{TNJSReadStreamOpts}): TNJSReadStream;
    function createWriteStream(Path: string): TNJSWriteStream;
    function createWriteStream(Path: string; const Options: TJSObject{TNJSWriteStreamOpts}): TNJSWriteStream;
    function existsSync(Path: string): boolean;
    procedure fchmodSync(fd: TNJSFileDesc; mode: TNJSFileMode);
    procedure fchownSync(fd: TNJSFileDesc; uid, gid: NativeInt);
    procedure fdatasyncSync(fd: TNJSFileDesc);
    procedure fstatSync(fd: TNJSFileDesc; const Options: TJSObject{TNJSStatOpts});
    procedure fsyncSync(fd: TNJSFileDesc);
    procedure ftruncateSync(fd: TNJSFileDesc; Len: nativeint = 0);
    procedure futimesSync(fd: TNJSFileDesc; atime: NativeInt; mtime: NativeInt);
    procedure lchownSync(path: string; uid, gid: NativeInt);
    procedure linkSync(ExistingPath, NewPath: string);
    procedure lstatSync(Path: string);
    procedure lstatSync(Path: string; const Options: TJSObject{TNJSStatOpts});
    procedure mkdirSync(Path: string; const Options: TJSObject{TNJSMkdirOpts});
    // mkdtempSync
    function openSync(Path: string; Flags: string; mode: TNJSFileMode): TNJSFileDesc;
    function readdirSync(Path: string): TJSArray; // TStringDynArray
    function readdirSync(Path: string; const Options: TJSObject{TNJSReadDirOpts}): TJSArray; // can be TStringDynArray or TNJSDirEntArray if withFileTypes=true
    function readFileSync(Path: string; const Options: TJSObject{TNJSReadFileOpts}): string;
    function readlinkSync(Path: string): string;
    function readlinkSync(Path: string; const Options: TJSObject{TNJSReadLinkOpts}): string;
    // readSync(fd, buffer, offset, length, position)
    // realpathSync(path[, options])
    procedure renameSync(OldPath, NewPath: string);
    procedure rmdirSync(Path: string);
    function statSync(Path: string): TNJSStats;
    function statSync(Path: string; const Options: TJSObject{TNJSStatOpts}): TNJSStats;
    procedure symlinkSync(Target, Path: string; LinkType: string = 'file');
    procedure truncateSync(Path: string; len: NativeInt = 0);
    procedure unlinkSync(Path: string);
    // unwatchFile(filename[, listener])
    // utimesSync(path, atime, mtime)
    // watch(filename[, options][, listener])
    // watchFile(filename[, options], listener)
    procedure writeFileSync(
      aFile: jsvalue; // string | buffer | URL | filedescriptor
      Data: jsvalue // string | buffer | typedarray | DataView
      );
    procedure writeFileSync(
      aFile: jsvalue; // string | buffer | URL | filedescriptor
      Data: jsvalue; // string | buffer | typedarray | DataView
      const Options: TJSObject{TNJSWriteFileOpts});
    function writeSync(fd: TNJSFileDesc;
      buffer: jsvalue; // buffer | TypedArray | DataView
      Offset, Count, Position: NativeInt): NativeInt;
    function writeSync(fd: TNJSFileDesc; Data: string; Position: NativeInt;
      Encoding: string): NativeInt;
  end;

type

  { TNJSPathParsed }

  TNJSPathParsed = class external name 'TNJSPathParsed'
  public
    dir: string;
    root: string;
    base: string;
    name: string;
    ext: string;
  end;

  { TNJSPath }

  TNJSPath = class external name 'path'
  public
    win32: TJSObject; // todo
    posix: TJSObject; // todo
  public const
    // Beware: nodejs uses "separator" and "delimiter" the other way round than FPC/Delphi
    delimiter: char; // search PATH delimiter, windows ;, posix :
    sep: char; // directory delimiter, windows \, posix: /
  public
    function basename(Path: string; Ext: string = ''): string; // remove the directory, optional ext to chomp off
    function dirname(Path: string): string; // returns directory without trailing sep
    function extname(Path: string): string; // returns from last occurence of '.', if path starts with '.' the empty string is returned
    function format(PathObject: TJSObject): string; {
      PathObjectis can contain the following string properties:
      dir, root, base, name, ext
      root is ignored if dir exists
      ext and name are ignored if base exists
      }
    function isAbsolute(Path: string): boolean; // '' returns false
    function join(Path1: string): string; varargs; // joins all passed strings with sep and normalizes, e.g. resolves '..', if the result is empty it returns '.'
    function normalize(Path: string): string; // resolves '..' and '.' folders, reduces multiple delimiters, trailing sep is preserved, empty string is returned as '.', on windows replaces / with \\
    function parse(Path: string): TNJSPathParsed;
    function relative(FromPath, ToPath: string): string; // resolve both, then create relative, if both the same returns ''
    function resolve(Path1: string): string; varargs; // resolve from right to left, prepend until an absolute path is created
    function toNamespacedPath(Path: string): string; // windows only
  end;

var
  NJS_FS: TNJSFS;
  NJS_Path: TNJSPath;

implementation

function GetCurrentDir: string;
begin
  Result:=NJS_Path.resolve('');
end;

function FileExists(const Filename: string): boolean;
begin
  Result:=NJS_FS.existsSync(Filename);
end;

function DirectoryExists(const Filename: string): boolean;
var
  stats: TNJSStats;
begin
  try
    stats:=NJS_FS.statSync(Filename);
  except
    exit(false);
  end;
  Result:=stats.isDirectory;
end;

function ExtractFilePath(const Filename: string): string;
var
  i : longint;
begin
  i := Length(FileName);
  while (i > 0) and not (FileName[i] in AllDirectorySeparators) do
    Dec(i);
  If I>0 then
    Result := Copy(FileName, 1, i)
  else
    Result:='';
end;

function ExtractFileName(const Filename: string): string;
var
  i : longint;
begin
  I := Length(FileName);
  while (I > 0) and not (FileName[i] in AllDirectorySeparators) do
    Dec(I);
  Result := Copy(FileName, I + 1, MaxInt);
end;

function ExtractFileExt(const Filename: string): string;
var
  i : longint;
  SOF : Boolean; // Dot at Start of filename ?
begin
  Result:='';
  I := Length(FileName);
  while (I > 0) and not (FileName[i] in AllDirectorySeparators) do
    begin
    if (Filename[i]=ExtensionSeparator) then
      begin
	  SOF:=(I=1) or (FileName[i-1] in AllowDirectorySeparators);
	  if (Not SOF) or FirstDotAtFileNameStartIsExtension then
	    Result:=Copy(FileName, I, MaxInt);
      exit;
	  end;
    Dec(I);
    end;
end;

function SetDirSeparators(const Filename: string): string;
var
  i: Integer;
begin
  Result:=Filename;
  For i:=1 to Length(Result) do
    If (Result[i] in AllowDirectorySeparators)
        and (Result[i]<>DirectorySeparator) then
      Result[i]:=DirectorySeparator;
end;

function ExpandFileName(const Filename: string): string;
var
  IsAbs: Boolean;
  HomeDir, Fn: String;
begin
  Fn := SetDirSeparators(Filename);
  IsAbs := NJS_Path.isAbsolute(Fn);
  if (not IsAbs) then
  begin
    if (PathDelim='/')
        and (((Length(Fn) > 1) and (Fn[1] = '~') and (Fn[2] = '/'))
           or (Fn = '~') ) then
    begin
      HomeDir := NJS_OS.homedir;
      if not NJS_Path.isAbsolute(HomeDir) then
        HomeDir := ExpandFileName(HomeDir);
      Fn := HomeDir + Copy(Fn,2,length(Fn));
      IsAbs := True;
    end;
  end;
  if IsAbs then
  begin
    Result := NJS_Path.resolve(Fn);
  end
  else
  begin
    Fn := IncludeTrailingPathDelimiter(GetCurrentDir) + Fn;
    Fn := NJS_Path.resolve(Fn);
    Result := Fn;
  end;
end;

function ExcludeTrailingPathDelimiter(const Filename: string): string;
Var
  L : Integer;
begin
  L:=Length(Filename);
  If (L>0) and (Filename[L] in AllowDirectorySeparators) then
    Result:=LeftStr(Filename,L-1)
  else
    Result:=Filename;
end;

function IncludeTrailingPathDelimiter(const Filename: string): string;
Var
  l : Integer;
begin
  Result:=Filename;
  l:=Length(Result);
  If (L=0) or not (Result[l] in AllowDirectorySeparators) then
    Result+=DirectorySeparator;
end;

function ChangeFileExt(const Filename, NewExt: string): string;
var
  i : longint;
  SOF : Boolean; // start of filename
begin
  Result:=Filename;
  i := Length(Result);
  for i:=length(Result) downto 1 do
    if Result[i] in AllDirectorySeparators then
      break
    else if (Result[i]=ExtensionSeparator) then
      begin
	  SOF:=(I=1) or (Result[i-1] in AllowDirectorySeparators);
	  if (Not SOF) or FirstDotAtFileNameStartIsExtension then
        begin
        Result := LeftStr(Result, I - 1) + NewExt;
        exit;
        end;
      end;
  Result+=NewExt;
end;

function DeleteFile(const Filename: String): Boolean;
begin
  try
    NJS_FS.unlinkSync(Filename);
  except
    exit(false);
  end;
  Result:=true;
end;

function RenameFile(const OldName, NewName: String): Boolean;
begin
  try
    NJS_FS.renameSync(OldName,NewName);
  except
    exit(false);
  end;
  Result:=true;
end;

function FindFirst(const Path: String; Attr: Longint; out Rslt: TSearchRec
  ): Longint;
var
  Mask: String;
  Entries: TStringDynArray;
  Iterator: TJSObject;
begin
  Mask:=ExtractFileName(Path);
  if Mask<>AllFilesMask then
    raise Exception.Create('FindFirst: ToDo: Mask='+Path);
  try
    Entries:=TStringDynArray(NJS_FS.readdirSync(NJS_Path.dirname(Path)));
  except
    exit(-1);
  end;
  Iterator:=TJSObject.new;
  Iterator['path']:=ExtractFilePath(Path);
  Iterator['index']:=-1;
  Iterator['entries']:=Entries;
  Iterator['attr']:=Attr;
  Rslt.FindHandle:=Iterator;
  Result:=FindNext(Rslt);
end;

function FindNext(var Rslt: TSearchRec): Longint;
var
  Iterator: TJSObject;
  Entries: TStringDynArray;
  Index: NativeInt;
  Attr: LongInt;
  Path: String;
  Stats: TNJSStats;
  Name: string;
  IsDirectory: Boolean;
begin
  Iterator:=TJSObject(Rslt.FindHandle);
  Path:=IncludeTrailingPathDelimiter(String(Iterator['path']));
  Entries:=TStringDynArray(Iterator['entries']);
  Attr:=longint(Iterator['attr']);
  Index:=NativeInt(Iterator['index']);
  //writeln('FindNext Path=',Path,' Index=',Index,'/',length(Entries),' Attr=',Attr);
  repeat
    inc(Index);
    if Index>=length(Entries) then break;
    Name:=Entries[Index];
    Rslt.Name:=Name;
    Stats:=nil;
    try
      Stats:=NJS_FS.statSync(Path+Name);
    except
    end;
    if Stats=nil then continue;
    IsDirectory:=Stats.isDirectory;
    if IsDirectory and (faDirectory and Attr=0) then continue;
    // fill in Rslt
    Rslt.Time:=Stats.mtime.time div 1000;
    Rslt.Size:=Stats.size;
    Rslt.Attr:=0;
    if IsDirectory then
      Rslt.Attr+=faDirectory;
    Iterator['index']:=Index;
    exit(0);
  until false;
  Iterator['index']:=length(Entries);
  Result:=-1;
end;

procedure FindClose(var F: TSearchrec);
begin
  F.FindHandle:=nil;
end;

function FileDateToDateTime(Filedate: Longint): TDateTime;
var
  d: TJSDate;
begin
  d:=TJSDate.new(Filedate*1000);
  Result:=JSDateToDateTime(d);
end;

function DateTimeToFileDate(DateTime: TDateTime): longint;
var
  d: TJSDate;
begin
  d:=DateTimeToJSDate(DateTime);
  Result:=d.Time div 1000;
end;

initialization
  NJS_FS:=TNJSFS(Require('fs'));
  NJS_Path:=TNJSPath(Require('path'));
  PathDelim:=NJS_Path.sep;
  PathSeparator:=NJS_Path.delimiter;
  DirectorySeparator:=NJS_Path.sep;
  PathSep:=NJS_Path.delimiter;
  case lowercase(NJS_OS.platform) of
  'win32':
    begin
    DriveSeparator:=':';
    AllowDriveSeparators:=[':'];
    MaxPathLen:=260;
    MAX_PATH:=MaxPathLen;
    end;
  end;
  AllDirectorySeparators:=AllowDirectorySeparators+AllowDriveSeparators;

end.

