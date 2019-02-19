{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2017 by Mattias Gaertner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit NodeJS;

{$mode objfpc}
{$ModeSwitch externalclass}

interface

uses
  JS, Types;

type

  { TNJSOS }

  TNJSOS = class external name 'os'
  public const
    EOL: string; // end of line, Windows: \r\n, Posix: \r
    function arch: string; // name of architecture, e.g. 'arm', 'arm64', 'ia32', 'mips', 'mipsel', 'ppc', 'ppc64', 's390', 's390x', 'x32', and 'x64'
    function cpus: TJSObjectDynArray;
    function endianness: string; // 'BE', 'LE'
    function freemem: nativeint; // free memory in bytes
    function getPriority(pid: NativeInt = 0): NativeInt;
    function homedir: string;
    function hostname: string;
    function loadavg: TDoubleDynArray; // 1, 5 and 15 minutes
    function networkInterfaces: TJSObject;
    function platform: string; // e.g. 'aix', 'darwin', 'freebsd', 'linux', 'openbsd', 'sunos', 'win32'
    function release: string; // operating system release
    procedure setPriority(pid, priority: NativeInt);
    procedure setPriority(priority: NativeInt); // current process
    function tmpdir: string; // operating system's default directory for temporary files
    function totalmem: NativeInt;
    function ostype: string; // e.g. 'Linux', 'Darwin', 'Windows_NT'
    function uptime: NativeInt;
    function userInfo(const options: TJSObject): TJSObject;
  end;

  { TNJSBuffer }

  TNJSBuffer = class external name 'buffer'
  public
    class function alloc(Size: NativeInt): TJSArrayBuffer;
    class function alloc(Size: NativeInt; const Fill: JSValue): TJSArrayBuffer;
    class function alloc(Size: NativeInt; const Fill: JSValue; Encoding: String): TJSArrayBuffer;
    class function allocUnsafe(Size: NativeInt): TJSArrayBuffer;
    class function from(const Values: TJSValueDynArray): TJSArrayBuffer;
    class function from(s: String): TJSArrayBuffer;
    class function from(s, Encoding: String): TJSArrayBuffer;
    class function from(Buffer: TJSArrayBuffer): TJSArrayBuffer;
    class function from(Buffer: TJSArrayBuffer; Offset: NativeInt): TJSArrayBuffer;
    class function from(Buffer: TJSArrayBuffer; Offset, Count: NativeInt): TJSArrayBuffer;
  end;

  TNJSProcessConfig = TJSObject;
  TNJSProcessCPUUsage = TJSObject;

  { TJSProcess }

  TNJSProcess = class external name 'process'
  private
    class var FVersions: TJSObject external name 'versions';
    class var FVersion: String external name 'version';
    class var FRelease: TJSObject external name 'release';
    class var FPlatform: String external name 'platform';
    class var FPid: NativeInt external name 'pid';
    class var FExecPath: String external name 'execPath';
    class var FExecArgv: TStringDynArray external name 'execArgv';
    class var FEnv: TJSObject external name 'env';
    class var FConfig: TNJSProcessConfig external name 'config';
    class var FArch: string external name 'arch';
    class var FArgv: TStringDynArray external name 'argv';
    class function Getumask: NativeInt external name 'umask';
    class procedure Setumask(const AValue: NativeInt) external name 'umask';
  public
    class procedure abort;
    class property arch: string read FArch;
    class property argv: TStringDynArray read FArgv;
    // ToDo assert
    // ToDo binding
    class procedure chdir(Directory: String);
    class property config: TNJSProcessConfig read FConfig;
    class function cpuUsage: TNJSProcessCPUUsage;
    class function cpuUsage(const previousValue: TNJSProcessCPUUsage): TNJSProcessCPUUsage;
    class function cwd: String;
    class procedure emitWarning(warning: String);
    class procedure emitWarning(warning, name: String);
    //class procedure emitWarning(warning, name: String; ctor: ?);
    class property env: TJSObject read FEnv;
    class property execArgv: TStringDynArray read FExecArgv;
    class property execPath: String read FExecPath;
    class procedure exit(code: NativeInt = 0);
    class var exitCode: NativeInt;
    class function getegid: NativeInt; // only POSIX platforms
    class function geteuid: NativeInt; // only POSIX platforms
    class function getgid: JSValue; // only POSIX platforms
    class function getgroups: TJSValueDynArray; // only POSIX platforms
    class function getuid: NativeInt; // only POSIX platforms
    // ToDo hrtime([time])
    // ToDo: initgroups(user, extra_group)
    class procedure kill(pid: NativeInt);
    class procedure kill(pid: NativeInt; signal: String);
    // ToDo: mainModule
    class function memoryUsage: TJSObject; // ToDo
    // ToDo: nextTick(callback[, ...args])
    // ToDo: openStdin
    class property pid: NativeInt read FPid;
    class property platform: String read FPlatform;
    class property release: TJSObject read FRelease;
    // ToDo: send(message[, sendHandle[, options]][, callback])
    class procedure setegid(const id: JSValue);
    class procedure seteuid(const id: JSValue);
    class procedure setgid(const id: JSValue);
    class procedure setgroups(const groups: TJSValueDynArray);
    class procedure setuid(const id: JSValue);
    // ToDo: stderr
    // ToDo: stdin
    // ToDo: stdout
    class var title: String;
    class property umask: NativeInt read Getumask write Setumask;
    class function uptime: NativeInt;
    class property version: String read FVersion;
    class property versions: TJSObject read FVersions;
  end;

  { TNJSConsole }

  TNJSConsole = class external name 'Console'
  Public
    procedure assert(anAssertion : string; const Obj1 : JSValue); varargs;
    procedure dir(const Obj1 : JSValue);
    procedure dir(const Obj1 : JSValue; const options: TJSObject);
    procedure error(const Obj1 : JSValue); varargs;
    procedure info(const Obj1 : JSValue); varargs;
    procedure log(const Obj1 : JSValue); varargs;
    procedure time(const aName : string);
    procedure timeEnd(const aName : string);
    procedure trace;
    procedure warn(const Obj1 : JSValue); varargs;
  end;

function Require(ModuleName: String): JSValue; external name 'require';

var
  Console: TNJSConsole external name 'console';
  NJS_OS: TNJSOS;

implementation

initialization
  NJS_OS:=TNJSOS(Require('os'));
  LineEnding:=NJS_OS.EOL;
  sLineBreak:=NJS_OS.EOL;

end.

