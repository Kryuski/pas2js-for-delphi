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
  JS, Types, node.events;

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
  TNJSBuffer = Class external name 'Buffer' (TJSObject)
  private
    FBuffer : TJSArrayBuffer; external name 'buffer';
    FByteOffset : NativeInt;  external name 'byteOffset';
    function getbuf(Index : NativeInt): Byte; external name '[]';
    procedure setbuf(Index : NativeInt; AValue: Byte); external name '[]';
    class var FPoolSize : NativeInt; external name 'poolSize';
  public
    constructor new (size : NativeInt); overload; deprecated;
    constructor new (aString : string); overload; deprecated;
    constructor new (aString,aEncoding : string); overload; deprecated;
    constructor new (arrayBuffer : TJSArray); overload; deprecated;
    constructor new (Buffer : TNJSBuffer); overload; deprecated;
    constructor new (Buffer : TJSUint8Array); overload; deprecated;
    constructor new (arrayBuffer : TJSArrayBuffer); overload; deprecated;
    constructor new (arrayBuffer : TJSArrayBuffer; byteOffset : NativeInt); overload; deprecated;
    constructor new (arrayBuffer : TJSArrayBuffer; byteOffset,length : NativeInt); overload;deprecated;
    class function alloc(size : NativeInt) : TNJSBuffer; overload;
    class function alloc(size : NativeInt; Fill : string) : TNJSBuffer; overload;
    class function alloc(size : NativeInt; Fill : TNJSBuffer) : TNJSBuffer; overload;
    class function alloc(size : NativeInt; Fill : NativeInt) : TNJSBuffer; overload;
    class function alloc(size : NativeInt; Fill,Encoding : string) : TNJSBuffer; overload;
    class function allocUnsafe(size : NativeInt) : TNJSBuffer; overload;
    class function allocUnsafeSlow(size : NativeInt) : TNJSBuffer; overload;
    class function byteLength(aData: string) : NativeInt; overload;
    class function byteLength(aData,aEncoding: string) : NativeInt; overload;
    class function byteLength(aData: TNJSBuffer) : NativeInt; overload;
    class function byteLength(aData: TJSDataView) : NativeInt; overload;
    class function byteLength(aData: TJSArrayBuffer) : NativeInt; overload;
//    class function byteLength(aData: TJSSharedArrayBuffer) : NativeInt; overload;
    class function compare(buf1, buf2 : TNJSBuffer) : NativeInt; overload;
    class function compare(buf1, buf2 : TJSUint8Array) : NativeInt; overload;
    class function compare(buf1: TJSUint8Array; buf2 : TNJSBuffer) : NativeInt; overload;
    class function compare(buf1: TNJSBuffer; buf2 : TJSUint8Array) : NativeInt; overload;
    class function concat(buf1: array of TNJSBuffer) : TNJSBuffer; overload;
    class function concat(buf1: array of TJSUInt8Array) : TNJSBuffer; overload;
    class function concat(buf1: array of TNJSBuffer; totalLength : NativeInt) : TNJSBuffer; overload;
    class function concat(buf1: array of TJSUInt8Array; totalLength : NativeInt) : TNJSBuffer; overload;
    class function from(arr: array of byte): TNJSBuffer; overload;
    class function from(arr: array of Integer): TNJSBuffer; overload;
    class function from(arrayBuffer : TJSArrayBuffer): TNJSBuffer; overload;
    class function from(arrayBuffer : TJSArrayBuffer; aByteOffset : NativeInt): TNJSBuffer; overload;
    class function from(arrayBuffer : TJSArrayBuffer; aByteOffset,aLength : NativeInt): TNJSBuffer; overload;
    class function from(const Values: TJSValueDynArray): TNJSBuffer;
    class function from(arrayBuffer : TJSUint8Array): TNJSBuffer; overload;
    class function from(arrayBuffer : TNJSBuffer): TNJSBuffer; overload;
    class function from(aObject : TJSObject): TNJSBuffer; overload;
    class function from(aObject : TJSObject; aEncoding : String): TNJSBuffer; overload;
    class function from(aObject : TJSObject; aOffset : NativeInt): TNJSBuffer; overload;
    class function from(aObject : TJSObject; aEncoding : String;aLength : NativeInt): TNJSBuffer; overload;
    class function from(aObject : TJSObject; aOffset,aLength : NativeInt): TNJSBuffer; overload;
    class function from(aString : String): TNJSBuffer; overload;
    class function from(aString, aEncoding : String): TNJSBuffer; overload;
    class function isBuffer(aObj : TJSObject): Boolean; overload;
    class function isEncoding(aEncoding : string): Boolean; overload;
    class property poolSize : NativeInt read FPoolSize;
    function compare(target : TNJSBuffer) : NativeInt; overload;
    function compare(target : TJSUint8Array) : NativeInt; overload;
    function compare(target : TNJSBuffer; targetStart: NativeInt) : NativeInt; overload;
    function compare(target : TJSUint8Array; targetStart: NativeInt) : NativeInt; overload;
    function compare(target : TNJSBuffer; targetStart, targetEnd: NativeInt) : NativeInt; overload;
    function compare(target : TJSUint8Array; targetStart, targetEnd: NativeInt) : NativeInt; overload;
    function compare(target : TNJSBuffer; targetStart, targetEnd, sourceStart: NativeInt) : NativeInt; overload;
    function compare(target : TJSUint8Array; targetStart, targetEnd, sourceStart: NativeInt) : NativeInt; overload;
    function compare(target : TNJSBuffer; targetStart, targetEnd, sourceStart, SourceEnd: NativeInt) : NativeInt; overload;
    function compare(target : TJSUint8Array; targetStart, targetEnd, sourceStart, SourceEnd: NativeInt) : NativeInt; overload;
    function copy(target : TNJSBuffer) : NativeInt; overload;
    function copy(target : TJSUint8Array) : NativeInt; overload;
    function copy(target : TNJSBuffer; targetStart: NativeInt) : NativeInt; overload;
    function copy(target : TJSUint8Array; targetStart: NativeInt) : NativeInt; overload;
    function copy(target : TNJSBuffer; targetStart, sourceStart: NativeInt) : NativeInt; overload;
    function copy(target : TJSUint8Array; targetStart, sourceStart: NativeInt) : NativeInt; overload;
    function copy(target : TNJSBuffer; targetStart, sourceStart, sourceEnd: NativeInt) : NativeInt; overload;
    function copy(target : TJSUint8Array; targetStart, sourceStart, sourceEnd: NativeInt) : NativeInt; overload;
    function fill(value : String) : TNJSBuffer;overload;
    function fill(value : String; aOffset : integer) : TNJSBuffer;overload;
    function fill(value : String; aOffset,aEnd : integer) : TNJSBuffer;overload;
    function fill(value : String; aOffset,aEnd : integer;aEncoding : string) : TNJSBuffer;overload;
    function fill(value : TNJSBuffer) : TNJSBuffer;overload;
    function fill(value : TNJSBuffer; aOffset : integer) : TNJSBuffer;overload;
    function fill(value : TNJSBuffer; aOffset,aEnd : integer) : TNJSBuffer;overload;
    function fill(value : NativeInt) : TNJSBuffer;overload;
    function fill(value : NativeInt; aOffset : integer) : TNJSBuffer;overload;
    function fill(value : NativeInt; aOffset,aEnd : integer) : TNJSBuffer;overload;
    function fill(value : TJSUint8Array) : TNJSBuffer;overload;
    function fill(value : TJSUint8Array; aOffset : integer) : TNJSBuffer;overload;
    function fill(value : TJSUint8Array; aOffset,aEnd : integer) : TNJSBuffer;overload;
    function includes(value : string)  : Boolean;overload;
    function includes(value,encoding : string)  : Boolean;overload;
    function includes(value : integer)  : Boolean;overload;
    function includes(value  : Boolean)  : Boolean;overload;
    function includes(value : TJSUint8Array)  : Boolean;overload;
    function includes(value : string; aByteOffset:NativeInt)  : Boolean;overload;
    function includes(value : string;aByteOffset:NativeInt;encoding : string)  : Boolean;overload;
    function includes(value : integer; aByteOffset:NativeInt)  : Boolean;overload;
    function includes(value  : Boolean; aByteOffset:NativeInt)  : Boolean;overload;
    function includes(value : TJSUint8Array; aByteOffset:NativeInt)  : Boolean;overload;
    function indexOf(value : string)  : NativeInt;overload;
    function indexOf(value,encoding : string)  : NativeInt;overload;
    function indexOf(value : integer)  : NativeInt;overload;
    function indexOf(value  : NativeInt)  : NativeInt;overload;
    function indexOf(value : TJSUint8Array)  : NativeInt;overload;
    function indexOf(value : string; aByteOffset:NativeInt)  : NativeInt;overload;
    function indexOf(value : string;aByteOffset:NativeInt;encoding : string)  : NativeInt;overload;
    function indexOf(value : integer; aByteOffset:NativeInt)  : NativeInt;overload;
    function indexOf(value  : NativeInt; aByteOffset:NativeInt)  : NativeInt;overload;
    function indexOf(value : TJSUint8Array; aByteOffset:NativeInt)  : NativeInt;overload;
    function entries : TJSIterator;
    function equals(target : TNJSBuffer) : boolean; overload;
    function equals(target : TJSUint8Array) : boolean; overload;
    property buf [Index : NativeInt] : Byte Read getbuf write setbuf;
    property buffer : TJSArrayBuffer read FBuffer;
    property byteOffset : nativeint read FByteOffset;
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

  TNJSEventEmitterHandler = node.events.TNJSEventEmitterHandler;
  TNJSEventEmitterHandlerArray = node.events.TNJSEventEmitterHandlerArray;
  TNJSEventEmitter = node.events.TNJSEventEmitter;
  TNJSEvents = node.events.TNJSEventEmitter;

  TNJSTimerCallBack = reference to procedure;

  TNJSImmediate = class external name 'Immediate' (TJSObject)
    function hasRef : boolean;
    function ref : TNJSImmediate;
    function unref : TNJSImmediate;
  end;

  TNJSTimeout = class external name 'Timeout' (TJSObject)
    function hasRef : boolean;
    function ref : TNJSTimeout;
    function refresh : TNJSTimeout;
    function unref : TNJSTimeout;
  end;



function Require(ModuleName: String): JSValue; external name 'require';
Procedure clearImmediate(aImmediate : TNJSImmediate); external name 'clearImmediate';
Procedure clearInterval(aTimeout : TNJSTimeout); external name 'clearInterval';
Procedure clearTimeout(aTimeout : TNJSTimeout); external name 'clearTimeout';
function setImmediate(aCallback : TNJSTimerCallBack) : TNJSImmediate; varargs; external name 'setImmediate';
function setInterval(aCallback : TNJSTimerCallBack; aMsecDelay : Integer) : TNJStimeout; varargs; external name 'setInterval';
function setTimeout(aCallback : TNJSTimerCallBack; aMsecDelay : Integer) : TNJStimeout; varargs; external name 'setTimeout';

var
  Console: TNJSConsole external name 'console';
  global : TJSObject;
  NJS_OS: TNJSOS;

implementation

initialization
  NJS_OS:=TNJSOS(Require('os'));
  LineEnding:=NJS_OS.EOL;
  sLineBreak:=NJS_OS.EOL;

end.

