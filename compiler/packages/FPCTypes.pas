(* Internal FPC types missing in Delphi.
  Copied from system.pas, systemh.inc
*)

unit FPCTypes;

{$I delphi_defines.inc}

interface

uses
  Types;

const
  FPC_FULLVERSION = 30202;
  LineEnding = #13#10;
  LFNSupport = True;
  DirectorySeparator = '\';
  DriveSeparator = ':';
  ExtensionSeparator = '.';
  PathSeparator = ';';
  AllowDirectorySeparators: set of AnsiChar = ['\','/'];
  AllowDriveSeparators: set of AnsiChar = [':'];
  maxExitCode = 65535;
  MaxPathLen = 260;
  AllFilesMask = '*';
  ECMAScript = 5;

  { some values which are used in RTL for TSystemCodePage type }
  CP_ACP     = 0;     // default to ANSI code page
  CP_OEMCP   = 1;     // default to OEM (console) code page
  CP_UTF16   = 1200;  // utf-16
  CP_UTF16BE = 1201;  // unicodeFFFE
  CP_UTF7    = 65000; // utf-7
  CP_UTF8    = 65001; // utf-8
  CP_ASCII   = 20127; // us-ascii
  CP_NONE    = $FFFF; // rawbytestring encoding

type
  { The compiler has all integer types defined internally. Here
    we define only aliases }
  DWord    = LongWord;
  QWord    = UInt64;
  UnicodeChar = WideChar;
  PUnicodeChar = PWideChar;
  TSystemCodePage = Word;

{$ifdef CPU64}
  SizeInt = Int64;
  SizeUInt = QWord;
  PtrInt = Int64;
  PtrUInt = QWord;
  ValSInt = int64;
  ValUInt = qword;
  CodePointer = Pointer;
  CodePtrInt = PtrInt;
  CodePtrUInt = PtrUInt;
  TExitCode = Longint;
{$endif CPU64}

{$ifdef CPU32}
  SizeInt = Longint;
  SizeUInt = DWord;
  PtrInt = Longint;
  PtrUInt = DWord;
  ValSInt = Longint;
  ValUInt = Cardinal;
  CodePointer = Pointer;
  CodePtrInt = PtrInt;
  CodePtrUInt = PtrUInt;
  TExitCode = Longint;
{$endif CPU32}

  //PathStr = string[FileNameLen];
  PathStr = string;
  TStringArray = array of string;

function SetDirSeparators(Const FileName : PathStr) : PathStr;
function BoolToStr(B: Boolean;UseBoolStrs:Boolean=False): string; overload;
function BoolToStr(B: boolean; const TrueS, FalseS: string): string; overload;
function HexStr(Val: Int64; Cnt: Byte): string;
function StrToQWord(const s: string): QWord; inline;
function TryStrToQWord(const s: string; Out Q : QWord) : boolean;
function TryStringToGUID(const S: string; out Guid: TGUID): Boolean;
function MethodPointersEqual(const MethodPointer1, MethodPointer2): Boolean;
procedure MyVal(const S: string; out V: Int64; out Code: Integer);

resourcestring
  SListIndexError               = 'List index (%d) out of bounds';

implementation

uses
  SysUtils;

type
  ObjpasInt = LongInt;

procedure DoDirSeparators(Var FileName : PathStr);
var
  I : longint;
begin
  For I:=1 to Length(FileName) do
    If CharInSet(FileName[I],AllowDirectorySeparators) then
      FileName[i]:=DirectorySeparator;
end;

function SetDirSeparators (Const FileName : PathStr) : PathStr;
begin
  Result:=FileName;
  DoDirSeparators (Result);
end;

procedure CheckBoolStrs;
begin
    If Length(TrueBoolStrs)=0 then
      begin
        SetLength(TrueBoolStrs,1);
        TrueBoolStrs[0]:='True';
      end;
    If Length(FalseBoolStrs)=0 then
      begin
        SetLength(FalseBoolStrs,1);
        FalseBoolStrs[0]:='False';
      end;
end;

function BoolToStr(B: Boolean;UseBoolStrs:Boolean=False): string;
begin
 if UseBoolStrs Then
  begin
    CheckBoolStrs;
    if B then
      Result:=TrueBoolStrs[0]
    else
      Result:=FalseBoolStrs[0];
  end
 else
  If B then
    Result:='-1'
  else
    Result:='0';
end;

// from textmode IDE util funcs.
function BoolToStr(B: boolean; const TrueS, FalseS: string): string;
begin
  if B then Result:=TrueS else BoolToStr:=FalseS;
end;

const
  HexTbl : array[0..15] of Char = '0123456789ABCDEF';

function HexStr(Val: Int64; Cnt: Byte): string;
var
  i: ObjpasInt;
begin
  SetLength(Result, Cnt);
  for i := Cnt downto 1 do begin
    Result[i] := HexTbl[Val and $f];
    Val := Val shr 4;
  end;
end;

function StrToQWord(const s: string): QWord;
begin
  Result := StrToUInt64(s)
end;

function TryStrToQWord(const s: string; Out Q: QWord): boolean;
var
  Error: Integer;
begin
  Val(s, Q, Error);
  Result := Error = 0
end;

function TryStringToGUID(const S: string; out Guid: TGUID): Boolean;
var
  e: Boolean;
  p: PChar;

  function rb: Byte;
  begin
    Result := 0;
    case p^ of
      '0'..'9': Result := Byte(p^) - Byte('0');
      'a'..'f': Result := Byte(p^) - Byte('a') + 10;
      'A'..'F': Result := Byte(p^) - Byte('A') + 10;
      else e := False;
    end;
    Inc(p);
  end;

  procedure nextChar(c: Char);
  begin
    if p^ <> c then
      e := False;
    Inc(p);
  end;

begin
  if Length(S)<>38 then Exit(False);
  e := True;
  p := PChar(S);
  nextChar('{');
  Guid.D1 := rb shl 28 or rb shl 24 or rb shl 20 or rb shl 16 or rb shl 12 or rb shl 8 or rb shl 4 or rb;
  nextChar('-');
  Guid.D2 := rb shl 12 or rb shl 8 or rb shl 4 or rb;
  nextChar('-');
  Guid.D3 := rb shl 12 or rb shl 8 or rb shl 4 or rb;
  nextChar('-');
  Guid.D4[0] := rb shl 4 or rb;
  Guid.D4[1] := rb shl 4 or rb;
  nextChar('-');
  Guid.D4[2] := rb shl 4 or rb;
  Guid.D4[3] := rb shl 4 or rb;
  Guid.D4[4] := rb shl 4 or rb;
  Guid.D4[5] := rb shl 4 or rb;
  Guid.D4[6] := rb shl 4 or rb;
  Guid.D4[7] := rb shl 4 or rb;
  nextChar('}');
  Result := e;
end;

function MethodPointersEqual(const MethodPointer1, MethodPointer2): Boolean;
var
  Method1: System.TMethod absolute MethodPointer1;
  Method2: System.TMethod absolute MethodPointer2;
begin
  Result := (Method1.Code=Method2.Code) and (Method1.Data=Method2.Data)
end;

// Converts a binary value in string representation ('%11111') to an integer
// For Delphi 10.4 and below. Delphi 11 supports binary constants.
procedure BinToInt(const S: string; out V: Int64; out Code: Integer);
begin
  V := 0;
  Code := Length(S);
  if Code > SizeOf(V)*8+1 then
    Exit;
  while Code > 1 do begin
    case S[Code] of
      '0', '1': V := V or Byte(S[Code] = '1') shl (Length(S)-Code);
      else Exit;
    end;
    Dec(Code);
  end;
  Dec(Code);
end;

procedure MyVal(const S: string; out V: Int64; out Code: Integer);
begin
  if (Length(S) >= 2) and (S[1] = '%') then
    BinToInt(S, V, Code)
  else
    Val(S, V, Code);
end;

end.
