(* Internal FPC types missing in Delphi.
  Copied from system.pas, systemh.inc
*)

Unit FPCTypes;

Interface

uses
  Types;

const
  FPC_FULLVERSION = 30004;
  DirectorySeparator = '\';
  AllowDirectorySeparators : set of AnsiChar = ['\','/'];
{$IFNDEF HAS_FILENAMELEN}
  FileNameLen = 255;
{$ENDIF HAS_FILENAMELEN}
  AllFilesMask = '*';
  PathSeparator = ';';
  LineEnding = #13#10;
  ECMAScript = 5;
  CP_NONE    = $FFFF; // rawbytestring encoding
  b00011111 = $1F;
  b00100000 = $20;
  b10000000 = $80;
  b11000000 = $C0;
  b11100000 = $E0;
  b11110000 = $F0;
  b11111000 = $F8;

type
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
{$ELSE}
  SizeInt = Longint;
  SizeUInt = DWord;
  PtrInt = Longint;
  PtrUInt = DWord;
  ValSInt = Longint;
  ValUInt = Cardinal;
  CodePointer = Pointer;
  CodePtrInt = PtrInt;
  CodePtrUInt = PtrUInt;
{$endif CPU32}
  QWord = UInt64;
  UnicodeChar = WideChar;
  PUnicodeChar = PWideChar;
  TSystemCodePage = Word;

  //PathStr = string[FileNameLen];
  PathStr = string;
  TStringArray = array of string;

Function SetDirSeparators(Const FileName : PathStr) : PathStr;
function BoolToStr(B: Boolean;UseBoolStrs:Boolean=False): string; overload;
function BoolToStr(B: boolean; const TrueS, FalseS: string): string; overload;
function HexStr(Val: Int64; Cnt: Byte): string;
function StrToQWord(const s: string): QWord;
function TryStrToQWord(const s: string; Out Q : QWord) : boolean;
function TryStringToGUID(const S: string; out Guid: TGUID): Boolean;
function MethodPointersEqual(const MethodPointer1, MethodPointer2): Boolean;

Implementation

uses
  SysUtils;

type
  ObjpasInt = LongInt;

Procedure DoDirSeparators(Var FileName : PathStr);
var
  I : longint;
begin
  For I:=1 to Length(FileName) do
    If CharInSet(FileName[I],AllowDirectorySeparators) then
      FileName[i]:=DirectorySeparator;
end;

Function SetDirSeparators (Const FileName : PathStr) : PathStr;
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

end.