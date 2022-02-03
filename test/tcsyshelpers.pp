unit tcsyshelpers;

{$mode objfpc}{$h+}

interface

uses
  SysUtils, fpcunit, testregistry;

Type

  { TTestHelpers }

  TTestHelpers = class(TTestCase)
  private
    procedure EqualGUID(Msg: String; Expected, Actual: TGUID);
    procedure EqualGUIDSwap(Msg: String; Expected, Actual: TGUID);
    procedure GetGUID(out G: TGUID);
  Published
//  Public
    procedure TestGUIDHelperCreateUntypedData;
    procedure TestGUIDHelperCreateUntypedDataEndian;
    procedure TestGUIDHelperCreateInteger;
    procedure TestGUIDHelperCreateIntegerBytes;
    procedure TestGUIDHelperCreateNew;
    procedure TestGUIDHelperCreateString;
    procedure TestGUIDHelperCreateTBytes;
    procedure TestGUIDHelperCreateTBytesAtIndex;
    procedure TestGUIDHelperCreateWords;
    procedure TestGUIDHelperToByteArray;
    procedure TestGUIDHelperToString;
    procedure TestByteHelper;
    procedure TestCardinalHelper;
    procedure TestLongintHelper;
    procedure TestNegLongintHelper;
    procedure TestNegShortIntHelper;
    procedure TestNegSmallintHelper;
    procedure TestShortIntHelper;
    procedure TestSmallintHelper;
    procedure TestWordHelper;
    procedure TestIsNanDouble;
    procedure TestByteClearBit;
    procedure TestByteSetBit;
    procedure TestByteTestBit;
    procedure TestByteToggleBit;
    procedure TestShortIntSetBit;
    procedure TestShortIntToggleBit;
    procedure TestCardinalClearBit;
    procedure TestCardinalSetBit;
    procedure TestCardinalTestBit;
    procedure TestCardinalToggleBit;
    procedure TestLongintClearBit;
    procedure TestLongintSetBit;
    procedure TestLongintTestBit;
    procedure TestLongintToggleBit;
    procedure TestShortIntClearBit;
    procedure TestShortIntTestBit;
    procedure TestSmallIntClearBit;
    procedure TestSmallIntSetBit;
    procedure TestSmallIntTestBit;
    procedure TestSmallIntToggleBit;
    procedure TestWordClearBit;
    procedure TestWordSetBit;
    procedure TestWordTestBit;
    procedure TestWordToggleBit;
    procedure TestNativeUintSetBit;
    procedure TestNativeUIntToggleBit;
    procedure TestNativeIntHelper;
    procedure TestNativeUintHelper;
    procedure TestNativeUIntTestBit;
    procedure TestNativeIntClearBit;
    procedure TestNativeIntSetBit;
    procedure TestNativeIntToggleBit;
    procedure TestNativeIntTestBit;
  end;

Implementation

Procedure TTestHelpers.TestByteHelper;

Const
  Value               = 123;
  ValueAsString       = '123';
  ValueAsHex          = '7B';
  ValueAsHexDig       = 4;
  ValueAsHexDigString = '007B';

Var
  V : Byte;
begin
  {$i tohelper.inc}
end;

Procedure TTestHelpers.TestShortIntHelper;

Const
  Value               = 123;
  ValueAsString       = '123';
  ValueAsHex          = '7B';
  ValueAsHexDig       = 4;
  ValueAsHexDigString = '007B';

Var
  V : ShortInt;
begin
  {$i tohelper.inc}
end;

Procedure TTestHelpers.TestNegShortIntHelper;

Const
  Value               = -123;
  ValueAsString       = '-123';
  ValueAsHex          = '85';
  ValueAsHexDig       = 4;
  ValueAsHexDigString = 'FF85';

Var
  V : ShortInt;

begin
  {$i tohelper.inc}
end;

Procedure TTestHelpers.TestWordHelper;

Const
  Value               = 1024;
  ValueAsString       = '1024';
  ValueAsHex          = '0400';
  ValueAsHexDig       = 6;
  ValueAsHexDigString = '000400';

Var
  V : Word;
begin
  {$i tohelper.inc}
end;

Procedure TTestHelpers.TestSmallintHelper;

Const
  Value               = 1024;
  ValueAsString       = '1024';
  ValueAsHex          = '0400';
  ValueAsHexDig       = 6;
  ValueAsHexDigString = '000400';

Var
  V : Smallint;
begin
  {$i tohelper.inc}
end;

Procedure TTestHelpers.TestNegSmallintHelper;

Const
  Value               = -1024;
  ValueAsString       = '-1024';
  ValueAsHex          = 'FC00';
  ValueAsHexDig       = 6;
  ValueAsHexDigString = 'FFFC00';

Var
  V : Smallint;
begin
  {$i tohelper.inc}
end;

Procedure TTestHelpers.TestCardinalHelper;

Const
  Value               = 131072;
  ValueAsString       = '131072';
  ValueAsHex          = '00020000';
  ValueAsHexDig       = 10;
  ValueAsHexDigString = '0000020000';

Var
  V : Cardinal;
begin
  {$i tohelper.inc}
end;

Procedure TTestHelpers.TestLongintHelper;

Const
  Value               = 131072;
  ValueAsString       = '131072';
  ValueAsHex          = '00020000';
  ValueAsHexDig       = 10;
  ValueAsHexDigString = '0000020000';

Var
  V : Longint;

begin
  {$i tohelper.inc}
end;

Procedure TTestHelpers.TestNegLongintHelper;

Const
  Value               = -131072;
  ValueAsString       = '-131072';
  ValueAsHex          = 'FFFE0000';
  ValueAsHexDig       = 10;
  ValueAsHexDigString = '00FFFE0000';

Var
  V : Longint;
begin
  {$i tohelper.inc}
end;

Procedure TTestHelpers.TestNativeUintHelper;

Const
  Value               = 17179869184; // 2^34
  ValueAsString       = '17179869184';
  ValueAsHex          = '0000000400000000';
  ValueAsHexDig       = 18;
  ValueAsHexDigString = '000000000400000000';

Var
  V : NativeUInt;
begin
  Fail('Not implemented yet');
 // {$i tohelper.inc}
end;

Procedure TTestHelpers.TestNativeIntHelper;

Const
  Value               = 17179869184; // 2^34
  ValueAsString       = '17179869184';
  ValueAsHex          = '0000000400000000';
  ValueAsHexDig       = 18;
  ValueAsHexDigString = '000000000400000000';

Var
  V : NativeInt;
begin
  Fail('Notimplemented');
//  {$i tohelper.inc}
end;


Procedure TTestHelpers.GetGUID(out G : TGUID);

Var
  I : Integer;

begin
  G.D1:=$DDCCBBAA;
  G.D2:=$EEFF;
  G.D3:=$CAAC;
  For I:=0 to 7 do
   G.D4[i]:=(1 shl i) and $FF;
end;

Procedure TTestHelpers.EqualGUID(Msg : String;Expected,Actual : TGUID);

Var
  I : Integer;

begin
  AssertEquals(Msg+' D1 equal',Expected.D1,Actual.D1);
  AssertEquals(Msg+' D2 equal',Expected.D2,Actual.D2);
  AssertEquals(Msg+' D2 equal',Expected.D3,Actual.D3);
  For I:=0 to 7 do
   AssertEquals(Msg+' D4['+IntToStr(I)+'] equal',Expected.D4[i],Actual.D4[i]);
end;

Procedure TTestHelpers.EqualGUIDSwap(Msg : String;Expected,Actual : TGUID);

Var
  I : Integer;

begin
  AssertEquals(Msg+' D1 equal',SwapEndian(Expected.D1),Actual.D1);
  AssertEquals(Msg+' D2 equal',SwapEndian(Expected.D2),Actual.D2);
  AssertEquals(Msg+' D2 equal',SwapEndian(Expected.D3),Actual.D3);
  For I:=0 to 7 do
   AssertEquals(Msg+' D4['+IntToStr(I)+'] equal',Expected.D4[i],Actual.D4[i]);
end;

Procedure TTestHelpers.TestGUIDHelperCreateUntypedData;

Var
  Src,Dest : TGUID;

begin
  GetGUID(Src);
  Dest:=TGUID.Create(Src, True);
  if CPUEndian = TEndian.Big then
    EqualGUID('BE CPU: Create(Data,True)',Src,Dest)
  else
    EqualGUIDSwap('LE CPU: Create(Data,True)',Src,Dest);
  Dest:=TGUID.Create(Src, False);
  if CPUEndian = TEndian.Big then
    EqualGUIDSwap('BE CPU: Create(Data,False)',Src,Dest)
  else
    EqualGUID('LE CPU : Create(Data,False)',Src,Dest);
end;

Procedure TTestHelpers.TestGUIDHelperCreateUntypedDataEndian;

Var
  Src,Dest : TGUID;

begin
  GetGUID(Src);
  Dest:=TGUID.Create(Src, True);
  if CPUEndian = TEndian.Big then
    EqualGUID('BE CPU: Create(Data,True)',Src,Dest)
  else
    EqualGUIDSwap('LE CPU: Create(Data,True)',Src,Dest);
  Dest:=TGUID.Create(Src, False);
  if CPUEndian = TEndian.Big then
    EqualGUIDSwap('BE CPU: Create(Data,False)',Src,Dest)
  else
    EqualGUID('LE CPU : Create(Data,False)',Src,Dest);
end;


Procedure TTestHelpers.TestGUIDHelperCreateTBytes;

Var
  Src,Dest : TGUID;
  SrcBytes : TBytes;
  D,I : Cardinal;

begin
  GetGUID(Src);
  SrcBytes:=[];
  SetLength(SrcBytes,16);
  D:=Src.D1;
  SrcBytes[0]:=D shr 24;
  SrcBytes[1]:=(D shr 16) and $FF;
  SrcBytes[2]:=(D shr 8) and $FF;
  SrcBytes[3]:=(D and $FF);
  D:=Src.D2;
  SrcBytes[4]:=(D shr 8) and $FF;
  SrcBytes[5]:=(D and $FF);
  D:=Src.D3;
  SrcBytes[6]:=(D shr 8) and $FF;
  SrcBytes[7]:=(D and $FF);
  For I:=0 to 7 do
   SrcBytes[8+i]:=Src.D4[i];
  Dest:=TGUID.Create(SrcBytes, TEndian.Big);
  if CPUEndian = TEndian.Big then
    EqualGUID('BE CPU: Create(Data,True)',Src,Dest)
  else
    EqualGUIDSwap('LE CPU: Create(Data,True)',Src,Dest);
  Dest:=TGUID.Create(SrcBytes, TEndian.Little);
  if CPUEndian = TEndian.Big then
    EqualGUIDSwap('BE CPU: Create(Data,False)',Src,Dest)
  else
    EqualGUID('LE CPU : Create(Data,False)',Src,Dest);
end;

Procedure TTestHelpers.TestGUIDHelperCreateTBytesAtIndex;

Var
  Src,Dest : TGUID;
  SrcBytes : TBytes;
  I,D : Cardinal;

begin
  GetGUID(Src);
  SrcBytes:=[];
  SetLength(SrcBytes,32);
  D:=Src.D1;
  SrcBytes[4]:=D shr 24;
  SrcBytes[5]:=(D shr 16) and $FF;
  SrcBytes[6]:=(D shr 8) and $FF;
  SrcBytes[7]:=(D and $FF);
  D:=Src.D2;
  SrcBytes[8]:=(D shr 8) and $FF;
  SrcBytes[9]:=(D and $FF);
  D:=Src.D3;
  SrcBytes[10]:=(D shr 8) and $FF;
  SrcBytes[11]:=(D and $FF);
  For I:=0 to 7 do
   SrcBytes[12+i]:=Src.D4[i];
  Dest:=TGUID.Create(SrcBytes, 4, TEndian.Big);
  if CPUEndian = TEndian.Big then
    EqualGUID('BE CPU: Create(Data,True)',Src,Dest)
  else
    EqualGUIDSwap('LE CPU: Create(Data,True)',Src,Dest);
  Dest:=TGUID.Create(SrcBytes, 4, TEndian.Little);
  if CPUEndian = TEndian.Big then
    EqualGUIDSwap('BE CPU: Create(Data,False)',Src,Dest)
  else
    EqualGUID('LE CPU : Create(Data,False)',Src,Dest);
end;

Procedure TTestHelpers.TestGUIDHelperCreateString;

Var
  Src,Dest : TGUID;

begin
  GetGUID(Src);
  Dest:=TGUID.Create(GUIDToString(Src));
  EqualGUID('Check equals',Src,Dest);
end;

Procedure TTestHelpers.TestGUIDHelperCreateIntegerBytes;

// Class Function Create(A: Integer; B: SmallInt; C: SmallInt; const D: TBytes): TGUID; overload; static;

Var
  A,I : Integer;
  B,C : Smallint;
  D : TBytes;
  Dest : TGUID;

begin
  A:=1;
  B:=2;
  C:=3;
  D:=Nil;
  SetLength(D,8);
  For I:=0 to 7 do
    D[i]:=4+I;
  Dest:=TGuid.Create(A,B,C,D);
  AssertEquals('D1',1,Dest.D1);
  AssertEquals('D2',2,Dest.D2);
  AssertEquals('D3',3,Dest.D3);
  For I:=0 to 7 do
    AssertEquals('D4['+IntToStr(i)+']',I+4,Dest.D4[i]);
end;

Procedure TTestHelpers.TestGUIDHelperCreateWords;
// Class Function Create(A: Cardinal; B: Word; C: Word; D, E, F, G, H, I, J, K: Byte): TGUID; overload; static;

Var
  A,I : Cardinal;
  B,C : Word;
  Dest : TGUID;

begin
  A:=1;
  B:=Word($FFFE);
  C:=Word($FFFF);
  Dest:=TGuid.Create(A,B,C,4,5,6,7,8,9,10,11);
  AssertEquals('D1',1,Dest.D1);
  AssertEquals('D2',$FFFE,Dest.D2);
  AssertEquals('D3',$FFFF,Dest.D3);
  For I:=0 to 7 do
    AssertEquals('D4['+IntToStr(i)+']',I+4,Dest.D4[i]);
end;

Procedure TTestHelpers.TestGUIDHelperCreateInteger;
// Class Function Create(A: Integer; B: SmallInt; C: SmallInt; D, E, F, G, H, I, J, K: Byte): TGUID; overload; static;

Var
  A,I : Integer;
  B,C : Smallint;
  Dest : TGUID;

begin
  A:=1;
  B:=Smallint($FFFE);
  C:=Smallint($FFFF);
  Dest:=TGuid.Create(A,B,C,4,5,6,7,8,9,10,11);
  AssertEquals('D1',1,Dest.D1);
  AssertEquals('D2',$FFFE,Dest.D2);
  AssertEquals('D3',$FFFF,Dest.D3);
  For I:=0 to 7 do
    AssertEquals('D4['+IntToStr(i)+']',I+4,Dest.D4[i]);
end;

Procedure TTestHelpers.TestGUIDHelperCreateNew;
// Class Function NewGuid: TGUID; static;

Var
  Src,Dest : TGuid;
  I,J : integer;

begin
  // All we can do is check that you don't get the same GUID twice.
  Src:=TGuid.NewGuid;
  Dest:=TGuid.NewGuid;
  I:=0;
  Inc(I,Ord(Src.D1<>Dest.D1));
  Inc(I,Ord(Src.D2<>Dest.D2));
  Inc(I,Ord(Src.D3<>Dest.D3));
  For J:=0 to 7 do
    Inc(I,Ord(Src.D4[i]<>Dest.D4[i]));
  AssertTrue('D1<>D2',I>0);
end;

Procedure TTestHelpers.TestGUIDHelperToByteArray;

Var
  Src,Dest : TGuid;
  D : TBytes;

begin
  // All we can do is check that you don't get the same GUID twice.
  Src:=TGuid.NewGuid;
  D:=Src.ToByteArray(CPUEndian);
  Dest:=TGUID.Create(D,CPUEndian);
  EqualGUID('Check equals',Src,Dest);
  if CPUEndian=TEndian.Big then
    Dest:=TGUID.Create(D,TEndian.Little)
  else
    Dest:=TGUID.Create(D,TEndian.Big);
  EqualGUIDSwap('Swapped, Check equals',Src,Dest);
end;

Procedure TTestHelpers.TestGUIDHelperToString;
// Function ToString: string;

Var
  Src : TGuid;
  S : String;
begin
  CreateGUID(Src);
  S:=GuidToString(Src);
  AssertEquals('Equal',S,Src.ToString);
  Delete(S,1,1);
  Delete(S,Length(S),1);
  AssertEquals('Equal',S,Src.ToString(True));
end;


Procedure TTestHelpers.TestIsNanDouble;


var
  Value: Double;

begin
  asm
  Value = Number.NaN; // Double.NaN;
  end;
  AssertEquals('Is Nan',True,Value.IsNan);
end;




Procedure TTestHelpers.TestByteSetBit;
var
  Index: TByteBitIndex;
  B: Byte;
const
  Expected: array[TByteBitIndex] of byte = ($01,$03,$07,$0F,$1F,$3F,$7F,$FF);
begin
  // writeln('TestByteSetBit Start');
  B := 0;
  for Index in TByteBitIndex do
  begin
    B.SetBit(Index);
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],B);
  end;
  // writeln('TestByteSetBit: OK');
end;

Procedure TTestHelpers.TestByteToggleBit;
var
  Index: TByteBitIndex;
  B: Byte;
const
  Expected: array[TByteBitIndex] of byte = ($01,$03,$07,$0F,$1F,$3F,$7F,$FF);
begin
  // writeln('TestByteToggleBit Start');
  B := 0;
  for Index in TByteBitIndex do
  begin
    B.ToggleBit(Index);
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],B);
  end;
  // writeln('TestByteToggleBit: OK');
end;

Procedure TTestHelpers.TestByteClearBit;
var
  Index: TByteBitIndex;
  B: Byte;
const
  Expected: array[TByteBitIndex] of byte = ($FE,$FD,$FB,$F7,$EF,$DF,$BF,$7F);
begin
  // writeln('TestByteClearBit Start');
  for Index in TByteBitIndex do
  begin
    B := High(Byte);
    B.ClearBit(Index);
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],B);
  end;
  // writeln('TestByteClearBit: OK');
end;

Procedure TTestHelpers.TestByteTestBit;
var
  Index: TByteBitIndex;
  B: Byte;
const
  Expected: array[TByteBitIndex] of Boolean = (True,False,True,False,True,False,True,False);
begin
  // writeln('TestByteTestBit Start');
  B := $55;
  for Index in TByteBitIndex do
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],B.TestBit(Index));
  // writeln('TestByteTestBit: OK');
end;


Procedure TTestHelpers.TestShortIntSetBit;
var
  Index: TShortIntBitIndex;
  S: ShortInt;
const
  Expected: array[TByteBitIndex] of ShortInt = (
    ShortInt($01),ShortInt($03),ShortInt($07),ShortInt($0F),
    ShortInt($1F),ShortInt($3F),ShortInt($7F),ShortInt($FF));
begin
  // writeln('TestShortIntSetBit Start');
  S := 0;
  for Index in TShortIntBitIndex do
  begin
    S.SetBit(Index);
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],S);
  end;
  // writeln('TestShortIntSetBit: OK');
end;

Procedure TTestHelpers.TestShortIntToggleBit;
var
  Index: TShortIntBitIndex;
  S: ShortInt;
const
  Expected: array[TByteBitIndex] of ShortInt = (
    ShortInt($01),ShortInt($03),ShortInt($07),ShortInt($0F),
    ShortInt($1F),ShortInt($3F),ShortInt($7F),ShortInt($FF));
begin
  // writeln('TestShortIntToggleBit Start');
  S := 0;
  for Index in TShortIntBitIndex do
  begin
    S.ToggleBit(Index);
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],S);
  end;
  // writeln('TestShortIntToggleBit: OK');
end;

Procedure TTestHelpers.TestShortIntClearBit;

var
  Index: TShortIntBitIndex;
  S: ShortInt;
const
  Expected: array[TByteBitIndex] of ShortInt = (
    ShortInt($FE),ShortInt($FD),ShortInt($FB),ShortInt($F7),
    ShortInt($EF),ShortInt($DF),ShortInt($BF),ShortInt($7F));
begin
  // writeln('TestShortIntClearBit Start');
  for Index in TShortIntBitIndex do
  begin
    S := ShortInt($FF);
    S.ClearBit(Index);// was Togglebit ?
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],S);
  end;
  // writeln('TestShortIntClearBit: OK');
end;

Procedure TTestHelpers.TestShortIntTestBit;

var
  Index: TShortIntBitIndex;
  S: ShortInt;
const
  Expected: array[TByteBitIndex] of Boolean = (True,False,True,False,True,False,True,False);
begin
  // writeln('TestShortIntTestBit Start');
  S := ShortInt($55);
  for Index in TShortIntBitIndex do
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],S.TestBit(Index));
  // writeln('TestShortIntTestBit: OK');
end;


Procedure TTestHelpers.TestWordSetBit;

var
  Index: TWordBitIndex;
  W: Word;
const
  Expected: array[TWordBitIndex] of Word = (
    $0001,$0003,$0007,$000F,$001F,$003F,$007F,$00FF,
    $01FF,$03FF,$07FF,$0FFF,$1FFF,$3FFF,$7FFF,$FFFF);
begin
  // writeln('TestWordSetBit Start');
  W := 0;
  for Index in TWordBitIndex do
  begin
    W.SetBit(Index);
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],W);
  end;
  // writeln('TestWordSetBit: OK');
end;


Procedure TTestHelpers.TestWordToggleBit;
var
  Index: TWordBitIndex;
  W: Word;
const
  Expected: array[TWordBitIndex] of Word = (
    $0001,$0003,$0007,$000F,$001F,$003F,$007F,$00FF,
    $01FF,$03FF,$07FF,$0FFF,$1FFF,$3FFF,$7FFF,$FFFF);
begin
  // writeln('TestWordToggleBit Start');
  W := 0;
  for Index in TWordBitIndex do
  begin
    W.ToggleBit(Index);
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],W);
  end;
  // writeln('TestWordToggleBit: OK');
end;


Procedure TTestHelpers.TestWordClearBit;
var
  Index: TWordBitIndex;
  W: Word;
const
  Expected: array[TWordBitIndex] of Word = (
    $FFFE,$FFFD,$FFFB,$FFF7,$FFEF,$FFDF,$FFBF,$FF7F,
    $FEFF,$FDFF,$FBFF,$F7FF,$EFFF,$DFFF,$BFFF,$7FFF);
begin
  // writeln('TestWordClearBit Start');
  for Index in TWordBitIndex do
  begin
    W := High(Word);
    W.ClearBit(Index);
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],W);
  end;
  // writeln('TestWordClearBit: OK');
end;

Procedure TTestHelpers.TestWordTestBit;
var
  Index: TWordBitIndex;
  W: Word;
const
  Expected: array[TWordBitIndex] of Boolean = (True,False,True,False,True,False,True,False,
                                               True,False,True,False,True,False,True,False);
begin
  // writeln('TestWordTestBit Start');
  W := $5555;
  for Index in TWordBitIndex do
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],W.TestBit(Index));
  // writeln('TestWordTestBit: OK');
end;


Procedure TTestHelpers.TestSmallIntSetBit;
var
  Index: TSmallIntBitIndex;
  S: SmallInt;

const
  Expected: array[TSmallIntBitIndex] of SmallInt = (
    SmallInt($0001),SmallInt($0003),SmallInt($0007),SmallInt($000F),
    SmallInt($001F),SmallInt($003F),SmallInt($007F),SmallInt($00FF),
    SmallInt($01FF),SmallInt($03FF),SmallInt($07FF),SmallInt($0FFF),
    SmallInt($1FFF),SmallInt($3FFF),SmallInt($7FFF),SmallInt($FFFF));
begin
  // writeln('TestSmallIntSetBit Start');
  S := 0;
  for Index in TSmallIntBitIndex do
  begin
    S.SetBit(Index);
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],S);
  end;
  // writeln('TestSmallIntSetBit: OK');
end;


Procedure TTestHelpers.TestSmallIntToggleBit;
var
  Index: TSmallIntBitIndex;
  S: SmallInt;
const
  Expected: array[TSmallIntBitIndex] of SmallInt = (
    SmallInt($0001),SmallInt($0003),SmallInt($0007),SmallInt($000F),
    SmallInt($001F),SmallInt($003F),SmallInt($007F),SmallInt($00FF),
    SmallInt($01FF),SmallInt($03FF),SmallInt($07FF),SmallInt($0FFF),
    SmallInt($1FFF),SmallInt($3FFF),SmallInt($7FFF),SmallInt($FFFF));
begin
  // writeln('TestSmallIntToggleBit Start');
  S := 0;
  for Index in TSmallIntBitIndex do
  begin
    S.ToggleBit(Index);
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],S);
  end;
  // writeln('TestSmallIntToggleBit: OK');
end;


Procedure TTestHelpers.TestSmallIntClearBit;
var
  Index: TSmallIntBitIndex;
  S: SmallInt;
const
  Expected: array[TSmallIntBitIndex] of SmallInt = (
    SmallInt($FFFE),SmallInt($FFFD),SmallInt($FFFB),SmallInt($FFF7),
    SmallInt($FFEF),SmallInt($FFDF),SmallInt($FFBF),SmallInt($FF7F),
    SmallInt($FEFF),SmallInt($FDFF),SmallInt($FBFF),SmallInt($F7FF),
    SmallInt($EFFF),SmallInt($DFFF),SmallInt($BFFF),SmallInt($7FFF));
begin
  // writeln('TestSmallIntClearBit Start');
  for Index in TSmallIntBitIndex do
  begin
    S := SmallInt($FFFF);
    S.ClearBit(Index);
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],S);
  end;
  // writeln('TestSmallIntClearBit: OK');
end;


Procedure TTestHelpers.TestSmallIntTestBit;
var
  Index: TSmallIntBitIndex;
  S: SmallInt;
const
  Expected: array[TSmallIntBitIndex] of Boolean = (True,False,True,False,True,False,True,False,
                                                   True,False,True,False,True,False,True,False);
begin
  // writeln('TestSmallIntTestBit Start');
  S := SMallInt($5555);
  for Index in TSmallIntBitIndex do
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],S.TestBit(Index));
  // writeln('TestSmallIntTestBit: OK');
end;


Procedure TTestHelpers.TestCardinalSetBit;
var
  Index: TCardinalBitIndex;
  C: Cardinal;
const
  Expected: array[TCardinalBitIndex] of Cardinal = (
    $00000001,$00000003,$00000007,$0000000F,
    $0000001F,$0000003F,$0000007F,$000000FF,
    $000001FF,$000003FF,$000007FF,$00000FFF,
    $00001FFF,$00003FFF,$00007FFF,$0000FFFF,
    $0001FFFF,$0003FFFF,$0007FFFF,$000FFFFF,
    $001FFFFF,$003FFFFF,$007FFFFF,$00FFFFFF,
    $01FFFFFF,$03FFFFFF,$07FFFFFF,$0FFFFFFF,
    $1FFFFFFF,$3FFFFFFF,$7FFFFFFF,$FFFFFFFF);
begin
  // writeln('TestCardinalSetBit Start');
  C := 0;
  for Index in TCardinalBitIndex do
  begin
    C.SetBit(Index);
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],C);
  end;
  // writeln('TestCardinalSetBit: OK');
end;


Procedure TTestHelpers.TestCardinalToggleBit;
var
  Index: TCardinalBitIndex;
  C: Cardinal;
const
  Expected: array[TCardinalBitIndex] of Cardinal = (
    $00000001,$00000003,$00000007,$0000000F,
    $0000001F,$0000003F,$0000007F,$000000FF,
    $000001FF,$000003FF,$000007FF,$00000FFF,
    $00001FFF,$00003FFF,$00007FFF,$0000FFFF,
    $0001FFFF,$0003FFFF,$0007FFFF,$000FFFFF,
    $001FFFFF,$003FFFFF,$007FFFFF,$00FFFFFF,
    $01FFFFFF,$03FFFFFF,$07FFFFFF,$0FFFFFFF,
    $1FFFFFFF,$3FFFFFFF,$7FFFFFFF,$FFFFFFFF);
begin
  // writeln('TestCardinalToggleBit Start');
  C := 0;
  for Index in TCardinalBitIndex do
  begin
    C.ToggleBit(Index);
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],C);
  end;
  // writeln('TestCardinalToggleBit: OK');
end;


Procedure TTestHelpers.TestCardinalClearBit;
var
  Index: TCardinalBitIndex;
  C: Cardinal;
const
  Expected: array[TCardinalBitIndex] of Cardinal = (
    $FFFFFFFE,$FFFFFFFD,$FFFFFFFB,$FFFFFFF7,
    $FFFFFFEF,$FFFFFFDF,$FFFFFFBF,$FFFFFF7F,
    $FFFFFEFF,$FFFFFDFF,$FFFFFBFF,$FFFFF7FF,
    $FFFFEFFF,$FFFFDFFF,$FFFFBFFF,$FFFF7FFF,
    $FFFEFFFF,$FFFDFFFF,$FFFBFFFF,$FFF7FFFF,
    $FFEFFFFF,$FFDFFFFF,$FFBFFFFF,$FF7FFFFF,
    $FEFFFFFF,$FDFFFFFF,$FBFFFFFF,$F7FFFFFF,
    $EFFFFFFF,$DFFFFFFF,$BFFFFFFF,$7FFFFFFF);
begin
  // writeln('TestCardinalClearBit Start');
  for Index in TCardinalBitIndex do
  begin
    C := High(Cardinal);
    C.ClearBit(Index);
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],C);
  end;
  // writeln('TestCardinalClearBit: OK');
end;

Procedure TTestHelpers.TestCardinalTestBit;
var
  Index: TCardinalBitIndex;
  C: Cardinal;
const
  Expected: array[TCardinalBitIndex] of Boolean = (
                                               True,False,True,False,True,False,True,False,
                                               True,False,True,False,True,False,True,False,
                                               True,False,True,False,True,False,True,False,
                                               True,False,True,False,True,False,True,False);
begin
  // writeln('TestCardinalTestBit Start');
  C := $55555555;
  for Index in TCardinalBitIndex do
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],C.TestBit(Index));
  // writeln('TestCardinalTestBit: OK');
end;


Procedure TTestHelpers.TestLongintSetBit;

var
  Index: TLongintBitIndex;
  L: Longint;

const
  Expected: array[TLongintBitIndex] of Longint = (
    Longint($00000001),Longint($00000003),Longint($00000007),Longint($0000000F),
    Longint($0000001F),Longint($0000003F),Longint($0000007F),Longint($000000FF),
    Longint($000001FF),Longint($000003FF),Longint($000007FF),Longint($00000FFF),
    Longint($00001FFF),Longint($00003FFF),Longint($00007FFF),Longint($0000FFFF),
    Longint($0001FFFF),Longint($0003FFFF),Longint($0007FFFF),Longint($000FFFFF),
    Longint($001FFFFF),Longint($003FFFFF),Longint($007FFFFF),Longint($00FFFFFF),
    Longint($01FFFFFF),Longint($03FFFFFF),Longint($07FFFFFF),Longint($0FFFFFFF),
    Longint($1FFFFFFF),Longint($3FFFFFFF),Longint($7FFFFFFF),Longint($FFFFFFFF));

begin
  // writeln('TestLongintSetBit Start');
  L := 0;
  for Index in TLongintBitIndex do
  begin
    L.SetBit(Index);
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],L);
  end;
  // writeln('TestLongintSetBit: OK');
end;


Procedure TTestHelpers.TestLongintToggleBit;
var
  Index: TLongintBitIndex;
  L: Longint;
const
  Expected: array[TLongintBitIndex] of Longint = (
    Longint($00000001),Longint($00000003),Longint($00000007),Longint($0000000F),
    Longint($0000001F),Longint($0000003F),Longint($0000007F),Longint($000000FF),
    Longint($000001FF),Longint($000003FF),Longint($000007FF),Longint($00000FFF),
    Longint($00001FFF),Longint($00003FFF),Longint($00007FFF),Longint($0000FFFF),
    Longint($0001FFFF),Longint($0003FFFF),Longint($0007FFFF),Longint($000FFFFF),
    Longint($001FFFFF),Longint($003FFFFF),Longint($007FFFFF),Longint($00FFFFFF),
    Longint($01FFFFFF),Longint($03FFFFFF),Longint($07FFFFFF),Longint($0FFFFFFF),
    Longint($1FFFFFFF),Longint($3FFFFFFF),Longint($7FFFFFFF),Longint($FFFFFFFF));

begin
  // writeln('TestLongintToggleBit Start');
  L := 0;
  for Index in TLongintBitIndex do
  begin
    L.ToggleBit(Index);
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],L);
  end;
  // writeln('TestLongintToggleBit: OK');
end;


Procedure TTestHelpers.TestLongintClearBit;
var
  Index: TLongintBitIndex;
  L: Longint;
const
  Expected: array[TLongintBitIndex] of Longint = (
    Longint($FFFFFFFE),Longint($FFFFFFFD),Longint($FFFFFFFB),Longint($FFFFFFF7),
    Longint($FFFFFFEF),Longint($FFFFFFDF),Longint($FFFFFFBF),Longint($FFFFFF7F),
    Longint($FFFFFEFF),Longint($FFFFFDFF),Longint($FFFFFBFF),Longint($FFFFF7FF),
    Longint($FFFFEFFF),Longint($FFFFDFFF),Longint($FFFFBFFF),Longint($FFFF7FFF),
    Longint($FFFEFFFF),Longint($FFFDFFFF),Longint($FFFBFFFF),Longint($FFF7FFFF),
    Longint($FFEFFFFF),Longint($FFDFFFFF),Longint($FFBFFFFF),Longint($FF7FFFFF),
    Longint($FEFFFFFF),Longint($FDFFFFFF),Longint($FBFFFFFF),Longint($F7FFFFFF),
    Longint($EFFFFFFF),Longint($DFFFFFFF),Longint($BFFFFFFF),Longint($7FFFFFFF));

begin
  // writeln('TestLongintClearBit Start');
  for Index in TLongintBitIndex do
  begin
    L := Longint($FFFFFFFF);
    L.ClearBit(Index);
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],L);
  end;
  // writeln('TestLongintClearBit: OK');
end;


Procedure TTestHelpers.TestLongintTestBit;

var
  Index: TLongintBitIndex;
  L: Longint;
const
  Expected: array[TLongintBitIndex] of Boolean = (
                                               True,False,True,False,True,False,True,False,
                                               True,False,True,False,True,False,True,False,
                                               True,False,True,False,True,False,True,False,
                                               True,False,True,False,True,False,True,False);
begin
  // writeln('TestLongintTestBit Start');
  L := Longint($55555555);
  for Index in TLongintBitIndex do
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],L.TestBit(Index));
  // writeln('TestLongintTestBit: OK');
end;



Procedure TTestHelpers.TestNativeUintSetBit;
var
  Index: TQWordBitIndex;
  Q: NativeUInt;
const
  Expected: array[TQWordBitIndex] of NativeUInt = (
    $0000000000000001,$0000000000000003,$0000000000000007,$000000000000000F,
    $000000000000001F,$000000000000003F,$000000000000007F,$00000000000000FF,
    $00000000000001FF,$00000000000003FF,$00000000000007FF,$0000000000000FFF,
    $0000000000001FFF,$0000000000003FFF,$0000000000007FFF,$000000000000FFFF,
    $000000000001FFFF,$000000000003FFFF,$000000000007FFFF,$00000000000FFFFF,
    $00000000001FFFFF,$00000000003FFFFF,$00000000007FFFFF,$0000000000FFFFFF,
    $0000000001FFFFFF,$0000000003FFFFFF,$0000000007FFFFFF,$000000000FFFFFFF,
    $000000001FFFFFFF,$000000003FFFFFFF,$000000007FFFFFFF,$00000000FFFFFFFF,
    $00000001FFFFFFFF,$00000003FFFFFFFF,$00000007FFFFFFFF,$0000000FFFFFFFFF,
    $0000001FFFFFFFFF,$0000003FFFFFFFFF,$0000007FFFFFFFFF,$000000FFFFFFFFFF,
    $000001FFFFFFFFFF,$000003FFFFFFFFFF,$000007FFFFFFFFFF,$00000FFFFFFFFFFF,
    $00001FFFFFFFFFFF,$00003FFFFFFFFFFF,$00007FFFFFFFFFFF,$0000FFFFFFFFFFFF,
    $0001FFFFFFFFFFFF,$0003FFFFFFFFFFFF,$0007FFFFFFFFFFFF,$000FFFFFFFFFFFFF,
    $001FFFFFFFFFFFFF{,$003FFFFFFFFFFFFF,$007FFFFFFFFFFFFF,$00FFFFFFFFFFFFFF,
    $01FFFFFFFFFFFFFF,$03FFFFFFFFFFFFFF,$07FFFFFFFFFFFFFF,$0FFFFFFFFFFFFFFF,
    $1FFFFFFFFFFFFFFF,$3FFFFFFFFFFFFFFF,$7FFFFFFFFFFFFFFF,QWORD($FFFFFFFFFFFFFFFF)});
begin
  Fail('Not implemented');
  // writeln('TestQWordSetBit Start');
  Q := 0;
  for Index in TQWordBitIndex do
  begin
// TODO    Q.SetBit(Index);
//    AssertEquals('Bit '+IntToStr(Index),Expected[Index],S);
  end;
  // writeln('TestQWordSetBit: OK');
end;


Procedure TTestHelpers.TestNativeUIntToggleBit;

var
  Index: TQWordBitIndex;
  Q: NativeUint;
const
  Expected: array[TQWordBitIndex] of NativeUInt = (
    $0000000000000001,$0000000000000003,$0000000000000007,$000000000000000F,
    $000000000000001F,$000000000000003F,$000000000000007F,$00000000000000FF,
    $00000000000001FF,$00000000000003FF,$00000000000007FF,$0000000000000FFF,
    $0000000000001FFF,$0000000000003FFF,$0000000000007FFF,$000000000000FFFF,
    $000000000001FFFF,$000000000003FFFF,$000000000007FFFF,$00000000000FFFFF,
    $00000000001FFFFF,$00000000003FFFFF,$00000000007FFFFF,$0000000000FFFFFF,
    $0000000001FFFFFF,$0000000003FFFFFF,$0000000007FFFFFF,$000000000FFFFFFF,
    $000000001FFFFFFF,$000000003FFFFFFF,$000000007FFFFFFF,$00000000FFFFFFFF,
    $00000001FFFFFFFF,$00000003FFFFFFFF,$00000007FFFFFFFF,$0000000FFFFFFFFF,
    $0000001FFFFFFFFF,$0000003FFFFFFFFF,$0000007FFFFFFFFF,$000000FFFFFFFFFF,
    $000001FFFFFFFFFF,$000003FFFFFFFFFF,$000007FFFFFFFFFF,$00000FFFFFFFFFFF,
    $00001FFFFFFFFFFF,$00003FFFFFFFFFFF,$00007FFFFFFFFFFF,$0000FFFFFFFFFFFF,
    $0001FFFFFFFFFFFF,$0003FFFFFFFFFFFF,$0007FFFFFFFFFFFF,$000FFFFFFFFFFFFF,
    $001FFFFFFFFFFFFF{,$003FFFFFFFFFFFFF,$007FFFFFFFFFFFFF,$00FFFFFFFFFFFFFF,
    $01FFFFFFFFFFFFFF,$03FFFFFFFFFFFFFF,$07FFFFFFFFFFFFFF,$0FFFFFFFFFFFFFFF,
    $1FFFFFFFFFFFFFFF,$3FFFFFFFFFFFFFFF,$7FFFFFFFFFFFFFFF,QWORD($FFFFFFFFFFFFFFFF)});
begin
  Fail('Not implemented');
  // writeln('TestQWordToggleBit Start');
  Q := 0;
  for Index in TQWordBitIndex do
  begin
// TODO    Q.ToggleBit(Index);
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],Q);
  end;
  // writeln('TestQWordToggleBit: OK');
end;



Procedure TTestHelpers.TestNativeUIntTestBit;
var
  Index: TQWordBitIndex;
  Q: NativeUint;
const
  Expected: array[TQWordBitIndex] of Boolean = (True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True);
begin
  Fail('NotImplemented');
  // writeln('TestQWordTestBit Start');
  Q := $5555555555;
  for Index in TQWordBitIndex do
//    AssertEquals('Bit '+IntToStr(Index),Expected[Index],Q.TestBit(Index));
  // writeln('TestQWordTestBit: OK');
end;



Procedure TTestHelpers.TestNativeIntSetBit;
var
  Index: TInt64BitIndex;
  I64: NativeInt;
const
  Expected: array[TNativeIntBitIndex] of NativeInt = (
    NativeInt($0000000000000001),NativeInt($0000000000000003),NativeInt($0000000000000007),NativeInt($000000000000000F),
    NativeInt($000000000000001F),NativeInt($000000000000003F),NativeInt($000000000000007F),NativeInt($00000000000000FF),
    NativeInt($00000000000001FF),NativeInt($00000000000003FF),NativeInt($00000000000007FF),NativeInt($0000000000000FFF),
    NativeInt($0000000000001FFF),NativeInt($0000000000003FFF),NativeInt($0000000000007FFF),NativeInt($000000000000FFFF),
    NativeInt($000000000001FFFF),NativeInt($000000000003FFFF),NativeInt($000000000007FFFF),NativeInt($00000000000FFFFF),
    NativeInt($00000000001FFFFF),NativeInt($00000000003FFFFF),NativeInt($00000000007FFFFF),NativeInt($0000000000FFFFFF),
    NativeInt($0000000001FFFFFF),NativeInt($0000000003FFFFFF),NativeInt($0000000007FFFFFF),NativeInt($000000000FFFFFFF),
    NativeInt($000000001FFFFFFF),NativeInt($000000003FFFFFFF),NativeInt($000000007FFFFFFF),NativeInt($00000000FFFFFFFF),
    NativeInt($00000001FFFFFFFF),NativeInt($00000003FFFFFFFF),NativeInt($00000007FFFFFFFF),NativeInt($0000000FFFFFFFFF),
    NativeInt($0000001FFFFFFFFF),NativeInt($0000003FFFFFFFFF),NativeInt($0000007FFFFFFFFF),NativeInt($000000FFFFFFFFFF),
    NativeInt($000001FFFFFFFFFF),NativeInt($000003FFFFFFFFFF),NativeInt($000007FFFFFFFFFF),NativeInt($00000FFFFFFFFFFF),
    NativeInt($00001FFFFFFFFFFF),NativeInt($00003FFFFFFFFFFF),NativeInt($00007FFFFFFFFFFF),NativeInt($0000FFFFFFFFFFFF),
    NativeInt($0001FFFFFFFFFFFF),NativeInt($0003FFFFFFFFFFFF),NativeInt($0007FFFFFFFFFFFF),NativeInt($000FFFFFFFFFFFFF),
    NativeInt($001FFFFFFFFFFFFF));
begin
  Fail('Not implemented');
  // writeln('TestNativeIntSetBit Start');
  I64 := 0;
  for Index in TNativeIntBitIndex do
  begin
// TODO    I64.SetBit(Index);
    AssertEquals('Bit '+IntToStr(Index),Expected[Index],I64);
  end;
  // writeln('TestNativeIntSetBit: OK');
end;


Procedure TTestHelpers.TestNativeIntToggleBit;
var
  Index: TNativeIntBitIndex;
  I64: NativeInt;
const
  Expected: array[TNativeIntBitIndex] of NativeInt = (
  NativeInt($0000000000000001),NativeInt($0000000000000003),NativeInt($0000000000000007),NativeInt($000000000000000F),
  NativeInt($000000000000001F),NativeInt($000000000000003F),NativeInt($000000000000007F),NativeInt($00000000000000FF),
  NativeInt($00000000000001FF),NativeInt($00000000000003FF),NativeInt($00000000000007FF),NativeInt($0000000000000FFF),
  NativeInt($0000000000001FFF),NativeInt($0000000000003FFF),NativeInt($0000000000007FFF),NativeInt($000000000000FFFF),
  NativeInt($000000000001FFFF),NativeInt($000000000003FFFF),NativeInt($000000000007FFFF),NativeInt($00000000000FFFFF),
  NativeInt($00000000001FFFFF),NativeInt($00000000003FFFFF),NativeInt($00000000007FFFFF),NativeInt($0000000000FFFFFF),
  NativeInt($0000000001FFFFFF),NativeInt($0000000003FFFFFF),NativeInt($0000000007FFFFFF),NativeInt($000000000FFFFFFF),
  NativeInt($000000001FFFFFFF),NativeInt($000000003FFFFFFF),NativeInt($000000007FFFFFFF),NativeInt($00000000FFFFFFFF),
  NativeInt($00000001FFFFFFFF),NativeInt($00000003FFFFFFFF),NativeInt($00000007FFFFFFFF),NativeInt($0000000FFFFFFFFF),
  NativeInt($0000001FFFFFFFFF),NativeInt($0000003FFFFFFFFF),NativeInt($0000007FFFFFFFFF),NativeInt($000000FFFFFFFFFF),
  NativeInt($000001FFFFFFFFFF),NativeInt($000003FFFFFFFFFF),NativeInt($000007FFFFFFFFFF),NativeInt($00000FFFFFFFFFFF),
  NativeInt($00001FFFFFFFFFFF),NativeInt($00003FFFFFFFFFFF),NativeInt($00007FFFFFFFFFFF),NativeInt($0000FFFFFFFFFFFF),
  NativeInt($0001FFFFFFFFFFFF),NativeInt($0003FFFFFFFFFFFF),NativeInt($0007FFFFFFFFFFFF),NativeInt($000FFFFFFFFFFFFF),
  NativeInt($001FFFFFFFFFFFFF));
begin
  Fail('Not implemented');
  // writeln('TestNativeIntToggleBit Start');
  I64 := 0;
  for Index in TNativeIntBitIndex do
  begin
    //I64.ToggleBit(Index);
      AssertEquals('Bit '+IntToStr(Index),Expected[Index],I64);
  end;
  // writeln('TestNativeIntToggleBit: OK');
end;


Procedure TTestHelpers.TestNativeIntClearBit;
var
  Index: TNativeIntBitIndex;
  I64: NativeInt;
begin
  Fail('NotImplemented');
//  for Index in TNativeIntBitIndex do
  begin
//    I64 := NativeInt($FFFFFFFFFFFFFFFF);
//    I64.ClearBit(Index);
//    AssertEquals('Bit '+IntToStr(Index),Expected[Index],I64);
  end;
  // writeln('TestNativeIntClearBit: OK');
end;


Procedure TTestHelpers.TestNativeIntTestBit;
var
  Index: TNativeIntBitIndex;
  I64: NativeInt;
const
  Expected: array[TNativeIntBitIndex] of Boolean = (True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True,False,True,False,
                                                True,False,True,False,True);
begin
  // writeln('TestNativeIntTestBit Start');
  I64 := NativeInt($5555555555555);
  for Index in TNativeIntBitIndex do
//    AssertEquals('Bit '+IntToStr(Index),Expected[Index],I64.TestBit(Index));
  // writeln('TestNativeIntTestBit: OK');
end;



initialization
  RegisterTest(TTestHelpers);
end.

