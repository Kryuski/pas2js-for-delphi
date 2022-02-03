{ TStream classes tests.

  Copyright (C) 2020 Michael Van Canneyt michael@freepascal.org

  This library is free software; you can redistribute it and/or modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version
  with the following modification:

  As a special exception, the copyright holders of this library give you permission to link this library with independent modules
  to produce an executable, regardless of the license terms of these independent modules,and to copy and distribute the resulting
  executable under terms of your choice, provided that you also meet, for each linked independent module, the terms and conditions
  of the license of that module. An independent module is a module which is not derived from or based on this library. If you
  modify this library, you may extend this exception to your version of the library, but you are not obligated to do so. If you do
  not wish to do so, delete this exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along with this library; if not, write to the Free
  Software Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
unit tcstream;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

type
  { TTestStream }

  TTestStream= class(TTestCase)
  protected
    FStream : TStream;
    procedure SetUp; override;
    procedure TearDown; override;
    procedure AssertBytes(B : Array of Byte; aMessage : string = ''; ObserveEndian : Boolean = True); overload;
    // procedure AssertBytes(B : TBytes; aMessage : string = '');overload;
    Function CreateBytes(aCount : integer) : TBytes;
    Property Stream :  TStream Read FStream;
  published
    procedure TestHookUp;
    Procedure TestBytes;
    Procedure TestBytesLarge;
    Procedure TestBytesLargeCopy;
    Procedure TestByte;
    Procedure TestByteBuffer;
    Procedure TestInt8;
    Procedure TestInt8Buffer;
    Procedure TestUInt8;
    Procedure TestUInt8Buffer;
    Procedure TestSmallint;
    Procedure TestSmallintBuffer;
    Procedure TestInt16;
    Procedure TestInt16Neg;
    Procedure TestInt16Buffer;
    Procedure TestUInt16;
    Procedure TestUInt16Buffer;
    Procedure TestInt32;
    Procedure TestInt32Neg;
    Procedure TestInt32Buffer;
    Procedure TestUInt32;
    Procedure TestUInt32Buffer;
    Procedure TestInt64;
    Procedure TestInt64Neg;
    Procedure TestInt64Buffer;
    Procedure TestBoolean;
    Procedure TestBooleanBuffer;
    Procedure TestWideChar;
    Procedure TestWideCharBuffer;
    Procedure TestDouble;
    Procedure TestDoubleBuffer;
{$ifndef ECMASCRIPT}
    Procedure TestAnsiChar;
    Procedure TestAnsicharBuffer;
    Procedure TestSingle;
    Procedure TestSingleBuffer;
    Procedure TestExtended;
    Procedure TestExtendedBuffer;
{$endif}
  end;

  { TTestBigendianStream }

  TTestBigendianStream= class(TTestStream)
  Public
    Procedure Setup; override;
  end;

  { TTestStringStream }

  TTestStringStream = class(TTestCase)
  private
    FStream: TStringStream;
  Public
    Procedure TearDown; override;
    Procedure DoCreate(S : String);
    Property Stream : TStringStream Read FStream;
  Published
    Procedure TestDataStringEmpty;
    Procedure TestDataString;
    Procedure TestWrite;
    Procedure TestRead;
    Procedure TestReadString;
    Procedure TestWriteString;
    Procedure TestCopyFrom;
  end;

implementation

{ TTestStringStream }

procedure TTestStringStream.TearDown;
begin
  FreeAndNil(FStream);
  inherited TearDown;
end;

procedure TTestStringStream.DoCreate(S: String);
begin
  FreeAndNil(FStream);
  FStream:=TStringStream.Create(S);
end;

procedure TTestStringStream.TestDataStringEmpty;
begin
  DoCreate('');
  AssertEquals('Empty','',Stream.DataString);
end;

procedure TTestStringStream.TestDataString;
begin
  DoCreate('ABCD');
  AssertEquals('Non-empty','ABCD',Stream.DataString);
end;

procedure TTestStringStream.TestWrite;
begin
  DoCreate('');
  Stream.WriteBufferData('A');
  Stream.WriteBufferData('B');
  Stream.WriteBufferData('C');
  Stream.WriteBufferData('D');
  AssertEquals('Write Contents','ABCD',Stream.DataString);
end;

procedure TTestStringStream.TestRead;

Var
  S : String;
  C : Char;
  I : Integer;

begin
  S:='ABCD';
  DoCreate(S);
  For I:=1 to Length(S) do
    begin
    Stream.ReadBufferData(C);
    AssertEquals(Format('Character at',[i]),S[i],C);
    end;
end;

procedure TTestStringStream.TestReadString;
Var
  S : String;


begin
  S:='ABCDEFGH';
  DoCreate(S);
  AssertEquals('2 characters','AB',Stream.ReadString(2));
  AssertEquals('Top off characters','CDEFGH',Stream.ReadString(11));
  S:='Hello World';
  FreeAndNil(FStream);
  DoCreate(S);
  AssertEquals('Correct string',S,Stream.ReadString(Length(S)));
end;

procedure TTestStringStream.TestWriteString;
begin
  DoCreate('');
  Stream.WriteString('AB');
  AssertEquals('Length 1',4,Stream.Size);
  AssertEquals('Datastring 1','AB',Stream.DataString);
  Stream.WriteString('CDEFGH');
  AssertEquals('Length 2',16,Stream.Size);
  AssertEquals('Datastring 2','ABCDEFGH',Stream.DataString);
end;

procedure TTestStringStream.TestCopyFrom;

Var
  S2 : TStringStream;

begin
  DoCreate('ABCD');
  S2:=TStringStream.Create('');
  try
    S2.CopyFrom(Stream,0);
    AssertEquals('Copied correctly','ABCD',S2.DataString);
  finally
    S2.Free;
  end;
end;

{ TTestBigendianStream }

procedure TTestBigendianStream.Setup;
begin
  inherited Setup;
  Stream.Endian:=Tendian.Big;
end;

procedure TTestStream.TestHookUp;
begin
  AssertNotNull('Have Stream',Stream);
end;

procedure TTestStream.TestBytes;

Var
  B : TBytes;

begin
  B:=CreateBytes(4);
  Stream.Write(B,4);
  AssertBytes(B,'Bytes, ignoring endianness',False);
end;

procedure TTestStream.TestBytesLarge;

Var
  B : TBytes;
begin
  B:=CreateBytes(8000);
  Stream.Write(B,Length(B));
  AssertBytes(B,'Bytes, ignoring endianness',False);
end;

procedure TTestStream.TestBytesLargeCopy;
Var
  B : TBytes;
  S : TStream;
begin
  S:=TBytesStream.Create([]);
  B:=CreateBytes(8000);
  S.Write(B,Length(B));
  Stream.CopyFrom(S,0);
  S.Free;
  AssertBytes(B,'Bytes, ignoring endianness',False);
end;

procedure TTestStream.TestByte;

Var
  S,D : Byte;

begin
  D:=0;
  S:=13;
  AssertEquals('Bytes written',1,Stream.WriteData(S));
  AssertBytes([S]);
  Stream.Position:=0;
  AssertEquals('Bytes read',1,Stream.ReadData(D));
  AssertEquals('Written data read correctly',S,D);
end;

procedure TTestStream.TestByteBuffer;

Var
  S,D : Byte;

begin
  D:=0;
  S:=13;
  Stream.WriteBufferData(S);
  AssertBytes([S]);
  Stream.Position:=0;
  Stream.ReadBufferData(D);
  AssertEquals('Written data read correctly',S,D);
end;

procedure TTestStream.TestInt8;

Var
  S,D : Int8;

begin
  D:=0;
  S:=-13;
  AssertEquals('Bytes written',1,Stream.WriteData(S));
  AssertBytes([S]);
  Stream.Position:=0;
  AssertEquals('Bytes read',1,Stream.ReadData(D));
  AssertEquals('Written data read correctly',S,D);
end;

procedure TTestStream.TestInt8Buffer;
Var
  S,D : Int8;
begin
  D:=0;
  S:=-13;
  Stream.WriteBufferData(S);
  AssertBytes([S]);
  Stream.Position:=0;
  Stream.ReadBufferData(D);
  AssertEquals('Written data read correctly',S,D);
end;

procedure TTestStream.TestUInt8;
Var
  S,D : UInt8;

begin
  D:=0;
  S:=139;
  AssertEquals('Bytes written',1,Stream.WriteData(S));
  AssertBytes([S]);
  Stream.Position:=0;
  AssertEquals('Bytes read',1,Stream.ReadData(D));
  AssertEquals('Written data read correctly',S,D);
end;

procedure TTestStream.TestUInt8Buffer;
Var
  S,D : UInt8;

begin
  D:=0;
  S:=139;
  Stream.WriteBufferData(S);
  AssertBytes([S]);
  Stream.Position:=0;
  Stream.ReadBufferData(D);
  AssertEquals('Written data read correctly',S,D);
end;

procedure TTestStream.TestSmallint;
Var
  S,D : SmallInt;

begin
  D:=0;
  S:=127*256+13;
  AssertEquals('Bytes written',2,Stream.WriteData(S));
  AssertBytes([13,127]);
  Stream.Position:=0;
  AssertEquals('Bytes read',2,Stream.ReadData(D));
  AssertEquals('Written data read correctly',S,D);
end;

procedure TTestStream.TestSmallintBuffer;

Var
  S,D : SmallInt;

begin
  D:=0;
  S:=127*256+13;
  Stream.WriteBufferData(S);
  AssertBytes([13,127]);
  Stream.Position:=0;
  Stream.ReadBufferData(D);
  AssertEquals('Written data read correctly',S,D);
end;

procedure TTestStream.TestInt16;

Var
  S,D : Int16;

begin
  D:=0;
  S:=127*256+13;
  AssertEquals('Bytes written',2,Stream.WriteData(S));
  AssertBytes([13,127]);
  Stream.Position:=0;
  AssertEquals('Bytes read',2,Stream.ReadData(D));
  AssertEquals('Written data read correctly',S,D);
end;

procedure TTestStream.TestInt16Neg;
Var
  S,D : Int16;

begin
  D:=0;
  S:=-4086; // $F00A;
  AssertEquals('Bytes written',2,Stream.WriteData(S));
  AssertBytes([$0A,$F0]);
  Stream.Position:=0;
  AssertEquals('Bytes read',2,Stream.ReadData(D));
  AssertEquals('Written data read correctly',S,D);
end;

procedure TTestStream.TestInt16Buffer;

Var
  S,D : Int16;

begin
  D:=0;
  S:=127*256+13;
  Stream.WriteBufferData(S);
  AssertBytes([13,127]);
  Stream.Position:=0;
  Stream.ReadBufferData(D);
  AssertEquals('Written data read correctly',S,D);
end;

procedure TTestStream.TestUInt16;

Var
  S,D : UInt16;

begin
  D:=0;
  S:=$F00A; // 61450
  AssertEquals('Bytes written',2,Stream.WriteData(S));
  AssertBytes([$0A,$F0]);
  Stream.Position:=0;
  AssertEquals('Bytes read',2,Stream.ReadData(D));
  AssertEquals('Written data read correctly',S,D);
end;

procedure TTestStream.TestUInt16Buffer;
Var
  S,D : UInt16;

begin
  D:=0;
  S:=$F00A;
  Stream.WriteBufferData(S);
  AssertBytes([$0A,$F0]);
  Stream.Position:=0;
  Stream.ReadBufferData(D);
  AssertEquals('Written data read correctly',S,D);
end;

procedure TTestStream.TestInt32;

Var
  S,D : Int32;

begin
  D:=0;
  // 2131560201
  S:=(127 shl 24) + (13 shl 16) + (7 shl 8) +  9;
  AssertEquals('Bytes written',4,Stream.WriteData(S));
  AssertBytes([9,7,13,127]);
  Stream.Position:=0;
  AssertEquals('Bytes read',4,Stream.ReadData(D));
  AssertEquals('Written data read correctly',S,D);
end;

procedure TTestStream.TestInt32Neg;

Var
  S,D : Int32;

begin
  D:=0;
  // -2146629879
  S:=Int32((128 shl 24) + (13 shl 16) + (7 shl 8) +  9);
  AssertEquals('Bytes written',4,Stream.WriteData(S));
  AssertBytes([9,7,13,128]);
  Stream.Position:=0;
  AssertEquals('Bytes read',4,Stream.ReadData(D));
  AssertEquals('Written data read correctly',S,D);
end;

procedure TTestStream.TestInt32Buffer;

Var
  S,D : Int32;

begin
  D:=0;
  // 2131560201
  S:=(127 shl 24) + (13 shl 16) + (7 shl 8) +  9;
  Stream.WriteBufferData(S);
  AssertBytes([9,7,13,127]);
  Stream.Position:=0;
  Stream.ReadBufferData(D);
  AssertEquals('Written data read correctly',S,D);
end;

procedure TTestStream.TestUInt32;

Var
  S,D : UInt32;

begin
  D:=0;
  // 2148337417
  S:=UINT32((128 shl 24) + (13 shl 16) + (7 shl 8) + 9);
  AssertEquals('Bytes written',4,Stream.WriteData(S));
  AssertBytes([9,7,13,128]);
  Stream.Position:=0;
  AssertEquals('Bytes read',4,Stream.ReadData(D));
  AssertEquals('Written data read correctly',S,D);
end;

procedure TTestStream.TestUInt32Buffer;

Var
  S,D : UInt32;

begin
  D:=0;
  // 2148337417
  S:=UINT32((128 shl 24) + (13 shl 16) + (7 shl 8) +  9);
  Stream.WriteBufferData(S);
  AssertBytes([9,7,13,128]);
  Stream.Position:=0;
  Stream.ReadBufferData(D);
  AssertEquals('Written data read correctly',S,D);
end;

procedure TTestStream.TestInt64;

Var
  S,D : {$IFDEF ECMASCRIPT}NativeLargeInt{$else}Int64{$endif};

begin
  D:=0;
  // 9154981354848060679
  // Javascript only has 52 bits : 7737333974279
  S:={$IFNDEF ECMASCRIPT} (127 shl 24) + (13 shl 16) {$endif}+ (7 shl 8) +  9;
  S:=(S shl 32) + ((125 shl 24) + (11 shl 16) + (5 shl 8) +  7);
  AssertEquals('Bytes written',8,Stream.WriteData(S));
  {$ifndef ECMASCRIPT}
  AssertBytes([7,5,11,125,9,7,{$IFNDEF ECMASCRIPT} 13,127 {$ELSE} 0,0 {$ENDIF}]);
  {$ENDIF}
  Stream.Position:=0;
  AssertEquals('Bytes read',8,Stream.ReadData(D));
  AssertEquals('Written data read correctly',S,D);
end;

procedure TTestStream.TestInt64Neg;
Var
  S,D : {$IFDEF ECMASCRIPT}NativeLargeInt{$else}Int64{$endif};

begin
  D:=0;
  {$IFNDEF ECMASCRIPT}
  // -9219705124773231353
  S:=Int64((128 shl 24) + (13 shl 16) + (7 shl 8) +  9);
  S:=Int64((S shl 32) + ((128 shl 24) + (11 shl 16) + (5 shl 8) +  7));
  AssertEquals('Bytes written',8,Stream.WriteData(S));
  AssertBytes([7,5,11,128,9,7,13,128]);
  Stream.Position:=0;
  AssertEquals('Bytes read',8,Stream.ReadData(D));
  AssertEquals('Written data read correctly',S,D);
  {$ELSE}
  S:=-9000199254740991;
  AssertEquals('Bytes written',8,Stream.WriteData(S));
  Stream.Position:=0;
  AssertEquals('Bytes read',8,Stream.ReadData(D));
  AssertEquals('Written data read correctly',S,D);
  {$ENDIF}
end;

procedure TTestStream.TestInt64Buffer;

Var
  S,D : {$IFDEF ECMASCRIPT}NativeLargeInt{$else}Int64{$endif};

begin
  D:=0;
  // 9154981354848060679
  // 7737333974279 for ECMAScript
  S:={$IFNDEF ECMASCRIPT} (127 shl 24) + (13 shl 16) {$endif}+ (7 shl 8) +  9;
  S:=(S shl 32) + ((125 shl 24) + (11 shl 16) + (5 shl 8) +  7);
  Stream.WriteBufferData(S);
  {$IFNDEF ECMASCRIPT}
  AssertBytes([7,5,11,125,9,7,13,127]);
  {$ENDIF}
  Stream.Position:=0;
  Stream.ReadBufferData(D);
  AssertEquals('Written data read correctly',S,D);
end;

procedure TTestStream.TestBoolean;

Var
  S,D : Boolean;

begin
  D:=False;
  // 9154981354848060679
  S:=True;
  AssertEquals('Bytes written',1,Stream.WriteData(S));
  AssertBytes([1]);
  Stream.Position:=0;
  AssertEquals('Bytes read',1,Stream.ReadData(D));
  AssertEquals('Written data read correctly',S,D);
end;

procedure TTestStream.TestBooleanBuffer;
Var
  S,D : Boolean;

begin
  D:=False;
  // 9154981354848060679
  S:=True;
  Stream.WriteBufferData(S);
  AssertBytes([1]);
  Stream.Position:=0;
  Stream.ReadBufferData(D);
  AssertEquals('Written data read correctly',S,D);
end;

{$IFNDEF ECMASCRIPT}
procedure TTestStream.TestAnsiChar;

Var
  S,D : AnsiChar;

begin
  D:=#0;
  S:='A';
  AssertEquals('Bytes written',1,Stream.WriteData(S));
  AssertBytes([Ord(S)]);
  Stream.Position:=0;
  AssertEquals('Bytes read',1,Stream.ReadData(D));
  AssertEquals('Written data read correctly',S,D);
end;

procedure TTestStream.TestAnsicharBuffer;
Var
  S,D : AnsiChar;

begin
  D:=#0;
  S:='A';
  Stream.WriteBufferData(S);
  AssertBytes([Ord(S)]);
  Stream.Position:=0;
  Stream.ReadBufferData(D);
  AssertEquals('Written data read correctly',S,D);
end;
{$endif}

procedure TTestStream.TestWideChar;

Var
  S,D : WideChar;

begin
  D:=#0;
  S:='A';
  AssertEquals('Bytes written',2,Stream.WriteData(S));
  AssertBytes([Ord(S),0]);
  Stream.Position:=0;
  AssertEquals('Bytes read',2,Stream.ReadData(D));
  AssertEquals('Written data read correctly',Ord(S),Ord(D));
end;

procedure TTestStream.TestWideCharBuffer;
Var
  S,D : WideChar;

begin
  D:=#0;
  S:='A';
  Stream.WriteBufferData(S);
  AssertBytes([Ord(S),0]);
  Stream.Position:=0;
  Stream.ReadBufferData(D);
  AssertEquals('Written data read correctly',Ord(S),Ord(D));
end;

{$ifndef ECMASCRIPT}
procedure TTestStream.TestSingle;

Var
  S,D : Single;
  B : TBytes = Nil;

begin
  D:=0;
  S:=123.45;
  AssertEquals('Bytes written',4,Stream.WriteData(S));
  Setlength(B,4);
  With TSingleRec(S) do
    begin
    B[0]:=Bytes[0];
    B[1]:=Bytes[1];
    B[2]:=Bytes[2];
    B[3]:=Bytes[3];
    end;
  AssertBytes(B);
  Stream.Position:=0;
  AssertEquals('Bytes read',4,Stream.ReadData(D));
  AssertEquals('Written data read correctly',S,D,0.0001);
end;

procedure TTestStream.TestSingleBuffer;
Var
  S,D : Single;
  B : TBytes = Nil;

begin
  D:=0;
  S:=123.45;
  Stream.WriteBufferData(S);
  Setlength(B,4);
  With TSingleRec(S) do
    begin
    B[0]:=Bytes[0];
    B[1]:=Bytes[1];
    B[2]:=Bytes[2];
    B[3]:=Bytes[3];
    end;
  AssertBytes(B);
  Stream.Position:=0;
  Stream.ReadBufferData(D);
  AssertEquals('Written data read correctly',S,D,0.0001);
end;
{$endif}


procedure TTestStream.TestDouble;

Var
  S,D : Double;
  B : TBytes;
{$ifndef ECMASCRIPT}
  i : integer;
{$endif}

begin
  B:=Default(TBytes);
  D:=0;
  S:=123.45;
  AssertEquals('Bytes written',8,Stream.WriteData(S));
  Setlength(B,8);
{$ifndef ECMASCRIPT}
  With TDoubleRec(S) do
    For I:=0 to 7 do
      B[I]:=Bytes[I];
  AssertBytes(B);
{$endif}
  Stream.Position:=0;
  AssertEquals('Bytes read',8,Stream.ReadData(D));
  AssertEquals('Written data read correctly',S,D,0.0001);
end;

procedure TTestStream.TestDoubleBuffer;

Var
  S,D : Double;
  B : TBytes;
{$ifndef ECMASCRIPT}
  i : integer;
{$endif}

begin
  B:=Default(TBytes);
  D:=0;
  S:=123.45;
  Stream.WriteBufferData(S);
  Setlength(B,8);
{$ifndef ECMASCRIPT}
  With TDoubleRec(S) do
    For I:=0 to 7 do
      B[I]:=Bytes[I];
  AssertBytes(B);
{$endif}
  Stream.Position:=0;
  Stream.ReadBufferData(D);
  AssertEquals('Written data read correctly',S,D,0.0001);
end;

{$ifndef ECMASCRIPT}
procedure TTestStream.TestExtended;
Var
  S,D : Extended;
  B : TBytes = Nil;
  i : integer;

begin
  D:=0;
  S:=123.45;
  AssertEquals('Bytes written',10,Stream.WriteData(S));
  Setlength(B,10);
  With TExtended80Rec(S) do
    For I:=0 to 9 do
      B[I]:=Bytes[I];
  AssertBytes(B);
  Stream.Position:=0;
  AssertEquals('Bytes read',10,Stream.ReadData(D));
  AssertEquals('Written data read correctly',S,D,0.0001);
end;

procedure TTestStream.TestExtendedBuffer;
Var
  S,D : Extended;
  B : TBytes = Nil;
  i : integer;

begin
  D:=0;
  S:=123.45;
  Stream.WriteBufferData(S);
  Setlength(B,10);
  With TExtended80Rec(S) do
    For I:=0 to 9 do
      B[I]:=Bytes[I];
  AssertBytes(B);
  Stream.Position:=0;
  Stream.ReadBufferData(D);
  AssertEquals('Written data read correctly',S,D,0.0001);
end;
{$endif}

procedure TTestStream.SetUp;

Var
  B : TBytes;

begin
  B:=Default(TBytes);
  SetLength(B,0);
  FStream:=TBytesStream.Create(B);
end;

procedure TTestStream.TearDown;
begin
  FreeAndNil(FStream);
end;

procedure TTestStream.AssertBytes(B: array of Byte; aMessage: string = ''; ObserveEndian : Boolean = True);
Var
  L,I,E: integer;
  A : Byte;
  SB : TBytes;

begin
  if AMessage<>'' then
    aMessage:=aMessage+': ';
  AssertEquals(aMessage+'Length bytes equals',Length(B),FStream.Size);
  SB:=TBytesStream(Stream).Bytes;
  L:=Length(B);
  for I:=0 to L-1 do
     begin
     E:=Byte(B[i] and $FF);
     if ObserveEndian and (Stream.Endian=Tendian.Big) then
       A:=SB[L-1-i]
     else
       A:=SB[i];
     AssertEquals(aMessage+'Byte['+IntToStr(I)+'] equals',E,A);
     end;
end;

{
procedure TTestStream.AssertBytes(B: TBytes; aMessage : string = '');

Var
  I : integer;
  SB : TBytes;

begin
  if AMessage<>'' then
    aMessage:=aMessage+': ';
  AssertEquals(aMessage+'Length bytes equals',Length(B),FStream.Size);
  SB:=TBytesStream(Stream).Bytes;
  for I:=0 to Length(B)-1 do
     AssertEquals(aMessage+'Byte['+IntToStr(I)+'] equals',B[i],SB[i]);
end;
}
function TTestStream.CreateBytes(aCount: integer): TBytes;

Var
  I : Integer;

begin
  Result:=Nil;
  SetLength(Result,aCount);
  For I:=0 to aCount-1 do
    Result[I]:=I+1;
end;



initialization
  RegisterTests([{TTestStream,TTestBigendianStream,}TTestStringStream]);
end.

