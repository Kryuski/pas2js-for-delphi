unit arrayutils;

{$mode objfpc}
{$modeswitch typehelpers}

interface

uses
  Types, SysUtils;

Type
  TBytesHelper = type helper for TBytes
  strict private
    function get_Length: Integer;
    procedure set_Length(const AValue: Integer);
  public
    function Compare(const ARight: TBytes): Integer;
    function High: Integer;
    function Low: Integer;
    function SubBytes(const AStartIndex: Integer): TBytes; overload; inline;
    function SubBytes(const AStartIndex, ALength: Integer): TBytes; overload; inline;
    property Length: Integer read get_Length write set_Length;
  end;

  TWords = TWordDynArray;
  TWordDynArrayHelper = type helper for TWordDynArray
  private
    function get_Length: Integer;
    procedure set_Length(const AValue: Integer);
  public
    class function FromBytes(const ABytes: TBytes): TWords; static;
    function High: Integer;
    function Low: Integer;
    function ToBytes: TBytes;
    property Length: Integer read get_Length write set_Length;
  end;


implementation

Const
  GreaterThanValue = 1;
  LessThanValue = -1;
  EqualsValue = 0;

{ TBytesHelper }

function TBytesHelper.Compare(const ARight: TBytes): Integer;
var
  lCount: Integer;
begin
  if Length > ARight.Length then
    Exit(GreaterThanValue);

  if Length < ARight.Length then
    Exit(LessThanValue);

  Result := EqualsValue;
  for lCount := Low to High do
  begin
    if Self[lCount] > ARight[lCount] then
      Exit(GreaterThanValue);
    if Self[lCount] < ARight[lCount] then
      Exit(LessThanValue);
  end;
end;

function TBytesHelper.get_Length: Integer;
begin
  Result := System.Length(Self);
end;

function TBytesHelper.High: Integer;
begin
  Result := System.High(Self);
end;

function TBytesHelper.Low: Integer;
begin
  Result := System.Low(Self);
end;

procedure TBytesHelper.set_Length(const AValue: Integer);
begin
  System.SetLength(Self, AValue);
end;

function TBytesHelper.SubBytes(const AStartIndex: Integer): TBytes;
begin
  Result := System.Copy(Self, AStartIndex, Length);
end;

function TBytesHelper.SubBytes(const AStartIndex, ALength: Integer): TBytes;
begin
  Result := System.Copy(Self, AStartIndex, ALength);
end;



function WordFromBytes(const ALow, AHigh: Byte): Word;
const
  cMask: Byte = $FF;
  cBitPerByte = 8;
begin
  Result := ALow + (AHigh and (cMask shl cBitPerByte));
end;

Type
  TWordBytes = Record
    aLow, aHigh: Byte;
  end;

Function WordToBytes(aValue : Word) : TWordBytes;
const
  cMask: Byte = $FF;
  cBitPerByte = 8;
begin
  Result.aLow := aValue and cMask;
  Result.aHigh := aValue and (cMask shl cBitPerByte);
end;


class function TWordDynArrayHelper.FromBytes(const ABytes: TBytes): TWords;
const
  cBytesPerWord = 2;
var
  lCount: Integer;
  lLow: Byte;
  lHigh: Byte;
begin
  lCount := ABytes.Length;
  if Odd(lCount) then
    Inc(lCount);
  Result.Length := lCount div cBytesPerWord;

  lCount := ABytes.Low;
  while lCount <= ABytes.High do
  begin
    lLow := ABytes[lCount];
    if lCount + 1 <= ABytes.High then
      lHigh := ABytes[lCount + 1]
    else
      lHigh := 0;

    Result[lCount div cBytesPerWord] := WordFromBytes(lLow, lHigh);
    Inc(lCount, cBytesPerWord);
  end;

end;

function TWordDynArrayHelper.get_Length: Integer;
begin
  Result := System.Length(Self);
end;

function TWordDynArrayHelper.High: Integer;
begin
  Result := System.High(Self);
end;

function TWordDynArrayHelper.Low: Integer;
begin
  Result := System.Low(Self);
end;

procedure TWordDynArrayHelper.set_Length(const AValue: Integer);
begin
  System.SetLength(Self, AValue);
end;

function TWordDynArrayHelper.ToBytes: TBytes;

const
  cBytesPerWord = 2;

var
  lCount: Integer;
  lNewIndex: Integer;
  aBytes : TWordBytes;

begin
  Result.Length := Length * cBytesPerWord;
  for lCount := Low to High do
  begin
    aBytes:=WordToBytes(Self[lCount]);
    lNewIndex := lCount * cBytesPerWord;
    Result[lNewIndex] := aBytes.aLow;
    Result[lNewIndex + 1] := aBytes.aHigh;
  end;
end;


end.

