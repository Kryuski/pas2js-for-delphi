unit DictHelper;

{$I delphi_defines.inc}

interface

uses
  Generics.Collections;

type
  TFPDictionary<TValue> = class(TDictionary<string,TValue>)
  private
    FArray: TArray<TPair<string,TValue>>;
    procedure EnsureArray; inline;
    procedure EmptyArray; inline;
    function GetValueOfIndex(Index: Integer): TValue;
    procedure SetValueOfIndex(Index: Integer; const Value: TValue);
  public
    function Add(const Key: string; const Value: TValue): Integer;
    function IndexOf(Value: TValue): Integer;
    function FindIndexOf(const Key: string): Integer;
    function NameOfIndex(Index: Integer): string;
    function Extract(const Value: TValue): TValue;
    procedure Delete(Index: Integer);
    procedure RemoveValue(const Value: TValue);
    property ValueOfIndex[Index: Integer]: TValue read GetValueOfIndex write SetValueOfIndex;
  end;

implementation

uses
  System.Generics.Defaults;

{ TFPDictionary<TValue> }

procedure TFPDictionary<TValue>.EnsureArray;
begin
  if Length(FArray) = 0 then
    FArray := ToArray
  else
    Assert(Length(FArray) = Count);
end;

procedure TFPDictionary<TValue>.EmptyArray;
begin
  SetLength(FArray, 0);
end;

function TFPDictionary<TValue>.Add(const Key: string; const Value: TValue): Integer;
begin
  inherited Add(Key, Value);
  EmptyArray;
  Result := FindIndexOf(Key);
end;

procedure TFPDictionary<TValue>.Delete(Index: Integer);
begin
  Remove(NameOfIndex(Index));
  EmptyArray;
end;

function TFPDictionary<TValue>.FindIndexOf(const Key: string): Integer;
begin
  EnsureArray;
  Result := Length(FArray)-1;
  while (Result >= 0) and (FArray[Result].Key <> Key) do
    Dec(Result);
end;

function TFPDictionary<TValue>.IndexOf(Value: TValue): Integer;
var
  i: Integer;
  c: IEqualityComparer<TValue>;
begin
  EnsureArray;
  c := TEqualityComparer<TValue>.Default;
  Result := Length(FArray)-1;
  while (Result >= 0) and not c.Equals(FArray[Result].Value, Value) do
    Dec(Result);
end;

function TFPDictionary<TValue>.NameOfIndex(Index: Integer): string;
begin
  Assert((Index >= 0) and (Index < Length(FArray)) and (Length(FArray) = Count));
  Result := FArray[Index].Key;
end;

function TFPDictionary<TValue>.GetValueOfIndex(Index: Integer): TValue;
begin
  Assert((Index >= 0) and (Index < Length(FArray)) and (Length(FArray) = Count));
  Result := FArray[Index].Value;
end;

procedure TFPDictionary<TValue>.SetValueOfIndex(Index: Integer;
  const Value: TValue);
begin
  Assert((Index >= 0) and (Index < Length(FArray)) and (Length(FArray) = Count));
  FArray[Index].Value := Value;
  Items[FArray[Index].Key] := Value;
end;

procedure TFPDictionary<TValue>.RemoveValue(const Value: TValue);
begin
  Extract(Value);
end;

function TFPDictionary<TValue>.Extract(const Value: TValue): TValue;
var
  c: IEqualityComparer<TValue>;
begin
  c := TEqualityComparer<TValue>.Default;
  for var pair in Self do
    if c.Equals(pair.Value, Value) then begin
      Result := Value;
      Remove(pair.Key);
      EmptyArray;
      Exit;
    end;
  Result := Default(TValue);
end;

end.
