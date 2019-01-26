{
    This file is part of the Free Component Library

    JSON source parser
    Copyright (c) 2007 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$h+}
unit jsonparser;

interface

uses
  Classes, SysUtils, fpJSON, jsonscanner, jsonreader, FPCTypes;
  
Type

  { TJSONParser }

  TJSONParser = Class(TBaseJSONReader)
  private
    FStack : Array of TJSONData;
    FStackPos : integer;
    FStruct : TJSONData;
    FValue : TJSONData;
    FKey: TJSONStringType;
    procedure Pop(aType: TJSONType);
    procedure Push(AValue : TJSONData);
    Function NewValue(AValue : TJSONData) : TJSONData;
  Protected
    procedure KeyValue(Const AKey : TJSONStringType); override;
    procedure StringValue(Const AValue : TJSONStringType);override;
    procedure NullValue; override;
    procedure FloatValue(Const AValue : Double); override;
    procedure BooleanValue(Const AValue : Boolean); override;
    procedure NumberValue(Const AValue : TJSONStringType); override;
    procedure IntegerValue(Const AValue : integer); override;
    procedure Int64Value(Const AValue : int64); override;
    procedure QWordValue(Const AValue : QWord); override;
    procedure StartArray; override;
    procedure StartObject; override;
    procedure EndArray; override;
    procedure EndObject; override;
  Public
    function Parse: TJSONData;
  end;
  
  EJSONParser = jsonReader.EJSONParser;
  
implementation

Resourcestring
  SErrStructure = 'Structural error';

{ TJSONParser }

procedure DefJSONParserHandler(AStream: TStream; const AUseUTF8: Boolean; out
  Data: TJSONData);

Var
  P : TJSONParser;
  AOptions: TJSONOptions;

begin
  Data:=Nil;
  AOptions:=[];
  if AUseUTF8 then
    Include(AOptions,joUTF8);
  P:=TJSONParser.Create(AStream,AOptions);
  try
    Data:=P.Parse;
  finally
    P.Free;
  end;
end;

procedure TJSONParser.Pop(aType: TJSONType);

begin
  if (FStackPos=0) then
    DoError(SErrStructure);
  If (FStruct.JSONType<>aType) then
    DoError(SErrStructure);
  Dec(FStackPos);
  FStruct:=FStack[FStackPos];
end;

procedure TJSONParser.Push(AValue: TJSONData);

begin
  if (FStackPos=Length(FStack)) then
    SetLength(FStack,FStackPos+10);
  FStack[FStackPos]:=FStruct;
  Inc(FStackPos);
  FStruct:=AValue;
end;

function TJSONParser.NewValue(AValue: TJSONData): TJSONData;
begin
  Result:=AValue;
  // Add to existing structural type
  if (FStruct is TJSONObject) then
    begin
    TJSONObject(FStruct).Add(FKey,AValue);
    FKey:='';
    end
  else if (FStruct is TJSONArray) then
    TJSONArray(FStruct).Add(AValue);
  // The first actual value is our result
  if (FValue=Nil) then
    FValue:=AValue;
end;

procedure TJSONParser.KeyValue(const AKey: TJSONStringType);
begin
  if (FStruct is TJSONObject) and (FKey='') then
    FKey:=Akey
  else
    DoError('Duplicatekey or no object');
end;

procedure TJSONParser.StringValue(const AValue: TJSONStringType);
begin
  NewValue(CreateJSON(AValue));
end;

procedure TJSONParser.NullValue;
begin
  NewValue(CreateJSON);
end;

procedure TJSONParser.FloatValue(const AValue: Double);
begin
  NewValue(CreateJSON(AValue));
end;

procedure TJSONParser.BooleanValue(const AValue: Boolean);
begin
  NewValue(CreateJSON(AValue));
end;

procedure TJSONParser.NumberValue(const AValue: TJSONStringType);
begin
  // Do nothing
  if AValue='' then ;
end;

procedure TJSONParser.IntegerValue(const AValue: integer);
begin
  NewValue(CreateJSON(AValue));
end;

procedure TJSONParser.Int64Value(const AValue: int64);
begin
  NewValue(CreateJSON(AValue));
end;

procedure TJSONParser.QWordValue(const AValue: QWord);
begin
  NewValue(CreateJSON(AValue));
end;

procedure TJSONParser.StartArray;
begin
  Push(NewValue(CreateJSONArray([])))
end;


procedure TJSONParser.StartObject;
begin
  Push(NewValue(CreateJSONObject([])));
end;

procedure TJSONParser.EndArray;
begin
  Pop(jtArray);
end;

procedure TJSONParser.EndObject;
begin
  Pop(jtObject);
end;


function TJSONParser.Parse: TJSONData;

begin
  SetLength(FStack,0);
  FStackPos:=0;
  FValue:=Nil;
  FStruct:=Nil;
  try
    DoExecute;
    Result:=FValue;
  except
    On E : exception do
      begin
      FreeAndNil(FValue);
      FStackPos:=0;
      SetLength(FStack,0);
      Raise;
      end;
  end;
end;

{
  Consume next token and convert to JSON data structure.
  If AtCurrent is true, the current token is used. If false,
  a token is gotten from the scanner.
  If AllowEOF is false, encountering a tkEOF will result in an exception.
}


procedure InitJSONHandler;
begin
  if not Assigned(@GetJSONParserHandler) then
    SetJSONParserHandler(DefJSONParserHandler);
end;

procedure DoneJSONHandler;
begin
  if @GetJSONParserHandler = @DefJSONParserHandler then
    SetJSONParserHandler(nil);
end;

initialization
  InitJSONHandler;
finalization
  DoneJSONHandler;
end.

