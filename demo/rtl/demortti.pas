program DemoRTTI;

uses SysUtils, TypInfo;

type

  { TShrimp }

  TShrimp = class
  private
    FSize: integer;
    procedure SetSize(const AValue: integer);
  published
    FName: String;
    function GetSize: integer;
    procedure SetName(const AValue: String);
    property Name: String read FName write SetName;
    property Size: integer read GetSize write SetSize;
  end;

{ TShrimp }

function TShrimp.GetSize: integer;
begin
  Result:=FSize;
end;

procedure TShrimp.SetSize(const AValue: integer);
begin
  FSize:=AValue;
end;

procedure TShrimp.SetName(const AValue: String);
begin
  if FName=AValue then Exit;
  FName:=AValue;
end;

procedure ShowFields(TI: TTypeInfoStruct);
var
  i: Integer;
  f: TTypeMemberField;
begin
  writeln('ShowFields of ',TI.Name);
  for i:=0 to TI.FieldCount-1 do
  begin
    f:=TTypeMemberField(TI.Members[TI.Fields[i]]);
    writeln('  Name="',f.Name,'" Kind=',str(f.Kind),
      ' Type=',f.TypeInfo.Name,' TypeKind=',str(f.TypeInfo.Kind));
  end;
end;

function ProcSignatureToStr(Sig: TProcedureSignature): String;
var
  i: Integer;
  Param: TProcedureParam;
begin
  Result:='(';
  for i:=0 to length(Sig.Params)-1 do
  begin
    Param:=Sig.Params[i];
    if i>0 then Result:=Result+';';
    Result:=Result+Param.Name;
    Result:=Result+':'+Param.TypeInfo.Name;
  end;
  Result:=Result+')';
  if Sig.ResultType<>nil then
    Result:=Result+':'+Sig.ResultType.Name+'/'+str(Sig.ResultType.Kind);
end;

procedure ShowMethods(TI: TTypeInfoStruct);
var
  i: Integer;
  m: TTypeMemberMethod;
begin
  writeln('ShowMethods of ',TI.Name);
  for i:=0 to TI.MethodCount-1 do
  begin
    m:=TTypeMemberMethod(TI.Members[TI.Methods[i]]);
    writeln('  Name="',m.Name,'" Kind=',str(m.Kind),
      ' MethodKind=',str(m.MethodKind),' Sig=',ProcSignatureToStr(m.ProcSig));
  end;
end;

procedure ShowProperties(Instance: TObject);
var
  i: Integer;
  PropInfo: TTypeMemberProperty;
  Value: String;
  TI: TTypeInfoClass;
begin
  TI:=TypeInfo(Instance);
  writeln('ShowProperties of ',TI.Name);
  for i:=0 to TI.PropCount-1 do
  begin
    PropInfo:=TTypeMemberProperty(TI.Members[TI.Properties[i]]);
    Value:='?';
    if PropInfo.Getter<>'' then
    begin
      case PropInfo.TypeInfo.Kind of
      tkInteger: Value:=IntToStr(GetNativeIntProp(Instance,PropInfo));
      tkString: Value:=GetStrProp(Instance,PropInfo);
      end;
    end;
    writeln('  Name="',PropInfo.Name,'"',
      ' Kind=',str(PropInfo.Kind),
      ' Type=',PropInfo.TypeInfo.Name,
      ' TypeKind=',str(PropInfo.TypeInfo.Kind),
      ' Value=',Value);
  end;
end;

procedure ShowObjectRTTI(Obj: TObject);
var
  ObjTI: TTypeInfoClass;
begin
  ObjTI:=TypeInfo(Obj);
  ShowFields(ObjTI);
  ShowMethods(ObjTI);
  ShowProperties(Obj);
end;

var
  Shrimp: TShrimp;
begin
  Shrimp:=TShrimp.Create;
  Shrimp.Name:='Shrimpy';
  Shrimp.Size:=42;
  ShowObjectRTTI(Shrimp);

  writeln('Setting Size to 137:');
  SetNativeIntProp(Shrimp,'size',137);
  writeln('Size=',GetNativeIntProp(Shrimp,'size'));
end.

