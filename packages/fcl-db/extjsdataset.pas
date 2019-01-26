unit ExtJSDataset;

{$mode objfpc}

interface

uses
  Classes, SysUtils, db, JS, jsondataset;

type
  { TExtJSJSONDataSet }

  // Base for ExtJS datasets. It handles MetaData conversion.
  TExtJSJSONDataSet = Class(TBaseJSONDataset)
  Private
    FFields : TJSArray;
    FIDField: String;
    FRoot: String;
  Protected
    // Data proxy support
    Procedure InternalOpen; override;
    function DoResolveRecordUpdate(anUpdate: TRecordUpdateDescriptor): Boolean; override;
    Function DataPacketReceived(ARequest: TDataRequest) : Boolean; override;
    Function GenerateMetaData : TJSObject;
    function ConvertDateFormat(S: String): String; virtual;
    Procedure MetaDataToFieldDefs; override;
    procedure InitDateTimeFields; override;
    function StringToFieldType(S: String): TFieldType;virtual;
    function GetStringFieldLength(F: TJSObject; AName: String; AIndex: Integer): integer; virtual;
  Public
    Constructor Create(AOwner : TComponent); override;
    // Can be set directly if the dataset is closed.
    Property MetaData;
    // Can be set directly if the dataset is closed. If metadata is set, it must match the data.
    Property Rows;
    // Root of data array in data packet
    property Root : String Read FRoot Write FRoot;
    // property IDField
    property IDField : String Read FIDField Write FIDField;
  published
    Property FieldDefs;
    Property Indexes;
    Property ActiveIndex;
    // redeclared data set properties
    property Active;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

  { TExtJSJSONObjectDataSet }
  // Use this dataset for data where the data is an array of objects.
  TExtJSJSONObjectDataSet = Class(TExtJSJSONDataSet)
  Protected
    Function CreateFieldMapper : TJSONFieldMapper; override;
  end;

  { TExtJSJSONArrayDataSet }
  // Use this dataset for data where the data is an array of arrays.
  TExtJSJSONArrayDataSet = Class(TExtJSJSONDataSet)
  Protected
    Function CreateFieldMapper : TJSONFieldMapper; override;
  end;

implementation

{ TExtJSJSONDataSet }

function TExtJSJSONDataSet.StringToFieldType(S: String): TFieldType;

begin
  if (s='int') then
    Result:=ftLargeInt
  else if (s='float') then
    Result:=ftFloat
  else if (s='boolean') then
    Result:=ftBoolean
  else if (s='date') then
    Result:=ftDateTime
  else if (s='string') or (s='auto') or (s='') then
    Result:=ftString
  else
    if MapUnknownToStringType then
      Result:=ftString
    else
      Raise EJSONDataset.CreateFmt('Unknown JSON data type : %s',[s]);
end;

function TExtJSJSONDataSet.GetStringFieldLength(F: TJSObject; AName: String;
  AIndex: Integer): integer;

Var
  I,L : Integer;
  D : JSValue;

begin
  Result:=0;
  D:=F.Properties['maxlen'];
  if Not jsIsNan(toNumber(D)) then
    begin
    Result:=Trunc(toNumber(D));
    if (Result<=0) then
      Raise EJSONDataset.CreateFmt('Invalid maximum length specifier for field %s',[AName])
    end
  else
    begin
    For I:=0 to Rows.Length-1 do
      begin
      D:=FieldMapper.GetJSONDataForField(Aname,AIndex,Rows[i]);
      if isString(D) then
        begin
        l:=Length(String(D));
        if L>Result then
          Result:=L;
        end;
      end;
    end;
  if (Result=0) then
    Result:=20;
end;

constructor TExtJSJSONDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  UseDateTimeFormatFields:=True;
end;

procedure TExtJSJSONDataSet.MetaDataToFieldDefs;

Var
  A : TJSArray;
  F : TJSObject;
  I,FS : Integer;
  N: String;
  ft: TFieldType;
  D : JSValue;

begin
  FieldDefs.Clear;
  D:=Metadata.Properties['fields'];
  if Not IsArray(D) then
    Raise EJSONDataset.Create('Invalid metadata object');
  A:=TJSArray(D);
  For I:=0 to A.Length-1 do
    begin
    If Not isObject(A[i]) then
      Raise EJSONDataset.CreateFmt('Field definition %d in metadata is not an object',[i]);
    F:=TJSObject(A[i]);
    D:=F.Properties['name'];
    If Not isString(D) then
      Raise EJSONDataset.CreateFmt('Field definition %d in has no or invalid name property',[i]);
    N:=String(D);
    D:=F.Properties['type'];
    If IsNull(D) or isUndefined(D) then
      ft:=ftstring
    else If Not isString(D) then
      begin
      Raise EJSONDataset.CreateFmt('Field definition %d in has invalid type property',[i])
      end
    else
      begin
      ft:=StringToFieldType(String(D));
      end;
    if (ft=ftString) then
      fs:=GetStringFieldLength(F,N,I)
    else
      fs:=0;
    FieldDefs.Add(N,ft,fs);
    end;
  FFields:=A;
end;

procedure TExtJSJSONDataSet.InternalOpen;

Var
  I : integer;

begin
  inherited InternalOpen;
  for I:=0 to Fields.Count-1 do
    If SameText(Fields[i].FieldName,IDField) then
      Fields[i].ProviderFlags:=Fields[i].ProviderFlags+[pfInKey];
end;

function TExtJSJSONDataSet.DoResolveRecordUpdate(anUpdate: TRecordUpdateDescriptor): Boolean;

Var
  D : JSValue;
  O : TJSObject;
  A : TJSArray;
  I,RecordIndex : Integer;
  FN : String;

begin
  Result:=True;
  if anUpdate.OriginalStatus=usDeleted then
    exit;
  D:=anUpdate.ServerData;
  If isNull(D) then
    exit;
  if not isNumber(AnUpdate.Bookmark.Data) then
    exit(False);
  RecordIndex:=Integer(AnUpdate.Bookmark.Data);
  If isString(D) then
    O:=TJSOBject(TJSJSON.Parse(String(D)))
  else if isObject(D) then
    O:=TJSOBject(D)
  else
    Exit(False);
  if Not isArray(O[Root]) then
    exit(False);
  A:=TJSArray(O[Root]);
  If A.Length=1 then
    begin
    O:=TJSObject(A[0]);
    For I:=0 to Fields.Count-1 do
      begin
      if O.hasOwnProperty(Fields[i].FieldName) then
        FieldMapper.SetJSONDataForField(Fields[i],Rows[RecordIndex],O[FN]);
      end;
    end;
end;

function TExtJSJSONDataSet.DataPacketReceived(ARequest: TDataRequest): Boolean;

Var
  O : TJSObject;
  A : TJSArray;

begin
  Result:=False;
  If isNull(aRequest.Data) then
    exit;
  If isString(aRequest.Data) then
    O:=TJSOBject(TJSJSON.Parse(String(aRequest.Data)))
  else if isObject(aRequest.Data) then
    O:=TJSOBject(aRequest.Data)
  else
    DatabaseError('Cannot handle data packet');
  if (Root='') then
    root:='rows';
  if (IDField='') then
    idField:='id';
  if O.hasOwnProperty('metaData') and isObject(o['metaData']) then
    begin
    if not Active then // Load fields from metadata
      metaData:=TJSObject(o['metaData']);
    // We must always check this one...
    if metaData.hasOwnProperty('root') and isString(metaData['root']) then
      Root:=string(metaData['root']);
    if metaData.hasOwnProperty('idField') and isString(metaData['idField']) then
      IDField:=string(metaData['idField']);
    end;
  if O.hasOwnProperty(Root) and isArray(o[Root]) then
    begin
    A:=TJSArray(o[Root]);
    Result:=A.Length>0;
    AddToRows(A);
    end;
end;

function TExtJSJSONDataSet.GenerateMetaData: TJSObject;

Var
  F : TJSArray;
  O : TJSObject;
  I,M : Integer;
  T : STring;

begin
  Result:=TJSObject.New;
  F:=TJSArray.New;
  Result.Properties['fields']:=F;
  For I:=0 to FieldDefs.Count -1 do
    begin
    O:=New(['name',FieldDefs[i].name]);
    F.push(O);
    M:=0;
    case FieldDefs[i].DataType of
      ftfixedchar,
      ftString:
        begin
        T:='string';
        M:=FieldDefs[i].Size;
        end;
      ftBoolean: T:='boolean';
      ftDate,
      ftTime,
      ftDateTime: T:='date';
      ftFloat: t:='float';
      ftInteger,
      ftAutoInc,
      ftLargeInt : t:='int';
    else
      Raise EJSONDataset.CreateFmt('Unsupported field type : %s',[Ord(FieldDefs[i].DataType)]);
    end; // case
    O.Properties['type']:=t;
    if M<>0 then
      O.Properties['maxlen']:=M;
    end;
  Result.Properties['root']:='rows';
end;

function TExtJSJSONDataSet.ConvertDateFormat(S: String): String;

{ Not handled: N S w z W t L o O P T Z c U MS }

begin
  Result:=StringReplace(S,'y','yy',[rfReplaceall]);
  Result:=StringReplace(Result,'Y','yyyy',[rfReplaceall]);
  Result:=StringReplace(Result,'g','h',[rfReplaceall]);
  Result:=StringReplace(Result,'G','hh',[rfReplaceall]);
  Result:=StringReplace(Result,'F','mmmm',[rfReplaceall]);
  Result:=StringReplace(Result,'M','mmm',[rfReplaceall]);
  Result:=StringReplace(Result,'n','m',[rfReplaceall]);
  Result:=StringReplace(Result,'D','ddd',[rfReplaceall]);
  Result:=StringReplace(Result,'j','d',[rfReplaceall]);
  Result:=StringReplace(Result,'l','dddd',[rfReplaceall]);
  Result:=StringReplace(Result,'i','nn',[rfReplaceall]);
  Result:=StringReplace(Result,'u','zzz',[rfReplaceall]);
  Result:=StringReplace(Result,'a','am/pm',[rfReplaceall,rfIgnoreCase]);
  Result:=LowerCase(Result);
end;

procedure TExtJSJSONDataSet.InitDateTimeFields;

Var
  F : TJSObject;
  FF : TField;
  I: Integer;
  Fmt : String;
  D : JSValue;

begin
  If (FFields=Nil) then
    Exit;
  For I:=0 to FFields.Length-1 do
    begin
    F:=TJSObject(FFields[i]);
    D:=F.Properties['type'];
    if isString(D) and (String(D)='date') then
      begin
      D:=F.Properties['dateFormat'];
      if isString(D) then
         begin
         FMT:=ConvertDateFormat(String(D));
         FF:=FindField(String(F.Properties['name']));
         if (FF<>Nil) and (FF.DataType in [ftDate,ftTime,ftDateTime]) and (FF.FieldKind=fkData) then
           begin
           if FF is TJSONDateField then
             TJSONDateField(FF).DateFormat:=Fmt
           else if FF is TJSONTimeField then
             TJSONTimeField(FF).TimeFormat:=Fmt
           else if FF is TJSONDateTimeField then
             TJSONDateTimeField(FF).DateTimeFormat:=Fmt;
           end;
         end;
      end;
    end;
end;

{ TExtJSJSONArrayDataSet }

function TExtJSJSONArrayDataSet.CreateFieldMapper: TJSONFieldMapper;
begin
  Result:=TJSONArrayFieldMapper.Create;
end;

{ TExtJSJSONObjectDataSet }

function TExtJSJSONObjectDataSet.CreateFieldMapper: TJSONFieldMapper;
begin
  Result:=TJSONObjectFieldMapper.Create;
end;

end.

