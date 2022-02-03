unit fieldmap;
{$mode objfpc}
{$H+}
interface

uses SysUtils, db;

{ ---------------------------------------------------------------------
  TFieldMap
  ---------------------------------------------------------------------}

type
  EFieldMap = Class(EDatabaseError);

  { TFieldMap }

  TFieldMap = Class(TObject)
  private
    FDataset: TDataset;
    FFreeDataset: Boolean;
    FOldOnOpen : TDataSetNotifyEvent;
  Protected
    Procedure DoOnOpen(Sender : TDataset);
    Function FindField(FN : String) : TField;
    Function FieldByName(FN : String) : TField;
  Public
    Constructor Create(ADataset : TDataset; HookOnOpen : Boolean = False);
    Destructor Destroy; override;
    Procedure InitFields; virtual; abstract;
    Procedure LoadObject(AObject : TObject); virtual;
    Function GetFromField(F : TField; ADefault : Integer) : Integer; overload;
    Function GetFromField(F : TField; ADefault : String) : String; overload;
    Function GetFromField(F : TField; ADefault : Boolean) : Boolean; overload;
    Function GetFromField(F : TField; ADefault : TDateTime) : TDateTime; overload;
    Function GetFromField(F : TField; ADefault : Currency) : Currency; overload;
    Function GetFromField(F : TField; ADefault : Double) : Double; overload;
    Property Dataset : TDataset Read FDataset;
    Property FreeDataset : Boolean Read FFreeDataset Write FFreeDataset;
  end;
  TFieldMapClass = Class of TFieldMap;

  { TParamMap }

  TParamMap = Class(TObject)
  private
    FParams: TParams;
  Protected
    Function FindParam(FN : String) : TParam;
    Function ParamByName(FN : String) : TParam;
  Public
    Constructor Create(AParams : TParams);
    Procedure InitParams; virtual; abstract;
    Procedure SaveObject(AObject : TObject); virtual; abstract;
    Property Params : TParams Read FParams;
  end;


implementation


resourcestring
  SErrNoDataset = '%s: No dataset available.';
  SErrNoParamsForParam  = '%s: No params to search param "%s".';
  SErrNoObjectToLoad = '%s: No object to load';

{ TParamMap }

function TParamMap.FindParam(FN: String): TParam;
begin
  Result:=FParams.FindParam(FN);
  {if (Result=Nil) then
    Writeln(ClassName,' param ',FN,' not found');}
end;

function TParamMap.ParamByName(FN: String): TParam;
begin
  If (FParams=Nil) then
    Raise Exception.CreateFmt(SErrNoParamsForParam,[ClassName,FN]);
  Result:=FParams.ParamByName(FN);
end;

constructor TParamMap.Create(AParams: TParams);
begin
  FParams:=AParams;
  InitParams;
end;

{ TFieldMap }

constructor TFieldMap.Create(ADataset: TDataset; HookOnOpen : Boolean = False);
begin
  if (ADataset=Nil) then
    Raise EFieldMap.CreateFmt(SErrNoDataset,[ClassName]);
  FDataset:=ADataset;
  if HookOnOpen then
    begin
    FOldOnOpen:=FDataset.AfterOpen;
    FDataset.AfterOpen:=@DoOnOpen;
    end;
  if FDataset.Active then
    InitFields;
end;

destructor TFieldMap.Destroy;
begin
  if FFreeDataset then
    FreeAndNil(FFreeDataset);
  inherited Destroy;
end;

procedure TFieldMap.LoadObject(AObject: TObject);
begin
  If (AObject=Nil) then
    Raise EFieldMap.CreateFmt(SErrNoObjectToLoad,[ClassName]);
end;

function TFieldMap.FieldByName(FN: String): TField;
begin
  Result:=FDataset.FieldByName(FN)
end;

procedure TFieldMap.DoOnOpen(Sender: TDataset);
begin
  InitFields;
  If Assigned(FOldOnOpen) then
    FOldOnOpen(Sender);
end;

function TFieldMap.FindField(FN: String): TField;
begin
  If (FDataset=Nil) then
    Result:=Nil
  else
    Result:=FDataset.FindField(FN);
end;

function TFieldMap.GetFromField(F: TField; ADefault: Integer): Integer;
begin
  If Assigned(F) then
    Result:=F.AsInteger
  else
    Result:=ADefault;
end;

function TFieldMap.GetFromField(F: TField; ADefault: String): String;
begin
  If Assigned(F) then
    Result:=F.AsString
  else
    Result:=ADefault;
end;

function TFieldMap.GetFromField(F: TField; ADefault: Boolean): Boolean;
begin
  If Assigned(F) then
    begin
    if (F is TStringField) then
      Result:=(F.AsString='+')
    else
      Result:=F.AsBoolean
    end
  else
    Result:=ADefault;
end;

function TFieldMap.GetFromField(F: TField; ADefault: TDateTime): TDateTime;
begin
  If Assigned(F) then
    Result:=F.AsDateTime
  else
    Result:=ADefault;
end;

function TFieldMap.GetFromField(F: TField; ADefault: Currency): Currency;
begin
  If Assigned(F) then
    Result:=F.AsFloat
  else
    Result:=ADefault;
end;

function TFieldMap.GetFromField(F: TField; ADefault: Double): Double;
begin
  If Assigned(F) then
    Result:=F.AsFloat
  else
    Result:=ADefault;
end;

end.
 
