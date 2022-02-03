{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019-Now by Michael Van Canneyt, member of the
    Free Pascal development team

    WEB Widget Set : DB-AWare bare HTML Widgets

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit dbhtmlwidgets;

{$mode objfpc}

interface

uses
  Classes, SysUtils, db, web, htmlwidgets;

Type

  { TCustomDBTableWidget }

  { TDBTableColumn }

  TDBTableColumn = class(TCustomTableColumn)
  private
    FFieldName: String;
    FTemplate: String;
    procedure SetFieldName(AValue: String);
  protected
    Function GetCaption : String; Override;
  Public
    Procedure Assign(Source : TPersistent); override;
  Published
    Property FieldName : String Read FFieldName Write SetFieldName;
    Property Template : String Read FTemplate Write FTemplate;
  end;

  { TDBTableColumns }

  TDBTableColumns = Class(TCustomTableColumns)
  private
    function GetCol(Index : Integer): TDBTableColumn;
    procedure SetCol(Index : Integer; AValue: TDBTableColumn);
  Public
    Function AddField(F : TField) : TDBTableColumn;
    Function AddField(const AFieldName,aCaption : String) : TDBTableColumn;
    Property DBColumns[Index : Integer] : TDBTableColumn Read GetCol Write SetCol; default;
  end;

   { TDBTableRowEnumerator }
   TDBTableRowEnumerator = class(TTableRowEnumerator)
     FBOf: boolean;
     FDataset : TDataset;
     FColFields : Array of TField;
     FRowKeyField : TField;
   private
     function ReplaceTemplate(aTemplate: string; aField: TField): String;
   Protected
     Procedure SetDataset(aDataset : TDataset); virtual;
   Public
     Procedure GetCellData(aCell: TTableWidgetCellData); override;
     Function MoveNext: Boolean; override;
     Property Dataset : TDataset Read FDataset;
   end;

  TCustomDBTableWidget = Class(TCustomTableWidget)
  private
    FDatasource: TDatasource;
    FRowKeyField: String;
    function GetColumns: TDBTableColumns;
    procedure SetColumns(AValue: TDBTableColumns);
    procedure SetDatasource(AValue: TDatasource);
    procedure SetRowKeyField(AValue: String);
  Protected
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RenderRow(aEnum : TTableRowEnumerator; aParent: TJSHTMLElement; aKind: TRowKind; aCell: TTableWidgetCellData);override;
    Procedure CreateDefaultColumns; override;
    Function CreateColumns: TCustomTableColumns; override;
    function GetDataset: TDataset;
    Function GetBodyRowEnumerator: TTableRowEnumerator; override;
    Property Datasource : TDatasource Read FDatasource write SetDatasource;
    Property RowKeyField : String Read FRowKeyField Write SetRowKeyField;
  Public
    Property Dataset : TDataset Read GetDataset;
    Property Columns : TDBTableColumns Read GetColumns Write SetColumns;
  end;

  TDBTableWidget = class(TCustomDBTableWidget)
  Public
    property Element;
  Published
    Property Classes;
    Property TableOptions;
    Property ParentID ;
    Property Datasource;
    Property Columns;
    Property OnGetCellData;
    Property OnCellClick;
    Property OnHeaderCellClick;
    Property OnFooterCellClick;
    Property OnRowClick;
    Property OnHeaderRowClick;
    Property OnFooterRowClick;
    Property RowKeyField;
  end;

  // Select that gets the values from a dataset.

  { TCustomDBSelectWidget }

  TCustomDBSelectWidget = class(TCustomSelectWidget)
  Private
    FDatasource : TDatasource;
    FItemField: String;
    FNullIsNotValue: Boolean;
    FValueField: String;
    function GetDataset: TDataset;
    function GetValue: String;
    procedure SetDatasource(AValue: TDatasource);
    procedure SetItemField(AValue: String);
    procedure SetNullIsNotValue(AValue: Boolean);
    procedure SetValueField(AValue: String);
  Protected
    Type
    { TStringsSelectOptionEnumerator }
    TDBSelectOptionEnumerator = Class(TSelectOptionEnumerator)
      FBof : Boolean;
      FDS :  TDataset;
      FTextField : TField;
      FValueField : TField;
      FCheckValue : Boolean;
      constructor Create(ASelect : TCustomSelectWidget); override;
      Function OptionText : String; override;
      Function HasValue : boolean; override;
      Function Value : string; override;
      function MoveNext: Boolean; override;
      Property Dataset : TDataset Read FDS;
    end;
    Function CreateOptionEnumerator: TSelectOptionEnumerator; override;
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  Protected
    Property Datasource : TDatasource Read FDatasource write SetDatasource;
    Property ItemField : String Read FItemField Write SetItemField;
    Property ValueField : String Read FValueField Write SetValueField;
    Property NullIsNotValue : Boolean Read FNullIsNotValue Write SetNullIsNotValue;
    Property Value : String Read GetValue;
  Public
    Property Dataset : TDataset Read GetDataset;
  end;

  { TDBSelectWidget }

  TDBSelectWidget = Class(TCustomDBSelectWidget)
  Public
    Property SelectionCount;
    Property SelectionValue;
    Property SelectionItem;
    Property Selected;
    Property Value;
    Property Options;
    Property SelectElement;
    Property ItemCount;
  Published
    Property Datasource;
    Property ItemField;
    Property ValueField;
    Property NullIsNotValue;
    property SelectedIndex;
    Property Multiple;
  end;

implementation

{ TCustomDBSelectWidget.TDBSelectOptionEnumerator }

constructor TCustomDBSelectWidget.TDBSelectOptionEnumerator.Create(ASelect: TCustomSelectWidget);

Var
  S : TCustomDBSelectWidget;

begin
  inherited Create(ASelect);
  FBOF:=True;
  S:=aSelect as TCustomDBSelectWidget;
  FDS:=S.Dataset;
  if FDS=Nil then
    exit;
  FTextField:=FDS.Fields.FindField(S.ItemField);
  if S.ValueField<>'' then
    FValueField:=FDS.Fields.FindField(S.ValueField);
  FCheckValue:=S.NullIsNotValue;
end;

function TCustomDBSelectWidget.TDBSelectOptionEnumerator.OptionText: String;
begin
  Result:=FTextField.DisplayText;
end;

function TCustomDBSelectWidget.TDBSelectOptionEnumerator.HasValue: boolean;
begin
  Result:=Assigned(FValueField);
  if Result and FCheckValue then
    Result:=Not FValueField.IsNull;
end;

function TCustomDBSelectWidget.TDBSelectOptionEnumerator.Value: string;
begin
  Result:=FValueField.DisplayText;
end;

function TCustomDBSelectWidget.TDBSelectOptionEnumerator.MoveNext: Boolean;

begin
  If not Assigned(Dataset) then
    exit(False);
  if FBOF then
    FBof:=False
  else
    Dataset.Next;
  Result:=Not Dataset.EOF;
end;

{ TCustomDBSelectWidget }

function TCustomDBSelectWidget.GetDataset: TDataset;
begin
  if Assigned(Datasource) then
    Result:=Datasource.Dataset
  else
    Result:=nil;
end;

function TCustomDBSelectWidget.GetValue: String;
begin
  Result:=TJSHTMLSelectElement(Element).Value;
end;

procedure TCustomDBSelectWidget.SetDatasource(AValue: TDatasource);
begin
  if FDatasource=AValue then Exit;
  if Assigned(FDatasource) then
    FDatasource.RemoveFreeNotification(Self);
  FDatasource:=AValue;
  if Assigned(FDatasource) then
    FDatasource.FreeNotification(Self);
end;

procedure TCustomDBSelectWidget.SetItemField(AValue: String);
begin
  if FItemField=AValue then Exit;
  FItemField:=AValue;
end;

procedure TCustomDBSelectWidget.SetNullIsNotValue(AValue: Boolean);
begin
  if FNullIsNotValue=AValue then Exit;
  FNullIsNotValue:=AValue;
end;

procedure TCustomDBSelectWidget.SetValueField(AValue: String);
begin
  if FValueField=AValue then Exit;
  FValueField:=AValue;
end;

function TCustomDBSelectWidget.CreateOptionEnumerator: TSelectOptionEnumerator;
begin
  Result:=TDBSelectOptionEnumerator.Create(Self);
end;

procedure TCustomDBSelectWidget.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent=FDatasource) then
    FDataSource:=Nil;
end;

{ TDBTableColumn }

procedure TDBTableColumn.SetFieldName(AValue: String);
begin
  if FFieldName=AValue then Exit;
  FFieldName:=AValue;
end;


function TDBTableColumn.GetCaption: String;
begin
  Result:=inherited GetCaption;
  if Result='' then
    Result:=FieldName;
end;

procedure TDBTableColumn.Assign(Source: TPersistent);
begin
  if Source is TDBTableColumn then
    FieldName:=TDBTableColumn(Source).FieldName;
  inherited Assign(Source);
end;

{ TDBTableColumns }

function TDBTableColumns.GetCol(Index : Integer): TDBTableColumn;
begin
  Result:=TDBTableColumn(Items[Index])
end;

procedure TDBTableColumns.SetCol(Index : Integer; AValue: TDBTableColumn);
begin
  Items[Index]:=AValue;
end;

function TDBTableColumns.AddField(F: TField): TDBTableColumn;
begin
  Result:=AddField(F.FieldName,F.DisplayLabel);
end;

function TDBTableColumns.AddField(const AFieldName, aCaption : String): TDBTableColumn;
begin
  Result:=(Add as TDBtableColumn);
  Result.FieldName:=aFieldName;
  Result.Caption:=aCaption;
end;

{ TDBTableRowEnumerator }

procedure TDBTableRowEnumerator.SetDataset(aDataset : TDataset);

Var
  T : TCustomDBTableWidget;
  I : Integer;

begin
  FBof:=True;
  FDataset:=aDataset;
  if Table is TCustomDBTableWidget then
    begin
    T:=Table as TCustomDBTableWidget;
    SetLength(FColFields,T.Columns.Count-1);
    For I:=0 to T.Columns.Count-1 do
      FColFields[I]:=Dataset.Fields.FindField(T.Columns[i].FieldName);
    if (T.RowKeyField<>'') then
      FRowKeyField:=Dataset.Fields.FindField(T.RowKeyField);
   end;
end;

Function TDBTableRowEnumerator.ReplaceTemplate(aTemplate : string; aField : TField) : String;

Var
  I : Integer;

begin
  Result:=aTemplate;
  if (aField<>Nil) then
    Result:=StringReplace(Result,'{{value}}',aField.AsString,[rfReplaceAll,rfIgnoreCase]);
  for I:=0 to Dataset.Fields.Count-1 do
    With Dataset.Fields[i] do
      Result:=StringReplace(Result,'{{'+FieldName+'}}',AsString,[rfReplaceAll,rfIgnoreCase]);
end;

procedure TDBTableRowEnumerator.GetCellData(aCell: TTableWidgetCellData);

Var
  F : TField;
  CC : TDBTableColumn;
begin
  if (aCell.Kind=rkBody) then
    begin
    F:=FColFields[ACell.Col];
    if aCell.Column is TDBTableColumn then
      CC:=TDBTableColumn(aCell.Column)
    else
      CC:=Nil;
    if Assigned(CC) and (CC.Template<>'') then
      begin
      aCell.Text:=replaceTemplate(CC.Template,F);
      aCell.asHTML:=True;
      end
    else if Assigned(F) then
      ACell.Text:=F.AsString;
    end
  else
    inherited GetCellData(aCell);
end;

function TDBTableRowEnumerator.MoveNext: Boolean;

begin
  if FBOF then
    FBof:=False
  else
    Dataset.Next;
  Result:=Not Dataset.EOF;
  if Result then
    Result:=inherited MoveNext; // Update row number
end;

{ TCustomDBTableWidget }

procedure TCustomDBTableWidget.SetDatasource(AValue: TDatasource);
begin
  if FDatasource=AValue then Exit;
  if Assigned(FDatasource) then
    FDatasource.RemoveFreeNotification(Self);
  FDatasource:=AValue;
  if Assigned(FDatasource) then
    FDatasource.FreeNotification(Self);
end;

procedure TCustomDBTableWidget.SetRowKeyField(AValue: String);
begin
  if FRowKeyField=AValue then Exit;
  FRowKeyField:=AValue;
  if IsRendered then
    Refresh;
end;

procedure TCustomDBTableWidget.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent=FDatasource) then
    FDataSource:=Nil;
end;

procedure TCustomDBTableWidget.RenderRow(aEnum: TTableRowEnumerator; aParent: TJSHTMLElement; aKind: TRowKind;
  aCell: TTableWidgetCellData);
begin
  With TDBTableRowEnumerator(aEnum) do
    begin
    if Assigned(FRowKeyField) then
      aParent.dataset['key']:=FRowKeyField.AsString;
    end;
  inherited RenderRow(aEnum, aParent, aKind, aCell);
end;

function TCustomDBTableWidget.GetColumns: TDBTableColumns;
begin
  Result:=CustomColumns as TDBTableColumns;
end;

procedure TCustomDBTableWidget.SetColumns(AValue: TDBTableColumns);
begin
  Customcolumns.Assign(AValue);
end;

procedure TCustomDBTableWidget.CreateDefaultColumns;

Var
  I : Integer;

begin
  With Dataset.Fields do
    For I:=0 to Count-1 do
      if Fields[i].Visible then
        Columns.AddField(Fields[i]);
end;

function TCustomDBTableWidget.CreateColumns: TCustomTableColumns;
begin
  Result:=TDBTableColumns.Create(TDBTableColumn);
end;

function TCustomDBTableWidget.GetDataset: TDataset;
begin
  Result:=Datasource.Dataset;
end;

function TCustomDBTableWidget.GetBodyRowEnumerator: TTableRowEnumerator;

begin
  Result:=TDBTableRowEnumerator.Create(Self);
  TDBTableRowEnumerator(Result).SetDataset(Dataset);

end;

end.

