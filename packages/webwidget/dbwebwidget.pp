{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019-Now by Michael Van Canneyt, member of the
    Free Pascal development team

    WEB Widget Set - DB-Aware widgets

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit dbwebwidget;


{$mode objfpc}

interface

uses
  Classes, webwidget, db;

Type

  { TCustomDBLoopTemplateWidget }
  TDBFieldValueData = Class(TLoopTemplateValue)
    Field : TField;
  end;
  TGetFieldValueEvent = Procedure (Sender : TObject; aData : TDBFieldValueData) of object;

  TCustomDBLoopTemplateWidget = Class(TCustomLoopTemplateWidget)
  private
    FDatasource: TDatasource;
    FOnFormatField: TGetFieldValueEvent;
    function GetDataset: TDataset;
    procedure SetDatasource(AValue: TDatasource);
  Protected
    Type
      TDBLoopEnumerator = Class(TLoopEnumerator)
      private
        FBof : Boolean;
        FDataset :  TDataset;
      public
        Procedure SetDataset(aDataset : TDataset);
        Function GetValue(Const aName : String): String; override;
        function MoveNext: Boolean; override;
        Property Dataset : TDataset Read FDataset;
      end;
  Protected
    function FormatField(aEnum : TDBLoopEnumerator; aField: TField): String;
    Function CreateLoopEnumerator (aCurrValues : TLoopTemplateValue) : TLoopEnumerator; override;
    class Function CreateCurrValues: TLoopTemplateValue; override;
  Protected
    Procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    Property Datasource : TDatasource Read FDatasource Write SetDatasource;
    Property OnFormatField : TGetFieldValueEvent Read FOnFormatField Write FOnFormatField;
  Public
    Property Dataset : TDataset Read GetDataset;
  end;

  TDBLoopTemplateWidget = Class(TCustomDBLoopTemplateWidget)
  Published
    Property HeaderTemplate;
    Property ItemTemplate;
    Property FooterTemplate;
    Property OnGetValue;
    Property References;
    Property Datasource;
    Property OnFormatField;
  end;

implementation

{ TCustomDBLoopTemplateWidget.TDBLoopEnumerator }

procedure TCustomDBLoopTemplateWidget.TDBLoopEnumerator.SetDataset(aDataset: TDataset);
begin
  FDataset:=aDataset;
  FBOF:=True;
end;

function TCustomDBLoopTemplateWidget.TDBLoopEnumerator.GetValue(const aName: String): String;

Var
  F : TField;

begin
  F:=Dataset.Fields.FindField(aName);
  if Assigned(F) then
    Result:=TCustomDBLoopTemplateWidget(Widget).FormatField(Self,F)
  else
    Result:=inherited GetValue(aName);
end;

function TCustomDBLoopTemplateWidget.FormatField(aEnum : TDBLoopEnumerator; aField : TField) : String;

Var
  F : TDBFieldValueData;

begin
  if Assigned(FOnFormatField) then
    begin
    F:=TDBFieldValueData(aEnum.CurrValues);
    F.Field:=aField;
    F.Value:=aField.AsString;
    FOnFormatField(Self,F);
    Result:=F.Value;
    end
  else
    Result:=aField.AsString;
end;

function TCustomDBLoopTemplateWidget.TDBLoopEnumerator.MoveNext: Boolean;
begin
  If not Assigned(Dataset) then
    exit(False);
  if FBOF then
    FBof:=False
  else
    Dataset.Next;
  Result:=Not Dataset.EOF;
  if Result then
    Result:=inherited MoveNext; // Update index;
end;

{ TCustomDBLoopTemplateWidget }

function TCustomDBLoopTemplateWidget.GetDataset: TDataset;
begin
  if Assigned(FDatasource) then
    Result:=FDatasource.Dataset
  else
    Result:=Nil;
end;

procedure TCustomDBLoopTemplateWidget.SetDatasource(AValue: TDatasource);
begin
  if FDatasource=AValue then Exit;
  if Assigned(FDatasource) then
    FDatasource.RemoveFreeNotification(Self);
  FDatasource:=AValue;
  if Assigned(FDatasource) then
    FDatasource.FreeNotification(Self);
end;

function TCustomDBLoopTemplateWidget.CreateLoopEnumerator(aCurrValues: TLoopTemplateValue): TLoopEnumerator;

Var
  DBL : TDBLoopEnumerator;

begin
  DBL:=TDBLoopEnumerator.Create(Self,aCurrValues);
  DBL.SetDataset(Self.Dataset);
  Result:=DBL;
end;

Class function TCustomDBLoopTemplateWidget.CreateCurrValues: TLoopTemplateValue;
begin
  Result:=TDBFieldValueData.Create;
end;

procedure TCustomDBLoopTemplateWidget.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent=FDatasource) then
    FDataSource:=Nil;
end;

end.

