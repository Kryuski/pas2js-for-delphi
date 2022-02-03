unit demohtmlwidgets;

{$mode objfpc}

interface

uses
  sysutils, web, js, webwidget, htmlwidgets,  widgetdemo;

Type
  
  { TButtonDemo }

  TButtonDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { TRadioDemo }

  TRadioDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { TCheckboxDemo }

  TCheckboxDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { TTextInputDemo }

  TTextInputDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;
  
  { TFileInputDemo }

  TFileInputDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { TDateInputDemo }

  TDateInputDemo = class(TDemoContainer)
  protected
    procedure DoChange(Sender: TObject; Event: TJSEvent); override;
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { TButtonInputDemo }

  TButtonInputDemo = class(TDemoContainer)
  private
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { THiddenInputDemo }

  THiddenInputDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { TTextAreaDemo }

  TTextAreaDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { TLabelWidgetDemo }

  TLabelWidgetDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { TTextWidgetDemo }

  TTextWidgetDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { TTextLinesWidgetDemo }

  TTextLinesWidgetDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { TSelectWidgetDemo }

  TSelectWidgetDemo = Class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { TTagWidgetDemo }

  TTagWidgetDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
  end;

  { TDivWidgetDemo }

  TDivWidgetDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
  end;
  { TTagWidgetDemo }

  { TParagraphWidget }

  TParagraphWidgetDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
  end;

  { TAudioWidgetDemo }

  TAudioWidgetDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
  end;

  { TVideoWidgetDemo }

  TVideoWidgetDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
  end;

  { TEventTableWidgetDemo }

  TEventTableWidgetDemo = Class(TDemoContainer)
  private
    procedure DoGetCellData(Sender: TObject; Enum: TTableRowEnumerator; aCell: TTableWidgetCellData);
  Public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { TStringsTableWidgetDemo }

  TStringsTableWidgetDemo = Class(TDemoContainer)
  Public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;


implementation

uses democonsts;

{
  Countrycodes are included as a JSON javascript definition in the HTML file, countrycodes.js
  We define it here as an external.
}

{$modeswitch externalclass}

Type
  TCountry = Class external name 'Object' (TJSObject)
    name,code : string;
  end;

Var
  CountryCodes : Array of TCountry; external name 'countrycodes';

{ TStringsTableWidgetDemo }

class function TStringsTableWidgetDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TStringsTableWidget;
end;

procedure TStringsTableWidgetDemo.ShowDemo;

Var
  STW : TStringsTableWidget;
  I : Integer;

begin
  inherited ShowDemo;
  STW:=TStringsTableWidget(WidgetInstance);
  STW.Classes:='table table-bordered table-striped table-hover table-sm';
  STW.CustomColumns.Add('ISO');
  STW.CustomColumns.Add('Country');
  STW.BeginUpdate;
  STW.RowCount:=Length(CountryCodes);
  For I:=0 to Length(CountryCodes)-1 do
    begin
    STW.Cells[0,I]:=CountryCodes[i].code;
    STW.Cells[1,i]:=CountryCodes[i].Name;
    end;
  STW.EndUpdate;
end;

{ TEventTableWidgetDemo }

procedure TEventTableWidgetDemo.DoGetCellData(Sender: TObject; Enum: TTableRowEnumerator; aCell: TTableWidgetCellData);

begin
  if aCell.Kind=rkBody then
    case aCell.Col of
      0 : aCell.Text:='Value '+IntToStr(aCell.Row+1);
      1 : aCell.Widget:=TTextInputWidget.Create(Self);
    end;
end;

class function TEventTableWidgetDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TEventTableWidget;
end;

procedure TEventTableWidgetDemo.ShowDemo;
begin
  inherited ShowDemo;
  TEventTableWidget(WidgetInstance).OnGetCellData:=@DoGetCellData;
  TEventTableWidget(WidgetInstance).CustomColumns.Add('Name');
  TEventTableWidget(WidgetInstance).CustomColumns.Add('Value');
  TEventTableWidget(WidgetInstance).RowCount:=10;
end;

{ TVideoWidgetDemo }

class function TVideoWidgetDemo.WebWidgetClass: TCustomWebWidgetClass;

begin
  Result:=TVideoWidget;
end;

{ TAudioWidgetDemo }

class function TAudioWidgetDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TAudioWidget;
end;


{ TSelectWidgetDemo }

class function TSelectWidgetDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TSelectWidget;
end;

procedure TSelectWidgetDemo.ShowDemo;

begin
  inherited ShowDemo;
  With TSelectWidget(WidgetInstance).Items do
     begin
     Add('Item 1');
     Add('Item 2');
     Add('Item 3');
     Add('Item 4');
     Add('Item 5');
     end;
end;

{ TParagraphWidget }

class function TParagraphWidgetDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TParagraphWidget;
end;

{ TDivWidgetDemo }

class function TDivWidgetDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TDivWidget
end;

{ TTagWidgetDemo }

class function TTagWidgetDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TTagWidget;
end;

{ TTextLinesWidgetDemo }

class function TTextLinesWidgetDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TTextLinesWidget;
end;

procedure TTextLinesWidgetDemo.ShowDemo;
begin
  inherited ShowDemo;
  With TTextLinesWidget(WidgetInstance).Lines do
    begin
    beginUpdate;
    try
      Add(Lorem1);
      Add('');
      Add(Lorem2);
      Add('');
      Add(Lorem3);
    finally
      EndUpdate;
    end;
    end;
end;

{ TTextWidgetDemo }

class function TTextWidgetDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TTextWidget
end;

procedure TTextWidgetDemo.ShowDemo;
begin
  inherited ShowDemo;
  TTextWidget(WidgetInstance).Text:=Lorem1;
end;

{ TLabelWidgetDemo }

class function TLabelWidgetDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TLabelwidget;
end;

procedure TLabelWidgetDemo.ShowDemo;
begin
  inherited ShowDemo;
  TLabelwidget(WidgetInstance).Text:=Lorem1;
end;

{ TTextAreaDemo }

class function TTextAreaDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TTextAreaWidget;
end;

procedure TTextAreaDemo.ShowDemo;
begin
  inherited ShowDemo;
  TTextAreaWidget(WidgetInstance).Rows:=20;
  TTextAreaWidget(WidgetInstance).Columns:=80;
  TTextAreaWidget(WidgetInstance).Lines.Add(Lorem1);
  TTextAreaWidget(WidgetInstance).Lines.Add('');
  TTextAreaWidget(WidgetInstance).Lines.Add(Lorem2);
  TTextAreaWidget(WidgetInstance).Lines.Add('');
  TTextAreaWidget(WidgetInstance).Lines.Add(Lorem3);
end;

{ THiddenInputDemo }

class function THiddenInputDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=THiddenInputWidget;
end;

procedure THiddenInputDemo.ShowDemo;
begin
  inherited ShowDemo;
  THiddenInputWidget(WidgetInstance).Value:='This value is hidden';
  THiddenInputWidget(WidgetInstance).ValueName:='MyHidden';
end;

{ TButtonDemo }

class function TButtonDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TButtonWidget;
end;

procedure TButtonDemo.ShowDemo;
begin
  inherited ShowDemo;
  WidgetInstance.OnClick:=@DoClick;
end;

{ TRadioDemo }


class function TRadioDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TRadioInputWidget;
end;

procedure TRadioDemo.ShowDemo;
begin
  inherited ShowDemo;
  TRadioInputWidget(WidgetInstance).Text:='A Radio';
  WidgetInstance.OnChange:=@DoChange;
end;

{ TCheckboxDemo }

class function TCheckboxDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TCheckboxInputWidget;
end;

procedure TCheckboxDemo.ShowDemo;
begin
  inherited ShowDemo;
  TCheckboxInputWidget(WidgetInstance).Text:='A checkbox';
  WidgetInstance.OnChange:=@DoChange;
end;

{ TTextInputDemo }

class function TTextInputDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TTextInputWidget;
end;

procedure TTextInputDemo.ShowDemo;
begin
  inherited ShowDemo;
  TTextInputWidget(WidgetInstance).Value:='A Text Value';
  WidgetInstance.OnChange:=@DoChange;
end;

{ TDateInputDemo }

procedure TDateInputDemo.DoChange(Sender: TObject; Event: TJSEvent);
begin
  Inherited;
  Writeln(Sender.ClassName,' date value: ', DateToStr(TDateInputWidget(WidgetInstance).Date));
end;

class function TDateInputDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TDateInputWidget;
end;

procedure TDateInputDemo.ShowDemo;
begin
  inherited ShowDemo;
  TDateInputWidget(WidgetInstance).Date:=Date+1;
  WidgetInstance.OnChange:=@DoChange;
end;

{ TFileInputDemo }


class function TFileInputDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TFileInputWidget;
end;

procedure TFileInputDemo.ShowDemo;
begin
  inherited ShowDemo;
//  TFileInputWidget(WidgetInstance).FileName:='my.txt';
  WidgetInstance.OnChange:=@DoChange;
end;

{ TButtonInputDemo }


class function TButtonInputDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TButtonInputWidget;
end;

procedure TButtonInputDemo.ShowDemo;
begin
  inherited ShowDemo;
  WidgetInstance.OnClick:=@DoClick;
  TButtonInputWidget(WidgetInstance).Value:='Press me';
end;


initialization
  TCheckboxDemo.RegisterDemo;
  TRadioDemo.RegisterDemo;
  TButtonDemo.RegisterDemo;
  TTextInputDemo.RegisterDemo;
  TDateInputDemo.RegisterDemo;
  TFileInputDemo.RegisterDemo;
  TButtonInputDemo.RegisterDemo;
  THiddenInputDemo.RegisterDemo;
  TTextAreaDemo.RegisterDemo;
  TLabelWidgetDemo.RegisterDemo;
  TTextWidgetDemo.RegisterDemo;
  TTagWidgetDemo.RegisterDemo;
  TDivWidgetDemo.RegisterDemo;
  TParagraphWidgetDemo.RegisterDemo;
  TSelectWidgetDemo.RegisterDemo;
  TAudioWidgetDemo.RegisterDemo;
  TVideoWidgetDemo.RegisterDemo;
  TEventTableWidgetDemo.RegisterDemo;
  TStringsTableWidgetDemo.RegisterDemo;
end.

