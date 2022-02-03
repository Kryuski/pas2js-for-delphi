unit propertygridwidget;

{$mode objfpc}

interface

uses
  Classes, SysUtils, types, web, webwidget, htmlwidgets, typinfo, contnrs;

Type

  { TPropertyEditor }
  TSimplePropertyGridWidget = Class;

  TPropertyEditor = Class (TObject)
  private
    FGrid : TSimplePropertyGridWidget;
    FInfo : TTypeMemberProperty;
    FInstance : TObject;
    Level : Integer;
    SubList: TFPObjectList;
    ElRow : TJSHTMLElement;
    ElName : TJSHTMLElement;
    ElValue : TJSHTMLElement;
    OnValueChanged : TNotifyEvent;
    procedure SetInstance(AValue: TObject);
  public
    function CreateEditorElement (aTag : String; aID : String) : TJSHTMLElement; virtual;
    Constructor Create(aGrid : TSimplePropertyGridWidget;aInstance : TObject; aInfo: TTypeMemberProperty);
    Destructor destroy; override;
    Procedure ValueChanged(Event: TJSEvent);
    // Initialize editor
    Function InitEditor : TJSHTMLElement; virtual; abstract;
    // Apply editor value to property
    Procedure EditorToProperty; virtual; abstract;
    // Apply property value to editor
    Procedure PropertyToEditor; virtual; abstract;
    Property Info : TTypeMemberProperty Read FInfo;
    Property Instance : TObject Read FInstance Write SetInstance;
    Property Grid : TSimplePropertyGridWidget Read FGrid;
  end;
  TPropertyEditorClass = Class of TPropertyEditor;

  { TBooleanPropertyEditor }

  TBooleanPropertyEditor = Class(TPropertyEditor)
    procedure EditorToProperty; override;
    Function InitEditor: TJSHTMLElement; override;
    procedure PropertyToEditor; override;
  end;

  { TIntegerPropertyEditor }

  TIntegerPropertyEditor = Class(TPropertyEditor)
    procedure EditorToProperty; override;
    Function InitEditor: TJSHTMLElement; override;
    procedure PropertyToEditor; override;
  end;

  { TFloatPropertyEditor }

  TFloatPropertyEditor = Class(TPropertyEditor)
    procedure EditorToProperty; override;
    Function InitEditor: TJSHTMLElement; override;
    procedure PropertyToEditor; override;
  end;

  { TStringPropertyEditor }

  TStringPropertyEditor = Class(TPropertyEditor)
    procedure EditorToProperty; override;
    Function InitEditor: TJSHTMLElement; override;
    procedure PropertyToEditor; override;
  end;

  { TLabelPropertyEditor }

  TLabelPropertyEditor = Class(TPropertyEditor)
    procedure EditorToProperty; override;
    Function InitEditor: TJSHTMLElement; override;
    procedure PropertyToEditor; override;
  end;

  { TStringsPropertyEditor }

  TStringsPropertyEditor = Class(TPropertyEditor)
    procedure EditorToProperty; override;
    Function InitEditor: TJSHTMLElement; override;
    procedure PropertyToEditor; override;
  end;

  { TStringListPropertyEditor }

  TStringListPropertyEditor = Class(TPropertyEditor)
    // Values and texts to display.
    Procedure GetStrings(aTexts,aValues : TStrings); virtual; abstract;
    // Selected Option value
    Function GetSelectedValue : string;
    // Selected Option text
    Function GetSelectedText : string;
    Procedure SetSelectedValue(aValue : string);
    Function InitEditor: TJSHTMLElement; override;
  end;

  { TEnumerationPropertyEditor }

  TEnumerationPropertyEditor = Class(TStringListPropertyEditor)
    Procedure GetStrings(aTexts,aValues : TStrings); override;
    procedure EditorToProperty; override;
    procedure PropertyToEditor; override;
  end;
  
  { TComponentPropertyEditor }

  TComponentPropertyEditor = Class(TStringListPropertyEditor)
    Procedure GetStrings(aTexts,aValues : TStrings); override;
    procedure EditorToProperty; override;
    procedure PropertyToEditor; override;
  end;


  { TSetPropertyEditor }

  TSetPropertyEditor = Class(TPropertyEditor)
    Function InitEditor: TJSHTMLElement; override;
    procedure EditorToProperty; override;
    procedure PropertyToEditor; override;
  end;

  { TPersistentPropertyEditor }

  TPersistentPropertyEditor = Class(TPropertyEditor)
    labelEL,ButtoneL: TJSHTMLelement;
    procedure EditorToProperty; override;
    Function InitEditor: TJSHTMLElement; override;
    procedure PropertyToEditor; override;
  end;

  { TSimplePropertyGridWidget }

  // ObjectStack : 
  // Collection: |PropName| -0 ClassName (N) |
  // Persistent: | > PropName| -0 ClassName (N) |
  // Top: 
  // ObjectPath: < Instance1.PropName1[3].PropName2 |
  // + (N) [ XXX V] - ]  
  TSimplePropertyGridWidget = class(TWebWidget)
  private
    FLookupRoot: TComponent;
    FMySubject: TObject;
    FProperties : TFPObjectList;
    procedure DoPropertyChanged(Sender: TObject);
    procedure SetSubject(AValue: TObject);
    procedure SetLookupRoot(AValue: TComponent);
    procedure UpdatePropertyFromEditor(aRow: TPropertyEditor);
  Protected
    function CreateEditorElement (aTag : String; aID : String) : TJSHTMLElement; virtual;
    function AllowProperty(M: TTypeMemberProperty): boolean; virtual;
    procedure CreatePropertyList(aInstance: TObject; aList: TFPObjectList); virtual;
    function CreatePropertyEditor(aInstance: TObject; aInfo: TTypeMemberProperty): TpropertyEditor; virtual;
    procedure CreatePropertyGrid(aTableElement, aRowElement: TJSHTMLElement; FMySubject: TObject; aProperties: TFPObjectList); virtual;
    function DoBooleanEditor(aEditor: TJSHTMLElement; aInfo: TPropertyEditor): TJSHTMLElement; virtual;
    function DoComponentEditor(aEditor: TJSHTMLElement; aInfo: TPropertyEditor): TJSHTMLElement; virtual;
    function DoEnumerationEditor(aEditor: TJSHTMLElement; aInfo: TPropertyEditor): TJSHTMLElement; virtual;
    function DoFloatEditor(aEditor: TJSHTMLElement; aInfo: TPropertyEditor): TJSHTMLElement; virtual;
    function DoIntegerEditor(aEditor: TJSHTMLElement; aInfo: TPropertyEditor): TJSHTMLElement; virtual;
    function DoLabelEditor(aEditor: TJSHTMLElement; aInfo: TPropertyEditor): TJSHTMLElement; virtual;
    function DoPersistentEditor(aEditor: TJSHTMLElement; aInfo: TPropertyEditor): TJSHTMLElement;virtual;
    function DoSetEditor(aEditor: TJSHTMLElement; aInfo: TPropertyEditor): TJSHTMLElement;virtual;
    function DoStringEditor(aEditor: TJSHTMLElement; aInfo: TPropertyEditor): TJSHTMLElement;virtual;
    function DoStringsEditor(aEditor: TJSHTMLElement; aInfo: TPropertyEditor): TJSHTMLElement;virtual;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Function DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement; override;
    Function HTMLTag : String; override;
  Published
    Property Subject : TObject Read FMySubject Write SetSubject;
    Property LookupRoot : TComponent Read FLookupRoot Write SetLookupRoot;
  end;

implementation

uses js;

{ TSetPropertyEditor }

function TSetPropertyEditor.InitEditor: TJSHTMLElement;

Var
  inp : TJSHTMLTableElement;
  cell : TJSHTMLTableDataCellElement;
  r : TJSHTMLTableRowElement;
  vals : TIntegerDynArray;
  eInfo : TTypeInfoEnum;
  I : Integer;
  selcb,CB : TJSHTMLInputElement;
  lbl : TJSHTMLLabelElement;

begin
  Result:=CreateEditorElement('table','');
  eInfo:=TTypeInfoEnum(TTypeInfoSet(Info.TypeInfo).CompType);
  // Row with the property as a string.
  R:=TJSHTMLTableRowElement(CreateEditorElement('tr',''));
  Result.Append(R);
  cell:=TJSHTMLTableDatacellElement(CreateEditorElement('td',''));
  cell.InnerText:=GetSetProp(instance,Info);
  R.AppendChild(Cell);
  // For every element in the set, a checkbox row
  for I:=TTypeInfoEnum(Info.TypeInfo).MinValue to TTypeInfoEnum(Info.TypeInfo).MaxValue do
    begin
    // Row
    R:=TJSHTMLTableRowElement(CreateEditorElement('tr',''));
    Result.Append(R);
    // Cell
    cell:=TJSHTMLTableDatacellElement(CreateEditorElement('td',''));
    R.AppendChild(Cell);
    // Input
    CB:=TJSHTMLInputElement(CreateEditorElement('input',''));
    cb._type:='checkbox';
    cb.id:=Grid.GenerateID;
    cb.dataset['enumval']:=IntTostr(i);
    cell.AppendChild(cb);
    // label for input
    lbl:=TJSHTMLLabelElement(CreateEditorElement('label',''));
    lbl.For_:=cb.Id;
    lbl.InnerText:=TTypeInfoEnum(Info.TypeInfo).EnumType.IntToName[I];
    cell.AppendChild(lbl);
    end;
end;

procedure TSetPropertyEditor.EditorToProperty;
Var
  vals : TIntegerDynArray;
  els: TJSNodeList;
  cb : TJSHTMLInputElement;
  I,V : Integer;

begin
  vals:=[];
  els:=elValue.querySelectorAll('[data-enumval]');
  for I:=0 to els.length-1 do
    begin
    CB:=TJSHTMLInputElement(els[i]);
    if CB.checked then
      begin
      V:=StrToIntDef(String(CB.Dataset['enumval']),-1);
      if V>=0 then
        TJSArray(vals).push(V);
      end;
    end;
  SetSetPropArray(instance,Info,Vals)
end;

procedure TSetPropertyEditor.PropertyToEditor;

Var
  vals : TIntegerDynArray;
  els: TJSNodeList;
  cb : TJSHTMLInputElement;
  I : Integer;

begin
  vals:=GetSetPropArray(instance,Info);
  els:=elValue.querySelectorAll('[data-enumval]');
  for I:=0 to els.length-1 do
    begin
    CB:=TJSHTMLInputElement(els[i]);
    CB.checked:=TJSArray(vals).indexOf(StrToIntDef(String(CB.Dataset['enumval']),-1))>=0;
    end;
end;

{ TFloatPropertyEditor }

procedure TFloatPropertyEditor.EditorToProperty;
Var
  S : String;
  F : Double;

begin
  S:=TJSHTMLInputElement(ElValue).value;
  if TryStrToFloat(S,F) then
    SetFloatProp(Instance,Info,F)
end;

function TFloatPropertyEditor.InitEditor: TJSHTMLElement;

begin
  Result:=CreateEditorElement('input','');
  TJSHTMLInputElement(Result)._type:='number';
end;

procedure TFloatPropertyEditor.PropertyToEditor;
begin
  TJSHTMLInputElement(elValue).Value:=FloatToStr(GetFloatProp(Instance,Info));
end;

{ TStringListPropertyEditor }

function TStringListPropertyEditor.GetSelectedValue: string;

Var
  Sel : TJSHTMLSelectElement;

begin
  Sel:=TJSHTMLSelectElement(ElValue);
  if Sel.selectedIndex>=0 then
    Result:=TJSHTMLOptionElement(Sel.Options.Items[Sel.selectedIndex]).Value
  else
    Result:='';
end;

function TStringListPropertyEditor.GetSelectedText: string;
Var
  Sel : TJSHTMLSelectElement;

begin
  Sel:=TJSHTMLSelectElement(ElValue);
  if Sel.selectedIndex>=0 then
    Result:=TJSHTMLOptionElement(Sel.Options.Items[Sel.selectedIndex]).InnerText
  else
    Result:='';
end;

procedure TStringListPropertyEditor.SetSelectedValue(aValue: string);
Var
  Inp:TJSHTMLSelectElement;
  I : integer;
  o : TJSHTMLOptionElement;
begin
  Inp:=TJSHTMLSelectElement(elValue);
  O:=Nil;
  I:=0;
  While (O=Nil) and (I<inp.options.length) do
    begin
    if TJSHTMLOptionElement(inp.options.Items[i]).value=aValue then
      o:=TJSHTMLOptionElement(inp.options.Items[i]);
    Inc(I);
    end;
  if assigned(o) then
    o.selected:=True;
end;

function TStringListPropertyEditor.InitEditor: TJSHTMLElement;

Var
  inp : TJSHTMLSelectElement;
  O,osel : TJSHTMLOptionElement;
  avalS : String;
  aVal,I : Integer;
  aTexts,aValues : TStrings;

begin
  Result:=nil;
  aTexts:=Nil;
  aValues:=TstringList.Create;
  try
    aTexts:=TstringList.Create;
    GetStrings(aTexts,aValues);
    Result:=CreateEditorElement('select','');
    Inp:=TJSHTMLSelectElement(Result);
    inp.Multiple:=False;
    For I:=0 to aTexts.Count-1 do
      begin
      O:=TJSHTMLOptionElement(CreateEditorElement('option',''));
      O.InnerText:=aTexts[i];
      if (I<aValues.Count) then
        O.value:=aValues[i];
      Inp.appendChild(O)
      end
  finally
    aTexts.Free;
    aValues.Free;
  end;
end;

{ TEnumerationPropertyEditor }

procedure TEnumerationPropertyEditor.GetStrings(aTexts,aValues: TStrings);

Var
  I : Integer;

begin
  for I:=TTypeInfoEnum(Info.TypeInfo).MinValue to TTypeInfoEnum(Info.TypeInfo).MaxValue do
    begin
    aTexts.Add(TTypeInfoEnum(Info.TypeInfo).EnumType.IntToName[I]);
    aValues.Add(IntToStr(I));
    end;

end;

procedure TEnumerationPropertyEditor.EditorToProperty;

Var
  Sel : TJSHTMLSelectElement;
  S : String;

begin
  S:=GetSelectedValue;
  if S<>'' then
    SetOrdProp(Instance,Info,StrToInt(S));
end;

procedure TEnumerationPropertyEditor.PropertyToEditor;

var
  aVal : Integer;

begin
  AVal:=GetOrdProp(Instance,Info);
  SetSelectedValue(IntToStr(aVal));
end;

{ TComponentPropertyEditor }

procedure TComponentPropertyEditor.GetStrings(aTexts,aValues: TStrings);

Var
  I : Integer;
  N : String;

begin
  for I:=0 to Grid.LookupRoot.ComponentCount-1 do
    begin
    N:=Grid.LookupRoot.Components[i].Name;
    if N<>'' then
      aTexts.Add(N);
    end;
end;

procedure TComponentPropertyEditor.EditorToProperty;

Var
  Sel : TJSHTMLSelectElement;
  S : String;
  C : TComponent;
  
begin
  S:=GetSelectedValue;
  if S<>'' then
    begin
    C:=Grid.LookupRoot.FindComponent(S);
    SetObjectProp(Instance,Info,C);
    end
  else
    C:=Nil;  
  SetObjectProp(Instance,Info,C);
end;

procedure TComponentPropertyEditor.PropertyToEditor;

var
  aVal : TObject;

begin
  AVal:=GetObjectProp(Instance,Info);
  if (AVal is TComponent) then
    SetSelectedValue(TComponent(aVal).Name)
  else
    SetSelectedValue('');  
end;

{ TLabelPropertyEditor }

procedure TLabelPropertyEditor.EditorToProperty;

Var
  S : String;

begin
  S:=TJSHTMLLabelElement(ElValue).InnerText;
  SetStrProp(Instance,Info,S);
end;

function TLabelPropertyEditor.InitEditor: TJSHTMLElement;


begin
  Result:=CreateEditorElement('label','');
end;

procedure TLabelPropertyEditor.PropertyToEditor;
begin
  TJSHTMLLabelElement(elValue).InnerText:=GetStrProp(Instance,Info);
end;

{ TPersistentPropertyEditor }

procedure TPersistentPropertyEditor.EditorToProperty;

var
  i : Integer;

begin
  For I:=0 to SubList.Count-1 do
    TPersistentPropertyEditor(SubList[i]).EditorToProperty;
end;

function TPersistentPropertyEditor.InitEditor: TJSHTMLElement;

Var
  O : TObject;
  I : Integer;
  SPE : TPropertyEditor;

begin
  Result:=CreateEditorElement('div','');
  ButtonEl:=CreateEditorElement('i','');
  ButtonEl.ClassName:='fi fi-plus';
  Result.AppendChild(ButtonEl);
  Labelel:=CreateEditorElement('span','');
  Labelel.className:='persistentname';
  labelel.InnerText:='?';
  O:=GetObjectProp(Instance,Info);
  Writeln('Getting sublist for ',O.ClassName);
  SubList:=TFPObjectList.Create(True);
  Grid.CreatePropertyList(O,SubList);
  For I:=0 to SubList.Count-1 do
    begin
    SPE:=TPropertyEditor(SubList[i]);
    SPE.InitEditor;
    end;
end;

procedure TPersistentPropertyEditor.PropertyToEditor;

Var
  I : Integer;
  SPE : TPropertyEditor;
  O : TObject;

begin
  LabelEl.innertext:=GetObjectProp(Instance,Info).ClassName;
  O:=GetObjectProp(Self.Instance,Info);
  For I:=0 to SubList.Count-1 do
    begin
    SPE:=TPropertyEditor(SubList[i]);
    SPE.Instance:=O;
    if Assigned(SPE.Instance) then
      SPE.PropertyToEditor;
    end;
end;


{ TStringPropertyEditor }

procedure TStringPropertyEditor.EditorToProperty;
Var
  S : String;
begin
  S:=TJSHTMLInputElement(ElValue).value;
  SetStrProp(Instance,Info,S);
end;

function TStringPropertyEditor.InitEditor: TJSHTMLElement;


begin
  Result:=CreateEditorElement('input','');
  TJSHTMLInputElement(Result)._type:='text';
end;

procedure TStringPropertyEditor.PropertyToEditor;
begin
  TJSHTMLInputElement(elValue).Value:=GetStrProp(Instance,Info);
end;

{ TStringsPropertyEditor }

procedure TStringsPropertyEditor.EditorToProperty;

Var
  L : TStrings;  
begin
  L:=TStringList.Create;
  try
    L.Text:=TJSHTMLTextAreaElement(ElValue).value;
    SetObjectProp(Instance,Info,L);
  finally
    l.free;
  end;  
end;

function TStringsPropertyEditor.InitEditor: TJSHTMLElement;

Var
  AVal : TObject;

begin
  Result:=TJSHTMLTextAreaElement(CreateEditorElement('textarea',''));
  Result['rows']:='5';
  Result['columns']:='40';
end;

procedure TStringsPropertyEditor.PropertyToEditor;

Var
  AVal : TObject;

begin
  AVal:=GetObjectProp(Instance,Info);
  if Assigned(AVal) then
    TJSHTMLTextAreaElement(elValue).Value:=TStrings(aVal).Text
  else  
    TJSHTMLTextAreaElement(elValue).Value:='';
end;

{ TIntegerPropertyEditor }

procedure TIntegerPropertyEditor.EditorToProperty;

Var
  S : String;
  I : NativeInt;

begin
  S:=TJSHTMLInputElement(ElValue).value;
  if TryStrToInt64(S,I) then
    SetOrdProp(Instance,Info,I)
end;

function TIntegerPropertyEditor.InitEditor: TJSHTMLElement;
begin
  Result:=CreateEditorElement('input','');
  Result.Dataset['propname']:=Info.Name;
  TJSHTMLInputElement(Result)._type:='number';
end;

procedure TIntegerPropertyEditor.PropertyToEditor;
begin
  TJSHTMLInputElement(ElValue).Value:=IntToStr(GetOrdProp(Instance,Info));
end;

{ TBooleanPropertyEditor }

procedure TBooleanPropertyEditor.EditorToProperty;
Var
  B : Boolean;
begin
  B:=TJSHTMLInputElement(ElValue).checked;
  SetBoolProp(Instance,Info,B);
end;

function TBooleanPropertyEditor.InitEditor: TJSHTMLElement;
begin
  Result:=CreateEditorElement('input','');
  TJSHTMLInputElement(Result)._type:='checkbox';
end;

procedure TBooleanPropertyEditor.PropertyToEditor;
begin
  TJSHTMLInputElement(elValue).Checked:=GetBoolProp(Instance,Info);
end;

{ TPropertyEditor }

procedure TPropertyEditor.ValueChanged(Event: TJSEvent);
begin
  If Assigned(OnValueChanged) then
    OnValueChanged(Self);
  EditorToProperty;
end;

destructor TPropertyEditor.destroy;
begin
  FreeAndNil(Sublist);
  inherited destroy;
end;

{ TSimplePropertyGridWidget }

procedure TSimplePropertyGridWidget.SetSubject(AValue: TObject);
begin
  Writeln('Setting subject ', Assigned(aValue));
  if FMySubject=AValue then Exit;
  FMySubject:=AValue;
  if IsRendered then
    Refresh;
end;

function TSimplePropertyGridWidget.AllowProperty(M : TTypeMemberProperty) : boolean;

begin
  Result:=M.TypeInfo.Kind in [tkInteger,tkChar,tkString,tkEnumeration,tkSet,tkDouble,tkBool,tkClass];
end;

procedure TSimplePropertyGridWidget.CreatePropertyList(aInstance : TObject; aList: TFPObjectList);

Var
  R : TPropertyEditor;
  M: TTypeMemberProperty;

begin
  aList.Clear;
  if Not Assigned(aInstance) then
    exit;
  for M in GetPropList(aInstance) do
    if AllowProperty(M) then
      begin
      // Writeln('Row for property ',M.Name);
      R:=CreatePropertyEditor(aInstance,M);
      if R<>Nil then
        aList.Add(R);
      end;
end;

constructor TSimplePropertyGridWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FProperties:=TFPObjectList.Create(True);
end;

destructor TSimplePropertyGridWidget.Destroy;
begin
  FreeAndNil(FProperties);
  inherited Destroy;
end;

function TSimplePropertyGridWidget.DoRenderHTML(aParent, aElement: TJSHTMLElement) : TJSHTMLElement;


begin
  // Writeln('Grid Render HTML',assigned(FMySubject));
  Result:=Inherited DoRenderHTML(aParent,aElement);
  CreatePropertyList(FMySubject,FProperties);
  aElement.className:='propGridTable table-bordered table-sm table-dark';
  CreatePropertyGrid(aElement,Nil,FMySubject,FProperties);
end;

procedure TSimplePropertyGridWidget.CreatePropertyGrid(aTableElement, aRowElement: TJSHTMLElement; FMySubject: TObject;
  aProperties: TFPObjectList);

Var
  R : TPropertyEditor;
  TR,TD : TJSHTMLElement;
  I : integer;
  O : TObject;

begin
  For I:=0 to aProperties.Count-1 do
    begin
    R:=TPropertyEditor(aProperties[i]);
    TR:=CreateEditorElement('tr','');
    TR.dataset['propertyName']:=R.Info.Name;
    TR.ClassName:='propGridRow';
    R.ElRow:=TR;
    aTableElement.AppendChild(TR);
    TD:=CreateEditorElement('td','');
    TD.className:='propGridNameCell';
    TR.AppendChild(TD);
    TD.InnerText:=R.Info.Name;
    R.ElName:=TD;
    TD:=CreateEditorElement('td','');
    TD.className:='propGridDataCell';
    TR.AppendChild(TD);
    R.ElValue:=R.InitEditor;
    if Assigned(R.ElValue) then
      begin
      R.PropertyToEditor;
      R.ElValue.Dataset['propname']:=R.Info.Name;
      R.ElValue.addEventListener('change',@R.ValueChanged);
      end;
    TD.AppendChild(R.ElValue);
    if Assigned(R.SubList) then
      begin
      O:=GetObjectProp(R.Instance,R.Info);
      if Assigned(O) then
        CreatePropertyGrid(aTableElement,TR,O,R.Sublist);
      end;
    end;
end;

function TSimplePropertyGridWidget.DoIntegerEditor(aEditor : TJSHTMLElement; aInfo: TPropertyEditor) : TJSHTMLElement;


begin
  Result:=aEditor;
  if Result=Nil then
    begin
    Result:=CreateEditorElement('input','');
    Result.Dataset['propname']:=aInfo.Info.Name;
    TJSHTMLInputElement(Result)._type:='number';
    end;
  TJSHTMLInputElement(Result).Value:=IntToStr(GetOrdProp(aInfo.Instance,aInfo.Info));
end;

function TSimplePropertyGridWidget.DoFloatEditor(aEditor : TJSHTMLElement; aInfo: TPropertyEditor) : TJSHTMLElement;


begin
  Result:=aEditor;
  if Result=Nil then
    begin
    Result:=CreateEditorElement('input','');
    Result.Dataset['propname']:=aInfo.Info.Name;
    TJSHTMLInputElement(Result)._type:='number';
    end;
  TJSHTMLInputElement(Result).Value:=FloatToStr(GetFloatProp(aInfo.Instance,aInfo.Info));
end;

function TSimplePropertyGridWidget.DoStringEditor(aEditor : TJSHTMLElement; aInfo: TPropertyEditor) : TJSHTMLElement;

begin
  Result:=aEditor;
  if Result=Nil then
    begin
    Result:=CreateEditorElement('input','');
    TJSHTMLInputElement(Result)._type:='text';
    end;
  TJSHTMLInputElement(Result).Value:=GetStrProp(aInfo.Instance,aInfo.Info);
end;

function TSimplePropertyGridWidget.DoBooleanEditor(aEditor : TJSHTMLElement; aInfo: TPropertyEditor) : TJSHTMLElement;

begin
end;

function TSimplePropertyGridWidget.DoEnumerationEditor(aEditor : TJSHTMLElement; aInfo: TPropertyEditor) : TJSHTMLElement;


Var
  inp : TJSHTMLSelectElement;
  O,osel : TJSHTMLOptionElement;
  avalS : String;
  aVal,I : Integer;


begin
  oSel:=nil;
  Result:=aEditor;
  AVal:=GetOrdProp(aInfo.Instance,aInfo.Info);
  aValS:=IntTostr(aVal);
  if Result=Nil then
    begin
    Result:=CreateEditorElement('select','');
    Inp:=TJSHTMLSelectElement(Result);
    inp.Multiple:=False;
    for I:=TTypeInfoEnum(aInfo.Info.TypeInfo).MinValue to TTypeInfoEnum(aInfo.Info.TypeInfo).MaxValue do
      begin
      O:=TJSHTMLOptionElement(CreateEditorElement('option',''));
      O.InnerText:=TTypeInfoEnum(aInfo.Info.TypeInfo).EnumType.IntToName[I];
      O.value:=IntToStr(I);
      if I=aVal then
        oSel:=o;
      Inp.appendChild(O)
      end;
    end
  else
    begin
    Inp:=TJSHTMLSelectElement(Result);
    for I:=0 to inp.options.length-1 do
      if TJSHTMLOptionElement(inp.options.Items[i]).value=aVals then
        oSel:=TJSHTMLOptionElement(inp.options.Items[i]);
    end;
  if assigned(oSel) then
    oSel.selected:=True;
  Inp.Value:=IntToStr(GetOrdProp(aInfo.Instance,aInfo.Info));
end;

function TSimplePropertyGridWidget.DoSetEditor(aEditor : TJSHTMLElement; aInfo: TPropertyEditor) : TJSHTMLElement;

Var
  inp : TJSHTMLTableElement;
  cell : TJSHTMLTableDataCellElement;
  r : TJSHTMLTableRowElement;
  vals : TIntegerDynArray;
  eInfo : TTypeInfoEnum;
  I : Integer;
  selcb,CB : TJSHTMLInputElement;
  lbl : TJSHTMLLabelElement;

begin
  result:=aEditor;
  eInfo:=TTypeInfoEnum(TTypeInfoSet(aInfo.Info.TypeInfo).CompType);
  vals:=GetSetPropArray(aInfo.instance,aInfo.Info);
  if Result=Nil then
    begin
    Result:=CreateEditorElement('table','');
    // Row with the property as a string.
    R:=TJSHTMLTableRowElement(CreateEditorElement('tr',''));
    Result.Append(R);
    cell:=TJSHTMLTableDatacellElement(CreateEditorElement('td',''));
    cell.InnerText:=GetSetProp(aInfo.instance,aInfo.Info);
    R.AppendChild(Cell);
    // For every element in the set, a checkbox row
    for I:=TTypeInfoEnum(aInfo.Info.TypeInfo).MinValue to TTypeInfoEnum(aInfo.Info.TypeInfo).MaxValue do
      begin
      // Row
      R:=TJSHTMLTableRowElement(CreateEditorElement('tr',''));
      Result.Append(R);
      // Cell
      cell:=TJSHTMLTableDatacellElement(CreateEditorElement('td',''));
      R.AppendChild(Cell);
      // Input
      CB:=TJSHTMLInputElement(CreateEditorElement('input',''));
      cb._type:='checkbox';
      cb.id:=GenerateID;
      cb.dataset['enumval']:=IntTostr(i);
      cell.AppendChild(cb);
      // label for input
      lbl:=TJSHTMLLabelElement(CreateEditorElement('label',''));
      lbl.For_:=cb.Id;
      lbl.InnerText:=TTypeInfoEnum(aInfo.Info.TypeInfo).EnumType.IntToName[I];
      cell.AppendChild(lbl);
      end;
    end;
end;

function TSimplePropertyGridWidget.DoComponentEditor(aEditor : TJSHTMLElement; aInfo: TPropertyEditor) : TJSHTMLElement;


Var
  inp : TJSHTMLSelectElement;
  O,osel : TJSHTMLOptionElement;
  I : Integer;
  L,aVal : TObject;

begin
  oSel:=nil;
  Result:=aEditor;
  AVal:=GetObjectProp(aInfo.Instance,aInfo.Info);
  if Result=Nil then
    begin
    Result:=CreateEditorElement('select','');
    Inp:=TJSHTMLSelectElement(Result);
    inp.Multiple:=False;
    if Assigned(LookupRoot) then
       begin
       For I:=0 to LookupRoot.ComponentCount-1 do
         begin
         O:=TJSHTMLOptionElement(CreateEditorElement('option',''));
         O.InnerText:=LookupRoot.Components[i].Name;
         O.value:=LookupRoot.Components[i].Name;
         if aVal=LookupRoot.Components[i] then
           oSel:=o;
         Inp.appendChild(O);
         end;
      end;
    end
  else
    begin
    Inp:=TJSHTMLSelectElement(Result);
    if Assigned(LookupRoot) then
      begin
      I:=0;
      While (oSeL=Nil) and (I<inp.options.length) do
        begin
        O:=TJSHTMLOptionElement(inp.options.Items[i]);
        L:=LookupRoot.FindComponent(O.Value);
        if L=aval then
          oSel:=O;
        inc(I);
        end;
      end;
    end;
  if assigned(oSel) then
    oSel.selected:=True;
end;

function TSimplePropertyGridWidget.DoLabelEditor(aEditor : TJSHTMLElement; aInfo: TPropertyEditor) : TJSHTMLElement;

Var
  AVal : TObject;

begin
  AVal:=GetObjectProp(aInfo.Instance,aInfo.Info);
  Result:=aEditor;
  if Result=Nil then
    Result:=TJSHTMLLabelElement(CreateEditorElement('label',''));
  Result.InnerText:=aVal.ClassName;
end;

function TSimplePropertyGridWidget.DoPersistentEditor(aEditor : TJSHTMLElement; aInfo: TPropertyEditor) : TJSHTMLElement;

Var
  AVal : TObject;

begin
  AVal:=GetObjectProp(aInfo.Instance,aInfo.Info);
  Result:=aEditor;
  if Result=Nil then
    Result:=TJSHTMLLabelElement(CreateEditorElement('label',''));
  Result.InnerText:=aVal.ClassName;
end;

function TSimplePropertyGridWidget.DoStringsEditor(aEditor : TJSHTMLElement; aInfo: TPropertyEditor) : TJSHTMLElement;

Var
  AVal : TObject;

begin
  AVal:=GetObjectProp(aInfo.Instance,aInfo.Info);
  Result:=aEditor;
  if Result=Nil then
    begin
    Result:=TJSHTMLTextAreaElement(CreateEditorElement('textarea',''));
    Result['rows']:='5';
    end;
  Result.InnerText:=aVal.ClassName;
end;


function TSimplePropertyGridWidget.CreatePropertyEditor (aInstance : TObject; aInfo: TTypeMemberProperty) : TpropertyEditor;

Var
  EditorClass : TPropertyEditorClass;
  aEditor : TPropertyEditor;

begin
  EditorClass:=nil;
  Result:=Nil;
  // Writeln('Creating property grid editor for ',aInfo.Info.Name);
  case aInfo.TypeInfo.Kind of
    tkInteger : EditorClass:=TIntegerPropertyEditor;
    tkChar    : EditorClass:=TStringPropertyEditor;
    tkString  : EditorClass:=TStringPropertyEditor;
    tkEnumeration : EditorClass:=TEnumerationPropertyEditor;
    tkSet : EditorClass:=TSetPropertyEditor;
    tkDouble : EditorClass:=TFloatPropertyEditor;
    tkBool : EditorClass:=TBooleanPropertyEditor;
    tkClass :
       begin
       If TTypeInfoClass(aInfo.TypeInfo).ClassType.InheritsFrom(TComponent) then
         EditorClass:=TComponentPropertyEditor
       else if TTypeInfoClass(aInfo.TypeInfo).ClassType.InheritsFrom(TStrings) then
         EditorClass:=TStringsPropertyEditor
       else if TTypeInfoClass(aInfo.TypeInfo).ClassType.InheritsFrom(TPersistent) then
         EditorClass:=TPersistentPropertyEditor
       else
         EditorClass:=TLabelPropertyEditor;
       end;
  end;
  if Assigned(EditorClass) then
    Result:= EditorClass.Create(self,aInstance,aInfo);
end;

procedure TSimplePropertyGridWidget.DoPropertyChanged(Sender: TObject);

Var
  aRow : TPropertyEditor absolute sender;

begin
  Writeln('Property ',aRow.Info.Name,' changed');
end;

(*
procedure TSimplePropertyGridWidget.ApplyIntegerEditor(aRow : TPropertyEditor);

Var
  S : String;
  I : NativeInt;

begin
  S:=TJSHTMLInputElement(aRow.ElValue).value;
  if TryStrToInt64(S,I) then
    SetOrdProp(aRow.Instance,aRow.Info,I)
end;

procedure TSimplePropertyGridWidget.ApplyStringEditor(aRow : TPropertyEditor);

Var
  S : String;

begin
  S:=TJSHTMLInputElement(aRow.ElValue).value;
  SetStrProp(aRow.Instance,aRow.Info,S);
end;

procedure TSimplePropertyGridWidget.ApplyEnumEditor(aRow : TPropertyEditor);

Var
  Sel : TJSHTMLSelectElement;

begin
  Sel:=TJSHTMLSelectElement(aRow.ElValue);
  SetOrdProp(aRow.Instance,aRow.Info,StrToInt(TJSHTMLOptionElement(Sel.Options.Items[Sel.selectedIndex]).value));
end;


procedure TSimplePropertyGridWidget.ApplySetEditor(aRow : TPropertyEditor);

begin

end;

procedure TSimplePropertyGridWidget.ApplyFloatEditor(aRow : TPropertyEditor);

Var
  S : String;
  D : Double;
  Err : Integer;

begin
  S:=TJSHTMLInputElement(aRow.ElValue).value;
  val(S,D,Err);
  if Err<>0 then
    if TryStrToFloat(S,D) then
      Err:=0;
  if Err=0 then
    SetFloatProp(aRow.Instance,aRow.Info,D);
end;

procedure TSimplePropertyGridWidget.ApplyBooleanEditor(aRow : TPropertyEditor);


begin
end;

procedure TSimplePropertyGridWidget.ApplyComponentEditor(aRow : TPropertyEditor);

begin
end;

procedure TSimplePropertyGridWidget.ApplyStringsEditor(aRow : TPropertyEditor);

begin
end;

procedure TSimplePropertyGridWidget.ApplyPersistentEditor(aRow : TPropertyEditor);

begin
end;
*)

procedure TSimplePropertyGridWidget.UpdatePropertyFromEditor(aRow : TPropertyEditor);

begin
// Writeln('Creating property grid editor for ',aInfo.Info.Name);

(*
  case Arow.Info.TypeInfo.Kind of
    tkInteger : ApplyIntegerEditor(aRow);
    tkChar    : ApplyStringEditor(aRow);
    tkString  : ApplyStringEditor(aRow);
    tkEnumeration : ApplyEnumEditor(aRow);
    tkSet : ApplySetEditor(aRow);
    tkDouble : ApplyFloatEditor(aRow);
    tkBool : ApplyBooleanEditor(aRow);
    tkClass :
       begin
       If TTypeInfoClass(aRow.Info.TypeInfo).ClassType.InheritsFrom(TComponent) then
         ApplyComponentEditor(aRow)
       else if TTypeInfoClass(aRow.Info.TypeInfo).ClassType.InheritsFrom(TStrings) then
         ApplyStringsEditor(aRow)
       else if TTypeInfoClass(aRow.Info.TypeInfo).ClassType.InheritsFrom(TPersistent) then
         ApplyPersistentEditor(aRow)
       end;
  end;
*)
//  RefreshProperties(aRow);
end;

function TSimplePropertyGridWidget.CreateEditorElement(aTag: String; aID: String): TJSHTMLElement;

begin
  Result:=CreateElement(aTag,aID);
  Case lowercase(aTag) of
    'input',
    'select' :
       Result.className:='form-control';
  end;
end;

procedure TSimplePropertyGridWidget.SetLookupRoot(AValue: TComponent);
begin
  if FLookupRoot=AValue then Exit;
  FLookupRoot:=AValue;
  if IsRendered then
    Refresh;
end;

procedure TPropertyEditor.SetInstance(AValue: TObject);
begin
  if FInstance=AValue then Exit;
  FInstance:=AValue;
end;

function TPropertyEditor.CreateEditorElement(aTag: String; aID: String): TJSHTMLElement;
begin
  Result:=Grid.CreateEditorElement(aTag,aID);
end;

constructor TPropertyEditor.Create(aGrid: TSimplePropertyGridWidget; aInstance: TObject; aInfo: TTypeMemberProperty);
begin
  FGrid:=aGrid;
  FInstance:=aInstance;
  FInfo:=aInfo;
end;

function TSimplePropertyGridWidget.HTMLTag: String;
begin
  Result:='TABLE';
end;

end.

