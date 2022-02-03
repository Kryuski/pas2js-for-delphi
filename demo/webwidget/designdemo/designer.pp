unit designer;

{$mode objfpc}

interface

uses
  Classes, SysUtils, libjquery, webwidget, htmlwidgets, contnrs, js, web, webideclient;

Type

  { TRegisteredWidget }

  TRegisteredWidget = Class
  Private
    FClass : TWebwidgetClass;
    FImageName : String;
  Public
    Constructor Create(aClass : TWebwidgetClass; aImageName : String);
    Property WidgetClass : TWebwidgetClass Read FClass;
    Property ImageName : String Read FimageName;
  end;

  TWidgetButtonWidget = Class(TButtonWidget)
    MyWidget : TRegisteredWidget;
  end;

  TSortable = Class helper for TJQuery
    Procedure sortable(Options : TJSObject); external name 'sortable'; overload;
    Procedure sortable(Options : string); external name 'sortable'; overload;
  end;

  { TDesignDemo }

  TDesignDemo = class(TComponent)
  private
    FIDEClient: TIDEClient;
    procedure AddWidgetByName(aID: NativeInt; AName: String);
    function CreateNewWidget(aParent: TCustomWebWidget; aClass: TCustomWebWidgetClass): TCustomWebWidget;
    function DoActive(Event: TEventListenerEvent): boolean;
    procedure DoCommandsReceived(Sender: TObject; aCommands: TJSArray);
    procedure DoWidgetAddClick(Sender: TObject; Event: TJSEvent);
    procedure SetIDEClient(AValue: TIDEClient);
    function SortableOptions: TJSObject;
    function StreamWidget(aWidget: TCustomWebWidget): String;
  Public
    FConfirmAdd : NativeInt;
    FAddWidget : TRegisteredWidget;
    FPage : TWebPage;
    FToolBar : TContainerWidget;
    FButtons : Array[1..10] of TButtonWidget;
    FWidgetButtons : Array of TWidgetButtonWidget;
    FRegisteredWidgets : TObjectList;
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Procedure RegisterWidgets;
    Procedure RegisterWidget(aClass : TWebWidgetClass; aImageName : String);
    procedure SetAddMode(aRegisteredWidget: TRegisteredWidget);
    Procedure FillToolBar;
    Procedure SetupPage;
    property IDEClient: TIDEClient Read FIDEClient Write SetIDEClient;
  end;

implementation

Const
  SSortableSelect = '#designpage, #designpage [data-ww-element-content]';

Type

  { TJumboWidget }

  TJumboWidget = class(TSimpleTemplateWidget)
  Public
    Constructor Create(aOwner:  TComponent); override;
  end;
type
  TWidgetHack = Class(TCustomWebWidget)
    Property Element;
    Property ElementID;
    Property TopElement;
    Property ContentElement;
  end;
  TPageHack  = Class(TWebPage)
    Property Element;
    Property ElementID;
  end;

{ TJumboWidget }

constructor TJumboWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Template:='<div class="jumbotron">'+sLineBreak+
  '<h1 class="display-4">Hello, world!</h1>'+sLineBreak+
  '<p class="lead">This is a simple hero unit, a simple jumbotron-style component for calling extra attention to featured content or information.</p>'+sLineBreak+
  '<hr class="my-4">'+sLineBreak+
  '<p>It uses utility classes for typography and spacing to space content out within the larger container.</p>'+sLineBreak+
  '<p class="lead">'+sLineBreak+
  '  <a class="btn btn-primary btn-lg" href="#" role="button">Learn more</a>'+sLineBreak+
  '</p>'+sLineBreak+
  '</div>';
end;


{ TRegisteredWidget }

constructor TRegisteredWidget.Create(aClass: TWebwidgetClass; aImageName: String);
begin
  FClass:=aClass;
  FImageName:=aImageName;
end;

{ TDesignDemo }

function TDesignDemo.CreateNewWidget(aParent : TCustomWebWidget; aClass : TCustomWebWidgetClass) : TCustomWebWidget;

begin
  Result:=aClass.Create(FPage);
  Result.Name:=Result.ClassName+IntToStr(FPage.ChildCount);
  Result.Parent:=aParent;
  Result.Refresh;
  if Assigned(IDEClient) then
    IDEClient.SendAction('create',New(['widget',Result.Name,'class',Result.ClassName]));

end;

function TDesignDemo.StreamWidget(aWidget : TCustomWebWidget) : String;

Var
  S: TBytesStream;
  T : TStringStream;

begin
  T:=Nil;
  S:=TBytesStream.Create(Nil);
  try
    S.WriteComponent(aWidget);
    S.Position:=0;
    T:=TStringStream.Create('');
    ObjectBinaryToText(S,T);
    Result:=T.DataString;
  finally
    T.Free;
    S.Free;
  end;
end;

function TDesignDemo.DoActive(Event: TEventListenerEvent): boolean;

Const
  Toolbar = '<div class="designerToolbar">' +
            '<div class="designerDragHandle ui-icon ui-icon-arrow-4"></div>' +
            '<div class="designerDelete ui-icon ui-icon-trash"></div>' +
            '</div>';

var
  aParent,aNew : TCustomWebWidget;
  aNewActive : TJSHTMLElement;
  aNewWidget : TRegisteredWidget;
  ElId: String;

begin
  Result:=True;
  JQuery('.designerActive').removeClass('designerActive');
  JQuery('.designerToolbar').remove();
  aNewActive:=TJSHTMLElement(event.target);
  if (aNewActive.dataset[STopElementData] <> nil) and not isUndefined(aNewActive.dataset[STopElementData]) then
    ElId := String(aNewActive.dataset[STopElementData])
  else
    ElId := aNewActive.id;
  aParent:=FPage.FindWidgetByID(ElId);
  if aParent=Nil then
    aParent:=FPage;

  if (FAddWidget<>Nil) then
    begin
    aNewWidget:=FAddWidget;
    FAddWidget:=Nil;
    if aParent<>Nil then
      begin
      aNew:=CreateNewWidget(aParent,aNewWidget.WidgetClass);
      aNewActive:=TWidgetHack(aNew).TopElement;
      if Assigned(TWidgetHack(aNew).ContentElement) then
         JQuery(TWidgetHack(aNew).ContentElement).Sortable(SortableOptions);
      jQuery(aNewActive).on_('click',@DoActive);
      aParent:=aNew;
      end;
    end;
  JQuery(aNewActive).AddClass('designerActive').prepend(toolbar);
  if assigned(aParent) and Assigned(IDEClient) then
    IDEClient.SendAction('select',New(['widget',aParent.Name,'class',aParent.ClassName,'state',StreamWidget(aParent)]));
end;

procedure TDesignDemo.SetAddMode(aRegisteredWidget : TRegisteredWidget);

begin
  FAddWidget:=aRegisteredWidget;
end;

procedure TDesignDemo.DoWidgetAddClick(Sender: TObject; Event: TJSEvent);
begin
  SetAddMode((Sender as TWidgetButtonWidget).MyWidget);
end;

procedure TDesignDemo.AddWidgetByName(aID : NativeInt; AName : String);

Var
  I : integer;
  Btn : TWidgetButtonWidget;

begin
  I:=FRegisteredWidgets.Count-1;
  While (i>=0) and Not SameText(TRegisteredWidget(FRegisteredWidgets[i]).WidgetClass.ClassName,aName) do
    Dec(I);
  if I<0 then exit;
  SetAddMode(TRegisteredWidget(FRegisteredWidgets[i]));
  FConfirmAdd:=aID;
end;

procedure TDesignDemo.DoCommandsReceived(Sender: TObject; aCommands: TJSArray);

var
  J,P : TJSOBject;
  aName : String;
  aID : NativeInt;
  I : integer;

begin
  for I:=0 to aCommands.Length-1 do
    begin
    J:=TJSObject(aCommands[i]);
    aName:=String(J['name']);
    aID:=NativeInt(J['id']);
    p:=TJSObject(J['payload']);
    case aName of
      'addWidget' : AddWidgetByName(aID,String(P['class']));
    end;
    end;
end;

procedure TDesignDemo.SetIDEClient(AValue: TIDEClient);
begin
  if FIDEClient=AValue then Exit;
  FIDEClient:=AValue;
  if assigned(FIDEClient) then
    begin
    FIDEClient.OnCommands:=@DoCommandsReceived;
    FIDEClient.StartCommandPolling;
    FToolBar.Visible:=False;
    end;
end;

function TDesignDemo.SortableOptions: TJSObject;

begin
  Result:=New([
    'items','> [data-ww-element-top]',
    'connectWith','[data-ww-element-content]',
    'placeholder','designerPlaceholder',
    'tolerance','pointer',
//    'containment',TPageHack(FPage).Element,
//    'handle','[data-ww-element-top]',
    'cancel',''
  ]);
end;

constructor TDesignDemo.Create(aOwner: TComponent);

begin
  inherited Create(aOwner);
  FRegisteredWidgets:=TObjectList.Create;
  RegisterWidgets;
  FillToolBar;
  SetUpPage;
  //  []
  JQuery(SSortableSelect).Sortable(SortableOptions);
  jQuery('#designpage').on_('click','[data-ww-element-top]',@DoActive);
  jQuery('#designpage').on_('click',@DoActive);
end;

destructor TDesignDemo.Destroy;
begin
  FreeAndNil(FRegisteredWidgets);
  inherited Destroy;
end;

procedure TDesignDemo.RegisterWidgets;

begin
  RegisterWidget(TButtonWidget,'button');
  RegisterWidget(TCheckBoxInputWidget,'checkbox');
  RegisterWidget(TRadioInputWidget,'radio');
  RegisterWidget(TTextInputWidget,'edit');
  RegisterWidget(TImageWidget,'image');
  RegisterWidget(TTextAreaWidget,'memo');
  RegisterWidget(TSelectWidget,'select');
  RegisterWidget(TContainerWidget,'container');
  RegisterWidget(TJumboWidget,'jumbo');
end;

procedure TDesignDemo.RegisterWidget(aClass: TWebWidgetClass; aImageName: String);
begin
  FRegisteredWidgets.Add(TRegisteredWidget.Create(aClass,aImageName));
end;

procedure TDesignDemo.SetupPage;

Const
  ButtonClasses : Array[0..8] of string
    = ('primary','secondary','success','danger','warning','info','light','dark','link');

var
  I : Integer;

begin
  FPage:=TWebPage.Create(Self);
  FPage.ElementID:='designpage';
  FPage.Refresh;
  For I:=0 to 9 do
    begin
    FButtons[I]:=TButtonWidget.Create(FPage);
    FButtons[I].Classes:='btn btn-'+ButtonClasses[I mod 9];
    FButtons[I].Text:='Button #'+IntToStr(I+1);
    FButtons[I].Parent:=FPage;
    FButtons[I].Refresh;
    end;
end;

procedure TDesignDemo.FillToolBar;

Var
  RW : TRegisteredWidget;
  I : Integer;
  Btn : TWidgetButtonWidget;

begin
  FToolBar:=TContainerWidget.Create(Self);
  FToolbar.Styles.EnsureStyle('min-height','34px');
  FToolbar.Styles.RemoveStyle('width');
  FToolbar.Styles.RemoveStyle('height');
  FToolbar.ElementId:='toolbar';
  FToolbar.Refresh;
  SetLength(FWidgetButtons,FRegisteredWidgets.Count);
  For I:=0 to FRegisteredWidgets.Count-1 do
    begin
    RW:=TRegisteredWidget(FRegisteredWidgets[I]);
    Btn:=TWidgetButtonWidget.Create(Self);
    FWidgetButtons[I]:=Btn;
    Btn.MyWidget:=RW;
    Btn.Classes:='btn btn-light';
    Btn.Text:='';
    Btn.Parent:=FToolbar;
    Btn.Styles.Add('background-image','url("widgets/'+RW.ImageName+'.png")');
    Btn.Refresh;
    Btn.Data['widgetClass']:=RW.WidgetClass.ClassName;
    Btn.OnClick:=@DoWidgetAddClick;
    end;
end;

end.

