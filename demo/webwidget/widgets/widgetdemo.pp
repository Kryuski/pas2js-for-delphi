unit widgetdemo;

{$mode objfpc}

interface

uses
  Classes, SysUtils, web, webwidget;

Type
  TDemoRegistration = Class;

  { TDemoContainer }

  TDemoContainer = class(TWebWidget)
  private
    FWidgetInstance: TCustomWebWidget;
  Protected
    // Event that can be used by descendents to attach to OnClick
    procedure DoClick(Sender: TObject; Event: TJSEvent); virtual;
    // Event that can be used by descendents to attach to OnChange
    procedure DoChange(Sender: TObject; Event: TJSEvent); virtual;
    // Override this to return something else but the widget itself
    function GetInspectorInstance: TObject; virtual;
  Public
    function HTMLTag: String; override;
    function DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement; override;
    Class Function Demohelp : String; virtual;
    Class Function Description : String; virtual;
    Class function WebWidgetClass : TCustomWebWidgetClass; virtual; abstract;
    Class Function RegisterDemo : TDemoRegistration;
    // The widget instance
    Property WidgetInstance : TCustomWebWidget Read FWidgetInstance;
    // The object to show in object inspector. By default, this is the widget instance
    Property InspectorInstance : TObject Read GetInspectorInstance;
    Procedure ShowDemo; virtual;
  end;
  TDemoContainerClass = Class of TDemoContainer;

  { TDemoRegistration }

  TDemoRegistration = class(TCollectionItem)
  private
    FDemoclass: TDemoContainerClass;
  Public
    Procedure Assign(Source : TPersistent); override;
    Property DemoClass : TDemoContainerClass Read FDemoclass;
  end;

  { TDemoRegistrationList }

  TDemoRegistrationList = Class(TCollection)
  private
    function GetD(aIndex : integer): TDemoRegistration;
  Public
    Function IndexOf(aClassName: string): Integer;
    Function Find(aClassName: string): TDemoRegistration;
    Function AddDemo(aClass : TDemoContainerClass) : TDemoRegistration;
    Property Demos[aIndex : integer] : TDemoRegistration read GetD; default;
  end;

  { TDemofactory }

  TDemofactory = Class(TComponent)
  Private
    Class var _Instance : TDemofactory;
  Private
    FList : TDemoRegistrationList;
    function GetC: Integer;
    function GetD(aIndex : Integer): TDemoRegistration;
  Public
    Constructor Create(aOwner : TComponent); override;
    Destructor Destroy; override;
    Class Function Instance : TDemoFactory;
    Function RegisterDemo(aClass : TDemoContainerClass) : TDemoRegistration;
    Function FindDemo(aClassName : String) : TDemoRegistration;
    Function FindDemoClass(aClassName : String) : TDemoContainerClass;
    Property Demos[aIndex : Integer] : TDemoRegistration Read GetD;
    Property DemoCount : Integer Read GetC;
  end;

Function DemoFactory : TDemofactory;


implementation

uses typinfo;

Function DemoFactory : TDemofactory;

begin
  Result:=TDemoFactory.Instance;
end;

{ TDemoRegistration }

procedure TDemoRegistration.Assign(Source: TPersistent);
begin
  if Source is TDemoRegistration then
     Self.FDemoclass:=(Source as TDemoRegistration).DemoClass;
  inherited Assign(Source);
end;

{ TDemofactory }

function TDemofactory.GetC: Integer;
begin
  Result:=FList.Count;
end;

function TDemofactory.GetD(aIndex : Integer): TDemoRegistration;
begin
  Result:=Flist[aIndex];
end;

constructor TDemofactory.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FList:=TDemoRegistrationList.Create(TDemoRegistration);
end;

destructor TDemofactory.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

class function TDemofactory.Instance: TDemoFactory;
begin
  If _instance=Nil then
    _Instance:=TDemofactory.Create(Nil);
  Result:=_Instance;
end;

function TDemofactory.RegisterDemo(aClass: TDemoContainerClass): TDemoRegistration;
begin
  Result:=FList.AddDemo(AClass);
end;

function TDemofactory.FindDemo(aClassName: String): TDemoRegistration;
begin
  Result:=FList.Find(aClassName);
end;

function TDemofactory.FindDemoClass(aClassName: String): TDemoContainerClass;

Var
  R : TDemoRegistration;

begin
  R:=FindDemo(aClassName);
  if Assigned(R) then
    Result:=R.DemoClass
  else
    Result:=Nil;
end;

{ TDemoRegistrationList }

function TDemoRegistrationList.GetD(aIndex : integer): TDemoRegistration;
begin
  Result:=TDemoRegistration(Items[aIndex]);
end;

function TDemoRegistrationList.IndexOf(aClassName: string): Integer;
begin
  Result:=Count-1;
  while (Result>=0) and Not SameText(aClassName,GetD(Result).DemoClass.ClassName) do
    Dec(Result);
end;

function TDemoRegistrationList.Find(aClassName: string): TDemoRegistration;

Var
  Idx : integer;

begin
  Idx:=IndexOf(aClassName);
  if Idx<>-1 then
    Result:=GetD(Idx)
  else
    Result:=Nil;
end;

function TDemoRegistrationList.AddDemo(aClass: TDemoContainerClass): TDemoRegistration;
begin
  Result:=Add as TDemoRegistration;
  Result.FDemoclass:=aClass;
end;

{ TDemoContainer }

function TDemoContainer.GetInspectorInstance: TObject;
begin
  Result:=WidgetInstance;
end;

procedure TDemoContainer.DoClick(Sender: TObject; Event: TJSEvent);
begin
  Writeln(Sender.ClassName,' : OnClick event');
end;

procedure TDemoContainer.DoChange(Sender: TObject; Event: TJSEvent);
begin
  Writeln(Sender.ClassName,' : OnChange event');
  if IsPublishedProp(Sender,'Checked') then
    Writeln(Sender.ClassName,' checked value :', GetBoolProp(WidgetInstance,'Checked'));
  if IsPublishedProp(Sender,'Value') then
    Writeln(Sender.ClassName,' Value :', GetJSValueProp(WidgetInstance,'Value'));
end;

function TDemoContainer.HTMLTag: String;
begin
  result:='DIV';
end;

function TDemoContainer.DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement;

begin
  Result:=inherited DoRenderHTML(aParent, aElement);
end;

class function TDemoContainer.Demohelp: String;
begin
  Result:='';
end;

class function TDemoContainer.Description: String;
begin
  Result:=WebwidgetClass.ClassName+' demo';
end;

class function TDemoContainer.RegisterDemo: TDemoRegistration;
begin
  Result:=TDemoFactory.Instance.RegisterDemo(Self);
end;

procedure TDemoContainer.ShowDemo;

  Function MaybeSet(aName : string) : boolean;

  begin
    Result:=IsPublishedProp(FWidgetInstance,aName) and
            (GetPropInfo(FWidgetInstance,aName).TypeInfo.Kind=tkString);
    if result then
       SetStrProp(FWidgetInstance,aName,'Demo '+FWidgetInstance.Classname);
  end;

begin
  FreeAndNil(FWidgetInstance);
  if Assigned(WebWidgetClass) then
    begin
    FWidgetInstance:=WebWidgetClass.Create(Self);
    FWidgetInstance.Name:=WebWidgetClass.ClassName+'1';
    FWidgetInstance.Parent:=Self;
    if not MaybeSet('Caption') then
      MaybeSet('Text');
    FWidgetInstance.Refresh;
    end;
end;

end.

