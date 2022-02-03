unit frmDemo;

{$mode objfpc}

interface

uses
  Classes, SysUtils, web, webwidget, htmlwidgets, widgetdemo, propertygridwidget, webrouter;

Type

  { THelpDemoContainer }

  THelpDemoContainer = class(TDemoContainer)
  Protected
    Function HTMLTag: String; override;
    Procedure ApplyWidgetSettings(aElement: TJSHTMLElement); override;
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
  end;
  { TDemoForm }

  TDemoForm = Class(TComponent)
  Private
    Fcontainer : TDemoContainer;
    FSelectedClass : TDemoContainerClass;
    FDemoParent : TJSHTMLElement;
    FPropertyGrid : TSimplePropertyGridWidget;
    FCBshowconsole: TJSHTMLInputelement;
    FConsole: TJSHTMLElement;
    Flist : TSimpleLoopTemplateWidget;
    procedure DoGetValue(Sender: TObject; aValue: TLoopTemplateValue);
    procedure DoRoute(URl: String; aRoute: TRoute; Params: TStrings);
    procedure DoSelectDemo(Sender: TObject; Event: TJSEvent);
    function DoShowConsole(Event: TEventListenerEvent): boolean;
    procedure ShowSelected;
  Public
    Constructor Create(aOwner : TComponent) ; override;
    Procedure Show;
    property SelectedClass : TDemoContainerClass Read FSelectedClass;
  end;

implementation

Const
  SDemoContainerID = 'democontainer';
  SDemoListID = 'demolist';
  SPropertyGridID = 'propertygrid';
  SShowconsoleID = 'showconsole';
  SpasjsconsoleID = 'pasjsconsole';
{ THelpDemoContainer }

function THelpDemoContainer.HTMLTag: String;
begin
  Result:='p';
end;

procedure THelpDemoContainer.ApplyWidgetSettings(aElement: TJSHTMLElement);
begin
  inherited ApplyWidgetSettings(aElement);
  aElement.innerHTML:='Please select an element in the list located at the left of the screen';
end;

class function THelpDemoContainer.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=Nil;
end;


{ TDemoForm }



procedure TDemoForm.ShowSelected;

Var
  C: TDemoContainerClass;

begin
  C:=FSelectedClass;
  if C=Nil then
    C:=THelpDemoContainer;
  FDemoParent.InnerHTML:='';
  if Assigned(FContainer) then
    FreeAndNil(FContainer);
  FDemoParent.InnerHTML:=''; // To be sure
  FContainer:=C.Create(Self);
  FContainer.Name:='CurrentContainer';
  FContainer.ParentID:=SDemoContainerID;
  FContainer.Refresh;
  FContainer.ShowDemo;
  FPropertyGrid.Subject:=FContainer.InspectorInstance;
  FPropertyGrid.LookupRoot:=FContainer;
  FPropertyGrid.Refresh;
end;

procedure TDemoForm.DoSelectDemo(Sender: TObject; Event: TJSEvent);
begin
{  Writeln('Changed : ',Flist.SelectedIndex);
  if Flist.SelectedIndex>=0 then
    Writeln('Value : ',Flist.Values[Flist.SelectedIndex]);
  FSelectedClass:=TDemoFactory.Instance.FindDemoClass(Flist.Values[Flist.SelectedIndex]);}
  ShowSelected;
end;

function TDemoForm.DoShowConsole(Event: TEventListenerEvent): boolean;
begin
  if FCBShowConsole.checked then
    FConsole.style.cssText:=''
  else
    FConsole.style.cssText:='display: none;'
end;

constructor TDemoForm.Create(aOwner: TComponent);

begin
  inherited Create(aOwner);
  Flist:=TSimpleLoopTemplateWidget.Create(Self);
  Flist.OnGetValue:=@DoGetValue;
  FList.HeaderTemplate:='<div class="list-group">';
  FList.FooterTemplate:='</div>';
  FList.ItemTemplate:='<a href="#/{{DemoName}}/" class="list-group-item list-group-item-action">{{DemoDescription}}</a>';

  FDemoParent:=TJSHTMLelement(Document.getElementById(SdemocontainerID));
  FPropertyGrid:=TSimplePropertyGridWidget.Create(Self);
  FCBshowconsole:=TJSHTMLInputelement(Document.getElementById(SShowconsoleID));
  FCBshowconsole.OnChange:=@DoShowConsole;
  FConsole:=TJSHTMLElement(Document.getElementById(SpasjsconsoleID));
  Router.RegisterRoute(':Demo',@DoRoute,True);
end;

procedure TDemoForm.DoRoute(URl : String; aRoute : TRoute; Params: TStrings);

begin
  writeln('In route: ', Params.Values['Demo']);
  FSelectedClass:=TDemoFactory.Instance.FindDemoClass(Params.Values['Demo']);
  ShowSelected;
end;

procedure TDemoForm.DoGetValue(Sender: TObject; aValue: TLoopTemplateValue);

Var
  F : TDemoFactory;
begin
  F:=TDemoFactory.Instance;
  if aValue.Name='DemoName' then
    aValue.Value:=F.Demos[aValue.Index].DemoClass.ClassName
  else if aValue.Name='DemoDescription' then
    aValue.Value:=F.Demos[aValue.Index].DemoClass.Description
end;

procedure TDemoForm.Show;

Var
  F : TDemoFactory;
  I : Integer;

begin
  F:=TDemoFactory.Instance;
  Flist.ParentID:=SDemoListID;
{  for I:=0 to F.DemoCount-1 do
    begin
    Flist.Items.Add(F.Demos[i].DemoClass.Description);
    Flist.Values.Add(F.Demos[i].DemoClass.ClassName);
    end;
  Flist.OnChange:=@DoSelectDemo;
}
  FList.ItemCount:=DemoFactory.DemoCount;
  Writeln('Demo count: ',FList.ItemCount);
  Flist.Refresh;
  FPropertyGrid.ParentID:=SPropertyGridID;
  ShowSelected;
end;

end.

