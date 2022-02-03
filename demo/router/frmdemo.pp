unit frmdemo;

{$mode objfpc}{$H+}

interface

uses
  sysutils, classes, web, webrouter;

Type

  { TDemoForm }

  TDemoForm = Class
    Panel,
    PanelContent,
    labelContent : TJSElement;
    function DismissAlert(Event: TJSMouseEvent): boolean;
    function ButtonClick(Event: TJSMouseEvent): boolean;
    constructor Create(aFormNo : Integer; UseSlash : Boolean = False);
  private
    function DoLinkClick(aEvent: TJSMouseEvent): boolean;
  end;


implementation


function TDemoForm.DismissAlert(Event: TJSMouseEvent): boolean;
begin
  PanelContent.removeChild(labelContent);
  Result:=true;
end;

function TDemoForm.ButtonClick(Event: TJSMouseEvent): boolean;
begin
  Router.Push(String(TJSHTMLElement(Event.target).Dataset['link']));
end;

constructor TDemoForm.Create(aFormNo: Integer; UseSlash: Boolean);

Var
  adiv, Link, Button : TJSHTMLElement;
  i : integer;

  Function MakeLink(aID : integer; ForURL : Boolean) : String;

  begin
    Result:='/form';
    if forURL and (Router.HistoryKind=hkHash) then
      Result:='#'+Result;
    if useSlash then
      Result:=Result+'/';
    Result:=Result+IntToStr(aID);
  end;

begin
  Panel:=document.createElement('div');
  // attrs are default array property...
  Panel['class']:='panel panel-default';
  PanelContent:=document.createElement('div');
  PanelContent['class']:='panel-body';
  labelContent:=document.createElement('div');
  labelContent.innerHTML:='Hello from form '+IntToStr(aFormNo);
  labelContent['role']:='alert';
  labelContent['class']:='alert alert-info';
  TJSHTMLElement(labelContent).onclick:=@DismissAlert;
  aDiv:=TJSHTMLElement(document.createElement('div'));
  aDiv['style']:='padding: 0px 0px 10px 0px;';
  PanelContent.appendChild(adiv);
  For I:=1 to 5 do
    if (I<>aFormNo) then
      begin
      Link:=TJSHTMLElement(document.createElement('a'));
      link['href']:=MakeLink(i,True);
      link.innerHTML:='Go to form <span class="badge">'+IntToStr(i)+'</span>';
      if (Router.HistoryKind<>hkHTML5) then
        Link.onclick:=@DoLinkClick;
      adiv.appendChild(link);
      end;
  PanelContent.appendChild(labelContent);
  For I:=1 to 5 do
    if (I<>aFormNo) then
      begin
      Button:=TJSHTMLElement(document.createElement('button'));
      Button['class']:='btn btn-default';
      Button.InnerHTML:='Go to form '+IntToStr(i);
      Button.Dataset['link']:=MakeLink(i,false);
      Button.onclick:=@ButtonClick;
      PanelContent.appendChild(Button);
      end;
  document.body.appendChild(Panel);
  Panel.appendChild(PanelContent);
end;

function TDemoForm.DoLinkClick(aEvent: TJSMouseEvent): boolean;

Var
  URL : String;
  p: Integer;

begin
  URL:=String(aEvent.target['href']);
  P:=Pos('#',URL);
  URL:=Copy(URL,P+1,Length(URL)-P);
  Writeln('URL :',URL);
  Router.Push(URL);
  aEvent.preventDefault;
end;

end.

