program demoxhr;

uses SysUtils, JS, Web, DB, JSonDataset, ExtJSDataset, strutils, DBConst;

Type

  { TForm }

  TForm = Class
    XHR : TJSXMLHttpRequest;
    Table,
    Panel,
    PanelContent,
    Button : TJSElement;
    DS : TExtJSJSONObjectDataSet;

    function onLoad(Event: TEventListenerEvent): boolean;
    Constructor create;
    function CreateTable: TJSElement;
  private
    function ButtonClick(Event: TJSMouseEvent): boolean;
    function CreateRow(AName: String; APopulation: NativeInt): TJSElement;
  end;


function TForm.CreateRow(AName : String; APopulation : NativeInt) : TJSElement;

Var
  C : TJSElement;

begin
  Result:=document.createElement('TR');
  C:=document.createElement('TD');
  Result.Append(C);
  C.appendChild(Document.createTextNode(AName));
  C:=document.createElement('TD');
  Result.Append(C);
  C.AppendChild(document.createTextNode(IntToStr(APopulation)));
end;

function TForm.CreateTable : TJSElement;

Var
  TH,R,H : TJSElement;

begin
  Result:=document.createElement('TABLE');
  Result.className:='table table-striped table-bordered table-hover table-condensed';
  TH:=document.createElement('THEAD');
  Result.Append(TH);
  R:=document.createElement('TR');
  TH.Append(R);
  H:=document.createElement('TH');
  R.Append(H);
  H.AppendChild(document.createTextNode('Name'));
  H:=document.createElement('TH');
  R.Append(H);
  H.AppendChild(document.createTextNode('Population'));
end;

function TForm.onLoad(Event: TEventListenerEvent): boolean;

var
  i : integer;
  C,J : TJSObject;
  N,TB : TJSElement;

begin
  console.log('Result of call ',xhr.Status);
{  While (PanelContent.childNodes.length>0) do
    PanelContent.removeChild(PanelContent.childNodes.item(PanelContent.childNodes.length-1));}
  if (xhr.status = 200) then
    begin
    J:=TJSJSON.parseObject(xhr.responseText);
    DS.Metadata:=TJSObject(J.Properties['metaData']);
    DS.Rows:=TJSArray(J.Properties['Data']);
    DS.Open;
    Table:=CreateTable;
    Document.Body.append(Table);
    TB:=document.createElement('TBODY');
    Table.Append(TB);
    While not DS.EOF do
      begin
      TB.Append(CreateRow(DS.FieldByName('Name').AsString,DS.FieldByName('Population').AsInteger));
      DS.Next;
      end;
    end
  else
    begin
    N:=Document.CreateElement('div');
    N.appendChild(Document.createTextNode('Failed to load countries: '+IntToStr(xhr.Status)));
    PanelContent.append(N);
    end;
  Result := True;
end;

function TForm.ButtonClick(Event: TJSMouseEvent): boolean;

begin
  xhr:=TJSXMLHttpRequest.New;
  xhr.addEventListener('load', @OnLoad);
  xhr.open('GET', 'countries.json', true);
  xhr.send;
  Result:=true;
end;

constructor TForm.create;


begin
  Panel:=document.createElement('div');
  // attrs are default array property...
  Panel['class']:='panel panel-default';
  PanelContent:=document.createElement('div');
  PanelContent['class']:='panel-body';
  Button:=document.createElement('input');
  Button['id']:='Button1';
  Button['type']:='submit';
  Button.className:='btn btn-default';
  Button['value']:='Fetch countries';
  TJSHTMLElement(Button).onclick:=@ButtonClick;
  document.body.appendChild(panel);
  Panel.appendChild(PanelContent);
  PanelContent.appendChild(Button);
  DS:=TExtJSJSONObjectDataSet.Create(Nil);
end;

begin
  TForm.Create;
end.

