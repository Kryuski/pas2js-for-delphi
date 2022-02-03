program demobootstraptable;

{$mode objfpc}

uses
  JS, Classes, SysUtils, Web, libjquery, libbootstraptable, browserapp, DB, ExtJSDataset;

Type

  { TDemo }

  TDemo = class(TBrowserApplication)
  Private
    xhr:TJSXMLHttpRequest;
    FDS : TExtJSJSONObjectDataSet;
    function CreateRow(AName: String; APopulation: NativeInt): TJSHTMLElement;
    procedure CreateTable;
    function onLoad(Event: TEventListenerEvent): boolean;
  Protected
    procedure DoRun; override;
  end;

{ TDemo }

procedure TDemo.doRun;

Var
  O : TBootstrapTableOptions;

begin
  inherited doRun;
  // Using plain TJSObject
  JQuery('#table-prefilled').BootStrapTable(New(['search',true,'showColumns',true]));
  // Using TBootstrapTableOptions
  O:=TBootstrapTableOptions.new;
  O.search:=true;
  O.showColumns:=true;
  O.pagination:=true;
  O.showExtendedPagination:=True;
  O.url:='countries.json';
  O.sidePagination:='client';
  O.dataField:='Data';
  JQuery('#table-data').BootStrapTable(O);
  JQuery('#table-data').RefreshTable(New([]));
  // Using dataset
  FDS:=TExtJSJSONObjectDataSet.Create(Nil);
  xhr:=TJSXMLHttpRequest.New;
  xhr.addEventListener('load', @OnLoad);
  xhr.open('GET', 'countries.json', true);
  xhr.send;
end;

function TDemo.onLoad(Event: TEventListenerEvent): boolean;

var
  i : integer;
  C,J : TJSObject;
  N,TB : TJSElement;

begin
  if (xhr.status = 200) then
    begin
    J:=TJSJSON.parseObject(xhr.responseText);
    FDS.Metadata:=TJSObject(J.Properties['metaData']);
    FDS.Rows:=TJSArray(J.Properties['Data']);
    FDS.Open;
    CreateTable;
    end
  else
    begin
    N:=Document.CreateElement('div');
    N.appendChild(Document.createTextNode('Failed to load countries: '+IntToStr(xhr.Status)));
    document.GetElementByID('tab-manual').appendChild(N);
    end;
end;

procedure TDemo.CreateTable;

Var
  TB,P,R,C : TJSHTMLElement;
  FName,FPop : TField;
  O : TBootstrapTableOptions;

begin
  TB:=GetHTMLElement('table-manual');
  P:=CreateHTMLElement('THEAD');
  TB.AppendChild(P);
  R:=CreateHTMLElement('TR');
  P.AppendChild(R);
  C:=CreateHTMLElement('TH');
  C.InnerText:='Name';
  C.Dataset['field']:='Name';
  R.AppendChild(C);
  C:=CreateHTMLElement('TH');
  C.InnerText:='Pop.';
  C.Dataset['field']:='Population';
  R.AppendChild(C);
  P:=CreateHTMLElement('TBODY');
  TB.AppendChild(P);
  FName:=FDS.FieldByName('Name');
  FPop:=FDS.FieldByName('Population');
  While not FDS.EOF do
    begin
    P.AppendChild(CreateRow(FName.AsString,FPop.AsInteger));
    FDS.Next;
    end;
  O:=TBootstrapTableOptions.new;
  O.search:=true;
  O.showColumns:=true;
  O.pagination:=true;
  O.showExtendedPagination:=True;
  O.url:='countries.json';
  O.sidePagination:='client';
  O.dataField:='Data';
  JQuery(TB).BootStrapTable(O);
end;

function TDemo.CreateRow(AName : String; APopulation : NativeInt) : TJSHTMLElement;

Var
  C : TJSHTMLElement;

begin
  Result:=createHTMLElement('TR');
  C:=createHTMLElement('TD');
  Result.Append(C);
  C.appendChild(Document.createTextNode(AName));
  C:=createHTMLElement('TD');
  Result.Append(C);
  C.AppendChild(document.createTextNode(IntToStr(APopulation)));
end;



begin
  With TDemo.Create(Nil) do
    begin
    Initialize;
    Run;
    end;
end.
