program demoload;

uses SysUtils, Classes, JS, Web, DB, JSONDataset, ExtJSDataset, RestConnection;

Type

  { TRestDataset }

  TRestDataset = Class(TExtJSJSONObjectDataSet)
  private
    FConnection: TRestConnection;
  Protected
    Function DoGetDataProxy: TDataProxy; override;
  Public
    Property Connection: TRestConnection Read FConnection Write FConnection;
  end;

  { TForm }

  TForm = Class
    Table,
    TBody : TJSElement;
    Panel,
    PanelContent,
    Button : TJSElement;
    DS : TRestDataset;
    Conn : TRestConnection;
    Constructor create;
    function CreateTable: TJSElement;
  private
    procedure AddRecords;
    function ButtonClick(Event: TJSMouseEvent): boolean;
    function CreateRow(AName: String; APopulation: NativeInt): TJSElement;
    procedure DoAfterLoad(DataSet: TDataSet);
    procedure DoGetURL(Sender: TComponent; aRequest: TDataRequest; Var aURL: String);
    procedure DoLoadFail(DataSet: TDataSet; ID: Integer; const ErrorMsg: String);
    procedure DSOpen(DataSet: TDataSet);
  end;

function TRestDataset.DoGetDataProxy: TDataProxy;
begin
  Result:=Connection.DataProxy;
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

procedure TForm.DoAfterLoad(DataSet: TDataSet);
begin
  if Dataset.Active then
    begin
    Writeln('Loading additional records to table');
    // We're on the last record of the last batch, so move forward 1 record
    Dataset.Next;
    AddRecords;
    end;
end;

procedure TForm.DoGetURL(Sender: TComponent; aRequest: TDataRequest; Var aURL: String);
begin
  aURL:='countries-'+IntToStr(aRequest.requestID)+'.json';
end;

procedure TForm.DoLoadFail(DataSet: TDataSet; ID: Integer; const ErrorMsg: String);
Var
  N : TJSElement;
begin
  N:=Document.CreateElement('div');
  N.appendChild(Document.createTextNode('Failed to load countries...'+ErrorMsg));
  PanelContent.append(N);
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

procedure TForm.DSOpen(DataSet: TDataSet);

begin
  console.log('Dataset opened');
  Table:=CreateTable;
  Document.Body.append(Table);
  TBody:=document.createElement('TBODY');
  Table.Append(TBody);
  AddRecords;
end;

procedure TForm.AddRecords;

begin
  Writeln('Adding records to table');
  While not DS.EOF do
    begin
    TBody.Append(CreateRow(DS.FieldByName('Name').AsString,DS.FieldByName('Population').AsInteger));
    DS.Next;
    end;
end;

function TForm.ButtonClick(Event: TJSMouseEvent): boolean;

begin
  DS.Load([],Nil);
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

  Conn:=TRestConnection.Create(nil);
  Conn.BaseURL:='countries.json';
  Conn.OnGetURL:=@DoGetURL;

  DS:=TRestDataset.Create(Nil);
  DS.Connection:=Conn;
  DS.OnLoadFail:=@DoLoadFail;
  DS.AfterLoad:=@DoAfterLoad;
  DS.AfterOpen:=@DSOpen;
end;

begin
  TForm.Create;
end.

