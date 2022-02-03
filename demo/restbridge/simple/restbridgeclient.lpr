program restbridgeclient;

{$mode objfpc}

uses
  JS, Classes, SysUtils, Web, db, jsondataset, sqldbrestdataset;

Type

  { TMainForm }

  TMainForm = class(TComponent)
  Private
    FData: TSQLDBRestDataset;
    FConn: TSQLDBRestConnection;
    FBtnResources : TJSHTMLButtonElement;
    FBtnData : TJSHTMLButtonElement;
    FEdtURL : TJSHTMLInputElement;
    FEdtUserName : TJSHTMLInputElement;
    FEdtPassword : TJSHTMLInputElement;
    FSelResource : TJSHTMLSelectElement;
    FDataHead : TJSHTMLElement;
    FDataBody : TJSHTMLElement;
    function ConfigureConnection: Boolean;
    function ConfigureDataset: Boolean;
    procedure CreateDataHead(Dataset: TDataset);
    function CreateDataRow(aRowNo: Integer; Dataset: TDataset): String;
    procedure CreateDataTable(Dataset: TDataset);
    procedure DoGetResources(Sender: TObject);
    procedure DoOpen(DataSet: TDataSet);
    function GetData(aEvent: TJSMouseEvent): boolean;
    function GetElement(const aID: String): TJSHTMLElement;
    function GetResources(aEvent: TJSMouseEvent): boolean;
  Public
    Constructor Create(aOwner : TComponent); override;
    Procedure BindElements;
  end;

{ TMainForm }

Function TMainForm.ConfigureDataset : Boolean;

begin
  FData.ResourceName:=FSelResource.value;
  Result:=(FData.ResourceName<>'');
  if not Result then
    Window.Alert('Dataset not correctly configured');
end;

Function TMainForm.ConfigureConnection : Boolean;

begin
  FConn.BaseURL:=FedtURL.value;
  FConn.Password:=FEdtPassword.value;
  FConn.UserName:=FEdtUserName.value;
  Result:=FConn.BaseURL<>'';
  if not Result then
    Window.Alert('Connection not correctly configured');
end;

function TMainForm.GetResources(aEvent: TJSMouseEvent): boolean;
begin
  if ConfigureConnection then
    FConn.GetResources
end;

procedure TMainForm.DoGetResources(Sender: TObject);

Var
  S : String;
  I : Integer;

begin
  S:='<option selected>Choose...</option>';
  For I:=0 to FConn.ResourceList.Count-1 do
    S:=S+sLineBreak+'<option>'+FConn.ResourceList[i]+'</option>';
  FSelResource.innerHTML:=S;
end;

procedure TMainForm.CreateDataHead(Dataset : TDataset);

Var
  sHTML : String;
  I : integer;

begin
  sHTML:='<tr>';
  sHTML:=sHTML+'<th>#</th>';
  For I:=0 to Dataset.FieldDefs.Count-1 do
    sHTML:=sHTML+'<th>'+Dataset.FieldDefs[i].Name+'</th>'+sLineBreak;
  sHTML:=sHTML+'</tr>';
  FDataHead.innerHTML:=sHTML;
end;

Function TMainForm.CreateDataRow(aRowNo : Integer; Dataset : TDataset) : String;

Var
  I : integer;
  sHTML : String;

begin
  // Prepend dataset name to id?
  sHtml:=Format('<tr id="row-%d"><td>%d</td>',[DataSet.RecNo,aRowNo]);
  For I:=0 to Dataset.Fields.Count-1 do
    sHTML:=sHTML+'<td>'+Dataset.Fields[i].AsString+'</td>'+sLineBreak;
  sHTML:=sHtml+'</tr>';
  Result:=sHTML;
end;

procedure TMainForm.CreateDataTable(Dataset : TDataset);

Var
  sHTML : String;
  I : integer;

begin
  sHTML:='';
  I:=0;
  while not Dataset.EOF do
    begin
    inc(i);
    sHtml:=SHTML+CreateDataRow(i,Dataset);
    Dataset.Next;
    end;
  FDataBody.innerHTML:=sHTML;
end;


procedure TMainForm.DoOpen(DataSet: TDataSet);
begin
  CreateDataHead(Dataset);
  CreateDataTable(Dataset);
end;

function TMainForm.GetData(aEvent: TJSMouseEvent): boolean;
begin
  If not ConfigureConnection then
    exit;
  if not ConfigureDataset then
    exit;
  FData.Load([],Nil);
end;

constructor TMainForm.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  // Ideally, this is done in an IDE
  FConn:=TSQLDBRestConnection.Create(Self);
  FCOnn.OnGetResources:=@DoGetResources;
  FData:=TSQLDBRestDataset.Create(Self);
  FData.AfterOpen:=@DoOpen;
  FData.Connection:=FConn;
  // This must always be done in code
  BindElements;
end;

Function TMainForm.GetElement(Const aID : String): TJSHTMLElement;

begin
  Result:=TJSHTMLElement(document.getElementById(aID));
  if (Result=Nil) then
    Console.Log('Could not find element '+aID);
end;

procedure TMainForm.BindElements;
begin
  FBtnResources:=TJSHTMLButtonElement(getElement('btnResources'));
  FBtnResources.OnClick:=@GetResources;
  FBtnData:=TJSHTMLButtonElement(getElement('btnFetch'));
  FBtnData.OnClick:=@GetData;
  FSelResource:=TJSHTMLSelectElement(GetElement('selResource'));
  FEdtURL:=TJSHTMLInputElement(getElement('edtURL'));
  FEdtUserName:=TJSHTMLInputElement(getElement('edtUserName'));
  FEdtPassword:=TJSHTMLInputElement(getElement('edtPassword'));
  FDataHead:=getElement('datahead');
  FDataBody:=getElement('databody');
end;


begin
  TMainForm.Create(Nil);
end.
