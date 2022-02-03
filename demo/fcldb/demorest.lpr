program demorest;

uses sysutils, classes, js, web, db, extjsdataset, jsondataset, restconnection;

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
  private
    Table,
    TBody : TJSElement;
    Panel,
    PanelContent,
    ButtonDelete,
    ButtonAdd,
    ButtonApplyUpdates,
    ButtonChange,
    ButtonFetch : TJSElement;
    EName,
    EPopulation: TJSHTMLInputElement;
    DS : TRestDataset;
    Conn : TRestConnection;
  Public
    Constructor create;
    function CreateTable: TJSElement;
    procedure AddRecords;
    function ApplyUpdatesClick(aEvent{%H-}: TJSMouseEvent): boolean;
    function ButtonAddClick(aEvent: TJSMouseEvent): boolean;
    function ButtonDeleteClick(aEvent{%H-}: TJSMouseEvent): boolean;
    function ButtonFetchClick(Event{%H-}: TJSMouseEvent): boolean;
    function CreateInput(aParent: TJSElement; Atype, AName, ALabel, aID: String): TJSHTMLInputElement;
    function CreateRow(AID : Integer; AName: String; APopulation: NativeInt): TJSElement;
    function DoAddRecord(aEvent{%H-}: TJSMouseEvent): boolean;
    procedure DoAfterLoad(DataSet: TDataSet);
    function DoEditRecord(aEvent{%H-}: TJSMouseEvent): boolean;
    procedure DoGetURL(Sender: TComponent; aRequest: TDataRequest; Var aURL: String);
    procedure DoLoadFail(DataSet{%H-}: TDataSet; ID{%H-}: Integer; const ErrorMsg: String);
    procedure DSAfterApplyUpdates(DataSet{%H-}: TDataSet; Updates{%H-}: TResolveResults);
    procedure DSOpen(DataSet{%H-}: TDataSet);
    procedure ResetButtons(Sender: TDataset);
    function SelectRecord(aEvent: TJSMouseEvent): boolean;
  end;

function TRestDataset.DoGetDataProxy: TDataProxy;
begin
  Result:=Connection.DataProxy;
end;


function TForm.CreateRow(AID : Integer; AName : String; APopulation : NativeInt) : TJSElement;

Var
  C : TJSElement;
  S : String;

begin
  Result:=document.createElement('TR');
  S:=IntToStr(AID);
  Result.id:=S;
  C:=document.createElement('TD');
  C.id:=S+'_name';
  Result.Append(C);
  C.appendChild(Document.createTextNode(AName));
  Result.id:=IntToStr(AID);
  C:=document.createElement('TD');
  C.id:=S+'_population';
  Result.Append(C);
  C.AppendChild(document.createTextNode(IntToStr(APopulation)));
  TJSHTMLElement(Result).onclick:=@SelectRecord;
end;

function TForm.DoAddRecord(aEvent: TJSMouseEvent): boolean;

begin
  DS.Append;
  DS.FieldByName('name').AsString:=EName.value;
  DS.FieldByName('population').AsString:=EPopulation.value;
  DS.Post;
  Result:=True;
end;

function TForm.DoEditRecord(aEvent: TJSMouseEvent): boolean;

Var
  E : TJSElement;

begin
  Writeln('Updating record: ',DS.RecNo);
  DS.Edit;
  DS.FieldByName('name').AsString:=EName.value;
  DS.FieldByName('population').AsString:=EPopulation.value;
  DS.Post;
  E:=Document.getElementById(IntToStr(DS.RecNo)+'_name');
  if Assigned(E) then
    E.innerText:=EName.value;
  E:=Document.getElementById(IntToStr(DS.RecNo)+'_population');
  if Assigned(E) then
    E.innerText:=EPopulation.value;
  Result:=True;
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
  aURL:='http://localhost:3000/countries/?limit=20&offset='+IntToStr((aRequest.RequestID-1)*20);
end;

procedure TForm.DoLoadFail(DataSet: TDataSet; ID: Integer; const ErrorMsg: String);
Var
  N : TJSElement;
begin
  N:=Document.CreateElement('div');
  N.appendChild(Document.createTextNode('Failed to load countries...'+ErrorMsg));
  PanelContent.append(N);
end;

procedure TForm.DSAfterApplyUpdates(DataSet: TDataSet; Updates : TResolveResults);
begin
  Window.Alert('Updates applied on server!');
  EName.value:='';
  EPopulation.value:='0';
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
  if not Assigned(Table) then
    begin
    Table:=CreateTable;
    Document.Body.append(Table);
    TBody:=document.createElement('TBODY');
    Table.Append(TBody);
    end;
  DS.First;
  AddRecords;
end;

function TForm.SelectRecord(aEvent: TJSMouseEvent): boolean;

Var
  aID : integer;
  e : TJSElement;

begin
  e:=TJSElement(aEvent.target);
  While Assigned(e) and Not SameText(e.nodeName,'tr') do
    e:=e.parentElement;
  if Not Assigned(E) then exit;
  aId:=StrToIntDef(e.ID,-1);
  Writeln('Jumping to record : ',aID);
  if (AID<>-1) then
    begin
    DS.RecNo:=AID;
    EName.value:=DS.FieldByName('Name').AsString;
    EPopulation.value:=DS.FieldByName('Population').AsString;
    TJSHTMLInputElement(ButtonChange).disabled:=false;
    TJSHTMLInputElement(ButtonDelete).disabled:=false;
    end;
  Result:=true;
end;

procedure TForm.AddRecords;

begin
  While not DS.EOF do
    begin
    // Writeln(DS.RecNo, ' - ',DS.FieldByName('Name').AsString,'-',DS.FieldByName('Population').AsInteger);
    TBody.Append(CreateRow(DS.RecNo, DS.FieldByName('Name').AsString,DS.FieldByName('Population').AsInteger));
    DS.Next;
    end;
  TJSHTMLInputElement(ButtonChange).disabled:=false;
end;

function TForm.ApplyUpdatesClick(aEvent: TJSMouseEvent): boolean;
begin
  DS.ApplyUpdates;
  Result:=true;
end;

function TForm.ButtonAddClick(aEvent: TJSMouseEvent): boolean;
begin
  DS.Append;
  DS.FieldByname('Name').AsString:=EName.value;
  DS.FieldByname('Population').AsLargeInt:=StrToInt(EPopulation.Value);
  DS.Post;
  AddRecords;
end;

function TForm.ButtonDeleteClick(aEvent: TJSMouseEvent): boolean;

Var
  ID : Integer;
  E : TJSElement;

begin
  ID:=DS.RecNo;
  if ID=0 then ;
  DS.Delete;
  EName.value:='';
  EPopulation.Value:='';
  E:=Document.getElementById(IntToStr(DS.RecNo));
  if E<>Nil then
    E.parentElement.removeChild(e);
  Result:=true;
end;

function TForm.ButtonFetchClick(Event: TJSMouseEvent): boolean;

begin
  if Assigned(TBody) then
    TBody.innerHTML:='';
  DS.Load([],Nil);
  Result:=true;
end;

Function TForm.CreateInput(aParent : TJSElement; Atype,AName,ALabel,aID : String) : TJSHTMLInputElement;

Var
  aDiv,aLabelDiv : TJSElement;

begin
  adiv:=document.createElement('div');
  adiv.className:='form-group';
  aLabelDiv:=document.createElement('label');
  aLabelDiv['for']:=aID;
  aLabelDiv.innerText:=ALabel;
  aDiv.appendChild(aLabelDiv);
  Result:=TJSHTMLInputElement(document.createElement('input'));
  Result.id:=AID;
  Result.name:=AName;
  Result['type']:=AType;
  aDiv.appendChild(Result);
  aParent.appendChild(aDiv);
end;

Procedure TForm.ResetButtons (Sender : TDataset);

begin
  TJSHTMLInputElement(ButtonChange).disabled:=True;
  TJSHTMLInputElement(ButtonDelete).disabled:=True;
end;

constructor TForm.create;

Var
  F : TJSElement;

begin
  Panel:=document.createElement('div');
  // attrs are default array property...
  Panel['class']:='panel panel-default';
  PanelContent:=document.createElement('div');
  PanelContent['class']:='panel-body';
  document.body.appendChild(panel);
  Panel.appendChild(PanelContent);
  // fetch button
  ButtonFetch:=document.createElement('button');
  ButtonFetch['id']:='ButtonFetch';
  ButtonFetch.className:='btn btn-default';
  ButtonFetch.InnerText:='Fetch countries';
  TJSHTMLElement(ButtonFetch).onclick:=@ButtonFetchClick;
  PanelContent.AppendChild(ButtonFetch);
  // Input form
  F:=document.createElement('form');
  F.ClassName:='form-inline';
  PanelContent.appendChild(F);
  EName:=CreateInput(F, 'text','EName','Name','IDName');
  EPopulation:=CreateInput(F, 'number','EPopulation','Population','IDPopulation');
  // Add/Apply updates button
  // Add
  ButtonAdd:=document.createElement('button');
  ButtonAdd['id']:='ButtonAdd';
  ButtonAdd.className:='btn btn-default';
  ButtonAdd.InnerText:='Add record';
  TJSHTMLElement(ButtonAdd).OnClick:=@DoAddRecord;
  PanelContent.AppendChild(ButtonAdd);
  // Edit
  ButtonChange:=document.createElement('button');
  ButtonChange['id']:='ButtonChange';
  ButtonChange.className:='btn btn-default';
  ButtonChange.InnerText:='Update record';
  TJSHTMLInputElement(ButtonChange).disabled:=true;
  TJSHTMLElement(ButtonChange).OnClick:=@DoEditRecord;
  PanelContent.AppendChild(ButtonChange);
  // Delete
  ButtonDelete:=document.createElement('button');
  ButtonDelete['id']:='ButtonDelete';
  ButtonDelete.className:='btn btn-default';
  ButtonDelete.InnerText:='Delete record';
  TJSHTMLInputElement(ButtonDelete).disabled:=true;
  TJSHTMLElement(ButtonDelete).OnClick:=@ButtonDeleteClick;
  PanelContent.AppendChild(ButtonDelete);
  // Apply updates
  ButtonApplyUpdates:=document.createElement('button');
  ButtonApplyUpdates['id']:='ButtonAdd';
  ButtonApplyUpdates.className:='btn btn-default';
  ButtonApplyUpdates.InnerText:='Apply updates';
  TJSHTMLElement(ButtonApplyUpdates).onclick:=@ApplyUpdatesClick;
  PanelContent.AppendChild(ButtonApplyUpdates);
  DS:=TRestDataset.Create(Nil);
  Conn:=TRestConnection.Create(nil);
  Conn.BaseURL:='/countries';
  Conn.OnGetURL:=@DoGetURL;
  DS.Connection:=Conn;
  DS.OnLoadFail:=@DoLoadFail;
  DS.AfterLoad:=@DoAfterLoad;
  DS.AfterOpen:=@DSOpen;
  DS.AfterApplyUpdates:=@DSAfterApplyUpdates;
  DS.AfterPost:=@ResetButtons;
  DS.AfterDelete:=@ResetButtons;
end;

begin
  TForm.Create;
end.

