program sampledarw;

{$mode objfpc}

uses
  Classes, SysUtils, Web, DB, dadataset;

Type

  { TSampleForm }

  TSampleForm = Class(TComponent)
  Private
    divWrapper : TJSHTMLElement;
    btnDeleteData,
    btnAddData,
    btnEditData,
    btnGetData : TJSHTMLButtonElement;
    tblBody : TJSHTMLElement;
    FConn : TDAConnection;
    FDataset : TDADataset;
    procedure AfterLoad(DataSet: TDataSet);
    procedure BindElements;
    procedure CreateDataset;
    function DoAddDataClick(aEvent: TJSMouseEvent): boolean;
    procedure DoAfterApplyUpdates(Sender: TDataset; info: TResolveResults);
    procedure DoCountriesOpen(DataSet: TDataSet);
    function DoDeleteDataClick(aEvent: TJSMouseEvent): boolean;
    function DoEditDataClick(aEvent: TJSMouseEvent): boolean;
    function DoGetDataClick(aEvent: TJSMouseEvent): boolean;
    procedure ShowCountries;
  Public
    Procedure Show;
  end;

{ TSampleForm }

procedure TSampleForm.CreateDataset;

begin
  FConn:=TDaConnection.Create(Self);
  FConn.URL:='/proxy/Server/bin';
  FConn.StreamerType:=stBin;
  FDataset:=TDaDataset.Create(Self);
  FDataset.DAConnection:=FConn;
  FDataset.TableName:='Country';
  FDataset.AfterOpen:=@DoCountriesOpen;
  FDataset.AfterApplyUpdates:=@DoAfterApplyUpdates;
  FDataset.AfterLoad:=@AfterLoad;
  FDataset.DAOptions:=[doRefreshAllFields];
end;


procedure TSampleForm.DoAfterApplyUpdates(Sender: TDataset; info: TResolveResults);

Var
  I : Integer;
  ACount,aSucceeded : Integer;

begin
  aCount:=Length(Info.Records);
  aSucceeded:=0;
  For I:=0 to aCount-1 do
    if Info.Records[i].ResolveStatus=rsResolved then
      Inc(aSucceeded);
  window.alert(Format('Sent %d records for update to the server, %d succeeded.',[aCount,aSucceeded]));
  if aSucceeded>0 then
    begin
    FDataset.First;
    ShowCountries;
    end;
end;

procedure TSampleForm.BindElements;

begin
  btnGetData:=TJSHTMLButtonElement(Document.getElementById('btn-fetch'));
  btnGetData.onClick:=@DoGetDataClick;
  btnAddData:=TJSHTMLButtonElement(Document.getElementById('btn-add'));
  btnAddData.onClick:=@DoAddDataClick;
  btnAddData.Disabled:=True;
  btnEditData:=TJSHTMLButtonElement(Document.getElementById('btn-edit'));
  btnEditData.onClick:=@DoEditDataClick;
  btnEditData.Disabled:=True;
  btnDeleteData:=TJSHTMLButtonElement(Document.getElementById('btn-delete'));
  btnDeleteData.onClick:=@DoDeleteDataClick;
  btnDeleteData.Disabled:=True;
  tblBody:=TJSHTMLElement(Document.getElementById('tableRows'));
  divWrapper:=TJSHTMLElement(Document.getElementById('wrapper'));
end;

procedure TSampleForm.AfterLoad(DataSet: TDataSet);
begin
  Writeln('Loaded');
end;

procedure TSampleForm.DoCountriesOpen(DataSet: TDataSet);


begin
  Writeln('Countries open :',Dataset.RecordCount);
  btnEditData.Disabled:=False;
  btnAddData.Disabled:=False;
  btnDeleteData.Disabled:=False;
  ShowCountries;
end;

function TSampleForm.DoDeleteDataClick(aEvent: TJSMouseEvent): boolean;
begin
  Result:=False;
  FDataset.last;
  FDataset.Delete;
  FDataset.ApplyUpdates;
end;

procedure TSampleForm.ShowCountries;

  Function escape(S : String) : String;

  begin
    Result:=StringReplace(S,'&','&amp;',[rfReplaceAll]);
    Result:=StringReplace(S,'<','&lt;',[rfReplaceAll]);
    Result:=StringReplace(S,'>','&gt;',[rfReplaceAll]);
    Result:=StringReplace(S,'"','&quot;',[rfReplaceAll]);
    Result:=StringReplace(S,'''','&#39;',[rfReplaceAll]);
  end;

Var
  FISo,FName,FNiceName,fISO3,Fnum,FPhone : TField;
  HTML : String;

begin
  FISO:=FDataset.FieldByname('ISO');
  FISO3:=FDataset.FieldByname('ISO3');
  FName:=FDataset.FieldByname('Name');
  FNiceName:=FDataset.FieldByname('NiceName');
  FNum:=FDataset.FieldByname('Numcode');
  FPhone:=FDataset.FieldByname('phonecode');
  While not FDataset.EOF do
    begin
    html:=Html+'<TR><TD>'+Escape(FISO.AsString)+'</TD>'
              +'<TD>'+Escape(FIso3.AsString)+'</TD>'
              +'<TD>'+Escape(FName.AsString)+'</TD>'
              +'<TD>'+Escape(FNiceName.AsString)+'</TD>'
              +'<TD>'+Escape(FNum.AsString)+'</TD>'
              +'<TD>'+Escape(FPHone.AsString)+'</TD></TR>';
    FDataset.Next;
    end;
  tblBody.InnerHTMl:=HTML;
  divWrapper['style']:='';
end;

function TSampleForm.DoAddDataClick(aEvent: TJSMouseEvent): boolean;
begin
  Result:=False;
   With FDataset do
     begin
     Append;
     FieldByname('ISO').AsString:='zz';
     FieldByname('ISO3').AsString:='zzz';
     FieldByname('Name').AsString:='CountryZ';
     FieldByname('NiceName').AsString:='Country with name Z';
     FieldByname('PhoneCode').AsInteger:=999;
     FieldByname('NumCode').AsInteger:=99;
     Post;
     ApplyUpdates;
     end;
end;


function TSampleForm.DoEditDataClick(aEvent: TJSMouseEvent): boolean;

begin
  Result:=False;
  With FDataset do
    begin
    Last;
    Edit;
    FieldByname('ISO3').AsString:='zz3';
    FieldByname('Name').AsString:='CountryZZ';
    FieldByname('NiceName').AsString:='Country without name Z';
    FieldByname('PhoneCode').AsInteger:=666;
    FieldByname('NumCode').AsInteger:=66;
    Post;
    ApplyUpdates;
    end;
end;

function TSampleForm.DoGetDataClick(aEvent: TJSMouseEvent): boolean;
begin
  Result:=False;
  FDataset.Close;
  FDataset.load([],Nil);
end;


procedure TSampleForm.Show;

begin
  CreateDataset;
  BindElements;
end;

begin
  With TSampleForm.Create(Nil) do
    Show;
end.
