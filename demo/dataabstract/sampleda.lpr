program sampleda;

{$mode objfpc}

uses
  JS, Classes, SysUtils, Web, DB, dasdk, dadataset;

Type

  { TSampleForm }

  TSampleForm = Class(TComponent)
  Private
    divWrapper : TJSHTMLElement;
    btnGetData : TJSHTMLButtonElement;
    tblBody : TJSHTMLElement;
    FConn : TDAConnection;
    FDataset : TDADataset;
    procedure AfterLoad(DataSet: TDataSet; Data: JSValue);
    procedure BindElements;
    procedure CreateDataset;
    procedure DoClientsOpen(DataSet: TDataSet);
    function DoGetDataClick(aEvent: TJSMouseEvent): boolean;
    procedure DoLoginOK(result: Boolean; UserInfo: TDAUserInfo);
  Public
    Procedure Show;
  end;

{ TSampleForm }

procedure TSampleForm.CreateDataset;

begin
  FConn:=TDaConnection.Create(Self);
  FConn.URL:='https://sample.remobjects.com/bin';
  FConn.OnLogin:=@DoLoginOK;
  FConn.StreamerType:=stBin;
  FDataset:=TDaDataset.Create(Self);
  FDataset.DAConnection:=FConn;
  FDataset.TableName:='Clients';
  FDataset.AfterOpen:=@DoClientsOpen;
end;

procedure TSampleForm.BindElements;

begin
  btnGetData:=TJSHTMLButtonElement(Document.getElementById('btn-fetch'));
  btnGetData.onClick:=@DoGetDataClick;
  tblBody:=TJSHTMLElement(Document.getElementById('tableRows'));
  divWrapper:=TJSHTMLElement(Document.getElementById('wrapper'));
end;

procedure TSampleForm.AfterLoad(DataSet: TDataSet; Data: JSValue);
begin
  Writeln('Loaded');
end;

procedure TSampleForm.DoClientsOpen(DataSet: TDataSet);

  Function escape(S : String) : String;

  begin
    Result:=StringReplace(S,'&','&amp;',[rfReplaceAll]);
    Result:=StringReplace(S,'<','&lt;',[rfReplaceAll]);
    Result:=StringReplace(S,'>','&gt;',[rfReplaceAll]);
    Result:=StringReplace(S,'"','&quot;',[rfReplaceAll]);
    Result:=StringReplace(S,'''','&#39;',[rfReplaceAll]);
  end;

Var
  FID,FName,FPhone : TField;
  HTML : String;

begin
  Writeln('Clients open :',Dataset.RecordCount);
  FID:=Dataset.FieldByname('ClientId');
  FName:=Dataset.FieldByname('ClientName');
  FPhone:=Dataset.FieldByname('ContactPhone');
  While not Dataset.EOF do
    begin
    html:=Html+'<TR><TD>'+Escape(FID.AsString)+'</TD>'
              +'<TD>'+Escape(FName.AsString)+'</TD>'
              +'<TD>'+Escape(FPhone.AsString)+'</TD></TR>';
    Dataset.Next;
    end;
  tblBody.InnerHTMl:=HTML;
  divWrapper['style']:='';
end;

function TSampleForm.DoGetDataClick(aEvent: TJSMouseEvent): boolean;
begin
  FConn.LoginEx('User=simple;Password=simple;');
  Result:=False;
end;

procedure TSampleForm.DoLoginOK(result: Boolean; UserInfo: TDAUserInfo);
begin
  Writeln('Login :',result);
  if Result then
    begin
    divWrapper['style']:='display: none;';
    FDataset.Active:=False;
    FDataset.Load([],@AfterLoad);
    end
  else
    window.Alert('Failed to log in !')
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
