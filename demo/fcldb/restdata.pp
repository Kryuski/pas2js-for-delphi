unit restdata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpdefs;

Procedure HandleRest(aRequest : TRequest; AResponse : TResponse);



implementation

uses jsonparser,fpjson;

Var
  Packet : TJSONObject;
  LastID : integer;

Function Data : TJSONArray;

begin
  Result:=Packet.Arrays['Data'];
end;

Function MetaData : TJSONObject;
begin
  Result:=Packet.Objects['metaData'];
end;

Procedure LoadRestData;

Var
  F : TFileStream;
  D : TJSONData;
  I : Integer;

begin
  F:=TFileStream.Create('countries.json',fmOpenRead or fmShareDenyWrite);
  try
    D:=GetJSON(F);
    if not (D is TJSONObject) then
      D.Free
    else
      Packet:=D as TJSONObject;
    MetaData.Arrays['fields'].Add(TJSONObject.Create(['name','id','type','int']));
    For I:=0 to Data.Count-1 do
      Data.Objects[i].Add('id',I+1);
    LastID:=Data.Count;
  finally
    F.Free;
  end;
end;

Procedure DoneRestData;

begin
  Packet.Free;
end;

Procedure SendRecords(aFirst,aLast : integer; AResponse : TResponse);

Var
  J: TJSONObject;
  A : TJSONArray;
  I : Integer;

begin
  A:=TJSONArray.Create;
  J:=TJSONObject.Create(['metaData',MetaData.Clone,'Data',A]);
  try
    I:=AFirst;
    While (I<=ALast) and (I<Data.Count) do
      begin
      A.Add(Data[i].Clone);
      Inc(I);
      end;
    AResponse.Content:=J.FormatJSON();
    AResponse.ContentLength:=Length(AResponse.Content);
    AResponse.ContentType:='application/json';
    AResponse.SendContent;
  finally
    J.Free;
  end;
end;

Function IndexOfID(ID : Integer) : Integer;

begin
  Result:=Data.Count-1;
  While (Result>=0) and (Data.Objects[Result].Integers['id']<>ID) do
   Dec(Result);

end;

Procedure NotFound(AResponse : TResponse);

begin
  AResponse.Code:=404;
  AResponse.CodeText:='Not found';
  AResponse.SendResponse;
end;

Procedure InvalidParam(AResponse : TResponse; AContent : String = '');

begin
  AResponse.Code:=400;
  AResponse.CodeText:='Bad request';
  AResponse.COntent:=AContent;
  AResponse.ContentLength:=Length(AContent);
  AResponse.SendResponse;
end;

Function GetRequestID(aRequest : TRequest) : Integer;

Var
  P : String;
  I : integer;

begin
  P:=ARequest.PathInfo;
  if (P<>'') and (P[1]='/') then
    Delete(P,1,1);
  I:=Pos('/',P);
  if I=0 then
    I:=Length(P);
  Delete(P,1,I);
  if (P<>'') then
    Result:=StrToIntDef(P,-1)
  else
    Result:=-2;
end;

Procedure GetRecords(aRequest : TRequest; AResponse : TResponse);

Var
  P : String;
  I,ID,First,Last : Integer;

begin
  First:=0;
  Last:=Data.Count-1;
  ID:=GetRequestID(ARequest);
  if ID=-1 then
    begin
    Writeln('Bad GET request: ',aRequest.PATHINFO);
    InvalidParam(AResponse);
    exit;
    end;
  if (ID<>-2) then
    begin
    First:=IndexOfID(ID);
    if First=-1 then
      begin
      NotFound(AResponse);
      exit;
      end;
    Last:=First;
    SendRecords(First,Last,AResponse);
    end
  else
    begin
    First:=StrToIntDef(ARequest.QueryFields.Values['Offset'],First);
    if (ARequest.QueryFields.Values['Limit']<>'') then
      Last:=First+StrToIntDef(ARequest.QueryFields.Values['Limit'],20)-1; // Default page size of 20
    Writeln('Count ',Data.Count,', Offset: ',First,', Last: ',Last);
    SendRecords(First,last,AResponse);
    end;
end;


Function  GetObject(aRequest : TRequest; Out AReason : String) : TJSONObject;

Var
  D : TJSONData;
  C,I : Integer;

begin
  Result:=Nil;
  if Not SameText(aRequest.ContentType,'application/json') then
    Exit;
  try
    D:=GetJSON(aRequest.Content,true);
    if (D is TJSONObject) then
      begin
      Result:=D as TJSONObject;
      C:=Result.Count-1;
      D:=Nil;
      end
    else
      begin
      FreeAndNil(D);
      end;
    I:=C;
    While Assigned(Result) and (I>=0) do
      begin
      if (Pos(Result.Names[I]+';','id;Name;Population;')=0) then
        begin
        AReason:='Invalid property: '+Result.Names[I];
        FreeAndNil(Result);
        end;
      Dec(I);
      end;
  except
    On E : Exception do
      AReason:=E.ClassName+': '+E.Message;
  end;
end;

Procedure CreateRecord(aRequest : TRequest; AResponse : TResponse);

Var
  O,D,F : TJSONObject;
  I : integer;
  R : String;


begin
  I:=GetRequestID(ARequest);
  if I<>-2 then
    begin
    Writeln('Bad POST request: ',aRequest.PathInfo);
    R:='No ID allowed for POST';
    InvalidParam(AResponse,R);
    exit;
    end;
  O:=GetObject(ARequest,R);
  if (O=Nil) then
    begin
    Writeln('Bad POST request: ',aRequest.Content,' : ',R);
    InvalidParam(AResponse,R);
    exit;
    end;
  try
    D:=TJSONObject.Create;
    I:=Data.Add(D);
    D.Add('id',LastID);
    D.add('Name',O.Strings['Name']);
    D.add('Population',O.Int64s['Population']);
    Inc(LastID);
    SendRecords(I,I,AResponse);
  finally
    O.Free;
  end;
end;

Procedure UpdateRecord(aRequest : TRequest; AResponse : TResponse);

Var
  ID,Idx : Integer;
  O,D : TJSONObject;
  R : String;

begin
  ID:=GetRequestID(ARequest);
  if ID<0 then
    begin
    R:='Need ID for PUT';
    InvalidParam(AResponse,R);
    exit;
    end;
  IDx:=IndexOfID(ID);
  if IDX=-1 then
    begin
    NotFound(AResponse);
    exit;
    end;
  O:=GetObject(ARequest,R);
  if (O=Nil) then
    begin
    Writeln('Bad PUT request: ',aRequest.Content,' : ',R);
    InvalidParam(AResponse,R);
    exit;
    end;
  D:=Data.Objects[Idx];
  if O.IndexOfName('Name')<>-1 then
    D.Strings['Name']:=O.Strings['Name'];
  if O.IndexOfName('Population')<>-1 then
    D.Int64s['Population']:=O.Int64s['Population'];
  SendRecords(Idx,Idx,AResponse);
end;

Procedure DeleteRecord(aRequest : TRequest; AResponse : TResponse);

Var
  ID,Idx : Integer;
  R : String;


begin
  ID:=GetRequestID(ARequest);
  if ID<0 then
    begin
    R:='Need ID for DELETE';
    InvalidParam(AResponse,R);
    exit;
    end;
  ID:=GetRequestID(ARequest);
  IDx:=IndexOfID(ID);
  if IDX=-1 then
    begin
    NotFound(AResponse);
    exit;
    end;
  Data.Delete(Idx);
  AResponse.Code:=204;
  AResponse.CodeText:='No content';
  AResponse.SendResponse;
end;


Procedure HandleRest(aRequest : TRequest; AResponse : TResponse);

begin
  Writeln('Method : ',ARequest.Method);
  Case UpperCase(ARequest.Method) of
    'GET': GetRecords(ARequest,AResponse);
    'POST': CreateRecord(ARequest,AResponse);
    'PUT': UpdateRecord(ARequest,AResponse);
    'DELETE': DeleteRecord(ARequest,AResponse);
  else
    Raise EHTTP.CreateHelp('Method not allowed',405);
  end;
end;

initialization
  LoadRestData;

finalization
  DoneRestData;
end.

