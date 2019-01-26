program democollection;

uses browserconsole, sysutils, classes;

Type

  { TMyCollection }

  TMyCollectionItem = class(TCollectionItem)
  private
    FMyName: String;
  Published
    Property MyName : String Read FMyName Write FMyName;
  end;

  { TMyCollectionItems }

  TMyCollectionItems = Class(TCollection)
  private
    function GetMI(AIndex : Integer): TMyCollectionItem;
    procedure SetMI(AIndex : Integer; AValue: TMyCollectionItem);
  Public
    Function AddItem : TMyCollectionItem;
    Property MyItems[AIndex : Integer] : TMyCollectionItem Read GetMI Write SetMI;default;
  end;

{ TMyCollectionItems }

function TMyCollectionItems.GetMI(AIndex : Integer): TMyCollectionItem;
begin
  Result:=Items[AIndex] as TMyCollectionItem;
end;

procedure TMyCollectionItems.SetMI(AIndex : Integer; AValue: TMyCollectionItem);
begin
  Items[AIndex]:=AValue;
end;

function TMyCollectionItems.AddItem: TMyCollectionItem;
begin
  Result:=Add as TMyCollectionItem;
end;

Procedure DumpCollection(C : TMyCollectionItems);

Var
  S : String;
  I : integer;

begin
  S:='';
  For I:=0 to C.Count-1 do
    begin
    If S<>'' then
      S:=S+', ';
    S:=S+'['+IntToStr(I)+'] : '+C[i].MyName;
    end;
  Writeln('Fruit collection: ',s);
end;

Procedure DumpCollection2(C : TMyCollectionItems);

Var
  S : String;
  Itm : TCollectionItem;

begin
  S:='';
  For Itm in C do
    begin
    If S<>'' then
      S:=S+', ';
    S:=S+'['+IntToStr(Itm.Index)+'] : '+TMyCollectionItem(Itm).MyName;
    end;
  Writeln('Fruit collection: ',s);
end;

{
  Const
  MyNames : Array [1..10] of string
          = ('apple','pear','banana','orange','lemon','prune',
             'pineapple','strawberry','raspberry','cherry');
}

Function GetName(I : integer) : String;

begin
  Case I of
   1 : Result:='apple';
   2 : Result:='pear';
   3 : Result:='banana';
   4 : Result:='orange';
   5 : Result:='lemon';
   6 : Result:='prune';
   7 : Result:='pineapple';
   8 : Result:='strawberry';
   9 : Result:='raspberry';
   10 : Result:='cherry';
 end;
end;



Var
  MyC : TMyCollectionItems;
  C : TMyCollectionItem;
  I : Integer;

begin
  MyC:=TMyCollectionItems.Create(TMyCollectionItem);
  For I:=1 to 10 do
    begin
    C:=MyC.AddItem;
    C.MyName:=GetName(i);
    end;
  DumpCollection(MyC);
  Writeln('pruning prune');
  MyC.Delete(5);
  DumpCollection(MyC);
  Writeln('Prefer banana over pear');
  MyC.Exchange(1,2);
  DumpCollection2(MyC);
  Writeln('Indigestion, no more fruit');
  MyC.Clear;
  DumpCollection2(MyC);
end.

