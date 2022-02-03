unit tcclasses;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, fpcunit, testregistry;

Type

  { TSortObj }

  TSortObj = Class (TObject)
    Value : String;
    Constructor create(aValue : string);
  end;
  { TTestClasses }

  TTestClasses = class(TTestCase)
  private
    FList: TFPList;
    FStrings: TStringList;
    procedure AssertSortedList;
    procedure AssertSortedStrings;
  public
    Procedure Setup; override;
    procedure TearDown; override;
    property Strings : TStringList Read FStrings;
    property List : TFPList Read FList;
  Published
    Procedure TestSort;
    Procedure TestSorted;
    Procedure TestSortedInverse;
    Procedure TestListSort;
    Procedure TestListSorted;
    Procedure TestListSortedInverse;
  end;

implementation

{ TSortObj }

constructor TSortObj.create(aValue: string);
begin
  Value:=aValue;
end;

{ TTestClasses }

procedure TTestClasses.Setup;
begin
  Inherited;
  FStrings:=TStringLisT.Create;
  FList:=TFPList.Create;
end;

procedure TTestClasses.TearDown;

Var
  I : Integer;
  O : TObject;

begin
  FreeAndNil(FStrings);
  For I:=0 to Flist.Count-1 do
    begin
    O:=TObject(Flist[i]);
    FreeAndNil(O);
    end;
  FreeAndNil(FList);
  Inherited;
end;

procedure TTestClasses.AssertSortedStrings;

Var
  I : Integer;

begin
  For I:=0 to Strings.Count-2 do
    if not (Strings[i]<=Strings[i+1]) then
      Fail(Strings.Text+Format('Not sorted at %d (%s) - %d (%s)',[I,Strings[i],I+1,Strings[i+1]]));
end;

procedure TTestClasses.TestSort;
begin
  Strings.Add('beta');
  Strings.Add('delta');
  Strings.Add('alfa');
  Strings.Add('gamma');
  Strings.Sort;
  AssertSortedStrings;
end;

procedure TTestClasses.TestSorted;
begin
  Strings.Add('alfa');
  Strings.Add('beta');
  Strings.Add('gamma');
  Strings.Add('delta');
  Strings.Sort;
  AssertSortedStrings;
end;

procedure TTestClasses.TestSortedInverse;
begin
  Strings.Add('delta');
  Strings.Add('gamma');
  Strings.Add('beta');
  Strings.Add('alfa');
  Strings.Sort;
  AssertSortedStrings;
end;

procedure TTestClasses.AssertSortedList;

Var
  I : Integer;

begin
  For I:=0 to FList.Count-2 do
    if not (TSortObj(Flist[i]).Value<=TSortObj(Flist[i+1]).Value) then
      Fail(Strings.Text+Format('Not sorted at %d (%s) - %d (%s)',[I,TSortObj(Flist[i]).Value,I+1,TSortObj(Flist[i+1]).Value]));

end;

Function ObjSort (A,B : JSValue) : Integer;

begin
  Result:=CompareText(TSortObj(A).Value,TSortObj(B).Value);
end;

procedure TTestClasses.TestListSort;
begin
  FList.Add(TSortObj.Create('beta'));
  FList.Add(TSortObj.Create('delta'));
  FList.Add(TSortObj.Create('alfa'));
  FList.Add(TSortObj.Create('gamma'));
  FList.Sort(@ObjSort);
  AssertSortedList;
end;

procedure TTestClasses.TestListSorted;
begin
  FList.Add(TSortObj.Create('alfa'));
  FList.Add(TSortObj.Create('beta'));
  FList.Add(TSortObj.Create('gamma'));
  FList.Add(TSortObj.Create('delta'));
  FList.Sort(@ObjSort);
  AssertSortedList;
end;

procedure TTestClasses.TestListSortedInverse;
begin
  FList.Add(TSortObj.Create('delta'));
  FList.Add(TSortObj.Create('gamma'));
  FList.Add(TSortObj.Create('beta'));
  FList.Add(TSortObj.Create('alfa'));
  FList.Sort(@ObjSort);
  AssertSortedList;
end;

initialization
  RegisterTests([TTestClasses]);

end.

