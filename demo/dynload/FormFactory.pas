unit formfactory;

interface

uses classes;

Type
  TFormClass = Class of TComponent;

procedure RegisterForm(aClass: TFormClass);
Function GetFormClassByName(const aName : String) : TFormClass;

implementation

uses sysutils, js;

Type
  TFormDef = Record
    aClass : TFormClass;
    aName : string;
  end;
  TFormDefArray = Array of TFormDef;
  
Var
  List : TFormDefArray;

procedure RegisterForm(aClass: TFormClass);

Var
  Def : TFormDef;
    
begin
  Def.aClass:=aClass;
  Def.aName:=aClass.ClassName;
  TJSArray(List).Push(JSValue(Def));
end;

Function IndexOfFormClassByName(const aName : String) : Integer;

begin
  Result:=Length(List)-1;
  While (Result>=0) and Not SameText(aName,List[Result].aName) do 
    Dec(Result);
end;

Function GetFormClassByName(const aName : String) : TFormClass;

Var
  Idx: Integer;
  
begin
  Idx:=IndexOfFormClassByName(aName);
  if Idx=-1 then 
    Result:=Nil
  else
    Result:=List[Idx].aClass;
end;

initialization
  SetLength(List,0);
end.
