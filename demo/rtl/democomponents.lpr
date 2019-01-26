program democomponents;

uses browserconsole, Classes;

Type
   TMyGeneration = (first,second,third);

   { TMyParentComponent }

   TMyParentComponent = Class(TComponent)
   private
     FMyProperty: TMyGeneration;
   Published
     Property MyProperty : TMyGeneration Read FMyProperty Write FMyProperty;
   end;

   { TMyChildComponent }

   TMyChildComponent = Class(TMyParentComponent)
   Public
     destructor Destroy; override;
   end;

Var
  DestroyCount : Integer;

{ TMyChildComponent }

destructor TMyChildComponent.Destroy;
begin
  DestroyCount:=DestroyCount+1;
  Writeln('Destroying child "',Name,'", current count : ',DestroyCount);
  inherited Destroy;
end;

Var
  P : TMyParentComponent;
  C : TMyChildComponent;
  Ci : TComponent;

begin
  P:=TMyParentComponent.Create(Nil);
  try
    P.Name:='Parent1';
    P.MyProperty:=First;
    C:=TMyChildComponent.Create(P);
    C.Name:='Child1';
    C.MyProperty:=Second;
    C:=TMyChildComponent.Create(C);
    C.Name:='Child2';
    C.MyProperty:=Third;
    C:=TMyChildComponent.Create(P);
    C.Name:='Child3';
    C.MyProperty:=Second;
    Writeln('Components for loop');
    For CI in P do
      begin
      Write(CI.Name,', is child : ',C is TMyChildComponent,' ');
      If C is TMyChildComponent then
        Write('My property : ',TMyChildComponent(C).MyProperty);
      Writeln('');
      end;
  finally
    P.Destroy;
  end;
end.

