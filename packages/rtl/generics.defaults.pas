unit Generics.Defaults;

{$MODE DELPHI}

interface

//uses Classes;

type

  { IComparer }

  IComparer<T> = interface ['{505778ED-F783-4456-9691-32F419CC5E18}']
    function Compare(const Left, Right: T): Integer; overload;
  end;

  TOnComparison<T> = reference to function(const Left, Right: T): Integer;

  { TComparer }

  TComparer<T> = class(TInterfacedObject, IComparer<T>)
  private
    type TMyComparer = TComparer<T>;
    class var DefaultComparer: TMyComparer;
  public
    class function Default: IComparer<T>; static;
    function Compare(const ALeft, ARight: T): Integer; virtual; abstract; overload;

    class function Construct(const AComparison: TOnComparison<T>): IComparer<T>; overload;
  end;

  { TDefaultComparer }

  TDefaultComparer<T> = class(TComparer<T>)
  public
    function Compare(const ALeft, ARight: T): Integer; override; overload;
  end;

  { TDelegatedComparerEvents }

  TDelegatedComparerEvents<T> = class(TComparer<T>)
  private
    FComparison: TOnComparison<T>;
  public
    function Compare(const ALeft, ARight: T): Integer; override;
    constructor Create(AComparison: TOnComparison<T>);
  end;

  IEnumerator<T> = interface
    function GetCurrent: T;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: T read GetCurrent;
  end;

  IEnumerable<T> = interface
    function GetEnumerator: IEnumerator<T>;
  end;

implementation

{ TComparer }

class function TComparer<T>.Default: IComparer<T>;
begin
  if DefaultComparer=nil then
    DefaultComparer:=TDefaultComparer<T>.Create;
  Result:=DefaultComparer;
end;

class function TComparer<T>.Construct(const AComparison: TOnComparison<T>
  ): IComparer<T>;
begin
   Result := TDelegatedComparerEvents<T>.Create(AComparison);
end;

{ TDefaultComparer }

function TDefaultComparer<T>.Compare(const ALeft, ARight: T): Integer;
begin
  asm
    if (ALeft < ARight) return -1;
    if (ALeft > ARight) return 1;
  end;
  Result:=0;
  if ALeft = ARight then exit;
end;

{ TDelegatedComparerEvents }

function TDelegatedComparerEvents<T>.Compare(const ALeft, ARight: T
  ): Integer;
begin
  Result := FComparison(ALeft, ARight);
end;

constructor TDelegatedComparerEvents<T>.Create(AComparison: TOnComparison<T>);
begin
  Inherited create;
  FComparison := AComparison;
end;


end.

