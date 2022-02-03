{ Components used in component streaming tests.

  Copyright (C) 2020- Michael Van Canneyt michael@freepascal.org

  This library is free software; you can redistribute it and/or modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either version 2 of the License, or (at your option) any later version
  with the following modification:

  As a special exception, the copyright holders of this library give you permission to link this library with independent modules
  to produce an executable, regardless of the license terms of these independent modules,and to copy and distribute the resulting
  executable under terms of your choice, provided that you also meet, for each linked independent module, the terms and conditions
  of the license of that module. An independent module is a module which is not derived from or based on this library. If you
  modify this library, you may extend this exception to your version of the library, but you are not obligated to do so. If you do
  not wish to do so, delete this exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along with this library; if not, write to the Free
  Software Foundation, Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.
}
unit testcomps;

interface

uses classes, sysutils;

Type
  TEmptyComponent = Class(TComponent)
  end;

  // Simple integer, fits in 1 byte
  TIntegerComponent = Class(TComponent)
  private
    FIntProp: Integer;
  Public
     Constructor Create(AOwner : TComponent); override;
  Published
    Property IntProp : Integer Read FIntProp Write FIntProp;
  end;

  // Simple integer, fits in 2 bytes
  TIntegerComponent2 = Class(TComponent)
  private
    FIntProp: Integer;
  Public
     Constructor Create(AOwner : TComponent); override;
  Published
    Property IntProp : Integer Read FIntProp Write FIntProp;
  end;

  // Simple integer, fits in 3 bytes
  TIntegerComponent3 = Class(TComponent)
  private
    FIntProp: Integer;
  Public
     Constructor Create(AOwner : TComponent); override;
  Published
    Property IntProp : Integer Read FIntProp Write FIntProp;
  end;

  // Simple integer, Default value. (set)
  TIntegerComponent4 = Class(TComponent)
  private
    FIntProp: Integer;
  Public
     Constructor Create(AOwner : TComponent); override;
  Published
    Property IntProp : Integer Read FIntProp Write FIntProp default 6;
  end;

  // Simple integer, Default value. (not set)
  TIntegerComponent5 = Class(TComponent)
  private
    FIntProp: Integer;
  Public
     Constructor Create(AOwner : TComponent); override;
  Published
    Property IntProp : Integer Read FIntProp Write FIntProp default 6;
  end;

  // Simple Int64 property fits in a single byte.
  TInt64Component = Class(TComponent)
  private
    FIntProp: NativeInt;
  Public
     Constructor Create(AOwner : TComponent); override;
  Published
    Property Int64Prop : NativeInt Read FIntProp Write FIntProp;
  end;

  // Simple Int64 property fits 2 bytes.
  TInt64Component2 = Class(TComponent)
  private
    FIntProp: NativeInt;
  Public
     Constructor Create(AOwner : TComponent); override;
  Published
    Property Int64Prop : NativeInt Read FIntProp Write FIntProp;
  end;

  // Simple Int64 property fits 3 bytes.
  TInt64Component3 = Class(TComponent)
  private
    FIntProp: NativeInt;
  Public
     Constructor Create(AOwner : TComponent); override;
  Published
    Property Int64Prop : NativeInt Read FIntProp Write FIntProp;
  end;

  // Simple Int64 property fits 4 bytes.
  TInt64Component4 = Class(TComponent)
  private
    FIntProp: NativeInt;
  Public
     Constructor Create(AOwner : TComponent); override;
  Published
    Property Int64Prop : NativeInt Read FIntProp Write FIntProp;
  end;

  // Int64 property with default, set.
  TInt64Component5 = Class(TComponent)
  private
    FIntProp: NativeInt;
  Public
     Constructor Create(AOwner : TComponent); override;
  Published
    Property Int64Prop : NativeInt Read FIntProp Write FIntProp default 7;
  end;

  // Int64 property with default, not set.
  TInt64Component6 = Class(TComponent)
  private
    FIntProp: NativeInt;
  Public
     Constructor Create(AOwner : TComponent); override;
  Published
    Property Int64Prop : NativeInt Read FIntProp Write FIntProp default 7;
  end;

  { TCharComponent2 }

  TCharComponent = Class(TComponent)
  private
    C: Char;
  Public
    Constructor Create(AOwner : TComponent);  override;
  Published
    Property CharProp : Char Read C Write C;
  end;


  // String property.
  TStringComponent = Class(TComponent)
  private
    F: String;
  Public
    Constructor Create(AOwner : TComponent);  override;
  Published
    Property StringProp : String Read F Write F;
  end;

  // String property, empty
  TStringComponent2 = Class(TComponent)
  private
    F: String;
  Published
    Property StringProp : String Read F Write F;
  end;

  // String property, with quote.

  { TStringComponent3 }

  TStringComponent3 = Class(TComponent)
  private
    F: String;
  Public
    Constructor Create(AOwner : TComponent);  override;
  Published
    Property StringProp : String Read F Write F;
  end;


  // WideString property
  TWideStringComponent = Class(TComponent)
  private
    F: WideString;
  Public
    Constructor Create(AOwner : TComponent);  override;
  Published
    Property WideStringProp : WideString Read F Write F;
  end;

  // WideString property, empty
  TWideStringComponent2 = Class(TComponent)
  private
    F: WideString;
  Published
    Property WideStringProp : WideString Read F Write F;
  end;

  // Single property
  TSingleComponent = Class(TComponent)
  private
    F: Single;
  Public
    Constructor Create(AOwner : TComponent);  override;
  Published
    Property SingleProp : Single Read F Write F;
  end;

  // Double property
  TDoubleComponent = Class(TComponent)
  private
    F: Double;
  Public
    Constructor Create(AOwner : TComponent);  override;
  Published
    Property DoubleProp : Double Read F Write F;
  end;

  // Extended property
  TExtendedComponent = Class(TComponent)
  private
    F: Extended;
  Public
    Constructor Create(AOwner : TComponent);  override;
  Published
    Property ExtendedProp : Extended Read F Write F;
  end;

  (*
  // Comp property
  TCompComponent = Class(TComponent)
  private
    F: Comp;
  Public
    Constructor Create(AOwner : TComponent);  override;
  Published
    Property ExtendedProp : Comp Read F Write F;
  end;
  *)

  // Currency property
  TCurrencyComponent = Class(TComponent)
  private
    F: Currency;
  Public
    Constructor Create(AOwner : TComponent);  override;
  Published
    Property CurrencyProp : Currency Read F Write F;
  end;

  // DateTime property, date only
  TDateTimeComponent = Class(TComponent)
  private
    F: TDateTime;
  Public
    Constructor Create(AOwner : TComponent);  override;
  Published
    Property DateTimeProp : TDateTime Read F Write F;
  end;

  // DateTime property, time only
  TDateTimeComponent2 = Class(TComponent)
  private
    F: TDateTime;
  Public
    Constructor Create(AOwner : TComponent);  override;
  Published
    Property DateTimeProp : TDateTime Read F Write F;
  end;

  // DateTime property, Date and time
  TDateTimeComponent3 = Class(TComponent)
  private
    F: TDateTime;
  Public
    Constructor Create(AOwner : TComponent);  override;
  Published
    Property DateTimeProp : TDateTime Read F Write F;
  end;

  TDice = (one,two,three,four,five,six);

  // Enum property. No default (i.e. 0)
  TEnumComponent = Class(TComponent)
  private
    F: TDice;
  Public
    Constructor Create(AOwner : TComponent);  override;
  Published
    Property Dice : TDice Read F Write F;
  end;

  // Enum  property, not set
  TEnumComponent2 = Class(TComponent)
  private
    F: TDice;
  Published
    Property Dice : TDice Read F Write F;
  end;

  // Enum property with default, not set
  TEnumComponent3 = Class(TComponent)
  private
    F: TDice;
  Public
    Constructor Create(AOwner : TComponent);  override;
  Published
    Property Dice : TDice Read F Write F default two;
  end;

  // Enum property with default, set
  TEnumComponent4 = Class(TComponent)
  private
    F: TDice;
  Public
    Constructor Create(AOwner : TComponent);  override;
  Published
    Property Dice : TDice Read F Write F default two;
  end;

  // Enum property with default, no need to set
  TEnumComponent5 = Class(TComponent)
  private
    F: TDice;
  Published
    Property Dice : TDice Read F Write F default one;
  end;

  Throws = Set of TDice;

  // Set property, no default.
  TSetComponent = Class(TComponent)
  private
    F: Throws;
  Public
    Constructor Create(AOwner : TComponent);  override;
  Published
    Property Throw : Throws Read F Write F;
  end;

  // Set property, no default, not set
  TSetComponent2 = Class(TComponent)
  private
    F: Throws;
  Published
    Property Throw : Throws Read F Write F;
  end;

  // Set property, default, not set
  TSetComponent3 = Class(TComponent)
  private
    F: Throws;
  Public
    Constructor Create(AOwner : TComponent);  override;
  Published
    Property Throw : Throws Read F Write F default [three,six];
  end;

  // Set property, default, set
  TSetComponent4 = Class(TComponent)
  private
    F: Throws;
  Public
    Constructor Create(AOwner : TComponent);  override;
  Published
    Property Throw : Throws Read F Write F default [three,six];
  end;

  // Multiple components.
  TMultipleComponent = Class(TComponent)
  private
    FCurrency: Currency;
    FInt: Integer;
    FString: String;
    FDice: TDice;
    F: Throws;
  Public
    Constructor Create(AOwner : TComponent);  override;
  Published
    Property IntProp : Integer Read FInt Write FInt;
    Property StringProp : String Read FString Write FString;
    Property CurrencyProp : Currency Read FCurrency Write FCurrency;
    Property Dice : TDice Read FDice Write FDice;
    Property Throw : Throws Read F Write F;
  end;

  TTestPersistent1 = Class(TPersistent)
  private
    FInt: Integer;
    FAstring: String;
  Public
    Procedure Assign(ASource : TPersistent); override;
  Published
    Property AInteger : Integer Read FInt Write FInt;
    Property AString : String Read FAstring Write FAsTring;
  end;

  // Persistent as a published property.
  TPersistentComponent = Class(TComponent)
  private
    FPers: TTestPersistent1;
    procedure SetPers(const Value: TTestPersistent1);
  Public
    Constructor Create(AOwner : TComponent);  override;
    Destructor Destroy; override;
  Published
    Property Persist : TTestPersistent1 Read FPers Write SetPers;
  end;

  // For use in collection streaming
  TTestItem = Class(TCollectionItem)
  Private
    F : String;
  Published
    Property StrProp : String Read F Write F;
  end;

  // For use in collection streaming: items with two properties

  { TTest2Item }

  TTest2Item = Class(TCollectionItem)
  Private
    F1, F2 : String;
  public
  Published
    Property StrProp1 : String Read F1 Write F1;
    Property StrProp2 : String Read F2 Write F2;
  end;


  TTestCollection = Class(TCollection)
  Public
    Constructor Create;
  end;

  // Empty collection
  TCollectionComponent = Class(TComponent)
  Private
    FColl : TCollection;
    Procedure SetColl(AColl : TCollection);
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
  Published
    Property Coll : TCollection Read FColl Write SetCOll;
  end;

  // collection with elements.
  TCollectionComponent2 = Class(TCollectionComponent)
  Public
    Constructor Create(AOwner : TComponent); override;
  end;

  // collection with elements, one has no props
  TCollectionComponent3 = Class(TCollectionComponent)
  Public
    Constructor Create(AOwner : TComponent); override;
  end;

  // collection with changed propname, one element
  TCollectionComponent4 = Class(TComponent)
    FColl : TTestCollection;
    Procedure SetColl(AColl : TTestCollection);
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
  Published
    Property Coll : TTestCollection Read FColl Write SetColl;
  end;

  // collection two elements, items with two properties
  TCollectionComponent5 = Class(TComponent)
    FColl : TCollection;
    Procedure SetColl(AColl : TCollection);
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
  Published
    Property Coll : TCollection Read FColl Write SetColl;
  end;

  // Component as published property
  TOwnedComponent = Class(TComponent)
    F : TComponent;
  Public
    Constructor Create(AOwner : TComponent);  override;
  Published
    Property CompProp : TComponent Read F Write F;
  end;

  // Use this if owned components should also be streamed.
  TChildrenComponent = Class(TComponent)
    // Owned components are children
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
  end;

  // Stream sub component.
  TStreamedOwnedComponent = Class(TChildrenComponent)
  Public
    Constructor Create(AOwner : TComponent);  override;
  Published
    Sub : TIntegerComponent;
  end;
  
  // Stream 2 sub components
  TStreamedOwnedComponents = Class(TChildrenComponent)
  Public
    Constructor Create(AOwner : TComponent);  override;
  Published
    SubA : TIntegerComponent;
    SubB : TStringComponent;
  end;

  // Method tests.

  THandler = Procedure of Object;

  // Method property that points to own method.
  TMethodComponent = Class(TComponent)
  Private
    F : THandler;
  Public
    Constructor Create(AOwner : TComponent);  override;
  Published
    Procedure MyMethod;
    Property MethodProp : THandler Read F Write F;
  end;

  // Method property of owned component that points to own method.
  TMethodComponent2 = Class(TChildrenComponent)
  Public
    Constructor Create(AOwner : TComponent);  override;
  Published
    Procedure MyMethod2;
  end;

  { TMyItem }

  TMyItem = Class(TCollectionItem)
  private
    FNR: Integer;
    FStr: String;
  Public
    Procedure Assign(Source : TPersistent); override;
  Published
    Property Nr : Integer Read FNR Write FNR;
    Property Str: String Read FStr Write FStr;
  end;

  { TMyColl }

  TMyColl = Class(TCollection)
  private
    function GetIt(index : Integer): TMyItem;
    procedure SetIt(index : Integer; const AValue: TMyItem);
  Public
    Property It[index : Integer] : TMyItem Read GetIt Write SetIt; default;
  end;

  { TCollComp }

  TCollComp = Class(TComponent)
  private
    FMyColl: TMyColl;
    procedure SetMyColl(const AValue: TMyColl);
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Function ToStream : TStream;
    Procedure FromStream(AStream : TStream);
  Published
    Property MyColl : TMyColl Read FMyColl Write SetMyColl;
  end;


Implementation

{ TCharComponent2 }

constructor TCharComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  C:=#10;
end;

{ TStringComponent3 }

constructor TStringComponent3.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  F:='A ''quoted'' string';
end;

procedure TChildrenComponent.GetChildren(Proc: TGetChildProc; Root: TComponent);

Var
  I : Integer;

begin
  if (Root=Nil) and (Root<>Nil) then exit;// Silence warning
  For I:=0 to ComponentCount-1 do
    Proc(Components[i]);
end;


{ TIntegerComponent }

constructor TIntegerComponent.Create(AOwner: TComponent);
begin
  inherited;
  FIntProp:=3;
end;


{ TInt64Component }

constructor TInt64Component.Create(AOwner: TComponent);
begin
  inherited;
  FIntProp:=4;
end;

{ TInt64Component2 }

constructor TInt64Component2.Create(AOwner: TComponent);
begin
  inherited;
  FIntProp:=2 shl 9;
end;

{ TIntegerComponent2 }

constructor TIntegerComponent2.Create(AOwner: TComponent);
begin
  inherited;
  FintProp:=2 shl 9;
end;

{ TIntegerComponent3 }

constructor TIntegerComponent3.Create(AOwner: TComponent);
begin
  inherited;
  FintProp:=2 shl 17;
end;

{ TInt64Component3 }

constructor TInt64Component3.Create(AOwner: TComponent);
begin
  inherited;
  FintProp:=2 shl 17;
end;

{ TInt64Component4 }

constructor TInt64Component4.Create(AOwner: TComponent);
begin
  inherited;
  FintProp:=NativeInt(MaxInt)+NativeInt(2 shl 14);
end;

{ TStringComponent }

constructor TStringComponent.Create(AOwner: TComponent);
begin
  Inherited;
  F:='A string';
end;

{ TWideStringComponent }

constructor TWideStringComponent.Create(AOwner: TComponent);
begin
  inherited;
  F:='Some WideString';
end;

{ TSingleComponent }

constructor TSingleComponent.Create(AOwner: TComponent);
begin
  inherited;
  F:=1.23;
end;

{ TDoubleComponent }

constructor TDoubleComponent.Create(AOwner: TComponent);
begin
  inherited;
  F:=2.34;
end;

{ TExtendedComponent }

constructor TExtendedComponent.Create(AOwner: TComponent);
begin
  inherited;
  F:=3.45;
end;

{ TCompComponent }
(*
constructor TCompComponent.Create(AOwner: TComponent);
begin
  inherited;
  F:=4.56;
end;
*)

{ TCurrencyComponent }

constructor TCurrencyComponent.Create(AOwner: TComponent);
begin
  inherited;
  F:=5.67;
end;

{ TDateTimeComponent }

constructor TDateTimeComponent.Create(AOwner: TComponent);
begin
  inherited;
  F:=EncodeDate(1996,8,1);
end;

{ TDateTimeComponent2 }

constructor TDateTimeComponent2.Create(AOwner: TComponent);
begin
  inherited;
  F:=EncodeTime(23,20,0,0);
end;

{ TDateTimeComponent3 }

constructor TDateTimeComponent3.Create(AOwner: TComponent);
begin
  inherited;
  F:=EncodeDate(1996,8,1)+EncodeTime(23,20,0,0);
end;

{ TEnumComponent }

constructor TEnumComponent.Create(AOwner: TComponent);
begin
  inherited;
  F:=Four;
end;

{ TSetComponent }

constructor TSetComponent.Create(AOwner: TComponent);
begin
  inherited;
  F:=[two,five];
end;

{ TIntegerComponent4 }

constructor TIntegerComponent4.Create(AOwner: TComponent);
begin
  inherited;
  FIntProp:=6;
end;

{ TIntegerComponent5 }

constructor TIntegerComponent5.Create(AOwner: TComponent);
begin
  inherited;
  FintProp:=5;
end;

{ TInt64Component5 }

constructor TInt64Component5.Create(AOwner: TComponent);
begin
  inherited;
  FIntProp:=7;
end;

{ TInt64Component6 }

constructor TInt64Component6.Create(AOwner: TComponent);
begin
  inherited;
  FintProp:=8;
end;

{ TEnumComponent3 }

constructor TEnumComponent3.Create(AOwner: TComponent);
begin
  inherited;
  F:=Three;
end;

{ TEnumComponent4 }

constructor TEnumComponent4.Create(AOwner: TComponent);
begin
  inherited;
  F:=Two;
end;

{ TSetComponent4 }

constructor TSetComponent4.Create(AOwner: TComponent);
begin
  inherited;
  F:=[Three,Six];
end;

{ TSetComponent3 }

constructor TSetComponent3.Create(AOwner: TComponent);
begin
  inherited;
  F:=[One,Four];
end;

{ TMultipleComponent }

constructor TMultipleComponent.Create(AOwner: TComponent);
begin
  inherited;
  FInt:=1;
  FCurrency:=2.3;
  FString:='A String';
  FDice:=two;
  F:=[three,four];
end;

{ TTestPersistent1 }

procedure TTestPersistent1.Assign(ASource: TPersistent);

Var
  T :TTestPersistent1;

begin
  If ASource is TTestPersistent1 then
    begin
    T:=ASource as TTestPersistent1;
    FInt:=T.FInt;
    FAString:=T.FAString;
    end
  else
    inherited;
end;

{ TPersistentComponent }

constructor TPersistentComponent.Create(AOwner: TComponent);
begin
  inherited;
  FPers:=TTestPersistent1.Create;
  FPers.AInteger:=3;
  FPers.AString:='A persistent string';
end;

Destructor TPersistentComponent.Destroy;

begin
  FreeAndNil(FPers);
  Inherited;
end;

procedure TPersistentComponent.SetPers(const Value: TTestPersistent1);
begin
  FPers.Assign(Value);
end;

{ TCollectionComponent }

Procedure TCollectionComponent.SetColl(AColl : TCollection);

begin
  FColl.Assign(AColl);
end;

Constructor TCollectionComponent.Create(AOwner : TComponent);

begin
  Inherited;
  FColl:=TCollection.Create(TTestItem);
end;

Destructor TCollectionComponent.Destroy;

begin
  FreeAndNil(FColl);
  Inherited;
end;

{ TCollectionComponent2 }

Constructor TCollectionComponent2.Create(AOwner : TComponent);

begin
  Inherited;
  (FColl.Add as TTestItem).StrProp:='First';
  (FColl.Add as TTestItem).StrProp:='Second';
  (FColl.Add as TTestItem).StrProp:='Third';
end;

{ TCollectionComponen3 }

Constructor TCollectionComponent3.Create(AOwner : TComponent);

begin
  Inherited;
  (FColl.Add as TTestItem).StrProp:='First';
  (FColl.Add as TTestItem).StrProp:='';
  (FColl.Add as TTestItem).StrProp:='Third';
end;

{ TCollectionComponent4 }

constructor TCollectionComponent4.Create(AOwner: TComponent);
begin
  inherited;
  FColl:=TTestCollection.Create;
  (FColl.Add as TTestItem).StrProp:='Something'
end;

destructor TCollectionComponent4.Destroy;
begin
  FreeAndNil(FColl);
  inherited;
end;

procedure TCollectionComponent4.SetColl(AColl: TTestCollection);
begin
  FColl.Assign(AColl);
end;

{ TCollectionComponent5 }

procedure TCollectionComponent5.SetColl(AColl: TCollection);
begin
  FColl.Assign(AColl);
end;

constructor TCollectionComponent5.Create(AOwner: TComponent);
var
  Item : TTest2Item;
begin
  inherited Create(AOwner);
  FColl:=TCollection.Create(TTest2Item);
  Item := FColl.Add as TTest2Item;
  Item.StrProp1 := 'Something';
  Item.StrProp2 := 'Otherthing';
  Item := FColl.Add as TTest2Item;
  Item.StrProp1 := 'Something 2';
  Item.StrProp2 := 'Otherthing 2';
end;

destructor TCollectionComponent5.Destroy;
begin
  FreeAndNil(FColl);
  inherited Destroy;
end;

{ TTestCollection }

Constructor TTestCollection.Create;
begin
  Inherited Create(TTestitem);
  PropName:='MyCollProp';
end;

{ TStreamedOwnedComponent }

Constructor TStreamedOwnedComponent.Create(AOwner : TComponent);

begin
  Inherited;
  Sub:=TIntegerComponent.Create(Self);
  Sub.Name:='Sub';
end;

{ TStreamedOwnedComponents }

constructor TStreamedOwnedComponents.Create(AOwner: TComponent);
begin
  inherited;
  SubA:=TIntegerComponent.Create(Self);
  SubA.Name:='SubA';
  SubB:=TStringComponent.Create(Self);
  SubB.Name:='SubB';
end;


Constructor TOwnedComponent.Create(AOwner : TComponent);

Var
  C: TComponent;

begin
  Inherited;
  C:=TIntegerComponent.Create(Self);
  C.Name:='SubComponent';
  CompProp:=C;
end;


{ TMethodComponent }

Constructor TMethodComponent.Create(AOwner : TComponent);

begin
  Inherited;
  MethodProp:=@MyMethod;
end;

Procedure TMethodComponent.MyMethod;

begin
  // Do nothing.
end;

{ TMethodComponent2 }

constructor TMethodComponent2.Create(AOwner: TComponent);

Var
  C : TMethodComponent;

begin
  inherited;
  C:=TMethodComponent.Create(Self);
  C.Name:='AComponent';
  C.MethodProp:=@MyMethod2;
end;

Procedure TMethodComponent2.MyMethod2;

begin
 // Do nothng
end;

{ TMyColl }

function TMyColl.GetIt(index : Integer): TMyItem;
begin
  Result:=Items[Index] as TMyItem;
end;

procedure TMyColl.SetIt(index : Integer; const AValue: TMyItem);
begin
  Items[Index]:=AValue;
end;

{ TCollComp }

procedure TCollComp.SetMyColl(const AValue: TMyColl);
begin
  if (FMyColl=AValue) then
    exit;
  FMyColl.Assign(AValue);
end;

constructor TCollComp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMyColl:=TMyCOll.Create(TMyItem);
end;

destructor TCollComp.Destroy;
begin
  FreeAndNil(FMyColl);
  inherited Destroy;
end;

function TCollComp.ToStream: TStream;
begin
  Result:=TMemoryStream.Create;
  Result.WriteComponent(Self);
  Result.Position:=0;
end;

procedure TCollComp.FromStream(AStream: TStream);
begin
  AStream.ReadComponent(Self);
  Astream.Free;
end;

procedure TMyItem.Assign(Source: TPersistent);

Var
  I : TMyItem;

begin
  If (Source is TMyItem) then
    begin
    I:=Source as TMyItem;
    FNR:=I.NR;
    FStr:=I.Str;
    end
  else
    inherited Assign(Source);
end;

end.
