{ Component streaming tests.

  Copyright (C) 2020 Michael Van Canneyt michael@freepascal.org

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

Unit tccompstreaming;

interface

Uses
  SysUtils, Classes, tcstreaming, fpcunit, testregistry, testcomps;

Type

  { TTestComponentStream }

  TTestComponentStream = Class(TTestStreaming)
  Published
      Procedure TestTEmptyComponent;
      Procedure TestTEmptyComponentText;
      Procedure TestTIntegerComponent;
      Procedure TestTIntegerComponentText;
      Procedure TestTIntegerComponent2;
      Procedure TestTIntegerComponent2Text;
      Procedure TestTIntegerComponent3;
      Procedure TestTIntegerComponent3Text;
      Procedure TestTIntegerComponent4;
      Procedure TestTIntegerComponent5;
      Procedure TestTInt64Component;
      Procedure TestTInt64ComponentText;
      Procedure TestTInt64Component2;
      Procedure TestTInt64Component2Text;
      Procedure TestTInt64Component3;
      Procedure TestTInt64Component3Text;
      Procedure TestTInt64Component4;
      Procedure TestTInt64Component4Text;
      Procedure TestTInt64Component5;
      Procedure TestTInt64Component6;
      Procedure TestTCharComponent;
      Procedure TestTCharComponentText;
      Procedure TestTStringComponent;
      Procedure TestTStringComponentText;
      Procedure TestTStringComponent2;
      Procedure TestTStringComponent3;
      Procedure TestTStringComponent4;
      Procedure TestTStringComponent3Text;
      Procedure TestTStringComponent4Text;
      Procedure TestTWideStringComponent;
      Procedure TestTWideStringComponentText;
      Procedure TestTWideStringComponent2;
      Procedure TestTSingleComponent;
      Procedure TestTDoubleComponent;
      Procedure TestTDoubleComponentText;
      Procedure TestTExtendedComponent;
//      Procedure TestTCompComponent;
      Procedure TestTCurrencyComponent;
      procedure TestTCurrencyComponentText;
      Procedure TestTDateTimeComponent;
      Procedure TestTDateTimeComponent2;
      Procedure TestTDateTimeComponent3;
      Procedure TestTEnumComponent;
      Procedure TestTEnumComponentText;
      Procedure TestTEnumComponent2;
      Procedure TestTEnumComponent3;
      Procedure TestTEnumComponent4;
      Procedure TestTEnumComponent5;
      Procedure TestTSetComponent;
      Procedure TestTSetComponentText;
      Procedure TestTSetComponent2;
      Procedure TestTSetComponent3;
      Procedure TestTSetComponent4;
      Procedure TestTMultipleComponent;
      Procedure TestTMultipleComponentText;
      Procedure TestTPersistentComponent;
      Procedure TestTPersistentComponentText;
      Procedure TestTCollectionComponent;
      Procedure TestTCollectionComponentText;
      Procedure TestTCollectionComponent2;
      Procedure TestTCollectionComponent2Text;
      Procedure TestTCollectionComponent3;
      Procedure TestTCollectionComponent4;
      Procedure TestTCollectionComponent5;
      Procedure TestTOwnedComponent;
      Procedure TestTOwnedComponentText;
      Procedure TestTStreamedOwnedComponent;
      Procedure TestTStreamedOwnedComponentText;
      Procedure TestTStreamedOwnedComponents;
      Procedure TestTStreamedOwnedComponentsText;
      Procedure TestTMethodComponent;
      Procedure TestTMethodComponentText;
      Procedure TestTMethodComponent2;
      Procedure TestTMethodComponent2Text;
      // Read
      // ReadText will convert to text by calling text version, and read back after objecttexttobinary.
      Procedure TestTEmptyComponentRead;
      procedure TestTEmptyComponentReadText;
      Procedure TestTIntegerComponentRead;
      procedure TestTIntegerComponentReadText;
      Procedure TestTIntegerComponent2Read;
      Procedure TestTIntegerComponent2ReadText;
      Procedure TestTIntegerComponent3Read;
      Procedure TestTIntegerComponent3ReadText;
      Procedure TestTIntegerComponent4Read;
      Procedure TestTIntegerComponent5Read;
      Procedure TestTInt64ComponentRead;
      Procedure TestTInt64ComponentReadText;
      Procedure TestTInt64Component2Read;
      Procedure TestTInt64Component2ReadText;
      Procedure TestTInt64Component3Read;
      Procedure TestTInt64Component3ReadText;
      Procedure TestTInt64Component4Read;
      Procedure TestTInt64Component4ReadText;
      Procedure TestTInt64Component5Read;
      Procedure TestTInt64Component6Read;
      Procedure TestTCharComponentRead;
      Procedure TestTStringComponentRead;
      Procedure TestTStringComponentReadText;
      Procedure TestTStringComponent2Read;
      Procedure TestTWideStringComponentRead;
      Procedure TestTWideStringComponentReadText;
      Procedure TestTWideStringComponent2Read;
      Procedure TestTSingleComponentRead;
      Procedure TestTDoubleComponentRead;
      Procedure TestTDoubleComponentReadText;
      Procedure TestTExtendedComponentRead;
//      Procedure TestTCompComponent;
      Procedure TestTCurrencyComponentRead;
      Procedure TestTDateTimeComponentRead;
      Procedure TestTDateTimeComponent2Read;
      Procedure TestTDateTimeComponent3Read;
      Procedure TestTEnumComponentRead;
      Procedure TestTEnumComponentReadText;
      Procedure TestTEnumComponent2Read;
      Procedure TestTEnumComponent3Read;
      Procedure TestTEnumComponent4Read;
      Procedure TestTEnumComponent5Read;
      Procedure TestTSetComponentRead;
      Procedure TestTSetComponentReadText;
      Procedure TestTSetComponent2Read;
      Procedure TestTSetComponent3Read;
      Procedure TestTSetComponent4Read;
      Procedure TestTMultipleComponentRead;
      Procedure TestTMultipleComponentReadText;
      Procedure TestTPersistentComponentRead;
      Procedure TestTPersistentComponentReadText;
      Procedure TestTCollectionComponentRead;
      Procedure TestTCollectionComponentReadText;
      Procedure TestTCollectionComponent2Read;
      Procedure TestTCollectionComponent2ReadText;
      Procedure TestTCollectionComponent3Read;
      Procedure TestTCollectionComponent4Read;
      Procedure TestTCollectionComponent5Read;
      Procedure TestTOwnedComponentRead;
      Procedure TestTOwnedComponentReadText;
      Procedure TestTStreamedOwnedComponentRead;
      Procedure TestTStreamedOwnedComponentReadText;
      Procedure TestTStreamedOwnedComponentsRead;
      Procedure TestTStreamedOwnedComponentsReadText;
    end;


  { TTestCollectionStream }

  TTestCollectionStream = Class(TTestCase)
  private
    procedure CompareColl(CA, CB: TMyColl);
    function CreateColl(Anr: Integer): TCollComp;
    function EmptyComp: TCollComp;
    procedure TestNr(ACount: Integer);
  Published
    procedure Test1;
    procedure Test2;
    procedure Test3;
    procedure TestClear;
    procedure TestEmpty;
  end;

Implementation

Const
   LE = sLineBreak;

procedure TTestComponentStream.TestTEmptyComponent;


Var
  C : TComponent;

begin
  C:=TEmptyComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TEmptyComponent');
    ExpectBareString('TestTEmptyComponent');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTEmptyComponentText;

Const
   SData = 'object TestTEmptyComponent: TEmptyComponent'+sLineBreak+'end'+sLineBreak;

begin
  TestTEmptyComponent;
  CheckAsString(sData);
end;

procedure TTestComponentStream.TestTEmptyComponentRead;

Var
  C : TEmptyComponent;
begin
  TestTEmptyComponent;
  C:=TEmptyComponent.Create(Nil);
  try
    LoadFromStream(C);
    AssertEquals('Name','TestTEmptyComponent',C.Name);
  finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTEmptyComponentReadText;

Var
  C : TEmptyComponent;

begin
  TestTEmptyComponentText;
  C:=TEmptyComponent.Create(Nil);
  try
    LoadFromtextStream(C);
    AssertEquals('Name','TestTEmptyComponent',C.Name);
  finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTIntegerComponent;


Var
  C : TComponent;

begin
  C:=TIntegerComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TIntegerComponent');
    ExpectBareString('TestTIntegerComponent');
    ExpectBareString('IntProp');
    ExpectInteger(3);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTIntegerComponentText;

Const
   SData =
     'object TestTIntegerComponent: TIntegerComponent'+sLineBreak+
     '  IntProp = 3'+sLineBreak+
     'end'+sLineBreak;

begin
  TestTIntegerComponent;
  CheckAsString(sData);
end;

procedure TTestComponentStream.TestTIntegerComponentRead;

Var
  C : TIntegerComponent;

begin
  TestTIntegerComponent;
  C:=TIntegerComponent.Create(Nil);
  Try
    LoadFromStream(C);
    AssertEquals('Name','TestTIntegerComponent',C.Name);
    AssertEquals('IntProp',3,C.IntProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTIntegerComponentReadText;

Var
  C : TIntegerComponent;

begin
  TestTIntegerComponentText;
  C:=TIntegerComponent.Create(Nil);
  Try
    LoadFromTextStream(C);
    AssertEquals('Name','TestTIntegerComponent',C.Name);
    AssertEquals('IntProp',3,C.IntProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTIntegerComponent2Read;

Var
  C : TIntegerComponent2;

begin
  TestTIntegerComponent2;
  C:=TIntegerComponent2.Create(Nil);
  Try
    LoadFromStream(C);
    AssertEquals('Name','TestTIntegerComponent2',C.Name);
    AssertEquals('IntProp',1024,C.IntProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTIntegerComponent2ReadText;
Var
  C : TIntegerComponent2;

begin
  TestTIntegerComponent2Text;
  C:=TIntegerComponent2.Create(Nil);
  Try
    LoadFromTextStream(C);
    AssertEquals('Name','TestTIntegerComponent2',C.Name);
    AssertEquals('IntProp',1024,C.IntProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTIntegerComponent3Read;

Var
  C : TIntegerComponent3;

begin
  TestTIntegerComponent3;
  C:=TIntegerComponent3.Create(Nil);
  Try
    LoadFromStream(C);
    AssertEquals('Name','TestTIntegerComponent3',C.Name);
    AssertEquals('IntProp',262144,C.IntProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTIntegerComponent3ReadText;
Var
  C : TIntegerComponent3;

begin
  TestTIntegerComponent3Text;
  C:=TIntegerComponent3.Create(Nil);
  Try
    LoadFromTextStream(C);
    AssertEquals('Name','TestTIntegerComponent3',C.Name);
    AssertEquals('IntProp',262144,C.IntProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTIntegerComponent4Read;

Var
  C : TIntegerComponent4;

begin
  TestTIntegerComponent4;
  C:=TIntegerComponent4.Create(Nil);
  Try
    LoadFromStream(C);
    AssertEquals('Name','TestTIntegerComponent4',C.Name);
    AssertEquals('IntProp',6,C.IntProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTIntegerComponent5Read;

Var
  C : TIntegerComponent5;

begin
  TestTIntegerComponent5;
  C:=TIntegerComponent5.Create(Nil);
  Try
    LoadFromStream(C);
    AssertEquals('Name','TestTIntegerComponent5',C.Name);
    AssertEquals('IntProp',5,C.IntProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTInt64ComponentRead;

Var
  C : TInt64Component;

begin
  TestTInt64Component;
  C:=TInt64Component.Create(Nil);
  Try
    C.Int64Prop:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTInt64Component',C.Name);
    AssertEquals('Int64Prop',4,C.Int64Prop);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTInt64ComponentReadText;
Var
  C : TInt64Component;

begin
  TestTInt64ComponentText;
  C:=TInt64Component.Create(Nil);
  Try
    C.Int64Prop:=0;
    LoadFromTextStream(C);
    AssertEquals('Name','TestTInt64Component',C.Name);
    AssertEquals('Int64Prop',4,C.Int64Prop);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTInt64Component2Read;

Var
  C : TInt64Component2;

begin
  TestTInt64Component2;
  C:=TInt64Component2.Create(Nil);
  Try
    C.Int64Prop:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTInt64Component2',C.Name);
    AssertEquals('Int64Prop',2 shl 9,C.Int64Prop);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTInt64Component2ReadText;

Var
  C : TInt64Component2;

begin
  TestTInt64Component2Text;
  C:=TInt64Component2.Create(Nil);
  Try
    C.Int64Prop:=0;
    LoadFromTextStream(C);
    AssertEquals('Name','TestTInt64Component2',C.Name);
    AssertEquals('Int64Prop',2 shl 9,C.Int64Prop);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTInt64Component3Read;

Var
  C : TInt64Component3;

begin
  TestTInt64Component3;
  C:=TInt64Component3.Create(Nil);
  Try
    C.Int64Prop:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTInt64Component3',C.Name);
    AssertEquals('Int64Prop',2 shl 17,C.Int64Prop);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTInt64Component3ReadText;
Var
  C : TInt64Component3;

begin
  TestTInt64Component3Text;
  C:=TInt64Component3.Create(Nil);
  Try
    C.Int64Prop:=0;
    LoadFromTextStream(C);
    AssertEquals('Name','TestTInt64Component3',C.Name);
    AssertEquals('Int64Prop',2 shl 17,C.Int64Prop);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTInt64Component4Read;

Var
  C : TInt64Component4;

begin
  TestTInt64Component4;
  C:=TInt64Component4.Create(Nil);
  Try
    C.Int64Prop:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTInt64Component4',C.Name);
    AssertEquals('Int64Prop',NativeInt(MaxInt)+NativeInt(2 shl 14),C.Int64Prop);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTInt64Component4ReadText;
Var
  C : TInt64Component4;

begin
  TestTInt64Component4Text;
  C:=TInt64Component4.Create(Nil);
  Try
    C.Int64Prop:=0;
    LoadFromTextStream(C);
    AssertEquals('Name','TestTInt64Component4',C.Name);
    AssertEquals('Int64Prop',NativeInt(MaxInt)+NativeInt(2 shl 14),C.Int64Prop);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTInt64Component5Read;

Var
  C : TInt64Component5;

begin
  TestTInt64Component5;
  C:=TInt64Component5.Create(Nil);
  Try
    C.Int64Prop:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTInt64Component5',C.Name);
    // Not written, so zero remains
    AssertEquals('Int64Prop',0,C.Int64Prop);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTInt64Component6Read;
Var
  C : TInt64Component6;

begin
  TestTInt64Component6;
  C:=TInt64Component6.Create(Nil);
  Try
    C.Int64Prop:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTInt64Component6',C.Name);
    AssertEquals('Int64Prop',8,C.Int64Prop);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTCharComponentRead;
Var
  C : TCharComponent;

begin
  TestTCharComponent;
  C:=TCharComponent.Create(Nil);
  Try
    C.CharProp:='A';
    LoadFromStream(C);
    AssertEquals('Name','TestTCharComponent',C.Name);
    AssertEquals('StringProp',#10,C.CharProp);
  Finally
    C.Free;
  end;

end;

procedure TTestComponentStream.TestTStringComponentRead;

Var
  C : TStringComponent;

begin
  TestTStringComponent;
  C:=TStringComponent.Create(Nil);
  Try
    C.StringProp:='';
    LoadFromStream(C);
    AssertEquals('Name','TestTStringComponent',C.Name);
    AssertEquals('StringProp','A string',C.StringProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTStringComponentReadText;
Var
  C : TStringComponent;

begin
  TestTStringComponentText;
  C:=TStringComponent.Create(Nil);
  Try
    C.StringProp:='';
    LoadFromTextStream(C);
    AssertEquals('Name','TestTStringComponent',C.Name);
    AssertEquals('StringProp','A string',C.StringProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTStringComponent2Read;

Var
  C : TStringComponent2;

begin
  TestTStringComponent2;
  C:=TStringComponent2.Create(Nil);
  Try
    C.StringProp:='abc';
    LoadFromStream(C);
    AssertEquals('Name','TestTStringComponent2',C.Name);
    AssertEquals('StringProp','abc',C.StringProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTWideStringComponentRead;

Var
  C : TWideStringComponent;

begin
  TestTWideStringComponent;
  C:=TWideStringComponent.Create(Nil);
  Try
    C.WideStringProp:='abc';
    LoadFromStream(C);
    AssertEquals('Name','TestTWideStringComponent',C.Name);
    AssertEquals('WideStringProp','Some WideString',C.WideStringProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTWideStringComponentReadText;
Var
  C : TWideStringComponent;

begin
  TestTWideStringComponentText;
  C:=TWideStringComponent.Create(Nil);
  Try
    C.WideStringProp:='abc';
    LoadFromTextStream(C);
    AssertEquals('Name','TestTWideStringComponent',C.Name);
    AssertEquals('WideStringProp','Some WideString',C.WideStringProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTWideStringComponent2Read;
Var
  C : TWideStringComponent2;

begin
  TestTWideStringComponent2;
  C:=TWideStringComponent2.Create(Nil);
  Try
    C.WideStringProp:='abc';
    LoadFromStream(C);
    AssertEquals('Name','TestTWideStringComponent2',C.Name);
    AssertEquals('WideStringProp','abc',C.WideStringProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTSingleComponentRead;

Var
  C : TSingleComponent;

begin
  TestTSingleComponent;
  C:=TSingleComponent.Create(Nil);
  Try
    C.SingleProp:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTSingleComponent',C.Name);
    AssertEquals('SingleProp',1.23,C.SingleProp,0.01);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTDoubleComponentRead;

Var
  C : TDoubleComponent;

begin
  TestTDoubleComponent;
  C:=TDoubleComponent.Create(Nil);
  Try
    C.DoubleProp:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTDoubleComponent',C.Name);
    AssertEquals('DoubleProp',2.34,C.DoubleProp,0.01);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTDoubleComponentReadText;
Var
  C : TDoubleComponent;

begin
  TestTDoubleComponentText;
  C:=TDoubleComponent.Create(Nil);
  Try
    C.DoubleProp:=0;
    LoadFromTextStream(C);
    AssertEquals('Name','TestTDoubleComponent',C.Name);
    // TODO: extend precision to 0.1
    AssertEquals('DoubleProp',2.34,C.DoubleProp,0.1);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTExtendedComponentRead;

Var
  C : TExtendedComponent;

begin
  TestTExtendedComponent;
  C:=TExtendedComponent.Create(Nil);
  Try
    C.ExtendedProp:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTExtendedComponent',C.Name);
    AssertEquals('ExtendedProp',3.45,C.ExtendedProp,0.01);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTCurrencyComponentRead;

Var
  C : TCurrencyComponent;

begin
  TestTCurrencyComponent;
  C:=TCurrencyComponent.Create(Nil);
  Try
    C.CurrencyProp:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTCurrencyComponent',C.Name);
    AssertEquals('CurrencyProp',5.67,C.CurrencyProp,0.01);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTDateTimeComponentRead;

Var
  C : TDateTimeComponent;

begin
  TestTDateTimeComponent;
  C:=TDateTimeComponent.Create(Nil);
  Try
    C.DateTimeProp:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTDateTimeComponent',C.Name);
    AssertEquals('DateTimeProp',35278.00,C.DateTimeProp,0.01);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTDateTimeComponent2Read;

Var
  C : TDateTimeComponent2;

begin
  TestTDateTimeComponent2;
  C:=TDateTimeComponent2.Create(Nil);
  Try
    C.DateTimeProp:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTDateTimeComponent2',C.Name);
    AssertEquals('DateTimeProp',0.97,C.DateTimeProp,0.01);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTDateTimeComponent3Read;
Var
  C : TDateTimeComponent3;

begin
  TestTDateTimeComponent3;
  C:=TDateTimeComponent3.Create(Nil);
  Try
    C.DateTimeProp:=0;
    LoadFromStream(C);
    AssertEquals('Name','TestTDateTimeComponent3',C.Name);
    AssertEquals('DateTimeProp',35278.97,C.DateTimeProp,0.01);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTEnumComponentRead;

Var
  C : TEnumComponent;

begin
  TestTEnumComponent;
  C:=TEnumComponent.Create(Nil);
  Try
    C.Dice:=One;
    LoadFromStream(C);
    AssertEquals('Name','TestTEnumComponent',C.Name);
    AssertTrue('Dice',four=C.Dice);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTEnumComponentReadText;
Var
  C : TEnumComponent;

begin
  TestTEnumComponentText;
  C:=TEnumComponent.Create(Nil);
  Try
    C.Dice:=One;
    LoadFromTextStream(C);
    AssertEquals('Name','TestTEnumComponent',C.Name);
    AssertTrue('Dice',four=C.Dice);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTEnumComponent2Read;

Var
  C : TEnumComponent2;

begin
  TestTEnumComponent2;
  C:=TEnumComponent2.Create(Nil);
  Try
    C.Dice:=Three;
    LoadFromStream(C);
    AssertEquals('Name','TestTEnumComponent2',C.Name);
    // Stream does  a value
    AssertTrue('Dice',One=C.Dice);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTEnumComponent3Read;

Var
  C : TEnumComponent3;

begin
  TestTEnumComponent3;
  C:=TEnumComponent3.Create(Nil);
  Try
    LoadFromStream(C);
    AssertEquals('Name','TestTEnumComponent3',C.Name);
    // Stream does not contain a value
    AssertTrue('Dice',Three=C.Dice);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTEnumComponent4Read;

Var
  C : TEnumComponent4;

begin
  TestTEnumComponent4;
  C:=TEnumComponent4.Create(Nil);
  Try
    C.Dice:=six;
    LoadFromStream(C);
    AssertEquals('Name','TestTEnumComponent4',C.Name);
    // Stream does not contain a value
    AssertTrue('Dice',Six=C.Dice);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTEnumComponent5Read;
Var
  C : TEnumComponent5;

begin
  TestTEnumComponent5;
  C:=TEnumComponent5.Create(Nil);
  Try
    C.Dice:=six;
    LoadFromStream(C);
    AssertEquals('Name','TestTEnumComponent5',C.Name);
    // Stream does not contain a value
    AssertTrue('Dice',Six=C.Dice);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTSetComponentRead;

Var
  C : TSetComponent;

begin
  TestTSetComponent;
  C:=TSetComponent.Create(Nil);
  Try
    C.Throw:=[];
    LoadFromStream(C);
    AssertEquals('Name','TestTSetComponent',C.Name);
    AssertTrue('Throw',[two,five]=C.Throw);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTSetComponentReadText;
Var
  C : TSetComponent;

begin
  TestTSetComponentText;
  C:=TSetComponent.Create(Nil);
  Try
    C.Throw:=[];
    LoadFromTextStream(C);
    AssertEquals('Name','TestTSetComponent',C.Name);
    AssertTrue('Throw',[two,five]=C.Throw);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTSetComponent2Read;

Var
  C : TSetComponent2;

begin
  TestTSetComponent2;
  C:=TSetComponent2.Create(Nil);
  Try
    C.Throw:=[one,six];
    LoadFromStream(C);
    AssertEquals('Name','TestTSetComponent2',C.Name);
    // Nothing was streamed
    AssertTrue('Throw',[one,six]=C.Throw);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTSetComponent3Read;

Var
  C : TSetComponent3;

begin
  TestTSetComponent3;
  C:=TSetComponent3.Create(Nil);
  Try
    C.Throw:=[two,six];
    LoadFromStream(C);
    AssertEquals('Name','TestTSetComponent3',C.Name);
    // Nothing was streamed
    AssertTrue('Throw',[one,four]=C.Throw);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTSetComponent4Read;

Var
  C : TSetComponent4;

begin
  TestTSetComponent4;
  C:=TSetComponent4.Create(Nil);
  Try
    C.Throw:=[two,six];
    LoadFromStream(C);
    AssertEquals('Name','TestTSetComponent4',C.Name);
    // Nothing was streamed
    AssertTrue('Throw',[two,six]=C.Throw);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTMultipleComponentRead;

Var
  C : TMultipleComponent;

begin
  TestTMultipleComponent;
  C:=TMultipleComponent.Create(Nil);
  Try
    c.IntProp:=23;
    C.Dice:=six;
    C.CurrencyProp:=12.3;
    C.StringProp:='abc';
    LoadFromStream(C);
    AssertEquals('Name','TestTMultipleComponent',C.Name);
    AssertEquals('IntProp',1,C.IntProp);
    AssertEquals('StringProp','A String',C.StringProp);
    AssertEquals('CurrencyProp',2.3,C.CurrencyProp,0.1);
    AssertTrue('Dice',two=C.Dice);
    AssertTrue('Throw',[three,four]=C.Throw);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTMultipleComponentReadText;

Var
  C : TMultipleComponent;

begin
  TestTMultipleComponentText;
  C:=TMultipleComponent.Create(Nil);
  Try
    c.IntProp:=23;
    C.Dice:=six;
    C.CurrencyProp:=12.3;
    C.StringProp:='abc';
    LoadFromTextStream(C);
    AssertEquals('Name','TestTMultipleComponent',C.Name);
    AssertEquals('IntProp',1,C.IntProp);
    AssertEquals('StringProp','A String',C.StringProp);
    AssertEquals('CurrencyProp',2.3,C.CurrencyProp,0.1);
    AssertTrue('Dice',two=C.Dice);
    AssertTrue('Throw',[three,four]=C.Throw);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTPersistentComponentRead;

Var
  C : TPersistentComponent;

begin
  TestTPersistentComponent;
  C:=TPersistentComponent.Create(Nil);
  Try
    C.Persist.AInteger:=36;
    C.Persist.AString:='nono';
    LoadFromStream(C);
    AssertEquals('Name','TestTPersistentComponent',C.Name);
    AssertEquals('Persist.AInteger',3,C.Persist.AInteger);
    AssertEquals('Persist.AString','A persistent string',C.Persist.AString);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTPersistentComponentReadText;
Var
  C : TPersistentComponent;

begin
  TestTPersistentComponentText;
  C:=TPersistentComponent.Create(Nil);
  Try
    C.Persist.AInteger:=36;
    C.Persist.AString:='nono';
    LoadFromTextStream(C);
    AssertEquals('Name','TestTPersistentComponent',C.Name);
    AssertEquals('Persist.AInteger',3,C.Persist.AInteger);
    AssertEquals('Persist.AString','A persistent string',C.Persist.AString);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTCollectionComponentRead;

Var
  C : TCollectionComponent;

begin
  TestTCollectionComponent;
  C:=TCollectionComponent.Create(Nil);
  Try
    C.Coll.Add;
    LoadFromStream(C);
    AssertEquals('Name','TestTCollectionComponent',C.Name);
    // If the stream does not have a collection, it does not get cleared
    AssertEquals('Coll count',1,C.Coll.Count);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTCollectionComponentReadText;

Var
  C : TCollectionComponent;

begin
  TestTCollectionComponentText;
  C:=TCollectionComponent.Create(Nil);
  Try
    C.Coll.Add;
    LoadFromTextStream(C);
    AssertEquals('Name','TestTCollectionComponent',C.Name);
    // If the stream does not have a collection, it does not get cleared
    AssertEquals('Coll count',1,C.Coll.Count);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTCollectionComponent2Read;

Var
  C : TCollectionComponent2;

begin
  TestTCollectionComponent2;
  C:=TCollectionComponent2.Create(Nil);
  Try
    C.Coll.Add;
    LoadFromStream(C);
    AssertEquals('Name','TestTCollectionComponent2',C.Name);
    AssertEquals('Coll count',3,C.Coll.Count);
    AssertEquals('Correct class type',TTestItem,C.Coll.Items[0].ClassType);
    AssertEquals('Coll 0 Property','First',TTestItem(C.Coll.items[0]).StrProp);
    AssertEquals('Coll 1 Property','Second',TTestItem(C.Coll.Items[1]).StrProp);
    AssertEquals('Coll 2 Property','Third',TTestItem(C.Coll.Items[2]).StrProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTCollectionComponent2ReadText;

Var
  C : TCollectionComponent2;

begin
  TestTCollectionComponent2Text;
  C:=TCollectionComponent2.Create(Nil);
  Try
    C.Coll.Add;
    LoadFromTextStream(C);
    AssertEquals('Name','TestTCollectionComponent2',C.Name);
    AssertEquals('Coll count',3,C.Coll.Count);
    AssertEquals('Correct class type',TTestItem,C.Coll.Items[0].ClassType);
    AssertEquals('Coll 0 Property','First',TTestItem(C.Coll.items[0]).StrProp);
    AssertEquals('Coll 1 Property','Second',TTestItem(C.Coll.Items[1]).StrProp);
    AssertEquals('Coll 2 Property','Third',TTestItem(C.Coll.Items[2]).StrProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTCollectionComponent3Read;

Var
  C : TCollectionComponent3;

begin
  TestTCollectionComponent3;
  C:=TCollectionComponent3.Create(Nil);
  Try
    C.Coll.Add;
    LoadFromStream(C);
    AssertEquals('Name','TestTCollectionComponent3',C.Name);
    AssertEquals('Coll count',3,C.Coll.Count);
    AssertEquals('Correct class type',TTestItem,C.Coll.Items[0].ClassType);
    AssertEquals('Coll 0 Property','First',TTestItem(C.Coll.items[0]).StrProp);
    AssertEquals('Coll 1 Property','',TTestItem(C.Coll.Items[1]).StrProp);
    AssertEquals('Coll 2 Property','Third',TTestItem(C.Coll.Items[2]).StrProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTCollectionComponent4Read;

Var
  C : TCollectionComponent4;

begin
  TestTCollectionComponent4;
  C:=TCollectionComponent4.Create(Nil);
  Try
    C.Coll.Add;
    LoadFromStream(C);
    AssertEquals('Name','TestTCollectionComponent4',C.Name);
    AssertEquals('Coll count',1,C.Coll.Count);
    AssertEquals('Correct class type',TTestItem,C.Coll.Items[0].ClassType);
    AssertEquals('Coll 0 Property','Something',TTestItem(C.Coll.items[0]).StrProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTCollectionComponent5Read;

Var
  C : TCollectionComponent5;

begin
  TestTCollectionComponent5;
  C:=TCollectionComponent5.Create(Nil);
  Try
    C.Coll.Add;
    LoadFromStream(C);
    AssertEquals('Name','TestTCollectionComponent5',C.Name);
    AssertEquals('Coll count',2,C.Coll.Count);
    AssertEquals('Correct class type',TTest2Item,C.Coll.Items[0].ClassType);
    AssertEquals('Coll 0 Property 1','Something',TTest2Item(C.Coll.items[0]).StrProp1);
    AssertEquals('Coll 0 Property 2','Otherthing',TTest2Item(C.Coll.items[0]).StrProp2);
    AssertEquals('Coll 1 property 1','Something 2',TTest2Item(C.Coll.items[1]).StrProp1);
    AssertEquals('Coll 1 property 2','Otherthing 2',TTest2Item(C.Coll.items[1]).StrProp2);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTOwnedComponentRead;

Var
  C : TOwnedComponent;
  C2 : TComponent;
begin
  TestTOwnedComponent;
  C:=TOwnedComponent.Create(Nil);
  try
    C2:=C.CompProp;
    C.CompProp:=nil;
    LoadFromStream(C);
    AssertEquals('Name','TestTOwnedComponent',C.Name);
    AssertEquals('ComponentCount',1,C.ComponentCount);
    AssertSame('ComponentCount',C2,C.CompProp);
  finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTOwnedComponentReadText;

Var
  C : TOwnedComponent;
  C2 : TComponent;

begin
  TestTOwnedComponentText;
  C:=TOwnedComponent.Create(Nil);
  try
    C2:=C.CompProp;
    C.CompProp:=nil;
    LoadFromTextStream(C);
    AssertEquals('Name','TestTOwnedComponent',C.Name);
    AssertEquals('ComponentCount',1,C.ComponentCount);
    AssertSame('ComponentCount',C2,C.CompProp);
  finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTStreamedOwnedComponentRead;

Var
  C : TStreamedOwnedComponent;

begin
  TestTStreamedOwnedComponent;
  C:=TStreamedOwnedComponent.Create(Nil);
  Try
    C.Sub.Free;
    C.Sub:=Nil;
    LoadFromStream(C);
    AssertEquals('Name','TestTStreamedOwnedComponent',C.Name);
    AssertNotNull('Have sub',C.Sub);
    AssertEquals('Correct class',TIntegerComponent,C.Sub.ClassType);
    AssertEquals('Name','Sub',C.Sub.Name);
    AssertEquals('Name',3,C.Sub.IntProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTStreamedOwnedComponentReadText;

Var
  C : TStreamedOwnedComponent;

begin
  TestTStreamedOwnedComponentText;
  C:=TStreamedOwnedComponent.Create(Nil);
  Try
    C.Sub.Free;
    C.Sub:=Nil;
    LoadFromTextStream(C);
    AssertEquals('Name','TestTStreamedOwnedComponent',C.Name);
    AssertNotNull('Have sub',C.Sub);
    AssertEquals('Correct class',TIntegerComponent,C.Sub.ClassType);
    AssertEquals('Name','Sub',C.Sub.Name);
    AssertEquals('Name',3,C.Sub.IntProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTStreamedOwnedComponentsRead;

Var
  C : TStreamedOwnedComponents;

begin
  TestTStreamedOwnedComponents;
  C:=TStreamedOwnedComponents.Create(Nil);
  Try
    C.SubA.Free;
    C.SubA:=Nil;
    C.SubB.Free;
    C.SubB:=Nil;
    LoadFromStream(C);
    AssertEquals('Name','TestTStreamedOwnedComponents',C.Name);
    AssertNotNull('Have sub A',C.SubA);
    AssertEquals('Correct sub A class',TIntegerComponent,C.SubA.ClassType);
    AssertEquals('Name','SubA',C.SubA.Name);
    AssertEquals('Name',3,C.SubA.IntProp);
    AssertNotNull('Have sub B',C.SubB);
    AssertEquals('Correct sub B class',TStringComponent,C.SubB.ClassType);
    AssertEquals('Name','SubB',C.SubB.Name);
    AssertEquals('Name','A string',C.SubB.StringProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTStreamedOwnedComponentsReadText;

Var
  C : TStreamedOwnedComponents;

begin
  TestTStreamedOwnedComponentsText;
  C:=TStreamedOwnedComponents.Create(Nil);
  Try
    C.SubA.Free;
    C.SubA:=Nil;
    C.SubB.Free;
    C.SubB:=Nil;
    LoadFromTextStream(C);
    AssertEquals('Name','TestTStreamedOwnedComponents',C.Name);
    AssertNotNull('Have sub A',C.SubA);
    AssertEquals('Correct sub A class',TIntegerComponent,C.SubA.ClassType);
    AssertEquals('Name','SubA',C.SubA.Name);
    AssertEquals('Name',3,C.SubA.IntProp);
    AssertNotNull('Have sub B',C.SubB);
    AssertEquals('Correct sub B class',TStringComponent,C.SubB.ClassType);
    AssertEquals('Name','SubB',C.SubB.Name);
    AssertEquals('Name','A string',C.SubB.StringProp);
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTIntegerComponent2;

Var
  C : TComponent;

begin
  C:=TIntegerComponent2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TIntegerComponent2');
    ExpectBareString('TestTIntegerComponent2');
    ExpectBareString('IntProp');
    ExpectInteger(1024);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTIntegerComponent2Text;

Const
 SData3 =
   'object TestTIntegerComponent2: TIntegerComponent2'+sLineBreak+
   '  IntProp = 1024'+sLineBreak+
   'end'+sLineBreak;

begin
  TestTIntegerComponent2;
  CheckAsString(SData3);
end;


procedure TTestComponentStream.TestTIntegerComponent3;

Var
  C : TComponent;

begin
  C:=TIntegerComponent3.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TIntegerComponent3');
    ExpectBareString('TestTIntegerComponent3');
    ExpectBareString('IntProp');
    ExpectInteger(262144);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTIntegerComponent3Text;

Const
 SData4 =
   'object TestTIntegerComponent3: TIntegerComponent3'+sLineBreak+
   '  IntProp = 262144'+sLineBreak+
   'end'+sLineBreak;

begin
  TestTIntegerComponent3;
  CheckAsString(SData4);
end;


procedure TTestComponentStream.TestTIntegerComponent4;

Var
  C : TComponent;

begin
  C:=TIntegerComponent4.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TIntegerComponent4');
    ExpectBareString('TestTIntegerComponent4');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


procedure TTestComponentStream.TestTIntegerComponent5;

Var
  C : TComponent;

begin
  C:=TIntegerComponent5.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TIntegerComponent5');
    ExpectBareString('TestTIntegerComponent5');
    ExpectBareString('IntProp');
    ExpectInteger(5);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


procedure TTestComponentStream.TestTInt64Component;

Var
  C : TComponent;

begin
  C:=TInt64Component.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TInt64Component');
    ExpectBareString('TestTInt64Component');
    ExpectBareString('Int64Prop');
    ExpectInteger(4);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTInt64ComponentText;

Const
 SData5 =
   'object TestTInt64Component: TInt64Component'+sLineBreak+
   '  Int64Prop = 4'+sLineBreak+
   'end'+sLineBreak;

begin
  TestTInt64Component;
  CheckAsString(SData5);
end;


procedure TTestComponentStream.TestTInt64Component2;

Var
  C : TComponent;

begin
  C:=TInt64Component2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TInt64Component2');
    ExpectBareString('TestTInt64Component2');
    ExpectBareString('Int64Prop');
    ExpectInteger(1024);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTInt64Component2Text;

Const
 SData6 =
   'object TestTInt64Component2: TInt64Component2'+sLineBreak+
   '  Int64Prop = 1024'+sLineBreak+
   'end'+sLineBreak;

begin
  TestTInt64Component2;
  CheckAsString(SData6);
end;


procedure TTestComponentStream.TestTInt64Component3;

Var
  C : TComponent;

begin
  C:=TInt64Component3.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TInt64Component3');
    ExpectBareString('TestTInt64Component3');
    ExpectBareString('Int64Prop');
    ExpectInteger(262144);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTInt64Component3Text;

Const
 SData7 =
   'object TestTInt64Component3: TInt64Component3'+sLineBreak+
   '  Int64Prop = 262144'+sLineBreak+
   'end'+sLineBreak;

begin
  TestTInt64Component3;
  CheckAsString(SData7);
end;


procedure TTestComponentStream.TestTInt64Component4;

Var
  C : TComponent;

begin
  C:=TInt64Component4.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TInt64Component4');
    ExpectBareString('TestTInt64Component4');
    ExpectBareString('Int64Prop');
    ExpectInt64(2147516415{     2147745791});
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTInt64Component4Text;

Const
 SData8 =
   'object TestTInt64Component4: TInt64Component4'+sLineBreak+
   '  Int64Prop = 2147516415'+sLineBreak+
   'end'+sLineBreak;

begin
  TestTInt64Component4;
  CheckAsString(SData8);
end;


procedure TTestComponentStream.TestTInt64Component5;

Var
  C : TComponent;

begin
  C:=TInt64Component5.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TInt64Component5');
    ExpectBareString('TestTInt64Component5');
//    ExpectBareString('Int64Prop');
//    ExpectInteger(7);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


procedure TTestComponentStream.TestTInt64Component6;

Var
  C : TComponent;

begin
  C:=TInt64Component6.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TInt64Component6');
    ExpectBareString('TestTInt64Component6');
    ExpectBareString('Int64Prop');
    ExpectInteger(8);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTCharComponent;

Var
  C : TComponent;

begin
  C:=TCharComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TCharComponent');
    ExpectBareString('TestTCharComponent');
    ExpectBareString('CharProp');
    ExpectString(#10);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTCharComponentText;

Const
  SData111 =
     'object TestTCharComponent: TCharComponent'+sLineBreak+
     '  CharProp = #10'+sLineBreak+
     'end'+sLineBreak;


begin
  TestTCharComponent;
  CheckAsString(SData111);
end;


procedure TTestComponentStream.TestTStringComponent;

Var
  C : TComponent;

begin
  C:=TStringComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TStringComponent');
    ExpectBareString('TestTStringComponent');
    ExpectBareString('StringProp');
    ExpectString('A string');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTStringComponentText;

Const
  SData9 =
     'object TestTStringComponent: TStringComponent'+sLineBreak+
     '  StringProp = ''A string'''+sLineBreak+
     'end'+sLineBreak;

begin
  TestTStringComponent;
  CheckAsString(SData9);
end;


procedure TTestComponentStream.TestTStringComponent2;

Var
  C : TComponent;

begin
  C:=TStringComponent2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TStringComponent2');
    ExpectBareString('TestTStringComponent2');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTStringComponent3;

Var
  C : TComponent;

begin
  C:=TStringComponent3.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TStringComponent3');
    ExpectBareString('TestTStringComponent3');
    ExpectBareString('StringProp');
    ExpectString('A ''quoted'' string');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTStringComponent4;
Var
  C : TComponent;

begin
  C:=TStringComponent3.Create(Nil);
  Try
    TStringComponent3(C).StringProp:='A '#10' whitespace string';
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TStringComponent3');
    ExpectBareString('TestTStringComponent3');
    ExpectBareString('StringProp');
    ExpectString('A '#10' whitespace string');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTStringComponent3Text;
Const
  SData10 =
     'object TestTStringComponent3: TStringComponent3'+sLineBreak+
     '  StringProp = ''A ''''quoted'''' string'''+sLineBreak+
     'end'+sLineBreak;

begin
  TestTStringComponent3;
  CheckAsString(SData10);
end;

procedure TTestComponentStream.TestTStringComponent4Text;
Const
  SData101 =
     'object TestTStringComponent3: TStringComponent3'+sLineBreak+
     '  StringProp = ''A ''#10'' whitespace string'''+sLineBreak+
     'end'+sLineBreak;

begin
  TestTStringComponent4;
  CheckAsString(SData101);
end;


procedure TTestComponentStream.TestTWideStringComponent;

Var
  C : TComponent;

begin
  C:=TWideStringComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TWideStringComponent');
    ExpectBareString('TestTWideStringComponent');
    ExpectBareString('WideStringProp');
    ExpectString('Some WideString');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTWideStringComponentText;
Const
  SData11 =
     'object TestTWideStringComponent: TWideStringComponent'+sLineBreak+
     '  WideStringProp = ''Some WideString'''+sLineBreak+
     'end'+sLineBreak;

begin
  TestTWideStringComponent;
  CheckAsString(SData11);
end;


procedure TTestComponentStream.TestTWideStringComponent2;

Var
  C : TComponent;

begin
  C:=TWideStringComponent2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TWideStringComponent2');
    ExpectBareString('TestTWideStringComponent2');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


procedure TTestComponentStream.TestTSingleComponent;

Var
  C : TComponent;

begin
  C:=TSingleComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TSingleComponent');
    ExpectBareString('TestTSingleComponent');
    ExpectBareString('SingleProp');
    ExpectExtended(1.23);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


procedure TTestComponentStream.TestTDoubleComponent;

Var
  C : TComponent;

begin
  C:=TDoubleComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TDoubleComponent');
    ExpectBareString('TestTDoubleComponent');
    ExpectBareString('DoubleProp');
    ExpectExtended(2.34);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTDoubleComponentText;

Const
 SData12 =
   'object TestTDoubleComponent: TDoubleComponent'+sLineBreak+
   '  DoubleProp =  2.3399999999999999E+000'+sLineBreak+
   'end'+sLineBreak;

begin
  TestTDoubleComponent;
  CheckAsString(SData12);
end;


procedure TTestComponentStream.TestTExtendedComponent;

Var
  C : TComponent;

begin
  C:=TExtendedComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TExtendedComponent');
    ExpectBareString('TestTExtendedComponent');
    ExpectBareString('ExtendedProp');
    ExpectExtended(3.45);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


(*
Procedure TTestComponentStream.TestTCompComponent;

Var
  C : TComponent;

begin
  C:=TCompComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TCompComponent');
    ExpectBareString('TestTCompComponent');
    ExpectBareString('ExtendedProp');
    ExpectExtended(5.00);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;
*)

procedure TTestComponentStream.TestTCurrencyComponent;

Var
  C : TComponent;

begin
  C:=TCurrencyComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TCurrencyComponent');
    ExpectBareString('TestTCurrencyComponent');
    ExpectBareString('CurrencyProp');
    ExpectInteger(56700);
// Natively, this is:
//    ExpectExtended(5.67);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTCurrencyComponentText;
Const
 SData13 =
   'object TestTCurrencyComponent: TCurrencyComponent'+sLineBreak+
   '  CurrencyProp = 56700'+sLineBreak+
   'end'+sLineBreak;

begin
  TestTCurrencyComponent;
  CheckAsString(SData13);
end;


procedure TTestComponentStream.TestTDateTimeComponent;

Var
  C : TComponent;

begin
  C:=TDateTimeComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TDateTimeComponent');
    ExpectBareString('TestTDateTimeComponent');
    ExpectBareString('DateTimeProp');
    ExpectExtended(35278.00);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


procedure TTestComponentStream.TestTDateTimeComponent2;

Var
  C : TComponent;

begin
  C:=TDateTimeComponent2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TDateTimeComponent2');
    ExpectBareString('TestTDateTimeComponent2');
    ExpectBareString('DateTimeProp');
    ExpectExtended(0.97);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


procedure TTestComponentStream.TestTDateTimeComponent3;

Var
  C : TComponent;

begin
  C:=TDateTimeComponent3.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TDateTimeComponent3');
    ExpectBareString('TestTDateTimeComponent3');
    ExpectBareString('DateTimeProp');
    ExpectExtended(35278.97);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


procedure TTestComponentStream.TestTEnumComponent;

Var
  C : TComponent;

begin
  C:=TEnumComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TEnumComponent');
    ExpectBareString('TestTEnumComponent');
    ExpectBareString('Dice');
    ExpectIdent('four');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTEnumComponentText;

Const
  SData14 =
     'object TestTEnumComponent: TEnumComponent'+sLineBreak+
     '  Dice = four'+sLineBreak+
     'end'+sLineBreak;

begin
  TestTEnumComponent;
  CheckAsString(SData14);
end;


procedure TTestComponentStream.TestTEnumComponent2;

Var
  C : TComponent;

begin
  C:=TEnumComponent2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TEnumComponent2');
    ExpectBareString('TestTEnumComponent2');
{$ifndef FPC}
    // FPC does not stream an undeclared default value, it assumes the
    // 0-the value is the default.
    ExpectBareString('Dice');
    ExpectIdent('one');
{$endif FPC}
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


procedure TTestComponentStream.TestTEnumComponent3;

Var
  C : TComponent;

begin
  C:=TEnumComponent3.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TEnumComponent3');
    ExpectBareString('TestTEnumComponent3');
    ExpectBareString('Dice');
    ExpectIdent('three');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


procedure TTestComponentStream.TestTEnumComponent4;

Var
  C : TComponent;

begin
  C:=TEnumComponent4.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TEnumComponent4');
    ExpectBareString('TestTEnumComponent4');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTEnumComponent5;

Var
  C : TComponent;

begin
  C:=TEnumComponent5.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TEnumComponent5');
    ExpectBareString('TestTEnumComponent5');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


procedure TTestComponentStream.TestTSetComponent;

Var
  C : TComponent;

begin
  C:=TSetComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TSetComponent');
    ExpectBareString('TestTSetComponent');
    ExpectBareString('Throw');
    ExpectValue(vaSet);
    ExpectBareString('two');
    ExpectBareString('five');
    ExpectBareString('');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTSetComponentText;
Const
  SData15 =
     'object TestTSetComponent: TSetComponent'+sLineBreak+
     '  Throw = [two, five]'+sLineBreak+
     'end'+sLineBreak;

begin
  TestTSetComponent;
  CheckAsString(SData15);
end;


procedure TTestComponentStream.TestTSetComponent2;

Var
  C : TComponent;

begin
  C:=TSetComponent2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TSetComponent2');
    ExpectBareString('TestTSetComponent2');
{$ifdef delphi}
    // Same as for sets: a set with undeclared default is regarded as
    // A set with default [], and is not streamed if it is empty.
    ExpectBareString('Throw');
    ExpectValue(vaSet);
    ExpectBareString('');
{$endif delphi}
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


procedure TTestComponentStream.TestTSetComponent3;

Var
  C : TComponent;

begin
  C:=TSetComponent3.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TSetComponent3');
    ExpectBareString('TestTSetComponent3');
    ExpectBareString('Throw');
    ExpectValue(vaSet);
    ExpectBareString('one');
    ExpectBareString('four');
    ExpectBareString('');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


procedure TTestComponentStream.TestTSetComponent4;

Var
  C : TComponent;

begin
  // Writeln('Start test');
  C:=TSetComponent4.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TSetComponent4');
    ExpectBareString('TestTSetComponent4');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


procedure TTestComponentStream.TestTMultipleComponent;

Var
  C : TComponent;

begin
  C:=TMultipleComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TMultipleComponent');
    ExpectBareString('TestTMultipleComponent');
    ExpectBareString('IntProp');
    ExpectInteger(1);
    ExpectBareString('StringProp');
    ExpectString('A String');
    ExpectBareString('CurrencyProp');
    ExpectInteger(23000);
//    ExpectExtended(2.30);
    ExpectBareString('Dice');
    ExpectIdent('two');
    ExpectBareString('Throw');
    ExpectValue(vaSet);
    ExpectBareString('three');
    ExpectBareString('four');
    ExpectBareString('');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTMultipleComponentText;
Const
  SData16 =
     'object TestTMultipleComponent: TMultipleComponent'+sLineBreak+
     '  IntProp = 1'+sLineBreak+
     '  StringProp = ''A String'''+sLineBreak+
     '  CurrencyProp = 23000'+sLineBreak+
     '  Dice = two'+sLineBreak+
     '  Throw = [three, four]'+sLineBreak+
     'end'+sLineBreak;

begin
  TestTMultipleComponent;
  CheckAsString(SData16);
end;


procedure TTestComponentStream.TestTPersistentComponent;

Var
  C : TComponent;

begin
  C:=TPersistentComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TPersistentComponent');
    ExpectBareString('TestTPersistentComponent');
    ExpectBareString('Persist.AInteger');
    ExpectInteger(3);
    ExpectBareString('Persist.AString');
    ExpectString('A persistent string');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTPersistentComponentText;

Const
  SData17 =
     'object TestTPersistentComponent: TPersistentComponent'+sLineBreak+
     '  Persist.AInteger = 3'+sLineBreak+
     '  Persist.AString = ''A persistent string'''+sLineBreak+
     'end'+sLineBreak;

begin
  TestTPersistentComponent;
  CheckAsString(SData17);
end;


procedure TTestComponentStream.TestTCollectionComponent;

Var
  C : TComponent;

begin
  C:=TCollectionComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TCollectionComponent');
    ExpectBareString('TestTCollectionComponent');
    ExpectBareString('Coll');
    ExpectValue(vaCollection);
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTCollectionComponentText;
Const
  SData18 =
     'object TestTCollectionComponent: TCollectionComponent'+sLineBreak+
     '  Coll = <>'+sLineBreak+
     'end'+sLineBreak;

begin
  TestTCollectionComponent;
  CheckAsString(SData18);
end;


procedure TTestComponentStream.TestTCollectionComponent2;

Var
  C : TComponent;

begin
  C:=TCollectionComponent2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TCollectionComponent2');
    ExpectBareString('TestTCollectionComponent2');
    ExpectBareString('Coll');
    ExpectValue(vaCollection);
    ExpectValue(vaList);
    ExpectBareString('StrProp');
    ExpectString('First');
    ExpectEndOfList;
    ExpectValue(vaList);
    ExpectBareString('StrProp');
    ExpectString('Second');
    ExpectEndOfList;
    ExpectValue(vaList);
    ExpectBareString('StrProp');
    ExpectString('Third');
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTCollectionComponent2Text;
Const
  SData19 =
     'object TestTCollectionComponent2: TCollectionComponent2'+sLineBreak+
     '  Coll = <  '+sLineBreak+
     '    item'+sLineBreak+
     '      StrProp = ''First'''+sLineBreak+
     '    end  '+sLineBreak+
     '    item'+sLineBreak+
     '      StrProp = ''Second'''+sLineBreak+
     '    end  '+sLineBreak+
     '    item'+sLineBreak+
     '      StrProp = ''Third'''+sLineBreak+
     '    end>'+sLineBreak+
     'end'+sLineBreak;

begin
  TestTCollectionComponent2;
  CheckAsString(SData19);
end;


procedure TTestComponentStream.TestTCollectionComponent3;

Var
  C : TComponent;

begin
  C:=TCollectionComponent3.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TCollectionComponent3');
    ExpectBareString('TestTCollectionComponent3');
    ExpectBareString('Coll');
    ExpectValue(vaCollection);
    ExpectValue(vaList);
    ExpectBareString('StrProp');
    ExpectString('First');
    ExpectEndOfList;
    ExpectValue(vaList);
    ExpectEndOfList;
    ExpectValue(vaList);
    ExpectBareString('StrProp');
    ExpectString('Third');
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


procedure TTestComponentStream.TestTCollectionComponent4;

Var
  C : TComponent;

begin
  C:=TCollectionComponent4.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TCollectionComponent4');
    ExpectBareString('TestTCollectionComponent4');
    ExpectBareString('Coll');
    ExpectValue(vaCollection);
    ExpectValue(vaList);
    ExpectBareString('StrProp');
    ExpectString('Something');
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTCollectionComponent5;

Var
  C : TComponent;

begin
  C:=TCollectionComponent5.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TCollectionComponent5');
    ExpectBareString('TestTCollectionComponent5');
    ExpectBareString('Coll');
    ExpectValue(vaCollection);
    ExpectValue(vaList);
    ExpectBareString('StrProp1');
    ExpectString('Something');
    ExpectBareString('StrProp2');
    ExpectString('Otherthing');
    ExpectEndOfList;
    ExpectValue(vaList);
    ExpectBareString('StrProp1');
    ExpectString('Something 2');
    ExpectBareString('StrProp2');
    ExpectString('Otherthing 2');
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;


procedure TTestComponentStream.TestTOwnedComponent;

Var
  C : TComponent;

begin
  C:=TOwnedComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TOwnedComponent');
    ExpectBareString('TestTOwnedComponent');
    ExpectBareString('CompProp');
    ExpectIdent('SubComponent');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTOwnedComponentText;

Const
  SData20 =
     'object TestTOwnedComponent: TOwnedComponent'+sLineBreak+
     '  CompProp = SubComponent'+sLineBreak+
     'end'+sLineBreak;

begin
  TestTOwnedComponent;
  CheckAsString(SData20);
end;


procedure TTestComponentStream.TestTStreamedOwnedComponent;

Var
  C : TComponent;

begin
  C:=TStreamedOwnedComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TStreamedOwnedComponent');
    ExpectBareString('TestTStreamedOwnedComponent');
    ExpectEndOfList;
    ExpectFlags([],0);
    ExpectBareString('TIntegerComponent');
    ExpectBareString('Sub');
    ExpectBareString('IntProp');
    ExpectInteger(3);
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfStream;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTStreamedOwnedComponentText;
Const
  SData21 =
     'object TestTStreamedOwnedComponent: TStreamedOwnedComponent'+sLineBreak+
     '  object Sub: TIntegerComponent'+sLineBreak+
     '    IntProp = 3'+sLineBreak+
     '  end'+sLineBreak+
     'end'+sLineBreak;

begin
  TestTStreamedOwnedComponent;
  CheckAsString(SData21);
end;

procedure TTestComponentStream.TestTStreamedOwnedComponents;

Var
  C : TComponent;

begin
  C:=TStreamedOwnedComponents.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TStreamedOwnedComponents');
    ExpectBareString('TestTStreamedOwnedComponents');
    ExpectEndOfList;
    ExpectFlags([],0);
    ExpectBareString('TIntegerComponent');
    ExpectBareString('SubA');
    ExpectBareString('IntProp');
    ExpectInteger(3);
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectFlags([],0);
    ExpectBareString('TStringComponent');
    ExpectBareString('SubB');
    ExpectBareString('StringProp');
    ExpectString('A string');
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfStream;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTStreamedOwnedComponentsText;
Const
  SData22 =
     'object TestTStreamedOwnedComponents: TStreamedOwnedComponents'+sLineBreak+
     '  object SubA: TIntegerComponent'+sLineBreak+
     '    IntProp = 3'+sLineBreak+
     '  end'+sLineBreak+
     '  object SubB: TStringComponent'+sLineBreak+
     '    StringProp = ''A string'''+sLineBreak+
     '  end'+sLineBreak+
     'end'+sLineBreak;

begin
  TestTStreamedOwnedComponents;
  CheckAsString(SData22);
end;


procedure TTestComponentStream.TestTMethodComponent;

Var
  C : TComponent;

begin
  C:=TMethodComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TMethodComponent');
    ExpectBareString('TestTMethodComponent');
    ExpectBareString('MethodProp');
    ExpectIdent('MyMethod');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTMethodComponentText;

Const
  SData23 =
     'object TestTMethodComponent: TMethodComponent'+sLineBreak+
     '  MethodProp = MyMethod'+sLineBreak+
     'end'+sLineBreak;

begin
  TestTMethodComponent;
  CheckAsString(SData23);
end;


procedure TTestComponentStream.TestTMethodComponent2;

Var
  C : TComponent;

begin
  C:=TMethodComponent2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TMethodComponent2');
    ExpectBareString('TestTMethodComponent2');
    ExpectEndOfList;
    ExpectFlags([],0);
    ExpectBareString('TMethodComponent');
    ExpectBareString('AComponent');
    ExpectBareString('MethodProp');
    ExpectIdent('MyMethod2');
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
  end;
end;

procedure TTestComponentStream.TestTMethodComponent2Text;
Const
  SData24 =
     'object TestTMethodComponent2: TMethodComponent2'+sLineBreak+
     '  object AComponent: TMethodComponent'+sLineBreak+
     '    MethodProp = MyMethod2'+sLineBreak+
     '  end'+sLineBreak+
     'end'+sLineBreak;

begin
  TestTMethodComponent2;
  CheckAsString(SData24);
end;



Procedure TTestCollectionStream.CompareColl(CA,CB : TMyColl);

Var
  I : Integer;

begin
  AssertEquals('Counts differ: %d %d',CA.Count,CB.Count);
  For I:=0 to CA.Count-1 do
    begin
    AssertEquals(Format('Nr property of element %d equals',[I]),CA[i].Nr,CB[i].Nr);
    AssertEquals(Format('Str property of element %d equals',[I]),CA[i].Str,CB[i].Str);
    end;
end;

Function TTestCollectionStream.EmptyComp : TCollComp;

begin
  Result:=TCollComp.Create(Nil);
end;

Function TTestCollectionStream.CreateColl(Anr : Integer) : TCollComp;

Var
  I : Integer;
  T : TMyItem;

begin
  Result:=EmptyComp;
  Result.Name:='C'+IntToStr(Anr);
  For I:=0 to ANr-1 do
    begin
    T:=Result.MyColl.Add as TMyItem;
    T.Nr:=I; // not I+1, so the default value gets tested too
    T.Str:=IntToStr(I+1);
    end;
end;

Procedure TTestCollectionStream.TestEmpty;

Var
 CA,CB : TCollComp;

begin
  CA:=CreateColl(0);
  try
    CB:=EmptyComp;
    Try
      CB.FromStream(CA.ToStream);
      CompareColl(CA.MyColl,CB.MyColl);
    Finally
      CB.Free;
    end;
  Finally
    CA.Free;
  end;
end;

Procedure TTestCollectionStream.TestNr(ACount : Integer);

Var
 CA,CB : TCollComp;

begin
  CA:=CreateColl(ACount);
  try
    CB:=EmptyComp;
    Try
      CB.FromStream(CA.ToStream);
      CompareColl(CA.MyColl,CB.MyColl);
    Finally
      CB.Free;
    end;
  Finally
    CA.Free;
  end;
end;

Procedure TTestCollectionStream.TestClear;

Var
 CA,CB : TCollComp;

begin
  CA:=CreateColl(3);
  try
    CB:=CreateColl(1);
    CB.Name:='';
    Try
      // CB collection should be cleared before loading.
      CB.FromStream(CA.ToStream);
      CompareColl(CA.MyColl,CB.MyColl);
    Finally
      CB.Free;
    end;
  Finally
    CA.Free;
  end;
end;

Procedure TTestCollectionStream.Test1;

begin
  TestNr(1);
end;

Procedure TTestCollectionStream.Test2;

begin
  TestNr(2);
end;

Procedure TTestCollectionStream.Test3;

begin
  TestNr(3);
end;

begin
  RegisterTests([TTestComponentStream{,TTestCollectionStream}]);
end.
