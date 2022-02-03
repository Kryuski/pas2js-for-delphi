unit tcstringhelp;

{$mode objfpc}{$H+}
//{$modeswitch advancedrecords}
//{$modeswitch typehelpers}

interface

uses
   SysUtils, fpcunit, testregistry;

Const
  TBI = 'To be implemented';

Type
  TTestStringHelper = Class(TTestCase)
  Public
    Function TestArray(Msg : string; Aexpected : Array of string; AActual : TStringArray) : boolean;
  Published
    Procedure TestCompare;
    Procedure TestCompareOrdinal;
    Procedure TestCompareText;
    Procedure TestCopy;
    Procedure TestCreate;
    Procedure TestEndsText;
    Procedure TestEquals;
    Procedure TestFormat;
    Procedure TestIsNullOrEmpty;
    Procedure TestIsNullOrWhiteSpace;
    Procedure TestJoin;
    Procedure TestLowerCase;
    Procedure TestParse;
    Procedure TestToBoolean;
    Procedure TestToDouble;
    Procedure TestToExtended;
    Procedure TestToInt64;
    Procedure TestToInteger;
    Procedure TestUppercase;
    Procedure TestCompareTo;
    Procedure TestContains;
    Procedure TestCountChar;
    Procedure TestDeQuotedString;
    Procedure TestEndsWith;
    Procedure TestGetHashCode;
    Procedure TestIndexOf;
    Procedure TestIndexOfAny;
    Procedure TestIndexOfAnyString;
    Procedure TestIndexOfUnquoted;
    Procedure TestIndexOfAnyUnquoted;
    Procedure TestInsert;
    Procedure TestIsDelimiter;
    Procedure TestIsEmpty;
    Procedure TestLastDelimiter;
    Procedure TestLastIndexOf;
    Procedure TestLastIndexOfString;
    Procedure TestLastIndexOfAny;
    Procedure TestPadLeft;
    Procedure TestPadRight;
    Procedure TestQuotedString;
    Procedure TestRemove;
    Procedure TestReplace;
    Procedure TestSplit;
    Procedure TestSplitString;
    Procedure TestStartsWith;
    Procedure TestSubstring;
    Procedure TestToCharArray;
    Procedure TestToLower;
    Procedure TestToLowerInvariant;
    Procedure TestToUpper;
    Procedure TestToUpperInvariant;
    Procedure TestTrim;
    Procedure TestTrimLeft;
    Procedure TestTrimRight;
    Procedure TestTrimEnd;
    Procedure TestTrimStart;
    Procedure TestChars;
    Procedure TestLength;
  end;

implementation

Procedure TTestStringHelper.TestCompare;

Var
  r : Integer;

begin
  // Simple cases
  R:=String.Compare('A','B');
  AssertTrue('1. Simple Compare strings (A,B) :'+IntToStr(R)+'<0',R<0);
  R:=String.Compare('B','A');
  AssertTrue('2. Simple Compare strings (B,A) :'+IntToStr(R)+'>0',R>0);
  R:=String.Compare('A','A');
  AssertTrue('3. Simple Compare strings (A,A) :'+IntToStr(R)+'=0',R=0);
  R:=String.Compare('A','a', true);
  AssertTrue('4. Simple ignore case Compare strings (A,a) : '+IntToStr(R)+'=0',R=0);
  R:=String.Compare('b','a',True);
  AssertTrue('5. Simple ignore case Compare strings (b,a) : '+IntToStr(R)+'>0',R>0);
  R:=String.Compare('A','a',[coIgnoreCase]);
  AssertTrue('6. [coIgnoreCase] Compare strings (A,a) : '+IntToStr(R)+'=0',R=0);
  R:=String.Compare('b','a',[coIgnoreCase]);
  AssertTrue('7. [coIgnoreCase] Compare strings (b,a) : '+IntToStr(R)+'>0',R>0);
  // Check whether index is observed.
  R:=String.Compare('AA',1,'AB',1,1);
  AssertTrue('8. Compare(''AA'',1,''AB'',1,1) :'+IntToStr(R)+'<0',R<0);
  R:=String.Compare('AB',1,'AA',1,1);
  AssertTrue('9. Compare(''AB'',1,''AA'',1,1) :'+IntToStr(R)+'>0',R>0);
  R:=String.Compare('AA',1,'AA',1,1);
  AssertTrue('10. Compare(''AA'',1,''AA'',1,1) :'+IntToStr(R)+'=0',R=0);
  // Make sure only len chars are used.
  R:=String.Compare('AAC',1,'ABD',1,1);
  AssertTrue('11. Compare(''AAC'',1,''ABD'',1,1) :'+IntToStr(R)+'<0',R<0);
  R:=String.Compare('ABC',1,'AAD',1,1);
  AssertTrue('12 Compare(''ABC'',1,''AAD'',1,1) :'+IntToStr(R)+'>0',R>0);
  R:=String.Compare('AAC',1,'AAD',1,1);
  AssertTrue('13. Compare(''AAC'',1,''AAD'',1,1) :'+IntToStr(R)+'=0',R=0);
  // Index, case insensitive
  R:=String.Compare('AA',1,'Aa',1,1,true);
  AssertTrue('14. Compare(''AA'',1,''Aa'',1,1,true) : '+IntToStr(R)+'=0',R=0);
  R:=String.Compare('Ab',1,'Aa',1,1,True);
  AssertTrue('15. Compare(''Ab'',1,''Aa'',1,1,True) : '+IntToStr(R)+'>0',R>0);
  R:=String.Compare('A',1,'a',1,1,[coIgnoreCase]);
  AssertTrue('16. Compare(''A'',1,''a'',1,1,[coIgnoreCase]) : '+IntToStr(R)+'=0',R=0);
  // Index, maxlen, case insensitive
  R:=String.Compare('AAC',1,'AaD',1,1,true);
  AssertTrue('17. Compare(''AAC'',1,''AaD'',1,1,true) : '+IntToStr(R)+'=0',R=0);
  R:=String.Compare('AbC',1,'AaD',1,1,True);
  AssertTrue('18. Compare(''AbC'',1,''AaD'',1,1,True) : '+IntToStr(R)+'>0',R>0);
  R:=String.Compare('AAC',1,'AaD',1,1,[coIgnoreCase]);
  AssertTrue('19. Compare(''AAC'',1,''AaD'',1,1,[coIgnoreCase]) : '+IntToStr(R)+'=0',R=0);
end;

Procedure TTestStringHelper.TestCompareOrdinal;

Var
  r : Integer;

begin
  // Simple
  R:=String.CompareOrdinal('A','B');
  AssertTrue('1. Simple Compare strings (A,B) :'+IntToStr(R)+'<0',R<0);
  R:=String.CompareOrdinal('B','A');
  AssertTrue('2. Simple Compare strings (B,A) :'+IntToStr(R)+'>0',R>0);
  R:=String.CompareOrdinal('A','A');
  AssertTrue('3. Simple Compare strings (A,A) :'+IntToStr(R)+'=0',R=0);
  // Index
  R:=String.CompareOrdinal('AA',1,'AB',1,1);
  AssertTrue('4. Simple Compare strings (AA,1,AB,1,1) :'+IntToStr(R)+'<0',R<0);
  R:=String.CompareOrdinal('AB',1,'AA',1,1);
  AssertTrue('5. Simple Compare strings (AB,1,AA,1,1) :'+IntToStr(R)+'>0',R>0);
  R:=String.CompareOrdinal('AA',1,'AA',1,1);
  AssertTrue('6. Simple Compare strings (AA,1,AA,1,1) :'+IntToStr(R)+'=0',R=0);
  // Index, maxlen
  R:=String.CompareOrdinal('AAC',1,'ABD',1,1);
  AssertTrue('7. Simple Compare strings (AAC,1,ABD,1,1) :'+IntToStr(R)+'<0',R<0);
  R:=String.CompareOrdinal('ABC',1,'AAD',1,1);
  AssertTrue('8. Simple Compare strings (ABC,1,AAD,1,1) :'+IntToStr(R)+'>0',R>0);
  R:=String.CompareOrdinal('AAD',1,'AAD',1,1);
  AssertTrue('9. Simple Compare strings (AAC,1,AAD,1,1) :'+IntToStr(R)+'=0',R=0);
end;

Procedure TTestStringHelper.TestCompareText;

Var
  r : Integer;

begin
  R:=String.CompareText('A','B');
  AssertTrue('1. Simple Compare strings (A,B) :'+IntToStr(R)+'<0',R<0);
  R:=String.CompareText('B','A');
  AssertTrue('Simple Compare strings (B,A) :'+IntToStr(R)+'>0',R>0);
  R:=String.CompareText('A','A');
  AssertTrue('Simple Compare strings (A,A) :'+IntToStr(R)+'=0',R=0);
  //
  R:=String.CompareText('A','b');
  AssertTrue('Simple Compare strings (A,b) :'+IntToStr(R)+'<0',R<0);
  R:=String.CompareText('B','a');
  AssertTrue('Simple Compare strings (B,a) :'+IntToStr(R)+'>0',R>0);
  R:=String.CompareText('A','a');
  AssertTrue('Simple Compare strings (A,a) :'+IntToStr(R)+'=0',R=0);
end;

Procedure TTestStringHelper.TestCopy;

var
  A,S : String;

begin
  A:=TBI;
  S:=String.Copy(A);
  AssertEquals('Copy creates equal copy',A,S);
end;

Procedure TTestStringHelper.TestCreate;

Var
  A : String;

begin
  A:=String.Create('*',5);
  AssertEquals('Create with char and length','*****',A);
  A:=String.Create(['a','b','c','d','e']);
  AssertEquals('Create with array of char','abcde',A);
  A:=String.Create(['a','b','c','d','e'],1,3);
  AssertEquals('Create with array of char and index, len','bcd',A);
end;

Procedure TTestStringHelper.TestEndsText;

begin
  AssertTrue('1. EndsText, correct',String.EndsText('be','to be or not to be'));
  AssertTrue('2. EndsText, correct, case insensitive',String.EndsText('BE','to be or not to be'));
  AssertFalse('3. EndsText, not correct',String.EndsText('de','to be or not to be'));
  AssertFalse('4. EndsText, empty',String.EndsText('','to be or not to be'));
end;

Procedure TTestStringHelper.TestEquals;

Var
  A,B : String;

begin
  A:='Yes';
  B:='No';
  AssertFalse('1. Equals(A,B)',String.Equals(A,B));
  B:='Yes';
  AssertTrue('2. Equals(A,B)',String.Equals(A,B));
  B:='No';
  AssertFalse('3. A.Equals(B)',A.Equals(B));
  B:='Yes';
  AssertTrue('4. A.Equals(B)',A.Equals(B));
end;

Procedure TTestStringHelper.TestFormat;


begin
  AssertEquals('1. Format as class function','A1 BC', String.Format('A%d B%s',[1,'C']));
  AssertEquals('2. Format function','A1 BC', 'A%d B%s'.Format([1,'C']));
end;

Procedure TTestStringHelper.TestIsNullOrEmpty;

begin 
  AssertTrue('1. Empty string returns true',String.IsNullOrEmpty(''));
end;

Procedure TTestStringHelper.TestIsNullOrWhiteSpace;

Var
  C : Char;
begin
  AssertTrue('2. Empty string returns true',String.IsNullOrWhitespace(''));
  For C:=#1 to #32 do
    AssertTrue('Char '+IntToStr(Ord(C))+' string is whitespace',String.IsNullOrWhiteSpace(C));
end;

Procedure TTestStringHelper.TestJoin;

begin
  AssertEquals('1 element','ABC',String.Join(' ',['ABC']));
  AssertEquals('2 elements','ABC DEF',String.Join(' ',['ABC','DEF']));
  AssertEquals('3 elements','ABC DEF GHI',String.Join(' ',['ABC','DEF','GHI']));
  AssertEquals('5 elements, index','ABC DEF GHI',String.Join(' ',['NONO','ABC','DEF','GHI','nono'],1,3));
end;

Procedure TTestStringHelper.TestLowerCase;

begin
  AssertEquals('1. Simple Lowercase','abc',String.Lowercase('ABC'));
end;

Procedure TTestStringHelper.TestParse;

Var
  E : Extended;
begin
  E:=12.3;
  AssertEquals('Boolean','-1',String.Parse(True));
  AssertEquals('Integer','12',String.Parse(Integer(12)));
  AssertEquals('Int64','45',String.Parse(NativeInt(45)));
  AssertEquals('Extended',FloatToStr(E),String.Parse(E));
end;

Procedure TTestStringHelper.TestToBoolean;

begin
  AssertTrue('Class function, true',String.ToBoolean('True'));
  AssertTrue('function 1',String.ToBoolean('1'));
  AssertFalse('Class function false',String.ToBoolean('False'));
  AssertFalse('function 0',String.ToBoolean('0'));
end;

Procedure TTestStringHelper.TestToDouble;

begin
  AssertEquals('Class function, 0',0.0,String.ToDouble('0.0'),0.1);
  AssertEquals('Class function, 1.2',1.2,String.ToDouble('1.2'),0.1);
  AssertEquals('function, 0',0.0,'0.0'.ToDouble,0.1);
  AssertEquals('function, 1.2',1.2,'1.2'.ToDouble,0.1);
end;

Procedure TTestStringHelper.TestToExtended;

begin
  AssertEquals('Class function, 0',0.0,String.ToExtended('0.0'),0.1);
  AssertEquals('Class function, 1.2',1.2,String.ToExtended('1.2'),0.1);
  AssertEquals('function, 0',0.0,'0.0'.ToExtended,0.1);
  AssertEquals('function, 1.2',1.2,'1.2'.ToExtended,0.1);
end;

Procedure TTestStringHelper.TestToInt64;

begin
  AssertEquals('Class function, 0',0,String.ToNativeInt('0'));
  AssertEquals('Class function, 12',12,String.ToNativeInt('12'));
  AssertEquals('Class function, 1234567890123',1234567890123,String.ToNativeInt('1234567890123'));
  // 2 characters because it does not work on length 1, compiler assumes Char as in Delphi
  AssertEquals('Class function, 0',0,'00'.ToNativeInt);
  AssertEquals('Class function, 12',12,'12'.ToNativeInt);
  AssertEquals('Class function, 1234567890123',1234567890123,'1234567890123'.ToNativeInt);
end;

Procedure TTestStringHelper.TestToInteger;

begin
  AssertEquals('Class function, 0',0,String.ToInteger('0'));
  AssertEquals('Class function, 12',12,String.ToInteger('12'));
  AssertEquals('Class function, 123456789',123456789,String.ToInteger('123456789'));
  // 2 characters because it does not work on length 1, compiler assumes Char as in Delphi
  AssertEquals('Class function, 0',0,'00'.ToInteger);
  AssertEquals('Class function, 12',12,'12'.ToInteger);
  AssertEquals('Class function, 123456789',123456789,'123456789'.ToInteger);
end;

Procedure TTestStringHelper.TestUppercase;

begin
  AssertEquals('1. Simple Lowercase','ABC',String.UpperCase('abc'));
end;

Procedure TTestStringHelper.TestCompareTo;

begin
  // 2 characters because it does not work on length 1, compiler assumes Char as in Delphi
  AssertTrue('1. A<B','AA'.CompareTo('AB')<0);
  AssertTrue('1. A=A','AA'.CompareTo('AA')=0);
  AssertTrue('1. B>A','AB'.CompareTo('AA')>0);
end;

Procedure TTestStringHelper.TestContains;

begin
  AssertTrue('ABC contains AB','ABC'.Contains('AB'));
  AssertTrue('ABC contains BC','ABC'.Contains('BC'));
  AssertTrue('ABC contains B','ABC'.Contains('B'));
  AssertFalse('ABC does not contain empty','ABC'.Contains(''));
  AssertFalse('ABC does not contain DEF','ABC'.Contains('DEF'));
  AssertFalse('ABC does not contain a','ABC'.Contains('a'));
end;

(*
  // No PChar support

Procedure TTestStringHelper.TestCopyTo;

Type
  TCharArray = Array Of Char;

Const
  Res1 : Array[0..4] of Char = ('T','o',' ','b','e');

Var
  S : String;
  A : TCharArray;
  I : Integer;

begin
  A:=Default(TCharArray);
  S:=TBI;
  SetLength(A,5);
  S.CopyTo(0,A,0,5);
  For I:=0 to 4 do
    AssertEquals('Zero indexes, Char '+IntToStr(i),Res1[I],A[I]);
  S:='AB'+S;
  S.CopyTo(2,A,0,5);
  For I:=0 to 4 do
    AssertEquals('Source index, zero dest index, Char '+IntToStr(i),Res1[I],A[I]);
  SetLength(A,8);
  S.CopyTo(2,A,3,5);
  For I:=0 to 4 do
    AssertEquals('Source index, dest index, Char '+IntToStr(i),Res1[I],A[I+3]);
end;
*)

Procedure TTestStringHelper.TestCountChar;

begin
  AssertEquals('Empty string',0,''.CountChar(' '));
  AssertEquals('Start and end ',2,' ** '.CountChar(' '));
  AssertEquals('Middle',2,'*  *'.CountChar(' '));
end;

Procedure TTestStringHelper.TestDeQuotedString;

Const
  C =  TBI;
  C2 =  'To be ''implemented';
  C3 =  'To be "implemented';

Var
  S : String;

begin
  S:=''''+C+'''';
  AssertEquals('Simple case of '+S,C,S.DequotedString);
  S:=''''+StringReplace(C2,'''','''''',[rfReplaceAll])+'''';
  AssertEquals('Quoted case of '+S,C2,S.DequotedString);
  S:='"'+C+'"';
  AssertEquals('Simple case of '+S,C,S.DequotedString('"'));
  S:='"'+StringReplace(C3,'"','""',[rfReplaceAll])+'"';
  AssertEquals('Quoted case of '+S,C3,S.DequotedString('"'));
end;

Procedure TTestStringHelper.TestEndsWith;

Var
  S : String;

begin
  S:=TBI;
  AssertTrue('length 0', S.EndsWith(''));
  AssertTrue('length 1', S.EndsWith('d'));
  AssertTrue('length 2', S.EndsWith('ed'));
  AssertTrue('equal length (same)', S.EndsWith(S));
  AssertFalse('length+2', S.EndsWith(S+'ed'));
  AssertFalse('Random string', S.EndsWith('erd'));
  AssertTrue('match case ', S.EndsWith('ed',False));
  AssertFalse('match case, no match ', S.EndsWith('eD',False));
  AssertTrue('no match case, match ', S.EndsWith('ED',True));
  AssertFalse('no match case, no match ', S.EndsWith('DED',True));
end;
Procedure TTestStringHelper.TestGetHashCode;
{
Function GetHashCode: Integer;
}

Var
  S : String;

begin
  S:=TBI;
  AssertTrue('Nonzero hashcode',S.GetHashCode<>0);
  // A more meaningful test would be nice...
end;

Procedure TTestStringHelper.TestIndexOf;

Var
  S : String;

begin
  S:=TBI+' To perfection';
  // Char based.
  AssertEquals('Char, Nonexisting returns -1',-1,S.IndexOf('a'));
  AssertEquals('Char, Existing, zero based',0,S.IndexOf('T'));
  AssertEquals('Char, Case sensitive',-1,S.IndexOf('I'));
  AssertEquals('Char, using start index',10,S.IndexOf('e',5));
  AssertEquals('Char, using start index and count, not found',-1,S.IndexOf('e',5,5));
  AssertEquals('Char, using start index and count,found',10,S.IndexOf('e',5,6));
  // String based.
  AssertEquals('String, Nonexisting returns -1',-1,S.IndexOf('a'));
  AssertEquals('String, zero based',0,S.IndexOf('T'));
  AssertEquals('String, case sensitive',-1,S.IndexOf('I'));
  AssertEquals('String, using start index',18,S.IndexOf('To',2));
  AssertEquals('String, using start index and count ',-1,S.IndexOf('To be',2,4));
  AssertEquals('String, using start index and count (partial overlap)',-1,S.IndexOf('To be',16,4));
end;

Procedure TTestStringHelper.TestIndexOfAny;

Var
  S : String;
  ES : Array of Char;
begin
  S:=TBI;
  es:=[];
  // Just a set
  SetLength(ES,0);
  AssertEquals('Empty set',-1,S.IndexOfAny(ES));
  AssertEquals('Single char in set, no match',-1,S.IndexOfAny(['a']));
  AssertEquals('Single char in set, no match (wrong case)',-1,S.IndexOfAny(['O']));
  AssertEquals('2 chars in set, no match',-1,S.IndexOfAny(['a','z']));
  AssertEquals('Single char in set, match',4,S.IndexOfAny(['e']));
  AssertEquals('2 chars in set, 2nd matches',3,S.IndexOfAny(['a','b']));
  // Start index
  AssertEquals('StartIndex, Empty set',-1,S.IndexOfAny(ES,2));
  AssertEquals('StartIndex, Single char in set, no match',-1,S.IndexOfAny(['a'],2));
  AssertEquals('StartIndex, Single char in set, no match (wrong case)',-1,S.IndexOfAny(['O'],1));
  AssertEquals('StartIndex, Single char in set, no match (index too big)',-1,S.IndexOfAny(['o'],2));
  AssertEquals('StartIndex, 2 chars in set, no match',-1,S.IndexOfAny(['a','z'],4));
  AssertEquals('StartIndex, Single char in set, match',4,S.IndexOfAny(['e'],3));
  AssertEquals('StartIndex, 2 chars in set, 2nd matches',3,S.IndexOfAny(['a','b'],2));
  // Start index, count
  AssertEquals('StartIndex, count, Empty set',-1,S.IndexOfAny(ES,2,3));
  AssertEquals('StartIndex, count, Single char in set, no match',-1,S.IndexOfAny(['a'],2));
  AssertEquals('StartIndex, count, Single char in set, no match (wrong case)',-1,S.IndexOfAny(['O'],1));
  AssertEquals('StartIndex, count, Single char in set, no match (index too big)',-1,S.IndexOfAny(['o'],2,4));
  AssertEquals('StartIndex, count, Single char in set, no match (index too big, count too small)',-1,S.IndexOfAny(['o'],5,5));
  AssertEquals('StartIndex, count, 2 chars in set, no match',-1,S.IndexOfAny(['a','z'],4,3));
  AssertEquals('StartIndex, count, Single char in set, match',4,S.IndexOfAny(['e'],3,4));
  AssertEquals('StartIndex, count, Single char in set, match in range',10,S.IndexOfAny(['e'],5,6));
  AssertEquals('StartIndex, count, 2 chars in set, 2nd matches',3,S.IndexOfAny(['a','b'],2,3));
end;

Procedure TTestStringHelper.TestIndexOfAnyString;

Var
  S : String;
  ES : Array of String;
begin
  S:=TBI;
  ES:=[];
  // Just a set
  SetLength(ES,0);
  AssertEquals('Empty set',-1,S.IndexOfAny(ES));
  AssertEquals('Single string in set, no match',-1,S.IndexOfAny(['ab']));
  AssertEquals('Single string in set, no match (wrong case)',-1,S.IndexOfAny(['TO']));
  AssertEquals('2 strings in set, no match',-1,S.IndexOfAny(['ab','yz']));
  AssertEquals('Single string in set, match',4,S.IndexOfAny(['e ']));
  AssertEquals('2 strings in set, 2nd matches',3,S.IndexOfAny(['ee','be']));
  // Start index
  AssertEquals('StartIndex, Empty set',-1,S.IndexOfAny(ES,2));
  AssertEquals('StartIndex, Single string in set, no match',-1,S.IndexOfAny(['aa'],2));
  AssertEquals('StartIndex, Single string in set, no match (wrong case)',-1,S.IndexOfAny(['TO'],1));
  AssertEquals('StartIndex, Single string in set, no match (index too big)',-1,S.IndexOfAny(['To'],2));
  AssertEquals('StartIndex, 2 strings in set, no match',-1,S.IndexOfAny(['aa','zz'],4));
  AssertEquals('StartIndex, Single string in set, match',4,S.IndexOfAny(['e '],3));
  AssertEquals('StartIndex, 2 strings in set, 2nd matches',3,S.IndexOfAny(['aa','be'],2));
  // Start index, count
  AssertEquals('StartIndex, count, Empty set',-1,S.IndexOfAny(ES,2,3));
  AssertEquals('StartIndex, count, Single string in set, no match',-1,S.IndexOfAny(['aa'],2));
  AssertEquals('StartIndex, count, Single string in set, no match (wrong case)',-1,S.IndexOfAny(['tO'],1));
  AssertEquals('StartIndex, count, Single string in set, no match (index too big)',-1,S.IndexOfAny(['To'],2,4));
  AssertEquals('StartIndex, count, Single string in set, no match (index too big, count too small)',-1,S.IndexOfAny(['To'],5,5));
  AssertEquals('StartIndex, count, 2 strings in set, no match',-1,S.IndexOfAny(['aa','zz'],4,3));
  AssertEquals('StartIndex, count, Single string in set, match',4,S.IndexOfAny(['e '],3,4));
  AssertEquals('StartIndex, count, Single string in set, match in range',10,S.IndexOfAny(['em'],5,7));
  AssertEquals('StartIndex, count, 2 strings in set, 2nd matches',3,S.IndexOfAny(['aa','be'],2,3));
end;

Procedure TTestStringHelper.TestIndexOfUnquoted;

Var
  S : String;

begin
  // Tests created from special cases in Embarcadero docs.
  S:='"Thias" ias iat';
  AssertEquals('Simple case, match',8,S.IndexOfUnquoted('ia','"','"'));
  S:='"This  is"  it';
  AssertEquals('Simple case, match',10,S.IndexOfUnquoted('  ','"','"'));
  S:='"Thias ias iat';
  AssertEquals('Opening but not closed',-1,S.IndexOfAnyUnquoted(['i','a'],'"','"'));
  S:='"Thias" "ias" "iat"';
  AssertEquals('Only spaces unquoted',-1,S.IndexOfAnyUnquoted(['i','a'],'"','"'));
  S:='<Thias <ias>> iat';
  AssertEquals('Different start/end quotes',14,S.IndexOfAnyUnquoted(['i','a'],'<','>'));
  S:='"Thias" ias iat';
  AssertEquals('Start index',3,S.IndexOfAnyUnquoted(['i','a'],'"','"',1));
  S:='Thias" "ias" "iat';
  AssertEquals('Start index',-1,S.IndexOfAnyUnquoted(['i','a'],'"','"',6));
end;

Procedure TTestStringHelper.TestIndexOfAnyUnquoted;

Var
  S : String;

begin
  // Tests created from special cases in Embarcadero docs.
  S:='"This" is it';
  AssertEquals('Simple case, match',7,S.IndexOfAnyUnquoted(['i'],'"','"'));
  AssertEquals('Simple case 2, match',7,S.IndexOfAnyUnquoted(['a','i'],'"','"'));
  S:='"This is it';
  AssertEquals('Opening but not closed',-1,S.IndexOfAnyUnquoted(['i'],'"','"'));
  S:='"This" "is" "it"';
  AssertEquals('Only spaces unquoted',-1,S.IndexOfAnyUnquoted(['i'],'"','"'));
  S:='<This <is>> it';
  AssertEquals('Different start/end quotes',12,S.IndexOfAnyUnquoted(['i'],'<','>'));
  S:='"This" is it';
  // The documentation is WRONG on this one. Delphi prints 3, not 2 as in the docs.
  AssertEquals('Start index',3,S.IndexOfAnyUnquoted(['i'],'"','"',1));
  S:='This" "is" "it';
  AssertEquals('Start index',-1,S.IndexOfAnyUnquoted(['i'],'"','"',5));
end;

Procedure TTestStringHelper.TestInsert;

Var
  S : String;

begin
  S:=TBI;
  AssertEquals('0 based (1) (result)','All To be implemented',S.Insert(0,'All '));
  AssertEquals('0 based (1) (self)','All To be implemented',S);
  S:=TBI;
  AssertEquals('0 based (2)','To be completely implemented',S.Insert(6,'completely '));
  S:=TBI;
  AssertEquals('Negative index','completely '+TBI,S.Insert(-3,'completely '));
  S:=TBI;
  AssertEquals('Too big index',TBI+'completely ',S.Insert(Length(S)+1,'completely '));
end;

Procedure TTestStringHelper.TestIsDelimiter;

Var
  S : String;

begin
  S:=TBI;
  AssertTrue('Simple case, true',S.IsDelimiter('be',3));
  AssertFalse('Simple case, false',S.IsDelimiter('ba',4));
end;

Procedure TTestStringHelper.TestIsEmpty;

Var
  S : String;

begin
  S:='';
  AssertTrue('Simple case, true',S.IsEmpty);
  S:='abc';
  AssertFalse('Simple case, false',S.IsEmpty);
end;

Procedure TTestStringHelper.TestLastDelimiter;

Var
  S : String;
begin
  S:=TBI;
  AssertEquals('Simple case, match, zero based ',0,S.LastDelimiter('T'));
  AssertEquals('Simple case, no match ',-1,S.LastDelimiter('a'));
  AssertEquals('Simple case',3,S.LastDelimiter('b'));
  AssertEquals('Simple, check last match ',Length(TBI)-2,S.LastDelimiter('e'));
  AssertEquals('Multi, no match ',-1,S.LastDelimiter('qy'));
  AssertEquals('Multi, last match 1',Length(TBI)-1,S.LastDelimiter('ed'));
  AssertEquals('Multi, last match 2',Length(TBI)-2,S.LastDelimiter('eb'));
end;

Procedure TTestStringHelper.TestLastIndexOf;

Var
  S : String;

begin
  S:=TBI;
  AssertEquals('Simple case, no match',-1,S.LastIndexOf('a'));
  AssertEquals('Simple case, zero based',0,S.LastIndexOf('T'));
  AssertEquals('Simple case last',Length(TBI)-2,S.LastIndexOf('e'));
  AssertEquals('Simple case, startindex too low',-1,S.LastIndexOf('e',3));
  AssertEquals('Simple case, startindex OK ',4,S.LastIndexOf('e',7));
  AssertEquals('Simple case, startindex OK, count too small ',-1,S.LastIndexOf('e',7,3));
  AssertEquals('Simple case, startindex OK, count border',4,S.LastIndexOf('e',7,4));
end;

Procedure TTestStringHelper.TestLastIndexOfString;

Var
  S : String;

begin
  S:=TBI;
  AssertEquals('Simple case, no match',-1,S.LastIndexOf('aa'));
  AssertEquals('Simple case, zero based',0,S.LastIndexOf('To'));
  AssertEquals('Simple case last',Length(TBI)-2,S.LastIndexOf('ed'));
  AssertEquals('Simple case, startindex too low',-1,S.LastIndexOf('ed',3));
  AssertEquals('Simple case, startindex OK ',3,S.LastIndexOf('be',7));
  AssertEquals('Simple case, startindex OK, count too small ',-1,S.LastIndexOf('be',7,3));
  AssertEquals('Simple case, startindex OK, count border',3,S.LastIndexOf('be',7,4));
end;

Procedure TTestStringHelper.TestLastIndexOfAny;

Var
  S : String;

begin
  S:=TBI;
  AssertEquals('Simple case, no match',-1,S.LastIndexOfAny(['x']));
  AssertEquals('Double case, no match',-1,S.LastIndexOfAny(['a','x']));
  AssertEquals('Simple case, zero based',0,S.LastIndexOfAny(['T']));
  AssertEquals('Double case, zero based',0,S.LastIndexOfAny(['T','q']));
  AssertEquals('Simple case last',Length(TBI)-2,S.LastIndexOf('e'));
  AssertEquals('Simple case, startindex too low',-1,S.LastIndexOf('e',3));
  AssertEquals('Simple case, startindex OK ',4,S.LastIndexOf('e',7));
  AssertEquals('Simple case, startindex OK, count too small ',-1,S.LastIndexOf('e',7,3));
  AssertEquals('Simple case, startindex OK, count border',4,S.LastIndexOf('e',7,4));
end;

Procedure TTestStringHelper.TestPadLeft;

Var
  S : String;

begin
  S:='TBI';
  AssertEquals('Default char','  TBI',S.PadLeft(5));
  AssertEquals('Length reached','TBI',S.PadLeft(3));
  AssertEquals('Length over','TBI',S.PadLeft(2));
  AssertEquals('Alternate char','**TBI',S.PadLeft(5,'*'));
end;
Procedure TTestStringHelper.TestPadRight;

Var
  S : String;

begin
  S:='TBI';
  AssertEquals('Default char','TBI  ',S.PadRight(5));
  AssertEquals('Original remains untouched','TBI',S);
  AssertEquals('Length reached','TBI',S.PadRight(3));
  AssertEquals('Original remains untouched','TBI',S);
  AssertEquals('Length over','TBI',S.PadRight(2));
  AssertEquals('Original remains untouched','TBI',S);
  AssertEquals('Alternate char','TBI**',S.PadRight(5,'*'));
  AssertEquals('Original remains untouched','TBI',S);
end;

Procedure TTestStringHelper.TestQuotedString;

Const
  TII = '''This'' is it';
  TII2 = '"This" is it';

Var
  S : String;

begin
  S:=TBI;
  AssertEquals('Default case',''''+TBI+'''',S.QuotedString);
  AssertEquals('Original remains untouched',TBI,S);
  S:=TII;
  AssertEquals('Quotes present, doubled','''''''This'''' is it''',S.QuotedString);
  AssertEquals('Original remains untouched',TII,S);
  // Other quote char
  S:=TBI;
  AssertEquals('Quote ", Default case','"'+TBI+'"',S.QuotedString('"'));
  AssertEquals('Quote ", Original remains untouched',TBI,S);
  S:=TII2;
  AssertEquals('Quote ", Quotes present, doubled','"""This"" is it"',S.QuotedString('"'));
  AssertEquals('Quote ", Original remains untouched',TII2,S);
end;

Procedure TTestStringHelper.TestRemove;

Var
  S : String;

begin
  S:=TBI;
  AssertEquals('Delete all','',S.Remove(0));
  AssertEquals('Delete all, original unchanged',TBI,S);
  AssertEquals('Delete from index','To',S.Remove(2));
  AssertEquals('Delete all, original unchanged',TBI,S);
  AssertEquals('Delete from negative index',TBI,S.Remove(-1));
  AssertEquals('Delete from negative, original unchanged',TBI,S);
  // Count
  AssertEquals('Delete N from start','be implemented',S.Remove(0,3));
  AssertEquals('Delete all, original unchanged',TBI,S);
  AssertEquals('Delete from start index, count','To implemented',S.Remove(2,3));
  AssertEquals('Delete from start index, count, original unchanged',TBI,S);
  AssertEquals('Delete from negative index, count',TBI,S.Remove(-1,4));
  AssertEquals('Delete from negative index, count, original unchanged',TBI,S);

end;

Procedure TTestStringHelper.TestReplace;
{
Function Replace(OldChar: Char; NewChar: Char): string; overload;
Function Replace(OldChar: Char; NewChar: Char; ReplaceFlags: TReplaceFlags): string; overload;
Function Replace(const OldValue: string; const NewValue: string): string; overload;
Function Replace(const OldValue: string; const NewValue: string; ReplaceFlags: TReplaceFlags): string; overload;
}

Var
  S : String;

begin
  S:=TBI;
  // Char
  AssertEquals('Simple char','to be implemented',S.Replace('T','t'));
  AssertEquals('Simple char, original unchanged',TBI,S);
  AssertEquals('Simple char is case sensitive',TBI,S.Replace('t','t'));
  AssertEquals('Simple char is replace all','To ba implamantad',S.Replace('e','a'));
  AssertEquals('Simple char, case insensitive','to be implemented',S.Replace('t','t',[rfIgnoreCase]));
  AssertEquals('Simple char, only first','To ba implemented',S.Replace('e','a',[]));
  AssertEquals('Simple char, replace all','To ba implamantad',S.Replace('e','a',[rfReplaceAll]));
  // String
  AssertEquals('Simple string','ta be implemented',S.Replace('To','ta'));
  AssertEquals('Simple string, case sensitive',TBI,S.Replace('to','ta'));
  S:='AB AB';
  AssertEquals('Simple string is replace all','cd cd',S.Replace('AB','cd'));
  S:=TBI;
  AssertEquals('Simple string, case insensitive','to be implemented',S.Replace('to','to',[rfIgnoreCase]));
  S:='AB AB AB';
  AssertEquals('Simple sting, only first','cd AB AB',S.Replace('AB','cd',[]));
  S:='AB AB AB';
  AssertEquals('Simple string, replace all','cd cd cd',S.Replace('AB','cd',[rfReplaceAll]));
end;

Function TTestStringHelper.TestArray(Msg : string; Aexpected : Array of string; AActual : TStringArray) : boolean;

Var
  I : integer;

begin
  Result:=False;
  AssertEquals(Msg+': Length correct',Length(AExpected),Length(AActual));
  for I:=0 to Length(AExpected)-1 do
    AssertEquals(Msg+': Element '+IntToStr(i)+' correct',AExpected[i],AActual[i]);
  Result:=True;
end;

Procedure TTestStringHelper.TestSplit;

Const
  TII  = '"This is" it' ;
  TII2 = '<This is> it' ;
  TII3 = '<This is>  it' ;
  CA: array[0..7] of string = ('F0;F1;F2', ';F1;F2', ';;F2', 'F0;;F2', ';;', 'F0;F1;', 'F0;;', ';F1;');

Var
  S : String;
  C: TStringArray;

begin
  S:='a b';
  C:=S.Split([' ']);
  TestArray('One letter',['a','b'],C);
  S:=TBI;
  C:=S.Split([' ']);
  TestArray('Simple case',['To','be','implemented'],C);
  C:=S.Split([' '],2);
  TestArray('Simple case, count',['To','be'],C);
  S:=TII;
  C:=S.Split([' ','"']);
  TestArray('Quote and space',['','This','is','','it'],C);
  C:=S.Split([' ','"'],TStringSplitOptions.ExcludeEmpty);
  TestArray('Quote and space, exclude empty',['This','is','it'],C);
  C:=S.Split([' '],2);
  TestArray('Quote and space, count 2',['"This','is"'],C);
  C:=S.Split([' ','"'],2,TStringSplitOptions.ExcludeEmpty);
  TestArray('Quote and space, exclude empty,count 2',['This','is'],C);
  C:=S.Split([' ','"'],1,TStringSplitOptions.ExcludeEmpty);
  TestArray('Quote and space, exclude empty, count 1',['This'],C);
  C:=S.Split([' '],'"','"');
  TestArray('Quoted, space only',['"This is"','it'],C);
  C:=S.Split([' '],'"','"',1);
  TestArray('Quoted, space only; count',['"This is"'],C);
  S:=TII2;
  C:=S.Split([' '],'<','>');
  TestArray('Quoted <>, space only',['<This is>','it'],C);
  S:=TII3;
  C:=S.Split([' '],'<','>');
  TestArray('Quoted <>, space only, have space',['<This is>','','it'],C);
  S:=TII3;
  C:=S.Split([' '],'<','>',TStringSplitOptions.ExcludeEmpty);
  TestArray('Quoted <>, space only, have space, exclude empty',['<This is>','it'],C);
  for S in CA do
    begin
    C := S.Split([';']);
    AssertEquals('Error : expect 3 elements when splitting string '+S,3,Length(C));
    end;
end;

Procedure TTestStringHelper.TestSplitString;


Const
  TII  = '"This  is"  it' ;
  TII2 = '<This  is>  it' ;
  TII3 = '<This  is>    it' ;

Var
  S : String;
  C: TStringArray;

begin
  S:=StringReplace(TBI,' ','  ',[rfReplaceAll]);
{  C:=S.Split(['  ']);
  TestArray('Simple case',['To','be','implemented'],C);
  C:=S.Split(['  '],2);
  TestArray('Simple case, count',['To','be'],C);
  S:=TII;
  C:=S.Split(['  ','"']);
  TestArray('Quote and space',['','This','is','','it'],C);
  C:=S.Split(['  ','"'],ExcludeEmpty);
  TestArray('Quote and space, exclude empty',['This','is','it'],C);
  C:=S.Split(['  '],2);
  TestArray('Quote and space, count 2',['"This','is"'],C);
  C:=S.Split(['  ','"'],2,ExcludeEmpty);
  TestArray('Quote and space, exclude empty,count 2',['This','is'],C);
  C:=S.Split(['  ','"'],1,ExcludeEmpty);
  TestArray('Quote and space, exclude empty, count 1',['This'],C);
  }
  S:=TII;
  C:=S.Split(['  '],'"','"');
  TestArray('Quoted, space only',['"This  is"','it'],C);
  C:=S.Split(['  '],'"','"',1);
  TestArray('Quoted, space only; count',['"This  is"'],C);
  S:=TII2;
  C:=S.Split(['  '],'<','>');
  TestArray('Quoted <>, space only',['<This  is>','it'],C);
  S:=TII3;
  C:=S.Split(['  '],'<','>');
  TestArray('Quoted <>, space only, have space',['<This  is>','','it'],C);
  S:=TII3;
  C:=S.Split(['  '],'<','>',TStringSplitOptions.ExcludeEmpty);
  TestArray('Quoted <>, space only, have space, exclude empty',['<This  is>','it'],C);
end;


Procedure TTestStringHelper.TestStartsWith;

Var
  S : String;

begin
  S:=TBI;
  AssertTrue('Match empty',S.StartsWith(''));
  AssertTrue('Match',S.StartsWith('To'));
  AssertFalse('Match, case sensitive',S.StartsWith('to'));
  AssertFalse('No Match',S.StartsWith('ab'));
  AssertFalse('No Match, complete',S.StartsWith('To n'));
  AssertFalse('Match, only start',S.StartsWith('be'));
  AssertTrue('Match, case insensitive',S.StartsWith('To'));
end;

Procedure TTestStringHelper.TestSubstring;

Var
  S : String;

begin
  S:=TBI;
  // No length
  AssertEquals('0 based','ed',S.SubString(Length(S)-2));
  AssertEquals('0 based, original untouched',TBI,S);
  AssertEquals('Index too big','',S.SubString(Length(S)+2));
  AssertEquals('Index negative',TBI,S.SubString(-1));
  // Length
  AssertEquals('0 based','To',S.SubString(0,2));
  AssertEquals('0 based, original untouched',TBI,S);
  AssertEquals('Index too big','',S.SubString(Length(S)+2,3));
  AssertEquals('Index negative','To',S.SubString(-1,2));
  AssertEquals('Sub, index','be',S.SubString(3,2));
end;

Procedure TTestStringHelper.TestToCharArray;

Var
  S : String;
  C : TCharArray;
  I : integer;

begin
  S:=TBI;
  C:=S.ToCharArray;
  AssertEquals('No args, length',Length(S),Length(C));
  For I:=1 to Length(S) do
    AssertEquals('No args, character (1-based) : '+IntToStr(i),S[i],C[i-1]);
  C:=S.ToCharArray(3,Length(S)-3);
  AssertEquals('No args, length',Length(S)-3,Length(C));
  For I:=4 to Length(S) do
    AssertEquals('Args(3,len), character (1-based) : '+IntToStr(i),S[i],C[i-4]);
end;

Procedure TTestStringHelper.TestToLower;

Var
  S : String;

begin
  S:=TBI;
  AssertEquals('Lowercase','to be implemented',S.ToLower);
  AssertEquals('Lowercase, original unmodified',TBI,S);
end;

Procedure TTestStringHelper.TestToLowerInvariant;

Var
  S : String;

begin
  S:=TBI;
  AssertEquals('Lowercase','to be implemented',S.ToLowerInvariant);
  AssertEquals('Lowercase, original unmodified',TBI,S);
  // This probably needs testing of some special cases.
end;

Procedure TTestStringHelper.TestToUpper;

Var
  S : String;

begin
  S:=TBI;
  AssertEquals('Lowercase','TO BE IMPLEMENTED',S.ToUpper);
  AssertEquals('Lowercase, original unmodified',TBI,S);
end;

Procedure TTestStringHelper.TestToUpperInvariant;

Var
  S : String;

begin
  S:=TBI;
  AssertEquals('Lowercase','TO BE IMPLEMENTED',S.ToUpperInvariant);
  AssertEquals('Lowercase, original unmodified',TBI,S);
  // This probably needs testing of some special cases.
end;

Procedure TTestStringHelper.TestTrim;
Var
  T,S : String;
  C : Char;

begin
  S:=TBI;
  For C:=#0 to #32 do
    S:=C+S+C;
  T:=S;
  AssertEquals('By default all chars below #32 stripped',TBI,S.Trim);
  AssertEquals('Original unmodified',T,S);
  S:='lmn'+TBI+'lmn';
  T:=S;
  AssertEquals('Strip all indicated chars',TBI,S.Trim(['l','m','n']));
  AssertEquals('Strip all indicated chars, Original unmodified',T,S);
end;

Procedure TTestStringHelper.TestTrimLeft;

Var
  O,T,S : String;
  C : Char;

begin
  S:=TBI;
  T:=TBI;
  For C:=#0 to #32 do
    begin
    S:=C+S+C;
    T:=T+C;
    end;
  O:=S;
  AssertEquals('By default all chars below #32 stripped',T,S.TrimLeft);
  AssertEquals('Original unmodified',O,S);
  S:='lmn'+TBI+'lmn';
  T:=TBI+'lmn';
  O:=S;
  AssertEquals('Strip all indicated chars',T,S.TrimLeft(['l','m','n']));
  AssertEquals('Strip all indicated chars, Original unmodified',O,S);
end;

Procedure TTestStringHelper.TestTrimRight;

Var
  O,T,S : String;
  C : Char;

begin
  S:=TBI;
  T:=TBI;
  For C:=#0 to #32 do
    begin
    S:=C+S+C;
    T:=C+T;
    end;
  O:=S;
  AssertEquals('By default all chars below #32 stripped',T,S.TrimRight);
  AssertEquals('Original unmodified',O,S);
  S:='lmn'+TBI+'lmn';
  T:='lmn'+TBI;
  O:=S;
  AssertEquals('Strip all indicated chars',T,S.TrimRight(['l','m','n']));
  AssertEquals('Strip all indicated chars, Original unmodified',O,S);
end;

Procedure TTestStringHelper.TestTrimEnd;

Var
  O,T,S : String;

begin
  S:='lmn'+TBI+'lmn';
  T:='lmn'+TBI;
  O:=S;
  AssertEquals('Strip all indicated chars',T,S.TrimRight(['l','m','n']));
  AssertEquals('Strip all indicated chars, Original unmodified',O,S);
end;

Procedure TTestStringHelper.TestTrimStart;

Var
  O,T,S : String;

begin
  S:='lmn'+TBI+'lmn';
  T:=TBI+'lmn';
  O:=S;
  AssertEquals('Strip all indicated chars',T,S.TrimLeft(['l','m','n']));
  AssertEquals('Strip all indicated chars, Original unmodified',O,S);
end;

Procedure TTestStringHelper.TestChars;

Var
  S : String;
  I : Integer;
begin
  S:=TBI;
  For I:=1 to Length(S) do
    AssertEquals('Character (1-based)'+IntToStr(i),S[i],S.Chars[i-1]);
end;

Procedure TTestStringHelper.TestLength;

Var
  S : String;

begin
  S:=TBI;
  AssertEquals('Correct length',Length(TBI),S.Length);
end;

(* // Template code;
Procedure TTestStringHelper.Test;

begin
  Result:='To be implemented';
end;
*)

Procedure RegisterStringHelperTests;


begin
  RegisterTest(TTestStringHelper);
end;

initialization
  RegisterStringHelperTests;
end.

