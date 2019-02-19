{
    Delphi/Kylix compatibility unit: String handling routines.

    This file is part of the Free Pascal run time library.
    Copyright (c) 2018 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}

{$inline on}
unit strutils;

interface

uses
  SysUtils;

{ ---------------------------------------------------------------------
    Case insensitive search/replace
  ---------------------------------------------------------------------}

Function AnsiResemblesText(const AText, AOther: string): Boolean;
Function AnsiContainsText(const AText, ASubText: string): Boolean;
Function AnsiStartsText(const ASubText, AText: string): Boolean;
Function AnsiEndsText(const ASubText, AText: string): Boolean;
Function AnsiReplaceText(const AText, AFromText, AToText: string): string;
Function AnsiMatchText(const AText: string; const AValues: array of string): Boolean;
Function AnsiIndexText(const AText: string; const AValues: array of string): Integer;

Function StartsText(const ASubText, AText: string): Boolean;
Function EndsText(const ASubText, AText: string): Boolean;

function ResemblesText(const AText, AOther: string): Boolean;
function ContainsText(const AText, ASubText: string): Boolean;
function MatchText(const AText: string; const AValues: array of string): Boolean;
function IndexText(const AText: string; const AValues: array of string): Integer;

{ ---------------------------------------------------------------------
    Case sensitive search/replace
  ---------------------------------------------------------------------}

Function AnsiContainsStr(const AText, ASubText: string): Boolean;
Function AnsiStartsStr(const ASubText, AText: string): Boolean;
Function AnsiEndsStr(const ASubText, AText: string): Boolean;
Function AnsiReplaceStr(const AText, AFromText, AToText: string): string;
Function AnsiMatchStr(const AText: string; const AValues: array of string): Boolean;
Function AnsiIndexStr(const AText: string; const AValues: array of string): Integer;
Function MatchStr(const AText: String; const AValues: array of String): Boolean;
Function IndexStr(const AText: String; const AValues: array of String): Integer;
function ContainsStr(const AText, ASubText: string): Boolean;
Function StartsStr(const ASubText, AText: string): Boolean;
Function EndsStr(const ASubText, AText: string): Boolean;

{ ---------------------------------------------------------------------
    Miscellaneous
  ---------------------------------------------------------------------}

Function DupeString(const AText: string; ACount: Integer): string;
Function ReverseString(const AText: string): string;
Function AnsiReverseString(const AText: String): String;
Function StuffString(const AText: string; AStart, ALength: Cardinal;  const ASubText: string): string;
Function RandomFrom(const AValues: array of string): string; overload;
Function IfThen(AValue: Boolean; const ATrue: string; const AFalse: string = ''): string; overload;
function NaturalCompareText (const S1 , S2 : string ): Integer ;
function NaturalCompareText(const Str1, Str2: string; const ADecSeparator, AThousandSeparator: String): Integer;


{ ---------------------------------------------------------------------
    VB emulations.
  ---------------------------------------------------------------------}

Function LeftStr(const AText: String; const ACount: SizeInt): String;
Function RightStr(const AText: String; const ACount: SizeInt): String;
Function MidStr(const AText: String; const AStart, ACount: SizeInt): String;
Function RightBStr(const AText: String; const AByteCount: SizeInt): String;
Function MidBStr(const AText: String; const AByteStart, AByteCount: SizeInt): String;
Function AnsiLeftStr(const AText: String; const ACount: SizeInt): String;
Function AnsiRightStr(const AText: String; const ACount: SizeInt): String;
Function AnsiMidStr(const AText: String; const AStart, ACount: SizeInt): String;
Function LeftBStr(const AText: String; const AByteCount: SizeInt): String;

{ ---------------------------------------------------------------------
    Extended search and replace
  ---------------------------------------------------------------------}

Var
  { Default word delimiters are any character except the core alphanumerics. }
  WordDelimiters: Array of Char;// = [#0..#255] - ['a'..'z','A'..'Z','1'..'9','0'];
  
Const
  SErrAmountStrings    = 'Amount of search and replace strings don''t match';
  SInvalidRomanNumeral = '%s is not a valid Roman numeral';

type
  TStringSearchOption = (soDown, soMatchCase, soWholeWord);
  TStringSearchOptions = set of TStringSearchOption;
  TStringSeachOption = TStringSearchOption;

Function PosEx(const SubStr, S: string; Offset: SizeUint): SizeInt;
Function PosEx(const SubStr, S: string): SizeInt; // Offset: Cardinal = 1
Function PosEx(c:char; const S: string; Offset: SizeUint): SizeInt;
function StringsReplace(const S: string; OldPattern, NewPattern: array of string;  Flags: TReplaceFlags): string;

{ ---------------------------------------------------------------------
    Delphi compat
  ---------------------------------------------------------------------}

Function ReplaceStr(const AText, AFromText, AToText: string): string;
Function ReplaceText(const AText, AFromText, AToText: string): string;

{ ---------------------------------------------------------------------
    Soundex Functions.
  ---------------------------------------------------------------------}

type
  TSoundexLength = 1..MaxInt;

Function Soundex(const AText: string; ALength: TSoundexLength): string;
Function Soundex(const AText: string): string; // ; ALength: TSoundexLength = 4

type
  TSoundexIntLength = 1..8;

Function SoundexInt(const AText: string; ALength: TSoundexIntLength): Integer;
Function SoundexInt(const AText: string): Integer; //; ALength: TSoundexIntLength = 4
Function DecodeSoundexInt(AValue: Integer): string;
Function SoundexWord(const AText: string): Word;
Function DecodeSoundexWord(AValue: Word): string;
Function SoundexSimilar(const AText, AOther: string; ALength: TSoundexLength): Boolean;
Function SoundexSimilar(const AText, AOther: string): Boolean; //; ALength: TSoundexLength = 4
Function SoundexCompare(const AText, AOther: string; ALength: TSoundexLength): Integer;
Function SoundexCompare(const AText, AOther: string): Integer; //; ALength: TSoundexLength = 4
Function SoundexProc(const AText, AOther: string): Boolean;

type
  TCompareTextProc = Function(const AText, AOther: string): Boolean;

Var
  AnsiResemblesProc: TCompareTextProc;
  ResemblesProc: TCompareTextProc;

{ ---------------------------------------------------------------------
    Other functions, based on RxStrUtils.
  ---------------------------------------------------------------------}
type
 TRomanConversionStrictness = (rcsStrict, rcsRelaxed, rcsDontCare);


function IsEmptyStr(const S: string; const EmptyChars: Array of char): Boolean;
function DelSpace(const S: string): string;
function DelChars(const S: string; Chr: Char): string;
function DelSpace1(const S: string): string;
function Tab2Space(const S: string; Numb: Byte): string;
function NPos(const C: string; S: string; N: Integer): SizeInt;
Function RPosEX(C:char;const S : String;offs:cardinal):SizeInt; overload;
Function RPosex (Const Substr : String; Const Source : String;offs:cardinal) : SizeInt; overload;
Function RPos(c:char;const S : String):SizeInt; overload;
Function RPos (Const Substr : String; Const Source : String) : SizeInt; overload;
function AddChar(C: Char; const S: string; N: Integer): string;
function AddCharR(C: Char; const S: string; N: Integer): string;
function PadLeft(const S: string; N: Integer): string;
function PadRight(const S: string; N: Integer): string;
function PadCenter(const S: string; Len: SizeInt): string;
function Copy2Symb(const S: string; Symb: Char): string;
function Copy2SymbDel(var S: string; Symb: Char): string;
function Copy2Space(const S: string): string;
function Copy2SpaceDel(var S: string): string;
function AnsiProperCase(const S: string; const WordDelims: Array of char): string;
function WordCount(const S: string; const WordDelims: Array of char): SizeInt;
function WordPosition(const N: Integer; const S: string; const WordDelims: Array of char): SizeInt;
function ExtractWord(N: Integer; const S: string;  const WordDelims: Array of char): string;
function ExtractWordPos(N: Integer; const S: string; const WordDelims: Array of char; out Pos: Integer): string;
function ExtractDelimited(N: Integer; const S: string;  const Delims: Array of char): string;
function ExtractSubstr(const S: string; var Pos: Integer;  const Delims: Array of char): string;
function IsWordPresent(const W, S: string; const WordDelims: Array of char): Boolean;
function FindPart(const HelpWilds, InputStr: string): SizeInt;
function IsWild(InputStr, Wilds: string; IgnoreCase: Boolean): Boolean;
function XorString(const Key, Src: String): String;
function XorEncode(const Key, Source: string): string;
function XorDecode(const Key, Source: string): string;
function GetCmdLineArg(const Switch: string; SwitchChars: Array of char): string;
function Numb2USA(const S: string): string;
function Hex2Dec(const S: string): Longint;
function Dec2Numb(N: Longint; Len, Base: Byte): string;
function Numb2Dec(S: string; Base: Byte): Longint;
function IntToBin(Value: Longint; Digits, Spaces: Integer): string;
function IntToBin(Value: Longint; Digits: Integer): string;
function IntToBin(Value: NativeInt; Digits:integer): string;
function IntToRoman(Value: Longint): string;
function TryRomanToInt(S: String; out N: LongInt; Strictness: TRomanConversionStrictness = rcsRelaxed): Boolean;
function RomanToInt(const S: string; Strictness: TRomanConversionStrictness = rcsRelaxed): Longint;
function RomanToIntDef(Const S : String; const ADefault: Longint = 0; Strictness: TRomanConversionStrictness = rcsRelaxed): Longint;

const
  DigitChars = ['0'..'9'];
  Brackets = ['(',')','[',']','{','}'];
  StdWordDelims = [#0..' ',',','.',';','/','\',':','''','"','`'] + Brackets;
  StdSwitchChars = ['-','/'];

function PosSet (const c:Array of char;const s : String ):SizeInt;
function PosSet (const c:string;const s : String ):SizeInt;
function PosSetEx (const c:Array of char;const s : String;count:Integer ):SizeInt;
function PosSetEx (const c:string;const s : String;count:Integer ):SizeInt;

Procedure Removeleadingchars(VAR S : String; Const CSet:Array of char);
Procedure RemoveTrailingChars(VAR S : String;Const CSet:Array of char);
Procedure RemovePadChars(VAR S : String;Const CSet:Array of char);

function TrimLeftSet(const S: String;const CSet:Array of char): String;
Function TrimRightSet(const S: String;const CSet:Array of char): String;
function TrimSet(const S: String;const CSet:Array of char): String;


type
  SizeIntArray = array of SizeInt;


implementation

uses js;

{ ---------------------------------------------------------------------
   Possibly Exception raising functions
  ---------------------------------------------------------------------}


function Hex2Dec(const S: string): Longint;
var
  HexStr: string;
begin
  if Pos('$',S)=0 then
    HexStr:='$'+ S
  else
    HexStr:=S;
  Result:=StrToInt(HexStr);
end;

{
  We turn off implicit exceptions, since these routines are tested, and it 
  saves 20% codesize (and some speed) and don't throw exceptions, except maybe 
  heap related. If they don't, that is consider a bug.

  In the future, be wary with routines that use strtoint, floating point 
  and/or format() derivatives. And check every divisor for 0.
}

{$IMPLICITEXCEPTIONS OFF}

{ ---------------------------------------------------------------------
    Case insensitive search/replace
  ---------------------------------------------------------------------}
Function AnsiResemblesText(const AText, AOther: string): Boolean;

begin
  if Assigned(AnsiResemblesProc) then
    Result:=AnsiResemblesProc(AText,AOther)
  else
    Result:=False;
end;

Function AnsiContainsText(const AText, ASubText: string): Boolean;
begin
  Result:=Pos(Uppercase(ASubText),Uppercase(AText))>0;
end;


Function AnsiStartsText(const ASubText, AText: string): Boolean;
begin
  if (Length(AText) >= Length(ASubText)) and (ASubText <> '') then
    Result := SameText(ASubText,Copy(AText,1,Length(ASubText)))
  else
    Result := False;
end;


Function AnsiEndsText(const ASubText, AText: string): Boolean;
begin
  if Length(AText) >= Length(ASubText) then
    Result := SameText(aSubText,RightStr(aText,Length(ASubText)))
  else
    Result := False;
end;


Function AnsiReplaceText(const AText, AFromText, AToText: string): string;
begin
  Result := StringReplace(AText,AFromText,AToText,[rfReplaceAll,rfIgnoreCase]);
end;


Function AnsiMatchText(const AText: string; const AValues: array of string): Boolean;
begin
  Result:=(AnsiIndexText(AText,AValues)<>-1)
end;


Function AnsiIndexText(const AText: string; const AValues: array of string): Integer;

var
  i : Integer;

begin
  Result:=-1;
  if (high(AValues)=-1) or (High(AValues)>MaxInt) Then
    Exit;
  for i:=low(AValues) to High(Avalues) do
     if CompareText(avalues[i],atext)=0 Then
       exit(i);  // make sure it is the first val.
end;


{ ---------------------------------------------------------------------
    Case sensitive search/replace
  ---------------------------------------------------------------------}

Function AnsiContainsStr(const AText, ASubText: string): Boolean;
begin
  Result := Pos(ASubText,AText)>0;
end;


Function AnsiStartsStr(const ASubText, AText: string): Boolean;
begin
  if (Length(AText) >= Length(ASubText)) and (ASubText <> '') then
    Result := (ASubText=Copy(aText,1,Length(ASubtext)))
  else
    Result := False;
end;


Function AnsiEndsStr(const ASubText, AText: string): Boolean;
begin
  if Length(AText) >= Length(ASubText) then
    Result := (ASubText=RightStr(aText,Length(ASubText)))
  else
    Result := False;
end;


Function AnsiReplaceStr(const AText, AFromText, AToText: string): string;
begin
  Result := StringReplace(AText,AFromText,AToText,[rfReplaceAll]);
end;


Function AnsiMatchStr(const AText: string; const AValues: array of string): Boolean;
begin
  Result:=AnsiIndexStr(AText,Avalues)<>-1;
end;


Function AnsiIndexStr(const AText: string; const AValues: array of string): Integer;
var
  i : longint;
begin
  result:=-1;
  if (high(AValues)=-1) or (High(AValues)>MaxInt) Then
    Exit;
  for i:=low(AValues) to High(Avalues) do
     if (avalues[i]=AText) Then
       exit(i);                                 // make sure it is the first val.
end;


Function MatchStr(const AText: String; const AValues: array of String): Boolean;
begin
  Result := IndexStr(AText,AValues) <> -1;
end;


Function IndexStr(const AText: String; const AValues: array of String): Integer;
var
  i: longint;
begin
  Result := -1;
  if (high(AValues) = -1) or (High(AValues) > MaxInt) Then
    Exit;
  for i := low(AValues) to High(Avalues) do
     if (avalues[i] = AText) Then
       exit(i);                                 // make sure it is the first val.
end;

{ ---------------------------------------------------------------------
    Playthingies
  ---------------------------------------------------------------------}

Function DupeString(const AText: string; ACount: Integer): string;

var i : SizeInt;

begin
 result:='';
 for i:=1 to ACount do
   Result:=Result+aText;
end;

Function ReverseString(const AText: string): string;

var
  i,j : SizeInt;

begin
  setlength(result,length(atext));
  i:=1; j:=length(atext);
  while (i<=j) do
    begin
    result[i]:=atext[j-i+1];
    inc(i);
    end;
end;


Function AnsiReverseString(const AText: String): String;

begin
  Result:=ReverseString(AText);
end;


Function StuffString(const AText: string; AStart, ALength: Cardinal;  const ASubText: string): string;

var i,j,k : SizeUInt;

begin
  j:=length(ASubText);
  i:=length(AText);
  if AStart>i then 
    aStart:=i+1;
  k:=i+1-AStart;
  if ALength> k then
    ALength:=k;
  SetLength(Result,i+j-ALength);
  Result:=Copy(AText,1,AStart-1)+Copy(ASubText,1,J)+Copy(AText,AStart+ALength,I+1-AStart-ALength);
end;

Function RandomFrom(const AValues: array of string): string; overload;

begin
  if high(AValues)=-1 then exit('');
  result:=Avalues[random(High(AValues)+1)];
end;

Function IfThen(AValue: Boolean; const ATrue: string; const AFalse: string = ''): string; overload;

begin
  if avalue then
    result:=atrue
  else
    result:=afalse;
end;

function NaturalCompareText(const Str1, Str2: string; const ADecSeparator, AThousandSeparator: String): Integer;
{
 NaturalCompareBase compares strings in a collated order and
 so numbers are sorted too. It sorts like this:

 01
 001
 0001

 and

 0
 00
 000
 000_A
 000_B

 in a intuitive order.
 }
var
  Num1, Num2: double;
  pStr1, pStr2: integer;
  Len1, Len2: SizeInt;
  TextLen1, TextLen2: SizeInt;
  TextStr1: string = '';
  TextStr2: string = '';
  i: SizeInt;
  j: SizeInt;
  
  function Sign(const AValue: sizeint): integer;

  begin
    If Avalue<0 then
      Result:=-1
    else If Avalue>0 then
      Result:=1
    else
      Result:=0;
  end;

  function IsNumber(ch: char): boolean; overload;
  begin
    Result := ch in ['0'..'9'];
  end;

  function GetInteger(aString : String; var pch: integer; var Len: sizeint): double;

  begin
    Result := 0;
    while (pch<=length(astring)) and IsNumber(AString[pch]) do
      begin
      Result := (Result * 10) + Ord(Astring[pch]) - Ord('0');
      Inc(Len);
      Inc(pch);
      end;
  end;

  procedure GetChars;

  begin
    TextLen1 := 0;
    while not (Str1[pStr1 + TextLen1] in ['0'..'9']) and ((pStr1 + TextLen1)<=Length(Str1)) do
      Inc(TextLen1);
    TextStr1:='';
    i := 1;
    j := 0;
    while i <= TextLen1 do
      begin
      TextStr1 := TextStr1+Str1[pStr1 + j];
      Inc(i);
      Inc(j);
      end;

    TextLen2 := 0;
    while not (Str2[pStr2 + TextLen2] in ['0'..'9']) and ((pStr2 + TextLen2)<=Length(Str2)) do
      Inc(TextLen2);
    i := 1;
    j := 0;
    while i <= TextLen2 do
      begin
      TextStr2 := TextStr2+Str2[pStr2 + j];
      Inc(i);
      Inc(j);
    end;
  end;

begin
  if (Str1 <> '') and (Str2 <> '') then
    begin
    pStr1 := 1;
    pStr2 := 1;
    Result := 0;
    while (pStr1<=Length(Str1)) and (pStr2 <=Length(Str2)) do
      begin
      TextLen1 := 1;
      TextLen2 := 1;
      Len1 := 0;
      Len2 := 0;
      while (Str1[pStr1] = ' ') do
      begin
        Inc(pStr1);
        Inc(Len1);
      end;
      while (Str2[pstr2] = ' ') do
        begin
        Inc(pStr2);
        Inc(Len2);
        end;
      if IsNumber(Str1[pStr1]) and IsNumber(Str2[pStr2]) then
        begin
         Num1 := GetInteger(Str1,pStr1, Len1);
         Num2 := GetInteger(Str2, pStr2, Len2);
        if Num1 < Num2 then
          Result := -1
        else if Num1 > Num2 then
          Result := 1
        else
        begin
          Result := Sign(Len1 - Len2);
        end;
        Dec(pStr1);
        Dec(pStr2);
        end
      else
        begin
        GetChars;
        if TextStr1 <> TextStr2 then
          Result := CompareText(TextStr1, TextStr2)
        else
          Result := 0;
        end;
      if Result <> 0 then
        Break;
      Inc(pStr1, TextLen1);
      Inc(pStr2, TextLen2);
      end;
    end;
  Num1:=Length(Str1);
  Num2:=Length(Str2);
  if (Result = 0) and (Num1 <> Num2) then
    begin
    if Num1 < Num2 then
      Result := -1
    else
      Result := 1;
    end;
  if ADecSeparator='' then ;
  if aThousandSeparator='' then ;
end;

function NaturalCompareText (const S1 , S2 : string ): Integer ;
begin
  Result := NaturalCompareText(S1, S2, DecimalSeparator,ThousandSeparator);
end;

{ ---------------------------------------------------------------------
    VB emulations.
  ---------------------------------------------------------------------}

Function LeftStr(const AText: String; const ACount: SizeInt): String;

begin
  Result:=Copy(AText,1,ACount);
end;

Function RightStr(const AText: String; const ACount: SizeInt): String;

var j,l:SizeInt;

begin
  l:=length(atext);
  j:=ACount;
  if j>l then j:=l;
  Result:=Copy(AText,l-j+1,j);
end;

Function MidStr(const AText: String; const AStart, ACount: SizeInt): String;

begin
  if (ACount=0) or (AStart>length(atext)) then
    exit('');
  Result:=Copy(AText,AStart,ACount);
end;



Function LeftBStr(const AText: String; const AByteCount: SizeInt): String;

begin
  Result:=LeftStr(AText,AByteCount);
end;


Function RightBStr(const AText: String; const AByteCount: SizeInt): String;
begin
  Result:=RightStr(Atext,AByteCount);
end;


Function MidBStr(const AText: String; const AByteStart, AByteCount: SizeInt): String;
begin
  Result:=MidStr(AText,AByteStart,AByteCount);
end;


Function AnsiLeftStr(const AText: String; const ACount: SizeInt): String;
begin
  Result := copy(AText,1,ACount);
end;


Function AnsiRightStr(const AText: String; const ACount: SizeInt): String;
begin
  Result := copy(AText,length(AText)-ACount+1,ACount);
end;


Function AnsiMidStr(const AText: String; const AStart, ACount: SizeInt): String;
begin
  Result:=Copy(AText,AStart,ACount);
end;


Function PosEx(const SubStr, S: string; Offset: SizeUint): SizeInt;


begin
  result:=TJSString.New(S).IndexOf(SubStr,offset-1)+1;
end;

Function PosEx(c:char; const S: string; Offset: SizeUint): SizeInt;
begin
  result:=TJSString.New(S).IndexOf(c,offset-1)+1;
end;

Function PosEx(const SubStr, S: string): SizeInt; // Offset: Cardinal = 1

begin
  Result:=posex(substr,s,1);
end;


function StringsReplace(const S: string; OldPattern, NewPattern: array of string;  Flags: TReplaceFlags): string;

var pc,pcc,lastpc : integer;
    strcount      : integer;
    ResStr,
    CompStr       : string;
    Found         : Boolean;
    sc            : sizeint;

begin
  sc := length(OldPattern);
  if sc <> length(NewPattern) then
    raise exception.Create(SErrAmountStrings);

  dec(sc);

  if rfIgnoreCase in Flags then
    begin
    CompStr:=UpperCase(S);
    for strcount := 0 to sc do
      OldPattern[strcount] := UpperCase(OldPattern[strcount]);
    end
  else
    CompStr := s;

  ResStr := '';
  pc := 1;
  pcc := 1;
  lastpc := pc+Length(S);

  while pc < lastpc do
    begin
    Found := False;
    for strcount := 0 to sc do
      begin
      if (Copy(compStr,pc,Length(OldPattern[strcount]))=OldPattern[strcount]) then
        begin
        ResStr := ResStr + NewPattern[strcount];
        pc := pc+Length(OldPattern[strcount]);
        pcc := pcc+Length(OldPattern[strcount]);
        Found := true;
        end
      end;
    if not found then
      begin
      ResStr := ResStr + S[pcc];
      inc(pc);
      inc(pcc);
      end
    else if not (rfReplaceAll in Flags) then
      begin
      ResStr := ResStr + copy(S,pcc,Length(S)-pcc+1);
      break;
      end;
    end;
  Result := ResStr;
end;

{ ---------------------------------------------------------------------
    Delphi compat
  ---------------------------------------------------------------------}

Function ReplaceStr(const AText, AFromText, AToText: string): string;
begin
  result:=AnsiReplaceStr(AText, AFromText, AToText);
end;

Function ReplaceText(const AText, AFromText, AToText: string): string;
begin
  result:=AnsiReplaceText(AText, AFromText, AToText);
end;

{ ---------------------------------------------------------------------
    Soundex Functions.
  ---------------------------------------------------------------------}
Var
  SScore : String =
      '00000000000000000000000000000000'+ // 1..32
      '00000000000000000000000000000000'+ // 33..64
      '0123012i02245501262301i2i2'+ // 65..90
      '000000'+ // 91..96
      '0123012i02245501262301i2i2'+ // 97..122
      '00000000000000000000000000000000'+ // 123..154
      '00000000000000000000000000000000'+ // 155..186
      '00000000000000000000000000000000'+ // 187..218
      '00000000000000000000000000000000'+ // 219..250
      '00000'; // 251..255

Function Soundex(const AText: string; ALength: TSoundexLength): string;

Var
  S,PS : Char;
  I,L : SizeInt;

begin
  Result:='';
  PS:=#0;
  If Length(AText)>0 then
    begin
    Result:=Upcase(AText[1]);
    I:=2;
    L:=Length(AText);
    While (I<=L) and (Length(Result)<ALength) do
      begin
      S:=SScore[Ord(AText[i])];
      If Not (S in ['0','i',PS]) then
        Result:=Result+S;
      If (S<>'i') then
        PS:=S;
      Inc(I);
      end;
    end;
  L:=Length(Result);
  If (L<ALength) then
    Result:=Result+StringOfChar('0',Alength-L);
end;



Function Soundex(const AText: string): string; // ; ALength: TSoundexLength = 4

begin
  Result:=Soundex(AText,4);
end;

Const
  Ord0 = Ord('0');
  OrdA = Ord('A');

Function SoundexInt(const AText: string; ALength: TSoundexIntLength): Integer;

var
  SE: string;
  I: SizeInt;

begin
  Result:=-1;
  SE:=Soundex(AText,ALength);
  If Length(SE)>0 then
    begin
    Result:=Ord(SE[1])-OrdA;
    if ALength > 1 then
      begin
      Result:=Result*26+(Ord(SE[2])-Ord0);
      for I:=3 to ALength do
        Result:=(Ord(SE[I])-Ord0)+Result*7;
      end;
    Result:=ALength+Result*9;
    end;
end;


Function SoundexInt(const AText: string): Integer; //; ALength: TSoundexIntLength = 4
begin
  Result:=SoundexInt(AText,4);
end;


Function DecodeSoundexInt(AValue: Integer): string;

var
  I, Len: Integer;

begin
  Result := '';
  Len := AValue mod 9;
  AValue := AValue div 9;
  for I:=Len downto 3 do
    begin
    Result:=Chr(Ord0+(AValue mod 7))+Result;
    AValue:=AValue div 7;
    end;
  if Len>1 then
    begin
    Result:=Chr(Ord0+(AValue mod 26))+Result;
    AValue:=AValue div 26;
    end;
  Result:=Chr(OrdA+AValue)+Result;
end;


Function SoundexWord(const AText: string): Word;

Var
  S : String;

begin
  S:=SoundEx(Atext,4);
  Result:=Ord(S[1])-OrdA;
  Result:=Result*26+ord(S[2])-48;
  Result:=Result*7+ord(S[3])-48;
  Result:=Result*7+ord(S[4])-48;
end;


Function DecodeSoundexWord(AValue: Word): string;
begin
  Result := Chr(Ord0+ (AValue mod 7));
  AValue := AValue div 7;
  Result := Chr(Ord0+ (AValue mod 7)) + Result;
  AValue := AValue div 7;
  Result := IntToStr(AValue mod 26) + Result;
  AValue := AValue div 26;
  Result := Chr(OrdA+AValue) + Result;
end;


Function SoundexSimilar(const AText, AOther: string; ALength: TSoundexLength): Boolean;
begin
  Result:=Soundex(AText,ALength)=Soundex(AOther,ALength);
end;


Function SoundexSimilar(const AText, AOther: string): Boolean; //; ALength: TSoundexLength = 4
begin
  Result:=SoundexSimilar(AText,AOther,4);
end;


Function SoundexCompare(const AText, AOther: string; ALength: TSoundexLength): Integer;
begin
  Result:=AnsiCompareStr(Soundex(AText,ALength),Soundex(AOther,ALength));
end;


Function SoundexCompare(const AText, AOther: string): Integer; //; ALength: TSoundexLength = 4
begin
  Result:=SoundexCompare(AText,AOther,4);
end;


Function SoundexProc(const AText, AOther: string): Boolean;
begin
  Result:=SoundexSimilar(AText,AOther);
end;

{ ---------------------------------------------------------------------
    RxStrUtils-like functions.
  ---------------------------------------------------------------------}



function IsEmptyStr(const S: string; const EmptyChars: Array of char): Boolean;

var
  i,l: SizeInt;

begin
  l:=Length(S);
  i:=1;
  Result:=True;
  while Result and (i<=l) do
    begin
    Result:=CharInSet(S[i],EmptyChars);
    Inc(i);
    end;
end;

function DelSpace(const S: String): string;

begin
  Result:=DelChars(S,' ');
end;

function DelChars(const S: string; Chr: Char): string;

var
  I,J: SizeInt;

begin
  Result:=S;
  I:=Length(Result);
  While I>0 do
    begin
    if Result[I]=Chr then
      begin
      J:=I-1;
      While (J>0) and (Result[J]=Chr) do
        Dec(j);
      Delete(Result,J+1,I-J);
      I:=J+1;
      end;
    dec(I);
    end;
end;

function DelSpace1(const S: string): string;

var
  I : SizeInt;

begin
  Result:=S;
  for i:=Length(Result) downto 2 do
    if (Result[i]=' ') and (Result[I-1]=' ') then
      Delete(Result,I,1);
end;

function Tab2Space(const S: string; Numb: Byte): string;

var
  I: SizeInt;

begin
  I:=1;
  Result:=S;
  while I <= Length(Result) do
    if Result[I]<>Chr(9) then
      inc(I)
    else
      begin
      Result[I]:=' ';
      If (Numb>1) then
        Insert(StringOfChar(' ',Numb-1),Result,I);
      Inc(I,Numb);
      end;
end;

function NPos(const C: string; S: string; N: Integer): SizeInt;

var
  i,p,k: SizeInt;

begin
  Result:=0;
  if N<1 then
    Exit;
  k:=0;
  i:=1;
  Repeat
    p:=pos(C,S);
    Inc(k,p);
    if p>0 then
      delete(S,1,p);
    Inc(i);
  Until (i>n) or (p=0);
  If (P>0) then
    Result:=K;
end;

function AddChar(C: Char; const S: string; N: Integer): string;

Var
  l : SizeInt;

begin
  Result:=S;
  l:=Length(Result);
  if l<N then
    Result:=StringOfChar(C,N-l)+Result;
end;

function AddCharR(C: Char; const S: string; N: Integer): string;

Var
  l : SizeInt;

begin
  Result:=S;
  l:=Length(Result);
  if l<N then
    Result:=Result+StringOfChar(C,N-l);
end;


function PadRight(const S: string; N: Integer): string;
begin
  Result:=AddCharR(' ',S,N);
end;


function PadLeft(const S: string; N: Integer): string;
begin
  Result:=AddChar(' ',S,N);
end;


function Copy2Symb(const S: string; Symb: Char): string;

var
  p: SizeInt;

begin
  p:=Pos(Symb,S);
  if p=0 then
    p:=Length(S)+1;
  Result:=Copy(S,1,p-1);
end;

function Copy2SymbDel(var S: string; Symb: Char): string;

var
  p: SizeInt;

begin
  p:=Pos(Symb,S);
  if p=0 then
    begin
      result:=s;
      s:='';
    end
  else
    begin	
      Result:=Copy(S,1,p-1);
      delete(s,1,p);		
    end;
end;

function Copy2Space(const S: string): string;
begin
  Result:=Copy2Symb(S,' ');
end;

function Copy2SpaceDel(var S: string): string;
begin
  Result:=Copy2SymbDel(S,' ');
end;

function AnsiProperCase(const S: string; const WordDelims: Array of char): string;

var
  P,L : Integer;

begin
  Result:=LowerCase(S);
  P:=1;
  L:=Length(Result);
  while (P<=L) do
    begin
    while (P<=L) and (CharInSet(Result[P],WordDelims)) do
      inc(P);
    if (P<=L) then
      Result[P]:=UpCase(Result[P]);
    while (P<=L) and not CharInSet(Result[P],WordDelims) do
      inc(P);
    end;
end;

function WordCount(const S: string; const WordDelims: Array of char): SizeInt;

var
  P,L : Integer;

begin
  Result:=0;
  P:=1;
  L:=Length(S);
  while (P<=L) do
    begin
    while (P<=L) and (CharInSet(S[P],WordDelims)) do
      Inc(P);
    if (P<=L) then
      inc(Result);
    while (P<=L) and not (CharInSet(S[P],WordDelims)) do
      inc(P);
    end;
end;

function WordPosition(const N: Integer; const S: string; const WordDelims: Array of char): SizeInt;

var
  PS,P,PE,Count : Integer;

begin
  Result:=0;
  Count:=0;
  PS:=1;
  PE:=Length(S);
  P:=PS;
  while (P<=PE) and (Count<>N) do
    begin
    while (P<=PE) and CharInSet(S[P],WordDelims) do
      inc(P);
    if (P<=PE) then
      inc(Count);
    if (Count<>N) then
      while (P<=PE) and not CharInSet(S[P],WordDelims) do
        inc(P)
    else
      Result:=(P-PS)+1;
    end;
end;


function ExtractWord(N: Integer; const S: string; const WordDelims: Array of char): string;
var
  i: LongInt;
begin
  Result:=ExtractWordPos(N,S,WordDelims,i);
end;


function ExtractWordPos(N: Integer; const S: string; const WordDelims: Array of char; out Pos: Integer): string;

var
  i,j,l: SizeInt;

begin
  j:=0;
  i:=WordPosition(N, S, WordDelims);
  if (I>MaxInt) then
    begin
    Result:='';
    Pos:=-1;
    Exit;
    end;
  Pos:=i;
  if (i<>0) then
    begin
    j:=i;
    l:=Length(S);
    while (j<=L) and not CharInSet(S[j],WordDelims) do
      inc(j);
    end;
  Result:=Copy(S,I,j-I);
end;


function ExtractDelimited(N: Integer; const S: string; const Delims: Array of char): string;
var
  w,i,l,len: SizeInt;
begin
  w:=0;
  i:=1;
  l:=0;
  len:=Length(S);
  SetLength(Result, 0);
  while (i<=len) and (w<>N) do
    begin
    if CharInSet(S[i],Delims) then
      inc(w)
    else
      begin
      if (N-1)=w then
        begin
        inc(l);
        Result:=Result+S[i];
        end;
      end;
    inc(i);
    end;
end;


function ExtractSubstr(const S: string; var Pos: Integer; const Delims: Array of char): string;

var
  i,l: SizeInt;

begin
  i:=Pos;
  l:=Length(S);
  while (i<=l) and not CharInSet(S[i],Delims) do
    inc(i);
  Result:=Copy(S,Pos,i-Pos);
  while (i<=l) and CharInSet(S[i],Delims) do
    inc(i);
  if I>MaxInt then
    Pos:=MaxInt
  else
    Pos:=i;
end;

function isWordPresent(const W, S: string; const WordDelims: Array of char): Boolean;

var
  i,Count : SizeInt;

begin
  Result:=False;
  Count:=WordCount(S, WordDelims);
  I:=1;
  While (Not Result) and (I<=Count) do
    begin
    Result:=ExtractWord(i,S,WordDelims)=W;
    Inc(i);
    end;
end;


function Numb2USA(const S: string): string;
var
  i, NA: Integer;
begin
  i:=Length(S);
  Result:=S;
  NA:=0;
  while (i > 0) do begin
    if ((Length(Result) - i + 1 - NA) mod 3 = 0) and (i <> 1) then
    begin
      insert(',', Result, i);
      inc(NA);
    end;
    Dec(i);
  end;
end;

function PadCenter(const S: string; Len: SizeInt): string;
begin
  if Length(S)<Len then
    begin
    Result:=StringOfChar(' ',(Len div 2) -(Length(S) div 2))+S;
    Result:=Result+StringOfChar(' ',Len-Length(Result));
    end
  else
    Result:=S;
end;


function Dec2Numb(N: Longint; Len, Base: Byte): string;

var
  C: Integer;
  Number: Longint;

begin
  if N=0 then
    Result:='0'
  else
    begin
    Number:=N;
    Result:='';
    while Number>0 do
      begin
      C:=Number mod Base;
      if C>9 then
        C:=C+55
      else
        C:=C+48;
      Result:=Chr(C)+Result;
      Number:=Number div Base;
      end;
    end;
  if (Result<>'') then
    Result:=AddChar('0',Result,Len);
end;

function Numb2Dec(S: string; Base: Byte): Longint;

var
  i, P: sizeint;

begin
  i:=Length(S);
  Result:=0;
  S:=UpperCase(S);
  P:=1;
  while (i>=1) do
    begin
    if (S[i]>'@') then
      Result:=Result+(Ord(S[i])-55)*P
    else
      Result:=Result+(Ord(S[i])-48)*P;
    Dec(i);
    P:=P*Base;
    end;
end;

Function RomanValues(C : Char) : Word;

begin
  Case c of
    'C' : Result:=100;
    'D' : Result:=500;
    'I' : Result:=1;
    'L' : Result:=50;
    'M' : Result:=1000;
    'V' : Result:=5;
    'X' : Result:=10;
  else
    Result:=0;
  end;
end;

function RomanToIntDontCare(const S: String): Longint;
{This was the original implementation of RomanToInt,
 it is internally used in TryRomanToInt when Strictness = rcsDontCare}
const
  RomanChars  = ['C','D','I','L','M','V','X'];

var
  index, Next: Char;
  i,l: SizeInt;
  Negative: Boolean;

begin
  Result:=0;
  i:=0;
  Negative:=(Length(S)>0) and (S[1]='-');
  if Negative then
    inc(i);
  l:=Length(S);
  while (i<l) do
    begin
    inc(i);
    index:=UpCase(S[i]);
    if index in RomanChars then
      begin
      if (i+1)<=l then
        Next:=UpCase(S[i+1])
      else
        Next:=#0;
      if (Next in RomanChars) and (RomanValues(index)<RomanValues(Next)) then
        begin
        inc(Result, RomanValues(Next));
        Dec(Result, RomanValues(index));
        inc(i);
        end
      else
        inc(Result, RomanValues(index));
      end
    else
      begin
      Result:=0;
      Exit;
      end;
    end;
  if Negative then
    Result:=-Result;
end;


{ TryRomanToInt: try to convert a roman numeral to an integer
  Parameters:
  S: Roman numeral (like: 'MCMXXII')
  N: Integer value of S (only meaningfull if the function succeeds)
  Stricness: controls how strict the parsing of S is
    - rcsStrict:
      * Follow common subtraction rules
         - only 1 preceding subtraction character allowed: IX = 9, but IIX <> 8
         - from M you can only subtract C
         - from D you can only subtract C
         - from C you can only subtract X
         - from L you can only subtract X
         - from X you can only subtract I
         - from V you can only subtract I
      *  The numeral is parsed in "groups" (first M's, then D's etc.), the next group to be parsed
         must always be of a lower denomination than the previous one.
         Example: 'MMDCCXX' is allowed but 'MMCCXXDD' is not
      * There can only ever be 3 consecutive M's, C's, X's or I's
      * There can only ever be 1 D, 1 L and 1 V
      * After IX or IV there can be no more characters
      * Negative numbers are not supported
      // As a consequence the maximum allowed Roman numeral is MMMCMXCIX = 3999, also N can never become 0 (zero)

    - rcsRelaxed: Like rcsStrict but with the following exceptions:
      * An infinite number of (leading) M's is allowed
      * Up to 4 consecutive M's, C's, X's and I's are allowed
      // So this is allowed: 'MMMMMMCXIIII'  = 6124

    - rcsDontCare:
      * no checking on the order of "groups" is done
      * there are no restrictions on the number of consecutive chars
      * negative numbers are supported
      * an empty string as input will return True and N will be 0
      * invalid input will return false
      // for backwards comatibility: it supports rather ludicrous input like '-IIIMIII' -> -(2+(1000-1)+3)=-1004
}

function TryRomanToInt(S: String; out N: LongInt; Strictness: TRomanConversionStrictness = rcsRelaxed): Boolean;

var
  i, Len: SizeInt;
  Terminated: Boolean;

begin
  Result := (False);
  S := UpperCase(S);  //don't use AnsiUpperCase please
  Len := Length(S);
  if (Strictness = rcsDontCare) then
  begin
    N := RomanToIntDontCare(S);
    if (N = 0) then
    begin
      Result := (Len = 0);
    end
    else
      Result := True;
    Exit;
  end;
  if (Len = 0) then Exit;
  i := 1;
  N := 0;
  Terminated := False;
  //leading M's
  while (i <= Len) and ((Strictness <> rcsStrict) or (i < 4)) and (S[i] = 'M') do
  begin
    //writeln('TryRomanToInt: Found 1000');
    Inc(i);
    N := N + 1000;
  end;
  //then CM or or CD or D or (C, CC, CCC, CCCC)
  if (i <= Len) and (S[i] = 'D') then
  begin
    //writeln('TryRomanToInt: Found 500');
    Inc(i);
    N := N + 500;
  end
  else if (i + 1 <= Len) and (S[i] = 'C') then
  begin
    if (S[i+1] = 'M') then
    begin
      //writeln('TryRomanToInt: Found 900');
      Inc(i,2);
      N := N + 900;
    end
    else if (S[i+1] = 'D') then
    begin
      //writeln('TryRomanToInt: Found 400');
      Inc(i,2);
      N := N + 400;
    end;
  end ;
  //next max 4 or 3 C's, depending on Strictness
  if (i <= Len) and (S[i] = 'C') then
  begin
    //find max 4 C's
    //writeln('TryRomanToInt: Found 100');
    Inc(i);
    N := N + 100;
    if (i <= Len) and (S[i] = 'C') then
    begin
      //writeln('TryRomanToInt: Found 100');
      Inc(i);
      N := N + 100;
    end;
    if (i <= Len) and (S[i] = 'C') then
    begin
      //writeln('TryRomanToInt: Found 100');
      Inc(i);
      N := N + 100;
    end;
    if (Strictness <> rcsStrict) and (i <= Len) and (S[i] = 'C') then
    begin
      //writeln('TryRomanToInt: Found 100');
      Inc(i);
      N := N + 100;
    end;
  end;

  //then XC or XL
  if (i + 1 <= Len) and (S[i] = 'X') then
  begin
    if (S[i+1] = 'C') then
    begin
      //writeln('TryRomanToInt: Found 90');
      Inc(i,2);
      N := N + 90;
    end
    else if  (S[i+1] = 'L') then
    begin
      //writeln('TryRomanToInt: Found 40');
      Inc(i,2);
      N := N + 40;
    end;
  end;

  //then L
  if (i <= Len) and (S[i] = 'L') then
  begin
    //writeln('TryRomanToInt: Found 50');
    Inc(i);
    N := N + 50;
  end;

  //then (X, xx, xxx, xxxx)
  if (i <= Len) and (S[i] = 'X') then
  begin
    //find max 3 or 4 X's, depending on Strictness
    //writeln('TryRomanToInt: Found 10');
    Inc(i);
    N := N + 10;
    if (i <= Len) and (S[i] = 'X') then
    begin
      //writeln('TryRomanToInt: Found 10');
      Inc(i);
      N := N + 10;
    end;
    if (i <= Len) and (S[i] = 'X') then
    begin
      //writeln('TryRomanToInt: Found 10');
      Inc(i);
      N := N + 10;
    end;
    if (Strictness <> rcsStrict) and (i <= Len) and (S[i] = 'X') then
    begin
      //writeln('TryRomanToInt: Found 10');
      Inc(i);
      N := N + 10;
    end;
  end;

  //then IX or IV
  if (i + 1 <= Len) and (S[i] = 'I') then
  begin
    if (S[i+1] = 'X') then
    begin
      Terminated := (True);
      //writeln('TryRomanToInt: Found 9');
      Inc(i,2);
      N := N + 9;
    end
    else if (S[i+1] = 'V') then
    begin
      Terminated := (True);
      //writeln('TryRomanToInt: Found 4');
      Inc(i,2);
      N := N + 4;
    end;
  end;

  //then V
  if (not Terminated) and (i <= Len) and (S[i] = 'V') then
  begin
    //writeln('TryRomanToInt: Found 5');
    Inc(i);
    N := N + 5;
  end;


  //then I
  if (not Terminated) and (i <= Len) and (S[i] = 'I') then
  begin
    Terminated := (True);
    //writeln('TryRomanToInt: Found 1');
    Inc(i);
    N := N + 1;
    //Find max 2 or 3 closing I's, depending on strictness
    if (i <= Len) and (S[i] = 'I') then
    begin
      //writeln('TryRomanToInt: Found 1');
      Inc(i);
      N := N + 1;
    end;
    if (i <= Len) and (S[i] = 'I') then
    begin
      //writeln('TryRomanToInt: Found 1');
      Inc(i);
      N := N + 1;
    end;
    if (Strictness <> rcsStrict) and (i <= Len) and (S[i] = 'I') then
    begin
      //writeln('TryRomanToInt: Found 1');
      Inc(i);
      N := N + 1;
    end;
  end;

  //writeln('TryRomanToInt: Len = ',Len,' i = ',i);
  Result := (i > Len);
  //if Result then writeln('TryRomanToInt: N = ',N);

end;

function RomanToInt(const S: string; Strictness: TRomanConversionStrictness = rcsRelaxed): Longint;
begin
  if not TryRomanToInt(S, Result, Strictness) then
    raise EConvertError.CreateFmt(SInvalidRomanNumeral,[S]);
end;

function RomanToIntDef(const S: String; const ADefault: Longint;
  Strictness: TRomanConversionStrictness): Longint;
begin
  if not TryRomanToInt(S, Result, Strictness) then
    Result := ADefault;
end;




function intToRoman(Value: Longint): string;

const
  Arabics : Array[1..13] of Integer
          = (1,4,5,9,10,40,50,90,100,400,500,900,1000);
  Romans  :  Array[1..13] of String
          = ('I','IV','V','IX','X','XL','L','XC','C','CD','D','CM','M');

var
  i: Integer;

begin
  Result:='';
  for i:=13 downto 1 do
    while (Value >= Arabics[i]) do
      begin
        Value:=Value-Arabics[i];
        Result:=Result+Romans[i];
      end;
end;

function intToBin(Value: Longint; Digits, Spaces: Integer): string;

var endpos : integer;
    p,p2: integer;
    k: integer;

begin
  Result:='';
  if (Digits>32) then
    Digits:=32;
  if (spaces=0) then
   begin
     result:=inttobin(value,digits);
     exit;
   end;
  endpos:=digits+ (digits-1) div spaces;
  setlength(result,endpos);
  p:=endpos;
  p2:=1;
  k:=spaces;
  while (p>=p2) do
    begin
      if k=0 then
       begin
         Result[p]:=' ';
         dec(p);
         k:=spaces;
       end;
      Result[P]:=chr(48+(cardinal(value) and 1));
      value:=cardinal(value) shr 1;
      dec(p); 
      dec(k);
   end;
end;

function intToBin(Value: Longint; Digits:integer): string;

var
    p,p2 : integer;

begin
  result:='';
  if digits<=0 then exit;
  setlength(result,digits);
  p:=digits;
  p2:=1;
  // typecasts because we want to keep intto* delphi compat and take an integer
  while (p>=p2) and (cardinal(value)>0) do     
    begin
       Result[p]:=chr(48+(cardinal(value) and 1));
       value:=cardinal(value) shr 1;
       dec(p); 
    end;
  digits:=p-p2+1;
  While digits>0 do
    begin
    Result[Digits]:=Chr(48);
    Dec(Digits);
    end;
end;

function IntToBin(Value: NativeInt; Digits:integer): string;
var
      p,p2 : integer;
begin
  result:='';
  if digits<=0 then exit;
  setlength(result,digits);
  p:=digits;
  p2:=1;
  // typecasts because we want to keep intto* delphi compat and take a signed val
  // and avoid warnings
  while (p>=p2) and (value>0) do
    begin
       Result[p]:=chr(48+(cardinal(value) and 1));
       value:=value div 2;
       dec(p); 
    end;
  digits:=p-p2+1;
  While digits>0 do
    result[digits]:=#48;
end;


function FindPart(const HelpWilds, inputStr: string): SizeInt;
var
  Diff, i, J: SizeInt;

begin
  Result:=0;
  i:=Pos('?',HelpWilds);
  if (i=0) then
    Result:=Pos(HelpWilds, inputStr)
  else
    begin
    Diff:=Length(inputStr) - Length(HelpWilds);
    for i:=0 to Diff do
      begin
      for J:=1 to Length(HelpWilds) do
        if (inputStr[i + J] = HelpWilds[J]) or (HelpWilds[J] = '?') then
          begin
          if (J=Length(HelpWilds)) then
            begin
            Result:=i+1;
            Exit;
            end;
          end
        else
          Break;
      end;
    end;
end;

Function isMatch(level : integer;inputstr,wilds : string; CWild, CinputWord: SizeInt;MaxInputword,maxwilds : SizeInt; Out EOS : Boolean) : Boolean;

begin
  EOS:=False;
  Result:=True;
  repeat
    if Wilds[CWild] = '*' then { handling of '*' }
      begin
      inc(CWild);
      while Wilds[CWild] = '?' do { equal to '?' }
        begin
        { goto next letter }
        inc(CWild);
        inc(CinputWord);
        end;
      { increase until a match }
      Repeat
        while (inputStr[CinputWord]<>Wilds[CWild]) and (CinputWord <= MaxinputWord) do
          inc(CinputWord);
        Result:=isMatch(Level+1,inputstr,wilds,CWild, CinputWord,MaxInputword,maxwilds,EOS);
        if not Result then
          Inc(cInputWord);
      Until Result or (CinputWord>=MaxinputWord);
      if Result and EOS then
        Exit;
      Continue;
      end;
    if Wilds[CWild] = '?' then { equal to '?' }
      begin
      { goto next letter }
      inc(CWild);
      inc(CinputWord);
      Continue;
      end;
    if inputStr[CinputWord] = Wilds[CWild] then { equal letters }
      begin
      { goto next letter }
      inc(CWild);
      inc(CinputWord);
      Continue;
      end;
    Result:=false;
    Exit;
  until (CinputWord > MaxinputWord) or (CWild > MaxWilds);
  { no completed evaluation, we need to check what happened }
  if (CinputWord <= MaxinputWord) or (CWild < MaxWilds) then
    Result:=false
  else if (CWild>Maxwilds) then
    EOS:=False
  else
    begin
    EOS:=Wilds[CWild]='*';
    if not EOS then
      Result:=False;
    end
end;

function isWild(inputStr, Wilds: string; ignoreCase: boolean): boolean;

var
  i: SizeInt;
  MaxinputWord, MaxWilds: SizeInt; { Length of inputStr and Wilds }
  eos : Boolean;

begin
  Result:=true;
  if Wilds = inputStr then
    Exit;
  { delete '**', because '**' = '*' }
  i:=Pos('**', Wilds);
  while i > 0 do
    begin
    Delete(Wilds, i, 1);
    i:=Pos('**', Wilds);
    end;
  if Wilds = '*' then { for fast end, if Wilds only '*' }
    Exit;
  MaxinputWord:=Length(inputStr);
  MaxWilds:=Length(Wilds);
  if (MaxWilds = 0) or (MaxinputWord = 0) then
    begin
    Result:=false;
    Exit;
    end;
  if ignoreCase then { upcase all letters }
    begin
    inputStr:=UpperCase(inputStr);
    Wilds:=UpperCase(Wilds);
    end;
  Result:=isMatch(1,inputStr,wilds,1,1,MaxinputWord, MaxWilds,EOS);
end;


function XorString(const Key, Src: String): String;
var
  i: SizeInt;
begin
  Result:=Src;
  if Length(Key) > 0 then
    for i:=1 to Length(Src) do
      Result[i]:=Chr(Ord(Key[1 + ((i - 1) mod Length(Key))]) xor Ord(Src[i]));
end;

function XorEncode(const Key, Source: string): string;

var
  i: Integer;
  C: Byte;

begin
  Result:='';
  for i:=1 to Length(Source) do
    begin
    if Length(Key) > 0 then
      C:=Ord(Key[1 + ((i - 1) mod Length(Key))]) xor Ord(Source[i])
    else
      C:=Ord(Source[i]);
    Result:=Result+LowerCase(intToHex(C, 2));
    end;
end;

function XorDecode(const Key, Source: string): string;
var
  i: Integer;
  C: Char;
begin
  Result:='';
  for i:=0 to Length(Source) div 2 - 1 do
    begin
    C:=Chr(StrTointDef('$' + Copy(Source, (i * 2) + 1, 2), Ord(' ')));
    if Length(Key) > 0 then
      C:=Chr(Ord(Key[1 + (i mod Length(Key))]) xor Ord(C));
    Result:=Result + C;
    end;
end;

function GetCmdLineArg(const Switch: string; SwitchChars: Array of char): string;
var
  i: Integer;
  S: string;
begin
  i:=1;
  Result:='';
  while (Result='') and (i<=ParamCount) do
    begin
    S:=ParamStr(i);
    if (Length(SwitchChars)=0) or (CharInSet(S[1],SwitchChars) and (Length(S) > 1)) and
       (CompareText(Copy(S,2,Length(S)-1),Switch)=0) then
      begin
      inc(i);
      if i<=ParamCount then
        Result:=ParamStr(i);
      end;
    inc(i);
    end;
end;

Function RPosEX(C:char;const S : String;offs:cardinal):SizeInt; overload;

Begin
 Result:=TJSString.New(S).lastIndexOf(c,offs-1)+1;
End;

Function RPos(c:char;const S : String):SizeInt;

begin
  Result:=RPosEx(string(C),S,Length(S));
end;

Function RPos (Const Substr : String; Const Source : String) : SizeInt; overload;
begin
  Result:=RPosEx(SubStr,Source,Length(Source));
end;

Function RPosex (Const Substr : String; Const Source : String;offs:cardinal) : SizeInt; overload;

begin
  Result:=TJSString.New(Source).lastIndexOf(SubStr,offs-1)+1;
end;

function possetex (const c:Array of char;const s : String;count:Integer ):SizeInt;

var i,j:SizeInt;

begin
  if s='' then
    j:=0
  else
    begin
    i:=length(s);
    j:=count;
    if j>i then
      begin
      result:=0;
      exit;
      end;
    while (j<=i) and (not CharInSet(s[j],c)) do inc(j);
    if (j>i) then
      j:=0;                                         // not found.
    end;
 result:=j;
end;

function PosSetEx (const c:string;const s : String;count:Integer ):SizeInt;

var
  cset : Array of char;
  i,l    : SizeInt;
begin
  L:=Length(C);
  SetLength(Cset,L);
  if L>0 then
    for i:=1 to l do
      cset[i-1]:=c[i];
  Result:=PosSetEx(cset,s,count);
end;

function posset (const c:Array of char;const s : String ):SizeInt;

begin
  result:=possetex(c,s,1);
end;

function PosSet (const c:string;const s : String ):SizeInt;

begin
  Result:=PosSetEx(c,S,1);
end;


Procedure Removeleadingchars(Var S : String; Const CSet:Array of char);

var
  I,J : Longint;

begin
  I:=Length(S);
  if (I>0) then
    begin
    J:=1;
    while (J<=I) and CharInSet(S[J],CSet) DO
      inc(J);
    if J>1 then
      Delete(S,1,J-1);
    end;
end;


function TrimLeftSet(const S: String;const CSet:Array of char): String;

begin
  result:=s;
  removeleadingchars(result,cset); 
end;

Procedure RemoveTrailingChars(VAR S : String;Const CSet:Array of char);

var
  i,j : longint;

begin
  I:=Length(S);
  if (I>0) then
    begin
    J:=I;
    while (j>0) and CharInSet(S[J],CSet) do
      dec(J);
    if J<>I then
      setLength(S,J);
    End;
End;

Function TrimRightSet(const S: String;const CSet:Array of char): String;

begin
  result:=s;
  RemoveTrailingchars(result,cset); 
end;

Procedure RemovePadChars(VAR S : String;Const CSet:Array of char);

var
  I,J,K: longint;

begin
  I:=Length(S);
  if I=0 then exit;
  J:=I;
  while (j>0) and CharInset(S[J],CSet) do
    dec(J);
  if j=0 Then
    begin
    s:='';
    exit;
    end;
  SetLength(S,J);
  I:=J;
  k:=1;
  while (k<=I) and CharInSet(S[k],CSet) do
    inc(k);
  if k>1 Then
    Delete(S,1,K-1);
end;

function TrimSet(const S: String;const CSet:Array of char): String;

begin
  Result:=s;
  RemovePadChars(Result,cset);
end;

function StartsText(const ASubText, AText: string): Boolean; inline;
begin
  Result := AnsiStartsText(ASubText, AText);
end;


function EndsText(const ASubText, AText: string): Boolean;
begin
  Result := AnsiEndsText(ASubText, AText);
end;

function ResemblesText(const AText, AOther: string): Boolean;
begin
  if Assigned(ResemblesProc) then
    Result := ResemblesProc(AText, AOther)
  else
    Result := False;
end;

function ContainsText(const AText, ASubText: string): Boolean;
begin
  Result := AnsiContainsText(AText, ASubText);
end;

function MatchText(const AText: string; const AValues: array of string): Boolean;
begin
  Result := AnsiMatchText(AText, AValues);
end;

function IndexText(const AText: string; const AValues: array of string): Integer;
begin
  Result := AnsiIndexText(AText, AValues);
end;

function ContainsStr(const AText, ASubText: string): Boolean;
begin
  Result := AnsiContainsStr(AText, ASubText);
end;

Function StartsStr(const ASubText, AText: string): Boolean;

begin
  Result := AnsiStartsStr(AText, ASubText);
end;

Function EndsStr(const ASubText, AText: string): Boolean;

begin
  Result := AnsiEndsStr(AText, ASubText);
end;


initialization
  AnsiResemblesProc:= @SoundexProc;
  ResemblesProc:=@SoundexProc;

end.
