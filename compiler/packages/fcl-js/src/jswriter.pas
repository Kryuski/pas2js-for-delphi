{ *********************************************************************
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2016 Michael Van Canneyt.
       
    Javascript minifier
            
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
                   
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
                                
  **********************************************************************}
unit jswriter;

{$IFDEF Pas2JS}
{$i fcl-js.inc}
{$ELSE}
{$I delphi_defines.inc}
{$ENDIF}

{ $DEFINE DEBUGJSWRITER}

interface

uses
  {$ifdef pas2js}
  JS,
  {$endif}
  SysUtils, Classes, jsbase, jstree;

type
  TJSWriterString = UnicodeString;
  TJSWriterChar = WideChar;
  {$ifndef pas2js}
  PJSWriterChar = PWideChar;
  {$endif}

  TTextWriter = class;

  TTextWriterWriting = procedure(Sender: TTextWriter) of object;

  { TTextWriter }

  TTextWriter = class(TObject)
  private
    FCurElement: TJSElement;
    FCurLine: Integer;
    FCurColumn: Integer;
    FLineBreak: string;
    FOnWriting: TTextWriterWriting;
  protected
    function DoWrite(const S: TJSWriterString): Integer; overload; virtual; abstract;
    {$ifdef FPC_HAS_CPSTRING}
    function DoWrite(const S: RawByteString): Integer; overload; virtual; abstract;
    {$endif}
    procedure SetCurElement(const AValue: TJSElement); virtual;
    procedure Writing; virtual; // called before adding new characters
  public
    // All functions return the number of bytes copied to output stream.
    constructor Create;
    {$ifdef FPC_HAS_CPSTRING}
    function Write(const S: RawByteString): Integer; overload;
    function WriteLn(const S: RawByteString): Integer; overload;
    {$endif}
    function Write(const S: TJSWriterString): Integer; overload;
    function WriteLn(const S: TJSWriterString): Integer; overload;
    function Write(const Fmt: TJSWriterString; Args : Array of const): Integer; overload;
    function WriteLn(const Fmt: TJSWriterString; Args : Array of const): Integer; overload;
    function Write(const Args : Array of const): Integer; overload;
    function WriteLn(const Args : Array of const): Integer; overload;
    property CurLine: Integer read FCurLine write FCurLine;
    property CurColumn: Integer read FCurColumn write FCurColumn;// char index, not codepoint
    property CurElement: TJSElement read FCurElement write SetCurElement;
    property OnWriting: TTextWriterWriting read FOnWriting write FOnWriting;
    Property LineBreak: string read FLineBreak write FLineBreak;
  end;

  {$ifdef HasFileWriter}
  { TFileWriter }

  TFileWriter = class(TTextWriter)
  protected
    {$ifdef NodeJS}
    {$else}
    FFile: TFileStream;
    {$endif}
    FFileName: string;
    function DoWrite(const S: TJSWriterString): Integer; override;
    {$ifdef FPC_HAS_CPSTRING}
    function DoWrite(const S: RawByteString): Integer; override;
    {$endif}
  public
    constructor Create(const AFileName: string); reintroduce;
    destructor Destroy; override;
    procedure Flush;
    procedure Close;
    property FileName: string read FFileName;
  end;
  {$endif}

  TBufferWriter_Buffer = array of {$ifdef fpc}TJSWriterChar{$else}string{$endif};

  { TBufferWriter }

  TBufferWriter = class(TTextWriter)
  private type
    TBuffer = TBufferWriter_Buffer;
  private
    FBufPos,
    FCapacity: Cardinal;
    FBuffer: TBuffer;
    function GetAsString: TJSWriterString;
    function GetBufferLength: Integer;
    function GetCapacity: Cardinal;
    procedure SetAsString(const AValue: TJSWriterString);
    procedure SetCapacity(AValue: Cardinal);
  protected
    function DoWrite(const S: TJSWriterString): Integer; override;
  public
    constructor Create(const ACapacity: Cardinal); reintroduce;
    {$ifdef fpc}
    procedure SaveToFile(const AFileName: string);
    {$endif}
    property Buffer: TBufferWriter_Buffer read FBuffer;
    property BufferLength: Integer read GetBufferLength;
    property Capacity: Cardinal read GetCapacity write SetCapacity;
    property AsString: TJSWriterString read GetAsString write SetAsString;
  end;

  TJSEscapeQuote = (
    jseqSingle,
    jseqDouble,
    jseqBoth
    );

  { TJSWriter }

  TWriteOption = (woCompact,
                  woTabIndent,
                  woEmptyStatementAsComment,
                  woQuoteElementNames,
                  woCompactArrayLiterals,
                  woCompactObjectLiterals,
                  woCompactArguments);
  TWriteOptions = Set of TWriteOption;

  TJSWriter = class
  private
    FCurIndent: Integer;
    FFreeWriter: Boolean;
    FIndentChar: Char;
    FIndentSize: Byte;
    FLastChar: WideChar;
    FLinePos: Integer;
    FOptions: TWriteOptions;
    FSkipCurlyBrackets: Boolean;
    FSkipRoundBrackets: Boolean;
    FWriter: TTextWriter;
    procedure SetOptions(AValue: TWriteOptions);
    procedure InternalWrite(const S: TJSWriterString);
  protected
    // Helper routines
    procedure Error(const Msg: TJSWriterString); overload;
    procedure Error(const Fmt: TJSWriterString; Args: array of const); overload;
    procedure WriteIndent; overload;
    procedure Write(const S: TJSWriterString);
    procedure WriteLn(const S: TJSWriterString);
    // one per type of statement
    procedure WriteValue(V: TJSValue);  virtual;
    procedure WriteRegularExpressionLiteral(El: TJSRegularExpressionLiteral);
    procedure WriteVariableStatement(El: TJSVariableStatement);
    procedure WriteEmptyBlockStatement(El: TJSEmptyBlockStatement); virtual;
    procedure WriteEmptyStatement(El: TJSEmptyStatement);virtual;
    Procedure WriteDebuggerStatement(E: TJSDebuggerStatement) ;virtual;
    procedure WriteLiteral(El: TJSLiteral);virtual;
    procedure WriteArrayLiteral(El: TJSArrayLiteral);virtual;
    procedure WriteObjectLiteral(El: TJSObjectLiteral);virtual;
    procedure WriteMemberExpression(El: TJSMemberExpression);virtual;
    procedure WriteCallExpression(El: TJSCallExpression);virtual;
    procedure WriteSwitchStatement(El: TJSSwitchStatement);virtual;
    procedure WriteUnary(El: TJSUnary);virtual;
    procedure WriteAssignStatement(El: TJSAssignStatement);virtual;
    procedure WriteForInStatement(El: TJSForInStatement);virtual;
    procedure WriteWhileStatement(El: TJSWhileStatement);virtual;
    Procedure WriteImportStatement(El: TJSImportStatement);virtual;
    Procedure WriteExportStatement(El: TJSExportStatement);virtual;
    procedure WriteForStatement(El: TJSForStatement);virtual;
    procedure WriteIfStatement(El: TJSIfStatement);virtual;
    procedure WriteSourceElements(El: TJSSourceElements);virtual;
    procedure WriteStatementList(El: TJSStatementList);virtual;
    procedure WriteTryStatement(El: TJSTryStatement);virtual;
    procedure WriteVarDeclaration(El: TJSVarDeclaration);virtual;
    procedure WriteWithStatement(El: TJSWithStatement);virtual;
    procedure WriteVarDeclarationList(El: TJSVariableDeclarationList);virtual;
    procedure WriteConditionalExpression(El: TJSConditionalExpression);virtual;
    procedure WriteFunctionBody(El: TJSFunctionBody);virtual;
    procedure WriteFunctionDeclarationStatement(El: TJSFunctionDeclarationStatement);virtual;
    procedure WriteLabeledStatement(El: TJSLabeledStatement);virtual;
    procedure WriteReturnStatement(El: TJSReturnStatement);virtual;
    procedure WriteTargetStatement(El: TJSTargetStatement);virtual;
    procedure WriteFuncDef(FD: TJSFuncDef);virtual;
    procedure WritePrimaryExpression(El: TJSPrimaryExpression);virtual;
    procedure WriteBinary(El: TJSBinary);virtual;
    function IsEmptyStatement(El: TJSElement): boolean;
    function HasLineEnding(El: TJSElement): boolean;
  public
    function EscapeString(const S: TJSString; Quote: TJSEscapeQuote = jseqDouble): TJSString;
    constructor Create(AWriter: TTextWriter); overload;
    {$ifdef HasFileWriter}
    constructor Create(const AFileName: string); overload;
    {$endif}
    destructor Destroy; override;
    procedure WriteJS(El: TJSElement);
    procedure Indent;
    procedure Undent;
    property Writer: TTextWriter read FWriter;
    property Options: TWriteOptions read FOptions write SetOptions;
    property IndentSize: Byte read FIndentSize write FIndentSize;
    property LastChar: WideChar read FLastChar;
    property SkipCurlyBrackets: Boolean read FSkipCurlyBrackets write FSkipCurlyBrackets;
    property SkipRoundBrackets: Boolean read FSkipRoundBrackets write FSkipRoundBrackets;
  end;
  EJSWriter = class(Exception);

Function QuoteJSString(const S: TJSString; Quote: TJSChar = #0): TJSString;

implementation

{$IFNDEF Pas2JS}
uses
  FPCTypes, StrUtils;
{$ENDIF}

Resourcestring
  SErrUnknownJSClass = 'Unknown javascript element class: %s';
  SErrNilNode = 'Nil node in Javascript';

function QuoteJSString(const S: TJSString; Quote: TJSChar): TJSString;
var
  i, j, Count: Integer;
begin
  if Quote=#0 then begin
    if Pos('"',S)>0 then
      Quote:=''''
    else
      Quote:='"';
    end;
  Result := '' + Quote;
  Count := length(S);
  i := 0;
  j := 0;
  while i < Count do begin
    inc(i);
    if S[i] = Quote then begin
      Result := Result + copy(S, 1 + j, i - j) + Quote;
      j := i;
    end;
  end;
  if i <> j then
    Result := Result + copy(S, 1 + j, i - j);
  Result := Result + Quote;
end;

{ TBufferWriter }

function TBufferWriter.GetBufferLength: Integer;
begin
  Result := FBufPos;
end;

function TBufferWriter.GetAsString: TJSWriterString;
begin
  {$ifdef pas2js}
  if FBufPos<Length(FBuffer) then
    TJSArray(FBuffer).Length := FBufPos;
  Result := TJSArray(FBuffer).join('');
  {$else}
  Result := '';
  SetLength(Result, BufferLength);
  if BufferLength>0 then
    Move(FBuffer[0], Result[1], BufferLength*SizeOf(TJSWriterChar));
  {$endif}
end;

function TBufferWriter.GetCapacity: Cardinal;
begin
  Result := Length(FBuffer);
end;

procedure TBufferWriter.SetAsString(const AValue: TJSWriterString);
begin
  {$ifdef pas2js}
  SetLength(FBuffer,0);
  FCapacity := 0;
  {$endif}
  FBufPos := 0;
  DoWrite(AValue);
end;

procedure TBufferWriter.SetCapacity(AValue: Cardinal);
begin
  if FCapacity=AValue then Exit;
  SetLength(FBuffer,AValue);
  if (FBufPos>Capacity) then
    FBufPos := Capacity;
end;

function TBufferWriter.DoWrite(const S: TJSWriterString): Integer;
{$ifdef pas2js}
begin
  Result := Length(S)*2;
  if Result=0 then exit;
  TJSArray(FBuffer).push(S);
  inc(FBufPos);
  FCapacity := FBufPos;
end;
{$else}
var
  DesLen, MinLen: Integer;
begin
  Result := Length(S);
  if Result = 0 then exit;
  MinLen := Integer(FBufPos) + Result;
  if MinLen > Integer(Capacity) then begin
    DesLen := (FCapacity*3) div 2;
    if DesLen > MinLen then
      MinLen := DesLen;
    Capacity := MinLen;
  end;
  Move(S[1], FBuffer[FBufPos], Result*SizeOf(TJSWriterChar));
  Inc(FBufPos, Result);
end;
{$endif}

constructor TBufferWriter.Create(const ACapacity: Cardinal);
begin
  inherited Create;
  Capacity := ACapacity;
end;

{$ifdef fpc}
procedure TBufferWriter.SaveToFile(const AFileName: string);
var
  f: file;
begin
  Assign(f, AFileName);
  Rewrite(f, 1);
  try
    BlockWrite(f, FBuffer[1], FBufPos);
  finally
    Close(f);
  end;
end;
{$endif}

{ TJSWriter }
{AllowWriteln}

procedure TJSWriter.SetOptions(AValue: TWriteOptions);
begin
  if FOptions=AValue then Exit;
  FOptions := AValue;
  If woTabIndent in FOptions then
    FIndentChar := #9
  else
    FIndentChar := ' ';
end;

procedure TJSWriter.Error(const Msg: TJSWriterString);
begin
  Raise EJSWriter.Create(Msg);
end;

procedure TJSWriter.Error(const Fmt: TJSWriterString;
  Args: array of const);
begin
  Raise EJSWriter.CreateFmt(Fmt,Args);
end;

procedure TJSWriter.InternalWrite(const S: TJSWriterString);
begin
  if S = '' then
    Exit;
  FLinePos := FLinePos + Writer.Write(S);
  FLastChar := S[Length(S)];
end;

procedure TJSWriter.WriteIndent;
begin
  If (FLinePos=0) and (FCurIndent>0) then
    InternalWrite(StringOfChar(FIndentChar,FCurIndent));
end;

procedure TJSWriter.Indent;
begin
  Inc(FCurIndent,FIndentSIze);
end;

procedure TJSWriter.Undent;
begin
  if (FCurIndent>=FIndentSIze) then
    Dec(FCurIndent,FIndentSIze)
  else
    FCurIndent := 0;
end;

procedure TJSWriter.Write(const S: TJSWriterString);
begin
  //system.writeln('TJSWriter.Write TJSWriterString=',S);
  WriteIndent;
  InternalWrite(S);
end;

procedure TJSWriter.WriteLn(const S: TJSWriterString);
begin
  //system.writeln('TJSWriter.WriteLn TJSWriterString=',S);
  WriteIndent;
  InternalWrite(S + sLineBreak);
  FLinePos := 0;
end;

function TJSWriter.EscapeString(const S: TJSString; Quote: TJSEscapeQuote
  ): TJSString;
var
  I,J,L: Integer;
  R: TJSString;
  c: WideChar;
begin
  //system.writeln('TJSWriter.EscapeString "',S,'"');
  I := 1;
  J := 1;
  R := '';
  L := Length(S);
  While I<=L do
    begin
    c := S[I];
    if (c in [#0..#31,'"','''','/','\'])
        or (c>=#$ff00) or ((c>=#$D800) and (c<=#$DFFF)) then
      begin
      R := R+Copy(S,J,I-J);
      Case c of
        '\': R := R+'\\';
        '/': R := R+'\/';
        '"': if Quote=jseqSingle then R := R+'"' else R := R+'\"';
        '''': if Quote=jseqDouble then R := R+'''' else R := R+'\''';
        #0..#7,#11,#14..#31: R := R+'\x'+TJSString(hexStr(ord(c),2));
        #8 : R := R+'\b';
        #9 : R := R+'\t';
        #10: R := R+'\n';
        #12: R := R+'\f';
        #13: R := R+'\r';
        #$D800..#$DFFF:
          begin
          if (I<L) then
            begin
            c := S[I+1];
            if (c>=#$D000) and (c<=#$DFFF) then
              begin
              inc(I,2); // surrogate, two char codepoint
              continue;
              end;
            // invalid UTF-16, cannot be encoded as UTF-8 -> encode as hex
            R := R+'\u'+TJSString(HexStr(ord(S[i]),4));
            end
          else
            // invalid UTF-16 at end of string, cannot be encoded as UTF-8 -> encode as hex
            R := R+'\u'+TJSString(HexStr(ord(c),4));
          end;
        #$FF00..#$FFFF: R := R+'\u'+TJSString(HexStr(ord(c),4));
      end;
      J := I+1;
      end;
    Inc(I);
    end;
  R := R+Copy(S,J,I-1);
  Result := R;
  //system.writeln('TJSWriter.EscapeString Result="',Result,'"');
end;

procedure TJSWriter.WriteValue(V: TJSValue);
const
  TabWidth = 4;

  function GetLineIndent(const S: TJSString; var p: Integer): Integer;
  var
    h, l: Integer;
  begin
    h := p;
    l := Length(S);
    Result := 0;
    while h<=l do
      begin
      case S[h] of
      #9: Result := Result+(TabWidth-Result mod TabWidth);
      ' ': inc(Result);
      else break;
      end;
      inc(h);
      end;
    p := h;
  end;

  function SkipToNextLineEnd(const S: TJSString; p: integer): integer;
  var
    l: SizeInt;
  begin
    l:=length(S);
    while (p<=l) and not (S[p] in [#10,#13]) do inc(p);
    Result:=p;
  end;

  function SkipToNextLineStart(const S: TJSString; p: Integer): Integer;
  var
    l: Integer;
  begin
    l := Length(S);
    while p<=l do
      begin
      case S[p] of
      #10,#13:
        begin
        if (p<l) and (S[p+1] in [#10,#13]) and (S[p]<>S[p+1]) then
          inc(p,2)
        else
          inc(p);
        break;
        end
      else inc(p);
      end;
      end;
    Result := p;
  end;

Var
  S , S2: string;
  JS: TJSString;
  p, StartP: Integer;
  MinIndent, CurLineIndent, j, Exp, Code: Integer;
  i: SizeInt;
  D, AsNumber: TJSNumber;
begin
  if V.CustomValue<>'' then begin
    JS := V.CustomValue;
    if JS='' then exit;
    p := SkipToNextLineStart(JS,1);
    if p>Length(JS) then begin
      // simple value
      Write(JS);
      exit;
    end;
    // multi line value
    // find minimum indent
    MinIndent := -1;
    repeat
      CurLineIndent := GetLineIndent(JS,p);
      if (MinIndent<0) or (MinIndent>CurLineIndent) then
        MinIndent := CurLineIndent;
      p := SkipToNextLineStart(JS,p);
    until p>Length(JS);
    // write value lines indented
    p := 1;
    GetLineIndent(JS,p); // the first line is already indented, skip
    repeat
      StartP := p;
      p := SkipToNextLineEnd(JS,StartP);
      Write(copy(JS,StartP,p-StartP));
      if p>Length(JS) then break;
      Write(sLineBreak);
      p:=SkipToNextLineStart(JS,p);
      CurLineIndent := GetLineIndent(JS,p);
      Write(StringOfChar(FIndentChar,FCurIndent+CurLineIndent-MinIndent));
    until false;
    exit;
  end;
  case V.ValueType of
    jstUNDEFINED: S := 'undefined';
    jstNull: s := 'null';
    jstBoolean: if V.AsBoolean then s := 'true' else s := 'false';
    jstString: begin
      JS := V.AsString;
      if Pos('"',JS)>0 then
        JS := ''''+EscapeString(JS,jseqSingle)+''''
      else
        JS := '"'+EscapeString(JS,jseqDouble)+'"';
      Write(JS);
      exit;
    end;
    jstNumber: begin
      AsNumber:=V.AsNumber;
      if (Frac(AsNumber)=0)
          and (AsNumber>=double(MinSafeIntDouble))
          and (AsNumber<=double(MaxSafeIntDouble)) then
        begin
        {$WARN IMPLICIT_STRING_CAST OFF}
        Str(Round(AsNumber),S);
        {$WARN IMPLICIT_STRING_CAST ON}
        end
      else
        begin
        {$WARN IMPLICIT_STRING_CAST OFF}
        Str(AsNumber,S);
        {$WARN IMPLICIT_STRING_CAST ON}
        if S[1]=' ' then Delete(S,1,1);
        i := Pos('E',S);
        if i>2 then begin
          // Delphi-generated float numbers has standard representation:
          // 1.00000000000000E-0012 --> 1E-12
          // 1.00000000000000E-0003 --> 0.001
          // 2.99999999999990E+0000 --> 2.9999999999999
          // 2.99999999999999E+0003 --> 2999.99999999999
          // 2.11199999999999E+0017 --> 2.11199999999999E+17
          j := i-2;
          case s[j] of
            '0': begin
              // check for 1.2340000000000001E...
              while (j>1) and (s[j]='0') do Dec(j);
              if s[j]='.' then Inc(j);
              S2 := LeftStr(S,j)+copy(S,i,Length(S));
              Val(S2,D,Code);
              if (Code=0) and (D=AsNumber) then
                S := S2;
              end;
            '9': begin
              // check for 1.234999999999991E...
              while (j>1) and (s[j]='9') do dec(j);
              // cut '99999'
              S2 := LeftStr(S,j)+copy(S,i,Length(S));
              if S[j]='.' then
                Insert('0',S2,j+1);
              // increment, e.g. 1.2999 -> 1.3
              repeat
                case S2[j] of
                  '0'..'8': begin
                    S2[j] := chr(ord(S2[j])+1);
                    break;
                  end;
                  '9':
                    S2[j] := '0';
                  '.': ;
                end;
                dec(j);
                if (j=0) or not (S2[j] in ['0'..'9','.']) then begin
                  // e.g. -9.999 became 0.0
                  Val(copy(S,i+1,Length(S)),Exp,Code);
                  if Code=0 then begin
                    S2 := '1E'+IntToStr(Exp+1);
                    if S[1]='-' then
                      S2 := '-'+S2;
                  end;
                  break;
                end;
              until false;
              Val(S2,D,Code);
              if (Code=0) and (D=AsNumber) then
                S := S2;
            end;
            else if s[i-1]='0' then
              begin
              // 1.2340E...
              S2:=LeftStr(S,i-2)+copy(S,i,length(S));
              val(S2,D,Code);
              if (Code=0) and (D=AsNumber) then
                S:=S2;
              end;
          end;
        end;
        // chomp default exponent E+000
        i := Pos('E',S);
        if i>0 then begin
          Val(copy(S,i+1,Length(S)),Exp,Code);
          if Code=0 then begin
            if Exp=0 then
              // 1.1E+000 -> 1.1
              Delete(S,i,Length(S))
            else if (Exp>=-6) and (Exp<=6) then begin
              // small exponent -> use notation without E (1E-0003 --> 0.001)
              Delete(S,i,Length(S));
              j := Pos('.',S);
              if j>0 then
                Delete(S,j,1)
              else begin
                j := 1;
                while not (S[j] in ['0'..'9']) do inc(j);
              end;
              if Exp<0 then begin
                // e.g. -1.2  E-1
                while Exp<0 do begin
                  if (j>1) and (S[j-1] in ['0'..'9']) then
                    dec(j)
                  else
                    Insert('0',S,j);
                  inc(Exp);
                end;
                Insert('.',S,j);
                if (j=1) or not (S[j-1] in ['0'..'9']) then
                  Insert('0',S,j);
                if S[Length(S)]='0' then
                  Delete(S,Length(S),1);
              end else begin
                // e.g. -1.2  E1
                while Exp>0 do begin
                  if (j<=Length(S)) and (S[j] in ['0'..'9']) then
                    inc(j)
                  else
                    Insert('0',S,j);
                  dec(Exp);
                end;
                if j<=Length(S) then
                  Insert('.',S,j);
              end;
            end else begin
              // e.g. 1.1E+0010  -> 1.1E10
              S := LeftStr(S,i)+IntToStr(Exp);
              if (i >= 4) and (s[i-1] = '0') and (s[i-2] = '.') then
                // e.g. 1.0E22 -> 1E22
                Delete(S, i-2, 2);
            end
          end;
        end;
      end;
    end;
    jstObject: ;
    jstReference: ;
    jstCompletion: ;
  end;
  if S='' then exit;
  case S[1] of
    '+': if FLastChar='+' then Write(' ');
    '-': if FLastChar='-' then Write(' ');
  end;
  Write(S);
end;

constructor TJSWriter.Create(AWriter: TTextWriter);
begin
  FWriter := AWriter;
  FIndentChar := ' ';
end;

{$ifdef HasFileWriter}
constructor TJSWriter.Create(const AFileName: string);
begin
  Create(TFileWriter.Create(AFileName));
  FFreeWriter := True;
end;
{$endif}

destructor TJSWriter.Destroy;
begin
  If FFreeWriter then
    begin
    FWriter.Free;
    FWriter := Nil;
    end;
  inherited Destroy;
end;

procedure TJSWriter.WriteFuncDef(FD: TJSFuncDef);

Var
  C: Boolean;
  I: Integer;
  A, LastEl: TJSElement;

begin
  LastEl := Writer.CurElement;
  C := (woCompact in Options);
  if fd.IsAsync then
    Write('async ');
  Write('function ');
  If (FD.Name<>'') then
    Write(FD.Name);
  Write('(');
  if Assigned(FD.Params) then
    For I := 0 to FD.Params.Count-1 do
      begin
      write(FD.Params[i]);
      if I<FD.Params.Count-1 then
        if C then Write(',') else Write (', ');
      end;
  Write(') {');
  if Not (C or FD.IsEmpty) then
    begin
    Writeln('');
    Indent;
    end;
  if Assigned(FD.Body) then
    begin
    FSkipCurlyBrackets := True;
    //writeln('TJSWriter.WriteFuncDef '+FD.Body.ClassName);
    WriteJS(FD.Body);
    A := FD.Body.A;
    If (Assigned(A))
        and (not (A is TJSStatementList))
        and (not (A is TJSSourceElements))
        and (not (A is TJSEmptyBlockStatement))
    then
      begin
      if FLastChar<>';' then
        Write(';');
      if C then
        Write(' ')
      else
        Writeln('');
      end;
    end;
  Writer.CurElement := LastEl;
  if C then
    Write('}')
  else
    begin
    Undent;
    Write('}'); // do not writeln
    end;
end;

procedure TJSWriter.WriteEmptyBlockStatement(El: TJSEmptyBlockStatement);
begin
  if El=nil then ;
  if woCompact in Options then
    Write('{}')
  else
    begin
    Writeln('{');
    Write('}');
    end;
end;

procedure TJSWriter.WriteEmptyStatement(El: TJSEmptyStatement);
begin
  if El=nil then ;
  if woEmptyStatementAsComment in Options then
    Write('/* Empty statement */')
end;

procedure TJSWriter.WriteDebuggerStatement(E: TJSDebuggerStatement);
begin
  if E=nil then ;
  Write('debugger');
end;

procedure TJSWriter.WriteRegularExpressionLiteral(
  El: TJSRegularExpressionLiteral);

begin
  Write('/');
  Write(EscapeString(El.Pattern.AsString,jseqBoth));
  Write('/');
  If Assigned(El.PatternFlags) then
    Write(EscapeString(El.PatternFlags.AsString,jseqBoth));
end;

procedure TJSWriter.WriteLiteral(El: TJSLiteral);
begin
  WriteValue(El.Value);
end;

procedure TJSWriter.WritePrimaryExpression(El: TJSPrimaryExpression);
begin
  if El is TJSPrimaryExpressionThis then
    Write('this')
  else if El is TJSPrimaryExpressionIdent then
    Write(TJSPrimaryExpressionIdent(El).Name)
  else
    Error(SErrUnknownJSClass,[El.ClassName]);
end;

procedure TJSWriter.WriteArrayLiteral(El: TJSArrayLiteral);
var
  chars: array[Boolean] of string;
  i,c: Integer;
  isArgs,WC , MultiLine: Boolean;
  bc: string;
begin
  chars[False] := '[]';
  chars[True] := '()';
  isArgs := El is TJSArguments;
  bc := chars[isArgs];
  c := El.Elements.Count-1;
  if c=-1 then begin
    Write(bc);
    Exit;
  end;
  WC := (woCompact in Options) or
    ((Not isArgs) and (woCompactArrayLiterals in Options)) or
    (isArgs and (woCompactArguments in Options)) ;
  MultiLine := (not WC) and (c>4);
  if MultiLine then begin
    Writeln(bc[1]);
    Indent;
  end else
    Write(bc[1]);
  for I := 0 to c do begin
    FSkipRoundBrackets := true;
    WriteJS(El.Elements[i].Expr);
    if I<c then
      if WC then
        Write(',')
      else if MultiLine then
        Writeln(',')
      else
        Write(', ');
  end;
  if MultiLine then begin
    Writeln('');
    Undent;
  end;
  Writer.CurElement := El;
  Write(bc[2]);
end;


procedure TJSWriter.WriteObjectLiteral(El: TJSObjectLiteral);
var
  i,C: Integer;
  QE,WC: Boolean;
  S: TJSString;
  Prop: TJSObjectLiteralElement;
begin
  C := El.Elements.Count-1;
  QE := (woQuoteElementNames in Options);
  if C=-1 then
    begin
    Write('{}');
    Exit;
    end;
  WC := (woCompact in Options) or (woCompactObjectLiterals in Options);
  if WC then
    Write('{')
  else
    begin
    Writeln('{');
    Indent;
    end;
  For I := 0 to C do
   begin
   Prop := El.Elements[i];
   Writer.CurElement := Prop.Expr;
   S := Prop.Name;
   if QE or not IsValidJSIdentifier(S) then begin
     if (length(S)>1)
         and (((S[1]='"') and (S[length(S)]='"'))
           or ((S[1]='''') and (S[length(S)]=''''))) then
       // already quoted
     else
       S:=QuoteJSString(s);
   end;
   Write(S+': ');
   Indent;
   FSkipRoundBrackets := true;
   WriteJS(Prop.Expr);
   if I<C then
     if WC then Write(', ') else Writeln(',');
   Undent;
   end;
  FSkipRoundBrackets := false;
  if not WC then
    begin
    Writeln('');
    Undent;
    end;
  Writer.CurElement := El;
  Write('}');
end;

procedure TJSWriter.WriteMemberExpression(El: TJSMemberExpression);

var
  MExpr: TJSElement;
  Args: TJSArguments;
begin
  if El is TJSNewMemberExpression then
    Write('new ');
  MExpr := El.MExpr;
  if (MExpr is TJSPrimaryExpression)
      or (MExpr is TJSDotMemberExpression)
      or (MExpr is TJSBracketMemberExpression)
      // Note: new requires brackets in this case: new (a())()
      or ((MExpr is TJSCallExpression) and not (El is TJSNewMemberExpression))
      or (MExpr is TJSLiteral) then
    begin
    WriteJS(MExpr);
    Writer.CurElement := El;
    end
  else
    begin
    Write('(');
    WriteJS(MExpr);
    Writer.CurElement := El;
    Write(')');
    end;
  if El is TJSDotMemberExpression then
    begin
    Write('.');
    Write(TJSDotMemberExpression(El).Name);
    end
  else if El is TJSBracketMemberExpression then
    begin
    write('[');
    FSkipRoundBrackets := true;
    WriteJS(TJSBracketMemberExpression(El).Name);
    Writer.CurElement := El;
    FSkipRoundBrackets := false;
    write(']');
    end
  else if (El is TJSNewMemberExpression) then
    begin
    Args := TJSNewMemberExpression(El).Args;
    if Assigned(Args) then
      begin
      Writer.CurElement := Args;
      WriteArrayLiteral(Args);
      end
    else
      Write('()');
    end;
end;

procedure TJSWriter.WriteCallExpression(El: TJSCallExpression);

begin
  WriteJS(El.Expr);
  if Assigned(El.Args) then
    begin
    Writer.CurElement := El.Args;
    WriteArrayLiteral(El.Args);
    end
  else
    begin
    Writer.CurElement := El;
    Write('()');
    end;
end;

procedure TJSWriter.WriteUnary(El: TJSUnary);
Var
  S: string;
begin
  FSkipRoundBrackets := false;
  S := El.PreFixOperator;
  if (S<>'') then
    begin
    case S[1] of
    '+': if FLastChar='+' then Write(' ');
    '-': if FLastChar='-' then Write(' ');
    end;
    Write(S);
    end;
  WriteJS(El.A);
  S:=El.PostFixOperator;
  if (S<>'') then
    begin
    Writer.CurElement:=El;
    case S[1] of
    '+': if FLastChar='+' then Write(' ');
    '-': if FLastChar='-' then Write(' ');
    end;
    Write(S);
    end;
end;

procedure TJSWriter.WriteStatementList(El: TJSStatementList);

Var
  C : Boolean;
  LastEl: TJSElement;
  ElStack: array of TJSElement;
  ElStackIndex: integer;

  procedure WriteNonListEl(CurEl: TJSElement);
  begin
    if IsEmptyStatement(CurEl) then exit;
    if (LastEl<>nil) then
      begin
      if FLastChar<>';' then
        Write(';');
      if C then
        Write(' ')
      else
        Writeln('');
      end;
    WriteJS(CurEl);
    LastEl:=CurEl;
  end;

  procedure Push(CurEl: TJSElement);
  begin
    if CurEl=nil then exit;
    if ElStackIndex=length(ElStack) then
      SetLength(ElStack,ElStackIndex+8);
    ElStack[ElStackIndex]:=CurEl;
    inc(ElStackIndex);
  end;

  function Pop: TJSElement;
  begin
    if ElStackIndex=0 then exit(nil);
    dec(ElStackIndex);
    Result:=ElStack[ElStackIndex];
  end;

var
  B : Boolean;
  CurEl: TJSElement;
  List: TJSStatementList;
begin
  //write('TJSWriter.WriteStatementList '+BoolToStr(FSkipCurlyBrackets,true));
  //if El.A<>nil then write(' El.A='+El.A.ClassName) else write(' El.A=nil');
  //if El.B<>nil then write(' El.B='+El.B.ClassName) else write(' El.B=nil');
  //writeln(' ');

  C:=(woCompact in Options);
  B:= Not FSkipCurlyBrackets;
  FSkipCurlyBrackets:=True;
  if B then
    begin
    Write('{');
    Indent;
    if not C then writeln('');
    end;

  // traverse statementlist using a heap stack to avoid large stack depths
  LastEl:=nil;
  ElStackIndex:=0;
  CurEl:=El;
  while CurEl<>nil do
    begin
    if CurEl is TJSStatementList then
      begin
      List:=TJSStatementList(CurEl);
      if List.A is TJSStatementList then
        begin
        Push(List.B);
        CurEl:=List.A;
        end
      else
        begin
        WriteNonListEl(List.A);
        if List.B is TJSStatementList then
          CurEl:=List.B
        else
          begin
          WriteNonListEl(List.B);
          CurEl:=nil;
          end;
        end;
      end
    else
      begin
      WriteNonListEl(CurEl);
      CurEl:=nil;
      end;
    if CurEl=nil then
      CurEl:=Pop;
    end;

  if (LastEl<>nil) and not C then
    if FLastChar=';' then
      writeln('')
    else
      writeln(';');

  if B then
    begin
    Undent;
    Writer.CurElement := El;
    Write('}'); // do not writeln
    end;
end;

procedure TJSWriter.WriteWithStatement(El: TJSWithStatement);
begin
   Write('with (');
   FSkipRoundBrackets := true;
   WriteJS(El.A);
   FSkipRoundBrackets := false;
   Writer.CurElement := El;
   if (woCompact in Options) then
     Write(') ')
   else
     WriteLn(')');
   Indent;
   WriteJS(El.B);
   Undent;
end;

procedure TJSWriter.WriteVarDeclarationList(El: TJSVariableDeclarationList);

begin
  WriteJS(El.A);
  If Assigned(El.B) then
    begin
    Write(', ');
    WriteJS(El.B);
    end;
end;

procedure TJSWriter.WriteBinary(El: TJSBinary);
var
  ElC: TClass;
  S : String;

  procedure WriteRight(Bin: TJSBinary);
  begin
    FSkipRoundBrackets:=(Bin.B.ClassType=ElC)
          and ((ElC=TJSLogicalOrExpression)
            or (ElC=TJSLogicalAndExpression));
    Write(S);
    WriteJS(Bin.B);
    Writer.CurElement:=Bin;
  end;

Var
  AllowCompact, WithBrackets: Boolean;
  Left: TJSElement;
  SubBin: TJSBinaryExpression;
  Binaries: TJSElementArray;
  BinariesCnt: integer;
begin
  {$IFDEF VerboseJSWriter}
  System.writeln('TJSWriter.WriteBinary SkipRoundBrackets=',FSkipRoundBrackets);
  {$ENDIF}
  WithBrackets:=not FSkipRoundBrackets;
  if WithBrackets then
    Write('(');
  FSkipRoundBrackets:=false;
  ElC:=El.ClassType;
  Left:=El.A;
  AllowCompact:=False;

  S:='';
  if (El is TJSBinaryExpression) then
    begin
    S:=TJSBinaryExpression(El).OperatorString;
    AllowCompact:=TJSBinaryExpression(El).AllowCompact;
    end;
  If Not (AllowCompact and (woCompact in Options)) then
    begin
    if El is TJSCommaExpression then
      S:=S+' '
    else
      S:=' '+S+' ';
    end;

  if (Left is TJSBinaryExpression)
      and (Left.ClassType=ElC)
      and ((ElC=TJSLogicalOrExpression)
        or (ElC=TJSLogicalAndExpression)
        or (ElC=TJSBitwiseAndExpression)
        or (ElC=TJSBitwiseOrExpression)
        or (ElC=TJSBitwiseXOrExpression)
        or (ElC=TJSAdditiveExpressionPlus)
        or (ElC=TJSAdditiveExpressionMinus)
        or (ElC=TJSMultiplicativeExpressionMul)) then
    begin
    // handle left handed multi add without stack
    SetLength(Binaries{%H-},8);
    BinariesCnt:=0;
    while Left is TJSBinaryExpression do
      begin
      SubBin:=TJSBinaryExpression(Left);
      if SubBin.ClassType<>ElC then break;
      if BinariesCnt=length(Binaries) then
        SetLength(Binaries,BinariesCnt*2);
      Binaries[BinariesCnt]:=SubBin;
      inc(BinariesCnt);
      Left:=SubBin.A;
      end;

    WriteJS(Left);
    Writer.CurElement:=El;

    while BinariesCnt>0 do
      begin
      dec(BinariesCnt);
      WriteRight(TJSBinaryExpression(Binaries[BinariesCnt]));
      end;
    end
  else
    begin;
    WriteJS(Left);
    Writer.CurElement:=El;
    end;
  WriteRight(El);
  if WithBrackets then
    Write(')');
end;

function TJSWriter.IsEmptyStatement(El: TJSElement): boolean;
begin
  if (El=nil) then
    exit(true);
  if (El.ClassType=TJSEmptyStatement) and not (woEmptyStatementAsComment in Options) then
    exit(true);
  Result := false;
end;

function TJSWriter.HasLineEnding(El: TJSElement): boolean;
begin
  if El<>nil then
    begin
    if (El.ClassType=TJSStatementList) or (El.ClassType=TJSSourceElements) then
      exit(true);
    end;
  Result := false;
end;

procedure TJSWriter.WriteConditionalExpression(El: TJSConditionalExpression);

var
  NeedBrackets: Boolean;
begin
  NeedBrackets := true;
  if NeedBrackets then
    begin
    write('(');
    FSkipRoundBrackets := true;
    end;
  WriteJS(El.A);
  write(' ? ');
  if El.B<>nil then
    WriteJS(El.B);
  write(' : ');
  if El.C<>nil then
    WriteJS(El.C);
  if NeedBrackets then
    write(')');
end;

procedure TJSWriter.WriteAssignStatement(El: TJSAssignStatement);
var
  S: string;
begin
  WriteJS(El.LHS);
  Writer.CurElement := El;
  S := El.OperatorString;
  If Not (woCompact in Options) then
    S := ' '+S+' ';
  Write(S);
  FSkipRoundBrackets := true;
  WriteJS(El.Expr);
  FSkipRoundBrackets := false;
end;

procedure TJSWriter.WriteVarDeclaration(El: TJSVarDeclaration);

begin
  Write(El.Name);
  if Assigned(El.Init) then
    begin
    Write(' = ');
    FSkipRoundBrackets := true;
    WriteJS(El.Init);
    FSkipRoundBrackets := false;
    end;
end;

procedure TJSWriter.WriteIfStatement(El: TJSIfStatement);

var
  HasBTrue, C, HasBFalse, BTrueNeedBrackets: Boolean;
begin
  C := woCompact in Options;
  Write('if (');
  FSkipRoundBrackets := true;
  WriteJS(El.Cond);
  Writer.CurElement := El;
  FSkipRoundBrackets := false;
  Write(')');
  If Not C then
    Write(' ');
  HasBTrue := not IsEmptyStatement(El.BTrue);
  HasBFalse := not IsEmptyStatement(El.BFalse);
  if HasBTrue then
    begin
    // Note: the 'else' needs {} in front
    BTrueNeedBrackets := HasBFalse and not (El.BTrue is TJSStatementList)
      and not (El.BTrue is TJSEmptyBlockStatement);
    if BTrueNeedBrackets then
      if C then
        Write('{')
      else
        begin
        Writeln('{');
        Indent;
        end;
    WriteJS(El.BTrue);
    if BTrueNeedBrackets then
      if C then
        Write('}')
      else
        begin
        Undent;
        Writeln('}');
        end;
    end;
  if HasBFalse then
    begin
    Writer.CurElement := El.BFalse;
    if not HasBTrue then
      begin
      if C then
        Write('{}')
      else
        Writeln('{}');
      end
    else
      Write(' ');
    Write('else ');
    WriteJS(El.BFalse)
    end
  else
    Writer.CurElement := El;
end;

procedure TJSWriter.WriteForInStatement(El: TJSForInStatement);

begin
  Write('for (');
  if Assigned(El.LHS) then
    begin
    WriteJS(El.LHS);
    Writer.CurElement := El;
    end;
  Write(' in ');
  if Assigned(El.List) then
    begin
    WriteJS(El.List);
    Writer.CurElement := El;
    end;
  Write(') ');
  if Assigned(El.Body) then
    WriteJS(El.Body);
end;

procedure TJSWriter.WriteForStatement(El: TJSForStatement);

begin
  Write('for (');
  if Assigned(El.Init) then
    WriteJS(El.Init);
  Write('; ');
  if Assigned(El.Cond) then
    begin
    FSkipRoundBrackets := true;
    WriteJS(El.Cond);
    FSkipRoundBrackets := false;
    end;
  Write('; ');
  if Assigned(El.Incr) then
    WriteJS(El.Incr);
  Writer.CurElement := El;
  Write(') ');
  if Assigned(El.Body) then
    WriteJS(El.Body);
end;

procedure TJSWriter.WriteWhileStatement(El: TJSWhileStatement);


begin
  if El is TJSDoWhileStatement then
    begin
    Write('do ');
    if Assigned(El.Body) then
      begin
      FSkipCurlyBrackets := false;
      WriteJS(El.Body);
      Writer.CurElement := El;
      end;
    Write(' while (');
    If Assigned(El.Cond) then
      begin
      FSkipRoundBrackets := true;
      WriteJS(EL.Cond);
      Writer.CurElement := El;
      FSkipRoundBrackets := false;
      end;
    Write(')');
    end
  else
    begin
    Write('while (');
    If Assigned(El.Cond) then
      begin
      FSkipRoundBrackets := true;
      WriteJS(EL.Cond);
      Writer.CurElement := El;
      FSkipRoundBrackets := false;
      end;
    Write(') ');
    if Assigned(El.Body) then
      WriteJS(El.Body);
    end;
end;

procedure TJSWriter.WriteImportStatement(El: TJSImportStatement);

Var
  I : integer;
  N : TJSNamedImportElement;
  needFrom : Boolean;

begin
  Write('import ');
  needFrom:=False;
  if El.DefaultBinding<>'' then
    begin
    Write(El.DefaultBinding+' ');
    if (El.NameSpaceImport<>'') or El.HaveNamedImports then
      Write(', ');
    needFrom:=True;
    end;
  if El.NameSpaceImport<>''  then
    begin
    Write('* as '+El.NameSpaceImport+' ');
    needFrom:=True;
    end;
  if El.HaveNamedImports then
    begin
    needFrom:=True;
    Write('{ ');
    For I:=0 to EL.NamedImports.Count-1 do
      begin
      N:=EL.NamedImports[i];
      if I>0 then
        Write(', ');
      Write(N.Name+' ');
      if N.Alias<>'' then
        Write('as '+N.Alias+' ');
      end;
    Write('} ');
    end;
  if NeedFrom then
    Write('from ');
  write('"'+El.ModuleName+'"');
end;

procedure TJSWriter.WriteExportStatement(El: TJSExportStatement);
Var
  I : integer;
  N : TJSExportNameElement;

begin
  Write('export ');
  if El.IsDefault then
    Write('default ');
  if assigned(El.Declaration) then
    WriteJS(El.Declaration)
  else if (El.NameSpaceExport<>'') then
    begin
    if El.NameSpaceExport<>'*' then
      Write('* as '+El.NameSpaceExport)
    else
      Write('*');
    if El.ModuleName<>'' then
      Write(' from "'+El.ModuleName+'"');
    end
  else if El.HaveExportNames then
    begin
    Write('{ ');
    For I:=0 to El.ExportNames.Count-1 do
      begin
      N:=El.ExportNames[i];
      if I>0 then
        Write(', ');
      Write(N.Name);
      if N.Alias<>'' then
        Write(' as '+N.Alias);
      end;
    Write(' }');
    if El.ModuleName<>'' then
      Write(' from "'+El.ModuleName+'"');
    end;
end;

procedure TJSWriter.WriteSwitchStatement(El: TJSSwitchStatement);
var
  C: Boolean;
  I: Integer;
  EC: TJSCaseElement;
begin
  C := (woCompact in Options);
  Write('switch (');
  If Assigned(El.Cond) then
    begin
    FSkipRoundBrackets := true;
    WriteJS(El.Cond);
    Writer.CurElement := El;
    FSkipRoundBrackets := false;
    end;
  if C then
    Write(') {')
  else
    Writeln(') {');
  For I := 0 to El.Cases.Count-1 do
    begin
    EC := El.Cases[i];
    if EC=El.TheDefault then
      Write('default')
    else
      begin
      Writer.CurElement := EC.Expr;
      Write('case ');
      FSkipRoundBrackets := true;
      WriteJS(EC.Expr);
      FSkipRoundBrackets := false;
      end;
    if Assigned(EC.Body) then
      begin
      FSkipCurlyBrackets := true;
      If C then
        Write(': ')
      else
        Writeln(':');
      Indent;
      WriteJS(EC.Body);
      Undent;
      if (EC.Body is TJSStatementList) or (EC.Body is TJSEmptyBlockStatement) then
        begin
        if C then
          begin
          if I<El.Cases.Count-1 then
            Write(' ');
          end
        else
          Writeln('');
        end
      else if C then
        Write('; ')
      else
        Writeln(';');
      end
    else
      begin
      if C then
        Write(': ')
      else
        Writeln(':');
      end;
    end;
  Writer.CurElement := El;
  Write('}');
end;

procedure TJSWriter.WriteTargetStatement(El: TJSTargetStatement);
var
  TN: TJSString;
begin
  TN := El.TargetName;
  if (El is TJSForStatement) then
    WriteForStatement(TJSForStatement(El))
  else if (El is TJSSwitchStatement) then
    WriteSwitchStatement(TJSSwitchStatement(El))
  else if (El is TJSForInStatement) then
    WriteForInStatement(TJSForInStatement(El))
  else if El is TJSWhileStatement then
    WriteWhileStatement(TJSWhileStatement(El))
  else if (El is TJSContinueStatement) then
    begin
    if (TN<>'') then
      Write('continue '+TN)
    else
      Write('continue');
    end
  else if (El is TJSBreakStatement) then
    begin
   if (TN<>'') then
      Write('break '+TN)
    else
      Write('break');
    end
  else
    Error('Unknown target statement class: "%s"',[El.ClassName])
end;

procedure TJSWriter.WriteReturnStatement(El: TJSReturnStatement);

begin
  if El.Expr=nil then
    Write('return')
  else
    begin
    Write('return ');
    FSkipRoundBrackets := true;
    WriteJS(El.Expr);
    FSkipRoundBrackets := false;
    end;
end;

procedure TJSWriter.WriteLabeledStatement(El: TJSLabeledStatement);
begin
  if Assigned(El.TheLabel) then
    begin
    Write(El.TheLabel.Name);
    if woCompact in Options then
      Write(': ')
    else
      Writeln(':');
    end;
  // Target ??
  WriteJS(El.A);
end;

procedure TJSWriter.WriteTryStatement(El: TJSTryStatement);
var
  C: Boolean;
begin
  C := woCompact in Options;
  Write('try {');
  if not IsEmptyStatement(El.Block) then
    begin
    if Not C then writeln('');
    FSkipCurlyBrackets := True;
    Indent;
    WriteJS(El.Block);
    if (Not C) and (not (El.Block is TJSStatementList)) then writeln('');
    Undent;
    end;
  Writer.CurElement := El;
  Write('}');
  If (El is TJSTryCatchFinallyStatement) or (El is TJSTryCatchStatement) then
    begin
    Write(' catch');
    if El.Ident<>'' then Write(' ('+El.Ident+')');
    If C then
      Write(' {')
    else
      Writeln(' {');
    if not IsEmptyStatement(El.BCatch) then
      begin
      FSkipCurlyBrackets := True;
      Indent;
      WriteJS(El.BCatch);
      Undent;
      if (Not C) and (not (El.BCatch is TJSStatementList)) then writeln('');
      end;
    Writer.CurElement := El;
    Write('}');
    end;
  If (El is TJSTryCatchFinallyStatement) or (El is TJSTryFinallyStatement) then
    begin
    If C then
      Write(' finally {')
    else
      Writeln(' finally {');
    if not IsEmptyStatement(El.BFinally) then
      begin
      Indent;
      FSkipCurlyBrackets := True;
      WriteJS(El.BFinally);
      Undent;
      if (Not C) and (not (El.BFinally is TJSStatementList)) then writeln('');
      end;
    Writer.CurElement := El;
    Write('}');
    end;
end;

procedure TJSWriter.WriteFunctionBody(El: TJSFunctionBody);

begin
  //writeln('TJSWriter.WriteFunctionBody '+El.A.ClassName+' FSkipBrackets='+BoolToStr(FSkipCurlyBrackets,'true','false'));
  if not IsEmptyStatement(El.A) then
    WriteJS(El.A);
end;

procedure TJSWriter.WriteFunctionDeclarationStatement(
  El: TJSFunctionDeclarationStatement);

begin
  if Assigned(El.AFunction) then
    WriteFuncDef(El.AFunction);
end;

procedure TJSWriter.WriteSourceElements(El: TJSSourceElements);
var
  C: Boolean;

  procedure WriteElements(Elements: TJSElementNodes);
  Var
    I: Integer;
    E: TJSElement;
  begin
    if Elements=nil then exit;
    For I := 0 to Elements.Count-1 do
      begin
      E := Elements.Nodes[i].Node;
      WriteJS(E);
      if Not C then
        WriteLn(';')
      else
        if I<Elements.Count-1 then
          Write('; ')
        else
          Write(';')
      end;
  end;

begin
  C := (woCompact in Options);
  WriteElements(El.Vars);
  WriteElements(El.Functions);
  WriteElements(El.Statements);
end;

procedure TJSWriter.WriteVariableStatement(El: TJSVariableStatement);

Const
  Keywords : Array[TJSVarType] of string = ('var','let','const');

begin
  Write(Keywords[el.varType]+' ');
  FSkipRoundBrackets:=true;
  WriteJS(El.VarDecl);
end;

procedure TJSWriter.WriteJS(El: TJSElement);
var
  C: TClass;
begin
{$IFDEF DEBUGJSWRITER}
  if (El<>Nil) then
    system.Writeln('WriteJS: ',El.ClassName,' ',El.Line,',',El.Column)
  else
    system.Writeln('WriteJS: El = Nil');
{$ENDIF}
  Writer.CurElement := El;
  C := El.ClassType;
  if (C=TJSEmptyBlockStatement ) then
    WriteEmptyBlockStatement(TJSEmptyBlockStatement(El))
  else if (C=TJSEmptyStatement) then
    WriteEmptyStatement(TJSEmptyStatement(El))
  else if (C=TJSDebuggerStatement) then
    WriteDebuggerStatement(TJSDebuggerStatement(El))
  else if (C=TJSLiteral) then
    WriteLiteral(TJSLiteral(El))
  else if C.InheritsFrom(TJSPrimaryExpression) then
    WritePrimaryExpression(TJSPrimaryExpression(El))
  else if C.InheritsFrom(TJSArrayLiteral) then
    WriteArrayLiteral(TJSArrayLiteral(El))
  else if (C=TJSObjectLiteral) then
    WriteObjectLiteral(TJSObjectLiteral(El))
  else if C.InheritsFrom(TJSMemberExpression) then
    WriteMemberExpression(TJSMemberExpression(El))
  else if (C=TJSRegularExpressionLiteral) then
    WriteRegularExpressionLiteral(TJSRegularExpressionLiteral(El))
  else if (C=TJSCallExpression) then
    WriteCallExpression(TJSCallExpression(El))
  else if (C=TJSLabeledStatement) then // Before unary
    WriteLabeledStatement(TJSLabeledStatement(El))
  else if (C=TJSFunctionBody) then // Before unary
    WriteFunctionBody(TJSFunctionBody(El))
  else if (C=TJSVariableStatement) then // Before unary
    WriteVariableStatement(TJSVariableStatement(El))
  else if C.InheritsFrom(TJSUnary) then
    WriteUnary(TJSUnary(El))
  else if (C=TJSVariableDeclarationList) then
    WriteVarDeclarationList(TJSVariableDeclarationList(El)) // Must be before binary
  else if (C=TJSStatementList) then
    WriteStatementList(TJSStatementList(El)) // Must be before binary
  else if (C=TJSWithStatement) then
    WriteWithStatement(TJSWithStatement(El)) // Must be before binary
  else if C.InheritsFrom(TJSBinary) then
    WriteBinary(TJSBinary(El))
  else if (C=TJSConditionalExpression) then
    WriteConditionalExpression(TJSConditionalExpression(El))
  else if C.InheritsFrom(TJSAssignStatement) then
    WriteAssignStatement(TJSAssignStatement(El))
  else if (C=TJSVarDeclaration) then
    WriteVarDeclaration(TJSVarDeclaration(El))
  else if (C=TJSIfStatement) then
    WriteIfStatement(TJSIfStatement(El))
  else if (C=TJSImportStatement) then
    WriteImportStatement(TJSImportStatement(El))
  else if (C=TJSExportStatement) then
    WriteExportStatement(TJSExportStatement(El))
  else if C.InheritsFrom(TJSTargetStatement) then
    WriteTargetStatement(TJSTargetStatement(El))
  else if (C=TJSReturnStatement) then
    WriteReturnStatement(TJSReturnStatement(El))
  else if C.InheritsFrom(TJSTryStatement) then
    WriteTryStatement(TJSTryStatement(El))
  else if (C=TJSFunctionDeclarationStatement) then
    WriteFunctionDeclarationStatement(TJSFunctionDeclarationStatement(El))
  else if (C=TJSSourceElements) then
    WriteSourceElements(TJSSourceElements(El))
  else if El=Nil then
    Error(SErrNilNode)
  else
    Error(SErrUnknownJSClass,[El.ClassName]);
//  Write('/* '+El.ClassName+' */');
  FSkipCurlyBrackets := False;
end;
{AllowWriteln-}

{$ifdef HasFileWriter}
{ TFileWriter }

function TFileWriter.DoWrite(const S: TJSWriterString): Integer;
begin
  {$ifdef NodeJS}
  system.writeln('TFileWriter.DoWrite ToDo ', S);
  Result := 0;
  {$else}
  {$ifdef FPC_HAS_CPSTRING}
  Result := DoWrite(UTF8Encode(S));
  {$ELSE}
  Result := FFile.Write(S[1], Length(S)*SizeOf(TJSWriterChar))
  {$ENDIF}
  {$endif}
end;

{$ifdef FPC_HAS_CPSTRING}
function TFileWriter.DoWrite(const S: RawByteString): Integer;
begin
  Result := FFile.Write(S[1], Length(S))
end;
{$endif}

constructor TFileWriter.Create(const AFileName: string);
begin
  inherited Create;
  FFileName := AFileName;
  {$ifdef NodeJS}
  system.writeln('TFileWriter.Create ToDo ',AFileName);
  {$else}
  FFile := TFileStream.Create(AFileName, fmCreate);
  {$endif}
end;

destructor TFileWriter.Destroy;
begin
  Close;
  Inherited;
end;

procedure TFileWriter.Flush;
begin
  {$ifdef NodeJS}
  system.writeln('TFileWriter.Flush ToDO');
  {$else}
  {$endif}
end;

procedure TFileWriter.Close;
begin
  {$ifdef NodeJS}
  system.writeln('TFileWriter.DoWrite ToDo ');
  {$else}
  FreeAndNil(FFile);
  {$endif}
end;
{$endif}

{ TTextWriter }

procedure TTextWriter.SetCurElement(const AValue: TJSElement);
begin
  FCurElement := AValue;
end;

procedure TTextWriter.Writing;
begin
  if Assigned(OnWriting) then
    OnWriting(Self);
end;

constructor TTextWriter.Create;
begin
  FCurLine := 1;
  FCurColumn := 1;
  FLineBreak:=sLineBreak;
end;

{$ifdef FPC_HAS_CPSTRING}
function TTextWriter.Write(const S: TJSWriterString): Integer;
var
  p: PJSWriterChar;
  c: TJSWriterChar;
begin
  Result := 0;
  if S='' then exit;
  Writing;
  Result := DoWrite(S);
  p := PJSWriterChar(S);
  repeat
    c := p^;
    case c of
      #0:
        if p-PJSWriterChar(S) = Length(S){!!*SizeOf(TJSWriterChar)} then
          break
        else
          inc(FCurColumn);
      #10,#13: begin
        FCurColumn := 1;
        inc(FCurLine);
        inc(p);
        if (p^ in [#10,#13]) and (c<>p^) then inc(p);
        continue;
      end;
      else
        // ignore low/high surrogate, CurColumn is char index, not codepoint
        inc(FCurColumn);
    end;
    inc(p);
  until false;
end;
{$ELSE}
function TTextWriter.Write(const S: TJSWriterString): Integer;
var
  c: Char;
  l, p: Integer;
begin
  if S='' then exit;
  Writing;
  Result:=DoWrite(S);
  l:=length(S);
  p:=1;
  while p<=l do
    begin
    c:=S[p];
    case c of
    #10,#13:
      begin
      FCurColumn:=1;
      inc(FCurLine);
      inc(p);
      if (p<=l) and (S[p] in [#10,#13]) and (c<>S[p]) then inc(p);
      end;
    else
      // Note about UTF-8 multibyte chars: CurColumn is char index, not codepoint
      inc(FCurColumn);
      inc(p);
    end;
    end;
end;
{$ENDIF}

function TTextWriter.WriteLn(const S: TJSWriterString): Integer;
begin
  Result:=Write(S)+Write(LineBreak);
end;

{$ifdef FPC_HAS_CPSTRING}
function TTextWriter.Write(const S: RawByteString): Integer;
var
  c: AnsiChar;
  l, p: Integer;
begin
  Result := 0;
  if S='' then exit;
  Writing;
  Result := DoWrite(S);
  l := Length(S);
  p := 1;
  while p<=l do begin
    c := S[p];
    case c of
      #10,#13: begin
        FCurColumn := 1;
        inc(FCurLine);
        inc(p);
        if (p<=l) and (S[p] in [#10,#13]) and (c<>S[p]) then inc(p);
      end;
      else
        // Note about UTF-8 multibyte chars: CurColumn is char index, not codepoint
        inc(FCurColumn);
        inc(p);
    end;
  end;
end;

function TTextWriter.WriteLn(const S: RawByteString): Integer;
begin
  Result := Write(S) + Write(sLineBreak);
end;
{$endif}

function TTextWriter.Write(const Fmt: TJSWriterString;
  Args: array of const): Integer;
begin
  Result := Write(Format(Fmt,Args));
end;

function TTextWriter.WriteLn(const Fmt: TJSWriterString;
  Args: array of const): Integer;
begin
  Result := WriteLn(Format(Fmt,Args));
end;

function TTextWriter.Write(const Args: array of const): Integer;

var
  i: Integer;
  {$ifdef pas2js}
  v: jsvalue;
  {$else}
  v: TVarRec;
  {$endif}
  s: TJSWriterString;
begin
  Result := 0;
  For i := Low(Args) to High(Args) do begin
    v := Args[i];
    s := '';
    {$ifdef pas2js}
    case jsTypeOf(v) of
      'boolean':
        if v then s := 'true' else s := 'false';
      'number':
        if isInteger(v) then
          s := str(NativeInt(v))
        else
          s := str(Double(v));
      'string':
        s := string(v);
      else continue;
    end;
    Result := Result+Write(s);
    {$else}
    s := '';
    case v.VType of
       vtInteger      : {$WARN IMPLICIT_STRING_CAST OFF}Str(v.VInteger,s);{$WARN IMPLICIT_STRING_CAST ON}
       vtBoolean      : if v.VBoolean then s := 'true' else s := 'false';
       vtChar         : s := TJSWriterString(AnsiString(v.VChar));
       vtWideChar     : s := v.VWideChar;
       vtExtended     : {$WARN IMPLICIT_STRING_CAST OFF}Str(v.VExtended^,s);{$WARN IMPLICIT_STRING_CAST ON}
       vtString       : s := TJSWriterString(v.VString^);
       vtPChar        : s := TJSWriterString(AnsiString(v.VPChar^));
       vtPWideChar    : s := v.VPWideChar;
       vtAnsiString   : s := PChar(v.VAnsiString);
       vtCurrency     : {$WARN IMPLICIT_STRING_CAST OFF}Str(v.VCurrency^,s);{$WARN IMPLICIT_STRING_CAST ON}
       vtVariant      : s := v.VVariant^;
       vtWideString   : s := PWideChar(v.VWideString);
       vtInt64        : {$WARN IMPLICIT_STRING_CAST OFF}Str(v.VInt64^,s);{$WARN IMPLICIT_STRING_CAST ON}
       vtUnicodeString: s := PWideChar(v.VUnicodeString);
       //!!vtQWord        : Str(v.VQWord^,s);
    end;
    if s <> '' then
      Result := Result+Write(s)
    {$endif}
  end;
end;

function TTextWriter.WriteLn(const Args: array of const): Integer;
begin
  Result := Write(Args) + Writeln('');
end;

end.

