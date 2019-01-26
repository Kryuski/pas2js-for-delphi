{
    This file is part of the Free Component Library

    Pascal source parser
    Copyright (c) 2000-2005 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit PParser;

{$h+}
{$I pas2js_defines.inc}

interface

uses
  {$ifdef NODEJS}
  NodeJSFS,
  {$endif}
  SysUtils, Classes, PasTree, PScanner, FPCTypes;

{$ifdef fpc}
  {$IF FPC_FULLVERSION<30101}
    {$define EmulateArrayInsert}
  {$endif}
{$endif}

// message numbers
const
  nErrNoSourceGiven = 2001;
  nErrMultipleSourceFiles = 2002;
  nParserError = 2003;
  nParserErrorAtToken = 2004;
  nParserUngetTokenError = 2005;
  nParserExpectTokenError = 2006;
  nParserForwardNotInterface = 2007;
  nParserExpectVisibility = 2008;
  nParserStrangeVisibility = 2009;
  nParserExpectToken2Error = 2010;
  nParserExpectedCommaRBracket = 2011;
  nParserExpectedCommaSemicolon = 2012;
  nParserExpectedAssignIn = 2013;
  nParserExpectedCommaColon = 2014;
  nErrUnknownOperatorType = 2015;
  nParserOnlyOneArgumentCanHaveDefault = 2016;
  nParserExpectedLBracketColon = 2017;
  nParserExpectedSemiColonEnd = 2018;
  nParserExpectedConstVarID = 2019;
  nParserExpectedNested = 2020;
  nParserExpectedColonID = 2021;
  nParserSyntaxError = 2022;
  nParserTypeSyntaxError = 2023;
  nParserArrayTypeSyntaxError = 2024;
  nParserExpectedIdentifier = 2026;
  nParserNotAProcToken = 2026;
  nRangeExpressionExpected = 2027;
  nParserExpectCase = 2028;
  // free 2029;
  nLogStartImplementation = 2030;
  nLogStartInterface = 2031;
  nParserNoConstructorAllowed = 2032;
  nParserNoFieldsAllowed = 2033;
  nParserInvalidRecordVisibility = 2034;
  nErrRecordConstantsNotAllowed = 2035;
  nErrRecordMethodsNotAllowed = 2036;
  nErrRecordPropertiesNotAllowed = 2037;
  nErrRecordTypesNotAllowed = 2038;
  nParserTypeNotAllowedHere = 2039;
  nParserNotAnOperand = 2040;
  nParserArrayPropertiesCannotHaveDefaultValue = 2041;
  nParserDefaultPropertyMustBeArray = 2042;
  nParserUnknownProcedureType = 2043;
  nParserGenericArray1Element = 2044;
  nParserGenericClassOrArray = 2045;
  nParserDuplicateIdentifier = 2046;
  nParserDefaultParameterRequiredFor = 2047;
  nParserOnlyOneVariableCanBeInitialized = 2048;
  nParserExpectedTypeButGot = 2049;
  nParserPropertyArgumentsCanNotHaveDefaultValues = 2050;
  nParserExpectedExternalClassName = 2051;
  nParserNoConstRangeAllowed = 2052;
  nErrRecordVariablesNotAllowed = 2053;
  nParserResourcestringsMustBeGlobal = 2054;
  nParserOnlyOneVariableCanBeAbsolute = 2055;
  nParserXNotAllowedInY = 2056;
  nFileSystemsNotSupported = 2057;

// resourcestring patterns of messages
resourcestring
  SErrNoSourceGiven = 'No source file specified';
  SErrMultipleSourceFiles = 'Please specify only one source file';
  SParserError = 'Error';
  SParserErrorAtToken = '%s at token "%s" in file %s at line %d column %d';
  SParserUngetTokenError = 'Internal error: Cannot unget more tokens, history buffer is full';
  SParserExpectTokenError = 'Expected "%s"';
  SParserForwardNotInterface = 'The use of a FORWARD procedure modifier is not allowed in the interface';
  SParserExpectVisibility = 'Expected visibility specifier';
  SParserStrangeVisibility = 'Strange strict visibility encountered: "%s"';
  SParserExpectToken2Error = 'Expected "%s" or "%s"';
  SParserExpectedCommaRBracket = 'Expected "," or ")"';
  SParserExpectedCommaSemicolon = 'Expected "," or ";"';
  SParserExpectedAssignIn = 'Expected := or in';
  SParserExpectedCommaColon = 'Expected "," or ":"';
  SErrUnknownOperatorType = 'Unknown operator type: %s';
  SParserOnlyOneArgumentCanHaveDefault = 'A default value can only be assigned to 1 parameter';
  SParserExpectedLBracketColon = 'Expected "(" or ":"';
  SParserExpectedSemiColonEnd = 'Expected ";" or "End"';
  SParserExpectedConstVarID = 'Expected "const", "var" or identifier';
  SParserExpectedNested = 'Expected nested keyword';
  SParserExpectedColonID = 'Expected ":" or identifier';
  SParserSyntaxError = 'Syntax error';
  SParserTypeSyntaxError = 'Syntax error in type';
  SParserArrayTypeSyntaxError = 'Syntax error in array type';
  SParserExpectedIdentifier = 'Identifier expected';
  SParserNotAProcToken = 'Not a procedure or function token';
  SRangeExpressionExpected = 'Range expression expected';
  SParserExpectCase = 'Case label expression expected';
  // free for 2029
  SLogStartImplementation = 'Start parsing implementation section.';
  SLogStartInterface = 'Start parsing interface section';
  SParserNoConstructorAllowed = 'Constructors or Destructors are not allowed in Interfaces or Record helpers';
  SParserNoFieldsAllowedInX = 'Fields are not allowed in %s';
  SParserInvalidRecordVisibility = 'Records can only have public and (strict) private as visibility specifiers';
  SErrRecordConstantsNotAllowed = 'Record constants not allowed at this location.';
  SErrRecordVariablesNotAllowed = 'Record variables not allowed at this location.';
  SErrRecordMethodsNotAllowed = 'Record methods not allowed at this location.';
  SErrRecordPropertiesNotAllowed = 'Record properties not allowed at this location.';
  SErrRecordTypesNotAllowed = 'Record types not allowed at this location.';
  SParserTypeNotAllowedHere = 'Type "%s" not allowed here';
  SParserNotAnOperand = 'Not an operand: (%d: %s)';
  SParserArrayPropertiesCannotHaveDefaultValue = 'Array properties cannot have default value';
  SParserDefaultPropertyMustBeArray = 'The default property must be an array property';
  SParserUnknownProcedureType = 'Unknown procedure type "%d"';
  SParserGenericArray1Element = 'Generic arrays can have only 1 template element';
  SParserGenericClassOrArray = 'Generic can only be used with classes or arrays';
  SParserDuplicateIdentifier = 'Duplicate identifier "%s"';
  SParserDefaultParameterRequiredFor = 'Default parameter required for "%s"';
  SParserOnlyOneVariableCanBeInitialized = 'Only one variable can be initialized';
  SParserExpectedTypeButGot = 'Expected type, but got %s';
  SParserPropertyArgumentsCanNotHaveDefaultValues = 'Property arguments can not have default values';
  SParserExpectedExternalClassName = 'Expected external class name';
  SParserNoConstRangeAllowed = 'Const ranges are not allowed';
  SParserResourcestringsMustBeGlobal = 'Resourcestrings can be only static or global';
  SParserOnlyOneVariableCanBeAbsolute = 'Only one variable can be absolute';
  SParserXNotAllowedInY = '%s is not allowed in %s';
  SErrFileSystemNotSupported = 'No support for filesystems enabled';

type
  TPasScopeType = (
    stModule,  // e.g. unit, program, library
    stUsesClause,
    stTypeSection,
    stTypeDef, // e.g. a TPasType
    stResourceString, // e.g. TPasResString
    stProcedure, // also method, procedure, constructor, destructor, ...
    stProcedureHeader,
    stWithExpr, // calls BeginScope after parsing every WITH-expression
    stExceptOnExpr,
    stExceptOnStatement,
    stDeclaration, // e.g. a TPasProperty, TPasVariable, TPasArgument
    stAncestors, // the list of ancestors and interfaces of a class
    stInitialFinalization
    );
  TPasScopeTypes = set of TPasScopeType;

  TPasParserLogHandler = procedure (Sender: TObject; const Msg: string) of object;
  TPParserLogEvent = (pleInterface,pleImplementation);
  TPParserLogEvents = set of TPParserLogEvent;
  TPasParser = Class;

  { TPasTreeContainer }

  TPasTreeContainer = class
  private
    FCurrentParser: TPasParser;
    FNeedComments: Boolean;
    FOnLog: TPasParserLogHandler;
    FPParserLogEvents: TPParserLogEvents;
    FScannerLogEvents: TPScannerLogEvents;
  protected
    FPackage: TPasPackage;
    FInterfaceOnly: Boolean;
    procedure SetCurrentParser(AValue: TPasParser); virtual;
  public
    function CreateElement(AClass: TPTreeElement; const AName: string;
      AParent: TPasElement; const ASourceFilename: string;
      ASourceLinenumber: Integer): TPasElement;overload;
    function CreateElement(AClass: TPTreeElement; const AName: string;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: string; ASourceLinenumber: Integer): TPasElement;overload;
      virtual; abstract;
    function CreateElement(AClass: TPTreeElement; const AName: string;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASrcPos: TPasSourcePos): TPasElement; overload;
      virtual;
    function CreateFunctionType(const AName, AResultName: string; AParent: TPasElement;
      UseParentAsResultParent: Boolean; const ASrcPos: TPasSourcePos): TPasFunctionType;
    function FindElement(const AName: string): TPasElement; virtual; abstract;
    procedure BeginScope(ScopeType: TPasScopeType; El: TPasElement); virtual;
    procedure FinishScope(ScopeType: TPasScopeType; El: TPasElement); virtual;
    procedure FinishTypeAlias(var aType: TPasType); virtual;
    function FindModule(const AName: string): TPasModule; overload; virtual;
    function FindModule(const AName: string; NameExpr, InFileExpr: TPasExpr): TPasModule; overload; virtual;
    function CheckPendingUsedInterface(Section: TPasSection): boolean; virtual; // true if changed
    function NeedArrayValues(El: TPasElement): boolean; virtual;
    function GetDefaultClassVisibility(AClass: TPasClassType): TPasMemberVisibility; virtual;
    procedure ModeChanged(Sender: TObject; NewMode: TModeSwitch;
      Before: boolean; var Handled: boolean); virtual;
    property Package: TPasPackage read FPackage;
    property InterfaceOnly: Boolean Read FInterfaceOnly Write FInterFaceOnly;
    property ScannerLogEvents: TPScannerLogEvents Read FScannerLogEvents Write FScannerLogEvents;
    property ParserLogEvents: TPParserLogEvents Read FPParserLogEvents Write FPParserLogEvents;
    property OnLog: TPasParserLogHandler Read FOnLog Write FOnLog;
    property CurrentParser: TPasParser Read FCurrentParser Write SetCurrentParser;
    property NeedComments: Boolean Read FNeedComments Write FNeedComments;
  end;

  EParserError = class(Exception)
  private
    FFilename: string;
    FRow, FColumn: Integer;
  public
    constructor Create(const AReason, AFilename: string;
      ARow, AColumn: Integer); reintroduce;
    property Filename: string read FFilename;
    property Row: Integer read FRow;
    property Column: Integer read FColumn;
  end;

  TExprKind = (ek_Normal, ek_PropertyIndex);
  TIndentAction = (iaNone,iaIndent,iaUndent);

  { TPasParser }

  TPasParser = class
  private
    const FTokenRingSize = 32;
    type
      TTokenRec = record
        Token: TToken;
        AsString: string;
        Comments: TStrings;
        SourcePos: TPasSourcePos;
        TokenPos: TPasSourcePos;
      end;
      PTokenRec = ^TTokenRec;
  private
    FCurModule: TPasModule;
    FFileResolver: TBaseFileResolver;
    FImplicitUses: TStrings;
    FLastMsg: string;
    FLastMsgArgs: TMessageArgs;
    FLastMsgNumber: integer;
    FLastMsgPattern: string;
    FLastMsgType: TMessageType;
    FLogEvents: TPParserLogEvents;
    FOnLog: TPasParserLogHandler;
    FOptions: TPOptions;
    FScanner: TPascalScanner;
    FEngine: TPasTreeContainer;
    FCurToken: TToken;
    FCurTokenString: string;
    FSavedComments: string;
    // UngetToken support:
    FTokenRing: array[0..FTokenRingSize-1] of TTokenRec;
    FTokenRingCur: Integer; // index of current token in FTokenBuffer
    FTokenRingStart: Integer; // first valid ring index in FTokenBuffer, if FTokenRingStart=FTokenRingEnd the ring is empty
    FTokenRingEnd: Integer; // first invalid ring index in FTokenBuffer
    {$ifdef VerbosePasParser}
    FDumpIndent: string;
    procedure DumpCurToken(const Msg: string; IndentAction: TIndentAction = iaNone);
    {$endif}
    function CheckOverloadList(AList: TFPList; AName: string; out OldMember: TPasElement): TPasOverloadedProc;
    function DoCheckHint(Element: TPasElement): Boolean;
    function GetCurrentModeSwitches: TModeSwitches;
    procedure SetCurrentModeSwitches(AValue: TModeSwitches);
    function GetVariableModifiers(Parent: TPasElement;
      Out VarMods: TVariableModifiers; Out LibName, ExportName: TPasExpr;
      const AllowedMods: TVariableModifiers): string;
    function GetVariableValueAndLocation(Parent: TPasElement; Out Value: TPasExpr; Out AbsoluteExpr: TPasExpr; Out Location: string): Boolean;
    procedure HandleProcedureModifier(Parent: TPasElement; pm: TProcedureModifier);
    procedure HandleProcedureTypeModifier(ProcType: TPasProcedureType; ptm: TProcTypeModifier);
    procedure ParseMembersLocalConsts(AType: TPasMembersType; AVisibility: TPasMemberVisibility);
    procedure ParseMembersLocalTypes(AType: TPasMembersType; AVisibility: TPasMemberVisibility);
    procedure ParseVarList(Parent: TPasElement; VarList: TFPList; AVisibility: TPasMemberVisibility; Full: Boolean);
    procedure SetOptions(AValue: TPOptions);
    procedure OnScannerModeChanged(Sender: TObject; NewMode: TModeSwitch;
      Before: boolean; var Handled: boolean);
  protected
    function SaveComments: string; overload;
    function SaveComments(const AValue: string): string; overload;
    function LogEvent(E: TPParserLogEvent): Boolean; inline;
    procedure DoLog(MsgType: TMessageType; MsgNumber: integer; const Msg: string; SkipSourceInfo: Boolean = False);overload;
    procedure DoLog(MsgType: TMessageType; MsgNumber: integer; const Fmt: string; Args: Array of {$ifdef pas2js}jsvalue{$else}const{$endif};SkipSourceInfo: Boolean = False);overload;
    function GetProcTypeFromToken(tk: TToken; IsClass: Boolean=False ): TProcType;
    procedure ParseAsmBlock(AsmBlock: TPasImplAsmStatement); virtual;
    procedure ParseRecordFieldList(ARec: TPasRecordType; AEndToken: TToken; AllowMethods: Boolean);
    procedure ParseRecordVariantParts(ARec: TPasRecordType; AEndToken: TToken);
    function GetProcedureClass(ProcType: TProcType): TPTreeElement;
    procedure ParseClassFields(AType: TPasClassType; const AVisibility: TPasMemberVisibility; IsClassField: Boolean);
    procedure ParseClassMembers(AType: TPasClassType);
    procedure ProcessMethod(AType: TPasClassType; IsClass: Boolean; AVisibility: TPasMemberVisibility);
    procedure ReadGenericArguments(List: TFPList;Parent: TPasElement);
    procedure ReadSpecializeArguments(Spec: TPasSpecializeType);
    function ReadDottedIdentifier(Parent: TPasElement; out Expr: TPasExpr; NeedAsString: boolean): string;
    function CheckProcedureArgs(Parent: TPasElement;
      Args: TFPList; // list of TPasArgument
      ProcType: TProcType): boolean;
    function CheckVisibility(S: string; var AVisibility: TPasMemberVisibility): Boolean;
    procedure ParseExc(MsgNumber: integer; const Msg: string); overload;
    procedure ParseExc(MsgNumber: integer; const Fmt: string;
      const Args: array of {$ifdef pas2js}jsvalue{$else}const{$endif}); overload;
    procedure ParseExcExpectedIdentifier;
    procedure ParseExcSyntaxError;
    procedure ParseExcTokenError(const Arg: string);
    function OpLevel(t: TToken): Integer;
    function TokenToExprOp(const AToken: TToken): TExprOpCode;
    function CreateElement(AClass: TPTreeElement; const AName: string; AParent: TPasElement): TPasElement;overload;
    function CreateElement(AClass: TPTreeElement; const AName: string; AParent: TPasElement; const ASrcPos: TPasSourcePos): TPasElement;overload;
    function CreateElement(AClass: TPTreeElement; const AName: string; AParent: TPasElement; AVisibility: TPasMemberVisibility): TPasElement;overload;
    function CreateElement(AClass: TPTreeElement; const AName: string; AParent: TPasElement; AVisibility: TPasMemberVisibility; const ASrcPos: TPasSourcePos): TPasElement;overload;
    function CreatePrimitiveExpr(AParent: TPasElement; AKind: TPasExprKind; const AValue: string): TPrimitiveExpr;
    function CreateBoolConstExpr(AParent: TPasElement; AKind: TPasExprKind; const ABoolValue: Boolean): TBoolConstExpr;
    function CreateBinaryExpr(AParent: TPasElement; xleft, xright: TPasExpr; AOpCode: TExprOpCode): TBinaryExpr; overload;
    function CreateBinaryExpr(AParent: TPasElement; xleft, xright: TPasExpr; AOpCode: TExprOpCode; const ASrcPos: TPasSourcePos): TBinaryExpr; overload;
    procedure AddToBinaryExprChain(var ChainFirst: TPasExpr;
      Element: TPasExpr; AOpCode: TExprOpCode; const ASrcPos: TPasSourcePos);
    {$IFDEF VerbosePasParser}
    procedure WriteBinaryExprChain(Prefix: string; First, Last: TPasExpr);
    {$ENDIF}
    function CreateUnaryExpr(AParent: TPasElement; AOperand: TPasExpr; AOpCode: TExprOpCode): TUnaryExpr; overload;
    function CreateUnaryExpr(AParent: TPasElement; AOperand: TPasExpr; AOpCode: TExprOpCode; const ASrcPos: TPasSourcePos): TUnaryExpr; overload;
    function CreateArrayValues(AParent: TPasElement): TArrayValues;
    function CreateFunctionType(const AName, AResultName: string; AParent: TPasElement;
             UseParentAsResultParent: Boolean; const NamePos: TPasSourcePos): TPasFunctionType;
    function CreateInheritedExpr(AParent: TPasElement): TInheritedExpr;
    function CreateSelfExpr(AParent: TPasElement): TSelfExpr;
    function CreateNilExpr(AParent: TPasElement): TNilExpr;
    function CreateRecordValues(AParent: TPasElement): TRecordValues;
    function IsCurTokenHint(out AHint: TPasMemberHint): Boolean; overload;
    function IsCurTokenHint: Boolean; overload;
    function TokenIsCallingConvention(const S: string; out CC: TCallingConvention): Boolean; virtual;
    function TokenIsProcedureModifier(Parent: TPasElement; const S: string; Out PM: TProcedureModifier): Boolean; virtual;
    function TokenIsAnonymousProcedureModifier(Parent: TPasElement; S: string; Out PM: TProcedureModifier): Boolean; virtual;
    function TokenIsProcedureTypeModifier(Parent: TPasElement; const S: string; Out PTM: TProcTypeModifier): Boolean; virtual;
    function CheckHint(Element: TPasElement; ExpectSemiColon: Boolean): TPasMemberHints;
    function IsAnonymousProcAllowed(El: TPasElement): boolean; virtual;
    function ParseParams(AParent: TPasElement; ParamsKind: TPasExprKind; AllowFormatting: Boolean = False): TParamsExpr;
    function ParseExprOperand(AParent: TPasElement): TPasExpr;
    function ParseExpIdent(AParent: TPasElement): TPasExpr; deprecated 'use ParseExprOperand instead'; // since fpc 3.3.1
    procedure DoParseClassType(AType: TPasClassType);
    function DoParseExpression(AParent: TPaselement;InitExpr: TPasExpr=nil; AllowEqual: Boolean = True): TPasExpr;
    function DoParseConstValueExpression(AParent: TPasElement): TPasExpr;
    function CheckPackMode: TPackMode;
    function AddUseUnit(ASection: TPasSection; const NamePos: TPasSourcePos;
      AUnitName: string; NameExpr: TPasExpr; InFileExpr: TPrimitiveExpr): TPasUsesUnit;
    procedure CheckImplicitUsedUnits(ASection: TPasSection);
    procedure FinishedModule; virtual;
    // Overload handling
    procedure AddProcOrFunction(Decs: TPasDeclarations; AProc: TPasProcedure);
    function  CheckIfOverloaded(AParent: TPasElement; const AName: string): TPasElement;
  public
    constructor Create(AScanner: TPascalScanner; AFileResolver: TBaseFileResolver;  AEngine: TPasTreeContainer);
    Destructor Destroy; override;
    procedure SetLastMsg(MsgType: TMessageType; MsgNumber: integer; const Fmt: string; Args: Array of {$ifdef pas2js}jsvalue{$else}const{$endif});
    // General parsing routines
    function CurTokenName: string;
    function CurTokenText: string;
    function CurComments: TStrings;
    function CurTokenPos: TPasSourcePos;
    function CurSourcePos: TPasSourcePos;
    function HasToken: boolean;
    function SavedComments: string;
    procedure NextToken; // read next non whitespace, non space
    procedure ChangeToken(tk: TToken);
    procedure UngetToken;
    procedure CheckToken(tk: TToken);
    procedure CheckTokens(tk: TTokens);
    procedure ExpectToken(tk: TToken);
    procedure ExpectTokens(tk:  TTokens);
    function GetPrevToken: TToken;
    function ExpectIdentifier: string;
    function CurTokenIsIdentifier(const S: string): Boolean;
    // Expression parsing
    function isEndOfExp(AllowEqual: Boolean = False; CheckHints: Boolean = True): Boolean;
    function ExprToText(Expr: TPasExpr): string;
    function ArrayExprToText(Expr: TPasExprArray): string;
    // Type declarations
    function ResolveTypeReference(Name: string; Parent: TPasElement): TPasType;
    function ParseComplexType(Parent: TPasElement = Nil): TPasType;
    function ParseTypeDecl(Parent: TPasElement): TPasType;
    function ParseType(Parent: TPasElement; const NamePos: TPasSourcePos; const TypeName: string = ''; Full: Boolean = false; GenericArgs: TFPList = nil): TPasType;
    function ParseReferenceToProcedureType(Parent: TPasElement; const NamePos: TPasSourcePos; const TypeName: string): TPasProcedureType;
    function ParseProcedureType(Parent: TPasElement; const NamePos: TPasSourcePos; const TypeName: string; const PT: TProcType): TPasProcedureType;
    function ParseStringType(Parent: TPasElement; const NamePos: TPasSourcePos; const TypeName: string): TPasAliasType;
    function ParseSimpleType(Parent: TPasElement; const NamePos: TPasSourcePos; const TypeName: string; IsFull: Boolean = False): TPasType;
    function ParseAliasType(Parent: TPasElement; const NamePos: TPasSourcePos; const TypeName: string): TPasType;
    function ParseTypeReference(Parent: TPasElement; NeedExpr: boolean; out Expr: TPasExpr): TPasType;
    function ParsePointerType(Parent: TPasElement; const NamePos: TPasSourcePos; const TypeName: string): TPasPointerType;
    function ParseArrayType(Parent: TPasElement; const NamePos: TPasSourcePos; const TypeName: string; PackMode: TPackMode): TPasArrayType;
    function ParseFileType(Parent: TPasElement; const NamePos: TPasSourcePos; const TypeName : string): TPasFileType;
    function ParseRecordDecl(Parent: TPasElement; const NamePos: TPasSourcePos; const TypeName: string; const Packmode: TPackMode = pmNone): TPasRecordType;
    function ParseEnumType(Parent: TPasElement; const NamePos: TPasSourcePos; const TypeName: string): TPasEnumType;
    function ParseSetType(Parent: TPasElement; const NamePos: TPasSourcePos; const TypeName: string; AIsPacked: Boolean = False): TPasSetType;
    function ParseSpecializeType(Parent: TPasElement; const TypeName: string): TPasSpecializeType;
    function ParseClassDecl(Parent: TPasElement; const NamePos: TPasSourcePos; const AClassName: string; AObjKind: TPasObjKind; PackMode: TPackMode= pmNone; GenericArgs: TFPList = nil): TPasType;
    function ParseProperty(Parent: TPasElement; const AName: string; AVisibility: TPasMemberVisibility; IsClassField: boolean): TPasProperty;
    function ParseRangeType(AParent: TPasElement; const NamePos: TPasSourcePos; const TypeName: string; Full: Boolean = True): TPasRangeType;
    procedure ParseExportDecl(Parent: TPasElement; List: TFPList);
    // Constant declarations
    function ParseConstDecl(Parent: TPasElement): TPasConst;
    function ParseResourcestringDecl(Parent: TPasElement): TPasResString;
    procedure ParseAttribute(Parent: TPasElement);
    // Variable handling. This includes parts of records
    procedure ParseVarDecl(Parent: TPasElement; List: TFPList);
    procedure ParseInlineVarDecl(Parent: TPasElement; List: TFPList;  AVisibility: TPasMemberVisibility  = visDefault; ClosingBrace: Boolean = False);
    // Main scope parsing
    procedure ParseMain(var Module: TPasModule);
    procedure ParseUnit(var Module: TPasModule);
    function GetLastSection: TPasSection; virtual;
    function CanParseContinue(out Section: TPasSection): boolean; virtual;
    procedure ParseContinue; virtual;
    procedure ParseProgram(var Module: TPasModule; SkipHeader: Boolean = False);
    procedure ParseLibrary(var Module: TPasModule);
    procedure ParseOptionalUsesList(ASection: TPasSection);
    procedure ParseUsesList(ASection: TPasSection);
    procedure ParseInterface;
    procedure ParseImplementation;
    procedure ParseInitialization;
    procedure ParseFinalization;
    procedure ParseDeclarations(Declarations: TPasDeclarations);
    procedure ParseStatement(Parent: TPasImplBlock;  out NewImplElement: TPasImplElement);
    procedure ParseLabels(AParent: TPasElement);
    procedure ParseProcBeginBlock(Parent: TProcedureBody);
    procedure ParseProcAsmBlock(Parent: TProcedureBody);
    // function/procedure declaration
    function  ParseProcedureOrFunctionDecl(Parent: TPasElement; ProcType: TProcType;AVisibility: TPasMemberVisibility = VisDefault): TPasProcedure;
    procedure ParseArgList(Parent: TPasElement;
      Args: TFPList; // list of TPasArgument
      EndToken: TToken);
    procedure ParseProcedureOrFunction(Parent: TPasElement; Element: TPasProcedureType; ProcType: TProcType; OfObjectPossible: Boolean);
    procedure ParseProcedureBody(Parent: TPasElement);
    function ParseMethodResolution(Parent: TPasElement): TPasMethodResolution;
    // Properties for external access
    property FileResolver: TBaseFileResolver read FFileResolver;
    property Scanner: TPascalScanner read FScanner;
    property Engine: TPasTreeContainer read FEngine;
    property CurToken: TToken read FCurToken;
    property CurTokenString: string read FCurTokenString;
    property Options: TPOptions Read FOptions Write SetOptions;
    property CurrentModeswitches: TModeSwitches Read GetCurrentModeSwitches Write SetCurrentModeSwitches;
    property CurModule: TPasModule Read FCurModule;
    property LogEvents: TPParserLogEvents Read FLogEvents Write FLogEvents;
    property OnLog: TPasParserLogHandler Read FOnLog Write FOnLog;
    property ImplicitUses: TStrings read FImplicitUses;
    property LastMsg: string read FLastMsg write FLastMsg;
    property LastMsgNumber: integer read FLastMsgNumber write FLastMsgNumber;
    property LastMsgType: TMessageType read FLastMsgType write FLastMsgType;
    property LastMsgPattern: string read FLastMsgPattern write FLastMsgPattern;
    property LastMsgArgs: TMessageArgs read FLastMsgArgs write FLastMsgArgs;
  end;

type
  TParseSourceOption = (
    {$ifdef HasStreams}
    poUseStreams,
    {$endif}
    poSkipDefaultDefs);
  TParseSourceOptions = set of TParseSourceOption;

var
  DefaultFileResolverClass: TBaseFileResolverClass = Nil;

function ParseSource(AEngine: TPasTreeContainer;
                     const FPCCommandLine, OSTarget, CPUTarget: string): TPasModule; overload;
{$ifdef HasStreams}
function ParseSource(AEngine: TPasTreeContainer;
                     const FPCCommandLine, OSTarget, CPUTarget: string;
                     UseStreams : Boolean): TPasModule; deprecated; overload;
{$endif}
function ParseSource(AEngine: TPasTreeContainer;
                     const FPCCommandLine, OSTarget, CPUTarget: string;
                     Options: TParseSourceOptions): TPasModule; overload;

function IsHintToken(T: string; Out AHint: TPasMemberHint): boolean;
function IsProcModifier(S: string; Out PM: TProcedureModifier): Boolean;
function IsCallingConvention(S: string; out CC: TCallingConvention): Boolean;
function TokenToAssignKind( tk: TToken): TAssignKind;

implementation

const
  WhitespaceTokensToIgnore = [tkWhitespace, tkComment, tkLineEnding, tkTab];

type
  TDeclType = (declNone, declConst, declResourcestring, declType,
               declVar, declThreadvar, declProperty, declExports);

function IsHintToken(T: string; Out AHint: TPasMemberHint): boolean;
const
   MemberHintTokens: Array[TPasMemberHint] of string =
     ('deprecated','library','platform','experimental','unimplemented');
var
  I: TPasMemberHint;
begin
  t := LowerCase(t);
  for I := Low(TPasMemberHint) to High(TPasMemberHint) do
    begin
    result := (t=MemberHintTokens[i]);
    if Result then
      begin
      aHint := I;
      Exit;
      end;
    end;
end;


function IsCallingConvention(S: string; out CC: TCallingConvention): Boolean;
const
  CCNames: array[TCallingConvention] of string
    = ('','register','pascal','cdecl','stdcall','oldfpccall','safecall','syscall');
var
  C: TCallingConvention;
begin
  S := Lowercase(s);
  for C := Low(TCallingConvention) to High(TCallingConvention) do begin
    Result := (CCNames[c]<>'') and (s=CCnames[c]);
    if Result then begin
      CC := C;
      Exit;
    end;
  end;
end;

function IsProcModifier(S: string; Out PM: TProcedureModifier): Boolean;
var
  P: TProcedureModifier;
begin
  S := LowerCase(S);
  for P := Low(TProcedureModifier) to High(TProcedureModifier) do begin
    Result := s=ModifierNames[P];
    if Result then begin
      PM := P;
      Exit;
    end;
  end;
end;

function TokenToAssignKind( tk: TToken): TAssignKind;

begin
  case tk of
    tkAssign        : Result := akDefault;
    tkAssignPlus    : Result := akAdd;
    tkAssignMinus   : Result := akMinus;
    tkAssignMul     : Result := akMul;
    tkAssignDivision: Result := akDivision;
  else
    Raise Exception.CreateFmt('Not an assignment token: %s',[TokenInfos[tk]]);
  end;
end;

function ParseSource(AEngine: TPasTreeContainer;
  const FPCCommandLine, OSTarget, CPUTarget: string): TPasModule;

begin
  Result := ParseSource(AEngine,FPCCommandLine, OSTarget, CPUTarget,[]);
end;

{$ifdef HasStreams}
function ParseSource(AEngine: TPasTreeContainer;
  const FPCCommandLine, OSTarget, CPUTarget: string; UseStreams: Boolean): TPasModule;

begin
  if UseStreams then
    Result := ParseSource(AEngine,FPCCommandLine, OSTarget, CPUTarget,[poUseStreams])
  else
    Result := ParseSource(AEngine,FPCCommandLine, OSTarget, CPUTarget,[]);
end;
{$endif}

function ParseSource(AEngine: TPasTreeContainer;
  const FPCCommandLine, OSTarget, CPUTarget: string;
  Options: TParseSourceOptions): TPasModule;

var
  FileResolver: TBaseFileResolver;
  Parser: TPasParser;
  Start, CurPos: integer; // in FPCCommandLine
  Filename: string;
  Scanner: TPascalScanner;

  procedure ProcessCmdLinePart;
  var
    l: Integer;
    s: string;
  begin
    l := CurPos - Start;
    if l <= 0 then
      Exit;
    s := Trim(copy(FPCCommandLine,Start,l));
    if (s[1] = '-') and (length(s)>1) then
    begin
      case s[2] of
        'd': // -d define
          Scanner.AddDefine(UpperCase(Copy(s, 3, Length(s))));
        'u': // -u undefine
          Scanner.RemoveDefine(UpperCase(Copy(s, 3, Length(s))));
        'F': // -F
          if (length(s)>2) and (s[3] = 'i') then // -Fi include path
            FileResolver.AddIncludePath(Copy(s, 4, Length(s)));
        'I': // -I include path
          FileResolver.AddIncludePath(Copy(s, 3, Length(s)));
        'S': // -S mode
          if length(s)>2 then begin
            l := 3;
            While L<=Length(S) do begin
              case S[l] of
                'c': Scanner.Options := Scanner.Options+[po_CAssignments];
                'd': Scanner.SetCompilerMode('DELPHI');
                '2': Scanner.SetCompilerMode('OBJFPC');
                'h': ; // do nothing
              end;
              inc(l);
            end;
          end;
        'M' :
           begin
           delete(S,1,2);
           Scanner.SetCompilerMode(S);
           end;
      end;
    end else
      if Filename <> '' then
        raise ENotSupportedException.Create(SErrMultipleSourceFiles)
      else
        Filename := s;
  end;

var
  s: string;
begin
  if DefaultFileResolverClass=Nil then
    raise ENotImplemented.Create(SErrFileSystemNotSupported);
  Result := nil;
  FileResolver := nil;
  Scanner := nil;
  Parser := nil;
  try
    FileResolver := DefaultFileResolverClass.Create;
    {$ifdef HasStreams}
    if FileResolver is TFileResolver then
      TFileResolver(FileResolver).UseStreams := poUseStreams in Options;
    {$endif}
    Scanner := TPascalScanner.Create(FileResolver);
    Scanner.LogEvents := AEngine.ScannerLogEvents;
    Scanner.OnLog := AEngine.Onlog;
    if not (poSkipDefaultDefs in Options) then
      begin
      Scanner.AddDefine('FPK');
      Scanner.AddDefine('FPC');
      // TargetOS
      s := UpperCase(OSTarget);
      Scanner.AddDefine(s);
      if s = 'LINUX' then
        Scanner.AddDefine('UNIX')
      else if s = 'FREEBSD' then
      begin
        Scanner.AddDefine('BSD');
        Scanner.AddDefine('UNIX');
      end else if s = 'NETBSD' then
      begin
        Scanner.AddDefine('BSD');
        Scanner.AddDefine('UNIX');
      end else if s = 'SUNOS' then
      begin
        Scanner.AddDefine('SOLARIS');
        Scanner.AddDefine('UNIX');
      end else if s = 'GO32V2' then
        Scanner.AddDefine('DPMI')
      else if s = 'BEOS' then
        Scanner.AddDefine('UNIX')
      else if s = 'QNX' then
        Scanner.AddDefine('UNIX')
      else if s = 'AROS' then
        Scanner.AddDefine('HASAMIGA')
      else if s = 'MORPHOS' then
        Scanner.AddDefine('HASAMIGA')
      else if s = 'AMIGA' then
        Scanner.AddDefine('HASAMIGA');

      // TargetCPU
      s := UpperCase(CPUTarget);
      Scanner.AddDefine('CPU'+s);
      if (s='X86_64') then
        Scanner.AddDefine('CPU64')
      else
        Scanner.AddDefine('CPU32');
      end;
    Parser := TPasParser.Create(Scanner, FileResolver, AEngine);
    if (poSkipDefaultDefs in Options) then
      begin
      Writeln('>>> Clearing <<<');
      Parser.ImplicitUses.Clear;
      end;
    Writeln('Implicit >>>',Parser.ImplicitUses.Text,'<<<');
    Filename := '';
    Parser.LogEvents := AEngine.ParserLogEvents;
    Parser.OnLog := AEngine.Onlog;

    if FPCCommandLine<>'' then
      begin
      Start := 1;
      CurPos := Start;
      while CurPos<length(FPCCommandLine) do
        begin
        if (FPCCommandLine[CurPos] = ' ') and (FPCCommandLine[CurPos+1]<>' ') then
          begin
          ProcessCmdLinePart;
          Start := CurPos + 1;
          end;
        Inc(CurPos);
        end;
      Inc(CurPos);
      ProcessCmdLinePart;
      end;

    if Filename = '' then
      raise Exception.Create(SErrNoSourceGiven);
{$IFDEF HASFS}
    FileResolver.AddIncludePath(ExtractFilePath(FileName));
{$ENDIF}
    Scanner.OpenFile(Filename);
    Parser.ParseMain(Result);
  finally
    Parser.Free;
    Scanner.Free;
    FileResolver.Free;
  end;
end;

{ ---------------------------------------------------------------------
  TPasTreeContainer
  ---------------------------------------------------------------------}

procedure TPasTreeContainer.SetCurrentParser(AValue: TPasParser);
begin
  if FCurrentParser=AValue then Exit;
  FCurrentParser := AValue;
end;

function TPasTreeContainer.CreateElement(AClass: TPTreeElement;
  const AName: string; AParent: TPasElement; const ASourceFilename: string;
  ASourceLinenumber: Integer): TPasElement;
begin
  Result := CreateElement(AClass, AName, AParent, visDefault, ASourceFilename,
    ASourceLinenumber);
end;

function TPasTreeContainer.CreateElement(AClass: TPTreeElement;
  const AName: string; AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASrcPos: TPasSourcePos): TPasElement;
begin
  Result := CreateElement(AClass, AName, AParent, AVisibility, ASrcPos.FileName,
    ASrcPos.Row);
end;

function TPasTreeContainer.CreateFunctionType(const AName, AResultName: string;
  AParent: TPasElement; UseParentAsResultParent: Boolean;
  const ASrcPos: TPasSourcePos): TPasFunctionType;
var
  ResultParent: TPasElement;
begin
  Result := TPasFunctionType(CreateElement(TPasFunctionType, AName, AParent,
    visDefault, ASrcPos));

  if UseParentAsResultParent then
    ResultParent := AParent
  else
    ResultParent := Result;

  TPasFunctionType(Result).ResultEl :=
    TPasResultElement(CreateElement(TPasResultElement, AResultName, ResultParent,
    visDefault, ASrcPos));
end;

procedure TPasTreeContainer.BeginScope(ScopeType: TPasScopeType; El: TPasElement
  );
begin
  if ScopeType=stModule then ; // avoid compiler warning
  if El=nil then ;
end;

procedure TPasTreeContainer.FinishScope(ScopeType: TPasScopeType;
  El: TPasElement);
begin
  if ScopeType=stModule then ; // avoid compiler warning
  if Assigned(El) and (CurrentParser<>nil) then
    El.SourceEndLinenumber := CurrentParser.CurSourcePos.Row;
end;

procedure TPasTreeContainer.FinishTypeAlias(var aType: TPasType);
begin
  if aType=nil then ;
end;

function TPasTreeContainer.FindModule(const AName: string): TPasModule;
begin
  if AName='' then ;  // avoid compiler warning
  Result := nil;
end;

function TPasTreeContainer.FindModule(const AName: string; NameExpr,
  InFileExpr: TPasExpr): TPasModule;
begin
  Result := FindModule(AName);
  if NameExpr=nil then ;
  if InFileExpr=nil then ;
end;

function TPasTreeContainer.CheckPendingUsedInterface(Section: TPasSection
  ): boolean;
begin
  if Section=nil then ;  // avoid compiler warning
  Result := false;
end;

function TPasTreeContainer.NeedArrayValues(El: TPasElement): boolean;
begin
  Result := false;
  if El=nil then ;  // avoid compiler warning
end;

function TPasTreeContainer.GetDefaultClassVisibility(AClass: TPasClassType
  ): TPasMemberVisibility;
begin
  Result := visDefault;
  if AClass=nil then ;  // avoid compiler warning
end;

procedure TPasTreeContainer.ModeChanged(Sender: TObject; NewMode: TModeSwitch;
  Before: boolean; var Handled: boolean);
begin
  if Sender=nil then ;
  if NewMode=msDelphi then ;
  if Before then ;
  if Handled then ;
end;

{ ---------------------------------------------------------------------
  EParserError
  ---------------------------------------------------------------------}

constructor EParserError.Create(const AReason, AFilename: string;
  ARow, AColumn: Integer);
begin
  inherited Create(AReason);
  FFilename := AFilename;
  FRow := ARow;
  FColumn := AColumn;
end;

{ ---------------------------------------------------------------------
  TPasParser
  ---------------------------------------------------------------------}

procedure TPasParser.ParseExc(MsgNumber: integer; const Msg: string);
begin
  ParseExc(MsgNumber,Msg,[]);
end;

procedure TPasParser.ParseExc(MsgNumber: integer; const Fmt: string;
  const Args: array of {$ifdef pas2js}jsvalue{$else}const{$endif});
var
  p: TPasSourcePos;
begin
  {$IFDEF VerbosePasParser}
  writeln('TPasParser.ParseExc Token="',CurTokenText,'"');
  //writeln('TPasParser.ParseExc ',Scanner.CurColumn,' ',Scanner.CurSourcePos.Column,' ',Scanner.CurTokenPos.Column,' ',Scanner.CurSourceFile.Filename);
  {$ENDIF}
  SetLastMsg(mtError,MsgNumber,Fmt,Args);
  p := Scanner.CurTokenPos;
  if p.FileName='' then
    p := Scanner.CurSourcePos;
  if p.Row=0 then
    begin
    p.Row := 1;
    p.Column := 1;
    end;
  raise EParserError.Create(SafeFormat(SParserErrorAtToken,
    [FLastMsg, CurTokenName, p.FileName, p.Row, p.Column])
    {$ifdef addlocation}+' ('+IntToStr(p.Row)+' '+IntToStr(p.Column)+')'{$endif},
    p.FileName, p.Row, p.Column);
end;

procedure TPasParser.ParseExcExpectedIdentifier;
begin
  ParseExc(nParserExpectedIdentifier,SParserExpectedIdentifier);
end;

procedure TPasParser.ParseExcSyntaxError;
begin
  ParseExc(nParserSyntaxError,SParserSyntaxError);
end;

procedure TPasParser.ParseExcTokenError(const Arg: string);
begin
  ParseExc(nParserExpectTokenError,SParserExpectTokenError,[Arg]);
end;

constructor TPasParser.Create(AScanner: TPascalScanner;
  AFileResolver: TBaseFileResolver; AEngine: TPasTreeContainer);
begin
  inherited Create;
  FScanner := AScanner;
  if not Assigned(FScanner.OnModeChanged) then
    FScanner.OnModeChanged := OnScannerModeChanged;
  FFileResolver := AFileResolver;
  FTokenRingCur := High(FTokenRing);
  FEngine := AEngine;
  if Assigned(FEngine) then
    begin
    FEngine.CurrentParser := Self;
    if FEngine.NeedComments then
      FScanner.SkipComments := not FEngine.NeedComments;
    end;
  FImplicitUses := TStringList.Create;
  FImplicitUses.Add('System'); // system always implicitely first.
end;

destructor TPasParser.Destroy;
var
  i: Integer;
  m: TPScannerModeDirective;
begin
  m := OnScannerModeChanged;
  if MethodPointersEqual(FScanner.OnModeChanged, m) then
    FScanner.OnModeChanged := nil;
  if Assigned(FEngine) then
    begin
    FEngine.CurrentParser := Nil;
    FEngine := nil;
    end;
  FreeAndNil(FImplicitUses);
  for i := low(FTokenRing) to high(FTokenRing) do
    FreeAndNil(FTokenRing[i].Comments);
  inherited Destroy;
end;

function TPasParser.CurTokenName: string;
begin
  if CurToken = tkIdentifier then
    Result := 'Identifier ' + FCurTokenString
  else
    Result := TokenInfos[CurToken];
end;

function TPasParser.CurTokenText: string;
begin
  case CurToken of
    tkIdentifier, tkString, tkNumber, tkChar:
      Result := FCurTokenString;
    else
      Result := TokenInfos[CurToken];
  end;
end;

function TPasParser.CurComments: TStrings;
begin
  if FTokenRingStart=FTokenRingEnd then
    Result := nil
  else
    Result := FTokenRing[FTokenRingCur].Comments;
end;

function TPasParser.CurTokenPos: TPasSourcePos;
begin
  if HasToken then
    Result := FTokenRing[FTokenRingCur].TokenPos
  else if Scanner<>nil then
    Result := Scanner.CurTokenPos
  else
    Result := Default(TPasSourcePos);
end;

function TPasParser.CurSourcePos: TPasSourcePos;
begin
  if HasToken then
    Result := FTokenRing[FTokenRingCur].SourcePos
  else if Scanner<>nil then
    Result := Scanner.CurSourcePos
  else
    Result := Default(TPasSourcePos);
end;

function TPasParser.HasToken: boolean;
begin
  if FTokenRingStart<FTokenRingEnd then
    Result := (FTokenRingCur>=FTokenRingStart) and (FTokenRingCur<FTokenRingEnd)
  else
    Result := (FTokenRingCur>=FTokenRingStart) or (FTokenRingCur<FTokenRingEnd);
end;

function TPasParser.SavedComments: string;
begin
  Result := FSavedComments;
end;

procedure TPasParser.NextToken;
var
  P: PTokenRec;
begin
  FTokenRingCur := (FTokenRingCur+1) mod FTokenRingSize;
  P := @FTokenRing[FTokenRingCur];
  if FTokenRingCur <> FTokenRingEnd then begin
    // Get token from buffer
    //writeln('TPasParser.NextToken REUSE Start=',FTokenRingStart,' Cur=',FTokenRingCur,' End=',FTokenRingEnd,' Cur=',CurTokenString);
    FCurToken := Scanner.CheckToken(P^.Token,P^.AsString);
    FCurTokenString := P^.AsString;
  end else begin
    // Fetch new token
    //writeln('TPasParser.NextToken FETCH Start=',FTokenRingStart,' Cur=',FTokenRingCur,' End=',FTokenRingEnd,' Cur=',CurTokenString);
    FTokenRingEnd := (FTokenRingEnd+1) mod FTokenRingSize;
    if FTokenRingStart=FTokenRingEnd then
      FTokenRingStart := (FTokenRingStart+1) mod FTokenRingSize;
    try
      if p^.Comments=nil then
        p^.Comments := TStringList.Create
      else
        p^.Comments.Clear;
      repeat
        FCurToken := Scanner.FetchToken;
        if FCurToken=tkComment then
          p^.Comments.Add(Scanner.CurTokenString);
      until not (FCurToken in WhitespaceTokensToIgnore);
    except
      on e: EScannerError do begin
        if po_KeepScannerError in Options then
          raise
        else begin
          FLastMsgType := mtError;
          FLastMsgNumber := Scanner.LastMsgNumber;
          FLastMsgPattern := Scanner.LastMsgPattern;
          FLastMsg := Scanner.LastMsg;
          FLastMsgArgs := Scanner.LastMsgArgs;
          raise EParserError.Create(e.Message,
            Scanner.CurFilename, Scanner.CurRow, Scanner.CurColumn);
        end;
      end;
    end;
    p^.Token := FCurToken;
    FCurTokenString := Scanner.CurTokenString;
    p^.AsString := FCurTokenString;
    p^.SourcePos := Scanner.CurSourcePos;
    p^.TokenPos := Scanner.CurTokenPos;
  end;
  //writeln('TPasParser.NextToken END Start=',FTokenRingStart,' Cur=',FTokenRingCur,' End=',FTokenRingEnd,' Cur=',CurTokenString);
end;

procedure TPasParser.ChangeToken(tk: TToken);
var
  Cur, Last: PTokenRec;
  IsLast: Boolean;
begin
  //writeln('TPasParser.ChangeToken FTokenBufferSize=',FTokenRingStart,' FTokenBufferIndex=',FTokenRingCur);
  IsLast := ((FTokenRingCur+1) mod FTokenRingSize)=FTokenRingEnd;
  if (CurToken=tkshr) and (tk=tkGreaterThan) and IsLast then
    begin
    // change last token '>>' into two '>'
    Cur := @FTokenRing[FTokenRingCur];
    Cur^.Token := tkGreaterThan;
    Cur^.AsString := '>';
    Last := @FTokenRing[FTokenRingEnd];
    Last^.Token := tkGreaterThan;
    Last^.AsString := '>';
    if Last^.Comments<>nil then
      Last^.Comments.Clear;
    Last^.SourcePos := Cur^.SourcePos;
    dec(Cur^.SourcePos.Column);
    Last^.TokenPos := Cur^.TokenPos;
    inc(Last^.TokenPos.Column);
    FTokenRingEnd := (FTokenRingEnd+1) mod FTokenRingSize;
    if FTokenRingStart=FTokenRingEnd then
      FTokenRingStart := (FTokenRingStart+1) mod FTokenRingSize;
    FCurToken := tkGreaterThan;
    FCurTokenString := '>';
    end
  else
    CheckToken(tk);
end;

procedure TPasParser.UngetToken;

var
  P: PTokenRec;
begin
  //writeln('TPasParser.UngetToken START Start=',FTokenRingStart,' Cur=',FTokenRingCur,' End=',FTokenRingEnd,' Cur=',CurTokenString);
  if FTokenRingStart = FTokenRingEnd then
    ParseExc(nParserUngetTokenError,SParserUngetTokenError);
  if FTokenRingCur>0 then
    dec(FTokenRingCur)
  else
    FTokenRingCur := High(FTokenRing);
  P := @FTokenRing[FTokenRingCur];
  FCurToken := P^.Token;
  FCurTokenString := P^.AsString;
  //writeln('TPasParser.UngetToken END Start=',FTokenRingStart,' Cur=',FTokenRingCur,' End=',FTokenRingEnd,' Cur=',CurTokenString);
end;

procedure TPasParser.CheckToken(tk: TToken);
begin
  if (CurToken<>tk) then
    begin
    {$IFDEF VerbosePasParser}
    writeln('TPasParser.ParseExcTokenError string="',CurTokenString,'" Text="',CurTokenText,'" CurToken=',CurToken,' tk=',tk);
    {$ENDIF}
    ParseExcTokenError(TokenInfos[tk]);
    end;
end;

procedure TPasParser.CheckTokens(tk: TTokens);
var
  S: string;
  t: TToken;
begin
  if not (CurToken in tk) then
    begin
    {$IFDEF VerbosePasParser}
    writeln('TPasParser.ParseExcTokenError string="',CurTokenString,'" Text="',CurTokenText,'" CurToken=',CurToken);
    {$ENDIF}
    S := '';
    for t := Low(TToken) to High(TToken) do
      if t in tk then
        begin
        if (S<>'') then
          S := S+' or ';
        S := S+TokenInfos[t];
        end;
    ParseExcTokenError(S);
    end;
end;


procedure TPasParser.ExpectToken(tk: TToken);
begin
  NextToken;
  CheckToken(tk);
end;

procedure TPasParser.ExpectTokens(tk: TTokens);
begin
  NextToken;
  CheckTokens(tk);
end;

function TPasParser.GetPrevToken: TToken;
var
  i: Integer;
  P: PTokenRec;
begin
  if FTokenRingStart = FTokenRingEnd then
    Exit(tkEOF);
  i := FTokenRingCur;
  if i>0 then
    dec(i)
  else
    i := High(FTokenRing);
  P := @FTokenRing[i];
  Result := P^.Token;
end;

function TPasParser.ExpectIdentifier: string;
begin
  ExpectToken(tkIdentifier);
  Result := CurTokenString;
end;

function TPasParser.CurTokenIsIdentifier(const S: string): Boolean;
begin
  Result := (Curtoken=tkIdentifier) and (CompareText(S,CurtokenText)=0);
end;

function TPasParser.IsCurTokenHint(out AHint: TPasMemberHint): Boolean;
begin
  Result := CurToken=tklibrary;
  if Result then
    AHint := hLibrary
  else if (CurToken=tkIdentifier) then
    Result := IsHintToken(CurTokenString,ahint);
end;

function TPasParser.IsCurTokenHint: Boolean;
var
  dummy: TPasMemberHint;
begin
  Result := IsCurTokenHint(dummy);
end;

function TPasParser.TokenIsCallingConvention(const S: string; out
  CC: TCallingConvention): Boolean;
begin
  Result := IsCallingConvention(S,CC);
end;

function TPasParser.TokenIsProcedureModifier(Parent: TPasElement;
  const S: string; out PM: TProcedureModifier): Boolean;
begin
  Result := IsProcModifier(S,PM);
  if not Result then Exit;
  While (Parent<>Nil) do
    begin
    if Parent is TPasClassType then
      begin
      if PM in [pmPublic,pmForward] then Exit(false);
      case TPasClassType(Parent).ObjKind of
      okInterface,okDispInterface:
        if not (PM in [pmOverload, pmMessage,
                        pmDispId,pmNoReturn,pmFar,pmFinal]) then Exit(false);
      end;
      Exit;
      end
    else if Parent is TPasRecordType then
      begin
      if not (PM in [pmOverload,
                     pmInline, pmAssembler,
                     pmExternal,
                     pmNoReturn, pmFar, pmFinal]) then Exit(false);
      Exit;
      end;
    Parent := Parent.Parent;
    end;
end;

function TPasParser.TokenIsAnonymousProcedureModifier(Parent: TPasElement;
  S: string; out PM: TProcedureModifier): Boolean;
begin
  Result := IsProcModifier(S,PM);
  if not Result then Exit;
  Result := PM in [pmAssembler];
  if Parent=nil then ;
end;

function TPasParser.TokenIsProcedureTypeModifier(Parent: TPasElement;
  const S: string; out PTM: TProcTypeModifier): Boolean;
begin
  if CompareText(S,ProcTypeModifiers[ptmVarargs])=0 then
    begin
    Result := true;
    PTM := ptmVarargs;
    end
  else if CompareText(S,ProcTypeModifiers[ptmStatic])=0 then
    begin
    Result := true;
    PTM := ptmStatic;
    end
  else
   Result := false;
  if Parent=nil then;
end;

function TPasParser.CheckHint(Element: TPasElement; ExpectSemiColon: Boolean
  ): TPasMemberHints;

var
  Found: Boolean;
  h: TPasMemberHint;

begin
  Result := [];
  Repeat
    NextToken;
    Found := IsCurTokenHint(h);
    if Found then
      begin
      Include(Result,h);
      if (h=hDeprecated) then
        begin
        NextToken;
        if (Curtoken<>tkString) then
          UnGetToken
        else if assigned(Element) then
          Element.HintMessage := CurTokenString;
        end;
      end;
  Until not Found;
  UngetToken;
  if Assigned(Element) then
    Element.Hints := Result;
  if ExpectSemiColon then
    ExpectToken(tkSemiColon);
end;

function TPasParser.IsAnonymousProcAllowed(El: TPasElement): boolean;
begin
  while El is TPasExpr do
    El := El.Parent;
  Result := El is TPasImplBlock; // only in statements
end;

function TPasParser.CheckPackMode: TPackMode;

begin
  NextToken;
  case CurToken of
    tkPacked   : Result := pmPacked;
    tkbitpacked: Result := pmBitPacked;
  else
    result := pmNone;
  end;
  if (Result<>pmNone) then
     begin
     NextToken;
     if not (CurToken in [tkArray, tkRecord, tkObject, tkClass, tkSet]) then
       ParseExcTokenError('SET, ARRAY, RECORD, OBJECT or CLASS');
     end;
end;

function IsSimpleTypeToken(var AName: string): Boolean;
const
   SimpleTypeCount = 15;
   SimpleTypeNames: Array[1..SimpleTypeCount] of string =
     ('byte','boolean','char','integer','int64','longint','longword','double',
      'shortint','smallint','string','word','qword','cardinal','widechar');
   SimpleTypeCaseNames: Array[1..SimpleTypeCount] of string =
     ('Byte','Boolean','Char','Integer','Int64','LongInt','LongWord','Double',
     'ShortInt','SmallInt','string','Word','QWord','Cardinal','WideChar');
var
  S: string;
  I: Integer;
begin
  S := LowerCase(AName);
  I := SimpleTypeCount;
  While (I>0) and (s<>SimpleTypeNames[i]) do
    Dec(I);
  Result := (I>0);
  if Result Then
    AName := SimpleTypeCaseNames[I];
end;

function TPasParser.ParseStringType(Parent: TPasElement;
  const NamePos: TPasSourcePos; const TypeName: string): TPasAliasType;

var
  LengthAsText: string;
  ok: Boolean;
  Params: TParamsExpr;
  LengthExpr: TPasExpr;

begin
  Result := TPasAliasType(CreateElement(TPasAliasType, TypeName, Parent, NamePos));
  ok := false;
  try
    if (Result.Name='') then
      Result.Name := 'string';
    Result.Expr := CreatePrimitiveExpr(Result,pekIdent,TypeName);
    NextToken;
    LengthAsText := '';
    if CurToken=tkSquaredBraceOpen then
      begin
      Params := TParamsExpr(CreateElement(TParamsExpr,'',Result));
      Params.Value := Result.Expr;
      Result.Expr := Params;
      LengthAsText := '';
      NextToken;
      LengthExpr := DoParseExpression(Result,nil,false);
      Params.AddParam(LengthExpr);
      CheckToken(tkSquaredBraceClose);
      LengthAsText := ExprToText(LengthExpr);
      end
    else
      UngetToken;
    Result.DestType := TPasStringType(CreateElement(TPasStringType,'string',Result));
    TPasStringType(Result.DestType).LengthExpr := LengthAsText;
    ok := true;
  finally
    if not ok then
      Result.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
  end;
end;

function TPasParser.ParseSimpleType(Parent: TPasElement;
  const NamePos: TPasSourcePos; const TypeName: string; IsFull: Boolean
  ): TPasType;
type
  TSimpleTypeKind = (stkNone,stkAlias,stkString,stkRange,stkSpecialize);
var
  ref: TPasType;
  k: TSimpleTypeKind;
  name: string;
  st: TPasSpecializeType;
  expr: TPasExpr;
  srcPos: TPasSourcePos;
  ok: Boolean;
begin
  Result := nil;
  name := CurTokenString;
  expr := nil;
  ref := nil;
  k := stkNone;
  try
    if IsFull then
      expr := CreatePrimitiveExpr(Parent,pekIdent,name);
    NextToken;
    while CurToken=tkDot do
      begin
      srcPos := CurTokenPos;
      ExpectIdentifier;
      name := name+'.'+CurTokenString;
      if IsFull then
        AddToBinaryExprChain(expr,CreatePrimitiveExpr(Parent,pekIdent,CurTokenString),
                             eopSubIdent,srcPos);
      NextToken;
      end;
    // Current token is first token after identifier.
    if IsFull and (CurToken=tkSemicolon) or isCurTokenHint then begin // type A = B;
      k := stkAlias;
      UnGetToken;
    end else if IsFull and (CurToken=tkSquaredBraceOpen) then begin
      if LowerCase(name)='string' then // type A = string[12]; shortstring
        k := stkString
      else
        ParseExcSyntaxError;
      UnGetToken;
    end else if (CurToken = tkLessThan) then begin // A = B<t>;
      k := stkSpecialize;
    end else if (CurToken in [tkBraceOpen,tkDotDot]) then begin // A: B..C;
      k := stkRange;
      UnGetToken;
    end else begin
      if IsFull then
        ParseExcTokenError(';');
      k := stkAlias;
      if (not (po_resolvestandardtypes in Options)) and (LowerCase(name)='string') then
        k := stkString;
      UnGetToken;
    end;
    case k of
      stkString: begin
        ReleaseAndNil(TPasElement(expr){$IFDEF CheckPasTreeRefCount},'CreateElement'{$ENDIF});
        Result := ParseStringType(Parent,NamePos,TypeName);
      end;
      stkSpecialize: begin
        st := TPasSpecializeType(CreateElement(TPasSpecializeType, TypeName, Parent, CurTokenPos));
        try
          if expr<>nil then
            begin
            st.expr := expr;
            expr.Parent := st;
            expr := nil;
            end;
          ref := ResolveTypeReference(name,st);
          st.DestType := ref;
          ReadSpecializeArguments(st);
          if TypeName<>'' then
            Engine.FinishScope(stTypeDef,st);
          Result := st;
        finally
          if Result=nil then
            st.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
        end;
      end;
      stkRange: begin
        ReleaseAndNil(TPasElement(expr){$IFDEF CheckPasTreeRefCount},'CreateElement'{$ENDIF});
        UnGetToken; // move to '='
        Result := ParseRangeType(Parent,NamePos,TypeName,False);
      end;
      stkAlias: begin
        ref := ResolveTypeReference(name,Parent);
        if IsFull then begin
          Result := TPasAliasType(CreateElement(TPasAliasType, TypeName, Parent, NamePos));
          TPasAliasType(Result).DestType := ref;
          TPasAliasType(Result).expr := expr;
          expr.Parent := Result;
          if TypeName<>'' then begin
            ok := false;
            try
              Engine.FinishScope(stTypeDef,Result);
              ok := true;
            finally
              if not ok then
                Result.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
            end;
          end;
        end else
          Result := ref;
      end;
    end;
  finally
    if Result=nil then begin
      ReleaseAndNil(TPasElement(expr){$IFDEF CheckPasTreeRefCount},'CreateElement'{$ENDIF});
      ReleaseAndNil(TPasElement(ref){$IFDEF CheckPasTreeRefCount},'ResolveTypeReference'{$ENDIF});
    end;
  end;
end;

// On entry, we're on the TYPE token
function TPasParser.ParseAliasType(Parent: TPasElement;
  const NamePos: TPasSourcePos; const TypeName: string): TPasType;
var
  ok: Boolean;
begin
  Result := TPasTypeAliasType(CreateElement(TPasTypeAliasType, TypeName, Parent, NamePos));
  ok := false;
  try
    TPasTypeAliasType(Result).DestType := ParseType(Result,NamePos,'');
    Engine.FinishTypeAlias(Result);
    Engine.FinishScope(stTypeDef,Result);
    ok := true;
  finally
    if not ok then
      Result.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
  end;
end;

function TPasParser.ParseTypeReference(Parent: TPasElement; NeedExpr: boolean;
  out Expr: TPasExpr): TPasType;
// returns either
// a) TPasSpecializeType, Expr=nil
// b) TPasUnresolvedTypeRef, Expr<>nil
// c) TPasType, Expr<>nil
var
  Name: string;
  IsSpecialize: Boolean;
  ST: TPasSpecializeType;
begin
  Expr := nil;
  ST := nil;
  try
    if not (msDelphi in CurrentModeswitches) and (CurToken=tkspecialize) then
      begin
      IsSpecialize := true;
      NextToken;
      end
    else
      IsSpecialize := false;
    // read dotted identifier
    CheckToken(tkIdentifier);
    Name := ReadDottedIdentifier(Parent,Expr,true);
    // resolve type
    Result := ResolveTypeReference(Name,Parent);

    if CurToken=tkLessThan then
      begin
      // specialize
      ST := TPasSpecializeType(CreateElement(TPasSpecializeType,'',Parent));
      ST.DestType := Result;
      ST.Expr := Expr;
      Expr := nil;
      // read nested specialize arguments
      ReadSpecializeArguments(ST);
      Result := ST;
      ST := nil;
      NextToken;
      end
    else if IsSpecialize then
      CheckToken(tkLessThan)
    else
      begin
      // simple type reference
      if not NeedExpr then
        ReleaseAndNil(TPasElement(Expr){$IFDEF CheckPasTreeRefCount},'CreateElement'{$ENDIF});
      end;
  finally
    if ST<>nil then St.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
  end;
end;

function TPasParser.ParsePointerType(Parent: TPasElement;
  const NamePos: TPasSourcePos; const TypeName: string): TPasPointerType;

var
  ok: Boolean;
begin
  Result := TPasPointerType(CreateElement(TPasPointerType, TypeName, Parent, NamePos));
  ok := false;
  Try
    TPasPointerType(Result).DestType := ParseType(Result,CurSourcePos);
    Engine.FinishScope(stTypeDef,Result);
    ok := true;
  finally
    if not ok then
      Result.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
  end;
end;

function TPasParser.ParseEnumType(Parent: TPasElement;
  const NamePos: TPasSourcePos; const TypeName: string): TPasEnumType;

var
  EnumValue: TPasEnumValue;
  ok: Boolean;

begin
  Result := TPasEnumType(CreateElement(TPasEnumType, TypeName, Parent, NamePos));
  ok := false;
  try
    while True do
      begin
      NextToken;
      SaveComments;
      EnumValue := TPasEnumValue(CreateElement(TPasEnumValue, CurTokenString, Result));
      Result.Values.Add(EnumValue);
      NextToken;
      if CurToken = tkBraceClose then
        break
      else if CurToken in [tkEqual,tkAssign] then
        begin
        NextToken;
        EnumValue.Value := DoParseExpression(Result);
       // UngetToken;
        if CurToken = tkBraceClose then
          Break
        else if not (CurToken=tkComma) then
          ParseExc(nParserExpectedCommaRBracket,SParserExpectedCommaRBracket);
        end
      else if not (CurToken=tkComma) then
        ParseExc(nParserExpectedCommaRBracket,SParserExpectedCommaRBracket)
      end;
    Engine.FinishScope(stTypeDef,Result);
    ok := true;
  finally
    if not ok then
      Result.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
  end;
end;

function TPasParser.ParseSetType(Parent: TPasElement;
  const NamePos: TPasSourcePos; const TypeName: string; AIsPacked: Boolean = False): TPasSetType;

var
  ok: Boolean;
begin
  Result := TPasSetType(CreateElement(TPasSetType, TypeName, Parent, NamePos));
  Result.IsPacked := AIsPacked;
  ok := false;
  try
    ExpectToken(tkOf);
    Result.EnumType := ParseType(Result,CurSourcePos);
    Engine.FinishScope(stTypeDef,Result);
    ok := true;
  finally
    if not ok then
      Result.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
  end;
end;

function TPasParser.ParseType(Parent: TPasElement;
  const NamePos: TPasSourcePos; const TypeName: string = ''; Full: Boolean = false; GenericArgs: TFPList = Nil
  ): TPasType;

const
  // These types are allowed only when full type declarations
  FullTypeTokens = [tkGeneric,{tkSpecialize,}tkClass,tkInterface,tkDispInterface,tkType];
  // Parsing of these types already takes care of hints
  NoHintTokens = [tkProcedure,tkFunction];
var
  PM: TPackMode;
  CH, isHelper, ok: Boolean;
begin
  Result := nil;
  // NextToken and check pack mode
  Pm := CheckPackMode;
  if Full then
    CH := not (CurToken in NoHintTokens)
  else
    begin
    CH := False;
    if (CurToken in FullTypeTokens) then
      ParseExc(nParserTypeNotAllowedHere,SParserTypeNotAllowedHere,[CurtokenText]);
    end;
  ok := false;
  Try
    case CurToken of
      // types only allowed when full
      tkObject: Result := ParseClassDecl(Parent, NamePos, TypeName, okObject,PM);
      tkDispInterface:
        Result := ParseClassDecl(Parent, NamePos, TypeName, okDispInterface);
      tkInterface:
        Result := ParseClassDecl(Parent, NamePos, TypeName, okInterface);
      tkSpecialize: Result := ParseSpecializeType(Parent,TypeName);
      tkClass:
        begin
        isHelper := false;
        NextToken;
        if CurTokenIsIdentifier('Helper') then
          begin
          // class helper: atype end;
          // class helper for atype end;
          NextToken;
          isHelper := CurToken in [tkfor,tkBraceOpen];
          UnGetToken;
          end;
        UngetToken;
        if isHelper then
          Result := ParseClassDecl(Parent,NamePos,TypeName,okClassHelper,PM, GenericArgs)
        else
          Result := ParseClassDecl(Parent, NamePos, TypeName, okClass, PM, GenericArgs);
        end;
      tkType:
        begin
        isHelper := false;
        if msTypeHelpers in Scanner.CurrentModeSwitches then
          begin
          NextToken;
          if CurTokenIsIdentifier('helper') then
            begin
            // atype = type helper;
            // atype = type helper for atype end;
            NextToken;
            isHelper := CurToken in [tkfor,tkBraceOpen];
            UnGetToken;
            end;
          UnGetToken;
          end;
        if isHelper then
          Result := ParseClassDecl(Parent,NamePos,TypeName,okTypeHelper,PM)
        else
          Result := ParseAliasType(Parent,NamePos,TypeName);
        end;
      // Always allowed
      tkIdentifier:
        begin
        // Bug 31709: PReference = ^Reference;
        // Checked in Delphi: ^Reference to procedure; is not allowed !!
        if CurTokenIsIdentifier('reference') and not (Parent is TPasPointerType) then
          begin
          CH := False;
          Result := ParseReferencetoProcedureType(Parent,NamePos,TypeName)
          end
        else
          Result := ParseSimpleType(Parent,NamePos,TypeName,Full);
        end;
      tkCaret: Result := ParsePointerType(Parent,NamePos,TypeName);
      tkFile: Result := ParseFileType(Parent,NamePos,TypeName);
      tkArray: Result := ParseArrayType(Parent,NamePos,TypeName,pm);
      tkBraceOpen: Result := ParseEnumType(Parent,NamePos,TypeName);
      tkSet: Result := ParseSetType(Parent,NamePos,TypeName,pm=pmPacked);
      tkProcedure: Result := ParseProcedureType(Parent,NamePos,TypeName,ptProcedure);
      tkFunction: Result := ParseProcedureType(Parent,NamePos,TypeName,ptFunction);
      tkRecord:
        begin
        NextToken;
        isHelper := false;
        if CurTokenIsIdentifier('Helper') then
          begin
          // record helper: atype end;
          // record helper for atype end;
          NextToken;
          isHelper := CurToken in [tkfor,tkBraceOpen];
          UnGetToken;
          end;
        UngetToken;
        if isHelper then
          Result := ParseClassDecl(Parent,NamePos,TypeName,okRecordHelper,PM)
        else
          Result := ParseRecordDecl(Parent,NamePos,TypeName,PM);
        end;
      tkNumber,tkMinus,tkChar:
        begin
        UngetToken;
        Result := ParseRangeType(Parent,NamePos,TypeName,Full);
        end;
    else
      ParseExcExpectedIdentifier;
    end;
    if CH then
      CheckHint(Result,True);
    ok := true;
  finally
    if not ok then
      if Result<>nil then
        Result.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
  end;
end;

function TPasParser.ParseReferenceToProcedureType(Parent: TPasElement; const NamePos: TPasSourcePos; const TypeName: string
  ): TPasProcedureType;
begin
  Result := nil;
  if not CurTokenIsIdentifier('reference') then
    ParseExcTokenError('reference');
  ExpectToken(tkTo);
  NextToken;
  case CurToken of
   tkprocedure: Result := ParseProcedureType(Parent,NamePos,TypeName,ptProcedure);
   tkfunction: Result := ParseProcedureType(Parent,NamePos,TypeName,ptFunction);
  else
    ParseExcTokenError('procedure or function');
  end;
  Result.IsReferenceTo := True;
end;

function TPasParser.ParseComplexType(Parent: TPasElement = Nil): TPasType;
begin
  NextToken;
  case CurToken of
    tkProcedure:
      begin
        Result := TPasProcedureType(CreateElement(TPasProcedureType, '', Parent));
        ParseProcedureOrFunction(Result, TPasProcedureType(Result), ptProcedure, True);
        if CurToken = tkSemicolon then
          UngetToken;        // Unget semicolon
      end;
    tkFunction:
      begin
        Result := CreateFunctionType('', 'Result', Parent, False, CurSourcePos);
        ParseProcedureOrFunction(Result, TPasFunctionType(Result), ptFunction, True);
        if CurToken = tkSemicolon then
          UngetToken;        // Unget semicolon
      end;
  else
    UngetToken;
    Result := ParseType(Parent,CurSourcePos);
  end;
end;

function TPasParser.ParseArrayType(Parent: TPasElement;
  const NamePos: TPasSourcePos; const TypeName: string; PackMode: TPackMode
  ): TPasArrayType;

var
  S: string;
  ok: Boolean;
  RangeExpr: TPasExpr;

begin
  Result := TPasArrayType(CreateElement(TPasArrayType, TypeName, Parent, NamePos));
  ok := false;
  try
    Result.PackMode := PackMode;
    NextToken;
    S := '';
    case CurToken of
      tkSquaredBraceOpen:
        begin
          repeat
            NextToken;
            if po_arrayrangeexpr in Options then
              begin
              RangeExpr := DoParseExpression(Result);
              Result.AddRange(RangeExpr);
              end
            else if CurToken<>tkSquaredBraceClose then
               S := S+CurTokenText;
            if CurToken=tkSquaredBraceClose then
              break
            else if CurToken=tkComma then
              continue
            else if po_arrayrangeexpr in Options then
              ParseExcTokenError(']');
          until false;
          Result.IndexRange := S;
          ExpectToken(tkOf);
          Result.ElType := ParseType(Result,CurSourcePos);
        end;
      tkOf:
        begin
        NextToken;
        if CurToken = tkConst then
        else
          begin
          UngetToken;
          Result.ElType := ParseType(Result,CurSourcePos);
          end;
        end
      else
        ParseExc(nParserArrayTypeSyntaxError,SParserArrayTypeSyntaxError);
    end;
    // TPasProcedureType parsing has eaten the semicolon;
    // We know it was a local definition if the array def (result) is the parent
    if (Result.ElType is TPasProcedureType) and (Result.ElType.Parent=Result) then
      UnGetToken;
    Engine.FinishScope(stTypeDef,Result);
    ok := true;
  finally
    if not ok then
      Result.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
  end;
end;

function TPasParser.ParseFileType(Parent: TPasElement;
  const NamePos: TPasSourcePos; const TypeName: string): TPasFileType;
begin
  Result := TPasFileType(CreateElement(TPasFileType, TypeName, Parent, NamePos));
  NextToken;
  if CurToken=tkOf then
    Result.ElType := ParseType(Result,CurSourcePos)
  else
   UngetToken;
end;

function TPasParser.isEndOfExp(AllowEqual: Boolean = False; CheckHints: Boolean = True):Boolean;
const
  EndExprToken = [
    tkEOF, tkBraceClose, tkSquaredBraceClose, tkSemicolon, tkComma, tkColon,
    tkdo, tkdownto, tkelse, tkend, tkof, tkthen, tkto
  ];
begin
  Result := (CurToken in EndExprToken) or (CheckHints and IsCurTokenHint);
  if not (Result or AllowEqual) then
    Result := (Curtoken=tkEqual);
end;

function TPasParser.ExprToText(Expr: TPasExpr): string;
var
  C: TClass;
begin
  C := Expr.ClassType;
  if C=TPrimitiveExpr then
    Result := TPrimitiveExpr(Expr).Value
  else if C=TSelfExpr then
    Result := 'self'
  else if C=TBoolConstExpr then
    Result := BoolToStr(TBoolConstExpr(Expr).Value,'true','false')
  else if C=TNilExpr then
    Result := 'nil'
  else if C=TInheritedExpr then
    Result := 'inherited'
  else if C=TUnaryExpr then
    Result := OpcodeStrings[TUnaryExpr(Expr).OpCode]+ExprToText(TUnaryExpr(Expr).Operand)
  else if C=TBinaryExpr then
    begin
    Result := ExprToText(TBinaryExpr(Expr).left);
    if OpcodeStrings[TBinaryExpr(Expr).OpCode]<>'' then
      Result := Result+OpcodeStrings[TBinaryExpr(Expr).OpCode]
    else
      Result := Result+' ';
    Result := Result+ExprToText(TBinaryExpr(Expr).right)
    end
  else if C=TParamsExpr then
    begin
    case TParamsExpr(Expr).Kind of
      pekArrayParams: Result := ExprToText(TParamsExpr(Expr).Value)
        +'['+ArrayExprToText(TParamsExpr(Expr).Params)+']';
      pekFuncParams: Result := ExprToText(TParamsExpr(Expr).Value)
        +'('+ArrayExprToText(TParamsExpr(Expr).Params)+')';
      pekSet: Result := '['+ArrayExprToText(TParamsExpr(Expr).Params)+']';
      else ParseExc(nErrUnknownOperatorType,SErrUnknownOperatorType,[ExprKindNames[TParamsExpr(Expr).Kind]]);
    end;
    end
  else
    ParseExc(nErrUnknownOperatorType,SErrUnknownOperatorType,['TPasParser.ExprToText: '+Expr.ClassName]);
end;

function TPasParser.ArrayExprToText(Expr: TPasExprArray): string;
var
  i: Integer;
begin
  Result := '';
  for i := 0 to length(Expr)-1 do
    begin
    if i>0 then
      Result := Result+',';
    Result := Result+ExprToText(Expr[i]);
    end;
end;

function TPasParser.ResolveTypeReference(Name: string; Parent: TPasElement): TPasType;
var
  SS: Boolean;
  Ref: TPasElement;
begin
  Ref := Nil;
  SS := (not (po_ResolveStandardTypes in FOptions)) and isSimpleTypeToken(Name);
  if not SS then
    begin
    Ref := Engine.FindElement(Name);
    if Ref=nil then
      begin
      {$IFDEF VerbosePasResolver}
      {AllowWriteln}
      if po_resolvestandardtypes in FOptions then
        begin
        writeln('ERROR: TPasParser.ParseSimpleType resolver failed to raise an error');
        ParseExcExpectedIdentifier;
        end;
      {AllowWriteln-}
      {$ENDIF}
      end
    else if not (Ref is TPasType) then
      ParseExc(nParserExpectedTypeButGot,SParserExpectedTypeButGot,[Ref.ElementTypeName]);
    end;
  if (Ref=Nil) then
    Result := TPasUnresolvedTypeRef(CreateElement(TPasUnresolvedTypeRef,Name,Parent))
  else
    begin
    Ref.AddRef{$IFDEF CheckPasTreeRefCount}('ResolveTypeReference'){$ENDIF};
    Result := TPasType(Ref);
    end;
end;

function TPasParser.ParseParams(AParent: TPasElement; ParamsKind: TPasExprKind;
  AllowFormatting: Boolean = False): TParamsExpr;
var
  Params : TParamsExpr;
  Expr   : TPasExpr;
  PClose : TToken;

begin
  Result := nil;
  if ParamsKind in [pekArrayParams, pekSet] then
    begin
    if CurToken<>tkSquaredBraceOpen then
      ParseExc(nParserExpectTokenError,SParserExpectTokenError,['[']);
    PClose := tkSquaredBraceClose;
    end
  else
    begin
    if CurToken<>tkBraceOpen then
      ParseExc(nParserExpectTokenError,SParserExpectTokenError,['(']);
    PClose := tkBraceClose;
    end;

  Params := TParamsExpr(CreateElement(TParamsExpr,'',AParent,CurTokenPos));
  try
    Params.Kind := ParamsKind;
    NextToken;
    if not isEndOfExp(false,false) then
      begin
      repeat
        Expr := DoParseExpression(Params);
        if not Assigned(Expr) then
          ParseExcSyntaxError;
        Params.AddParam(Expr);
        if (CurToken=tkColon) then
          if not AllowFormatting then
            ParseExc(nParserExpectTokenError,SParserExpectTokenError,[','])
          else
            begin
            NextToken;
            Expr.format1 := DoParseExpression(Expr);
            if (CurToken=tkColon) then
              begin
              NextToken;
              Expr.format2 := DoParseExpression(Expr);
              end;
            end;
        if not (CurToken in [tkComma, PClose]) then
          ParseExc(nParserExpectTokenError,SParserExpectTokenError,[',']);

        if CurToken = tkComma then
          begin
          NextToken;
          if CurToken = PClose then
            begin
            //ErrorExpected(parser, 'identifier');
            ParseExcSyntaxError;
            end;
          end;
      until CurToken=PClose;
      end;
    NextToken;
    Result := Params;
  finally
    if Result=nil then
      Params.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
  end;
end;

function TPasParser.TokenToExprOp(const AToken: TToken): TExprOpCode;
begin
  Result := eopNone;
  case AToken of
    tkMul                  : Result := eopMultiply;
    tkPlus                 : Result := eopAdd;
    tkMinus                : Result := eopSubtract;
    tkDivision             : Result := eopDivide;
    tkLessThan             : Result := eopLessThan;
    tkEqual                : Result := eopEqual;
    tkGreaterThan          : Result := eopGreaterThan;
    tkAt                   : Result := eopAddress;
    tkAtAt                 : Result := eopMemAddress;
    tkNotEqual             : Result := eopNotEqual;
    tkLessEqualThan        : Result := eopLessthanEqual;
    tkGreaterEqualThan     : Result := eopGreaterThanEqual;
    tkPower                : Result := eopPower;
    tkSymmetricalDifference: Result := eopSymmetricalDifference;
    tkIs                   : Result := eopIs;
    tkAs                   : Result := eopAs;
    tkSHR                  : Result := eopSHR;
    tkSHL                  : Result := eopSHL;
    tkAnd                  : Result := eopAnd;
    tkOr                   : Result := eopOR;
    tkXor                  : Result := eopXOR;
    tkMod                  : Result := eopMod;
    tkDiv                  : Result := eopDiv;
    tkNot                  : Result := eopNot;
    tkIn                   : Result := eopIn;
    tkDot                  : Result := eopSubIdent;
    tkCaret                : Result := eopDeref;
  else
    ParseExc(nParserNotAnOperand, SParserNotAnOperand,
      [Byte(AToken), TokenInfos[AToken]]);
  end;
end;

function TPasParser.ParseExprOperand(AParent: TPasElement): TPasExpr;

  function IsWriteOrStr(P: TPasExpr): boolean;

  var
    N: string;
  begin
    Result := P is TPrimitiveExpr;
    if Result then
      begin
      N := LowerCase(TPrimitiveExpr(P).Value);
      // We should actually resolve this to system.NNN
      Result := (N='write') or (N='str') or (N='writeln') or (N='writestr');
      end;
  end;

  procedure HandleSelf(var Last: TPasExpr);

  var
    b      : TBinaryExpr;
    optk   : TToken;

  begin
    NextToken;
    if CurToken = tkDot then
      begin // self.Write(EscapeText(AText));
      optk := CurToken;
      NextToken;
      b := CreateBinaryExpr(AParent,Last, ParseExprOperand(AParent), TokenToExprOp(optk));
      if not Assigned(b.right) then
        begin
        b.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
        ParseExcExpectedIdentifier;
        end;
      Last := b;
      end;
    UngetToken;
  end;

  function IsSpecialize: boolean;
  var
    LookAhead, i: Integer;

    function Next: boolean;
    begin
      if LookAhead=FTokenRingSize then Exit(false);
      NextToken;
      inc(LookAhead);
      Result := true;
    end;

  begin
    Result := false;
    LookAhead := 0;
    CheckToken(tkLessThan);
    try
      Next;
      if not (CurToken in [tkIdentifier,tkself]) then Exit;
      while Next do
        case CurToken of
        tkDot:
          begin
          if not Next then Exit;
          if not (CurToken in [tkIdentifier,tkself,tktrue,tkfalse]) then Exit;
          end;
        tkComma:
          begin
          if not Next then Exit;
          if not (CurToken in [tkIdentifier,tkself]) then Exit;
          end;
        tkLessThan:
          begin
          // e.g. A<B<
          // not a valid comparison, could be a specialization -> good enough
          Exit(true);
          end;
        tkGreaterThan:
          begin
          // e.g. A<B>
          Exit(true);
          end;
        else
          Exit;
        end;
    finally
      for i := 1 to LookAhead do
        UngetToken;
    end;
  end;

var
  Last, Func, Expr: TPasExpr;
  Params: TParamsExpr;
  Bin: TBinaryExpr;
  ok, CanSpecialize: Boolean;
  aName: string;
  ISE: TInlineSpecializeExpr;
  ST: TPasSpecializeType;
  SrcPos, ScrPos: TPasSourcePos;
  ProcType: TProcType;
  ProcExpr: TProcedureExpr;

begin
  Result := nil;
  CanSpecialize := false;
  aName := '';
  case CurToken of
    tkString: Last := CreatePrimitiveExpr(AParent,pekString,CurTokenString);
    tkChar:   Last := CreatePrimitiveExpr(AParent,pekString,CurTokenText);
    tkNumber: Last := CreatePrimitiveExpr(AParent,pekNumber,CurTokenString);
    tkIdentifier: begin
      CanSpecialize := true;
      aName := CurTokenText;
      if CompareText(aName,'self')=0 then begin
        Last := CreateSelfExpr(AParent);
        HandleSelf(Last);
      end else
        Last := CreatePrimitiveExpr(AParent,pekIdent,aName);
    end;
    tkfalse, tktrue:    Last := CreateBoolConstExpr(AParent,pekBoolConst, CurToken=tktrue);
    tknil:              Last := CreateNilExpr(AParent);
    tkSquaredBraceOpen: Last := ParseParams(AParent,pekSet);
    tkinherited: begin
      //inherited; inherited function
      Last := CreateInheritedExpr(AParent);
      NextToken;
      if (CurToken=tkIdentifier) then begin
        SrcPos := CurTokenPos;
        Bin := CreateBinaryExpr(AParent,Last,ParseExprOperand(AParent),eopNone,SrcPos);
        if not Assigned(Bin.right) then begin
          Bin.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
          ParseExcExpectedIdentifier;
        end;
        Exit(Bin);
      end;
      UngetToken;
    end;
    tkself: begin
      CanSpecialize := true;
      aName := CurTokenText;
      Last := CreateSelfExpr(AParent);
      HandleSelf(Last);
    end;
    tkprocedure,tkfunction: begin
      if not IsAnonymousProcAllowed(AParent) then
        ParseExcExpectedIdentifier;
      if CurToken=tkprocedure then
        ProcType := ptAnonymousProcedure
      else
        ProcType := ptAnonymousFunction;
      ProcExpr := nil;
      try
        ProcExpr := TProcedureExpr(CreateElement(TProcedureExpr,'',AParent,visPublic));
        ProcExpr.Proc := TPasAnonymousProcedure(ParseProcedureOrFunctionDecl(ProcExpr,ProcType));
        Result := ProcExpr;
      finally
        if Result=nil then
          ProcExpr.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
      end;
      Exit; // do not allow postfix operators . ^. [] ()
    end;
    tkCaret: begin
      // is this still needed?
      // ^A..^_ characters. See #16341
      NextToken;
      if not (length(CurTokenText)=1) or not (CurTokenText[1] in ['A'..'_']) then begin
        UngetToken;
        ParseExcExpectedIdentifier;
      end;
      Last := CreatePrimitiveExpr(AParent,pekString, '^'+CurTokenText);
    end;
    else
      ParseExcExpectedIdentifier;
  end;
  Result := Last;
  ok := false;
  ISE := nil;
  try
    if Last.Kind<>pekSet then NextToken;
    if not (Last.Kind in [pekNumber,pekString,pekSet,pekIdent,pekSelf,pekNil]) then begin
      ok := true;
      Exit;
    end;
    Func := Last;
    repeat
      case CurToken of
        tkDot: begin
          ScrPos := CurTokenPos;
          NextToken;
          if CurToken in [tkIdentifier,tktrue,tkfalse,tkself] then begin // true and false are sub identifiers as well
            aName := aName+'.'+CurTokenString;
            Expr := CreatePrimitiveExpr(AParent,pekIdent,CurTokenString);
            AddToBinaryExprChain(Result,Expr,eopSubIdent,ScrPos);
            Func := Expr;
            NextToken;
          end else begin
            UngetToken;
            ParseExcExpectedIdentifier;
          end;
        end;
        tkBraceOpen, tkSquaredBraceOpen: begin
          if CurToken=tkBraceOpen then
            Params := ParseParams(AParent,pekFuncParams,IsWriteOrStr(Func))
          else
            Params := ParseParams(AParent,pekArrayParams);
          if not Assigned(Params) then Exit;
          Params.Value := Result;
          Result.Parent := Params;
          Result := Params;
          CanSpecialize := false;
          Func := nil;
        end;
        tkCaret: begin
          Result := CreateUnaryExpr(AParent,Result,TokenToExprOp(CurToken));
          NextToken;
          CanSpecialize := false;
          Func := nil;
        end;
        tkLessThan: begin
          SrcPos := CurTokenPos;
          if not CanSpecialize or not IsSpecialize then
            break
          else begin
            // an inline specialization (e.g. A<B,C>)
            ISE := TInlineSpecializeExpr(CreateElement(TInlineSpecializeExpr,'',AParent,SrcPos));
            ISE.Kind := pekSpecialize;
            ST := TPasSpecializeType(CreateElement(TPasSpecializeType,'',ISE,SrcPos));
            ISE.DestType := ST;
            ReadSpecializeArguments(ST);
            ST.DestType := ResolveTypeReference(aName,ST);
            ST.Expr := Result;
            Result := ISE;
            ISE := nil;
            CanSpecialize := false;
            NextToken;
          end;
          Func := nil;
        end
        else
          break;
      end;
    until false;
    ok := true;
  finally
    if not ok then begin
      Result.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
      ISE.Free;
    end;
  end;
end;

function TPasParser.ParseExpIdent(AParent: TPasElement): TPasExpr;
begin
  Result := ParseExprOperand(AParent);
end;

function TPasParser.OpLevel(t: TToken): Integer;
begin
  case t of
  //  tkDot:
  //    Result := 5;
    tknot,tkAt,tkAtAt:
      Result := 4;
    tkMul, tkDivision, tkdiv, tkmod, tkand, tkShl,tkShr, tkas, tkPower, tkis:
      // Note that "is" has same precedence as "and" in Delphi and fpc, even though
      // some docs say otherwise. e.g. "Obj is TObj and aBool"
      Result := 3;
    tkPlus, tkMinus, tkor, tkxor:
      Result := 2;
    tkEqual, tkNotEqual, tkLessThan, tkLessEqualThan, tkGreaterThan, tkGreaterEqualThan, tkin:
      Result := 1;
  else
    Result := 0;
  end;
end;

function TPasParser.DoParseExpression(AParent: TPaselement; InitExpr: TPasExpr;
  AllowEqual: Boolean): TPasExpr;
type
  TOpStackItem = record
    Token: TToken;
    SrcPos: TPasSourcePos;
  end;

var
  ExpStack : TFPList; // list of TPasExpr
  OpStack  : array of TOpStackItem;
  OpStackTop: integer;
  PrefixCnt: Integer;
  x        : TPasExpr;
  i        : Integer;
  TempOp   : TToken;
  NotBinary: Boolean;

const
  PrefixSym = [tkPlus, tkMinus, tknot, tkAt, tkAtAt]; // + - not @ @@
  BinaryOP  = [tkMul, tkDivision, tkdiv, tkmod,  tkDotDot,
               tkand, tkShl,tkShr, tkas, tkPower,
               tkPlus, tkMinus, tkor, tkxor, tkSymmetricalDifference,
               tkEqual, tkNotEqual, tkLessThan, tkLessEqualThan,
               tkGreaterThan, tkGreaterEqualThan, tkin, tkis];

  function PopExp: TPasExpr;
  begin
    if ExpStack.Count>0 then begin
      Result := TPasExpr(ExpStack[ExpStack.Count-1]);
      ExpStack.Delete(ExpStack.Count-1);
    end else
      Result := nil;
  end;

  procedure PushOper(Token: TToken);
  begin
    inc(OpStackTop);
    if OpStackTop=length(OpStack) then
      SetLength(OpStack,length(OpStack)*2+4);
    OpStack[OpStackTop].Token := Token;
    OpStack[OpStackTop].SrcPos := CurTokenPos;
  end;

  function PeekOper: TToken;
  begin
    if OpStackTop>=0 then Result := OpStack[OpStackTop].Token
    else Result := tkEOF;
  end;

  function PopOper(out SrcPos: TPasSourcePos): TToken;
  begin
    Result := PeekOper;
    if Result=tkEOF then
      SrcPos := DefPasSourcePos
    else
      begin
      SrcPos := OpStack[OpStackTop].SrcPos;
      dec(OpStackTop);
      end;
  end;

  procedure PopAndPushOperator;
  var
    t      : TToken;
    xright : TPasExpr;
    xleft  : TPasExpr;
    bin    : TBinaryExpr;
    SrcPos: TPasSourcePos;
  begin
    t := PopOper(SrcPos);
    xright := PopExp;
    xleft := PopExp;
    if t=tkDotDot then
      begin
      bin := CreateBinaryExpr(AParent,xleft,xright,eopNone,SrcPos);
      bin.Kind := pekRange;
      end
    else
      bin := CreateBinaryExpr(AParent,xleft,xright,TokenToExprOp(t),SrcPos);
    ExpStack.Add(bin);
  end;

var
  AllowedBinaryOps: Set of TToken;
  SrcPos: TPasSourcePos;
  ArrParams: TParamsExpr;

begin
  AllowedBinaryOps := BinaryOP;
  if not AllowEqual then
    Exclude(AllowedBinaryOps,tkEqual);
  {$ifdef VerbosePasParser}
  //DumpCurToken('Entry',iaIndent);
  {$endif}
  Result := nil;
  ExpStack := TFPList.Create;
  SetLength(OpStack,4);
  OpStackTop := -1;
  try
    repeat
      NotBinary := True;
      PrefixCnt := 0;
      if not Assigned(InitExpr) then
        begin

        // parse prefix operators
        while CurToken in PrefixSym do
          begin
          PushOper(CurToken);
          inc(PrefixCnt);
          NextToken;
          end;

        if (CurToken = tkBraceOpen) then
          begin
          NextToken;
          x := DoParseExpression(AParent);
          if not Assigned(x) then
            ParseExcSyntaxError;
          if (CurToken<>tkBraceClose) then
            begin
            x.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
            CheckToken(tkBraceClose);
            end;
          NextToken;
          repeat
            case CurToken of
            tkCaret:
              begin
              // for expressions like (ppdouble)^^;
              x := CreateUnaryExpr(AParent,x, TokenToExprOp(tkCaret));
              NextToken;
              end;
            tkBraceOpen:
              begin
              // for expressions like (a+b)(0);
              ArrParams := ParseParams(AParent,pekFuncParams,False);
              ArrParams.Value := x;
              x.Parent := ArrParams;
              x := ArrParams;
              end;
            tkSquaredBraceOpen:
              begin
              // for expressions like (PChar(a)+10)[0];
              ArrParams := ParseParams(AParent,pekArrayParams,False);
              ArrParams.Value := x;
              x.Parent := ArrParams;
              x := ArrParams;
              end;
            tkDot:
              begin
              // for expressions like (TObject(m)).Free;
              NextToken;
              x := CreateBinaryExpr(AParent,x, ParseExprOperand(AParent), TokenToExprOp(tkDot));
              end
            else
              break;
            end;
          until false;
          end
        else
          begin
          x := ParseExprOperand(AParent);
          if not Assigned(x) then
            ParseExcSyntaxError;
          end;
        ExpStack.Add(x);

        for i := 1 to PrefixCnt do
          begin
          TempOp := PopOper(SrcPos);
          x := PopExp;
          if (TempOp=tkMinus) and (x.Kind=pekRange) then
            begin
            TBinaryExpr(x).Left := CreateUnaryExpr(x, TBinaryExpr(x).left,
                                                 eopSubtract, SrcPos);
            ExpStack.Add(x);
            end
          else
            ExpStack.Add(CreateUnaryExpr(AParent, x, TokenToExprOp(TempOp), SrcPos));
          end;
        end
      else
        begin
        // the first part of the expression has been parsed externally.
        // this is used by Constant Expression parser (CEP) parsing only,
        // whenever it makes a false assuming on constant expression type.
        // i.e: SI_PAD_SIZE = ((128/sizeof(longint)) - 3);
        //
        // CEP assumes that it's array or record, because the expression
        // starts with "(". After the first part is parsed, the CEP meets "-"
        // that assures, it's not an array expression. The CEP should give the
        // first part back to the expression parser, to get the correct
        // token tree according to the operations priority.
        //
        // quite ugly. type information is required for CEP to work clean
        ExpStack.Add(InitExpr);
        InitExpr := nil;
        end;
      if (CurToken in AllowedBinaryOPs) then
        begin
        // process operators of higher precedence than next operator
        NotBinary := False;
        TempOp := PeekOper;
        while (OpStackTop>=0) and (OpLevel(TempOp)>=OpLevel(CurToken)) do begin
          PopAndPushOperator;
          TempOp := PeekOper;
        end;
        PushOper(CurToken);
        NextToken;
        end;
       //Writeln('Bin ',NotBinary ,' or EOE ',isEndOfExp, ' Ex ',Assigned(x),' stack ',ExpStack.Count);
    until NotBinary or isEndOfExp(AllowEqual, NotBinary);

    if not NotBinary then ParseExcExpectedIdentifier;

    while OpStackTop>=0 do PopAndPushOperator;

    // only 1 expression should be left on the OpStack
    if ExpStack.Count<>1 then
      ParseExcSyntaxError;
    Result := TPasExpr(ExpStack[0]);
    Result.Parent := AParent;

  finally
    {$ifdef VerbosePasParser}
    if not Assigned(Result) then
      DumpCurToken('Exiting (no result)',iaUndent)
    else
      DumpCurtoken('Exiting (Result: "'+Result.GetDeclaration(true)+'") ',iaUndent);
    {$endif}
    if not Assigned(Result) then begin
      // expression error!
      for i := 0 to ExpStack.Count-1 do
        TPasExpr(ExpStack[i]).Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
    end;
    SetLength(OpStack,0);
    ExpStack.Free;
  end;
end;


function GetExprIdent(p: TPasExpr): string;
begin
  Result := '';
  if not Assigned(p) then Exit;
  if (p.ClassType=TPrimitiveExpr) and (p.Kind=pekIdent) then
    Result := TPrimitiveExpr(p).Value
  else if (p.ClassType=TSelfExpr) then
    Result := 'Self';
end;

function TPasParser.DoParseConstValueExpression(AParent: TPasElement): TPasExpr;
// sets CurToken to token behind expression

  function lastfield:boolean;

  begin
    Result := CurToken<>tkSemicolon;
    if not Result then
     begin
       NextToken;
       if CurToken=tkBraceClose then
         Result := true
       else
         UngetToken;
     end;
  end;

  procedure ReadArrayValues(x: TPasExpr);
  var
    a: TArrayValues;
  begin
    Result := nil;
    a := nil;
    try
      a := CreateArrayValues(AParent);
      if x<>nil then
        begin
        a.AddValues(x);
        x := nil;
        end;
      repeat
        NextToken;
        a.AddValues(DoParseConstValueExpression(a));
      until CurToken<>tkComma;
      Result := a;
    finally
      if Result=nil then
        begin
        a.Free;
        x.Free;
        end;
    end;
  end;

var
  x , v: TPasExpr;
  n: string;
  r: TRecordValues;
begin
  if CurToken <> tkBraceOpen then
    Result := DoParseExpression(AParent)
  else begin
    Result := nil;
    if Engine.NeedArrayValues(AParent) then
      ReadArrayValues(nil)
    else
      begin
      NextToken;
      x := DoParseConstValueExpression(AParent);
      case CurToken of
        tkComma: // array of values (a,b,c);
          ReadArrayValues(x);

        tkColon: // record field (a:xxx;b:yyy;c:zzz);
          begin
          if not (x is TPrimitiveExpr) then
            CheckToken(tkBraceClose);
          r := nil;
          try
            n := GetExprIdent(x);
            r := CreateRecordValues(AParent);
            NextToken;
            v := DoParseConstValueExpression(r);
            r.AddField(TPrimitiveExpr(x), v);
            x := nil;
            if not lastfield then
              repeat
                n := ExpectIdentifier;
                x := CreatePrimitiveExpr(r,pekIdent,n);
                ExpectToken(tkColon);
                NextToken;
                v := DoParseConstValueExpression(AParent);
                r.AddField(TPrimitiveExpr(x), v);
                x := nil;
              until lastfield; // CurToken<>tkSemicolon;
            Result := r;
          finally
            if Result=nil then
              begin
              r.Free;
              x.Free;
              end;
          end;
          end;
      else
        // Binary expression!  ((128 div sizeof(longint)) - 3);
        Result := DoParseExpression(AParent,x);
        if CurToken<>tkBraceClose then
          begin
          ReleaseAndNil(TPasElement(Result){$IFDEF CheckPasTreeRefCount},'CreateElement'{$ENDIF});
          ParseExc(nParserExpectedCommaRBracket,SParserExpectedCommaRBracket);
          end;
        NextToken;
        if CurToken <> tkSemicolon then // the continue of expression
          Result := DoParseExpression(AParent,Result);
        Exit;
      end;
      end;
    if CurToken<>tkBraceClose then
      begin
      ReleaseAndNil(TPasElement(Result){$IFDEF CheckPasTreeRefCount},'CreateElement'{$ENDIF});
      ParseExc(nParserExpectedCommaRBracket,SParserExpectedCommaRBracket);
      end;
    NextToken;
  end;
end;

function TPasParser.CheckOverloadList(AList: TFPList; AName: string; out
  OldMember: TPasElement): TPasOverloadedProc;

var
  I: Integer;

begin
  Result := Nil;
  I := 0;
  While (Result=Nil) and (I<AList.Count) do
    begin
    OldMember := TPasElement(AList[i]);
    if CompareText(OldMember.Name, AName) = 0 then
      begin
      if OldMember is TPasOverloadedProc then
        Result := TPasOverloadedProc(OldMember)
      else
        begin
        Result := TPasOverloadedProc(CreateElement(TPasOverloadedProc, AName, OldMember.Parent));
        OldMember.Parent := Result;
        Result.Visibility := OldMember.Visibility;
        Result.Overloads.Add(OldMember);
        Result.SourceFilename := OldMember.SourceFilename;
        Result.SourceLinenumber := OldMember.SourceLinenumber;
        Result.DocComment := Oldmember.DocComment;
        AList[i] := Result;
        end;
      end;
    Inc(I);
    end;
  if Result=Nil then
    OldMember := Nil;
end;

procedure TPasParser.AddProcOrFunction(Decs: TPasDeclarations;
  AProc: TPasProcedure);
var
  I: Integer;
  OldMember: TPasElement;
  OverloadedProc: TPasOverloadedProc;
begin
  with Decs do
    begin
    if not (po_nooverloadedprocs in Options) then
      OverloadedProc := CheckOverloadList(Functions,AProc.Name,OldMember)
    else
      OverloadedProc := nil;
    if (OverloadedProc<>Nil) then
      begin
      OverLoadedProc.Overloads.Add(AProc);
      if (OldMember<>OverloadedProc) then
        begin
        I := Declarations.IndexOf(OldMember);
        if I<>-1 then
          Declarations[i] := OverloadedProc;
        end;
      end
    else
      begin
      Declarations.Add(AProc);
      Functions.Add(AProc);
      end;
    end;
end;

// Return the parent of a function declaration. This is AParent,
// except when AParent is a class/record and the function is overloaded.
// Then the parent is the overload object.
function TPasParser.CheckIfOverloaded(AParent: TPasElement; const AName: string): TPasElement;
var
  Member: TPasElement;
  OverloadedProc: TPasOverloadedProc;

begin
  Result := AParent;
  if (not (po_nooverloadedprocs in Options)) and (AParent is TPasMembersType) then
    begin
    OverloadedProc := CheckOverLoadList(TPasMembersType(AParent).Members,AName,Member);
    if (OverloadedProc<>Nil) then
      Result := OverloadedProc;
    end;
end;

procedure TPasParser.ParseMain(var Module: TPasModule);
begin
  Module := nil;
  NextToken;
  SaveComments;
  case CurToken of
    tkUnit:
      ParseUnit(Module);
    tkProgram:
      ParseProgram(Module);
    tkLibrary:
      ParseLibrary(Module);
    tkEOF:
      CheckToken(tkprogram);
  else
    UngetToken;
    ParseProgram(Module,True);
  end;
end;

// Starts after the "unit" token
procedure TPasParser.ParseUnit(var Module: TPasModule);
var
  AUnitName: string;
  StartPos: TPasSourcePos;
  HasFinished: Boolean;
begin
  StartPos := CurTokenPos;
  Module := nil;
  AUnitName := ExpectIdentifier;
  NextToken;
  while CurToken = tkDot do
    begin
    ExpectIdentifier;
    AUnitName := AUnitName + '.' + CurTokenString;
    NextToken;
    end;
  UngetToken;
  Module := TPasModule(CreateElement(TPasModule, AUnitName, Engine.Package, StartPos));
  FCurModule := Module;
  HasFinished := true;
  try
    if Assigned(Engine.Package) then
      begin
      Module.PackageName := Engine.Package.Name;
      Engine.Package.Modules.Add(Module);
      Module.AddRef{$IFDEF CheckPasTreeRefCount}('TPasPackage.Modules'){$ENDIF};
      end;
    CheckHint(Module,True);
    ExpectToken(tkInterface);
    if po_StopOnUnitInterface in Options then
      begin
      HasFinished := false;
      {$IFDEF VerbosePasResolver}
      writeln('TPasParser.ParseUnit pause parsing after unit name ',CurModule.Name);
      {$ENDIF}
      Exit;
      end;
    ParseInterface;
    if (Module.InterfaceSection<>nil)
        and (Module.InterfaceSection.PendingUsedIntf<>nil) then
      begin
      HasFinished := false;
      {$IFDEF VerbosePasResolver}
      writeln('TPasParser.ParseUnit pause parsing after interface uses list ',CurModule.Name);
      {$ENDIF}
      end;
    if (Module.ImplementationSection<>nil)
        and (Module.ImplementationSection.PendingUsedIntf<>nil) then
      begin
      HasFinished := false;
      {$IFDEF VerbosePasResolver}
      writeln('TPasParser.ParseUnit pause parsing after implementation uses list ',CurModule.Name);
      {$ENDIF}
      end;
    if HasFinished then
      FinishedModule;
  finally
    if HasFinished then
      FCurModule := nil; // clear module if there is an error or finished parsing
  end;
end;

function TPasParser.GetLastSection: TPasSection;
begin
  Result := nil;
  if FCurModule=nil then
    Exit; // parse completed
  if CurModule is TPasProgram then
    Result := TPasProgram(CurModule).ProgramSection
  else if CurModule is TPasLibrary then
    Result := TPasLibrary(CurModule).LibrarySection
  else if (CurModule.ClassType=TPasModule) or (CurModule is TPasUnitModule) then
    begin
    if CurModule.ImplementationSection<>nil then
      Result := CurModule.ImplementationSection
    else
      Result := CurModule.InterfaceSection; // might be nil
    end;
end;

function TPasParser.CanParseContinue(out Section: TPasSection): boolean;
begin
  Result := false;
  Section := nil;
  if FCurModule=nil then
    Exit; // parse completed
  if (LastMsg<>'') and (LastMsgType<=mtError) then
    begin
    {$IF defined(VerbosePasResolver) or defined(VerboseUnitQueue)}
    writeln('TPasParser.CanParseContinue ',CurModule.Name,' LastMsg="',LastMsgType,':',LastMsg,'"');
    {$ENDIF}
    Exit;
    end;
  if (Scanner.LastMsg<>'') and (Scanner.LastMsgType<=mtError) then
    begin
    {$IF defined(VerbosePasResolver) or defined(VerboseUnitQueue)}
    writeln('TPasParser.CanParseContinue ',CurModule.Name,' Scanner.LastMsg="',Scanner.LastMsgType,':',Scanner.LastMsg,'"');
    {$ENDIF}
    Exit;
    end;

  Section := GetLastSection;
  if Section=nil then
    if (po_StopOnUnitInterface in Options)
        and ((CurModule is TPasUnitModule) or (CurModule.ClassType=TPasModule))
        and (CurModule.InterfaceSection=nil) then
      Exit(true)
    else
      begin
      {$IFDEF VerboseUnitQueue}
      writeln('TPasParser.CanParseContinue ',CurModule.Name,' no LastSection');
      {$ENDIF}
      Exit(false);
      end;
  Result := Section.PendingUsedIntf=nil;
  {$IFDEF VerboseUnitQueue}
  writeln('TPasParser.CanParseContinue ',CurModule.Name,' Result=',Result,' ',Section.ElementTypeName);
  {$ENDIF}
end;

procedure TPasParser.ParseContinue;
// continue parsing after stopped due to pending uses
var
  Section: TPasSection;
  HasFinished: Boolean;
begin
  if CurModule=nil then
    ParseExcTokenError('TPasParser.ParseContinue missing module');
  {$IFDEF VerbosePasParser}
  writeln('TPasParser.ParseContinue ',CurModule.Name);
  {$ENDIF}
  if not CanParseContinue(Section) then
    ParseExcTokenError('TPasParser.ParseContinue missing section');
  HasFinished := true;
  try
    if Section=nil then
      begin
      // continue after unit name
      ParseInterface;
      end
    else
      begin
      // continue after uses clause
      Engine.FinishScope(stUsesClause,Section);
      ParseDeclarations(Section);
      end;
    Section := GetLastSection;
    if Section=nil then
      ParseExc(nErrNoSourceGiven,'[20180306112327]');
    if Section.PendingUsedIntf<>nil then
      HasFinished := false;
    if HasFinished then
      FinishedModule;
  finally
    if HasFinished then
      FCurModule := nil; // clear module if there is an error or finished parsing
  end;
end;

// Starts after the "program" token
procedure TPasParser.ParseProgram(var Module: TPasModule; SkipHeader: Boolean = False);
var
  PP: TPasProgram;
  Section: TProgramSection;
  N: string;
  StartPos: TPasSourcePos;
  HasFinished: Boolean;
  {$IFDEF VerbosePasResolver}
  aSection: TPasSection;
  {$ENDIF}
begin
  StartPos := CurTokenPos;
  if SkipHeader then
    N := ChangeFileExt(Scanner.CurFilename,'')
  else
    begin
    N := ExpectIdentifier;
    NextToken;
    while CurToken = tkDot do
      begin
      ExpectIdentifier;
      N := N + '.' + CurTokenString;
      NextToken;
      end;
    UngetToken;
    end;
  Module := nil;
  PP := TPasProgram(CreateElement(TPasProgram, N, Engine.Package, StartPos));
  Module := PP;
  HasFinished := true;
  FCurModule := Module;
  try
    if Assigned(Engine.Package) then
    begin
      Module.PackageName := Engine.Package.Name;
      Engine.Package.Modules.Add(Module);
    end;
    if not SkipHeader then
      begin
      NextToken;
      if (CurToken=tkBraceOpen) then
        begin
        PP.InputFile := ExpectIdentifier;
        NextToken;
        if not (CurToken in [tkBraceClose,tkComma]) then
          ParseExc(nParserExpectedCommaRBracket,SParserExpectedCommaRBracket);
        if (CurToken=tkComma) then
          PP.OutPutFile := ExpectIdentifier;
        ExpectToken(tkBraceClose);
        NextToken;
        end;
      if (CurToken<>tkSemicolon) then
        ParseExcTokenError(';');
      end;
    Section := TProgramSection(CreateElement(TProgramSection, '', CurModule));
    PP.ProgramSection := Section;
    ParseOptionalUsesList(Section);
    HasFinished := Section.PendingUsedIntf=nil;
    if not HasFinished then
      begin
      {$IFDEF VerbosePasResolver}
      {AllowWriteln}
      writeln('TPasParser.ParseProgram pause parsing after uses list of "',CurModule.Name,'"');
      if CanParseContinue(aSection) then
        begin
        writeln('TPasParser.ParseProgram Section=',Section.ClassName,' Section.PendingUsedIntf=',Section.PendingUsedIntf<>nil);
        if aSection<>nil then
          writeln('TPasParser.ParseProgram aSection=',aSection.ClassName,' ',Section=aSection);
        ParseExc(nErrNoSourceGiven,'[20180305172432] ');
        end;
      {AllowWriteln-}
      {$ENDIF}
      Exit;
      end;
    ParseDeclarations(Section);
    FinishedModule;
  finally
    if HasFinished then
      FCurModule := nil; // clear module if there is an error or finished parsing
  end;
end;

// Starts after the "library" token
procedure TPasParser.ParseLibrary(var Module: TPasModule);
var
  PP: TPasLibrary;
  Section: TLibrarySection;
  N: string;
  StartPos: TPasSourcePos;
  HasFinished: Boolean;

begin
  StartPos := CurTokenPos;
  N := ExpectIdentifier;
  NextToken;
  while CurToken = tkDot do
    begin
    ExpectIdentifier;
    N := N + '.' + CurTokenString;
    NextToken;
    end;
  UngetToken;
  Module := nil;
  PP := TPasLibrary(CreateElement(TPasLibrary, N, Engine.Package, StartPos));
  Module := PP;
  HasFinished := true;
  FCurModule := Module;
  try
    if Assigned(Engine.Package) then
    begin
      Module.PackageName := Engine.Package.Name;
      Engine.Package.Modules.Add(Module);
    end;
    NextToken;
    if (CurToken<>tkSemicolon) then
        ParseExcTokenError(';');
    Section := TLibrarySection(CreateElement(TLibrarySection, '', CurModule));
    PP.LibrarySection := Section;
    ParseOptionalUsesList(Section);
    HasFinished := Section.PendingUsedIntf=nil;
    if not HasFinished then
      Exit;
    ParseDeclarations(Section);
    FinishedModule;
  finally
    if HasFinished then
      FCurModule := nil; // clear module if there is an error or finished parsing
  end;
end;

procedure TPasParser.ParseOptionalUsesList(ASection: TPasSection);
// checks if next token is Uses keyword and reads the uses list
begin
  NextToken;
  CheckImplicitUsedUnits(ASection);
  if CurToken=tkuses then
    ParseUsesList(ASection)
  else
    UngetToken;
  Engine.CheckPendingUsedInterface(ASection);
  if ASection.PendingUsedIntf<>nil then
    Exit;
  Engine.FinishScope(stUsesClause,ASection);
end;

// Starts after the "interface" token
procedure TPasParser.ParseInterface;
var
  Section: TInterfaceSection;
begin
  if LogEvent(pleInterface) then
    DoLog(mtInfo,nLogStartInterface,SLogStartInterface);
  Section := TInterfaceSection(CreateElement(TInterfaceSection, '', CurModule));
  CurModule.InterfaceSection := Section;
  ParseOptionalUsesList(Section);
  if Section.PendingUsedIntf<>nil then
    Exit;
  ParseDeclarations(Section); // this also parses the Implementation section
end;

// Starts after the "implementation" token
procedure TPasParser.ParseImplementation;
var
  Section: TImplementationSection;
begin
  Section := TImplementationSection(CreateElement(TImplementationSection, '', CurModule));
  CurModule.ImplementationSection := Section;
  ParseOptionalUsesList(Section);
  if Section.PendingUsedIntf<>nil then
    Exit;
  ParseDeclarations(Section);
end;

procedure TPasParser.ParseInitialization;
var
  Section: TInitializationSection;
  SubBlock: TPasImplElement;
begin
  Section := TInitializationSection(CreateElement(TInitializationSection, '', CurModule,CurTokenPos));
  CurModule.InitializationSection := Section;
  repeat
    NextToken;
    if (CurToken=tkend) then
    begin
      ExpectToken(tkDot);
      Engine.FinishScope(stInitialFinalization,Section);
      Exit;
    end
    else if (CurToken=tkfinalization) then
    begin
      Engine.FinishScope(stInitialFinalization,Section);
      ParseFinalization;
      Exit;
    end
    else if CurToken<>tkSemiColon then
    begin
      UngetToken;
      ParseStatement(Section,SubBlock);
      if SubBlock=nil then
        ExpectToken(tkend);
    end;
  until false;
end;

procedure TPasParser.ParseFinalization;
var
  Section: TFinalizationSection;
  SubBlock: TPasImplElement;
begin
  Section := TFinalizationSection(CreateElement(TFinalizationSection, '', CurModule));
  CurModule.FinalizationSection := Section;
  repeat
    NextToken;
    if (CurToken=tkend) then
    begin
      ExpectToken(tkDot);
      Engine.FinishScope(stInitialFinalization,Section);
      Exit;
    end
    else if CurToken<>tkSemiColon then
    begin
      UngetToken;
      ParseStatement(Section,SubBlock);
      if SubBlock=nil then
        ExpectToken(tkend);
    end;
  until false;
end;

function TPasParser.GetProcTypeFromToken(tk: TToken; IsClass: Boolean
  ): TProcType;
begin
  Result := ptProcedure;
  case tk of
    tkProcedure :
      if IsClass then
        Result := ptClassProcedure
      else
        Result := ptProcedure;
    tkFunction:
      if IsClass then
        Result := ptClassFunction
      else
        Result := ptFunction;
    tkConstructor:
      if IsClass then
        Result := ptClassConstructor
      else
        Result := ptConstructor;
    tkDestructor:
      if IsClass then
        Result := ptClassDestructor
      else
        Result := ptDestructor;
    tkOperator:
      if IsClass then
        Result := ptClassOperator
      else
        Result := ptOperator;
    else
      ParseExc(nParserNotAProcToken, SParserNotAProcToken);
  end;
end;

procedure TPasParser.ParseDeclarations(Declarations: TPasDeclarations);
var
  CurBlock: TDeclType;

  procedure SetBlock(NewBlock: TDeclType);
  begin
    if CurBlock=NewBlock then Exit;
    if CurBlock=declType then
      Engine.FinishScope(stTypeSection,Declarations);
    CurBlock := NewBlock;
    Scanner.SetForceCaret(NewBlock=declType);
  end;

var
  ConstEl: TPasConst;
  ResStrEl: TPasResString;
  TypeEl: TPasType;
  ClassEl: TPasClassType;
  ArrEl: TPasArrayType;
  List: TFPList;
  i,j: Integer;
  VarEl: TPasVariable;
  ExpEl: TPasExportSymbol;
  PropEl: TPasProperty;
  TypeName: string;
  PT: TProcType;
  NamePos: TPasSourcePos;
  ok: Boolean;
  Proc: TPasProcedure;
  RecordEl: TPasRecordType;
begin
  CurBlock := declNone;
  while True do
  begin
    if CurBlock in [DeclNone,declConst,declType] then
      Scanner.SetTokenOption(toOperatorToken)
    else
      Scanner.UnSetTokenOption(toOperatorToken);
    NextToken;
    Scanner.SkipGlobalSwitches := true;
  //  writeln('TPasParser.ParseDeclarations Token=',CurTokenString,' ',CurToken, ' ',scanner.CurFilename);
    case CurToken of
    tkend:
      begin
      if (CurModule is TPasProgram) and (CurModule.InitializationSection=Nil) then
        ParseExcTokenError('begin');
      ExpectToken(tkDot);
      break;
      end;
    tkimplementation:
      if (Declarations is TInterfaceSection) then
        begin
        if not Engine.InterfaceOnly then
          begin
          if LogEvent(pleImplementation) then
            DoLog(mtInfo,nLogStartImplementation,SLogStartImplementation);
          SetBlock(declNone);
          ParseImplementation;
          end;
        break;
        end;
    tkinitialization:
      if (Declarations is TInterfaceSection)
      or ((Declarations is TImplementationSection) and not (Declarations is TProgramSection)) then
        begin
        SetBlock(declNone);
        ParseInitialization;
        break;
        end;
    tkfinalization:
      if (Declarations is TInterfaceSection)
      or ((Declarations is TImplementationSection) and not (Declarations is TProgramSection)) then
        begin
        SetBlock(declNone);
        ParseFinalization;
        break;
        end;
    tkUses:
      if Declarations.ClassType=TInterfaceSection then
        ParseExcTokenError(TokenInfos[tkimplementation])
      else if Declarations is TPasSection then
        ParseExcTokenError(TokenInfos[tkend])
      else
        ParseExcSyntaxError;
    tkConst:
      SetBlock(declConst);
    tkexports:
      SetBlock(declExports);
    tkResourcestring:
      if Declarations is TPasSection then
        SetBlock(declResourcestring)
      else
        begin
        {$IFDEF VerbosePasParser}
        writeln('TPasParser.ParseDeclarations ',Declarations.Parent.ClassName);
        {$ENDIF}
        ParseExc(nParserResourcestringsMustBeGlobal,SParserResourcestringsMustBeGlobal);
        end;
    tkType:
      SetBlock(declType);
    tkVar:
      SetBlock(declVar);
    tkThreadVar:
      SetBlock(declThreadVar);
    tkProperty:
      SetBlock(declProperty);
    tkProcedure, tkFunction, tkConstructor, tkDestructor, tkOperator:
      begin
      SetBlock(declNone);
      SaveComments;
      pt := GetProcTypeFromToken(CurToken);
      AddProcOrFunction(Declarations, ParseProcedureOrFunctionDecl(Declarations, pt));
      end;
    tkClass:
      begin
        SetBlock(declNone);
        SaveComments;
        NextToken;
        if CurToken in [tkprocedure,tkFunction,tkConstructor,tkDestructor] then
          begin
          pt := GetProcTypeFromToken(CurToken,True);
          AddProcOrFunction(Declarations,ParseProcedureOrFunctionDecl(Declarations, pt));
          end
        else
          CheckToken(tkprocedure);
      end;
    tkIdentifier:
      begin
        Scanner.UnSetTokenOption(toOperatorToken);
        SaveComments;
        case CurBlock of
          declConst:
            begin
              ConstEl := ParseConstDecl(Declarations);
              Declarations.Declarations.Add(ConstEl);
              Declarations.Consts.Add(ConstEl);
              Engine.FinishScope(stDeclaration,ConstEl);
            end;
          declResourcestring:
            begin
              ResStrEl := ParseResourcestringDecl(Declarations);
              Declarations.Declarations.Add(ResStrEl);
              Declarations.ResStrings.Add(ResStrEl);
              Engine.FinishScope(stResourceString,ResStrEl);
            end;
          declType:
            begin
            TypeEl := ParseTypeDecl(Declarations);
            // Scanner.SetForceCaret(OldForceCaret); // It may have been switched off
            if Assigned(TypeEl) then        // !!!
              begin
                Declarations.Declarations.Add(TypeEl);
                if (TypeEl.ClassType = TPasClassType)
                    and (not (po_keepclassforward in Options)) then
                begin
                  // Remove previous forward declarations, if necessary
                  for i := 0 to Declarations.Classes.Count - 1 do
                  begin
                    ClassEl := TPasClassType(Declarations.Classes[i]);
                    if CompareText(ClassEl.Name, TypeEl.Name) = 0 then
                    begin
                      Declarations.Classes.Delete(i);
                      for j := 0 to Declarations.Declarations.Count - 1 do
                        if CompareText(TypeEl.Name,
                          TPasElement(Declarations.Declarations[j]).Name) = 0 then
                        begin
                          Declarations.Declarations.Delete(j);
                          break;
                        end;
                      ClassEl.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
                      break;
                    end;
                  end;
                  // Add the new class to the class list
                  Declarations.Classes.Add(TypeEl)
                end else
                  Declarations.Types.Add(TypeEl);
              end;
            end;
          declExports:
            begin
            List := TFPList.Create;
            try
              ok := false;
              try
                ParseExportDecl(Declarations, List);
                ok := true;
              finally
                if not ok then
                  for i := 0 to List.Count - 1 do
                    TPasExportSymbol(List[i]).Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
              end;
              for i := 0 to List.Count - 1 do
              begin
                ExpEl := TPasExportSymbol(List[i]);
                Declarations.Declarations.Add(ExpEl);
                Declarations.ExportSymbols.Add(ExpEl);
              end;
            finally
              List.Free;
            end;
            end;
          declVar, declThreadVar:
            begin
              List := TFPList.Create;
              try
                ParseVarDecl(Declarations, List);
                for i := 0 to List.Count - 1 do
                begin
                  VarEl := TPasVariable(List[i]);
                  Declarations.Declarations.Add(VarEl);
                  Declarations.Variables.Add(VarEl);
                  Engine.FinishScope(stDeclaration,VarEl);
                end;
                CheckToken(tkSemicolon);
              finally
                List.Free;
              end;
            end;
          declProperty:
            begin
            PropEl := ParseProperty(Declarations,CurtokenString,visDefault,false);
            Declarations.Declarations.Add(PropEl);
            Declarations.Properties.Add(PropEl);
            Engine.FinishScope(stDeclaration,PropEl);
            end;
        else
          ParseExcSyntaxError;
        end;
      end;
    tkGeneric:
      begin
        if CurBlock <> declType then
          ParseExcSyntaxError;
        TypeName := ExpectIdentifier;
        NamePos := CurSourcePos;
        List := TFPList.Create;
        try
          ReadGenericArguments(List,Declarations);
          ExpectToken(tkEqual);
          NextToken;
          case CurToken of
            tkObject,
            tkClass :
              begin
              ClassEl := TPasClassType(CreateElement(TPasClassType,
                TypeName, Declarations, NamePos));
              Declarations.Declarations.Add(ClassEl);
              Declarations.Classes.Add(ClassEl);
              ClassEl.SetGenericTemplates(List);
              NextToken;
              DoParseClassType(ClassEl);
              CheckHint(ClassEl,True);
              Engine.FinishScope(stTypeDef,ClassEl);
              end;
           tkRecord:
             begin
             RecordEl := TPasRecordType(CreateElement(TPasRecordType,
               TypeName, Declarations, NamePos));
             Declarations.Declarations.Add(RecordEl);
             Declarations.Classes.Add(RecordEl);
             RecordEl.SetGenericTemplates(List);
             NextToken;
             ParseRecordFieldList(RecordEl,tkend,
                              (msAdvancedRecords in Scanner.CurrentModeSwitches)
                              and not (Declarations is TProcedureBody)
                              and (RecordEl.Name<>''));
             CheckHint(RecordEl,True);
             Engine.FinishScope(stTypeDef,RecordEl);
             end;
           tkArray:
             begin
             if List.Count<>1 then
               ParseExc(nParserGenericArray1Element,sParserGenericArray1Element);
             ArrEl := TPasArrayType(ParseArrayType(Declarations,NamePos,TypeName,pmNone));
             Declarations.Declarations.Add(ArrEl);
             Declarations.Types.Add(ArrEl);
             CheckHint(ArrEl,True);
             ArrEl.ElType.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
             ArrEl.ElType := TPasGenericTemplateType(List[0]);
             List.Clear;
             Engine.FinishScope(stTypeDef,ArrEl);
             end;
          else
            ParseExc(nParserGenericClassOrArray,SParserGenericClassOrArray);
          end;
        finally
          for i := 0 to List.Count-1 do
            TPasElement(List[i]).Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
          List.Free;
        end;
      end;
    tkbegin:
      begin
      if Declarations is TProcedureBody then
        begin
        Proc := Declarations.Parent as TPasProcedure;
        if pmAssembler in Proc.Modifiers then
          ParseExc(nParserExpectTokenError,SParserExpectTokenError,['asm']);
        SetBlock(declNone);
        ParseProcBeginBlock(TProcedureBody(Declarations));
        break;
        end
      else if (Declarations is TInterfaceSection)
      or (Declarations is TImplementationSection) then
        begin
        SetBlock(declNone);
        ParseInitialization;
        break;
        end
      else
        ParseExcSyntaxError;
      end;
    tkasm:
      begin
      if Declarations is TProcedureBody then
        begin
        Proc := Declarations.Parent as TPasProcedure;
        // Assembler keyword is optional in Delphi mode (bug 31690)
        if not ((pmAssembler in Proc.Modifiers) or (msDelphi in CurrentModeswitches)) then
          ParseExc(nParserExpectTokenError,SParserExpectTokenError,['begin']);
        SetBlock(declNone);
        ParseProcAsmBlock(TProcedureBody(Declarations));
        break;
        end
      else
        ParseExcSyntaxError;
      end;
    tklabel:
      begin
        SetBlock(declNone);
        if not (Declarations is TInterfaceSection) then
          ParseLabels(Declarations);
      end;
    tkSquaredBraceOpen:
      if [msPrefixedAttributes,msIgnoreAttributes]*CurrentModeSwitches<>[] then
        ParseAttribute(Declarations)
      else
        ParseExcSyntaxError;
    else
      ParseExcSyntaxError;
    end;
  end;
  SetBlock(declNone);
end;

function TPasParser.AddUseUnit(ASection: TPasSection;
  const NamePos: TPasSourcePos; AUnitName: string; NameExpr: TPasExpr;
  InFileExpr: TPrimitiveExpr): TPasUsesUnit;

  procedure CheckDuplicateInUsesList(AUnitName: string; UsesClause: TPasUsesClause);
  var
    i: Integer;
  begin
    if UsesClause=nil then Exit;
    for i := 0 to length(UsesClause)-1 do
      if CompareText(AUnitName,UsesClause[i].Name)=0 then
        ParseExc(nParserDuplicateIdentifier,SParserDuplicateIdentifier,[AUnitName]);
  end;

var
  UnitRef: TPasElement;
  UsesUnit: TPasUsesUnit;
begin
  Result := nil;
  UsesUnit := nil;
  UnitRef := nil;
  try
    {$IFDEF VerbosePasParser}
    writeln('TPasParser.AddUseUnit AUnitName=',AUnitName,' CurModule.Name=',CurModule.Name);
    {$ENDIF}
    if CompareText(AUnitName,CurModule.Name)=0 then
      begin
      if CompareText(AUnitName,'System')=0 then
        Exit; // for compatibility ignore implicit use of system in system
      ParseExc(nParserDuplicateIdentifier,SParserDuplicateIdentifier,[AUnitName]);
      end;
    CheckDuplicateInUsesList(AUnitName,ASection.UsesClause);
    if ASection.ClassType=TImplementationSection then
      CheckDuplicateInUsesList(AUnitName,CurModule.InterfaceSection.UsesClause);

    UnitRef := Engine.FindModule(AUnitName,NameExpr,InFileExpr);
    if Assigned(UnitRef) then
      UnitRef.AddRef{$IFDEF CheckPasTreeRefCount}('TPasUsesUnit.Module'){$ENDIF}
    else
      UnitRef := TPasUnresolvedUnitRef(CreateElement(TPasUnresolvedUnitRef,
        AUnitName, ASection, NamePos));

    UsesUnit := TPasUsesUnit(CreateElement(TPasUsesUnit,AUnitName,ASection,NamePos));
    Result := ASection.AddUnitToUsesList(AUnitName,NameExpr,InFileExpr,UnitRef,UsesUnit);
    if InFileExpr<>nil then
      begin
      if UnitRef is TPasModule then
        begin
        if TPasModule(UnitRef).Filename='' then
          TPasModule(UnitRef).Filename := InFileExpr.Value;
        end
      else if UnitRef is TPasUnresolvedUnitRef then
        TPasUnresolvedUnitRef(UnitRef).FileName := InFileExpr.Value;
      end;
  finally
    if Result=nil then
      begin
      if UsesUnit<>nil then
        UsesUnit.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
      if NameExpr<>nil then
        NameExpr.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
      if InFileExpr<>nil then
        InFileExpr.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
      if UnitRef<>nil then
        UnitRef.Release{$IFDEF CheckPasTreeRefCount}('FindModule'){$ENDIF};
      end;
  end;
end;

procedure TPasParser.CheckImplicitUsedUnits(ASection: TPasSection);
var
  i: Integer;
  NamePos: TPasSourcePos;
begin
  if not (ASection.ClassType=TImplementationSection) Then // interface,program,library,package
    begin
    // load implicit units, like 'System'
    NamePos := CurSourcePos;
    for i := 0 to ImplicitUses.Count-1 do
      AddUseUnit(ASection,NamePos,ImplicitUses[i],nil,nil);
    end;
end;

procedure TPasParser.FinishedModule;
begin
  if Scanner<>nil then
    Scanner.FinishedModule;
  Engine.FinishScope(stModule,CurModule);
end;

// Starts after the "uses" token
procedure TPasParser.ParseUsesList(ASection: TPasSection);
var
  AUnitName, aName: string;
  NameExpr: TPasExpr;
  InFileExpr: TPrimitiveExpr;
  FreeExpr: Boolean;
  NamePos, SrcPos: TPasSourcePos;
  aModule: TPasModule;
begin
  Scanner.SkipGlobalSwitches := true;
  NameExpr := nil;
  InFileExpr := nil;
  FreeExpr := true;
  try
    Repeat
      FreeExpr := true;
      AUnitName := ExpectIdentifier;
      NamePos := CurSourcePos;
      NameExpr := CreatePrimitiveExpr(ASection,pekString,AUnitName);
      NextToken;
      while CurToken = tkDot do
      begin
        SrcPos := CurTokenPos;
        ExpectIdentifier;
        aName := CurTokenString;
        AUnitName := AUnitName + '.' + aName;
        AddToBinaryExprChain(NameExpr,
              CreatePrimitiveExpr(ASection,pekString,aName),eopSubIdent,SrcPos);
        NextToken;
      end;
      if (CurToken=tkin) then
        begin
        if (msDelphi in CurrentModeswitches) then
          begin
          aModule := ASection.GetModule;
          if (aModule<>nil)
              and ((aModule.ClassType=TPasModule) or (aModule is TPasUnitModule)) then
            CheckToken(tkSemicolon); // delphi does not allow in-filename in units
          end;
        ExpectToken(tkString);
        InFileExpr := CreatePrimitiveExpr(ASection,pekString,CurTokenString);
        NextToken;
        end;
      FreeExpr := false;
      AddUseUnit(ASection,NamePos,AUnitName,NameExpr,InFileExpr);
      InFileExpr := nil;
      NameExpr := nil;

      if not (CurToken in [tkComma,tkSemicolon]) then
        ParseExc(nParserExpectedCommaSemicolon,SParserExpectedCommaSemicolon);
    Until (CurToken=tkSemicolon);
  finally
    if FreeExpr then
      begin
      ReleaseAndNil(TPasElement(NameExpr){$IFDEF CheckPasTreeRefCount},'CreateElement'{$ENDIF});
      ReleaseAndNil(TPasElement(InFileExpr){$IFDEF CheckPasTreeRefCount},'CreateElement'{$ENDIF});
      end;
  end;
end;

// Starts after the variable name
function TPasParser.ParseConstDecl(Parent: TPasElement): TPasConst;

var
  OldForceCaret,ok: Boolean;

begin
  SaveComments;
  Result := TPasConst(CreateElement(TPasConst, CurTokenString, Parent));
  if Parent is TPasMembersType then
    Include(Result.VarModifiers,vmClass);
  ok := false;
  try
    NextToken;
    if CurToken = tkColon then
      begin
      if not (bsWriteableConst in Scanner.CurrentBoolSwitches) then
        Result.IsConst := true;
      OldForceCaret := Scanner.SetForceCaret(True);
      try
        Result.VarType := ParseType(Result,CurSourcePos);
      finally
        Scanner.SetForceCaret(OldForceCaret);
      end;
{      if Result.VarType is TPasRangeType then
        Ungettoken; // Range type stops on token after last range token}
      end
    else
      begin
      UngetToken;
      Result.IsConst := true;
      end;
    NextToken;
    if CurToken=tkEqual then
      begin
      NextToken;
      Result.Expr := DoParseConstValueExpression(Result);
      if (Result.VarType=Nil) and (Result.Expr.Kind=pekRange) then
        ParseExc(nParserNoConstRangeAllowed,SParserNoConstRangeAllowed);
      end
    else if (Result.VarType<>nil)
        and (po_ExtConstWithoutExpr in Options) then
      begin
      if (Parent is TPasClassType)
          and TPasClassType(Parent).IsExternal
          and (TPasClassType(Parent).ObjKind=okClass) then
        // typed const without expression is allowed in external class
        Result.IsConst := true
      else if CurToken=tkSemicolon then
        begin
        NextToken;
        if CurTokenIsIdentifier('external') then
          begin
          // typed external const without expression is allowed
          Result.IsConst := true;
          Include(Result.VarModifiers,vmExternal);
          NextToken;
          if CurToken in [tkString,tkIdentifier] then
            begin
            // external LibraryName;
            // external LibraryName name ExportName;
            // external name ExportName;
            if not CurTokenIsIdentifier('name') then
              Result.LibraryName := DoParseExpression(Result);
            if not CurTokenIsIdentifier('name') then
              ParseExcSyntaxError;
            NextToken;
            if not (CurToken in [tkChar,tkString,tkIdentifier]) then
              ParseExcTokenError(TokenInfos[tkString]);
            Result.ExportName := DoParseExpression(Parent);
            Result.IsConst := true; // external const is readonly
            end
          else if CurToken=tkSemicolon then
            // external;
          else
            ParseExcSyntaxError;
          end
        else
          begin
          UngetToken;
          CheckToken(tkEqual);
          end;
        end
      else
        CheckToken(tkEqual);
      end
    else
      CheckToken(tkEqual);
    UngetToken;
    CheckHint(Result,not (Parent is TPasMembersType));
    ok := true;
  finally
    if not ok then
      ReleaseAndNil(TPasElement(Result){$IFDEF CheckPasTreeRefCount},'CreateElement'{$ENDIF});
  end;
end;

// Starts after the variable name
function TPasParser.ParseResourcestringDecl(Parent: TPasElement): TPasResString;
var
  ok: Boolean;
begin
  SaveComments;
  Result := TPasResString(CreateElement(TPasResString, CurTokenString, Parent));
  ok := false;
  try
    ExpectToken(tkEqual);
    NextToken; // skip tkEqual
    Result.Expr := DoParseConstValueExpression(Result);
    UngetToken;
    CheckHint(Result,True);
    ok := true;
  finally
    if not ok then
      ReleaseAndNil(TPasElement(Result){$IFDEF CheckPasTreeRefCount},'CreateElement'{$ENDIF});
  end;
end;

procedure TPasParser.ParseAttribute(Parent: TPasElement);
var
  Expr: TPasExpr;
begin
  repeat
    // skip attribute
    // [name,name(param,param,...),...]
    // [name(param,name=param)]
    repeat
      ExpectIdentifier;
      NextToken;
    until CurToken<>tkDot;
    if CurToken=tkBraceOpen then
      begin
      repeat
        NextToken;
        if CurToken=tkBraceClose then
          break;
        Expr := DoParseConstValueExpression(Parent);
        Expr.Free;
      until CurToken<>tkComma;
      CheckToken(tkBraceClose);
      NextToken;
      end;
  until CurToken<>tkComma;
  CheckToken(tkSquaredBraceClose);
end;

procedure TPasParser.ReadGenericArguments(List: TFPList; Parent: TPasElement);

var
  N: string;
  T: TPasGenericTemplateType;

begin
  ExpectToken(tkLessThan);
  repeat
    N := ExpectIdentifier;
    T := TPasGenericTemplateType(CreateElement(TPasGenericTemplateType,N,Parent));
    List.Add(T);
    NextToken;
    if Curtoken = tkColon then
      begin
      T.TypeConstraint := ExpectIdentifier;
      NextToken;
      end;
    if not (CurToken in [tkComma,tkSemicolon,tkGreaterThan]) then
      ParseExc(nParserExpectToken2Error,SParserExpectToken2Error,
        [TokenInfos[tkComma], TokenInfos[tkColon], TokenInfos[tkGreaterThan]]);
  until CurToken = tkGreaterThan;
end;

procedure TPasParser.ReadSpecializeArguments(Spec: TPasSpecializeType);

var
  Name: string;
  Ref: TPasType;
  IsNested: Boolean;
  NestedSpec: TPasSpecializeType;
  Expr: TPasExpr;

begin
  CheckToken(tkLessThan);
  NextToken;
  Expr := nil;
  Ref := nil;
  NestedSpec := nil;
  try
    repeat
      if not (msDelphi in CurrentModeswitches) and (CurToken=tkspecialize) then
        begin
        IsNested := true;
        NextToken;
        end
      else
        IsNested := false;
      // read dotted identifier
      CheckToken(tkIdentifier);
      Expr := nil;
      Name := ReadDottedIdentifier(Spec,Expr,true);

      if CurToken=tkLessThan then
        begin
        // nested specialize
        // resolve type
        Ref := ResolveTypeReference(Name,Spec);
        // create nested specialize
        NestedSpec := TPasSpecializeType(CreateElement(TPasSpecializeType,'',Spec));
        NestedSpec.DestType := Ref;
        Ref := nil;
        NestedSpec.Expr := Expr;
        Expr := nil;
        // read nested specialize arguments
        ReadSpecializeArguments(NestedSpec);
        // add nested specialize
        Spec.AddParam(NestedSpec);
        NestedSpec := nil;
        NextToken;
        end
      else if IsNested then
        CheckToken(tkLessThan)
      else
        begin
        // simple type reference
        Spec.AddParam(Expr);
        Expr := nil;
        end;

      if CurToken=tkComma then
        begin
        NextToken;
        continue;
        end
      else if CurToken=tkshr then
        begin
        ChangeToken(tkGreaterThan);
        break;
        end
      else if CurToken=tkGreaterThan then
        break
      else
        ParseExc(nParserExpectToken2Error,SParserExpectToken2Error,
          [TokenInfos[tkComma], TokenInfos[tkGreaterThan]]);
    until false;
  finally
    Expr.Free;
    if Ref<>nil then Ref.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
    if NestedSpec<>nil then NestedSpec.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
  end;
end;

function TPasParser.ReadDottedIdentifier(Parent: TPasElement; out
  Expr: TPasExpr; NeedAsString: boolean): string;
var
  SrcPos: TPasSourcePos;
begin
  Expr := nil;
  if NeedAsString then
    Result := CurTokenString
  else
    Result := '';
  CheckToken(tkIdentifier);
  Expr := CreatePrimitiveExpr(Parent,pekIdent,CurTokenString);
  NextToken;
  while CurToken=tkDot do
    begin
    SrcPos := CurTokenPos;
    ExpectIdentifier;
    if NeedAsString then
      Result := Result+'.'+CurTokenString;
    AddToBinaryExprChain(Expr,CreatePrimitiveExpr(Parent,pekIdent,CurTokenString),
                         eopSubIdent,SrcPos);
    NextToken;
    end;
end;

// Starts after the type name
function TPasParser.ParseRangeType(AParent: TPasElement;
  const NamePos: TPasSourcePos; const TypeName: string; Full: Boolean
  ): TPasRangeType;

var
  PE: TPasExpr;
  ok: Boolean;

begin
  Result := TPasRangeType(CreateElement(TPasRangeType, TypeName, AParent, NamePos));
  ok := false;
  try
    if Full then
      begin
      if not (CurToken=tkEqual) then
        ParseExcTokenError(TokenInfos[tkEqual]);
      end;
    NextToken;
    PE := DoParseExpression(Result,Nil,False);
    if not ((PE is TBinaryExpr) and (TBinaryExpr(PE).Kind=pekRange)) then
      begin
      PE.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
      ParseExc(nRangeExpressionExpected,SRangeExpressionExpected);
      end;
    Result.RangeExpr := TBinaryExpr(PE);
    UngetToken;
    Engine.FinishScope(stTypeDef,Result);
    ok := true;
  finally
    if not ok then
      Result.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
  end;
end;

// Starts after Exports, on first identifier.
procedure TPasParser.ParseExportDecl(Parent: TPasElement; List: TFPList);
var
  E: TPasExportSymbol;
begin
  Repeat
    if List.Count<>0 then
      ExpectIdentifier;
    E := TPasExportSymbol(CreateElement(TPasExportSymbol,CurtokenString,Parent));
    List.Add(E);
    NextToken;
    if CurTokenIsIdentifier('INDEX') then
      begin
      NextToken;
      E.Exportindex := DoParseExpression(E,Nil)
      end
    else if CurTokenIsIdentifier('NAME') then
      begin
      NextToken;
      E.ExportName := DoParseExpression(E,Nil)
      end;
    if not (CurToken in [tkComma,tkSemicolon]) then
      ParseExc(nParserExpectedCommaSemicolon,SParserExpectedCommaSemicolon);
  until (CurToken=tkSemicolon);
end;

function TPasParser.ParseSpecializeType(Parent: TPasElement;
  const TypeName: string): TPasSpecializeType;

var
  ok: Boolean;
begin
  NextToken;
  Result := ParseSimpleType(Parent,CurSourcePos,TypeName) as TPasSpecializeType;
  ok := false;
  try
    Engine.FinishScope(stTypeDef,Result);
    ok := true;
  finally
    if not ok then
      Result.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
  end;
end;

function TPasParser.ParseProcedureType(Parent: TPasElement;
  const NamePos: TPasSourcePos; const TypeName: string; const PT: TProcType
  ): TPasProcedureType;

var
  ok: Boolean;
begin
  if PT in [ptFunction,ptClassFunction] then
    Result := CreateFunctionType(TypeName, 'Result', Parent, False, NamePos)
  else
    Result := TPasProcedureType(CreateElement(TPasProcedureType, TypeName, Parent, NamePos));
  ok := false;
  try
    ParseProcedureOrFunction(Result, TPasProcedureType(Result), PT, True);
    ok := true;
  finally
    if not ok then
      Result.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
  end;
end;

function TPasParser.ParseTypeDecl(Parent: TPasElement): TPasType;

var
  TypeName: string;
  NamePos: TPasSourcePos;
  OldForceCaret: Boolean;
  List: TFPList;
  i: Integer;

begin
  TypeName := CurTokenString;
  NamePos := CurSourcePos;
  List := Nil;
  OldForceCaret := Scanner.SetForceCaret(True);
  try
    NextToken;
    if (CurToken=tkLessThan) and (msDelphi in CurrentModeswitches) then
      List := TFPList.Create;
    UnGetToken; // ReadGenericArguments starts at <
    if Assigned(List) then
      ReadGenericArguments(List,Parent);
    ExpectToken(tkEqual);
    Result := ParseType(Parent,NamePos,TypeName,True,List);
  finally
    Scanner.SetForceCaret(OldForceCaret);
    if List<>nil then
      begin
      for i := 0 to List.Count-1 do
        TPasElement(List[i]).Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
      List.Free;
      end;
  end;
end;

function TPasParser.GetVariableValueAndLocation(Parent: TPasElement; out
  Value: TPasExpr; out AbsoluteExpr: TPasExpr; out Location: string): Boolean;

begin
  Value := Nil;
  AbsoluteExpr := Nil;
  Location := '';
  NextToken;
  Result := CurToken=tkEqual;
  if Result then
    begin
    NextToken;
    Value := DoParseConstValueExpression(Parent);
    end;
  if (CurToken=tkAbsolute) then
    begin
    Result := True;
    ExpectIdentifier;
    Location := CurTokenText;
    AbsoluteExpr := CreatePrimitiveExpr(Parent,pekIdent,CurTokenText);
    NextToken;
    While CurToken=tkDot do
      begin
      ExpectIdentifier;
      Location := Location+'.'+CurTokenText;
      AbsoluteExpr := CreateBinaryExpr(Parent,AbsoluteExpr,CreatePrimitiveExpr(Parent,pekIdent,CurTokenText),eopSubIdent);
      NextToken;
      end;
    UnGetToken;
    end
  else
    UngetToken;
end;

function TPasParser.GetVariableModifiers(Parent: TPasElement; out
  VarMods: TVariableModifiers; out LibName, ExportName: TPasExpr;
  const AllowedMods: TVariableModifiers): string;

var
  S: string;
  ExtMod: TVariableModifier;
begin
  Result := '';
  LibName := nil;
  ExportName := nil;
  VarMods := [];
  NextToken;
  if (vmCVar in AllowedMods) and CurTokenIsIdentifier('cvar') then
    begin
    Result := ';cvar';
    Include(VarMods,vmcvar);
    ExpectToken(tkSemicolon);
    NextToken;
    end;
  s := LowerCase(CurTokenText);
  if (vmExternal in AllowedMods) and (s='external') then
    ExtMod := vmExternal
  else if (vmPublic in AllowedMods) and (s='public') then
    ExtMod := vmPublic
  else if (vmExport in AllowedMods) and (s='export') then
    ExtMod := vmExport
  else
    begin
    UngetToken;
    Exit;
    end;
  Include(varMods,ExtMod);
  Result := Result+';'+CurTokenText;

  NextToken;
  if not (CurToken in [tkString,tkIdentifier]) then
    begin
    if (CurToken=tkSemicolon) and (ExtMod in [vmExternal,vmPublic]) then
      Exit;
    ParseExcSyntaxError;
    end;
  // export name exportname;
  // public;
  // public name exportname;
  // external;
  // external libname;
  // external libname name exportname;
  // external name exportname;
  if (ExtMod=vmExternal) and (CurToken in [tkString,tkIdentifier])
      and not (CurTokenIsIdentifier('name')) then
    begin
    Result := Result + ' ' + CurTokenText;
    LibName := DoParseExpression(Parent);
    end;
  if not CurTokenIsIdentifier('name') then
    ParseExcSyntaxError;
  NextToken;
  if not (CurToken in [tkChar,tkString,tkIdentifier]) then
    ParseExcTokenError(TokenInfos[tkString]);
  Result := Result + ' ' + CurTokenText;
  ExportName := DoParseExpression(Parent);
end;


// Full means that a full variable declaration is being parsed.
procedure TPasParser.ParseVarList(Parent: TPasElement; VarList: TFPList;
  AVisibility: TPasMemberVisibility; Full: Boolean);
// on Exception the VarList is restored, no need to Release the new elements
var
  i, oldListCount: Integer;
  value, aLibName, aExpName, absoluteExpr: TPasExpr;
  varType: TPasType;
  varEl: TPasVariable;
  h: TPasMemberHints;
  varMods, allowedVarMods: TVariableModifiers;
  d, mods, absoluteLocString: string;
  oldForceCaret, ok, ExternalStruct: Boolean;
begin
  value := Nil;
  aLibName := nil;
  aExpName := nil;
  absoluteExpr := nil;
  absoluteLocString := '';
  oldListCount := VarList.Count;
  ok := false;
  try
    d := SaveComments; // This means we support only one comment per 'list'.
    Repeat
      // create the TPasVariable here, so that SourceLineNumber is correct
      varEl := TPasVariable(CreateElement(TPasVariable,CurTokenString,Parent,
                                        AVisibility,CurTokenPos));
      VarList.Add(varEl);
      NextToken;
      if not (CurToken in [tkComma,tkColon]) then
        ParseExc(nParserExpectedCommaColon,SParserExpectedCommaColon);
      if CurToken=tkComma then
        ExpectIdentifier;
    Until (CurToken=tkColon);
    oldForceCaret := Scanner.SetForceCaret(True);
    try
      varType := ParseComplexType(varEl);
    finally
      Scanner.SetForceCaret(oldForceCaret);
    end;
    // read type
    for i := oldListCount to VarList.Count - 1 do
      begin
      varEl := TPasVariable(VarList[i]);
      // Writeln(varEl.Name, AVisibility);
      varEl.varType := varType;
      //varType.Parent := varEl; // this is wrong for references
      if (i>oldListCount) then
        varType.AddRef{$IFDEF CheckPasTreeRefCount}('TPasVariable.VarType'){$ENDIF};
      end;

    h := CheckHint(Nil,False);
    if Full then
      GetVariableValueAndLocation(varEl,value,absoluteExpr,absoluteLocString);
    if (VarList.Count>oldListCount+1) then
      begin
      // multiple variables
      if value<>nil then
        ParseExc(nParserOnlyOneVariableCanBeInitialized,SParserOnlyOneVariableCanBeInitialized);
      if absoluteExpr<>nil then
        ParseExc(nParserOnlyOneVariableCanBeAbsolute,SParserOnlyOneVariableCanBeAbsolute);
      end;
    TPasVariable(VarList[oldListCount]).Expr := value;
    value := nil;

    // Note: external members are allowed for non external classes too
    ExternalStruct := (msExternalClass in CurrentModeSwitches)
                    and (Parent is TPasMembersType);

    h := h+CheckHint(Nil,False);
    if Full or ExternalStruct then
      begin
      NextToken;
      if Curtoken<>tkSemicolon then
        UnGetToken;
      varEl := TPasVariable(VarList[0]);
      allowedVarMods := [];
      if ExternalStruct then
        allowedVarMods := [vmExternal]
      else
        allowedVarMods := [vmCVar,vmExternal,vmPublic,vmExport];
      mods := GetVariableModifiers(varEl,varMods,aLibName,aExpName,allowedVarMods);
      if (mods='') and (CurToken<>tkSemicolon) then
        NextToken;
      end
    else
      begin
      NextToken;
      varMods := [];
      mods := '';
      end;
    SaveComments(d);

    // connect
    for i := oldListCount to VarList.Count - 1 do
      begin
      varEl := TPasVariable(VarList[i]);
      // Writeln(varEl.Name, AVisibility);
      // procedure declaration eats the hints.
      if Assigned(varType) and (varType is TPasProcedureType) then
        varEl.Hints := varType.Hints
      else
        varEl.Hints := h;
      varEl.Modifiers := mods;
      varEl.VarModifiers := varMods;
      varEl.{%h-}AbsoluteLocation := absoluteLocString;
      if absoluteExpr<>nil then
        begin
        varEl.absoluteExpr := absoluteExpr;
        absoluteExpr := nil;
        end;
      if aLibName<>nil then
        begin
        varEl.LibraryName := aLibName;
        aLibName := nil;
        end;
      if aExpName<>nil then
        begin
        varEl.ExportName := aExpName;
        aExpName := nil;
        end;
      end;
    ok := true;
  finally
    if not ok then
      begin
      if aLibName<>nil then aLibName.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
      if aExpName<>nil then aExpName.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
      if absoluteExpr<>nil then absoluteExpr.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
      if value<>nil then value.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
      for i := oldListCount to VarList.Count-1 do
        TPasElement(VarList[i]).Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
      VarList.Count := oldListCount;
      end;
  end;
end;

procedure TPasParser.SetOptions(AValue: TPOptions);
begin
  if FOptions=AValue then Exit;
  FOptions := AValue;
  if Assigned(FScanner) then
    FScanner.Options := AValue;
end;

procedure TPasParser.OnScannerModeChanged(Sender: TObject;
  NewMode: TModeSwitch; Before: boolean; var Handled: boolean);
begin
  Engine.ModeChanged(Self,NewMode,Before,Handled);
end;

function TPasParser.SaveComments: string;
begin
  if Engine.NeedComments then
    FSavedComments := CurComments.Text; // Expensive, so don't do unless needed.
  Result := FSavedComments;
end;

function TPasParser.SaveComments(const AValue: string): string;
begin
  FSavedComments := AValue;
  Result := FSavedComments;
end;

function TPasParser.LogEvent(E: TPParserLogEvent): Boolean;
begin
  Result := E in FLogEvents;
end;

procedure TPasParser.SetLastMsg(MsgType: TMessageType; MsgNumber: integer;
  const Fmt: string; Args: array of {$ifdef pas2js}jsvalue{$else}const{$endif});
begin
  FLastMsgType := MsgType;
  FLastMsgNumber := MsgNumber;
  FLastMsgPattern := Fmt;
  FLastMsg := SafeFormat(Fmt,Args);
  CreateMsgArgs(FLastMsgArgs,Args);
end;

procedure TPasParser.DoLog(MsgType: TMessageType; MsgNumber: integer;
  const Msg: string; SkipSourceInfo: Boolean);
begin
  DoLog(MsgType,MsgNumber,Msg,[],SkipSourceInfo);
end;

procedure TPasParser.DoLog(MsgType: TMessageType; MsgNumber: integer;
  const Fmt: string; Args: array of {$ifdef pas2js}jsvalue{$else}const{$endif};
  SkipSourceInfo: Boolean);

var
  Msg: string;

begin
  if (Scanner<>nil) and Scanner.IgnoreMsgType(MsgType) then
    Exit;
  SetLastMsg(MsgType,MsgNumber,Fmt,Args);
  if Assigned(FOnLog) then
    begin
    Msg := MessageTypeNames[MsgType]+': ';
    if SkipSourceInfo or not assigned(scanner) then
      Msg := Msg+FLastMsg
    else
      Msg := Msg+Format('%s(%d,%d): %s',[Scanner.CurFilename,Scanner.CurRow,Scanner.CurColumn,FLastMsg]);
    FOnLog(Self,Msg);
    end;
end;

procedure TPasParser.ParseInlineVarDecl(Parent: TPasElement; List: TFPList;
  AVisibility: TPasMemberVisibility = VisDefault; ClosingBrace: Boolean = False);

var
  tt: TTokens;
begin
  ParseVarList(Parent,List,AVisibility,False);
  tt := [tkEnd,tkSemicolon];
  if ClosingBrace then
    Include(tt,tkBraceClose);
  if not (CurToken in tt) then
    ParseExc(nParserExpectedSemiColonEnd,SParserExpectedSemiColonEnd);
end;

// Starts after the variable name
procedure TPasParser.ParseVarDecl(Parent: TPasElement; List: TFPList);

begin
  ParseVarList(Parent,List,visDefault,True);
end;

// Starts after the opening bracket token
procedure TPasParser.ParseArgList(Parent: TPasElement; Args: TFPList; EndToken: TToken);
var
  IsUntyped, ok, LastHadDefaultValue: Boolean;
  Name: string;
  Value: TPasExpr;
  i, OldArgCount: Integer;
  Arg: TPasArgument;
  Access: TArgumentAccess;
  ArgType: TPasType;
begin
  LastHadDefaultValue := false;
  while True do
  begin
    OldArgCount := Args.Count;
    Access := argDefault;
    IsUntyped := False;
    ArgType := nil;
    NextToken;
    if CurToken = tkConst then
    begin
      Access := argConst;
      Name := ExpectIdentifier;
    end else if CurToken = tkConstRef then
    begin
      Access := argConstref;
      Name := ExpectIdentifier;
    end else if CurToken = tkVar then
    begin
      Access := ArgVar;
      Name := ExpectIdentifier;
    end else if (CurToken = tkIdentifier) and (UpperCase(CurTokenString) = 'OUT') then
    begin
      Access := ArgOut;
      Name := ExpectIdentifier;
    end else if CurToken = tkIdentifier then
      Name := CurTokenString
    else
      ParseExc(nParserExpectedConstVarID,SParserExpectedConstVarID);
    while True do
    begin
      Arg := TPasArgument(CreateElement(TPasArgument, Name, Parent));
      Arg.Access := Access;
      Args.Add(Arg);
      NextToken;
      if CurToken = tkColon then
        break
      else if ((CurToken = tkSemicolon) or (CurToken = tkBraceClose)) and
        (Access <> argDefault) then
      begin
        // found an untyped const or var argument
        UngetToken;
        IsUntyped := True;
        break
      end
      else if CurToken <> tkComma then
        ParseExc(nParserExpectedCommaColon,SParserExpectedCommaColon);
      NextToken;
      if CurToken = tkIdentifier then
        Name := CurTokenString
      else
        ParseExc(nParserExpectedConstVarID,SParserExpectedConstVarID);
    end;
    Value := Nil;
    if not IsUntyped then
      begin
      Arg := TPasArgument(Args[OldArgCount]);
      ArgType := ParseType(Arg,CurSourcePos);
      ok := false;
      try
        NextToken;
        if CurToken = tkEqual then
          begin
          if (Args.Count>OldArgCount+1) then
            begin
            ArgType.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
            ArgType := nil;
            ParseExc(nParserOnlyOneArgumentCanHaveDefault,SParserOnlyOneArgumentCanHaveDefault);
            end;
          if Parent is TPasProperty then
            ParseExc(nParserPropertyArgumentsCanNotHaveDefaultValues,
              SParserPropertyArgumentsCanNotHaveDefaultValues);
          NextToken;
          Value := DoParseExpression(Arg,Nil);
          // After this, we're on ), which must be unget.
          LastHadDefaultValue := true;
          end
        else if LastHadDefaultValue then
          ParseExc(nParserDefaultParameterRequiredFor,
            SParserDefaultParameterRequiredFor,[TPasArgument(Args[OldArgCount]).Name]);
        UngetToken;
        ok := true;
      finally
        if (not ok) and (ArgType<>nil) then
          ArgType.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
      end;
      end;

    for i := OldArgCount to Args.Count - 1 do
    begin
      Arg := TPasArgument(Args[i]);
      Arg.ArgType := ArgType;
      if Assigned(ArgType) then
        begin
        if (i > OldArgCount) then
          ArgType.AddRef{$IFDEF CheckPasTreeRefCount}('TPasArgument.ArgType'){$ENDIF};
        end;
      Arg.ValueExpr := Value;
      Value := Nil; // Only the first gets a value. OK, since var A,B: Integer = 1 is not allowed.
    end;

    for i := OldArgCount to Args.Count - 1 do
      Engine.FinishScope(stDeclaration,TPasArgument(Args[i]));

    NextToken;
    if (CurToken = tkIdentifier) and (LowerCase(CurTokenString) = 'location') then
      begin
        NextToken; // remove 'location'
        NextToken; // remove register
      end;
    if CurToken = EndToken then
      break;
    CheckToken(tkSemicolon);
  end;
end;


function TPasParser.CheckProcedureArgs(Parent: TPasElement; Args: TFPList;
  ProcType: TProcType): boolean;

begin
  NextToken;
  if CurToken=tkBraceOpen then
    begin
    Result := true;
    NextToken;
    if (CurToken<>tkBraceClose) then
      begin
      UngetToken;
      ParseArgList(Parent, Args, tkBraceClose);
      end;
    end
  else
    begin
    Result := false;
    case ProcType of
    ptOperator,ptClassOperator:
      ParseExc(nParserExpectedLBracketColon,SParserExpectedLBracketColon);
    ptAnonymousProcedure,ptAnonymousFunction:
      case CurToken of
      tkIdentifier, // e.g. procedure assembler
      tkbegin,tkvar,tkconst,tktype,tkprocedure,tkfunction:
        UngetToken;
      tkColon:
        if ProcType=ptAnonymousFunction then
          UngetToken
        else
          ParseExcTokenError('begin');
      else
        ParseExcTokenError('begin');
      end;
    else
      case CurToken of
        tkSemicolon, // e.g. procedure;
        tkColon, // e.g. function: id
        tkof, // e.g. procedure of object
        tkis, // e.g. procedure is nested
        tkIdentifier: // e.g. procedure cdecl;
          UngetToken;
      else
        ParseExcTokenError(';');
      end;
    end;
    end;
end;

procedure TPasParser.HandleProcedureModifier(Parent: TPasElement; pm: TProcedureModifier);

var
  Tok: string;
  P: TPasProcedure;
  E: TPasExpr;

  procedure AddModifier;
  begin
    if pm in P.Modifiers then
      ParseExcSyntaxError;
    P.AddModifier(pm);
  end;

begin
  P := TPasProcedure(Parent);
  if pm<>pmPublic then
    AddModifier;
  case pm of
  pmExternal:
    begin
    NextToken;
    if CurToken in [tkString,tkIdentifier] then
      begin
      // external libname
      // external libname name XYZ
      // external name XYZ
      Tok := UpperCase(CurTokenString);
      if not ((CurToken=tkIdentifier) and (Tok='NAME')) then
        begin
        E := DoParseExpression(Parent);
        if Assigned(P) then
          P.LibraryExpr := E;
        end;
      if CurToken=tkSemicolon then
        UnGetToken
      else
        begin
        Tok := UpperCase(CurTokenString);
        if ((CurToken=tkIdentifier) and (Tok='NAME')) then
          begin
          NextToken;
          if not (CurToken in [tkChar,tkString,tkIdentifier]) then
            ParseExcTokenError(TokenInfos[tkString]);
          E := DoParseExpression(Parent);
          if Assigned(P) then
            P.LibrarySymbolName := E;
          end;
        end;
      end
    else
      UngetToken;
    end;
  pmPublic:
    begin
    NextToken;
    if not CurTokenIsIdentifier('name') then
      begin
      if P.Parent is TPasMembersType then
        begin
        // public section starts
        UngetToken;
        UngetToken;
        Exit;
        end;
      AddModifier;
      CheckToken(tkSemicolon);
      Exit;
      end
    else
      begin
      AddModifier;
      NextToken;  // Should be "public name string".
      if not (CurToken in [tkString,tkIdentifier]) then
        ParseExcTokenError(TokenInfos[tkString]);
      E := DoParseExpression(Parent);
      if Parent is TPasProcedure then
        TPasProcedure(Parent).PublicName := E;
      if (CurToken <> tkSemicolon) then
        ParseExcTokenError(TokenInfos[tkSemicolon]);
      end;
    end;
  pmForward:
    begin
    if (Parent.Parent is TInterfaceSection) then
       begin
       ParseExc(nParserForwardNotInterface,SParserForwardNotInterface);
       UngetToken;
       end;
    end;
  pmMessage:
    begin
    Repeat
      NextToken;
      if CurToken<>tkSemicolon then
        begin
        if Parent is TPasProcedure then
          TPasProcedure(Parent).MessageName := CurtokenString;
        if (CurToken=tkString) and (Parent is TPasProcedure) then
          TPasProcedure(Parent).Messagetype := pmtString;
        end;
    until CurToken = tkSemicolon;
    UngetToken;
    end;
  pmDispID:
    begin
    TPasProcedure(Parent).DispIDExpr := DoParseExpression(Parent,Nil);
    if CurToken = tkSemicolon then
      UngetToken;
    end;
  end; // case
end;

procedure TPasParser.HandleProcedureTypeModifier(ProcType: TPasProcedureType;
  ptm: TProcTypeModifier);
begin
  if ptm in ProcType.Modifiers then
    ParseExcSyntaxError;
  Include(ProcType.Modifiers,ptm);
end;

// Next token is expected to be a "(", ";" or for a function ":". The caller
// will get the token after the final ";" as next token.

function TPasParser.DoCheckHint(Element: TPasElement): Boolean;

var
  ahint: TPasMemberHint;

begin
  Result := IsCurTokenHint(ahint);
  if Result then  // deprecated,platform,experimental,library, unimplemented etc
    begin
    Element.Hints := Element.Hints+[ahint];
    if aHint=hDeprecated then
      begin
      NextToken;
      if (CurToken<>tkString) then
        UngetToken
      else
        Element.HintMessage := CurTokenString;
      end;
    end;
end;

procedure TPasParser.ParseProcedureOrFunction(Parent: TPasElement;
  Element: TPasProcedureType; ProcType: TProcType; OfObjectPossible: Boolean);

  function FindInSection(AName: string; ASection: TPasSection): Boolean;

  var
    I: integer;
    Cn,FN: string;
    CT: TPasClassType;

  begin
    I := ASection.Functions.Count-1;
    While (I>=0) and (CompareText(TPasElement(ASection.Functions[I]).Name,AName)<>0) do
      Dec(I);
    Result := I<>-1;
    I := Pos('.',AName);
    if (not Result) and (I>0) then
      begin
      CN := Copy(AName,1,I-1);
      FN := AName;
      Delete(FN,1,I);
      I := ASection.Classes.Count-1;
      While not Result and (I>=0) do
        begin
        CT := TPasClassType(ASection.Classes[i]);
        if CompareText(CT.Name,CN)=0 then
          Result := CT.FindMember(TPasFunction, FN)<>Nil;
        Dec(I);
        end;
      end;
  end;

  procedure ConsumeSemi;
  begin
    NextToken;
    if (CurToken <> tkSemicolon) and IsCurTokenHint then
      UngetToken;
  end;

var
  tok: string;
  cc: TCallingConvention;
  pm: TProcedureModifier;
  resultEl: TPasResultElement;
  ok: Boolean;
  isProcType: Boolean; // false = procedure, true = procedure type
  isAnonymous: Boolean;
  ptm: TProcTypeModifier;
  modTokenCount: Integer;
  lastToken: TToken;
begin
  // Element must be non-nil. Removed all checks for not-nil.
  // If it is nil, the following fails anyway.
  CheckProcedureArgs(Element,Element.Args,ProcType);
  isProcType := not (Parent is TPasProcedure);
  isAnonymous := (not isProcType) and (ProcType in [ptAnonymousProcedure,ptAnonymousFunction]);
  case ProcType of
    ptFunction,ptClassFunction,ptAnonymousFunction: begin
      NextToken;
      if CurToken = tkColon then begin
        resultEl := TPasFunctionType(Element).resultEl;
        resultEl.ResultType := ParseType(resultEl,CurSourcePos);
      end
      // In Delphi mode, the signature in the implementation section can be
      // without result as it was declared
      // We actually check if the function exists in the interface section.
      else if (not isAnonymous)
        and (msDelphi in CurrentModeswitches)
        and (Assigned(CurModule.ImplementationSection)
          or (CurModule is TPasProgram))
      then begin
        if Assigned(CurModule.InterfaceSection) then
          ok := FindInSection(Parent.Name,CurModule.InterfaceSection)
        else if (CurModule is TPasProgram) and Assigned(TPasProgram(CurModule).ProgramSection) then
          ok := FindInSection(Parent.Name,TPasProgram(CurModule).ProgramSection)
        else
          ok := False;
        if not ok then
          CheckToken(tkColon)
        else begin
          CheckToken(tkSemiColon);
          UngetToken;
        end;
      end else begin
        // Raise error
        CheckToken(tkColon);
      end;
    end;
    ptOperator,ptClassOperator:
      begin
      NextToken;
      resultEl := TPasFunctionType(Element).resultEl;
      if (CurToken=tkIdentifier) then
        begin
        resultEl.Name := CurTokenName;
        ExpectToken(tkColon);
        end
      else
        if (CurToken=tkColon) then
          resultEl.Name := 'Result'
        else
          ParseExc(nParserExpectedColonID,SParserExpectedColonID);
        resultEl.ResultType := ParseType(resultEl,CurSourcePos);
      end;
  end;
  if OfObjectPossible then
    begin
    NextToken;
    if (CurToken = tkOf) then
      begin
      ExpectToken(tkObject);
      Element.IsOfObject := True;
      end
    else if (CurToken = tkIs) then
      begin
      expectToken(tkIdentifier);
      if (lowerCase(CurTokenString)<>'nested') then
        ParseExc(nParserExpectedNested,SParserExpectedNested);
      Element.IsNested := True;
      end
    else
      UnGetToken;
    end;
  modTokenCount := 0;
  //writeln('TPasParser.ParseProcedureOrFunction IsProcType=',isProcType,' IsAnonymous=',isAnonymous);
  Repeat
    inc(modTokenCount);
    //writeln('TPasParser.ParseProcedureOrFunction ',modTokenCount,' ',CurToken,' ',CurTokenText);
    lastToken := CurToken;
    NextToken;
    if (CurToken = tkEqual) and isProcType and (modTokenCount<=3) then
      begin
      // for example: const p: procedure = nil;
      UngetToken;
      Engine.FinishScope(stProcedureHeader,Element);
      Exit;
      end;
    if CurToken=tkSemicolon then
      begin
      if isAnonymous then
        CheckToken(tkbegin); // begin expected, but ; found
      if lastToken=tkSemicolon then
        ParseExcSyntaxError;
      continue;
      end
    else if TokenIsCallingConvention(CurTokenString,cc) then
      begin
      Element.CallingConvention := cc;
      if cc = ccSysCall then
      begin
        // remove LibBase
        NextToken;
        if CurToken=tkSemiColon then
          UngetToken
        else
          // remove legacy or basesysv on MorphOS syscalls
          begin
          if CurTokenIsIdentifier('legacy') or CurTokenIsIdentifier('BaseSysV') then
            NextToken;
          NextToken; // remove offset
          end;
      end;
      if isProcType then
        begin
        ExpectTokens([tkSemicolon,tkEqual]);
        if CurToken=tkEqual then
          UngetToken;
        end
      else if isAnonymous then
      else
        ExpectTokens([tkSemicolon]);
      end
    else if isAnonymous and TokenIsAnonymousProcedureModifier(Parent,CurTokenString,pm) then
      HandleProcedureModifier(Parent,pm)
    else if TokenIsProcedureTypeModifier(Parent,CurTokenString,ptm) then
      HandleProcedureTypeModifier(Element,ptm)
    else if (not isProcType) and (not isAnonymous)
        and TokenIsProcedureModifier(Parent,CurTokenString,pm) then
      HandleProcedureModifier(Parent,pm)
    else if (CurToken=tklibrary) and not isProcType and not isAnonymous then
      // library is a token and a directive.
      begin
      tok := UpperCase(CurTokenString);
      NextToken;
      if (tok<>'NAME') then
        begin
        if hLibrary in Element.Hints then
          ParseExcSyntaxError;
        Element.Hints := Element.Hints+[hLibrary];
        end
      else
        begin
        NextToken;  // Should be "export name astring".
        ExpectToken(tkSemicolon);
        end;
      end
    else if (not isAnonymous) and DoCheckHint(Element) then
      // deprecated,platform,experimental,library, unimplemented etc
      ConsumeSemi
    else if (CurToken=tkIdentifier) and (not isAnonymous)
        and (CompareText(CurTokenText,'alias')=0) then
      begin
      ExpectToken(tkColon);
      ExpectToken(tkString);
      if (Parent is TPasProcedure) then
        (Parent as TPasProcedure).AliasName := CurTokenText;
      ExpectToken(tkSemicolon);
      end
    else if (CurToken = tkSquaredBraceOpen) then
      begin
      if ([msPrefixedAttributes,msIgnoreAttributes]*CurrentModeswitches<>[]) then
        begin
        // [attribute]
        UngetToken;
        break;
        end
      else
        begin
        // ToDo: read FPC's [] modifiers, e.g. [public,alias:'']
        repeat
          NextToken;
          if CurToken in [tkSquaredBraceOpen,tkSemicolon] then
            CheckToken(tkSquaredBraceClose);
        until CurToken = tkSquaredBraceClose;
        ExpectToken(tkSemicolon);
        end;
      end
    else
      begin
      // not a modifier/hint/calling convention
      if lastToken=tkSemicolon then
        begin
        UngetToken;
        if isAnonymous then
          ParseExcSyntaxError;
        break;
        end
      else if isAnonymous then
        begin
        UngetToken;
        break;
        end
      else
        begin
        CheckToken(tkSemicolon);
        continue;
        end;
      end;
    // Writeln('Done: ',TokenInfos[Curtoken],' ',CurtokenString);
  Until false;
  if (ProcType in [ptOperator,ptClassOperator]) and (Parent is TPasOperator) then
    TPasOperator(Parent).CorrectName;
  Engine.FinishScope(stProcedureHeader,Element);
  if (not isProcType)
  and (not TPasProcedure(Parent).IsForward)
  and (not TPasProcedure(Parent).IsExternal)
  and ((Parent.Parent is TImplementationSection)
     or (Parent.Parent is TProcedureBody)
     or isAnonymous)
  then
    ParseProcedureBody(Parent);
  if not isProcType then
    Engine.FinishScope(stProcedure,Parent);
end;

// starts after the semicolon
procedure TPasParser.ParseProcedureBody(Parent: TPasElement);

var
  Body: TProcedureBody;

begin
  Body := TProcedureBody(CreateElement(TProcedureBody, '', Parent));
  TPasProcedure(Parent).Body := Body;
  ParseDeclarations(Body);
end;

function TPasParser.ParseMethodResolution(Parent: TPasElement
  ): TPasMethodResolution;
var
  ok: Boolean;
begin
  ok := false;
  Result := TPasMethodResolution(CreateElement(TPasMethodResolution,'',Parent));
  try
    if CurToken=tkfunction then
      Result.ProcClass := TPasFunction
    else
      Result.ProcClass := TPasProcedure;
    ExpectToken(tkIdentifier);
    Result.InterfaceName := CreatePrimitiveExpr(Result,pekIdent,CurTokenString);
    ExpectToken(tkDot);
    ExpectToken(tkIdentifier);
    Result.InterfaceProc := CreatePrimitiveExpr(Result,pekIdent,CurTokenString);
    ExpectToken(tkEqual);
    ExpectToken(tkIdentifier);
    Result.ImplementationProc := CreatePrimitiveExpr(Result,pekIdent,CurTokenString);
    NextToken;
    if CurToken=tkSemicolon then
    else if CurToken=tkend then
      UngetToken
    else
      CheckToken(tkSemicolon);
    ok := true;
  finally
    if not ok then
      Result.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
  end;
end;

function TPasParser.ParseProperty(Parent: TPasElement; const AName: string;
  AVisibility: TPasMemberVisibility; IsClassField: boolean): TPasProperty;

  function GetAccessorName(aParent: TPasElement; out Expr: TPasExpr): string;
  var
    params: TParamsExpr;
    param: TPasExpr;
    srcPos: TPasSourcePos;
  begin
    ExpectIdentifier;
    Result := CurTokenString;
    Expr := CreatePrimitiveExpr(aParent,pekIdent,CurTokenString);

    // read .subident.subident...
    repeat
      NextToken;
      if CurToken <> tkDot then break;
      srcPos := CurTokenPos;
      ExpectIdentifier;
      Result := Result + '.' + CurTokenString;
      AddToBinaryExprChain(Expr,CreatePrimitiveExpr(aParent,pekIdent,CurTokenString),
        eopSubIdent,srcPos);
    until false;

    // read optional array index
    if CurToken <> tkSquaredBraceOpen then
      UnGetToken
    else
      begin
      Result := Result + '[';
      params := TParamsExpr(CreateElement(TParamsExpr,'',aParent));
      params.Kind := pekArrayParams;
      params.Value := Expr;
      Expr.Parent := params;
      Expr := params;
      NextToken;
      param := nil;
      case CurToken of
        tkChar:             param := CreatePrimitiveExpr(aParent,pekString, CurTokenText);
        tkNumber:           param := CreatePrimitiveExpr(aParent,pekNumber, CurTokenString);
        tkIdentifier:       param := CreatePrimitiveExpr(aParent,pekIdent, CurTokenText);
        tkfalse, tktrue:    param := CreateBoolConstExpr(aParent,pekBoolConst, CurToken=tktrue);
      else
        ParseExcExpectedIdentifier;
      end;
      params.AddParam(param);
      Result := Result + CurTokenString;
      ExpectToken(tkSquaredBraceClose);
      Result := Result + ']';
      end;
    repeat
      NextToken;
      if CurToken <> tkDot then
        begin
        UngetToken;
        break;
        end;
      srcPos := CurTokenPos;
      ExpectIdentifier;
      Result := Result + '.' + CurTokenString;
      AddToBinaryExprChain(Expr,CreatePrimitiveExpr(aParent,pekIdent,CurTokenString),
                           eopSubIdent,srcPos);
    until false;
  end;

  procedure ParseImplements;
  var
    Identifier: string;
    Expr: TPasExpr;
    l: Integer;
  begin
    // comma list of identifiers
    repeat
      ExpectToken(tkIdentifier);
      l := length(Result.Implements);
      Identifier := ReadDottedIdentifier(Result,Expr,l=0);
      if l=0 then
        Result.ImplementsName := Identifier;
      SetLength(Result.Implements,l+1);
      Result.Implements[l] := Expr;
    until CurToken<>tkComma;
  end;

var
  isArray , ok, IsClass: Boolean;
  ObjKind: TPasObjKind;
begin
  Result := TPasProperty(CreateElement(TPasProperty,AName,Parent,AVisibility));
  if IsClassField then
    Include(Result.VarModifiers,vmClass);
  IsClass := (Parent<>nil) and (Parent.ClassType=TPasClassType);
  if IsClass then
    ObjKind := TPasClassType(Parent).ObjKind
  else
    ObjKind := okClass;
  ok := false;
  try
    NextToken;
    isArray := CurToken=tkSquaredBraceOpen;
    if isArray then
      begin
      ParseArgList(Result, Result.Args, tkSquaredBraceClose);
      NextToken;
      end;
    if CurToken = tkColon then
      begin
      Result.VarType := ParseType(Result,CurSourcePos);
      NextToken;
      end
    else if not IsClass then
      ParseExcTokenError(':');
    if CurTokenIsIdentifier('INDEX') then
      begin
      NextToken;
      Result.IndexExpr := DoParseExpression(Result);
      end;
    if CurTokenIsIdentifier('READ') then
      begin
      Result.ReadAccessorName := GetAccessorName(Result,Result.ReadAccessor);
      NextToken;
      end;
    if CurTokenIsIdentifier('WRITE') then
      begin
      Result.WriteAccessorName := GetAccessorName(Result,Result.WriteAccessor);
      NextToken;
      end;
    if IsClass and (ObjKind=okDispInterface) then
      begin
      if CurTokenIsIdentifier('READONLY') then
        begin
        Result.DispIDReadOnly := True;
        NextToken;
        end;
      if CurTokenIsIdentifier('DISPID') then
        begin
        NextToken;
        Result.DispIDExpr := DoParseExpression(Result,Nil);
        end;
      end;
    if IsClass and (ObjKind=okClass) and CurTokenIsIdentifier('IMPLEMENTS') then
      ParseImplements;
    if CurTokenIsIdentifier('STORED') then
      begin
      if not (ObjKind in [okClass]) then
        ParseExc(nParserXNotAllowedInY,SParserXNotAllowedInY,['STORED',ObjKindNames[ObjKind]]);
      NextToken;
      if CurToken = tkTrue then
        begin
        Result.StoredAccessorName := 'True';
        Result.StoredAccessor := CreateBoolConstExpr(Result,pekBoolConst,true);
        end
      else if CurToken = tkFalse then
        begin
        Result.StoredAccessorName := 'False';
        Result.StoredAccessor := CreateBoolConstExpr(Result,pekBoolConst,false);
        end
      else if CurToken = tkIdentifier then
        begin
        UngetToken;
        Result.StoredAccessorName := GetAccessorName(Result,Result.StoredAccessor);
        end
      else
        ParseExcSyntaxError;
      NextToken;
      end;
    if CurTokenIsIdentifier('DEFAULT') then
      begin
      if not (ObjKind in [okClass]) then
        ParseExc(nParserXNotAllowedInY,SParserXNotAllowedInY,['DEFAULT',ObjKindNames[ObjKind]]);
      if isArray then
        ParseExc(nParserArrayPropertiesCannotHaveDefaultValue,SParserArrayPropertiesCannotHaveDefaultValue);
      NextToken;
      Result.DefaultExpr := DoParseExpression(Result);
  //      NextToken;
      end
    else if CurtokenIsIdentifier('NODEFAULT') then
      begin
      if not (ObjKind in [okClass]) then
        ParseExc(nParserXNotAllowedInY,SParserXNotAllowedInY,['NODEFAULT',ObjKindNames[ObjKind]]);
      Result.IsNodefault := true;
      if Result.DefaultExpr<>nil then
        ParseExcSyntaxError;
      NextToken;
      end;
    // Here the property ends. There can still be a 'default'
    if CurToken = tkSemicolon then
      begin
      NextToken;
      if CurTokenIsIdentifier('DEFAULT') then
        begin
        if (Result.VarType<>Nil) and (not isArray) then
          ParseExc(nParserDefaultPropertyMustBeArray,SParserDefaultPropertyMustBeArray);
        NextToken;
        if CurToken = tkSemicolon then
          begin
          Result.IsDefault := True;
          NextToken;
          end
        end;
      // Handle hints
      while DoCheckHint(Result) do
        NextToken;
      if Result.Hints=[] then
        UngetToken;
      end
    else if CurToken=tkend then
      // ok
    else
      CheckToken(tkSemicolon);
    ok := true;
  finally
    if not ok then
      Result.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
  end;
end;

// Starts after the "begin" token
procedure TPasParser.ParseProcBeginBlock(Parent: TProcedureBody);
var
  BeginBlock: TPasImplBeginBlock;
  SubBlock: TPasImplElement;
  Proc: TPasProcedure;
begin
  BeginBlock := TPasImplBeginBlock(CreateElement(TPasImplBeginBlock, '', Parent));
  Parent.Body := BeginBlock;
  repeat
    NextToken;
//    writeln('TPasParser.ParseProcBeginBlock ',curtokenstring);
    if CurToken=tkend then
      break
    else if CurToken<>tkSemiColon then
    begin
      UngetToken;
      ParseStatement(BeginBlock,SubBlock);
      if SubBlock=nil then
        ExpectToken(tkend);
    end;
  until false;
  Proc := Parent.Parent as TPasProcedure;
  if Proc.GetProcTypeEnum in [ptAnonymousProcedure,ptAnonymousFunction] then
    NextToken
  else
    ExpectToken(tkSemicolon);
//  writeln('TPasParser.ParseProcBeginBlock ended ',curtokenstring);
end;

procedure TPasParser.ParseProcAsmBlock(Parent: TProcedureBody);
var
  AsmBlock: TPasImplAsmStatement;
begin
  AsmBlock := TPasImplAsmStatement(CreateElement(TPasImplAsmStatement,'',Parent));
  Parent.Body := AsmBlock;
  ParseAsmBlock(AsmBlock);
  NextToken;
  if not (Parent.Parent is TPasAnonymousProcedure) then
    CheckToken(tkSemicolon);
end;

procedure TPasParser.ParseAsmBlock(AsmBlock: TPasImplAsmStatement);

var
  LastToken: TToken;
  p: PTokenRec;

  function atEndOfAsm: Boolean;

  begin
    Result := (CurToken=tkEnd) and not (LastToken in [tkAt,tkAtAt]);
  end;

begin
  if po_asmwhole in Options then
    begin
    FTokenRingCur := 0;
    FTokenRingStart := 0;
    FTokenRingEnd := 1;
    p := @FTokenRing[0];
    p^.Comments.Clear;
    repeat
      Scanner.ReadNonPascalTillEndToken(true);
      case Scanner.CurToken of
      tkLineEnding,tkWhitespace:
        AsmBlock.Tokens.Add(Scanner.CurTokenString);
      tkend:
        begin
        p^.Token := tkend;
        p^.AsString := Scanner.CurTokenString;
        break;
        end
      else
        begin
        // missing end
        p^.Token := tkEOF;
        p^.AsString := '';
        break;
        end;
      end;
    until false;
    FCurToken := p^.Token;
    FCurTokenString := p^.AsString;
    CheckToken(tkend);
    end
  else
    begin
    LastToken := tkEOF;
    NextToken;
    While not atEndOfAsm do
      begin
      AsmBlock.Tokens.Add(CurTokenText);
      LastToken := CurToken;
      NextToken;
      end;
    end;
  // Do not consume end. Current token will normally be end;
end;

// Next token is start of (compound) statement
// After parsing CurToken is on last token of statement
procedure TPasParser.ParseStatement(Parent: TPasImplBlock;
  out NewImplElement: TPasImplElement);
var
  CurBlock: TPasImplBlock;

  {$IFDEF VerbosePasParser}
  function i: string;
  var
    c: TPasElement;
  begin
    Result := 'ParseImplCompoundStatement ';
    c := CurBlock;
    while c<>nil do begin
      Result := Result+'  ';
      c := c.Parent;
    end;
  end;
  {$ENDIF}

  function CloseBlock: boolean; // true if parent reached
  var C: TPasImplBlockClass;
  begin
    C := TPasImplBlockClass(CurBlock.ClassType);
    if C=TPasImplExceptOn then
      Engine.FinishScope(stExceptOnStatement,CurBlock)
    else if C=TPasImplWithDo then
      Engine.FinishScope(stWithExpr,CurBlock);
    CurBlock := CurBlock.Parent as TPasImplBlock;
    Result := CurBlock=Parent;
  end;

  function CloseStatement(CloseIfs: boolean): boolean; // true if parent reached
  begin
    if CurBlock=Parent then Exit(true);
    while CurBlock.CloseOnSemicolon
    or (CloseIfs and (CurBlock is TPasImplIfElse)) do
      if CloseBlock then Exit(true);
    Result := false;
  end;

  procedure CreateBlock(NewBlock: TPasImplBlock);
  begin
    CurBlock.AddElement(NewBlock);
    CurBlock := NewBlock;
    if NewImplElement=nil then NewImplElement := CurBlock;
  end;

  procedure CheckSemicolon;
  var
    t: TToken;
  begin
    if (CurBlock.Elements.Count=0) then Exit;
    t := GetPrevToken;
    if t in [tkSemicolon,tkColon] then
      Exit;
    if (CurBlock.ClassType=TPasImplIfElse) and (t=tkelse) then
      Exit;
    {$IFDEF VerbosePasParser}
    writeln('TPasParser.ParseStatement.CheckSemicolon Prev=',GetPrevToken,' Cur=',CurToken,' ',CurBlock.ClassName,' ',CurBlock.Elements.Count,' ',TObject(CurBlock.Elements[0]).ClassName);
    {$ENDIF}
    ParseExcTokenError('Semicolon');
  end;

var
  CmdElem: TPasImplElement;

  procedure AddStatement(El: TPasImplElement);
  begin
    CurBlock.AddElement(El);
    CmdElem := El;
    UngetToken;
  end;

var
  SubBlock: TPasImplElement;
  Left, Right: TPasExpr;
  El: TPasImplElement;
  lt: TLoopType;
  SrcPos: TPasSourcePos;
  Name: string;
  TypeEl: TPasType;
  ImplRaise: TPasImplRaise;
  Expr: TPasExpr;

begin
  NewImplElement := nil;
  El := nil;
  Left := nil;
  try
    CurBlock := Parent;
    while True do
    begin
      NextToken;
      //WriteLn({$IFDEF VerbosePasParser}i,{$ENDIF}' Token=',CurTokenText);
      case CurToken of
      tkasm:
        begin
        CheckSemicolon;
        El := TPasImplElement(CreateElement(TPasImplAsmStatement,'',CurBlock,CurTokenPos));
        ParseAsmBlock(TPasImplAsmStatement(El));
        CurBlock.AddElement(El);
        El := nil;
        if NewImplElement=nil then NewImplElement := CurBlock;
        if CloseStatement(False) then
          break;
        end;
      tkbegin:
        begin
        CheckSemicolon;
        El := TPasImplElement(CreateElement(TPasImplBeginBlock,'',CurBlock,CurTokenPos));
        CreateBlock(TPasImplBeginBlock(El));
        El := nil;
        end;
      tkrepeat:
        begin
        CheckSemicolon;
        El := TPasImplRepeatUntil(CreateElement(TPasImplRepeatUntil,'',CurBlock,CurTokenPos));
        CreateBlock(TPasImplRepeatUntil(El));
        El := nil;
        end;
      tkIf:
        begin
          CheckSemicolon;
          SrcPos := CurTokenPos;
          NextToken;
          Left := DoParseExpression(CurBlock);
          UngetToken;
          El := TPasImplIfElse(CreateElement(TPasImplIfElse,'',CurBlock,SrcPos));
          TPasImplIfElse(El).ConditionExpr := Left;
          Left.Parent := El;
          Left := nil;
          //WriteLn(i,'IF Condition="',Condition,'" Token=',CurTokenText);
          CreateBlock(TPasImplIfElse(El));
          El := nil;
          ExpectToken(tkthen);
        end;
      tkelse:
        if (CurBlock is TPasImplIfElse) then
        begin
          if TPasImplIfElse(CurBlock).IfBranch=nil then
          begin
            // empty then statement  e.g. if condition then else
            El := TPasImplCommand(CreateElement(TPasImplCommand,'', CurBlock,CurTokenPos));
            CurBlock.AddElement(El);
            El := nil;
          end;
          if TPasImplIfElse(CurBlock).ElseBranch<>nil then
          begin
            // this and the following 3 may solve TPasImplIfElse.AddElement BUG
            // ifs without begin end
            // if .. then
            //  if .. then
            //   else
            // else
            CloseBlock;
            CloseStatement(false);
          end;
          // case ... else without semicolon in front.
        end else if (CurBlock is TPasImplCaseStatement) then
        begin
          UngetToken;
          CloseStatement(False);
          break;
        end else if (CurBlock is TPasImplWhileDo) then
        begin
          CloseBlock;
          UngetToken;
        end else if (CurBlock is TPasImplForLoop) then
        begin
          //if .. then for .. do smt else ..
          CloseBlock;
          UngetToken;
        end else if (CurBlock is TPasImplWithDo) then
        begin
          //if .. then with .. do smt else ..
          CloseBlock;
          UngetToken;
        end else if (CurBlock is TPasImplRaise) then
        begin
          //if .. then Raise Exception else ..
          CloseBlock;
          UngetToken;
        end else if (CurBlock is TPasImplAsmStatement) then
        begin
          //if .. then asm end else ..
          CloseBlock;
          UngetToken;
        end else if (CurBlock is TPasImplTryExcept) then
        begin
          CloseBlock;
          El := TPasImplTryExceptElse(CreateElement(TPasImplTryExceptElse,'',CurBlock,CurTokenPos));
          TPasImplTry(CurBlock).ElseBranch := TPasImplTryExceptElse(El);
          CurBlock := TPasImplTryExceptElse(El);
          El := nil;
        end else
          ParseExcSyntaxError;
      tkwhile:
        begin
          // while Condition do
          CheckSemicolon;
          SrcPos := CurTokenPos;
          NextToken;
          Left := DoParseExpression(CurBlock);
          UngetToken;
          //WriteLn(i,'WHILE Condition="',Condition,'" Token=',CurTokenText);
          El := TPasImplWhileDo(CreateElement(TPasImplWhileDo,'',CurBlock,SrcPos));
          TPasImplWhileDo(El).ConditionExpr := Left;
          Left := nil;
          CreateBlock(TPasImplWhileDo(El));
          El := nil;
          ExpectToken(tkdo);
        end;
      tkgoto:
        begin
        CheckSemicolon;
        NextToken;
        CurBlock.AddCommand('goto '+curtokenstring);
        // expecttoken(tkSemiColon);
        end;
      tkfor:
        begin
          // for VarName := StartValue to EndValue do
          // for VarName in Expression do
          CheckSemicolon;
          El := TPasImplForLoop(CreateElement(TPasImplForLoop,'',CurBlock,CurTokenPos));
          ExpectIdentifier;
          Expr := CreatePrimitiveExpr(El,pekIdent,CurTokenString);
          TPasImplForLoop(El).VariableName := Expr;
          repeat
            NextToken;
            case CurToken of
              tkAssign:
                begin
                lt := ltNormal;
                break;
                end;
              tkin:
                begin
                lt := ltIn;
                break;
                end;
              tkDot:
                begin
                SrcPos := CurTokenPos;
                ExpectIdentifier;
                AddToBinaryExprChain(Expr,
                  CreatePrimitiveExpr(El,pekIdent,CurTokenString), eopSubIdent,SrcPos);
                TPasImplForLoop(El).VariableName := Expr;
                end;
            else
              ParseExc(nParserExpectedAssignIn,SParserExpectedAssignIn);
            end;
          until false;
          NextToken;
          TPasImplForLoop(El).StartExpr := DoParseExpression(El);
          if (Lt=ltNormal) then
            begin
            if not (CurToken in [tkTo,tkDownTo]) then
              ParseExcTokenError(TokenInfos[tkTo]);
            if CurToken=tkdownto then
              Lt := ltDown;
            NextToken;
            TPasImplForLoop(El).EndExpr := DoParseExpression(El);
            end;
          TPasImplForLoop(El).LoopType := lt;
          if (CurToken<>tkDo) then
            ParseExcTokenError(TokenInfos[tkDo]);
          CreateBlock(TPasImplForLoop(El));
          El := nil;
          //WriteLn(i,'FOR "',VarName,'" := ',StartValue,' to ',EndValue,' Token=',CurTokenText);
        end;
      tkwith:
        begin
          // with Expr do
          // with Expr, Expr do
          CheckSemicolon;
          SrcPos := CurTokenPos;
          NextToken;
          El := TPasImplWithDo(CreateElement(TPasImplWithDo,'',CurBlock,SrcPos));
          Left := DoParseExpression(CurBlock);
          //writeln(i,'WITH Expr="',Expr,'" Token=',CurTokenText);
          TPasImplWithDo(El).AddExpression(Left);
          Left.Parent := El;
          Engine.BeginScope(stWithExpr,Left);
          Left := nil;
          CreateBlock(TPasImplWithDo(El));
          El := nil;
          repeat
            if CurToken=tkdo then break;
            if CurToken<>tkComma then
              ParseExcTokenError(TokenInfos[tkdo]);
            NextToken;
            Left := DoParseExpression(CurBlock);
            //writeln(i,'WITH ...,Expr="',Expr,'" Token=',CurTokenText);
            TPasImplWithDo(CurBlock).AddExpression(Left);
            Engine.BeginScope(stWithExpr,Left);
            Left := nil;
          until false;
        end;
      tkcase:
        begin
          CheckSemicolon;
          SrcPos := CurTokenPos;
          NextToken;
          Left := DoParseExpression(CurBlock);
          UngetToken;
          //writeln(i,'CASE OF Expr="',Expr,'" Token=',CurTokenText);
          ExpectToken(tkof);
          El := TPasImplCaseOf(CreateElement(TPasImplCaseOf,'',CurBlock,SrcPos));
          TPasImplCaseOf(El).CaseExpr := Left;
          Left.Parent := El;
          Left := nil;
          CreateBlock(TPasImplCaseOf(El));
          El := nil;
          repeat
            NextToken;
            //writeln(i,'CASE OF Token=',CurTokenText);
            case CurToken of
            tkend:
              begin
              if CurBlock.Elements.Count=0 then
                ParseExc(nParserExpectCase,SParserExpectCase);
              break; // end without else
              end;
            tkelse:
              begin
                // create case-else block
                El := TPasImplCaseElse(CreateElement(TPasImplCaseElse,'',CurBlock,CurTokenPos));
                TPasImplCaseOf(CurBlock).ElseBranch := TPasImplCaseElse(El);
                CreateBlock(TPasImplCaseElse(El));
                El := nil;
                break;
              end
            else
              // read case values
              if (curToken=tkIdentifier) and (LowerCase(CurtokenString)='otherwise') then
                begin
                // create case-else block
                El := TPasImplCaseElse(CreateElement(TPasImplCaseElse,'',CurBlock,CurTokenPos));
                TPasImplCaseOf(CurBlock).ElseBranch := TPasImplCaseElse(El);
                CreateBlock(TPasImplCaseElse(El));
                El := nil;
                break;
                end
              else
                repeat
                  SrcPos := CurTokenPos;
                  Left := DoParseExpression(CurBlock);
                  //writeln(i,'CASE value="',Expr,'" Token=',CurTokenText);
                  if CurBlock is TPasImplCaseStatement then
                    begin
                    TPasImplCaseStatement(CurBlock).Expressions.Add(Left);
                    Left := nil;
                    end
                  else
                    begin
                    El := TPasImplCaseStatement(CreateElement(TPasImplCaseStatement,'',CurBlock,SrcPos));
                    TPasImplCaseStatement(El).AddExpression(Left);
                    Left := nil;
                    CreateBlock(TPasImplCaseStatement(El));
                    El := nil;
                    end;
                  //writeln(i,'CASE after value Token=',CurTokenText);
                  if (CurToken=tkComma) then
                    NextToken
                  else if (CurToken<>tkColon) then
                    ParseExcTokenError(TokenInfos[tkComma]);
                until Curtoken=tkColon;
              // read statement
              ParseStatement(CurBlock,SubBlock);
              CloseBlock;
              if CurToken<>tkSemicolon then
              begin
                NextToken;
                if not (CurToken in [tkSemicolon,tkelse,tkend]) then
                  ParseExcTokenError(TokenInfos[tkSemicolon]);
                if CurToken<>tkSemicolon then
                  UngetToken;
              end;
            end;
          until false;
          if CurToken=tkend then
          begin
            if CloseBlock then break;
            if CloseStatement(false) then break;
          end;
        end;
      tktry:
        begin
        CheckSemicolon;
        El := TPasImplTry(CreateElement(TPasImplTry,'',CurBlock,CurTokenPos));
        CreateBlock(TPasImplTry(El));
        El := nil;
        end;
      tkfinally:
        begin
          if CloseStatement(true) then
          begin
            UngetToken;
            break;
          end;
          if CurBlock is TPasImplTry then
          begin
            El := TPasImplTryFinally(CreateElement(TPasImplTryFinally,'',CurBlock,CurTokenPos));
            TPasImplTry(CurBlock).FinallyExcept := TPasImplTryFinally(El);
            CurBlock := TPasImplTryFinally(El);
            El := nil;
          end else
            ParseExcSyntaxError;
        end;
      tkexcept:
        begin
          if CloseStatement(true) then
          begin
            UngetToken;
            break;
          end;
          if CurBlock is TPasImplTry then
          begin
            //writeln(i,'EXCEPT');
            El := TPasImplTryExcept(CreateElement(TPasImplTryExcept,'',CurBlock,CurTokenPos));
            TPasImplTry(CurBlock).FinallyExcept := TPasImplTryExcept(El);
            CurBlock := TPasImplTryExcept(El);
            El := nil;
          end else
            ParseExcSyntaxError;
        end;
      tkraise:
        begin
        CheckSemicolon;
        ImplRaise := TPasImplRaise(CreateElement(TPasImplRaise,'',CurBlock,CurTokenPos));
        CreateBlock(ImplRaise);
        NextToken;
        if Curtoken in [tkElse,tkEnd,tkSemicolon] then
          UnGetToken
        else
          begin
          ImplRaise.ExceptObject := DoParseExpression(ImplRaise);
          if (CurToken=tkIdentifier) and (Uppercase(CurtokenString)='AT') then
            begin
            NextToken;
            ImplRaise.ExceptAddr := DoParseExpression(ImplRaise);
            end;
          if Curtoken in [tkElse,tkEnd,tkSemicolon] then
            UngetToken
          end;
        end;
      tkend:
        begin
          if CloseStatement(true) then
          begin
            UngetToken;
            break;
          end;
          if CurBlock is TPasImplBeginBlock then
          begin
            if CloseBlock then break; // close end
            if CloseStatement(false) then break;
          end else if CurBlock is TPasImplCaseElse then
          begin
            if CloseBlock then break; // close else
            if CloseBlock then break; // close caseof
            if CloseStatement(false) then break;
          end else if CurBlock is TPasImplTryHandler then
          begin
            if CloseBlock then break; // close finally/except
            if CloseBlock then break; // close try
            if CloseStatement(false) then break;
          end else
            ParseExcSyntaxError;
        end;
      tkSemiColon:
        if CloseStatement(true) then break;
      tkFinalization:
        if CloseStatement(true) then
          begin
          UngetToken;
          break;
          end;
      tkuntil:
        begin
          if CloseStatement(true) then
          begin
            UngetToken;
            break;
          end;
          if CurBlock is TPasImplRepeatUntil then
          begin
            NextToken;
            Left := DoParseExpression(CurBlock);
            UngetToken;
            TPasImplRepeatUntil(CurBlock).ConditionExpr := Left;
            Left := nil;
            //WriteLn(i,'UNTIL Condition="',Condition,'" Token=',CurTokenString);
            if CloseBlock then break;
          end else
            ParseExcSyntaxError;
        end;
      tkEOF:
        CheckToken(tkend);
      tkAt,tkAtAt,tkBraceOpen,tkIdentifier,tkNumber,tkSquaredBraceOpen,tkMinus,tkPlus,tkinherited:
        begin
        // Do not check this here:
        //      if (CurToken=tkAt) and not (msDelphi in CurrentModeswitches) then
        //        ParseExc;
        CheckSemicolon;

        // On is usable as an identifier
        if lowerCase(CurTokenText)='on' then
          begin
            // in try except:
            // on E: Exception do
            // on Exception do
            if CurBlock is TPasImplTryExcept then
            begin
              SrcPos := CurTokenPos;
              ExpectIdentifier;
              El := TPasImplExceptOn(CreateElement(TPasImplExceptOn,'',CurBlock,SrcPos));
              SrcPos := CurSourcePos;
              Name := CurTokenString;
              NextToken;
              //writeln('ON t=',Name,' Token=',CurTokenText);
              if CurToken=tkColon then
                begin
                // the first expression was the variable name
                NextToken;
                TypeEl := ParseSimpleType(El,SrcPos,'');
                TPasImplExceptOn(El).TypeEl := TypeEl;
                TPasImplExceptOn(El).VarEl := TPasVariable(CreateElement(TPasVariable,
                                      Name,El,SrcPos));
                TPasImplExceptOn(El).VarEl.VarType := TypeEl;
                TypeEl.AddRef{$IFDEF CheckPasTreeRefCount}('TPasVariable.VarType'){$ENDIF};
                end
              else
                begin
                UngetToken;
                TPasImplExceptOn(El).TypeEl := ParseSimpleType(El,SrcPos,'');
                end;
              Engine.FinishScope(stExceptOnExpr,El);
              CreateBlock(TPasImplExceptOn(El));
              El := nil;
              ExpectToken(tkDo);
            end else
              ParseExcSyntaxError;
          end
        else
          begin
          SrcPos := CurTokenPos;
          Left := DoParseExpression(CurBlock);
          case CurToken of
            tkAssign,
            tkAssignPlus,
            tkAssignMinus,
            tkAssignMul,
            tkAssignDivision:
            begin
              // assign statement
              El := TPasImplAssign(CreateElement(TPasImplAssign,'',CurBlock,SrcPos));
              TPasImplAssign(El).left := Left;
              Left.Parent := El;
              Left := nil;
              TPasImplAssign(El).Kind := TokenToAssignKind(CurToken);
              NextToken;
              Right := DoParseExpression(CurBlock);
              TPasImplAssign(El).right := Right;
              Right.Parent := El;
              AddStatement(El);
              El := nil;
            end;
            tkColon:
            begin
              if not (Left is TPrimitiveExpr) then
                ParseExcTokenError(TokenInfos[tkSemicolon]);
              // label mark. todo: check mark identifier in the list of labels
              El := TPasImplLabelMark(CreateElement(TPasImplLabelMark,'', CurBlock,SrcPos));
              TPasImplLabelMark(El).LabelId := TPrimitiveExpr(Left).Value;
              ReleaseAndNil(TPasElement(Left){$IFDEF CheckPasTreeRefCount},'CreateElement'{$ENDIF});
              CurBlock.AddElement(El);
              CmdElem := TPasImplLabelMark(El);
              El := nil;
            end;
          else
            // simple statement (function call)
            El := TPasImplSimple(CreateElement(TPasImplSimple,'',CurBlock,SrcPos));
            TPasImplSimple(El).Expr := Left;
            Left := nil;
            AddStatement(El);
            El := nil;
          end;

          if not (CmdElem is TPasImplLabelMark) then
            if NewImplElement=nil then NewImplElement := CmdElem;
          end;
        end;
      else
        ParseExcSyntaxError;
      end;
    end;
  finally
    if El<>nil then El.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
    if Left<>nil then Left.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
  end;
end;

procedure TPasParser.ParseLabels(AParent: TPasElement);
var
  Labels: TPasLabels;
begin
  Labels := TPasLabels(CreateElement(TPasLabels, '', AParent));
  repeat
    Labels.Labels.Add(ExpectIdentifier);
    NextToken;
    if not (CurToken in [tkSemicolon, tkComma]) then
      ParseExcTokenError(TokenInfos[tkSemicolon]);
  until CurToken=tkSemicolon;
end;

// Starts after the "procedure" or "function" token
function TPasParser.GetProcedureClass(ProcType: TProcType): TPTreeElement;
begin
  Result := nil;
  case ProcType of
    ptFunction      : Result := TPasFunction;
    ptClassFunction : Result := TPasClassFunction;
    ptClassProcedure: Result := TPasClassProcedure;
    ptClassConstructor: Result := TPasClassConstructor;
    ptClassDestructor : Result := TPasClassDestructor;
    ptProcedure     : Result := TPasProcedure;
    ptConstructor   : Result := TPasConstructor;
    ptDestructor    : Result := TPasDestructor;
    ptOperator      : Result := TPasOperator;
    ptClassOperator : Result := TPasClassOperator;
    ptAnonymousProcedure: Result := TPasAnonymousProcedure;
    ptAnonymousFunction: Result := TPasAnonymousFunction;
  else
    ParseExc(nParserUnknownProcedureType,SParserUnknownProcedureType,[Ord(ProcType)]);
  end;
end;

function TPasParser.ParseProcedureOrFunctionDecl(Parent: TPasElement;
  ProcType: TProcType; AVisibility: TPasMemberVisibility): TPasProcedure;

  function ExpectProcName: string;

  var
    L: TFPList;
    I: Integer;

  begin
    Result := ExpectIdentifier;
    //writeln('ExpectProcName ',Parent.Classname);
    if Parent is TImplementationSection then
    begin
      NextToken;
      repeat
        if CurToken=tkDot then
          Result := Result+'.'+ExpectIdentifier
        else if CurToken=tkLessThan then
          begin // <> can be ignored, we read the list but discard its content
          UnGetToken;
          L := TFPList.Create;
          Try
            ReadGenericArguments(L,Parent);
          finally
            for I := 0 to L.Count-1 do
              TPasElement(L[i]).Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
            L.Free;
          end;
          end
        else
          break;
        NextToken;
      until false;
      UngetToken;
    end;
  end;

var
  name: string;
  pc: TPTreeElement;
  ot: TOperatorType;
  isTokenBased, ok: Boolean;
begin
  ot := otUnknown;
  isTokenBased := False;
  case ProcType of
    ptOperator, ptClassOperator: begin
      NextToken;
      isTokenBased := Curtoken<>tkIdentifier;
      if isTokenBased then
        ot := TPasOperator.TokenToOperatorType(CurTokenText)
      else
        ot := TPasOperator.NameToOperatorType(CurTokenString);
      if (ot=otUnknown) then
        ParseExc(nErrUnknownOperatorType,SErrUnknownOperatorType,[CurTokenString]);
      name := OperatorNames[ot];
    end;
    ptAnonymousProcedure, ptAnonymousFunction:
      name := '';
    else
      name := ExpectProcName;
  end;
  pc := GetProcedureClass(ProcType);
  if name <> '' then
    Parent := CheckIfOverLoaded(Parent,name);
  Result := TPasProcedure(CreateElement(pc,name,Parent,AVisibility));
  ok := false;
  try
    case ProcType of
      ptFunction, ptClassFunction, ptOperator, ptClassOperator, ptAnonymousFunction: begin
        Result.ProcType := CreateFunctionType('', 'Result', Result, False, CurTokenPos);
        if (ProcType in [ptOperator, ptClassOperator]) then begin
          TPasOperator(Result).TokenBased := isTokenBased;
          TPasOperator(Result).OperatorType := ot;
          TPasOperator(Result).CorrectName;
        end;
      end;
      else
        Result.ProcType := TPasProcedureType(CreateElement(TPasProcedureType, '', Result));
    end;
    ParseProcedureOrFunction(Result, Result.ProcType, ProcType, False);
    Result.Hints := Result.ProcType.Hints;
    Result.HintMessage := Result.ProcType.HintMessage;
    // + is detected as 'positive', but is in fact Add if there are 2 arguments.
    if (ProcType in [ptOperator, ptClassOperator]) then
      with TPasOperator(Result) do begin
        if (OperatorType in [otPositive, otNegative]) then begin
          if (ProcType.Args.Count>1) then begin
            case OperatorType of
              otPositive: OperatorType := otPlus;
              otNegative: OperatorType := otMinus;
            end;
            name := OperatorNames[OperatorType];
            TPasOperator(Result).CorrectName;
          end;
        end;
      end;
    ok := true;
  finally
    if not ok then
      Result.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
  end;
end;

// Current token is the first token after tkOf
procedure TPasParser.ParseRecordVariantParts(ARec: TPasRecordType;
  AEndToken: TToken);

var
  M: TPasRecordType;
  V: TPasVariant;
  Done: Boolean;

begin
  Repeat
    V := TPasVariant(CreateElement(TPasVariant, '', ARec));
    ARec.Variants.Add(V);
    Repeat
      NextToken;
      V.Values.Add(DoParseExpression(ARec));
      if not (CurToken in [tkComma,tkColon]) then
        ParseExc(nParserExpectedCommaColon,SParserExpectedCommaColon);
    Until (curToken=tkColon);
    ExpectToken(tkBraceOpen);
    NextToken;
    M := TPasRecordType(CreateElement(TPasRecordType,'',V));
    V.Members := M;
    ParseRecordFieldList(M,tkBraceClose,False);
    // Current token is closing ), so we eat that
    NextToken;
    // If there is a semicolon, we eat that too.
    if CurToken=tkSemicolon then
      NextToken;
    // ParseExpression starts with a nexttoken.
    // So we need to determine the next token, and if it is an ending token, unget.
    Done := CurToken=AEndToken;
    if not Done then
      Ungettoken;
  Until Done;
end;

{$ifdef VerbosePasParser}
procedure TPasParser.DumpCurToken(const Msg: string; IndentAction: TIndentAction
  );
begin
  {AllowWriteln}
  if IndentAction=iaUndent then
    FDumpIndent := copy(FDumpIndent,1,Length(FDumpIndent)-2);
  Writeln(FDumpIndent,Msg,': ',TokenInfos[CurToken],' "',CurTokenString,'", Position: ',Scanner.CurFilename,'(',Scanner.CurRow,',',SCanner.CurColumn,'): ',Scanner.CurLine);
  if IndentAction=iaIndent then
    FDumpIndent := FDumpIndent+'  ';
  {$ifdef pas2js}
  // ToDo
  {$else}
  Flush(output);
  {$endif}
  {AllowWriteln-}
end;
{$endif}

function TPasParser.GetCurrentModeSwitches: TModeSwitches;
begin
  if Assigned(FScanner) then
    Result := FScanner.CurrentModeSwitches
  else
    Result := [msNone];
end;

procedure TPasParser.SetCurrentModeSwitches(AValue: TModeSwitches);
begin
  if Assigned(FScanner) then
    FScanner.CurrentModeSwitches := AValue;
end;

// Starts on first token after Record or (. Ends on AEndToken
procedure TPasParser.ParseRecordFieldList(ARec: TPasRecordType;
  AEndToken: TToken; AllowMethods: Boolean);

var
  VariantName: string;
  v: TPasMemberVisibility;
  Proc: TPasProcedure;
  ProcType: TProcType;
  Prop: TPasProperty;
  isClass: Boolean;
  NamePos: TPasSourcePos;
  OldCount, i: Integer;
begin
  if AllowMethods then
    v := visPublic
  else
    v := visDefault;
  isClass := False;
  while CurToken<>AEndToken do
    begin
    SaveComments;
    case CurToken of
      tkType:
        begin
        if not AllowMethods then
          ParseExc(nErrRecordTypesNotAllowed,SErrRecordTypesNotAllowed);
        ExpectToken(tkIdentifier);
        ParseMembersLocalTypes(ARec,v);
        end;
      tkConst:
        begin
        if not AllowMethods then
          ParseExc(nErrRecordConstantsNotAllowed,SErrRecordConstantsNotAllowed);
        ExpectToken(tkIdentifier);
        ParseMembersLocalConsts(ARec,v);
        end;
      tkVar:
        begin
        if not AllowMethods then
          ParseExc(nErrRecordVariablesNotAllowed,SErrRecordVariablesNotAllowed);
        ExpectToken(tkIdentifier);
        OldCount := ARec.Members.Count;
        ParseInlineVarDecl(ARec, ARec.Members, v, AEndToken=tkBraceClose);
        for i := OldCount to ARec.Members.Count-1 do
          begin
          if isClass then
            with TPasVariable(ARec.Members[i]) do
              VarModifiers := VarModifiers + [vmClass];
          Engine.FinishScope(stDeclaration,TPasVariable(ARec.Members[i]));
          end;
        end;
      tkClass:
        begin
        if not AllowMethods then
          begin
          NextToken;
          case CurToken of
          tkConst: ParseExc(nErrRecordConstantsNotAllowed,SErrRecordConstantsNotAllowed);
          tkvar: ParseExc(nErrRecordVariablesNotAllowed,SErrRecordVariablesNotAllowed);
          else
            ParseExc(nErrRecordMethodsNotAllowed,SErrRecordMethodsNotAllowed);
          end;
          end;
        if isClass then
          ParseExc(nParserTypeSyntaxError,SParserTypeSyntaxError);
        isClass := True;
        Scanner.SetTokenOption(toOperatorToken);
        end;
      tkProperty:
        begin
        if not AllowMethods then
          ParseExc(nErrRecordPropertiesNotAllowed,SErrRecordPropertiesNotAllowed);
        ExpectToken(tkIdentifier);
        Prop := ParseProperty(ARec,CurtokenString,v,isClass);
        Arec.Members.Add(Prop);
        Engine.FinishScope(stDeclaration,Prop);
        end;
      tkOperator,
      tkProcedure,
      tkConstructor,
      tkFunction :
        begin
        if not AllowMethods then
          ParseExc(nErrRecordMethodsNotAllowed,SErrRecordMethodsNotAllowed);
        ProcType := GetProcTypeFromToken(CurToken,isClass);
        Proc := ParseProcedureOrFunctionDecl(ARec,ProcType,v);
        if Proc.Parent is TPasOverloadedProc then
          TPasOverloadedProc(Proc.Parent).Overloads.Add(Proc)
        else
          ARec.Members.Add(Proc);
        end;
      tkDestructor:
        ParseExc(nParserNoConstructorAllowed,SParserNoConstructorAllowed);
      tkGeneric, // Counts as field name
      tkIdentifier :
        begin
        if AllowMethods and CheckVisibility(CurTokenString,v) then
          begin
          if not (v in [visPrivate,visPublic,visStrictPrivate]) then
            ParseExc(nParserInvalidRecordVisibility,SParserInvalidRecordVisibility);
          NextToken;
          Continue;
          end;
        OldCount := ARec.Members.Count;
        ParseInlineVarDecl(ARec, ARec.Members, v, AEndToken=tkBraceClose);
        for i := OldCount to ARec.Members.Count-1 do
          Engine.FinishScope(stDeclaration,TPasVariable(ARec.Members[i]));
        end;
      tkCase :
        begin
        ARec.Variants := TFPList.Create;
        NextToken;
        VariantName := CurTokenString;
        NamePos := CurSourcePos;
        NextToken;
        if CurToken=tkColon then
          begin
          ARec.VariantEl := TPasVariable(CreateElement(TPasVariable,VariantName,ARec,NamePos));
          TPasVariable(ARec.VariantEl).VarType := ParseType(ARec,CurSourcePos);
          end
        else
          begin
          UnGetToken;
          UnGetToken;
          ARec.VariantEl := ParseType(ARec,CurSourcePos);
          end;
        ExpectToken(tkOf);
        ParseRecordVariantParts(ARec,AEndToken);
        end;
    else
      ParseExc(nParserTypeSyntaxError,SParserTypeSyntaxError);
    end;
    if CurToken<>tkClass then
      begin
      isClass := False;
      Scanner.UnSetTokenOption(toOperatorToken);
      end;
    if CurToken<>AEndToken then
      NextToken;
    end;
end;

// Starts after the "record" token
function TPasParser.ParseRecordDecl(Parent: TPasElement;
  const NamePos: TPasSourcePos; const TypeName: string;
  const Packmode: TPackMode): TPasRecordType;

var
  ok: Boolean;
begin
  Result := TPasRecordType(CreateElement(TPasRecordType, TypeName, Parent, NamePos));
  ok := false;
  try
    Result.PackMode := PackMode;
    NextToken;
    ParseRecordFieldList(Result,tkEnd,
      (msAdvancedRecords in Scanner.CurrentModeSwitches) and not (Parent is TProcedureBody));
    Engine.FinishScope(stTypeDef,Result);
    ok := true;
  finally
    if not ok then
      begin
      Result.Parent := nil; // clear references from members to Result
      Result.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
      end;
  end;
end;

function IsVisibility(S: string; var AVisibility: TPasMemberVisibility): Boolean;
const
  VNames: array[TPasMemberVisibility] of string =
    ('', 'private', 'protected', 'public', 'published', 'automated', '', '');
var
  v: TPasMemberVisibility;
begin
  S := lowerCase(S);
  for v := Low(TPasMemberVisibility) to High(TPasMemberVisibility) do begin
    Result := (VNames[v]<>'') and (S=VNames[v]);
    if Result then begin
      AVisibility := v;
      Exit;
    end;
  end;
end;

function TPasParser.CheckVisibility(S: string;
  var AVisibility: TPasMemberVisibility): Boolean;

var
  B: Boolean;

begin
  s := LowerCase(CurTokenString);
  B := (S='strict');
  if B then
    begin
    NextToken;
    s := LowerCase(CurTokenString);
    end;
  Result := isVisibility(S,AVisibility);
  if Result then
    begin
    if (AVisibility=visPublished) and (msOmitRTTI in Scanner.CurrentModeSwitches) then
      AVisibility := visPublic;
    if B then
      case AVisibility of
        visPrivate  : AVisibility := visStrictPrivate;
        visProtected: AVisibility := visStrictProtected;
      else
        ParseExc(nParserStrangeVisibility,SParserStrangeVisibility,[S]);
      end
    end
  else if B then
    ParseExc(nParserExpectVisibility,SParserExpectVisibility);
end;

procedure TPasParser.ProcessMethod(AType: TPasClassType; IsClass: Boolean; AVisibility: TPasMemberVisibility);

var
  Proc: TPasProcedure;
  ProcType: TProcType;
begin
  ProcType := GetProcTypeFromToken(CurToken,isClass);
  Proc := ParseProcedureOrFunctionDecl(AType,ProcType,AVisibility);
  if Proc.Parent is TPasOverloadedProc then
    TPasOverloadedProc(Proc.Parent).Overloads.Add(Proc)
  else
    AType.Members.Add(Proc);
end;

procedure TPasParser.ParseClassFields(AType: TPasClassType;
  const AVisibility: TPasMemberVisibility; IsClassField: Boolean);
var
  varList: TFPList;
  element: TPasElement;
  i: Integer;
  isStatic: Boolean;
  varEl: TPasVariable;
begin
  isStatic := False;
  varList := TFPList.Create;
  try
    ParseInlineVarDecl(AType, varList, AVisibility, False);
    if CurToken=tkSemicolon then
      begin
      NextToken;
      isStatic := CurTokenIsIdentifier('static');
      if isStatic then
        ExpectToken(tkSemicolon)
      else
        UngetToken;
      end;
    for i := 0 to varList.Count - 1 do
      begin
      element := TPasElement(varList[i]);
      element.Visibility := AVisibility;
      AType.Members.Add(element);
      if (element is TPasVariable) then
        begin
        varEl := TPasVariable(element);
        if IsClassField then
          Include(varEl.VarModifiers,vmClass);
        if isStatic then
          Include(varEl.VarModifiers,vmStatic);
        Engine.FinishScope(stDeclaration,varEl);
        end;
      end;
  finally
    varList.Free;
  end;
end;

procedure TPasParser.ParseMembersLocalTypes(AType: TPasMembersType;
  AVisibility: TPasMemberVisibility);

var
  T: TPasType;
  Done: Boolean;
begin
  // Writeln('Parsing local types');
  Repeat
    T := ParseTypeDecl(AType);
    T.Visibility := AVisibility;
    AType.Members.Add(t);
    // Writeln(CurtokenString,' ',TokenInfos[Curtoken]);
    NextToken;
    Done := (Curtoken<>tkIdentifier) or CheckVisibility(CurTokenString,AVisibility);
    if Done then
      UngetToken;
  Until Done;
  Engine.FinishScope(stTypeSection,AType);
end;

procedure TPasParser.ParseMembersLocalConsts(AType: TPasMembersType;
  AVisibility: TPasMemberVisibility);

var
  C: TPasConst;
  Done: Boolean;
begin
  // Writeln('Parsing local consts');
  Repeat
    C := ParseConstDecl(AType);
    C.Visibility := AVisibility;
    AType.Members.Add(C);
    Engine.FinishScope(stDeclaration,C);
    //Writeln('TPasParser.ParseMembersLocalConsts ',CurtokenString,' ',TokenInfos[CurToken]);
    NextToken;
    if CurToken<>tkSemicolon then
      Exit;
    NextToken;
    Done := (CurToken<>tkIdentifier) or CheckVisibility(CurTokenString,AVisibility);
    if Done then
      UngetToken;
  Until Done;
end;

procedure TPasParser.ParseClassMembers(AType: TPasClassType);
type
  TSectionType = (stNone, stConst, stType, stVar, stClassVar);
var
  CurVisibility: TPasMemberVisibility;
  CurSection: TSectionType;
  haveClass,
    IsMethodResolution: Boolean; // true means last token was class keyword
  LastToken: TToken;
  PropEl: TPasProperty;
  MethodRes: TPasMethodResolution;
begin
  CurSection := stNone;
  haveClass := false;
  if Assigned(FEngine) then
    CurVisibility := FEngine.GetDefaultClassVisibility(AType)
  else
    CurVisibility := visPublic;
  LastToken := CurToken;
  while (CurToken<>tkEnd) do
    begin
    //writeln('TPasParser.ParseClassMembers LastToken=',LastToken,' CurToken=',CurToken,' haveClass=',haveClass,' CurSection=',CurSection);
    case CurToken of
      tkType:
        begin
        case AType.ObjKind of
        okClass,okObject,okGeneric,
        okClassHelper,okRecordHelper,okTypeHelper: ;
        else
          ParseExc(nParserXNotAllowedInY,SParserXNotAllowedInY,['TYPE',ObjKindNames[AType.ObjKind]]);
        end;
        CurSection := stType;
        end;
      tkConst:
        begin
        if haveClass then
          ParseExc(nParserExpectToken2Error,SParserExpectToken2Error,
            ['procedure','var']);
        case AType.ObjKind of
        okClass,okObject,okGeneric,
        okClassHelper,okRecordHelper,okTypeHelper: ;
        else
          ParseExc(nParserXNotAllowedInY,SParserXNotAllowedInY,['CONST',ObjKindNames[AType.ObjKind]]);
        end;
        CurSection := stConst;
        end;
      tkVar:
        if not (CurSection in [stVar,stClassVar]) then
          begin
          if (AType.ObjKind in [okClass,okObject,okGeneric])
          or (haveClass and (AType.ObjKind in [okClassHelper,okRecordHelper,okTypeHelper])) then
            // ok
          else
            ParseExc(nParserXNotAllowedInY,SParserXNotAllowedInY,['VAR',ObjKindNames[AType.ObjKind]]);
          if LastToken=tkClass then
            CurSection := stClassVar
          else
            CurSection := stVar;
          end;
      tkIdentifier:
        if CheckVisibility(CurtokenString,CurVisibility) then
          CurSection := stNone
        else
          begin
          if not haveClass then
            SaveComments;
          case CurSection of
          stType:
            ParseMembersLocalTypes(AType,CurVisibility);
          stConst :
            ParseMembersLocalConsts(AType,CurVisibility);
          stNone,
          stVar:
            begin
            if not (AType.ObjKind in [okObject,okClass,okGeneric]) then
              ParseExc(nParserNoFieldsAllowed,SParserNoFieldsAllowedInX,[ObjKindNames[AType.ObjKind]]);
            ParseClassFields(AType,CurVisibility,CurSection=stClassVar);
            HaveClass := False;
            end;
          stClassVar:
            begin
            if not (AType.ObjKind in [okObject,okClass,okGeneric,okClassHelper,okRecordHelper,okTypeHelper]) then
              ParseExc(nParserNoFieldsAllowed,SParserNoFieldsAllowedInX,[ObjKindNames[AType.ObjKind]]);
            ParseClassFields(AType,CurVisibility,CurSection=stClassVar);
            HaveClass := False;
            end;
          else
            Raise Exception.Create('Internal error 201704251415');
          end;
          end;
      tkConstructor,tkDestructor:
        begin
        curSection := stNone;
        if not haveClass then
          SaveComments;
        if (AType.ObjKind in [okObject,okClass,okGeneric])
            or ((CurToken=tkconstructor)
              and (AType.ObjKind in [okClassHelper,okTypeHelper,okRecordHelper])) then
          // ok
        else
          ParseExc(nParserNoConstructorAllowed,SParserNoConstructorAllowed);
        ProcessMethod(AType,HaveClass,CurVisibility);
        haveClass := False;
        end;
      tkProcedure,tkFunction:
        begin
        curSection := stNone;
        IsMethodResolution := false;
        if not haveClass then
          begin
          SaveComments;
          if AType.ObjKind=okClass then
            begin
            NextToken;
            if CurToken=tkIdentifier then
              begin
              NextToken;
              IsMethodResolution := CurToken=tkDot;
              UngetToken;
              end;
            UngetToken;
            end;
          end;
        if IsMethodResolution then
          begin
          MethodRes := ParseMethodResolution(AType);
          AType.Members.Add(MethodRes);
          Engine.FinishScope(stDeclaration,MethodRes);
          end
        else
          ProcessMethod(AType,HaveClass,CurVisibility);
        haveClass := False;
        end;
      tkclass:
        begin
        case AType.ObjKind of
        okClass,okObject,okGeneric,
        okClassHelper,okRecordHelper,okTypeHelper: ;
        else
          ParseExc(nParserXNotAllowedInY,SParserXNotAllowedInY,['CLASS',ObjKindNames[AType.ObjKind]]);
        end;
        SaveComments;
        HaveClass := True;
        curSection := stNone;
        end;
      tkProperty:
        begin
        curSection := stNone;
        if not haveClass then
          SaveComments;
        ExpectIdentifier;
        PropEl := ParseProperty(AType,CurtokenString,CurVisibility,HaveClass);
        AType.Members.Add(PropEl);
        Engine.FinishScope(stDeclaration,PropEl);
        HaveClass := False;
        end;
      tkSquaredBraceOpen:
        if [msPrefixedAttributes,msIgnoreAttributes]*CurrentModeswitches<>[] then
          ParseAttribute(AType)
        else
          CheckToken(tkIdentifier);
    else
      CheckToken(tkIdentifier);
    end;
    LastToken := CurToken;
    NextToken;
    end;
end;

procedure TPasParser.DoParseClassType(AType: TPasClassType);

var
  s: string;
  Expr: TPasExpr;

begin
  if (CurToken=tkIdentifier) and (AType.ObjKind in [okClass,okGeneric]) then
    begin
    s := LowerCase(CurTokenString);
    if (s = 'sealed') or (s = 'abstract') then
      begin
      AType.Modifiers.Add(s);
      NextToken;
      end;
    end;
  // Parse ancestor list
  AType.IsForward := (CurToken=tkSemiColon);
  if (CurToken=tkBraceOpen) then
    begin
    // read ancestor and interfaces
    if (AType.ObjKind=okRecordHelper)
        and ([msTypeHelpers,msDelphi]*Scanner.CurrentModeSwitches=[msDelphi]) then
      // Delphi does not support ancestors in record helpers
      CheckToken(tkend);
    NextToken;
    AType.AncestorType := ParseTypeReference(AType,false,Expr);
    if AType.ObjKind in [okClass,okGeneric] then
      while CurToken=tkComma do
        begin
        NextToken;
        AType.Interfaces.Add(ParseTypeReference(AType,false,Expr));
        end;
    CheckToken(tkBraceClose);
    NextToken;
    AType.IsShortDefinition := (CurToken=tkSemicolon);
    end;
  if (AType.ObjKind in [okClassHelper,okRecordHelper,okTypeHelper]) then
    begin
    CheckToken(tkfor);
    NextToken;
    AType.HelperForType := ParseTypeReference(AType,false,Expr);
    end;
  Engine.FinishScope(stAncestors,AType);
  if AType.IsShortDefinition or AType.IsForward then
    UngetToken
  else
    begin
    if (AType.ObjKind in [okInterface,okDispInterface]) and (CurToken = tkSquaredBraceOpen) then
      begin
      NextToken;
      AType.GUIDExpr := DoParseExpression(AType);
      if (CurToken<>tkSquaredBraceClose) then
        ParseExcTokenError(TokenInfos[tkSquaredBraceClose]);
      NextToken;
      end;
    ParseClassMembers(AType);
    end;
end;

function TPasParser.ParseClassDecl(Parent: TPasElement;
  const NamePos: TPasSourcePos; const AClassName: string;
  AObjKind: TPasObjKind; PackMode: TPackMode; GenericArgs: TFPList): TPasType;
var
  ok: Boolean;
  AExternalNameSpace, AExternalName: string;
  PCT:TPasClassType;
begin
  NextToken;
  if (AObjKind = okClass) and (CurToken = tkOf) then
    begin
    Result := TPasClassOfType(CreateElement(TPasClassOfType, AClassName,
      Parent, NamePos));
    ok := false;
    try
      ExpectIdentifier;
      UngetToken;                // Only names are allowed as following type
      TPasClassOfType(Result).DestType := ParseType(Result,CurSourcePos);
      Engine.FinishScope(stTypeDef,Result);
      ok := true;
    finally
      if not ok then
        Result.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
    end;
    Exit;
    end;
  if ((AObjKind in [okClass,okInterface]) and (msExternalClass in CurrentModeswitches)
      and CurTokenIsIdentifier('external')) then
    begin
    NextToken;
    if CurToken<>tkString then
      UnGetToken
    else
      AExternalNameSpace := CurTokenString;
    ExpectIdentifier;
    if not CurTokenIsIdentifier('Name')  then
      ParseExc(nParserExpectedExternalClassName,SParserExpectedExternalClassName);
    NextToken;
    if not (CurToken in [tkChar,tkString]) then
      CheckToken(tkString);
    AExternalName := CurTokenString;
    NextToken;
    end
  else
    begin
    AExternalNameSpace := '';
    AExternalName := '';
    end;
  if AObjKind in [okClassHelper,okRecordHelper,okTypeHelper] then
    begin
    if not CurTokenIsIdentifier('Helper') then
      ParseExcSyntaxError;
    NextToken;
    end;
  PCT := TPasClassType(CreateElement(TPasClassType, AClassName,
    Parent, NamePos));
  Result := PCT;
  ok := false;
  try
    PCT.HelperForType := nil;
    PCT.IsExternal := (AExternalName<>'');
    if AExternalName<>'' then
      PCT.ExternalName := {$ifdef pas2js}DeQuoteString{$else}AnsiDequotedStr{$endif}(AExternalName,'''');
    if AExternalNameSpace<>'' then
    PCT.ExternalNameSpace := {$ifdef pas2js}DeQuoteString{$else}AnsiDequotedStr{$endif}(AExternalNameSpace,'''');
    PCT.ObjKind := AObjKind;
    PCT.PackMode := PackMode;
    if AObjKind=okInterface then
      begin
      if SameText(Scanner.CurrentValueSwitch[vsInterfaces],'CORBA') then
        PCT.InterfaceType := citCorba;
      end;
    if Assigned(GenericArgs) then
      PCT.SetGenericTemplates(GenericArgs);
    DoParseClassType(PCT);
    Engine.FinishScope(stTypeDef,Result);
    ok := true;
  finally
    if not ok then
      begin
      PCT.Parent := nil; // clear references from members to PCT
      Result.Release{$IFDEF CheckPasTreeRefCount}('CreateElement'){$ENDIF};
      end;
  end;
end;

function TPasParser.CreateElement(AClass: TPTreeElement; const AName: string;
  AParent: TPasElement): TPasElement;
begin
  Result := Engine.CreateElement(AClass, AName, AParent, visDefault, CurSourcePos);
end;

function TPasParser.CreateElement(AClass: TPTreeElement; const AName: string;
  AParent: TPasElement; const ASrcPos: TPasSourcePos): TPasElement;
begin
  Result := Engine.CreateElement(AClass, AName, AParent, visDefault, ASrcPos);
end;

function TPasParser.CreateElement(AClass: TPTreeElement; const AName: string;
  AParent: TPasElement; AVisibility: TPasMemberVisibility): TPasElement;
begin
  Result := Engine.CreateElement(AClass, AName, AParent, AVisibility,
    CurSourcePos);
end;

function TPasParser.CreateElement(AClass: TPTreeElement; const AName: string;
  AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASrcPos: TPasSourcePos): TPasElement;
begin
  if (ASrcPos.Row=0) and (ASrcPos.FileName='') then
    Result := Engine.CreateElement(AClass, AName, AParent, AVisibility, CurSourcePos)
  else
    Result := Engine.CreateElement(AClass, AName, AParent, AVisibility, ASrcPos);
end;

function TPasParser.CreatePrimitiveExpr(AParent: TPasElement;
  AKind: TPasExprKind; const AValue: string): TPrimitiveExpr;
begin
  Result := TPrimitiveExpr(CreateElement(TPrimitiveExpr,'',AParent,CurTokenPos));
  Result.Kind := AKind;
  Result.Value := AValue;
end;

function TPasParser.CreateBoolConstExpr(AParent: TPasElement;
  AKind: TPasExprKind; const ABoolValue: Boolean): TBoolConstExpr;
begin
  Result := TBoolConstExpr(CreateElement(TBoolConstExpr,'',AParent,CurTokenPos));
  Result.Kind := AKind;
  Result.Value := ABoolValue;
end;

function TPasParser.CreateBinaryExpr(AParent: TPasElement; xleft,
  xright: TPasExpr; AOpCode: TExprOpCode): TBinaryExpr;
begin
  Result := CreateBinaryExpr(AParent,xleft,xright,AOpCode,CurSourcePos);
end;

function TPasParser.CreateBinaryExpr(AParent: TPasElement; xleft,
  xright: TPasExpr; AOpCode: TExprOpCode; const ASrcPos: TPasSourcePos
  ): TBinaryExpr;
begin
  Result := TBinaryExpr(CreateElement(TBinaryExpr,'',AParent,ASrcPos));
  Result.OpCode := AOpCode;
  Result.Kind := pekBinary;
  if xleft<>nil then
    begin
    Result.left := xleft;
    xleft.Parent := Result;
    end;
  if xright<>nil then
    begin
    Result.right := xright;
    xright.Parent := Result;
    end;
end;

procedure TPasParser.AddToBinaryExprChain(var ChainFirst: TPasExpr;
  Element: TPasExpr; AOpCode: TExprOpCode; const ASrcPos: TPasSourcePos);
begin
  if Element=nil then
    Exit
  else if ChainFirst=nil then
    // empty chain => simply add element, no need to create TBinaryExpr
    ChainFirst := Element
  else
    // create new binary, old becomes left, Element right
    ChainFirst := CreateBinaryExpr(ChainFirst.Parent,ChainFirst,Element,AOpCode,ASrcPos);
end;

{$IFDEF VerbosePasParser}
{AllowWriteln}
procedure TPasParser.WriteBinaryExprChain(Prefix: string; First, Last: TPasExpr
  );
var
  i: Integer;
begin
  if First=nil then
    begin
    write(Prefix,'First=nil');
    if Last=nil then
      writeln('=Last')
    else
      begin
      writeln(', ERROR Last=',Last.ClassName);
      ParseExcSyntaxError;
      end;
    end
  else if Last=nil then
    begin
    writeln(Prefix,'ERROR Last=nil First=',First.ClassName);
    ParseExcSyntaxError;
    end
  else if First is TBinaryExpr then
    begin
    i := 0;
    while First is TBinaryExpr do
      begin
      writeln(Prefix,Space(i*2),'bin.left=',TBinaryExpr(First).left.ClassName);
      if First=Last then break;
      First := TBinaryExpr(First).right;
      inc(i);
      end;
    if First<>Last then
      begin
      writeln(Prefix,Space(i*2),'ERROR Last is not last in chain');
      ParseExcSyntaxError;
      end;
    if not (Last is TBinaryExpr) then
      begin
      writeln(Prefix,Space(i*2),'ERROR Last is not TBinaryExpr: ',Last.ClassName);
      ParseExcSyntaxError;
      end;
    if TBinaryExpr(Last).right=nil then
      begin
      writeln(Prefix,Space(i*2),'ERROR Last.right=nil');
      ParseExcSyntaxError;
      end;
    writeln(Prefix,Space(i*2),'last.right=',TBinaryExpr(Last).right.ClassName);
    end
  else if First=Last then
    writeln(Prefix,'First=Last=',First.ClassName)
  else
    begin
    write(Prefix,'ERROR First=',First.ClassName);
    if Last<>nil then
      writeln(' Last=',Last.ClassName)
    else
      writeln(' Last=nil');
    end;
end;
{AllowWriteln-}
{$ENDIF}

function TPasParser.CreateUnaryExpr(AParent: TPasElement; AOperand: TPasExpr;
  AOpCode: TExprOpCode): TUnaryExpr;
begin
  Result := CreateUnaryExpr(AParent,AOperand,AOpCode,CurTokenPos);
end;

function TPasParser.CreateUnaryExpr(AParent: TPasElement; AOperand: TPasExpr;
  AOpCode: TExprOpCode; const ASrcPos: TPasSourcePos): TUnaryExpr;
begin
  Result := TUnaryExpr(CreateElement(TUnaryExpr,'',AParent,ASrcPos));
  Result.Kind := pekUnary;
  Result.Operand := AOperand;
  Result.Operand.Parent := Result;
  Result.OpCode := AOpCode;
end;

function TPasParser.CreateArrayValues(AParent: TPasElement): TArrayValues;
begin
  Result := TArrayValues(CreateElement(TArrayValues,'',AParent));
  Result.Kind := pekListOfExp;
end;

function TPasParser.CreateFunctionType(const AName, AResultName: string;
  AParent: TPasElement; UseParentAsResultParent: Boolean;
  const NamePos: TPasSourcePos): TPasFunctionType;
begin
  Result := Engine.CreateFunctionType(AName,AResultName,
                                    AParent,UseParentAsResultParent,
                                    NamePos);
end;

function TPasParser.CreateInheritedExpr(AParent: TPasElement): TInheritedExpr;
begin
  Result := TInheritedExpr(CreateElement(TInheritedExpr,'',AParent,CurTokenPos));
  Result.Kind := pekInherited;
end;

function TPasParser.CreateSelfExpr(AParent: TPasElement): TSelfExpr;
begin
  Result := TSelfExpr(CreateElement(TSelfExpr,'Self',AParent,CurTokenPos));
  Result.Kind := pekSelf;
end;

function TPasParser.CreateNilExpr(AParent: TPasElement): TNilExpr;
begin
  Result := TNilExpr(CreateElement(TNilExpr,'nil',AParent,CurTokenPos));
  Result.Kind := pekNil;
end;

function TPasParser.CreateRecordValues(AParent: TPasElement): TRecordValues;
begin
  Result := TRecordValues(CreateElement(TRecordValues,'',AParent));
  Result.Kind := pekListOfExp;
end;

initialization
{$IFDEF HASFS}
  DefaultFileResolverClass := TFileResolver;
{$ENDIF}
end.
