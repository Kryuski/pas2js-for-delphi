{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2017 by Michael Van Canneyt, member of the
    Free Pascal development team

    DB database unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit DB;

{$mode objfpc}

{ $define dsdebug}
interface

uses Classes, SysUtils, JS, Types, DateUtils;

const

  dsMaxBufferCount = MAXINT div 8;
  dsMaxStringSize = 8192;

  // Used in AsBoolean for string fields to determine
  // whether it's true or false.
  YesNoChars : Array[Boolean] of char = ('N', 'Y');

  SQLDelimiterCharacters = [';',',',' ','(',')',#13,#10,#9];

type

{ Misc Dataset types }

  TDataSetState = (dsInactive, dsBrowse, dsEdit, dsInsert, dsSetKey,
    dsCalcFields, dsFilter, dsNewValue, dsOldValue, dsCurValue, dsBlockRead,
    dsInternalCalc, dsOpening, dsRefreshFields);

  TDataEvent = (deFieldChange, deRecordChange, deDataSetChange,
    deDataSetScroll, deLayoutChange, deUpdateRecord, deUpdateState,
    deCheckBrowseMode, dePropertyChange, deFieldListChange, deFocusControl,
    deParentScroll,deConnectChange,deReconcileError,deDisabledStateChange);

  TUpdateStatus = (usUnmodified, usModified, usInserted, usDeleted, usResolved, usResolveFailed);
  TUpdateStatusSet = Set of TUpdateStatus;

  TUpdateMode = (upWhereAll, upWhereChanged, upWhereKeyOnly);
  TResolverResponse = (rrSkip, rrAbort, rrMerge, rrApply, rrIgnore);

  TProviderFlag = (pfInUpdate, pfInWhere, pfInKey, pfHidden, pfRefreshOnInsert,pfRefreshOnUpdate);
  TProviderFlags = set of TProviderFlag;

{ Forward declarations }

  TFieldDef = class;
  TFieldDefs = class;
  TField = class;
  TFields = Class;
  TDataSet = class;
  TDataSource = Class;
  TDataLink = Class;
  TDataProxy = Class;
  TDataRequest = class;
  TRecordUpdateDescriptor = class;
  TRecordUpdateDescriptorList = class;
  TRecordUpdateBatch = class;

{ Exception classes }

  EDatabaseError = class(Exception);

  EUpdateError   = class(EDatabaseError)
  private
    FContext           : String;
    FErrorCode         : integer;
    FOriginalException : Exception;
    FPreviousError     : Integer;
  public
    constructor Create(NativeError, Context : String;
      ErrCode, PrevError : integer; E: Exception); reintroduce;
    Destructor Destroy; override;
    property Context : String read FContext;
    property ErrorCode : integer read FErrorcode;
    property OriginalException : Exception read FOriginalException;
    property PreviousError : Integer read FPreviousError;
  end;
  

{ TFieldDef }

  TFieldClass = class of TField;

  // Data type for field.
  TFieldType = (
    ftUnknown, ftString, ftInteger, ftLargeInt, ftBoolean, ftFloat, ftDate,
    ftTime, ftDateTime,  ftAutoInc, ftBlob, ftMemo, ftFixedChar,
    ftVariant,ftDataset
  );

{ TDateTimeRec }

  TFieldAttribute = (faHiddenCol, faReadonly, faRequired, faLink, faUnNamed, faFixed);
  TFieldAttributes = set of TFieldAttribute;

{ TNamedItem }

  TNamedItem = class(TCollectionItem)
  private
    FName: string;
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  Public  
    property DisplayName : string read GetDisplayName write SetDisplayName;
  published
    property Name : string read FName write SetDisplayName;
  end;

{ TDefCollection }

  TDefCollection = class(TOwnedCollection)
  private
    FDataset: TDataset;
    FUpdated: boolean;
  protected
    procedure SetItemName(Item: TCollectionItem); override;
  public
    constructor create(ADataset: TDataset; AOwner: TPersistent; AClass: TCollectionItemClass); reintroduce;
    function Find(const AName: string): TNamedItem;
    procedure GetItemNames(List: TStrings);
    function IndexOf(const AName: string): Longint;
    property Dataset: TDataset read FDataset;
    property Updated: boolean read FUpdated write FUpdated;
  end;

{ TFieldDef }

  TFieldDef = class(TNamedItem)
  Private
    FAttributes : TFieldAttributes;
    FDataType : TFieldType;
    FFieldNo : Longint;
    FInternalCalcField : Boolean;
    FPrecision : Longint;
    FRequired : Boolean;
    FSize : Integer;
    Function GetFieldClass : TFieldClass;
    procedure SetAttributes(AValue: TFieldAttributes);
    procedure SetDataType(AValue: TFieldType);
    procedure SetPrecision(const AValue: Longint);
    procedure SetSize(const AValue: Integer);
    procedure SetRequired(const AValue: Boolean);
  public
    constructor Create(ACollection : TCollection); override;
    constructor Create(AOwner: TFieldDefs; const AName: string;  ADataType: TFieldType; ASize: Integer; ARequired: Boolean; AFieldNo: Longint); overload;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function CreateField(AOwner: TComponent): TField;
    property FieldClass: TFieldClass read GetFieldClass;
    property FieldNo: Longint read FFieldNo;
    property InternalCalcField: Boolean read FInternalCalcField write FInternalCalcField;
    property Required: Boolean read FRequired write SetRequired;
  Published
    property Attributes: TFieldAttributes read FAttributes write SetAttributes default [];
    property DataType: TFieldType read FDataType write SetDataType;
    property Precision: Longint read FPrecision write SetPrecision default 0;
    property Size: Integer read FSize write SetSize default 0;
  end;
  TFieldDefClass = Class of TFieldDef;

{ TFieldDefs }

  TFieldDefs = class(TDefCollection)
  private
    FHiddenFields : Boolean;
    function GetItem(Index: Longint): TFieldDef; reintroduce;
    procedure SetItem(Index: Longint; const AValue: TFieldDef); reintroduce;
  Protected
    Class Function FieldDefClass : TFieldDefClass; virtual;
  public
    constructor Create(ADataSet: TDataSet); reintroduce;
//    destructor Destroy; override;
    Function Add(const AName: string; ADataType: TFieldType; ASize, APrecision{%H-}: Integer; ARequired, AReadOnly: Boolean; AFieldNo : Integer) : TFieldDef; overload;
    Function Add(const AName: string; ADataType: TFieldType; ASize: Word; ARequired: Boolean; AFieldNo : Integer) : TFieldDef; overload;
    procedure Add(const AName: string; ADataType: TFieldType; ASize: Word; ARequired: Boolean); overload;
    procedure Add(const AName: string; ADataType: TFieldType; ASize: Word); overload;
    procedure Add(const AName: string; ADataType: TFieldType); overload;
    Function AddFieldDef : TFieldDef;
    procedure Assign(FieldDefs: TFieldDefs); overload;
    function Find(const AName: string): TFieldDef; reintroduce;
//    procedure Clear;
//    procedure Delete(Index: Longint);
    procedure Update; overload;
    Function MakeNameUnique(const AName : String) : string; virtual;
    Property HiddenFields : Boolean Read FHiddenFields Write FHiddenFields;
    property Items[Index: Longint]: TFieldDef read GetItem write SetItem; default;
  end;
  TFieldDefsClass = Class of TFieldDefs;

{ TField }

  TFieldKind = (fkData, fkCalculated, fkLookup, fkInternalCalc);
  TFieldKinds = Set of TFieldKind;

  TFieldNotifyEvent = procedure(Sender: TField) of object;
  TFieldGetTextEvent = procedure(Sender: TField; var aText: string;
    DisplayText: Boolean) of object;
  TFieldSetTextEvent = procedure(Sender: TField; const aText: string) of object;
  TFieldChars = Array of Char;

{ TLookupList }

  TLookupList = class(TObject)
  private
    FList: TFPList;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Add(const AKey, AValue: JSValue);
    procedure Clear;
    function FirstKeyByValue(const AValue: JSValue): JSValue;
    function ValueOfKey(const AKey: JSValue): JSValue;
    procedure ValuesToStrings(AStrings: TStrings);
  end;

{ TField }

  TField = class(TComponent)
  private
    FAlignment : TAlignment;
    FAttributeSet : String;
    FCalculated : Boolean;
    FConstraintErrorMessage : String;
    FCustomConstraint : String;
    FDataSet : TDataSet;
//    FDataSize : Word;
    FDataType : TFieldType;
    FDefaultExpression : String;
    FDisplayLabel : String;
    FDisplayWidth : Longint;
//    FEditMask: TEditMask;
    FFieldDef: TFieldDef;
    FFieldKind : TFieldKind;
    FFieldName : String;
    FFieldNo : Longint;
    FFields : TFields;
    FHasConstraints : Boolean;
    FImportedConstraint : String;
    FIsIndexField : Boolean;
    FKeyFields : String;
    FLookupCache : Boolean;
    FLookupDataSet : TDataSet;
    FLookupKeyfields : String;
    FLookupresultField : String;
    FLookupList: TLookupList;
    FOnChange : TFieldNotifyEvent;
    FOnGetText: TFieldGetTextEvent;
    FOnSetText: TFieldSetTextEvent;
    FOnValidate: TFieldNotifyEvent;
    FOrigin : String;
    FReadOnly : Boolean;
    FRequired : Boolean;
    FSize : integer;
    FValidChars : TFieldChars;
    FValueBuffer : JSValue;
    FValidating : Boolean;
    FVisible : Boolean;
    FProviderFlags : TProviderFlags;
    function GetIndex : longint;
    function GetLookup: Boolean;
    procedure SetAlignment(const AValue: TAlignMent);
    procedure SetIndex(const AValue: Longint);
    function GetDisplayText: String;
    function GetEditText: String;
    procedure SetEditText(const AValue: string);
    procedure SetDisplayLabel(const AValue: string);
    procedure SetDisplayWidth(const AValue: Longint);
    function GetDisplayWidth: integer;
    procedure SetLookup(const AValue: Boolean);
    procedure SetReadOnly(const AValue: Boolean);
    procedure SetVisible(const AValue: Boolean);
    function IsDisplayLabelStored : Boolean;
    function IsDisplayWidthStored: Boolean;
    function GetLookupList: TLookupList;
    procedure CalcLookupValue;
  protected
    Procedure RaiseAccessError(const TypeName: string);
    function AccessError(const TypeName: string): EDatabaseError;
    procedure CheckInactive;
    class procedure CheckTypeSize(AValue: Longint); virtual;
    procedure Change; virtual;
    procedure Bind(Binding: Boolean); virtual;
    procedure DataChanged;
    function GetAsBoolean: Boolean; virtual;
    function GetAsBytes: TBytes; virtual;
    function GetAsLargeInt: NativeInt; virtual;
    function GetAsDateTime: TDateTime; virtual;
    function GetAsFloat: Double; virtual;
    function GetAsLongint: Longint; virtual;
    function GetAsInteger: Longint; virtual;
    function GetAsJSValue: JSValue; virtual;
    function GetOldValue: JSValue; virtual;
    function GetAsString: string; virtual;
    function GetCanModify: Boolean; virtual;
    function GetClassDesc: String; virtual;
    function GetDataSize: Integer; virtual;
    function GetDefaultWidth: Longint; virtual;
    function GetDisplayName : String;
    function GetCurValue: JSValue; virtual;
    function GetNewValue: JSValue; virtual;
    function GetIsNull: Boolean; virtual;
    procedure GetText(var AText: string; ADisplayText{%H-}: Boolean); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PropertyChanged(LayoutAffected: Boolean);
    procedure SetAsBoolean(AValue{%H-}: Boolean); virtual;
    procedure SetAsDateTime(AValue{%H-}: TDateTime); virtual;
    procedure SetAsFloat(AValue{%H-}: Double); virtual;
    procedure SetAsLongint(AValue: Longint); virtual;
    procedure SetAsInteger(AValue{%H-}: Longint); virtual;
    procedure SetAsLargeInt(AValue{%H-}: NativeInt); virtual;
    procedure SetAsJSValue(const AValue: JSValue); virtual;
    procedure SetAsString(const AValue{%H-}: string); virtual;
    procedure SetDataset(AValue : TDataset); virtual;
    procedure SetDataType(AValue: TFieldType);
    procedure SetNewValue(const AValue: JSValue);
    procedure SetSize(AValue: Integer); virtual;
    procedure SetParentComponent(Value: TComponent); override;
    procedure SetText(const AValue: string); virtual;
    procedure SetVarValue(const AValue{%H-}: JSValue); virtual;
    procedure SetAsBytes(const AValue{%H-}: TBytes); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignValue(const AValue: JSValue);
    procedure Clear; virtual;
    procedure FocusControl;
    function GetData : JSValue;
    class function IsBlob: Boolean; virtual;
    function IsValidChar(InputChar: Char): Boolean; virtual;
    procedure RefreshLookupList;
    procedure SetData(Buffer: JSValue); overload;
    procedure SetFieldType(AValue{%H-}: TFieldType); virtual;
    procedure Validate(Buffer: Pointer);
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsLongint: Longint read GetAsLongint write SetAsLongint;
    property AsLargeInt: NativeInt read GetAsLargeInt write SetAsLargeInt;
    property AsInteger: Longint read GetAsInteger write SetAsInteger;
    property AsString: string read GetAsString write SetAsString;
    property AsJSValue: JSValue read GetAsJSValue write SetAsJSValue;
    property AttributeSet: string read FAttributeSet write FAttributeSet;
    property Calculated: Boolean read FCalculated write FCalculated;
    property CanModify: Boolean read GetCanModify;
    property CurValue: JSValue read GetCurValue;
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property DataSize: Integer read GetDataSize;
    property DataType: TFieldType read FDataType;
    property DisplayName: String Read GetDisplayName;
    property DisplayText: String read GetDisplayText;
    property FieldNo: Longint read FFieldNo;
    property IsIndexField: Boolean read FIsIndexField;
    property IsNull: Boolean read GetIsNull;
    property Lookup: Boolean read GetLookup write SetLookup; deprecated;
    property NewValue: JSValue read GetNewValue write SetNewValue;
    property Size: Integer read FSize write SetSize;
    property Text: string read GetEditText write SetEditText;
    property ValidChars : TFieldChars read FValidChars write FValidChars;
    property Value: JSValue read GetAsJSValue write SetAsJSValue;
    property OldValue: JSValue read GetOldValue;
    property LookupList: TLookupList read GetLookupList;
    Property FieldDef : TFieldDef Read FFieldDef;
  published
    property Alignment : TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property CustomConstraint: string read FCustomConstraint write FCustomConstraint;
    property ConstraintErrorMessage: string read FConstraintErrorMessage write FConstraintErrorMessage;
    property DefaultExpression: string read FDefaultExpression write FDefaultExpression;
    property DisplayLabel : string read GetDisplayName write SetDisplayLabel stored IsDisplayLabelStored;
    property DisplayWidth: Longint read GetDisplayWidth write SetDisplayWidth stored IsDisplayWidthStored;
    property FieldKind: TFieldKind read FFieldKind write FFieldKind;
    property FieldName: string read FFieldName write FFieldName;
    property HasConstraints: Boolean read FHasConstraints;
    property Index: Longint read GetIndex write SetIndex;
    property ImportedConstraint: string read FImportedConstraint write FImportedConstraint;
    property KeyFields: string read FKeyFields write FKeyFields;
    property LookupCache: Boolean read FLookupCache write FLookupCache;
    property LookupDataSet: TDataSet read FLookupDataSet write FLookupDataSet;
    property LookupKeyFields: string read FLookupKeyFields write FLookupKeyFields;
    property LookupResultField: string read FLookupResultField write FLookupResultField;
    property Origin: string read FOrigin write FOrigin;
    property ProviderFlags : TProviderFlags read FProviderFlags write FProviderFlags;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property Required: Boolean read FRequired write FRequired;
    property Visible: Boolean read FVisible write SetVisible default True;
    property OnChange: TFieldNotifyEvent read FOnChange write FOnChange;
    property OnGetText: TFieldGetTextEvent read FOnGetText write FOnGetText;
    property OnSetText: TFieldSetTextEvent read FOnSetText write FOnSetText;
    property OnValidate: TFieldNotifyEvent read FOnValidate write FOnValidate;
  end;

{ TStringField }

  TStringField = class(TField)
  private
    FFixedChar     : boolean;
    FTransliterate : Boolean;
  protected
    class procedure CheckTypeSize(AValue: Longint); override;
    function GetAsBoolean: Boolean; override;
    function GetAsDateTime: TDateTime; override;
    function GetAsFloat: Double; override;
    function GetAsInteger: Longint; override;
    function GetAsLargeInt: NativeInt; override;
    function GetAsString: String; override;
    function GetAsJSValue: JSValue; override;
    function GetDefaultWidth: Longint; override;
    procedure GetText(var AText: string; ADisplayText{%H-}: Boolean); override;
    procedure SetAsBoolean(AValue: Boolean); override;
    procedure SetAsDateTime(AValue: TDateTime); override;
    procedure SetAsFloat(AValue: Double); override;
    procedure SetAsInteger(AValue: Longint); override;
    procedure SetAsLargeInt(AValue: NativeInt); override;
    procedure SetAsString(const AValue: String); override;
    procedure SetVarValue(const AValue: JSValue); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetFieldType(AValue: TFieldType); override;
    property FixedChar : Boolean read FFixedChar write FFixedChar;
    property Transliterate: Boolean read FTransliterate write FTransliterate;
    property Value: String read GetAsString write SetAsString;
  published
    property Size default 20;
  end;


{ TNumericField }

  TNumericField = class(TField)
  Private
    FDisplayFormat : String;
    FEditFormat : String;
  protected
    class procedure CheckTypeSize(AValue: Longint); override;
    procedure RangeError(AValue, Min, Max: Double);
    procedure SetDisplayFormat(const AValue: string);
    procedure SetEditFormat(const AValue: string);
    function  GetAsBoolean: Boolean; override;
    Procedure SetAsBoolean(AValue: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Alignment default taRightJustify;
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
    property EditFormat: string read FEditFormat write SetEditFormat;
  end;

{ TLongintField }

  TIntegerField = class(TNumericField)
  private
    FMinValue,
    FMaxValue,
    FMinRange,
    FMaxRange  : Longint;
    Procedure SetMinValue (AValue : longint);
    Procedure SetMaxValue (AValue : longint);
  protected
    function GetAsFloat: Double; override;
    function GetAsInteger: Longint; override;
    function GetAsString: string; override;
    function GetAsJSValue: JSValue; override;
    procedure GetText(var AText: string; ADisplayText: Boolean); override;
    function GetValue(var AValue: Longint): Boolean;
    procedure SetAsFloat(AValue: Double); override;
    procedure SetAsInteger(AValue: Longint); override;
    procedure SetAsString(const AValue: string); override;
    procedure SetVarValue(const AValue: JSValue); override;
    function GetAsLargeInt: NativeInt; override;
    procedure SetAsLargeInt(AValue: NativeInt); override;
  public
    constructor Create(AOwner: TComponent); override;
    Function CheckRange(AValue : Longint) : Boolean;
    property Value: Longint read GetAsInteger write SetAsInteger;
  published
    property MaxValue: Longint read FMaxValue write SetMaxValue default 0;
    property MinValue: Longint read FMinValue write SetMinValue default 0;
  end;

{ TLargeintField }

  TLargeintField = class(TNumericField)
  private
    FMinValue,
    FMaxValue,
    FMinRange,
    FMaxRange  : NativeInt;
    Procedure SetMinValue (AValue : NativeInt);
    Procedure SetMaxValue (AValue : NativeInt);
  protected
    function GetAsFloat: Double; override;
    function GetAsInteger: Longint; override;
    function GetAsLargeInt: NativeInt; override;
    function GetAsString: string; override;
    function GetAsJSValue: JSValue; override;
    procedure GetText(var AText: string; ADisplayText: Boolean); override;
    function GetValue(var AValue: NativeInt): Boolean;
    procedure SetAsFloat(AValue: Double); override;
    procedure SetAsInteger(AValue: Longint); override;
    procedure SetAsLargeInt(AValue: NativeInt); override;
    procedure SetAsString(const AValue: string); override;
    procedure SetVarValue(const AValue: JSValue); override;
  public
    constructor Create(AOwner: TComponent); override;
    Function CheckRange(AValue : NativeInt) : Boolean;
    property Value: NativeInt read GetAsLargeInt write SetAsLargeInt;
  published
    property MaxValue: NativeInt read FMaxValue write SetMaxValue default 0;
    property MinValue: NativeInt read FMinValue write SetMinValue default 0;
  end;

{ TAutoIncField }

  TAutoIncField = class(TIntegerField)
  Protected
    procedure SetAsInteger(AValue: Longint); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TFloatField }

  TFloatField = class(TNumericField)
  private
    FCurrency: Boolean;
    FMaxValue : Double;
    FMinValue : Double;
    FPrecision : Longint;
    procedure SetCurrency(const AValue: Boolean);
    procedure SetPrecision(const AValue: Longint);
  protected
    function GetAsFloat: Double; override;
    function GetAsLargeInt: NativeInt; override;
    function GetAsInteger: Longint; override;
    function GetAsJSValue: JSValue; override;
    function GetAsString: string; override;
    procedure GetText(var AText: string; ADisplayText: Boolean); override;
    procedure SetAsFloat(AValue: Double); override;
    procedure SetAsLargeInt(AValue: NativeInt); override;
    procedure SetAsInteger(AValue: Longint); override;
    procedure SetAsString(const AValue: string); override;
    procedure SetVarValue(const AValue: JSValue); override;
  public
    constructor Create(AOwner: TComponent); override;
    Function CheckRange(AValue : Double) : Boolean;
    property Value: Double read GetAsFloat write SetAsFloat;

  published
    property Currency: Boolean read FCurrency write SetCurrency default False;
    property MaxValue: Double read FMaxValue write FMaxValue;
    property MinValue: Double read FMinValue write FMinValue;
    property Precision: Longint read FPrecision write SetPrecision default 15; // min 2 instellen, delphi compat
  end;


{ TBooleanField }

  TBooleanField = class(TField)
  private
    FDisplayValues : String;
    // First byte indicates uppercase or not.
    FDisplays : Array[Boolean,Boolean] of string;
    Procedure SetDisplayValues(const AValue : String);
  protected
    function GetAsBoolean: Boolean; override;
    function GetAsString: string; override;
    function GetAsJSValue: JSValue; override;
    function GetAsInteger: Longint; override;
    function GetDefaultWidth: Longint; override;
    procedure SetAsBoolean(AValue: Boolean); override;
    procedure SetAsString(const AValue: string); override;
    procedure SetAsInteger(AValue: Longint); override;
    procedure SetVarValue(const AValue: JSValue); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: Boolean read GetAsBoolean write SetAsBoolean;
  published
    property DisplayValues: string read FDisplayValues write SetDisplayValues;
  end;

{ TDateTimeField }

  TDateTimeField = class(TField)
  private
    FDisplayFormat : String;
    procedure SetDisplayFormat(const AValue: string);
  protected
    Function ConvertToDateTime(aValue : JSValue; aRaiseError : Boolean) : TDateTime; virtual;
    Function DateTimeToNativeDateTime(aValue : TDateTime) : JSValue; virtual;
    function GetAsDateTime: TDateTime; override;
    function GetAsFloat: Double; override;
    function GetAsString: string; override;
    function GetAsJSValue: JSValue; override;
    function GetDataSize: Integer; override;
    procedure GetText(var AText: string; ADisplayText: Boolean); override;
    procedure SetAsDateTime(AValue: TDateTime); override;
    procedure SetAsFloat(AValue: Double); override;
    procedure SetAsString(const AValue: string); override;
    procedure SetVarValue(const AValue: JSValue); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Value: TDateTime read GetAsDateTime write SetAsDateTime;
  published
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
  end;

{ TDateField }

  TDateField = class(TDateTimeField)
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TTimeField }

  TTimeField = class(TDateTimeField)
  protected
    procedure SetAsString(const AValue: string); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TBinaryField }

  TBinaryField = class(TField)
  protected
    class procedure CheckTypeSize(AValue: Longint); override;
    Function BlobToBytes(aValue : JSValue) : TBytes; virtual;
    Function BytesToBlob(aValue : TBytes) : JSValue; virtual;
    function GetAsString: string; override;
    function GetAsJSValue: JSValue; override;
    function GetValue(var AValue: TBytes): Boolean;
    procedure SetAsString(const AValue: string); override;
    procedure SetVarValue(const AValue: JSValue); override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Size default 16;
  end;

{ TBytesField }


{ TBlobField }
  TBlobStreamMode = (bmRead, bmWrite, bmReadWrite);
//  TBlobType = ftBlob..ftMemo;

  TBlobField = class(TBinaryField)
  private
    FModified : Boolean;
    // Wrapper that retrieves FDataType as a TBlobType
    //   function GetBlobType: TBlobType;
    // Wrapper that calls SetFieldType
    //   procedure SetBlobType(AValue: TBlobType);
  protected
    function GetBlobSize: Longint; virtual;
    function GetIsNull: Boolean; override;
    procedure GetText(var AText: string; ADisplayText{%H-}: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Clear; override;
    class function IsBlob: Boolean; override;
    procedure SetFieldType(AValue: TFieldType); override;
    property BlobSize: Longint read GetBlobSize;
    property Modified: Boolean read FModified write FModified;
    property Value: string read GetAsString write SetAsString;
  published
   // property BlobType: TBlobType read GetBlobType write SetBlobType; // default ftBlob;
    property Size default 0;
  end;

{ TMemoField }

  TMemoField = class(TBlobField)
  public
    constructor Create(AOwner: TComponent); override;
  end;


{ TVariantField }

  TVariantField = class(TField)
  protected
    class procedure CheckTypeSize(aValue{%H-}: Integer); override;

    function GetAsBoolean: Boolean; override;
    procedure SetAsBoolean(aValue: Boolean); override;

    function GetAsDateTime: TDateTime; override;
    procedure SetAsDateTime(aValue: TDateTime); override;

    function GetAsFloat: Double; override;
    procedure SetAsFloat(aValue: Double); override;

    function GetAsInteger: Longint; override;
    procedure SetAsInteger(AValue: Longint); override;

    function GetAsString: string; override;
    procedure SetAsString(const aValue: string); override;


    function GetAsJSValue: JSValue; override;
    procedure SetVarValue(const aValue: JSValue); override;

  public
    constructor Create(AOwner: TComponent); override;
  end;

{ TIndexDef }

  TIndexDefs = class;

  TIndexOption = (ixPrimary, ixUnique, ixDescending, ixCaseInsensitive,
    ixExpression, ixNonMaintained);
  TIndexOptions = set of TIndexOption;

  TIndexDef = class(TNamedItem)
  Private
    FCaseinsFields: string;
    FDescFields: string;
    FExpression : String;
    FFields : String;
    FOptions : TIndexOptions;
    FSource : String;
  protected
    function GetExpression: string;
    procedure SetCaseInsFields(const AValue: string); virtual;
    procedure SetDescFields(const AValue: string);
    procedure SetExpression(const AValue: string);
  public
    constructor Create(Owner: TIndexDefs; const AName, TheFields: string;
      TheOptions: TIndexOptions); overload;
    procedure Assign(Source: TPersistent); override;
  published
    property Expression: string read GetExpression write SetExpression;
    property Fields: string read FFields write FFields;
    property CaseInsFields: string read FCaseinsFields write SetCaseInsFields;
    property DescFields: string read FDescFields write SetDescFields;
    property Options: TIndexOptions read FOptions write FOptions;
    property Source: string read FSource write FSource;
  end;
  TIndexDefClass = class of TIndexDef;
{ TIndexDefs }

  TIndexDefs = class(TDefCollection)
  Private
    Function  GetItem(Index: Integer): TIndexDef; reintroduce;
    Procedure SetItem(Index: Integer; Value: TIndexDef); reintroduce;
  public
    constructor Create(ADataSet: TDataSet); virtual; overload;
    procedure Add(const Name, Fields: string; Options: TIndexOptions); reintroduce;
    Function AddIndexDef: TIndexDef;
    function Find(const IndexName: string): TIndexDef; reintroduce;
    function FindIndexForFields(const Fields{%H-}: string): TIndexDef;
    function GetIndexForFields(const Fields: string;
      CaseInsensitive: Boolean): TIndexDef;
    procedure Update; overload; virtual;
    Property Items[Index: Integer] : TIndexDef read GetItem write SetItem; default;
  end;

{ TCheckConstraint }

  TCheckConstraint = class(TCollectionItem)
  Private
    FCustomConstraint : String;
    FErrorMessage : String;
    FFromDictionary : Boolean;
    FImportedConstraint : String;
  public
    procedure Assign(Source{%H-}: TPersistent); override;
  //  function GetDisplayName: string; override;
  published
    property CustomConstraint: string read FCustomConstraint write FCustomConstraint;
    property ErrorMessage: string read FErrorMessage write FErrorMessage;
    property FromDictionary: Boolean read FFromDictionary write FFromDictionary;
    property ImportedConstraint: string read FImportedConstraint write FImportedConstraint;
  end;

{ TCheckConstraints }

  TCheckConstraints = class(TCollection)
  Private
   Function GetItem(Index{%H-} : Longint) : TCheckConstraint; reintroduce;
   Procedure SetItem(index{%H-} : Longint; Value{%H-} : TCheckConstraint); reintroduce;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner{%H-}: TPersistent); reintroduce;
    function Add: TCheckConstraint; reintroduce;
    property Items[Index: Longint]: TCheckConstraint read GetItem write SetItem; default;
  end;

  { TFieldsEnumerator }

  TFieldsEnumerator = class
  private
    FPosition: Integer;
    FFields: TFields;
    function GetCurrent: TField;
  public
    constructor Create(AFields: TFields); reintroduce;
    function MoveNext: Boolean;
    property Current: TField read GetCurrent;
  end;

{ TFields }

  TFields = Class(TObject)
  Private
    FDataset : TDataset;
    FFieldList : TFpList;
    FOnChange : TNotifyEvent;
    FValidFieldKinds : TFieldKinds;
  Protected
    Procedure ClearFieldDefs;
    Procedure Changed;
    Procedure CheckfieldKind(Fieldkind : TFieldKind; Field : TField);
    Function GetCount : Longint;
    Function GetField (Index : Integer) : TField;
    Procedure SetField(Index: Integer; Value: TField);
    Procedure SetFieldIndex (Field : TField;Value : Integer);
    Property OnChange : TNotifyEvent Read FOnChange Write FOnChange;
    Property ValidFieldKinds : TFieldKinds Read FValidFieldKinds;
  Public
    Constructor Create(ADataset : TDataset); reintroduce;
    Destructor Destroy;override;
    Procedure Add(Field : TField);
    Procedure CheckFieldName (Const Value : String);
    Procedure CheckFieldNames (Const Value : String);
    Procedure Clear;
    Function FindField (Const Value : String) : TField;
    Function FieldByName (Const Value : String) : TField;
    Function FieldByNumber(FieldNo : Integer) : TField;
    Function GetEnumerator: TFieldsEnumerator;
    Procedure GetFieldNames (Values : TStrings);
    Function IndexOf(Field : TField) : Longint;
    procedure Remove(Value : TField);
    Property Count : Integer Read GetCount;
    Property Dataset : TDataset Read FDataset;
    Property Fields [Index : Integer] : TField Read GetField Write SetField; default;
  end;
  TFieldsClass = Class of TFields;

{ TParam }

  TBlobData = TBytes;  // Delphi defines it as alias to TBytes

  TParamBinding = array of integer;

  TParamType = (ptUnknown, ptInput, ptOutput, ptInputOutput, ptResult);
  TParamTypes = set of TParamType;

  TParamStyle = (psInterbase,psPostgreSQL,psSimulated);

  TParams = class;

  TParam = class(TCollectionItem)
  private
    FValue: JSValue;
    FPrecision: Integer;
    FNumericScale: Integer;
    FName: string;
    FDataType: TFieldType;
    FBound: Boolean;
    FParamType: TParamType;
    FSize: Integer;
    Function GetDataSet: TDataSet;
    Function IsParamStored: Boolean;
  protected
    Procedure AssignParam(Param: TParam);
    Procedure AssignTo(Dest: TPersistent); override;
    Function GetAsBoolean: Boolean;
    Function GetAsBytes: TBytes;
    Function GetAsDateTime: TDateTime;
    Function GetAsFloat: Double;
    Function GetAsInteger: Longint;
    Function GetAsLargeInt: NativeInt;
    Function GetAsMemo: string;
    Function GetAsString: string;
    Function GetAsJSValue: JSValue;
    Function GetDisplayName: string; override;
    Function GetIsNull: Boolean;
    Function IsEqual(AValue: TParam): Boolean;
    Procedure SetAsBlob(const AValue: TBlobData);
    Procedure SetAsBoolean(AValue: Boolean);
    Procedure SetAsBytes(const AValue{%H-}: TBytes);
    Procedure SetAsDate(const AValue: TDateTime);
    Procedure SetAsDateTime(const AValue: TDateTime);
    Procedure SetAsFloat(const AValue: Double);
    Procedure SetAsInteger(AValue: Longint);
    Procedure SetAsLargeInt(AValue: NativeInt);
    Procedure SetAsMemo(const AValue: string);
    Procedure SetAsString(const AValue: string);
    Procedure SetAsTime(const AValue: TDateTime);
    Procedure SetAsJSValue(const AValue: JSValue);
    Procedure SetDataType(AValue: TFieldType);
    Procedure SetText(const AValue: string);
  public
    constructor Create(ACollection: TCollection); overload; override;
    constructor Create(AParams: TParams; AParamType: TParamType); reintroduce; overload;
    Procedure Assign(Source: TPersistent); override;
    Procedure AssignField(Field: TField);
    Procedure AssignToField(Field: TField);
    Procedure AssignFieldValue(Field: TField; const AValue: JSValue);
    Procedure AssignFromField(Field : TField);
    Procedure Clear;
    Property AsBlob : TBlobData read GetAsBytes  write SetAsBytes;
    Property AsBoolean : Boolean read GetAsBoolean write SetAsBoolean;
    Property AsBytes : TBytes read GetAsBytes write SetAsBytes;
    Property AsDate : TDateTime read GetAsDateTime write SetAsDate;
    Property AsDateTime : TDateTime read GetAsDateTime write SetAsDateTime;
    Property AsFloat : Double read GetAsFloat write SetAsFloat;
    Property AsInteger : LongInt read GetAsInteger write SetAsInteger;
    Property AsLargeInt : NativeInt read GetAsLargeInt write SetAsLargeInt;
    Property AsMemo : string read GetAsMemo write SetAsMemo;
    Property AsSmallInt : LongInt read GetAsInteger write SetAsInteger;
    Property AsString : string read GetAsString write SetAsString;
    Property AsTime : TDateTime read GetAsDateTime write SetAsTime;
    Property Bound : Boolean read FBound write FBound;
    Property Dataset : TDataset Read GetDataset;
    Property IsNull : Boolean read GetIsNull;
    Property Text : string read GetAsString write SetText;
  published
    Property DataType : TFieldType read FDataType write SetDataType;
    Property Name : string read FName write FName;
    Property NumericScale : Integer read FNumericScale write FNumericScale default 0;
    Property ParamType : TParamType read FParamType write FParamType;
    Property Precision : Integer read FPrecision write FPrecision default 0;
    Property Size : Integer read FSize write FSize default 0;
    Property Value : JSValue read GetAsJSValue write SetAsJSValue stored IsParamStored;
  end;
  TParamClass = Class of TParam;

{ TParams }

  TParams = class(TCollection)
  private
    FOwner: TPersistent;
    Function  GetItem(Index: Integer): TParam; reintroduce;
    Function  GetParamValue(const ParamName: string): JSValue;
    Procedure SetItem(Index: Integer; Value: TParam); reintroduce;
    Procedure SetParamValue(const ParamName: string; const Value: JSValue);
  protected
    Procedure AssignTo(Dest: TPersistent); override;
    Function  GetDataSet: TDataSet;
    Function  GetOwner: TPersistent; override;
    Class Function ParamClass : TParamClass; virtual;
  public
    Constructor Create(AOwner: TPersistent; AItemClass : TCollectionItemClass); overload;
    Constructor Create(AOwner: TPersistent); overload;
    Constructor Create; overload; reintroduce;
    Procedure AddParam(Value: TParam);
    Procedure AssignValues(Value: TParams);
    Function  CreateParam(FldType: TFieldType; const ParamName: string; ParamType: TParamType): TParam;
    Function  FindParam(const Value: string): TParam;
    Procedure GetParamList(List: TList; const ParamNames: string);
    Function  IsEqual(Value: TParams): Boolean;
    Function  ParamByName(const Value: string): TParam;
    Function  ParseSQL(SQL: String; DoCreate: Boolean): String; overload;
    Function  ParseSQL(SQL: String; DoCreate, EscapeSlash, EscapeRepeat : Boolean; ParameterStyle : TParamStyle): String; overload;
    Function  ParseSQL(SQL: String; DoCreate, EscapeSlash, EscapeRepeat : Boolean; ParameterStyle : TParamStyle; out ParamBinding: TParambinding): String; overload;
    Function  ParseSQL(SQL: String; DoCreate, EscapeSlash, EscapeRepeat : Boolean; ParameterStyle : TParamStyle; out ParamBinding: TParambinding; out ReplaceString : string): String; overload;
    Procedure RemoveParam(Value: TParam);
    Procedure CopyParamValuesFromDataset(ADataset : TDataset; CopyBound : Boolean);
    Property Dataset : TDataset Read GetDataset;
    Property Items[Index: Integer] : TParam read GetItem write SetItem; default;
    Property ParamValues[const ParamName: string] : JSValue read GetParamValue write SetParamValue;
  end;

{ TDataSet }
  
  TBookmarkFlag = (bfCurrent, bfBOF, bfEOF, bfInserted);
  TBookmark = record
    Data : JSValue;
    Flag : TBookmarkFlag;
  end; // Bookmark is always the index in the data array.
  TBookmarkStr = string; // JSON encoded version of the above

  TGetMode = (gmCurrent, gmNext, gmPrior);
  TGetResult = (grOK, grBOF, grEOF, grError);

  TResyncMode = set of (rmExact, rmCenter);
  TDataAction = (daFail, daAbort, daRetry);
  TUpdateAction = (uaFail, uaAbort, uaSkip, uaRetry, uaApplied);
  TUpdateKind = (ukModify, ukInsert, ukDelete);

  TLocateOption = (loCaseInsensitive, loPartialKey);
  TLocateOptions = set of TLocateOption;
  TDataOperation = procedure of object;

  TDataSetNotifyEvent = procedure(DataSet: TDataSet) of object;
  TDataSetErrorEvent = procedure(DataSet: TDataSet; E: EDatabaseError;
    var DataAction: TDataAction) of object;

  TFilterOption = (foCaseInsensitive, foNoPartialCompare);
  TFilterOptions = set of TFilterOption;

  TLoadOption = (loNoOpen,loNoEvents,loAtEOF);
  TLoadOptions = Set of TLoadOption;
  TDatasetLoadEvent = procedure(DataSet: TDataSet; Data : JSValue) of object;
  TDatasetLoadFailEvent = procedure(DataSet: TDataSet; ID : Integer; Const ErrorMsg : String) of object;

  TFilterRecordEvent = procedure(DataSet: TDataSet;
    var Accept: Boolean) of object;
  TDatasetClass = Class of TDataset;

  TRecordState = (rsNew,rsClean,rsUpdate,rsDelete);
  TDataRecord = record
    data : JSValue;
    state : TRecordState;
    bookmark : JSValue;
    bookmarkFlag : TBookmarkFlag;
  end;
  TBuffers = Array of TDataRecord;

  TResolveInfo = record
    Data : JSValue;
    Status : TUpdateStatus;
    Error : String; // Only filled on error.
    BookMark : TBookmark;
    _private : JSValue; // for use by descendents of TDataset
  end;
  TResolveInfoArray = Array of TResolveInfo;

  // Record so we can extend later on
  TResolveResults = record
    Records : TResolveInfoArray;
  end;

  TOnRecordResolveEvent = Procedure (Sender : TDataset; info : TResolveInfo) of object;
  TApplyUpdatesEvent = Procedure (Sender : TDataset; info : TResolveResults) of object;

{------------------------------------------------------------------------------}

  TDataSet = class(TComponent)
  Private
    FAfterApplyUpdates: TApplyUpdatesEvent;
    FAfterLoad: TDatasetNotifyEvent;
    FBeforeApplyUpdates: TDatasetNotifyEvent;
    FBeforeLoad: TDatasetNotifyEvent;
    FBlockReadSize: Integer;
    FCalcBuffer: TDataRecord;
    FCalcFieldsCount: Longint;
    FOnLoadFail: TDatasetLoadFailEvent;
    FOnRecordResolved: TOnRecordResolveEvent;
    FOpenAfterRead : boolean;
    FActiveRecord: Longint;
    FAfterCancel: TDataSetNotifyEvent;
    FAfterClose: TDataSetNotifyEvent;
    FAfterDelete: TDataSetNotifyEvent;
    FAfterEdit: TDataSetNotifyEvent;
    FAfterInsert: TDataSetNotifyEvent;
    FAfterOpen: TDataSetNotifyEvent;
    FAfterPost: TDataSetNotifyEvent;
    FAfterRefresh: TDataSetNotifyEvent;
    FAfterScroll: TDataSetNotifyEvent;
    FAutoCalcFields: Boolean;
    FBOF: Boolean;
    FBeforeCancel: TDataSetNotifyEvent;
    FBeforeClose: TDataSetNotifyEvent;
    FBeforeDelete: TDataSetNotifyEvent;
    FBeforeEdit: TDataSetNotifyEvent;
    FBeforeInsert: TDataSetNotifyEvent;
    FBeforeOpen: TDataSetNotifyEvent;
    FBeforePost: TDataSetNotifyEvent;
    FBeforeRefresh: TDataSetNotifyEvent;
    FBeforeScroll: TDataSetNotifyEvent;
    FBlobFieldCount: Longint;
    FBuffers : TBuffers;
    FBufferCount: Longint;
    FConstraints: TCheckConstraints;
    FDisableControlsCount : Integer;
    FDisableControlsState : TDatasetState;
    FCurrentRecord: Longint;
    FDataSources : TFPList;
    FDefaultFields: Boolean;
    FEOF: Boolean;
    FEnableControlsEvent : TDataEvent;
    FFieldList : TFields;
    FFieldDefs: TFieldDefs;
    FFilterOptions: TFilterOptions;
    FFilterText: string;
    FFiltered: Boolean;
    FFound: Boolean;
    FInternalCalcFields: Boolean;
    FModified: Boolean;
    FOnCalcFields: TDataSetNotifyEvent;
    FOnDeleteError: TDataSetErrorEvent;
    FOnEditError: TDataSetErrorEvent;
    FOnFilterRecord: TFilterRecordEvent;
    FOnNewRecord: TDataSetNotifyEvent;
    FOnPostError: TDataSetErrorEvent;
    FRecordCount: Longint;
    FIsUniDirectional: Boolean;
    FState : TDataSetState;
    FInternalOpenComplete: Boolean;
    FDataProxy : TDataProxy;
    FDataRequestID : Integer;
    FUpdateBatchID : Integer;
    FChangeList : TFPList;
    FBatchList : TFPList;
    Procedure DoInsertAppend(DoAppend : Boolean);
    Procedure DoInternalOpen;
    Function  GetBuffer (Index : longint) : TDataRecord;
    function GetBufferCount: Longint;
    function GetDataProxy: TDataProxy;
    Procedure RegisterDataSource(ADataSource : TDataSource);
    procedure SetConstraints(Value: TCheckConstraints);
    procedure SetDataProxy(AValue: TDataProxy);
    Procedure ShiftBuffersForward;
    Procedure ShiftBuffersBackward;
    Function  TryDoing (P : TDataOperation; Ev : TDatasetErrorEvent) : Boolean;
    Function GetActive : boolean;
    Procedure UnRegisterDataSource(ADataSource : TDataSource);
    procedure SetBlockReadSize(AValue: Integer); virtual;
    Procedure SetFieldDefs(AFieldDefs: TFieldDefs);
    procedure DoInsertAppendRecord(const Values: array of jsValue; DoAppend : boolean);
    // Callback for Tdataproxy.DoGetData;
    function ResolveRecordUpdate(anUpdate: TRecordUpdateDescriptor): Boolean;
    procedure HandleRequestresponse(ARequest: TDataRequest);
  protected
    // Proxy methods
    // Override this to integrate package in local data
    // call OnRecordResolved
    procedure DoOnRecordResolved(anUpdate: TRecordUpdateDescriptor); virtual;
    // Convert TRecordUpdateDescriptor to ResolveInfo
    function RecordUpdateDescriptorToResolveInfo(anUpdate: TRecordUpdateDescriptor): TResolveInfo; virtual;
    function DoResolveRecordUpdate(anUpdate{%H-}: TRecordUpdateDescriptor): Boolean; virtual;
    Function GetRecordUpdates(AList: TRecordUpdateDescriptorList) : Integer; virtual;
    procedure ResolveUpdateBatch(Sender: TObject; aBatch: TRecordUpdateBatch); virtual;
    Function DataPacketReceived(ARequest{%H-}: TDataRequest) : Boolean; virtual;
    function DoLoad(aOptions: TLoadOptions; aAfterLoad: TDatasetLoadEvent): Boolean; virtual;
    function DoGetDataProxy: TDataProxy; virtual;
    Procedure InitChangeList; virtual;
    Procedure DoneChangeList; virtual;
    Procedure ClearChangeList;
    Function IndexInChangeList(aBookmark: TBookmark): Integer; virtual;
    Function AddToChangeList(aChange : TUpdateStatus) : TRecordUpdateDescriptor ; virtual;
    Procedure RemoveFromChangeList(R : TRecordUpdateDescriptor); virtual;
    Procedure DoApplyUpdates;
    procedure RecalcBufListSize;
    procedure ActivateBuffers; virtual;
    procedure BindFields(Binding: Boolean);
    procedure BlockReadNext; virtual;
    function  BookmarkAvailable: Boolean;
    procedure CalculateFields(Var Buffer: TDataRecord); virtual;
    procedure CheckActive; virtual;
    procedure CheckInactive; virtual;
    procedure CheckBiDirectional;
    procedure Loaded; override;
    procedure ClearBuffers; virtual;
    procedure ClearCalcFields(var Buffer{%H-}: TDataRecord); virtual;
    procedure CloseBlob(Field{%H-}: TField); virtual;
    procedure CloseCursor; virtual;
    procedure CreateFields; virtual;
    procedure DataEvent(Event: TDataEvent; Info: JSValue); virtual;
    procedure DestroyFields; virtual;
    procedure DoAfterCancel; virtual;
    procedure DoAfterClose; virtual;
    procedure DoAfterDelete; virtual;
    procedure DoAfterEdit; virtual;
    procedure DoAfterInsert; virtual;
    procedure DoAfterOpen; virtual;
    procedure DoAfterPost; virtual;
    procedure DoAfterScroll; virtual;
    procedure DoAfterRefresh; virtual;
    procedure DoBeforeCancel; virtual;
    procedure DoBeforeClose; virtual;
    procedure DoBeforeDelete; virtual;
    procedure DoBeforeEdit; virtual;
    procedure DoBeforeInsert; virtual;
    procedure DoBeforeOpen; virtual;
    procedure DoBeforePost; virtual;
    procedure DoBeforeScroll; virtual;
    procedure DoBeforeRefresh; virtual;
    procedure DoOnCalcFields; virtual;
    procedure DoOnNewRecord; virtual;
    procedure DoBeforeLoad; virtual;
    procedure DoAfterLoad; virtual;
    procedure DoBeforeApplyUpdates; virtual;
    procedure DoAfterApplyUpdates(const ResolveInfo: TResolveResults); virtual;
    function  FieldByNumber(FieldNo: Longint): TField;
    function  FindRecord(Restart{%H-}, GoForward{%H-}: Boolean): Boolean; virtual;
    function  GetBookmarkStr: TBookmarkStr; virtual;
    procedure GetCalcFields(Var Buffer: TDataRecord); virtual;
    function  GetCanModify: Boolean; virtual;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    function  GetFieldClass(FieldType: TFieldType): TFieldClass; virtual;
    Function  GetfieldCount : Integer;
    function  GetFieldValues(const FieldName : string) : JSValue; virtual;
    function  GetIsIndexField(Field{%H-}: TField): Boolean; virtual;
    function  GetIndexDefs(IndexDefs : TIndexDefs; IndexTypes : TIndexOptions) : TIndexDefs;
    function  GetNextRecords: Longint; virtual;
    function  GetNextRecord: Boolean; virtual;
    function  GetPriorRecords: Longint; virtual;
    function  GetPriorRecord: Boolean; virtual;
    function  GetRecordCount: Longint; virtual;
    function  GetRecNo: Longint; virtual;
    procedure InitFieldDefs; virtual;
    procedure InitFieldDefsFromfields;
    procedure InitRecord(var Buffer: TDataRecord); virtual;
    procedure InternalCancel; virtual;
    procedure InternalEdit; virtual;
    procedure InternalInsert; virtual;
    procedure InternalRefresh; virtual;
    procedure OpenCursor(InfoQuery: Boolean); virtual;
    procedure OpenCursorcomplete; virtual;
    procedure RefreshInternalCalcFields(Var Buffer{%H-}: TDataRecord); virtual;
    procedure RestoreState(const Value: TDataSetState);
    Procedure SetActive (Value : Boolean); virtual;
    procedure SetBookmarkStr(const Value: TBookmarkStr); virtual;
    procedure SetBufListSize(Value: Longint); virtual;
    procedure SetChildOrder(Child: TComponent; Order: Longint); override;
    procedure SetCurrentRecord(Index: Longint); virtual;
    procedure SetDefaultFields(const Value: Boolean);
    procedure SetFiltered(Value: Boolean); virtual;
    procedure SetFilterOptions(Value: TFilterOptions); virtual;
    procedure SetFilterText(const Value: string); virtual;
    procedure SetFieldValues(const FieldName: string; Value: JSValue); virtual;
    procedure SetFound(const Value: Boolean); virtual;
    procedure SetModified(Value: Boolean);
    procedure SetName(const NewName: TComponentName); override;
    procedure SetOnFilterRecord(const Value: TFilterRecordEvent); virtual;
    procedure SetRecNo(Value{%H-}: Longint); virtual;
    procedure SetState(Value: TDataSetState);
    function SetTempState(const Value: TDataSetState): TDataSetState;
    Function TempBuffer: TDataRecord;
    procedure UpdateIndexDefs; virtual;
    property ActiveRecord: Longint read FActiveRecord;
    property CurrentRecord: Longint read FCurrentRecord;
    property BlobFieldCount: Longint read FBlobFieldCount;
    property Buffers[Index: Longint]: TDataRecord read GetBuffer;
    property BufferCount: Longint read GetBufferCount;
    property CalcBuffer: TDataRecord read FCalcBuffer;
    property CalcFieldsCount: Longint read FCalcFieldsCount;
    property InternalCalcFields: Boolean read FInternalCalcFields;
    property Constraints: TCheckConstraints read FConstraints write SetConstraints;
    function AllocRecordBuffer: TDataRecord; virtual;
    procedure FreeRecordBuffer(var Buffer{%H-}: TDataRecord); virtual;
    procedure GetBookmarkData(Buffer{%H-}: TDataRecord; var Data{%H-}: TBookmark); virtual;
    function GetBookmarkFlag(Buffer{%H-}: TDataRecord): TBookmarkFlag; virtual;
    function GetDataSource: TDataSource; virtual;
    function GetRecordSize: Word; virtual;
    procedure InternalAddRecord(Buffer{%H-}: Pointer; AAppend{%H-}: Boolean); virtual;
    procedure InternalDelete; virtual;
    procedure InternalFirst; virtual;
    procedure InternalGotoBookmark(ABookmark{%H-}: TBookmark); virtual;
    procedure InternalHandleException(E: Exception); virtual;
    procedure InternalInitRecord(var Buffer{%H-}: TDataRecord); virtual;
    procedure InternalLast; virtual;
    procedure InternalPost; virtual;
    procedure InternalSetToRecord(Buffer{%H-}: TDataRecord); virtual;
    procedure SetBookmarkFlag(Var Buffer{%H-}: TDataRecord; Value{%H-}: TBookmarkFlag); virtual;
    procedure SetBookmarkData(Var Buffer{%H-}: TDataRecord; Data{%H-}: TBookmark); virtual;
    procedure SetUniDirectional(const Value: Boolean);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // These use the active buffer
    function GetFieldData(Field: TField): JSValue;  virtual; overload;
    procedure SetFieldData(Field: TField; AValue : JSValue);  virtual; overload;
    function GetFieldData(Field: TField; Buffer: TDatarecord): JSValue;  virtual; overload;
    procedure SetFieldData(Field: TField; var Buffer: TDatarecord; AValue : JSValue);  virtual; overload;
    class function FieldDefsClass : TFieldDefsClass; virtual;
    class function FieldsClass : TFieldsClass; virtual;
  protected { abstract methods }
    function GetRecord(var Buffer: TDataRecord; GetMode: TGetMode; DoCheck: Boolean): TGetResult; virtual; abstract;
    procedure InternalClose; virtual; abstract;
    procedure InternalOpen; virtual; abstract;
    procedure InternalInitFieldDefs; virtual; abstract;
    function IsCursorOpen: Boolean; virtual; abstract;
    property DataProxy : TDataProxy Read GetDataProxy Write SetDataProxy;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ActiveBuffer: TDataRecord;
    procedure Append;
    procedure AppendRecord(const Values: array of jsValue);
    function BookmarkValid(ABookmark{%H-}: TBookmark): Boolean; virtual;
    function ConvertToDateTime(aValue : JSValue; ARaiseException : Boolean) : TDateTime; virtual;
    function ConvertDateTimeToNative(aValue : TDateTime) : JSValue; virtual;
    Class function DefaultConvertToDateTime(aValue : JSValue; ARaiseException{%H-} : Boolean) : TDateTime; virtual;
    Class function DefaultConvertDateTimeToNative(aValue : TDateTime) : JSValue; virtual;
    Function BlobDataToBytes(aValue : JSValue) : TBytes; virtual;
    Class Function DefaultBlobDataToBytes(aValue : JSValue) : TBytes; virtual;
    Function BytesToBlobData(aValue : TBytes) : JSValue ; virtual;
    Class Function DefaultBytesToBlobData(aValue : TBytes) : JSValue; virtual;
    procedure Cancel; virtual;
    procedure CheckBrowseMode;
    procedure ClearFields;
    procedure Close;
    Procedure ApplyUpdates;
    function  ControlsDisabled: Boolean;
    function CompareBookmarks(Bookmark1{%H-}, Bookmark2{%H-}: TBookmark): Longint; virtual;
    procedure CursorPosChanged;
    procedure Delete; virtual;
    procedure DisableControls;
    procedure Edit;
    procedure EnableControls;
    function FieldByName(const FieldName: string): TField;
    function FindField(const FieldName: string): TField;
    function FindFirst: Boolean; virtual;
    function FindLast: Boolean; virtual;
    function FindNext: Boolean; virtual;
    function FindPrior: Boolean; virtual;
    procedure First;
    procedure FreeBookmark(ABookmark{%H-}: TBookmark); virtual;
    function GetBookmark: TBookmark; virtual;
    function GetCurrentRecord(Buffer{%H-}: TDataRecord): Boolean; virtual;
    procedure GetFieldList(List: TList; const FieldNames: string); overload;
    procedure GetFieldList(List: TFPList; const FieldNames: string); overload;
    procedure GetFieldNames(List: TStrings);
    procedure GotoBookmark(const ABookmark: TBookmark);
    procedure Insert; reintroduce;
    procedure InsertRecord(const Values: array of JSValue);
    function IsEmpty: Boolean;
    function IsLinkedTo(ADataSource: TDataSource): Boolean;
    function IsSequenced: Boolean; virtual;
    procedure Last;
    Function Load(aOptions : TLoadOptions; aAfterLoad : TDatasetLoadEvent) : Boolean;
    function Locate(const KeyFields{%H-}: string; const KeyValues{%H-}: JSValue; Options{%H-}: TLocateOptions) : boolean; virtual;
    function Lookup(const KeyFields{%H-}: string; const KeyValues{%H-}: JSValue; const ResultFields{%H-}: string): JSValue; virtual;
    function MoveBy(Distance: Longint): Longint;
    procedure Next;
    procedure Open;
    procedure Post; virtual;
    procedure Prior;
    procedure Refresh;
    procedure Resync(Mode: TResyncMode); virtual;
    procedure SetFields(const Values: array of JSValue);
    procedure UpdateCursorPos;
    procedure UpdateRecord;
    Function GetPendingUpdates : TResolveInfoArray;
    function UpdateStatus: TUpdateStatus; virtual;
    property BlockReadSize: Integer read FBlockReadSize write SetBlockReadSize;
    property BOF: Boolean read FBOF;
    property Bookmark: TBookmark read GetBookmark write GotoBookmark;
    property CanModify: Boolean read GetCanModify;
    property DataSource: TDataSource read GetDataSource;
    property DefaultFields: Boolean read FDefaultFields;
    property EOF: Boolean read FEOF;
    property FieldCount: Longint read GetFieldCount;
    property FieldDefs: TFieldDefs read FFieldDefs write SetFieldDefs;
    property Found: Boolean read FFound;
    property Modified: Boolean read FModified;
    property IsUniDirectional: Boolean read FIsUniDirectional default False;
    property RecordCount: Longint read GetRecordCount;
    property RecNo: Longint read GetRecNo write SetRecNo;
    property RecordSize: Word read GetRecordSize;
    property State: TDataSetState read FState;
    property Fields : TFields read FFieldList;
    property FieldValues[FieldName : string] : JSValue read GetFieldValues write SetFieldValues; default;
    property Filter: string read FFilterText write SetFilterText;
    property Filtered: Boolean read FFiltered write SetFiltered default False;
    property FilterOptions: TFilterOptions read FFilterOptions write SetFilterOptions;
    property Active: Boolean read GetActive write SetActive default False;
    property AutoCalcFields: Boolean read FAutoCalcFields write FAutoCalcFields default true;
    property BeforeOpen: TDataSetNotifyEvent read FBeforeOpen write FBeforeOpen;
    property AfterOpen: TDataSetNotifyEvent read FAfterOpen write FAfterOpen;
    property BeforeClose: TDataSetNotifyEvent read FBeforeClose write FBeforeClose;
    property AfterClose: TDataSetNotifyEvent read FAfterClose write FAfterClose;
    property BeforeInsert: TDataSetNotifyEvent read FBeforeInsert write FBeforeInsert;
    property AfterInsert: TDataSetNotifyEvent read FAfterInsert write FAfterInsert;
    property BeforeEdit: TDataSetNotifyEvent read FBeforeEdit write FBeforeEdit;
    property AfterEdit: TDataSetNotifyEvent read FAfterEdit write FAfterEdit;
    property BeforePost: TDataSetNotifyEvent read FBeforePost write FBeforePost;
    property AfterPost: TDataSetNotifyEvent read FAfterPost write FAfterPost;
    property BeforeCancel: TDataSetNotifyEvent read FBeforeCancel write FBeforeCancel;
    property AfterCancel: TDataSetNotifyEvent read FAfterCancel write FAfterCancel;
    property BeforeDelete: TDataSetNotifyEvent read FBeforeDelete write FBeforeDelete;
    property AfterDelete: TDataSetNotifyEvent read FAfterDelete write FAfterDelete;
    property BeforeScroll: TDataSetNotifyEvent read FBeforeScroll write FBeforeScroll;
    property AfterScroll: TDataSetNotifyEvent read FAfterScroll write FAfterScroll;
    property BeforeRefresh: TDataSetNotifyEvent read FBeforeRefresh write FBeforeRefresh;
    property BeforeLoad : TDatasetNotifyEvent Read FBeforeLoad Write FBeforeLoad;
    Property AfterLoad : TDatasetNotifyEvent Read FAfterLoad Write FAfterLoad;
    Property BeforeApplyUpdates : TDatasetNotifyEvent Read FBeforeApplyUpdates Write FBeforeApplyUpdates;
    Property AfterApplyUpdates : TApplyUpdatesEvent Read FAfterApplyUpdates Write FAfterApplyUpdates;
    property AfterRefresh: TDataSetNotifyEvent read FAfterRefresh write FAfterRefresh;
    property OnCalcFields: TDataSetNotifyEvent read FOnCalcFields write FOnCalcFields;
    property OnDeleteError: TDataSetErrorEvent read FOnDeleteError write FOnDeleteError;
    property OnEditError: TDataSetErrorEvent read FOnEditError write FOnEditError;
    property OnFilterRecord: TFilterRecordEvent read FOnFilterRecord write SetOnFilterRecord;
    property OnNewRecord: TDataSetNotifyEvent read FOnNewRecord write FOnNewRecord;
    Property OnRecordResolved : TOnRecordResolveEvent Read FOnRecordResolved Write FOnRecordResolved;
    property OnPostError: TDataSetErrorEvent read FOnPostError write FOnPostError;
    property OnLoadFail : TDatasetLoadFailEvent Read FOnLoadFail Write FOnLoadFail;
  end;

{ TDataLink }

  TDataLink = class(TPersistent)
  private
    FFirstRecord,
    FBufferCount : Integer;
    FActive,
    FDataSourceFixed,
    FEditing,
    FReadOnly,
    FUpdatingRecord,
    FVisualControl : Boolean;
    FDataSource : TDataSource;
    Function  CalcFirstRecord(Index : Integer) : Integer;
    Procedure CalcRange;
    Procedure CheckActiveAndEditing;
    Function  GetDataset : TDataset;
    procedure SetActive(AActive: Boolean);
    procedure SetDataSource(Value: TDataSource);
    Procedure SetReadOnly(Value : Boolean);
  protected
    procedure ActiveChanged; virtual;
    procedure CheckBrowseMode; virtual;
    procedure DataEvent(Event: TDataEvent; Info: JSValue); virtual;
    procedure DataSetChanged; virtual;
    procedure DataSetScrolled(Distance{%H-}: Integer); virtual;
    procedure EditingChanged; virtual;
    procedure FocusControl(Field{%H-}: JSValue); virtual;
    function  GetActiveRecord: Integer; virtual;
    function  GetBOF: Boolean; virtual;
    function  GetBufferCount: Integer; virtual;
    function  GetEOF: Boolean; virtual;
    function  GetRecordCount: Integer; virtual;
    procedure LayoutChanged; virtual;
    function  MoveBy(Distance: Integer): Integer; virtual;
    procedure RecordChanged(Field{%H-}: TField); virtual;
    procedure SetActiveRecord(Value: Integer); virtual;
    procedure SetBufferCount(Value: Integer); virtual;
    procedure UpdateData; virtual;
    property VisualControl: Boolean read FVisualControl write FVisualControl;
    property FirstRecord: Integer read FFirstRecord write FFirstRecord;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function  Edit: Boolean;
    procedure UpdateRecord;
    property Active: Boolean read FActive;
    property ActiveRecord: Integer read GetActiveRecord write SetActiveRecord;
    property BOF: Boolean read GetBOF;
    property BufferCount: Integer read GetBufferCount write SetBufferCount;
    property DataSet: TDataSet read GetDataSet;
    property DataSource: TDataSource read FDataSource write SetDataSource;
    property DataSourceFixed: Boolean read FDataSourceFixed write FDataSourceFixed;
    property Editing: Boolean read FEditing;
    property Eof: Boolean read GetEOF;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property RecordCount: Integer read GetRecordCount;
  end;

{ TDetailDataLink }

  TDetailDataLink = class(TDataLink)
  protected
    function GetDetailDataSet: TDataSet; virtual;
  public
    property DetailDataSet: TDataSet read GetDetailDataSet;
  end;

{ TMasterDataLink }

  TMasterDataLink = class(TDetailDataLink)
  private
    FDetailDataSet: TDataSet;
    FFieldNames: string;
    FFields: TList;
    FOnMasterChange: TNotifyEvent;
    FOnMasterDisable: TNotifyEvent;
    procedure SetFieldNames(const Value: string);
  protected
    procedure ActiveChanged; override;
    procedure CheckBrowseMode; override;
    function GetDetailDataSet: TDataSet; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
    Procedure DoMasterDisable; virtual;
    Procedure DoMasterChange; virtual;
  public
    constructor Create(ADataSet: TDataSet);virtual; reintroduce;
    destructor Destroy; override;
    property FieldNames: string read FFieldNames write SetFieldNames;
    property Fields: TList read FFields;
    property OnMasterChange: TNotifyEvent read FOnMasterChange write FOnMasterChange;
    property OnMasterDisable: TNotifyEvent read FOnMasterDisable write FOnMasterDisable;
  end;

{ TMasterParamsDataLink }

  TMasterParamsDataLink = Class(TMasterDataLink)
  Private
    FParams : TParams;
    Procedure SetParams(AValue : TParams);
  Protected
    Procedure DoMasterDisable; override;
    Procedure DoMasterChange; override;
  Public
    constructor Create(ADataSet: TDataSet); override;
    Procedure RefreshParamNames; virtual;
    Procedure CopyParamsFromMaster(CopyBound : Boolean); virtual;
    Property Params : TParams Read FParams Write SetParams;
  end;

{ TDataSource }

  TDataChangeEvent = procedure(Sender: TObject; Field: TField) of object;

  TDataSource = class(TComponent)
  private
    FDataSet: TDataSet;
    FDataLinks: TList;
    FEnabled: Boolean;
    FAutoEdit: Boolean;
    FState: TDataSetState;
    FOnStateChange: TNotifyEvent;
    FOnDataChange: TDataChangeEvent;
    FOnUpdateData: TNotifyEvent;
    procedure DistributeEvent(Event: TDataEvent; Info: JSValue);
    procedure RegisterDataLink(DataLink: TDataLink);
    Procedure ProcessEvent(Event : TDataEvent; Info : JSValue);
    procedure SetDataSet(ADataSet: TDataSet);
    procedure SetEnabled(Value: Boolean);
    procedure UnregisterDataLink(DataLink: TDataLink);
  protected
    Procedure DoDataChange (Info : Pointer);virtual;
    Procedure DoStateChange; virtual;
    Procedure DoUpdateData;
    property DataLinks: TList read FDataLinks;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Edit;
    function IsLinkedTo(ADataSet{%H-}: TDataSet): Boolean;
    property State: TDataSetState read FState;
  published
    property AutoEdit: Boolean read FAutoEdit write FAutoEdit default True;
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property OnStateChange: TNotifyEvent read FOnStateChange write FOnStateChange;
    property OnDataChange: TDataChangeEvent read FOnDataChange write FOnDataChange;
    property OnUpdateData: TNotifyEvent read FOnUpdateData write FOnUpdateData;
  end;


  { TDataRequest }
  TDataRequestResult = (rrFail,rrEOF,rrOK);
  TDataRequestEvent = Procedure (ARequest : TDataRequest) of object;

  TDataRequest = Class(TObject)
  private
    FBookmark: TBookMark;
    FCurrent: TBookMark;
    FDataset: TDataset;
    FErrorMsg: String;
    FEvent: TDatasetLoadEvent;
    FLoadOptions: TLoadOptions;
    FRequestID: Integer;
    FSuccess: TDataRequestResult;
    FData : JSValue;
    FAfterRequest : TDataRequestEvent;
    FDataProxy : TDataProxy;
  Protected
    Procedure DoAfterRequest;
  Public
    Constructor Create(aDataProxy : TDataProxy; aOptions: TLoadOptions; aAfterRequest: TDataRequestEvent; aAfterLoad: TDatasetLoadEvent); virtual; reintroduce;
    property DataProxy : TDataProxy Read FDataProxy;
    Property Dataset : TDataset Read FDataset;
    Property Bookmark : TBookMark Read FBookmark;
    Property RequestID : Integer Read FRequestID;
    Property LoadOptions : TLoadOptions Read FLoadOptions;
    Property Current : TBookMark Read FCurrent;
    Property Success : TDataRequestResult Read FSuccess Write FSuccess;
    Property Event : TDatasetLoadEvent Read FEvent;
    Property ErrorMsg : String Read FErrorMsg Write FErrorMsg;
    Property Data : JSValue read FData Write FData;
  end;
  TDataRequestClass = Class of TDataRequest;

  { TRecordUpdateDescriptor }

  TRecordUpdateDescriptor = Class(TObject)
  private
    FBookmark: TBookmark;
    FData: JSValue;
    FDataset: TDataset;
    FProxy: TDataProxy;
    FResolveError: String;
    FServerData: JSValue;
    FStatus: TUpdateStatus;
    FOriginalStatus : TUpdateStatus;
  Protected
    Procedure SetStatus(aValue : TUpdateStatus); virtual;
    Procedure Reset;
  Public
    Constructor Create(aProxy : TDataProxy; aDataset : TDataset; aBookmark : TBookMark; AData : JSValue; AStatus : TUpdateStatus); reintroduce;
    Procedure Resolve(aData : JSValue);
    Procedure ResolveFailed(aError : String);
    Property Proxy : TDataProxy read FProxy;
    Property Dataset : TDataset Read FDataset;
    Property OriginalStatus : TUpdateStatus Read FOriginalStatus;
    Property Status : TUpdateStatus Read FStatus;
    Property ServerData : JSValue Read FServerData;
    Property Data : JSValue Read FData;
    Property Bookmark : TBookmark Read FBookmark;
    Property ResolveError : String Read FResolveError ;
  end;
  TRecordUpdateDescriptorClass = Class of TRecordUpdateDescriptor;

  { TRecordUpdateDescriptorList }

  TRecordUpdateDescriptorList = Class(TFPList)
  private
    function GetUpdate(AIndex : Integer): TRecordUpdateDescriptor;
  Public
    Property UpdateDescriptors[AIndex : Integer] : TRecordUpdateDescriptor Read GetUpdate; Default;
  end;

  { TRecordUpdateBatch }
  TUpdateBatchStatus = (ubsPending,ubsProcessing,ubsProcessed,ubsResolved);
  TResolveBatchEvent = Procedure (Sender : TObject; ARequest : TRecordUpdateBatch) of object;

  TRecordUpdateBatch = class(TObject)
  private
    FBatchID: Integer;
    FDataset: TDataset;
    FLastChangeIndex: Integer;
    FList: TRecordUpdateDescriptorList;
    FOnResolve: TResolveBatchEvent;
    FOwnsList: Boolean;
    FStatus: TUpdateBatchStatus;
  Protected
    Property LastChangeIndex : Integer Read FLastChangeIndex;
  Public
    Constructor Create (aBatchID : Integer; AList : TRecordUpdateDescriptorList; AOwnsList : Boolean); reintroduce;
    Destructor Destroy; override;
    Procedure FreeList;
    Property Dataset : TDataset Read FDataset Write FDataset;
    Property OnResolve : TResolveBatchEvent Read FOnResolve Write FOnResolve;
    Property OwnsList : Boolean Read FOwnsList;
    property BatchID : Integer Read FBatchID;
    Property Status : TUpdateBatchStatus Read FStatus Write FStatus;
    Property List : TRecordUpdateDescriptorList Read FList;
  end;
  TRecordUpdateBatchClass = Class of TRecordUpdateBatch;

  { TDataProxy }

  TDataProxy = Class(TComponent)
  Protected
    Function GetDataRequestClass : TDataRequestClass; virtual;
    Function GetUpdateDescriptorClass : TRecordUpdateDescriptorClass; virtual;
    Function GetUpdateBatchClass : TRecordUpdateBatchClass; virtual;
    // Use this to call resolve event, and free the batch.
    Procedure ResolveBatch(aBatch : TRecordUpdateBatch);
  Public
    Function GetDataRequest(aOptions: TLoadOptions; aAfterRequest: TDataRequestEvent; aAfterLoad: TDatasetLoadEvent) : TDataRequest; virtual;
    Function GetUpdateDescriptor(aDataset : TDataset; aBookmark : TBookMark; AData : JSValue; AStatus : TUpdateStatus) : TRecordUpdateDescriptor; virtual;
    function GetRecordUpdateBatch(aBatchID: Integer; AList: TRecordUpdateDescriptorList; AOwnsList: Boolean=True): TRecordUpdateBatch; virtual;
    // actual calls to do the work. Dataset wi
    Function DoGetData(aRequest : TDataRequest) : Boolean; virtual; abstract;
    // TDataProxy is responsible for calling OnResolve and if not, Freeing the batch.
    Function ProcessUpdateBatch(aBatch : TRecordUpdateBatch): Boolean; virtual; abstract;
  end;

const
  {
  TFieldType = (
    ftUnknown, ftString, ftInteger, ftLargeInt, ftBoolean, ftFloat, ftDate,
    ftTime, ftDateTime,  ftAutoInc, ftBlob, ftMemo, ftFixedChar,
    ftVariant
  );
  }

Const
  Fieldtypenames : Array [TFieldType] of String =
    (
      {ftUnknown} 'Unknown',
      {ftString} 'String',
      {ftInteger} 'Integer',
      {ftLargeint} 'NativeInt',
      {ftBoolean} 'Boolean',
      {ftFloat} 'Float',
      {ftDate} 'Date',
      {ftTime} 'Time',
      {ftDateTime} 'DateTime',
      {ftAutoInc} 'AutoInc',
      {ftBlob} 'Blob',
      {ftMemo} 'Memo',
      {ftFixedChar} 'FixedChar',
      {ftVariant} 'Variant',
      {ftDataset} 'Dataset'
    );

  DefaultFieldClasses : Array [TFieldType] of TFieldClass =
    (
      { ftUnknown} Tfield,
      { ftString} TStringField,
      { ftInteger} TIntegerField,
      { ftLargeint} TLargeIntField,
      { ftBoolean} TBooleanField,
      { ftFloat} TFloatField,
      { ftDate} TDateField,
      { ftTime} TTimeField,
      { ftDateTime} TDateTimeField,
      { ftAutoInc} TAutoIncField,
      { ftBlob} TBlobField,
      { ftMemo} TMemoField,
      { ftFixedChar} TStringField,
      { ftVariant} TVariantField,
      { ftDataset} Nil
    );

  dsEditModes = [dsEdit, dsInsert, dsSetKey];
  dsWriteModes = [dsEdit, dsInsert, dsSetKey, dsCalcFields, dsFilter,
                  dsNewValue, dsInternalCalc, dsRefreshFields];
  // Correct list of all field types that are BLOB types.
  // Please use this instead of checking TBlobType which will give
  // incorrect results
  ftBlobTypes = [ftBlob, ftMemo];


{ Auxiliary functions }

Procedure DatabaseError (Const Msg : String); overload;
Procedure DatabaseError (Const Msg : String; Comp : TComponent); overload;
Procedure DatabaseErrorFmt (Const Fmt : String; Const Args : Array Of JSValue); overload;
Procedure DatabaseErrorFmt (Const Fmt : String; Const Args : Array Of JSValue; Comp : TComponent); overload;
Function ExtractFieldName(Const Fields: String; var Pos: Integer): String;

// function SkipComments(var p: PChar; EscapeSlash, EscapeRepeat : Boolean) : boolean;

// operator Enumerator(ADataSet: TDataSet): TDataSetEnumerator;
 
implementation

uses DBConst,TypInfo;

{ ---------------------------------------------------------------------
    Auxiliary functions
  ---------------------------------------------------------------------}

Procedure DatabaseError (Const Msg : String);

begin
  Raise EDataBaseError.Create(Msg);
end;

Procedure DatabaseError (Const Msg : String; Comp : TComponent);

begin
  if assigned(Comp) and (Comp.Name <> '') then
    Raise EDatabaseError.CreateFmt('%s : %s',[Comp.Name,Msg])
  else
    DatabaseError(Msg);
end;

Procedure DatabaseErrorFmt (Const Fmt : String; Const Args : Array Of JSValue);

begin
  Raise EDatabaseError.CreateFmt(Fmt,Args);
end;

Procedure DatabaseErrorFmt (Const Fmt : String; Const Args : Array Of JSValue;
                            Comp : TComponent);
begin
  if assigned(comp) then
    Raise EDatabaseError.CreateFmt(Format('%s : %s',[Comp.Name,Fmt]),Args)
  else
    DatabaseErrorFmt(Fmt, Args);
end;

function ExtractFieldName(const Fields: string; var Pos: Integer): string;
var
  i: Integer;
  FieldsLength: Integer;
begin
  i:=Pos;
  FieldsLength:=Length(Fields);
  while (i<=FieldsLength) and (Fields[i]<>';') do Inc(i);
  Result:=Trim(Copy(Fields,Pos,i-Pos));
  if (i<=FieldsLength) and (Fields[i]=';') then Inc(i);
  Pos:=i;
end;

{ TRecordUpdateBatch }

constructor TRecordUpdateBatch.Create(aBatchID: Integer; AList: TRecordUpdateDescriptorList; AOwnsList : Boolean);
begin
  FBatchID:=aBatchID;
  FList:=AList;
  FOwnsList:=AOwnsList;
  FStatus:=ubsPending;
end;

destructor TRecordUpdateBatch.Destroy;
begin
  if OwnsList then
    FreeList;
  inherited Destroy;
end;

procedure TRecordUpdateBatch.FreeList;
begin
  FreeAndNil(FList);
end;

{ TRecordUpdateDescriptorList }

function TRecordUpdateDescriptorList.GetUpdate(AIndex : Integer): TRecordUpdateDescriptor;
begin
  Result:=TRecordUpdateDescriptor(Items[AIndex]);
end;

{ TRecordUpdateDescriptor }

procedure TRecordUpdateDescriptor.SetStatus(aValue: TUpdateStatus);
begin
  FStatus:=AValue;
end;

procedure TRecordUpdateDescriptor.Reset;
begin
  FStatus:=FOriginalStatus;
  FResolveError:='';
  FServerData:=Null;
end;

constructor TRecordUpdateDescriptor.Create(aProxy: TDataProxy; aDataset: TDataset; aBookmark: TBookMark; AData: JSValue;
  AStatus: TUpdateStatus);
begin
  FDataset:=aDataset;
  FBookmark:=aBookmark;
  FData:=AData;
  FStatus:=AStatus;
  FOriginalStatus:=AStatus;
  FProxy:=aProxy;
end;


procedure TRecordUpdateDescriptor.Resolve(aData: JSValue);
begin
  FStatus:=usResolved;
  FServerData:=AData;
end;

procedure TRecordUpdateDescriptor.ResolveFailed(aError: String);
begin
  SetStatus(usResolveFailed);
  FResolveError:=AError;
end;

{ TDataRequest }

procedure TDataRequest.DoAfterRequest;
begin
  if Assigned(FAfterRequest) then
    FAfterRequest(Self);
end;

constructor TDataRequest.Create(aDataProxy : TDataProxy; aOptions: TLoadOptions; aAfterRequest: TDataRequestEvent; aAfterLoad: TDatasetLoadEvent);
begin
  FDataProxy:=aDataProxy;
  FLoadOptions:=aOptions;
  FEvent:=aAfterLoad;
  FAfterRequest:=aAfterRequest;
end;

{ TDataProxy }

function TDataProxy.GetDataRequestClass: TDataRequestClass;
begin
  Result:=TDataRequest;
end;

function TDataProxy.GetUpdateDescriptorClass: TRecordUpdateDescriptorClass;
begin
  Result:=TRecordUpdateDescriptor;
end;

function TDataProxy.GetUpdateBatchClass: TRecordUpdateBatchClass;
begin
  Result:=TRecordUpdateBatch;
end;

procedure TDataProxy.ResolveBatch(aBatch: TRecordUpdateBatch);
begin
  try
    If Assigned(ABatch.FOnResolve) then
      ABatch.FOnResolve(Self,ABatch);
  finally
    aBatch.Free;
  end;
end;

function TDataProxy.GetDataRequest(aOptions: TLoadOptions; aAfterRequest : TDataRequestEvent; aAfterLoad: TDatasetLoadEvent): TDataRequest;
begin
  Result:=GetDataRequestClass.Create(Self,aOptions,aAfterRequest,aAfterLoad);
end;

function TDataProxy.GetUpdateDescriptor(aDataset : TDataset; aBookmark: TBookMark; AData: JSValue; AStatus: TUpdateStatus): TRecordUpdateDescriptor;
begin
  Result:=GetUpdateDescriptorClass.Create(Self,aDataset, aBookmark,AData,AStatus);
end;

function TDataProxy.GetRecordUpdateBatch(aBatchID: Integer; AList: TRecordUpdateDescriptorList; AOwnsList : Boolean = True): TRecordUpdateBatch;
begin
  Result:=GetUpdateBatchClass.Create(aBatchID,AList,AOwnsList);
end;


{ EUpdateError }
constructor EUpdateError.Create(NativeError, Context : String;
                                ErrCode, PrevError : integer; E: Exception);
                                
begin
  Inherited CreateFmt(NativeError,[Context]);
  FContext := Context;
  FErrorCode := ErrCode;
  FPreviousError := PrevError;
  FOriginalException := E;
end;

Destructor EUpdateError.Destroy;

begin
  FOriginalException.Free;
  Inherited;
end;

{ TNamedItem }

function TNamedItem.GetDisplayName: string;
begin
  Result := FName;
end;

procedure TNamedItem.SetDisplayName(const Value: string);
Var TmpInd : Integer;
begin
  if FName=Value then exit;
  if (Value <> '') and (Collection is TFieldDefs ) then
    begin
    TmpInd :=  (TDefCollection(Collection).IndexOf(Value));
    if (TmpInd >= 0) and (TmpInd <> Index) then
      DatabaseErrorFmt(SDuplicateName, [Value, Collection.ClassName]);
    end;
  FName:=Value;
  inherited SetDisplayName(Value);
end;

{ TDefCollection }

procedure TDefCollection.SetItemName(Item: TCollectionItem);

Var
  N : TNamedItem;
  TN : String;

begin
  N:=Item as TNamedItem;
  if N.Name = '' then
    begin
    TN:=Copy(ClassName, 2, 5) + IntToStr(N.ID+1);
    if assigned(Dataset) then
      TN:=Dataset.Name+TN;
    N.Name:=TN;
    end
  else
    inherited SetItemName(Item);
end;

constructor TDefCollection.create(ADataset: TDataset; AOwner: TPersistent;
  AClass: TCollectionItemClass);
begin
  inherited Create(AOwner,AClass);
  FDataset := ADataset;
end;

function TDefCollection.Find(const AName: string): TNamedItem;
var i: integer;
begin
  Result := Nil;
  for i := 0 to Count - 1 do
    if AnsiSameText(TNamedItem(Items[i]).Name, AName) then
    begin
    Result := TNamedItem(Items[i]);
    Break;
    end;
end;

procedure TDefCollection.GetItemNames(List: TStrings);
var i: LongInt;
begin
  for i := 0 to Count - 1 do
    List.Add(TNamedItem(Items[i]).Name);
end;

function TDefCollection.IndexOf(const AName: string): Longint;
var i: LongInt;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if AnsiSameText(TNamedItem(Items[i]).Name, AName) then
    begin
    Result := i;
    Break;
    end;
end;

{ TIndexDef }

procedure TIndexDef.SetDescFields(const AValue: string);
begin
  if FDescFields=AValue then exit;
  if AValue <> '' then FOptions:=FOptions + [ixDescending];
  FDescFields:=AValue;
end;

procedure TIndexDef.Assign(Source: TPersistent);
var idef : TIndexDef;
begin
  idef := nil;
  if Source is TIndexDef then
    idef := Source as TIndexDef;
  if Assigned(idef) then
     begin
     FName := idef.Name;
     FFields := idef.Fields;
     FOptions := idef.Options;
     FCaseinsFields := idef.CaseInsFields;
     FDescFields := idef.DescFields;
     FSource := idef.Source;
     FExpression := idef.Expression;
     end
  else
    inherited Assign(Source);
end;

function TIndexDef.GetExpression: string;
begin
  Result := FExpression;
end;

procedure TIndexDef.SetExpression(const AValue: string);
begin
  FExpression := AValue;
end;

procedure TIndexDef.SetCaseInsFields(const AValue: string);
begin
  if FCaseinsFields=AValue then exit;
  if AValue <> '' then FOptions:=FOptions + [ixCaseInsensitive];
  FCaseinsFields:=AValue;
end;

constructor TIndexDef.Create(Owner: TIndexDefs; const AName, TheFields: string;
      TheOptions: TIndexOptions);

begin
  FName := aname;
  inherited create(Owner);
  FFields := TheFields;
  FOptions := TheOptions;
end;


{ TIndexDefs }

Function TIndexDefs.GetItem (Index : integer) : TIndexDef;

begin
  Result:=(Inherited GetItem(Index)) as TIndexDef;
end;

Procedure TIndexDefs.SetItem(Index: Integer; Value: TIndexDef);
begin
  Inherited SetItem(Index,Value);
end;

constructor TIndexDefs.Create(ADataSet: TDataSet);

begin
  inherited create(ADataset, Owner, TIndexDef);
end;


Function TIndexDefs.AddIndexDef: TIndexDef;

begin
//  Result := inherited add as TIndexDef;
  Result:=TIndexDefClass(Self.ItemClass).Create(Self,'','',[]);
end;

procedure TIndexDefs.Add(const Name, Fields: string; Options: TIndexOptions);

begin
  TIndexDefClass(Self.ItemClass).Create(Self,Name,Fields,Options);
end;

function TIndexDefs.Find(const IndexName: string): TIndexDef;
begin
  Result := (inherited Find(IndexName)) as TIndexDef;
  if (Result=Nil) Then
    DatabaseErrorFmt(SIndexNotFound, [IndexName], FDataSet);
end;

function TIndexDefs.FindIndexForFields(const Fields: string): TIndexDef;

begin
  //!! To be implemented
  Result:=nil;
end;


function TIndexDefs.GetIndexForFields(const Fields: string;
  CaseInsensitive: Boolean): TIndexDef;

var
  i, FieldsLen: integer;
  Last: TIndexDef;
begin
  Last := nil;
  FieldsLen := Length(Fields);
  for i := 0 to Count - 1 do
  begin
    Result := Items[I];
    if (Result.Options * [ixDescending, ixExpression] = []) and
       (not CaseInsensitive or (ixCaseInsensitive in Result.Options)) and
       AnsiSameText(Fields, Result.Fields) then
    begin
      Exit;
    end else
    if AnsiSameText(Fields, Copy(Result.Fields, 1, FieldsLen)) and
       ((Length(Result.Fields) = FieldsLen) or
       (Result.Fields[FieldsLen + 1] = ';')) then
    begin
      if (Last = nil) or
         ((Last <> nil) And (Length(Last.Fields) > Length(Result.Fields))) then
           Last := Result;
    end;
  end;
  Result := Last;
end;

procedure TIndexDefs.Update;

begin
  if (not updated) and assigned(Dataset) then
    begin
    Dataset.UpdateIndexDefs;
    updated := True;
    end;
end;

{ TCheckConstraint }

procedure TCheckConstraint.Assign(Source: TPersistent);

begin
  //!! To be implemented
end;



{ TCheckConstraints }

Function TCheckConstraints.GetItem(Index : Longint) : TCheckConstraint;

begin
  //!! To be implemented
  Result := nil;
end;


Procedure TCheckConstraints.SetItem(index : Longint; Value : TCheckConstraint);

begin
  //!! To be implemented
end;


function TCheckConstraints.GetOwner: TPersistent;

begin
  //!! To be implemented
  Result := nil;
end;


constructor TCheckConstraints.Create(AOwner: TPersistent);

begin
  //!! To be implemented
  inherited Create(TCheckConstraint);
end;


function TCheckConstraints.Add: TCheckConstraint;

begin
  //!! To be implemented
  Result := nil;
end;

{ TLookupList }

constructor TLookupList.Create;

begin
  FList := TFPList.Create;
end;

destructor TLookupList.Destroy;

begin
  Clear;
  FList.Destroy;
  inherited Destroy;
end;

procedure TLookupList.Add(const AKey, AValue: JSValue);

var LookupRec: TJSObject;

begin
  LookupRec:=New(['Key',AKey,'Value',AValue]);
  FList.Add(LookupRec);
end;

procedure TLookupList.Clear;

begin
  FList.Clear;
end;

function TLookupList.FirstKeyByValue(const AValue: JSValue): JSValue;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    with TJSObject(FList[i]) do
      if Properties['Value'] = AValue then
        begin
        Result := Properties['Key'];
        exit;
        end;
  Result := Null;
end;

function TLookupList.ValueOfKey(const AKey: JSValue): JSValue;

  Function VarArraySameValues(VarArray1,VarArray2 : TJSValueDynArray) : Boolean;
  // This only works for one-dimensional vararrays with a lower bound of 0
  // and equal higher bounds wich only contains JSValues.
  // The vararrays returned by GetFieldValues do apply.
  var i : integer;
  begin
    Result := True;
    if (Length(VarArray1)<>Length(VarArray2)) then
      exit;
    for i := 0 to Length(VarArray1) do
      begin
      if VarArray1[i]<>VarArray2[i] then
        begin
        Result := false;
        Exit;
        end;
    end;
  end;

var I: Integer;
begin
  Result := Null;
  if IsNull(AKey) then Exit;
  i := FList.Count - 1;
  if IsArray(AKey) then
    while (i >= 0) And not VarArraySameValues(TJSValueDynArray(TJSOBject(FList.Items[I]).Properties['Key']),TJSValueDynArray(AKey)) do Dec(i)
  else
    while (i >= 0) And (TJSObject(FList[I]).Properties['Key'] <> AKey) do Dec(i);
  if i >= 0 then Result := TJSObject(FList[I]).Properties['Value'];
end;

procedure TLookupList.ValuesToStrings(AStrings: TStrings);

var
  i: Integer;
  p: TJSObject;

begin
  AStrings.Clear;
  for i := 0 to FList.Count - 1 do
    begin
    p := TJSObject(FList[i]);
    AStrings.AddObject(String(p.properties['Value']), TObject(p));
    end;
end;

{ ---------------------------------------------------------------------
    TDataSet
  ---------------------------------------------------------------------}

Const
  DefaultBufferCount = 10;

constructor TDataSet.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  FFieldDefs:=FieldDefsClass.Create(Self);
  FFieldList:=FieldsClass.Create(Self);
  FDataSources:=TFPList.Create;
  FConstraints:=TCheckConstraints.Create(Self);
  SetLength(FBuffers,1);
  FActiveRecord := 0;
  FEOF := True;
  FBOF := True;
  FIsUniDirectional := False;
  FAutoCalcFields := True;
  FDataRequestID:=0;
end;



destructor TDataSet.Destroy;

var
  i: Integer;

begin
  Active:=False;
  FFieldDefs.Free;
  FFieldList.Free;
  With FDataSources do
    begin
    While Count>0 do
      TDataSource(Items[Count - 1]).DataSet:=Nil;
    Destroy;
    end;
  for i := 0 to FBufferCount do
    FreeRecordBuffer(FBuffers[i]);
  FConstraints.Free;
  SetLength(FBuffers,1);
  Inherited Destroy;
end;

// This procedure must be called when the first record is made/read
procedure TDataSet.ActivateBuffers;

begin
  FBOF:=False;
  FEOF:=False;
  FActiveRecord:=0;
end;

procedure TDataSet.BindFields(Binding: Boolean);

var i, FieldIndex: Integer;
    FieldDef: TFieldDef;
    Field: TField;

begin
  { FieldNo is set to -1 for calculated/lookup fields, to 0 for unbound field
    and for bound fields it is set to FieldDef.FieldNo }
  FCalcFieldsCount := 0;
  FBlobFieldCount := 0;
  for i := 0 to Fields.Count - 1 do
    begin
    Field := Fields[i];
    Field.FFieldDef := Nil;
    if not Binding then
      Field.FFieldNo := 0
    else if Field.FieldKind in [fkCalculated, fkLookup] then
      begin
      Field.FFieldNo := -1;
      Inc(FCalcFieldsCount);
      end
    else
      begin
      FieldIndex := FieldDefs.IndexOf(Field.FieldName);
      if FieldIndex = -1 then
        DatabaseErrorFmt(SFieldNotFound,[Field.FieldName],Self)
      else
        begin
        FieldDef := FieldDefs[FieldIndex];
        Field.FFieldDef := FieldDef;
        Field.FFieldNo := FieldDef.FieldNo;
        if FieldDef.InternalCalcField then
          FInternalCalcFields := True;
        if Field.IsBlob then
          begin
          Field.FSize := FieldDef.Size;
          Inc(FBlobFieldCount);
          end;
        // synchronize CodePage between TFieldDef and TField
        // character data in record buffer and field buffer should have same CodePage
        end;
      end;
    Field.Bind(Binding);
    end;
end;

function TDataSet.BookmarkAvailable: Boolean;

Const BookmarkStates = [dsBrowse,dsEdit,dsInsert];

begin
  Result:=(Not IsEmpty) and  not FIsUniDirectional and (State in BookmarkStates)
          and (getBookMarkFlag(ActiveBuffer)=bfCurrent);
end;

procedure TDataSet.CalculateFields(var Buffer: TDataRecord);
var
  i: Integer;
  OldState: TDatasetState;
begin
  FCalcBuffer := Buffer; 
  if FState <> dsInternalCalc then
  begin
    OldState := FState;
    FState := dsCalcFields;
    try
      ClearCalcFields(FCalcBuffer);
      if not IsUniDirectional then
        for i := 0 to FFieldList.Count - 1 do
          if FFieldList[i].FieldKind = fkLookup then
            FFieldList[i].CalcLookupValue;
    finally
      DoOnCalcFields;
      FState := OldState;
    end;
  end;
end;

procedure TDataSet.CheckActive;

begin
  If Not Active then
    DataBaseError(SInactiveDataset,Self);
end;

procedure TDataSet.CheckInactive;

begin
  If Active then
    DataBaseError(SActiveDataset,Self);
end;

procedure TDataSet.ClearBuffers;

begin
  FRecordCount:=0;
  FActiveRecord:=0;
  FCurrentRecord:=-1;
  FBOF:=True;
  FEOF:=True;
end;

procedure TDataSet.ClearCalcFields(var Buffer: TDataRecord);

begin
  // Empty
end;

procedure TDataSet.CloseBlob(Field: TField);

begin
  //!! To be implemented
end;

procedure TDataSet.CloseCursor;

begin
  ClearBuffers;
  SetBufListSize(0);
  Fields.ClearFieldDefs;
  InternalClose;
  FInternalOpenComplete := False;
end;

procedure TDataSet.CreateFields;

Var I : longint;

begin
{$ifdef DSDebug}
  Writeln ('Creating fields');
  Writeln ('Count : ',fielddefs.Count);
  For I:=0 to FieldDefs.Count-1 do
    Writeln('Def ',I,' : ',Fielddefs.items[i].Name,'(',Fielddefs.items[i].FieldNo,')');
{$endif}
  For I:=0 to FieldDefs.Count-1 do
    With FieldDefs.Items[I] do
      If DataType<>ftUnknown then
        begin
        {$ifdef DSDebug}
        Writeln('About to create field ',FieldDefs.Items[i].Name);
        {$endif}
        CreateField(self);
        end;
end;

procedure TDataSet.DataEvent(Event: TDataEvent; Info: JSValue);

  procedure HandleFieldChange(aField: TField);
  begin
    if aField.FieldKind in [fkData, fkInternalCalc] then
      SetModified(True);
      
    if State <> dsSetKey then begin
      if aField.FieldKind = fkData then begin
        if FInternalCalcFields then
          RefreshInternalCalcFields(FBuffers[FActiveRecord])
        else if FAutoCalcFields and (FCalcFieldsCount <> 0) then
          CalculateFields(FBuffers[FActiveRecord]);
      end;
      aField.Change;
    end;
  end;
  
  procedure HandleScrollOrChange;
  begin
    if State <> dsInsert then
      UpdateCursorPos;
  end;

var
  i: Integer;
begin
  case Event of
    deFieldChange   : HandleFieldChange(TField(Info));
    deDataSetChange,
    deDataSetScroll : HandleScrollOrChange;
    deLayoutChange  : FEnableControlsEvent:=deLayoutChange;    
  end;

  if not ControlsDisabled and (FState <> dsBlockRead) then begin
    for i := 0 to FDataSources.Count - 1 do
      TDataSource(FDataSources[i]).ProcessEvent(Event, Info);
  end;
end;

procedure TDataSet.DestroyFields;

begin
  FFieldList.Clear;
end;

procedure TDataSet.DoAfterCancel;

begin
 If assigned(FAfterCancel) then
   FAfterCancel(Self);
end;

procedure TDataSet.DoAfterClose;

begin
 If assigned(FAfterClose) and not (csDestroying in ComponentState) then
   FAfterClose(Self);
end;

procedure TDataSet.DoAfterDelete;

begin
 If assigned(FAfterDelete) then
   FAfterDelete(Self);
end;

procedure TDataSet.DoAfterEdit;

begin
 If assigned(FAfterEdit) then
   FAfterEdit(Self);
end;

procedure TDataSet.DoAfterInsert;

begin
 If assigned(FAfterInsert) then
   FAfterInsert(Self);
end;

procedure TDataSet.DoAfterOpen;

begin
 If assigned(FAfterOpen) then
   FAfterOpen(Self);
end;

procedure TDataSet.DoAfterPost;

begin
 If assigned(FAfterPost) then
   FAfterPost(Self);
end;

procedure TDataSet.DoAfterScroll;

begin
 If assigned(FAfterScroll) then
   FAfterScroll(Self);
end;

procedure TDataSet.DoAfterRefresh;

begin
 If assigned(FAfterRefresh) then
   FAfterRefresh(Self);
end;

procedure TDataSet.DoBeforeCancel;

begin
 If assigned(FBeforeCancel) then
   FBeforeCancel(Self);
end;

procedure TDataSet.DoBeforeClose;

begin
 If assigned(FBeforeClose) and not (csDestroying in ComponentState) then
   FBeforeClose(Self);
end;

procedure TDataSet.DoBeforeDelete;

begin
 If assigned(FBeforeDelete) then
   FBeforeDelete(Self);
end;

procedure TDataSet.DoBeforeEdit;

begin
 If assigned(FBeforeEdit) then
   FBeforeEdit(Self);
end;

procedure TDataSet.DoBeforeInsert;

begin
 If assigned(FBeforeInsert) then
   FBeforeInsert(Self);
end;

procedure TDataSet.DoBeforeOpen;

begin
 If assigned(FBeforeOpen) then
   FBeforeOpen(Self);
end;

procedure TDataSet.DoBeforePost;

begin
 If assigned(FBeforePost) then
   FBeforePost(Self);
end;

procedure TDataSet.DoBeforeScroll;

begin
 If assigned(FBeforeScroll) then
   FBeforeScroll(Self);
end;

procedure TDataSet.DoBeforeRefresh;

begin
 If assigned(FBeforeRefresh) then
   FBeforeRefresh(Self);
end;

procedure TDataSet.DoInternalOpen;

begin
  InternalOpen;
  FInternalOpenComplete := True;
{$ifdef dsdebug}
  Writeln ('Calling internal open');
{$endif}
{$ifdef dsdebug}
  Writeln ('Calling RecalcBufListSize');
{$endif}
  FRecordCount := 0;
  RecalcBufListSize;
  FBOF := True;
  FEOF := (FRecordCount = 0);
  if Assigned(DataProxy) then
    InitChangeList;
end;

procedure TDataSet.DoOnCalcFields;

begin
 If Assigned(FOnCalcfields) then
   FOnCalcFields(Self);
end;

procedure TDataSet.DoOnNewRecord;

begin
 If assigned(FOnNewRecord) then
   FOnNewRecord(Self);
end;

procedure TDataSet.DoBeforeLoad;
begin
  If Assigned(FBeforeLoad) then
    FBeforeLoad(Self);
end;

procedure TDataSet.DoAfterLoad;
begin
  if Assigned(FAfterLoad) then
    FAfterLoad(Self);
end;

procedure TDataSet.DoBeforeApplyUpdates;

begin
  If Assigned(FBeforeApplyUpdates) then
    FBeforeApplyUpdates(Self);
end;

procedure TDataSet.DoAfterApplyUpdates(Const ResolveInfo : TResolveResults);

begin
  If Assigned(FAfterApplyUpdates) then
    FAfterApplyUpdates(Self,ResolveInfo);
end;

function TDataSet.FieldByNumber(FieldNo: Longint): TField;

begin
  Result:=FFieldList.FieldByNumber(FieldNo);
end;

function TDataSet.FindRecord(Restart, GoForward: Boolean): Boolean;

begin
  //!! To be implemented
  Result:=false;
end;


function TDataSet.GetBookmarkStr: TBookmarkStr;

Var
  B : TBookMark;

begin
  Result:='';
  If BookMarkAvailable then
    begin
    GetBookMarkData(ActiveBuffer,B);
    Result:=TJSJSON.stringify(B);
    end
end;

function TDataSet.GetBuffer(Index: longint): TDataRecord;

begin
  Result:=FBuffers[Index];
end;

function TDataSet.GetBufferCount: Longint;
begin
  Result:=Length(FBuffers);
end;

function TDataSet.DoGetDataProxy: TDataProxy;

begin
  Result:=nil;
end;

procedure TDataSet.InitChangeList;

begin
  DoneChangeList;
  FChangeList:=TFPList.Create;
end;

procedure TDataSet.ClearChangeList;

Var
  I : integer;

begin
  If not Assigned(FChangeList) then
    exit;
  For I:=0 to FChangeList.Count-1 do
    begin
    TObject(FChangeList[i]).Destroy;
    FChangeList[i]:=Nil;
    end;
end;

Function TDataSet.IndexInChangeList(aBookmark : TBookmark) : Integer;

begin
  Result:=-1;
  if Not assigned(FChangeList) then
    exit;
  Result:=FChangeList.Count-1;
  While (Result>=0) and (CompareBookmarks(aBookMark,TRecordUpdateDescriptor(FChangeList[Result]).Bookmark)<>0) do
    Dec(Result);
end;

Function TDataSet.AddToChangeList(aChange: TUpdateStatus) : TRecordUpdateDescriptor;

Var
  B : TBookmark;
  I : Integer;

begin
  Result:=Nil;
  if Not Assigned(FChangeList) then
    Exit;
  B:=GetBookmark;
  I:=IndexInChangeList(B);
  if (I=-1) then
    begin
    if Assigned(DataProxy) then
      Result:=DataProxy.GetUpdateDescriptor(Self,B,ActiveBuffer.data,aChange)
    else
      Result:=TRecordUpdateDescriptor.Create(Nil,Self,B,ActiveBuffer.data,aChange);
    FChangeList.Add(Result);
    end
  else
    begin
    Result:=TRecordUpdateDescriptor(FChangeList[i]);
    Case aChange of
      usDeleted : Result.FStatus:=usDeleted;
      usInserted : DatabaseError(SErrInsertingSameRecordtwice,Self);
      usModified : Result.FData:=ActiveBuffer.Data;
    end
    end;
end;

procedure TDataSet.RemoveFromChangeList(R: TRecordUpdateDescriptor);

begin
  if Not (Assigned(R) and Assigned(FChangeList)) then
    Exit;
end;

Function TDataSet.GetRecordUpdates(AList: TRecordUpdateDescriptorList) : Integer;

Var
  I,MinIndex : integer;

begin
  MinIndex:=0; // Check batch list for minimal index ?
  For I:=MinIndex to FChangeList.Count-1 do
    Alist.Add(FChangeList[i]);
  Result:=FChangeList.Count;
end;

Function TDataSet.ResolveRecordUpdate(anUpdate: TRecordUpdateDescriptor) : Boolean;

// This must return true if the record may be removed from the list of 'modified' records.
// If it returns false, the record is kept in the list of modified records.

begin
  try
    Result:=DoResolveRecordUpdate(anUpdate);
    If not Result then
      anUpdate.FStatus:=usResolveFailed;
  except
    On E : Exception do
      begin
      anUpdate.ResolveFailed(E.Classname+': '+E.Message);
      Result:=False;
      end;
  end;
  DoOnRecordResolved(anUpdate);
end;

Function TDataSet.RecordUpdateDescriptorToResolveInfo(anUpdate: TRecordUpdateDescriptor) : TResolveInfo;

begin
  Result.BookMark:=anUpdate.Bookmark;
  Result.Data:=anUpdate.Data;
  Result.Status:=anUpdate.Status;
  Result.Error:=anUpdate.ResolveError;
end;

procedure TDataSet.DoOnRecordResolved(anUpdate: TRecordUpdateDescriptor) ;

Var
  Info : TResolveInfo;

begin
  if Not Assigned(OnRecordResolved) then exit;
  Info:=RecordUpdateDescriptorToResolveInfo(anUpdate);
  OnRecordResolved(Self,Info);
end;

procedure TDataSet.ResolveUpdateBatch(Sender: TObject; aBatch : TRecordUpdateBatch);

Var
  BI,RI,Idx: integer;
  RUD : TRecordUpdateDescriptor;
  doRemove : Boolean;
  Results : TResolveResults;

begin
  if Assigned(FBatchList) and (aBatch.Dataset=Self) then
    BI:=FBatchList.IndexOf(aBatch)
  else
    BI:=-1;
  if (BI=-1) then
    Exit;
  FBatchList.Delete(Bi);
  SetLength(Results.Records, aBatch.List.Count);
  For RI:=0 to aBatch.List.Count-1 do
    begin
    RUD:=aBatch.List[RI];
    Results.Records[RI]:=RecordUpdateDescriptorToResolveInfo(RUD);
    aBatch.List.Items[RI]:=Nil;
    Idx:=IndexInChangeList(RUD.Bookmark);
    if (Idx<>-1) then
      begin
      doRemove:=False;
      if (RUD.Status=usResolved) then
        DoRemove:=ResolveRecordUpdate(RUD)
      else
        // What if not resolvable.. ?
        DoRemove:=(RUD.Status in [usUnmodified]);
      If DoRemove then
        begin
        RUD.Free;
        FChangeList.Delete(Idx);
        end
      else
        RUD.Reset; // So we try it again in next applyupdates.
      end;
    end;
  if (FBatchList.Count=0) then
    FreeAndNil(FBatchList);
  DoAfterApplyUpdates(Results);
end;

procedure TDataSet.DoApplyUpdates;

Var
  B : TRecordUpdateBatch;
  l : TRecordUpdateDescriptorList;
  I : integer;

begin
  if Not Assigned(DataProxy) then
    DatabaseError(SErrDoApplyUpdatesNeedsProxy,Self);
  if Not (Assigned(FChangeList) and (FChangeList.Count>0)) then
    Exit;
  L:=TRecordUpdateDescriptorList.Create;
  try
    I:=GetRecordUpdates(L);
  except
    L.Free;
    Raise;
  end;
  Inc(FUpdateBatchID);
  B:=DataProxy.GetRecordUpdateBatch(FUpdateBatchID,L,True);
  B.FDataset:=Self;
  B.FLastChangeIndex:=I;
  B.OnResolve:=@ResolveUpdateBatch;
  If not Assigned(FBatchlist) then
    FBatchlist:=TFPList.Create;
  FBatchList.Add(B);
  DataProxy.ProcessUpdateBatch(B);
end;

procedure TDataSet.DoneChangeList;

begin
  ClearChangeList;
  FreeAndNil(FChangeList);
end;

function TDataSet.GetDataProxy: TDataProxy;

begin
  If (FDataProxy=Nil) then
    DataProxy:=DoGetDataProxy;
  Result:=FDataProxy;
end;

function TDataSet.DataPacketReceived(ARequest: TDataRequest): Boolean;

begin
  Result:=False;
end;

procedure TDataSet.HandleRequestresponse(ARequest: TDataRequest);

Var
  DataAdded : Boolean;

begin
  if Not Assigned(ARequest) then
    exit;
  Case ARequest.Success of
  rrFail:
    begin
    if Assigned(FOnLoadFail) then
      FOnLoadFail(Self,aRequest.RequestID,aRequest.ErrorMsg);
    end;
  rrEOF,
  rrOK :
    begin
    DataAdded:=False;
    // Notify caller
    if Assigned(ARequest.Event) then
      ARequest.Event(Self,aRequest.Data);
    // allow descendent to integrate data.
    // Must be done before user is notified or dataset is opened...
    if (ARequest.Success<>rrEOF) then
      DataAdded:=DataPacketReceived(aRequest);
    // Open if needed.
    if Not (Active or (loNoOpen in aRequest.LoadOptions)) then
      begin
      // Notify user
      if not (loNoEvents in aRequest.LoadOptions) then
        DoAfterLoad;
      Open
      end
    else
      begin
      if (loAtEOF in aRequest.LoadOptions) and DataAdded then
        FEOF:=False;
      if not (loNoEvents in aRequest.LoadOptions) then
        DoAfterLoad;
      end;
    end;
  end;
  aRequest.Destroy;
end;

function TDataSet.DoResolveRecordUpdate(anUpdate: TRecordUpdateDescriptor): Boolean;
begin
  Result:=True;
end;

procedure TDataSet.GetCalcFields(var Buffer: TDataRecord);

begin
  if (FCalcFieldsCount > 0) or FInternalCalcFields then
    CalculateFields(Buffer);
end;

function TDataSet.GetCanModify: Boolean;

begin
  Result:= not FIsUnidirectional;
end;

procedure TDataSet.GetChildren(Proc: TGetChildProc; Root: TComponent);

var
 I: Integer;
 Field: TField;

begin
 for I := 0 to Fields.Count - 1 do begin
   Field := Fields[I];
   if (Field.Owner = Root) then
     Proc(Field);
 end;
end;

function TDataSet.GetDataSource: TDataSource;
begin
  Result:=nil;
end;

function TDataSet.GetRecordSize: Word;
begin
  Result := 0;
end;

procedure TDataSet.InternalAddRecord(Buffer: Pointer; AAppend: Boolean);
begin
  // empty stub
end;

procedure TDataSet.InternalDelete;
begin
  // empty stub
end;

procedure TDataSet.InternalFirst;
begin
  // empty stub
end;

procedure TDataSet.InternalGotoBookmark(ABookmark: TBookMark);
begin
  // empty stub
end;


function TDataset.GetFieldData(Field: TField; Buffer: TDatarecord): JSValue;

begin
  Result:=TJSObject(buffer.data).Properties[Field.FieldName];
end;


procedure TDataSet.SetFieldData(Field: TField; var Buffer: TDataRecord; AValue : JSValue);

begin
  TJSObject(buffer.data).Properties[Field.FieldName]:=AValue;
end;


function TDataSet.GetFieldClass(FieldType: TFieldType): TFieldClass;

begin
  Result := DefaultFieldClasses[FieldType];
end;

function TDataSet.GetIsIndexField(Field: TField): Boolean;

begin
  Result:=False;
end;

function TDataSet.GetIndexDefs(IndexDefs: TIndexDefs; IndexTypes: TIndexOptions
  ): TIndexDefs;
  
var i,f : integer;
    IndexFields : TStrings;
    
begin
  IndexDefs.Update;
  Result := TIndexDefs.Create(Self);
  Result.Assign(IndexDefs);
  i := 0;
  IndexFields := TStringList.Create;
  while i < result.Count do
    begin
    if (not ((IndexTypes = []) and (result[i].Options = []))) and
       ((IndexTypes * result[i].Options) = []) then
      begin
      result.Delete(i);
      dec(i);
      end
    else
      begin
//      ExtractStrings([';'],[' '],result[i].Fields,Indexfields);
      for f := 0 to IndexFields.Count-1 do
        if FindField(Indexfields[f]) = nil  then
         begin
        result.Delete(i);
        dec(i);
        break;
        end;
      end;
    inc(i);
    end;
  IndexFields.Free;
end;

function TDataSet.GetNextRecord: Boolean;

Var
   T : TDataRecord;

begin
{$ifdef dsdebug}
  Writeln ('Getting next record. Internal RecordCount : ',FRecordCount);
  Writeln ('Getting next record. Internal buffercount : ',FBufferCount);
{$endif}
  If FRecordCount>0 Then
    SetCurrentRecord(FRecordCount-1);
  Result:=GetRecord(FBuffers[FBufferCount],gmNext,True)=grOK;
  if Result then
    begin
    If FRecordCount=0 then ActivateBuffers;
    if FRecordCount=FBufferCount then
      ShiftBuffersBackward
    else
      begin
      Inc(FRecordCount);
      FCurrentRecord:=FRecordCount - 1;
      T:=FBuffers[FCurrentRecord];
      FBuffers[FCurrentRecord]:=FBuffers[FBufferCount];
      FBuffers[FBufferCount]:=T;
      end;
    end
  else
    CursorPosChanged;
{$ifdef dsdebug}
  Writeln ('Result getting next record : ',Result);
{$endif}
end;

function TDataSet.GetNextRecords: Longint;

begin
  Result:=0;
{$ifdef dsdebug}
  Writeln ('Getting next record(s), need :',FBufferCount);
{$endif}
  While (FRecordCount<FBufferCount) and GetNextRecord do
    Inc(Result);
{$ifdef dsdebug}
  Writeln ('Result Getting next record(S), GOT :',RESULT);
{$endif}
end;

function TDataSet.GetPriorRecord: Boolean;

begin
{$ifdef dsdebug}
  Writeln ('GetPriorRecord: Getting previous record');
{$endif}
  CheckBiDirectional;
  If FRecordCount>0 Then SetCurrentRecord(0);
  Result:=GetRecord(FBuffers[FBufferCount],gmPrior,True)=grOK;
  if Result then
    begin
      If FRecordCount=0 then ActivateBuffers;
      ShiftBuffersForward;

      if FRecordCount<FBufferCount then
        Inc(FRecordCount);
    end
  else
    CursorPosChanged;
{$ifdef dsdebug}
  Writeln ('Result getting prior record : ',Result);
{$endif}
end;

function TDataSet.GetPriorRecords: Longint;

begin
  Result:=0;
{$ifdef dsdebug}
  Writeln ('Getting previous record(s), need :',FBufferCount);
{$endif}
  While (FRecordCount<FBufferCount) and GetPriorRecord do
    Inc(Result);
end;

function TDataSet.GetRecNo: Longint;

begin
  Result := -1;
end;

function TDataSet.GetRecordCount: Longint;

begin
  Result := -1;
end;

procedure TDataSet.InitFieldDefs;

begin
  if IsCursorOpen then
    InternalInitFieldDefs
  else
    begin
    try
      OpenCursor(True);
    finally
      CloseCursor;
      end;
    end;
end;

procedure TDataSet.SetBlockReadSize(AValue: Integer);
begin
  // the state is changed even when setting the same BlockReadSize (follows Delphi behavior)
  // e.g., state is dsBrowse and BlockReadSize is 1. Setting BlockReadSize to 1 will change state to dsBlockRead
  FBlockReadSize := AValue;
  if AValue > 0 then
  begin
    CheckActive; 
    SetState(dsBlockRead);
  end	
  else
  begin
    //update state only when in dsBlockRead 
    if FState = dsBlockRead then
      SetState(dsBrowse);
  end;	
end;

procedure TDataSet.SetFieldDefs(AFieldDefs: TFieldDefs);

begin
  Fields.ClearFieldDefs;
  FFieldDefs.Assign(AFieldDefs);
end;

procedure TDataSet.DoInsertAppendRecord(const Values: array of JSValue; DoAppend : boolean);
var i : integer;
    ValuesSize : integer;
begin
  ValuesSize:=Length(Values);
  if ValuesSize>FieldCount then DatabaseError(STooManyFields,self);
  if DoAppend then
    Append
  else
    Insert;
  for i := 0 to ValuesSize-1 do
    Fields[i].AssignValue(Values[i]);
  Post;
end;

procedure TDataSet.InitFieldDefsFromFields;
var i : integer;

begin
  if FieldDefs.Count = 0 then
    begin
    FieldDefs.BeginUpdate;
    try
      for i := 0 to Fields.Count-1 do with Fields[i] do
        if not (FieldKind in [fkCalculated,fkLookup]) then // Do not add fielddefs for calculated/lookup fields.
          begin
          FFieldDef:=FieldDefs.FieldDefClass.Create(FieldDefs,FieldName,DataType,Size,Required,FieldDefs.Count+1);
          with FFieldDef do
            begin
            if Required then Attributes := Attributes + [faRequired];
            if ReadOnly then Attributes := Attributes + [faReadOnly];
            end;
          end;
    finally
      FieldDefs.EndUpdate;
      end;
    end;
end;

procedure TDataSet.InitRecord(var Buffer: TDataRecord);

begin
  InternalInitRecord(Buffer);
  ClearCalcFields(Buffer);
end;

procedure TDataSet.InternalCancel;

begin
  //!! To be implemented
end;

procedure TDataSet.InternalEdit;

begin
  //!! To be implemented
end;

procedure TDataSet.InternalRefresh;

begin
  //!! To be implemented
end;

procedure TDataSet.OpenCursor(InfoQuery: Boolean);

begin
  if InfoQuery then
    InternalInitFieldDefs
  else if State <> dsOpening then
    DoInternalOpen;
end;

procedure TDataSet.OpenCursorcomplete;
begin
  try
    if FState = dsOpening then DoInternalOpen
  finally
    if FInternalOpenComplete then
      begin
      SetState(dsBrowse);
      DoAfterOpen;
      if not IsEmpty then
        DoAfterScroll;
      end
    else
      begin
      SetState(dsInactive);
      CloseCursor;
      end;
  end;
end;

procedure TDataSet.RefreshInternalCalcFields(Var Buffer: TDataRecord);

begin
  //!! To be implemented
end;

function TDataSet.SetTempState(const Value: TDataSetState): TDataSetState;

begin
  result := FState;
  FState := value;
  inc(FDisableControlsCount);
end;

procedure TDataSet.RestoreState(const Value: TDataSetState);

begin
  FState := value;
  dec(FDisableControlsCount);
end;

function TDataSet.GetActive: boolean;

begin
  result := (FState <> dsInactive) and (FState <> dsOpening);
end;

procedure TDataSet.InternalHandleException(E :Exception);

begin
  ShowException(E,Nil);
end;

procedure TDataSet.InternalInitRecord(var Buffer: TDataRecord);
begin
  // empty stub
end;

procedure TDataSet.InternalLast;
begin
  // empty stub
end;

procedure TDataSet.InternalPost;

  Procedure CheckRequiredFields;

  Var I : longint;

  begin
    For I:=0 to FFieldList.Count-1 do
      With FFieldList[i] do
        // Required fields that are NOT autoinc !! Autoinc cannot be set !!
        if Required and not ReadOnly and
           (FieldKind=fkData) and Not (DataType=ftAutoInc) and IsNull then
          DatabaseErrorFmt(SNeedField,[DisplayName],Self);
  end;

begin
  CheckRequiredFields;
end;

procedure TDataSet.InternalSetToRecord(Buffer: TDataRecord);
begin
  // empty stub
end;

procedure TDataSet.SetBookmarkFlag(Var Buffer: TDataRecord; Value: TBookmarkFlag);
begin
  // empty stub
end;

procedure TDataSet.SetBookmarkData(Var Buffer: TDataRecord; Data: TBookmark);
begin
  // empty stub
end;

procedure TDataSet.SetUniDirectional(const Value: Boolean);
begin
  FIsUniDirectional := Value;
end;

procedure TDataSet.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent=FDataProxy) then
    FDataProxy:=Nil;
end;

class function TDataSet.FieldDefsClass: TFieldDefsClass;
begin
  Result:=TFieldDefs;
end;

class function TDataSet.FieldsClass: TFieldsClass;
begin
  Result:=TFields;
end;

procedure TDataSet.SetActive(Value: Boolean);

begin
  if value and (Fstate = dsInactive) then
    begin
    if csLoading in ComponentState then
      begin
      FOpenAfterRead := true;
      exit;
      end
    else
      begin
      DoBeforeOpen;
      FEnableControlsEvent:=deLayoutChange;
      FInternalCalcFields:=False;
      try
        FDefaultFields:=FieldCount=0;
        OpenCursor(False);
      finally
        if FState <> dsOpening then OpenCursorComplete;
        end;
      end;
    FModified:=False;
    end
  else if not value and (Fstate <> dsinactive) then
    begin
    DoBeforeClose;
    SetState(dsInactive);
    FDataRequestID:=0;
    DoneChangeList;
    CloseCursor;
    DoAfterClose;
    FModified:=False;
    end
end;

procedure TDataSet.Loaded;

begin
  inherited;
  try
    if FOpenAfterRead then SetActive(true);
  except
    on E : Exception do
      if csDesigning in Componentstate then
        InternalHandleException(E);
      else
        raise;
  end;
end;


procedure TDataSet.RecalcBufListSize;

var
  i, j, ABufferCount: Integer;
  DataLink: TDataLink;

begin
{$ifdef dsdebug}
  Writeln('Recalculating buffer list size - check cursor');
{$endif}
  If Not IsCursorOpen Then
    Exit;
{$ifdef dsdebug}
  Writeln('Recalculating buffer list size');
{$endif}
  if IsUniDirectional then
    ABufferCount := 1
  else
    ABufferCount := DefaultBufferCount;
{$ifdef dsdebug}
  Writeln('Recalculating buffer list size, start count: ',ABufferCount);
{$endif}
  for i := 0 to FDataSources.Count - 1 do
    for j := 0 to TDataSource(FDataSources[i]).DataLinks.Count - 1 do
      begin
      DataLink:=TDataLink(TDataSource(FDataSources[i]).DataLinks[j]);
      if ABufferCount<DataLink.BufferCount then
        ABufferCount:=DataLink.BufferCount;
      end;
{$ifdef dsdebug}
  Writeln('Recalculating buffer list size, end  count: ',ABufferCount);
{$endif}

  If (FBufferCount=ABufferCount) Then
    exit;

{$ifdef dsdebug}
  Writeln('Setting buffer list size');
{$endif}
  SetBufListSize(ABufferCount);
{$ifdef dsdebug}
  Writeln('Getting next buffers');
{$endif}
  GetNextRecords;
  if (FRecordCount < FBufferCount) and not IsUniDirectional then
    begin
    FActiveRecord := FActiveRecord + GetPriorRecords;
    CursorPosChanged;
    end;
{$Ifdef dsDebug}
  WriteLn(
    'SetBufferCount: FActiveRecord=',FActiveRecord,
    ' FCurrentRecord=',FCurrentRecord,
    ' FBufferCount= ',FBufferCount,
    ' FRecordCount=',FRecordCount);
{$Endif}
end;

procedure TDataSet.SetBookmarkStr(const Value: TBookmarkStr);

Var
  O: TJSObject;
  B : TBookmark;

begin
  O:=TJSJSON.parseObject(Value);
  B.Flag:=TBookmarkFlag(O.Properties['flag']);
  B.Data:=O.Properties['Index'];
  GotoBookMark(B)
end;

procedure TDataSet.SetBufListSize(Value: Longint);

Var
  I : Integer;

begin
  if Value < 0 then Value := 0;
  If Value=FBufferCount Then
    exit;
  // Less buffers, shift buffers.
  if value>BufferCount then
    begin
    For I:=FBufferCount to Value do
      FBuffers[i]:=AllocRecordBuffer;
    end
  else if value<BufferCount then
    if (value>=0) and (FActiveRecord>Value-1) then
      begin
      for i := 0 to (FActiveRecord-Value) do
        ShiftBuffersBackward;
      FActiveRecord := Value -1;
      end;
  SetLength(FBuffers,Value+1); // FBuffers[FBufferCount] is used as a temp buffer
  FBufferCount:=Value;
  if FRecordCount > FBufferCount then
    FRecordCount := FBufferCount;

end;

procedure TDataSet.SetChildOrder(Child: TComponent; Order: Longint);

var
  Field: TField;
begin
  Field := Child as TField;
  if Fields.IndexOf(Field) >= 0 then
    Field.Index := Order;
end;

procedure TDataSet.SetCurrentRecord(Index: Longint);

begin
  If FCurrentRecord<>Index then
    begin
{$ifdef DSdebug}
    Writeln ('Setting current record to: ',index);
{$endif}
    if not FIsUniDirectional then Case GetBookMarkFlag(FBuffers[Index]) of
      bfCurrent : InternalSetToRecord(FBuffers[Index]);
      bfBOF : InternalFirst;
      bfEOF : InternalLast;
      end;
    FCurrentRecord:=Index;
    end;
end;

procedure TDataSet.SetDefaultFields(const Value: Boolean);
begin
  FDefaultFields := Value;
end;

procedure TDataSet.CheckBiDirectional;

begin
  if FIsUniDirectional then DataBaseError(SUniDirectional,Self);
end;

procedure TDataSet.SetFilterOptions(Value: TFilterOptions);

begin
  CheckBiDirectional;
  FFilterOptions := Value;
end;

procedure TDataSet.SetFilterText(const Value: string);

begin
  FFilterText := value;
end;

procedure TDataSet.SetFiltered(Value: Boolean);

begin
  if Value then CheckBiDirectional;
  FFiltered := value;
end;

procedure TDataSet.SetFound(const Value: Boolean);
begin
  FFound := Value;
end;

procedure TDataSet.SetModified(Value: Boolean);

begin
  FModified := value;
end;

procedure TDataSet.SetName(const NewName: TComponentName);

  function CheckName(const FieldName: string): string;
  var i,j: integer;
  begin
    Result := FieldName;
    i := 0;
    j := 0;
    while (i < Fields.Count) do begin
      if Result = Fields[i].FieldName then begin
        inc(j);
        Result := FieldName + IntToStr(j);
      end else Inc(i);
    end;
  end;

var
  i: integer;
  nm: string;
  old: string;

begin
  if Self.Name = NewName then Exit;
  old := Self.Name;
  inherited SetName(NewName);
  if (csDesigning in ComponentState) then
    for i := 0 to Fields.Count - 1 do begin
      nm := old + Fields[i].FieldName;
      if Copy(Fields[i].Name, 1, Length(nm)) = nm then
        Fields[i].Name := CheckName(NewName + Fields[i].FieldName);
    end;
end;

procedure TDataSet.SetOnFilterRecord(const Value: TFilterRecordEvent);

begin
  CheckBiDirectional;
  FOnFilterRecord := Value;
end;

procedure TDataSet.SetRecNo(Value: Longint);

begin
  //!! To be implemented
end;

procedure TDataSet.SetState(Value: TDataSetState);

begin
  If Value<>FState then
    begin
    FState:=Value;
    if Value=dsBrowse then
      FModified:=false;
    DataEvent(deUpdateState,0);
    end;
end;

function TDataSet.TempBuffer: TDataRecord;

begin
  Result := FBuffers[FRecordCount];
end;

procedure TDataSet.UpdateIndexDefs;

begin
  // Empty Abstract
end;

function TDataSet.AllocRecordBuffer: TDataRecord;
begin
  Result.data:=Null;
  Result.state:=rsNew;
//  Result := nil;
end;

procedure TDataSet.FreeRecordBuffer(var Buffer: TDataRecord);
begin
  // empty stub
end;

procedure TDataSet.GetBookmarkData(Buffer: TDataRecord; var Data: TBookmark);
begin

end;


function TDataSet.GetBookmarkFlag(Buffer: TDataRecord): TBookmarkFlag;
begin
  Result := bfCurrent;
end;

function TDataSet.ControlsDisabled: Boolean;

begin
  Result := (FDisableControlsCount > 0);
end;

function TDataSet.ActiveBuffer: TDataRecord;

begin
{$ifdef dsdebug}
  Writeln ('Active buffer requested. Returning record number: ',ActiveRecord);
{$endif}
  Result:=FBuffers[FActiveRecord];
end;

function TDataSet.GetFieldData(Field: TField): JSValue;
begin
  Result:=GetFieldData(Field,ActiveBuffer);
end;

procedure TDataSet.SetFieldData(Field: TField; AValue: JSValue);
begin
  SetFieldData(Field,FBuffers[FActiveRecord],AValue);
end;

procedure TDataSet.Append;

begin
  DoInsertAppend(True);
end;

procedure TDataSet.InternalInsert;

begin
  //!! To be implemented
end;

procedure TDataSet.AppendRecord(const Values: array of JSValue);

begin
  DoInsertAppendRecord(Values,True);
end;

function TDataSet.BookmarkValid(ABookmark: TBookmark): Boolean;
{
  Should be overridden by descendant objects.
}
begin
  Result:=False
end;



function TDataSet.ConvertToDateTime(aValue: JSValue; ARaiseException: Boolean): TDateTime;
begin
  Result:=DefaultConvertToDateTime(aValue,ARaiseException);
end;

class function TDataSet.DefaultConvertToDateTime(aValue: JSValue; ARaiseException: Boolean): TDateTime;
begin
  Result:=0;
  if IsString(aValue) then
    begin
    if not TryRFC3339ToDateTime(String(AValue),Result) then
      Raise EConvertError.CreateFmt(SErrInvalidDateTime,[String(aValue)])
    end
  else if IsNumber(aValue) then
    Result:=TDateTime(AValue)
end;

function TDataSet.ConvertDateTimeToNative(aValue : TDateTime) : JSValue;

begin
  Result:=DefaultConvertDateTimeToNative(aValue);
end;

Class function TDataSet.DefaultConvertDateTimeToNative(aValue : TDateTime) : JSValue;

begin
  Result:=DateTimeToRFC3339(aValue);
end;

function TDataSet.BlobDataToBytes(aValue: JSValue): TBytes;
begin
  Result:=DefaultBlobDataToBytes(aValue);
end;

class function TDataSet.DefaultBlobDataToBytes(aValue: JSValue): TBytes;

Var
  S : String;
  I,J,L : Integer;

begin
  SetLength(Result,0);
  // We assume a string, hex-encoded.
  if isString(AValue) then
    begin
    S:=String(Avalue);
    L:=Length(S);
    SetLength(Result,(L+1) div 2);
    I:=1;
    J:=0;
    While (I<L) do
      begin
      Result[J]:=StrToInt('$'+Copy(S,I,2));
      Inc(I,2);
      Inc(J,1);
      end;
    end;
end;

Function TDataSet.BytesToBlobData(aValue : TBytes) : JSValue ;

begin
  Result:=DefaultBytesToBlobData(aValue);
end;

Class Function TDataSet.DefaultBytesToBlobData(aValue : TBytes) : JSValue;

Var
  S : String;
  I : Integer;

begin
  if Length(AValue)=0 then
    Result:=Null
  else
    begin
    S:='';
    For I:=0 to Length(AValue) do
      TJSString(S).Concat(IntToHex(aValue[i],2));
    end;
end;

procedure TDataSet.Cancel;

begin
  If State in [dsEdit,dsInsert] then
    begin
    DataEvent(deCheckBrowseMode,0);
    DoBeforeCancel;
    UpdateCursorPos;
    InternalCancel;
    if (State = dsInsert) and (FRecordCount = 1) then
      begin
      FEOF := true;
      FBOF := true;
      FRecordCount := 0;
      InitRecord(FBuffers[FActiveRecord]);
      SetState(dsBrowse);
      DataEvent(deDatasetChange,0);
      end
    else
      begin
      SetState(dsBrowse);
      SetCurrentRecord(FActiveRecord);
      resync([]);
      end;
    DoAfterCancel;
    end;
end;

procedure TDataSet.CheckBrowseMode;

begin
  CheckActive;
  DataEvent(deCheckBrowseMode,0);
  Case State of
    dsEdit,dsInsert:
      begin
      UpdateRecord;
      If Modified then
        Post
      else
        Cancel;
      end;
    dsSetKey: Post;
  end;
end;

procedure TDataSet.ClearFields;


begin
  DataEvent(deCheckBrowseMode, 0);
  InternalInitRecord(FBuffers[FActiveRecord]);
  if State <> dsSetKey then
    GetCalcFields(FBuffers[FActiveRecord]);
  DataEvent(deRecordChange, 0);
end;

procedure TDataSet.Close;

begin
  Active:=False;
end;

procedure TDataSet.ApplyUpdates;
begin
  DoBeforeApplyUpdates;
  DoApplyUpdates;
end;

function TDataSet.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Longint;

begin
  Result:=0;
end;

procedure TDataSet.CursorPosChanged;


begin
  FCurrentRecord:=-1;
end;

procedure TDataSet.Delete;

Var
  R : TRecordUpdateDescriptor;

begin
  If Not CanModify then
    DatabaseError(SDatasetReadOnly,Self);
  If IsEmpty then
    DatabaseError(SDatasetEmpty,Self);
  if State in [dsInsert] then
  begin
    Cancel;
  end else begin
    DataEvent(deCheckBrowseMode,0);
{$ifdef dsdebug}
    writeln ('Delete: checking required fields');
{$endif}
    DoBeforeDelete;
    DoBeforeScroll;
    R:=AddToChangeList(usDeleted);
    If Not TryDoing(@InternalDelete,OnDeleteError) then
      begin
      if Assigned(R) then
        RemoveFromChangeList(R);
      exit;
      end;
{$ifdef dsdebug}
    writeln ('Delete: Internaldelete succeeded');
{$endif}
    SetState(dsBrowse);
{$ifdef dsdebug}
    writeln ('Delete: Browse mode set');
{$endif}
    SetCurrentRecord(FActiveRecord);
    Resync([]);
    DoAfterDelete;
    DoAfterScroll;
  end;
end;

procedure TDataSet.DisableControls;


begin
  If FDisableControlsCount=0 then
    begin
    { Save current state,
      needed to detect change of state when enabling controls.
    }
    FDisableControlsState:=FState;
    FEnableControlsEvent:=deDatasetChange;
    end;
  Inc(FDisableControlsCount);
end;

procedure TDataSet.DoInsertAppend(DoAppend: Boolean);


  procedure DoInsert(DoAppend : Boolean);

  Var
    BookBeforeInsert : TBookmark;
    TempBuf : TDataRecord;
    I : integer;

  begin
  // need to scroll up al buffers after current one,
  // but copy current bookmark to insert buffer.
  If FRecordCount > 0 then
    BookBeforeInsert:=Bookmark;

  if not DoAppend then
    begin
    if FRecordCount > 0 then
      begin
      TempBuf := FBuffers[FBufferCount];
      for I:=FBufferCount downto FActiveRecord+1 do
        FBuffers[I]:=FBuffers[I-1];
      FBuffers[FActiveRecord]:=TempBuf;
      end;
    end
  else if FRecordCount=FBufferCount then
    ShiftBuffersBackward
  else
    begin
    if FRecordCount>0 then
      inc(FActiveRecord);
    end;

  // Active buffer is now edit buffer. Initialize.
  InitRecord(FBuffers[FActiveRecord]);
  CursorPosChanged;

  // Put bookmark in edit buffer.
  if FRecordCount=0 then
    SetBookmarkFlag(FBuffers[FActiveRecord],bfEOF)
  else
    begin
    fBOF := false;
    // 29:01:05, JvdS: Why is this here?!? It can result in records with the same bookmark-data?
    // I would say that the 'internalinsert' should do this. But I don't know how Tdbf handles it

    // 1-apr-06, JvdS: It just sets the bookmark of the newly inserted record to the place
    // where the record should be inserted. So it is ok.
    if FRecordCount > 0 then
      begin
      SetBookMarkData(FBuffers[FActiveRecord],BookBeforeInsert);
      FreeBookmark(BookBeforeInsert);
      end;
    end;

  InternalInsert;

  // update buffer count.
  If FRecordCount<FBufferCount then
    Inc(FRecordCount);
  end;

begin
  CheckBrowseMode;
  If Not CanModify then
    DatabaseError(SDatasetReadOnly,Self);
  DoBeforeInsert;
  DoBeforeScroll;
  If Not DoAppend then
    begin
{$ifdef dsdebug}
    Writeln ('going to insert mode');
{$endif}
    DoInsert(false);
    end
  else
    begin
{$ifdef dsdebug}
    Writeln ('going to append mode');
{$endif}
    ClearBuffers;
    InternalLast;
    GetPriorRecords;
    if FRecordCount>0 then
      FActiveRecord:=FRecordCount-1;
    DoInsert(True);
    SetBookmarkFlag(FBuffers[FActiveRecord],bfEOF);
    FBOF :=False;
    FEOF := true;
    end;
  SetState(dsInsert);
  try
    DoOnNewRecord;
  except
    SetCurrentRecord(FActiveRecord);
    resync([]);
    raise;
  end;
  // mark as not modified.
  FModified:=False;
  // Final events.
  DataEvent(deDatasetChange,0);
  DoAfterInsert;
  DoAfterScroll;
{$ifdef dsdebug}
  Writeln ('Done with append');
{$endif}
end;

procedure TDataSet.Edit;

begin
  If State in [dsEdit,dsInsert] then exit;
  CheckBrowseMode;
  If Not CanModify then
    DatabaseError(SDatasetReadOnly,Self);
  If FRecordCount = 0 then
    begin
    Append;
    Exit;
    end;
  DoBeforeEdit;
  If Not TryDoing(@InternalEdit,OnEditError) then exit;
  GetCalcFields(FBuffers[FActiveRecord]);
  SetState(dsEdit);
  DataEvent(deRecordChange,0);
  DoAfterEdit;
end;

procedure TDataSet.EnableControls;


begin
  if FDisableControlsCount > 0 then
    Dec(FDisableControlsCount);

  if FDisableControlsCount = 0 then begin
    if FState <> FDisableControlsState then
      DataEvent(deUpdateState, 0);

    if (FState <> dsInactive) and (FDisableControlsState <> dsInactive) then
      DataEvent(FEnableControlsEvent, 0);
  end;
end;

function TDataSet.FieldByName(const FieldName: string): TField;


begin
  Result:=FindField(FieldName);
  If Result=Nil then
    DatabaseErrorFmt(SFieldNotFound,[FieldName],Self);
end;

function TDataSet.FindField(const FieldName: string): TField;


begin
  Result:=FFieldList.FindField(FieldName);
end;

function TDataSet.FindFirst: Boolean;


begin
  Result:=False;
end;

function TDataSet.FindLast: Boolean;


begin
  Result:=False;
end;

function TDataSet.FindNext: Boolean;


begin
  Result:=False;
end;

function TDataSet.FindPrior: Boolean;


begin
  Result:=False;
end;

procedure TDataSet.First;


begin
  CheckBrowseMode;
  DoBeforeScroll;
  if not FIsUniDirectional then
    ClearBuffers
  else if not FBof then
    begin
    Active := False;
    Active := True;
    end;
  try
    InternalFirst;
    if not FIsUniDirectional then GetNextRecords;
  finally
    FBOF:=True;
    DataEvent(deDatasetChange,0);
    DoAfterScroll;
    end;
end;

procedure TDataSet.FreeBookmark(ABookmark: TBookmark);


begin
  {$ifdef noautomatedbookmark}
   FreeMem(ABookMark,FBookMarkSize);
  {$endif}
end;

function TDataSet.GetBookmark: TBookmark;


begin
  if BookmarkAvailable then
    GetBookMarkdata(ActiveBuffer,Result)
  else
    Result.Data:=Null;
end;

function TDataSet.GetCurrentRecord(Buffer: TDataRecord): Boolean;


begin
  Result:=False;
end;

procedure TDataSet.GetFieldList(List: TList; const FieldNames: string);

var
  F: TField;
  N: String;
  StrPos: Integer;

begin
  if (FieldNames = '') or (List = nil) then
    Exit;
  StrPos := 1;
  repeat
    N := ExtractFieldName(FieldNames, StrPos);
    F := FieldByName(N);
    List.Add(F);
  until StrPos > Length(FieldNames);
end;

procedure TDataSet.GetFieldList(List: TFPList; const FieldNames: string);
var
  F: TField;
  N: String;
  StrPos: Integer;

begin
  if (FieldNames = '') or (List = nil) then
    Exit;
  StrPos := 1;
  repeat
    N := ExtractFieldName(FieldNames, StrPos);
    F := FieldByName(N);
    List.Add(F);
  until StrPos > Length(FieldNames);
end;

procedure TDataSet.GetFieldNames(List: TStrings);


begin
  FFieldList.GetFieldNames(List);
end;

procedure TDataSet.GotoBookmark(const ABookmark: TBookmark);


begin
  If Assigned(ABookMark) then
    begin
    CheckBrowseMode;
    DoBeforeScroll;
{$ifdef dsdebug}
    Writeln('Gotobookmark: ',ABookMark.Data);
{$endif}
    InternalGotoBookMark(ABookMark);
    Resync([rmExact,rmCenter]);
    DoAfterScroll;
    end;
end;

procedure TDataSet.Insert;

begin
  DoInsertAppend(False);
end;

procedure TDataSet.InsertRecord(const Values: array of JSValue);

begin
  DoInsertAppendRecord(Values,False);
end;

function TDataSet.IsEmpty: Boolean;

begin
  Result:=(fBof and fEof) and
          (not (State = dsInsert)); // After an insert on an empty dataset, both fBof and fEof are true
end;

function TDataSet.IsLinkedTo(ADataSource: TDataSource): Boolean;

begin
//!! Not tested, I never used nested DS
  if (ADataSource = nil) or (ADataSource.Dataset = nil) then begin
    Result := False
  end else if ADataSource.Dataset = Self then begin
    Result := True;
  end else begin
    Result := ADataSource.Dataset.IsLinkedTo(ADataSource.Dataset.DataSource);
  end;
//!! DataSetField not implemented
end;

function TDataSet.IsSequenced: Boolean;

begin
  Result := True;
end;

procedure TDataSet.Last;

begin
  CheckBiDirectional;
  CheckBrowseMode;
  DoBeforeScroll;
  ClearBuffers;
  try
    // Writeln('FActiveRecord before last',FActiveRecord);
    InternalLast;
    // Writeln('FActiveRecord after last',FActiveRecord);
    GetPriorRecords;
    // Writeln('FRecordCount: ',FRecordCount);
    if FRecordCount>0 then
      FActiveRecord:=FRecordCount-1;
    // Writeln('FActiveRecord ',FActiveRecord);
  finally
    FEOF:=true;
    DataEvent(deDataSetChange, 0);
    DoAfterScroll;
    end;
end;

function TDataSet.DoLoad(aOptions: TLoadOptions; aAfterLoad: TDatasetLoadEvent): Boolean;

Var
  Request : TDataRequest;

begin
  if not (loNoEvents in aOptions) then
    DoBeforeLoad;
  Result:=DataProxy<>Nil;
  if Not Result then
    Exit;
  Request:=DataProxy.GetDataRequest(aOptions,@HandleRequestResponse,aAfterLoad);
  Request.FDataset:=Self;
  If Active then
    Request.FBookmark:=GetBookmark;
  Inc(FDataRequestID);
  Request.FRequestID:=FDataRequestID;
  DataProxy.DoGetData(Request);
end;


function TDataSet.Load(aOptions: TLoadOptions; aAfterLoad: TDatasetLoadEvent): Boolean;

begin
  if loAtEOF in aOptions then
    DatabaseError(SatEOFInternalOnly,Self);
  Result:=DoLoad(aOptions,aAfterLoad);
end;

function TDataSet.MoveBy(Distance: Longint): Longint;
Var
  TheResult: Integer;

  Function ScrollForward : Integer;
  begin
    Result:=0;
{$ifdef dsdebug}
    Writeln('Scrolling forward : ',Distance);
    Writeln('Active buffer : ',FActiveRecord);
    Writeln('RecordCount   : ',FRecordCount);
    WriteLn('BufferCount   : ',FBufferCount);
{$endif}
    FBOF:=False;
    While (Distance>0) and not FEOF do
      begin
      If FActiveRecord<FRecordCount-1 then
        begin
        Inc(FActiveRecord);
        Dec(Distance);
        Inc(TheResult); //Inc(Result);
        end
      else
        begin
{$ifdef dsdebug}
       Writeln('Moveby : need next record');
{$endif}
        If GetNextRecord then
          begin
          Dec(Distance);
          Dec(Result);
          Inc(TheResult); //Inc(Result);
          end
        else
          begin
          FEOF:=true;
          // Allow to load more records.
          DoLoad([loNoOpen,loAtEOF],Nil);
          end;
        end;
      end
  end;

  Function ScrollBackward : Integer;
  begin
    CheckBiDirectional;
    Result:=0;
{$ifdef dsdebug}
    Writeln('Scrolling backward : ',Abs(Distance));
    Writeln('Active buffer : ',FActiveRecord);
    Writeln('RecordCunt    : ',FRecordCount);
    WriteLn('BufferCount   : ',FBufferCount);
{$endif}
    FEOF:=False;
    While (Distance<0) and not FBOF do
      begin
      If FActiveRecord>0 then
        begin
        Dec(FActiveRecord);
        Inc(Distance);
        Dec(TheResult); //Dec(Result);
        end
      else
        begin
       {$ifdef dsdebug}
       Writeln('Moveby : need next record');
       {$endif}
        If GetPriorRecord then
          begin
          Inc(Distance);
          Inc(Result);
          Dec(TheResult); //Dec(Result);
          end
        else
          FBOF:=true;
        end;
      end
  end;

Var
  Scrolled : Integer;

begin
  CheckBrowseMode;
  Result:=0; TheResult:=0;
  DoBeforeScroll;
  If (Distance = 0) or
     ((Distance>0) and FEOF) or
     ((Distance<0) and FBOF) then
    exit;
  Try
    Scrolled := 0;
    If Distance>0 then
      Scrolled:=ScrollForward
    else
      Scrolled:=ScrollBackward;
  finally
{$ifdef dsdebug}
    WriteLn('ActiveRecord=', FActiveRecord,' FEOF=',FEOF,' FBOF=',FBOF);
{$Endif}
    DataEvent(deDatasetScroll,Scrolled);
    DoAfterScroll;
    Result:=TheResult;
  end;
end;

procedure TDataSet.Next;

begin
  if BlockReadSize>0 then
    BlockReadNext
  else
    MoveBy(1);
end;

procedure TDataSet.BlockReadNext;
begin
  MoveBy(1);
end;

procedure TDataSet.Open;

begin
  Active:=True;
end;

procedure TDataSet.Post;

Const
  UpdateStates : Array[Boolean] of TUpdateStatus = (usModified,usInserted);

Var
  R : TRecordUpdateDescriptor;
  WasInsert : Boolean;

begin
  UpdateRecord;
  if State in [dsEdit,dsInsert] then
    begin
    DataEvent(deCheckBrowseMode,0);
{$ifdef dsdebug}
    writeln ('Post: checking required fields');
{$endif}
    DoBeforePost;
    WasInsert:=State=dsInsert;
    If Not TryDoing(@InternalPost,OnPostError) then exit;
    CursorPosChanged;
{$ifdef dsdebug}
    writeln ('Post: Internalpost succeeded');
{$endif}
// First set the state to dsBrowse, then the Resync, to prevent the calling of
// the deDatasetChange event, while the state is still 'editable', while the db isn't
    SetState(dsBrowse);
    Resync([]);
    // We get the new values here, since the bookmark should now be correct to find the record later on when doing applyupdates.
    R:=AddToChangeList(UpdateStates[wasInsert]);
    if Assigned(R) then
      R.FBookmark:=BookMark;
{$ifdef dsdebug}
    writeln ('Post: Browse mode set');
{$endif}
    DoAfterPost;
    end
  else if State<>dsSetKey then
    DatabaseErrorFmt(SNotEditing, [Name], Self);
end;

procedure TDataSet.Prior;

begin
  MoveBy(-1);
end;

procedure TDataSet.Refresh;

begin
  CheckbrowseMode;
  DoBeforeRefresh;
  UpdateCursorPos;
  InternalRefresh;
{ SetCurrentRecord is called by UpdateCursorPos already, so as long as
  InternalRefresh doesn't do strange things this should be ok. }
//  SetCurrentRecord(FActiveRecord);
  Resync([]);
  DoAfterRefresh;
end;

procedure TDataSet.RegisterDataSource(ADataSource: TDataSource);

begin
  FDataSources.Add(ADataSource);
  RecalcBufListSize;
end;


procedure TDataSet.Resync(Mode: TResyncMode);

var i,count : integer;

begin
  // See if we can find the requested record.
{$ifdef dsdebug}
    Writeln ('Resync called');
{$endif}
  if FIsUnidirectional then Exit;
// place the cursor of the underlying dataset to the active record
//  SetCurrentRecord(FActiveRecord);

// Now look if the data on the current cursor of the underlying dataset is still available
  If GetRecord(FBuffers[0],gmCurrent,False)<>grOk Then
// If that fails and rmExact is set, then raise an exception
    If rmExact in Mode then
      DatabaseError(SNoSuchRecord,Self)
// else, if rmexact is not set, try to fetch the next  or prior record in the underlying dataset
    else if (GetRecord(FBuffers[0],gmNext,True)<>grOk) and
            (GetRecord(FBuffers[0],gmPrior,True)<>grOk) then
      begin
{$ifdef dsdebug}
      Writeln ('Resync: fuzzy resync');
{$endif}
      // nothing found, invalidate buffer and bail out.
      ClearBuffers;
      // Make sure that the active record is 'empty', ie: that all fields are null
      InternalInitRecord(FBuffers[FActiveRecord]);
      DataEvent(deDatasetChange,0);
      exit;
      end;
  FCurrentRecord := 0;
  FEOF := false;
  FBOF := false;

// If we've arrived here, FBuffer[0] is the current record
  If (rmCenter in Mode) then
    count := (FRecordCount div 2)
  else
    count := FActiveRecord;
  i := 0;
  FRecordCount := 1;
  FActiveRecord := 0;

// Fill the buffers before the active record
  while (i < count) and GetPriorRecord do
    inc(i);
  FActiveRecord := i;
// Fill the rest of the buffer
  GetNextRecords;
// If the buffer is not full yet, try to fetch some more prior records
  if FRecordCount < FBufferCount then FActiveRecord:=FActiveRecord+getpriorrecords;
// That's all folks!
  DataEvent(deDatasetChange,0);
end;

procedure TDataSet.SetFields(const Values: array of JSValue);

Var I  : longint;
begin
  For I:=0 to high(Values) do
    Fields[I].AssignValue(Values[I]);
end;

function TDataSet.TryDoing(P: TDataOperation; Ev: TDatasetErrorEvent): Boolean;

Var Retry : TDataAction;

begin
{$ifdef dsdebug}
  Writeln ('Trying to do');
  If P=Nil then writeln ('Procedure to call is nil !!!');
{$endif dsdebug}
  Result:=True;
  Retry:=daRetry;
  while Retry=daRetry do
    Try
{$ifdef dsdebug}
      Writeln ('Trying : updatecursorpos');
{$endif dsdebug}
      UpdateCursorPos;
{$ifdef dsdebug}
      Writeln ('Trying to do it');
{$endif dsdebug}
      P();
      exit;
    except
      On E : EDatabaseError do
        begin
        retry:=daFail;
        If Assigned(Ev) then
          Ev(Self,E,Retry);
        Case Retry of
          daFail : Raise;
          daAbort : Abort;
        end;
        end;
    else
      Raise;
    end;
{$ifdef dsdebug}
  Writeln ('Exit Trying to do');
{$endif dsdebug}
end;

procedure TDataSet.UpdateCursorPos;

begin
  If FRecordCount>0 then
    SetCurrentRecord(FActiveRecord);
end;

procedure TDataSet.UpdateRecord;

begin
  if not (State in dsEditModes) then
    DatabaseErrorFmt(SNotEditing, [Name], Self);
  DataEvent(deUpdateRecord, 0);
end;

function TDataSet.GetPendingUpdates: TResolveInfoArray;

Var
  L : TRecordUpdateDescriptorList;
  I : integer;

begin
  L:=TRecordUpdateDescriptorList.Create;
  try
    SetLength(Result,GetRecordUpdates(L));
    For I:=0 to L.Count-1 do
      Result[i]:=RecordUpdateDescriptorToResolveInfo(L[i]);
  finally
    L.Free;
  end;
end;

function TDataSet.UpdateStatus: TUpdateStatus;

begin
  Result:=usUnmodified;
end;

procedure TDataSet.SetConstraints(Value: TCheckConstraints);
begin
  FConstraints.Assign(Value);
end;

procedure TDataSet.SetDataProxy(AValue: TDataProxy);
begin
  If AValue=FDataProxy then
    exit;
  if Assigned(FDataProxy) then
    FDataProxy.RemoveFreeNotification(Self);
  FDataProxy:=AValue;
  if Assigned(FDataProxy) then
    FDataProxy.FreeNotification(Self)
end;

function TDataSet.GetfieldCount: Integer;

begin
  Result:=FFieldList.Count;
end;

procedure TDataSet.ShiftBuffersBackward;

var
  TempBuf : TDataRecord;
  I : Integer;

begin
  TempBuf := FBuffers[0];
  For I:=1 to FBufferCount do
    FBuffers[I-1]:=FBuffers[i];
  FBuffers[BufferCount]:=TempBuf;
end;

procedure TDataSet.ShiftBuffersForward;

var
  TempBuf : TDataRecord;
  I : Integer;

begin
  TempBuf := FBuffers[FBufferCount];
  For I:=FBufferCount downto 1 do
    FBuffers[I]:=FBuffers[i-1];
  FBuffers[0]:=TempBuf;
end;

function TDataSet.GetFieldValues(const FieldName: string): JSValue;

var
  i: Integer;
  FieldList: TList;
  A : TJSValueDynArray;

begin
  FieldList := TList.Create;
  try
    GetFieldList(FieldList, FieldName);
    if FieldList.Count>1 then
      begin
      SetLength(A,FieldList.Count);
      for i := 0 to FieldList.Count - 1 do
        A[i] := TField(FieldList[i]).Value;
      Result:=A;
      end
    else
      Result := FieldByName(FieldName).Value;
  finally
    FieldList.Free;
  end;
end;

procedure TDataSet.SetFieldValues(const FieldName: string; Value: JSValue);

var
  i : Integer;
  FieldList: TList;
  A : TJSValueDynArray;

begin
  if IsArray(Value) then
    begin
    FieldList := TList.Create;
    try
      GetFieldList(FieldList, FieldName);
      A:=TJSValueDynArray(Value);
      if (FieldList.Count = 1) and (Length(A)>0) then
        // Allow for a field type that can deal with an array
        FieldByName(FieldName).Value := Value
      else
        for i := 0 to FieldList.Count - 1 do
          TField(FieldList[i]).Value := A[i];
    finally
      FieldList.Free;
    end;
    end
  else
    FieldByName(FieldName).Value := Value;
end;

function TDataSet.Locate(const KeyFields: string; const KeyValues: JSValue;
  Options: TLocateOptions): boolean;

begin
  CheckBiDirectional;
  Result := False;
end;

function TDataSet.Lookup(const KeyFields: string; const KeyValues: JSValue;
  const ResultFields: string): JSValue;

begin
  CheckBiDirectional;
  Result := Null;
end;


procedure TDataSet.UnRegisterDataSource(ADataSource: TDataSource);

begin
  FDataSources.Remove(ADataSource);
end;

{ ---------------------------------------------------------------------
    TFieldDef
  ---------------------------------------------------------------------}

constructor TFieldDef.Create(ACollection: TCollection);

begin
  Inherited Create(ACollection);
  FFieldNo:=Index+1;
end;

constructor TFieldDef.Create(AOwner: TFieldDefs; const AName: string; ADataType: TFieldType; ASize: Integer; ARequired: Boolean;
  AFieldNo: Longint);
begin
{$ifdef dsdebug }
  Writeln('TFieldDef.Create : ',Aname,'(',AFieldNo,')');
{$endif}
  Inherited Create(AOwner);
  Name:=Aname;
  FDatatype:=ADatatype;
  FSize:=ASize;
  FRequired:=ARequired;
  FPrecision:=-1;
  FFieldNo:=AFieldNo;
end;

destructor TFieldDef.Destroy;

begin
  Inherited destroy;
end;

procedure TFieldDef.Assign(Source: TPersistent);
var fd: TFieldDef;
begin
  fd := nil;
  if Source is TFieldDef then
    fd := Source as TFieldDef;
  if Assigned(fd) then begin
    Collection.BeginUpdate;
    try
      Name := fd.Name;
      DataType := fd.DataType;
      Size := fd.Size;
      Precision := fd.Precision;
      FRequired := fd.Required;
    finally
      Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TFieldDef.CreateField(AOwner: TComponent): TField;

var TheField : TFieldClass;

begin
{$ifdef dsdebug}
  Writeln ('Creating field '+FNAME);
{$endif dsdebug}
  TheField:=GetFieldClass;
  if TheField=Nil then
    DatabaseErrorFmt(SUnknownFieldType,[FName]);
  Result:=TheField.Create(AOwner);
  Try
    Result.FFieldDef:=Self;
    Result.Size:=FSize;
    Result.Required:=FRequired;
    Result.FFieldName:=FName;
    Result.FDisplayLabel:=DisplayName;
    Result.FFieldNo:=Self.FieldNo;
    Result.SetFieldType(DataType);
    Result.FReadOnly:=(faReadOnly in Attributes);
{$ifdef dsdebug}
    Writeln ('TFieldDef.CreateField : Result Fieldno : ',Result.FieldNo,'; Self : ',FieldNo);
    Writeln ('TFieldDef.CreateField : Trying to set dataset');
{$endif dsdebug}
    Result.Dataset:=TFieldDefs(Collection).Dataset;
   if (Result is TFloatField) then
      TFloatField(Result).Precision := FPrecision;
  except
    Result.Free;
    Raise;
  end;
end;

procedure TFieldDef.SetAttributes(AValue: TFieldAttributes);
begin
  FAttributes := AValue;
  Changed(False);
end;

procedure TFieldDef.SetDataType(AValue: TFieldType);
begin
  FDataType := AValue;
  Changed(False);
end;

procedure TFieldDef.SetPrecision(const AValue: Longint);
begin
  FPrecision := AValue;
  Changed(False);
end;

procedure TFieldDef.SetSize(const AValue: Integer);
begin
  FSize := AValue;
  Changed(False);
end;

procedure TFieldDef.SetRequired(const AValue: Boolean);
begin
  FRequired := AValue;
  Changed(False);
end;

function TFieldDef.GetFieldClass: TFieldClass;

begin
  //!! Should be owner as tdataset but that doesn't work ??

  If Assigned(Collection) And
     (Collection is TFieldDefs) And
     Assigned(TFieldDefs(Collection).Dataset) then
    Result:=TFieldDefs(Collection).Dataset.GetFieldClass(FDataType)
  else
    Result:=Nil;
end;


{ ---------------------------------------------------------------------
    TFieldDefs
  ---------------------------------------------------------------------}

{
destructor TFieldDefs.Destroy;

begin
  FItems.Free;
  // This will destroy all fielddefs since we own them...
  Inherited Destroy;
end;
}

procedure TFieldDefs.Add(const AName: string; ADataType: TFieldType);

begin
  Add(AName,ADatatype,0,False);
end;

procedure TFieldDefs.Add(const AName: string; ADataType: TFieldType; ASize : Word);

begin
  Add(AName,ADatatype,ASize,False);
end;

procedure TFieldDefs.Add(const AName: string; ADataType: TFieldType; ASize: Word;
  ARequired: Boolean);

begin
  If Length(AName)=0 Then
    DatabaseError(SNeedFieldName,Dataset);
  // the fielddef will register itself here as an owned component.
  // fieldno is 1 based !
  BeginUpdate;
  try
    Add(AName,ADataType,ASize,ARequired,Count+1);
  finally
    EndUpdate;
  end;
end;

function TFieldDefs.GetItem(Index: Longint): TFieldDef;

begin
  Result := TFieldDef(inherited Items[Index]);
end;

procedure TFieldDefs.SetItem(Index: Longint; const AValue: TFieldDef);
begin
  inherited Items[Index] := AValue;
end;

class function TFieldDefs.FieldDefClass: TFieldDefClass;
begin
  Result:=TFieldDef;
end;

constructor TFieldDefs.Create(ADataSet: TDataSet);
begin
  Inherited Create(ADataset, Owner, FieldDefClass);
end;

function TFieldDefs.Add(const AName: string; ADataType: TFieldType; ASize, APrecision: Integer;
  ARequired, AReadOnly: Boolean; AFieldNo: Integer): TFieldDef;
begin
  Result:=FieldDefClass.Create(Self, MakeNameUnique(AName), ADataType, ASize, ARequired, AFieldNo);
  if AReadOnly then
    Result.Attributes := Result.Attributes + [faReadOnly];
end;

function TFieldDefs.Add(const AName: string; ADataType: TFieldType; ASize: Word; ARequired: Boolean; AFieldNo: Integer): TFieldDef;
begin
  Result:=FieldDefClass.Create(Self,AName,ADataType,ASize,ARequired,AFieldNo);
end;

procedure TFieldDefs.Assign(FieldDefs: TFieldDefs);

var I : longint;

begin
  Clear;
  For i:=0 to FieldDefs.Count-1 do
    With FieldDefs[i] do
      Add(Name,DataType,Size,Required);
end;

function TFieldDefs.Find(const AName: string): TFieldDef;
begin
  Result := (Inherited Find(AName)) as TFieldDef;
  if Result=nil then DatabaseErrorFmt(SFieldNotFound,[AName],FDataset);
end;

{
procedure TFieldDefs.Clear;

var I : longint;

begin
  For I:=FItems.Count-1 downto 0 do
    TFieldDef(Fitems[i]).Free;
  FItems.Clear;
end;
}

procedure TFieldDefs.Update;

begin
  if not Updated then
    begin
    If Assigned(Dataset) then
      DataSet.InitFieldDefs;
    Updated := True;
    end;
end;

function TFieldDefs.MakeNameUnique(const AName: String): string;
var DblFieldCount : integer;
begin
  DblFieldCount := 0;
  Result := AName;
  while assigned(inherited Find(Result)) do
    begin
    inc(DblFieldCount);
    Result := AName + '_' + IntToStr(DblFieldCount);
    end;
end;

function TFieldDefs.AddFieldDef: TFieldDef;

begin
  Result:=FieldDefClass.Create(Self,'',ftUnknown,0,False,Count+1);
end;

{ ---------------------------------------------------------------------
    TField
  ---------------------------------------------------------------------}

Const
//  SBCD = 'BCD';
  SBoolean = 'Boolean';
  SDateTime = 'TDateTime';
  SFloat = 'Float';
  SInteger = 'Integer';
  SLargeInt = 'NativeInt';
  SJSValue = 'JSValue';
  SString = 'String';
  SBytes = 'Bytes';

constructor TField.Create(AOwner: TComponent);

//Var
//  I : Integer;

begin
  Inherited Create(AOwner);
  FVisible:=True;
  SetLength(FValidChars,255);
//  For I:=0 to 255 do
//    FValidChars[i]:=Char(i);

  FProviderFlags := [pfInUpdate,pfInWhere];
end;

destructor TField.Destroy;

begin
  IF Assigned(FDataSet) then
    begin
    FDataSet.Active:=False;
    if Assigned(FFields) then
      FFields.Remove(Self);
    end;
  FLookupList.Free;
  Inherited Destroy;
end;

Procedure TField.RaiseAccessError(const TypeName: string);

Var
  E : EDatabaseError;

begin
  E:=AccessError(TypeName);
  Raise E;
end;

function TField.AccessError(const TypeName: string): EDatabaseError;

begin
  Result:=EDatabaseError.CreateFmt(SinvalidTypeConversion,[TypeName,FFieldName]);
end;

procedure TField.Assign(Source: TPersistent);

begin
  if Source = nil then Clear
  else if Source is TField then begin
    Value := TField(Source).Value;
  end else
    inherited Assign(Source);
end;

procedure TField.AssignValue(const AValue: JSValue);

  procedure Error;
  begin
    DatabaseErrorFmt(SFieldValueError, [DisplayName]);
  end;

begin
  Case GetValueType(AValue) of
    jvtNull : Clear;
    jvtBoolean : AsBoolean:=Boolean(AValue);
    jvtInteger : AsLargeInt:=NativeInt(AValue);
    jvtFloat : AsFloat:=Double(AValue);
    jvtString : AsString:=String(AValue);
    jvtArray : SetAsBytes(TBytes(AValue));
  else
    Error;
  end;
end;

procedure TField.Bind(Binding: Boolean);

begin
  if Binding and (FieldKind=fkLookup) then
    begin
    if ((FLookupDataSet = nil) or (FLookupKeyFields = '') or
       (FLookupResultField = '') or (FKeyFields = '')) then
      DatabaseErrorFmt(SLookupInfoError, [DisplayName]);
    FFields.CheckFieldNames(FKeyFields);
    FLookupDataSet.Open;
    FLookupDataSet.Fields.CheckFieldNames(FLookupKeyFields);
    FLookupDataSet.FieldByName(FLookupResultField);
    if FLookupCache then
      RefreshLookupList;
    end;
end;

procedure TField.Change;

begin
  If Assigned(FOnChange) Then
    FOnChange(Self);
end;

procedure TField.CheckInactive;

begin
  If Assigned(FDataSet) then
    FDataset.CheckInactive;
end;

procedure TField.Clear;

begin
  SetData(Nil);
end;

procedure TField.DataChanged;

begin
  FDataset.DataEvent(deFieldChange,self);
end;

procedure TField.FocusControl;
var
  Field1: TField;
begin
  Field1 := Self;
  FDataSet.DataEvent(deFocusControl,Field1);
end;

function TField.GetAsBoolean: Boolean;
begin
  raiseAccessError(SBoolean);
  Result:=false;
end;

function TField.GetAsBytes: TBytes;

begin
  raiseAccessError(SBytes);
  Result:=nil;
end;


function TField.GetAsDateTime: TDateTime;

begin
  raiseAccessError(SdateTime);
  Result:=0.0;
end;

function TField.GetAsFloat: Double;

begin
  raiseAccessError(SDateTime);
  Result:=0.0;
end;

function TField.GetAsLargeInt: NativeInt;
begin
  RaiseAccessError(SLargeInt);
  Result:=0;
end;

function TField.GetAsLongint: Longint;

begin
  Result:=GetAsInteger;
end;

function TField.GetAsInteger: Longint;

begin
  RaiseAccessError(SInteger);
  Result:=0;
end;

function TField.GetAsJSValue: JSValue;

begin
  Result:=GetData
end;


function TField.GetAsString: string;
begin
  Result := GetClassDesc
end;

function TField.GetOldValue: JSValue;

var SaveState : TDatasetState;

begin
  SaveState := FDataset.State;
  try
    FDataset.SetTempState(dsOldValue);
    Result := GetAsJSValue;
  finally
    FDataset.RestoreState(SaveState);
  end;
end;

function TField.GetNewValue: JSValue;

var SaveState : TDatasetState;

begin
  SaveState := FDataset.State;
  try
    FDataset.SetTempState(dsNewValue);
    Result := GetAsJSValue;
  finally
    FDataset.RestoreState(SaveState);
  end;
end;

procedure TField.SetNewValue(const AValue: JSValue);

var SaveState : TDatasetState;

begin
  SaveState := FDataset.State;
  try
    FDataset.SetTempState(dsNewValue);
    SetAsJSValue(AValue);
  finally
    FDataset.RestoreState(SaveState);
  end;
end;

function TField.GetCurValue: JSValue;

var SaveState : TDatasetState;

begin
  SaveState := FDataset.State;
  try
    FDataset.SetTempState(dsCurValue);
    Result := GetAsJSValue;
  finally
    FDataset.RestoreState(SaveState);
  end;
end;

function TField.GetCanModify: Boolean;

begin
  Result:=Not ReadOnly;
  If Result then
    begin
    Result := FieldKind in [fkData, fkInternalCalc];
    if Result then
      begin
      Result:=Assigned(DataSet) and Dataset.Active;
      If Result then
        Result:= DataSet.CanModify;
      end;
    end;
end;

function TField.GetClassDesc: String;
var ClassN : string;
begin
  ClassN := copy(ClassName,2,pos('Field',ClassName)-2);
  if isNull then
    result := '(' + LowerCase(ClassN) + ')'
   else
    result := '(' + UpperCase(ClassN) + ')';
end;


function TField.GetData : JSValue;

begin
  IF FDataset=Nil then
    DatabaseErrorFmt(SNoDataset,[FieldName]);
  If FValidating then
    result:=FValueBuffer
  else
    Result:=FDataset.GetFieldData(Self);
end;

function TField.GetDataSize: Integer;

begin
  Result:=0;
end;

function TField.GetDefaultWidth: Longint;

begin
  Result:=10;
end;

function TField.GetDisplayName  : String;

begin
  If FDisplayLabel<>'' then
    result:=FDisplayLabel
  else
    Result:=FFieldName;
end;

function TField.IsDisplayLabelStored: Boolean;

begin
  Result:=(DisplayLabel<>FieldName);
end;

function TField.IsDisplayWidthStored: Boolean;

begin
  Result:=(FDisplayWidth<>0);
end;

function TField.GetLookupList: TLookupList;
begin
  if not Assigned(FLookupList) then
    FLookupList := TLookupList.Create;
  Result := FLookupList;
end;

procedure TField.CalcLookupValue;
begin
// MVC: TODO
//  if FLookupCache then
//    Value := LookupList.ValueOfKey(FDataSet.FieldValues[FKeyFields])
//  else if
  if Assigned(FLookupDataSet) and FDataSet.Active then
    Value := FLookupDataSet.Lookup(FLookupKeyfields, FDataSet.FieldValues[FKeyFields], FLookupresultField)
  else
    Value:=Null;
end;

function TField.GetIndex: longint;

begin
  If Assigned(FDataset) then
    Result:=FDataset.FFieldList.IndexOf(Self)
  else
    Result:=-1;
end;

function TField.GetLookup: Boolean;
begin
  Result := FieldKind = fkLookup;
end;

procedure TField.SetAlignment(const AValue: TAlignMent);
begin
  if FAlignment <> AValue then
    begin
    FAlignment := AValue;
    PropertyChanged(false);
    end;
end;

procedure TField.SetIndex(const AValue: Longint);
begin
  if FFields <> nil then FFields.SetFieldIndex(Self, AValue)
end;

function TField.GetIsNull: Boolean;

begin
  Result:=js.IsNull(GetData);
end;

function TField.GetParentComponent: TComponent;

begin
  Result := DataSet;
end;

procedure TField.GetText(var AText: string; ADisplayText: Boolean);

begin
  AText:=GetAsString;
end;

function TField.HasParent: Boolean;

begin
  HasParent:=True;
end;

function TField.IsValidChar(InputChar: Char): Boolean;

begin
  // FValidChars must be set in Create.
  Result:=CharInset(InputChar,FValidChars);
end;

procedure TField.RefreshLookupList;
var
  tmpActive: Boolean;
begin
  if not Assigned(FLookupDataSet) or (Length(FLookupKeyfields) = 0)
  or (Length(FLookupresultField) = 0) or (Length(FKeyFields) = 0) then
    Exit;
    
  tmpActive := FLookupDataSet.Active;
  try
    FLookupDataSet.Active := True;
    FFields.CheckFieldNames(FKeyFields);
    FLookupDataSet.Fields.CheckFieldNames(FLookupKeyFields);
    FLookupDataset.FieldByName(FLookupResultField); // I presume that if it doesn't exist it throws exception, and that a field with null value is still valid
    LookupList.Clear; // have to be F-less because we might be creating it here with getter!

    FLookupDataSet.DisableControls;
    try
      FLookupDataSet.First;
      while not FLookupDataSet.Eof do
      begin
//        FLookupList.Add(FLookupDataSet.FieldValues[FLookupKeyfields], FLookupDataSet.FieldValues[FLookupResultField]);
        FLookupDataSet.Next;
      end;
    finally
      FLookupDataSet.EnableControls;
    end;
  finally
    FLookupDataSet.Active := tmpActive;
  end;
end;

procedure TField.Notification(AComponent: TComponent; Operation: TOperation);

begin
  Inherited Notification(AComponent,Operation);
  if (Operation = opRemove) and (AComponent = FLookupDataSet) then
    FLookupDataSet := nil;
end;

procedure TField.PropertyChanged(LayoutAffected: Boolean);

begin
  If (FDataset<>Nil) and (FDataset.Active) then
    If LayoutAffected then
      FDataset.DataEvent(deLayoutChange,0)
    else
      FDataset.DataEvent(deDatasetchange,0);
end;


procedure TField.SetAsBytes(const AValue: TBytes);
begin
  RaiseAccessError(SBytes);
end;

procedure TField.SetAsBoolean(AValue: Boolean);

begin
  RaiseAccessError(SBoolean);
end;

procedure TField.SetAsDateTime(AValue: TDateTime);

begin
  RaiseAccessError(SDateTime);
end;

procedure TField.SetAsFloat(AValue: Double);

begin
  RaiseAccessError(SFloat);
end;

procedure TField.SetAsJSValue(const AValue: JSValue);

begin
  if js.IsNull(AValue) then
    Clear
  else
    try
      SetVarValue(AValue);
    except
      on EVariantError do
        DatabaseErrorFmt(SFieldValueError, [DisplayName]);
    end;
end;


procedure TField.SetAsLongint(AValue: Longint);
begin
  SetAsInteger(AValue);
end;

procedure TField.SetAsInteger(AValue: Longint);
begin
  RaiseAccessError(SInteger);
end;

procedure TField.SetAsLargeInt(AValue: NativeInt);
begin
  RaiseAccessError(SLargeInt);
end;

procedure TField.SetAsString(const AValue: string);
begin
  RaiseAccessError(SString);
end;

procedure TField.SetData(Buffer: JSValue);

begin
  If Not Assigned(FDataset) then
    DatabaseErrorFmt(SNoDataset,[FieldName]);
  FDataSet.SetFieldData(Self,Buffer);
end;

procedure TField.SetDataset(AValue: TDataset);

begin
{$ifdef dsdebug}
  Writeln ('Setting dataset');
{$endif}
  If AValue=FDataset then exit;
  If Assigned(FDataset) Then
    begin
    FDataset.CheckInactive;
    FDataset.FFieldList.Remove(Self);
    end;
  If Assigned(AValue) then
    begin
    AValue.CheckInactive;
    AValue.FFieldList.Add(Self);
    end;
  FDataset:=AValue;
end;

procedure TField.SetDataType(AValue: TFieldType);

begin
  FDataType := AValue;
end;

procedure TField.SetFieldType(AValue: TFieldType);

begin
  { empty }
end;

procedure TField.SetParentComponent(Value: TComponent);

begin
  if not (csLoading in ComponentState) then
    DataSet := Value as TDataSet;
end;

procedure TField.SetSize(AValue: Integer);

begin
  CheckInactive;
  CheckTypeSize(AValue);
  FSize:=AValue;
end;

procedure TField.SetText(const AValue: string);

begin
  SetAsString(AValue);
end;

procedure TField.SetVarValue(const AValue: JSValue);
begin
  RaiseAccessError(SJSValue);
end;

procedure TField.Validate(Buffer: Pointer);

begin
  If assigned(OnValidate) Then
    begin
    FValueBuffer:=Buffer;
    FValidating:=True;
    Try
      OnValidate(Self);
    finally
      FValidating:=False;
    end;
    end;
end;

class function TField.IsBlob: Boolean;

begin
  Result:=False;
end;

class procedure TField.CheckTypeSize(AValue: Longint);

begin
  If (AValue<>0) and Not IsBlob Then
    DatabaseErrorFmt(SInvalidFieldSize,[AValue]);
end;

// TField private methods

procedure TField.SetEditText(const AValue: string);
begin
  if Assigned(OnSetText) then
    OnSetText(Self, AValue)
  else
    SetText(AValue);
end;

function TField.GetEditText: String;
begin
  SetLength(Result, 0);
  if Assigned(OnGetText) then
    OnGetText(Self, Result, False)
  else
    GetText(Result, False);
end;

function TField.GetDisplayText: String;
begin
  SetLength(Result, 0);
  if Assigned(OnGetText) then
    OnGetText(Self, Result, True)
  else
    GetText(Result, True);
end;

procedure TField.SetDisplayLabel(const AValue: string);
begin
  if FDisplayLabel<>AValue then
    begin
    FDisplayLabel:=AValue;
    PropertyChanged(true);
    end;
end;

procedure TField.SetDisplayWidth(const AValue: Longint);
begin
  if FDisplayWidth<>AValue then
    begin
    FDisplayWidth:=AValue;
    PropertyChanged(True);
    end;
end;

function TField.GetDisplayWidth: integer;
begin
  if FDisplayWidth=0 then
    result:=GetDefaultWidth
  else
    result:=FDisplayWidth;
end;

procedure TField.SetLookup(const AValue: Boolean);
const
  ValueToLookupMap: array[Boolean] of TFieldKind = (fkData, fkLookup);
begin
  FieldKind := ValueToLookupMap[AValue];
end;

procedure TField.SetReadOnly(const AValue: Boolean);
begin
  if (FReadOnly<>AValue) then
    begin
    FReadOnly:=AValue;
    PropertyChanged(True);
    end;
end;

procedure TField.SetVisible(const AValue: Boolean);
begin
  if FVisible<>AValue then
    begin
    FVisible:=AValue;
    PropertyChanged(True);
    end;
end;


{ ---------------------------------------------------------------------
    TStringField
  ---------------------------------------------------------------------}


constructor TStringField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  SetDataType(ftString);
  FFixedChar := False;
  FTransliterate := False;
  FSize := 20;
end;

procedure TStringField.SetFieldType(AValue: TFieldType);
begin
  if AValue in [ftString, ftFixedChar] then
    SetDataType(AValue);
end;

class procedure TStringField.CheckTypeSize(AValue: Longint);

begin
// A size of 0 is allowed, since for example Firebird allows
// a query like: 'select '' as fieldname from table' which
// results in a string with size 0.
  If (AValue<0) Then
    DatabaseErrorFmt(SInvalidFieldSize,[AValue])
end;

function TStringField.GetAsBoolean: Boolean;

var S : String;

begin
  S:=GetAsString;
  result := (Length(S)>0) and (Upcase(S[1]) in ['T',YesNoChars[True]]);
end;

function TStringField.GetAsDateTime: TDateTime;

begin
  Result:=StrToDateTime(GetAsString);
end;

function TStringField.GetAsFloat: Double;

begin
  Result:=StrToFloat(GetAsString);
end;

function TStringField.GetAsInteger: Longint;

begin
  Result:=StrToInt(GetAsString);
end;

function TStringField.GetAsLargeInt: NativeInt;

begin
  Result:=StrToInt64(GetAsString);
end;

function TStringField.GetAsString: String;

Var
  V : JSValue;

begin
  V:=GetData;
  if isString(V) then
    Result := String(V)
  else
    Result:='';
end;


function TStringField.GetAsJSValue: JSValue;

begin
  Result:=GetData
end;


function TStringField.GetDefaultWidth: Longint;

begin
  result:=Size;
end;

procedure TStringField.GetText(var AText: string; ADisplayText: Boolean);

begin
    AText:=GetAsString;
end;


procedure TStringField.SetAsBoolean(AValue: Boolean);

begin
  If AValue Then
    SetAsString('T')
  else
    SetAsString('F');
end;

procedure TStringField.SetAsDateTime(AValue: TDateTime);

begin
  SetAsString(DateTimeToStr(AValue));
end;

procedure TStringField.SetAsFloat(AValue: Double);

begin
  SetAsString(FloatToStr(AValue));
end;

procedure TStringField.SetAsInteger(AValue: Longint);

begin
  SetAsString(IntToStr(AValue));
end;

procedure TStringField.SetAsLargeInt(AValue: NativeInt);

begin
  SetAsString(IntToStr(AValue));
end;


procedure TStringField.SetAsString(const AValue: String);
begin
  SetData(AValue);
end;


procedure TStringField.SetVarValue(const AValue: JSValue);
begin
  if isString(AVAlue) then
    SetAsString(String(AValue))
  else
    RaiseAccessError(SFieldValueError);
end;


{ ---------------------------------------------------------------------
    TNumericField
  ---------------------------------------------------------------------}


constructor TNumericField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  AlignMent:=taRightJustify;
end;

class procedure TNumericField.CheckTypeSize(AValue: Longint);
begin
  // This procedure is only added because some TDataset descendents have the
  // but that they set the Size property as if it is the DataSize property.
  // To avoid problems with those descendents, allow values <= 16.
  If (AValue>16) Then
    DatabaseErrorFmt(SInvalidFieldSize,[AValue]);
end;

procedure TNumericField.RangeError(AValue, Min, Max: Double);

begin
  DatabaseErrorFmt(SRangeError,[AValue,Min,Max,FieldName]);
end;

procedure TNumericField.SetDisplayFormat(const AValue: string);

begin
 If FDisplayFormat<>AValue then
   begin
   FDisplayFormat:=AValue;
   PropertyChanged(True);
   end;
end;

procedure TNumericField.SetEditFormat(const AValue: string);

begin
  If FEditFormat<>AValue then
    begin
    FEditFormat:=AValue;
    PropertyChanged(True);
    end;
end;

function TNumericField.GetAsBoolean: Boolean;
begin
  Result:=GetAsInteger<>0;
end;

procedure TNumericField.SetAsBoolean(AValue: Boolean);
begin
  SetAsInteger(ord(AValue));
end;

{ ---------------------------------------------------------------------
    TIntegerField
  ---------------------------------------------------------------------}


constructor TIntegerField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  SetDataType(ftInteger);
  FMinRange:=Low(LongInt);
  FMaxRange:=High(LongInt);
//  MVC : Todo
//  FValidchars:=['+','-','0'..'9'];
end;

function TIntegerField.GetAsFloat: Double;

begin
  Result:=GetAsInteger;
end;

function TIntegerField.GetAsLargeInt: NativeInt;
begin
  Result:=GetAsInteger;
end;

function TIntegerField.GetAsInteger: Longint;

begin
  If Not GetValue(Result) then
    Result:=0;
end;

function TIntegerField.GetAsJSValue: JSValue;

var L : Longint;

begin
  If GetValue(L) then
    Result:=L
  else
    Result:=Null;
end;

function TIntegerField.GetAsString: string;

var L : Longint;

begin
  If GetValue(L) then
    Result:=IntTostr(L)
  else
    Result:='';
end;


procedure TIntegerField.GetText(var AText: string; ADisplayText: Boolean);

var l : longint;
    fmt : string;

begin
  Atext:='';
  If Not GetValue(l) then exit;
  If ADisplayText or (FEditFormat='') then
    fmt:=FDisplayFormat
  else
    fmt:=FEditFormat;
  If length(fmt)<>0 then
    AText:=FormatFloat(fmt,L)
  else
    Str(L,AText);
end;

function TIntegerField.GetValue(var AValue: Longint): Boolean;

var
  V : JSValue;

begin
  V:=GetData;
  Result:=isInteger(V);
  if Result then
    AValue:=Longint(V);
end;

procedure TIntegerField.SetAsLargeInt(AValue: NativeInt);
begin
  if (AValue>=FMinRange) and (AValue<=FMaxRange) then
    SetAsInteger(AValue)
  else
    RangeError(AValue,FMinRange,FMaxRange);
end;

procedure TIntegerField.SetAsFloat(AValue: Double);

begin
  SetAsInteger(Round(AValue));
end;

procedure TIntegerField.SetAsInteger(AValue: Longint);
begin
  If CheckRange(AValue) then
    SetData(AValue)
  else
    if (FMinValue<>0) or (FMaxValue<>0) then
      RangeError(AValue,FMinValue,FMaxValue)
    else
      RangeError(AValue,FMinRange,FMaxRange);
end;

procedure TIntegerField.SetVarValue(const AValue: JSValue);
begin
  if IsInteger(aValue) then
    SetAsInteger(Integer(AValue))
  else
    RaiseAccessError(SInteger);
end;

procedure TIntegerField.SetAsString(const AValue: string);

var L,Code : longint;

begin
  If length(AValue)=0 then
    Clear
  else
    begin
    Val(AValue,L,Code);
    If Code=0 then
      SetAsInteger(L)
    else
      DatabaseErrorFmt(SNotAnInteger,[AValue]);
    end;
end;

Function TIntegerField.CheckRange(AValue : longint) : Boolean;

begin
  if (FMinValue<>0) or (FMaxValue<>0) then
    Result := (AValue>=FMinValue) and (AValue<=FMaxValue)
  else
    Result := (AValue>=FMinRange) and (AValue<=FMaxRange);
end;

Procedure TIntegerField.SetMaxValue (AValue : longint);

begin
  If (AValue>=FMinRange) and (AValue<=FMaxRange) then
    FMaxValue:=AValue
  else
    RangeError(AValue,FMinRange,FMaxRange);
end;

Procedure TIntegerField.SetMinValue (AValue : longint);

begin
  If (AValue>=FMinRange) and (AValue<=FMaxRange) then
    FMinValue:=AValue
  else
    RangeError(AValue,FMinRange,FMaxRange);
end;

{ ---------------------------------------------------------------------
    TLargeintField
  ---------------------------------------------------------------------}


constructor TLargeintField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  SetDataType(ftLargeint);
  FMinRange:=Low(NativeInt);
  FMaxRange:=High(NativeInt);
// MVC : Todo
//  FValidchars:=['+','-','0'..'9'];
end;

function TLargeintField.GetAsFloat: Double;

begin
  Result:=GetAsLargeInt;
end;

function TLargeintField.GetAsLargeInt: NativeInt;

begin
  If Not GetValue(Result) then
    Result:=0;
end;

function TLargeIntField.GetAsJSValue: JSValue;

var L : NativeInt;

begin
  If GetValue(L) then
    Result:=L
  else
    Result:=Null;
end;

function TLargeintField.GetAsInteger: Longint;

begin
  Result:=GetAsLargeInt;
end;

function TLargeintField.GetAsString: string;

var L : NativeInt;

begin
  If GetValue(L) then
    Result:=IntTostr(L)
  else
    Result:='';
end;

procedure TLargeintField.GetText(var AText: string; ADisplayText: Boolean);

var l : NativeInt;
    fmt : string;

begin
  Atext:='';
  If Not GetValue(l) then exit;
  If ADisplayText or (FEditFormat='') then
    fmt:=FDisplayFormat
  else
    fmt:=FEditFormat;
  If length(fmt)<>0 then
    AText:=FormatFloat(fmt,L)
  else
    Str(L,AText);
end;

function TLargeintField.GetValue(var AValue: NativeInt): Boolean;

var
  P : JSValue;

begin
  P:=GetData;
  Result:=isInteger(P);
  if Result then
    AValue:=NativeInt(P);
end;

procedure TLargeintField.SetAsFloat(AValue: Double);

begin
  SetAsLargeInt(Round(AValue));
end;

procedure TLargeintField.SetAsLargeInt(AValue: NativeInt);

begin
  If CheckRange(AValue) then
    SetData(AValue)
  else
    RangeError(AValue,FMinValue,FMaxValue);
end;

procedure TLargeintField.SetAsInteger(AValue: Longint);

begin
  SetAsLargeInt(AValue);
end;

procedure TLargeintField.SetAsString(const AValue: string);

var L     : NativeInt;
    code  : Longint;

begin
  If length(AValue)=0 then
    Clear
  else
    begin
    Val(AValue,L,Code);
    If Code=0 then
      SetAsLargeInt(L)
    else
      DatabaseErrorFmt(SNotAnInteger,[AValue]);
    end;
end;

procedure TLargeintField.SetVarValue(const AValue: JSValue);
begin
  if IsInteger(Avalue) then
    SetAsLargeInt(NativeInt(AValue))
  else
    RaiseAccessError(SLargeInt);
end;

Function TLargeintField.CheckRange(AValue : NativeInt) : Boolean;

begin
  if (FMinValue<>0) or (FMaxValue<>0) then
    Result := (AValue>=FMinValue) and (AValue<=FMaxValue)
  else
    Result := (AValue>=FMinRange) and (AValue<=FMaxRange);
end;

Procedure TLargeintField.SetMaxValue (AValue : NativeInt);

begin
  If (AValue>=FMinRange) and (AValue<=FMaxRange) then
    FMaxValue:=AValue
  else
    RangeError(AValue,FMinRange,FMaxRange);
end;

Procedure TLargeintField.SetMinValue (AValue : NativeInt);

begin
  If (AValue>=FMinRange) and (AValue<=FMaxRange) then
    FMinValue:=AValue
  else
    RangeError(AValue,FMinRange,FMaxRange);
end;


{ TAutoIncField }

constructor TAutoIncField.Create(AOwner: TComponent);

begin
  Inherited Create(AOWner);
  SetDataType(ftAutoInc);
end;

Procedure TAutoIncField.SetAsInteger(AValue: Longint);

begin
  // Some databases allows insertion of explicit values into identity columns
  // (some of them also allows (some not) updating identity columns)
  // So allow it at client side and leave check for server side
  //if not(FDataSet.State in [dsFilter,dsSetKey,dsInsert]) then
  //  DataBaseError(SCantSetAutoIncFields);
  inherited;
end;

{ TFloatField }

procedure TFloatField.SetCurrency(const AValue: Boolean);
begin
  if FCurrency=AValue then exit;
  FCurrency:=AValue;
end;

procedure TFloatField.SetPrecision(const AValue: Longint);
begin
  if (AValue = -1) or (AValue > 1) then
    FPrecision := AValue
  else
    FPrecision := 2;
end;

function TFloatField.GetAsFloat: Double;

Var
  P : JSValue;

begin
  P:=GetData;
  If IsNumber(P) then
    Result:=Double(P)
  else
    Result:=0.0;
end;

function TFloatField.GetAsJSValue: JSValue;

var
  P : JSValue;

begin
  P:=GetData;
  if IsNumber(P) then
    Result:=P
  else
    Result:=Null;
end;

function TFloatField.GetAsLargeInt: NativeInt;
begin
  Result:=Round(GetAsFloat);
end;

function TFloatField.GetAsInteger: Longint;

begin
  Result:=Round(GetAsFloat);
end;

function TFloatField.GetAsString: string;

var
  P : JSValue;

begin
  P:=GetData;
  if IsNumber(P) then
    Result:=FloatToStr(Double(P))
  else
    Result:='';
end;

procedure TFloatField.GetText(var AText: string; ADisplayText: Boolean);

Var
  fmt : string;
  E : Double;
  Digits : integer;
  ff: TFloatFormat;
  P : JSValue;

begin
  AText:='';
  P:=GetData;
  if Not IsNumber(P) then
    exit;
  E:=Double(P);
  If ADisplayText or (Length(FEditFormat) = 0) Then
    Fmt:=FDisplayFormat
  else
    Fmt:=FEditFormat;
    
  Digits := 0;
  if not FCurrency then
    ff := ffGeneral
  else
    begin
    Digits := 2;
    ff := ffFixed;
    end;


  If fmt<>'' then
    AText:=FormatFloat(fmt,E)
  else
    AText:=FloatToStrF(E,ff,FPrecision,Digits);
end;

procedure TFloatField.SetAsFloat(AValue: Double);

begin
  If CheckRange(AValue) then
    SetData(AValue)
  else
    RangeError(AValue,FMinValue,FMaxValue);
end;

procedure TFloatField.SetAsLargeInt(AValue: NativeInt);
begin
  SetAsFloat(AValue);
end;

procedure TFloatField.SetAsInteger(AValue: Longint);

begin
  SetAsFloat(AValue);
end;

procedure TFloatField.SetAsString(const AValue: string);

var f : Double;

begin
  If (AValue='') then
    Clear
  else  
    begin
    If not TryStrToFloat(AValue,F) then
      DatabaseErrorFmt(SNotAFloat, [AValue]);
    SetAsFloat(f);
    end;
end;

procedure TFloatField.SetVarValue(const AValue: JSValue);
begin
  if IsNumber(aValue) then
    SetAsFloat(Double(AValue))
  else
    RaiseAccessError('Float');
end;

constructor TFloatField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  SetDataType(ftFloat);
  FPrecision:=15;
// MVC
//  FValidChars := [DecimalSeparator, '+', '-', '0'..'9', 'E', 'e'];
end;

Function TFloatField.CheckRange(AValue : Double) : Boolean;

begin
  If (FMinValue<>0) or (FMaxValue<>0) then
    Result:=(AValue>=FMinValue) and (AValue<=FMaxValue)
  else
    Result:=True;
end;

{ TBooleanField }

function TBooleanField.GetAsBoolean: Boolean;

var
  P : JSValue;

begin
  P:=GetData;
  if isBoolean(P) then
    Result:=Boolean(P)
  else
    Result:=False;
end;

function TBooleanField.GetAsJSValue: JSValue;

var
  P : JSValue;

begin
  P:=GetData;
  if isBoolean(P) then
    Result:=Boolean(P)
  else
    Result:=Null;
end;

function TBooleanField.GetAsString: string;

var
  P : JSValue;

begin
  P:=GetData;
  if isBoolean(P) then
    Result:=FDisplays[False,Boolean(P)]
  else
    result:='';
end;

function TBooleanField.GetDefaultWidth: Longint;

begin
  Result:=Length(FDisplays[false,false]);
  If Result<Length(FDisplays[false,True]) then
    Result:=Length(FDisplays[false,True]);
end;

function TBooleanField.GetAsInteger: Longint;
begin
  Result := ord(GetAsBoolean);
end;

procedure TBooleanField.SetAsInteger(AValue: Longint);
begin
  SetAsBoolean(AValue<>0);
end;

procedure TBooleanField.SetAsBoolean(AValue: Boolean);

begin
  SetData(AValue);
end;

procedure TBooleanField.SetAsString(const AValue: string);

var Temp : string;

begin
  Temp:=UpperCase(AValue);
  if Temp='' then
    Clear
  else if pos(Temp, FDisplays[True,True])=1 then
    SetAsBoolean(True)
  else if pos(Temp, FDisplays[True,False])=1 then
    SetAsBoolean(False)
  else
    DatabaseErrorFmt(SNotABoolean,[AValue]);
end;

procedure TBooleanField.SetVarValue(const AValue: JSValue);
begin
  if isBoolean(aValue) then
    SetAsBoolean(Boolean(AValue))
  else if isNumber(aValue) then
    SetAsBoolean(Double(AValue)<>0)
end;

constructor TBooleanField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  SetDataType(ftBoolean);
  DisplayValues:='True;False';
end;

Procedure TBooleanField.SetDisplayValues(const AValue : String);

var I : longint;

begin
  If FDisplayValues<>AValue then
    begin
    I:=Pos(';',AValue);
    If (I<2) or (I=Length(AValue)) then
      DatabaseErrorFmt(SInvalidDisplayValues,[AValue]);
    FdisplayValues:=AValue;
    // Store display values and their uppercase equivalents;
    FDisplays[False,True]:=Copy(AValue,1,I-1);
    FDisplays[True,True]:=UpperCase(FDisplays[False,True]);
    FDisplays[False,False]:=Copy(AValue,I+1,Length(AValue)-i);
    FDisplays[True,False]:=UpperCase(FDisplays[False,False]);
    PropertyChanged(True);
    end;
end;

{ TDateTimeField }

procedure TDateTimeField.SetDisplayFormat(const AValue: string);
begin
  if FDisplayFormat<>AValue then begin
    FDisplayFormat:=AValue;
    PropertyChanged(True);
  end;
end;

function TDateTimeField.ConvertToDateTime(aValue: JSValue; aRaiseError: Boolean): TDateTime;
begin
  if Assigned(Dataset) then
    Result:=Dataset.ConvertToDateTime(aValue,aRaiseError)
  else
    Result:=TDataset.DefaultConvertToDateTime(aValue,aRaiseError);
end;

function TDateTimeField.DateTimeToNativeDateTime(aValue: TDateTime): JSValue;
begin
  if Assigned(Dataset) then
    Result:=Dataset.ConvertDateTimeToNative(aValue)
  else
    Result:=TDataset.DefaultConvertDateTimeToNative(aValue);
end;

function TDateTimeField.GetAsDateTime: TDateTime;

begin
  Result:=ConvertToDateTime(GetData,False);
end;

procedure TDateTimeField.SetVarValue(const AValue: JSValue);

begin
  SetAsDateTime(ConvertToDateTime(aValue,True));
end;

function TDateTimeField.GetAsJSValue: JSValue;

begin
  Result:=GetData;
  if Not isString(Result) then
    Result:=Null;
end;

function TDateTimeField.GetDataSize: Integer;
begin
  Result:=inherited GetDataSize;
end;

function TDateTimeField.GetAsFloat: Double;

begin
  Result:=GetAsdateTime;
end;


function TDateTimeField.GetAsString: string;

begin
  GetText(Result,False);
end;


Procedure TDateTimeField.GetText(var AText: string; ADisplayText: Boolean);

var
  R : TDateTime;
  F : String;

begin
  R:=ConvertToDateTime(GetData,false);
  If (R=0) then
    AText:=''
  else
    begin
    If (ADisplayText) and (Length(FDisplayFormat)<>0) then
      F:=FDisplayFormat
    else
      Case DataType of
       ftTime : F:=LongTimeFormat;
       ftDate : F:=ShortDateFormat;
      else
       F:='c'
      end;
    AText:=FormatDateTime(F,R);
    end;
end;


procedure TDateTimeField.SetAsDateTime(AValue: TDateTime);

begin
  SetData(DateTimeToNativeDateTime(aValue));
end;


procedure TDateTimeField.SetAsFloat(AValue: Double);

begin
  SetAsDateTime(AValue);
end;


procedure TDateTimeField.SetAsString(const AValue: string);

var R : TDateTime;

begin
  if AValue<>'' then
    begin
    R:=StrToDateTime(AValue);
    SetData(DateTimeToNativeDateTime(R));
    end
  else
    SetData(Null);
end;


constructor TDateTimeField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  SetDataType(ftDateTime);
end;


{ TDateField }

constructor TDateField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  SetDataType(ftDate);
end;


{ TTimeField }

constructor TTimeField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  SetDataType(ftTime);
end;

procedure TTimeField.SetAsString(const AValue: string);

var
  R : TDateTime;

begin
  if AValue<>'' then
    begin
    R:=StrToTime(AValue);
    SetData(DateTimeToNativeDateTime(R));
    end
  else
    SetData(Null);
end;



{ TBinaryField }

class procedure TBinaryField.CheckTypeSize(AValue: Longint);

begin
  // Just check for really invalid stuff; actual size is
  // dependent on the record...
  If AValue<1 then
    DatabaseErrorFmt(SInvalidFieldSize,[AValue]);
end;

Function TBinaryField.BlobToBytes(aValue : JSValue) : TBytes;

begin
  if Assigned(Dataset) then
    Result:=DataSet.BlobDataToBytes(aValue)
  else
    Result:=TDataSet.DefaultBlobDataToBytes(aValue)
end;

Function TBinaryField.BytesToBlob(aValue : TBytes) : JSValue;

begin
  if Assigned(Dataset) then
    Result:=DataSet.BytesToBlobData(aValue)
  else
    Result:=TDataSet.DefaultBytesToBlobData(aValue)
end;

function TBinaryField.GetAsString: string;

var
  V : JSValue;
  S : TBytes;
  I : Integer;

begin
  Result := '';
  V:=GetData;
  if V<>Null then
    begin
    S:=BlobToBytes(V);
    For I:=0 to Length(S) do
       TJSString(Result).Concat(TJSString.fromCharCode(S[I]));
    end;
end;


function TBinaryField.GetAsJSValue: JSValue;

begin
  Result:=GetData;
end;


function TBinaryField.GetValue(var AValue: TBytes): Boolean;
var
  V : JSValue;
begin
  V:=GetData;
  Result:=(V<>Null);
  if Result then
    AValue:=BlobToBytes(V)
  else
    SetLength(AValue,0);
end;



procedure TBinaryField.SetAsString(const AValue: string);

var
  B : TBytes;
  i : Integer;

begin
  SetLength(B, Length(aValue));
  For I:=1 to Length(aValue) do
    B[i-1]:=Ord(aValue[i]);
  SetAsBytes(B);
end;


procedure TBinaryField.SetVarValue(const AValue: JSValue);

var
  B: TBytes;
  I,Len: integer;

begin
  if IsArray(AValue) then
    begin
    Len:=Length(TJSValueDynArray(AValue));
    SetLength(B, Len);
    For I:=1 to Len-1 do
      B[i]:=TBytes(AValue)[i];
    SetAsBytes(B);
    end
  else if IsString(AValue) then
    SetAsString(String(AValue))
  else
    RaiseAccessError('Blob');
end;


constructor TBinaryField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
end;



{ TBlobField }

constructor TBlobField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  SetDataType(ftBlob);
end;

procedure TBlobField.Clear;
begin
  SetData(Null);
end;

(*
function TBlobField.GetBlobType: TBlobType;
begin
  Result:=ftBlob;
end;

procedure TBlobField.SetBlobType(AValue: TBlobType);
begin
  SetFieldType(TFieldType(AValue));
end;
*)




function TBlobField.GetBlobSize: Longint;

var
  B : TBytes;

begin
  B:=GetAsBytes;
  Result:=Length(B);
end;


function TBlobField.GetIsNull: Boolean;

begin
  if Not Modified then
    Result:= inherited GetIsNull
  else
    Result:=GetBlobSize=0;
end;

procedure TBlobField.GetText(var AText: string; ADisplayText: Boolean);
begin
  AText := inherited GetAsString;
end;

class function TBlobField.IsBlob: Boolean;

begin
  Result:=True;
end;

procedure TBlobField.SetFieldType(AValue: TFieldType);
begin
  if AValue in ftBlobTypes then
    SetDataType(AValue);
end;

{ TMemoField }

constructor TMemoField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftMemo);
end;



{ TVariantField }

constructor TVariantField.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SetDataType(ftVariant);
end;

class procedure TVariantField.CheckTypeSize(aValue: Integer);
begin
  { empty }
end;

function TVariantField.GetAsBoolean: Boolean;
begin
  Result :=GetAsJSValue=True;
end;

function TVariantField.GetAsDateTime: TDateTime;

Var
  V : JSValue;

begin
  V:=GetData;
  if Assigned(Dataset) then
    Result:=Dataset.ConvertToDateTime(V,True)
  else
    Result:=TDataset.DefaultConvertToDateTime(V,True)
end;

function TVariantField.GetAsFloat: Double;

Var
  V : JSValue;

begin
  V:=GetData;
  if isNumber(V) then
    Result:=Double(V)
  else if isString(V) then
    Result:=parsefloat(String(V))
  else
    RaiseAccessError('Variant');
end;

function TVariantField.GetAsInteger: Longint;
Var
  V : JSValue;

begin
  V:=GetData;
  if isInteger(V) then
    Result:=Integer(V)
  else if isString(V) then
    Result:=parseInt(String(V))
  else
    RaiseAccessError('Variant');
end;

function TVariantField.GetAsString: string;
Var
  V : JSValue;

begin
  V:=GetData;
  if isInteger(V) then
    Result:=IntToStr(Integer(V))
  else if isNumber(V) then
      Result:=FloatToStr(Double(V))
  else if isString(V) then
    Result:=String(V)
  else
    RaiseAccessError('Variant');
end;


function TVariantField.GetAsJSValue: JSValue;
begin
  Result:=GetData;
end;

procedure TVariantField.SetAsBoolean(aValue: Boolean);
begin
  SetVarValue(aValue);
end;

procedure TVariantField.SetAsDateTime(aValue: TDateTime);
begin
  SetVarValue(aValue);
end;

procedure TVariantField.SetAsFloat(aValue: Double);
begin
  SetVarValue(aValue);
end;

procedure TVariantField.SetAsInteger(AValue: Longint);
begin
  SetVarValue(aValue);
end;

procedure TVariantField.SetAsString(const aValue: string);
begin
  SetVarValue(aValue);
end;

procedure TVariantField.SetVarValue(const aValue: JSValue);
begin
  SetData(aValue);
end;

{ TFieldsEnumerator }

function TFieldsEnumerator.GetCurrent: TField;
begin
  Result := FFields[FPosition];
end;

constructor TFieldsEnumerator.Create(AFields: TFields);
begin
  inherited Create;
  FFields := AFields;
  FPosition := -1;
end;

function TFieldsEnumerator.MoveNext: Boolean;
begin
  inc(FPosition);
  Result := FPosition < FFields.Count;
end;

{ TFields }

constructor TFields.Create(ADataset: TDataset);

begin
  FDataSet:=ADataset;
  FFieldList:=TFpList.Create;
  FValidFieldKinds:=[fkData..fkInternalcalc];
end;

destructor TFields.Destroy;

begin
  if Assigned(FFieldList) then
    Clear;
  FreeAndNil(FFieldList);
  inherited Destroy;
end;

procedure TFields.ClearFieldDefs;

Var
  i : Integer;

begin
  For I:=0 to Count-1 do
    Fields[i].FFieldDef:=Nil;
end;

procedure TFields.Changed;

begin
  // Removed FDataSet.Active check, needed for Persistent fields (see bug ID 30954)
  if (FDataSet <> nil) and not (csDestroying in FDataSet.ComponentState) then
    FDataSet.DataEvent(deFieldListChange, 0);
  If Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TFields.CheckfieldKind(Fieldkind: TFieldKind; Field: TField);

begin
  If Not (FieldKind in ValidFieldKinds) Then
    DatabaseErrorFmt(SInvalidFieldKind,[Field.FieldName]);
end;

function TFields.GetCount: Longint;

begin
  Result:=FFieldList.Count;
end;


function TFields.GetField(Index: Integer): TField;

begin
  Result:=Tfield(FFieldList[Index]);
end;

procedure TFields.SetField(Index: Integer; Value: TField);
begin
  Fields[Index].Assign(Value);
end;

procedure TFields.SetFieldIndex(Field: TField; Value: Integer);
var Old : Longint;
begin
  Old := FFieldList.indexOf(Field);
  If Old=-1 then
    Exit;
  // Check value
  If Value<0 Then Value:=0;
  If Value>=Count then Value:=Count-1;
  If Value<>Old then
    begin
    FFieldList.Delete(Old);
    FFieldList.Insert(Value,Field);
    Field.PropertyChanged(True);
    Changed;
    end;
end;

procedure TFields.Add(Field: TField);

begin
  CheckFieldName(Field.FieldName);
  FFieldList.Add(Field);
  Field.FFields:=Self;
  Changed;
end;

procedure TFields.CheckFieldName(const Value: String);

begin
  If FindField(Value)<>Nil then
    DataBaseErrorFmt(SDuplicateFieldName,[Value],FDataset);
end;

procedure TFields.CheckFieldNames(const Value: String);

var
  N: String;
  StrPos: Integer;

begin
  if Value = '' then
    Exit;
  StrPos := 1;
  repeat
    N := ExtractFieldName(Value, StrPos);
    // Will raise an error if no such field...
    FieldByName(N);
  until StrPos > Length(Value);
end;

procedure TFields.Clear;
var
  AField: TField;
begin
  while FFieldList.Count > 0 do 
    begin
    AField := TField(FFieldList.Last);
    AField.FDataSet := Nil;
    AField.Free;
    FFieldList.Delete(FFieldList.Count - 1);
    end;
  Changed;
end;

function TFields.FindField(const Value: String): TField;
var S : String;
    I : longint;
begin
  S:=UpperCase(Value);
  For I:=0 To FFieldList.Count-1 do
  begin
    Result:=TField(FFieldList[I]);
    if S=UpperCase(Result.FieldName) then
    begin
      {$ifdef dsdebug}
      Writeln ('Found field ',Value);
      {$endif}
      Exit;
    end;
  end;
  Result:=Nil;
end;

function TFields.FieldByName(const Value: String): TField;

begin
  Result:=FindField(Value);
  If result=Nil then
    DatabaseErrorFmt(SFieldNotFound,[Value],FDataset);
end;

function TFields.FieldByNumber(FieldNo: Integer): TField;
var i : Longint;
begin
  For I:=0 to FFieldList.Count-1 do
  begin
    Result:=TField(FFieldList[I]);
    if FieldNo=Result.FieldNo then
      Exit;
  end;
  Result:=Nil;
end;

function TFields.GetEnumerator: TFieldsEnumerator;

begin
  Result:=TFieldsEnumerator.Create(Self);
end;

procedure TFields.GetFieldNames(Values: TStrings);
var i : longint;
begin
  Values.Clear;
  For I:=0 to FFieldList.Count-1 do
    Values.Add(Tfield(FFieldList[I]).FieldName);
end;

function TFields.IndexOf(Field: TField): Longint;

begin
  Result:=FFieldList.IndexOf(Field);
end;

procedure TFields.Remove(Value : TField);

begin
  FFieldList.Remove(Value);
  Value.FFields := nil;
  Changed;
end;

{ ---------------------------------------------------------------------
    TDatalink
  ---------------------------------------------------------------------}

Constructor TDataLink.Create;

begin
  Inherited Create;
  FBufferCount:=1;
  FFirstRecord := 0;
  FDataSource := nil;
  FDatasourceFixed:=False;
end;


Destructor TDataLink.Destroy;

begin
  Factive:=False;
  FEditing:=False;
  FDataSourceFixed:=False;
  DataSource:=Nil;
  Inherited Destroy;
end;


Procedure TDataLink.ActiveChanged;

begin
  FFirstRecord := 0;
end;

Procedure TDataLink.CheckActiveAndEditing;

Var
  B : Boolean;

begin
  B:=Assigned(DataSource) and Not (DataSource.State in [dsInactive,dsOpening]);
  If B<>FActive then
    begin
    FActive:=B;
    ActiveChanged;
    end;
  B:=Assigned(DataSource) and (DataSource.State in dsEditModes) and Not FReadOnly;
  If B<>FEditing Then
    begin
    FEditing:=B;
    EditingChanged;
    end;
end;


Procedure TDataLink.CheckBrowseMode;

begin
end;


Function TDataLink.CalcFirstRecord(Index : Integer) : Integer;
begin
  if DataSource.DataSet.FActiveRecord > FFirstRecord + Index + FBufferCount - 1 then
    Result := DataSource.DataSet.FActiveRecord - (FFirstRecord + Index + FBufferCount - 1)
  else if DataSource.DataSet.FActiveRecord < FFirstRecord + Index then
    Result := DataSource.DataSet.FActiveRecord - (FFirstRecord + Index)
  else Result := 0;
  
  Inc(FFirstRecord, Index + Result);
end;


Procedure TDataLink.CalcRange;
var
    aMax, aMin: integer;
begin
  aMin:= DataSet.FActiveRecord - FBufferCount + 1;
  If aMin < 0 Then aMin:= 0;
  aMax:= Dataset.FBufferCount - FBufferCount;
  If aMax < 0 then aMax:= 0;

  If aMax>DataSet.FActiveRecord Then aMax:=DataSet.FActiveRecord;
  If FFirstRecord < aMin Then FFirstRecord:= aMin;
  If FFirstrecord > aMax Then FFirstRecord:= aMax;

  If (FfirstRecord<>0) And
     (DataSet.FActiveRecord - FFirstRecord < FBufferCount -1) Then
    Dec(FFirstRecord, 1);

end;


Procedure TDataLink.DataEvent(Event: TDataEvent; Info: JSValue);


begin
  Case Event of
    deFieldChange, deRecordChange:
      If Not FUpdatingRecord then
        RecordChanged(TField(Info));
    deDataSetChange: begin
      SetActive(DataSource.DataSet.Active);
      CalcRange;
      CalcFirstRecord(Integer(Info));
      DatasetChanged;
    end;
    deDataSetScroll: DatasetScrolled(CalcFirstRecord(Integer(Info)));
    deLayoutChange: begin
      CalcFirstRecord(Integer(Info));
      LayoutChanged;
    end;
    deUpdateRecord: UpdateRecord;
    deUpdateState: CheckActiveAndEditing;
    deCheckBrowseMode: CheckBrowseMode;
    deFocusControl:
      FocusControl(Info);
  end;
end;


Procedure TDataLink.DataSetChanged;

begin
  RecordChanged(Nil);
end;


Procedure TDataLink.DataSetScrolled(Distance: Integer);

begin
  DataSetChanged;
end;


Procedure TDataLink.EditingChanged;

begin
end;


Procedure TDataLink.FocusControl(Field: JSValue);

begin
end;


Function TDataLink.GetActiveRecord: Integer;

begin
  Result:=Dataset.FActiveRecord - FFirstRecord;
end;

Function TDatalink.GetDataSet : TDataset;

begin
  If Assigned(Datasource) then
    Result:=DataSource.DataSet
  else
    Result:=Nil;  
end;


Function TDataLink.GetBOF: Boolean;

begin
  Result:=DataSet.BOF
end;


Function TDataLink.GetBufferCount: Integer;

begin
  Result:=FBufferCount;
end;


Function TDataLink.GetEOF: Boolean;

begin
  Result:=DataSet.EOF
end;


Function TDataLink.GetRecordCount: Integer;

begin
  Result:=Dataset.FRecordCount;
  If Result>BufferCount then
    Result:=BufferCount;
end;


Procedure TDataLink.LayoutChanged;

begin
  DataSetChanged;
end;


Function TDataLink.MoveBy(Distance: Integer): Integer;

begin
  Result:=DataSet.MoveBy(Distance);
end;


Procedure TDataLink.RecordChanged(Field: TField);

begin
end;


Procedure TDataLink.SetActiveRecord(Value: Integer);

begin
{$ifdef dsdebug}
  Writeln('Datalink. Setting active record to ',Value,' with firstrecord ',ffirstrecord);
{$endif}
  Dataset.FActiveRecord:=Value + FFirstRecord;
end;


Procedure TDataLink.SetBufferCount(Value: Integer);

begin
  If FBufferCount<>Value then
    begin
      FBufferCount:=Value;
      if Active then begin
        DataSet.RecalcBufListSize;
        CalcRange;
      end;
    end;
end;

procedure TDataLink.SetActive(AActive: Boolean);
begin
  if Active <> AActive then
  begin
    FActive := AActive;
    // !!!: Set internal state
    ActiveChanged;
  end;
end;

Procedure TDataLink.SetDataSource(Value : TDatasource);

begin
  if FDataSource = Value then
    Exit;
  if not FDataSourceFixed then
    begin
    if Assigned(DataSource) then
      Begin
      DataSource.UnregisterDatalink(Self);
      FDataSource := nil;
      CheckActiveAndEditing;
      End;
    FDataSource := Value;
    if Assigned(DataSource) then
      begin
      DataSource.RegisterDatalink(Self);
      CheckActiveAndEditing;
      End;
    end;
end;

Procedure TDatalink.SetReadOnly(Value : Boolean);

begin
  If FReadOnly<>Value then
    begin
    FReadOnly:=Value;
    CheckActiveAndEditing;
    end;
end;

Procedure TDataLink.UpdateData;

begin
end;



Function TDataLink.Edit: Boolean;

begin
  If Not FReadOnly then
    DataSource.Edit;
  // Triggered event will set FEditing
  Result:=FEditing;
end;


Procedure TDataLink.UpdateRecord;

begin
  FUpdatingRecord:=True;
  Try
    UpdateData;
  finally
    FUpdatingRecord:=False;
  end;
end;


{ ---------------------------------------------------------------------
    TDetailDataLink
  ---------------------------------------------------------------------}

Function TDetailDataLink.GetDetailDataSet: TDataSet;

begin
  Result := nil;
end;


{ ---------------------------------------------------------------------
    TMasterDataLink
  ---------------------------------------------------------------------}

constructor TMasterDataLink.Create(ADataSet: TDataSet);

begin
  inherited Create;
  FDetailDataSet:=ADataSet;
  FFields:=TList.Create;
end;


destructor TMasterDataLink.Destroy;

begin
  FFields.Free;
  inherited Destroy;
end;


Procedure TMasterDataLink.ActiveChanged;

begin
  FFields.Clear;
  if Active then
    try
      DataSet.GetFieldList(FFields, FFieldNames);
    except
      FFields.Clear;
      raise;
    end;
  if FDetailDataSet.Active and not (csDestroying in FDetailDataSet.ComponentState) then
    if Active and (FFields.Count > 0) then
      DoMasterChange
    else
      DoMasterDisable;  
end;


Procedure TMasterDataLink.CheckBrowseMode;

begin
  if FDetailDataSet.Active then FDetailDataSet.CheckBrowseMode;
end;


Function TMasterDataLink.GetDetailDataSet: TDataSet;

begin
  Result := FDetailDataSet;
end;


Procedure TMasterDataLink.LayoutChanged;

begin
  ActiveChanged;
end;


Procedure TMasterDataLink.RecordChanged(Field: TField);

begin
  if (DataSource.State <> dsSetKey) and FDetailDataSet.Active and
     (FFields.Count > 0) and ((Field = nil) or
     (FFields.IndexOf(Field) >= 0)) then
    DoMasterChange;  
end;

procedure TMasterDatalink.SetFieldNames(const Value: string);

begin
  if FFieldNames <> Value then
    begin
    FFieldNames := Value;
    ActiveChanged;
    end;
end;

Procedure TMasterDataLink.DoMasterDisable; 

begin
  if Assigned(FOnMasterDisable) then 
    FOnMasterDisable(Self);
end;

Procedure TMasterDataLink.DoMasterChange; 

begin
  If Assigned(FOnMasterChange) then
    FOnMasterChange(Self);
end;

{ ---------------------------------------------------------------------
    TMasterParamsDataLink
  ---------------------------------------------------------------------}

constructor TMasterParamsDataLink.Create(ADataSet: TDataSet);

Var
  P : TParams;

begin
  inherited Create(ADataset);
  If (ADataset<>Nil) then
    begin
    P:=TParams(GetObjectProp(ADataset,'Params',TParams));
    if (P<>Nil) then
      Params:=P;
    end;  
end;


Procedure TMasterParamsDataLink.SetParams(AValue : TParams);

begin
  FParams:=AValue;
  If (AValue<>Nil) then
    RefreshParamNames;
end;

Procedure TMasterParamsDataLink.RefreshParamNames; 

Var
  FN : String;
  DS : TDataset;
  F  : TField;
  I : Integer;
  P : TParam;


begin
  FN:='';
  DS:=Dataset;
  If Assigned(FParams) then
    begin
    F:=Nil;
    For I:=0 to FParams.Count-1 do
      begin
      P:=FParams[i];
      if not P.Bound then
        begin
        If Assigned(DS) then
          F:=DS.FindField(P.Name);
        If (Not Assigned(DS)) or (not DS.Active) or (F<>Nil) then
          begin
          If (FN<>'') then
            FN:=FN+';';
          FN:=FN+P.Name;
          end;
        end;
      end;
    end;
  FieldNames:=FN;  
end;

Procedure TMasterParamsDataLink.CopyParamsFromMaster(CopyBound : Boolean);

begin
  if Assigned(FParams) then
    FParams.CopyParamValuesFromDataset(Dataset,CopyBound);
end;

Procedure TMasterParamsDataLink.DoMasterDisable; 

begin
  Inherited;
  // If master dataset is closing, leave detail dataset intact (Delphi compatible behavior)
  // If master dataset is reopened, relationship will be reestablished
end;

Procedure TMasterParamsDataLink.DoMasterChange; 

begin
  Inherited;
  if Assigned(Params) and Assigned(DetailDataset) and DetailDataset.Active then
    begin
    DetailDataSet.CheckBrowseMode;
    DetailDataset.Close;
    DetailDataset.Open;
    end;
end;

{ ---------------------------------------------------------------------
    TDatasource
  ---------------------------------------------------------------------}

Constructor TDataSource.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  FDatalinks := TList.Create;
  FEnabled := True;
  FAutoEdit := True;
end;


Destructor TDataSource.Destroy;

begin
  FOnStateCHange:=Nil;
  Dataset:=Nil;
  With FDataLinks do
    While Count>0 do
      TDatalink(Items[Count - 1]).DataSource:=Nil;
  FDatalinks.Free;
  inherited Destroy;
end;


Procedure TDatasource.Edit;

begin
  If (State=dsBrowse) and AutoEdit Then
    Dataset.Edit;
end;


Function TDataSource.IsLinkedTo(ADataSet: TDataSet): Boolean;

begin
  Result:=False;
end;


procedure TDatasource.DistributeEvent(Event: TDataEvent; Info: JSValue);


Var
  i : Longint;

begin
  With FDatalinks do
    begin
    For I:=0 to Count-1 do
      With TDatalink(Items[i]) do
        If Not VisualControl Then
          DataEvent(Event,Info);
    For I:=0 to Count-1 do
      With TDatalink(Items[i]) do
        If VisualControl Then
          DataEvent(Event,Info);
    end;
end;

procedure TDatasource.RegisterDataLink(DataLink: TDataLink);

begin
  FDatalinks.Add(DataLink);
  if Assigned(DataSet) then
    DataSet.RecalcBufListSize;
end;


procedure TDatasource.SetDataSet(ADataSet: TDataSet);
begin
  If FDataset<>Nil Then
    Begin
    FDataset.UnRegisterDataSource(Self);
    FDataSet:=nil;
    ProcessEvent(deUpdateState,0);
    End;
  If ADataset<>Nil Then
    begin
    ADataset.RegisterDatasource(Self);
    FDataSet:=ADataset;
    ProcessEvent(deUpdateState,0);
    End;
end;


procedure TDatasource.SetEnabled(Value: Boolean);

begin
  FEnabled:=Value;
end;


Procedure TDatasource.DoDataChange (Info : Pointer);

begin
  If Assigned(OnDataChange) Then
    OnDataChange(Self,TField(Info));
end;

Procedure TDatasource.DoStateChange;

begin
  If Assigned(OnStateChange) Then
    OnStateChange(Self);
end;


Procedure TDatasource.DoUpdateData;

begin
  If Assigned(OnUpdateData) Then
    OnUpdateData(Self);
end;


procedure TDatasource.UnregisterDataLink(DataLink: TDataLink);

begin
  FDatalinks.Remove(Datalink);
  If Dataset<>Nil then
    DataSet.RecalcBufListSize;
  //Dataset.SetBufListSize(DataLink.BufferCount);
end;


procedure TDataSource.ProcessEvent(Event : TDataEvent; Info : JSValue);

Const
    OnDataChangeEvents = [deRecordChange, deDataSetChange, deDataSetScroll,
                          deLayoutChange,deUpdateState];

Var
  NeedDataChange : Boolean;
  FLastState : TdataSetState;

begin
  // Special UpdateState handling.
  If Event=deUpdateState then
    begin
    NeedDataChange:=(FState=dsInactive);
    FLastState:=FState;
    If Assigned(Dataset) then
      FState:=Dataset.State
    else
      FState:=dsInactive;
    // Don't do events if nothing changed.
    If FState=FLastState then
      exit;
    end
  else
    NeedDataChange:=True;
  DistributeEvent(Event,Info);
  // Extra handlers
  If Not (csDestroying in ComponentState) then
    begin
    If (Event=deUpdateState) then
      DoStateChange;
    If (Event in OnDataChangeEvents) and
       NeedDataChange Then
      DoDataChange(Nil);
    If (Event = deFieldChange) Then
      DoDataCHange(Pointer(Info));
    If (Event=deUpdateRecord) then
      DoUpdateData;
    end;
 end;


procedure SkipQuotesString(S : String; var p : integer; QuoteChar : char; EscapeSlash, EscapeRepeat : Boolean);

var notRepeatEscaped : boolean;
begin
  Inc(p);
  repeat
    notRepeatEscaped := True;
    while not CharInSet(S[p],[#0, QuoteChar]) do
      begin
      if EscapeSlash and (S[p]='\') and (P<Length(S)) then
        Inc(p,2) // make sure we handle \' and \\ correct
      else
        Inc(p);
      end;
    if S[p]=QuoteChar then
      begin
      Inc(p); // skip final '
      if (S[p]=QuoteChar) and EscapeRepeat then // Handle escaping by ''
        begin
        notRepeatEscaped := False;
        inc(p);
        end
      end;
  until notRepeatEscaped;
end;


{ TParams }

Function TParams.GetItem(Index: Integer): TParam;
begin
  Result:=(Inherited GetItem(Index)) as TParam;
end;

Function TParams.GetParamValue(const ParamName: string): JSValue;
begin
  Result:=ParamByName(ParamName).Value;
end;

Procedure TParams.SetItem(Index: Integer; Value: TParam);
begin
  Inherited SetItem(Index,Value);
end;

Procedure TParams.SetParamValue(const ParamName: string; const Value: JSValue);
begin
  ParamByName(ParamName).Value:=Value;
end;

Procedure TParams.AssignTo(Dest: TPersistent);
begin
 if (Dest is TParams) then
   TParams(Dest).Assign(Self)
 else
   inherited AssignTo(Dest);
end;

Function TParams.GetDataSet: TDataSet;
begin
  If (FOwner is TDataset) Then
    Result:=TDataset(FOwner)
  else
    Result:=Nil;
end;

Function TParams.GetOwner: TPersistent;
begin
  Result:=FOwner;
end;

Class Function TParams.ParamClass: TParamClass;
begin
  Result:=TParam;
end;

Constructor TParams.Create(AOwner: TPersistent; AItemClass: TCollectionItemClass
  );
begin
  Inherited Create(AItemClass);
  FOwner:=AOwner;
end;


Constructor TParams.Create(AOwner: TPersistent);
begin
  Create(AOwner,ParamClass);
end;

Constructor TParams.Create;
begin
  Create(Nil);
end;

Procedure TParams.AddParam(Value: TParam);
begin
  Value.Collection:=Self;
end;

Procedure TParams.AssignValues(Value: TParams);

Var
  I : Integer;
  P,PS : TParam;

begin
  For I:=0 to Value.Count-1 do
    begin
    PS:=Value[i];
    P:=FindParam(PS.Name);
    If Assigned(P) then
      P.Assign(PS);
    end;
end;

Function TParams.CreateParam(FldType: TFieldType; const ParamName: string;
  ParamType: TParamType): TParam;

begin
  Result:=Add as TParam;
  Result.Name:=ParamName;
  Result.DataType:=FldType;
  Result.ParamType:=ParamType;
end;

Function TParams.FindParam(const Value: string): TParam;

Var
  I : Integer;

begin
  Result:=Nil;
  I:=Count-1;
  While (Result=Nil) and (I>=0) do
    If (CompareText(Value,Items[i].Name)=0) then
      Result:=Items[i]
    else
      Dec(i);
end;

Procedure TParams.GetParamList(List: TList; const ParamNames: string);

Var
  P: TParam;
  N: String;
  StrPos: Integer;

begin
  if (ParamNames = '') or (List = nil) then
    Exit;
  StrPos := 1;
  repeat
    N := ExtractFieldName(ParamNames, StrPos);
    P := ParamByName(N);
    List.Add(P);
  until StrPos > Length(ParamNames);
end;

Function TParams.IsEqual(Value: TParams): Boolean;

Var
  I : Integer;

begin
  Result:=(Value.Count=Count);
  I:=Count-1;
  While Result and (I>=0) do
    begin
    Result:=Items[I].IsEqual(Value[i]);
    Dec(I);
    end;
end;

Function TParams.ParamByName(const Value: string): TParam;
begin
  Result:=FindParam(Value);
  If (Result=Nil) then
    DatabaseErrorFmt(SParameterNotFound,[Value],Dataset);
end;

Function TParams.ParseSQL(SQL: String; DoCreate: Boolean): String;

var pb : TParamBinding;
    rs : string;

begin
  Result := ParseSQL(SQL,DoCreate,True,True,psInterbase, pb, rs);
end;

Function TParams.ParseSQL(SQL: String; DoCreate, EscapeSlash,
  EscapeRepeat: Boolean; ParameterStyle: TParamStyle): String;

var pb : TParamBinding;
    rs : string;

begin
  Result := ParseSQL(SQL,DoCreate,EscapeSlash,EscapeRepeat,ParameterStyle,pb, rs);
end;

Function TParams.ParseSQL(SQL: String; DoCreate, EscapeSlash,
  EscapeRepeat: Boolean; ParameterStyle: TParamStyle; out
  ParamBinding: TParambinding): String;

var rs : string;

begin
  Result := ParseSQL(SQL,DoCreate,EscapeSlash, EscapeRepeat, ParameterStyle,ParamBinding, rs);
end;

function SkipComments(S : String; Var p: Integer; EscapeSlash, EscapeRepeat : Boolean) : Boolean;

begin
  Result := False;
  case S[P] of
  '''', '"', '`':
    begin
    Result := True;
    // single quote, double quote or backtick delimited string
    SkipQuotesString(S,p, S[p], EscapeSlash, EscapeRepeat);
    end;
  '-': // possible start of -- comment
    begin
    Inc(p);
    if S[p]='-' then // -- comment
      begin
      Result := True;
      repeat // skip until at end of line
        Inc(p);
      until CharInset(S[p],[#10, #13, #0]);
      while CharInSet(S[p],[#10, #13]) do
        Inc(p); // newline is part of comment
      end;
    end;
  '/': // possible start of /* */ comment
    begin
    Inc(p);
    if S[p]='*' then // /* */ comment
      begin
      Result := True;
      Inc(p);
      while p<=Length(S) do
        begin
        if S[p]='*' then // possible end of comment
          begin
          Inc(p);
          if S[p]='/' then Break; // end of comment
          end
        else
          Inc(p);
        end;
      if (P<=Length(s)) and (S[p]='/') then
        Inc(p); // skip final /
      end;
    end;
  end; {case}
end;

Function TParams.ParseSQL(SQL: String; DoCreate, EscapeSlash,
  EscapeRepeat: Boolean; ParameterStyle: TParamStyle; out
  ParamBinding: TParambinding; out ReplaceString: string): String;

type
  // used for ParamPart
  TStringPart = record
    Start,Stop:integer;
  end;

const
  ParamAllocStepSize = 8;
  PAramDelimiters : Array of char = (';',',',' ','(',')',#13,#10,#9,#0,'=','+','-','*','\','/','[',']','|');

var
  IgnorePart:boolean;
  p,ParamNameStart,BufStart:Integer;
  ParamName:string;
  QuestionMarkParamCount,ParameterIndex,NewLength:integer;
  ParamCount:integer; // actual number of parameters encountered so far;
                      // always <= Length(ParamPart) = Length(Parambinding)
                      // Parambinding will have length ParamCount in the end
  ParamPart:array of TStringPart; // describe which parts of buf are parameters
  NewQueryLength:integer;
  NewQuery:string;
  NewQueryIndex,BufIndex,CopyLen,i:integer;    // Parambinding will have length ParamCount in the end
  tmpParam:TParam;

begin
  if DoCreate then Clear;
  // Parse the SQL and build ParamBinding
  ParamCount:=0;
  NewQueryLength:=Length(SQL);
  SetLength(ParamPart,ParamAllocStepSize);
  SetLength(ParamBinding,ParamAllocStepSize);
  QuestionMarkParamCount:=0; // number of ? params found in query so far

  ReplaceString := '$';
  if ParameterStyle = psSimulated then
    while pos(ReplaceString,SQL) > 0 do ReplaceString := ReplaceString+'$';

  p:=1;
  BufStart:=p; // used to calculate ParamPart.Start values
  repeat
    while SkipComments(SQL,p,EscapeSlash,EscapeRepeat) do ;
    case SQL[p] of
    ':','?': // parameter
      begin
      IgnorePart := False;
      if SQL[p]=':' then
        begin // find parameter name
        Inc(p);
        if charInSet(SQL[p],[':','=',' ']) then  // ignore ::, since some databases uses this as a cast (wb 4813)
          begin
          IgnorePart := True;
          Inc(p);
          end
        else
          begin
          if (SQL[p]='"') then // Check if the parameter-name is between quotes
             begin
             ParamNameStart:=p;
             SkipQuotesString(SQL,p,'"',EscapeSlash,EscapeRepeat);
             // Do not include the quotes in ParamName, but they must be included
             // when the parameter is replaced by some place-holder.
             ParamName:=Copy(SQL,ParamNameStart+1,p-ParamNameStart-2);
             end
          else
             begin
             ParamNameStart:=p;
             while not CharInSet(SQL[p], ParamDelimiters) do
               Inc(p);
             ParamName:=Copy(SQL,ParamNameStart,p-ParamNameStart);
             end;
          end;
        end
      else
        begin
        Inc(p);
        ParamNameStart:=p;
        ParamName:='';
        end;
      if not IgnorePart then
        begin
        Inc(ParamCount);
        if ParamCount>Length(ParamPart) then
          begin
          NewLength:=Length(ParamPart)+ParamAllocStepSize;
          SetLength(ParamPart,NewLength);
          SetLength(ParamBinding,NewLength);
          end;
        if DoCreate then
          begin
          // Check if this is the first occurance of the parameter
          tmpParam := FindParam(ParamName);
          // If so, create the parameter and assign the Parameterindex
          if not assigned(tmpParam) then
            ParameterIndex := CreateParam(ftUnknown, ParamName, ptInput).Index
          else  // else only assign the ParameterIndex
            ParameterIndex := tmpParam.Index;
          end
        // else find ParameterIndex
        else
          begin
          if ParamName<>'' then
            ParameterIndex:=ParamByName(ParamName).Index
          else
            begin
            ParameterIndex:=QuestionMarkParamCount;
            Inc(QuestionMarkParamCount);
            end;
          end;
        if ParameterStyle in [psPostgreSQL,psSimulated] then
          begin
          i:=ParameterIndex+1;
          repeat
            inc(NewQueryLength);
            i:=i div 10;
          until i=0;
          end;
        // store ParameterIndex in FParamIndex, ParamPart data
        ParamBinding[ParamCount-1]:=ParameterIndex;
        ParamPart[ParamCount-1].Start:=ParamNameStart-BufStart;
        ParamPart[ParamCount-1].Stop:=p-BufStart+1;
        // update NewQueryLength
        Dec(NewQueryLength,p-ParamNameStart);
        end;
      end;
    #0:
      Break; // end of SQL
    else
      Inc(p);
    end;
  until false;
  SetLength(ParamPart,ParamCount);
  SetLength(ParamBinding,ParamCount);
  if ParamCount<=0 then
    NewQuery:=SQL
  else
    begin
    // replace :ParamName by ? for interbase and by $x for postgresql/psSimulated
    // (using ParamPart array and NewQueryLength)
    if (ParameterStyle = psSimulated) and (length(ReplaceString) > 1) then
      inc(NewQueryLength,(paramcount)*(length(ReplaceString)-1));

    SetLength(NewQuery,NewQueryLength);
    NewQueryIndex:=1;
    BufIndex:=1;
    for i:=0 to High(ParamPart) do
      begin
      CopyLen:=ParamPart[i].Start-BufIndex;
      NewQuery:=NewQuery+Copy(SQL,BufIndex,CopyLen);
      Inc(NewQueryIndex,CopyLen);
      case ParameterStyle of
        psInterbase : begin
                        NewQuery:=NewQuery+'?';
                        Inc(NewQueryIndex);
                      end;
        psPostgreSQL,
        psSimulated : begin
                      ParamName := IntToStr(ParamBinding[i]+1);
                      NewQuery:=StringOfChar('$',Length(ReplaceString));
                      NewQuery:=NewQuery+ParamName;
                      end;
      end;
      BufIndex:=ParamPart[i].Stop;
    end;
    CopyLen:=Length(SQL)+1-BufIndex;
    if (CopyLen>0) then
      NewQuery:=NewQuery+Copy(SQL,BufIndex,CopyLen);
    end;
  Result:=NewQuery;
end;


Procedure TParams.RemoveParam(Value: TParam);
begin
   Value.Collection:=Nil;
end;

{ TParam }

Function TParam.GetDataSet: TDataSet;
begin
  If Assigned(Collection) and (Collection is TParams) then
    Result:=TParams(Collection).GetDataset
  else
    Result:=Nil;
end;

Function TParam.IsParamStored: Boolean;
begin
  Result:=Bound;
end;

Procedure TParam.AssignParam(Param: TParam);
begin
  if Not Assigned(Param) then
    begin
    Clear;
    FDataType:=ftunknown;
    FParamType:=ptUnknown;
    Name:='';
    Size:=0;
    Precision:=0;
    NumericScale:=0;
    end
  else
    begin
    FDataType:=Param.DataType;
    if Param.IsNull then
      Clear
    else
      FValue:=Param.FValue;
    FBound:=Param.Bound;
    Name:=Param.Name;
    if (ParamType=ptUnknown) then
      ParamType:=Param.ParamType;
    Size:=Param.Size;
    Precision:=Param.Precision;
    NumericScale:=Param.NumericScale;
    end;
end;

Procedure TParam.AssignTo(Dest: TPersistent);
begin
  if (Dest is TField) then
    AssignToField(TField(Dest))
  else
    inherited AssignTo(Dest);
end;

Function TParam.GetAsBoolean: Boolean;
begin
  If IsNull then
    Result:=False
  else
    Result:=FValue=true;
end;

Function TParam.GetAsBytes: TBytes;
begin
  if IsNull then
    Result:=nil
  else if isArray(FValue) then
    Result:=TBytes(FValue)
end;

Function TParam.GetAsDateTime: TDateTime;
begin
  If IsNull then
    Result:=0.0
  else
    Result:=TDateTime(FValue);
end;

Function TParam.GetAsFloat: Double;
begin
  If IsNull then
    Result:=0.0
  else
    Result:=Double(FValue);
end;

Function TParam.GetAsInteger: Longint;
begin
  If IsNull or not IsInteger(FValue) then
    Result:=0
  else
    Result:=Integer(FValue);
end;

Function TParam.GetAsLargeInt: NativeInt;
begin
  If IsNull or not IsInteger(FValue) then
    Result:=0
  else
    Result:=NativeInt(FValue);
end;


Function TParam.GetAsMemo: string;
begin
  If IsNull or not IsString(FValue) then
    Result:=''
  else
    Result:=String(FValue);
end;

Function TParam.GetAsString: string;

begin
  If IsNull or not IsString(FValue) then
    Result:=''
  else
    Result:=String(FValue);
end;

Function TParam.GetAsJSValue: JSValue;
begin
  if IsNull then
    Result:=Null
  else
    Result:=FValue;
end;
Function TParam.GetDisplayName: string;
begin
  if (FName<>'') then
    Result:=FName
  else
    Result:=inherited GetDisplayName
end;

Function TParam.GetIsNull: Boolean;
begin
  Result:= JS.IsNull(FValue);
end;

Function TParam.IsEqual(AValue: TParam): Boolean;
begin
  Result:=(Name=AValue.Name)
          and (IsNull=AValue.IsNull)
          and (Bound=AValue.Bound)
          and (DataType=AValue.DataType)
          and (ParamType=AValue.ParamType)
          and (GetValueType(FValue)=GetValueType(AValue.FValue))
          and (FValue=AValue.FValue);
end;

Procedure TParam.SetAsBlob(const AValue: TBlobData);
begin
  FDataType:=ftBlob;
  Value:=AValue;
end;

Procedure TParam.SetAsBoolean(AValue: Boolean);
begin
  FDataType:=ftBoolean;
  Value:=AValue;
end;

procedure TParam.SetAsBytes(const AValue: TBytes);
begin

end;

Procedure TParam.SetAsDate(const AValue: TDateTime);
begin
  FDataType:=ftDate;
  Value:=AValue;
end;

Procedure TParam.SetAsDateTime(const AValue: TDateTime);
begin
  FDataType:=ftDateTime;
  Value:=AValue;
end;

Procedure TParam.SetAsFloat(const AValue: Double);
begin
  FDataType:=ftFloat;
  Value:=AValue;
end;

Procedure TParam.SetAsInteger(AValue: Longint);
begin
  FDataType:=ftInteger;
  Value:=AValue;
end;

Procedure TParam.SetAsLargeInt(AValue: NativeInt);
begin
  FDataType:=ftLargeint;
  Value:=AValue;
end;

Procedure TParam.SetAsMemo(const AValue: string);
begin
  FDataType:=ftMemo;
  Value:=AValue;
end;

Procedure TParam.SetAsString(const AValue: string);
begin
  if FDataType <> ftFixedChar then
    FDataType := ftString;
  Value:=AValue;
end;


Procedure TParam.SetAsTime(const AValue: TDateTime);
begin
  FDataType:=ftTime;
  Value:=AValue;
end;

Procedure TParam.SetAsJSValue(const AValue: JSValue);

begin
  FValue:=AValue;
  FBound:=not JS.IsNull(AValue);
  if FBound then
    case GetValueType(aValue) of
      jvtBoolean : FDataType:=ftBoolean;
      jvtInteger : FDataType:=ftInteger;
      jvtFloat : FDataType:=ftFloat;
      jvtObject,jvtArray : FDataType:=ftBlob;
    end;
end;

Procedure TParam.SetDataType(AValue: TFieldType);


begin
  FDataType:=AValue;

end;

Procedure TParam.SetText(const AValue: string);
begin
  Value:=AValue;
end;

constructor TParam.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  ParamType:=ptUnknown;
  DataType:=ftUnknown;
  FValue:=Null;
end;

constructor TParam.Create(AParams: TParams; AParamType: TParamType);
begin
  Create(AParams);
  ParamType:=AParamType;
end;

Procedure TParam.Assign(Source: TPersistent);
begin
  if (Source is TParam) then
    AssignParam(TParam(Source))
  else if (Source is TField) then
    AssignField(TField(Source))
  else if (source is TStrings) then
    AsMemo:=TStrings(Source).Text
  else
    inherited Assign(Source);
end;

Procedure TParam.AssignField(Field: TField);
begin
  if Assigned(Field) then
    begin
    // Need TField.Value
    AssignFieldValue(Field,Field.Value);
    Name:=Field.FieldName;
    end
  else
    begin
    Clear;
    Name:='';
    end
end;

Procedure TParam.AssignToField(Field : TField);

begin
  if Assigned(Field) then
    case FDataType of
      ftUnknown  : DatabaseErrorFmt(SUnknownParamFieldType,[Name],DataSet);
      // Need TField.AsSmallInt
      // Need TField.AsWord
      ftInteger,
      ftAutoInc  : Field.AsInteger:=AsInteger;
      ftFloat    : Field.AsFloat:=AsFloat;
      ftBoolean  : Field.AsBoolean:=AsBoolean;
      ftBlob,
      ftString,
      ftMemo,
      ftFixedChar: Field.AsString:=AsString;
      ftTime,
      ftDate,
      ftDateTime : Field.AsDateTime:=AsDateTime;
    end;
end;

Procedure TParam.AssignFromField(Field : TField);

begin
  if Assigned(Field) then
    begin
    FDataType:=Field.DataType;
    case Field.DataType of
      ftUnknown  : DatabaseErrorFmt(SUnknownParamFieldType,[Name],DataSet);
      ftInteger,
      ftAutoInc  : AsInteger:=Field.AsInteger;
      ftFloat    : AsFloat:=Field.AsFloat;
      ftBoolean  : AsBoolean:=Field.AsBoolean;
      ftBlob,
      ftString,
      ftMemo,
      ftFixedChar: AsString:=Field.AsString;
      ftTime,
      ftDate,
      ftDateTime : AsDateTime:=Field.AsDateTime;
    end;
    end;
end;

Procedure TParam.AssignFieldValue(Field: TField; const AValue: JSValue);

begin
  If Assigned(Field) then
    begin

    if (Field.DataType = ftString) and TStringField(Field).FixedChar then
      FDataType := ftFixedChar
    else if (Field.DataType = ftMemo) and (Field.Size > 255) then
      FDataType := ftString
    else
      FDataType := Field.DataType;
    if JS.IsNull(AValue) then
      Clear
    else
      Value:=AValue;

    Size:=Field.DataSize;
    FBound:=True;

    end;
end;

Procedure TParam.Clear;
begin
  FValue:=Null;
end;


Procedure TParams.CopyParamValuesFromDataset(ADataSet: TDataSet;
  CopyBound: Boolean);

Var
  I : Integer;
  P : TParam;
  F : TField;

begin
  If assigned(ADataSet) then
    For I:=0 to Count-1 do
     begin
     P:=Items[i];
     if CopyBound or (not P.Bound) then
       begin
       // Master dataset must be active and unbound parameters must have fields
       // with same names in master dataset (Delphi compatible behavior)
       F:=ADataSet.FieldByName(P.Name);
       P.AssignField(F);
       If Not CopyBound then
         P.Bound:=False;
       end;
    end;
end;

initialization

end.
