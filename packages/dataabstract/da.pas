{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2018 by Michael Van Canneyt, member of the
    Free Pascal development team

    Remobjects Data Abstract external classes.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit DA;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses Types, JS, DASDK;

Type
  // Forward classes
  TDADataTable = class;
  TDABIN2DataStreamer = class;
  TDAJSONDataStreamer = class;
  TDARemoteDataAdapter = Class;
  TDAChange = class;
  TDADelta = class;
  TDADeltas = class;
  TDAField = class;
  TDALookupField = class;
  TDADataTableRow = class;
  TDAExpression  = class;
  TDADynamicWhere = class;
  TDAConstantExpression = class;
  TDAParameterExpression  = class;
  TDANullExpression  = class;
  TDAFieldExpression  = class;
  TDAMacroExpression  = class;
  TDAUnaryExpression  = class;
  TDABinaryExpression  = class;
  TDABetweenExpression  = class;
  TDAListExpression  = class;
  TDAUtil = Class;
  TDARemoteDataAdaptor = Class;


  TDAStream = String;

  TDADataStreamer = class external name 'RemObjects.DataAbstract.DataStreamer' (TJSObject)
  Public 
    procedure initializeRead;
    procedure initializeWrite;
    procedure finalizeWrite;
    function getStream : TDAStream;
    procedure setStream(aStream : TDAStream);
    procedure readDataset(aDataset : TDADataTable);
    function readDelta : TDADelta;
    procedure writeDelta(aDelta : TDADelta);
    Property Stream : TDAStream Read getStream write setStream;
  end;

  TDADataStreamerClass = Class of TDADataStreamer;
  
  TDABIN2DataStreamer = class external name 'RemObjects.DataAbstract.Bin2DataStreamer' (TDADataStreamer)
    function readByte : Byte;
    function readInteger : NativeInt;
    function readAnsiStringWithLength : String;
    function readUtf8StringWithLength : string;
    function read (aType : string) : TJSObject;
    function readParam (acount : Integer) : TDADataParameter;
    function readField(acount : Integer) : TDAField;
    Procedure writeByte(aValue : Byte);
    Procedure writeInteger(aValue : NativeInt);
    Procedure writeAnsiStringWithLength(aValue : String);
    Procedure write(aType : string; aValue : TJSObject);
  end;
  
  TDAJSONDataStreamer = class external name 'RemObjects.DataAbstract.JSONDataStreamer' (TDADataStreamer)
  end;  
  
  TDARemoteDataAdapter = Class external name 'RemObjects.DataAbstract.RemoteDataAdapter' (TJSObject)
  Public
    Constructor New(Const aURL, aDataServiceName, aLoginServiceName : String; 
                    aStreamerClass : TDADataStreamerClass);
  end;

  TDAChange = class external name 'RemObjects.DataAbstract.Change' (TJSObject)
  end;

  TDAChangeArray = array of TDAChange;
  
  TLogField = record
    name : string;
    datatype : string;
  end;
  TLogFieldArray = array of TLogfield;
  
  TDADelta = class external name 'RemObjects.DataAbstract.Delta' (TJSObject)
  Private
    FData : TDAChangeArray; external name 'data'; 
    FKeyFields : TStringDynArray; external name 'keyfields';
    FLoggedFields : TLogFieldArray; external name 'loggedfields';
    FName : string; external name 'name';
  Public
    Function intFindId(anId : Integer) : TDAChange;
    Property data : TDAChangeArray Read FData;
    Property keyFields : TStringDynArray Read FKeyFields;
    Property LoggedFields : TLogFieldArray Read FLoggedFields; 
    Property Name : String Read FName;
  end;

  TDADeltas = class external name 'RemObjects.DataAbstract.Deltas' (TJSObject)
  Public
    Function FindByName (Const aName : String) : TDADelta;
  end;

  TDATableRowNotifyEvent = reference to procedure(row : TDADataTableRow);
  TDADataTableRowArray = array of TDADataTableRow;
  TDAFieldArray = Array of TDAField;

  TDADataTable = class external name 'RemObjects.DataAbstract.DataTable' (TJSObject)
  Public
    name : string;
    rows : TDADataTableRowArray;
    fields : TDAFieldArray;
    deletedrows : TDADataTableRowArray;
    frecordbuffer : TJSArray;
    fNextRecID : Integer;
    fIndex : Integer;
    bofFlag : Boolean;
    eofFlag : Boolean;
    dynamicWhere : TJSObject;
    onNewRecord : TDATableRowNotifyEvent;
    onBeforeDelete: TDATableRowNotifyEvent;
    onAfterDelete: TDATableRowNotifyEvent;
    onBeforeScroll: TDATableRowNotifyEvent;
    onAfterScroll: TDATableRowNotifyEvent;
    Procedure checkRequired;
    Procedure locate(aName : String; aValue : JSValue);
    procedure addLookupField(const aName,aSourceField : String; aLookupTable : TDADataTable;
                             const aLookupKeyField, aLookupResultField : String);
    procedure getNextId;
    function appendRow  : TDADataTableRow;
    procedure deleteRow;
    procedure markDeleted;
    function fieldNumByName(Const aName : string) : Integer;
    function fieldByName(Const aName : string) : TDAField;
    procedure setFieldValue(Const aName : string; aValue : JSValue);
    function getFieldValue(Const aName : string) : JSValue;
    procedure setFieldAsString(Const aName, aValue : String);
    function getFieldAsString(Const aName : string) : String;
    function currentRow : TDADataTableRow;
    procedure first;
    procedure last;
    procedure next;
    procedure prev;
    Function findId(anID: Integer) : TDADataTableRow;
    function eof : boolean;
    function bof : boolean;
    procedure post;
    procedure cancel;
  end;
 
  TDAField = class external name 'RemObjects.DataAbstract.Field' (TJSObject)
  Public
    alignment : string;
    blobtype: string;
    businessClassID : String;
    calculated : string;
    customAttributes : string;
    dataType : string;
    name: string;
    type_ : string external name 'type';
    logChanges : boolean;
    readOnly : boolean;
    serverAutoRefresh : Boolean;
    serverCalculated : Boolean;
    description : string;
    decimalPrecision : Integer;
    decimalScale : integer;
    defaultValue : string;
    dictionaryEntry : String;
    displayLabel : String;
    displayWidth : integer;
    inPrimaryKey : Boolean;
    visible : boolean;
    required : boolean;
    size : integer;
    Procedure checkReadOnly;
  end;

  TDALookupField = class external name 'RemObjects.DataAbstract.LookupField' (TJSObject)
  Public
    sourceField : string;
    lookupTable : TDADataTable;
    lookupKeyField: String;
    lookupResultField : string;
  end;

  TDADataTableRow = class external name 'RemObjects.DataAbstract.DataTableRow' (TJSObject)
  Public
    recID : Integer;
    state : string;
    __oldValues : array of JSValue;
    __newValues : array of JSValue;
  end;

  TDAExpression  = class external name 'RemObjects.DataAbstract.Expression' (TJSObject);

  TDADynamicWhere = class external name 'RemObjects.DataAbstract.DynamicWhere' (TJSObject)
  Public
    constructor New(anExpression : TDAExpression);
    function toXML : String;
  end;
 
  TDAConstantExpression  = class external name 'RemObjects.DataAbstract.ConstantExpression' (TDAExpression)
  Public
    constructor new (aType : String; aValue : JSValue; ANull : Byte);
  end;

  TDAParameterExpression  = class external name 'RemObjects.DataAbstract.ParameterExpression' (TDAExpression)
  Public
    constructor new (const aName, aType : String; aSize : Integer);
  end;

  TDANullExpression  = class external name 'RemObjects.DataAbstract.NullExpression' (TDAExpression)
  public
    constructor new;
  end;

  
  TDAFieldExpression  = class external name 'RemObjects.DataAbstract.FieldExpression' (TDAExpression)
  public
    constructor new(aName : string);
  end;
  
  TDAMacroExpression  = class external name 'RemObjects.DataAbstract.MacroExpression' (TDAExpression)
  public
    constructor new(aName : string);
  end;
  
  
  TDAUnaryExpression  = class external name 'RemObjects.DataAbstract.UnaryExpression' (TDAExpression)
  public
    constructor new(aNode : TDAExpression; aOperator : string);
  end;
  
  TDABinaryExpression  = class external name 'RemObjects.DataAbstract.BinaryExpression' (TDAExpression)
   public
    constructor new(aNode1,aNode2 : TDAExpression; aOperator : string);
  end;
  
  TDABetweenExpression  = class external name 'RemObjects.DataAbstract.BetweenExpression' (TDAExpression)
  public
    constructor new(aNode1,aNode2,aNode3 : TDAExpression);
  end;
  
  TDAListExpression  = class external name 'RemObjects.DataAbstract.ListExpression' (TDAExpression)
  public
    constructor new(aList : array of TDAExpression);
  end;
  

  TDAUtil = Class external name 'RemObjects.DataAbstract.Util' (TJSObject)
  Public
    function createDataParameter(aName : String;aValue : JSValue) : TJSObject;
    function createRequestInfo(IncludeSchema : Boolean; MaxRecords : Integer; UserFilter : String; Parameters  : Array of JSValue) : TJSObject;
    function createRequestInfoV5(IncludeSchema : Boolean; MaxRecords : Integer; UserFilter : String; Parameters  : Array of JSValue) : TJSOBject;
    function createRequestInfoV6(SQL : String; MaxRecords : Integer; UserFilter : String; Parameters  : Array of JSValue) : TJSObject;
    procedure setupScriptingCallBacks;
  end;  
  
  TDACallBack = procedure;
  TDALoginNeededCallBack = reference to procedure(aCallBack : TDACallBack);   
  TDAChangeFailHandler = reference to procedure (aData : TDAChange);
  
  TDARemoteDataAdaptor = Class external name 'RemObjects.DataAbstract.RemoteDataAdapter' (TJSObject)
  Private
    FSendReducedDelta : boolean; external name 'sendReducedDelta';
  Public
    onLoginNeeded : TDALoginNeededCallBack;
    onChangeFail : TDAChangeFailHandler;
    function getDataService() : TDADataAbstractService;
    function getLoginService() : TDASimpleLoginService;
    procedure login(aUserID,aPassword,aConnectionName : String; OnSuccess : TDASuccessEvent; OnFailed : TDAFailedEvent);
    procedure logout(OnSuccess : TDASuccessEvent; OnFailed : TDAFailedEvent);
    function createStreaer: TDAJSONDatastreamer;
    procedure setSendReducedDelta  (aValue : Boolean);
    procedure getSchema(aFilter : String;OnSuccess : TDASuccessEvent; OnFailed : TDAFailedEvent);
    function buildDelta(aTable : TDADataTable) : TDADelta;
    procedure createTableFromSchema(const aTableName : String; aTable : TDADataTable; CallBack: TDACallBack);
    procedure executeCommand(const aName : String; Parameters: TDADataParameterArray; OnSuccess : TDASuccessEvent; OnFailed : TDAFailedEvent);
    function getAutoGetScripts : Boolean;
    procedure setAutoGetScripts(aValue : boolean);
    Procedure getSQLData(aTable : TDADataTable; const SQL : String;OnSuccess : TDASuccessEvent; OnFailed : TDAFailedEvent);
    Procedure getData(aTable : TDADataTable; aRequest : TDATableRequestInfo;OnSuccess : TDASuccessEvent; OnFailed : TDAFailedEvent);
    procedure applyUpdates(aTable : TDADataTable; OnSuccess : TDASuccessEvent; OnFailed : TDAFailedEvent);
    property sendReducedDelta : Boolean Read FSendReducedDelta Write setSendReducedDelta;
    property AutoGetScripts : boolean Read getAutoGetScripts write setAutoGetScripts;
  end;

  TDAHTMLTableView = class external name 'RemObjects.DataAbstract.Views.HtmlTableView'
  Public
    constructor new(aTable : TDADataTable; aHTMLTableID : String);
  end;

  TDAVerticalHTMLTableView = class external name 'RemObjects.DataAbstract.Views.VerticalHtmlTableView'
  Public
    constructor new(aTable : TDADataTable; aHTMLTableID : String);
  end;
Implementation

end.
