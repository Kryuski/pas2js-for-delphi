{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2018 by Michael Van Canneyt, member of the
    Free Pascal development team

    Remobjects Data Abstract external classes definitions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit dasdk;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses JS, ROSDK;

Type
  TDAUserInfo = Class;
  TDASuccessEvent = Procedure (res : JSValue) of object;
  TDAFailedEvent = Procedure (response : TJSOBject; fail : String) of object;

  TDALoginSuccessEvent = Reference to Procedure (result : Boolean; UserInfo : TDAUserInfo);

  TDABaseLoginService = class external name 'RemObjects.DataAbstract.Server.SimpleLoginService' (TJSObject)
  Public
    Constructor new(ch : TROHTTPClientChannel; msg : TROMessage; aServiceName : string);
    Procedure LoginEx(aLoginString :String; aSuccess : TDALoginSuccessEvent; aFailure : TDAFailedEvent);
    Procedure Logout(aSuccess : TDASuccessEvent; aFailure : TDAFailedEvent);
  end;

  TDASimpleLoginService = class external name 'RemObjects.DataAbstract.Server.SimpleLoginService' (TDABaseLoginService)
  Public
    Procedure Login(aUserId,aPassword :String; aSuccess : TDALoginSuccessEvent; aFailure : TDAFailedEvent);
  end;

  TDAStringArray = class external name 'RemObjects.DataAbstract.Server.StringArray'
  Public
    constructor new;
    procedure fromObject(aItems : array of string); overload;
  end;

  TDADataParameterData = Record
    Name: string;
    Value : JSValue;
  End;
  TDADataParameterDataArray = array of TDADataParameterData;

  TDADataParameter = class external name 'RemObjects.DataAbstract.Server.DataParameter' (TROStructType)
  Public
    constructor new;
    Procedure fromObject(aItem : TDADataParameterData); overload;
  Public
    Name : TROValue;
    Value : TROValue;
  end;

  TDADataParameterArray = class external name 'RemObjects.DataAbstract.Server.DataParameterArray' (TROArrayType)
  Public
    constructor new;
    Procedure fromObject(aItems : Array of TDADataParameterData); overload;
    function toObject : TDADataParameterDataArray; reintroduce;
  Public
    items : Array of TDADataParameter;
  end;

  TDAColumnSortingData = record
    FieldName : String;
    SortDirection : String;
  end;
  TDAColumnSortingDataArray = Array of TDAColumnSortingData;

  TDAColumnSorting = class external name 'RemObjects.DataAbstract.Server.ColumnSorting' (TROStructType)
  Public
    FieldName : TROValue;
    Direction : TROValue;
  end;

  TDAColumnSortingArray = class external name 'RemObjects.DataAbstract.Server.ColumnSortingArray' (TROArrayType)
  Public
    constructor new;
    Procedure fromObject(aItems : Array of TDAColumnSortingData); overload;
    function toObject : TDAColumnSortingDataArray; reintroduce;
  Public
    items : Array of TDAColumnSorting;
  end;

  TDATableRequestInfoData = record
    IncludeSchema : boolean;
    MaxRecords : Integer;
    Parameters : TDADataParameterDataArray;
    UserFilter : String;
  end;
  TDATableRequestInfoDataArray = array of TDATableRequestInfoData;

  TDATableRequestInfo = class external name 'RemObjects.DataAbstract.Server.TableRequestInfo' (TROStructType)
  Public
    constructor new;
    procedure fromObject(aItem : TDATableRequestInfoData);reintroduce; overload;
    procedure fromObject(aItem : TJSObject);reintroduce; overload;
    Function toObject : TDATableRequestInfoData; reintroduce;
  Public
    IncludeSchema : TROValue;
    MaxRecords : TROValue;
    Parameters : TROValue;
    UserFilter : TROValue;
  end;

  TDATableRequestInfoV5Data = record
    DynamicSelectFieldNames : Array of string;
    IncludeSchema : boolean;
    MaxRecords : Integer;
    Parameters : Array of TDADataParameterData;
    UserFilter : String;
    Sorting : TDAColumnSortingData;
    WhereClause : String;
  end;

  TDATableRequestInfoV5 = class external name 'RemObjects.DataAbstract.Server.TableRequestInfoV5' (TROStructType)
  Public
    constructor new;
    procedure fromObject(aItem : TDATableRequestInfoV5Data);reintroduce;overload;
    procedure fromObject(aItem : TJSObject);reintroduce;overload;
    function toObject : TDATableRequestInfoV5Data;reintroduce;
  Public
    DynamicSelectFieldNames : TROValue;
    IncludeSchema : TROValue;
    MaxRecords : TROValue;
    Parameters : TROValue;
    UserFilter : TROValue;
    Sorting : TROValue;
    WhereClause : TROValue;
  end;

  TDATableRequestInfoV6Data = record
    IncludeSchema : boolean;
    MaxRecords : Integer;
    Parameters : Array of TDADataParameterData;
    SQL : String;
    UserFilter : String;
  end;

  TDATableRequestInfoV6 = class external name 'RemObjects.DataAbstract.Server.TableRequestInfoV6' (TROStructType)
  Public
    constructor new;
    procedure fromObject(aItem : TDATableRequestInfoData);reintroduce;overload;
    procedure fromObject(aItem : TJSObject);reintroduce;overload;
    function toObject : TDATableRequestInfoV6Data;reintroduce;
  Public
    IncludeSchema : TROValue;
    MaxRecords : TROValue;
    Parameters : TDADataParameterArray;
    Sql : TROValue;
    UserFilter : TROValue;
  end;

  TDAUserInfoData = record
    Attributes : array of JSValue;
    Privileges : Array of string;
    SessionID : String;
    UserData : JSValue;
    UserID : String;
  end;

  TDAUserInfo = class external name 'RemObjects.DataAbstract.Server.UserInfo' (TROStructType)
    constructor new;
    procedure fromObject(aItem : TDAUserInfo);reintroduce; overload;
    procedure fromObject(aItem : TJSObject);reintroduce; overload;
    function toObject : TDAUserInfoData;reintroduce;
  Public
    Attributes : TROValue;
    Privileges : TROValue;
    SessionID : TROValue;
    UserData : TROValue;
    UserID : TROValue;
  end;

  TDATableRequestInfoArray = class external name 'RemObjects.DataAbstract.Server.TableRequestInfoArray'  (TROArrayType)
  Public
    constructor new;
    procedure fromObject(aItems : Array of TDATableRequestInfoData);overload;
    procedure fromObject(aItems : Array of TDATableRequestInfoV5Data);overload;
    procedure fromObject(aItems : Array of TDATableRequestInfoV6Data);overload;
  Public
    items : array of TDATableRequestInfo;
  end;

  TDADataAbstractService = class external name 'RemObjects.DataAbstract.Server.DataAbstractService'
  Public
    Constructor new(ch : TROHTTPClientChannel; msg : TROMessage; aServiceName : string);
    Procedure GetSchema(aFilter : String; aSuccess : TDASuccessEvent; aFailure : TDAFailedEvent);
    Procedure GetData(aTables : TDAStringArray; info : TDATableRequestInfoArray; aSuccess : TDASuccessEvent; aFailure : TDAFailedEvent);
    Procedure UpdateData(aDelta : String; aSuccess : TDASuccessEvent; aFailure : TDAFailedEvent);
    Procedure ExecuteCommand(aCommandName : String; params : TDADataParameterArray; aSuccess : TDASuccessEvent; aFailure : TDAFailedEvent);
    Procedure ExecuteCommandEx(aCommandName : String; params : TDADataParameterArray; aSuccess : TDASuccessEvent; aFailure : TDAFailedEvent);
    Procedure GetTableSchema(aTableNameArray : TDAStringArray;aSuccess : TDASuccessEvent; aFailure : TDAFailedEvent);
    Procedure GetCommandSchema(aCommandNameArray : TDAStringArray;aSuccess : TDASuccessEvent; aFailure : TDAFailedEvent);
    Procedure GetSQLData(aSQLText : String; aIncludeSchema : Boolean; aMaxRecords : Integer; aSuccess : TDASuccessEvent; aFailure : TDAFailedEvent);
    Procedure GetSQLDataEx(aSQLText : String; aIncludeSchema : Boolean; aMaxRecords : Integer; aDynamicWhereXML : String; aSuccess : TDASuccessEvent; aFailure : TDAFailedEvent);
    Procedure SQLExecuteCommand(aSQLText : String; aSuccess : TDASuccessEvent; aFailure : TDAFailedEvent);
    Procedure SQLExecuteCommandEx(aSQLText,aDynamicWhereXML : String; aSuccess : TDASuccessEvent; aFailure : TDAFailedEvent);
    Procedure getDatasetScripts(DatasetNames : String; aSuccess : TDASuccessEvent; aFailure : TDAFailedEvent);
    Procedure RegisterForDataChangeNotification(aTableName : String; aSuccess : TDASuccessEvent; aFailure : TDAFailedEvent);
    Procedure UnregisterForDataChangeNotification(aTableName : String; aSuccess : TDASuccessEvent; aFailure : TDAFailedEvent);
  end;

implementation

end.
