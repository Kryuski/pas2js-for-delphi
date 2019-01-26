{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2018 by Michael Van Canneyt, member of the
    Free Pascal development team

    Remobjects SDK external classes definitions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit ROSDK;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  Types, JS;

Type
  TROValue = record
    dataType : string;
    value : JSValue;
  end;
  TROComplexType = class;
  TROEnumType = class;
  TROStructType = class;
  TROArrayType = class;
  TROException = class;
  TROEventSink = class;
  TROClientChannel = class;
  TROHTTPClientChannel = class;
  TROMessage = class;
  TROJSONMessage = class;
  TROBinMessage = class;
  TROBinHeader = class;
  TRORemoteService = class;
  TROService = class;
  TROEventReceiver = class;

  TROUtil = class external name 'RemObjects.UTIL' (TJSObject)
  Public
    class function toBase64 (Const aValue : String) : String;
    class function fromBase64 (Const aValue : String) : String;
    class procedure showMessage (Const msg : string);
    class procedure showError(Const msg : string; e : JSValue);
    class function toJSON(aValue : jsValue) : String;
    class function parseJSON (Const aValue : string) : JSValue;
    class function NewGuid() : string;
    class function GuidToArray(Const aGuid : string) : TIntegerDynArray;
    class function guidToByteArray(Const aGuid : string): String;
    class function zeroPad(const num : string; count : integer) : String;
    class function byteArrayToGuid(const byteArray : string) : String;
    class function strToByteArray (const str : string) : string;
    class function byteArrayToStr(const byteArray : string) : string;
    class function byteArrayToUtf16(const byteArray : string) : string;
    class function utf16ToByteArray(const str : string) : string;
    class function ISO8601toDateTime(const str: String) : TJSDate;
    class function dateTimeToSOAPString(aValue: TJSDate) : string;
    class function decimalToString(aDecimal : array of integer ) : string;
    class function stringToDecimal(const aString : String) : TIntegerDynArray;
    class Function checkArgumentTypes (args : Array of JSValue; types : array of string) : Boolean;
  end;


  TROException = class external name 'RemObjects.SDK.ROException' (TJSError);

  TROComplexType = class external name 'RemObjects.SDK.ROComplexType' (TJSObject)
  Public
    Procedure readFrom(aMessage : TROMessage);
    Procedure writeTo(aMessage : TROMessage);
  end;

  TROEnumType = class external name 'RemObjects.SDK.ROEnumType' (TROComplexType)
  Public
    Procedure fromObject(aObject : TJSObject); overload;
    Function toObject(aStoreType : Boolean) : TJSObject;overload;
  end;

  TROStructType = class external name 'RemObjects.SDK.ROStructType' (TROComplexType)
  Public
    Procedure fromObject(aObject : TJSObject);overload;
    Function toObject(aStoreType : Boolean) : TJSObject;overload;
  end;

  TROArrayType = class external name 'RemObjects.SDK.ROArrayType' (TROComplexType)
  Public
    Procedure fromObject(aObject : Array of TJSObject);overload;
    Function toObject(aStoreType : Boolean)  : TJSObjectDynArray;overload;
  end;

  TRODispatchSuccessEvent = reference to Procedure (msg : TROMessage);
  TRODispatchFailedEvent = reference to Procedure (msg : TROMessage; aError : TJSError);
  TROCallBack = Procedure;
  TROOnLoginNeeded =  reference to procedure(aCallBack : TROCallBack);

  TROClientChannel = class external name 'RemObjects.SDK.ClientChannel' (TJSObject)
  Public
    onLoginNeeded : TROOnLoginNeeded;
  Public
    Constructor new(aURL : String);
    Procedure dispatch(aMessage : TROMessage; onSuccess : TRODispatchSuccessEvent; OnError : TRODispatchFailedEvent);
  end;


  TROHTTPCallback = reference to procedure (aResponse : String; aStatus : Integer);
  TROHTTPClientChannel = class external name 'RemObjects.SDK.HTTPClientChannel' (TROClientChannel)
  Public
    Procedure post(aMessage : TROMessage; isBinary : Boolean; OnSuccess,OnError : TROHTTPCallback);
  end;

  TROEventSink = class external name 'RemObjects.SDK.ROEventSink' (TJSObject)
  Public
    Procedure readEvent(aMessage : TROMessage; aName : string);
  end;


  TROMessage = class external name 'RemObjects.SDK.Message' (TJSObject)
  Public
    constructor new;
    Function Clone : TROMessage;
    function getClientID : String;
    procedure setClientID(const aValue : String);
    function getErrorMessage : String;
    procedure setErrorResponse (Const aResponse : String);
    Procedure initialize (Const aServiceName,aMethodName : string; aMessageType : Integer);
    Procedure finalize;
    function requestStream : String; // Dummy
    procedure setResponseStream(const aValue : String);
    function read (const aName,aType : String) : TROValue;
    Procedure write (const aName,aType : String; aValue : JSValue);
    Property ClientID : String Read getClientID Write setClientID;
  end;

  TROJSONMessage = class external name 'RemObjects.SDK.JSONMessage' (TROMessage)
  end;

  TROBinHeader = class external name 'RemObjects.SDK.BinHeader' (TJSObject)
  Public
    function asStream: String;
    Procedure ReadFrom(aStream : String);
    function isValidHeader : Boolean;
    function getCompressed : Boolean;
    Procedure setCompressed(aValue : Boolean);
    function getMessageType : integer;
    Procedure setMessageType(aValue : integer);
    procedure setClientID(aValue : String);
    Property Compressed : Boolean Read getCompressed Write setCompressed;
    Property MessageType : Integer Read getMessageType write SetMessageType;
  end;

  TROBinMessage = class external name 'RemObjects.SDK.BinMessage' (TROMessage)
  public
    constructor new;
    Procedure writeVariant(aValue : JSValue);
    Procedure writeinteger(aValue : Integer);
    Procedure writeStrWithLength(aValue : string);
    function readByte : Byte;
    function readCompressed : String;
    function readVariant : JSValue;
  end;

  TROEventCallback = reference to procedure (event : TJSObject); // Or TROComplexType ?

  TROEventReceiver  = class external name 'RemObjects.SDK.ROEventReceiver' (TJSObject)
  Public
    Constructor new(aChannel : TROClientChannel; aMessage : TROMessage; aServiceName : string; aTimeOut : Integer);
    Procedure addHandler(anEventName : String; aCallback : TROEventCallback);
    Procedure setActive(aValue : boolean);
    function getActive : Boolean;
    function getTimeout : integer;
    procedure setTimeout(aValue : Integer);
    Procedure intPollServer;
    Property Active : Boolean Read GetActive Write SetActive;
    Property TimeOut : Integer read GetTimeOut Write SetTimeout;
  end;

  TRORemoteService = Class external name 'RemObjects.SDK.RemoteService' (TJSObject)
    Constructor new(aChannel : TROClientChannel; aMessage : TROMessage; aServiceName : string);
  end;

  TROService = Class external name 'RemObjects.SDK.ROService' (TJSObject)
  Public
    Constructor new(aService : TRORemoteService);
    Constructor new(aChannel : TROClientChannel; aMessage : TROMessage; aServiceName : string);
    function getMessage : TROMessage;
    function getChannel : TROClientChannel;
    function getServiceName : String;
    Property Message : TROMessage Read getMessage;
    Property Channel : TROClientChannel Read getChannel;
    Property ServiceName : String Read getServiceName;
  end;

  TROBinaryParser = Class external name 'BinaryParser' (TJSObject)
    procedure warn;
    function decodeFloat(data : JSValue; precisionbits,exponentbits :Integer) : double;
    function encodeFloat(value: double; precisionbits,exponentbits :Integer) : string;
    function decodeInt(data : JSValue; bits : Integer; Signed : boolean) : NativeInt;
    function encodeInt(data : NativeInt; bits : Integer; Signed : boolean) : String;
  end;

implementation

end.

