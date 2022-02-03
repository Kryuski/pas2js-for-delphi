{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2020 by Michael Van Canneyt

    NodeJS onoff module import.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit node.onoff;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  JS,nodejs;

Type
  TGPIOReadCallBack = reference to procedure(err : TJSError; aValue : Byte);
  TGPIOWriteCallBack = reference to procedure(err : TJSError);

  TNJSGPIO = class external name 'Gpio' (TJSObject)
    class var
      FAccessible : boolean; external name 'accessible' ;
    constructor new (agpio : byte; aDirection : string); overload;
    constructor new (agpio : byte; aDirection,aEdge : String); overload;
    constructor new (agpio : byte; aDirection,aEdge : string; aOptions : TJSObject); overload;
    procedure read; overload;
    procedure read(aCallBack : TGPIOReadCallBack); overload;
    function readSync : Byte;
    procedure write(aValue: Byte); overload;
    procedure write(aCallBack : TGPIOWriteCallBack); overload;
    procedure writeSync(aValue: Byte);
    procedure watch(aCallback : TGPIOReadCallBack);
    procedure unwatch(aCallback : TGPIOReadCallBack);
    procedure unwatchAll;
    function getdirection : string; external name 'direction';
    procedure setDirection(aDirection : string);
    function getEdge : string; external name 'edge';
    procedure setEdge(aEdge : string);
    function GetactiveLow : boolean; external name 'activeLow';
    procedure setActiveLow(aInvert : Boolean);
    procedure unexport;
    property Direction : string read getdirection write setdirection;
    property Edge : string read GetEdge write SetEdge;
    property ActiveLow : Boolean Read getActiveLow write setActiveLow;
    class property accessible: boolean Read Faccessible;
  const
    High = 1;
    Low = 0;
  end;
  TNJSGPIOClass = class of TNJSGPIO;

var
  TGPIO : TNJSGPIOClass;

implementation

Type
  TNJSOnOffModule = class external name 'Object' (TJSObject)
    Gpio : TNJSGPIOClass;
  end;  
  
initialization
  TGPIO:=TNJSOnOffModule(require('onoff')).Gpio;
end.
