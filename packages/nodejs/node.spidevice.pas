{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2020 by Michael Van Canneyt

    NodeJS spi-dev module import.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit node.spidevice;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  JS,NodeJS;


Type
  TNJSErrorCallBack = reference to procedure (err : TJSError);
  TSPIDeviceMessage = class external name 'Object' (TJSObject)
    byteLength : Longint; //- number, 32-bit, the number of bytes to transfer
    sendBuffer : TNJSBuffer; // - optional Buffer, transmit data
    receiveBuffer : TNJSBuffer; // - optional Buffer, receive data
    speedHz : longint; //  optional number, 32-bit, override of the device's clock frequency in Hertz
    microSecondDelay : smallint; // - optional number, 16-bit, delay after the last bit transfer before optionally deselecting the device before the next transfer, default 0
    bitsPerWord : byte; //  optional number, 8-bit, override of the device's wordsize
    chipSelectChange : Boolean; // - optional boolean, true to deselect device before starting the next transfer, default false
  end;

  TSPIDeviceMessages = Array of TSPIDeviceMessage;

  TNJSCompletionCallBack = reference to procedure (err : TJSError; msg : TSPIDeviceMessage);

  TSPIDeviceOptions = class external name 'Object' (TJSObject)
    mode : byte;
    chipSelectHigh : boolean;
    lsbFirst : boolean;
    threeWire : boolean;
    loopback : boolean;
    noChipSelect : boolean;
    ready : boolean;
    bitsPerWord : boolean;
    maxSpeedHz : longint;
  end;
  TNJSDeviceOptionsCallBack = reference to procedure (err : TJSError; opts : TSPIDeviceOptions);

  TNJSSPIDevice = class external name 'Object' (TJSObject)
    function transfer(message : TSPIDeviceMessages; callback :  TNJSCompletionCallBack) : TNJSSPIDevice;
    function transferSync(message : TSPIDeviceMessages) : TNJSSPIDevice;
    function getOptions(callback :  TNJSDeviceOptionsCallBack) : TNJSSPIDevice;
    function getOptionsSync : TSPIDeviceOptions;
    function setOptions(aOptions : TSPIDeviceOptions; callback :  TNJSErrorCallBack) : TNJSSPIDevice;
    function setOptionsSync(aOptions : TSPIDeviceOptions) : TNJSSPIDevice;
    procedure close(callback :  TNJSErrorCallBack);
    procedure closeSync;
  end;
  TNJSSPIDeviceClass = class of TNJSSPIDevice;

  TNSJSSPIdeviceModule = class external name 'Object' (TJSObject)
  Public
    var
      MODE0 : byte;
      MODE1 : byte;
      MODE2 : byte;
      MODE3 : byte;
  Public
    function open (busNumber, deviceNumber : Byte; CallBack: TNJSErrorCallBack) :  TNJSSPIDevice;
    function open (busNumber, deviceNumber : Byte; Options : TSPIDeviceOptions; CallBack: TNJSErrorCallBack) :  TNJSSPIDevice;
    function openSync (busNumber, deviceNumber : Byte; Options : TSPIDeviceOptions) :  TNJSSPIDevice;
    function openSync (busNumber, deviceNumber : Byte) :  TNJSSPIDevice;
  end;

var
  spi : TNSJSSPIdeviceModule;

implementation

initialization
  spi:=TNSJSSPIdeviceModule(require('spi-device'));
end.

