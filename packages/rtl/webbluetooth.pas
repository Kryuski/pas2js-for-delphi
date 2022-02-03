Unit webbluetooth;

{$MODE ObjFPC}
{$H+}
{$modeswitch externalclass}

interface

uses JS,web;

Type
  // Forward class definitions
  TJSBluetooth = Class;
  TJSBluetoothPermissionResult = Class;
  TJSValueEvent = Class;
  TJSBluetoothDevice = Class;
  TJSBluetoothManufacturerDataMap = Class;
  TJSBluetoothServiceDataMap = Class;
  TJSBluetoothAdvertisingEvent = Class;
  TJSBluetoothRemoteGATTServer = Class;
  TJSBluetoothRemoteGATTService = Class;
  TJSBluetoothRemoteGATTCharacteristic = Class;
  TJSBluetoothCharacteristicProperties = Class;
  TJSBluetoothRemoteGATTDescriptor = Class;
  TJSCharacteristicEventHandlers = Class;
  TJSBluetoothDeviceEventHandlers = Class;
  TJSServiceEventHandlers = Class;
  TJSBluetoothUUID = Class;
  TJSBluetoothDataFilterInit = Class;
  TJSBluetoothLEScanFilterInit = Class;
  TJSRequestDeviceOptions = Class;
  TJSBluetoothPermissionDescriptor = Class;
  TJSAllowedBluetoothDevice = Class;
  TJSBluetoothPermissionData = Class;
  TJSValueEventInit = Class;
  TJSBluetoothAdvertisingEventInit = Class;
  UUID = String;
  // Union of DOMString, unsigned long
  BluetoothServiceUUID = JSValue; 
  // Union of DOMString, unsigned long
  BluetoothCharacteristicUUID = JSValue; 
  // Union of DOMString, unsigned long
  BluetoothDescriptorUUID = JSValue; 
  
  { --------------------------------------------------------------------
    TJSBluetoothDataFilterInit
    --------------------------------------------------------------------}
  
  TJSBluetoothDataFilterInit = class(TJSObject)
    dataPrefix : TJSBufferSource;
    mask : TJSBufferSource;
  end;
  
  { --------------------------------------------------------------------
    TJSBluetoothLEScanFilterInit
    --------------------------------------------------------------------}
  
  TBluetoothServiceUUIDDynArray = Array of BluetoothServiceUUID;
  TJSBluetoothLEScanFilterInit = class(TJSObject)
    services : TBluetoothServiceUUIDDynArray;
    name : String;
    namePrefix : String;
    manufacturerData : TJSObject;
    serviceData : TJSObject;
  end;
  
  { --------------------------------------------------------------------
    TJSRequestDeviceOptions
    --------------------------------------------------------------------}
  
  TTJSBluetoothLEScanFilterInitDynArray = Array of TJSBluetoothLEScanFilterInit;
  TJSRequestDeviceOptions = class(TJSObject)
    filters : TTJSBluetoothLEScanFilterInitDynArray;
    optionalServices : TBluetoothServiceUUIDDynArray;
    acceptAllDevices : boolean;
  end;
  
  { --------------------------------------------------------------------
    TJSBluetoothPermissionDescriptor
    --------------------------------------------------------------------}
  
  TJSBluetoothPermissionDescriptor = class(TJSObject)
    deviceId : String;
    filters : TTJSBluetoothLEScanFilterInitDynArray;
    optionalServices : TBluetoothServiceUUIDDynArray;
    acceptAllDevices : boolean;
  end;
  
  { --------------------------------------------------------------------
    TJSAllowedBluetoothDevice
    --------------------------------------------------------------------}
  
  TJSAllowedBluetoothDevice = class(TJSObject)
    deviceId : String;
    mayUseGATT : boolean;
    allowedServices : JSValue;
  end;
  
  { --------------------------------------------------------------------
    TJSBluetoothPermissionData
    --------------------------------------------------------------------}
  
  TTJSAllowedBluetoothDeviceDynArray = Array of TJSAllowedBluetoothDevice;
  TJSBluetoothPermissionData = class(TJSObject)
    allowedDevices : TTJSAllowedBluetoothDeviceDynArray;
  end;
  
  { --------------------------------------------------------------------
    TJSValueEventInit
    --------------------------------------------------------------------}
  
  TJSValueEventInit = class(TJSObject)
    value : JSValue;
  end;
  
  { --------------------------------------------------------------------
    TJSBluetoothAdvertisingEventInit
    --------------------------------------------------------------------}
  
  TJSValueDynArray = Array of JSValue;
  TJSBluetoothAdvertisingEventInit = class(TJSObject)
    device : TJSBluetoothDevice;
    uuids : TJSValueDynArray;
    name : String;
    appearance : Cardinal;
    txPower : byte;
    rssi : byte;
    manufacturerData : TJSObject;
    serviceData : TJSObject;
  end;
  
  { --------------------------------------------------------------------
    TJSBluetooth
    --------------------------------------------------------------------}
  
  TJSBluetooth = class external name 'Bluetooth'  (TJSEventTarget)
  Private
    FreferringDevice : TJSBluetoothDevice; external name 'referringDevice'; 
  Public
    onavailabilitychanged : TJSEventHandler;
    function getAvailability: TJSPromise;
    function requestDevice(options : TJSRequestDeviceOptions): TJSPromise; overload;
    function requestDevice: TJSPromise; overload;
    Property referringDevice : TJSBluetoothDevice Read FreferringDevice; 
  end;
  
  { --------------------------------------------------------------------
    TJSBluetoothPermissionResult
    --------------------------------------------------------------------}
  
  TTJSBluetoothDeviceDynArray = Array of TJSBluetoothDevice;
  TJSBluetoothPermissionResult = class external name 'BluetoothPermissionResult'  (TJSOBject)
  Private
  Public
    devices : TTJSBluetoothDeviceDynArray;
  end;
  
  { --------------------------------------------------------------------
    TJSValueEvent
    --------------------------------------------------------------------}
  
  TJSValueEvent = class external name 'ValueEvent'  (TJSEvent)
  Private
    Fvalue : JSValue; external name 'value'; 
  Public
    Property value : JSValue Read Fvalue; 
  end;
  
  { --------------------------------------------------------------------
    TJSBluetoothDevice
    --------------------------------------------------------------------}
  
  TJSBluetoothDevice = class external name 'BluetoothDevice' 
  Private
    Fid : String; external name 'id'; 
    Fname : String; external name 'name'; 
    Fgatt : TJSBluetoothRemoteGATTServer; external name 'gatt'; 
    FwatchingAdvertisements : boolean; external name 'watchingAdvertisements'; 
  Public
    function watchAdvertisements: TJSPromise;
    Procedure unwatchAdvertisements;
    Property id : String Read Fid; 
    Property name : String Read Fname; 
    Property gatt : TJSBluetoothRemoteGATTServer Read Fgatt; 
    Property watchingAdvertisements : boolean Read FwatchingAdvertisements; 
  end;
  
  { --------------------------------------------------------------------
    TJSBluetoothManufacturerDataMap
    --------------------------------------------------------------------}
  
  TJSBluetoothManufacturerDataMap = class external name 'BluetoothManufacturerDataMap' 
  Private
  Public
  end;
  
  { --------------------------------------------------------------------
    TJSBluetoothServiceDataMap
    --------------------------------------------------------------------}
  
  TJSBluetoothServiceDataMap = class external name 'BluetoothServiceDataMap' 
  Private
  Public
  end;
  
  { --------------------------------------------------------------------
    TJSBluetoothAdvertisingEvent
    --------------------------------------------------------------------}
  
  TUUIDDynArray = Array of UUID;
  TJSBluetoothAdvertisingEvent = class external name 'BluetoothAdvertisingEvent'  (TJSEvent)
  Private
    Fdevice : TJSBluetoothDevice; external name 'device'; 
    Fuuids : TUUIDDynArray; external name 'uuids'; 
    Fname : String; external name 'name'; 
    Fappearance : Cardinal; external name 'appearance'; 
    FtxPower : byte; external name 'txPower'; 
    Frssi : byte; external name 'rssi'; 
    FmanufacturerData : TJSBluetoothManufacturerDataMap; external name 'manufacturerData'; 
    FserviceData : TJSBluetoothServiceDataMap; external name 'serviceData'; 
  Public
    Property device : TJSBluetoothDevice Read Fdevice; 
    Property uuids : TUUIDDynArray Read Fuuids; 
    Property name : String Read Fname; 
    Property appearance : Cardinal Read Fappearance; 
    Property txPower : byte Read FtxPower; 
    Property rssi : byte Read Frssi; 
    Property manufacturerData : TJSBluetoothManufacturerDataMap Read FmanufacturerData; 
    Property serviceData : TJSBluetoothServiceDataMap Read FserviceData; 
  end;
  
  { --------------------------------------------------------------------
    TJSBluetoothRemoteGATTServer
    --------------------------------------------------------------------}
  
  TJSBluetoothRemoteGATTServer = class external name 'BluetoothRemoteGATTServer' 
  Private
    Fdevice : TJSBluetoothDevice; external name 'device'; 
    Fconnected : boolean; external name 'connected'; 
  Public
    function connect: TJSPromise;
    Procedure disconnect;
    function getPrimaryService(service : BluetoothServiceUUID): TJSPromise;
    function getPrimaryServices(service : BluetoothServiceUUID): TJSPromise; overload;
    function getPrimaryServices: TJSPromise; overload;
    Property device : TJSBluetoothDevice Read Fdevice; 
    Property connected : boolean Read Fconnected; 
  end;
  
  { --------------------------------------------------------------------
    TJSBluetoothRemoteGATTService
    --------------------------------------------------------------------}
  
  TJSBluetoothRemoteGATTService = class external name 'BluetoothRemoteGATTService' 
  Private
    Fdevice : TJSBluetoothDevice; external name 'device'; 
    Fuuid : UUID; external name 'uuid'; 
    FisPrimary : boolean; external name 'isPrimary'; 
  Public
    function getCharacteristic(characteristic : BluetoothCharacteristicUUID): TJSPromise;
    function getCharacteristics(characteristic : BluetoothCharacteristicUUID): TJSPromise; overload;
    function getCharacteristics: TJSPromise; overload;
    function getIncludedService(service : BluetoothServiceUUID): TJSPromise;
    function getIncludedServices(service : BluetoothServiceUUID): TJSPromise; overload;
    function getIncludedServices: TJSPromise; overload;
    Property device : TJSBluetoothDevice Read Fdevice; 
    Property _uuid : UUID Read Fuuid; 
    Property isPrimary : boolean Read FisPrimary; 
  end;
  
  { --------------------------------------------------------------------
    TJSBluetoothRemoteGATTCharacteristic
    --------------------------------------------------------------------}
  
  TJSBluetoothRemoteGATTCharacteristic = class external name 'BluetoothRemoteGATTCharacteristic' 
  Private
    Fservice : TJSBluetoothRemoteGATTService; external name 'service'; 
    Fuuid : UUID; external name 'uuid'; 
    Fproperties : TJSBluetoothCharacteristicProperties; external name 'properties'; 
    Fvalue : TJSDataView; external name 'value'; 
  Public
    function getDescriptor(descriptor : BluetoothDescriptorUUID): TJSPromise;
    function getDescriptors(descriptor : BluetoothDescriptorUUID): TJSPromise; overload;
    function getDescriptors: TJSPromise; overload;
    function readValue: TJSPromise;
    function writeValue(value : TJSBufferSource): TJSPromise;
    function startNotifications: TJSPromise;
    function stopNotifications: TJSPromise;
    Property service : TJSBluetoothRemoteGATTService Read Fservice; 
    Property _uuid : UUID Read Fuuid; 
    Property properties : TJSBluetoothCharacteristicProperties Read Fproperties; 
    Property value : TJSDataView Read Fvalue; 
  end;
  
  { --------------------------------------------------------------------
    TJSBluetoothCharacteristicProperties
    --------------------------------------------------------------------}
  
  TJSBluetoothCharacteristicProperties = class external name 'BluetoothCharacteristicProperties' 
  Private
    Fbroadcast : boolean; external name 'broadcast'; 
    Fread : boolean; external name 'read'; 
    FwriteWithoutResponse : boolean; external name 'writeWithoutResponse'; 
    Fwrite : boolean; external name 'write'; 
    Fnotify : boolean; external name 'notify'; 
    Findicate : boolean; external name 'indicate'; 
    FauthenticatedSignedWrites : boolean; external name 'authenticatedSignedWrites'; 
    FreliableWrite : boolean; external name 'reliableWrite'; 
    FwritableAuxiliaries : boolean; external name 'writableAuxiliaries'; 
  Public
    Property broadcast : boolean Read Fbroadcast; 
    Property read : boolean Read Fread; 
    Property writeWithoutResponse : boolean Read FwriteWithoutResponse; 
    Property write : boolean Read Fwrite; 
    Property notify : boolean Read Fnotify; 
    Property indicate : boolean Read Findicate; 
    Property authenticatedSignedWrites : boolean Read FauthenticatedSignedWrites; 
    Property reliableWrite : boolean Read FreliableWrite; 
    Property writableAuxiliaries : boolean Read FwritableAuxiliaries; 
  end;
  
  { --------------------------------------------------------------------
    TJSBluetoothRemoteGATTDescriptor
    --------------------------------------------------------------------}
  
  TJSBluetoothRemoteGATTDescriptor = class external name 'BluetoothRemoteGATTDescriptor' 
  Private
    Fcharacteristic : TJSBluetoothRemoteGATTCharacteristic; external name 'characteristic'; 
    Fuuid : UUID; external name 'uuid'; 
    Fvalue : TJSDataView; external name 'value'; 
  Public
    function readValue: TJSPromise;
    function writeValue(value : TJSBufferSource): TJSPromise;
    Property characteristic : TJSBluetoothRemoteGATTCharacteristic Read Fcharacteristic; 
    Property _uuid : UUID Read Fuuid; 
    Property value : TJSDataView Read Fvalue; 
  end;
  
  { --------------------------------------------------------------------
    TJSCharacteristicEventHandlers
    --------------------------------------------------------------------}
  
  TJSCharacteristicEventHandlers = class external name 'CharacteristicEventHandlers' 
  Private
  Public
      oncharacteristicvaluechanged : TJSEventHandler;
  end;
  
  { --------------------------------------------------------------------
    TJSBluetoothDeviceEventHandlers
    --------------------------------------------------------------------}
  
  TJSBluetoothDeviceEventHandlers = class external name 'BluetoothDeviceEventHandlers' 
  Private
  Public
      ongattserverdisconnected : TJSEventHandler;
  end;
  
  { --------------------------------------------------------------------
    TJSServiceEventHandlers
    --------------------------------------------------------------------}
  
  TJSServiceEventHandlers = class external name 'ServiceEventHandlers' 
  Private
  Public
      onserviceadded : TJSEventHandler;
      onservicechanged : TJSEventHandler;
      onserviceremoved : TJSEventHandler;
  end;
  
  { --------------------------------------------------------------------
    TJSBluetoothUUID
    --------------------------------------------------------------------}
  
  TJSBluetoothUUID = class external name 'BluetoothUUID' 
  Private
  Public
    function getService(name : JSValue): UUID;
    function getCharacteristic(name : JSValue): UUID;
    function getDescriptor(name : JSValue): UUID;
    function canonicalUUID(alias : NativeInt): UUID;
  end;
  
  { --------------------------------------------------------------------
    TJSNavigator
    --------------------------------------------------------------------}
  
  TJSNavigator = class external name 'Navigator' 
  Private
    Fbluetooth : TJSBluetooth; external name 'bluetooth';
  Public
    Property bluetooth : TJSBluetooth Read Fbluetooth;
  end;

implementation


end.
