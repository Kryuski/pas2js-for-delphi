{
    This file is part of the Pas2JS run time library.
    Copyright (c) 2020 by Michael Van Canneyt

    OpenTok.js import classes.

    Actual Opentok API is copyright Tokbox/Nexmo/Vonage

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit libopentok;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  JS, web, types;

Type
  TJSMediaStreamTrack = class external name 'MediaStreamTrack' (TJSObject);

{ ---------------------------------------------------------------------
  Forward declarations
  ---------------------------------------------------------------------}

  TOT = Class;
  TOTArchiveEvent = Class;
  TOTAudioLevelUpdatedEvent = Class;
  TOTCapabilities = Class;
  TOTConnection = Class;
  TOTConnectionEvent = Class;
  TOTDevice = Class;
  TOTError = Class;
  TOTEvent = Class;
  TOTEventDispatcher = Class;
  TOTExceptionEvent = Class;
  TOTGetDevicesResponse = Class;
  TOTGetUserMediaOptions = Class;
  TOTInitPublisherOptions = Class;
  TOTInitSessionOptions = Class;
  TOTMediaStoppedEvent = Class;
  TOTPublisher = Class;
  TOTPublisherStats = Class;
  TOTPublisherStyleOptions = Class;
  TOTScreenSharingCapabilities = Class;
  TOTSession = Class;
  TOTSessionConnectEvent = Class;
  TOTSessionDisonnectEvent = Class;
  TOTSignalEvent = Class;
  TOTStream = Class;
  TOTStreamEvent = Class;
  TOTStreamPropertyChangedEvent = Class;
  TOTSubscriber = Class;
  TOTSubscriberStats = Class;
  TOTSubscriberStyleOptions = Class;
  TOTSupportedCodecsResponse = Class;
  TOTUIStyle = Class;
  TOTVideoElementCreatedEvent = Class;
  TOTVideoEnabledChangedEvent = Class;

  TOTConnectionDynArray = Array of TOTConnection;
  TOTStreamDynArray = Array of TOTStream;

{ ---------------------------------------------------------------------
  Events
  ---------------------------------------------------------------------}


  TOTEvent = Class external name 'ArchiveEvent' (TJSEvent)
  Public
    Function isDefaultPrevented : Boolean;
  end;

  { TOTStreamEvent }

  TOTBaseStreamEvent  = Class external name 'StreamEvent' (TOTEvent)
  private
    FStream: TOTStream; external name 'stream';
  public
    Property Stream : TOTStream read FStream;
  end;

  { TOTStreamEvent }

  TOTStreamEvent  = Class external name 'StreamEvent' (TOTBaseStreamEvent)
  private
    Fcancelable: Boolean; external name 'cancelable';
    FReason: string; external name 'reason';
  Public
    Property Cancelable : Boolean Read Fcancelable;
    Property Reason : string read FReason;
  end;

  { TOTStreamPropertyChangedEvent }

  TOTStreamPropertyChangedEvent = Class external name 'StreamPropertyChangedEvent' (TOTBaseStreamEvent)
  Private
    FChangedProperty: string; external name 'changedProperty';
    FNewValue: TJSObject; external name 'newValue';
    FOldValue: TJSObject; external name 'oldValue';
  Public
    Property ChangedProperty : string read FChangedProperty;
    Property NewValue : TJSObject Read FNewValue;
    Property OldValue : TJSObject Read FOldValue;
  end;

  { TOTSessionConnectEvent }

  TOTSessionConnectEvent  = Class external name 'SessionConnect' (TOTEvent)
  private
    FReason: string; external name 'reason';
  Public
    Property Reason : string read FReason;
  end;

  { TOTSessionDisonnectEvent }

  TOTSessionDisonnectEvent  = Class external name 'SessionDisconnect' (TOTEvent)
  private
    FConnections: TOTConnectionDynArray; external name 'connections';
    FStreams: TOTStreamDynArray; external name 'streams';
  Public
    Property Connections : TOTConnectionDynArray Read FConnections;
    Property Streams : TOTStreamDynArray Read FStreams;
  end;

  { TOTSignalEvent }

  TOTSignalEvent  = Class external name 'SignalEvent' (TOTEvent)
  private
    FData: string; external name 'data';
    FFrom: TOTConnection; external name 'from';
    Ftype: string; external name 'type';
  Public
    Property Data : string read FData;
    Property From : TOTConnection Read FFrom;
    Property Type_ : string read Ftype;
  end;

  { TOTAudioLevelUpdatedEvent }

  TOTAudioLevelUpdatedEvent = Class external name 'AudioLevelUpdatedEvent' (TOTEvent)
  private
    faudiolevel: Double;  external name 'audioLevel';
  Public
    Property AudioLevel : Double read faudiolevel;
  end;

  { TOTConnectionEvent }

  TOTConnectionEvent = Class external name 'ConnectionEvent' (TOTEvent)
  private
    fconnection: TOTConnection;external name 'connection';
    freason: string;external name 'reason';
  Public
    Property Connection : TOTConnection read fconnection;
    Property Reason : string read freason;
  end;

  { TOTExceptionEvent }

  TOTExceptionEvent = Class external name 'ExceptionEvent' (TOTEvent)
  private
    FCode: nativeint;external name 'code';
    FMessage: string;external name 'message';
    FTitle: string;external name 'title';
  Public
    Property Code : nativeint read FCode;
    Property Message : string read FMessage;
    Property Title : string read FTitle;
  end;

  { TOTMediaStoppedEvent }

  TOTMediaStoppedEvent = Class external name 'MediaStoppedEvent' (TOTEvent)
  private
    FMessage: string; external name 'message';
    FTitle: string; external name 'title';
    FTrack: TJSMediaStreamTrack; external name 'track';
  Public
    Property Message : string read FMessage;
    Property Title : string Read FTitle;
    Property Track : TJSMediaStreamTrack Read FTrack;
  end;

  { TOTArchiveEvent }

  TOTArchiveEvent = Class external name 'ArchiveEvent' (TOTEvent)
  private
    FID: string; external name 'id';
    FName: string; external name 'name';
  Public
    Property ID : string read FID;
    Property Name : string read FName;
  end;

  { TOTVideoElementCreatedEvent }

  TOTVideoElementCreatedEvent = Class external name 'VideoElementCreatedEvent' (TOTEvent)
  private
    Felement: TJSHTMLElement; external name 'element';
  Public
    Property Element : TJSHTMLElement Read Felement;
  end;

  { TOTVideoEnabledChangedEvent }

  TOTVideoEnabledChangedEvent = Class external name 'VideoEnabledChangedEvent' (TOTEvent)
  private
    FCancelable: Boolean; external name 'cancelable';
    FReason: string; external name 'reason';
    FTarget: TJSObject; external name 'target';
    FType: string; external name 'type';
  Public
    Property Cancelable : Boolean Read FCancelable;
    Property Reason : string Read FReason;
    Property Target : TJSObject Read FTarget;
    Property Type_ : string read FType;
  end;

  { ---------------------------------------------------------------------
    Real classes
    ---------------------------------------------------------------------}


  { TOTError }

  TOTError = class external name 'Error' (TJSError)
  Private
    FCode: nativeint;  external name 'code';
    FName: string; external name 'name';
  Public
    Property Code : nativeint read FCode Write FCode;
    Property name : string read FName Write FName;
  end;
  TOTErrorHandler = reference to procedure(aErr : TOTError);

  { TOTEventDispatcher }

  TOTEventDispatcher = Class external name 'EventDispatcher' (TJSObject)
  Public
    Procedure off(aType : string; aHandler : TJSEventHandler); overload;
    Procedure off(aType : string); overload;
    Procedure off(aTypes : TJSObject; aContext : JSValue); overload;
    Procedure off(aTypes : TJSObject); overload;
    Function on_(aType : string; aHandler : TJSEventHandler) : TOTEventDispatcher; overload; external name 'on';
    Function on_(aType : string; aHandler : TJSEventHandler; aContext : JSValue): TOTEventDispatcher; overload; external name 'on';
    Function on_(aType : string; aHandler : TJSRawEventHandler) : TOTEventDispatcher; overload; external name 'on';
    Function on_(aType : string; aHandler : TJSRawEventHandler; aContext : JSValue): TOTEventDispatcher; overload; external name 'on';
    Function once(aType : string; aHandler : TJSRawEventHandler) : TJSObject; overload;
    Function once(aType : string; aHandler : TJSRawEventHandler; aContext : JSValue) : TJSObject; overload;
    Function once(aType : string; aHandler : TJSEventHandler) : TJSObject; overload;
    Function once(aType : string; aHandler : TJSEventHandler; aContext : JSValue) : TJSObject; overload;
  end;

  TOTConnection  = Class external name 'Connection' (TJSObject)
  Private
    FconnectionId : string; external name 'connectionId';
    FcreationTime : NativeInt;  external name 'creationTime';
    Fdata : string; external name 'data';
  Public
    Property connectionId : string Read FconnectionId Write FconnectionId;
    Property creationTime : NativeInt Read FcreationTime Write FcreationTime;
    Property data : string Read Fdata Write Fdata;
  end;

  TOTMediaResolution = Record
    height : nativeint;
    width : nativeint;
  end;

  { TOTStream }

  TOTStream = Class external name 'Stream' (TJSObject)
  private
    FConnection: TOTConnection; external name 'connection';
    FCreationTime: NativeInt; external name 'creationTime';
    FFrameRate: NativeInt; external name 'frameRate';
    FHasAudio: Boolean; external name 'hasAudio';
    FHasVideo: Boolean; external name 'hasVideo';
    FName: string; external name 'name';
    FStreamID: string; external name 'streamId';
    FvideoDimensions: TOTMediaResolution; external name 'videoDimensions';
    FVideoType: string; external name 'videoType';
  Public
    Property Connection : TOTConnection Read FConnection;
    Property CreationTime : NativeInt Read FCreationTime;
    Property FrameRate : NativeInt Read FFrameRate;
    Property HasAudio : Boolean Read FHasAudio;
    Property HasVideo : Boolean Read FHasVideo;
    Property Name : string Read FName;
    Property StreamId : string Read FStreamID;
    Property VideoDimensions : TOTMediaResolution Read FvideoDimensions;
    Property VideoType : string Read FVideoType;
  end;

  TOTCapabilities = Class external name 'Capabilities' (TJSObject)
  Private
    FforceDisconnect : byte; external name 'forceDisconnect';
    FforceUnpublish : byte; external name 'forceUnpublish';
    Fpublish : byte; external name 'publish';
    Fsubscribe : byte; external name 'subscribe';
  Public
    Property forceDisconnect : byte Read FforceDisconnect Write FforceDisconnect;
    Property forceUnpublish : byte Read FforceUnpublish Write FforceUnpublish;
    Property publish : byte Read Fpublish Write Fpublish;
    Property subscribe : byte Read Fsubscribe Write Fsubscribe;
  end;

  { TOTScreenSharingCapabilities }

  TOTScreenSharingCapabilities = Class external name 'Object' (TJSOBject)
  private
    FextensionRegistered: Boolean; external name 'extensionRegistered';
    FextensionInstalled: boolean; external name 'extensionInstalled';
    FextensionRequired: string; external name 'extensionRequired';
    FSupported: boolean;external name 'supported';
    FsupportedSources: TJSObject;external name 'supportedSources';
  Public
    Property extensionInstalled : boolean Read FextensionInstalled;
    Property supported : boolean read FSupported;
    Property supportedSources : TJSObject read FsupportedSources ; deprecated;
    Property extensionRequired : string read FextensionRequired;
    Property extensionRegistered : Boolean read FextensionRegistered;
  end;
  TOTScreenSharingCapabilityCallback = reference to Procedure (aResponse : TOTScreenSharingCapabilities);

  { TOTDevice }

  TOTDevice = class external name 'Object' (TJSOBject)
  private
    FDeviceID: string; external name 'deviceId';
    FKind: string; external name 'kind';
    FLabel: String; external name 'label';
  Public
    Property DeviceId : string read FDeviceID;
    Property Kind : string read FKind;
    Property Label_ : String read FLabel;
  end;
  TOTDeviceDynArray = Array of TOTDevice;

  { TOTGetDevicesResponse }

  TOTGetDevicesResponse = Class external name 'Object' (TJSOBject)
  private
    FDevices: TOTDeviceDynArray; external name 'devices';
    FError: TOTError; external name 'devices';
  Public
    Property Devices : TOTDeviceDynArray read FDevices;
    Property Error : TOTError read FError;
  end;
  TOTGetDevicesCallback = reference to Procedure (aResponse : TOTGetDevicesResponse);

  { TOTSupportedCodecsResponse }

  TOTSupportedCodecsResponse = class external name 'Object' (TJSObject)
  private
    FvideoDecoders: TStringDynArray; external name 'videoDecoders';
    FVideoEncoders: TStringDynArray; external name 'videoEncoders';
  public
    Property VideoDecoders : TStringDynArray Read FvideoDecoders;
    Property VideoEncoders : TStringDynArray read FVideoEncoders;
  end;

  TOTGetUserMediaOptions = class external name 'Object' (TJSObject)
  Private
    FaudioSource : JSValue; external name 'audioSource';
    FaudioSourceString : String; external name 'audioSource';
    FaudioSourceBoolean : boolean; external name 'audioSource';
    FaudioSourceTrack : TJSMediaStreamTrack; external name 'audioSource';
    FenableStereo : Boolean; external name 'enableStereo';
    FdisableAudioProcessing : Boolean;  external name 'disableAudioProcessing';
    FfacingMode : string; external name 'facingMode';
    FframeRate : Byte;external name 'frameRate';
    FmaxResolution : TOTMediaResolution; external name 'maxResolution';
    Fresolution : string; external name 'resolution';
    FvideoSource : JSValue; external name 'videoSource';
    FvideoSourceString : String; external name 'videoSource';
    FvideoSourceBoolean : boolean; external name 'videoSource';
    FvideoSourceTrack : TJSMediaStreamTrack; external name 'videoSource';
  Public
    Property AudioSource : JSValue Read FaudioSource Write FaudioSource;
    Property AudioSourceString : String Read FaudioSourceString Write FaudioSourceString;
    Property AudioSourceBoolean : boolean Read FaudioSourceBoolean Write FaudioSourceBoolean;
    Property AudioSourceTrack : TJSMediaStreamTrack Read FaudioSourceTrack Write FaudioSourceTrack;
    Property DisableAudioProcessing : Boolean Read FdisableAudioProcessing Write FdisableAudioProcessing;
    Property EnableStereo : Boolean Read FenableStereo Write FenableStereo;
    Property FacingMode : string Read FfacingMode Write FfacingMode;
    Property FrameRate : Byte Read FframeRate Write FframeRate;
    Property MaxResolution : TOTMediaResolution Read FmaxResolution Write FmaxResolution;
    Property Resolution : string Read Fresolution Write Fresolution;
    Property VideoSource : JSValue Read FvideoSource Write FvideoSource;
    Property VideoSourceString : String Read FvideoSourceString Write FvideoSourceString;
    Property VideoSourceBoolean : boolean Read FvideoSourceBoolean Write FvideoSourceBoolean;
    Property VideoSourceTrack : TJSMediaStreamTrack Read FvideoSourceTrack Write FvideoSourceTrack;
  end;

  TOTUIStyle = Class external name 'Object' (TJSObject)
  Public
    FarchiveStatusDisplayMode : string; external name 'archiveStatusDisplayMode';
    FaudioLevelDisplayMode : string; external name 'audioLevelDisplayMode';
    FbackgroundImageURI : String; external name 'backgroundImageURI';
    FbuttonDisplayMode : String; external name 'buttonDisplayMode';
    FnameDisplayMode : string; external name 'nameDisplayMode';
  Public
    Property ArchiveStatusDisplayMode : string Read FarchiveStatusDisplayMode Write FarchiveStatusDisplayMode;
    Property AudioLevelDisplayMode : string Read FaudioLevelDisplayMode Write FaudioLevelDisplayMode;
    Property BackgroundImageURI : String Read FbackgroundImageURI Write FbackgroundImageURI;
    Property ButtonDisplayMode : String Read FbuttonDisplayMode Write FbuttonDisplayMode;
    Property NameDisplayMode : string Read FnameDisplayMode Write FnameDisplayMode;
  end;

  TOTInitPublisherOptions = Class external name 'Object' (TOTGetUserMediaOptions)
  Private
    FaudioBitrate : Nativeint; external name 'audioBitrate';
    FaudioFallbackEnabled : Boolean;  external name 'audioFallbackEnabled';
    FfitMode: string; external name 'fitMode';
    Fheight : NativeInt; external name 'height';
    FheightString : String; external name 'height'; // Same as height but as string;
    FinsertDefaultUI : Boolean; external name 'insertDefaultUI';
    FinsertMode : string; external name 'insertMode';
    Fmirror : boolean; external name 'mirror';
    Fname : string;external name 'name';
    FpublishAudio : Boolean; external name 'publishAudio';
    FpublishVideo : Boolean; external name 'publishVideo';
    FshowControls : Boolean; external name 'showControls';
    Fstyle : TOTUIStyle; external name 'style';
    FusePreviousDeviceSelection : Boolean;   external name 'usePreviousDeviceSelection';
    Fwidth : NativeInt; external name 'width';
    FwidthString : String; external name 'width'; // Same as width but as string;
  Public
    Property AudioBitrate : Nativeint Read FaudioBitrate Write FaudioBitrate ;
    Property AudioFallbackEnabled : Boolean Read FaudioFallbackEnabled Write FaudioFallbackEnabled ;
    Property FitMode: string Read FfitMode Write FfitMode ;
    Property Height : NativeInt Read Fheight Write Fheight ;
    Property HeightString : String Read FheightString Write FheightString ;
    Property InsertDefaultUI : Boolean Read FinsertDefaultUI Write FinsertDefaultUI ;
    Property InsertMode : string Read FinsertMode Write FinsertMode ;
    Property Mirror : boolean Read Fmirror Write Fmirror ;
    Property Name : string Read Fname Write Fname ;
    Property PublishAudio : Boolean Read FpublishAudio Write FpublishAudio ;
    Property PublishVideo : Boolean Read FpublishVideo Write FpublishVideo ;
    Property ShowControls : Boolean Read FshowControls Write FshowControls ;
    Property Style : TOTUIStyle Read Fstyle Write Fstyle ;
    Property UsePreviousDeviceSelection : Boolean Read FusePreviousDeviceSelection Write FusePreviousDeviceSelection ;
    Property Width : NativeInt Read Fwidth Write Fwidth ;
    Property WidthString : String Read FwidthString Write FwidthString ;
  end;

  TOTInitPublisherCallback = TOTErrorHandler;

  TOTInitSessionOptions = Class external name 'Object' (TJSObject)
  Protected
    FconnectionEventsSuppressed : Boolean; external name 'connectionEventsSuppressed';
    FipWhitelist : boolean; external name 'ipWhitelist';
    FiceConfig : TJSObject; external name 'iceConfig';
  Public
    Property ConnectionEventsSuppressed : Boolean Read FconnectionEventsSuppressed Write FconnectionEventsSuppressed ;
    Property IPWhitelist : boolean Read FipWhitelist Write FipWhitelist ;
    Property IceConfig : TJSObject Read FiceConfig Write FiceConfig ;
  end;

  TOTSubscriberAudioStats = record
    bytesReceived : NativeInt;
    packetsLost : NativeInt;
    packetsReceived : NativeInt;
  end;

  TOTSubscriberVideoStats = record
    bytesReceived : NativeInt;
    frameRate : NativeInt;
    packetsLost : NativeInt;
    packetsReceived : NativeInt;
  end;

  { TOTSubscriberStats }

  TOTSubscriberStats = Class external name 'Object' (TJSObject)
  private
    Faudio: TOTSubscriberAudioStats; external name 'audio';
    FTimeStamp: NativeInt; external name 'timestamp';
    Fvideo: TOTSubscriberVideoStats; external name 'video';
  Public
    Property audio : TOTSubscriberAudioStats Read Faudio;
    Property timestamp : NativeInt Read FTimeStamp;
    Property video : TOTSubscriberVideoStats Read Fvideo;
  end;

  TOTSubscriberStatsCallBack = Reference to Procedure (Error : TOTError; Stats : TOTSubscriberStats);

  TOTSubscriberStyleOptions = class external name 'Object' (TJSObject)
  Private
    FaudioBlockedDisplayMode : string; external name 'audioBlockedDisplayMode';
    FaudioLevelDisplayMode : string; external name 'audioLevelDisplayMode';
    FarchiveStatusDisplayMode : string; external name 'archiveStatusDisplayMode';
    FbackgroundImageURI : String; external name 'backgroundImageURI';
    FbuttonDisplayMode : string; external name 'buttonDisplayMode';
    FnameDisplayMode : string; external name 'nameDisplayMode';
    FvideoDisabledDisplayMode : string; external name 'videoDisabledDisplayMode';
  Public
    Property AudioBlockedDisplayMode : string Read FaudioBlockedDisplayMode Write FaudioBlockedDisplayMode ;
    Property AudioLevelDisplayMode : string Read FaudioLevelDisplayMode Write FaudioLevelDisplayMode ;
    Property ArchiveStatusDisplayMode : string Read FarchiveStatusDisplayMode Write FarchiveStatusDisplayMode ;
    Property BackgroundImageURI : String Read FbackgroundImageURI Write FbackgroundImageURI ;
    Property ButtonDisplayMode : string Read FbuttonDisplayMode Write FbuttonDisplayMode ;
    Property NameDisplayMode : string Read FnameDisplayMode Write FnameDisplayMode ;
    Property VideoDisabledDisplayMode : string Read FvideoDisabledDisplayMode Write FvideoDisabledDisplayMode ;
  end;

  { TOTSubscriber }

  TOTSubscriber = Class external name 'Subscriber' (TOTEventDispatcher)
  private
    FElement: TJSHTMLElement; external name 'element';
    FID: String; external name 'id';
    FStream: TOTStream; external name 'stream';
  Public
    Function getAudioVolume : NativeInt;
    Function getImgData : String;
    Procedure getStats(CallBack : TOTSubscriberStatsCallBack);
    Function getStyle : TJSObject;
    Function isAudioBlocked : boolean;
    Function restrictFrameRate(aValue : Boolean) : TOTSubscriber;
    Function setAudioVolume(aValue : NativeInt) : TOTSubscriber;
    Procedure setPreferredFrameRate(aFrameRate : NativeInt);
    Procedure setPreferredResolution(aResolution : TOTMediaResolution);
    Function setStyle(aStyle : TOTSubscriberStyleOptions) : TOTSubscriber; overload;
    Function setStyle(aProp,aValue : String): TOTSubscriber; overload;
    Function subscribeToAudio(aValue : Boolean) : TOTSubscriber;
    Function subscribeToVideo(aValue : Boolean) : TOTSubscriber;
    Function videoHeight : NativeInt;
    Function videoWidth : NativeInt;
  Public
    Property Element : TJSHTMLElement Read FElement;
    Property ID : String Read FID;
    Property Stream : TOTStream Read FStream;
  end;
  TOTSubscriberArray = Array of TOTSubscriber;

  { TOTSession }

  TOTConnectCallBack = TOTErrorHandler;

  TOTSignalData = Class external name 'Object' (TJSObject)
  Private
    Fdata : string; external name 'data';
    FretryAfterReconnect : Boolean;  external name 'retryAfterReconnect';
    Fto : TOTConnection; external name 'to';
    FType : string; external name 'type';
  Public
    Property Data : string Read FData Write FData;
    Property RetryAfterReconnect : Boolean Read FretryAfterReconnect Write FretryAfterReconnect;
    Property To_ : TOTConnection Read FTo Write FTo;
    Property Type_ : string Read FType Write FType;
  end;


  { TJSSubscribeStyle }

  TJSSubscribeStyle = class external name 'Object' (TJSObject)
  Private
    FaudioBlockedDisplayMode : string; external name 'audioBlockedDisplayMode';
    FaudioLeveldDisplayMode : string; external name 'audioLeveldDisplayMode';
    FbackgroundImageURI : String; external name 'backgroundImageURI';
    FbuttonDisplayMode : string; external name 'buttonDisplayMode';
    FnameDisplayMode : string; external name 'nameDisplayMode';
    FvideoDisabledDisplayMode : String;  external name 'videoDisabledDisplayMode';
  Public
    Property AudioBlockedDisplayMode : string Read FaudioBlockedDisplayMode Write FaudioBlockedDisplayMode;
    Property AudioLeveldDisplayMode : string Read FaudioLeveldDisplayMode Write FaudioLeveldDisplayMode;
    Property BackgroundImageURI : String Read FbackgroundImageURI Write FbackgroundImageURI;
    Property ButtonDisplayMode : string Read FbuttonDisplayMode Write FbuttonDisplayMode;
    Property NameDisplayMode : string Read FnameDisplayMode Write FnameDisplayMode;
    Property VideoDisabledDisplayMode : String Read FvideoDisabledDisplayMode Write FvideoDisabledDisplayMode;
  end;

  { TOTInitSubscriberOptions }

  TOTInitSubscriberOptions = class external name 'Object' (TJSObject)
  Private
    FaudioVolume : Byte; external name 'audioVolume';
    Ffitmode : string; external name 'fitmode';
    Fheight : NativeInt; external name 'height';
    FheightString : String; external name 'height';
    FinsertDefaultUI : Boolean; external name 'insertDefaultUI';
    FinsertMode : string; external name 'insertMode';
    FpreferredFrameRate : NativeInt; external name 'preferredFrameRate';
    FpreferredResolution : TOTMediaResolution;  external name 'preferredResolution';
    FshowControls : Boolean; external name 'showControls';
    Fstyle : TJSSubscribeStyle; external name 'style';
    FsubscribeToAudio : Boolean; external name 'subscribeToAudio';
    FsubscribeToVideo : Boolean; external name 'subscribeToVideo';
    FtestNetwork : Boolean;  external name 'testNetwork';
    Fwidth : NativeInt;   external name 'width';
    FwidthString : String; external name 'width';
  Public
    Property AudioVolume : Byte read FaudioVolume Write FaudioVolume;
    Property Fitmode : string read Ffitmode Write Ffitmode;
    Property Height : NativeInt read Fheight Write Fheight;
    Property HeightString : String read FheightString Write FheightString; // height as string;
    Property InsertDefaultUI : Boolean read FinsertDefaultUI Write FinsertDefaultUI;
    Property InsertMode : string read FinsertMode Write FinsertMode;
    Property PreferredFrameRate : NativeInt read FpreferredFrameRate Write FpreferredFrameRate;
    Property PreferredResolution : TOTMediaResolution read FpreferredResolution Write FpreferredResolution;
    Property ShowControls : Boolean read FshowControls Write FshowControls;
    Property Style : TJSSubscribeStyle read Fstyle Write Fstyle;
    Property SubscribeToAudio : Boolean read FsubscribeToAudio Write FsubscribeToAudio;
    Property SubscribeToVideo : Boolean read FsubscribeToVideo Write FsubscribeToVideo;
    Property TestNetwork : Boolean read FtestNetwork Write FtestNetwork;
    Property Width : NativeInt read Fwidth Write Fwidth;
    Property WidthString : String read FwidthString Write FwidthString; //  width as string;
  end;


  TOTSession = Class external name 'Session' (TOTEventDispatcher)
  private
    FCapabilities: TOTCapabilities; external name 'capabilities';
    FConnection: TOTConnection; external name 'connection';
    FSessionID: string; external name 'sessionId';
  public
    Procedure connect(aToken : String; callback : TOTConnectCallBack);
    Procedure disconnect;
    Procedure forceDisconnect(aConnection : TOTConnection; callBack : TOTConnectCallBack);
    Procedure forceUnpublish(aStream : TOTStream; callBack : TOTConnectCallBack);
    Function getPublisherForStream(aStream : TOTStream) : TOTPublisher;
    Function getSubscribersForStream(aStream : TOTStream) : TOTSubscriberArray;
    Function publish(aPublisher : TOTPublisher; callBack : TOTErrorHandler) : TOTPublisher; overload;
    Function publish(aPublisher : TOTPublisher) : TOTPublisher; overload;
    Procedure signal(aSignal : TJSObject; callBack :TOTErrorHandler);
    Procedure signal(aSignal : TOTSignalData; callBack :TOTErrorHandler);
    Function subscribe(stream : TOTStream; target : TJSHTMLElement; aProperties : TOTInitSubscriberOptions; callBack : TOTErrorHandler) : TOTSubscriber; overload;
    Function subscribe(stream : TOTStream; targetID : String; aProperties : TOTInitSubscriberOptions; callBack : TOTErrorHandler) : TOTSubscriber; overload;
    Procedure unpublish(aPublisher : TOTPublisher);
    Procedure unsubscribe(aSubscriber : TOTSubscriber);
  Public
    Property Capabilities : TOTCapabilities Read FCapabilities;
    Property Connnection : TOTConnection Read FConnection;
    Property SessionId : string Read FSessionID;
  end;

  TOTPublisherAudioStats = record
    bytesSent : NativeInt;
    packetsLost : NativeInt;
    packetsSent : NativeInt;
  end;

  TOTPublisherVideoStats = record
    bytesSent : NativeInt;
    frameRate : NativeInt;
    packetsLost : NativeInt;
    packetsSent : NativeInt;
  end;

  TOTPublisherStatsRecord = record
    audio : TOTPublisherAudioStats;
    timestamp : NativeInt;
    video : TOTPublisherVideoStats;
  end;

  { TOTPublisherStats }

  TOTPublisherStats = class external name 'Object' (TJSObject)
  private
    FConnectionID: String; external name 'connectionId';
    FStats: TOTPublisherStatsRecord; external name 'stats';
    FSubscriberID: String; external name 'subscriberId';
  Public
    Property ConnectionId : String read FConnectionID;
    Property Stats : TOTPublisherStatsRecord read FStats;
    Property SubscriberId : String Read FSubscriberID;
  end;
  TOTPublisherStatsArray = Array of TOTPublisherStats;

  TOTPublisherStatsCallback = Reference to Procedure (error : TOTError; StatsArray : TOTPublisherStatsArray);

  { TOTPublisherStyleOptions }

  TOTPublisherStyleOptions = class external name 'Object' (TJSObject)
  Private
    FarchiveStatusDisplayMode : string; external name 'archiveStatusDisplayMode';
    FaudioLevelDisplayMode : string; external name 'audioLevelDisplayMode';
    FbackgroundImageURI : String; external name 'backgroundImageURI';
    FbuttonDisplayMode : string; external name 'buttonDisplayMode';
    FnameDisplayMode : string; external name 'nameDisplayMode';
  Public
    Property ArchiveStatusDisplayMode : string Read FArchiveStatusDisplayMode Write FArchiveStatusDisplayMode;
    Property AudioLevelDisplayMode : string Read FaudioLevelDisplayMode Write FaudioLevelDisplayMode;
    Property BackgroundImageURI : String Read FbackgroundImageURI Write FbackgroundImageURI;
    Property ButtonDisplayMode : string Read FbuttonDisplayMode Write FbuttonDisplayMode;
    Property NameDisplayMode : string Read FnameDisplayMode Write FnameDisplayMode;
  end;

  { TOTPublisher }

  TOTPublisher = Class external name 'Publisher' (TOTEventDispatcher)
  private
    FaccessAllowed: Boolean; external name 'accessAllowed';
    Felement: TJSHTMLElement; external name 'element';
    FID: string;external name 'id';
    FSession: TOTSession;external name 'session';
    FStream: TOTStream;external name 'stream';
  public
    Function cycleVideo : TJSPromise;
    Function destroy : TOTPublisher;
    Function getAudioSource : TJSMediaStreamTrack;
    Function getImgData : String;
    Procedure getStats (callback : TOTPublisherStatsCallback);
    Function getStyle : TJSObject;
    Procedure publishAudio(value : Boolean);
    Procedure publishVideo(value : Boolean);
    Function setAudioSource(aID : String) : TJSPromise; overload;
    Function setAudioSource(aStream : TJSMediaStreamTrack) : TJSPromise; overload;
    Function setStyle(aStyle : TOTPublisherStyleOptions) : TOTPublisher; overload;
    Function setStyle(aProp,aValue : String): TOTPublisher; overload;
    Function videoHeight : NativeInt;
    Function videoWidth : NativeInt;
  Public
    Property AccessAllowed : Boolean read FaccessAllowed;
    Property Element : TJSHTMLElement read Felement;
    Property ID : string read FID;
    Property Stream : TOTStream Read FStream;
    Property Session : TOTSession read FSession;
  end;

{ ---------------------------------------------------------------------
  Main class, serves as namespace.
  ---------------------------------------------------------------------}


  { TOT }

  TOTReportIssueCallBack = reference to Procedure (err : TOTError; reportID : string);

  TOT = Class external name 'OT' (TOTEventDispatcher)
  Public
    const NONE : Integer;
    const ERROR : Integer;
    const WARN : Integer;
    const INFO : Integer;
    const LOG_ : Integer; // Cannot use external name :(
    const DEBUG : Integer;
  Public
    class Procedure checkScreenSharingCapability(callBack : TOTScreenSharingCapabilityCallback);
    class Function checkSystemRequirements : NativeInt;
    class Procedure getDevices(callBack : TOTGetDevicesCallback);
    class Function getSupportedCodecs : TJSPromise;
    class Function getUserMedia : TJSPromise;
    class Function getUserMedia(options : TOTGetUserMediaOptions) : TJSPromise;
    class Function initPublisher(aElement : TJSHTMLElement; properties : TOTInitPublisherOptions; CompletionHandler : TOTInitPublisherCallback) : TOTPublisher;
    class Function initPublisher(aElementId : String; properties : TOTInitPublisherOptions; CompletionHandler : TOTInitPublisherCallback) : TOTPublisher;
    class Function initSession(APIKey,SessionID  : String) : TOTSession; overload;
    class Function initSession(APIKey,SessionID  : String; Options : TOTInitSessionOptions) : TOTSession; overload;
    class Procedure log(msg : string);
    class Procedure registerScreenSharingExtension(kind : String; id : string; version : Integer);
    class Procedure reportIssue(aCallBack : TOTReportIssueCallBack);
    class Procedure setLogLevel(Number : String);
    class Procedure unblockAudio;
    class Procedure upgradeSystemRequirements;
   end;
   TOTClass = Class of TOT;

var
   OpenTok : TOTClass; external name 'OT';

implementation

end.

