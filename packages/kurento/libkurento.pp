{
    This file is part of the Pas2JS run time library.
    Copyright (C) 2020 Michael Van Canneyt

    Kurento import classes for pas2js

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
 
unit libkurento;

{$mode objfpc}
{$modeswitch externalclass}

interface

uses
  Types, JS, Web, node.events;

Type

{ ---------------------------------------------------------------------
  EventEmitter
  ---------------------------------------------------------------------}

  TKJSEventEmitterHandler = TNJSEventEmitterHandler;
  TKJSEventEmitter = TNJSEventEmitter;

{ ---------------------------------------------------------------------
  Kurento-Client
  ---------------------------------------------------------------------}

Type
  TKurentoMediaObject = Class;
  TKurentoMediaElement = Class;
  TKurentoMediaPipeline = Class;
  TKurentoClient = Class;
  TElementConnectionData = Class;


  TKurentoClientOptions = class external name 'Object' (TJSObject)
    failAfter : Integer;
    enableTransactions : Boolean;
    strict_ : boolean; external name 'strict';
    access_token : string;
    max_retries : integer;
    request_timeout : integer;
    response_timeout : integer;
    duplicates_timeoout : Integer;
    socket : TJSObject;
  end;

  TErrorCallBack = reference to Procedure(aError : TJSError);
  TMediaObjectCallBack = reference to procedure(aError : TJSError; aResult : TKurentoMediaObject);
  TIntegerCallBack = reference to procedure (aError : TJSError; aResult : NativeInt);
  TBooleanCallBack = reference to procedure (aError : TJSError; aResult : Boolean);
  TObjectCallBack = reference to procedure (aError : TJSError; aResult : TJSObject);
  TStringCallBack = reference to procedure (aError : TJSError; aResult : String);
  TFloatCallBack = reference to procedure (aError : TJSError; aResult : Double);
  TArrayCallBack = reference to procedure (aError : TJSError; aResult : TJSArray);
  TMediaPipeLineCallBack = reference to procedure(aError : TJSError; aResult : TKurentoMediaPipeline);
  TElementConnectionDataCallBack = reference to procedure(aError : TJSError; aResult : TElementConnectionData);

  TKurentoMediaObject = class external name 'Object' (TKJSEventEmitter)
    id : string;
    name : string;
    tags : TJSObject;
    Function addTag(aKey,aValue : String; aCallBack : TErrorCallBack) : TJSPromise;
    Function getTag(aKey: String; aCallBack : TStringCallBack) : TJSPromise;
    Function getTags(aKey: String; aCallBack : TArrayCallBack) : TJSPromise;
    Function getChildren(aCallback : TMediaObjectCallBack) : TJSPromise;
    Function getCreationTime(aCallBack : TIntegerCallBack) : TJSPromise;
    Function getMediaPipeLine(aCallBack: TMediaPipeLineCallBack) : TJSPromise;
    Function getName(aCallBack : TStringCallBack) : TJSPromise;
    Function getParent(aCallBack : TMediaObjectCallBack) : TJSPromise;
    Function getSendTagsInEvents(aCallback : TBooleanCallBack) : TJSPromise;
    Function removeTag(aKey: String; aCallBack : TStringCallBack) : TJSPromise;
    Function setName(aCallBack : TErrorCallBack) : TJSPromise;
    Function setSendTagsInEvents(aValue : Boolean;aCallBack : TErrorCallBack) : TJSPromise;
    procedure release; // Not documented but present...
  end;
  TKurentoMediaObjectDynArray = array of TKurentoMediaObject;

  TMediaType = Class external name 'Object' (TJSObject);

  TElementConnectionData = Class external name 'Object' (TJSObject)
    source : TKurentoMediaElement;
    sink : TKurentoMediaElement;
    type_ : TMediaType; external name 'type';
    sourceDescription : String;
    sinkDescription : String;
  end;
  TSinkCallBack = TElementConnectionDataCallBack;

  TKurentoMediaElement = class external name 'Object' (TKurentoMediaObject)
    function connect(sink : TKurentoMediaElement; CallBack : TErrorCallBack) : TJSPromise; overload;
    function connect(sink : TKurentoMediaElement; MediaType : TMediaType; CallBack : TErrorCallBack) : TJSPromise; overload;
    function connect(sink : TKurentoMediaElement; MediaType : TMediaType; SourceDescription : String; CallBack : TErrorCallBack) : TJSPromise; overload;
    function connect(sink : TKurentoMediaElement; MediaType : TMediaType; SourceDescription,SinkDescription : String; CallBack : TErrorCallBack) : TJSPromise; overload;
    function disconnect(sink : TKurentoMediaElement; CallBack : TErrorCallBack) : TJSPromise; overload;
    function disconnect(sink : TKurentoMediaElement; MediaType : TMediaType; CallBack : TErrorCallBack) : TJSPromise;overload;
    function disconnect(sink : TKurentoMediaElement; MediaType : TMediaType; SourceDescription : String; CallBack : TErrorCallBack) : TJSPromise;overload;
    function disconnect(sink : TKurentoMediaElement; MediaType : TMediaType; SourceDescription,SinkDescription : String; CallBack : TErrorCallBack) : TJSPromise;overload;
    function getSinkConnections(callBack : TSinkCallBack) : TJSPromise;overload;
    function getSinkConnections(mediaType : TMediaType; callBack : TSinkCallBack) : TJSPromise;overload;
    function getSinkConnections(mediaType : TMediaType; aDescription : string; callBack : TSinkCallBack) : TJSPromise;overload;
    function getSourceConnections(callBack : TSinkCallBack) : TJSPromise;overload;
    function getSourceConnections(mediaType : TMediaType; callBack : TSinkCallBack) : TJSPromise;overload;
    function getSourceConnections(mediaType : TMediaType; aDescription : string; callBack : TSinkCallBack) : TJSPromise;overload;
    function getStats(callBack : TSinkCallBack) : TJSPromise;overload;
    function getStats(mediaType : TMediaType; callBack : TObjectCallBack) : TJSPromise;overload;
    function isMediaFlowingIn(callBack : TBooleanCallBack) : TJSPromise;overload;
    function isMediaFlowingIn(mediaType : TMediaType; callBack : TBooleanCallBack) : TJSPromise;overload;
    function isMediaFlowingIn(mediaType : TMediaType; Description : String; callBack : TBooleanCallBack) : TJSPromise;overload;
    function isMediaFlowingOut(callBack : TBooleanCallBack) : TJSPromise;overload;
    function isMediaFlowingOut(mediaType : TMediaType; callBack : TBooleanCallBack) : TJSPromise;overload;
    function isMediaFlowingOut(mediaType : TMediaType; Description : String; callBack : TBooleanCallBack) : TJSPromise;overload;
    function isMediaTranscoding(callBack : TBooleanCallBack) : TJSPromise;overload;
    function isMediaTranscoding(mediaType : TMediaType; callBack : TBooleanCallBack) : TJSPromise;overload;
    function isMediaTranscoding(mediaType : TMediaType; Description : String; callBack : TBooleanCallBack) : TJSPromise;overload;
    function setAudioFormat(caps : TJSObject; Callback : TErrorCallBack) : TJSPromise;
    function setOutputBitrate(aRate : NativeInt; Callback : TErrorCallBack) : TJSPromise;
    function setVideoFormat(caps : TJSObject; Callback : TErrorCallBack) : TJSPromise;
    function getMaxOutputBitrate(Callback : TIntegerCallBack) : TJSPromise;
    function getMinOutputBitrate(Callback : TIntegerCallBack) : TJSPromise;
    function setMaxOutputBitrate(aValue : NativeInt;Callback : TErrorCallBack) : TJSPromise;
    function setMinOutputBitrate(aValue : NativeInt;Callback : TErrorCallBack) : TJSPromise;
  end;
  TKurentoMediaElementDynArray = array of TKurentoMediaElement;

  TKurentoServerManager = Class external name 'Object' (TKurentoMediaObject)
    function getCpuCount(aCallBack : TIntegerCallBack) : TJSPromise;
    function getKmd(aCallBack : TStringCallBack) : TJSPromise;
    function getUsedCpu(aCallBack : TFloatCallBack) : TJSPromise;
    function getUsedMemory(aCallBack : TIntegerCallBack) : TJSPromise;
    function getInfo(aCallBack : TObjectCallBack) : TJSPromise;
    function getMetadata(aCallBack : TStringCallBack) : TJSPromise;
    function getPipeLines(aCallBack : TMediaPipeLineCallBack) : TJSPromise;
    function getSessions(aCallBack : TStringCallBack) : TJSPromise;
  end;

  TKurentoCreateCallBack = reference to Procedure(aError : TJSError; aResult : TKurentoMediaObject);
  TKurentoCreateArrayCallBack = reference to Procedure(aError : TJSError; aResult : TKurentoMediaElementDynArray);

  TKurentoMediaPipeline = class external name 'Object' (TKurentoMediaObject)
    // Missing from documentation :-(
    // single
    function create(aType : string; params : TStringDynArray; aCallback: TKurentoCreateCallBack) : TKurentoMediaObject;
    function create(aType : string; aCallback: TKurentoCreateCallBack) : TKurentoMediaObject;
    // Multi
    function create(aType : string; aCallback: TKurentoCreateArrayCallBack) : TKurentoMediaObjectDynArray;
    function create(aType : string; params : TStringDynArray;  aCallback: TKurentoCreateArrayCallBack) : TKurentoMediaObjectDynArray;
  end;

  TKurentoEndPoint = class external name 'Object' (TKurentoMediaElement);
  TKurentoSessionEndPoint = class external name 'Object' (TKurentoEndPoint);

  TKurentoSdpEndPoint = class external name 'Object' (TKurentoSessionEndPoint)
    Function generateOffer(aCallBack : TStringCallBack) : TJSPromise;
    Function getLocalSessionDescriptor(aCallBack : TStringCallBack) : TJSPromise;
    Function getRemoteSessionDescriptor(aCallBack : TStringCallBack) : TJSPromise;
    function processAnswer(answer : String;aCallBack : TStringCallBack) : TJSPromise;
    function processOffer(offer : String;aCallBack : TStringCallBack) : TJSPromise;
    function getMaxAudioRecvBandwidth(aCallBack : TIntegerCallBack): TJSPromise;
    function getMaxVideoRecvBandwidth(aCallBack : TIntegerCallBack): TJSPromise;
    function setMaxAudioRecvBandwidth(aValue : NativeInt; aCallBack : TErrorCallBack): TJSPromise;
    function setMaxVideoRecvBandwidth(aValue : NativeInt; aCallBack : TErrorCallBack): TJSPromise;
  end;

  TKurentoBaseRtpEndPoint = class external name 'Object' (TKurentoSdpEndPoint)
    Function getConnectionState(callback : TObjectCallBack) : TJSPromise;
    // audio Recv/send
    function getMinAudioRecvBandwidth(aCallBack : TIntegerCallBack): TJSPromise;
    function getMinAudioSendBandwidth(aCallBack : TIntegerCallBack): TJSPromise;
    function setMinAudioRecvBandwidth(aValue : NativeInt; aCallBack : TErrorCallBack): TJSPromise;
    function setMinAudioSendBandwidth(aValue : NativeInt; aCallBack : TErrorCallBack): TJSPromise;
    // video Recv/send
    function getMinVideoRecvBandwidth(aCallBack : TIntegerCallBack): TJSPromise;
    function getMinVideoSendBandwidth(aCallBack : TIntegerCallBack): TJSPromise;
    function setMinVideoRecvBandwidth(aValue : NativeInt; aCallBack : TErrorCallBack): TJSPromise;
    function setMinVideoSendBandwidth(aValue : NativeInt; aCallBack : TErrorCallBack): TJSPromise;

    Function getRembParams(callback : TObjectCallBack) : TJSPromise;
    Function setRembParams(aValue : string; Callback : TErrorCallBack) : TJSPromise;
    Function getMtu(Callback : TIntegerCallBack) : TJSPromise;
    Function setMtu(aValue : Integer; Callback : TErrorCallBack) : TJSPromise;
  end;

  TKurentoIceCandidate = class external name 'Object' (TJSObject)
  end;

  TKurentoWebRtcEndPoint = class external name 'Object' (TKurentoBaseRtpEndPoint)
    Constructor new;
    Function addIceCandidate(aCandidate : TKurentoIceCandidate; callback : TErrorCallBack) : TJSPromise;
    Function closeDataChannel(channelID : Integer; callback : TErrorCallBack) : TJSPromise;
    Function createDataChannel(aLabel: String; Ordered : Boolean; MaxPacketLifeTime : Integer; maxRetransMits : Integer;Protocol : string; Callback : TErrorCallBack) : TJSPromise;
    Function createDataChannel(aLabel: String; Ordered : Boolean; MaxPacketLifeTime : Integer; maxRetransMits : Integer; Callback : TErrorCallBack) : TJSPromise;
    Function createDataChannel(aLabel: String; Ordered : Boolean; MaxPacketLifeTime : Integer; Callback : TErrorCallBack) : TJSPromise;
    Function createDataChannel(aLabel: String; Ordered : Boolean; Callback : TErrorCallBack) : TJSPromise;
    Function createDataChannel(aLabel: String; Callback : TErrorCallBack) : TJSPromise;
    Function createDataChannel(Callback : TErrorCallBack) : TJSPromise;
    Function gatherCandidates(Callback : TErrorCallBack) : TJSPromise;
    Function getExternalAddress(Callback : TStringCallBack) : TJSPromise;
    Function getICECandidatePairs(Callback : TObjectCallBack) : TJSPromise;
    Function getIceConnectionState(Callback : TObjectCallBack) : TJSPromise;
    Function getNetworkInterfaces(Callback : TStringCallBack) : TJSPromise;
    Function getStunServerAddress(Callback : TStringCallBack) : TJSPromise;
    Function getStunServerPort(Callback : TIntegerCallBack) : TJSPromise;
    Function getTurnUrl(Callback : TStringCallBack) : TJSPromise;
    Function setExternalAddress(aValue : string; Callback : TErrorCallBack) : TJSPromise;
    Function setNetworkInterfaces(aValue : string; Callback : TErrorCallBack) : TJSPromise;
    Function setStunServerAddress(aValue : string; Callback : TErrorCallBack) : TJSPromise;
    Function setStunServerPort(aValue : Integer; Callback : TErrorCallBack) : TJSPromise;
    Function setTurnUrl(aValue : string; Callback : TErrorCallBack) : TJSPromise;
  end;

  TKurentoClientCallBack = reference to Procedure(aError : TJSError; aClient : TKurentoClient);
  TKurentoErrorCallBack = TErrorCallBack;
  TKurentoServerManagerCallback = reference to Procedure(aError : TJSError; aResult : TKurentoServerManager);

  TKurentoClient = class external name 'kurentoClient.KurentoClient' (TNJSEventEmitter)
    Constructor new(aURL : String; aOptions : TKurentoClientOptions; aCallBack : TKurentoClientCallBack);
    Constructor new(aURL : String; aCallBack : TKurentoClientCallBack);
    procedure beginTransaction;
    procedure endTransaction;
    procedure close;
    function catch(onRejected : TJSPromiseResolver) : TJSPromise;
    function connect(aMedia : TKurentoMediaObject; aCallBack : TKurentoErrorCallBack) : TJSPromise;
    // single
    function create(aType : string; params : TStringDynArray; aCallback: TKurentoCreateCallBack) : TKurentoMediaObject;
    function create(aType : string; aCallback: TKurentoCreateCallBack) : TKurentoMediaObject;
    // Multi
    function create(aType : string; aCallback: TKurentoCreateArrayCallBack) : TKurentoMediaObjectDynArray;
    function create(aType : string; params : TStringDynArray;  aCallback: TKurentoCreateArrayCallBack) : TKurentoMediaObjectDynArray;
    function getComplexType(aType : string) : JSValue;
    function getMediaObjectById(aID : String; aCallBack : TKurentoCreateCallBack) : TJSPromise;
    function getServerManager(aCallBack : TKurentoServerManagerCallBack) : TJSPromise;
    function then_(onfulfilled, onRejected : TJSPromiseResolver) : TJSPromise;
  end;


  TMediaObjectCreator = class external name 'kurentoClient.MediaObjectCreator' (TJSObject)
    constructor new(aHost : String; encodeCreate,encodeRPC,encodeTransaction,describe : JSValue);
    // single
    function create(aType : string; params : TStringDynArray; aCallback: TKurentoCreateCallBack) : TKurentoMediaObject;
    function create(aType : string; aCallback: TKurentoCreateCallBack) : TKurentoMediaObject;
    // Multi
    function create(aType : string; aCallback: TKurentoCreateArrayCallBack) : TKurentoMediaObjectDynArray;
    function create(aType : string; params : TStringDynArray;  aCallback: TKurentoCreateArrayCallBack) : TKurentoMediaObjectDynArray;
  end;

  TKurentoClientGlobal = Class external name 'kurentoClient' (TJSObject)
    MediaObjectCreator : TMediaObjectCreator;
    Class function getComplexType(aType : string) : JSValue;
  end;


Function KurentoClient (aURL : String; aOptions : TKurentoClientOptions; aCallBack : TKurentoClientCallBack) : TJSPromise; external name 'kurentoClient.KurentoClient';
Function KurentoClient (aURL : String; aCallBack : TKurentoClientCallBack) : TJSPromise; external name 'kurentoClient.KurentoClient';

{ ---------------------------------------------------------------------
  Kurento-utils
  ---------------------------------------------------------------------}

Const
  sModeRecv     = 'recv';
  sModeSend     = 'send';
  sModeSendRecv = 'sendRecv';
  sSourceWebCam = 'webcam';
  sSourceScreen = 'screen';
  sSourceWindow = 'window';


Type
  TWebRtcProc = reference to procedure;
  TWebRtcCallBack = reference to procedure(aError : TJSError);

  TPeerConnection = Class external name 'Object' (TJSObject);
  TDataChannel = Class external name 'Object' (TJSObject);
  TKurentoOffer = string;
  TKurentoAnswer = string;

  TKurentoOfferCallBack = reference to Procedure(aError : TJSError; aOffer : TKurentoOffer);
  TKurentoProcessAnswerCallback = reference to Procedure(aError : TJSError; aAnswer : TKurentoAnswer);


  TDataChannelConfig = class external name 'Object' (TJSObject)
    id : string;
    options : TJSObject;
    onopen : TWebRtcProc;
    onclose : TWebRtcProc;
    onmessage : TWebRtcProc;
    onbufferedamountlow : TWebRtcProc;
    onerror : TWebRTCCallBack;
  end;

  TICEConfig = class external name 'Object' (TJSObject)
    iceServers : TJSObjectDynArray;
  end;

  TWebRtcPeerOptions = class external name 'Object' (TJSObject)
    localVideo : TJSHTMLElement;
    remoteVideo : TJSHTMLElement;
    videoStream : TJSObject;
    audioStream : TJSObject;
    mediaConstraints : TJSObject;
    peerConnection : TPeerConnection;
    sendSource : String;
    dataChannels : String;
    dataChannelConfig : TDataChannelConfig;
    onstreamended : TWebRtcProc;
    onicecandidate : TWebRtcProc;
    onicecandidategatheringdone : TWebRtcProc;
    simulcast : boolean;
    configuration : TICEConfig;
  end;

  TWebRtcPeer = class external name 'kurentoUtils.WebRtcPeer' (TKJSEventEmitter)
  Private
    FPeerConnection : TPeerConnection; external name 'peerConnection';
    FID : String; external name 'id';
    FlocalVideo : TJSHTMLElement; external name 'localVideo';
    FRemoteVideo : TJSHTMLElement external name 'remoteVideo';
    FDataChannel : TDataChannel; external name 'dataChannel';
    FCurrentFrame : TJSHTMLCanvasElement ; external name 'currentFrame';
    FaudioEnabled : Boolean; External name 'audioEnabled';
    FvideoEnabled : Boolean; External name 'videoEnabled';
  Public
    Constructor new(Mode : String; Options: TWebRtcPeerOptions; CallBack : TWebRtcCallBack);
    Constructor new(Mode : String; Options: TJSObject; CallBack : TWebRtcCallBack);
    Class function WebRtcPeerRecvOnly(Options: TWebRtcPeerOptions; CallBack : TWebRtcCallBack) : TWebRtcPeer;
    Class function WebRtcPeerSendOnly(Options: TWebRtcPeerOptions; CallBack : TWebRtcCallBack) : TWebRtcPeer;
    Class function WebRtcPeerSendrecv(Options: TWebRtcPeerOptions; CallBack : TWebRtcCallBack) : TWebRtcPeer;
    Class function harkUtils(stream , options : TJSObject) :JSValue;
    Class procedure bufferSizeCandidates(pc : TPeerConnection;CallBack : TWebRtcCallBack);
    Procedure addIceCandidate(aCandidate : TJSObject; callback : TErrorCallBack) ;
    function getPeerConnection : TPeerConnection;
    function getLocalSessionDescriptor : JSValue;
    function getRemoteSessionDescriptor : JSValue;
    procedure setRemoteVideo;
    procedure showLocalVideo;
    procedure send(Data : TJSObject);
    procedure dispose;
    procedure generateOffer(aOfferCallBack : TKurentoOfferCallBack);
    procedure processOffer(aOffer : JSValue; CallBack : TKurentoOfferCallBack);
    procedure processAnswer(aOffer : JSValue; CallBack : TErrorCallBack);
    function getLocalStream(aIndex : integer) : TJSMediaStreamTrack;
    function getRemoteStream(aIndex : integer) : TJSMediaStreamTrack;
    Property PeerConnection : TPeerConnection Read FPeerConnection;
    Property ID : String Read FID;
    Property LocalVideo : TJSHTMLElement Read FlocalVideo;
    Property RemoteVideo : TJSHTMLElement Read FRemoteVideo;
    Property DataChannel : TDataChannel Read FDataChannel;
    Property CurrentFrame : TJSHTMLCanvasElement Read FCurrentFrame;
    Property AudioEnabled : Boolean Read FAudioEnabled;
    Property VideoEnabled : Boolean Read FVideoEnabled;
  end;

implementation

end.

