Unit webrtc;

{$MODE ObjFPC}
{$H+}
{$modeswitch externalclass}

interface

uses SysUtils, JS,web;

{
  Automatically generated file by TWebIDLToPas on 2020-03-23 14:48:13
  
  Used command-line options : 
  -x
  web
  -i
  /home/michael/webrtc.idl
  -d
  TJSObject
  -t
  DOMTimeStamp=String,Mediastream=TJSHTMLMediaStream,DOMHighResTimeStamp=double,MediaStreamTrack=JSValue,RTCStatsType=String,EventTarget=TJSEventTarget,EventHandler=TJSEventHandler,Promise=TJSPromise,VoidFunction=TProcedure,Event=TJSEvent,Blob=TJSBlob,ArrayBufferView=TJSTypedArray
  -p
  
  Command-line options translate to: 
  
  Options : [coDictionaryAsClass,coaddOptionsToheader]
  Keyword prefix : 
  Keyword suffix : _
  Class prefix : TJS
  Class suffix : 
  Field prefix : F
  WEBIDLversion : v2
  Type aliases:
  DOMTimeStamp=String
  Mediastream=TJSHTMLMediaStream
  DOMHighResTimeStamp=double
  MediaStreamTrack=JSValue
  RTCStatsType=String
  EventTarget=TJSEventTarget
  EventHandler=TJSEventHandler
  Promise=TJSPromise
  VoidFunction=TProcedure
  Event=TJSEvent
  Blob=TJSBlob
  ArrayBufferView=TJSTypedArray
}
Type
  // Forward class definitions
  TJSRTCPeerConnection = Class;
  TJSRTCSessionDescription = Class;
  TJSRTCIceCandidate = Class;
  TJSRTCPeerConnectionIceEvent = Class;
  TJSRTCPeerConnectionIceErrorEvent = Class;
  TJSRTCCertificate = Class;
  TJSRTCRtpSender = Class;
  TJSRTCRtpReceiver = Class;
  TJSRTCRtpTransceiver = Class;
  TJSRTCDtlsTransport = Class;
  TJSRTCIceTransport = Class;
  TJSRTCTrackEvent = Class;
  TJSRTCSctpTransport = Class;
  TJSRTCDataChannel = Class;
  TJSRTCDataChannelEvent = Class;
  TJSRTCDTMFSender = Class;
  TJSRTCDTMFToneChangeEvent = Class;
  TJSRTCStatsReport = Class;
  TJSRTCError = Class;
  TJSRTCErrorEvent = Class;
  TJSRTCConfiguration = Class;
  TJSRTCIceServer = Class;
  TJSRTCOfferAnswerOptions = Class;
  TJSRTCOfferOptions = Class;
  TJSRTCAnswerOptions = Class;
  TJSRTCSessionDescriptionInit = Class;
  TJSRTCIceCandidateInit = Class;
  TJSRTCPeerConnectionIceEventInit = Class;
  TJSRTCPeerConnectionIceErrorEventInit = Class;
  TJSRTCCertificateExpiration = Class;
  TJSRTCRtpTransceiverInit = Class;
  TJSRTCRtpParameters = Class;
  TJSRTCRtpSendParameters = Class;
  TJSRTCRtpReceiveParameters = Class;
  TJSRTCRtpCodingParameters = Class;
  TJSRTCRtpDecodingParameters = Class;
  TJSRTCRtpEncodingParameters = Class;
  TJSRTCRtcpParameters = Class;
  TJSRTCRtpHeaderExtensionParameters = Class;
  TJSRTCRtpCodecParameters = Class;
  TJSRTCRtpCapabilities = Class;
  TJSRTCRtpCodecCapability = Class;
  TJSRTCRtpHeaderExtensionCapability = Class;
  TJSRTCRtpContributingSource = Class;
  TJSRTCRtpSynchronizationSource = Class;
  TJSRTCDtlsFingerprint = Class;
  TJSRTCIceParameters = Class;
  TJSRTCIceCandidatePair = Class;
  TJSRTCTrackEventInit = Class;
  TJSRTCDataChannelInit = Class;
  TJSRTCDataChannelEventInit = Class;
  TJSRTCDTMFToneChangeEventInit = Class;
  TJSRTCStats = Class;
  TJSRTCErrorInit = Class;
  TJSRTCErrorEventInit = Class;
  RTCIceCredentialType = String;
  RTCIceTransportPolicy = String;
  RTCBundlePolicy = String;
  RTCRtcpMuxPolicy = String;
  RTCSignalingState = String;
  RTCIceGatheringState = String;
  RTCPeerConnectionState = String;
  RTCIceConnectionState = String;
  RTCSdpType = String;
  RTCIceProtocol = String;
  RTCIceTcpCandidateType = String;
  RTCIceCandidateType = String;
  RTCRtpTransceiverDirection = String;
  RTCDegradationPreference = String;
  RTCDtlsTransportState = String;
  RTCIceGathererState = String;
  RTCIceTransportState = String;
  RTCIceRole = String;
  RTCIceComponent = String;
  RTCSctpTransportState = String;
  RTCDataChannelState = String;
  RTCErrorDetailType = String;
  RTCPeerConnectionErrorCallback = Procedure (error : TJSError);
  RTCSessionDescriptionCallback = Procedure (description : TJSRTCSessionDescriptionInit);
  
  { --------------------------------------------------------------------
    TJSRTCConfiguration
    --------------------------------------------------------------------}
  
  TTJSRTCIceServerDynArray = Array of TJSRTCIceServer;
  TTJSRTCCertificateDynArray = Array of TJSRTCCertificate;
  TJSRTCConfiguration = class(TJSObject)
    iceServers : TTJSRTCIceServerDynArray;
    iceTransportPolicy : RTCIceTransportPolicy;
    bundlePolicy : RTCBundlePolicy;
    rtcpMuxPolicy : RTCRtcpMuxPolicy;
    peerIdentity : String;
    certificates : TTJSRTCCertificateDynArray;
    iceCandidatePoolSize : Byte;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCIceServer
    --------------------------------------------------------------------}
  
  TJSRTCIceServer = class(TJSObject)
    urls : JSValue;
    username : String;
    credentialType : RTCIceCredentialType;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCOfferAnswerOptions
    --------------------------------------------------------------------}
  
  TJSRTCOfferAnswerOptions = class(TJSObject)
  end;
  
  { --------------------------------------------------------------------
    TJSRTCOfferOptions
    --------------------------------------------------------------------}
  
  TJSRTCOfferOptions = class(TJSObject)
    iceRestart : boolean;
    offerToReceiveAudio : boolean;
    offerToReceiveVideo : boolean;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCAnswerOptions
    --------------------------------------------------------------------}
  
  TJSRTCAnswerOptions = class(TJSObject)
  end;
  
  { --------------------------------------------------------------------
    TJSRTCSessionDescriptionInit
    --------------------------------------------------------------------}
  
  TJSRTCSessionDescriptionInit = class(TJSObject)
    type_ : RTCSdpType;external name 'type';
    sdp : String;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCIceCandidateInit
    --------------------------------------------------------------------}
  
  TJSRTCIceCandidateInit = class(TJSObject)
    candidate : String;
    sdpMid : String;
    sdpMLineIndex : Cardinal;
    usernameFragment : String;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCPeerConnectionIceEventInit
    --------------------------------------------------------------------}
  
  TJSRTCPeerConnectionIceEventInit = class(TJSObject)
    candidate : TJSRTCIceCandidate;
    url : String;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCPeerConnectionIceErrorEventInit
    --------------------------------------------------------------------}
  
  TJSRTCPeerConnectionIceErrorEventInit = class(TJSObject)
    hostCandidate : String;
    url : String;
    errorCode : Cardinal;
    statusText : String;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCCertificateExpiration
    --------------------------------------------------------------------}
  
  TJSRTCCertificateExpiration = class(TJSObject)
    expires : String;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCRtpTransceiverInit
    --------------------------------------------------------------------}
  
  TTJSHTMLMediaStreamDynArray = Array of TJSHTMLMediaStream;
  TTJSRTCRtpEncodingParametersDynArray = Array of TJSRTCRtpEncodingParameters;
  TJSRTCRtpTransceiverInit = class(TJSObject)
    direction : RTCRtpTransceiverDirection;
    streams : TTJSHTMLMediaStreamDynArray;
    sendEncodings : TTJSRTCRtpEncodingParametersDynArray;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCRtpParameters
    --------------------------------------------------------------------}
  
  TTJSRTCRtpHeaderExtensionParametersDynArray = Array of TJSRTCRtpHeaderExtensionParameters;
  TTJSRTCRtpCodecParametersDynArray = Array of TJSRTCRtpCodecParameters;
  TJSRTCRtpParameters = class(TJSObject)
    headerExtensions : TTJSRTCRtpHeaderExtensionParametersDynArray;
    rtcp : TJSRTCRtcpParameters;
    codecs : TTJSRTCRtpCodecParametersDynArray;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCRtpSendParameters
    --------------------------------------------------------------------}
  
  TJSRTCRtpSendParameters = class(TJSObject)
    transactionId : String;
    encodings : TTJSRTCRtpEncodingParametersDynArray;
    degradationPreference : RTCDegradationPreference;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCRtpReceiveParameters
    --------------------------------------------------------------------}
  
  TTJSRTCRtpDecodingParametersDynArray = Array of TJSRTCRtpDecodingParameters;
  TJSRTCRtpReceiveParameters = class(TJSObject)
    encodings : TTJSRTCRtpDecodingParametersDynArray;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCRtpCodingParameters
    --------------------------------------------------------------------}
  
  TJSRTCRtpCodingParameters = class(TJSObject)
    rid : String;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCRtpDecodingParameters
    --------------------------------------------------------------------}
  
  TJSRTCRtpDecodingParameters = class(TJSObject)
  end;
  
  { --------------------------------------------------------------------
    TJSRTCRtpEncodingParameters
    --------------------------------------------------------------------}
  
  TJSRTCRtpEncodingParameters = class(TJSObject)
    active : boolean;
    maxBitrate : NativeInt;
    scaleResolutionDownBy : Double;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCRtcpParameters
    --------------------------------------------------------------------}
  
  TJSRTCRtcpParameters = class(TJSObject)
    cname : String;
    reducedSize : boolean;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCRtpHeaderExtensionParameters
    --------------------------------------------------------------------}
  
  TJSRTCRtpHeaderExtensionParameters = class(TJSObject)
    uri : String;
    id : Cardinal;
    encrypted : boolean;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCRtpCodecParameters
    --------------------------------------------------------------------}
  
  TJSRTCRtpCodecParameters = class(TJSObject)
    payloadType : Byte;
    mimeType : String;
    clockRate : NativeInt;
    channels : Cardinal;
    sdpFmtpLine : String;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCRtpCapabilities
    --------------------------------------------------------------------}
  
  TTJSRTCRtpCodecCapabilityDynArray = Array of TJSRTCRtpCodecCapability;
  TTJSRTCRtpHeaderExtensionCapabilityDynArray = Array of TJSRTCRtpHeaderExtensionCapability;
  TJSRTCRtpCapabilities = class(TJSObject)
    codecs : TTJSRTCRtpCodecCapabilityDynArray;
    headerExtensions : TTJSRTCRtpHeaderExtensionCapabilityDynArray;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCRtpCodecCapability
    --------------------------------------------------------------------}
  
  TJSRTCRtpCodecCapability = class(TJSObject)
    mimeType : String;
    clockRate : NativeInt;
    channels : Cardinal;
    sdpFmtpLine : String;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCRtpHeaderExtensionCapability
    --------------------------------------------------------------------}
  
  TJSRTCRtpHeaderExtensionCapability = class(TJSObject)
    uri : String;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCRtpContributingSource
    --------------------------------------------------------------------}
  
  TJSRTCRtpContributingSource = class(TJSObject)
    timestamp : double;
    source : NativeInt;
    audioLevel : Double;
    rtpTimestamp : NativeInt;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCRtpSynchronizationSource
    --------------------------------------------------------------------}
  
  TJSRTCRtpSynchronizationSource = class(TJSObject)
    voiceActivityFlag : boolean;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCDtlsFingerprint
    --------------------------------------------------------------------}
  
  TJSRTCDtlsFingerprint = class(TJSObject)
    algorithm : String;
    value : String;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCIceParameters
    --------------------------------------------------------------------}
  
  TJSRTCIceParameters = class(TJSObject)
    usernameFragment : String;
    password : String;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCIceCandidatePair
    --------------------------------------------------------------------}
  
  TJSRTCIceCandidatePair = class(TJSObject)
    local : TJSRTCIceCandidate;
    remote : TJSRTCIceCandidate;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCTrackEventInit
    --------------------------------------------------------------------}
  
  TJSRTCTrackEventInit = class(TJSObject)
    receiver : TJSRTCRtpReceiver;
    track : JSValue;
    streams : TTJSHTMLMediaStreamDynArray;
    transceiver : TJSRTCRtpTransceiver;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCDataChannelInit
    --------------------------------------------------------------------}
  
  TJSRTCDataChannelInit = class(TJSObject)
    ordered : boolean;
    maxPacketLifeTime : Cardinal;
    maxRetransmits : Cardinal;
    protocol : String;
    negotiated : boolean;
    id : Cardinal;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCDataChannelEventInit
    --------------------------------------------------------------------}
  
  TJSRTCDataChannelEventInit = class(TJSObject)
    channel : TJSRTCDataChannel;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCDTMFToneChangeEventInit
    --------------------------------------------------------------------}
  
  TJSRTCDTMFToneChangeEventInit = class(TJSObject)
    tone : String;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCStats
    --------------------------------------------------------------------}
  
  TJSRTCStats = class(TJSObject)
    timestamp : double;
    type_ : String;external name 'type';
    id : String;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCErrorInit
    --------------------------------------------------------------------}
  
  TJSRTCErrorInit = class(TJSObject)
    errorDetail : RTCErrorDetailType;
    sdpLineNumber : Integer;
    httpRequestStatusCode : Integer;
    sctpCauseCode : Integer;
    receivedAlert : NativeInt;
    sentAlert : NativeInt;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCErrorEventInit
    --------------------------------------------------------------------}
  
  TJSRTCErrorEventInit = class(TJSObject)
    error : TJSRTCError;
  end;
  
  { --------------------------------------------------------------------
    TJSRTCPeerConnection
    --------------------------------------------------------------------}
  
  TTJSRTCRtpSenderDynArray = Array of TJSRTCRtpSender;
  TTJSRTCRtpReceiverDynArray = Array of TJSRTCRtpReceiver;
  TTJSRTCRtpTransceiverDynArray = Array of TJSRTCRtpTransceiver;
  
  TJSRTCPeerConnection = class external name 'RTCPeerConnection'  (TJSEventTarget)
  Private
    FlocalDescription : TJSRTCSessionDescription; external name 'localDescription'; 
    FcurrentLocalDescription : TJSRTCSessionDescription; external name 'currentLocalDescription'; 
    FpendingLocalDescription : TJSRTCSessionDescription; external name 'pendingLocalDescription'; 
    FremoteDescription : TJSRTCSessionDescription; external name 'remoteDescription'; 
    FcurrentRemoteDescription : TJSRTCSessionDescription; external name 'currentRemoteDescription'; 
    FpendingRemoteDescription : TJSRTCSessionDescription; external name 'pendingRemoteDescription'; 
    FsignalingState : RTCSignalingState; external name 'signalingState'; 
    FiceGatheringState : RTCIceGatheringState; external name 'iceGatheringState'; 
    FiceConnectionState : RTCIceConnectionState; external name 'iceConnectionState'; 
    FconnectionState : RTCPeerConnectionState; external name 'connectionState'; 
    FcanTrickleIceCandidates : boolean; external name 'canTrickleIceCandidates'; 
    Fsctp : TJSRTCSctpTransport; external name 'sctp'; 
  Public
    
      onnegotiationneeded : TJSEventHandler;
      onicecandidate : TJSEventHandler;
      onicecandidateerror : TJSEventHandler;
      onsignalingstatechange : TJSEventHandler;
      oniceconnectionstatechange : TJSEventHandler;
      onicegatheringstatechange : TJSEventHandler;
      onconnectionstatechange : TJSEventHandler;
      ontrack : TJSEventHandler;
      ondatachannel : TJSEventHandler;
    constructor New(configuration : TJSRTCConfiguration); overload;
    constructor New; overload;
    function createOffer(options : TJSRTCOfferOptions): TJSPromise; overload;
    function createOffer: TJSPromise; overload;
    function createAnswer(options : TJSRTCAnswerOptions): TJSPromise; overload;
    function createAnswer: TJSPromise; overload;
    function setLocalDescription(description : TJSRTCSessionDescriptionInit): TJSPromise; overload;
    function setLocalDescription: TJSPromise; overload;
    function setRemoteDescription(description : TJSRTCSessionDescriptionInit): TJSPromise; overload;
    function setRemoteDescription: TJSPromise; overload;
    function addIceCandidate(candidate : TJSRTCIceCandidateInit): TJSPromise; overload;
    function addIceCandidate: TJSPromise; overload;
    Procedure restartIce;
    function getConfiguration: TJSRTCConfiguration;
    Procedure setConfiguration(configuration : TJSRTCConfiguration);
    Procedure close;
    function createOffer(successCallback : RTCSessionDescriptionCallback; failureCallback : RTCPeerConnectionErrorCallback; options : TJSRTCOfferOptions): TJSPromise; overload;
    function createOffer(successCallback : RTCSessionDescriptionCallback; failureCallback : RTCPeerConnectionErrorCallback): TJSPromise; overload;
    function createAnswer(successCallback : RTCSessionDescriptionCallback; failureCallback : RTCPeerConnectionErrorCallback): TJSPromise;
    function addIceCandidate(candidate : TJSRTCIceCandidateInit; successCallback : TProcedure; failureCallback : RTCPeerConnectionErrorCallback): TJSPromise;
    function generateCertificate(keygenAlgorithm : AlgorithmIdentifier): TJSPromise;
    function getSenders: TTJSRTCRtpSenderDynArray;
    function getReceivers: TTJSRTCRtpReceiverDynArray;
    function getTransceivers: TTJSRTCRtpTransceiverDynArray;
    function addTrack(track : JSValue; streams : TJSHTMLMediaStream): TJSRTCRtpSender; varargs;
    Procedure removeTrack(sender : TJSRTCRtpSender);
    function addTransceiver(trackOrKind : JSValue; init : TJSRTCRtpTransceiverInit): TJSRTCRtpTransceiver; overload;
    function addTransceiver(trackOrKind : JSValue): TJSRTCRtpTransceiver; overload;
    function createDataChannel(label_ : String; dataChannelDict : TJSRTCDataChannelInit): TJSRTCDataChannel; overload;
    function createDataChannel(label_ : String): TJSRTCDataChannel; overload;
    function getStats(selector : JSValue): TJSPromise; overload;
    function getStats: TJSPromise; overload;
    Property localDescription : TJSRTCSessionDescription Read FlocalDescription; 
    Property currentLocalDescription : TJSRTCSessionDescription Read FcurrentLocalDescription; 
    Property pendingLocalDescription : TJSRTCSessionDescription Read FpendingLocalDescription; 
    Property remoteDescription : TJSRTCSessionDescription Read FremoteDescription; 
    Property currentRemoteDescription : TJSRTCSessionDescription Read FcurrentRemoteDescription; 
    Property pendingRemoteDescription : TJSRTCSessionDescription Read FpendingRemoteDescription; 
    Property signalingState : RTCSignalingState Read FsignalingState; 
    Property iceGatheringState : RTCIceGatheringState Read FiceGatheringState; 
    Property iceConnectionState : RTCIceConnectionState Read FiceConnectionState; 
    Property connectionState : RTCPeerConnectionState Read FconnectionState; 
    Property canTrickleIceCandidates : boolean Read FcanTrickleIceCandidates; 
    Property sctp : TJSRTCSctpTransport Read Fsctp; 
  end;
  
  { --------------------------------------------------------------------
    TJSRTCSessionDescription
    --------------------------------------------------------------------}
  
  TJSRTCSessionDescription = class external name 'RTCSessionDescription' 
  Private
    Ftype_ : RTCSdpType; external name 'type'; 
    Fsdp : String; external name 'sdp'; 
  Public
    constructor New(descriptionInitDict : TJSRTCSessionDescriptionInit); overload;
    constructor New; overload;
    function toJSON: TJSObject;
    Property type_ : RTCSdpType Read Ftype_; 
    Property sdp : String Read Fsdp; 
  end;
  
  { --------------------------------------------------------------------
    TJSRTCIceCandidate
    --------------------------------------------------------------------}
  
  TJSRTCIceCandidate = class external name 'RTCIceCandidate' 
  Private
    Fcandidate : String; external name 'candidate'; 
    FsdpMid : String; external name 'sdpMid'; 
    FsdpMLineIndex : Cardinal; external name 'sdpMLineIndex'; 
    Ffoundation : String; external name 'foundation'; 
    Fcomponent : RTCIceComponent; external name 'component'; 
    Fpriority : NativeInt; external name 'priority'; 
    Faddress : String; external name 'address'; 
    Fprotocol : RTCIceProtocol; external name 'protocol'; 
    Fport : Cardinal; external name 'port'; 
    Ftype_ : RTCIceCandidateType; external name 'type'; 
    FtcpType : RTCIceTcpCandidateType; external name 'tcpType'; 
    FrelatedAddress : String; external name 'relatedAddress'; 
    FrelatedPort : Cardinal; external name 'relatedPort'; 
    FusernameFragment : String; external name 'usernameFragment'; 
  Public
    constructor New(candidateInitDict : TJSRTCIceCandidateInit); overload;
    constructor New; overload;
    function toJSON: TJSRTCIceCandidateInit;
    Property candidate : String Read Fcandidate; 
    Property sdpMid : String Read FsdpMid; 
    Property sdpMLineIndex : Cardinal Read FsdpMLineIndex; 
    Property foundation : String Read Ffoundation; 
    Property component : RTCIceComponent Read Fcomponent; 
    Property priority : NativeInt Read Fpriority; 
    Property address : String Read Faddress; 
    Property protocol : RTCIceProtocol Read Fprotocol; 
    Property port : Cardinal Read Fport; 
    Property type_ : RTCIceCandidateType Read Ftype_; 
    Property tcpType : RTCIceTcpCandidateType Read FtcpType; 
    Property relatedAddress : String Read FrelatedAddress; 
    Property relatedPort : Cardinal Read FrelatedPort; 
    Property usernameFragment : String Read FusernameFragment; 
  end;
  
  { --------------------------------------------------------------------
    TJSRTCPeerConnectionIceEvent
    --------------------------------------------------------------------}
  
  TJSRTCPeerConnectionIceEvent = class external name 'RTCPeerConnectionIceEvent'  (TJSEvent)
  Private
    Fcandidate : TJSRTCIceCandidate; external name 'candidate'; 
    Furl : String; external name 'url'; 
  Public
    constructor New(type_ : String; eventInitDict : TJSRTCPeerConnectionIceEventInit); overload;
    constructor New(type_ : String); overload;
    Property candidate : TJSRTCIceCandidate Read Fcandidate; 
    Property url : String Read Furl; 
  end;
  
  { --------------------------------------------------------------------
    TJSRTCPeerConnectionIceErrorEvent
    --------------------------------------------------------------------}
  
  TJSRTCPeerConnectionIceErrorEvent = class external name 'RTCPeerConnectionIceErrorEvent'  (TJSEvent)
  Private
    Faddress : String; external name 'address'; 
    Fport : Cardinal; external name 'port'; 
    Furl : String; external name 'url'; 
    FerrorCode : Cardinal; external name 'errorCode'; 
    FerrorText : String; external name 'errorText'; 
  Public
    constructor New(type_ : String; eventInitDict : TJSRTCPeerConnectionIceErrorEventInit);
    Property address : String Read Faddress; 
    Property port : Cardinal Read Fport; 
    Property url : String Read Furl; 
    Property errorCode : Cardinal Read FerrorCode; 
    Property errorText : String Read FerrorText; 
  end;
  
  { --------------------------------------------------------------------
    TJSRTCCertificate
    --------------------------------------------------------------------}
  
  TTJSRTCDtlsFingerprintDynArray = Array of TJSRTCDtlsFingerprint;
  
  TJSRTCCertificate = class external name 'RTCCertificate' 
  Private
    Fexpires : String; external name 'expires'; 
  Public
    function getFingerprints: TTJSRTCDtlsFingerprintDynArray;
    Property expires : String Read Fexpires; 
  end;
  
  { --------------------------------------------------------------------
    TJSRTCRtpSender
    --------------------------------------------------------------------}
  
  TJSRTCRtpSender = class external name 'RTCRtpSender' 
  Private
    Ftrack : JSValue; external name 'track'; 
    Ftransport : TJSRTCDtlsTransport; external name 'transport'; 
    Fdtmf : TJSRTCDTMFSender; external name 'dtmf'; 
  Public
    function getCapabilities(kind : String): TJSRTCRtpCapabilities;
    function setParameters(parameters : TJSRTCRtpSendParameters): TJSPromise;
    function getParameters: TJSRTCRtpSendParameters;
    function replaceTrack(withTrack : JSValue): TJSPromise;
    Procedure setStreams(streams : TJSHTMLMediaStream); varargs;
    function getStats: TJSPromise;
    Property track : JSValue Read Ftrack; 
    Property transport : TJSRTCDtlsTransport Read Ftransport; 
    Property dtmf : TJSRTCDTMFSender Read Fdtmf; 
  end;
  
  { --------------------------------------------------------------------
    TJSRTCRtpReceiver
    --------------------------------------------------------------------}
  
  TTJSRTCRtpContributingSourceDynArray = Array of TJSRTCRtpContributingSource;
  TTJSRTCRtpSynchronizationSourceDynArray = Array of TJSRTCRtpSynchronizationSource;
  
  TJSRTCRtpReceiver = class external name 'RTCRtpReceiver' 
  Private
    Ftrack : JSValue; external name 'track'; 
    Ftransport : TJSRTCDtlsTransport; external name 'transport'; 
  Public
    function getCapabilities(kind : String): TJSRTCRtpCapabilities;
    function getParameters: TJSRTCRtpReceiveParameters;
    function getContributingSources: TTJSRTCRtpContributingSourceDynArray;
    function getSynchronizationSources: TTJSRTCRtpSynchronizationSourceDynArray;
    function getStats: TJSPromise;
    Property track : JSValue Read Ftrack; 
    Property transport : TJSRTCDtlsTransport Read Ftransport; 
  end;
  
  { --------------------------------------------------------------------
    TJSRTCRtpTransceiver
    --------------------------------------------------------------------}
  
  TJSRTCRtpTransceiver = class external name 'RTCRtpTransceiver' 
  Private
    Fmid : String; external name 'mid'; 
    Fsender : TJSRTCRtpSender; external name 'sender'; 
    Freceiver : TJSRTCRtpReceiver; external name 'receiver'; 
    FcurrentDirection : RTCRtpTransceiverDirection; external name 'currentDirection'; 
  Public
      direction : RTCRtpTransceiverDirection;
    Procedure stop;
    Procedure setCodecPreferences(codecs : TTJSRTCRtpCodecCapabilityDynArray);
    Property mid : String Read Fmid; 
    Property sender : TJSRTCRtpSender Read Fsender; 
    Property receiver : TJSRTCRtpReceiver Read Freceiver; 
    Property currentDirection : RTCRtpTransceiverDirection Read FcurrentDirection; 
  end;
  
  { --------------------------------------------------------------------
    TJSRTCDtlsTransport
    --------------------------------------------------------------------}
  
  TTJSArrayBufferDynArray = Array of TJSArrayBuffer;
  
  TJSRTCDtlsTransport = class external name 'RTCDtlsTransport'  (TJSEventTarget)
  Private
    FiceTransport : TJSRTCIceTransport; external name 'iceTransport'; 
    Fstate : RTCDtlsTransportState; external name 'state'; 
  Public
      onstatechange : TJSEventHandler;
      onerror : TJSEventHandler;
    function getRemoteCertificates: TTJSArrayBufferDynArray;
    Property iceTransport : TJSRTCIceTransport Read FiceTransport; 
    Property state : RTCDtlsTransportState Read Fstate; 
  end;
  
  { --------------------------------------------------------------------
    TJSRTCIceTransport
    --------------------------------------------------------------------}
  
  TTJSRTCIceCandidateDynArray = Array of TJSRTCIceCandidate;
  
  TJSRTCIceTransport = class external name 'RTCIceTransport'  (TJSEventTarget)
  Private
    Frole : RTCIceRole; external name 'role'; 
    Fcomponent : RTCIceComponent; external name 'component'; 
    Fstate : RTCIceTransportState; external name 'state'; 
    FgatheringState : RTCIceGathererState; external name 'gatheringState'; 
  Public
      onstatechange : TJSEventHandler;
      ongatheringstatechange : TJSEventHandler;
      onselectedcandidatepairchange : TJSEventHandler;
    function getLocalCandidates: TTJSRTCIceCandidateDynArray;
    function getRemoteCandidates: TTJSRTCIceCandidateDynArray;
    function getSelectedCandidatePair: TJSRTCIceCandidatePair;
    function getLocalParameters: TJSRTCIceParameters;
    function getRemoteParameters: TJSRTCIceParameters;
    Property role : RTCIceRole Read Frole; 
    Property component : RTCIceComponent Read Fcomponent; 
    Property state : RTCIceTransportState Read Fstate; 
    Property gatheringState : RTCIceGathererState Read FgatheringState; 
  end;
  
  { --------------------------------------------------------------------
    TJSRTCTrackEvent
    --------------------------------------------------------------------}
  
  TJSRTCTrackEvent = class external name 'RTCTrackEvent'  (TJSEvent)
  Private
    Freceiver : TJSRTCRtpReceiver; external name 'receiver'; 
    Ftrack : JSValue; external name 'track'; 
    Fstreams : TTJSHTMLMediaStreamDynArray; external name 'streams'; 
    Ftransceiver : TJSRTCRtpTransceiver; external name 'transceiver'; 
  Public
    constructor New(type_ : String; eventInitDict : TJSRTCTrackEventInit);
    Property receiver : TJSRTCRtpReceiver Read Freceiver; 
    Property track : JSValue Read Ftrack; 
    Property streams : TTJSHTMLMediaStreamDynArray Read Fstreams; 
    Property transceiver : TJSRTCRtpTransceiver Read Ftransceiver; 
  end;
  
  { --------------------------------------------------------------------
    TJSRTCSctpTransport
    --------------------------------------------------------------------}
  
  TJSRTCSctpTransport = class external name 'RTCSctpTransport'  (TJSEventTarget)
  Private
    Ftransport : TJSRTCDtlsTransport; external name 'transport'; 
    Fstate : RTCSctpTransportState; external name 'state'; 
    FmaxMessageSize : Double; external name 'maxMessageSize'; 
    FmaxChannels : Cardinal; external name 'maxChannels'; 
  Public
      onstatechange : TJSEventHandler;
    Property transport : TJSRTCDtlsTransport Read Ftransport; 
    Property state : RTCSctpTransportState Read Fstate; 
    Property maxMessageSize : Double Read FmaxMessageSize; 
    Property maxChannels : Cardinal Read FmaxChannels; 
  end;
  
  { --------------------------------------------------------------------
    TJSRTCDataChannel
    --------------------------------------------------------------------}
  
  TJSRTCDataChannel = class external name 'RTCDataChannel'  (TJSEventTarget)
  Private
    Flabel_ : String; external name 'label'; 
    Fordered : boolean; external name 'ordered'; 
    FmaxPacketLifeTime : Cardinal; external name 'maxPacketLifeTime'; 
    FmaxRetransmits : Cardinal; external name 'maxRetransmits'; 
    Fprotocol : String; external name 'protocol'; 
    Fnegotiated : boolean; external name 'negotiated'; 
    Fid : Cardinal; external name 'id'; 
    FreadyState : RTCDataChannelState; external name 'readyState'; 
    FbufferedAmount : NativeInt; external name 'bufferedAmount'; 
  Public
      bufferedAmountLowThreshold : NativeInt;
      onopen : TJSEventHandler;
      onbufferedamountlow : TJSEventHandler;
      onerror : TJSEventHandler;
      onclosing : TJSEventHandler;
      onclose : TJSEventHandler;
      onmessage : TJSEventHandler;
      binaryType : String;
    Procedure close;
    Procedure send(data : String);
    Procedure send(data : TJSBlob);
    Procedure send(data : TJSArrayBuffer);
    Procedure send(data : TJSTypedArray);
    Property label_ : String Read Flabel_; 
    Property ordered : boolean Read Fordered; 
    Property maxPacketLifeTime : Cardinal Read FmaxPacketLifeTime; 
    Property maxRetransmits : Cardinal Read FmaxRetransmits; 
    Property protocol : String Read Fprotocol; 
    Property negotiated : boolean Read Fnegotiated; 
    Property id : Cardinal Read Fid; 
    Property readyState : RTCDataChannelState Read FreadyState; 
    Property bufferedAmount : NativeInt Read FbufferedAmount; 
  end;
  
  { --------------------------------------------------------------------
    TJSRTCDataChannelEvent
    --------------------------------------------------------------------}
  
  TJSRTCDataChannelEvent = class external name 'RTCDataChannelEvent'  (TJSEvent)
  Private
    Fchannel : TJSRTCDataChannel; external name 'channel'; 
  Public
    constructor New(type_ : String; eventInitDict : TJSRTCDataChannelEventInit);
    Property channel : TJSRTCDataChannel Read Fchannel; 
  end;
  
  { --------------------------------------------------------------------
    TJSRTCDTMFSender
    --------------------------------------------------------------------}
  
  TJSRTCDTMFSender = class external name 'RTCDTMFSender'  (TJSEventTarget)
  Private
    FcanInsertDTMF : boolean; external name 'canInsertDTMF'; 
    FtoneBuffer : String; external name 'toneBuffer'; 
  Public
      ontonechange : TJSEventHandler;
    Procedure insertDTMF(tones : String; duration : NativeInt; interToneGap : NativeInt); overload;
    Procedure insertDTMF(tones : String); overload;
    Procedure insertDTMF(tones : String; duration : NativeInt); overload;
    Property canInsertDTMF : boolean Read FcanInsertDTMF; 
    Property toneBuffer : String Read FtoneBuffer; 
  end;
  
  { --------------------------------------------------------------------
    TJSRTCDTMFToneChangeEvent
    --------------------------------------------------------------------}
  
  TJSRTCDTMFToneChangeEvent = class external name 'RTCDTMFToneChangeEvent'  (TJSEvent)
  Private
    Ftone : String; external name 'tone'; 
  Public
    constructor New(type_ : String; eventInitDict : TJSRTCDTMFToneChangeEventInit);
    Property tone : String Read Ftone; 
  end;
  
  { --------------------------------------------------------------------
    TJSRTCStatsReport
    --------------------------------------------------------------------}
  
  TJSRTCStatsReport = class external name 'RTCStatsReport' 
  Private
  Public
  end;
  
  { --------------------------------------------------------------------
    TJSRTCError
    --------------------------------------------------------------------}
  
  TJSRTCError = class external name 'RTCError'  (TJSError)
  Private
    FerrorDetail : RTCErrorDetailType; external name 'errorDetail'; 
    FsdpLineNumber : Integer; external name 'sdpLineNumber'; 
    FhttpRequestStatusCode : Integer; external name 'httpRequestStatusCode'; 
    FsctpCauseCode : Integer; external name 'sctpCauseCode'; 
    FreceivedAlert : NativeInt; external name 'receivedAlert'; 
    FsentAlert : NativeInt; external name 'sentAlert'; 
  Public
    constructor New(init : TJSRTCErrorInit; message : String); overload;
    constructor New(init : TJSRTCErrorInit); overload;
    Property errorDetail : RTCErrorDetailType Read FerrorDetail; 
    Property sdpLineNumber : Integer Read FsdpLineNumber; 
    Property httpRequestStatusCode : Integer Read FhttpRequestStatusCode; 
    Property sctpCauseCode : Integer Read FsctpCauseCode; 
    Property receivedAlert : NativeInt Read FreceivedAlert; 
    Property sentAlert : NativeInt Read FsentAlert; 
  end;
  
  { --------------------------------------------------------------------
    TJSRTCErrorEvent
    --------------------------------------------------------------------}
  
  TJSRTCErrorEvent = class external name 'RTCErrorEvent'  (TJSEvent)
  Private
    Ferror : TJSRTCError; external name 'error'; 
  Public
    constructor New(type_ : String; eventInitDict : TJSRTCErrorEventInit);
    Property error : TJSRTCError Read Ferror; 
  end;

implementation


end.
