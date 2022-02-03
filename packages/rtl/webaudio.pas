Unit webaudio;

{$MODE ObjFPC}
{$H+}
{$modeswitch externalclass}

interface

uses SysUtils, JS,web,types;

{
  Automatically generated file by TWebIDLToPas on 2018-06-23 10:52:00
  
  Used command-line options : 
  -i
  webaudio.idl
  -v
  v2
  -t
  EventTarget=TJSEventTarget,DOMHighResTimeStamp=TJSDOMHighResTimeStamp,HTMLMediaElement=TJSElement,MediaStream=JSValue,MediaStreamTrack=JSValue,EventHandler=TJSEVentHandler,Promise=TJSPromise,Event=TJSEvent,Worklet=TJSOBject,WorkletGlobalScope=TJSObject,VoidFunction=TProcedure,MessagePort=TJSMessagePort
  -x
  web,types
  -d
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
  EventTarget=TJSEventTarget
  DOMHighResTimeStamp=TJSDOMHighResTimeStamp
  HTMLMediaElement=TJSElement
  MediaStream=JSValue
  MediaStreamTrack=JSValue
  EventHandler=TJSEVentHandler
  Promise=TJSPromise
  Event=TJSEvent
  Worklet=TJSOBject
  WorkletGlobalScope=TJSObject
  VoidFunction=TProcedure
  MessagePort=TJSMessagePort
}
Type
  // Forward class definitions
  TJSBaseAudioContext = Class;
  TJSAudioContext = Class;
  TJSOfflineAudioContext = Class;
  TJSOfflineAudioCompletionEvent = Class;
  TJSAudioBuffer = Class;
  TJSAudioNode = Class;
  TJSAudioParam = Class;
  TJSAudioScheduledSourceNode = Class;
  TJSAnalyserNode = Class;
  TJSAudioBufferSourceNode = Class;
  TJSAudioDestinationNode = Class;
  TJSAudioListener = Class;
  TJSAudioProcessingEvent = Class;
  TJSBiquadFilterNode = Class;
  TJSChannelMergerNode = Class;
  TJSChannelSplitterNode = Class;
  TJSConstantSourceNode = Class;
  TJSConvolverNode = Class;
  TJSDelayNode = Class;
  TJSDynamicsCompressorNode = Class;
  TJSGainNode = Class;
  TJSIIRFilterNode = Class;
  TJSMediaElementAudioSourceNode = Class;
  TJSMediaStreamAudioDestinationNode = Class;
  TJSMediaStreamAudioSourceNode = Class;
  TJSMediaStreamTrackAudioSourceNode = Class;
  TJSOscillatorNode = Class;
  TJSPannerNode = Class;
  TJSPeriodicWave = Class;
  TJSScriptProcessorNode = Class;
  TJSStereoPannerNode = Class;
  TJSWaveShaperNode = Class;
  TJSAudioWorklet = Class;
  TJSAudioWorkletGlobalScope = Class;
  TJSAudioParamMap = Class;
  TJSAudioWorkletNode = Class;
  TJSAudioWorkletProcessor = Class;
  TJSAudioContextOptions = Class;
  TJSAudioTimestamp = Class;
  TJSOfflineAudioContextOptions = Class;
  TJSOfflineAudioCompletionEventInit = Class;
  TJSAudioBufferOptions = Class;
  TJSAudioNodeOptions = Class;
  TJSAnalyserOptions = Class;
  TJSAudioBufferSourceOptions = Class;
  TJSAudioProcessingEventInit = Class;
  TJSBiquadFilterOptions = Class;
  TJSChannelMergerOptions = Class;
  TJSChannelSplitterOptions = Class;
  TJSConstantSourceOptions = Class;
  TJSConvolverOptions = Class;
  TJSDelayOptions = Class;
  TJSDynamicsCompressorOptions = Class;
  TJSGainOptions = Class;
  TJSIIRFilterOptions = Class;
  TJSMediaElementAudioSourceOptions = Class;
  TJSMediaStreamAudioSourceOptions = Class;
  TJSMediaStreamTrackAudioSourceOptions = Class;
  TJSOscillatorOptions = Class;
  TJSPannerOptions = Class;
  TJSPeriodicWaveConstraints = Class;
  TJSPeriodicWaveOptions = Class;
  TJSStereoPannerOptions = Class;
  TJSWaveShaperOptions = Class;
  TJSAudioWorkletNodeOptions = Class;
  TJSAudioParamDescriptor = Class;
  AudioContextState = String;
  AudioContextLatencyCategory = String;
  ChannelCountMode = String;
  ChannelInterpretation = String;
  AutomationRate = String;
  BiquadFilterType = String;
  OscillatorType = String;
  PanningModelType = String;
  DistanceModelType = String;
  OverSampleType = String;
  DecodeErrorCallback = Procedure (error : TJSError);
  DecodeSuccessCallback = Procedure (decodedData : TJSAudioBuffer);
  
  { --------------------------------------------------------------------
    TJSAudioContextOptions
    --------------------------------------------------------------------}
  
  TJSAudioContextOptions = class external name 'Object' (TJSObject)
    latencyHint : JSValue;
    sampleRate : Double;
  end;
  
  { --------------------------------------------------------------------
    TJSAudioTimestamp
    --------------------------------------------------------------------}
  
  TJSAudioTimestamp = class external name 'Object' (TJSObject)
    contextTime : Double;
    performanceTime : TJSDOMHighResTimeStamp;
  end;
  
  { --------------------------------------------------------------------
    TJSOfflineAudioContextOptions
    --------------------------------------------------------------------}
  
  TJSOfflineAudioContextOptions = class external name 'Object' (TJSObject)
    numberOfChannels : NativeInt;
    length_ : NativeInt;external name 'length';
    sampleRate : Double;
  end;
  
  { --------------------------------------------------------------------
    TJSOfflineAudioCompletionEventInit
    --------------------------------------------------------------------}
  
  TJSOfflineAudioCompletionEventInit = class external name 'Object' (TJSObject)
    renderedBuffer : TJSAudioBuffer;
  end;
  
  { --------------------------------------------------------------------
    TJSAudioBufferOptions
    --------------------------------------------------------------------}
  
  TJSAudioBufferOptions = class external name 'Object' (TJSObject)
    numberOfChannels : NativeInt;
    length_ : NativeInt;external name 'length';
    sampleRate : Double;
  end;
  
  { --------------------------------------------------------------------
    TJSAudioNodeOptions
    --------------------------------------------------------------------}
  
  TJSAudioNodeOptions = class external name 'Object' (TJSObject)
    channelCount : NativeInt;
    _channelCountMode : ChannelCountMode;external name 'channelCountMode';
    _channelInterpretation : ChannelInterpretation;external name 'channelInterpretation';
  end;
  
  { --------------------------------------------------------------------
    TJSAnalyserOptions
    --------------------------------------------------------------------}
  
  TJSAnalyserOptions = class external name 'Object' (TJSObject)
    fftSize : NativeInt;
    maxDecibels : Double;
    minDecibels : Double;
    smoothingTimeConstant : Double;
  end;
  
  { --------------------------------------------------------------------
    TJSAudioBufferSourceOptions
    --------------------------------------------------------------------}
  
  TJSAudioBufferSourceOptions = class external name 'Object' (TJSObject)
    buffer : TJSAudioBuffer;
    detune : Double;
    loop : boolean;
    loopEnd : Double;
    loopStart : Double;
    playbackRate : Double;
  end;
  
  { --------------------------------------------------------------------
    TJSAudioProcessingEventInit
    --------------------------------------------------------------------}
  
  TJSAudioProcessingEventInit = class external name 'Object' (TJSObject)
    playbackTime : Double;
    inputBuffer : TJSAudioBuffer;
    outputBuffer : TJSAudioBuffer;
  end;
  
  { --------------------------------------------------------------------
    TJSBiquadFilterOptions
    --------------------------------------------------------------------}
  
  TJSBiquadFilterOptions = class external name 'Object' (TJSObject)
    type_ : BiquadFilterType;external name 'type';
    Q : Double;
    detune : Double;
    frequency : Double;
    gain : Double;
  end;
  
  { --------------------------------------------------------------------
    TJSChannelMergerOptions
    --------------------------------------------------------------------}
  
  TJSChannelMergerOptions = class external name 'Object' (TJSObject)
    numberOfInputs : NativeInt;
  end;
  
  { --------------------------------------------------------------------
    TJSChannelSplitterOptions
    --------------------------------------------------------------------}
  
  TJSChannelSplitterOptions = class external name 'Object' (TJSObject)
    numberOfOutputs : NativeInt;
  end;
  
  { --------------------------------------------------------------------
    TJSConstantSourceOptions
    --------------------------------------------------------------------}
  
  TJSConstantSourceOptions = class external name 'Object' (TJSObject)
    offset : Double;
  end;
  
  { --------------------------------------------------------------------
    TJSConvolverOptions
    --------------------------------------------------------------------}
  
  TJSConvolverOptions = class external name 'Object' (TJSObject)
    buffer : TJSAudioBuffer;
    disableNormalization : boolean;
  end;
  
  { --------------------------------------------------------------------
    TJSDelayOptions
    --------------------------------------------------------------------}
  
  TJSDelayOptions = class external name 'Object' (TJSObject)
    maxDelayTime : Double;
    delayTime : Double;
  end;
  
  { --------------------------------------------------------------------
    TJSDynamicsCompressorOptions
    --------------------------------------------------------------------}
  
  TJSDynamicsCompressorOptions = class external name 'Object' (TJSObject)
    attack : Double;
    knee : Double;
    ratio : Double;
    release : Double;
    threshold : Double;
  end;
  
  { --------------------------------------------------------------------
    TJSGainOptions
    --------------------------------------------------------------------}
  
  TJSGainOptions = class external name 'Object' (TJSObject)
    gain : Double;
  end;
  
  { --------------------------------------------------------------------
    TJSIIRFilterOptions
    --------------------------------------------------------------------}
  
  TJSIIRFilterOptions = class external name 'Object' (TJSObject)
    feedforward : TDoubleDynArray;
    feedback : TDoubleDynArray;
  end;
  
  { --------------------------------------------------------------------
    TJSMediaElementAudioSourceOptions
    --------------------------------------------------------------------}
  
  TJSMediaElementAudioSourceOptions = class external name 'Object' (TJSObject)
    mediaElement : TJSElement;
  end;
  
  { --------------------------------------------------------------------
    TJSMediaStreamAudioSourceOptions
    --------------------------------------------------------------------}
  
  TJSMediaStreamAudioSourceOptions = class external name 'Object' (TJSObject)
    mediaStream : JSValue;
  end;
  
  { --------------------------------------------------------------------
    TJSMediaStreamTrackAudioSourceOptions
    --------------------------------------------------------------------}
  
  TJSMediaStreamTrackAudioSourceOptions = class external name 'Object' (TJSObject)
    mediaStreamTrack : JSValue;
  end;
  
  { --------------------------------------------------------------------
    TJSOscillatorOptions
    --------------------------------------------------------------------}
  
  TJSOscillatorOptions = class external name 'Object' (TJSObject)
    type_ : OscillatorType;external name 'type';
    frequency : Double;
    detune : Double;
    periodicWave : TJSPeriodicWave;
  end;
  
  { --------------------------------------------------------------------
    TJSPannerOptions
    --------------------------------------------------------------------}
  
  TJSPannerOptions = class external name 'Object' (TJSObject)
    panningModel : PanningModelType;
    distanceModel : DistanceModelType;
    positionX : Double;
    positionY : Double;
    positionZ : Double;
    orientationX : Double;
    orientationY : Double;
    orientationZ : Double;
    refDistance : Double;
    maxDistance : Double;
    rolloffFactor : Double;
    coneInnerAngle : Double;
    coneOuterAngle : Double;
    coneOuterGain : Double;
  end;
  
  { --------------------------------------------------------------------
    TJSPeriodicWaveConstraints
    --------------------------------------------------------------------}
  
  TJSPeriodicWaveConstraints = class external name 'Object' (TJSObject)
    disableNormalization : boolean;
  end;
  
  { --------------------------------------------------------------------
    TJSPeriodicWaveOptions
    --------------------------------------------------------------------}
  
  TJSPeriodicWaveOptions = class external name 'Object' (TJSObject)
    real : TDoubleDynArray;
    imag : TDoubleDynArray;
  end;
  
  { --------------------------------------------------------------------
    TJSStereoPannerOptions
    --------------------------------------------------------------------}
  
  TJSStereoPannerOptions = class external name 'Object' (TJSObject)
    pan : Double;
  end;
  
  { --------------------------------------------------------------------
    TJSWaveShaperOptions
    --------------------------------------------------------------------}
  
  TJSWaveShaperOptions = class external name 'Object' (TJSObject)
    curve : TDoubleDynArray;
    oversample : OverSampleType;
  end;
  
  { --------------------------------------------------------------------
    TJSAudioWorkletNodeOptions
    --------------------------------------------------------------------}
  
  TJSAudioWorkletNodeOptions = class external name 'Object' (TJSObject)
    numberOfInputs : NativeInt;
    numberOfOutputs : NativeInt;
    outputChannelCount : TNativeIntDynArray;
    parameterData : TJSObject;
    processorOptions : TJSObject;
  end;
  
  { --------------------------------------------------------------------
    TJSAudioParamDescriptor
    --------------------------------------------------------------------}
  
  TJSAudioParamDescriptor = class external name 'Object' (TJSObject)
    name : String;
    defaultValue : Double;
    minValue : Double;
    maxValue : Double;
    _automationRate : AutomationRate;external name 'automationRate';
  end;
  
  { --------------------------------------------------------------------
    TJSBaseAudioContext
    --------------------------------------------------------------------}
  
  TDoubleDynArray = Array of Double;
  
  TJSBaseAudioContext = class external name 'BaseAudioContext'  (TJSEventTarget)
  Private
    Fdestination : TJSAudioDestinationNode; external name 'destination'; 
    FsampleRate : Double; external name 'sampleRate'; 
    FcurrentTime : Double; external name 'currentTime'; 
    Flistener : TJSAudioListener; external name 'listener'; 
    Fstate : AudioContextState; external name 'state'; 
    FaudioWorklet : TJSAudioWorklet; external name 'audioWorklet'; 
  Public
    
      onstatechange : TJSEVentHandler;
    function createAnalyser: TJSAnalyserNode;
    function createBiquadFilter: TJSBiquadFilterNode;
    function createBuffer(numberOfChannels : NativeInt; length_ : NativeInt; sampleRate : Double): TJSAudioBuffer;
    function createBufferSource: TJSAudioBufferSourceNode;
    function createChannelMerger(numberOfInputs : NativeInt): TJSChannelMergerNode; overload;
    function createChannelMerger: TJSChannelMergerNode; overload;
    function createChannelSplitter(numberOfOutputs : NativeInt): TJSChannelSplitterNode; overload;
    function createChannelSplitter: TJSChannelSplitterNode; overload;
    function createConstantSource: TJSConstantSourceNode;
    function createConvolver: TJSConvolverNode;
    function createDelay(maxDelayTime : Double): TJSDelayNode; overload;
    function createDelay: TJSDelayNode; overload;
    function createDynamicsCompressor: TJSDynamicsCompressorNode;
    function createGain: TJSGainNode;
    function createIIRFilter(feedforward : TDoubleDynArray; feedback : TDoubleDynArray): TJSIIRFilterNode;
    function createOscillator: TJSOscillatorNode;
    function createPanner: TJSPannerNode;
    function createPeriodicWave(real : TDoubleDynArray; imag : TDoubleDynArray; constraints : TJSPeriodicWaveConstraints): TJSPeriodicWave; overload;
    function createPeriodicWave(real : TDoubleDynArray; imag : TDoubleDynArray): TJSPeriodicWave; overload;
    function createScriptProcessor(bufferSize : NativeInt; numberOfInputChannels : NativeInt; numberOfOutputChannels : NativeInt): TJSScriptProcessorNode; overload;
    function createScriptProcessor: TJSScriptProcessorNode; overload;
    function createScriptProcessor(bufferSize : NativeInt): TJSScriptProcessorNode; overload;
    function createScriptProcessor(bufferSize : NativeInt; numberOfInputChannels : NativeInt): TJSScriptProcessorNode; overload;
    function createStereoPanner: TJSStereoPannerNode;
    function createWaveShaper: TJSWaveShaperNode;
    function decodeAudioData(audioData : TJSArrayBuffer; successCallback : DecodeSuccessCallback; errorCallback : DecodeErrorCallback): TJSPromise; overload;
    function decodeAudioData(audioData : TJSArrayBuffer): TJSPromise; overload;
    function decodeAudioData(audioData : TJSArrayBuffer; successCallback : DecodeSuccessCallback): TJSPromise; overload;
    function resume: TJSPromise;
    Property destination : TJSAudioDestinationNode Read Fdestination; 
    Property sampleRate : Double Read FsampleRate; 
    Property currentTime : Double Read FcurrentTime; 
    Property listener : TJSAudioListener Read Flistener; 
    Property state : AudioContextState Read Fstate; 
    Property audioWorklet : TJSAudioWorklet Read FaudioWorklet; 
  end;
  
  { --------------------------------------------------------------------
    TJSAudioContext
    --------------------------------------------------------------------}
  
  TJSAudioContext = class external name 'AudioContext'  (TJSBaseAudioContext)
  Private
    FbaseLatency : Double; external name 'baseLatency'; 
    FoutputLatency : Double; external name 'outputLatency'; 
  Public
    function getOutputTimestamp: TJSAudioTimestamp;
    function suspend: TJSPromise;
    function close: TJSPromise;
    function createMediaElementSource(mediaElement : TJSElement): TJSMediaElementAudioSourceNode;
    function createMediaStreamSource(mediaStream : JSValue): TJSMediaStreamAudioSourceNode;
    function createMediaStreamTrackSource(mediaStreamTrack : JSValue): TJSMediaStreamTrackAudioSourceNode;
    function createMediaStreamDestination: TJSMediaStreamAudioDestinationNode;
    Property baseLatency : Double Read FbaseLatency; 
    Property outputLatency : Double Read FoutputLatency; 
  end;
  
  { --------------------------------------------------------------------
    TJSOfflineAudioContext
    --------------------------------------------------------------------}
  
  TJSOfflineAudioContext = class external name 'OfflineAudioContext'  (TJSBaseAudioContext)
  Private
    Flength_ : NativeInt; external name 'length'; 
  Public
      oncomplete : TJSEVentHandler;
    function startRendering: TJSPromise;
    function suspend(suspendTime : Double): TJSPromise;
    Property length_ : NativeInt Read Flength_; 
  end;
  
  { --------------------------------------------------------------------
    TJSOfflineAudioCompletionEvent
    --------------------------------------------------------------------}
  
  TJSOfflineAudioCompletionEvent = class external name 'OfflineAudioCompletionEvent'  (TJSEvent)
  Private
    FrenderedBuffer : TJSAudioBuffer; external name 'renderedBuffer'; 
  Public
    Property renderedBuffer : TJSAudioBuffer Read FrenderedBuffer; 
  end;
  
  { --------------------------------------------------------------------
    TJSAudioBuffer
    --------------------------------------------------------------------}
  
  TJSAudioBuffer = class external name 'AudioBuffer' 
  Private
    FsampleRate : Double; external name 'sampleRate'; 
    Flength_ : NativeInt; external name 'length'; 
    Fduration : Double; external name 'duration'; 
    FnumberOfChannels : NativeInt; external name 'numberOfChannels'; 
  Public
    function getChannelData(channel : NativeInt): TJSFloat32Array;
    Procedure copyFromChannel(destination : TJSFloat32Array; channelNumber : NativeInt; startInChannel : NativeInt); overload;
    Procedure copyFromChannel(destination : TJSFloat32Array; channelNumber : NativeInt); overload;
    Procedure copyToChannel(source : TJSFloat32Array; channelNumber : NativeInt; startInChannel : NativeInt); overload;
    Procedure copyToChannel(source : TJSFloat32Array; channelNumber : NativeInt); overload;
    Property sampleRate : Double Read FsampleRate; 
    Property length_ : NativeInt Read Flength_; 
    Property duration : Double Read Fduration; 
    Property numberOfChannels : NativeInt Read FnumberOfChannels; 
  end;
  
  { --------------------------------------------------------------------
    TJSAudioNode
    --------------------------------------------------------------------}
  
TJSAudioNode = class external name 'AudioNode'  (TJSEventTarget)
  Private
    Fcontext : TJSBaseAudioContext; external name 'context'; 
    FnumberOfInputs : NativeInt; external name 'numberOfInputs'; 
    FnumberOfOutputs : NativeInt; external name 'numberOfOutputs'; 
  Public
      channelCount : NativeInt;
      _channelCountMode : ChannelCountMode;external name 'channelCountMode';
      _channelInterpretation : ChannelInterpretation;external name 'channelInterpretation';
    function connect(destinationNode : TJSAudioNode; output : NativeInt; input : NativeInt): TJSAudioNode; overload;
    function connect(destinationNode : TJSAudioNode): TJSAudioNode; overload;
    function connect(destinationNode : TJSAudioNode; output : NativeInt): TJSAudioNode; overload;
    Procedure connect(destinationParam : TJSAudioParam; output : NativeInt); overload;
    Procedure connect(destinationParam : TJSAudioParam); overload;
    Procedure disconnect;
    Procedure disconnect(output : NativeInt);
    Procedure disconnect(destinationNode : TJSAudioNode);
    Procedure disconnect(destinationNode : TJSAudioNode; output : NativeInt);
    Procedure disconnect(destinationNode : TJSAudioNode; output : NativeInt; input : NativeInt);
    Procedure disconnect(destinationParam : TJSAudioParam);
    Procedure disconnect(destinationParam : TJSAudioParam; output : NativeInt);
    Property context : TJSBaseAudioContext Read Fcontext; 
    Property numberOfInputs : NativeInt Read FnumberOfInputs; 
    Property numberOfOutputs : NativeInt Read FnumberOfOutputs; 
  end;
  
  { --------------------------------------------------------------------
    TJSAudioParam
    --------------------------------------------------------------------}
  
  TJSAudioParam = class external name 'AudioParam' 
  Private
    FdefaultValue : Double; external name 'defaultValue'; 
    FminValue : Double; external name 'minValue'; 
    FmaxValue : Double; external name 'maxValue'; 
  Public
      value : Double;
      _automationRate : AutomationRate;external name 'automationRate';
    function setValueAtTime(value : Double; startTime : Double): TJSAudioParam;
    function linearRampToValueAtTime(value : Double; endTime : Double): TJSAudioParam;
    function exponentialRampToValueAtTime(value : Double; endTime : Double): TJSAudioParam;
    function setTargetAtTime(target : Double; startTime : Double; timeConstant : Double): TJSAudioParam;
    function setValueCurveAtTime(values : TDoubleDynArray; startTime : Double; duration : Double): TJSAudioParam;
    function cancelScheduledValues(cancelTime : Double): TJSAudioParam;
    function cancelAndHoldAtTime(cancelTime : Double): TJSAudioParam;
    Property defaultValue : Double Read FdefaultValue; 
    Property minValue : Double Read FminValue; 
    Property maxValue : Double Read FmaxValue; 
  end;
  
  { --------------------------------------------------------------------
    TJSAudioScheduledSourceNode
    --------------------------------------------------------------------}
  
  TJSAudioScheduledSourceNode = class external name 'AudioScheduledSourceNode'  (TJSAudioNode)
  Private
  Public
      onended : TJSEVentHandler;
    Procedure start(when : Double); overload;
    Procedure start; overload;
    Procedure stop(when : Double); overload;
    Procedure stop; overload;
  end;
  
  { --------------------------------------------------------------------
    TJSAnalyserNode
    --------------------------------------------------------------------}
  
  TJSAnalyserNode = class external name 'AnalyserNode'  (TJSAudioNode)
  Private
    FfrequencyBinCount : NativeInt; external name 'frequencyBinCount'; 
  Public
      fftSize : NativeInt;
      minDecibels : Double;
      maxDecibels : Double;
      smoothingTimeConstant : Double;
    Procedure getFloatFrequencyData(array_ : TJSFloat32Array);
    Procedure getByteFrequencyData(array_ : TJSUint8Array);
    Procedure getFloatTimeDomainData(array_ : TJSFloat32Array);
    Procedure getByteTimeDomainData(array_ : TJSUint8Array);
    Property frequencyBinCount : NativeInt Read FfrequencyBinCount; 
  end;
  
  { --------------------------------------------------------------------
    TJSAudioBufferSourceNode
    --------------------------------------------------------------------}
  
  TJSAudioBufferSourceNode = class external name 'AudioBufferSourceNode'  (TJSAudioScheduledSourceNode)
  Private
    FplaybackRate : TJSAudioParam; external name 'playbackRate'; 
    Fdetune : TJSAudioParam; external name 'detune'; 
  Public
      buffer : TJSAudioBuffer;
      loop : boolean;
      loopStart : Double;
      loopEnd : Double;
    Procedure start(when : Double; offset : Double; duration : Double); overload;
    Procedure start; overload;
    Procedure start(when : Double); overload;
    Procedure start(when : Double; offset : Double); overload;
    Property playbackRate : TJSAudioParam Read FplaybackRate; 
    Property detune : TJSAudioParam Read Fdetune; 
  end;
  
  { --------------------------------------------------------------------
    TJSAudioDestinationNode
    --------------------------------------------------------------------}
  
  TJSAudioDestinationNode = class external name 'AudioDestinationNode'  (TJSAudioNode)
  Private
    FmaxChannelCount : NativeInt; external name 'maxChannelCount'; 
  Public
    Property maxChannelCount : NativeInt Read FmaxChannelCount; 
  end;
  
  { --------------------------------------------------------------------
    TJSAudioListener
    --------------------------------------------------------------------}
  
  TJSAudioListener = class external name 'AudioListener' 
  Private
    FpositionX : TJSAudioParam; external name 'positionX'; 
    FpositionY : TJSAudioParam; external name 'positionY'; 
    FpositionZ : TJSAudioParam; external name 'positionZ'; 
    FforwardX : TJSAudioParam; external name 'forwardX'; 
    FforwardY : TJSAudioParam; external name 'forwardY'; 
    FforwardZ : TJSAudioParam; external name 'forwardZ'; 
    FupX : TJSAudioParam; external name 'upX'; 
    FupY : TJSAudioParam; external name 'upY'; 
    FupZ : TJSAudioParam; external name 'upZ'; 
  Public
    Procedure setPosition(x : Double; y : Double; z : Double);
    Procedure setOrientation(x : Double; y : Double; z : Double; xUp : Double; yUp : Double; zUp : Double);
    Property positionX : TJSAudioParam Read FpositionX; 
    Property positionY : TJSAudioParam Read FpositionY; 
    Property positionZ : TJSAudioParam Read FpositionZ; 
    Property forwardX : TJSAudioParam Read FforwardX; 
    Property forwardY : TJSAudioParam Read FforwardY; 
    Property forwardZ : TJSAudioParam Read FforwardZ; 
    Property upX : TJSAudioParam Read FupX; 
    Property upY : TJSAudioParam Read FupY; 
    Property upZ : TJSAudioParam Read FupZ; 
  end;
  
  { --------------------------------------------------------------------
    TJSAudioProcessingEvent
    --------------------------------------------------------------------}
  
  TJSAudioProcessingEvent = class external name 'AudioProcessingEvent'  (TJSEvent)
  Private
    FplaybackTime : Double; external name 'playbackTime'; 
    FinputBuffer : TJSAudioBuffer; external name 'inputBuffer'; 
    FoutputBuffer : TJSAudioBuffer; external name 'outputBuffer'; 
  Public
    Property playbackTime : Double Read FplaybackTime; 
    Property inputBuffer : TJSAudioBuffer Read FinputBuffer; 
    Property outputBuffer : TJSAudioBuffer Read FoutputBuffer; 
  end;
  
  { --------------------------------------------------------------------
    TJSBiquadFilterNode
    --------------------------------------------------------------------}
  
  TJSBiquadFilterNode = class external name 'BiquadFilterNode'  (TJSAudioNode)
  Private
    Ffrequency : TJSAudioParam; external name 'frequency'; 
    Fdetune : TJSAudioParam; external name 'detune'; 
    FQ : TJSAudioParam; external name 'Q'; 
    Fgain : TJSAudioParam; external name 'gain'; 
  Public
      type_ : BiquadFilterType;external name 'type';
    Procedure getFrequencyResponse(frequencyHz : TJSFloat32Array; magResponse : TJSFloat32Array; phaseResponse : TJSFloat32Array);
    Property frequency : TJSAudioParam Read Ffrequency; 
    Property detune : TJSAudioParam Read Fdetune; 
    Property Q : TJSAudioParam Read FQ; 
    Property gain : TJSAudioParam Read Fgain; 
  end;
  
  { --------------------------------------------------------------------
    TJSChannelMergerNode
    --------------------------------------------------------------------}
  
  TJSChannelMergerNode = class external name 'ChannelMergerNode'  (TJSAudioNode)
  Private
  Public
  end;
  
  { --------------------------------------------------------------------
    TJSChannelSplitterNode
    --------------------------------------------------------------------}
  
  TJSChannelSplitterNode = class external name 'ChannelSplitterNode'  (TJSAudioNode)
  Private
  Public
  end;
  
  { --------------------------------------------------------------------
    TJSConstantSourceNode
    --------------------------------------------------------------------}
  
  TJSConstantSourceNode = class external name 'ConstantSourceNode'  (TJSAudioScheduledSourceNode)
  Private
    Foffset : TJSAudioParam; external name 'offset'; 
  Public
    Property offset : TJSAudioParam Read Foffset; 
  end;
  
  { --------------------------------------------------------------------
    TJSConvolverNode
    --------------------------------------------------------------------}
  
  TJSConvolverNode = class external name 'ConvolverNode'  (TJSAudioNode)
  Private
  Public
      buffer : TJSAudioBuffer;
      normalize : boolean;
  end;
  
  { --------------------------------------------------------------------
    TJSDelayNode
    --------------------------------------------------------------------}
  
  TJSDelayNode = class external name 'DelayNode'  (TJSAudioNode)
  Private
    FdelayTime : TJSAudioParam; external name 'delayTime'; 
  Public
    Property delayTime : TJSAudioParam Read FdelayTime; 
  end;
  
  { --------------------------------------------------------------------
    TJSDynamicsCompressorNode
    --------------------------------------------------------------------}
  
  TJSDynamicsCompressorNode = class external name 'DynamicsCompressorNode'  (TJSAudioNode)
  Private
    Fthreshold : TJSAudioParam; external name 'threshold'; 
    Fknee : TJSAudioParam; external name 'knee'; 
    Fratio : TJSAudioParam; external name 'ratio'; 
    Freduction : Double; external name 'reduction'; 
    Fattack : TJSAudioParam; external name 'attack'; 
    Frelease : TJSAudioParam; external name 'release'; 
  Public
    Property threshold : TJSAudioParam Read Fthreshold; 
    Property knee : TJSAudioParam Read Fknee; 
    Property ratio : TJSAudioParam Read Fratio; 
    Property reduction : Double Read Freduction; 
    Property attack : TJSAudioParam Read Fattack; 
    Property release : TJSAudioParam Read Frelease; 
  end;
  
  { --------------------------------------------------------------------
    TJSGainNode
    --------------------------------------------------------------------}
  
  TJSGainNode = class external name 'GainNode'  (TJSAudioNode)
  Private
    Fgain : TJSAudioParam; external name 'gain'; 
  Public
    Property gain : TJSAudioParam Read Fgain; 
  end;
  
  { --------------------------------------------------------------------
    TJSIIRFilterNode
    --------------------------------------------------------------------}
  
  TJSIIRFilterNode = class external name 'IIRFilterNode'  (TJSAudioNode)
  Private
  Public
    Procedure getFrequencyResponse(frequencyHz : TJSFloat32Array; magResponse : TJSFloat32Array; phaseResponse : TJSFloat32Array);
  end;
  
  { --------------------------------------------------------------------
    TJSMediaElementAudioSourceNode
    --------------------------------------------------------------------}
  
  TJSMediaElementAudioSourceNode = class external name 'MediaElementAudioSourceNode'  (TJSAudioNode)
  Private
    FmediaElement : TJSElement; external name 'mediaElement'; 
  Public
    Property mediaElement : TJSElement Read FmediaElement; 
  end;
  
  { --------------------------------------------------------------------
    TJSMediaStreamAudioDestinationNode
    --------------------------------------------------------------------}
  
  TJSMediaStreamAudioDestinationNode = class external name 'MediaStreamAudioDestinationNode'  (TJSAudioNode)
  Private
    Fstream : JSValue; external name 'stream'; 
  Public
    Property stream : JSValue Read Fstream; 
  end;
  
  { --------------------------------------------------------------------
    TJSMediaStreamAudioSourceNode
    --------------------------------------------------------------------}
  
  TJSMediaStreamAudioSourceNode = class external name 'MediaStreamAudioSourceNode'  (TJSAudioNode)
  Private
    FmediaStream : JSValue; external name 'mediaStream'; 
  Public
    Property mediaStream : JSValue Read FmediaStream; 
  end;
  
  { --------------------------------------------------------------------
    TJSMediaStreamTrackAudioSourceNode
    --------------------------------------------------------------------}
  
  TJSMediaStreamTrackAudioSourceNode = class external name 'MediaStreamTrackAudioSourceNode'  (TJSAudioNode)
  Private
  Public
  end;
  
  { --------------------------------------------------------------------
    TJSOscillatorNode
    --------------------------------------------------------------------}
  
  TJSOscillatorNode = class external name 'OscillatorNode'  (TJSAudioScheduledSourceNode)
  Private
    Ffrequency : TJSAudioParam; external name 'frequency'; 
    Fdetune : TJSAudioParam; external name 'detune'; 
  Public
      type_ : OscillatorType;external name 'type';
    Procedure setPeriodicWave(periodicWave : TJSPeriodicWave);
    Property frequency : TJSAudioParam Read Ffrequency; 
    Property detune : TJSAudioParam Read Fdetune; 
  end;
  
  { --------------------------------------------------------------------
    TJSPannerNode
    --------------------------------------------------------------------}
  
  TJSPannerNode = class external name 'PannerNode'  (TJSAudioNode)
  Private
    FpositionX : TJSAudioParam; external name 'positionX'; 
    FpositionY : TJSAudioParam; external name 'positionY'; 
    FpositionZ : TJSAudioParam; external name 'positionZ'; 
    ForientationX : TJSAudioParam; external name 'orientationX'; 
    ForientationY : TJSAudioParam; external name 'orientationY'; 
    ForientationZ : TJSAudioParam; external name 'orientationZ'; 
  Public
      panningModel : PanningModelType;
      distanceModel : DistanceModelType;
      refDistance : Double;
      maxDistance : Double;
      rolloffFactor : Double;
      coneInnerAngle : Double;
      coneOuterAngle : Double;
      coneOuterGain : Double;
    Procedure setPosition(x : Double; y : Double; z : Double);
    Procedure setOrientation(x : Double; y : Double; z : Double);
    Property positionX : TJSAudioParam Read FpositionX; 
    Property positionY : TJSAudioParam Read FpositionY; 
    Property positionZ : TJSAudioParam Read FpositionZ; 
    Property orientationX : TJSAudioParam Read ForientationX; 
    Property orientationY : TJSAudioParam Read ForientationY; 
    Property orientationZ : TJSAudioParam Read ForientationZ; 
  end;
  
  { --------------------------------------------------------------------
    TJSPeriodicWave
    --------------------------------------------------------------------}
  
  TJSPeriodicWave = class external name 'PeriodicWave' 
  Private
  Public
  end;
  
  { --------------------------------------------------------------------
    TJSScriptProcessorNode
    --------------------------------------------------------------------}
  
  TJSScriptProcessorNode = class external name 'ScriptProcessorNode'  (TJSAudioNode)
  Private
    FbufferSize : Integer; external name 'bufferSize'; 
  Public
      onaudioprocess : TJSEVentHandler;
    Property bufferSize : Integer Read FbufferSize; 
  end;
  
  { --------------------------------------------------------------------
    TJSStereoPannerNode
    --------------------------------------------------------------------}
  
  TJSStereoPannerNode = class external name 'StereoPannerNode'  (TJSAudioNode)
  Private
    Fpan : TJSAudioParam; external name 'pan'; 
  Public
    Property pan : TJSAudioParam Read Fpan; 
  end;
  
  { --------------------------------------------------------------------
    TJSWaveShaperNode
    --------------------------------------------------------------------}
  
  TJSWaveShaperNode = class external name 'WaveShaperNode'  (TJSAudioNode)
  Private
  Public
      curve : TJSFloat32Array;
      oversample : OverSampleType;
  end;
  
  { --------------------------------------------------------------------
    TJSAudioWorklet
    --------------------------------------------------------------------}
  
  TJSAudioWorklet = class external name 'AudioWorklet'  (TJSOBject)
  Private
  Public
  end;
  
  { --------------------------------------------------------------------
    TJSAudioWorkletGlobalScope
    --------------------------------------------------------------------}
  
  TJSAudioWorkletGlobalScope = class external name 'AudioWorkletGlobalScope'  (TJSObject)
  Private
    FcurrentFrame : NativeInt; external name 'currentFrame'; 
    FcurrentTime : Double; external name 'currentTime'; 
    FsampleRate : Double; external name 'sampleRate'; 
  Public
    Procedure registerProcessor(name : String; processorCtor : TProcedure);
    Property currentFrame : NativeInt Read FcurrentFrame; 
    Property currentTime : Double Read FcurrentTime; 
    Property sampleRate : Double Read FsampleRate; 
  end;
  
  { --------------------------------------------------------------------
    TJSAudioParamMap
    --------------------------------------------------------------------}
  
  TJSAudioParamMap = class external name 'AudioParamMap' 
  Private
  Public
  end;
  
  { --------------------------------------------------------------------
    TJSAudioWorkletNode
    --------------------------------------------------------------------}
  
  TJSAudioWorkletNode = class external name 'AudioWorkletNode'  (TJSAudioNode)
  Private
    Fparameters : TJSAudioParamMap; external name 'parameters'; 
    Fport : TJSMessagePort; external name 'port'; 
  Public
      onprocessorerror : TJSEVentHandler;
    Property parameters : TJSAudioParamMap Read Fparameters; 
    Property port : TJSMessagePort Read Fport; 
  end;
  
  { --------------------------------------------------------------------
    TJSAudioWorkletProcessor
    --------------------------------------------------------------------}
  
  TJSAudioWorkletProcessor = class external name 'AudioWorkletProcessor' 
  Private
    Fport : TJSMessagePort; external name 'port'; 
  Public
    Property port : TJSMessagePort Read Fport; 
  end;

implementation


end.
