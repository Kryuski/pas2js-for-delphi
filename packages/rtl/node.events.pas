unit node.events;

{$mode objfpc}
{$ModeSwitch externalclass}

interface

uses
  JS;

Type
  TNJSEventEmitterHandler = reference to procedure(arg : JSValue);
  TNJSEventEmitterHandlerArray = array of TNJSEventEmitterHandler;

  TNJSEventEmitter = class external name 'EventEmitter' (TJSObject)
    class var captureRejections : Boolean;
    class var defaultMaxListeners : Integer;
    class var errorMonitor : TNJSEventEmitter;
    function addListener(const EventName : String; Listener : TNJSEventEmitterHandler) : TNJSEventEmitter;
    function on_(const EventName : String; Listener : TNJSEventEmitterHandler) : TNJSEventEmitter; external name 'on';
    function once(const EventName : String; Listener : TNJSEventEmitterHandler) : TNJSEventEmitter;
    function off(const EventName : String; Listener : TNJSEventEmitterHandler) : TNJSEventEmitter;
    function emit(const EventName : String) : Boolean; varargs;
    function eventnames : TJSStringDynArray;
    function getMaxListeners : Integer;
    function listenerCount(const EventName : String) : Integer;
    function listeners(const EventName : String) : TNJSEventEmitterHandlerArray;
    function prependListener(const EventName : String; Listener : TNJSEventEmitterHandler) : TNJSEventEmitter;
    function prependOnceListener(const EventName : String; Listener : TNJSEventEmitterHandler) : TNJSEventEmitter;
    function removeListener(const EventName : String; Listener : TNJSEventEmitterHandler) : TNJSEventEmitter;
    function setMaxListeners(aMax : Integer): TNJSEventEmitter;
    function rawListeners(const EventName : String) : TNJSEventEmitterHandlerArray;
  end;


  TNJSEvents = class external name 'events' (TJSObject)
    function once(emitter : TNJSEventEmitter; aName : string) : TJSPromise;
    function on_(emitter : TNJSEventEmitter; aName : string) : TJSAsyncIterator;
  end;

implementation

end.

