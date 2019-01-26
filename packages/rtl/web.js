rtl.module("Web",["System","Types","JS"],function () {
  "use strict";
  var $mod = this;
  $mod.$rtti.$RefToProcVar("TJSEventHandler",{procsig: rtl.newTIProcSig([["Event",$mod.$rtti["TEventListenerEvent"]]],rtl.boolean)});
  $mod.$rtti.$ProcVar("TJSNodeListCallBack",{procsig: rtl.newTIProcSig([["currentValue",$mod.$rtti["TJSNode"]],["currentIndex",rtl.nativeint],["list",$mod.$rtti["TJSNodeList"]]])});
  $mod.$rtti.$MethodVar("TJSNodeListEvent",{procsig: rtl.newTIProcSig([["currentValue",$mod.$rtti["TJSNode"]],["currentIndex",rtl.nativeint],["list",$mod.$rtti["TJSNodeList"]]]), methodkind: 0});
  $mod.$rtti.$ProcVar("TDOMTokenlistCallBack",{procsig: rtl.newTIProcSig([["Current",rtl.jsvalue],["currentIndex",rtl.nativeint],["list",$mod.$rtti["TJSDOMTokenList"]]])});
  rtl.recNewT($mod,"TJSClientRect",function () {
    this.left = 0.0;
    this.top = 0.0;
    this.right = 0.0;
    this.bottom = 0.0;
    this.$eq = function (b) {
      return (this.left === b.left) && (this.top === b.top) && (this.right === b.right) && (this.bottom === b.bottom);
    };
    this.$assign = function (s) {
      this.left = s.left;
      this.top = s.top;
      this.right = s.right;
      this.bottom = s.bottom;
      return this;
    };
    var $r = $mod.$rtti.$Record("TJSClientRect",{});
    $r.addField("left",rtl.double);
    $r.addField("top",rtl.double);
    $r.addField("right",rtl.double);
    $r.addField("bottom",rtl.double);
  });
  rtl.recNewT($mod,"TJSElementCreationOptions",function () {
    this.named = "";
    this.$eq = function (b) {
      return this.named === b.named;
    };
    this.$assign = function (s) {
      this.named = s.named;
      return this;
    };
    var $r = $mod.$rtti.$Record("TJSElementCreationOptions",{});
    $r.addField("named",rtl.string);
  });
  rtl.recNewT($mod,"TJSEventInit",function () {
    this.bubbles = false;
    this.cancelable = false;
    this.scoped = false;
    this.composed = false;
    this.$eq = function (b) {
      return (this.bubbles === b.bubbles) && (this.cancelable === b.cancelable) && (this.scoped === b.scoped) && (this.composed === b.composed);
    };
    this.$assign = function (s) {
      this.bubbles = s.bubbles;
      this.cancelable = s.cancelable;
      this.scoped = s.scoped;
      this.composed = s.composed;
      return this;
    };
    var $r = $mod.$rtti.$Record("TJSEventInit",{});
    $r.addField("bubbles",rtl.boolean);
    $r.addField("cancelable",rtl.boolean);
    $r.addField("scoped",rtl.boolean);
    $r.addField("composed",rtl.boolean);
  });
  $mod.$rtti.$ProcVar("TJSNameSpaceMapperCallback",{procsig: rtl.newTIProcSig([["aNameSpace",rtl.string]],rtl.string)});
  $mod.$rtti.$RefToProcVar("TJSDataTransferItemCallBack",{procsig: rtl.newTIProcSig([["aData",rtl.string]])});
  $mod.$rtti.$RefToProcVar("TJSDragDropEventHandler",{procsig: rtl.newTIProcSig([["aEvent",$mod.$rtti["TJSDragEvent"]]],rtl.boolean)});
  $mod.$rtti.$RefToProcVar("THTMLClickEventHandler",{procsig: rtl.newTIProcSig([["aEvent",$mod.$rtti["TJSMouseEvent"]]],rtl.boolean)});
  rtl.createClassExt($mod,"TJSAnimationEvent",Event,"",function () {
    this.$init = function () {
    };
    this.$final = function () {
    };
  });
  rtl.createClassExt($mod,"TJSLoadEvent",Event,"",function () {
    this.$init = function () {
    };
    this.$final = function () {
    };
  });
  rtl.createClassExt($mod,"TJSPageTransitionEvent",Event,"",function () {
    this.$init = function () {
    };
    this.$final = function () {
    };
  });
  $mod.$rtti.$RefToProcVar("TJSPageTransitionEventHandler",{procsig: rtl.newTIProcSig([["aEvent",$mod.$rtti["TJSPageTransitionEvent"]]],rtl.boolean)});
  $mod.$rtti.$RefToProcVar("TJSHashChangeEventhandler",{procsig: rtl.newTIProcSig([["aEvent",$mod.$rtti["TJSHashChangeEvent"]]],rtl.boolean)});
  $mod.$rtti.$RefToProcVar("TJSMouseWheelEventHandler",{procsig: rtl.newTIProcSig([["aEvent",$mod.$rtti["TJSWheelEvent"]]],rtl.boolean)});
  $mod.$rtti.$RefToProcVar("TJSMouseEventHandler",{procsig: rtl.newTIProcSig([["aEvent",$mod.$rtti["TJSMouseEvent"]]],rtl.boolean)});
  $mod.$rtti.$RefToProcVar("THTMLAnimationEventHandler",{procsig: rtl.newTIProcSig([["aEvent",$mod.$rtti["TJSAnimationEvent"]]],rtl.boolean)});
  $mod.$rtti.$RefToProcVar("TJSErrorEventHandler",{procsig: rtl.newTIProcSig([["aEvent",$mod.$rtti["TJSErrorEvent"]]],rtl.boolean)});
  $mod.$rtti.$RefToProcVar("TJSFocusEventHandler",{procsig: rtl.newTIProcSig([["aEvent",$mod.$rtti["TJSEvent"]]],rtl.boolean)});
  $mod.$rtti.$RefToProcVar("TJSKeyEventhandler",{procsig: rtl.newTIProcSig([["aEvent",$mod.$rtti["TJSKeyboardEvent"]]],rtl.boolean)});
  $mod.$rtti.$RefToProcVar("TJSLoadEventhandler",{procsig: rtl.newTIProcSig([["aEvent",$mod.$rtti["TJSLoadEvent"]]],rtl.boolean)});
  $mod.$rtti.$RefToProcVar("TJSPointerEventHandler",{procsig: rtl.newTIProcSig([["aEvent",$mod.$rtti["TJSPointerEvent"]]],rtl.boolean)});
  $mod.$rtti.$RefToProcVar("TJSUIEventHandler",{procsig: rtl.newTIProcSig([["aEvent",$mod.$rtti["TJSUIEvent"]]],rtl.boolean)});
  $mod.$rtti.$RefToProcVar("TJSPopStateEventHandler",{procsig: rtl.newTIProcSig([["aEvent",$mod.$rtti["TJSPopStateEvent"]]],rtl.boolean)});
  $mod.$rtti.$RefToProcVar("TJSStorageEventHandler",{procsig: rtl.newTIProcSig([["aEvent",$mod.$rtti["TJSStorageEvent"]]],rtl.boolean)});
  $mod.$rtti.$RefToProcVar("TJSProgressEventhandler",{procsig: rtl.newTIProcSig([["aEvent",$mod.$rtti["TJSProgressEvent"]]],rtl.boolean)});
  $mod.$rtti.$RefToProcVar("TJSTouchEventHandler",{procsig: rtl.newTIProcSig([["aEvent",$mod.$rtti["TJSTouchEvent"]]],rtl.boolean)});
  rtl.createClass($mod,"TJSIDBTransactionMode",pas.System.TObject,function () {
    this.readonly = "readonly";
    this.readwrite = "readwrite";
    this.versionchange = "versionchange";
  });
  rtl.recNewT($mod,"TJSIDBIndexParameters",function () {
    this.unique = false;
    this.multiEntry = false;
    this.locale = "";
    this.$eq = function (b) {
      return (this.unique === b.unique) && (this.multiEntry === b.multiEntry) && (this.locale === b.locale);
    };
    this.$assign = function (s) {
      this.unique = s.unique;
      this.multiEntry = s.multiEntry;
      this.locale = s.locale;
      return this;
    };
    var $r = $mod.$rtti.$Record("TJSIDBIndexParameters",{});
    $r.addField("unique",rtl.boolean);
    $r.addField("multiEntry",rtl.boolean);
    $r.addField("locale",rtl.string);
  });
  rtl.recNewT($mod,"TJSCreateObjectStoreOptions",function () {
    this.keyPath = undefined;
    this.autoIncrement = false;
    this.$eq = function (b) {
      return (this.keyPath === b.keyPath) && (this.autoIncrement === b.autoIncrement);
    };
    this.$assign = function (s) {
      this.keyPath = s.keyPath;
      this.autoIncrement = s.autoIncrement;
      return this;
    };
    var $r = $mod.$rtti.$Record("TJSCreateObjectStoreOptions",{});
    $r.addField("keyPath",rtl.jsvalue);
    $r.addField("autoIncrement",rtl.boolean);
  });
  rtl.recNewT($mod,"TJSPositionError",function () {
    this.code = 0;
    this.message = "";
    this.$eq = function (b) {
      return (this.code === b.code) && (this.message === b.message);
    };
    this.$assign = function (s) {
      this.code = s.code;
      this.message = s.message;
      return this;
    };
    var $r = $mod.$rtti.$Record("TJSPositionError",{});
    $r.addField("code",rtl.longint);
    $r.addField("message",rtl.string);
  });
  rtl.recNewT($mod,"TJSPositionOptions",function () {
    this.enableHighAccuracy = false;
    this.timeout = 0;
    this.maximumAge = 0;
    this.$eq = function (b) {
      return (this.enableHighAccuracy === b.enableHighAccuracy) && (this.timeout === b.timeout) && (this.maximumAge === b.maximumAge);
    };
    this.$assign = function (s) {
      this.enableHighAccuracy = s.enableHighAccuracy;
      this.timeout = s.timeout;
      this.maximumAge = s.maximumAge;
      return this;
    };
    var $r = $mod.$rtti.$Record("TJSPositionOptions",{});
    $r.addField("enableHighAccuracy",rtl.boolean);
    $r.addField("timeout",rtl.longint);
    $r.addField("maximumAge",rtl.longint);
  });
  rtl.recNewT($mod,"TJSCoordinates",function () {
    this.latitude = 0.0;
    this.longitude = 0.0;
    this.altitude = 0.0;
    this.accuracy = 0.0;
    this.altitudeAccuracy = 0.0;
    this.heading = 0.0;
    this.speed = 0.0;
    this.$eq = function (b) {
      return (this.latitude === b.latitude) && (this.longitude === b.longitude) && (this.altitude === b.altitude) && (this.accuracy === b.accuracy) && (this.altitudeAccuracy === b.altitudeAccuracy) && (this.heading === b.heading) && (this.speed === b.speed);
    };
    this.$assign = function (s) {
      this.latitude = s.latitude;
      this.longitude = s.longitude;
      this.altitude = s.altitude;
      this.accuracy = s.accuracy;
      this.altitudeAccuracy = s.altitudeAccuracy;
      this.heading = s.heading;
      this.speed = s.speed;
      return this;
    };
    var $r = $mod.$rtti.$Record("TJSCoordinates",{});
    $r.addField("latitude",rtl.double);
    $r.addField("longitude",rtl.double);
    $r.addField("altitude",rtl.double);
    $r.addField("accuracy",rtl.double);
    $r.addField("altitudeAccuracy",rtl.double);
    $r.addField("heading",rtl.double);
    $r.addField("speed",rtl.double);
  });
  rtl.recNewT($mod,"TJSPosition",function () {
    this.timestamp = "";
    this.$new = function () {
      var r = Object.create(this);
      r.coords = $mod.TJSCoordinates.$new();
      return r;
    };
    this.$eq = function (b) {
      return this.coords.$eq(b.coords) && (this.timestamp === b.timestamp);
    };
    this.$assign = function (s) {
      this.coords.$assign(s.coords);
      this.timestamp = s.timestamp;
      return this;
    };
    var $r = $mod.$rtti.$Record("TJSPosition",{});
    $r.addField("coords",$mod.$rtti["TJSCoordinates"]);
    $r.addField("timestamp",rtl.string);
  });
  $mod.$rtti.$ProcVar("TJSGeoLocationCallback",{procsig: rtl.newTIProcSig([["aPosition",$mod.$rtti["TJSPosition"]]])});
  $mod.$rtti.$MethodVar("TJSGeoLocationEvent",{procsig: rtl.newTIProcSig([["aPosition",$mod.$rtti["TJSPosition"]]]), methodkind: 0});
  $mod.$rtti.$ProcVar("TJSGeoLocationErrorCallback",{procsig: rtl.newTIProcSig([["aValue",$mod.$rtti["TJSPositionError"]]])});
  $mod.$rtti.$MethodVar("TJSGeoLocationErrorEvent",{procsig: rtl.newTIProcSig([["aValue",$mod.$rtti["TJSPositionError"]]]), methodkind: 0});
  rtl.recNewT($mod,"TJSServiceWorkerContainerOptions",function () {
    this.scope = "";
    this.$eq = function (b) {
      return this.scope === b.scope;
    };
    this.$assign = function (s) {
      this.scope = s.scope;
      return this;
    };
    var $r = $mod.$rtti.$Record("TJSServiceWorkerContainerOptions",{});
    $r.addField("scope",rtl.string);
  });
  $mod.$rtti.$RefToProcVar("TJSTimerCallBack",{procsig: rtl.newTIProcSig(null)});
  $mod.$rtti.$ProcVar("TFrameRequestCallback",{procsig: rtl.newTIProcSig([["aTime",rtl.double]])});
  $mod.$rtti.$DynArray("TJSWindowArray",{eltype: $mod.$rtti["TJSWindow"]});
  $mod.$rtti.$RefToProcVar("THTMLCanvasToBlobCallback",{procsig: rtl.newTIProcSig([["aBlob",$mod.$rtti["TJSBlob"]]],rtl.boolean)});
  rtl.recNewT($mod,"TJSTextMetrics",function () {
    this.width = 0.0;
    this.actualBoundingBoxLeft = 0.0;
    this.actualBoundingBoxRight = 0.0;
    this.fontBoundingBoxAscent = 0.0;
    this.fontBoundingBoxDescent = 0.0;
    this.actualBoundingBoxAscent = 0.0;
    this.actualBoundingBoxDescent = 0.0;
    this.emHeightAscent = 0.0;
    this.emHeightDescent = 0.0;
    this.hangingBaseline = 0.0;
    this.alphabeticBaseline = 0.0;
    this.ideographicBaseline = 0.0;
    this.$eq = function (b) {
      return (this.width === b.width) && (this.actualBoundingBoxLeft === b.actualBoundingBoxLeft) && (this.actualBoundingBoxRight === b.actualBoundingBoxRight) && (this.fontBoundingBoxAscent === b.fontBoundingBoxAscent) && (this.fontBoundingBoxDescent === b.fontBoundingBoxDescent) && (this.actualBoundingBoxAscent === b.actualBoundingBoxAscent) && (this.actualBoundingBoxDescent === b.actualBoundingBoxDescent) && (this.emHeightAscent === b.emHeightAscent) && (this.emHeightDescent === b.emHeightDescent) && (this.hangingBaseline === b.hangingBaseline) && (this.alphabeticBaseline === b.alphabeticBaseline) && (this.ideographicBaseline === b.ideographicBaseline);
    };
    this.$assign = function (s) {
      this.width = s.width;
      this.actualBoundingBoxLeft = s.actualBoundingBoxLeft;
      this.actualBoundingBoxRight = s.actualBoundingBoxRight;
      this.fontBoundingBoxAscent = s.fontBoundingBoxAscent;
      this.fontBoundingBoxDescent = s.fontBoundingBoxDescent;
      this.actualBoundingBoxAscent = s.actualBoundingBoxAscent;
      this.actualBoundingBoxDescent = s.actualBoundingBoxDescent;
      this.emHeightAscent = s.emHeightAscent;
      this.emHeightDescent = s.emHeightDescent;
      this.hangingBaseline = s.hangingBaseline;
      this.alphabeticBaseline = s.alphabeticBaseline;
      this.ideographicBaseline = s.ideographicBaseline;
      return this;
    };
    var $r = $mod.$rtti.$Record("TJSTextMetrics",{});
    $r.addField("width",rtl.double);
    $r.addField("actualBoundingBoxLeft",rtl.double);
    $r.addField("actualBoundingBoxRight",rtl.double);
    $r.addField("fontBoundingBoxAscent",rtl.double);
    $r.addField("fontBoundingBoxDescent",rtl.double);
    $r.addField("actualBoundingBoxAscent",rtl.double);
    $r.addField("actualBoundingBoxDescent",rtl.double);
    $r.addField("emHeightAscent",rtl.double);
    $r.addField("emHeightDescent",rtl.double);
    $r.addField("hangingBaseline",rtl.double);
    $r.addField("alphabeticBaseline",rtl.double);
    $r.addField("ideographicBaseline",rtl.double);
  });
  $mod.$rtti.$RefToProcVar("TJSOnReadyStateChangeHandler",{procsig: rtl.newTIProcSig(null)});
  rtl.recNewT($mod,"TJSWheelEventInit",function () {
    this.deltaX = 0.0;
    this.deltaY = 0.0;
    this.deltaZ = 0.0;
    this.deltaMode = 0;
    this.$eq = function (b) {
      return (this.deltaX === b.deltaX) && (this.deltaY === b.deltaY) && (this.deltaZ === b.deltaZ) && (this.deltaMode === b.deltaMode);
    };
    this.$assign = function (s) {
      this.deltaX = s.deltaX;
      this.deltaY = s.deltaY;
      this.deltaZ = s.deltaZ;
      this.deltaMode = s.deltaMode;
      return this;
    };
    var $r = $mod.$rtti.$Record("TJSWheelEventInit",{});
    $r.addField("deltaX",rtl.double);
    $r.addField("deltaY",rtl.double);
    $r.addField("deltaZ",rtl.double);
    $r.addField("deltaMode",rtl.nativeint);
  });
  rtl.createClass($mod,"TJSKeyNames",pas.System.TObject,function () {
    this.Alt = "Alt";
    this.AltGraph = "AltGraph";
    this.CapsLock = "CapsLock";
    this.Control = "Control";
    this.Fn = "Fn";
    this.FnLock = "FnLock";
    this.Hyper = "Hyper";
    this.Meta = "Meta";
    this.NumLock = "NumLock";
    this.ScrollLock = "ScrollLock";
    this.Shift = "Shift";
    this.Super = "Super";
    this.Symbol = "Symbol";
    this.SymbolLock = "SymbolLock";
    this.Enter = "Enter";
    this.Tab = "Tab";
    this.Space = " ";
    this.ArrowDown = "ArrowDown";
    this.ArrowLeft = "ArrowLeft";
    this.ArrowRight = "ArrowRight";
    this.ArrowUp = "ArrowUp";
    this._End = "End";
    this.Home = "Home";
    this.PageDown = "PageDown";
    this.PageUp = "PageUp";
    this.BackSpace = "Backspace";
    this.Clear = "Clear";
    this.Copy = "Copy";
    this.CrSel = "CrSel";
    this.Cut = "Cut";
    this.Delete = "Delete";
    this.EraseEof = "EraseEof";
    this.ExSel = "ExSel";
    this.Insert = "Insert";
    this.Paste = "Paste";
    this.Redo = "Redo";
    this.Undo = "Undo";
    this.Accept = "Accept";
    this.Again = "Again";
    this.Attn = "Attn";
    this.Cancel = "Cancel";
    this.ContextMenu = "Contextmenu";
    this.Escape = "Escape";
    this.Execute = "Execute";
    this.Find = "Find";
    this.Finish = "Finish";
    this.Help = "Help";
    this.Pause = "Pause";
    this.Play = "Play";
    this.Props = "Props";
    this.Select = "Select";
    this.ZoomIn = "ZoomIn";
    this.ZoomOut = "ZoomOut";
    this.BrightnessDown = "BrightnessDown";
    this.BrightnessUp = "BrightnessUp";
    this.Eject = "Eject";
    this.LogOff = "LogOff";
    this.Power = "Power";
    this.PowerOff = "PowerOff";
    this.PrintScreen = "PrintScreen";
    this.Hibernate = "Hibernate";
    this.Standby = "Standby";
    this.WakeUp = "WakeUp";
    this.AllCandidates = "AllCandidates";
    this.Alphanumeric = "Alphanumeric";
    this.CodeInput = "CodeInput";
    this.Compose = "Compose";
    this.Convert = "Convert";
    this.Dead = "Dead";
    this.FinalMode = "FinalMode";
    this.GroupFirst = "GroupFirst";
    this.GroupLast = "GroupLast";
    this.GroupNext = "GroupNext";
    this.GroupPrevious = "GroupPrevious";
    this.ModelChange = "ModelChange";
    this.NextCandidate = "NextCandidate";
    this.NonConvert = "NonConvert";
    this.PreviousCandidate = "PreviousCandidate";
    this.Process = "Process";
    this.SingleCandidate = "SingleCandidate";
    this.HangulMode = "HangulMode";
    this.HanjaMode = "HanjaMode";
    this.JunjaMode = "JunjaMode";
    this.Eisu = "Eisu";
    this.Hankaku = "Hankaku";
    this.Hiranga = "Hiranga";
    this.HirangaKatakana = "HirangaKatakana";
    this.KanaMode = "KanaMode";
    this.Katakana = "Katakana";
    this.Romaji = "Romaji";
    this.Zenkaku = "Zenkaku";
    this.ZenkakuHanaku = "ZenkakuHanaku";
    this.F1 = "F1";
    this.F2 = "F2";
    this.F3 = "F3";
    this.F4 = "F4";
    this.F5 = "F5";
    this.F6 = "F6";
    this.F7 = "F7";
    this.F8 = "F8";
    this.F9 = "F9";
    this.F10 = "F10";
    this.F11 = "F11";
    this.F12 = "F12";
    this.F13 = "F13";
    this.F14 = "F14";
    this.F15 = "F15";
    this.F16 = "F16";
    this.F17 = "F17";
    this.F18 = "F18";
    this.F19 = "F19";
    this.F20 = "F20";
    this.Soft1 = "Soft1";
    this.Soft2 = "Soft2";
    this.Soft3 = "Soft3";
    this.Soft4 = "Soft4";
    this.Decimal = "Decimal";
    this.Key11 = "Key11";
    this.Key12 = "Key12";
    this.Multiply = "Multiply";
    this.Add = "Add";
    this.NumClear = "Clear";
    this.Divide = "Divide";
    this.Subtract = "Subtract";
    this.Separator = "Separator";
    this.AppSwitch = "AppSwitch";
    this.Call = "Call";
    this.Camera = "Camera";
    this.CameraFocus = "CameraFocus";
    this.EndCall = "EndCall";
    this.GoBack = "GoBack";
    this.GoHome = "GoHome";
    this.HeadsetHook = "HeadsetHook";
    this.LastNumberRedial = "LastNumberRedial";
    this.Notification = "Notification";
    this.MannerMode = "MannerMode";
    this.VoiceDial = "VoiceDial";
  });
  rtl.recNewT($mod,"TJSMutationRecord",function () {
    this.type_ = "";
    this.target = null;
    this.addedNodes = null;
    this.removedNodes = null;
    this.previousSibling = null;
    this.nextSibling = null;
    this.attributeName = "";
    this.attributeNamespace = "";
    this.oldValue = "";
    this.$eq = function (b) {
      return (this.type_ === b.type_) && (this.target === b.target) && (this.addedNodes === b.addedNodes) && (this.removedNodes === b.removedNodes) && (this.previousSibling === b.previousSibling) && (this.nextSibling === b.nextSibling) && (this.attributeName === b.attributeName) && (this.attributeNamespace === b.attributeNamespace) && (this.oldValue === b.oldValue);
    };
    this.$assign = function (s) {
      this.type_ = s.type_;
      this.target = s.target;
      this.addedNodes = s.addedNodes;
      this.removedNodes = s.removedNodes;
      this.previousSibling = s.previousSibling;
      this.nextSibling = s.nextSibling;
      this.attributeName = s.attributeName;
      this.attributeNamespace = s.attributeNamespace;
      this.oldValue = s.oldValue;
      return this;
    };
    var $r = $mod.$rtti.$Record("TJSMutationRecord",{});
    $r.addField("type_",rtl.string);
    $r.addField("target",$mod.$rtti["TJSNode"]);
    $r.addField("addedNodes",$mod.$rtti["TJSNodeList"]);
    $r.addField("removedNodes",$mod.$rtti["TJSNodeList"]);
    $r.addField("previousSibling",$mod.$rtti["TJSNode"]);
    $r.addField("nextSibling",$mod.$rtti["TJSNode"]);
    $r.addField("attributeName",rtl.string);
    $r.addField("attributeNamespace",rtl.string);
    $r.addField("oldValue",rtl.string);
  });
  $mod.$rtti.$DynArray("TJSMutationRecordArray",{eltype: $mod.$rtti["TJSMutationRecord"]});
  $mod.$rtti.$RefToProcVar("TJSMutationCallback",{procsig: rtl.newTIProcSig([["mutations",$mod.$rtti["TJSMutationRecordArray"]],["observer",$mod.$rtti["TJSMutationObserver"]]])});
  rtl.recNewT($mod,"TJSMutationObserverInit",function () {
    this.attributes = false;
    this.attributeOldValue = false;
    this.characterData = false;
    this.characterDataOldValue = false;
    this.childList = false;
    this.subTree = false;
    this.attributeFilter = null;
    this.$eq = function (b) {
      return (this.attributes === b.attributes) && (this.attributeOldValue === b.attributeOldValue) && (this.characterData === b.characterData) && (this.characterDataOldValue === b.characterDataOldValue) && (this.childList === b.childList) && (this.subTree === b.subTree) && (this.attributeFilter === b.attributeFilter);
    };
    this.$assign = function (s) {
      this.attributes = s.attributes;
      this.attributeOldValue = s.attributeOldValue;
      this.characterData = s.characterData;
      this.characterDataOldValue = s.characterDataOldValue;
      this.childList = s.childList;
      this.subTree = s.subTree;
      this.attributeFilter = s.attributeFilter;
      return this;
    };
    var $r = $mod.$rtti.$Record("TJSMutationObserverInit",{});
    $r.addField("attributes",rtl.boolean);
    $r.addField("attributeOldValue",rtl.boolean);
    $r.addField("characterData",rtl.boolean);
    $r.addField("characterDataOldValue",rtl.boolean);
    $r.addField("childList",rtl.boolean);
    $r.addField("subTree",rtl.boolean);
    $r.addField("attributeFilter",pas.JS.$rtti["TJSArray"]);
  });
});
