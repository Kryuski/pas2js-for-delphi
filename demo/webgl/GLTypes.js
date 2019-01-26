rtl.module("System",[],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.LineEnding = "\n";
  this.sLineBreak = $mod.LineEnding;
  this.PathDelim = "\/";
  this.AllowDirectorySeparators = rtl.createSet(47);
  this.AllowDriveSeparators = rtl.createSet(58);
  this.ExtensionSeparator = ".";
  this.MaxSmallint = 32767;
  this.MinSmallint = -32768;
  this.MaxShortInt = 127;
  this.MinShortInt = -128;
  this.MaxByte = 0xFF;
  this.MaxWord = 0xFFFF;
  this.MaxLongint = 0x7fffffff;
  this.MaxCardinal = 0xffffffff;
  this.Maxint = 2147483647;
  this.IsMultiThread = false;
  $mod.$rtti.$inherited("Real",rtl.double,{});
  $mod.$rtti.$inherited("Extended",rtl.double,{});
  $mod.$rtti.$inherited("TDateTime",rtl.double,{});
  $mod.$rtti.$inherited("TTime",$mod.$rtti["TDateTime"],{});
  $mod.$rtti.$inherited("TDate",$mod.$rtti["TDateTime"],{});
  $mod.$rtti.$inherited("Int64",rtl.nativeint,{});
  $mod.$rtti.$inherited("UInt64",rtl.nativeuint,{});
  $mod.$rtti.$inherited("QWord",rtl.nativeuint,{});
  $mod.$rtti.$inherited("Single",rtl.double,{});
  $mod.$rtti.$inherited("Comp",rtl.nativeint,{});
  $mod.$rtti.$inherited("UnicodeString",rtl.string,{});
  $mod.$rtti.$inherited("WideString",rtl.string,{});
  this.TTextLineBreakStyle = {"0": "tlbsLF", tlbsLF: 0, "1": "tlbsCRLF", tlbsCRLF: 1, "2": "tlbsCR", tlbsCR: 2};
  $mod.$rtti.$Enum("TTextLineBreakStyle",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TTextLineBreakStyle});
  rtl.recNewT($mod,"TGuid",function () {
    this.D1 = 0;
    this.D2 = 0;
    this.D3 = 0;
    this.$new = function () {
      var r = Object.create(this);
      r.D4 = rtl.arraySetLength(null,0,8);
      return r;
    };
    this.$eq = function (b) {
      return (this.D1 === b.D1) && (this.D2 === b.D2) && (this.D3 === b.D3) && rtl.arrayEq(this.D4,b.D4);
    };
    this.$assign = function (s) {
      this.D1 = s.D1;
      this.D2 = s.D2;
      this.D3 = s.D3;
      this.D4 = s.D4.slice(0);
      return this;
    };
    var $r = $mod.$rtti.$Record("TGuid",{});
    $r.addField("D1",rtl.longword);
    $r.addField("D2",rtl.word);
    $r.addField("D3",rtl.word);
    $mod.$rtti.$StaticArray("TGuid.D4$a",{dims: [8], eltype: rtl.byte});
    $r.addField("D4",$mod.$rtti["TGuid.D4$a"]);
  });
  $mod.$rtti.$inherited("TGUIDString",rtl.string,{});
  $mod.$rtti.$Class("TObject");
  $mod.$rtti.$ClassRef("TClass",{instancetype: $mod.$rtti["TObject"]});
  rtl.createClass($mod,"TObject",null,function () {
    this.$init = function () {
    };
    this.$final = function () {
    };
    this.Create = function () {
      return this;
    };
    this.Destroy = function () {
    };
    this.Free = function () {
      this.$destroy("Destroy");
    };
    this.ClassType = function () {
      return this;
    };
    this.ClassNameIs = function (Name) {
      var Result = false;
      Result = $impl.SameText(Name,this.$classname);
      return Result;
    };
    this.InheritsFrom = function (aClass) {
      return (aClass!=null) && ((this==aClass) || aClass.isPrototypeOf(this));
    };
    this.AfterConstruction = function () {
    };
    this.BeforeDestruction = function () {
    };
    this.GetInterface = function (iid, obj) {
      var Result = false;
      var i = iid.$intf;
      if (i){
        // iid is the private TGuid of an interface
        i = rtl.getIntfG(this,i.$guid,2);
        if (i){
          obj.set(i);
          return true;
        }
      };
      Result = this.GetInterfaceByStr(rtl.guidrToStr(iid),obj);
      return Result;
    };
    this.GetInterface$1 = function (iidstr, obj) {
      var Result = false;
      Result = this.GetInterfaceByStr(iidstr,obj);
      return Result;
    };
    this.GetInterfaceByStr = function (iidstr, obj) {
      var Result = false;
      if (!$mod.IObjectInstance["$str"]) $mod.IObjectInstance["$str"] = rtl.guidrToStr($mod.IObjectInstance);
      if (iidstr == $mod.IObjectInstance["$str"]) {
        obj.set(this);
        return true;
      };
      var i = rtl.getIntfG(this,iidstr,2);
      obj.set(i);
      return i!==null;
      Result = false;
      return Result;
    };
    this.GetInterfaceWeak = function (iid, obj) {
      var Result = false;
      Result = this.GetInterface(iid,obj);
      if (Result){
        var o = obj.get();
        if (o.$kind==='com'){
          o._Release();
        }
      };
      return Result;
    };
    this.Equals = function (Obj) {
      var Result = false;
      Result = Obj === this;
      return Result;
    };
    this.ToString = function () {
      var Result = "";
      Result = this.$classname;
      return Result;
    };
  });
  this.S_OK = 0;
  this.S_FALSE = 1;
  this.E_NOINTERFACE = -2147467262;
  this.E_UNEXPECTED = -2147418113;
  this.E_NOTIMPL = -2147467263;
  rtl.createInterface($mod,"IUnknown","{00000000-0000-0000-C000-000000000046}",["QueryInterface","_AddRef","_Release"],null,function () {
    this.$kind = "com";
    var $r = this.$rtti;
    $r.addMethod("QueryInterface",1,[["iid",$mod.$rtti["TGuid"],2],["obj",null,4]],rtl.longint);
    $r.addMethod("_AddRef",1,null,rtl.longint);
    $r.addMethod("_Release",1,null,rtl.longint);
  });
  rtl.createInterface($mod,"IInvokable","{88387EF6-BCEE-3E17-9E85-5D491ED4FC10}",[],$mod.IUnknown,function () {
  });
  rtl.createInterface($mod,"IEnumerator","{ECEC7568-4E50-30C9-A2F0-439342DE2ADB}",["GetCurrent","MoveNext","Reset"],$mod.IUnknown,function () {
    var $r = this.$rtti;
    $r.addMethod("GetCurrent",1,null,$mod.$rtti["TObject"]);
    $r.addMethod("MoveNext",1,null,rtl.boolean);
    $r.addMethod("Reset",0,null);
    $r.addProperty("Current",1,$mod.$rtti["TObject"],"GetCurrent","");
  });
  rtl.createInterface($mod,"IEnumerable","{9791C368-4E51-3424-A3CE-D4911D54F385}",["GetEnumerator"],$mod.IUnknown,function () {
    var $r = this.$rtti;
    $r.addMethod("GetEnumerator",1,null,$mod.$rtti["IEnumerator"]);
  });
  rtl.createClass($mod,"TInterfacedObject",$mod.TObject,function () {
    this.$init = function () {
      $mod.TObject.$init.call(this);
      this.fRefCount = 0;
    };
    this.QueryInterface = function (iid, obj) {
      var Result = 0;
      if (this.GetInterface(iid,obj)) {
        Result = 0}
       else Result = -2147467262;
      return Result;
    };
    this._AddRef = function () {
      var Result = 0;
      this.fRefCount += 1;
      Result = this.fRefCount;
      return Result;
    };
    this._Release = function () {
      var Result = 0;
      this.fRefCount -= 1;
      Result = this.fRefCount;
      if (this.fRefCount === 0) this.$destroy("Destroy");
      return Result;
    };
    this.BeforeDestruction = function () {
      if (this.fRefCount !== 0) rtl.raiseE('EHeapMemoryError');
    };
    rtl.addIntf(this,$mod.IUnknown);
  });
  $mod.$rtti.$ClassRef("TInterfacedClass",{instancetype: $mod.$rtti["TInterfacedObject"]});
  rtl.createClass($mod,"TAggregatedObject",$mod.TObject,function () {
    this.$init = function () {
      $mod.TObject.$init.call(this);
      this.fController = null;
    };
    this.GetController = function () {
      var Result = null;
      var $ok = false;
      try {
        Result = rtl.setIntfL(Result,this.fController);
        $ok = true;
      } finally {
        if (!$ok) rtl._Release(Result);
      };
      return Result;
    };
    this.QueryInterface = function (iid, obj) {
      var Result = 0;
      Result = this.fController.QueryInterface(iid,obj);
      return Result;
    };
    this._AddRef = function () {
      var Result = 0;
      Result = this.fController._AddRef();
      return Result;
    };
    this._Release = function () {
      var Result = 0;
      Result = this.fController._Release();
      return Result;
    };
    this.Create$1 = function (aController) {
      $mod.TObject.Create.call(this);
      this.fController = aController;
      return this;
    };
  });
  rtl.createClass($mod,"TContainedObject",$mod.TAggregatedObject,function () {
    this.QueryInterface = function (iid, obj) {
      var Result = 0;
      if (this.GetInterface(iid,obj)) {
        Result = 0}
       else Result = -2147467262;
      return Result;
    };
    rtl.addIntf(this,$mod.IUnknown);
  });
  this.IObjectInstance = $mod.TGuid.$clone({D1: 0xD91C9AF4, D2: 0x3C93, D3: 0x420F, D4: [0xA3,0x03,0xBF,0x5B,0xA8,0x2B,0xFD,0x23]});
  this.IsConsole = false;
  this.FirstDotAtFileNameStartIsExtension = false;
  $mod.$rtti.$ProcVar("TOnParamCount",{procsig: rtl.newTIProcSig(null,rtl.longint)});
  $mod.$rtti.$ProcVar("TOnParamStr",{procsig: rtl.newTIProcSig([["Index",rtl.longint]],rtl.string)});
  this.OnParamCount = null;
  this.OnParamStr = null;
  this.ParamCount = function () {
    var Result = 0;
    if ($mod.OnParamCount != null) {
      Result = $mod.OnParamCount()}
     else Result = 0;
    return Result;
  };
  this.ParamStr = function (Index) {
    var Result = "";
    if ($mod.OnParamStr != null) {
      Result = $mod.OnParamStr(Index)}
     else if (Index === 0) {
      Result = "js"}
     else Result = "";
    return Result;
  };
  this.Frac = function (A) {
    return A % 1;
  };
  this.Odd = function (A) {
    return A&1 != 0;
  };
  this.Random = function (Range) {
    return Math.floor(Math.random()*Range);
  };
  this.Sqr = function (A) {
    return A*A;
  };
  this.Sqr$1 = function (A) {
    return A*A;
  };
  this.Trunc = function (A) {
    if (!Math.trunc) {
      Math.trunc = function(v) {
        v = +v;
        if (!isFinite(v)) return v;
        return (v - v % 1) || (v < 0 ? -0 : v === 0 ? v : 0);
      };
    }
    $mod.Trunc = Math.trunc;
    return Math.trunc(A);
  };
  this.DefaultTextLineBreakStyle = $mod.TTextLineBreakStyle.tlbsLF;
  this.Int = function (A) {
    var Result = 0.0;
    Result = $mod.Trunc(A);
    return Result;
  };
  this.Copy = function (S, Index, Size) {
    if (Index<1) Index = 1;
    return (Size>0) ? S.substring(Index-1,Index+Size-1) : "";
  };
  this.Copy$1 = function (S, Index) {
    if (Index<1) Index = 1;
    return S.substr(Index-1);
  };
  this.Delete = function (S, Index, Size) {
    var h = "";
    if ((Index < 1) || (Index > S.get().length) || (Size <= 0)) return;
    h = S.get();
    S.set($mod.Copy(h,1,Index - 1) + $mod.Copy$1(h,Index + Size));
  };
  this.Pos = function (Search, InString) {
    return InString.indexOf(Search)+1;
  };
  this.Pos$1 = function (Search, InString, StartAt) {
    return InString.indexOf(Search,StartAt-1)+1;
  };
  this.Insert = function (Insertion, Target, Index) {
    var t = "";
    if (Insertion === "") return;
    t = Target.get();
    if (Index < 1) {
      Target.set(Insertion + t)}
     else if (Index > t.length) {
      Target.set(t + Insertion)}
     else Target.set($mod.Copy(t,1,Index - 1) + Insertion + $mod.Copy(t,Index,t.length));
  };
  this.upcase = function (c) {
    return c.toUpperCase();
  };
  this.val = function (S, NI, Code) {
    NI.set($impl.valint(S,-4503599627370496,4503599627370495,Code));
  };
  this.val$1 = function (S, NI, Code) {
    var x = 0.0;
    x = Number(S);
    if (isNaN(x) || (x !== $mod.Int(x)) || (x < 0)) {
      Code.set(1)}
     else {
      Code.set(0);
      NI.set($mod.Trunc(x));
    };
  };
  this.val$2 = function (S, SI, Code) {
    SI.set($impl.valint(S,-128,127,Code));
  };
  this.val$3 = function (S, B, Code) {
    B.set($impl.valint(S,0,255,Code));
  };
  this.val$4 = function (S, SI, Code) {
    SI.set($impl.valint(S,-32768,32767,Code));
  };
  this.val$5 = function (S, W, Code) {
    W.set($impl.valint(S,0,65535,Code));
  };
  this.val$6 = function (S, I, Code) {
    I.set($impl.valint(S,-2147483648,2147483647,Code));
  };
  this.val$7 = function (S, C, Code) {
    C.set($impl.valint(S,0,4294967295,Code));
  };
  this.val$8 = function (S, d, Code) {
    var x = 0.0;
    x = Number(S);
    if (isNaN(x)) {
      Code.set(1)}
     else {
      Code.set(0);
      d.set(x);
    };
  };
  this.val$9 = function (S, b, Code) {
    if ($impl.SameText(S,"true")) {
      Code.set(0);
      b.set(true);
    } else if ($impl.SameText(S,"false")) {
      Code.set(0);
      b.set(false);
    } else Code.set(1);
  };
  this.StringOfChar = function (c, l) {
    var Result = "";
    var i = 0;
    if ((l>0) && c.repeat) return c.repeat(l);
    Result = "";
    for (var $l1 = 1, $end2 = l; $l1 <= $end2; $l1++) {
      i = $l1;
      Result = Result + c;
    };
    return Result;
  };
  this.Write = function () {
    var i = 0;
    for (var $l1 = 0, $end2 = rtl.length(arguments) - 1; $l1 <= $end2; $l1++) {
      i = $l1;
      if ($impl.WriteCallBack != null) {
        $impl.WriteCallBack(arguments[i],false)}
       else $impl.WriteBuf = $impl.WriteBuf + ("" + arguments[i]);
    };
  };
  this.Writeln = function () {
    var i = 0;
    var l = 0;
    var s = "";
    l = rtl.length(arguments) - 1;
    if ($impl.WriteCallBack != null) {
      for (var $l1 = 0, $end2 = l; $l1 <= $end2; $l1++) {
        i = $l1;
        $impl.WriteCallBack(arguments[i],i === l);
      };
    } else {
      s = $impl.WriteBuf;
      for (var $l3 = 0, $end4 = l; $l3 <= $end4; $l3++) {
        i = $l3;
        s = s + ("" + arguments[i]);
      };
      console.log(s);
      $impl.WriteBuf = "";
    };
  };
  $mod.$rtti.$ProcVar("TConsoleHandler",{procsig: rtl.newTIProcSig([["S",rtl.jsvalue],["NewLine",rtl.boolean]])});
  this.SetWriteCallBack = function (H) {
    var Result = null;
    Result = $impl.WriteCallBack;
    $impl.WriteCallBack = H;
    return Result;
  };
  this.Assigned = function (V) {
    return (V!=undefined) && (V!=null) && (!rtl.isArray(V) || (V.length > 0));
  };
  this.StrictEqual = function (A, B) {
    return A === B;
  };
  this.StrictInequal = function (A, B) {
    return A !== B;
  };
  $mod.$init = function () {
    rtl.exitcode = 0;
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.SameText = function (s1, s2) {
    return s1.toLowerCase() == s2.toLowerCase();
  };
  $impl.WriteBuf = "";
  $impl.WriteCallBack = null;
  $impl.valint = function (S, MinVal, MaxVal, Code) {
    var Result = 0;
    var x = 0.0;
    x = Number(S);
    if (isNaN(x)) {
      var $tmp1 = $mod.Copy(S,1,1);
      if ($tmp1 === "$") {
        x = Number("0x" + $mod.Copy$1(S,2))}
       else if ($tmp1 === "&") {
        x = Number("0o" + $mod.Copy$1(S,2))}
       else if ($tmp1 === "%") {
        x = Number("0b" + $mod.Copy$1(S,2))}
       else {
        Code.set(1);
        return Result;
      };
    };
    if (isNaN(x) || (x !== $mod.Int(x))) {
      Code.set(1)}
     else if ((x < MinVal) || (x > MaxVal)) {
      Code.set(2)}
     else {
      Result = $mod.Trunc(x);
      Code.set(0);
    };
    return Result;
  };
});
rtl.module("RTLConsts",["System"],function () {
  "use strict";
  var $mod = this;
  this.SArgumentMissing = 'Missing argument in format "%s"';
  this.SInvalidFormat = 'Invalid format specifier : "%s"';
  this.SInvalidArgIndex = 'Invalid argument index in format: "%s"';
  this.SListCapacityError = "List capacity (%s) exceeded.";
  this.SListCountError = "List count (%s) out of bounds.";
  this.SListIndexError = "List index (%s) out of bounds";
  this.SSortedListError = "Operation not allowed on sorted list";
  this.SDuplicateString = "String list does not allow duplicates";
  this.SErrFindNeedsSortedList = "Cannot use find on unsorted list";
  this.SInvalidName = 'Invalid component name: "%s"';
  this.SInvalidBoolean = '"%s" is not a valid boolean.';
  this.SDuplicateName = 'Duplicate component name: "%s"';
  this.SErrInvalidDate = 'Invalid date: "%s"';
  this.SErrInvalidTimeFormat = 'Invalid time format: "%s"';
  this.SInvalidDateFormat = 'Invalid date format: "%s"';
  this.SCantReadPropertyS = 'Cannot read property "%s"';
  this.SCantWritePropertyS = 'Cannot write property "%s"';
  this.SErrPropertyNotFound = 'Unknown property: "%s"';
  this.SIndexedPropertyNeedsParams = 'Indexed property "%s" needs parameters';
  this.SErrInvalidInteger = 'Invalid integer value: "%s"';
  this.SErrInvalidFloat = 'Invalid floating-point value: "%s"';
  this.SInvalidDateTime = "Invalid date-time value: %s";
  this.SInvalidCurrency = "Invalid currency value: %s";
  this.SErrInvalidDayOfWeek = "%d is not a valid day of the week";
  this.SErrInvalidTimeStamp = 'Invalid date\/timestamp : "%s"';
  this.SErrInvalidDateWeek = "%d %d %d is not a valid dateweek";
  this.SErrInvalidDayOfYear = "Year %d does not have a day number %d";
  this.SErrInvalidDateMonthWeek = "Year %d, month %d, Week %d and day %d is not a valid date.";
  this.SErrInvalidDayOfWeekInMonth = "Year %d Month %d NDow %d DOW %d is not a valid date";
  this.SInvalidJulianDate = "%f Julian cannot be represented as a DateTime";
  this.SErrInvalidHourMinuteSecMsec = "%d:%d:%d.%d is not a valid time specification";
  this.SInvalidGUID = '"%s" is not a valid GUID value';
});
rtl.module("Types",["System"],function () {
  "use strict";
  var $mod = this;
  this.TDirection = {"0": "FromBeginning", FromBeginning: 0, "1": "FromEnd", FromEnd: 1};
  $mod.$rtti.$Enum("TDirection",{minvalue: 0, maxvalue: 1, ordtype: 1, enumtype: this.TDirection});
  $mod.$rtti.$DynArray("TBooleanDynArray",{eltype: rtl.boolean});
  $mod.$rtti.$DynArray("TIntegerDynArray",{eltype: rtl.longint});
  $mod.$rtti.$DynArray("TNativeIntDynArray",{eltype: rtl.nativeint});
  $mod.$rtti.$DynArray("TStringDynArray",{eltype: rtl.string});
  $mod.$rtti.$DynArray("TDoubleDynArray",{eltype: rtl.double});
  $mod.$rtti.$DynArray("TJSValueDynArray",{eltype: rtl.jsvalue});
  this.TDuplicates = {"0": "dupIgnore", dupIgnore: 0, "1": "dupAccept", dupAccept: 1, "2": "dupError", dupError: 2};
  $mod.$rtti.$Enum("TDuplicates",{minvalue: 0, maxvalue: 2, ordtype: 1, enumtype: this.TDuplicates});
  $mod.$rtti.$MethodVar("TListCallback",{procsig: rtl.newTIProcSig([["data",rtl.jsvalue],["arg",rtl.jsvalue]]), methodkind: 0});
  $mod.$rtti.$ProcVar("TListStaticCallback",{procsig: rtl.newTIProcSig([["data",rtl.jsvalue],["arg",rtl.jsvalue]])});
  rtl.recNewT($mod,"TSize",function () {
    this.cx = 0;
    this.cy = 0;
    this.$eq = function (b) {
      return (this.cx === b.cx) && (this.cy === b.cy);
    };
    this.$assign = function (s) {
      this.cx = s.cx;
      this.cy = s.cy;
      return this;
    };
    var $r = $mod.$rtti.$Record("TSize",{});
    $r.addField("cx",rtl.longint);
    $r.addField("cy",rtl.longint);
  });
  rtl.recNewT($mod,"TPoint",function () {
    this.x = 0;
    this.y = 0;
    this.$eq = function (b) {
      return (this.x === b.x) && (this.y === b.y);
    };
    this.$assign = function (s) {
      this.x = s.x;
      this.y = s.y;
      return this;
    };
    var $r = $mod.$rtti.$Record("TPoint",{});
    $r.addField("x",rtl.longint);
    $r.addField("y",rtl.longint);
  });
  rtl.recNewT($mod,"TRect",function () {
    this.Left = 0;
    this.Top = 0;
    this.Right = 0;
    this.Bottom = 0;
    this.$eq = function (b) {
      return (this.Left === b.Left) && (this.Top === b.Top) && (this.Right === b.Right) && (this.Bottom === b.Bottom);
    };
    this.$assign = function (s) {
      this.Left = s.Left;
      this.Top = s.Top;
      this.Right = s.Right;
      this.Bottom = s.Bottom;
      return this;
    };
    var $r = $mod.$rtti.$Record("TRect",{});
    $r.addField("Left",rtl.longint);
    $r.addField("Top",rtl.longint);
    $r.addField("Right",rtl.longint);
    $r.addField("Bottom",rtl.longint);
  });
  this.EqualRect = function (r1, r2) {
    var Result = false;
    Result = (r1.Left === r2.Left) && (r1.Right === r2.Right) && (r1.Top === r2.Top) && (r1.Bottom === r2.Bottom);
    return Result;
  };
  this.Rect = function (Left, Top, Right, Bottom) {
    var Result = $mod.TRect.$new();
    Result.Left = Left;
    Result.Top = Top;
    Result.Right = Right;
    Result.Bottom = Bottom;
    return Result;
  };
  this.Bounds = function (ALeft, ATop, AWidth, AHeight) {
    var Result = $mod.TRect.$new();
    Result.Left = ALeft;
    Result.Top = ATop;
    Result.Right = ALeft + AWidth;
    Result.Bottom = ATop + AHeight;
    return Result;
  };
  this.Point = function (x, y) {
    var Result = $mod.TPoint.$new();
    Result.x = x;
    Result.y = y;
    return Result;
  };
  this.PtInRect = function (aRect, p) {
    var Result = false;
    Result = (p.y >= aRect.Top) && (p.y < aRect.Bottom) && (p.x >= aRect.Left) && (p.x < aRect.Right);
    return Result;
  };
  this.IntersectRect = function (aRect, R1, R2) {
    var Result = false;
    var lRect = $mod.TRect.$new();
    lRect.$assign(R1);
    if (R2.Left > R1.Left) lRect.Left = R2.Left;
    if (R2.Top > R1.Top) lRect.Top = R2.Top;
    if (R2.Right < R1.Right) lRect.Right = R2.Right;
    if (R2.Bottom < R1.Bottom) lRect.Bottom = R2.Bottom;
    if ($mod.IsRectEmpty(lRect)) {
      aRect.$assign($mod.Rect(0,0,0,0));
      Result = false;
    } else {
      Result = true;
      aRect.$assign(lRect);
    };
    return Result;
  };
  this.UnionRect = function (aRect, R1, R2) {
    var Result = false;
    var lRect = $mod.TRect.$new();
    lRect.$assign(R1);
    if (R2.Left < R1.Left) lRect.Left = R2.Left;
    if (R2.Top < R1.Top) lRect.Top = R2.Top;
    if (R2.Right > R1.Right) lRect.Right = R2.Right;
    if (R2.Bottom > R1.Bottom) lRect.Bottom = R2.Bottom;
    if ($mod.IsRectEmpty(lRect)) {
      aRect.$assign($mod.Rect(0,0,0,0));
      Result = false;
    } else {
      aRect.$assign(lRect);
      Result = true;
    };
    return Result;
  };
  this.IsRectEmpty = function (aRect) {
    var Result = false;
    Result = (aRect.Right <= aRect.Left) || (aRect.Bottom <= aRect.Top);
    return Result;
  };
  this.OffsetRect = function (aRect, DX, DY) {
    var Result = false;
    aRect.Left += DX;
    aRect.Top += DY;
    aRect.Right += DX;
    aRect.Bottom += DY;
    Result = true;
    return Result;
  };
  this.CenterPoint = function (aRect) {
    var Result = $mod.TPoint.$new();
    function Avg(a, b) {
      var Result = 0;
      if (a < b) {
        Result = a + ((b - a) >>> 1)}
       else Result = b + ((a - b) >>> 1);
      return Result;
    };
    Result.x = Avg(aRect.Left,aRect.Right);
    Result.y = Avg(aRect.Top,aRect.Bottom);
    return Result;
  };
  this.InflateRect = function (aRect, dx, dy) {
    var Result = false;
    aRect.Left -= dx;
    aRect.Top -= dy;
    aRect.Right += dx;
    aRect.Bottom += dy;
    Result = true;
    return Result;
  };
  this.Size = function (AWidth, AHeight) {
    var Result = $mod.TSize.$new();
    Result.cx = AWidth;
    Result.cy = AHeight;
    return Result;
  };
  this.Size$1 = function (aRect) {
    var Result = $mod.TSize.$new();
    Result.cx = aRect.Right - aRect.Left;
    Result.cy = aRect.Bottom - aRect.Top;
    return Result;
  };
});
rtl.module("JS",["System","Types"],function () {
  "use strict";
  var $mod = this;
  rtl.createClass($mod,"EJS",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.FMessage = "";
    };
    this.Create$1 = function (Msg) {
      this.FMessage = Msg;
      return this;
    };
  });
  $mod.$rtti.$DynArray("TJSObjectDynArray",{eltype: $mod.$rtti["TJSObject"]});
  $mod.$rtti.$DynArray("TJSObjectDynArrayArray",{eltype: $mod.$rtti["TJSObjectDynArray"]});
  $mod.$rtti.$DynArray("TJSStringDynArray",{eltype: rtl.string});
  rtl.recNewT($mod,"TLocaleCompareOptions",function () {
    this.localematched = "";
    this.usage = "";
    this.sensitivity = "";
    this.ignorePunctuation = false;
    this.numeric = false;
    this.caseFirst = "";
    this.$eq = function (b) {
      return (this.localematched === b.localematched) && (this.usage === b.usage) && (this.sensitivity === b.sensitivity) && (this.ignorePunctuation === b.ignorePunctuation) && (this.numeric === b.numeric) && (this.caseFirst === b.caseFirst);
    };
    this.$assign = function (s) {
      this.localematched = s.localematched;
      this.usage = s.usage;
      this.sensitivity = s.sensitivity;
      this.ignorePunctuation = s.ignorePunctuation;
      this.numeric = s.numeric;
      this.caseFirst = s.caseFirst;
      return this;
    };
    var $r = $mod.$rtti.$Record("TLocaleCompareOptions",{});
    $r.addField("localematched",rtl.string);
    $r.addField("usage",rtl.string);
    $r.addField("sensitivity",rtl.string);
    $r.addField("ignorePunctuation",rtl.boolean);
    $r.addField("numeric",rtl.boolean);
    $r.addField("caseFirst",rtl.string);
  });
  $mod.$rtti.$ProcVar("TReplaceCallBack",{procsig: rtl.newTIProcSig(null,rtl.string,2)});
  $mod.$rtti.$RefToProcVar("TJSArrayEvent",{procsig: rtl.newTIProcSig([["element",rtl.jsvalue],["index",rtl.nativeint],["anArray",$mod.$rtti["TJSArray"]]],rtl.boolean)});
  $mod.$rtti.$RefToProcVar("TJSArrayMapEvent",{procsig: rtl.newTIProcSig([["element",rtl.jsvalue],["index",rtl.nativeint],["anArray",$mod.$rtti["TJSArray"]]],rtl.jsvalue)});
  $mod.$rtti.$RefToProcVar("TJSArrayReduceEvent",{procsig: rtl.newTIProcSig([["accumulator",rtl.jsvalue],["currentValue",rtl.jsvalue],["currentIndex",rtl.nativeint],["anArray",$mod.$rtti["TJSArray"]]],rtl.jsvalue)});
  $mod.$rtti.$RefToProcVar("TJSArrayCompareEvent",{procsig: rtl.newTIProcSig([["a",rtl.jsvalue],["b",rtl.jsvalue]],rtl.nativeint)});
  $mod.$rtti.$ProcVar("TJSTypedArrayCallBack",{procsig: rtl.newTIProcSig([["element",rtl.jsvalue],["index",rtl.nativeint],["anArray",$mod.$rtti["TJSTypedArray"]]],rtl.boolean)});
  $mod.$rtti.$MethodVar("TJSTypedArrayEvent",{procsig: rtl.newTIProcSig([["element",rtl.jsvalue],["index",rtl.nativeint],["anArray",$mod.$rtti["TJSTypedArray"]]],rtl.boolean), methodkind: 1});
  $mod.$rtti.$ProcVar("TJSTypedArrayMapCallBack",{procsig: rtl.newTIProcSig([["element",rtl.jsvalue],["index",rtl.nativeint],["anArray",$mod.$rtti["TJSTypedArray"]]],rtl.jsvalue)});
  $mod.$rtti.$MethodVar("TJSTypedArrayMapEvent",{procsig: rtl.newTIProcSig([["element",rtl.jsvalue],["index",rtl.nativeint],["anArray",$mod.$rtti["TJSTypedArray"]]],rtl.jsvalue), methodkind: 1});
  $mod.$rtti.$ProcVar("TJSTypedArrayReduceCallBack",{procsig: rtl.newTIProcSig([["accumulator",rtl.jsvalue],["currentValue",rtl.jsvalue],["currentIndex",rtl.nativeint],["anArray",$mod.$rtti["TJSTypedArray"]]],rtl.jsvalue)});
  $mod.$rtti.$ProcVar("TJSTypedArrayCompareCallBack",{procsig: rtl.newTIProcSig([["a",rtl.jsvalue],["b",rtl.jsvalue]],rtl.nativeint)});
  $mod.$rtti.$RefToProcVar("TJSPromiseResolver",{procsig: rtl.newTIProcSig([["aValue",rtl.jsvalue]],rtl.jsvalue)});
  $mod.$rtti.$RefToProcVar("TJSPromiseExecutor",{procsig: rtl.newTIProcSig([["resolve",$mod.$rtti["TJSPromiseResolver"]],["reject",$mod.$rtti["TJSPromiseResolver"]]])});
  $mod.$rtti.$RefToProcVar("TJSPromiseFinallyHandler",{procsig: rtl.newTIProcSig(null)});
  this.New = function (aElements) {
    var Result = null;
    var L = 0;
    var I = 0;
    var S = "";
    L = rtl.length(aElements);
    if ((L % 2) === 1) throw $mod.EJS.$create("Create$1",["Number of arguments must be even"]);
    I = 0;
    while (I < L) {
      if (!rtl.isString(aElements[I])) {
        S = String(I);
        throw $mod.EJS.$create("Create$1",["Argument " + S + " must be a string."]);
      };
      I += 2;
    };
    I = 0;
    Result = new Object();
    while (I < L) {
      S = "" + aElements[I];
      Result[S] = aElements[I + 1];
      I += 2;
    };
    return Result;
  };
  this.JSDelete = function (Obj, PropName) {
    return delete Obj[PropName];
  };
  this.hasValue = function (v) {
    if(v){ return true; } else { return false; };
  };
  this.isBoolean = function (v) {
    return typeof(v) == 'boolean';
  };
  this.isCallback = function (v) {
    return rtl.isObject(v) && rtl.isObject(v.scope) && (rtl.isString(v.fn) || rtl.isFunction(v.fn));
  };
  this.isChar = function (v) {
    return (typeof(v)!="string") && (v.length==1);
  };
  this.isClass = function (v) {
    return (typeof(v)=="object") && (v!=null) && (v.$class == v);
  };
  this.isClassInstance = function (v) {
    return (typeof(v)=="object") && (v!=null) && (v.$class == Object.getPrototypeOf(v));
  };
  this.isInteger = function (v) {
    return Math.floor(v)===v;
  };
  this.isNull = function (v) {
    return v === null;
  };
  this.isRecord = function (v) {
    return (typeof(v)=="function") && (typeof(v.$create) == "function");
  };
  this.isUndefined = function (v) {
    return v == undefined;
  };
  this.isDefined = function (v) {
    return !(v == undefined);
  };
  this.isUTF16Char = function (v) {
    if (typeof(v)!="string") return false;
    if ((v.length==0) || (v.length>2)) return false;
    var code = v.charCodeAt(0);
    if (code < 0xD800){
      if (v.length == 1) return true;
    } else if (code <= 0xDBFF){
      if (v.length==2){
        code = v.charCodeAt(1);
        if (code >= 0xDC00 && code <= 0xDFFF) return true;
      };
    };
    return false;
  };
  this.jsInstanceOf = function (aFunction, aFunctionWithPrototype) {
    return aFunction instanceof aFunctionWithPrototype;
  };
  this.toNumber = function (v) {
    return v-0;
  };
  this.toInteger = function (v) {
    var Result = 0;
    if ($mod.isInteger(v)) {
      Result = Math.floor(v)}
     else Result = 0;
    return Result;
  };
  this.toObject = function (Value) {
    var Result = null;
    if (rtl.isObject(Value)) {
      Result = rtl.getObject(Value)}
     else Result = null;
    return Result;
  };
  this.toArray = function (Value) {
    var Result = null;
    if (rtl.isArray(Value)) {
      Result = rtl.getObject(Value)}
     else Result = null;
    return Result;
  };
  this.toBoolean = function (Value) {
    var Result = false;
    if ($mod.isBoolean(Value)) {
      Result = !(Value == false)}
     else Result = false;
    return Result;
  };
  this.ToString = function (Value) {
    var Result = "";
    if (rtl.isString(Value)) {
      Result = "" + Value}
     else Result = "";
    return Result;
  };
  this.TJSValueType = {"0": "jvtNull", jvtNull: 0, "1": "jvtBoolean", jvtBoolean: 1, "2": "jvtInteger", jvtInteger: 2, "3": "jvtFloat", jvtFloat: 3, "4": "jvtString", jvtString: 4, "5": "jvtObject", jvtObject: 5, "6": "jvtArray", jvtArray: 6};
  $mod.$rtti.$Enum("TJSValueType",{minvalue: 0, maxvalue: 6, ordtype: 1, enumtype: this.TJSValueType});
  this.GetValueType = function (JS) {
    var Result = 0;
    var t = "";
    if ($mod.isNull(JS)) {
      Result = $mod.TJSValueType.jvtNull}
     else {
      t = typeof(JS);
      if (t === "string") {
        Result = $mod.TJSValueType.jvtString}
       else if (t === "boolean") {
        Result = $mod.TJSValueType.jvtBoolean}
       else if (t === "object") {
        if (rtl.isArray(JS)) {
          Result = $mod.TJSValueType.jvtArray}
         else Result = $mod.TJSValueType.jvtObject;
      } else if (t === "number") if ($mod.isInteger(JS)) {
        Result = $mod.TJSValueType.jvtInteger}
       else Result = $mod.TJSValueType.jvtFloat;
    };
    return Result;
  };
});
rtl.module("SysUtils",["System","RTLConsts","JS"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.FreeAndNil = function (Obj) {
    var o = null;
    o = Obj.get();
    if (o === null) return;
    Obj.set(null);
    o.$destroy("Destroy");
  };
  $mod.$rtti.$ProcVar("TProcedure",{procsig: rtl.newTIProcSig(null)});
  this.FloatRecDigits = 19;
  rtl.recNewT($mod,"TFloatRec",function () {
    this.Exponent = 0;
    this.Negative = false;
    this.$new = function () {
      var r = Object.create(this);
      r.Digits = rtl.arraySetLength(null,"",19);
      return r;
    };
    this.$eq = function (b) {
      return (this.Exponent === b.Exponent) && (this.Negative === b.Negative) && rtl.arrayEq(this.Digits,b.Digits);
    };
    this.$assign = function (s) {
      this.Exponent = s.Exponent;
      this.Negative = s.Negative;
      this.Digits = s.Digits.slice(0);
      return this;
    };
    var $r = $mod.$rtti.$Record("TFloatRec",{});
    $r.addField("Exponent",rtl.longint);
    $r.addField("Negative",rtl.boolean);
    $mod.$rtti.$StaticArray("TFloatRec.Digits$a",{dims: [19], eltype: rtl.char});
    $r.addField("Digits",$mod.$rtti["TFloatRec.Digits$a"]);
  });
  this.TEndian = {"0": "Little", Little: 0, "1": "Big", Big: 1};
  $mod.$rtti.$Enum("TEndian",{minvalue: 0, maxvalue: 1, ordtype: 1, enumtype: this.TEndian});
  $mod.$rtti.$StaticArray("TByteArray",{dims: [32768], eltype: rtl.byte});
  $mod.$rtti.$StaticArray("TWordArray",{dims: [16384], eltype: rtl.word});
  $mod.$rtti.$DynArray("TBytes",{eltype: rtl.byte});
  $mod.$rtti.$DynArray("TStringArray",{eltype: rtl.string});
  $mod.$rtti.$StaticArray("TMonthNameArray",{dims: [12], eltype: rtl.string});
  $mod.$rtti.$StaticArray("TDayTable",{dims: [12], eltype: rtl.word});
  $mod.$rtti.$StaticArray("TWeekNameArray",{dims: [7], eltype: rtl.string});
  $mod.$rtti.$StaticArray("TDayNames",{dims: [7], eltype: rtl.string});
  rtl.createClass($mod,"Exception",pas.System.TObject,function () {
    this.$init = function () {
      pas.System.TObject.$init.call(this);
      this.fMessage = "";
      this.fHelpContext = 0;
    };
    this.Create$1 = function (Msg) {
      this.fMessage = Msg;
      return this;
    };
    this.CreateFmt = function (Msg, Args) {
      this.Create$1($mod.Format(Msg,Args));
      return this;
    };
    this.CreateHelp = function (Msg, AHelpContext) {
      this.Create$1(Msg);
      this.fHelpContext = AHelpContext;
      return this;
    };
    this.CreateFmtHelp = function (Msg, Args, AHelpContext) {
      this.Create$1($mod.Format(Msg,Args));
      this.fHelpContext = AHelpContext;
      return this;
    };
    this.ToString = function () {
      var Result = "";
      Result = this.$classname + ": " + this.fMessage;
      return Result;
    };
  });
  $mod.$rtti.$ClassRef("ExceptClass",{instancetype: $mod.$rtti["Exception"]});
  rtl.createClass($mod,"EExternal",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EMathError",$mod.EExternal,function () {
  });
  rtl.createClass($mod,"EInvalidOp",$mod.EMathError,function () {
  });
  rtl.createClass($mod,"EZeroDivide",$mod.EMathError,function () {
  });
  rtl.createClass($mod,"EOverflow",$mod.EMathError,function () {
  });
  rtl.createClass($mod,"EUnderflow",$mod.EMathError,function () {
  });
  rtl.createClass($mod,"EAbort",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EInvalidCast",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EAssertionFailed",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EObjectCheck",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EConvertError",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EFormatError",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EIntError",$mod.EExternal,function () {
  });
  rtl.createClass($mod,"EDivByZero",$mod.EIntError,function () {
  });
  rtl.createClass($mod,"ERangeError",$mod.EIntError,function () {
  });
  rtl.createClass($mod,"EIntOverflow",$mod.EIntError,function () {
  });
  rtl.createClass($mod,"EInOutError",$mod.Exception,function () {
    this.$init = function () {
      $mod.Exception.$init.call(this);
      this.ErrorCode = 0;
    };
  });
  rtl.createClass($mod,"EHeapMemoryError",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EExternalException",$mod.EExternal,function () {
  });
  rtl.createClass($mod,"EInvalidPointer",$mod.EHeapMemoryError,function () {
  });
  rtl.createClass($mod,"EOutOfMemory",$mod.EHeapMemoryError,function () {
  });
  rtl.createClass($mod,"EVariantError",$mod.Exception,function () {
    this.$init = function () {
      $mod.Exception.$init.call(this);
      this.ErrCode = 0;
    };
    this.CreateCode = function (Code) {
      this.ErrCode = Code;
      return this;
    };
  });
  rtl.createClass($mod,"EAccessViolation",$mod.EExternal,function () {
  });
  rtl.createClass($mod,"EBusError",$mod.EAccessViolation,function () {
  });
  rtl.createClass($mod,"EPrivilege",$mod.EExternal,function () {
  });
  rtl.createClass($mod,"EStackOverflow",$mod.EExternal,function () {
  });
  rtl.createClass($mod,"EControlC",$mod.EExternal,function () {
  });
  rtl.createClass($mod,"EAbstractError",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EPropReadOnly",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EPropWriteOnly",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EIntfCastError",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EInvalidContainer",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EInvalidInsert",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EPackageError",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EOSError",$mod.Exception,function () {
    this.$init = function () {
      $mod.Exception.$init.call(this);
      this.ErrorCode = 0;
    };
  });
  rtl.createClass($mod,"ESafecallException",$mod.Exception,function () {
  });
  rtl.createClass($mod,"ENoThreadSupport",$mod.Exception,function () {
  });
  rtl.createClass($mod,"ENoWideStringSupport",$mod.Exception,function () {
  });
  rtl.createClass($mod,"ENotImplemented",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EArgumentException",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EArgumentOutOfRangeException",$mod.EArgumentException,function () {
  });
  rtl.createClass($mod,"EArgumentNilException",$mod.EArgumentException,function () {
  });
  rtl.createClass($mod,"EPathTooLongException",$mod.Exception,function () {
  });
  rtl.createClass($mod,"ENotSupportedException",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EDirectoryNotFoundException",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EFileNotFoundException",$mod.Exception,function () {
  });
  rtl.createClass($mod,"EPathNotFoundException",$mod.Exception,function () {
  });
  rtl.createClass($mod,"ENoConstructException",$mod.Exception,function () {
  });
  this.EmptyStr = "";
  this.EmptyWideStr = "";
  this.HexDisplayPrefix = "$";
  this.LeadBytes = {};
  this.CharInSet = function (Ch, CSet) {
    var Result = false;
    var I = 0;
    Result = false;
    I = rtl.length(CSet) - 1;
    while (!Result && (I >= 0)) {
      Result = Ch === CSet[I];
      I -= 1;
    };
    return Result;
  };
  this.LeftStr = function (S, Count) {
    return (Count>0) ? S.substr(0,Count) : "";
  };
  this.RightStr = function (S, Count) {
    var l = S.length;
    return (Count<1) ? "" : ( Count>=l ? S : S.substr(l-Count));
  };
  this.Trim = function (S) {
    return S.trim();
  };
  this.TrimLeft = function (S) {
    return S.replace(/^[\s\uFEFF\xA0\x00-\x1f]+/,'');
  };
  this.TrimRight = function (S) {
    return S.replace(/[\s\uFEFF\xA0\x00-\x1f]+$/,'');
  };
  this.UpperCase = function (s) {
    return s.toUpperCase();
  };
  this.LowerCase = function (s) {
    return s.toLowerCase();
  };
  this.CompareStr = function (s1, s2) {
    var l1 = s1.length;
    var l2 = s2.length;
    if (l1<=l2){
      var s = s2.substr(0,l1);
      if (s1<s){ return -1;
      } else if (s1>s){ return 1;
      } else { return l1<l2 ? -1 : 0; };
    } else {
      var s = s1.substr(0,l2);
      if (s<s2){ return -1;
      } else { return 1; };
    };
  };
  this.SameStr = function (s1, s2) {
    return s1 == s2;
  };
  this.CompareText = function (s1, s2) {
    var l1 = s1.toLowerCase();
    var l2 = s2.toLowerCase();
    if (l1>l2){ return 1;
    } else if (l1<l2){ return -1;
    } else { return 0; };
  };
  this.SameText = function (s1, s2) {
    return s1.toLowerCase() == s2.toLowerCase();
  };
  this.AnsiCompareText = function (s1, s2) {
    return s1.localeCompare(s2);
  };
  this.AnsiSameText = function (s1, s2) {
    return s1.localeCompare(s2) == 0;
  };
  this.AnsiCompareStr = function (s1, s2) {
    var Result = 0;
    Result = $mod.CompareText(s1,s2);
    return Result;
  };
  this.AppendStr = function (Dest, S) {
    Dest.set(Dest.get() + S);
  };
  this.Format = function (Fmt, Args) {
    var Result = "";
    var ChPos = 0;
    var OldPos = 0;
    var ArgPos = 0;
    var DoArg = 0;
    var Len = 0;
    var Hs = "";
    var ToAdd = "";
    var Index = 0;
    var Width = 0;
    var Prec = 0;
    var Left = false;
    var Fchar = "";
    var vq = 0;
    function ReadFormat() {
      var Result = "";
      var Value = 0;
      function ReadInteger() {
        var Code = 0;
        var ArgN = 0;
        if (Value !== -1) return;
        OldPos = ChPos;
        while ((ChPos <= Len) && (Fmt.charAt(ChPos - 1) <= "9") && (Fmt.charAt(ChPos - 1) >= "0")) ChPos += 1;
        if (ChPos > Len) $impl.DoFormatError(1,Fmt);
        if (Fmt.charAt(ChPos - 1) === "*") {
          if (Index === -1) {
            ArgN = ArgPos}
           else {
            ArgN = Index;
            Index += 1;
          };
          if ((ChPos > OldPos) || (ArgN > (rtl.length(Args) - 1))) $impl.DoFormatError(1,Fmt);
          ArgPos = ArgN + 1;
          if (rtl.isNumber(Args[ArgN]) && pas.JS.isInteger(Args[ArgN])) {
            Value = Math.floor(Args[ArgN])}
           else $impl.DoFormatError(1,Fmt);
          ChPos += 1;
        } else {
          if (OldPos < ChPos) {
            pas.System.val(pas.System.Copy(Fmt,OldPos,ChPos - OldPos),{get: function () {
                return Value;
              }, set: function (v) {
                Value = v;
              }},{get: function () {
                return Code;
              }, set: function (v) {
                Code = v;
              }});
            if (Code > 0) $impl.DoFormatError(1,Fmt);
          } else Value = -1;
        };
      };
      function ReadIndex() {
        if (Fmt.charAt(ChPos - 1) !== ":") {
          ReadInteger()}
         else Value = 0;
        if (Fmt.charAt(ChPos - 1) === ":") {
          if (Value === -1) $impl.DoFormatError(2,Fmt);
          Index = Value;
          Value = -1;
          ChPos += 1;
        };
      };
      function ReadLeft() {
        if (Fmt.charAt(ChPos - 1) === "-") {
          Left = true;
          ChPos += 1;
        } else Left = false;
      };
      function ReadWidth() {
        ReadInteger();
        if (Value !== -1) {
          Width = Value;
          Value = -1;
        };
      };
      function ReadPrec() {
        if (Fmt.charAt(ChPos - 1) === ".") {
          ChPos += 1;
          ReadInteger();
          if (Value === -1) Value = 0;
          Prec = Value;
        };
      };
      Index = -1;
      Width = -1;
      Prec = -1;
      Value = -1;
      ChPos += 1;
      if (Fmt.charAt(ChPos - 1) === "%") {
        Result = "%";
        return Result;
      };
      ReadIndex();
      ReadLeft();
      ReadWidth();
      ReadPrec();
      Result = pas.System.upcase(Fmt.charAt(ChPos - 1));
      return Result;
    };
    function Checkarg(AT, err) {
      var Result = false;
      Result = false;
      if (Index === -1) {
        DoArg = ArgPos}
       else DoArg = Index;
      ArgPos = DoArg + 1;
      if ((DoArg > (rtl.length(Args) - 1)) || (pas.JS.GetValueType(Args[DoArg]) !== AT)) {
        if (err) $impl.DoFormatError(3,Fmt);
        ArgPos -= 1;
        return Result;
      };
      Result = true;
      return Result;
    };
    Result = "";
    Len = Fmt.length;
    ChPos = 1;
    OldPos = 1;
    ArgPos = 0;
    while (ChPos <= Len) {
      while ((ChPos <= Len) && (Fmt.charAt(ChPos - 1) !== "%")) ChPos += 1;
      if (ChPos > OldPos) Result = Result + pas.System.Copy(Fmt,OldPos,ChPos - OldPos);
      if (ChPos < Len) {
        Fchar = ReadFormat();
        var $tmp1 = Fchar;
        if ($tmp1 === "D") {
          Checkarg(pas.JS.TJSValueType.jvtInteger,true);
          ToAdd = $mod.IntToStr(Math.floor(Args[DoArg]));
          Width = Math.abs(Width);
          Index = Prec - ToAdd.length;
          if (ToAdd.charAt(0) !== "-") {
            ToAdd = pas.System.StringOfChar("0",Index) + ToAdd}
           else pas.System.Insert(pas.System.StringOfChar("0",Index + 1),{get: function () {
              return ToAdd;
            }, set: function (v) {
              ToAdd = v;
            }},2);
        } else if ($tmp1 === "U") {
          Checkarg(pas.JS.TJSValueType.jvtInteger,true);
          if (Math.floor(Args[DoArg]) < 0) $impl.DoFormatError(3,Fmt);
          ToAdd = $mod.IntToStr(Math.floor(Args[DoArg]));
          Width = Math.abs(Width);
          Index = Prec - ToAdd.length;
          ToAdd = pas.System.StringOfChar("0",Index) + ToAdd;
        } else if ($tmp1 === "E") {
          if (Checkarg(pas.JS.TJSValueType.jvtFloat,false) || Checkarg(pas.JS.TJSValueType.jvtInteger,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),$mod.TFloatFormat.ffFixed,9999,Prec);
        } else if ($tmp1 === "F") {
          if (Checkarg(pas.JS.TJSValueType.jvtFloat,false) || Checkarg(pas.JS.TJSValueType.jvtInteger,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),$mod.TFloatFormat.ffFixed,9999,Prec);
        } else if ($tmp1 === "G") {
          if (Checkarg(pas.JS.TJSValueType.jvtFloat,false) || Checkarg(pas.JS.TJSValueType.jvtInteger,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),$mod.TFloatFormat.ffGeneral,Prec,3);
        } else if ($tmp1 === "N") {
          if (Checkarg(pas.JS.TJSValueType.jvtFloat,false) || Checkarg(pas.JS.TJSValueType.jvtInteger,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),$mod.TFloatFormat.ffNumber,9999,Prec);
        } else if ($tmp1 === "M") {
          if (Checkarg(pas.JS.TJSValueType.jvtFloat,false) || Checkarg(pas.JS.TJSValueType.jvtInteger,true)) ToAdd = $mod.FloatToStrF(rtl.getNumber(Args[DoArg]),$mod.TFloatFormat.ffCurrency,9999,Prec);
        } else if ($tmp1 === "S") {
          Checkarg(pas.JS.TJSValueType.jvtString,true);
          Hs = "" + Args[DoArg];
          Index = Hs.length;
          if ((Prec !== -1) && (Index > Prec)) Index = Prec;
          ToAdd = pas.System.Copy(Hs,1,Index);
        } else if ($tmp1 === "P") {
          Checkarg(pas.JS.TJSValueType.jvtInteger,true);
          ToAdd = $mod.IntToHex(Math.floor(Args[DoArg]),31);
        } else if ($tmp1 === "X") {
          Checkarg(pas.JS.TJSValueType.jvtInteger,true);
          vq = Math.floor(Args[DoArg]);
          Index = 31;
          if (Prec > Index) {
            ToAdd = $mod.IntToHex(vq,Index)}
           else {
            Index = 1;
            while (((1 << (Index * 4)) <= vq) && (Index < 16)) Index += 1;
            if (Index > Prec) Prec = Index;
            ToAdd = $mod.IntToHex(vq,Prec);
          };
        } else if ($tmp1 === "%") ToAdd = "%";
        if (Width !== -1) if (ToAdd.length < Width) if (!Left) {
          ToAdd = pas.System.StringOfChar(" ",Width - ToAdd.length) + ToAdd}
         else ToAdd = ToAdd + pas.System.StringOfChar(" ",Width - ToAdd.length);
        Result = Result + ToAdd;
      };
      ChPos += 1;
      OldPos = ChPos;
    };
    return Result;
  };
  this.BytesOf = function (AVal) {
    var Result = [];
    var I = 0;
    Result = rtl.arraySetLength(Result,0,AVal.length);
    for (var $l1 = 0, $end2 = AVal.length - 1; $l1 <= $end2; $l1++) {
      I = $l1;
      Result[I] = AVal.charCodeAt((I + 1) - 1);
    };
    return Result;
  };
  this.StringOf = function (ABytes) {
    var Result = "";
    var I = 0;
    Result = "";
    for (var $l1 = 0, $end2 = rtl.length(ABytes) - 1; $l1 <= $end2; $l1++) {
      I = $l1;
      Result = Result + String.fromCharCode(ABytes[I]);
    };
    return Result;
  };
  this.LocaleCompare = function (s1, s2, locales) {
    return s1.localeCompare(s2,locales) == 0;
  };
  this.NormalizeStr = function (S, Norm) {
    return S.normalize(Norm);
  };
  var Alpha = rtl.createSet(null,65,90,null,97,122,95);
  var AlphaNum = rtl.unionSet(Alpha,rtl.createSet(null,48,57));
  var Dot = ".";
  this.IsValidIdent = function (Ident, AllowDots, StrictDots) {
    var Result = false;
    var First = false;
    var I = 0;
    var Len = 0;
    Len = Ident.length;
    if (Len < 1) return false;
    First = true;
    Result = false;
    I = 1;
    while (I <= Len) {
      if (First) {
        if (!(Ident.charCodeAt(I - 1) in Alpha)) return Result;
        First = false;
      } else if (AllowDots && (Ident.charAt(I - 1) === Dot)) {
        if (StrictDots) {
          if (I >= Len) return Result;
          First = true;
        };
      } else if (!(Ident.charCodeAt(I - 1) in AlphaNum)) return Result;
      I = I + 1;
    };
    Result = true;
    return Result;
  };
  this.TStringReplaceFlag = {"0": "rfReplaceAll", rfReplaceAll: 0, "1": "rfIgnoreCase", rfIgnoreCase: 1};
  $mod.$rtti.$Enum("TStringReplaceFlag",{minvalue: 0, maxvalue: 1, ordtype: 1, enumtype: this.TStringReplaceFlag});
  $mod.$rtti.$Set("TStringReplaceFlags",{comptype: $mod.$rtti["TStringReplaceFlag"]});
  this.StringReplace = function (aOriginal, aSearch, aReplace, Flags) {
    var Result = "";
    var REFlags = "";
    var REString = "";
    REFlags = "";
    if ($mod.TStringReplaceFlag.rfReplaceAll in Flags) REFlags = "g";
    if ($mod.TStringReplaceFlag.rfIgnoreCase in Flags) REFlags = REFlags + "i";
    REString = aSearch.replace(new RegExp($impl.RESpecials,"g"),"\\$1");
    Result = aOriginal.replace(new RegExp(REString,REFlags),aReplace);
    return Result;
  };
  this.QuoteString = function (aOriginal, AQuote) {
    var Result = "";
    Result = AQuote + $mod.StringReplace(aOriginal,AQuote,AQuote + AQuote,rtl.createSet($mod.TStringReplaceFlag.rfReplaceAll)) + AQuote;
    return Result;
  };
  this.QuotedStr = function (s, QuoteChar) {
    var Result = "";
    Result = $mod.QuoteString(s,QuoteChar);
    return Result;
  };
  this.DeQuoteString = function (aQuoted, AQuote) {
    var Result = "";
    var i = 0;
    Result = aQuoted;
    if (Result.substr(0,1) !== AQuote) return Result;
    Result = Result.slice(1);
    i = 1;
    while (i <= Result.length) {
      if (Result.charAt(i - 1) === AQuote) {
        if ((i === Result.length) || (Result.charAt((i + 1) - 1) !== AQuote)) {
          Result = Result.slice(0,i - 1);
          return Result;
        } else Result = Result.slice(0,i - 1) + Result.slice(i);
      } else i += 1;
    };
    return Result;
  };
  this.IsDelimiter = function (Delimiters, S, Index) {
    var Result = false;
    Result = false;
    if ((Index > 0) && (Index <= S.length)) Result = pas.System.Pos(S.charAt(Index - 1),Delimiters) !== 0;
    return Result;
  };
  this.AdjustLineBreaks = function (S) {
    var Result = "";
    Result = $mod.AdjustLineBreaks$1(S,pas.System.DefaultTextLineBreakStyle);
    return Result;
  };
  this.AdjustLineBreaks$1 = function (S, Style) {
    var Result = "";
    var I = 0;
    var L = 0;
    var Res = "";
    function Add(C) {
      Res = Res + C;
    };
    I = 0;
    L = S.length;
    Result = "";
    while (I <= L) {
      var $tmp1 = S.charAt(I - 1);
      if ($tmp1 === "\n") {
        if (Style in rtl.createSet(pas.System.TTextLineBreakStyle.tlbsCRLF,pas.System.TTextLineBreakStyle.tlbsCR)) Add("\r");
        if (Style === pas.System.TTextLineBreakStyle.tlbsCRLF) Add("\n");
        I += 1;
      } else if ($tmp1 === "\r") {
        if (Style === pas.System.TTextLineBreakStyle.tlbsCRLF) Add("\r");
        Add("\n");
        I += 1;
        if (S.charAt(I - 1) === "\n") I += 1;
      } else {
        Add(S.charAt(I - 1));
        I += 1;
      };
    };
    Result = Res;
    return Result;
  };
  var Quotes = rtl.createSet(39,34);
  this.WrapText = function (Line, BreakStr, BreakChars, MaxCol) {
    var Result = "";
    var L = "";
    var C = "";
    var LQ = "";
    var BC = "";
    var P = 0;
    var BLen = 0;
    var Len = 0;
    var HB = false;
    var IBC = false;
    Result = "";
    L = Line;
    BLen = BreakStr.length;
    if (BLen > 0) {
      BC = BreakStr.charAt(0)}
     else BC = "\x00";
    Len = L.length;
    while (Len > 0) {
      P = 1;
      LQ = "\x00";
      HB = false;
      IBC = false;
      while ((P <= Len) && ((P <= MaxCol) || !IBC) && ((LQ !== "\x00") || !HB)) {
        C = L.charAt(P - 1);
        if (C === LQ) {
          LQ = "\x00"}
         else if (C.charCodeAt() in Quotes) LQ = C;
        if (LQ !== "\x00") {
          P += 1}
         else {
          HB = (C === BC) && (BreakStr === pas.System.Copy(L,P,BLen));
          if (HB) {
            P += BLen}
           else {
            if (P >= MaxCol) IBC = $mod.CharInSet(C,BreakChars);
            P += 1;
          };
        };
      };
      Result = Result + pas.System.Copy(L,1,P - 1);
      pas.System.Delete({get: function () {
          return L;
        }, set: function (v) {
          L = v;
        }},1,P - 1);
      Len = L.length;
      if ((Len > 0) && !HB) Result = Result + BreakStr;
    };
    return Result;
  };
  this.WrapText$1 = function (Line, MaxCol) {
    var Result = "";
    Result = $mod.WrapText(Line,pas.System.sLineBreak,[" ","-","\t"],MaxCol);
    return Result;
  };
  this.IntToStr = function (Value) {
    var Result = "";
    Result = "" + Value;
    return Result;
  };
  this.TryStrToInt = function (S, res) {
    var Result = false;
    var NI = 0;
    Result = $mod.TryStrToInt$1(S,{get: function () {
        return NI;
      }, set: function (v) {
        NI = v;
      }});
    if (Result) res.set(NI);
    return Result;
  };
  this.TryStrToInt$1 = function (S, res) {
    var Result = false;
    var Radix = 10;
    var N = "";
    var J = undefined;
    N = S;
    var $tmp1 = pas.System.Copy(N,1,1);
    if ($tmp1 === "$") {
      Radix = 16}
     else if ($tmp1 === "&") {
      Radix = 8}
     else if ($tmp1 === "%") Radix = 2;
    if (Radix !== 10) pas.System.Delete({get: function () {
        return N;
      }, set: function (v) {
        N = v;
      }},1,1);
    J = parseInt(N,Radix);
    Result = !isNaN(J);
    if (Result) res.set(Math.floor(J));
    return Result;
  };
  this.StrToIntDef = function (S, aDef) {
    var Result = 0;
    var R = 0;
    if ($mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }})) {
      Result = R}
     else Result = aDef;
    return Result;
  };
  this.StrToIntDef$1 = function (S, aDef) {
    var Result = 0;
    var R = 0;
    if ($mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }})) {
      Result = R}
     else Result = aDef;
    return Result;
  };
  this.StrToInt = function (S) {
    var Result = 0;
    var R = 0;
    if (!$mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SErrInvalidInteger,[S]]);
    Result = R;
    return Result;
  };
  this.StrToNativeInt = function (S) {
    var Result = 0;
    if (!$mod.TryStrToInt$1(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SErrInvalidInteger,[S]]);
    return Result;
  };
  this.StrToInt64 = function (S) {
    var Result = 0;
    var N = 0;
    if (!$mod.TryStrToInt$1(S,{get: function () {
        return N;
      }, set: function (v) {
        N = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SErrInvalidInteger,[S]]);
    Result = N;
    return Result;
  };
  this.StrToInt64Def = function (S, ADefault) {
    var Result = 0;
    if ($mod.TryStrToInt64(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) Result = ADefault;
    return Result;
  };
  this.TryStrToInt64 = function (S, res) {
    var Result = false;
    var R = 0;
    Result = $mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }});
    if (Result) res.set(R);
    return Result;
  };
  this.StrToQWord = function (S) {
    var Result = 0;
    var N = 0;
    if (!$mod.TryStrToInt$1(S,{get: function () {
        return N;
      }, set: function (v) {
        N = v;
      }}) || (N < 0)) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SErrInvalidInteger,[S]]);
    Result = N;
    return Result;
  };
  this.StrToQWordDef = function (S, ADefault) {
    var Result = 0;
    if (!$mod.TryStrToQWord(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) Result = ADefault;
    return Result;
  };
  this.TryStrToQWord = function (S, res) {
    var Result = false;
    var R = 0;
    Result = $mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }}) && (R >= 0);
    if (Result) res.set(R);
    return Result;
  };
  this.StrToUInt64 = function (S) {
    var Result = 0;
    var N = 0;
    if (!$mod.TryStrToInt$1(S,{get: function () {
        return N;
      }, set: function (v) {
        N = v;
      }}) || (N < 0)) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SErrInvalidInteger,[S]]);
    Result = N;
    return Result;
  };
  this.StrToUInt64Def = function (S, ADefault) {
    var Result = 0;
    if (!$mod.TryStrToUInt64(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) Result = ADefault;
    return Result;
  };
  this.TryStrToUInt64 = function (S, res) {
    var Result = false;
    var R = 0;
    Result = $mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }}) && (R >= 0);
    if (Result) res.set(R);
    return Result;
  };
  this.StrToDWord = function (S) {
    var Result = 0;
    if (!$mod.TryStrToDWord(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SErrInvalidInteger,[S]]);
    return Result;
  };
  this.StrToDWordDef = function (S, ADefault) {
    var Result = 0;
    if (!$mod.TryStrToDWord(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) Result = ADefault;
    return Result;
  };
  this.TryStrToDWord = function (S, res) {
    var Result = false;
    var R = 0;
    Result = $mod.TryStrToInt$1(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }}) && (R >= 0) && (R <= 0xFFFFFFFF);
    if (Result) res.set(R);
    return Result;
  };
  var HexDigits = "0123456789ABCDEF";
  this.IntToHex = function (Value, Digits) {
    var Result = "";
    if (Digits === 0) Digits = 1;
    Result = "";
    while (Value > 0) {
      Result = HexDigits.charAt(((Value & 15) + 1) - 1) + Result;
      Value = Value >>> 4;
    };
    while (Result.length < Digits) Result = "0" + Result;
    return Result;
  };
  this.MaxCurrency = 450359962737.0495;
  this.MinCurrency = -450359962737.0496;
  this.TFloatFormat = {"0": "ffFixed", ffFixed: 0, "1": "ffGeneral", ffGeneral: 1, "2": "ffExponent", ffExponent: 2, "3": "ffNumber", ffNumber: 3, "4": "ffCurrency", ffCurrency: 4};
  var Rounds = "123456789:";
  this.FloatToDecimal = function (Value, Precision, Decimals) {
    var Result = $mod.TFloatRec.$new();
    var Buffer = "";
    var InfNan = "";
    var OutPos = 0;
    var error = 0;
    var N = 0;
    var L = 0;
    var C = 0;
    var GotNonZeroBeforeDot = false;
    var BeforeDot = false;
    Result.Negative = false;
    Result.Exponent = 0;
    for (C = 0; C <= 19; C++) Result.Digits[C] = "0";
    if (Value === 0) return Result;
    Buffer=Value.toPrecision(21); // Double precision;
    N = 1;
    L = Buffer.length;
    while (Buffer.charAt(N - 1) === " ") N += 1;
    Result.Negative = Buffer.charAt(N - 1) === "-";
    if (Result.Negative) {
      N += 1}
     else if (Buffer.charAt(N - 1) === "+") N += 1;
    if (L >= (N + 2)) {
      InfNan = pas.System.Copy(Buffer,N,3);
      if (InfNan === "Inf") {
        Result.Digits[0] = "\x00";
        Result.Exponent = 32767;
        return Result;
      };
      if (InfNan === "Nan") {
        Result.Digits[0] = "\x00";
        Result.Exponent = -32768;
        return Result;
      };
    };
    OutPos = 0;
    Result.Exponent = 0;
    BeforeDot = true;
    GotNonZeroBeforeDot = false;
    while ((L >= N) && (Buffer.charAt(N - 1) !== "E")) {
      if (Buffer.charAt(N - 1) === ".") {
        BeforeDot = false}
       else {
        if (BeforeDot) {
          Result.Exponent += 1;
          Result.Digits[OutPos] = Buffer.charAt(N - 1);
          if (Buffer.charAt(N - 1) !== "0") GotNonZeroBeforeDot = true;
        } else Result.Digits[OutPos] = Buffer.charAt(N - 1);
        OutPos += 1;
      };
      N += 1;
    };
    N += 1;
    if (N <= L) {
      pas.System.val$6(pas.System.Copy(Buffer,N,(L - N) + 1),{get: function () {
          return C;
        }, set: function (v) {
          C = v;
        }},{get: function () {
          return error;
        }, set: function (v) {
          error = v;
        }});
      Result.Exponent += C;
    };
    N = OutPos;
    L = 19;
    while (N < L) {
      Result.Digits[N] = "0";
      N += 1;
    };
    if ((Decimals + Result.Exponent) < Precision) {
      N = Decimals + Result.Exponent}
     else N = Precision;
    if (N >= L) N = L - 1;
    if (N === 0) {
      if (Result.Digits[0] >= "5") {
        Result.Digits[0] = "1";
        Result.Digits[1] = "\x00";
        Result.Exponent += 1;
      } else Result.Digits[0] = "\x00";
    } else if (N > 0) {
      if (Result.Digits[N] >= "5") {
        do {
          Result.Digits[N] = "\x00";
          N -= 1;
          Result.Digits[N] = Rounds.charAt(($mod.StrToInt(Result.Digits[N]) + 1) - 1);
        } while (!((N === 0) || (Result.Digits[N] < ":")));
        if (Result.Digits[0] === ":") {
          Result.Digits[0] = "1";
          Result.Exponent += 1;
        };
      } else {
        Result.Digits[N] = "0";
        while ((N > -1) && (Result.Digits[N] === "0")) {
          Result.Digits[N] = "\x00";
          N -= 1;
        };
      };
    } else Result.Digits[0] = "\x00";
    if ((Result.Digits[0] === "\x00") && !GotNonZeroBeforeDot) {
      Result.Exponent = 0;
      Result.Negative = false;
    };
    return Result;
  };
  this.FloatToStr = function (Value) {
    var Result = "";
    Result = $mod.FloatToStrF(Value,$mod.TFloatFormat.ffGeneral,15,0);
    return Result;
  };
  this.FloatToStrF = function (Value, format, Precision, Digits) {
    var Result = "";
    var DS = "";
    DS = $mod.DecimalSeparator;
    var $tmp1 = format;
    if ($tmp1 === $mod.TFloatFormat.ffGeneral) {
      Result = $impl.FormatGeneralFloat(Value,Precision,DS)}
     else if ($tmp1 === $mod.TFloatFormat.ffExponent) {
      Result = $impl.FormatExponentFloat(Value,Precision,Digits,DS)}
     else if ($tmp1 === $mod.TFloatFormat.ffFixed) {
      Result = $impl.FormatFixedFloat(Value,Digits,DS)}
     else if ($tmp1 === $mod.TFloatFormat.ffNumber) {
      Result = $impl.FormatNumberFloat(Value,Digits,DS,$mod.ThousandSeparator)}
     else if ($tmp1 === $mod.TFloatFormat.ffCurrency) Result = $impl.FormatNumberCurrency(Value * 10000,Digits,DS,$mod.ThousandSeparator);
    if ((format !== $mod.TFloatFormat.ffCurrency) && (Result.length > 1) && (Result.charAt(0) === "-")) $impl.RemoveLeadingNegativeSign({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},DS);
    return Result;
  };
  this.TryStrToFloat = function (S, res) {
    var Result = false;
    var J = undefined;
    var N = "";
    N = S;
    if ($mod.ThousandSeparator !== "") N = $mod.StringReplace(N,$mod.ThousandSeparator,"",rtl.createSet($mod.TStringReplaceFlag.rfReplaceAll));
    if ($mod.DecimalSeparator !== ".") N = $mod.StringReplace(N,$mod.DecimalSeparator,".",{});
    J = parseFloat(N);
    Result = !isNaN(J);
    if (Result) res.set(rtl.getNumber(J));
    return Result;
  };
  this.StrToFloatDef = function (S, aDef) {
    var Result = 0.0;
    if (!$mod.TryStrToFloat(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) Result = aDef;
    return Result;
  };
  this.StrToFloat = function (S) {
    var Result = 0.0;
    if (!$mod.TryStrToFloat(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SErrInvalidFloat,[S]]);
    return Result;
  };
  var MaxPrecision = 18;
  this.FormatFloat = function (Fmt, aValue) {
    var Result = "";
    var E = 0.0;
    var FV = $mod.TFloatRec.$new();
    var Section = "";
    var SectionLength = 0;
    var ThousandSep = false;
    var IsScientific = false;
    var DecimalPos = 0;
    var FirstDigit = 0;
    var LastDigit = 0;
    var RequestedDigits = 0;
    var ExpSize = 0;
    var Available = 0;
    var Current = 0;
    var PadZeroes = 0;
    var DistToDecimal = 0;
    function InitVars() {
      E = aValue;
      Section = "";
      SectionLength = 0;
      ThousandSep = false;
      IsScientific = false;
      DecimalPos = 0;
      FirstDigit = 2147483647;
      LastDigit = 0;
      RequestedDigits = 0;
      ExpSize = 0;
      Available = -1;
    };
    function ToResult(AChar) {
      Result = Result + AChar;
    };
    function AddToResult(AStr) {
      Result = Result + AStr;
    };
    function WriteDigit(ADigit) {
      if (ADigit === "\x00") return;
      DistToDecimal -= 1;
      if (DistToDecimal === -1) {
        AddToResult($mod.DecimalSeparator);
        ToResult(ADigit);
      } else {
        ToResult(ADigit);
        if (ThousandSep && ((DistToDecimal % 3) === 0) && (DistToDecimal > 1)) AddToResult($mod.ThousandSeparator);
      };
    };
    function GetDigit() {
      var Result = "";
      Result = "\x00";
      if (Current <= Available) {
        Result = FV.Digits[Current];
        Current += 1;
      } else if (DistToDecimal <= LastDigit) {
        DistToDecimal -= 1}
       else Result = "0";
      return Result;
    };
    function CopyDigit() {
      if (PadZeroes === 0) {
        WriteDigit(GetDigit())}
       else if (PadZeroes < 0) {
        PadZeroes += 1;
        if (DistToDecimal <= FirstDigit) {
          WriteDigit("0")}
         else DistToDecimal -= 1;
      } else {
        while (PadZeroes > 0) {
          WriteDigit(GetDigit());
          PadZeroes -= 1;
        };
        WriteDigit(GetDigit());
      };
    };
    function GetSections(SP) {
      var Result = 0;
      var FL = 0;
      var i = 0;
      var C = "";
      var Q = "";
      var inQuote = false;
      Result = 1;
      SP.get()[1] = -1;
      SP.get()[2] = -1;
      SP.get()[3] = -1;
      inQuote = false;
      Q = "\x00";
      i = 1;
      FL = Fmt.length;
      while (i <= FL) {
        C = Fmt.charAt(i - 1);
        var $tmp1 = C;
        if ($tmp1 === ";") {
          if (!inQuote) {
            if (Result > 3) throw $mod.Exception.$create("Create$1",["Invalid float format"]);
            SP.get()[Result] = i + 1;
            Result += 1;
          };
        } else if (($tmp1 === '"') || ($tmp1 === "'")) {
          if (inQuote) {
            inQuote = C !== Q}
           else {
            inQuote = true;
            Q = C;
          };
        };
        i += 1;
      };
      if (SP.get()[Result] === -1) SP.get()[Result] = FL + 1;
      return Result;
    };
    function AnalyzeFormat() {
      var I = 0;
      var Len = 0;
      var Q = "";
      var C = "";
      var InQuote = false;
      Len = Section.length;
      I = 1;
      InQuote = false;
      Q = "\x00";
      while (I <= Len) {
        C = Section.charAt(I - 1);
        if (C.charCodeAt() in rtl.createSet(34,39)) {
          if (InQuote) {
            InQuote = C !== Q}
           else {
            InQuote = true;
            Q = C;
          };
        } else if (!InQuote) {
          var $tmp1 = C;
          if ($tmp1 === ".") {
            if (DecimalPos === 0) DecimalPos = RequestedDigits + 1}
           else if ($tmp1 === ",") {
            ThousandSep = $mod.ThousandSeparator !== "\x00"}
           else if (($tmp1 === "e") || ($tmp1 === "E")) {
            I += 1;
            if (I < Len) {
              C = Section.charAt(I - 1);
              IsScientific = C.charCodeAt() in rtl.createSet(45,43);
              if (IsScientific) while ((I < Len) && (Section.charAt((I + 1) - 1) === "0")) {
                ExpSize += 1;
                I += 1;
              };
              if (ExpSize > 4) ExpSize = 4;
            };
          } else if ($tmp1 === "#") {
            RequestedDigits += 1}
           else if ($tmp1 === "0") {
            if (RequestedDigits < FirstDigit) FirstDigit = RequestedDigits + 1;
            RequestedDigits += 1;
            LastDigit = RequestedDigits + 1;
          };
        };
        I += 1;
      };
      if (DecimalPos === 0) DecimalPos = RequestedDigits + 1;
      LastDigit = DecimalPos - LastDigit;
      if (LastDigit > 0) LastDigit = 0;
      FirstDigit = DecimalPos - FirstDigit;
      if (FirstDigit < 0) FirstDigit = 0;
    };
    function ValueOutSideScope() {
      var Result = false;
      Result = ((FV.Exponent >= 18) && !IsScientific) || (FV.Exponent === 0x7FF) || (FV.Exponent === 0x800);
      return Result;
    };
    function CalcRunVars() {
      var D = 0;
      var P = 0;
      if (IsScientific) {
        P = RequestedDigits;
        D = 9999;
      } else {
        P = 18;
        D = (RequestedDigits - DecimalPos) + 1;
      };
      FV.$assign($mod.FloatToDecimal(aValue,P,D));
      DistToDecimal = DecimalPos - 1;
      if (IsScientific) {
        PadZeroes = 0}
       else {
        PadZeroes = FV.Exponent - (DecimalPos - 1);
        if (PadZeroes >= 0) DistToDecimal = FV.Exponent;
      };
      Available = -1;
      while ((Available < 18) && (FV.Digits[Available + 1] !== "\x00")) Available += 1;
    };
    function FormatExponent(ASign, aExponent) {
      var Result = "";
      Result = $mod.IntToStr(aExponent);
      Result = pas.System.StringOfChar("0",ExpSize - Result.length) + Result;
      if (aExponent < 0) {
        Result = "-" + Result}
       else if ((aExponent > 0) && (ASign === "+")) Result = ASign + Result;
      return Result;
    };
    var I = 0;
    var S = 0;
    var C = "";
    var Q = "";
    var PA = [];
    var InLiteral = false;
    PA = rtl.arraySetLength(PA,0,4);
    Result = "";
    InitVars();
    if (E > 0) {
      S = 1}
     else if (E < 0) {
      S = 2}
     else S = 3;
    PA[0] = 0;
    I = GetSections({get: function () {
        return PA;
      }, set: function (v) {
        PA = v;
      }});
    if ((I < S) || ((PA[S] - PA[S - 1]) === 0)) S = 1;
    SectionLength = PA[S] - PA[S - 1] - 1;
    Section = pas.System.Copy(Fmt,PA[S - 1] + 1,SectionLength);
    Section = rtl.strSetLength(Section,SectionLength);
    AnalyzeFormat();
    CalcRunVars();
    if ((SectionLength === 0) || ValueOutSideScope()) {
      Section=E.toPrecision(15);
      Result = Section;
    };
    I = 1;
    Current = 0;
    Q = " ";
    InLiteral = false;
    if (FV.Negative && (S === 1)) ToResult("-");
    while (I <= SectionLength) {
      C = Section.charAt(I - 1);
      if (C.charCodeAt() in rtl.createSet(34,39)) {
        if (InLiteral) {
          InLiteral = C !== Q}
         else {
          InLiteral = true;
          Q = C;
        };
      } else if (InLiteral) {
        ToResult(C)}
       else {
        var $tmp1 = C;
        if (($tmp1 === "0") || ($tmp1 === "#")) {
          CopyDigit()}
         else if (($tmp1 === ".") || ($tmp1 === ",")) {}
        else if (($tmp1 === "e") || ($tmp1 === "E")) {
          ToResult(C);
          I += 1;
          if (I <= Section.length) {
            C = Section.charAt(I - 1);
            if (C.charCodeAt() in rtl.createSet(43,45)) {
              AddToResult(FormatExponent(C,(FV.Exponent - DecimalPos) + 1));
              while ((I < SectionLength) && (Section.charAt((I + 1) - 1) === "0")) I += 1;
            };
          };
        } else {
          ToResult(C);
        };
      };
      I += 1;
    };
    return Result;
  };
  this.TrueBoolStrs = [];
  this.FalseBoolStrs = [];
  this.StrToBool = function (S) {
    var Result = false;
    if (!$mod.TryStrToBool(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SInvalidBoolean,[S]]);
    return Result;
  };
  this.BoolToStr = function (B, UseBoolStrs) {
    var Result = "";
    if (UseBoolStrs) {
      $impl.CheckBoolStrs();
      if (B) {
        Result = $mod.TrueBoolStrs[0]}
       else Result = $mod.FalseBoolStrs[0];
    } else if (B) {
      Result = "-1"}
     else Result = "0";
    return Result;
  };
  this.BoolToStr$1 = function (B, TrueS, FalseS) {
    var Result = "";
    if (B) {
      Result = TrueS}
     else Result = FalseS;
    return Result;
  };
  this.StrToBoolDef = function (S, Default) {
    var Result = false;
    if (!$mod.TryStrToBool(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) Result = Default;
    return Result;
  };
  this.TryStrToBool = function (S, Value) {
    var Result = false;
    var Temp = "";
    var I = 0;
    var D = 0.0;
    var Code = 0;
    Temp = $mod.UpperCase(S);
    pas.System.val$8(Temp,{get: function () {
        return D;
      }, set: function (v) {
        D = v;
      }},{get: function () {
        return Code;
      }, set: function (v) {
        Code = v;
      }});
    Result = true;
    if (Code === 0) {
      Value.set(D !== 0.0)}
     else {
      $impl.CheckBoolStrs();
      for (var $l1 = 0, $end2 = rtl.length($mod.TrueBoolStrs) - 1; $l1 <= $end2; $l1++) {
        I = $l1;
        if (Temp === $mod.UpperCase($mod.TrueBoolStrs[I])) {
          Value.set(true);
          return Result;
        };
      };
      for (var $l3 = 0, $end4 = rtl.length($mod.FalseBoolStrs) - 1; $l3 <= $end4; $l3++) {
        I = $l3;
        if (Temp === $mod.UpperCase($mod.FalseBoolStrs[I])) {
          Value.set(false);
          return Result;
        };
      };
      Result = false;
    };
    return Result;
  };
  this.ConfigExtension = ".cfg";
  this.SysConfigDir = "";
  $mod.$rtti.$ProcVar("TOnGetEnvironmentVariable",{procsig: rtl.newTIProcSig([["EnvVar",rtl.string,2]],rtl.string)});
  $mod.$rtti.$ProcVar("TOnGetEnvironmentString",{procsig: rtl.newTIProcSig([["Index",rtl.longint]],rtl.string)});
  $mod.$rtti.$ProcVar("TOnGetEnvironmentVariableCount",{procsig: rtl.newTIProcSig(null,rtl.longint)});
  this.OnGetEnvironmentVariable = null;
  this.OnGetEnvironmentString = null;
  this.OnGetEnvironmentVariableCount = null;
  this.GetEnvironmentVariable = function (EnvVar) {
    var Result = "";
    if ($mod.OnGetEnvironmentVariable != null) {
      Result = $mod.OnGetEnvironmentVariable(EnvVar)}
     else Result = "";
    return Result;
  };
  this.GetEnvironmentVariableCount = function () {
    var Result = 0;
    if ($mod.OnGetEnvironmentVariableCount != null) {
      Result = $mod.OnGetEnvironmentVariableCount()}
     else Result = 0;
    return Result;
  };
  this.GetEnvironmentString = function (Index) {
    var Result = "";
    if ($mod.OnGetEnvironmentString != null) {
      Result = $mod.OnGetEnvironmentString(Index)}
     else Result = "";
    return Result;
  };
  this.ShowException = function (ExceptObject, ExceptAddr) {
    var S = "";
    S = "Application raised an exception " + ExceptObject.$classname;
    if ($mod.Exception.isPrototypeOf(ExceptObject)) S = S + " : " + ExceptObject.fMessage;
    window.alert(S);
    if (ExceptAddr === null) ;
  };
  this.Abort = function () {
    throw $mod.EAbort.$create("Create$1",[$impl.SAbortError]);
  };
  this.TEventType = {"0": "etCustom", etCustom: 0, "1": "etInfo", etInfo: 1, "2": "etWarning", etWarning: 2, "3": "etError", etError: 3, "4": "etDebug", etDebug: 4};
  $mod.$rtti.$Enum("TEventType",{minvalue: 0, maxvalue: 4, ordtype: 1, enumtype: this.TEventType});
  $mod.$rtti.$Set("TEventTypes",{comptype: $mod.$rtti["TEventType"]});
  rtl.recNewT($mod,"TSystemTime",function () {
    this.Year = 0;
    this.Month = 0;
    this.Day = 0;
    this.DayOfWeek = 0;
    this.Hour = 0;
    this.Minute = 0;
    this.Second = 0;
    this.MilliSecond = 0;
    this.$eq = function (b) {
      return (this.Year === b.Year) && (this.Month === b.Month) && (this.Day === b.Day) && (this.DayOfWeek === b.DayOfWeek) && (this.Hour === b.Hour) && (this.Minute === b.Minute) && (this.Second === b.Second) && (this.MilliSecond === b.MilliSecond);
    };
    this.$assign = function (s) {
      this.Year = s.Year;
      this.Month = s.Month;
      this.Day = s.Day;
      this.DayOfWeek = s.DayOfWeek;
      this.Hour = s.Hour;
      this.Minute = s.Minute;
      this.Second = s.Second;
      this.MilliSecond = s.MilliSecond;
      return this;
    };
    var $r = $mod.$rtti.$Record("TSystemTime",{});
    $r.addField("Year",rtl.word);
    $r.addField("Month",rtl.word);
    $r.addField("Day",rtl.word);
    $r.addField("DayOfWeek",rtl.word);
    $r.addField("Hour",rtl.word);
    $r.addField("Minute",rtl.word);
    $r.addField("Second",rtl.word);
    $r.addField("MilliSecond",rtl.word);
  });
  rtl.recNewT($mod,"TTimeStamp",function () {
    this.Time = 0;
    this.Date = 0;
    this.$eq = function (b) {
      return (this.Time === b.Time) && (this.Date === b.Date);
    };
    this.$assign = function (s) {
      this.Time = s.Time;
      this.Date = s.Date;
      return this;
    };
    var $r = $mod.$rtti.$Record("TTimeStamp",{});
    $r.addField("Time",rtl.longint);
    $r.addField("Date",rtl.longint);
  });
  this.TimeSeparator = ":";
  this.DateSeparator = "-";
  this.ShortDateFormat = "yyyy-mm-dd";
  this.LongDateFormat = "ddd, yyyy-mm-dd";
  this.ShortTimeFormat = "hh:nn";
  this.LongTimeFormat = "hh:nn:ss";
  this.DecimalSeparator = ".";
  this.ThousandSeparator = "";
  this.TimeAMString = "AM";
  this.TimePMString = "PM";
  this.HoursPerDay = 24;
  this.MinsPerHour = 60;
  this.SecsPerMin = 60;
  this.MSecsPerSec = 1000;
  this.MinsPerDay = 24 * 60;
  this.SecsPerDay = 1440 * 60;
  this.MSecsPerDay = 86400 * 1000;
  this.MaxDateTime = 2958465.99999999;
  this.MinDateTime = -693593.99999999;
  this.JulianEpoch = -2415018.5;
  this.UnixEpoch = -2415018.5 + 2440587.5;
  this.DateDelta = 693594;
  this.UnixDateDelta = 25569;
  this.MonthDays = [[31,28,31,30,31,30,31,31,30,31,30,31],[31,29,31,30,31,30,31,31,30,31,30,31]];
  this.ShortMonthNames = ["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"];
  this.LongMonthNames = ["January","February","March","April","May","June","July","August","September","October","November","December"];
  this.ShortDayNames = ["Sun","Mon","Tue","Wed","Thu","Fri","Sat"];
  this.LongDayNames = ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"];
  rtl.createClass($mod,"TFormatSettings",pas.System.TObject,function () {
    this.GetCurrencyDecimals = function () {
      var Result = 0;
      Result = $mod.CurrencyDecimals;
      return Result;
    };
    this.GetCurrencyFormat = function () {
      var Result = 0;
      Result = $mod.CurrencyFormat;
      return Result;
    };
    this.GetCurrencyString = function () {
      var Result = "";
      Result = $mod.CurrencyString;
      return Result;
    };
    this.GetDateSeparator = function () {
      var Result = "";
      Result = $mod.DateSeparator;
      return Result;
    };
    this.GetDecimalSeparator = function () {
      var Result = "";
      Result = $mod.DecimalSeparator;
      return Result;
    };
    this.GetLongDateFormat = function () {
      var Result = "";
      Result = $mod.LongDateFormat;
      return Result;
    };
    this.GetLongDayNames = function () {
      var Result = rtl.arraySetLength(null,"",7);
      Result = $mod.LongDayNames.slice(0);
      return Result;
    };
    this.GetLongMonthNames = function () {
      var Result = rtl.arraySetLength(null,"",12);
      Result = $mod.LongMonthNames.slice(0);
      return Result;
    };
    this.GetLongTimeFormat = function () {
      var Result = "";
      Result = $mod.LongTimeFormat;
      return Result;
    };
    this.GetNegCurrFormat = function () {
      var Result = 0;
      Result = $mod.NegCurrFormat;
      return Result;
    };
    this.GetShortDateFormat = function () {
      var Result = "";
      Result = $mod.ShortDateFormat;
      return Result;
    };
    this.GetShortDayNames = function () {
      var Result = rtl.arraySetLength(null,"",7);
      Result = $mod.ShortDayNames.slice(0);
      return Result;
    };
    this.GetShortMonthNames = function () {
      var Result = rtl.arraySetLength(null,"",12);
      Result = $mod.ShortMonthNames.slice(0);
      return Result;
    };
    this.GetShortTimeFormat = function () {
      var Result = "";
      Result = $mod.ShortTimeFormat;
      return Result;
    };
    this.GetThousandSeparator = function () {
      var Result = "";
      Result = $mod.ThousandSeparator;
      return Result;
    };
    this.GetTimeAMString = function () {
      var Result = "";
      Result = $mod.TimeAMString;
      return Result;
    };
    this.GetTimePMString = function () {
      var Result = "";
      Result = $mod.TimePMString;
      return Result;
    };
    this.GetTimeSeparator = function () {
      var Result = "";
      Result = $mod.TimeSeparator;
      return Result;
    };
    this.SetCurrencyFormat = function (AValue) {
      $mod.CurrencyFormat = AValue;
    };
    this.SetCurrencyString = function (AValue) {
      $mod.CurrencyString = AValue;
    };
    this.SetDateSeparator = function (Value) {
      $mod.DateSeparator = Value;
    };
    this.SetDecimalSeparator = function (Value) {
      $mod.DecimalSeparator = Value;
    };
    this.SetLongDateFormat = function (Value) {
      $mod.LongDateFormat = Value;
    };
    this.SetLongDayNames = function (AValue) {
      $mod.LongDayNames = AValue.slice(0);
    };
    this.SetLongMonthNames = function (AValue) {
      $mod.LongMonthNames = AValue.slice(0);
    };
    this.SetLongTimeFormat = function (Value) {
      $mod.LongTimeFormat = Value;
    };
    this.SetNegCurrFormat = function (AValue) {
      $mod.NegCurrFormat = AValue;
    };
    this.SetShortDateFormat = function (Value) {
      $mod.ShortDateFormat = Value;
    };
    this.SetShortDayNames = function (AValue) {
      $mod.ShortDayNames = AValue.slice(0);
    };
    this.SetShortMonthNames = function (AValue) {
      $mod.ShortMonthNames = AValue.slice(0);
    };
    this.SetShortTimeFormat = function (Value) {
      $mod.ShortTimeFormat = Value;
    };
    this.SetCurrencyDecimals = function (AValue) {
      $mod.CurrencyDecimals = AValue;
    };
    this.SetThousandSeparator = function (Value) {
      $mod.ThousandSeparator = Value;
    };
    this.SetTimeAMString = function (Value) {
      $mod.TimeAMString = Value;
    };
    this.SetTimePMString = function (Value) {
      $mod.TimePMString = Value;
    };
    this.SetTimeSeparator = function (Value) {
      $mod.TimeSeparator = Value;
    };
  });
  this.FormatSettings = null;
  this.TwoDigitYearCenturyWindow = 50;
  this.DateTimeToJSDate = function (aDateTime) {
    var Result = null;
    var Y = 0;
    var M = 0;
    var D = 0;
    var h = 0;
    var n = 0;
    var s = 0;
    var z = 0;
    $mod.DecodeDate(pas.System.Trunc(aDateTime),{get: function () {
        return Y;
      }, set: function (v) {
        Y = v;
      }},{get: function () {
        return M;
      }, set: function (v) {
        M = v;
      }},{get: function () {
        return D;
      }, set: function (v) {
        D = v;
      }});
    $mod.DecodeTime(pas.System.Frac(aDateTime),{get: function () {
        return h;
      }, set: function (v) {
        h = v;
      }},{get: function () {
        return n;
      }, set: function (v) {
        n = v;
      }},{get: function () {
        return s;
      }, set: function (v) {
        s = v;
      }},{get: function () {
        return z;
      }, set: function (v) {
        z = v;
      }});
    Result = new Date(Y,M,D,h,n,s,z);
    return Result;
  };
  this.JSDateToDateTime = function (aDate) {
    var Result = 0.0;
    Result = $mod.EncodeDate(aDate.getFullYear(),aDate.getMonth() + 1,aDate.getDate()) + $mod.EncodeTime(aDate.getHours(),aDate.getMinutes(),aDate.getSeconds(),aDate.getMilliseconds());
    return Result;
  };
  this.DateTimeToTimeStamp = function (DateTime) {
    var Result = $mod.TTimeStamp.$new();
    var D = 0.0;
    D = DateTime * 86400000;
    if (D < 0) {
      D = D - 0.5}
     else D = D + 0.5;
    Result.Time = pas.System.Trunc(Math.abs(pas.System.Trunc(D)) % 86400000);
    Result.Date = 693594 + Math.floor(pas.System.Trunc(D) / 86400000);
    return Result;
  };
  this.TimeStampToDateTime = function (TimeStamp) {
    var Result = 0.0;
    Result = $mod.ComposeDateTime(TimeStamp.Date - 693594,TimeStamp.Time / 86400000);
    return Result;
  };
  this.MSecsToTimeStamp = function (MSecs) {
    var Result = $mod.TTimeStamp.$new();
    Result.Date = pas.System.Trunc(MSecs / 86400000);
    MSecs = MSecs - (Result.Date * 86400000);
    Result.Time = Math.round(MSecs);
    return Result;
  };
  this.TimeStampToMSecs = function (TimeStamp) {
    var Result = 0;
    Result = TimeStamp.Time + (TimeStamp.Date * 86400000);
    return Result;
  };
  this.TryEncodeDate = function (Year, Month, Day, date) {
    var Result = false;
    var c = 0;
    var ya = 0;
    Result = (Year > 0) && (Year < 10000) && (Month >= 1) && (Month <= 12) && (Day > 0) && (Day <= $mod.MonthDays[+$mod.IsLeapYear(Year)][Month - 1]);
    if (Result) {
      if (Month > 2) {
        Month -= 3}
       else {
        Month += 9;
        Year -= 1;
      };
      c = Math.floor(Year / 100);
      ya = Year - (100 * c);
      date.set(((146097 * c) >>> 2) + ((1461 * ya) >>> 2) + Math.floor(((153 * Month) + 2) / 5) + Day);
      date.set(date.get() - 693900);
    };
    return Result;
  };
  this.TryEncodeTime = function (Hour, Min, Sec, MSec, Time) {
    var Result = false;
    Result = (Hour < 24) && (Min < 60) && (Sec < 60) && (MSec < 1000);
    if (Result) Time.set(((Hour * 3600000) + (Min * 60000) + (Sec * 1000) + MSec) / 86400000);
    return Result;
  };
  this.EncodeDate = function (Year, Month, Day) {
    var Result = 0.0;
    if (!$mod.TryEncodeDate(Year,Month,Day,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",["%s-%s-%s is not a valid date specification",[$mod.IntToStr(Year),$mod.IntToStr(Month),$mod.IntToStr(Day)]]);
    return Result;
  };
  this.EncodeTime = function (Hour, Minute, Second, MilliSecond) {
    var Result = 0.0;
    if (!$mod.TryEncodeTime(Hour,Minute,Second,MilliSecond,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",["%s:%s:%s.%s is not a valid time specification",[$mod.IntToStr(Hour),$mod.IntToStr(Minute),$mod.IntToStr(Second),$mod.IntToStr(MilliSecond)]]);
    return Result;
  };
  this.ComposeDateTime = function (date, Time) {
    var Result = 0.0;
    if (date < 0) {
      Result = pas.System.Trunc(date) - Math.abs(pas.System.Frac(Time))}
     else Result = pas.System.Trunc(date) + Math.abs(pas.System.Frac(Time));
    return Result;
  };
  this.DecodeDate = function (date, Year, Month, Day) {
    var ly = 0;
    var ld = 0;
    var lm = 0;
    var j = 0;
    if (date <= -693594) {
      Year.set(0);
      Month.set(0);
      Day.set(0);
    } else {
      if (date > 0) {
        date = date + (1 / (86400000 * 2))}
       else date = date - (1 / (86400000 * 2));
      if (date > $mod.MaxDateTime) date = $mod.MaxDateTime;
      j = ((pas.System.Trunc(date) + 693900) << 2) - 1;
      ly = Math.floor(j / 146097);
      j = j - (146097 * ly);
      ld = j >>> 2;
      j = Math.floor(((ld << 2) + 3) / 1461);
      ld = (((ld << 2) + 7) - (1461 * j)) >>> 2;
      lm = Math.floor(((5 * ld) - 3) / 153);
      ld = Math.floor((((5 * ld) + 2) - (153 * lm)) / 5);
      ly = (100 * ly) + j;
      if (lm < 10) {
        lm += 3}
       else {
        lm -= 9;
        ly += 1;
      };
      Year.set(ly);
      Month.set(lm);
      Day.set(ld);
    };
  };
  this.DecodeDateFully = function (DateTime, Year, Month, Day, DOW) {
    var Result = false;
    $mod.DecodeDate(DateTime,Year,Month,Day);
    DOW.set($mod.DayOfWeek(DateTime));
    Result = $mod.IsLeapYear(Year.get());
    return Result;
  };
  this.DecodeTime = function (Time, Hour, Minute, Second, MilliSecond) {
    var l = 0;
    l = $mod.DateTimeToTimeStamp(Time).Time;
    Hour.set(Math.floor(l / 3600000));
    l = l % 3600000;
    Minute.set(Math.floor(l / 60000));
    l = l % 60000;
    Second.set(Math.floor(l / 1000));
    l = l % 1000;
    MilliSecond.set(l);
  };
  this.DateTimeToSystemTime = function (DateTime, SystemTime) {
    $mod.DecodeDateFully(DateTime,{p: SystemTime, get: function () {
        return this.p.Year;
      }, set: function (v) {
        this.p.Year = v;
      }},{p: SystemTime, get: function () {
        return this.p.Month;
      }, set: function (v) {
        this.p.Month = v;
      }},{p: SystemTime, get: function () {
        return this.p.Day;
      }, set: function (v) {
        this.p.Day = v;
      }},{p: SystemTime, get: function () {
        return this.p.DayOfWeek;
      }, set: function (v) {
        this.p.DayOfWeek = v;
      }});
    $mod.DecodeTime(DateTime,{p: SystemTime, get: function () {
        return this.p.Hour;
      }, set: function (v) {
        this.p.Hour = v;
      }},{p: SystemTime, get: function () {
        return this.p.Minute;
      }, set: function (v) {
        this.p.Minute = v;
      }},{p: SystemTime, get: function () {
        return this.p.Second;
      }, set: function (v) {
        this.p.Second = v;
      }},{p: SystemTime, get: function () {
        return this.p.MilliSecond;
      }, set: function (v) {
        this.p.MilliSecond = v;
      }});
    SystemTime.DayOfWeek -= 1;
  };
  this.SystemTimeToDateTime = function (SystemTime) {
    var Result = 0.0;
    Result = $mod.ComposeDateTime($impl.DoEncodeDate(SystemTime.Year,SystemTime.Month,SystemTime.Day),$impl.DoEncodeTime(SystemTime.Hour,SystemTime.Minute,SystemTime.Second,SystemTime.MilliSecond));
    return Result;
  };
  this.DayOfWeek = function (DateTime) {
    var Result = 0;
    Result = 1 + ((pas.System.Trunc(DateTime) - 1) % 7);
    if (Result <= 0) Result += 7;
    return Result;
  };
  this.Date = function () {
    var Result = 0.0;
    Result = pas.System.Trunc($mod.Now());
    return Result;
  };
  this.Time = function () {
    var Result = 0.0;
    Result = $mod.Now() - $mod.Date();
    return Result;
  };
  this.Now = function () {
    var Result = 0.0;
    Result = $mod.JSDateToDateTime(new Date());
    return Result;
  };
  this.IncMonth = function (DateTime, NumberOfMonths) {
    var Result = 0.0;
    var Year = 0;
    var Month = 0;
    var Day = 0;
    $mod.DecodeDate(DateTime,{get: function () {
        return Year;
      }, set: function (v) {
        Year = v;
      }},{get: function () {
        return Month;
      }, set: function (v) {
        Month = v;
      }},{get: function () {
        return Day;
      }, set: function (v) {
        Day = v;
      }});
    $mod.IncAMonth({get: function () {
        return Year;
      }, set: function (v) {
        Year = v;
      }},{get: function () {
        return Month;
      }, set: function (v) {
        Month = v;
      }},{get: function () {
        return Day;
      }, set: function (v) {
        Day = v;
      }},NumberOfMonths);
    Result = $mod.ComposeDateTime($impl.DoEncodeDate(Year,Month,Day),DateTime);
    return Result;
  };
  this.IncAMonth = function (Year, Month, Day, NumberOfMonths) {
    var TempMonth = 0;
    var S = 0;
    if (NumberOfMonths >= 0) {
      S = 1}
     else S = -1;
    Year.set(Year.get() + Math.floor(NumberOfMonths / 12));
    TempMonth = (Month.get() + (NumberOfMonths % 12)) - 1;
    if ((TempMonth > 11) || (TempMonth < 0)) {
      TempMonth -= S * 12;
      Year.set(Year.get() + S);
    };
    Month.set(TempMonth + 1);
    if (Day.get() > $mod.MonthDays[+$mod.IsLeapYear(Year.get())][Month.get() - 1]) Day.set($mod.MonthDays[+$mod.IsLeapYear(Year.get())][Month.get() - 1]);
  };
  this.IsLeapYear = function (Year) {
    var Result = false;
    Result = ((Year % 4) === 0) && (((Year % 100) !== 0) || ((Year % 400) === 0));
    return Result;
  };
  this.DateToStr = function (date) {
    var Result = "";
    Result = $mod.FormatDateTime("ddddd",date);
    return Result;
  };
  this.TimeToStr = function (Time) {
    var Result = "";
    Result = $mod.FormatDateTime("tt",Time);
    return Result;
  };
  this.DateTimeToStr = function (DateTime, ForceTimeIfZero) {
    var Result = "";
    Result = $mod.FormatDateTime($impl.DateTimeToStrFormat[+ForceTimeIfZero],DateTime);
    return Result;
  };
  this.StrToDate = function (S) {
    var Result = 0.0;
    Result = $mod.StrToDate$2(S,$mod.ShortDateFormat,"\x00");
    return Result;
  };
  this.StrToDate$1 = function (S, separator) {
    var Result = 0.0;
    Result = $mod.StrToDate$2(S,$mod.ShortDateFormat,separator);
    return Result;
  };
  this.StrToDate$2 = function (S, useformat, separator) {
    var Result = 0.0;
    var MSg = "";
    Result = $impl.IntStrToDate({get: function () {
        return MSg;
      }, set: function (v) {
        MSg = v;
      }},S,useformat,separator);
    if (MSg !== "") throw $mod.EConvertError.$create("Create$1",[MSg]);
    return Result;
  };
  this.StrToTime = function (S) {
    var Result = 0.0;
    Result = $mod.StrToTime$1(S,$mod.TimeSeparator);
    return Result;
  };
  this.StrToTime$1 = function (S, separator) {
    var Result = 0.0;
    var Msg = "";
    Result = $impl.IntStrToTime({get: function () {
        return Msg;
      }, set: function (v) {
        Msg = v;
      }},S,S.length,separator);
    if (Msg !== "") throw $mod.EConvertError.$create("Create$1",[Msg]);
    return Result;
  };
  this.StrToDateTime = function (S) {
    var Result = 0.0;
    var TimeStr = "";
    var DateStr = "";
    var PartsFound = 0;
    PartsFound = $impl.SplitDateTimeStr(S,{get: function () {
        return DateStr;
      }, set: function (v) {
        DateStr = v;
      }},{get: function () {
        return TimeStr;
      }, set: function (v) {
        TimeStr = v;
      }});
    var $tmp1 = PartsFound;
    if ($tmp1 === 0) {
      Result = $mod.StrToDate("")}
     else if ($tmp1 === 1) {
      if (DateStr.length > 0) {
        Result = $mod.StrToDate$2(DateStr,$mod.ShortDateFormat,$mod.DateSeparator)}
       else Result = $mod.StrToTime(TimeStr)}
     else if ($tmp1 === 2) Result = $mod.ComposeDateTime($mod.StrToDate$2(DateStr,$mod.ShortDateFormat,$mod.DateSeparator),$mod.StrToTime(TimeStr));
    return Result;
  };
  this.FormatDateTime = function (FormatStr, DateTime) {
    var Result = "";
    function StoreStr(APos, Len) {
      Result = Result + pas.System.Copy(FormatStr,APos,Len);
    };
    function StoreString(AStr) {
      Result = Result + AStr;
    };
    function StoreInt(Value, Digits) {
      var S = "";
      S = $mod.IntToStr(Value);
      while (S.length < Digits) S = "0" + S;
      StoreString(S);
    };
    var Year = 0;
    var Month = 0;
    var Day = 0;
    var DayOfWeek = 0;
    var Hour = 0;
    var Minute = 0;
    var Second = 0;
    var MilliSecond = 0;
    function StoreFormat(FormatStr, Nesting, TimeFlag) {
      var Token = "";
      var lastformattoken = "";
      var prevlasttoken = "";
      var Count = 0;
      var Clock12 = false;
      var tmp = 0;
      var isInterval = false;
      var P = 0;
      var FormatCurrent = 0;
      var FormatEnd = 0;
      if (Nesting > 1) return;
      FormatCurrent = 1;
      FormatEnd = FormatStr.length;
      Clock12 = false;
      isInterval = false;
      P = 1;
      while (P <= FormatEnd) {
        Token = FormatStr.charAt(P - 1);
        var $tmp1 = Token;
        if (($tmp1 === "'") || ($tmp1 === '"')) {
          P += 1;
          while ((P < FormatEnd) && (FormatStr.charAt(P - 1) !== Token)) P += 1;
        } else if (($tmp1 === "A") || ($tmp1 === "a")) {
          if (($mod.CompareText(pas.System.Copy(FormatStr,P,3),"A\/P") === 0) || ($mod.CompareText(pas.System.Copy(FormatStr,P,4),"AMPM") === 0) || ($mod.CompareText(pas.System.Copy(FormatStr,P,5),"AM\/PM") === 0)) {
            Clock12 = true;
            break;
          };
        };
        P += 1;
      };
      Token = "ÿ";
      lastformattoken = " ";
      prevlasttoken = "H";
      while (FormatCurrent <= FormatEnd) {
        Token = $mod.UpperCase(FormatStr.charAt(FormatCurrent - 1)).charAt(0);
        Count = 1;
        P = FormatCurrent + 1;
        var $tmp2 = Token;
        if (($tmp2 === "'") || ($tmp2 === '"')) {
          while ((P < FormatEnd) && (FormatStr.charAt(P - 1) !== Token)) P += 1;
          P += 1;
          Count = P - FormatCurrent;
          StoreStr(FormatCurrent + 1,Count - 2);
        } else if ($tmp2 === "A") {
          if ($mod.CompareText(pas.System.Copy(FormatStr,FormatCurrent,4),"AMPM") === 0) {
            Count = 4;
            if (Hour < 12) {
              StoreString($mod.TimeAMString)}
             else StoreString($mod.TimePMString);
          } else if ($mod.CompareText(pas.System.Copy(FormatStr,FormatCurrent,5),"AM\/PM") === 0) {
            Count = 5;
            if (Hour < 12) {
              StoreStr(FormatCurrent,2)}
             else StoreStr(FormatCurrent + 3,2);
          } else if ($mod.CompareText(pas.System.Copy(FormatStr,FormatCurrent,3),"A\/P") === 0) {
            Count = 3;
            if (Hour < 12) {
              StoreStr(FormatCurrent,1)}
             else StoreStr(FormatCurrent + 2,1);
          } else throw $mod.EConvertError.$create("Create$1",["Illegal character in format string"]);
        } else if ($tmp2 === "\/") {
          StoreString($mod.DateSeparator);
        } else if ($tmp2 === ":") {
          StoreString($mod.TimeSeparator)}
         else if (($tmp2 === " ") || ($tmp2 === "C") || ($tmp2 === "D") || ($tmp2 === "H") || ($tmp2 === "M") || ($tmp2 === "N") || ($tmp2 === "S") || ($tmp2 === "T") || ($tmp2 === "Y") || ($tmp2 === "Z") || ($tmp2 === "F")) {
          while ((P <= FormatEnd) && ($mod.UpperCase(FormatStr.charAt(P - 1)) === Token)) P += 1;
          Count = P - FormatCurrent;
          var $tmp3 = Token;
          if ($tmp3 === " ") {
            StoreStr(FormatCurrent,Count)}
           else if ($tmp3 === "Y") {
            if (Count > 2) {
              StoreInt(Year,4)}
             else StoreInt(Year % 100,2);
          } else if ($tmp3 === "M") {
            if (isInterval && ((prevlasttoken === "H") || TimeFlag)) {
              StoreInt(Minute + ((Hour + (pas.System.Trunc(Math.abs(DateTime)) * 24)) * 60),0)}
             else if ((lastformattoken === "H") || TimeFlag) {
              if (Count === 1) {
                StoreInt(Minute,0)}
               else StoreInt(Minute,2);
            } else {
              var $tmp4 = Count;
              if ($tmp4 === 1) {
                StoreInt(Month,0)}
               else if ($tmp4 === 2) {
                StoreInt(Month,2)}
               else if ($tmp4 === 3) {
                StoreString($mod.ShortMonthNames[Month - 1])}
               else {
                StoreString($mod.LongMonthNames[Month - 1]);
              };
            };
          } else if ($tmp3 === "D") {
            var $tmp5 = Count;
            if ($tmp5 === 1) {
              StoreInt(Day,0)}
             else if ($tmp5 === 2) {
              StoreInt(Day,2)}
             else if ($tmp5 === 3) {
              StoreString($mod.ShortDayNames[DayOfWeek])}
             else if ($tmp5 === 4) {
              StoreString($mod.LongDayNames[DayOfWeek])}
             else if ($tmp5 === 5) {
              StoreFormat($mod.ShortDateFormat,Nesting + 1,false)}
             else {
              StoreFormat($mod.LongDateFormat,Nesting + 1,false);
            };
          } else if ($tmp3 === "H") {
            if (isInterval) {
              StoreInt(Hour + (pas.System.Trunc(Math.abs(DateTime)) * 24),0)}
             else if (Clock12) {
              tmp = Hour % 12;
              if (tmp === 0) tmp = 12;
              if (Count === 1) {
                StoreInt(tmp,0)}
               else StoreInt(tmp,2);
            } else {
              if (Count === 1) {
                StoreInt(Hour,0)}
               else StoreInt(Hour,2);
            }}
           else if ($tmp3 === "N") {
            if (isInterval) {
              StoreInt(Minute + ((Hour + (pas.System.Trunc(Math.abs(DateTime)) * 24)) * 60),0)}
             else if (Count === 1) {
              StoreInt(Minute,0)}
             else StoreInt(Minute,2)}
           else if ($tmp3 === "S") {
            if (isInterval) {
              StoreInt(Second + ((Minute + ((Hour + (pas.System.Trunc(Math.abs(DateTime)) * 24)) * 60)) * 60),0)}
             else if (Count === 1) {
              StoreInt(Second,0)}
             else StoreInt(Second,2)}
           else if ($tmp3 === "Z") {
            if (Count === 1) {
              StoreInt(MilliSecond,0)}
             else StoreInt(MilliSecond,3)}
           else if ($tmp3 === "T") {
            if (Count === 1) {
              StoreFormat($mod.ShortTimeFormat,Nesting + 1,true)}
             else StoreFormat($mod.LongTimeFormat,Nesting + 1,true)}
           else if ($tmp3 === "C") {
            StoreFormat($mod.ShortDateFormat,Nesting + 1,false);
            if ((Hour !== 0) || (Minute !== 0) || (Second !== 0)) {
              StoreString(" ");
              StoreFormat($mod.LongTimeFormat,Nesting + 1,true);
            };
          } else if ($tmp3 === "F") {
            StoreFormat($mod.ShortDateFormat,Nesting + 1,false);
            StoreString(" ");
            StoreFormat($mod.LongTimeFormat,Nesting + 1,true);
          };
          prevlasttoken = lastformattoken;
          lastformattoken = Token;
        } else {
          StoreString(Token);
        };
        FormatCurrent += Count;
      };
    };
    $mod.DecodeDateFully(DateTime,{get: function () {
        return Year;
      }, set: function (v) {
        Year = v;
      }},{get: function () {
        return Month;
      }, set: function (v) {
        Month = v;
      }},{get: function () {
        return Day;
      }, set: function (v) {
        Day = v;
      }},{get: function () {
        return DayOfWeek;
      }, set: function (v) {
        DayOfWeek = v;
      }});
    $mod.DecodeTime(DateTime,{get: function () {
        return Hour;
      }, set: function (v) {
        Hour = v;
      }},{get: function () {
        return Minute;
      }, set: function (v) {
        Minute = v;
      }},{get: function () {
        return Second;
      }, set: function (v) {
        Second = v;
      }},{get: function () {
        return MilliSecond;
      }, set: function (v) {
        MilliSecond = v;
      }});
    if (FormatStr !== "") {
      StoreFormat(FormatStr,0,false)}
     else StoreFormat("C",0,false);
    return Result;
  };
  this.TryStrToDate = function (S, Value) {
    var Result = false;
    Result = $mod.TryStrToDate$2(S,Value,$mod.ShortDateFormat,"\x00");
    return Result;
  };
  this.TryStrToDate$1 = function (S, Value, separator) {
    var Result = false;
    Result = $mod.TryStrToDate$2(S,Value,$mod.ShortDateFormat,separator);
    return Result;
  };
  this.TryStrToDate$2 = function (S, Value, useformat, separator) {
    var Result = false;
    var Msg = "";
    Result = S.length !== 0;
    if (Result) {
      Value.set($impl.IntStrToDate({get: function () {
          return Msg;
        }, set: function (v) {
          Msg = v;
        }},S,useformat,separator));
      Result = Msg === "";
    };
    return Result;
  };
  this.TryStrToTime = function (S, Value) {
    var Result = false;
    Result = $mod.TryStrToTime$1(S,Value,"\x00");
    return Result;
  };
  this.TryStrToTime$1 = function (S, Value, separator) {
    var Result = false;
    var Msg = "";
    Result = S.length !== 0;
    if (Result) {
      Value.set($impl.IntStrToTime({get: function () {
          return Msg;
        }, set: function (v) {
          Msg = v;
        }},S,S.length,separator));
      Result = Msg === "";
    };
    return Result;
  };
  this.TryStrToDateTime = function (S, Value) {
    var Result = false;
    var I = 0;
    var dtdate = 0.0;
    var dttime = 0.0;
    Result = false;
    I = pas.System.Pos($mod.TimeSeparator,S);
    if (I > 0) {
      while ((I > 0) && (S.charAt(I - 1) !== " ")) I -= 1;
      if (I > 0) {
        if (!$mod.TryStrToDate(pas.System.Copy(S,1,I - 1),{get: function () {
            return dtdate;
          }, set: function (v) {
            dtdate = v;
          }})) return Result;
        if (!$mod.TryStrToTime(pas.System.Copy(S,I + 1,S.length - I),{get: function () {
            return dttime;
          }, set: function (v) {
            dttime = v;
          }})) return Result;
        Value.set($mod.ComposeDateTime(dtdate,dttime));
        Result = true;
      } else Result = $mod.TryStrToTime(S,Value);
    } else Result = $mod.TryStrToDate(S,Value);
    return Result;
  };
  this.StrToDateDef = function (S, Defvalue) {
    var Result = 0.0;
    Result = $mod.StrToDateDef$1(S,Defvalue,"\x00");
    return Result;
  };
  this.StrToDateDef$1 = function (S, Defvalue, separator) {
    var Result = 0.0;
    if (!$mod.TryStrToDate$1(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},separator)) Result = Defvalue;
    return Result;
  };
  this.StrToTimeDef = function (S, Defvalue) {
    var Result = 0.0;
    Result = $mod.StrToTimeDef$1(S,Defvalue,"\x00");
    return Result;
  };
  this.StrToTimeDef$1 = function (S, Defvalue, separator) {
    var Result = 0.0;
    if (!$mod.TryStrToTime$1(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},separator)) Result = Defvalue;
    return Result;
  };
  this.StrToDateTimeDef = function (S, Defvalue) {
    var Result = 0.0;
    if (!$mod.TryStrToDateTime(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) Result = Defvalue;
    return Result;
  };
  this.CurrentYear = function () {
    var Result = 0;
    Result = (new Date()).getFullYear();
    return Result;
  };
  this.ReplaceTime = function (dati, NewTime) {
    dati.set($mod.ComposeDateTime(dati.get(),NewTime));
  };
  this.ReplaceDate = function (DateTime, NewDate) {
    var tmp = 0.0;
    tmp = NewDate;
    $mod.ReplaceTime({get: function () {
        return tmp;
      }, set: function (v) {
        tmp = v;
      }},DateTime.get());
    DateTime.set(tmp);
  };
  this.FloatToDateTime = function (Value) {
    var Result = 0.0;
    if ((Value < $mod.MinDateTime) || (Value > $mod.MaxDateTime)) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SInvalidDateTime,[$mod.FloatToStr(Value)]]);
    Result = Value;
    return Result;
  };
  this.CurrencyFormat = 0;
  this.NegCurrFormat = 0;
  this.CurrencyDecimals = 2;
  this.CurrencyString = "$";
  this.FloattoCurr = function (Value) {
    var Result = 0;
    if (!$mod.TryFloatToCurr(Value,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SInvalidCurrency,[$mod.FloatToStr(Value)]]);
    return Result;
  };
  this.TryFloatToCurr = function (Value, AResult) {
    var Result = false;
    Result = ((Value * 10000) >= $mod.MinCurrency) && ((Value * 10000) <= $mod.MaxCurrency);
    if (Result) AResult.set(Math.floor(Value * 10000));
    return Result;
  };
  this.CurrToStr = function (Value) {
    var Result = "";
    Result = $mod.FloatToStrF(Value / 10000,$mod.TFloatFormat.ffGeneral,-1,0);
    return Result;
  };
  this.StrToCurr = function (S) {
    var Result = 0;
    if (!$mod.TryStrToCurr(S,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SInvalidCurrency,[S]]);
    return Result;
  };
  this.TryStrToCurr = function (S, Value) {
    var Result = false;
    var D = 0.0;
    Result = $mod.TryStrToFloat(S,{get: function () {
        return D;
      }, set: function (v) {
        D = v;
      }});
    if (Result) Value.set(Math.floor(D * 10000));
    return Result;
  };
  this.StrToCurrDef = function (S, Default) {
    var Result = 0;
    var R = 0;
    if ($mod.TryStrToCurr(S,{get: function () {
        return R;
      }, set: function (v) {
        R = v;
      }})) {
      Result = R}
     else Result = Default;
    return Result;
  };
  $mod.$rtti.$DynArray("TPathStrArray",{eltype: rtl.string});
  this.ChangeFileExt = function (FileName, Extension) {
    var Result = "";
    var i = 0;
    var EndSep = {};
    var SOF = false;
    i = FileName.length;
    EndSep = rtl.unionSet(rtl.unionSet(pas.System.AllowDirectorySeparators,pas.System.AllowDriveSeparators),rtl.createSet(pas.System.ExtensionSeparator.charCodeAt()));
    while ((i > 0) && !(FileName.charCodeAt(i - 1) in EndSep)) i -= 1;
    if ((i === 0) || (FileName.charAt(i - 1) !== pas.System.ExtensionSeparator)) {
      i = FileName.length + 1}
     else {
      SOF = (i === 1) || (FileName.charCodeAt(i - 1 - 1) in pas.System.AllowDirectorySeparators);
      if (SOF && !pas.System.FirstDotAtFileNameStartIsExtension) i = FileName.length + 1;
    };
    Result = pas.System.Copy(FileName,1,i - 1) + Extension;
    return Result;
  };
  this.ExtractFilePath = function (FileName) {
    var Result = "";
    var i = 0;
    var EndSep = {};
    i = FileName.length;
    EndSep = rtl.unionSet(pas.System.AllowDirectorySeparators,pas.System.AllowDriveSeparators);
    while ((i > 0) && !$impl.CharInSet$1(FileName.charAt(i - 1),EndSep)) i -= 1;
    if (i > 0) {
      Result = pas.System.Copy(FileName,1,i)}
     else Result = "";
    return Result;
  };
  this.ExtractFileDrive = function (FileName) {
    var Result = "";
    var i = 0;
    var l = 0;
    Result = "";
    l = FileName.length;
    if (l < 2) return Result;
    if ($impl.CharInSet$1(FileName.charAt(1),pas.System.AllowDriveSeparators)) {
      Result = pas.System.Copy(FileName,1,2)}
     else if ($impl.CharInSet$1(FileName.charAt(0),pas.System.AllowDirectorySeparators) && $impl.CharInSet$1(FileName.charAt(1),pas.System.AllowDirectorySeparators)) {
      i = 2;
      while ((i < l) && !$impl.CharInSet$1(FileName.charAt((i + 1) - 1),pas.System.AllowDirectorySeparators)) i += 1;
      i += 1;
      while ((i < l) && !$impl.CharInSet$1(FileName.charAt((i + 1) - 1),pas.System.AllowDirectorySeparators)) i += 1;
      Result = pas.System.Copy(FileName,1,i);
    };
    return Result;
  };
  this.ExtractFileName = function (FileName) {
    var Result = "";
    var i = 0;
    var EndSep = {};
    i = FileName.length;
    EndSep = rtl.unionSet(pas.System.AllowDirectorySeparators,pas.System.AllowDriveSeparators);
    while ((i > 0) && !$impl.CharInSet$1(FileName.charAt(i - 1),EndSep)) i -= 1;
    Result = pas.System.Copy(FileName,i + 1,2147483647);
    return Result;
  };
  this.ExtractFileExt = function (FileName) {
    var Result = "";
    var i = 0;
    var EndSep = {};
    var SOF = false;
    Result = "";
    i = FileName.length;
    EndSep = rtl.unionSet(rtl.unionSet(pas.System.AllowDirectorySeparators,pas.System.AllowDriveSeparators),rtl.createSet(pas.System.ExtensionSeparator.charCodeAt()));
    while ((i > 0) && !$impl.CharInSet$1(FileName.charAt(i - 1),EndSep)) i -= 1;
    if ((i > 0) && (FileName.charAt(i - 1) === pas.System.ExtensionSeparator)) {
      SOF = (i === 1) || (FileName.charCodeAt(i - 1 - 1) in pas.System.AllowDirectorySeparators);
      if (!SOF || pas.System.FirstDotAtFileNameStartIsExtension) Result = pas.System.Copy(FileName,i,2147483647);
    } else Result = "";
    return Result;
  };
  this.ExtractFileDir = function (FileName) {
    var Result = "";
    var i = 0;
    var EndSep = {};
    i = FileName.length;
    EndSep = rtl.unionSet(pas.System.AllowDirectorySeparators,pas.System.AllowDriveSeparators);
    while ((i > 0) && !$impl.CharInSet$1(FileName.charAt(i - 1),EndSep)) i -= 1;
    if ((i > 1) && $impl.CharInSet$1(FileName.charAt(i - 1),pas.System.AllowDirectorySeparators) && !$impl.CharInSet$1(FileName.charAt(i - 1 - 1),EndSep)) i -= 1;
    Result = pas.System.Copy(FileName,1,i);
    return Result;
  };
  this.ExtractRelativepath = function (BaseName, DestName) {
    var Result = "";
    var OneLevelBack = "";
    var Source = "";
    var Dest = "";
    var Sc = 0;
    var Dc = 0;
    var I = 0;
    var J = 0;
    var SD = [];
    var DD = [];
    OneLevelBack = ".." + pas.System.PathDelim;
    if ($mod.UpperCase($mod.ExtractFileDrive(BaseName)) !== $mod.UpperCase($mod.ExtractFileDrive(DestName))) {
      Result = DestName;
      return Result;
    };
    Source = $mod.ExcludeTrailingPathDelimiter($mod.ExtractFilePath(BaseName));
    Dest = $mod.ExcludeTrailingPathDelimiter($mod.ExtractFilePath(DestName));
    SD = $mod.GetDirs(Source);
    Sc = rtl.length(SD);
    DD = $mod.GetDirs(Dest);
    Dc = rtl.length(SD);
    I = 0;
    while ((I < Dc) && (I < Sc)) {
      if ($mod.SameText(DD[I],SD[I])) {
        I += 1}
       else break;
    };
    Result = "";
    for (var $l1 = I, $end2 = Sc; $l1 <= $end2; $l1++) {
      J = $l1;
      Result = Result + OneLevelBack;
    };
    for (var $l3 = I, $end4 = Dc; $l3 <= $end4; $l3++) {
      J = $l3;
      Result = Result + DD[J] + pas.System.PathDelim;
    };
    Result = Result + $mod.ExtractFileName(DestName);
    return Result;
  };
  this.IncludeTrailingPathDelimiter = function (Path) {
    var Result = "";
    var l = 0;
    Result = Path;
    l = Result.length;
    if ((l === 0) || !$impl.CharInSet$1(Result.charAt(l - 1),pas.System.AllowDirectorySeparators)) Result = Result + pas.System.PathDelim;
    return Result;
  };
  this.ExcludeTrailingPathDelimiter = function (Path) {
    var Result = "";
    var L = 0;
    L = Path.length;
    if ((L > 0) && $impl.CharInSet$1(Path.charAt(L - 1),pas.System.AllowDirectorySeparators)) L -= 1;
    Result = pas.System.Copy(Path,1,L);
    return Result;
  };
  this.IncludeLeadingPathDelimiter = function (Path) {
    var Result = "";
    var l = 0;
    Result = Path;
    l = Result.length;
    if ((l === 0) || !$impl.CharInSet$1(Result.charAt(0),pas.System.AllowDirectorySeparators)) Result = pas.System.PathDelim + Result;
    return Result;
  };
  this.ExcludeLeadingPathDelimiter = function (Path) {
    var Result = "";
    var L = 0;
    Result = Path;
    L = Result.length;
    if ((L > 0) && $impl.CharInSet$1(Result.charAt(0),pas.System.AllowDirectorySeparators)) pas.System.Delete({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},1,1);
    return Result;
  };
  this.IsPathDelimiter = function (Path, Index) {
    var Result = false;
    Result = (Index > 0) && (Index <= Path.length) && $impl.CharInSet$1(Path.charAt(Index - 1),pas.System.AllowDirectorySeparators);
    return Result;
  };
  this.SetDirSeparators = function (FileName) {
    var Result = "";
    var I = 0;
    Result = FileName;
    for (var $l1 = 1, $end2 = Result.length; $l1 <= $end2; $l1++) {
      I = $l1;
      if ($impl.CharInSet$1(Result.charAt(I - 1),pas.System.AllowDirectorySeparators)) Result = rtl.setCharAt(Result,I - 1,pas.System.PathDelim);
    };
    return Result;
  };
  this.GetDirs = function (DirName) {
    var Result = [];
    var I = 0;
    var J = 0;
    var L = 0;
    var D = "";
    I = 1;
    J = 0;
    L = 0;
    Result = rtl.arraySetLength(Result,"",DirName.length);
    while (I <= DirName.length) {
      if ($impl.CharInSet$1(DirName.charAt(I - 1),pas.System.AllowDirectorySeparators)) {
        D = pas.System.Copy(DirName,J + 1,J - I);
        if (D !== "") {
          Result[L] = D;
          L += 1;
        };
        J = I;
      };
      I += 1;
    };
    Result = rtl.arraySetLength(Result,"",L);
    return Result;
  };
  this.ConcatPaths = function (Paths) {
    var Result = "";
    var I = 0;
    if (rtl.length(Paths) > 0) {
      Result = Paths[0];
      for (var $l1 = 1, $end2 = rtl.length(Paths) - 1; $l1 <= $end2; $l1++) {
        I = $l1;
        Result = $mod.IncludeTrailingPathDelimiter(Result) + $mod.ExcludeLeadingPathDelimiter(Paths[I]);
      };
    } else Result = "";
    return Result;
  };
  this.GUID_NULL = pas.System.TGuid.$clone({D1: 0x00000000, D2: 0x0000, D3: 0x0000, D4: [0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00]});
  this.Supports = function (Instance, AClass, Obj) {
    var Result = false;
    Result = (Instance !== null) && (Instance.QueryInterface(pas.System.IObjectInstance,Obj) === 0) && Obj.get().$class.InheritsFrom(AClass);
    return Result;
  };
  this.Supports$1 = function (Instance, IID, Intf) {
    var Result = false;
    Result = (Instance !== null) && (Instance.QueryInterface(IID,Intf) === 0);
    return Result;
  };
  this.Supports$2 = function (Instance, IID, Intf) {
    var Result = false;
    Result = (Instance !== null) && Instance.GetInterface(IID,Intf);
    return Result;
  };
  this.Supports$3 = function (Instance, IID, Intf) {
    var Result = false;
    Result = (Instance !== null) && Instance.GetInterfaceByStr(IID,Intf);
    return Result;
  };
  this.Supports$4 = function (Instance, AClass) {
    var Result = false;
    var Temp = null;
    Result = $mod.Supports(Instance,AClass,{get: function () {
        return Temp;
      }, set: function (v) {
        Temp = v;
      }});
    return Result;
  };
  this.Supports$5 = function (Instance, IID) {
    var Result = false;
    var Temp = null;
    try {
      Result = $mod.Supports$1(Instance,IID,{get: function () {
          return Temp;
        }, set: function (v) {
          Temp = v;
        }});
    } finally {
      rtl._Release(Temp);
    };
    return Result;
  };
  this.Supports$6 = function (Instance, IID) {
    var Result = false;
    var Temp = null;
    Result = $mod.Supports$2(Instance,IID,{get: function () {
        return Temp;
      }, set: function (v) {
        Temp = v;
      }});
    if (Temp && Temp.$kind==='com') Temp._Release();
    return Result;
  };
  this.Supports$7 = function (Instance, IID) {
    var Result = false;
    var Temp = null;
    Result = $mod.Supports$3(Instance,IID,{get: function () {
        return Temp;
      }, set: function (v) {
        Temp = v;
      }});
    if (Temp && Temp.$kind==='com') Temp._Release();
    return Result;
  };
  this.Supports$8 = function (AClass, IID) {
    var Result = false;
    var maps = undefined;
    if (AClass === null) return false;
    maps = AClass["$intfmaps"];
    if (!maps) return false;
    if (rtl.getObject(maps)[$mod.GUIDToString(IID)]) return true;
    Result = false;
    return Result;
  };
  this.Supports$9 = function (AClass, IID) {
    var Result = false;
    var maps = undefined;
    if (AClass === null) return false;
    maps = AClass["$intfmaps"];
    if (!maps) return false;
    if (rtl.getObject(maps)[$mod.UpperCase(IID)]) return true;
    Result = false;
    return Result;
  };
  this.TryStringToGUID = function (s, Guid) {
    var Result = false;
    var re = null;
    if (s.length !== 38) return false;
    re = new RegExp("^\\{[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}\\}$");
    Result = re.test(s);
    if (!Result) {
      Guid.D1 = 0;
      return Result;
    };
    rtl.strToGUIDR(s,Guid);
    Result = true;
    return Result;
  };
  this.StringToGUID = function (S) {
    var Result = pas.System.TGuid.$new();
    if (!$mod.TryStringToGUID(S,Result)) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SInvalidGUID,[S]]);
    return Result;
  };
  this.GUIDToString = function (guid) {
    var Result = "";
    Result = rtl.guidrToStr(guid);
    return Result;
  };
  this.IsEqualGUID = function (guid1, guid2) {
    var Result = false;
    var i = 0;
    if ((guid1.D1 !== guid2.D1) || (guid1.D2 !== guid2.D2) || (guid1.D3 !== guid2.D3)) return false;
    for (i = 0; i <= 7; i++) if (guid1.D4[i] !== guid2.D4[i]) return false;
    Result = true;
    return Result;
  };
  this.GuidCase = function (guid, List) {
    var Result = 0;
    for (var $l1 = rtl.length(List) - 1; $l1 >= 0; $l1--) {
      Result = $l1;
      if ($mod.IsEqualGUID(guid,List[Result])) return Result;
    };
    Result = -1;
    return Result;
  };
  this.CreateGUID = function (GUID) {
    var Result = 0;
    function R(B) {
      var Result = 0;
      var v = 0;
      v = pas.System.Random(256);
      while (B > 1) {
        v = (v * 256) + pas.System.Random(256);
        B -= 1;
      };
      Result = v;
      return Result;
    };
    var I = 0;
    Result = 0;
    GUID.D1 = R(4);
    GUID.D2 = R(2);
    GUID.D3 = R(2);
    for (I = 0; I <= 7; I++) GUID.D4[I] = R(1);
    return Result;
  };
  $mod.$init = function () {
    $mod.FormatSettings = $mod.TFormatSettings.$create("Create");
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.SAbortError = "Operation aborted";
  $impl.CharInSet$1 = function (Ch, CSet) {
    var Result = false;
    Result = Ch.charCodeAt() in CSet;
    return Result;
  };
  $impl.CheckBoolStrs = function () {
    if (rtl.length($mod.TrueBoolStrs) === 0) {
      $mod.TrueBoolStrs = rtl.arraySetLength($mod.TrueBoolStrs,"",1);
      $mod.TrueBoolStrs[0] = "True";
    };
    if (rtl.length($mod.FalseBoolStrs) === 0) {
      $mod.FalseBoolStrs = rtl.arraySetLength($mod.FalseBoolStrs,"",1);
      $mod.FalseBoolStrs[0] = "False";
    };
  };
  $impl.feInvalidFormat = 1;
  $impl.feMissingArgument = 2;
  $impl.feInvalidArgIndex = 3;
  $impl.DoFormatError = function (ErrCode, fmt) {
    var $tmp1 = ErrCode;
    if ($tmp1 === 1) {
      throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SInvalidFormat,[fmt]])}
     else if ($tmp1 === 2) {
      throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SArgumentMissing,[fmt]])}
     else if ($tmp1 === 3) throw $mod.EConvertError.$create("CreateFmt",[pas.RTLConsts.SInvalidArgIndex,[fmt]]);
  };
  $impl.maxdigits = 15;
  $impl.ReplaceDecimalSep = function (S, DS) {
    var Result = "";
    var P = 0;
    P = pas.System.Pos(".",S);
    if (P > 0) {
      Result = pas.System.Copy(S,1,P - 1) + DS + pas.System.Copy(S,P + 1,S.length - P)}
     else Result = S;
    return Result;
  };
  $impl.FormatGeneralFloat = function (Value, Precision, DS) {
    var Result = "";
    var P = 0;
    var PE = 0;
    var Q = 0;
    var Exponent = 0;
    if ((Precision === -1) || (Precision > 15)) Precision = 15;
    Result = rtl.floatToStr(Value,Precision + 7);
    Result = $mod.TrimLeft(Result);
    P = pas.System.Pos(".",Result);
    if (P === 0) return Result;
    PE = pas.System.Pos("E",Result);
    if (PE === 0) {
      Result = $impl.ReplaceDecimalSep(Result,DS);
      return Result;
    };
    Q = PE + 2;
    Exponent = 0;
    while (Q <= Result.length) {
      Exponent = ((Exponent * 10) + Result.charCodeAt(Q - 1)) - "0".charCodeAt();
      Q += 1;
    };
    if (Result.charAt((PE + 1) - 1) === "-") Exponent = -Exponent;
    if (((P + Exponent) < PE) && (Exponent > -6)) {
      Result = rtl.strSetLength(Result,PE - 1);
      if (Exponent >= 0) {
        for (var $l1 = 0, $end2 = Exponent - 1; $l1 <= $end2; $l1++) {
          Q = $l1;
          Result = rtl.setCharAt(Result,P - 1,Result.charAt((P + 1) - 1));
          P += 1;
        };
        Result = rtl.setCharAt(Result,P - 1,".");
        P = 1;
        if (Result.charAt(P - 1) === "-") P += 1;
        while ((Result.charAt(P - 1) === "0") && (P < Result.length) && (pas.System.Copy(Result,P + 1,DS.length) !== DS)) pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},P,1);
      } else {
        pas.System.Insert(pas.System.Copy("00000",1,-Exponent),{get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},P - 1);
        Result = rtl.setCharAt(Result,P - Exponent - 1,Result.charAt(P - Exponent - 1 - 1));
        Result = rtl.setCharAt(Result,P - 1,".");
        if (Exponent !== -1) Result = rtl.setCharAt(Result,P - Exponent - 1 - 1,"0");
      };
      Q = Result.length;
      while ((Q > 0) && (Result.charAt(Q - 1) === "0")) Q -= 1;
      if (Result.charAt(Q - 1) === ".") Q -= 1;
      if ((Q === 0) || ((Q === 1) && (Result.charAt(0) === "-"))) {
        Result = "0"}
       else Result = rtl.strSetLength(Result,Q);
    } else {
      while (Result.charAt(PE - 1 - 1) === "0") {
        pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},PE - 1,1);
        PE -= 1;
      };
      if (Result.charAt(PE - 1 - 1) === DS) {
        pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},PE - 1,1);
        PE -= 1;
      };
      if (Result.charAt((PE + 1) - 1) === "+") {
        pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},PE + 1,1)}
       else PE += 1;
      while (Result.charAt((PE + 1) - 1) === "0") pas.System.Delete({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},PE + 1,1);
    };
    Result = $impl.ReplaceDecimalSep(Result,DS);
    return Result;
  };
  $impl.FormatExponentFloat = function (Value, Precision, Digits, DS) {
    var Result = "";
    var P = 0;
    DS = $mod.DecimalSeparator;
    if ((Precision === -1) || (Precision > 15)) Precision = 15;
    Result = rtl.floatToStr(Value,Precision + 7);
    while (Result.charAt(0) === " ") pas.System.Delete({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},1,1);
    P = pas.System.Pos("E",Result);
    if (P === 0) {
      Result = $impl.ReplaceDecimalSep(Result,DS);
      return Result;
    };
    P += 2;
    if (Digits > 4) Digits = 4;
    Digits = (Result.length - P - Digits) + 1;
    if (Digits < 0) {
      pas.System.Insert(pas.System.Copy("0000",1,-Digits),{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},P)}
     else while ((Digits > 0) && (Result.charAt(P - 1) === "0")) {
      pas.System.Delete({get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},P,1);
      if (P > Result.length) {
        pas.System.Delete({get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},P - 2,2);
        break;
      };
      Digits -= 1;
    };
    Result = $impl.ReplaceDecimalSep(Result,DS);
    return Result;
  };
  $impl.FormatFixedFloat = function (Value, Digits, DS) {
    var Result = "";
    if (Digits === -1) {
      Digits = 2}
     else if (Digits > 18) Digits = 18;
    Result = rtl.floatToStr(Value,0,Digits);
    if ((Result !== "") && (Result.charAt(0) === " ")) pas.System.Delete({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},1,1);
    Result = $impl.ReplaceDecimalSep(Result,DS);
    return Result;
  };
  $impl.FormatNumberFloat = function (Value, Digits, DS, TS) {
    var Result = "";
    var P = 0;
    if (Digits === -1) {
      Digits = 2}
     else if (Digits > 15) Digits = 15;
    Result = rtl.floatToStr(Value,0,Digits);
    if ((Result !== "") && (Result.charAt(0) === " ")) pas.System.Delete({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},1,1);
    P = pas.System.Pos(".",Result);
    Result = $impl.ReplaceDecimalSep(Result,DS);
    P -= 3;
    if ((TS !== "") && (TS !== "\x00")) while (P > 1) {
      if (Result.charAt(P - 1 - 1) !== "-") pas.System.Insert(TS,{get: function () {
          return Result;
        }, set: function (v) {
          Result = v;
        }},P);
      P -= 3;
    };
    return Result;
  };
  $impl.RemoveLeadingNegativeSign = function (AValue, DS) {
    var Result = false;
    var i = 0;
    var TS = "";
    var StartPos = 0;
    Result = false;
    StartPos = 2;
    TS = $mod.ThousandSeparator;
    for (var $l1 = StartPos, $end2 = AValue.get().length; $l1 <= $end2; $l1++) {
      i = $l1;
      Result = (AValue.get().charCodeAt(i - 1) in rtl.createSet(48,DS.charCodeAt(),69,43)) || (AValue.get().charAt(i - 1) === TS);
      if (!Result) break;
    };
    if (Result && (AValue.get().charAt(0) === "-")) pas.System.Delete(AValue,1,1);
    return Result;
  };
  $impl.FormatNumberCurrency = function (Value, Digits, DS, TS) {
    var Result = "";
    var Negative = false;
    var P = 0;
    if (Digits === -1) {
      Digits = $mod.CurrencyDecimals}
     else if (Digits > 18) Digits = 18;
    Result = rtl.floatToStr(Value / 10000,0,Digits);
    Negative = Result.charAt(0) === "-";
    if (Negative) pas.System.Delete({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},1,1);
    P = pas.System.Pos(".",Result);
    if (TS !== "") {
      if (P !== 0) {
        Result = $impl.ReplaceDecimalSep(Result,DS)}
       else P = Result.length + 1;
      P -= 3;
      while (P > 1) {
        pas.System.Insert(TS,{get: function () {
            return Result;
          }, set: function (v) {
            Result = v;
          }},P);
        P -= 3;
      };
    };
    if (Negative) $impl.RemoveLeadingNegativeSign({get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }},DS);
    if (!Negative) {
      var $tmp1 = $mod.CurrencyFormat;
      if ($tmp1 === 0) {
        Result = $mod.CurrencyString + Result}
       else if ($tmp1 === 1) {
        Result = Result + $mod.CurrencyString}
       else if ($tmp1 === 2) {
        Result = $mod.CurrencyString + " " + Result}
       else if ($tmp1 === 3) Result = Result + " " + $mod.CurrencyString;
    } else {
      var $tmp2 = $mod.NegCurrFormat;
      if ($tmp2 === 0) {
        Result = "(" + $mod.CurrencyString + Result + ")"}
       else if ($tmp2 === 1) {
        Result = "-" + $mod.CurrencyString + Result}
       else if ($tmp2 === 2) {
        Result = $mod.CurrencyString + "-" + Result}
       else if ($tmp2 === 3) {
        Result = $mod.CurrencyString + Result + "-"}
       else if ($tmp2 === 4) {
        Result = "(" + Result + $mod.CurrencyString + ")"}
       else if ($tmp2 === 5) {
        Result = "-" + Result + $mod.CurrencyString}
       else if ($tmp2 === 6) {
        Result = Result + "-" + $mod.CurrencyString}
       else if ($tmp2 === 7) {
        Result = Result + $mod.CurrencyString + "-"}
       else if ($tmp2 === 8) {
        Result = "-" + Result + " " + $mod.CurrencyString}
       else if ($tmp2 === 9) {
        Result = "-" + $mod.CurrencyString + " " + Result}
       else if ($tmp2 === 10) {
        Result = Result + " " + $mod.CurrencyString + "-"}
       else if ($tmp2 === 11) {
        Result = $mod.CurrencyString + " " + Result + "-"}
       else if ($tmp2 === 12) {
        Result = $mod.CurrencyString + " " + "-" + Result}
       else if ($tmp2 === 13) {
        Result = Result + "-" + " " + $mod.CurrencyString}
       else if ($tmp2 === 14) {
        Result = "(" + $mod.CurrencyString + " " + Result + ")"}
       else if ($tmp2 === 15) Result = "(" + Result + " " + $mod.CurrencyString + ")";
    };
    return Result;
  };
  $impl.RESpecials = "([\\[\\]\\(\\)\\\\\\.\\*])";
  $impl.DoEncodeDate = function (Year, Month, Day) {
    var Result = 0;
    var D = 0.0;
    if ($mod.TryEncodeDate(Year,Month,Day,{get: function () {
        return D;
      }, set: function (v) {
        D = v;
      }})) {
      Result = pas.System.Trunc(D)}
     else Result = 0;
    return Result;
  };
  $impl.DoEncodeTime = function (Hour, Minute, Second, MilliSecond) {
    var Result = 0.0;
    if (!$mod.TryEncodeTime(Hour,Minute,Second,MilliSecond,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) Result = 0;
    return Result;
  };
  $impl.DateTimeToStrFormat = ["c","f"];
  var WhiteSpace = " \b\t\n\f\r";
  var Digits = "0123456789";
  $impl.IntStrToDate = function (ErrorMsg, S, useformat, separator) {
    var Result = 0.0;
    function FixErrorMsg(errmarg) {
      ErrorMsg.set($mod.Format(pas.RTLConsts.SInvalidDateFormat,[errmarg]));
    };
    var df = "";
    var d = 0;
    var m = 0;
    var y = 0;
    var ly = 0;
    var ld = 0;
    var lm = 0;
    var n = 0;
    var i = 0;
    var len = 0;
    var c = 0;
    var dp = 0;
    var mp = 0;
    var yp = 0;
    var which = 0;
    var s1 = "";
    var values = [];
    var YearMoreThenTwoDigits = false;
    values = rtl.arraySetLength(values,0,4);
    Result = 0;
    len = S.length;
    ErrorMsg.set("");
    while ((len > 0) && (pas.System.Pos(S.charAt(len - 1),WhiteSpace) > 0)) len -= 1;
    if (len === 0) {
      FixErrorMsg(S);
      return Result;
    };
    YearMoreThenTwoDigits = false;
    if (separator === "\x00") if ($mod.DateSeparator !== "\x00") {
      separator = $mod.DateSeparator}
     else separator = "-";
    df = $mod.UpperCase(useformat);
    yp = 0;
    mp = 0;
    dp = 0;
    which = 0;
    i = 0;
    while ((i < df.length) && (which < 3)) {
      i += 1;
      var $tmp1 = df.charAt(i - 1);
      if ($tmp1 === "Y") {
        if (yp === 0) {
          which += 1;
          yp = which;
        }}
       else if ($tmp1 === "M") {
        if (mp === 0) {
          which += 1;
          mp = which;
        }}
       else if ($tmp1 === "D") if (dp === 0) {
        which += 1;
        dp = which;
      };
    };
    for (i = 1; i <= 3; i++) values[i] = 0;
    s1 = "";
    n = 0;
    for (var $l2 = 1, $end3 = len; $l2 <= $end3; $l2++) {
      i = $l2;
      if (pas.System.Pos(S.charAt(i - 1),Digits) > 0) s1 = s1 + S.charAt(i - 1);
      if ((separator !== " ") && (S.charAt(i - 1) === " ")) continue;
      if ((S.charAt(i - 1) === separator) || ((i === len) && (pas.System.Pos(S.charAt(i - 1),Digits) > 0))) {
        n += 1;
        if (n > 3) {
          FixErrorMsg(S);
          return Result;
        };
        if ((n === yp) && (s1.length > 2)) YearMoreThenTwoDigits = true;
        pas.System.val$6(s1,{a: n, p: values, get: function () {
            return this.p[this.a];
          }, set: function (v) {
            this.p[this.a] = v;
          }},{get: function () {
            return c;
          }, set: function (v) {
            c = v;
          }});
        if (c !== 0) {
          FixErrorMsg(S);
          return Result;
        };
        s1 = "";
      } else if (pas.System.Pos(S.charAt(i - 1),Digits) === 0) {
        FixErrorMsg(S);
        return Result;
      };
    };
    if ((which < 3) && (n > which)) {
      FixErrorMsg(S);
      return Result;
    };
    $mod.DecodeDate($mod.Date(),{get: function () {
        return ly;
      }, set: function (v) {
        ly = v;
      }},{get: function () {
        return lm;
      }, set: function (v) {
        lm = v;
      }},{get: function () {
        return ld;
      }, set: function (v) {
        ld = v;
      }});
    if (n === 3) {
      y = values[yp];
      m = values[mp];
      d = values[dp];
    } else {
      y = ly;
      if (n < 2) {
        d = values[1];
        m = lm;
      } else if (dp < mp) {
        d = values[1];
        m = values[2];
      } else {
        d = values[2];
        m = values[1];
      };
    };
    if ((y >= 0) && (y < 100) && !YearMoreThenTwoDigits) {
      ly = ly - $mod.TwoDigitYearCenturyWindow;
      y += Math.floor(ly / 100) * 100;
      if (($mod.TwoDigitYearCenturyWindow > 0) && (y < ly)) y += 100;
    };
    if (!$mod.TryEncodeDate(y,m,d,{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) ErrorMsg.set(pas.RTLConsts.SErrInvalidDate);
    return Result;
  };
  var AMPM_None = 0;
  var AMPM_AM = 1;
  var AMPM_PM = 2;
  var tiHour = 0;
  var tiMin = 1;
  var tiSec = 2;
  var tiMSec = 3;
  var Digits$1 = "0123456789";
  $impl.IntStrToTime = function (ErrorMsg, S, Len, separator) {
    var Result = 0.0;
    var AmPm = 0;
    var TimeValues = [];
    function SplitElements(TimeValues, AmPm) {
      var Result = false;
      var Cur = 0;
      var Offset = 0;
      var ElemLen = 0;
      var Err = 0;
      var TimeIndex = 0;
      var FirstSignificantDigit = 0;
      var Value = 0;
      var DigitPending = false;
      var MSecPending = false;
      var AmPmStr = "";
      var CurChar = "";
      var I = 0;
      var allowedchars = "";
      Result = false;
      AmPm.set(0);
      MSecPending = false;
      TimeIndex = 0;
      for (I = 0; I <= 3; I++) TimeValues.get()[I] = 0;
      Cur = 1;
      while ((Cur < Len) && (S.charAt(Cur - 1) === " ")) Cur += 1;
      Offset = Cur;
      if ((Cur > (Len - 1)) || (S.charAt(Cur - 1) === separator) || (S.charAt(Cur - 1) === $mod.DecimalSeparator)) {
        return Result;
      };
      DigitPending = pas.System.Pos(S.charAt(Cur - 1),Digits$1) > 0;
      while (Cur <= Len) {
        CurChar = S.charAt(Cur - 1);
        if (pas.System.Pos(CurChar,Digits$1) > 0) {
          if (!DigitPending || (TimeIndex > 3)) {
            return Result;
          };
          Offset = Cur;
          if (CurChar !== "0") {
            FirstSignificantDigit = Offset}
           else FirstSignificantDigit = -1;
          while ((Cur < Len) && (pas.System.Pos(S.charAt((Cur + 1) - 1),Digits$1) > 0)) {
            if ((FirstSignificantDigit === -1) && (S.charAt(Cur - 1) !== "0")) FirstSignificantDigit = Cur;
            Cur += 1;
          };
          if (FirstSignificantDigit === -1) FirstSignificantDigit = Cur;
          ElemLen = (1 + Cur) - FirstSignificantDigit;
          if ((ElemLen <= 2) || ((ElemLen <= 3) && (TimeIndex === 3))) {
            pas.System.val$6(pas.System.Copy(S,FirstSignificantDigit,ElemLen),{get: function () {
                return Value;
              }, set: function (v) {
                Value = v;
              }},{get: function () {
                return Err;
              }, set: function (v) {
                Err = v;
              }});
            TimeValues.get()[TimeIndex] = Value;
            TimeIndex += 1;
            DigitPending = false;
          } else {
            return Result;
          };
        } else if (CurChar === " ") {}
        else if (CurChar === separator) {
          if (DigitPending || (TimeIndex > 2)) {
            return Result;
          };
          DigitPending = true;
          MSecPending = false;
        } else if (CurChar === $mod.DecimalSeparator) {
          if (DigitPending || MSecPending || (TimeIndex !== 3)) {
            return Result;
          };
          DigitPending = true;
          MSecPending = true;
        } else {
          if ((AmPm.get() !== 0) || DigitPending) {
            return Result;
          };
          Offset = Cur;
          allowedchars = $mod.DecimalSeparator + " ";
          if (separator !== "\x00") allowedchars = allowedchars + separator;
          while ((Cur < (Len - 1)) && (pas.System.Pos(S.charAt((Cur + 1) - 1),allowedchars) === 0) && (pas.System.Pos(S.charAt((Cur + 1) - 1),Digits$1) === 0)) Cur += 1;
          ElemLen = (1 + Cur) - Offset;
          AmPmStr = pas.System.Copy(S,1 + Offset,ElemLen);
          if ($mod.CompareText(AmPmStr,$mod.TimeAMString) === 0) {
            AmPm.set(1)}
           else if ($mod.CompareText(AmPmStr,$mod.TimePMString) === 0) {
            AmPm.set(2)}
           else if ($mod.CompareText(AmPmStr,"AM") === 0) {
            AmPm.set(1)}
           else if ($mod.CompareText(AmPmStr,"PM") === 0) {
            AmPm.set(2)}
           else {
            return Result;
          };
          if (TimeIndex === 0) {
            DigitPending = true;
          } else {
            TimeIndex = 3 + 1;
            DigitPending = false;
          };
        };
        Cur += 1;
      };
      if ((TimeIndex === 0) || ((AmPm.get() !== 0) && ((TimeValues.get()[0] > 12) || (TimeValues.get()[0] === 0))) || DigitPending) return Result;
      Result = true;
      return Result;
    };
    TimeValues = rtl.arraySetLength(TimeValues,0,4);
    if (separator === "\x00") if ($mod.TimeSeparator !== "\x00") {
      separator = $mod.TimeSeparator}
     else separator = ":";
    AmPm = 0;
    if (!SplitElements({get: function () {
        return TimeValues;
      }, set: function (v) {
        TimeValues = v;
      }},{get: function () {
        return AmPm;
      }, set: function (v) {
        AmPm = v;
      }})) {
      ErrorMsg.set($mod.Format(pas.RTLConsts.SErrInvalidTimeFormat,[S]));
      return Result;
    };
    if ((AmPm === 2) && (TimeValues[0] !== 12)) {
      TimeValues[0] += 12}
     else if ((AmPm === 1) && (TimeValues[0] === 12)) TimeValues[0] = 0;
    if (!$mod.TryEncodeTime(TimeValues[0],TimeValues[1],TimeValues[2],TimeValues[3],{get: function () {
        return Result;
      }, set: function (v) {
        Result = v;
      }})) ErrorMsg.set($mod.Format(pas.RTLConsts.SErrInvalidTimeFormat,[S]));
    return Result;
  };
  var WhiteSpace$1 = "\t\n\r ";
  $impl.SplitDateTimeStr = function (DateTimeStr, DateStr, TimeStr) {
    var Result = 0;
    var p = 0;
    var DummyDT = 0.0;
    Result = 0;
    DateStr.set("");
    TimeStr.set("");
    DateTimeStr = $mod.Trim(DateTimeStr);
    if (DateTimeStr.length === 0) return Result;
    if (($mod.DateSeparator === " ") && ($mod.TimeSeparator === " ") && (pas.System.Pos(" ",DateTimeStr) > 0)) {
      DateStr.set(DateTimeStr);
      return 1;
    };
    p = 1;
    if ($mod.DateSeparator !== " ") {
      while ((p < DateTimeStr.length) && !(pas.System.Pos(DateTimeStr.charAt((p + 1) - 1),WhiteSpace$1) > 0)) p += 1;
    } else {
      p = pas.System.Pos($mod.TimeSeparator,DateTimeStr);
      if (p !== 0) do {
        p -= 1;
      } while (!((p === 0) || (pas.System.Pos(DateTimeStr.charAt(p - 1),WhiteSpace$1) > 0)));
    };
    if (p === 0) p = DateTimeStr.length;
    DateStr.set(pas.System.Copy(DateTimeStr,1,p));
    TimeStr.set($mod.Trim(pas.System.Copy(DateTimeStr,p + 1,100)));
    if (TimeStr.get().length !== 0) {
      Result = 2}
     else {
      Result = 1;
      if ((($mod.DateSeparator !== $mod.TimeSeparator) && (pas.System.Pos($mod.TimeSeparator,DateStr.get()) > 0)) || (($mod.DateSeparator === $mod.TimeSeparator) && !$mod.TryStrToDate(DateStr.get(),{get: function () {
          return DummyDT;
        }, set: function (v) {
          DummyDT = v;
        }}))) {
        TimeStr.set(DateStr.get());
        DateStr.set("");
      };
    };
    return Result;
  };
});
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
rtl.module("webgl",["System","SysUtils","JS","Web"],function () {
  "use strict";
  var $mod = this;
  $mod.$rtti.$Class("TJSWebGLContextAttributes");
  $mod.$rtti.$Class("TJSWebGLContextEventInit");
  rtl.createClassExt($mod,"TJSWebGLContextAttributes",Object,"",function () {
    this.$init = function () {
      this.alpha = false;
      this.depth = false;
      this.stencil = false;
      this.antialias = false;
      this.premultipliedAlpha = false;
      this.preserveDrawingBuffer = false;
      this.powerPreference = "";
      this.failIfMajorPerformanceCaveat = false;
    };
    this.$final = function () {
    };
  });
  rtl.createClassExt($mod,"TJSWebGLContextEventInit",Object,"",function () {
    this.$init = function () {
      this.statusMessage = "";
    };
    this.$final = function () {
    };
  });
  $mod.$rtti.$DynArray("TGLfloatDynArray",{eltype: rtl.double});
  $mod.$rtti.$DynArray("TGLintDynArray",{eltype: rtl.longint});
});
rtl.module("Math",["System","SysUtils"],function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  this.MinInteger = -0xfffffffffffff - 1;
  this.MaxInteger = 0xfffffffffffff;
  this.MinDouble = 5.0e-324;
  this.MaxDouble = 1.7e+308;
  this.InRange = function (AValue, AMin, AMax) {
    return (AValue >= AMin) && (AValue <= AMax);
  };
  this.InRange$1 = function (AValue, AMin, AMax) {
    return (AValue >= AMin) && (AValue <= AMax);
  };
  this.EnsureRange = function (AValue, AMin, AMax) {
    if (AValue<AMin){ return AMin;
    } else if (AValue>AMax){ return AMax;
    } else return AValue;
  };
  this.EnsureRange$1 = function (AValue, AMin, AMax) {
    if (AValue<AMin){ return AMin;
    } else if (AValue>AMax){ return AMax;
    } else return AValue;
  };
  $mod.$rtti.$Int("TRoundToRange",{minvalue: -37, maxvalue: 37, ordtype: 0});
  this.RoundTo = function (AValue, Digits) {
    var Result = 0.0;
    var RV = 0.0;
    RV = $mod.IntPower(10,Digits);
    Result = Math.round(AValue / RV) * RV;
    return Result;
  };
  this.SimpleRoundTo = function (AValue, Digits) {
    var Result = 0.0;
    var RV = 0.0;
    RV = $mod.IntPower(10,-Digits);
    if (AValue < 0) {
      Result = pas.System.Int((AValue * RV) - 0.5) / RV}
     else Result = pas.System.Int((AValue * RV) + 0.5) / RV;
    return Result;
  };
  this.randg = function (mean, stddev) {
    var Result = 0.0;
    var U1 = 0.0;
    var S2 = 0.0;
    do {
      U1 = (2 * Math.random()) - 1;
      S2 = pas.System.Sqr$1(U1) + pas.System.Sqr$1((2 * Math.random()) - 1);
    } while (!(S2 < 1));
    Result = (Math.sqrt((-2 * Math.log(S2)) / S2) * U1 * stddev) + mean;
    return Result;
  };
  this.RandomRange = function (aFrom, aTo) {
    var Result = 0;
    Result = pas.System.Random(Math.abs(aFrom - aTo)) + Math.min(aTo,aFrom);
    return Result;
  };
  this.RandomRange$1 = function (aFrom, aTo) {
    var Result = 0;
    var m = 0;
    if (aFrom < aTo) {
      m = aFrom}
     else m = aTo;
    Result = pas.System.Random(Math.abs(aFrom - aTo)) + m;
    return Result;
  };
  this.NegativeValue = -1;
  this.ZeroValue = 0;
  this.PositiveValue = 1;
  this.IsZero = function (d, Epsilon) {
    var Result = false;
    if (Epsilon === 0) Epsilon = 1E-12;
    Result = Math.abs(d) <= Epsilon;
    return Result;
  };
  this.IsZero$1 = function (d) {
    var Result = false;
    Result = Math.abs(d) <= 1E-12;
    return Result;
  };
  this.IsInfinite = function (d) {
    return (d==Infinity) || (d==-Infinity);
  };
  this.SameValue = function (A, B, Epsilon) {
    var Result = false;
    if (Epsilon === 0.0) Epsilon = Math.max(Math.min(Math.abs(A),Math.abs(B)) * 1E-12,1E-12);
    if (A > B) {
      Result = (A - B) <= Epsilon}
     else Result = (B - A) <= Epsilon;
    return Result;
  };
  this.LogN = function (A, Base) {
    var Result = 0.0;
    Result = Math.log(A) / Math.log(Base);
    return Result;
  };
  this.Ceil = function (A) {
    var Result = 0;
    Result = pas.System.Trunc(Math.ceil(A));
    return Result;
  };
  this.Floor = function (A) {
    var Result = 0;
    Result = pas.System.Trunc(Math.floor(A));
    return Result;
  };
  this.Ceil64 = function (A) {
    var Result = 0;
    Result = pas.System.Trunc(Math.ceil(A));
    return Result;
  };
  this.Floor64 = function (A) {
    var Result = 0;
    Result = pas.System.Trunc(Math.ceil(A));
    return Result;
  };
  this.ldexp = function (x, p) {
    var Result = 0.0;
    Result = x * $mod.IntPower(2.0,p);
    return Result;
  };
  this.Frexp = function (X, Mantissa, Exponent) {
    Exponent.set(0);
    if (X !== 0) if (Math.abs(X) < 0.5) {
      do {
        X = X * 2;
        Exponent.set(Exponent.get() - 1);
      } while (!(Math.abs(X) >= 0.5))}
     else while (Math.abs(X) >= 1) {
      X = X / 2;
      Exponent.set(Exponent.get() + 1);
    };
    Mantissa.set(X);
  };
  this.lnxp1 = function (x) {
    var Result = 0.0;
    var y = 0.0;
    if (x >= 4.0) {
      Result = Math.log(1.0 + x)}
     else {
      y = 1.0 + x;
      if (y === 1.0) {
        Result = x}
       else {
        Result = Math.log(y);
        if (y > 0.0) Result = Result + ((x - (y - 1.0)) / y);
      };
    };
    return Result;
  };
  this.IntPower = function (base, exponent) {
    var Result = 0.0;
    var i = 0;
    if ((base === 0.0) && (exponent === 0)) {
      Result = 1}
     else {
      i = Math.abs(exponent);
      Result = 1.0;
      while (i > 0) {
        while ((i & 1) === 0) {
          i = i >>> 1;
          base = pas.System.Sqr$1(base);
        };
        i = i - 1;
        Result = Result * base;
      };
      if (exponent < 0) Result = 1.0 / Result;
    };
    return Result;
  };
  this.DivMod = function (Dividend, Divisor, Result, Remainder) {
    if (Dividend < 0) {
      Dividend = -Dividend;
      Result.set(-Math.floor(Dividend / Divisor));
      Remainder.set(-(Dividend + (Result.get() * Divisor)));
    } else {
      Result.set(Math.floor(Dividend / Divisor));
      Remainder.set(Dividend - (Result.get() * Divisor));
    };
  };
  this.DivMod$1 = function (Dividend, Divisor, Result, Remainder) {
    if (Dividend < 0) {
      Dividend = -Dividend;
      Result.set(-Math.floor(Dividend / Divisor));
      Remainder.set(-(Dividend + (Result.get() * Divisor)));
    } else {
      Result.set(Math.floor(Dividend / Divisor));
      Remainder.set(Dividend - (Result.get() * Divisor));
    };
  };
  this.DivMod$2 = function (Dividend, Divisor, Result, Remainder) {
    Result.set(Math.floor(Dividend / Divisor));
    Remainder.set(Dividend - (Result.get() * Divisor));
  };
  this.DivMod$3 = function (Dividend, Divisor, Result, Remainder) {
    if (Dividend < 0) {
      Dividend = -Dividend;
      Result.set(-Math.floor(Dividend / Divisor));
      Remainder.set(-(Dividend + (Result.get() * Divisor)));
    } else {
      Result.set(Math.floor(Dividend / Divisor));
      Remainder.set(Dividend - (Result.get() * Divisor));
    };
  };
  this.DegToRad = function (deg) {
    var Result = 0.0;
    Result = deg * (Math.PI / 180.0);
    return Result;
  };
  this.RadToDeg = function (rad) {
    var Result = 0.0;
    Result = rad * (180.0 / Math.PI);
    return Result;
  };
  this.GradToRad = function (grad) {
    var Result = 0.0;
    Result = grad * (Math.PI / 200.0);
    return Result;
  };
  this.RadToGrad = function (rad) {
    var Result = 0.0;
    Result = rad * (200.0 / Math.PI);
    return Result;
  };
  this.DegToGrad = function (deg) {
    var Result = 0.0;
    Result = deg * (200.0 / 180.0);
    return Result;
  };
  this.GradToDeg = function (grad) {
    var Result = 0.0;
    Result = grad * (180.0 / 200.0);
    return Result;
  };
  this.CycleToRad = function (cycle) {
    var Result = 0.0;
    Result = 2 * Math.PI * cycle;
    return Result;
  };
  this.RadToCycle = function (rad) {
    var Result = 0.0;
    Result = rad * (1 / (2 * Math.PI));
    return Result;
  };
  this.DegNormalize = function (deg) {
    var Result = 0.0;
    Result = deg - (pas.System.Int(deg / 360) * 360);
    if (Result < 0) Result = Result + 360;
    return Result;
  };
  this.Norm = function (data) {
    var Result = 0.0;
    Result = Math.sqrt($impl.sumofsquares(data));
    return Result;
  };
  this.Mean = function (data) {
    var Result = 0.0;
    var N = 0;
    N = rtl.length(data);
    if (N === 0) {
      Result = 0}
     else Result = $mod.Sum(data) / N;
    return Result;
  };
  this.Sum = function (data) {
    var Result = 0.0;
    var i = 0;
    var N = 0;
    N = rtl.length(data);
    Result = 0.0;
    for (var $l1 = 0, $end2 = N - 1; $l1 <= $end2; $l1++) {
      i = $l1;
      Result = Result + data[i];
    };
    return Result;
  };
  this.SumsAndSquares = function (data, Sum, SumOfSquares) {
    var i = 0;
    var n = 0;
    var t = 0.0;
    var s = 0.0;
    var ss = 0.0;
    n = rtl.length(data);
    ss = 0.0;
    s = 0.0;
    for (var $l1 = 0, $end2 = n - 1; $l1 <= $end2; $l1++) {
      i = $l1;
      t = data[i];
      ss = ss + pas.System.Sqr$1(t);
      s = s + t;
    };
    Sum.set(s);
    SumOfSquares.set(ss);
  };
  this.StdDev = function (data) {
    var Result = 0.0;
    Result = Math.sqrt($mod.Variance(data));
    return Result;
  };
  this.MeanAndStdDev = function (data, Mean, StdDev) {
    var I = 0;
    var N = 0;
    var M = 0.0;
    var S = 0.0;
    N = rtl.length(data);
    M = 0;
    S = 0;
    for (var $l1 = 0, $end2 = N - 1; $l1 <= $end2; $l1++) {
      I = $l1;
      M = M + data[I];
      S = S + pas.System.Sqr$1(data[I]);
    };
    M = M / N;
    S = S - (N * pas.System.Sqr$1(M));
    if (N > 1) {
      S = Math.sqrt(S / (N - 1))}
     else S = 0;
    Mean.set(M);
    StdDev.set(S);
  };
  this.Variance = function (data) {
    var Result = 0.0;
    var n = 0;
    n = rtl.length(data);
    if (n === 1) {
      Result = 0}
     else Result = $mod.TotalVariance(data) / (n - 1);
    return Result;
  };
  this.TotalVariance = function (data) {
    var Result = 0.0;
    var S = 0.0;
    var SS = 0.0;
    var N = 0;
    N = rtl.length(data);
    if (rtl.length(data) === 1) {
      Result = 0}
     else {
      $mod.SumsAndSquares(data,{get: function () {
          return S;
        }, set: function (v) {
          S = v;
        }},{get: function () {
          return SS;
        }, set: function (v) {
          SS = v;
        }});
      Result = SS - (pas.System.Sqr$1(S) / N);
    };
    return Result;
  };
  this.PopNStdDev = function (data) {
    var Result = 0.0;
    Result = Math.sqrt($mod.PopNVariance(data));
    return Result;
  };
  this.PopNVariance = function (data) {
    var Result = 0.0;
    var N = 0;
    N = rtl.length(data);
    if (N === 0) {
      Result = 0}
     else Result = $mod.TotalVariance(data) / N;
    return Result;
  };
  this.MomentSkewKurtosis = function (data, m1, m2, m3, m4, skew, kurtosis) {
    var i = 0;
    var N = 0;
    var deviation = 0.0;
    var deviation2 = 0.0;
    var reciprocalN = 0.0;
    var lm1 = 0.0;
    var lm2 = 0.0;
    var lm3 = 0.0;
    var lm4 = 0.0;
    var lskew = 0.0;
    var lkurtosis = 0.0;
    N = rtl.length(data);
    lm1 = 0;
    reciprocalN = 1 / N;
    for (var $l1 = 0, $end2 = N - 1; $l1 <= $end2; $l1++) {
      i = $l1;
      lm1 = lm1 + data[i];
    };
    lm1 = reciprocalN * lm1;
    lm2 = 0;
    lm3 = 0;
    lm4 = 0;
    for (var $l3 = 0, $end4 = N - 1; $l3 <= $end4; $l3++) {
      i = $l3;
      deviation = data[i] - lm1;
      deviation2 = deviation * deviation;
      lm2 = lm2 + deviation2;
      lm3 = lm3 + (deviation2 * deviation);
      lm4 = lm4 + (deviation2 * deviation2);
    };
    lm2 = reciprocalN * lm2;
    lm3 = reciprocalN * lm3;
    lm4 = reciprocalN * lm4;
    lskew = lm3 / (Math.sqrt(lm2) * lm2);
    lkurtosis = lm4 / (lm2 * lm2);
    m1.set(lm1);
    m2.set(lm2);
    m3.set(lm3);
    m4.set(lm4);
    skew.set(lskew);
    kurtosis.set(lkurtosis);
  };
  this.TPaymentTime = {"0": "ptEndOfPeriod", ptEndOfPeriod: 0, "1": "ptStartOfPeriod", ptStartOfPeriod: 1};
  $mod.$rtti.$Enum("TPaymentTime",{minvalue: 0, maxvalue: 1, ordtype: 1, enumtype: this.TPaymentTime});
  this.FutureValue = function (ARate, NPeriods, APayment, APresentValue, APaymentTime) {
    var Result = 0.0;
    var q = 0.0;
    var qn = 0.0;
    var factor = 0.0;
    if (ARate === 0) {
      Result = -APresentValue - (APayment * NPeriods)}
     else {
      q = 1.0 + ARate;
      qn = Math.pow(q,NPeriods);
      factor = (qn - 1) / (q - 1);
      if (APaymentTime === $mod.TPaymentTime.ptStartOfPeriod) factor = factor * q;
      Result = -((APresentValue * qn) + (APayment * factor));
    };
    return Result;
  };
  var DELTA = 0.001;
  var EPS = 1E-9;
  var MAXIT = 20;
  this.InterestRate = function (NPeriods, APayment, APresentValue, AFutureValue, APaymentTime) {
    var Result = 0.0;
    var r1 = 0.0;
    var r2 = 0.0;
    var dr = 0.0;
    var fv1 = 0.0;
    var fv2 = 0.0;
    var iteration = 0;
    iteration = 0;
    r1 = 0.05;
    do {
      r2 = r1 + 0.001;
      fv1 = $mod.FutureValue(r1,NPeriods,APayment,APresentValue,APaymentTime);
      fv2 = $mod.FutureValue(r2,NPeriods,APayment,APresentValue,APaymentTime);
      dr = ((AFutureValue - fv1) / (fv2 - fv1)) * 0.001;
      r1 = r1 + dr;
      iteration += 1;
    } while (!((Math.abs(dr) < 1.0E-9) || (iteration >= 20)));
    Result = r1;
    return Result;
  };
  this.NumberOfPeriods = function (ARate, APayment, APresentValue, AFutureValue, APaymentTime) {
    var Result = 0.0;
    var q = 0.0;
    var x1 = 0.0;
    var x2 = 0.0;
    if (ARate === 0) {
      Result = -(APresentValue + AFutureValue) / APayment}
     else {
      q = 1.0 + ARate;
      if (APaymentTime === $mod.TPaymentTime.ptStartOfPeriod) APayment = APayment * q;
      x1 = APayment - (AFutureValue * ARate);
      x2 = APayment + (APresentValue * ARate);
      if ((x2 === 0) || ((Math.sign(x1) * Math.sign(x2)) < 0)) {
        Result = Infinity}
       else {
        Result = Math.log(x1 / x2) / Math.log(q);
      };
    };
    return Result;
  };
  this.Payment = function (ARate, NPeriods, APresentValue, AFutureValue, APaymentTime) {
    var Result = 0.0;
    var q = 0.0;
    var qn = 0.0;
    var factor = 0.0;
    if (ARate === 0) {
      Result = -(AFutureValue + APresentValue) / NPeriods}
     else {
      q = 1.0 + ARate;
      qn = Math.pow(q,NPeriods);
      factor = (qn - 1) / (q - 1);
      if (APaymentTime === $mod.TPaymentTime.ptStartOfPeriod) factor = factor * q;
      Result = -(AFutureValue + (APresentValue * qn)) / factor;
    };
    return Result;
  };
  this.PresentValue = function (ARate, NPeriods, APayment, AFutureValue, APaymentTime) {
    var Result = 0.0;
    var q = 0.0;
    var qn = 0.0;
    var factor = 0.0;
    if (ARate === 0.0) {
      Result = -AFutureValue - (APayment * NPeriods)}
     else {
      q = 1.0 + ARate;
      qn = Math.pow(q,NPeriods);
      factor = (qn - 1) / (q - 1);
      if (APaymentTime === $mod.TPaymentTime.ptStartOfPeriod) factor = factor * q;
      Result = -(AFutureValue + (APayment * factor)) / qn;
    };
    return Result;
  };
  this.IfThen = function (val, ifTrue, ifFalse) {
    var Result = 0;
    if (val) {
      Result = ifTrue}
     else Result = ifFalse;
    return Result;
  };
  this.IfThen$1 = function (val, ifTrue, ifFalse) {
    var Result = 0.0;
    if (val) {
      Result = ifTrue}
     else Result = ifFalse;
    return Result;
  };
  $mod.$rtti.$Int("TValueRelationship",{minvalue: -1, maxvalue: 1, ordtype: 0});
  this.EqualsValue = 0;
  this.LessThanValue = -1;
  this.GreaterThanValue = 1;
  this.CompareValue = function (A, B) {
    var Result = 0;
    Result = 1;
    if (A === B) {
      Result = 0}
     else if (A < B) Result = -1;
    return Result;
  };
  this.CompareValue$1 = function (A, B) {
    var Result = 0;
    Result = 1;
    if (A === B) {
      Result = 0}
     else if (A < B) Result = -1;
    return Result;
  };
  this.CompareValue$2 = function (A, B) {
    var Result = 0;
    Result = 1;
    if (A === B) {
      Result = 0}
     else if (A < B) Result = -1;
    return Result;
  };
  this.CompareValue$3 = function (A, B, delta) {
    var Result = 0;
    Result = 1;
    if (Math.abs(A - B) <= delta) {
      Result = 0}
     else if (A < B) Result = -1;
    return Result;
  };
},null,function () {
  "use strict";
  var $mod = this;
  var $impl = $mod.$impl;
  $impl.DZeroResolution = 1E-12;
  $impl.sumofsquares = function (data) {
    var Result = 0.0;
    var i = 0;
    var N = 0;
    N = rtl.length(data);
    Result = 0.0;
    for (var $l1 = 0, $end2 = N - 1; $l1 <= $end2; $l1++) {
      i = $l1;
      Result = Result + pas.System.Sqr$1(data[i]);
    };
    return Result;
  };
});
rtl.module("GLTypes",["System","webgl","Math","SysUtils"],function () {
  "use strict";
  var $mod = this;
  $mod.$rtti.$DynArray("TJSFloat32List",{eltype: rtl.double});
  rtl.recNewT($mod,"TVec2",function () {
    this.x = 0.0;
    this.y = 0.0;
    this.$eq = function (b) {
      return (this.x === b.x) && (this.y === b.y);
    };
    this.$assign = function (s) {
      this.x = s.x;
      this.y = s.y;
      return this;
    };
    var $r = $mod.$rtti.$Record("TVec2",{});
    $r.addField("x",rtl.double);
    $r.addField("y",rtl.double);
  });
  rtl.recNewT($mod,"TVec3",function () {
    this.x = 0.0;
    this.y = 0.0;
    this.z = 0.0;
    this.$eq = function (b) {
      return (this.x === b.x) && (this.y === b.y) && (this.z === b.z);
    };
    this.$assign = function (s) {
      this.x = s.x;
      this.y = s.y;
      this.z = s.z;
      return this;
    };
    var $r = $mod.$rtti.$Record("TVec3",{});
    $r.addField("x",rtl.double);
    $r.addField("y",rtl.double);
    $r.addField("z",rtl.double);
  });
  $mod.$rtti.$DynArray("TRGBAb",{eltype: rtl.byte});
  $mod.$rtti.$DynArray("TRGBAf",{eltype: rtl.double});
  this.V3 = function (x, y, z) {
    var Result = $mod.TVec3.$new();
    Result.x = x;
    Result.y = y;
    Result.z = z;
    return Result;
  };
  this.ToFloats = function (v) {
    var Result = [];
    Result = rtl.arraySetLength(Result,0.0,3);
    Result[0] = v.x;
    Result[1] = v.y;
    Result[2] = v.z;
    return Result;
  };
  this.VecStr = function (v) {
    var Result = "";
    Result = "{" + pas.SysUtils.FloatToStr(v.x) + "," + pas.SysUtils.FloatToStr(v.y) + "," + pas.SysUtils.FloatToStr(v.z) + "}";
    return Result;
  };
  this.Show = function (v) {
    pas.System.Writeln("{",v.x,",",v.y,",",v.z,"}");
  };
  this.Add = function (v, amount) {
    var Result = $mod.TVec3.$new();
    Result.$assign($mod.V3(v.x + amount,v.y + amount,v.z + amount));
    return Result;
  };
  this.Add$1 = function (v, amount) {
    var Result = $mod.TVec3.$new();
    Result.$assign($mod.V3(v.x + amount.x,v.y + amount.y,v.z + amount.z));
    return Result;
  };
  this.Subtract = function (v, amount) {
    var Result = $mod.TVec3.$new();
    Result.$assign($mod.V3(v.x - amount,v.y - amount,v.z - amount));
    return Result;
  };
  this.Subtract$1 = function (v, amount) {
    var Result = $mod.TVec3.$new();
    Result.$assign($mod.V3(v.x - amount.x,v.y - amount.y,v.z - amount.z));
    return Result;
  };
  this.Multiply = function (v, amount) {
    var Result = $mod.TVec3.$new();
    Result.$assign($mod.V3(v.x * amount,v.y * amount,v.z * amount));
    return Result;
  };
  this.Multiply$1 = function (v, amount) {
    var Result = $mod.TVec3.$new();
    Result.$assign($mod.V3(v.x * amount.x,v.y * amount.y,v.z * amount.z));
    return Result;
  };
  this.Divide = function (v, amount) {
    var Result = $mod.TVec3.$new();
    Result.$assign($mod.V3(v.x / amount,v.y / amount,v.z / amount));
    return Result;
  };
  this.Divide$1 = function (v, amount) {
    var Result = $mod.TVec3.$new();
    Result.$assign($mod.V3(v.x / amount.x,v.y / amount.y,v.z / amount.z));
    return Result;
  };
  this.Sum = function (v) {
    var Result = 0.0;
    Result = v.x + v.y + v.z;
    return Result;
  };
  this.Magnitude = function (v) {
    var Result = 0.0;
    Result = Math.sqrt(Math.pow(v.x,2) + Math.pow(v.y,2) + Math.pow(v.z,2));
    return Result;
  };
  this.SquaredLength = function (v) {
    var Result = 0.0;
    Result = pas.System.Sqr$1(v.x) + pas.System.Sqr$1(v.y) + pas.System.Sqr$1(v.z);
    return Result;
  };
  this.Normalize = function (v) {
    var Result = $mod.TVec3.$new();
    Result.$assign($mod.Divide($mod.TVec3.$clone(v),$mod.Magnitude($mod.TVec3.$clone(v))));
    return Result;
  };
  this.Dot = function (v, point) {
    var Result = 0.0;
    Result = (v.x * point.x) + (v.y * point.y) + (v.z * point.z);
    return Result;
  };
  this.Cross = function (v, point) {
    var Result = $mod.TVec3.$new();
    Result.x = (v.y * point.z) - (v.z * point.y);
    Result.y = (v.z * point.x) - (v.x * point.z);
    Result.z = (v.x * point.y) - (v.y * point.x);
    return Result;
  };
  this.V2 = function (x, y) {
    var Result = $mod.TVec2.$new();
    Result.x = x;
    Result.y = y;
    return Result;
  };
  this.ToFloats$1 = function (v) {
    var Result = [];
    Result = rtl.arraySetLength(Result,0.0,2);
    Result[0] = v.x;
    Result[1] = v.y;
    return Result;
  };
  this.RGBAb = function (r, g, b, a) {
    var Result = [];
    Result[0] = r;
    Result[1] = g;
    Result[2] = b;
    Result[3] = a;
    return Result;
  };
  this.RGBAf = function (r, g, b, a) {
    var Result = [];
    Result[0] = r;
    Result[1] = g;
    Result[2] = b;
    Result[3] = a;
    return Result;
  };
});
