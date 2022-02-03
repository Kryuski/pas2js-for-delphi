rtl.module("PushJS",["System","JS"],function () {
  "use strict";
  var $mod = this;
  $mod.$rtti.$RefToProcVar("TPushFunction",{procsig: rtl.newTIProcSig(null)});
  $mod.$rtti.$RefToProcVar("TPushParamsFallback",{procsig: rtl.newTIProcSig([["payload",$mod.$rtti["TPushFallbackPayload"]]])});
});
