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
