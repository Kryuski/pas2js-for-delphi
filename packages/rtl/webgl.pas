Unit webgl;

{$MODE ObjFPC}
{$H+}
{$modeswitch externalclass}

interface

uses SysUtils, JS,web;

{
  Automatically generated file by TWebIDLToPas on 2018-06-23 15:31:57
  
  Used command-line options : 
  -i
  webgl.idl
  -v
  v2
  -t
  EventTarget=TJSEventTarget,DOMHighResTimeStamp=TJSDOMHighResTimeStamp,HTMLMediaElement=TJSElement,MediaStream=JSValue,MediaStreamTrack=JSValue,EventHandler=TJSEVentHandler,Promise=TJSPromise,Event=TJSEvent,Worklet=TJSOBject,WorkletGlobalScope=TJSObject,VoidFunction=TProcedure,MessagePort=TJSMessagePort,HTMLCanvasElement=TJSHTMLCanvasElement,BufferSource=TJSBufferSource,ArrayBufferView=TJSTypedArray
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
  HTMLCanvasElement=TJSHTMLCanvasElement
  BufferSource=TJSBufferSource
  ArrayBufferView=TJSTypedArray
}
Type
  // Forward class definitions
  TJSWebGLObject = Class;
  TJSWebGLBuffer = Class;
  TJSWebGLFramebuffer = Class;
  TJSWebGLProgram = Class;
  TJSWebGLRenderbuffer = Class;
  TJSWebGLShader = Class;
  TJSWebGLTexture = Class;
  TJSWebGLUniformLocation = Class;
  TJSWebGLActiveInfo = Class;
  TJSWebGLShaderPrecisionFormat = Class;
  TJSWebGLRenderingContextBase = Class;
  TJSWebGLRenderingContext = Class;
  TJSWebGLContextEvent = Class;
  TJSWebGLContextAttributes = Class;
  TJSWebGLContextEventInit = Class;
  WebGLPowerPreference = String;
  GLenum = NativeInt;
  GLboolean = boolean;
  GLbitfield = NativeInt;
  GLbyte = byte;
  GLshort = Integer;
  GLint = Integer;
  GLsizei = Integer;
  GLintptr = NativeInt;
  GLsizeiptr = NativeInt;
  GLubyte = Byte;
  GLushort = Cardinal;
  GLuint = NativeInt;
  GLfloat = Double;
  GLclampf = Double;
  // Union of ImageBitmap, ImageData, HTMLImageElement, HTMLCanvasElement, HTMLVideoElement
  TexImageSource = JSValue; 
  // Union of Float32Array, sequence
  Float32List = JSValue; 
  // Union of Int32Array, sequence
  Int32List = JSValue; 
  
  { --------------------------------------------------------------------
    TJSWebGLContextAttributes
    --------------------------------------------------------------------}
  
  TJSWebGLContextAttributes = class(TJSObject)
    alpha : GLboolean;
    depth : GLboolean;
    stencil : GLboolean;
    antialias : GLboolean;
    premultipliedAlpha : GLboolean;
    preserveDrawingBuffer : GLboolean;
    powerPreference : WebGLPowerPreference;
    failIfMajorPerformanceCaveat : GLboolean;
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLContextEventInit
    --------------------------------------------------------------------}
  
  TJSWebGLContextEventInit = class(TJSObject)
    statusMessage : String;
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLObject
    --------------------------------------------------------------------}
  
  TJSWebGLObject = class external name 'WebGLObject' 
  Private
  Public
    
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLBuffer
    --------------------------------------------------------------------}
  
  TJSWebGLBuffer = class external name 'WebGLBuffer'  (TJSWebGLObject)
  Private
  Public
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLFramebuffer
    --------------------------------------------------------------------}
  
  TJSWebGLFramebuffer = class external name 'WebGLFramebuffer'  (TJSWebGLObject)
  Private
  Public
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLProgram
    --------------------------------------------------------------------}
  
  TJSWebGLProgram = class external name 'WebGLProgram'  (TJSWebGLObject)
  Private
  Public
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLRenderbuffer
    --------------------------------------------------------------------}
  
  TJSWebGLRenderbuffer = class external name 'WebGLRenderbuffer'  (TJSWebGLObject)
  Private
  Public
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLShader
    --------------------------------------------------------------------}
  
  TJSWebGLShader = class external name 'WebGLShader'  (TJSWebGLObject)
  Private
  Public
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLTexture
    --------------------------------------------------------------------}
  
  TJSWebGLTexture = class external name 'WebGLTexture'  (TJSWebGLObject)
  Private
  Public
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLUniformLocation
    --------------------------------------------------------------------}
  
  TJSWebGLUniformLocation = class external name 'WebGLUniformLocation' 
  Private
  Public
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLActiveInfo
    --------------------------------------------------------------------}
  
  TJSWebGLActiveInfo = class external name 'WebGLActiveInfo' 
  Private
    Fsize : GLint; external name 'size'; 
    Ftype_ : GLenum; external name 'type'; 
    Fname : String; external name 'name'; 
  Public
    Property size : GLint Read Fsize; 
    Property type_ : GLenum Read Ftype_; 
    Property name : String Read Fname; 
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLShaderPrecisionFormat
    --------------------------------------------------------------------}
  
  TJSWebGLShaderPrecisionFormat = class external name 'WebGLShaderPrecisionFormat' 
  Private
    FrangeMin : GLint; external name 'rangeMin'; 
    FrangeMax : GLint; external name 'rangeMax'; 
    Fprecision : GLint; external name 'precision'; 
  Public
    Property rangeMin : GLint Read FrangeMin; 
    Property rangeMax : GLint Read FrangeMax; 
    Property precision : GLint Read Fprecision; 
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLRenderingContextBase
    --------------------------------------------------------------------}
  
  TStringDynArray = Array of String;
  TTJSWebGLShaderDynArray = Array of TJSWebGLShader;
  TGLfloatDynArray = Array of GLfloat;
  TGLintDynArray = Array of GLint;

  // MG: added ancestor (TJSObject), issue 34745
  TJSWebGLRenderingContextBase = class external name 'WebGLRenderingContextBase' (TJSObject)
  Private
    Fcanvas : TJSHTMLCanvasElement; external name 'canvas'; 
    FdrawingBufferWidth : GLsizei; external name 'drawingBufferWidth'; 
    FdrawingBufferHeight : GLsizei; external name 'drawingBufferHeight'; 
  Public
    Const
      DEPTH_BUFFER_BIT = $00000100;
      STENCIL_BUFFER_BIT = $00000400;
      COLOR_BUFFER_BIT = $00004000;
      POINTS = $0000;
      LINES = $0001;
      LINE_LOOP = $0002;
      LINE_STRIP = $0003;
      TRIANGLES = $0004;
      TRIANGLE_STRIP = $0005;
      TRIANGLE_FAN = $0006;
      ZERO = 0;
      ONE = 1;
      SRC_COLOR = $0300;
      ONE_MINUS_SRC_COLOR = $0301;
      SRC_ALPHA = $0302;
      ONE_MINUS_SRC_ALPHA = $0303;
      DST_ALPHA = $0304;
      ONE_MINUS_DST_ALPHA = $0305;
      DST_COLOR = $0306;
      ONE_MINUS_DST_COLOR = $0307;
      SRC_ALPHA_SATURATE = $0308;
      FUNC_ADD = $8006;
      BLEND_EQUATION = $8009;
      BLEND_EQUATION_RGB = $8009;
      BLEND_EQUATION_ALPHA = $883D;
      FUNC_SUBTRACT = $800A;
      FUNC_REVERSE_SUBTRACT = $800B;
      BLEND_DST_RGB = $80C8;
      BLEND_SRC_RGB = $80C9;
      BLEND_DST_ALPHA = $80CA;
      BLEND_SRC_ALPHA = $80CB;
      CONSTANT_COLOR = $8001;
      ONE_MINUS_CONSTANT_COLOR = $8002;
      CONSTANT_ALPHA = $8003;
      ONE_MINUS_CONSTANT_ALPHA = $8004;
      BLEND_COLOR = $8005;
      ARRAY_BUFFER = $8892;
      ELEMENT_ARRAY_BUFFER = $8893;
      ARRAY_BUFFER_BINDING = $8894;
      ELEMENT_ARRAY_BUFFER_BINDING = $8895;
      STREAM_DRAW = $88E0;
      STATIC_DRAW = $88E4;
      DYNAMIC_DRAW = $88E8;
      BUFFER_SIZE = $8764;
      BUFFER_USAGE = $8765;
      CURRENT_VERTEX_ATTRIB = $8626;
      FRONT = $0404;
      BACK = $0405;
      FRONT_AND_BACK = $0408;
      CULL_FACE = $0B44;
      BLEND = $0BE2;
      DITHER = $0BD0;
      STENCIL_TEST = $0B90;
      DEPTH_TEST = $0B71;
      SCISSOR_TEST = $0C11;
      POLYGON_OFFSET_FILL = $8037;
      SAMPLE_ALPHA_TO_COVERAGE = $809E;
      SAMPLE_COVERAGE = $80A0;
      NO_ERROR = 0;
      INVALID_ENUM = $0500;
      INVALID_VALUE = $0501;
      INVALID_OPERATION = $0502;
      OUT_OF_MEMORY = $0505;
      CW = $0900;
      CCW = $0901;
      LINE_WIDTH = $0B21;
      ALIASED_POINT_SIZE_RANGE = $846D;
      ALIASED_LINE_WIDTH_RANGE = $846E;
      CULL_FACE_MODE = $0B45;
      FRONT_FACE = $0B46;
      DEPTH_RANGE = $0B70;
      DEPTH_WRITEMASK = $0B72;
      DEPTH_CLEAR_VALUE = $0B73;
      DEPTH_FUNC = $0B74;
      STENCIL_CLEAR_VALUE = $0B91;
      STENCIL_FUNC = $0B92;
      STENCIL_FAIL = $0B94;
      STENCIL_PASS_DEPTH_FAIL = $0B95;
      STENCIL_PASS_DEPTH_PASS = $0B96;
      STENCIL_REF = $0B97;
      STENCIL_VALUE_MASK = $0B93;
      STENCIL_WRITEMASK = $0B98;
      STENCIL_BACK_FUNC = $8800;
      STENCIL_BACK_FAIL = $8801;
      STENCIL_BACK_PASS_DEPTH_FAIL = $8802;
      STENCIL_BACK_PASS_DEPTH_PASS = $8803;
      STENCIL_BACK_REF = $8CA3;
      STENCIL_BACK_VALUE_MASK = $8CA4;
      STENCIL_BACK_WRITEMASK = $8CA5;
      VIEWPORT_ = $0BA2;
      SCISSOR_BOX = $0C10;
      COLOR_CLEAR_VALUE = $0C22;
      COLOR_WRITEMASK = $0C23;
      UNPACK_ALIGNMENT = $0CF5;
      PACK_ALIGNMENT = $0D05;
      MAX_TEXTURE_SIZE = $0D33;
      MAX_VIEWPORT_DIMS = $0D3A;
      SUBPIXEL_BITS = $0D50;
      RED_BITS = $0D52;
      GREEN_BITS = $0D53;
      BLUE_BITS = $0D54;
      ALPHA_BITS = $0D55;
      DEPTH_BITS = $0D56;
      STENCIL_BITS = $0D57;
      POLYGON_OFFSET_UNITS = $2A00;
      POLYGON_OFFSET_FACTOR = $8038;
      TEXTURE_BINDING_2D = $8069;
      SAMPLE_BUFFERS = $80A8;
      SAMPLES = $80A9;
      SAMPLE_COVERAGE_VALUE = $80AA;
      SAMPLE_COVERAGE_INVERT = $80AB;
      COMPRESSED_TEXTURE_FORMATS = $86A3;
      DONT_CARE = $1100;
      FASTEST = $1101;
      NICEST = $1102;
      GENERATE_MIPMAP_HINT = $8192;
      BYTE = $1400;
      UNSIGNED_BYTE = $1401;
      SHORT = $1402;
      UNSIGNED_SHORT = $1403;
      INT = $1404;
      UNSIGNED_INT = $1405;
      FLOAT = $1406;
      DEPTH_COMPONENT = $1902;
      ALPHA = $1906;
      RGB = $1907;
      RGBA = $1908;
      LUMINANCE = $1909;
      LUMINANCE_ALPHA = $190A;
      UNSIGNED_SHORT_4_4_4_4 = $8033;
      UNSIGNED_SHORT_5_5_5_1 = $8034;
      UNSIGNED_SHORT_5_6_5 = $8363;
      FRAGMENT_SHADER = $8B30;
      VERTEX_SHADER = $8B31;
      MAX_VERTEX_ATTRIBS = $8869;
      MAX_VERTEX_UNIFORM_VECTORS = $8DFB;
      MAX_VARYING_VECTORS = $8DFC;
      MAX_COMBINED_TEXTURE_IMAGE_UNITS = $8B4D;
      MAX_VERTEX_TEXTURE_IMAGE_UNITS = $8B4C;
      MAX_TEXTURE_IMAGE_UNITS = $8872;
      MAX_FRAGMENT_UNIFORM_VECTORS = $8DFD;
      SHADER_TYPE = $8B4F;
      DELETE_STATUS = $8B80;
      LINK_STATUS = $8B82;
      VALIDATE_STATUS = $8B83;
      ATTACHED_SHADERS = $8B85;
      ACTIVE_UNIFORMS = $8B86;
      ACTIVE_ATTRIBUTES = $8B89;
      SHADING_LANGUAGE_VERSION = $8B8C;
      CURRENT_PROGRAM = $8B8D;
      NEVER = $0200;
      LESS = $0201;
      EQUAL = $0202;
      LEQUAL = $0203;
      GREATER = $0204;
      NOTEQUAL = $0205;
      GEQUAL = $0206;
      ALWAYS = $0207;
      KEEP = $1E00;
      REPLACE = $1E01;
      INCR = $1E02;
      DECR = $1E03;
      INVERT = $150A;
      INCR_WRAP = $8507;
      DECR_WRAP = $8508;
      VENDOR = $1F00;
      RENDERER = $1F01;
      VERSION = $1F02;
      NEAREST = $2600;
      LINEAR = $2601;
      NEAREST_MIPMAP_NEAREST = $2700;
      LINEAR_MIPMAP_NEAREST = $2701;
      NEAREST_MIPMAP_LINEAR = $2702;
      LINEAR_MIPMAP_LINEAR = $2703;
      TEXTURE_MAG_FILTER = $2800;
      TEXTURE_MIN_FILTER = $2801;
      TEXTURE_WRAP_S = $2802;
      TEXTURE_WRAP_T = $2803;
      TEXTURE_2D = $0DE1;
      TEXTURE = $1702;
      TEXTURE_CUBE_MAP = $8513;
      TEXTURE_BINDING_CUBE_MAP = $8514;
      TEXTURE_CUBE_MAP_POSITIVE_X = $8515;
      TEXTURE_CUBE_MAP_NEGATIVE_X = $8516;
      TEXTURE_CUBE_MAP_POSITIVE_Y = $8517;
      TEXTURE_CUBE_MAP_NEGATIVE_Y = $8518;
      TEXTURE_CUBE_MAP_POSITIVE_Z = $8519;
      TEXTURE_CUBE_MAP_NEGATIVE_Z = $851A;
      MAX_CUBE_MAP_TEXTURE_SIZE = $851C;
      TEXTURE0 = $84C0;
      TEXTURE1 = $84C1;
      TEXTURE2 = $84C2;
      TEXTURE3 = $84C3;
      TEXTURE4 = $84C4;
      TEXTURE5 = $84C5;
      TEXTURE6 = $84C6;
      TEXTURE7 = $84C7;
      TEXTURE8 = $84C8;
      TEXTURE9 = $84C9;
      TEXTURE10 = $84CA;
      TEXTURE11 = $84CB;
      TEXTURE12 = $84CC;
      TEXTURE13 = $84CD;
      TEXTURE14 = $84CE;
      TEXTURE15 = $84CF;
      TEXTURE16 = $84D0;
      TEXTURE17 = $84D1;
      TEXTURE18 = $84D2;
      TEXTURE19 = $84D3;
      TEXTURE20 = $84D4;
      TEXTURE21 = $84D5;
      TEXTURE22 = $84D6;
      TEXTURE23 = $84D7;
      TEXTURE24 = $84D8;
      TEXTURE25 = $84D9;
      TEXTURE26 = $84DA;
      TEXTURE27 = $84DB;
      TEXTURE28 = $84DC;
      TEXTURE29 = $84DD;
      TEXTURE30 = $84DE;
      TEXTURE31 = $84DF;
      ACTIVE_TEXTURE = $84E0;
      REPEAT_ = $2901;
      CLAMP_TO_EDGE = $812F;
      MIRRORED_REPEAT = $8370;
      FLOAT_VEC2 = $8B50;
      FLOAT_VEC3 = $8B51;
      FLOAT_VEC4 = $8B52;
      INT_VEC2 = $8B53;
      INT_VEC3 = $8B54;
      INT_VEC4 = $8B55;
      BOOL = $8B56;
      BOOL_VEC2 = $8B57;
      BOOL_VEC3 = $8B58;
      BOOL_VEC4 = $8B59;
      FLOAT_MAT2 = $8B5A;
      FLOAT_MAT3 = $8B5B;
      FLOAT_MAT4 = $8B5C;
      SAMPLER_2D = $8B5E;
      SAMPLER_CUBE = $8B60;
      VERTEX_ATTRIB_ARRAY_ENABLED = $8622;
      VERTEX_ATTRIB_ARRAY_SIZE = $8623;
      VERTEX_ATTRIB_ARRAY_STRIDE = $8624;
      VERTEX_ATTRIB_ARRAY_TYPE = $8625;
      VERTEX_ATTRIB_ARRAY_NORMALIZED = $886A;
      VERTEX_ATTRIB_ARRAY_POINTER = $8645;
      VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = $889F;
      IMPLEMENTATION_COLOR_READ_TYPE = $8B9A;
      IMPLEMENTATION_COLOR_READ_FORMAT = $8B9B;
      COMPILE_STATUS = $8B81;
      LOW_FLOAT = $8DF0;
      MEDIUM_FLOAT = $8DF1;
      HIGH_FLOAT = $8DF2;
      LOW_INT = $8DF3;
      MEDIUM_INT = $8DF4;
      HIGH_INT = $8DF5;
      FRAMEBUFFER = $8D40;
      RENDERBUFFER = $8D41;
      RGBA4 = $8056;
      RGB5_A1 = $8057;
      RGB565 = $8D62;
      DEPTH_COMPONENT16 = $81A5;
      STENCIL_INDEX8 = $8D48;
      DEPTH_STENCIL = $84F9;
      RENDERBUFFER_WIDTH = $8D42;
      RENDERBUFFER_HEIGHT = $8D43;
      RENDERBUFFER_INTERNAL_FORMAT = $8D44;
      RENDERBUFFER_RED_SIZE = $8D50;
      RENDERBUFFER_GREEN_SIZE = $8D51;
      RENDERBUFFER_BLUE_SIZE = $8D52;
      RENDERBUFFER_ALPHA_SIZE = $8D53;
      RENDERBUFFER_DEPTH_SIZE = $8D54;
      RENDERBUFFER_STENCIL_SIZE = $8D55;
      FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = $8CD0;
      FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = $8CD1;
      FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = $8CD2;
      FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = $8CD3;
      COLOR_ATTACHMENT0 = $8CE0;
      DEPTH_ATTACHMENT = $8D00;
      STENCIL_ATTACHMENT = $8D20;
      DEPTH_STENCIL_ATTACHMENT = $821A;
      NONE = 0;
      FRAMEBUFFER_COMPLETE = $8CD5;
      FRAMEBUFFER_INCOMPLETE_ATTACHMENT = $8CD6;
      FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = $8CD7;
      FRAMEBUFFER_INCOMPLETE_DIMENSIONS = $8CD9;
      FRAMEBUFFER_UNSUPPORTED = $8CDD;
      FRAMEBUFFER_BINDING = $8CA6;
      RENDERBUFFER_BINDING = $8CA7;
      MAX_RENDERBUFFER_SIZE = $84E8;
      INVALID_FRAMEBUFFER_OPERATION = $0506;
      UNPACK_FLIP_Y_WEBGL = $9240;
      UNPACK_PREMULTIPLY_ALPHA_WEBGL = $9241;
      CONTEXT_LOST_WEBGL = $9242;
      UNPACK_COLORSPACE_CONVERSION_WEBGL = $9243;
      BROWSER_DEFAULT_WEBGL = $9244;
  Public
    function getContextAttributes: TJSWebGLContextAttributes;
    function isContextLost: boolean;
    function getSupportedExtensions: TStringDynArray;
    function getExtension(name : String): TJSObject;
    Procedure activeTexture(texture : GLenum);
    Procedure attachShader(program_ : TJSWebGLProgram; shader : TJSWebGLShader);
    Procedure bindAttribLocation(program_ : TJSWebGLProgram; index : GLuint; name : String);
    Procedure bindBuffer(target : GLenum; buffer : TJSWebGLBuffer);
    Procedure bindFramebuffer(target : GLenum; framebuffer : TJSWebGLFramebuffer);
    Procedure bindRenderbuffer(target : GLenum; renderbuffer : TJSWebGLRenderbuffer);
    Procedure bindTexture(target : GLenum; texture : TJSWebGLTexture);
    Procedure blendColor(red : GLclampf; green : GLclampf; blue : GLclampf; alpha : GLclampf);
    Procedure blendEquation(mode : GLenum);
    Procedure blendEquationSeparate(modeRGB : GLenum; modeAlpha : GLenum);
    Procedure blendFunc(sfactor : GLenum; dfactor : GLenum);
    Procedure blendFuncSeparate(srcRGB : GLenum; dstRGB : GLenum; srcAlpha : GLenum; dstAlpha : GLenum);
    Procedure bufferData(target : GLenum; size : GLsizeiptr; usage : GLenum); overload;
    Procedure bufferData(target : GLenum; data : TJSBufferSource; usage : GLenum); overload;
    Procedure bufferSubData(target : GLenum; offset : GLintptr; data : TJSBufferSource);
    function checkFramebufferStatus(target : GLenum): GLenum;
    Procedure clear(mask : GLbitfield);
    Procedure clearColor(red : GLclampf; green : GLclampf; blue : GLclampf; alpha : GLclampf);
    Procedure clearDepth(depth : GLclampf);
    Procedure clearStencil(s : GLint);
    Procedure colorMask(red : GLboolean; green : GLboolean; blue : GLboolean; alpha : GLboolean);
    Procedure compileShader(shader : TJSWebGLShader);
    Procedure compressedTexImage2D(target : GLenum; level : GLint; internalformat : GLenum; width : GLsizei; height : GLsizei; border : GLint; data : TJSTypedArray);
    Procedure compressedTexSubImage2D(target : GLenum; level : GLint; xoffset : GLint; yoffset : GLint; width : GLsizei; height : GLsizei; format : GLenum; data : TJSTypedArray);
    Procedure copyTexImage2D(target : GLenum; level : GLint; internalformat : GLenum; x : GLint; y : GLint; width : GLsizei; height : GLsizei; border : GLint);
    Procedure copyTexSubImage2D(target : GLenum; level : GLint; xoffset : GLint; yoffset : GLint; x : GLint; y : GLint; width : GLsizei; height : GLsizei);
    function createBuffer: TJSWebGLBuffer;
    function createFramebuffer: TJSWebGLFramebuffer;
    function createProgram: TJSWebGLProgram;
    function createRenderbuffer: TJSWebGLRenderbuffer;
    function createShader(type_ : GLenum): TJSWebGLShader;
    function createTexture: TJSWebGLTexture;
    Procedure cullFace(mode : GLenum);
    Procedure deleteBuffer(buffer : TJSWebGLBuffer);
    Procedure deleteFramebuffer(framebuffer : TJSWebGLFramebuffer);
    Procedure deleteProgram(program_ : TJSWebGLProgram);
    Procedure deleteRenderbuffer(renderbuffer : TJSWebGLRenderbuffer);
    Procedure deleteShader(shader : TJSWebGLShader);
    Procedure deleteTexture(texture : TJSWebGLTexture);
    Procedure depthFunc(func : GLenum);
    Procedure depthMask(flag : GLboolean);
    Procedure depthRange(zNear : GLclampf; zFar : GLclampf);
    Procedure detachShader(program_ : TJSWebGLProgram; shader : TJSWebGLShader);
    Procedure disable(cap : GLenum);
    Procedure disableVertexAttribArray(index : GLuint);
    Procedure drawArrays(mode : GLenum; first : GLint; count : GLsizei);
    Procedure drawElements(mode : GLenum; count : GLsizei; type_ : GLenum; offset : GLintptr);
    Procedure enable(cap : GLenum);
    Procedure enableVertexAttribArray(index : GLuint);
    Procedure finish;
    Procedure flush;
    Procedure framebufferRenderbuffer(target : GLenum; attachment : GLenum; renderbuffertarget : GLenum; renderbuffer : TJSWebGLRenderbuffer);
    Procedure framebufferTexture2D(target : GLenum; attachment : GLenum; textarget : GLenum; texture : TJSWebGLTexture; level : GLint);
    Procedure frontFace(mode : GLenum);
    Procedure generateMipmap(target : GLenum);
    function getActiveAttrib(program_ : TJSWebGLProgram; index : GLuint): TJSWebGLActiveInfo;
    function getActiveUniform(program_ : TJSWebGLProgram; index : GLuint): TJSWebGLActiveInfo;
    function getAttachedShaders(program_ : TJSWebGLProgram): TTJSWebGLShaderDynArray;
    function getAttribLocation(program_ : TJSWebGLProgram; name : String): GLint;
    function getBufferParameter(target : GLenum; pname : GLenum): JSValue;
    function getParameter(pname : GLenum): JSValue;
    function getError: GLenum;
    function getFramebufferAttachmentParameter(target : GLenum; attachment : GLenum; pname : GLenum): JSValue;
    function getProgramParameter(program_ : TJSWebGLProgram; pname : GLenum): JSValue;
    function getProgramInfoLog(program_ : TJSWebGLProgram): String;
    function getRenderbufferParameter(target : GLenum; pname : GLenum): JSValue;
    function getShaderParameter(shader : TJSWebGLShader; pname : GLenum): JSValue;
    function getShaderPrecisionFormat(shadertype : GLenum; precisiontype : GLenum): TJSWebGLShaderPrecisionFormat;
    function getShaderInfoLog(shader : TJSWebGLShader): String;
    function getShaderSource(shader : TJSWebGLShader): String;
    function getTexParameter(target : GLenum; pname : GLenum): JSValue;
    function getUniform(program_ : TJSWebGLProgram; location : TJSWebGLUniformLocation): JSValue;
    function getUniformLocation(program_ : TJSWebGLProgram; name : String): TJSWebGLUniformLocation;
    function getVertexAttrib(index : GLuint; pname : GLenum): JSValue;
    function getVertexAttribOffset(index : GLuint; pname : GLenum): GLintptr;
    Procedure hint(target : GLenum; mode : GLenum);
    function isBuffer(buffer : TJSWebGLBuffer): GLboolean;
    function isEnabled(cap : GLenum): GLboolean;
    function isFramebuffer(framebuffer : TJSWebGLFramebuffer): GLboolean;
    function isProgram(program_ : TJSWebGLProgram): GLboolean;
    function isRenderbuffer(renderbuffer : TJSWebGLRenderbuffer): GLboolean;
    function isShader(shader : TJSWebGLShader): GLboolean;
    function isTexture(texture : TJSWebGLTexture): GLboolean;
    Procedure lineWidth(width : GLfloat);
    Procedure linkProgram(program_ : TJSWebGLProgram);
    Procedure pixelStorei(pname : GLenum; param : GLint);
    Procedure polygonOffset(factor : GLfloat; units : GLfloat);
    Procedure readPixels(x : GLint; y : GLint; width : GLsizei; height : GLsizei; format : GLenum; type_ : GLenum; pixels : TJSTypedArray);
    Procedure renderbufferStorage(target : GLenum; internalformat : GLenum; width : GLsizei; height : GLsizei);
    Procedure sampleCoverage(value : GLclampf; invert : GLboolean);
    Procedure scissor(x : GLint; y : GLint; width : GLsizei; height : GLsizei);
    Procedure shaderSource(shader : TJSWebGLShader; source : String);
    Procedure stencilFunc(func : GLenum; ref : GLint; mask : GLuint);
    Procedure stencilFuncSeparate(face : GLenum; func : GLenum; ref : GLint; mask : GLuint);
    Procedure stencilMask(mask : GLuint);
    Procedure stencilMaskSeparate(face : GLenum; mask : GLuint);
    Procedure stencilOp(fail : GLenum; zfail : GLenum; zpass : GLenum);
    Procedure stencilOpSeparate(face : GLenum; fail : GLenum; zfail : GLenum; zpass : GLenum);
    Procedure texImage2D(target : GLenum; level : GLint; internalformat : GLint; width : GLsizei; height : GLsizei; border : GLint; format : GLenum; type_ : GLenum; pixels : TJSTypedArray);
    Procedure texImage2D(target : GLenum; level : GLint; internalformat : GLint; format : GLenum; type_ : GLenum; source : TexImageSource);
    Procedure texParameterf(target : GLenum; pname : GLenum; param : GLfloat);
    Procedure texParameteri(target : GLenum; pname : GLenum; param : GLint);
    Procedure texSubImage2D(target : GLenum; level : GLint; xoffset : GLint; yoffset : GLint; width : GLsizei; height : GLsizei; format : GLenum; type_ : GLenum; pixels : TJSTypedArray);
    Procedure texSubImage2D(target : GLenum; level : GLint; xoffset : GLint; yoffset : GLint; format : GLenum; type_ : GLenum; source : TexImageSource);
    Procedure uniform1f(location : TJSWebGLUniformLocation; x : GLfloat);
    Procedure uniform2f(location : TJSWebGLUniformLocation; x : GLfloat; y : GLfloat);
    Procedure uniform3f(location : TJSWebGLUniformLocation; x : GLfloat; y : GLfloat; z : GLfloat);
    Procedure uniform4f(location : TJSWebGLUniformLocation; x : GLfloat; y : GLfloat; z : GLfloat; w : GLfloat);
    Procedure uniform1i(location : TJSWebGLUniformLocation; x : GLint);
    Procedure uniform2i(location : TJSWebGLUniformLocation; x : GLint; y : GLint);
    Procedure uniform3i(location : TJSWebGLUniformLocation; x : GLint; y : GLint; z : GLint);
    Procedure uniform4i(location : TJSWebGLUniformLocation; x : GLint; y : GLint; z : GLint; w : GLint);
    Procedure uniform1fv(location : TJSWebGLUniformLocation; v : Float32List);
    Procedure uniform2fv(location : TJSWebGLUniformLocation; v : Float32List);
    Procedure uniform3fv(location : TJSWebGLUniformLocation; v : Float32List);
    Procedure uniform4fv(location : TJSWebGLUniformLocation; v : Float32List);
    Procedure uniform1iv(location : TJSWebGLUniformLocation; v : Int32List);
    Procedure uniform2iv(location : TJSWebGLUniformLocation; v : Int32List);
    Procedure uniform3iv(location : TJSWebGLUniformLocation; v : Int32List);
    Procedure uniform4iv(location : TJSWebGLUniformLocation; v : Int32List);
    Procedure uniformMatrix2fv(location : TJSWebGLUniformLocation; transpose : GLboolean; value : Float32List);
    Procedure uniformMatrix3fv(location : TJSWebGLUniformLocation; transpose : GLboolean; value : Float32List);
    Procedure uniformMatrix4fv(location : TJSWebGLUniformLocation; transpose : GLboolean; value : Float32List);
    Procedure useProgram(program_ : TJSWebGLProgram);
    Procedure validateProgram(program_ : TJSWebGLProgram);
    Procedure vertexAttrib1f(index : GLuint; x : GLfloat);
    Procedure vertexAttrib2f(index : GLuint; x : GLfloat; y : GLfloat);
    Procedure vertexAttrib3f(index : GLuint; x : GLfloat; y : GLfloat; z : GLfloat);
    Procedure vertexAttrib4f(index : GLuint; x : GLfloat; y : GLfloat; z : GLfloat; w : GLfloat);
    Procedure vertexAttrib1fv(index : GLuint; values : Float32List);
    Procedure vertexAttrib2fv(index : GLuint; values : Float32List);
    Procedure vertexAttrib3fv(index : GLuint; values : Float32List);
    Procedure vertexAttrib4fv(index : GLuint; values : Float32List);
    Procedure vertexAttribPointer(index : GLuint; size : GLint; type_ : GLenum; normalized : GLboolean; stride : GLsizei; offset : GLintptr);
    Procedure viewport(x : GLint; y : GLint; width : GLsizei; height : GLsizei);
    Property canvas : TJSHTMLCanvasElement Read Fcanvas; 
    Property drawingBufferWidth : GLsizei Read FdrawingBufferWidth; 
    Property drawingBufferHeight : GLsizei Read FdrawingBufferHeight; 
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLRenderingContext
    --------------------------------------------------------------------}
  
  TJSWebGLRenderingContext = class external name 'WebGLRenderingContext'  (TJSWebGLRenderingContextBase)
  Private
  Public
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLContextEvent
    --------------------------------------------------------------------}
  
  TJSWebGLContextEvent = class external name 'WebGLContextEvent'  (TJSEvent)
  Private
    FstatusMessage : String; external name 'statusMessage'; 
  Public
    Property statusMessage : String Read FstatusMessage; 
  end;

implementation


end.
