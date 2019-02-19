unit websvg;

{$MODE ObjFPC}
{$H+}
{$MODESWITCH externalclass}

interface

uses SysUtils, web, JS;

type
  // Forward class
  TJSSVGSVGElement = class;
  TJSSVGNumberList = class;
  TJSSVGAngle = class;
  TJSSVGAnimatedAngle = class;
  TJSSVGAnimatedBoolean = class;
  TJSSVGAnimatedEnumeration = class;
  TJSSVGAnimatedInteger = class;
  TJSSVGAnimatedLength = class;
  TJSSVGAnimatedLengthList = class;
  TJSSVGAnimatedNumber = class;
  TJSSVGAnimatedNumberList = class;
  TJSSVGAnimatedPreserveAspectRatio = class;
  TJSSVGAnimatedRect = class;
  TJSSVGAnimatedString = class;
  TJSSVGAnimatedTransformList = class;
  TJSSVGAnimateElement = class;
  TJSSVGAnimateMotionElement = class;
  TJSSVGAnimateTransformElement = class;
  TJSSVGAnimationElement = class;
  TJSSVGCircleElement = class;
  TJSSVGClipPathElement = class;
  TJSSVGComponentTransferFunctionElement = class;
  TJSSVGDefsElement = class;
  TJSSVGDescElement = class;
  TJSSVGDiscardElement = class;
  TJSSVGDocument = class;
  TJSSVGElement = class;
  TJSSVGEllipseElement = class;
  TJSSVGFEBlendElement = class;
  TJSSVGFEColorMatrixElement = class;
  TJSSVGFEComponentTransferElement = class;
  TJSSVGFECompositeElement = class;
  TJSSVGFEConvolveMatrixElement = class;
  TJSSVGFEDiffuseLightingElement = class;
  TJSSVGFEDisplacementMapElement = class;
  TJSSVGFEDistantLightElement = class;
  TJSSVGFEDropShadowElement = class;
  TJSSVGFEFloodElement = class;
  TJSSVGFEFuncAElement = class;
  TJSSVGFEFuncBElement = class;
  TJSSVGFEFuncGElement = class;
  TJSSVGFEFuncRElement = class;
  TJSSVGFEGaussianBlurElement = class;
  TJSSVGFEImageElement = class;
  TJSSVGFEMergeElement = class;
  TJSSVGFEMergeNodeElement = class;
  TJSSVGFEMorphologyElement = class;
  TJSSVGFEOffsetElement = class;
  TJSSVGFEPointLightElement = class;
  TJSSVGFESpecularLightingElement = class;
  TJSSVGFESpotLightElement = class;
  TJSSVGFETileElement = class;
  TJSSVGFETurbulenceElement = class;
  TJSSVGFilterElement = class;
  TJSSVGFilterPrimitiveStandardAttributes = class;
  TJSSVGFitToViewBox = class;
  TJSSVGForeignObjectElement = class;
  TJSSVGGElement = class;
  TJSSVGGeometryElement = class;
  TJSSVGGradientElement = class;
  TJSSVGGraphicsElement = class;
  TJSSVGImageElement = class;
  TJSSVGLength = class;
  TJSSVGLinearGradientElement = class;
  TJSSVGLineElement = class;
  TJSSVGMarkerElement = class;
  TJSSVGMaskElement = class;
  TJSSVGMatrix = class;
  TJSSVGMetadataElement = class;
  TJSSVGMPathElement = class;
  TJSSVGNumber = class;
  TJSSVGPathElement = class;
  TJSSVGPatternElement = class;
  TJSSVGPoint = class;
  TJSSVGPolygonElement = class;
  TJSSVGPolylineElement = class;
  TJSSVGPreserveAspectRatio = class;
  TJSSVGRadialGradientElement = class;
  TJSSVGRect = class;
  TJSSVGRectElement = class;
  TJSSVGScriptElement = class;
  TJSSVGSetElement = class;
  TJSSVGStopElement = class;
  TJSSVGStyleElement = class;
  TJSSVGSwitchElement = class;
  TJSSVGSymbolElement = class;
  TJSSVGTests = class;
  TJSSVGTextContentElement = class;
  TJSSVGTextElement = class;
  TJSSVGTextPathElement = class;
  TJSSVGTextPositioningElement = class;
  TJSSVGTitleElement = class;
  TJSSVGTransform = class;
  TJSSVGTSpanElement = class;
  TJSSVGUnitTypes = class;
  TJSSVGURIReference = class;
  TJSSVGUseElement = class;
  TJSSVGViewElement = class;
  TJSSVGZoomAndPan = class;

type
  DOMStringMap = class external name 'DOMStringMap'
  private
    function GetItem(name: String): String; external name '[]';
    procedure SetItem(name: String; value: String); external name '[]';
  public
    //procedure (name: String);
    property Items[name: String]: String read GetItem write SetItem; default;
  end;

  { --------------------------------------------------------------------
    TJSSVGElement
    --------------------------------------------------------------------}
  TJSSVGElement = class external name 'SVGElement'(TJSElement)
  private
    FclassName: TJSSVGAnimatedString; external name 'className';
    Fdataset: DOMStringMap; external name 'dataset';
    Fstyle: TJSCSSStyleDeclaration; external name 'style';
    FownerSVGElement: TJSSVGSVGElement; external name 'ownerSVGElement';
    FviewportElement: TJSSVGElement; external name 'viewportElement';
  public
    tabIndex: Integer;
    procedure focus;
    procedure blur;
    property className: TJSSVGAnimatedString read FclassName;
    property dataset: DOMStringMap read Fdataset;
    property style: TJSCSSStyleDeclaration read Fstyle;
    property ownerSVGElement: TJSSVGSVGElement read FownerSVGElement;
    property viewportElement: TJSSVGElement read FviewportElement;
  end;

  { --------------------------------------------------------------------
    TJSSVGGraphicsElement
    --------------------------------------------------------------------}
  TJSSVGGraphicsElement = class external name 'SVGGraphicsElement' (TJSSVGElement)
  private
    Ftransform: TJSSVGAnimatedTransformList; external name 'transform';
    FnearestViewportElement: TJSSVGElement; external name 'nearestViewportElement';
    FfarthestViewportElement: TJSSVGElement; external name
      'farthestViewportElement';
  public
    function getBBox: TJSSVGRect;
    function getCTM: TJSSVGMatrix;
    function getScreenCTM: TJSSVGMatrix;
    property transform: TJSSVGAnimatedTransformList read Ftransform;
    property nearestViewportElement: TJSSVGElement read FnearestViewportElement;
    property farthestViewportElement: TJSSVGElement read FfarthestViewportElement;
  end;

  { --------------------------------------------------------------------
    TJSSVGSVGElement
    --------------------------------------------------------------------}
  TJSSVGSVGElement = class external name 'SVGSVGElement' (TJSSVGGraphicsElement)
  private
    Fx: TJSSVGAnimatedLength; external name 'x';
    Fy: TJSSVGAnimatedLength; external name 'y';
    Fwidth: TJSSVGAnimatedLength; external name 'width';
    Fheight: TJSSVGAnimatedLength; external name 'height';
    FcurrentTranslate: TJSSVGPoint; external name 'currentTranslate';
  public
    currentScale: Double;
    function getIntersectionList(rect: TJSSVGRect; referenceElement: TJSSVGElement):
      TJSNodeList;
    function getEnclosureList(rect: TJSSVGRect; referenceElement: TJSSVGElement):
      TJSNodeList;
    function checkIntersection(element: TJSSVGElement; rect: TJSSVGRect): boolean;
    function checkEnclosure(element: TJSSVGElement; rect: TJSSVGRect): boolean;
    procedure deselectAll;
    function createSVGNumber: TJSSVGNumber;
    function createSVGLength: TJSSVGLength;
    function createSVGAngle: TJSSVGAngle;
    function createSVGPoint: TJSSVGPoint;
    function createSVGMatrix: TJSSVGMatrix;
    function createSVGRect: TJSSVGRect;
    function createSVGTransform: TJSSVGTransform;
    function createSVGTransformFromMatrix(matrix: TJSSVGMatrix): TJSSVGTransform;
    function getElementById(elementId: string): TJSElement;
    function suspendRedraw(maxWaitMilliseconds: NativeInt): NativeInt;
    procedure unsuspendRedraw(suspendHandleId: NativeInt);
    procedure unsuspendRedrawAll;
    procedure forceRedraw;
    procedure pauseAnimations;
    procedure unpauseAnimations;
    function animationsPaused: boolean;
    function getCurrentTime: Double;
    procedure setCurrentTime(seconds: Double);
    property x: TJSSVGAnimatedLength read Fx;
    property y: TJSSVGAnimatedLength read Fy;
    property width: TJSSVGAnimatedLength read Fwidth;
    property height: TJSSVGAnimatedLength read Fheight;
    property currentTranslate: TJSSVGPoint read FcurrentTranslate;
  end;

  { --------------------------------------------------------------------
    TJSSVGAElement
    --------------------------------------------------------------------}
  TJSSVGAElement = class external name 'SVGAElement' (TJSSVGGraphicsElement)
  private
    Ftarget: TJSSVGAnimatedString; external name 'target';
  public
    property target: TJSSVGAnimatedString read Ftarget;
  end;

  { --------------------------------------------------------------------
  TJSSVGAngle
  --------------------------------------------------------------------}
  TJSSVGAngle = class external name 'SVGAngle'
  private
    FunitType: Cardinal; external name 'unitType';
  public
  const
    SVG_ANGLETYPE_UNKNOWN = 0;
    SVG_ANGLETYPE_UNSPECIFIED = 1;
    SVG_ANGLETYPE_DEG = 2;
    SVG_ANGLETYPE_RAD = 3;
    SVG_ANGLETYPE_GRAD = 4;
    Public
      value: Double;
    valueInSpecifiedUnits: Double;
    valueAsString: string;
    procedure newValueSpecifiedUnits(unitType: Cardinal; valueInSpecifiedUnits: Double);
    procedure convertToSpecifiedUnits(unitType: Cardinal);
    property unitType: Cardinal read FunitType;
  end;

  { --------------------------------------------------------------------
    TJSSVGAnimatedAngle
    --------------------------------------------------------------------}
  TJSSVGAnimatedAngle = class external name 'SVGAnimatedAngle'
  private
    FbaseVal: TJSSVGAngle; external name 'baseVal';
    FanimVal: TJSSVGAngle; external name 'animVal';
  public
    property baseVal: TJSSVGAngle read FbaseVal;
    property animVal: TJSSVGAngle read FanimVal;
  end;

  { --------------------------------------------------------------------
    TJSSVGAnimatedBoolean
    --------------------------------------------------------------------}
  TJSSVGAnimatedBoolean = class external name 'SVGAnimatedBoolean'
  private
    FanimVal: boolean; external name 'animVal';
  public
    baseVal: boolean;
    property animVal: boolean read FanimVal;
  end;

  { --------------------------------------------------------------------
    TJSSVGAnimatedEnumeration
    --------------------------------------------------------------------}
  TJSSVGAnimatedEnumeration = class external name 'SVGAnimatedEnumeration'
  private
    FanimVal: Cardinal; external name 'animVal';
  public
    baseVal: Cardinal;
    property animVal: Cardinal read FanimVal;
  end;

  { --------------------------------------------------------------------
    TJSSVGAnimatedInteger
    --------------------------------------------------------------------}
  TJSSVGAnimatedInteger = class external name 'SVGAnimatedInteger'
  private
    FanimVal: Integer; external name 'animVal';
  public
    baseVal: Integer;
    property animVal: Integer read FanimVal;
  end;

  { --------------------------------------------------------------------
    TJSSVGAnimatedLength
    --------------------------------------------------------------------}
  TJSSVGAnimatedLength = class external name 'SVGAnimatedLength'
  private
    FbaseVal: TJSSVGLength; external name 'baseVal';
    FanimVal: TJSSVGLength; external name 'animVal';
  public
    property baseVal: TJSSVGLength read FbaseVal;
    property animVal: TJSSVGLength read FanimVal;
  end;

{ --------------------------------------------------------------------
  TJSSVGLengthList
  --------------------------------------------------------------------}
  TJSSVGLengthList = class external name 'SVGLengthList'
  public
    numberOfItems: Integer;
    procedure clear;
    function initialize(newItem: TJSSVGLength): TJSSVGLength;
    function getItem(&index: Integer): TJSSVGLength;
    function insertItemBefore(newItem: TJSSVGLength; &index: Integer): TJSSVGLength;
    function replaceItem(newItem: TJSSVGLength; &index: Integer): TJSSVGLength;
    function removeItem(&index: Integer): TJSSVGLength;
    function appendItem(newItem: TJSSVGLength): TJSSVGLength;
  end;

  { --------------------------------------------------------------------
    TJSSVGAnimatedLengthList
    --------------------------------------------------------------------}
  TJSSVGAnimatedLengthList = class external name 'SVGAnimatedLengthList'
  private
    FbaseVal: TJSSVGLengthList; external name 'baseVal';
    FanimVal: TJSSVGLengthList; external name 'animVal';
  public
    property baseVal: TJSSVGLengthList read FbaseVal;
    property animVal: TJSSVGLengthList read FanimVal;
  end;

  { --------------------------------------------------------------------
    TJSSVGAnimatedNumber
    --------------------------------------------------------------------}
  TJSSVGAnimatedNumber = class external name 'SVGAnimatedNumber'
  private
    FanimVal: Double; external name 'animVal';
  public
    baseVal: Double;
    property animVal: Double read FanimVal;
  end;

  { --------------------------------------------------------------------
    TJSSVGNumberList
  --------------------------------------------------------------------}
  TJSSVGNumberList = class external name 'SVGNumberList'
  public
    numberOfItems: Integer;
    procedure clear;
    function initialize(newItem: TJSSVGNumber): TJSSVGNumber;
    function getItem(&index: Integer): TJSSVGNumber;
    function insertItemBefore(newItem: TJSSVGNumber; &index: Integer): TJSSVGNumber;
    function replaceItem(newItem: TJSSVGNumber; &index: Integer): TJSSVGNumber;
    function removeItem(&index: Integer): TJSSVGNumber;
    function appendItem(newItem: TJSSVGNumber): TJSSVGNumber;
  end;

  { --------------------------------------------------------------------
    TJSSVGAnimatedNumberList
    --------------------------------------------------------------------}
  TJSSVGAnimatedNumberList = class external name 'SVGAnimatedNumberList'
  private
    FbaseVal: TJSSVGNumberList; external name 'baseVal';
    FanimVal: TJSSVGNumberList; external name 'animVal';
  public
    property baseVal: TJSSVGNumberList read FbaseVal;
    property animVal: TJSSVGNumberList read FanimVal;
  end;

  { --------------------------------------------------------------------
    TJSSVGAnimatedPreserveAspectRatio
    --------------------------------------------------------------------}
  TJSSVGAnimatedPreserveAspectRatio = class external name
    'SVGAnimatedPreserveAspectRatio'
    private
    FbaseVal: TJSSVGPreserveAspectRatio; external name 'baseVal';
    FanimVal: TJSSVGPreserveAspectRatio; external name 'animVal';
  public
    property baseVal: TJSSVGPreserveAspectRatio read FbaseVal;
    property animVal: TJSSVGPreserveAspectRatio read FanimVal;
  end;

  { --------------------------------------------------------------------
    TJSSVGAnimatedRect
    --------------------------------------------------------------------}
  TJSSVGAnimatedRect = class external name 'SVGAnimatedRect'
  private
    FbaseVal: TJSSVGRect; external name 'baseVal';
    FanimVal: TJSSVGRect; external name 'animVal';
  public
    property baseVal: TJSSVGRect read FbaseVal;
    property animVal: TJSSVGRect read FanimVal;
  end;

  { --------------------------------------------------------------------
    TJSSVGAnimatedString
    --------------------------------------------------------------------}
  TJSSVGAnimatedString = class external name 'SVGAnimatedString'
  private
    FanimVal: string; external name 'animVal';
  public
    baseVal: string;
    property animVal: string read FanimVal;
  end;

 { --------------------------------------------------------------------
  TJSSVGTransformList
  --------------------------------------------------------------------}
  TJSSVGTransformList = class external name 'SVGTransformList'
  public
    numberOfItems: Integer;
    procedure clear;
    function initialize(newItem: TJSSVGTransform): TJSSVGTransform;
    function getItem(&index: Integer): TJSSVGTransform;
    function insertItemBefore(newItem: TJSSVGTransform; &index: Integer): TJSSVGTransform;
    function replaceItem(newItem: TJSSVGTransform; &index: Integer): TJSSVGTransform;
    function removeItem(&index: Integer): TJSSVGTransform;
    function appendItem(newItem: TJSSVGTransform): TJSSVGTransform;
    function createSVGTransformFromMatrix(matrix: TJSSVGMatrix): TJSSVGTransform;
    function consolidate: TJSSVGTransform;
  end;

  { --------------------------------------------------------------------
    TJSSVGAnimatedTransformList
    --------------------------------------------------------------------}
  TJSSVGAnimatedTransformList = class external name 'SVGAnimatedTransformList'
  private
    FbaseVal: TJSSVGTransformList; external name 'baseVal';
    FanimVal: TJSSVGTransformList; external name 'animVal';
  public
    property baseVal: TJSSVGTransformList read FbaseVal;
    property animVal: TJSSVGTransformList read FanimVal;
  end;

  { --------------------------------------------------------------------
    TJSSVGAnimationElement
    --------------------------------------------------------------------}
  TJSSVGAnimationElement = class external name 'SVGAnimationElement' (TJSSVGElement)
  private
    FtargetElement: TJSSVGElement; external name 'targetElement';
  public
    onbegin: TJSEventHandler;
    onend: TJSEventHandler;
    onrepeat: TJSEventHandler;
    function getStartTime: Double;
    function getCurrentTime: Double;
    function getSimpleDuration: Double;
    procedure beginElement;
    procedure beginElementAt(offset: Double);
    procedure endElement;
    procedure endElementAt(offset: Double);
    property targetElement: TJSSVGElement read FtargetElement;
  end;

  { --------------------------------------------------------------------
    TJSSVGAnimateElement
    --------------------------------------------------------------------}
  TJSSVGAnimateElement = class external name 'SVGAnimateElement' (TJSSVGAnimationElement)
    private
  public
  end;

  { --------------------------------------------------------------------
    TJSSVGAnimateMotionElement
    --------------------------------------------------------------------}
  TJSSVGAnimateMotionElement = class external name 'SVGAnimateMotionElement' (TJSSVGAnimationElement)
    private
  public
  end;

  { --------------------------------------------------------------------
    TJSSVGAnimateTransformElement
    --------------------------------------------------------------------}
  TJSSVGAnimateTransformElement = class external name
    'SVGAnimateTransformElement' (TJSSVGAnimationElement)
    private
  public
  end;

  { --------------------------------------------------------------------
    TJSSVGGeometryElement
    --------------------------------------------------------------------}
  TJSSVGGeometryElement = class external name 'SVGGeometryElement' (TJSSVGGraphicsElement)
    private
    FpathLength: TJSSVGAnimatedNumber; external name 'pathLength';
  public
    function isPointInFill(point: TJSSVGPoint): boolean;
    function isPointInStroke(point: TJSSVGPoint): boolean;
    function getTotalLength: Double;
    function getPointAtLength(distance: Double): TJSSVGPoint;
    property pathLength: TJSSVGAnimatedNumber read FpathLength;
  end;

  { --------------------------------------------------------------------
    TJSSVGCircleElement
    --------------------------------------------------------------------}
  TJSSVGCircleElement = class external name 'SVGCircleElement' (TJSSVGGeometryElement)
    private
    Fcx: TJSSVGAnimatedLength; external name 'cx';
    Fcy: TJSSVGAnimatedLength; external name 'cy';
    Fr: TJSSVGAnimatedLength; external name 'r';
  public
    property cx: TJSSVGAnimatedLength read Fcx;
    property cy: TJSSVGAnimatedLength read Fcy;
    property r: TJSSVGAnimatedLength read Fr;
  end;

  { --------------------------------------------------------------------
    TJSSVGClipPathElement
    --------------------------------------------------------------------}
  TJSSVGClipPathElement = class external name
    'SVGClipPathElement' (TJSSVGGraphicsElement)
    private
    FclipPathUnits: TJSSVGAnimatedEnumeration; external name 'clipPathUnits';
  public
    property clipPathUnits: TJSSVGAnimatedEnumeration read FclipPathUnits;
  end;

  { --------------------------------------------------------------------
    TJSSVGComponentTransferFunctionElement
    --------------------------------------------------------------------}
  TJSSVGComponentTransferFunctionElement = class external name
    'SVGComponentTransferFunctionElement' (TJSSVGElement)
    private
    Ftype_: TJSSVGAnimatedEnumeration; external name 'type';
    FtableValues: TJSSVGAnimatedNumberList; external name 'tableValues';
    Fslope: TJSSVGAnimatedNumber; external name 'slope';
    Fintercept: TJSSVGAnimatedNumber; external name 'intercept';
    Famplitude: TJSSVGAnimatedNumber; external name 'amplitude';
    Fexponent: TJSSVGAnimatedNumber; external name 'exponent';
    Foffset: TJSSVGAnimatedNumber; external name 'offset';
  public
  const
    SVG_FECOMPONENTTRANSFER_TYPE_UNKNOWN = 0;
    SVG_FECOMPONENTTRANSFER_TYPE_IDENTITY = 1;
    SVG_FECOMPONENTTRANSFER_TYPE_TABLE = 2;
    SVG_FECOMPONENTTRANSFER_TYPE_DISCRETE = 3;
    SVG_FECOMPONENTTRANSFER_TYPE_LINEAR = 4;
    SVG_FECOMPONENTTRANSFER_TYPE_GAMMA = 5;
  Public
    property type_: TJSSVGAnimatedEnumeration read Ftype_;
    property tableValues: TJSSVGAnimatedNumberList read FtableValues;
    property slope: TJSSVGAnimatedNumber read Fslope;
    property intercept: TJSSVGAnimatedNumber read Fintercept;
    property amplitude: TJSSVGAnimatedNumber read Famplitude;
    property exponent: TJSSVGAnimatedNumber read Fexponent;
    property offset: TJSSVGAnimatedNumber read Foffset;
  end;

  { --------------------------------------------------------------------
    TJSSVGDefsElement
    --------------------------------------------------------------------}
  TJSSVGDefsElement = class external name 'SVGDefsElement' (TJSSVGGraphicsElement)
  private
  public
  end;

  { --------------------------------------------------------------------
    TJSSVGDescElement
    --------------------------------------------------------------------}
  TJSSVGDescElement = class external name 'SVGDescElement' (TJSSVGElement)
  private
  public
  end;

  { --------------------------------------------------------------------
    TJSSVGDocument
    --------------------------------------------------------------------}
  TJSSVGDocument = class external name 'SVGDocument' (TJSDocument)
  public
    Ftitle: String; external name 'tile';
    Freferrer: String; external name 'referrer';
    Fdomain: String; external name 'domain';
    fURL: String; external name 'URL';
    rootElement: TJSSVGSVGElement;
  end;

  { --------------------------------------------------------------------
    TJSSVGDiscardElement
    --------------------------------------------------------------------}
  TJSSVGDiscardElement = class external name 'SVGDiscardElement' (TJSSVGElement)
  private
  public
  end;

  { --------------------------------------------------------------------
    TJSDocument
    --------------------------------------------------------------------}
  TJSDocument = class external name 'Document'
  private
    FrootElement: TJSSVGSVGElement; external name 'rootElement';
  public
    property rootElement: TJSSVGSVGElement read FrootElement;
  end;

  { --------------------------------------------------------------------
    TJSSVGEllipseElement
    --------------------------------------------------------------------}
  TJSSVGEllipseElement = class external name
    'SVGEllipseElement' (TJSSVGGeometryElement)
    private
    Fcx: TJSSVGAnimatedLength; external name 'cx';
    Fcy: TJSSVGAnimatedLength; external name 'cy';
    Frx: TJSSVGAnimatedLength; external name 'rx';
    Fry: TJSSVGAnimatedLength; external name 'ry';
  public
    property cx: TJSSVGAnimatedLength read Fcx;
    property cy: TJSSVGAnimatedLength read Fcy;
    property rx: TJSSVGAnimatedLength read Frx;
    property ry: TJSSVGAnimatedLength read Fry;
  end;

  { --------------------------------------------------------------------
    TJSSVGFEBlendElement
    --------------------------------------------------------------------}
  TJSSVGFEBlendElement = class external name 'SVGFEBlendElement' (TJSSVGElement)
  private
    Fin1: TJSSVGAnimatedString; external name 'in1';
    Fin2: TJSSVGAnimatedString; external name 'in2';
    Fmode: TJSSVGAnimatedEnumeration; external name 'mode';
  public
  const
    SVG_FEBLEND_MODE_UNKNOWN = 0;
    SVG_FEBLEND_MODE_NORMAL = 1;
    SVG_FEBLEND_MODE_MULTIPLY = 2;
    SVG_FEBLEND_MODE_SCREEN = 3;
    SVG_FEBLEND_MODE_DARKEN = 4;
    SVG_FEBLEND_MODE_LIGHTEN = 5;
  Public
    property in1: TJSSVGAnimatedString read Fin1;
    property in2: TJSSVGAnimatedString read Fin2;
    property mode: TJSSVGAnimatedEnumeration read Fmode;
  end;

  { --------------------------------------------------------------------
    TJSSVGFEColorMatrixElement
    --------------------------------------------------------------------}
  TJSSVGFEColorMatrixElement = class external name
    'SVGFEColorMatrixElement' (TJSSVGElement)
    private
    Fin1: TJSSVGAnimatedString; external name 'in1';
    Ftype_: TJSSVGAnimatedEnumeration; external name 'type';
    Fvalues: TJSSVGAnimatedNumberList; external name 'values';
  public
  const
    SVG_FECOLORMATRIX_TYPE_UNKNOWN = 0;
    SVG_FECOLORMATRIX_TYPE_MATRIX = 1;
    SVG_FECOLORMATRIX_TYPE_SATURATE = 2;
    SVG_FECOLORMATRIX_TYPE_HUEROTATE = 3;
    SVG_FECOLORMATRIX_TYPE_LUMINANCETOALPHA = 4;
  Public
    property in1: TJSSVGAnimatedString read Fin1;
    property type_: TJSSVGAnimatedEnumeration read Ftype_;
    property values: TJSSVGAnimatedNumberList read Fvalues;
  end;

  { --------------------------------------------------------------------
    TJSSVGFEComponentTransferElement
    --------------------------------------------------------------------}
  TJSSVGFEComponentTransferElement = class external name
    'SVGFEComponentTransferElement' (TJSSVGElement)
    private
    Fin1: TJSSVGAnimatedString; external name 'in1';
  public
    property in1: TJSSVGAnimatedString read Fin1;
  end;

  { --------------------------------------------------------------------
    TJSSVGFECompositeElement
    --------------------------------------------------------------------}
  TJSSVGFECompositeElement = class external name
    'SVGFECompositeElement' (TJSSVGElement)
    private
    Fin2: TJSSVGAnimatedString; external name 'in2';
    Fin1: TJSSVGAnimatedString; external name 'in1';
    Foperator_: TJSSVGAnimatedEnumeration; external name 'operator';
    Fk1: TJSSVGAnimatedNumber; external name 'k1';
    Fk2: TJSSVGAnimatedNumber; external name 'k2';
    Fk3: TJSSVGAnimatedNumber; external name 'k3';
    Fk4: TJSSVGAnimatedNumber; external name 'k4';
  public
  const
    SVG_FECOMPOSITE_OPERATOR_UNKNOWN = 0;
    SVG_FECOMPOSITE_OPERATOR_OVER = 1;
    SVG_FECOMPOSITE_OPERATOR_IN = 2;
    SVG_FECOMPOSITE_OPERATOR_OUT = 3;
    SVG_FECOMPOSITE_OPERATOR_ATOP = 4;
    SVG_FECOMPOSITE_OPERATOR_XOR = 5;
    SVG_FECOMPOSITE_OPERATOR_ARITHMETIC = 6;
  Public
    property in2: TJSSVGAnimatedString read Fin2;
    property in1: TJSSVGAnimatedString read Fin1;
    property operator_: TJSSVGAnimatedEnumeration read Foperator_;
    property k1: TJSSVGAnimatedNumber read Fk1;
    property k2: TJSSVGAnimatedNumber read Fk2;
    property k3: TJSSVGAnimatedNumber read Fk3;
    property k4: TJSSVGAnimatedNumber read Fk4;
  end;

  { --------------------------------------------------------------------
    TJSSVGFEConvolveMatrixElement
    --------------------------------------------------------------------}
  TJSSVGFEConvolveMatrixElement = class external name
    'SVGFEConvolveMatrixElement' (TJSSVGElement)
    private
    Fin1: TJSSVGAnimatedString; external name 'in1';
    ForderX: TJSSVGAnimatedInteger; external name 'orderX';
    ForderY: TJSSVGAnimatedInteger; external name 'orderY';
    FkernelMatrix: TJSSVGAnimatedNumberList; external name 'kernelMatrix';
    Fdivisor: TJSSVGAnimatedNumber; external name 'divisor';
    Fbias: TJSSVGAnimatedNumber; external name 'bias';
    FtargetX: TJSSVGAnimatedInteger; external name 'targetX';
    FtargetY: TJSSVGAnimatedInteger; external name 'targetY';
    FedgeMode: TJSSVGAnimatedEnumeration; external name 'edgeMode';
    FkernelUnitLengthX: TJSSVGAnimatedNumber; external name 'kernelUnitLengthX';
    FkernelUnitLengthY: TJSSVGAnimatedNumber; external name 'kernelUnitLengthY';
    FpreserveAlpha: TJSSVGAnimatedBoolean; external name 'preserveAlpha';
  public
  const
    SVG_EDGEMODE_UNKNOWN = 0;
    SVG_EDGEMODE_DUPLICATE = 1;
    SVG_EDGEMODE_WRAP = 2;
    SVG_EDGEMODE_NONE = 3;
  Public
    property in1: TJSSVGAnimatedString read Fin1;
    property orderX: TJSSVGAnimatedInteger read ForderX;
    property orderY: TJSSVGAnimatedInteger read ForderY;
    property kernelMatrix: TJSSVGAnimatedNumberList read FkernelMatrix;
    property divisor: TJSSVGAnimatedNumber read Fdivisor;
    property bias: TJSSVGAnimatedNumber read Fbias;
    property targetX: TJSSVGAnimatedInteger read FtargetX;
    property targetY: TJSSVGAnimatedInteger read FtargetY;
    property edgeMode: TJSSVGAnimatedEnumeration read FedgeMode;
    property kernelUnitLengthX: TJSSVGAnimatedNumber read FkernelUnitLengthX;
    property kernelUnitLengthY: TJSSVGAnimatedNumber read FkernelUnitLengthY;
    property preserveAlpha: TJSSVGAnimatedBoolean read FpreserveAlpha;
  end;

  { --------------------------------------------------------------------
    TJSSVGFEDiffuseLightingElement
    --------------------------------------------------------------------}
  TJSSVGFEDiffuseLightingElement = class external name
    'SVGFEDiffuseLightingElement' (TJSSVGElement)
    private
    Fin1: TJSSVGAnimatedString; external name 'in1';
    FsurfaceScale: TJSSVGAnimatedNumber; external name 'surfaceScale';
    FdiffuseConstant: TJSSVGAnimatedNumber; external name 'diffuseConstant';
    FkernelUnitLengthX: TJSSVGAnimatedNumber; external name 'kernelUnitLengthX';
    FkernelUnitLengthY: TJSSVGAnimatedNumber; external name 'kernelUnitLengthY';
  public
    property in1: TJSSVGAnimatedString read Fin1;
    property surfaceScale: TJSSVGAnimatedNumber read FsurfaceScale;
    property diffuseConstant: TJSSVGAnimatedNumber read FdiffuseConstant;
    property kernelUnitLengthX: TJSSVGAnimatedNumber read FkernelUnitLengthX;
    property kernelUnitLengthY: TJSSVGAnimatedNumber read FkernelUnitLengthY;
  end;

  { --------------------------------------------------------------------
    TJSSVGFEDisplacementMapElement
    --------------------------------------------------------------------}
  TJSSVGFEDisplacementMapElement = class external name
    'SVGFEDisplacementMapElement' (TJSSVGElement)
    private
    Fin1: TJSSVGAnimatedString; external name 'in1';
    Fin2: TJSSVGAnimatedString; external name 'in2';
    Fscale: TJSSVGAnimatedNumber; external name 'scale';
    FxChannelSelector: TJSSVGAnimatedEnumeration; external name 'xChannelSelector';
    FyChannelSelector: TJSSVGAnimatedEnumeration; external name 'yChannelSelector';
  public
  const
    SVG_CHANNEL_UNKNOWN = 0;
    SVG_CHANNEL_R = 1;
    SVG_CHANNEL_G = 2;
    SVG_CHANNEL_B = 3;
    SVG_CHANNEL_A = 4;
  Public
    property in1: TJSSVGAnimatedString read Fin1;
    property in2: TJSSVGAnimatedString read Fin2;
    property scale: TJSSVGAnimatedNumber read Fscale;
    property xChannelSelector: TJSSVGAnimatedEnumeration read FxChannelSelector;
    property yChannelSelector: TJSSVGAnimatedEnumeration read FyChannelSelector;
  end;

  { --------------------------------------------------------------------
    TJSSVGFEDistantLightElement
    --------------------------------------------------------------------}
  TJSSVGFEDistantLightElement = class external name
    'SVGFEDistantLightElement' (TJSSVGElement)
    private
    Fazimuth: TJSSVGAnimatedNumber; external name 'azimuth';
    Felevation: TJSSVGAnimatedNumber; external name 'elevation';
  public
    property azimuth: TJSSVGAnimatedNumber read Fazimuth;
    property elevation: TJSSVGAnimatedNumber read Felevation;
  end;

  { --------------------------------------------------------------------
    TJSSVGFEDropShadowElement
    --------------------------------------------------------------------}
  TJSSVGFEDropShadowElement = class external name
    'SVGFEDropShadowElement' (TJSSVGElement)
    private
    Fin1: TJSSVGAnimatedString; external name 'in1';
    Fdx: TJSSVGAnimatedNumber; external name 'dx';
    Fdy: TJSSVGAnimatedNumber; external name 'dy';
    FstdDeviationX: TJSSVGAnimatedNumber; external name 'stdDeviationX';
    FstdDeviationY: TJSSVGAnimatedNumber; external name 'stdDeviationY';
  public
    procedure setStdDeviation(stdDeviationX: Double; stdDeviationY: Double);
    property in1: TJSSVGAnimatedString read Fin1;
    property dx: TJSSVGAnimatedNumber read Fdx;
    property dy: TJSSVGAnimatedNumber read Fdy;
    property stdDeviationX: TJSSVGAnimatedNumber read FstdDeviationX;
    property stdDeviationY: TJSSVGAnimatedNumber read FstdDeviationY;
  end;

  { --------------------------------------------------------------------
    TJSSVGFEFloodElement
    --------------------------------------------------------------------}
  TJSSVGFEFloodElement = class external name 'SVGFEFloodElement' (TJSSVGElement)
  private
  public
  end;

  { --------------------------------------------------------------------
    TJSSVGFEFuncAElement
    --------------------------------------------------------------------}
  TJSSVGFEFuncAElement = class external name
    'SVGFEFuncAElement' (TJSSVGComponentTransferFunctionElement)
    private
  public
  end;

  { --------------------------------------------------------------------
    TJSSVGFEFuncBElement
    --------------------------------------------------------------------}
  TJSSVGFEFuncBElement = class external name
    'SVGFEFuncBElement' (TJSSVGComponentTransferFunctionElement)
    private
  public
  end;

  { --------------------------------------------------------------------
    TJSSVGFEFuncGElement
    --------------------------------------------------------------------}
  TJSSVGFEFuncGElement = class external name
    'SVGFEFuncGElement' (TJSSVGComponentTransferFunctionElement)
    private
  public
  end;

  { --------------------------------------------------------------------
    TJSSVGFEFuncRElement
    --------------------------------------------------------------------}
  TJSSVGFEFuncRElement = class external name
    'SVGFEFuncRElement' (TJSSVGComponentTransferFunctionElement)
    private
  public
  end;

  { --------------------------------------------------------------------
    TJSSVGFEGaussianBlurElement
    --------------------------------------------------------------------}
  TJSSVGFEGaussianBlurElement = class external name
    'SVGFEGaussianBlurElement' (TJSSVGElement)
    private
    Fin1: TJSSVGAnimatedString; external name 'in1';
    FstdDeviationX: TJSSVGAnimatedNumber; external name 'stdDeviationX';
    FstdDeviationY: TJSSVGAnimatedNumber; external name 'stdDeviationY';
  public
    procedure setStdDeviation(stdDeviationX: Double; stdDeviationY: Double);
    property in1: TJSSVGAnimatedString read Fin1;
    property stdDeviationX: TJSSVGAnimatedNumber read FstdDeviationX;
    property stdDeviationY: TJSSVGAnimatedNumber read FstdDeviationY;
  end;

  { --------------------------------------------------------------------
    TJSSVGFEImageElement
    --------------------------------------------------------------------}
  TJSSVGFEImageElement = class external name 'SVGFEImageElement' (TJSSVGElement)
  private
    FpreserveAspectRatio: TJSSVGAnimatedPreserveAspectRatio; external name
      'preserveAspectRatio';
  public
    property preserveAspectRatio: TJSSVGAnimatedPreserveAspectRatio read
      FpreserveAspectRatio;
  end;

  { --------------------------------------------------------------------
    TJSSVGFEMergeElement
    --------------------------------------------------------------------}
  TJSSVGFEMergeElement = class external name 'SVGFEMergeElement' (TJSSVGElement)
  private
  public
  end;

  { --------------------------------------------------------------------
    TJSSVGFEMergeNodeElement
    --------------------------------------------------------------------}
  TJSSVGFEMergeNodeElement = class external name
    'SVGFEMergeNodeElement' (TJSSVGElement)
    private
    Fin1: TJSSVGAnimatedString; external name 'in1';
  public
    property in1: TJSSVGAnimatedString read Fin1;
  end;

  { --------------------------------------------------------------------
    TJSSVGFEMorphologyElement
    --------------------------------------------------------------------}
  TJSSVGFEMorphologyElement = class external name
    'SVGFEMorphologyElement' (TJSSVGElement)
    private
    Fin1: TJSSVGAnimatedString; external name 'in1';
    Foperator_: TJSSVGAnimatedEnumeration; external name 'operator';
    FradiusX: TJSSVGAnimatedNumber; external name 'radiusX';
    FradiusY: TJSSVGAnimatedNumber; external name 'radiusY';
  public
  const
    SVG_MORPHOLOGY_OPERATOR_UNKNOWN = 0;
    SVG_MORPHOLOGY_OPERATOR_ERODE = 1;
    SVG_MORPHOLOGY_OPERATOR_DILATE = 2;
  Public
    property in1: TJSSVGAnimatedString read Fin1;
    property operator_: TJSSVGAnimatedEnumeration read Foperator_;
    property radiusX: TJSSVGAnimatedNumber read FradiusX;
    property radiusY: TJSSVGAnimatedNumber read FradiusY;
  end;

  { --------------------------------------------------------------------
    TJSSVGFEOffsetElement
    --------------------------------------------------------------------}
  TJSSVGFEOffsetElement = class external name 'SVGFEOffsetElement' (TJSSVGElement)
  private
    Fin1: TJSSVGAnimatedString; external name 'in1';
    Fdx: TJSSVGAnimatedNumber; external name 'dx';
    Fdy: TJSSVGAnimatedNumber; external name 'dy';
  public
    property in1: TJSSVGAnimatedString read Fin1;
    property dx: TJSSVGAnimatedNumber read Fdx;
    property dy: TJSSVGAnimatedNumber read Fdy;
  end;

  { --------------------------------------------------------------------
    TJSSVGFEPointLightElement
    --------------------------------------------------------------------}
  TJSSVGFEPointLightElement = class external name
    'SVGFEPointLightElement' (TJSSVGElement)
    private
    Fx: TJSSVGAnimatedNumber; external name 'x';
    Fy: TJSSVGAnimatedNumber; external name 'y';
    Fz: TJSSVGAnimatedNumber; external name 'z';
  public
    property x: TJSSVGAnimatedNumber read Fx;
    property y: TJSSVGAnimatedNumber read Fy;
    property z: TJSSVGAnimatedNumber read Fz;
  end;

  { --------------------------------------------------------------------
    TJSSVGFESpecularLightingElement
    --------------------------------------------------------------------}
  TJSSVGFESpecularLightingElement = class external name
    'SVGFESpecularLightingElement' (TJSSVGElement)
    private
    Fin1: TJSSVGAnimatedString; external name 'in1';
    FsurfaceScale: TJSSVGAnimatedNumber; external name 'surfaceScale';
    FspecularConstant: TJSSVGAnimatedNumber; external name 'specularConstant';
    FspecularExponent: TJSSVGAnimatedNumber; external name 'specularExponent';
    FkernelUnitLengthX: TJSSVGAnimatedNumber; external name 'kernelUnitLengthX';
    FkernelUnitLengthY: TJSSVGAnimatedNumber; external name 'kernelUnitLengthY';
  public
    property in1: TJSSVGAnimatedString read Fin1;
    property surfaceScale: TJSSVGAnimatedNumber read FsurfaceScale;
    property specularConstant: TJSSVGAnimatedNumber read FspecularConstant;
    property specularExponent: TJSSVGAnimatedNumber read FspecularExponent;
    property kernelUnitLengthX: TJSSVGAnimatedNumber read FkernelUnitLengthX;
    property kernelUnitLengthY: TJSSVGAnimatedNumber read FkernelUnitLengthY;
  end;

  { --------------------------------------------------------------------
    TJSSVGFESpotLightElement
    --------------------------------------------------------------------}
  TJSSVGFESpotLightElement = class external name
    'SVGFESpotLightElement' (TJSSVGElement)
    private
    Fx: TJSSVGAnimatedNumber; external name 'x';
    Fy: TJSSVGAnimatedNumber; external name 'y';
    Fz: TJSSVGAnimatedNumber; external name 'z';
    FpointsAtX: TJSSVGAnimatedNumber; external name 'pointsAtX';
    FpointsAtY: TJSSVGAnimatedNumber; external name 'pointsAtY';
    FpointsAtZ: TJSSVGAnimatedNumber; external name 'pointsAtZ';
    FspecularExponent: TJSSVGAnimatedNumber; external name 'specularExponent';
    FlimitingConeAngle: TJSSVGAnimatedNumber; external name 'limitingConeAngle';
  public
    property x: TJSSVGAnimatedNumber read Fx;
    property y: TJSSVGAnimatedNumber read Fy;
    property z: TJSSVGAnimatedNumber read Fz;
    property pointsAtX: TJSSVGAnimatedNumber read FpointsAtX;
    property pointsAtY: TJSSVGAnimatedNumber read FpointsAtY;
    property pointsAtZ: TJSSVGAnimatedNumber read FpointsAtZ;
    property specularExponent: TJSSVGAnimatedNumber read FspecularExponent;
    property limitingConeAngle: TJSSVGAnimatedNumber read FlimitingConeAngle;
  end;

  { --------------------------------------------------------------------
    TJSSVGFETileElement
    --------------------------------------------------------------------}
  TJSSVGFETileElement = class external name 'SVGFETileElement' (TJSSVGElement)
  private
    Fin1: TJSSVGAnimatedString; external name 'in1';
  public
    property in1: TJSSVGAnimatedString read Fin1;
  end;

  { --------------------------------------------------------------------
    TJSSVGFETurbulenceElement
    --------------------------------------------------------------------}
  TJSSVGFETurbulenceElement = class external name
    'SVGFETurbulenceElement' (TJSSVGElement)
    private
    FbaseFrequencyX: TJSSVGAnimatedNumber; external name 'baseFrequencyX';
    FbaseFrequencyY: TJSSVGAnimatedNumber; external name 'baseFrequencyY';
    FnumOctaves: TJSSVGAnimatedInteger; external name 'numOctaves';
    Fseed: TJSSVGAnimatedNumber; external name 'seed';
    FstitchTiles: TJSSVGAnimatedEnumeration; external name 'stitchTiles';
    Ftype_: TJSSVGAnimatedEnumeration; external name 'type';
  public
  const
    SVG_TURBULENCE_TYPE_UNKNOWN = 0;
    SVG_TURBULENCE_TYPE_FRACTALNOISE = 1;
    SVG_TURBULENCE_TYPE_TURBULENCE = 2;
    SVG_STITCHTYPE_UNKNOWN = 0;
    SVG_STITCHTYPE_STITCH = 1;
    SVG_STITCHTYPE_NOSTITCH = 2;
  Public
    property baseFrequencyX: TJSSVGAnimatedNumber read FbaseFrequencyX;
    property baseFrequencyY: TJSSVGAnimatedNumber read FbaseFrequencyY;
    property numOctaves: TJSSVGAnimatedInteger read FnumOctaves;
    property seed: TJSSVGAnimatedNumber read Fseed;
    property stitchTiles: TJSSVGAnimatedEnumeration read FstitchTiles;
    property type_: TJSSVGAnimatedEnumeration read Ftype_;
  end;

  { --------------------------------------------------------------------
    TJSSVGFilterElement
    --------------------------------------------------------------------}
  TJSSVGFilterElement = class external name 'SVGFilterElement' (TJSSVGElement)
  private
    FfilterUnits: TJSSVGAnimatedEnumeration; external name 'filterUnits';
    FprimitiveUnits: TJSSVGAnimatedEnumeration; external name 'primitiveUnits';
    Fx: TJSSVGAnimatedLength; external name 'x';
    Fy: TJSSVGAnimatedLength; external name 'y';
    Fwidth: TJSSVGAnimatedLength; external name 'width';
    Fheight: TJSSVGAnimatedLength; external name 'height';
  public
    property filterUnits: TJSSVGAnimatedEnumeration read FfilterUnits;
    property primitiveUnits: TJSSVGAnimatedEnumeration read FprimitiveUnits;
    property x: TJSSVGAnimatedLength read Fx;
    property y: TJSSVGAnimatedLength read Fy;
    property width: TJSSVGAnimatedLength read Fwidth;
    property height: TJSSVGAnimatedLength read Fheight;
  end;

  { --------------------------------------------------------------------
    TJSSVGFilterPrimitiveStandardAttributes
    --------------------------------------------------------------------}
  TJSSVGFilterPrimitiveStandardAttributes = class external name
    'SVGFilterPrimitiveStandardAttributes'
    private
    Fx: TJSSVGAnimatedLength; external name 'x';
    Fy: TJSSVGAnimatedLength; external name 'y';
    Fwidth: TJSSVGAnimatedLength; external name 'width';
    Fheight: TJSSVGAnimatedLength; external name 'height';
    Fresult: TJSSVGAnimatedString; external name 'result';
  public
    property x: TJSSVGAnimatedLength read Fx;
    property y: TJSSVGAnimatedLength read Fy;
    property width: TJSSVGAnimatedLength read Fwidth;
    property height: TJSSVGAnimatedLength read Fheight;
    property result: TJSSVGAnimatedString read Fresult;
  end;

  { --------------------------------------------------------------------
    TJSSVGFitToViewBox
    --------------------------------------------------------------------}
  TJSSVGFitToViewBox = class external name 'SVGFitToViewBox'
  private
    FviewBox: TJSSVGAnimatedRect; external name 'viewBox';
    FpreserveAspectRatio: TJSSVGAnimatedPreserveAspectRatio; external name
      'preserveAspectRatio';
  public
    property viewBox: TJSSVGAnimatedRect read FviewBox;
    property preserveAspectRatio: TJSSVGAnimatedPreserveAspectRatio read
      FpreserveAspectRatio;
  end;

  { --------------------------------------------------------------------
    TJSSVGForeignObjectElement
    --------------------------------------------------------------------}
  TJSSVGForeignObjectElement = class external name
    'SVGForeignObjectElement' (TJSSVGGraphicsElement)
    private
    Fx: TJSSVGAnimatedLength; external name 'x';
    Fy: TJSSVGAnimatedLength; external name 'y';
    Fwidth: TJSSVGAnimatedLength; external name 'width';
    Fheight: TJSSVGAnimatedLength; external name 'height';
  public
    property x: TJSSVGAnimatedLength read Fx;
    property y: TJSSVGAnimatedLength read Fy;
    property width: TJSSVGAnimatedLength read Fwidth;
    property height: TJSSVGAnimatedLength read Fheight;
  end;

  { --------------------------------------------------------------------
    TJSSVGGElement
    --------------------------------------------------------------------}
  TJSSVGGElement = class external name 'SVGGElement' (TJSSVGGraphicsElement)
  private
  public
  end;

  { --------------------------------------------------------------------
    TJSSVGGradientElement
    --------------------------------------------------------------------}
  TJSSVGGradientElement = class external name 'SVGGradientElement' (TJSSVGElement)
  private
    FgradientUnits: TJSSVGAnimatedEnumeration; external name 'gradientUnits';
    FgradientTransform: TJSSVGAnimatedTransformList; external name
      'gradientTransform';
    FspreadMethod: TJSSVGAnimatedEnumeration; external name 'spreadMethod';
  public
  const
    SVG_SPREADMETHOD_UNKNOWN = 0;
    SVG_SPREADMETHOD_PAD = 1;
    SVG_SPREADMETHOD_REFLECT = 2;
    SVG_SPREADMETHOD_REPEAT = 3;
  Public
    property gradientUnits: TJSSVGAnimatedEnumeration read FgradientUnits;
    property gradientTransform: TJSSVGAnimatedTransformList read FgradientTransform;
    property spreadMethod: TJSSVGAnimatedEnumeration read FspreadMethod;
  end;

  { --------------------------------------------------------------------
    TJSSVGImageElement
    --------------------------------------------------------------------}
  TJSSVGImageElement = class external name 'SVGImageElement' (TJSSVGGraphicsElement)
  private
    Fx: TJSSVGAnimatedLength; external name 'x';
    Fy: TJSSVGAnimatedLength; external name 'y';
    Fwidth: TJSSVGAnimatedLength; external name 'width';
    Fheight: TJSSVGAnimatedLength; external name 'height';
    FpreserveAspectRatio: TJSSVGAnimatedPreserveAspectRatio; external name
      'preserveAspectRatio';
  public
    property x: TJSSVGAnimatedLength read Fx;
    property y: TJSSVGAnimatedLength read Fy;
    property width: TJSSVGAnimatedLength read Fwidth;
    property height: TJSSVGAnimatedLength read Fheight;
    property preserveAspectRatio: TJSSVGAnimatedPreserveAspectRatio read
      FpreserveAspectRatio;
  end;

  { --------------------------------------------------------------------
    TJSSVGLength
    --------------------------------------------------------------------}
  TJSSVGLength = class external name 'SVGLength'
  private
    FunitType: Cardinal; external name 'unitType';
  public
  const
    SVG_LENGTHTYPE_UNKNOWN = 0;
    SVG_LENGTHTYPE_NUMBER = 1;
    SVG_LENGTHTYPE_PERCENTAGE = 2;
    SVG_LENGTHTYPE_EMS = 3;
    SVG_LENGTHTYPE_EXS = 4;
    SVG_LENGTHTYPE_PX = 5;
    SVG_LENGTHTYPE_CM = 6;
    SVG_LENGTHTYPE_MM = 7;
    SVG_LENGTHTYPE_IN = 8;
    SVG_LENGTHTYPE_PT = 9;
    SVG_LENGTHTYPE_PC = 10;
  Public
    value: Double;
    valueInSpecifiedUnits: Double;
    valueAsString: string;
    procedure newValueSpecifiedUnits(unitType: Cardinal; valueInSpecifiedUnits: Double);
    procedure convertToSpecifiedUnits(unitType: Cardinal);
    property unitType: Cardinal read FunitType;
  end;

  { --------------------------------------------------------------------
    TJSSVGLinearGradientElement
    --------------------------------------------------------------------}
  TJSSVGLinearGradientElement = class external name
    'SVGLinearGradientElement' (TJSSVGGradientElement)
    private
    Fx1: TJSSVGAnimatedLength; external name 'x1';
    Fy1: TJSSVGAnimatedLength; external name 'y1';
    Fx2: TJSSVGAnimatedLength; external name 'x2';
    Fy2: TJSSVGAnimatedLength; external name 'y2';
  public
    property x1: TJSSVGAnimatedLength read Fx1;
    property y1: TJSSVGAnimatedLength read Fy1;
    property x2: TJSSVGAnimatedLength read Fx2;
    property y2: TJSSVGAnimatedLength read Fy2;
  end;

  { --------------------------------------------------------------------
    TJSSVGLineElement
    --------------------------------------------------------------------}
  TJSSVGLineElement = class external name 'SVGLineElement' (TJSSVGGeometryElement)
  private
    Fx1: TJSSVGAnimatedLength; external name 'x1';
    Fy1: TJSSVGAnimatedLength; external name 'y1';
    Fx2: TJSSVGAnimatedLength; external name 'x2';
    Fy2: TJSSVGAnimatedLength; external name 'y2';
  public
    property x1: TJSSVGAnimatedLength read Fx1;
    property y1: TJSSVGAnimatedLength read Fy1;
    property x2: TJSSVGAnimatedLength read Fx2;
    property y2: TJSSVGAnimatedLength read Fy2;
  end;

  { --------------------------------------------------------------------
    TJSSVGMarkerElement
    --------------------------------------------------------------------}
  TJSSVGMarkerElement = class external name 'SVGMarkerElement' (TJSSVGElement)
  private
    FrefX: TJSSVGAnimatedLength; external name 'refX';
    FrefY: TJSSVGAnimatedLength; external name 'refY';
    FmarkerUnits: TJSSVGAnimatedEnumeration; external name 'markerUnits';
    FmarkerWidth: TJSSVGAnimatedLength; external name 'markerWidth';
    FmarkerHeight: TJSSVGAnimatedLength; external name 'markerHeight';
    ForientType: TJSSVGAnimatedEnumeration; external name 'orientType';
    ForientAngle: TJSSVGAnimatedAngle; external name 'orientAngle';
  public
  const
    SVG_MARKERUNITS_UNKNOWN = 0;
    SVG_MARKERUNITS_USERSPACEONUSE = 1;
    SVG_MARKERUNITS_STROKEWIDTH = 2;
    SVG_MARKER_ORIENT_UNKNOWN = 0;
    SVG_MARKER_ORIENT_AUTO = 1;
    SVG_MARKER_ORIENT_ANGLE = 2;
  Public
    procedure setOrientToAuto;
    procedure setOrientToAngle(angle: TJSSVGAngle);
    property refX: TJSSVGAnimatedLength read FrefX;
    property refY: TJSSVGAnimatedLength read FrefY;
    property markerUnits: TJSSVGAnimatedEnumeration read FmarkerUnits;
    property markerWidth: TJSSVGAnimatedLength read FmarkerWidth;
    property markerHeight: TJSSVGAnimatedLength read FmarkerHeight;
    property orientType: TJSSVGAnimatedEnumeration read ForientType;
    property orientAngle: TJSSVGAnimatedAngle read ForientAngle;
  end;

  { --------------------------------------------------------------------
    TJSSVGMaskElement
    --------------------------------------------------------------------}
  TJSSVGMaskElement = class external name 'SVGMaskElement' (TJSSVGElement)
  private
    FmaskUnits: TJSSVGAnimatedEnumeration; external name 'maskUnits';
    FmaskContentUnits: TJSSVGAnimatedEnumeration; external name 'maskContentUnits';
    Fx: TJSSVGAnimatedLength; external name 'x';
    Fy: TJSSVGAnimatedLength; external name 'y';
    Fwidth: TJSSVGAnimatedLength; external name 'width';
    Fheight: TJSSVGAnimatedLength; external name 'height';
  public
    property maskUnits: TJSSVGAnimatedEnumeration read FmaskUnits;
    property maskContentUnits: TJSSVGAnimatedEnumeration read FmaskContentUnits;
    property x: TJSSVGAnimatedLength read Fx;
    property y: TJSSVGAnimatedLength read Fy;
    property width: TJSSVGAnimatedLength read Fwidth;
    property height: TJSSVGAnimatedLength read Fheight;
  end;

  { --------------------------------------------------------------------
    TJSSVGMatrix
    --------------------------------------------------------------------}
  TJSSVGMatrix = class external name 'SVGMatrix'
  private
  public
    a: double;
    b: double;
    c: double;
    d: double;
    e: double;
    f: double;
    function multiply(secondMatrix: TJSSVGMatrix): TJSSVGMatrix;
    function inverse: TJSSVGMatrix;
    function translate(x: Double; y: Double): TJSSVGMatrix;
    function scale(scaleFactor: Double): TJSSVGMatrix;
    function scaleNonUniform(scaleFactorX: Double; scaleFactorY: Double):
      TJSSVGMatrix;
    function rotate(angle: Double): TJSSVGMatrix;
    function rotateFromVector(x: Double; y: Double): TJSSVGMatrix;
    function flipX: TJSSVGMatrix;
    function flipY: TJSSVGMatrix;
    function skewX(angle: Double): TJSSVGMatrix;
    function skewY(angle: Double): TJSSVGMatrix;
  end;

  { --------------------------------------------------------------------
    TJSSVGMetadataElement
    --------------------------------------------------------------------}
  TJSSVGMetadataElement = class external name 'SVGMetadataElement' (TJSSVGElement)
  private
  public
  end;

  { --------------------------------------------------------------------
    TJSSVGMPathElement
    --------------------------------------------------------------------}
  TJSSVGMPathElement = class external name 'SVGMPathElement' (TJSSVGElement)
  private
  public
  end;

  { --------------------------------------------------------------------
    TJSSVGNumber
    --------------------------------------------------------------------}
  TJSSVGNumber = class external name 'SVGNumber'
  private
  public
    value: Double;
  end;

  { --------------------------------------------------------------------
    TJSSVGPathElement
    --------------------------------------------------------------------}
  TJSSVGPathElement = class external name 'SVGPathElement' (TJSSVGGeometryElement)
  private
  public
    function getPathSegAtLength(distance: Double): NativeInt;
  end;

  { --------------------------------------------------------------------
    TJSSVGPatternElement
    --------------------------------------------------------------------}
  TJSSVGPatternElement = class external name 'SVGPatternElement' (TJSSVGElement)
  private
    FpatternUnits: TJSSVGAnimatedEnumeration; external name 'patternUnits';
    FpatternContentUnits: TJSSVGAnimatedEnumeration; external name
      'patternContentUnits';
    FpatternTransform: TJSSVGAnimatedTransformList; external name
      'patternTransform';
    Fx: TJSSVGAnimatedLength; external name 'x';
    Fy: TJSSVGAnimatedLength; external name 'y';
    Fwidth: TJSSVGAnimatedLength; external name 'width';
    Fheight: TJSSVGAnimatedLength; external name 'height';
  public
    property patternUnits: TJSSVGAnimatedEnumeration read FpatternUnits;
    property patternContentUnits: TJSSVGAnimatedEnumeration read
      FpatternContentUnits;
    property patternTransform: TJSSVGAnimatedTransformList read FpatternTransform;
    property x: TJSSVGAnimatedLength read Fx;
    property y: TJSSVGAnimatedLength read Fy;
    property width: TJSSVGAnimatedLength read Fwidth;
    property height: TJSSVGAnimatedLength read Fheight;
  end;

  { --------------------------------------------------------------------
    TJSSVGPoint
    --------------------------------------------------------------------}
  TJSSVGPoint = class external name 'SVGPoint'
  private
  public
    x: Double;
    y: Double;
    function matrixTransform(matrix: TJSSVGMatrix): TJSSVGPoint;
  end;

  { --------------------------------------------------------------------
    TJSSVGPointList
    --------------------------------------------------------------------}
  TJSSVGPointList = class external name 'SVGPointList'
  public
    numberOfItems: Integer;
    procedure clear;
    function initialize(newItem: TJSSVGPoint): TJSSVGPoint;
    function getItem(&index: Integer): TJSSVGPoint;
    function insertItemBefore(newItem: TJSSVGPoint; &index: Integer): TJSSVGPoint;
    function replaceItem(newItem: TJSSVGPoint; &index: Integer): TJSSVGPoint;
    function removeItem(&index: Integer): TJSSVGPoint;
    function appendItem(newItem: TJSSVGPoint): TJSSVGPoint;
  end;

  { --------------------------------------------------------------------
    TJSSVGPolygonElement
    --------------------------------------------------------------------}
  TJSSVGPolygonElement = class external name
    'SVGPolygonElement' (TJSSVGGeometryElement)
    private
    Fpoints: TJSSVGPointList; external name 'points';
    FanimatedPoints: TJSSVGPointList; external name 'animatedPoints';
  public
    property points: TJSSVGPointList read Fpoints;
    property animatedPoints: TJSSVGPointList read FanimatedPoints;
  end;

  { --------------------------------------------------------------------
    TJSSVGPolylineElement
    --------------------------------------------------------------------}
  TJSSVGPolylineElement = class external name
    'SVGPolylineElement' (TJSSVGGeometryElement)
    private
    Fpoints: TJSSVGPointList; external name 'points';
    FanimatedPoints: TJSSVGPointList; external name 'animatedPoints';
  public
    property points: TJSSVGPointList read Fpoints;
    property animatedPoints: TJSSVGPointList read FanimatedPoints;
  end;

  { --------------------------------------------------------------------
    TJSSVGPreserveAspectRatio
    --------------------------------------------------------------------}
  TJSSVGPreserveAspectRatio = class external name 'SVGPreserveAspectRatio'
  private
  public
  const
    SVG_PRESERVEASPECTRATIO_UNKNOWN = 0;
    SVG_PRESERVEASPECTRATIO_NONE = 1;
    SVG_PRESERVEASPECTRATIO_XMINYMIN = 2;
    SVG_PRESERVEASPECTRATIO_XMIDYMIN = 3;
    SVG_PRESERVEASPECTRATIO_XMAXYMIN = 4;
    SVG_PRESERVEASPECTRATIO_XMINYMID = 5;
    SVG_PRESERVEASPECTRATIO_XMIDYMID = 6;
    SVG_PRESERVEASPECTRATIO_XMAXYMID = 7;
    SVG_PRESERVEASPECTRATIO_XMINYMAX = 8;
    SVG_PRESERVEASPECTRATIO_XMIDYMAX = 9;
    SVG_PRESERVEASPECTRATIO_XMAXYMAX = 10;
    SVG_MEETORSLICE_UNKNOWN = 0;
    SVG_MEETORSLICE_MEET = 1;
    SVG_MEETORSLICE_SLICE = 2;
  Public
    align: Cardinal;
    meetOrSlice: Cardinal;
  end;

  { --------------------------------------------------------------------
    TJSSVGRadialGradientElement
    --------------------------------------------------------------------}
  TJSSVGRadialGradientElement = class external name
    'SVGRadialGradientElement' (TJSSVGGradientElement)
    private
    Fcx: TJSSVGAnimatedLength; external name 'cx';
    Fcy: TJSSVGAnimatedLength; external name 'cy';
    rFr: TJSSVGAnimatedLength; external name 'r';
    Ffx: TJSSVGAnimatedLength; external name 'fx';
    Ffy: TJSSVGAnimatedLength; external name 'fy';
    Ffr: TJSSVGAnimatedLength; external name 'fr';
  public
    property cx: TJSSVGAnimatedLength read Fcx;
    property cy: TJSSVGAnimatedLength read Fcy;
    property r: TJSSVGAnimatedLength read rFr;
    property fx: TJSSVGAnimatedLength read Ffx;
    property fy: TJSSVGAnimatedLength read Ffy;
    property fr: TJSSVGAnimatedLength read Ffr;
  end;

  { --------------------------------------------------------------------
    TJSSVGRect
    --------------------------------------------------------------------}
  TJSSVGRect = class external name 'SVGRect'
  private
  public
    x: Double;
    y: Double;
    width: Double;
    height: Double;
  end;

  { --------------------------------------------------------------------
    TJSSVGRectElement
    --------------------------------------------------------------------}
  TJSSVGRectElement = class external name 'SVGRectElement' (TJSSVGGeometryElement)
  private
    Fx: TJSSVGAnimatedLength; external name 'x';
    Fy: TJSSVGAnimatedLength; external name 'y';
    Fwidth: TJSSVGAnimatedLength; external name 'width';
    Fheight: TJSSVGAnimatedLength; external name 'height';
    Frx: TJSSVGAnimatedLength; external name 'rx';
    Fry: TJSSVGAnimatedLength; external name 'ry';
  public
    property x: TJSSVGAnimatedLength read Fx;
    property y: TJSSVGAnimatedLength read Fy;
    property width: TJSSVGAnimatedLength read Fwidth;
    property height: TJSSVGAnimatedLength read Fheight;
    property rx: TJSSVGAnimatedLength read Frx;
    property ry: TJSSVGAnimatedLength read Fry;
  end;

  { --------------------------------------------------------------------
    TJSSVGScriptElement
    --------------------------------------------------------------------}
  TJSSVGScriptElement = class external name 'SVGScriptElement' (TJSSVGElement)
  private
  public
    type_: string; external name 'type';
  end;

  { --------------------------------------------------------------------
    TJSSVGSetElement
    --------------------------------------------------------------------}
  TJSSVGSetElement = class external name 'SVGSetElement' (TJSSVGAnimationElement)
  private
  public
  end;

  { --------------------------------------------------------------------
    TJSSVGStopElement
    --------------------------------------------------------------------}
  TJSSVGStopElement = class external name 'SVGStopElement' (TJSSVGElement)
  private
    Foffset: TJSSVGAnimatedNumber; external name 'offset';
  public
    property offset: TJSSVGAnimatedNumber read Foffset;
  end;

  { --------------------------------------------------------------------
    TJSSVGStyleElement
    --------------------------------------------------------------------}
  TJSSVGStyleElement = class external name 'SVGStyleElement' (TJSSVGElement)
  private
    Fsheet: TJSStyleSheet; external name 'sheet';
  public
    type_: string; external name 'type';
    media: string;
    title: string;
    disabled: boolean;
    property sheet: TJSStyleSheet read Fsheet;
  end;

  { --------------------------------------------------------------------
    TJSSVGSwitchElement
    --------------------------------------------------------------------}
  TJSSVGSwitchElement = class external name
    'SVGSwitchElement' (TJSSVGGraphicsElement)
    private
  public
  end;

  { --------------------------------------------------------------------
    TJSSVGSymbolElement
    --------------------------------------------------------------------}
  TJSSVGSymbolElement = class external name 'SVGSymbolElement' (TJSSVGElement)
  private
  public
  end;

  { --------------------------------------------------------------------
    TJSSVGStringList
    --------------------------------------------------------------------}
  TJSSVGStringList = class external name 'SVGStringList'
  public
    numberOfItems: Integer;
    procedure clear;
    function initialize(newItem: String): String;
    function getItem(&index: Integer): String;
    function insertItemBefore(newItem: String; &index: Integer): String;
    function replaceItem(newItem: String; &index: Integer): String;
    function removeItem(&index: Integer): String;
    function appendItem(newItem: String): String;
  end;

  { --------------------------------------------------------------------
    TJSSVGTests
    --------------------------------------------------------------------}
  TJSSVGTests = class external name 'SVGTests'
  private
    FrequiredExtensions: TJSSVGStringList; external name 'requiredExtensions';
    FsystemLanguage: TJSSVGStringList; external name 'systemLanguage';
  public
    property requiredExtensions: TJSSVGStringList read FrequiredExtensions;
    property systemLanguage: TJSSVGStringList read FsystemLanguage;
  end;

  { --------------------------------------------------------------------
    TJSSVGTextContentElement
    --------------------------------------------------------------------}
  TJSSVGTextContentElement = class external name
    'SVGTextContentElement' (TJSSVGGraphicsElement)
    private
    FtextLength: TJSSVGAnimatedLength; external name 'textLength';
    FlengthAdjust: TJSSVGAnimatedEnumeration; external name 'lengthAdjust';
  public
  const
    LENGTHADJUST_UNKNOWN = 0;
    LENGTHADJUST_SPACING = 1;
    LENGTHADJUST_SPACINGANDGLYPHS = 2;
  Public
    function getNumberOfChars: Integer;
    function getComputedTextLength: Double;
    function getSubStringLength(charnum: NativeInt; nchars: NativeInt): Double;
    function getStartPositionOfChar(charnum: NativeInt): TJSSVGPoint;
    function getEndPositionOfChar(charnum: NativeInt): TJSSVGPoint;
    function getExtentOfChar(charnum: NativeInt): TJSSVGRect;
    function getRotationOfChar(charnum: NativeInt): Double;
    function getCharNumAtPosition(point: TJSSVGPoint): Integer;
    procedure selectSubString(charnum: NativeInt; nchars: NativeInt);
    property textLength: TJSSVGAnimatedLength read FtextLength;
    property lengthAdjust: TJSSVGAnimatedEnumeration read FlengthAdjust;
  end;

  { --------------------------------------------------------------------
    TJSSVGTextPositioningElement
    --------------------------------------------------------------------}
  TJSSVGTextPositioningElement = class external name
    'SVGTextPositioningElement' (TJSSVGTextContentElement)
    private
    Fx: TJSSVGAnimatedLengthList; external name 'x';
    Fy: TJSSVGAnimatedLengthList; external name 'y';
    Fdx: TJSSVGAnimatedLengthList; external name 'dx';
    Fdy: TJSSVGAnimatedLengthList; external name 'dy';
    Frotate: TJSSVGAnimatedNumberList; external name 'rotate';
  public
    property x: TJSSVGAnimatedLengthList read Fx;
    property y: TJSSVGAnimatedLengthList read Fy;
    property dx: TJSSVGAnimatedLengthList read Fdx;
    property dy: TJSSVGAnimatedLengthList read Fdy;
    property rotate: TJSSVGAnimatedNumberList read Frotate;
  end;

  { --------------------------------------------------------------------
    TJSSVGTextElement
    --------------------------------------------------------------------}
  TJSSVGTextElement = class external name 'SVGTextElement' (TJSSVGTextPositioningElement)
    private
  public
  end;

  { --------------------------------------------------------------------
    TJSSVGTextPathElement
    --------------------------------------------------------------------}
  TJSSVGTextPathElement = class external name
    'SVGTextPathElement' (TJSSVGTextContentElement)
    private
    FstartOffset: TJSSVGAnimatedLength; external name 'startOffset';
    Fmethod: TJSSVGAnimatedEnumeration; external name 'method';
    Fspacing: TJSSVGAnimatedEnumeration; external name 'spacing';
  public
  const
    TEXTPATH_METHODTYPE_UNKNOWN = 0;
    TEXTPATH_METHODTYPE_ALIGN = 1;
    TEXTPATH_METHODTYPE_STRETCH = 2;
    TEXTPATH_SPACINGTYPE_UNKNOWN = 0;
    TEXTPATH_SPACINGTYPE_AUTO = 1;
    TEXTPATH_SPACINGTYPE_EXACT = 2;
  Public
    property startOffset: TJSSVGAnimatedLength read FstartOffset;
    property method: TJSSVGAnimatedEnumeration read Fmethod;
    property spacing: TJSSVGAnimatedEnumeration read Fspacing;
  end;

  { --------------------------------------------------------------------
    TJSSVGTitleElement
    --------------------------------------------------------------------}
  TJSSVGTitleElement = class external name 'SVGTitleElement' (TJSSVGElement)
  private
  public
  end;

  { --------------------------------------------------------------------
    TJSSVGTransform
    --------------------------------------------------------------------}
  TJSSVGTransform = class external name 'SVGTransform'
  private
    Ftype_: Cardinal; external name 'type';
    Fmatrix: TJSSVGMatrix; external name 'matrix';
    Fangle: Double; external name 'angle';
  public
  const
    SVG_TRANSFORM_UNKNOWN = 0;
    SVG_TRANSFORM_MATRIX = 1;
    SVG_TRANSFORM_TRANSLATE = 2;
    SVG_TRANSFORM_SCALE = 3;
    SVG_TRANSFORM_ROTATE = 4;
    SVG_TRANSFORM_SKEWX = 5;
    SVG_TRANSFORM_SKEWY = 6;
  Public
    procedure setMatrix(matrix: TJSSVGMatrix);
    procedure setTranslate(tx: Double; ty: Double);
    procedure setScale(sx: Double; sy: Double);
    procedure setRotate(angle: Double; cx: Double; cy: Double);
    procedure setSkewX(angle: Double);
    procedure setSkewY(angle: Double);
    property type_: Cardinal read Ftype_;
    property matrix: TJSSVGMatrix read Fmatrix;
    property angle: Double read Fangle;
  end;

  { --------------------------------------------------------------------
    TJSSVGTSpanElement
    --------------------------------------------------------------------}
  TJSSVGTSpanElement = class external name
    'SVGTSpanElement' (TJSSVGTextPositioningElement)
    private
  public
  end;

  { --------------------------------------------------------------------
    TJSSVGUnitTypes
    --------------------------------------------------------------------}
  TJSSVGUnitTypes = class external name 'SVGUnitTypes'
  private
  public
    const
      SVG_UNIT_TYPE_UNKNOWN = 0;
      SVG_UNIT_TYPE_USERSPACEONUSE = 1;
      SVG_UNIT_TYPE_OBJECTBOUNDINGBOX = 2;
  Public
  end;

  { --------------------------------------------------------------------
    TJSSVGURIReference
    --------------------------------------------------------------------}
  TJSSVGURIReference = class external name 'SVGURIReference'
  private
    Fhref: TJSSVGAnimatedString; external name 'href';
  public
    property href: TJSSVGAnimatedString read Fhref;
  end;

  { --------------------------------------------------------------------
    TJSSVGUseElement
    --------------------------------------------------------------------}
  TJSSVGUseElement = class external name 'SVGUseElement' (TJSSVGGraphicsElement)
  private
    Fx: TJSSVGAnimatedLength; external name 'x';
    Fy: TJSSVGAnimatedLength; external name 'y';
    Fwidth: TJSSVGAnimatedLength; external name 'width';
    Fheight: TJSSVGAnimatedLength; external name 'height';
  public
    property x: TJSSVGAnimatedLength read Fx;
    property y: TJSSVGAnimatedLength read Fy;
    property width: TJSSVGAnimatedLength read Fwidth;
    property height: TJSSVGAnimatedLength read Fheight;
  end;

  { --------------------------------------------------------------------
    TJSSVGViewElement
    --------------------------------------------------------------------}
  TJSSVGViewElement = class external name 'SVGViewElement' (TJSSVGElement)
  private
  public
  end;

  { --------------------------------------------------------------------
    TJSSVGZoomAndPan
    --------------------------------------------------------------------}
  TJSSVGZoomAndPan = class external name 'SVGZoomAndPan'
  private
  public
  const
    SVG_ZOOMANDPAN_UNKNOWN = 0;
    SVG_ZOOMANDPAN_DISABLE = 1;
    SVG_ZOOMANDPAN_MAGNIFY = 2;
  Public
    zoomAndPan: Cardinal;
  end;

implementation

end.