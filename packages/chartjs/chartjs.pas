{
    This file is part of the Pas2JS run time library.
    Copyright (C) 2019 Silvio Clecio (silvioprog)

    Pascal mapping for ChartJS: https://www.chartjs.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit ChartJS;

{$MODE OBJFPC}
{$MODESWITCH EXTERNALCLASS}

interface

uses
  JS,
  Web;

type
  TChart = class;
  TChartController = class;
  TChartOptions = class;
  TChartAnimationCallback = class;
  TChartMoment = class;

  { Chart Moment display formats class. }
  TChartMomentDisplayFormats = class external name 'Object' (TJSObject)
  public
    millisecond: string;
    second: string;
    minute: string;
    hour: string;
    day: string;
    week: string;
    month: string;
    quarter: string;
    year: string;
  end;

  { Chart Moment time type. }
  TChartMomentTime = JSValue;

  { Chart Moment parser. }
  TChartMomentParser = reference to function(const data: JSValue): TChartMoment;

  { Chart Moment class. }
  TChartMoment = class external name 'Object' (TJSObject)
  public
    { Sets how different time units are displayed. }
    displayFormats: TChartMomentDisplayFormats;
    { If true and the unit is set to 'week', then the first day of the week will
      be Monday. Otherwise, it will be Sunday. }
    isoWeekday: Boolean;
    { If defined, this will override the data maximum. }
    max: TChartMomentTime;
    { If defined, this will override the data minimum. }
    min: TChartMomentTime;
    { Custom parser for dates. }
    parser: string;
    parser_: TChartMomentParser; external name 'parser';
    { If defined, dates will be rounded to the start of this unit. See Time
      Units below for the allowed units. }
    round: string;
    { The moment js format string to use for the tooltip. }
    tooltipFormat: string;
    { If defined, will force the unit to be a certain type. See Time Units
      section below for details. }
    unit_: string; external name 'unit';
    { The number of units between grid lines. }
    stepSize: NativeUInt;
    { The minimum display format to be used for a time unit. }
    minUnit: string;
  end;

  { Chart size. }
  TChartSize = class external name 'Object' (TJSObject)
  public
    { Width of the chart. }
    width: NativeUInt;
    { Height of the chart. }
    height: NativeUInt;
  end;

  { Dataset class. }
  TChartDataset = class external name 'Object' (TJSObject)
  public
    { Chart type. }
    type_: string; external name 'type';
    { The label for the dataset which appears in the legend and tooltips. }
    label_: string; external name 'label';
    { If true, it represents a hidden dataset. }
    hidden: Boolean;
  end;

  { Called when a resize occurs. }
  TChartResizeEventHandler = reference to procedure(const chart: TChart;
    const newSize: TChartSize);

  { Hover class. }
  TChartHover = class external name 'Object' (TJSObject)
  public
    { Called when any of the events fire. Called in the context of the chart and
      passed the event and an array of active elements (bars, points, etc). }
    onHover: TJSMouseEventHandler;
    { Sets which elements appear in the tooltip. }
    mode: string;
    { If true, the hover mode only applies when the mouse position intersects an
      item on the chart. }
    intersect: Boolean;
    { Can be set to 'x', 'y', or 'xy' to define which directions are used in
      calculating distances. Defaults to 'x' for index mode and 'xy' in dataset
      and nearest modes. }
    axis: string;
    { Duration in milliseconds it takes to animate hover style changes. }
    animationDuration: NativeUInt;
  end;

  { Animation render event. }
  TChartAnimationRenderEventHandler = reference to procedure(
    const chart: TChart; const animation: TChartAnimationCallback);

  { Animation event. }
  TChartAnimationEventHandler = reference to procedure(
    const animation: TChartAnimationCallback);

  { Animation callbacks class. }
  TChartAnimationCallback = class external name 'Object' (TJSObject)
  public
    { Chart object. }
    chart: TChart;
    { Current Animation frame number. }
    currentStep: NativeUInt;
    { Number of animation frames. }
    numSteps: NativeUInt;
    { Animation easing to use. }
    easing: string;
    { Function that renders the chart. }
    render: TChartAnimationRenderEventHandler;
    { User callback. }
    onAnimationProgress: TChartAnimationEventHandler;
    { User callback. }
    onAnimationComplete: TChartAnimationEventHandler;
  end;

  { Animation configuration class. }
  TChartAnimationConfiguration = class external name 'Object' (TJSObject)
  public
    { The number of milliseconds an animation takes. }
    duration: NativeUInt;
    { Easing function to use. }
    easing: string;
    { Callback called on each step of an animation. }
    onProgress: TChartAnimationEventHandler;
    { Callback called at the end of an animation. }
    onComplete: TChartAnimationEventHandler;
    { If true, the chart will animate in with a rotation animation. }
    animateRotate: Boolean;
    { If true, will animate scaling the chart from the center outwards. }
    animateScale: Boolean;
  end;

  { Padding to add inside the chart. }
  TChartPadding = class external name 'Object' (TJSObject)
  public
    top: NativeInt;
    right: NativeInt;
    bottom: NativeInt;
    left:NativeInt;
  end;

  { Layout configuration class. }
  TChartLayoutConfiguration = class external name 'Object' (TJSObject)
  public
    { The padding to add inside the chart. }
    padding: NativeInt;
    { The padding to add inside the chart. }
    padding_: TChartPadding; external name 'padding';
  end;

  { Legend item class. }
  TChartLegendItem = class external name 'Object' (TJSObject)
  public
    { Label that will be displayed. }
    text: string;
    { Fill style of the legend box. }
    fillStyle: string;
    { If true, this item represents a hidden dataset. Label will be rendered
      with a strike-through effect. }
    hidden: Boolean;
    { For box border. See
      https://developer.mozilla.org/en/docs/Web/API/CanvasRenderingContext2D/lineCap. }
    lineCap: string;
    { For box border. See
      https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/setLineDash. }
    lineDash: array of NativeUInt;
    lineDash_: TJSArray; external name 'lineDash';
    { For box border. See
      https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/lineDashOffset. }
    lineDashOffset: NativeUInt;
    { For box border. See
      https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D/lineJoin. }
    lineJoin: string;
    { Width of box border. }
    lineWidth: NativeUInt;
    { Stroke style of the legend box. }
    strokeStyle: string;
    { Point style of the legend box (only used if usePointStyle is true). }
    pointStyle: string;
  end;

  { Legend click event. }
  TChartLegendClickEventHandler = reference to procedure(
    const ev: TJSMouseEvent; const legendItem: TChartLegendItem);

  { Legend event handler. }
  TChartLegendEventHandler = reference to function(
    const legendItem: TChartLegendItem): TChartLegendItem;

  { Legend label class. }
  TChartLegendLabel = class external name 'Object' (TJSObject)
  public
    { Width of coloured box. }
    boxWidth: NativeUInt;
    { Font size of text. }
    fontSize: NativeUInt;
    { Font style of text. }
    fontStyle: string;
    { Color of text. }
    fontColor: string;
    { Font family of legend text. }
    fontFamily: string;
    { Padding between rows of colored boxes. }
    padding: NativeInt;
    { Generates legend items for each thing in the legend. Default
      implementation returns the text + styling for the color box. }
    generateLabels: TChartLegendEventHandler;
    { Filters legend items out of the legend. Receives 2 parameters,
      a Legend Item and the chart data. }
    filter: TChartLegendEventHandler;
    { Label style will match corresponding point style (size is based on
      fontSize, boxWidth is not used in this case). }
    usePointStyle: Boolean;
  end;

  { Legend configuration class. }
  TChartLegendConfiguration = class external name 'Object' (TJSObject)
  public
    { Is the legend shown? }
    display: Boolean;
    { Position of the legend. }
    position: string;
    { Marks that this box should take the full width of the canvas (pushing down
      other boxes). This is unlikely to need to be changed in day-to-day use. }
    fullWidth: Boolean;
    { A callback that is called when a click event is registered on a label
      item. }
    onClick: TChartLegendClickEventHandler;
    { A callback that is called when a 'mousemove' event is registered on top of
    a label item. }
    onHover: TJSMouseEventHandler;
    { Legend will show datasets in reverse order. }
    reverse: Boolean;
    { Legend weight. }
    weight: NativeUInt;
    { Legend label configuration. }
    labels: TChartLegendLabel;
  end;

  { Title configuration class. }
  TChartTitleConfiguration = class external name 'Object' (TJSObject)
  public
    { Is the title shown? }
    display: Boolean;
    { Position of title. }
    position: string;
    { Font size. }
    fontSize: NativeUInt;
    { Font family for the title text. }
    fontFamily: string;
    { Font color. }
    fontColor: string;
    { Font style. }
    fontStyle: string;
    { Number of pixels to add above and below the title text. }
    padding: NativeInt;
    { Height of an individual line of text. }
    lineHeight: JSValue;
    { Title text to display. If specified as an array, text is rendered on
      multiple lines. }
    text: string;
    texts: TJSStringDynArray; external name 'text';
    text_: TJSArray; external name 'text';
  end;

  { Tooltip item class. }
  TChartTooltipItem = class external name 'Object' (TJSObject)
  public
    { X Value of the tooltip as a string. }
    xLabel: String;
    { Y value of the tooltip as a string. }
    yLabel: String;
    { Index of the dataset the item comes from. }
    datasetIndex: NativeInt;
    { Index of this data item in the dataset. }
    index: NativeInt;
    { X position of matching point. }
    x: NativeInt;
    { Y position of matching point. }
    y: NativeInt;
  end;

  { Tooltip model body class. }
  TChartTooltipModelBody = class external name 'Object' (TJSObject)
  public
    { Lines of text before the line with the color square. }
    before: TJSStringDynArray;
    { Lines of text to render as the main item with color square. }
    lines: TJSStringDynArray;
    { Lines of text to render after the main lines. }
    after: TJSStringDynArray;
  end;

  { Tooltip model class. }
  TChartTooltipModel = class external name 'Object' (TJSObject)
  public
    { The items that we are rendering in the tooltip. }
    dataPoints: array of TChartTooltipItem;
    dataPoints_: TJSArray; external name 'dataPoints';
    { Positioning. }
    xPadding: NativeInt;
    yPadding: NativeInt;
    xAlign: string;
    yAlign: string;
    { X and Y properties are the top left of the tooltip. }
    x: NativeInt;
    y: NativeInt;
    width: NativeUInt;
    height: NativeUInt;
    { Where the tooltip points to. }
    caretX: NativeInt;
    caretY: NativeInt;
    { Body
       The body lines that need to be rendered.
       Each object contains 3 parameters.
       before: String[] // lines of text before the line with the color square.
       lines: String[] // lines of text to render as the main item with color square.
       after: String[] // lines of text to render after the main lines. }
    body: array of TChartTooltipModelBody;
    body_: TJSArray; external name 'body';
    { Lines of text that appear after the title but before the body. }
    beforeBody: TJSStringDynArray;
    beforeBody_: TJSArray; external name 'beforeBody';
    { Line of text that appear after the body and before the footer. }
    afterBody: TJSStringDynArray;
    afterBody_: TJSArray; external name 'afterBody';
    bodyFontColor: string;
    _bodyFontFamily: string;
    _bodyFontStyle: string;
    _bodyAlign: string;
    bodyFontSize: NativeUInt;
    bodySpacing: NativeInt;
    { Title
       lines of text that form the title. }
    title: TJSStringDynArray;
    title_: TJSArray; external name 'title';
    titleFontColor: string;
    _titleFontFamily: string;
    _titleFontStyle: string;
    titleFontSize: NativeUInt;
    _titleAlign: string;
    titleSpacing: NativeInt;
    titleMarginBottom: NativeInt;
    { Footer
       lines of text that form the footer. }
    footer: TJSStringDynArray;
    footer_: TJSArray; external name 'footer';
    footerFontColor: string;
    _footerFontFamily: string;
    _footerFontStyle: string;
    footerFontSize: NativeUInt;
    _footerAlign: string;
    footerSpacing: NativeInt;
    footerMarginTop: NativeInt;
    { Appearance. }
    caretSize: NativeUInt;
    cornerRadius: NativeInt;
    backgroundColor: string;
    { Colors to render for each item in body[]. This is the color of the squares
      in the tooltip. }
    labelColors: TJSStringDynArray;
    labelColors_: TJSArray; external name 'labelColors';
    { 0 opacity is a hidden tooltip. }
    opacity: NativeUInt;
    legendColorBackground: string;
    displayColors: Boolean;
  end;

  { Tooltips custom event handler. }
  TChartTooltipsCustomEventHandler = reference to procedure(
    const tooltipModel: TChartTooltipModel);

  { Tooltip array event handler. }
  TChartTooltipArrayEventHandler = reference to function(
    const arr: array of TChartTooltipItem; const data: JSValue): string;

  { Tooltip item event handler. }
  TChartTooltipItemEventHandler = reference to function(
    const item: TChartTooltipItem; const data: JSValue): string;

  { Tooltip return colors class. }
  TChartTooltipReturnColors = class external name 'Object' (TJSObject)
  public
    { Border color. }
    borderColor: string;
    { Background color. }
    backgroundColor: string;
  end;

  { Tooltip colors event handler. }
  TChartTooltipColorsEventHandler = reference to function(
    const item: TChartTooltipItem;
    const data: JSValue): TChartTooltipReturnColors;

  { Tooltip callbacks class. }
  TChartTooltipCallbacks = class external name 'Object' (TJSObject)
  public
    { Returns the text to render before the title. }
    beforeTitle: TChartTooltipArrayEventHandler;
    { Returns text to render as the title of the tooltip. }
    title: TChartTooltipArrayEventHandler;
    { Returns text to render after the title. }
    afterTitle: TChartTooltipArrayEventHandler;
    { Returns text to render before the body section. }
    beforeBody: TChartTooltipArrayEventHandler;
    { Returns text to render before an individual label. This will be called for
      each item in the tooltip. }
    beforeLabel: TChartTooltipItemEventHandler;
    { Returns text to render for an individual item in the tooltip. }
    label_: TChartTooltipItemEventHandler; external name 'label';
    { Returns the colors to render for the tooltip item. }
    labelColor: TChartTooltipColorsEventHandler;
    { Returns the colors for the text of the label for the tooltip item. }
    labelTextColor: TChartTooltipItemEventHandler;
    { Returns text to render after an individual label. }
    afterLabel: TChartTooltipItemEventHandler;
    { Returns text to render after the body section. }
    afterBody: TChartTooltipArrayEventHandler;
    { Returns text to render before the footer section. }
    beforeFooter: TChartTooltipArrayEventHandler;
    { Returns text to render as the footer of the tooltip. }
    footer: TChartTooltipArrayEventHandler;
    { Text to render after the footer section. }
    afterFooter: TChartTooltipArrayEventHandler;
  end;

  { Tooltip configuration class. }
  TChartTooltipsConfiguration = class external name 'Object' (TJSObject)
  public
    { Are on-canvas tooltips enabled. }
    enabled: Boolean;
    { Custom tooltips allow you to hook into the tooltip rendering process so
      that you can render the tooltip in your own custom way. Generally this is
      used to create an HTML tooltip instead of an oncanvas one. }
    custom: TChartTooltipsCustomEventHandler;
    { Sets which elements appear in the tooltip. }
    mode: string;
    { If true, the tooltip mode applies only when the mouse position intersects
      with an element. If false, the mode will be applied at all times. }
    intersect: Boolean;
    { The mode for positioning the tooltip. }
    position: string;
    { Tooltip callbacks. }
    callbacks: TChartTooltipCallbacks;
    { Allows sorting of tooltip items. }
    itemSort: TJSArrayCompareEvent;
    { Allows filtering of tooltip items. }
    filter: TJSArrayCallback;
    { Background color of the tooltip. }
    backgroundColor: string;
    { Title font. }
    titleFontFamily: string;
    { Title font size. }
    titleFontSize: NativeUInt;
    { Title font style. }
    titleFontStyle: string;
    { Title font color. }
    titleFontColor: string;
    { Spacing to add to top and bottom of each title line. }
    titleSpacing: NativeInt;
    { Margin to add on bottom of title section. }
    titleMarginBottom: NativeInt;
    { Body line font. }
    bodyFontFamily: string;
    { Body font size. }
    bodyFontSize: NativeUInt;
    { Body font style. }
    bodyFontStyle: string;
    { Body font color. }
    bodyFontColor: string;
    { Spacing to add to top and bottom of each tooltip item. }
    bodySpacing: NativeInt;
    { Footer font. }
    footerFontFamily: string;
    { Footer font size. }
    footerFontSize: NativeUInt;
    { Footer font style. }
    footerFontStyle: string;
    { Footer font color. }
    footerFontColor: string;
    { Spacing to add to top and bottom of each footer line. }
    footerSpacing: NativeInt;
    { Margin to add before drawing the footer. }
    footerMarginTop: NativeInt;
    { Padding to add on left and right of tooltip. }
    xPadding: NativeInt;
    { Padding to add on top and bottom of tooltip. }
    yPadding: NativeInt;
    { Extra distance to move the end of the tooltip arrow away from the tooltip
      point. }
    caretPadding: NativeInt;
    { Size, in px, of the tooltip arrow. }
    caretSize: NativeUInt;
    { Radius of tooltip corner curves. }
    cornerRadius: NativeInt;
    { Color to draw behind the colored boxes when multiple items are in the
      tooltip. }
    multiKeyBackground: string;
    { If true, color boxes are shown in the tooltip. }
    displayColors: Boolean;
    { Color of the border. }
    borderColor: string;
    { Size of the border. }
    borderWidth: NativeUInt;
  end;

  { Element point class. }
  TChartElementPoint = class external name 'Object' (TJSObject)
  public
    { Point radius. }
    radius: NativeInt;
    radius_: JSValue; external name 'radius';
    { Point style. }
    pointStyle: string;
    { Point rotation (in degrees). }
    rotation: NativeInt;
    { Point fill color. }
    backgroundColor: string;
    backgroundColor_: JSValue; external name 'backgroundColor';
    { Point stroke width. }
    borderWidth: NativeUInt;
    borderWidth_: JSValue; external name 'borderWidth';
    { Point stroke color. }
    borderColor: string;
    borderColor_: JSValue; external name 'borderColor';
    { Extra radius added to point radius for hit detection. }
    hitRadius: NativeInt;
    { Point radius when hovered. }
    hoverRadius: NativeInt;
    { Stroke width when hovered. }
    hoverBorderWidth: NativeUInt;
    hoverBorderWidth_: JSValue; external name 'hoverBorderWidth';
    { Background colour when hovered. }
    hoverBackgroundColor: string;
    hoverBackgroundColor_: JSValue; external name 'hoverBackgroundColor';
    { Border colour when hovered. }
    hoverBorderColor: string;
    hoverBorderColor_: JSValue; external name 'hoverBorderColor';
  end;

  { Element line class. }
  TChartElementLine = class external name 'Object' (TJSObject)
  public
    { Bézier curve tension (0 for no Bézier curves). }
    tension: Double;
    { Line fill color. }
    backgroundColor: string;
    backgroundColor_: JSValue; external name 'backgroundColor';
    { Line stroke width. }
    borderWidth: NativeUInt;
    borderWidth_: JSValue; external name 'borderWidth';
    { Line stroke color. }
    borderColor: string;
    borderColor_: JSValue; external name 'borderColor';
    { Line cap style. }
    borderCapStyle: string;
    { Line dash. }
    borderDash: array of NativeInt;
    borderDash_: TJSArray; external name 'borderDash';
    { Line dash offset. }
    borderDashOffset: NativeInt;
    { Line join style. }
    borderJoinStyle: string;
    { true to keep Bézier control inside the chart, false for no restriction. }
    capBezierPoints: Boolean;
    { Fill location: 'zero', 'top', 'bottom', true (eq. 'zero') or false
      (no fill). }
    fill: JSValue;
    { true to show the line as a stepped line (tension will be ignored). }
    stepped: Boolean;
  end;

  { Element rectangle class. }
  TChartElementRectangle = class external name 'Object' (TJSObject)
  public
    { Bar fill color. }
    backgroundColor: string;
    backgroundColor_: JSValue; external name 'backgroundColor';
    { Bar stroke width. }
    borderWidth: NativeUInt;
    borderWidth_: JSValue; external name 'borderWidth';
    { Bar stroke color. }
    borderColor: string;
    borderColor_: JSValue; external name 'borderColor';
    { Skipped (excluded) border: 'bottom', 'left', 'top' or 'right'. }
    borderSkipped: string;
  end;

  { Element arc class. }
  TChartElementArc = class external name 'Object' (TJSObject)
  public
    { Arc fill color. }
    backgroundColor: string;
    backgroundColor_: JSValue; external name 'backgroundColor';
    { Arc stroke color. }
    borderColor: string;
    borderColor_: JSValue; external name 'borderColor';
    { Arc stroke width. }
    borderWidth: NativeUInt;
    borderWidth_: JSValue; external name 'borderWidth';
  end;

  { Elements configuration class. }
  TChartElementsConfiguration = class external name 'Object' (TJSObject)
  public
    { Point configuration. }
    point: TChartElementPoint;
    { Line configuration. }
    line: TChartElementLine;
    { Rectangle configuration. }
    rectangle: TChartElementRectangle;
    { Arc configuration. }
    arc: TChartElementArc;
  end;

  { Scale service class. }
  TCharScaleService = class external name 'Object' (TJSObject)
  public
    (* Scale config defaults. *)
    procedure registerScaleType(const type_: string;
      const scaleConstructor, scaleDefaults: JSValue);
    function getScaleConstructor(const type_: string): JSValue;
    function getScaleDefaults(const type_: string): JSValue;
    procedure updateScaleDefaults(const type_: string; const additions: JSValue);
    procedure addScalesToLayout(chart: TChart);
  end;

  { Scale callback. }
  TChartScaleCallback = reference to procedure(const axis: TJSObject);

  { Scale callbacks class. }
  TChartScaleCallbacks = class external name 'Object' (TJSObject)
  public
    { Callback called before the update process starts. }
    beforeUpdate: TChartScaleCallback;
    { Callback that runs before dimensions are set. }
    beforeSetDimensions: TChartScaleCallback;
    { Callback that runs after dimensions are set. }
    afterSetDimensions: TChartScaleCallback;
    { Callback that runs before data limits are determined. }
    beforeDataLimits: TChartScaleCallback;
    { Callback that runs after data limits are determined. }
    afterDataLimits: TChartScaleCallback;
    { Callback that runs before ticks are created. }
    beforeBuildTicks: TChartScaleCallback;
    { Callback that runs after ticks are created. Useful for filtering ticks. }
    afterBuildTicks: TChartScaleCallback;
    { Callback that runs before ticks are converted into strings. }
    beforeTickToLabelConversion: TChartScaleCallback;
    { Callback that runs after ticks are converted into strings. }
    afterTickToLabelConversion: TChartScaleCallback;
    { Callback that runs before tick rotation is determined. }
    beforeCalculateTickRotation: TChartScaleCallback;
    { Callback that runs after tick rotation is determined. }
    afterCalculateTickRotation: TChartScaleCallback;
    { Callback that runs before the scale fits to the canvas. }
    beforeFit: TChartScaleCallback;
    { Callback that runs after the scale fits to the canvas. }
    afterFit: TChartScaleCallback;
    { Callback that runs at the end of the update process. }
    afterUpdate: TChartScaleCallback;
  end;

  { Scale class. }
  TChartScaleConfiguration = class external name 'Object' (TJSObject)
  public
    { If set to false the axis is hidden from view. Overrides gridLines.display,
      scaleLabel.display, and ticks.display. }
    display: Boolean;
    { Callback functions to hook into the axis lifecycle. }
    callbacks: TChartScaleCallbacks;
    { The weight used to sort the axis. Higher weights are further away from the
      chart area. }
    weight: NativeUInt;
  end;

  { Scales class. }
  TChartScalesConfiguration = class external name 'Object' (TJSObject)
  public
    { xAxes properties. }
    xAxes: array of JSValue;
    xAxes_: TJSArray; external name 'xAxes';
    { yAxes properties. }
    yAxes: array of JSValue;
    yAxes_: TJSArray; external name 'yAxes';
  end;

  { Base axes class. }
  TChartAxes = class external name 'Object' (TChartDataset)
  public
    { Stacked chart. }
    stacked: Boolean;
  end;

  { Scale grid line class. }
  TChartScaleGridLine = class external name 'Object' (TJSObject)
  public
    { If false, do not display grid lines for this axis. }
    display: Boolean;
    { If true, gridlines are circular (on radar chart only). }
    circular: Boolean;
    { The color of the grid lines. If specified as an array, the first color applies to the first grid line, the second to the second grid line and so on. }
    color: string;
    colors: string; external name 'color';
    color_: TJSArray; external name 'color';
    { Length and spacing of dashes on grid lines. }
    borderDash: NativeInt;
    borderDashs: array of NativeInt; external name 'borderDash';
    borderDash_: TJSArray; external name 'borderDash';
    { Offset for line dashes. }
    borderDashOffset: NativeInt;
    { Stroke width of grid lines. }
    lineWidth: NativeUInt;
    lineWidths: array of NativeUInt; external name 'lineWidth';
    lineWidth_: TJSArray; external name 'lineWidth';
    { If true, draw border at the edge between the axis and the chart area. }
    drawBorder: Boolean;
    { If true, draw lines on the chart area inside the axis lines. This is
      useful when there are multiple axes and you need to control which grid
      lines are drawn. }
    drawOnChartArea: Boolean;
    { If true, draw lines beside the ticks in the axis area beside the chart. }
    drawTicks: Boolean;
    { Length in pixels that the grid lines will draw into the axis area. }
    tickMarkLength: NativeUInt;
    { Stroke width of the grid line for the first index (index 0). }
    zeroLineWidth: NativeUInt;
    { Stroke color of the grid line for the first index (index 0). }
    zeroLineColor: string;
    { Length and spacing of dashes of the grid line for the first
      index (index 0). }
    zeroLineBorderDash: NativeInt;
    zeroLineBorderDashs: array of NativeInt; external name 'zeroLineBorderDash';
    zeroLineBorderDash_: TJSArray; external name 'zeroLineBorderDash';
    { Offset for line dashes of the grid line for the first index (index 0). }
    zeroLineBorderDashOffset: NativeInt;
    { If true, grid lines will be shifted to be between labels. This is set to
      true for a category scale in a bar chart by default. }
    offsetGridLines: Boolean;
  end;

  { Scale label class. }
  TChartScaleLabel = class external name 'Object' (TJSObject)
  public
    { If true, display the axis title. }
    display: Boolean;
    { The text for the title. (i.e. "# of People" or "Response Choices"). }
    labelString: string;
    { Height of an individual line of text. }
    lineHeight: JSValue;
    { Font color for scale title. }
    fontColor: string;
    { Font family for the scale title, follows CSS font-family options. }
    fontFamily: string;
    { Font size for scale title. }
    fontSize: NativeUInt;
    { Font style for the scale title, follows CSS font-style options (i.e.
      normal, italic, oblique, initial, inherit). }
    fontStyle: string;
    { Padding to apply around scale labels. Only top and bottom are
      implemented. }
    padding: JSValue;
  end;

  { Scale cartesian tick class. }
  TChartScaleCartesianTick = class external name 'Object' (TChartAxes)
  public
    { If true, automatically calculates how many labels that can be shown and
      hides labels accordingly. Turn it off to show all labels no matter what. }
    autoSkip: Boolean;
    { Padding between the ticks on the horizontal axis when autoSkip is enabled.
      Note: Only applicable to horizontal scales. }
    autoSkipPadding: NativeInt;
    { Distance in pixels to offset the label from the centre point of the
      tick (in the y direction for the x axis, and the x direction for the
      y axis). Note: this can cause labels at the edges to be cropped by
      the edge of the canvas. }
    labelOffset: NativeInt;
    { Maximum rotation for tick labels when rotating to condense labels.
      Note: Rotation doesn't occur until necessary. Note: Only applicable to
      horizontal scales. }
    maxRotation: NativeInt;
    { Minimum rotation for tick labels. Note: Only applicable to horizontal
      scales. }
    minRotation: NativeInt;
    { Flips tick labels around axis, displaying the labels inside the chart
      instead of outside. Note: Only applicable to vertical scales. }
    mirror: Boolean;
    { Padding between the tick label and the axis. When set on a vertical axis,
      this applies in the horizontal (X) direction. When set on a horizontal
      axis, this applies in the vertical (Y) direction. }
    padding: NativeInt;
  end;

  { Scale cartesian class. }
  TChartScaleCartesian = class external name 'Object' (TChartAxes)
  public
    { type (inherited):
      Type of scale being employed. Custom scales can be created and registered
      with a string key. This allows changing the type of an axis for a chart. }
    { Position of the axis in the chart. Possible values are: 'top', 'left',
      'bottom', 'right'. }
    position: string;
    { If true, extra space is added to the both edges and the axis is scaled to
      fit into the chart area. This is set to true for a category scale in a bar
      chart by default. }
    offset: Boolean;
    { The ID is used to link datasets and scale axes together. }
    id: string;
    { Grid lines configuration. }
    gridLines: TChartScaleGridLine;
    { Scale label configuration. }
    scaleLabel: TChartScaleLabel;
    { Tick configuration. }
    ticks: TChartScaleCartesianTick;
  end;

  { Scale cartesian category tick class. }
  TChartScaleCartesianCategoryTick = class external name 'Object' (TChartScaleCartesianTick)
  public
    { An array of labels to display. }
    labels: TJSStringDynArray;
    labels_: TJSArray; external name 'labels';
    { The minimum item to display. }
    min: string;
    { The maximum item to display. }
    max: string;
  end;

  { Scale cartesian linear tick class. }
  TChartScaleCartesianLinearTick = class external name 'Object' (TChartScaleCartesianTick)
  public
    { If true, scale will include 0 if it is not already included. }
    beginAtZero: Boolean;
    { User defined minimum number for the scale, overrides minimum value from
      data. }
    min: NativeUInt;
    { User defined maximum number for the scale, overrides maximum value from
      data. }
    max: NativeUInt;
    { Maximum number of ticks and gridlines to show. }
    maxTicksLimit: NativeUInt;
    { If defined and stepSize is not specified, the step size will be rounded to
      this many decimal places. }
    precision: NativeInt;
    { User defined fixed step size for the scale. }
    stepSize: NativeUInt;
    { Adjustment used when calculating the maximum data value. }
    suggestedMax: NativeUInt;
    { Adjustment used when calculating the minimum data value. }
    suggestedMin: NativeUInt;
  end;

  { Scale cartesian logarithmic tick class. }
  TChartScaleCartesianLogarithmicTick = class external name 'Object' (TChartScaleCartesianTick)
  public
    { User defined minimum number for the scale, overrides minimum value from
      data. }
    min: NativeUInt;
    { User defined maximum number for the scale, overrides maximum value from
      data. }
    max: NativeUInt;
  end;

  { Scale cartesian time tick class. }
  TChartScaleCartesianTimeTick = class external name 'Object' (TJSObject)
  public
    { How ticks are generated. }
    source: string;
  end;

  { Scale cartesian time class. }
  TChartScaleCartesianTime = class external name 'Object' (TChartScaleCartesianTick)
  public
    { Is the time shown? }
    display: Boolean;
    { How data is plotted. }
    distribution: string;
    { Determines the scale bounds. }
    bounds: string;
    {
    ticks.source:
      How ticks are generated. }
    ticks: TChartScaleCartesianTimeTick;
    {
    time.displayFormats:
      Sets how different time units are displayed. time.isoWeekday - If true and
      the unit is set to 'week', then the first day of the week will be Monday.
      Otherwise, it will be Sunday.
    time.max:
      If defined, this will override the data maximum.
    time.min:
      If defined, this will override the data minimum.
    time.parser:
      Custom parser for dates.
    time.round:
      If defined, dates will be rounded to the start of this unit. See Time
      Units below for the allowed units.
    time.tooltipFormat:
      The moment js format string to use for the tooltip.
    time.unit:
      If defined, will force the unit to be a certain type. See Time Units
      section below for details.
    time.stepSize:
      The number of units between grid lines.
    time.minUnit:
      The minimum display format to be used for a time unit.
    }
    time: TChartMoment;
  end;

  { Scale radial linear point label class. }
  TChartScaleRadialLinearPointLabel = class external name 'Object' (TChartAxes)
  public
    { Callback function to transform data labels to point labels. The default
      implementation simply returns the current string. }
    callback: TJSPromiseResolver;
    { Font color for point labels. }
    fontColor: string;
    fontColors: TJSStringDynArray; external name 'fontColor';
    fontColor_: TJSArray; external name 'fontColor';
    { Font family to use when rendering labels. }
    fontFamily: string;
    { Font size in pixels. }
    fontSize: NativeUInt;
    { Font style to use when rendering point labels. }
    fontStyle: string;
  end;

  { Scale radial linear tick class. }
  TChartScaleRadialLinearTick = class external name 'Object' (TChartAxes)
  public
    { Color of label backdrops. }
    backdropColor: string;
    { Horizontal padding of label backdrop. }
    backdropPaddingX: NativeInt;
    { Vertical padding of label backdrop. }
    backdropPaddingY: NativeInt;
    { If true, scale will include 0 if it is not already included. }
    beginAtZero: Boolean;
    { User defined minimum number for the scale, overrides minimum value from
      data. }
    min: NativeUInt;
    { User defined maximum number for the scale, overrides maximum value from
      data. }
    max: NativeUInt;
    { Maximum number of ticks and gridlines to show. }
    maxTicksLimit: NativeUInt;
    { If defined and stepSize is not specified, the step size will be rounded to
      this many decimal places. }
    precision: NativeInt;
    { User defined fixed step size for the scale. }
    stepSize: NativeUInt;
    { Adjustment used when calculating the maximum data value. }
    suggestedMax: NativeUInt;
    { Adjustment used when calculating the minimum data value. }
    suggestedMin: NativeUInt;
    { If true, draw a background behind the tick labels. }
    showLabelBackdrop: Boolean;
  end;

  { Scale radial linear angle line class. }
  TChartScaleRadialLinearAngleLine = class external name 'Object' (TChartAxes)
  public
    { If true, angle lines are shown. }
    display: Boolean;
    { Color of angled lines. }
    color: string;
    { Width of angled lines. }
    lineWidth: NativeUInt;
  end;

  { Scale radial linear class. }
  TChartScaleRadialLinear = class external name 'Object' (TChartAxes)
  public
    { Angle line configuration. }
    angleLines: TChartScaleRadialLinearAngleLine;
    { Grid line configuration. }
    gridLines: TChartScaleGridLine;
    { Point label configuration. }
    pointLabels: TChartScaleRadialLinearAngleLine;
    { Tick configuration. }
    ticks: TChartScaleRadialLinearTick;
  end;

  { Plugins hook argument class. }
  TChartPluginsHookArgument = class external name 'Object' (TJSObject)
  public
    { The dataset index. }
    index: NativeInt;
    { The dataset metadata. }
    meta: TJSObject;
    { The current animation value, between 0.0 and 1.0. }
    easingValue: Double;
  end;

{$PUSH}{$WARN 4501 OFF}

  { Plugins hook class. }
  TChartPluginsHook = class(TJSObject)
  public
    { Creates an instance of TChartPluginsHook. }
    constructor new;
    { Called before initializing chart. }
    procedure beforeInit(chart: TChartController;
      options: TChartOptions); virtual;
    { Called after chart has been initialized and before the first update. }
    procedure afterInit(chart: TChartController;
      options: TChartOptions); virtual;
    { Called before updating chart. If any plugin returns false, the update. }
    function beforeUpdate(chart: TChartController;
      options: TChartOptions): Boolean; virtual;
    { Called after chart has been updated and before rendering. Note that this
      hook will not be called if the chart update has been previously cancelled. }
    procedure afterUpdate(chart: TChartController;
      options: TChartOptions); virtual;
    { Called before updating the chart datasets. If any plugin returns false,
      the datasets update is cancelled until another update is triggered. }
    function beforeDatasetsUpdate(chart: TChartController;
      options: TChartOptions): Boolean; virtual;
    { Called after the chart datasets have been updated. Note that this hook
      will not be called if the datasets update has been previously cancelled. }
    procedure afterDatasetsUpdate(chart: TChartController;
      options: TChartOptions); virtual;
    { Called before updating the chart dataset at the given args.index. If any
      plugin returns false, the datasets update is cancelled until another
      update is triggered. }
    function beforeDatasetUpdate(chart: TChartController;
      const args: TChartPluginsHookArgument;
      options: TChartOptions): Boolean; virtual;
    { Called after the chart datasets at the given args.index has been updated.
      Note that this hook will not be called if the datasets update has been
      previously cancelled. }
    procedure afterDatasetUpdate(chart: TChartController;
      const args: TChartPluginsHookArgument; options: TChartOptions); virtual;
    { Called before laying out chart. If any plugin returns false, the layout
      update is cancelled until another update is triggered. }
    function beforeLayout(chart: TChartController;
      options: TChartOptions): Boolean; virtual;
    { Called after the chart has been layed out. Note that this hook will not be
      called if the layout update has been previously cancelled. }
    procedure afterLayout(chart: TChartController;
      options: TChartOptions); virtual;
    { Called before rendering chart. If any plugin returns false, the rendering
      is cancelled until another render is triggered. }
    function beforeRender(chart: TChartController;
      options: TChartOptions): Boolean; virtual;
    { Called after the chart has been fully rendered (and animation completed).
      Note that this hook will not be called if the rendering has been
      previously cancelled. }
    procedure afterRender(chart: TChartController;
      options: TChartOptions); virtual;
    { Called before drawing chart at every animation frame specified by the
      given easing value. If any plugin returns false, the frame drawing is
      cancelled until another render is triggered. }
    function beforeDraw(chart: TChartController; easingValue: NativeInt;
      options: TChartOptions): Boolean; virtual;
    { Called after the chart has been drawn for the specific easing value.
      Note that this hook will not be called if the drawing has been previously
      cancelled. }
    procedure afterDraw(chart: TChartController; easingValue: NativeInt;
      options: TChartOptions); virtual;
    { Called before drawing the chart datasets. If any plugin returns false, the
      datasets drawing is cancelled until another render is triggered. }
    function beforeDatasetsDraw(chart: TChartController; easingValue: NativeInt;
      options: TChartOptions): Boolean; virtual;
    { Called after the chart datasets have been drawn. Note that this hook will
      not be called if the datasets drawing has been previously cancelled. }
    procedure afterDatasetsDraw(chart: TChartController; easingValue: NativeInt;
      options: TChartOptions); virtual;
    { Called before drawing the chart dataset at the given args.index (datasets
      are drawn in the reverse order). If any plugin returns false, the datasets
      drawing is cancelled until another render is triggered. }
    function beforeDatasetDraw(chart: TChartController;
      const args: TChartPluginsHookArgument;
      options: TChartOptions): Boolean; virtual;
    { Called after the chart datasets at the given args.index have been drawn
      (datasets are drawn in the reverse order). Note that this hook will not be
      called if the datasets drawing has been previously cancelled. }
    procedure afterDatasetDraw(chart: TChartController;
      const args: TChartPluginsHookArgument; options: TChartOptions); virtual;
    { Called before drawing the tooltip. If any plugin returns false, the
      tooltip drawing is cancelled until another render is triggered. }
    function beforeTooltipDraw(chart: TChartController;
      const args: TChartPluginsHookArgument;
      options: TChartOptions): Boolean; virtual;
    { Called after drawing the tooltip. Note that this hook will not be called
      if the tooltip drawing has been previously cancelled. }
    procedure afterTooltipDraw(chart: TChartController;
      const args: TChartPluginsHookArgument; options: TChartOptions); virtual;
    { Called before processing the specified event. If any plugin returns false,
      the event will be discarded. }
    procedure beforeEvent(chart: TChartController; event: TJSEvent;
      options: TChartOptions); virtual;
    { Called after the event has been consumed. Note that this hook will not be
      called if the event has been previously discarded. }
    procedure afterEvent(chart: TChartController; event: TJSEvent;
      options: TChartOptions); virtual;
    { Called after the chart as been resized. }
    procedure resize(chart: TChartController; size: NativeInt;
      options: TChartOptions); virtual;
    { Called after the chart as been destroyed. }
    procedure destroy(chart: TChartController; options: TChartOptions); virtual;
  end;

{$POP}

  { Plugins class. }
  TChartPlugins = class external name 'Chart' (TJSObject)
  public
    { Registers the given plugin(s) if not already registered. }
    procedure register(plugin: TChartPlugins); overload;
    procedure register(plugin: TJSObject); overload;
    procedure register(plugin: TJSArray); overload;
    { Unregisters the given plugin(s) only if registered. }
    procedure unregister(plugin: TChartPlugins); overload;
    procedure unregister(plugin: TJSObject); overload;
    procedure unregister(plugin: TJSArray); overload;
    { Remove all registered plugins. }
    procedure clear;
    { Returns the number of registered plugins. }
    function count: NativeUInt;
    { Returns all registered plugin instances. }
    function getAll: TJSArray;
    { Calls enabled plugins for `chart` on the specified hook and with the given
      args. This method immediately returns as soon as a plugin explicitly
      returns false. The returned value can be used, for instance, to interrupt
      the current action. }
    function notify(chart: TChart; const hook: string;
      const args: TJSArray): Boolean; overload;
    function notify(chart: TChart; const hook: string): Boolean; varargs; overload;
  end;

  { Plugins filler class. }
  TChartPluginsFiller = class external name 'Object' (TJSObject)
  public
    { Fill propagation when target is hidden. }
    propagate: Boolean;
  end;

  { Plugins configuration class. }
  TChartPluginsConfiguration = class external name 'Object' (TChartPlugins)
  public
    { Filler properties. }
    filler: TChartPluginsFiller;
  end;

  { Options context class. }
  TChartOptionsContext = class external name 'Object' (TJSObject)
  public
    { The associated chart. }
    chart: TChartController;
    { Index of the current data. }
    dataIndex: NativeInt;
    { Dataset at index datasetIndex. }
    dataset: TChartDataset;
    { Index of the current dataset. }
    datasetIndex: NativeInt;
  end;

  { Options class. }
  TChartOptions = class external name 'Object' (TJSObject)
  public
    { Resizes the chart canvas when its container does. }
    responsive: Boolean;
    { Duration in milliseconds it takes to animate to new size after a resize
      event. }
    responsiveAnimationDuration: NativeUInt;
    { Maintain the original canvas aspect ratio (width / height) when resizing. }
    maintainAspectRatio: Boolean;
    { Canvas aspect ratio (i.e. width / height, a value of 1 representing a
      square canvas). Note that this option is ignored if the height is
      explicitly defined either as attribute or via the style. }
    aspectRatio: NativeInt;
    { Called when a resize occurs. Gets passed two arguments: the chart instance
      and the new size. }
    onResize: TChartResizeEventHandler;
    { Override the window's default devicePixelRatio. }
    devicePixelRatio: NativeInt;
    { Configure hover properties. }
    hover: TChartHover;
    { The events option defines the browser events that the chart should listen
      to for tooltips and hovering. }
    events: TJSStringDynArray;
    events_: TJSArray; external name 'events';
    { Called when any of the events fire. Called in the context of the chart and
      passed the event and an array of active elements (bars, points, etc). }
    onHover: TJSMouseEventHandler;
    { Called if the event is of type 'mouseup' or 'click'. Called in the context
      of the chart and passed the event and an array of active elements. }
    onClick: THTMLClickEventHandler;
    { Default color used in chart colors. }
    defaultColor: string;
    { Default font color for all text. }
    defaultFontColor: string;
    { Default font family for all text. }
    defaultFontFamily: string;
    { Default font size (in px) for text. Does not apply to radialLinear scale
      point labels. }
    defaultFontSize: NativeUInt;
    { Default font style. Does not apply to tooltip title or footer. Does not
      apply to chart title. }
    defaultFontStyle: string;
    { Default line height for all lines. }
    defaultLineHeight: JSValue;
    { If false, the lines between points are not drawn. }
    showLines: Boolean;
    { Configure animation properties. }
    animation: TChartAnimationConfiguration;
    animation_: JSValue; external name 'animation';
    { Configure layout properties. }
    layout: TChartLayoutConfiguration;
    layout_: JSValue; external name 'layout';
    { Default legend properties. }
    legend: TChartLegendConfiguration;
    legend_: JSValue; external name 'legend';
    { Configure title properties. }
    title: TChartTitleConfiguration;
    title_: JSValue; external name 'title';
    { Configure tooltip properties. }
    tooltips: TChartTooltipsConfiguration;
    tooltips_: JSValue; external name 'tooltips';
    { Configure elements properties. }
    elements: TChartElementsConfiguration;
    elements_: JSValue; external name 'elements';
    { Configure chart scale. }
    scale: TChartScaleConfiguration;
    scale_: JSValue; external name 'scale';
    { Configure chart scales. }
    scales: TChartScalesConfiguration;
    scales_: JSValue; external name 'scales';
    { Configure plugins properties. }
    plugins: TChartPluginsConfiguration;
    plugins_: JSValue; external name 'plugins';
  end;

  { X/Y data class. }
  TChartXYData = class(TJSObject)
  public
    { X value. }
    x: JSValue;
    x_: Double; external name 'x';
    { Y value. }
    y: JSValue;
    y_: Double; external name 'y';
    { Creates an instance of TChartXYData. }
    constructor new(const x, y: JSValue); overload;
    constructor new(const x: JSValue); overload;
  end;

  { T/Y data class. }
  TChartTYData = class(TJSObject)
  public
    { T value. }
    t: JSValue;
    t_: Double; external name 't';
    { Y value. }
    y: JSValue;
    y_: Double; external name 'y';
    { Creates an instance of TChartTYData. }
    constructor new(const t, y: JSValue); overload;
    constructor new(const t: JSValue); overload;
  end;

  { X/Y/R data class. }
  TChartXYRData = class(TJSObject)
  public
    { X value. }
    x: JSValue;
    x_: Double; external name 'x';
    { Y value. }
    y: JSValue;
    y_: Double; external name 'y';
    { R value. }
    r: JSValue;
    r_: Double; external name 'e';
    { Creates an instance of TChartXYRData. }
    constructor new(const x, y, r: JSValue); overload;
    constructor new(const x, y: JSValue); overload;
    constructor new(const x: JSValue); overload;
  end;

  { X/Y/V data class. }
  TChartXYVData = class(TJSObject)
  public
    { X value. }
    x: JSValue;
    x_: Double; external name 'x';
    { Y value. }
    y: JSValue;
    y_: Double; external name 'y';
    { V value. }
    v: JSValue;
    v_: Double; external name 'v';
    { Creates an instance of TChartXYVData. }
    constructor new(const x, y, v: JSValue); overload;
    constructor new(const x, y: JSValue); overload;
    constructor new(const x: JSValue); overload;
  end;

  { Data class. }
  TChartData = class external name 'Object' (TJSObject)
  public
    { Label array passed to the chart. }
    labels: TJSStringDynArray;
    labels_: TJSArray; external name 'labels';
    { Dataset array passed to the chart. }
    datasets: array of TChartDataset;
    datasets_: TJSArray; external name 'datasets';
  end;

  { Configuration class. }
  TChartConfiguration = class external name 'Object' (TJSObject)
  public
    { Chart type. }
    type_: string; external name 'type';
    { Chart data. }
    data: TChartData;
    { Chart options. }
    options: TChartOptions;
    { Plugins options. }
    plugins: array of TChartPlugins;
    plugins_: TJSArray; external name 'plugins';
  end;

  { Line dataset class. }
  TChartLineDataset = class external name 'Object' (TChartDataset)
  public
    { The ID of the x axis to plot this dataset on. If not specified, this
      defaults to the ID of the first found x axis. }
    xAxisID: string;
    { The ID of the y axis to plot this dataset on. If not specified, this
      defaults to the ID of the first found y axis. }
    yAxisID: string;
    { The fill color under the line. }
    backgroundColor: string;
    { The color of the line. }
    borderColor: string;
    { The width of the line in pixels. }
    borderWidth: NativeUInt;
    { Length and spacing of dashes. }
    borderDash: array of NativeInt;
    borderDash_: TJSArray; external name 'borderDash';
    { Offset for line dashes. }
    borderDashOffset: NativeInt;
    { Cap style of the line. }
    borderCapStyle: string;
    { Line joint style. }
    borderJoinStyle: string;
    { Algorithm used to interpolate a smooth curve from the discrete data
      points. }
    cubicInterpolationMode: string;
    { How to fill the area under the line. }
    fill: JSValue;
    { Bezier curve tension of the line. Set to 0 to draw straightlines. This
      option is ignored if monotone cubic interpolation is used. }
    lineTension: Double;
    { The fill color for points. }
    pointBackgroundColor: string;
    pointBackgroundColors: TJSStringDynArray; external name 'pointBackgroundColor';
    pointBackgroundColor_: TJSArray; external name 'pointBackgroundColor';
    { The border color for points. }
    pointBorderColor: string;
    pointBorderColors: TJSStringDynArray; external name 'pointBorderColor';
    pointBorderColor_: TJSArray; external name 'pointBorderColor';
    { The width of the point border in pixels. }
    pointBorderWidth: NativeUInt;
    pointBorderWidths: array of NativeUInt; external name 'pointBorderWidth';
    pointBorderWidth_: TJSArray; external name 'pointBorderWidth';
    { The radius of the point shape. If set to 0, the point is not rendered. }
    pointRadius: NativeInt;
    pointRadiuses: array of NativeInt; external name 'pointRadius';
    pointRadius_: TJSArray; external name 'pointRadius';
    { Style of the point. }
    pointStyle: string;
    pointStyles: TJSStringDynArray; external name 'pointStyle';
    pointStyle_: TJSArray; external name 'pointStyle';
    pointStyleImage: TJSObject; external name 'pointStyle';
    pointStyleImage_: TJSObjectDynArray; external name 'pointStyle';
    { The rotation of the point in degrees. }
    pointRotation: NativeInt;
    pointRotations: array of NativeInt; external name 'pointRotation';
    pointRotation_: TJSArray; external name 'pointRotation';
    { The pixel size of the non-displayed point that reacts to mouse events. }
    pointHitRadius: NativeInt;
    pointHitRadiuses: array of NativeInt; external name 'pointHitRadius';
    pointHitRadius_: TJSArray; external name 'pointHitRadius';
    { Point background color when hovered. }
    pointHoverBackgroundColor: string;
    pointHoverBackgroundColors: TJSStringDynArray; external name 'pointHoverBackgroundColor';
    pointHoverBackgroundColor_: TJSArray; external name 'pointHoverBackgroundColor';
    { Point border color when hovered. }
    pointHoverBorderColor: string;
    pointHoverBorderColors: TJSStringDynArray; external name 'pointHoverBorderColor';
    pointHoverBorderColor_: TJSArray; external name 'pointHoverBorderColor';
    { Border width of point when hovered. }
    pointHoverBorderWidth: NativeUInt;
    pointHoverBorderWidths: array of NativeUInt; external name 'pointHoverBorderWidth';
    pointHoverBorderWidth_: TJSArray; external name 'pointHoverBorderWidth';
    { The radius of the point when hovered. }
    pointHoverRadius: NativeInt;
    pointHoverRadiuses: array of NativeInt; external name 'pointHoverRadius';
    pointHoverRadius_: TJSArray; external name 'pointHoverRadius';
    { If false, the line is not drawn for this dataset. }
    showLine: Boolean;
    { If true, lines will be drawn between points with no or null data. If
      false, points with NaN data will create a break in the line. }
    spanGaps: Boolean;
    { If the line is shown as a stepped line. }
    steppedLine: Boolean;
    steppedLine_: string; external name 'steppedLine';
    { Array data passed to the chart. }
    data: array of Integer;
    datas: array of TChartXYData; external name 'data';
    data_: TJSArray; external name 'data';
  end;

  { Line options class. }
  TChartLineOptions = class external name 'Object' (TJSObject)
  public
    { If false, the lines between points are not drawn. }
    showLines: Boolean;
    { If false, NaN data causes a break in the line. }
    spanGaps: Boolean;
  end;

  { Bar dataset class. }
  TChartBarDataset = class external name 'Object' (TChartDataset)
  public
    { The ID of the x axis to plot this dataset on. If not specified, this
      defaults to the ID of the first found x axis. }
    xAxisID: string;
    { The ID of the y axis to plot this dataset on. If not specified, this
      defaults to the ID of the first found y axis. }
    yAxisID: string;
    { The fill color of the bar. }
    backgroundColor: string;
    backgroundColors: TJSStringDynArray; external name 'backgroundColor';
    backgroundColor_: TJSArray; external name 'backgroundColor';
    { The color of the bar border. }
    borderColor: string;
    borderColors: TJSStringDynArray; external name 'borderColor';
    borderColor_: TJSArray; external name 'borderColor';
    { The stroke width of the bar in pixels. }
    borderWidth: NativeUInt;
    borderWidths: array of NativeUInt; external name 'borderWidth';
    borderWidth_: TJSArray; external name 'borderWidth';
    { Which edge to skip drawing the border for. }
    borderSkipped: string;
    { The fill colour of the bars when hovered. }
    hoverBackgroundColor: string;
    hoverBackgroundColors: TJSStringDynArray; external name 'hoverBackgroundColor';
    hoverBackgroundColor_: TJSArray; external name 'hoverBackgroundColor';
    { The stroke colour of the bars when hovered. }
    hoverBorderColor: string;
    hoverBorderColors: TJSStringDynArray; external name 'hoverBorderColor';
    hoverBorderColor_: TJSArray; external name 'hoverBorderColor';
    { The stroke width of the bars when hovered. }
    hoverBorderWidth: NativeUInt;
    hoverBorderWidths: array of NativeUInt; external name 'hoverBorderWidth';
    hoverBorderWidth_: TJSArray; external name 'hoverBorderWidth';
    { The ID of the group to which this dataset belongs to (when stacked, each
      group will be a separate stack). }
    stack: string;
    { Array data passed to the chart. }
    data: array of Integer;
    datas: array of TChartXYData; external name 'data';
    data_: TJSArray; external name 'data';
  end;

  { Bar options class. }
  TChartBarOptions = class external name 'Object' (TJSObject)
  public
    { Percent (0-1) of the available width each bar should be within the
      category width. 1.0 will take the whole category width and put the bars
      right next to each other. }
    barPercentage: Double;
    { Percent (0-1) of the available width each category should be within the
      sample width. }
    categoryPercentage: Double;
    { Manually set width of each bar in pixels. If set to 'flex', it computes
      "optimal" sample widths that globally arrange bars side by side. If not
      set (default), bars are equally sized based on the smallest interval. }
    barThickness: JSValue;
    { Set this to ensure that bars are not sized thicker than this. }
    maxBarThickness: NativeInt;
    {
    gridLines.offsetGridLines:
      If true, the bars for a particular data point fall between the grid lines.
      The grid line will move to the left by one half of the tick interval. If
      false, the grid line will go right down the middle of the bars. }
    gridLines: TChartScaleGridLine;
  end;

  { Radar dataset class. }
  TChartRadarDataset = class external name 'Object' (TChartDataset)
  public
    { The fill color under the line. }
    backgroundColor: string;
    backgroundColor_: JSValue; external name 'backgroundColor';
    { The color of the line. }
    borderColor: string;
    borderColor_: JSValue; external name 'borderColor';
    { The width of the line in pixels. }
    borderWidth: NativeUInt;
    borderWidth_: JSValue; external name 'borderWidth';
    { Length and spacing of dashes. }
    borderDash: array of NativeInt;
    borderDash_: TJSArray; external name 'borderDash';
    { Offset for line dashes. }
    borderDashOffset: NativeInt;
    { Cap style of the line. }
    borderCapStyle: string;
    { Line joint style. }
    borderJoinStyle: string;
    { How to fill the area under the line. }
    fill: JSValue;
    { Bezier curve tension of the line. Set to 0 to draw straightlines. }
    lineTension: NativeInt;
    { The fill color for points. }
    pointBackgroundColor: string;
    pointBackgroundColors: TJSStringDynArray; external name 'pointBackgroundColor';
    pointBackgroundColor_: TJSArray; external name 'pointBackgroundColor';
    { The border color for points. }
    pointBorderColor: string;
    pointBorderColors: TJSStringDynArray; external name 'pointBorderColor';
    pointBorderColor_: TJSArray; external name 'pointBorderColor';
    { The width of the point border in pixels. }
    pointBorderWidth: NativeUInt;
    pointBorderWidths: array of NativeUInt; external name 'pointBorderWidth';
    pointBorderWidth_: TJSArray; external name 'pointBorderWidth';
    { The radius of the point shape. If set to 0, the point is not rendered. }
    pointRadius: NativeInt;
    pointRadiuses: array of NativeInt; external name 'pointRadius';
    pointRadius_: TJSArray; external name 'pointRadius';
    { The rotation of the point in degrees. }
    pointRotation: NativeInt;
    pointRotations: array of NativeInt; external name 'pointRotation';
    pointRotation_: TJSArray; external name 'pointRotation';
    { Style of the point. }
    pointStyle: string;
    pointStyles: TJSStringDynArray; external name 'pointStyle';
    pointStyle_: TJSArray; external name 'pointStyle';
    pointStyleImage: TJSObject; external name 'pointStyle';
    pointStyleImage_: TJSObjectDynArray; external name 'pointStyle';
    { The pixel size of the non-displayed point that reacts to mouse events. }
    pointHitRadius: NativeInt;
    pointHitRadiuses: array of NativeInt; external name 'pointHitRadius';
    pointHitRadius_: TJSArray; external name 'pointHitRadius';
    { Point background color when hovered. }
    pointHoverBackgroundColor: string;
    pointHoverBackgroundColors: TJSStringDynArray; external name 'pointHoverBackgroundColor';
    pointHoverBackgroundColor_: TJSArray; external name 'pointHoverBackgroundColor';
    { Point border color when hovered. }
    pointHoverBorderColor: string;
    pointHoverBorderColors: TJSStringDynArray; external name 'pointHoverBorderColor';
    pointHoverBorderColor_: TJSArray; external name 'pointHoverBorderColor';
    { Border width of point when hovered. }
    pointHoverBorderWidth: NativeUInt;
    pointHoverBorderWidths: array of NativeUInt; external name 'pointHoverBorderWidth';
    pointHoverBorderWidth_: TJSArray; external name 'pointHoverBorderWidth';
    { The radius of the point when hovered. }
    pointHoverRadius: NativeInt;
    pointHoverRadiuses: array of NativeInt; external name 'pointHoverRadius';
    pointHoverRadius_: TJSArray; external name 'pointHoverRadius';
    { Array data passed to the chart. }
    data: array of Integer;
    datas: array of TChartXYData; external name 'data';
    data_: TJSArray; external name 'data';
  end;

  { Unlike other charts, the radar chart has no chart specific options. }

  { Doughnut dataset class. }
  TChartDoughnutDataset = class external name 'Object' (TChartDataset)
  public
    { The fill color of the arcs in the dataset. }
    backgroundColor: TJSStringDynArray;
    backgroundColor_: TJSArray; external name 'backgroundColor';
    { The border color of the arcs in the dataset. }
    borderColor: TJSStringDynArray;
    borderColor_: TJSArray; external name 'borderColor';
    { The border width of the arcs in the dataset. }
    borderWidth: array of NativeUInt;
    borderWidth_: TJSArray; external name 'borderWidth';
    { The fill colour of the arcs when hovered. }
    hoverBackgroundColor: TJSStringDynArray;
    hoverBackgroundColor_: TJSArray; external name 'hoverBackgroundColor';
    { The stroke colour of the arcs when hovered. }
    hoverBorderColor: TJSStringDynArray;
    hoverBorderColor_: TJSArray; external name 'hoverBorderColor';
    { The stroke width of the arcs when hovered. }
    hoverBorderWidth: array of NativeUInt;
    hoverBorderWidth_: TJSArray; external name 'hoverBorderWidth';
    { Array data passed to the chart. }
    data: array of Integer;
    data_: TJSArray; external name 'data';
  end;

  { Doughnut options class. }
  TChartDoughnutOptions = class external name 'Object' (TJSObject)
  public
    { The percentage of the chart that is cut out of the middle. }
    cutoutPercentage: NativeInt;
    { Starting angle to draw arcs from. }
    rotation: Double;
    { Sweep to allow arcs to cover. }
    circumference: Double;
    {
    animation.animateRotate:
      If true, the chart will animate in with a rotation animation. This
      property is in the options.animation object.
    animation.animateScale:
      If true, will animate scaling the chart from the center outwards. }
    animation: TChartAnimationConfiguration;
  end;

  { Pie dataset class. }
  TChartPieDataset = class external name 'Object' (TChartDoughnutDataset)
  end;

  { Pie options class. }
  TChartPieOptions = class external name 'Object' (TChartDoughnutOptions)
  end;

  { Polar area dataset class. }
  TChartPolarAreaDataset = class external name 'Object' (TChartDataset)
  public
    { The fill color of the arcs in the dataset. }
    backgroundColor: TJSStringDynArray;
    backgroundColor_: TJSArray; external name 'backgroundColor';
    { The border color of the arcs in the dataset. }
    borderColor: TJSStringDynArray;
    borderColor_: TJSArray; external name 'borderColor';
    { The border width of the arcs in the dataset. }
    borderWidth: array of NativeUInt;
    borderWidth_: TJSArray; external name 'borderWidth';
    { The fill colour of the arcs when hovered. }
    hoverBackgroundColor: TJSStringDynArray;
    hoverBackgroundColor_: TJSArray; external name 'hoverBackgroundColor';
    { The stroke colour of the arcs when hovered. }
    hoverBorderColor: TJSStringDynArray;
    hoverBorderColor_: TJSArray; external name 'hoverBorderColor';
    { The stroke width of the arcs when hovered. }
    hoverBorderWidth: array of NativeUInt;
    hoverBorderWidth_: TJSArray; external name 'hoverBorderWidth';
    { Array data passed to the chart. }
    data: array of Integer;
    data_: TJSArray; external name 'data';
  end;

  { Polar area options class. }
  TChartPolarAreaOptions = class external name 'Object' (TJSObject)
  public
    { Starting angle to draw arcs for the first item in a dataset. }
    startAngle: Double;
    {
    animation.animateRotate:
      If true, the chart will animate in with a rotation animation. This
      property is in the options.animation object.
    animation.animateScale:
      If true, will animate scaling the chart from the center outwards. }
    animation: TChartAnimationConfiguration;
  end;

  { Bubble dataset class. }
  TChartBubbleDataset = class external name 'Object' (TChartDataset)
  public
    { Bubble background color. }
    backgroundColor: string;
    backgroundColor_: JSValue; external name 'backgroundColor';
    { Bubble border color. }
    borderColor: string;
    borderColor_: JSValue; external name 'borderColor';
    { Bubble border width (in pixels). }
    borderWidth: NativeUInt;
    borderWidth_: JSValue; external name 'borderWidth';
    { Bubble background color when hovered. }
    hoverBackgroundColor: string;
    hoverBackgroundColor_: JSValue; external name 'hoverBackgroundColor';
    { Bubble border color hovered. }
    hoverBorderColor: string;
    hoverBorderColor_: JSValue; external name 'hoverBorderColor';
    { Bubble border width when hovered (in pixels). }
    hoverBorderWidth: NativeUInt;
    hoverBorderWidth_: JSValue; external name 'hoverBorderWidth';
    { Bubble additional radius when hovered (in pixels). }
    hoverRadius: NativeInt;
    { Bubble additional radius for hit detection (in pixels). }
    hitRadius: NativeInt;
    { Bubble shape style. }
    pointStyle: string;
    { Bubble rotation (in degrees). }
    rotation: NativeInt;
    { Bubble radius (in pixels). }
    radius: NativeInt;
    radius_: JSValue; external name 'radius';
    { Array data passed to the chart. }
    data: array of TChartXYRData;
    data_: TJSArray; external name 'data';
  end;

  { Bubble options class. }
  TChartBubbleOptions = class(TChartOptions)
  end;

  { Scatter dataset class. }
  TChartScatterDataset = class external name 'Object' (TChartLineDataset)
  end;

  { Scatter options class. }
  TChartScatterOptions = class external name 'Object' (TChartOptions)
  end;

  { Default properties class. }
  TChartDefaults = class external name 'Object' (TJSObject)
  public
    { Global properties of the chart. }
    global: TChartOptions;
    { Default line options. }
    line: TChartLineOptions;
    { Default bar options. }
    bar: TChartBarOptions;
    { Default doughnut options. }
    doughnut: TChartDoughnutOptions;
    { Default pie options. }
    pie: TChartPieOptions;
    { Default polar area options. }
    polarArea: TChartPolarAreaOptions;
    { Default bubble options. }
    bubble: TChartBubbleOptions;
    { Default scatter options. }
    scatter: TChartScatterOptions;
  end;

  { ChartJS class. }
  TChart = class external name 'Chart' (TJSObject)
  public class var
    { Default properties of the chart. }
    defaults: TChartDefaults;
    { Default configuration for the scales. }
    scaleService: TCharScaleService;
    { Plugins object. }
    plugins: TChartPlugins;
  public
    { Configuration object. }
    config: TChartConfiguration;
    { Creates a new instante of TChart. }
    constructor new(item: JSValue); overload;
    { Creates a new instante of TChart. }
    constructor new(item: JSValue; const config: JSValue); overload;
    { Destroys any chart instances that are created. }
    procedure destroy;
    { Triggers an update of the chart. }
    procedure update(const config: JSValue); overload;
    procedure update; overload;
    { Reset the chart to it's state before the initial animation. }
    procedure reset;
    { Triggers a redraw of all chart elements. }
    procedure render(const config: JSValue); overload;
    procedure render; overload;
    { Stops any current animation loop. }
    function stop: TChart;
    { Resizes the canvas element. }
    function resize: TChart;
    { Clears the chart canvas. }
    function clear: TChart;
    { This returns a base 64 encoded string of the chart in it's current state. }
    function toBase64Image: string;
    { Returns a HTML string of a legend for that chart. }
    function generateLegend: string;
    { Returns the single element at the event position. }
    function getElementAtEvent(ev: TJSEvent): TJSArray;
    { Returns all elements at the same data index. }
    function getElementsAtEvent(ev: TJSEvent): TJSArray;
    { Looks for the element under the event point, then returns all elements
      from that dataset. }
    function getDatasetAtEvent(ev: TJSEvent): TJSArray;
    { Looks for the dataset that matches the current index and returns that
      metadata. }
    function getDatasetMeta(index: Integer): JSValue;
  end;

  { Controller class. }
  TChartController = class external name 'Chart' (TChart)
  public
    id: JSValue;
    ctx: TJSCanvasRenderingContext2D;
    canvas: TJSHTMLCanvasElement;
    width: NativeUInt;
    height:  NativeUInt;
    aspectRatio: NativeInt;
    options: TChartOptions;
    controller: TChart;
    data: TChartData;
    scales: TJSObject;
    lastActive: TJSArray;
    active: TJSArray;
    animating: Boolean;
    boxes: TJSArray;
    chartArea: TChartPadding;
    _bufferedRender: Boolean;
    _bufferedRequest: TJSObject;
  end;

implementation

{$PUSH}{$WARN 5027 OFF}

constructor TChartPluginsHook.new;
var
  dummy: Pointer;
begin
  dummy := @beforeInit;
  dummy := @afterInit;
  dummy := @beforeUpdate;
  dummy := @afterUpdate;
  dummy := @beforeDatasetsUpdate;
  dummy := @afterDatasetsUpdate;
  dummy := @beforeDatasetUpdate;
  dummy := @afterDatasetUpdate;
  dummy := @beforeLayout;
  dummy := @afterLayout;
  dummy := @beforeRender;
  dummy := @afterRender;
  dummy := @beforeDraw;
  dummy := @afterDraw;
  dummy := @beforeDatasetsDraw;
  dummy := @afterDatasetsDraw;
  dummy := @beforeDatasetDraw;
  dummy := @afterDatasetDraw;
  dummy := @beforeTooltipDraw;
  dummy := @afterTooltipDraw;
  dummy := @beforeEvent;
  dummy := @afterEvent;
  dummy := @resize;
  dummy := @destroy;
end;

{$POP}

procedure TChartPluginsHook.beforeInit(chart: TChartController;
  options: TChartOptions);
begin
end;

procedure TChartPluginsHook.afterInit(chart: TChartController;
  options: TChartOptions);
begin
end;

function TChartPluginsHook.beforeUpdate(chart: TChartController;
  options: TChartOptions): Boolean;
begin
  Result := True;
end;

procedure TChartPluginsHook.afterUpdate(chart: TChartController;
  options: TChartOptions);
begin
end;

function TChartPluginsHook.beforeDatasetsUpdate(chart: TChartController;
  options: TChartOptions): Boolean;
begin
  Result := True;
end;

procedure TChartPluginsHook.afterDatasetsUpdate(chart: TChartController;
  options: TChartOptions);
begin
end;

function TChartPluginsHook.beforeDatasetUpdate(chart: TChartController;
  const args: TChartPluginsHookArgument; options: TChartOptions): Boolean;
begin
  Result := True;
end;

procedure TChartPluginsHook.afterDatasetUpdate(chart: TChartController;
  const args: TChartPluginsHookArgument; options: TChartOptions);
begin
end;

function TChartPluginsHook.beforeLayout(chart: TChartController;
  options: TChartOptions): Boolean;
begin
  Result := True;
end;

procedure TChartPluginsHook.afterLayout(chart: TChartController;
  options: TChartOptions);
begin
end;

function TChartPluginsHook.beforeRender(chart: TChartController;
  options: TChartOptions): Boolean;
begin
  Result := True;
end;

procedure TChartPluginsHook.afterRender(chart: TChartController;
  options: TChartOptions);
begin
end;

function TChartPluginsHook.beforeDraw(chart: TChartController;
  easingValue: NativeInt; options: TChartOptions): Boolean;
begin
  Result := True;
end;

procedure TChartPluginsHook.afterDraw(chart: TChartController;
  easingValue: NativeInt; options: TChartOptions);
begin
end;

function TChartPluginsHook.beforeDatasetsDraw(chart: TChartController;
  easingValue: NativeInt; options: TChartOptions): Boolean;
begin
  Result := True;
end;

procedure TChartPluginsHook.afterDatasetsDraw(chart: TChartController;
  easingValue: NativeInt; options: TChartOptions);
begin
end;

function TChartPluginsHook.beforeDatasetDraw(chart: TChartController;
  const args: TChartPluginsHookArgument; options: TChartOptions): Boolean;
begin
  Result := True;
end;

procedure TChartPluginsHook.afterDatasetDraw(chart: TChartController;
  const args: TChartPluginsHookArgument; options: TChartOptions);
begin
end;

function TChartPluginsHook.beforeTooltipDraw(chart: TChartController;
  const args: TChartPluginsHookArgument; options: TChartOptions): Boolean;
begin
  Result := True;
end;

procedure TChartPluginsHook.afterTooltipDraw(chart: TChartController;
  const args: TChartPluginsHookArgument; options: TChartOptions);
begin
end;

procedure TChartPluginsHook.beforeEvent(chart: TChartController;
  event: TJSEvent; options: TChartOptions);
begin
end;

procedure TChartPluginsHook.afterEvent(chart: TChartController;
  event: TJSEvent; options: TChartOptions);
begin
end;

procedure TChartPluginsHook.resize(chart: TChartController; size: NativeInt;
  options: TChartOptions);
begin
end;

procedure TChartPluginsHook.destroy(chart: TChartController;
  options: TChartOptions);
begin
end;

{ TChartXYData }

constructor TChartXYData.new(const x, y: JSValue);
begin
  Self.x := x;
  Self.y := y;
end;

constructor TChartXYData.new(const x: JSValue);
begin
  Self.x := x;
  Self.y := 0;
end;

{ TChartTYData }

constructor TChartTYData.new(const t, y: JSValue);
begin
  Self.t := t;
  Self.y := y;
end;

constructor TChartTYData.new(const t: JSValue);
begin
  Self.t := t;
  Self.y := 0;
end;

{ TChartXYRData }

constructor TChartXYRData.new(const x, y, r: JSValue);
begin
  Self.x := x;
  Self.y := y;
  Self.r := r;
end;

constructor TChartXYRData.new(const x, y: JSValue);
begin
  Self.x := x;
  Self.y := y;
  Self.r := 0;
end;

constructor TChartXYRData.new(const x: JSValue);
begin
  Self.x := x;
  Self.y := 0;
  Self.r := 0;
end;

{ TChartXYVData }

constructor TChartXYVData.new(const x, y, v: JSValue);
begin
  Self.x := x;
  Self.y := y;
  Self.v := v;
end;

constructor TChartXYVData.new(const x, y: JSValue);
begin
  Self.x := x;
  Self.y := y;
  Self.v := 0;
end;

constructor TChartXYVData.new(const x: JSValue);
begin
  Self.x := x;
  Self.y := 0;
  Self.v := 0;
end;

end.
