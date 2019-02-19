program demoscriptablebubble;

{$MODE OBJFPC}
{$MODESWITCH EXTERNALCLASS}

uses
  JS,
  Web,
  SysUtils,
  Math,
  ChartJS;

const
  COLORS: array[0..8] of string = ('#4dc9f6', '#f67019', '#f53794', '#537bc4',
    '#acc236', '#166a8f', '#00a950', '#58595b', '#8549ba');

function rand(min, max: Integer): Integer;
begin
  Result := RandomRange(min, max);
end;

function ftos(const f: Double): string;
begin
  Result := FloatToStrF(f, ffNumber, 15, 1);
end;

const
  DATA_COUNT = 16;
  MIN_XY = -150;
  MAX_XY = 100;

function generateData: TJSArray;
var
  i: Integer;
begin
  Result := TJSArray.new;
  for i := 0 to Pred(DATA_COUNT) do
  begin
    Result.push(TChartXYVData.new(rand(MIN_XY, MAX_XY), rand(MIN_XY, MAX_XY),
      rand(0, 1000)));
  end;
end;

function colorize(opaque: Boolean; context: TChartOptionsContext): JSValue;
var
  value: TChartXYVData;
  x, y, r, g, b, a: Double;
begin
  value := TChartXYVData(
    TChartBubbleDataset(context.dataset).data_[context.dataIndex]);
  x := value.x_ / 100;
  y := value.y_ / 100;
  r := IfThen((x < 0) and (y < 0), 250, IfThen(x < 0, 150, IfThen(y < 0, 50, 0)));
  g := IfThen((x < 0) and (y < 0), IfThen(x < 0, 50, IfThen(y < 0, 150, 250)));
  b := IfThen((x < 0) and (y < 0), 0, IfThen((x > 0) and (y > 0), 250, 150));
  a := IfThen(opaque, 1, 0.5 * value.v_ / 1000);
  Result := 'rgba(' + ftos(r) + ',' + ftos(g) + ',' + ftos(b) + ',' +
    ftos(a) + ')';
end;

function color(index: NativeInt): string;
begin
  Result := COLORS[index mod Length(COLORS)];
end;

var
  chart: TChart;
  config: TChartConfiguration;
  options: TChartOptions;
  data: TChartData;
  dataset: TChartBubbleDataset;
begin
  data := TChartData.new;
  data.datasets_ := TJSArray.new;
  dataset := TChartBubbleDataset.new;
  dataset.data_ := generateData;
  data.datasets_.push(dataset);
  dataset := TChartBubbleDataset.new;
  dataset.data_ := generateData;
  data.datasets_.push(dataset);

  options := TChartOptions.new;
  options.aspectRatio := 1;
  options.legend_ := False;
  options.tooltips_ := False;
  options.elements := TChartElementsConfiguration.new;
  options.elements.point := TChartElementPoint.new;
  options.elements.point.backgroundColor_ := TJSFunction(@colorize).bind(nil, False);
  options.elements.point.borderColor_ := TJSFunction(@colorize).bind(nil, True);
  options.elements.point.borderWidth_ :=
    function(context: TChartOptionsContext): JSValue
    begin
      Result := Min(Max(1, context.datasetIndex + 1), 8);
    end;
  options.elements.point.hoverBackgroundColor := 'transparent';
  options.elements.point.hoverBorderColor_ :=
    function(context: TChartOptionsContext): JSValue
    begin
      Result := color(context.datasetIndex);
    end;
  options.elements.point.hoverBorderWidth_ :=
    function(context: TChartOptionsContext): JSValue
    var
      value: TChartXYVData;
    begin
      value := TChartXYVData(
        TChartBubbleDataset(context.dataset).data_[context.dataIndex]);
      Result := Round(8 * value.v_ / 1000);
    end;
  options.elements.point.radius_ :=
    function(context: TChartOptionsContext): JSValue
    var
      value: TChartXYVData;
      size: NativeUInt;
      base: Double;
    begin
      value := TChartXYVData(TChartBubbleDataset(
        context.dataset).data_[context.dataIndex]);
      size := context.chart.width;
      base := Abs(value.v_) / 1000;
      Result := (size / 24) * base;
    end;
  config := TChartConfiguration.new;
  config.type_ := 'bubble';
  config.data := data;
  config.options := options;
  chart := TChart.new('chart-0', config);

  document.getElementById('randomize').addEventListener('click',
    procedure
    begin
      chart.config.data.datasets_.forEach(
        function(element: JSValue; index: NativeInt; arr: TJSArray): Boolean
        begin
          TChartBubbleDataset(element).data_ := generateData;
        end);
      chart.update;
    end);
  document.getElementById('addDataset').addEventListener('click',
    procedure
    var
      dataset: TChartBubbleDataset;
    begin
      dataset := TChartBubbleDataset.new;
      dataset.data_ := generateData;
      chart.config.data.datasets_.push(dataset);
      chart.update;
    end);
  document.getElementById('removeDataset').addEventListener('click',
    procedure
    begin
      chart.config.data.datasets_.shift;
      chart.update;
    end);
end.
