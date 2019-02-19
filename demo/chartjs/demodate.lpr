program demodate;

{$MODE OBJFPC}
{$MODESWITCH EXTERNALCLASS}

uses
  JS,
  Web,
  Types,
  SysUtils,
  Math,
  ChartJS;

type
  TChartColors = class external name 'Object' (TJSObject)
  public
    red: string;
    orange: string;
    yellow: string;
    green: string;
    blue: string;
    purple: string;
    grey: string;
  end;

const
  timeFormat = 'MM/DD/YYYY HH:mm';

function newDateString(const days: NativeUInt): string; assembler;
asm
  return moment().add(days, 'd').format(pas.program.timeFormat);
end;

function randomScalingFactor: NativeUInt;
begin
  Result := RandomRange(-100, 100);
end;

var
  chart: TChart;
  config: TChartConfiguration;
  barDataset: TChartBarDataset;
  lineDataset: TChartLineDataset;
  axis: TChartScaleCartesianTime;
  chartColors: TChartColors;
  colorNames: TStringDynArray;
begin
  config := TChartConfiguration.new;
  config.data := TChartData.new;
  config.type_ := 'bar';
  config.data.labels := [newDateString(0), newDateString(1), newDateString(2),
   newDateString(3), newDateString(4), newDateString(5), newDateString(6)];

  config.data.datasets_ := TJSArray.new;

  barDataset := TChartBarDataset.new;
  barDataset.type_ := 'bar';
  barDataset.label_ := 'Dataset 1';
  barDataset.backgroundColor := 'rgba(255, 99, 132, 0.5)';
  barDataset.borderColor := 'rgb(255, 99, 132)';
  barDataset.data := [randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor];
  config.data.datasets_.push(barDataset);

  barDataset := TChartBarDataset.new;
  barDataset.type_ := 'bar';
  barDataset.label_ := 'Dataset 2';
  barDataset.backgroundColor := 'rgba(54, 162, 235, 0.5)';
  barDataset.borderColor := 'rgb(54, 162, 235)';
  barDataset.data := [randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor];
  config.data.datasets_.push(barDataset);

  lineDataset := TChartLineDataset.new;
  lineDataset.type_ := 'line';
  lineDataset.label_ := 'Dataset 3';
  lineDataset.backgroundColor := 'rgba(75, 192, 192, 0.5)';
  lineDataset.borderColor := 'rgb(75, 192, 192)';
  lineDataset.fill := False;
  lineDataset.data := [randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor];
  config.data.datasets_.push(lineDataset);

  config.options := TChartOptions.new;
  config.options.title := TChartTitleConfiguration.new;
  config.options.title.text := 'Chart.js Combo Time Scale';

  config.options.scales := TChartScalesConfiguration.new;
  axis := TChartScaleCartesianTime.new;
  axis.type_ := 'time';
  axis.display := True;
  axis.time := TChartMoment.new;
  axis.time.parser := timeFormat;
  //axis.time.round := 'day';
  config.options.scales.xAxes := [axis];

  chart := TChart.new('myChart', config);
  chartColors := TChartColors.new;
  chartColors.red := 'rgb(255, 99, 132)';
  chartColors.orange := 'rgb(255, 159, 64)';
  chartColors.yellow := 'rgb(255, 205, 86)';
  chartColors.green := 'rgb(75, 192, 192)';
  chartColors.blue := 'rgb(54, 162, 235)';
  chartColors.purple := 'rgb(153, 102, 255)';
  chartColors.grey := 'rgb(201, 203, 207)';
  colorNames := TJSObject.keys(chartColors);

  document.getElementById('randomizeData').addEventListener('click',
    procedure
    begin
      config.data.datasets_.forEach(
        function(element: JSValue; index: NativeInt; array_: TJSArray): Boolean
        var
          dataset: TChartBarDataset;
        begin
          dataset := TChartBarDataset(element);
          dataset.data_ := dataset.data_.map(
            function(element: JSValue; index: NativeInt; array_: TJSArray): JSValue
            begin
              Result := randomScalingFactor;
            end);
        end);
      chart.update;
    end);

  document.getElementById('addDataset').addEventListener('click',
    procedure
    var
      newDataset: TChartBarDataset;
      colorName, newColor: string;
      index: NativeUInt;
    begin
      colorName := colorNames[config.data.datasets_.length mod Length(colorNames)];
      newColor := string(chartColors[colorName]);
      newDataset := TChartBarDataset.new;
      newDataset.label_ := 'Dataset ' + IntToStr(config.data.datasets_.length);
      newDataset.borderColor := newColor;
      asm
        newDataset.backgroundColor = Chart.helpers.color(newColor).alpha(0.5).rgbString();
      end;
      newDataset.data_ := TJSArray.new;
      for index := 0 to Pred(config.data.labels_.Length) do
        newDataset.data_.push(randomScalingFactor);
      config.data.datasets_.push(newDataset);
      chart.update;
    end);

  document.getElementById('addData').addEventListener('click',
    procedure
    var
      index: NativeUInt;
    begin
      if config.data.datasets_.length > 0 then
        config.data.labels_.push(newDateString(config.data.labels_.length));
      for index := 0 to Pred(config.data.datasets_.length) do
        TChartBarDataset(config.data.datasets[index]).data_.push(randomScalingFactor);
      chart.update;
    end);

  document.getElementById('removeDataset').addEventListener('click',
    procedure
    begin
      config.data.datasets_.splice(0, 1);
      chart.update;
    end);

  document.getElementById('removeData').addEventListener('click',
    procedure
    begin
      config.data.labels_.splice(-1, 1); // remove the label first
      config.data.datasets_.forEach(
        function(el: JSValue; datasetIndex: NativeInt; arr: TJSArray): Boolean
        begin
          TChartBarDataset(config.data.datasets_[datasetIndex]).data_.pop;
          Result := True;
        end);
      chart.update;
    end);
end.
