program demodatalabelling;

{$MODE OBJFPC}
{$MODESWITCH EXTERNALCLASS}
{$WARN 4501 OFF}

uses
  JS,
  Web,
  SysUtils,
  Math,
  ChartJS;

type
  TMyPlugin = class(TChartPluginsHook)
  public
    procedure afterDatasetsDraw(chart: TChartController;
      easingValue: NativeInt; options: TChartOptions); override;
  end;

procedure TMyPlugin.afterDatasetsDraw(chart: TChartController;
  easingValue: NativeInt; options: TChartOptions);
begin
  chart.data.datasets_.forEach(
    function (el: JSValue; i: NativeInt; arr: TJSArray): Boolean
    type
      TTooltipPositionFunc = reference to function: TChartTooltipItem;
    var
      ctx: TJSCanvasRenderingContext2D;
      dataset, meta: TChartBarDataset;
      position: TChartTooltipItem;
      fontSize: NativeUInt;
    begin
      meta := TChartBarDataset(chart.getDatasetMeta(i));
      if not meta.hidden then
      begin
        meta.data_.forEach(
          function (element: JSValue; index: NativeInt; arr: TJSArray): Boolean
          begin
            ctx := chart.ctx;
            ctx.fillStyle := 'rgb(0, 0, 0)';
            fontSize := 16;
            asm
              ctx.font = Chart.helpers.fontString(fontSize, 'normal',
                'Helvetica Neue');
            end;
            ctx.textAlign := 'center';
            ctx.textBaseline := 'middle';
            position := TTooltipPositionFunc(TJSObject(element)['tooltipPosition'])();
            dataset := TChartBarDataset(el);
            ctx.fillText(IntToStr(dataset.data[index]),
              position.x, position.y - (fontSize / 2) - 5);
            Result := True;
          end);
      end;
      Result := True;
    end);
end;

function randomScalingFactor: NativeUInt;
begin
  Result := RandomRange(-100, 100);
end;

var
  myBar: TChart;
  config: TChartConfiguration;
  barChartData: TChartData;
  dataset: TChartBarDataset;
begin
  barChartData := TChartData.new;
  barChartData.datasets_ := TJSArray.new;
  barChartData.labels := ['January', 'February', 'March', 'April', 'May',
    'June', 'July'];

  dataset := TChartBarDataset.new;
  dataset.type_ := 'bar';
  dataset.label_ := 'Dataset 1';
  dataset.backgroundColor := 'rgba(255, 99, 132, 0.2)';
  dataset.borderColor := 'rgb(255, 99, 132)';
  dataset.data := [randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor];
  barChartData.datasets_.push(dataset);

  dataset := TChartBarDataset.new;
  dataset.type_ := 'line';
  dataset.label_ := 'Dataset 2';
  dataset.backgroundColor := 'rgba(54, 162, 235, 0.2)';
  dataset.borderColor := 'rgb(54, 162, 235)';
  dataset.data := [randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor];
  barChartData.datasets_.push(dataset);

  dataset := TChartBarDataset.new;
  dataset.type_ := 'bar';
  dataset.label_ := 'Dataset 3';
  dataset.backgroundColor := 'rgba(75, 192, 192, 0.2)';
  dataset.borderColor := 'rgb(75, 192, 192)';
  dataset.data := [randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor];
  barChartData.datasets_.push(dataset);

  config := TChartConfiguration.new;
  config.type_ := 'bar';
  config.data := barChartData;
  config.options := TChartOptions.new;
  config.options.responsive := True;
  config.options.title := TChartTitleConfiguration.new;
  config.options.title.display := True;
  config.options.title.text := 'Chart.js Combo Bar Line Chart';

  TChart.plugins.register(TMyPlugin.new);

  myBar := TChart.new('canvas', config);

  document.getElementById('randomizeData').addEventListener('click',
    procedure
    begin
      barChartData.datasets_.forEach(
        function (element: JSValue; index: NativeInt; arr: TJSArray): Boolean
        var
          dataset: TChartBarDataset;
        begin
          dataset := TChartBarDataset(element);
          dataset.data_ := dataset.data_.map(
            function (el: JSValue; i: NativeInt; a: TJSArray): JSValue
            begin
              Result := randomScalingFactor;
            end);
        end);
      myBar.update;
    end);
end.
