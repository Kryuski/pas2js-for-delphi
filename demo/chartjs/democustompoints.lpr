program democustompoints;

{$MODE OBJFPC}
{$MODESWITCH EXTERNALCLASS}

uses
  JS,
  Web,
  SysUtils,
  Math,
  libjquery,
  ChartJS;

function randomScalingFactor: integer;
begin
  Result := RandomRange(-100, 100);
end;

var
  config: TChartConfiguration;
  lineChartData: TChartData;
  dataset: TChartLineDataset;
  customTooltips: TChartTooltipsCustomEventHandler;
begin
  customTooltips :=
    procedure(const tooltip: TChartTooltipModel)
    var
      canvas: TJSHTMLCanvasElement;
    begin
      canvas := TChartController(JSThis['_chart']).canvas;
      JQuery(canvas).css('cursor', 'pointer');
      JQuery('.chartjs-tooltip').css('opacity', 0);
      if ((not Assigned(tooltip)) or (not Assigned(tooltip.dataPoints))) and
        (tooltip.opacity = 0) then
        Exit;
      if tooltip.dataPoints_.Length > 0 then
        tooltip.dataPoints_.forEach(
          function(element: JSValue; index: NativeInt; arr: TJSArray): Boolean
          var
            tooltp: TJQuery;
            dataPoint: TChartTooltipItem;
            content: string;
          begin
            dataPoint := TChartTooltipItem(element);
            content := TJSArray.new(dataPoint.xLabel, dataPoint.yLabel).join(': ');
            tooltp := JQuery('#tooltip-' + IntToStr(dataPoint.datasetIndex));
            tooltp.html(content);
            tooltp.css('opacity', 1);
            tooltp.css('top', FloatToStr(canvas.offsetTop + dataPoint.y) + 'px');
            tooltp.css('left', FloatToStr(canvas.offsetLeft + dataPoint.x) + 'px');
          end);
    end;
  lineChartData := TChartData.new;
  lineChartData.datasets_ := TJSArray.new;
  lineChartData.labels := ['January', 'February', 'March', 'April', 'May',
    'June', 'July'];
  dataset := TChartLineDataset.new;
  dataset.label_ := 'My First dataset';
  dataset.backgroundColor := 'rgba(255, 99, 132, 0.2)';
  dataset.borderColor := 'rgb(255, 99, 132)';
  dataset.pointBackgroundColor := 'rgb(255, 99, 132)';
  dataset.data := [randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor];
  lineChartData.datasets_.push(dataset);
  dataset := TChartLineDataset.new;
  dataset.label_ := 'My Second dataset';
  dataset.backgroundColor := 'rgba(54, 162, 235, 0.2)';
  dataset.borderColor := 'rgb(54, 162, 235)';
  dataset.pointBackgroundColor := 'rgb(54, 162, 235)';
  dataset.data := [randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor];
  lineChartData.datasets_.push(dataset);
  config := TChartConfiguration.new;
  config.type_ := 'line';
  config.data := lineChartData;
  config.options := TChartOptions.new;
  config.options.title := TChartTitleConfiguration.new;
  config.options.title.display := False;
  config.options.title.text := 'Chart.js - Custom Tooltips using Data Points';
  config.options.tooltips := TChartTooltipsConfiguration.new;
  config.options.tooltips.enabled := False;
  config.options.tooltips.mode := 'index';
  config.options.tooltips.intersect := False;
  config.options.tooltips.custom := customTooltips;
  TChart.new('chart1', config);
end.
