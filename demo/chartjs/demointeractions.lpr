program demointeractions;

{$MODE OBJFPC}
{$MODESWITCH EXTERNALCLASS}

uses
  JS,
  Web,
  SysUtils,
  ChartJS;

function createConfig(const mode: string; intersect: Boolean): TChartConfiguration;
var
  dataset: TChartLineDataset;
begin
  Result := TChartConfiguration.new;
  Result.type_ := 'line';
  Result.data := TChartData.new;
  Result.data.labels := ['January', 'February', 'March', 'April', 'May',
    'June', 'July'];
  Result.data.datasets_ := TJSArray.new;
  dataset := TChartLineDataset.new;
  dataset.label_ := 'My First dataset';
  dataset.borderColor := 'rgb(255, 99, 132)';
  dataset.backgroundColor := 'rgb(255, 99, 132)';
  dataset.data := [10, 30, 46, 2, 8, 50, 0];
  dataset.fill := False;
  Result.data.datasets_.push(dataset);
  dataset := TChartLineDataset.new;
  dataset.label_ := 'My Second dataset';
  dataset.borderColor := 'rgb(54, 162, 235)';
  dataset.backgroundColor := 'rgb(54, 162, 235)';
  dataset.data := [7, 49, 46, 13, 25, 30, 22];
  dataset.fill := False;
  Result.data.datasets_.push(dataset);
  Result.options := TChartOptions.new;
  Result.options.responsive := True;
  Result.options.title := TChartTitleConfiguration.new;
  Result.options.title.display := True;
  Result.options.title.text := 'Mode: ' + mode + ', intersect = ' +
    BoolToStr(intersect, 'true', 'false');
  Result.options.tooltips := TChartTooltipsConfiguration.new;
  Result.options.tooltips.mode := mode;
  Result.options.tooltips.intersect := intersect;
  Result.options.hover := TChartHover.new;
  Result.options.hover.mode := mode;
  Result.options.hover.intersect := intersect;
end;

var
  container, div_: TJSElement;
  canvas: TJSHTMLCanvasElement;
  hover: TChartHover;
  hovers: TJSArray;
  config: TChartConfiguration;
  ctx: TJSObject;
begin
  container := document.querySelector('.demoarea');
  hovers := TJSArray.new;
  hover := TChartHover.new;
  hover.mode := 'index';
  hover.intersect := true;
  hovers.push(hover);
  hover := TChartHover.new;
  hover.mode := 'index';
  hover.intersect := False;
  hovers.push(hover);

  hover.mode := 'dataset';
  hover.intersect := True;
  hovers.push(hover);
  hover.mode := 'dataset';
  hover.intersect := False;
  hovers.push(hover);
  hover.mode := 'point';
  hover.intersect := True;
  hovers.push(hover);
  hover.mode := 'point';
  hover.intersect := False;
  hovers.push(hover);
  hover.mode := 'nearest';
  hover.intersect := True;
  hovers.push(hover);
  hover.mode := 'nearest';
  hover.intersect := False;
  hovers.push(hover);
  hover.mode := 'x';
  hover.intersect := True;
  hovers.push(hover);
  hover.mode := 'x';
  hover.intersect := False;
  hovers.push(hover);
  hover.mode := 'y';
  hover.intersect := True;
  hovers.push(hover);
  hover.mode := 'y';
  hover.intersect := False;
  hovers.forEach(
    function(element: JSValue; index: NativeInt; arr: TJSArray): Boolean
    var
      details: TChartHover;
    begin
      div_ := document.createElement('div');
      div_.classList.add('chart-container');
      canvas := TJSHTMLCanvasElement(document.createElement('canvas'));
      div_.appendChild(canvas);
      container.appendChild(div_);
      ctx := canvas.getContext('2d');
      details := TChartHover(element);
      config := createConfig(details.mode, details.intersect);
      TChart.new(ctx, config);
    end);
end.
