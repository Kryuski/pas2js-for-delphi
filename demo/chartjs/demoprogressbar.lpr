program demoprogressbar;

{$MODE OBJFPC}
{$MODESWITCH EXTERNALCLASS}

uses
  JS,
  Web,
  Math,
  ChartJS;

function randomScalingFactor: NativeUInt;
begin
  Result := RandomRange(-100, 100);
end;

var
  progress: TJSHTMLProgressElement;
  myLine: TChart;
  config: TChartConfiguration;
  dataset: TChartLineDataset;
begin
  progress := TJSHTMLProgressElement(document.getElementById('animationProgress'));
  config := TChartConfiguration.new;
  config.data := TChartData.new;
  config.data.datasets_ := TJSArray.new;
  config.type_ := 'line';
  config.data.labels := ['January', 'February', 'March', 'April', 'May',
    'June', 'July'];
  dataset := TChartLineDataset.new;
  dataset.label_ := 'My First dataset';
  dataset.data := [randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor];
  dataset.fill := False;
  dataset.borderColor := 'rgb(255, 99, 132)';
  dataset.backgroundColor := 'rgb(255, 99, 132)';
  config.data.datasets_.push(dataset);
  dataset := TChartLineDataset.new;
  dataset.label_ := 'My Second dataset ';
  dataset.fill := False;
  dataset.borderColor := 'rgb(54, 162, 235)';
  dataset.backgroundColor := 'rgb(54, 162, 235)';
  dataset.data := [randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor];
  config.data.datasets_.push(dataset);
  config.options := TChartOptions.new;
  config.options.title := TChartTitleConfiguration.new;
  config.options.title.display := True;
  config.options.title.text := 'Chart.js Line Chart - Animation Progress Bar';
  config.options.animation := TChartAnimationConfiguration.new;
  config.options.animation.duration := 2000;
  config.options.animation.onProgress :=
    procedure(const animation: TChartAnimationCallback)
    begin
      progress.value := animation.currentStep / animation.numSteps;
    end;
  config.options.animation.onComplete :=
    procedure(const animation: TChartAnimationCallback)
    begin
      window.setTimeout(
        procedure
        begin
          progress.value := 0;
        end, 2000);
    end;
  myLine := TChart.new('canvas', config);
  document.getElementById('randomizeData').addEventListener('click',
    procedure
    begin
      config.data.datasets_.forEach(
        function(el: JSValue; index: NativeInt; arr: TJSArray): Boolean
        var
          dataset: TChartLineDataset;
        begin
          dataset := TChartLineDataset(el);
          dataset.data_ := dataset.data_.map(
            function(el: JSValue; index: NativeInt; arr: TJSArray): JSValue
            begin
              Result := randomScalingFactor;
            end);
          Result := True;
        end);
      myLine.update;
    end);
end.
