program demoarea;

{$MODE OBJFPC}
{$MODESWITCH EXTERNALCLASS}

uses
  JS,
  Math,
  ChartJS;

function randomScalingFactor: NativeUInt;
begin
  Result := RandomRange(-100, 100);
end;

var
  config: TChartConfiguration;
  dataset: TChartLineDataset;
begin
  config := TChartConfiguration.new;
  config.type_ := 'line';
  config.data := TChartData.new;
  config.data.labels := ['January', 'February', 'March', 'April', 'May',
    'June', 'July'];

  dataset := TChartLineDataset.new;
  dataset.label_ := 'My First dataset';
  dataset.borderColor := 'rgb(255, 99, 132)';
  dataset.backgroundColor := 'rgb(255, 99, 132)';
  dataset.data := [randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor];
  config.data.datasets_ := TJSArray.new;
  config.data.datasets_.push(dataset);

  dataset := TChartLineDataset.new;
  dataset.label_ := 'My Second dataset';
  dataset.borderColor := 'rgb(54, 162, 235)';
  dataset.backgroundColor := 'rgb(54, 162, 235)';
  dataset.data := [randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor];
  config.data.datasets_.push(dataset);

  dataset := TChartLineDataset.new;
  dataset.label_ := 'My Third dataset';
  dataset.borderColor := 'rgb(75, 192, 192)';
  dataset.backgroundColor := 'rgb(75, 192, 192)';
  dataset.data := [randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor];
  config.data.datasets_.push(dataset);

  dataset := TChartLineDataset.new;
  dataset.label_ := 'My Four dataset';
  dataset.borderColor := 'rgb(255, 205, 86)';
  dataset.backgroundColor := 'rgb(255, 205, 86)';
  dataset.data := [randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor, randomScalingFactor,
    randomScalingFactor, randomScalingFactor];
  config.data.datasets_.push(dataset);

  TChart.new('myChart', config);
end.
