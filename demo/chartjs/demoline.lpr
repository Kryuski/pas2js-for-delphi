program demoline;

{$MODE OBJFPC}
{$MODESWITCH EXTERNALCLASS}

uses
  ChartJS;

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
  dataset.data := [65, 59, 80, 81, 56, 55, 40];
  dataset.fill := False;
  dataset.borderColor := 'rgb(75, 192, 192)';
  dataset.lineTension := 0.1;
  config.data.datasets := [dataset];
  TChart.new('myChart', config);
end.
