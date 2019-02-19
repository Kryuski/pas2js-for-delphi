program demomixed;

{$MODE OBJFPC}
{$MODESWITCH EXTERNALCLASS}

uses
  ChartJS;

var
  config: TChartConfiguration;
  dataset1: TChartBarDataset;
  dataset2: TChartLineDataset;
begin
  config := TChartConfiguration.new;
  config.type_ := 'bar';
  config.data := TChartData.new;
  config.data.labels := ['January', 'February', 'March', 'April'];

  dataset1 := TChartBarDataset.new;
  dataset1.label_ := 'Bar Dataset';
  dataset1.data := [10, 20, 30, 40];
  dataset1.borderColor := 'rgb(255, 99, 132)';
  dataset1.backgroundColor := 'rgba(255, 99, 132, 0.2)';

  dataset2 := TChartLineDataset.new;
  dataset2.label_ := 'Line Dataset';
  dataset2.data := [50, 50, 50, 50];
  dataset2.type_ := 'line';
  dataset2.fill := False;
  dataset2.borderColor := 'rgb(54, 162, 235)';

  config.data.datasets := [dataset1, dataset2];
  TChart.new('myChart', config);
end.
