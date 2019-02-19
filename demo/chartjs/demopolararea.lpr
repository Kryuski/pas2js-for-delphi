program demopolararea;

{$MODE OBJFPC}
{$MODESWITCH EXTERNALCLASS}

uses
  ChartJS;

var
  config: TChartConfiguration;
  dataset: TChartPolarAreaDataset;
begin
  config := TChartConfiguration.new;
  config.type_ := 'polarArea';
  config.data := TChartData.new;
  config.data.labels := ['Red', 'Green', 'Yellow', 'Grey', 'Blue'];
  dataset := TChartPolarAreaDataset.new;
  dataset.data := [11, 16, 7, 3, 14];
  dataset.backgroundColor := ['rgb(255, 99, 132)', 'rgb(75, 192, 192)',
    'rgb(255, 205, 86)', 'rgb(201, 203, 207)', 'rgb(54, 162, 235)'];
  config.data.datasets := [dataset];
  TChart.new('myChart', config);
end.
