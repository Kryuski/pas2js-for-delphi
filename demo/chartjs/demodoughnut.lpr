program demodoughnut;

{$MODE OBJFPC}
{$MODESWITCH EXTERNALCLASS}

uses
  ChartJS;

var
  config: TChartConfiguration;
  dataset: TChartDoughnutDataset;
begin
  config := TChartConfiguration.new;
  config.type_ := 'doughnut';
  config.data := TChartData.new;
  config.data.labels := ['Red', 'Blue', 'Yellow'];
  dataset := TChartDoughnutDataset.new;
  dataset.data := [300, 50, 100];
  dataset.backgroundColor := ['rgb(255, 99, 132)', 'rgb(54, 162, 235)',
    'rgb(255, 205, 86)'];
  config.data.datasets := [dataset];
  TChart.new('myChart', config);
end.
