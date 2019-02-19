program demobubble;

{$MODE OBJFPC}
{$MODESWITCH EXTERNALCLASS}

uses
  ChartJS;

var
  config: TChartConfiguration;
  dataset: TChartBubbleDataset;
begin
  config := TChartConfiguration.new;
  config.type_ := 'bubble';
  dataset := TChartBubbleDataset.new;
  dataset.label_ := 'First Dataset';
  dataset.data := [TChartXYRData.new(20, 30, 15), TChartXYRData.new(40, 10, 10)];
  dataset.backgroundColor := 'rgb(255, 99, 132)';
  config.data := TChartData.new;
  config.data.datasets := [dataset];
  TChart.new('myChart', config);
end.
