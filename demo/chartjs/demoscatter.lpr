program demoscatter;

{$MODE OBJFPC}
{$MODESWITCH EXTERNALCLASS}

uses
  ChartJS;

var
  config: TChartConfiguration;
  dataset: TChartScatterDataset;
begin
  config := TChartConfiguration.new;
  config.type_ := 'scatter';
  dataset := TChartScatterDataset.new;
  dataset.label_ := 'Scatter Dataset';
  dataset.datas := [TChartXYData.new(1, -1.711e-2),
    TChartXYData.new(1.26, -2.708e-2), TChartXYData.new(1.58, -4.285e-2),
    TChartXYData.new(2.0, -6.772e-2), TChartXYData.new(2.51, -1.068e-1),
    TChartXYData.new(3.16, -1.681e-1), TChartXYData.new(3.98, -2.635e-1),
    TChartXYData.new(5.01, -4.106e-1), TChartXYData.new(6.31, -6.339e-1),
    TChartXYData.new(7.94, -9.659e-1), TChartXYData.new(10.00, -1.445),
    TChartXYData.new(12.6, -2.110), TChartXYData.new(15.8, -2.992),
    TChartXYData.new(20.0, -4.102), TChartXYData.new(25.1, -5.429),
    TChartXYData.new(31.6, -6.944), TChartXYData.new(39.8, -8.607),
    TChartXYData.new(50.1, -1.038e1), TChartXYData.new(63.1, -1.223e1),
    TChartXYData.new(79.4, -1.413e1), TChartXYData.new(100.00, -1.607e1),
    TChartXYData.new(126, -1.803e1), TChartXYData.new(158, -2e1),
    TChartXYData.new(200, -2.199e1), TChartXYData.new(251, -2.398e1),
    TChartXYData.new(316, -2.597e1), TChartXYData.new(398, -2.797e1),
    TChartXYData.new(501, -2.996e1), TChartXYData.new(631, -3.196e1),
    TChartXYData.new(794, -3.396e1), TChartXYData.new(1000, -3.596e1)];
  dataset.borderColor := 'rgba(255, 99, 132, 0.5)';
  dataset.backgroundColor := 'rgb(255, 99, 132)';
  config.data := TChartData.new;
  config.data.datasets := [dataset];
  TChart.new('myChart', config);
end.
