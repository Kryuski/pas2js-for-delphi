program demobar;

{$MODE OBJFPC}
{$MODESWITCH EXTERNALCLASS}

uses
  ChartJS;

var
  config: TChartConfiguration;
  dataset: TChartBarDataset;
  radialLinear: TChartScaleRadialLinear;
begin
  config := TChartConfiguration.new;
  config.type_ := 'bar';
  config.data := TChartData.new;
  config.data.labels := ['January', 'February', 'March', 'April', 'May',
    'June', 'July'];
  dataset := TChartBarDataset.new;
  dataset.label_ := 'My First Dataset';
  dataset.data := [65,59,80,81,56,55,40];
  dataset.backgroundColors := ['rgba(255, 99, 132, 0.2)',
    'rgba(255, 159, 64, 0.2)', 'rgba(255, 205, 86, 0.2)',
    'rgba(75, 192, 192, 0.2)', 'rgba(54, 162, 235, 0.2)',
    'rgba(153, 102, 255, 0.2)', 'rgba(201, 203, 207, 0.2)'];
  dataset.borderColors := ['rgb(255, 99, 132)', 'rgb(255, 159, 64)',
    'rgb(255, 205, 86)', 'rgb(75, 192, 192)', 'rgb(54, 162, 235)',
    'rgb(153, 102, 255)', 'rgb(201, 203, 207)'];
  dataset.borderWidth := 1;
  config.data.datasets := [dataset];

  config.options := TChartOptions.new;
  config.options.scales := TChartScalesConfiguration.new;
  radialLinear := TChartScaleRadialLinear.new;
  radialLinear.ticks := TChartScaleRadialLinearTick.new;
  radialLinear.ticks.beginAtZero := True;
  config.options.scales.yAxes := [radialLinear];

  TChart.new('myChart', config);
end.
