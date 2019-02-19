program demoradar;

{$MODE OBJFPC}
{$MODESWITCH EXTERNALCLASS}

uses
  ChartJS;

var
  config: TChartConfiguration;
  dataset1, dataset2: TChartRadarDataset;
begin
  config := TChartConfiguration.new;
  config.type_ := 'radar';
  config.data := TChartData.new;
  config.data.labels := ['Eating', 'Drinking', 'Sleeping', 'Designing',
    'Coding', 'Cycling', 'Running'];

  dataset1 := TChartRadarDataset.new;
  dataset1.label_ := 'My First Dataset';
  dataset1.data := [65, 59, 90, 81, 56, 55, 40];
  dataset1.fill := True;
  dataset1.backgroundColor := 'rgba(255, 99, 132, 0.2)';
  dataset1.borderColor := 'rgb(255, 99, 132)';
  dataset1.pointBackgroundColor := 'rgb(255, 99, 132)';
  dataset1.pointBorderColor := '#fff';
  dataset1.pointHoverBackgroundColor := '#fff';
  dataset1.pointHoverBorderColor := 'rgb(255, 99, 132)';

  dataset2 := TChartRadarDataset.new;
  dataset2.label_ := 'My Second Dataset';
  dataset2.data := [28, 48, 40, 19, 96, 27, 100];
  dataset2.fill := True;
  dataset2.backgroundColor := 'rgba(54, 162, 235, 0.2)';
  dataset2.borderColor := 'rgb(54, 162, 235)';
  dataset2.pointBackgroundColor := 'rgb(54, 162, 235)';
  dataset2.pointBorderColor := '#fff';
  dataset2.pointHoverBackgroundColor := '#fff';
  dataset2.pointHoverBorderColor := 'rgb(54, 162, 235)';

  config.options := TChartOptions.new;
  config.options.elements := TChartElementsConfiguration.new;
  config.options.elements.line := TChartElementLine.new;
  config.options.elements.line.tension := 0;
  config.options.elements.line.borderWidth := 3;

  config.data.datasets := [dataset1, dataset2];
  TChart.new('myChart', config);
end.
