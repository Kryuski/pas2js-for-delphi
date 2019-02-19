program demotime;

{$MODE OBJFPC}
{$MODESWITCH EXTERNALCLASS}

uses
  JS,
  Web,
  Math,
  ChartJS;

function moment(const date: JSValue; const fmt: string): TJSDate; external name 'moment';

function randomNumber(const min, max: Double): Double;
begin
  Result := Random * (max - min) + min;
end;

function randomBar(const date: TJSDate; const lastClose: Double): TChartTYData;
var
  open, close: Double;
begin
  open := randomNumber(lastClose * 0.95, lastClose * 1.05);
  close := randomNumber(open * 0.95, open * 1.05);
  Result := TChartTYData.new(date.valueOf, close);
end;

procedure generateData(const date, data, labels); assembler;
asm
  while (data.length < 60) {
    date = date.clone().add(1, 'd');
    if (date.isoWeekday() <= 5) {
      data.push(pas.program.randomBar(date, data[data.length - 1].y));
      labels.push(date);
    }
  }
end;

const
  dateFormat = 'MMMM DD YYYY';
var
  select: TJSHTMLSelectElement;
  chart: TChart;
  config: TChartConfiguration;
  dataset: TChartLineDataset;
  data, labels: TJSArray;
  date: TJSDate;
  axis: TChartScaleCartesianTime;
begin
  date := moment('April 01 2017', dateFormat);
  data := TJSArray.new(randomBar(date, 30));
  labels := TJSArray.new(date);

  generateData(date, data, labels);

  dataset := TChartLineDataset.new;
  dataset.label_ := 'CHRT - Chart.js Corporation';
  dataset.backgroundColor := 'rgba(255, 99, 132, 0.5)';
  dataset.borderColor := 'rgb(255, 99, 132)';
  dataset.data_ := data;
  dataset.type_ := 'line';
  dataset.pointRadius := 0;
  dataset.fill := False;
  dataset.lineTension := 0;
  dataset.borderWidth := 2;

  axis := TChartScaleCartesianTime.new;
  axis.type_ := 'time';
  axis.distribution := 'series';
  axis.ticks := TChartScaleCartesianTimeTick.new;
  axis.ticks.source := 'labels';

  config := TChartConfiguration.new;
  config.type_ := 'bar';
  config.data := TChartData.new;
  config.data.datasets := [dataset];
  config.options := TChartOptions.new;
  config.options.scales := TChartScalesConfiguration.new;
  config.options.scales.xAxes_ := TJSArray.new(axis);

  chart := TChart.new('myChart', config);

  select := TJSHTMLSelectElement(document.getElementById('type'));
  select.addEventListener('change',
    procedure
    begin
      chart.config.data.datasets[0].type_ := select.value;
      chart.update;
    end);
end.
