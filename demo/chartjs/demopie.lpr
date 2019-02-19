program demopie;

{$MODE OBJFPC}
{$MODESWITCH EXTERNALCLASS}

uses
  JS,
  Web,
  SysUtils,
  ChartJS;

var
  config: TChartConfiguration;
  dataset: TChartBarDataset;
begin
  TChart.defaults.global.tooltips.custom :=
    procedure(const tooltip: TChartTooltipModel)
    var
      tooltipEl: TJSHTMLElement;
      titleLines, bodyLines: TJSArray;
      innerHtml, style, span: string;
      colors: TChartTooltipReturnColors;
      positionY, positionX: NativeUInt;
    begin
      tooltipEl := TJSHTMLElement(document.getElementById('chartjs-tooltip'));
      // Hide if no tooltip
      if tooltip.opacity = 0 then
      begin
        tooltipEl.style['opacity'] := 0;
        Exit;
      end;
      // Set caret Position
      tooltipEl.classList.remove('above', 'below', 'no-transform');
      if Assigned(tooltip.yAlign) then
        tooltipEl.classList.add(tooltip.yAlign)
      else
        tooltipEl.classList.add('no-transform');
      // Set Text
      if Assigned(tooltip.body) then
      begin
        if Assigned(tooltip.title_) then
          titleLines := tooltip.title_
        else
          titleLines := TJSArray.new;
        bodyLines := tooltip.body_.map(
          function(bodyItem: JSValue; index: NativeInt; arr: TJSArray): JSValue
          begin
            Result := TChartTooltipModelBody(bodyItem).lines;
          end);
        innerHtml := '<thead>';
        titleLines.forEach(
          function(title: JSValue; index: NativeInt; arr: TJSArray): Boolean
          begin
            innerHtml += '<tr><th>' + string(title) + '</th></tr>';
            Result := True;
          end);
        innerHtml += '</thead><tbody>';
        bodyLines.forEach(
          function(body: JSValue; index: NativeInt; arr: TJSArray): Boolean
          begin
            colors := TChartTooltipReturnColors(tooltip.labelColors_[index]);
            style := 'background:' + colors.backgroundColor;
            style += '; border-color:' + colors.borderColor;
            style += '; border-width: 2px';
            span := '<span class="chartjs-tooltip-key" style="' + style +
              '"></span>';
            innerHtml += '<tr><td>' + span + string(body) + '</td></tr>';
            Result := True;
          end);
        innerHtml += '</tbody>';
        tooltipEl.querySelector('table').innerHTML := innerHtml;
      end;
      asm
        positionY = this._chart.canvas.offsetTop;
        positionX = this._chart.canvas.offsetLeft;
      end;
      // Display, position, and set styles for font
      tooltipEl.style['opacity'] := 1;
      tooltipEl.style['left'] := IntToStr(positionX + tooltip.caretX) + 'px';
      tooltipEl.style['top'] := IntToStr(positionY + tooltip.caretY) + 'px';
      tooltipEl.style['fontFamily'] := tooltip._bodyFontFamily;
      tooltipEl.style['fontSize'] := tooltip.bodyFontSize;
      tooltipEl.style['fontStyle'] := tooltip._bodyFontStyle;
      tooltipEl.style['padding'] := IntToStr(tooltip.yPadding) + 'px ' +
        IntToStr(tooltip.xPadding) + 'px';
    end;
  config := TChartConfiguration.new;
  config.type_ := 'pie';
  config.data := TChartData.new;
  dataset := TChartBarDataset.new;
  dataset.data := [300, 50, 100, 40, 10];
  dataset.backgroundColors := ['rgb(255, 99, 132)', 'rgb(255, 159, 64)',
    'rgb(255, 205, 86)', 'rgb(75, 192, 192)', 'rgb(54, 162, 235)'];
  config.data.datasets := [dataset];
  config.data.labels := ['Red', 'Orange', 'Yellow', 'Green', 'Blue'];
  config.options := TChartOptions.new;
  config.options.responsive := true;
  config.options.legend := TChartLegendConfiguration.new;
  config.options.legend.display := False;
  config.options.tooltips := TChartTooltipsConfiguration.new;
  config.options.tooltips.enabled := False;
  TChart.new('chart-area', config);
end.
