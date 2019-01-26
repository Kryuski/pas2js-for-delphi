program basic;

{$MODE OBJFPC}
{$MODESWITCH EXTERNALCLASS}

uses
  Web,
  jsPDF;

var
  doc: TjsPDF;
  opt: string;
  edSelExample: TJSHTMLSelectElement;
  btDownload: TJSHTMLButtonElement;
  ebPreview: TJSHTMLEmbedElement;

procedure edSelExampleChange;
begin
  doc := TjsPDF.new;
  opt := edSelExample.value;
  case opt of
    'hello': doc.text('Hello world. I love TjsPDF! :-)', 10, 10);
    'font':
    begin
      doc.text('This is the default font.', 20, 20);

      doc.setFont('courier');
      doc.setFontStyle('normal');
      doc.text('This is courier normal.', 20, 30);

      doc.setFont('times');
      doc.setFontStyle('italic');
      doc.text('This is times italic.', 20, 40);

      doc.setFont('helvetica');
      doc.setFontStyle('bold');
      doc.text('This is helvetica bold.', 20, 50);

      doc.setFont('courier');
      doc.setFontStyle('bolditalic');
      doc.text('This is courier bolditalic.', 20, 60);

      doc.setFont('times');
      doc.setFontStyle('normal');
      doc.text('This is centred text.', 105, 80, nil, nil, 'center');
      doc.text('And a little bit more underneath it.', 105, 90, nil, nil, 'center');
      doc.text('This is right aligned text', 200, 100, nil, nil, 'right');
      doc.text('And some more', 200, 110, nil, nil, 'right');
      doc.text('Back to left',20, 120);

      doc.text('10 degrees rotated', 20, 140, nil, 10);
      doc.text('-10 degrees rotated', 20, 160, nil, -10);
    end;
    'two_page':
    begin
      doc.text('Hello world!', 20, 20);
      doc.text('This is client-side Javascript, pumping out a PDF.', 20, 30);
      doc.addPage('l', 'a6');
      doc.text('Do you like that?', 20, 20);
    end;
    'circles':
    begin
      doc.ellipse(40, 20, 10, 5);

      doc.setFillColor(0,0,255);
      doc.ellipse(80, 20, 10, 5, 'F');

      doc.setLineWidth(1);
      doc.setDrawColor(0);
      doc.setFillColor(255,0,0);
      doc.circle(120, 20, 5, 'FD');
    end;
    'lines':
    begin
      doc.line(20, 20, 60, 20); // horizontal line

      doc.setLineWidth(0.5);
      doc.line(20, 25, 60, 25);

      doc.setLineWidth(1);
      doc.line(20, 30, 60, 30);

      doc.setLineWidth(1.5);
      doc.line(20, 35, 60, 35);

      doc.setDrawColor(255,0,0); // draw red lines

      doc.setLineWidth(0.1);
      doc.line(100, 20, 100, 60); // vertical line

      doc.setLineWidth(0.5);
      doc.line(105, 20, 105, 60);

      doc.setLineWidth(1);
      doc.line(110, 20, 110, 60);

      doc.setLineWidth(1.5);
      doc.line(115, 20, 115, 60);
    end;
    'rectangles':
    begin
      // Empty square
      doc.rect(20, 20, 10, 10);

      // Filled square
      doc.rect(40, 20, 10, 10, 'F');

      // Empty red square
      doc.setDrawColor(255,0,0);
      doc.rect(60, 20, 10, 10);

      // Filled square with red borders
      doc.setDrawColor(255,0,0);
      doc.rect(80, 20, 10, 10, 'FD');

      // Filled red square
      doc.setDrawColor(0);
      doc.setFillColor(255,0,0);
      doc.rect(100, 20, 10, 10, 'F');

       // Filled red square with black borders
      doc.setDrawColor(0);
      doc.setFillColor(255,0,0);
      doc.rect(120, 20, 10, 10, 'FD');

      // Black square with rounded corners
      doc.setDrawColor(0);
      doc.setFillColor(255, 255, 255);
      doc.roundedRect(140, 20, 10, 10, 3, 3, 'FD');
    end;
    'triangles':
    begin
      doc.triangle(60, 100, 60, 120, 80, 110, 'FD');

      doc.setLineWidth(1);
      doc.setDrawColor(255,0,0);
      doc.setFillColor(0,0,255);
      doc.triangle(100, 100, 110, 100, 120, 130, 'FD');
    end;
  end;
  ebPreview.src := doc.output_('datauristring');
  btDownload.disabled := opt = '';
end;

procedure btDownloadClick;
begin
  doc.save(opt + '.pdf');
end;

begin
  edSelExample := TJSHTMLSelectElement(document.getElementById('edSelExample'));
  edSelExample.addEventListener('change', @edSelExampleChange);
  btDownload := TJSHTMLButtonElement(document.getElementById('btDownload'));
  btDownload.addEventListener('click', @btDownloadClick);
  ebPreview := TJSHTMLEmbedElement(document.getElementById('ebPreview'));
end.
