{
    This file is part of the Pas2JS run time library.
    Copyright (C) 2018 Silvio Clecio (silvioprog)

    Pascal mapping for jsPDF: https://parall.ax/products/jspdf

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit jsPDF;

{$MODE OBJFPC}
{$MODESWITCH EXTERNALCLASS}

interface

uses
  Types,
  JS,
  SysUtils;

type
  TjsPDF = class external name 'jsPDF'
  public class var
    { jsPDF.API is a STATIC property of jsPDF class. jsPDF.API is an object you
      can add methods and properties to. The methods / properties you add will
      show up in new jsPDF objects. One property is prepopulated. It is the
      'events' Object. Plugin authors can add topics, callbacks to this object.
      These will be reassigned to all new instances of jsPDF. }
    API: TJSObject external name 'API';
    { The version of jsPDF. }
    version: string external name 'version';
  public
    { Creates new jsPDF document object instance. If the first parameter
      (orientation) is an object, it will be interpreted as an object of named
      parameters. }
    constructor new(
      { Measurement unit to be used when coordinates are specified.
        Possible values are "pt" (points), "mm" (Default), "cm", "in" or "px". }
      orientation: JSValue = 'p';
      { Measurement unit to be used when coordinates are specified. Possible
        values are "pt" (points), "mm" (Default), "cm", "in" or "px". }
      const &unit: string = 'mm';
      { The format of the first page. Can be:

          a0 - a10
          b0 - b10
          c0 - c10
          dl
          letter
          government-letter
          legal
          junior-legal
          ledger
          tabloid
          credit-card

        Default is "a4". If you want to use your own format just pass instead
        of one of the above predefined formats the size as an number-array,
        e.g. [595.28, 841.89] }
      const format: string = 'a4'); overload;
    constructor new(const orientation: string = 'p'; const &unit: string = 'mm';
      const format: string = 'a4'); overload;
    { Add a custom font to the current instance. }
    procedure addFont(
      { PDF specification full name for the font. }
      const postScriptName: string;
      { PDF-document-instance-specific label assinged to the font. }
      const id: string = '';
      { Style of the Font. }
      const fontStyle: string = '';
      { Encoding_name-to-Font_metrics_object mapping. }
      encoding: JSValue = nil);
    { Adds (and transfers the focus to) new page to the PDF document. }
    function addPage(
      { The format of the new page. Can be:

          a0 - a10
          b0 - b10
          c0 - c10
          dl
          letter
          government-letter
          legal
          junior-legal
          ledger
          tabloid
          credit-card

          Default is "a4". If you want to use your own format just pass instead
          of one of the above predefined formats the size as an number-array,
          e.g. [595.28, 841.89] }
      const format: string = 'a4';
      { Orientation of the new page. Possible values are "portrait" or
        "landscape" (or shortcuts "p" (Default), "l"). }
      const orientation: string = 'p'): TjsPDF; overload;
    function addPage(format: array of Double = nil;
      const orientation: string = 'p'): TjsPDF; overload;
    { Adds an circle to PDF. }
    function circle(
      { Coordinate (in units declared at inception of PDF document) against left
        edge of the page. }
      x: JSValue = Undefined;
      { Coordinate (in units declared at inception of PDF document) against
        upper edge of the page. }
      y: JSValue = Undefined;
      { Radius (in units declared at inception of PDF document). }
      r: JSValue = Undefined;
      { A string specifying the painting style or null. Valid styles
        include: 'S' [default] - stroke, 'F' - fill, and 'DF' (or 'FD') - fill
        then stroke. A null value postpones setting the style so that a shape
        may be composed using multiple method calls. The last drawing method
        call used to define the shape should not have a null style argument. }
      const style: string = 'S'): TjsPDF;
    { All .clip() after calling drawing ops with a style argument of null. }
    function clip(const rule: string): TjsPDF;
    { Deletes a page from the PDF. }
    function deletePage: TjsPDF;
    { Adds an ellipse to PDF. }
    function ellipse(
      { Coordinate (in units declared at inception of PDF document) against left
        edge of the page. }
      x: JSValue = Undefined;
      { Coordinate (in units declared at inception of PDF document) against
        upper edge of the page. }
      y: JSValue = Undefined;
      { Radius along x axis (in units declared at inception of PDF document). }
      rx: JSValue = Undefined;
      { Radius along y axis (in units declared at inception of PDF document). }
      ry: JSValue = Undefined;
      { A string specifying the painting style or null. Valid styles
        include: 'S' [default] - stroke, 'F' - fill, and 'DF' (or 'FD') - fill
        then stroke. A null value postpones setting the style so that a shape
        may be composed using multiple method calls. The last drawing method
        call used to define the shape should not have a null style argument. }
      const style: string = 'S'): TjsPDF;
    { Get global value of CharSpace. }
    function getCharSpace: Double;
    { }
    function getCreationDate: TJSDate;
    { Gets the stroke color for upcoming elements. }
    function getDrawColor: string;
    { GUID. }
    function getFileId: string;
    { Gets the fill color for upcoming elements. }
    function getFillColor: string;
    { Returns an object - a tree of fontName to fontStyle relationships
      available to active PDF document. }
    function getFontList: TJSObject;
    { Gets the fontsize for upcoming text elements. }
    function getFontSize: Double;
    { Gets the LineHeightFactor, default: 1.15. }
    function getLineHeightFactor: Double;
    { Get value of R2L functionality. }
    function getR2L: Boolean;
    { Gets the text color for upcoming elements. }
    function getTextColor: string;
    { }
    function insertPage(beforePage: TJSObject): TjsPDF;
    { Draw a line on the current page. }
    function line(x1: JSValue = Undefined; y1: JSValue = Undefined;
      x2: JSValue = Undefined; y2: JSValue = Undefined): TjsPDF;
    { Adds series of curves (straight lines or cubic bezier curves) to canvas,
      starting at `x`, `y` coordinates. All data points in `lines` are relative
      to last line origin. `x`, `y` become x1,y1 for first line / curve in the
      set. For lines you only need to specify [x2, y2] - (ending point) vector
      against x1, y1 starting point. For bezier curves you need to specify
      [x2,y2,x3,y3,x4,y4] - vectors to control points 1, 2, ending point. All
      vectors are against the start of the curve - x1,y1. }
    function lines(
      { Array of *vector* shifts as pairs (lines) or sextets (cubic bezier
        curves). }
      lines: array of TDoubleDynArray;
      { Coordinate (in units declared at inception of PDF document) against left
        edge of the page. }
      x: JSValue = Undefined;
      { Coordinate (in units declared at inception of PDF document) against
        upper edge of the page. }
      y: JSValue = Undefined;
      { (Defaults to [1.0,1.0]) x,y Scaling factor for all vectors. Elements can
        be any floating number Sub-one makes drawing smaller. Over-one grows the
        drawing. Negative flips the direction. }
      scale: TDoubleDynArray = [1.0, 1.0];
      { A string specifying the painting style or null. Valid styles
        include: 'S' [default] - stroke, 'F' - fill, and 'DF' (or 'FD') - fill
        then stroke. A null value postpones setting the style so that a shape
        may be composed using multiple method calls. The last drawing method
        call used to define the shape should not have a null style argument. }
      const style: string = 'S';
      { If true, the path is closed with a straight line from the end of the
        last curve to the starting point. }
      closed: Boolean = False): TjsPDF;
    { }
    function movePage(targetPage: TJSObject; beforePage: TJSObject): TjsPDF;
    { Generates the PDF document. If `type` argument is undefined, output is raw
      body of resulting PDF returned as a string. }
    function output(
      { A string identifying one of the possible output types. Possible values
        are 'arraybuffer', 'blob', 'bloburi'/'bloburl',
        'datauristring'/'dataurlstring', 'datauri'/'dataurl',
        'dataurlnewwindow'. }
      const &type: string;
      { An object providing some additional signalling to PDF generator.
        Possible options are 'filename'. }
      options: TJSObject = nil): TjsPDF;
    function output_(const &type: string;
      options: TJSObject = nil): string; external name 'output';
    { Adds a rectangle to PDF. }
    function rect(
      { Coordinate (in units declared at inception of PDF document) against left
        edge of the page. }
      x: JSValue = Undefined;
      { Coordinate (in units declared at inception of PDF document) against
        upper edge of the page. }
      y: JSValue = Undefined;
      { Width (in units declared at inception of PDF document). }
      w: JSValue = Undefined;
      { Height (in units declared at inception of PDF document). }
      h: JSValue = Undefined;
      { A string specifying the painting style or null. Valid styles
        include: 'S' [default] - stroke, 'F' - fill, and 'DF' (or 'FD') - fill
        then stroke. A null value postpones setting the style so that a shape
        may be composed using multiple method calls. The last drawing method
        call used to define the shape should not have a null style argument. }
      const style: string = 'S'): TjsPDF;
    { Adds a rectangle with rounded corners to PDF. }
    function roundedRect(
      { Coordinate (in units declared at inception of PDF document) against left
        edge of the page. }
      x: JSValue = Undefined;
      { Coordinate (in units declared at inception of PDF document) against
        upper edge of the page. }
      y: JSValue = Undefined;
      { Width (in units declared at inception of PDF document). }
      w: JSValue = Undefined;
      { Height (in units declared at inception of PDF document). }
      h: JSValue = Undefined;
      { Radius along x axis (in units declared at inception of PDF document). }
      rx: JSValue = Undefined;
      { Radius along y axis (in units declared at inception of PDF document). }
      ry: JSValue = Undefined;
      { A string specifying the painting style or null. Valid styles
        include: 'S' [default] - stroke, 'F' - fill, and 'DF' (or 'FD') - fill
        then stroke. A null value postpones setting the style so that a shape
        may be composed using multiple method calls. The last drawing method
        call used to define the shape should not have a null style argument. }
      const style: string = 'S'): TjsPDF;
    { Saves as PDF document. An alias of jsPDF.output('save', 'filename.pdf').
      Uses FileSaver.js-method saveAs. }
    function save(
      { The filename including extension. }
      const filename: TFileName;
      { An Object with additional options, possible options: 'returnPromise'. }
      options: TJSObject = nil): TjsPDF;
    { Set global value of CharSpace. }
    function setCharSpace(charSpace: JSValue): TjsPDF;
    { }
    function setCreationDate(date: TJSDate): TjsPDF;
    { Set the display mode options of the page like zoom and layout. }
    function setDisplayMode(
      { You can pass an integer or percentage as a string. 2 will scale the
        document up 2x, '200%' will scale up by the same amount. You can also
        set it to 'fullwidth', 'fullheight', 'fullpage', or 'original'. Only
        certain PDF readers support this, such as Adobe Acrobat. }
      zoom: JSValue;
      { Layout mode can be:
        'continuous' - this is the default continuous scroll.
        'single' - the single page mode only shows one page at a time.
        'twoleft' - two column left mode, first page starts on the left, and
        'tworight' - pages are laid out in two columns, with the first page on
        the right. This would be used for books. }
      const layout: string = '';
      { 'UseOutlines' - it shows the outline of the document on the left.
        'UseThumbs' - shows thumbnails along the left.
        'FullScreen' - prompts the user to enter fullscreen mode. }
      const pmode: string = ''): TjsPDF;
    { Adds a properties to the PDF document. }
    function setDocumentProperties(
      { property_name-to-property_value object structure. }
      A: TJSObject): TjsPDF;
    { Sets the stroke color for upcoming elements. Depending on the number of
      arguments given, Gray, RGB, or CMYK color space is implied. When only ch1
      is given, "Gray" color space is implied and it must be a value in the
      range from 0.00 (solid black) to to 1.00 (white) if values are
      communicated as String types, or in range from 0 (black) to 255 (white) if
      communicated as Number type. The RGB-like 0-255 range is provided for
      backward compatibility. When only ch1,ch2,ch3 are given, "RGB" color space
      is implied and each value must be in the range from 0.00 (minimum intensity)
      to to 1.00 (max intensity) if values are communicated as String types, or
      from 0 (min intensity) to to 255 (max intensity) if values are
      communicated as Number types. The RGB-like 0-255 range is provided for
      backward compatibility. When ch1,ch2,ch3,ch4 are given, "CMYK" color space
      is implied and each value must be a in the range from 0.00 (0% concentration)
      to to 1.00 (100% concentration) Because JavaScript treats fixed point
      numbers badly (rounds to floating point nearest to binary representation)
      it is highly advised to communicate the fractional numbers as String types,
      not JavaScript Number type. }
    function setDrawColor(
      (* Color channel value or {string} ch1 color value in hexadecimal,
         example: '#FFFFFF'. *)
      ch1: JSValue = Undefined;
      { Color channel value. }
      ch2: JSValue = Undefined;
      { Color channel value. }
      ch3: JSValue = Undefined;
      { Color channel value. }
      ch4: JSValue = Undefined): TjsPDF;
    { }
    function setFileId(
      { GUID. }
      const value: string): TjsPDF;
    { Sets the fill color for upcoming elements. Depending on the number of arguments given, Gray, RGB, or CMYK color space is implied. When only ch1 is given, "Gray" color space is implied and it must be a value in the range from 0.00 (solid black) to to 1.00 (white) if values are communicated as String types, or in range from 0 (black) to 255 (white) if communicated as Number type. The RGB-like 0-255 range is provided for backward compatibility. When only ch1,ch2,ch3 are given, "RGB" color space is implied and each value must be in the range from 0.00 (minimum intensity) to to 1.00 (max intensity) if values are communicated as String types, or from 0 (min intensity) to to 255 (max intensity) if values are communicated as Number types. The RGB-like 0-255 range is provided for backward compatibility. When ch1,ch2,ch3,ch4 are given, "CMYK" color space is implied and each value must be a in the range from 0.00 (0% concentration) to to 1.00 (100% concentration) Because JavaScript treats fixed point numbers badly (rounds to floating point nearest to binary representation) it is highly advised to communicate the fractional numbers as String types, not JavaScript Number type. }
    function setFillColor(
      (* Color channel value or {string} ch1 color value in hexadecimal,
         example: '#FFFFFF'. *)
      ch1: JSValue = Undefined;
      { Color channel value. }
      ch2: JSValue = Undefined;
      { Color channel value. }
      ch3: JSValue = Undefined;
      { Color channel value. }
      ch4: JSValue = Undefined): TjsPDF;
    { Sets text font face, variant for upcoming text elements. See output of
      jsPDF.getFontList() for possible font names, styles. }
    function setFont(
      { Font name or family. Example: "times". }
      const fontName: string;
      { Font style or variant. Example: "italic". }
      const fontStyle: string): TjsPDF; overload;
    function setFont(const fontName: string): TjsPDF; overload;
    { Sets font size for upcoming text elements. }
    function setFontSize(
      { Font size in points. }
      size: JSValue): TjsPDF;
    { Switches font style or variant for upcoming text elements, while keeping
      the font face or family same. See output of jsPDF.getFontList() for
      possible font names, styles. }
    function setFontStyle(
      { Font style or variant. Example: "italic". }
      const style: string): TjsPDF;
    (* Sets the line cap styles. See {jsPDF.CapJoinStyles} for variants. *)
    function setLineCap(
      { A string or number identifying the type of line cap. }
      style: JSValue): TjsPDF;
    { Sets the LineHeightFactor of proportion. }
    function setLineHeightFactor(
      { LineHeightFactor value. Default: 1.15. }
      value: JSValue): TjsPDF;
    (* Sets the line join styles. See {jsPDF.CapJoinStyles} for variants. *)
    function setLineJoin(
      { A string or number identifying the type of line join. }
      style: JSValue): TjsPDF;
    { Sets line width for upcoming lines. }
    function setLineWidth(
      { Line width (in units declared at inception of PDF document). }
      width: JSValue): TjsPDF;
    { Adds (and transfers the focus to) new page to the PDF document. }
    function setPage(
      { Switch the active page to the page number specified. }
      page: Cardinal): TjsPDF;
    { Set value of R2L functionality. }
    function setR2L(value: Boolean = True): TjsPDF;
    { Sets the text color for upcoming elements. Depending on the number of
      arguments given, Gray, RGB, or CMYK color space is implied. When only ch1
      is given, "Gray" color space is implied and it must be a value in the
      range from 0.00 (solid black) to to 1.00 (white) if values are
      communicated as String types, or in range from 0 (black) to 255 (white) if
      communicated as Number type. The RGB-like 0-255 range is provided for
      backward compatibility. When only ch1,ch2,ch3 are given, "RGB" color space
      is implied and each value must be in the range from 0.00 (minimum intensity)
      to to 1.00 (max intensity) if values are communicated as String types, or
      from 0 (min intensity) to to 255 (max intensity) if values are communicated
      as Number types. The RGB-like 0-255 range is provided for backward
      compatibility. When ch1,ch2,ch3,ch4 are given, "CMYK" color space is
      implied and each value must be a in the range from 0.00 (0% concentration)
      to to 1.00 (100% concentration) Because JavaScript treats fixed point
      numbers badly (rounds to floating point nearest to binary representation)
      it is highly advised to communicate the fractional numbers as String types,
      not JavaScript Number type. }
    function setTextColor(
      (* Color channel value or {string} ch1 color value in hexadecimal,
         example: '#FFFFFF'. *)
      ch1: JSValue = Undefined;
      { Color channel value. }
      ch2: JSValue = Undefined;
      { Color channel value. }
      ch3: JSValue = Undefined;
      { Color channel value. }
      ch4: JSValue = Undefined): TjsPDF;
    { Adds text to page. Supports adding multiline text when 'text' argument is
      an Array of Strings. }
    function text(
      { String or array of strings to be added to the page. Each line is shifted
        one line down per font, spacing settings declared before this call. }
      const text: string;
      { Coordinate (in units declared at inception of PDF document) against left
        edge of the page. }
      x: JSValue = Undefined;
      { Coordinate (in units declared at inception of PDF document) against
        upper edge of the page. }
      y: JSValue = Undefined;
      { Collection of settings signaling how the text must be encoded. }
      options: JSValue = Undefined): TjsPDF; overload; varargs;
    function text(const text: array of string;
      x: JSValue = Undefined; y: JSValue = Undefined;
      options: JSValue = nil): TjsPDF; overload; varargs;
    { Adds a triangle to PDF. }
    function triangle(
      { Coordinate (in units declared at inception of PDF document) against left
        edge of the page. }
      x1: JSValue = Undefined;
      { Coordinate (in units declared at inception of PDF document) against
        upper edge of the page. }
      y1: JSValue = Undefined;
      { Coordinate (in units declared at inception of PDF document) against
        left edge of the page. }
      x2: JSValue = Undefined;
      { Coordinate (in units declared at inception of PDF document) against
        upper edge of the page. }
      y2: JSValue = Undefined;
      { Coordinate (in units declared at inception of PDF document) against
        left edge of the page. }
      x3: JSValue = Undefined;
      { Coordinate (in units declared at inception of PDF document) against
        upper edge of the page. }
      y3: JSValue = Undefined;
      { A string specifying the painting style or null. Valid styles
        include: 'S' [default] - stroke, 'F' - fill, and 'DF' (or 'FD') - fill
        then stroke. A null value postpones setting the style so that a shape
        may be composed using multiple method calls. The last drawing method
        call used to define the shape should not have a null style argument. }
      const style: string = ''): TjsPDF;
  end;

implementation

end.
