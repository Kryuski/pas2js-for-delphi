unit frmmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JS, Web;

Type

  { TMainForm }
  TJSHTMLElementArray = Array of TJSHTMLElement;

  TFormStyle = (fsPlain,fsHorizontal,fsInline);
  TMainForm = Class(TComponent)
  private
    labelclassname,
    controlclassname,
    Prefix : String;
    FNavs : TJSHTMLElementArray;
    PTop : TJSHTMLElement;
    POptions : TJSHTMLElement;
    CBDemo : TJSHTMLSelectElement;
    CBFormat : TJSHTMLSelectElement;
    CBNewWindow : TJSHTMLInputElement;
    PPDFOptions : TJSHTMLElement;
    PHTMLOptions : TJSHTMLElement;
    PImageOptions : TJSHTMLElement;
    PTabs : TJSHTMLElementArray;
    ImageOptionsArr,
    PDFOptionsArr,
    HTMLOPtionsArr : TJSHTMLElementArray;
    function AddConfigureFramePage(aParent: TJSHTMLElement; AnArray: TJSHTMLElementArray; STartAt: integer): Integer;
    function AddConfigureTOCPage(aParent: TJSHTMLElement; AnArray: TJSHTMLElementArray; STartAt: integer): Integer;
    function AddConfigurePageNavigator(aParent: TJSHTMLElement; AnArray: TJSHTMLElementArray; STartAt: integer): Integer;
    function AddEdit(AParent: TJSHTMLElement; const AName, ALabel: String): TJSHTMLInputElement;
    procedure AddHTMLOptions(AParent: TJSHTMLElement);
    procedure AddImageOptions(AParent: TJSHTMLElement);
    procedure AddPDFOptions(AParent: TJSHTMLElement);
    function AddStyleEmbedding(AParent: TJSHTMLElement): TJSHTMLElement;
    function CreateTabSheet(Aparent: TJSHTMLElement; AIDName: String; IsActive: Boolean): TJSHTMLElement;
    function DemosLoaded(Event: TEventListenerEvent): boolean;
    function DoClick(aEvent: TJSMouseEvent): boolean;
    function DoNavClick(aEvent: TJSMouseEvent): boolean;
    procedure GetDemoList;
    procedure Getoptions(A: TJSHTMLElementArray; L: Tstrings);
  Protected
    Procedure ShowError(Msg : String);
    Function AddFormControl(AParent : TJSHTMLElement; Const AID,ALabel : String) : TJSHTMLElement;
  Public
    Function AddPanel(AParent : TJSHTMLElement; AHeader : String; AHeading : Integer = 0) : TJSHTMLElement;
    Function AddCheckBox(AParent : TJSHTMLElement; Const AName,ALabel : String; IsInline : Boolean = False) : TJSHTMLInputElement;
    Function AddNumber(AParent : TJSHTMLElement; Const AName,ALabel : String) : TJSHTMLInputElement;
    Function AddColor(AParent : TJSHTMLElement; Const AName,ALabel : String) : TJSHTMLInputElement;
    function AddCombo(AParent: TJSHTMLElement; const AName, ALabel: String; AValues: array of String): TJSHTMLSelectElement;
    // Element 0 is the container, 1+i are the tabs
    function AddNav(AParent: TJSHTMLElement; Tabs: array of string; AActive: String; AOnClick: THTMLClickEventHandler): TJSHTMLElementArray;
    Function AddForm(AParent : TJSHTMLElement; FormStyle : TFormStyle = fsPlain) : TJSHTMLFormElement;
    Constructor create(AOwner : TCOmponent); override;
    Procedure Initialize;
  end;

implementation

Const
//  ServerURL = 'http://localhost:8080/';
  ServerURL = '/';
  BaseURL = ServerURL + 'Generate?';

{ TMainForm }

function TMainForm.DoNavClick(aEvent: TJSMouseEvent): boolean;


  Procedure ActivateTab(AID: string);

  Var
    I : Integer;
    E : TJSHTMLElement;

  begin
    For I:=1 to Length(FNavs)-1 do
      begin
      E:=FNavs[i];
      if E.id=AID then
        E.classlist.add('active')
      else
        E.classlist.remove('active');
      end;
  end;

  Procedure Activate(P : TJSHTMLElement);

  Var
    I : Integer;
    E : TJSHTMLElement;

  begin
    For I:=0 to Length(Ptabs)-1 do
      begin
      E:=PTabs[i];
      if E=P then
        begin
        E.classlist.add('active');
        E.classlist.add('in');
        end
      else
        begin
        E.classlist.remove('active');
        E.classlist.remove('in');
        end;
      end;
  end;

Var
  tid : String;

begin
  Result:=true;
  tid:=TJSElement(aEvent.CurrentTarget).id;
  ActivateTab(tid);
  If (tid='navpdf') then
    Activate(PPDFOptions)
  else If (tid='navhtml') then
    Activate(PHTMLOptions)
  else If (tid='navfpimage') then
    Activate(PImageOptions)
end;

function TMainForm.AddFormControl(AParent: TJSHTMLElement; const AID, ALabel: String): TJSHTMLElement;

Var
  FG,D : TJSElement;
  L : TJSElement;

begin
  FG:=Document.createElement('div');
  FG.className:='form-group';
  AParent.appendChild(FG);
  L:=Document.createElement('label');
  L.Attrs['for']:=AID;
  L.classname:=labelclassname;
  FG.append(L);
  if (ALabel<>'') then
    L.append(ALabel);
  D:=Document.createElement('div');
  FG.Append(D);
  D.classname:=controlclassname;
  Result:=TJSHTMLElement(D);
end;

function TMainForm.AddPanel(AParent: TJSHTMLElement; AHeader: String; AHeading: Integer = 0): TJSHTMLElement;

Var
  P,H,H2,B : TJSElement;

begin
  P:=Document.createElement('div');
  P.classname:='panel panel-default';
  AParent.Append(P);
  if (AHeader<>'') then
    begin
    H:=Document.createElement('div');
    h.classname:='panel-heading';
    P.Append(H);
    if AHeading=0 then
      H.Append(AHeader)
    else
      begin
      H2:=Document.createElement('h'+IntToStr(AHeading));
      H2.ClassName:='panel-title';
      H2.Append(AHeader);
      H.Append(H2);
      end;
    end;
  B:=Document.createElement('div');
  P.Append(B);
  B.classname:='panel-body';
  Result:=TJSHTMLElement(B);
end;

function TMainForm.AddNav(AParent: TJSHTMLElement; Tabs: array of string; AActive: String; AOnClick: THTMLClickEventHandler): TJSHTMLElementArray;

Var
  I : Integer;
  TP,A,T : TJSHTMLElement;



begin
  TP:=TJSHTMLElement(Document.CreateElement('ul'));
  TP.ClassName:='nav nav-tabs';
  SetLength(Result,1+(Length(Tabs) div 2));
  Result[0]:=TP;
  AParent.Append(TP);
  I:=0;
  While I<Length(Tabs)-1 do
    begin
    T:=TJSHTMLElement(Document.CreateElement('li'));
    T['role']:='presentation';
    T.id:='nav'+Tabs[i];
    if SameText(T.ID,'nav'+AActive) then
      T.Classname:='active';
    A:=TJSHTMLElement(Document.CreateElement('a'));
    A.Append(Tabs[I+1]);
    A['href']:='#';
    T.onclick:=AOnClick;
    T.Append(A);
    TP.Append(T);
    Result[1+(I div 2)]:=T;
    inc(I,2);
    end;
end;

function TMainForm.AddForm(AParent: TJSHTMLElement; FormStyle: TFormStyle): TJSHTMLFormElement;


begin
  Result:=TJSHTMLFormElement(document.CreateElement('form'));
  Case FormStyle of
    fsInline : Result.className:='form-inline';
    fsHorizontal : Result.className:='form-horizontal';
  end;
  AParent.Append(Result);
end;

function TMainForm.AddCheckBox(AParent: TJSHTMLElement; const AName, ALabel: String; IsInline: Boolean): TJSHTMLInputElement;

Var
  FG,D,D2,L : TJSHTMLElement;
  IE : TJSHTMLInputElement;

begin
//  FG:=AddFormControl(APArent,Prefix+AName,aLabel);
  FG:=TJSHTMLElement(Document.createElement('div'));
  FG.className:='form-group';
  AParent.appendChild(FG);
  D:=TJSHTMLElement(Document.createElement('div'));
  D.ClassName:='col-sm-offset-3 col-sm-8';
  FG.Append(D);
  D2:=TJSHTMLElement(Document.createElement('div'));
  D2.ClassName:='checkbox';
  D.Append(D2);
  L:=TJSHTMLElement(Document.createElement('label'));
  L.Attrs['for']:='ID'+Prefix+AName;
  D2.append(L);
  IE:=TJSHTMLInputElement(Document.createElement('input'));
  L.Append(IE);
  if (ALabel<>'') then
    L.append(ALabel);
  IE['type']:='checkbox';
//  IE.ClassName:='form-control';
  IE.id:='ID'+Prefix+AName;
  IE.name:=Prefix+AName;
  IE.value:='1';
  Result:=IE;
end;

function TMainForm.AddNumber(AParent: TJSHTMLElement; const AName, ALabel: String): TJSHTMLInputElement;

Var
  FG : TJSHTMLElement;
  IE : TJSHTMLInputElement;

begin
  FG:=AddFormControl(APArent,Prefix+AName,aLabel);
  IE:=TJSHTMLInputElement(Document.createElement('input'));
  FG.Append(IE);
  IE['type']:='number';
  IE.ClassName:='form-control';
  IE.id:='ID'+Prefix+AName;
  IE.Name:=Prefix+AName;
  Result:=IE;
end;

function TMainForm.AddColor(AParent: TJSHTMLElement; const AName, ALabel: String): TJSHTMLInputElement;
Var
  FG : TJSHTMLElement;
  IE : TJSHTMLInputElement;

begin
  FG:=AddFormControl(APArent,Prefix+AName,aLabel);
  IE:=TJSHTMLInputElement(Document.createElement('input'));
  FG.Append(IE);
  IE['type']:='color';
  IE.ClassName:='form-control';
  IE.id:='ID'+Prefix+AName;
  IE.name:=Prefix+AName;
  Result:=IE;
end;

function TMainForm.AddEdit(AParent: TJSHTMLElement; const AName, ALabel: String): TJSHTMLInputElement;

Var
  FG : TJSHTMLElement;
  IE : TJSHTMLInputElement;

begin
  FG:=AddFormControl(APArent,Prefix+AName,aLabel);
  IE:=TJSHTMLInputElement(Document.createElement('input'));
  FG.Append(IE);
  IE['type']:='edit';
  IE.ClassName:='form-control';
  IE.id:='ID'+Prefix+AName;
  IE.name:=Prefix+AName;
  Result:=IE;
end;

function TMainForm.AddCombo(AParent: TJSHTMLElement; const AName, ALabel: String; AValues : Array of String): TJSHTMLSelectElement;

Var
  FG: TJSHTMLElement;
  SE : TJSHTMLSelectElement;
  O  : TJSHTMLOptionElement;
  I  : Integer;

begin
  FG:=AddFormControl(APArent,Prefix+AName,ALabel);
  SE:=TJSHTMLSelectElement(Document.createElement('select'));

  SE.ClassName:='form-control';
  FG.Append(SE);
  SE.id:='ID'+Prefix+AName;
  SE.Name:=Prefix+AName;
  I:=0;

  While I<Length(AValues)-1 do
    begin
    O:=TJSHTMLOptionElement(Document.CreateElement('option'));
    O.value:=AVAlues[i];
    O.Append(AVAlues[i+1]);
    SE.add(O);
    Inc(I,2);
    end;
  Result:=SE;
end;


constructor TMainForm.create(AOwner: TCOmponent);
begin
  inherited create(AOwner);
  labelclassname:='col-sm-3 control-label';
  controlclassname:='col-sm-8';
  Initialize;
end;

Function TMainForm.CreateTabSheet(Aparent : TJSHTMLElement;AIDName : String; IsActive : Boolean) : TJSHTMLElement;

Var
  C : String;

begin
  Result:=TJSHTMLElement(Document.createElement('div'));
  C:='tab-pane fade';
  if IsActive then
    c:=c+' in active';
  Result.id:=AIDName;
  Result.classname:=C;
  AParent.append(Result);
end;

Procedure TMainForm.Getoptions(A : TJSHTMLElementArray; L : Tstrings);

Var
  I : Integer;
  E : TJSHTMLElement;
  IE : TJSHTMLInputElement;
  SE : TJSHTMLSelectElement;

begin
  for I:=0 to Length(A)-1 do
    begin
    E:=A[i];
    if SameText(E.tagName,'INPUT') then
      begin
      IE:=TJSHTMLInputElement(E);
      if IE._type='checkbox' then
        begin
        if IE.checked then
          L.Add(IE.Name+'=1');
        end
      else if IE.value<>'' then
        L.Add(IE.Name+'='+IE.value);
      end
    else if SameText(E.tagName,'SELECT') then
      begin
      SE:=TJSHTMLSelectElement(E);
      L.Add(SE.Name+'='+SE.value);
      end;
    end;
end;

function TMainForm.DoClick(aEvent: TJSMouseEvent): boolean;

Var
  Fmt,URL : String;
  A : TJSHTMLElementArray;
  L : Tstrings;
  I : Integer;

begin
  Result:=true;
  URL:=BaseURL;
  fmt:=CBFormat.value;
  if (fmt='HTML') then
    A:=HTMLOPtionsArr
  else if (fmt='PDF') then
    A:=PDFOptionsArr
  else if (fmt='FPImage') then
    A:=ImageOptionsArr
  else
    begin
    ShowError('Unknown output format: '+Fmt);
    Exit;
    end;
  URL:=URL+'demo='+CBDemo.value;
  URL:=URL+'&format='+fmt;
  L:=TstringList.Create;
  try
    GetOptions(A,L);
    For I:=0 to L.Count-1 do
      URL:=URL+'&'+L[i];
  finally
    L.Free;
  end;
  if (CBNewWindow.checked) then
    Window.Open(URL)
  else
    Window.locationString:=URL;
end;

procedure TMainForm.AddPDFOptions(AParent : TJSHTMLElement);

Var
  F : TJSHTMLElement;
  I : Integer;

  Procedure AddOption(E : TJSHTMLElement);

  begin
    PDFOptionsArr[I]:=E;
    inc(I);
  end;

begin
  prefix:='pdf.';
  F:=AddForm(aParent,fsHorizontal);
  I:=0;
  SetLength(PDFOptionsArr,9);
  AddOption(AddCheckbox(F,'pagelayout','Create outLine'));
  AddOption(AddCheckbox(F,'compresstext','Compress text'));
  AddOption(AddCheckbox(F,'compressfonts','Compress fonts'));
  AddOption(AddCheckbox(F,'compressimages','Compress images'));
  AddOption(AddCheckbox(F,'userawjpeg','use raw JPEG'));
  AddOption(AddCheckbox(F,'noembeddedfonts','Do not embed fonts'));
  AddOption(AddCheckbox(F,'pageoriginattop','Page origin at top'));
  AddOption(AddCheckbox(F,'subsetfont','Embed only used subset of font'));
  AddOption(AddCombo(F,'pagelayout','Page layout',['single','Single page','two','Two pages','continuous','Continuous layout']));
end;

Function TMainForm.AddStyleEmbedding (AParent : TJSHTMLElement): TJSHTMLElement;

begin
  Result:=AddCombo(AParent,'styleembedding','Style embedding',[
    'inline','Inline, in HTML element',
    'styletag','In separate style tag',
    'cssfile','In separate CSS file'
  ]);
end;

function TMainForm.AddConfigureFramePage(aParent: TJSHTMLElement; AnArray: TJSHTMLElementArray; STartAt: integer): Integer;
begin
  AnArray[StartAt]:=AddEdit(AParent,'framecssfilename','CSS file name');
  AnArray[StartAt+1]:=AddNumber(AParent,'toczonesize','TOC Zone size (percentage)');
  AnArray[StartAt+2]:=AddCombo(AParent,'toczoneposition','Position of TOC zone',[
  'left','Left',
  'right', 'Right',
  'top' , 'Top',
  'bottom', 'Bottom'
  ]);
  Result:=3;
end;

Function TMainForm.AddConfigureTOCPage(aParent : TJSHTMLElement; AnArray : TJSHTMLElementArray; STartAt : integer) : Integer;

begin
  AnArray[StartAt]:=AddEdit(AParent,'toccssfilename','CSS file name');
  AnArray[StartAt+1]:=AddEdit(AParent,'oddpagestyle','Odd page style elements');
  AnArray[StartAt+2]:=AddEdit(AParent,'evenpagestyle','Even page style elements');
  AnArray[StartAt+3]:=AddCheckBox(AParent,'skipstyling','Skip Styling');
  Result:=4;
end;

function TMainForm.AddConfigurePageNavigator(aParent: TJSHTMLElement; AnArray: TJSHTMLElementArray; STartAt: integer): Integer;

Var
  P : TJSHTMLElement;

begin
  P:=AddPanel(Aparent,'Navigator positions',5);
  anArray[StartAt]:=AddCheckBox(P,'topnavigator','Top',True);
  anArray[StartAt+1]:=AddCheckBox(P,'leftnavigator','Left',True);
  anArray[StartAt+2]:=AddCheckBox(P,'rightnavigator','Right',True);
  anArray[StartAt+3]:=AddCheckBox(P,'bottomnavigator','Bottom',True);
  P:=AddPanel(Aparent,'Navigator options',5);
  anArray[StartAt+4]:=AddCheckBox(P,'firstlast','Add First/Last buttons');
  anArray[StartAt+5]:=AddCheckBox(P,'alwaysfirstlast','Always add First/Last buttons');
  anArray[StartAt+6]:=AddCheckBox(P,'pageno','Add page number');
  anArray[StartAt+7]:=AddCheckBox(P,'image','Use images (Not yet implemented)');
  anArray[StartAt+8]:=AddCheckBox(P,'skipstyling','Skip all styling');
  anArray[StartAt+9]:=AddCheckBox(P,'usepagenofm','Use Page N/M display');
  anArray[StartAt+10]:=AddCheckBox(P,'pagenoedit','Allow page number editing');
  P:=AddPanel(Aparent,'Width/Color',5);
  anArray[StartAt+11]:=AddNumber(P,'navigatorfixedwidth','Fixed width');
  anArray[StartAt+12]:=AddNumber(P,'navigatorfixedheight','Fixed height');
  anArray[StartAt+13]:=AddNumber(P,'navigatorfixedmargin','Fixed margin');
  anArray[StartAt+14]:=AddColor(P,'navigatorbgcolor','Active link color');
  anArray[StartAt+15]:=AddColor(P,'navigatorinactivebgcolor','Inactive link color');
  Result:=16;
end;

procedure TMainForm.AddHTMLOptions(AParent : TJSHTMLElement);

Var
  F : TJSHTMLFormElement;
  E : TJSHTMLElement;
  I : Integer;

  Procedure AddOption(E : TJSHTMLElement);

  begin
    HTMLOptionsArr[I]:=E;
    inc(I);
  end;

begin
  Prefix:='html.';
  F:=AddForm(aParent,fsHorizontal);
  I:=0;
  SetLength(HTMLOptionsArr,100);
  E:=AddPanel(F,'HTML Options',4);
  AddOption(AddCheckBox(E,'fixedpositioning','Use fixed positioning'));
  AddOption(AddCheckbox(E,'inlineimage','Use inline images'));
  AddOption(AddCheckbox(E,'useimgtag','Use IMG tag'));
  AddOption(AddCheckbox(E,'tocpageframe','Create TOC Frame'));
  AddOption(AddCheckbox(E,'memoasis','Insert memos as-is (let browser handle layout)'));
  AddOption(AddCheckbox(E,'externaljs','Use external file for JS'));
  AddOption(AddNumber(E,'DPI','DPI (resolution)'));
  AddOption(AddEdit(E,'sequence','Sequence format'));
  AddOption(AddStyleEmbedding(E));
  AddOption(AddNumber(E,'offsettop','Fixed positioning, offset from top'));
  AddOption(AddNumber(E,'offsetleft','Fixed positioning, offset from left'));
  E:=AddPanel(F,'TOC page',4);
  I:=I+AddConfigureTOCPage(E,HTMLOptionsArr,I);
  E:=AddPanel(F,'Frame page',4);
  I:=I+AddConfigureFramePage(E,HTMLOptionsArr,I);
  E:=AddPanel(F,'Page Navigator',4);
  I:=I+AddConfigurePageNavigator(E,HTMLOptionsArr,I);
  SetLength(HTMLOptionsArr,I);
end;

procedure TMainForm.AddImageOptions(AParent : TJSHTMLElement);

Var
  F : TJSHTMLFormElement;
  E : TJSHTMLElement;
  I : Integer;

  Procedure AddOption(A : TJSHTMLElement);

  begin
    ImageOptionsArr[I]:=A;
    inc(I);
  end;

begin
  F:=AddForm(aParent,fsHorizontal);
  I:=0;
  SetLength(ImageOptionsArr,100);
  E:=AddPanel(F,'Image Options',4);
  Prefix:='image.';
  AddOption(AddCheckBox(E,'useframes','Use frames'));
  AddOption(AddCheckBox(E,'externaljs','Use external Javascript file'));
  AddOption(AddNumber(E,'DPI','Image DPI'));
  AddOption(AddEdit(E,'sequence','Page number sequence format'));
  AddStyleEmbedding(E);
  E:=AddPanel(F,'TOC page',4);
  I:=I+AddConfigureTOCPage(E,ImageOptionsArr,I);
  E:=AddPanel(F,'Frame page',4);
  I:=I+AddConfigureFramePage(E,ImageOptionsArr,I);
  E:=AddPanel(F,'Page Navigator',4);
  I:=I+AddConfigurePageNavigator(E,ImageOptionsArr,I);
  SetLength(ImageOptionsArr,I);
end;

function TMainForm.DemosLoaded(Event: TEventListenerEvent): boolean;

var
  i : integer;
  C,J : TJSObject;
  A : TJSObjectDynArray;
  N,D : String;
  O : TJSHTMLOptionElement;
  xhr : TJSXMLHttpRequest;

begin
  xhr:=TJSXMLHttpRequest(Event.Target);
  console.log('Result of call ',xhr.Status);
  if (xhr.status = 200) then
    begin
    J:=TJSJSON.parseObject(xhr.responseText);
    A:=TJSObjectDynArray(J.Properties['data']);
    for I:=0 to Length(A)-1 do
      begin
      C:=A[i];
      N:=String(C.Properties['name']);
      D:=String(C.Properties['description']);
      O:=TJSHTMLOptionElement(Document.CreateElement('option'));
      O.value:=N;
      O.text:=D;
      CBDemo.append(O);
      end;
    end
  else
    ShowError('Failed to load countries: '+IntToStr(xhr.Status));
  Result := True;
end;

procedure TMainForm.GetDemoList;

Var
  xhr : TJSXMLHttpRequest;

begin
  xhr:=TJSXMLHttpRequest.New;
  xhr.addEventListener('load', @DemosLoaded);
  // Case sensitive
  xhr.open('GET', ServerURL+'ReportList/', true);
  xhr.send;
end;

procedure TMainForm.ShowError(Msg: String);

Var
  E : TJSHTMLElement;

begin
  E:=TJSHTMLElement(Document.CreateElement('p'));
  E.className:='text-danger';
  E.append(Msg);
  Ptop.append(E);
end;

procedure TMainForm.Initialize;

Var
  frm, cont, PP, BOK : TJSHTMLElement;

begin
  cont:=TJSHTMLElement(Document.createElement('div'));
  cont.classname:='container-fluid';
  Document.Body.Append(cont);
  PTop:=AddPanel(Cont,'Report',1);
  frm:=AddForm(PTop ,fsHorizontal);
  frm.style.cssText:='width: 50%';
  CBDemo:=AddCombo(frm,'output','Report demo',[]);
  GetDemoList;

  CBFormat:=AddCombo(frm,'format','Output format',['PDF','PDF','HTML','Plain HTML','FPIMage','Images in HTML pages']);
  CBNewWindow:=AddCheckBox(frm,'NewWindow','Open in new window');
  BOK:=TJSHTMLElement(Document.createElement('button'));
  BOK.Append('Show report');
  BOK.ClassName:='btn btn-default';
  BOK.onClick:=@DoClick;
  PTOP.Append(BOK);
  POptions:=AddPanel(Cont,'Options',1);
  FNavs:=AddNav(POptions,['pdf','PDF','html','HTML','fpimage','Image'],'PDF',@DoNavClick);
  PP:=TJSHTMLElement(Document.CreateElement('div'));
  PP.classname:='tab-content';
  POPtions.Append(PP);
  SetLength(PTabs,3);
  PPDFOptions:=CreateTabSheet(PP,'pdf',True);
  AddPDFOptions(PPDFOptions);
  PHTMLOptions:=CreateTabSheet(PP,'html',False);
  AddHTMLOptions(PHTMLOptions);
  PImageOptions:=CreateTabSheet(PP,'image',False);
  AddImageOptions(PImageOptions);
  PTabs[0]:=PPDFOptions;
  PTabs[1]:=PHTMLOptions;
  PTabs[2]:=PImageOptions;
end;

end.

