unit demobootstrap;

{$mode objfpc}

interface

uses
  sysutils, classes, web, js, webwidget, htmlwidgets, bootstrapwidgets,  widgetdemo;

Type

  { TSimpleToastWidgetDemo }

  TSimpleToastWidgetDemo = class(TDemoContainer)
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;

  { TToastManagerDemo }

  TToastManagerDemo = class(TDemoContainer)
  private
    procedure DoShowToast(Sender: TObject; Event: TJSEvent);
  Protected
    FLabelClosable:TLabelWidget;
    FLabelHeader:TLabelWidget;
    FLabelBody:TLabelWidget;
    FLabelContextual:TLabelWidget;
    FHeader : TTextInputWidget;
    FBody : TTextAreaWidget;
    FContextual : TSelectWidget;
    FClosable : TCheckboxInputWidget;
    FShowButton: TBootstrapButton;
  public
    Class Function Demohelp : String; override;
    Class Function Description : String; override;
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Function GetInspectorInstance: TObject; override;
    Procedure ShowDemo; override;
  end;


  { TBootstrapButtonDemo }

  TBootstrapButtonDemo = class(TDemoContainer)
  Private
  public
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Procedure ShowDemo; override;
  end;


  { TBootstrapModalDemo }

  TBootstrapModalDemo = class(TDemoContainer)
  Private
    FShow : TBootstrapButton;
    FHide : TBootstrapButton;
    FMemo : TTextAreaWidget;
    procedure DoHideDialog(Sender: TObject; Event: TJSEvent);
    procedure DoModalHide(Sender: TObject; El: TJSHTMLElement; Values: TStrings);
    procedure DoShowDialog(Sender: TObject; Event: TJSEvent);
    Function Modal : TBootstrapModal;
  public
    Class Function Demohelp : String; override;
    Class Function Description : String; override;
    class function WebWidgetClass: TCustomWebWidgetClass; override;
    Function GetInspectorInstance: TObject; override;
    Procedure ShowDemo; override;
  end;


implementation

{ TBootstrapModalDemo }

procedure TBootstrapModalDemo.DoHideDialog(Sender: TObject; Event: TJSEvent);
begin
  Modal.Hide;
end;

procedure TBootstrapModalDemo.DoModalHide(Sender: TObject; El: TJSHTMLElement; Values: TStrings);
begin
  Fmemo.Lines.Add('Modal closed and returned '+IntToStr(Values.Count)+' values.');
  FMemo.Lines.AddStrings(Values);
end;

procedure TBootstrapModalDemo.DoShowDialog(Sender: TObject; Event: TJSEvent);
begin
  Modal.Show;
end;

function TBootstrapModalDemo.Modal: TBootstrapModal;
begin
  Result:=WidgetInstance as TBootstrapModal;
end;

class function TBootstrapModalDemo.Demohelp: String;
begin
  Result:='Bootstrap modal showas a modal template and returns list of form values on close (hide)';
end;

class function TBootstrapModalDemo.Description: String;
begin
  Result:='Bootstrap modal demo';
end;

class function TBootstrapModalDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TBootStrapModal;
end;

function TBootstrapModalDemo.GetInspectorInstance: TObject;
begin
  Result:=inherited GetInspectorInstance;
end;


Const
  SampleModal =
    '<div class="modal fade" tabindex="-1" role="dialog" aria-labelledby="exampleModalLabel" aria-hidden="true"> '+
    '  <div class="modal-dialog" role="document">'+
    '    <div class="modal-content">'+
    '      <div class="modal-header">'+
    '        <h5 class="modal-title" id="exampleModalLabel">Modal title</h5>'+
    '        <button type="button" class="close" data-dismiss="modal" aria-label="Close">'+
    '          <span aria-hidden="true">&times;</span>'+
    '        </button>'+
    '      </div>'+
    '      <div class="modal-body">'+
    '        <p>Modal body text goes here.</p>'+
    '      </div>'+
    '      <div class="modal-footer">'+
    '        <button type="button" class="btn btn-secondary" data-dismiss="modal">Close</button>'+
    '        <button type="button" class="btn btn-primary">Save changes</button>'+
    '      </div>'+
    '    </div>'+
    '  </div>'+
    '</div>';

procedure TBootstrapModalDemo.ShowDemo;

Var
  T : TContextual;
  R : TRowWidget;
  C : TColWidget;

  Procedure AddRow;
  begin
    R:=TRowWidget.Create(Self);
    R.Parent:=Self;
    C:=TColWidget.Create(Self);
    C.DefaultColSpan:=6;
    C.Parent:=R;
  end;

begin
  inherited ShowDemo;
  Modal.Template:=SampleModal;
  AddRow;
  C.DefaultColSpan:=6;
  FShow:=TBootstrapButton.Create(Self);
  FShow.OnClick:=@DoShowDialog;
  FShow.Contextual:=cPrimary;
  FShow.Parent:=C;
  FShow.Text:='Show dialog';
  C:=TColWidget.Create(Self);
  C.Parent:=R;
  C.DefaultColSpan:=6;
  FHide:=TBootstrapButton.Create(Self);
  FHide.OnClick:=@DoHideDialog;
  FHide.Contextual:=cSecondary;
  FHide.Parent:=C;
  FHide.Text:='Hide dialog';
  AddRow;
  With TLabelWidget.Create(Self) do
    begin
    Text:='Log:';
    Parent:=C;
    end;
  AddRow;
  FMemo:=TTextAreaWidget.Create(Self);
  FMemo.Parent:=C;
  FMemo.Columns:=80;
  FMemo.Rows:=25;
  Modal.OnHide:=@DoModalHide;
  Refresh;
end;

{ TToastManagerDemo }

procedure TToastManagerDemo.DoShowToast(Sender: TObject; Event: TJSEvent);

Var
  T : TContextual;
begin
  if FContextual.SelectedIndex<>-1 then
    T:=TContextual(FContextual.SelectedIndex)
  else
    T:=cNone;
  TToastManager.Instance.ShowToast(FHeader.Value,FBody.Lines.text,T,FClosable.Checked);
end;

class function TToastManagerDemo.Demohelp: String;
begin
  Result:='Toast manager demo: click button to show a toast in the top-left corner';
end;

class function TToastManagerDemo.Description: String;
begin
  Result:='Toast manager demo';
end;

class function TToastManagerDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=Nil;
end;

function TToastManagerDemo.GetInspectorInstance: TObject;
begin
  Result:=TToastManager.Instance;
end;

procedure TToastManagerDemo.ShowDemo;

Var
  T : TContextual;
  R : TRowWidget;
  C : TColWidget;

  Procedure AddRow;
  begin
    R:=TRowWidget.Create(Self);
    R.Parent:=Self;
    C:=TColWidget.Create(Self);
    C.Parent:=R;
  end;

begin
  inherited ShowDemo;
  TToastManager.Instance.ParentID:='toastarea-stack';
  // Contextual
  AddRow;
  FLabelContextual:=TLabelWidget.Create(Self);
  FLabelContextual.Text:='Contextual class for message';
  FContextual:=TSelectWidget.Create(Self);
  For T in TContextual do
    FContextual.Items.Add(ContextualNames[t]);
  FContextual.SelectedIndex:=0;
  FLabelContextual.LabelFor:=FContextual;
  FLabelContextual.Parent:=C;
  FContextual.Parent:=C;
  FContextual.Classes:='form-control';
  // Header
  AddRow;
  FLabelHeader:=TLabelWidget.Create(Self);
  FLabelHeader.Text:='Toast header';
  FHeader:=TTextInputWidget.Create(Self);
  FHeader.Value:='The message title';
  FLabelHeader.LabelFor:=FHeader;
  FLabelHeader.Parent:=C;
  FHeader.Parent:=C;
  FHeader.Classes:='form-control';
  // Body
  AddRow;
  FLabelBody:=TLabelWidget.Create(Self);
  FLabelBody.Text:='Toast body';
  FBody:=TTextAreaWidget.Create(Self);
  FBody.Lines.Text:='A nice message to show';
  FLabelBody.LabelFor:=FBody;
  FLabelBody.Parent:=C;
  FBody.Parent:=C;
  FBody.Classes:='form-control';
  // Closable checkbox
  AddRow;
  FLabelClosable:=TLabelWidget.Create(Self);
  FLabelClosable.Text:='Allow to close toast';
  FClosable:=TCheckboxInputWidget.Create(Self);
  FClosable.Classes:='form-check-input';
  FLabelClosable.LabelFor:=FClosable;
  FLabelClosable.Parent:=C;
  FClosable.Parent:=C;
  // button
  AddRow;
  FShowButton:=TBootstrapButton.Create(Self);
  FShowButton.Text:='Show toast';
  FShowButton.Parent:=C;
  FShowButton.OnClick:=@DoShowToast;
  Refresh;
end;

{ TBootstrapButtonDemo }

class function TBootstrapButtonDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TBootstrapButton;
end;

procedure TBootstrapButtonDemo.ShowDemo;
begin
  inherited ShowDemo;
  WidgetInstance.OnClick:=@DoClick;
end;

{ TToastManagerDemo }

class function TSimpleToastWidgetDemo.WebWidgetClass: TCustomWebWidgetClass;
begin
  Result:=TSimpleToastWidget;
end;

procedure TSimpleToastWidgetDemo.ShowDemo;
begin
  inherited ShowDemo;
end;

initialization
  TBootstrapButtonDemo.RegisterDemo;
  TSimpleToastWidgetDemo.RegisterDemo;
  TToastManagerDemo.RegisterDemo;
  TBootstrapModalDemo.Registerdemo;
end.

