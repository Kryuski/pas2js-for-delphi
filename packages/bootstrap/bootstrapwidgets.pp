{
    This file is part of the Pas2JS run time library.
    Copyright (C) 2019 Michael Van Canneyt

    extra Bootstrap widgets

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit bootstrapwidgets;

{$mode objfpc}

interface

uses
  Classes, SysUtils, js, libjquery, libbootstrap, web, webwidget, htmlwidgets;

Type

  { TSimpleToastWidget }
  TContextual = (cNone,cPrimary,cSecondary,cSuccess,cDanger,cWarning,cInfo,cLight,cDark);

  TSimpleToastWidget = Class(TCustomTemplateWidget)
  private
    FAnimate: Boolean;
    FAutoHide: Boolean;
    FBody: String;
    FBoolean: Boolean;
    FContextual: TContextual;
    FHeader: String;
    FHeaderImage: String;
    FHideDelay: Integer;
    FMinWidth: Integer;
    FSmallHeader: String;
    procedure SetAnimate(AValue: Boolean);
    procedure SetAutoHide(AValue: Boolean);
    procedure SetBody(AValue: String);
    procedure SetBoolean(AValue: Boolean);
    procedure SetContextual(AValue: TContextual);
    procedure SetHeader(AValue: String);
    procedure SetHeaderImage(AValue: String);
    procedure SetHideDelay(AValue: Integer);
    procedure SetMinWidth(AValue: Integer);
    procedure SetSmallHeader(AValue: String);
  Protected
    function BodyHTML: String; virtual;
    function CloseButtonHTML: String; virtual;
    function HeaderHTML: String; virtual;
    Function GetTemplateHTML: String; override;
    Function DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement; override;
  Public
    Constructor Create(aOwner : TComponent); override;
    Procedure Hide;
  Published
    Property Header : String Read FHeader Write SetHeader;
    Property SmallHeader : String Read FSmallHeader Write SetSmallHeader;
    Property Body : String Read FBody Write SetBody;
    Property HeaderImage : String Read FHeaderImage Write SetHeaderImage;
    Property CloseButton : Boolean Read FBoolean Write SetBoolean;
    Property Contextual : TContextual Read FContextual write SetContextual;
    Property HideDelay : Integer Read FHideDelay Write SetHideDelay default 2000;
    Property AutoHide : Boolean Read FAutoHide Write SetAutoHide default True;
    Property Animate : Boolean Read FAnimate Write SetAnimate default False;
    Property MinWidth : Integer Read FMinWidth Write SetMinWidth default 200;
  end;

  // Encapsulates the global tag where the toasts are shown.

  { TToastManager }

  TToastManager = Class(TWebWidget)
  Private
    FAnimate: Boolean;
    FAutoHide: Boolean;
    FHideDelay: Integer;
    FMinheight: Integer;
    FMinWidth: Integer;
    FMultiToast: Boolean;
    FContentElement : TJSHTMLElement;
    FToastIcon: String;
    class var
      _instance : TToastManager;
    procedure CheckInit;
    procedure SetMinHeight(AValue: Integer);
    procedure SetMultiToast(AValue: Boolean);
   Protected
    Class Function DefaultParentElement : TJSHTMLElement; override;
    Function DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement; override;
    Function GetContentElement: TJSHTMLELement; override;
    Procedure InvalidateElement; override;
    Function HTMLTag: String; override;
  Public
    Constructor Create(aOwner : TComponent); override;
    class function Instance : TToastManager;
    function ShowToast(const aHeader, aBody: String; aContext: TContextual=cNone; Closable: Boolean=True): TSimpleToastWidget;
  Published
    Property MultiToast : Boolean Read FMultiToast Write SetMultiToast;
    Property MinHeight : Integer Read FMinheight Write SetMinHeight default 250;
    Property ToastHideDelay : Integer Read FHideDelay Write FHideDelay default 2000;
    Property ToastAutoHide : Boolean Read FAutoHide Write FAutoHide default True;
    Property ToastAnimate : Boolean Read FAnimate Write FAnimate default False;
    Property ToastMinWidth : Integer Read FMinWidth Write FMinWidth default 200;
    Property ToastIcon : String Read FToastIcon Write FToastIcon;
  end;

  { TBootstrapButton }

  TBootstrapButton = class (TButtonWidget)
  private
    FContextual: TContextual;
    FOutLine: Boolean;
    procedure SetContextual(AValue: TContextual);
    procedure SetOutLine(AValue: Boolean);
  Protected
    Function RecalcClasses(aOldContextual : TContextual; aOldOutline : Boolean) : String;
  Public
    Constructor Create(aOwner : TComponent); override;
  Published
    Property Contextual : TContextual Read FContextual Write SetContextual default cPrimary;
    Property Outline : Boolean Read FOutLine Write SetOutLine;
  end;

  { TRowWidget }

  TRowWidget = Class(TDIVWidget)
  Protected
    Function WidgetClasses: String; override;
  end;

  { TColWidget }
  TColMediaSize = (cmsDefault,cmsXL,cmsLG,cmsMD,cmsSM);
  TColWidget = Class(TDIVWidget)
  private
    FSpan : Array[TColMediaSize] of Integer;
    function GetSpan(AIndex: Integer): Integer;
    procedure SetSpan(AIndex: Integer; AValue: Integer);
  Protected
    Function RecalcClasses(aRemoveSize : TColmediaSize;aRemoveValue : Integer) : String;
    Function WidgetClasses: String; override;
  Public
    Constructor Create(aOwner : TComponent); override;
  Published
    Property DefaultColSpan : Integer Index 0 Read GetSpan Write SetSpan;
    Property ExtraLargeColSpan : Integer Index 1 Read GetSpan Write SetSpan;
    Property LargeColSpan : Integer Index 2 Read GetSpan Write SetSpan;
    Property MediumColSpan : Integer Index 3 Read GetSpan Write SetSpan;
    Property SmallColSpan : Integer Index 4 Read GetSpan Write SetSpan;
  end;

  { TBootstrapModal }
  TOnModalHideEvent = Procedure (Sender : TObject; El : TJSHTMLElement; Values : TStrings) of object;

  TModalItemKind = (mikValue,mikClose);

  { TModalReferenceItem }

  TModalReferenceItem = Class(TReferenceItem)
  private
    FInitialValue: String;
    FKind: TModalItemKind;
  Protected
    Function GetValue: String;
    Procedure SetValue(aValue: String);
  Public
    Procedure Assign(Source : TPersistent); override;
    Property Value : String Read GetValue Write SetValue;
  Published
    Property Kind : TModalItemKind Read FKind Write FKind;
    Property InitialValue : String Read FInitialValue Write FInitialValue;
  end;

  { TModalReferences }

  TModalReferences = Class(TWebWidgetReferences)
  private
    function GetMR(aIndex : Integer): TModalReferenceItem;
    procedure SetMR(aIndex : Integer; AValue: TModalReferenceItem);
  Public
    Function Add(Const aName : String; aSelector : String; aKind : TModalItemKind) : TModalReferenceItem; overload;
    Property ModalRefs[aIndex : Integer] : TModalReferenceItem Read GetMR Write SetMR; default;
  end;

  TBootstrapModal = Class(TCustomTemplateWidget)
  private
    FHideEl : TJSHTMLElement;
    FBackDrop: Boolean;
    FFocus: Boolean;
    FKeyBoard: Boolean;
    FOnHide: TOnModalHideEvent;
    FShowOnRender: Boolean;
    FTemplate: String;
    FShowing : Boolean;
    function GetModalReferences: TModalReferences;
    function HideClick(Event: TJSEvent): Boolean;
    procedure SetModalReferences(AValue: TModalReferences);
    procedure SetTemplate(AValue: String);
  protected
    Function BootstrapHide(Event : TJSEvent) : Boolean;
    Function DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement; override;
    Function GetTemplateHTML: String; override;
    Procedure RefreshReferences; override;
    Function CreateReferences: TWebWidgetReferences; override;
  Public
    procedure GetValues(aList: TStrings);
    Procedure Show;
    Procedure Hide;
    Property Showing : Boolean Read FShowing;
  Published
    Property ShowOnRender: Boolean Read FShowOnRender Write FShowOnrender;
    Property BackDrop : Boolean Read FBackDrop Write FBackDrop;
    Property KeyBoard : Boolean Read FKeyBoard Write FKeyBoard;
    Property Focus : Boolean Read FFocus Write FFocus;
    Property Template : String Read FTemplate Write SetTemplate;
    Property OnHide : TOnModalHideEvent Read FOnHide Write FOnHide;
    Property References : TModalReferences Read GetModalReferences Write SetModalReferences;
  end;


Const
  ContextualNames : Array[TContextual] of string = ('','primary','secondary','success','danger','warning','info','light','dark');

Function Toasts : TToastManager;

Implementation

function Toasts: TToastManager;
begin
  Result:=TToastManager.Instance;
end;

{ TColWidget }

function TColWidget.GetSpan(AIndex: Integer): Integer;
begin
  Result:=FSpan[TColMediaSize(aIndex)];
end;

procedure TColWidget.SetSpan(AIndex: Integer; AValue: Integer);

Var
  aOld : integer;

begin
  if aValue=FSpan[TColMediaSize(aIndex)] then exit;
  aOld:=FSpan[TColMediaSize(aIndex)];
  FSpan[TColMediaSize(aIndex)]:=aValue;
  recalcClasses(TColMediaSize(aIndex),aOld);
end;

function TColWidget.RecalcClasses(aRemoveSize : TColmediaSize; aRemoveValue: Integer): String;

Const
  IdxNames : Array[TColMediaSize] of string = ('-','-xl-','-lg-','-md-','-sm-');

Var
  c : String;
  S : TColmediaSize;
begin
  Result:='';
  C:=RemoveClasses(Classes,' col-'+IdxNames[aRemoveSize]+IntToStr(aRemoveValue));
  For S in TColmediaSize do
    begin
    if FSpan[S]<>0 then
      Result:=Result+' col-'+IdxNames[S]+IntToStr(FSpan[S]);
    Classes:=AddClasses(C,Result);
    end;
end;

function TColWidget.WidgetClasses: String;
begin
  Result:='col '+inherited WidgetClasses;
end;

constructor TColWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  DefaultColSpan:=12;
end;

{ TRowWidget }

function TRowWidget.WidgetClasses: String;
begin
  Result:='row '+inherited WidgetClasses;
end;

{ TBootstrapButton }

procedure TBootstrapButton.SetContextual(AValue: TContextual);

Var
  Old : TContextual;

begin
  if FContextual=AValue then Exit;
  old:=FContextual;
  FContextual:=AValue;
  RecalcClasses(Old,FOutline);
end;

procedure TBootstrapButton.SetOutLine(AValue: Boolean);

Var
  Old : Boolean;

begin
  if FOutLine=AValue then Exit;
  old:=FoutLine;
  FOutLine:=AValue;
  RecalcClasses(FContextual,Old);
end;

function TBootstrapButton.RecalcClasses(aOldContextual: TContextual; aOldOutline: Boolean): String;

Const
  OL : Array[Boolean] of string = ('','outline-');
Var
  c : String;

begin
  Result:='btn btn-'+OL[FOutLine]+ContextualNames[FContextual];
  C:=RemoveClasses(Classes,'btn-'+OL[aOldOutLine]+ContextualNames[aOldContextual]);
  Classes:=AddClasses(C,Result);
end;

constructor TBootstrapButton.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  Contextual:=cPrimary;
end;


{ TToastManager }

class function TToastManager.Instance: TToastManager;
begin
  if _instance=Nil then
   _instance:=TToastManager.Create(Nil);
  Result:=_instance;
end;

procedure TToastManager.CheckInit;

begin
  if not IsRendered then
    Refresh;
end;

procedure TToastManager.SetMinHeight(AValue: Integer);
begin
  if FMinheight=AValue then Exit;
  FMinheight:=AValue;
  if IsRendered then
    Refresh;
end;

procedure TToastManager.SetMultiToast(AValue: Boolean);
begin
  if FMultiToast=AValue then Exit;
  FMultiToast:=AValue;
  if IsRendered then refresh;
end;

class function TToastManager.DefaultParentElement: TJSHTMLElement;
begin
  Result:=TJSHTMLElement(Document.body);
end;

function TToastManager.DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement;

Var
  El : TJSHTMLElement;

begin
  Result:=AElement;
  Result['aria-live']:='polite';
  Result['aria-atomic']:='true';
  Result['style']:='position: relative; min-height: '+IntToStr(MinHeight)+'px;';
  if not MultiToast then
    FContentElement:=Result
  else
    begin
    El:=CreateElement('div',ElementID+'-multi');
    El['style']:='position: absolute; top: 0; right: 0;';
    FContentElement:=El;
    Result.AppendChild(El);
    end;
end;

function TToastManager.GetContentElement: TJSHTMLELement;
begin
  Result:=FContentElement;
  if (Result=Nil) then
    Result:=Element;
end;

procedure TToastManager.InvalidateElement;
begin
  inherited;
  FContentElement:=nil;
end;


function TToastManager.HTMLTag: String;
begin
  Result:='div';
end;

constructor TToastManager.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FMinHeight:=250;
  FMinWidth:=200;
  FMultiToast:=True;
  FHideDelay:=2000;
  FAutoHide:=True;
  FAnimate:=False;
end;

function TToastManager.ShowToast(const aHeader, aBody: String; aContext : TContextual = cNone; Closable: Boolean = True): TSimpleToastWidget;
begin
  CheckInit;
  Result:=TSimpleToastWidget.Create(Self) ;
  With Result do
    begin
    Parent:=Self;
    Header:=aHeader;
    Body:=aBody;
    HeaderImage:=ToastIcon;
    CloseButton:=Closable;
    Contextual:=aContext;
    AutoHide:=ToastAutoHide;
    HideDelay:=ToastHideDelay;
    Animate:=ToastAnimate;
    MinWidth:=ToastMinWidth;
    Refresh;
    end;
end;

{ TSimpleToastWidget }

function TSimpleToastWidget.CloseButtonHTML: String;

Var
  S : String;

begin
  S:=ContextualNames[Contextual];
  if S<>'' then
    S:='text-'+S;
  Result:=Result+ '<button type="button" class="ml-2 mb-1 close '+S+'" data-dismiss="toast" aria-label="Close">';
  Result:=Result+ '   <span aria-hidden="true">&times;</span>';
  Result:=Result+ '</button>';
end;

function TSimpleToastWidget.HeaderHTML: String;

Var
  S : String;

begin
  S:=ContextualNames[Contextual];
  if S<>'' then
    S:='text-'+S;
  Result:='<div class="toast-header '+S+'">';
  if HeaderImage<>'' then
    Result:=Result+'<img src="'+HeaderImage+'" class="rounded mr-2">';
  Result:=Result+'<div class="mr-auto">'+Header+'</div>';
  if (SmallHeader<>'') then
    Result:=Result+'<small>'+SmallHeader+'</small>';
  if CloseButton then
    Result:=Result+CloseButtonHTML;
  Result:=Result+'</div>';
end;

procedure TSimpleToastWidget.SetBody(AValue: String);
begin
  if FBody=AValue then Exit;
  FBody:=AValue;
  if isRendered then Refresh;
end;

procedure TSimpleToastWidget.SetAnimate(AValue: Boolean);
begin
  if FAnimate=AValue then Exit;
  FAnimate:=AValue;
  if isRendered then Refresh;
end;

procedure TSimpleToastWidget.SetAutoHide(AValue: Boolean);
begin
  if FAutoHide=AValue then Exit;
  FAutoHide:=AValue;
  if isRendered then Refresh;
end;

procedure TSimpleToastWidget.SetBoolean(AValue: Boolean);
begin
  if FBoolean=AValue then Exit;
  FBoolean:=AValue;
  if isRendered then Refresh;
end;

procedure TSimpleToastWidget.SetContextual(AValue: TContextual);
begin
  if FContextual=AValue then Exit;
  FContextual:=AValue;
  if isRendered then Refresh;
end;

procedure TSimpleToastWidget.SetHeader(AValue: String);
begin
  if FHeader=AValue then Exit;
  FHeader:=AValue;
  if isRendered then Refresh;
end;

procedure TSimpleToastWidget.SetHeaderImage(AValue: String);
begin
  if FHeaderImage=AValue then Exit;
  FHeaderImage:=AValue;
  if isRendered then Refresh;
end;

procedure TSimpleToastWidget.SetHideDelay(AValue: Integer);
begin
  if FHideDelay=AValue then Exit;
  FHideDelay:=AValue;
  if isRendered then Refresh;
end;

procedure TSimpleToastWidget.SetMinWidth(AValue: Integer);
begin
  if FMinWidth=AValue then Exit;
  FMinWidth:=AValue;
  if isRendered then Refresh;
end;

procedure TSimpleToastWidget.SetSmallHeader(AValue: String);
begin
  if FSmallHeader=AValue then Exit;
  FSmallHeader:=AValue;
  if isRendered then Refresh;
end;

function TSimpleToastWidget.BodyHTML: String;

Var
  S : String;

begin
  S:=ContextualNames[Contextual];
  if S<>'' then
    S:='alert-'+S;
  Result:='<div class="toast-body '+S+'">';
  Result:=Result+Body;
  Result:=Result+'</div>';
end;

function TSimpleToastWidget.GetTemplateHTML: String;

Var
  Head : String;

begin
  Result:='<div class="toast" aria-live="assertive" aria atomic="true" style="min-width: '+IntToStr(MinWidth)+'px;">';
  Result:=Result+HeaderHTML;
  Result:=Result+BodyHTML;
  Result:=Result+'</div>';
end;

function TSimpleToastWidget.DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement;
begin
  Result:=inherited DoRenderHTML(aParent, aElement);
  JQuery(Result).toast(New(['animation',FAnimate,'autohide',autohide,'delay',FHideDelay]));
  JQuery(Result).ToastShow;
end;

constructor TSimpleToastWidget.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FMinWidth:=200;
  FAutoHide:=True;
  FHideDelay:=2000;
end;

procedure TSimpleToastWidget.Hide;
begin
  JQuery(Element).Toast('hide');
end;

{ TModalReferenceItem }

procedure TModalReferenceItem.Assign(Source: TPersistent);
begin
  if Source is TModalReferenceItem then
    Self.Kind:=TModalReferenceItem(Source).Kind
  else
    inherited Assign(Source);
end;

function TModalReferenceItem.GetValue: String;
begin
  if (Kind=mikValue) and (Element<>Nil) then
    Result:=TJSHTMLInputElement(Element).value;
end;

procedure TModalReferenceItem.SetValue(aValue: String);
begin
  if (Kind=mikValue) and (Element<>Nil) then
    TJSHTMLInputElement(Element).value:=aValue;
end;

{ TModalReferences }

function TModalReferences.GetMR(aIndex : Integer): TModalReferenceItem;
begin
  Result:=TModalReferenceItem(Items[aIndex])
end;

procedure TModalReferences.SetMR(aIndex : Integer; AValue: TModalReferenceItem);
begin
  Items[aIndex]:=aValue;
end;

function TModalReferences.Add(const aName: String; aSelector: String; aKind: TModalItemKind): TModalReferenceItem;
begin
  Result:=TModalReferenceItem(Inherited Add(aName,aselector));
  Result.Kind:=aKind;
end;

{ TBootstrapModal }

procedure TBootstrapModal.Hide;

begin
  if Assigned(Element) then
    jQuery(Element).ModalHide;
end;

function TBootstrapModal.HideClick (Event : TJSEvent): Boolean;

begin
  // Writeln('In hide click');
  FHideEl:=TJSHtmlElement(Event.targetElement);
  Hide;
end;

function TBootstrapModal.GetModalReferences: TModalReferences;
begin
  Result:=TModalReferences(Inherited References);
end;

procedure TBootstrapModal.SetModalReferences(AValue: TModalReferences);
begin
  References.Assign(aValue);
end;

procedure TBootstrapModal.SetTemplate(AValue: String);
begin
  if FTemplate=AValue then Exit;
  if Showing Then
    Raise EWidgets.Create('Cannot set template while showing bootstrap modal');
  FTemplate:=AValue;
  If IsRendered then
    Refresh;
end;

procedure TBootstrapModal.GetValues(aList: TStrings);

Var
  I : integer;
  Itm : TModalReferenceItem;

begin
  For I:=0 to references.Count-1 do
    begin
    Itm:=references[i];
    if (itm.Kind=mikValue) and assigned(itm.Element) then
      aList.Values[Itm.Name]:=Itm.Value
    end;
end;


function TBootstrapModal.BootstrapHide(Event: TJSEvent): Boolean;

Var
  L : Tstrings;
  I,C : integer;

begin
  Result:=False;
  FShowing:=False;
  // Writeln('In bootstraphide callback ', assigned(onhide));
  If Assigned(OnHide) then
    begin
    C:=0;
    L:=Nil;
    For I:=0 to references.Count-1 do
      if references[i].Kind=mikValue then Inc(C);
    if C>0 then
      begin
      L:=TStringList.Create;
      GetValues(L);
      end;
    Try
      OnHide(Self,FHideEl,L);
    finally
      L.Free;
    end;
    end;
end;

function TBootstrapModal.DoRenderHTML(aParent, aElement: TJSHTMLElement): TJSHTMLElement;

begin
  FHideEl:=Nil;
  Result:=inherited DoRenderHTML(aParent, aElement);
  JQuery(Result).modal(New([
    'backdrop', BackDrop,
    'keyboard', keyboard,
    'focus',Focus,
    'show',ShowOnRender
  ]));
  jQuery(Result).on_('hidden.bs.modal',@BootstrapHide);
end;

function TBootstrapModal.GetTemplateHTML: String;
begin
  Result:=FTemplate;
end;

procedure TBootstrapModal.RefreshReferences;

Var
  I : Integer;
  E : TJSHTMLElement;
  MR : TModalReferenceItem;

begin
  inherited RefreshReferences;
  E:=References.FindElementByName('OK');
  if (E<>Nil) then
    jQuery(E).on_('click',@HideClick);
  for I:=0 to References.Count-1 do
    begin
    MR:=References[i];
    if (MR.Kind=mikClose) then
      begin
      if MR.Exists then
        if Not MR.IsArray then
          jQuery(MR.Element).on_('click',@HideClick)
        else
          For E in MR.Elements do
            jQuery(E).on_('click',@HideClick);
        end
    else if (MR.Kind=mikValue) then
      begin
      if (MR.element<>Nil) and (MR.InitialValue<>'') then
        MR.Value:=MR.InitialValue;
      end;
    end;
end;

function TBootstrapModal.CreateReferences: TWebWidgetReferences;
begin
  Result:=TModalReferences.Create(Self,TModalReferenceItem);
end;

procedure TBootstrapModal.Show;
begin
  FHideEl:=Nil;
  if not IsRendered then
    Refresh;
  JQuery(Element).ModalShow;
  FShowing:=True;
end;


end.
