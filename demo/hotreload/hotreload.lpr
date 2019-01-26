program hotreload;

uses sysutils,js,web, hotreloadclient;

Type

  { TForm }

  TForm = Class
    Button1 : TJSElement;
    Edit : TJSHTMLInputElement;
    FLastReq : TJSXMLHttpRequest;
    function RecompileClick(Event: TJSMouseEvent): boolean;
    Constructor create; reintroduce;
  private
    function DoReloadChange(Event: TEventListenerEvent): boolean;
    function doResponse(Event: TEventListenerEvent): boolean;
  end;

function TForm.doResponse(Event: TEventListenerEvent): boolean;

Var
  Data : TJSObject;
  S : String;

begin
  Result:=True;
  console.warn('Compile request response received');
  try
     Data:=TJSJSON.parseObject(FLastReq.responseText);
     if data['success'] then
       begin
       if isInteger(data['compileID']) then
         begin
         S:=String(data['compileID']);
         Edit.value:=S;
         end;
       end
     else
       Edit.value:=''
  except
    console.error('Compile request response received non-json response: '+FLastReq.ResponseText);
  end;
  FLastReq:=nil;
end;

function TForm.RecompileClick(Event: TJSMouseEvent): boolean;

Var
  Req : TJSXMLHttpRequest;

begin
   Result:=True;
   console.info('Recompile requested');
   Req:=TJSXMLHttpRequest.new;
   Req.addEventListener('load',@DoResponse);
   Req.open('POST','$sys/compile');
   Req.send;
   FLastReq:=Req;
end;

constructor TForm.Create;

Var
  D,Panel,PanelContent : TJSElement;
  CB : TJSHTMLInputElement;

begin
   Panel:=document.createElement('div');
   // attrs are default array property...
   Panel['class']:='panel panel-default';
   PanelContent:=document.createElement('div');
   PanelContent['class']:='panel-body';
   Button1:=document.createElement('input');
   Button1['id']:='Button1';
   Button1['type']:='submit';
   Button1['class']:='btn btn-default';
   Button1['value']:='Recompile';
   TJSHTMLElement(Button1).onclick:=@RecompileClick;
   document.body.appendChild(Panel);
   Edit:=TJSHTMLInputElement(document.createElement('input'));
   Edit['id']:='recompileID';
   Edit.readOnly:=true;
   cb:=TJSHTMLInputElement(document.createElement('input'));
   cb['id']:='autoreload';
   cb['type']:='checkbox';
   cb.onchange:=@DoReloadChange;
   cb.checked:=THotReload.getglobal.options.Reload;
   Panel.appendChild(PanelContent);
   PanelContent.appendChild(Button1);
   D:=Document.createElement('span');
   D.id:='buildlabel';
   D.appendChild(Document.createTextNode('Build ID'));
   PanelContent.appendChild(D);
   PanelContent.appendChild(Edit);
   PanelContent.appendChild(cb);
   D:=Document.createElement('span');
   D.id:='reloadhint';
   D.appendChild(Document.createTextNode('Reload page on build/sync'));
   PanelContent.appendChild(D);
end;

function TForm.DoReloadChange(Event: TEventListenerEvent): boolean;
begin
  Result:=True;
  THotReload.getglobal.options.Reload:=not THotReload.getglobal.options.Reload;
end;

begin
  THotReload.StartHotReload;
  TForm.Create;

//  THotReload.getglobal.options.Path:='status.json';
//  THotReload.getglobal.options.log:=True;
end.

