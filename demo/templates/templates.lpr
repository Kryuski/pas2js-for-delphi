program templates;

{$mode objfpc}

uses
  browserconsole, JS, Classes, SysUtils, Web, Rtl.TemplateLoader, browserapp;

TYpe

  { TMyApp }

  TMyApp = Class(TBrowserApplication)
    FLoader : TTemplateLoader;
    procedure DoRUn; override;
  private
    procedure DoGlobalFail(Sender: TObject; const aTemplate, aError: String; aErrorcode: Integer);
    procedure DoGlobalLoaded(Sender: TObject; const aTemplate: String);
    procedure DoLocalFail(Sender: TObject; const aTemplate, aError: String; aErrorcode: Integer);
    procedure DoLocalLoaded(Sender: TObject; const aTemplate: String);
  end;

{ TMyApp }

procedure TMyApp.DoRUn;
begin
  FLoader:=TTemplateLoader.Create(Self);
  FLoader.OnLoad:=@DoGlobalLoaded;
  FLoader.OnLoadFail:=@DoGlobalFail;
  Floader.BaseURL:='templates/';
  FLoader.LoadTemplate('this','this.txt');
  FLoader.LoadTemplate('thistoo','thistoo.txt',@DoLocalLoaded,@DoLocalFail);
  FLoader.LoadTemplate('thisnot','thisnot.txt',@DoLocalLoaded,@DoLocalFail);
  FLoader.LoadTemplates(['one','this.txt','two','thistoo.txt','threenot','thisalsonot.txt'],@DoLocalLoaded,@DoLocalFail);
end;

procedure TMyApp.DoGlobalFail(Sender: TObject; const aTemplate, aError: String; aErrorcode: Integer);
begin
  Writeln('GLobal fail load for : ',aTemplate,' Error: ',aError,' Code : ',aErrorCode);
end;

procedure TMyApp.DoGlobalLoaded(Sender: TObject; const aTemplate: String);
begin
  Writeln('GLobal load OK: ',aTemplate,' Template text ',Floader.Templates[aTemplate]);
end;

procedure TMyApp.DoLocalFail(Sender: TObject; const aTemplate, aError: String; aErrorcode: Integer);
begin
  Writeln('Local fail load for : ',aTemplate,' Error: ',aError,' Code : ',aErrorCode);
end;

procedure TMyApp.DoLocalLoaded(Sender: TObject; const aTemplate: String);
begin
  Writeln('Local load OK: ',aTemplate,' Template text ',Floader.Templates[aTemplate]);
end;


begin
  With TMyApp.Create(Nil) do
    begin
    Initialize;
    run;
    end;
end.
