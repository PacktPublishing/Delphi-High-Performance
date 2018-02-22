program CacheDemo;

uses
  Vcl.Forms,
  CacheDemoMain in 'CacheDemoMain.pas' {frmCacheTest},
  DHPCache in 'DHPCache.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmCacheTest, frmCacheTest);
  Application.Run;
end.
