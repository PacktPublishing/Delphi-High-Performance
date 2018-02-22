program ParallelPaint;

uses
  Vcl.Forms,
  ParallelPaintMain in 'ParallelPaintMain.pas' {frmParallelPaint};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmParallelPaint, frmParallelPaint);
  Application.Run;
end.
