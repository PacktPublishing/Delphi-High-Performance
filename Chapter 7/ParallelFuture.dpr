program ParallelFuture;

uses
  Vcl.Forms,
  ParallelFutureMain in 'ParallelFutureMain.pas' {frmFuture};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmFuture, frmFuture);
  Application.Run;
end.
