program ParallelJoin;

uses
  Vcl.Forms,
  ParallelJoinMain in 'ParallelJoinMain.pas' {frmParallelJoin};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmParallelJoin, frmParallelJoin);
  Application.Run;
end.
