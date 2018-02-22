program ParallelTasks;

uses
  Vcl.Forms,
  ParallelTasksMain in 'ParallelTasksMain.pas' {frmParallelTasks};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmParallelTasks, frmParallelTasks);
  Application.Run;
end.
