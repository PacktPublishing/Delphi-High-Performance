program Tasks;

uses
  Vcl.Forms,
  TasksMain in 'TasksMain.pas' {frmTasks};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmTasks, frmTasks);
  Application.Run;
end.
