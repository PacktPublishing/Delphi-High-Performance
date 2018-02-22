program Deadlock;

uses
  Vcl.Forms,
  deadlock1 in 'deadlock1.pas' {frmDeadlock};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmDeadlock, frmDeadlock);
  Application.Run;
end.
