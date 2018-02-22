program Threads;

uses
  Vcl.Forms,
  ThreadsMain in 'ThreadsMain.pas' {frmThreads};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmThreads, frmThreads);
  Application.Run;
end.
