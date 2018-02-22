program LoggingMemoryManager;

uses
  Vcl.Forms,
  LoggingMMMain in 'LoggingMMMain.pas' {frmLoggingMM},
  LoggingMM in 'LoggingMM.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmLoggingMM, frmLoggingMM);
  Application.Run;
end.
