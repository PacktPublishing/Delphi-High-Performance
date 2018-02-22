program ProgressBar;

uses
  Vcl.Forms,
  ProgressBarMain in 'ProgressBarMain.pas' {frmProgressBar};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmProgressBar, frmProgressBar);
  Application.Run;
end.
