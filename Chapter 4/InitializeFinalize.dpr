program InitializeFinalize;

uses
  Vcl.Forms,
  InitializeFinalizeMain in 'InitializeFinalizeMain.pas' {frmInitFin};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmInitFin, frmInitFin);
  Application.Run;
end.
