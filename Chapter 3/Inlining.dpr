program Inlining;

uses
  Vcl.Forms,
  InliningMain in 'InliningMain.pas' {frmInlining};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmInlining, frmInlining);
  Application.Run;
end.
