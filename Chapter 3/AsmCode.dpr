program AsmCode;

uses
  Vcl.Forms,
  AsmCodeMain in 'AsmCodeMain.pas' {frmAsmCode};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmAsmCode, frmAsmCode);
  Application.Run;
end.
