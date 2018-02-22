program SlowCode_VCL;

uses
  Vcl.Forms,
  SlowCode_VCL_main in 'SlowCode_VCL_main.pas' {frmSlowCode};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSlowCode, frmSlowCode);
  Application.Run;
end.
