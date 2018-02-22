program SlowCode_VCL_Instrumented;

uses
  Vcl.Forms,
  SlowCode_VCL_main_instrumented in 'SlowCode_VCL_main_instrumented.pas' {frmSlowCode};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSlowCode, frmSlowCode);
  Application.Run;
end.
