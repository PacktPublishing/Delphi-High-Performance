program CompilerOptions;

uses
  Vcl.Forms,
  CompilerOptionsMain in 'CompilerOptionsMain.pas' {frmCompilerOptions};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmCompilerOptions, frmCompilerOptions);
  Application.Run;
end.
