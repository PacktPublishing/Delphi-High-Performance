program ParameterPassing;

uses
  Vcl.Forms,
  ParameterPassingMain in 'ParameterPassingMain.pas' {frmParamPassing};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmParamPassing, frmParamPassing);
  Application.Run;
end.
