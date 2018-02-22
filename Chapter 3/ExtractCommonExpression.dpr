program ExtractCommonExpression;

uses
  Vcl.Forms,
  ExtractCommonExpressionMain in 'ExtractCommonExpressionMain.pas' {frmCommonExpression};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmCommonExpression, frmCommonExpression);
  Application.Run;
end.
