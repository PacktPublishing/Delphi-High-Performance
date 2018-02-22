program ParallelPipeline;

uses
  Vcl.Forms,
  ParallelPipelineMain in 'ParallelPipelineMain.pas' {frmPipeline};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmPipeline, frmPipeline);
  Application.Run;
end.
