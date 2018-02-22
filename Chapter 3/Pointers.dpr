program Pointers;

uses
  Vcl.Forms,
  PointersMain in 'PointersMain.pas' {frmPointers};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmPointers, frmPointers);
  Application.Run;
end.
