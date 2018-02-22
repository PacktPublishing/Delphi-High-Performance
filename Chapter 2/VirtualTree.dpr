program VirtualTree;

uses
  Vcl.Forms,
  VirtualTreeMain in 'VirtualTreeMain.pas' {frmVTV};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmVTV, frmVTV);
  Application.Run;
end.
