program AnonMethod;

uses
  Vcl.Forms,
  AnonMethodMain in 'AnonMethodMain.pas' {frmAnonMthod};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmAnonMthod, frmAnonMthod);
  Application.Run;
end.
