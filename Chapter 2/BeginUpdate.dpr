program BeginUpdate;

uses
  Vcl.Forms,
  BeginUpdateMain in 'BeginUpdateMain.pas' {frmBeginUpdate};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmBeginUpdate, frmBeginUpdate);
  Application.Run;
end.
