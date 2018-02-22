program BeginUpdateFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  BeginUpdateFMXMain in 'BeginUpdateFMXMain.pas' {frmBeginUpdate};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmBeginUpdate, frmBeginUpdate);
  Application.Run;
end.
