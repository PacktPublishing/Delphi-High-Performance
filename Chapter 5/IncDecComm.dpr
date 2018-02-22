program IncDecComm;

uses
  Vcl.Forms,
  IncDecCommMain in 'IncDecCommMain.pas' {frmIncDecComm};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmIncDecComm, frmIncDecComm);
  Application.Run;
end.
