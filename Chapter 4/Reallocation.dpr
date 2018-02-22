program Reallocation;

uses
  Vcl.Forms,
  ReallocationMain in 'ReallocationMain.pas' {frmReallocation};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmReallocation, frmReallocation);
  Application.Run;
end.
