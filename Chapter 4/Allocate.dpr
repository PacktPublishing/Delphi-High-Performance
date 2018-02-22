program Allocate;

uses
  Vcl.Forms,
  AllocateMain in 'AllocateMain.pas' {frmAllocate};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmAllocate, frmAllocate);
  Application.Run;
end.
