program ReadWrite;

uses
  Vcl.Forms,
  readWrite1 in 'readWrite1.pas' {frmReadWrite};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmReadWrite, frmReadWrite);
  Application.Run;
end.
