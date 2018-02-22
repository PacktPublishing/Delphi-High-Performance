program RandomWordSearch;

uses
  Vcl.Forms,
  RandomWordSearch1 in 'RandomWordSearch1.pas' {frmRandomWordSearch};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmRandomWordSearch, frmRandomWordSearch);
  Application.Run;
end.
