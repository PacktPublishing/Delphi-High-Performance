program IncDec;

uses
  Vcl.Forms,
  incDec1 in 'incDec1.pas' {frmIncDec};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmIncDec, frmIncDec);
  Application.Run;
end.
