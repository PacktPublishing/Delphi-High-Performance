program DataTypes;

uses
  Vcl.Forms,
  DataTypesMain in 'DataTypesMain.pas' {frmDataTypes};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmDataTypes, frmDataTypes);
  Application.Run;
end.
