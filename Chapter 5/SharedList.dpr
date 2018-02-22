program SharedList;

uses
  Vcl.Forms,
  SharedListMain in 'SharedListMain.pas' {frmSharedList};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmSharedList, frmSharedList);
  Application.Run;
end.
