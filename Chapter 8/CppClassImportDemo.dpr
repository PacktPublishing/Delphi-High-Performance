program CppClassImportDemo;

uses
  Vcl.Forms,
  CppClassMain in 'CppClassMain.pas' {frmCppClassDemo},
  CppClassImport in 'CppClassImport.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmCppClassDemo, frmCppClassDemo);
  Application.Run;
end.
