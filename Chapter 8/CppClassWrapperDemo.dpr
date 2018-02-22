program CppClassWrapperDemo;

uses
  Vcl.Forms,
  CppClasswrapperMain in 'CppClasswrapperMain.pas' {frmCppClassDemo},
  CppClassImport in 'CppClassImport.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmCppClassDemo, frmCppClassDemo);
  Application.Run;
end.
