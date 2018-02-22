program Fibonacci;

uses
  Vcl.Forms,
  FibonacciMain in 'FibonacciMain.pas' {frmFibonacci};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmFibonacci, frmFibonacci);
  Application.Run;
end.
