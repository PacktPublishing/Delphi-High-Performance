unit FibonacciMain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin;

type
  TFiboFunc = function (element: int64): int64 of object;

  TfrmFibonacci = class(TForm)
    Label1: TLabel;
    SpinEdit1: TSpinEdit;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    FFibonacciTable: TArray<int64>;
    function FibonacciRecursiveMemoized(element: int64): int64;
    function FibonacciIterative(element: int64): int64;
    function FibonacciMemoized(element: int64): int64;
    function FibonacciRecursive(element: int64): int64;
    procedure Time(fibonacci: TFiboFunc);
  public
  end;

var
  frmFibonacci: TfrmFibonacci;

implementation

uses
  System.Diagnostics;

{$R *.dfm}

procedure TfrmFibonacci.Button1Click(Sender: TObject);
begin
  Time(FibonacciRecursive);
end;

procedure TfrmFibonacci.Button2Click(Sender: TObject);
begin
  Time(FibonacciMemoized);
end;

procedure TfrmFibonacci.Button3Click(Sender: TObject);
begin
  Time(FibonacciIterative);
end;

function TfrmFibonacci.FibonacciIterative(element: int64): int64;
var
  a,b: int64;
begin
  a := 1;
  b := 0;
  repeat
    if element = 1 then
      Exit(a);
    b := b + a;
    if element = 2 then
      Exit(b);
    a := a + b;
    Dec(element, 2);
  until false;
end;

function TfrmFibonacci.FibonacciRecursiveMemoized(element: int64): int64;
begin
  if FFibonacciTable[element] >= 0 then
    Result := FFibonacciTable[element]
  else
  begin
    if element < 3 then
      Result := 1
    else
      Result := FibonacciRecursiveMemoized(element - 1) + FibonacciRecursiveMemoized(element - 2);
    FFibonacciTable[element] := Result;
  end;
end;

function TfrmFibonacci.FibonacciMemoized(element: int64): int64;
var
  i: Integer;
begin
  SetLength(FFibonacciTable, element+1);
  for i := Low(FFibonacciTable) to High(FFibonacciTable) do
    FFibonacciTable[i] := -1;
  Result := FibonacciRecursiveMemoized(element);
end;

function TfrmFibonacci.FibonacciRecursive(element: int64): int64;
begin
  if element < 3 then
    Result := 1
  else
    Result := FibonacciRecursive(element - 1) + FibonacciRecursive(element - 2);
end;

procedure TfrmFibonacci.Time(fibonacci: TFiboFunc);
var
  element: Integer;
  stopwatch: TStopwatch;
  value: int64;
begin
  element := SpinEdit1.Value;
  stopwatch := TStopwatch.StartNew;
  value := fibonacci(element);
  stopwatch.Stop;
  ListBox1.ItemIndex := ListBox1.Items.Add(Format('fib(%d) = %d in %d ms',
    [element, value, stopwatch.ElapsedMilliseconds]));
end;

end.
