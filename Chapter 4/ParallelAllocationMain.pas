unit ParallelAllocationMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin;

type
  TfrmParallelAllocation = class(TForm)
    btnRunTest: TButton;
    ListBox1: TListBox;
    procedure btnRunTestClick(Sender: TObject);
  private
    function RunTest(parallel: boolean): int64;
  public
  end;

var
  frmParallelAllocation: TfrmParallelAllocation;

implementation

{$R *.dfm}

uses
  System.Threading,
  System.Diagnostics,
  Generics.Collections;

type
  TFoo = class
  private
    fValue: Double;
  public
    constructor create;
    property Value: double read fValue;
  end;

  TFooList = class
  private
    fList: TObjectList<TFoo>;
    fSize: integer;
    fValue: double;
  public
    constructor Create;
    destructor Destroy; override;

    function Execute: double;

    property Size: integer read fSize;
    property Value: double read fValue;
  end;

  TSpeedTest = class
  private
    fIterations: integer;
    fList: TObjectList<TFooList>;
    fNumWorkers: integer;
  strict protected
  public
    constructor create(aIterations: integer);
    destructor Destroy; override;

    function Execute(parallelTest: boolean): integer;

    property Iterations: integer read fIterations write fIterations;
  end;

procedure TfrmParallelAllocation.btnRunTestClick(Sender: TObject);
var
  durationSingle: int64;
  durationMulti: int64;
begin
  btnRunTest.Enabled := false;
  btnRunTest.Update;
  Screen.Cursor := crHourGlass;

  durationSingle := RunTest(false);
  durationMulti := RunTest(true);

  btnRunTest.Enabled := true;
  Screen.Cursor := crDefault;

  ListBox1.Items.Add(Format(
    'Single threaded: %d; Multi-threaded: %d (%.1fx faster)',
    [durationSingle, durationMulti, durationSingle/durationMulti]));
end;

{ TSpeedTest }

constructor TSpeedTest.create(aIterations: integer);
var
  aFooList: TFooList;
  i: Integer;
begin
  inherited create;

  fList := TObjectList<TFooList>.Create;
  fNumWorkers := 1;

  fIterations := aIterations;
  for i := 0 to fIterations - 1 do
  begin
    aFooList := TFooList.create;
    fList.Add(aFooList)
  end;
end;

destructor TSpeedTest.Destroy;
begin
  fList.Free;
  inherited;
end;

function TSpeedTest.Execute(parallelTest: boolean): integer;
var
  i: Integer;
  sw: TStopwatch;
begin

  sw := TStopwatch.Create;
  sw.Reset;
  sw.Start;

  if parallelTest then
    TParallel.For(0, fList.Count - 1,
      procedure(i: integer)
      begin
        fList[i].Execute;
      end)

  else
    for i := 0 to fList.Count - 1 do
      fList[i].Execute;

  sw.Stop;
  result := sw.ElapsedMilliseconds;

end;

{ TFooList }

constructor TFooList.Create;
begin
  inherited create;
  fSize := 12000;
  fList := TObjectList<TFoo>.Create;
  fList.Capacity := 10;
end;

destructor TFooList.Destroy;
begin
  fList.Free;
  inherited;
end;

function TFooList.Execute: double;
var
  aFoo: TFoo;
  i: Integer;
begin

  for i := 0 to Size - 1 do
  begin
    aFoo := TFoo.create;
    fList.Add(aFoo);
  end;

  result := 0;
  while fList.Count > 0 do
  begin
    fValue := fList[0].Value;
    result := fList[0].Value;
    fList.Delete(0);
  end;

end;

{ TFoo }

constructor TFoo.create;
begin
  inherited create;
  fValue := Random;
end;

function TfrmParallelAllocation.RunTest(parallel: boolean): int64;
var
  SpeedTest: TSpeedTest;
begin
  SpeedTest := TSpeedTest.create(100);
  Result := SpeedTest.Execute(parallel);
  SpeedTest.Free;
end;

end.
