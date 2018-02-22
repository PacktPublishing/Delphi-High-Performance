unit CompilerOptionsMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmCompilerOptions = class(TForm)
    btnRangeChecking: TButton;
    ListBox1: TListBox;
    btnOptimization: TButton;
    btnRangeError: TButton;
    btnOverflowChecking: TButton;
    btnOverflowError: TButton;
    btnAlignment: TButton;
    Button1: TButton;
    procedure btnRangeCheckingClick(Sender: TObject);
    procedure btnOptimizationClick(Sender: TObject);
    procedure btnRangeErrorClick(Sender: TObject);
    procedure btnOverflowCheckingClick(Sender: TObject);
    procedure btnOverflowErrorClick(Sender: TObject);
    procedure btnAlignmentClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
  public
  end;

var
  frmCompilerOptions: TfrmCompilerOptions;

implementation

uses
  System.Diagnostics,
  System.Generics.Collections;

type
  {$A1}
  TRecA1 = record
    Field1: byte;
    Field2: int64;
    Field3: word;
    Field4: double;
  end;
  {$A2}
  TRecA2 = record
    Field1: byte;
    Field2: int64;
    Field3: word;
    Field4: double;
  end;
  {$A4}
  TRecA4 = record
    Field1: byte;
    Field2: int64;
    Field3: word;
    Field4: double;
  end;
  {$A8}
  TRecA8 = record
    Field1: byte;
    Field2: int64;
    Field3: word;
    Field4: double;
  end;
  {$A8}


{$R *.dfm}

procedure NestedCall;
begin
  // this code is compiled with $R+ or $R-, depending on project settings
end;

procedure Example;
begin
  // this code is compiled with $R+ or $R-, depending on project settings

  {$IFOPT R+}{$DEFINE RANGECHECKING}{$ELSE}{$UNDEF RANGECHECKING}{$ENDIF}

  {$R+}
  // this code is compiled with $R+
  NestedCall;

  {$IFDEF RANGECHECKING}{$R+}{$ELSE}{$R-}{$ENDIF}

  // this code is compiled with $R+ or $R-, depending on project settings
end;

{$IFOPT Q+}{$DEFINE OVERFLOWCHECK}{$ELSE}{$UNDEF OVERFLOWCHECK}{$ENDIF}
procedure TfrmCompilerOptions.btnOverflowCheckingClick(Sender: TObject);
var
  i,j: Integer;
  sw: TStopwatch;
  overOn, overOff: int64;
begin
  {$OVERFLOWCHECKS ON}
  sw := TStopwatch.StartNew;

  j := 0;
  for i := 1 to $FFFFFFF do
    j := j + 1;

  overOn := sw.ElapsedMilliseconds;

  {$OVERFLOWCHECKS OFF}
  sw := TStopwatch.StartNew;

  j := 0;
  for i := 1 to $FFFFFFF do
    j := j + 1;

  overOff := sw.ElapsedMilliseconds;

  ListBox1.Items.Add(Format('With overflow checking: %d ms, ' +
    'Without overflow checking: %d ms', [overOn, overOff]));
end;

procedure TfrmCompilerOptions.btnOverflowErrorClick(Sender: TObject);
var
  i: cardinal;
begin
  {$Q-}
  i := $FFFFFFFF;
  // Without overflow checks, Inc will work and i will be 0
  Inc(i);
  {$Q+}
  i := $FFFFFFFF;
  // With overflow checks, Inc will raise an exception
  Inc(i);
end;
{$IFDEF OVERFLOWCHECK}{$Q+}{$ELSE}{$Q-}{$ENDIF}

procedure TfrmCompilerOptions.btnRangeCheckingClick(Sender: TObject);
var
  arr1: array [1..50000] of Integer;
  arr2: array [1..50000] of Integer;
  sw: TStopwatch;
  i,j: Integer;
  rangeOn,rangeOff: int64;
begin
  {$IFOPT R+}{$DEFINE RANGECHECKING}{$ELSE}{$UNDEF RANGECHECKING}{$ENDIF}
  {$OPTIMIZATION ON}

  sw := TStopwatch.StartNew;

  {$RANGECHECKS ON}
  for j := 1 to 1000 do
  begin
    for i := Low(arr1) to High(arr1) do
      arr1[i] := i;
    for i := Low(arr1) to High(arr1)-1 do
      arr1[i] := arr1[i] + arr1[i+1];
  end;
  {$IFDEF RANGECHECKING}{$R+}{$ELSE}{$R-}{$ENDIF}

  rangeOn := sw.ElapsedMilliseconds;

  sw := TStopwatch.StartNew;

  {$RANGECHECKS OFF}
  for j := 1 to 1000 do
  begin
    for i := Low(arr2) to High(arr2) do
      arr2[i] := i;
    for i := Low(arr2) to High(arr2)-1 do
      arr2[i] := arr2[i] + arr2[i+1];
  end;
  {$IFDEF RANGECHECKING}{$R+}{$ELSE}{$R-}{$ENDIF}

  rangeOff := sw.ElapsedMilliseconds;

  ListBox1.Items.Add(Format('With range checking: %d ms, ' +
    'Without range checking: %d ms', [rangeOn, rangeOff]));
end;

{$IFOPT O+}{$DEFINE OPTIMIZATION}{$ELSE}{$UNDEF OPTIMIZATION}{$ENDIF}
{$OPTIMIZATION ON}
function Optimized: int64;
var
  arr1: array [1..50000] of Integer;
  sw: TStopwatch;
  i,j: Integer;
begin
  sw := TStopwatch.StartNew;

  for j := 1 to 1000 do
  begin
    for i := Low(arr1) to High(arr1) do
      arr1[i] := i;
    for i := Low(arr1) to High(arr1)-1 do
      arr1[i] := arr1[i] + arr1[i+1];
  end;

  Result:= sw.ElapsedMilliseconds;
end;
{$OPTIMIZATION OFF}
function NonOptimized: int64;
var
  arr1: array [1..50000] of Integer;
  sw: TStopwatch;
  i,j: Integer;
begin
  sw := TStopwatch.StartNew;

  for j := 1 to 1000 do
  begin
    for i := Low(arr1) to High(arr1) do
      arr1[i] := i;
    for i := Low(arr1) to High(arr1)-1 do
      arr1[i] := arr1[i] + arr1[i+1];
  end;

  Result:= sw.ElapsedMilliseconds;
end;
{$IFDEF OPTIMIZATION}{$O+}{$ELSE}{$O-}{$ENDIF}

procedure TfrmCompilerOptions.btnOptimizationClick(Sender: TObject); {$Q-}
begin
  ListBox1.Items.Add(Format('With optimization: %d ms, ' +
    'Without optimization: %d ms', [Optimized, NonOptimized]));
end;

procedure TfrmCompilerOptions.btnRangeErrorClick(Sender: TObject);
var
  arr: array [1..100] of Integer;
  i: Integer;
begin
  for i := Low(arr) to High(arr) do
    arr[i] := i;

  {$R-} // the code will work although it accesses arr[101]
  for i := Low(arr) to High(arr) do
    arr[i] := arr[i] + arr[i+1];

  {$R+} // the code will raise an exception while accessing arr[101]
  for i := Low(arr) to High(arr) do
    arr[i] := arr[i] + arr[i+1];

  {$IFDEF RANGECHECKING}{$R+}{$ELSE}{$R-}{$ENDIF}
end;

{$RANGECHECKS OFF}
{$OPTIMIZATION OFF}

function Align1: int64;
var
  arr: TArray<TRecA1>;
  i: Integer;
  sw: TStopwatch;
  rep: Integer;
begin
  SetLength(arr, 10000000);

  sw := TStopwatch.StartNew;

  for i := Low(arr) to High(arr) do begin
    arr[i].Field2 := i;
    arr[i].Field4 := i;
  end;

  for rep := 1 to 10 do
  for i := Low(arr) to High(arr)-1 do begin
    arr[i].Field2 := 3*arr[i+1].Field2;
    arr[i].Field4 := 5*arr[i+1].Field4;
  end;

  Result := sw.ElapsedMilliseconds;
end;

function Align2: int64;
var
  arr: TArray<TRecA2>;
  i: Integer;
  sw: TStopwatch;
  rep: Integer;
begin
  SetLength(arr, 10000000);

  sw := TStopwatch.StartNew;

  for i := Low(arr) to High(arr) do begin
    arr[i].Field2 := i;
    arr[i].Field4 := i;
  end;

  for rep := 1 to 10 do
  for i := Low(arr) to High(arr)-1 do begin
    arr[i].Field2 := 3*arr[i+1].Field2;
    arr[i].Field4 := 5*arr[i+1].Field4;
  end;

  Result := sw.ElapsedMilliseconds;
end;

function Align4: int64;
var
  arr: TArray<TRecA4>;
  i: Integer;
  sw: TStopwatch;
  rep: Integer;
begin
  SetLength(arr, 10000000);

  sw := TStopwatch.StartNew;

  for i := Low(arr) to High(arr) do begin
    arr[i].Field2 := i;
    arr[i].Field4 := i;
  end;

  for rep := 1 to 10 do
  for i := Low(arr) to High(arr)-1 do begin
    arr[i].Field2 := 3*arr[i+1].Field2;
    arr[i].Field4 := 5*arr[i+1].Field4;
  end;

  Result := sw.ElapsedMilliseconds;
end;

function Align8: int64;
var
  arr: TArray<TRecA8>;
  i: Integer;
  sw: TStopwatch;
  rep: Integer;
begin
  SetLength(arr, 10000000);

  sw := TStopwatch.StartNew;

  for i := Low(arr) to High(arr) do begin
    arr[i].Field2 := i;
    arr[i].Field4 := i;
  end;

  for rep := 1 to 10 do
  for i := Low(arr) to High(arr)-1 do begin
    arr[i].Field2 := 3*arr[i+1].Field2;
    arr[i].Field4 := 5*arr[i+1].Field4;
  end;

  Result := sw.ElapsedMilliseconds;
end;

procedure TfrmCompilerOptions.btnAlignmentClick(Sender: TObject);
begin
  ListBox1.Items.Add(Format('A1: %d ms, A2: %d ms, A4: %d ms, A8: %d ms',
    [Align1, Align2, Align4, Align8]));
end;

procedure TfrmCompilerOptions.Button1Click(Sender: TObject);
begin
  ListBox1.Items.Add(Format('SizeOf() A1: %d, A2: %d, A4: %d, A8: %d',
    [SizeOf(TRecA1), SizeOf(TRecA2), SizeOf(TRecA4), SizeOf(TRecA8)]));

end;
{$IFDEF RANGECHECKING}{$R+}{$ELSE}{$R-}{$ENDIF}
{$IFDEF OPTIMIZATION}{$O+}{$ELSE}{$O-}{$ENDIF}

end.
