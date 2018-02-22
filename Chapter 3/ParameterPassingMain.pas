unit ParameterPassingMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

const
  CArraySize = 100000;

type
  TBigArray = array [1..CArraySize] of integer;
  TIntArray = array of integer;

  TRecord = record
    Field1: int64;
    Field2: int64;
    Field3: int64;
    Field4: int64;
  end;

  TfrmParamPassing = class(TForm)
    btnArray: TButton;
    btnConstArray: TButton;
    ListBox1: TListBox;
    btnDynArray: TButton;
    btnConstDynArray: TButton;
    btnRecord: TButton;
    btnConstRecord: TButton;
    btnString: TButton;
    btnConstString: TButton;
    btnConstInterface: TButton;
    btnInterface: TButton;
    btnDynArray2: TButton;
    btnDynArray3: TButton;
    btnConstDynArray2: TButton;
    btnConstDynArray3: TButton;
    procedure btnArrayClick(Sender: TObject);
    procedure btnConstArrayClick(Sender: TObject);
    procedure btnDynArrayClick(Sender: TObject);
    procedure btnConstDynArrayClick(Sender: TObject);
    procedure btnRecordClick(Sender: TObject);
    procedure btnConstRecordClick(Sender: TObject);
    procedure btnStringClick(Sender: TObject);
    procedure btnConstStringClick(Sender: TObject);
    procedure btnInterfaceClick(Sender: TObject);
    procedure btnConstInterfaceClick(Sender: TObject);
    procedure btnDynArray3Click(Sender: TObject);
    procedure btnDynArray2Click(Sender: TObject);
    procedure btnConstDynArray2Click(Sender: TObject);
    procedure btnConstDynArray3Click(Sender: TObject);
  private
    procedure ProcIntf(intf: IInterface);
    procedure ProcIntfConst(const intf: IInterface);
    procedure ProcRecord(rec: TRecord);
    procedure ProcRecordConst(const rec: TRecord);
    procedure ProcString(s: string);
    procedure ProcStringConst(const s: string);
    procedure ScanArray(arr: TBigArray);
    procedure ScanArrayConst(const arr: TBigArray);
    procedure ScanDynArray(arr: TArray<Integer>);
    procedure ScanDynArrayConst(const arr: TArray<Integer>);
    procedure ScanDynArray2(arr: array of integer);
    procedure ScanDynArrayConst2(const arr: array of integer);
    procedure ScanDynArray3(arr: TIntArray);
    procedure ScanDynArrayConst3(const arr: TIntArray);
  public
  end;

var
  frmParamPassing: TfrmParamPassing;

implementation

uses
  System.Diagnostics;

{$R *.dfm}

procedure TfrmParamPassing.ScanArray(arr: TBigArray);
begin
  arr[1] := 42;
end;

procedure TfrmParamPassing.ScanArrayConst(const arr: TBigArray);
begin
  // compiler doesn't allow that:
  // arr[1] := 42;
end;

procedure TfrmParamPassing.btnArrayClick(Sender: TObject);
var
  arr: TBigArray;
  sw: TStopwatch;
  i: Integer;
begin
  arr[1] := 1;
  sw := TStopwatch.StartNew;
  for i := 1 to 10000 do
    ScanArray(arr);
  sw.Stop;

  ListBox1.Items.Add(Format('Array: %d ms, arr[1] = %d', [sw.ElapsedMilliseconds, arr[1]]));
end;

procedure TfrmParamPassing.btnConstArrayClick(Sender: TObject);
var
  arr: TBigArray;
  sw: TStopwatch;
  i: Integer;
begin
  arr[1] := 1;
  sw := TStopwatch.StartNew;
  for i := 1 to 10000 do
    ScanArrayConst(arr);
  sw.Stop;

  ListBox1.Items.Add(Format('Const array: %d ms, arr[1] = %d', [sw.ElapsedMilliseconds, arr[1]]));
end;

procedure TfrmParamPassing.ScanDynArray(arr: TArray<Integer>);
begin
  arr[1] := 42;
end;

procedure TfrmParamPassing.ScanDynArray2(arr: array of integer);
begin
  arr[1] := 42;
end;

procedure TfrmParamPassing.ScanDynArray3(arr: TIntArray);
begin
  arr[1] := 42;
end;

procedure TfrmParamPassing.ScanDynArrayConst(const arr: TArray<Integer>);
begin
  // strangely, compiler allows that
  arr[1] := 42;
end;

procedure TfrmParamPassing.ScanDynArrayConst2(const arr: array of integer);
begin
  // in this case folllowing line doesn't compile
  // arr[1] := 42;
end;

procedure TfrmParamPassing.ScanDynArrayConst3(const arr: TIntArray);
begin
  // it compiles!
  arr[1] := 42;
end;

procedure TfrmParamPassing.btnDynArrayClick(Sender: TObject);
var
  arr: TArray<Integer>;
  sw: TStopwatch;
  i: Integer;
begin
  SetLength(arr, CArraySize);
  arr[1] := 1;
  sw := TStopwatch.StartNew;
  for i := 1 to 10000 do
    ScanDynArray(arr);
  sw.Stop;

  ListBox1.Items.Add(Format('TArray<Integer>: %d ms, arr[1] = %d', [sw.ElapsedMilliseconds, arr[1]]));
end;

procedure TfrmParamPassing.btnDynArray2Click(Sender: TObject);
var
  arr: array of integer;
  sw: TStopwatch;
  i: Integer;
begin
  SetLength(arr, CArraySize);
  arr[1] := 1;
  sw := TStopwatch.StartNew;
  for i := 1 to 10000 do
    ScanDynArray2(arr);
  sw.Stop;

  ListBox1.Items.Add(Format('array of Integer: %d ms, arr[1] = %d', [sw.ElapsedMilliseconds, arr[1]]));
end;

procedure TfrmParamPassing.btnDynArray3Click(Sender: TObject);
var
  arr: array of integer;
  sw: TStopwatch;
  i: Integer;
begin
  SetLength(arr, CArraySize);
  arr[1] := 1;
  sw := TStopwatch.StartNew;
  for i := 1 to 10000 do
    ScanDynArrayConst2(arr);
  sw.Stop;

  ListBox1.Items.Add(Format('TIntArray: %d ms, arr[1] = %d', [sw.ElapsedMilliseconds, arr[1]]));
end;

procedure TfrmParamPassing.btnConstDynArrayClick(Sender: TObject);
var
  arr: TArray<Integer>;
  sw: TStopwatch;
  i: Integer;
begin
  SetLength(arr, CArraySize);
  arr[1] := 1;
  sw := TStopwatch.StartNew;
  for i := 1 to 10000 do
    ScanDynArrayConst(arr);
  sw.Stop;

  ListBox1.Items.Add(Format('Const TArray<Integer>: %d ms, arr[1] = %d', [sw.ElapsedMilliseconds, arr[1]]));
end;

procedure TfrmParamPassing.btnConstDynArray2Click(Sender: TObject);
var
  arr: array of integer;
  sw: TStopwatch;
  i: Integer;
begin
  SetLength(arr, CArraySize);
  arr[1] := 1;
  sw := TStopwatch.StartNew;
  for i := 1 to 10000 do
    ScanDynArrayConst2(arr);
  sw.Stop;

  ListBox1.Items.Add(Format('const array of Integer: %d ms, arr[1] = %d', [sw.ElapsedMilliseconds, arr[1]]));
end;

procedure TfrmParamPassing.btnConstDynArray3Click(Sender: TObject);
var
  arr: TIntArray;
  sw: TStopwatch;
  i: Integer;
begin
  SetLength(arr, CArraySize);
  arr[1] := 1;
  sw := TStopwatch.StartNew;
  for i := 1 to 10000 do
    ScanDynArrayConst3(arr);
  sw.Stop;

  ListBox1.Items.Add(Format('Const TIntArray: %d ms, arr[1] = %d', [sw.ElapsedMilliseconds, arr[1]]));
end;

procedure TfrmParamPassing.ProcRecord(rec: TRecord);
begin
  // do nothing
end;

procedure TfrmParamPassing.ProcRecordConst(const rec: TRecord);
begin
  // do nothing
end;

procedure TfrmParamPassing.btnRecordClick(Sender: TObject);
var
  rec: TRecord;
  sw: TStopwatch;
  i: Integer;
begin
  sw := TStopwatch.StartNew;
  for i := 1 to 10000000 do
    ProcRecord(rec);
  sw.Stop;

  ListBox1.Items.Add(Format('Record: %d ms', [sw.ElapsedMilliseconds]));
end;

procedure TfrmParamPassing.btnConstRecordClick(Sender: TObject);
var
  rec: TRecord;
  sw: TStopwatch;
  i: Integer;
begin
  sw := TStopwatch.StartNew;
  for i := 1 to 10000000 do
    ProcRecordConst(rec);
  sw.Stop;

  ListBox1.Items.Add(Format('Const record: %d ms', [sw.ElapsedMilliseconds]));
end;

procedure TfrmParamPassing.ProcString(s: string);
begin
  // Enable next line and code will suddenly become much slower!
  //s[1] := 'a';
end;

procedure TfrmParamPassing.ProcStringConst(const s: string);
begin
  // do nothing
end;

procedure TfrmParamPassing.btnStringClick(Sender: TObject);
var
  sw: TStopwatch;
  i: Integer;
  s: string;
begin
  SetLength(s, 10000);
  sw := TStopwatch.StartNew;
  for i := 1 to 10000000 do
    ProcString(s);
  sw.Stop;

  ListBox1.Items.Add(Format('String: %d ms', [sw.ElapsedMilliseconds]));
end;

procedure TfrmParamPassing.btnConstStringClick(Sender: TObject);
var
  sw: TStopwatch;
  i: Integer;
  s: string;
begin
  SetLength(s, 10000);
  sw := TStopwatch.StartNew;
  for i := 1 to 10000000 do
    ProcStringConst(s);
  sw.Stop;

  ListBox1.Items.Add(Format('Const string: %d ms', [sw.ElapsedMilliseconds]));
end;

procedure TfrmParamPassing.ProcIntf(intf: IInterface);
begin
  // do nothing
end;

procedure TfrmParamPassing.ProcIntfConst(const intf: IInterface);
begin
  // do nothing
end;

procedure TfrmParamPassing.btnInterfaceClick(Sender: TObject);
var
  sw: TStopwatch;
  i: Integer;
  int: IInterface;
begin
  int := TInterfacedObject.Create;
  sw := TStopwatch.StartNew;
  for i := 1 to 10000000 do
    ProcIntf(int);
  sw.Stop;

  ListBox1.Items.Add(Format('Interface: %d ms', [sw.ElapsedMilliseconds]));
end;

procedure TfrmParamPassing.btnConstInterfaceClick(Sender: TObject);
var
  sw: TStopwatch;
  i: Integer;
  int: IInterface;
begin
  int := TInterfacedObject.Create;
  sw := TStopwatch.StartNew;
  for i := 1 to 10000000 do
    ProcIntfConst(int);
  sw.Stop;

  ListBox1.Items.Add(Format('Const interface: %d ms', [sw.ElapsedMilliseconds]));
end;

end.
