unit DataTypesMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TRecord = record
    a: integer;
    b: string;
    c: integer;
  end;

  TfrmDataTypes = class(TForm)
    btnCopyOnWrite: TButton;
    ListBox1: TListBox;
    btnSharedDynArrays: TButton;
    btnRecordInit: TButton;
    btnCopyRec: TButton;
    procedure btnCopyOnWriteClick(Sender: TObject);
    procedure btnSharedDynArraysClick(Sender: TObject);
    procedure btnRecordInitClick(Sender: TObject);
    procedure btnCopyRecClick(Sender: TObject);
  private
    procedure ShowRecord(const rec: TRecord);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDataTypes: TfrmDataTypes;

implementation

uses
  System.Diagnostics;

{$R *.dfm}

procedure TfrmDataTypes.btnCopyOnWriteClick(Sender: TObject);
var
  s1, s2: string;
begin
  s1 := 'Delphi';
  s2 := s1;
  ListBox1.Items.Add(Format('s1 = %p [%s], s2 = %p [%s]',
    [PPointer(@s1)^, s1, PPointer(@s2)^, s2]));
  s2[1] := 'd';
  ListBox1.Items.Add(Format('s1 = %p [%s], s2 = %p [%s]',
    [PPointer(@s1)^, s1, PPointer(@s2)^, s2]));
end;


function ToStringArr(const arr: TArray<integer>): string;
var
  sarr: TArray<string>;
  i: Integer;
begin
  SetLength(sarr, Length(arr));
  for i := Low(arr) to High(arr) do
    sarr[i] := IntToStr(arr[i]);
  Result := ''.Join(', ', sarr);
end;

procedure TfrmDataTypes.btnSharedDynArraysClick(Sender: TObject);
var
  arr1, arr2: TArray<Integer>;
begin
  arr1 := [1, 2, 3, 4, 5];
  arr2 := arr1;
  ListBox1.Items.Add(Format('arr1 = %p [%s], arr2 = %p [%s]',
    [PPointer(@arr1)^, ToStringArr(arr1), PPointer(@arr2)^, ToStringArr(arr2)]));
  arr1[2] := 42;
  ListBox1.Items.Add(Format('arr1 = %p [%s], arr2 = %p [%s]',
    [PPointer(@arr1)^, ToStringArr(arr1), PPointer(@arr2)^, ToStringArr(arr2)]));
  SetLength(arr2, Length(arr2));
  arr1[2] := 17;
  ListBox1.Items.Add(Format('arr1 = %p [%s], arr2 = %p [%s]',
    [PPointer(@arr1)^, ToStringArr(arr1), PPointer(@arr2)^, ToStringArr(arr2)]));
end;

procedure TfrmDataTypes.ShowRecord(const rec: TRecord);
begin
  ListBox1.Items.Add(Format('a = %d, b = ''%s'', c = %d', [rec.a, rec.b, rec.c]));
end;

procedure TfrmDataTypes.btnRecordInitClick(Sender: TObject);
var
  rec: TRecord;
begin
  ShowRecord(rec);
  rec := Default(TRecord);
  ShowRecord(rec);
end;

type
  TUnmanaged = record
    a, b, c, d: NativeUInt;
  end;

  TManaged = record
    a, b, c, d: IInterface;
  end;

procedure TfrmDataTypes.btnCopyRecClick(Sender: TObject);
var
  u1, u2: TUnmanaged;
  m1, m2: TManaged;
  i: Integer;
  sw: TStopwatch;
begin
  u1 := Default(TUnmanaged);
  sw := TStopwatch.StartNew;
  for i := 1 to 1000000 do
    u2 := u1;
  sw.Stop;
  ListBox1.Items.Add(Format('TUnmanaged: %d ms', [sw.ElapsedMilliseconds]));

  m1 := Default(TManaged);
  sw := TStopwatch.StartNew;
  for i := 1 to 1000000 do
    m2 := m1;
  sw.Stop;
  ListBox1.Items.Add(Format('TManaged: %d ms', [sw.ElapsedMilliseconds]));
end;

end.
