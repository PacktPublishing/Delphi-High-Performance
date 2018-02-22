unit ReallocationMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmReallocation = class(TForm)
    btnAppendString: TButton;
    btnSetLength: TButton;
    ListBox1: TListBox;
    btnAppendArray: TButton;
    btnSetLengthArray: TButton;
    btnAppendTList: TButton;
    btnSetCapacityTList: TButton;
    procedure btnAppendStringClick(Sender: TObject);
    procedure btnSetLengthClick(Sender: TObject);
    procedure btnAppendArrayClick(Sender: TObject);
    procedure btnSetLengthArrayClick(Sender: TObject);
    procedure btnAppendTListClick(Sender: TObject);
    procedure btnSetCapacityTListClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmReallocation: TfrmReallocation;

implementation

uses
  System.Generics.Collections,
  System.Diagnostics;

{$R *.dfm}

const
  CNumChars = 10000000;

procedure TfrmReallocation.btnAppendStringClick(Sender: TObject);
var
  sw: TStopwatch;
  s: String;
  i: Integer;
begin
  sw := TStopwatch.StartNew;
  s := '';
  for i := 1 to CNumChars do
    s := s + '*';
  sw.Stop;
  ListBox1.Items.Add(Format('Append string: %d ms', [sw.ElapsedMilliseconds]));
end;

procedure TfrmReallocation.btnSetLengthClick(Sender: TObject);
var
  sw: TStopwatch;
  s: String;
  i: Integer;
begin
  sw := TStopwatch.StartNew;
  SetLength(s, CNumChars);
  for i := 1 to CNumChars do
    s[i] := '*';
  sw.Stop;
  ListBox1.Items.Add(Format('Preallocate string: %d ms', [sw.ElapsedMilliseconds]));
end;

procedure TfrmReallocation.btnAppendArrayClick(Sender: TObject);
var
  sw: TStopwatch;
  arr: TArray<char>;
  i: Integer;
begin
  sw := TStopwatch.StartNew;
  SetLength(arr, 0);
  for i := 1 to CNumChars do begin
    SetLength(arr, Length(arr) + 1);
    arr[High(arr)] := '*';
  end;
  sw.Stop;
  ListBox1.Items.Add(Format('Append array: %d ms', [sw.ElapsedMilliseconds]));
end;

procedure TfrmReallocation.btnSetLengthArrayClick(Sender: TObject);
var
  sw: TStopwatch;
  arr: TArray<char>;
  i: Integer;
begin
  sw := TStopwatch.StartNew;
  SetLength(arr, CNumChars);
  for i := 1 to CNumChars do
    arr[i-1] := '*';
  sw.Stop;
  ListBox1.Items.Add(Format('Preallocate array: %d ms', [sw.ElapsedMilliseconds]));
end;

procedure TfrmReallocation.btnAppendTListClick(Sender: TObject);
var
  sw: TStopwatch;
  list: TList<Char>;
  i: Integer;
begin
  list := TList<Char>.Create;
  try
    sw := TStopwatch.StartNew;
    for i := 1 to CNumChars do
      list.Add('*');
    sw.Stop;
  finally
    FreeAndNil(list);
  end;
  ListBox1.Items.Add(Format('Append TList: %d ms', [sw.ElapsedMilliseconds]));
end;

procedure TfrmReallocation.btnSetCapacityTListClick(Sender: TObject);
var
  sw: TStopwatch;
  list: TList<Char>;
  i: Integer;
begin
  list := TList<Char>.Create;
  try
    sw := TStopwatch.StartNew;
    list.Capacity := CNumChars;
    for i := 1 to CNumChars do
      list.Add('*');
    sw.Stop;
  finally
    FreeAndNil(list);
  end;
  ListBox1.Items.Add(Format('Append TList: %d ms', [sw.ElapsedMilliseconds]));
end;

end.
