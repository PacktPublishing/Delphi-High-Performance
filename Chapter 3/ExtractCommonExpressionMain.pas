unit ExtractCommonExpressionMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmCommonExpression = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
  public
  end;

var
  frmCommonExpression: TfrmCommonExpression;

implementation

uses
system.sysutils
  System.Diagnostics;

{$R *.dfm}

const
  CMaxItems = 1000;

procedure TfrmCommonExpression.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  ListBox1.Items.BeginUpdate;
  try
    for i := 1 to CMaxItems do
      ListBox1.Items.Add(Format('Author %d-Title %d', [i, i]));
  finally ListBox1.Items.EndUpdate; end;
end;

procedure TfrmCommonExpression.Button1Click(Sender: TObject);
var
  i: Integer;
  sw: TStopwatch;
begin
  ListBox1.Items.BeginUpdate;
  try
    sw := TStopwatch.StartNew;
    for i := 0 to ListBox1.Count - 1 do
      ListBox1.Items[i] :=
        Copy(ListBox1.Items[i], Pos('-', ListBox1.Items[i]) + 1, Length(ListBox1.Items[i]))
        + '-'
        + Copy(ListBox1.Items[i], 1, Pos('-', ListBox1.Items[i]) - 1);
    sw.Stop;
    Button1.Caption := IntToStr(sw.ElapsedMilliseconds);
  finally ListBox1.Items.EndUpdate; end;
end;

procedure TfrmCommonExpression.Button2Click(Sender: TObject);
var
  i: Integer;
  s: string;
  p: Integer;
  sw: TStopwatch;
begin
  ListBox1.Items.BeginUpdate;
  try
    sw := TStopwatch.StartNew;
    for i := 0 to ListBox1.Count - 1 do begin
      s := ListBox1.Items[i];
      p := Pos('-', s);
      ListBox1.Items[i] := Copy(s, p + 1, Length(s)) + '-' + Copy(s, 1, p - 1);
    end;
    sw.Stop;
    Button2.Caption := IntToStr(sw.ElapsedMilliseconds);
  finally ListBox1.Items.EndUpdate; end;
end;

end.
