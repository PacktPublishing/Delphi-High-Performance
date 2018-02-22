unit BeginUpdateMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TfrmBeginUpdate = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    Memo1: TMemo;
    StatusBar1: TStatusBar;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    procedure Time(addToListBox, addToMemo: TProc);
  public
  end;

var
  frmBeginUpdate: TfrmBeginUpdate;

implementation

uses
  System.Diagnostics;

{$R *.dfm}

const
  CNumLines = 10000;

procedure TfrmBeginUpdate.Button1Click(Sender: TObject);
begin
  Time(
    procedure
    var
      i: integer;
    begin
      for i := 1 to CNumLines do
        ListBox1.Items.Add('Line ' + IntToStr(i));
    end,

    procedure
    var
      i: integer;
    begin
      for i := 1 to CNumLines do
        Memo1.Lines.Add('Line ' + IntToStr(i));
    end
  );
end;

procedure TfrmBeginUpdate.Button2Click(Sender: TObject);
begin
  Time(
    procedure
    var
      i: integer;
    begin
      ListBox1.Items.BeginUpdate;
      for i := 1 to CNumLines do
        ListBox1.Items.Add('Line ' + IntToStr(i));
      ListBox1.Items.EndUpdate;
    end,

    procedure
    var
      i: integer;
    begin
      Memo1.Lines.BeginUpdate;
      for i := 1 to CNumLines do
        Memo1.Lines.Add('Line ' + IntToStr(i));
      Memo1.Lines.EndUpdate;
    end
  );
end;

procedure TfrmBeginUpdate.Button3Click(Sender: TObject);
begin
  Time(
    procedure
    begin
    end,

    procedure
    var
      i: integer;
      sl: TStringlist;
    begin
      sl := TStringList.Create;
      for i := 1 to CNumLines do
        sl.Add('Line ' + IntToStr(i));
      Memo1.Text := sl.Text;
      FreeAndNil(sl);
    end
  );
end;

procedure TfrmBeginUpdate.Time(addToListBox, addToMemo: TProc);
var
  iLine: Integer;
  listBoxTime: int64;
  memoTime: int64;
  stopwatch: TStopwatch;
begin
  ListBox1.Clear;
  Memo1.Clear;

  stopwatch := TStopwatch.StartNew;

  addToListBox();

  stopwatch.Stop;
  listBoxTime := stopwatch.ElapsedMilliseconds;

  stopwatch.Reset;
  stopwatch.Start;

  addToMemo();

  stopwatch.Stop;
  memoTime := stopwatch.ElapsedMilliseconds;
  StatusBar1.SimpleText := Format('ListBox: %d ms, Memo: %d ms', [listBoxTime, memoTime]);
end;

end.
