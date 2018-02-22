unit BeginUpdateFMXMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.ListBox, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.StdCtrls;

type
  TfrmBeginUpdate = class(TForm)
    ListBox1: TListBox;
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    StatusBar1: TStatusBar;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    procedure Time(addToListBox, addToMemo: TProc);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmBeginUpdate: TfrmBeginUpdate;

implementation

uses
  System.Diagnostics;

{$R *.fmx}

const
  CNumLines = 1000;

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
  Memo1.Text := '';

  stopwatch := TStopwatch.StartNew;

  addToListBox();

  stopwatch.Stop;
  listBoxTime := stopwatch.ElapsedMilliseconds;

  stopwatch.Reset;
  stopwatch.Start;

  addToMemo();

  stopwatch.Stop;
  memoTime := stopwatch.ElapsedMilliseconds;
  Label1.Text := Format('ListBox: %d ms, Memo: %d ms', [listBoxTime, memoTime]);
end;

end.
