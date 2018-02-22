unit InitializeFinalizeMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmInitFin = class(TForm)
    btnNewDisp: TButton;
    btnInitFin: TButton;
    ListBox1: TListBox;
    btnGetMem: TButton;
    procedure btnNewDispClick(Sender: TObject);
    procedure btnInitFinClick(Sender: TObject);
    procedure btnGetMemClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmInitFin: TfrmInitFin;

implementation

{$R *.dfm}

type
  TRecord = record
    s1, s2, s3, s4: string;
  end;
  PRecord = ^TRecord;

procedure TfrmInitFin.btnNewDispClick(Sender: TObject);
var
  rec: PRecord;
begin
  GetMem(rec, SizeOf(TRecord));
  FillChar(rec^, SizeOf(TRecord), '*');
  FreeMem(rec);

  New(rec);
  try
    rec.s1 := '4';
    rec.s2 := '2';
    rec.s4 := rec.s1 + rec.s2 + rec.s4;
    ListBox1.Items.Add('New: ' + rec.s4);
  finally
    Dispose(rec);
  end;
end;

procedure TfrmInitFin.btnInitFinClick(Sender: TObject);
var
  rec: PRecord;
begin
  GetMem(rec, SizeOf(TRecord));
  FillChar(rec^, SizeOf(TRecord), '*');
  FreeMem(rec);

  GetMem(rec, SizeOf(TRecord));
  try
    Initialize(rec^);
    rec.s1 := '4';
    rec.s2 := '2';
    rec.s4 := rec.s1 + rec.s2 + rec.s4;
    ListBox1.Items.Add('GetMem+Initialize: ' + rec.s4);
  finally
    Finalize(rec^);
    FreeMem (rec);
  end;
end;

procedure TfrmInitFin.btnGetMemClick(Sender: TObject);
var
  rec: PRecord;
begin
  GetMem(rec, SizeOf(TRecord));
  FillChar(rec^, SizeOf(TRecord), '*');
  FreeMem(rec);

  GetMem(rec, SizeOf(TRecord));
  try
    rec.s1 := '4';
    rec.s2 := '2';
    rec.s4 := rec.s1 + rec.s2 + rec.s4;
    ListBox1.Items.Add('GetMem+Initialize: ' + rec.s4);
  finally
    FreeMem (rec);
  end;
end;

end.
