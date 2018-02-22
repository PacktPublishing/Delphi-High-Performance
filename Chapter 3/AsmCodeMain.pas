unit AsmCodeMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmAsmCode = class(TForm)
    btnPascal: TButton;
    btnAsm: TButton;
    ListBox1: TListBox;
    procedure btnPascalClick(Sender: TObject);
    procedure btnAsmClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAsmCode: TfrmAsmCode;

implementation

uses
  System.Diagnostics;

{$R *.dfm}

type
  TVec4 = packed record
    X, Y, Z, W: Single;
  end;

function Multiply_PAS(const A, B: TVec4): TVec4;
begin
  Result.X := A.X * B.X;
  Result.Y := A.Y * B.Y;
  Result.Z := A.Z * B.Z;
  Result.W := A.W * B.W;
end;

function Multiply_ASM(const A, B: TVec4): TVec4;
asm
  movups xmm0, [A]
  movups xmm1, [B]
  mulps  xmm0, xmm1
  movups [Result], xmm0
end;

procedure TfrmAsmCode.btnPascalClick(Sender: TObject);
var
  v1, v2, prod: TVec4;
  sw: TStopwatch;
  i: Integer;
  det: Double;
begin
  v1.X := 1.1; v1.Y := 2.2; v1.Z := 3.3; v1.W := 4.4;
  v2.X := 6.6; v2.Y := 7.7; v2.Z := 8.8; v2.W := 9.9;

  sw := TStopwatch.StartNew;
  for i := 1 to 10000000 do
    prod := Multiply_PAS(v1, v2);
  sw.Stop;
  ListBox1.Items.Add(Format('Pascal: %d ms', [sw.ElapsedMilliseconds]));
  ListBox1.Items.Add(Format('%.6f, %.6f, %.6f, %.6f', [prod.X, prod.Y, prod.Z, prod.W]));
end;

procedure TfrmAsmCode.btnAsmClick(Sender: TObject);
var
  v1, v2, prod: TVec4;
  sw: TStopwatch;
  i: Integer;
  det: Double;
begin
  v1.X := 1.1; v1.Y := 2.2; v1.Z := 3.3; v1.W := 4.4;
  v2.X := 6.6; v2.Y := 7.7; v2.Z := 8.8; v2.W := 9.9;

  sw := TStopwatch.StartNew;
  for i := 1 to 10000000 do
    prod := Multiply_ASM(v1, v2);
  sw.Stop;
  ListBox1.Items.Add(Format('Assembler: %d ms', [sw.ElapsedMilliseconds]));
  ListBox1.Items.Add(Format('%.6f, %.6f, %.6f, %.6f', [prod.X, prod.Y, prod.Z, prod.W]));
end;

end.
