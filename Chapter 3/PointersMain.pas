unit PointersMain;

{$OPTIMIZATION ON}

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmPointers = class(TForm)
    btnArray: TButton;
    ListBox1: TListBox;
    btnPointer: TButton;
    btnPointerMath: TButton;
    procedure btnArrayClick(Sender: TObject);
    procedure btnPointerClick(Sender: TObject);
    procedure btnPointerMathClick(Sender: TObject);
  private
    function PrepareData: TArray<cardinal>;
  public
  end;

var
  frmPointers: TfrmPointers;

implementation

uses
  System.Diagnostics;

{$R *.dfm}

function TfrmPointers.PrepareData: TArray<cardinal>;
var
  i: Integer;
begin
  SetLength(Result, 100000000);
  for i := Low(Result) to High(Result) do
    Result[i] := $0055AACC;
end;

procedure TfrmPointers.btnArrayClick(Sender: TObject);
var
  rgbData: TArray<cardinal>;
  sw: TStopwatch;
  i: Integer;
  r,b: Byte;
begin
  rgbData := PrepareData;
  ListBox1.Items.Add(Format('Initial: %.8x, %.8x, ... %.8x, %.8x',
    [rgbData[0], rgbData[1], rgbData[High(rgbData)-1], rgbData[High(rgbData)]]));
  sw := TStopwatch.StartNew;

  for i := Low(rgbData) to High(rgbData) do
  begin
    b := rgbData[i] AND $00FF0000 SHR 16;
    r := rgbData[i] AND $000000FF;
    rgbData[i] := rgbData[i] AND $FF00FF00 OR (r SHL 16) OR b;
  end;

  sw.Stop;
  ListBox1.Items.Add(Format('Array: %d ms; %.8x, %.8x, ... %.8x, %.8x',
    [sw.ElapsedMilliseconds, rgbData[0], rgbData[1], rgbData[High(rgbData)-1], rgbData[High(rgbData)]]));
end;

procedure TfrmPointers.btnPointerClick(Sender: TObject);
var
  rgbData: TArray<cardinal>;
  sw: TStopwatch;
  i: Integer;
  r: Byte;
  pRed: PByte;
  pBlue: PByte;
begin
  rgbData := PrepareData;
  ListBox1.Items.Add(Format('Initial: %.8x, %.8x, ... %.8x, %.8x',
    [rgbData[0], rgbData[1], rgbData[High(rgbData)-1], rgbData[High(rgbData)]]));
  sw := TStopwatch.StartNew;

  pRed := @rgbData[0];
  pBlue := pRed;
  Inc(pBlue,2);
  for i := Low(rgbData) to High(rgbData) do
  begin
    r := pRed^;
    pRed^ := pBlue^;
    pBlue^ := r;
    Inc(pRed, SizeOf(rgbData[0]));
    Inc(pBlue, SizeOf(rgbData[0]));
  end;

  sw.Stop;
  ListBox1.Items.Add(Format('Pointer: %d ms; %.8x, %.8x, ... %.8x, %.8x',
    [sw.ElapsedMilliseconds, rgbData[0], rgbData[1], rgbData[High(rgbData)-1], rgbData[High(rgbData)]]));
end;

procedure TfrmPointers.btnPointerMathClick(Sender: TObject);
var
  pb: PByte;
  pi: PInteger;
  pa: PAnsiChar;
  pc: PChar;
begin
  pb := pointer(0);
  pb := pb + 1;
  ListBox1.Items.Add(Format('PByte increment = %d', [NativeUInt(pb)]));

  pi := pointer(0);
  {$POINTERMATH ON}
  pi := pi + 1;
  {$POINTERMATH OFF}
  ListBox1.Items.Add(Format('PInteger increment = %d', [NativeUInt(pi)]));

  pa := pointer(0);
  pa := pa + 1;
  ListBox1.Items.Add(Format('PAnsiChar increment = %d', [NativeUInt(pa)]));

  pc := pointer(0);
  pc := pc + 1;
  ListBox1.Items.Add(Format('PChar increment = %d', [NativeUInt(pc)]));
end;

end.
 