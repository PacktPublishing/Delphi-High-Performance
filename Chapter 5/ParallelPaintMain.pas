unit ParallelPaintMain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.Types, System.SysUtils, System.Variants, System.Classes,
  System.Threading, System.SyncObjs,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls;

type
  TfrmParallelPaint = class(TForm)
    PaintBox1: TPaintBox;
    PaintBox3: TPaintBox;
    PaintBox2: TPaintBox;
    PaintBox4: TPaintBox;
    PaintBox5: TPaintBox;
    PaintBox6: TPaintBox;
    PaintBox7: TPaintBox;
    PaintBox8: TPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FCanvas: TCanvas;
    FP1: TPoint;
    FP2: TPoint;
    FColor: TColor;
    FStop: boolean;
    FTasks: TArray<ITask>;
    FNumRunning: int64;
    procedure DrawLine(canvas: TCanvas; p1, p2: TPoint; color: TColor);
    procedure SynchronizedDraw;
    procedure SyncDrawLine(canvas: TCanvas; p1, p2: TPoint; color: TColor);
    procedure DrawLines(paintBox: TPaintBox);
    function MakeTask(paintBox: TPaintBox): TProc;
    procedure QueueDrawLine(canvas: TCanvas; p1, p2: TPoint; color: TColor);
  end;

var
  frmParallelPaint: TfrmParallelPaint;

implementation

{$R *.dfm}

procedure TfrmParallelPaint.DrawLine(canvas: TCanvas; p1, p2: TPoint;
  color: TColor);
begin
  Canvas.Pen.Color := color;
  Canvas.MoveTo(p1.X, p1.Y);
  Canvas.LineTo(p2.X, p2.Y);
end;

procedure TfrmParallelPaint.QueueDrawLine(canvas: TCanvas; p1, p2: TPoint; color: TColor);
begin
  TThread.Queue(nil,
    procedure
    begin
      Canvas.Pen.Color := color;
      Canvas.MoveTo(p1.X, p1.Y);
      Canvas.LineTo(p2.X, p2.Y);
    end);
end;

procedure TfrmParallelPaint.SynchronizedDraw;
begin
  FCanvas.Pen.Color := FColor;
  FCanvas.MoveTo(FP1.X, FP1.Y);
  FCanvas.LineTo(FP2.X, FP2.Y);
end;

procedure TfrmParallelPaint.SyncDrawLine(canvas: TCanvas; p1, p2: TPoint;
  color: TColor);
begin
  FCanvas := canvas;
  FP1 := p1;
  FP2 := p2;
  FColor := color;
  TThread.Synchronize(nil, SynchronizedDraw);
end;

procedure TfrmParallelPaint.DrawLines(paintBox: TPaintBox);
var
  p1, p2: TPoint;
  d1, d2: TPoint;
  canvas: TCanvas;
  color: TColor;
begin
  TInterlocked.Increment(FNumRunning);
  canvas := paintBox.Canvas;
  p1 := Point(50, 10);
  d1 := Point(2, 3);
  p2 := Point(10, 50);
  d2 := Point(3, 2);
  color := 0;
  while not FStop do
  begin
    DrawLine(canvas, p1, p2, color);
//    QueueDrawLine(canvas, p1, p2, color);
//    SyncDrawLine(canvas, p1, p2, color);
    color := color + $010102;
    if color > $FFFFFF then
      color := 0;
    p1 := p1 + d1;
    if (p1.X < 0) or (p1.X >= paintBox.Width) then begin
      d1.X := - d1.X;
      p1.X := p1.X + 2 * d1.X;
    end;
    if (p1.Y < 0) or (p1.Y >= paintBox.Height) then begin
      d1.Y := - d1.Y;
      p1.Y := p1.Y + 2 * d1.Y;
    end;
    p2 := p2 + d2;
    if (p2.X < 0) or (p2.X >= paintBox.Width) then begin
      d2.X := - d2.X;
      p2.X := p2.X + 2 * d2.X;
    end;
    if (p2.Y < 0) or (p2.Y >= paintBox.Height) then begin
      d2.Y := - d2.Y;
      p2.Y := p2.Y + 2 * d2.Y;
    end;
    Sleep(1);
  end;
  TInterlocked.Decrement(FNumRunning);
end;

function TfrmParallelPaint.MakeTask(paintBox: TPaintBox): TProc;
begin
  Result :=
    procedure
    begin
      DrawLines(paintBox);
    end;
end;

procedure TfrmParallelPaint.FormCreate(Sender: TObject);
var
  i: integer;
  pb: TArray<TPaintBox>;
begin
  pb := TArray<TPaintbox>.Create(PaintBox1, PaintBox2, PaintBox3, PaintBox4,
                                 PaintBox5, PaintBox6, PaintBox7, PaintBox8);
  SetLength(FTasks, Length(pb));
  for i := 0 to High(pb) do
    FTasks[i] := TTask.Run(MakeTask(pb[i]));
end;

procedure TfrmParallelPaint.FormDestroy(Sender: TObject);
begin
  FStop := true;
  while TInterlocked.Read(FNumRunning) > 0 do
    Application.ProcessMessages;
  TTask.WaitForAll(FTasks);
end;

end.
