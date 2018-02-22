unit ProgressBarMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls;

type
  TfrmProgressBar = class(TForm)
    Button1: TButton;
    ProgressBar1: TProgressBar;
    procedure Button1Click(Sender: TObject);
  private
    function Test0To2G: Integer;
    function Test0To100: Integer;
  public
    { Public declarations }
  end;

var
  frmProgressBar: TfrmProgressBar;

implementation

uses
  System.Diagnostics;

{$R *.dfm}

const
  CFileSize = 2000000000;

procedure TfrmProgressBar.Button1Click(Sender: TObject);
var
  time2G: Integer;
  time100: Integer;
begin
  time2G := Test0To2G;
  time100 := Test0To100;
  ShowMessageFmt('0 - 2.000.000.000: %d ms'#13#10 +
                 '0 - 100: %d ms', [time2G, time100]);
end;

function TfrmProgressBar.Test0To100: Integer;
var
  total: Integer;
  block: Integer;
  sw: TStopwatch;
  lastPct: Integer;
  currPct: Integer;
begin
  // This code simulates reading 2 billion byte file in 1 KB blocks.
  // After each read, progress is calculated in percents (rounded to the
  // nearest whole number). Only when this number changes, a progress bar
  // Max set to 100 is updated.

  sw := TStopwatch.StartNew;
  ProgressBar1.Max := 100;
  ProgressBar1.Position := 0;
  lastPct := 0;

  total := 0;
  while total < CFileSize do begin
    block := CFileSize - total;
    if block > 1024 then
      block := 1024;
    // reading 'block' bytes
    Inc(total, block);

    currPct := Round(total / CFileSize * 100);
    if currPct > lastPct then
    begin
      lastPct := currPct;
//      ProgressBar1.Position := currPct+1;
      ProgressBar1.Position := currPct;
      ProgressBar1.Update;
    end;
  end;

//  ProgressBar1.Position := ProgressBar1.Max;
//  ProgressBar1.Max := ProgressBar1.Max - 1;

  Result := sw.ElapsedMilliseconds;
end;

function TfrmProgressBar.Test0To2G: Integer;
var
  total: Integer;
  block: Integer;
  sw: TStopwatch;
begin
  // This code simulates reading 2 billion byte file in 1 KB blocks.
  // After each read, number of currently read bytes is assigned directly
  // to a progress bar with Max set to CFileSize.

  sw := TStopwatch.StartNew;
  ProgressBar1.Max := CFileSize;
  ProgressBar1.Position := 0;

  total := 0;
  while total < CFileSize do begin
    block := CFileSize - total;
    if block > 1024 then
      block := 1024;
    // reading 'block' bytes
    Inc(total, block);

    ProgressBar1.Position := total;
    ProgressBar1.Update;
  end;
  Result := sw.ElapsedMilliseconds;
end;

end.
