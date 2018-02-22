unit InliningMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmInlining = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    function IncrementShouldBeInline(value: integer): integer; inline;
  public
  end;

var
  frmInlining: TfrmInlining;

implementation

uses
  System.Diagnostics;

{$R *.dfm}

function Increment(value: integer): integer;
begin
  Result := value + 1;
end;

procedure TfrmInlining.Button1Click(Sender: TObject);
var
  value: Integer;
  i: Integer;
  sw: TStopwatch;
begin
  sw := TStopwatch.StartNew;
  value := 0;
  for i := 1 to 10000000 do
    value := Increment(value);
  sw.Stop;
  ShowMessageFmt('%d ms', [sw.ElapsedMilliseconds]);
end;

function IncrementInline(value: integer): integer; inline;
begin
  Result := value + 1;
end;

procedure TfrmInlining.Button2Click(Sender: TObject);
var
  value: Integer;
  i: Integer;
  sw: TStopwatch;
begin
  sw := TStopwatch.StartNew;
  value := 0;
  for i := 1 to 10000000 do
    value := IncrementInline(value);
  sw.Stop;
  ShowMessageFmt('%d ms', [sw.ElapsedMilliseconds]);
end;

procedure TfrmInlining.Button3Click(Sender: TObject);
var
  value: Integer;
  i: Integer;
  sw: TStopwatch;
begin
  sw := TStopwatch.StartNew;
  value := 0;
  for i := 1 to 10000000 do
    value := IncrementShouldBeInline(value);
  sw.Stop;
  ShowMessageFmt('%d ms', [sw.ElapsedMilliseconds]);
end;

function TfrmInlining.IncrementShouldBeInline(value: integer): integer;
begin
  Result := value + 1;
end;

end.
