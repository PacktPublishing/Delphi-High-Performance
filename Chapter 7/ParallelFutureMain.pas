unit ParallelFutureMain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Threading,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmFuture = class(TForm)
    btnFuture: TButton;
    btnGetValue: TButton;
    ListBox1: TListBox;
    btnFuture2: TButton;
    procedure btnFutureClick(Sender: TObject);
    procedure btnGetValueClick(Sender: TObject);
    procedure btnFuture2Click(Sender: TObject);
  private
    FFuture: IFuture<integer>;
    procedure ReportFuture;
  public
  end;

var
  frmFuture: TfrmFuture;

implementation

{$R *.dfm}

const
  CHighestNumber = 5000000;

function IsPrime(value: integer): boolean;
var
  i: Integer;
begin
  Result := True;
  for i := 2 to Round(Sqrt(value)) do
    if (value mod i) = 0 then
      Exit(False);
end;

function CountPrimes: integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 2 to CHighestNumber do
    if IsPrime(i) then
      Inc(Result);
end;

procedure TfrmFuture.btnFutureClick(Sender: TObject);
begin
  FFuture := TTask.Future<integer>(CountPrimes);
end;

procedure TfrmFuture.btnFuture2Click(Sender: TObject);
begin
  FFuture := TTask.Future<integer>(
    function: Integer
    begin
      Result := CountPrimes;
      TThread.Queue(nil, ReportFuture);
    end);
end;

procedure TfrmFuture.btnGetValueClick(Sender: TObject);
begin
  if not assigned(FFuture) then
    Exit;

  ListBox1.Items.Add('Result = ' + FFuture.Value.ToString);
  FFuture := nil;
end;

procedure TfrmFuture.ReportFuture;
begin
  ListBox1.Items.Add('Result = ' + FFuture.Value.ToString);
  FFuture := nil;
end;

end.
