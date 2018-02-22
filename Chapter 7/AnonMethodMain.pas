unit AnonMethodMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmAnonMthod = class(TForm)
    btnAnonProblem: TButton;
    ListBox1: TListBox;
    btnAnonFix: TButton;
    procedure btnAnonProblemClick(Sender: TObject);
    procedure btnAnonFixClick(Sender: TObject);
  private
    procedure Log(i: integer);
    function MakeLog(i: integer): TThreadProcedure;
  public
  end;

var
  frmAnonMthod: TfrmAnonMthod;

implementation

{$R *.dfm}

procedure TfrmAnonMthod.btnAnonProblemClick(Sender: TObject);
var
  i: Integer;
begin
  ListBox1.Items.Add('');
  for i := 1 to 20 do
    TThread.ForceQueue(nil,
      procedure
      begin
        Log(i);
      end);
end;

function TfrmAnonMthod.MakeLog(i: integer): TThreadProcedure;
begin
  Result :=
    procedure
    begin
      Log(i);
    end;
end;

procedure TfrmAnonMthod.btnAnonFixClick(Sender: TObject);
var
  i: Integer;
begin
  ListBox1.Items.Add('');
  for i := 1 to 20 do
    TThread.ForceQueue(nil, MakeLog(i));
end;

procedure TfrmAnonMthod.Log(i: integer);
begin
  ListBox1.Items[ListBox1.Items.Count - 1] :=
    ListBox1.Items[ListBox1.Items.Count - 1] + i.ToString + ' ';
end;

end.
