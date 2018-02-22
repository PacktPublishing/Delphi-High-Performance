unit TasksMain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Threading,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmTasks = class(TForm)
    btnTask: TButton;
    ListBox1: TListBox;
    btnTwoTasks: TButton;
    procedure btnTaskClick(Sender: TObject);
    procedure btnTwoTasksClick(Sender: TObject);
  private
    FTask: ITask;
  protected
    procedure CalcA;
    procedure CalcB;
    procedure PingMe;
  public
  end;

var
  frmTasks: TfrmTasks;

implementation

uses
  System.Diagnostics;

{$R *.dfm}

procedure TfrmTasks.btnTaskClick(Sender: TObject);
begin
  FTask := TTask.Create(PingMe);
  FTask.Start;
end;

procedure TfrmTasks.btnTwoTasksClick(Sender: TObject);
var
  tasks: array [1..2] of ITask;
  sw: TStopwatch;
begin
  sw := TStopwatch.StartNew;
  tasks[1] := TTask.Run(CalcA);
  tasks[2] := TTask.Run(CalcB);
  TTask.WaitForAll(tasks);
  sw.Stop;
  ListBox1.Items.Add('Elapsed time: ' + sw.ElapsedMilliseconds.ToString);
end;

procedure TfrmTasks.CalcA;
begin
  Sleep(1000); //simulate hard work
end;

procedure TfrmTasks.CalcB;
begin
  Sleep(1500); //simulate hard work
end;

procedure TfrmTasks.PingMe;
var
  i: Integer;
  threadID: TThreadID;
begin
  threadID := GetCurrentThreadID;
  for i := 1 to 5 do
  begin
    TThread.Queue(nil,
      procedure
      begin
        ListBox1.Items.Add('Ping from thread ' + threadID.ToString);
      end);
    Sleep(1000);
  end;
  TThread.Queue(nil,
    procedure
    begin
      FTask := nil;
    end);
end;

end.
