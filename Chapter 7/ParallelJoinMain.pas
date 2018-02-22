unit ParallelJoinMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmParallelJoin = class(TForm)
    btnJoin2: TButton;
    btnJoin3: TButton;
    ListBox1: TListBox;
    btnDHPJoin: TButton;
    btnDHPJoinExc: TButton;
    btnJoin1p2: TButton;
    procedure btnJoin2Click(Sender: TObject);
    procedure btnJoin3Click(Sender: TObject);
    procedure btnDHPJoinClick(Sender: TObject);
    procedure btnDHPJoinExcClick(Sender: TObject);
    procedure btnJoin1p2Click(Sender: TObject);
  private
    procedure Task1;
    procedure Task2;
    procedure Task2E;
    procedure Task3;
    procedure Task3E;
    procedure TasksStopped(E: Exception);
    procedure QueueLog(const msg: string);
  public
  end;

var
  frmParallelJoin: TfrmParallelJoin;

implementation

uses
  System.Threading,
  DHPThreading;

{$R *.dfm}

procedure TfrmParallelJoin.btnDHPJoinClick(Sender: TObject);
begin
  ListBox1.Items.Add('Starting tasks');
  Join([Task1, Task2, Task3]).Await(
    procedure
    begin
      ListBox1.Items.Add('Tasks stopped');
    end);
end;

procedure TfrmParallelJoin.btnDHPJoinExcClick(Sender: TObject);
begin
  ListBox1.Items.Add('Starting tasks');
  Join([Task1, Task2E, Task3E]).Await(TasksStopped);
end;

procedure NullTask;
begin
end;

procedure TfrmParallelJoin.btnJoin1p2Click(Sender: TObject);
begin
  ListBox1.Items.Add('Starting tasks');
  TParallel.Join([NullTask, Task1, Task2]);
end;

procedure TfrmParallelJoin.btnJoin2Click(Sender: TObject);
begin
  ListBox1.Items.Add('Starting tasks');
  TParallel.Join(Task1, Task2);
end;

procedure TfrmParallelJoin.btnJoin3Click(Sender: TObject);
begin
  ListBox1.Items.Add('Starting tasks');
  TParallel.Join([Task1, Task2, Task3]);
end;

procedure TfrmParallelJoin.QueueLog(const msg: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      ListBox1.Items.Add(msg);
    end);
end;

procedure TfrmParallelJoin.Task1;
begin
  QueueLog('Task1 started in thread ' + TThread.Current.ThreadID.ToString);
  Sleep(1000);
  QueueLog('Task1 stopped in thread ' + TThread.Current.ThreadID.ToString);
end;

procedure TfrmParallelJoin.Task2;
begin
  QueueLog('Task2 started in thread ' + TThread.Current.ThreadID.ToString);
  Sleep(1000);
  QueueLog('Task2 stopped in thread ' + TThread.Current.ThreadID.ToString);
end;

procedure TfrmParallelJoin.Task2E;
begin
  QueueLog('Task2E started in thread ' + TThread.Current.ThreadID.ToString);
  Sleep(1000);
  QueueLog('Task2E raising exception in thread ' + TThread.Current.ThreadID.ToString);
  raise Exception.Create('Task2 exception');
end;

procedure TfrmParallelJoin.Task3;
begin
  QueueLog('Task3 started in thread ' + TThread.Current.ThreadID.ToString);
  Sleep(1000);
  QueueLog('Task3 stopped in thread ' + TThread.Current.ThreadID.ToString);
end;

procedure TfrmParallelJoin.Task3E;
begin
  QueueLog('Task3E started in thread ' + TThread.Current.ThreadID.ToString);
  Sleep(1000);
  QueueLog('Task3E raising exception in thread ' + TThread.Current.ThreadID.ToString);
  raise Exception.Create('Task3E exception');
end;

procedure TfrmParallelJoin.TasksStopped(E: Exception);
var
  i: Integer;
begin
  ListBox1.Items.Add('Tasks stopped');
  if assigned(E) then
    if not (E is EAggregateException) then
      raise E
    else begin
      for i := 0 to EAggregateException(E).Count - 1 do
        ListBox1.Items.Add('Task raised exception: ' +
          EAggregateException(E)[i].Message);
      ReleaseExceptionObject;
    end;
end;

end.
