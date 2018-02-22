unit ParallelTasksMain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Threading,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin,
  Vcl.ExtCtrls;

type
  TfrmParallelTasks = class(TForm)
    btnCheckPrimes1: TButton;
    inpNumTasks: TSpinEdit;
    Label1: TLabel;
    ListBox1: TListBox;
    btnCheckPrimes2: TButton;
    btnRunTasks: TButton;
    btnCustomThreadPool: TButton;
    btnAsyncTask: TButton;
    btnException2: TButton;
    TimerCheckTask: TTimer;
    btnException1: TButton;
    procedure btnAsyncTaskClick(Sender: TObject);
    procedure btnCheckPrimes1Click(Sender: TObject);
    procedure btnCheckPrimes2Click(Sender: TObject);
    procedure btnRunTasksClick(Sender: TObject);
    function  PrepareTask(lowBound, highBound: integer; taskResult: PInteger): TProc;
    function  PrepareTask2(lowBound, highBound: integer; taskResult: PInteger): TProc;
    procedure btnCustomThreadPoolClick(Sender: TObject);
    procedure btnException2Click(Sender: TObject);
    procedure TimerCheckTaskTimer(Sender: TObject);
    procedure btnException1Click(Sender: TObject);
  private
    FNumRunning: int64;
    FMaxRunning: int64;
    FTask: ITask;
    procedure LongTask;
    procedure LongTaskCompleted;
    procedure LongTaskError(exc: Exception);
    procedure ExceptionTask;
    procedure ExplicitExceptionTask;
  public
  end;

var
  frmParallelTasks: TfrmParallelTasks;

implementation

uses
  System.Diagnostics, System.SyncObjs;

{$R *.dfm}

const
  CHighestNumber = 10000000;

{ TfrmParallelTasks }

procedure TfrmParallelTasks.LongTask;
begin
  Sleep(2000);
  TThread.Queue(nil, LongTaskCompleted);
end;

procedure TfrmParallelTasks.LongTaskCompleted;
begin
  FTask := nil;
  btnAsyncTask.Enabled := True;
end;

procedure TfrmParallelTasks.LongTaskError(exc: Exception);
begin
  ListBox1.Items.Add(Format('Task raised exception %s %s', [exc.ClassName, exc.Message]));
  ReleaseExceptionObject;
end;

procedure TfrmParallelTasks.btnAsyncTaskClick(Sender: TObject);
begin
  FTask := TTask.Run(LongTask);
  btnAsyncTask.Enabled := False;
end;

procedure TfrmParallelTasks.btnException1Click(Sender: TObject);
begin
  FTask := TTask.Run(ExplicitExceptionTask);
end;

procedure TfrmParallelTasks.btnException2Click(Sender: TObject);
begin
  FTask := TTask.Run(ExceptionTask);
  TimerCheckTask.Enabled := true;
end;

procedure SleepProc;
begin
  Sleep(2500);
end;

procedure TfrmParallelTasks.btnRunTasksClick(Sender: TObject);
var
  sw: TStopwatch;
  tasks: array [1..2] of ITask;
begin
  sw := TStopwatch.StartNew;
  tasks[1] := TTask.Run(SleepProc);
  tasks[2] := TTask.Create(procedure begin Sleep(2000)  end).Start;
  TTask.WaitForAll(tasks);
  sw.Stop;
  ListBox1.Items.Add(Format('Total time: %d ms', [sw.ElapsedMilliseconds]));
end;

procedure TfrmParallelTasks.TimerCheckTaskTimer(Sender: TObject);
var
  i: integer;
begin
  if assigned(FTask) then
  begin
    if FTask.Status in [TTaskStatus.Completed, TTaskStatus.Canceled, TTaskStatus.Exception] then
    begin
      if FTask.Status = TTaskStatus.Exception then
      try
        FTask.Wait(0);
      except
        on E: EAggregateException do
          for i := 0 to E.Count - 1 do
            ListBox1.Items.Add(Format('Task raised exception %s %s',
              [E[i].ClassName, E[i].Message]));
      end;
      FTask := nil;
      TimerCheckTask.Enabled := true;
    end;
  end;
end;

procedure TfrmParallelTasks.ExceptionTask;
begin
  Sleep(1000);
  raise Exception.Create('Task exception');
end;

procedure TfrmParallelTasks.ExplicitExceptionTask;
var
  exc: TObject;
begin
  try
    raise Exception.Create('Task exception');
    TThread.Queue(nil, LongTaskCompleted);
  except
    on E: Exception do
    begin
      exc := AcquireExceptionObject;
      TThread.Queue(nil,
        procedure
        begin
          LongTaskError(Exception(exc));
        end);
    end;
  end;
end;

function IsPrime(value: integer): boolean;
var
  i: Integer;
begin
  Result := (value > 1);
  if Result then
    for i := 2 to Round(Sqrt(value)) do
      if (value mod i) = 0 then
        Exit(False);
end;

function FindPrimes(lowBound, highBound: integer): integer;
var
  i: Integer;
begin
  Result := 0;
  for i := lowBound to highBound do
    if IsPrime(i) then
      Inc(Result);
end;

function TfrmParallelTasks.PrepareTask(lowBound, highBound: integer; taskResult: PInteger): TProc;
begin
  Result :=
    procedure
    begin
      taskResult^ := FindPrimes(lowBound, highBound);
    end;
end;

procedure TfrmParallelTasks.btnCheckPrimes1Click(Sender: TObject);
var
  aggregate: Integer;
  i: Integer;
  highBound: Integer;
  lowBound: Integer;
  numTasks: Integer;
  results: TArray<Integer>;
  sw: TStopwatch;
  tasks: TArray<ITask>;

begin
  sw := TStopwatch.StartNew;

  numTasks := inpNumTasks.Value;
  SetLength(tasks, numTasks);
  SetLength(results, numTasks);

  lowBound := 0;
  for i := 1 to numTasks do
  begin
    highBound := Round(CHighestNumber / numTasks * i);
    tasks[i-1] := TTask.Run(PrepareTask(lowBound, highBound, @results[i-1]));
    lowBound := highBound + 1;
  end;
  TTask.WaitForAll(tasks);

  aggregate := 0;
  for i in results do
    Inc(aggregate, i);

  sw.Stop;
  ListBox1.Items.Add(Format('%d prime numbers from 1 to %d found in %d ms with %d tasks',
    [aggregate, CHighestNumber, sw.ElapsedMilliseconds, numTasks]));
end;

function TfrmParallelTasks.PrepareTask2(lowBound, highBound: integer; taskResult: PInteger): TProc;
begin
  Result :=
    procedure
    var
      running: int64;
      max: int64;
    begin
      running := TInterlocked.Increment(FNumRunning);
      max := TInterlocked.Read(FMaxRunning);
      if running > max then
        TInterlocked.CompareExchange(FMaxRunning, running, max);
      TInterlocked.Add(taskResult^, FindPrimes(lowBound, highBound));
      TInterlocked.Decrement(FNumRunning);
    end;
end;

procedure TfrmParallelTasks.btnCheckPrimes2Click(Sender: TObject);
var
  aggregate: Integer;
  i: Integer;
  highBound: Integer;
  lowBound: Integer;
  numTasks: Integer;
  sw: TStopwatch;
  tasks: TArray<ITask>;

begin
  sw := TStopwatch.StartNew;

  numTasks := inpNumTasks.Value;
  SetLength(tasks, numTasks);
  aggregate := 0;

  FNumRunning := 0;
  FMaxRunning := 0;

  lowBound := 0;
  for i := 1 to numTasks do
  begin
    highBound := Round(CHighestNumber / numTasks * i);
    tasks[i-1] := TTask.Run(PrepareTask2(lowBound, highBound, @aggregate));
//    Sleep(1);
    lowBound := highBound + 1;
  end;
  TTask.WaitForAll(tasks);

  sw.Stop;
  ListBox1.Items.Add(Format(
    '%d prime numbers from 1 to %d found in %d ms with %d tasks, %d running at the same time',
    [aggregate, CHighestNumber, sw.ElapsedMilliseconds, numTasks, FMaxRunning]));
end;

procedure TfrmParallelTasks.btnCustomThreadPoolClick(Sender: TObject);
var
  aggregate: Integer;
  i: Integer;
  highBound: Integer;
  lowBound: Integer;
  numTasks: Integer;
  sw: TStopwatch;
  tasks: TArray<ITask>;
  tp: TThreadPool;

begin
  sw := TStopwatch.StartNew;

  numTasks := inpNumTasks.Value;
  SetLength(tasks, numTasks);
  aggregate := 0;

  FNumRunning := 0;
  FMaxRunning := 0;

  tp := TThreadPool.Create;
  if not tp.SetMinWorkerThreads(numTasks) then
    ListBox1.Items.Add('Failed to set minimum number of worker threads');

  lowBound := 0;
  for i := 1 to numTasks do
  begin
    highBound := Round(CHighestNumber / numTasks * i);
    tasks[i-1] := TTask.Run(PrepareTask2(lowBound, highBound, @aggregate), tp);
    lowBound := highBound + 1;
  end;
  TTask.WaitForAll(tasks);

  FreeAndNil(tp);

  sw.Stop;
  ListBox1.Items.Add(Format(
    '%d prime numbers from 1 to %d found in %d ms with %d tasks, %d running at the same time',
    [aggregate, CHighestNumber, sw.ElapsedMilliseconds, numTasks, FMaxRunning]));
end;

end.
