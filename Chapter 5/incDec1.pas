unit incDec1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.SyncObjs, System.Threading, System.Diagnostics;

type
  TfrmIncDec = class(TForm)
    btnSingleThreaded: TButton;
    btnMultithreaded: TButton;
    ListBox1: TListBox;
    btnLocking: TButton;
    btnInterlocked: TButton;
    btnTMonitor: TButton;
    btnMutex: TButton;
    btnSpinlock: TButton;
    procedure btnSingleThreadedClick(Sender: TObject);
    procedure btnMultithreadedClick(Sender: TObject);
    procedure btnLockingClick(Sender: TObject);
    procedure btnInterlockedClick(Sender: TObject);
    procedure btnTMonitorClick(Sender: TObject);
    procedure btnMutexClick(Sender: TObject);
    procedure btnSpinlockClick(Sender: TObject);
  private
    FValue: integer;
    FLock: TCriticalSection;
    FMutex: TMutex;
    FTimer: TStopwatch;
    FSpinlock: TSpinlock;
    procedure IncValue;
    procedure LockedIncValue;
    procedure MonitorLockedIncValue;
    procedure MutexIncValue;
    procedure InterlockedIncValue;
    procedure SpinlockIncValue;
    procedure DecValue;
    procedure LockedDecValue;
    procedure MonitorLockedDecValue;
    procedure MutexDecValue;
    procedure InterlockedDecValue;
    procedure SpinlockDecValue;
    procedure StartTimer; inline;
    procedure StopTimer; inline;
    procedure LogValue(const name: string);
  public
  end;

var
  frmIncDec: TfrmIncDec;

implementation

{$R *.dfm}

const
  CNumRepeat = 10000000;

procedure RunInParallel(task1, task2: TProc);
var
  tasks: array [0..1] of ITask;
begin
  tasks[0] := TTask.Run(task1);
  tasks[1] := TTask.Run(task2);
  TTask.WaitForAll(tasks);
end;

{ TfrmIncDec }

procedure TfrmIncDec.StartTimer;
begin
  FTimer := TStopwatch.StartNew;
end;

procedure TfrmIncDec.StopTimer;
begin
  FTimer.Stop;
end;

procedure TfrmIncDec.btnSingleThreadedClick(Sender: TObject);
begin
  StartTimer;

  FValue := 0;
  IncValue;
  DecValue;

  StopTimer;
  LogValue('Single threaded');
end;

procedure TfrmIncDec.btnMultithreadedClick(Sender: TObject);
begin
  StartTimer;

  FValue := 0;
  RunInParallel(IncValue, DecValue);

  StopTimer;
  LogValue('Multithreaded');
end;

procedure TfrmIncDec.btnMutexClick(Sender: TObject);
begin
  StartTimer;

  FValue := 0;
  FMutex := TMutex.Create(nil, false, '');
  try

    RunInParallel(MutexIncValue, MutexDecValue);

  finally
    FreeAndNil(FLock);
  end;

  StopTimer;
  LogValue('Mutex');
end;

procedure TfrmIncDec.btnLockingClick(Sender: TObject);
begin
  StartTimer;

  FValue := 0;
  FLock := TCriticalSection.Create;
  try

    RunInParallel(LockedIncValue, LockedDecValue);

  finally
    FreeAndNil(FLock);
  end;

  StopTimer;
  LogValue('Critical section');
end;

procedure TfrmIncDec.btnInterlockedClick(Sender: TObject);
begin
  StartTimer;

  FValue := 0;
  RunInParallel(InterlockedIncValue, InterlockedDecValue);

  StopTimer;
  LogValue('Interlocked');
end;

procedure TfrmIncDec.btnSpinlockClick(Sender: TObject);
begin
  StartTimer;

  FValue := 0;
  FSpinlock := TSpinLock.Create(false);

  RunInParallel(SpinlockIncValue, SpinlockDecValue);

  StopTimer;
  LogValue('Spinlock');
end;

procedure TfrmIncDec.btnTMonitorClick(Sender: TObject);
begin
  StartTimer;

  FValue := 0;
  RunInParallel(MonitorLockedIncValue, MonitorLockedDecValue);

  StopTimer;
  LogValue('TMonitor');
end;

procedure TfrmIncDec.IncValue;
var
  i: integer;
  value: integer;
begin
  for i := 1 to CNumRepeat do begin
    value := FValue;
    FValue := value + 1;
  end;
end;

procedure TfrmIncDec.DecValue;
var
  i: integer;
  value: integer;
begin
  for i := 1 to CNumRepeat do begin
    value := FValue;
    FValue := value - 1;
  end;
end;

procedure TfrmIncDec.InterlockedIncValue;
var
  i: integer;
begin
  for i := 1 to CNumRepeat do
    TInterlocked.Increment(FValue);
end;

procedure TfrmIncDec.InterlockedDecValue;
var
  i: integer;
begin
  for i := 1 to CNumRepeat do
    TInterlocked.Decrement(FValue);
end;

procedure TfrmIncDec.LockedIncValue;
var
  i: integer;
  value: integer;
begin
  for i := 1 to CNumRepeat do begin
    FLock.Acquire;
    value := FValue;
    FValue := value + 1;
    FLock.Release;
  end;
end;

procedure TfrmIncDec.LogValue(const name: string);
begin
  ListBox1.Items.Add(Format('%s: %d [%d ms]', [name, FValue, FTimer.ElapsedMilliseconds]));
end;

procedure TfrmIncDec.MonitorLockedDecValue;
var
  value: integer;
  i: Integer;
begin
  for i := 1 to CNumRepeat do begin
    System.TMonitor.Enter(Self);
    value := FValue;
    FValue := value + 1;
    System.TMonitor.Exit(Self);
  end;
end;

procedure TfrmIncDec.MonitorLockedIncValue;
var
  value: integer;
  i: Integer;
begin
  for i := 1 to CNumRepeat do begin
    System.TMonitor.Enter(Self);
    value := FValue;
    FValue := value - 1;
    System.TMonitor.Exit(Self);
  end;
end;

procedure TfrmIncDec.MutexDecValue;
var
  i: integer;
  value: integer;
begin
  for i := 1 to CNumRepeat do begin
    FMutex.Acquire;
    value := FValue;
    FValue := value - 1;
    FMutex.Release;
  end;
end;

procedure TfrmIncDec.MutexIncValue;
var
  i: integer;
  value: integer;
begin
  for i := 1 to CNumRepeat do begin
    FMutex.Acquire;
    value := FValue;
    FValue := value + 1;
    FMutex.Release;
  end;
end;

procedure TfrmIncDec.SpinlockDecValue;
var
  i: integer;
  value: integer;
begin
  for i := 1 to CNumRepeat do begin
    FSpinlock.Enter;
    value := FValue;
    FValue := value - 1;
    FSpinlock.Exit;
  end;
end;

procedure TfrmIncDec.SpinlockIncValue;
var
  i: integer;
  value: integer;
begin
  for i := 1 to CNumRepeat do begin
    FSpinlock.Enter;
    value := FValue;
    FValue := value + 1;
    FSpinlock.Exit;
  end;
end;

procedure TfrmIncDec.LockedDecValue;
var
  i: integer;
  value: integer;
begin
  for i := 1 to CNumRepeat do begin
    FLock.Acquire;
    try
      value := FValue;
      FValue := value - 1;
    finally FLock.Release; end;
  end;
end;

end.
