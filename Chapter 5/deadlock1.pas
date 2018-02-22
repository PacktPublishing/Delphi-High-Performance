unit deadlock1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.Threading, System.SyncObjs;

type
  TSharedData = class
    Counter: int64;
    LockCounter: TCriticalSection;
    LockOther: TCriticalSection;
    constructor Create;
    destructor Destroy; override;
  end;

  TfrmDeadlock = class(TForm)
    btnTask1: TButton;
    btnTask2: TButton;
    Timer1: TTimer;
    btnTryTask1: TButton;
    btnTryTask2: TButton;
    procedure btnTask1Click(Sender: TObject);
    procedure btnTask2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnTryTask1Click(Sender: TObject);
    procedure btnTryTask2Click(Sender: TObject);
  private
    FShared: TSharedData;
    FTask1: ITask;
    FTask2: ITask;
  end;

var
  frmDeadlock: TfrmDeadlock;

implementation

{$R *.dfm}

procedure TaskProc1(const task: ITask; const shared: TSharedData);
begin
  while frmDeadlock.FTask1.Status <> TTaskStatus.Canceled do begin
    shared.LockCounter.Acquire;
    shared.LockOther.Acquire;
    shared.Counter := shared.Counter + 1;
    shared.LockOther.Release;
    shared.LockCounter.Release;
  end;
end;

procedure TaskProc2(const task: ITask; const shared: TSharedData);
begin
  while frmDeadlock.FTask1.Status <> TTaskStatus.Canceled do begin
    shared.LockOther.Acquire;
    shared.LockCounter.Acquire;
    shared.Counter := shared.Counter + 1;
    shared.LockCounter.Release;
    shared.LockOther.Release;
  end;
end;

procedure TryTaskProc1(const task: ITask; const shared: TSharedData);
begin
  while frmDeadlock.FTask1.Status <> TTaskStatus.Canceled do begin
    shared.LockCounter.Acquire;
    if shared.LockOther.TryEnter then
    begin
      shared.Counter := shared.Counter + 1;
      shared.LockOther.Leave;
    end;
    shared.LockCounter.Release;
  end;
end;

procedure TryTaskProc2(const task: ITask; const shared: TSharedData);
begin
  while frmDeadlock.FTask1.Status <> TTaskStatus.Canceled do begin
    shared.LockOther.Acquire;
    if shared.LockCounter.TryEnter then
    begin
      shared.Counter := shared.Counter + 1;
      shared.LockCounter.Leave;
    end;
    shared.LockOther.Release;
  end;
end;

{ TfrmDeadlock }

procedure TfrmDeadlock.btnTask1Click(Sender: TObject);
begin
  btnTask1.Enabled := false;
  btnTryTask1.Enabled := false;
  FTask1 := TTask.Run(
    procedure
    begin
      TThread.NameThreadForDebugging('task 1');
      TaskProc1(FTask1, FShared);
    end);
end;

procedure TfrmDeadlock.btnTask2Click(Sender: TObject);
begin
  btnTask2.Enabled := false;
  btnTryTask2.Enabled := false;
  FTask2 := TTask.Run(
    procedure
    begin
      TThread.NameThreadForDebugging('task 2');
      TaskProc2(FTask1, FShared);
    end);
end;

procedure TfrmDeadlock.btnTryTask1Click(Sender: TObject);
begin
  btnTask1.Enabled := false;
  btnTryTask1.Enabled := false;
  FTask1 := TTask.Run(
    procedure
    begin
      TThread.NameThreadForDebugging('task 1');
      TryTaskProc1(FTask1, FShared);
    end);
end;

procedure TfrmDeadlock.btnTryTask2Click(Sender: TObject);
begin
  btnTask2.Enabled := false;
  btnTryTask2.Enabled := false;
  FTask2 := TTask.Run(
    procedure
    begin
      TThread.NameThreadForDebugging('task 2');
      TryTaskProc2(FTask2, FShared);
    end);
end;

procedure TfrmDeadlock.FormCreate(Sender: TObject);
begin
  FShared := TSharedData.Create;
end;

procedure TfrmDeadlock.FormDestroy(Sender: TObject);
begin
  if assigned(FTask1) then begin
    FTask1.Cancel;
    try FTask1.Wait; except on E: EOperationCancelled do ; end;
  end;
  if assigned(FTask2) then begin
    FTask2.Cancel;
    try FTask2.Wait; except on E: EOperationCancelled do ; end;
  end;
  FreeAndNil(FShared);
end;

procedure TfrmDeadlock.Timer1Timer(Sender: TObject);
begin
  FShared.LockCounter.Acquire;
  Caption := IntToStr(FShared.Counter);
  FShared.LockCounter.Release;
end;

{ TSharedData }

constructor TSharedData.Create;
begin
  inherited Create;
  LockCounter := TCriticalSection.Create;
  LockOther := TCriticalSection.Create;
end;

destructor TSharedData.Destroy;
begin
  FreeAndNil(LockCounter);
  FreeAndNil(LockOther);
  inherited;
end;

end.
