unit IncDecCommMain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.Types, System.SysUtils, System.Variants, System.Classes,
  System.Generics.Collections, System.Threading, System.Diagnostics,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls;

const
  MSG_TASK_DONE = WM_USER;
  MSG_OBJ_INT   = WM_USER + 1;

type
  TfrmIncDecComm = class(TForm)
    btnMessage: TButton;
    ListBox1: TListBox;
    btnSynchronize: TButton;
    btnQueue: TButton;
    btnThQueueAndTImer: TButton;
    TimerCheckQueue: TTimer;
    btnAllocateHwnd: TButton;
    btnObjInt: TButton;
    procedure btnMessageClick(Sender: TObject);
    procedure btnSynchronizeClick(Sender: TObject);
    procedure btnQueueClick(Sender: TObject);
    procedure btnThQueueAndTImerClick(Sender: TObject);
    procedure TimerCheckQueueTimer(Sender: TObject);
    procedure btnAllocateHwndClick(Sender: TObject);
    procedure btnObjIntClick(Sender: TObject);
  private
    FValue: integer;
    FNumDone: integer;
    FResultQueue: TThreadedQueue<integer>;
    FTasks: array [0..1] of ITask;
    FTimer: TStopwatch;
    FMsgWnd: THandle;
    procedure Done(const name: string);
    procedure IncMessage(startValue: integer);
    procedure DecMessage(startValue: integer);
    procedure IncMsgHwnd(startValue: integer);
    procedure DecMsgHwnd(startValue: integer);
    procedure IncSynchronize(startValue: integer);
    procedure DecSynchronize(startValue: integer);
    procedure IncQueue(startValue: integer);
    procedure DecQueue(startValue: integer);
    procedure IncThQueue(startValue: integer);
    procedure DecThQueue(startValue: integer);
    procedure PartialResult(value: integer);
    procedure PartialResultQ(value: integer);
    procedure RunInParallel(task1, task2: TProc<integer>);
    procedure MsgWndProc(var msg: TMessage);
    procedure MsgTaskDone(var msg: TMessage); message MSG_TASK_DONE;
    procedure MsgObjInt(var msg: TMessage); message MSG_OBJ_INT;
  public
  end;

var
  frmIncDecComm: TfrmIncDecComm;

implementation

{$R *.dfm}

const
  CNumRepeat = 10000000;

{ TfrmIncDec }

procedure TfrmIncDecComm.btnAllocateHwndClick(Sender: TObject);
begin
  FTimer := TStopwatch.StartNew;

  FValue := 0;
  FNumDone := 0;
  FMsgWnd := AllocateHwnd(MsgWndProc);
  Assert(FMsgWnd <> 0);
  RunInParallel(IncMsgHwnd, DecMsgHwnd);
end;

procedure TfrmIncDecComm.btnMessageClick(Sender: TObject);
begin
  FTimer := TStopwatch.StartNew;

  FValue := 0;
  FNumDone := 0;
  RunInParallel(IncMessage, DecMessage);
end;

procedure TfrmIncDecComm.btnQueueClick(Sender: TObject);
begin
  FTimer := TStopwatch.StartNew;

  FValue := 0;
  FNumDone := 0;
  RunInParallel(IncQueue, DecQueue);
end;

procedure TfrmIncDecComm.btnSynchronizeClick(Sender: TObject);
begin
  FTimer := TStopwatch.StartNew;

  FValue := 0;
  FNumDone := 0;
  RunInParallel(IncSynchronize, DecSynchronize);
end;

procedure TfrmIncDecComm.btnThQueueAndTImerClick(Sender: TObject);
begin
  FTimer := TStopwatch.StartNew;

  FValue := 0;
  FNumDone := 0;
  FResultQueue := TThreadedQueue<integer>.Create(2, 0, 0);
  RunInParallel(IncThQueue, DecThQueue);
  TimerCheckQueue.Enabled := true;
end;

procedure TfrmIncDecComm.btnObjIntClick(Sender: TObject);
var
  tobj: TObject;
  iint: IInterface;
begin
  tobj := TObject.Create;
  iint := TInterfacedObject.Create;
  iint._AddRef;
  PostMessage(Handle, MSG_OBJ_INT, NativeUInt(tobj), NativeInt(iint));
end;

procedure TfrmIncDecComm.DecMessage(startValue: integer);
var
  i: integer;
  value: integer;
begin
  value := startValue;
  for i := 1 to CNumRepeat do
    value := value - 1;

  PostMessage(Handle, MSG_TASK_DONE, value, 0);
end;

procedure TfrmIncDecComm.DecMsgHwnd(startValue: integer);
var
  i: integer;
  value: integer;
begin
  value := startValue;
  for i := 1 to CNumRepeat do
    value := value - 1;

  PostMessage(FMsgWnd, MSG_TASK_DONE, value, 0);
end;

procedure TfrmIncDecComm.DecQueue(startValue: integer);
var
  i: integer;
  value: integer;
begin
  value := startValue;
  for i := 1 to CNumRepeat do
    value := value - 1;

  TThread.Queue(nil,
    procedure
    begin
      PartialResultQ(value);
    end);
end;

procedure TfrmIncDecComm.DecSynchronize(startValue: integer);
var
  i: integer;
  value: integer;
begin
  value := startValue;
  for i := 1 to CNumRepeat do
    value := value - 1;

  TThread.Synchronize(nil,
    procedure
    begin
      PartialResult(value);
    end);
end;

procedure TfrmIncDecComm.DecThQueue(startValue: integer);
var
  i: integer;
  value: integer;
begin
  value := startValue;
  for i := 1 to CNumRepeat do
    value := value - 1;

  Assert(FResultQueue.PushItem(value) = wrSignaled);
end;

procedure TfrmIncDecComm.Done(const name: string);
begin
  FTasks[0] := nil;
  FTasks[1] := nil;
  FreeAndNil(FResultQueue);
  TimerCheckQueue.Enabled := false;
  if FMsgWnd <> 0 then
  begin
    DeallocateHwnd(FMsgWnd);
    FMsgWnd := 0;
  end;

  ListBox1.Items.Add(Format('%s: %d [%d ms]', [name, FValue, FTimer.ElapsedMilliseconds]));
end;

procedure TfrmIncDecComm.IncMessage(startValue: integer);
var
  i: integer;
  value: integer;
begin
  value := startValue;
  for i := 1 to CNumRepeat do
    value := value + 1;

  PostMessage(Handle, MSG_TASK_DONE, value, 0);
end;

procedure TfrmIncDecComm.IncMsgHwnd(startValue: integer);
var
  i: integer;
  value: integer;
begin
  value := startValue;
  for i := 1 to CNumRepeat do
    value := value + 1;

  PostMessage(FMsgWnd, MSG_TASK_DONE, value, 0);
end;

procedure TfrmIncDecComm.IncQueue(startValue: integer);
var
  i: integer;
  value: integer;
begin
  value := startValue;
  for i := 1 to CNumRepeat do
    value := value + 1;

  TThread.Queue(nil,
    procedure
    begin
      PartialResultQ(value);
    end);
end;

procedure TfrmIncDecComm.IncSynchronize(startValue: integer);
var
  i: integer;
  value: integer;
begin
  value := startValue;
  for i := 1 to CNumRepeat do
    value := value + 1;

  TThread.Synchronize(nil,
    procedure
    begin
      PartialResult(value);
    end);
end;

procedure TfrmIncDecComm.IncThQueue(startValue: integer);
var
  i: integer;
  value: integer;
begin
  value := startValue;
  for i := 1 to CNumRepeat do
    value := value + 1;

  Assert(FResultQueue.PushItem(value) = wrSignaled);
end;

procedure TfrmIncDecComm.MsgObjInt(var msg: TMessage);
var
  tobj: TObject;
  iint: IInterface;
begin
  tobj := TObject(msg.WParam);
  tobj.Free;
  iint := IInterface(msg.LParam);
  iint._Release;
end;

procedure TfrmIncDecComm.MsgTaskDone(var msg: TMessage);
begin
  Inc(FValue, msg.WParam);
  Inc(FNumDone);
  if FNumDone = 2 then
    Done('Windows message');
end;

procedure TfrmIncDecComm.MsgWndProc(var msg: TMessage);
begin
  if Msg.Msg = MSG_TASK_DONE then
  begin
    Inc(FValue, msg.WParam);
    Inc(FNumDone);
    if FNumDone = 2 then
      Done('Windows message');
  end
  else
    DefWindowProc(FMsgWnd, msg.Msg, msg.wParam, msg.lParam);
end;

procedure TfrmIncDecComm.PartialResult(value: integer);
begin
  Inc(FValue, value);
  Inc(FNumDone);
  if FNumDone = 2 then
    Done('Synchronize');
end;

procedure TfrmIncDecComm.PartialResultQ(value: integer);
begin
  Inc(FValue, value);
  Inc(FNumDone);
  if FNumDone = 2 then
    Done('Queue');
end;

procedure TfrmIncDecComm.RunInParallel(task1, task2: TProc<integer>);
begin
  FNumDone := 0;
  FTasks[0] := TTask.Run(procedure begin task1(0); end);
  FTasks[1] := TTask.Run(procedure begin task2(0); end);
end;

procedure TfrmIncDecComm.TimerCheckQueueTimer(Sender: TObject);
var
  qsize: integer;
  value: integer;
begin
  while FResultQueue.PopItem(qsize, value) = wrSignaled do begin
    FValue := FValue + value;
    Inc(FNumDone);
    if FNumDone = 2 then begin
      Done('TThreadedQueue + TTimer');
      break; //while
    end;
  end;
end;

end.
