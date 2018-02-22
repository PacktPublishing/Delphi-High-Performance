unit ThreadsCommMain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.Rtti, System.Types, System.SysUtils, System.Variants, System.Classes,
  System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin,
  DHPThreads;

type
  TfrmThreadComm = class(TForm)
    btnStart: TButton;
    ListBox1: TListBox;
    inpPing: TSpinEdit;
    btnChangePing: TButton;
    btnStop: TButton;
    btnStartTimer: TButton;
    btnChangePingTimer: TButton;
    btnStopTimer: TButton;
    inpPingTimer: TSpinEdit;
    procedure btnStartClick(Sender: TObject);
    procedure btnChangePingClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure btnStartTimerClick(Sender: TObject);
    procedure btnChangePingTimerClick(Sender: TObject);
    procedure btnStopTimerClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FThread: TCommThread;
    FTimerThread: TCommTimerThread;
  strict protected
    procedure ProcessThreadMessages(value: TValue);
  public
  end;

var
  frmThreadComm: TfrmThreadComm;

implementation

{$R *.dfm}

type
  TPingMsg = TPair<integer,cardinal>;

  TMyThread = class(TCommThread)
  protected
    procedure Execute; override;
  end;

  TMyTimerThread = class(TCommTimerThread)
  strict private
    FPingMsg: integer;
  strict protected
    procedure SendPing;
  protected
    procedure Initialize; override;
    procedure ProcessMessage(const msg: TValue); override;
    procedure ProcessTimer; override;
  end;

procedure TfrmThreadComm.btnChangePingClick(Sender: TObject);
begin
  FThread.SendToThread(inpPing.Value);
end;

procedure TfrmThreadComm.btnChangePingTimerClick(Sender: TObject);
begin
  FTimerThread.SendToThread(inpPingTimer.Value);
end;

procedure TfrmThreadComm.btnStartClick(Sender: TObject);
begin
  FThread := TMyThread.Create(ProcessThreadMessages);
  FThread.SendToThread(42);
  btnStart.Enabled := false;
  btnStop.Enabled := true;
  btnChangePing.Enabled := true;
end;

procedure TfrmThreadComm.btnStartTimerClick(Sender: TObject);
begin
  FTimerThread := TMyTimerThread.Create(ProcessThreadMessages);
  FTimerThread.Interval := 5000;
  FTimerThread.SendToThread(42);
  btnStartTimer.Enabled := false;
  btnStopTimer.Enabled := true;
  btnChangePingTimer.Enabled := true;
end;

procedure TfrmThreadComm.btnStopClick(Sender: TObject);
begin
  FreeAndNil(FThread);
  btnStart.Enabled := true;
  btnStop.Enabled := false;
  btnChangePing.Enabled := false;
end;

procedure TfrmThreadComm.btnStopTimerClick(Sender: TObject);
begin
  FreeAndNil(FTimerThread);
  btnStartTimer.Enabled := true;
  btnStopTimer.Enabled := false;
  btnChangePingTimer.Enabled := false;
end;

procedure TfrmThreadComm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FThread);
  FreeAndNil(FTimerThread);
end;

{ TMyThread }

procedure TMyThread.Execute;
var
  pingMsg: integer;
  value: TValue;
begin
  pingMsg := 0;
  if not SendToMain(TValue.From<TPingMsg>(TPingMsg.Create(pingMsg, ThreadID))) then
    raise Exception.Create('Queue full!');

  while Event.WaitFor(5000) in [wrSignaled, wrTimeout] do
  begin
    Event.ResetEvent;
    // message processing
    while (not Terminated) and GetMessage(value) do
      pingMsg := value.AsInteger;
    // termination
    if Terminated then
      break;
    // workload
    if not SendToMain(TValue.From<TPingMsg>(TPingMsg.Create(pingMsg, ThreadID))) then
      raise Exception.Create('Queue full!');
  end;
end;

procedure TfrmThreadComm.ProcessThreadMessages(value: TValue);
var
  pingMsg: TPingMsg;
begin
  pingMsg := value.AsType<TPingMsg>;
  ListBox1.Items.Add(Format('%s: %d from thread %d',
    [FormatDateTime('hh:nn:ss.zzz', Now), pingMsg.Key, pingMsg.Value]));
end;

{ TMyTimerThread }

procedure TMyTimerThread.Initialize;
begin
  SendPing;
end;

procedure TMyTimerThread.ProcessMessage(const msg: TValue);
begin
  FPingMsg := msg.AsInteger;
end;

procedure TMyTimerThread.ProcessTimer;
begin
  SendPing;
end;

procedure TMyTimerThread.SendPing;
begin
  if not SendToMain(TValue.From<TPingMsg>(TPingMsg.Create(FPingMsg, ThreadID))) then
    raise Exception.Create('Queue full!');
end;

end.
