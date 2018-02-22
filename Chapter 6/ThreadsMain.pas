unit ThreadsMain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

const
  WM_PING = WM_USER;

type
  TfrmThreads = class(TForm)
    btnThread: TButton;
    btnFreeOnTerm: TButton;
    btnAnonymous: TButton;
    ListBox1: TListBox;
    btnStopThread: TButton;
    btnExceptThread: TButton;
    procedure btnThreadClick(Sender: TObject);
    procedure btnFreeOnTermClick(Sender: TObject);
    procedure btnAnonymousClick(Sender: TObject);
    procedure btnStopThreadClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnExceptThreadClick(Sender: TObject);
  private
    FExceptThread: TThread;
    FThread: TThread;
    FThreadFoT: TThread;
  protected
    procedure MsgPing(var msg: TMessage); message WM_PING;
    procedure PingMe;
    procedure ReportThreadTerminated(Sender: TObject);
  public
  end;

var
  frmThreads: TfrmThreads;

implementation

{$R *.dfm}

type
  TMyThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TFreeThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TExceptThread = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TfrmThreads.btnAnonymousClick(Sender: TObject);
var
  thread: TThread;
begin
  thread := TThread.CreateAnonymousThread(PingMe);
  thread.OnTerminate := ReportThreadTerminated;
  thread.Start;
end;

procedure TfrmThreads.btnExceptThreadClick(Sender: TObject);
begin
  FExceptThread := TExceptThread.Create(True);
  FExceptThread.FreeOnTerminate := true;
  FExceptThread.OnTerminate := ReportThreadTerminated;
  FExceptThread.Start;
end;

procedure TfrmThreads.btnFreeOnTermClick(Sender: TObject);
begin
  FThreadFoT := TFreeThread.Create(True);
  FThreadFoT.FreeOnTerminate := true;
  FThreadFoT.OnTerminate := ReportThreadTerminated;
  FThreadFoT.Start;
end;

procedure TfrmThreads.btnStopThreadClick(Sender: TObject);
begin
  FThread.Terminate;
  FThread.WaitFor;
  FreeAndNil(FThread);
  btnThread.Enabled := true;
  btnStopThread.Enabled := false;
end;

procedure TfrmThreads.btnThreadClick(Sender: TObject);
begin
  FThread := TMyThread.Create;
  FThread.OnTerminate := ReportThreadTerminated;
  btnThread.Enabled := false;
  btnStopThread.Enabled := true;
end;

procedure TfrmThreads.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FThread);
  if assigned(FThreadFoT) then
    FThreadFoT.Terminate;
end;

procedure TfrmThreads.MsgPing(var msg: TMessage);
begin
  ListBox1.Items.Add('Ping from thread ' + msg.WParam.ToString);
end;

procedure TfrmThreads.PingMe;
var
  i: Integer;
begin
  for i := 1 to 5 do
  begin
    PostMessage(Handle, WM_PING, TThread.Current.ThreadID, 0);
    Sleep(1000);
  end;
end;

procedure TfrmThreads.ReportThreadTerminated(Sender: TObject);
var
  thread: TThread;
begin
  Assert(Sender is TThread);
  thread := TThread(Sender);

  ListBox1.Items.Add('Terminating thread ' + thread.ThreadID.ToString);

  if assigned(thread.FatalException) then
  begin
    ListBox1.Items.Add(Format('Thread raised exception: [%s] %s',
      [thread.FatalException.ClassName, Exception(thread.FatalException).Message]));
    ReleaseExceptionObject;
  end;

  FThreadFoT := nil;
end;

{ TMyThread }

procedure TMyThread.Execute;
begin
  while not Terminated do
  begin
    PostMessage(frmThreads.Handle, WM_PING, ThreadID, 0);
    Sleep(1000);
  end;
end;

{ TFreeThread }

procedure TFreeThread.Execute;
var
  i: Integer;
begin
  for i := 1 to 5 do
  begin
    PostMessage(frmThreads.Handle, WM_PING, GetCurrentThreadID, 0);
    Sleep(1000);
    if Terminated then
      Exit;
  end;
end;

{ TExceptThread }

procedure TExceptThread.Execute;
begin
  Sleep(1000);
  raise Exception.Create('Thread exception');
end;

end.
