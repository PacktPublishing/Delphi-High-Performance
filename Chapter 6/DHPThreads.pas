unit DHPThreads;

interface

uses
  System.Rtti, System.SysUtils, System.Classes, System.Generics.Collections,
  System.Diagnostics, System.SyncObjs;

type
  TCommThread = class(TThread)
  public type
    TMessageReceiver = TProc<TValue>;
  strict private
    FQueueToMain    : TThreadedQueue<TValue>;
    FQueueToThread  : TThreadedQueue<TValue>;
    FMessageReceiver: TMessageReceiver;
  protected
    Event: TEvent;
    function GetMessage(var value: TValue): boolean;
    procedure PushMessagesToReceiver;
    function SendToMain(const value: TValue): boolean;
    procedure TerminatedSet; override;
  public
    constructor Create(CreateSuspended: boolean); overload;
    constructor Create(MessageReceiver: TMessageReceiver; CreateSuspended: boolean = False;
      ToThreadQueueSize: integer = 1024; ToMainQueueSize: integer = 1024); overload;
    destructor Destroy; override;
    function SendToThread(const value: TValue): boolean;
  end;

  TCommTimerThread = class(TCommThread)
  strict private
    FInterval: integer;
    FTimer: TStopwatch;
  strict protected
    function CalculateTimeout: integer;
    function GetInterval: integer;
    procedure SetInterval(const Value: integer);
  protected
    procedure Cleanup; virtual;
    procedure Execute; override;
    procedure Initialize; virtual;
    procedure ProcessMessage(const msg: TValue); virtual;
    procedure ProcessTimer; virtual;
  public
    property Interval: integer read GetInterval write SetInterval;
  end;

implementation

{ TCommThread }

constructor TCommThread.Create(CreateSuspended: Boolean);
begin
  Create(nil, CreateSuspended);
end;

constructor TCommThread.Create(MessageReceiver: TMessageReceiver;
  CreateSuspended: boolean; ToThreadQueueSize, ToMainQueueSize: integer);
begin
  inherited Create(CreateSuspended);
  FQueueToThread := TThreadedQueue<TValue>.Create(ToThreadQueueSize, 0, 0);
  Event := TEvent.Create;
  FMessageReceiver := MessageReceiver;
  if assigned(MessageReceiver) then
    FQueueToMain := TThreadedQueue<TValue>.Create(ToMainQueueSize, 0, 0);
end;

destructor TCommThread.Destroy;
begin
  inherited;
  FreeAndNil(Event);
  FreeAndNil(FQueueToThread);
  FreeAndNil(FQueueToMain);
end;

function TCommThread.GetMessage(var value: TValue): boolean;
begin
  Result := (FQueueToThread.PopItem(value) = wrSignaled);
end;

procedure TCommThread.PushMessagesToReceiver;
var
  value: TValue;
begin
  // This method executes from the main thread!
  while FQueueToMain.PopItem(value) = wrSignaled do
    FMessageReceiver(value);
end;

function TCommThread.SendToMain(const value: TValue): boolean;
begin
  if not assigned(FQueueToMain) then
    raise Exception.Create('MessageReceiver method was not set in constructor!');
  Result := (FQueueToMain.PushItem(value) = wrSignaled);
  if Result then
    TThread.Queue(nil, PushMessagesToReceiver);
end;

function TCommThread.SendToThread(const value: TValue): boolean;
begin
  Result := (FQueueToThread.PushItem(value) = wrSignaled);
  if Result then
    Event.SetEvent;
end;

procedure TCommThread.TerminatedSet;
begin
  Event.SetEvent;
  inherited;
end;

{ TCommTimerThread }

function TCommTimerThread.CalculateTimeout: integer;
begin
  if FInterval = 0 then
    Result := INFINITE
  else if FTimer.ElapsedMilliseconds >= FInterval then
    Result := 0
  else
    Result := FInterval - FTimer.ElapsedMilliseconds;
end;

procedure TCommTimerThread.Cleanup;
begin
  // do nothing
end;

procedure TCommTimerThread.Execute;
var
  awaited: TWaitResult;
  timeout: Cardinal;
  value: TValue;
begin
  Initialize;
  try
    repeat
      awaited := Event.WaitFor(CalculateTimeout);
      event.ResetEvent;
      case awaited of
        wrSignaled:
          begin
            while (not Terminated) and GetMessage(value) do
              ProcessMessage(value);
            if Terminated then
              break;
          end;
        wrTimeout:
          begin
            FTimer.Reset;
            FTimer.Start;
            ProcessTimer;
          end
        else
          break; //Terminate thread
      end;
    until false;
  finally
    Cleanup;
  end;
end;

function TCommTimerThread.GetInterval: integer;
var
  interval: int64;
begin
  Result := TInterlocked.Read(interval);
end;

procedure TCommTimerThread.Initialize;
begin
  // do nothing
end;

procedure TCommTimerThread.ProcessMessage(const msg: TValue);
begin
  // do nothing
end;

procedure TCommTimerThread.ProcessTimer;
begin
  // do nothing
end;

procedure TCommTimerThread.SetInterval(const Value: integer);
begin
  TInterlocked.Exchange(FInterval, Value);
  FTimer := TStopwatch.StartNew;
end;

end.
