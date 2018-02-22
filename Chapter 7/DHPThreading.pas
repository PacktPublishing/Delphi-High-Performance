unit DHPThreading;

interface

uses
  System.SysUtils;

type
  IAsync = interface ['{190C1975-FFCF-47AD-B075-79BC8F4157DA}']
    procedure Await(const awaitProc: TProc);
  end;

  IJoin = interface ['{ED4B4531-B233-4A02-A09A-13EE488FCCA3}']
    procedure Await(const awaitProc: TProc); overload;
    procedure Await(const awaitProc: TProc<Exception>); overload;
  end;

function Async(const asyncProc: TProc): IAsync;
function Join(const async: array of TProc): IJoin;

implementation

uses
  Winapi.Windows,
  System.Classes, System.SyncObjs, System.Threading;

type
  TAsync = class(TInterfacedObject, IAsync)
  strict private
    FAsyncProc: TProc;
    FAwaitProc: TProc;
    FSelf: IAsync;
  strict protected
    procedure Run;
  public
    constructor Create(const asyncProc: TProc);
    procedure Await(const awaitProc: TProc);
  end;

  TJoin = class(TInterfacedObject, IJoin)
  strict private
    FAsyncProc: TArray<TProc>;
    FAwaitProc: TProc<Exception>;
    FExceptions: TArray<Exception>;
    FNumRunning: integer;
    FSelf: IJoin;
  strict protected
    procedure AppendException(E: Exception);
    function CreateAggregateException: Exception;
    function GetAsyncProc(i: integer): TProc;
    procedure Run(const asyncProc: TProc);
  public
    constructor Create(const asyncProc: array of TProc);
    procedure Await(const awaitProc: TProc); overload;
    procedure Await(const awaitProc: TProc<Exception>); overload;
  end;

function Async(const asyncProc: TProc): IAsync;
begin
  Result := TAsync.Create(asyncProc);
end;

function Join(const async: array of TProc): IJoin;
begin
  Result := TJoin.Create(async);
end;

{ TAsync }

constructor TAsync.Create(const asyncProc: TProc);
begin
  inherited Create;
  FAsyncProc := asyncProc;
end;

procedure TAsync.Run;
begin
  FAsyncProc();

  TThread.Queue(nil,
    procedure
    begin
      FAwaitProc();
      FSelf := nil;
    end
  );
end;

procedure TAsync.Await(const awaitProc: TProc);
begin
  FSelf := Self;
  FAwaitProc := awaitProc;
  TTask.Run(Run);
end;

{ TJoin }

constructor TJoin.Create(const asyncProc: array of TProc);
var
  i: integer;
begin
  inherited Create;
  SetLength(FAsyncProc, Length(asyncProc));
  for i := Low(asyncProc) to High(asyncProc) do
    FAsyncProc[i] := asyncProc[i];
end;

function TJoin.CreateAggregateException: Exception;
begin
  if Length(FExceptions) = 0 then
    Result := nil
  else
    Result := EAggregateException.Create(FExceptions);
end;

function TJoin.GetAsyncProc(i: integer): TProc;
begin
  Result :=
    procedure
    begin
      Run(FAsyncProc[i]);
    end;
end;

procedure TJoin.Await(const awaitProc: TProc);
begin
  Await(
    procedure (E: Exception)
    begin
      if assigned(E) then
        raise E;
      awaitProc();
    end);
end;

procedure TJoin.AppendException(E: Exception);
begin
  TMonitor.Enter(Self);
  try
    SetLength(FExceptions, Length(FExceptions) + 1);
    FExceptions[High(FExceptions)] := e;
  finally
    TMonitor.Exit(Self);
  end;
end;

procedure TJoin.Await(const awaitProc: TProc<Exception>);
var
  i: integer;
begin
  FAwaitProc := awaitProc;
  FNumRunning := Length(FAsyncProc);
  FSelf := Self;
  for i := Low(FAsyncProc) to High(FAsyncProc) do begin
    TTask.Run(GetAsyncProc(i));
    {$IF CompilerVersion = 32}Sleep(1);{$IFEND}
  end;
end;

procedure TJoin.Run(const asyncProc: TProc);
begin
  try
    asyncProc();
  except
    on E: Exception do
      AppendException(AcquireExceptionObject as Exception);
  end;

  if TInterlocked.Decrement(FNumRunning) = 0 then
    TThread.Queue(nil,
      procedure
      begin
        FAwaitProc(CreateAggregateException);
        FSelf := nil;
      end);
end;

end.
