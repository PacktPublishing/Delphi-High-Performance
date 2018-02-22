unit SharedListMain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.SyncObjs,
  System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmSharedList = class(TForm)
    btnShared: TButton;
    ListBox1: TListBox;
    btnLocked: TButton;
    btnMREW: TButton;
    Button1: TButton;
    procedure btnSharedClick(Sender: TObject);
    procedure btnLockedClick(Sender: TObject);
    procedure btnMREWClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FList: TList<Integer>;
    FThreadList: TThreadList<Integer>;
    FLock: TCriticalSection;
    FMREW: TMREWSync;
    procedure RunReadersAndWriters(const name: string;
      const readerProc, writerProc: TProc);
  public
    procedure ListReader;
    procedure ListWriter;
    procedure LockedListReader;
    procedure LockedListWriter;
    procedure MREWListReader;
    procedure MREWListWriter;
    procedure ThreadListReader;
    procedure ThreadListWriter;
  end;

var
  frmSharedList: TfrmSharedList;

implementation

uses
  System.Threading, System.Diagnostics;

{$R *.dfm}

const
  CNumWrites = 1000;
  CNumReads  = 1000000;

  CNumWriters = 1;
  CNumReaders = 20;

procedure TfrmSharedList.RunReadersAndWriters(const name: string;
  const readerProc, writerProc: TProc);
var
  task: array [0 .. CNumReaders] of ITask;
  i: Integer;
  sw: TStopwatch;
begin
  FList := TList<Integer>.Create;
  FThreadList := TThreadList<Integer>.Create;

  task[0] := TTask.Create(writerProc);
  for i := 1 to CNumReaders do
    task[i] := TTask.Create(readerProc);

  sw := TStopwatch.StartNew;

  for i := 0 to CNumReaders do
    task[i].Start;

  TTask.WaitForAll(task);
  FreeAndNil(FThreadList);
  FreeAndNIl(FList);

  sw.Stop;
  ListBox1.Items.Add(Format('%s / elapsed time = %d', [name, sw.ElapsedMilliseconds]));
end;

procedure TfrmSharedList.btnLockedClick(Sender: TObject);
begin
  FLock := TCriticalSection.Create;
  RunReadersAndWriters('TCriticalSection', LockedListReader, LockedListWriter);
  FreeAndNil(FLock);
end;

procedure TfrmSharedList.btnSharedClick(Sender: TObject);
begin
  RunReadersAndWriters('Shared', ListReader, ListWriter);
end;

procedure TfrmSharedList.Button1Click(Sender: TObject);
begin
  RunReadersAndWriters('TThreadList', ThreadListReader, ThreadListWriter);
end;

procedure TfrmSharedList.btnMREWClick(Sender: TObject);
begin
  FMREW := TMREWSync.Create;
  RunReadersAndWriters('MREW', MREWListReader, MREWListWriter);
  FreeAndNil(FMREW);
end;

procedure TfrmSharedList.ListReader;
var
  i, j, a: Integer;
begin
  for i := 1 to CNumReads do
    for j := 0 to FList.Count - 1 do
      a := FList[j];
end;

procedure TfrmSharedList.ListWriter;
var
  i: Integer;
begin
  for i := 1 to CNumWrites do
  begin
    Sleep(1);
    if FList.Count > 10 then
      FList.Delete(Random(10))
    else
      FList.Add(Random(100));
  end;
end;

procedure TfrmSharedList.LockedListReader;
var
  i, j, a: Integer;
begin
  for i := 1 to CNumReads do
  begin
    FLock.Acquire;
    try
      for j := 0 to FList.Count - 1 do
        a := FList[j];
    finally FLock.Release; end;
  end;
end;

procedure TfrmSharedList.LockedListWriter;
var
  i: Integer;
begin
  for i := 1 to CNumWrites do
  begin
    Sleep(1);
    FLock.Acquire;
    try
      if FList.Count > 10 then
        FList.Delete(Random(10))
      else
        FList.Add(Random(100));
    finally FLock.Release; end;
  end;
end;

procedure TfrmSharedList.MREWListReader;
var
  i, j, a: Integer;
begin
  for i := 1 to CNumReads do
  begin
    FMREW.BeginRead;
    try
      for j := 0 to FList.Count - 1 do
        a := FList[j];
    finally FMREW.EndRead; end;
  end;
end;

procedure TfrmSharedList.MREWListWriter;
var
  i: Integer;
begin
  for i := 1 to CNumWrites do
  begin
    Sleep(1);
    FMREW.BeginWrite;
    try
      if FList.Count > 10 then
        FList.Delete(Random(10))
      else
        FList.Add(Random(100));
    finally FMREW.EndWrite; end;
  end;
end;

procedure TfrmSharedList.ThreadListReader;
var
  i, j, a: Integer;
  list: TList<Integer>;
begin
  for i := 1 to CNumReads do
  begin
    list := FThreadList.LockList;
    try
      for j := 0 to FList.Count - 1 do
        a := FList[j];
    finally FThreadList.UnlockList; end;
  end;
end;

procedure TfrmSharedList.ThreadListWriter;
var
  i: Integer;
  list: TList<Integer>;
begin
  for i := 1 to CNumWrites do
  begin
    Sleep(1);
    list := FThreadList.LockList;
    try
      if FList.Count > 10 then
        FList.Delete(Random(10))
      else
        FList.Add(Random(100));
    finally FThreadList.UnlockList; end;
  end;
end;

end.
