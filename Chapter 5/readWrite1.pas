unit readWrite1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  GpLists,
  System.SyncObjs, System.Threading, System.Diagnostics;

type
  TfrmReadWrite = class(TForm)
    btnReadWrite: TButton;
    ListBox1: TListBox;
    btnReadWriteLock: TButton;
    Button1: TButton;
    procedure btnReadWriteClick(Sender: TObject);
    procedure btnReadWriteLockClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FBuf: Int64;
    FPValue: PInt64;
    FValueList: TGpInt64List;
    FLock: TCriticalSection;
    FStart: TStopwatch;
    procedure Writer;
    procedure LockedWriter;
    procedure InterlockedWriter;
    procedure Reader;
    procedure LockedReader;
    procedure InterlockedReader;
  public
  end;

var
  frmReadWrite: TfrmReadWrite;

implementation

{$R *.dfm}

const
  CNumRepeat = 10000000;

function AsHexText(list: TGpInt64List; const delimiter: string): string;
var
  hsl  : TStringList;
  iItem: integer;
begin
  hsl := TStringList.Create;
  try
    for iItem := 0 to list.Count-2 do
      hsl.Add(Format('%.16x', [list[iItem]]));
    Result := hsl.Text;
    if delimiter <> '' then
      Result := StringReplace(Result, #13#10, delimiter, [rfReplaceAll]);
    if list.Count > 0 then
      Result := Result + Format('%.16x', [list[list.Count-1]]);
  finally FreeAndNil(hsl); end;
end;

procedure TfrmReadWrite.btnReadWriteClick(Sender: TObject);
var
  tasks: array [0..1] of ITask;
begin
  btnReadWrite.Enabled := false; btnReadWrite.Update;

  FPValue := @FBuf;
  FPValue^ := $7777777700000000;
  FValueList := TGpInt64List.Create;

  FValueList.Sorted := true;
  FValueList.Duplicates := dupIgnore;

  FStart := TStopwatch.StartNew;

  tasks[0] := TTask.Run(Writer);
  tasks[1] := TTask.Run(Reader);

  TTask.WaitForAll(tasks);
  FStart.Stop;

  ListBox1.Items.Add(AsHexText(FValueList, ', '));
  ListBox1.Items.Add('Total time: ' + FStart.ElapsedMilliseconds.ToString);

  FreeAndNil(FValueList);
  btnReadWrite.Enabled := true;
  btnReadWriteLock.Enabled := true;
end;

procedure TfrmReadWrite.btnReadWriteLockClick(Sender: TObject);
var
  tasks: array [0..1] of ITask;
begin
  btnReadWrite.Enabled := false; btnReadWrite.Update;

  FPValue := @FBuf;
  FPValue^ := $7777777700000000;
  FValueList := TGpInt64List.Create;

  FValueList.Sorted := true;
  FValueList.Duplicates := dupIgnore;

  FStart := TStopwatch.StartNew;

  tasks[0] := TTask.Run(LockedWriter);
  tasks[1] := TTask.Run(LockedReader);

  TTask.WaitForAll(tasks);
  FStart.Stop;

  ListBox1.Items.Add(AsHexText(FValueList, ', '));
  ListBox1.Items.Add('Locked total time: ' + FStart.ElapsedMilliseconds.ToString);

  FreeAndNil(FValueList);
  btnReadWrite.Enabled := true;
  btnReadWriteLock.Enabled := true;
end;

procedure TfrmReadWrite.Button1Click(Sender: TObject);
var
  tasks: array [0..1] of ITask;
begin
  btnReadWrite.Enabled := false; btnReadWrite.Update;

  FPValue := @FBuf;
  FPValue^ := $7777777700000000;
  FValueList := TGpInt64List.Create;

  FValueList.Sorted := true;
  FValueList.Duplicates := dupIgnore;

  FStart := TStopwatch.StartNew;

  tasks[0] := TTask.Run(InterlockedWriter);
  tasks[1] := TTask.Run(InterlockedReader);

  TTask.WaitForAll(tasks);
  FStart.Stop;

  ListBox1.Items.Add(AsHexText(FValueList, ', '));
  ListBox1.Items.Add('Interlocked total time: ' + FStart.ElapsedMilliseconds.ToString);

  FreeAndNil(FValueList);
  btnReadWrite.Enabled := true;
  btnReadWriteLock.Enabled := true;
end;

procedure TfrmReadWrite.FormCreate(Sender: TObject);
begin
  FLock := TCriticalSection.Create;
end;

procedure TfrmReadWrite.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FLock);
end;

procedure TfrmReadWrite.InterlockedReader;
var
  i: integer;
  value: int64;
begin
  for i := 1 to CNumRepeat do begin
    value := TInterlocked.Read(FPValue^);
    FValueList.Add(value);
  end;
end;

procedure TfrmReadWrite.InterlockedWriter;
var
  i: integer;
begin
  for i := 1 to CNumRepeat do begin
    TInterlocked.Exchange(FPValue^, $7777777700000000);
    TInterlocked.Exchange(FPValue^, $0000000077777777);
  end;
end;

procedure TfrmReadWrite.LockedReader;
var
  i: integer;
  value: int64;
begin
  for i := 1 to CNumRepeat do begin
    FLock.Acquire;
    value := FPValue^;
    FLock.Release;
    FValueList.Add(value);
  end;
end;

procedure TfrmReadWrite.LockedWriter;
var
  i: integer;
begin
  for i := 1 to CNumRepeat do begin
    FLock.Acquire;
    FPValue^ := $7777777700000000;
    FLock.Release;
    FLock.Acquire;
    FPValue^ := $0000000077777777;
    FLock.Release;
  end;
end;

procedure TfrmReadWrite.Reader;
var
  i: integer;
begin
  for i := 1 to CNumRepeat do
    FValueList.Add(FPValue^);
end;

procedure TfrmReadWrite.Writer;
var
  i: integer;
begin
  for i := 1 to CNumRepeat do begin
    FPValue^ := $7777777700000000;
    FPValue^ := $0000000077777777;
  end;
end;

end.
