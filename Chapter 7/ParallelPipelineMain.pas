unit ParallelPipelineMain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  OtlCommon, OtlComm, OtlCollections, OtlTask, OtlTaskControl, OtlParallel;

const
  MSG_ERROR = WM_USER;
  MSG_OK    = WM_USER + 1;

type
  TStatistics = record
    Files: Int64;
    Lines: int64;
    Words: int64;
    Chars: int64;
  end;

  TfrmPipeline = class(TForm)
    Label1: TLabel;
    inpStartFolder: TEdit;
    btnSelectFolder: TButton;
    btnStart: TButton;
    ListBox1: TListBox;
    FileOpenDialog1: TFileOpenDialog;
    lblProcessing: TLabel;
    TimerUpdateProcessing: TTimer;
    procedure btnStartClick(Sender: TObject);
    procedure btnSelectFolderClick(Sender: TObject);
    procedure TimerUpdateProcessingTimer(Sender: TObject);
  private
    FLastProcessed: string;
    FPipeline: IOmniPipeline;
    procedure CreatePipeline;
    procedure Cleanup;
    procedure FolderScanner(const input, output: IOmniBlockingCollection;
      const task: IOmniTask);
    procedure FileReader(const input, output: IOmniBlockingCollection;
      const task: IOmniTask);
    function GenerateStatistics(sl: TStringList): TStatistics;
    procedure HandleFileReaderMessage(const task: IOmniTaskControl;
      const msg: TOmniMessage);
    procedure Merge(var aggregate: TStatistics; const stat: TStatistics);
    procedure ShowResult;
    procedure StatAggregator(const input, output: IOmniBlockingCollection;
      const task: IOmniTask);
    procedure StatCollector(const input: TOmniValue; var output: TOmniValue);
  public
  end;

var
  frmPipeline: TfrmPipeline;

implementation

uses
  DSiWin32;

{$R *.dfm}

procedure TfrmPipeline.btnSelectFolderClick(Sender: TObject);
begin
  if FileOpenDialog1.Execute then
    inpStartFolder.Text := FileOpenDialog1.FileName;
end;

procedure TfrmPipeline.btnStartClick(Sender: TObject);
begin
  if btnStart.Tag = 0 then begin
    ListBox1.Items.Add(inpStartFolder.Text);
    CreatePipeline;
    btnStart.Caption := 'Stop';
    btnStart.Tag := 1;
    TimerUpdateProcessing.Enabled := true;
  end
  else
    if assigned(FPipeline) then
      FPipeline.Cancel;
end;

procedure TfrmPipeline.CreatePipeline;
begin
  FPipeline := Parallel.Pipeline;
  FPipeline.Stage(FolderScanner);
  FPipeline.Stage(FileReader, Parallel.TaskConfig.OnMessage(HandleFileReaderMessage))
    .NumTasks(Environment.Process.Affinity.Count)
    .Throttle(1000);
  FPipeline.Stage(StatCollector)
    .NumTasks(2);
  FPipeline.Stage(StatAggregator);

  FPipeline.OnStopInvoke(ShowResult);

  FPipeline.Run;

  FPipeline.Input.Add(inpStartFolder.Text);
  FPipeline.Input.CompleteAdding;
end;

procedure TfrmPipeline.Cleanup;
begin
  FPipeline := nil;
  TimerUpdateProcessing.Enabled := false;
  lblProcessing.Visible := false;
  btnStart.Caption := 'Start';
  btnStart.Tag := 0;
end;

procedure TfrmPipeline.FolderScanner(
  const input, output: IOmniBlockingCollection;
  const task: IOmniTask);
var
  value: TOmniValue;
begin
  for value in input do begin
    DSiEnumFilesEx(
      IncludeTrailingPathDelimiter(value) + '*.pas', 0, true,
      procedure (const folder: string; S: TSearchRec;
        isAFolder: boolean; var stopEnum: boolean)
      begin
        stopEnum := task.CancellationToken.IsSignalled;
        if (not stopEnum) and (not isAFolder) then
          output.TryAdd(IncludeTrailingPathDelimiter(folder) + S.Name);
      end);
  end;
end;

function TfrmPipeline.GenerateStatistics(sl: TStringList): TStatistics;
var
  s: string;
begin
  Result := Default(TStatistics);
  Result.Files := 1;
  Result.Lines := sl.Count;

  for s in sl do begin
    Result.Chars := Result.Chars + Length(s);
    Result.Words := Result.Words + Length(s.Split([' ']));
  end;
end;

procedure TfrmPipeline.HandleFileReaderMessage(const task: IOmniTaskControl;
  const msg: TOmniMessage);
begin
  if msg.MsgID = MSG_ERROR then
    ListBox1.ItemIndex := ListBox1.Items.Add('*** ' + msg.MsgData)
  else
    FLastProcessed := msg.MsgData;
end;

procedure TfrmPipeline.Merge(var aggregate: TStatistics;
  const stat: TStatistics);
begin
  aggregate.Chars := aggregate.Chars + stat.Chars;
  aggregate.Words := aggregate.Words + stat.Words;
  aggregate.Lines := aggregate.Lines + stat.Lines;
  aggregate.Files := aggregate.Files + stat.Files;
end;

procedure TfrmPipeline.FileReader(const input, output: IOmniBlockingCollection;
  const task: IOmniTask);
var
  sl: TStringList;
  value: TOmniValue;
  outValue: TOmniValue;
begin
  for value in input do begin
    if task.CancellationToken.IsSignalled then
      break;

    sl := TStringList.Create;
    try
      sl.LoadFromFile(value);
      outValue.AsOwnedObject := sl;
      sl := nil;
      output.TryAdd(outValue);
      task.Comm.Send(MSG_OK, value);
    except
      task.Comm.Send(MSG_ERROR, value);
      FreeAndNil(sl);
    end;
  end;
end;

procedure TfrmPipeline.StatCollector(const input: TOmniValue;
  var output: TOmniValue);
var
  stat: TStatistics;
begin
  stat := GenerateStatistics(input.ToObject<TStringList>);
  output := TOmniValue.FromRecord<TStatistics>(stat);
end;

procedure TfrmPipeline.TimerUpdateProcessingTimer(Sender: TObject);
begin
  lblProcessing.Caption := FLastProcessed;
  lblProcessing.Visible := true;
end;

procedure TfrmPipeline.StatAggregator(const input,
  output: IOmniBlockingCollection; const task: IOmniTask);
var
  aggregate: TStatistics;
  stat: TStatistics;
  value: TOmniValue;
begin
  aggregate := Default(TStatistics);

  for value in input do begin
    if task.CancellationToken.IsSignalled then
      break;

    stat := value.ToRecord<TStatistics>;
    Merge(aggregate, stat);
  end;

  output.TryAdd(TOmniValue.FromRecord<TStatistics>(aggregate));
end;

procedure TfrmPipeline.ShowResult;
var
  value: TOmniValue;
  stat: TStatistics;
begin
  if FPipeline.Output.TryTake(value) then begin
    stat := value.ToRecord<TStatistics>;
    ListBox1.Items.Add(Format('Files: %d, Lines: %d, Words: %d, Characters: %d',
      [stat.Files, stat.Lines, stat.Words, stat.Chars]));
  end;
  Cleanup;
end;

end.
