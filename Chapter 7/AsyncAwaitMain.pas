unit AsyncAwaitMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmAsyncAwait = class(TForm)
    btnLongTask: TButton;
    btnLongTaskAsync: TButton;
    btnLongTaskAsync2: TButton;
    btnLongTaskAsync3: TButton;
    ListBox1: TListBox;
    procedure btnLongTaskClick(Sender: TObject);
    procedure btnLongTaskAsyncClick(Sender: TObject);
  private
    procedure LongTask;
    procedure Log(const msg: string);
  public
  end;

var
  frmAsyncAwait: TfrmAsyncAwait;

implementation

uses
  DHPThreading;

{$R *.dfm}

procedure TfrmAsyncAwait.Log(const msg: string);
begin
  TThread.ForceQueue(nil,
    procedure
    begin
      ListBox1.Items.Add(msg);
    end);
end;

procedure TfrmAsyncAwait.LongTask;
begin
  Log(Format('Long task started in thread %d', [TThread.Current.ThreadID]));
  Sleep(5000);
  Log(Format('Long task stopped in thread %d', [TThread.Current.ThreadID]));
end;

procedure TfrmAsyncAwait.btnLongTaskClick(Sender: TObject);
begin
  btnLongTask.Enabled := false;
  btnLongTask.Update;

  LongTask;

  btnLongTask.Enabled := true;
end;

procedure TfrmAsyncAwait.btnLongTaskAsyncClick(Sender: TObject);
begin
  Log(Format('Button click in thread %d', [TThread.Current.ThreadID]));

  (Sender as TButton).Enabled := false;

  Async(LongTask)
  .Await(
    procedure
    begin
      Log(Format('Await in thread %d', [TThread.Current.ThreadID]));
      (Sender as TButton).Enabled := true;
    end
  );
end;

end.
