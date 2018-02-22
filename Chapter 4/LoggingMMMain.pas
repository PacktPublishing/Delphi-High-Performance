unit LoggingMMMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmLoggingMM = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    procedure Button1Click(Sender: TObject);
  private
  strict protected
    procedure LoadLog(const logFile: string);
  public
  end;

var
  frmLoggingMM: TfrmLoggingMM;

implementation

uses
  System.Generics.Collections,
  LoggingMM;

{$R *.dfm}

procedure TfrmLoggingMM.Button1Click(Sender: TObject);
var
  list: TList<integer>;
  i: Integer;
  mmLog: String;
begin
  mmLog := ChangeFileExt(ParamStr(0), '_memory.log');
  if not InstallMM(mmLog) then
    ListBox1.Items.Add('Failed to install memory manager');

  list := TList<integer>.Create;
  for i := 1 to 1024 do
    list.Add(i);
  FreeAndNil(list);

  if not UninstallMM then
    ListBox1.Items.Add('Failed to uninstall memory manager');

  LoadLog(mmLog);
end;

procedure TfrmLoggingMM.LoadLog(const logFile: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    try
      sl.LoadFromFile(logFile);
    except
      on E: Exception do
        ListBox1.Items.Add('Failed to load log file: ' + E.Message);
    end;
    ListBox1.Items.AddStrings(sl);
  finally
    FreeAndNil(sl);
  end;
end;

end.
