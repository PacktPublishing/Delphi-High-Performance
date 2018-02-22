unit CppClassWrapperMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin;

type
  TfrmCppClassDemo = class(TForm)
    Label1: TLabel;
    SpinEdit1: TSpinEdit;
    btnWrap: TButton;
    ListBox1: TListBox;
    procedure btnWrapClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmCppClassDemo: TfrmCppClassDemo;

implementation

uses
  CppClassWrapper;

{$R *.dfm}

procedure TfrmCppClassDemo.btnWrapClick(Sender: TObject);
var
  cpp: TCppClass;
begin
  cpp := TCppClass.Create;
  try
    cpp.SetValue(SpinEdit1.Value);
    ListBox1.Items.Add(Format('square(%d) = %d', [SpinEdit1.Value, cpp.GetSquare]));
  finally
    FreeAndNil(cpp);
  end;
end;

procedure TfrmCppClassDemo.FormCreate(Sender: TObject);
begin
  TCppClass.InitializeWrapper;
end;

procedure TfrmCppClassDemo.FormDestroy(Sender: TObject);
begin
  TCppClass.FinalizeWrapper;
end;

end.
