unit CppClassMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin;

type
  TfrmCppClassDemo = class(TForm)
    Label1: TLabel;
    SpinEdit1: TSpinEdit;
    btnImportLib: TButton;
    ListBox1: TListBox;
    procedure btnImportLibClick(Sender: TObject);
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
  CppClassImport;

{$R *.dfm}

procedure TfrmCppClassDemo.btnImportLibClick(Sender: TObject);
var
  idxClass: Integer;
  value: Integer;
begin
  if CreateCppClass(idxClass) <> 0 then
    ListBox1.Items.Add('CreateCppClass failed')
  else if CppClass_setValue(idxClass, SpinEdit1.Value) <> 0 then
    ListBox1.Items.Add('CppClass_setValue failed')
  else if CppClass_getSquare(idxClass, value) <> 0 then
    ListBox1.Items.Add('CppClass_getSquare failed')
  else begin
    ListBox1.Items.Add(Format('square(%d) = %d', [SpinEdit1.Value, value]));
    if DestroyCppClass(idxClass) <> 0 then
      ListBox1.Items.Add('DestroyCppClass failed')
  end;
end;

procedure TfrmCppClassDemo.FormCreate(Sender: TObject);
begin
  if Initialize <> 0 then
    ListBox1.Items.Add('Initialize failed')
end;

procedure TfrmCppClassDemo.FormDestroy(Sender: TObject);
begin
  if Finalize <> 0 then
    ListBox1.Items.Add('Finalize failed');
end;

end.
