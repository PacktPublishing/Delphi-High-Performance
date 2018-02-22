unit SlowCode_VCL_main_instrumented;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Generics.Collections,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Samples.Spin,
  _uAsmProfDllLoader;

type
  TfrmSlowCode = class(TForm)
    lblHowMany: TLabel;
    inpHowMany: TSpinEdit;
    btnTest: TButton;
    outResults: TMemo;
    procedure btnTestClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure ShowElements(data: TArray<Integer>);
    function SlowMethod(highBound: Integer): TArray<Integer>;
    function Filter(list: TList<Integer>): TArray<Integer>;
    function Reverse(s: string): string;
    function ElementInDataDivides(data: TList<Integer>;
      value: Integer): boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSlowCode: TfrmSlowCode;

implementation

{$R *.dfm}

function TfrmSlowCode.ElementInDataDivides(data: TList<Integer>; value: Integer): boolean;
var
  i: Integer;
begin
  Result := True;
  for i in data do
    if (value <> i) and ((value mod i) = 0) then
      Exit;
  Result := False;
end;

function TfrmSlowCode.Reverse(s: string): string;
var
  ch: char;
begin
  Result := '';
  for ch in s do
    Result := ch + Result;
end;

function TfrmSlowCode.Filter(list: TList<Integer>): TArray<Integer>;
var
  i: Integer;
  reversed: Integer;
begin
  SetLength(Result, 0);
  for i in list do
  begin
    reversed := StrToInt(Reverse(IntToStr(i)));
    if not ElementInDataDivides(list, reversed) then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[High(Result)] := i;
    end;
  end;
end;

procedure TfrmSlowCode.FormCreate(Sender: TObject);
begin
  if _uAsmProfDllLoader.LoadProfilerDll then
    _uAsmProfDllLoader.ShowProfileForm;
end;

function TfrmSlowCode.SlowMethod(highBound: Integer): TArray<Integer>;
var
  i: Integer;
  temp: TList<Integer>;
begin
  temp := TList<Integer>.Create;
  try
    for i := 2 to highBound do
      if not ElementInDataDivides(temp, i) then
        temp.Add(i);
    Result := Filter(temp);
  finally
    FreeAndNil(temp);
  end;
end;

procedure TfrmSlowCode.ShowElements(data: TArray<Integer>);
var
  i: integer;
  s: string;
begin
  s := '';
  for i in data do
    s := s + IntToStr(i) + ' ';
  outResults.Text := s;
end;

procedure TfrmSlowCode.btnTestClick(Sender: TObject);
var
  data: TArray<Integer>;
begin
  outResults.Text := '';
  outResults.Update;

  _uAsmProfDllLoader.StartProfiler(False);
  data := SlowMethod(inpHowMany.Value);
  _uAsmProfDllLoader.StopProfiler;

  ShowElements(data);
end;

end.
