unit AllocateMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmAllocate = class(TForm)
    btnAllocClass: TButton;
    btnAllocRecord: TButton;
    ListBox1: TListBox;
    btnAllocGeneric: TButton;
    procedure btnAllocClassClick(Sender: TObject);
    procedure btnAllocRecordClick(Sender: TObject);
    procedure btnAllocGenericClick(Sender: TObject);
  private
  public
  end;

var
  frmAllocate: TfrmAllocate;

implementation

uses
  System.Diagnostics;

const
  CNumNodes = 10000000;

type
  TNodeObj = class
    Left, Right: TNodeObj;
    Data: NativeUInt;
  end;

  PNodeRec = ^TNodeRec;
  TNodeRec = record
    Left, Right: PNodeRec;
    Data: NativeUInt;
  end;

  TTree<T> = class
  strict private type
    TNodeObj<T1> = class
      Left, Right: TNodeObj<T1>;
      Data: T1;
    end;

    PNodeRec = ^TNodeRec;
    TNodeRec<T1> = record
      Left, Right: PNodeRec;
      Data: T1;
    end;
  public
    function AllocateObj(numNodes: integer): int64;
    function AllocateRec(numNodes: integer): int64;
  end;


{$R *.dfm}

{$RANGECHECKS OFF}

procedure TfrmAllocate.btnAllocClassClick(Sender: TObject);
var
  sw: TStopwatch;
  i: Integer;
  nodes: TArray<TNodeObj>;
  createTime: Int64;
begin
  SetLength(nodes, CNumNodes);
  sw := TStopwatch.StartNew;
  for i := 0 to CNumNodes-1 do
    nodes[i] := TNodeObj.Create;
  createTime := sw.ElapsedMilliseconds;
  sw := TStopwatch.StartNew;
  for i := 0 to CNumNodes-1 do
    nodes[i].Free;
  sw.Stop;
  ListBox1.Items.Add(Format('Objects: %d ms [Create] / %d ms [Free]',
    [createTime, sw.ElapsedMilliseconds]));
end;

procedure TfrmAllocate.btnAllocRecordClick(Sender: TObject);
var
  sw: TStopwatch;
  i: Integer;
  nodes: TArray<PNodeRec>;
  createTime: Int64;
begin
  SetLength(nodes, CNumNodes);
  sw := TStopwatch.StartNew;
  for i := 0 to CNumNodes-1 do
    New(nodes[i]);
  createTime := sw.ElapsedMilliseconds;
  sw := TStopwatch.StartNew;
  for i := 0 to CNumNodes-1 do
    Dispose(nodes[i]);
  sw.Stop;
  ListBox1.Items.Add(Format('Records: %d ms [New] / %d ms [Dispose]',
    [createTime, sw.ElapsedMilliseconds]));
end;

{ TTree<T> }

procedure TfrmAllocate.btnAllocGenericClick(Sender: TObject);
var
  tree: TTree<String>;
begin
  tree := TTree<String>.Create;
  try
    ListBox1.Items.Add(Format('Objects<string>: %d ms', [tree.AllocateObj(CNumNodes)]));
    ListBox1.Items.Add(Format('Records<string>: %d ms', [tree.AllocateRec(CNumNodes)]));
  finally
    FreeAndNil(tree);
  end;
end;

function TTree<T>.AllocateObj(numNodes: integer): int64;
var
  sw: TStopwatch;
  i: Integer;
  nodes: TArray<TNodeObj<T>>;
begin
  SetLength(nodes, CNumNodes);
  sw := TStopwatch.StartNew;
  for i := 0 to CNumNodes-1 do
    nodes[i] := TNodeObj<T>.Create;
  for i := 0 to CNumNodes-1 do
    nodes[i].Free;
  Result := sw.ElapsedMilliseconds;
end;

function TTree<T>.AllocateRec(numNodes: integer): int64;
var
  sw: TStopwatch;
  i: Integer;
  nodes: TArray<PNodeRec>;
begin
  SetLength(nodes, CNumNodes);
  sw := TStopwatch.StartNew;
  for i := 0 to CNumNodes-1 do
    New(nodes[i]);
  for i := 0 to CNumNodes-1 do
    Dispose(nodes[i]);
  Result := sw.ElapsedMilliseconds;
end;

var
  s: string;
initialization
  s := '42';
  sleep(0);
end.
