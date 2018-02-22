unit CacheDemoMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TfrmCacheTest = class(TForm)
    Test: TButton;
    Memo1: TMemo;
    procedure TestClick(Sender: TObject);
  private
    procedure Log(const msg: string);
  public
  end;

var
  frmCacheTest: TfrmCacheTest;

implementation

uses
  DHPCache;

{$R *.dfm}

type
  TInteger = class
  strict private
    FValue: Integer;
  public class var
    NumCreated: integer;
    NumDestroyed: integer;
  public
    constructor Create(AValue: Integer);
    destructor Destroy; override;
    property Value: Integer read FValue;
  end;

procedure TfrmCacheTest.Log(const msg: string);
begin
  Memo1.Text := Memo1.Text + msg + #13#10;
end;

procedure TfrmCacheTest.TestClick(Sender: TObject);
var
  cache: TDHPCache<Integer,TInteger>;
  i: Integer;
  value: TInteger;
begin
  Log('Creating cache of 30 owned objects');
  cache := TDHPCache<Integer,TInteger>.Create(30, true);

  Log('Inserting 100 objects with values 1 to 100 into the cache');
  for i := 1 to 100 do
    cache.Update(i, TInteger.Create(i));

  Log('Values 71 to 100 should lie in the cache');
  for i := 71 to 100 do
    if not cache.TryGetValue(i, value) then
      Log(Format('*** Key %d was missing!', [i]))
    else if value.Value <> i then
      Log(Format('*** Value for key %d is wrong: %d', [i, value.Value]));

  Log('Values 1 to 70 should NOT lie in the cache');
  for i := 1 to 70 do
    if cache.TryGetValue(i, value) then
      Log(Format('*** Key %d was found in the cache with value %d', [i, value.Value]));

  Log('Updating key 71 so that it will moved to the head of the MRU list');
  cache.Update(71, TInteger.Create(127));

  Log('Adding key 101 to the cache');
  cache.Update(101, TInteger.Create(101 ));

  Log('Key 72 shoud be removed while key 71 should still be present with value of 127');
  if cache.TryGetValue(72, value) then
    Log('*** Key 72 was found in the cache');
  if not cache.TryGetValue(71, value) then
    Log('*** Key 71 was not found in the cache')
  else if value.Value <> 127 then
    Log(Format('*** Value for key 71 is wrong: %d', [i, value.Value]));

  Log('Destroying the cache');
  FreeAndNil(cache);

  Log(Format('Number of destroyed objects = %d of %d',
    [TInteger.NumDestroyed, TInteger.NumCreated]));
end;

{ TInteger }

constructor TInteger.Create(AValue: Integer);
begin
  inherited Create;
  Inc(NumCreated);
  FValue := AValue;
end;

destructor TInteger.Destroy;
begin
  Inc(NumDestroyed);
  inherited;
end;

end.
