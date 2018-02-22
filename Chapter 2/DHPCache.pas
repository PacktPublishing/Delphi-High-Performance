unit DHPCache;

/// This unit implements a fast O(1) limited-size cache.

interface

uses
  System.Generics.Collections;

type
  ///  The TDHPCache class maintains a dictionary of (key, index) pairs where
  ///  an 'index' is a pointer into a MRU linked list of values.
  ///  That allows us to remove the leastrecently used key from the dictionary
  //   when the cache becomes full in O(1) time.
  ///  As the linked list has a known maximum size, it is stored as an
  ///  array of list elements and 'index' from the dictionary is just the
  ///  element number.

  TDHPCache<K,V> = class
  strict private type
    TListElement = record
      Next : Integer;
      Prev : Integer;
      Key  : K;
      Value: V;
    end;
  var
    FCache     : TDictionary<K,Integer>;
    FFreeList  : Integer;
    FHead      : Integer;
    FKeys      : TArray<TListElement>;
    FOwnsValues: boolean;
    FTail      : Integer;
  strict protected
    procedure AddElement(const key: K; const value: V);
    procedure BuildLinkedList(numElements: Integer);
    procedure DestroyOwnedValues;
    function GetFree: Integer;
    procedure InsertInFront(elementIdx: Integer);
    function IsFull: boolean;
    procedure MoveToFront(elementIdx: Integer);
    function RemoveOldest: Integer;
    procedure Unlink(element: Integer);
    procedure UpdateElement(element: Integer; const key: K; const value: V);
  public
    constructor Create(ANumElements: Integer; AOwnsValues: boolean = false);
    destructor  Destroy; override;
    function TryGetValue(const key: K; var value: V): boolean;
    procedure Update(const key: K; const value: V);
  end;

implementation

uses
  System.SysUtils;

{ TDHPCache<K, V> }

constructor TDHPCache<K, V>.Create(ANumElements: Integer;
  AOwnsValues: boolean);
begin
  inherited Create;
  FOwnsValues := AOwnsValues;
  FCache := TDictionary<K,Integer>.Create(ANumElements);
  BuildLinkedList(ANumElements);
end;

destructor TDHPCache<K, V>.Destroy;
begin
  if FOwnsValues then
    DestroyOwnedValues;
  FreeAndNil(FCache);
  inherited;
end;

procedure TDHPCache<K, V>.DestroyOwnedValues;
begin
  while FHead >= 0 do begin
    PObject(@FKeys[FHead].Value)^.DisposeOf;
    FHead := FKeys[FHead].Next;
  end;
end;

procedure TDHPCache<K, V>.AddElement(const key: K; const value: V);
var
  element: integer;
begin
  if IsFull then
    element := RemoveOldest
  else
    element := GetFree;
  InsertInFront(element);
  FKeys[element].Key := key;
  FKeys[element].Value := value;
  FCache.Add(key, element);
end;

procedure TDHPCache<K, V>.BuildLinkedList(numElements: Integer);
var
  i: Integer;
begin
  SetLength(FKeys, numElements);

  for i := 0 to numElements - 2 do
    FKeys[i].Next := i + 1;
  FKeys[numElements - 1].Next := -1;

  for i := 1 to numElements - 1 do
    FKeys[i].Prev := i - 1;
  FKeys[0].Prev := -1;

  FHead := -1;
  FTail := -1;
  FFreeList := 0;
end;

function TDHPCache<K, V>.GetFree: Integer;
begin
  if FFreeList < 0 then
    raise Exception.Create('TDHPCache<K, V>.GetFree: Free list is empty!');
  Result := FFreeList;
  FFreeList := FKeys[FFreeList].Next;
end;

procedure TDHPCache<K, V>.InsertInFront(elementIdx: Integer);
begin
  FKeys[elementIdx].Next := FHead;
  FKeys[elementIdx].Prev := -1;
  if FHead >= 0 then
    FKeys[FHead].Prev := elementIdx;
  FHead := elementIdx;
  if FTail < 0 then
    FTail := FHead;
end;

function TDHPCache<K, V>.IsFull: boolean;
begin
  Result := (FFreeList < 0);
end;

procedure TDHPCache<K, V>.MoveToFront(elementIdx: Integer);
begin
  Unlink(elementIdx);
  InsertInFront(elementIdx);
end;

function TDHPCache<K, V>.RemoveOldest: Integer;
var
  element: Integer;
begin
  if FTail < 0 then
    raise Exception.Create('TDHPCache<K, V>.RemoveOldest: List is empty!');
  Result := FTail;
  Unlink(FTail);
  FCache.Remove(FKeys[Result].Key);
  if FOwnsValues then
    PObject(@FKeys[Result].Value)^.DisposeOf;
end;

function TDHPCache<K, V>.TryGetValue(const key: K; var value: V): boolean;
var
  element: Integer;
begin
  Result := FCache.TryGetValue(key, element);
  if Result then
    value := FKeys[element].Value;
end;

procedure TDHPCache<K, V>.Unlink(element: Integer);
begin
  if FKeys[element].Next >= 0 then
    FKeys[FKeys[element].Next].Prev := FKeys[element].Prev
  else
  begin
    Assert(FTail = element);
    FTail := FKeys[element].Prev;
  end;
  if FKeys[element].Prev >= 0 then
    FKeys[FKeys[element].Prev].Next := FKeys[element].Next
  else
  begin
    Assert(FHead = element);
    FHead := FKeys[element].Next;
  end;
end;

procedure TDHPCache<K, V>.Update(const key: K; const value: V);
var
  element: Integer;
begin
  if FCache.TryGetValue(key, element) then
    UpdateElement(element, key, value)
  else
    AddElement(key, value);
end;

procedure TDHPCache<K, V>.UpdateElement(element: Integer; const key: K;
  const value: V);
var
  oldValue: V;
begin
  if not FOwnsValues then
    FKeys[element].Value := value
  else
  begin
    oldValue := FKeys[element].Value;
    if PObject(@value)^ <> PObject(@oldValue)^ then
    begin
      FKeys[element].Value := value;
      PObject(@oldValue)^.DisposeOf;
    end;
  end;
  MoveToFront(element);
end;

end.


