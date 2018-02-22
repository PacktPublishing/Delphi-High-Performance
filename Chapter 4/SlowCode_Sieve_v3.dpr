program SlowCode_Sieve_v3;

{$APPTYPE CONSOLE}
{$R *.res}
{$OPTIMIZATION ON}
{$RANGECHECKS OFF}

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Classes;

function ElementInDataDivides(data: TList<Integer>; value: Integer): boolean;
var
  i: Integer;
  highBound: integer;
begin
  Result := True;
  highBound := Trunc(Sqrt(value));
  for i in data do begin
    if i > highBound then
      break;
    if (value <> i) and ((value mod i) = 0) then
      Exit;
  end;
  Result := False;
end;

function ReverseInt(value: Integer): Integer;
begin
  Result := 0;
  while value <> 0 do
  begin
    Result := Result * 10 + (value mod 10);
    value := value div 10;
  end;
end;

function Filter(list: TList<Integer>): TArray<Integer>;
var
  i: Integer;
  reversed: Integer;
  outIdx: Integer;
begin
  SetLength(Result, list.Count);
  outIdx := 0;
  for i in list do
  begin
    reversed := ReverseInt(i);
    if not ElementInDataDivides(list, reversed) then
    begin
      Result[outIdx] := i;
      Inc(outIdx);
    end;
  end;
  SetLength(Result, outIdx);
end;

procedure GeneratePrimes(highBound: Integer; list: TList<integer>);
var
  sieve: TArray<Integer>;
  i: Integer;
  num: Integer;
begin
  SetLength(sieve, highBound+1);
  FillChar(sieve[0], Length(sieve) * SizeOf(sieve[0]), 0);

  // initialize to 0, 0, 2, 3, 0, 5, 0, 7, 0, 9, 0, 11, ...
  num := 3;
  for i := 2 to (highBound + 1) div 2 do
  begin
    sieve[num] := num;
    Inc(num,2);
  end;
  sieve[2] := 2;

  num := 3;
  while num <= Sqrt(highBound) do
  begin
    if sieve[num] <> 0 then
    begin
      i := num * num;
      while i <= highBound do
      begin
        sieve[i] := 0;
        Inc(i, num);
      end;
    end;
    Inc(num, 2);
  end;

  num := 0;
  for i := 2 to highBound do
    if sieve[i] <> 0 then
      Inc(num);

  list.Capacity := num;
  for i := 2 to highBound do
  begin
    if sieve[i] <> 0 then
      list.Add(i);
  end;
end;

function SlowMethod(highBound: Integer): TArray<Integer>;
var
  i: Integer;
  temp: TList<Integer>;
begin
  temp := TList<Integer>.Create;
  try
    GeneratePrimes(highBound, temp);
    Result := Filter(temp);
  finally
    FreeAndNil(temp);
  end;
end;

procedure ShowElements(data: TArray<Integer>);
var
  i: integer;
begin
  for i in data do
    Write(i, ' ');
  Writeln;
end;

procedure Test;
var
  data: TArray<Integer>;
  highBound: Integer;
begin
  repeat
    Writeln('How many numbers (0 to exit)?');
    Write('> ');
    Readln(highBound);
    if highBound = 0 then
      Exit;

    data := SlowMethod(highBound);
    ShowElements(data);
  until false;
end;

begin
  try
    Test;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
