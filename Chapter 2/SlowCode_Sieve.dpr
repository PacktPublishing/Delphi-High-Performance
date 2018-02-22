program SlowCode_Sieve;

{$APPTYPE CONSOLE}
{$R *.res}

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

function Reverse(s: string): string;
var
  ch: char;
begin
  Result := '';
  for ch in s do
    Result := ch + Result;
end;

function Filter(list: TList<Integer>): TArray<Integer>;
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
