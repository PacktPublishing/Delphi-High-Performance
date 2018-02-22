program SlowCode_v2;

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

function SlowMethod(highBound: Integer): TArray<Integer>;
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
