program SlowCode_Stopwatch;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Classes,
  System.Diagnostics;

var
  Timing_ElementInData: TStopwatch;
  Timing_Filter: TStopwatch;
  Timing_SlowMethod: TStopwatch;

function ElementInDataDivides(data: TList<Integer>; value: Integer): boolean;
var
  i: Integer;
begin
  Timing_ElementInData.Start;
  try

    Result := True;
    for i in data do
      if (value <> i) and ((value mod i) = 0) then
        Exit;
    Result := False;

  finally
    Timing_ElementInData.Stop;
  end;
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
  Timing_Filter.Start;

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

  Timing_Filter.Stop;
end;

function SlowMethod(highBound: Integer): TArray<Integer>;
var
  i: Integer;
  temp: TList<Integer>;
  sw: TStopwatch;
begin
  sw := TStopwatch.StartNew;

  temp := TList<Integer>.Create;
  try
    for i := 2 to highBound do
      if not ElementInDataDivides(temp, i) then
        temp.Add(i);
    Result := Filter(temp);
  finally
    FreeAndNil(temp);
  end;

  sw.Stop;
  Writeln('SlowMethod: ', sw.ElapsedMilliseconds, ' ms');
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

procedure InitializeTiming;
var
  i: Integer;
begin
  Timing_ElementInData := TStopwatch.Create;
  Timing_Filter := TStopwatch.Create;
  Timing_SlowMethod := TStopwatch.Create;
end;

procedure ShowTimingResults;
begin
  Writeln('Total time spent in SlowMethod: ', Timing_SlowMethod.ElapsedMilliseconds, ' ms');
  Writeln('Total time spent in ElementInDataDivides: ', Timing_ElementInData.ElapsedMilliseconds, ' ms');
  Writeln('Total time spent in Filter: ', Timing_Filter.ElapsedMilliseconds, ' ms');
end;

begin
  try
    InitializeTiming;

    Test;

    ShowTimingResults;
    Write('Press Enter to exit');
    Readln;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
