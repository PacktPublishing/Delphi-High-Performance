unit LoggingMM;

interface

function InstallMM(const fileName: string): boolean;
function UninstallMM: boolean;

implementation

uses
  Winapi.Windows,
  System.SysUtils;

var
  MMIsInstalled: boolean;
  OldMM: TMemoryManagerEx;
  LoggingFile: THandle;

procedure Write(const s: PAnsiChar); overload;
var
  written: DWORD;
begin
  WriteFile(LoggingFile, s^, StrLen(s), written, nil);
end;

procedure Write(n: NativeUInt); overload;
var
  buf: array [1..18] of AnsiChar;
  i: Integer;
  digit: Integer;
begin
  buf[18] := #0;
  for i := 17 downto 2 do
  begin
    digit := n mod 16;
    n := n div 16;

    if digit < 10 then
      buf[i] := AnsiChar(digit + Ord('0'))
    else
      buf[i] := AnsiChar(digit - 10 + Ord('A'));
  end;
  buf[1] := '$';

  Write(@buf);
end;

procedure Writeln;
begin
  Write(#13#10);
end;

function LoggingGetMem(Size: NativeInt): Pointer;
begin
  Result := OldMM.GetMem(Size);

  Write('GetMem(');
  Write(Size);
  Write(') = ');
  Write(NativeUInt(Result));
  Writeln;
end;

function LoggingFreeMem(P: Pointer): Integer;
begin
  Result := OldMM.FreeMem(P);

  Write('FreeMem(');
  Write(NativeUInt(P));
  Write(') = ');
  Write(Result);
  Writeln;
end;

function LoggingReallocMem(P: Pointer; Size: NativeInt): Pointer;
begin
  Result := OldMM.ReallocMem(P, Size);
  Write('ReallocMem(');
  Write(NativeUInt(P));
  Write(',');
  Write(Size);
  Write(') = ');
  Write(NativeUInt(Result));
  Writeln;
end;

function LoggingAllocMem(Size: NativeInt): Pointer;
begin
  Result := OldMM.AllocMem(Size);
  Write('AllocMem(');
  Write(Size);
  Write(') = ');
  Write(NativeUInt(Result));
  Writeln;
end;

function InstallMM(const fileName: string): boolean;
var
  myMM: TMemoryManagerEx;
begin
  if MMIsInstalled then
    Exit(False);

  LoggingFile := CreateFile(PChar(fileName), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0);
  if LoggingFile = INVALID_HANDLE_VALUE then
    Exit(False);

  GetMemoryManager(OldMM);

  myMM.GetMem := @LoggingGetMem;
  myMM.FreeMem := @LoggingFreeMem;
  myMM.ReallocMem := @LoggingReallocMem;
  myMM.AllocMem := @LoggingAllocMem;
  myMM.RegisterExpectedMemoryLeak := nil;
  myMM.UnregisterExpectedMemoryLeak := nil;
  SetMemoryManager(myMM);

  MMIsInstalled := True;

  Result := True;
end;

function UninstallMM: boolean;
begin
  if not MMIsInstalled then
    Exit(False);

  SetMemoryManager(OldMM);

  if LoggingFile <> INVALID_HANDLE_VALUE then begin
    CloseHandle(LoggingFile);
    LoggingFile := INVALID_HANDLE_VALUE;
  end;

  MMIsInstalled := False;

  Result := True;
end;

initialization
  MMIsInstalled := false;
finalization
  if MMIsInstalled then
    UninstallMM;
end.
