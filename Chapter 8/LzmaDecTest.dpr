program LzmaDecTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Winapi.Windows,
  System.Win.Crtl,
  System.SysUtils;

{$L LzmaDec.obj}

procedure LzmaDec_Init(var state); cdecl; external;
procedure LzmaDec_Free(var state; alloc: pointer); cdecl; external;

//function  memcpy(dest, src: Pointer; count: size_t): Pointer; cdecl; external 'msvcrt.dll';

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
