program DecompressTest;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.Win.Crtl,
  System.SysUtils;

{$LINK decompress.obj}
{$LINK huffman.obj}
{$LINK bzlib.obj}
//{$LINK compress.obj}
//{$LINK blocksort.obj}

// because compiler is single-pass
//procedure BZ2_decompress; external;           //decompress.obj
//procedure BZ2_hbMakeCodeLengths; external;    //huffman.obj
//procedure BZ2_hbAssignCodes; external;        //huffman.obj

var
  BZ2_rNums: array[0..511] of Longint;
  BZ2_crc32Table: array[0..255] of Longint;

procedure bz_internal_error(errcode: Integer); cdecl;
begin
  raise Exception.CreateFmt('Compression Error %d', [errcode]);
end;

begin
  try
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
