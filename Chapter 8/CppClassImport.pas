unit CppClassImport;

interface

const
  CPP_CLASS_LIB = 'DllLib1.dll';

  function Initialize: integer;
    stdcall; external CPP_CLASS_LIB name 'Initialize' delayed;

  function Finalize: integer;
    stdcall; external CPP_CLASS_LIB name 'Finalize' delayed;

  function CreateCppClass(var index: integer): integer;
    stdcall; external CPP_CLASS_LIB name 'CreateCppClass' delayed;

  function DestroyCppClass(index: integer): integer;
    stdcall; external CPP_CLASS_LIB name 'DestroyCppClass' delayed;

  function CppClass_setValue(index: integer; value: integer): integer;
    stdcall; external CPP_CLASS_LIB name 'CppClass_setValue' delayed;

  function CppClass_getSquare(index: integer; var value: integer): integer;
    stdcall; external CPP_CLASS_LIB name 'CppClass_getSquare' delayed;

implementation

uses
  System.SysUtils;

function DelayedHook(notification: dliNotification;
  loadInfo: PDelayLoadInfo): Pointer; stdcall;
begin
  if notification = dliFailLoadLibrary then
    raise Exception.Create('Failed to load library: ' +  loadInfo.szDll)
  else if notification = dliFailGetProcAddress then begin
    if loadInfo.dlp.fImportByName then
      raise Exception.Create('Function ' + loadinfo.dlp.szProcName +
        ' not found in ' + loadInfo.szDll)
    else
      raise Exception.Create('Function with ordinal ' +
        loadInfo.dlp.dwOrdinal.ToString + ' not found in ' + loadInfo.szDll);
  end;
end;

initialization
  SetDliFailureHook2(DelayedHook);
finalization
  SetDliFailureHook2(nil);
end.
