unit CppClassWrapper;

interface

type
  TCppClass = class
  strict private
    FIndex: integer;
  public
    class procedure InitializeWrapper;
    class procedure FinalizeWrapper;
    constructor Create;
    destructor Destroy; override;
    procedure SetValue(value: integer);
    function GetSquare: integer;
  end;

implementation

uses
  System.SysUtils,
  CppClassImport;

{ TCppClass }

class procedure TCppClass.InitializeWrapper;
begin
  if Initialize <> 0 then
    raise Exception.Create('Initialize failed');
end;

class procedure TCppClass.FinalizeWrapper;
begin
  if Finalize <> 0 then
    raise Exception.Create('Finalize failed');
end;

constructor TCppClass.Create;
begin
  inherited Create;
  if CreateCppClass(FIndex) <> 0 then
    raise Exception.Create('CreateCppClass failed');
end;

destructor TCppClass.Destroy;
begin
  if DestroyCppClass(FIndex) <> 0 then
    raise Exception.Create('DestroyCppClass failed');
  inherited;
end;

procedure TCppClass.SetValue(value: integer);
begin
  if CppClass_setValue(FIndex, value) <> 0 then
    raise Exception.Create('CppClass_setValue failed');
end;

function TCppClass.GetSquare: integer;
begin
  if CppClass_getSquare(FIndex, Result) <> 0 then
    raise Exception.Create('CppClass_getSquare failed');
end;

end.
