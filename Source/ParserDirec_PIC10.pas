{Unidad que implementa a la clase TParserDirec, que sirve como contenedor para
implementar las funcionaliddes adicionales del procesamiento de directivas.
}
unit ParserDirec_PIC10;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, ParserDirec, Pic10Devices, ParserAsm_PIC10;
type

  { TParserDirec }
  TParserDirec = class(TParserAsm)
  private  //Rutinas del evaluador de expresiones
    function read_PIC_ENHANCED: Single;
    procedure write_PIC_ENHANCED(AValue: Single);
  private
    procedure ProcPROCESSOR;
  public
    procedure ClearMacros;
  end;

  procedure SetLanguage;

implementation

procedure SetLanguage;
begin
  ParserAsm_PIC10.SetLanguage;
end;

{ TParserDirec }
procedure TParserDirec.ProcPROCESSOR;
begin
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  if not GetHardwareInfo(pic, lexDir.GetToken) then begin
    GenErrorDir(ER_UNKNO_DEVIC, [lexDir.GetToken]);
    exit;
  end;
end;
//Otras variables
function TParserDirec.read_PIC_ENHANCED: Single;
begin
  if pic.Enhanced then Result := 1 else Result := 0;
end;
procedure TParserDirec.write_PIC_ENHANCED(AValue: Single);
begin
  pic.Enhanced := (round(AValue) <> 0);
end;
procedure TParserDirec.ClearMacros;
begin
  inherited;
  //Agrega nuevas instrucciones
  AddInstruction('PROCESSOR', @ProcPROCESSOR);
  //Agrega nuevas variables del sistema
  AddSysVariableNumber('PIC_ENHANCED', @read_PIC_ENHANCED, @write_PIC_ENHANCED);
end;

end.
