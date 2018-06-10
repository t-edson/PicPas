{Unidad que implementa a la clase TParserDirec, que sirve como contenedor para
implementar las funcionaliddes adicionales del procesamiento de directivas.
}
unit ParserDirec_PIC17;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, ParserDirec, Pic17Devices, ParserAsm_PIC17;

type

  { TParserDirec }
  TParserDirec = class(TParserAsm)
  private
    procedure ProcPROCESSOR;
  public
    procedure ClearMacros;
  end;

  procedure SetLanguage;

implementation
procedure SetLanguage;
begin
  ParserAsm_PIC17.SetLanguage;
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
procedure TParserDirec.ClearMacros;
begin
  inherited;
  //Agrega nuevas instrucciones
  AddInstruction('PROCESSOR', @ProcPROCESSOR);
end;

end.
