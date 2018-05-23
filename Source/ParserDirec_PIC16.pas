{Unidad que implementa a la clase TParserDirec, que sirve como contenedor para
implementar las funcionaliddes de procesamiento de directivas.
}
unit ParserDirec_PIC16;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, Graphics, LCLProc, SynFacilHighlighter, XpresBas,
  XpresElementsPIC, ParserDirec, Pic16Devices, ParserAsm_PIC16, Globales, MisUtils;

type

  { TParserDirec }
  TParserDirec = class(TParserAsm)
  private  //Rutinas del evaluador de expresiones
  private
    procedure ProcPROCESSOR;
  protected //Variables internas del compilador
    procedure ProcDIRline(const AsmLin: string; out ctxChanged: boolean);
  end;

  procedure SetLanguage;

implementation
procedure SetLanguage;
begin
  ParserAsm_PIC16.SetLanguage;
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
procedure TParserDirec.ProcDIRline(const AsmLin: string; out ctxChanged: boolean);
{Procesa una directiva, que ha sido definida, para que solo ocupe una sola línea,
para simplificar el procesamiento, ya que si las macros ocupan más de una línea,
complican tremendamente la exploración del lexer y la ubicación de errores.
Sin embargo, las directivas de tipo $IFDEF ... o ELSE ...  se pueden procesar aquí,
leyendo varias líneas sucesivas del código fuente.}
var
  lin: String;
  dmac: TDirMacro;
  p: TSrcPos;
  dvar: TDirVar;
begin
  ctxChanged := false;
  IniExplorDirec(lin);
  if tokType <> lexDir.tnIdentif then begin
    GenErrorDir(ER_ERROR_DIREC);
    exit;
  end;
  //sigue identificador
  case UpperCase(lexDir.GetToken) of
  'PROCESSOR' : ProcPROCESSOR;
  'FREQUENCY' : ProcFREQUENCY;
  'CONFIG'    : ProcCONFIG;
  'INCLUDE'   : ProcINCLUDE(lin, ctxChanged);
  'OUTPUTHEX' : ProcOUTPUTHEX(lin);
  'DEFINE'    : ProcDEFINE(lin);
  'IFDEF'     : ProcIFDEF(lin, false);
  'IFNDEF'    : ProcIFDEF(lin, true);
  'IF'        : ProcIF(lin, false);
  'IFNOT'     : ProcIF(lin, true);
  'ELSE'      : ProcELSE;
  'ENDIF'     : ProcENDIF;
  'MODE'      : ProcMODE;
  'MSGBOX'    : ProcMSGBOX;
  'MSGERR'    : ProcMSGERR;
  'MSGWAR'    : ProcMSGWAR;
  'INFO'      : ProcINFO;
  'WARNING'   : ProcWARNING;
  'ERROR'     : ProcERROR;
  'SET'       : ProcSET;
  'CLEAR_STATE_RAM': ProcCLEAR_STATE_RAM;
  'SET_STATE_RAM'  : ProcSET_STATE_RAM;
  'SET_MAPPED_RAM' : ProcSET_MAPPED_RAM;
  'RESET_PINS'     : ProcRESET_PINS;
  'SET_PIN_NAME'   : ProcSET_PIN_NAME;
  'MAP_RAM_TO_PIN' : ProcMAP_RAM_TO_PIN;
  'SET_UNIMP_BITS' : ProcSET_UNIMP_BITS;
  'SET_UNIMP_BITS1': ProcSET_UNIMP_BITS1;
  else
    //Puede ser una macro
    dmac := FindMacro(lexDir.GetToken);
    if dmac <> nil  then begin
      p := cIn.ReadSrcPos;   //Guarda posición del token
      cIn.Next;  //pasa la directiva
      cIn.NewContextFromTxt(
        dmac.value, //Pasa a explorar contenido de la macro como cadena
        dmac.posDef.fil {Fija el archivo de definiición de la macro.}
      );
      cIn.curCon.autoClose := true;   //Para que se cierre, al finalizar
      cIn.curCon.FixErrPos := true;   //Para que se ignore la posición de los errores
      cIn.curCon.ErrPosition := p;    //Posición a usar para ubicar el error
      cIn.curCon.PreErrorMsg := 'Macro '+dmac.name+': ';
      ctxChanged := true;  //Marca bandera para indciar que se ha cambiado de contexto
    end else if DefinedVar(lexDir.GetToken, dvar) then begin
      //Es variable
      p := cIn.ReadSrcPos;   //Guarda posición del token
      cIn.Next;  //pasa la directiva
      cIn.NewContextFromTxt(
        dvar.valor.valStr, //Pasa a explorar valor de la variable como texto
        '' {Fija el archivo de definiición.}
      );
      cIn.curCon.autoClose := true;   //Para que se cierre, al finalizar
      cIn.curCon.FixErrPos := true;   //Para que se ignore la posición de los errores
      cIn.curCon.ErrPosition := p;    //Posición a usar para ubicar el error
      cIn.curCon.PreErrorMsg := 'Variable '+dvar.nomb+': ';
      ctxChanged := true;  //Marca bandera para indciar que se ha cambiado de contexto
    end else begin
      GenErrorDir(ER_UNKNO_DIREC, [lexDir.GetToken]);
      exit;
    end;
  end;
end;

end.
