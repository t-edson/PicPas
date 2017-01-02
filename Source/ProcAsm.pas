{Unidad para procesar código ensamblador del PIC
La implementación es aún básica. Soporta código ensamblador, pero falta implementar
validaciones y flexibilidad para aceptar etiquetas, direcciones de variables o
constantes.
                                 Por Tito Hinostroza.   23/08/2015
}
unit ProcAsm;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, strutils, MisUtils, SynFacilHighlighter,
  SynEditHighlighter, Pic16Utils, XpresParserPIC;
var
  asmRow: integer;     //número de fila explorada
  asmErr: string;      //texto del error
  asmErrLin: integer;  //línea de error de ASM

type
  TerrRoutine = procedure(msg: string) of object;
  TerrRoutine2 = procedure(msg: string; const Args: array of const);

procedure InitAsm(pic0: TPIC16; cpx0: TCompilerBase);
procedure ProcASMlime(const AsmLin: string);  //Procesa una línea de código ASM

implementation
var
  InAsm : boolean;
  lexAsm: TSynFacilSyn;  //lexer para analizar ASM
  pic   : TPIC16;        //referencia al PIC
  cpx   : TCompilerBase;

function tokType: TSynHighlighterAttributes; inline;
begin
  Result := TSynHighlighterAttributes(PtrUInt(lexAsm.GetTokenKind));
end;
procedure skipWhites;
//salta blancos o comentarios
begin
  if tokType = lexAsm.tkSpace then
    lexAsm.Next;  //quita espacios
  //puede que siga comentario
  if tokType = lexAsm.tkComment then
    lexAsm.Next;
  //después de un comentario no se espera nada.
end;
function CaptureComma: boolean;
{Captura una coma. Si no encuentra devuelve error}
begin
  skipWhites;
  if lexAsm.GetToken = ',' then begin
    lexAsm.Next;   //toma la coma
    Result := true;
    exit;
  end else begin
    Result := false;
    cpx.GenError('Expected ",".');
    exit;
  end;
end;
function CaptureRegister(var f: byte): boolean;
{Captura la referencia a un registro y devuelve en "f". Si no encuentra devuelve error}
begin
  skipWhites;
  if tokType = lexAsm.tkNumber then begin
    //es una dirección numérica
    f := StrToInt(lexAsm.GetToken);
    lexAsm.Next;
    Result := true;
    exit;
  end else begin
    cpx.GenError('Expected address or variable name.');
    //asmErrLin := asmRow;
    Result := false;
    exit;
  end;
end;
function CaptureAddress(var a: word): boolean;
{Captura una dirección a una instrucción y devuelve en "a". Si no encuentra devuelve error}
begin
  skipWhites;
  if tokType = lexAsm.tkNumber then begin
    //es una dirección numérica
    a := StrToInt(lexAsm.GetToken);
    lexAsm.Next;
    Result := true;
    exit;
  end else begin
    cpx.GenError('Expected address.');
    Result := false;
    exit;
  end;
end;
function CaptureByte(var k: byte): boolean;
{Captura un byte y devuelve en "k". Si no encuentra devuelve error}
var
  n: Integer;
begin
  skipWhites;
  if tokType = lexAsm.tkNumber then begin
    //es una dirección numérica
    n := StrToInt(lexAsm.GetToken);
    if (n>255) then begin
      cpx.GenError('Expected byte.');
      exit(false);
    end;
    k:=n;
    lexAsm.Next;
    exit(true);
  end else begin
    cpx.GenError('Expected byte.');
    exit(false);
  end;
end;
function CaptureDestinat(var d: TPIC16destin): boolean;
{Captura el destino de una instrucción y devuelve en "d". Si no encuentra devuelve error}
var
  dest: String;
begin
  skipWhites;
  dest := lexAsm.GetToken;
  if (LowerCase(dest)='f') or (dest='1') then begin
    d := toF;
    lexAsm.Next;
    exit(true);
  end else if (LowerCase(dest)='w') or (dest='0') then begin
    d := toW;
    lexAsm.Next;
    exit(true);
  end else begin
    cpx.GenError('Expected "w" or "f".');
    exit(false);
  end;
end;
function CaptureNbit(var b: byte): boolean;
{Captura el número de bit de una instrucción y devuelve en "b". Si no encuentra devuelve error}
var
  dest: String;
begin
  skipWhites;
  if tokType = lexAsm.tkNumber then begin
    //es una dirección numérica
    b := StrToInt(lexAsm.GetToken);
    if (b>7) then begin
      cpx.GenError('Expected number of bit: 0..7.');
      Result := false;
      exit;
    end;
    lexAsm.Next;
    Result := true;
    exit;
  end else begin
    cpx.GenError('Expected number of bit: 0..7.');
    Result := false;
    exit;
  end;
end;

procedure StartASM; //Inicia el procesamiento de código ASM
begin
  InAsm := true;  //para indicar que estamos en medio de código ASM
  asmRow := 1;    //inicia línea
end;
procedure EndASM;  //Termina el procesamiento de código ASM
begin
  InAsm := false;
end;
procedure ProcInstrASM;
//Procesa una instrucción ASM
var
  stx: string;
  idInst: TPIC16Inst;
  tok: String;
  f : byte;
  d: TPIC16destin;
  b: byte;
  a: word;
  k: byte;
begin
  tok := lexAsm.GetToken;
  //debería ser una instrucción
  idInst := pic.FindOpcode(tok, stx);
  if idInst = _Inval then begin
    cpx.GenError('Invalid ASM Opcode: %s', [tok]);
    exit;
  end;
  //es un código válido
  lexAsm.Next;
  case stx of
  'fd': begin   //se espera 2 parámetros
    if not CaptureRegister(f) then exit;
    if not CaptureComma then exit;
    if not CaptureDestinat(d) then exit;
    pic.codAsmFD(idInst, f, d);
  end;
  'f':begin
    if not CaptureRegister(f) then exit;
    pic.codAsmF(idInst, f);
  end;
  'fb':begin
    if not CaptureRegister(f) then exit;
    if not CaptureComma then exit;
    if not CaptureNbit(b) then exit;
    pic.codAsmFB(idInst, f, b);
  end;
  'a': begin
    if not CaptureAddress(a) then exit;
    pic.codAsmA(idInst, a);
  end;
  'k': begin
     if not CaptureByte(k) then exit;
     pic.codAsmK(idInst, k);
  end;
  '': begin
    pic.codAsm(idInst);
  end;
  end;
  //no debe quedar más que espacios o comentarios
  skipWhites;
  if tokType <> lexAsm.tkEol then begin
    cpx.GenError('Syntax error: "%s"', [lexAsm.GetToken]);
    exit;
  end;

end;
procedure ProcASM(const AsmLin: string);
{Procesa una línea en ensamblador}
var
  lbl: String;
begin
  inc(asmRow);   //cuenta destíneas
  if Trim(AsmLin) = '' then exit;
  //procesa la destínea
  lexAsm.SetLine(asmLin, asmRow);  //inicia cadena
  if tokType = lexAsm.tkKeyword then begin
    ProcInstrASM;
    if cpx.HayError then exit;
  end else if tokType = lexAsm.tkIdentif then begin
    //puede ser una etiqueta
    lbl := lexAsm.GetToken;   //guarda posible etiqueta
    lexAsm.Next;
    if lexAsm.GetToken = ':' then begin
      //definitivamente es una etiqueta
      lexAsm.Next;
      skipwhites;
      if tokType <> lexAsm.tkEol then begin
        //Hay algo más. Solo puede ser una instrucción
        ProcInstrASM;
        if cpx.HayError then exit;
      end;
    end else begin
      //puede ser una etiqueta
      skipwhites;
      if tokType <> lexAsm.tkEol then begin
        //Hay algo más. Solo puede ser una instrucción
        ProcInstrASM;
        if cpx.HayError then exit;
      end;
    end;
  end else if tokType = lexAsm.tkComment then begin
    skipWhites;
    if tokType <> lexAsm.tkEol then begin
      cpx.GenError('Syntax error: "%s"', [lexAsm.GetToken]);
      exit;
    end;
  end else if tokType = lexAsm.tkSpace then begin
    skipWhites;
    if tokType <> lexAsm.tkEol then begin
      //Hay algo más. Solo puede ser una instrucción
      ProcInstrASM;
      if cpx.HayError then exit;
    end;
  end else begin
    cpx.GenError('Syntax error: "%s"', [lexAsm.GetToken]);
    exit;
  end;
  skipWhites;  //quita espacios
//  msgbox(AsmLin);
end;
function IsStartASM(var lin: string): boolean;
{Indica si una línea contiene al delimitador inicial "ASM". Si es así, la recorta.}
begin
  if not AnsiStartsText('asm', lin) then
    exit(false);  //definitivamente no es
  //hay coincidencia pero hay que analziar más
  if length(lin) = 3 then begin
    lin := copy(lin, 4, length(lin));  //quita "asm"
    exit(true);  //es exacto
  end;
  //podrìa ser, pero hay que descartar que no sea parte de un identificador
  if lin[4] in ['a'..'z', 'A'..'Z', '0'..'9', '_'] THEN
    exit(false); //es parte de un identificador
  //es por descarte
  lin := copy(lin, 4, length(lin));  //quita "asm"
  exit(true);
end;
function IsEndASM(var  lin: string): boolean;
{Indica si una línea contiene al delimitador final "END". Si es así, la recorta.}
begin
  if not AnsiEndsText('end', lin) then
    exit(false);  //definitivamente no es
  //hay coincidencia pero hay que analziar más
  if length(lin) = 3 then begin
    lin := copy(Lin, 1, length(Lin)-3);  //quita "end"
    exit(true);  //es exacto
  end;
  //podrìa ser, pero hay que descartar que no sea parte de un identificador
  if lin[length(lin)-3] in ['a'..'z', 'A'..'Z', '0'..'9', '_'] THEN
    exit(false); //es parte de un identificador
  //es por descarte
  lin := copy(Lin, 1, length(Lin)-3);  //quita "end"
  exit(true);
end;

procedure InitAsm(pic0: TPIC16; cpx0: TCompilerBase);
{Inicia el procesamiento del código en ensamblador}
begin
  pic := pic0;     //para poder codificarf instrucciones
  cpx := cpx0;     //para acceder a rutinas de error y variables, constantes, ...
end;
procedure ProcASMlime(const AsmLin: string); //Procesa una línea de código ASM
var
  lin: String;
begin
  lin := AsmLin;  //crea copia para poder modificarla
  //Extrae el texto entre los delimitadores de ensamblador
  if IsStartASM(lin) then begin
    //es la primera línea de ensamblador
    StartASM;
    //puede incluir también al delimitador "end"
    if IsEndASM(lin) then begin
      ProcASM(lin);  //procesa por si queda código
      EndASM;
    end else begin
      ProcASM(lin);  //procesa por si queda código
    end;
  end else if IsEndASM(lin) then begin
    //es la última línea de ensamblador
    ProcASM(lin);  //procesa por si queda código
    EndASM;
  end else begin
    //línea común de asm
    ProcASM(lin);
  end;
end;

procedure SetLanguage(lang: string);
begin
  case lang of
  'en': begin
    dicClear;  //it's yet in English
  end;
  'es': begin
    //Update messages
    dicSet('Expected ",".', 'Se esperaba ","');
    dicSet('Expected address or variable name.','Se esperaba dirección o variable.');
    dicSet('Invalid ASM Opcode: %s', 'Instrucción inválida: %s');
    dicSet('Expected "w" or "f".','Se esperaba "w" or "f".');
    dicSet('Syntax error: "%s"', 'Error de sintaxis: "%s"');
    dicSet('Expected number of bit: 0..7.', 'Se esperaba número de bit: 0..7');
    dicSet('Expected address.', 'Se esperaba dirección');
    dicSet('Expected byte.', 'Se esperaba byte');
  end;
  end;
end;

initialization
  InAsm := false;
  {Define la sintaxis del lexer que se usará para analizar el código en ensamblador.}
  lexAsm := TSynFacilSyn.Create(nil);  //crea lexer para analzar ensamblador
  lexAsm.DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');
  lexAsm.DefTokContent('[0-9]', '[0-9.]*', lexAsm.tkNumber);
  lexAsm.DefTokContent('[$]','[0-9A-Fa-f]*', lexAsm.tkNumber);
  lexAsm.DefTokContent('[%]','[01]*', lexAsm.tkNumber);
  lexAsm.AddIdentSpecList('ADDWF ANDWF CLRF CLRW COMF DECF DECFSZ INCF', lexAsm.tkKeyword);
  lexAsm.AddIdentSpecList('INCFSZ IORWF MOVF MOVWF NOP RLF RRF SUBWF SWAPF XORWF', lexAsm.tkKeyword);
  lexAsm.AddIdentSpecList('BCF BSF BTFSC BTFSS', lexAsm.tkKeyword);
  lexAsm.AddIdentSpecList('ADDLW ANDLW CALL CLRWDT GOTO IORLW MOVLW RETFIE', lexAsm.tkKeyword);
  lexAsm.AddIdentSpecList('RETLW RETURN SLEEP SUBLW XORLW', lexAsm.tkKeyword);
  lexAsm.DefTokDelim(';','', lexAsm.tkComment);
  lexAsm.Rebuild;
finalization
  lexAsm.Destroy;
end.

