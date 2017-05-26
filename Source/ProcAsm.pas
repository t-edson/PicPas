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
  Classes, SysUtils, fgl, strutils, MisUtils, SynFacilHighlighter,
  SynEditHighlighter, LCLProc, Pic16Utils, XpresBas, XpresParserPIC, Globales,
  XpresElementsPIC;
var
  asmRow: integer;     //número de fila explorada
  asmErr: string;      //texto del error
  asmErrLin: integer;  //línea de error de ASM

type
  TerrRoutine = procedure(msg: string) of object;
  TerrRoutine2 = procedure(msg: string; const Args: array of const);

procedure SetLanguage(idLang: string);
procedure InitAsm(pic0: TPIC16; cpx0: TCompilerBase);
procedure ProcASMlime(const AsmLin: string);  //Procesa una línea de código ASM

implementation
type
  //Datos de una etiqueta
  TPicLabel = class
    txt: string;   //nombre de la etiqueta
    add: integer;  //dirección
  end;
  TPicLabel_list = specialize TFPGObjectList<TPicLabel>;

var
  InAsm : boolean;
  lexAsm: TSynFacilSyn;   //lexer para analizar ASM
  pic   : TPIC16;         //referencia al PIC
  labels: TPicLabel_list; //Lista de etiquetas
  cpx   : TCompilerBase;
var  //Mensajes
  ER_EXPEC_COMMA, ER_EXP_ADR_VAR, ER_NOTYPVAR_AL, ER_INV_ASMCODE: String;
  ER_EXPECT_W_F, ER_SYNTAX_ERR_, ER_EXPE_NUMBIT, ER_EXPECT_ADDR: String;
  ER_EXPECT_BYTE, WA_ADDR_TRUNC: String;

procedure SetLanguage(idLang: string);
begin
  curLang := idLang;
  //Update messages
  ER_EXPEC_COMMA := trans('Expected ",".', 'Se esperaba ","','','');
  ER_EXP_ADR_VAR := trans('Expected address or variable name.','Se esperaba dirección o variable.','','');
  ER_NOTYPVAR_AL := trans('Cannot get address of this Variable', 'No se puede obtener la dirección de esta variable.', '','');
  ER_INV_ASMCODE := trans('Invalid ASM Opcode: %s', 'Instrucción inválida: %s','','');
  ER_EXPECT_W_F  := trans('Expected "w" or "f".','Se esperaba "w" or "f".','','');
  ER_SYNTAX_ERR_ := trans('Syntax error: "%s"', 'Error de sintaxis: "%s"','','');
  ER_EXPE_NUMBIT := trans('Expected number of bit: 0..7.', 'Se esperaba número de bit: 0..7','','');
  ER_EXPECT_ADDR := trans('Expected address.', 'Se esperaba dirección','','');
  ER_EXPECT_BYTE := trans('Expected byte.', 'Se esperaba byte','','');
  WA_ADDR_TRUNC  := trans('Address truncated to fit instruction.', 'Dirección truncada, al tamaño de la instrucción', '','');
end;
procedure GenError(msg: string);
{Genera un error corrigiendo la posición horizontal}
var
  p: TSrcPos;
begin
  p := cpx.cIn.ReadSrcPos;
  p.col := lexAsm.GetX;  //corrige columna
  cpx.GenErrorPos(msg, [], p);
end;
procedure GenError(msg: string; const Args: array of const);
var
  p: TSrcPos;
begin
  p := cpx.cIn.ReadSrcPos;
  p.col := lexAsm.GetX;  //corrige columna
  cpx.GenErrorPos(msg, Args, p);
end;
function tokType: integer; inline;
begin
  Result := lexAsm.GetTokenKind;
end;
procedure skipWhites;
//salta blancos o comentarios
begin
  if tokType = lexAsm.tnSpace then
    lexAsm.Next;  //quita espacios
  //puede que siga comentario
  if tokType = lexAsm.tnComment then
    lexAsm.Next;
  //después de un comentario no se espera nada.
end;
function GetFaddress(addr: integer): byte;
{Obtiene una dirección de registro para una isntrucción ASM, truncando, si es necesario,
los bits adicionales.}
begin
  if addr>255 then begin
    addr := addr and $7F;
    //Indica con advertencia
    cpx.GenWarn(WA_ADDR_TRUNC);
  end;
  Result := addr;
end;
procedure AddLabel(name: string; addr: integer);
{Agrega una etiqueta a la lista}
var
  lbl: TPicLabel;
begin
  lbl := TPicLabel.Create;
  lbl.txt:= UpCase(name);
  lbl.add := addr;
  labels.Add(lbl);
end;
function IsLabel(txt: string; out dir: integer): boolean;
{Indica si un nombre es una etiqueta. Si lo es, devuelve TRUE, y la dirección la retorna
en "dir".}
var
  lbl: TPicLabel;
begin
  //No se espera procesar muchsa etiquetas
  for lbl in labels do begin
    if lbl.txt = upcase(txt) then begin
      dir := lbl.add;
      exit(true);
    end;
  end;
  //No encontró
  exit(false);
end;
function CaptureByte(var k: byte): boolean;
{Captura un byte y devuelve en "k". Si no encuentra devuelve error}
var
  n: Integer;
begin
  skipWhites;
  if tokType = lexAsm.tnNumber then begin
    //es una dirección numérica
    n := StrToInt(lexAsm.GetToken);
    if (n>255) then begin
      GenError(ER_EXPECT_BYTE);
      exit(false);
    end;
    k:=n;
    lexAsm.Next;
    exit(true);
  end else begin
    GenError(ER_EXPECT_BYTE);
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
    GenError(ER_EXPECT_W_F);
    exit(false);
  end;
end;
function CaptureNbit(var b: byte): boolean;
{Captura el número de bit de una instrucción y devuelve en "b". Si no encuentra devuelve error}
begin
  skipWhites;
  if tokType = lexAsm.tnNumber then begin
    //es una dirección numérica
    b := StrToInt(lexAsm.GetToken);
    if (b>7) then begin
      GenError(ER_EXPE_NUMBIT);
      Result := false;
      exit;
    end;
    lexAsm.Next;
    Result := true;
    exit;
  end else begin
    GenError(ER_EXPE_NUMBIT);
    Result := false;
    exit;
  end;
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
    GenError(ER_EXPEC_COMMA);
    exit;
  end;
end;
function CaptureBitVar(var f, b: byte): boolean;
{Captura una variable de tipo Bit. Si no encuentra, devuelve FALSE (no genera error).}
var
  ele: TxpElement;
  xvar: TxpEleVar;
begin
  skipWhites;
  if tokType <> lexAsm.tnIdentif then exit(false);  //no es identificador
  //Hay un identificador
  ele := cpx.TreeElems.FindFirst(lexAsm.GetToken);  //identifica elemento
  if ele = nil then exit(false);  //nos e identifica
  //Se identificó elemento
  if not (ele is TxpEleVar) then exit(false);
  //Es variable
  if (ele.typ <> typBit) and (ele.typ <> typBool) then exit(false);
  //Es variable bit o boolean
  lexAsm.Next;   //toma identificador
  xvar := TxpEleVar(ele);
  if cpx.FirstPass then Inc(xvar.nCalled);  //lleva la cuenta
  f := GetFaddress(xvar.adrBit.offs);
  b := xvar.adrBit.bit;
  exit(true);
end;
function CaptureRegister(var f: byte): boolean;
{Captura la referencia a un registro y devuelve en "f". Si no encuentra devuelve error}
var
  n: integer;
  ele: TxpElement;
  xvar: TxpEleVar;
begin
  skipWhites;
  if tokType = lexAsm.tnNumber then begin
    //es una dirección numérica
    n := StrToInt(lexAsm.GetToken);  //Puede reconcoer hexadecimales con $, binario con %
    f := GetFaddress(n);
    lexAsm.Next;
    Result := true;
    exit;
  end else if tokType = lexAsm.tnIdentif then begin
    //Es un identificador, puede ser referencia a una variable
    ele := cpx.TreeElems.FindFirst(lexAsm.GetToken);  //identifica elemento
    if ele = nil then begin
      //No identifica a este elemento
      GenError(ER_EXP_ADR_VAR);
      exit;
    end;
    if ele is TxpEleVar then begin
      xvar := TxpEleVar(ele);
      if cpx.FirstPass then Inc(xvar.nCalled);  //lleva la cuenta
      if (xvar.typ = typByte) or (xvar.typ = typChar) then begin
        n := xvar.AbsAddr;
        f := GetFaddress(n);
        lexAsm.Next;
        Result := true;
        exit;
      end else begin
        GenError(ER_NOTYPVAR_AL);
        Result := false;
        exit;
      end;
    end else begin
      //No es variable
      GenError(ER_EXP_ADR_VAR);
      Result := false;
      exit;
    end;
  end else begin
    GenError(ER_EXP_ADR_VAR);
    //asmErrLin := asmRow;
    Result := false;
    exit;
  end;
end;
function CaptureAddress(var a: word): boolean;
{Captura una dirección a una instrucción y devuelve en "a". Si no encuentra geenra
error y devuelve FALSE.}
var
  dir: integer;
  offset: byte;
begin
  skipWhites;
  if lexAsm.GetToken = '$' then begin
    //Es una dirección relativa
    lexAsm.Next;
    skipWhites;
    //Puede tener + o -
    if (lexAsm.GetToken= '') or (lexAsm.GetToken = ';') then begin
      //Termina la instrucción sin o con es comentario
      a := pic.iFlash;
      Result := true;
      exit;
    end else if lexAsm.GetToken = '+' then begin
      //Es dirección sumada
      lexAsm.Next;
      skipWhites;
      CaptureByte(offset);  //captura desplazamiento
      if cpx.HayError then exit(false);
      Result := true;
      a := pic.iFlash + offset;
      exit;
    end else if lexAsm.GetToken = '-' then begin
      //Es dirección restada
      lexAsm.Next;
      skipWhites;
      CaptureByte(offset);  //captura desplazamiento
      if cpx.HayError then exit(false);
      Result := true;
      a := pic.iFlash - offset;
      exit;
    end else begin
      //Sigue otra cosa
      GenError(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
    end;
  end else if tokType = lexAsm.tnNumber then begin
    //es una dirección numérica
    a := StrToInt(lexAsm.GetToken);
    lexAsm.Next;
    Result := true;
    exit;
  end else if (tokType = lexAsm.tnIdentif) and IsLabel(lexAsm.GetToken, dir) then begin
    //Es un identificador
    a := dir;
    lexAsm.Next;
    Result := true;
    exit;
  end else begin
    GenError(ER_EXPECT_ADDR);
    Result := false;
    exit;
  end;
end;

procedure StartASM; //Inicia el procesamiento de código ASM
begin
  InAsm := true;  //para indicar que estamos en medio de código ASM
  asmRow := 1;    //inicia línea
  labels.Clear;   //limpia etiquetas
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
    GenError(ER_INV_ASMCODE, [tok]);
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
  'fb':begin  //para instrucciones de tipo bit
    if CaptureBitVar(f, b) then begin
      //Es una referencia a variable bit.
    end else begin
      if not CaptureRegister(f) then exit;
      if not CaptureComma then exit;
      if not CaptureNbit(b) then exit;
    end;
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
  if tokType <> lexAsm.tnEol then begin
    GenError(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
    exit;
  end;

end;
procedure ProcASM(const AsmLin: string);
{Procesa una línea en ensamblador.}
var
  lbl: String;
begin
  inc(asmRow);   //cuenta destíneas
  if Trim(AsmLin) = '' then exit;
  //procesa la destínea
  lexAsm.SetLine(asmLin, asmRow);  //inicia cadena
  if tokType = lexAsm.tnKeyword then begin
    ProcInstrASM;
    if cpx.HayError then exit;
  end else if tokType = lexAsm.tnIdentif then begin
    //Puede ser una etiqueta
    lbl := lexAsm.GetToken;   //guarda posible etiqueta
    lexAsm.Next;
    if lexAsm.GetToken = ':' then begin
      //definitivamente es una etiqueta
      AddLabel(lbl, pic.iFlash);
      lexAsm.Next;
      skipwhites;
      if tokType <> lexAsm.tnEol then begin
        //Hay algo más. Solo puede ser una instrucción
        ProcInstrASM;
        if cpx.HayError then exit;
      end;
    end else begin
      //puede ser una etiqueta
      AddLabel(lbl, pic.iFlash);
      skipwhites;
      if tokType <> lexAsm.tnEol then begin
        //Hay algo más. Solo puede ser una instrucción
        ProcInstrASM;
        if cpx.HayError then exit;
      end;
    end;
  end else if tokType = lexAsm.tnComment then begin
    skipWhites;
    if tokType <> lexAsm.tnEol then begin
      GenError(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
      exit;
    end;
  end else if tokType = lexAsm.tnSpace then begin
    skipWhites;
    if tokType <> lexAsm.tnEol then begin
      //Hay algo más. Solo puede ser una instrucción
      ProcInstrASM;
      if cpx.HayError then exit;
    end;
  end else begin
    GenError(ER_SYNTAX_ERR_, [lexAsm.GetToken]);
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

initialization
  InAsm := false;
  labels := TPicLabel_list.Create(true);
  {Define la sintaxis del lexer que se usará para analizar el código en ensamblador.}
  lexAsm := TSynFacilSyn.Create(nil);  //crea lexer para analzar ensamblador
  lexAsm.DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');
  lexAsm.DefTokContent('[0-9]', '[0-9.]*', lexAsm.tnNumber);
  lexAsm.DefTokContent('[$]','[0-9A-Fa-f]*', lexAsm.tnNumber);
  lexAsm.DefTokContent('[%]','[01]*', lexAsm.tnNumber);
  lexAsm.AddIdentSpecList('ADDWF ANDWF CLRF CLRW COMF DECF DECFSZ INCF', lexAsm.tnKeyword);
  lexAsm.AddIdentSpecList('INCFSZ IORWF MOVF MOVWF NOP RLF RRF SUBWF SWAPF XORWF', lexAsm.tnKeyword);
  lexAsm.AddIdentSpecList('BCF BSF BTFSC BTFSS', lexAsm.tnKeyword);
  lexAsm.AddIdentSpecList('ADDLW ANDLW CALL CLRWDT GOTO IORLW MOVLW RETFIE', lexAsm.tnKeyword);
  lexAsm.AddIdentSpecList('RETLW RETURN SLEEP SUBLW XORLW', lexAsm.tnKeyword);
  lexAsm.DefTokDelim(';','', lexAsm.tnComment);
  lexAsm.Rebuild;
finalization
  lexAsm.Destroy;
  labels.Destroy;
end.

