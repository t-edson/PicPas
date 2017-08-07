{Unidad que implementa a la clase TParserDirec, que sirve como contenedor para
implementar las funcionaliddes de procesamiento de directivas.
}
unit ParserDirec;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, ParserAsm, fgl, SynFacilHighlighter, Globales,
  XpresElementsPIC, Pic16Devices, MisUtils, XpresBas, LCLProc;

type
  //Identifica a una macro
  TPicMacro = class
    name  : string;
    value : string;
    posDef: TSrcPos;   //posición del contexto
  end;

  TPicMacro_list = specialize TFPGObjectList<TPicMacro>;

  { TParserDirec }
  TParserDirec = class(TParserAsm)
  private
    lexDir : TSynFacilSyn;  //lexer para analizar directivas
    tokIni : integer;  //Posición inicial del token actual
    dirOperator: Integer;
    dirDelimiter: integer;
    WaitForEndIF: integer;
    procedure GenErrorDir(msg: string; const Args: array of const);
    procedure GenErrorDir(msg: string);
    procedure IniExplorDirec(out lin: string);
    function modeStr: string;
    procedure ProcCONFIG;
    procedure ProcFREQUENCY;
    procedure ProcDEFINE(lin: string);
    procedure ProcIFDEF(lin: string; negated: boolean);
    procedure ProcINCLUDE(lin: string; var ctxChanged: boolean);
    procedure ProcMODE;
    procedure ProcMSGBOX;
    procedure ProcPROCESSOR;
    function ScanIFDEF(out tok: string): boolean;
    procedure skipWhites;
    function GetIdent: string;
    function tokType: integer;
    procedure DefLexDirectiv;
  protected //Variables internas del compilador
    mainFile  : string;  //archivo inicial que se compila
    mode      : (modPascal, modPicPas);
    macroList : TPicMacro_list;
    ConfigWord: integer;  //Bits de configuración
    procedure NewMacro(macName, macValue: string);
    procedure ProcDIRline(const AsmLin: string; out ctxChanged: boolean);
    function DefinedMacro(macName: string): boolean;
    function FindMacro(macName: string): TPicMacro;
  public
    Compiling : boolean;  //Bandera para el compilado
    procedure ClearMacros;
    constructor Create; override;
    destructor Destroy; override;
  end;

  procedure SetLanguage(idLang: string);

implementation
  {$I ..\language\tra_ParserDirec.pas}

{ TParserDirec }
procedure TParserDirec.GenErrorDir(msg: string);
{Genera un error corrigiendo la posición horizontal}
var
  p: TSrcPos;
begin
  p := cIn.ReadSrcPos;
  p.col := tokIni + lexDir.GetX;  //corrige columna
  GenErrorPos(msg, [], p);
end;
procedure TParserDirec.GenErrorDir(msg: string; const Args: array of const);
var
  p: TSrcPos;
begin
  p := cIn.ReadSrcPos;
  p.col := tokIni + lexDir.GetX;  //corrige columna
  GenErrorPos(msg, Args, p);
end;
function TParserDirec.modeStr: string;
begin
  case mode of
  modPascal: Result := 'modPascal';
  modPicPas: Result := 'modPicPas';
  else
    Result := 'Unknown';
  end;
end;
function TParserDirec.tokType: integer;
begin
  Result := lexdir.GetTokenKind;
end;
procedure TParserDirec.skipWhites;
begin
  if tokType = lexDir.tnSpace then
    lexDir.Next;  //quita espacios
end;
function TParserDirec.GetIdent: string;
begin
  if tokType = lexDir.tnSpace then
    lexDir.Next;  //quita espacios
  //verifica
  if lexDir.GetTokenKind <> lexDir.tnIdentif then begin
    GenErrorDir(ER_IDENT_EXPEC);
    exit;
  end;
  Result := lexDir.GetToken;
  lexDir.Next;  //toma identificador
end;
procedure TParserDirec.NewMacro(macName, macValue: string);
{Agrega una nueva macro a la lista de macros}
var
  mac: TPicMacro;
begin
  mac := TPicMacro.Create;
  mac.name := macName;
  mac.value := macValue;
  //Ubica la posición del contexto
  mac.posDef := cIn.ReadSrcPos;
  macroList.Add(mac);
end;
function TParserDirec.DefinedMacro(macName: string): boolean;
{Indica si una macro ha sido definida}
var
  mac: TPicMacro;
begin
  macName := UpCase(macName);
  for mac in macroList do begin
    if UpCase(mac.name) = macName then begin
      exit(true);  //encontró
    end;
  end;
//No se encontró
  exit(false);
end;
function TParserDirec.FindMacro(macName: string): TPicMacro;
{Busca la definición de una macro, si la encuentra, devuelve la referecnia, de otra
forma dewvuelve NIL.}
var
  mac: TPicMacro;
begin
  macName := UpCase(macName);
  for mac in macroList do begin
    if UpCase(mac.name) = macName then begin
      exit(mac);  //encontró
    end;
  end;
//No se encontró
  exit(nil);
end;
function TParserDirec.ScanIFDEF(out tok: string): boolean;
{Explora el texto, hasta encontrar la directiva $ENDIF o $ELSE.  Si llega al
 final del contexto, sin encontrar alguna de estas directivas, devuelve FALSE.}
var
  tmp, direc: string;
begin
  while not cIn.Eof do begin
//    debugln(cIn.tok);
    if cIn.tokType = tnDirective then begin
      //Podría ser el delimitador buscado
      IniExplorDirec(tmp);
      direc := UpperCase(lexDir.GetToken);
      if (direc = 'ENDIF') or (direc='ELSE') then begin
        //Encontró el delimitador
        tok := direc;
        cIn.Next;  //toma el token
        exit(true);  //y continúa
      end;
    end;
    cIn.Next;
  end;
  //No encontró
  exit(false);
end;
procedure TParserDirec.ProcPROCESSOR;
begin
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  if not GetHardwareInfo(pic, lexDir.GetToken) then begin
    GenErrorDir(ER_UNKNO_DEVIC, [lexDir.GetToken]);
    exit;
  end;
end;
procedure TParserDirec.ProcFREQUENCY;
var
  f: Longint;
begin
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  if tokType <> lexDir.tnNumber then begin
    GenErrorDir(ER_ERROR_DIREC);
    exit;
  end;
  if not TryStrToInt(lexDir.GetToken, f) then begin
    GenErrorDir(ER_ERROR_FREQ);
    exit;
  end;
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  case UpperCase(lexDir.GetToken) of
  'KHZ': f := f * 1000;
  'MHZ': f := f * 1000000;
  else
    GenErrorDir(ER_ERROR_DIREC);
    exit;
  end;
  pic.frequen:=f; //asigna freecuencia
end;
procedure TParserDirec.ProcCONFIG;
var
  Ident, tmp: String;
  mac: TPicMacro;
  valBit: Longint;
begin
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  if lexDir.GetTokenKind = lexDir.tnNumber then begin
    //Es un valor numérico
    if not TryStrToInt(lexDir.GetToken, valBit) then begin
      GenErrorDir(ER_INVAL_CBIT_, [Ident]);
      exit;
    end;
    //Ya se tiene el valor numérico
    ConfigWord := valBit;  //carga directamente
    lexDir.Next;
    skipWhites;
    //No debe seguir nada
    if not lexDir.GetEol then begin
      GenErrorDir(ER_INVAL_CBIT_, [Ident]);
      exit;
    end;
    exit;
  end;
  //Debe seguir identificadores
  while not lexDir.GetEol do begin
    Ident := GetIdent;
    if HayError then exit;
    mac := FindMacro(Ident);
    if mac <> nil then begin
      //Hay macro definida. Hay que extraer su valor.
      tmp := trim(mac.value);
      if copy(tmp,1,2) = '0x' then begin
        delete(tmp,1,2);
        tmp := '$' + tmp;
      end;
      if not TryStrToInt(tmp, valBit) then begin
        GenErrorDir(ER_INVAL_CBIT_, [Ident]);
        exit;
      end;
      //Ya se tiene el valor numérico
      if ConfigWord = -1 then begin
        //Es la primera vez que se inicia
        ConfigWord := $FFFF;
      end;
      ConfigWord := ConfigWord and valBit;  //Marca bandera

      //msgbox('|'+mac.value+'|');
    end else begin
      //No está definido
      GenErrorDir(ER_CONF_UNDEF_, [Ident]);
      exit;
    end;
    skipWhites;
    if lexDir.GetToken = ',' then begin
      lexDir.Next;  //pasa al siguiente
    end;
    skipWhites;
  end;
end;
procedure TParserDirec.ProcINCLUDE(lin: string; var ctxChanged: boolean);
{Implementa la inclusión de un archivo externo en el código}
var
  filPath: string;
begin
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  //Toma el restante de la cadena
  filPath := copy(lin, lexDir.GetX);
  if (pos('/', filPath)=0) and (pos('\', filPath)=0) then begin
    //No incluye información de ruta. Asume que está en la misma ruta.
    filPath := ExtractFileDir(mainFile) + DirectorySeparator + filPath;
  end;
  if not FileExists(filPath) then begin
    GenErrorDir(ER_FILE_NO_FND_, [filPath]);
    exit;
  end;
  //Ya se tiene el archivo
  cIn.Next;  //pasa la directiva
  cIn.NewContextFromFile(filPath);  //Pasa a explorar contenido del archivo
  cIn.curCon.autoClose := true;   //Para que se cierre, al finalizar
  //cIn.curCon.FixErrPos := true;   //Para que se ignore la posición de los errores
  //cIn.curCon.ErrPosition := p;    //Posición a usar para ubicar el error
  //cIn.curCon.PreErrorMsg := 'Macro '+mac.name+': ';
  ctxChanged := true;   //Marca bandera para indciar que se ha cambiado de contexto

end;
procedure TParserDirec.ProcDEFINE(lin: string);
var
  Ident, value: String;
begin
  lexDir.Next;  //pasa al siguiente
  Ident := GetIdent;
  if HayError then exit;
  skipWhites;
  if lexDir.GetEol then begin
    //Se definió un identificador sin valor
    NewMacro(Ident, '');
  end else begin
    //Sigue algo más
    if lexDir.GetToken <> '=' then begin
      GenErrorDir(ER_EXPEC_EQUAL);
      exit;
    end;
    lexDir.Next;  //toma símbolo
    skipWhites;
    if lexDir.GetEol then begin
      GenErrorDir(ER_SYNTAX_ERRO);
    end;
    //Toma definición
    value := copy(lin, lexDir.GetX, length(lin));
    NewMacro(Ident, value);
  end;
end;
procedure TParserDirec.ProcIFDEF(lin: string; negated: boolean);
  function EvaluateExp(const Ident: string): boolean;
  {Evalúa el resultado de la expresión de la directiva $IFDEF.
  Debería ejecutarse solo una vez, en ProcIFDEF(()}
  var
    xDirec: TxpEleDIREC;
    ele: TxpElement;
  begin
    if FirstPass then begin
      //Agrega el nodo para guardar información para la segunda pasada
      xDirec := TxpEleDIREC.Create;
      xDirec.srcDec := cIn.ReadSrcPos;   //guarda posición de aparición
      TreeDirec.AddElement(xDirec, false);  //Agrega sin verificación de nombre
      //Evalúa
      Result := DefinedMacro(Ident) xor negated;
      //Guarda resultado
      xDirec.ifDefResult := Result;
    end else begin
      {En al segunda pasada, ya no se evalúa, porque la segunda pasada, ya no se
      hace en el orden del código fuente, y se pierde la secuencia de directivas.}
      for ele in TreeDirec.curNode.elements do begin
        //Busca la directiva de la dirección actual (ubicada en la primera pasada)
        if ele.srcDec.EqualTo(cIn.ReadSrcPos) then begin
          //Encontró
          Result := TxpEleDIREC(ele).ifDefResult;
          exit;
        end;
      end;
      //No encontró. Esto no debería pasar.
      MsgErr('Implementation error.');
    end;
  end;
var
  Ident, direc: String;
begin
  lexDir.Next;  //pasa al siguiente
  Ident := GetIdent;
  if HayError then exit;
  skipWhites;
  if lexDir.GetEol then begin
    //Esto es lo normal. Buscamos el identificador
    if EvaluateExp(Ident) then begin
      //Está definido
      inc(WaitForEndIF);  //marca bandera para esperar
    end else begin
      //No está definido, no se debe compilar hasta un {$ENDIF} o un {$ELSE}
      cIn.Next;  //toma token {$IDEF  }
      //Explora, sin compilar, hasta encontrar directiva delimitadora.
      if not ScanIFDEF(direc) then begin
        //Llegó al final del código fuente, sin encontrar el ENDIF
        GenErrorDir(ER_ENDIF_NOFOU);
        exit;
      end;
      //Encontró token delimitador
      //Si es $ENDIF, no hay problema, todo termina allí, pero si es un else:
      if direc='ELSE' then begin
        inc(WaitForEndIF);  //marca bandera para esperar
      end;
    end;
  end else begin
    //Sigue algo más. No se esperaba.
    GenErrorDir(ER_SYNTAX_ERRO);
  end;
end;
procedure TParserDirec.ProcMODE;
var
  txtMode: String;
begin
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  txtMode := UpCase(lexDir.GetToken);
  if txtMode = 'PICPAS' then begin
    self.mode := modPicPas;
  end else if txtMode = 'PASCAL' then begin
    self.mode := modPascal;
  end else begin
    GenErrorDir(ER_MODE_UNKNOWN, [txtMode]);
    exit;
  end;
end;
procedure TParserDirec.ProcMSGBOX;
var
  txtMsg, tmp: String;
  mac: TPicMacro;
begin
  lexDir.Next;  //pasa al siguiente
  skipWhites;
  //Captura tokens para mostrarlos
  txtMsg := '';
  while not lexDir.GetEol do begin
    tmp :=  lexDir.GetToken;  //lee token
    if lexDir.GetTokenKind = lexDir.tnString then begin
      tmp := copy(tmp, 2, length(tmp)-2);
//      end else if lexDir.GetToken = '}' then begin
//        break;  //sale
    end else begin
      //Verifica si es variable de sistema
      if UpCase(tmp) = '$FREQUENCY' then tmp := IntToStr(pic.frequen);
      if UpCase(tmp) = '$PROCESSOR' then tmp := pic.Model;
      if UpCase(tmp) = '$MODE' then tmp := modeStr;
      if UpCase(tmp) = '$CURRBANK' then tmp := IntToStr(CurrBank);
      if UpCase(tmp) = '$IFLASH' then tmp := IntToStr(pic.iFlash);
      if UpCase(tmp) = '$WAITFORENDIF' then tmp := IntToStr(WaitForEndIF);
      //Verifica si es una macro definida
      if tmp[1]  = '$' then begin
        //Puede ser macro
        mac := FindMacro(copy(tmp,2 ));
        if mac<>nil then begin
          tmp := mac.value;
        end;
      end;
    end;
    //Concatena
    txtMsg := txtMsg + tmp;
    lexDir.Next;  //pasa al siguiente
    skipWhites;
  end;
  //Solo muestra en compilación y en la primera pasada
  if Compiling and FirstPass then msgbox(txtMsg);
end;
procedure TParserDirec.IniExplorDirec(out lin: string);
(*Inicia la exploración del token de directiva. Extrae el delimitador final "}", y
posiciona al lexer justo despues del delimitador inicial "{$". Devuelve la línea
procesada en "lin" (sin delimitador final). Además inicia tokIni
*)
var
  dlin: Integer;
  p: TSrcPos;
begin
  //Fija el inicio del token actual (Esto es válido, porque las directivas son "unilineas")
  tokIni := Cin.curCon.lex.GetX - 1;
  //Usa SynFacilSyn como lexer para analizar texto
  lin := Cin.tok;
  dlin := length(lin);
  if lin[dlin] = '}'  then begin
    delete(lin, dlin, 1);  //quita delimitador final de directiva
  end else begin
    //Es un error, pero es salvable.
    //Ubicamos el error, "manualmente", porque aún no hemos explorado con el lexer.
    p := cIn.ReadSrcPos;
    p.col := tokIni + dlin + 1;  //columna al final
    GenErrorPos(ER_EXPECTED_BR, [], p);
  end;
  //Inicia exploración con el lexer "lexDir"
  lexDir.SetLine(lin, 0);  //inicia cadena
  lexDir.Next;  //Salta el "{$"
  skipWhites;
end;
procedure TParserDirec.ProcDIRline(const AsmLin: string; out ctxChanged: boolean);
{Procesa una directiva, que ha sido definida, para que solo ocupe una sola línea,
para simplificar el procesamiento, ya que si las macros ocupan más de una línea,
complican tremendamente la exploración del lexer y la ubicación de errores.
Sin embargo, las directivas de tipo $IFDEF ... o ELSE ...  se pueden procesar aquí,
leyendo varias líneas sucesivas del código fuente.}
var
  lin, direc: String;
  mac: TPicMacro;
  p: TSrcPos;
begin
  ctxChanged := false;
  IniExplorDirec(lin);
  if tokType <> lexDir.tnIdentif then begin
    GenErrorDir(ER_ERROR_DIREC);
    exit;
  end;
  //sigue identificador
  case UpperCase(lexDir.GetToken) of
  'PROCESSOR': ProcPROCESSOR;
  'FREQUENCY': ProcFREQUENCY;
  'CONFIG'   : ProcCONFIG;
  'INCLUDE'  : ProcINCLUDE(lin, ctxChanged);
  'DEFINE'   : ProcDEFINE(lin);
  'IFDEF'    : ProcIFDEF(lin, false);
  'IFNDEF'   : ProcIFDEF(lin, true);
  'ELSE'     : begin
    if WaitForEndIF>0 then begin
      {Estamos dentro de un IF, que se supone dio verdadero, de otra forma, no llegaría
      por aquí. De ser así, el ELSE debe ser falso.}
      cIn.Next;  //toma token {$ELSE}
      //Explora, sin compilar, hasta encontrar directiva delimitadora.
      if not ScanIFDEF(direc) then begin
        //Llegó al final del código fuente, sin encontrar el ENDIF
        GenErrorDir(ER_ENDIF_NOFOU);
        exit;
      end;
      //Encontró token delimitador
      //Si es $ENDIF, no hay problema, todo termina allí, pero si es un else:
      if direc='ELSE' then begin
        GenErrorDir(ER_UNEXP_ELSE);
        exit;
      end;
      //Encontró un $ENDIF
      dec(WaitForEndIF);  //lleva la cuenta
    end else begin
      //No se esperaba
      GenErrorDir(ER_UNEXP_ENDIF);
      exit;
    end;
  end;
  'ENDIF'    : begin
    if WaitForEndIF>0 then begin
      //Se esperaba el delimitador
      Dec(WaitForEndIF);  //para que ya no siga buscando
    end else begin
      //No se esperaba
      GenErrorDir(ER_UNEXP_ENDIF);
      exit;
    end;
  end;
  'MODE'     : ProcMODE;
  'MSGBOX'   : ProcMSGBOX;
  else
    //Puede ser una macro
    mac := FindMacro(lexDir.GetToken);
    if mac <> nil  then begin
      p := cIn.ReadSrcPos;   //Guarda posición del token
      cIn.Next;  //pasa la directiva
      cIn.NewContextFromTxt(
        mac.value, //Pasa a explorar contenido de la macro como cadena
        mac.posDef.fil {Fija el archivo de definiición de la macro.}
      );
      cIn.curCon.autoClose := true;   //Para que se cierre, al finalizar
      cIn.curCon.FixErrPos := true;   //Para que se ignore la posición de los errores
      cIn.curCon.ErrPosition := p;    //Posición a usar para ubicar el error
      cIn.curCon.PreErrorMsg := 'Macro '+mac.name+': ';
      ctxChanged := true;  //Marca bandera para indciar que se ha cambiado de contexto
      //MsgBox(mac.value);
    end else begin
      GenErrorDir(ER_UNKNO_DIREC, [lexDir.GetToken]);
      exit;
    end;
  end;
end;
procedure TParserDirec.ClearMacros;
begin
  macroList.Clear;
  WaitForEndIF := 0;
  ConfigWord := -1;   //-1 significa, No Inicializado
end;
constructor TParserDirec.Create;
begin
  inherited Create;
  lexDir := TSynFacilSyn.Create(nil);  //crea lexer para analzar directivas
  macroList := TPicMacro_list.Create(true);
  DefLexDirectiv;
end;
destructor TParserDirec.Destroy;
begin
  macroList.Destroy;
  lexDir.Destroy;
  inherited Destroy;
end;
procedure TParserDirec.DefLexDirectiv;
(*Define la sintaxis del lexer que se usará para analizar las directivas. La que
 debe estar entre los símbolo {$ ... }
*)
begin
  dirOperator := lexDir.NewTokType('operator');
  dirDelimiter:= lexDir.NewTokType('delimiter');
  //solo se requiere identificadores y números
  lexDir.DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');
  lexDir.DefTokContent('[0-9]', '[0-9.]*', lexDir.tnNumber);
  lexDir.DefTokContent('[$]','[0-9A-Fa-f]*', lexDir.tnNumber);
  lexDir.DefTokDelim('''', '''', lexDir.tnString);  //cadenas
  lexDir.AddSymbSpec('=', dirOperator);
  lexDir.AddSymbSpec('{$', dirDelimiter);
  lexDir.AddSymbSpec('}', dirDelimiter);
  lexDir.Rebuild;
end;

end.

