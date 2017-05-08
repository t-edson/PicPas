{Unidad con rutinas del analizador sintáctico.
}
unit Parser;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, lclProc, SynEditHighlighter, types, SynFacilHighlighter,
  MisUtils, XpresBas, XpresTypes, XpresElementsPIC, XpresParserPIC, Pic16Utils,
  Pic16Devices, Globales, ProcAsm, GenCod, GenCodPic, FormConfig {Por diseño, parecería que GenCodPic, no debería accederse desde aquí};
type

 { TCompiler }
  TCompiler = class(TGenCod)
  private  //Funciones básicas
    mainFile: string;  //archivo inicial que se compila
    lexDir : TSynFacilSyn;  //lexer para analizar directivas
    procedure cInNewLine(lin: string);
    function StartOfSection: boolean;
    procedure ResetFlashAndRAM;
    procedure getListOfIdent(var itemList: TStringDynArray; out
      srcPosArray: TSrcPosArray);
    procedure ProcComments;
    procedure CaptureDecParams(fun: TxpEleFun);
    procedure CompileIF;
    procedure CompileREPEAT;
    procedure CompileWHILE;
    procedure TreeElemsTreeChange;
  protected   //Métodos OVERRIDE
    procedure TipDefecNumber(var Op: TOperand; toknum: string); override;
    procedure TipDefecString(var Op: TOperand; tokcad: string); override;
    procedure TipDefecBoolean(var Op: TOperand; tokcad: string); override;
  private  //Rutinas para la compilación y enlace
    procedure CompileProcBody(fun: TxpEleFun);
  private //Compilación de secciones
    procedure CompileConstDeclar;
    procedure CompileGlobalVarDeclar;
    procedure CompileProcDeclar;
    procedure CompileInstructionDummy;
    procedure CompileInstruction;
    procedure DefLexDirectiv;
    procedure CompileCurBlock;
    procedure CompileUnit(uni: TxpEleUnit);
    procedure CompileUsesDeclaration;
    procedure CompileProgram;
    procedure CompileLinkProgram;
  public
    procedure Compile(NombArc: string; LinArc: Tstrings);
    procedure RAMusage(lins: TStrings; varDecType: TVarDecType);  //uso de memoria RAM
    procedure DumpCode(lins: TSTrings; incAdrr, incCom: boolean);  //uso de la memoria Flash
    function RAMusedStr: string;
    function FLASHusedStr: string;
    procedure GetResourcesUsed(out ramUse, romUse, stkUse: single);
    constructor Create; override;
    destructor Destroy; override;
  end;

//procedure Compilar(NombArc: string; LinArc: Tstrings);
var
  cxp : TCompiler;

implementation

procedure GenError(msg: string);
begin
  cxp.GenError(msg);
end;
procedure GenError(msg: string; const Args: array of const);
begin
  cxp.GenError(msg,Args);
end;
function HayError: boolean;
begin
  Result := cxp.HayError;
end;
//Funciones básicas
procedure TCompiler.cInNewLine(lin: string);
//Se pasa a una nueva _Línea en el contexto de entrada
begin
  pic.addTopComm('    ;'+trim(lin));  //agrega _Línea al código ensmblador
end;
function TCompiler.StartOfSection: boolean;
begin
  Result := (cIn.tokL ='var') or (cIn.tokL ='const') or
            (cIn.tokL ='type') or (cIn.tokL ='procedure');
end;
procedure TCompiler.ResetFlashAndRAM;
{Reinicia el dispositivo, para empezar a escribir en la posición $000 de la FLASH, y
en laposición inicial de la RAM.}
begin
  pic.iFlash := 0;  //Ubica puntero al inicio.
  pic.ClearMemRAM;  //Pone las celdas como no usadas y elimina nombres.
  CurrBank := 0;
  StartRegs;        //Limpia registros de trabajo, auxiliares, y de pila.
end;
procedure TCompiler.getListOfIdent(var itemList: TStringDynArray; out srcPosArray: TSrcPosArray);
{Lee una lista de identificadores separados por comas, hasta encontra un caracter distinto
de coma. Si el primer elemento no es un identificador o si después de la coma no sigue un
identificador, genera error.
También devuelve una lista de las posiciones de los identificadores, en el código fuente.}
var
  item: String;
  n: Integer;
begin
  setlength(srcPosArray,0 );
  setlength(itemList, 0);  //hace espacio
  repeat
    cIn.SkipWhites;
    //ahora debe haber un identificador
    if cIn.tokType <> tnIdentif then begin
      GenError('Identifier expected.');
      exit;
    end;
    //hay un identificador
    item := cIn.tok;
    //sgrega nombre de ítem
    n := high(itemList)+1;
    setlength(itemList, n+1);  //hace espacio
    setlength(srcPosArray, n+1);  //hace espacio
    itemList[n] := item;  //agrega nombre
    srcPosArray[n] := cIn.ReadSrcPos;  //agrega ubicación de declaración
    cIn.Next;  //lo toma identificador despues, de guardar ubicación
    cIn.SkipWhites;
    if cIn.tok <> ',' then break; //sale
    cIn.Next;  //toma la coma
  until false;
end;
procedure TCompiler.ProcComments;
//Procesa comentarios y directivas
  function tokType: integer;
  begin
    Result := lexdir.GetTokenKind;
  end;
  procedure skipWhites;
  begin
    if tokType = lexDir.tnSpace then
      lexDir.Next;  //quita espacios
  end;
var
  f: Integer;
begin
  cIn.SkipWhites;
  while (cIn.tokType = tnDirective) or (cIn.tokType = tnAsm) do begin
    if cIn.tokType = tnAsm then begin
      //procesa la línea ASM
      ProcASMlime(cIn.tok);
      if HayError then exit;
    end else begin
      //Se ha detectado una directiva
      //Usa SynFacilSyn como lexer para analizar texto
      lexDir.SetLine(copy(Cin.tok,3,1000), 0);  //inicica cadena
      if tokType = lexDir.tnSpace then  lexDir.Next;  //quita espacios
      if tokType <> lexDir.tnIdentif then begin
        GenError('Error in directive.');
        exit;
      end;
      //sigue identificador
      case UpperCase(lexDir.GetToken) of
      'PROCESSOR': begin
        lexDir.Next;  //pasa al siguiente
        skipWhites;
        if not GetHardwareInfo(pic, lexDir.GetToken) then begin
          GenError('Unknown device: %s', [lexDir.GetToken]);
          exit;
        end;
      end;
      'FREQUENCY': begin
        lexDir.Next;  //pasa al siguiente
        skipWhites;
        if tokType <> lexDir.tnNumber then begin
          GenError('Error in directive.');
          exit;
        end;
        if not TryStrToInt(lexDir.GetToken, f) then begin
          GenError('Error in frecuencia.');
          exit;
        end;
        lexDir.Next;  //pasa al siguiente
        skipWhites;
        case UpperCase(lexDir.GetToken) of
        'KHZ': f := f * 1000;
        'MHZ': f := f * 1000000;
        else
          GenError('Error in directive.');
          exit;
        end;
        pic.frequen:=f; //asigna freecuencia
      end;
      'POINTERS': begin
        lexDir.Next;  //pasa al siguiente
        skipWhites;

      end;
      'CONFIG': begin
        lexDir.Next;  //pasa al siguiente
        skipWhites;

      end;
      'DEFINE': begin
        lexDir.Next;  //pasa al siguiente
        skipWhites;

      end;
      else
        GenError('Unknown directive: %s', [lexDir.GetToken]);
        exit;
      end;
    end;
    //pasa w siguiente
    cIn.Next;
    cIn.SkipWhites;  //limpia blancos
  end;
end;
procedure TCompiler.CaptureDecParams(fun: TxpEleFun);
//Lee la declaración de parámetros de una función.
var
  parType: String;
  parName: String;
begin
  SkipWhites;
  if EOBlock or EOExpres then begin
    //no tiene parámetros
  end else begin
    //Debe haber parámetros
    if cIn.tok<>'(' then begin
      GenError('"(" expected.');
      exit;
    end;
    cin.Next;
    repeat
      if cIn.tokType <> tnIdentif then begin
        GenError('Identifier expected.');
        exit;
      end;
      parName := cIn.tok;   //lee tipo de parámetro
      cIn.Next;
      cIn.SkipWhites;
      if cIn.tok<>':' then begin
        GenError('":" expected.');
        exit;
      end;
      cIn.Next;
      cIn.SkipWhites;

      if (cIn.tokType <> tnType) then begin
        GenError('Identifier of type expected.');
        exit;
      end;
      parType := cIn.tok;   //lee tipo de parámetro
      cIn.Next;
      //ya tiene el nombre y el tipo
      CreateParam(fun, parName, parType);
      if HayError then exit;
      if cIn.tok = ';' then begin
        cIn.Next;   //toma separador
        SkipWhites;
      end else begin
        //no sigue separador de parámetros,
        //debe terminar la lista de parámetros
        //¿Verificar EOBlock or EOExpres ?
        break;
      end;
    until false;
    //busca paréntesis final
    if cIn.tok<>')' then begin
      GenError('")" expected.'); exit;
    end;
    cin.Next;
  end;
end;
procedure TCompiler.CompileIF;
{Compila uan extructura IF}
var
  dg: Integer;
  dg2: Integer;
begin
  GetExpressionE(0);
  if HayError then exit;
  if res.typ<>typBool then begin
    GenError('Boolean expression expected.');
    exit;
  end;
  cIn.SkipWhites;
  if cIn.tokL<>'then' then begin
    GenError('"then" expected.');
    exit;
  end;
  cIn.Next;   //toma "then"
  //aquí debe estar el cuerpo del "if"
  case res.catOp of
  coConst: begin  //la condición es fija
    if res.valBool then begin
      //es verdadero, siempre se ejecuta
      CompileInstruction;
      if HayError then exit;
      if cIn.tokL = 'else' then begin
        //hay bloque ELSE, pero no se ejecutará nunca
        cIn.Next;   //toma "else"
        CompileInstructionDummy;  //solo para mantener la sintaxis
        if HayError then exit;
      end;
    end else begin
      //aquí se debería lanzar una advertencia
      CompileInstructionDummy;  //solo para mantener la sintaxis
      if HayError then exit;
      if cIn.tokL = 'else' then begin
        //hay bloque ELSE, que sí se ejecutará
        cIn.Next;   //toma "else"
        CompileInstruction;
        if HayError then exit;
      end;
    end;
  end;
  coVariab:begin
    _BTFSS(res.offs, res.bit);  //verifica condición
    _GOTO_PEND(dg);  //salto pendiente
    CompileInstruction;
    if HayError then exit;
    if cIn.tokL <> 'else' then begin  //no hay blqoue ELSE
      pic.codGotoAt(dg, _PC);   //termina de codificar el salto
    end else begin
      //hay bloque ELSE
      cIn.Next;   //toma "else"
      _GOTO_PEND(dg2);  //salto pendiente
      pic.codGotoAt(dg, _PC);   //termina de codificar el salto
      CompileInstruction;
      if HayError then exit;
      pic.codGotoAt(dg2, _PC);   //termina de codificar el salto
    end;
  end;
  coExpres:begin
    if res.Inverted then begin //_Lógica invertida
      _BTFSC(Z.offs, Z.bit);   //verifica condición
    end else begin
      _BTFSS(Z.offs, Z.bit);   //verifica condición
    end;
    _GOTO_PEND(dg);  //salto pendiente
    CompileInstruction;
    if HayError then exit;
    if cIn.tokL <> 'else' then begin  //no hay blqoue ELSE
      pic.codGotoAt(dg, _PC);   //termina de codificar el salto
    end else begin
      //hay bloque ELSE
      cIn.Next;   //toma "else"
      _GOTO_PEND(dg2);  //salto pendiente
      pic.codGotoAt(dg, _PC);   //termina de codificar el salto
      CompileInstruction;
      if HayError then exit;
      pic.codGotoAt(dg2, _PC);   //termina de codificar el salto
    end;
  end;
  end;
end;
procedure TCompiler.CompileREPEAT;
{Compila uan extructura WHILE}
var
  l1: Word;
begin
  l1 := _PC;        //guarda dirección de inicio
//  CompileInstruction;  //debería completarse las instrucciones de tipo "break"
  CompileCurBlock;
  if HayError then exit;
  cIn.SkipWhites;
  if cIn.tokL<>'until' then begin
    GenError('"until" expected.');
    exit;
  end;
  cIn.Next;   //toma "until"
  GetExpressionE(0);
  if HayError then exit;
  if res.typ<>typBool then begin
    GenError('Boolean expression expected.');
    exit;
  end;
  case res.catOp of
  coConst: begin  //la condición es fija
    if res.valBool then begin
      //lazo nulo
    end else begin
      //lazo infinito
      _GOTO(l1);
    end;
  end;
  coVariab:begin
    _BTFSS(res.offs, res.bit);  //verifica condición
    _GOTO(l1);
    //sale cuando la condición es verdadera
  end;
  coExpres:begin
    if res.Inverted then begin //_Lógica invertida
      _BTFSC(Z.offs, Z.bit);   //verifica condición
    end else begin
      _BTFSS(Z.offs, Z.bit);   //verifica condición
    end;
    _GOTO(l1);
    //sale cuando la condición es verdadera
  end;
  end;
end;
procedure TCompiler.CompileWHILE;
{Compila uan extructura WHILE}
var
  l1: Word;
  dg: Integer;
begin
  l1 := _PC;        //guarda dirección de inicio
  GetExpressionE(0);
  if HayError then exit;
  if res.typ<>typBool then begin
    GenError('Boolean expression expected.');
    exit;
  end;
  cIn.SkipWhites;
  if cIn.tokL<>'do' then begin
    GenError('"do" expected.');
    exit;
  end;
  cIn.Next;   //toma "do"
  //aquí debe estar el cuerpo del "while"
  case res.catOp of
  coConst: begin  //la condición es fija
    if res.valBool then begin
      //lazo infinito
      CompileInstruction;  //debería completarse las instrucciones de tipo "break"
      if HayError then exit;
      _GOTO(l1);
    end else begin
      //lazo nulo
      //aquí se debería lanzar una advertencia
      CompileInstructionDummy;
      if HayError then exit;
    end;
  end;
  coVariab:begin
    _BTFSS(res.offs, res.bit);  //verifica condición
    _GOTO_PEND(dg);  //salto pendiente
    CompileInstruction;
    if HayError then exit;
    _GOTO(l1);
    //ya se tiene el destino del salto
    pic.codGotoAt(dg, _PC);   //termina de codificar el salto
  end;
  coExpres:begin
    if res.Inverted then begin  //_Lógica invertida
      _BTFSC(Z.offs, Z.bit);   //verifica condición
    end else begin
      _BTFSS(Z.offs, Z.bit);   //verifica condición
    end;
    _GOTO_PEND(dg);  //salto pendiente
    CompileInstruction;
    if HayError then exit;
    _GOTO(l1);
    //ya se tiene el destino del salto
    pic.codGotoAt(dg, _PC);   //termina de codificar el salto
  end;
  end;
end;
procedure TCompiler.TreeElemsTreeChange;
begin
  GenError('Internal Error: Syntax Tree modified on linking.');
end;
//Métodos OVERRIDE
procedure TCompiler.TipDefecNumber(var Op: TOperand; toknum: string);
{Procesa constantes numéricas, ubicándolas en el tipo de dato apropiado (byte, word, ... )
 Si no logra ubicar el tipo de número, o no puede leer su valor, generará  un error.}
var
  n: int64;   //para almacenar a los enteros
//  f: extended;  //para almacenar a reales
begin
  if pos('.',toknum) <> 0 then begin  //es flotante
    Op.catTyp := t_float;   //es flotante
    GenError('Unvalid float number.');  //No hay soporte aún para flotantes
//    try
//      f := StrToFloat(toknum);  //carga con la mayor precisión posible
//    except
//      Op.typ := nil;
//      GenError('Unvalid float number.');
//      exit;
//    end;
//    //busca el tipo numérico más pequeño que pueda albergar a este número
//    Op.size := 4;   //se asume que con 4 bytes bastará
//    {Aquí se puede decidir el tamaño de acuerdo a la cantidad de decimales indicados}
//
//    Op.valFloat := f;  //debe devolver un extended
//    menor := 1000;
//    for i:=0 to typs.Count-1 do begin
//      { TODO : Se debería tener una lista adicional TFloatTypes, para acelerar la
//      búsqueda}
//      if (typs[i].cat = t_float) and (typs[i].size>=Op.size) then begin
//        //guarda el menor
//        if typs[i].size < menor then  begin
//           imen := i;   //guarda referencia
//           menor := typs[i].size;
//        end;
//      end;
//    end;
//    if menor = 1000 then  //no hubo tipo
//      Op.typ := nil
//    else  //encontró
//      Op.typ:=typs[imen];
//
  end else begin     //es entero
    Op.catTyp := t_uinteger;   //es entero sin signo
    //Intenta convertir la cadena. Notar que se reconocen los formatos $FF y %0101
    if not TryStrToInt64(toknum, n) then begin
      //Si el lexer ha hecho bien su trabajo, esto solo debe pasar, cuando el
      //número tiene muhcos dígitos.
      GenError('Error in number.');
      exit;
    end;
    Op.valInt := n;   //copia valor de constante entera
    {Asigna un tipo, de acuerdo al rango. Notar que el tipo más pequeño, usado
    es el byte, y no el bit.}
    if (n>=0) and  (n<=255) then begin
      Op.size := 1;
      Op.typ := typByte;
    end else if (n>= 0) and  (n<=65535) then begin
      Op.size := 2;
      Op.typ := typWord;
    end else  begin //no encontró
      GenError('No type defined to accommodate this number.');
      Op.typ := nil;
    end;
  end;
end;
procedure TCompiler.TipDefecString(var Op: TOperand; tokcad: string);
//Devuelve el tipo de cadena encontrado en un token
//var
//  i: Integer;
begin
{  Op.catTyp := t_string;   //es cadena
  Op.size:=length(tokcad);
  //toma el texto
  Op.valStr := copy(cIn.tok,2, length(cIn.tok)-2);   //quita comillas
  //////////// Verifica si hay tipos string definidos ////////////
  if length(Op.valStr)=1 then begin
    Op.typ := tipChr;
  end else
    Op.typ :=nil;  //no hay otro tipo}
end;
procedure TCompiler.TipDefecBoolean(var Op: TOperand; tokcad: string);
//Devuelve el tipo de cadena encontrado en un token
begin
  Op.catTyp := t_boolean;   //es flotante
  Op.size:=-1;   //se usará un byte
  //convierte valor constante
  Op.valBool:= (tokcad[1] in ['t','T']);
  Op.typ:=typBool;
end;
//Rutinas para la compilación y enlace
procedure TCompiler.CompileProcBody(fun: TxpEleFun);
{Compila la declaración de un procedimiento}
begin
  StartCodeSub(fun);  //inicia codificación de subrutina
  CompileInstruction;
  if HayError then exit;
  _RETURN();  //instrucción de salida
  EndCodeSub;  //termina codificación
  fun.srcSize := pic.iFlash - fun.adrr;   //calcula tamaño
end;
//Compilación de secciones
procedure TCompiler.CompileConstDeclar;
var
  consNames: array of string;  //nombre de variables
  cons: TxpEleCon;
  srcPosArray: TSrcPosArray;
  i: integer;
begin
  //procesa lista de constantes a,b,cons ;
  getListOfIdent(consNames, srcPosArray);
  if HayError then begin  //precisa el error
    GenError('Identifier of constant expected.');
    exit;
  end;
  //puede seguir "=" o identificador de tipo
  if cIn.tok = '=' then begin
    cIn.Next;  //pasa al siguiente
    //Debe seguir una expresiócons constante, que no genere consódigo
    GetExpressionE(0);
    if HayError then exit;
    if res.catOp <> coConst then begin
      GenError('Constant expression expected.');
    end;
    //Hasta aquí todo bien, crea la(s) constante(s).
    for i:= 0 to high(consNames) do begin
      //crea constante
      cons := CreateCons(consNames[i], res.typ);
      cons.srcDec := srcPosArray[i];  //guarda punto de declaración
      if not TreeElems.AddElement(cons) then begin
        GenErrorPos('Duplicated identifier: "%s"', [cons.name], cons.srcDec);
        cons.Destroy;   //hay una constante creada
        exit;
      end;
      res.CopyConsValTo(cons); //asigna valor
    end;
//  end else if cIn.tok = ':' then begin
  end else begin
    GenError('"=" or "," expected.');
    exit;
  end;
  if not CaptureDelExpres then exit;
  ProcComments;
  //puede salir con error
end;
procedure TCompiler.CompileGlobalVarDeclar;
{Compila la declaración de variables. Usa una sintaxis, sencilla, similar w la de
 Pascal. Lo normal seríw que se sobreescriba este método para adecuarlo al lenguaje
 que se desee implementar. }
var
  absAddr: integer;
  absBit: integer;
  Number: TOperand;  //para ser es usado por las subrutinas

  procedure CheckAbsoluteBit;
  {Extrae la parte del bit de una dirección absoluta. Actualiza "absBit"}
  begin
    if cIn.tok<>'.' then begin
      GenError('"." expected.');
      exit;
    end;
    cIn.Next;    //Pasa al siguiente
    //toma posición de bit
    TipDefecNumber(Number, cIn.tok); //encuentra tipo de número, tamaño y valor
    if HayError then exit;  //verifica
    if Number.CanBeByte then
       absBit := Number.valInt
    else begin
      GenError('Invalid memory address.');
      exit;
    end;
    if absBit > 7 then begin
      GenError('Invalid memory address.');
      exit;
    end;
    cIn.Next;    //Pasa al siguiente
  end;
  procedure CheckAbsolute(var IsAbs: boolean; const IsBit: boolean);
  {Verifica si lo que sigue es la sintaxis ABSOLUTE ... . Si esa así, procesa el texto,
  pone "IsAbs" en TRUE y actualiza los valores "absAddr" y "absBit". }
  var
    xvar: TxpEleVar;
    n: integer;
    tmp: String;
  begin
    IsAbs := false;   //bandera
    absAddr := -1;  //inicia valores pore defecto, por seguridad
    absBit := -1;   //inicia valores pore defecto, por seguridad
    if (cIn.tokL <> 'absolute') and (cIn.tok <> '@') then begin
      exit;  //no es variable absoluta
    end;
    //// Hay especificación de dirección absoluta ////
    IsAbs := true;    //marca bandera
    cIn.Next;
    cIn.SkipWhites;
    if cIn.tokType = tnNumber then begin
      TipDefecNumber(Number, cIn.tok); //encuentra tipo de número, tamaño y valor
      if HayError then exit;  //verifica
      if Number.catTyp = t_float then begin
        //Caso especial porque el puede ser el formato <dirección>.<bit> que es
        //totalmenet válido, y el lexer lo captura como un solo token.
        if IsBit then begin
          //Puede ser válido el número "decimal", hay que extraer los campos,
          //pero primero hay que asegurarnos que no tenga notación exponencial.
          if (pos('e', cIn.tok)<>0) or (pos('E', cIn.tok)<>0) then begin
            GenError('Invalid memory address.');
            exit;
          end;
          //ya sabemos que tiene que ser decimal, con punto
          absAddr := trunc(Number.valFloat);   //la dirección es la parte entera
          n := pos('.', cIn.tok);   //no debe fallar
          tmp := copy(cIn.tok, n+1, 1000);   //copia parte decimal
          if length(tmp)<>1 then begin  //valida longitud
            GenError('Invalid memory address.');
            exit;
          end;
          absBit:=StrToInt(tmp);   //no debe fallar
          //valida
          if not pic.ValidRAMaddr(absAddr) then begin
            GenError('Invalid memory address for this device.');
            exit;
          end;
          if absBit > 7 then begin
            GenError('Invalid memory address.');
            exit;
          end;
          cIn.Next;    //Pasa al siguiente
        end else begin  //no puede ser correcto
          GenError('Invalid memory address.');
          exit;
        end;
      end else begin
        //Se asume número entero
        if Number.CanBeWord then
           absAddr := Number.aWord
        else begin
          GenError('Invalid memory address.');
          exit;
        end;
        if not pic.ValidRAMaddr(absAddr) then begin
          GenError('Invalid memory address for this device.');
          exit;
        end;
        cIn.Next;    //Pasa al siguiente
        if IsBit then begin
          CheckAbsoluteBit;  //es un boolean, debe especificarse el bit
          if HayError then exit;  //verifica
        end;
      end;
    end else if cIn.tokType = tnIdentif then begin
      //Puede ser variable
      xvar := TreeElems.FindVar(cIn.tok);
      if xvar<>nil then begin
        //Es variable
        absAddr := xvar.AbsAddr;  //debe ser absoluta
        if absAddr = ADRR_ERROR then begin
          GenError('Unknown type.');
          exit;
        end;
      end else begin
        GenError('Identifier of variable expected.');
        exit;
      end;
      cIn.Next;    //Pasa al siguiente
      if IsBit then begin
        CheckAbsoluteBit;  //es un boolean o bit, debe especificarse el bit
        if HayError then exit;  //verifica
      end;
    end else begin   //error
      GenError('Numeric address expected.');
      exit;
    end;
  end;

var
  varType: String;
  varNames: array of string;  //nombre de variables
  isAbsolute: Boolean;
  typ: TType;
  xvar: TxpEleVar;
  srcPosArray: TSrcPosArray;
  i: Integer;
begin
  //Procesa variables a,b,c : int;
  getListOfIdent(varNames, srcPosArray);
  if HayError then begin  //precisa el error
    GenError('Identifier of variable expected.');
    exit;
  end;
  //usualmente debería seguir ":"
  if cIn.tok = ':' then begin
    //Debe seguir, el tipo de la variable
    cIn.Next;  //lo toma
    cIn.SkipWhites;
    if (cIn.tokType <> tnType) then begin
      GenError('Identifier of type expected.');
      exit;
    end;
    varType := cIn.tok;   //lee tipo
    cIn.Next;
    cIn.SkipWhites;
    //Valida el tipo
    typ := FindType(varType);
    if typ = nil then begin
      GenError('Undefined type "%s"', [varType]);
      exit;
    end;
    //verifica si tiene dirección absoluta
    CheckAbsolute(isAbsolute, (typ = typBool) or (typ = typBit) );
    if HayError then exit;
    //reserva espacio para las variables
    for i := 0 to high(varNames) do begin
      xvar := CreateVar(varNames[i], typ, absAddr, absBit);
      xvar.srcDec := srcPosArray[i];  //Actualiza posición
      CreateVarInRAM(xvar);  //Crea la variable
      if HayError then begin
        xvar.Destroy;   //Hay una variable creada
        exit;
      end;
      if not TreeElems.AddElement(xvar) then begin
        GenErrorPos('Duplicated identifier: "%s"', [xvar.name], xvar.srcDec);
        xvar.Destroy;   //Hay una variable creada
        exit;
      end;
    end;
  end else begin
    GenError('":" or "," expected.');
    exit;
  end;
  if not CaptureDelExpres then exit;
  ProcComments;
  //puede salir con error
end;
procedure TCompiler.CompileProcDeclar;
{Compila la declaración de procedimientos. Tanto procedimientos como funciones
 se manejan internamente como funciones}
var
  procName: String;
  fun: TxpEleFun;
  srcPos: TSrcPos;
begin
  {Este método, solo se ejecutará en la primera pasada, en donde todos los procedimientos
  se codifican al inicio de la memoria FLASH, y las variables y registros se ubican al
  inicio de la memeoria RAM, ya que lo que importa es simplemente recabar información
  del procedimiento, y no tanto codificarlo. }
  ResetFlashAndRAM;   //Limpia RAM y FLASH
  //Toma información de ubicación, al inicio del procedimiento
  srcPos := cIn.ReadSrcPos;
  cIn.SkipWhites;
  //Ahora debe haber un identificador
  if cIn.tokType <> tnIdentif then begin
    GenError('Identifier expected.');
    exit;
  end;
  //hay un identificador
  procName := cIn.tok;
  cIn.Next;  //lo toma
  {Ya tiene los datos mínimos para crear la función. La crea con "proc" en NIL,
  para indicar que es una función definida por el usuario.}
  fun := CreateFunction(procName, typNull, @callFunct);
  fun.srcDec := srcPos;   //Toma ubicación en el código
  TreeElems.AddElementAndOpen(fun);  //Se abre un nuevo espacio de nombres

  CaptureDecParams(fun);
  if HayError then exit;
  //Recién aquí puede verificar duplicidad, porque ya se leyeron los parámetros
  if not ValidateFunction then exit;
  cIn.SkipWhites;
  if cIn.tok=';' then begin //encontró delimitador de expresión
    cIn.Next;   //lo toma
    ProcComments;  //quita espacios
    if HayError then exit;   //puede dar error por código assembler o directivas
  end else begin  //hay otra cosa
    GenError('";" expected.');  //por ahora
    exit;  //debe ser un error
  end;
  //Empiezan las declaraciones VAR, CONST, PROCEDURE, TYPE
  while StartOfSection do begin
    if cIn.tokL = 'var' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'begin') do begin
        CompileGlobalVarDeclar;
        if HayError then exit;;
      end;
    end else if cIn.tokL = 'const' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'begin') do begin
        CompileConstDeclar;
        if HayError then exit;;
      end;
//    end else if cIn.tokL = 'procedure' then begin
//      cIn.Next;    //lo toma
//      CompileProcDeclar;
    end else begin
      GenError('Not implemented: "%s"', [cIn.tok]);
      exit;
    end;
  end;
  if cIn.tokL <> 'begin' then begin
    GenError('"begin" expected.');
    exit;
  end;
  //Ahora empieza el cuerpo de la función o las declaraciones
  fun.adrr := pic.iFlash;    //toma dirección de inicio del código. Es solo referencial.
  fun.posCtx := cIn.PosAct;  //Guarda posición para la segunda compilación
  CompileProcBody(fun);
  TreeElems.CloseElement; //cierra espacio de nombres de la función
  if cIn.tokType=tnExpDelim then begin //encontró delimitador de expresión
    cIn.Next;   //lo toma
    ProcComments;  //quita espacios
    if HayError then exit;   //puede dar error por código assembler o directivas
  end else begin  //hay otra cosa
    GenError('";" expected.');  //por ahora
    exit;  //debe ser un error
  end;
end;
procedure TCompiler.CompileInstructionDummy;
{Compila una instrucción pero sin generar código. }
var
  p: Integer;
begin
  p := pic.iFlash;
  CompileInstruction;  //compila solo para mantener la sintaxis
  pic.iFlash := p;     //elimina lo compilado
  //puede salir con error
  { TODO : Debe limpiar la memoria flash que ocupó, para dejar la casa limpia. }
end;
procedure TCompiler.CompileInstruction;
{Compila una única instrucción o un bloque BEGIN ... END. Puede generara Error.}
begin
  ProcComments;
  if HayError then exit;   //puede dar error por código assembler o directivas
  if cIn.tokL='begin' then begin
    //es bloque
    cIn.Next;  //toma "begin"
    CompileCurBlock;   //llamada recursiva
    if HayError then exit;
    if cIn.tokL<>'end' then begin
      GenError('"end" expected.');
      exit;
    end;
    cIn.Next;  //toma "end"
    ProcComments;
    //puede salir con error
  end else begin
    //es una instrucción
    if cIn.tokType = tnStruct then begin
      if cIn.tokl = 'if' then begin
        cIn.Next;         //pasa "if"
        CompileIF;
      end else if cIn.tokl = 'while' then begin
        cIn.Next;         //pasa "while"
        CompileWHILE;
      end else if cIn.tokl = 'repeat' then begin
        cIn.Next;         //pasa "until"
        CompileREPEAT;
      end else begin
        GenError('Unknown structure.');
        exit;
      end;
    end else begin
      //debe ser es una expresión
      GetExpressionE(0);
    end;
    if HayError then exit;;
    if pic.MsjError<>'' then begin
      //El pic también puede dar error
      GenError(pic.MsjError);
    end;
  end;
end;
procedure TCompiler.CompileCurBlock;
{Compila el bloque de código actual hasta encontrar un delimitador de bloque, o fin
de archivo. }
begin
  ProcComments;
  if HayError then exit;   //puede dar error por código assembler o directivas
  while not cIn.Eof and (cIn.tokType<>tnBlkDelim) do begin
    //se espera una expresión o estructura
    CompileInstruction;
    if HayError then exit;   //aborta
    //se espera delimitador
    if cIn.Eof then break;  //sale por fin de archivo
    //busca delimitador
    ProcComments;
    if HayError then exit;   //puede dar error por código assembler o directivas
    if cIn.tokType=tnExpDelim then begin //encontró delimitador de expresión
      cIn.Next;   //lo toma
      ProcComments;  //quita espacios
      if HayError then exit;   //puede dar error por código assembler o directivas
    end else begin  //hay otra cosa
      GenError('";" expected.');  //por ahora
      exit;  //debe ser un error
    end;
  end;
end;
procedure TCompiler.CompileUnit(uni: TxpEleUnit);
{Realiza la compilación de una unidad}
begin
  ClearError;
  pic.MsjError := '';
  ProcComments;
  if HayError then exit;
  //Busca UNIT
  if cIn.tokL = 'unit' then begin
    cIn.Next;  //pasa al nombre
    ProcComments;
    if HayError then exit;   //puede dar error por código assembler o directivas
    if cIn.Eof then begin
      GenError('Name of unit expected.');
      exit;
    end;
    if UpCase(cIn.tok)<>UpCase(uni.name) then begin
      GenError('Name of unit doesn''t match file name.');
      exit;
    end;
    cIn.Next;  //Toma el nombre y pasa al siguiente
    if not CaptureDelExpres then exit;
  end else begin
    GenError('Expected: UNIT');
    exit;
  end;
  ProcComments;
  if HayError then exit;
  if cIn.tokL <> 'interface' then begin
    GenError('Expected: INTERFACE');
    exit;
  end;
  cIn.Next;   //toma
  ProcComments;
  if cIn.Eof then begin
    GenError('Expected "uses", "var", "type", "const" or "implementation".');
    exit;
  end;
  ProcComments;
  //Busca USES
  CompileUsesDeclaration;
  if cIn.Eof then begin
    GenError('Expected "var", "type" or "const".');
    exit;
  end;
  ProcComments;
//  Cod_StartProgram;  //Se pone antes de codificar procedimientos y funciones
  if HayError then exit;
  //Empiezan las declaraciones
  while StartOfSection do begin
    if cIn.tokL = 'var' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'implementation') do begin
        CompileGlobalVarDeclar;
        if HayError then exit;;
      end;
    end else if cIn.tokL = 'const' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'implementation') do begin
        CompileConstDeclar;
        if HayError then exit;;
      end;
    end else if cIn.tokL = 'procedure' then begin
      cIn.Next;    //lo toma
      CompileProcDeclar;
    end else begin
      GenError('Not implemented: "%s"', [cIn.tok]);
      exit;
    end;
  end;
  ProcComments;
  if HayError then exit;
  if cIn.tokL <> 'implementation' then begin
    GenError('Expected: IMPLEMENTATION');
    exit;
  end;
  cIn.Next;   //toma
  ProcComments;
  if cIn.Eof then begin
    GenError('Expected "end".');
//    GenError('Expected "end", "initilization", or "finalization".');
    exit;
  end;
  ProcComments;
//  //procesa cuerpo
//  ResetFlashAndRAM;  {No es tan necesario, pero para seguir un orden y tener limpio
//                     también, la flash y memoria, después de algún psoible procedimiento.}
//  if cIn.tokL = 'begin' then begin
//    cIn.Next;   //coge "begin"
//    //Guardamos la ubicación física, real, en el archivo, después del BEGIN
//    TreeElems.main.posCtx := cIn.PosAct;
//    TreeElems.main.src.Fil := cIn.curCon.arc;
//    TreeElems.main.src.Row := cIn.curCon.row;
//    TreeElems.main.src.Col := cIn.curCon.col;
//    //codifica el contenido
//    CompileCurBlock;   //compila el cuerpo
//    if HayError then exit;
//    if cIn.Eof then begin
//      GenError('Unexpected end of file. "end" expected.');
//      exit;       //sale
//    end;
//    if cIn.tokL <> 'end' then begin  //verifica si termina el programa
//      GenError('"end" expected.');
//      exit;       //sale
//    end;
//    cIn.Next;   //coge "end"
//    //debería seguir el punto
//    if cIn.tok <> '.' then begin
//      GenError('"." expected.');
//      exit;       //sale
//    end;
//    cIn.Next;
//    //no debe haber más instrucciones
//    cIn.SkipWhites;
//    if not cIn.Eof then begin
//      GenError('Syntax error. Nothing should be after "END."');
//      exit;       //sale
//    end;
//    _SLEEP();   //agrega instrucción final
//  end else begin
//    GenError('Expected "begin", "var", "type" or "const".');
//    exit;
//  end;
//  Cod_EndProgram;
end;
procedure TCompiler.CompileUsesDeclaration;
{Compila la unidad indicada.}
var
  uni: TxpEleUnit;
  uPath: String;
  uName: String;
begin
  if cIn.tokL = 'uses' then begin
    cIn.Next;  //pasa al nombre
    //Toma una a una las unidades
    repeat
      cIn.SkipWhites;
      //ahora debe haber un identificador
      if cIn.tokType <> tnIdentif then begin
        GenError('Identifier expected.');
        exit;
      end;
      //hay un identificador de unidad
      uName := cIn.tok;
      uni := CreateUnit(uName);
      //Verifica si existe ya el nombre de la unidad
      if uni.DuplicateIn(TreeElems.curNode.elements) then begin
        GenError('Identifier duplicated: %s.', [uName]);
        uni.Destroy;
        exit;
      end;
      uni.srcDec := cIn.ReadSrcPos;   //guarda posición de declaración
      //Ubica al archivo de la unidad
      uName := uName + '.pas';  //nombre de archivo
      //Primero busca en la misma ubicación del archivo fuente
      uPath := ExtractFileDir(mainFile) + DirectorySeparator + uName;
      if FileExists(uPath) then begin
        //Lo encontró. Deja en "uPath"
      end else begin
        //No lo encontró, busca en la carpeta de librerías
        uPath := rutUnits + DirectorySeparator + uName;
        if FileExists(uPath) then begin
          //Lo encontró. Deja en "uPath"
        end else begin
          //No lo encuentra
          uPath := '';
        end;
      end;
      if uPath = '' then begin
        GenError('File not found: %s', [uName]);
        uni.Destroy;
        exit;
      end;
{----}TreeElems.AddElementAndOpen(uni);
      cIn.NewContextFromFile(uPath);
      //Aquí ya se puede realizar otra exploración, como si fuera el archivo principal
      CompileUnit(uni);
      cIn.CloseContext;  //cierra el contexto
      if HayError then exit;  //EL error debe haber guardado la ubicaicón del error
{----}TreeElems.CloseElement; //cierra espacio de nombres de la función
      cIn.Next;  //toma nombre
      cIn.SkipWhites;
      if cIn.tok <> ',' then break; //sale
      cIn.Next;  //toma la coma
    until false;
    if not CaptureDelExpres then exit;
  end;
end;
procedure TCompiler.CompileProgram;
{Compila un programa en el contexto actual. Empieza a codificar el código a partir de
la posición actual de memoria en el PIC (iFlash).}
begin
  TreeElems.Clear;
  TreeElems.OnTreeChange := nil;   //Se va a modificar el árbol
  listFunSys.Clear;
  CreateSystemElements;  //Crea los elementos del sistema
  //ClearTypes;      //limpia los tipos
  //Inicia PIC
  ExprLevel := 0;  //inicia
  pic.ClearMemFlash;
  ResetFlashAndRAM;  {Realmente lo que importa aquí sería limpiar solo la RAM, porque
                      cada procedimiento, reiniciará el puntero de FLASH}
  ClearError;
  pic.MsjError := '';
  ProcComments;
  if HayError then exit;
  //Busca PROGRAM
  if cIn.tokL = 'unit' then begin
    //Se intenta compilar una unidad
    GenError('Cannot compile a unit.');
    exit;
  end;
  if cIn.tokL = 'program' then begin
    cIn.Next;  //pasa al nombre
    ProcComments;
    if HayError then exit;   //puede dar error por código assembler o directivas
    if cIn.Eof then begin
      GenError('Name of program expected.');
      exit;
    end;
    cIn.Next;  //Toma el nombre y pasa al siguiente
    if not CaptureDelExpres then exit;
  end;
  if cIn.Eof then begin
    GenError('Expected "program", "begin", "var", "type" or "const".');
    exit;
  end;
  ProcComments;
  //Busca USES
  CompileUsesDeclaration;

  if cIn.Eof then begin
    GenError('Expected "begin", "var", "type" or "const".');
    exit;
  end;
  ProcComments;
  Cod_StartProgram;  //Se pone antes de codificar procedimientos y funciones
  if HayError then exit;
  //Empiezan las declaraciones
  while StartOfSection do begin
    if cIn.tokL = 'var' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'begin') do begin
        CompileGlobalVarDeclar;
        if HayError then exit;;
      end;
    end else if cIn.tokL = 'const' then begin
      cIn.Next;    //lo toma
      while not StartOfSection and (cIn.tokL <>'begin') do begin
        CompileConstDeclar;
        if HayError then exit;;
      end;
    end else if cIn.tokL = 'procedure' then begin
      cIn.Next;    //lo toma
      CompileProcDeclar;
    end else begin
      GenError('Not implemented: "%s"', [cIn.tok]);
      exit;
    end;
  end;
  //procesa cuerpo
  ResetFlashAndRAM;  {No es tan necesario, pero para seguir un orden y tener limpio
                     también, la flash y memoria, después de algún psoible procedimiento.}
  if cIn.tokL = 'begin' then begin
    cIn.Next;   //coge "begin"
    //Guardamos la ubicación física, real, en el archivo, después del BEGIN
    TreeElems.main.posCtx := cIn.PosAct;
    TreeElems.main.srcDec := cIn.ReadSrcPos;
    //codifica el contenido
    CompileCurBlock;   //compila el cuerpo
    if HayError then exit;
    if cIn.Eof then begin
      GenError('Unexpected end of file. "end" expected.');
      exit;       //sale
    end;
    if cIn.tokL <> 'end' then begin  //verifica si termina el programa
      GenError('"end" expected.');
      exit;       //sale
    end;
    cIn.Next;   //coge "end"
    //debería seguir el punto
    if cIn.tok <> '.' then begin
      GenError('"." expected.');
      exit;       //sale
    end;
    cIn.Next;
    //no debe haber más instrucciones
    cIn.SkipWhites;
    if not cIn.Eof then begin
      GenError('Syntax error. Nothing should be after "END."');
      exit;       //sale
    end;
    _SLEEP();   //agrega instrucción final
  end else begin
    GenError('Expected "begin", "var", "type" or "const".');
    exit;
  end;
  Cod_EndProgram;
end;
procedure TCompiler.CompileLinkProgram;
{Genera el código compilado final. Usa la información del árbol de sintaxis, para
ubicar a los diversos elementos que deben compilarse.
Se debe llamar después de compilar con CompileProgram.
Esto es lo más cercano a un enlazador, que hay en PicPas.}
var
  elem : TxpElement;
  xvar : TxpEleVar;
  fun  : TxpEleFun;
  iniMain: integer;
begin
  TreeElems.OnTreeChange := @TreeElemsTreeChange;  //Protege las modificaciones
  ExprLevel := 0;
  pic.ClearMemFlash;
  ResetFlashAndRAM;
  ClearError;
  pic.MsjError := '';
  //Verifica las constantes usadas
  for elem in TreeElems.main.elements do if elem is TxpEleCon then begin
      if elem.nCalled = 0 then begin
        GenWarnPos('Unused constant: %s', [elem.name], elem.srcDec);
      end;
  end;
  pic.iFlash:= 0;  //inicia puntero a Flash
  //Genera espacio para las variables globales
  for elem in TreeElems.main.elements do if elem is TxpEleVar then begin
      xvar := TxpEleVar(elem);
      if xvar.nCalled>0 then begin
        //Asigna una dirección válida para esta variable
        CreateVarInRAM(xVar);  //Crea la variable
      end else begin
        GenWarnPos('Unused variable: %s', [xVar.name], xvar.srcDec);
        xvar.ResetAddress
      end;
  end;
  pic.iFlash:= 0;  //inicia puntero a Flash
  _GOTO_PEND(iniMain);       //instrucción de salto inicial
  //Codifica las funciones del sistema usadas
  for fun in listFunSys do begin
    if (fun.nCalled > 0) and (fun.compile<>nil) then begin
      //Función usada y que tiene una subrutina ASM
      fun.adrr := pic.iFlash;  //actualiza la dirección final
      PutLabel('__'+fun.name);
      fun.compile(fun);   //codifica
      if HayError then exit;  //Puede haber error
      if pic.MsjError<>'' then begin //Error en el mismo PIC
          GenError(pic.MsjError);
          exit;
      end;
    end;
  end;
  //Codifica las subrutinas usadas
  for elem in TreeElems.main.elements do if elem is TxpEleFun then begin
      fun := TxpEleFun(elem);
      if fun.nCalled>0 then begin
        //Compila la función en la dirección actual
        fun.adrr := pic.iFlash;    //Actualiza la dirección final
        cIn.PosAct := fun.posCtx;  //Posiciona escáner
        PutLabel('__'+fun.name);
        TreeElems.OpenElement(fun); //Ubica el espacio de nombres, de forma similar a la pre-compilación
        CompileProcBody(fun);
        TreeElems.CloseElement;
        if HayError then exit;     //Puede haber error
      end else begin
        //Esta función no se usa
        GenWarnPos('Unused procedure: %s', [fun.name], fun.srcDec);
      end;
  end;
  //Compila cuerpo del programa principal
  pic.codGotoAt(iniMain, _PC);   //termina de codificar el salto
  cIn.PosAct := TreeElems.main.posCtx;   //ubica escaner
  PutLabel('__main_program__');
  CompileCurBlock;
  PutLabel('__end_program__');
  {No es necesario hacer más validaciones, porque ya se hicieron en la primera pasada}
  _SLEEP();   //agrega instrucción final
end;
procedure TCompiler.Compile(NombArc: string; LinArc: Tstrings);
//Compila el contenido de un archivo.
begin
  mainFile := NombArc;
  //se pone en un "try" para capturar errores y para tener un punto salida de salida
  //único
  if ejecProg then begin
    GenError('There is a compilation in progress.');
    exit;  //sale directamente
  end;
  try
    ejecProg := true;  //marca bandera
    ClearError;
    //Genera instrucciones de inicio
    cIn.ClearAll;       //elimina todos los Contextos de entrada
    //compila el texto indicado
    cIn.NewContextFromFile(NombArc, LinArc);   //Crea nuevo contenido
    if HayError then exit;

    {Se hace una primera pasada para ver, a modo de exploración, para ver qué
    procedimientos, y varaibles son realmente usados, de modo que solo estos, serán
    codificados en la segunda pasada. Así evitamos incluir, código innecesario.}
consoleTickStart;
    debugln('*** Compiling: Pass 1.');
    pic.iFlash := 0;     //dirección de inicio del código principal
    FirstPass := true;
    CompileProgram;  //puede dar error
    if HayError then exit;
consoleTickCount('-->First Pass.');
    debugln('*** Compiling/Linking: Pass 2.');
    {Compila solo los procedimientos usados, leyendo la información del árbol de sintaxis,
    que debe haber sido actualizado en la primera pasada.}
    FirstPass := false;
    CompileLinkProgram;
    cIn.ClearAll;//es necesario por dejar limpio
    //Genera archivo hexa, en la misma ruta del programa
    pic.GenHex(ExtractFileDir(mainFile) + DirectorySeparator + 'output.hex');
consoleTickCount('-->Second Pass.');
  finally
    ejecProg := false;
    //tareas de finalización
    //como actualizar estado
  end;
end;
function AdrStr(absAdr: word): string;
{formatea una dirección en cadena.}
begin
  Result := '0x' + IntToHex(AbsAdr, 3);
end;
procedure TCompiler.RAMusage(lins: TStrings; varDecType: TVarDecType);
{Devuelve una cadena con información sobre el uso de la memoria.}
var
  adStr: String;
  v: TxpEleVar;
  nam: string;
  reg: TPicRegister;
  rbit: TPicRegisterBit;
begin
  for v in TreeElems.AllVars do begin
    case varDecType of
    dvtDBDb: begin
      adStr := v.AddrString;  //dirección hexadecimal
      if adStr='' then adStr := 'XXXX';  //Error en dirección
      if (v.typ = typBool) or (v.typ = typBit) then begin
        lins.Add(' ' + v.name + ' Db ' +  adStr);
      end else if v.typ = typByte then begin
        lins.Add(' ' + v.name + ' DB ' +  adStr);
      end else if v.typ = typWord then begin
        lins.Add(' ' + v.name + ' DW ' +  adStr);
      end else begin
        lins.Add(' "' + v.name + '"->' +  adStr);
      end;
    end;
    dvtEQU: begin;
      if (v.typ = typBool) or (v.typ = typBit) then begin
        lins.Add('#define ' + v.name + ' ' + AdrStr(v.AbsAddr) + ',' +
                                             IntToStr(v.adrBit.bit));
      end else if v.typ = typByte then begin
        if v.nCalled = 0 then begin
          lins.Add(v.name + ' EQU <unused>');
        end else begin
          lins.Add(v.name + ' EQU ' +  AdrStr(v.AbsAddr));
        end;
      end else if v.typ = typWord then begin
        lins.Add(v.name+'@0' + ' EQU ' +  AdrStr(v.AbsAddrL));
        lins.Add(v.name+'@1' + ' EQU ' +  AdrStr(v.AbsAddrH));
      end else begin
        lins.Add('"' + v.name + '"->' +  AdrStr(v.AbsAddr));
      end;
    end;
    end;
  end;
  //Reporte de registros de trabajo, auxiliares y de pila
  if (listRegAux.Count>0) or (listRegAuxBit.Count>0) then begin
    lins.Add(';------ Work and Aux. Registers ------');
    for reg in listRegAux do begin
      if not reg.assigned then continue;  //puede haber registros de trabajo no asignados
      nam := pic.NameRAM(reg.offs, reg.bank); //debería tener nombre
      adStr := '0x' + IntToHex(reg.AbsAdrr, 3);
      lins.Add(nam + ' EQU ' +  adStr);
    end;
    for rbit in listRegAuxBit do begin
      nam := pic.NameRAMbit(rbit.offs, rbit.bank, rbit.bit); //debería tener nombre
      adStr := '0x' + IntToHex(rbit.AbsAdrr, 3);
      lins.Add('#define' + nam + ' ' +  adStr + ',' + IntToStr(rbit.bit));
    end;
  end;
  if (listRegStk.Count>0) or (listRegStkBit.Count>0) then begin
    lins.Add(';------ Stack Registers ------');
    for reg in listRegStk do begin
      nam := pic.NameRAM(reg.offs, reg.bank); //debería tener nombre
      adStr := '0x' + IntToHex(reg.AbsAdrr, 3);
      lins.Add(nam + ' EQU ' +  adStr);
    end;
    for rbit in listRegStkBit do begin
      nam := pic.NameRAMbit(rbit.offs, rbit.bank, rbit.bit); //debería tener nombre
      adStr := '0x' + IntToHex(rbit.AbsAdrr, 3);
      lins.Add('#define ' + nam + ' ' +  adStr + ',' + IntToStr(rbit.bit));
    end;
  end;
//  lins.Add(';-------------------------');
end;
procedure TCompiler.DumpCode(lins: TSTrings; incAdrr, incCom: boolean);
begin
//  AsmList := TStringList.Create;  //crea lista para almacenar ensamblador
  pic.DumpCode(lins, incAdrr, incCom);
end;
function TCompiler.RAMusedStr: string;
var
  usedRAM, totRAM: Word;
begin
  totRAM := pic.TotalMemRAM;
  if totRAM=0 then exit;  //protección
  usedRAM := pic.UsedMemRAM;
  Result := 'RAM Used   = ' + IntToStr(usedRAM) +'/'+ IntToStr(totRAM) + 'B (' +
        FloatToStrF(100*usedRAM/totRAM, ffGeneral, 1, 3) + '%)';
end;
function TCompiler.FLASHusedStr: string;
var
  totROM: Integer;
  usedROM: Word;
begin
  totROM := pic.MaxFlash;
  usedROM := pic.UsedMemFlash;
  Result := 'Flash Used = ' + IntToStr(usedROM) +'/'+ IntToStr(totROM) + ' (' +
        FloatToStrF(100*usedROM/totROM, ffGeneral, 1, 3) + '%)';
end;
procedure TCompiler.GetResourcesUsed(out ramUse, romUse, stkUse: single);
var
  totROM, usedROM: Word;
  usedRAM, totRAM: Word;
begin
  //Calcula RAM
  ramUse := 0;  //calor por defecto
  totRAM := pic.TotalMemRAM;
  if totRAM = 0 then exit;  //protección
  usedRAM := pic.UsedMemRAM;
  ramUse := usedRAM/ totRAM;
  //Calcula ROM
  romUse:= 0;  //calor por defecto
  totROM := pic.MaxFlash;
  if totROM = 0 then exit; //protección
  usedROM := pic.UsedMemFlash;
  romUse := usedROM/totROM;
  //Calcula STACK
  stkUse := 0;  //calor por defecto
  { TODO : Por implementar }
end;
procedure TCompiler.DefLexDirectiv;
{Define la sintaxis del lexer que se usará para analizar las directivas. La que
 debe estar entre los símbolo {$ ... }
}
begin
  //solo se requiere identificadores y números
  lexDir.DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');
  lexDir.DefTokContent('[0-9]', '[0-9.]*', lexDir.tnNumber);
  lexDir.Rebuild;
end;
constructor TCompiler.Create;
begin
  inherited Create;
  lexDir := TSynFacilSyn.Create(nil);  //crea lexer para analzar directivas
  DefLexDirectiv;
  cIn.OnNewLine:=@cInNewLine;
  StartSyntax;   //Debe hacerse solo una vez al inicio
  DefCompiler;   //Debe hacerse solo una vez al inicio
  InitAsm(pic, self);   //inicia el procesamiento de ASM
end;
destructor TCompiler.Destroy;
begin
  lexDir.Destroy;
  inherited Destroy;
end;

procedure SetLanguage(lang: string);
begin
  case lang of
  'en': begin
    dicClear;  //it's yet in English
  end;
  'es': begin
    //Update messages
    dicSet('Not implemented.', 'No implementado');
    dicSet('Not implemented: "%s"', 'No implementado: "%s"');
    dicSet('Identifier expected.', 'Identificador esperado.');
    dicSet('Duplicated identifier: "%s"', 'Identificador duplicado: "%s"');
    dicSet('Unvalid float number.', 'Número decimal no válido.');
    dicSet('Error in number.', 'Error en número');
    dicSet('No type defined to accommodate this number.', 'No hay tipo definido para albergar a este número.');
    dicSet('Undefined type "%s"', 'Tipo "%s" no definido.');
    dicSet('"." expected.', 'Se esperaba "."');
    dicSet('Invalid memory address.', 'Dirección de memoria inválida.');
    dicSet('Invalid memory address for this device.', 'No existe esta dirección de memoria en este dispositivo.');
    dicSet('Identifier of variable expected.', 'Se esperaba identificador de variable.');
    dicSet('Unknown type.', 'Tipo desconocido');
    dicSet('Identifier of constant expected.', 'Se esperaba identificador de constante');
    dicSet('Numeric address expected.', 'Se esperaba dirección numérica.');
    dicSet('Identifier of type expected.', 'Se esperaba identificador de tipo.');
    dicSet('":" or "," expected.', 'Se esperaba ":" o ",".');
    dicSet('"=", ":" or "," expected.', 'Se esperaba "=", ":" o ",".');
    dicSet('"begin" expected.', 'Se esperaba "begin".');
    dicSet('"end" expected.', 'Se esperaba "end".');
    dicSet('"." expected.', 'Se esperaba "."');
    dicSet('"(" expected.', 'Se esperaba "("');
    dicSet('":" expected.', 'Se esperaba ":"');
    dicSet('";" expected.', 'Se esperaba ";"');
    dicSet('Boolean expression expected.', 'Se esperaba expresión booleana.');
    dicSet('"do" expected.', 'Se esperaba "do".');
    dicSet('"then" expected.', 'Se esperaba "then"');
    dicSet('"until" expected.', 'Se esperaba "until"');
    dicSet('Unknown structure.', 'Estructura desconocida.');
    dicSet('Name of program expected.', 'Se esperaba nombre de programa.');
    dicSet('Expected "begin", "var", "type" or "const".', 'Se esperaba "begin", "var", "type" o "const".');
    dicSet('Unexpected end of file. "end" expected.', 'Inesperado fin de archivo. Se esperaba "end".');
    dicSet('There is a compilation in progress.', 'Ya se está compilando un programa actualmente.');
    dicSet('Constant expression expected.', 'Se esperaba una expresión constante');
    dicSet('Clock frequency not supported.', 'Frecuencia de reloj no soportada.');
    dicSet('Error in directive.', 'Error en directiva');
    dicSet('Unknown directive: %s', 'Directiva desconocida: %s');
    dicSet('Cannot increase a constant.', 'No se puede incrementar una constante.');
    dicSet('Cannot increase an expression.','No se puede incrementar una expresión.');
    dicSet('Cannot decrease a constant.', 'No se puede disminuir una constante.');
    dicSet('Cannot decrease an expression.','No se puede disminuir una expresión.');
    dicSet('Unknown device: %s', 'Dispositivo desconocido: %s');
    dicSet('Syntax error. Nothing should be after "END."', 'Error de sintaxis. Nada debe aparecer después de "END."');
  end;
  end;
end;

initialization
  //Es necesario crear solo una instancia del compilador.
  cxp := TCompiler.Create;  //Crea una instancia del compilador

finalization
  cxp.Destroy;
end.

