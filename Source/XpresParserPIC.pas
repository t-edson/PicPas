{XpresParserPIC

Versión de XpresParser, orientada a trabajar con microcontroladores PIC.
La idea es tener aquí todas las rutinas que en lo posible sean independientes del
lenguaje y del modelo de PIC.

Para mayor información sobre el uso del framework Xpres, consultar la documentación
técnica.
}
//{$Define LogExpres}
unit XpresParserPIC;
interface
uses
  Classes, SysUtils, Forms, LCLType, lclProc, SynEditHighlighter,
  SynFacilHighlighter, XpresBas, XpresTypesPIC, XpresElementsPIC, MisUtils,
  Pic16Utils;
const
  TIT_BODY_ELE = 'Body';
type
//Tipo de expresión, de acuerdo a la posición en que aparece
TPosExpres = (pexINDEP,  //Expresión independiente
              pexASIG,   //Expresión de asignación
              pexPROC,   //Expresión de procedimiento
              pexSTRUC,  //Expresión de estructura
              pexPARAM,  //Expresión de parámetro de función
              pexPARSY   //Expresión de parámetro de función de sistema
              );
TOperType = (operUnary,  //Operación Unaria
             operBinary  //Operación Binaria
             );
{ TOperand }
//Operando
TOperand = object
public
  catOp: TCatOperan; //Categoría de operando
  typ  : TType;      //Referencia al tipo de dato
  txt  : string;     //Texto del operando o expresión, tal como aparece en la fuente
//  fun  : Tfunc;    //referencia a función en caso de que sea una función
  rVar : TxpEleVar;  //referencia a la variable, en caso de que sea variable
  Inverted: boolean; {Este campo se usa para cuando el operando es de tipo Bit o Boolean.
                      Indica que la lógica debe leerse de forma invertida.}
  {---------------------------------------------------------}
  function catOpStr: string;
  function catOpChr: char;
  procedure LoadToReg; inline;  //Pone el operador en registros de Trabajo
  procedure DefineRegister; inline;
  function FindOperator(const oper: string): TxpOperator; //devuelve el objeto operador
  //Funciones para facilitar el acceso a campos de la variable, cuando sea variable
  function VarName: string; inline; //nombre de la variable, cuando sea de categ. coVariab
  function offs: TVarOffs; //dirección de la variable
  function Loffs: TVarOffs; inline; //dirección del byte bajo
  function Hoffs: TVarOffs; inline; //dirección del byte alto
  function bank: TVarBank; inline;  //banco
  function Lbank: TVarBank; inline;  //banco
  function bit : byte; inline;  //posición del bit
public  //Manejo de la lógica
  procedure Invert;  //Invierte la lógica del operando
private
  Val  : TConsValue;
  procedure SetvalBool(AValue: boolean);
  procedure SetvalFloat(AValue: extended);
  procedure SetvalInt(AValue: Int64);
//  procedure SetvalStr(AValue: string);
public
  //Campos de acceso a los valores constantes
  property valInt  : Int64 read val.ValInt write SetvalInt;
  property valFloat: extended read val.ValFloat write SetvalFloat;
  property valBool : boolean read val.ValBool write SetvalBool;
//  property valStr  : string read val.ValStr write SetvalStr;
  //funciones de ayuda para adaptar los tipos numéricos
  function aWord: word; inline;  //devuelve el valor en Word
  function HByte: byte; inline;  //devuelve byte alto de valor entero
  function LByte: byte; inline;  //devuelve byte bajo de valor entero
  //campos para validar el rango de los valores
  function CanBeByte: boolean;   //indica si cae en el rango de un BYTE
  function CanBeWord: boolean;   //indica si cae en el rango de un WORD
  //métodos para mover valores desde/hacia una constante externa
  procedure CopyConsValTo(var c: TxpEleCon);
  procedure GetConsValFrom(const c: TxpEleCon);
end;

{ TCompilerBase }
{Clase base para crear al objeto compilador}
TCompilerBase = class
protected
  procedure IdentifyField(xOperand: TOperand);
  procedure LogExpLevel(txt: string);
protected  //Eventos del compilador
  OnExprStart: procedure of object;  {Se genera al iniciar la
                                      evaluación de una expresión.}
  OnExprEnd  : procedure(posExpres: TPosExpres) of object;  {Se genera al terminar de
                                                             evaluar una expresión.}
  pic        : TPIC16;   //Objeto PIC de la serie 16.
  ExprLevel  : Integer;  //Nivel de anidamiento de la rutina de evaluación de expresiones
  RTstate    : TType;    {Estado de los RT. Si es NIL, indica que los RT, no tienen
                         ningún dato cargado, sino indican el tipo cargado en los RT.}
  function CaptureDelExpres: boolean;
  procedure TipDefecNumber(var Op: TOperand; toknum: string); virtual; abstract;
  procedure TipDefecString(var Op: TOperand; tokcad: string); virtual; abstract;
  procedure TipDefecBoolean(var Op: TOperand; tokcad: string); virtual; abstract;
  function EOExpres: boolean;
  function EOBlock: boolean;
  procedure SkipWhites; virtual;  //rutina para saltar blancos
  //Manejo de tipos
  procedure ClearTypes;
  function CreateType(nom0: string; cat0: TCatType; siz0: smallint): TType;
  function FindType(TypName: string): TType;
  //Manejo de constantes
  function CreateCons(consName: string; typ: ttype): TxpEleCon;
  //Manejo de variables
  function CreateVar(varName: string; typ: ttype; absAddr, absBit: integer): TxpEleVar;
  //Manejo de funciones
  function CreateFunction(funName: string; typ: ttype; procParam,
    procCall: TProcExecFunction): TxpEleFun;
  function ValidateFunction: boolean;
  function CreateSysFunction(funName: string; procParam,
    procCall: TProcExecFunction): TxpEleFun;
  procedure CaptureParamsFinal(fun: TxpEleFun);
  function CaptureTok(tok: string): boolean;
  function CaptureStr(str: string): boolean;
  procedure CaptureParams;
  //Manejo del cuerpo del programa
  function CreateBody: TxpEleBody;
  //Manejo de Unidades
  function CreateUnit(uniName: string): TxpEleUnit;
  //Mmanejo de expresiones
  function GetOperand: TOperand; virtual;
  function GetOperandPrec(pre: integer): TOperand;
  function GetOperator(const Op: Toperand): Txpoperator;
  procedure GetExpressionE(const prec: Integer; posExpres: TPosExpres = pexINDEP);
public
  FirstPass  : boolean;   //Indica que está en la primera pasada.
  TreeElems: TXpTreeElements; //tablas de elementos del lenguaje
  listFunSys: TxpEleFuns;   //lista de funciones del sistema
protected
  typs   : TTypes;      //lista de tipos (El nombre "types" ya está reservado)
  function GetExpression(const prec: Integer): TOperand;
  //LLamadas a las rutinas de operación
  procedure Oper(var Op1: TOperand; opr: TxpOperator; var Op2: TOperand);
  procedure OperPre(var Op1: TOperand; opr: TxpOperator);
  procedure OperPost(var Op1: TOperand; opr: TxpOperator);
public  //Referencias a los tipos predefinidos de tokens.
  tnEol     : integer;
  tnSymbol  : integer;
  tnSpace   : integer;
  tnIdentif : integer;
  tnNumber  : integer;
  tnKeyword : integer;
  tnString  : integer;
  tnComment : integer;
  //Atributos
  tkEol     : TSynHighlighterAttributes;
  tkSymbol  : TSynHighlighterAttributes;
  tkSpace   : TSynHighlighterAttributes;
  tkIdentif : TSynHighlighterAttributes;
  tkNumber  : TSynHighlighterAttributes;
  tkKeyword : TSynHighlighterAttributes;
  tkString  : TSynHighlighterAttributes;
  tkComment : TSynHighlighterAttributes;
  //otras referencias
  tnOperator: integer;
  tnBoolean : integer;
  tnSysFunct: integer;
  tnType    : integer;
  //Atributos
  tkOperator: TSynHighlighterAttributes;
  tkBoolean : TSynHighlighterAttributes;
  tkSysFunct: TSynHighlighterAttributes;
  tkType    : TSynHighlighterAttributes;
public //Tipos adicionales de tokens
  tnStruct   : integer;
  tnDirective: integer;
  tnAsm      : integer;
  tnExpDelim : integer;
  tnBlkDelim : integer;
  tnChar     : integer;
  tnOthers   : integer;
public
  xLex   : TSynFacilSyn; //resaltador - lexer
  cIn    : TContexts;   //entrada de datos
  //variables públicas del compilador
  ejecProg: boolean;    //Indica que se está ejecutando un programa o compilando
  DetEjec: boolean;     //para detener la ejecución (en intérpretes)

  func0  : TxpEleFun;   //función interna para almacenar parámetros
  p1, p2 : ^TOperand;   //Pasa los operandos de la operación actual
  res    : TOperand;    //resultado de la evaluación de la última expresión.
  catOperation: TCatOperation;  //combinación de categorías de los operandos
  operType : TOperType; //Tipo de oepración
  function CatOperationToStr(Op: string=','): string;
public  //Manejo de errores y advertencias
  HayError: boolean;
  OnWarning: procedure(warTxt: string; fileName: string; row, col: integer) of object;
  OnError  : procedure(errTxt: string; fileName: string; row, col: integer) of object;
  procedure ClearError;
  procedure GenWarn(msg: string; fil: String; row, col: integer);
  procedure GenWarn(msg: string; const Args: array of const; fil: String; row, col: integer);
  procedure GenWarn(msg: string);
  procedure GenWarn(msg: string; const Args: array of const);
  procedure GenWarnPos(msg: string; const Args: array of const; srcPos: TSrcPos);
  //Rutinas de generación de error
  procedure GenError(msg: string; fil: String; row, col: integer);
  procedure GenError(msg: String; const Args: array of const; fil: String; row, col: integer);
  procedure GenError(msg: string);
  procedure GenError(msg: String; const Args: array of const);
  procedure GenErrorPos(msg: String; const Args: array of const; srcPos: TSrcPos);
public  //Inicialización
  constructor Create; virtual;
  destructor Destroy; override;
end;

implementation
uses Graphics;

{TCompilerBase}
function TCompilerBase.CatOperationToStr(Op: string=','): string;
{Devuelve una cadena descriptiva de la variable global "catOperation"}
begin
  case catOperation of
  coConst_Const  : exit('Constant'+ Op +'Constant');
  coConst_Variab : exit('Constant'+ Op +'Variable');
  coConst_Expres : exit('Constant'+ Op +'Expression');
  coVariab_Const : exit('Variable'+ Op +'Constant');
  coVariab_Variab: exit('Variable'+ Op +'Variable');
  coVariab_Expres: exit('Variable'+ Op +'Expression');
  coExpres_Const : exit('Expression'+ Op +'Constant');
  coExpres_Variab: exit('Expression'+ Op +'Variable');
  coExpres_Expres: exit('Expression'+ Op +'Expression');
  end;
end;
function TCompilerBase.EOExpres: boolean; inline;
//Indica si se ha llegado al final de una expresión.
begin
  Result := cIn.tok = ';';  //en este caso de ejemplo, usamos punto y coma
  {En la práctica, puede ser conveniente definir un tipo de token como "tkExpDelim", para
   mejorar el tiempo de respuesta del procesamiento, de modo que la condición sería:
     Result := cIn.tokType = tkExpDelim;
  }
end;
function TCompilerBase.EOBlock: boolean; inline;
//Indica si se ha llegado el final de un bloque
begin
  Result := cIn.tokType = tnBlkDelim;
  {No está implementado aquí, pero en la práctica puede ser conveniente definir un tipo de token
   como "tnBlkDelim", para mejorar el tiempo de respuesta del procesamiento, de modo que la
   condición sería:
  Result := cIn.tokType = tnBlkDelim;}
end;
procedure TCompilerBase.SkipWhites;
{Se crea como rutina aparte, para facilitar el poder cambiar el comportamiento y
adaptarlo al modo de trabajo del lenguaje.}
begin
  cIn.SkipWhites;
end;
function TCompilerBase.CaptureDelExpres: boolean;
//Verifica si sigue un delimitador de expresión. Si encuentra devuelve false.
begin
  SkipWhites;
  if EOExpres then begin //encontró
    cIn.Next;   //pasa al siguiente
    exit(true);
  end else begin   //es un error
    GenError('";" expected.');
    exit(false);  //sale con error
  end;

end;
//Manejo de tipos
procedure TCompilerBase.ClearTypes;  //Limpia los tipos
begin
  typs.Clear;
end;
function TCompilerBase.CreateType(nom0: string; cat0: TCatType; siz0: smallint): TType;
{Crea una nueva definición de tipo en el compilador. Devuelve referencia al tipo recien
creado. El nombre del tipo, debe darse en minúscula, porque así se hará la búsqueda. con
FindType().}
var
  r: TType;
begin
  //verifica nombre
  if FindType(nom0) <> nil then begin
    GenError('Duplicated identifier: "%s"', [nom0]);
    exit;
  end;
  //configura nuevo tipo
  r := TType.Create;
  r.name:=nom0;
  r.cat:=cat0;
  r.size:=siz0;
  //agrega
  typs.Add(r);
  Result:=r;   //devuelve índice al tipo
end;
function TCompilerBase.FindType(TypName: string): TType;
{Busca un tipo, por su nombre. Si no encuentra, devuelve NIL.}
var
  t: TType;
begin
  TypName := LowerCase(TypName);
  for t in typs do begin
    if t.name = TypName then exit(t);
  end;
  exit(nil)
end;
function TCompilerBase.CreateCons(consName: string; typ: ttype): TxpEleCon;
{Rutina para crear una constante. Devuelve referencia a la constante creada.}
var
  conx  : TxpEleCon;
begin
  //registra variable en la tabla
  conx := TxpEleCon.Create;
  conx.name:=consName;
  conx.typ := typ;   //fija  referencia a tipo
  Result := conx;
end;
//Manejo de variables
function TCompilerBase.CreateVar(varName: string; typ: ttype; absAddr,
  absBit: integer): TxpEleVar;
{Rutina para crear una variable. Devuelve referencia a la variable creada.}
var
  xVar: TxpEleVar;
begin
  xVar := TxpEleVar.Create;
  xVar.name := varName;
  xVar.typ := typ;
  xVar.solAdr := absAddr;   //Si no es ABSOLUTE, valdrá -1
  xVar.solBit := absBit;
  Result := xVAr;
end;
//Manejo de funciones
function TCompilerBase.CreateFunction(funName: string; typ: ttype;
  procParam, procCall: TProcExecFunction): TxpEleFun;
{Crea una nueva función y devuelve la referecnia a la función.}
var
  fun : TxpEleFun;
begin
  fun := TxpEleFun.Create;
  fun.name:= funName;
  fun.typ := typ;
  fun.procParam := procParam;
  fun.procCall:= procCall;
  fun.ClearParams;
  Result := fun;
end;
function TCompilerBase.ValidateFunction: boolean;
{Valida la última función introducida, verificando que no haya otra función con el
 mismo nombre y mismos parámetros. De ser así devuelve FALSE.
 Se debe llamar después de haber leido los parámetros de la función. }
begin
  if not TreeElems.ValidateCurElement then begin
    GenError('Duplicated function: %s',[TreeElems.CurNodeName]);
    exit(false);
  end;
  exit(true);  //validación sin error
end;
function TCompilerBase.CreateSysFunction(funName: string;
  procParam, procCall: TProcExecFunction): TxpEleFun;
{Crea una función del sistema. A diferencia de las funciones definidas por el usuario,
una función del sistema se crea, sin crear espacios de nombre. La idea es poder
crearlas rápidamente. "procParam", solo es necesario, cuando la función del sistema
debe devolver valores (No es procedimiento).}
var
  fun : TxpEleFun;
begin
  fun := TxpEleFun.Create;  //Se crea como una función normal
  fun.name:= funName;
  fun.typ := typNull;
  fun.procParam := procParam;
  fun.procCall:= procCall;
  fun.ClearParams;
//  TreeElems.AddElement(fun, false);  //no verifica duplicidad
  listFunSys.Add(fun);  //Las funciones de sistema son accesibles siempre
  Result := fun;
end;
function TCompilerBase.CaptureTok(tok: string): boolean;
{Toma el token indicado del contexto de entrada. Si no lo encuentra, genera error y
devuelve FALSE.}
  procedure GenErrorInLastLine(var p: TSrcPos);
  {Genera error posicionando el punto del error, en una línea anterios, que no esté
  vacía.}
  var
    lin: String;
  begin
    if p.row>1 then begin
      //Hay línea anterior
      repeat
        p.row := p.row - 1;
        lin := cIn.curCon.curLines[p.row - 1];
      until (p.row<=1) or (trim(lin)<>'');
      //Encontró línea anterior no nula o llegó a la primera línea.
      p.col := length(lin) + 1;   //mueve al final
      GenErrorPos('"%s" expected.', [tok], p);  //Genera error
    end else begin
      //No hay línea anterior
      p.col := 1;   //mueve al inicio
      GenErrorPos('"%s" expected.', [tok], p);  //Genera error
    end;
  end;

var
  x: integer;
  lin: String;
  p: TSrcPos;
begin
  //Debe haber parámetros
  if cIn.tok<>tok then begin
    //No se encontró el token. Muestra mensaje de error.
    {Pero el error, debe estar antes, así que hacemos la magia de explorar hacia atrás,
    hasta encontrar el token involucrado.}
    p := cIn.ReadSrcPos;   //posición actual
    x := p.col;   //lee posición actual
    if x>1 then begin
      //Hay algo antes del token
      lin := cIn.curCon.CurLine;
      repeat
        dec(x);
      until (x<=1) or (lin[x] <> ' ');
      if x<=1 then begin
        //Está lleno de espacios, hasta el inicio.
        //Es muy probable que el error esté en la línea anterior.
        GenErrorInLastLine(p);
      end else begin
        //Encontró, en la misma línea un caracter diferente de espacio
        GenErrorPos('"%s" expected.', [tok], p);  //Genera error ahí mismo
      end;
    end else begin
      //Está al inicio de la línea. El error debe estar antes
      GenErrorInLastLine(p);
    end;
    exit(false);
  end;
  cin.Next;
  exit(true);
end;
function TCompilerBase.CaptureStr(str: string): boolean;
//Similar a CaptureTok(), pero para cadenas. Se debe dar el texto en minúscula.
begin
  //Debe haber parámetros
  if cIn.tokL<>str then begin
    GenError('"%s" expected.', [str]);
    exit(false);
  end;
  cin.Next;
  exit(true);
end;
procedure TCompilerBase.CaptureParams;
//Lee los parámetros de una función en la función interna funcs[0]
begin
  func0.ClearParams;
  if EOBlock or EOExpres then begin
    //no tiene parámetros
  end else begin
    //Debe haber parámetros
    if cIn.tok <> '(' then begin
      //Si no sigue '(', significa que no hay parámetros.
      exit;
    end;
    cIn.Next;  //Toma paréntesis
    repeat
      GetExpressionE(0, pexPARAM);  //captura parámetro
      if HayError then exit;   //aborta
      //guarda tipo de parámetro
      func0.CreateParam('',res.typ, nil);
      if cIn.tok = ',' then begin
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
    if not CaptureTok(')') then exit;
  end;
end;
procedure TCompilerBase.CaptureParamsFinal(fun: TxpEleFun);
{Captura los parámetros asignándolos a las variables de la función que representan a los
parámetros. No hace falta verificar, no debería dar error, porque ya se verificó con
CaptureParams. }
var
  i: Integer;
  par: TxpParFunc;
  Op1: TOperand;
  op: TxpOperator;
begin
  if EOBlock or EOExpres then exit;  //sin parámetros
  CaptureTok('(');   //No debe dar error porque ya se verificó
  for i := 0 to high(fun.pars) do begin
    par := fun.pars[i];
    {Ya sirvió "RTstate", ahora lo limpiamos, no vaya a pasar que las rutinas de
    asignación, piensen que los RT están ocupados, cuando la verdad es que han sido
    liberados, precisamente para ellas.}
    RTstate := nil;
    //Evalúa parámetro
    Inc(ExprLevel);    //cuenta el anidamiento
    res := GetExpression(0);  //llama como sub-expresión
    Dec(ExprLevel);
    if HayError then exit;   //aborta
    if cIn.tok = ',' then begin
      cIn.Next;
      SkipWhites;
    end;
    //Genera código para la asignación
    if par.pvar.IsRegister then begin
      {Cuando es parámetro registro, no se asigna, se deja en el registro(s) de
       trabajo.}
      res.LoadToReg;
    end else begin
      Op1.catOp := coVariab;  //configura el operando como variable
      Op1.rVar  := par.pvar;
      if FirstPass then par.pvar.AddCaller;   //se está usando
      Op1.typ   := par.pvar.typ;  //necesario para "FindOperator"
      op := Op1.FindOperator(':=');  //busca la operación
      Oper(Op1, op, res);   //Codifica la asignación
    end;
  end;
  if not CaptureTok(')') then exit;
end;
function TCompilerBase.CreateBody: TxpEleBody;
var
  body: TxpEleBody;
begin
  body := TxpEleBody.Create;
  body.name := TIT_BODY_ELE;
  Result := body;
end;
function TCompilerBase.CreateUnit(uniName: string): TxpEleUnit;
var
  uni: TxpEleUnit;
begin
  uni := TxpEleUnit.Create;
  uni.name := uniName;
  Result := uni;
end;
//Manejo de expresiones
procedure TCompilerBase.IdentifyField(xOperand: TOperand);
{Identifica el campo de una variable. Si encuentra algún problema genera error.
Notar que el parámetro es por valor, es decir, se crea una copia, por seguridad.
Puede generar código de evaluación. Devuelve el resultado en "res". }
var
  field: TTypField;
  identif: String;
begin
  cIn.Next;    //TOma el "."
  if (cIn.tokType<>tnIdentif) and (cIn.tokType<>tnNumber) then begin
    GenError('Identifier expected.');
    exit;
  end;
  //Hay un identificador
  identif :=  cIn.tokL;
  //Prueba con campos del tipo
  for field in xOperand.typ.fields do begin
    if LowerCase(field.Name) = identif then begin
      //Encontró el campo
      field.proc(@xOperand);  //Devuelve resultado en "res"
      cIn.Next;    //Coge identificador
      if cIn.tok = '.' then begin
        //Aún hay más campos, seguimos procesando
        //Como "IdentifyField", crea una copia del parámetro, no hay cruce con el resultado
        IdentifyField(res);
      end;
      exit;
    end;
  end;
  //No encontró
  GenError('Unknown identifier: %s', [identif]);
end;
function TCompilerBase.GetOperand: TOperand;
{Parte de la funcion analizadora de expresiones que genera codigo para leer un operando.
Debe devolver el tipo del operando y también el valor. En algunos casos, puede modificar
"res".}
var
  xcon: TxpEleCon;
  xvar: TxpEleVar;
  xfun: TxpEleFun;
  tmp, oprTxt: String;
  ele: TxpElement;
  Op: TOperand;
  posAct, posPar: TPosCont;
  opr: TxpOperator;
  Found: Boolean;
  posFlash: Integer;
  cod: Longint;
  RTstate0: TType;
begin
  ClearError;
  SkipWhites;
  Result.Inverted := false;   //inicia campo
  if cIn.tokType = tnNumber then begin  //constantes numéricas
    Result.catOp:=coConst;       //constante es Mono Operando
    {$IFDEF LogExpres} Result.txt:= cIn.tok; {$ENDIF}   //toma el texto
    TipDefecNumber(Result, cIn.tok); //encuentra tipo de número, tamaño y valor
    if HayError then exit;  //verifica
    cIn.Next;    //Pasa al siguiente
  end else if cIn.tokType = tnChar then begin  //constante caracter
    Result.catOp:=coConst;       //constante es Mono Operando
    {$IFDEF LogExpres} Result.txt:= cIn.tok; {$ENDIF}   //toma el texto
    if not TryStrToInt(copy(cIn.tok, 2), cod) then begin
      GenError('Error in character.');   //tal vez, sea muy grande
      exit;
    end;
    if (cod<0) or (cod>255) then begin
      GenError('Invalid code for char.');
      exit;
    end;
    Result.valInt := cod;
    Result.typ := typChar;
    cIn.Next;    //Pasa al siguiente
  end else if cIn.tokType = tnString then begin  //constante cadena
    Result.catOp:=coConst;       //constante es Mono Operando
    if length(cIn.tok)=2 then begin  //Es ''
      GenError('Char expected.');
      exit;
    end else if length(cIn.tok)>3 then begin  //Es 'aaaa...'
      GenError('Too long string for a Char.');
      exit;
    end;
    {$IFDEF LogExpres} Result.txt:= cIn.tok; {$ENDIF}   //toma el texto
    Result.valInt := ord(cIn.tok[2]);
    Result.typ := typChar;
    cIn.Next;    //Pasa al siguiente
  end else if (cIn.tokType = tnSysFunct) or //función del sistema
              (cIn.tokL = 'bit') or   //"bit" es de tipo "tnType"
              (cIn.tokL = 'word') then begin  //"word" es de tipo "tnType"
    {Se sabe que es función, pero no se tiene la función exacta porque puede haber
     versiones, sobrecargadas de la misma función.}
    tmp := UpCase(cIn.tok);  //guarda nombre de función
    cIn.Next;    //Toma identificador
    //Busca la función
    for xfun in listFunSys do begin
      if (Upcase(xfun.name) = tmp) then begin
        {Encontró. Llama a la función de procesamiento, quien se encargará de
        extraer los parámetros y analizar la sintaxis.}
        if FirstPass and (xfun.compile<>nil) then begin
          {LLeva la cuenta de llamadas, solo cuando hay subrutinas. Para funciones
           INLINE, no vale la pena, gastar recursos.}
          xfun.AddCaller;
        end;
        xfun.procCall(xfun);  //Para que devuelva el tipo y codifique el _CALL o lo implemente
        //Puede devolver typNull, si no es una función.
        Result := res;  //copia tipo y categoría y otros campso relevantes
        {$IFDEF LogExpres} Result.txt:= tmp; {$ENDIF}    //toma el texto
        exit;
      end;
    end;
    GenError('Not implemented.');
  end else if cIn.tokType = tnIdentif then begin  //puede ser variable, constante, función
    ele := TreeElems.FindFirst(cIn.tok);  //identifica elemento
    if ele = nil then begin
      //No identifica a este elemento
      GenError('Unknown identifier: %s', [cIn.tok]);
      exit;
    end;
    if ele.elemType = eltVar then begin
      //es una variable
      xvar := TxpEleVar(ele);
      if FirstPass then xvar.AddCaller;   //lleva la cuenta
      cIn.Next;    //Pasa al siguiente
      if xvar.IsRegister then begin
        //Es una variables REGISTER
        Result.catOp:=coExpres;
        Result.typ:=xvar.typ;
        //Faltaría asegurarse de que los registros estén disponibles
        Result.DefineRegister;
      end else begin
        //Es una variable común
        Result.catOp:=coVariab;    //variable
        Result.typ:=xvar.typ;
        Result.rVar:=xvar;   //guarda referencia a la variable
        {$IFDEF LogExpres} Result.txt:= xvar.name; {$ENDIF}   //toma el texto
        //Verifica si tiene referencia a campos con "."
        if cIn.tok = '.' then begin
          IdentifyField(Result);
          Result := res;  //notar que se usa "res".
          if HayError then exit;;
        end;
      end;
    end else if ele.elemType = eltCons then begin  //es constante
      //es una constante
      xcon := TxpEleCon(ele);
      if FirstPass then xcon.AddCaller;//lleva la cuenta
      cIn.Next;    //Pasa al siguiente
      Result.catOp:=coConst;    //constante
      Result.typ:=xcon.typ;
      Result.GetConsValFrom(xcon);  //lee valor
      {$IFDEF LogExpres} Result.txt:= xcon.name; {$ENDIF}   //toma el texto
      //Verifica si tiene referencia a campos con "."
      if cIn.tok = '.' then begin
        IdentifyField(Result);
        Result := res;  //notar que se usa "res".
        if HayError then exit;;
      end;
    end else if ele.elemType = eltFunc then begin  //es función
      {Se sabe que es función, pero no se tiene la función exacta porque puede haber
       versiones, sobrecargadas de la misma función.}
      cIn.Next;    //Toma identificador
      SkipWhites;  //Quita posibles blancos
      posPar := cIn.PosAct;   //guarda porque va a pasar otra vez por aquí
      posFlash := pic.iFlash; //guarda posición, antes del código de evaluación.
      RTstate0 := RTstate;    //guarda porque se va a alterar con CaptureParams().
      CaptureParams;  //primero lee parámetros
      if HayError then exit;
      //Aquí se identifica la función exacta, que coincida con sus parámetros
      xfun := TxpEleFun(ele);
      //Primero vemos si la primera función encontrada, coincide:
      if func0.SameParams(xfun) then begin
        //Coincide
        Found := true;
      end else begin
        //No es, es una pena. Ahora tenemos que seguir buscando en el árbol de sintaxis.
        repeat
          //Usar FindNextFunc, es la forma es eficiente, porque retoma la búsqueda anterior.
          xfun := TreeElems.FindNextFunc;
        until (xfun = nil) or func0.SameParams(xfun);
        Found := (xfun <> nil);
      end;
      if Found then begin
        //Ya se identificó a la función que cuadra con los parámetros
        {$IFDEF LogExpres} Result.txt:= cIn.tok; {$ENDIF}   //toma el texto
        {Ahora que ya sabe cúal es la función referenciada, captura de nuevo los
        parámetros, pero asignándola al parámetro que corresponde.}
        cIn.PosAct := posPar;
        pic.iFlash := posFlash;
        RTstate := RTstate0;
        xfun.procParam(xfun);  //antes de leer los parámetros
        if high(func0.pars)+1>0 then
          CaptureParamsFinal(xfun);  //evalúa y asigna
//if RTstate = nil then debugln('RTstate=NIL') else debugln('RTstate='+RTstate.name);
        if FirstPass then xfun.AddCaller;  //se hace después de leer parámetros
        xfun.procCall(xfun); //codifica el "CALL"
        RTstate := xfun.typ;  //para indicar que los RT están ocupados
        Result.catOp := coExpres;
        Result.typ := xfun.typ;
        exit;
      end else begin
        //Encontró la función, pero no coincidió con los parámetros
        GenError('Type parameters error on %s', [ele.name + '()']);
        exit;
      end;
    end else begin
      GenError('Not implemented.');
      exit;
    end;
  end else if cIn.tokType = tnBoolean then begin  //true o false
    Result.catOp:=coConst;       //constante es Mono Operando
    {$IFDEF LogExpres} Result.txt:= cIn.tok; {$ENDIF}   //toma el texto
    TipDefecBoolean(Result, cIn.tok); //encuentra tipo y valor
    if HayError then exit;  //verifica
    cIn.Next;    //Pasa al siguiente
  end else if cIn.tok = '(' then begin  //"("
    cIn.Next;
    Inc(ExprLevel);  //cuenta el anidamiento
    Result := GetExpression(0);
    Dec(ExprLevel);
    if HayError then exit;
    If cIn.tok = ')' Then begin
       cIn.Next;  //lo toma
    end Else begin
       GenError('Error in expression. ")" expected.');
       Exit;       //error
    end;
{  end else if (cIn.tokType = tkString) then begin  //constante cadena
    Result.catOp:=coConst;     //constante es Mono Operando
    TipDefecString(Result, cIn.tok); //encuentra tipo de número, tamaño y valor
    if pErr.HayError then exit;  //verifica
    if Result.typ = nil then begin
       GenError('No hay tipo definido para albergar a esta constante cadena');
       exit;
     end;
    cIn.Next;    //Pasa al siguiente
}
  end else if cIn.tokType = tnOperator then begin
    {Si sigue un operador puede ser un operador Unario.
    El problema que tenemos, es que no sabemos de antemano el tipo, para saber si el
    operador aplica a ese tipo como operador Unario Pre. Así que asumiremos que es así,
    sino retrocedemos.}
    posAct := cIn.PosAct;   //Esto puede ser pesado en términos de CPU
    oprTxt := cIn.tok;   //guarda el operador
    cIn.Next; //pasa al siguiente
    Op := GetOperand();   //toma el operando. ¡¡¡Importante los peréntesis!!!
    if HayError then exit;
    //Ahora ya tenemos el tipo. Hay que ver si corresponde el operador
    opr := Op.typ.FindUnaryPreOperator(oprTxt);
    if opr = nullOper then begin
      {Este tipo no permite este operador Unario (a lo mejor ni es unario)}
      cIn.PosAct := posAct;
      GenError('Cannot apply the operator "%s" to type "%s"', [oprTxt, Op.typ.name]);
      exit;
    end;
    //Sí corresponde. Así que apliquémoslo
    OperPre(Op, opr);
    Result := res;
  end else begin
    //No se reconoce el operador
    GenError('Operand expected.');
  end;
end;
procedure TCompilerBase.LogExpLevel(txt: string);
{Genera una cadena de registro , considerando el valor de "ExprLevel"}
begin
  debugln(space(3*ExprLevel)+ txt );
end;
procedure TCompilerBase.Oper(var Op1: TOperand; opr: TxpOperator; var Op2: TOperand);
{Ejecuta una operación con dos operandos y un operador. "opr" es el operador de Op1.
El resultado debe devolverse en "res". En el caso de intérpretes, importa el
resultado de la Operación.
En el caso de compiladores, lo más importante es el tipo del resultado, pero puede
usarse también "res" para cálculo de expresiones constantes.
}
var
  Operation: TxpOperation;
  tmp: String;
begin
   {$IFDEF LogExpres}
   LogExpLevel('-- Op1='+Op1.txt+', Op2='+Op2.txt+' --');
   {$ENDIF}
   ClearError;
   //Busca si hay una operación definida para: <tipo de Op1>-opr-<tipo de Op2>
   Operation := opr.FindOperation(Op2.typ);
   if Operation = nil then begin
      tmp := '(' + Op1.typ.name + ') '+ opr.txt;
      tmp := tmp +  ' ('+Op2.typ.name+')';
      GenError('Illegal Operation: %s',
               [tmp]);
      exit;
    end;
   {Llama al evento asociado con p1 y p2 como operandos. }
   p1 := @Op1; p2 := @Op2;  { Se usan punteros por velocidad. De otra forma habría que
                             copiar todo el objeto.}
   //junta categorías de operandos
   catOperation := TCatOperation((Ord(Op1.catOp) << 2) or ord(Op2.catOp));
   operType := operBinary;
   {Ejecuta la operación.
   Los parámetros de entrada se dejan en p1 y p2. El resultado debe dejarse en "res"}
   Operation.proc;
   //Completa campos de "res", si es necesario
   {$IFDEF LogExpres}
   LogExpLevel('Oper('+Op1.catOpChr + ' ' + opr.txt + ' ' + Op2.catOpChr+') -> ' +
                res.catOpChr);
   res.txt := Op1.txt + ' ' + opr.txt + ' ' + Op2.txt;   //texto de la expresión
   {$ENDIF}
//   res.uop := opr;   //última operación ejecutada
End;
procedure TCompilerBase.OperPre(var Op1: TOperand; opr: TxpOperator);
{Ejecuta una operación con un operando y un operador unario de tipo Pre. "opr" es el
operador de Op1.
El resultado debe devolverse en "res".}
begin
  {$IFDEF LogExpres}
  LogExpLevel('-- Op1='+Op1.txt+' --');
  {$ENDIF}
   ClearError;
   if opr.OperationPre = nil then begin
      GenError('Illegal Operation: %s',
                 [opr.txt + '('+Op1.typ.name+')']);
      exit;
    end;
   {Llama al evento asociado con p1 como operando. }
   p1 := @Op1; {Solo hay un parámetro}
   operType := operUnary;
   {Ejecuta la operación. El resultado debe dejarse en "res"}
   opr.OperationPre;
   //Completa campos de "res", si es necesario
   {$IFDEF LogExpres}
   LogExpLevel('Oper('+ opr.txt + ' ' + Op1.catOpChr+ ') -> ' + res.catOpChr);
   res.txt := opr.txt + Op1.txt;
   {$ENDIF}
end;
procedure TCompilerBase.OperPost(var Op1: TOperand; opr: TxpOperator);
{Ejecuta una operación con un operando y un operador unario de tipo Post. "opr" es el
operador de Op1.
El resultado debe devolverse en "res".}
begin
  {$IFDEF LogExpres}
  LogExpLevel('-- Op1='+Op1.txt+' --');
  {$ENDIF}
   ClearError;
   if opr.OperationPost = nil then begin
      GenError('Illegal Operation: %s',
                 ['('+Op1.typ.name+')' + opr.txt]);
      exit;
    end;
   {Llama al evento asociado con p1 como operando. }
   p1 := @Op1; {Solo hay un parámetro}
   operType := operUnary;
   {Ejecuta la operación. El resultado debe dejarse en "res"}
   opr.OperationPost;
   //Completa campos de "res", si es necesario
   {$IFDEF LogExpres}
   LogExpLevel('Oper('+Op1.catOpChr+ ' ' +opr.txt +') -> ' + res.catOpChr);
   res.txt := Op1.txt + opr.txt;   //indica que es expresión
   {$ENDIF}
end;
//procedure TCompilerBase.Oper(var Op1: TOperand);
//{Ejecuta una operación con un solo operando, que puede ser una constante, una variable
//o la llamada a una función.
//El resultado debe devolverse en "res".
//La implementación debe decidir, qué hacer cuando se encuentra un solo operando como
//expresión. En algunos casos puede ser inválido.
//}
//begin
//  {$IFDEF LogExpres}
//  LogExpLevel('Eval('+Op1.catOpChr+')');
//  {$ENDIF}
//  ClearError;
//  {Llama al evento asociado con p1 como operando. }
//  p1 := @Op1; {Solo hay un parámetro}
//  {Ejecuta la operación. El resultado debe dejarse en "res"}
//  if Op1.typ.OperationLoad <> nil then Op1.typ.OperationLoad();
//  //Completa campos de "res", si es necesario
////   res.txt := Op1.catOpChr + opr.txt + Op2.txt;   //texto de la expresión
////   res.uop := opr;   //última operación ejecutada
//end;
function TCompilerBase.GetOperandPrec(pre: integer): TOperand;
//Toma un operando realizando hasta encontrar un operador de precedencia igual o menor
//a la indicada
var
  Op1: TOperand;
  Op2: TOperand;
  opr: TxpOperator;
  pos: TPosCont;
begin
  {$IFDEF LogExpres}
//  LogExpLevel('GetOperandP('+IntToStr(pre)+')');
  {$ENDIF}
  Op1 :=  GetOperand;  //toma el operador
  if HayError then exit;
  //verifica si termina la expresion
  pos := cIn.PosAct;    //Guarda por si lo necesita
  SkipWhites;
  opr := GetOperator(Op1);
  if opr = nil then begin  //no sigue operador
    Result:=Op1;
  end else if opr=nullOper then begin  //hay operador pero, ..
    //no está definido el operador siguente para el Op1, (no se puede comparar las
    //precedencias) asumimos que aquí termina el operando.
    cIn.PosAct := pos;   //antes de coger el operador
    GenError('Undefined operator: %s for type: %s', [opr.txt, Op1.typ.name]);
    exit;
//    Result:=Op1;
  end else begin  //si está definido el operador (opr) para Op1, vemos precedencias
    If opr.prec > pre Then begin  //¿Delimitado por precedencia de operador?
      //es de mayor precedencia, se debe Oper antes.
      Op2 := GetOperandPrec(pre);  //toma el siguiente operando (puede ser recursivo)
      if HayError then exit;
      Oper(Op1, opr, Op2);  //devuelve en "res"
      Result:=res;
    End else begin  //la precedencia es menor o igual, debe salir
      cIn.PosAct := pos;   //antes de coger el operador
      Result:=Op1;
    end;
  end;
end;
function TCompilerBase.GetOperator(const Op: Toperand): Txpoperator;
{Busca la referecnia a un operador de "Op", leyendo del contexto de entrada
Si no encuentra un operador en el contexto, devuelve NIL, pero no lo toma.
Si el operador encontrado no se aplica al operando, devuelve nullOper.}
begin
  if cIn.tokType <> tnOperator then exit(nil);
  //Hay un operador
  Result := Op.typ.FindBinaryOperator(cIn.tok);
  cIn.Next;   //toma el token
end;
function TCompilerBase.GetExpression(const prec: Integer): TOperand; //inline;
{Analizador de expresiones. Esta es probablemente la función más importante del
 compilador. Procesa una expresión en el contexto de entrada llama a los eventos
 configurados para que la expresión se evalúe (intérpretes) o se compile (compiladores).
 Devuelve un operando con información sobre el resultado de la expresión.}
var
  Op1, Op2  : TOperand;   //Operandos
  opr1: TxpOperator;  //Operadores
begin
  ClearError;
  //----------------coger primer operando------------------
  Op1 := GetOperand;
  if HayError then exit;
  //verifica si termina la expresion
  SkipWhites;
  opr1 := GetOperator(Op1);
  if opr1 = nil then begin  //no sigue operador
    //Expresión de un solo operando. Lo carga por si se necesita
    //Oper(Op1);
    Result:=Op1;
    exit;  //termina ejecucion
  end;
  //------- sigue un operador ---------
  //verifica si el operador aplica al operando
  if opr1 = nullOper then begin
    GenError('Undefined operator: %s for type: %s', [opr1.txt, Op1.typ.name]);
    exit;
  end;
  //inicia secuencia de lectura: <Operador> <Operando>
  while opr1<>nil do begin
    //¿Delimitada por precedencia?
    If opr1.prec<= prec Then begin  //es menor que la que sigue, expres.
      Result := Op1;  //solo devuelve el único operando que leyó
      exit;
    End;
    if opr1.OperationPost<>nil then begin  //Verifica si es operación Unaria
      OperPost(Op1, opr1);
      if HayError then exit;
      Op1 := res;
      SkipWhites;
      opr1 := GetOperator(Op1);
      continue;
    end;
    //--------------------coger segundo operando--------------------
    Op2 := GetOperandPrec(Opr1.prec);   //toma operando con precedencia
    if HayError then exit;
    //prepara siguiente operación
    Oper(Op1, opr1, Op2);    //evalua resultado en "res"
    Op1 := res;
    if HayError then exit;
    SkipWhites;
    opr1 := GetOperator(Op1);   {lo toma ahora con el tipo de la evaluación Op1 (opr1) Op2
                                porque puede que Op1 (opr1) Op2, haya cambiado de tipo}
  end;  //hasta que ya no siga un operador
  Result := Op1;  //aquí debe haber quedado el resultado
end;
procedure TCompilerBase.GetExpressionE(const prec: Integer;
  posExpres: TPosExpres);
{Envoltura para GetExpression(). Se coloca así porque GetExpression()
tiene diversos puntos de salida y Se necesita llamar siempre a expr_end() al
terminar.
Toma una expresión del contexto de entrada y devuelve el resultado em "res".
"isParam" indica que la expresión evaluada es el parámetro de una función.}
begin
  Inc(ExprLevel);  //cuenta el anidamiento
  {$IFDEF LogExpres} LogExpLevel('>Inic.expr'); {$ENDIF}
  if OnExprStart<>nil then OnExprStart;  //llama a evento
  res := GetExpression(prec);
  if HayError then exit;
  if OnExprEnd<>nil then OnExprEnd(posExpres);    //llama al evento de salida
  {$IFDEF LogExpres} LogExpLevel('>Fin.expr'); {$ENDIF}
  Dec(ExprLevel);
  {$IFDEF LogExpres}
  if ExprLevel = 0 then debugln('');
  {$ENDIF}
end;
//Manejo de errores y advertencias
procedure TCompilerBase.ClearError;
begin
  HayError := false;
end;
procedure TCompilerBase.GenWarn(msg: string; fil: String; row, col: integer);
{Genera un mensaje de advertencia en la posición indicada.}
begin
  if OnWarning<>nil then OnWarning(msg, fil, row, col);
end;
procedure TCompilerBase.GenWarn(msg: string; const Args: array of const;
  fil: String; row, col: integer);
begin
  GenWarn(Format(msg, Args), fil, row, col);
end;
procedure TCompilerBase.GenWarn(msg: string);
{Genera un mensaje de Advertencia, en la posición actual del contexto. }
begin
  if (cIn = nil) or (cIn.curCon = nil) then begin
    GenWarn(msg, '', -1, -1);
  end else begin
    GenWarn(msg, cIn.curCon.arc, cIn.curCon.row, cIn.curCon.col);
  end;
end;
procedure TCompilerBase.GenWarn(msg: string; const Args: array of const);
{Genera un mensaje de Advertencia, en la posición actual del contexto. }
begin
  GenWarn(Format(msg, Args));
end;
procedure TCompilerBase.GenWarnPos(msg: string; const Args: array of const;
  srcPos: TSrcPos);
begin
  GenWarn(Format(msg, Args), srcPos.fil, srcPos.row, srcPos.col);
end;
//Rutinas de generación de error
procedure TCompilerBase.GenError(msg: string; fil: String; row, col: integer);
{Genera un mensaje de error en la posición indicada.}
begin
  if OnError<>nil then OnError(msg, fil, row, col);
  HayError := true;
end;
procedure TCompilerBase.GenError(msg: String; const Args: array of const;
  fil: String; row, col: integer);
{Versión con parámetros de GenError.}
begin
  GenError(Format(msg, Args), fil, row, col);
end;
procedure TCompilerBase.GenError(msg: string);
{Función de acceso rápido para Perr.GenError(). Pasa como posición a la posición
del contexto actual. Realiza la traducción del mensaje también.}
begin
  if (cIn = nil) or (cIn.curCon = nil) then begin
    GenError(msg, '', -1, -1);
  end else begin
    GenError(msg, cIn.curCon.arc, cIn.curCon.row, cIn.curCon.col);
  end;
end;
procedure TCompilerBase.GenError(msg: String; const Args: array of const);
{Genera un mensaje de error eb la posición actual del contexto.}
begin
  GenError(Format(msg, Args));
end;
procedure TCompilerBase.GenErrorPos(msg: String; const Args: array of const;
  srcPos: TSrcPos);
{Genera error en una posición específica del código}
begin
  GenError(Format(msg, Args), srcPos.fil, srcPos.row, srcPos.col);
end;

//Inicialización
constructor TCompilerBase.Create;
begin
  ClearError;   //inicia motor de errores
  //Inicia lista de tipos
  typs := TTypes.Create(true);
  //Crea arbol de elementos
  TreeElems := TXpTreeElements.Create;
  listFunSys:= TxpEleFuns.Create(true);
  //inicia la sintaxis
  xLex := TSynFacilSyn.Create(nil);   //crea lexer
  func0 := TxpEleFun.Create;  //crea la función 0, para uso interno

  cIn := TContexts.Create(xLex); //Crea lista de Contextos
  ejecProg := false;
  //Actualiza las referencias a los tipos de tokens existentes en SynFacilSyn
  tnEol     := xLex.tnEol;
  tnSymbol  := xLex.tnSymbol;
  tnSpace   := xLex.tnSpace;
  tnIdentif := xLex.tnIdentif;
  tnNumber  := xLex.tnNumber;
  tnKeyword := xLex.tnKeyword;
  tnString  := xLex.tnString;
  tnComment := xLex.tnComment;
  //Atributos
  tkEol     := xLex.tkEol;
  tkSymbol  := xLex.tkSymbol;
  tkSpace   := xLex.tkSpace;
  tkIdentif := xLex.tkIdentif;
  tkNumber  := xLex.tkNumber;
  tkKeyword := xLex.tkKeyword;
  tkString  := xLex.tkString;
  tkComment := xLex.tkComment;
  //Crea nuevos tipos necesarios para el Analizador Sintáctico
  tnOperator := xLex.NewTokType('Operator', tkOperator); //necesario para analizar expresiones
  tnBoolean  := xLex.NewTokType('Boolean', tkBoolean);  //constantes booleanas
  tnSysFunct := xLex.NewTokType('SysFunct', tkSysFunct); //funciones del sistema
  tnType     := xLex.NewTokType('Types', tkType);    //tipos de datos
end;
destructor TCompilerBase.Destroy;
begin
  cIn.Destroy; //Limpia lista de Contextos
  func0.Destroy;
  xLex.Free;
  listFunSys.Destroy;
  TreeElems.Destroy;
  typs.Free;
  inherited Destroy;
end;

{ TOperand }
function TOperand.VarName: string; inline;
begin
  Result := rVar.name;
end;
function TOperand.offs: TVarOffs;
{Dirección de memoria, cuando es de tipo Char, Byte, Bit o Boolean.}
begin
  if (typ = typBit) or (typ = typBool) then begin
    Result := rVar.adrBit.offs;
  end else begin  //Char o byte
    Result := rVar.adrByte0.offs;
  end;
end;
function TOperand.Loffs: TVarOffs;
{Dirección de memoria baja, cuando es de tipo Word.}
begin
  Result := rVar.adrByte0.offs;
end;
function TOperand.Hoffs: TVarOffs;
begin
  Result := rVar.adrByte1.offs;
end;
function TOperand.bank: TVarBank;
{Banco, cuando es de tipo Byte.}
begin
  if (typ = typBit) or (typ = typBool) then begin
    Result := rVar.adrBit.bank;
  end else begin  //Char o byte
    Result := rVar.adrByte0.bank;
  end;
end;
function TOperand.Lbank: TVarBank;
{Banco del byte bajo, cuando es de tipo Word.}
begin
  Result := rVar.adrByte0.bank;
end;
function TOperand.bit: byte;
begin
  //Si se pide el bit, se asume que es de tipo "Bit".
  Result := rVar.adrBit.bit;
end;

procedure TOperand.Invert;
begin
  if catOp = coConst then begin
    //Para constantes, no se puede negar la lógica, sino el valor
    valBool := not valBool;
  end else begin
    //Variables y expresiones, sí.
    Inverted := not Inverted;  //invierte lógica
  end;
end;

function TOperand.aWord: word; inline;
begin
  Result := word(valInt);
end;
function TOperand.HByte: byte; inline;
begin
  Result := HI(word(valInt));
end;
function TOperand.LByte: byte; inline;
begin
  Result := LO(word(valInt));
end;
function TOperand.CanBeWord: boolean;
{Indica si el valor constante que contiene, puede ser convertido a un WORD sin pérdida}
begin
  Result := (valInt>=0) and (valInt<=$ffff);
end;
function TOperand.CanBeByte: boolean;
{Indica si el valor constante que contiene, puede ser convertido a un BYTE sin pérdida}
begin
  Result := (valInt>=0) and (valInt<=$ff);
end;
procedure TOperand.CopyConsValTo(var c: TxpEleCon);
begin
  //hace una copia selectiva por velocidad, de acuerdo a la categoría
  case typ.cat of
  t_boolean : c.val.ValBool:=val.ValBool;
  t_integer,
  t_uinteger: c.val.ValInt  := val.ValInt;
  t_float   : c.val.ValFloat:= val.ValFloat;
  t_string  : c.val.ValStr  := val.ValStr;
  else
    MsgErr('Internal PicPas error');
    {En teoría, cualquier valor constante que pueda contener TOperand, debería poder
    transferirse a una cosntante, porque usan el mismo contenedor, así que si pasa esto
    solo puede ser que faltó implementar.}
  end;
end;
procedure TOperand.GetConsValFrom(const c: TxpEleCon);
{Copia valores constante desde una constante. Primero TOperand, debería tener inicializado
 correctamente su campo "catTyp". }
begin
  case typ.cat of
  t_boolean : val.ValBool := c.val.ValBool;
  t_integer,
  t_uinteger: val.ValInt := c.val.ValInt;
  t_float   : val.ValFloat := c.val.ValFloat;
  t_string  : val.ValStr := c.val.ValStr;
  else
    MsgErr('Internal PicPas error');
    //faltó implementar.
  end;
end;
procedure TOperand.SetvalBool(AValue: boolean);
begin
  val.ValBool:=AValue;
end;
procedure TOperand.SetvalFloat(AValue: extended);
begin
//  if FvalFloat=AValue then Exit;
  val.ValFloat:=AValue;
end;
procedure TOperand.SetvalInt(AValue: Int64);
begin
//  if FvalInt=AValue then Exit;
  val.ValInt:=AValue;
end;
function TOperand.catOpStr: string;
{Categoría en cadena.}
begin
  case catOp of
  coConst : exit('Constant');
  coVariab: exit('Variable');
  coExpres: exit('Expression');
  end;
end;
function TOperand.catOpChr: char;
{Categoría en caracter.}
begin
  case catOp of
  coConst : exit('k');
  coVariab: exit('v');
  coExpres: exit('X');
  end;
end;
procedure TOperand.LoadToReg;
begin
  if typ.OnLoadToReg<> nil then typ.OnLoadToReg(@self);
end;
procedure TOperand.DefineRegister;
begin
  //llama al evento de pila
  if typ.OnDefineRegister<> nil then typ.OnDefineRegister;
end;
function TOperand.FindOperator(const oper: string): TxpOperator;
//Recibe la cadena de un operador y devuelve una referencia a un objeto Toperator, del
//operando. Si no está definido el operador para este operando, devuelve nullOper.
begin
  Result := typ.FindBinaryOperator(oper);
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
    dicSet('Duplicated identifier: "%s"', 'Identificador duplicado: "%s"');
    dicSet('Undefined type "%s"', 'Tipo "%s" no definido.');
    dicSet('";" expected.', 'Se esperaba ";"');
    dicSet('"%s" expected.', 'Se esperaba "%s"');
    dicSet('Type parameters error on %s', 'Error en tipo de parámetros de %s');
    dicSet('Unknown identifier: %s', 'Identificador desconocido: %s');
    dicSet('Function not implemented: %s', 'Función no implementada: "%s"');
    dicSet('Error in expression. ")" expected.', 'Error en expresión. Se esperaba ")"');
    dicSet('Operand expected.', 'Se esperaba operando.');
    dicSet('Illegal Operation: %s', 'Operación no válida: %s');
    dicSet('Undefined operator: %s for type: %s','No está definido el operador: %s para tipo: %s');
    dicSet('Duplicated function: %s','Función duplicada: %s');
    dicSet('Cannot apply the operator "%s" to type "%s"', 'No se puede aplicar el operador "%s" al tipo "%s"');
  end;
  end;
end;

end.
