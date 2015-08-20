{XpresParserPIC

Versión de XpresParser, orientada a trabajar con microcontroladores PIC.
La idea es tener aquí todas las rutinas que en lo sposible sean independientes del
lenguaje y del modelo de PIC.

Las variables públicas más importantes de este módulo son:

 vars[]  -> almacena a las variables declaradas
 typs[]  -> almacena a los tipos declarados
 funcs[] -> almacena a las funciones declaradas
 cons[]  -> almacena a las constantes declaradas

Para mayor información sobre el uso del framework Xpres, consultar la documentación
técnica.
}
unit XpresParserPIC;
interface
uses
  Classes, SysUtils, fgl, Forms, LCLType, Dialogs, lclProc,
  SynEditHighlighter, SynFacilHighlighter, SynFacilBasic,
  XpresBas, MisUtils;

type  //tipos enumerados
//Categoría de Operando
TCatOperan = (
  coConst =%00,  //Constante. Inlcuyendo expresiones de constantes evaluadas.
  coVariab=%01,  //Variable. Variable única.
  coExpres=%10   //Expresión. Algo que requiere cálculo (incluyendo a una función).
);
{Categoría de operación. Se construye para poder representar dos valores de TCatOperan
 en una solo valor byte (juntando sus bits), para facilitar el uso de un CASE ... OF}
TCatOperation =(
  coConst_Const=  %0000,
  coConst_Variab= %0001,
  coConst_Expres= %0010,
  coVariab_Const= %0100,
  coVariab_Variab=%0101,
  coVariab_Expres=%0110,
  coExpres_Const= %1000,
  coExpres_Variab=%1001,
  coExpres_Expres=%1010
);

//categorías básicas de tipo de datos
TCatType=(
  t_integer,  //números enteros
  t_uinteger, //enteros sin signo
  t_float,    //de coma flotante
  t_string,   //cadena de caracteres
  t_boolean,  //booleano
  t_enum      //enumerado
);

//tipo de identificador
TIdentifType = (idtNone, idtVar, idtFunc, idtCons);

type
TType = class;
TOperator = class;

TVarOffs = word;
TVarBank = byte;

{Espacio para almacenar a los posibles valores de una constante.
Debe tener campos para los tipos básicos de variable haya en "TCatType" y para valores
estructurados}
TConsValue = record
  ValInt  : Int64;    //Para alojar a los valores t_integer y t_uinteger
  ValFloat: extended; //Para alojar a los valores t_float
  ValBool : boolean;  //Para alojar a los valores t_boolean
  ValStr  : string;   //Para alojar a los valores t_string
end;

TCons = record
  nom : string;   //nombre de la variable
  typ : Ttype;    //tipo de la variable
  amb : string;   //ámbito o alcance de la constante
  //valores de la constante
  val : TConsValue;
end;
//registro para almacenar información de las variables
TVar = record
  nom : string;   //nombre de la variable
  typ : Ttype;    //tipo de la variable
  amb : string;   //ámbito o alcance de la variable
  //direción física. Usado para implementar un compilador
  offs: TVarOffs;
  bank: TVarBank;   //banco o segmento. Usado solo en algunas arquitecturas
  bit : byte;       //posición del bit. Usado para variables booleanas.
end;

{ TOperand }
//Operando
TOperand = object
public
  catOp: TCatOperan; //Categoría de operando
  typ  : TType;     //Referencia al tipo de dato
  txt  : string;    //Texto del operando o expresión, tal como aparece en la fuente
  ifun : integer;   //índice a funciones, en caso de que sea función
  ivar : integer;   //índice a variables, en caso de que sea variable
  {---------------------------------------------------------
  Estos campos describen al operando, independientemente de que se le encuentree
  un tipo, válido. Si se le encuentra un tipo válido, se tendrá la referencia al tipo
  en "typ", y allí se tendrán los campos que describirán cómo se debe tratar realmente
  al operando. No tienen por qué coincidir con los campos equivalentes de "typ"}
  catTyp: TCatType;  //Categoría de Tipo de dato.
  size  : integer;   //Tamaño del operando en bytes.
  {---------------------------------------------------------}
  procedure Push; inline;  //pone el operador en la pila
  procedure Pop; inline;   //saca el operador en la pila
  function FindOperator(const oper: string): TOperator; //devuelve el objeto operador
  function GetOperator: Toperator;
  //Funciones para facilitar el acceso a campos de la variable, cuando sea variable
  function VarName: string; inline; //nombre de la variable, cuando sea de categ. coVariab
  function offs: TVarOffs; inline;  //dirección de la variable
  function Loffs: TVarOffs; inline; //dirección del byte bajo
  function Hoffs: TVarOffs; inline; //dirección del byte alto
  function bank: TVarBank; inline;  //banco o segmento
  function bit : byte; inline;  //banco o segmento
private
  Val  : TConsValue;
  procedure SetvalBool(AValue: boolean);
  procedure SetvalFloat(AValue: extended);
  procedure SetvalInt(AValue: Int64);
public
  //Campos de acceso a los valores constantes
  property valInt  : Int64 read val.ValInt write SetvalInt;
  property valFloat: extended read val.ValFloat write SetvalFloat;
  property valBool : boolean read val.ValBool write SetvalBool;
  //funciones de ayuda para adaptar los tipos numéricos
  function aWord: word; inline;  //devuelve el valor en Word
  function HByte: byte; inline;  //devuelve byte alto de valor entero
  function LByte: byte; inline;  //devuelve byte bajo de valor entero
  //campos para validar el rango de los valores
  function CanBeWord: boolean;   //indica si cae en el rango de un WORD
  function CanBeByte: boolean;   //indica si cae en el rango de un BYTE
  //métodos para mover valores desde/hacia una constante externa
  procedure CopyConsValTo(var c: TCons);
  procedure GetConsValFrom(const c: Tcons);
end;
TProcLoadOperand = procedure(const Op: TOperand);

TProcDefineVar = procedure(const varName, varInitVal: string);
TProcExecOperat = procedure;
TProcExecFunction = procedure(ifun: integer);  //con índice de función

//"Tipos de datos"

//Tipo operación
TxOperation = class
  OperatType : TType;   //tipo de Operando sobre el cual se aplica la operación.
  proc       : TProcExecOperat;  //Procesamiento de la operación
end;

TOperations = specialize TFPGObjectList<TxOperation>; //lista de bloques

//Operador
{ TOperator }

TOperator = class
  txt: string;    //cadena del operador '+', '-', '++', ...
  jer: byte;      //precedencia
  nom: string;    //nombre de la operación (suma, resta)
  idx: integer;   //ubicación dentro de un arreglo
  Operations: TOperations;  //operaciones soportadas. Debería haber tantos como
                            //Num. Operadores * Num.Tipos compatibles.
  function CreateOperation(OperadType: Ttype; proc: TProcExecOperat): TxOperation;  //Crea operación
  function FindOperation(typ0: Ttype): TxOperation;  //Busca una operación para este operador
  constructor Create;
  destructor Destroy; override;
end;

TOperators = specialize TFPGObjectList<TOperator>; //lista de bloques

{ TType }
//"Tipos de datos"
TType = class
  name : string;      //nombre del tipo ("int8", "int16", ...)
  cat  : TCatType;    //categoría del tipo (numérico, cadena, etc)
  size : smallint;    //tamaño en bytes del tipo
  idx  : smallint;    //ubicación dentro de la matriz de tipos
  amb  : TFaSynBlock; //ámbito de validez del tipo
  OnGlobalDef: TProcDefineVar; {Evento. Es llamado cada vez que se encuentra la
                                declaración de una variable (de este tipo) en el ámbito global.}
  OnLocalDef: TProcDefineVar;  {Evento. Es llamado cada vez que se encuentra la
                                declaración de una variable (de este tipo) en un ámbito local.}
  OnPush  : TProcLoadOperand; {Evento. Es llamado cuando se pide cargar un operando
                               (de este tipo) en la pila. }
  OnPop   : TProcLoadOperand; {Evento. Es llamado cuando se pide cargar un operando
                               (de este tipo) en la pila. }
  codLoad: string;   //código de carga de operando. Se usa si "onLoad" es NIL.
  Operators: TOperators;      //operadores soportados
//  procedure DefineLoadOperand(codLoad0: string);  //Define carga de un operando
  function CreateOperator(txt0: string; jer0: byte; name0: string): TOperator; //Crea operador
  function FindOperator(const Opr: string): TOperator;  //indica si el operador está definido
  constructor Create;
  destructor Destroy; override;
end;

//Lista de tipos
TTypes = specialize TFPGObjectList<TType>; //lista de bloques

TFindFuncResult = (TFF_NONE, TFF_PARTIAL, TFF_FULL);

//registro para almacenar información de las funciones
Tfunc = record
  name: string;   //nombre de la función
  typ : Ttype;    //tipo que devuelve
  pars: array of Ttype;  //parámetros de entrada
  amb : string;   //ámbito o alcance de la función
  //direción física. Usado para implementar un compilador
  adrr: integer;  //dirección física
  //Campos usados para implementar el intérprete sin máquina virtual
  proc: TProcExecFunction;  //referencia a la función que implementa
  posF: TPoint;    //posición donde empieza la función en el código
end;

{ TCompiler }

{Clase base para crear al objeto compilador}

{ TCompilerBase }

TCompilerBase = class
protected  //Eventos del compilador
  {Notar que estos eventos no se definen para usar métofdos de objetos, sino que
  por comodidad para impementar el intérprete (y espero que por velocidad), usan
  simples procedimientos aislados}
  OnExprStart: procedure(const exprLevel: integer);     {Se genera al iniciar la
                                                     evaluación de una expresión.}
  OnExprEnd  : procedure(const exprLevel: integer; isParam: boolean);  {Se genera
                                             el terminar de evaluar una expresión}
  ExprLevel: Integer;  //Nivel de anidamiento de la rutina de evaluación de expresiones
  procedure ClearTypes;
  function CreateType(nom0: string; cat0: TCatType; siz0: smallint): TType;
  procedure CreateFunction(funName, varType: string);
  function CreateFunction(funName: string; typ: ttype; proc: TProcExecFunction
    ): integer;
  function CreateSysFunction(funName: string; typ: ttype; proc: TProcExecFunction
    ): integer;
  procedure CreateParam(ifun: integer; name: string; typ: ttype);
  function CaptureDelExpres: boolean;
  procedure ClearVars;
  procedure ClearAllConst;
  procedure ClearAllFuncs;
  procedure ClearAllVars;
  procedure ClearFuncs;
//  function CategName(cat: TCatType): string;
  procedure TipDefecNumber(var Op: TOperand; toknum: string); virtual; abstract;
  procedure TipDefecString(var Op: TOperand; tokcad: string); virtual; abstract;
  procedure TipDefecBoolean(var Op: TOperand; tokcad: string); virtual; abstract;
  function FindDuplicFunction: boolean;
  function EOExpres: boolean;
  function EOBlock: boolean;
  procedure SkipWhites; virtual;  //rutina para saltar blancos
  procedure GetExpression(const prec: Integer; isParam: boolean=false);
//  procedure GetBoolExpression;
//  procedure CreateVariable(const varName: string; typ: ttype);
//  procedure CreateVariable(varName, varType: string);
  function FindVar(const varName: string; out idx: integer): boolean;
  function FindCons(const conName: string; out idx: integer): boolean;
  function FindFunc(const funName: string; out idx: integer): boolean;
  function FindPredefName(name: string): TIdentifType;
  function GetOperand: TOperand; virtual;
  function GetOperandP(pre: integer): TOperand;
  procedure ClearParamsFunc(ifun: integer); virtual;
  procedure CaptureParams; virtual;
  function FindFuncWithParams0(const funName: string; var idx: integer;
    idx0: integer=1): TFindFuncResult;
private
  procedure Evaluar(var Op1: TOperand; opr: TOperator; var Op2: TOperand);
  function GetExpressionCore(const prec: Integer): TOperand;
  function SameParamsFunctions(iFun1, iFun2: integer): boolean;
public
  PErr  : TPError;     //Objeto de Error
  xLex  : TSynFacilSyn; //resaltador - lexer
  //variables públicas del compilador
  ejecProg: boolean;   //Indica que se está ejecutando un programa o compilando
  DetEjec: boolean;   //para detener la ejecución (en intérpretes)
  function HayError: boolean;
  procedure GenError(msg: string);
  procedure GenError(msg: String; const Args: array of const);
  function ArcError: string;
  function nLinError: integer;
  function nColError: integer;
  procedure ShowError;
public
  constructor Create; virtual;
  destructor Destroy; override;
end;

var
  {Variables globales. Realmente deberían ser campos de TCompilerBase. Se ponen aquí,
   para que puedan ser accedidas fácilmente desde el archivo "interprte.pas"}

  vars  : array of TVar;  //lista de variables
  funcs : array of Tfunc; //lista de funciones
  cons  : array of TCons;  //lista de constantes
  typs  : TTypes;       //lista de tipos (EL nombre "types" ya está reservado)
  nIntVar : integer;    //número de variables internas
  nIntFun : integer;    //número de funciones internas
  nIntCon : integer;    //número de constantes internas

  cIn    : TContexts;   //entrada de datos
  p1, p2 : TOperand;    //Pasa los operandos de la operación actual
  res    : TOperand;    //resultado de la evaluación de la última expresión.
  catOperation: TCatOperation;  //combinación de categorías de los operandos
  //referencias obligatorias
  tkEol     : TSynHighlighterAttributes;
  tkIdentif : TSynHighlighterAttributes;
  tkKeyword : TSynHighlighterAttributes;
  tkNumber  : TSynHighlighterAttributes;
  tkString  : TSynHighlighterAttributes;
  tkOperator: TSynHighlighterAttributes;
  tkBoolean : TSynHighlighterAttributes;
  tkSysFunct: TSynHighlighterAttributes;

implementation
uses Graphics;
var  //variables privadas del compilador
  nullOper : TOperator; //Operador nulo. Usado como valor cero.

//////////////// implementación de métodos  //////////////////
{ TOperator }

function TOperator.CreateOperation(OperadType: Ttype; proc: TProcExecOperat): TxOperation;
var
  r: TxOperation;
begin
  //agrega
  r := TxOperation.Create;
  r.OperatType:=OperadType;
//  r.CodForConst:=codCons;
//  r.CodForVar:=codVar;
//  r.CodForExpr:=codExp;
  r.proc:=proc;
  //agrega
  operations.Add(r);
  Result := r;
end;
function TOperator.FindOperation(typ0: Ttype): TxOperation;
{Busca, si encuentra definida, alguna operación, de este operador con el tipo indicado.
Si no lo encuentra devuelve NIL}
var
  r: TxOperation;
begin
  Result := nil;
  for r in Operations do begin
    if r.OperatType = typ0 then begin
      exit(r);
    end;
  end;
end;
constructor TOperator.Create;
begin
  Operations := TOperations.Create(true);
end;
destructor TOperator.Destroy;
begin
  Operations.Free;
  inherited Destroy;
end;

{ TType }
function TType.CreateOperator(txt0: string; jer0: byte; name0: string): TOperator;
{Permite crear un nuevo ooperador soportado por este tipo de datos. Si hubo error,
devuelve NIL. En caso normal devuelve una referencia al operador creado}
var
  r: TOperator;  //operador
begin
  //verifica nombre
  if FindOperator(txt0)<>nullOper then begin
    Result := nil;  //indica que hubo error
    exit;
  end;
  //inicia
  r := TOperator.Create;
  r.txt:=txt0;
  r.jer:=jer0;
  r.nom:=name0;
  r.idx:=Operators.Count;
  //agrega
  Operators.Add(r);
  Result := r;
end;
function TType.FindOperator(const Opr: string): TOperator;
//Recibe la cadena de un operador y devuelve una referencia a un objeto Toperator, del
//tipo. Si no está definido el operador para este tipo, devuelve nullOper.
var
  i: Integer;
begin
  Result := nullOper;   //valor por defecto
  for i:=0 to Operators.Count-1 do begin
    if Operators[i].txt = upCase(Opr) then begin
      exit(Operators[i]); //está definido
    end;
  end;
  //no encontró
  Result.txt := Opr;    //para que sepa el operador leído
end;
constructor TType.Create;
begin
  Operators := TOperators.Create(true);  //crea contenedor de Contextos, con control de objetos.
end;
destructor TType.Destroy;
begin
  Operators.Free;
  inherited Destroy;
end;

{TCompilerBase}
function TCompilerBase.HayError: boolean;
begin
  Result := PErr.HayError;
end;
procedure TCompilerBase.GenError(msg: string);
{Función de acceso rápido para Perr.GenError(). Pasa como posición a la posición
del contexto actual. Realiza la traducción del mensaje también.}
begin
  if (cIn = nil) or (cIn.curCon = nil) then
    Perr.GenError(dic(msg),'',1)
  else
    Perr.GenError(dic(msg), cIn.curCon);
end;
procedure TCompilerBase.GenError(msg : String; const Args : Array of const);
{Versión con parámetros de GenError.}
begin
  if (cIn = nil) or (cIn.curCon = nil) then
    Perr.GenError(dic(msg, Args),'',1)
  else
    Perr.GenError(dic(msg, Args), cIn.curCon);
end;
function TCompilerBase.ArcError: string;
begin
  Result:= Perr.ArcError;
end;
function TCompilerBase.nLinError: integer;
begin
  Result := Perr.nLinError;
end;
function TCompilerBase.nColError: integer;
begin
  Result := Perr.nColError;
end;
procedure TCompilerBase.ShowError;
begin
  Application.MessageBox(PChar(Perr.TxtErrorRC),
                         PChar(Perr.NombPrograma), MB_ICONEXCLAMATION);
//  Perr.Show;
end;

function TCompilerBase.FindVar(const varName:string; out idx: integer): boolean;
//Busca el nombre dado para ver si se trata de una variable definida
//Si encuentra devuelve TRUE y actualiza el índice.
var
  tmp: String;
  i: Integer;
begin
  Result := false;
  tmp := upCase(varName);
  for i:=0 to high(vars) do begin
    if Upcase(vars[i].nom)=tmp then begin
      idx := i;
      exit(true);
    end;
  end;
end;
function TCompilerBase.FindFunc(const funName:string; out idx: integer): boolean;
//Busca el nombre dado para ver si se trata de una función definida
//Si encuentra devuelve TRUE y actualiza el índice.
var
  tmp: String;
  i: Integer;
begin
  Result := false;
  tmp := upCase(funName);
  for i:=0 to high(funcs) do begin
    if Upcase(funcs[i].name)=tmp then begin
      idx := i;
      exit(true);
    end;
  end;
end;
function TCompilerBase.FindCons(const conName:string; out idx: integer): boolean;
//Busca el nombre dado para ver si se trata de una constante definida
//Si encuentra devuelve TRUE y actualiza el índice.
var
  tmp: String;
  i: Integer;
begin
  Result := false;
  tmp := upCase(conName);
  for i:=0 to high(cons) do begin
    if Upcase(cons[i].nom)=tmp then begin
      idx := i;
      exit(true);
    end;
  end;
end;
function TCompilerBase.FindPredefName(name: string): TIdentifType;
//Busca un identificador e indica si ya existe el nombre, sea como variable,
//función o constante.
var i: integer;
begin
  //busca como variable
  if FindVar(name,i) then begin
     exit(idtVar);
  end;
  //busca como función
  if FindFunc(name,i) then begin
     exit(idtFunc);
  end;
  //busca como constante
  if FindCons(name,i) then begin
     exit(idtCons);
  end;
  //no lo encuentra
  exit(idtNone);
end;

//Manejo de tipos
function TCompilerBase.CreateType(nom0: string; cat0: TCatType; siz0: smallint): TType;
//Crea una nueva definición de tipo en el compilador. Devuelve referencia al tipo recien creado
var r: TType;
  i: Integer;
begin
  //verifica nombre
  for i:=0 to typs.Count-1 do begin
    if typs[i].name = nom0 then begin
      GenError('Unknown type identifier.');
      exit;
    end;
  end;
  //configura nuevo tipo
  r := TType.Create;
  r.name:=nom0;
  r.cat:=cat0;
  r.size:=siz0;
  r.idx:=typs.Count;  //toma ubicación
//  r.amb:=;  //debe leer el bloque actual
  //agrega
  typs.Add(r);
  Result:=r;   //devuelve índice al tipo
end;
procedure TCompilerBase.ClearTypes;  //Limpia los tipos
begin
  typs.Clear;
end;
procedure TCompilerBase.ClearVars;
//Limpia todas las variables creadas por el usuario.
begin
  setlength(vars, nIntVar);  //deja las del sistema
end;
procedure TCompilerBase.ClearAllVars;
//Elimina todas las variables, incluyendo las predefinidas.
begin
  nIntVar := 0;
  setlength(vars,0);
end;
procedure TCompilerBase.ClearFuncs;
//Limpia todas las funciones creadas por el usuario.
begin
  setlength(funcs,nIntFun);  //deja las del sistema
end;
procedure TCompilerBase.ClearAllFuncs;
//Elimina todas las funciones, incluyendo las predefinidas.
begin
  nIntFun := 0;
  setlength(funcs,0);
end;
procedure TCompilerBase.ClearAllConst;
//Elimina todas las funciones, incluyendo las predefinidas.
begin
  nIntCon := 0;
  setlength(cons,0);
end;

function TCompilerBase.CreateFunction(funName: string; typ: ttype; proc: TProcExecFunction): integer;
//Crea una nueva función y devuelve un índice a la función.
var
  r : Tfunc;
  i, n: Integer;
begin
  //verifica si existe como variable
  if FindVar(funName, i) then begin
    GenError('Duplicated identifier: "%s"', [funName]);
    exit;
  end;
  //verifica si existe como constante
  if FindCons(funName, i)then begin
    GenError('Duplicated identifier: "%s"', [funName]);
    exit;
  end;
  //puede existir como función, no importa (sobrecarga)
  //registra la función en la tabla
  r.name:= funName;
  r.typ := typ;
  r.proc:= proc;
  setlength(r.pars,0);  //inicia arreglo
  //agrega
  n := high(funcs)+1;
  setlength(funcs, n+1);
  funcs[n] := r;
  Result := n;
end;
procedure TCompilerBase.CreateFunction(funName, varType: string);
//Define una nueva función en memoria.
var t: ttype;
  hay: Boolean;
begin
  //Verifica el tipo
  hay := false;
  for t in typs do begin
    if t.name=varType then begin
       hay:=true; break;
    end;
  end;
  if not hay then begin
    GenError('Undefined type "%s"', [varType]);
    exit;
  end;
  CreateFunction(funName, t, nil);
  //Ya encontró tipo, llama a evento
//  if t.OnGlobalDef<>nil then t.OnGlobalDef(funName, '');
end;
function TCompilerBase.CreateSysFunction(funName: string; typ: ttype; proc: TProcExecFunction): integer;
//Crea una función del sistema o interna. Estas funciones estan siempre disponibles.
//Las funciones internas deben crearse todas al inicio.
begin
  Result := CreateFunction(funName, typ, proc);
  Inc(nIntFun);  //leva la cuenta
end;
procedure TCompilerBase.ClearParamsFunc(ifun: integer);  inline;
//Elimina los parámetros de una función
begin
  setlength(funcs[ifun].pars,0);
end;
procedure TCompilerBase.CreateParam(ifun: integer; name: string; typ: ttype);
//Crea un parámetro para una función
var
  n: Integer;
begin
  //agrega
  n := high(funcs[ifun].pars)+1;
  setlength(funcs[ifun].pars, n+1);
  funcs[ifun].pars[n] := typ;  //agrega referencia
end;
function TCompilerBase.SameParamsFunctions(iFun1, iFun2: integer): boolean;
//Compara los parámetros de dos funciones. Si tienen el mismo número de
//parámetros y el mismo tipo, devuelve TRUE.
var
  i: Integer;
begin
  Result:=true;  //se asume que son iguales
  if High(funcs[iFun1].pars) <> High(funcs[iFun2].pars) then
    exit(false);   //distinto número de parámetros
  //hay igual número de parámetros, verifica
  for i := 0 to High(funcs[iFun1].pars) do begin
    if funcs[iFun1].pars[i] <> funcs[iFun2].pars[i] then begin
      exit(false);
    end;
  end;
  //si llegó hasta aquí, hay coincidencia, sale con TRUE
end;
function TCompilerBase.FindFuncWithParams0(const funName: string; var idx: integer;
  idx0 : integer = 1): TFindFuncResult;
{Busca una función que coincida con el nombre "funName" y con los parámetros de funcs[0]
El resultado puede ser:
 TFF_NONE   -> No se encuentra.
 TFF_PARTIAL-> Se encuentra solo el nombre.
 TFF_FULL   -> Se encuentra y coninciden sus parámetros, actualiza "idx".
"idx0", es el índice inicial desde donde debe buscar. Permite acelerar las búsquedas, cuando
ya se ha explorado antes.
}
var
  tmp: String;
  params : string;   //parámetros de la función
  i: Integer;
  hayFunc: Boolean;
begin
  Result := TFF_NONE;   //por defecto
  hayFunc := false;
  tmp := UpCase(funName);
  for i:=idx0 to high(funcs) do begin  //no debe empezar en 0, porque allí está func[0]
    if Upcase(funcs[i].name)= tmp then begin
      //coincidencia, compara
      hayFunc := true;  //para indicar que encontró el nombre
      if SameParamsFunctions(i,0) then begin
        idx := i;    //devuelve ubicación
        Result := TFF_FULL; //encontró
        exit;
      end;
    end;
  end;
  //si llego hasta aquí es porque no encontró coincidencia
  if hayFunc then begin
    //Encontró al menos el nombre de la función, pero no coincide en los parámetros
    Result := TFF_PARTIAL;
    {Construye la lista de parámetros de las funciones con el mismo nombre. Solo
    hacemos esta tarea pesada aquí, porque  sabemos que se detendrá la compilación}
    params := '';   //aquí almacenará la lista
{    for i:=idx0 to high(funcs) do begin  //no debe empezar 1n 0, porque allí está func[0]
      if Upcase(funcs[i].name)= tmp then begin
        for j:=0 to high(funcs[i].pars) do begin
          params += funcs[i].pars[j].name + ',';
        end;
        params += LineEnding;
      end;
    end;}
  end;
end;
function TCompilerBase.FindDuplicFunction: boolean;
//Verifica si la última función agregada, está duplicada con alguna de las
//funciones existentes. Permite la sobrecarga. Si encuentra la misma
//función definida 2 o más veces, genera error y devuelve TRUE.
//DEbe llamarse siempre, después de definir una función nueva.
var
  ufun : String;
  i,j,n: Integer;
  tmp: String;
begin
  Result := false;
  n := high(funcs);  //última función
  ufun := funcs[n].name;
  //busca sobrecargadas en las funciones anteriores
  for i:=0 to n-1 do begin
    if funcs[i].name = ufun then begin
      //hay una sobrecargada, verifica tipos de parámetros
      if not SameParamsFunctions(i,n) then break;
      //Tiene igual cantidad de parámetros y del mismo tipo. Genera Error
      tmp := '';
      for j := 0 to High(funcs[i].pars) do begin
        tmp += funcs[n].pars[j].name+', ';
      end;
      if length(tmp)>0 then tmp := copy(tmp,1,length(tmp)-2); //quita coma final
      GenError('Duplicated function: %s', [ufun+'( '+tmp+' )']);
      exit(true);
    end;
  end;
end;
{function TCompilerBase.CategName(cat: TCatType): string;
begin
   case cat of
   t_integer: Result := 'Numérico';
   t_uinteger: Result := 'Numérico sin signo';
   t_float: Result := 'Flotante';
   t_string: Result := 'Cadena';
   t_boolean: Result := 'Booleano';
   t_enum: Result := 'Enumerado';
   else Result := 'Desconocido';
   end;
end;}
function TCompilerBase.EOExpres: boolean; inline;
//Indica si se ha llegado al final de una expresión.
begin
  Result := cIn.tok = ';';  //en este caso de ejemplo, usamos putno y coma
  {En la práctica, puede ser conveniente definir un tipo de token como "tkExpDelim", para
   mejorar el tiempo de respuesta del procesamiento, de modo que la condición sería:
     Result := cIn.tokType = tkExpDelim;
  }
end;
function TCompilerBase.EOBlock: boolean; inline;
//Indica si se ha llegado el final de un bloque
begin
  Result := false;
  {No está implementado aquí, pero en la práctica puede ser conveniente definir un tipo de token
   como "tkBlkDelim", para mejorar el tiempo de respuesta del procesamiento, de modo que la
   condición sería:
  Result := cIn.tokType = tkBlkDelim;}
end;
procedure TCompilerBase.SkipWhites;
{Se crea como rutina aparte, para facilitar el poder cambiar el comportamiento y
adaptarlo al modo de trabajo del lenguaje.}
begin
  cIn.SkipWhites;
end;

{ Rutinas del compilador }
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
procedure TCompilerBase.CaptureParams;
//Lee los parámetros de una función en la función interna funcs[0]
begin
  SkipWhites;
  ClearParamsFunc(0);   //inicia parámetros
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
      GetExpression(0, true);  //captura parámetro
      if perr.HayError then exit;   //aborta
      //guarda tipo de parámetro
      CreateParam(0,'', res.typ);
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
    if cIn.tok<>')' then begin
      GenError('")" expected.'); exit;
    end;
    cin.Next;
  end;
end;
function TCompilerBase.GetOperand: TOperand;
{Parte de la funcion analizadora de expresiones que genera codigo para leer un operando.
Debe devolver el tipo del operando y también el valor (obligatorio para el caso
de intérpretes y opcional para compiladores)}
var
  i: Integer;
  ivar: Integer;
  icons: Integer;
  ifun: Integer;
  tmp: String;
begin
  PErr.Clear;
  SkipWhites;
  if cIn.tokType = tkNumber then begin  //constantes numéricas
    Result.catOp:=coConst;       //constante es Mono Operando
    Result.txt:= cIn.tok;     //toma el texto
    TipDefecNumber(Result, cIn.tok); //encuentra tipo de número, tamaño y valor
    if pErr.HayError then exit;  //verifica
    cIn.Next;    //Pasa al siguiente
  end else if cIn.tokType = tkIdentif then begin  //puede ser variable, constante, función
    if FindVar(cIn.tok, ivar) then begin
      //es una variable
      Result.ivar:=ivar;   //guarda referencia a la variable
      Result.catOp:=coVariab;    //variable
      Result.catTyp:= vars[ivar].typ.cat;  //categoría
      Result.typ:=vars[ivar].typ;
      Result.txt:= cIn.tok;     //toma el texto
      cIn.Next;    //Pasa al siguiente
    end else if FindCons(cIn.tok, icons) then begin  //es constante
      //es una variable
//      Result.ivar:=ivar;   //guarda referencia a la variable
      Result.catOp:=coConst;    //constante
      Result.catTyp:= cons[icons].typ.cat;  //categoría
      Result.typ:=cons[icons].typ;
      Result.GetConsValFrom(cons[icons]);  //lee valor
      Result.txt:= cIn.tok;     //toma el texto
      cIn.Next;    //Pasa al siguiente
    end else if FindFunc(cIn.tok, ifun) then begin  //no es variable, debe ser función
      tmp := cIn.tok;  //guarda nombre de función
      cIn.Next;    //Toma identificador
      CaptureParams;  //primero lee parámetros
      if HayError then exit;
      //busca como función
      case FindFuncWithParams0(tmp, i, ifun) of  //busca desde ifun
      //TFF_NONE:      //No debería pasar esto
      TFF_PARTIAL:   //encontró la función, pero no coincidió con los parámetros
         GenError('Type parameters error on %s', [tmp +'()']);
      TFF_FULL:     //encontró completamente
        begin   //encontró
          Result.ifun:=i;      //guarda referencia a la función
          Result.catOp :=coExpres; //expresión
          Result.txt:= cIn.tok;    //toma el texto
    //      Result.catTyp:= funcs[i].typ.cat;  //no debería ser necesario
          Result.typ:=funcs[i].typ;
          funcs[i].proc(i);  //llama al código de la función
          exit;
        end;
      end;
    end else begin
      GenError('Unknown identifier: %s', [cIn.tok]);
      exit;
    end;
  end else if cIn.tokType = tkSysFunct then begin  //es función de sistema
    //Estas funciones debem crearse al iniciar el compilador y están siempre disponibles.
    tmp := cIn.tok;  //guarda nombre de función
    cIn.Next;    //Toma identificador
    CaptureParams;  //primero lee parámetros en func[0]
    if HayError then exit;
    //buscamos una declaración que coincida.
    case FindFuncWithParams0(tmp, i) of
    TFF_NONE:      //no encontró ni la función
       GenError('Function not implemented: %s', [tmp]);
    TFF_PARTIAL:   //encontró la función, pero no coincidió con los parámetros
      GenError('Type parameters error on %s', [tmp +'()']);
    TFF_FULL:     //encontró completamente
      begin   //encontró
        Result.ifun:=i;      //guarda referencia a la función
        Result.catOp :=coExpres; //expresión
        Result.txt:= cIn.tok;    //toma el texto
  //      Result.catTyp:= funcs[i].typ.cat;  //no debería ser necesario
        Result.typ:=funcs[i].typ;
        funcs[i].proc(i);  //llama al código de la función
        exit;
      end;
    end;
  end else if cIn.tokType = tkBoolean then begin  //true o false
    Result.catOp:=coConst;       //constante es Mono Operando
    Result.txt:= cIn.tok;     //toma el texto
    TipDefecBoolean(Result, cIn.tok); //encuentra tipo y valor
    if pErr.HayError then exit;  //verifica
    cIn.Next;    //Pasa al siguiente
  end else if cIn.tok = '(' then begin  //"("
    cIn.Next;
    GetExpression(0);
    Result := res;
    if PErr.HayError then exit;
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
  end else if (cIn.tokType = tkOperator then begin
   //los únicos símbolos válidos son +,-, que son parte de un número
    }
  end else begin
    //No se reconoce el operador
    GenError('Operand expected.');
  end;
end;
procedure TCompilerBase.Evaluar(var Op1: TOperand; opr: TOperator; var Op2: TOperand);
{Ejecuta una operación con dos operandos y un operador. "opr" es el operador de Op1.
El resultado debe devolverse en "res". En el caso de intérpretes, importa el
resultado de la Operación.
En el caso de compiladores, lo más importante es el tipo del resultado, pero puede
usarse también "res" para cálculo de expresiones constantes.
}
var
  o: TxOperation;
begin
   debugln(space(ExprLevel)+' Eval('+Op1.txt + ',' + Op2.txt+')');
   PErr.IniError;
   //Busca si hay una operación definida para: <tipo de Op1>-opr-<tipo de Op2>
   o := opr.FindOperation(Op2.typ);
   if o = nil then begin
//      GenError('No se ha definido la operación: (' +
//                    Op1.typ.name + ') '+ opr.txt + ' ('+Op2.typ.name+')');
      GenError('Illegal Operation: %s',
               ['(' + Op1.typ.name + ') '+ opr.txt + ' ('+Op2.typ.name+')']);
      Exit;
    end;
   {Llama al evento asociado con p1 y p2 como operandos. Debe devolver el resultado
   en "res"}
   p1 := Op1; p2 := Op2;  { TODO : Debe optimizarse }
   catOperation := TCatOperation((Ord(Op1.catOp) << 2) or ord(Op2.catOp)); //junta categorías de operandos
   o.proc;      //Ejecuta la operación
   //El resultado debe estar en "res"
   //Completa campos de "res", si es necesario
//   res.txt := Op1.txt + opr.txt + Op2.txt;   //texto de la expresión
//   res.uop := opr;   //última operación ejecutada
End;
function TCompilerBase.GetOperandP(pre: integer): TOperand;
//Toma un operando realizando hasta encontrar un operador de precedencia igual o menor
//a la indicada
var
  Op1: TOperand;
  Op2: TOperand;
  opr: TOperator;
  pos: TPosCont;
begin
  debugln(space(ExprLevel)+' GetOperandP('+IntToStr(pre)+')');
  Op1 :=  GetOperand;  //toma el operador
  if pErr.HayError then exit;
  //verifica si termina la expresion
  pos := cIn.PosAct;    //Guarda por si lo necesita
  SkipWhites;
  opr := Op1.GetOperator;
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
    If opr.jer > pre Then begin  //¿Delimitado por precedencia de operador?
      //es de mayor precedencia, se debe evaluar antes.
      Op2 := GetOperandP(pre);  //toma el siguiente operando (puede ser recursivo)
      if pErr.HayError then exit;
      Evaluar(Op1, opr, Op2);  //devuelve en "res"
      Result:=res;
    End else begin  //la precedencia es menor o igual, debe salir
      cIn.PosAct := pos;   //antes de coger el operador
      Result:=Op1;
    end;
  end;
end;
function TCompilerBase.GetExpressionCore(const prec: Integer): TOperand; //inline;
{Analizador de expresiones. Esta es probablemente la función más importante del
 compilador. Procesa una expresión en el contexto de entrada llama a los eventos
 configurados para que la expresión se evalúe (intérpretes) o se compile (compiladores).
 Devuelve un operando con información sobre el resultado de la expresión.}
var
  Op1, Op2  : TOperand;   //Operandos
  opr1: TOperator;  //Operadores
begin
  Op1.catTyp:=t_integer;    //asumir opcion por defecto
  Op2.catTyp:=t_integer;   //asumir opcion por defecto
  pErr.Clear;
  //----------------coger primer operando------------------
  Op1 := GetOperand; if pErr.HayError then exit;
  debugln(space(ExprLevel)+' Op1='+Op1.txt);
  //verifica si termina la expresion
  SkipWhites;
  opr1 := Op1.GetOperator;
  if opr1 = nil then begin  //no sigue operador
    //Expresión de un solo operando. Lo carga por si se necesita
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
    If opr1.jer <= prec Then begin  //es menor que la que sigue, expres.
      Result := Op1;  //solo devuelve el único operando que leyó
      exit;
    End;
{    //--------------------coger operador ---------------------------
	//operadores unitarios ++ y -- (un solo operando).
    //Se evaluan como si fueran una mini-expresión o función
	if opr1.id = op_incremento then begin
      case Op1.catTyp of
        t_integer: Cod_IncremOperanNumerico(Op1);
      else
        GenError('Operador ++ no es soportado en este tipo de dato.',PosAct);
        exit;
      end;
      opr1 := cogOperador; if pErr.HayError then exit;
      if opr1.id = Op_ninguno then begin  //no sigue operador
        Result:=Op1; exit;  //termina ejecucion
      end;
    end else if opr1.id = op_decremento then begin
      case Op1.catTyp of
        t_integer: Cod_DecremOperanNumerico(Op1);
      else
        GenError('Operador -- no es soportado en este tipo de dato.',PosAct);
        exit;
      end;
      opr1 := cogOperador; if pErr.HayError then exit;
      if opr1.id = Op_ninguno then begin  //no sigue operador
        Result:=Op1; exit;  //termina ejecucion
      end;
    end;}
    //--------------------coger segundo operando--------------------
    Op2 := GetOperandP(Opr1.jer);   //toma operando con precedencia
    debugln(space(ExprLevel)+' Op2='+Op2.txt);
    if pErr.HayError then exit;
    //prepara siguiente operación
    Evaluar(Op1, opr1, Op2);    //evalua resultado en "res"
    Op1 := res;
    if PErr.HayError then exit;
    SkipWhites;
    opr1 := Op1.GetOperator;   {lo toma ahora con el tipo de la evaluación Op1 (opr1) Op2
                                porque puede que Op1 (opr1) Op2, haya cambiado de tipo}
  end;  //hasta que ya no siga un operador
  Result := Op1;  //aquí debe haber quedado el resultado
end;
procedure TCompilerBase.GetExpression(const prec: Integer; isParam: boolean = false
    //indep: boolean = false
    );
{Envoltura para GetExpressionCore(). Se coloca así porque GetExpressionCore()
tiene diversos puntos de salida y Se necesita llamar siempre a expr_end() al
terminar.
Toma una expresión del contexto de entrada y devuelve el resultado em "res".
"isParam" indica que la expresión evaluada es el parámetro de una función.
"indep", indica que la expresión que se está evaluando es anidada pero es independiente
de la expresion que la contiene, así que se puede liberar los registros o pila.
}
{ TODO : Para optimizar debería existir solo GetExpression() y no GetExpressionCore() }
begin
  Inc(ExprLevel);  //cuenta el anidamiento
  debugln(space(ExprLevel)+'>Inic.expr');
  if OnExprStart<>nil then OnExprStart(ExprLevel);  //llama a evento
  res := GetExpressionCore(prec);
  if PErr.HayError then exit;
  if OnExprEnd<>nil then OnExprEnd(ExprLevel, isParam);    //llama al evento de salida
  debugln(space(ExprLevel)+'>Fin.expr');
  Dec(ExprLevel);
  if ExprLevel = 0 then debugln('');
end;
{procedure TCompilerBase.GetBoolExpression;
//Simplifica la evaluación de expresiones booleanas, validando el tipo
begin
  GetExpression(0);  //evalua expresión
  if PErr.HayError then exit;
  if res.Typ.cat <> t_boolean then begin
    GenError('Se esperaba expresión booleana');
  end;
end;

function GetNullExpression(): TOperand;
//Simplifica la evaluación de expresiones sin dar error cuando encuentra algún delimitador
begin
  if
  GetExpression(0);  //evalua expresión
  if PErr.HayError then exit;
end;}

constructor TCompilerBase.Create;
begin
  PErr.IniError;   //inicia motor de errores
  //Inicia lista de tipos
  typs := TTypes.Create(true);
  //Inicia variables, funciones y constantes
  ClearAllVars;
  ClearAllFuncs;
  ClearAllConst;
  //crea el operador NULL
  nullOper := TOperator.Create;
  //inicia la sintaxis
  xLex := TSynFacilSyn.Create(nil);   //crea lexer
  CreateSysFunction('', nil, nil);  //crea la función 0, para uso interno

  if HayError then PErr.Show;
  cIn := TContexts.Create(xLex); //Crea lista de Contextos
  ejecProg := false;
end;
destructor TCompilerBase.Destroy;
begin
  cIn.Destroy; //Limpia lista de Contextos
  xLex.Free;
  nullOper.Free;
  typs.Free;
  inherited Destroy;
end;

{ TOperand }
function TOperand.VarName: string; inline;
begin
  Result := vars[ivar].nom;
end;
function TOperand.offs: TVarOffs;
begin
  Result := vars[ivar].offs;
end;
function TOperand.Loffs: TVarOffs;
begin
  Result := vars[ivar].offs;
end;
function TOperand.Hoffs: TVarOffs;
begin
  Result := vars[ivar].offs+1;  //por ahora se asume que es el siguiente byte
end;

function TOperand.bank: TVarBank;
begin
  Result := vars[ivar].bank;
end;
function TOperand.bit: byte;
begin
  Result := vars[ivar].bit;
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

procedure TOperand.CopyConsValTo(var c: TCons);
begin
  //hace una copia selectiva por velocidad, de acuerdo a la categoría
  case catTyp of
  t_boolean : c.val.ValBool:=val.ValBool;
  t_integer,
  t_uinteger: c.val.ValInt  := val.ValInt;
  t_float   : c.val.ValFloat:= val.ValFloat;
  t_string  : c.val.ValStr  := val.ValStr;
  else
    {En teoría, cualquier valor constante que pueda contener TOperand, debería poder
    transferirse a una cosntante, porque usan el mismo contenedor, así que si pasa esto
    solo puede ser que faltó implementar.}
  end;
end;

procedure TOperand.GetConsValFrom(const c: Tcons);
{Copia valores constante desde una constante. Primero TOperand, debería tener inicializado
 correctamente su campo "catTyp". }
begin
  case catTyp of
  t_boolean : val.ValBool := c.val.ValBool;
  t_integer,
  t_uinteger: val.ValInt := c.val.ValInt;
  t_float   : val.ValFloat := c.val.ValFloat;
  t_string  : val.ValStr := c.val.ValStr;
  else
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

procedure TOperand.Push;
begin
  //llama al evento de pila
  if typ.OnPush <> nil then typ.OnPush(self);
end;
procedure TOperand.Pop;
begin
  //llama al evento de pila
  if typ.OnPop <> nil then typ.OnPop(self);
end;

function TOperand.FindOperator(const oper: string): TOperator;
//Recibe la cadena de un operador y devuelve una referencia a un objeto Toperator, del
//operando. Si no está definido el operador para este operando, devuelve nullOper.
begin
  Result := typ.FindOperator(oper);
end;
function TOperand.GetOperator: Toperator;
//Lee del contexto de entrada y toma un operador. Si no encuentra un operador, devuelve NIL.
//Si el operador encontrado no se aplica al operando, devuelve nullOper.
begin
  if cIn.tokType <> tkOperator then exit(nil);
  //hay un operador
  Result := typ.FindOperator(cIn.tok);
  cIn.Next;   //toma el token
end;

procedure SetLanguage(lang: string);
begin
  case lang of
  'en': begin
    dicClear;  //it's yet in English
  end;
  'es': begin
    //Update messages
    dicSet('Unknown type identifier.', 'Identificador de tipo duplicado.');
    dicSet('Duplicated identifier: "%s"', 'Identificador duplicado: "%s"');
    dicSet('Undefined type "%s"', 'Tipo "%s" no definido.');
    dicSet('Duplicated function: %s','Función duplicada: %s');
    dicSet('";" expected.', 'Se esperaba ";"');
    dicSet('"(" expected.', 'Se esperaba "("');
    dicSet('")" expected.', 'Se esperaba ")"');
    dicSet('Type parameters error on %s', 'Error en tipo de parámetros de %s');
    dicSet('Unknown identifier: %s', 'Identificador desconocido: %s');
    dicSet('Function not implemented: %s', 'Función no implementada: "%s"');
    dicSet('Error in expression. ")" expected.', 'Error en expresión. Se esperaba ")"');
    dicSet('Operand expected.', 'Se esperaba operando.');
    dicSet('Illegal Operation: %s', 'Operación no válida: %s');
    dicSet('Undefined operator: %s for type: %s','No está definido el operador: %s para tipo: %s');
  end;
  end;
end;

end.
