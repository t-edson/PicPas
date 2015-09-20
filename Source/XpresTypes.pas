{
XpresTypes 0.7b
=============
Definiciones para el manejo de expresiones.
Por Tito Hinostroza.
Aquí están definidas los objetos claves para el manejo de expresiones:
Los tipos, los operadores y las operaciones
 }
unit XpresTypes;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, SynFacilBasic;

type

//categorías básicas de tipo de datos
TCatType=(
  t_integer,  //números enteros
  t_uinteger, //enteros sin signo
  t_float,    //de coma flotante
  t_string,   //cadena de caracteres
  t_boolean,  //booleano
  t_enum      //enumerado
);

{Espacio para almacenar a los posibles valores de una constante.
Debe tener campos para los tipos básicos de variable haya en "TCatType" y para valores
estructurados}
TConsValue = record
  ValInt  : Int64;    //Para alojar a los valores t_integer y t_uinteger
  ValFloat: extended; //Para alojar a los valores t_float
  ValBool : boolean;  //Para alojar a los valores t_boolean
  ValStr  : string;   //Para alojar a los valores t_string
end;

TType = class;

//"Tipos de datos"
TProcExecOperat = procedure;
TProcDefineVar = procedure(const varName, varInitVal: string);
//TProcLoadOperand = procedure(const Op: TOperand);
TProcLoadOperand = procedure;

//Tipo operación
TxOperation = class
  OperatType : TType;   //tipo de Operando sobre el cual se aplica la operación.
  proc       : TProcExecOperat;  //Procesamiento de la operación
end;

TOperations = specialize TFPGObjectList<TxOperation>; //lista de operaciones

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

TOperators = specialize TFPGObjectList<TOperator>; //lista de operadores

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
//  codLoad : string;   //código de carga de operando. Se usa si "onLoad" es NIL.
  Operators: TOperators;      //operadores soportados
  function CreateOperator(txt0: string; jer0: byte; name0: string): TOperator; //Crea operador
  function FindOperator(const Opr: string): TOperator;  //indica si el operador está definido
  constructor Create;
  destructor Destroy; override;
end;

//Lista de tipos
TTypes = specialize TFPGObjectList<TType>; //lista de bloques

var  //variables privadas del compilador
  nullOper : TOperator; //Operador nulo. Usado como valor cero.

implementation

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

{ TOperator }
function TOperator.CreateOperation(OperadType: Ttype; proc: TProcExecOperat): TxOperation;
var
  r: TxOperation;
begin
  //agrega
  r := TxOperation.Create;
  r.OperatType:=OperadType;
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

initialization
  //crea el operador NULL
  nullOper := TOperator.Create;

finalization
  nullOper.Free;

end.

