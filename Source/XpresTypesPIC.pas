{
XpresTypes
==========
Por Tito Hinostroza.

Definiciones básicas para el manejo de expresiones.
Aquí están definidas los objetos claves para el manejo de expresiones:
Los tipos, los operadores y las operaciones
}
unit XpresTypesPIC;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl;

type  //tipos enumerados

  //Grupos de tipo de datos
  TTypeGroup=(
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

  //Categoría de Operando
  TCatOperan = (
    coConst =%00,  //Constante. Inlcuyendo expresiones de constantes evaluadas.
    coVariab=%01,  //Variable. Variable única.
    coExpres=%10   //Expresión. Algo que requiere cálculo (incluyendo a una función).
  );
  {Categoría de operación. Se construye para poder representar dos valores de TCatOperan
   en una solo valor byte (juntando sus bits), para facilitar el uso de un CASE ... OF}
  TCatOperation =(
    coConst_Const  = %0000,
    coConst_Variab = %0001,
    coConst_Expres = %0010,
    coVariab_Const = %0100,
    coVariab_Variab= %0101,
    coVariab_Expres= %0110,
    coExpres_Const = %1000,
    coExpres_Variab= %1001,
    coExpres_Expres= %1010
  );

//  TType = class;

  //Eventos
  TProcExecOperat = procedure of object;
  TProcDefineVar = procedure(const varName, varInitVal: string) of object;
  {Evento para cargar un  operando en la pila.
  "OpPtr" debería ser "TOperand", pero aún no se define "TOperand".}
  TProcLoadOperand = procedure(const OpPtr: pointer) of object;
{
  //Tipo operación
  TxpOperation = class
    OperatType : TType;   //tipo de Operando sobre el cual se aplica la operación.
    proc       : TProcExecOperat;  //Procesamiento de la operación
  end;

  TxpOperations = specialize TFPGObjectList<TxpOperation>; //lista de operaciones
}
  TxpOperatorKind = (
    opkUnaryPre,   //operador Unario Pre
    opkUnaryPost,  //operador Unario Post
    opkBinary      //operador Binario
  );
{
  { TxpOperator }
  //Operador
  TxpOperator = class
  private
    Operations: TxpOperations;  //operaciones soportadas. Debería haber tantos como
                                //Num. Operadores * Num.Tipos compatibles.
  public
    txt  : string;    //cadena del operador '+', '-', '++', ...
    prec : byte;      //precedencia
    name : string;    //nombre de la operación (suma, resta)
    kind : TxpOperatorKind;   //Tipo de operador
    OperationPre: TProcExecOperat;  {Operación asociada al Operador. Usado cuando es un
                                    operador unario PRE. }
    OperationPost: TProcExecOperat; {Operación asociada al Operador. Usado cuando es un
                                    operador unario POST }
    function CreateOperation(OperadType: TType; proc: TProcExecOperat): TxpOperation;  //Crea operación
    function FindOperation(typ0: TType): TxpOperation;  //Busca una operación para este operador
    constructor Create;
    destructor Destroy; override;
  end;

  TxpOperators = specialize TFPGObjectList<TxpOperator>; //lista de operadores
}
  {Evento para llamar al código de procesamiento de un campo.
  "OpPtr" debería ser "TOperand", pero aún no se define "TOperand".}
  TTypFieldProc = procedure(const OpPtr: pointer) of object;

  TTypField = class
    Name : string;  //Nombre del campo
    proc : TTypFieldProc;  //rutina de procesamiento
  end;
  TTypFields = specialize TFPGObjectList<TTypField>;

  { TType }
  {Tipos de datos básicos. Notar que esta calse define a los tipos básicos del lenguaje,
   no a los tipos predefinidos.}
{  TType = class
  public
    name : string;      //Nombre del tipo ("int8", "int16", ...)
    grp  : TTypeGroup;  //Grupo del tipo (numérico, cadena, etc)
    size : smallint;    //Tamaño en bytes del tipo
    procedure SaveToStk;
    procedure DefineRegister;
    function IsSizeBit: boolean;
  public   //Eventos
    {Este evento es llamado automáticamente por el Analizador de expresiones,
     cuando encuentre una expresión de un solo operando, de este tipo.
    Por seguridad, debe implementarse siempre para cada tipo creado. La implementación
    más simple sería devolver en "res", el operando "p1^".}
    OperationLoad: TProcExecOperat; {Evento. Es llamado cuando se pide evaluar una
                                 expresión de un solo operando de este tipo. Es un caso
                                 especial que debe ser tratado por la implementación}
    {Estos eventos NO se generan automáticamente en TCompilerBase, sino que es la implementación, la
     que deberá llamarlos. Son como una ayuda para facilitar la implementación.
     OnPush y OnPop, son útiles para cuando la implementación va a manejar pila.}
    OnSaveToStk: procedure of object;  //Salva datos en reg. de Pila
    OnLoadToReg: TProcLoadOperand; {Se usa cuando se solicita cargar un operando
                                 (de este tipo) en la pila. }
    OnDefineRegister : procedure of object; {Se usa cuando se solicita descargar un operando
                                 (de este tipo) de la pila. }
    OnGlobalDef: TProcDefineVar; {Es llamado cada vez que se encuentra la
                                  declaración de una variable (de este tipo) en el ámbito global.}
  public  //Campos de operadores
    Operators: TxpOperators;      //Operadores soportados
    operAsign: TxpOperator;       //Se guarda una referencia al operador de aignación
    function CreateBinaryOperator(txt: string; prec: byte; OpName: string): TxpOperator;
    function CreateUnaryPreOperator(txt: string; prec: byte; OpName: string;
                                    proc: TProcExecOperat): TxpOperator;
    function CreateUnaryPostOperator(txt: string; prec: byte; OpName: string;
                                     proc: TProcExecOperat): TxpOperator;
    //Funciones de búsqueda
    function FindBinaryOperator(const OprTxt: string): TxpOperator;
    function FindUnaryPreOperator(const OprTxt: string): TxpOperator;
    function FindUnaryPostOperator(const OprTxt: string): TxpOperator;
  public  //Manejo de campos
    fields: TTypFields;   {lista de métodos del tipo. Se define como lista dinámica,
                          para permitir agregar nuevos métodos a los tipos básicos.}
    procedure CreateField(metName: string; proc: TTypFieldProc);
  public   //Inicialización
    constructor Create;
    destructor Destroy; override;
  end;

  //Lista de tipos
  TTypes = specialize TFPGObjectList<TType>; //lista de bloques
}
{
var
  nullOper : TxpOperator; //Operador nulo. Usado como valor cero.
}
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
{
{ TxpOperator }
function TxpOperator.CreateOperation(OperadType: TType; proc: TProcExecOperat
  ): TxpOperation;
var
  r: TxpOperation;
begin
  //agrega
  r := TxpOperation.Create;
  r.OperatType:=OperadType;
  r.proc:=proc;
  //agrega
  operations.Add(r);
  Result := r;
end;
function TxpOperator.FindOperation(typ0: TType): TxpOperation;
{Busca, si encuentra definida, alguna operación, de este operador con el tipo indicado.
Si no lo encuentra devuelve NIL}
var
  r: TxpOperation;
begin
  Result := nil;
  for r in Operations do begin
    if r.OperatType = typ0 then begin
      exit(r);
    end;
  end;
end;
constructor TxpOperator.Create;
begin
  Operations := TxpOperations.Create(true);
end;
destructor TxpOperator.Destroy;
begin
  Operations.Free;
  inherited Destroy;
end;

{ TxpType }
procedure TType.SaveToStk;
begin
  if OnSaveToStk<>nil then OnSaveToStk;
end;
procedure TType.DefineRegister;
begin
  if OnDefineRegister<>nil then OnDefineRegister;
end;
function TType.IsSizeBit: boolean;
{Indica si el tipo tiene un bit de tamaño.}
begin
  Result := size = -1;
end;
function TType.CreateBinaryOperator(txt: string; prec: byte; OpName: string
  ): TxpOperator;
{Permite crear un nuevo ooperador binario soportado por este tipo de datos. Si hubiera
error, devuelve NIL. En caso normal devuelve una referencia al operador creado}
var
  r: TxpOperator;  //operador
begin
  //verifica nombre
  if FindBinaryOperator(txt)<>nullOper then begin
    Result := nil;  //indica que hubo error
    exit;
  end;
  //Crea y configura objeto
  r := TxpOperator.Create;
  r.txt:=txt;
  r.prec:=prec;
  r.name:=OpName;
  r.kind:=opkBinary;
  //Agrega operador
  Operators.Add(r);
  Result := r;
  //Verifica si es el operador de asignación
  if txt = ':=' then begin
    //Lo guarda porque este operador se usa y no vale la pena buscarlo
    operAsign := r;
  end;
end;
function TType.CreateUnaryPreOperator(txt: string; prec: byte; OpName: string;
  proc: TProcExecOperat): TxpOperator;
{Crea operador unario de tipo Pre, para este tipo de dato.}
var
  r: TxpOperator;  //operador
begin
  //Crea y configura objeto
  r := TxpOperator.Create;
  r.txt:=txt;
  r.prec:=prec;
  r.name:=OpName;
  r.kind:=opkUnaryPre;
  r.OperationPre:=proc;
  //Agrega operador
  Operators.Add(r);
  Result := r;
end;
function TType.CreateUnaryPostOperator(txt: string; prec: byte; OpName: string;
  proc: TProcExecOperat): TxpOperator;
{Crea operador binario de tipo Post, para este tipo de dato.}
var
  r: TxpOperator;  //operador
begin
  //Crea y configura objeto
  r := TxpOperator.Create;
  r.txt:=txt;
  r.prec:=prec;
  r.name:=OpName;
  r.kind:=opkUnaryPost;
  r.OperationPost:=proc;
  //Agrega operador
  Operators.Add(r);
  Result := r;
end;

function TType.FindBinaryOperator(const OprTxt: string): TxpOperator;
{Recibe el texto de un operador y devuelve una referencia a un objeto TxpOperator, del
tipo. Si no está definido el operador para este tipo, devuelve nullOper.}
var
  oper: TxpOperator;
begin
  Result := nullOper;   //valor por defecto
  for oper in Operators do begin
    if (oper.kind = opkBinary) and (oper.txt = upCase(OprTxt)) then begin
      exit(oper); //está definido
    end;
  end;
  //no encontró
  Result.txt := OprTxt;    //para que sepa el operador leído
end;
function TType.FindUnaryPreOperator(const OprTxt: string): TxpOperator;
{Recibe el texto de un operador unario Pre y devuelve una referencia a un objeto
TxpOperator, del tipo. Si no está definido el operador para este tipo, devuelve nullOper.}
var
  oper: TxpOperator;
begin
  Result := nullOper;   //valor por defecto
  for oper in Operators do begin
    if (oper.kind = opkUnaryPre) and (oper.txt = upCase(OprTxt)) then begin
      exit(oper); //está definido
    end;
  end;
  //no encontró
  Result.txt := OprTxt;    //para que sepa el operador leído
end;
function TType.FindUnaryPostOperator(const OprTxt: string): TxpOperator;
{Recibe el texto de un operador unario Post y devuelve una referencia a un objeto
TxpOperator, del tipo. Si no está definido el operador para este tipo, devuelve nullOper.}
var
  oper: TxpOperator;
begin
  Result := nullOper;   //valor por defecto
  for oper in Operators do begin
    if (oper.kind = opkUnaryPost) and (oper.txt = upCase(OprTxt)) then begin
      exit(oper); //está definido
    end;
  end;
  //no encontró
  Result.txt := OprTxt;    //para que sepa el operador leído
end;

procedure TType.CreateField(metName: string; proc: TTypFieldProc);
{Crea un campo para un tipo. Algo similar a un método, propiedad o Type helper.}
var
  fun : TTypField;
begin
  fun := TTypField.Create;  //Se crea como una función normal
  fun.Name := metName;
  fun.proc := proc;
//no verifica duplicidad
  fields.Add(fun);
end;
constructor TType.Create;
begin
  Operators := TxpOperators.Create(true);  //crea contenedor de Contextos, con control de objetos.
  fields:= TTypFields.Create(true);
end;
destructor TType.Destroy;
begin
  fields.Destroy;
  Operators.Free;
  inherited Destroy;
end;
}
{initialization
  //crea el operador NULL
  nullOper := TxpOperator.Create;

finalization
  nullOper.Free;}

end.

