{
XpresTypes
==========
Por Tito Hinostroza.

Definiciones básicas para el manejo de elementos que representan a tipos.
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

  //Eventos
  TProcExecOperat = procedure of object;
  TProcDefineVar = procedure(const varName, varInitVal: string) of object;
  {Evento para cargar un  operando en la pila.
  "OpPtr" debería ser "TOperand", pero aún no se define "TOperand".}
  TProcLoadOperand = procedure(const OpPtr: pointer) of object;

  TxpOperatorKind = (
    opkUnaryPre,   //operador Unario Pre
    opkUnaryPost,  //operador Unario Post
    opkBinary      //operador Binario
  );
  {Evento para llamar al código de procesamiento de un campo.
  "OpPtr" debería ser "TOperand", pero aún no se define "TOperand".}
  TTypFieldProc = procedure(const OpPtr: pointer) of object;

  TTypField = class
    Name : string;  //Nombre del campo
    proc : TTypFieldProc;  //rutina de procesamiento
  end;
  TTypFields = specialize TFPGObjectList<TTypField>;

implementation

end.

