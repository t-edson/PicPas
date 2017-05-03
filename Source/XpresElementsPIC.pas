{
XpresElements
=============
Definiciones para el manejo de los elementos del compilador: funciones, constantes, variables.
Todos estos elementos se deberían almacenar en una estrucutura de arbol.
Esta unidad es similar a la de Xpres, solamente contiene cambios menores.
Por Tito Hinostroza.
}
unit XpresElementsPIC;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, fgl, XpresTypes, XpresBas;
const
  ADRR_ERROR = $FFFF;
type
  {Estos tipos están relacionados con el hardware, y tal vez deberían estar declarados
  en otra unidad. Pero se ponen aquí porque son pocos.
  La idea es que sean simples contenedores de direcciones físicas. En un inicio se pensó
  declararlos como RECORD por velocidad (para no usar memoria dinámica), pero dado que no
  se tienen requerimientos altos de velocidad en PicPas, se delcaran como clases. }
  //Tipo de registro
  TPicRegType = (prtWorkReg,   //de trabajo
                 prtAuxReg,    //auxiliar
                 prtStkReg     //registro de pila
  );
  { TPicRegister }
  {Objeto que sirve para modelar a un registro del PIC (una dirección de memoria, usada
   para un fin particular)}
  TPicRegister = class
  public
    assigned: boolean;  //indica si tiene una dirección física asignada
    used   : boolean;   //Indica si está usado.
    offs   : byte;      //Desplazamiento en memoria
    bank   : byte;      //Banco del registro
    typ    : TPicRegType; //Tipo de registro
    function AbsAdrr: word;  //Diección absoluta
  end;
  TPicRegister_list = specialize TFPGObjectList<TPicRegister>; //lista de registros

  { TPicRegisterBit }
  {Objeto que sirve para modelar a un bit del PIC (una dirección de memoria, usada
   para un fin particular)}
  TPicRegisterBit = class
  public
    assigned: boolean;  //indica si tiene una dirección física asignada
    used   : boolean;   //Indica si está usado.
    offs   : byte;      //Desplazamiento en memoria
    bank   : byte;      //Banco del registro
    bit    : byte;      //bit del registro
    typ    : TPicRegType; //Tipo de registro
    function AbsAdrr: word;  //Diección absoluta
  end;
  TPicRegisterBit_list = specialize TFPGObjectList<TPicRegisterBit>; //lista de registros

var
  /////// Tipos de datos del lenguaje ////////////
  typNull: TType;     //Tipo nulo (sin tipo)
  typBit : TType;
  typBool: TType;     //Booleanos
  typByte: TType;     //número sin signo
  typWord: TType;     //número sin signo
//  tipChr : Ttype;   //un caracter

type
  //Tipos de elementos del lenguaje
  TxpElemType = (eltNone,  //sin tipo
                 eltMain,  //programa principal
                 eltVar,   //variable
                 eltFunc,  //función
                 eltCons,  //constante
                 eltType   //tipo
                );
  TFindFuncResult = (TFF_NONE, TFF_PARTIAL, TFF_FULL);

  TxpElement = class;
  TxpElements = specialize TFPGObjectList<TxpElement>;

  { TxpElement }
  //Clase base para todos los elementos
  TxpElement = class
  public
  public
    name : string;       //nombre de la variable
    typ  : TType;        //tipo del elemento, si aplica
    Parent: TxpElement;    //referencia al padre
    elemType: TxpElemType; //no debería ser necesario
//    Used: integer;       //veces que se usa este nombre
    elements: TxpElements;  //referencia a nombres anidados, cuando sea función
    function AddElement(elem: TxpElement): TxpElement;
    function DuplicateIn(list: TObject): boolean; virtual;
    function FindIdxElemName(const eName: string; var idx0: integer): boolean;
    constructor Create; virtual;
    destructor Destroy; override;
  public  //Ubicación física de la declaración del elmento
    posCtx: TPosCont;  //Ubicación en el código fuente
    {Datos de la ubicación en el código fuente. Guardan parte de la información de,
    posCtx, pero se mantienen, aún después de cerrar los contextos de entrada.}
    srcFile: string;   //archivo donde se encuentra la declaración
    srcRow : integer;  //número de línea de la declaración
    srcCol : integer;  //número de columna de la declaración
  end;

  TVarOffs = word;
  TVarBank = byte;

  //Clase para modelar al bloque principal
  { TxpEleMain }
  TxpEleMain = class(TxpElement)
    //Como este nodo representa al programa principal, se incluye información física
    adrr   : integer;  //dirección física
    srcSize: integer;  {Tamaño del código compilado. En la primera pasada, es referencial,
                        porque el tamaño puede variar al reubicarse.}
    constructor Create; override;
  end;

  { TxpEleType }
  //Clase para modelar a los tipos definidos por el usuario
  { Es diferente a XpresTypes: TType, aunque no se ha hecho un anaálisis profundo }
  TxpEleType= class(TxpElement)
    //valores de la constante
    constructor Create; override;
  end;
  TxpEleTypes= specialize TFPGObjectList<TxpEleType>; //lista de variables

  { TxpEleCon }
  //Clase para modelar a las constantes
  TxpEleCon = class(TxpElement)
    //valores de la constante
    val : TConsValue;
    constructor Create; override;
  end;
  TxpEleCons = specialize TFPGObjectList<TxpEleCon>; //lista de constantes

  { TxpEleVar }
  //Clase para modelar a las variables
  TxpEleVar = class(TxpElement)
    nCalled: integer;  //Número de veces que es llamada la variable
    {Campos de dirección solicitadas en la declaración de la variable, cuando es
    Absoluta. Si no se usan, se ponen en -1}
    solAdr : integer;    //dirección absoluta.
    solBit : shortint;   //posición del bit
    //Campos para guardar las direcciones físicas asignadas en RAM.
    adrBit : TPicRegisterBit;  //Dirección física, cuando es de tipo Bit/Boolean
    adrByte0: TPicRegister;   //Dirección física, cuando es de tipo Byte/Word
    adrByte1: TPicRegister;   //Dirección física, cuando es de tipo Word
    function AbsAddr : word;   //Devuelve la dirección absoluta de la variable
    function AbsAddrL: word;   //Devuelve la dirección absoluta de la variable (LOW)
    function AbsAddrH: word;   //Devuelve la dirección absoluta de la variable (HIGH)
    function AddrString: string;  //Devuelve la dirección física como cadena
    function BitMask: byte;  //Máscara de bit, de acuerdo al valor del campo "bit".
    procedure ResetAddress;  //Limpia las direcciones físicas
    constructor Create; override;
    destructor Destroy; override;
  end;
  TxpEleVars = specialize TFPGObjectList<TxpEleVar>; //lista de variables

  { TxpEleFun }
  //Clase para almacenar información de las funciones
  TxpEleFun = class;
  TProcExecFunction = procedure(fun: TxpEleFun) of object;  //con índice de función
  TxpEleFun = class(TxpElement)
  private
    pars: array of TType;  //parámetros de entrada
  public
    nCalled: integer;  //Número de veces que es llamada la función
    //Direción física.
    adrr: integer;     //Dirección física
    srcSize: integer;  {Tamaño del código compilado. En la primera pasada, es referencial,
                        porque el tamaño puede variar al reubicarse.}
    {Referencia a la función que implementa, la llamada a la función en ensambaldor.
    En funciones del sistema, puede que se implemente INLINE, sin llamada a subrutinas,
    pero en las funciones comunes, siempre usa CALL ... }
    procCall: TProcExecFunction;
    {Método que llama a una rutina que codificará la rutina ASM que implementa la función.
     La idea es que este campo solo se use para algunas funciones del sistema.}
    compile: TProcExecFunction;
    ///////////////
    procedure ClearParams;
    procedure CreateParam(parName: string; typ0: TType);
    function SameParams(Fun2: TxpEleFun): boolean;
    function ParamTypesList: string;
    function DuplicateIn(list: TObject): boolean; override;
    constructor Create; override;
  end;
  TxpEleFuns = specialize TFPGObjectList<TxpEleFun>;

  { TXpTreeElements }
  {Árbol de elementos. Solo se espera que haya una instacia de este objeto. Aquí es
  donde se guardará la referencia a todas los elementos (variables, constantes, ..)
  creados.
  Este árbol se usa también como un equivalente al NameSpace, porque se usa para
  buscar los nombres de los elementos, en una estructura en arbol}
  TXpTreeElements = class
  private
    curNode : TxpElement;  //referencia al nodo actual
    vars    : TxpEleVars;
    //variables de estado para la búsqueda con FindFirst() - FindNext()
    curFindName: string;
    curFindNode: TxpElement;
    curFindIdx: integer;
  public
    main    : TxpEleMain;  //nodo raiz
    procedure Clear;
    function AllVars: TxpEleVars;
    function CurNodeName: string;
    //funciones para llenado del arbol
    function AddElement(elem: TxpElement; verifDuplic: boolean=true): boolean;
    procedure OpenElement(elem: TxpElement);
    function ValidateCurElement: boolean;
    procedure CloseElement;
    //Métodos para identificación de nombres
    function FindNext: TxpElement;
    function FindFirst(const name: string): TxpElement;
    function FindNextFunc: TxpEleFun;
    function FindVar(varName: string): TxpEleVar;
  public  //constructor y destructror
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TPicRegister }
function TPicRegister.AbsAdrr: word;
begin
  Result := bank * $80 + offs;
end;

{ TPicRegisterBit }
function TPicRegisterBit.AbsAdrr: word;
begin
  Result := bank * $80 + offs;
end;

{ TxpElement }
function TxpElement.AddElement(elem: TxpElement): TxpElement;
{Agrega un elemento hijo al elemento actual. Devuelve referencia. }
begin
  elem.Parent := self;  //actualzia referencia
  elements.Add(elem);   //agrega a la lista de nombres
  Result := elem;       //no tiene mucho sentido
end;
function TxpElement.DuplicateIn(list: TObject): boolean;
{Debe indicar si el elemento está duplicado en la lista de elementos proporcionada.}
var
  uName: String;
  ele: TxpElement;
begin
  uName := upcase(name);
  for ele in TxpElements(list) do begin
    if upcase(ele.name) = uName then begin
      exit(true);
    end;
  end;
  exit(false);
end;
function TxpElement.FindIdxElemName(const eName: string; var idx0: integer): boolean;
{Busca un nombre en su lista de elementos. Inicia buscando en idx0, hasta el final.
 Si encuentra, devuelve TRUE y deja en idx0, la posición en donde se encuentra.}
var
  i: Integer;
  uName: String;
begin
  uName := upcase(eName);
  //empieza la búsqueda en "idx0"
  for i := idx0 to elements.Count-1 do begin
    { TODO : Tal vez sería mejor usar el método de búsqueda interno de la lista,
     que es más rápido, pero trabaja con listas ordenadas. }
    if upCase(elements[i].name) = uName then begin
      //sale dejando idx0 en la posición encontrada
      idx0 := i;
      exit(true);
    end;
  end;
  exit(false);
end;
constructor TxpElement.Create;
begin
  elemType := eltNone;
end;
destructor TxpElement.Destroy;
begin
  elements.Free;  //por si contenía una lista
  inherited Destroy;
end;
{ TxpEleMain }
constructor TxpEleMain.Create;
begin
  elemType:=eltMain;
  Parent := nil;  //la raiz no tiene padre
end;
{ TxpEleCon }
constructor TxpEleCon.Create;
begin
  elemType:=eltCons;
end;
function TxpEleVar.AbsAddr: word;
{Devuelve la dirección absoluta de la variable. Tener en cuenta que la variable, no
siempre tiene un solo byte, así que se trata de devolver siempre la dirección del
byte de menor peso.}
begin
  if (typ = typBit) or (typ = typBool) then begin
    Result := adrBit.AbsAdrr;
  end else if typ = typByte then begin
    Result := adrByte0.AbsAdrr;
  end else if typ = typWord then begin
    Result := adrByte0.AbsAdrr;
  end else begin
    Result := ADRR_ERROR;
  end;
end;
function TxpEleVar.AbsAddrL: word;
{Dirección absoluta de la variable de menor pero, cuando es de tipo WORD.}
begin
  if typ = typWord then begin
    Result := adrByte0.AbsAdrr;
  end else begin
    Result := ADRR_ERROR;
  end;
end;
function TxpEleVar.AbsAddrH: word;
{Dirección absoluta de la variable de mayor pero, cuando es de tipo WORD.}
begin
  if typ = typWord then begin
    Result := adrByte1.AbsAdrr;
  end else begin
    Result := ADRR_ERROR;
  end;
end;
function TxpEleVar.AddrString: string;
{Devuelve una cadena, que representa a la dirección física.}
begin
  if (typ = typBit) or (typ = typBool) then begin
    Result := 'bnk'+ IntToStr(adrBit.bank) + ':$' + IntToHex(adrBit.offs, 3) + '.' + IntToStr(adrBit.bit);
  end else if typ = typByte then begin
    Result := 'bnk'+ IntToStr(adrByte0.bank) + ':$' + IntToHex(adrByte0.offs, 3);
  end else if typ = typWord then begin
    Result := 'bnk'+ IntToStr(adrByte0.bank) + ':$' + IntToHex(adrByte0.offs, 3);
  end else begin
    Result := '';   //Error
  end;
end;
function TxpEleVar.BitMask: byte;
{Devuelve la máscara, de acuerdo a su valor de "bit".}
begin
    case adrBit.bit of
    0: Result := %00000001;
    1: Result := %00000010;
    2: Result := %00000100;
    3: Result := %00001000;
    4: Result := %00010000;
    5: Result := %00100000;
    6: Result := %01000000;
    7: Result := %10000000;
    end;
end;
procedure TxpEleVar.ResetAddress;
begin
  adrBit.bank := 0;
  adrBit.offs := 0;
  adrBit.bit := 0;

  adrByte0.bank := 0;
  adrByte0.offs := 0;

  adrByte1.bank := 0;
  adrByte1.offs := 0;
end;
{ TxpEleVar }
constructor TxpEleVar.Create;
begin
  elemType:=eltVar;
  adrBit:= TPicRegisterBit.Create;  //
  adrByte0:= TPicRegister.Create;
  adrByte1:= TPicRegister.Create;
end;
destructor TxpEleVar.Destroy;
begin
  adrByte0.Destroy;
  adrByte1.Destroy;
  adrBit.Destroy;
  inherited Destroy;
end;
{ TxpEleType }
constructor TxpEleType.Create;
begin
  elemType:=eltType;
end;
{ TxpEleFun }
procedure TxpEleFun.ClearParams;
//Elimina los parámetros de una función
begin
  setlength(pars,0);
end;
procedure TxpEleFun.CreateParam(parName: string; typ0: ttype);
//Crea un parámetro para la función
var
  n: Integer;
begin
  //agrega
  n := high(pars)+1;
  setlength(pars, n+1);
  pars[n] := typ0;  //agrega referencia
end;
function TxpEleFun.SameParams(Fun2: TxpEleFun): boolean;
{Compara los parámetros de la función con las de otra. Si tienen el mismo número
de parámetros y el mismo tipo, devuelve TRUE.}
var
  i: Integer;
begin
  Result:=true;  //se asume que son iguales
  if High(pars) <> High(Fun2.pars) then
    exit(false);   //distinto número de parámetros
  //hay igual número de parámetros, verifica
  for i := 0 to High(pars) do begin
    if pars[i] <> Fun2.pars[i] then begin
      exit(false);
    end;
  end;
  //si llegó hasta aquí, hay coincidencia, sale con TRUE
end;
function TxpEleFun.ParamTypesList: string;
{Devuelve una lista con los nombres de los tipos de los parámetros, de la forma:
(byte, word) }
var
  tmp: String;
  j: Integer;
begin
  tmp := '';
  for j := 0 to High(pars) do begin
    tmp += pars[j].name+', ';
  end;
  //quita coma final
  if length(tmp)>0 then tmp := copy(tmp,1,length(tmp)-2);
  Result := '('+tmp+')';
end;
function TxpEleFun.DuplicateIn(list: TObject): boolean;
var
  uName: String;
  ele: TxpElement;
begin
  uName := upcase(name);
  for ele in TxpElements(list) do begin
    if ele = self then Continue;  //no se compara el mismo
    if upcase(ele.name) = uName then begin
      //hay coincidencia de nombre
      if ele.elemType = eltFunc then begin
        //para las funciones, se debe comparar los parámetros
        if SameParams(TxpEleFun(ele)) then begin
          exit(true);
        end;
      end else begin
        //si tiene el mismo nombre que cualquier otro elemento, es conflicto
        exit(true);
      end;
    end;
  end;
  exit(false);
end;
constructor TxpEleFun.Create;
begin
  elemType:=eltFunc;
end;
{ TXpTreeElements }
procedure TXpTreeElements.Clear;
begin
  main.elements.Clear;  //esto debe hacer un borrado recursivo
  curNode := main;      //retorna al nodo principal
end;
function TXpTreeElements.AllVars: TxpEleVars;
{Devuelve una lista de todas las variables usadas, incluyendo las de las funciones y
 procedimientos.}
  procedure AddVars(nod: TxpElement);
  var
    ele : TxpElement;
  begin
    if nod.elements<>nil then begin
      for ele in nod.elements do begin
        if ele.elemType = eltVar then begin
          vars.Add(TxpEleVar(ele));
        end else begin
          if ele.elements<>nil then
            AddVars(ele);  //recursivo
        end;
      end;
    end;
  end;
begin
  if vars = nil then begin  //debe estar creada la lista
    vars := TxpEleVars.Create(false);
  end else begin
    vars.Clear;   //por si estaba llena
  end;
  AddVars(curNode);
  Result := vars;
end;
function TXpTreeElements.CurNodeName: string;
{Devuelve el nombre del nodo actual}
begin
  Result := curNode.name;
end;
//funciones para llenado del arbol
function TXpTreeElements.AddElement(elem: TxpElement; verifDuplic: boolean = true): boolean;
{Agrega un elemento al nodo actual. Si ya existe el nombre del nodo, devuelve false}
begin
  Result := true;
  //Verifica si hay conflicto. Solo es necesario buscar en el nodo actual.
  if verifDuplic and elem.DuplicateIn(curNode.elements) then begin
    exit(false);  //ya existe
  end;
  //agrega el nodo
  curNode.AddElement(elem);
end;
procedure TXpTreeElements.OpenElement(elem: TxpElement);
{Agrega un elemento y cambia el nodo actual. Este método está reservado para
las funciones o procedimientos}
begin
  {las funciones o procedimientos no se validan inicialmente, sino hasta que
  tengan todos sus parámetros agregados, porque pueden ser sobrecargados.}
  curNode.AddElement(elem);
  //Genera otro espacio de nombres
  elem.elements := TxpElements.Create(true);  //su propia lista
  curNode := elem;  //empieza a trabajar en esta lista
end;
function TXpTreeElements.ValidateCurElement: boolean;
{Este método es el complemento de OpenElement(). Se debe llamar cuando ya se
 tienen creados los parámetros de la función o procedimiento, para verificar
 si hay duplicidad, en cuyo caso devolverá FALSE}
begin
  //Se asume que el nodo a validar ya se ha abierto, con OpenElement() y es el actual
  if curNode.DuplicateIn(curNode.Parent.elements) then begin  //busca en el nodo anterior
    exit(false);
  end else begin
    exit(true);
  end;
end;
procedure TXpTreeElements.CloseElement;
{Sale del nodo actual y retorna al nodo padre}
begin
  if curNode.Parent<>nil then
    curNode := curNode.Parent;
end;
//Métodos para identificación de nombres
function TXpTreeElements.FindNext: TxpElement;
{Realiza una búsqueda recursiva en el nodo "curFindNode", a partir de la posición,
"curFindIdx", el elemento con nombre "curFindName"}
begin
  if curFindNode.FindIdxElemName(curFindName, curFindIdx) then begin
    //Lo encontró, asigna resultado
    Result := curFindNode.elements[curFindIdx];
    //Deja estado listo para la siguiente búsqueda
    curFindIdx := curFindIdx + 1;
    exit;
  end else begin
    //No encontró
    if curFindNode.Parent = nil then begin
      Result := nil;
      exit;  //no hay espacios padres
    end;
    //busca en el espacio padre
    curFindNode := curFindNode.Parent;  //apunta al padre
    curFindIdx := 0;  //empieza desde el inicio
    Result := FindNext();  //Recursividad IMPORTANTE: Usar paréntesis.
    exit;
  end;
end;
function TXpTreeElements.FindFirst(const name: string): TxpElement;
{Busca un nombre siguiendo la estructura del espacio de nombres (primero en el espacio
 actual y luego en los espacios padres).
 Si encuentra devuelve la referencia. Si no encuentra, devuelve NIL}
begin
  //Busca recursivamente, a partir del espacio actual
  curFindNode := curNode;  //Actualiza nodo actual de búsqueda
  curFindName := name;  //Esta valor no cambairá en toda la búsqueda
  curFindIdx := 0;      //Busca desde el inicio
  Result := FindNext;
end;
function TXpTreeElements.FindNextFunc: TxpEleFun;
{Explora recursivamente haciá la raiz, en el arbol de sintaxis, hasta encontrar el nombre
de la fución indicada. Debe llamarse después de FindFirst().
Si no enecuentra devuelve NIL.}
var
  ele: TxpElement;
begin
  repeat
    ele := FindNext;
  until (ele=nil) or (ele is TxpEleFun);
  //Puede que haya encontrado la función o no
  if ele = nil then exit(nil);  //No encontró
  Result := TxpEleFun(ele);   //devuelve como función
end;
function TXpTreeElements.FindVar(varName: string): TxpEleVar;
{Busca una variable con el nombre indicado en el espacio de nombres actual}
var
  ele : TxpElement;
  uName: String;
begin
  uName := upcase(varName);
  for ele in curNode.elements do begin
    if (ele.elemType = eltVar) and (upCase(ele.name) = uName) then begin
      Result := TxpEleVar(ele);
      exit;
    end;
  end;
  exit(nil);
end;
//constructor y destructror
constructor TXpTreeElements.Create;
begin
  main:= TxpEleMain.Create;  //No debería
  main.elements := TxpElements.Create(true);  //debe tener lista
  curNode := main;  //empieza con el nodo principal como espacio de nombres actual
end;
destructor TXpTreeElements.Destroy;
begin
  main.Destroy;
  vars.Free;    //por si estaba creada
  inherited Destroy;
end;
end.

