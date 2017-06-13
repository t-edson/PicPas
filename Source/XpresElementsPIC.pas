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
  Classes, SysUtils, fgl, XpresTypesPIC, XpresBas, LCLProc;
const
  ADRR_ERROR = $FFFF;
type
  {Estos tipos están relacionados con el hardware, y tal vez deberían estar declarados
  en otra unidad. Pero se ponen aquí porque son pocos.
  La idea es que sean simples contenedores de direcciones físicas. En un inicio se pensó
  declararlos como RECORD por velocidad (para no usar memoria dinámica), pero dado que no
  se tienen requerimientos altos de velocidad en PicPas, se declaran como clases. }
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
    offs   : byte;      //Desplazamiento en memoria
    bank   : byte;      //Banco del registro
    assigned: boolean;  //indica si tiene una dirección física asignada
    used   : boolean;   //Indica si está usado.
    typ    : TPicRegType; //Tipo de registro
    function AbsAdrr: word;  //Diección absoluta
    procedure Assign(srcReg: TPicRegister);
  end;
  TPicRegister_list = specialize TFPGObjectList<TPicRegister>; //lista de registros

  { TPicRegisterBit }
  {Objeto que sirve para modelar a un bit del PIC (una dirección de memoria, usada
   para un fin particular)}
  TPicRegisterBit = class
  public
    offs   : byte;      //Desplazamiento en memoria
    bank   : byte;      //Banco del registro
    bit    : byte;      //bit del registro
    assigned: boolean;  //indica si tiene una dirección física asignada
    used   : boolean;   //Indica si está usado.
    typ    : TPicRegType; //Tipo de registro
    function AbsAdrr: word;  //Diección absoluta
    procedure Assign(srcReg: TPicRegisterBit);
  end;
  TPicRegisterBit_list = specialize TFPGObjectList<TPicRegisterBit>; //lista de registros

var
  /////// Tipos de datos del lenguaje ////////////
  typNull: TType;     //Tipo nulo (sin tipo) Usado para procedimientos
  typBit : TType;
  typBool: TType;     //Booleanos
  typByte: TType;     //número sin signo
  typWord: TType;     //número sin signo
  typChar: TType;     //caracter individual
//  tipChr : Ttype;   //un caracter

type

  //Tipos de elementos del lenguaje
  TxpElemType = (eltNone,  //sin tipo
                 eltMain,  //programa principal
                 eltVar,   //variable
                 eltFunc,  //función
                 eltCons,  //constante
                 eltType,  //tipo
                 eltUnit,  //unidad
                 elBody    //cuerpo del programa
                );

  TxpElement = class;
  TxpElements = specialize TFPGObjectList<TxpElement>;

  TxpEleBody = class;

  //Datos sobre la llamada a un elemento desde otro elemento
  TxpEleCaller = class
    curBnk: byte;       //banco RAM, desde donde se llama
    caller: TxpElement; //función que llama a esta función
  end;
  TxpListCallers = specialize TFPGObjectList<TxpEleCaller>;

  { TxpElement }
  //Clase base para todos los elementos
  TxpElement = class
  private
    function AddElement(elem: TxpElement): TxpElement;
  public  //Gestion de llamadas al elemento
    lstCallers: TxpListCallers;  //Lista de funciones que llaman a esta función.
    OnAddCaller: procedure(elem: TxpElement) of object;
    procedure AddCaller;
    function nCalled: integer;  //número de llamadas
    function IsCalledBy(callElem: TxpElement): boolean; //Identifica a un llamador
    function FindCalling(callElem: TxpElement): TxpEleCaller; //Identifica a un llamada
    function RemoveCallsFrom(callElem: TxpElement): integer; //Elimina llamadas
    procedure ClearCallers;  //limpia lista de llamantes
    function DuplicateIn(list: TxpElements): boolean; virtual;
  public
    name : string;        //nombre de la variable
    typ  : TType;         //tipo del elemento, si aplica
    Parent: TxpElement;   //referencia al padre
    elemType: TxpElemType; //no debería ser necesario
    elements: TxpElements; //referencia a nombres anidados, cuando sea función
    function FindIdxElemName(const eName: string; var idx0: integer): boolean;
    function LastNode: TxpElement;
    function BodyNode: TxpEleBody;
    function Index: integer;
    constructor Create; virtual;
    destructor Destroy; override;
  public  //Ubicación física de la declaración del elmento
    posCtx: TPosCont;  //Ubicación en el código fuente
    {Datos de la ubicación en el código fuente, donde el elemento es declarado. Guardan
    parte de la información de posCtx, pero se mantiene, aún después de cerrar los
    contextos de entrada.}
    srcDec: TSrcPos;
  end;

  TVarOffs = word;
  TVarBank = byte;

  //Clase para modelar al bloque principal
  { TxpEleMain }
  TxpEleMain = class(TxpElement)
    //Como este nodo representa al programa principal, se incluye información física
    srcSize: integer;  {Tamaño del código compilado. En la primera pasada, es referencial,
                        porque el tamaño puede variar al reubicarse.}
    constructor Create; override;
  end;

  { TxpEleType }
  //Clase para modelar a los tipos definidos por el usuario
  { Es diferente a XpresTypesPIC.TType, aunque no se ha hecho un análisis profundo }
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
    {Campos de dirección solicitadas en la declaración de la variable, cuando es
    Absoluta. Si no se usan, se ponen en -1}
    solAdr : integer;    //dirección absoluta.
    solBit : shortint;   //posición del bit
    {Bandera para indicar si la variable, ha sido declarada en la sección INTERFACE. Este
    campo es úitl para cuando se procesan unidades.}
    InInterface: boolean;
    //Bandera para indicar si la variable, se está usando como parámetro
    IsParameter: boolean;
    {Bandera para indicar que el valor de la variable se alamcena en lso registros de
    trabajo, es decir que se manejan, más como expresión que como variables. Se diseñó,
    como una forma rápida para pasar parámetros a funciones.}
    IsRegister: boolean;
    //Campos para guardar las direcciones físicas asignadas en RAM.
    adrBit : TPicRegisterBit;  //Dirección física, cuando es de tipo Bit/Boolean
    adrByte0: TPicRegister;   //Dirección física, cuando es de tipo Byte/Char/Word
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

  //Parámetro de una función
  TxpParFunc = record
    name: string;    //nombre de parámetro
    typ : TType;     //tipo del parñametro
    pvar: TxpEleVar; //referecnia a la variable que se usa para el parámetro
  end;

  TxpEleFun = class;
  { TxpEleFun }
  //Clase para almacenar información de las funciones
  TProcExecFunction = procedure(fun: TxpEleFun) of object;  //con índice de función
  TxpEleFun = class(TxpElement)
  public
    pars: array of TxpParFunc;  //parámetros de entrada
    adrr: integer;     //Dirección física, en donde se compila
    adrReturn: integer;  //Dirección física del RETURN final de la función.
    srcSize: integer;  {Tamaño del código compilado. En la primera pasada, es referencial,
                        porque el tamaño puede variar al reubicarse.}
    //Banco de RAM, al iniciar la eejcución de la subrutina.
    iniBnk: byte;
    {Bsndera que indica si se produce cambio de banco desde dentro del código de la
    función.}
    BankChanged: boolean;
    {Referencia a la función que implemanta, la rutina de porcesamiento que se debe
    hacer, antes de empezar a leer los parámetros de la función.}
    procParam: TProcExecFunction;
    {Referencia a la función que implementa, la llamada a la función en ensamblador.
    En funciones del sistema, puede que se implemente INLINE, sin llamada a subrutinas,
    pero en las funciones comunes, siempre usa CALL ... }
    procCall: TProcExecFunction;
    {Método que llama a una rutina que codificará la rutina ASM que implementa la función.
     La idea es que este campo solo se use para algunas funciones del sistema.}
    compile: TProcExecFunction;
    {Bandera para indicar si la función, ha sido implementada. Este campo es util, para
     cuando se usa FORWARD o cuando se compilan unidades.}
    Implemented: boolean;
    {Bandera para indicar si la función, ha sido declarada en la sección INTERFACE. Este
    campo es úitl para cuando se procesan unidades.}
    InInterface: boolean;
    ///////////////
    procedure ClearParams;
    procedure CreateParam(parName: string; typ0: ttype; pvar: TxpEleVar);
    function SameParams(Fun2: TxpEleFun): boolean;
    function ParamTypesList: string;
    function DuplicateIn(list: TxpElements): boolean; override;
    procedure SetElementsUnused;
    constructor Create; override;
  end;
  TxpEleFuns = specialize TFPGObjectList<TxpEleFun>;

  { TxpEleUnit }
  //Clase para modelar a las constantes
  TxpEleUnit = class(TxpElement)
    constructor Create; override;
  end;
  TxpEleUnits = specialize TFPGObjectList<TxpEleUnit>; //lista de constantes

  //Clase para modelar al cuerpo principal del programa

  { TxpEleBody }

  TxpEleBody = class(TxpElement)
    adrr   : integer;  //dirección física
    constructor Create; override;
  end;

  { TXpTreeElements }
  {Árbol de elementos. Solo se espera que haya una instacia de este objeto. Aquí es
  donde se guardará la referencia a todas los elementos (variables, constantes, ..)
  creados.
  Este árbol se usa también como un equivalente al NameSpace, porque se usa para
  buscar los nombres de los elementos, en una estructura en arbol}
  TXpTreeElements = class
  private
    //variables de estado para la búsqueda con FindFirst() - FindNext()
    curFindName: string;
    curFindNode: TxpElement;
    curFindIdx : integer;
    inUnit     : boolean;
  public
    main    : TxpEleMain;  //nodo raiz
    curNode : TxpElement;  //referencia al nodo actual
    AllVars    : TxpEleVars;
    AllFuncs   : TxpEleFuns;
    OnAddElement: procedure(xpElem: TxpElement) of object;  //Evento
    procedure Clear;
    procedure RefreshAllVars;
    procedure RefreshAllFuncs;
    function CurNodeName: string;
    function LastNode: TxpElement;
    function BodyNode: TxpEleBody;
    //funciones para llenado del arbol
    function AddElement(elem: TxpElement; verifDuplic: boolean=true): boolean;
    procedure AddElementAndOpen(elem: TxpElement);
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
procedure TPicRegister.Assign(srcReg: TPicRegister);
begin
  offs    := srcReg.offs;
  bank    := srcReg.bank;
  assigned:= srcReg.assigned;
  used    := srcReg.used;
  typ     := srcReg.typ;
end;
{ TPicRegisterBit }
function TPicRegisterBit.AbsAdrr: word;
begin
  Result := bank * $80 + offs;
end;
procedure TPicRegisterBit.Assign(srcReg: TPicRegisterBit);
begin
  offs    := srcReg.offs;
  bank    := srcReg.bank;
  bit     := srcReg.bit;
  assigned:= srcReg.assigned;
  used    := srcReg.used;
  typ     := srcReg.typ;
end;

{ TxpElement }
function TxpElement.AddElement(elem: TxpElement): TxpElement;
{Agrega un elemento hijo al elemento actual. Devuelve referencia. }
begin
  elem.Parent := self;  //actualzia referencia
  elements.Add(elem);   //agrega a la lista de nombres
  Result := elem;       //no tiene mucho sentido
end;
function TxpElement.FindIdxElemName(const eName: string; var idx0: integer): boolean;
{Busca un nombre en su lista de elementos. Inicia buscando desde idx0, hasta el inicio.
 Si encuentra, devuelve TRUE y deja en idx0, la posición en donde se encuentra.}
var
  i: Integer;
  uName: String;
begin
  uName := upcase(eName);
  //empieza la búsqueda en "idx0"
  for i := idx0 downto 0 do begin
    if upCase(elements[i].name) = uName then begin
      //sale dejando idx0 en la posición encontrada
      idx0 := i;
      exit(true);
    end;
  end;
  exit(false);
end;
function TxpElement.LastNode: TxpElement;
{Devuelve una referencia al último nodo de "elements"}
begin
  if elements = nil then exit(nil);
  if elements.Count = 0 then exit(nil);
  Result := elements[elements.Count-1];
end;
function TxpElement.BodyNode: TxpEleBody;
{Devuelve la referecnia al cuerpo del programa. Aplicable a nodos de tipo función o
"Main". Si no lo encuentra, devuelve NIL.}
var
  elem: TxpElement;
begin
  elem := LastNode;   //Debe ser el último
  if not(elem is TxpEleBody) then begin
    exit(nil);  //No deberría pasar
  end;
  //Devuelve referencia
  Result := TxpEleBody(elem);
end;
function TxpElement.Index: integer;
{Devuelve la ubicación del elemento, dentro de su nodo padre.}
begin
  Result := Parent.elements.IndexOf(self);  //No es muy rápido
end;
//Gestion de llamadas al elemento
procedure TxpElement.AddCaller;
{Agrega información sobre el elemento "llamador", es decir, la función/cuerpo que hace
referencia a este elemento.}
begin
  {Lo maneja a través de evento, para poder acceder a información, del elemento actual
  y datos adicionales, a los que no se tiene acceso desde el contexto de esta clase.}
  if OnAddCaller<>nil then OnAddCaller(self);
end;
function TxpElement.nCalled: integer;
begin
  Result := lstCallers.Count;
end;
function TxpElement.IsCalledBy(callElem: TxpElement): boolean;
{Indica si el elemento es llamado por "callElem". Puede haber varias llamadas desde
"callElem", pero basta que haya una para devolver TRUE.}
var
  cal : TxpEleCaller;
begin
  for cal in lstCallers do begin
    if cal.caller = callElem then exit(true);
  end;
  exit(false);
end;
function TxpElement.FindCalling(callElem: TxpElement): TxpEleCaller;
{Busca la llamada de un elemento. Si no lo encuentra devuelve NIL.}
var
  cal : TxpEleCaller;
begin
  for cal in lstCallers do begin
    if cal.caller = callElem then exit(cal);
  end;
  exit(nil);
end;
function TxpElement.RemoveCallsFrom(callElem: TxpElement): integer;
{Elimina las referencias de llamadas desde un elemento en particular.
Devuelve el número de referencias eliminadas.}
var
  cal : TxpEleCaller;
  n, i: integer;
begin
  {La búsqueda debe hacerse al revés para evitar el problema de borrar múltiples
  elementos}
  n := 0;
  for i := lstCallers.Count-1 downto 0 do begin
    cal := lstCallers[i];
    if cal.caller = callElem then begin
      lstCallers.Delete(i);
      inc(n);
    end;
  end;
  Result := n;
end;
procedure TxpElement.ClearCallers;
begin
  lstCallers.Clear;
end;
function TxpElement.DuplicateIn(list: TxpElements): boolean;
{Debe indicar si el elemento está duplicado en la lista de elementos proporcionada.}
var
  uName: String;
  ele: TxpElement;
begin
  uName := upcase(name);
  for ele in list do begin
    if upcase(ele.name) = uName then begin
      exit(true);
    end;
  end;
  exit(false);
end;
constructor TxpElement.Create;
begin
  elemType := eltNone;
  lstCallers:= TxpListCallers.Create(true);
end;
destructor TxpElement.Destroy;
begin
  lstCallers.Destroy;
  elements.Free;  //por si contenía una lista
  inherited Destroy;
end;
{ TxpEleMain }
constructor TxpEleMain.Create;
begin
  inherited;
  elemType:=eltMain;
  Parent := nil;  //la raiz no tiene padre
end;
{ TxpEleCon }
constructor TxpEleCon.Create;
begin
  inherited;
  elemType:=eltCons;
end;
function TxpEleVar.AbsAddr: word;
{Devuelve la dirección absoluta de la variable. Tener en cuenta que la variable, no
siempre tiene un solo byte, así que se trata de devolver siempre la dirección del
byte de menor peso.}
begin
  if (typ = typBit) or (typ = typBool) then begin
    Result := adrBit.AbsAdrr;
  end else if (typ = typByte) or (typ = typChar) then begin
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
  end else if (typ = typByte) or (typ = typChar) then begin
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
  inherited;
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
  inherited;
  elemType:=eltType;
end;
{ TxpEleFun }
procedure TxpEleFun.ClearParams;
//Elimina los parámetros de una función
begin
  setlength(pars,0);
end;
procedure TxpEleFun.CreateParam(parName: string; typ0: ttype; pvar: TxpEleVar);
//Crea un parámetro para la función
var
  n: Integer;
begin
  //agrega
  n := high(pars)+1;
  setlength(pars, n+1);
  pars[n].name := parName;
  pars[n].typ  := typ0;  //agrega referencia
  pars[n].pvar := pvar;
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
    if pars[i].typ <> Fun2.pars[i].typ then begin
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
function TxpEleFun.DuplicateIn(list: TxpElements): boolean;
var
  uName: String;
  ele: TxpElement;
begin
  uName := upcase(name);
  for ele in list do begin
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
procedure TxpEleFun.SetElementsUnused;
{Marca todos sus elementos con "nCalled = 0". Se usa cuando se determina que una función
no es usada.}
var
  elem: TxpElement;
begin
  if elements = nil then exit;  //No tiene
  //Marca sus elementos, como no llamados
  for elem in elements do begin
    elem.ClearCallers;
    if elem is TxpEleVar then begin
      TxpEleVar(elem).ResetAddress;
    end;
  end;
end;
constructor TxpEleFun.Create;
begin
  inherited;
  elemType:=eltFunc;
end;

{ TxpEleUnit }
constructor TxpEleUnit.Create;
begin
  inherited;
  elemType:=eltUnit;
end;
{ TxpEleBody }
constructor TxpEleBody.Create;
begin
  inherited;
  elemType := elBody;
end;
{ TXpTreeElements }
procedure TXpTreeElements.Clear;
begin
  main.elements.Clear;  //esto debe hacer un borrado recursivo
  curNode := main;      //retorna al nodo principal
  //ELimina lista internas
  AllVars.Clear;
  AllFuncs.Clear;
end;
procedure TXpTreeElements.RefreshAllVars;
{Devuelve una lista de todas las variables del árbol de sintaxis, incluyendo las de las
funciones y procedimientos.}
  procedure AddVars(nod: TxpElement);
  var
    ele : TxpElement;
  begin
    if nod.elements<>nil then begin
      for ele in nod.elements do begin
        if ele.elemType = eltVar then begin
          AllVars.Add(TxpEleVar(ele));
        end else begin
          if ele.elements<>nil then
            AddVars(ele);  //recursivo
        end;
      end;
    end;
  end;
begin
  AllVars.Clear;   //por si estaba llena
  AddVars(main);
end;
procedure TXpTreeElements.RefreshAllFuncs;
{Actualiza una lista de todas las funciones del árbol de sintaxis, incluyendo las de las
unidades.}
  procedure AddFuncs(nod: TxpElement);
  var
    ele : TxpElement;
  begin
    if nod.elements<>nil then begin
      for ele in nod.elements do begin
        if ele.elemType = eltFunc then begin
          AllFuncs.Add(TxpEleFun(ele));
        end else begin
          if ele.elements<>nil then
            AddFuncs(ele);  //recursivo
        end;
      end;
    end;
  end;
begin
  AllFuncs.Clear;   //por si estaba llena
  AddFuncs(main);
end;
function TXpTreeElements.CurNodeName: string;
{Devuelve el nombre del nodo actual}
begin
  Result := curNode.name;
end;
function TXpTreeElements.LastNode: TxpElement;
{Devuelve una referencia al último nodo de "main"}
begin
  Result := main.LastNode;
end;
function TXpTreeElements.BodyNode: TxpEleBody;
{Devuelve la referecnia al cuerpo principal del programa.}
begin
  Result := main.BodyNode;
end;
//funciones para llenado del arbol
function TXpTreeElements.AddElement(elem: TxpElement; verifDuplic: boolean = true): boolean;
{Agrega un elemento al nodo actual. Si ya existe el nombre del nodo, devuelve false.
Este es el punto único de entrada para realizar cambios en el árbol.}
begin
  Result := true;
  //Verifica si hay conflicto. Solo es necesario buscar en el nodo actual.
  if verifDuplic and elem.DuplicateIn(curNode.elements) then begin
    exit(false);  //ya existe
  end;
  //Agrega el nodo
  curNode.AddElement(elem);
  if OnAddElement<>nil then OnAddElement(elem);
end;
procedure TXpTreeElements.AddElementAndOpen(elem: TxpElement);
{Agrega un elemento y cambia el nodo actual al espacio de este elemento nuevo. Este
método está reservado para las funciones o procedimientos}
begin
  {las funciones o procedimientos no se validan inicialmente, sino hasta que
  tengan todos sus parámetros agregados, porque pueden ser sobrecargados.}
  AddElement(elem, false);
  //Genera otro espacio de nombres
  elem.elements := TxpElements.Create(true);  //su propia lista
  curNode := elem;  //empieza a trabajar en esta lista
end;
procedure TXpTreeElements.OpenElement(elem: TxpElement);
{Accede al espacio de nombres del elemento indicado.}
begin
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
"curFindIdx", hacia "atrás", el elemento con nombre "curFindName". También implementa
la búsqueda en unidades.
Esta rutina es quien define la resolución de nombres (alcance) en PicPas.}
var
  tmp: String;
  elem: TxpElement;
begin
//  debugln(' Explorando nivel: [%s] en pos: %d', [curFindNode.name, curFindIdx - 1]);
  tmp := UpCase(curFindName);  //convierte pra comparación
  repeat
    curFindIdx := curFindIdx - 1;  //Siempre salta a la posición anterior
    if curFindIdx<0 then begin
      //No encontró, en ese nivel. Hay que ir más atrás. Pero esto se resuelve aquí.
      if curFindNode.Parent = nil then begin
        //No hay nodo padre. Este es el nodo Main
        Result := nil;
        exit;  //aquí termina la búsqueda
      end;
      //Busca en el espacio padre
      curFindIdx := curFindNode.Index;  //posición actual
      curFindNode := curFindNode.Parent;  //apunta al padre
      if inUnit then inUnit := false;   //Sale de una unidad
      Result := FindNext();  //Recursividad IMPORTANTE: Usar paréntesis.
//      Result := nil;
      exit;
    end;
    //Verifica ahora este elemento
    elem := curFindNode.elements[curFindIdx];
    if UpCase(elem.name) = tmp then begin
      //Encontró en "curFindIdx"
      Result := elem;
      //La siguiente búsqueda empezará en "curFindIdx-1".
      exit;
    end else begin
      //No tiene el mismo nombre, a lo mejor es una unidad
      if (elem is TxpEleUnit) and not inUnit then begin   //Si es el priemr nodo de unidad
        //¡Diablos es una unidad! Ahora tenemos que implementar la búsqueda.
        inUnit := true;   //Marca, para que solo busque en un nivel
        curFindIdx := elem.elements.Count;  //para que busque desde el último
        curFindNode := elem;  //apunta a la unidad
        Result := FindNext();  //Recursividad IMPORTANTE: Usar paréntesis.
        if Result <> nil then begin  //¿Ya encontró?
          exit;  //Sí. No hay más que hacer aquí
        end;
        //No encontró. Hay que seguir buscando
      end;
    end;
  until false;
end;
function TXpTreeElements.FindFirst(const name: string): TxpElement;
{Rutina que permite resolver un identificador dentro del árbol de sintaxis, siguiendo las
reglas de alcance de identifiacdores (primero en el espacio actual y luego en los
espacios padres).
 Si encuentra devuelve la referencia. Si no encuentra, devuelve NIL}
begin
  //Busca recursivamente, a partir del espacio actual
  curFindName := name;     //Este valor no cambiará en toda la búsqueda
  inUnit := false;     //Inicia bandera
  if curNode is TxpEleBody then begin
    {Para los cuerpos de procemientos o de programa, se debe explorar hacia atrás a
    partir de la posición del nodo actual.}
    curFindIdx := curNode.Index;  //Ubica posición
    curFindNode := curNode.Parent;  //Actualiza nodo actual de búsqueda
    Result := FindNext;
  end else begin
    {La otras forma de resolución, debe ser:
    1. Declaración de constantes, cuando se definen como expresión con otras constantes
    2. Declaración de variables, , cuando se definen como ABSOLUTE <variable>
    }
    curFindNode := curNode;  //Actualiza nodo actual de búsqueda
    {Formalmente debería apuntar a la posicñon del elemento actual, pero se deja
    apuntando a la posición final, sin peligro, porque, la resolución de nombres para
    consatntes y variables, se hace solo en la primera pasada (con el árbol de sintaxis
    llenándose.)}
    curFindIdx := curNode.elements.Count;
    //Busca
    Result := FindNext;
  end;
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
  main.name := 'Main';
  main.elements := TxpElements.Create(true);  //debe tener lista
  AllFuncs := TxpEleFuns.Create(false);   //Crea lista
  AllVars := TxpEleVars.Create(false);   //Crea lista
  curNode := main;  //empieza con el nodo principal como espacio de nombres actual
end;
destructor TXpTreeElements.Destroy;
begin
  main.Destroy;
  AllVars.Free;    //por si estaba creada
  AllFuncs.Free;
  inherited Destroy;
end;
end.

