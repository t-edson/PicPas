{XpresParserPIC

Versión de XpresParser, orientada a trabajar con microcontroladores PIC.
La idea es tener aquí todas las rutinas que en lo posible sean independientes del
lenguaje y del modelo de PIC.

Las variables públicas más importantes de este módulo son:

 typs[]  -> almacena a los tipos declarados

Para mayor información sobre el uso del framework Xpres, consultar la documentación
técnica.
}
unit XpresParserPIC;
interface
uses
  Classes, SysUtils, Forms, LCLType, lclProc, SynEditHighlighter,
  SynFacilHighlighter, XpresBas, XpresTypes, XpresElementsPIC, MisUtils;

type
{ TOperand }
//Operando
TOperand = object
public
  catOp: TCatOperan; //Categoría de operando
  typ  : TType;     //Referencia al tipo de dato
  txt  : string;    //Texto del operando o expresión, tal como aparece en la fuente
//  fun  : Tfunc;     //referencia a función en caso de que sea una función
  rVar : TxpEleVar;    //referencia a la variable, en caso de que sea variable
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
  function FindOperator(const oper: string): TxpOperator; //devuelve el objeto operador
  //Funciones para facilitar el acceso a campos de la variable, cuando sea variable
  function VarName: string; inline; //nombre de la variable, cuando sea de categ. coVariab
  function offs: TVarOffs; inline;  //dirección de la variable
  function Loffs: TVarOffs; inline; //dirección del byte bajo
  function Hoffs: TVarOffs; inline; //dirección del byte alto
  function bank: TVarBank; inline;  //banco o segmento
  function bit : byte; inline;  //posición del bit
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
  function CanBeWord: boolean;   //indica si cae en el rango de un WORD
  function CanBeByte: boolean;   //indica si cae en el rango de un BYTE
  //métodos para mover valores desde/hacia una constante externa
  procedure CopyConsValTo(var c: TxpEleCon);
  procedure GetConsValFrom(const c: TxpEleCon);
end;

{ TCompilerBase }
{Clase base para crear al objeto compilador}
TCompilerBase = class
protected  //Eventos del compilador
  OnExprStart: procedure of object;  {Se genera al iniciar la
                                             evaluación de una expresión.}
  OnExprEnd  : procedure(isParam: boolean) of object;  {Se genera
                                             el terminar de evaluar una expresión}
  ExprLevel  : Integer;  //Nivel de anidamiento de la rutina de evaluación de expresiones
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
  //Manejo de funciones
  function CreateFunction(funName: string; typ: ttype; proc: TProcExecFunction): TxpEleFun;
  //  procedure CreateFunction(funName, varType: string);
  function ValidateFunction: boolean;
  procedure CloseFunction;
  function CreateSysFunction(funName: string; typ: ttype; proc: TProcExecFunction): TxpEleFun;
  procedure CreateParam(fun: TxpEleFun; parName: string; typStr: string);
  procedure CaptureParams; virtual;
  //Mmanejo de expresiones
  function GetOperand: TOperand; virtual;
  function GetOperandPrec(pre: integer): TOperand;
  function GetOperator(const Op: Toperand): Txpoperator;
  procedure GetExpression(const prec: Integer; isParam: boolean=false);
//  procedure GetBoolExpression;
//  procedure CreateVariable(const varName: string; typ: ttype);
//  procedure CreateVariable(varName, varType: string);
public
  TreeElems: TXpTreeElements; //tablas de elementos del lenguaje
private
  typs   : TTypes;       //lista de tipos (El nombre "types" ya está reservado)
  function GetExpressionCore(const prec: Integer): TOperand;
  procedure Evaluar(var Op1: TOperand; opr: TxpOperator; var Op2: TOperand);
  procedure EvaluarPre(var Op1: TOperand; opr: TxpOperator);
  procedure EvaluarPost(var Op1: TOperand; opr: TxpOperator);
  procedure Evaluar(var Op1: TOperand);
public  //Referencias a los tipos predefinidos de tokens.
  tkEol     : TSynHighlighterAttributes;
  tkSymbol  : TSynHighlighterAttributes;
  tkSpace   : TSynHighlighterAttributes;
  tkIdentif : TSynHighlighterAttributes;
  tkNumber  : TSynHighlighterAttributes;
  tkKeyword : TSynHighlighterAttributes;
  tkString  : TSynHighlighterAttributes;
  tkComment : TSynHighlighterAttributes;
  //otras referencias
  tkOperator: TSynHighlighterAttributes;
  tkBoolean : TSynHighlighterAttributes;
  tkSysFunct: TSynHighlighterAttributes;
  tkType    : TSynHighlighterAttributes;
public
  xLex   : TSynFacilSyn; //resaltador - lexer
  cIn    : TContexts;   //entrada de datos
  //variables públicas del compilador
  ejecProg: boolean;   //Indica que se está ejecutando un programa o compilando
  DetEjec: boolean;   //para detener la ejecución (en intérpretes)

  func0  : TxpEleFun;      //función interna para almacenar parámetros
  p1, p2 : ^TOperand;    //Pasa los operandos de la operación actual
  res    : TOperand;    //resultado de la evaluación de la última expresión.
  catOperation: TCatOperation;  //combinación de categorías de los operandos

  function CatOperationToStr(Op: string=','): string;
public  //Manejo de errores
  PErr  : TPError;     //Objeto de Error
  function HayError: boolean;
  procedure GenError(msg: string);
  procedure GenError(msg: String; const Args: array of const);
  function ArcError: string;
  function nLinError: integer;
  function nColError: integer;
  procedure ShowError;
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
  coVariab_Expres: exit('Variable'+ Op +'Constant');
  coExpres_Const : exit('Expression'+ Op +'Constant');
  coExpres_Variab: exit('Expression'+ Op +'Variable');
  coExpres_Expres: exit('Expression'+ Op +'Expression');
  end;
end;
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
procedure TCompilerBase.GenError(msg: String; const Args: array of const);
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
                         PChar(Perr.NombPrograma), MB_ICONERROR);
//  Perr.Show;
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
//Manejo de funciones
function TCompilerBase.CreateFunction(funName: string; typ: ttype;
  proc: TProcExecFunction): TxpEleFun;
{Crea una nueva función y devuelve un índice a la función.}
var
  fun : TxpEleFun;
begin
  fun := TxpEleFun.Create;
  fun.name:= funName;
  fun.typ := typ;
  fun.proc:= proc;
  fun.ClearParams;
  TreeElems.OpenElement(fun);  //Se abre un nuevo espacio de nombres, pero no se valida duplicidad aún
  Result := fun;
end;
{procedure TCompilerBase.CreateFunction(funName, varType: string);
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
end;}
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
procedure TCompilerBase.CloseFunction;
{Cierra el espacio de trabajo de la función actual. Se debe llamar después de procesar
todo el cuerpo de la función}
begin
  TreeElems.CloseElement;
end;
function TCompilerBase.CreateSysFunction(funName: string; typ: ttype;
  proc: TProcExecFunction): TxpEleFun;
{Crea una función del sistema. A diferencia de las funciones definidas por el usuario,
una función del sistema se crea, sin crear espacios de nombre. La idea es poder
crearlas rápidamente.}
var
  fun : TxpEleFun;
begin
  fun := TxpEleFun.Create;  //Se crea como una función normal
  fun.name:= funName;
  fun.typ := typ;
  fun.proc:= proc;
  fun.ClearParams;
  TreeElems.AddElement(fun, false);  //no verifica duplicidad
  Result := fun;
end;
procedure TCompilerBase.CreateParam(fun: TxpEleFun; parName: string;
  typStr: string);
//Crea un parámetro para una función
var
  typ : TType;
begin
  //busca tipo
  typ := FindType(typStr);
  if typ = nil then begin
    GenError('Undefined type "%s"', [typStr]);
    exit;
  end;
  //agrega
  fun.CreateParam(parName, typ);
end;
procedure TCompilerBase.CaptureParams;
//Lee los parámetros de una función en la función interna funcs[0]
begin
  SkipWhites;
  func0.ClearParams;
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
      func0.CreateParam('',res.typ);
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
//Manejo de expresiones
function TCompilerBase.GetOperand: TOperand;
{Parte de la funcion analizadora de expresiones que genera codigo para leer un operando.
Debe devolver el tipo del operando y también el valor (obligatorio para el caso
de intérpretes y opcional para compiladores)}
var
  xcon: TxpEleCon;
  xvar: TxpEleVar;
  xfun: TxpEleFun;
  tmp, oprTxt: String;
  ele: TxpElement;
  Op: TOperand;
  posAct: TPosCont;
  opr: TxpOperator;
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
    ele := TreeElems.FindFirst(cIn.tok);  //identifica elemento
    if ele = nil then begin
      //No identifica a este elemento
      GenError('Unknown identifier: %s', [cIn.tok]);
      exit;
    end;
    if ele.elemType = eltVar then begin
      //es una variable
      xvar := TxpEleVar(ele);
      Result.rVar:=xvar;   //guarda referencia a la variable
      Result.catOp:=coVariab;    //variable
      Result.catTyp:= xvar.typ.cat;  //categoría
      Result.typ:=xvar.typ;
      Result.txt:= cIn.tok;     //toma el texto
      cIn.Next;    //Pasa al siguiente
    end else if ele.elemType = eltCons then begin  //es constante
      //es una constante
      xcon := TxpEleCon(ele);
      Result.catOp:=coConst;    //constante
      Result.catTyp:= xcon.typ.cat;  //categoría
      Result.typ:=xcon.typ;
      Result.GetConsValFrom(xcon);  //lee valor
      Result.txt:= cIn.tok;     //toma el texto
      cIn.Next;    //Pasa al siguiente
    end else if ele.elemType = eltFunc then begin  //es función
      {Se sabe que es función, pero no se tiene la función exacta porque puede haber
       versiones, sobrecargadas de la misma función.}
      tmp := cIn.tok;  //guarda nombre de función
      cIn.Next;    //Toma identificador
      CaptureParams;  //primero lee parámetros
      if HayError then exit;
      //Aquí se identifica la función exacta, que coincida xcon sus parámetros
      { TODO : No es la forma más eficiente, explorar nuevamente todo el NAMESPACE. Tal vez se debería usar funciones de tipo FindFirst y FindNExt }
      case TreeElems.FindFuncWithParams(tmp, func0, xfun) of
      //TFF_NONE:      //No debería pasar esto
      TFF_PARTIAL:   //encontró la función, pero no coincidió xcon los parámetros
         GenError('Type parameters error on %s', [tmp +'()']);
      TFF_FULL:     //encontró completamente
        begin   //encontró
          Result.catOp :=coExpres; //expresión
          Result.txt:= cIn.tok;    //toma el texto
          Result.typ:=xfun.typ;
          xfun.proc(xfun);  //llama al código de la función
          exit;
        end;
      end;
    end else begin
      GenError('Not implemented.');
      exit;
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
}
  end else if cIn.tokType = tkOperator then begin
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
      GenError('No se puede aplicar el operador "%s" al tip "%s"', [oprTxt, Op.typ.name]);
      exit;
    end;
    //Sí corresponde. Así que apliquémoslo
    EvaluarPre(Op, opr);
    Result := res;
  end else begin
    //No se reconoce el operador
    GenError('Operand expected.');
  end;
end;
procedure TCompilerBase.Evaluar(var Op1: TOperand; opr: TxpOperator; var Op2: TOperand);
{Ejecuta una operación con dos operandos y un operador. "opr" es el operador de Op1.
El resultado debe devolverse en "res". En el caso de intérpretes, importa el
resultado de la Operación.
En el caso de compiladores, lo más importante es el tipo del resultado, pero puede
usarse también "res" para cálculo de expresiones constantes.
}
var
  Operation: TxpOperation;
begin
  debugln(space(ExprLevel)+' Eval('+Op1.txt + opr.txt + Op2.txt+')');
   PErr.IniError;
   //Busca si hay una operación definida para: <tipo de Op1>-opr-<tipo de Op2>
   Operation := opr.FindOperation(Op2.typ);
   if Operation = nil then begin
//      GenError('No se ha definido la operación: (' +
//                    Op1.typ.name + ') '+ opr.txt + ' ('+Op2.typ.name+')');
      GenError('Illegal Operation: %s',
               ['(' + Op1.typ.name + ') '+ opr.txt + ' ('+Op2.typ.name+')']);
      Exit;
    end;
   {Llama al evento asociado con p1 y p2 como operandos. }
   p1 := @Op1; p2 := @Op2;  { Se usan punteros por velocidad. De otra forma habría que
                             copiar todo el objeto.}
   //junta categorías de operandos
   catOperation := TCatOperation((Ord(Op1.catOp) << 2) or ord(Op2.catOp));
   {Ejecuta la operación.
   Los parámetros de entrada se dejan en p1 y p2. El resultado debe dejarse en "res"}
   Operation.proc;
   //Completa campos de "res", si es necesario
//   res.txt := Op1.txt + opr.txt + Op2.txt;   //texto de la expresión
//   res.uop := opr;   //última operación ejecutada
End;
procedure TCompilerBase.EvaluarPre(var Op1: TOperand; opr: TxpOperator);
{Ejecuta una operación con un operando y un operador unario de tipo Pre. "opr" es el
operador de Op1.
El resultado debe devolverse en "res".}
begin
  {$IFDEF LogExpres} debugln(space(ExprLevel)+' Eval('+ opr.txt + Op1.txt + ')'); {$ENDIF}
   PErr.IniError;
   if opr.OperationPre = nil then begin
      GenError('Operación no válida: ' +
                 opr.txt + '('+Op1.typ.name+')');
      Exit;
    end;
   {Llama al evento asociado con p1 como operando. }
   p1 := @Op1; {Solo hay un parámetro}
   {Ejecuta la operación. El resultado debe dejarse en "res"}
   opr.OperationPre;
   //Completa campos de "res", si es necesario
   {$IFDEF LogExpres} res.txt := '%';{$ENDIF}   //indica que es expresión
end;
procedure TCompilerBase.EvaluarPost(var Op1: TOperand; opr: TxpOperator);
{Ejecuta una operación con un operando y un operador unario de tipo Post. "opr" es el
operador de Op1.
El resultado debe devolverse en "res".}
begin
  {$IFDEF LogExpres} debugln(space(ExprLevel)+' Eval('+Op1.txt + opr.txt +')'); {$ENDIF}
   PErr.IniError;
   if opr.OperationPost = nil then begin
      GenError('Operación no válida: ' +
                 '('+Op1.typ.name+')' + opr.txt);
      Exit;
    end;
   {Llama al evento asociado con p1 como operando. }
   p1 := @Op1; {Solo hay un parámetro}
   {Ejecuta la operación. El resultado debe dejarse en "res"}
   opr.OperationPost;
   //Completa campos de "res", si es necesario
   {$IFDEF LogExpres} res.txt := '%';{$ENDIF}   //indica que es expresión
end;
procedure TCompilerBase.Evaluar(var Op1: TOperand);
{Ejecuta una operación con un solo operando, que puede ser una constante, una variable
o la llamada a una función.
El resultado debe devolverse en "res".
La implementación debe decidir, qué hacer cuando se encuentra un solo operando como
expresión. En algunos casos puede ser inválido.
}
begin
  {$IFDEF LogExpres}
  debugln(space(ExprLevel)+' Eval('+Op1.txt+')');
  {$ENDIF}
  PErr.IniError;
  {Llama al evento asociado con p1 como operando. }
  p1 := @Op1; {Solo hay un parámetro}
  {Ejecuta la operación. El resultado debe dejarse en "res"}
  if Op1.typ.OperationLoad <> nil then Op1.typ.OperationLoad();
  //Completa campos de "res", si es necesario
//   res.txt := Op1.txt + opr.txt + Op2.txt;   //texto de la expresión
//   res.uop := opr;   //última operación ejecutada
end;
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
  debugln(space(ExprLevel)+' GetOperandP('+IntToStr(pre)+')');
  {$ENDIF}
  Op1 :=  GetOperand;  //toma el operador
  if pErr.HayError then exit;
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
      //es de mayor precedencia, se debe evaluar antes.
      Op2 := GetOperandPrec(pre);  //toma el siguiente operando (puede ser recursivo)
      if pErr.HayError then exit;
      Evaluar(Op1, opr, Op2);  //devuelve en "res"
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
  if cIn.tokType <> tkOperator then exit(nil);
  //hay un operador
  Result := Op.typ.FindBinaryOperator(cIn.tok);
  cIn.Next;   //toma el token
end;
function TCompilerBase.GetExpressionCore(const prec: Integer): TOperand; //inline;
{Analizador de expresiones. Esta es probablemente la función más importante del
 compilador. Procesa una expresión en el contexto de entrada llama a los eventos
 configurados para que la expresión se evalúe (intérpretes) o se compile (compiladores).
 Devuelve un operando con información sobre el resultado de la expresión.}
var
  Op1, Op2  : TOperand;   //Operandos
  opr1: TxpOperator;  //Operadores
begin
  Op1.catTyp:=t_integer;    //asumir opcion por defecto
  Op2.catTyp:=t_integer;   //asumir opcion por defecto
  pErr.Clear;
  //----------------coger primer operando------------------
  Op1 := GetOperand;
  if pErr.HayError then exit;
  {$IFDEF LogExpres} debugln(space(ExprLevel)+' Op1='+Op1.txt); {$ENDIF}
  //verifica si termina la expresion
  SkipWhites;
  opr1 := GetOperator(Op1);
  if opr1 = nil then begin  //no sigue operador
    //Expresión de un solo operando. Lo carga por si se necesita
    //Evaluar(Op1);
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
      EvaluarPost(Op1, opr1);
      if PErr.HayError then exit;
      Op1 := res;
      SkipWhites;
      opr1 := GetOperator(Op1);
      continue;
    end;
    //--------------------coger segundo operando--------------------
    Op2 := GetOperandPrec(Opr1.prec);   //toma operando con precedencia
    {$IFDEF LogExpres} debugln(space(ExprLevel)+' Op2='+Op2.txt); {$ENDIF}
    if pErr.HayError then exit;
    //prepara siguiente operación
    Evaluar(Op1, opr1, Op2);    //evalua resultado en "res"
    Op1 := res;
    if PErr.HayError then exit;
    SkipWhites;
    opr1 := GetOperator(Op1);   {lo toma ahora con el tipo de la evaluación Op1 (opr1) Op2
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
  {$IFDEF LogExpres}
  debugln(space(ExprLevel)+'>Inic.expr');
  {$ENDIF}
  if OnExprStart<>nil then OnExprStart;  //llama a evento
  res := GetExpressionCore(prec);
  if PErr.HayError then exit;
  if OnExprEnd<>nil then OnExprEnd(isParam);    //llama al evento de salida
  {$IFDEF LogExpres}
  debugln(space(ExprLevel)+'>Fin.expr');
  {$ENDIF}
  Dec(ExprLevel);
  {$IFDEF LogExpres}
  if ExprLevel = 0 then debugln('');
  {$ENDIF}
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
//Inicialización
constructor TCompilerBase.Create;
begin
  PErr.IniError;   //inicia motor de errores
  //Inicia lista de tipos
  typs := TTypes.Create(true);
  //Crea arbol de elementos
  TreeElems := TXpTreeElements.Create;
  //inicia la sintaxis
  xLex := TSynFacilSyn.Create(nil);   //crea lexer
  func0 := TxpEleFun.Create;  //crea la función 0, para uso interno

  if HayError then PErr.Show;
  cIn := TContexts.Create(xLex); //Crea lista de Contextos
  ejecProg := false;
  //Actualiza las referencias a los tipos de tokens existentes en SynFacilSyn
  tkEol     := xLex.tkEol;
  tkSymbol  := xLex.tkSymbol;
  tkSpace   := xLex.tkSpace;
  tkIdentif := xLex.tkIdentif;
  tkNumber  := xLex.tkNumber;
  tkKeyword := xLex.tkKeyword;
  tkString  := xLex.tkString;
  tkComment := xLex.tkComment;
  //Crea nuevos tipos necesarios para el Analizador Sintáctico
  tkOperator := xLex.NewTokType('Operator'); //necesario para analizar expresiones
  tkBoolean  := xLex.NewTokType('Boolean');  //constantes booleanas
  tkSysFunct := xLex.NewTokType('SysFunct'); //funciones del sistema
  tkType     := xLex.NewTokType('Types');    //tipos de datos
end;
destructor TCompilerBase.Destroy;
begin
  cIn.Destroy; //Limpia lista de Contextos
  func0.Destroy;
  xLex.Free;
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
begin
  Result := rVar.offs;
end;
function TOperand.Loffs: TVarOffs;
begin
  Result := rVar.offs;
end;
function TOperand.Hoffs: TVarOffs;
begin
  Result := rVar.offs+1;  //por ahora se asume que es el siguiente byte
end;

function TOperand.bank: TVarBank;
begin
  Result := rVar.bank;
end;
function TOperand.bit: byte;
begin
  Result := rVar.bit;
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

procedure TOperand.GetConsValFrom(const c: TxpEleCon);
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
  if typ.OperationPush<> nil then typ.OperationPush(@self);
end;
procedure TOperand.Pop;
begin
  //llama al evento de pila
  if typ.OperationPop<> nil then typ.OperationPop(@self);
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
    dicSet('"(" expected.', 'Se esperaba "("');
    dicSet('")" expected.', 'Se esperaba ")"');
    dicSet('Type parameters error on %s', 'Error en tipo de parámetros de %s');
    dicSet('Unknown identifier: %s', 'Identificador desconocido: %s');
    dicSet('Function not implemented: %s', 'Función no implementada: "%s"');
    dicSet('Error in expression. ")" expected.', 'Error en expresión. Se esperaba ")"');
    dicSet('Operand expected.', 'Se esperaba operando.');
    dicSet('Illegal Operation: %s', 'Operación no válida: %s');
    dicSet('Undefined operator: %s for type: %s','No está definido el operador: %s para tipo: %s');
    dicSet('Duplicated function: %s','Función duplicada: %s');
  end;
  end;
end;

end.
