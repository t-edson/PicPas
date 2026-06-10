{
Unit including the base clase for the compiler, the routines for manage operands
and the TOperand class definition.
The idea is that most of the code that is independent of the language to implement
and of the hardware.
                                            By Tito Hinostroza 2019 - Lima - Perú.
}
unit CompOperands;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, SynEditHighlighter,
  XpresTypesPIC, XpresElementsPIC, MisUtils, alexiaLex, XpresBas;
type
{ TOperand }
//Objeto Operando. Para información sobre operandos, ver la documentación técnica.
TOperand = object
private
  FSto   : TStoOperand; //Almacenamiento del operando
  FTyp   : TxpEleType;  //Tipo del operando, cuando no es stVariab
  FVar   : TxpEleVar;   //Referencia a variable
  FValue : TConsValue;  //Valores constantes, cuando el operando es constante
  function GetTyp: TxpEleType;
  procedure SetvalBool(AValue: boolean);
  procedure SetvalFloat(AValue: extended);
  procedure SetvalInt(AValue: Int64);
public  //Campos generales
  txt     : string;   //Texto del operando o expresión, tal como aparece en la fuente
  Inverted: boolean; {Este campo se usa para cuando el operando es de tipo Bit o Boolean.
                      Indica que la lógica debe leerse de forma invertida.}
  rVarBase: TxpEleVar;  {Referencia a variable base, cuando el operando es de tipo:
                         <variable>.<campo>.<campo>. }
  {"Sto", "Typ" y "rVar" se consideran de solo lectura. Para cambiarlos, se han definido
  los métodos: SetAsConst(), SetAsVariab(), SetAsExpres() y SetAsNull, que ofrecen una
  forma más sencilla y segura que cambiar "Cat", "Typ", y rVar" individualmente (que es
  como se hacía en versiones anteriores).}
  property Sto : TStoOperand read FSto;  //Alamcenamiento de operando
  property Typ : TxpEleType read GetTyp; //Tipo del operando
  property rVar: TxpEleVar  read FVar;   //Referencia a la variable.
  //property rVarOff: TxpEleVar read FVarOff;   //Referencia a la variable.
  procedure SetAsConst(xtyp: TxpEleType);
  procedure SetAsVariab(xvar: TxpEleVar);
  procedure SetAsExpres(xtyp: TxpEleType);
  procedure SetAsVarRef(VarBase: TxpEleVar);
  procedure SetAsVarRef(VarBase: TxpEleVar; ValOff: integer);
  procedure SetAsExpRef(VarBase: TxpEleVar; Etyp: TxpEleType);
  procedure SetAsNull;
  function StoOpStr: string;
  function StoOpChr: char;
  procedure DefineRegister; inline;
  function FindOperator(const oper: string): TxpOperator; //devuelve el objeto operador
  procedure Invert;  //Invierte la lógica del operando
public  //Campos acceso cuando sea variable.
  function VarName: string; inline; //nombre de la variable, cuando sea de categ. coVariab
  function offs : TVarOffs; //dirección de la variable
  function Boffs: TVarOffs; inline; //dirección del byte del bit
  function Loffs: TVarOffs; inline; //dirección del byte bajo
  function Hoffs: TVarOffs; inline; //dirección del byte alto
  function Eoffs: TVarOffs; inline; //dirección del byte alto
  function Uoffs: TVarOffs; inline; //dirección del byte alto
  function bank : TVarBank; inline;  //banco
  function addr : TVarOffs; //dirección absoluta de la variable
  function Lbank: TVarBank; inline;  //banco
  function bit : byte; inline;  //posición del bit
public  //Campos de acceso a los valores constantes
  property Value   : TConsValue read FValue;
  property valInt  : Int64 read FValue.ValInt write SetvalInt;
  property valFloat: extended read FValue.ValFloat write SetvalFloat;
  property valBool : boolean read FValue.ValBool write SetvalBool;
  property valStr  : string read FValue.ValStr write FValue.ValStr;
  //Funciones de ayuda para adaptar los tipos numéricos
  function aWord: word; inline;  //devuelve el valor en Word
  function LByte: byte; inline;  //devuelve byte bajo de valor entero
  function HByte: byte; inline;  //devuelve byte alto de valor entero
  function EByte: byte; inline;  //devuelve byte bajo de valor entero
  function UByte: byte; inline;  //devuelve byte bajo de valor entero
  //campos para validar el rango de los valores
  function CanBeByte: boolean;   //indica si cae en el rango de un BYTE
  function CanBeWord: boolean;   //indica si cae en el rango de un WORD
  //métodos para mover valores desde/hacia una constante externa
  procedure CopyConsValTo(var c: TxpEleCon);
  procedure GetConsValFrom(const c: TxpEleCon);
private //Manage items
  curSize: integer;
  function GetNItems: integer;
public  //Manage items when it's constant array
  property nItems: integer read GetNItems;
  procedure InitItems;
  procedure AddConsItem(const c: TConsValue);
  procedure CloseItems;
  procedure StringToArrayOfChar(str: string);
end;
TOperandPtr = ^TOperand;

{ TCompOperands }

TCompOperands = class
protected //Access to properties of p1^ y p2^.
  p1, p2   : ^TOperand;   //Pasa los operandos de la operación actual
  res      : TOperand;    //resultado de la evaluación de la última expresión.
  function value1: dword;
  function value1L: word;
  function value1H: word;
  function value1U: word;
  function value1E: word;
  function value2: dword;
  function value2L: word;
  function value2H: word;
  function bit1: TPicRegisterBit;
  function bit2: TPicRegisterBit;
  function byte1: TPicRegister;
  function byte1L: TPicRegister;
  function byte1H: TPicRegister;
  function byte1E: TPicRegister;
  function byte1U: TPicRegister;
  function byte2: TPicRegister;
  function byte2L: TPicRegister;
  function byte2H: TPicRegister;
  function byte2E: TPicRegister;
  function byte2U: TPicRegister;
  function stoOperation: TStoOperandsROB; inline;
  procedure ExchangeP1_P2;
  function OperationStr(Opt: TxpOperation): string;
public
  lex      : TContexts;   //Entrada de datos
  msg: TMessageManager;    //Referencia al gestor de mensajes
public    //Errors and warnings
  curLocation: TxpEleLocation;   //Ubicación actual de exploración **** ¿Mover al lexer como en Alexia?
  procedure ClearError;
  function HayError: boolean; inline;          //Flag for errors
  //Rutinas de generación de mensajes
  procedure GenInfo(txt: string);
  //Rutinas de generación de advertencias
  procedure GenWarn(txt: string);
  procedure GenWarn(txt: string; const Args: array of const);
  procedure GenWarn(txt: string; const Args: array of const; srcPos: TSrcPos);
  procedure GenError(txt: string; const srcPos: TSrcPos);
  procedure GenError(txt: String; const Args: array of const;
    const srcPos: TSrcPos);
  procedure GenError(txt: string);
  procedure GenError(txt: String; const Args: array of const);
public   //Referencias a los tipos predefinidos de tokens.
  tnEol       : integer;
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
public   //Tipos adicionales de tokens
  tnStruct   : integer;
  tnDirective: integer;
  tnAsm      : integer;
  tnExpDelim : integer;
  tnBlkDelim : integer;
  tnChar     : integer;
  tnOthers   : integer;
public    //Initialization
  constructor Create(msg0: TMessageManager);
  destructor Destroy; override;
end;

implementation
{ TOperand }
function TOperand.VarName: string; inline;
begin
  Result := rVar.name;
end;
function TOperand.offs: TVarOffs;
{Dirección de memoria, cuando es de tipo Char, Byte, Bit o Boolean.}
begin
  if FSto = stVarRef then begin
    //Caso especial, de stVarRefVar
//    if FVar = nil then
//      Result := FVarOff.offs
//    else
      Result := FVar.offs;
  end else begin
    //Para todos los otros casos, esto debe funcionar, según la docuemntación.
    Result := FVar.offs;
  end;
end;
function TOperand.Boffs: TVarOffs;
begin
  Result := rVar.adrBit.offs;
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
function TOperand.Eoffs: TVarOffs;
begin
  Result := rVar.adrByte2.offs;
end;
function TOperand.Uoffs: TVarOffs;
begin
  Result := rVar.adrByte3.offs;
end;
function TOperand.bank: TVarBank;
{Banco, del operando. Se supone que si se pide el banoo es porque es stVariab, o también
puede se el caso stVarRefVar. }
begin
  if FSto = stVarRef then begin
    //Caso especial, de stVarRefVar
//    if FVar = nil then
//      Result := FVarOff.bank
//    else
      Result := FVar.bank;
  end else begin
    //Para todos los otros casos, esto debe funcionar, según la docuemntación.
    Result := FVar.bank;
  end;
end;
function TOperand.addr: TVarOffs;
begin
  Result := FVar.addr;
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
  if Sto = stConst then begin
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
function TOperand.LByte: byte; inline;
begin
  Result := LO(word(valInt));
end;
function TOperand.HByte: byte; inline;
begin
  Result := HI(word(valInt));
end;
function TOperand.EByte: byte;
begin
  Result := (valInt >> 16) and $FF;
end;

function TOperand.UByte: byte;
begin
  Result := (valInt >> 24) and $FF;
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
  c.val := FValue;
end;
procedure TOperand.GetConsValFrom(const c: TxpEleCon);
{Copia valores constante desde una constante. Primero TOperand, debería tener inicializado
 correctamente su campo "catTyp". }
begin
  FValue := c.val;
end;
function TOperand.GetNItems: integer;
{Returns the number of items when operand is a static array.}
begin
  if Sto = stVariab then begin
    //It's a variable
    exit(rVar.typ.nItems)
  end else if Sto = stConst then begin
    exit(FValue.nItems);
  end else begin
      exit(0);
  end;
end;
//Manage items
const CONS_ITEM_BLOCK = 3;
procedure TOperand.InitItems;
begin
  FValue.nItems := 0;
  curSize := CONS_ITEM_BLOCK;   //Block size
  setlength(FValue.items, curSize);  //initial size
end;
procedure TOperand.AddConsItem(const c: TConsValue);
begin
  FValue.items[FValue.nItems] := c;
  inc(FValue.nItems);
  if FValue.nItems >= curSize then begin
    curSize += CONS_ITEM_BLOCK;   //Increase size by block
    setlength(FValue.items, curSize);  //make space
  end;
end;
procedure TOperand.CloseItems;
begin
  setlength(FValue.items, FValue.nItems);
end;
procedure TOperand.StringToArrayOfChar(str: string);
{Init the constant value as array of char from a string.}
var
  i: Integer;
begin
  FValue.nItems := length(str);
  setlength(FValue.items, FValue.nItems);
  for i:=0 to FValue.nItems-1 do begin
    FValue.items[i].ValInt := ord(str[i+1]);
  end;
end;

function TOperand.GetTyp: TxpEleType;
{Devuelve el tipo de la variable referenciada. }
begin
  if Sto = stVariab then begin
    Result := FVar.typ;
  end else if Sto = stVarRef then begin
    //Variable referenciada por variables.
    //Se implementa de acuerdo a la Doc. Técnica - Sección "Operandos sVarRef"
    Result := FVar.typ.itmType;   //¿No debería ser solo FVar.typ?
  end else if Sto = stExpRef then begin
    //Variable referenciada por expresión.
    //Se implementa de acuerdo a la Doc. Técnica - Sección "Operandos sExpRef"
    if FVar = nil then begin
      //Debe ser puntero.
      Result := FTyp.itmType;
    end else begin
      //Debe ser arreglo
      Result := FVar.typ;
    end;
  end else begin
    //Constante o expresión
    Result := FTyp;
  end;
end;
procedure TOperand.SetvalBool(AValue: boolean);
begin
  FValue.ValBool:=AValue;
end;
procedure TOperand.SetvalFloat(AValue: extended);
begin
//  if FvalFloat=AValue then Exit;
  FValue.ValFloat:=AValue;
end;
procedure TOperand.SetvalInt(AValue: Int64);
begin
//  if FvalInt=AValue then Exit;
  FValue.ValInt:=AValue;
end;
procedure TOperand.SetAsConst(xtyp: TxpEleType);
{Fija el almacenamiento del Operando como Constante, del tipo indicado}
begin
  FSto := stConst;
  FTyp := xtyp;
  rVarBase := nil;  //Inicia a este valor
end;
procedure TOperand.SetAsVariab(xvar: TxpEleVar);
{Fija el almacenamiento del Operando como Variable, del tipo de la variable}
begin
  FSto := stVariab;
  FVar := xvar;    //No hace falta actualziar el tipo
  rVarBase := nil;  //Inicia a este valor
end;
procedure TOperand.SetAsExpres(xtyp: TxpEleType);
{Fija el almacenamiento del Operando como Expresión, del tipo indicado}
begin
  FSto := stExpres;
  FTyp := xtyp;
  rVarBase := nil;  //Inicia a este valor
end;
procedure TOperand.SetAsVarRef(VarBase: TxpEleVar);
{Fija el operando como de tipo stVarRefVar.}
begin
  FSto := stVarRef;
  FVar := VarBase;
  rVarBase := nil;  //Inicia a este valor
end;
procedure TOperand.SetAsVarRef(VarBase: TxpEleVar; ValOff: integer);
{Versión de SetAsVarRef(), con desplazamiento constante.}
begin
  FSto := stVarRef;
  FVar := VarBase;
  FValue.ValInt := ValOff;
  rVarBase := nil;  //Inicia a este valor
end;
procedure TOperand.SetAsExpRef(VarBase: TxpEleVar; Etyp: TxpEleType);
begin
  FSto := stExpRef;
  FVar := VarBase;
  FTyp := Etyp;
  rVarBase := nil;  //Inicia a este valor
end;
procedure TOperand.SetAsNull;
{Configura al operando como de tipo Null}
begin
  {Se pone como constante, que es el tipo más simple, además se protege, pro si era
  variable}
  FSto := stConst;
  FTyp := typNull;   //Este es el tipo NULO
end;
function TOperand.StoOpStr: string;
{Devuelve lmacenamiento como cadena.}
begin
  case Sto of
  stConst : exit('Constant');
  stVariab, stVarRef, stExpRef: exit('Variable');
  stExpres: exit('Expression');
  else
    exit('');
  end;
end;
function TOperand.StoOpChr: char;
{Devuelve lmacenamiento como caracter.}
begin
  Result := ' ';
  case Sto of
  stConst : exit('k');
  stVariab: exit('v');
  stExpres: exit('X');
  end;
end;
procedure TOperand.DefineRegister;
begin
  //llama al evento de pila
  if Typ.OnDefRegister<> nil then Typ.OnDefRegister;
end;
function TOperand.FindOperator(const oper: string): TxpOperator;
//Recibe la cadena de un operador y devuelve una referencia a un objeto Toperator, del
//operando. Si no está definido el operador para este operando, devuelve nullOper.
begin
  Result := Typ.FindBinaryOperator(oper);
end;

//Accesos a propiedades de p1^ y p2^.
function TCompOperands.value1: dword; inline;
begin
  Result := p1^.valInt;
end;
function TCompOperands.value1L: word; inline;
begin
  Result := p1^.LByte;
end;
function TCompOperands.value1H: word; inline;
begin
  Result := p1^.HByte;
end;
function TCompOperands.value1U: word; inline;
begin
  Result := p1^.UByte;
end;
function TCompOperands.value1E: word; inline;
begin
  Result := p1^.EByte;
end;
function TCompOperands.value2: dword; inline;
begin
  Result := p2^.valInt;
end;
function TCompOperands.value2L: word; inline;
begin
  Result := p2^.LByte;
end;
function TCompOperands.value2H: word; inline;
begin
  Result := p2^.HByte;
end;
function TCompOperands.byte1: TPicRegister; inline;
begin
  Result := p1^.rVar.adrByte0;
end;
function TCompOperands.byte1L: TPicRegister; inline;
begin
  Result := p1^.rVar.adrByte0;
end;
function TCompOperands.byte1H: TPicRegister; inline;
begin
  Result := p1^.rVar.adrByte1;
end;
function TCompOperands.byte1E: TPicRegister;
begin
  Result := p1^.rVar.adrByte2;
end;
function TCompOperands.byte1U: TPicRegister;
begin
  Result := p1^.rVar.adrByte3;
end;
function TCompOperands.byte2: TPicRegister; inline;
begin
  Result := p2^.rVar.adrByte0;
end;
function TCompOperands.byte2L: TPicRegister; inline;
begin
  Result := p2^.rVar.adrByte0;
end;
function TCompOperands.byte2H: TPicRegister; inline;
begin
  Result := p2^.rVar.adrByte1;
end;
function TCompOperands.byte2E: TPicRegister;
begin
  Result := p2^.rVar.adrByte2;
end;
function TCompOperands.byte2U: TPicRegister;
begin
  Result := p2^.rVar.adrByte3;
end;
function TCompOperands.bit1: TPicRegisterBit; inline;
begin
  Result := p1^.rVar.adrBit;
end;
function TCompOperands.bit2: TPicRegisterBit; inline;
begin
  Result := p2^.rVar.adrBit;
end;
function TCompOperands.stoOperation: TStoOperandsROB;
begin
  //Combinación de los almacenamientos de los operandos
  Result := TStoOperandsROB((Ord(p1^.Sto) << 3) or ord(p2^.Sto));
end;
procedure TCompOperands.ExchangeP1_P2;
{Intercambia el orden de los operandos.}
var
  tmp : ^TOperand;
begin
  //Invierte los operandos
  tmp := p1;
  p1 := p2;
  p2 := tmp;
end;
function TCompOperands.OperationStr(Opt: TxpOperation): string;
{Devuelve una cadena indicando los tipos/alacenamiento y la operación, que se tiene
en "p1", "p2" y "Opt".}
var
  type1, type2: TxpEleType;
  Operat: TxpOperator;
begin
  type1 := Opt.parent.parent;
  type2 := Opt.ToType;
  Operat:= Opt.parent;
  Result := p1^.StoOpStr+' '+type1.name + ' ' + Operat.txt + ' ' + p2^.StoOpStr+' '+type2.name;
end;
//Errors and warnings
procedure TCompOperands.ClearError;
{Limpia la bandera de errores. Tomar en cuenta que solo se debe usar para iniciar el
procesamiento de errores. Limpiar errores en medio de la compilación, podría hacer que
se pierda el rastro de errores anteriores, y que inclusive, la compilación termine sin
error, aún cuando haya generado errores intermedios.
Como norma, se podría decir que solo se debe usar, después de haber proecsado un posible
error anterior.}
begin
  msg.nErrors := 0;
  msg.nInfos := 0;
  msg.nWarns := 0;
end;
function TCompOperands.HayError: boolean;
begin
  exit(msg.nErrors>0);
end;
procedure TCompOperands.GenInfo(txt: string);
{Genera un mensaje de Información, en la posición actual del contexto. }
begin
  msg.info(lex.GetMsgInfo(txt));
end;
procedure TCompOperands.GenWarn(txt: string);
{Genera un mensaje de Advertencia, en la posición actual del contexto. }
begin
  msg.warn(lex.GetMsgInfo(txt));
end;
procedure TCompOperands.GenWarn(txt: string; const Args: array of const);
{Genera un mensaje de Advertencia, en la posición actual del contexto. }
begin
  GenWarn(Format(txt, Args));
end;
procedure TCompOperands.GenWarn(txt: string; const Args: array of const;
  srcPos: TSrcPos);
begin
  msg.warn(lex.GetMsgInfo(Format(txt, Args), srcPos));
end;
//Rutinas de generación de error
procedure TCompOperands.GenError(txt: string; const srcPos: TSrcPos);
{Genera un mensaje de error en la posición indicada.}
begin
  msg.error(lex.GetMsgInfoE(txt, srcPos));
end;
procedure TCompOperands.GenError(txt: String; const Args: array of const;
  const srcPos: TSrcPos);
{Versión con parámetros de GenError.}
begin
  msg.error(lex.GetMsgInfoE(Format(txt, Args), srcPos));
end;
procedure TCompOperands.GenError(txt: string);
{Función de acceso rápido para Perr.GenError(). Pasa como posición a la posición
del contexto actual. Realiza la traducción del mensaje también.}
begin
  msg.error(lex.GetMsgInfoE(txt));
end;
procedure TCompOperands.GenError(txt: String; const Args: array of const);
{Genera un mensaje de error eb la posición actual del contexto.}
begin
  GenError(Format(txt, Args));
end;

constructor TCompOperands.Create(msg0: TMessageManager);
begin
  msg := msg0;
end;
destructor TCompOperands.Destroy;
begin
  inherited Destroy;
end;

end.

