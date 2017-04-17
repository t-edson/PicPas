{Unidad que agrega campos necesarios a la clase TCompilerBase, para la generación de
código con el PIC16F.}
unit GenCodPic;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, XPresParserPIC, XpresElementsPIC, Pic16Utils, XpresTypes,
  MisUtils, SynEditHighlighter, LCLType, LCLProc, fgl;
const
  STACK_SIZE = 8;  //tamaño de pila para subrutinas en el PIC
  MAX_REGS_AUX_BYTE = 4;    //cantidad máxima de registros a usar
  MAX_REGS_AUX_BIT = 4;    //cantidad máxima de registros bit a usar
  MAX_REGS_STACK_BYTE = 4;   //cantidad máxima de registros a usar en la pila
  MAX_REGS_STACK_BIT = 4;   //cantidad máxima de registros a usar en la pila

type
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
  end;
  TPicRegisterBit_list = specialize TFPGObjectList<TPicRegisterBit>; //lista de registros

  { TGenCodPic }

  TGenCodPic = class(TCompilerBase)
  private
    linRep : string;   //línea para generar de reporte
    procedure ProcByteUsed(offs, bnk: byte; regPtr: TPIC16RamCellPtr);
  protected
    pic    : TPIC16;           //Objeto PIC de la serie 16.
    W      : TPicRegister;     //Registro Interno.
    Z      : TPicRegisterBit;  //Registro Interno.
    H      : TPicRegister;     //Registros de trabajo. Se crean siempre.
    listRegAux: TPicRegister_list;  //lista de registros de trabajo y auxiliares
    listRegStk: TPicRegister_list;  //lista de registros de pila
    listRegAuxBit: TPicRegisterBit_list;
    listRegStkBit: TPicRegisterBit_list;
    stackTop: integer;   //índice al límite superior de la pila
    stackTopBit: integer;   //índice al límite superior de la pila
    procedure PutComLine(cmt: string);
    procedure PutComm(cmt: string);
    function ReportRAMusage: string;
    function ValidateByteRange(n: integer): boolean;
    function ValidateWordRange(n: integer): boolean;
    //Métodos para fijar el resultado
    procedure SetResult(typ: TType; CatOp: TCatOperan);
    procedure SetResultConst_bool(valBool: boolean);
    procedure SetResultConst_bit(valBit: boolean);
    procedure SetResultConst_byte(valByte: integer);
    procedure SetResultConst_word(valWord: integer);
    procedure SetResultVariab(typ: TType; rVar: TxpEleVar);
    procedure SetResultExpres(typ: TType);
  private  //Rutinas de gestión de memoria de bajo nivel
    procedure AssignRAM(reg: TPicRegister; regName: string);  //Asigna a una dirección física
    procedure AssignRAM(reg: TPicRegisterBit);  //Asigna a una dirección física
    function CreateRegisterByte(RegType: TPicRegType): TPicRegister;
    function CreateRegisterBit(RegType: TPicRegType): TPicRegisterBit;
  protected  //Rutinas de gestión de memoria
    //Manejo de memoria para registros
    function GetAuxRegisterByte: TPicRegister;
    function GetAuxRegisterBit: TPicRegisterBit;
    function GetStkRegisterByte: TPicRegister;
    function GetStkRegisterBit: TPicRegisterBit;
    function FreeStkRegisterByte(var reg: TPicRegister): boolean;
    function FreeStkRegisterBit(var reg: TPicRegisterBit): boolean;
    function RequireResult_W: TPicRegister;
    function RequireResult_Z: TPicRegisterBit;
    procedure SaveW(out reg: TPicRegister);
    procedure SaveZ(out reg: TPicRegisterBit);
    procedure RestoreW(reg: TPicRegister);
    procedure RestoreZ(reg: TPicRegisterBit);
    procedure RequireH(preserve: boolean = true);
    procedure RequireHW;
    //Manejo de variables
    {Estas rutinas estarían mejor ubicadas en TCompilerBase, pero como dependen del
    objeto "pic", se colocan mejor aquí.}
    function CreateVar(const varName: string; typ: ttype; absAdd: integer=-1;
      absBit: integer=-1): TxpEleVar;
  protected  //Variables de expresión.
    {Estas variables, se inician al inicio de cada expresión y su valor es válido
    hasta el final de la expresión.}
    LastCatOp  : TCatOperan; //Categoría de operando, de la subexpresión anterior.
//    LastBank   : byte;       //Banco de RAM, de la subexpresión anterior.
    CurrBank   : Byte;       //Banco RAM actual
    //Variables de estado de las expresiones booleanas
    BooleanInverted : boolean;  //indica que la lógica del bit de salida está invertida
    BooleanBit : 0..7;          {Indica el bit de STATUS, que se usa, para devolver el
                                 resultado de una expresión booleana o de bit.(Z o C). Por
                                 lo general, estará siempre en 2 = Z.}
  protected  //Instrucciones
    procedure CodAsmFD(const inst: TPIC16Inst; const f: byte; d: TPIC16destin
      );
    procedure CodAsmK(const inst: TPIC16Inst; const k: byte);
    procedure _ADDLW(const k: word);
    procedure _ADDWF(const f: byte; d: TPIC16destin);
    procedure _ANDLW(const k: word);
    procedure _ANDWF(const f: byte; d: TPIC16destin);
    function _BANKSEL(targetBank: byte): byte;
    procedure _BCF(const f, b: byte);
    procedure _BSF(const f, b: byte);
    procedure _BTFSC(const f, b: byte);
    procedure _BTFSS(const f, b: byte);
    procedure _CALL(const a: word);
    procedure _CLRF(const f: byte);
    procedure _CLRW;
    procedure _CLRWDT;
    procedure _COMF(const f: byte; d: TPIC16destin);
    procedure _DECF(const f: byte; d: TPIC16destin);
    procedure _DECFSZ(const f: byte; d: TPIC16destin);
    procedure _GOTO(const a: word);
    procedure _GOTO_PEND(var igot: integer);
    procedure _INCF(const f: byte; d: TPIC16destin);
    procedure _INCFSZ(const f: byte; d: TPIC16destin);
    procedure _IORLW(const k: word);
    procedure _IORWF(const f: byte; d: TPIC16destin);
    procedure _MOVF(const f: byte; d: TPIC16destin);
    procedure _MOVLW(const k: word);
    procedure _MOVWF(const f: byte);
    procedure _NOP;
    procedure _RETFIE;
    procedure _RETLW(const k: word);
    procedure _RETURN;
    procedure _RLF(const f: byte; d: TPIC16destin);
    procedure _RRF(const f: byte; d: TPIC16destin);
    procedure _SLEEP;
    procedure _SUBLW(const k: word);
    procedure _SUBWF(const f: byte; d: TPIC16destin);
    procedure _SWAPF(const f: byte; d: TPIC16destin);
    procedure _XORLW(const k: word);
    procedure _XORWF(const f: byte; d: TPIC16destin);
    function _PC: word;
    function _CLOCK: integer;
  public  //Inicialización
    //Atributos adicionales
    tkStruct   : TSynHighlighterAttributes;
    tkDirective: TSynHighlighterAttributes;
    tkAsm      : TSynHighlighterAttributes;
    tkExpDelim : TSynHighlighterAttributes;
    tkBlkDelim : TSynHighlighterAttributes;
    tkOthers   : TSynHighlighterAttributes;
    procedure StartRegs;
    constructor Create; override;
    destructor Destroy; override;
  end;
const
  //constantes útiles para ensamblador
  _STATUS = $03;
  _C = 0;
  _Z = 2;
  _RP0 = 5;
  _RP1 = 6;
//  _IRP = 7;

var
  /////// Tipos de datos del lenguaje ////////////
  typNull: TType;     //Tipo nulo (sin tipo)
  typBit : TType;
  typBool: TType;     //Booleanos
  typByte: TType;     //número sin signo
  typWord: TType;     //número sin signo
//  tipChr : Ttype;   //un caracter

implementation

{ TPicRegister }
procedure TGenCodPic.ProcByteUsed(offs, bnk: byte; regPtr: TPIC16RamCellPtr);
begin
  linRep := linRep + regPtr^.name +
            ' DB ' + 'bnk'+ IntToStr(bnk) + ':$' + IntToHex(offs, 3) + LineEnding;
end;
procedure TGenCodPic.PutComLine(cmt: string); inline; //agrega comentario al código
begin
  pic.addCommAsm(cmt);  //agrega línea al código ensmblador
end;
procedure TGenCodPic.PutComm(cmt: string); inline; //agrega comentario lateral al código
begin
  pic.addCommAsm1('|'+cmt);  //agrega línea al código ensmblador
end;
function TGenCodPic.ReportRAMusage: string;
{Genera un reporte de uso de la memoria RAM}
begin
  linRep := '';
  pic.ExploreUsed(@ProcByteUsed);
  Result := linRep;
end;
function TGenCodPic.ValidateByteRange(n: integer): boolean;
//Verifica que un valor entero, se pueda convertir a byte. Si no, devuelve FALSE.
begin
  if (n>=0) and (n<256) then
     exit(true)
  else begin
    GenError('Valor numérico excede el rango de un byte.');
    exit(false);
  end;
end;
function TGenCodPic.ValidateWordRange(n: integer): boolean;
//Verifica que un valor entero, se pueda convertir a byte. Si no, devuelve FALSE.
begin
  if (n>=0) and (n<65536) then
     exit(true)
  else begin
    GenError('Valor numérico excede el rango de un word.');
    exit(false);
  end;
end;
procedure TGenCodPic.SetResult(typ: TType; CatOp: TCatOperan);
{Fija los parámetros del resultado de una subexpresion.}
begin
  res.typ := typ;
  res.catOp := CatOp;
  LastCatOp := CatOp;  {Guarda la categoría, para que la siguiente instrucción sepa
                       cuál fue la categoría de la última expresión}
end;
procedure TGenCodPic.SetResultConst_bool(valBool: boolean);
begin
  SetResult(typBool, coConst);
  res.valBool := valBool;
end;
procedure TGenCodPic.SetResultConst_bit(valBit: boolean);
begin
  SetResult(typBit, coConst);
  res.valBool := valBit;
end;
procedure TGenCodPic.SetResultConst_byte(valByte: integer);
begin
  if not ValidateByteRange(valByte) then
    exit;  //Error de rango
  SetResult(typByte, coConst);
  res.valInt := valByte;
end;
procedure TGenCodPic.SetResultConst_word(valWord: integer);
begin
  if not ValidateWordRange(valWord) then
    exit;  //Error de rango
  SetResult(typWord, coConst);
  res.valInt := valWord;
end;
procedure TGenCodPic.SetResultVariab(typ: TType; rVar: TxpEleVar);
begin
  SetResult(typ, coVariab);
  res.rVar := rVar;
end;
procedure TGenCodPic.SetResultExpres(typ: TType);
begin
  SetResult(typ, coExpres);
end;
//Rutinas de gestión de memoria de bajo nivel
procedure TGenCodPic.AssignRAM(reg: TPicRegister; regName: string);
//Asocia a una dirección física de la memoria del PIC.
//Si encuentra error, devuelve el mensaje de error en "MsjError"
begin
  {Esta dirección física, la mantendrá este registro hasta el final de la compilación
  y en teoría, hasta el final de la ejecución de programa en el PIC.}
  if not pic.GetFreeByte(reg.offs, reg.bank) then begin
    GenError('RAM memory is full.');
    exit;
  end;
  pic.SetNameRAM(reg.offs, reg.bank, regName);  //pone nombrea registro
  reg.assigned := true;  //marca como que ya tiene memoria asignada
  reg.used := false;  //aún no se usa
end;
procedure TGenCodPic.AssignRAM(reg: TPicRegisterBit);
begin
  if not pic.GetFreeBit(reg.offs, reg.bank, reg.bit) then begin
    GenError('RAM memory is full.');
    exit;
  end;
//    pic.SetNameRAM(reg.offs, reg.bank, regName);  //pone nombrea registro
  reg.assigned := true;  //marca como que ya tiene memoria asignada
  reg.used := false;  //aún no se usa
end;
function TGenCodPic.CreateRegisterByte(RegType: TPicRegType): TPicRegister;
{Crea una nueva entrada para registro en listRegAux[], pero no le asigna memoria.
 Si encuentra error, devuelve NIL. Este debería ser el único punto de entrada
para agregar un nuevo registro a listRegAux.}
var
  reg: TPicRegister;
begin
  //Agrega un nuevo objeto TPicRegister a la lista;
  reg := TPicRegister.Create;  //Crea objeto
  reg.typ := RegType;    //asigna tipo
  listRegAux.Add(reg);   //agrega a lista
  if listRegAux.Count > MAX_REGS_AUX_BYTE then begin
    //Se asume que se desbordó la memoria evaluando a alguna expresión
    GenError('Very complex expression. To simplify.');
    exit(nil);
  end;
  Result := reg;   //devuelve referencia
end;
function TGenCodPic.CreateRegisterBit(RegType: TPicRegType): TPicRegisterBit;
{Crea una nueva entrada para registro en listRegAux[], pero no le asigna memoria.
 Si encuentra error, devuelve NIL. Este debería ser el único punto de entrada
para agregar un nuevo registro a listRegAux.}
var
  reg: TPicRegisterBit;
begin
  //Agrega un nuevo objeto TPicRegister a la lista;
  reg := TPicRegisterBit.Create;  //Crea objeto
  reg.typ := RegType;    //asigna tipo
  listRegAuxBit.Add(reg);   //agrega a lista
  if listRegAuxBit.Count > MAX_REGS_AUX_BIT then begin
    //Se asume que se desbordó la memoria evaluando a alguna expresión
    GenError('Very complex expression. To simplify.');
    exit(nil);
  end;
  Result := reg;   //devuelve referencia
end;
//Rutinas de Gestión de memoria
function TGenCodPic.GetAuxRegisterByte: TPicRegister;
{Devuelve la dirección de un registro de trabajo libre. Si no encuentra alguno, lo crea.
 Si hay algún error, llama a GenError() y devuelve NIL}
var
  reg: TPicRegister;
  regName: String;
begin
  //Busca en los registros creados
  {Notar que no se incluye en la búsqueda a los registros de trabajo. Esto es por un
  tema de orden, si bien podría ser factible, permitir usar algún registro de trabajo no
  usado, como registro auxiliar.}
  for reg in listRegAux do begin
    //Se supone que todos los registros auxiliares, estarán siempre asignados
    if (reg.typ = prtAuxReg) and not reg.used then
      exit(reg);
  end;
  //No encontró ninguno libre, crea uno en memoria
  reg := CreateRegisterByte(prtAuxReg);
  if reg = nil then exit(nil);  //hubo errir
  regName := 'aux'+IntToSTr(listRegAux.Count);
  AssignRAM(reg, regName);   //Asigna memoria. Puede generar error.
  if HayError then exit;
  Result := reg;   //Devuelve la referencia
end;
function TGenCodPic.GetAuxRegisterBit: TPicRegisterBit;
{Devuelve la dirección de un registro de trabajo libre. Si no encuentra alguno, lo crea.
 Si hay algún error, llama a GenError() y devuelve NIL}
var
  reg: TPicRegisterBit;
begin
  //Busca en los registros creados
  {Notar que no se incluye en la búsqueda a los registros de trabajo. Esto es por un
  tema de orden, si bien podría ser factible, permitir usar algún registro de trabajo no
  usado, como registro auxiliar.}
  for reg in listRegAuxBit do begin
    //Se supone que todos los registros auxiliares, estarán siempre asignados
    if (reg.typ = prtAuxReg) and not reg.used then
      exit(reg);
  end;
  //No encontró ninguno libre, crea uno en memoria
  reg := CreateRegisterBit(prtAuxReg);
  if reg = nil then exit(nil);  //hubo errir
//  regName := 'aux'+IntToSTr(listRegAux.Count);
  AssignRAM(reg);   //Asigna memoria. Puede generar error.
  if HayError then exit;
  Result := reg;   //Devuelve la referencia
end;
function TGenCodPic.GetStkRegisterByte: TPicRegister;
{Pone un registro de un byte, en la pila, de modo que se pueda luego acceder con
FreeStkRegisterByte(). Si hay un error, devuelve NIL.
Notar que esta no es una pila de memoria en el PIC, sino una emulación de pila
en el compilador.}
var
  reg0: TPicRegister;
  regName: String;
begin
  //Validación
  if stackTop>MAX_REGS_STACK_BYTE then begin
    //Se asume que se desbordó la memoria evaluando a alguna expresión
    GenError('Very complex expression. To simplify.');
    exit(nil);
  end;
  if stackTop>listRegStk.Count-1 then begin
    //Apunta a una posición vacía. hay qie agregar
    //Agrega un nuevo objeto TPicRegister a la lista;
    reg0 := TPicRegister.Create;  //Crea objeto
    reg0.typ := prtStkReg;    //asigna tipo
    listRegStk.Add(reg0);   //agrega a lista
    regName := 'stk'+IntToSTr(listRegStk.Count);
    AssignRAM(reg0, regName);   //Asigna memoria. Puede generar error.
    if HayError then exit;
  end;
  Result := listRegStk[stackTop];  //toma registro
  Result.used := true;   //lo marca
  inc(stackTop);  //actualiza
end;
function TGenCodPic.GetStkRegisterBit: TPicRegisterBit;
{Pone un registro de un bit, en la pila, de modo que se pueda luego acceder con
FreeStkRegisterBit(). Si hay un error, devuelve NIL.
Notar que esta no es una pila de memoria en el PIC, sino una emulación de pila
en el compilador.}
var
  reg0: TPicRegisterBit;
begin
  //Validación
  if stackTopBit>MAX_REGS_STACK_BIT then begin
    //Se asume que se desbordó la memoria evaluando a alguna expresión
    GenError('Very complex expression. To simplify.');
    exit(nil);
  end;
  if stackTopBit>listRegStkBit.Count-1 then begin
    //Apunta a una posición vacía. hay qie agregar
    //Agrega un nuevo objeto TPicRegister a la lista;
    reg0 := TPicRegisterBit.Create;  //Crea objeto
    reg0.typ := prtStkReg;    //asigna tipo
    listRegStkBit.Add(reg0);   //agrega a lista
//    regName := 'stk'+IntToSTr(listRegStkBit.Count);
    AssignRAM(reg0);   //Asigna memoria. Puede generar error.
    if HayError then exit;
  end;
  Result := listRegStkBit[stackTopBit];  //toma registro
  Result.used := true;   //lo marca
  inc(stackTopBit);  //actualiza
end;
function TGenCodPic.FreeStkRegisterByte(var reg: TPicRegister): boolean;
{Libera el último byte, que se pidió a la RAM. Devuelve en "reg", la dirección del último
 byte pedido. Si hubo error, devuelve FALSE.
 Liberarlos significa que estarán disponibles, para la siguiente vez que se pidan}
begin
   if stackTop=0 then begin  //Ya está abajo
     GenError('Stack Overflow');
     exit(false);
   end;
   dec(stackTop);   //Baja puntero
   reg := listRegStk[stackTop];  //devuelve referencia
   reg.used := false;   //marca como no usado
   {Notar que, aunque se devuelve la referencia, el registro está libre, para otra
   operación con GetStkRegisterByte(). Tenerlo en cuenta. }
end;
function TGenCodPic.FreeStkRegisterBit(var reg: TPicRegisterBit): boolean;
{Libera el último bit, que se pidió a la RAM. Devuelve en "reg", la dirección del último
 byte pedido. Si hubo error, devuelve FALSE.
 Liberarlos significa que estarán disponibles, para la siguiente vez que se pidan}
begin
   if stackTopBit=0 then begin  //Ya está abajo
     GenError('Stack Overflow');
     exit(false);
   end;
   dec(stackTopBit);   //Baja puntero
   reg := listRegStkBit[stackTopBit];  //devuelve referencia
   reg.used := false;   //marca como no usado
   {Notar que, aunque se devuelve la referencia, el registro está libre, para otra
   operación con GetStkRegisterByte(). Tenerlo en cuenta. }
end;
function TGenCodPic.RequireResult_W: TPicRegister;
{Indica que se va a utilizar al acumulador W, para devolver un resultado de tipo Byte.
De encontrarse ocupado, se pone en la pila y devuelve la referencia al registro de pila
usado. Si no hay espacio, genera error.}
begin
  if W.used then begin
    //Ya lo usó la subexpresión anterior (seguro que fue una expresión byte o word)
    Result := GetStkRegisterByte;  //pide memoria
    //guarda W { TODO : Falta validar el banco }
    _MOVWF(Result.offs);PutComm(';save W');
    Result.used := true;
  end else begin
    Result := nil;
  end;
  W.used := true;   //Lo marca como indicando que se va a ocupar
end;
function TGenCodPic.RequireResult_Z: TPicRegisterBit;
{Indica que se va a utilizar Z, para devolver un resultado de tipo Bit o Boolean.
De encontrarse ocupado, se pone en la pila y devuelve la referencia al registro de pila
usado. Si no hay espacio, genera error.}
begin
  if Z.used then begin
    //Ya lo usó la subexpresión anterior (seguro que fue una expresión bit o boolean)
    Result := GetStkRegisterBit;  //pide memoria
    //guarda Z { TODO : Falta validar el banco }
    _BCF(Result.offs, Result.bit); PutComm(';save Z');
    _BTFSC(Z.offs, Z.bit);
    _BSF(Result.offs, Result.bit);
    Result.used := true;
  end else begin
    Result := nil;
  end;
  Z.used := true;   //Lo marca como indicando que se va a ocupar
end;
procedure TGenCodPic.SaveW(out reg: TPicRegister);
{Verifica si el registro W, está siendo usado y de ser así genera la instrucción para
guardarlo en un registro auxiliar.}
begin
  if W.used then begin
    reg := GetAuxRegisterByte;
    if reg = nil then exit;
    _MOVWF(reg.offs);PutComm(';save W');
  end else begin
    reg := nil;
  end;
end;
procedure TGenCodPic.SaveZ(out reg: TPicRegisterBit);
{Verifica si el registro Z, está siendo usado y de ser así genera la instrucción para
guardarlo en un registro auxiliar.}
begin
  if Z.used then begin
    reg := GetAuxRegisterBit;
    if reg = nil then exit;
    _BCF(reg.offs, reg.bit); PutComm(';save Z');
    _BTFSC(Z.offs, Z.bit);
    _BSF(reg.offs, reg.bit);
  end else begin
    reg := nil;
  end;
end;
procedure TGenCodPic.RestoreW(reg: TPicRegister);
{Devuelve a W, el valor guardado previamente con RequireW}
begin
  if reg<>nil then begin
    reg.used := false;  //libera registro auxiliar
    _MOVF(reg.offs, toW);PutComm(';restore W');
  end;
end;
procedure TGenCodPic.RestoreZ(reg: TPicRegisterBit);
{Devuelve a Z, el valor guardado previamente con RequireZ}
begin
  if reg<>nil then begin
    reg.used := false;  //libera registro auxiliar
    _BCF(Z.offs, Z.bit); PutComm(';restore Z');
    _BTFSC(reg.offs, reg.bit);
    _BSF(Z.offs, Z.bit);
  end;
end;
procedure TGenCodPic.RequireH(preserve: boolean = true);
{Indica que va a usar el registro H.}
var
  tmpReg: TPicRegister;
begin
  if not H.assigned then begin
    //Ni siquiera tiene dirección asignada. Primero hay que ubicarlo en memoria.
    AssignRAM(H, '_H');
  end else begin
    //Ya existe.
    if preserve and H.used then begin
      //Ya lo usó la subexpresión anterior (seguro que fue una expresión de algún tipo)
      tmpReg := GetStkRegisterByte;   //pide una posición de memoria
      //guarda H { TODO : Falta validar el banco }
      _MOVF(H.offs, toW);PutComm(';guarda H');
      _MOVWF(tmpReg.offs);PutComm(';guarda H');
      tmpReg.used := true;   //marca
    end;
  end;
  H.used := true;  //lo marca como que lo va a usar
end;
procedure TGenCodPic.RequireHW;
{Indica que se va a devolver un resultado de tipo Word, en la generación de código.
Debe llamarse siempre, antes de generar código para la subexpresión.}
begin
  RequireResult_W;
  RequireH;
end;
//Manejo de variables
function TGenCodPic.CreateVar(const varName: string; typ: ttype;
         absAdd: integer = -1; absBit: integer = -1): TxpEleVar;
{Rutina para crear variable. Devuelve referencia a la variable creada. Si se especifican
 "absAdd" y/o "absBit", se coloca a la variable en una dirección absoluta.}
var
  r   : TxpEleVar;
  i   : Integer;
  offs, bnk, bit : byte;
begin
  //busca espacio para ubicarla
  if absAdd=-1 then begin
    //Caso normal, sin dirección absoluta.
    if typ.size<0 then begin
      //Se asume que se están pidiendo bits
      if typ.size<>-1 then begin   //por ahora se soporta 1 bit
        GenError('Size of data not supported.');
        exit;
      end;
      if not pic.GetFreeBit(offs, bnk, bit) then begin
        GenError('RAM memory is full.');
        exit;
      end;
    end else begin
      //Se asume que se están pidiendo bytes
      if not pic.GetFreeBytes(typ.size, offs, bnk) then begin
        GenError('RAM memory is full.');
        exit;
      end;
    end;
  end else begin
    //Se debe crear en una posición absoluta
    pic.AbsToBankRAM(absAdd, offs, bnk);   //convierte dirección
    if absBit<>-1 then bit := absBit;      //para los bits no hay transformación
  end;
  //Pone nombre a la celda en RAM, para que pueda desensamblarse con detalle
  if typ.size = 1 then begin
    //Es un simple byte
    pic.SetNameRAM(offs,bnk, varName);
  end else if typ.size = -1 then begin
    //Es un boolean o bit. No pone nombre, porque varias variables pueden compartir este byte.
//    pic.SetNameRAM(offs,bnk, '_map');   //no tiene nombre único
  end else begin
    //Se asume que la variable ha sido entregada con posiciones consecutivas
    for i:=0 to typ.size -1 do
      pic.SetNameRAM(offs+i, bnk, varName+'['+IntToStr(i)+']');
  end;
  //registra variable en la tabla
  r := TxpEleVar.Create;
  r.name := varName;
  r.typ  := typ;   //fija  referencia a tipo
  r.offs := offs;
  r.bank := bnk;
  r.bit  := bit;
  if not TreeElems.AddElement(r) then begin
    GenError('Duplicated identifier: "%s"', [varName]);
    exit;
  end;
  Result := r;
  //Ya encontró tipo, llama a evento
  if typ.OnGlobalDef<>nil then typ.OnGlobalDef(varName, '');
end;
//Rutinas generales para la codificación
procedure TGenCodPic.CodAsmFD(const inst: TPIC16Inst; const f: byte; d: TPIC16destin); inline;
begin
  pic.codAsmFD(inst, f, d);
end;
procedure TGenCodPic.CodAsmK(const inst: TPIC16Inst; const k: byte); inline;
begin
  pic.codAsmK(inst, k);
end;
{procedure CodAsm(const inst: TPIC16Inst; const f, b: byte); inline;
begin
  pic.codAsmFB(inst, f, b);
end;}
//rutinas que facilitan la codifición de instrucciones
procedure TGenCodPic._ADDWF(const f: byte; d: TPIC16destin); inline;
begin
  pic.codAsmFD(ADDWF, f,d);
end;
procedure TGenCodPic._ANDWF(const f: byte; d: TPIC16destin); inline;
begin
  pic.codAsmFD(ANDWF, f,d);
end;
function TGenCodPic._BANKSEL(targetBank: byte): byte;
{Verifica si se está en el banco deseado, de no ser así geenra las instrucciones
 para el cambio de banco.
 Devuelve el número de instrucciones generado.}
begin
  if targetBank = CurrBank then
    exit;
  //se está en un banco diferente
  case CurrBank of
  0: case targetBank of
       1: begin
         _BSF(_STATUS, _RP0);
         CurrBank:=1;
         exit(1);
       end;
       2: begin
         _BSF(_STATUS, _RP1);
         CurrBank:=2;
         exit(1);
       end;
       3: begin
         _BSF(_STATUS, _RP0);
         _BSF(_STATUS, _RP1);
         CurrBank:=3;
         exit(2);
       end;
     end;
  1: case targetBank of
       0: begin
         _BCF(_STATUS, _RP0);
         CurrBank:=0;
         exit(1);
       end;
       2: begin
         _BSF(_STATUS, _RP1);
         _BCF(_STATUS, _RP0);
         CurrBank:=2;
         exit(2);
       end;
       3: begin
         _BSF(_STATUS, _RP1);
         CurrBank:=3;
         exit(1);
       end;
     end;
  2: case targetBank of
       0: begin
         _BCF(_STATUS, _RP1);
         CurrBank:=0;
         exit(1);
       end;
       1: begin
         _BCF(_STATUS, _RP1);
         _BSF(_STATUS, _RP0);
         CurrBank:=1;
         exit(2);
       end;
       3: begin
         _BSF(_STATUS, _RP0);
         CurrBank:=3;
         exit(1);
       end;
     end;
  3: case targetBank of
       0: begin
         _BCF(_STATUS, _RP1);
         _BCF(_STATUS, _RP0);
         CurrBank:=0;
         exit(2);
       end;
       1: begin
         _BCF(_STATUS, _RP1);
         CurrBank:=1;
         exit(1);
       end;
       2: begin
         _BCF(_STATUS, _RP0);
         CurrBank:=2;
         exit(1);
       end;
     end;
  // Este caso es equivalentea decir "no sé en qué banco estoy"
  255: case targetBank of
       0: begin
         _BCF(_STATUS, _RP1);
         _BCF(_STATUS, _RP0);
         CurrBank:=0;
         exit(2);
       end;
       1: begin
         _BCF(_STATUS, _RP1);
         _BSF(_STATUS, _RP0);
         CurrBank:=1;
         exit(2);
       end;
       2: begin
         _BSF(_STATUS, _RP1);
         _BCF(_STATUS, _RP0);
         CurrBank:=2;
         exit(2);
       end;
       3: begin
         _BSF(_STATUS, _RP1);
         _BSF(_STATUS, _RP0);
         CurrBank:=3;
         exit(2);
       end;
     end;
  end;
  //No generó instrucciones
  exit(0);
end;
procedure TGenCodPic._CLRF(const f: byte); inline;
begin
  pic.codAsmF(CLRF, f);
end;
procedure TGenCodPic._CLRW(); inline;
begin
  pic.codAsm(CLRW);
end;
procedure TGenCodPic._COMF(const f: byte; d: TPIC16destin); inline;
begin
  pic.codAsmFD(COMF, f,d);
end;
procedure TGenCodPic._DECF(const f: byte; d: TPIC16destin); inline;
begin
  pic.codAsmFD(DECF, f,d);
end;
procedure TGenCodPic._DECFSZ(const f: byte; d: TPIC16destin); inline;
begin
  pic.codAsmFD(DECFSZ, f,d);
end;
procedure TGenCodPic._INCF(const f: byte; d: TPIC16destin); inline;
begin
  pic.codAsmFD(INCF, f,d);
end;
procedure TGenCodPic._INCFSZ(const f: byte; d: TPIC16destin); inline;
begin
  pic.codAsmFD(INCFSZ, f,d);
end;
procedure TGenCodPic._IORWF(const f: byte; d: TPIC16destin); inline;
begin
  pic.codAsmFD(IORWF, f,d);
end;
procedure TGenCodPic._MOVF(const f: byte; d: TPIC16destin); inline;
begin
  pic.codAsmFD(MOVF, f,d);
end;
procedure TGenCodPic._MOVWF(const f: byte); inline;
begin
  pic.codAsmF(MOVWF, f);
end;
procedure TGenCodPic._NOP(); inline;
begin
  pic.codAsm(NOP);
end;
procedure TGenCodPic._RLF(const f: byte; d: TPIC16destin); inline;
begin
  pic.codAsmFD(RLF, f,d);
end;
procedure TGenCodPic._RRF(const f: byte; d: TPIC16destin); inline;
begin
  pic.codAsmFD(RRF, f,d);
end;
procedure TGenCodPic._SUBWF(const f: byte; d: TPIC16destin); inline;
begin
  pic.codAsmFD(SUBWF, f,d);
end;
procedure TGenCodPic._SWAPF(const f: byte; d: TPIC16destin); inline;
begin
  pic.codAsmFD(SWAPF, f,d);
end;
procedure TGenCodPic._XORWF(const f: byte; d: TPIC16destin); inline;
begin
  pic.codAsmFD(XORWF, f,d);
end;
procedure TGenCodPic._BCF(const f, b: byte); inline;
begin
  pic.codAsmFB(BCF, f, b);
end;
procedure TGenCodPic._BSF(const f, b: byte); inline;
begin
  pic.codAsmFB(BSF, f, b);
end;
procedure TGenCodPic._BTFSC(const f, b: byte); inline;
begin
  pic.codAsmFB(BTFSC, f, b);
end;
procedure TGenCodPic._BTFSS(const f, b: byte); inline;
begin
  pic.codAsmFB(BTFSS, f, b);
end;
procedure TGenCodPic._ADDLW(const k: word); inline;
begin
  pic.codAsmK(ADDLW, k);
end;
procedure TGenCodPic._ANDLW(const k: word); inline;
begin
  pic.codAsmK(ANDLW, k);
end;
procedure TGenCodPic._CALL(const a: word); inline;
begin
  pic.codAsmA(CALL, a);
end;
procedure TGenCodPic._CLRWDT(); inline;
begin
  pic.codAsm(CLRWDT);
end;
procedure TGenCodPic._GOTO(const a: word); inline;
begin
  pic.codAsmA(GOTO_, a);
end;
procedure TGenCodPic._IORLW(const k: word); inline;
begin
  pic.codAsmK(IORLW, k);
end;
procedure TGenCodPic._MOVLW(const k: word); inline;
begin
  pic.codAsmK(MOVLW, k);
end;
procedure TGenCodPic._RETFIE(); inline;
begin
  pic.codAsm(RETFIE);
end;
procedure TGenCodPic._RETLW(const k: word); inline;
begin
  pic.codAsmK(RETLW, k);
end;
procedure TGenCodPic._RETURN(); inline;
begin
  pic.codAsm(RETURN);
end;
procedure TGenCodPic._SLEEP(); inline;
begin
  pic.codAsm(SLEEP);
end;
procedure TGenCodPic._SUBLW(const k: word); inline;
begin
  pic.codAsmK(SUBLW, k);
end;
procedure TGenCodPic._XORLW(const k: word); inline;
begin
  pic.codAsmK(XORLW, k);
end;
procedure TGenCodPic._GOTO_PEND(var  igot: integer);
{Escribe una instrucción GOTO, pero sin precisar el destino aún. Devuelve la dirección
 donde se escribe el GOTO, para poder completarla posteriormente.
}
begin
  igot := pic.iFlash;  //guarda posición de instrucción de salto
  pic.codAsmA(GOTO_, 0);  //pone salto indefinido
end;
function TGenCodPic._PC: word; inline;
{Devuelve la dirección actual en Flash}
begin
  Result := pic.iFlash;
end;
function TGenCodPic._CLOCK: integer; inline;
{Devuelve la frecuencia de reloj del PIC}
begin
  Result := pic.frequen;
end;
//Inicialización
procedure TGenCodPic.StartRegs;
{Inicia los registros de trabajo en la lista.}
begin
  listRegAux.Clear;
  listRegStk.Clear;   //limpia la pila
  stackTop := 0;
  listRegAuxBit.Clear;
  listRegStkBit.Clear;   //limpia la pila
  stackTopBit := 0;
  {Crea registro de trabajo adicional H, para que esté definido, pero aún no tiene
  asignado una posición en memoria.}
  H := CreateRegisterByte(prtWorkReg);
  //Puede salir con error
end;
constructor TGenCodPic.Create;
begin
  inherited Create;
  pic := TPIC16.Create;
  listRegAux := TPicRegister_list.Create(true);
  listRegStk := TPicRegister_list.Create(true);
  listRegAuxBit:= TPicRegisterBit_list.Create(true);
  listRegStkBit:= TPicRegisterBit_list.Create(true);

  stackTop := 0;  //Apunta a la siguiente posición libre
  stackTopBit := 0;  //Apunta a la siguiente posición libre
  {Crea registro de trabajo W. El registro W, es el registro interno del PIC, y no
  necesita un mapeo en RAM. Solo se le crea aquí, para poder usar su propiedad "used"}
  W := TPicRegister.Create;
  W.assigned := false;   //se le marca así, para que no se intente usar
  {Crea registro de trabajo Z. El registro Z, es el registro interno del PIC, y está
  siempre asignado en RAM. }
  Z := TPicRegisterBit.Create;
  Z.offs := _STATUS;
  Z.bit := _Z;
  Z.assigned := true;   //ya está asignado desde el principio
end;
destructor TGenCodPic.Destroy;
begin
  W.Destroy;
  Z.Destroy;
  listRegAuxBit.Destroy;
  listRegStkBit.Destroy;
  listRegStk.Destroy;
  listRegAux.Destroy;
  pic.Destroy;
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
    dicSet(';save W', ';guardar W');
    dicSet(';save Z', ';guardar Z');
    dicSet(';restore W','restaurar W');
    dicSet(';restore Z','restaurar Z');
    dicSet('Size of data not supported.', 'Tamaño de dato no soportado.');
    dicSet('RAM memory is full.', 'Memoria RAM agotada.');
    dicSet('Duplicated identifier: "%s"', 'Identificador duplicado: "%s"');
    dicSet('Undefined type "%s"', 'Tipo "%s" no definido.');
    dicSet('Very complex expression. To simplify.','Expresión muy compleja. Simplificar.');
    dicSet('Stack Overflow', 'Desborde de pila.');
  end;
  end;
end;

end.

