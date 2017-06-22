{Unidad que agrega campos necesarios a la clase TCompilerBase, para la generación de
código con el PIC16F.}
unit GenCodPic;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, XPresParserPIC, XpresElementsPIC, Pic16Utils, XpresTypesPIC,
  MisUtils, LCLType, LCLProc;
const
  STACK_SIZE = 8;  //tamaño de pila para subrutinas en el PIC
  MAX_REGS_AUX_BYTE = 4;    //cantidad máxima de registros a usar
  MAX_REGS_AUX_BIT = 4;    //cantidad máxima de registros bit a usar
  MAX_REGS_STACK_BYTE = 4;   //cantidad máxima de registros a usar en la pila
  MAX_REGS_STACK_BIT = 4;   //cantidad máxima de registros a usar en la pila

type

  { TGenCodPic }

  TGenCodPic = class(TCompilerBase)
  private
    linRep : string;   //línea para generar de reporte
    procedure ProcByteUsed(offs, bnk: byte; regPtr: TPIC16RamCellPtr);
    function OperandsUseHW: boolean;
    function OperandsUseRT(opType: TOperType): boolean;
    function OperandsUseW: boolean;
  protected
    W      : TPicRegister;     //Registro Interno.
    Z      : TPicRegisterBit;  //Registro Interno.
    C      : TPicRegisterBit;  //Registro Interno.
    H      : TPicRegister;     //Registros de trabajo. Se crean siempre.
    listRegAux: TPicRegister_list;  //lista de registros de trabajo y auxiliares
    listRegStk: TPicRegister_list;  //lista de registros de pila
    listRegAuxBit: TPicRegisterBit_list;  //lista de registros de trabajo y auxiliares
    listRegStkBit: TPicRegisterBit_list;
    stackTop: integer;   //índice al límite superior de la pila
    stackTopBit: integer;   //índice al límite superior de la pila
    procedure PutLabel(lbl: string); inline;
    procedure PutTopComm(cmt: string; replace: boolean = true); inline;
    procedure PutComm(cmt: string); inline;
    procedure PutFwdComm(cmt: string); inline;
    procedure AddCaller(elem: TxpElement);
    function ReportRAMusage: string;
    function ValidateByteRange(n: integer): boolean;
    function ValidateWordRange(n: integer): boolean;
    procedure ExchangeP1_P2;
  protected  //Variables de expresión.
    {Estas variables, se inician al inicio de cada expresión y su valor es válido
    hasta el final de la expresión.}
    CurrBank  : Byte;    //Banco RAM actual
    //Variables de estado de las expresiones booleanas
    InvertedFromC: boolean; {Indica que el resultado de una expresión Booleana o Bit, se
                             ha obtenido, en la última subexpresion, copaindo el bit C al
                             bit Z, con inversión lógica. Se usa para opciones de
                             optimziación de código.}
  protected
    {Campo usado para detectar cambios en los bancos de RAM, usando las instrucciones
    _BANKRESET o _BANKSEL}
    BankChanged: boolean;
  protected  //Rutinas de gestión de memoria de bajo nivel
    procedure AssignRAM(reg: TPicRegister; regName: string);  //Asigna a una dirección física
    procedure AssignRAMbit(reg: TPicRegisterBit; regName: string);  //Asigna a una dirección física
    function CreateRegisterByte(RegType: TPicRegType): TPicRegister;
    function CreateRegisterBit(RegType: TPicRegType): TPicRegisterBit;
  protected  //Variables temporales para acceso a campos de variables
    varFields: TxpEleVars;  //lista de variables
    function CreateTmpVar(nam: string; typ: TType): TxpEleVar;
  protected  //Rutinas de gestión de memoria para registros
    varStkBit : TxpEleVar;   //variable bit. Usada para trabajar con la pila
    varStkByte: TxpEleVar;   //variable byte. Usada para trabajar con la pila
    varStkWord: TxpEleVar;   //variable word. Usada para trabajar con la pila
    function GetAuxRegisterByte: TPicRegister;
    function GetAuxRegisterBit: TPicRegisterBit;
    //Gestión de la pila
    function GetStkRegisterByte: TPicRegister;
    function GetStkRegisterBit: TPicRegisterBit;
    function GetVarBitFromStk: TxpEleVar;
    function GetVarByteFromStk: TxpEleVar;
    function GetVarWordFromStk: TxpEleVar;
    function FreeStkRegisterByte(var reg: TPicRegister): boolean;
    function FreeStkRegisterWord: boolean;
    function FreeStkRegisterBit: boolean;
  protected  //Rutinas de gestión de memoria para variables
    {Estas rutinas estarían mejor ubicadas en TCompilerBase, pero como dependen del
    objeto "pic", se colocan mejor aquí.}
    procedure AssignRAMinBit(absAdd, absBit: integer; var reg: TPicRegisterBit; regName: string);
    procedure AssignRAMinByte(absAdd: integer; var reg: TPicRegister; regName: string);
    procedure CreateVarInRAM(nVar: TxpEleVar);
  protected  //Métodos para fijar el resultado
    procedure SetResult(typ: TType; CatOp: TCatOperan);
    procedure SetResultConst_bool(valBool: boolean);
    procedure SetResultConst_bit(valBit: boolean);
    procedure SetResultConst_byte(valByte: integer);
    procedure SetResultConst_char(valByte: integer);
    procedure SetResultConst_word(valWord: integer);

    procedure SetResultVariab_bit(rVar: TxpEleVar; Inverted: boolean);
    procedure SetResultVariab_byte(rVar: TxpEleVar);
    procedure SetResultVariab_char(rVar: TxpEleVar);

    procedure SetResultExpres_bit(opType: TOperType; Inverted: boolean);
    procedure SetResultExpres_bool(opType: TOperType; Inverted: boolean);
    procedure SetResultExpres_byte(opType: TOperType);
    procedure SetResultExpres_char(opType: TOperType);
    procedure SetResultExpres_word(opType: TOperType);
  protected  //Instrucciones
    procedure CodAsmFD(const inst: TPIC16Inst; const f: byte; d: TPIC16destin
      );
    procedure CodAsmK(const inst: TPIC16Inst; const k: byte);
    procedure _ADDLW(const k: word);
    procedure _ADDWF(const f: byte; d: TPIC16destin);
    procedure _ANDLW(const k: word);
    procedure _ANDWF(const f: byte; d: TPIC16destin);
    function  _BANKSEL(targetBank: byte): byte;
    procedure _BANKRESET;
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
    procedure _GOTO_PEND(out igot: integer);
    procedure _LABEL(igot: integer);
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
  public  //Opciones de compilación
    incDetComm: boolean;   //Incluir Comentarios detallados.
    SetProIniBnk: boolean; //Incluir instrucciones de cambio de banco al inicio de procedimientos
    SetProEndBnk: boolean; //Incluir instrucciones de cambio de banco al final de procedimientos
  public  //Acceso a registro de trabajo
    property H_register: TPicRegister read H;
  public  //Inicialización
    function PicName: string;
    function PicNameShort: string;
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

implementation
procedure TGenCodPic.AddCaller(elem: TxpElement);
{Agrega una llamada a un elemento de la sintaxis.}
var
  fc: TxpEleCaller;
begin
  fc:= TxpEleCaller.Create;
  //Carga información del estado actual del parser
  fc.caller := TreeElems.curNode;
  fc.curBnk := CurrBank;
  elem.lstCallers.Add(fc);
end;
{ TGenCodPic }
procedure TGenCodPic.ProcByteUsed(offs, bnk: byte; regPtr: TPIC16RamCellPtr);
begin
  linRep := linRep + regPtr^.name +
            ' DB ' + 'bnk'+ IntToStr(bnk) + ':$' + IntToHex(offs, 3) + LineEnding;
end;
procedure TGenCodPic.PutLabel(lbl: string);
{Agrega uan etiqueta antes de la instrucción. Se recomienda incluir solo el nombre de
la etiqueta, sin ":", ni comentarios, porque este campo se usará para desensamblar.}
begin
  pic.addTopLabel(lbl);  //agrega línea al código ensmblador
end;
procedure TGenCodPic.PutTopComm(cmt: string; replace: boolean = true);
//Agrega comentario al inicio de la posición de memoria
begin
  pic.addTopComm(cmt, replace);  //agrega línea al código ensmblador
end;
procedure TGenCodPic.PutComm(cmt: string);
//Agrega comentario lateral al código. Se llama después de poner la instrucción.
begin
  pic.addSideComm(cmt, true);  //agrega línea al código ensmblador
end;
procedure TGenCodPic.PutFwdComm(cmt: string);
//Agrega comentario lateral al código. Se llama antes de poner la instrucción.
begin
  pic.addSideComm(cmt, false);  //agrega línea al código ensmblador
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
procedure TGenCodPic.ExchangeP1_P2;
{Intercambai el orden de los operandos.}
var
  tmp : ^TOperand;
begin
  //Invierte los operandos
  tmp := p1;
  p1 := p2;
  p2 := tmp;
  //Actualiza catOperation
  catOperation := TCatOperation((Ord(p1^.catOp) << 2) or ord(p2^.catOp));
end;
//Funciones auxiliares privadas
function OperandUseW(Oper: TOperand): boolean;
{Indica si el operando está usando el registro W}
begin
  if (Oper.catOp = coExpres) and
     ((Oper.typ = typByte) or (Oper.typ = typWord)) then
    exit(true)
  else
    exit(false);
end;
function OperandUseH(Oper: TOperand): boolean;
{Indica si el operando está usando el registro H}
begin
  if (Oper.catOp = coExpres) and
     (Oper.typ = typWord) then
    exit(true)
  else
    exit(false);
end;
function OperandUseHW(Oper: TOperand): boolean;
{Indica si el operando está usando los registros H,W}
begin
  if (Oper.catOp = coExpres) and
     (Oper.typ = typWord) then
    exit(true)
  else
    exit(false);
end;
function TGenCodPic.OperandsUseRT(opType: TOperType): boolean;
{Indica si alguno de los operandos, está usando algún registro de trabajo.}
begin
  if (
     (operType = operUnary) and (p1^.catOp = coExpres)
     ) or (
     (operType = operBinary) and ((p1^.catOp = coExpres) or (p2^.catOp = coExpres))
     )
  then exit(true)
  else exit(false);
end;
function TGenCodPic.OperandsUseW: boolean;
{Indica si alguno de los operandos, está usando el registro W, para devolver su
resultado.}
begin
  if (
     (operType = operUnary) and OperandUseW(p1^)
     ) or (
     (operType = operBinary) and (OperandUseW(p1^) or OperandUseW(p2^))
     )
  then exit(true)
  else exit(false);
end;
function TGenCodPic.OperandsUseHW: boolean;
{Indica si alguno de los operandos, está usando los registros H,W, para devolver su
resultado.}
begin
  if (
     (operType = operUnary) and OperandUseHW(p1^)
     ) or (
     (operType = operBinary) and (OperandUseHW(p1^) or OperandUseHW(p2^))
     )
  then exit(true)
  else exit(false);

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
  pic.SetNameRAM(reg.offs, reg.bank, regName);  //pone nombre a registro
  reg.assigned := true;  //marca como que ya tiene memoria asignada
  reg.used := false;  //aún no se usa
end;
procedure TGenCodPic.AssignRAMbit(reg: TPicRegisterBit; regName: string);
begin
  if not pic.GetFreeBit(reg.offs, reg.bank, reg.bit) then begin
    GenError('RAM memory is full.');
    exit;
  end;
  pic.SetNameRAMbit(reg.offs, reg.bank, reg.bit, regName);  //pone nombre a bit
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
function TGenCodPic.CreateTmpVar(nam: string; typ: TType): TxpEleVar;
var
  tmpVar: TxpEleVar;
begin
  tmpVar:= TxpEleVar.Create;
  tmpVar.name := nam;
  tmpVar.typ := typ;
  tmpVar.solAdr := -1;
  tmpVar.solBit := -1;
  varFields.Add(tmpVar);  //agrega
  Result := tmpVar;
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
  regName: String;
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
  regName := 'aux'+IntToSTr(listRegAuxBit.Count);
  AssignRAMbit(reg, regName);   //Asigna memoria. Puede generar error.
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
    reg0.typ := prtStkReg;   //asigna tipo
    listRegStk.Add(reg0);    //agrega a lista
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
  regName: String;
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
    listRegStkBit.Add(reg0);  //agrega a lista
    regName := 'stk'+IntToSTr(listRegStkBit.Count);
    AssignRAMbit(reg0, regName);   //Asigna memoria. Puede generar error.
    if HayError then exit;
  end;
  Result := listRegStkBit[stackTopBit];  //toma registro
  Result.used := true;   //lo marca
  inc(stackTopBit);  //actualiza
end;
function TGenCodPic.GetVarBitFromStk: TxpEleVar;
{Devuelve la referencia a una variable bit, que representa al último bit agregado en
la pila. Se usa como un medio de trabajar con los datos de la pila.}
var
  topreg: TPicRegisterBit;
begin
  topreg := listRegStkBit[stackTopBit-1];  //toma referecnia de registro de la pila
  //Usamos la variable "varStkBit" que existe siempre, para devolver la referencia.
  //Primero la hacemos apuntar a la dirección física de la pila
  varStkBit.adrBit.Assign(topreg);
  //Ahora que tenemos ya la variable configurada, devolvemos la referecnia
  Result := varStkBit;
end;
function TGenCodPic.GetVarByteFromStk: TxpEleVar;
{Devuelve la referencia a una variable byte, que representa al último byte agregado en
la pila. Se usa como un medio de trabajar con los datos de la pila.}
var
  topreg: TPicRegister;
begin
  topreg := listRegStk[stackTop-1];  //toma referecnia de registro de la pila
  //Usamos la variable "varStkByte" que existe siempre, para devolver la referencia.
  //Primero la hacemos apuntar a la dirección física de la pila
  varStkByte.adrByte0.Assign(topReg);
  //Ahora que tenemos ya la variable configurada, devolvemos la referecnia
  Result := varStkByte;
end;
function TGenCodPic.GetVarWordFromStk: TxpEleVar;
{Devuelve la referencia a una variable word, que representa al último word agregado en
la pila. Se usa como un medio de trabajar con los datos de la pila.}
var
  topreg: TPicRegister;
begin
  //Usamos la variable "varStkWord" que existe siempre, para devolver la referencia.
  //Primero la hacemos apuntar a la dirección física de la pila
  topreg := listRegStk[stackTop-1];  //toma referecnia de registro de la pila
  varStkWord.adrByte1.Assign(topreg);
  topreg := listRegStk[stackTop-2];  //toma referecnia de registro de la pila
  varStkWord.adrByte0.Assign(topreg);
  //Ahora que tenemos ya la variable configurada, devolvemos la referecnia
  Result := varStkWord;
end;
function TGenCodPic.FreeStkRegisterBit: boolean;
{Libera el último bit, que se pidió a la RAM. Devuelve en "reg", la dirección del último
 byte pedido. Si hubo error, devuelve FALSE.
 Liberarlos significa que estarán disponibles, para la siguiente vez que se pidan}
begin
   if stackTopBit=0 then begin  //Ya está abajo
     GenError('Stack Overflow');
     exit(false);
   end;
   dec(stackTopBit);   //Baja puntero
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
function TGenCodPic.FreeStkRegisterWord: boolean;
begin
   if stackTop<=1 then begin  //Ya está abajo
     GenError('Stack Overflow');
     exit(false);
   end;
   dec(stackTop, 2);   //Baja puntero
end;
////Rutinas de gestión de memoria para variables
procedure TGenCodPic.AssignRAMinBit(absAdd, absBit: integer;
  var reg: TPicRegisterBit; regName: string);
{Aeigna RAM a un registro o lo coloca en la dirección indicada.}
begin
  //Obtiene los valores de: offs, bnk, y bit, para el alamacenamiento.
  if absAdd=-1 then begin
    //Caso normal, sin dirección absoluta.
    AssignRAMbit(reg, regName);
    reg.used := true;
    //Puede salir con error
  end else begin
    //Se debe crear en una posición absoluta
    pic.AbsToBankRAM(absAdd, reg.offs, reg.bank);   //convierte dirección
    reg.bit := absBit;  //para los bits no hay transformación
    reg.assigned := true;
    reg.used := true;
    //Pone nombre a la celda en RAM, para que pueda desensamblarse con detalle
    pic.SetNameRAMbit(reg.offs, reg.bank, reg.bit, regName);
  end;
end;
procedure TGenCodPic.AssignRAMinByte(absAdd: integer; var reg: TPicRegister;
  regName: string);
{Aeigna RAM a un registro o lo coloca en la dirección indicada.}
begin
  //Obtiene los valores de: offs, bnk, y bit, para el alamacenamiento.
  if absAdd=-1 then begin
    //Caso normal, sin dirección absoluta.
    AssignRAM(reg, regName);
    reg.used := true;
    //Puede salir con error
  end else begin
    //Se debe crear en una posición absoluta
    pic.AbsToBankRAM(absAdd, reg.offs, reg.bank);   //convierte dirección
    reg.assigned := true;
    reg.used := true;
    //Pone nombre a la celda en RAM, para que pueda desensamblarse con detalle
    pic.SetNameRAM(reg.offs, reg.bank, regName);
  end;
end;
procedure TGenCodPic.CreateVarInRAM(nVar: TxpEleVar);
{Rutina para asignar espacio físico a una variable. Funciona también en variables
ABSOLUTE. }
var
  varName: String;
  absAdd: integer;
  absBit: integer;
begin
  //Valores solicitados. Ya deben estar iniciado este campo
  varName := nVar.name;
  absAdd  := nVar.solAdr;  //si no aplica, debe valer -1
  absBit  := nVar.solBit;  //si no aplica, debe valer -1
  if nVar.typ = typBit then begin
    AssignRAMinBit(absAdd, absBit, nVar.adrBit, varName);
  end else if nVar.typ = typBool then begin
    AssignRAMinBit(absAdd, absBit, nVar.adrBit, varName);
  end else if nVar.typ = typByte then begin
    AssignRAMinByte(absAdd, nVar.adrByte0, varName);
  end else if nVar.typ = typChar then begin
    AssignRAMinByte(absAdd, nVar.adrByte0, varName);
  end else if nVar.typ = typWord then begin
    //registra variable en la tabla
    {Asigna espacio para los dos bytes. Notar que:
    1. Si se especifica dirección absoluta, esta se usa solo para el primer byte.
    2. Los dos bytes, no necesariamente serán consecutivos (se toma los que estén libres)}
    AssignRAMinByte(absAdd, nVar.adrByte0, varName+'@0');
    AssignRAMinByte(-1    , nVar.adrByte1, varName+'@1');
  end else begin
    GenError('Not implemented.', [varName]);
  end;
  if HayError then  exit;
  if nVar.typ.OnGlobalDef<>nil then nVar.typ.OnGlobalDef(varName, '');
end;
//Métodos para fijar el resultado
procedure TGenCodPic.SetResult(typ: TType; CatOp: TCatOperan);
{Fija los parámetros del resultado de una subexpresion. Este método se debe ejcutar,
siempre antes de evaluar cada subexpresión, así que es una especie de evento
"OnSubExpresionStart".}
begin
  if incDetComm then begin  //Incluir comentario detallado
    if operType = operBinary then begin
      PutTopComm('      ;Oper(' + p1^.catOpChr + ':' + p1^.typ.name + ',' +
                                  p2^.catOpChr + ':' + p2^.typ.name + ')', false);
    end else begin  //Debe ser unario
      PutTopComm('      ;Oper(' + p1^.catOpChr + ':' + p1^.typ.name + ')', false);
    end;
  end;
  res.typ := typ;
  res.catOp := CatOp;
  InvertedFromC:=false;   //para limpiar el estado
  if CatOp = coExpres then begin
    //Actualiza el estado de los registros de trabajo
    RTstate := typ;
  end;
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
  {Se asume que no se necesita invertir la lógica, en una constante, ya que en este caso
  tenemos control pleno de su valor}
  res.Inverted := false;
end;
procedure TGenCodPic.SetResultConst_byte(valByte: integer);
begin
  if not ValidateByteRange(valByte) then
    exit;  //Error de rango
  SetResult(typByte, coConst);
  res.valInt := valByte;
end;
procedure TGenCodPic.SetResultConst_char(valByte: integer);
begin
  SetResult(typChar, coConst);
  res.valInt := valByte;
end;
procedure TGenCodPic.SetResultConst_word(valWord: integer);
begin
  if not ValidateWordRange(valWord) then
    exit;  //Error de rango
  SetResult(typWord, coConst);
  res.valInt := valWord;
end;
procedure TGenCodPic.SetResultVariab_bit(rVar: TxpEleVar; Inverted: boolean);
begin
  SetResult(typBit, coVariab);
  res.rVar := rVar;
  res.Inverted := Inverted;
end;
procedure TGenCodPic.SetResultVariab_byte(rVar: TxpEleVar);
begin
  SetResult(typByte, coVariab);
  res.rVar := rVar;
end;
procedure TGenCodPic.SetResultVariab_char(rVar: TxpEleVar);
begin
  SetResult(typChar, coVariab);
  res.rVar := rVar;
end;
procedure TGenCodPic.SetResultExpres_bit(opType: TOperType; Inverted: boolean);
{Define el resultado como una expresión de tipo Bit, y se asegura de reservar el registro
Z, para devolver la salida. Debe llamarse cuando se tienen los operandos de
la oepración en p1^ y p2^, porque toma infiormación de allí.}
begin
  //Se van a usar los RT. Verificar si los RT están ocupadoa
  if OperandsUseRT(opType) then begin
    //Alguno de los operandos de la operación actual, está usando algún RT
  end else begin
    {Los RT no están siendo usados, por la operación actual.
     Pero pueden estar ocupados por la operación anterior (Ver doc. técnica).}
    if RTstate<>nil then begin
      //Si se usan RT en la operación anterior. Hay que salvar en pila
      RTstate.SaveToStk;  //Se guardan por tipo
    end else begin
      //No se usan. Están libres
    end;
  end;
  //Fija el resultado
  SetResult(typBit, coExpres);  //actualiza "RTstate"
  res.Inverted := Inverted;
end;
procedure TGenCodPic.SetResultExpres_bool(opType: TOperType; Inverted: boolean);
{Define el resultado como una expresión de tipo Boolean, y se asegura de reservar el
registro Z, para devolver la salida. Debe llamarse cuando se tienen los operandos de
la oepración en p1^y p2^, porque toma infiormación de allí.}
begin
  //Se van a usar los RT. Verificar si los RT están ocupadoa
  if OperandsUseRT(opType) then begin
    //Alguno de los operandos de la operación actual, está usando algún RT
  end else begin
    {Los RT no están siendo usados, por la operación actual.
     Pero pueden estar ocupados por la operación anterior (Ver doc. técnica).}
    if RTstate<>nil then begin
      //Si se usan RT en la operación anterior. Hay que salvar en pila
      RTstate.SaveToStk;  //Se guardan por tipo
    end else begin
      //No se usan. Están libres
    end;
  end;
  //Fija el resultado
  SetResult(typBool, coExpres);  //actualiza "RTstate"
  res.Inverted := Inverted;
end;
procedure TGenCodPic.SetResultExpres_byte(opType: TOperType);
{Define el resultado como una expresión de tipo Byte, y se asegura de reservar el
registro W, para devolver la salida. Debe llamarse cuando se tienen los operandos de
la oepración en p1^y p2^, porque toma información de allí.}
begin
  //Se van a usar los RT. Verificar si los RT están ocupadoa
  if OperandsUseRT(opType) then begin
    //Alguno de los operandos de la operación actual, está usando algún RT
  end else begin
    {Los RT no están siendo usados, por la operación actual.
     Pero pueden estar ocupados por la operación anterior (Ver doc. técnica).}
    if RTstate<>nil then begin
      //Si se usan RT en la operación anterior. Hay que salvar en pila
      RTstate.SaveToStk;  //Se guardan por tipo
    end else begin
      //No se usan. Están libres
    end;
  end;
  //Fija el resultado
  SetResult(typByte, coExpres);  //actualiza "RTstate"
end;
procedure TGenCodPic.SetResultExpres_char(opType: TOperType);
{Define el resultado como una expresión de tipo Char, y se asegura de reservar el
registro W, para devolver la salida. Debe llamarse cuando se tienen los operandos de
la oepración en p1^y p2^, porque toma infiormación de allí.}
begin
  //Se van a usar los RT. Verificar si los RT están ocupadoa
  if OperandsUseRT(opType) then begin
    //Alguno de los operandos de la operación actual, está usando algún RT
  end else begin
    {Los RT no están siendo usados, por la operación actual.
     Pero pueden estar ocupados por la operación anterior (Ver doc. técnica).}
    if RTstate<>nil then begin
      //Si se usan RT en la operación anterior. Hay que salvar en pila
      RTstate.SaveToStk;  //Se guardan por tipo
    end else begin
      //No se usan. Están libres
    end;
  end;
  //Fija el resultado
  SetResult(typChar, coExpres);  //actualiza "RTstate"
end;
procedure TGenCodPic.SetResultExpres_word(opType: TOperType);
{Define el resultado como una expresión de tipo Word, y se asegura de reservar los
registros H,W, para devolver la salida.}
begin
  //Se van a usar los RT. Verificar si los RT están ocupadoa
  if OperandsUseRT(opType) then begin
    //Alguno de los operandos de la operación actual, está usando algún RT
    typWord.DefineRegister;   //Se asegura que exista H
  end else begin
    {Los RT no están siendo usados, por la operación actual.
     Pero pueden estar ocupados por la operación anterior (Ver doc. técnica).}
    if RTstate<>nil then begin
      //Si se usan RT en la operación anterior. Hay que salvar en pila
      RTstate.SaveToStk;  //Se guardan por tipo
    end else begin
      //No se usan. Están libres
    end;
  end;
  //Fija el resultado
  SetResult(typWord, coExpres);
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
procedure TGenCodPic._BANKRESET;
{Reinicia el banco al banco 0, independientemente de donde se pueda encontrar antes.
Siempre genera dos instrucciones. Se usa cuando no se puede predecir exactamente, en que
banco se encontrará el compilador.}
begin
  if pic.NumBanks > 1 then begin
    _BCF(_STATUS, _RP0); PutComm(';Bank reset.');
  end;
  if pic.NumBanks > 2 then begin
    _BCF(_STATUS, _RP1); PutComm(';Bank reset.');
  end;
  CurrBank:=0;
  BankChanged := true;   //Se asume que hubo cambio
end;
function TGenCodPic._BANKSEL(targetBank: byte): byte;
{Verifica si se está en el banco deseado, de no ser así geenra las instrucciones
 para el cambio de banco.
 Devuelve el número de instrucciones generado.}
var
  curRP0: Byte;
  nInst, newRP0, curRP1, newRP1: byte;
begin
  nInst := 0;  //Número de instrucciones usadas pro defecto
  if pic.NumBanks = 1 then
    exit(nInst);  //Caso especial. ¿Hay un PIC de esta serie con un banco?
  if targetBank = CurrBank then
    exit(nInst);  //Ya estamos en el banco pedido
  //Se está en un banco diferente
  if CurrBank = 255 then begin
    //Este caso es equivalente a decir "no sé en qué banco estoy"
    case targetBank of
    0: begin
       _BCF(_STATUS, _RP0); PutComm(';Bank set.');
       inc(nInst);
       if pic.NumBanks > 2 then begin
         _BCF(_STATUS, _RP1); PutComm(';Bank set.');
         inc(nInst);
       end;
     end;
    1: begin
       _BSF(_STATUS, _RP0); PutComm(';Bank set.');
       inc(nInst);
       if pic.NumBanks > 2 then begin
         _BCF(_STATUS, _RP1); PutComm(';Bank set.');
         inc(nInst);
       end;
     end;
    2: begin
       _BCF(_STATUS, _RP0); PutComm(';Bank set.');
       inc(nInst);
       if pic.NumBanks > 2 then begin
         _BSF(_STATUS, _RP1); PutComm(';Bank set.');
         inc(nInst);
       end;
     end;
    3: begin
       _BSF(_STATUS, _RP0); PutComm(';Bank set.');
       inc(nInst);
       if pic.NumBanks > 2 then begin
         _BSF(_STATUS, _RP1); PutComm(';Bank set.');
         inc(nInst);
       end;
     end;
    end;
    CurrBank := targetBank;  //Fija banco actual
    BankChanged := true;
    exit(nInst);
  end else begin
    //Se debe cambiar al banco solicitado
    ////////// Verifica RP0 ////////////
    curRP0 := CurrBank and $01;
    newRP0 := targetBank and $01;
    if curRP0 <> newRP0 then begin
      //Debe haber cambio
      inc(nInst);
      if curRP0 = 0 then begin
        _BSF(_STATUS, _RP0); PutComm(';Bank set.');
      end else begin
        _BCF(_STATUS, _RP0); PutComm(';Bank set.');
      end;
    end;
    ////////// Verifica RP1 ////////////
    curRP1 := CurrBank and $02;
    newRP1 := targetBank and $02;
    if (pic.NumBanks > 2) and (curRP1 <> newRP1) then begin
      //Debe haber cambio
      inc(nInst);
      if curRP1 = 0 then begin
        _BSF(_STATUS, _RP1); PutComm(';Bank set.');
      end else begin
        _BCF(_STATUS, _RP1); PutComm(';Bank set.');
      end;
    end;
    //////////////////////////////////////
    CurrBank := targetBank;
    BankChanged := true;
    exit(nInst);
  end;
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
procedure TGenCodPic._GOTO_PEND(out igot: integer);
{Escribe una instrucción GOTO, pero sin precisar el destino aún. Devuelve la dirección
 donde se escribe el GOTO, para poder completarla posteriormente.
}
begin
  igot := pic.iFlash;  //guarda posición de instrucción de salto
  pic.codAsmA(GOTO_, 0);  //pone salto indefinido
end;
procedure TGenCodPic._LABEL(igot: integer);
{Termina de codiricar el GOTO_PEND}
begin
  pic.codGotoAt(igot, _PC);
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
function TGenCodPic.PicName: string;
begin
  Result := pic.Model;
end;
function TGenCodPic.PicNameShort: string;
{Genera el nombre del PIC, quitándole la parte inicial "PIC".}
begin
  Result := copy(pic.Model, 4, length(pic.Model));
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
  //Crea variables de trabajo
  varStkBit := TxpEleVar.Create;
  varStkBit.typ := typBit;
  varStkByte := TxpEleVar.Create;
  varStkByte.typ := typByte;
  varStkWord := TxpEleVar.Create;
  varStkWord.typ := typWord;
  //Crea lista de variables temporales
  varFields:= TxpEleVars.Create(true);
  //Inicializa contenedores
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
  {Crea registro de trabajo C. El registro C, es el registro interno del PIC, y está
  siempre asignado en RAM. }
  C := TPicRegisterBit.Create;
  C.offs := _STATUS;
  C.bit := _C;
  C.assigned := true;   //ya está asignado desde el principio
end;
destructor TGenCodPic.Destroy;
begin
  C.Destroy;
  Z.Destroy;
  W.Destroy;
  listRegAuxBit.Destroy;
  listRegStkBit.Destroy;
  listRegStk.Destroy;
  listRegAux.Destroy;
  varFields.Destroy;
  varStkBit.Destroy;
  varStkByte.Destroy;
  varStkWord.Destroy;
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
    dicSet(';save H', ';guardar H');
    dicSet(';restore W','restaurar W');
    dicSet(';restore Z','restaurar Z');
    dicSet('Size of data not supported.', 'Tamaño de dato no soportado.');
    dicSet('RAM memory is full.', 'Memoria RAM agotada.');
    dicSet('Duplicated identifier: "%s"', 'Identificador duplicado: "%s"');
    dicSet('Undefined type "%s"', 'Tipo "%s" no definido.');
    dicSet('Very complex expression. To simplify.','Expresión muy compleja. Simplificar.');
    dicSet('Stack Overflow', 'Desborde de pila.');
    dicSet('Not implemented.', 'No implementado');
  end;
  end;
end;

end.

