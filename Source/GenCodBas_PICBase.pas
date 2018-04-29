{Unidad que agrega campos necesarios a la clase TCompilerBase, para la generación de
código con el PIC16F.}
unit GenCodBas_PICBase;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, XpresElementsPIC, XpresTypesPIC, PicCore, PicBaseUtils, Parser,
  MisUtils, LCLType, LCLProc;
const
  STACK_SIZE = 8;      //tamaño de pila para subrutinas en el PIC
  MAX_REGS_AUX_BYTE = 6;   //cantidad máxima de registros a usar
  MAX_REGS_AUX_BIT = 4;    //cantidad máxima de registros bit a usar
  MAX_REGS_STACK_BYTE = 8; //cantidad máxima de registros a usar en la pila
  MAX_REGS_STACK_BIT = 4;  //cantidad máxima de registros a usar en la pila

type
  { TGenCodPic }

  { TGenCod1PICBase }

  TGenCodBas_PICBase = class(TCompilerBase)
  private
    linRep : string;   //línea para generar de reporte
    posFlash: Integer;
    procedure GenCodPicReqStartCodeGen;
    procedure GenCodPicReqStopCodeGen;
    function GetIdxParArray(out WithBrack: boolean; out par: TOperand): boolean;
    function GetValueToAssign(WithBrack: boolean; arrVar: TxpEleVar; out
      value: TOperand): boolean;
    procedure ProcByteUsed(offs, bnk: byte; regPtr: TPICRamCellPtr);
    procedure word_ClearItems(const OpPtr: pointer);
    procedure word_GetItem(const OpPtr: pointer);
    procedure word_SetItem(const OpPtr: pointer);
  protected
    //Registros de trabajo
    W      : TPicRegister;     //Registro Interno.
    Z      : TPicRegisterBit;  //Registro Interno.
    C      : TPicRegisterBit;  //Registro Interno.
    H      : TPicRegister;     //Registros de trabajo. Se crean siempre.
    E      : TPicRegister;     //Registros de trabajo. Se crean siempre.
    U      : TPicRegister;     //Registros de trabajo. Se crean siempre.
    //Registros auxiliares
    INDF   : TPicRegister;     //Registro Interno.
    FSR    : TPicRegister;     //Registro Interno.
    //Listas contenedoras de registros
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
    function ReportRAMusage: string;
    function ValidateByteRange(n: integer): boolean;
    function ValidateWordRange(n: integer): boolean;
    function ValidateDWordRange(n: Int64): boolean;
    procedure ExchangeP1_P2;
  protected
    procedure GenerateROBdetComment;
    procedure GenerateROUdetComment;
  protected  //Rutinas de gestión de memoria de bajo nivel
    procedure AssignRAM(out addr: word; regName: string; shared: boolean);  //Asigna a una dirección física
    procedure AssignRAMbit(out addr: word; out bit: byte; regName: string;
      shared: boolean);  //Asigna a una dirección física
    function CreateRegisterByte(RegType: TPicRegType): TPicRegister;
    function CreateRegisterBit(RegType: TPicRegType): TPicRegisterBit;
  protected  //Variables temporales
    {Estas variables temporales, se crean como forma de acceder a campos de una variable
     como varbyte.bit o varword.low. Se almacenan en "varFields" y se eliminan al final}
    varFields: TxpEleVars;  //Contenedor
    function CreateTmpVar(nam: string; eleTyp: TxpEleType): TxpEleVar;
    {Estas variables se usan para operaciones en el generador de código.
     No se almacenan en "varFields". Así se definió al principio, pero podrían también
     almacenarse, asumiendo que no importe crear variables dinámicas.}
    function NewTmpVarWord(rL, rH: TPicRegister): TxpEleVar;
    function NewTmpVarDword(rL, rH, rE, rU: TPicRegister): TxpEleVar;
  protected  //Rutinas de gestión de memoria para registros
    varStkBit : TxpEleVar;   //variable bit. Usada para trabajar con la pila
    varStkByte: TxpEleVar;   //variable byte. Usada para trabajar con la pila
    varStkWord: TxpEleVar;   //variable word. Usada para trabajar con la pila
    varStkDWord: TxpEleVar;  //variable dword. Usada para trabajar con la pila
    function GetAuxRegisterByte: TPicRegister;
    function GetAuxRegisterBit: TPicRegisterBit;
    //Gestión de la pila
    function GetStkRegisterByte: TPicRegister;
    function GetStkRegisterBit: TPicRegisterBit;
    function GetVarBitFromStk: TxpEleVar;
    function GetVarByteFromStk: TxpEleVar;
    function GetVarWordFromStk: TxpEleVar;
    function GetVarDWordFromStk: TxpEleVar;
    function FreeStkRegisterBit: boolean;
    function FreeStkRegisterByte: boolean;
    function FreeStkRegisterWord: boolean;
    function FreeStkRegisterDWord: boolean;
  protected  //Rutinas de gestión de memoria para variables
    {Estas rutinas estarían mejor ubicadas en TCompilerBase, pero como dependen del
    objeto "pic", se colocan mejor aquí.}
    procedure AssignRAMinBit(absAdd, absBit: integer; var addr: word;
      var bit: byte; regName: string; shared: boolean = false);
    procedure AssignRAMinByte(absAdd: integer; var addr: word; regName: string;
      shared: boolean = false);
    procedure CreateVarInRAM(nVar: TxpEleVar; shared: boolean = false);
  protected  //Métodos para fijar el resultado
    //Métodos básicos
    procedure SetResultNull;
    procedure SetResultConst(typ: TxpEleType);
    procedure SetResultVariab(rVar: TxpEleVar; Inverted: boolean = false);
    procedure SetResultExpres(typ: TxpEleType; ChkRTState: boolean = true);
    procedure SetResultVarRef(rVarBase: TxpEleVar);
    procedure SetResultExpRef(rVarBase: TxpEleVar; typ: TxpEleType; ChkRTState: boolean = true);
    //Fija el resultado de ROB como constante.
    procedure SetROBResultConst_bool(valBool: boolean);
    procedure SetROBResultConst_bit (valBit: boolean);
    procedure SetROBResultConst_byte(valByte: integer);
    procedure SetROBResultConst_char(valByte: integer);
    procedure SetROBResultConst_word(valWord: integer);
    procedure SetROBResultConst_dword(valWord: Int64);
    //Fija el resultado de ROB como variable
    procedure SetROBResultVariab(rVar: TxpEleVar; Inverted: boolean = false);
    //Fija el resultado de ROB como expresión
    {El parámetro "Opt", es más que nada para asegurar que solo se use con Operaciones
     binarias.}
    procedure SetROBResultExpres_bit(Opt: TxpOperation; Inverted: boolean);
    procedure SetROBResultExpres_bool(Opt: TxpOperation; Inverted: boolean);
    procedure SetROBResultExpres_byte(Opt: TxpOperation);
    procedure SetROBResultExpres_char(Opt: TxpOperation);
    procedure SetROBResultExpres_word(Opt: TxpOperation);
    procedure SetROBResultExpres_dword(Opt: TxpOperation);
    //Fija el resultado de ROU
    procedure SetROUResultConst_bit(valBit: boolean);
    procedure SetROUResultConst_byte(valByte: integer);
    procedure SetROUResultVariab(rVar: TxpEleVar; Inverted: boolean = false);
    procedure SetROUResultVarRef(rVarBase: TxpEleVar);
    procedure SetROUResultExpres_bit(Inverted: boolean);
    procedure SetROUResultExpres_byte;
    procedure SetROUResultExpRef(rVarBase: TxpEleVar; typ: TxpEleType);
    //Adicionales
    procedure ChangeResultBitToBool;
    procedure ChangeResultCharToByte;
    function ChangePointerToExpres(var ope: TOperand): boolean;
  protected  //Instrucciones que no manejan el cambio de banco
    procedure CodAsmFD(const inst: TPICBaseInst; const f: byte; d: TPICBaseDestin);
    procedure CodAsmK(const inst: TPICBaseInst; const k: byte);
      procedure _BANKSEL(targetBank: byte);
    procedure GenCodBank(targetAdrr: word);
    procedure _BANKRESET;
    function _PC: word;
    function _CLOCK: integer;
    procedure _LABEL(igot: integer);
    //Instrucciones simples
    procedure _ADDWF(const f: byte; d: TPICBaseDestin);
    procedure _ANDLW(const k: word);
    procedure _ANDWF(const f: byte; d: TPICBaseDestin);
    procedure _BCF(const f, b: byte);
    procedure _BSF(const f, b: byte);
    procedure _BTFSC(const f, b: byte);
    procedure _BTFSS(const f, b: byte);
    procedure _CALL(const a: word);
    procedure _CLRF(const f: byte);
    procedure _CLRW;
    procedure _CLRWDT;
    procedure _COMF(const f: byte; d: TPICBaseDestin);
    procedure _DECF(const f: byte; d: TPICBaseDestin);
    procedure _DECFSZ(const f: byte; d: TPICBaseDestin);
    procedure _GOTO(const a: word);
    procedure _GOTO_PEND(out igot: integer);
    procedure _INCF(const f: byte; d: TPICBaseDestin);
    procedure _INCFSZ(const f: byte; d: TPICBaseDestin);
    procedure _IORLW(const k: word);
    procedure _IORWF(const f: byte; d: TPICBaseDestin);
    procedure _MOVF(const f: byte; d: TPICBaseDestin);
    procedure _MOVLW(const k: word);
    procedure _MOVWF(const f: byte);
    procedure _NOP;
    procedure _RETFIE;
    procedure _RETLW(const k: word);
    procedure _RETURN;
    procedure _RLF(const f: byte; d: TPICBaseDestin);
    procedure _RRF(const f: byte; d: TPICBaseDestin);

    procedure _SLEEP;
    procedure _SUBWF(const f: byte; d: TPICBaseDestin);
    procedure _SWAPF(const f: byte; d: TPICBaseDestin);
    procedure _XORLW(const k: word);
    procedure _XORWF(const f: byte; d: TPICBaseDestin);
    //macros
    procedure _IFZERO;
    procedure _IFNZERO;
  protected  //Instrucciones que manejan el cambio de banco
    procedure kADDWF(const f: word; d: TPICBaseDestin);
    procedure kANDLW(const k: word);
    procedure kANDWF(const f: TPicRegister; d: TPICBaseDestin);
    procedure kBCF(const f: TPicRegisterBit);
    procedure kBSF(const f: TPicRegisterBit);
    procedure kBTFSC(const f: TPicRegisterBit);
    procedure kBTFSS(const f: TPicRegisterBit);
    procedure kCALL(const a: word);
    procedure kCLRF(const f: TPicRegister);
    procedure kCLRW;
    procedure kCLRWDT;
    procedure kCOMF(const f: word; d: TPICBaseDestin);
    procedure kDECF(const f: word; d: TPICBaseDestin);
    procedure kDECFSZ(const f: word; d: TPICBaseDestin);
    procedure kGOTO(const a: word);
    procedure kGOTO_PEND(out igot: integer);
    procedure kINCF(const f: TPicRegister; d: TPICBaseDestin);
    procedure kINCFSZ(const f: word; d: TPICBaseDestin);
    procedure kIORLW(const k: word);
    procedure kIORWF(const f: word; d: TPICBaseDestin);
    procedure kMOVF(const f: TPicRegister; d: TPICBaseDestin);
    procedure kMOVLW(const k: word);
    procedure kMOVWF(const f: TPicRegister);
    procedure kNOP;
    procedure kRETFIE;
    procedure kRETLW(const k: word);
    procedure kRETURN;
    procedure kRLF(const f: TPicRegister; d: TPICBaseDestin);
    procedure kRRF(const f: TPicRegister; d: TPICBaseDestin);
    procedure kSLEEP;
    procedure kSUBWF(const f: word; d: TPICBaseDestin);
    procedure kSWAPF(const f: TPicRegister; d: TPICBaseDestin);
    procedure kXORLW(const k: word);
    procedure kXORWF(const f: TPicRegister; d: TPICBaseDestin);
  public  //Opciones de compilación
    incDetComm  : boolean;   //Incluir Comentarios detallados.
    SetProIniBnk: boolean; //Incluir instrucciones de cambio de banco al inicio de procedimientos
    OptBnkAftPro: boolean; //Incluir instrucciones de cambio de banco al final de procedimientos
    OptBnkAftIF : boolean; //Optimizar instrucciones de cambio de banco al final de IF
    OptReuProVar: boolean; //Optimiza reutilizando variables locales de procedimientos
    OptRetProc  : boolean; //Optimiza el último exit de los procedimeintos.
  public  //Acceso a registro de trabajo
    property ProplistRegAux: TPicRegister_list read listRegAux;
    property ProplistRegAuxBit: TPicRegisterBit_list read listRegAuxBit;
    property H_register: TPicRegister read H;
    property E_register: TPicRegister read E;
    property U_register: TPicRegister read U;
  protected  //Funciones de tipos
    ///////////////// Tipo Bit ////////////////
    procedure bit_LoadToRT(const OpPtr: pointer; modReturn: boolean);
    procedure bit_DefineRegisters;
    procedure bit_SaveToStk;
    //////////////// Tipo Byte /////////////
    procedure byte_LoadToRT(const OpPtr: pointer; modReturn: boolean);
    procedure byte_DefineRegisters;
    procedure byte_SaveToStk;
    procedure byte_GetItem(const OpPtr: pointer);
    procedure byte_SetItem(const OpPtr: pointer);
    procedure byte_ClearItems(const OpPtr: pointer);
    procedure byte_bit(const OpPtr: pointer; nbit: byte);
    procedure byte_bit0(const OpPtr: pointer);
    procedure byte_bit1(const OpPtr: pointer);
    procedure byte_bit2(const OpPtr: pointer);
    procedure byte_bit3(const OpPtr: pointer);
    procedure byte_bit4(const OpPtr: pointer);
    procedure byte_bit5(const OpPtr: pointer);
    procedure byte_bit6(const OpPtr: pointer);
    procedure byte_bit7(const OpPtr: pointer);
    //////////////// Tipo Word /////////////
    procedure word_LoadToRT(const OpPtr: pointer; modReturn: boolean);
    procedure word_DefineRegisters;
    procedure word_SaveToStk;
    procedure word_Low(const OpPtr: pointer);
    procedure word_High(const OpPtr: pointer);
    //////////////// Tipo DWord /////////////
    procedure dword_LoadToRT(const OpPtr: pointer; modReturn: boolean);
    procedure dword_DefineRegisters;
    procedure dword_SaveToStk;
    procedure dword_Extra(const OpPtr: pointer);
    procedure dword_High(const OpPtr: pointer);
    procedure dword_HighWord(const OpPtr: pointer);
    procedure dword_Low(const OpPtr: pointer);
    procedure dword_LowWord(const OpPtr: pointer);
    procedure dword_Ultra(const OpPtr: pointer);
  public  //Inicialización
    pic        : TPICBase;       //Objeto PIC de la serie 16.
    function PicName: string;
    function PicNameShort: string;
    procedure StartRegs;
    constructor Create; override;
    destructor Destroy; override;
  end;
const
  //constantes útiles para ensamblador
  STATUS = $03;
  _C = 0;
  _Z = 2;
  _RP0 = 5;
  _RP1 = 6;
//  _IRP = 7;

implementation

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
    dicSet('No enough RAM', 'No hay suficiente RAM.');
    dicSet('Duplicated identifier: "%s"', 'Identificador duplicado: "%s"');
    dicSet('Undefined type "%s"', 'Tipo "%s" no definido.');
    dicSet('Very complex expression. To simplify.','Expresión muy compleja. Simplificar.');
    dicSet('Stack Overflow', 'Desborde de pila.');
    dicSet('Not implemented.', 'No implementado');
  end;
  end;
end;
{ TGenCodPic }
procedure TGenCodBas_PICBase.ProcByteUsed(offs, bnk: byte;
  regPtr: TPICRamCellPtr);
begin
  linRep := linRep + regPtr^.name +
            ' DB ' + 'bnk'+ IntToStr(bnk) + ':$' + IntToHex(offs, 3) + LineEnding;
end;
procedure TGenCodBas_PICBase.PutLabel(lbl: string);
{Agrega uan etiqueta antes de la instrucción. Se recomienda incluir solo el nombre de
la etiqueta, sin ":", ni comentarios, porque este campo se usará para desensamblar.}
begin
  pic.addTopLabel(lbl);  //agrega línea al código ensmblador
end;
procedure TGenCodBas_PICBase.PutTopComm(cmt: string; replace: boolean = true);
//Agrega comentario al inicio de la posición de memoria
begin
  pic.addTopComm(cmt, replace);  //agrega línea al código ensmblador
end;
procedure TGenCodBas_PICBase.PutComm(cmt: string);
//Agrega comentario lateral al código. Se llama después de poner la instrucción.
begin
  pic.addSideComm(cmt, true);  //agrega línea al código ensmblador
end;
procedure TGenCodBas_PICBase.PutFwdComm(cmt: string);
//Agrega comentario lateral al código. Se llama antes de poner la instrucción.
begin
  pic.addSideComm(cmt, false);  //agrega línea al código ensmblador
end;
function TGenCodBas_PICBase.ReportRAMusage: string;
{Genera un reporte de uso de la memoria RAM}
begin
  linRep := '';
  pic.ExploreUsed(@ProcByteUsed);
  Result := linRep;
end;
function TGenCodBas_PICBase.ValidateByteRange(n: integer): boolean;
//Verifica que un valor entero, se pueda convertir a byte. Si no, devuelve FALSE.
begin
  if (n>=0) and (n<256) then
     exit(true)
  else begin
    GenError('Numeric value exceeds a byte range.');
    exit(false);
  end;
end;
function TGenCodBas_PICBase.ValidateWordRange(n: integer): boolean;
//Verifica que un valor entero, se pueda convertir a byte. Si no, devuelve FALSE.
begin
  if (n>=0) and (n<65536) then
     exit(true)
  else begin
    GenError('Numeric value exceeds a word range.');
    exit(false);
  end;
end;
function TGenCodBas_PICBase.ValidateDWordRange(n: Int64): boolean;
begin
  if (n>=0) and (n<$100000000) then
     exit(true)
  else begin
    GenError('Numeric value exceeds a dword range.');
    exit(false);
  end;
end;
procedure TGenCodBas_PICBase.ExchangeP1_P2;
{Intercambai el orden de los operandos.}
var
  tmp : ^TOperand;
begin
  //Invierte los operandos
  tmp := p1;
  p1 := p2;
  p2 := tmp;
end;
procedure TGenCodBas_PICBase.GenerateROBdetComment;
{Genera un comentario detallado en el código ASM. Válido solo para
Rutinas de Operación binaria, que es cuando está definido operType, p1, y p2.}
begin
  if incDetComm then begin
    PutTopComm('      ;Oper(' + p1^.StoOpChr + ':' + p1^.Typ.name + ',' +
                                p2^.StoOpChr + ':' + p2^.Typ.name + ')', false);
  end;
end;
procedure TGenCodBas_PICBase.GenerateROUdetComment;
{Genera un comentario detallado en el código ASM. Válido solo para
Rutinas de Operación unaria, que es cuando está definido operType, y p1.}
begin
  if incDetComm then begin
    PutTopComm('      ;Oper(' + p1^.StoOpChr + ':' + p1^.Typ.name + ')', false);
  end;
end;
//Funciones auxiliares privadas
function OperandUseW(Oper: TOperand): boolean;
{Indica si el operando está usando el registro W}
begin
  if (Oper.Sto = stExpres) and
     ((Oper.Typ = typByte) or (Oper.Typ = typWord) or (Oper.Typ = typDWord)) then
    exit(true)
  else
    exit(false);
end;
function OperandUseH(Oper: TOperand): boolean;
{Indica si el operando está usando el registro H}
begin
  if (Oper.Sto = stExpres) and
     ((Oper.Typ = typWord) or (Oper.Typ = typDWord)) then
    exit(true)
  else
    exit(false);
end;
function OperandUseHW(Oper: TOperand): boolean;
{Indica si el operando está usando los registros H,W}
begin
  if (Oper.Sto = stExpres) and
     ((Oper.Typ = typWord) or (Oper.Typ = typDWord)) then
    exit(true)
  else
    exit(false);
end;
//Rutinas de gestión de memoria de bajo nivel
procedure TGenCodBas_PICBase.AssignRAM(out addr: word; regName: string; shared: boolean);
//Asocia a una dirección física de la memoria del PIC.
//Si encuentra error, devuelve el mensaje de error en "MsjError"
begin
  {Esta dirección física, la mantendrá este registro hasta el final de la compilación
  y en teoría, hasta el final de la ejecución de programa en el PIC.}
  if not pic.GetFreeByte(addr, shared) then begin
    GenError('No enough RAM');
    exit;
  end;
  pic.SetNameRAM(addr, regName);  //pone nombre a registro
end;
procedure TGenCodBas_PICBase.AssignRAMbit(out addr: word; out bit: byte; regName: string; shared: boolean);
begin
  if not pic.GetFreeBit(addr, bit, shared) then begin
    GenError('No enough RAM');
    exit;
  end;
  pic.SetNameRAMbit(addr, bit, regName);  //pone nombre a bit
end;
function TGenCodBas_PICBase.CreateRegisterByte(RegType: TPicRegType): TPicRegister;
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
function TGenCodBas_PICBase.CreateRegisterBit(RegType: TPicRegType): TPicRegisterBit;
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
function TGenCodBas_PICBase.CreateTmpVar(nam: string; eleTyp: TxpEleType): TxpEleVar;
{Crea una variable temporal agregándola al contenedor varFields, que es
limpiado al iniciar la compilación. Notar que la variable temporal creada, no tiene
RAM asiganda.}
var
  tmpVar: TxpEleVar;
begin
  tmpVar:= TxpEleVar.Create;
  tmpVar.name := nam;
  tmpVar.typ := eleTyp;
  tmpVar.havAdicPar := false;
  tmpVar.IsTmp := true;   //Para que se pueda luego identificar.
  varFields.Add(tmpVar);  //Agrega
  Result := tmpVar;
end;
function TGenCodBas_PICBase.NewTmpVarWord(rL, rH: TPicRegister): TxpEleVar;
{Crea una variable temporal Word, con las direcciones de los registros indicados, y
devuelve la referencia. La variable se crea sin asignación de memoria.}
begin
  Result := TxpEleVar.Create;
  Result.typ := typWord;
  Result.addr0 := rL.addr;  //asigna direcciones
  Result.addr1 := rH.addr;
end;
//Variables temporales
function TGenCodBas_PICBase.NewTmpVarDword(rL, rH, rE, rU: TPicRegister): TxpEleVar;
{Crea una variable temporal DWord, con las direcciones de los registros indicados, y
devuelve la referencia. La variable se crea sin asignación de memoria.}
begin
  Result := TxpEleVar.Create;
  Result.typ := typDWord;
  Result.addr0 := rL.addr;  //asigna direcciones
  Result.addr1 := rH.addr;
  Result.addr2 := rE.addr;
  Result.addr3 := rU.addr;
end;
//Rutinas de Gestión de memoria
function TGenCodBas_PICBase.GetAuxRegisterByte: TPicRegister;
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
    if (reg.typ = prtAuxReg) and not reg.used then begin
      reg.used := true;
      exit(reg);
    end;
  end;
  //No encontró ninguno libre, crea uno en memoria
  reg := CreateRegisterByte(prtAuxReg);
  if reg = nil then exit(nil);  //hubo errir
  regName := 'aux'+IntToSTr(listRegAux.Count);
  AssignRAM(reg.addr, regName, false);   //Asigna memoria. Puede generar error.
  if HayError then exit;
  reg.assigned := true;  //Tiene memoria asiganda
  reg.used := true;  //marca como usado
  Result := reg;   //Devuelve la referencia
end;
function TGenCodBas_PICBase.GetAuxRegisterBit: TPicRegisterBit;
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
  AssignRAMbit(reg.addr, reg.bit, regName, false);   //Asigna memoria. Puede generar error.
  if HayError then exit;
  reg.assigned := true;  //Tiene memoria asiganda
  reg.used := true;  //marca como usado
  Result := reg;   //Devuelve la referencia
end;
function TGenCodBas_PICBase.GetStkRegisterByte: TPicRegister;
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
    AssignRAM(reg0.addr, regName, false);   //Asigna memoria. Puede generar error.
    if HayError then exit(nil);
  end;
  Result := listRegStk[stackTop];  //toma registro
  Result.assigned := true;
  Result.used := true;   //lo marca
  inc(stackTop);  //actualiza
end;
function TGenCodBas_PICBase.GetStkRegisterBit: TPicRegisterBit;
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
    AssignRAMbit(reg0.addr, reg0.bit, regName, false);   //Asigna memoria. Puede generar error.
    if HayError then exit(nil);
  end;
  Result := listRegStkBit[stackTopBit];  //toma registro
  Result.assigned := true;
  Result.used := true;   //lo marca
  inc(stackTopBit);  //actualiza
end;
function TGenCodBas_PICBase.GetVarBitFromStk: TxpEleVar;
{Devuelve la referencia a una variable bit, que representa al último bit agregado en
la pila. Se usa como un medio de trabajar con los datos de la pila.}
var
  topreg: TPicRegisterBit;
begin
  topreg := listRegStkBit[stackTopBit-1];  //toma referecnia de registro de la pila
  //Usamos la variable "varStkBit" que existe siempre, para devolver la referencia.
  //Primero la hacemos apuntar a la dirección física de la pila
  varStkBit.addr0 := topreg.addr;
  varStkBit.bit0 := topreg.bit;
  //Ahora que tenemos ya la variable configurada, devolvemos la referecnia
  Result := varStkBit;
end;
function TGenCodBas_PICBase.GetVarByteFromStk: TxpEleVar;
{Devuelve la referencia a una variable byte, que representa al último byte agregado en
la pila. Se usa como un medio de trabajar con los datos de la pila.}
var
  topreg: TPicRegister;
begin
  topreg := listRegStk[stackTop-1];  //toma referencia de registro de la pila
  //Usamos la variable "varStkByte" que existe siempre, para devolver la referencia.
  //Primero la hacemos apuntar a la dirección física de la pila
  varStkByte.addr0 := topReg.addr;
  //Ahora que tenemos ya la variable configurada, devolvemos la referecnia
  Result := varStkByte;
end;
function TGenCodBas_PICBase.GetVarWordFromStk: TxpEleVar;
{Devuelve la referencia a una variable word, que representa al último word agregado en
la pila. Se usa como un medio de trabajar con los datos de la pila.}
var
  topreg: TPicRegister;
begin
  //Usamos la variable "varStkWord" que existe siempre, para devolver la referencia.
  //Primero la hacemos apuntar a la dirección física de la pila
  topreg := listRegStk[stackTop-1];  //toma referencia de registro de la pila
  varStkWord.addr1 := topreg.addr;
  topreg := listRegStk[stackTop-2];  //toma referencia de registro de la pila
  varStkWord.addr0 := topreg.addr;
  //Ahora que tenemos ya la variable configurada, devolvemos la referencia
  Result := varStkWord;
end;
function TGenCodBas_PICBase.GetVarDWordFromStk: TxpEleVar;
{Devuelve la referencia a una variable Dword, que representa al último Dword agregado en
la pila. Se usa como un medio de trabajar con los datos de la pila.}
var
  topreg: TPicRegister;
begin
  //Usamos la variable "varStkDWord" que existe siempre, para devolver la referencia.
  //Primero la hacemos apuntar a la dirección física de la pila
  topreg := listRegStk[stackTop-1];  //toma referencia de registro de la pila
  varStkDWord.addr3 := topreg.addr;
  topreg := listRegStk[stackTop-2];  //toma referencia de registro de la pila
  varStkDWord.addr2 := topreg.addr;
  topreg := listRegStk[stackTop-3];  //toma referencia de registro de la pila
  varStkDWord.addr1 := topreg.addr;
  topreg := listRegStk[stackTop-4];  //toma referencia de registro de la pila
  varStkDWord.addr0 := topreg.addr;
  //Ahora que tenemos ya la variable configurada, devolvemos la referencia
  Result := varStkDWord;
end;
function TGenCodBas_PICBase.FreeStkRegisterBit: boolean;
{Libera el último bit, que se pidió a la RAM. Si hubo error, devuelve FALSE.
 Liberarlos significa que estarán disponibles, para la siguiente vez que se pidan}
begin
   if stackTopBit=0 then begin  //Ya está abajo
     GenError('Stack Overflow');
     exit(false);
   end;
   dec(stackTopBit);   //Baja puntero
   exit(true);
end;
function TGenCodBas_PICBase.FreeStkRegisterByte: boolean;
{Libera el último byte, que se pidió a la RAM. Devuelve en "reg", la dirección del último
 byte pedido. Si hubo error, devuelve FALSE.
 Liberarlos significa que estarán disponibles, para la siguiente vez que se pidan}
begin
   if stackTop=0 then begin  //Ya está abajo
     GenError('Stack Overflow');
     exit(false);
   end;
   dec(stackTop);   //Baja puntero
   exit(true);
end;
function TGenCodBas_PICBase.FreeStkRegisterWord: boolean;
{Libera el último word, que se pidió a la RAM. Si hubo error, devuelve FALSE.}
begin
   if stackTop<=1 then begin  //Ya está abajo
     GenError('Stack Overflow');
     exit(false);
   end;
   dec(stackTop, 2);   //Baja puntero
   exit(true);
end;
function TGenCodBas_PICBase.FreeStkRegisterDWord: boolean;
{Libera el último dword, que se pidió a la RAM. Si hubo error, devuelve FALSE.}
begin
   if stackTop<=3 then begin  //Ya está abajo
     GenError('Stack Overflow');
     exit(false);
   end;
   dec(stackTop, 4);   //Baja puntero
   exit(true);
end;
////Rutinas de gestión de memoria para variables
procedure TGenCodBas_PICBase.AssignRAMinBit(absAdd, absBit: integer;
  var addr: word; var bit: byte; regName: string; shared: boolean = false);
{Aeigna RAM a un registro o lo coloca en la dirección indicada.}
begin
  //Obtiene los valores de: offs, bnk, y bit, para el alamacenamiento.
  if absAdd=-1 then begin
    //Caso normal, sin dirección absoluta.
    AssignRAMbit(addr, bit, regName, shared);
    //Puede salir con error
  end else begin
    //Se debe crear en una posición absoluta
    addr := absAdd;
    bit := absBit;  //para los bits no hay transformación
    //Pone nombre a la celda en RAM, para que pueda desensamblarse con detalle
    pic.SetNameRAMbit(addr, bit, regName);
  end;
end;
procedure TGenCodBas_PICBase.AssignRAMinByte(absAdd: integer;
  var addr: word; regName: string; shared: boolean = false);
{Asigna RAM a un registro o lo coloca en la dirección indicada.}
begin
  //Obtiene los valores de: offs, bnk, y bit, para el alamacenamiento.
  if absAdd=-1 then begin
    //Caso normal, sin dirección absoluta.
    AssignRAM(addr, regName, shared);
    //Puede salir con error
  end else begin
    //Se debe crear en una posición absoluta
    addr := absAdd;
    //Pone nombre a la celda en RAM, para que pueda desensamblarse con detalle
    pic.SetNameRAM(addr, regName);
  end;
end;
procedure TGenCodBas_PICBase.CreateVarInRAM(nVar: TxpEleVar; shared: boolean = false);
{Rutina para asignar espacio físico a una variable. La variable, es creada en memoria
con los parámetros que posea en ese momento. Si está definida como ABSOLUTE, se le
creará en la posicón indicada. }
var
  varName: String;
  absAdd: integer;
  absBit, nbytes: integer;
  typ: TxpEleType;
  //offs, bnk: byte;
  addr: word;
begin
  //Valores solicitados. Ya deben estar iniciado este campo.
  varName := nVar.name;
  typ := nVar.typ;
  if nVar.adicPar.isAbsol then begin
    absAdd := nVar.adicPar.absAddr;
    if typ.IsBitSize then begin
      absBit := nVar.adicPar.absBit;
    end else begin
      absBit := -1;
    end;
  end else begin
    absAdd  := -1;  //no aplica
    absBit  := -1;  //no aplica
  end;
  //Asigna espacio, de acuerdo al tipo
  if typ = typBit then begin
    AssignRAMinBit(absAdd, absBit, nVar.addr0, nVar.bit0, varName, shared);
  end else if typ = typBool then begin
    AssignRAMinBit(absAdd, absBit, nVar.addr0, nVar.bit0, varName, shared);
  end else if typ = typByte then begin
    AssignRAMinByte(absAdd, nVar.addr0, varName, shared);
  end else if typ = typChar then begin
    AssignRAMinByte(absAdd, nVar.addr0, varName, shared);
  end else if typ = typWord then begin
    //Registra variable en la tabla
    if absAdd = -1 then begin  //Variable normal
      //Los 2 bytes, no necesariamente serán consecutivos (se toma los que estén libres)}
      AssignRAMinByte(-1, nVar.addr0, varName+'@0', shared);
      AssignRAMinByte(-1, nVar.addr1, varName+'@1', shared);
    end else begin             //Variable absoluta
      //Las variables absolutas se almacenarán siempre consecutivas
      AssignRAMinByte(absAdd  , nVar.addr0, varName+'@0');
      AssignRAMinByte(absAdd+1, nVar.addr1, varName+'@1');
    end;
  end else if typ = typDWord then begin
    //Registra variable en la tabla
    if absAdd = -1 then begin  //Variable normal
      //Los 4 bytes, no necesariamente serán consecutivos (se toma los que estén libres)}
      AssignRAMinByte(-1, nVar.addr0, varName+'@0', shared);
      AssignRAMinByte(-1, nVar.addr1, varName+'@1', shared);
      AssignRAMinByte(-1, nVar.addr2, varName+'@2', shared);
      AssignRAMinByte(-1, nVar.addr3, varName+'@3', shared);
    end else begin             //Variable absoluta
      //Las variables absolutas se almacenarán siempre consecutivas
      AssignRAMinByte(absAdd  , nVar.addr0, varName+'@0');
      AssignRAMinByte(absAdd+1, nVar.addr1, varName+'@1');
      AssignRAMinByte(absAdd+2, nVar.addr2, varName+'@2');
      AssignRAMinByte(absAdd+3, nVar.addr3, varName+'@3');
    end;
  end else if typ.catType = tctArray then begin
    //Es un arreglo de algún tipo
    if absAdd<>-1 then begin
      //Se pide mapearlo de forma absoluta
      GenError('Not implemented.', [varName]);
      exit;
    end;
    //Asignamos espacio en RAM
    nbytes := typ.arrSize * typ.refType.size;
    if not pic.GetFreeBytes(nbytes, addr) then begin
      GenError('No enough RAM');
      exit;
    end;
    pic.SetNameRAM(addr, nVar.name);   //Nombre solo al primer byte
    //Fija dirección física. Se usa solamente "addr0", como referencia, porque
    //no se tienen suficientes registros para modelar todo el arreglo.
    nVar.addr0 := addr;
  end else if typ.catType = tctPointer then begin
    //Es un puntero a algún tipo.
    //Los punteros cortos, se manejan como bytes
    AssignRAMinByte(absAdd, nVar.addr0, varName, shared);
  end else begin
    GenError('Not implemented.', [varName]);
  end;
  if HayError then  exit;
  if typ.OnGlobalDef<>nil then typ.OnGlobalDef(varName, '');
end;
//Métodos para fijar el resultado
procedure TGenCodBas_PICBase.SetResultNull;
{Fija el resultado como NULL.}
begin
  res.SetAsNull;
  InvertedFromC:=false;   //para limpiar el estado
  res.Inverted := false;
end;
procedure TGenCodBas_PICBase.SetResultConst(typ: TxpEleType);
{Fija los parámetros del resultado de una subexpresion. Este método se debe ejcutar,
siempre antes de evaluar cada subexpresión.}
begin
  res.SetAsConst(typ);
  InvertedFromC:=false;   //para limpiar el estado
  {Se asume que no se necesita invertir la lógica, en una constante (booleana o bit), ya
  que en este caso, tenemos control pleno de su valor}
  res.Inverted := false;
end;
procedure TGenCodBas_PICBase.SetResultVariab(rVar: TxpEleVar; Inverted: boolean = false);
{Fija los parámetros del resultado de una subexpresion. Este método se debe ejcutar,
siempre antes de evaluar cada subexpresión.}
begin
  res.SetAsVariab(rVar);
  InvertedFromC:=false;   //para limpiar el estado
  //"Inverted" solo tiene sentido, para los tipos bit y boolean
  res.Inverted := Inverted;
end;
procedure TGenCodBas_PICBase.SetResultExpres(typ: TxpEleType; ChkRTState: boolean = true);
{Fija los parámetros del resultado de una subexpresion (en "res"). Este método se debe
ejecutar, siempre antes de evaluar cada subexpresión. Más exactamente, antes de generar
código para ña subexpresión, porque esta rutina puede generar su propio código.}
begin
  if ChkRTState then begin
    //Se pide verificar si se están suando los RT, para salvarlos en la pila.
    if RTstate<>nil then begin
      //Si se usan RT en la operación anterior. Hay que salvar en pila
      RTstate.SaveToStk;  //Se guardan por tipo
    end else begin
      //No se usan. Están libres
    end;
  end;
  //Fija como expresión
  res.SetAsExpres(typ);
  //Limpia el estado. Esto es útil que se haga antes de generar el código para una operación
  InvertedFromC:=false;
  //Actualiza el estado de los registros de trabajo.
  RTstate := typ;
end;
procedure TGenCodBas_PICBase.SetResultVarRef(rVarBase: TxpEleVar);
begin
  res.SetAsVarRef(rVarBase);
  InvertedFromC:=false;   //para limpiar el estado
  //No se usa "Inverted" en este almacenamiento
  res.Inverted := false;
end;
procedure TGenCodBas_PICBase.SetResultExpRef(rVarBase: TxpEleVar; typ: TxpEleType; ChkRTState: boolean = true);
begin
  if ChkRTState then begin
    //Se pide verificar si se están suando los RT, para salvarlos en la pila.
    if RTstate<>nil then begin
      //Si se usan RT en la operación anterior. Hay que salvar en pila
      RTstate.SaveToStk;  //Se guardan por tipo
    end else begin
      //No se usan. Están libres
    end;
  end;
  res.SetAsExpRef(rVarBase, typ);
  InvertedFromC:=false;   //para limpiar el estado
  //No se usa "Inverted" en este almacenamiento
  res.Inverted := false;
end;
//Fija el resultado de ROP como constante
procedure TGenCodBas_PICBase.SetROBResultConst_bool(valBool: boolean);
begin
  GenerateROBdetComment;
  SetResultConst(typBool);
  res.valBool := valBool;
end;
procedure TGenCodBas_PICBase.SetROBResultConst_bit(valBit: boolean);
begin
  GenerateROBdetComment;
  SetResultConst(typBit);
  res.valBool := valBit;
end;
procedure TGenCodBas_PICBase.SetROBResultConst_byte(valByte: integer);
begin
  GenerateROBdetComment;
  if not ValidateByteRange(valByte) then
    exit;  //Error de rango
  SetResultConst(typByte);
  res.valInt := valByte;
end;
procedure TGenCodBas_PICBase.SetROBResultConst_char(valByte: integer);
begin
  GenerateROBdetComment;
  SetResultConst(typChar);
  res.valInt := valByte;
end;
procedure TGenCodBas_PICBase.SetROBResultConst_word(valWord: integer);
begin
  GenerateROBdetComment;
  if not ValidateWordRange(valWord) then
    exit;  //Error de rango
  SetResultConst(typWord);
  res.valInt := valWord;
end;
procedure TGenCodBas_PICBase.SetROBResultConst_dword(valWord: Int64);
begin
  GenerateROBdetComment;
  if not ValidateDWordRange(valWord) then
    exit;  //Error de rango
  SetResultConst(typDWord);
  res.valInt := valWord;
end;
//Fija el resultado de ROP como variable
procedure TGenCodBas_PICBase.SetROBResultVariab(rVar: TxpEleVar; Inverted: boolean);
begin
  GenerateROBdetComment;
  SetResultVariab(rVar, Inverted);
end;
//Fija el resultado de ROP como expresión
procedure TGenCodBas_PICBase.SetROBResultExpres_bit(Opt: TxpOperation; Inverted: boolean);
{Define el resultado como una expresión de tipo Bit, y se asegura de reservar el registro
Z, para devolver la salida. Debe llamarse cuando se tienen los operandos de
la oepración en p1^ y p2^, porque toma infiormación de allí.}
begin
  GenerateROBdetComment;
  //Se van a usar los RT. Verificar si los RT están ocupadoa
  if (p1^.Sto = stExpres) or (p2^.Sto = stExpres) then begin
    //Alguno de los operandos de la operación actual, está usando algún RT
    SetResultExpres(typBit, false);  //actualiza "RTstate"
  end else begin
    {Los RT no están siendo usados, por la operación actual.
     Pero pueden estar ocupados por la operación anterior (Ver doc. técnica).}
    SetResultExpres(typBit);  //actualiza "RTstate"
  end;
  //Fija la lógica
  res.Inverted := Inverted;
end;
procedure TGenCodBas_PICBase.SetROBResultExpres_bool(Opt: TxpOperation; Inverted: boolean);
{Define el resultado como una expresión de tipo Boolean, y se asegura de reservar el
registro Z, para devolver la salida. Debe llamarse cuando se tienen los operandos de
la oepración en p1^y p2^, porque toma infiormación de allí.}
begin
  GenerateROBdetComment;
  //Se van a usar los RT. Verificar si los RT están ocupadoa
  if (p1^.Sto = stExpres) or (p2^.Sto = stExpres) then begin
    //Alguno de los operandos de la operación actual, está usando algún RT
    SetResultExpres(typBool, false);  //actualiza "RTstate"
  end else begin
    {Los RT no están siendo usados, por la operación actual.
     Pero pueden estar ocupados por la operación anterior (Ver doc. técnica).}
    SetResultExpres(typBool);  //actualiza "RTstate"
  end;
  //Fija la lógica
  res.Inverted := Inverted;
end;
procedure TGenCodBas_PICBase.SetROBResultExpres_byte(Opt: TxpOperation);
{Define el resultado como una expresión de tipo Byte, y se asegura de reservar el
registro W, para devolver la salida. Debe llamarse cuando se tienen los operandos de
la oepración en p1^y p2^, porque toma información de allí.}
begin
  GenerateROBdetComment;
  //Se van a usar los RT. Verificar si los RT están ocupadoa
  if (p1^.Sto = stExpres) or (p2^.Sto = stExpres) then begin
    //Alguno de los operandos de la operación actual, está usando algún RT
    SetResultExpres(typByte, false);  //actualiza "RTstate"
  end else begin
    {Los RT no están siendo usados, por la operación actual.
     Pero pueden estar ocupados por la operación anterior (Ver doc. técnica).}
    SetResultExpres(typByte);  //actualiza "RTstate"
  end;
end;
procedure TGenCodBas_PICBase.SetROBResultExpres_char(Opt: TxpOperation);
{Define el resultado como una expresión de tipo Char, y se asegura de reservar el
registro W, para devolver la salida. Debe llamarse cuando se tienen los operandos de
la oepración en p1^y p2^, porque toma infiormación de allí.}
begin
  GenerateROBdetComment;
  //Se van a usar los RT. Verificar si los RT están ocupadoa
  if (p1^.Sto = stExpres) or (p2^.Sto = stExpres) then begin
    //Alguno de los operandos de la operación actual, está usando algún RT
    SetResultExpres(typChar, false);  //actualiza "RTstate"
  end else begin
    {Los RT no están siendo usados, por la operación actual.
     Pero pueden estar ocupados por la operación anterior (Ver doc. técnica).}
    SetResultExpres(typChar);  //actualiza "RTstate"
  end;
end;
procedure TGenCodBas_PICBase.SetROBResultExpres_word(Opt: TxpOperation);
{Define el resultado como una expresión de tipo Word, y se asegura de reservar los
registros H,W, para devolver la salida.}
begin
  GenerateROBdetComment;
  //Se van a usar los RT. Verificar si los RT están ocupadoa
  if (p1^.Sto = stExpres) or (p2^.Sto = stExpres) then begin
    //Alguno de los operandos de la operación actual, está usando algún RT
    SetResultExpres(typWord, false);
  end else begin
    {Los RT no están siendo usados, por la operación actual.
     Pero pueden estar ocupados por la operación anterior (Ver doc. técnica).}
    SetResultExpres(typWord);
  end;
end;
procedure TGenCodBas_PICBase.SetROBResultExpres_dword(Opt: TxpOperation);
{Define el resultado como una expresión de tipo Word, y se asegura de reservar los
registros H,W, para devolver la salida.}
begin
  GenerateROBdetComment;
  //Se van a usar los RT. Verificar si los RT están ocupadoa
  if (p1^.Sto = stExpres) or (p2^.Sto = stExpres) then begin
    //Alguno de los operandos de la operación actual, está usando algún RT
    typDWord.DefineRegister;   //Se asegura que exista H, E y U
    SetResultExpres(typDWord, false);
  end else begin
    {Los RT no están siendo usados, por la operación actual.
     Pero pueden estar ocupados por la operación anterior (Ver doc. técnica).}
    SetResultExpres(typDWord);
  end;
end;
//Fija el resultado de ROU
procedure TGenCodBas_PICBase.SetROUResultConst_bit(valBit: boolean);
begin
  GenerateROUdetComment;
  SetResultConst(typBit);
  res.valBool := valBit;
end;
procedure TGenCodBas_PICBase.SetROUResultConst_byte(valByte: integer);
begin
  GenerateROUdetComment;
  if not ValidateByteRange(valByte) then
    exit;  //Error de rango
  SetResultConst(typByte);
  res.valInt := valByte;
end;
procedure TGenCodBas_PICBase.SetROUResultVariab(rVar: TxpEleVar; Inverted: boolean);
begin
  GenerateROUdetComment;
  SetResultVariab(rVar, Inverted);
end;
procedure TGenCodBas_PICBase.SetROUResultVarRef(rVarBase: TxpEleVar);
{Fija el resultado como una referencia de tipo stVarRefVar}
begin
  GenerateROUdetComment;
  SetResultVarRef(rVarBase);
end;
procedure TGenCodBas_PICBase.SetROUResultExpres_bit(Inverted: boolean);
{Define el resultado como una expresión de tipo Bit, y se asegura de reservar el registro
Z, para devolver la salida. Se debe usar solo para operaciones unarias.}
begin
  GenerateROUdetComment;
  //Se van a usar los RT. Verificar si los RT están ocupadoa
  if (p1^.Sto = stExpres) then begin
    //Alguno de los operandos de la operación actual, está usando algún RT
    SetResultExpres(typBit, false);  //actualiza "RTstate"
  end else begin
    {Los RT no están siendo usados, por la operación actual.
     Pero pueden estar ocupados por la operación anterior (Ver doc. técnica).}
    SetResultExpres(typBit);  //actualiza "RTstate"
  end;
  //Fija la lógica
  res.Inverted := Inverted;
end;
procedure TGenCodBas_PICBase.SetROUResultExpres_byte;
{Define el resultado como una expresión de tipo Byte, y se asegura de reservar el
registro W, para devolver la salida. Se debe usar solo para operaciones unarias.}
begin
  GenerateROUdetComment;
  //Se van a usar los RT. Verificar si los RT están ocupadoa
  if (p1^.Sto = stExpres) then begin
    //Alguno de los operandos de la operación actual, está usando algún RT
    SetResultExpres(typByte, false);  //actualiza "RTstate"
  end else begin
    {Los RT no están siendo usados, por la operación actual.
     Pero pueden estar ocupados por la operación anterior (Ver doc. técnica).}
    SetResultExpres(typByte);  //actualiza "RTstate"
  end;
end;
procedure TGenCodBas_PICBase.SetROUResultExpRef(rVarBase: TxpEleVar; typ: TxpEleType);
{Define el resultado como una expresión stVarRefExp, protegiendo los RT si es necesario.
Se debe usar solo para operaciones unarias.}
begin
  GenerateROUdetComment;
  //Se van a usar los RT. Verificar si los RT están ocupadoa
  if (p1^.Sto = stExpres) then begin
    //Alguno de los operandos de la operación actual, está usando algún RT
    SetResultExpRef(rVarBase, typ, false);  //actualiza "RTstate"
  end else begin
    {Los RT no están siendo usados, por la operación actual.
     Pero pueden estar ocupados por la operación anterior (Ver doc. técnica).}
    SetResultExpRef(rVarBase, typ);  //actualiza "RTstate"
  end;
end;
//Adicionales
procedure TGenCodBas_PICBase.ChangeResultBitToBool;
{Cambia el tipo de dato del resultado (que se supone es Bit), a Boolean.}
var
  tmpVar: TxpEleVar;
begin
  {Lo más fácil sería hacer: res.Typ := typBool;
  pero cuando "res", sea una variable, se estaría cambiando el ¡tipo de la variable!  }
  case res.Sto of
  stConst : res.SetAsConst(typBool);
  stExpres: res.SetAsExpres(typBool);
  stVariab: begin
    {Para el caso de variables es más complejo, porque no se puede modificar su tipo
    real, sino que hay que crear una variable temporal.}
    tmpVar := CreateTmpVar('', typBool);   //crea variable temporal Boolean
    tmpVar.addr0 := res.rVar.addr0;  //apunta al mismo bit
    tmpVar.bit0 := res.rVar.bit0;
    res.SetAsVariab(tmpVar);   //Devuelve boolean
  end;
  end;
end;
procedure TGenCodBas_PICBase.ChangeResultCharToByte;
begin

end;
function TGenCodBas_PICBase.ChangePointerToExpres(var ope: TOperand): boolean;
{Convierte un operando de tipo puntero dereferenciado (x^), en una expresión en los RT,
para que pueda ser evaluado, sin problemas, por las ROP.
Si hay error devuelve false.}
begin
  Result := true;
  if ope.Sto = stVarRefVar then begin
    //Se tiene una variable puntero dereferenciada: x^
    {Convierte en expresión, verificando los RT}
    if RTstate<>nil then begin
      //Si se usan RT en la operación anterior. Hay que salvar en pila
      RTstate.SaveToStk;  //Se guardan por tipo
      if HayError then exit(false);
    end;
    //Llama a rutina que mueve el operando a RT
    LoadToRT(ope);
    if HayError then exit(false);  //Por si no está implementado
    //COnfigura después SetAsExpres(), para que LoadToRT(), sepa el almacenamiento de "op"
    ope.SetAsExpres(ope.Typ);  //"ope.Typ" es el tipo al que apunta
    InvertedFromC:=false;
    RTstate := ope.Typ;
  end else if ope.Sto = stVarRefExp then begin
    //Es una expresión.
    {Se asume que el operando tiene su resultado en los RT. SI estuvieran en la pila
    no se aplicaría.}
    //Llama a rutina que mueve el operando a RT
    LoadToRT(ope);
    if HayError then exit(false);  //Por si no está implementado
    //COnfigura después SetAsExpres(), para que LoadToRT(), sepa el almacenamiento de "op"
    ope.SetAsExpres(ope.Typ);  //"ope.Typ" es el tipo al que apunta
    InvertedFromC:=false;
    RTstate := ope.Typ;
  end;
end;
//Rutinas generales para la codificación
procedure TGenCodBas_PICBase.CodAsmFD(const inst: TPICBaseInst; const f: byte;
  d: TPICBaseDestin);
begin
  pic.codAsmFD(inst, f, d);
end;
procedure TGenCodBas_PICBase.CodAsmK(const inst: TPICBaseInst; const k: byte);
begin
  pic.codAsmK(inst, k);
end;
{procedure CodAsm(const inst: TPIC16Inst; const f, b: byte); inline;
begin
  pic.codAsmFB(inst, f, b);
end;}
//rutinas que facilitan la codifición de instrucciones
procedure TGenCodBas_PICBase._BANKRESET;
{Reinicia el banco al banco 0, independientemente de donde se pueda encontrar antes.
Siempre genera dos instrucciones. Se usa cuando no se puede predecir exactamente, en que
banco se encontrará el compilador.}
begin
  if pic.NumBanks > 1 then begin
    _BCF(STATUS, _RP0); PutComm(';Bank reset.');
  end;
  if pic.NumBanks > 2 then begin
    _BCF(STATUS, _RP1); PutComm(';Bank reset.');
  end;
  CurrBank:=0;
end;
procedure TGenCodBas_PICBase._BANKSEL(targetBank: byte);
{Verifica si se está en el banco deseado, de no ser así genera las instrucciones
 para el cambio de banco.
 Devuelve el número de instrucciones generado.}
var
  curRP0: Byte;
  newRP0, curRP1, newRP1: byte;
begin
  if pic.NumBanks = 1 then
    exit;  //Caso especial. ¿Hay un PIC de esta serie con un banco?
  if targetBank = CurrBank then
    exit;  //Ya estamos en el banco pedido
  //Se está en un banco diferente
  ////////// Verifica RP0 ////////////
  curRP0 := CurrBank and $01;
  newRP0 := targetBank and $01;
  if (CurrBank = 255) or (curRP0 <> newRP0) then begin
    //Debe haber cambio
    if curRP0 = 0 then begin
      _BSF(STATUS, _RP0); PutComm(';Bank set.');
    end else begin
      _BCF(STATUS, _RP0); PutComm(';Bank set.');
    end;
  end;
  //Verifica si ya no hay más bancos
  if pic.NumBanks <= 2 then begin
    CurrBank := targetBank;
    exit;
  end;
  ////////// Verifica RP1 ////////////
  curRP1 := CurrBank and $02;
  newRP1 := targetBank and $02;
  if (CurrBank = 255) or (curRP1 <> newRP1) then begin
    //Debe haber cambio
    if curRP1 = 0 then begin
      _BSF(STATUS, _RP1); PutComm(';Bank set.');
    end else begin
      _BCF(STATUS, _RP1); PutComm(';Bank set.');
    end;
  end;
  //////////////////////////////////////
  CurrBank := targetBank;
  exit;
end;
procedure TGenCodBas_PICBase.GenCodBank(targetAdrr: word);
{Genera código de cambio de banco para acceder a la dirección indicada.
Se debe usar antes de una instrucción que va a acceder a RAM.}
var
  targetBank: byte;
begin
  if targetAdrr and $03f = $000 then exit;   //Mapeada siempre en los 4 bancos
  if targetAdrr and $03f = $004 then exit;   //Mapeada siempre en los 4 bancos
  targetBank := targetAdrr >> 7;
  { TODO : Se debería ver un medio rápido para detectar si la variable "targetAdrr" está
  mapeada, también, en otros bancos y así evitar cambios innecesarios de banco. }
  _BANKSEL(targetBank);
end;

function TGenCodBas_PICBase._PC: word; inline;
{Devuelve la dirección actual en Flash}
begin
  Result := pic.iFlash;
end;
function TGenCodBas_PICBase._CLOCK: integer; inline;
{Devuelve la frecuencia de reloj del PIC}
begin
  Result := pic.frequen;
end;
procedure TGenCodBas_PICBase._LABEL(igot: integer);
{Termina de codificar el GOTO_PEND}
begin
  pic.codGotoAt(igot, _PC);
end;
//Instrucciones simples
{Estas instrucciones no guardan la instrucción compilada en "lastOpCode".}
procedure TGenCodBas_PICBase._ANDLW(const k: word); inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmK(i_ANDLW, k);
end;
procedure TGenCodBas_PICBase._ADDWF(const f: byte; d: TPICBaseDestin);
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_ADDWF, f,d);
end;
procedure TGenCodBas_PICBase._ANDWF(const f: byte; d: TPICBaseDestin);
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_ANDWF, f,d);
end;
procedure TGenCodBas_PICBase._CLRF(const f: byte); inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmF(i_CLRF, f);
end;
procedure TGenCodBas_PICBase._CLRW; inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsm(i_CLRW);
end;
procedure TGenCodBas_PICBase._COMF(const f: byte; d: TPICBaseDestin);
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_COMF, f,d);
end;
procedure TGenCodBas_PICBase._DECF(const f: byte; d: TPICBaseDestin);
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_DECF, f,d);
end;
procedure TGenCodBas_PICBase._DECFSZ(const f: byte; d: TPICBaseDestin);
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_DECFSZ, f,d);
end;
procedure TGenCodBas_PICBase._INCF(const f: byte; d: TPICBaseDestin);
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_INCF, f,d);
end;
procedure TGenCodBas_PICBase._INCFSZ(const f: byte; d: TPICBaseDestin);
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_INCFSZ, f,d);
end;
procedure TGenCodBas_PICBase._IORWF(const f: byte; d: TPICBaseDestin);
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_IORWF, f,d);
end;
procedure TGenCodBas_PICBase._MOVF(const f: byte; d: TPICBaseDestin);
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_MOVF, f,d);
end;
procedure TGenCodBas_PICBase._MOVWF(const f: byte);
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmF(i_MOVWF, f);
end;
procedure TGenCodBas_PICBase._NOP; inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsm(i_NOP);
end;
procedure TGenCodBas_PICBase._RLF(const f: byte; d: TPICBaseDestin);
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_RLF, f,d);
end;
procedure TGenCodBas_PICBase._RRF(const f: byte; d: TPICBaseDestin);
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_RRF, f,d);
end;
procedure TGenCodBas_PICBase._SUBWF(const f: byte; d: TPICBaseDestin);
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_SUBWF, f,d);
end;
procedure TGenCodBas_PICBase._SWAPF(const f: byte; d: TPICBaseDestin);
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_SWAPF, f,d);
end;
procedure TGenCodBas_PICBase._BCF(const f, b: byte); inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFB(i_BCF, f, b);
end;
procedure TGenCodBas_PICBase._BSF(const f, b: byte); //inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFB(i_BSF, f, b);
end;
procedure TGenCodBas_PICBase._BTFSC(const f, b: byte); inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFB(i_BTFSC, f, b);
end;
procedure TGenCodBas_PICBase._BTFSS(const f, b: byte); inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFB(i_BTFSS, f, b);
end;
procedure TGenCodBas_PICBase._CALL(const a: word); inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmA(i_CALL, a);
end;
procedure TGenCodBas_PICBase._CLRWDT; inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsm(i_CLRWDT);
end;
procedure TGenCodBas_PICBase._GOTO(const a: word); inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmA(i_GOTO, a);
end;
procedure TGenCodBas_PICBase._GOTO_PEND(out igot: integer);
{Escribe una instrucción GOTO, pero sin precisar el destino aún. Devuelve la dirección
 donde se escribe el GOTO, para poder completarla posteriormente.
}
begin
  igot := pic.iFlash;  //guarda posición de instrucción de salto
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmA(i_GOTO, 0);  //pone salto indefinido
end;
procedure TGenCodBas_PICBase._IORLW(const k: word); inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmK(i_IORLW, k);
end;
procedure TGenCodBas_PICBase._MOVLW(const k: word); inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmK(i_MOVLW, k);
end;
procedure TGenCodBas_PICBase._RETFIE; inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsm(i_RETFIE);
end;
procedure TGenCodBas_PICBase._RETLW(const k: word); inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmK(i_RETLW, k);
end;
procedure TGenCodBas_PICBase._RETURN;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsm(i_RETURN);
end;
procedure TGenCodBas_PICBase._SLEEP; inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsm(i_SLEEP);
end;
procedure TGenCodBas_PICBase._XORLW(const k: word); inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmK(i_XORLW, k);
end;
procedure TGenCodBas_PICBase._XORWF(const f: byte; d: TPICBaseDestin);
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_XORWF, f,d);
end;
procedure TGenCodBas_PICBase._IFZERO;
begin
  _BTFSC(STATUS, _Z);
end;
procedure TGenCodBas_PICBase._IFNZERO;
begin
  _BTFSS(STATUS, _Z);
end;
//Instrucciones que manejan el cambio de banco
{Estas instrucciones guardan la instrucción compilada en "lastOpCode".}
procedure TGenCodBas_PICBase.kADDWF(const f: word; d: TPICBaseDestin);
begin
  GenCodBank(f);
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_ADDWF, f,d);
end;
procedure TGenCodBas_PICBase.kANDLW(const k: word); inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmK(i_ANDLW, k);
end;
procedure TGenCodBas_PICBase.kANDWF(const f: TPicRegister; d: TPICBaseDestin);
begin
  GenCodBank(f.addr);
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_ANDWF, f.addr, d);
end;
procedure TGenCodBas_PICBase.kCLRF(const f: TPicRegister); inline;
begin
  GenCodBank(f.addr);
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmF(i_CLRF, f.addr);
end;
procedure TGenCodBas_PICBase.kCLRW;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsm(i_CLRW);
end;
procedure TGenCodBas_PICBase.kCOMF(const f: word; d: TPICBaseDestin);
begin
  GenCodBank(f);
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_COMF, f,d);
end;
procedure TGenCodBas_PICBase.kDECF(const f: word; d: TPICBaseDestin);
begin
  GenCodBank(f);
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_DECF, f,d);
end;
procedure TGenCodBas_PICBase.kDECFSZ(const f: word; d: TPICBaseDestin);
begin
  GenCodBank(f);
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_DECFSZ, f,d);
end;
procedure TGenCodBas_PICBase.kINCF(const f: TPicRegister; d: TPICBaseDestin);
begin
  GenCodBank(f.addr);
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_INCF, f.addr, d);
end;
procedure TGenCodBas_PICBase.kINCFSZ(const f: word; d: TPICBaseDestin);
begin
  GenCodBank(f);
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_INCFSZ, f,d);
end;
procedure TGenCodBas_PICBase.kIORWF(const f: word; d: TPICBaseDestin);
begin
  GenCodBank(f);
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_IORWF, f,d);
end;
procedure TGenCodBas_PICBase.kMOVF(const f: TPicRegister; d: TPICBaseDestin);
begin
  GenCodBank(f.addr);
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_MOVF, f.addr, d);
end;
procedure TGenCodBas_PICBase.kMOVWF(const f: TPicRegister); inline;
begin
  GenCodBank(f.addr);
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmF(i_MOVWF, f.addr);
end;
procedure TGenCodBas_PICBase.kNOP; inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsm(i_NOP);
end;
procedure TGenCodBas_PICBase.kRLF(const f: TPicRegister; d: TPICBaseDestin);
begin
  GenCodBank(f.addr);
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_RLF, f.addr, d);
end;
procedure TGenCodBas_PICBase.kRRF(const f: TPicRegister; d: TPICBaseDestin);
begin
  GenCodBank(f.addr);
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_RRF, f.addr, d);
end;
procedure TGenCodBas_PICBase.kSUBWF(const f: word; d: TPICBaseDestin);
begin
  GenCodBank(f);
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_SUBWF, f,d);
end;
procedure TGenCodBas_PICBase.kSWAPF(const f: TPicRegister; d: TPICBaseDestin);
begin
  GenCodBank(f.addr);
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_SWAPF, f.addr, d);
end;
procedure TGenCodBas_PICBase.kBCF(const f: TPicRegisterBit);
begin
  GenCodBank(f.addr);
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFB(i_BCF, f.addr, f.bit);
end;
procedure TGenCodBas_PICBase.kBSF(const f: TPicRegisterBit);
begin
  GenCodBank(f.addr);
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFB(i_BSF, f.addr, f.bit);
end;
procedure TGenCodBas_PICBase.kBTFSC(const f: TPicRegisterBit);
begin
  GenCodBank(f.addr);
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFB(i_BTFSC, f.addr, f.bit);
end;
procedure TGenCodBas_PICBase.kBTFSS(const f: TPicRegisterBit);
begin
  GenCodBank(f.addr);
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFB(i_BTFSS, f.addr, f.bit);
end;
procedure TGenCodBas_PICBase.kCALL(const a: word); inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmA(i_CALL, a);
end;
procedure TGenCodBas_PICBase.kCLRWDT; inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsm(i_CLRWDT);
end;
procedure TGenCodBas_PICBase.kGOTO(const a: word); inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmA(i_GOTO, a);
end;
procedure TGenCodBas_PICBase.kGOTO_PEND(out igot: integer);
{Escribe una instrucción GOTO, pero sin precisar el destino aún. Devuelve la dirección
 donde se escribe el GOTO, para poder completarla posteriormente.
}
begin
  igot := pic.iFlash;  //guarda posición de instrucción de salto
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmA(i_GOTO, 0);  //pone salto indefinido
end;
procedure TGenCodBas_PICBase.kIORLW(const k: word); inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmK(i_IORLW, k);
end;
procedure TGenCodBas_PICBase.kMOVLW(const k: word); inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmK(i_MOVLW, k);
end;
procedure TGenCodBas_PICBase.kRETFIE;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsm(i_RETFIE);
end;
procedure TGenCodBas_PICBase.kRETLW(const k: word); inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmK(i_RETLW, k);
end;
procedure TGenCodBas_PICBase.kRETURN; inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsm(i_RETURN);
end;
procedure TGenCodBas_PICBase.kSLEEP; inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsm(i_SLEEP);
end;
procedure TGenCodBas_PICBase.kXORLW(const k: word); inline;
begin
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmK(i_XORLW, k);
end;
procedure TGenCodBas_PICBase.kXORWF(const f: TPicRegister; d: TPICBaseDestin);
begin
  GenCodBank(f.addr);
  pic.flash[pic.iFlash].curBnk := CurrBank;
  pic.codAsmFD(i_XORWF, f.addr, d);
end;

function TGenCodBas_PICBase.PicName: string;
begin
  Result := pic.Model;
end;
function TGenCodBas_PICBase.PicNameShort: string;
{Genera el nombre del PIC, quitándole la parte inicial "PIC".}
begin
  Result := copy(pic.Model, 4, length(pic.Model));
end;
function TGenCodBas_PICBase.GetIdxParArray(out WithBrack: boolean; out par: TOperand): boolean;
{Extrae el primer parámetro (que corresponde al índice) de las funciones getitem() o
setitem(). También reconoce las formas con corchetes [], y en ese caso pone "WithBrackets"
en TRUE. Si encuentra error, devuelve false.}
begin
  if cIn.tok = '[' then begin
    //Es la sintaxis a[i];
    WithBrack := true;
    cIn.Next;  //Toma "["
  end else begin
    //Es la sintaxis a.item(i);
    WithBrack := false;
    cIn.Next;  //Toma identificador de campo
    //Captura parámetro
    if not CaptureTok('(') then exit(false);
  end;
  par := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
  if HayError then exit(false);
  if par.Typ <> typByte then begin
    GenError('Expected byte as index.');
  end;
  if HayError then exit(false);
  exit(true);
end;
function TGenCodBas_PICBase.GetValueToAssign(WithBrack: boolean; arrVar: TxpEleVar; out value: TOperand): boolean;
{Lee el segundo parámetro de SetItem y devuelve en "value". Valida que sea sel tipo
correcto. Si hay error, devuelve FALSE.}
var
  typItem: TxpEleType;
begin
  if WithBrack then begin
    if not CaptureTok(']') then exit(false);
    cIn.SkipWhites;
    {Legalmente, aquí podría seguir otro operador, o función como ".bit0", y no solo
    ":=". Esto es una implementación algo limitada. Lo que debería hacerse, si no se
    encuentra ":=", sería devolver una referencia a variable, tal vez a un nuevo tipo
    de variable, con dirección "indexada", pero obligaría a crear un atributo más a
    las varaibles. El caso de un índice constante es más sencillo de procesar.}
    if not CaptureTok(':=') then exit(false);
  end else begin
    if not CaptureTok(',') then exit(false);
  end;
  value := GetExpression(0);  //Captura parámetro. No usa GetExpressionE, para no cambiar RTstate
  typItem := arrVar.typ.refType;
  if value.Typ <> typItem then begin  //Solo debería ser byte o char
    if (value.Typ = typByte) and (typItem = typWord) then begin
      //Son tipos compatibles
      value.SetAsConst(typWord);   //Cmabiamos el tipo
    end else begin
      GenError('%s expression expected.', [typItem.name]);
      exit(false);
    end;
  end;
  exit(true);
end;
///////////////// Tipo Bit ////////////////
procedure TGenCodBas_PICBase.bit_LoadToRT(const OpPtr: pointer; modReturn: boolean);
{Carga operando a registros Z.}
var
  Op: ^TOperand;
begin
  Op := OpPtr;
  case Op^.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    if Op^.valBool then
      _BSF(Z.offs, Z.bit)
    else
      _BCF(Z.offs, Z.bit);
  end;
  stVariab: begin
    //La lógica en Z, dene ser normal, proque no hay forma de leerla.
    //Como Z, está en todos los bancos, no hay mucho problema.
    if Op^.Inverted then begin
      //No se usa el registro W
      kBCF(Z);
      kBTFSS(Op^.rVar.adrBit);
      kBSF(Z);
    end else begin
      //No se usa el registro W
      kBCF(Z);
      kBTFSC(Op^.rVar.adrBit);
      kBSF(Z);
    end;
  end;
  stExpres: begin  //ya está en w
    if Op^.Inverted then begin
      //Aquí hay un problema, porque hay que corregir la lógica
      _MOVLW($1 << Z.bit);
      _ANDWF(Z.offs, toW);  //invierte Z
    end else begin
      //No hay mada que hacer
    end;
  end;
  else
    //Almacenamiento no implementado
    GenError('Not implemented.');
  end;
  if modReturn then _RETURN;  //codifica instrucción
end;
procedure TGenCodBas_PICBase.bit_DefineRegisters;
begin
  //No es encesario, definir registros adicionales a W
end;
procedure TGenCodBas_PICBase.bit_SaveToStk;
{Guarda el valor bit, cargado actualmente en Z, a pila.}
var
  stk: TPicRegisterBit;
begin
  stk := GetStkRegisterBit;  //pide memoria
  if stk= nil then exit;   //error
  //Guarda Z
  _BANKSEL(stk.bank);
  _BCF(stk.offs, stk.bit); PutComm(';save Z');
  _BTFSC(Z.offs, Z.bit); PutComm(';save Z');
  _BSF(stk.offs, stk.bit); PutComm(';save Z');
  stk.used := true;
end;
//////////////// Tipo Byte /////////////
procedure TGenCodBas_PICBase.byte_LoadToRT(const OpPtr: pointer; modReturn: boolean);
{Carga operando a registros de trabajo.}
var
  Op: ^TOperand;
  varPtr: TxpEleVar;
begin
  Op := OpPtr;
  case Op^.Sto of  //el parámetro debe estar en "res"
  stConst : begin
    if modReturn then _RETLW(Op^.valInt)
    else _MOVLW(Op^.valInt);
  end;
  stVariab: begin
    kMOVF(Op^.rVar.adrByte0, toW);
    if modReturn then _RETURN;
  end;
  stExpres: begin  //ya está en w
    if modReturn then _RETURN;
  end;
  stVarRefVar: begin
    //Se tiene una variable puntero dereferenciada: x^
    varPtr := Op^.rVar;  //Guarda referencia a la variable puntero
    //Mueve a W
    kMOVF(varPtr.adrByte0, toW);
    kMOVWF(FSR);  //direcciona
    kMOVF(INDF, toW);  //deje en W
    if modReturn then _RETURN;
  end;
  stVarRefExp: begin
    //Es una expresión derefernciada (x+a)^.
    {Se asume que el operando tiene su resultado en los RT. Si estuvieran en la pila
    no se aplicaría.}
    //Mueve a W
    _MOVWF(FSR.offs);  //direcciona
    _MOVF(0, toW);  //deje en W
    if modReturn then _RETURN;
  end;
  else
    //Almacenamiento no implementado
    GenError('Not implemented.');
  end;
end;
procedure TGenCodBas_PICBase.byte_DefineRegisters;
begin
  //No es encesario, definir registros adicionales a W
end;
procedure TGenCodBas_PICBase.byte_SaveToStk;
var
  stk: TPicRegister;
begin
  stk := GetStkRegisterByte;  //pide memoria
  //guarda W
  _BANKSEL(stk.bank);
  _MOVWF(stk.offs);PutComm(';save W');
  stk.used := true;
end;
procedure TGenCodBas_PICBase.byte_GetItem(const OpPtr: pointer);
//Función que devuelve el valor indexado
var
  Op: ^TOperand;
  arrVar, tmpVar: TxpEleVar;
  idx: TOperand;
  WithBrack: Boolean;
begin
  if not GetIdxParArray(WithBrack, idx) then exit;
  //Procesa
  Op := OpPtr;
  if Op^.Sto = stVariab then begin
    //Se aplica a una variable array. Lo Normal.
    arrVar := Op^.rVar;  //referencia a la variable.
    //Genera el código de acuerdo al índice
    case idx.Sto of
    stConst: begin  //ïndice constante
        tmpVar := CreateTmpVar('', typByte);
        tmpVar.addr0 := arrVar.addr0 + idx.valInt;  //¿Y si es de otro banco?
        SetResultVariab(tmpVar);
      end;
    stVariab: begin
        SetResultExpres(arrVar.typ.refType, true);  //Es array de bytes, o Char, devuelve Byte o Char
        LoadToRT(idx);   //Lo deja en W
        _MOVLW(arrVar.addr0);   //agrega OFFSET
        _ADDWF(04, toF);
        _MOVF(0, toW);  //lee indexado en W
    end;
    stExpres: begin
        SetResultExpres(arrVar.typ.refType, false);  //Es array de bytes, o Char, devuelve Byte o Char
        LoadToRT(idx);   //Lo deja en W
        _MOVLW(arrVar.addr0);   //agrega OFFSET
        _ADDWF(04, toF);
        _MOVF(0, toW);  //lee indexado en W
      end;
    end;
  end else begin
    GenError('Syntax error.');
  end;
  if WithBrack then begin
    if not CaptureTok(']') then exit;
  end else begin
    if not CaptureTok(')') then exit;
  end;
end;
procedure TGenCodBas_PICBase.byte_SetItem(const OpPtr: pointer);
//Función que fija un valor indexado
var
  WithBrack: Boolean;
var
  Op: ^TOperand;
  arrVar, rVar: TxpEleVar;
  idx, value: TOperand;
  idxTar: word;
begin
  if not GetIdxParArray(WithBrack, idx) then exit;
  //Procesa
  Op := OpPtr;
  if Op^.Sto = stVariab then begin  //Se aplica a una variable
    arrVar := Op^.rVar;  //referencia a la variable.
    res.SetAsNull;  //No devuelve nada
    //Genera el código de acuerdo al índice
    case idx.Sto of
    stConst: begin  //ïndice constante
        //Como el índice es constante, se puede acceder directamente
        idxTar := arrVar.adrByte0.offs+idx.valInt; //índice destino
        if not GetValueToAssign(WithBrack, arrVar, value) then exit;
        if (value.Sto = stConst) and (value.valInt=0) then begin
          //Caso especial, se pone a cero
          _CLRF(idxTar);
        end else begin
          //Sabemos que hay una expresión byte
          LoadToRT(value); //Carga resultado en W
          _MOVWF(idxTar);  //Mueve a arreglo
        end;
      end;
    stVariab: begin
        //El índice es una variable
        //Tenemos la referencia la variable en idx.rvar
        if not GetValueToAssign(WithBrack, arrVar, value) then exit;
        //Sabemos que hay una expresión byte
        if (value.Sto = stConst) and (value.valInt=0) then begin
          //Caso especial, se pide asignar una constante cero
          _MOVF(idx.offs, toW);  //índice
          _MOVLW(arrVar.addr0);
          _ADDWF(04, toF);  //Direcciona
          _CLRF($00);   //Pone a cero
        end else if value.Sto = stConst then begin
          //Es una constante cualquiera
          _MOVF(idx.offs, toW);  //índice
          _MOVLW(arrVar.addr0);
          _ADDWF(04, toF);  //Direcciona
          _MOVLW(value.valInt);
          _MOVWF($00);   //Escribe valor
        end else if value.Sto = stVariab then begin
          //Es una variable
          _MOVLW(arrVar.addr0);
          _ADDWF(04, toF);  //Direcciona
          _MOVWF($04);  //Direcciona
          _MOVF(value.offs, toW);
          _MOVWF($00);   //Escribe valor
        end else begin
          //Es una expresión. El resultado está en W
          //hay que mover value a arrVar[idx.rvar]
          typWord.DefineRegister;   //Para usar H
          _MOVWF(H.offs);  //W->H   salva H
          _MOVF(idx.offs, toW);  //índice
          _MOVLW(arrVar.addr0);
          _ADDWF(04, toF);  //Direcciona
          _MOVF(H.offs, toW);
          _MOVWF($00);   //Escribe valor
        end;
      end;
    stExpres: begin
      //El índice es una expresión y está en W.
      if not GetValueToAssign(WithBrack, arrVar, value) then exit;
      //Sabemos que hay una expresión byte
      if (value.Sto = stConst) and (value.valInt=0) then begin
        //Caso especial, se pide asignar una constante cero
        _MOVLW(arrVar.addr0);
        _ADDWF(04, toF);  //Direcciona
        _CLRF($00);   //Pone a cero
      end else if value.Sto = stConst then begin
        //Es una constante cualquiera
        _MOVLW(arrVar.addr0);
        _ADDWF(04, toF);  //Direcciona
        _MOVLW(value.valInt);
        _MOVWF($00);   //Escribe valor
      end else if value.Sto = stVariab then begin
        //Es una variable
        _MOVLW(arrVar.addr0);
        _ADDWF(FSR.offs, toF);  //Direcciona
        _MOVF(value.offs, toW);
        _MOVWF($00);   //Escribe valor
      end else begin
        //Es una expresión. El valor a asignar está en W, y el índice en la pila
        typWord.DefineRegister;   //Para usar H
        _MOVWF(H.offs);  //W->H   salva valor a H
        rVar := GetVarByteFromStk;  //toma referencia de la pila
        _MOVF(rVar.adrByte0.offs, toW);  //índice
        _MOVLW(arrVar.addr0);
        _ADDWF(04, toF);  //Direcciona
        _MOVF(H.offs, toW);
        _MOVWF($00);   //Escribe valor
        FreeStkRegisterByte;   //Para liberar
      end;
      end;
    end;
  end else begin
    GenError('Syntax error.');
  end;
  if WithBrack then begin
    //En este modo, no se requiere ")"
  end else begin
    if not CaptureTok(')') then exit;
  end;
end;
procedure TGenCodBas_PICBase.byte_ClearItems(const OpPtr: pointer);
{Limpia el contenido de todo el arreglo}
var
  Op: ^TOperand;
  xvar: TxpEleVar;
  j1: Word;
begin
  cIn.Next;  //Toma identificador de campo
  //Limpia el arreglo
  Op := OpPtr;
  case Op^.Sto of
  stVariab: begin
    xvar := Op^.rVar;  //Se supone que debe ser de tipo ARRAY
    res.SetAsConst(typByte);  //Realmente no es importante devolver un valor
    res.valInt {%H-}:= xvar.typ.arrSize;  //Devuelve tamaño
    if xvar.typ.arrSize = 0 then exit;  //No hay nada que limpiar
    if xvar.typ.arrSize = 1 then begin  //Es de un solo byte
      _BANKSEL(xvar.adrByte0.bank);
      _CLRF(xvar.adrByte0.offs);
    end else if xvar.typ.arrSize = 2 then begin  //Es de 2 bytes
      _BANKSEL(xvar.adrByte0.bank);
      _CLRF(xvar.adrByte0.offs);
      _CLRF(xvar.adrByte0.offs+1);
    end else if xvar.typ.arrSize = 3 then begin  //Es de 3 bytes
      _BANKSEL(xvar.adrByte0.bank);
      _CLRF(xvar.adrByte0.offs);
      _CLRF(xvar.adrByte0.offs+1);
      _CLRF(xvar.adrByte0.offs+2);
    end else if xvar.typ.arrSize = 4 then begin  //Es de 4 bytes
      _BANKSEL(xvar.adrByte0.bank);
      _CLRF(xvar.adrByte0.offs);
      _CLRF(xvar.adrByte0.offs+1);
      _CLRF(xvar.adrByte0.offs+2);
      _CLRF(xvar.adrByte0.offs+3);
    end else if xvar.typ.arrSize = 5 then begin  //Es de 5 bytes
      _BANKSEL(xvar.adrByte0.bank);
      _CLRF(xvar.adrByte0.offs);
      _CLRF(xvar.adrByte0.offs+1);
      _CLRF(xvar.adrByte0.offs+2);
      _CLRF(xvar.adrByte0.offs+3);
      _CLRF(xvar.adrByte0.offs+4);
    end else if xvar.typ.arrSize = 6 then begin  //Es de 6 bytes
      _BANKSEL(xvar.adrByte0.bank);
      _CLRF(xvar.adrByte0.offs);
      _CLRF(xvar.adrByte0.offs+1);
      _CLRF(xvar.adrByte0.offs+2);
      _CLRF(xvar.adrByte0.offs+3);
      _CLRF(xvar.adrByte0.offs+4);
      _CLRF(xvar.adrByte0.offs+5);
    end else if xvar.typ.arrSize = 7 then begin  //Es de 7 bytes
      _BANKSEL(xvar.adrByte0.bank);
      _CLRF(xvar.adrByte0.offs);
      _CLRF(xvar.adrByte0.offs+1);
      _CLRF(xvar.adrByte0.offs+2);
      _CLRF(xvar.adrByte0.offs+3);
      _CLRF(xvar.adrByte0.offs+4);
      _CLRF(xvar.adrByte0.offs+5);
      _CLRF(xvar.adrByte0.offs+6);
    end else begin
      //Implementa lazo, usando W como índice
      _MOVLW(xvar.adrByte0.offs);  //dirección inicial
      _MOVWF($04);   //FSR
      //_MOVLW(256-xvar.typ.arrSize);
j1:= _PC;
      _CLRF($00);    //Limpia [FSR]
      _INCF($04, toF);    //Siguiente
      _MOVLW(xvar.adrByte0.offs+256-xvar.typ.arrSize);  //End address
      _SUBWF($04, toW);
      _IFNZERO;
      _GOTO(j1);
    end;
  end;
  else
    GenError('Syntax error.');
  end;
end;
procedure TGenCodBas_PICBase.byte_bit(const OpPtr: pointer; nbit: byte);
//Implementa la operación del campo <tipo>.bit#
var
  xvar, tmpVar: TxpEleVar;
  msk: byte;
  Op: ^TOperand;
begin
  cIn.Next;       //Toma el identificador de campo
  Op := OpPtr;
  case Op^.Sto of
  stVariab: begin
    xvar := Op^.rVar;
    //Crea una variable temporal que representará al campo
    tmpVar := CreateTmpVar(xvar.name+'.bit' + IntToStr(nbit), typBit);   //crea variable temporal
    tmpVar.addr0 := xvar.addr0;
    tmpVar.bit0  := nbit;
    //Se devuelve una variable, byte
    res.SetAsVariab(tmpVar);   //actualiza la referencia (y actualiza el tipo).
  end;
  stConst: begin
    //Se devuelve una constante bit
    res.SetAsConst(typBit);
    msk := Op^.valInt and ($01 << nbit);
    res.valBool := msk <> 0;
  end;
  else
    GenError('Syntax error.');
  end;
end;
procedure TGenCodBas_PICBase.byte_bit0(const OpPtr: pointer);
begin
  byte_bit(OpPtr, 0);
end;
procedure TGenCodBas_PICBase.byte_bit1(const OpPtr: pointer);
begin
  byte_bit(OpPtr, 1);
end;
procedure TGenCodBas_PICBase.byte_bit2(const OpPtr: pointer);
begin
  byte_bit(OpPtr, 2);
end;
procedure TGenCodBas_PICBase.byte_bit3(const OpPtr: pointer);
begin
  byte_bit(OpPtr, 3);
end;
procedure TGenCodBas_PICBase.byte_bit4(const OpPtr: pointer);
begin
  byte_bit(OpPtr, 4);
end;
procedure TGenCodBas_PICBase.byte_bit5(const OpPtr: pointer);
begin
  byte_bit(OpPtr, 5);
end;
procedure TGenCodBas_PICBase.byte_bit6(const OpPtr: pointer);
begin
  byte_bit(OpPtr, 6);
end;
procedure TGenCodBas_PICBase.byte_bit7(const OpPtr: pointer);
begin
  byte_bit(OpPtr, 7);
end;
//////////////// Tipo DWord /////////////
procedure TGenCodBas_PICBase.word_LoadToRT(const OpPtr: pointer; modReturn: boolean);
{Carga el valor de una expresión a los registros de trabajo.}
var
  Op: ^TOperand;
  varPtr: TxpEleVar;
begin
  Op := OpPtr;
  case Op^.Sto of  //el parámetro debe estar en "Op^"
  stConst : begin
    //byte alto
    if Op^.HByte = 0 then begin
      _BANKSEL(H.bank);
      _CLRF(H.offs);
    end else begin
      _MOVLW(Op^.HByte);
      _BANKSEL(H.bank);
      _MOVWF(H.offs);
    end;
    //byte bajo
    if modReturn then _RETLW(Op^.LByte)
    else _MOVLW(Op^.LByte);
  end;
  stVariab: begin
    kMOVF(Op^.rVar.adrByte1, toW);
    kMOVWF(H);
    kMOVF(Op^.rVar.adrByte0, toW);
    if modReturn then _RETURN;
  end;
  stExpres: begin  //se asume que ya está en (H,w)
    if modReturn then _RETURN;
  end;
  stVarRefVar: begin
    //Se tiene una variable puntero dereferenciada: x^
    varPtr := Op^.rVar;  //Guarda referencia a la variable puntero
    //Mueve a W
    kINCF(varPtr.adrByte0, toW);  //varPtr.offs+1 -> W  (byte alto)
    _MOVWF(FSR.offs);  //direcciona byte alto
    _MOVF(0, toW);  //deje en W
    _BANKSEL(H.bank);
    _MOVWF(H.offs);  //Guarda byte alto
    _DECF(FSR.offs,toF);
    _MOVF(0, toW);  //deje en W byte bajo
    if modReturn then _RETURN;
  end;
  stVarRefExp: begin
    //Es una expresión desrefernciada (x+a)^.
    {Se asume que el operando tiene su resultado en los RT. Si estuvieran en la pila
    no se aplicaría.}
    //Mueve a W
    _MOVWF(FSR.offs);  //direcciona byte bajo
    _INCF(FSR.offs,toF);  //apunta a byte alto
    _MOVF(0, toW);  //deje en W
    _BANKSEL(H.bank);
    _MOVWF(H.offs);  //Guarda byte alto
    _DECF(FSR.offs,toF);
    _MOVF(0, toW);  //deje en W byte bajo
    if modReturn then _RETURN;
  end;
  else
    //Almacenamiento no implementado
    GenError('Not implemented.');
  end;
end;
procedure TGenCodBas_PICBase.word_DefineRegisters;
begin
  //Aparte de W, solo se requiere H
  if not H.assigned then begin
    AssignRAM(H.addr, '_H', false);
    H.assigned := true;
    H.used := false;
  end;
end;
procedure TGenCodBas_PICBase.word_SaveToStk;
var
  stk: TPicRegister;
begin
  //guarda W
  stk := GetStkRegisterByte;  //pide memoria
  if stk = nil then exit;
  _BANKSEL(stk.bank);
  _MOVWF(stk.offs);PutComm(';save W');
  stk.used := true;
  //guarda H
  stk := GetStkRegisterByte;   //pide memoria
  if stk = nil then exit;
  _BANKSEL(H.bank);
  _MOVF(H.offs, toW);PutComm(';save H');
  _BANKSEL(stk.bank);
  _MOVWF(stk.offs);
  stk.used := true;   //marca
end;
procedure TGenCodBas_PICBase.word_GetItem(const OpPtr: pointer);
//Función que devuelve el valor indexado
var
  Op: ^TOperand;
  arrVar, tmpVar: TxpEleVar;
  idx: TOperand;
  WithBrack: Boolean;
begin
  if not GetIdxParArray(WithBrack, idx) then exit;
  //Procesa
  Op := OpPtr;
  if Op^.Sto = stVariab then begin  //Se aplica a una variable
    arrVar := Op^.rVar;  //referencia a la variable.
    typWord.DefineRegister;
    //Genera el código de acuerdo al índice
    case idx.Sto of
    stConst: begin  //ïndice constante
      tmpVar := CreateTmpVar('', typWord);
      tmpVar.addr0 := arrVar.addr0+idx.valInt*2;  //¿Y si es de otro banco?
      tmpVar.addr1 := arrVar.addr0+idx.valInt*2+1;  //¿Y si es de otro banco?
      SetResultVariab(tmpVar);
//        SetResultExpres(arrVar.typ.refType, true);  //Es array de word, devuelve word
//        //Como el índice es constante, se puede acceder directamente
//        add0 := arrVar.adrByte0.offs+idx.valInt*2;
//        _MOVF(add0+1, toW);
//        _MOVWF(H.offs);    //byte alto
//        _MOVF(add0, toW);  //byte bajo
      end;
    stVariab: begin
      SetResultExpres(arrVar.typ.refType, true);  //Es array de word, devuelve word
      _BCF(STATUS, _C);
      _RLF(idx.offs, toW);      //Multiplica Idx por 2
      _MOVWF(FSR.offs);     //direcciona con FSR
      _MOVLW(arrVar.addr0+1);   //Agrega OFFSET + 1
      _ADDWF(FSR.offs, toF);

      _MOVF(0, toW);  //lee indexado en W
      _MOVWF(H.offs);    //byte alto
      _DECF(FSR.offs, toF);
      _MOVF(0, toW);  //lee indexado en W
    end;
    stExpres: begin
      SetResultExpres(arrVar.typ.refType, false);  //Es array de word, devuelve word
      _MOVWF(FSR.offs);     //idx a  FSR (usa como varaib. auxiliar)
      _BCF(STATUS, _C);
      _RLF(FSR.offs, toW);      //Multiplica Idx por 2
      _MOVWF(FSR.offs);     //direcciona con FSR
      _MOVLW(arrVar.addr0+1);   //Agrega OFFSET + 1
      _ADDWF(FSR.offs, toF);

      _MOVF(0, toW);  //lee indexado en W
      _MOVWF(H.offs);    //byte alto
      _DECF(FSR.offs, toF);
      _MOVF(0, toW);  //lee indexado en W
    end;
    end;
  end else begin
    GenError('Syntax error.');
  end;
  if WithBrack then begin
    if not CaptureTok(']') then exit;
  end else begin
    if not CaptureTok(')') then exit;
  end;
end;
procedure TGenCodBas_PICBase.word_SetItem(const OpPtr: pointer);
//Función que fija un valor indexado
var
  WithBrack: Boolean;
var
  Op: ^TOperand;
  arrVar, rVar: TxpEleVar;
  idx, value: TOperand;
  idxTar: Int64;
  aux: TPicRegister;
begin
  if not GetIdxParArray(WithBrack, idx) then exit;
  //Procesa
  Op := OpPtr;
  if Op^.Sto = stVariab then begin  //Se aplica a una variable
    arrVar := Op^.rVar;  //referencia a la variable.
    res.SetAsNull;  //No devuelve nada
    //Genera el código de acuerdo al índice
    case idx.Sto of
    stConst: begin  //Indice constante
        //Como el índice es constante, se puede acceder directamente
        idxTar := arrVar.adrByte0.offs+idx.valInt*2; //índice destino
        if not GetValueToAssign(WithBrack, arrVar, value) then exit;
        if value.Sto = stConst then begin
          //Es una constante
          //Byte bajo
          if value.LByte=0 then begin //Caso especial
            _CLRF(idxTar);
          end else begin
            _MOVLW(value.LByte);
            _MOVWF(idxTar);
          end;
          //Byte alto
          if value.HByte=0 then begin //Caso especial
            _CLRF(idxTar+1);
          end else begin
            _MOVLW(value.HByte);
            _MOVWF(idxTar+1);
          end;
        end else begin
          //El valor a asignar es variable o expresión
          typWord.DefineRegister;   //Para usar H
          //Sabemos que hay una expresión word
          LoadToRT(value); //Carga resultado en H,W
          _MOVWF(idxTar);  //Byte bajo
          _MOVF(H.offs, toW);
          _MOVWF(idxTar+1);  //Byte alto
        end;
      end;
    stVariab: begin
        //El índice es una variable
        //Tenemos la referencia la variable en idx.rvar
        if not GetValueToAssign(WithBrack, arrVar, value) then exit;
        //Sabemos que hay una expresión word
        if value.Sto = stConst then begin
          //El valor a escribir, es una constante cualquiera
          _BCF(STATUS, _C);
          _RLF(idx.offs, toW);  //índice * 2
          _MOVWF(FSR.offs);  //Direcciona
          _MOVLW(arrVar.addr0);  //Dirección de inicio
          _ADDWF(FSR.offs, toF);  //Direcciona
          ////// Byte Bajo
          if value.LByte = 0 then begin
            _CLRF($00);
          end else begin
            _MOVLW(value.LByte);
            _MOVWF($00);   //Escribe
          end;
          ////// Byte Alto
          _INCF(FSR.offs, toF);  //Direcciona a byte ALTO
          if value.HByte = 0 then begin
            _CLRF($00);
          end else begin
            _MOVLW(value.HByte);
            _MOVWF($00);   //Escribe
          end;
        end else if value.Sto = stVariab then begin
          //El valor a escribir, es una variable
          //Calcula dirfección de byte bajo
          _BCF(STATUS, _C);
          _RLF(idx.offs, toW);  //índice * 2
          _MOVWF(FSR.offs);  //Direcciona
          _MOVLW(arrVar.addr0);  //Dirección de inicio
          _ADDWF(FSR.offs, toF);  //Direcciona
          ////// Byte Bajo
          _MOVF(value.Loffs, toW);
          _MOVWF($00);   //Escribe
          ////// Byte Alto
          _INCF(FSR.offs, toF);  //Direcciona a byte ALTO
          _MOVF(value.Hoffs, toW);
          _MOVWF($00);   //Escribe
        end else begin
          //El valor a escribir, es una expresión y está en H,W
          //hay que mover value a arrVar[idx.rvar]
          aux := GetAuxRegisterByte;
          typWord.DefineRegister;   //Para usar H
          _MOVWF(aux.offs);  //W->   salva W (Valor.H)
          //Calcula dirección de byte bajo
          _BCF(STATUS, _C);
          _RLF(idx.offs, toW);  //índice * 2
          _MOVWF(FSR.offs);  //Direcciona
          _MOVLW(arrVar.addr0);  //Dirección de inicio
          _ADDWF(FSR.offs, toF);  //Direcciona
          ////// Byte Bajo
          _MOVF(aux.offs, toW);
          _MOVWF($00);   //Escribe
          ////// Byte Alto
          _INCF(FSR.offs, toF);  //Direcciona a byte ALTO
          _MOVF(H.offs, toW);
          _MOVWF($00);   //Escribe
          aux.used := false;
        end;
      end;
    stExpres: begin
      //El índice es una expresión y está en W.
      if not GetValueToAssign(WithBrack, arrVar, value) then exit;
      if value.Sto = stConst then begin
        //El valor a asignar, es una constante
        _MOVWF(FSR.offs);   //Salva W.
        _BCF(STATUS, _C);
        _RLF(FSR.offs, toF);  //idx * 2
        _MOVLW(arrVar.addr0);
        _ADDWF(FSR.offs, toF);  //Direcciona a byte bajo
        //Byte bajo
        if value.LByte = 0 then begin
          _CLRF($00);   //Pone a cero
        end else begin
          _MOVLW(value.LByte);
          _MOVWF($00);
        end;
        //Byte alto
        _INCF(FSR.offs, toF);
        if value.HByte = 0 then begin
          _CLRF($00);   //Pone a cero
        end else begin
          _MOVLW(value.HByte);
          _MOVWF($00);
        end;
      end else if value.Sto = stVariab then begin
        _MOVWF(FSR.offs);   //Salva W.
        _BCF(STATUS, _C);
        _RLF(FSR.offs, toF);  //idx * 2
        _MOVLW(arrVar.addr0);  //Dirección de inicio
        _ADDWF(FSR.offs, toF);  //Direcciona a byte bajo
        //Byte bajo
        _MOVF(value.Loffs, toW);
        _MOVWF($00);
        //Byte alto
        _INCF(FSR.offs, toF);
        _MOVF(value.Hoffs, toW);
        _MOVWF($00);
      end else begin
        //El valor a asignar está en H,W, y el índice (byte) en la pila
        typWord.DefineRegister;   //Para usar H
        aux := GetAuxRegisterByte;
        _MOVWF(aux.offs);  //W->aux   salva W
        rVar := GetVarByteFromStk;  //toma referencia de la pila
        //Calcula dirección de byte bajo
        _BCF(STATUS, _C);
        _RLF(rVar.adrByte0.offs, toF);  //índice * 2
        _MOVLW(arrVar.addr0);  //Dirección de inicio
        _ADDWF(FSR.offs, toF);  //Direcciona
        ////// Byte Bajo
        _MOVF(aux.offs, toW);
        _MOVWF($00);   //Escribe
        ////// Byte Alto
        _INCF(FSR.offs, toF);  //Direcciona a byte ALTO
        _MOVF(H.offs, toW);
        _MOVWF($00);   //Escribe
        aux.used := false;
      end;
    end;
    end;
  end else begin
    GenError('Syntax error.');
  end;
  if WithBrack then begin
    //En este modo, no se requiere ")"
  end else begin
    if not CaptureTok(')') then exit;
  end;
end;
procedure TGenCodBas_PICBase.word_ClearItems(const OpPtr: pointer);
begin

end;
procedure TGenCodBas_PICBase.word_Low(const OpPtr: pointer);
{Acceso al byte de menor peso de un word.}
var
  xvar, tmpVar: TxpEleVar;
  Op: ^TOperand;
begin
  cIn.Next;  //Toma identificador de campo
  Op := OpPtr;
  case Op^.Sto of
  stVariab: begin
    xvar := Op^.rVar;
    //Se devuelve una variable, byte
    //Crea una variable temporal que representará al campo
    tmpVar := CreateTmpVar(xvar.name+'.L', typByte);   //crea variable temporal
    tmpVar.addr0 :=  xvar.addr0;  //byte bajo
    res.SetAsVariab(tmpVar);
  end;
  stConst: begin
    //Se devuelve una constante bit
    res.SetAsConst(typByte);
    res.valInt := Op^.ValInt and $ff;
  end;
  else
    GenError('Syntax error.');
  end;
end;
procedure TGenCodBas_PICBase.word_High(const OpPtr: pointer);
{Acceso al byte de mayor peso de un word.}
var
  xvar, tmpVar: TxpEleVar;
  Op: ^TOperand;
begin
  cIn.Next;  //Toma identificador de campo
  Op := OpPtr;
  case Op^.Sto of
  stVariab: begin
    xvar := Op^.rVar;
    //Se devuelve una variable, byte
    //Crea una variable temporal que representará al campo
    tmpVar := CreateTmpVar(xvar.name+'.H', typByte);
    tmpVar.addr0 := xvar.addr1;  //byte alto
    res.SetAsVariab(tmpVar);
  end;
  stConst: begin
    //Se devuelve una constante bit
    res.SetAsConst(typByte);
    res.valInt := (Op^.ValInt and $ff00)>>8;
  end;
  else
    GenError('Syntax error.');
  end;
end;
//////////////// Tipo DWord /////////////
procedure TGenCodBas_PICBase.dword_LoadToRT(const OpPtr: pointer; modReturn: boolean);
{Carga el valor de una expresión a los registros de trabajo.}
var
  Op: ^TOperand;
begin
  Op := OpPtr;
  case Op^.Sto of  //el parámetro debe estar en "Op^"
  stConst : begin
    //byte U
    if Op^.UByte = 0 then begin
      _BANKSEL(U.bank);
      _CLRF(U.offs);
    end else begin
      _MOVLW(Op^.UByte);
      _BANKSEL(U.bank);
      _MOVWF(U.offs);
    end;
    //byte E
    if Op^.EByte = 0 then begin
      _BANKSEL(E.bank);
      _CLRF(E.offs);
    end else begin
      _MOVLW(Op^.EByte);
      _BANKSEL(E.bank);
      _MOVWF(E.offs);
    end;
    //byte H
    if Op^.HByte = 0 then begin
      _BANKSEL(H.bank);
      _CLRF(H.offs);
    end else begin
      _MOVLW(Op^.HByte);
      _BANKSEL(H.bank);
      _MOVWF(H.offs);
    end;
    //byte 0
    if modReturn then _RETLW(Op^.LByte)
    else _MOVLW(Op^.LByte);
  end;
  stVariab: begin
    kMOVF(Op^.rVar.adrByte3, toW);
    kMOVWF(U);

    kMOVF(Op^.rVar.adrByte2, toW);
    kMOVWF(E);

    kMOVF(Op^.rVar.adrByte1, toW);
    kMOVWF(H);

    kMOVF(Op^.rVar.adrByte0, toW);
    if modReturn then _RETURN;
  end;
  stExpres: begin  //se asume que ya está en (U,E,H,w)
    if modReturn then _RETURN;
  end;
  else
    //Almacenamiento no implementado
    GenError('Not implemented.');
  end;
end;
procedure TGenCodBas_PICBase.dword_DefineRegisters;
begin
  //Aparte de W, se requieren H, E y U
  if not H.assigned then begin
    AssignRAM(H.addr, '_H', false);
    H.assigned := true;
    H.used := false;
  end;
  if not E.assigned then begin
    AssignRAM(E.addr, '_E', false);
    E.assigned := true;
    E.used := false;
  end;
  if not U.assigned then begin
    AssignRAM(U.addr, '_U', false);
    U.assigned := true;
    U.used := false;
  end;
end;
procedure TGenCodBas_PICBase.dword_SaveToStk;
var
  stk: TPicRegister;
begin
  //guarda W
  stk := GetStkRegisterByte;  //pide memoria
  if HayError then exit;
  _BANKSEL(stk.bank);
  _MOVWF(stk.offs);PutComm(';save W');
  stk.used := true;
  //guarda H
  stk := GetStkRegisterByte;   //pide memoria
  if HayError then exit;
  _BANKSEL(H.bank);
  _MOVF(H.offs, toW);PutComm(';save H');
  _BANKSEL(stk.bank);
  _MOVWF(stk.offs);
  stk.used := true;   //marca
  //guarda E
  stk := GetStkRegisterByte;   //pide memoria
  if HayError then exit;
  _BANKSEL(E.bank);
  _MOVF(E.offs, toW);PutComm(';save E');
  _BANKSEL(stk.bank);
  _MOVWF(stk.offs);
  stk.used := true;   //marca
  //guarda U
  stk := GetStkRegisterByte;   //pide memoria
  if HayError then exit;
  _BANKSEL(U.bank);
  _MOVF(U.offs, toW);PutComm(';save U');
  _BANKSEL(stk.bank);
  _MOVWF(stk.offs);
  stk.used := true;   //marca
end;
procedure TGenCodBas_PICBase.dword_Low(const OpPtr: pointer);
{Acceso al byte de menor peso de un Dword.}
var
  xvar, tmpVar: TxpEleVar;
  Op: ^TOperand;
begin
  cIn.Next;  //Toma identificador de campo
  Op := OpPtr;
  case Op^.Sto of
  stVariab: begin
    xvar := Op^.rVar;
    //Se devuelve una variable, byte
    //Crea una variable temporal que representará al campo
    tmpVar := CreateTmpVar(xvar.name+'.Low', typByte);   //crea variable temporal
    tmpVar.addr0 := xvar.addr0;  //byte bajo
    res.SetAsVariab(tmpVar);
  end;
  stConst: begin
    //Se devuelve una constante byte
    res.SetAsConst(typByte);
    res.valInt := Op^.ValInt and $ff;
  end;
  else
    GenError('Syntax error.');
  end;
end;
procedure TGenCodBas_PICBase.dword_High(const OpPtr: pointer);
{Acceso al byte de mayor peso de un Dword.}
var
  xvar, tmpVar: TxpEleVar;
  Op: ^TOperand;
begin
  cIn.Next;  //Toma identificador de campo
  Op := OpPtr;
  case Op^.Sto of
  stVariab: begin
    xvar := Op^.rVar;
    //Se devuelve una variable, byte
    //Crea una variable temporal que representará al campo
    tmpVar := CreateTmpVar(xvar.name+'.High', typByte);
    tmpVar.addr0 := xvar.addr1;  //byte alto
    res.SetAsVariab(tmpVar);
  end;
  stConst: begin
    //Se devuelve una constante bit
    res.SetAsConst(typByte);
    res.valInt := (Op^.ValInt and $ff00)>>8;
  end;
  else
    GenError('Syntax error.');
  end;
end;
procedure TGenCodBas_PICBase.dword_Extra(const OpPtr: pointer);
{Acceso al byte 2 de un Dword.}
var
  xvar, tmpVar: TxpEleVar;
  Op: ^TOperand;
begin
  cIn.Next;  //Toma identificador de campo
  Op := OpPtr;
  case Op^.Sto of
  stVariab: begin
    xvar := Op^.rVar;
    //Se devuelve una variable, byte
    //Crea una variable temporal que representará al campo
    tmpVar := CreateTmpVar(xvar.name+'.Extra', typByte);
    tmpVar.addr0 := xvar.addr2;  //byte alto
    res.SetAsVariab(tmpVar);
  end;
  stConst: begin
    //Se devuelve una constante bit
    res.SetAsConst(typByte);
    res.valInt := (Op^.ValInt and $ff0000)>>16;
  end;
  else
    GenError('Syntax error.');
  end;
end;
procedure TGenCodBas_PICBase.dword_Ultra(const OpPtr: pointer);
{Acceso al byte 3 de un Dword.}
var
  xvar, tmpVar: TxpEleVar;
  Op: ^TOperand;
begin
  cIn.Next;  //Toma identificador de campo
  Op := OpPtr;
  case Op^.Sto of
  stVariab: begin
    xvar := Op^.rVar;
    //Se devuelve una variable, byte
    //Crea una variable temporal que representará al campo
    tmpVar := CreateTmpVar(xvar.name+'.Ultra', typByte);
    tmpVar.addr0 := xvar.addr3;  //byte alto
    res.SetAsVariab(tmpVar);
  end;
  stConst: begin
    //Se devuelve una constante bit
    res.SetAsConst(typByte);
    res.valInt := (Op^.ValInt and $ff000000)>>24;
  end;
  else
    GenError('Syntax error.');
  end;
end;
procedure TGenCodBas_PICBase.dword_LowWord(const OpPtr: pointer);
{Acceso al word de menor peso de un Dword.}
var
  xvar, tmpVar: TxpEleVar;
  Op: ^TOperand;
begin
  cIn.Next;  //Toma identificador de campo
  Op := OpPtr;
  case Op^.Sto of
  stVariab: begin
    xvar := Op^.rVar;
    //Se devuelve una variable, byte
    //Crea una variable temporal que representará al campo
    tmpVar := CreateTmpVar(xvar.name+'.LowW', typWord);   //crea variable temporal
    tmpVar.addr0 := xvar.addr0;  //byte bajo
    tmpVar.addr1 := xvar.addr1;  //byte alto
    res.SetAsVariab(tmpVar);   //actualiza la referencia
  end;
  stConst: begin
    //Se devuelve una constante bit
    res.SetAsConst(typWord);
    res.valInt := Op^.ValInt and $ffff;
  end;
  else
    GenError('Syntax error.');
  end;
end;
procedure TGenCodBas_PICBase.dword_HighWord(const OpPtr: pointer);
{Acceso al word de mayor peso de un Dword.}
var
  xvar, tmpVar: TxpEleVar;
  Op: ^TOperand;
begin
  cIn.Next;  //Toma identificador de campo
  Op := OpPtr;
  case Op^.Sto of
  stVariab: begin
    xvar := Op^.rVar;
    //Se devuelve una variable, byte
    //Crea una variable temporal que representará al campo
    tmpVar := CreateTmpVar(xvar.name+'.HighW', typWord);   //crea variable temporal
    tmpVar.addr0 := xvar.addr2;  //byte bajo
    tmpVar.addr1 := xvar.addr3;  //byte alto
    res.SetAsVariab(tmpVar);   //actualiza la referencia
  end;
  stConst: begin
    //Se devuelve una constante bit
    res.SetAsConst(typWord);
    res.valInt := (Op^.ValInt and $ffff0000) >> 16;
  end;
  else
    GenError('Syntax error.');
  end;
end;
procedure TGenCodBas_PICBase.GenCodPicReqStopCodeGen;
{Required Stop the Code generation}
begin
  posFlash := pic.iFlash; //Probably not the best way.
end;
procedure TGenCodBas_PICBase.GenCodPicReqStartCodeGen;
{Required Start the Code generation}
begin
  pic.iFlash := posFlash; //Probably not the best way.
end;
//Inicialización
procedure TGenCodBas_PICBase.StartRegs;
{Inicia los registros de trabajo en la lista.}
begin
  listRegAux.Clear;
  listRegStk.Clear;   //limpia la pila
  stackTop := 0;
  listRegAuxBit.Clear;
  listRegStkBit.Clear;   //limpia la pila
  stackTopBit := 0;
  {Crea registros de trabajo adicionales H,E,U, para que estén definidos, pero aún no
  tienen asignados una posición en memoria.}
  H := CreateRegisterByte(prtWorkReg);
  E := CreateRegisterByte(prtWorkReg);
  U := CreateRegisterByte(prtWorkReg);
  //Puede salir con error
end;
constructor TGenCodBas_PICBase.Create;
begin
  inherited Create;
  OnReqStartCodeGen:=@GenCodPicReqStartCodeGen;
  OnReqStopCodeGen:=@GenCodPicReqStopCodeGen;
  pic := TPICBase.Create;
  ///////////Crea tipos
  ClearTypes;
  typNull := CreateSysType('null',t_boolean,-1);
  ///////////////// Tipo Bit ////////////////
  typBit := CreateSysType('bit', t_uinteger,-1);   //de 1 bit
  typBit.OnLoadToRT  :=  @bit_LoadToRT;
  typBit.OnDefRegister:= @bit_DefineRegisters;
  typBit.OnSaveToStk  := @bit_SaveToStk;
//  opr:=typBit.CreateUnaryPreOperator('@', 6, 'addr', @Oper_addr_bit);

  ///////////////// Tipo Booleano ////////////////
  typBool := CreateSysType('boolean',t_boolean,-1);   //de 1 bit
  typBool.OnLoadToRT   := @bit_LoadToRT;  //es lo mismo
  typBool.OnDefRegister:= @bit_DefineRegisters;  //es lo mismo
  typBool.OnSaveToStk  := @bit_SaveToStk;  //es lo mismo

  //////////////// Tipo Byte /////////////
  typByte := CreateSysType('byte',t_uinteger,1);   //de 1 byte
  typByte.OnLoadToRT   := @byte_LoadToRT;
  typByte.OnDefRegister:= @byte_DefineRegisters;
  typByte.OnSaveToStk  := @byte_SaveToStk;
  //typByte.OnReadFromStk :=
  typByte.OnGetItem    := @byte_GetItem;
//  typByte.OnSetItem    := @byte_SetItem;
  typByte.OnClearItems := @byte_ClearItems;
  //Campos de bit
  typByte.CreateField('bit0', @byte_bit0);
  typByte.CreateField('bit1', @byte_bit1);
  typByte.CreateField('bit2', @byte_bit2);
  typByte.CreateField('bit3', @byte_bit3);
  typByte.CreateField('bit4', @byte_bit4);
  typByte.CreateField('bit5', @byte_bit5);
  typByte.CreateField('bit6', @byte_bit6);
  typByte.CreateField('bit7', @byte_bit7);
  //Campos de bit (se mantienen por compatibilidad)
  typByte.CreateField('0', @byte_bit0);
  typByte.CreateField('1', @byte_bit1);
  typByte.CreateField('2', @byte_bit2);
  typByte.CreateField('3', @byte_bit3);
  typByte.CreateField('4', @byte_bit4);
  typByte.CreateField('5', @byte_bit5);
  typByte.CreateField('6', @byte_bit6);
  typByte.CreateField('7', @byte_bit7);

  //////////////// Tipo Char /////////////
  //Tipo caracter
  typChar := CreateSysType('char',t_uinteger,1);   //de 1 byte. Se crea como uinteger para leer/escribir su valor como número
  typChar.OnLoadToRT   := @byte_LoadToRT;  //Es lo mismo
  typChar.OnDefRegister:= @byte_DefineRegisters;  //Es lo mismo
  typChar.OnSaveToStk  := @byte_SaveToStk; //Es lo mismo
  typChar.OnGetItem    := @byte_GetItem;   //Es lo mismo
//  typChar.OnSetItem    := @byte_SetItem;
  typChar.OnClearItems := @byte_ClearItems;

  //////////////// Tipo Word /////////////
  //Tipo numérico de dos bytes
  typWord := CreateSysType('word',t_uinteger,2);   //de 2 bytes
  typWord.OnLoadToRT   := @word_LoadToRT;
  typWord.OnDefRegister:= @word_DefineRegisters;
  typWord.OnSaveToStk  := @word_SaveToStk;
  typWord.OnGetItem    := @word_GetItem;   //Es lo mismo
//  typWord.OnSetItem    := @word_SetItem;
//  typWord.OnClearItems := @word_ClearItems;

  typWord.CreateField('Low', @word_Low);
  typWord.CreateField('High', @word_High);

  //////////////// Tipo DWord /////////////
  //Tipo numérico de cuatro bytes
  typDWord := CreateSysType('dword',t_uinteger,4);  //de 4 bytes
  typDWord.OnLoadToRT   := @dword_LoadToRT;
  typDWord.OnDefRegister:= @dword_DefineRegisters;
  typDWord.OnSaveToStk  := @dword_SaveToStk;

  typDWord.CreateField('Low',   @dword_Low);
  typDWord.CreateField('High',  @dword_High);
  typDWord.CreateField('Extra', @dword_Extra);
  typDWord.CreateField('Ultra', @dword_Ultra);
  typDWord.CreateField('LowWord', @dword_LowWord);
  typDWord.CreateField('HighWord',@dword_HighWord);

  //Crea variables de trabajo
  varStkBit  := TxpEleVar.Create;
  varStkBit.typ := typBit;
  varStkByte := TxpEleVar.Create;
  varStkByte.typ := typByte;
  varStkWord := TxpEleVar.Create;
  varStkWord.typ := typWord;
  varStkDWord := TxpEleVar.Create;
  varStkDWord.typ := typDWord;
  //Crea lista de variables temporales
  varFields    := TxpEleVars.Create(true);
  //Inicializa contenedores
  listRegAux   := TPicRegister_list.Create(true);
  listRegStk   := TPicRegister_list.Create(true);
  listRegAuxBit:= TPicRegisterBit_list.Create(true);
  listRegStkBit:= TPicRegisterBit_list.Create(true);
  stackTop     := 0;  //Apunta a la siguiente posición libre
  stackTopBit  := 0;  //Apunta a la siguiente posición libre
  {Crea registro de trabajo W. El registro W, es el registro interno del PIC, y no
  necesita un mapeo en RAM. Solo se le crea aquí, para poder usar su propiedad "used"}
  W := TPicRegister.Create;
  W.assigned := false;   //se le marca así, para que no se intente usar
  {Crea registro de trabajo Z. El registro Z, es el registro interno del PIC, y está
  siempre asignado en RAM. }
  Z := TPicRegisterBit.Create;
  Z.addr := STATUS;
  Z.bit := _Z;
  Z.assigned := true;   //ya está asignado desde el principio
  {Crea registro de trabajo C. El registro C, es el registro interno del PIC, y está
  siempre asignado en RAM. }
  C := TPicRegisterBit.Create;
  C.addr := STATUS;
  C.bit := _C;
  C.assigned := true;   //ya está asignado desde el principio
  //Crea registro interno INDF
  INDF := TPicRegister.Create;
  INDF.addr := $00;
  INDF.assigned := true;   //ya está asignado desde el principio
  {Crea registro auxiliar FSR. El registro FSR, es un registro interno del PIC, y está
  siempre asignado en RAM. }
  FSR := TPicRegister.Create;
  FSR.addr := $04;
  FSR.assigned := true;   //ya está asignado desde el principio
end;
destructor TGenCodBas_PICBase.Destroy;
begin
  INDF.Destroy;
  FSR.Destroy;
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
  varStkDWord.Destroy;
  pic.Destroy;
  inherited Destroy;
end;

end.

