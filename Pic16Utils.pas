{PIC16Utils

Descripción
===========
Unidad con utilidades para la programación de microcontroladores PIC de rango
medio con instrucciones de 14 bits. Incluye a la mayoría de la serie
PIC16FXXXX.
Esta unidad trabaja con tamaños de página de 2K y tamaños de bancos de 128 bytes.
Se define un objeto que representa a un PIC de esta serie, que está dimensionado
para poder representar al dispositivo más complejo.
El objetivo de esta unidad es poder servir como base para la implementación de
ensambladores, compiladores o hasta simuladores.

                                         Creado por Tito Hinostroza   26/07/2015
}

unit Pic16Utils;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LCLProc;
const
  PIC_MAX_RAM = 512;    //Máxima cantidad de memoria RAM
  PIC_MAX_FLASH = 8192; //Máxima cantidad e memoria Flash
  PIC_PAGE_SIZE = 2048;
  PIC_MAX_PINES = 64;   //Máxima cantidad de pines para el encapsulado
type  //tipos para instrucciones
  //Instrucciones para la serie 16
  TPIC16Inst = (
    //BYTE-ORIENTED FILE REGISTER OPERATIONS
    ADDWF,
    ANDWF,
    CLRF,
    CLRW,
    COMF ,
    DECF ,
    DECFSZ,
    INCF,
    INCFSZ,
    IORWF,
    MOVF,
    MOVWF,
    NOP,
    RLF,
    RRF,
    SUBWF,
    SWAPF,
    XORWF,
    //BIT-ORIENTED FILE REGISTER OPERATIONS
    BCF,
    BSF,
    BTFSC,
    BTFSS,
    //LITERAL AND CONTROL OPERATIONS
    ADDLW,
    ANDLW,
    CALL,
    CLRWDT,
    GOTO_,
    IORLW,
    MOVLW,
    RETFIE,
    RETLW,
    RETURN,
    SLEEP,
    SUBLW,
    XORLW,
    _Inval
  );
  //Indica el destino de la instrucción
  TPIC16destin = (
    toW = %00000000,    //al acumulador
    toF = %10000000     //a memoria
  );


type //Modelo de la memoria RAM
  TPIC16CellState = (
     cs_impleSFR,   //Registros de funciones especiales. Habilitado para uso.
     cs_impleGPR,   //Registros de uso general. Habilitado para uso.
     cs_unimplem   //No implementado
  );

  { TPIC16RamCell }
  {Modela a una dirección lógica de la memoria RAM. Se ha taratdo de hacer una
  definición eficiente de esta estructura para facilitar la implementación de
  simuladores en tiempo real. Podemos usar un tamaño mediano para este registro,
  porque no esperamos tener muchas celdas de RAM (<1K).}
  TPIC16RamCellPtr = ^TPIC16RamCell;
  TPIC16RamCell = object
  private
    Fvalue  : byte;     //value of the memory
    Fused   : byte;     //Bitmap. Indicates the used bits ($00->all free; $ff->all bits used.)
    Fimplem : byte;     //Bitmap. Indicates the implemented bits
    function Getused: byte;
    function Getvalue: byte;
    procedure Setused(AValue: byte);
    procedure Setvalue(AValue: byte);
  public
    addr   : word;     //dirección física de memoria, en donde está la celda.
    name   : string;   //Name of the register (or variable)
    bitname: array[0..7] of string;  //Name of the bits.
    shared : byte;     //Used to share this register
    state  : TPIC16CellState;  //status of the cell
    mappedTo: TPIC16RamCellPtr;  //Indica que está mapeado a otra celda, de otra dirección
    property value: byte read Getvalue write Setvalue;
    property used: byte read Getused write Setused;
    function AvailGPR: boolean;
  end;
  TPIC16Ram = array[0..PIC_MAX_RAM-1] of TPIC16RamCell;
  PIC16RamPtr = ^TPIC16Ram;
  TRutExplorRAM = procedure(offs, bnk: byte; regPtr: TPIC16RamCellPtr) of object;
  {Representa a un banco de memoria del PIC. En un banco las direcciones de memoria
   se mapean siempre desde $00 hasta $7F. No almacenan datos, solo usan referencias.}
  ptrRAMBank = ^TRAMBank;
  { TRAMBank }
  TRAMBank = object
  public
    numBank   : integer;       //Número de banco
    ramPtr    : PIC16RamPtr;  //Puntero a memoria RAM
    AddrStart : word;          //dirección de inicio en la memoria RAM total
  public
    procedure Init(num: byte; AddrStart0: word; ram0: PIC16RamPtr);  //inicia objeto
  end;

type  //Models for Flash memory
  TPIC16FlashCell = record
    value     : word;     //Value of the memory (OpCode)
    used      : boolean;  //Indicate if have been written
    curBnk    : byte;     //Current RAM bank where it's supposed this Opcode works.
    //Information of position in source code. Used for debug
    rowSrc    : word;     //Row number
    colSrc    : word;     //Column number
    idFile    : SmallInt; //Index to a file. No load the name to save space.
    {Estos campos de cadena ocupan bastante espacio, aún cuado están en NULL. Si se
    quisiera optimizar el uso de RAM, se podría pensar en codificar, varios campos en
    una sola cadena.}
    topLabel   : string;  //Label on the top of the cell.
    topComment : string;  //Comment on the top of the cell.
    sideComment: string;  //Right comment to code
    //Campos para deputación
    breakPnt  : boolean;  //Indicates if this cell have a Breakpoint
    {Be careful on the size of this record, because it's going to be multiplied by 8192}
  end;
  TPIC16Flash = array[0..PIC_MAX_FLASH-1] of TPIC16FlashCell;
  ptrPIC16Flash = ^TPIC16Flash;

  {Representa a una página de memoria del PIC. En una página las direcciones de memoria
   se mapean siempre desde $000 hasta $800. No almacenan datos, solo usan referencias.}
  ptrFlashPage = ^TFlashPage;
  { TFlashPage }
  TFlashPage = object
  private
    flash    : ptrPIC16Flash;  //puntero a memoria Flash
    AddrStart: word;           //dirección de inicio en la memoria flash total
  private
    function Getmem(i : word): TPIC16FlashCell;
    procedure Setmem(i : word; AValue: TPIC16FlashCell);
  public
    procedure Init(AddrStart0: word; flash0: ptrPIC16Flash);  //inicia objeto
    property mem[i : word] : TPIC16FlashCell read Getmem write Setmem;
    //funciones para administración de la memoria
    function Total: word; //total de bytes que contiene
  end;

type
  TPICpinType = (
    pptVcc,    //Alimentación
    pptGND,    //Tierra
    pptControl,//Pin de control
    pptPort,   //Puerto Entrada/Salida
    pptUnused  //Pin no usado
  );

  //Modela a un pin del PIC

  { TPICpin }

  TPICpin = object
    nam: string;      //Eqtiueta o nombre
    typ: TPICpinType; //Tipo de pin
    add: word;        //Dirección en RAM
    bit: byte;        //Bit en RAM
    function GetLabel: string;
  end;

  {Objeto que representa al hardware de un PIC de la serie 16}
  { TPIC16 }
  TPIC16 = class
  private  //Creación de archivo *.hex
    hexLines : TStringList;  //Uusado para crear archivo *.hex
    minUsed  : word;         //Dirección menor de la ROM usada
    maxUsed  : word;         //Dirección mayor de la ROM usdas
    function HexChecksum(const lin: string): string;
    procedure GenHexComm(comment: string);
    procedure GenHexExAdd(Data: word);
    procedure GenHexData(Address: word; Data: string);
    procedure GenHexEOF;
    function StrHexFlash(i1, i2: integer): string;
  private //Campos para procesar instrucciones
    FMaxFlash: integer;
    idIns: TPIC16Inst;    //ID de Instrucción.
    d_   : TPIC16destin;  //Destino de operación. Válido solo en algunas instrucciones.
    f_   : byte;          //Registro destino. Válido solo en algunas instrucciones.
    b_   : byte;          //Bit destino. Válido solo en algunas instrucciones.
    k_   : word;          //Parámetro Literal. Válido solo en algunas instrucciones.
    function GetBank(i : Longint): TRAMBank;
    function GetINTCON: byte;
    function GetINTCON_GIE: boolean;
    function GetPage(i : Longint): TFlashPage;
    function GetSTATUS: byte;
    function GetSTATUS_C: boolean;
    function GetSTATUS_DC: boolean;
    function GetSTATUS_IRP: boolean;
    function GetSTATUS_Z: boolean;
    procedure SetINTCON_GIE(AValue: boolean);
    procedure SetSTATUS_C(AValue: boolean);
    procedure SetSTATUS_DC(AValue: boolean);
    procedure SetSTATUS_IRP(AValue: boolean);
    procedure SetSTATUS_Z(AValue: boolean);
    procedure SetMaxFlash(AValue: integer);
    procedure SetFRAM(value: byte);
    function GetFRAM: byte;
  public   //Campos que modelan a los registros internos
    W        : byte;   //Registro de trabajo
    PCL      : byte;   //Contador de Programa L
    PCH      : byte;   //Contador de Programa H
    //pc     : word absolute PCL. //Se debería optimziar así, viendo compatib. en el hardware
    PCLATH   : byte;   //Contador de Programa H
    STKPTR   : 0..7;   //Puntero de pila
    STACK    : array[0..7] of word;
    pines    : array[1..PIC_MAX_PINES] of TPICpin;
    property STATUS: byte read GetSTATUS;
    property STATUS_Z: boolean read GetSTATUS_Z write SetSTATUS_Z;
    property STATUS_C: boolean read GetSTATUS_C write SetSTATUS_C;
    property STATUS_DC: boolean read GetSTATUS_DC write SetSTATUS_DC;
    property STATUS_IRP: boolean read GetSTATUS_IRP write SetSTATUS_IRP;
    property INTCON: byte read GetINTCON;
    property INTCON_GIE: boolean read GetINTCON_GIE write SetINTCON_GIE;
    property FRAM: byte read GetFRAM write SetFRAM;
  public   //Control de ejecución
    nClck : Int64;  //Contador de ciclos de reloj
    CommStop: boolean;  //Bandera para detener la ejecución
    OnExecutionMsg: procedure(message: string) of object;  //Genera mensaje en ejecución
    function CurInstruction: TPIC16Inst;
    procedure Exec(pc: word);  //Ejecuta la instrucción en la dirección indicada.
    procedure Exec();  //Ejecuta instrucción actual
    procedure ExecTo(endAdd: word);  //Ejecuta hasta cierta dirección
    procedure ExecNCycles(nCyc: integer; out stopped: boolean);  //Ejecuta hasta cierta dirección
    procedure Reset;
    procedure AddBreakopint(pc: word);
    procedure ToggleBreakopint(pc: word);
  public
    //memorias
    flash    : TPIC16Flash;   //memoria Flash
    ram      : TPIC16Ram;     //memoria RAM
    Model    : string;    //modelo de PIC
    Npins    : byte;      //número de pines
    frequen  : integer;   //frecuencia del reloj
    MaxFreq  : integer;   //máxima frecuencia del reloj
    //Propiedades que definen la arquitectura del PIC destino.
    NumBanks: byte;      //Número de bancos de RAM.
    NumPages: byte;      //Número de páginas de memoria Flash.
    bank0, bank1, bank2, bank3: TRAMBank;  //bancos de memoria RAM
    page0, page1, page2, page3: TFlashPage;  //páginas de memoria Flash
    iFlash: integer;   //puntero a la memoria Flash, para escribir
    MsjError: string;
    procedure Decode(const opCode: word);  //decodifica instrucción
    function Disassembler(const opCode: word; bankNum: byte = 255;
      useVarName: boolean = false): string;  //Desensambla la instrucción actual
    property banks[i : Longint]: TRAMBank Read GetBank;
    property pages[i : Longint]: TFlashPage Read GetPage;
    property MaxFlash: integer read FMaxFlash write SetMaxFlash;   {Máximo número de celdas de flash implementadas (solo en los casos de
                         implementación parcial de la Flash). Solo es aplicable cuando es mayor que 0}
    //Funciones para la memoria RAM
    function HaveConsecGPR(const i, n: word; maxRam: word): boolean; //Indica si hay "n" bytes libres
    procedure UseConsecGPR(const i, n: word);  //Ocupa "n" bytes en la posición "i"
    function GetFreeBit(var offs, bnk, bit: byte; shared: boolean): boolean;
    function GetFreeByte(out offs, bnk: byte; shared: boolean): boolean;
    function GetFreeBytes(const size: integer; var offs, bnk: byte): boolean;  //obtiene una dirección libre
    function TotalMemRAM: word; //devuelve el total de memoria RAM
    function UsedMemRAM: word;  //devuelve el total de memoria RAM usada
    procedure ExploreUsed(rutExplorRAM: TRutExplorRAM);    //devuelve un reporte del uso de la RAM
    function ValidRAMaddr(addr: word): boolean;  //indica si una posición de memoria es válida
    procedure ClearMemRAM;
    procedure DisableAllRAM;
    procedure SetStatRAM(i1, i2: word; status0: TPIC16CellState);
    procedure SetMappRAM(i1, i2: word; MappedTo: word);
    function SetStatRAMCom(strDef: string): boolean;
    function SetMappRAMCom(strDef: string): boolean;
    function MapRAMtoPIN(strDef: string): boolean;
    procedure SetPin(pNumber: integer; pLabel: string; pType: TPICpinType);
    function SetUnimpBITS(strDef: string): boolean;
    function BankToAbsRAM(const offset, bank: byte): word; //devuelve dirección absoluta
    procedure AbsToBankRAM(const AbsAddr: word; var offset, bank: byte); //convierte dirección absoluta
    //funciones para manejo de nombres
    function NameRAM(const addr: word; const bnk: byte): string;
    function NameRAMbit(const addr: word; const bnk,bit: byte): string;
    procedure SetNameRAM(const addr: word; const bnk: byte; const nam: string);  //Fija nombre a una celda de RAM
    procedure AddNameRAM(const addr: word; const bnk: byte; const nam: string);  //Agrega nombre a una celda de RAM
    procedure SetNameRAMbit(const addr: word; const bnk, bit: byte; const nam: string);  //Fija nombre a un bitde RAM
    //funciones para la memoria Flash
    function UsedMemFlash: word;  //devuelve el total de memoria Flash usada
    procedure ClearMemFlash;
    procedure SetSharedUnused;
    procedure SetSharedUsed;
    //Métodos para codificar instrucciones de acuerdo a la sintaxis
    procedure useFlash;
    procedure codAsmFD(const inst: TPIC16Inst; const f: byte; d: TPIC16destin);
    procedure codAsmF(const inst: TPIC16Inst; const f: byte);
    procedure codAsmFB(const inst: TPIC16Inst; const f: byte; b: byte);
    procedure codAsmK(const inst: TPIC16Inst; const k: byte);
    procedure codAsmA(const inst: TPIC16Inst; const a: word);
    procedure codAsm(const inst: TPIC16Inst);
    procedure codGotoAt(iflash0: integer; const k: word);
    procedure codCallAt(iflash0: integer; const k: word);
    //Métodos adicionales
    function FindOpcode(Op: string; var syntax: string): TPIC16Inst;  //busca Opcode
    procedure addTopLabel(lbl: string);  //Add a comment to the ASM code
    procedure addTopComm(comm: string; replace: boolean = true);  //Add a comment to the ASM code
    procedure addSideComm(comm: string; before: boolean); //Add lateral comment to the ASM code
    procedure addPosInformation(rowSrc, colSrc: word; idFile: byte);
    procedure GenHex(hexFile: string; ConfigWord: integer = - 1);  //genera un archivo hex
    procedure DumpCode(lOut: TStrings; incAdrr, incCom, incVarNam: boolean);  //vuelva en código que contiene
  public
    constructor Create;
    destructor Destroy; override;
  end;

var  //variables globales
  //mnemónico de las instrucciones
  PIC16InstName: array[low(TPIC16Inst)..high(TPIC16Inst)] of string[7];
  //sintaxis en ensamblador de las instrucciones
  PIC16InstSyntax: array[low(TPIC16Inst)..high(TPIC16Inst)] of string[5];

implementation

{ TPICpin }
function TPICpin.GetLabel: string;
{Devuelve una etiqueta para el pin}
begin
  case typ of
  pptUnused: Result := 'NC';
  else
    Result := nam;
  end;
end;
{ TPIC16RamCell }
function TPIC16RamCell.Getused: byte;
begin
  if mappedTo = nil then begin
    //Celda independiente
    Result := Fused;
  end else begin
    //Celda reflejada. Leemos la disponibilidad, de la celda origen.
    Result := mappedTo^.used;
  end;
end;
procedure TPIC16RamCell.Setused(AValue: byte);
begin
  if mappedTo = nil then begin
    //Celda independiente
    Fused := AValue;
  end else begin
    //Celda reflejada. Escribimos la disponibilidad, en la celda origen.
    mappedTo^.used := AValue;
  end;
end;
function TPIC16RamCell.Getvalue: byte;
begin
  if mappedTo = nil then begin
    //Celda independiente
    Result := Fvalue;
  end else begin
    //Celda reflejada. Leemos la disponibilidad, de la celda origen.
    Result := mappedTo^.Fvalue;
  end;
end;
procedure TPIC16RamCell.Setvalue(AValue: byte);
begin
  if mappedTo = nil then begin
    //Celda independiente
    Fvalue := AValue;
  end else begin
    //Celda reflejada. Escribimos la disponibilidad, en la celda origen.
    mappedTo^.Fvalue:= AValue;
  end;
end;
function TPIC16RamCell.AvailGPR: boolean;
{Indica si el registro es una dirección disponible en la memoria RAM.}
begin
  Result := (state = cs_impleGPR) and (mappedTo = nil);
end;
{ TRAMBank }
//procedure TRAMBank.Setmem(i: byte; AValue: TPIC16RamCellPtr);
////Escribe en un banco de memoria
//begin
//  //Se asume que i debe ser menor que $7F
//  if ram^[i+AddrStart].state = cs_mapToBnk then begin
//    //estas direcciones están mapeadas en otro banco
//    BankMapped^.mem[i] := AValue;
//  end else begin  //caso normal
//    ram^[i+AddrStart] := AValue;
//  end;
//end;
procedure TRAMBank.Init(num: byte; AddrStart0: word;
  ram0: PIC16RamPtr);
begin
  numBank := num;
  AddrStart :=AddrStart0;
  ramPtr       :=ram0;
end;
{ TFlashPage }
function TFlashPage.Getmem(i: word): TPIC16FlashCell;
begin
  //Se asume que i debe ser menor que $800
  Result := flash^[i+AddrStart];
end;
procedure TFlashPage.Setmem(i: word; AValue: TPIC16FlashCell);
begin
  flash^[i+AddrStart] := AValue;
end;
procedure TFlashPage.Init(AddrStart0: word; flash0: ptrPIC16Flash);
begin
  AddrStart :=AddrStart0;
  flash     :=flash0;
end;
function TFlashPage.Total: word;
begin
  Result := PIC_PAGE_SIZE;  //tamaño fijo
end;

{ TPIC16 }
procedure TPIC16.useFlash;
{Marca la posición actual, como usada, e incrementa el puntero iFlash. S ihay error,
actualiza el campo "MsjError"}
begin
  //Protección de desborde
  if iFlash > MaxFlash then begin
    MsjError := 'FLASH Memory limit exceeded.';
    exit;
  end;
  flash[iFlash].used := true;  //marca como usado
  inc(iFlash);
end;
procedure TPIC16.codAsmFD(const inst: TPIC16Inst; const f: byte; d: TPIC16destin);
{Codifica las instrucciones orientadas a registro, con sinatxis: NEMÓNICO f,d}
begin
  case inst of
  ADDWF : flash[iFlash].value := %00011100000000 + ord(d) + (f and %1111111);
  ANDWF : flash[iFlash].value := %00010100000000 + ord(d) + (f and %1111111);
  COMF  : flash[iFlash].value := %00100100000000 + ord(d) + (f and %1111111);
  DECF  : flash[iFlash].value := %00001100000000 + ord(d) + (f and %1111111);
  DECFSZ: flash[iFlash].value := %00101100000000 + ord(d) + (f and %1111111);
  INCF  : flash[iFlash].value := %00101000000000 + ord(d) + (f and %1111111);
  INCFSZ: flash[iFlash].value := %00111100000000 + ord(d) + (f and %1111111);
  IORWF : flash[iFlash].value := %00010000000000 + ord(d) + (f and %1111111);
  MOVF  : flash[iFlash].value := %00100000000000 + ord(d) + (f and %1111111);
  RLF   : flash[iFlash].value := %00110100000000 + ord(d) + (f and %1111111);
  RRF   : flash[iFlash].value := %00110000000000 + ord(d) + (f and %1111111);
  SUBWF : flash[iFlash].value := %00001000000000 + ord(d) + (f and %1111111);
  SWAPF : flash[iFlash].value := %00111000000000 + ord(d) + (f and %1111111);
  XORWF : flash[iFlash].value := %00011000000000 + ord(d) + (f and %1111111);
  else
    raise Exception.Create('Implementation Error.');
  end;
  useFlash;  //marca como usado e incrementa puntero.
end;
procedure TPIC16.codAsmF(const inst: TPIC16Inst; const f: byte);
{Codifica las instrucciones orientadas a registro, con sinatxis: NEMÓNICO f}
begin
  case inst of
  CLRF  : flash[iFlash].value := %00000110000000 + (f and %1111111);
  MOVWF : flash[iFlash].value := %00000010000000 + (f and %1111111);
  else
    raise Exception.Create('Implementation Error.');
  end;
  useFlash;  //marca como usado e incrementa puntero.
end;
procedure TPIC16.codAsmFB(const inst: TPIC16Inst; const f: byte; b: byte);
//Codifica las instrucciones orientadas a bit.
begin
  case inst of
  BCF  : flash[iFlash].value := %01000000000000 + word(b<<7) + (f and %1111111);
  BSF  : flash[iFlash].value := %01010000000000 + word(b<<7) + (f and %1111111);
  BTFSC: flash[iFlash].value := %01100000000000 + word(b<<7) + (f and %1111111);
  BTFSS: flash[iFlash].value := %01110000000000 + word(b<<7) + (f and %1111111);
  else
    raise Exception.Create('Implementation Error.');
  end;
  useFlash;  //marca como usado e incrementa puntero.
end;
procedure TPIC16.codAsmK(const inst: TPIC16Inst; const k: byte);
{Codifica las instrucciones con constantes.}
begin
  case inst of
  ADDLW : flash[iFlash].value := %11111000000000 + k;
  ANDLW : flash[iFlash].value := %11100100000000 + k;
  IORLW : flash[iFlash].value := %11100000000000 + k;
  MOVLW : flash[iFlash].value := %11000000000000 + k;
  RETLW : flash[iFlash].value := %11010000000000 + k;
  SUBLW : flash[iFlash].value := %11110000000000 + k;
  XORLW : flash[iFlash].value := %11101000000000 + k;
  else
    raise Exception.Create('Implementation Error.');
  end;
  useFlash;  //marca como usado e incrementa puntero.
end;
procedure TPIC16.codAsmA(const inst: TPIC16Inst; const a: word);
{Codifica las instrucciones de control.
 "a" debe ser word, porque la dirección destino, requiere 11 bits.}
begin
  case inst of
  CALL  : flash[iFlash].value := %10000000000000 + (a and %11111111111);
  GOTO_ : flash[iFlash].value := %10100000000000 + (a and %11111111111);
  else
    raise Exception.Create('Implementation Error.');
  end;
  useFlash;  //marca como usado e incrementa puntero.
end;
procedure TPIC16.codAsm(const inst: TPIC16Inst);
//Codifica las instrucciones de control.
begin
  case inst of
  CLRW  : flash[iFlash].value := %00000110000000;
  NOP   : flash[iFlash].value := %00000000000000;
  CLRWDT: flash[iFlash].value := %00000001100100;
  RETFIE: flash[iFlash].value := %00000000001001;
  RETURN: flash[iFlash].value := %00000000001000;
  SLEEP : flash[iFlash].value := %00000001100011;
  else
    raise Exception.Create('Implementation Error.');
  end;
  useFlash;  //marca como usado e incrementa puntero.
end;
procedure TPIC16.codGotoAt(iflash0: integer; const k: word);
{Codifica una instrucción GOTO, en una posición específica y sin alterar el puntero "iFlash"
actual. Se usa para completar saltos indefinidos}
begin
  flash[iFlash0].value := %10100000000000 + (k and %11111111111);
end;
procedure TPIC16.codCallAt(iflash0: integer; const k: word);
{Codifica una instrucción CALL, en una posición específica y sin alterar el puntero "iFlash"
actual. Se usa para completar llamadas indefinidas}
begin
  flash[iFlash0].value := %10000000000000 + (k and %11111111111);
end;

function TPIC16.FindOpcode(Op: string; var syntax: string): TPIC16Inst;
{Busca una cádena que represente a una instrucción (Opcode). Si encuentra devuelve
 el identificador de instrucción y una cadena que representa a la sintaxis en "syntax".
 Si no encuentra devuelve "_Inval". }
var
  idInst: TPIC16Inst;
  tmp: String;
  found: Boolean;
begin
  found := false;
  tmp := UpperCase(Op);
  for idInst := low(TPIC16Inst) to high(TPIC16Inst) do begin
    if PIC16InstName[idInst] = tmp then begin
      found := true;
      break;
    end;
  end;
  if found then begin
    Result := idInst;
    syntax := PIC16InstSyntax[idInst];
  end else  begin
    Result := _Inval;
  end;
end;
procedure TPIC16.addTopLabel(lbl: string);
begin
  flash[iFlash].topLabel := lbl;
end;
procedure TPIC16.addTopComm(comm: string; replace: boolean);
{Agrega un comentario de línea al código en la posición de memoria actual}
begin
  if replace then begin
    flash[iFlash].topComment := comm;
  end else begin
    flash[iFlash].topComment := flash[iFlash].topComment + comm;
  end;
end;
procedure TPIC16.addSideComm(comm: string; before: boolean);
{Agrega un comentario para que apareza al lado de la instrucción.
 "before" = TRUE -> Se debe llamar después de codificar la instrucción
 "before" = FALSE -> Se debe llamar antes de codificar la instrucción
 }
begin
  if before then begin
    if iFlash= 0 then exit;
    flash[iFlash-1].sideComment+=comm;   //se agrega al que pudiera haber
  end else begin
    if iFlash= 0 then exit;
    flash[iFlash].sideComment+=comm;   //se agrega al que pudiera haber
  end;
end;
procedure TPIC16.addPosInformation(rowSrc, colSrc: word; idFile: byte);
{Agrega information de la posición en el codigo fuente, a la posición actual de la
memoria flash.}
begin
  flash[iFlash].rowSrc := rowSrc;
  flash[iFlash].colSrc := colSrc;
  flash[iFlash].idFile := idFile;
end;
//Creación de archivo *.hex
function TPIC16.HexChecksum(const lin:string): string;
//Devuelve los caracteres en hexadecimal del Checksum, para el archivo *.hex
var
  i: Integer;
  chk: Integer;
  part: String;
begin
   i:=1;
   chk := 0;
   while i<length(lin) do begin
     part := copy(lin,i,2);
     chk := chk + StrToInt('$'+part);
     inc(i,2);
   end;
   chk := not chk;  //complemento a 1
   inc(chk);        //complemento a 2
   part := IntToHex(chk,4);  //a hexadecimal
   Result := copy(part, length(part)-1,2);  //recorta
end;
procedure TPIC16.GenHexComm(comment: string);
//Agrega una línea de comentario al archivo *.hex
begin
  hexLines.Add(';'+comment);
end;
procedure TPIC16.GenHexExAdd(Data: word);
//Agrega una línea de Extended Address al archivo *.hex
const RecordType = '04';
var
  ByteCount: Integer;
  lin: String;
begin
  ByteCount := 2;
  lin:= IntToHex(ByteCount,2) + '0000' + RecordType +  IntToHex(Data,4);
  hexLines.Add(':' + lin + HexChecksum(lin));
end;
procedure TPIC16.GenHexData(Address: word; Data: string);
//Agrega una línea de datos al archivo *.hex
const RecordType = '00';
var
  ByteCount: Integer;
  lin: String;
begin
  ByteCount := length(data) div 2;
  lin:= IntToHex(ByteCount,2) + IntToHex(Address*2,4) + RecordType +  Data;
  hexLines.Add(':'+lin + HexChecksum(lin));
end;
procedure TPIC16.GenHexEOF;
//Agrega una línea de Extended Address al archivo *.hex
begin
  hexLines.Add(':00000001FF');
end;
function  TPIC16.StrHexFlash(i1, i2: integer): string;
{Devuelve la cadena, de bytes hexadecimales de la memoria Flash, desde la posición
 i1 hasta i2.}
var
  i: Integer;
  tmp: String;
begin
  Result:='';
  for i:=i1 to i2 do begin
    tmp := IntToHex(flash[i].value,4);
    Result+=copy(tmp,3,2) + copy(tmp,1,2);  //se graba con los bytes invertidos
  end;
end;
//Campos para procesar instrucciones
function TPIC16.GetBank(i : Longint): TRAMBank;
begin
  case i of
  0: Result := bank0;
  1: Result := bank1;
  2: Result := bank2;
  3: Result := bank3;
  else
    Result := bank0;
  end;
end;
function TPIC16.GetPage(i: Longint): TFlashPage;
begin
  case i of
  0: Result := page0;
  1: Result := page1;
  2: Result := page2;
  3: Result := page3;
  else
    Result := page0;
  end;
end;
function TPIC16.GetSTATUS: byte;
begin
  Result := ram[$03].Fvalue;
end;
function TPIC16.GetSTATUS_Z: boolean;
begin
  Result := (ram[$03].Fvalue and %00000100) <> 0;
end;
procedure TPIC16.SetSTATUS_Z(AValue: boolean);
begin
  if AVAlue then ram[$03].Fvalue := ram[$03].Fvalue or  %00000100
            else ram[$03].Fvalue := ram[$03].Fvalue and %11111011;
end;
function TPIC16.GetSTATUS_C: boolean;
begin
  Result := (ram[$03].Fvalue and %00000001) <> 0;
end;
procedure TPIC16.SetSTATUS_C(AValue: boolean);
begin
  if AVAlue then ram[$03].Fvalue := ram[$03].Fvalue or  %00000001
            else ram[$03].Fvalue := ram[$03].Fvalue and %11111110;
end;
function TPIC16.GetSTATUS_DC: boolean;
begin
  Result := (ram[$03].Fvalue and %00000010) <> 0;
end;
procedure TPIC16.SetSTATUS_DC(AValue: boolean);
begin
  if AVAlue then ram[$03].Fvalue := ram[$03].Fvalue or  %00000010
            else ram[$03].Fvalue := ram[$03].Fvalue and %11111101;
end;
function TPIC16.GetSTATUS_IRP: boolean;
begin
  Result := (ram[$03].Fvalue and %10000000) <> 0;
end;
procedure TPIC16.SetSTATUS_IRP(AValue: boolean);
begin
  if AVAlue then ram[$03].Fvalue := ram[$03].Fvalue or  %10000000
            else ram[$03].Fvalue := ram[$03].Fvalue and %01111111;
end;
function TPIC16.GetINTCON: byte;
begin
  Result := ram[$0B].Fvalue;
end;
function TPIC16.GetINTCON_GIE: boolean;
begin
  Result := (ram[$0B].Fvalue and %10000000) <> 0;
end;
procedure TPIC16.SetINTCON_GIE(AValue: boolean);
begin
  if AVAlue then ram[$0B].Fvalue := ram[$0B].Fvalue or  %10000000
            else ram[$0B].Fvalue := ram[$0B].Fvalue and %01111111;
end;
procedure TPIC16.SetMaxFlash(AValue: integer);
begin
  if FMaxFlash = AValue then Exit;
  FMaxFlash := AValue;
end;
procedure TPIC16.SetFRAM(value: byte);
{Escribe en la RAM; en la dirección global f_, el valor "value"
Para determinar el valor real de la dirección, se toma en cuenta los bits de STATUS}
begin
  if f_ = 0 then begin
    //Caso especial de direccionamiento indirecto
    if STATUS_IRP then begin
      ram[ram[04].value + $100].value := value;
    end else begin
      ram[ram[04].value].value := value;
    end;
    exit;
  end;
  {Se escribe aplicando la máscara de bits implementados. Se podría usra la máscara en
  lectura o escritura, pero se prefiere hacerlo en escritura, porque se espera que se
  hagan menos operaciones de escritura que lectura.}
  case STATUS and %01100000 of
  %00000000: ram[f_     ].value := value and ram[f_     ].Fimplem;
  %00100000: ram[f_+ $80].value := value and ram[f_+ $80].Fimplem;
  %01000000: ram[f_+$100].value := value and ram[f_+$100].Fimplem;
  %01100000: ram[f_+$180].value := value and ram[f_+$180].Fimplem;
  end;
end;
function TPIC16.GetFRAM: byte;
{Devuelve el valor de la RAM, de la posición global f_.
Para determinar el valor real de la dirección, se toma en cuenta los bits de STATUS}
begin
  if f_ = 0 then begin
    //Caso especial de direccionamiento indirecto
    if STATUS_IRP then begin
      Result := ram[ram[04].value + $100].value;
    end else begin
      Result := ram[ram[04].value].value;
    end;
    exit;
  end;
  case STATUS and %01100000 of
  %00000000: Result := ram[f_     ].value;
  %00100000: Result := ram[f_+ $80].value;
  %01000000: Result := ram[f_+$100].value;
  %01100000: Result := ram[f_+$180].value;
  end;
end;

procedure TPIC16.Decode(const opCode: word);
{Decodifica la instrucción indicada. Actualiza siempre la variable "idIns", y
dependiendo de la instrucción, puede actualizar: d_, f_, b_ y k_}
var
  codH : byte;  //6 bits altos de la instrucción
  codL : byte;  //byte bajo de la instrucción
begin
  codH := (opCode and $3F00) >> 8;  //se debería optimizar
  codL := opCode and $00FF;
  case codH of
  %000111: begin
    idIns := ADDWF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %000101: begin
    idIns := ANDWF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %000001: begin
    if (codL and %10000000) = %10000000 then begin
      idIns := CLRF;
      f_ := codL and %01111111;
    end else begin
      idIns := CLRW;
    end;
  end;
  %001001: begin
    idIns := COMF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %000011: begin
    idIns := DECF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %001011: begin
    idIns := DECFSZ;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %001010: begin
    idIns := INCF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %001111: begin
    idIns := INCFSZ;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %000100: begin
    idIns := IORWF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %001000: begin
    idIns := MOVF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %000000: begin
    if (codL and %10000000) = %10000000 then begin
      idIns := MOVWF;
      f_ := codL and %01111111;
    end else begin
      //bit7 a cero, hay varias opciones
      case codL of
      %00000000,
      %00100000,
      %01000000,
      %01100000: begin
        idIns := NOP;
      end;
      %01100100: begin
        idIns := CLRWDT;
      end;
      %00001001: begin
        idIns := RETFIE;
      end;
      %00001000: begin
        idIns := RETURN;
      end;
      %01100011: begin
        idIns := SLEEP;
      end;
      else
        idIns := _Inval;
      end;
    end;
  end;
  %001101: begin
    idIns := RLF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %001100: begin
    idIns := RRF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %000010: begin
    idIns := SUBWF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %001110: begin
    idIns := SWAPF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %000110: begin
    idIns := XORWF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %111110,
  %111111: begin
    idIns := ADDLW;
    k_ := codL;
  end;
  %111001: begin
    idIns := ANDLW;
    k_ := codL;
  end;
  %111000: begin
    idIns := IORLW;
    k_ := codL;
  end;
  %110000,
  %110001,
  %110010,
  %110011: begin
    idIns := MOVLW;
    k_ := codL;
  end;
  %110100,
  %110101,
  %110110,
  %110111: begin
    idIns := RETLW;
    k_ := codL;
  end;
  %111100,
  %111101: begin
    idIns := SUBLW;
    k_ := codL;
  end;
  %111010: begin
    idIns := XORLW;
    k_ := codL;
  end;
  else
    if (codH and %110000) = %010000 then begin
      case codH and %001100 of
      %0000: begin
        idIns := BCF;
        b_ := (opCode and %1110000000) >> 7;
        f_ := codL and %01111111;
      end;
      %0100: begin
        idIns := BSF;
        b_ := (opCode and %1110000000) >> 7;
        f_ := codL and %01111111;
      end;
      %1000: begin
        idIns := BTFSC;
        b_ := (opCode and %1110000000) >> 7;
        f_ := codL and %01111111;
      end;
      %1100: begin
        idIns := BTFSS;
        b_ := (opCode and %1110000000) >> 7;
        f_ := codL and %01111111;
      end;
      else
        idIns := _Inval;
      end;
    end else if (codH and %111000) = %100000 then begin
      idIns := CALL;
      k_ := opCode and %11111111111;
    end else if (codH and %111000) = %101000 then begin
      idIns := GOTO_;
      k_ := opCode and %11111111111;
    end else begin
      idIns := _Inval;
    end;
  end;
end;
function TPIC16.Disassembler(const opCode: word; bankNum: byte = 255;
                             useVarName: boolean = false): string;
{Desensambla la instrucción "opCode".
"bankNum" -> Es el banco de trabajo en el que se supone se está decodificando el OpCode.
             Se usa para determinar la dirección de memoria real a la que se accede. Si
             no se conoce el valor, se debe indciar 255.

}
var
  nemo: String;
begin
  Decode(opCode);   //decodifica instrucción
  nemo := lowerCase(trim(PIC16InstName[idIns])) + ' ';
  case idIns of
  ADDWF,
  ANDWF,
  COMF ,
  DECF ,
  DECFSZ,
  INCF,
  INCFSZ,
  IORWF,
  MOVF,
  RLF,
  RRF,
  SUBWF,
  SWAPF,
  XORWF: begin
      if useVarName and (ram[f_].name<>'') then begin
        //Include address name
        if d_ = toF then
          Result := nemo + ram[f_].name + ',f'
        else
          Result := nemo + ram[f_].name + ',w';
      end else begin
        //No include address name
        if d_ = toF then
          Result := nemo + '0x'+IntToHex(f_,3) + ',f'
        else
          Result := nemo + '0x'+IntToHex(f_,3) + ',w';
      end;
     end;
  CLRF,
  MOVWF: begin
        if useVarName and (ram[f_].name<>'') then begin
          Result := nemo + ram[f_].name;
        end else begin
          Result := nemo + '0x'+IntToHex(f_,3);
        end;
     end;
  BCF,
  BSF,
  BTFSC,
  BTFSS: begin    //Instrucciones de bit
      if useVarName and (ram[f_].bitname[b_]<>'') then begin
        //Hay nombre de bit
        Result := nemo + ram[f_].bitname[b_];
      end else if useVarName and (ram[f_].name<>'') then begin
        //Hay nombre de byte
        Result := nemo + ram[f_].name + ', ' + IntToStr(b_);
      end else begin
        Result := nemo + '0x'+IntToHex(f_,3) + ', ' + IntToStr(b_);
      end;
     end;
  ADDLW,
  ANDLW,
  IORLW,
  MOVLW,
  RETLW,
  SUBLW,
  XORLW: begin
       Result := nemo + '0x'+IntToHex(k_,2);
     end;
  CALL,
  GOTO_: begin   //Faltaría decodificar la dirección
    Result := nemo + '0x'+IntToHex(k_,3);
  end;
  CLRW,
  NOP,
  CLRWDT,
  RETFIE,
  RETURN,
  SLEEP: begin
       Result := nemo ;
     end;
  else
    Result := 'Invalid'
  end;
end;
function TPIC16.CurInstruction: TPIC16Inst;
{Devuelve la instrucción, a la cue apunta PC, actualmente}
var
  val: Word;
begin
  val := flash[PCH*256+PCL].value; // page0.mem[PCL].value;
  Decode(val);   //decodifica instrucción
  Result := idIns;
end;
procedure TPIC16.Exec;
{Executa la instrucción actual}
var
  pc: word;
begin
  pc := PCH*256+PCL;
  Exec(pc);
end;
procedure TPIC16.Exec(pc: word);
{Ejecuta la instrución actual con dirección "pc".
Falta implementar las operaciones, cuando acceden al registro INDF, el Watchdog timer,
los contadores, las interrupciones}
var
  opc: Word;
  //fullAdd: word;
  msk, resNib: byte;
  resByte, bit7, bit0: byte;
  resWord: word;
  resInt : integer;
begin
  //Decodifica instrucción
  opc := flash[pc].value; // page0.mem[PCL].value;
  Decode(opc);   //decodifica instrucción
  case idIns of
  ADDWF: begin
    resByte := FRAM;
    resWord := W + resByte;
    resNib := (W and $0F) + (resByte and $0F);
    if d_ = toF then begin
      FRAM := resWord and $FF;
    end else begin  //toW
      w := resWord and $FF;
    end;
    STATUS_Z := (resWord and $ff) = 0;
    STATUS_C := (resWord > 255);
    STATUS_DC := (resNib > 15);
  end;
  ANDWF: begin
    resByte := W and FRAM;
    if d_ = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
  end;
  CLRF: begin
    FRAM := 0;
    STATUS_Z := true;
  end;
  CLRW: begin
    W := 0;
    STATUS_Z := true;
  end;
  COMF : begin
    resByte := not FRAM;
    if d_ = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
  end;
  DECF : begin
    resByte := FRAM;
    if resByte = 0 then resByte := $FF else dec(resByte);
    if d_ = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
  end;
  DECFSZ: begin
    resByte := FRAM;
    if resByte = 0 then resByte := $FF else dec(resByte);
    if d_ = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
    if STATUS_Z then begin
      //salta una instrucción
      if PCL = 255 then begin
        PCL := 0;
        inc(PCH);
      end else begin
        inc(PCL);
      end;
      Inc(nClck);   //En este caso toma un ciclo más
    end;
  end;
  INCF: begin
    resByte := FRAM;
    if resByte = 255 then resByte := 0 else inc(resByte);
    if d_ = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
  end;
  INCFSZ: begin
    resByte := FRAM;
    if resByte = 255 then resByte := 0 else inc(resByte);
    if d_ = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
    if STATUS_Z then begin
      //salta una instrucción
      if PCL = 255 then begin
        PCL := 0;
        inc(PCH);
      end else begin
        inc(PCL);
      end;
      Inc(nClck);   //En este caso toma un ciclo más
    end;
  end;
  IORWF: begin
    resByte := W or FRAM;
    if d_ = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte <> 0;
  end;
  MOVF: begin
    resByte := FRAM;
    if d_ = toF then begin
      //no mueve, solo verifica
      STATUS_Z := (resByte = 0);
    end else begin  //toW
      w := resByte;
      STATUS_Z := (resByte = 0);
    end;
  end;
  MOVWF: begin
    FRAM := W;   //escribe a donde esté mapeado, (si está mapeado)
    if f_ = $02 then begin //Es el PCL
      PCH := PCLATH;  //Cuando se escribe en PCL, se carga PCH con PCLATH
    end;
  end;
  NOP: begin
  end;
  RLF: begin
    resByte := FRAM;
    bit7 := resByte and $80; //guarda bit 7
    resByte := (resByte << 1) and $ff;  //desplaza
    //pone C en bit bajo
    if STATUS_C then begin  //C era 1
      resByte := resByte or $01;  //pone a 1 el bit 0
    end else begin          //C era 0
      //no es necesario agregarlo, porque por defecto se agrega 0
    end;
    //Actualiza C
    if bit7 = 0 then STATUS_C := false
                else STATUS_C := true;
    if d_ = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
  end;
  RRF: begin
    resByte := FRAM;
    bit0 := resByte and $01; //guarda bit 0
    resByte := resByte >> 1;  //desplaza
    //pone C en bit alto
    if STATUS_C then begin  //C era 1
      resByte := resByte or $80;  //pone a 1 el bit 0
    end else begin          //C era 0
      //no es necesario agregarlo, porque por defecto se agrega 0
    end;
    //Actualiza C
    if bit0 = 0 then STATUS_C := false
                else STATUS_C := true;
    if d_ = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
  end;
  SUBWF: begin
    resByte := FRAM;
    resInt := resByte - W;
    if d_ = toF then begin
      FRAM :=  resInt and $FF;
    end else begin  //toW
      w := resInt and $FF;
    end;
    STATUS_Z := (resInt = 0);
    if resInt < 0 then STATUS_C := false   //negativo
    else STATUS_C := true;
    resInt := (resByte and $0F) - (W and $0F);
    if resInt < 0 then STATUS_DC := false   //negativo
    else STATUS_DC := true;
  end;
  SWAPF: begin
    resByte := FRAM;
    FRAM := (resByte >> 4) or (resByte << 4);
  end;
  XORWF: begin
    resByte := W xor FRAM;
    if d_ = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte <> 0;
  end;
  //BIT-ORIENTED FILE REGISTER OPERATIONS
  BCF: begin
    msk := $1 << b_;
    msk := not msk;
    FRAM := FRAM and msk;
  end;
  BSF: begin
    msk := $1 << b_;
    FRAM := FRAM or msk;// b_
  end;
  BTFSC: begin
    msk := $1 << b_;
    if (FRAM and msk) = 0 then begin
      //salta una instrucción
      if PCL = 255 then begin
        PCL := 0;
        inc(PCH);
      end else begin
        inc(PCL);
      end;
      Inc(nClck);   //En este caso toma un ciclo más
    end;
  end;
  BTFSS: begin
    msk := $1 << b_;
    if (FRAM and msk) <> 0 then begin
      //salta una instrucción
      if PCL = 255 then begin
        PCL := 0;
        inc(PCH);
      end else begin
        inc(PCL);
      end;
      Inc(nClck);   //En este caso toma un ciclo más
    end;
  end;
  //LITERAL AND CONTROL OPERATIONS
  ADDLW: begin
    resWord := W + k_;
    resNib := (W and $0F) + (k_ and $0F);
    w := resWord and $FF;
    STATUS_Z := (resWord and $ff) = 0;
    STATUS_C := (resWord > 255);
    STATUS_DC := (resNib > 15);
  end;
  ANDLW: begin
    resByte := W and K_;
    w := resByte;
    STATUS_Z := resByte = 0;
  end;
  CALL: begin
    //Guarda dirección en Pila
    STACK[STKPTR] := PCH * 256 + PCL;
    if STKPTR = 7 then begin
      //Desborde de pila
      STKPTR := 0;
      if OnExecutionMsg<>nil then OnExecutionMsg('Stack Overflow on CALL OpCode at $' + IntToHex(pc,4));
    end else begin
      STKPTR := STKPTR +1;
    end;
    //En k, deben haber 11 bits
    PCL := k_ and $FF;
    PCH := word(k_ >> 8) or   //toma los 3 bits restantes de k
           (PCLATH and %00011000);  //y completa con los bits 3 y 4 de PCLATH
    Inc(nClck,2);   //Esta instrucción toma dos ciclos
    exit;
  end;
  CLRWDT: begin
  end;
  GOTO_: begin
      //En k, deben haber 11 bits
      PCL := k_ and $FF;
      PCH := byte(k_ >> 8) or   //toma los 3 bits restantes de k
             (PCLATH and %00011000);  //y completa con los bits 3 y 4 de PCLATH
      Inc(nClck,2);   //Esta instrucción toma dos ciclos
      exit;
  end;
  IORLW: begin
    resByte := W or k_;
    w := resByte;
    STATUS_Z := resByte <> 0;
  end;
  MOVLW: begin
      W := k_;
  end;
  RETFIE: begin
    //Saca dirección en Pila
    if STKPTR = 0 then begin
      //Desborde de pila
      STKPTR := 7;
      if OnExecutionMsg<>nil then OnExecutionMsg('Stack Overflow on RETFIE OpCode at $' + IntToHex(pc,4));
    end else begin
      STKPTR := STKPTR - 1;
    end;
    PCH := hi(STACK[STKPTR]);  //solo debería haber 5 bits
    PCL := lo(STACK[STKPTR]);
    Inc(nClck);   //Esta instrucción toma un ciclo más
    //Activa GIE
    INTCON_GIE := true;
  end;
  RETLW: begin
    //Saca dirección en Pila
    if STKPTR = 0 then begin
      //Desborde de pila
      STKPTR := 7;
      if OnExecutionMsg<>nil then OnExecutionMsg('Stack Overflow on RETLW OpCode at $' + IntToHex(pc,4));
    end else begin
      STKPTR := STKPTR - 1;
    end;
    PCH := hi(STACK[STKPTR]);  //solo debería haber 5 bits
    PCL := lo(STACK[STKPTR]);
    Inc(nClck);   //Esta instrucción toma un ciclo más
    //Fija valor en W
    W := k_;
  end;
  RETURN: begin
    //Saca dirección en Pila
    if STKPTR = 0 then begin
      //Desborde de pila
      STKPTR := 7;
      if OnExecutionMsg<>nil then OnExecutionMsg('Stack Overflow on RETURN OpCode at $' + IntToHex(pc,4));
    end else begin
      STKPTR := STKPTR - 1;
    end;
    PCH := hi(STACK[STKPTR]);  //solo debería haber 5 bits
    PCL := lo(STACK[STKPTR]);
    Inc(nClck);   //Esta instrucción toma un ciclo más
  end;
  SLEEP: begin
  end;
  SUBLW: begin
    resInt := k_ - W;
    w := resInt and $FF;
    STATUS_Z := (resInt = 0);
    if resInt < 0 then STATUS_C := false   //negativo
    else STATUS_C := true;
    resInt := (k_ and $0F) - (W and $0F);
    if resInt < 0 then STATUS_DC := false   //negativo
    else STATUS_DC := true;
  end;
  XORLW: begin
    resByte := W xor k_;
    w := resByte;
    STATUS_Z := resByte <> 0;
  end;
  _Inval: begin
    MsjError := 'Invalid Opcode';
  end;
  end;
  //Incrementa contador
  if PCL = 255 then begin
    PCL := 0;
    inc(PCH);
  end else begin
    inc(PCL);
  end;
  Inc(nClck);
end;
procedure TPIC16.ExecTo(endAdd: word);
{Ejecuta las instrucciones secuencialmente, desde la instrucción actual, hasta que el
contador del programa, sea igual a la dirección "endAdd".}
var
  pc: word;
begin
  //Hace una primera ejecución, sin verificar Breakpoints
  pc := PCH<<8+PCL;
  Exec(pc);
  //Ejecuta cíclicamnente
  pc := PCH<<8+PCL;
  while pc <> endAdd do begin
    if flash[pc].breakPnt then begin
      //Encontró un BreakPoint, sale sin ejecutar esa instrucción
      if OnExecutionMsg<>nil then OnExecutionMsg('Stopped for breakpoint.');
//      stopped := true;
      exit;
    end;
    //Ejecuta
    Exec(pc);
    pc := PCH<<8+PCL;  //Actuliza Contador de programa
  end;
end;
procedure TPIC16.ExecNCycles(nCyc: integer; out stopped: boolean);
{Ejecuta el número de ciclos indicados, o hasta que se produzca alguna condición
externa, que puede ser:
* Se encuentre un Punto de Interrupción.
* Se detecta la señal, de detenerse.
* Se genere algún error en la ejecución.
* Se ejecuta la instrucción SLEEP.
la bandera "stopped", indica que se ha detendio la ejecución sin completar la cantidad
de instrucciones requeridas.
Normalmente Se ejecutará el número de ciclos indicados, pero en algunos casos se
ejecutará un ciclo más, debido a que algunas instrucciones toman dos ciclos.}
var
  clkEnd: Int64;
  pc: word;
begin
  clkEnd := nClck + nCyc;   //Valor final del contador
  while nClck < clkEnd do begin
    pc := PCH<<8+PCL;
    if flash[pc].breakPnt then begin
      //Encontró un BreakPoint, sale sin ejecutar esa instrucción
      if OnExecutionMsg<>nil then OnExecutionMsg('Stopped for breakpoint.');
      stopped := true;
      exit;
    end;
    if not flash[pc].used then begin
      //Encontró un BreakPoint, sale sin ejecutar esa instrucción
      if OnExecutionMsg<>nil then OnExecutionMsg('Stopped for executing unused code.');
      stopped := true;
      exit;
    end;
    if CommStop then begin
      //Se detectó el comando STOP
      if OnExecutionMsg<>nil then OnExecutionMsg('Stopped for STOP command.');
      stopped := true;
      exit;
    end;
    //Ejecuta
    Exec(pc);
    if idIns = SLEEP then begin
      //Encontró un BreakPoint, sale sin ejecutar esa instrucción
      if OnExecutionMsg<>nil then OnExecutionMsg('Stopped for SLEEP Opcode.');
      stopped := true;
      exit;
    end;
  end;
  stopped := false;
end;
procedure TPIC16.Reset;
//Reinicia el dipsoitivo
var
  i: Integer;
begin
  PCL := 0;
  PCLATH := 0;
  PCH := 0;
  W := 0;
  STKPTR := 0;   //Posición inicial del puntero de pila
  nClck := 0;    //Inicia contador de ciclos
  CommStop := false;  //Limpia bandera
  //Limpia solamente el valor inicial, no toca los otros campos
  for i:=0 to high(ram) do begin
    ram[i].Fvalue := $00;
  end;
  ram[$03].Fvalue := %00011000;  //STATUS
end;
procedure TPIC16.AddBreakopint(pc: word);
//Agrega un punto de interrupción
begin
  if pc>=PIC_MAX_FLASH then exit;
  flash[pc].breakPnt := true;
end;
procedure TPIC16.ToggleBreakopint(pc: word);
//COnmuta el estado del Punto de Interrupción, en la posición indicada
begin
  if pc>=PIC_MAX_FLASH then exit;
  flash[pc].breakPnt := not flash[pc].breakPnt;
end;
//Funciones para la memoria RAM
function TPIC16.HaveConsecGPR(const i, n: word; maxRam: word): boolean;
{Indica si hay "n" bytes consecutivos libres en la posicióm "i", en RAM.
La búsqueda se hace solo hasta la posición "maxRam"}
var
  c: Integer;
  j: word;
begin
  Result := false;
  c := 0;
  j := i;
  while (j<=maxRam) and (c<n) do begin
    if (ram[j].state <> cs_impleGPR) or (ram[j].used <> 0) then exit;
    inc(c);      //verifica siguiente
    inc(j);
  end;
  if j>maxRam then exit;  //no hay más espacio
  //Si llega aquí es porque estaban libres los bloques
  Result := true;
end;
procedure TPIC16.UseConsecGPR(const i, n: word);
{Marca "n" bytes como usados en la posición de memoria "i", en la RAM.
 Debe haberse verificado previamente que los parámetros son válidos, porque aquí no
 se hará ninguna verificación.}
var j: word;
begin
  for j:=i to i+n-1 do begin
    ram[j].used:=255;  //todos los bits
  end;
end;
function TPIC16.GetFreeBit(var offs, bnk, bit: byte; shared: boolean): boolean;
{Devuelve una dirección libre de la memoria RAM (y el banco).
"Shared" indica que se marcará el bit como de tipo "Compartido", y se usa para el
caso en que se quiera comaprtir la misma posición para diversos variables.
Si encuentra espacio, devuelve TRUE.}
var
  maxRam: word;
  i: Integer;
begin
  Result := false;   //valor inicial
  maxRam := NumBanks * $80;  //posición máxima
  //Realmente debería explorar solo hasta la dirección implementada, por eficiencia
  for i:=0 to maxRam-1 do begin
    if (ram[i].state = cs_impleGPR) and (ram[i].used <> 255) then begin
      //Esta dirección tiene al menos un bit libre
      offs := i and $7F;  //devuelve dirección
      bnk := i >> 7;
      //busca el bit libre
      if          (ram[i].used and %00000001) = 0 then begin
        bit:=0;
      end else if (ram[i].used and %00000010) = 0 then begin
        bit:=1
      end else if (ram[i].used and %00000100) = 0 then begin
        bit:=2
      end else if (ram[i].used and %00001000) = 0 then begin
        bit:=3
      end else if (ram[i].used and %00010000) = 0 then begin
        bit:=4
      end else if (ram[i].used and %00100000) = 0 then begin
        bit:=5
      end else if (ram[i].used and %01000000) = 0 then begin
        bit:=6
      end else if (ram[i].used and %10000000) = 0 then begin
        bit:=7
      end;
      ram[i].used := ram[i].used or (byte(1)<<bit); //marca bit usado
      if shared then begin
        ram[i].shared:= ram[i].shared or (byte(1)<<bit); //marca bit compartido
      end;
      //Notar que la posición de memoria puede estar mapeada a otro banco.
      Result := true;  //indica que encontró espacio
      exit;
    end;
  end;
end;
function TPIC16.GetFreeByte(out offs, bnk: byte; shared: boolean): boolean;
{Devuelve una dirección libre de la memoria flash (y el banco).
"Shared" indica que se marcará el bit como de tipo "Compartido", y se usa para el
caso en que se quiera comaprtir la misma posición para diversos variables.
Si encuentra espacio, devuelve TRUE.}
var
  i: Integer;
  maxRam: word;
begin
  Result := false;   //valor inicial
  maxRam := NumBanks * $80;  //posición máxima
  //Realmente debería explorar solo hasta la dirección implementada, por eficiencia
  for i:=0 to maxRam-1 do begin
    if (ram[i].state = cs_impleGPR) and (ram[i].used = 0) then begin
      //Esta dirección está libre
      ram[i].used:=255;   //marca como usado
      if shared then begin
        ram[i].shared := 255;  //Marca como compartido
      end;
      offs := i and $7F;  //devuelve dirección
      bnk := i >> 7;
      //Notar que la posición de memoria puede estar mapeada a otro banco.
      Result := true;  //indica que encontró espacio
      exit;
    end;
  end;
end;
function TPIC16.GetFreeBytes(const size: integer; var offs, bnk: byte): boolean;
{Devuelve una dirección libre de la memoria flash (y el banco) para ubicar un bloque
 del tamaño indicado. Si encuentra espacio, devuelve TRUE.
 El tamaño se da en bytes, pero si el valor es negativo, se entiende que es en bits.}
var
  i: word;
  maxRam: Word;
begin
  Result := false;  //valor por defecto
  if size=0 then exit;
  maxRam := word(NumBanks * $80) - 1;
  for i:=0 to maxRam do begin  //verifica 1 a 1, por seguridad
    if HaveConsecGPR(i, size, maxRam) then begin
      //encontró del tamaño buscado
      UseConsecGPR(i, size);  //marca como usado
      offs := i and $7F;
      bnk  := i >> 7;
      Result := true;  //indica que encontró espacio
      exit;
    end;
  end;
end;
function TPIC16.TotalMemRAM: word;
{Devuelve el total de memoria RAM disponible}
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to word(NumBanks * $80) - 1 do begin
    if ram[i].AvailGPR then begin
      Result := Result + 1;
    end;
  end;
end;
function TPIC16.UsedMemRAM: word;
{Devuelve el total de memoria RAM usada}
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to word(NumBanks * $80) - 1 do begin
    if ram[i].AvailGPR and (ram[i].used <> 0) then begin
      //Notar que "AvailGPR" asegura que no se consideran registros maepados
      Result := Result + 1;
    end;
  end;
end;
procedure TPIC16.ExploreUsed(rutExplorRAM: TRutExplorRAM);
{Genera un reporte de uso de RAM}
var
  i: Integer;
begin
  for i := 0 to word(NumBanks * $80) - 1 do begin
    if ram[i].AvailGPR and (ram[i].used <> 0) then begin
      rutExplorRAM(i, 0, @ram[i]);
    end;
  end;
end;
function TPIC16.ValidRAMaddr(addr: word): boolean;
{Indica si la dirercción indicada es váldia dentro del hardware del PIC}
begin
  case NumBanks of
  1: begin
      if addr > $80 then exit(false);   //excede límite
  end;
  2: begin
      if addr > $100 then exit(false);   //excede límite
  end;
  3: begin
      if addr > $180 then exit(false);   //excede límite
  end;
  4: begin
      if addr > $200 then exit(false);   //excede límite
  end;
  end;
  exit(true);
end;
procedure TPIC16.ClearMemRAM;
{Limpia el contenido de la memoria}
var
  i: Integer;
begin
  for i:=0 to high(ram) do begin
    ram[i].Fvalue := $00;
    ram[i].used := 0;
    ram[i].name:='';
    ram[i].shared := 0;
//    ram[i].state := cs_unimplem;  //por defecto se considera no implementado
    ram[i].bitname[0] := '';
    ram[i].bitname[1] := '';
    ram[i].bitname[2] := '';
    ram[i].bitname[3] := '';
    ram[i].bitname[4] := '';
    ram[i].bitname[5] := '';
    ram[i].bitname[6] := '';
    ram[i].bitname[7] := '';
  end;
end;
procedure TPIC16.SetSharedUnused;
{Marca las posiciones que estén en "shared", como no usadas, para que se puedan
usar nuevamente.}
var
  i: Integer;
  amask: Byte;
begin
  for i:=0 to high(ram) do begin
    if (ram[i].state = cs_impleGPR) and (ram[i].shared <> 0) then begin
//debugln('    >> used $'+IntToHEx(i,3)+':'+ram[i].name);
      amask := not ram[i].shared;   //máscara invertida
      ram[i].used := ram[i].used and amask;  //pone en cero los bits shared
    end;
  end;
end;
procedure TPIC16.SetSharedUsed;
{Marca las posiciones que estén en "shared", como usadas, para que no se puedan
usar nuevamente.}
var
  i: Integer;
  amask: Byte;
begin
  for i:=0 to high(ram) do begin
    if (ram[i].state = cs_impleGPR) and (ram[i].shared <> 0) then begin
//debugln('    >> used $'+IntToHEx(i,3)+':'+ram[i].name);
      amask := ram[i].shared;   //máscara
      ram[i].used := ram[i].used or amask;  //pone en uno los bits shared
    end;
  end;
end;
procedure TPIC16.DisableAllRAM;
{Inicia el estado de toda la memoria RAM física definida em el Modelo.
Solo debería usarse, para cuando se va a definir el hardware del dispositivo.}
var
  i: word;
begin
  for i:=0 to high(ram) do begin
    ram[i].addr     := i;
    ram[i].state    := cs_unimplem;
    ram[i].mappedTo := nil;
    ram[i].Fimplem  := $FF;  //Todos implementados, por defecto
  end;
  //Inicia estado de pines
  for i:=1 to high(pines) do begin
    pines[i].typ := pptUnused;
  end;
end;
procedure TPIC16.SetStatRAM(i1, i2: word; status0: TPIC16CellState);
{Inicia el campo State, de la memoria. Permite definir el estado real de la memoria RAM.
}
var
  i: Integer;
begin
  for i:=i1 to i2 do begin  //verifica 1 a 1, por seguridad
    ram[i].state := status0;
  end;
end;
procedure TPIC16.SetMappRAM(i1, i2: word; MappedTo: word);
{Inicia el campo State, de la memoria. Permite definir el estado real de la memoria RAM.
"MappedTo", indica el número de banco al cual está mapeada la sección de memoria indicada,
cuando se pone "status0" en "cs_mapToBnk". En los otrso estados no es útil.}
var
  i: Integer;
begin
  for i:= i1 to i2 do begin  //verifica 1 a 1, por seguridad
    ram[i].mappedTo := @ram[MappedTo];
    inc(MappedTo)
  end;
end;
function TPIC16.SetStatRAMCom(strDef: string): boolean;
{Define el estado de la memoria RAM, usando una cadena de definición.
La cadena de definición, tiene el formato:
<comando 1>, <comando 2>, ...
Cada comando, tiene el formato:
<dirIni>-<dirFin>:<estado de memoria>
Un ejemplo de cadena de definición, es:
   '000-01F:SFR, 020-07F:GPR'
Si hay error, devuelve FALSE, y el mensaje de error en MsjError.
}
var
  coms: TStringList;
  add1, add2: longint;
  state: TPIC16CellState;
  staMem, com, str: String;
begin
  Result := true;
  coms:= TStringList.Create;
  try
    coms.Delimiter := ',';
    coms.DelimitedText := strDef;
    for str in coms do begin
      com := UpCase(trim(str));
      if com='' then continue;
      if length(com)<>11 then begin
        MsjError := 'Memory definition syntax error: Bad string size.';
        exit(false);
      end;
      if com[4] <> '-' then begin
        MsjError := 'Memory definition syntax error: Expected "-".';
        exit(false);
      end;
      if com[8] <> ':' then begin
        MsjError := 'Memory definition syntax error: Expected ":".';
        exit(false);
      end;
      //Debe tener el formato pedido
      if not TryStrToInt('$'+copy(com,1,3), add1) then begin
        MsjError := 'Memory definition syntax error: Wrong address.';
        exit(false);
      end;
      if not TryStrToInt('$'+copy(com,5,3), add2) then begin
        MsjError := 'Memory definition syntax error: Wrong address.';
        exit(false);
      end;
      staMem := copy(com, 9, 3);
      case staMem of
      'SFR': state := cs_impleSFR;
      'GPR': state := cs_impleGPR;
      'NIM': state := cs_unimplem;
      else
        MsjError := 'Memory definition syntax error: Expected SFR or GPR';
        exit(false);
      end;
      //Ya se tienen los parámetros, para definir la memoria
      SetStatRAM(add1, add2, state);
    end;
  finally
    coms.Destroy;
  end;
end;
function TPIC16.SetMappRAMCom(strDef: string): boolean;
{Define memoria RAM mapeeada, en otra dirección.
La cadena de definición, tiene el formato:
<comando 1>, <comando 2>, ...
Cada comando, tiene el formato:
<dirIni>-<dirFin>:<banco al que está mapeado>
Un ejemplo de cadena de definición, es:
   '000-01F:bnk0, 020-07F:bnk1'
Si hay error, devuelve FALSE, y el mensaje de error en MsjError.
}
var
  coms: TStringList;
  add1, add2, addTar: longint;
  bnkTarStr, com, str: String;
  bnkTar: byte;
begin
  Result := true;
  coms:= TStringList.Create;
  try
    coms.Delimiter := ',';
    coms.DelimitedText := strDef;
    for str in coms do begin
      com := UpCase(trim(str));
      if com='' then continue;
      if length(com)<>12 then begin
        MsjError := 'Memory mapping syntax error: Bad string size.';
        exit(false);
      end;
      if com[4] <> '-' then begin
        MsjError := 'Memory mapping syntax error: Expected "-".';
        exit(false);
      end;
      if com[8] <> ':' then begin
        MsjError := 'Memory mapping syntax error: Expected ":".';
        exit(false);
      end;
      //Debe tener el formato pedido
//      debugln(com);
      if not TryStrToInt('$'+copy(com,1,3), add1) then begin
        MsjError := 'Memory mapping syntax error: Wrong address.';
        exit(false);
      end;
      if not TryStrToInt('$'+copy(com,5,3), add2) then begin
        MsjError := 'Memory mapping syntax error: Wrong address.';
        exit(false);
      end;
      bnkTarStr := copy(com, 9, 4);
      if copy(bnkTarStr,1,3)<>'BNK' then begin
        MsjError := 'Memory mapping syntax error: Expected "bnk0", ...';
        exit(false);
      end;
      if not (bnkTarStr[4] in ['0'..'3']) then begin
        MsjError := 'Memory mapping syntax error: Expected "bnk0", ...';
        exit(false);
      end;
      bnkTar := ord(bnkTarStr[4])-48;  //convierte a número
      //Ya se tienen los parámetros, para definir el mapeo
      case bnkTar of
      0: addTar := (add1 and $7F);
      1: addTar := (add1 and $7F) or $080;
      2: addTar := (add1 and $7F) or $100;
      3: addTar := (add1 and $7F) or $180;
      end;
      SetMappRAM(add1, add2, addTar);
    end;
  finally
    coms.Destroy;
  end;
end;
function TPIC16.MapRAMtoPIN(strDef: string): boolean;
{Mapea puertos de memoria RAM a pines físicos del dispositivo. Útil para la simulación
La cadena de definición, tiene el formato:
<dirección>:<comando 1>, <comando 2>, ...
Cada comando, tiene el formato:
<dirIni>-<dirFin>:<banco al que está mapeado>
Un ejemplo de cadena de definición, es:
   '005:0-17,1-18,2-1,3-2,4-3'
Si hay error, devuelve FALSE, y el mensaje de error en MsjError.
}
var
  coms: TStringList;
  add1, pin, bit: longint;
  com, str, ramName: String;
  pSep: SizeInt;
begin
  Result := true;
  //Obtiene dirección
  if length(strDef) < 4 then begin
    MsjError := 'Syntax error';
    exit(false);
  end;
  if strDef[4] <> ':' then begin
    MsjError := 'Expected "<3-digits address>"';
    exit(false);
  end;
  if not TryStrToInt('$'+copy(strDef,1,3), add1) then begin
    MsjError := 'Address format error.';
    exit(false);
  end;
  delete(strDef, 1, 4);  //quita la dirección
  //Obtiene lista de asociaciones
  coms:= TStringList.Create;
  try
    coms.Delimiter := ',';
    coms.DelimitedText := strDef;
    for str in coms do begin
      com := UpCase(trim(str));  //asociación
      if com='' then continue;
      pSep := pos('-',com);   //Posición de separador
      if pSep = 0 then begin
        MsjError := 'Expected "-".';
        exit(false);
      end;
      //Debe tener el formato pedido
//      debugln(com);
      if not TryStrToInt(copy(com,1,pSep-1), bit) then begin
        MsjError := 'Error in bit number.';
        exit(false);
      end;
      if not TryStrToInt(copy(com,pSep+1,length(com)), pin) then begin
        MsjError := 'Error in pin number.';
        exit(false);
      end;
      if (pin<0) or (pin>PIC_MAX_PINES) then begin
        MsjError := 'Pin number out of range.';
        exit(false);
      end;
      if pin>Npins then begin
        MsjError := 'Pin number out of range, for this device.';
        exit(false);
      end;
      //Ya se tiene el BIT y el PIN. Configura datos del PIN
      pines[pin].add := add1;
      pines[pin].bit := bit;
      pines[pin].typ := pptPort;
      ramName := ram[add1].name;
      if ramName='' then ramName := 'PORT';
      pines[pin].nam :=  ramName + '.' + IntToStr(bit);  //Nombre pro defecto
    end;
  finally
    coms.Destroy;
  end;
end;
procedure TPIC16.SetPin(pNumber: integer; pLabel: string; pType: TPICpinType);
begin
  if pNumber>PIC_MAX_PINES then exit;
  pines[pNumber].nam := pLabel;
  pines[pNumber].typ := pType;
end;
function TPIC16.SetUnimpBITS(strDef: string): boolean;
{Fija bits no implementados en posciones de memoria RAM.}
var
  coms: TStringList;
  add1, n: longint;
  mskBits, com, str: String;
  mskBitsN: byte;
begin
  Result := true;
  coms:= TStringList.Create;
  try
    coms.Delimiter := ',';
    coms.DelimitedText := strDef;
    for str in coms do begin
      com := UpCase(trim(str));
      if com='' then continue;
      if length(com)<>6 then begin
        MsjError := 'Syntax error: Expected "$$$:$$".';
        exit(false);
      end;
      if com[4] <> ':' then begin
        MsjError := 'Syntax error: Expected ":".';
        exit(false);
      end;
      //Debe tener el formato pedido
//      debugln(com);
      if not TryStrToInt('$'+copy(com,1,3), add1) then begin
        MsjError := 'Syntax error: Wrong address.';
        exit(false);
      end;
      if add1>high(ram) then begin
        MsjError := 'Syntax error: Wrong address.';
        exit(false);
      end;
      mskBits := copy(com, 5, 2);
      if not TryStrToInt('$'+mskBits, n) then begin
        MsjError := 'Syntax error: Wrong mask.';
        exit(false);
      end;
      mskBitsN := n;  //Se supone que nunca será > 255
      //Ya se tienen los parámetros, para definir el mapeo
      ram[add1].Fimplem := mskBitsN;
    end;
  finally
    coms.Destroy;
  end;
end;
function TPIC16.BankToAbsRAM(const offset, bank: byte): word;
{Convierte una dirección y banco a una dirección absoluta}
begin
  Result := 0;
  case bank of
  0: Result := offset;
  1: Result := $80 +offset;
  2: Result := $100+offset;
  3: Result := $180+offset;
  end;
end;
procedure TPIC16.AbsToBankRAM(const AbsAddr: word; var offset, bank: byte);
{Convierte dirección absoluta a dirección en bancos}
begin
   offset := AbsAddr and %01111111;
   bank :=  AbsAddr >> 7;
end;
function TPIC16.NameRAM(const addr: word; const bnk: byte): string;
{Devuelve el nombre de una celda de la memoria RAM.}
begin
  Result := ram[BankToAbsRAM(addr, bnk)].name;
end;
function TPIC16.NameRAMbit(const addr: word; const bnk, bit: byte): string;
begin
  Result := ram[BankToAbsRAM(addr, bnk)].bitname[bit];
end;
procedure TPIC16.SetNameRAM(const addr: word; const bnk: byte; const nam: string
  );
{Escribe en el campo "name" de la RAM en la psoición indicada}
begin
   ram[BankToAbsRAM(addr, bnk)].name:=nam;
end;
procedure TPIC16.AddNameRAM(const addr: word; const bnk: byte; const nam: string
  );
{Escribe en el campo "name" de la RAM en la psoición indicada. Si ya existía un nombre,
lo argega después de una coma.}
begin
  if ram[BankToAbsRAM(addr, bnk)].name = '' then begin
    ram[BankToAbsRAM(addr, bnk)].name:=nam;
  end else begin
    ram[BankToAbsRAM(addr, bnk)].name+=','+nam;
  end;
end;
procedure TPIC16.SetNameRAMbit(const addr: word; const bnk, bit: byte;
  const nam: string);
begin
  if (bit>7) then exit;
  ram[BankToAbsRAM(addr, bnk)].bitname[bit] := nam;
end;
//Funciones para la memoria Flash
function TPIC16.UsedMemFlash: word;
var
  i: Integer;
begin
  Result := 0;
  for i:=$0000 to PIC_MAX_FLASH-1 do begin
    if flash[i].used then inc(Result);
  end;
end;
procedure TPIC16.ClearMemFlash;
var
  i: Integer;
begin
  for i:=0 to high(flash) do begin
    flash[i].value    := $3FFF;
    flash[i].used     := false;
    flash[i].curBnk   := 255;  //Desconocido
    flash[i].breakPnt := false;
    flash[i].topLabel   := '';
    flash[i].sideComment:= '';
    flash[i].topComment := '';
    flash[i].idFile   := -1;  //Indica no inicializado
  end;
end;
procedure TPIC16.GenHex(hexFile: string; ConfigWord: integer = -1);
{Genera el archivo *.hex, a partir de los datos almacenados en la memoria
FLASH.
Actualiza los campos, minUsed y maxUsed.}
var
  cfg, tmp: String;
  iHex: word;  //Índice para explorar
  dat: String; //Cadena de dígitos hexadecimales
  addr: word;  //Dirección de inicio
const
  MAX_INS_HEX = 8;  //Número máximo de instrucciones que devuelve por pasada

  function ExtractHex(out Addre: word): string;
  {Devuelve una cadena (de longitud que varía desde 0, hasta MAX_INS_HEX*4 caracteres)
  con valores en hexadecimal de isntrucciones, consecutivas usadas, en le memoria FLASH.
  La lectura se hace a partir de iHex, y los caracteres en hexadecimal se escriben en 4
  dígitos, en la misma forma que se usan para los archivos *.HEX.
  En "Addr" devuelve la dirección absoluta de inicio desde donde lee. Con cada llamada,
  devuelve los bloques consecutivos de datos. Si no hay más datos devuelve cadena vacía.}
  var p1, p2: word;
      cont, p: word;
      tmp: String;
  begin
    Result := '';
    //Busca inicio de instrucciones usadas, desde la posición iHex
    while (iHex<PIC_MAX_FLASH) and not flash[iHex].used  do begin
      inc(iHex);
    end;
    if iHex>=PIC_MAX_FLASH then begin
      //Llegó al final
      exit;  //sale con cadena nula
    end;
    //Ya encontró el inicio ahora busca celdas consecutivas
    p1 := iHex;
    Addre := p1;
    cont := 2;  //inicia contador
    inc(iHex);  //pasa al siguiente
    while (iHex<PIC_MAX_FLASH) and (cont<MAX_INS_HEX) and flash[iHex].used do begin
      inc(iHex);
      inc(cont);
    end;
    if iHex>=PIC_MAX_FLASH then begin
      //Salió porque Llegó al final
      p2 := PIC_MAX_FLASH-1;
    end else if cont>=MAX_INS_HEX then begin
      //Salió porque llegó al máximo de celdas
      if flash[iHex].used then begin
        //La ultima celda estaba ocupada
        p2 := iHex;
        inc(iHex);   //deja listo para la siguiente exploración
      end else begin
        //La ultima celda estaba ocupada
        p2 := iHex-1;
        //iHex, queda apuntando a la siguiente celda
      end;
    end else begin
      //Salió porque encontró celda sin usar
      p2 := iHex-1;
      //iHex, queda apuntando a la siguiente celda
    end;
    //Ya tiene las dos posiciones
    for p:=p1 to p2 do begin
      if p1<minUsed then minUsed := p1;   //Actualiza
      if p2>maxUsed then maxUsed := p2;   //Actualiza
      tmp := IntToHex(flash[p].value, 4);
      Result +=copy(tmp,3,2) + copy(tmp,1,2);  //se graba con los bytes invertidos
    end;
  end;

begin
  hexLines.Clear;      //Se usará la lista hexLines
  GenHexExAdd($0000);
  //Prepara extracción de datos
  minUsed := PIC_MAX_FLASH;
  maxUsed := 0;
  iHex := 0;
  //Inicia la extracción de código
  dat := ExtractHex(addr);
  while dat <>'' do begin
     GenHexData(addr, dat);
     dat := ExtractHex(addr);
  end;
  //Bits de configuración
  if ConfigWord<>-1 then begin
    //Se pide generar bits de configuración
    {Los bits de configuración para la serie 16F, se almacenan en:
Config: 0x2007 (0x400E in the HEX file)
EEPROM: 0x2100 (0x4200 in the HEX file) }
    cfg := IntToHex(ConfigWord and $FFFF, 4);
    tmp +=copy(cfg,3,2) + copy(cfg,1,2);  //se graba con los bytes invertidos
    GenHexData($2007, tmp);
  end;
  GenHexEOF;                    //Fin de archivo
  GenHexComm(self.Model);       //Comentario
  hexLines.SaveToFile(hexFile); //Genera archivo
end;
procedure TPIC16.DumpCode(lOut: TStrings; incAdrr, incCom, incVarNam: boolean);
{Desensambla las instrucciones grabadas en el PIC.
 Se debe llamar despues de llamar a GenHex(), para que se actualicen las variables}
var
  val, i: Word;
  lblLin, comLat, comLin, lin: String;
begin
  //Se supone que minUsed y maxUsed, ya deben haber sido actualizados.
  for i := minUsed to maxUsed do begin
    //Lee comentarios y etiqueta
    lblLin := flash[i].topLabel;
    comLat := flash[i].sideComment;
    comLin := flash[i].topComment;
    //Escribe etiqueta al inicio de línea
    if lblLin<>'' then lOut.Add(lblLin+':');
    //Escribe comentario al inicio de línea
    if incCom and (comLin<>'') then  begin
      lOut.Add(comLin);
    end;
    //Decodifica instrucción
    val := flash[i].value;
    //Escribe línea
    lin := Disassembler(val, incVarNam);  //Instrucción
    //Verificas si incluye dirección física
    if incAdrr then  begin
      lin := '0x'+IntToHex(i,3) + ' ' + lin;
    end;
    //Verifica si incluye comentario lateral
    if incCom then begin
      lin := lin  + ' ' + comLat;
    end;
    lOut.Add('    ' + lin);
  end;
end;
constructor TPIC16.Create;
begin
  inherited Create;
  hexLines := TStringList.Create;
  //configuración de hardware por defecto
  frequen := 4000000;    //4MHz
  NumBanks:=2;     //Número de bancos de RAM. Por defecto se asume 2
  NumPages:=1;     //Número de páginas de memoria Flash. Por defecto 1
  MaxFlash := PIC_PAGE_SIZE;  //En algunos casos, puede ser menor al tamaño de una página
  bank0.Init(0, $000, @ram);
  bank1.Init(1, $080, @ram);
  bank2.Init(2, $100, @ram);
  bank3.Init(3, $180, @ram);
  //inicia una configuración común
  ClearMemRAM;
  SetStatRAM($020, $04F, cs_impleGPR);

  page0.Init($0000          , @flash);
  page1.Init(1*PIC_PAGE_SIZE, @flash);
  page2.Init(2*PIC_PAGE_SIZE, @flash);
  page3.Init(3*PIC_PAGE_SIZE, @flash);

  //estado inicial
  iFlash := 0;   //posición de inicio
  ClearMemFlash;
end;
destructor TPIC16.Destroy;
begin
  hexLines.Destroy;
  inherited Destroy;
end;

procedure InitTables;
begin
  //Inicializa Mnemónico de instrucciones
  PIC16InstName[ADDWF ] := 'ADDWF';
  PIC16InstName[ANDWF ] := 'ANDWF';
  PIC16InstName[CLRF  ] := 'CLRF';
  PIC16InstName[CLRW  ] := 'CLRW';
  PIC16InstName[COMF  ] := 'COMF';
  PIC16InstName[DECF  ] := 'DECF';
  PIC16InstName[DECFSZ] := 'DECFSZ';
  PIC16InstName[INCF  ] := 'INCF';
  PIC16InstName[INCFSZ] := 'INCFSZ';
  PIC16InstName[IORWF ] := 'IORWF';
  PIC16InstName[MOVF  ] := 'MOVF';
  PIC16InstName[MOVWF ] := 'MOVWF';
  PIC16InstName[NOP   ] := 'NOP';
  PIC16InstName[RLF   ] := 'RLF';
  PIC16InstName[RRF   ] := 'RRF';
  PIC16InstName[SUBWF ] := 'SUBWF';
  PIC16InstName[SWAPF ] := 'SWAPF';
  PIC16InstName[XORWF ] := 'XORWF';
  PIC16InstName[BCF   ] := 'BCF';
  PIC16InstName[BSF   ] := 'BSF';
  PIC16InstName[BTFSC ] := 'BTFSC';
  PIC16InstName[BTFSS ] := 'BTFSS';
  PIC16InstName[ADDLW ] := 'ADDLW';
  PIC16InstName[ANDLW ] := 'ANDLW';
  PIC16InstName[CALL  ] := 'CALL';
  PIC16InstName[CLRWDT] := 'CLRWDT';
  PIC16InstName[GOTO_ ] := 'GOTO';
  PIC16InstName[IORLW ] := 'IORLW';
  PIC16InstName[MOVLW ] := 'MOVLW';
  PIC16InstName[RETFIE] := 'RETFIE';
  PIC16InstName[RETLW ] := 'RETLW';
  PIC16InstName[RETURN] := 'RETURN';
  PIC16InstName[SLEEP ] := 'SLEEP';
  PIC16InstName[SUBLW ] := 'SUBLW';
  PIC16InstName[XORLW ] := 'XORLW';
  PIC16InstName[_Inval] := '<Inval>';

  //Inicializa Sintaxis de las instrucciones
  {Los valorees para la sintaxis significan:
  f->dirección de un registro en RAM (0..127)
  d->destino (W o F)
  b->número de bit (0..7)
  a->dirección destino (0..$7FF)
  k->literal byte (0..255)
  }
  PIC16InstSyntax[ADDWF ] := 'fd';
  PIC16InstSyntax[ANDWF ] := 'fd';
  PIC16InstSyntax[CLRF  ] := 'f';
  PIC16InstSyntax[CLRW  ] := '';
  PIC16InstSyntax[COMF  ] := 'fd';
  PIC16InstSyntax[DECF  ] := 'fd';
  PIC16InstSyntax[DECFSZ] := 'fd';
  PIC16InstSyntax[INCF  ] := 'fd';
  PIC16InstSyntax[INCFSZ] := 'fd';
  PIC16InstSyntax[IORWF ] := 'fd';
  PIC16InstSyntax[MOVF  ] := 'fd';
  PIC16InstSyntax[MOVWF ] := 'f';
  PIC16InstSyntax[NOP   ] := '';
  PIC16InstSyntax[RLF   ] := 'fd';
  PIC16InstSyntax[RRF   ] := 'fd';
  PIC16InstSyntax[SUBWF ] := 'fd';
  PIC16InstSyntax[SWAPF ] := 'fd';
  PIC16InstSyntax[XORWF ] := 'fd';
  PIC16InstSyntax[BCF   ] := 'fb';
  PIC16InstSyntax[BSF   ] := 'fb';
  PIC16InstSyntax[BTFSC ] := 'fb';
  PIC16InstSyntax[BTFSS ] := 'fb';
  PIC16InstSyntax[ADDLW ] := 'k';
  PIC16InstSyntax[ANDLW ] := 'k';
  PIC16InstSyntax[CALL  ] := 'a';
  PIC16InstSyntax[CLRWDT] := '';
  PIC16InstSyntax[GOTO_ ] := 'a';
  PIC16InstSyntax[IORLW ] := 'k';
  PIC16InstSyntax[MOVLW ] := 'k';
  PIC16InstSyntax[RETFIE] := '';
  PIC16InstSyntax[RETLW ] := 'k';
  PIC16InstSyntax[RETURN] := '';
  PIC16InstSyntax[SLEEP ] := '';
  PIC16InstSyntax[SUBLW ] := 'k';
  PIC16InstSyntax[XORLW ] := 'k';
  PIC16InstSyntax[_Inval] := '<???>';
end;
initialization
  InitTables;
end.

