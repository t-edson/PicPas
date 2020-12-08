{
Description
===========
Utilities for programming Mid-range PIC microcontrollers with 14 bits instructions.
Include most of the PIC16 devices.
This unit works with 2K words pages and 128 bytes RAM banks.
The main class TPIC16 must model all devices of this family, including the most complex.
The aim of this unit is to be used as base for assemblers, compilers and simulators.

                                         Created by Tito Hinostroza   26/07/2015
}

unit Pic16Utils;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LCLProc, PicCore;
type  //Mid-range PIC instructions
  TPIC16Inst = (
    //BYTE-ORIENTED FILE REGISTER OPERATIONS
    i_ADDWF,
    i_ANDWF,
    i_CLRF,
    i_CLRW,
    i_COMF ,
    i_DECF ,
    i_DECFSZ,
    i_INCF,
    i_INCFSZ,
    i_IORWF,
    i_MOVF,
    i_MOVWF,
    i_NOP,
    i_RLF,
    i_RRF,
    i_SUBWF,
    i_SWAPF,
    i_XORWF,
    //BIT-ORIENTED FILE REGISTER OPERATIONS
    i_BCF,
    i_BSF,
    i_BTFSC,
    i_BTFSS,
    //LITERAL AND CONTROL OPERATIONS
    i_ADDLW,
    i_ANDLW,
    i_CALL,
    i_CLRWDT,
    i_GOTO,
    i_IORLW,
    i_MOVLW,
    i_RETFIE,
    i_RETLW,
    i_RETURN,
    i_SLEEP,
    i_SUBLW,
    i_XORLW,
    //INVALID INSTRUCTION
    i_Inval
  );
  //Indica el destino de la instrucción
  TPIC16destin = (
    toW = %00000000,    //al acumulador
    toF = %10000000     //a memoria
  );



const  //Constants of address and bit positions for some registers
  _STATUS = $03;
  _C      = 0;
  _Z      = 2;
  _RP0    = 5;
  _RP1    = 6;
//  _IRP   = 7;
type
  {Objeto que representa al hardware de un PIC de la serie 16}
  { TPIC16 }
  TPIC16 = class(TPicCore)
  public  //Campos para procesar instrucciones
    idIns: TPIC16Inst;    //ID de Instrucción.
    d_   : TPIC16destin;  //Destino de operación. Válido solo en algunas instrucciones.
    f_   : byte;          //Registro destino. Válido solo en algunas instrucciones.
    b_   : byte;          //Bit destino. Válido solo en algunas instrucciones.
    k_   : word;          //Parámetro Literal. Válido solo en algunas instrucciones.
  private //Campos para procesar instrucciones
    function GetBank(i : Longint): TPICRAMBank;
    function GetINTCON: byte;
    function GetINTCON_GIE: boolean;
    function GetPage(i : Longint): TPICFlashPage;
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
    procedure SetFRAM(value: byte);
    function GetFRAM: byte;
  public  //Campos que modelan a los registros internos
    W        : byte;   //Registro de trabajo
    PC       : TWordRec; //PC as record to fast access for bytes
    PCLATH   : byte;   //Contador de Programa H
    STKPTR   : 0..7;   //Puntero de pila
    STACK    : array[0..7] of word;
    property STATUS: byte read GetSTATUS;
    property STATUS_Z: boolean read GetSTATUS_Z write SetSTATUS_Z;
    property STATUS_C: boolean read GetSTATUS_C write SetSTATUS_C;
    property STATUS_DC: boolean read GetSTATUS_DC write SetSTATUS_DC;
    property STATUS_IRP: boolean read GetSTATUS_IRP write SetSTATUS_IRP;
    property INTCON: byte read GetINTCON;
    property INTCON_GIE: boolean read GetINTCON_GIE write SetINTCON_GIE;
    property FRAM: byte read GetFRAM write SetFRAM;
  public  //Execution control
    function CurInstruction: TPIC16Inst;
    procedure Exec(aPC: word); override; //Ejecuta la instrucción en la dirección indicada.
    procedure Exec; override; //Ejecuta instrucción actual
    procedure ExecTo(endAdd: word); override; //Ejecuta hasta cierta dirección
    procedure ExecStep; override; //Execute one instruction considering CALL as one instruction
    procedure ExecNCycles(nCyc: integer; out stopped: boolean); override; //Ejecuta hasta cierta dirección
    procedure Reset; override;
    function ReadPC: dword; override;
    procedure WritePC(AValue: dword); override;
  public  //Memories
    procedure Decode(const opCode: word);  //decodifica instrucción
    function Disassembler(const opCode: word; bankNum: byte = 255;
      useVarName: boolean = false): string;  //Desensambla la instrucción actual
    function DisassemblerAt(addr: word; useVarName: boolean = false): string; override;
    property banks[i : Longint]: TPICRAMBank Read GetBank;
    property pages[i : Longint]: TPICFlashPage Read GetPage;
  public  //RAM memory functions
    function GetFreeBit(out addr: word; out bit: byte; shared: boolean): boolean;
    function GetFreeByte(out addr: word; shared: boolean): boolean;
    function GetFreeBytes(const size: integer; var addr: word): boolean;  //obtiene una dirección libre
    function TotalMemRAM: word; //devuelve el total de memoria RAM
    function UsedMemRAM: word;  //devuelve el total de memoria RAM usada
    procedure ExploreUsed(rutExplorRAM: TPICRutExplorRAM);    //devuelve un reporte del uso de la RAM
    function ValidRAMaddr(addr: word): boolean;  //indica si una posición de memoria es válida
    function BankToAbsRAM(const offset, bank: byte): word; //devuelve dirección absoluta
    procedure AbsToBankRAM(const AbsAddr: word; var offset, bank: byte); //convierte dirección absoluta
  public  //Métodos para codificar instrucciones de acuerdo a la sintaxis
    procedure useFlash;
    procedure codAsmFD(const inst: TPIC16Inst; const f: word; d: TPIC16destin);
    procedure codAsmF(const inst: TPIC16Inst; const f: word);
    procedure codAsmFB(const inst: TPIC16Inst; const f: word; b: byte);
    procedure codAsmK(const inst: TPIC16Inst; const k: byte);
    procedure codAsmA(const inst: TPIC16Inst; const a: word);
    procedure codAsm(const inst: TPIC16Inst);
    procedure codGotoAt(iflash0: integer; const k: word);
    procedure codCallAt(iflash0: integer; const k: word);
    function codInsert(iflash0, nInsert, nWords: integer): boolean;
    procedure BTFSC_sw_BTFSS(iflash0: integer);
  public  //Métodos adicionales
    function FindOpcode(Op: string; out syntax: string): TPIC16Inst;  //busca Opcode
    procedure GenHex(hexFile: string; ConfigWord: integer = - 1);  //genera un archivo hex
    procedure DumpCode(lOut: TStrings; incAdrr, incCom, incVarNam: boolean);  //vuelva en código que contiene
  public  //Initialization
    constructor Create; override;
    destructor Destroy; override;
  end;

var  //variables globales
  //mnemónico de las instrucciones
  PIC16InstName: array[low(TPIC16Inst)..high(TPIC16Inst)] of string[7];
  //sintaxis en ensamblador de las instrucciones
  PIC16InstSyntax: array[low(TPIC16Inst)..high(TPIC16Inst)] of string[5];

implementation

{ TPIC16 }
procedure TPIC16.useFlash;
{Marca la posición actual, como usada, e incrementa el puntero iFlash. S ihay error,
actualiza el campo "MsjError"}
begin
  //Protección de desborde
  if iFlash >= MaxFlash then begin
    MsjError := 'FLASH Memory limit exceeded.';
    exit;
  end;
  flash[iFlash].used := true;  //marca como usado
  inc(iFlash);
end;
procedure TPIC16.codAsmFD(const inst: TPIC16Inst; const f: word; d: TPIC16destin);
{Codifica las instrucciones orientadas a registro, con sinatxis: NEMÓNICO f,d}
begin
  case inst of
  i_ADDWF : flash[iFlash].value := %00011100000000 + ord(d) + (f and %1111111);
  i_ANDWF : flash[iFlash].value := %00010100000000 + ord(d) + (f and %1111111);
  i_COMF  : flash[iFlash].value := %00100100000000 + ord(d) + (f and %1111111);
  i_DECF  : flash[iFlash].value := %00001100000000 + ord(d) + (f and %1111111);
  i_DECFSZ: flash[iFlash].value := %00101100000000 + ord(d) + (f and %1111111);
  i_INCF  : flash[iFlash].value := %00101000000000 + ord(d) + (f and %1111111);
  i_INCFSZ: flash[iFlash].value := %00111100000000 + ord(d) + (f and %1111111);
  i_IORWF : flash[iFlash].value := %00010000000000 + ord(d) + (f and %1111111);
  i_MOVF  : flash[iFlash].value := %00100000000000 + ord(d) + (f and %1111111);
  i_RLF   : flash[iFlash].value := %00110100000000 + ord(d) + (f and %1111111);
  i_RRF   : flash[iFlash].value := %00110000000000 + ord(d) + (f and %1111111);
  i_SUBWF : flash[iFlash].value := %00001000000000 + ord(d) + (f and %1111111);
  i_SWAPF : flash[iFlash].value := %00111000000000 + ord(d) + (f and %1111111);
  i_XORWF : flash[iFlash].value := %00011000000000 + ord(d) + (f and %1111111);
  else
    raise Exception.Create('Implementation Error.');
  end;
  useFlash;  //marca como usado e incrementa puntero.
end;
procedure TPIC16.codAsmF(const inst: TPIC16Inst; const f: word);
{Codifica las instrucciones orientadas a registro, con sinatxis: NEMÓNICO f}
begin
  case inst of
  i_CLRF  : flash[iFlash].value := %00000110000000 + (f and %1111111);
  i_MOVWF : flash[iFlash].value := %00000010000000 + (f and %1111111);
  else
    raise Exception.Create('Implementation Error.');
  end;
  useFlash;  //marca como usado e incrementa puntero.
end;
procedure TPIC16.codAsmFB(const inst: TPIC16Inst; const f: word; b: byte);
//Codifica las instrucciones orientadas a bit.
begin
  case inst of
  i_BCF  : flash[iFlash].value := %01000000000000 + word(b<<7) + (f and %1111111);
  i_BSF  : flash[iFlash].value := %01010000000000 + word(b<<7) + (f and %1111111);
  i_BTFSC: flash[iFlash].value := %01100000000000 + word(b<<7) + (f and %1111111);
  i_BTFSS: flash[iFlash].value := %01110000000000 + word(b<<7) + (f and %1111111);
  else
    raise Exception.Create('Implementation Error.');
  end;
  useFlash;  //marca como usado e incrementa puntero.
end;
procedure TPIC16.codAsmK(const inst: TPIC16Inst; const k: byte);
{Codifica las instrucciones con constantes.}
begin
  case inst of
  i_ADDLW : flash[iFlash].value := %11111000000000 + k;
  i_ANDLW : flash[iFlash].value := %11100100000000 + k;
  i_IORLW : flash[iFlash].value := %11100000000000 + k;
  i_MOVLW : flash[iFlash].value := %11000000000000 + k;
  i_RETLW : flash[iFlash].value := %11010000000000 + k;
  i_SUBLW : flash[iFlash].value := %11110000000000 + k;
  i_XORLW : flash[iFlash].value := %11101000000000 + k;
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
  i_CALL  : flash[iFlash].value := %10000000000000 + (a and %11111111111);
  i_GOTO : flash[iFlash].value := %10100000000000 + (a and %11111111111);
  else
    raise Exception.Create('Implementation Error.');
  end;
  useFlash;  //marca como usado e incrementa puntero.
end;
procedure TPIC16.codAsm(const inst: TPIC16Inst);
//Codifica las instrucciones de control.
begin
  case inst of
  i_CLRW  : flash[iFlash].value := %00000100000000;
  i_NOP   : flash[iFlash].value := %00000000000000;
  i_CLRWDT: flash[iFlash].value := %00000001100100;
  i_RETFIE: flash[iFlash].value := %00000000001001;
  i_RETURN: flash[iFlash].value := %00000000001000;
  i_SLEEP : flash[iFlash].value := %00000001100011;
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
{Codifica una instrucción i_CALL, en una posición específica y sin alterar el puntero "iFlash"
actual. Se usa para completar llamadas indefinidas}
begin
  flash[iFlash0].value := %10000000000000 + (k and %11111111111);
end;
function TPIC16.codInsert(iflash0, nInsert, nWords: integer): boolean;
{Inserta en la posición iflash0, "nInsert" palabras, desplazando "nWords" palabras.
Al final debe quedar "nInsert" palabras de espacio libre en iflash0.
Si hay error devuelve FALSE.}
var
  i: Integer;
begin
  Result := True;  //By default
  if iFlash+nInsert+nWords-1> MaxFlash then begin
    //Overflow on address
    exit(false);
  end;
  for i:= iflash + nInsert + nWords -1 downto iFlash + nWords do begin
    flash[i] := flash[i-nInsert];
  end;
end;
procedure TPIC16.BTFSC_sw_BTFSS(iflash0: integer);
{Exchange instruction i_BTFSC to i_BTFSS, or viceversa, in the specified address.}
begin
  //Solo necesita cambiar el bit apropiado
  flash[iFlash0].value := flash[iFlash0].value XOR %10000000000;
end;
function TPIC16.FindOpcode(Op: string; out syntax: string): TPIC16Inst;
{Busca una cádena que represente a una instrucción (Opcode). Si encuentra devuelve
 el identificador de instrucción y una cadena que representa a la sintaxis en "syntax".
 Si no encuentra devuelve "i_Inval". }
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
    Result := i_Inval;
  end;
end;
//Campos para procesar instrucciones
function TPIC16.GetBank(i : Longint): TPICRAMBank;
begin
  Result.AddrStart := i*PICBANKSIZE;
  Result.AddrEnd   := (i+1)*PICBANKSIZE-1;
end;
function TPIC16.GetPage(i: Longint): TPICFlashPage;
begin
  Result.AddrStart := i*PICPAGESIZE;
  Result.AddrEnd   := (i+1)*PICPAGESIZE-1;
end;
function TPIC16.GetSTATUS: byte;
begin
  Result := ram[_STATUS].dvalue;
end;
function TPIC16.GetSTATUS_Z: boolean;
begin
  Result := (ram[_STATUS].dvalue and %00000100) <> 0;
end;
procedure TPIC16.SetSTATUS_Z(AValue: boolean);
begin
  if AVAlue then ram[_STATUS].dvalue := ram[_STATUS].dvalue or  %00000100
            else ram[_STATUS].dvalue := ram[_STATUS].dvalue and %11111011;
end;
function TPIC16.GetSTATUS_C: boolean;
begin
  Result := (ram[_STATUS].dvalue and %00000001) <> 0;
end;
procedure TPIC16.SetSTATUS_C(AValue: boolean);
begin
  if AVAlue then ram[_STATUS].dvalue := ram[_STATUS].dvalue or  %00000001
            else ram[_STATUS].dvalue := ram[_STATUS].dvalue and %11111110;
end;
function TPIC16.GetSTATUS_DC: boolean;
begin
  Result := (ram[_STATUS].dvalue and %00000010) <> 0;
end;
procedure TPIC16.SetSTATUS_DC(AValue: boolean);
begin
  if AVAlue then ram[_STATUS].dvalue := ram[_STATUS].dvalue or  %00000010
            else ram[_STATUS].dvalue := ram[_STATUS].dvalue and %11111101;
end;
function TPIC16.GetSTATUS_IRP: boolean;
begin
  Result := (ram[_STATUS].dvalue and %10000000) <> 0;
end;
procedure TPIC16.SetSTATUS_IRP(AValue: boolean);
begin
  if AVAlue then ram[_STATUS].dvalue := ram[_STATUS].dvalue or  %10000000
            else ram[_STATUS].dvalue := ram[_STATUS].dvalue and %01111111;
end;
function TPIC16.GetINTCON: byte;
begin
  Result := ram[$0B].dvalue;
end;
function TPIC16.GetINTCON_GIE: boolean;
begin
  Result := (ram[$0B].dvalue and %10000000) <> 0;
end;
procedure TPIC16.SetINTCON_GIE(AValue: boolean);
begin
  if AVAlue then ram[$0B].dvalue := ram[$0B].dvalue or  %10000000
            else ram[$0B].dvalue := ram[$0B].dvalue and %01111111;
end;
procedure TPIC16.SetFRAM(value: byte);
{Escribe en la RAM; en la dirección global f_, el valor "value"
Para determinar el valor real de la dirección, se toma en cuenta los bits de STATUS}
var
  pRAM : TPICRamCellPtr;
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
  {Se escribe aplicando la máscara de bits implementados. Se podría usar la máscara en
  lectura o escritura, pero se prefiere hacerlo en escritura, porque se espera que se
  hagan menos operaciones de escritura que lectura.}
  case STATUS and %01100000 of
  %00000000: pRAM := @ram[f_              ];
  %00100000: pRAM := @ram[f_+PICBANKSIZE  ];
  %01000000: pRAM := @ram[f_+PICBANKSIZE*2];
  %01100000: pRAM := @ram[f_+PICBANKSIZE*3];
  end;
  pRAM^.value := value and pRAM^.implemAnd; // or pRAM^.implemOr; No se ha encontrado casos  que requieran implemOr
  {Se podría optimizar creando una constante en lugar de PICBANKSIZE y evitar así
  la multiplicación. La constante peude ser glocla, algo así como:
  cons PIC_BANK_SIZE = 128 y usar luego esta constante para asiganrla a PICBANKSIZE.}
end;
function TPIC16.GetFRAM: byte;
{Devuelve el valor de la RAM, de la posición global f_.
Para determinar el valor real de la dirección, se toma en cuenta los bits de STATUS}
var
  addr: Byte;
begin
  if f_ = 0 then begin
    //Caso especial de direccionamiento indirecto
    if STATUS_IRP then begin
      Result := ram[ram[04].value + $100].value;
    end else begin
      addr := ram[04].value;
      Result := ram[addr].value and ram[addr].implemAnd;
    end;
    exit;
  end;
  case STATUS and %01100000 of
  %00000000: Result := ram[f_               ].value;
  %00100000: Result := ram[f_+ PICBANKSIZE  ].value;
  %01000000: Result := ram[f_+ PICBANKSIZE*2].value;
  %01100000: Result := ram[f_+ PICBANKSIZE*3].value;
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
    idIns := i_ADDWF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %000101: begin
    idIns := i_ANDWF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %000001: begin
    if (codL and %10000000) = %10000000 then begin
      idIns := i_CLRF;
      f_ := codL and %01111111;
    end else begin
      idIns := i_CLRW;
    end;
  end;
  %001001: begin
    idIns := i_COMF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %000011: begin
    idIns := i_DECF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %001011: begin
    idIns := i_DECFSZ;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %001010: begin
    idIns := i_INCF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %001111: begin
    idIns := i_INCFSZ;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %000100: begin
    idIns := i_IORWF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %001000: begin
    idIns := i_MOVF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %000000: begin
    if (codL and %10000000) = %10000000 then begin
      idIns := i_MOVWF;
      f_ := codL and %01111111;
    end else begin
      //bit7 a cero, hay varias opciones
      case codL of
      %00000000,
      %00100000,
      %01000000,
      %01100000: begin
        idIns := i_NOP;
      end;
      %01100100: begin
        idIns := i_CLRWDT;
      end;
      %00001001: begin
        idIns := i_RETFIE;
      end;
      %00001000: begin
        idIns := i_RETURN;
      end;
      %01100011: begin
        idIns := i_SLEEP;
      end;
      else
        idIns := i_Inval;
      end;
    end;
  end;
  %001101: begin
    idIns := i_RLF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %001100: begin
    idIns := i_RRF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %000010: begin
    idIns := i_SUBWF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %001110: begin
    idIns := i_SWAPF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %000110: begin
    idIns := i_XORWF;
    d_ := TPIC16destin(codL and %10000000);
    f_ := codL and %01111111;
  end;
  %111110,
  %111111: begin
    idIns := i_ADDLW;
    k_ := codL;
  end;
  %111001: begin
    idIns := i_ANDLW;
    k_ := codL;
  end;
  %111000: begin
    idIns := i_IORLW;
    k_ := codL;
  end;
  %110000,
  %110001,
  %110010,
  %110011: begin
    idIns := i_MOVLW;
    k_ := codL;
  end;
  %110100,
  %110101,
  %110110,
  %110111: begin
    idIns := i_RETLW;
    k_ := codL;
  end;
  %111100,
  %111101: begin
    idIns := i_SUBLW;
    k_ := codL;
  end;
  %111010: begin
    idIns := i_XORLW;
    k_ := codL;
  end;
  else
    if (codH and %110000) = %010000 then begin
      case codH and %001100 of
      %0000: begin
        idIns := i_BCF;
        b_ := (opCode and %1110000000) >> 7;
        f_ := codL and %01111111;
      end;
      %0100: begin
        idIns := i_BSF;
        b_ := (opCode and %1110000000) >> 7;
        f_ := codL and %01111111;
      end;
      %1000: begin
        idIns := i_BTFSC;
        b_ := (opCode and %1110000000) >> 7;
        f_ := codL and %01111111;
      end;
      %1100: begin
        idIns := i_BTFSS;
        b_ := (opCode and %1110000000) >> 7;
        f_ := codL and %01111111;
      end;
      else
        idIns := i_Inval;
      end;
    end else if (codH and %111000) = %100000 then begin
      idIns := i_CALL;
      k_ := opCode and %11111111111;
    end else if (codH and %111000) = %101000 then begin
      idIns := i_GOTO;
      k_ := opCode and %11111111111;
    end else begin
      idIns := i_Inval;
    end;
  end;
end;
function TPIC16.Disassembler(const opCode: word; bankNum: byte = 255;
                             useVarName: boolean = false): string;
{Desensambla la instrucción "opCode". Esta rutina utiliza las variables: d_, f_, b_ y k_
"opCode"  -> Código de Operación que se desea decodificar. Se asuem que es de 14 bits.
"bankNum" -> Es el banco de trabajo en el que se supone se está decodificando el OpCode.
             Se usa para determinar la dirección de memoria real a la que se accede
             (cuando el OpCode alccede a memoria). Si no se conoce el valor, se debe
             poner en 255.
"useVarName" -> Indica que se quiere usar etiquetas para los nombres de las variables
             (En los Opcode que accedan a memoria). Solo es válido cuando
             bankNum = 0,1,2,3 y exista un nombre asociado a la variable.
}
var
  nemo: String;
  f: word;
begin
  Decode(opCode);   //Decode instruction. Update: idIns, d_, f_, b_ and k_
  nemo := lowerCase(trim(PIC16InstName[idIns])) + ' ';
  case idIns of
  i_ADDWF,
  i_ANDWF,
  i_COMF ,
  i_DECF ,
  i_DECFSZ,
  i_INCF,
  i_INCFSZ,
  i_IORWF,
  i_MOVF,
  i_RLF,
  i_RRF,
  i_SUBWF,
  i_SWAPF,
  i_XORWF: begin
      if bankNum in [0,1,2,3] then begin
        //Banco conocido
        f := f_ + PICBANKSIZE*bankNum;  //Dirección real
      end else begin
        //Se asume un banco desconocido
        useVarName := false;  //Desactiva por si acaso
        bankNum := 0;  //Trabajará en este banco
        f := f_;       //Dirección asumida
      end;
      if useVarName and (ram[f].name<>'') then begin
        //Required to include address name
        if d_ = toF then
          Result := nemo + ram[f].name + ',f'
        else
          Result := nemo + ram[f].name + ',w';
      end else begin
        //No Required to include address name
        if d_ = toF then
          Result := nemo + '0x'+IntToHex(f,3) + ',f'
        else
          Result := nemo + '0x'+IntToHex(f,3) + ',w';
      end;
     end;
  i_CLRF,
  i_MOVWF: begin
        if bankNum in [0,1,2,3] then begin
          //Banco conocido
          f := f_ + PICBANKSIZE*bankNum;  //Dirección real
        end else begin
          //Se asume un banco desconocido
          useVarName := false;  //Desactiva por si acaso
          bankNum := 0;  //Trabajará en este banco
          f := f_;        //Dirección asumida
        end;
        if useVarName and (ram[f].name<>'') then begin
          Result := nemo + ram[f].name;
        end else begin
          Result := nemo + '0x'+IntToHex(f,3);
        end;
     end;
  i_BCF,
  i_BSF,
  i_BTFSC,
  i_BTFSS: begin    //Instrucciones de bit
      if bankNum in [0,1,2,3] then begin
        //Banco conocido
        f := f_ + PICBANKSIZE*bankNum;  //Dirección real
      end else begin
        //Se asume un banco desconocido
        useVarName := false;  //Desactiva por si acaso
        bankNum := 0;  //Trabajará en este banco
        f := f_;        //Dirección asumida
      end;
      if useVarName and (ram[f].bitname[b_]<>'') then begin
        //Hay nombre de bit
        Result := nemo + ram[f].bitname[b_];
      end else if useVarName and (ram[f].name<>'') then begin
        //Hay nombre de byte
        Result := nemo + ram[f].name + ', ' + IntToStr(b_);
      end else begin
        Result := nemo + '0x'+IntToHex(f,3) + ', ' + IntToStr(b_);
      end;
     end;
  i_ADDLW,
  i_ANDLW,
  i_IORLW,
  i_MOVLW,
  i_RETLW,
  i_SUBLW,
  i_XORLW: begin
       Result := nemo + '0x'+IntToHex(k_,2);
     end;
  i_CALL,
  i_GOTO: begin   //Faltaría decodificar la dirección
    Result := nemo + '0x'+IntToHex(k_,3);
  end;
  i_CLRW,
  i_NOP,
  i_CLRWDT,
  i_RETFIE,
  i_RETURN,
  i_SLEEP: begin
       Result := nemo ;
     end;
  else
    Result := 'Invalid'
  end;
end;
function TPIC16.DisassemblerAt(addr: word; useVarName: boolean): string;
{Disassembler the instruction located at "addr"}
var
  valOp: Word;
  bnkOp: Byte;
begin
  valOp := flash[addr].value;
  bnkOp := flash[addr].curBnk;
  Result := Disassembler(valOp, bnkOp, useVarName);   //desensambla
end;
function TPIC16.CurInstruction: TPIC16Inst;
{Resturn the instruction pointed by PC, in this moment.}
begin
  Decode(flash[PC.W].value);   //decodifica instrucción
  Result := idIns;
end;
procedure TPIC16.Exec;
{Execute the current instruction.}
begin
  Exec(PC.W);
end;
procedure TPIC16.Exec(aPC: word);
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
  //Decode instruction
  opc := flash[aPC].value;
  Decode(opc);   //decodifica instrucción
  case idIns of
  i_ADDWF: begin
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
  i_ANDWF: begin
    resByte := W and FRAM;
    if d_ = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
  end;
  i_CLRF: begin
    FRAM := 0;
    STATUS_Z := true;
  end;
  i_CLRW: begin
    W := 0;
    STATUS_Z := true;
  end;
  i_COMF : begin
    resByte := not FRAM;
    if d_ = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
  end;
  i_DECF : begin
    resByte := FRAM;
    if resByte = 0 then resByte := $FF else dec(resByte);
    if d_ = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
  end;
  i_DECFSZ: begin
    resByte := FRAM;
    if resByte = 0 then resByte := $FF else dec(resByte);
    if d_ = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
    if STATUS_Z then begin
      Inc(PC.W);    //Jump one instrucción
      Inc(nClck);   //In this case it takes one more cicle
    end;
  end;
  i_INCF: begin
    resByte := FRAM;
    if resByte = 255 then resByte := 0 else inc(resByte);
    if d_ = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
  end;
  i_INCFSZ: begin
    resByte := FRAM;
    if resByte = 255 then resByte := 0 else inc(resByte);
    if d_ = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
    if STATUS_Z then begin
      Inc(PC.W);    //Jump one instrucción
      Inc(nClck);   //In this case it takes one more cicle
    end;
  end;
  i_IORWF: begin
    resByte := W or FRAM;
    if d_ = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
  end;
  i_MOVF: begin
    resByte := FRAM;
    if d_ = toF then begin
      //no mueve, solo verifica
      STATUS_Z := (resByte = 0);
    end else begin  //toW
      w := resByte;
      STATUS_Z := (resByte = 0);
    end;
  end;
  i_MOVWF: begin
    FRAM := W;   //escribe a donde esté mapeado, (si está mapeado)
    if f_ = $02 then begin //Es el PCL
      PC.H := PCLATH;  //Cuando se escribe en PCL, se carga PCH con PCLATH
    end;
  end;
  i_NOP: begin
  end;
  i_RLF: begin
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
  i_RRF: begin
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
  i_SUBWF: begin
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
  i_SWAPF: begin
    resByte := FRAM;
    FRAM := (resByte >> 4) or ((resByte << 4) and $FF);
  end;
  i_XORWF: begin
    resByte := W xor FRAM;
    if d_ = toF then begin
      FRAM := resByte;
    end else begin  //toW
      w := resByte;
    end;
    STATUS_Z := resByte = 0;
  end;
  //BIT-ORIENTED FILE REGISTER OPERATIONS
  i_BCF: begin
    msk := $1 << b_;
    msk := not msk;
    FRAM := FRAM and msk;
  end;
  i_BSF: begin
    msk := $1 << b_;
    FRAM := FRAM or msk;// b_
  end;
  i_BTFSC: begin
    msk := $1 << b_;
    if (FRAM and msk) = 0 then begin
      Inc(PC.W);    //Jump one instrucción
      Inc(nClck);   //In this case it takes one more cicle
    end;
  end;
  i_BTFSS: begin
    msk := $1 << b_;
    if (FRAM and msk) <> 0 then begin
      Inc(PC.W);    //Jump one instrucción
      Inc(nClck);   //In this case it takes one more cicle
    end;
  end;
  //LITERAL AND CONTROL OPERATIONS
  i_ADDLW: begin
    resWord := W + k_;
    resNib := (W and $0F) + (k_ and $0F);
    w := resWord and $FF;
    STATUS_Z := (resWord and $ff) = 0;
    STATUS_C := (resWord > 255);
    STATUS_DC := (resNib > 15);
  end;
  i_ANDLW: begin
    resByte := W and K_;
    w := resByte;
    STATUS_Z := resByte = 0;
  end;
  i_CALL: begin
    //Guarda dirección en Pila
    STACK[STKPTR] := PC.W;
    if STKPTR = 7 then begin
      //Desborde de pila
      STKPTR := 0;
      if OnExecutionMsg<>nil then OnExecutionMsg('Stack Overflow on CALL OpCode at $' + IntToHex(aPC,4));
    end else begin
      STKPTR := STKPTR +1;
    end;
    PC.W := k_;  //Takes the 11 bits from k
    PC.H := PC.H or (PCLATH and %00011000);  //And complete with bits 3 and 4 of PCLATH
    Inc(nClck,2);   //This instruction takes two cicles
    exit;
  end;
  i_CLRWDT: begin
  end;
  i_GOTO: begin
    PC.W := k_;  //Takes the 11 bits from k
    PC.H := PC.H or (PCLATH and %00011000);  //And complete with bits 3 and 4 of PCLATH
    Inc(nClck,2);   //This instruction takes two cicles
    exit;
  end;
  i_IORLW: begin
    resByte := W or k_;
    w := resByte;
    STATUS_Z := resByte = 0;
  end;
  i_MOVLW: begin
      W := k_;
  end;
  i_RETFIE: begin
    //Saca dirección en Pila
    if STKPTR = 0 then begin
      //Desborde de pila
      STKPTR := 7;
      if OnExecutionMsg<>nil then OnExecutionMsg('Stack Overflow on RETFIE OpCode at $' + IntToHex(aPC,4));
    end else begin
      STKPTR := STKPTR - 1;
    end;
    PC.W := STACK[STKPTR];  //Should be 13 bits
    Inc(nClck);   //Esta instrucción toma un ciclo más
    //Activa GIE
    INTCON_GIE := true;
  end;
  i_RETLW: begin
    //Saca dirección en Pila
    if STKPTR = 0 then begin
      //Desborde de pila
      STKPTR := 7;
      if OnExecutionMsg<>nil then OnExecutionMsg('Stack Overflow on RETLW OpCode at $' + IntToHex(aPC,4));
    end else begin
      STKPTR := STKPTR - 1;
    end;
    PC.W := STACK[STKPTR];  //Should be 13 bits
    Inc(nClck);   //Esta instrucción toma un ciclo más
    //Fija valor en W
    W := k_;
  end;
  i_RETURN: begin
    //Saca dirección en Pila
    if STKPTR = 0 then begin
      //Desborde de pila
      STKPTR := 7;
      if OnExecutionMsg<>nil then OnExecutionMsg('Stack Overflow on RETURN OpCode at $' + IntToHex(aPC,4));
    end else begin
      STKPTR := STKPTR - 1;
    end;
    PC.W := STACK[STKPTR];  //Should be 13 bits
    Inc(nClck);   //Esta instrucción toma un ciclo más
  end;
  i_SLEEP: begin
  end;
  i_SUBLW: begin
    resInt := k_ - W;
    w := resInt and $FF;
    STATUS_Z := (resInt = 0);
    if resInt < 0 then STATUS_C := false   //negativo
    else STATUS_C := true;
    resInt := (k_ and $0F) - (W and $0F);
    if resInt < 0 then STATUS_DC := false   //negativo
    else STATUS_DC := true;
  end;
  i_XORLW: begin
    resByte := W xor k_;
    w := resByte;
    STATUS_Z := resByte = 0;
  end;
  i_Inval: begin
    MsjError := 'Invalid Opcode';
  end;
  end;
  //Incrementa contador
  Inc(PC.W);
  Inc(nClck);
end;
procedure TPIC16.ExecTo(endAdd: word);
{Ejecuta las instrucciones secuencialmente, desde la instrucción actual, hasta que el
contador del programa, sea igual a la dirección "endAdd".}
begin
  //Hace una primera ejecución, sin verificar Breakpoints
  Exec(PC.W);
  //Ejecuta cíclicamnente
  while PC.W <> endAdd do begin
    if flash[PC.W].breakPnt then begin
      //Encontró un BreakPoint, sale sin ejecutar esa instrucción
      if OnExecutionMsg<>nil then OnExecutionMsg('Stopped for breakpoint.');
      exit;
    end;
    //Ejecuta
    Exec(PC.W);
    //Debe haber una forma de salir si es un lazo infinito
    //if (nClck and $800000) = $800000 then begin
    //end;
  end;
end;
procedure TPIC16.ExecStep;
begin
  if CurInstruction = i_CALL then begin
    ExecTo(PC.W+1);  //Ejecuta hasta la sgte. instrucción, salta el i_CALL
  end else begin
    Exec;
  end;
end;
procedure TPIC16.ExecNCycles(nCyc: integer; out stopped: boolean);
{Ejecuta el número de ciclos indicados, o hasta que se produzca alguna condición
externa, que puede ser:
* Se encuentre un Punto de Interrupción.
* Se detecta la señal, de detenerse.
* Se genere algún error en la ejecución.
* Se ejecuta la instrucción i_SLEEP.
la bandera "stopped", indica que se ha detendio la ejecución sin completar la cantidad
de instrucciones requeridas.
Normalmente Se ejecutará el número de ciclos indicados, pero en algunos casos se
ejecutará un ciclo más, debido a que algunas instrucciones toman dos ciclos.}
var
  clkEnd: Int64;
  _pc: word;
begin
  clkEnd := nClck + nCyc;   //Valor final del contador
  while nClck < clkEnd do begin
    _pc := PC.W;
    if flash[_pc].breakPnt then begin
      //Encontró un BreakPoint, sale sin ejecutar esa instrucción
      if OnExecutionMsg<>nil then OnExecutionMsg('Stopped for breakpoint.');
      stopped := true;
      exit;
    end;
    if not flash[_pc].used then begin
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
    Exec(_pc);
    if idIns = i_SLEEP then begin
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
  PC.W   := 0;
  PCLATH := 0;
  W      := 0;
  STKPTR := 0;   //Posición inicial del puntero de pila
  nClck  := 0;   //Inicia contador de ciclos
  CommStop := false;  //Limpia bandera
  //Limpia solamente el valor inicial, no toca los otros campos
  for i:=0 to high(ram) do begin
    ram[i].dvalue := $00 or
           ram[i].implemOr;  //To set unimplemented bits fixed to "1".
  end;
  ram[_STATUS].dvalue := %00011000;  //STATUS
  ram[$85].dvalue := $FF and ram[$85].implemAnd or ram[$85].implemOr;  //TRISA
  ram[$86].dvalue := $FF and ram[$86].implemAnd or ram[$86].implemOr;  //TRISB
  ram[$87].dvalue := $FF and ram[$87].implemAnd or ram[$87].implemOr;  //TRISC
  //No todos los PIC tienen PORTD y PORTE. Usamos este método indirecto para averiguarlo
  if ram[$0C].state = cs_impleSFR then begin
    ram[$88].dvalue := $FF and ram[$88].implemAnd or ram[$88].implemOr;  //TRISD
    ram[$89].dvalue := $FF and ram[$89].implemAnd or ram[$89].implemOr;  //TRISE
  end;
end;
function TPIC16.ReadPC: dword;
begin
  Result := PC.W;
end;
procedure TPIC16.WritePC(AValue: dword);
begin
  PC.W := AValue;
end;
//Funciones para la memoria RAM
function TPIC16.GetFreeBit(out addr: word; out bit: byte; shared: boolean): boolean;
{Devuelve una dirección libre de la memoria RAM (y el banco).
"Shared" indica que se marcará el bit como de tipo "Compartido", y se usa para el
caso en que se quiera comaprtir la misma posición para diversos variables.
Si encuentra espacio, devuelve TRUE.}
var
  maxRam: word;
  i: Integer;
begin
  Result := false;   //valor inicial
  maxRam := NumBanks * PICBANKSIZE;  //posición máxima
  //Realmente debería explorar solo hasta la dirección implementada, por eficiencia
  for i:=0 to maxRam-1 do begin
    if (ram[i].state = cs_impleGPR) and (ram[i].used <> 255) then begin
      //Esta dirección tiene al menos un bit libre
      addr := i;  //devuelve dirección
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
function TPIC16.GetFreeByte(out addr: word; shared: boolean): boolean;
{Devuelve una dirección libre de la memoria flash.
"Shared" indica que se marcará el bit como de tipo "Compartido", y se usa para el
caso en que se quiera comaprtir la misma posición para diversos variables.
Si encuentra espacio, devuelve TRUE.}
var
  i: Integer;
  maxRam: word;
begin
  Result := false;   //valor inicial
  maxRam := NumBanks * PICBANKSIZE;  //posición máxima
  //Realmente debería explorar solo hasta la dirección implementada, por eficiencia
  for i:=0 to maxRam-1 do begin
    if (ram[i].state = cs_impleGPR) and (ram[i].used = 0) then begin
      //Esta dirección está libre
      ram[i].used:=255;   //marca como usado
      if shared then begin
        ram[i].shared := 255;  //Marca como compartido
      end;
      addr := i;
      //Notar que la posición de memoria puede estar mapeada a otro banco.
      Result := true;  //indica que encontró espacio
      exit;
    end;
  end;
end;
function TPIC16.GetFreeBytes(const size: integer; var addr: word): boolean;
{Devuelve una dirección libre de la memoria flash (y el banco) para ubicar un bloque
 del tamaño indicado. Si encuentra espacio, devuelve TRUE.
 El tamaño se da en bytes, pero si el valor es negativo, se entiende que es en bits.}
var
  i: word;
  maxRam: Word;
begin
  Result := false;  //valor por defecto
  if size=0 then exit;
  maxRam := word(NumBanks * PICBANKSIZE) - 1;
  for i:=0 to maxRam do begin  //verifica 1 a 1, por seguridad
    if HaveConsecGPR(i, size, maxRam) then begin
      //encontró del tamaño buscado
      UseConsecGPR(i, size);  //marca como usado
      addr := i;
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
  for i := 0 to word(NumBanks * PICBANKSIZE) - 1 do begin
    if i > high(ram) then exit;  //Protection
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
  for i := 0 to word(NumBanks * PICBANKSIZE) - 1 do begin
    if i > high(ram) then exit;  //Protection
    if ram[i].AvailGPR and (ram[i].used <> 0) then begin
      //Notar que "AvailGPR" asegura que no se consideran registros maepados
      Result := Result + 1;
    end;
  end;
end;
procedure TPIC16.ExploreUsed(rutExplorRAM: TPICRutExplorRAM);
{Genera un reporte de uso de RAM}
var
  i: Integer;
begin
  for i := 0 to word(NumBanks * PICBANKSIZE) - 1 do begin
    if i > high(ram) then exit;  //Protection
    if ram[i].AvailGPR and (ram[i].used <> 0) then begin
      rutExplorRAM(i, 0, @ram[i]);
    end;
  end;
end;
function TPIC16.ValidRAMaddr(addr: word): boolean;
{Indica si la dirección indicada es válida dentro del hardware del PIC}
begin
  if addr > PICBANKSIZE*NumBanks then exit(false);   //excede límite
  exit(true);
end;
function TPIC16.BankToAbsRAM(const offset, bank: byte): word;
{Convierte una dirección y banco a una dirección absoluta}
begin
  Result := bank * PICBANKSIZE + offset;
end;
procedure TPIC16.AbsToBankRAM(const AbsAddr: word; var offset, bank: byte);
{Convierte dirección absoluta a dirección en bancos}
begin
   offset := AbsAddr and %01111111;
   bank :=  AbsAddr >> 7;
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
    while (iHex<PICMAXFLASH) and not flash[iHex].used  do begin
      inc(iHex);
    end;
    if iHex>=PICMAXFLASH then begin
      //Llegó al final
      exit;  //sale con cadena nula
    end;
    //Ya encontró el inicio ahora busca celdas consecutivas
    p1 := iHex;
    Addre := p1;
    cont := 2;  //inicia contador
    inc(iHex);  //pasa al siguiente
    while (iHex<PICMAXFLASH) and (cont<MAX_INS_HEX) and flash[iHex].used do begin
      inc(iHex);
      inc(cont);
    end;
    if iHex>=PICMAXFLASH then begin
      //Salió porque Llegó al final
      p2 := PICMAXFLASH-1;
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
    tmp := '';
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
  minUsed := PICMAXFLASH;
  maxUsed := 0;
  iHex := 0;
  //Inicia la extracción de código
  dat := ExtractHex(addr);
  while dat <>'' do begin
     GenHexData(addr, dat);
     dat := ExtractHex(addr);
  end;
  //Bits de configuración
  tmp := '';
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
  i: Word;
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
    lin := DisassemblerAt(i, incVarNam);  //Instrucción
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
  PICBANKSIZE := 128;     //RAM bank size
  PICMAXRAM   := PICBANKSIZE * 4; //Máx RAM memory (4 banks)
  PICPAGESIZE := 2048;
  PICMAXFLASH := PICPAGESIZE * 4; //Máx Flash memeory (4 pages)
  SetLength(ram, PICMAXRAM);
  SetLength(flash, PICMAXFLASH);
  //Default hardware settings
  NumBanks:=2;     //Número de bancos de RAM. Por defecto se asume 2
  NumPages:=1;     //Número de páginas de memoria Flash. Por defecto 1
  MaxFlash := PICPAGESIZE;  //En algunos casos, puede ser menor al tamaño de una página
  //inicia una configuración común
  ClearMemRAM;
  SetStatRAM($020, $04F, cs_impleGPR);

  //estado inicial
  iFlash := 0;   //posición de inicio
  ClearMemFlash;
end;
destructor TPIC16.Destroy;
begin
  inherited Destroy;
end;
procedure InitTables;
begin
  //Inicializa Mnemónico de instrucciones
  PIC16InstName[i_ADDWF ] := 'ADDWF';
  PIC16InstName[i_ANDWF ] := 'ANDWF';
  PIC16InstName[i_CLRF  ] := 'CLRF';
  PIC16InstName[i_CLRW  ] := 'CLRW';
  PIC16InstName[i_COMF  ] := 'COMF';
  PIC16InstName[i_DECF  ] := 'DECF';
  PIC16InstName[i_DECFSZ] := 'DECFSZ';
  PIC16InstName[i_INCF  ] := 'INCF';
  PIC16InstName[i_INCFSZ] := 'INCFSZ';
  PIC16InstName[i_IORWF ] := 'IORWF';
  PIC16InstName[i_MOVF  ] := 'MOVF';
  PIC16InstName[i_MOVWF ] := 'MOVWF';
  PIC16InstName[i_NOP   ] := 'NOP';
  PIC16InstName[i_RLF   ] := 'RLF';
  PIC16InstName[i_RRF   ] := 'RRF';
  PIC16InstName[i_SUBWF ] := 'SUBWF';
  PIC16InstName[i_SWAPF ] := 'SWAPF';
  PIC16InstName[i_XORWF ] := 'XORWF';
  PIC16InstName[i_BCF   ] := 'BCF';
  PIC16InstName[i_BSF   ] := 'BSF';
  PIC16InstName[i_BTFSC ] := 'BTFSC';
  PIC16InstName[i_BTFSS ] := 'BTFSS';
  PIC16InstName[i_ADDLW ] := 'ADDLW';
  PIC16InstName[i_ANDLW ] := 'ANDLW';
  PIC16InstName[i_CALL  ] := 'CALL';
  PIC16InstName[i_CLRWDT] := 'CLRWDT';
  PIC16InstName[i_GOTO ] := 'GOTO';
  PIC16InstName[i_IORLW ] := 'IORLW';
  PIC16InstName[i_MOVLW ] := 'MOVLW';
  PIC16InstName[i_RETFIE] := 'RETFIE';
  PIC16InstName[i_RETLW ] := 'RETLW';
  PIC16InstName[i_RETURN] := 'RETURN';
  PIC16InstName[i_SLEEP ] := 'SLEEP';
  PIC16InstName[i_SUBLW ] := 'SUBLW';
  PIC16InstName[i_XORLW ] := 'XORLW';
  PIC16InstName[i_Inval] := '<Inval>';

  //Inicializa Sintaxis de las instrucciones
  {Los valorees para la sintaxis significan:
  f->dirección de un registro en RAM (0..127)
  d->destino (W o F)
  b->número de bit (0..7)
  a->dirección destino (0..$7FF)
  k->literal byte (0..255)
  }
  PIC16InstSyntax[i_ADDWF ] := 'fd';
  PIC16InstSyntax[i_ANDWF ] := 'fd';
  PIC16InstSyntax[i_CLRF  ] := 'f';
  PIC16InstSyntax[i_CLRW  ] := '';
  PIC16InstSyntax[i_COMF  ] := 'fd';
  PIC16InstSyntax[i_DECF  ] := 'fd';
  PIC16InstSyntax[i_DECFSZ] := 'fd';
  PIC16InstSyntax[i_INCF  ] := 'fd';
  PIC16InstSyntax[i_INCFSZ] := 'fd';
  PIC16InstSyntax[i_IORWF ] := 'fd';
  PIC16InstSyntax[i_MOVF  ] := 'fd';
  PIC16InstSyntax[i_MOVWF ] := 'f';
  PIC16InstSyntax[i_NOP   ] := '';
  PIC16InstSyntax[i_RLF   ] := 'fd';
  PIC16InstSyntax[i_RRF   ] := 'fd';
  PIC16InstSyntax[i_SUBWF ] := 'fd';
  PIC16InstSyntax[i_SWAPF ] := 'fd';
  PIC16InstSyntax[i_XORWF ] := 'fd';
  PIC16InstSyntax[i_BCF   ] := 'fb';
  PIC16InstSyntax[i_BSF   ] := 'fb';
  PIC16InstSyntax[i_BTFSC ] := 'fb';
  PIC16InstSyntax[i_BTFSS ] := 'fb';
  PIC16InstSyntax[i_ADDLW ] := 'k';
  PIC16InstSyntax[i_ANDLW ] := 'k';
  PIC16InstSyntax[i_CALL  ] := 'a';
  PIC16InstSyntax[i_CLRWDT] := '';
  PIC16InstSyntax[i_GOTO ] := 'a';
  PIC16InstSyntax[i_IORLW ] := 'k';
  PIC16InstSyntax[i_MOVLW ] := 'k';
  PIC16InstSyntax[i_RETFIE] := '';
  PIC16InstSyntax[i_RETLW ] := 'k';
  PIC16InstSyntax[i_RETURN] := '';
  PIC16InstSyntax[i_SLEEP ] := '';
  PIC16InstSyntax[i_SUBLW ] := 'k';
  PIC16InstSyntax[i_XORLW ] := 'k';
  PIC16InstSyntax[i_Inval] := '<???>';
end;
initialization
  InitTables;
end.

