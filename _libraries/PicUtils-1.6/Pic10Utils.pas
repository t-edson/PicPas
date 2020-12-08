{
Description
===========
Utilities for programming Baseline PIC microcontrollers with 12 bits instructions.
Include most of the PIC10 devices.
This unit works with 512 words pages and 32 bytes RAM banks.
The main class TPIC10 must model all devices of this family, including the most complex.
The aim of this unit is to be used as base for assemblers, compilers and simulators.

                                         Created by Tito Hinostroza   26/04/2018
}

unit Pic10Utils;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LCLProc, PicCore, MisUtils;
type  //Baseline PIC instructions
  TPIC10Inst = (
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
    i_ANDLW,
    i_CALL,
    i_CLRWDT,
    i_GOTO,
    i_IORLW,
    i_MOVLW,
    i_RETLW,
    i_SLEEP,
    i_XORLW,
    i_OPTION,
    i_TRIS,
    //EXTENDED INSTRUCTIONS (Only for some Models)
    i_MOVLB,
    i_RETURN,
    i_RETFIE,
    //INVALID INSTRUCTION
    i_Inval
  );
  //Indica el destino de la instrucción
  TPIC10Destin = (
    toW = %000000,    //al acumulador
    toF = %100000     //a memoria
  );



const  //Constants of address and bit positions for some registers
  _STATUS = $03;
  _C      = 0;
  _Z      = 2;
  _RP0    = 5;
  _RP1    = 6;
//  _IRP   = 7;
type
  {Objeto que representa al hardware de un PIC de la serie 10}
  { TPIC10 }
  TPIC10 = class(TPicCore)
  public  //Campos para procesar instrucciones
    idIns: TPIC10Inst;    //ID de Instrucción.
    d_   : TPIC10Destin;  //Destino de operación. Válido solo en algunas instrucciones.
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
    OPTION   : byte;   //In Baseline PICs, this register is internal
    TRISA    : byte;   //In Baseline PICs, this register is internal
    TRISB    : byte;   //In Baseline PICs, this register is internal (Only exists for some devices)
    TRISC    : byte;   //In Baseline PICs, this register is internal (Only exists for some devices)
    BSR      : byte;   //In Baseline PICs, this register is internal (Only exists for some devices)
    property STATUS: byte read GetSTATUS;
    property STATUS_Z: boolean read GetSTATUS_Z write SetSTATUS_Z;
    property STATUS_C: boolean read GetSTATUS_C write SetSTATUS_C;
    property STATUS_DC: boolean read GetSTATUS_DC write SetSTATUS_DC;
    property STATUS_IRP: boolean read GetSTATUS_IRP write SetSTATUS_IRP;
    property INTCON: byte read GetINTCON;
    property INTCON_GIE: boolean read GetINTCON_GIE write SetINTCON_GIE;
    property FRAM: byte read GetFRAM write SetFRAM;
  public  //Execution control
    Enhanced: boolean; //Indicate a Enhanced core like 16F527, 16F570 with more instructions
    function CurInstruction: TPIC10Inst;
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
    procedure codAsmFD(const inst: TPIC10Inst; const f: word; d: TPIC10Destin);
    procedure codAsmF(const inst: TPIC10Inst; const f: word);
    procedure codAsmFB(const inst: TPIC10Inst; const f: word; b: byte);
    procedure codAsmK(const inst: TPIC10Inst; const k: byte);
    procedure codAsmA(const inst: TPIC10Inst; const a: word);
    procedure codAsm(const inst: TPIC10Inst);
    procedure codGotoAt(iflash0: integer; const k: word);
    procedure codCallAt(iflash0: integer; const k: word);
    function codInsert(iflash0, nInsert, nWords: integer): boolean;
    procedure BTFSC_sw_BTFSS(iflash0: integer);
  public  //Métodos adicionales
    function FindOpcode(Op: string; out syntax: string): TPIC10Inst;  //busca Opcode
    procedure GenHex(hexFile: string; ConfigWord: integer = - 1);  //genera un archivo hex
    procedure DumpCode(lOut: TStrings; incAdrr, incCom, incVarNam: boolean);  //vuelva en código que contiene
  public  //Initialization
    constructor Create; override;
    destructor Destroy; override;
  end;

var  //variables globales
  //mnemónico de las instrucciones
  PIC10InstName: array[low(TPIC10Inst)..high(TPIC10Inst)] of string[7];
  //sintaxis en ensamblador de las instrucciones
  PIC10InstSyntax: array[low(TPIC10Inst)..high(TPIC10Inst)] of string[5];

implementation

{ TPIC10 }
procedure TPIC10.useFlash;
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
procedure TPIC10.codAsmFD(const inst: TPIC10Inst; const f: word; d: TPIC10Destin);
{Codifica las instrucciones orientadas a registro, con sinatxis: NEMÓNICO f,d}
begin
  case inst of
  i_ADDWF : flash[iFlash].value := %000111000000 + ord(d) + (f and %11111);
  i_ANDWF : flash[iFlash].value := %000101000000 + ord(d) + (f and %11111);
  i_COMF  : flash[iFlash].value := %001001000000 + ord(d) + (f and %11111);
  i_DECF  : flash[iFlash].value := %000011000000 + ord(d) + (f and %11111);
  i_DECFSZ: flash[iFlash].value := %001011000000 + ord(d) + (f and %11111);
  i_INCF  : flash[iFlash].value := %001010000000 + ord(d) + (f and %11111);
  i_INCFSZ: flash[iFlash].value := %001111000000 + ord(d) + (f and %11111);
  i_IORWF : flash[iFlash].value := %000100000000 + ord(d) + (f and %11111);
  i_MOVF  : flash[iFlash].value := %001000000000 + ord(d) + (f and %11111);
  i_RLF   : flash[iFlash].value := %001101000000 + ord(d) + (f and %11111);
  i_RRF   : flash[iFlash].value := %001100000000 + ord(d) + (f and %11111);
  i_SUBWF : flash[iFlash].value := %000010000000 + ord(d) + (f and %11111);
  i_SWAPF : flash[iFlash].value := %001110000000 + ord(d) + (f and %11111);
  i_XORWF : flash[iFlash].value := %000110000000 + ord(d) + (f and %11111);
  else
    raise Exception.Create('Implementation Error.');
  end;
  useFlash;  //Mark as "used" and increase pointer.
end;
procedure TPIC10.codAsmF(const inst: TPIC10Inst; const f: word);
{Codifica las instrucciones orientadas a registro, con sinatxis: NEMÓNICO f}
begin
  case inst of
  i_CLRF  : flash[iFlash].value := %000001100000 + (f and %11111);
  i_MOVWF : flash[iFlash].value := %000000100000 + (f and %11111);
  i_TRIS  : flash[iFlash].value := %000000000000 + (f and %1111); //Only valid if f=5,6,7,8,9
  else
    raise Exception.Create('Implementation Error.');
  end;
  useFlash;  //Mark as "used" and increase pointer.
end;
procedure TPIC10.codAsmFB(const inst: TPIC10Inst; const f: word; b: byte);
//Codifica las instrucciones orientadas a bit.
begin
  case inst of
  i_BCF  : flash[iFlash].value := %010000000000 + word(b<<5) + (f and %11111);
  i_BSF  : flash[iFlash].value := %010100000000 + word(b<<5) + (f and %11111);
  i_BTFSC: flash[iFlash].value := %011000000000 + word(b<<5) + (f and %11111);
  i_BTFSS: flash[iFlash].value := %011100000000 + word(b<<5) + (f and %11111);
  else
    raise Exception.Create('Implementation Error.');
  end;
  useFlash;  //Mark as "used" and increase pointer.
end;
procedure TPIC10.codAsmK(const inst: TPIC10Inst; const k: byte);
{Codifica las instrucciones con constantes.}
begin
  case inst of
  i_ANDLW : flash[iFlash].value := %111000000000 + k;
  i_IORLW : flash[iFlash].value := %110100000000 + k;
  i_MOVLW : flash[iFlash].value := %110000000000 + k;
  i_RETLW : flash[iFlash].value := %100000000000 + k;
  i_XORLW : flash[iFlash].value := %111100000000 + k;
  i_MOVLB : flash[iFlash].value := %000000010000 + (k AND %111);  //Extended
  else
    raise Exception.Create('Implementation Error.');
  end;
  useFlash;  //Mark as "used" and increase pointer.
end;
procedure TPIC10.codAsmA(const inst: TPIC10Inst; const a: word);
{Codifica las instrucciones de control.
 "a" debe ser word, porque la dirección destino, requiere hasta 9 bits.}
begin
  case inst of
  i_CALL  : flash[iFlash].value := %100100000000 + (a and %11111111);
  i_GOTO : flash[iFlash].value := %101000000000 + (a and %111111111);
  else
    raise Exception.Create('Implementation Error.');
  end;
  useFlash;  //Mark as "used" and increase pointer.
end;
procedure TPIC10.codAsm(const inst: TPIC10Inst);
//Codifica las instrucciones de control.
begin
  case inst of
  i_CLRW  : flash[iFlash].value := %000001000000;
  i_NOP   : flash[iFlash].value := %000000000000;
  i_CLRWDT: flash[iFlash].value := %000000000100;
  i_SLEEP : flash[iFlash].value := %000000000011;
  i_OPTION: flash[iFlash].value := %000000000010;
  i_RETFIE: flash[iFlash].value := %000000011111;  //Extended
  i_RETURN: flash[iFlash].value := %000000011110;  //Extended
  else
    raise Exception.Create('Implementation Error.');
  end;
  useFlash;  //Mark as "used" and increase pointer.
end;
procedure TPIC10.codGotoAt(iflash0: integer; const k: word);
{Codifica una instrucción GOTO, en una posición específica y sin alterar el puntero "iFlash"
actual. Se usa para completar saltos indefinidos}
begin
  flash[iFlash0].value := %101000000000 + (k and %111111111);
end;
procedure TPIC10.codCallAt(iflash0: integer; const k: word);
{Codifica una instrucción i_CALL, en una posición específica y sin alterar el puntero "iFlash"
actual. Se usa para completar llamadas indefinidas}
begin
  flash[iFlash0].value := %100100000000 + (k and %11111111);
end;
function TPIC10.codInsert(iflash0, nInsert, nWords: integer): boolean;
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
procedure TPIC10.BTFSC_sw_BTFSS(iflash0: integer);
{Exchange instruction i_BTFSC to i_BTFSS, or viceversa, in the specified address.}
begin
  //Solo necesita cambiar el bit apropiado
  flash[iFlash0].value := flash[iFlash0].value XOR %10000000000;
end;
function TPIC10.FindOpcode(Op: string; out syntax: string): TPIC10Inst;
{Busca una cádena que represente a una instrucción (Opcode). Si encuentra devuelve
 el identificador de instrucción y una cadena que representa a la sintaxis en "syntax".
 Si no encuentra devuelve "i_Inval". }
var
  idInst: TPIC10Inst;
  tmp: String;
  found: Boolean;
begin
  found := false;
  tmp := UpperCase(Op);
  for idInst := low(TPIC10Inst) to high(TPIC10Inst) do begin
    if PIC10InstName[idInst] = tmp then begin
      found := true;
      break;
    end;
  end;
  if found then begin
    Result := idInst;
    syntax := PIC10InstSyntax[idInst];
  end else  begin
    Result := i_Inval;
  end;
end;
//Campos para procesar instrucciones
function TPIC10.GetBank(i : Longint): TPICRAMBank;
begin
  Result.AddrStart := i*PICBANKSIZE;
  Result.AddrEnd   := (i+1)*PICBANKSIZE-1;
end;
function TPIC10.GetPage(i: Longint): TPICFlashPage;
begin
  Result.AddrStart := i*PICPAGESIZE;
  Result.AddrEnd   := (i+1)*PICPAGESIZE-1;
end;
function TPIC10.GetSTATUS: byte;
begin
  Result := ram[_STATUS].value;
end;
function TPIC10.GetSTATUS_Z: boolean;
begin
  Result := (ram[_STATUS].dvalue and %00000100) <> 0;
end;
procedure TPIC10.SetSTATUS_Z(AValue: boolean);
begin
  if AVAlue then ram[_STATUS].dvalue := ram[_STATUS].dvalue or  %00000100
            else ram[_STATUS].dvalue := ram[_STATUS].dvalue and %11111011;
end;
function TPIC10.GetSTATUS_C: boolean;
begin
  Result := (ram[_STATUS].dvalue and %00000001) <> 0;
end;
procedure TPIC10.SetSTATUS_C(AValue: boolean);
begin
  if AVAlue then ram[_STATUS].dvalue := ram[_STATUS].dvalue or  %00000001
            else ram[_STATUS].dvalue := ram[_STATUS].dvalue and %11111110;
end;
function TPIC10.GetSTATUS_DC: boolean;
begin
  Result := (ram[_STATUS].dvalue and %00000010) <> 0;
end;
procedure TPIC10.SetSTATUS_DC(AValue: boolean);
begin
  if AVAlue then ram[_STATUS].dvalue := ram[_STATUS].dvalue or  %00000010
            else ram[_STATUS].dvalue := ram[_STATUS].dvalue and %11111101;
end;
function TPIC10.GetSTATUS_IRP: boolean;
begin
  Result := (ram[_STATUS].dvalue and %10000000) <> 0;
end;
procedure TPIC10.SetSTATUS_IRP(AValue: boolean);
begin
  if AVAlue then ram[_STATUS].dvalue := ram[_STATUS].dvalue or  %10000000
            else ram[_STATUS].dvalue := ram[_STATUS].dvalue and %01111111;
end;
function TPIC10.GetINTCON: byte;
begin
  Result := ram[$0B].dvalue;
end;
function TPIC10.GetINTCON_GIE: boolean;
begin
  Result := (ram[$0B].dvalue and %10000000) <> 0;
end;
procedure TPIC10.SetINTCON_GIE(AValue: boolean);
begin
  if AVAlue then ram[$0B].dvalue := ram[$0B].dvalue or  %10000000
            else ram[$0B].dvalue := ram[$0B].dvalue and %01111111;
end;
procedure TPIC10.SetFRAM(value: byte);
{Escribe en la RAM; en la dirección global f_, el valor "value"
Para determinar el valor real de la dirección, se toma en cuenta los bits de BSR}
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
  case BSR and %111 of
  %000: pRAM := @ram[f_              ];
  %001: pRAM := @ram[f_+PICBANKSIZE  ];
  %010: pRAM := @ram[f_+PICBANKSIZE*2];
  %011: pRAM := @ram[f_+PICBANKSIZE*3];
  %100: pRAM := @ram[f_+PICBANKSIZE*4];
  %101: pRAM := @ram[f_+PICBANKSIZE*5];
  %110: pRAM := @ram[f_+PICBANKSIZE*6];
  %111: pRAM := @ram[f_+PICBANKSIZE*7];
  end;
  pRAM^.value := value and pRAM^.implemAnd or pRAM^.implemOr;
end;
function TPIC10.GetFRAM: byte;
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
  case BSR and %111 of
  %000: Result := ram[f_              ].value;
  %001: Result := ram[f_+PICBANKSIZE  ].value;
  %010: Result := ram[f_+PICBANKSIZE*2].value;
  %011: Result := ram[f_+PICBANKSIZE*3].value;
  %100: Result := ram[f_+PICBANKSIZE*4].value;
  %101: Result := ram[f_+PICBANKSIZE*5].value;
  %110: Result := ram[f_+PICBANKSIZE*6].value;
  %111: Result := ram[f_+PICBANKSIZE*7].value;
  end;
end;
procedure TPIC10.Decode(const opCode: word);
{Decodifica la instrucción indicada. Actualiza siempre la variable "idIns", y
dependiendo de la instrucción, puede actualizar: d_, f_, b_ y k_

0000 0000 0000	i_NOP
0000 0000 0010	i_OPTION
0000 0000 0011	i_SLEEP
0000 0000 0100	i_CLRWDT
0000 0000 01ff	i_TRIS x (x=5,6,7, f=01,10,11)
0000 0000 1fff	i_TRIS x (x=8,9, f=000, 001)
0000 001f ffff	i_MOVWF f
0000 01df ffff	CLR f,d
0000 10df ffff	i_SUBWF f,d
0000 11df ffff	i_DECF f,d

0001 00df ffff	i_IORWF f,d
0001 01df ffff	i_ANDWF f,d
0001 10df ffff	i_XORWF f,d
0001 11df ffff	i_ADDWF f,d

0010 00df ffff	i_MOVF f,d
0010 01df ffff	i_COMF f,d
0010 10df ffff	i_INCF f,d
0010 11df ffff	i_DECFSZ f,d

0011 00df ffff	i_RRF f,d
0011 01df ffff	i_RLF f,d
0011 10df ffff	i_SWAPF f,d
0011 11df ffff	i_INCFSZ f,d

0100 bbbf ffff	i_BCF f,b
0101 bbbf ffff	i_BSF f,b
0110 bbbf ffff	i_BTFSC f,b
0111 bbbf ffff	i_BTFSS f,b

1000 kkkk kkkk	i_RETLW k
1001 kkkk kkkk	i_CALL k
101k kkkk kkkk	GOTO k
1100 kkkk kkkk	i_MOVLW k
1101 kkkk kkkk	i_IORLW k
1110 kkkk kkkk	i_ANDLW k
1111 kkkk kkkk	i_XORLW k
}
var
  codH : byte;  //6 bits altos de la instrucción
  codL : byte;  //byte bajo de la instrucción
begin
  codH := (opCode and $0F00) >> 8;  //se debería optimizar
  codL := opCode and $00FF;
  case codH of
  %0000: begin
    if (codL and %11111000) = %00010000 then begin
      idIns := i_MOVLB;   //Extended instruction
      k_ := codL and %00000111;
    end else if (codL and %11100000) = %00100000 then begin
      idIns := i_MOVWF;
      f_ := codL and %00011111;
    end else if (codL and %11100000) = %01100000 then begin
      idIns := i_CLRF;
      f_ := codL and %00011111;
    end else if (codL and %11000000) = %10000000 then begin
      idIns := i_SUBWF;
      d_ := TPIC10Destin(codL and %00100000);
      f_ := codL and %00011111;
    end else if (codL and %11000000) = %11000000 then begin
      idIns := i_DECF;
      d_ := TPIC10Destin(codL and %00100000);
      f_ := codL and %00011111;
    end else begin
      //bit7 a cero, hay varias opciones
      case codL of
      %00000000: begin
        idIns := i_NOP;
      end;
      %00011111: begin
        idIns := i_RETFIE;  //Extended instruction
      end;
      %00011110: begin
        idIns := i_RETURN;  //Extended instruction
      end;
      %01000000: begin
        idIns := i_CLRW;
      end;
      %00000010: begin
        idIns := i_OPTION;
      end;
      %00000011: begin
        idIns := i_SLEEP;
      end;
      %00000100: begin
        idIns := i_CLRWDT;
      end;
      %00000101: begin
        idIns := i_TRIS;
        f_ := 5;
      end;
      %00000110: begin
        idIns := i_TRIS;  //"i_TRIS GPIO" o "i_TRIS PORTB" according to the device
        f_ := 6;
      end;
      %00000111: begin
        idIns := i_TRIS;  //"i_TRIS PORTC" exist only in some devices
        f_ := 7;
      end;
      %00001000: begin
        idIns := i_TRIS;
        f_ := 8;
      end;
      %00001001: begin
        idIns := i_TRIS;
        f_ := 9;
      end;
      else
        idIns := i_Inval;
      end;
    end;
  end;
  %0001: begin
    if (codL and %11000000) = %00000000 then begin
      idIns := i_IORWF;
      d_ := TPIC10Destin(codL and %00100000);
      f_ := codL and %00011111;
    end else if (codL and %11000000) = %01000000 then begin
      idIns := i_ANDWF;
      d_ := TPIC10Destin(codL and %00100000);
      f_ := codL and %00011111;
    end else if (codL and %11000000) = %10000000 then begin
      idIns := i_XORWF;
      d_ := TPIC10Destin(codL and %00100000);
      f_ := codL and %00011111;
    end else if (codL and %11000000) = %11000000 then begin
      idIns := i_ADDWF;
      d_ := TPIC10Destin(codL and %00100000);
      f_ := codL and %00011111;
    end;
  end;
  %0010: begin
    if (codL and %11000000) = %00000000 then begin
      idIns := i_MOVF;
      d_ := TPIC10Destin(codL and %00100000);
      f_ := codL and %00011111;
    end else if (codL and %11000000) = %01000000 then begin
      idIns := i_COMF;
      d_ := TPIC10Destin(codL and %00100000);
      f_ := codL and %00011111;
    end else if (codL and %11000000) = %10000000 then begin
      idIns := i_INCF;
      d_ := TPIC10Destin(codL and %00100000);
      f_ := codL and %00011111;
    end else if (codL and %11000000) = %11000000 then begin
      idIns := i_DECFSZ;
      d_ := TPIC10Destin(codL and %00100000);
      f_ := codL and %00011111;
    end;
  end;
  %0011: begin
    if (codL and %11000000) = %00000000 then begin
      idIns := i_RRF;
      d_ := TPIC10Destin(codL and %00100000);
      f_ := codL and %00011111;
    end else if (codL and %11000000) = %01000000 then begin
      idIns := i_RLF;
      d_ := TPIC10Destin(codL and %00100000);
      f_ := codL and %00011111;
    end else if (codL and %11000000) = %10000000 then begin
      idIns := i_SWAPF;
      d_ := TPIC10Destin(codL and %00100000);
      f_ := codL and %00011111;
    end else if (codL and %11000000) = %11000000 then begin
      idIns := i_INCFSZ;
      d_ := TPIC10Destin(codL and %00100000);
      f_ := codL and %00011111;
    end;
  end;
  %0100: begin
    idIns := i_BCF;
    b_ := (codL and %11100000) >> 5;
    f_ := codL and %00011111;
  end;
  %0101: begin
    idIns := i_BSF;
    b_ := (codL and %11100000) >> 5;
    f_ := codL and %00011111;
  end;
  %0110: begin
    idIns := i_BTFSC;
    b_ := (codL and %11100000) >> 5;
    f_ := codL and %00011111;
  end;
  %0111: begin
    idIns := i_BTFSS;
    b_ := (codL and %11100000) >> 5;
    f_ := codL and %00011111;
  end;
  %1000: begin
    idIns := i_RETLW;
    k_ := codL;
  end;
  %1001: begin
    idIns := i_CALL;
    k_ := codL;
  end;
  %1010: begin
    idIns := i_GOTO;
    k_ := codL;
  end;
  %1011: begin
    idIns := i_GOTO;
    k_ := codL + 256;
  end;
  %1100: begin
    idIns := i_MOVLW;
    k_ := codL;
  end;
  %1101: begin
    idIns := i_IORLW;
    k_ := codL;
  end;
  %1110: begin
    idIns := i_ANDLW;
    k_ := codL;
  end;
  %1111: begin
    idIns := i_XORLW;
    k_ := codL;
  end;
  else
    idIns := i_Inval;
  end;
end;
function TPIC10.Disassembler(const opCode: word; bankNum: byte = 255;
                             useVarName: boolean = false): string;
{Desensambla la instrucción "opCode". Esta rutina utiliza las variables: d_, f_, b_ y k_
"opCode"  -> Código de Operación que se desea decodificar. Se asume que es de 12 bits.
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
  nemo := lowerCase(trim(PIC10InstName[idIns])) + ' ';
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
      if bankNum in [0,1,2,3,4,5,6,7] then begin
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
        if bankNum in [0,1,2,3,4,5,6,7] then begin
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
  i_TRIS: begin
    Result := nemo + '0x'+IntToHex(f_,2);
  end;
  i_BCF,
  i_BSF,
  i_BTFSC,
  i_BTFSS: begin    //Instrucciones de bit
      if bankNum in [0,1,2,3,4,5,6,7] then begin
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
  i_ANDLW,
  i_IORLW,
  i_MOVLW,
  i_RETLW,
  i_XORLW,
  i_MOVLB: begin
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
  i_SLEEP,
  i_OPTION: begin
       Result := nemo ;
     end;
  else
    Result := 'Invalid'
  end;
end;
function TPIC10.DisassemblerAt(addr: word; useVarName: boolean): string;
{Disassembler the instruction located in "addr"}
var
  valOp: Word;
  bnkOp: Byte;
begin
  valOp := flash[addr].value;
  bnkOp := flash[addr].curBnk;
  Result := Disassembler(valOp, bnkOp, useVarName);   //desensambla
end;
function TPIC10.CurInstruction: TPIC10Inst;
{Resturn the instruction pointed by PC, in this moment.}
begin
  Decode(flash[PC.W].value);   //decodifica instrucción
  Result := idIns;
end;
procedure TPIC10.Exec;
{Executa la instrucción actual}
begin
  Exec(PC.W);
end;
procedure TPIC10.Exec(aPC: word);
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
  i_XORLW: begin
    resByte := W xor k_;
    w := resByte;
    STATUS_Z := resByte = 0;
  end;
  i_OPTION: begin
    OPTION := W;
  end;
  i_TRIS: begin
    if f_ = 6 then TRISA := W;
    if f_ = 7 then TRISB := W;
    if f_ = 8 then TRISC := W;
  end;
  i_MOVLB: begin
    BSR := k_;
  end;
  i_Inval: begin
    MsjError := 'Invalid Opcode';
  end;
  end;
  //Incrementa contador
  Inc(PC.W);
  Inc(nClck);
end;
procedure TPIC10.ExecTo(endAdd: word);
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
procedure TPIC10.ExecStep;
begin
  if CurInstruction = i_CALL then begin
    ExecTo(PC.W+1);  //Ejecuta hasta la sgte. instrucción, salta el i_CALL
  end else begin
    Exec;
  end;
end;
procedure TPIC10.ExecNCycles(nCyc: integer; out stopped: boolean);
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
  consoleTickStart;
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
//  consoleTickCount('');
end;
procedure TPIC10.Reset;
//Reinicia el dipsoitivo
var
  i: Integer;
begin
  PC.W   := 0;
  PCLATH := 0;
  W      := 0;
  STKPTR := 0;   //Posición inicial del puntero de pila
  OPTION := $FF;
  TRISA  := $FF;
  BSR    := $00;
  nClck  := 0;    //Inicia contador de ciclos
  CommStop := false;  //Limpia bandera
  //Limpia solamente el valor inicial, no toca los otros campos
  for i:=0 to high(ram) do begin
    ram[i].dvalue := $00 or ram[i].implemOr;
  end;
  ram[_STATUS].dvalue := %00011000;  //STATUS
end;
function TPIC10.ReadPC: dword;
begin
  Result := PC.W;
end;
procedure TPIC10.WritePC(AValue: dword);
begin
  PC.W := AValue;
end;
//Funciones para la memoria RAM
function TPIC10.GetFreeBit(out addr: word; out bit: byte; shared: boolean): boolean;
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
function TPIC10.GetFreeByte(out addr: word; shared: boolean): boolean;
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
function TPIC10.GetFreeBytes(const size: integer; var addr: word): boolean;
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
function TPIC10.TotalMemRAM: word;
{Devuelve el total de memoria RAM disponible}
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to word(NumBanks * PICBANKSIZE) - 1 do begin
    if ram[i].AvailGPR then begin
      Result := Result + 1;
    end;
  end;
end;
function TPIC10.UsedMemRAM: word;
{Devuelve el total de memoria RAM usada}
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to word(NumBanks * PICBANKSIZE) - 1 do begin
    if ram[i].AvailGPR and (ram[i].used <> 0) then begin
      //Notar que "AvailGPR" asegura que no se consideran registros maepados
      Result := Result + 1;
    end;
  end;
end;
procedure TPIC10.ExploreUsed(rutExplorRAM: TPICRutExplorRAM);
{Genera un reporte de uso de RAM}
var
  i: Integer;
begin
  for i := 0 to word(NumBanks * PICBANKSIZE) - 1 do begin
    if ram[i].AvailGPR and (ram[i].used <> 0) then begin
      rutExplorRAM(i, 0, @ram[i]);
    end;
  end;
end;
function TPIC10.ValidRAMaddr(addr: word): boolean;
{Indica si la dirercción indicada es válida dentro del hardware del PIC}
begin
  if addr > PICBANKSIZE*NumBanks then exit(false);   //excede límite
  exit(true);
end;
function TPIC10.BankToAbsRAM(const offset, bank: byte): word;
{Convierte una dirección y banco a una dirección absoluta}
begin
  Result := bank * PICBANKSIZE + offset;
end;
procedure TPIC10.AbsToBankRAM(const AbsAddr: word; var offset, bank: byte);
{Convierte dirección absoluta a dirección en bancos}
begin
   offset := AbsAddr and %01111111;
   bank :=  AbsAddr >> 7;
end;
procedure TPIC10.GenHex(hexFile: string; ConfigWord: integer = -1);
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
  con valores en hexadecimal de instrucciones, consecutivas usadas, en le memoria FLASH.
  La lectura se hace a partir de iHex, y los caracteres en hexadecimal se escriben en 4
  dígitos, en la misma forma que se usan para los archivos *.HEX.
  En "Addre" devuelve la dirección absoluta de inicio desde donde lee. Con cada llamada,
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
//    GenHexData($2007, tmp);
    GenHexData($FFF, tmp);  //By convention it's used this address.
  end;
  GenHexEOF;                    //Fin de archivo
  GenHexComm(self.Model);       //Comentario
  hexLines.SaveToFile(hexFile); //Genera archivo
end;
procedure TPIC10.DumpCode(lOut: TStrings; incAdrr, incCom, incVarNam: boolean);
{Desensambla las instrucciones grabadas en el PIC.
 Se debe llamar despues de llamar a GenHex(), para que se actualicen las variables}
var
  valOp, i: Word;
  lblLin, comLat, comLin, lin: String;
  bnkOp: Byte;
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
    valOp := flash[i].value;
    bnkOp := flash[i].curBnk;
    //Escribe línea
    lin := Disassembler(valOp, bnkOp, incVarNam);  //Instrucción
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
constructor TPIC10.Create;
begin
  inherited Create;
  PICBANKSIZE := 32;      //RAM bank size
  PICMAXRAM   := PICBANKSIZE * 8;  //Máx RAM memory (8 banks)
  PICPAGESIZE := 512;
  PICMAXFLASH := PICPAGESIZE * 4;  //Máx Flash memeory (4 pages)
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
destructor TPIC10.Destroy;
begin
  inherited Destroy;
end;
procedure InitTables;
begin
  //Inicializa Mnemónico de instrucciones
  //BYTE-ORIENTED FILE REGISTER OPERATIONS
  PIC10InstName[i_ADDWF ] := 'ADDWF';
  PIC10InstName[i_ANDWF ] := 'ANDWF';
  PIC10InstName[i_CLRF  ] := 'CLRF';
  PIC10InstName[i_CLRW  ] := 'CLRW';
  PIC10InstName[i_COMF  ] := 'COMF';
  PIC10InstName[i_DECF  ] := 'DECF';
  PIC10InstName[i_DECFSZ] := 'DECFSZ';
  PIC10InstName[i_INCF  ] := 'INCF';
  PIC10InstName[i_INCFSZ] := 'INCFSZ';
  PIC10InstName[i_IORWF ] := 'IORWF';
  PIC10InstName[i_MOVF  ] := 'MOVF';
  PIC10InstName[i_MOVWF ] := 'MOVWF';
  PIC10InstName[i_NOP   ] := 'NOP';
  PIC10InstName[i_RLF   ] := 'RLF';
  PIC10InstName[i_RRF   ] := 'RRF';
  PIC10InstName[i_SUBWF ] := 'SUBWF';
  PIC10InstName[i_SWAPF ] := 'SWAPF';
  PIC10InstName[i_XORWF ] := 'XORWF';
  //BIT-ORIENTED FILE REGISTER OPERATIONS
  PIC10InstName[i_BCF   ] := 'BCF';
  PIC10InstName[i_BSF   ] := 'BSF';
  PIC10InstName[i_BTFSC ] := 'BTFSC';
  PIC10InstName[i_BTFSS ] := 'BTFSS';
  //LITERAL AND CONTROL OPERATIONS
  PIC10InstName[i_ANDLW ] := 'ANDLW';
  PIC10InstName[i_CALL  ] := 'CALL';
  PIC10InstName[i_CLRWDT] := 'CLRWDT';
  PIC10InstName[i_GOTO  ] := 'GOTO';
  PIC10InstName[i_IORLW ] := 'IORLW';
  PIC10InstName[i_MOVLW ] := 'MOVLW';
  PIC10InstName[i_RETLW ] := 'RETLW';
  PIC10InstName[i_SLEEP ] := 'SLEEP';
  PIC10InstName[i_XORLW ] := 'XORLW';
  PIC10InstName[i_OPTION] := 'OPTION';
  PIC10InstName[i_TRIS  ] := 'TRIS';
  //EXTENDED INSTRUCTIONS (Only for some Models)
  PIC10InstName[i_MOVLB ] := 'MOVLB';
  PIC10InstName[i_RETURN] := 'RETURN';
  PIC10InstName[i_RETFIE] := 'RETFIE';
  PIC10InstName[i_Inval ] := '<Inval>';

  //Inicializa Sintaxis de las instrucciones
  {Los valorees para la sintaxis significan:
  f->dirección de un registro en RAM (0..127)
  d->destino (W o F)
  b->número de bit (0..7)
  a->dirección destino (0..$7FF)
  k->literal byte (0..255)
  }
  //BYTE-ORIENTED FILE REGISTER OPERATIONS
  PIC10InstSyntax[i_ADDWF ] := 'fd';
  PIC10InstSyntax[i_ANDWF ] := 'fd';
  PIC10InstSyntax[i_CLRF  ] := 'f';
  PIC10InstSyntax[i_CLRW  ] := '';
  PIC10InstSyntax[i_COMF  ] := 'fd';
  PIC10InstSyntax[i_DECF  ] := 'fd';
  PIC10InstSyntax[i_DECFSZ] := 'fd';
  PIC10InstSyntax[i_INCF  ] := 'fd';
  PIC10InstSyntax[i_INCFSZ] := 'fd';
  PIC10InstSyntax[i_IORWF ] := 'fd';
  PIC10InstSyntax[i_MOVF  ] := 'fd';
  PIC10InstSyntax[i_MOVWF ] := 'f';
  PIC10InstSyntax[i_NOP   ] := '';
  PIC10InstSyntax[i_RLF   ] := 'fd';
  PIC10InstSyntax[i_RRF   ] := 'fd';
  PIC10InstSyntax[i_SUBWF ] := 'fd';
  PIC10InstSyntax[i_SWAPF ] := 'fd';
  PIC10InstSyntax[i_XORWF ] := 'fd';
  //BIT-ORIENTED FILE REGISTER OPERATIONS
  PIC10InstSyntax[i_BCF   ] := 'fb';
  PIC10InstSyntax[i_BSF   ] := 'fb';
  PIC10InstSyntax[i_BTFSC ] := 'fb';
  PIC10InstSyntax[i_BTFSS ] := 'fb';
  //LITERAL AND CONTROL OPERATIONS
  PIC10InstSyntax[i_ANDLW ] := 'k';
  PIC10InstSyntax[i_CALL  ] := 'a';
  PIC10InstSyntax[i_CLRWDT] := '';
  PIC10InstSyntax[i_GOTO ] := 'a';
  PIC10InstSyntax[i_IORLW ] := 'k';
  PIC10InstSyntax[i_MOVLW ] := 'k';
  PIC10InstSyntax[i_RETLW ] := 'k';
  PIC10InstSyntax[i_SLEEP ] := '';
  PIC10InstSyntax[i_XORLW ] := 'k';
  PIC10InstSyntax[i_OPTION] := '';
  PIC10InstSyntax[i_TRIS]   := 'f';
  //EXTENDED INSTRUCTIONS (Only for some Models)
  PIC10InstSyntax[i_MOVLB]  := 'k';
  PIC10InstSyntax[i_RETURN] := '';
  PIC10InstSyntax[i_RETFIE] := '';
  PIC10InstSyntax[i_Inval] := '<???>';
end;
initialization
  InitTables;
end.
{
}
