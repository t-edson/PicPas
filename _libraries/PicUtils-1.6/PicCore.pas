{PICCore

Contains basic definitions applicable to all PIC microcontroller Cores
                                         Created by Tito Hinostroza   28/04/2018
}
unit PicCore;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, LCLProc;
const
  PIC_MAX_PINES = 64;                //Max. number of pines for the package
type
  //Union to access bytes of a word
  TWordRec = record
    case byte of
      1 : (W : Word);
      {$IFDEF ENDIAN_LITTLE}
      2 : (L, H: Byte);
      {$ELSE}
      2 : (H, L: Byte);
      {$ENDIF}
    end;

  TPICCellState = (
     cs_impleSFR,   //Special function Registers. Can be used.
     cs_impleGPR,   //General Purpose Registers. Can be used.
     cs_unimplem    //Not implemented.
  );

  TPICPinType = (
    pptVcc,    //Alimentación
    pptGND,    //Tierra
    pptControl,//Pin de control
    pptPort,   //Puerto Entrada/Salida
    pptUnused  //Pin no usado
  );

  { TPICPin }
  //Model for a phisycal pin of the PIC
  TPICPin = object
    nam: string;      //Eqtiueta o nombre
    typ: TPICPinType; //Tipo de pin
    add: word;        //Dirección en RAM
    addTRIS: word;    //Dirección de bit de configuración
    bit: byte;        //Bit en RAM
    bitMask: byte;    //Bit mask
    bitmask2: byte;   //Inverted bit mask
    function GetLabel: string;
  end;
  TPICPinPtr = ^TPICPin;

type //Models for RAM memory
  { TPICRAMBank }
  {Represent a RAM memory bank of the PIC. }
  TPICRAMBank = object
    AddrStart : word;     //Address start of RAM bank
    AddrEnd   : word;     //Address end of RAM bank
  end;

  { TPICRamCell }
  {Modela a una dirección lógica de la memoria RAM. Se ha taratdo de hacer una
  definición eficiente de esta estructura para facilitar la implementación de
  simuladores en tiempo real. Podemos usar un tamaño mediano para este registro,
  porque no esperamos tener muchas celdas de RAM (<1K).}
  TPICRamCellPtr = ^TPICRamCell;
  TPICRamCell = object
  private
    Fvalue  : byte;     //value of the memory
    Fused   : byte;     //Bitmap. Indicates the used bits ($00->all free; $ff->all bits used.)
    FimplemAnd : byte;     //Bitmap. Defines unimplemented bits, set to zero
    FimplemOr  : byte;     //Bitmap. Defines unimplemented bits, sets to one
    function Getused: byte;
    function Getvalue: byte;
    procedure Setused(AValue: byte);
    procedure Setvalue(AValue: byte);
  public
    addr   : word;     //dirección física de memoria, en donde está la celda.
    name   : string;   //Name of the register (or variable)
    bitname: array[0..7] of string;  //Name of the bits.
    shared : byte;     //Used to share this register
    state  : TPICCellState;  //status of the cell
    mappedTo: TPICRamCellPtr;  //Indica que está mapeado a otra celda, de otra dirección
    property value: byte read Getvalue write Setvalue;
    property dvalue: byte read Fvalue write Fvalue;   //Direct access to "Fvalue".
    property implemAnd: byte read FimplemAnd write FimplemAnd;  //AND mask for implemented bits.
    property implemOr: byte read FimplemOr write FimplemOr;  //OR mask for implemented bits.
    property used: byte read Getused write Setused;
    function AvailGPR: boolean;
  end;

  TPICRam = array of TPICRamCell;
  TPICRamPtr = ^TPICRam;
  TPICRutExplorRAM = procedure(offs, bnk: byte; regPtr: TPICRamCellPtr) of object;

type  //Models for Flash memory

  TPICFlashCell = record
    value     : word;     //Value of the memory (OpCode)
    used      : boolean;  //Indicates if have been written
    curBnk    : byte;     {Current RAM bank where it's supposed this Opcode works.
                           The current bank can be different after executing this OpCode.}
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
  TPICFlashCellPtr = ^TPICFlashCell;

  { TPICFlashPage }
  {Represent a PIC memory page.}
  TPICFlashPage = object
    AddrStart : word;     //Address start of FLASH bank
    AddrEnd   : word;     //Address end of FLASH bank
  end;
  TPICFlash = array of TPICFlashCell;
  TPICFlashPtr = ^TPICFlash;

type

  { TPicCore }
  {Abcestor of all 8 bits PIC cores}
  TPicCore = class
  private
    FMaxFlash: integer;
    procedure SetMaxFlash(AValue: integer);
    function GetTokAddress(var str: string; delimiter: char): word;
  public   //Limits
    {This variables are set just one time. So they work as constant.}
    PICBANKSIZE : word;
    PICMAXRAM   : word;
    PICPAGESIZE : word;
    PICMAXFLASH : word;
  public   //General fields
    Model    : string;    //modelo de PIC
    frequen  : integer;   //frecuencia del reloj
    MaxFreq  : integer;   //máxima frecuencia del reloj
    //Propiedades que definen la arquitectura del PIC destino.
    NumBanks: byte;      //Número de bancos de RAM.
    NumPages: byte;      //Número de páginas de memoria Flash.
    MsjError: string;
    {Maximun numbers of Flash cells implemented (Generally used when the first Flash
    page is partially implemented). Applicable only when it's greater than zero}
    property MaxFlash: integer read FMaxFlash write SetMaxFlash;
  public   //Execution control
    nClck   : Int64;    //Contador de ciclos de reloj
    CommStop: boolean;  //Bandera para detener la ejecución
    OnExecutionMsg: procedure(message: string) of object;  //Genera mensaje en ejecución
  protected  //Generation of HEX files
    minUsed  : word;         //Dirección menor de la ROM usada
    maxUsed  : word;         //Dirección mayor de la ROM usdas
    hexLines : TStringList;  //Uusado para crear archivo *.hex
    function HexChecksum(const lin: string): string;
    procedure GenHexComm(comment: string);
    procedure GenHexExAdd(Data: word);
    procedure GenHexData(Address: word; Data: string);
    procedure GenHexEOF;
    function StrHexFlash(i1, i2: integer): string;
  public  //Memories
    flash    : TPICFlash;   //memoria Flash
    ram      : TPICRam;     //memoria RAM
    iFlash   : integer;   //puntero a la memoria Flash, para escribir
    function DisassemblerAt(addr: word; useVarName: boolean = false): string; virtual; abstract; //Desensambla la instrucción actual
  public  //RAM memory functions
    procedure ClearMemRAM;
    procedure DisableAllRAM;
    procedure SetStatRAM(i1, i2: word; status0: TPICCellState);
    procedure SetMappRAM(i1, i2: word; MappedTo: word);
    function SetStatRAMCom(strDef: string): boolean;
    function SetMappRAMCom(strDef: string): boolean;
    function MapRAMtoPIN(strDef: string): boolean;
    function HaveConsecGPR(const i, n: word; maxRam: word): boolean; //Indica si hay "n" bytes libres
    procedure UseConsecGPR(const i, n: word);  //Ocupa "n" bytes en la posición "i"
    procedure SetSharedUnused;
    procedure SetSharedUsed;
    function SetUnimpBITS(strDef: string): boolean;
    function SetUnimpBITS1(strDef: string): boolean;
  public  //Pins fields
    Npins    : byte;      //Number of pins
    pines    : array[1..PIC_MAX_PINES] of TPICPin;
    procedure ResetPins;
    procedure SetPin(pNumber: integer; pLabel: string; pType: TPICPinType);
    function SetPinName(strDef: string): boolean;
    procedure GetPinThev(nPin: integer; out vThev, rThev: Single);
    procedure SetNodePars(nPin: integer; const vNod, rNod: Single);
  public  //RAM name managment
    function NameRAM(const addr: word): string;
    function NameRAMbit(const addr: word; bit: byte): string;
    procedure SetNameRAM(const addr: word; const nam: string);  //Fija nombre a una celda de RAM
    procedure AddNameRAM(const addr: word; const nam: string);  //Agrega nombre a una celda de RAM
    procedure SetNameRAMbit(const addr: word; const bit: byte; const nam: string);  //Fija nombre a un bitde RAM
  public  //Execution control
    procedure AddBreakpoint(aPC: word);
    procedure ToggleBreakpoint(aPC: word);
    procedure Exec(aPC: word); virtual; abstract; //Ejecuta la instrucción en la dirección indicada.
    procedure Exec; virtual; abstract; //Ejecuta instrucción actual
    procedure ExecTo(endAdd: word); virtual; abstract; //Ejecuta hasta cierta dirección
    procedure ExecStep; virtual; abstract; //Execute one instruction considering CALL as one instruction
    procedure ExecNCycles(nCyc: integer; out stopped: boolean); virtual; abstract; //Ejecuta hasta cierta dirección
    procedure Reset; virtual; abstract;
    function ReadPC: dword; virtual; abstract;  //Defined DWORD to cover the 18F PC register
    procedure WritePC(AValue: dword); virtual; abstract;
  public  //Flash memory functions
    function UsedMemFlash: word;  //devuelve el total de memoria Flash usada
    procedure ClearMemFlash;
  public  //Others
    procedure addTopLabel(lbl: string);  //Add a comment to the ASM code
    procedure addTopComm(comm: string; replace: boolean = true);  //Add a comment to the ASM code
    procedure addSideComm(comm: string; before: boolean); //Add lateral comment to the ASM code
    procedure addPosInformation(rowSrc, colSrc: word; idFile: byte);
  public  //Initialization
    constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

{ TPICRamCell }
function TPICRamCell.Getused: byte;
begin
  if mappedTo = nil then begin
    //Celda independiente
    Result := Fused;
  end else begin
    //Reflected cell. Leemos la disponibilidad, de la celda origen.
    Result := mappedTo^.used;
  end;
end;
procedure TPICRamCell.Setused(AValue: byte);
begin
  if mappedTo = nil then begin
    //Celda independiente
    Fused := AValue;
  end else begin
    //Celda reflejada. Escribimos la disponibilidad, en la celda origen.
    mappedTo^.used := AValue;
  end;
end;
function TPICRamCell.Getvalue: byte;
begin
  if mappedTo = nil then begin
    //Celda independiente
    Result := Fvalue;
  end else begin
    //Reflected cell. Read the value from source.
    Result := mappedTo^.Fvalue;
  end;
end;
procedure TPICRamCell.Setvalue(AValue: byte);
begin
  if mappedTo = nil then begin
    //Celda independiente
    Fvalue := AValue;
  end else begin
    //Reflected cell. Write value to target.
    mappedTo^.Fvalue:= AValue;
  end;
end;
function TPICRamCell.AvailGPR: boolean;
{Indica si el registro es una dirección disponible en la memoria RAM.}
begin
  Result := (state = cs_impleGPR) and (mappedTo = nil);
end;
{ TPICPin }
function TPICPin.GetLabel: string;
{Devuelve una etiqueta para el pin}
begin
  case typ of
  pptUnused: Result := 'NC';
  else
    Result := nam;
  end;
end;
{ TPicCore }
procedure TPicCore.SetMaxFlash(AValue: integer);
begin
  if FMaxFlash = AValue then Exit;
  FMaxFlash := AValue;
end;
function TPicCore.GetTokAddress(var str: string; delimiter: char): word;
{Extract a number (address) from a string and delete until de delimiter.
If fail update the variable "MsjError".}
var
  p: SizeInt;
  n: Longint;
begin
  p := pos(delimiter, str);
  if p = 0 then begin
    MsjError := 'Expected "'+delimiter+'".';
    exit;
  end;
  //Have delimiter. Get number
  if not TryStrToInt('$'+copy(str,1,p-1), n) then begin
    MsjError := 'Wrong address.';
    exit;
  end;
  delete(str, 1, p);  //delete number and delimiter
  if n<0 then begin
    MsjError := 'Address cannot be negative.';
    exit;
  end;
  if n>$FFFF then begin
    MsjError := 'Address cannot be greater than $FFFF.';
    exit(0);
  end;
  Result := n;
end;
//Creación de archivo *.hex
function TPicCore.HexChecksum(const lin:string): string;
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
procedure TPicCore.GenHexComm(comment: string);
//Agrega una línea de comentario al archivo *.hex
begin
  hexLines.Add(';'+comment);
end;
procedure TPicCore.GenHexExAdd(Data: word);
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
procedure TPicCore.GenHexData(Address: word; Data: string);
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
procedure TPicCore.GenHexEOF;
//Agrega una línea de Extended Address al archivo *.hex
begin
  hexLines.Add(':00000001FF');
end;
function  TPicCore.StrHexFlash(i1, i2: integer): string;
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
//RAM memory functions
procedure TPicCore.ClearMemRAM;
{Limpia el contenido de la memoria}
var
  i: Integer;
begin
  for i:=0 to high(ram) do begin
    ram[i].dvalue := $00;
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
procedure TPicCore.DisableAllRAM;
{Inicia el estado de toda la memoria RAM física definida em el Modelo.
Solo debería usarse, para cuando se va a definir el hardware del dispositivo.}
var
  i: word;
begin
  for i:=0 to high(ram) do begin
    ram[i].addr     := i;
    ram[i].state    := cs_unimplem;
    ram[i].mappedTo := nil;
    ram[i].implemAnd:= $FF;  //Todos implementados, por defecto
    ram[i].implemOr := $00;  //Todos implementados, por defecto
  end;
  //Inicia estado de pines
  for i:=1 to high(pines) do begin
    pines[i].typ := pptUnused;
  end;
end;
procedure TPicCore.SetStatRAM(i1, i2: word; status0: TPICCellState);
{Inicia el campo State, de la memoria. Permite definir el estado real de la memoria RAM.
}
var
  i: Integer;
begin
  for i:=i1 to i2 do begin  //verifica 1 a 1, por seguridad
    if i>PICMAXRAM-1 then continue;  //protection
    ram[i].state := status0;
  end;
end;
procedure TPicCore.SetMappRAM(i1, i2: word; MappedTo: word);
{Inicia el campo State, de la memoria. Permite definir el estado real de la memoria RAM.
"MappedTo", indica el número de banco al cual está mapeada la sección de memoria indicada,
cuando se pone "status0" en "cs_mapToBnk". En los otrso estados no es útil.}
var
  i: Integer;
begin
  for i:= i1 to i2 do begin  //verifica 1 a 1, por seguridad
    if i>PICMAXRAM-1 then continue;  //protection
    ram[i].mappedTo := @ram[MappedTo];
    inc(MappedTo)
  end;
end;
function TPicCore.SetStatRAMCom(strDef: string): boolean;
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
  state: TPICCellState;
  com, str: String;
  bnkTar: Byte;
begin
  Result := true;
  coms:= TStringList.Create;
  try
    coms.Delimiter := ',';
    coms.DelimitedText := strDef;
    for str in coms do begin
      com := UpCase(trim(str));
      if com='' then continue;
      //Find Address1
      add1 := GetTokAddress(com, '-');
      if MsjError<>'' then begin
        MsjError := 'Memory definition syntax error: ' + MsjError;
        exit(false);
      end;
      //Find Address1
      add2 := GetTokAddress(com, ':');
      if MsjError<>'' then begin
        MsjError := 'Memory definition syntax error: ' + MsjError;
        exit(false);
      end;
      case copy(com,1,3) of
      'SFR': state := cs_impleSFR;
      'GPR': state := cs_impleGPR;
      'NIM': state := cs_unimplem;
      else
        MsjError := 'Memory definition syntax error: Expected SFR, GPR or NIM';
        exit(false);
      end;
      //Ya se tienen los parámetros, para definir la memoria
      SetStatRAM(add1, add2, state);
      //Verifica si hay parámetros adicionales
      delete(com, 1, 3);
      if com='' then begin
        //No hay
      end else if com=':ALL' then begin
        //Set the state for all Banks
        if PICBANKSIZE = $80 then begin //14 bits instructions PIC
          add1 := add1 and $7F;  //Fix to bank0
          add2 := add2 and $7F;  //Fix to bank0
          for bnkTar:=0 to NumBanks do begin
            SetStatRAM($080*bnkTar+add1, $080*bnkTar+add2, state);
          end;
        end else if PICBANKSIZE = $20 then begin //12 bits instructions PIC
          add1 := add1 and $1F;  //Fix to bank0
          add2 := add2 and $1F;  //Fix to bank0
          for bnkTar:=0 to NumBanks do begin
            SetStatRAM($020*bnkTar+add1, $020*bnkTar+add2, state);
          end;
        end;
      end else if com=':ALLMAPPED' then begin
        //Mapp all banks to the bank0
        if PICBANKSIZE = $80 then begin //14 bits instructions PIC
          add1 := add1 and $7F;  //Fix to bank0
          add2 := add2 and $7F;  //Fix to bank0
          //State
          for bnkTar:=0 to NumBanks do begin
            SetStatRAM($080*bnkTar+add1, $080*bnkTar+add2, state);
          end;
          //Map from bank1
          for bnkTar:=1 to NumBanks do begin
            SetMappRAM($080*bnkTar+add1, $080*bnkTar+add2, add1);
          end;
        end else if PICBANKSIZE = $20 then begin //12 bits instructions PIC
          add1 := add1 and $1F;  //Fix to bank0
          add2 := add2 and $1F;  //Fix to bank0
          //State
          for bnkTar:=0 to NumBanks do begin
            SetStatRAM($020*bnkTar+add1, $020*bnkTar+add2, state);
          end;
          //Map from bank1
          for bnkTar:=1 to NumBanks do begin
            SetMappRAM($020*bnkTar+add1, $020*bnkTar+add2, add1);
          end;
        end;
      end else begin
        MsjError := 'Memory definition error';
        exit(false);
      end;
    end;
  finally
    coms.Destroy;
  end;
end;
function TPicCore.SetMappRAMCom(strDef: string): boolean;
{Define RAM memory, mapped to other address.
String defintiom, have the format:
<command 1>, <command 2>, ...
Each command have the format:
<addresIni>-<addressFin>:<bank where it's mapped>
An example of string definition ei:
   '000-01F:bnk0, 020-07F:bnk1'
On error this function return FALSE, and the error menssage in MsjError.
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
      //Find Address1
      add1 := GetTokAddress(com, '-');
      if MsjError<>'' then begin
        MsjError := 'Memory mapping syntax error: ' + MsjError;
        exit(false);
      end;
      //Find Address1
      add2 := GetTokAddress(com, ':');
      if MsjError<>'' then begin
        MsjError := 'Memory mapping syntax error: ' + MsjError;
        exit(false);
      end;
      bnkTarStr := com;
      if copy(bnkTarStr,1,3)<>'BNK' then begin
        MsjError := 'Memory mapping syntax error: Expected "bnk0", ...';
        exit(false);
      end;
      if not (bnkTarStr[4] in ['0'..'3','A'..'F']) then begin
        MsjError := 'Memory mapping syntax error: Expected "bnk0", ...';
        exit(false);
      end;
      bnkTar := ord(bnkTarStr[4])-48;  //convierte a número
      //We already have the parameters to set the mapping
      if PICBANKSIZE = $80 then begin
        //This apply for 14 bits instructions PIC
        addTar := (add1 and $7F) + $080*bnkTar;
      end else if PICBANKSIZE = $20 then begin
        //This apply for 12 bits instructions PIC
        addTar := (add1 and $1F) or $020*bnkTar;
      end else begin
        MsjError := 'Invalid bank size for this device.';
        exit(false);
      end;
      if add1 = addTar then begin
        //Mapped to itself?
        MsjError := 'Cannot map to the same address.';
        exit(false);
      end;
      SetMappRAM(add1, add2, addTar);
    end;
  finally
    coms.Destroy;
  end;
end;
function TPicCore.MapRAMtoPIN(strDef: string): boolean;
{Mapea puertos de memoria RAM a pines físicos del dispositivo. Útil para la simulación
La cadena de definición, tiene el formato:
<dirección>:<lista de asociaciones>
Cada asociación tiene el formato:
<bit Number>-<pin Number>
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
      bit := bit and %111; //protección
      pines[pin].add := add1;
      pines[pin].bit := bit;
      pines[pin].bitMask := 1<<bit;        //Calculate mask here, to speed execution
      pines[pin].bitMask2 := byte(not(1<<bit));  //Inverted mask
      pines[pin].addTRIS := add1+$80;  {Calcula dirección de TRIS. NOTAR que esto solo
                                        es útil para la familia de Rango medio}
      pines[pin].typ := pptPort;
      ramName := ram[add1].name;
      if ramName='' then ramName := 'PORT';
      pines[pin].nam :=  ramName + '.' + IntToStr(bit);  //Nombre por defecto
    end;
  finally
    coms.Destroy;
  end;
end;
procedure TPicCore.ResetPins;
{Reset the pins of the device.}
var
  i: byte;
begin
  for i:=1 to Npins do begin
    pines[i].nam := ' ';
    pines[i].typ := pptUnused;
  end;
end;
procedure TPicCore.SetPin(pNumber: integer; pLabel: string; pType: TPICPinType);
begin
  if pNumber>PIC_MAX_PINES then exit;
  pines[pNumber].nam := pLabel;
  pines[pNumber].typ := pType;
end;
function TPicCore.SetPinName(strDef: string): boolean;
{Define the name for a specified Pin of the microcontroller, using a string.
"strDef" have the format:
<pin number>:<name of the pin>
On error this function return FALSE, and the error menssage in MsjError.
}
var
  com, pinName: String;
  pNumber: integer;
  pcol: SizeInt;
begin
  com := UpCase(trim(strDef));
  if com='' then exit;
  pcol := Pos(':', strDef);
  if pcol=0 then begin
    MsjError := 'SetPinName: Expected ":".';
    exit(false);
  end;
  //"com" must have the correct format
  if not TryStrToInt( copy(com, 1, pcol-1) , pNumber) then begin
    MsjError := 'SetPinName: Wrong Pin Number.';
    exit(false);
  end;
  pinName :=copy(com, pcol+1, 32);  //limited to 32
  SetPin(pNumber, pinName, pptControl);
end;
procedure TPicCore.GetPinThev(nPin: integer; out vThev, rThev: Single);
{Devuelve el modelo de Thevening del pin de salida, que incluye el voltaje de
la fuente (en voltios) y la resistencia interna (en ohmios).
Hubiera sido más sencillo devolver solo el nivel lógico de salida (alto o bajo),
pero aquí hemos querido ir un poco más lejos.}
const
  VCC   = 5;      //Source voltage
  MAX_I = 0.030;  //Max assumed current
  R_INT: Single = VCC / MAX_I;  //Internal resistance
begin
  if nPin>High(pines) then exit;
  case pines[nPin].typ of
  pptUnused: begin
    vThev := 0;
    rThev := 1e+10;  //Alta impedancia
  end;
  pptGND: begin
    vThev := 0;
    rThev := 0;
  end;
  pptVcc: begin
    vThev := VCC;
    rThev := 0;
  end;
  pptControl: begin  //Pin de Control
    vThev := 0;
    rThev := 1e+10;  //Alta impedancia
    //Habría que definir bien este tipo de pines
  end;
  pptPort: begin  //Input/Output port.
    //It's supposed to to bemapped at RAM
    with pines[nPin] do begin
      //First verify the stae of pin (input/utput)
      if ram[addTRIS].value and bitMask = 0 then begin
        //Is output
        if ram[add].value and bitMask = 0 then begin
          //Bit to 0 logic
          vThev := 0;
          rThev := R_INT;
        end else begin
          //Bit to 1 logic.
          vThev := VCC;
          rThev := R_INT;
        end;
      end else begin
        //In input
        vThev := 0;
        rThev := 1e+9;  //High impedance
      end;
    end;
  end;
  else
    vThev := 0;
    rThev := 1e+10;  //High impedance
  end;
end;
procedure TPicCore.SetNodePars(nPin: integer; const vNod, rNod: Single);
{Write the value of voltage and impedance to the physical pin of the device.}
begin
  if pines[nPin].typ = pptPort then begin
    //Only valid for Ports
    with pines[nPin] do begin
      //First verify the stae of pin (input/utput)
      if ram[addTRIS].value and bitMask = 0 then begin
        //Is output
        //Doesn't affect in RAM trying to set the voltage of an output pin.
      end else begin
        if vNod > 2.5 then begin
          //Set bit
          ram[add].value := ram[add].value or bitMask;
        end else begin
          //Clear bit
          ram[add].value := ram[add].value and bitMask2;
        end;
      end;
    end;
  end;
end;
function TPicCore.HaveConsecGPR(const i, n: word; maxRam: word): boolean;
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
procedure TPicCore.UseConsecGPR(const i, n: word);
{Marca "n" bytes como usados en la posición de memoria "i", en la RAM.
 Debe haberse verificado previamente que los parámetros son válidos, porque aquí no
 se hará ninguna verificación.}
var j: word;
begin
  for j:=i to i+n-1 do begin
    ram[j].used:=255;  //todos los bits
  end;
end;
procedure TPicCore.SetSharedUnused;
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
procedure TPicCore.SetSharedUsed;
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
function TPicCore.SetUnimpBITS(strDef: string): boolean;
{Set unimplemented bits (read as 0) in a specific positions of RAM.
The synyax of strDef is:
  $SET_UNIMP_BITS <command list>
The command syntax is:
  <byte address>:<bit mask>
A mask of $00 will disable all the bits.
}
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
      //Find Address1
      add1 := GetTokAddress(com, ':');
      if MsjError<>'' then begin
        MsjError := 'Syntax error: ' + MsjError;
        exit(false);
      end;
      if add1>high(ram) then begin
        MsjError := 'Address exceeds limit for this device.';
        exit(false);
      end;
      mskBits := com;
      if not TryStrToInt('$'+mskBits, n) then begin
        MsjError := 'Syntax error: Wrong mask.';
        exit(false);
      end;
      mskBitsN := n and $FF;  //Se supone que nunca será > 255
      //Ya se tienen los parámetros, para definir el mapeo
      ram[add1].implemAnd := mskBitsN;
    end;
  finally
    coms.Destroy;
  end;
end;
function TPicCore.SetUnimpBITS1(strDef: string): boolean;
{Set unimplemented bits (read as 1) in a specific positions of RAM.
The synyax of strDef is:
  $SET_UNIMP_BITS <command list>
The command syntax is:
  <byte address>:<bit mask>
A mask of $11 will disable all the bits.
}
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
      //Find Address1
      add1 := GetTokAddress(com, ':');
      if MsjError<>'' then begin
        MsjError := 'Syntax error: ' + MsjError;
        exit(false);
      end;
      if add1>high(ram) then begin
        MsjError := 'Address exceeds limit for this device.';
        exit(false);
      end;
      mskBits := com;
      if not TryStrToInt('$'+mskBits, n) then begin
        MsjError := 'Syntax error: Wrong mask.';
        exit(false);
      end;
      mskBitsN := n and $FF;  //Se supone que nunca será > 255
      //Ya se tienen los parámetros, para definir el mapeo
      ram[add1].implemOr := mskBitsN;
    end;
  finally
    coms.Destroy;
  end;
end;
//RAM name managment
function TPicCore.NameRAM(const addr: word): string;
{Devuelve el nombre de una celda de la memoria RAM.}
begin
  Result := ram[addr].name;
end;
function TPicCore.NameRAMbit(const addr: word; bit: byte): string;
begin
  Result := ram[addr].bitname[bit];
end;
procedure TPicCore.SetNameRAM(const addr: word; const nam: string
  );
{Escribe en el campo "name" de la RAM en la psoición indicada}
begin
  if addr > high(ram) then begin
    MsjError := 'Address exceed limits of this device.';
    exit;
  end;
  ram[addr].name:=nam;
end;
procedure TPicCore.AddNameRAM(const addr: word; const nam: string);
{Escribe en el campo "name" de la RAM en la psoición indicada. Si ya existía un nombre,
lo argega después de una coma.}
begin
  if ram[addr].name = '' then begin
    ram[addr].name:=nam;
  end else begin
    ram[addr].name+=','+nam;
  end;
end;
procedure TPicCore.SetNameRAMbit(const addr: word; const bit: byte; const nam: string);
begin
  if (bit>7) then exit;
  if addr > high(ram) then begin
    MsjError := 'Address exceed limits of this device.';
    exit;
  end;
  ram[addr].bitname[bit] := nam;
end;
//Execution control
procedure TPicCore.AddBreakpoint(aPC: word);
//Agrega un punto de interrupción
begin
  if aPC>=PICMAXFLASH then exit;
  flash[aPC].breakPnt := true;
end;
procedure TPicCore.ToggleBreakpoint(aPC: word);
//COnmuta el estado del Punto de Interrupción, en la posición indicada
begin
  if aPC>=PICMAXFLASH then exit;
  flash[aPC].breakPnt := not flash[aPC].breakPnt;
end;
//FLASH memory functions
function TPicCore.UsedMemFlash: word;
var
  i: Integer;
begin
  Result := 0;
  for i:=$0000 to PICMAXFLASH-1 do begin
    if flash[i].used then inc(Result);
  end;
end;
procedure TPicCore.ClearMemFlash;
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
procedure TPicCore.addTopLabel(lbl: string);
begin
  flash[iFlash].topLabel := lbl;
end;
procedure TPicCore.addTopComm(comm: string; replace: boolean);
{Agrega un comentario de línea al código en la posición de memoria actual}
begin
  if replace then begin
    flash[iFlash].topComment := comm;
  end else begin
    flash[iFlash].topComment := flash[iFlash].topComment + comm;
  end;
end;
procedure TPicCore.addSideComm(comm: string; before: boolean);
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
procedure TPicCore.addPosInformation(rowSrc, colSrc: word; idFile: byte);
{Agrega information de la posición en el codigo fuente, a la posición actual de la
memoria flash.}
begin
  flash[iFlash].rowSrc := rowSrc;
  flash[iFlash].colSrc := colSrc;
  flash[iFlash].idFile := idFile;
end;
//Initialization
constructor TPicCore.Create;
begin
  hexLines := TStringList.Create;
  frequen := 4000000;    //4MHz
end;
destructor TPicCore.Destroy;
begin
  hexLines.Destroy;
  inherited Destroy;
end;

initialization
end.

