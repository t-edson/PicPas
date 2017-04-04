{Unidad que agrega campos necesarios a la clase TCompilerBase, para la generación de
código con el PIC16F.}
unit GenCodPic;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, XPresParserPIC, XpresElementsPIC, Pic16Utils, XpresTypes,
  MisUtils;
type
  TGenCodPic = class(TCompilerBase)
  protected
    pic : TPIC16;    //objeto PIC de la serie 16
    curBank: Byte;   //Banco RAM actual
    procedure CodAsmFD(const inst: TPIC16Inst; const f: byte; d: TPIC16destin
      );
    procedure CodAsmK(const inst: TPIC16Inst; const k: byte);
    procedure _ADDLW(const k: word);
    procedure _ADDWF(const f: byte; d: TPIC16destin);
    procedure _ANDLW(const k: word);
    procedure _ANDWF(const f: byte; d: TPIC16destin);
    procedure _BANKSEL(targetBank: byte);
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
  protected  //Manejo de variables
    {Estas rutinas estarían emjor ubicadas en TCompilerBase, pero como dependen del
    objeto "pic", se colocan mejor aquí.}
    function CreateVar(const varName: string; typ: ttype; absAdd: integer=-1;
      absBit: integer=-1): TxpEleVar;
    function CreateVar(varName, varType: string; absAdd: integer=-1; absBit: integer
      =-1): TxpEleVar;
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

//Variables que deden ser accesibles al generador de código
//var
//  i_w2 : integer;  //índice a la variable temporal byte
//  i_w3 : integer;  //índice a la variable temporal byte
//  i_w4 : integer;  //índice a la variable temporal byte
//Funciones de acceso rápido a métodos del compilador. Se usan para ayudar al geenrador de código.
//rutinas generales para la codificación
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
procedure TGenCodPic._BANKSEL(targetBank: byte);
{Verifica si se está en el banco deseado, de no ser así geenra las instrucciones
 para el cambio de banco.}
begin
  if targetBank = curBank then
    exit;
  //se está en un banco diferente
  case curBank of
  0: case targetBank of
       1: begin
         _BSF(_STATUS, _RP0);
         curBank:=1;
       end;
       2: begin
         _BSF(_STATUS, _RP1);
         curBank:=2;
       end;
       3: begin
         _BSF(_STATUS, _RP0);
         _BSF(_STATUS, _RP1);
         curBank:=3;
       end;
     end;
  1: case targetBank of
       0: begin
         _BCF(_STATUS, _RP0);
         curBank:=0;
       end;
       2: begin
         _BSF(_STATUS, _RP1);
         _BCF(_STATUS, _RP0);
         curBank:=2;
       end;
       3: begin
         _BSF(_STATUS, _RP1);
         curBank:=3;
       end;
     end;
  2: case targetBank of
       0: begin
         _BCF(_STATUS, _RP1);
         curBank:=0;
       end;
       1: begin
         _BCF(_STATUS, _RP1);
         _BSF(_STATUS, _RP0);
         curBank:=1;
       end;
       3: begin
         _BSF(_STATUS, _RP0);
         curBank:=3;
       end;
     end;
  3: case targetBank of
       0: begin
         _BCF(_STATUS, _RP1);
         _BCF(_STATUS, _RP0);
         curBank:=0;
       end;
       1: begin
         _BCF(_STATUS, _RP1);
         curBank:=1;
       end;
       2: begin
         _BCF(_STATUS, _RP0);
         curBank:=2;
       end;
     end;
  // Este caso es equivalentea decir "no sé en qué banco estoy"
  255: case targetBank of
       0: begin
         _BCF(_STATUS, _RP1);
         _BCF(_STATUS, _RP0);
         curBank:=0;
       end;
       1: begin
         _BCF(_STATUS, _RP1);
         _BSF(_STATUS, _RP0);
         curBank:=1;
       end;
       2: begin
         _BSF(_STATUS, _RP1);
         _BCF(_STATUS, _RP0);
         curBank:=2;
       end;
       3: begin
         _BSF(_STATUS, _RP1);
         _BSF(_STATUS, _RP0);
         curBank:=3;
       end;
     end;
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
    //caso normal
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
    //se debe crear en una posición absoluta
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
  r.name:=varName;
  r.typ := typ;   //fija  referencia a tipo
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
function TGenCodPic.CreateVar(varName, varType: string;
         absAdd: integer = -1; absBit: integer = -1): TxpEleVar;
{Agrega una variable a la tabla de variables.}
var
  typ: ttype;
  varTypeL: String;
begin
  //Verifica el tipo
  varTypeL := LowerCase(varType);
  typ := FindType(varTypeL);
  if typ = nil then begin
    GenError('Undefined type "%s"', [varType]);
    exit;
  end;
  Result := CreateVar(varName, typ, absAdd ,absBit);
  //puede salir con error
end;

procedure SetLanguage(lang: string);
begin
  case lang of
  'en': begin
    dicClear;  //it's yet in English
  end;
  'es': begin
    //Update messages
    dicSet('Size of data not supported.', 'Tamaño de dato no soportado.');
    dicSet('RAM memory is full.', 'Memoria RAM agotada.');
    dicSet('Duplicated identifier: "%s"', 'Identificador duplicado: "%s"');
    dicSet('Undefined type "%s"', 'Tipo "%s" no definido.');
  end;
  end;
end;

end.

