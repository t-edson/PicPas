{Unidad que agrega campos necesarios a la clase TCompilerBase, para la generación de
código con el PIC16F.}
unit GenCodPic;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, XPresParserPIC, XpresElementsPIC, Pic16Utils, XpresTypes,
  MisUtils, LCLType, LCLProc, fgl;
const
  STACK_SIZE = 8;  //tamaño de pila para subrutinas en el PIC
  MAX_REGS_AUX = 4;    //cantidad máxima de registros a usar
  MAX_REGS_STACK = 4;   //cantidad máxima de registros a usar en la pila

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
  private
    pic: TPIC16;
  public
    assigned: boolean;  //indica si tiene una dirección física asignada
    used   : boolean;  //Indica si está usado. Se le debe actualizr después de una operación.
    offs   : byte;     //Desplazamiento en memoria
    bank   : byte;     //Banco del registro
    typ    : TPicRegType;  //Tipo de registro
  public  //Inicialización
    MsjError: string;
    procedure AssignRAM(regName: string);  //Asigna a una dirección física
    constructor Create(pic0: TPIC16);
  end;
  TPicRegister_list = specialize TFPGObjectList<TPicRegister>; //lista de registros

  { TGenCodPic }

  TGenCodPic = class(TCompilerBase)
  private
    {Estructura de pila. Se crea una tabla de las direcciones a usar (no necesariamente
    consecutivas, porque puede haber variables ABSOLUTE) como pila. El tamaño de la pila a
    usar no es fijo, sino que se crea de acuerdo al tamaño requerido para la evaluación de
    expresiones. Puede incluso ser de tamaño cero.}
    linRep : string;   //línea para generar de reporte
    procedure ProcByteUsed(offs, bnk: byte; regPtr: TPIC16RamCellPtr);
  protected
    pic    : TPIC16;       //Objeto PIC de la serie 16.
    w      : TPicRegister; //Se declara como Registro en RAM.
    sp     : integer;      //Puntero a la estructura de pila. Apunta a la posición libre
    spSize : integer;      //Tamaño actual de pila.
    H      : TPicRegister; //Registros de trabajo. Se crean siempre.
    registerList: TPicRegister_list;  //lista de registros de trabajo y auxiliares
    registerStack: TPicRegister_list; //lista de registros de pila
    stackTop: integer;   //índice al límite superior de la pila
    procedure SetResult(tipByte: TType; CatOp: TCatOperan);
    procedure PutComLine(cmt: string);
    procedure PutComm(cmt: string);
    function ReportRAMusage: string;
  private//Rutinas de gestión de memoria de bajo nivel
    function CreateByteRegister(RegType: TPicRegType): TPicRegister;
    function NewAuxRegisterByte: TPicRegister;
  protected  //Rutinas de gestión de memoria
    //Manejo de memoria para registros
    function GetAuxRegisterByte: TPicRegister;
    function PushByte(out reg: TPicRegister): boolean;
    function FreeByte(var reg: TPicRegister): boolean;
    procedure RequireW;
    procedure RequireH(preserve: boolean = true);
    procedure RequireResultByte;
    procedure RequireResultWord;
    //Manejo de variables
    {Estas rutinas estarían mejor ubicadas en TCompilerBase, pero como dependen del
    objeto "pic", se colocan mejor aquí.}
    function CreateVar(const varName: string; typ: ttype; absAdd: integer=-1;
      absBit: integer=-1): TxpEleVar;
  protected  //Variables de expresión.
    {Estas variables, se inician al inicio de cada expresión y su valor es válido
    hasta el final de la expresión.}
    LastCatOp  : TCatOperan; //Categoría de operando, de la subexpresión anterior.
    LastBank   : byte;       //Banco de RAM, de la subexpresión anterior.
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
  public  //Inicialización
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
  tipBit : TType;
  tipBool: TType;     //Booleanos
  tipByte: TType;     //número sin signo
  tipWord: TType;     //número sin signo
//  tipChr : Ttype;   //un caracter

implementation

{ TPicRegister }
procedure TPicRegister.AssignRAM(regName: string);
//Asocia a una dirección física de la memoria del PIC.
//Si encuentra error, devuelve el mensaje de error en "MsjError"
begin
  {Esta dirección física, la mantendrá este registro hasta el final de la compilación
  y en teoría, hasta el final de la ejecución de programa en el PIC.}
  if not pic.GetFreeByte(offs, bank) then begin
    MsjError := 'RAM memory is full.';
    exit;
  end;
  pic.SetNameRAM(offs, bank, regName);  //pone nombrea registro
  assigned := true;  //marca como que ya tiene memoria asignada
  used := false;  //aún no se usa
end;
constructor TPicRegister.Create(pic0: TPIC16);
begin
  pic := pic0;
end;
procedure TGenCodPic.ProcByteUsed(offs, bnk: byte; regPtr: TPIC16RamCellPtr);
begin
  linRep := linRep + regPtr^.name +
            ' DB ' + 'bnk'+ IntToStr(bnk) + ':$' + IntToHex(offs, 3) + LineEnding;
end;
procedure TGenCodPic.SetResult(tipByte: TType; CatOp: TCatOperan);
begin
  res.typ := tipByte;
  res.catOp := CatOp;
  LastCatOp := CatOp;  {Guarda la categoría, para que la siguiente instrucción sepa
                       cuál fue la categoría de la última expresión}
end;
//Rutinas de Gestión de memoria
function TGenCodPic.CreateByteRegister(RegType: TPicRegType): TPicRegister;
{Crea una nueva entrada para registro en registerList[], pero no le asigna memoria.
 Si encuentra error, devuelve NIL. Este debería ser el único punto de entrada
para agregar un nuevo registro a registerList.}
var
  reg: TPicRegister;
begin
  //Agrega un nuevo objeto TPicRegister a la lista;
  reg := TPicRegister.Create(pic);  //Crea objeto
  reg.typ := RegType;    //asigna tipo
  registerList.Add(reg);   //agrega a lista
  if registerList.Count > MAX_REGS_AUX then begin
    //Se asume que se desbordó la memoria evaluando a alguna expresión
    GenError('Very complex expression. To simplify.');
    exit(nil);
  end;
  Result := reg;   //devuelve referencia
end;
function TGenCodPic.NewAuxRegisterByte: TPicRegister;
{Asigna espacio en memoria y devuelve la referencia a un registro de 8 bits, en la
memoria del PIC. Si hay error, devuelve NIL. }
var
  reg: TPicRegister;
  regName: String;
begin
  //Agrega un nuevo objeto TPicRegister a la lista;
  reg := CreateByteRegister(prtAuxReg);
  if reg = nil then exit(nil);  //hubo errir
  regName := 'aux'+IntToSTr(registerList.Count);
  reg.AssignRAM(regName);   //Asigna memoria. Puede generar error.
//debugln('>agregando registro: ' + regName);
  if reg.MsjError<>'' then GenError(reg.MsjError);
  Result := reg;   //Devuelve la referencia
end;
function TGenCodPic.GetAuxRegisterByte: TPicRegister;
{Devuelve la dirección de un registro de trabajo libre. Si nop encuentra alguno, lo crea.}
var
  reg: TPicRegister;
begin
  //Busca en los registros creados
  {Notar que no se incluye en la búsqueda a los registros de trabajo. Esto es por un
  tema de orden, si bien podría ser factible, permitir usar algún registro de trabajo no
  usado, como registro auxiliar.}
  for reg in registerList do begin
    //Se supone que todos los registros auxiliares, estarán siempre asignados
    if (reg.typ = prtAuxReg) and not reg.used then
      exit(reg);
  end;
  //No encontró ninguno libre, crea uno en memoria
  Result := NewAuxRegisterByte;  //Puede generar error
end;
function TGenCodPic.PushByte(out reg: TPicRegister): boolean;
{Pone un registro de un byte, en la pila, de modo que se pueda luego acceder con
FreeByte(). Si hay un error, devuelve FALSE y NIL en "reg".
Notar que esta no es una pila de memoria en el PIC, sino una emulación de pila
en el compilador.}
var
  reg0: TPicRegister;
  regName: String;
begin
  //Validación
  if stackTop>MAX_REGS_STACK then begin
    //Se asume que se desbordó la memoria evaluando a alguna expresión
    GenError('Very complex expression. To simplify.');
    reg := nil;
    exit(false);
  end;
  if stackTop>registerStack.Count-1 then begin
    //Apunta a una posición vacía. hay qie agregar
    //Agrega un nuevo objeto TPicRegister a la lista;
    reg0 := TPicRegister.Create(pic);  //Crea objeto
    reg0.typ := prtStkReg;    //asigna tipo
    registerStack.Add(reg0);   //agrega a lista
    regName := 'stk'+IntToSTr(registerList.Count);
    reg.AssignRAM(regName);   //Asigna memoria. Puede generar error.
    if reg.MsjError<>'' then begin
      GenError(reg.MsjError);
      reg := nil;
      exit(false);
    end;
  end;
  reg := registerStack[stackTop];  //toma registro
  reg.used := true;   //lo marca
  inc(stackTop);  //actualiza
end;

function TGenCodPic.FreeByte(var reg: TPicRegister): boolean;
{Libera el último byte, que se pidió a la RAM. Devuelve en "reg", la dirección del último
 byte pedido. Si hubo error, devuelve FALSE.
 Liberarlos significa que estarán disponibles, para la siguiente vez que se pidan}
var
  i: Integer;
begin
   if registerList.Count<1 then begin
     GenError('Desborde de pila.');
     exit(false);
   end;
   //Busca el último usado, porque puede haber reg. no usdos en la parte superior.
   i := registerList.Count-1;  //Empieza con el último
   while i>=0 do begin
     if (registerList[i].typ = prtStkReg) and  registerList[i].used then begin
       //Encontró
       reg := registerList[i];
       reg.used := false;   //marca como no usado
       exit(true);
     end;
     dec(i);
   end;
   //No enocntró alguno usado
   reg := nil;
   GenError('Desborde de pila.');
   exit(false);
end;
procedure TGenCodPic.RequireW;
{Indica que se va a utilizar el acumulador. De encontrarse ocupado,
se pondrá en la pila. Si no hay espacio, genera error}
var
  tmpReg: TPicRegister;
begin
  if W.used then begin
    //Ya lo usó la subexpresión anterior (seguro que fue una expresión de algún tipo)
    //Pide una posición de memoria. Notar que puede ser un reg. de trabajo.
    tmpReg := GetAuxRegisterByte;
    //guarda W { TODO : Falta validar el banco }
    _MOVWF(tmpReg.offs);PutComm(';guarda W');
    tmpReg.used := true;
  end;
  w.used := true;   //Lo marca como indicando que se va a ocupar
end;
procedure TGenCodPic.RequireH(preserve: boolean = true);
{Indica que va a usar el registro H.}
var
  tmpReg: TPicRegister;
begin
  if not H.assigned then begin
    //Ni siquiera tiene dirección asignada. Primero hay que ubicarlo en memoria.
    H.AssignRAM('_H');
  end else begin
    //Ya existe.
    if preserve and H.used then begin
      //Ya lo usó la subexpresión anterior (seguro que fue una expresión de algún tipo)
      tmpReg := GetAuxRegisterByte;   //pide una posición de memoria
      //guarda H { TODO : Falta validar el banco }
      _MOVF(H.offs, toW);PutComm(';guarda H');
      _MOVWF(tmpReg.offs);PutComm(';guarda H');
      tmpReg.used := true;   //marca
    end;
  end;
  H.used := true;  //lo marca como que lo va a usar
end;
procedure TGenCodPic.RequireResultByte;
{Indica que se va a devolver un resultado de tipo Byte, en la generación de código.
Debe llamarse siempre, antes de generar código para la subexpresión.}
var
  tmpReg: TPicRegister;
begin
  {Los resultados Byte, se devuelven en W, así qie deebemos asegurarno de que el
   registro W, se encuentre libre para poder usarlo en esta subexpresión.}
  if W.used then begin
    //Ya lo usó la subexpresión anterior (seguro que fue una expresión de algún tipo)
    //Pide una posición de memoria. Notar que puede ser un reg. de trabajo.
    tmpReg := GetAuxRegisterByte;
    //guarda W { TODO : Falta validar el banco }
    _MOVWF(tmpReg.offs);PutComm(';guarda W');
  end;
  W.used := true;  //indica que lo va a usar
//  SetResult(tipByte, CatOp);
end;
procedure TGenCodPic.RequireResultWord;
{Indica que se va a devolver un resultado de tipo Word, en la generación de código.
Debe llamarse siempre, antes de generar código para la subexpresión.}
var
  tmpReg: TPicRegister;
begin
  {Los resultados Word, se devuelven en (W,H), así qie deebemos asegurarno de que los
   registros W y H, se encuentren libres para poder usarlo en esta subexpresión.}
  if W.used then begin
    //Ya lo usó la subexpresión anterior (seguro que fue una expresión de algún tipo)
    tmpReg := GetAuxRegisterByte;   //pide una posición de memoria
    //guarda W { TODO : Falta validar el banco }
    _MOVWF(tmpReg.offs);PutComm(';guarda W');
    tmpReg.used := true;
  end;
  W.used := true;  //indica que lo va a usar
  RequireH;
//  SetResult(tipWord, CatOp);
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
  if targetBank = CurrBank then
    exit;
  //se está en un banco diferente
  case CurrBank of
  0: case targetBank of
       1: begin
         _BSF(_STATUS, _RP0);
         CurrBank:=1;
       end;
       2: begin
         _BSF(_STATUS, _RP1);
         CurrBank:=2;
       end;
       3: begin
         _BSF(_STATUS, _RP0);
         _BSF(_STATUS, _RP1);
         CurrBank:=3;
       end;
     end;
  1: case targetBank of
       0: begin
         _BCF(_STATUS, _RP0);
         CurrBank:=0;
       end;
       2: begin
         _BSF(_STATUS, _RP1);
         _BCF(_STATUS, _RP0);
         CurrBank:=2;
       end;
       3: begin
         _BSF(_STATUS, _RP1);
         CurrBank:=3;
       end;
     end;
  2: case targetBank of
       0: begin
         _BCF(_STATUS, _RP1);
         CurrBank:=0;
       end;
       1: begin
         _BCF(_STATUS, _RP1);
         _BSF(_STATUS, _RP0);
         CurrBank:=1;
       end;
       3: begin
         _BSF(_STATUS, _RP0);
         CurrBank:=3;
       end;
     end;
  3: case targetBank of
       0: begin
         _BCF(_STATUS, _RP1);
         _BCF(_STATUS, _RP0);
         CurrBank:=0;
       end;
       1: begin
         _BCF(_STATUS, _RP1);
         CurrBank:=1;
       end;
       2: begin
         _BCF(_STATUS, _RP0);
         CurrBank:=2;
       end;
     end;
  // Este caso es equivalentea decir "no sé en qué banco estoy"
  255: case targetBank of
       0: begin
         _BCF(_STATUS, _RP1);
         _BCF(_STATUS, _RP0);
         CurrBank:=0;
       end;
       1: begin
         _BCF(_STATUS, _RP1);
         _BSF(_STATUS, _RP0);
         CurrBank:=1;
       end;
       2: begin
         _BSF(_STATUS, _RP1);
         _BCF(_STATUS, _RP0);
         CurrBank:=2;
       end;
       3: begin
         _BSF(_STATUS, _RP1);
         _BSF(_STATUS, _RP0);
         CurrBank:=3;
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
procedure TGenCodPic.StartRegs;
{Inicia los registros de trabajo en la lista.}
begin
  registerList.Clear;
  registerStack.Clear;   //limpia la pila
  {Crea registro de trabajo H, para que esté definido, pero aún no tiene asignado una
  posición en memoria.}
  H := CreateByteRegister(prtWorkReg);
  //Puede salir con error
end;
constructor TGenCodPic.Create;
begin
  inherited Create;
  pic := TPIC16.Create;
  registerList := TPicRegister_list.Create(true);
  registerStack := TPicRegister_list.Create(true);
  stackTop := 0;  //Apunta a la siguiente posición libre
  {Crea registro de trabajo W. El registro W, es el registro interno del PIC, y no
  necesita un mapeo en RAM. Solo se le crea aquí, para poder usar su propiedad "used"}
  W := TPicRegister.Create(pic);
  W.assigned := false;   //se le marca así, para que no se intente usar
end;
destructor TGenCodPic.Destroy;
begin
  W.Destroy;
  registerStack.Destroy;
  registerList.Destroy;
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
    dicSet('Size of data not supported.', 'Tamaño de dato no soportado.');
    dicSet('RAM memory is full.', 'Memoria RAM agotada.');
    dicSet('Duplicated identifier: "%s"', 'Identificador duplicado: "%s"');
    dicSet('Undefined type "%s"', 'Tipo "%s" no definido.');
    dicSet('Very complex expression. To simplify.','Expresión muy compleja. Simplificar.');
  end;
  end;
end;

end.

