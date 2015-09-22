{
Implementación de un compialdor sencillo de Pascal para microcontroladores PIC de
rango medio.
Esta implementación no permitirá recursividad, por las limitaciones de recursos de los
dispositivos más pequeños, y por la dificultad adicional en la conmutación de bancos
para los dispositivos más grandes.
El compilador está orientado a uso de registros (solo hay uno) y memoria RAM, pero se
implementa una pequeña estructura de pila para la evaluación de expresiones aritméticas
con cierta complejidad.
Solo se manejan datos de tipo boolean, byte y word, y operaciones sencillas.
}
type
//  TSatReg = (srFree, srUsed)
  //define un registro virtual para implementar un intérprete
  Tregister = record
    used   : boolean;  //indica si está usado
    offs   : byte;     //desplazamiento en memoria
    bank    : byte;     //banco
  end;

var
  /////// Tipos de datos del lenguaje ////////////
  typNull: TType;   //Tipo nulo (sin tipo)
  tipBool: TType;   //Booleanos
  tipByte: TType;   //número sin signo
  tipWord: TType;   //número sin signo
//  tipChr : Ttype;   //un caracter
  ////////// Registros virtuales ////////////
  {La arquitectura definida aquí contempla:

  Un registro de trabajo W, de 8 bits (el acumulador del PIC).
  Dos registros adicionales  H y L de 8 bits cada uno (Creados a demanda).

  Los resultados de una expresión se dejarán en:

  1. En Bit Z o C, de STATUS -> Si el resultado es de tipo boolean.
  2. El acumulador W        -> Si el resultado es de tipo byte.
  3. Los registros (H,w)    -> Si el resultado es tipo word.

  a menos que ya esté ocupado, en cuyo caso se guarda primero en un registro auxiliar.
  a menos que ya estén ocupados, en cuyo caso se pone primero en la pila.

  Despues de ejecutar alguna operación booleana que devuelva una expresión, se
  actaulizan las banderas: BooleanBit y BooleanInverted, que implican que:
  * Si BooleanInverted es TRUE, significa que la lógica de C o Z está invertida.
  * La bandera BooleanBit, indica si el resultado se deja en C o Z.

  Por normas de Xpres, se debe considerar que:
  * Todas las operaciones recibe sus dos parámetros en las variables p1 y p2.
  * El resultado de cualquier expresión se debe dejar indicado en el objeto "res".
  * Los valores enteros y enteros sin signo se cargan en valInt
  * Los valores booleanos se cargan en valBool
  * Los valores string se cargan en valStr
  * Las variables están mapeadas en el arreglo vars[]
  * Cada variable, de cualquier tipo, ocupa una celda de vars[]

  Los procedimientos de operaciones, deben actualizar en el acumulador:

  * El tipo de resultado (para poder evaluar la expresión completa como si fuera un
  operando nuevo)
  * La categoría del operador (constante, expresión, etc), para poder optimizar la generación
  de código.
  * El estado del registro (usado o libre)
   }
const
  STACK_SIZE = 8;    //tamaño de pila para subrutinas
  MAX_MEMTAB = 4; //tamaño máximo de la tabla de bloques de memoria
  //constantes útiles para ensamblador
  _STATUS = $03;
  _C = 0;
  _Z = 2;
  _RP0 = 5;
  _RP1 = 6;
//  _IRP = 7;
var
  w: Tregister;
  {Estrutura de pila. Se crea una tabla de las direcciones a usar (no necesariamente
  consecutivas, porque puede haber variables ABSOLUTE) como pila. El tamaño de la pila a
  usar   no es fijo, sino que se crea de acuerdo al tamaño requerido para la evaluación de
  expresiones. Puede incluso ser de tamaño cero.}
  memtab: array[0..MAX_MEMTAB-1] of Tregister;
  sp: integer;    //puntero a la estructura de pila. Apunta a la posición libre
  spSize: integer;  //tamaño actual de pila
  _H, _L: Tregister;   //registros de trabajo adicionales, para cálculos
  //variables de estado de las expresiones booleanas
  BooleanInverted : boolean;  //indica que la lógica del bit de salida está invertida
  BooleanBit : byte;          //indica el bit que se devuelve el resultado booleano (Z o C)

//Rutinas de gestión de memoria
function GetByte(var reg: Tregister; varNom: string = ''): boolean;
{Pide una dirección de memoria para alojar un byte en la RAM, para una tarea temporal.
 Devuelve en "reg" la dirección del byte pedido. Si falla devuelve FALSE.
 Una vez finalizada la tarea, se debe liberar los bytes pedidos (para usar eficiéntemente
 la RAM), a menos que se quiera mantener esa posición de memoria, rerservada exclusivamente.
 De acuerdo a los bytes solicitados, se va reservando espacio en la RAM, usando
 variables. Las direcciones usadas, se guardan en la tabla memtab[], aunque no siempre
 corresponden a direcciones consecutivas}
var
  aVar: TxpVar;
begin
   if sp>=MAX_MEMTAB then begin
     //Se asume que se desbordó la memoria evaluando a alguna expresión
     GenError('Expresión muy compleja. Simplificar.');
     exit(false);
   end;
   if sp>= spSize then begin
     //No hay espacio para este nuevo valor. Crea espacio como variable
     if varNom = '' then
       aVar := CreateVar('_stk'+IntToStr(sp), tipByte)
     else
       aVar := CreateVar(varNom, tipByte);
     if HayError then exit(false);
     memtab[sp].offs := aVar.offs;   //toma dirección libre
     memtab[sp].bank  := aVar.bank;
     inc(spSize);  //sube cota
   end;
   reg := memtab[sp];  //actualiza dirección
   reg.used:=false;    //porque es una dirección  nueva
   Inc(sp);
   exit(true);   //salió con éxito
end;
function FreeByte(var reg: Tregister): boolean;
{Libera el último byte, que se pidió a la RAM. Devuelve en "reg", la dirección del último
 byte pedido. Si hubo error, devuelve FALSE.
 Liberarlos significa que estarán disponibles, para la siguiente vez que se pidan}
begin
   if sp<=0 then begin
     GenError('Desborde de pila.');
     exit(false);
   end;
   Dec(sp);
   reg.offs := memtab[sp].offs;  //devuelve la dirección
   reg.bank := memtab[sp].bank;
   exit(true)
end;
{procedure PushW;
//Guarda valor del acumulador en memoria temporal. Así lo deja libre.
var
  r: Tregister;
begin
  if not GetByte(r) then exit;  //pide un byte en RAM
  _MOVWF(r.offs);  //mover acumulador a pila
end;
procedure PopW;
//Extrae de la memoria temporal, el valor del acumulador.
var
  r: Tregister;
begin
  if not FreeByte(r) then exit;
  _MOVF(r.offs, toW);  //retorna a W
end;}
//Rutinas adicionales
function ValidateByteRange(n: integer): boolean;
//Verifica que un valor entero, se pueda convertir a byte. Si no, devuelve FALSE.
begin
  if (n>=0) and (n<256) then
     exit(true)
  else begin
    GenError('Valor numérico excede el rango de un byte.');
    exit(false);
  end;
end;
function ValidateWordRange(n: integer): boolean;
//Verifica que un valor entero, se pueda convertir a byte. Si no, devuelve FALSE.
begin
  if (n>=0) and (n<65536) then
     exit(true)
  else begin
    GenError('Valor numérico excede el rango de un word.');
    exit(false);
  end;
end;
////////////rutinas obligatorias
procedure Cod_StartProgram;
//Codifica la parte inicial del programa
begin
  //Code('.CODE');   //inicia la sección de código
  sp := 0;  //inicia pila
  spSize:=0;  //inicia sin tamaño
  _H.offs := MAXBYTE;  //indica que no ha sido inicializado
  _L.offs := MAXBYTE;  //indica que no ha sido inicializado
end;
procedure Cod_EndProgram;
//Codifica la parte inicial del programa
begin
  //Code('END');   //inicia la sección de código
end;
procedure expr_start(const exprLevel: integer);
//Se ejecuta siempre al StartSyntax el procesamiento de una expresión
begin
  if exprLevel=1 then begin //es el primer nivel
//    Code('  ;expres');
    w.used:=false;   //inicia con el registro libre
    res.typ := tipByte;   //le pone un tipo por defecto
  end else begin  //es recursivo
//    if ALused then  //hay un dato evaluado
//      Code('  push al');  //lo guarda
  end;
end;
procedure expr_end(const exprLevel: integer; isParam: boolean);
//Se ejecuta al final de una expresión, si es que no ha habido error.
begin
  if isParam then begin
    //Se terminó de evaluar un parámetro
    res.Push;   //pone parámetro en pila
    if HayError then exit;
    w.used:=false;  //se libera registro
  end;
  if exprLevel = 1 then begin  //el último nivel
//    Code('  ;fin expres');
  end;
end;
procedure RequireHL;
{Indica que va a requerir usar los registros de trabajo _H y _L}
begin
  if _L.offs=MAXBYTE then  //no existe aún
     if not GetByte(_L, '_L') then exit;   //pide direcciones de RAM, para parámetro
  if _H.offs=MAXBYTE then  //no existe aún
     if not GetByte(_H, '_H') then exit;
end;
procedure ReserveW;
{Indica que se va a utilizar el acumulador. De encontrarse ocupado,
se pondrá en la pila. Si no hay espacio, genera error}
var
  r: Tregister;
begin
  if w.used then begin
    //Está ocupado. Usa espacio temporal para almacenar
    if not GetByte(r) then exit;  //pide un byte en RAM
    _MOVWF(r.offs);  //mueve acumulador a "pila"
  end;
  w.used := true;   //Lo marca como indicando que se va a ocupar
end;
procedure ReserveH;
{Indica que se va a utilizar el par de registros (_H,W). De encontrarse ocupado,
se pondrá en la pila. Si no hay espacio, genera error.
NOTA: Usa el registro W, así que este debe estar libre.}
var
  r: Tregister;
begin
  if _H.used then begin
    //Está ocupado. Usa espacio temporal para almacenar
    if not GetByte(r) then exit;  //pide un byte en RAM
    _MOVF(_H.offs, toW);
    _MOVWF(r.offs);  //mueve a pila
  end;
  _H.used := true;   //Lo marca como indicando que se va a ocupar
end;
////////////operaciones con Boolean
procedure bool_asig_bool;
begin
  if p1.catOp <> coVariab then begin  //validación
    GenError('Solo se puede asignar a variable.'); exit;
  end;
  case p2.catOp of
  coConst : begin
    if p2.valBool then begin
      _BSF(p1.offs, p1.bit);
    end else begin
      _BCF(p1.offs, p1.bit);
    end;
  end;
  coVariab: begin
    _BCF(p1.offs, p1.bit);
    _BTFSC(p2.offs, p2.bit);
    _BSF(p1.offs, p1.bit);
  end;
  coExpres: begin  //ya está en STATUS.Z
    if BooleanInverted then begin  //está invertido
      _BCF(p1.offs, p1.bit);
      _BTFSS(_STATUS, BooleanBit);
      _BSF(p1.offs, p1.bit);
    end else begin  //caso normal
      _BCF(p1.offs, p1.bit);
      _BTFSC(_STATUS, BooleanBit);
      _BSF(p1.offs, p1.bit);
    end;
  end;
  else
    GenError('Not implemented.'); exit;
  end;
  w.used:=false;  //No es importante lo que queda
end;
////////////operaciones con Byte
procedure byte_OnPush(const Op: pointer);
var
  OpPtr: ^TOperand;
begin
  OpPtr := Op;

end;
procedure byte_asig_byte;
begin
  if p1.catOp <> coVariab then begin  //validación
    GenError('Solo se puede asignar a variable.'); exit;
  end;
  case p2.catOp of
  coConst : begin
    if p2.valInt=0 then begin
      //caso especial
      _CLRF(p1.offs);
    end else begin
      _MOVLW(p2.valInt);
      _MOVWF(p1.offs);
    end;
  end;
  coVariab: begin
    _MOVF(p2.offs, toW);
    _MOVWF(p1.offs);
  end;
  coExpres: begin  //ya está en w
    _MOVWF(p1.offs);
  end;
  else
    GenError('No soportado'); exit;
  end;
  w.used:=false;  //No es importante lo que queda
end;
procedure byte_oper_byte(const InstLW, InstWF:TPIC16Inst);
{Rutina general en operaciones con bytes}
var
  r: Tregister;
begin
  case catOperation of
  coConst_Variab: begin
    ReserveW; if HayError then exit;   //pide acumualdor
    _MOVLW(p1.valInt);
    CodAsmFD(InstWF, p2.offs, toW);  //deja en W
  end;
  coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    //ReserveW; if HayError then exit;
    CodAsmK(InstLW, p1.valInt);  //deja en W
  end;
  coVariab_Const: begin
    ReserveW; if HayError then exit;
    _MOVLW(p2.valInt);
    CodAsmFD(InstWF, p1.offs, toW);  //deja en W
  end;
  coVariab_Variab:begin
    ReserveW; if HayError then exit;
    _MOVF(p2.offs, toW);
    CodAsmFD(InstWF, p1.offs, toW);  //deja en W
  end;
  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    //ReserveW; if HayError then exit;
    CodAsmFD(InstWF, p1.offs, toW);  //deja en W
  end;
  coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    //ReserveW; if HayError then exit;
    CodAsmK(InstLW, p2.valInt);  //deja en W
  end;
  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    //ReserveW; if HayError then exit;
    CodAsmFD(InstWF, p2.offs, toW);  //deja en W
  end;
  coExpres_Expres:begin
    //la expresión p1 debe estar salvada y p2 en el acumulador
    FreeByte(r);   //libera pila porque se usará el dato ahí contenido
    CodAsmFD(InstWF, r.offs, toW);  //opera directamente al dato que había en la pila. Deja en W
  end;
  end;
  //caso de salida más general
  res.typ := tipByte;   //el resultado será siempre entero
  res.catOp:=coExpres; //por defecto generará una expresión
  w.used:=true;        //se está usando el acumulador
end;
procedure byte_suma_byte;
begin
  if catOperation  = coConst_Const then begin  //suma de dos constantes. Caso especial
    res.valInt:=p1.valInt+p2.valInt;  //optimiza respuesta
    if not ValidateByteRange(res.valInt) then exit;  //necesario
    res.typ := tipByte;
    res.catOp:=coConst;
    //w.used:=false;  //no se usa el acumulador. Lo deja como estaba
    exit;  //sale aquí, porque es un caso particular
  end else  //caso general
    byte_oper_byte(ADDLW, ADDWF);
end;
procedure byte_resta_byte;
begin
  if catOperation  = coConst_Const then begin  //suma de dos constantes. Caso especial
    res.valInt:=p1.valInt-p2.valInt;  //optimiza respuesta
    if not ValidateByteRange(res.valInt) then exit;   //necesario
    res.typ := tipByte;
    res.catOp:=coConst;
    //w.used:=false;  //no se usa el acumulador. Lo deja como estaba
    exit;  //sale aquí, porque es un caso particular
  end else  //caso general
    byte_oper_byte(SUBLW, SUBWF);
end;
procedure byte_and_byte;
begin
  if catOperation  = coConst_Const then begin  //suma de dos constantes. Caso especial
    res.valInt:=p1.valInt and p2.valInt;  //optimiza respuesta
    if not ValidateByteRange(res.valInt) then exit;   //necesario
    res.typ := tipByte;
    res.catOp:=coConst;
    //w.used:=false;  //no se usa el acumulador. Lo deja como estaba
    exit;  //sale aquí, porque es un caso particular
  end else  //caso general
    byte_oper_byte(ANDLW, ANDWF);
end;
procedure byte_or_byte;
begin
  if catOperation  = coConst_Const then begin  //suma de dos constantes. Caso especial
    res.valInt:=p1.valInt or p2.valInt;  //optimiza respuesta
    if not ValidateByteRange(res.valInt) then exit;   //necesario
    res.typ := tipByte;
    res.catOp:=coConst;
    //w.used:=false;  //no se usa el acumulador. Lo deja como estaba
    exit;  //sale aquí, porque es un caso particular
  end else  //caso general
    byte_oper_byte(IORLW, IORWF);
end;
procedure byte_xor_byte;
begin
  if catOperation  = coConst_Const then begin  //suma de dos constantes. Caso especial
    res.valInt:=p1.valInt xor p2.valInt;  //optimiza respuesta
    if not ValidateByteRange(res.valInt) then exit;   //necesario
    res.typ := tipByte;
    res.catOp:=coConst;
    //w.used:=false;  //no se usa el acumulador. Lo deja como estaba
    exit;  //sale aquí, porque es un caso particular
  end else  //caso general
    byte_oper_byte(XORLW, XORWF);
end;
procedure byte_igual_byte;
var
  r: Tregister;
begin
  BooleanInverted := false;  //indica que trabaja en modo normal
  BooleanBit:=_Z;    //devolverá en Z
  if catOperation  = coConst_Const then begin  //compara constantes. Caso especial
    res.valBool := (p1.valInt = p2.valInt);  //optimiza respuesta
    res.typ := tipBool;
    res.catOp:=coConst;
    //w.used:=false;  //no se usa el acumulador. Lo deja como estaba
    exit;  //sale aquí, porque es un caso particular
  end else begin  //caso general
    case catOperation of
    coConst_Variab: begin
      ReserveW; if HayError then exit;   //pide acumualdor
      _MOVLW(p1.valInt);
      _SUBWF(p2.offs, toW);  //si iguales _Z=1
    end;
    coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
//      ReserveW; if HayError then exit;   //pide acumualdor
      _SUBLW(p1.valInt);  //si iguales _Z=1
    end;
    coVariab_Const: begin
      ReserveW; if HayError then exit;   //pide acumualdor
      _MOVLW(p2.valInt);
      _SUBWF(p1.offs, toW);  //si iguales _Z=1
    end;
    coVariab_Variab:begin
      ReserveW; if HayError then exit;   //pide acumualdor
      _MOVF(p1.offs, toW);
      _SUBWF(p2.offs, toW);  //si iguales _Z=1
    end;
    coVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
      //ReserveW; if HayError then exit;
      _SUBWF(p1.offs, toW);  //si iguales _Z=1
    end;
    coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
      //ReserveW; if HayError then exit;
      _SUBLW(p2.valInt);  //si iguales _Z=1
    end;
    coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
      //ReserveW; if HayError then exit;
      _SUBWF(p2.offs, toW);  //si iguales _Z=1
    end;
    coExpres_Expres:begin
      //la expresión p1 debe estar salvada y p2 en el acumulador
      FreeByte(r);   //libera pila porque se usará el dato ahí contenido
      _SUBWF(r.offs, toW);  //compara directamente a lo que había en pila.
    end;
    else
      GenError('Not implemented.'); exit;
    end;
    //caso de salida más general
    res.typ := tipBool;   //el resultado será siempre entero
    res.catOp:=coExpres; //por defecto generará una expresión
    //el resultado queda en _Z
  end;
end;
procedure byte_difer_byte;
begin
  byte_igual_byte;  //usa el mismo código
  BooleanInverted := true;  //solo indica que la Lógica se ha invertido
end;
////////////operaciones con Word
procedure word_asig_word;
begin
  if p1.catOp <> coVariab then begin  //validación
    GenError('Solo se puede asignar a variable.'); exit;
  end;
  case p2.catOp of
  coConst : begin
    _MOVLW(p2.LByte);
    _MOVWF(p1.Loffs);
    _MOVLW(p2.HByte);
    _MOVWF(p1.Hoffs);
  end;
  coVariab: begin
    _MOVF(p2.Loffs, toW);
    _MOVWF(p1.Loffs);
    _MOVF(p2.Hoffs, toW);
    _MOVWF(p1.Hoffs);
  end;
  coExpres: begin   //se asume que se tiene en (_H,w)
    _MOVWF(p1.Loffs);
    _MOVF(_H.offs, toW);
    _MOVWF(p1.Hoffs);
  end;
  else
    GenError('No soportado'); exit;
  end;
  w.used:=false;  //No es importante lo que queda
end;
procedure word_asig_byte;
begin
  if p1.catOp <> coVariab then begin  //validación
    GenError('Solo se puede asignar a variable.'); exit;
  end;
  case p2.catOp of
  coConst : begin
    if p2.valInt = 0 then begin
      //caso especial
      _CLRF(p1.Loffs);
      _CLRF(p1.Hoffs);
    end else begin;
      _CLRF(p1.Hoffs);
      _MOVLW(p2.valInt);
      _MOVWF(p1.Loffs);
    end;
  end;
  coVariab: begin
    _CLRF(p1.Hoffs);
    _MOVF(p2.Loffs, toW);
    _MOVWF(p1.Loffs);
  end;
  coExpres: begin   //se asume que está en w
    _CLRF(p1.Hoffs);
    _MOVWF(p1.offs);
  end;
  else
    GenError('No soportado'); exit;
  end;
  w.used:=false;  //No es importante lo que queda
end;
procedure word_suma_word;
var
  spH: Tregister;
  spL: Tregister;
begin
  if catOperation  = coConst_Const then begin  //suma de dos constantes. Caso especial
    res.valInt:=p1.valInt+p2.valInt;  //optimiza respuesta
    if not ValidateWordRange(res.valInt) then exit;  //necesario
    if res.valInt<256 then
      res.typ := tipByte  //para que permita optimizar
    else
      res.typ := tipWord;
    res.catOp:=coConst;
    //w.used:=false;  //no se usa el acumulador. Lo deja como estaba
    exit;  //sale aquí, porque es un caso particular
  end else  begin //caso general
    RequireHL;   //requerir registros auxiliares _H y _L
    if HayError then exit;
    case catOperation of
    coConst_Variab: begin
      ReserveW; if HayError then exit;
      ReserveH; if HayError then exit;
{      _movlw(p1.LByte);      //Carga menos peso del dato 1
      _addwf(p2.Loffs,toW);  //Suma menos peso del dato 2
      _movwf(_L);             //Almacena el resultado
      _movlw(p1.HByte);      //Carga más peso del dato 1
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _addlw(1);             //Si, suma 1 al acumulador
      _addwf(p2.Hoffs,toW);  //Suma más peso del dato 2
      _movwf(_H);             //Guarda el resultado
      _movf(_L,toW);          //deja byte bajo en W}
      //versión más corta que solo usa _H, por validar
      _movlw(p1.HByte);      //Carga más peso del dato 1
      _addwf(p2.Hoffs,toW);  //Suma más peso del dato 2
      _movwf(_H.offs);             //Guarda el resultado
      _movlw(p1.LByte);      //Carga menos peso del dato 1
      _addwf(p2.Loffs,toW);  //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(_H.offs, toF);
    end;
    coConst_Expres: begin  //la expresión p2 se evaluó y esta en (_H,W)
      //ReserveW; if HayError then exit;
      _movwf(_L.offs);             //guarda byte bajo
      _movlw(p1.HByte);      //Carga más peso del dato 1
      _addwf(_H.offs,toF);         //Suma y guarda
      _movlw(p1.LByte);      //Carga menos peso del dato 1
      _addwf(_L.offs,toW);         //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(_H.offs, toF);
    end;
    coVariab_Const: begin
      ReserveW; if HayError then exit;
      ReserveH; if HayError then exit;
      _movlw(p2.HByte);      //Carga más peso del dato 1
      _addwf(p1.Hoffs,toW);  //Suma más peso del dato 2
      _movwf(_H.offs);             //Guarda el resultado
      _movlw(p2.LByte);      //Carga menos peso del dato 1
      _addwf(p1.Loffs,toW);  //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(_H.offs, toF);
    end;
    coVariab_Variab:begin
      ReserveW; if HayError then exit;
      ReserveH; if HayError then exit;
      _movlw(p1.Hoffs);      //Carga más peso del dato 1
      _addwf(p2.Hoffs,toW);  //Suma más peso del dato 2
      _movwf(_H.offs);             //Guarda el resultado
      _movlw(p1.Loffs);      //Carga menos peso del dato 1
      _addwf(p2.Loffs,toW);  //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(_H.offs, toF);
    end;
    coVariab_Expres:begin   //la expresión p2 se evaluó y esta en (spH,W)
      //No es necesario reservar (spH,W) porque existen y se están usando
      _movwf(_L.offs);             //guarda byte bajo
      _movlw(p1.Hoffs);      //Carga más peso del dato 1
      _addwf(_H.offs,toF);         //Suma y guarda
      _movlw(p1.Loffs);      //Carga menos peso del dato 1
      _addwf(_L.offs,toW);         //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(_H.offs, toF);
    end;
    coExpres_Const: begin   //la expresión p1 se evaluó y esta en (spH,W)
      //No es necesario reservar (spH,W) porque existen y se están usando
      _movwf(_L.offs);             //guarda byte bajo
      _movlw(p2.HByte);      //Carga más peso del dato 1
      _addwf(_H.offs,toF);         //Suma y guarda
      _movlw(p2.LByte);      //Carga menos peso del dato 1
      _addwf(_L.offs,toW);         //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(_H.offs, toF);
    end;
    coExpres_Variab:begin  //la expresión p1 se evaluó y esta en (spH,W)
      //No es necesario reservar (spH,W) porque existen y se están usando
      _movwf(_L.offs);             //guarda byte bajo
      _movlw(p2.Hoffs);      //Carga más peso del dato 1
      _addwf(_H.offs,toF);         //Suma y guarda
      _movlw(p2.Loffs);      //Carga menos peso del dato 1
      _addwf(_L.offs,toW);         //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(_H.offs, toF);
    end;
    coExpres_Expres:begin
      //No es necesario reservar (spH,W) porque existen y se están usando
      //Además p1 está salvado en pila y p2 en (_H,W)
      FreeByte(spH);   //libera pila, obtiene dirección
      FreeByte(spL);   //libera pila, obtiene dirección
      _movwf(_L.offs);             //guarda byte bajo
      _movf(spH.offs, toW);      //Carga más peso del dato 1
      _addwf(_H.offs,toF);         //Suma y guarda
      _movf(spL.offs, toW);      //Carga menos peso del dato 1
      _addwf(_L.offs,toW);         //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(_H.offs, toF);
    end;
    else
      genError('Not implemented: "%s"', [CatOperationToStr]);
    end;
    //caso de salida más general
    res.typ := tipWord;   //el resultado será siempre entero
    res.catOp:=coExpres; //por defecto generará una expresión
    w.used:=true;        //se está usando el acumulador
  end;
end;
procedure word_suma_byte;
var
  spH: Tregister;
  spL: Tregister;
begin
  if catOperation  = coConst_Const then begin  //suma de dos constantes. Caso especial
    res.valInt:=p1.valInt+p2.valInt;  //optimiza respuesta
    if not ValidateWordRange(res.valInt) then exit;  //necesario
    if res.valInt<256 then
      res.typ := tipByte  //para que permita optimizar
    else
      res.typ := tipWord;
    res.catOp:=coConst;
    //w.used:=false;  //no se usa el acumulador. Lo deja como estaba
    exit;  //sale aquí, porque es un caso particular
  end else  begin //caso general
    RequireHL;   //requerir registros auxiliares _H y _L
    if HayError then exit;
    case catOperation of
    coConst_Variab: begin
      ReserveW; if HayError then exit;
      ReserveH; if HayError then exit;
      //versión más corta que solo usa _H, por validar
      _movlw(p1.HByte);      //Carga más peso del dato 1
      _movwf(_H.offs);
      _movlw(p1.LByte);      //Carga menos peso del dato 1
      _addwf(p2.Loffs,toW);  //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(_H.offs, toF);
    end;
    coConst_Expres: begin  //la expresión p2 se evaluó y esta en (W)
      //ReserveW; if HayError then exit;
      _movwf(_L.offs);             //guarda byte bajo
      _movlw(p1.HByte);      //Carga más peso del dato 1
      _movwf(_H.offs);
      _movlw(p1.LByte);      //Carga menos peso del dato 1
      _addwf(_L.offs,toW);         //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(_H.offs, toF);
    end;
    coVariab_Const: begin
      ReserveW; if HayError then exit;
      ReserveH; if HayError then exit;
      _MOVF(p1.Hoffs, toW);      //Carga más peso del dato 1
      _movwf(_H.offs);             //Guarda el resultado
      _movlw(p2.LByte);
      _addwf(p1.Loffs,toW);  //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(_H.offs, toF);
    end;
    coVariab_Variab:begin
      ReserveW; if HayError then exit;
      ReserveH; if HayError then exit;
      _movlw(p1.Hoffs);      //Carga más peso del dato 1
      _movwf(_H.offs);
      _movlw(p1.Loffs);      //Carga menos peso del dato 1
      _addwf(p2.Loffs,toW);  //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(_H.offs, toF);
    end;
    coVariab_Expres:begin   //la expresión p2 se evaluó y esta en (_H,W)
      //ReserveW; if HayError then exit;
      _movlw(p1.Hoffs);      //Carga más peso del dato 1
      _movwf(_H.offs);
      _addwf(p1.Loffs,toW);  //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(_H.offs, toF);
    end;
    coExpres_Const: begin   //la expresión p1 se evaluó y esta en (_H,W)
      //No es necesario reservar (H,W) porque existen y se están usando
      _addwf(p2.LByte,toW);         //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(_H.offs, toF);
    end;
    coExpres_Variab:begin  //la expresión p1 se evaluó y esta en (_H,W)
      //No es necesario reservar (H,W) porque existen y se están usando
      _addwf(p2.Loffs,toW);         //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(_H.offs, toF);
    end;
    coExpres_Expres:begin
      //la expresión p1 debe estar salvada y p2 en el acumulador
      FreeByte(spH);   //libera pila, obtiene dirección
      FreeByte(spL);   //libera pila, obtiene dirección
      _movf(spH.offs, toW);      //Carga más peso del dato 1
      _movwf(_H.offs);
      _addwf(spL.offs,toW);  //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(_H.offs, toF);
    end;
    else
      genError('Not implemented: "%s"', [CatOperationToStr] );
    end;
    //caso de salida más general
    res.typ := tipWord;   //el resultado será siempre entero
    res.catOp:=coExpres; //por defecto generará una expresión
    w.used:=true;        //se está usando el acumulador
  end;
end;
/////////////funciones del sistema
procedure codif_1mseg;
//Codifica rutina de reatrdo de 1mseg.
begin
  PutComLine('|;inicio rutina 1 mseg.');
  if _CLOCK = 1000000 then begin
    _MOVLW(62);  //contador de iteraciones
    _ADDLW(255);  //lazo de 4 ciclos
    _BTFSS(_STATUS,_Z);
    _GOTO(_PC-2); PutComm(';fin rutina 1 mseg a 1MHz.');
  end else if _CLOCK = 2000000 then begin
    _MOVLW(125);  //contador de iteraciones
    _ADDLW(255);  //lazo de 4 ciclos
    _BTFSS(_STATUS,_Z);
    _GOTO(_PC-2); PutComm(';fin rutina 1 mseg a 2MHz.');
  end else if _CLOCK = 4000000 then begin
    //rtuina básica para 4MHz
    _MOVLW(250);  //contador de iteraciones
    _ADDLW(255);  //lazo de 4 ciclos
    _BTFSS(_STATUS,_Z);
    _GOTO(_PC-2); PutComm(';fin rutina 1 mseg a 4MHz.');
  end else if _CLOCK = 8000000 then begin
    _MOVLW(250);
    _ADDLW(255);   //lazo de 8 ciclos
    _GOTO(_PC+1);  //introduce 4 ciclos más de retardo
    _GOTO(_PC+1);
    _BTFSS(_STATUS,_Z);
    _GOTO(_PC-4); PutComm(';fin rutina 1 mseg a 8Mhz.');
  end else if _CLOCK = 10000000 then begin
    _MOVLW(250);
    _ADDLW(255);   //lazo de 10 ciclos
    _GOTO(_PC+1);  //introduce 6 ciclos más de retardo
    _GOTO(_PC+1);
    _GOTO(_PC+1);
    _BTFSS(_STATUS,_Z);
    _GOTO(_PC-5); PutComm(';fin rutina 1 mseg a 10MHz.');
  end else if _CLOCK = 12000000 then begin
    _MOVLW(250);
    _ADDLW(255);   //lazo de 12 ciclos
    _GOTO(_PC+1);  //introduce 8 ciclos más de retardo
    _GOTO(_PC+1);
    _GOTO(_PC+1);
    _GOTO(_PC+1);
    _BTFSS(_STATUS,_Z);
    _GOTO(_PC-5); PutComm(';fin rutina 1 mseg a 12MHz.');
  end else begin
    GenError('Clock frequency not supported.');
  end;
end;
procedure codif_delay_ms(const fun: TxpFun);
//Codifica rutina de retardo en milisegundos
var
  delay: Word;
begin
   StartCodeSub(fun);  //inicia codificación
   PutComLine(';rutina delay.');
   //Esta rutina recibe los milisegundos en los registros (_H,_L) o en (_H,w) o en (w)
   //Em cualquier caso, siempre usa los registros _H y _L, además del acumulador "w".
   RequireHL;   //solicita que existan los registros _H y _L
   if HayError then exit;
   _CLRF(_H.offs); PutComm(' ;pto. de entrada con parámetro en (0,w)');
   _MOVWF(_L.offs); PutComm(';pto. de entrada con parámetro en (H,w)');
   _INCF(_H.offs,toF);  PutComm(';pto. de entrada con parámetro en (H,L)');
   _INCF(_L.offs,toF);  //corrección
 delay:= _PC;   //punto de entrada con parámetro en (_H,_L)
   _DECFSZ(_L.offs, toF);
   _GOTO(_PC+2);
   _DECFSZ(_H.offs, toF);
   _GOTO(_PC+2);
   _RETURN();
   codif_1mseg;   //codifica retardo 1 mseg
   if HayError then exit;
   _GOTO(delay);
   EndCodeSub;  //termina codificación
end;
procedure fun_putchar(fun: TxpFun);
begin
  //Esta es una fucnión INLINE
  //Esta función no devuelve un valor, por eso no nos preocupamos del tipo.
end;
procedure fun_delay_ms(fun: TxpFun);
begin
   if fun.adrr=-1 then begin
     //No ha sido codificada, la codifica.
     codif_delay_ms(fun);
   end;
   //Aquí sabemos que La rutina de retardo ya está codificada.
   case res.catOp of  //el parámetro debe estar en "res"
   coConst : begin
     _MOVLW(res.valInt);
     _call(fun.adrr);
   end;
   coVariab: begin
     _MOVF(res.offs, toW);
     _call(fun.adrr);
   end;
   coExpres: begin  //ya está en w
     _call(fun.adrr);  //ya está en W
   end;
   else
     GenError('No soportado'); exit;
   end;
end;
procedure fun_delay_ms_w(fun: TxpFun);
begin
   if fun.adrr=-1 then begin
     //No ha sido codificada, la codifica.
     codif_delay_ms(fun);
   end;
   //Aquí sabemos que La rutina de retardo ya está codificada.
   case res.catOp of  //el parámetro debe estar en "res"
   coConst : begin
     _MOVLW(res.HByte);
     _MOVWF(_H.offs);
     _MOVLW(res.LByte);
     _call(fun.adrr+1);
   end;
   coVariab: begin
     _MOVF(res.offs, toW);
     _MOVWF(_H.offs);
     _MOVF(res.offs+1, toW);
     _call(fun.adrr+1);
   end;
   coExpres: begin  //se asume que ya está en (_H,w)
     _call(fun.adrr+1);  //ya está en W
   end;
   else
     GenError('Not implemented.'); exit;
   end;
end;
procedure fun_Inc_byte(fun: TxpFun);
begin
  case res.catOp of  //el parámetro debe estar en "res"
  coConst : begin
    GenError('Cannot increase a constant.'); exit;
  end;
  coVariab: begin
    _INCF(res.offs, toF);
  end;
  coExpres: begin  //se asume que ya está en (_H,w)
    GenError('Cannot increase an expression.'); exit;
  end;
  else
    GenError('Not implemented.'); exit;
  end;
end;
procedure fun_Inc_word(fun: TxpFun);
begin
  case res.catOp of  //el parámetro debe estar en "res"
  coConst : begin
    GenError('Cannot increase a constant.'); exit;
  end;
  coVariab: begin
    _INCF(res.Loffs, toF);
    _BTFSC(_STATUS, _Z);
    _INCF(res.Hoffs, toF);
  end;
  coExpres: begin  //se asume que ya está en (_H,w)
    GenError('Cannot increase an expression.'); exit;
  end;
  else
    GenError('Not implemented.'); exit;
  end;
end;
procedure fun_Dec_byte(fun: TxpFun);
begin
  case res.catOp of  //el parámetro debe estar en "res"
  coConst : begin
    GenError('Cannot decrease a constant.'); exit;
  end;
  coVariab: begin
    _DECF(res.offs, toF);
  end;
  coExpres: begin  //se asume que ya está en (_H,w)
    GenError('Cannot decrease an expression.'); exit;
  end;
  else
    GenError('Not implemented.'); exit;
  end;
end;
procedure fun_Dec_word(fun: TxpFun);
begin
  case res.catOp of  //el parámetro debe estar en "res"
  coConst : begin
    GenError('Cannot decrease a constant.'); exit;
  end;
  coVariab: begin
    _MOVF(res.offs, toW);
    _BTFSC(_STATUS, _Z);
    _DECF(res.Hoffs, toF);
    _DECF(res.Loffs, toF);
  end;
  coExpres: begin  //se asume que ya está en (_H,w)
    GenError('Cannot decrease an expression.'); exit;
  end;
  else
    GenError('Not implemented.'); exit;
  end;
end;

procedure TCompiler.StartSyntax;
//Se ejecuta solo una vez al inicio
var
  opr: TOperator;
begin

  //Define métodos w usar
  OnExprStart := @expr_start;
  OnExprEnd := @expr_End;

  ///////////Crea tipos y operaciones
  ClearTypes;
  typNull := CreateType('boolean',t_boolean,-1);
  //tipo booleano
  tipBool :=CreateType('boolean',t_boolean,-1);   //de 1 bit
  //tipo numérico de un solo byte
  tipByte :=CreateType('byte',t_uinteger,1);   //de 2 bytes
  tipByte.OnPush:=@byte_OnPush;
  //tipo numérico de dos byte
  tipWord :=CreateType('word',t_uinteger,2);   //de 2 bytes

  //////// Operaciones con Boolean ////////////
  {Los operadores deben crearse con su precedencia correcta}
  opr:=tipBool.CreateOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(tipBool,@bool_asig_bool);

  {Precedencia de operadores en Pascal
6)    ~, not, signo "-"   (mayor precedencia)
5)    *, /, div, mod, and, shl, shr, &
4)    |, !, +, -, or, xor
3)    =, <>, <, <=, >, >=, in
2)    :=                  (menor precedencia)
}
  //////// Operaciones con Byte ////////////
  {Los operadores deben crearse con su precedencia correcta}
  opr:=tipByte.CreateOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(tipByte,@byte_asig_byte);
  opr:=tipByte.CreateOperator('+',4,'suma');  //suma
  opr.CreateOperation(tipByte,@byte_suma_byte);
  opr:=tipByte.CreateOperator('-',4,'resta');  //suma
  opr.CreateOperation(tipByte,@byte_resta_byte);
  opr:=tipByte.CreateOperator('AND',5,'and');  //suma
  opr.CreateOperation(tipByte,@byte_and_byte);
  opr:=tipByte.CreateOperator('OR',4,'or');  //suma
  opr.CreateOperation(tipByte,@byte_or_byte);
  opr:=tipByte.CreateOperator('XOR',4,'xor');  //suma
  opr.CreateOperation(tipByte,@byte_xor_byte);

  opr:=tipByte.CreateOperator('=',3,'igual');
  opr.CreateOperation(tipByte,@byte_igual_byte);
  opr:=tipByte.CreateOperator('<>',3,'difer');
  opr.CreateOperation(tipByte,@byte_difer_byte);

  //////// Operaciones con Word ////////////
  {Los operadores deben crearse con su precedencia correcta}
  opr:=tipWord.CreateOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(tipWord,@word_asig_word);
  opr.CreateOperation(tipByte,@word_asig_byte);
  opr:=tipWord.CreateOperator('+',4,'suma');  //suma
  opr.CreateOperation(tipWord,@word_suma_word);
  opr.CreateOperation(tipByte,@word_suma_byte);

end;
procedure TCompiler.CreateSystemElements;
{Inicia los elementos del sistema}
var
  f: TxpFun;  //índice para funciones
begin
  //////// Funciones del sistema ////////////
  {Notar que las funciones del sistema no crean espacios de nombres y no se hace
  validación para verificar la duplicidad (para hacer el proceso m´sa rápido).
  Es responsabilidad del progranador, no introducir funciones con conflictos.}
  f := CreateSysFunction('putchar', tipByte, @fun_putchar);
  f.CreateParam('',tipByte);
  f := CreateSysFunction('delay_ms', tipByte, @fun_delay_ms);
  f.CreateParam('',tipByte);
  f.adrr:=-1;   //para indicar que no está codificada
  f := CreateSysFunction('delay_ms', tipByte, @fun_delay_ms_w);
  f.CreateParam('',tipWord);
  f.adrr:=-1;   //para indicar que no está codificada
  f := CreateSysFunction('Inc', tipByte, @fun_Inc_byte);
  f.CreateParam('',tipByte);
  f := CreateSysFunction('Inc', tipByte, @fun_Inc_word);
  f.CreateParam('',tipWord);
  f := CreateSysFunction('Dec', tipByte, @fun_Dec_byte);
  f.CreateParam('',tipByte);
  f := CreateSysFunction('Dec', tipByte, @fun_Dec_word);
  f.CreateParam('',tipWord);
end;
