{
Implementación de un compilador sencillo de Pascal para microcontroladores PIC de
rango medio.
Esta implementación no permitirá recursividad, por las limitaciones de recursos de los
dispositivos más pequeños, y por la dificultad adicional en la conmutación de bancos
para los dispositivos más grandes.
El compilador está orientado a uso de registros (solo hay uno) y memoria RAM, pero se
implementa una especie de estructura de pila para la evaluación de expresiones
aritméticas con cierta complejidad y para el paso de parámetros a las funciones.
Solo se manejan datos de tipo bit, boolean, byte y word, y operaciones sencillas.
}
{La arquitectura definida aquí contempla:

Un registro de trabajo W, de 8 bits (el acumulador del PIC).
Dos registros adicionales  H y L de 8 bits cada uno (Creados a demanda).

Los resultados de una expresión se dejarán en:

1. En Bit Z o C, de STATUS -> Si el resultado es de tipo bit o boolean.
2. El acumulador W         -> Si el resultado es de tipo byte.
3. Los registros (H,w)     -> Si el resultado es tipo word.

Opcionalmente, si estos registros ya están ocupados, se guardan primero en la pila, o se
usan otros registros auxiliares.

Despues de ejecutar alguna operación booleana que devuelva una expresión, se
actualizan las banderas: BooleanBit y BooleanInverted, que implican que:
* Si BooleanInverted es TRUE, significa que la lógica de C o Z está invertida.
* La bandera BooleanBit, indica si el resultado se deja en C o Z.

Por normas de Xpres, se debe considerar que:
* Todas las operaciones recibe sus dos parámetros en las variables p1 y p2^.
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
unit GenCod;
{$mode objfpc}{$H+}
interface
uses
  Classes, SysUtils, SynEditHighlighter, Graphics, LCLType, SynFacilBasic,
  XpresTypes, XPresParserPIC, XpresElementsPIC, GenCodPic, Pic16Utils, MisUtils;
type
    { TGenCod }
    TGenCod = class(TGenCodPic)
    protected
      //campos para controlar la codificación de rutinas iniciales
      iniBloSub  : integer;   //inicio del blqoue de subrutinas
      curBloSub  : integer;   //fin del bloque de subrutinas
      finBloSub  : integer;   //tamaño máximo del bloque de subrutinas
      iFlashTmp  : integer;   //almacenamiento temporal para pic.iFlash
      Traslape   : boolean;   //bandera de traslape
      procedure callFunct(fun: TxpEleFun);
    private  //Operaciones con Bit
      procedure bit_load;
      procedure Oper_bit_asig_bit;
      procedure Oper_bit_asig_byte;
      procedure Oper_bit_and_bit;
      procedure Oper_bit_not;
    private  //Operaciones con boolean
      procedure bool_load;
      procedure Oper_bool_asig_bool;
    private  //Operaciones con byte
      procedure byte_OnPush(const OpPtr: pointer);
      procedure byte_load;
      procedure byte_oper_byte(const InstLW, InstWF: TPIC16Inst);
      procedure Oper_byte_asig_byte;
      procedure Oper_byte_sub_byte;
      procedure Oper_byte_add_byte;
      procedure Oper_byte_add_word;
      procedure Oper_byte_and_byte;
      procedure Oper_byte_or_byte;
      procedure Oper_byte_xor_byte;
      procedure Oper_byte_igual_byte;
      procedure Oper_byte_difer_byte;
    private  //Operaciones con Word
      procedure word_OnPush(const OpPtr: pointer);
      procedure word_load;
      procedure Oper_word_asig_word;
      procedure Oper_word_asig_byte;
      procedure Oper_word_add_word;
      procedure Oper_word_add_byte;
    private  //Funciones internas.
      DelayCoded: integer;  //bandera para codificación de retardo
      //Rutinas adicionales
      procedure codif_1mseg;
      procedure codif_delay_ms(const fun: TxpEleFun);
      procedure expr_end(posExpres: TPosExpres);
      procedure expr_start;
      procedure fun_Dec_byte(fun: TxpEleFun);
      procedure fun_Dec_word(fun: TxpEleFun);
      procedure fun_delay_ms(fun: TxpEleFun);
      procedure fun_delay_ms_w(fun: TxpEleFun);
      procedure fun_Inc_byte(fun: TxpEleFun);
      procedure fun_Inc_word(fun: TxpEleFun);
      procedure fun_putchar(fun: TxpEleFun);
    protected
      //Atributos adicionales
      tkStruct   : TSynHighlighterAttributes;
      tkDirective: TSynHighlighterAttributes;
      tkAsm      : TSynHighlighterAttributes;
      tkExpDelim : TSynHighlighterAttributes;
      tkBlkDelim : TSynHighlighterAttributes;
      tkOthers   : TSynHighlighterAttributes;
      procedure StartCodeSub(fun: TxpEleFun);
      procedure EndCodeSub;
      procedure Cod_StartProgram;
      procedure Cod_EndProgram;
      procedure CreateSystemElements;
    public
      procedure StartSyntax;
      procedure DefCompiler;
    end;

implementation

procedure TGenCod.StartCodeSub(fun: TxpEleFun);
{debe ser llamado para iniciar la codificación de una subrutina}
begin
  iFlashTmp :=  pic.iFlash;     //guarda puntero
  pic.iFlash := curBloSub;  //empieza a codificar aquí
  fun.adrr := curBloSub;  //fija inicio de rutina
end;
procedure TGenCod.EndCodeSub;
{debe ser llamado al terminar la codificaión de una subrutina}
begin
  curBloSub := pic.iFlash;  //indica siguiente posición libre
  if curBloSub > finBloSub then begin
    //hubo traslape
    Traslape:=true;
  end;
  pic.iFlash := iFlashTmp;     //retorna puntero
end;
procedure TGenCod.callFunct(fun: TxpEleFun);
{Rutina que debe llamara a uan función definida por el usuario}
begin
  //por ahora no hay problema de paginación
  _CALL(fun.adrr);
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
////////////rutinas obligatorias
procedure TGenCod.Cod_StartProgram;
//Codifica la parte inicial del programa
begin
  //Code('.CODE');   //inicia la sección de código
  LastBank := 0;
  StartRegs;  //Inicia registros
end;
procedure TGenCod.Cod_EndProgram;
//Codifica la parte inicial del programa
begin
  //Code('END');   //inicia la sección de código
end;
procedure TGenCod.expr_start;
//Se ejecuta siempre al StartSyntax el procesamiento de una expresión
begin
  //Inicia banderas de estado para empezar a calcular una expresión
  CurrBank := LastBank;   //Toma el banco de la expresión anterior
  LastCatOp := coConst;     //Al iniciar, asume ocnstante (no es exacto, pero sirve).
  BooleanInverted := false; //indica que trabaja en modo normal
  BooleanBit := _Z;         //por defecto, se trabaja con Z
  W.used := false;          //su ciclo de vida es de instrucción
  if H<>nil then
    H.used := false;        //su ciclo de vida es de instrucción
  res.typ := typByte;   //le pone un tipo por defecto
end;
procedure TGenCod.expr_end(posExpres: TPosExpres);
//Se ejecuta al final de una expresión, si es que no ha habido error.
begin
  case posExpres of
  pexPARSY: begin
    //Se terminó de evaluar un parámetro
    res.Push;   //Carga en registro de trabajo
    if HayError then exit;
    W.used:=false;  //se libera registro
  end;
  pexPARAM: begin
    //Se terminó de evaluar un parámetro
    res.Push;   //pone parámetro en pila
    if HayError then exit;
    W.used:=false;  //se libera registro
  end;
  end;
  if exprLevel = 1 then begin  //el último nivel
//    Code('  ;fin expres');
  end;
end;

////////////operaciones con Bit
procedure TGenCod.bit_load;
begin
  {Solo devuelve el mismo operador. No hace nada porque, si es constante o variable única,
  no necesita generar código (se hará si forma parte de una expresión), y si es expresión
  ya se generó el código. }
  res := p1^;   //No debería ser necesario copiar todos los campos
end;
procedure TGenCod.Oper_bit_asig_bit;
var
  OLD_W: TPicRegister;
begin
  if p1^.catOp <> coVariab then begin  //validación
    GenError('Solo se puede asignar a variable.'); exit;
  end;
  case p2^.catOp of
  coConst : begin
    {Actualmente no existen constantes de tipo "Bit", ya que el número menor que se
    reconoce es de typo byte. Por eso se define Oper_bit_asig_byte(). }
    if p2^.valBool then begin
      _BSF(p1^.offs, p1^.bit);
    end else begin
      _BCF(p1^.offs, p1^.bit);
    end;
  end;
  coVariab: begin
    if p1^.rVar = p2^.rVar then begin
      //Es asignación de la misma variable.
      if BooleanInverted then begin  //Es a := not a
        SaveW(OLD_W); //Va a usar W, así que lo guarda si es necesario
        if HayError then exit;   //verifica error.
        _MOVLW(p1^.rVar.BitMask);  //carga máscara
        _XORWF(p1^.offs, toF);
        RestoreW(OLD_W); //Restaura W, si estaba ocupado
      end else begin  //Es a := a
        PutComLine('No code, by optimizing.');
      end;
    end else begin
      if BooleanInverted then begin
        _BCF(p1^.offs, p1^.bit);
        _BTFSS(p2^.offs, p2^.bit);
        _BSF(p1^.offs, p1^.bit);
      end else begin
        _BCF(p1^.offs, p1^.bit);
        _BTFSC(p2^.offs, p2^.bit);
        _BSF(p1^.offs, p1^.bit);
      end;
    end;
  end;
  coExpres: begin  //ya está en STATUS.Z
    if BooleanInverted then begin  //está invertido
      _BCF(p1^.offs, p1^.bit);
      _BTFSS(_STATUS, BooleanBit);
      _BSF(p1^.offs, p1^.bit);
    end else begin  //caso normal
      _BCF(p1^.offs, p1^.bit);
      _BTFSC(_STATUS, BooleanBit);
      _BSF(p1^.offs, p1^.bit);
    end;
  end;
  else
    GenError('Not implemented.'); exit;
  end;
  SetResultExpres(typBit);  //Realmente, el resultado no es importante
end;
procedure TGenCod.Oper_bit_asig_byte;
begin
  if p1^.catOp <> coVariab then begin  //validación
    GenError('Solo se puede asignar a variable.'); exit;
  end;
  case p2^.catOp of
  coConst : begin
    {Esta es la única opción válida, pero solo para los valores 0 y 1}
    if p2^.valInt = 0 then begin
      _BCF(p1^.offs, p1^.bit);
    end else if p2^.valInt = 1 then begin
      _BSF(p1^.offs, p1^.bit);
    end else begin
      GenError('Invalid value for a bit variable.'); exit;
    end;
  end;
  coVariab,
  coExpres: begin  //ya está en STATUS.Z
    GenError('Cannot asign: (bit) := (byte).'); exit;
  end;
  else
    GenError('Not implemented.'); exit;
  end;
  SetResultExpres(typBit);  //Realmente, el resultado no es importante
end;
procedure TGenCod.Oper_bit_and_bit;
begin
//    case catOperation of
//    coConst_Const: begin  //AND de dos constantes. Caso especial
//      res.valBool := p1^.valInt and p2^.valInt;  //optimiza respuesta
//      res.typ := tipBit;
//      res.catOp:=coConst;
//      //w.used:=false;  //no se usa el acumulador. Lo deja como estaba
//      exit;  //sale aquí, porque es un caso particular
//    end
//    coConst_Variab: begin
//      ReserveW; if HayError then exit;   //pide Z
//      _MOVLW(p1^.valInt);
//      _BANKSEL(p2^.bank);
//      CodAsmFD(InstWF, p2^.offs, toW);  //deja en W
//    end;
//    coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
//      //ReserveW; if HayError then exit;
//      CodAsmK(InstLW, p1^.valInt);  //deja en W
//    end;
//    coVariab_Const: begin
//      ReserveW; if HayError then exit;
//      _MOVLW(p2^.valInt);
//      _BANKSEL(p1^.bank);
//      CodAsmFD(InstWF, p1^.offs, toW);  //deja en W
//    end;
//    coVariab_Variab:begin
//      ReserveW; if HayError then exit;
//      _BANKSEL(p2^.bank);
//      _MOVF(p2^.offs, toW);
//      _BANKSEL(p1^.bank);
//      CodAsmFD(InstWF, p1^.offs, toW);  //deja en W
//    end;
//    coVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
//      //ReserveW; if HayError then exit;
//      _BANKSEL(p1^.bank);
//      CodAsmFD(InstWF, p1^.offs, toW);  //deja en W
//    end;
//    coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
//      //ReserveW; if HayError then exit;
//      CodAsmK(InstLW, p2^.valInt);  //deja en W
//    end;
//    coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
//      //ReserveW; if HayError then exit;
//      _BANKSEL(p2^.bank);
//      CodAsmFD(InstWF, p2^.offs, toW);  //deja en W
//    end;
//    coExpres_Expres:begin
//      //la expresión p1 debe estar salvada y p2 en el acumulador
//      FreeStkRegisterByte(r);   //libera pila porque se usará el dato ahí contenido
//      _BANKSEL(r.bank);
//      CodAsmFD(InstWF, r.offs, toW);  //opera directamente al dato que había en la pila. Deja en W
//    end;
//    end;
//    //caso de salida más general
//    res.typ := tipByte;   //el resultado será siempre entero
//    res.catOp:=coExpres; //por defecto generará una expresión
//    w.used:=true;        //se está usando el acumulador
end;
procedure TGenCod.Oper_bit_not;
var
  mascara: byte;
begin
  case p1^.catOp of
  coConst : begin
    {Actualmente no existen constantes de tipo "Bit", pero si existieran, sería así}
    SetResultConst_bit(not p1^.valBool);
    BooleanInverted := false;  //aunque no se usa, se define por norma
  end;
  coVariab: begin
//    _MOVLW(p1^.rVar.BitMask);
//    _ANDWF(p1^.offs, toW);   //si es cero, pone Z en 1
//    SetResultExpres(tipBit);  //Devuelve en Z
//    BooleanInverted := false; //se usa lógica normal.
    {Optimiza invirtiendo solo la lógica, pero hay que tener cuidado si esta subexpresión
    se usa para asignación.}
    SetResultVariab(typBit, p1^.rVar);
    //No cambiamos su valor, sino su significado.
    BooleanInverted := not BooleanInverted;
  end;
  coExpres: begin  //ya está en STATUS.Z
    SetResultExpres(typBit);
    //No cambiamos su valor, sino su significado.
    BooleanInverted := not BooleanInverted;
  end;
  else
    GenError('Not implemented.'); exit;
  end;
end;
////////////operaciones con Boolean
procedure TGenCod.bool_load;
begin
  {Solo devuelve el mismo operador. No hace nada porque, si es constante o variable única,
  no necesita generar código (se hará si forma parte de una expresión), y si es expresión
  ya se generó el código. }
  res := p1^;   //No debería ser necesario copiar todos los campos
end;
procedure TGenCod.Oper_bool_asig_bool;
begin
  if p1^.catOp <> coVariab then begin  //validación
    GenError('Solo se puede asignar a variable.'); exit;
  end;
  case p2^.catOp of
  coConst : begin
    if p2^.valBool then begin
      _BSF(p1^.offs, p1^.bit);
    end else begin
      _BCF(p1^.offs, p1^.bit);
    end;
  end;
  coVariab: begin
    _BCF(p1^.offs, p1^.bit);
    _BTFSC(p2^.offs, p2^.bit);
    _BSF(p1^.offs, p1^.bit);
  end;
  coExpres: begin  //ya está en STATUS.Z
    if BooleanInverted then begin  //está invertido
      _BCF(p1^.offs, p1^.bit);
      _BTFSS(_STATUS, BooleanBit);
      _BSF(p1^.offs, p1^.bit);
    end else begin  //caso normal
      _BCF(p1^.offs, p1^.bit);
      _BTFSC(_STATUS, BooleanBit);
      _BSF(p1^.offs, p1^.bit);
    end;
  end;
  else
    GenError('Not implemented.'); exit;
  end;
  SetResultExpres(typBool);  //Realmente, el resultado no es importante
end;
////////////operaciones con Byte
procedure TGenCod.byte_OnPush(const OpPtr: pointer);
{Pone un byte en la pila. Se usa para pasar parámetros a función.}
var
  Op: ^TOperand;
begin
  Op := OpPtr;
  case Op^.catOp of  //el parámetro debe estar en "res"
  coConst : begin
    _MOVLW(res.valInt);
  end;
  coVariab: begin
    _BANKSEL(res.bank);
    _MOVF(res.offs, toW);
  end;
  coExpres: begin  //ya está en w
  end;
  end;
end;
procedure TGenCod.byte_load;
begin
  {Solo devuelve el mismo operador. No hace nada porque, si es constante o variable única,
  no necesita generar código (se hará si forma parte de una expresión), y si es expresión
  ya se generó el código. }
  res := p1^;   //No debería ser necesario copiar todos los campos
end;
procedure TGenCod.Oper_byte_asig_byte;
begin
  if p1^.catOp <> coVariab then begin  //validación
    GenError('Solo se puede asignar a variable.'); exit;
  end;
  case p2^.catOp of
  coConst : begin
    if p2^.valInt=0 then begin
      //caso especial
      _BANKSEL(p1^.bank);  //verifica banco destino
      _CLRF(p1^.offs);
    end else begin
      _MOVLW(p2^.valInt);
      _BANKSEL(p1^.bank);  //verifica banco destino
      _MOVWF(p1^.offs);
    end;
  end;
  coVariab: begin
    _BANKSEL(p2^.bank);  //verifica banco destino
    _MOVF(p2^.offs, toW);
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVWF(p1^.offs);
  end;
  coExpres: begin  //ya está en w
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVWF(p1^.offs);
  end;
  else
    GenError('No soportado'); exit;
  end;
  SetResultExpres(typByte);  //Realmente, el resultado no es importante
end;
procedure TGenCod.byte_oper_byte(const InstLW, InstWF:TPIC16Inst);
{Rutina general en operaciones con bytes}
var
  r: TPicRegister;
begin
  case catOperation of
  coConst_Variab: begin
    RequireResult_W;
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    CodAsmK(InstLW, p1^.valInt);  //deja en W
  end;
  coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    //ReserveW; if HayError then exit;
    CodAsmK(InstLW, p1^.valInt);  //deja en W
  end;
  coVariab_Const: begin
    RequireResult_W;
    _MOVLW(p2^.valInt);
    _BANKSEL(p1^.bank);
    CodAsmFD(InstWF, p1^.offs, toW);  //deja en W
  end;
  coVariab_Variab:begin
    RequireResult_W;
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    _BANKSEL(p1^.bank);
    CodAsmFD(InstWF, p1^.offs, toW);  //deja en W
  end;
  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    //ReserveW; if HayError then exit;
    _BANKSEL(p1^.bank);
    CodAsmFD(InstWF, p1^.offs, toW);  //deja en W
  end;
  coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    //ReserveW; if HayError then exit;
    CodAsmK(InstLW, p2^.valInt);  //deja en W
  end;
  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    //ReserveW; if HayError then exit;
    _BANKSEL(p2^.bank);
    CodAsmFD(InstWF, p2^.offs, toW);  //deja en W
  end;
  coExpres_Expres:begin
    //la expresión p1 debe estar salvada y p2 en el acumulador
    FreeStkRegisterByte(r);   //libera pila porque se usará el dato ahí contenido
    _BANKSEL(r.bank);
    CodAsmFD(InstWF, r.offs, toW);  //opera directamente al dato que había en la pila. Deja en W
  end;
  end;
  //Fija "res"
  SetResultExpres(typByte);
end;
procedure TGenCod.Oper_byte_add_byte;
begin
  if catOperation  = coConst_Const then begin  //suma de dos constantes. Caso especial
    SetResultConst_byte(p1^.valInt+p2^.valInt);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end else  //caso general
    byte_oper_byte(ADDLW, ADDWF);
end;
procedure TGenCod.Oper_byte_add_word;
var
  tmp : ^TOperand;
begin
  //Invierte los operandos
  tmp := p1;
  p1 := p2;
  p2 := tmp;
  //Actualiza catOperation
  catOperation := TCatOperation((Ord(p1^.catOp) << 2) or ord(p2^.catOp));
  //Y llama a la función opuesta
  Oper_word_add_byte;
end;
procedure TGenCod.Oper_byte_sub_byte;
begin
  if catOperation  = coConst_Const then begin  //suma de dos constantes. Caso especial
    SetResultConst_byte(p1^.valInt-p2^.valInt);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end else  //caso general
    byte_oper_byte(SUBLW, SUBWF);
end;
procedure TGenCod.Oper_byte_and_byte;
begin
  if catOperation  = coConst_Const then begin  //suma de dos constantes. Caso especial
    SetResultConst_byte(p1^.valInt and p2^.valInt);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end else  //caso general
    byte_oper_byte(ANDLW, ANDWF);
end;
procedure TGenCod.Oper_byte_or_byte;
begin
  if catOperation  = coConst_Const then begin  //suma de dos constantes. Caso especial
    SetResultConst_byte(p1^.valInt or p2^.valInt);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end else  //caso general
    byte_oper_byte(IORLW, IORWF);
end;
procedure TGenCod.Oper_byte_xor_byte;
begin
  if catOperation  = coConst_Const then begin  //suma de dos constantes. Caso especial
    SetResultConst_byte(p1^.valInt xor p2^.valInt);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end else  //caso general
    byte_oper_byte(XORLW, XORWF);
end;
procedure TGenCod.Oper_byte_igual_byte;
var
  r, OLD_W: TPicRegister;
begin
  BooleanInverted := false;  //indica que trabaja en modo normal
  if catOperation = coConst_Const then begin  //compara constantes. Caso especial
    SetResultConst_bool(p1^.valInt = p2^.valInt);
    exit;  //sale aquí, porque es un caso particular
  end else begin  //caso general
    case catOperation of
    coConst_Variab: begin
      SaveW(OLD_W); //Va a usar W, así que lo guarda si es necesario
      if HayError then exit;   //verifica error.
      _MOVLW(p1^.valInt);
      _BANKSEL(p2^.bank);  //verifica banco destino
      _SUBWF(p2^.offs, toW);  //si iguales _Z=1
      RestoreW(OLD_W);
    end;
    coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
//      ReserveW; if HayError then exit;   //pide acumulador
      _SUBLW(p1^.valInt);  //si iguales _Z=1
    end;
    coVariab_Const: begin
      SaveW(OLD_W); //Va a usar W, así que lo guarda si es necesario
      if HayError then exit;   //verifica error.
      _MOVLW(p2^.valInt);
      _BANKSEL(p1^.bank);  //verifica banco destino
      _SUBWF(p1^.offs, toW);  //si iguales _Z=1
      RestoreW(OLD_W);
    end;
    coVariab_Variab:begin
      SaveW(OLD_W); //Va a usar W, así que lo guarda si es necesario
      if HayError then exit;   //verifica error.
      _BANKSEL(p1^.bank);  //verifica banco destino
      _MOVF(p1^.offs, toW);
      _BANKSEL(p2^.bank);  //verifica banco destino
      _SUBWF(p2^.offs, toW);  //si iguales _Z=1
      RestoreW(OLD_W);
    end;
    coVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
      //ReserveW; if HayError then exit;
      _BANKSEL(p1^.bank);  //verifica banco destino
      _SUBWF(p1^.offs, toW);  //si iguales _Z=1
    end;
    coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
      //ReserveW; if HayError then exit;
      _SUBLW(p2^.valInt);  //si iguales _Z=1
    end;
    coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
      //ReserveW; if HayError then exit;
      _BANKSEL(p2^.bank);  //verifica banco destino
      _SUBWF(p2^.offs, toW);  //si iguales _Z=1
    end;
    coExpres_Expres:begin
      //la expresión p1 debe estar salvada y p2 en el acumulador
      FreeStkRegisterByte(r);   //libera pila porque se usará el dato ahí contenido
      _BANKSEL(r.bank);  //verifica banco destino
      _SUBWF(r.offs, toW);  //compara directamente a lo que había en pila.
    end;
    else
      GenError('Not implemented.'); exit;
    end;
    //caso de salida más general
    SetResultExpres(typBool);   //el resultado queda en _Z
  end;
end;
procedure TGenCod.Oper_byte_difer_byte;
begin
  Oper_byte_igual_byte;  //usa el mismo código
  BooleanInverted := not BooleanInverted;  //solo indica que la Lógica se ha invertido
end;
////////////operaciones con Word
procedure TGenCod.word_OnPush(const OpPtr: pointer);
{Carga el valor de una expresión a los registros de trabajo. Notar que no tiene que ver
con el nombre "OnPush". Solo se usa este evento predeclarado.}
var
  Op: ^TOperand;
begin
  Op := OpPtr;
  case Op^.catOp of  //el parámetro debe estar en "Op^"
  coConst : begin
    RequireHW;   //indica que va a usar H,W
    _MOVLW(Op^.HByte);
    _MOVWF(H.offs);
    _MOVLW(Op^.LByte);
  end;
  coVariab: begin
    RequireHW;   //indica que va a usar H,W
    _MOVF(Op^.offs, toW);
    _MOVWF(H.offs);
    _MOVF(Op^.offs+1, toW);
  end;
  coExpres: begin  //se asume que ya está en (H,w)
  end;
  end;
end;
procedure TGenCod.word_load;
begin
  {Solo devuelve el mismo operador. No hace nada porque, si es constante o variable única,
  no necesita generar código (se hará si forma parte de una expresión), y si es expresión
  ya se generó el código. }
  res := p1^;   //No debería ser necesario copiar todos los campos
end;
procedure TGenCod.Oper_word_asig_word;
begin
  if p1^.catOp <> coVariab then begin  //validación
    GenError('Solo se puede asignar a variable.'); exit;
  end;
  case p2^.catOp of
  coConst : begin
    _MOVLW(p2^.LByte);
    _MOVWF(p1^.Loffs);
    _MOVLW(p2^.HByte);
    _MOVWF(p1^.Hoffs);
  end;
  coVariab: begin
    _MOVF(p2^.Loffs, toW);
    _MOVWF(p1^.Loffs);
    _MOVF(p2^.Hoffs, toW);
    _MOVWF(p1^.Hoffs);
  end;
  coExpres: begin   //se asume que se tiene en (_H,w)
    _MOVWF(p1^.Loffs);
    _MOVF(H.offs, toW);
    _MOVWF(p1^.Hoffs);
  end;
  else
    GenError('No soportado'); exit;
  end;
  SetResultExpres(typWord);  //Realmente, el resultado no es importante
end;
procedure TGenCod.Oper_word_asig_byte;
begin
  if p1^.catOp <> coVariab then begin  //validación
    GenError('Solo se puede asignar a variable.'); exit;
  end;
  case p2^.catOp of
  coConst : begin
    if p2^.valInt = 0 then begin
      //caso especial
      _CLRF(p1^.Loffs);
      _CLRF(p1^.Hoffs);
    end else begin;
      _CLRF(p1^.Hoffs);
      _MOVLW(p2^.valInt);
      _MOVWF(p1^.Loffs);
    end;
  end;
  coVariab: begin
    _CLRF(p1^.Hoffs);
    _MOVF(p2^.Loffs, toW);
    _MOVWF(p1^.Loffs);
  end;
  coExpres: begin   //se asume que está en w
    _CLRF(p1^.Hoffs);
    _MOVWF(p1^.offs);
  end;
  else
    GenError('No soportado'); exit;
  end;
  SetResultExpres(typWord);  //Realmente, el resultado no es importante
end;
procedure TGenCod.Oper_word_add_word;
var
  spH: TPicRegister;
  spL: TPicRegister;
  aux: TPicRegister;
begin
  if catOperation  = coConst_Const then begin  //suma de dos constantes. Caso especial
    if p1^.valInt+p2^.valInt <256 then begin
      //Optimiza
      SetResultConst_byte(p1^.valInt+p2^.valInt);
    end else begin
      SetResultConst_word(p1^.valInt+p2^.valInt);
    end;
    exit;  //Puede salir con error
  end else begin //caso general
    if HayError then exit;
    case catOperation of
    coConst_Variab: begin
      RequireHW;   //indica que va a usar H,W
{     aux := GetUnusedByteRegister;  //Pide un registro libre
      _movlw(p1^.LByte);      //Carga menos peso del dato 1
      _addwf(p2^.Loffs,toW);  //Suma menos peso del dato 2
      _movwf(aux);             //Almacena el resultado
      _movlw(p1^.HByte);      //Carga más peso del dato 1
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _addlw(1);             //Si, suma 1 al acumulador
      _addwf(p2^.Hoffs,toW);  //Suma más peso del dato 2
      _movwf(H);             //Guarda el resultado
      _movf(aux,toW);          //deja byte bajo en W
      aux.Used := false;
}
      //versión más corta que solo usa H, por validar
      _movlw(p1^.HByte);      //Carga más peso del dato 1
      _addwf(p2^.Hoffs,toW);  //Suma más peso del dato 2
      _movwf(H.offs);             //Guarda el resultado
      _movlw(p1^.LByte);      //Carga menos peso del dato 1
      _addwf(p2^.Loffs,toW);  //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(H.offs, toF);
    end;
    coConst_Expres: begin  //la expresión p2 se evaluó y esta en (H,W)
      aux := GetAuxRegisterByte;  //Pide un registro libre
      _movwf(aux.offs);             //guarda byte bajo
      _movlw(p1^.HByte);      //Carga más peso del dato 1
      _addwf(H.offs,toF);         //Suma y guarda
      _movlw(p1^.LByte);      //Carga menos peso del dato 1
      _addwf(aux.offs,toW);         //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(H.offs, toF);
      aux.used := false;
    end;
    coVariab_Const: begin
      RequireHW;      //Indica que va a usar H,W
      _MOVLW(p2^.HByte);      //Carga más peso del dato 1
      _ADDWF(p1^.Hoffs,toW);  //Suma más peso del dato 2
      _MOVWF(H.offs);         //Guarda el resultado
      _MOVLW(p2^.LByte);      //Carga menos peso del dato 1
      _ADDWF(p1^.Loffs,toW);  //Suma menos peso del dato 2, deja en W
      _BTFSC(_STATUS,_C);     //Hubo acarreo anterior?
      _INCF(H.offs, toF);
    end;
    coVariab_Variab:begin
      RequireHW;      //Indica que va a usar H,W
      _MOVF(p1^.Hoffs, toW);  //Carga mayor peso del dato 1
      _ADDWF(p2^.Hoffs,toW);  //Suma mayor peso del dato 2
      _MOVWF(H.offs);         //Guarda mayor peso del resultado
      _MOVF(p1^.Loffs, toW);  //Carga menos peso del dato 1
      _ADDWF(p2^.Loffs,toW);  //Suma menos peso del dato 2, deja en W
      _BTFSC(_STATUS,_C);     //Hubo acarreo anterior?
      _INCF(H.offs, toF);
    end;
    coVariab_Expres:begin   //la expresión p2 se evaluó y esta en (H,W)
      //No es necesario reservar (H,W) porque existen y se están usando
      aux := GetAuxRegisterByte;  //Pide un registro libre
      _movwf(aux.offs);             //guarda byte bajo
      _movlw(p1^.Hoffs);      //Carga más peso del dato 1
      _addwf(H.offs,toF);         //Suma y guarda
      _movlw(p1^.Loffs);      //Carga menos peso del dato 1
      _addwf(aux.offs,toW);         //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);     //Hubo acarreo anterior?
      _incf(H.offs, toF);
      aux.used := false;
    end;
    coExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,W)
      //No es necesario reservar (H,W) porque existen y se están usando
      aux := GetAuxRegisterByte;  //Pide un registro libre
      _movwf(aux.offs);             //guarda byte bajo
      _movlw(p2^.HByte);      //Carga más peso del dato 1
      _addwf(H.offs,toF);         //Suma y guarda
      _movlw(p2^.LByte);      //Carga menos peso del dato 1
      _addwf(aux.offs,toW);         //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(H.offs, toF);
      aux.used := false;
    end;
    coExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,W)
      //No es necesario reservar (H,W) porque existen y se están usando
      aux := GetAuxRegisterByte;  //Pide un registro libre
      _movwf(aux.offs);             //guarda byte bajo
      _movlw(p2^.Hoffs);      //Carga más peso del dato 1
      _addwf(H.offs,toF);         //Suma y guarda
      _movlw(p2^.Loffs);      //Carga menos peso del dato 1
      _addwf(aux.offs,toW);         //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(H.offs, toF);
      aux.used := false;
    end;
    coExpres_Expres:begin
      //No es necesario reservar (H,W) porque existen y se están usando
      //Además p1 está salvado en pila y p2 en (_H,W)
      FreeStkRegisterByte(spH);   //libera pila, obtiene dirección
      FreeStkRegisterByte(spL);   //libera pila, obtiene dirección
      aux := GetAuxRegisterByte;  //Pide un registro libre
      _movwf(aux.offs);             //guarda byte bajo
      _movf(spH.offs, toW);      //Carga más peso del dato 1
      _addwf(H.offs,toF);         //Suma y guarda
      _movf(spL.offs, toW);      //Carga menos peso del dato 1
      _addwf(aux.offs,toW);         //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(H.offs, toF);
      aux.used := false;
    end;
    else
      genError('Not implemented: "%s"', [CatOperationToStr]);
    end;
    SetResultExpres(typWord);
  end;
end;
procedure TGenCod.Oper_word_add_byte;
var
  spH: TPicRegister;
  spL: TPicRegister;
  aux: TPicRegister;
begin
  if catOperation  = coConst_Const then begin  //suma de dos constantes. Caso especial
    if p1^.valInt+p2^.valInt <256 then begin
      //Optimiza
      SetResultConst_byte(p1^.valInt+p2^.valInt);
    end else begin
      SetResultConst_word(p1^.valInt+p2^.valInt);
    end;
    exit;  //puede salir con error
  end else begin //caso general
    if HayError then exit;
    case catOperation of
    coConst_Variab: begin
      RequireHW;   //indica que va a usar H,W
      //versión más corta que solo usa _H, por validar
      _movlw(p1^.HByte);      //Carga más peso del dato 1
      _movwf(H.offs);
      _movlw(p1^.LByte);      //Carga menos peso del dato 1
      _addwf(p2^.Loffs,toW);  //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(H.offs, toF);
    end;
    coConst_Expres: begin  //la expresión p2 se evaluó y esta en (W)
      aux := GetAuxRegisterByte;  //Pide un registro libre
      RequireH;   //Si hay expresión en "p2", se está usando W, pero no H (porque es byte)
      _movwf(aux.offs);      //guarda byte bajo
      _movlw(p1^.HByte);     //Carga más peso del dato 1
      _movwf(H.offs);
      _movlw(p1^.LByte);     //Carga menos peso del dato 1
      _addwf(aux.offs,toW);  //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(H.offs, toF);
      aux.used := false;
    end;
    coVariab_Const: begin
      RequireHW;     //Indica que va a usar H,W
      _MOVF(p1^.Hoffs, toW); //Carga más peso del dato 1
      _MOVWF(H.offs);        //Guarda el resultado
      _MOVLW(p2^.LByte);
      _ADDWF(p1^.Loffs,toW); //Suma menos peso del dato 2, deja en W
      _BTFSC(_STATUS,_C);    //Hubo acarreo anterior?
      _INCF(H.offs, toF);
    end;
    coVariab_Variab:begin
      RequireHW;   //indica que va a usar H,W
      _movlw(p1^.Hoffs);     //Carga más peso del dato 1
      _movwf(H.offs);
      _movlw(p1^.Loffs);     //Carga menos peso del dato 1
      _addwf(p2^.Loffs,toW); //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(H.offs, toF);
    end;
    coVariab_Expres:begin   //la expresión p2 se evaluó y esta en (_H,W)
      RequireH;   //Si hay expresión en "p2", se está usando W, pero no H (porque es byte)
      _movlw(p1^.Hoffs);      //Carga más peso del dato 1
      _movwf(H.offs);
      _addwf(p1^.Loffs,toW);  //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(H.offs, toF);
    end;
    coExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,W)
      //No es necesario reservar (H,W) porque existen y se están usando
      _addwf(p2^.LByte,toW);         //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(H.offs, toF);
    end;
    coExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,W)
      //No es necesario reservar (H,W) porque existen y se están usando
      _addwf(p2^.Loffs,toW);         //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(H.offs, toF);
    end;
    coExpres_Expres:begin
      //la expresión p1 debe estar salvada y p2 en el acumulador
      FreeStkRegisterByte(spH);   //libera pila, obtiene dirección
      FreeStkRegisterByte(spL);   //libera pila, obtiene dirección
      _movf(spH.offs, toW);      //Carga más peso del dato 1
      _movwf(H.offs);
      _addwf(spL.offs,toW);  //Suma menos peso del dato 2, deja en W
      _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
      _incf(H.offs, toF);
    end;
    else
      genError('Not implemented: "%s"', [CatOperationToStr] );
    end;
    SetResultExpres(typWord);
  end;
end;
/////////////funciones del sistema
procedure TGenCod.codif_1mseg;
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
procedure TGenCod.codif_delay_ms(const fun: TxpEleFun);
//Codifica rutina de retardo en milisegundos
var
  delay: Word;
  aux: TPicRegister;
begin
  StartCodeSub(fun);  //inicia codificación
  DelayCoded := fun.adrr;  //toma dirección
  PutComLine(';rutina delay.');
  RequireH(false);   //Se asegura de que se exista y lo marca como "usado".
  aux := GetAuxRegisterByte;  //Pide un registro libre
  {Esta rutina recibe los milisegundos en los registros en (H,w) o en (w)
  En cualquier caso, siempre usa el registros H , el acumulador "w" y un reg. auxiliar.
  Se supone que para pasar los parámetros, ya se requirió H, así que no es necesario
  crearlo.}
  if HayError then exit;
  _CLRF(H.offs); PutComm(' ;pto. de entrada con parámetro en (0,w)');
  _MOVWF(aux.offs); PutComm(';pto. de entrada con parámetro en (H,w)');
  _INCF(H.offs,toF);
  _INCF(aux.offs,toF);  //corrección
delay:= _PC;
  _DECFSZ(aux.offs, toF);
  _GOTO(_PC+2);
  _DECFSZ(H.offs, toF);
  _GOTO(_PC+2);
  _RETURN();
  codif_1mseg;   //codifica retardo 1 mseg
  if HayError then exit;
  _GOTO(delay);
  EndCodeSub;  //termina codificación
  aux.used := false;  //libera registro
end;
procedure TGenCod.fun_putchar(fun: TxpEleFun);
begin
  //Esta es una fucnión INLINE
  //Esta función no devuelve un valor, por eso no nos preocupamos del tipo.
end;
procedure TGenCod.fun_delay_ms(fun: TxpEleFun);
begin
   if DelayCoded=-1 then begin
     //No ha sido codificada, la codifica.
     codif_delay_ms(fun);
   end;
   //El parámetro byte, debe estar en W
   _call(DelayCoded);
end;
procedure TGenCod.fun_delay_ms_w(fun: TxpEleFun);
begin
   if DelayCoded=-1 then begin
     //No ha sido codificada, la codifica.
     codif_delay_ms(fun);
   end;
   //El parámetro word, debe estar en (H, W)
   _call(DelayCoded+1);
end;
procedure TGenCod.fun_Inc_byte(fun: TxpEleFun);
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
procedure TGenCod.fun_Inc_word(fun: TxpEleFun);
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
procedure TGenCod.fun_Dec_byte(fun: TxpEleFun);
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
procedure TGenCod.fun_Dec_word(fun: TxpEleFun);
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

procedure TGenCod.StartSyntax;
//Se ejecuta solo una vez al inicio
begin
  ///////////define la sintaxis del compilador
  //Tipos de tokens personalizados
  tkExpDelim := xLex.NewTokType('ExpDelim');//delimitador de expresión ";"
  tkBlkDelim := xLex.NewTokType('BlkDelim'); //delimitador de bloque
  tkStruct   := xLex.NewTokType('Struct');   //personalizado
  tkDirective:= xLex.NewTokType('Directive'); //personalizado
  tkAsm      := xLex.NewTokType('Asm');      //personalizado
  tkOthers   := xLex.NewTokType('Others');   //personalizado
  //Configura atributos
  tkKeyword.Style := [fsBold];     //en negrita
  tkBlkDelim.Foreground:=clGreen;
  tkBlkDelim.Style := [fsBold];    //en negrita
  tkStruct.Foreground:=clGreen;
  tkStruct.Style := [fsBold];      //en negrita
  //inicia la configuración
  xLex.ClearMethodTables;          //limpia tabla de métodos
  xLex.ClearSpecials;              //para empezar a definir tokens
  //crea tokens por contenido
  xLex.DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');
  xLex.DefTokContent('[0-9]', '[0-9.]*', tkNumber);
  xLex.DefTokContent('[$]','[0-9A-Fa-f]*', tkNumber);
  xLex.DefTokContent('[%]','[01]*', tkNumber);
  //define palabras claves
  xLex.AddIdentSpecList('THEN var type', tkKeyword);
  xLex.AddIdentSpecList('program public private method const', tkKeyword);
  xLex.AddIdentSpecList('class create destroy sub do begin', tkKeyword);
  xLex.AddIdentSpecList('END UNTIL', tkBlkDelim);
  xLex.AddIdentSpecList('true false', tkBoolean);
  xLex.AddIdentSpecList('if while repeat for', tkStruct);
  xLex.AddIdentSpecList('and or xor not div mod in', tkOperator);
  //tipos predefinidos
  xLex.AddIdentSpecList('bit boolean byte word', tkType);
  //funciones del sistema
  xLex.AddIdentSpecList('putchar delay_ms Inc Dec', tkSysFunct);
  //símbolos especiales
  xLex.AddSymbSpec('+',  tkOperator);
  xLex.AddSymbSpec('-',  tkOperator);
  xLex.AddSymbSpec('*',  tkOperator);
  xLex.AddSymbSpec('/',  tkOperator);
  xLex.AddSymbSpec('\',  tkOperator);
//  xLex.AddSymbSpec('%',  tkOperator);
  xLex.AddSymbSpec('**', tkOperator);
  xLex.AddSymbSpec('=',  tkOperator);
  xLex.AddSymbSpec('>',  tkOperator);
  xLex.AddSymbSpec('>=', tkOperator);
  xLex.AddSymbSpec('<;', tkOperator);
  xLex.AddSymbSpec('<=', tkOperator);
  xLex.AddSymbSpec('<>', tkOperator);
  xLex.AddSymbSpec('<=>',tkOperator);
  xLex.AddSymbSpec(':=', tkOperator);
  xLex.AddSymbSpec(';', tkExpDelim);
  xLex.AddSymbSpec('(',  tkOthers);
  xLex.AddSymbSpec(')',  tkOthers);
  xLex.AddSymbSpec(':',  tkOthers);
  xLex.AddSymbSpec(',',  tkOthers);
  //crea tokens delimitados
  xLex.DefTokDelim('''','''', tkString);
  xLex.DefTokDelim('"','"', tkString);
  xLex.DefTokDelim('//','', xLex.tkComment);
  xLex.DefTokDelim('{','}', xLex.tkComment, tdMulLin);
  xLex.DefTokDelim('{$','}', tkDirective);
  xLex.DefTokDelim('Asm','End', tkAsm, tdMulLin);
  //define bloques de sintaxis
//  xLex.AddBlock('{','}');
  xLex.Rebuild;   //es necesario para terminar la definición
end;
procedure TGenCod.DefCompiler;
var
  opr: TxpOperator;
begin
  //Define métodos a usar
  OnExprStart := @expr_start;
  OnExprEnd := @expr_End;

  ///////////Crea tipos y operaciones
  ClearTypes;
  typNull := CreateType('null',t_boolean,-1);
  //tipo bit
  typBit :=CreateType('bit',t_uinteger,-1);   //de 1 bit
  //tipo booleano
  typBool :=CreateType('boolean',t_boolean,-1);   //de 1 bit
  //tipo numérico de un solo byte
  typByte :=CreateType('byte',t_uinteger,1);   //de 2 bytes
  //tipo numérico de dos byte
  typWord :=CreateType('word',t_uinteger,2);   //de 2 bytes

  {Los operadores deben crearse con su precedencia correcta}
  //////// Operaciones con Boolean ////////////
  typBool.OperationLoad:=@bool_load;
  opr:=typBool.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typBool,@Oper_bool_asig_bool);
  //////// Operaciones con Bit ////////////
  typBit.OperationLoad:=@bit_load;
  opr:=typBit.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typBit, @Oper_bit_asig_bit);
  opr.CreateOperation(typByte, @Oper_bit_asig_byte);

  opr:=typBit.CreateUnaryPreOperator('NOT', 6, 'not', @Oper_bit_not);
  opr:=typBit.CreateBinaryOperator('+',4,'and');  //suma
  opr.CreateOperation(typBit,@Oper_bit_and_bit);
  {Precedencia de operadores en Pascal
6)    ~, not, signo "-"   (mayor precedencia)
5)    *, /, div, mod, and, shl, shr, &
4)    |, !, +, -, or, xor
3)    =, <>, <, <=, >, >=, in
2)    :=                  (menor precedencia)
}
  //////// Operaciones con Byte ////////////
  {Los operadores deben crearse con su precedencia correcta}
  typByte.OperationLoad:=@byte_load;
  typByte.OperationPush:=@byte_OnPush;
  opr:=typByte.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typByte,@Oper_byte_asig_byte);
  opr:=typByte.CreateBinaryOperator('+',4,'suma');  //suma
  opr.CreateOperation(typByte,@Oper_byte_add_byte);
  opr.CreateOperation(typWord,@Oper_byte_add_word);
  opr:=typByte.CreateBinaryOperator('-',4,'resta');  //suma
  opr.CreateOperation(typByte,@Oper_byte_sub_byte);
  opr:=typByte.CreateBinaryOperator('AND',5,'and');  //suma
  opr.CreateOperation(typByte,@Oper_byte_and_byte);
  opr:=typByte.CreateBinaryOperator('OR',4,'or');  //suma
  opr.CreateOperation(typByte,@Oper_byte_or_byte);
  opr:=typByte.CreateBinaryOperator('XOR',4,'xor');  //suma
  opr.CreateOperation(typByte,@Oper_byte_xor_byte);

  opr:=typByte.CreateBinaryOperator('=',3,'igual');
  opr.CreateOperation(typByte,@Oper_byte_igual_byte);
  opr:=typByte.CreateBinaryOperator('<>',3,'difer');
  opr.CreateOperation(typByte,@Oper_byte_difer_byte);

  //////// Operaciones con Word ////////////
  {Los operadores deben crearse con su precedencia correcta}
  typWord.OperationPush:=@word_OnPush;
  typWord.OperationLoad:=@word_load;
  opr:=typWord.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typWord,@Oper_word_asig_word);
  opr.CreateOperation(typByte,@Oper_word_asig_byte);
  opr:=typWord.CreateBinaryOperator('+',4,'suma');  //suma
  opr.CreateOperation(typWord,@Oper_word_add_word);
  opr.CreateOperation(typByte,@Oper_word_add_byte);

end;
procedure TGenCod.CreateSystemElements;
{Inicia los elementos del sistema. Se ejecuta cada vez que se compila.}
var
  f: TxpEleFun;  //índice para funciones
begin
  DelayCoded := -1;  //inicia
  //////// Funciones del sistema ////////////
  {Notar que las funciones del sistema no crean espacios de nombres y no se hace
  validación para verificar la duplicidad (para hacer el proceso más rápido).
  Es responsabilidad del progranador, no introducir funciones con conflictos.}
  f := CreateSysFunction('putchar', typByte, @fun_putchar);
  f.CreateParam('',typByte);
  f := CreateSysFunction('delay_ms', typByte, @fun_delay_ms);
  f.CreateParam('',typByte);
  f.adrr:=-1;   //para indicar que no está codificada
  f := CreateSysFunction('delay_ms', typByte, @fun_delay_ms_w);
  f.CreateParam('',typWord);
  f.adrr:=-1;   //para indicar que no está codificada
  f := CreateSysFunction('Inc', typByte, @fun_Inc_byte);
  f.CreateParam('',typByte);
  f := CreateSysFunction('Inc', typByte, @fun_Inc_word);
  f.CreateParam('',typWord);
  f := CreateSysFunction('Dec', typByte, @fun_Dec_byte);
  f.CreateParam('',typByte);
  f := CreateSysFunction('Dec', typByte, @fun_Dec_word);
  f.CreateParam('',typWord);
end;

procedure SetLanguage(lang: string);
begin
  case lang of
  'en': begin
    dicClear;  //it's yet in English
  end;
  'es': begin
    //Update messages
    dicSet('Not implemented.', 'No implementado.');
    dicSet('Invalid value for a bit variable.', 'Valor inválido para una variable bit');
  end;
  end;
end;
end.

