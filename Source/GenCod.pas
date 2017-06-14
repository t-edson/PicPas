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
  Classes, SysUtils, SynEditHighlighter, Graphics, LCLType, LCLProc,
  SynFacilBasic, XpresTypesPIC, XPresParserPIC, XpresElementsPIC, GenCodPic,
  Pic16Utils, MisUtils;
type
    { TGenCod }
    TGenCod = class(TGenCodPic)
    protected
      procedure callParam(fun: TxpEleFun);
      procedure callFunct(fun: TxpEleFun);
    private  //Operaciones con Bit
      procedure bit_LoadToReg(const OpPtr: pointer);
      procedure bit_DefineRegisters;
      procedure bit_SaveToStk;
      procedure CopyInvert_C_to_Z;
      procedure Oper_bit_asig_bit;
      procedure Oper_bit_asig_byte;
      procedure Oper_bit_and_bit;
      procedure Oper_bit_and_byte;
      procedure Oper_bit_or_bit;
      procedure Oper_bit_or_byte;
      procedure Oper_bit_xor_bit;
      procedure Oper_bit_xor_byte;
      procedure Oper_bit_equ_bit;
      procedure Oper_bit_equ_byte;
      procedure Oper_bit_dif_bit;
      procedure Oper_bit_dif_byte;
      procedure Oper_not_bit;
    private  //Operaciones con boolean
      procedure Oper_bool_asig_bool;
      procedure Oper_not_bool;
      procedure Oper_bool_and_bool;
      procedure Oper_bool_or_bool;
      procedure Oper_bool_xor_bool;
      procedure Oper_bool_equ_bool;
      procedure Oper_bool_dif_bool;
    private  //Operaciones con byte
      procedure byte_LoadToReg(const OpPtr: pointer);
      procedure byte_DefineRegisters;
      procedure byte_SaveToStk;
      procedure byte_oper_byte(const InstLW, InstWF: TPIC16Inst);
      procedure Oper_byte_asig_byte;
      procedure Oper_byte_sub_byte;
      procedure Oper_byte_add_byte;
      procedure Oper_byte_add_word;
      procedure Oper_byte_and_byte;
      procedure Oper_byte_and_bit;
      procedure Oper_byte_or_byte;
      procedure Oper_byte_or_bit;
      procedure Oper_byte_xor_byte;
      procedure Oper_byte_xor_bit;
      procedure Oper_byte_equal_byte;
      procedure Oper_byte_difer_byte;
      procedure Oper_byte_difer_bit;
      procedure Oper_byte_great_byte;
      procedure Oper_byte_less_byte;
      procedure Oper_byte_gequ_byte;
      procedure Oper_byte_lequ_byte;
      procedure CodifShift_by_W(aux: TPicRegister; toRight: boolean);
      procedure Oper_byte_shr_byte;
      procedure Oper_byte_shl_byte;
      procedure byte_bit(const OpPtr: pointer; nbit: byte);
      procedure byte_bit0(const OpPtr: pointer);
      procedure byte_bit1(const OpPtr: pointer);
      procedure byte_bit2(const OpPtr: pointer);
      procedure byte_bit3(const OpPtr: pointer);
      procedure byte_bit4(const OpPtr: pointer);
      procedure byte_bit5(const OpPtr: pointer);
      procedure byte_bit6(const OpPtr: pointer);
      procedure byte_bit7(const OpPtr: pointer);
    private  //Operaciones con Word
      procedure word_LoadToReg(const OpPtr: pointer);
      procedure word_DefineRegisters;
      procedure word_SaveToStk;
      procedure Oper_word_asig_word;
      procedure Oper_word_asig_byte;
      procedure Oper_word_equal_word;
      procedure Oper_word_difer_word;
      procedure Oper_word_great_word;
      procedure Oper_word_add_word;
      procedure Oper_word_add_byte;
      procedure Oper_word_sub_word;
      procedure word_Low(const OpPtr: pointer);
      procedure word_High(const OpPtr: pointer);
    private  //Operaciones con Char
      procedure Oper_char_asig_char;
      procedure Oper_char_equal_char;
      procedure Oper_char_difer_char;
    private  //Funciones internas.
      procedure codif_1mseg;
      procedure codif_delay_ms(fun: TxpEleFun);
      procedure expr_end(posExpres: TPosExpres);
      procedure expr_start;
      procedure fun_delay_ms(fun: TxpEleFun);
      procedure fun_Exit(fun: TxpEleFun);
      procedure fun_Inc(fun: TxpEleFun);
      procedure fun_Dec(fun: TxpEleFun);
      procedure fun_Ord(fun: TxpEleFun);
      procedure fun_Chr(fun: TxpEleFun);
      procedure fun_Bit(fun: TxpEleFun);
      procedure fun_SetAsInput(fun: TxpEleFun);
      procedure fun_SetAsOutput(fun: TxpEleFun);
      procedure fun_Word(fun: TxpEleFun);
      procedure fun_SetBank(fun: TxpEleFun);
    protected
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
//  iFlashTmp :=  pic.iFlash; //guarda puntero
//  pic.iFlash := curBloSub;  //empieza a codificar aquí
end;
procedure TGenCod.EndCodeSub;
{debe ser llamado al terminar la codificaión de una subrutina}
begin
//  curBloSub := pic.iFlash;  //indica siguiente posición libre
//  pic.iFlash := iFlashTmp;  //retorna puntero
end;
procedure TGenCod.callParam(fun: TxpEleFun);
{Rutina genérica, que se usa antes de leer los parámetros de }
begin
  {Haya o no, parámetros se debe proceder como en cualquier expresión, asumiendo que
  vamos a devolver una expresión.}
  if RTstate<>nil then begin
    //Si se usan RT en la operación anterior. Hay que salvar en pila
    RTstate.SaveToStk;  //Se guardan por tipo
  end;
  SetResult(fun.typ, coExpres);  //actualiza "RTstate"
end;
procedure TGenCod.callFunct(fun: TxpEleFun);
{Rutina genérica para llamara a una función definida por el usuario}
begin
  fun.iniBnk := CurrBank;   //fija el banco inicial
  //Por ahora, no se implementa paginación, pero despuñes habría que considerarlo.
  _CALL(fun.adrr);  //codifica el salto
  //Verifica las opciones de cambio de banco
  if SetProEndBnk then begin
    //Se debe incluir siempre instrucciones de cambio de banco
    _BANKRESET;
  end else begin
    //Se incluye solo, si el banco pudo haber cambiado
    if fun.BankChanged then begin
      //Ha habido cambios de banco dentro del procedimiento
      _BANKRESET;   //Por seguridad restauramos
      {Un análisis más fino podría determinar si se puede predecir el banco de salida.}
    end;
  end;
end;
procedure TGenCod.CopyInvert_C_to_Z;
begin
  //El resultado está en C (invertido), hay que pasarlo a Z
  _MOVLW($01 << _C);     //carga máscara de C
  _ANDWF(_STATUS, toW);   //el resultado está en Z, corregido en lógica.
  InvertedFromC := true;  //Indica que se ha hecho Z = 'C. para que se pueda optimizar
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
end;
procedure TGenCod.Cod_EndProgram;
//Codifica la parte inicial del programa
begin
  //Code('END');   //inicia la sección de código
end;
procedure TGenCod.expr_start;
//Se ejecuta siempre al iniciar el procesamiento de una expresión.
begin
  //Inicia banderas de estado para empezar a calcular una expresión
  W.used := false;        //Su ciclo de vida es de instrucción
  Z.used := false;        //Su ciclo de vida es de instrucción
  if H<>nil then
    H.used := false;      //Su ciclo de vida es de instrucción
  RTstate := nil;         //Inicia con los RT libres.
  //Limpia tabla de variables temporales
  varFields.Clear;
end;
procedure TGenCod.expr_end(posExpres: TPosExpres);
//Se ejecuta al final de una expresión, si es que no ha habido error.
begin
  if exprLevel = 1 then begin  //el último nivel
//    Code('  ;fin expres');
  end;
  //Muestra informa
end;
////////////operaciones con Bit
procedure TGenCod.bit_LoadToReg(const OpPtr: pointer);
{Carga operando a registros Z.}
var
  Op: ^TOperand;
begin
  Op := OpPtr;
  case Op^.catOp of  //el parámetro debe estar en "res"
  coConst : begin
    if Op^.valBool then
      _BSF(Z.offs, Z.bit)
    else
      _BCF(Z.offs, Z.bit);
  end;
  coVariab: begin
    //La lógica en Z, dene ser normal, proque no hay forma de leerla.
    //Como Z, está en todos los bancos, no hay mucho problema.
    if Op^.Inverted then begin
      //No se usa el registro W
      _BANKSEL(Op^.bank);
      _BCF(Z.offs, Z.bit);
      _BTFSS(Op^.offs, Op^.bit);
      _BSF(Z.offs, Z.bit);
    end else begin
      //No se usa el registro W
      _BANKSEL(Op^.bank);
      _BCF(Z.offs, Z.bit);
      _BTFSC(Op^.offs, Op^.bit);
      _BSF(Z.offs, Z.bit);
    end;
  end;
  coExpres: begin  //ya está en w
    if Op^.Inverted then begin
      //Aquí hay un problema, porque hay que corregir la lógica
      _MOVLW($1 << Z.bit);
      _ANDWF(Z.offs, toW);  //invierte Z
    end else begin
      //No hay mada que hacer
    end;
  end;
  end;
end;
procedure TGenCod.bit_DefineRegisters;
begin
  //No es encesario, definir registros adicionales a W
end;
procedure TGenCod.bit_SaveToStk;
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
procedure TGenCod.Oper_bit_asig_bit;
var
  dg: integer;
begin
  if p1^.catOp <> coVariab then begin  //validación
    GenError('Only variables can be assigned.'); exit;
  end;
  case p2^.catOp of
  coConst : begin
    SetResultExpres_bit(operType, false);  //Realmente, el resultado no es importante
    {Actualmente no existen constantes de tipo "Bit", ya que el número menor que se
    reconoce es de typo byte. Por eso se define Oper_bit_asig_byte(). }
    if p2^.valBool then begin
      _BANKSEL(p1^.bank);
      _BSF(p1^.offs, p1^.bit);
    end else begin
      _BANKSEL(p1^.bank);
      _BCF(p1^.offs, p1^.bit);
    end;
  end;
  coVariab: begin
    SetResultExpres_bit(operType, false);  //Realmente, el resultado no es importante
    if p1^.rVar = p2^.rVar then begin
      //Es asignación de la misma variable.
      if p2^.Inverted then begin  //Es a := not a
          //verifica error.
        _MOVLW(p1^.rVar.BitMask);  //carga máscara
        _BANKSEL(p1^.bank);
        _XORWF(p1^.offs, toF);
      end else begin  //Es a := a
        PutTopComm('No code, by optimizing.');
      end;
    end else begin
      //Es asignación de otra variable
      if p2^.Inverted then begin
        if p1^.bank = p2^.bank then begin //Están en el mismo banco
          //No se usa el registro W
          _BANKSEL(p1^.bank);
          _BCF(p1^.offs, p1^.bit);
          _BTFSS(p2^.offs, p2^.bit);
          _BSF(p1^.offs, p1^.bit);
        end else begin  //Están en bancos diferentes
          //No se usa el registro W
          _BANKSEL(p1^.bank);
          _BCF(p1^.offs, p1^.bit);
          _BANKSEL(p2^.bank);
          _BTFSC(p2^.offs, p2^.bit);
          _GOTO_PEND(dg);  //salto pendiente
          _BANKSEL(p1^.bank);  //cantidad de instrucciones
          _BSF(p1^.offs, p1^.bit);
          pic.codGotoAt(dg, _PC);   //termina de codificar el salto
          _BANKRESET;   //porque no se puede predecir el banco en este punto
        end;
      end else begin
        if p1^.bank = p2^.bank then begin //Están en el mismo banco
          //No se usa el registro W
          _BANKSEL(p1^.bank);
          _BCF(p1^.offs, p1^.bit);
          _BTFSC(p2^.offs, p2^.bit);
          _BSF(p1^.offs, p1^.bit);
        end else begin  //Están en bancos diferentes
          //No se usa el registro W
          _BANKSEL(p1^.bank);
          _BCF(p1^.offs, p1^.bit);
          _BANKSEL(p2^.bank);
          _BTFSS(p2^.offs, p2^.bit);
          _GOTO_PEND(dg);  //salto pendiente
          _BANKSEL(p1^.bank);  //cantidad de instrucciones
          _BSF(p1^.offs, p1^.bit);
          pic.codGotoAt(dg, _PC);   //termina de codificar el salto
          _BANKRESET;   //porque no se puede predecir el banco en este punto
        end;
      end;
    end;
  end;
  coExpres: begin  //ya está en STATUS.Z
    SetResultExpres_bit(operType, false);  //Realmente, el resultado no es importante
    if p2^.Inverted then begin  //está invertido
      //No se usa el registro W
      _BANKSEL(p1^.bank);
      _BCF(p1^.offs, p1^.bit);
      _BTFSS(Z.offs, Z.bit);
      _BSF(p1^.offs, p1^.bit);
    end else begin  //caso normal
      //No se usa el registro W
      _BANKSEL(p1^.bank);
      _BCF(p1^.offs, p1^.bit);
      _BTFSC(Z.offs, Z.bit);
      _BSF(p1^.offs, p1^.bit);
    end;
  end;
  else
    GenError('Not implemented.'); exit;
  end;
end;
procedure TGenCod.Oper_bit_asig_byte;
begin
  if p1^.catOp <> coVariab then begin  //validación
    GenError('Only variables can be assigned.'); exit;
  end;
  case p2^.catOp of
  coConst : begin
    SetResultExpres_bit(operType, false);  //Realmente, el resultado no es importante
    {Esta es la única opción válida, pero solo para los valores 0 y 1}
    if p2^.valInt = 0 then begin
      //No se usa el registro W
      _BANKSEL(p1^.bank);
      _BCF(p1^.offs, p1^.bit);
    end else if p2^.valInt = 1 then begin
      //No se usa el registro W
      _BANKSEL(p1^.bank);
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
end;
procedure TGenCod.Oper_bit_and_bit;
begin
    case catOperation of
    coConst_Const: begin  //AND de dos constantes. Caso especial
      SetResultConst_bit(p1^.valBool and p2^.valBool);
      exit;  //sale aquí, porque es un caso particular
    end;
    coConst_Variab: begin
      if p1^.valBool then begin  //p1 = 1
        //No usa ningún registro
        //Optimiza devolviendo la misma variable
        SetResultVariab_bit(p2^.rVar, p2^.Inverted);  //mantiene la lógica
      end else begin   //p1 = 0
        //No usa ningún registro
        //Optimiza devolviendo constante = 0
        SetResultConst_bit(false);
      end;
    end;
    coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
      if p1^.valBool then begin  //p1 = 1
        //No usa ningún registro
        //Optimiza devolviendo la misma expresión en Z
        SetResultExpres_bit(operType, p2^.Inverted);  //mantiene la lógica
      end else begin   //p1 = 0
        //No usa ningún registro
        //Optimiza devolviendo constante = 0
        SetResultConst_bit(false);
        Z.used := false;  //libera el bit Z, porque ya no importa la expresión
      end;
    end;
    coVariab_Const: begin
      if p2^.valBool then begin  //p2 = 1
        //No usa ningún registro
        //Optimiza devolviendo la misma variable
        SetResultVariab_bit(p1^.rVar, p1^.Inverted);  //mantiene la lógica
      end else begin   //p2 = 0
        //No usa ningún registro
        //Optimiza devolviendo constante = 0
        SetResultConst_bit(false);
      end;
    end;
    coVariab_Variab:begin
      if p1^.rVar = p2^.rVar then begin
        //Es la misma variable: a AND a
        //Optimiza devolviendo la misma variable
        SetResultVariab_bit(p1^.rVar, p1^.Inverted);
      end else begin
        if p1^.Inverted and p2^.Inverted then begin
          //Por La ley de Morgan, se convierten em OR
          p1^.Inverted := false;
          p2^.Inverted := false;
          Oper_bit_or_bit;  //procesa como OR
          res.Invert;   //y niega todo
          exit;
        end else if p1^.Inverted then begin
          //Este caso es lo inverso, no vale la pena implementarlo de nuevo
          ExchangeP1_P2;
          Oper_bit_and_bit;  //procesa como OR
          exit;
        end else if p2^.Inverted then begin
          SetResultExpres_bit(operType, false);  //Fija resultado
          //Mueve p2 a Z
          _BANKSEL(p2^.bank);
          _MOVLW(p2^.rVar.BitMask);
          _ANDWF(p2^.offs, toW);  //Z aparece normal
          //Aplica un AND entre Z y p1,
          _BANKSEL(p1^.bank);
          _BTFSS(p1^.offs, p1^.bit);   //Si es 1, deja tal cual
          _BCF(Z.offs, Z.bit);     //Si es 0, devuelve cero
        end else begin  //Caso normal
          SetResultExpres_bit(operType, true);  //Fija resultado, con lógica invertida
          //Mueve p2 a Z
          _BANKSEL(p2^.bank);
          _MOVLW(p2^.rVar.BitMask);
          _ANDWF(p2^.offs, toW);  //Z está invertido
          //Aplica un AND entre Z' y p1. Trabajamos con lógica invertida, por optimización
          _BANKSEL(p1^.bank);
          _BTFSS(p1^.offs, p1^.bit); //Si es 1, deja tal cual (pero sigue con lógica invertida)
          _BSF(Z.offs, Z.bit);       //Si es 0, devuelve cero (1 porque debe quedar con lógica invertida)
        end;
      end;
    end;
    coVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
      if p1^.Inverted and p2^.Inverted then begin
        //Por La ley de Morgan, se convierten em OR
        p1^.Inverted := false;
        p2^.Inverted := false;
        Oper_bit_or_bit;  //procesa como OR
        exit;
      end else if p1^.Inverted then begin  //lógica invertida en p1
        SetResultExpres_bit(operType, false); //Fija resultado
        //Aplica un AND entre p1' y Z.
        _BANKSEL(p1^.bank);
        _BTFSC(p1^.offs, p1^.bit); //Si es 0, deja tal cual
        _BCF(Z.offs, Z.bit);      //Si es 1, devuelve cero
      end else if p2^.Inverted then begin  //lógica invertida en Z
        SetResultExpres_bit(operType, true); //Deja la lógica invertida por optimización
        //Aplica un AND entre p1 y Z'.
        _BANKSEL(p1^.bank);
        _BTFSS(p1^.offs, p1^.bit); //Si es 1, deja tal cual
        _BSF(Z.offs, Z.bit);       //Si es 0, devuelve cero (1, porque es lógica es invertida)
      end else begin  //lógica normal
        SetResultExpres_bit(operType, false); //Fija resultado
        //Aplica un AND entre p1 y Z.
        _BANKSEL(p1^.bank);
        _BTFSS(p1^.offs, p1^.bit); //Si es 1, deja tal cual
        _BCF(Z.offs, Z.bit);      //Si es 0, devuelve cero
      end;
    end;
    coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
      ExchangeP1_P2;       //Convierte en coConst_Expres
      Oper_bit_and_bit;
      exit;
    end;
    coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
      ExchangeP1_P2;       //Convierte en coVariab_Expres
      Oper_bit_and_bit;
      exit;
    end;
    coExpres_Expres:begin
      //la expresión p1 debe estar salvada y p2 en el acumulador
      p1^.catOp := coVariab;
      p1^.rVar := GetVarBitFromStk;
      catOperation := TCatOperation((Ord(p1^.catOp) << 2) or ord(p2^.catOp));
      //Luego el caso es similar a variable-expresión
      Oper_bit_and_bit;
      FreeStkRegisterBit;   //Libera pila. Ya se usó el dato.
    end;
    else
      GenError('Not implemented.'); exit;
    end;
end;
procedure TGenCod.Oper_bit_and_byte;
begin
  if p2^.catOp <> coConst then begin
    GenError('Incompatible types: (bit) AND (byte).'); exit;
  end;
  //p2 es constante
  if p2^.valInt = 0 then begin
    p2^.typ := typBit;   //convierte en bit
    p2^.valBool := false;
    Oper_bit_and_bit;  //opera como bit
  end else if p2^.valInt = 1 then begin
    p2^.typ := typBit;   //convierte en bit
    p2^.valBool := true;
    Oper_bit_and_bit;  //opera como bit
  end else begin
    GenError('Incompatible types: (bit) AND (byte).'); exit;
  end;
end;
procedure TGenCod.Oper_bit_or_bit;
begin
    case catOperation of
    coConst_Const: begin  //AND de dos constantes. Caso especial
      SetResultConst_bit(p1^.valBool or p2^.valBool);
      exit;  //sale aquí, porque es un caso particular
    end;
    coConst_Variab: begin
      if p1^.valBool then begin  //p1 = 1
        //No usa ningún registro
        //Optimiza devolviendo constante = 1
        SetResultConst_bit(true);
      end else begin   //p1 = 0
        //No usa ningún registro
        //Optimiza devolviendo la misma variable
        SetResultVariab_bit(p2^.rVar, p2^.Inverted);
      end;
    end;
    coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
      if p1^.valBool then begin  //p1 = 1
        //No usa ningún registro
        //Optimiza devolviendo constante = 1
        SetResultConst_bit(true);
        Z.used := false;  //libera el bit Z, porque ya no importa la expresión
      end else begin   //p1 = 0
        //No usa ningún registro
        //Optimiza devolviendo la misma expresión en Z
        SetResultExpres_bit(operType, p2^.Inverted);  //mantiene la lógica
      end;
    end;
    coVariab_Const: begin
      if p2^.valBool then begin  //p2 = 1
        //No usa ningún registro
        //Optimiza devolviendo constante = 1
        SetResultConst_bit(true);
      end else begin   //p2 = 0
        //No usa ningún registro
        //Optimiza devolviendo la misma variable
        SetResultVariab_bit(p1^.rVar, p1^.Inverted);
      end;
    end;
    coVariab_Variab:begin
      if p1^.rVar = p2^.rVar then begin
        //Es la misma variable: a OR a
        //Optimiza devolviendo la misma variable
        SetResultVariab_bit(p1^.rVar, p1^.Inverted);
      end else begin
        if p1^.Inverted and p2^.Inverted then begin
          //Por La ley de Morgan, se convierten em AND
          p1^.Inverted := false;
          p2^.Inverted := false;
          Oper_bit_and_bit;  //procesa como OR
          res.Invert;
          exit;
        end else if p1^.Inverted then begin
          //Este caso es lo inverso, no vale la pena implementarlo de nuevo
          ExchangeP1_P2;
          Oper_bit_or_bit;  //procesa como OR
          exit;
        end else if p2^.Inverted then begin
          SetResultExpres_bit(operType, false);  //Fija resultado
          //Mueve p2 a Z
          _BANKSEL(p2^.bank);
          _MOVLW(p2^.rVar.BitMask);
          _ANDWF(p2^.offs, toW);  //Z aparece normal
          //Aplica un OR entre Z y p1,
          _BANKSEL(p1^.bank);
          _BTFSC(p1^.offs, p1^.bit);   //Si es 0, deja tal cual
          _BSF(Z.offs, Z.bit);     //Si es 1, devuelve uno
        end else begin  //Caso normal
          SetResultExpres_bit(operType, true);  //Fija resultado, con lógica invertida
          //Mueve p2 a Z
          _BANKSEL(p2^.bank);
          _MOVLW(p2^.rVar.BitMask);
          _ANDWF(p2^.offs, toW);  //Z está invertido
          //Aplica un OR entre p1 y Z'. Trabajamos con lógica invertida, por optimización
          _BANKSEL(p1^.bank);
          _BTFSC(p1^.offs, p1^.bit); //Si es 0, deja tal cual (pero sigue con lógica invertida)
          _BCF(Z.offs, Z.bit);       //Si es 1, devuelve 1 (0 porque debe quedar con lógica invertida)
        end;
      end;
    end;
    coVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
      if p1^.Inverted and p2^.Inverted then begin
        //Por La ley de Morgan, se convierten em AND
        p1^.Inverted := false;
        p2^.Inverted := false;
        Oper_bit_and_bit;  //procesa como OR
        exit;
      end else if p1^.Inverted then begin  //lógica invertida
        SetResultExpres_bit(operType, false);  //Fija resultado
        //Aplica un OR entre p1' y Z.
        _BANKSEL(p1^.bank);
        _BTFSS(p1^.offs, p1^.bit);   //Si es 1, deja tal cual
        _BSF(Z.offs, Z.bit);     //Si es 0, devuelve uno
      end else if p2^.Inverted then begin  //lógica invertida en Z
        SetResultExpres_bit(operType, true); //Deja la lógica invertida por optimización
        //Aplica un OR entre p1 y Z.
        _BANKSEL(p1^.bank);
        _BTFSC(p1^.offs, p1^.bit);   //Si es 0, deja tal cual
        _BCF(Z.offs, Z.bit);     //Si es 1, devuelve uno (0 porque es lógica invertida)
      end else begin   //lógica normal
        SetResultExpres_bit(operType, false);  //Fija resultado
        //Aplica un OR entre p1 y Z.
        _BANKSEL(p1^.bank);
        _BTFSC(p1^.offs, p1^.bit);   //Si es 0, deja tal cual
        _BSF(Z.offs, Z.bit);     //Si es 1, devuelve uno
      end;
    end;
    coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
      ExchangeP1_P2;       //Convierte en coConst_Expres
      Oper_bit_or_bit;
      exit;
    end;
    coExpres_Variab:begin  //la expresión p2 se evaluó y esta en W
      ExchangeP1_P2;       //Convierte en coVariab_Expres
      Oper_bit_or_bit;
      exit;
    end;
    coExpres_Expres:begin
      //la expresión p1 debe estar salvada y p2 en el acumulador
      p1^.catOp := coVariab;
      p1^.rVar  := GetVarBitFromStk;
      catOperation := TCatOperation((Ord(p1^.catOp) << 2) or ord(p2^.catOp));
      //Luego el caso es similar a variable-expresión
      Oper_bit_or_bit;
      FreeStkRegisterBit;   //Libera pila. Ya se usó el dato.
    end;
    else
      GenError('Not implemented.'); exit;
    end;
end;
procedure TGenCod.Oper_bit_or_byte;
begin
  if p2^.catOp <> coConst then begin
    GenError('Incompatible types: (bit) OR (byte).'); exit;
  end;
  //p2 es constante
  if p2^.valInt = 0 then begin
    p2^.typ := typBit;   //convierte en bit
    p2^.valBool := false;
    Oper_bit_or_bit;  //opera como bit
  end else if p2^.valInt = 1 then begin
    p2^.typ := typBit;   //convierte en bit
    p2^.valBool := true;
    Oper_bit_or_bit;  //opera como bit
  end else begin
    GenError('Incompatible types: (bit) OR (byte).'); exit;
  end;
end;
procedure TGenCod.Oper_bit_xor_bit;
begin
    case catOperation of
    coConst_Const: begin  //XOR de dos constantes. Caso especial
      SetResultConst_bit(p1^.valBool xor p2^.valBool);
      exit;  //sale aquí, porque es un caso particular
    end;
    coConst_Variab: begin
      if p1^.valBool then begin  //p1 = 1
        //Optimiza devolviendo la variable invertida
        SetResultVariab_bit(p2^.rVar, not p2^.Inverted);
      end else begin   //p1 = 0
        //Optimiza devolviendo la misma variable
        SetResultVariab_bit(p2^.rVar, p2^.Inverted);
      end;
    end;
    coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
      if p1^.valBool then begin  //p1 = 1
        //Optimiza devolviendo la expresión invertida
        SetResultExpres_bit(operType, not p2^.Inverted);  //mantiene la lógica
      end else begin   //p1 = 0
        //Optimiza devolviendo la misma expresión en Z
        SetResultExpres_bit(operType, p2^.Inverted);  //mantiene la lógica
      end;
    end;
    coVariab_Const: begin
      ExchangeP1_P2;  //Convierte a coConst_Variab
      Oper_bit_xor_bit;
      exit;
    end;
    coVariab_Variab:begin
      if p1^.rVar = p2^.rVar then begin
        //Es la misma variable: a XOR a
        //Optimiza devolviendo cero
        SetResultConst_bit(false);
      end else begin
        if p1^.Inverted and p2^.Inverted then begin
          p1^.Inverted := false;
          p2^.Inverted := false;
          Oper_bit_xor_bit;  //es lo mismo
          exit;
        end else if p1^.Inverted then begin
          //Este caso es lo inverso, no vale la pena implementarlo de nuevo
          ExchangeP1_P2;
          Oper_bit_xor_bit;  //procesa como OR
          exit;
        end else if p2^.Inverted then begin
          //a XOR b' = (z XOR b)'
          p2^.Inverted := false;
          Oper_bit_xor_bit;
          res.Invert;  //Invierte la lógica
          exit;
        end else begin  //Caso normal
          {Se optimiza bien, esta operación, porque es una rutina muy usada para loa
          las operaciones XOR, y porque también se utiliza el XOR para las comparaciones
          de bits.}
          if p1^.bit = p2^.bit then begin
            //Están en el mismo bit, se puede optimizar
            SetResultExpres_bit(operType, true);  //Fija resultado
            _BANKSEL(p2^.bank);
            _MOVF(p2^.offs, toW);  //mueve a W
            _BANKSEL(p1^.bank);
            _XORWF(p1^.offs, toW);      //APlica XOR,
            _ANDLW(p1^.rVar.BitMask);  //Aplica máscara al bit que nos interesa, queda en Z, invertido
          end else if p1^.bit = p2^.bit +1 then begin
            //p1 está a un bit a la izquierda, se puede optimizar
            SetResultExpres_bit(operType, true);  //Fija resultado
            _BANKSEL(p2^.bank);
            _RLF(p2^.offs, toW);  //alinea y mueve a W
            _BANKSEL(p1^.bank);
            _XORWF(p1^.offs, toW);      //APlica XOR,
            _ANDLW(p1^.rVar.BitMask);  //Aplica máscara al bit que nos interesa, queda en Z, invertido
          end else if p1^.bit = p2^.bit-1 then begin
            //p1 está a un bit a la derecha, se puede optimizar
            SetResultExpres_bit(operType, true);  //Fija resultado
            _BANKSEL(p2^.bank);
            _RRF(p2^.offs, toW);  //alinea y mueve a W
            _BANKSEL(p1^.bank);
            _XORWF(p1^.offs, toW);      //APlica XOR,
            _ANDLW(p1^.rVar.BitMask);  //Aplica máscara al bit que nos interesa, queda en Z, invertido
          end else if abs(p1^.bit - p2^.bit) = 4 then begin
            //p1 está a un nibble de distancia, se puede optimizar
            SetResultExpres_bit(operType, true);  //Fija resultado
            _BANKSEL(p2^.bank);
            _SWAPF(p2^.offs, toW);  //alinea y mueve a W
            _BANKSEL(p1^.bank);
            _XORWF(p1^.offs, toW);      //APlica XOR,
            _ANDLW(p1^.rVar.BitMask);  //Aplica máscara al bit que nos interesa, queda en Z, invertido
          end else begin
            //La forma larga
            SetResultExpres_bit(operType, false);  //Fija resultado,
            //Mueve p2 a Z
            _BANKSEL(p2^.bank);
            _MOVLW(p2^.rVar.BitMask);
            _ANDWF(p2^.offs, toW);  //Z está invertido
            //Aplica un XOR entre p1 y Z'.
            _BANKSEL(p1^.bank);
            _MOVLW($1 << Z.bit);   //carga máscara, y deja lista si es que se necesita
            _BTFSS(p1^.offs, p1^.bit);  //Si es 1, invierte, pero ya esta invertido, así que lo deja
            _ANDWF(Z.offs, toW);  //Si es 0, deja tal cual, pero como está invertido, hay que corregir
          end;
        end;
      end;
    end;
    coVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
      if p1^.Inverted and p2^.Inverted then begin
        p1^.Inverted := false;
        p2^.Inverted := false;
        Oper_bit_xor_bit;   //es lo mismo
        exit;
      end else if p1^.Inverted then begin  //lógica invertida
        SetResultExpres_bit(operType, false);  //Fija resultado
        //Aplica un XOR entre p1' y Z.
        _BANKSEL(p1^.bank);
        _MOVLW($1 << Z.bit);   //carga máscara, y deja lista si es eu se necesita
        _BTFSS(p1^.offs, p1^.bit);   //Si es 1(0), deja tal cual
        _ANDWF(Z.offs, toW);     //Si es 0(1), invierte
      end else if p2^.Inverted then begin  //lógica invertida en Z
        SetResultExpres_bit(operType, false);  //Fija resultado
        //Aplica un XOR entre p1 y Z'.
        _BANKSEL(p1^.bank);
        _MOVLW($1 << Z.bit);   //carga máscara, y deja lista si es eu se necesita
        _BTFSS(p1^.offs, p1^.bit);   //Si es 1, invierte (deja igual porque ya está invertido)
        _ANDWF(Z.offs, toW);     //Si es 0, deja tal cual (realmente debe invertir)
      end else begin   //lógica normal
        SetResultExpres_bit(operType, false);  //Fija resultado
        //Aplica un XOR entre p1 y Z.
        _BANKSEL(p1^.bank);
        _MOVLW($1 << Z.bit);   //carga máscara, y deja lista si es eu se necesita
        _BTFSC(p1^.offs, p1^.bit);  //Si es 0, deja tal cual
        _ANDWF(Z.offs, toW);         //Si es 1, invierte
      end;
    end;
    coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
      ExchangeP1_P2;       //Convierte en coConst_Expres
      Oper_bit_xor_bit;
      exit;
    end;
    coExpres_Variab:begin  //la expresión p2 se evaluó y esta en W
      ExchangeP1_P2;       //Convierte en coVariab_Expres
      Oper_bit_xor_bit;
      exit;
    end;
    coExpres_Expres:begin
      //la expresión p1 debe estar salvada y p2 en el acumulador
      p1^.catOp := coVariab;
      p1^.rVar := GetVarBitFromStk;
      catOperation := TCatOperation((Ord(p1^.catOp) << 2) or ord(p2^.catOp));
      //Luego el caso es similar a coVariab_Expres
      Oper_bit_xor_bit;
      FreeStkRegisterBit;   //Libera pila. Ya se usó el dato.
    end;
    else
      GenError('Not implemented.'); exit;
    end;
end;
procedure TGenCod.Oper_bit_xor_byte;
begin
  if p2^.catOp <> coConst then begin
    GenError('Incompatible types: (bit) XOR (byte).'); exit;
  end;
  //p2 es constante
  if p2^.valInt = 0 then begin
    p2^.typ := typBit;   //convierte en bit
    p2^.valBool := false;
    Oper_bit_xor_bit;  //opera como bit
  end else if p2^.valInt = 1 then begin
    p2^.typ := typBit;   //convierte en bit
    p2^.valBool := true;
    Oper_bit_xor_bit;  //opera como bit
  end else begin
    GenError('Incompatible types: (bit) XOR (byte).'); exit;
  end;
end;
procedure TGenCod.Oper_bit_equ_bit;
begin
  //Una comparación, es lo mismo que un XOR negado
  Oper_bit_xor_bit;  //puede devolver error
  //Niega la lógica
  res.Invert;  //Invierte la lógica
  res.typ := typBool;   //devuelve boolean
end;
procedure TGenCod.Oper_bit_equ_byte;
begin
  //Una comparación, es lo mismo que un XOR negado
  Oper_bit_xor_byte;  //puede devolver error
  res.Invert;  //Invierte la lógica
  res.typ := typBool;   //devuelve boolean
end;
procedure TGenCod.Oper_bit_dif_bit;
begin
  //Esta comparación, es lo mismo que un XOR
  Oper_bit_xor_bit;  //puede devolver error
  res.typ := typBool;   //devuelve boolean
end;
procedure TGenCod.Oper_bit_dif_byte;
begin
  //Una comparación, es lo mismo que un XOR
  Oper_bit_xor_byte;  //puede devolver error
  res.typ := typBool;   //devuelve boolean
end;
procedure TGenCod.Oper_not_bit;
begin
  case p1^.catOp of
  coConst : begin
    {Actualmente no existen constantes de tipo "Bit", pero si existieran, sería así}
    SetResultConst_bit(not p1^.valBool);
  end;
  coVariab: begin
    {Optimiza devolviendo la misma variable, pero invirtiendo la lógica.}
    SetResultVariab_bit(p1^.rVar, not p1^.Inverted);
  end;
  coExpres: begin  //ya está en STATUS.Z
    //No cambiamos su valor, sino su significado.
    SetResultExpres_bit(operType, not p1^.Inverted);
  end;
  else
    GenError('Not implemented.'); exit;
  end;
end;
////////////operaciones con Boolean
procedure TGenCod.Oper_bool_asig_bool;
begin
  Oper_bit_asig_bit;  //A bajo nivel es lo mismo
end;
procedure TGenCod.Oper_not_bool;
begin
  Oper_not_bit;  //A bajo nivel es lo mismo
  res.typ := typBool;  //pero debe devolver este tipo
end;
procedure TGenCod.Oper_bool_and_bool;
begin
  Oper_bit_and_bit;  //A bajo nivel es lo mismo
  res.typ := typBool;  //pero debe devolver este tipo
end;
procedure TGenCod.Oper_bool_or_bool;
begin
  Oper_bit_or_bit;  //A bajo nivel es lo mismo
  res.typ := typBool;  //pero debe devolver este tipo
end;
procedure TGenCod.Oper_bool_xor_bool;
begin
  Oper_bit_xor_bit;  //A bajo nivel es lo mismo
  res.typ := typBool;  //pero debe devolver este tipo
end;
procedure TGenCod.Oper_bool_equ_bool;
begin
  Oper_bit_equ_bit;  //Es lo mismo
end;
procedure TGenCod.Oper_bool_dif_bool;
begin
  Oper_bit_dif_bit;
end;
////////////operaciones con Byte
procedure TGenCod.byte_LoadToReg(const OpPtr: pointer);
{Carga operando a registros de trabajo.}
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
procedure TGenCod.byte_DefineRegisters;
begin
  //No es encesario, definir registros adicionales a W
end;
procedure TGenCod.byte_SaveToStk;
var
  stk: TPicRegister;
begin
  stk := GetStkRegisterByte;  //pide memoria
  //guarda W
  _BANKSEL(stk.bank);
  _MOVWF(stk.offs);PutComm(';save W');
  stk.used := true;
end;
procedure TGenCod.Oper_byte_asig_byte;
begin
  if p1^.catOp <> coVariab then begin  //validación
    GenError('Only variables can be assigned.'); exit;
  end;
  case p2^.catOp of
  coConst : begin
    SetResultExpres_byte(operType);  //Realmente, el resultado no es importante
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
    SetResultExpres_byte(operType);  //Realmente, el resultado no es importante
    _BANKSEL(p2^.bank);  //verifica banco destino
    _MOVF(p2^.offs, toW);
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVWF(p1^.offs);
  end;
  coExpres: begin  //ya está en w
    SetResultExpres_byte(operType);  //Realmente, el resultado no es importante
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVWF(p1^.offs);
  end;
  else
    GenError('No soportado'); exit;
  end;
end;
procedure TGenCod.byte_oper_byte(const InstLW, InstWF:TPIC16Inst);
{Rutina general en operaciones con bytes}
var
  r: TPicRegister;
begin
  case catOperation of
  coConst_Variab: begin
    SetResultExpres_byte(operType);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    CodAsmK(InstLW, p1^.valInt);  //deja en W
  end;
  coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    SetResultExpres_byte(operType);
    CodAsmK(InstLW, p1^.valInt);  //deja en W
  end;
  coVariab_Const: begin
    SetResultExpres_byte(operType);
    _MOVLW(p2^.valInt);
    _BANKSEL(p1^.bank);
    CodAsmFD(InstWF, p1^.offs, toW);  //deja en W
  end;
  coVariab_Variab:begin
    SetResultExpres_byte(operType);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    _BANKSEL(p1^.bank);
    CodAsmFD(InstWF, p1^.offs, toW);  //deja en W
  end;
  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetResultExpres_byte(operType);
    _BANKSEL(p1^.bank);
    CodAsmFD(InstWF, p1^.offs, toW);  //deja en W
  end;
  coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    SetResultExpres_byte(operType);
    CodAsmK(InstLW, p2^.valInt);  //deja en W
  end;
  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetResultExpres_byte(operType);
    _BANKSEL(p2^.bank);
    CodAsmFD(InstWF, p2^.offs, toW);  //deja en W
  end;
  coExpres_Expres:begin
    SetResultExpres_byte(operType);
    //la expresión p1 debe estar salvada y p2 en el acumulador
    FreeStkRegisterByte(r);   //libera pila porque se usará el dato ahí contenido
    _BANKSEL(r.bank);
    CodAsmFD(InstWF, r.offs, toW);  //opera directamente al dato que había en la pila. Deja en W
  end;
  end;
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
  r: TPicRegister;
begin
  case catOperation of
  coExpres_Expres:begin
    {Este es el único caso que no se puede invertir, por la posición de los operandos en
     la pila.}
    //la expresión p1 debe estar salvada y p2 en el acumulador
    p1^.catOp := coVariab;  //Convierte a variable
    p1^.rVar := GetVarByteFromStk;
    catOperation := TCatOperation((Ord(p1^.catOp) << 2) or ord(p2^.catOp));
    //Luego el caso es similar a coVariab_Expres
    Oper_byte_add_word;
    FreeStkRegisterByte(r);   //libera pila porque ya se usó el dato ahí contenido
  end;
  else
    //Para los otros casos, funciona
    ExchangeP1_P2;   //Invierte los operandos
    Oper_word_add_byte; //Y llama a la función opuesta
  end;
end;
procedure TGenCod.Oper_byte_sub_byte;
var
  r: TPicRegister;
begin
  case catOperation of
  coConst_Const:begin  //suma de dos constantes. Caso especial
    SetResultConst_byte(p1^.valInt-p2^.valInt);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end;
  coConst_Variab: begin
    SetResultExpres_byte(operType);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    _SUBLW(p1^.valInt);   //K - W -> W
  end;
  coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    SetResultExpres_byte(operType);
    _SUBLW(p1^.valInt);   //K - W -> W
  end;
  coVariab_Const: begin
    SetResultExpres_byte(operType);
    _MOVLW(p2^.valInt);
    _BANKSEL(p1^.bank);
    _SUBWF(p1^.offs, toW);  //F - W -> W
  end;
  coVariab_Variab:begin
    SetResultExpres_byte(operType);
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    _BANKSEL(p1^.bank);
    _SUBWF(p1^.offs, toW);  //F - W -> W
  end;
  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetResultExpres_byte(operType);
    _BANKSEL(p1^.bank);
    _SUBWF(p1^.offs, toW);  //F - W -> W
  end;
  coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    SetResultExpres_byte(operType);
    _SUBLW(p2^.valInt);  //K - W -> W
    _SUBLW(0);  //K - W -> W   //invierte W
  end;
  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetResultExpres_byte(operType);
    _BANKSEL(p2^.bank);
    _SUBWF(p2^.offs, toW);  //F - W -> W
    _SUBLW(0);  //K - W -> W   //invierte W
  end;
  coExpres_Expres:begin
    SetResultExpres_byte(operType);
    //la expresión p1 debe estar salvada y p2 en el acumulador
    FreeStkRegisterByte(r);   //libera pila porque se usará el dato ahí contenido
    _BANKSEL(r.bank);
    _SUBWF(r.offs, toW);  //opera directamente al dato que había en la pila. Deja en W
  end;
  else  //caso general
    byte_oper_byte(SUBLW, SUBWF);
  end;
end;
procedure TGenCod.Oper_byte_and_byte;
begin
  if catOperation  = coConst_Const then begin  //suma de dos constantes. Caso especial
    SetResultConst_byte(p1^.valInt and p2^.valInt);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end else  //caso general
    byte_oper_byte(ANDLW, ANDWF);
end;
procedure TGenCod.Oper_byte_and_bit;
begin
  {No hay problema en usar siempre ExchangeP1_P2, porque el caso Expresión-Expresión,
  no se implementa Oper_bit_and_byte.}
  ExchangeP1_P2;   //Invierte los operandos
  Oper_bit_and_byte;
end;
procedure TGenCod.Oper_byte_or_byte;
begin
  if catOperation  = coConst_Const then begin  //suma de dos constantes. Caso especial
    SetResultConst_byte(p1^.valInt or p2^.valInt);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end else  //caso general
    byte_oper_byte(IORLW, IORWF);
end;
procedure TGenCod.Oper_byte_or_bit;
begin
  {No hay problema en usar siempre ExchangeP1_P2, porque el caso Expresión-Expresión,
  no se implementa Oper_bit_or_byte.}
  ExchangeP1_P2;   //Invierte los operandos
  Oper_bit_or_byte;
end;
procedure TGenCod.Oper_byte_xor_byte;
begin
  if catOperation  = coConst_Const then begin  //suma de dos constantes. Caso especial
    SetResultConst_byte(p1^.valInt xor p2^.valInt);  //puede generar error
    exit;  //sale aquí, porque es un caso particular
  end else  //caso general
    byte_oper_byte(XORLW, XORWF);
end;
procedure TGenCod.Oper_byte_xor_bit;
begin
  {No hay problema en usar siempre ExchangeP1_P2, porque el caso Expresión-Expresión,
  no se implementa Oper_bit_xor_byte.}
  ExchangeP1_P2;   //Invierte los operandos
  Oper_bit_xor_byte;
end;
procedure TGenCod.Oper_byte_equal_byte;
var
  r: TPicRegister;
begin
  case catOperation of
  coConst_Const: begin  //compara constantes. Caso especial
    SetResultConst_bool(p1^.valInt = p2^.valInt);
  end;
  coConst_Variab: begin
    SetResultExpres_bool(operType, false);   //Se pide Z para el resultado
    _MOVLW(p1^.valInt);
    _BANKSEL(p2^.bank);  //verifica banco destino
    _SUBWF(p2^.offs, toW);  //si iguales _Z=1
  end;
  coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    SetResultExpres_bool(operType, false);   //Se pide Z para el resultado
    _SUBLW(p1^.valInt);  //si iguales _Z=1
  end;
  coVariab_Const: begin
    SetResultExpres_bool(operType, false);   //Se pide Z para el resultado
    _MOVLW(p2^.valInt);
    _BANKSEL(p1^.bank);  //verifica banco destino
    _SUBWF(p1^.offs, toW);  //si iguales _Z=1
  end;
  coVariab_Variab:begin
    SetResultExpres_bool(operType, false);   //Se pide Z para el resultado
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVF(p1^.offs, toW);
    _BANKSEL(p2^.bank);  //verifica banco destino
    _SUBWF(p2^.offs, toW);  //si iguales _Z=1
  end;
  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetResultExpres_bool(operType, false);   //Se pide Z para el resultado
    //ReserveW; if HayError then exit;
    _BANKSEL(p1^.bank);  //verifica banco destino
    _SUBWF(p1^.offs, toW);  //si iguales _Z=1
  end;
  coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    SetResultExpres_bool(operType, false);   //Se pide Z para el resultado
    //ReserveW; if HayError then exit;
    _SUBLW(p2^.valInt);  //si iguales _Z=1
  end;
  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetResultExpres_bool(operType, false);   //Se pide Z para el resultado
    //ReserveW; if HayError then exit;
    _BANKSEL(p2^.bank);  //verifica banco destino
    _SUBWF(p2^.offs, toW);  //si iguales _Z=1
  end;
  coExpres_Expres:begin
    SetResultExpres_bool(operType, false);   //Se pide Z para el resultado
    //la expresión p1 debe estar salvada y p2 en el acumulador
    FreeStkRegisterByte(r);   //libera pila porque se usará el dato ahí contenido
    _BANKSEL(r.bank);  //verifica banco destino
    _SUBWF(r.offs, toW);  //compara directamente a lo que había en pila.
  end;
  else
    GenError('Not implemented.'); exit;
  end;
end;
procedure TGenCod.Oper_byte_difer_byte;
begin
  Oper_byte_equal_byte;  //usa el mismo código
  res.Invert;  //Invierte la lógica
end;
procedure TGenCod.Oper_byte_difer_bit;
begin
  {No hay problema en usar siempre ExchangeP1_P2, porque el caso Expresión-Expresión,
  no se implementa Oper_bit_dif_byte.}
  ExchangeP1_P2;
  Oper_bit_dif_byte;
end;
procedure TGenCod.Oper_byte_great_byte;
var
  r, tmp: TPicRegister;
begin
  case catOperation of
  coConst_Const: begin  //compara constantes. Caso especial
    SetResultConst_bool(p1^.valInt > p2^.valInt);
  end;
  coConst_Variab: begin
    if p1^.valInt = 0 then begin
      //0 es mayor que nada
      SetResultConst_bool(false);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      SetResultExpres_bool(operType, false);   //Se pide Z para el resultado
      _MOVLW(p1^.valInt);
      _BANKSEL(p2^.bank);  //verifica banco destino
      _SUBWF(p2^.offs, toW);  //Si p1 > p2: C=0.
      CopyInvert_C_to_Z; //Pasa C a Z (invirtiendo)
    end;
  end;
  coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
    if p1^.valInt = 0 then begin
      //0 es mayor que nada
      SetResultConst_bool(false);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      //Optimiza rutina, usando: A>B  equiv. NOT (B<=A-1)
      //Se necesita asegurar que p1, es mayo que cero.
      SetResultExpres_bool(operType, true);  //invierte la lógica
      //p2, ya está en W
      _SUBLW(p1^.valInt-1);  //Si p1 > p2: C=0.
      CopyInvert_C_to_Z; //Pasa C a Z (invirtiendo)
    end;
  end;
  coVariab_Const: begin
    if p2^.valInt = 255 then begin
      //nada es mayor que 255
      SetResultConst_bool(false);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      SetResultExpres_bool(operType, false);   //Se pide Z para el resultado
      _BANKSEL(p1^.bank);  //verifica banco destino
      _MOVF(p1^.offs, toW);
      _SUBLW(p2^.valInt);  //Si p1 > p2: C=0.
      CopyInvert_C_to_Z; //Pasa C a Z (invirtiendo)
    end;
  end;
  coVariab_Variab:begin
    SetResultExpres_bool(operType, false);   //Se pide Z para el resultado
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVF(p1^.offs, toW);
    _BANKSEL(p2^.bank);  //verifica banco destino
    _SUBWF(p2^.offs, toW);  //Si p1 > p2: C=0.
    CopyInvert_C_to_Z; //Pasa C a Z (invirtiendo)
  end;
  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetResultExpres_bool(operType, false);   //Se pide Z para el resultado
    tmp := GetAuxRegisterByte;  //Se pide registro auxiliar
    _MOVWF(tmp.offs);    //guarda resultado de expresión
    //Ahora es como coVariab_Variab
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVF(p1^.offs, toW);
    _BANKSEL(tmp.bank);  //verifica banco destino
    _SUBWF(tmp.offs, toW);  //Si p1 > tmp: C=0.
    CopyInvert_C_to_Z; //Pasa C a Z (invirtiendo)
    tmp.used := false;  //libera
  end;
  coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    if p2^.valInt = 255 then begin
      //nada es mayor que 255
      SetResultConst_bool(false);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      SetResultExpres_bool(operType, false);   //Se pide Z para el resultado
  //    p1, ya está en W
      _SUBLW(p2^.valInt);  //Si p1 > p2: C=0.
      CopyInvert_C_to_Z; //Pasa C a Z (invirtiendo)
    end;
  end;
  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    SetResultExpres_bool(operType, false);   //Se pide Z para el resultado
    _BANKSEL(p2^.bank);  //verifica banco destino
    _SUBWF(p2^.offs, toW);  //Si p1 > p2: C=0.
    CopyInvert_C_to_Z; //Pasa C a Z (invirtiendo)
  end;
  coExpres_Expres:begin
    //la expresión p1 debe estar salvada y p2 en el acumulador
    p1^.catOp := coVariab;  //Convierte a variable
    p1^.rVar := GetVarByteFromStk;
    catOperation := TCatOperation((Ord(p1^.catOp) << 2) or ord(p2^.catOp));
    //Luego el caso es similar a coVariab_Expres
    Oper_byte_great_byte;
    FreeStkRegisterByte(r);   //libera pila porque ya se usó el dato ahí contenido
  end;
  end;
end;
procedure TGenCod.Oper_byte_less_byte;
var
  r: TPicRegister;
begin
  //A < B es lo mismo que B > A
  case catOperation of
  coExpres_Expres:begin
    {Este es el único caso que no se puede invertir, por la posición de los operandos en
     la pila.}
    //la expresión p1 debe estar salvada y p2 en el acumulador
    p1^.catOp := coVariab;  //Convierte a variable
    p1^.rVar := GetVarByteFromStk;
    catOperation := TCatOperation((Ord(p1^.catOp) << 2) or ord(p2^.catOp));
    //Luego el caso es similar a coVariab_Expres
    Oper_byte_less_byte;
    FreeStkRegisterByte(r);   //libera pila porque ya se usó el dato ahí contenido
  end;
  else
    //Para los otros casos, funciona
    ExchangeP1_P2;
    Oper_byte_great_byte;
  end;
end;
procedure TGenCod.Oper_byte_gequ_byte;
begin
  Oper_byte_less_byte;
  res.Invert;
end;
procedure TGenCod.Oper_byte_lequ_byte;
begin
  Oper_byte_great_byte;
  res.Invert;
end;
procedure TGenCod.CodifShift_by_W(aux: TPicRegister; toRight: boolean);
{Desplaza el registro "aux", las veces indicadas en el registro W.
Deja el resultado en W.
Deja el banco, en el banco de "aux"}
{ TODO : Tal vez se pueda optimizar usando una rutina que rote W, las veces indicadas en un registro, o se podría generar el código usando la rutina de WHILE. }
var
  loop1: Word;
  dg: integer;
begin
  _BANKSEL(aux.bank);  //quedará em este banco
  _ADDLW(1);   //corrige valor inicial
  loop1 := _PC;
  _SUBLW(1);  //decrementa
  _BTFSC(Z.offs, Z.bit);
  _GOTO_PEND(dg);     //Dio, cero, termina
  //Desplaza
  _BCF(_STATUS, _C);   //limpia bandera porque se hace rotación
  if toRight then  //a la derecha
    _RRF(aux.offs, toF)
  else
    _RLF(aux.offs, toF);
  _GOTO(loop1);
  //Terminó el lazo
  //Ya estamos en el banco de "aux"
  pic.codGotoAt(dg, _PC);   //termina de codificar el salto
  _MOVF(aux.offs, toW);  //deja en W
end;
procedure TGenCod.Oper_byte_shr_byte;  //Desplaza a la derecha
var
  aux: TPicRegister;
begin
  case catOperation of
  coConst_Const: begin  //compara constantes. Caso especial
    SetResultConst_byte(p1^.valInt >> p2^.valInt);
  end;
//  coConst_Variab: begin
//  end;
//  coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
//  end;
  coVariab_Const: begin
    SetResultExpres_byte(operType);   //Se pide Z para el resultado
    //Verifica casos simples
    if p2^.valInt = 0 then begin
      _BANKSEL(p1^.bank);  //verifica banco destino
      _MOVF(p1^.offs, toW);  //solo devuelve lo mismo en W
    end else if p2^.valInt = 1 then begin
      _BCF(_STATUS, _C);   //limpia bandera porque se hace rotación
      _BANKSEL(p1^.bank);  //verifica banco destino
      _RRF(p1^.offs, toW);  //devuelve desplazado en W
    end else if p2^.valInt = 2 then begin
      aux := GetAuxRegisterByte;
      //copia p1 a "aux"
      _BANKSEL(p1^.bank);  //verifica banco destino
      _BCF(_STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(p1^.offs, toW);  //desplaza y mueve
      _BANKSEL(aux.bank);
      _MOVWF(aux.offs);
      _BCF(_STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(aux.offs, toW);  //desplaza y devuelve en W
      aux.used := false;
    end else if p2^.valInt = 3 then begin
      aux := GetAuxRegisterByte;
      //copia p1 a "aux"
      _BANKSEL(p1^.bank);  //verifica banco destino
      _BCF(_STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(p1^.offs, toW);  //desplaza y mueve
      _BANKSEL(aux.bank);
      _MOVWF(aux.offs);
      _BCF(_STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(aux.offs, toF);  //desplaza
      _BCF(_STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(aux.offs, toW);  //desplaza y devuelve en W
      aux.used := false;
    end else if p2^.valInt = 4 then begin
      aux := GetAuxRegisterByte;
      //copia p1 a "aux"
      _BANKSEL(p1^.bank);  //verifica banco destino
      _BCF(_STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(p1^.offs, toW);  //desplaza y mueve
      _BANKSEL(aux.bank);
      _MOVWF(aux.offs);
      _BCF(_STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(aux.offs, toF);  //desplaza
      _BCF(_STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(aux.offs, toF);  //desplaza
      _BCF(_STATUS, _C);   //limpia bandera porque se hace rotación
      _RRF(aux.offs, toW);  //desplaza y devuelve en W
      aux.used := false;
    end else begin
      aux := GetAuxRegisterByte;
      //copia p1 a "aux"
      _BANKSEL(p1^.bank);  //verifica banco destino
      _MOVF(p1^.offs, toW);
      _BANKSEL(aux.bank);
      _MOVWF(aux.offs);
      //copia p2 a W
      _MOVLW(p2^.valInt);
      //lazo de rotación
      CodifShift_by_W(aux, true);
      aux.used := false;
    end;
  end;
  coVariab_Variab:begin
    SetResultExpres_byte(operType);   //Se pide Z para el resultado
    aux := GetAuxRegisterByte;
    //copia p1 a "aux"
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVF(p1^.offs, toW);
    _BANKSEL(aux.bank);
    _MOVWF(aux.offs);
    //copia p2 a W
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    //lazo de rotación
    CodifShift_by_W(aux, true);
    aux.used := false;
  end;
//  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
//  end;
//  coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
//  end;
//  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
//  end;
//  coExpres_Expres:begin
//  end;
  else
    GenError('Not implemented.'); exit;
  end;
end;
procedure TGenCod.Oper_byte_shl_byte;   //Desplaza a la izquierda
var
  aux: TPicRegister;
begin
  case catOperation of
  coConst_Const: begin  //compara constantes. Caso especial
    SetResultConst_byte(p1^.valInt >> p2^.valInt);
  end;
//  coConst_Variab: begin
//  end;
//  coConst_Expres: begin  //la expresión p2 se evaluó y esta en W
//  end;
  coVariab_Const: begin
    SetResultExpres_byte(operType);   //Se pide Z para el resultado
    //Verifica casos simples
    if p2^.valInt = 0 then begin
      _BANKSEL(p1^.bank);  //verifica banco destino
      _MOVF(p1^.offs, toW);  //solo devuelve lo mismo en W
    end else if p2^.valInt = 1 then begin
      _BCF(_STATUS, _C);   //limpia bandera porque se hace rotación
      _BANKSEL(p1^.bank);  //verifica banco destino
      _RLF(p1^.offs, toW);  //devuelve desplazado en W
    end else if p2^.valInt = 2 then begin
      aux := GetAuxRegisterByte;
      //copia p1 a "aux"
      _BANKSEL(p1^.bank);  //verifica banco destino
      _BCF(_STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(p1^.offs, toW);  //desplaza y mueve
      _BANKSEL(aux.bank);
      _MOVWF(aux.offs);
      _BCF(_STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(aux.offs, toW);  //desplaza y devuelve en W
      aux.used := false;
    end else if p2^.valInt = 3 then begin
      aux := GetAuxRegisterByte;
      //copia p1 a "aux"
      _BANKSEL(p1^.bank);  //verifica banco destino
      _BCF(_STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(p1^.offs, toW);  //desplaza y mueve
      _BANKSEL(aux.bank);
      _MOVWF(aux.offs);
      _BCF(_STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(aux.offs, toF);  //desplaza
      _BCF(_STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(aux.offs, toW);  //desplaza y devuelve en W
      aux.used := false;
    end else if p2^.valInt = 4 then begin
      aux := GetAuxRegisterByte;
      //copia p1 a "aux"
      _BANKSEL(p1^.bank);  //verifica banco destino
      _BCF(_STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(p1^.offs, toW);  //desplaza y mueve
      _BANKSEL(aux.bank);
      _MOVWF(aux.offs);
      _BCF(_STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(aux.offs, toF);  //desplaza
      _BCF(_STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(aux.offs, toF);  //desplaza
      _BCF(_STATUS, _C);   //limpia bandera porque se hace rotación
      _RLF(aux.offs, toW);  //desplaza y devuelve en W
      aux.used := false;
    end else begin
      aux := GetAuxRegisterByte;
      //copia p1 a "aux"
      _BANKSEL(p1^.bank);  //verifica banco destino
      _MOVF(p1^.offs, toW);
      _BANKSEL(aux.bank);
      _MOVWF(aux.offs);
      //copia p2 a W
      _MOVLW(p2^.valInt);
      //lazo de rotación
      CodifShift_by_W(aux, false);
      aux.used := false;
    end;
  end;
  coVariab_Variab:begin
    SetResultExpres_byte(operType);   //Se pide Z para el resultado
    aux := GetAuxRegisterByte;
    //copia p1 a "aux"
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVF(p1^.offs, toW);
    _BANKSEL(aux.bank);
    _MOVWF(aux.offs);
    //copia p2 a W
    _BANKSEL(p2^.bank);
    _MOVF(p2^.offs, toW);
    //lazo de rotación
    CodifShift_by_W(aux, false);
    aux.used := false;
  end;
//  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
//  end;
//  coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
//  end;
//  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
//  end;
//  coExpres_Expres:begin
//  end;
  else
    GenError('Not implemented.'); exit;
  end;
end;
procedure TGenCod.byte_bit(const OpPtr: pointer; nbit: byte);
//Implementa la operación del campo <tipo>.bit#
var
  xvar, tmpVar: TxpEleVar;
  msk: byte;
  Op: ^TOperand;
begin
  Op := OpPtr;
  case Op^.catOp of
  coVariab: begin
    xvar := Op^.rVar;
    //Se devuelve una variable, byte
    res.catOp := coVariab;
    res.typ   := typBit;
    //Crea una variable temporal que representará al campo
    tmpVar := CreateTmpVar(xvar.name+'.bit' + IntToStr(nbit), typBit);   //crea variable temporal
    tmpVar.adrBit.offs := xvar.adrByte0.offs;
    tmpVar.adrBit.bank := xvar.adrByte0.bank;
    tmpVar.adrBit.bit  := nbit;
    tmpVar.adrBit.assigned := xvar.adrByte0.assigned;
    tmpVar.adrBit.used     := xvar.adrByte0.used;
    res.rVar := tmpVar;   //actualiza la referencia
  end;
  coConst: begin
    //Se devuelve una constante bit
    res.catOp := coConst;
    res.typ   := typBit;
    msk := Op^.valInt and ($01 << nbit);
    res.valBool := msk <> 0;
  end;
  else
    GenError('Syntax error.');
  end;
end;
procedure TGenCod.byte_bit0(const OpPtr: pointer);
begin
  byte_bit(OpPtr, 0);
end;
procedure TGenCod.byte_bit1(const OpPtr: pointer);
begin
  byte_bit(OpPtr, 1);
end;
procedure TGenCod.byte_bit2(const OpPtr: pointer);
begin
  byte_bit(OpPtr, 2);
end;
procedure TGenCod.byte_bit3(const OpPtr: pointer);
begin
  byte_bit(OpPtr, 3);
end;
procedure TGenCod.byte_bit4(const OpPtr: pointer);
begin
  byte_bit(OpPtr, 4);
end;
procedure TGenCod.byte_bit5(const OpPtr: pointer);
begin
  byte_bit(OpPtr, 5);
end;
procedure TGenCod.byte_bit6(const OpPtr: pointer);
begin
  byte_bit(OpPtr, 6);
end;
procedure TGenCod.byte_bit7(const OpPtr: pointer);
begin
  byte_bit(OpPtr, 7);
end;
//////////// Operaciones con Word
procedure TGenCod.word_LoadToReg(const OpPtr: pointer);
{Carga el valor de una expresión a los registros de trabajo.}
var
  Op: ^TOperand;
begin
  Op := OpPtr;
  case Op^.catOp of  //el parámetro debe estar en "Op^"
  coConst : begin
    _MOVLW(Op^.HByte);
    _BANKSEL(H.bank);
    _MOVWF(H.offs);
    _MOVLW(Op^.LByte);
  end;
  coVariab: begin
    _BANKSEL(Op^.bank);
    _MOVF(Op^.Hoffs, toW);
    _BANKSEL(H.bank);
    _MOVWF(H.offs);
    _MOVF(Op^.Loffs, toW);
  end;
  coExpres: begin  //se asume que ya está en (H,w)
  end;
  end;
end;
procedure TGenCod.word_DefineRegisters;
begin
  //Aparte de W, solo se requiere H
  if not H.assigned then begin
    AssignRAM(H, '_H');
  end;
end;
procedure TGenCod.word_SaveToStk;
var
  stk: TPicRegister;
begin
  //guarda W
  stk := GetStkRegisterByte;  //pide memoria
  _BANKSEL(stk.bank);
  _MOVWF(stk.offs);PutComm(';save W');
  stk.used := true;
  //guarda H
  stk := GetStkRegisterByte;   //pide memoria
  _BANKSEL(H.bank);
  _MOVF(H.offs, toW);PutComm(';save H');
  _BANKSEL(stk.bank);
  _MOVWF(stk.offs);
  stk.used := true;   //marca
end;
procedure TGenCod.Oper_word_asig_word;
begin
  if p1^.catOp <> coVariab then begin  //validación
    GenError('Only variables can be assigned.'); exit;
  end;
  case p2^.catOp of
  coConst : begin
    SetResultExpres_word(operType);  //Realmente, el resultado no es importante
    _BANKSEL(p1^.bank);
    if p2^.LByte = 0 then begin  //optimiza
      _CLRF(p1^.Loffs);
    end else begin
      _MOVLW(p2^.LByte);
      _MOVWF(p1^.Loffs);
    end;
    if p2^.HByte = 0 then begin  //optimiza
      _CLRF(p1^.Hoffs);
    end else begin
      _MOVLW(p2^.HByte);
      _MOVWF(p1^.Hoffs);
    end;
  end;
  coVariab: begin
    SetResultExpres_word(operType);  //Realmente, el resultado no es importante
    _MOVF(p2^.Loffs, toW);
    _MOVWF(p1^.Loffs);
    _MOVF(p2^.Hoffs, toW);
    _MOVWF(p1^.Hoffs);
  end;
  coExpres: begin   //se asume que se tiene en (H,w)
    SetResultExpres_word(operType);  //Realmente, el resultado no es importante
    _MOVWF(p1^.Loffs);
    _MOVF(H.offs, toW);
    _MOVWF(p1^.Hoffs);
  end;
  else
    GenError('No soportado'); exit;
  end;
end;
procedure TGenCod.Oper_word_asig_byte;
begin
  if p1^.catOp <> coVariab then begin  //validación
    GenError('Only variables can be assigned.'); exit;
  end;
  case p2^.catOp of
  coConst : begin
    SetResultExpres_word(operType);  //Realmente, el resultado no es importante
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
    SetResultExpres_word(operType);  //Realmente, el resultado no es importante
    _CLRF(p1^.Hoffs);
    _MOVF(p2^.Loffs, toW);
    _MOVWF(p1^.Loffs);
  end;
  coExpres: begin   //se asume que está en w
    SetResultExpres_word(operType);  //Realmente, el resultado no es importante
    _CLRF(p1^.Hoffs);
    _MOVWF(p1^.offs);
  end;
  else
    GenError('No soportado'); exit;
  end;
end;
procedure TGenCod.Oper_word_equal_word;
var
  tmp: TPicRegister;
  sale: integer;
begin
  case catOperation of
  coConst_Const: begin  //compara constantes. Caso especial
    SetResultConst_bool(p1^.valInt = p2^.valInt);
  end;
  coConst_Variab: begin
    SetResultExpres_bool(operType, false);   //Se pide Z para el resultado
    //Compara byte alto
    _MOVLW(p1^.HByte);
    _BANKSEL(p2^.bank);  //verifica banco destino
    _SUBWF(p2^.Hoffs, toW); //p2-p1
    _BTFSS(Z.offs, Z.bit);
    _GOTO_PEND(sale);  //no son iguales
    //Son iguales, comparar el byte bajo
    _MOVLW(p1^.LByte);
    _BANKSEL(p2^.bank);  //verifica banco destino
    _SUBWF(p2^.Loffs,toW);	//p2-p1
_LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
  end;
  coConst_Expres: begin  //la expresión p2 se evaluó p2 esta en W
    SetResultExpres_bool(operType, false);   //Se pide Z para el resultado
    tmp := GetAuxRegisterByte;
    _BANKSEL(tmp.bank);
    _MOVWF(tmp.offs);   //salva byte bajo de Expresión
    //Compara byte alto
    _MOVLW(p1^.HByte);
    _BANKSEL(H.bank);  //verifica banco destino
    _SUBWF(H.offs, toW); //p2-p1
    _BTFSS(Z.offs, Z.bit);
    _GOTO_PEND(sale);  //no son iguales
    //Son iguales, comparar el byte bajo
    _MOVLW(p1^.LByte);
    _BANKSEL(tmp.bank);  //verifica banco destino
    _SUBWF(tmp.offs,toW);	//p2-p1
_LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
  end;
  coVariab_Const: begin
    ExchangeP1_P2;  //Convierte a coConst_Variab
    Oper_word_equal_word;
  end;
  coVariab_Variab:begin
    SetResultExpres_bool(operType, false);   //Se pide Z para el resultado
    //Compara byte alto
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVF(p1^.Hoffs, toW);
    _BANKSEL(p2^.bank);  //verifica banco destino
    _SUBWF(p2^.Hoffs, toW); //p2-p1
    _BTFSS(Z.offs, Z.bit);
    _GOTO_PEND(sale);  //no son iguales
    //Son iguales, comparar el byte bajo
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVF(p1^.Loffs, toW);
    _BANKSEL(p2^.bank);  //verifica banco destino
    _SUBWF(p2^.Loffs,toW);	//p2-p1
_LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
  end;
  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetResultExpres_bool(operType, false);   //Se pide Z para el resultado
    tmp := GetAuxRegisterByte;
    _BANKSEL(tmp.bank);
    _MOVWF(tmp.offs);   //salva byte bajo de Expresión
    //Compara byte alto
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVF(p1^.Hoffs, toW);
    _BANKSEL(H.bank);  //verifica banco destino
    _SUBWF(H.offs, toW); //p2-p1
    _BTFSS(Z.offs, Z.bit);
    _GOTO_PEND(sale);  //no son iguales
    //Son iguales, comparar el byte bajo
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVF(p1^.Loffs, toW);
    _BANKSEL(tmp.bank);  //verifica banco destino
    _SUBWF(tmp.offs,toW);	//p2-p1
    tmp.used := false;
_LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
  end;
  coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
    ExchangeP1_P2;  //Convierte a coConst_Expres;
    Oper_word_equal_word;
  end;
  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
    ExchangeP1_P2;  //Convierte a coVariab_Expres;
    Oper_word_equal_word;
  end;
  coExpres_Expres:begin
    //La expresión p1, debe estar salvada y p2 en (H,W)
    p1^.catOp := coVariab;
    p1^.rVar  := GetVarWordFromStk;
    catOperation := TCatOperation((Ord(p1^.catOp) << 2) or ord(p2^.catOp));
    //Luego el caso es similar a variable-expresión
    Oper_word_equal_word;
    FreeStkRegisterWord;
  end;
  else
    GenError('Not implemented.'); exit;
  end;
end;
procedure TGenCod.Oper_word_difer_word;
begin
  Oper_word_equal_word;
  res.Invert;
end;
procedure TGenCod.Oper_word_great_word;
var
  tmp: TPicRegister;
  sale: integer;
begin
  case catOperation of
  coConst_Const: begin  //compara constantes. Caso especial
    SetResultConst_bool(p1^.valInt > p2^.valInt);
  end;
  coConst_Variab: begin
    if p1^.valInt = 0 then begin
      //0 es mayor que nada
      SetResultConst_bool(false);
//      GenWarn('Expression will always be FALSE.');  //o TRUE si la lógica Está invertida
    end else begin
      SetResultExpres_bool(operType, false);   //Se pide Z para el resultado
      //Compara byte alto
      _MOVLW(p1^.HByte);
      _BANKSEL(p2^.bank);  //verifica banco destino
      _SUBWF(p2^.Hoffs, toW); //p2-p1
      _BTFSS(Z.offs, Z.bit);
      _GOTO_PEND(sale);  //no son iguales
      //Son iguales, comparar el byte bajo
      _MOVLW(p1^.LByte);
      _BANKSEL(p2^.bank);  //verifica banco destino
      _SUBWF(p2^.Loffs,toW);	//p2-p1
  _LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
      CopyInvert_C_to_Z;  //Pasa a Z
    end;
  end;
  coConst_Expres: begin  //la expresión p2 se evaluó p2 esta en W
    SetResultExpres_bool(operType, false);   //Se pide Z para el resultado
    tmp := GetAuxRegisterByte;
    _BANKSEL(tmp.bank);
    _MOVWF(tmp.offs);   //salva byte bajo de Expresión
    //Compara byte alto
    _MOVLW(p1^.HByte);
    _BANKSEL(H.bank);  //verifica banco destino
    _SUBWF(H.offs, toW); //p2-p1
    _BTFSS(Z.offs, Z.bit);
    _GOTO_PEND(sale);  //no son iguales
    //Son iguales, comparar el byte bajo
    _MOVLW(p1^.LByte);
    _BANKSEL(tmp.bank);  //verifica banco destino
    _SUBWF(tmp.offs,toW);	//p2-p1
_LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
    CopyInvert_C_to_Z;  //Pasa a Z
  end;
//  coVariab_Const: begin
//    ExchangeP1_P2;  //Convierte a coConst_Variab
//    Oper_word_equal_word;
//  end;
  coVariab_Variab:begin
    SetResultExpres_bool(operType, false);   //Se pide Z para el resultado
    //Compara byte alto
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVF(p1^.Hoffs, toW);
    _BANKSEL(p2^.bank);  //verifica banco destino
    _SUBWF(p2^.Hoffs, toW); //p2-p1
    _BTFSS(Z.offs, Z.bit);
    _GOTO_PEND(sale);  //no son iguales
    //Son iguales, comparar el byte bajo
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVF(p1^.Loffs, toW);
    _BANKSEL(p2^.bank);  //verifica banco destino
    _SUBWF(p2^.Loffs,toW);	//p2-p1
_LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
    CopyInvert_C_to_Z;  //Pasa a Z
  end;
  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en W
    SetResultExpres_bool(operType, false);   //Se pide Z para el resultado
    tmp := GetAuxRegisterByte;
    _BANKSEL(tmp.bank);
    _MOVWF(tmp.offs);   //salva byte bajo de Expresión
    //Compara byte alto
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVF(p1^.Hoffs, toW);
    _BANKSEL(H.bank);  //verifica banco destino
    _SUBWF(H.offs, toW); //p2-p1
    _BTFSS(Z.offs, Z.bit);
    _GOTO_PEND(sale);  //no son iguales
    //Son iguales, comparar el byte bajo
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVF(p1^.Loffs, toW);
    _BANKSEL(tmp.bank);  //verifica banco destino
    _SUBWF(tmp.offs,toW);	//p2-p1
    tmp.used := false;
_LABEL(sale); //Si p1=p2 -> Z=1. Si p1>p2 -> C=0.
    CopyInvert_C_to_Z;  //Pasa a Z
  end;
//  coExpres_Const: begin   //la expresión p1 se evaluó y esta en W
//    ExchangeP1_P2;  //Convierte a coConst_Expres;
//    Oper_word_equal_word;
//  end;
//  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en W
//    ExchangeP1_P2;  //Convierte a coVariab_Expres;
//    Oper_word_equal_word;
//  end;
  coExpres_Expres:begin
    //La expresión p1, debe estar salvada y p2 en (H,W)
    p1^.catOp := coVariab;
    p1^.rVar  := GetVarWordFromStk;
    catOperation := TCatOperation((Ord(p1^.catOp) << 2) or ord(p2^.catOp));
    //Luego el caso es similar a variable-expresión
    Oper_word_great_word;
    FreeStkRegisterWord;
  end;
  else
    GenError('Not implemented.'); exit;
  end;
end;
procedure TGenCod.Oper_word_add_word;
var
  spH: TPicRegister;
  spL: TPicRegister;
  aux: TPicRegister;
begin
  case catOperation of
  coConst_Const: begin
    if p1^.valInt+p2^.valInt <256 then begin
      //Optimiza
      SetResultConst_byte(p1^.valInt+p2^.valInt);
    end else begin
      SetResultConst_word(p1^.valInt+p2^.valInt);
    end;
  end;
  coConst_Variab: begin
    SetResultExpres_word(operType);
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
    _movwf(H.offs);         //Guarda el resultado
    _movlw(p1^.LByte);      //Carga menos peso del dato 1
    _addwf(p2^.Loffs,toW);  //Suma menos peso del dato 2, deja en W
    _btfsc(_STATUS,_C);     //Hubo acarreo anterior?
    _incf(H.offs, toF);
  end;
  coConst_Expres: begin  //la expresión p2 se evaluó y esta en (H,W)
    SetResultExpres_word(operType);
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
    SetResultExpres_word(operType);
    _MOVLW(p2^.HByte);      //Carga más peso del dato 1
    _ADDWF(p1^.Hoffs,toW);  //Suma más peso del dato 2
    _MOVWF(H.offs);         //Guarda el resultado
    _MOVLW(p2^.LByte);      //Carga menos peso del dato 1
    _ADDWF(p1^.Loffs,toW);  //Suma menos peso del dato 2, deja en W
    _BTFSC(_STATUS,_C);     //Hubo acarreo anterior?
    _INCF(H.offs, toF);
  end;
  coVariab_Variab:begin
    SetResultExpres_word(operType);
    _MOVF(p1^.Hoffs, toW);  //Carga mayor peso del dato 1
    _ADDWF(p2^.Hoffs,toW);  //Suma mayor peso del dato 2
    _MOVWF(H.offs);         //Guarda mayor peso del resultado
    _MOVF(p1^.Loffs, toW);  //Carga menos peso del dato 1
    _ADDWF(p2^.Loffs,toW);  //Suma menos peso del dato 2, deja en W
    _BTFSC(_STATUS,_C);     //Hubo acarreo anterior?
    _INCF(H.offs, toF);
  end;
  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en (H,W)
    SetResultExpres_word(operType);
    aux := GetAuxRegisterByte;  //Pide un registro libre
    _BANKSEL(aux.bank);
    _movwf(aux.offs);        //guarda byte bajo
    _BANKSEL(p1^.bank);
    _MOVF(p1^.Hoffs, toW);   //Carga más peso del dato 1
    _BANKSEL(H.bank);
    _addwf(H.offs,toF);      //Suma y guarda
    //Siguiente byte
    _BANKSEL(p1^.bank);
    _MOVF(p1^.Loffs, toW);       //Carga menos peso del dato 1
    _BANKSEL(aux.bank);
    _addwf(aux.offs,toW);    //Suma menos peso del dato 2, deja en W
    _btfsc(_STATUS,_C);      //Hubo acarreo anterior?
    _incf(H.offs, toF);
    aux.used := false;
  end;
  coExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,W)
    SetResultExpres_word(operType);
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
    SetResultExpres_word(operType);
    aux := GetAuxRegisterByte;  //Pide un registro libre
    _movwf(aux.offs);      //guarda byte bajo
    _BANKSEL(p2^.bank);
    _MOVF(p2^.Hoffs, toW);     //Carga más peso del dato 1
    _BANKSEL(H.bank);
    _addwf(H.offs,toF);    //Suma y guarda
    _BANKSEL(p2^.bank);
    _MOVF(p2^.Loffs, toW);     //Carga menos peso del dato 1
    _BANKSEL(aux.bank);
    _addwf(aux.offs,toW);  //Suma menos peso del dato 2, deja en W
    _BANKSEL(H.bank);
    _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
    _incf(H.offs, toF);
    aux.used := false;
  end;
  coExpres_Expres:begin
    SetResultExpres_word(operType);
    //p1 está salvado en pila y p2 en (_H,W)
    p1^.catOp := coVariab;  //Convierte a variable
    p1^.rVar := GetVarWordFromStk;
    catOperation := TCatOperation((Ord(p1^.catOp) << 2) or ord(p2^.catOp));
    //Luego el caso es similar a coVariab_Expres
    Oper_word_add_word;
    FreeStkRegisterByte(spH);   //libera pila, obtiene dirección
    FreeStkRegisterByte(spL);   //libera pila, obtiene dirección
  end;
  else
    genError('Not implemented: "%s"', [CatOperationToStr]);
  end;
end;
procedure TGenCod.Oper_word_add_byte;
var
  spH: TPicRegister;
  spL: TPicRegister;
  aux: TPicRegister;
begin
  case catOperation of
  coConst_Const: begin
    if p1^.valInt+p2^.valInt <256 then begin
      //Optimiza
      SetResultConst_byte(p1^.valInt+p2^.valInt);
    end else begin
      SetResultConst_word(p1^.valInt+p2^.valInt);
    end;
  end;
  coConst_Variab: begin
    SetResultExpres_word(operType);
    //versión más corta que solo usa _H, por validar
    _movlw(p1^.HByte);      //Carga más peso del dato 1
    _BANKSEL(H.bank);
    _movwf(H.offs);
    _movlw(p1^.LByte);      //Carga menos peso del dato 1
    _BANKSEL(p2^.bank);
    _addwf(p2^.Loffs,toW);  //Suma menos peso del dato 2, deja en W
    _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
    _incf(H.offs, toF);
  end;
  coConst_Expres: begin  //la expresión p2 se evaluó y esta en (W)
    SetResultExpres_word(operType);
    aux := GetAuxRegisterByte;  //Pide un registro libre
    _BANKSEL(aux.bank);
    _movwf(aux.offs);      //guarda byte bajo
    _movlw(p1^.HByte);     //Carga más peso del dato 1
    _BANKSEL(H.bank);
    _movwf(H.offs);
    _movlw(p1^.LByte);     //Carga menos peso del dato 1
    _BANKSEL(aux.bank);
    _addwf(aux.offs,toW);  //Suma menos peso del dato 2, deja en W
    _BANKSEL(H.bank);      //se cambia primero el banco, por si acaso.
    _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
    _incf(H.offs, toF);
    aux.used := false;
  end;
  coVariab_Const: begin
    SetResultExpres_word(operType);
    _BANKSEL(p1^.bank);      //se cambia primero el banco por si acaso
    _MOVF(p1^.Hoffs, toW); //Carga más peso del dato 1
    _BANKSEL(H.bank);      //se cambia primero el banco por si acaso
    _MOVWF(H.offs);        //Guarda el resultado
    _MOVLW(p2^.LByte);
    _BANKSEL(p1^.bank);      //se cambia primero el banco por si acaso
    _ADDWF(p1^.Loffs,toW); //Suma menos peso del dato 2, deja en W
    _BANKSEL(H.bank);      //se cambia primero el banco, por si acaso.
    _BTFSC(_STATUS,_C);    //Hubo acarreo anterior?
    _INCF(H.offs, toF);
  end;
  coVariab_Variab:begin
    SetResultExpres_word(operType);
    _BANKSEL(p1^.bank);
    _MOVF(p1^.Hoffs, toW);     //Carga más peso del dato 1
    _BANKSEL(H.bank);
    _MOVWF(H.offs);
    _BANKSEL(p1^.bank);
    _MOVF(p1^.Loffs, toW);     //Carga menos peso del dato 1
    _BANKSEL(p2^.bank);
    _ADDWF(p2^.Loffs,toW); //Suma menos peso del dato 2, deja en W
    _BANKSEL(H.bank);      //se cambia primero el banco, por si acaso.
    _BTFSC(_STATUS,_C);    //Hubo acarreo anterior?
    _INCF(H.offs, toF);
  end;
  coVariab_Expres:begin   //la expresión p2 se evaluó y esta en (_H,W)
    SetResultExpres_word(operType);
    aux := GetAuxRegisterByte;  //Pide un registro libre
    _BANKSEL(aux.bank);
    _movwf(aux.offs);        //guarda byte de expresión
    _BANKSEL(p1^.bank);
    _movf(p1^.Hoffs, toW);  //Carga Hbyte del dato 1
    _BANKSEL(H.bank);
    _movwf(H.offs);        //Lo deja para devolver en H
    _BANKSEL(aux.bank);
    _MOVF(aux.offs,toW);   //recupera byte de expresión
    _BANKSEL(p1^.bank);
    _addwf(p1^.Loffs,toW);  //Suma menos peso del dato 2, deja en W
    _BANKSEL(H.bank);      //se cambia primero el banco, por si acaso.
    _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
    _incf(H.offs, toF);
    aux.used := false;
  end;
  coExpres_Const: begin   //la expresión p1 se evaluó y esta en (H,W)
    SetResultExpres_word(operType);
    _addlw(p2^.LByte);     //Suma menos peso del dato 2, deja en W
    _BANKSEL(H.bank);      //se cambia primero el banco, por si acaso.
    _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
    _incf(H.offs, toF);
  end;
  coExpres_Variab:begin  //la expresión p1 se evaluó y esta en (H,W)
    SetResultExpres_word(operType);
    _BANKSEL(p2^.bank);
    _addwf(p2^.Loffs,toW);         //Suma menos peso del dato 2, deja en W
    _BANKSEL(H.bank);      //se cambia primero el banco, por si acaso.
    _btfsc(_STATUS,_C);    //Hubo acarreo anterior?
    _incf(H.offs, toF);
  end;
  coExpres_Expres:begin
    SetResultExpres_word(operType);
    //p1 está salvado en pila y p2 en (_H,W)
    p1^.catOp := coVariab;  //Convierte a variable
    p1^.rVar := GetVarWordFromStk;
    catOperation := TCatOperation((Ord(p1^.catOp) << 2) or ord(p2^.catOp));
    //Luego el caso es similar a coVariab_Expres
    Oper_word_add_byte;
    FreeStkRegisterByte(spH);   //libera pila
    FreeStkRegisterByte(spL);   //libera pila
  end;
  else
    genError('Not implemented: "%s"', [CatOperationToStr] );
  end;
end;
procedure TGenCod.Oper_word_sub_word;
begin
end;
procedure TGenCod.word_Low(const OpPtr: pointer);
{Acceso al byte de menor peso de un word.}
var
  xvar, tmpVar: TxpEleVar;
  Op: ^TOperand;
begin
  Op := OpPtr;
  case Op^.catOp of
  coVariab: begin
    xvar := Op^.rVar;
    //Se devuelve una variable, byte
    res.catOp := coVariab;
    res.typ   := typByte;
    //Crea una variable temporal que representará al campo
    tmpVar := CreateTmpVar(xvar.name+'.Low', typByte);   //crea variable temporal
    tmpVar.adrByte0.Assign(xvar.adrByte0);  //byte bajo
    res.rVar := tmpVar;   //actualiza la referencia
  end;
  coConst: begin
    //Se devuelve una constante bit
    res.catOp := coConst;
    res.typ   := typByte;
    res.valInt := Op^.ValInt and $ff;
  end;
  else
    GenError('Syntax error.');
  end;
end;
procedure TGenCod.word_High(const OpPtr: pointer);
{Acceso al byte de mayor peso de un word.}
var
  xvar, tmpVar: TxpEleVar;
  Op: ^TOperand;
begin
  Op := OpPtr;
  case Op^.catOp of
  coVariab: begin
    xvar := Op^.rVar;
    //Se devuelve una variable, byte
    res.catOp := coVariab;
    res.typ   := typByte;
    //Crea una variable temporal que representará al campo
    tmpVar := CreateTmpVar(xvar.name+'.High', typByte);
    tmpVar.adrByte0.Assign(xvar.adrByte1);  //byte alto
    res.rVar := tmpVar;   //actualiza la referencia
  end;
  coConst: begin
    //Se devuelve una constante bit
    res.catOp := coConst;
    res.typ   := typByte;
    res.valInt := (Op^.ValInt and $ff00)>>8;
  end;
  else
    GenError('Syntax error.');
  end;
end;
//////////// Operaciones con Char
procedure TGenCod.Oper_char_asig_char;
begin
  if p1^.catOp <> coVariab then begin  //validación
    GenError('Only variables can be assigned.'); exit;
  end;
  case p2^.catOp of
  coConst : begin
    SetResultExpres_char(operType);  //Realmente, el resultado no es importante
    if p2^.valInt=0 then begin
      //caso especial
      _BANKSEL(p1^.bank);  //verifica banco destino
      _CLRF(p1^.offs);
    end else begin
      _MOVLW(p2^.valInt);  //Los chars se manejan como números
      _BANKSEL(p1^.bank);  //verifica banco destino
      _MOVWF(p1^.offs);
    end;
  end;
  coVariab: begin
    SetResultExpres_char(operType);  //Realmente, el resultado no es importante
    _BANKSEL(p2^.bank);  //verifica banco destino
    _MOVF(p2^.offs, toW);
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVWF(p1^.offs);
  end;
  coExpres: begin  //ya está en w
    SetResultExpres_char(operType);  //Realmente, el resultado no es importante
    _BANKSEL(p1^.bank);  //verifica banco destino
    _MOVWF(p1^.offs);
  end;
  else
    GenError('No soportado'); exit;
  end;
end;
procedure TGenCod.Oper_char_equal_char;
begin
  Oper_byte_equal_byte;  //es lo mismo
end;
procedure TGenCod.Oper_char_difer_char;
begin
  Oper_byte_difer_byte; //es lo mismo
end;
/////////////funciones del sistema
procedure TGenCod.codif_1mseg;
//Codifica rutina de reatrdo de 1mseg.
begin
  PutFwdComm(';inicio rutina 1 mseg.');
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
procedure TGenCod.codif_delay_ms(fun: TxpEleFun);
//Codifica rutina de retardo en milisegundos
var
  delay: Word;
  aux: TPicRegister;
begin
  StartCodeSub(fun);  //inicia codificación
//  PutLabel('__delay_ms');
  PutTopComm('    ;delay routine.');
  typWord.DefineRegister;   //Se asegura de que se exista y lo marca como "usado".
  aux := GetAuxRegisterByte;  //Pide un registro libre
  {Esta rutina recibe los milisegundos en los registros en (H,w) o en (w)
  En cualquier caso, siempre usa el registros H , el acumulador "w" y un reg. auxiliar.
  Se supone que para pasar los parámetros, ya se requirió H, así que no es necesario
  crearlo.}
  if HayError then exit;
  _CLRF(H.offs);   PutComm(' ;enter when parameters in (0,w)');
  _MOVWF(aux.offs); PutComm(';enter when parameters in (H,w)');
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
procedure TGenCod.fun_delay_ms(fun: TxpEleFun);
begin
  if not CaptureTok('(') then exit;
  GetExpressionE(0, pexPARSY);  //captura parámetro
  if HayError then exit;   //aborta
  //Se terminó de evaluar un parámetro
  res.LoadToReg;   //Carga en registro de trabajo
  if HayError then exit;
  if res.typ = typByte then begin
    //El parámetro byte, debe estar en W
    _CALL(fun.adrr);
  end else if res.typ = typWord then begin
    //El parámetro word, debe estar en (H, W)
    _CALL(fun.adrr+1);
  end else begin
    GenError('Invalid parameter type: %s', [res.typ.name]);
    exit;
  end;
  //Verifica fin de parámetros
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_Exit(fun: TxpEleFun);
{Se debe dejar en los registros de trabajo, el valro del parámetro indicado.}
  procedure CodifRETURN(curBlk: TxpElement);
  begin
    //Codifica el salto de salida
    if curBlk is TxpEleFun then begin
      //En la primera pasada, no está definido "adrrEnd".
  //    adrReturn := abs(TxpEleFun(curBlk).adrReturn);  //protege
  //    if pic.iFlash = adrReturn then begin
  //      //No es necesario incluir salto, proque ya está al final
  //    end else begin
        _RETURN;
  //    end;
    end else begin
      GenError('Internal: No implemented.');
    end;
  end;
var
  curFunTyp: TType;
  curBlk: TxpElement;
//  adrReturn: word;
begin
  curBlk := TreeElems.curNode.Parent;  //El curNode, debe ser de tipo "Body".
  if curBlk is TxpEleMain then begin  //En el programa principal
    _SLEEP;   //Así se termina un programa en PicPas
    exit;
  end;
  curFunTyp := curBlk.typ;
  if curFunTyp = typNull then begin
    //No lleva parámetros,
    CodifRETURN(curBlk);
    exit;  //No hay nada, más que hacer
  end;
  if not CaptureTok('(') then exit;
  GetExpressionE(0, pexPARSY);  //captura parámetro
  if HayError then exit;   //aborta
  //Verifica fin de parámetros
  if not CaptureTok(')') then exit;
  //El resultado de la expresión está en "res".
  if curFunTyp <> res.typ then begin
    GenError('Expected a "%s" expression.', [curFunTyp.name]);
  end;
  res.LoadToReg;
  res.typ := typNull;  //No es función
  CodifRETURN(curBlk);  //Codifica salto
end;
procedure TGenCod.fun_Inc(fun: TxpEleFun);
begin
  if not CaptureTok('(') then exit;
  GetExpressionE(0, pexPARSY);  //captura parámetro
  if HayError then exit;   //aborta
  case res.catOp of  //el parámetro debe estar en "res"
  coConst : begin
    GenError('Cannot increase a constant.'); exit;
  end;
  coVariab: begin
    if res.typ = typByte then begin
      _INCF(res.offs, toF);
    end else if res.typ = typWord then begin
      _INCF(res.Loffs, toF);
      _BTFSC(_STATUS, _Z);
      _INCF(res.Hoffs, toF);
    end else begin
      GenError('Invalid parameter type: %s', [res.typ.name]);
      exit;
    end;
  end;
  coExpres: begin  //se asume que ya está en (_H,w)
    GenError('Cannot increase an expression.'); exit;
  end;
  end;
  res.typ := typNull;  //No es función
  //Verifica fin de parámetros
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_Dec(fun: TxpEleFun);
begin
  if not CaptureTok('(') then exit;
  GetExpressionE(0, pexPARSY);  //captura parámetro
  if HayError then exit;   //aborta
  case res.catOp of  //el parámetro debe estar en "res"
  coConst : begin
    GenError('Cannot decrease a constant.'); exit;
  end;
  coVariab: begin
    if res.typ = typByte then begin
      _DECF(res.offs, toF);
    end else if res.typ = typWord then begin
      _MOVF(res.offs, toW);
      _BTFSC(_STATUS, _Z);
      _DECF(res.Hoffs, toF);
      _DECF(res.Loffs, toF);
    end else begin
      GenError('Invalid parameter type: %s', [res.typ.name]);
      exit;
    end;
  end;
  coExpres: begin  //se asume que ya está en (_H,w)
    GenError('Cannot decrease an expression.'); exit;
  end;
  end;
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_Ord(fun: TxpEleFun);
begin
  if not CaptureTok('(') then exit;
  GetExpressionE(0, pexPARSY);  //captura parámetro
  if HayError then exit;   //aborta
  case res.catOp of  //el parámetro debe estar en "res"
  coConst : begin
    if res.typ = typChar then begin
      SetResultConst_byte(res.valInt);
    end else begin
      GenError('Cannot convert to ordinal.'); exit;
    end;
  end;
  coVariab: begin
    if res.typ = typChar then begin
      //Sigue siendo variable y apunta a la misma variable, solo que ahora es Byte.
      SetResultVariab_byte(res.rVar);
    end else begin
      GenError('Cannot convert to ordinal.'); exit;
    end;
  end;
  coExpres: begin  //se asume que ya está en (w)
    if res.typ = typChar then begin
      //Es la misma expresión, solo que ahora es Byte.
      res.typ := typByte; //No se puede usar SetResultExpres_byte, porque no hay p1 y p2
    end else begin
      GenError('Cannot convert to ordinal.'); exit;
    end;
  end;
  end;
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_Chr(fun: TxpEleFun);
begin
  if not CaptureTok('(') then exit;
  GetExpressionE(0, pexPARSY);  //captura parámetro
  if HayError then exit;   //aborta
  case res.catOp of  //el parámetro debe estar en "res"
  coConst : begin
    if res.typ = typByte then begin
      SetResultConst_char(res.valInt);
    end else begin
      GenError('Cannot convert to char.'); exit;
    end;
  end;
  coVariab: begin
    if res.typ = typByte then begin
      //Sigue siendo variable y apunta a la misma variable, solo que ahora es Char.
      SetResultVariab_char(res.rVar);
    end else begin
      GenError('Cannot convert to char.'); exit;
    end;
  end;
  coExpres: begin  //se asume que ya está en (w)
    if res.typ = typByte then begin
      //Es la misma expresión, solo que ahora es Char.
      res.typ := typChar; //No se puede usar SetResultExpres_char, porque no hay p1 y p2;
    end else begin
      GenError('Cannot convert to char.'); exit;
    end;
  end;
  end;
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_Bit(fun: TxpEleFun);
{Convierte byte a bit}
begin
  if not CaptureTok('(') then exit;
  GetExpressionE(0, pexPARSY);  //captura parámetro
  if HayError then exit;   //aborta
  case res.catOp of  //el parámetro debe estar en "res"
  coConst : begin
    if res.typ = typByte then begin
      if res.valInt= 0 then SetResultConst_bit(false)
      else SetResultConst_bit(true);
    end else begin
      GenError('Cannot convert to bit.'); exit;
    end;
  end;
  coVariab: begin
    if res.typ = typByte then begin
      //Se asumirá que cualuier valor diferente de cero, devuelve 1
      res.typ := typBit; //No se puede usar SetResultExpres_char, porque no hay p1 y p2;
      res.catOp := coExpres;
      _MOVF(res.offs, toW);   //el resultado aparecerá en Z, invertido
    end else begin
      GenError('Cannot convert to bit.'); exit;
    end;
  end;
  coExpres: begin  //se asume que ya está en (w)
    if res.typ = typByte then begin
      res.typ := typBit; //No se puede usar SetResultExpres_char, porque no hay p1 y p2;
      _ADDLW(0);   //el resultado aparecerá en Z, invertido
    end else begin
      GenError('Cannot convert to bit.'); exit;
    end;
  end;
  end;
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_SetAsInput(fun: TxpEleFun);
begin
  if not CaptureTok('(') then exit;
  GetExpressionE(0, pexPARSY);  //captura parámetro
  if HayError then exit;   //aborta
  case res.catOp of  //el parámetro debe estar en "res"
  coConst : begin
    GenError('PORT or BIT variable expected.'); exit;
  end;
  coVariab: begin
    if res.typ = typByte then begin
      //Se asume que será algo como PORTA, PORTB, ...
      _MOVLW($FF);   //todos como entrads
      _BANKSEL(1);   //los registros TRIS, están en el banco 1
      _MOVWF(res.offs); //escribe en TRIS
    end else if res.typ = typBit then begin
      //Se asume que será algo como PORTA.0, PORTB.0, ...
      _BANKSEL(1);   //los registros TRIS, están en el banco 1
      _BSF(res.offs, res.bit); //escribe en TRIS
    end else begin
      GenError('Invalid type.'); exit;
    end;
    res.typ := typNull;  //No es función así que no es necesario fijar el resultado
  end;
  coExpres: begin  //se asume que ya está en (w)
    GenError('PORT variable expected.'); exit;
  end;
  end;
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_SetAsOutput(fun: TxpEleFun);
begin
  if not CaptureTok('(') then exit;
  GetExpressionE(0, pexPARSY);  //captura parámetro
  if HayError then exit;   //aborta
  case res.catOp of  //el parámetro debe estar en "res"
  coConst : begin
    GenError('PORT variable expected.'); exit;
  end;
  coVariab: begin
    if res.typ = typByte then begin
      //Se asume que será algo como PORTA, PORTB, ...
      _BANKSEL(1);   //los registros TRIS, están en el banco 1
      _CLRF(res.offs); //escribe en TRIS
    end else if res.typ = typBit then begin
      //Se asume que será algo como PORTA.0, PORTB.0, ...
      _BANKSEL(1);   //los registros TRIS, están en el banco 1
      _BCF(res.offs, res.bit); //escribe en TRIS
    end else begin
      GenError('Invalid type.'); exit;
    end;
    res.typ := typNull;  //No es función así que no es necesario fijar el resultado
  end;
  coExpres: begin  //se asume que ya está en (w)
    GenError('PORT variable expected.'); exit;
  end;
  end;
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_Word(fun: TxpEleFun);
begin
  if not CaptureTok('(') then exit;
  GetExpressionE(0, pexPARSY);  //captura parámetro
  if HayError then exit;   //aborta
  case res.catOp of  //el parámetro debe estar en "res"
  coConst : begin
    if res.typ = typByte then begin
      res.typ := typWord;  //solo cambia el tipo
    end else if res.typ = typWord then begin
      //ya es Word
    end else begin
      GenError('Cannot convert to word.'); exit;
    end;
  end;
  coVariab: begin
//    if res.typ = typByte then begin
//      typWord.OperationPop;   //Para asegurar que exista H
//      res.typ := typWord; //No se puede usar SetResultExpres_word, porque no hay p1 y p2;
//      res.catOp := coExpres;  //Va a devolver una expresión
//  !!! Debería guardar en pila, el posible valor de W
//      SaveW(OLD_W); if HayError then exit;  //Va a usar W
//      _MOVF(res.offs, toW);   //el resultado aparecerá en Z, invertido
//      RestoreW(OLD_W);   ///ERROR, modifica Z otra vez
//    end else begin
      GenError('Cannot convert to word.'); exit;
//    end;
  end;
  coExpres: begin  //se asume que ya está en (w)
//    if res.typ = typByte then begin
//      res.typ := typBit; //No se puede usar SetResultExpres_char, porque no hay p1 y p2;
//      _ADDLW(0);   //el resultado aparecerá en Z, invertido
//    end else begin
      GenError('Cannot convert to word.'); exit;
//    end;
  end;
  end;
  if not CaptureTok(')') then exit;
end;
procedure TGenCod.fun_SetBank(fun: TxpEleFun);
{Define el banco actual}
begin
  if not CaptureTok('(') then exit;
  GetExpressionE(0, pexPARSY);  //captura parámetro
  if HayError then exit;   //aborta
  case res.catOp of  //el parámetro debe estar en "res"
  coConst : begin
    if (res.typ = typByte) or (res.typ = typWord) then begin
      //ya es Word
      CurrBank := 255;   //para forzar el cambio
      _BANKSEL(res.valInt);
    end else begin
      GenError('Number expected.'); exit;
    end;
  end;
  coVariab, coExpres: begin  //se asume que ya está en (w)
    GenError('A constant expected.'); exit;
  end;
  end;
  if not CaptureTok(')') then exit;
end;

procedure TGenCod.StartSyntax;
//Se ejecuta solo una vez al inicio
begin
  ///////////define la sintaxis del compilador
  //Tipos de tokens personalizados
  tnExpDelim := xLex.NewTokType('ExpDelim');//delimitador de expresión ";"
  tnBlkDelim := xLex.NewTokType('BlkDelim'); //delimitador de bloque
  tnStruct   := xLex.NewTokType('Struct');   //personalizado
  tnDirective:= xLex.NewTokType('Directive'); //personalizado
  tnAsm      := xLex.NewTokType('Asm');      //personalizado
  tnChar     := xLex.NewTokType('Char');     //personalizado
  tnOthers   := xLex.NewTokType('Others');   //personalizado
  //Configura atributos
  tkKeyword.Style := [fsBold];     //en negrita
  xLex.Attrib[tnBlkDelim].Foreground:=clGreen;
  xLex.Attrib[tnBlkDelim].Style := [fsBold];    //en negrita
  xLex.Attrib[tnStruct].Foreground:=clGreen;
  xLex.Attrib[tnStruct].Style := [fsBold];      //en negrita
  //inicia la configuración
  xLex.ClearMethodTables;          //limpia tabla de métodos
  xLex.ClearSpecials;              //para empezar a definir tokens
  //crea tokens por contenido
  xLex.DefTokIdentif('[A-Za-z_]', '[A-Za-z0-9_]*');
  xLex.DefTokContent('[0-9]', '[0-9.]*', tnNumber);
  xLex.DefTokContent('[$]','[0-9A-Fa-f]*', tnNumber);
  xLex.DefTokContent('[%]','[01]*', tnNumber);
  //define palabras claves
  xLex.AddIdentSpecList('THEN var type absolute', tnKeyword);
  xLex.AddIdentSpecList('program public private method const', tnKeyword);
  xLex.AddIdentSpecList('class create destroy sub do begin', tnKeyword);
  xLex.AddIdentSpecList('END ELSE ELSIF UNTIL', tnBlkDelim);
  xLex.AddIdentSpecList('true false', tnBoolean);
  xLex.AddIdentSpecList('if while repeat for', tnStruct);
  xLex.AddIdentSpecList('and or xor not div mod in', tnOperator);
  //tipos predefinidos
  xLex.AddIdentSpecList('bit boolean byte word char', tnType);
  //funciones del sistema
  xLex.AddIdentSpecList('exit delay_ms Inc Dec Ord Chr', tnSysFunct);
  xLex.AddIdentSpecList('SetAsInput SetAsOutput SetBank', tnSysFunct);
  //símbolos especiales
  xLex.AddSymbSpec('+',  tnOperator);
  xLex.AddSymbSpec('-',  tnOperator);
  xLex.AddSymbSpec('*',  tnOperator);
  xLex.AddSymbSpec('/',  tnOperator);
  xLex.AddSymbSpec('\',  tnOperator);
//  xLex.AddSymbSpec('%',  tnOperator);
  xLex.AddSymbSpec('**', tnOperator);
  xLex.AddSymbSpec('=',  tnOperator);
  xLex.AddSymbSpec('>',  tnOperator);
  xLex.AddSymbSpec('<',  tnOperator);
  xLex.AddSymbSpec('>=', tnOperator);
  xLex.AddSymbSpec('<=', tnOperator);
  xLex.AddSymbSpec('<>', tnOperator);
  xLex.AddSymbSpec('<=>',tnOperator);
  xLex.AddSymbSpec(':=', tnOperator);
  xLex.AddSymbSpec('>>', tnOperator);
  xLex.AddSymbSpec('<<', tnOperator);
  xLex.AddSymbSpec(';', tnExpDelim);
  xLex.AddSymbSpec('(',  tnOthers);
  xLex.AddSymbSpec(')',  tnOthers);
  xLex.AddSymbSpec(':',  tnOthers);
  xLex.AddSymbSpec(',',  tnOthers);
  //crea tokens delimitados
  xLex.DefTokDelim('''','''', tnString);
  xLex.DefTokContent('[#]','[0-9]*', tnChar);
//  xLex.DefTokDelim('"','"', tnString);

  xLex.DefTokDelim('//','', xLex.tnComment);
  xLex.DefTokDelim('{','}', xLex.tnComment, tdMulLin);
  xLex.DefTokDelim('(\*','\*)', xLex.tnComment, tdMulLin);
  xLex.DefTokDelim('{$','}', tnDirective);
  xLex.DefTokDelim('Asm','End', tnAsm, tdMulLin);
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
  typByte :=CreateType('byte',t_uinteger,1);   //de 1 bytes
  //tipo numérico de dos byte
  typWord :=CreateType('word',t_uinteger,2);   //de 2 bytes
  //tipo caracter
  typChar :=CreateType('char',t_uinteger,1);   //de 1 byte. Se crea como uinteger para leer/escribir su valor como númeor

  {Los operadores deben crearse con su precedencia correcta
  Precedencia de operadores en Pascal:
  6)    ~, not, signo "-"   (mayor precedencia)
  5)    *, /, div, mod, and, shl, shr, &
  4)    |, !, +, -, or, xor
  3)    =, <>, <, <=, >, >=, in
  2)    :=                  (menor precedencia)
  }
  //////// Operaciones con Bit ////////////
  typBit.OnLoadToReg := @bit_LoadToReg;
  typBit.OnDefineRegister:=@bit_DefineRegisters;
  typBit.OnSaveToStk := @bit_SaveToStk;

  opr:=typBit.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typBit, @Oper_bit_asig_bit);
  opr.CreateOperation(typByte, @Oper_bit_asig_byte);

  opr:=typBit.CreateUnaryPreOperator('NOT', 6, 'not', @Oper_not_bit);

  opr:=typBit.CreateBinaryOperator('AND',4,'and');
  opr.CreateOperation(typBit,@Oper_bit_and_bit);
  opr.CreateOperation(typByte,@Oper_bit_and_byte);

  opr:=typBit.CreateBinaryOperator('OR',4,'or');
  opr.CreateOperation(typBit,@Oper_bit_or_bit);
  opr.CreateOperation(typByte,@Oper_bit_or_byte);

  opr:=typBit.CreateBinaryOperator('XOR',4,'or');
  opr.CreateOperation(typBit,@Oper_bit_xor_bit);
  opr.CreateOperation(typByte,@Oper_bit_xor_byte);

  opr:=typBit.CreateBinaryOperator('=',4,'equal');
  opr.CreateOperation(typBit,@Oper_bit_equ_bit);
  opr.CreateOperation(typByte,@Oper_bit_equ_byte);

  opr:=typBit.CreateBinaryOperator('<>',4,'difer');
  opr.CreateOperation(typBit,@Oper_bit_dif_bit);
  opr.CreateOperation(typByte,@Oper_bit_dif_byte);

  //////// Operaciones con Boolean ////////////
  typBool.OnLoadToReg:=@bit_LoadToReg;  //es lo mismo
  typBool.OnDefineRegister:=@bit_DefineRegisters;  //es lo mismo
  typBool.OnSaveToStk := @bit_SaveToStk;  //es lo mismo
  opr:=typBool.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typBool,@Oper_bool_asig_bool);

  opr:=typBool.CreateUnaryPreOperator('NOT', 6, 'not', @Oper_not_bool);

  opr:=typBool.CreateBinaryOperator('AND',4,'and');  //suma
  opr.CreateOperation(typBool,@Oper_bool_and_bool);

  opr:=typBool.CreateBinaryOperator('OR',4,'or');  //suma
  opr.CreateOperation(typBool,@Oper_bool_or_bool);

  opr:=typBool.CreateBinaryOperator('XOR',4,'or');  //suma
  opr.CreateOperation(typBool,@Oper_bool_xor_bool);

  opr:=typBool.CreateBinaryOperator('=',4,'equal');
  opr.CreateOperation(typBool,@Oper_bool_equ_bool);

  opr:=typBool.CreateBinaryOperator('<>',4,'difer');
  opr.CreateOperation(typBool,@Oper_bool_dif_bool);

  //////// Operaciones con Byte ////////////
  {Los operadores deben crearse con su precedencia correcta}
  typByte.OnLoadToReg:=@byte_LoadToReg;
  typByte.OnDefineRegister:=@byte_DefineRegisters;
  typByte.OnSaveToStk := @byte_SaveToStk;

  opr:=typByte.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typByte,@Oper_byte_asig_byte);
  opr:=typByte.CreateBinaryOperator('+',4,'suma');  //suma
  opr.CreateOperation(typByte,@Oper_byte_add_byte);
  opr.CreateOperation(typWord,@Oper_byte_add_word);
  opr:=typByte.CreateBinaryOperator('-',4,'resta');  //suma
  opr.CreateOperation(typByte,@Oper_byte_sub_byte);
  opr:=typByte.CreateBinaryOperator('AND',5,'and');  //suma
  opr.CreateOperation(typByte,@Oper_byte_and_byte);
  opr.CreateOperation(typBit ,@Oper_byte_and_bit);
  opr:=typByte.CreateBinaryOperator('OR',4,'or');  //suma
  opr.CreateOperation(typByte,@Oper_byte_or_byte);
  opr.CreateOperation(typBit,@Oper_byte_or_bit);
  opr:=typByte.CreateBinaryOperator('XOR',4,'xor');  //suma
  opr.CreateOperation(typByte,@Oper_byte_xor_byte);
  opr.CreateOperation(typBit,@Oper_byte_xor_bit);

  opr:=typByte.CreateBinaryOperator('=',3,'equal');
  opr.CreateOperation(typByte,@Oper_byte_equal_byte);
  opr:=typByte.CreateBinaryOperator('<>',3,'difer');
  opr.CreateOperation(typByte,@Oper_byte_difer_byte);
  opr.CreateOperation(typBit,@Oper_byte_difer_bit);

  opr:=typByte.CreateBinaryOperator('>',3,'great');
  opr.CreateOperation(typByte,@Oper_byte_great_byte);
  opr:=typByte.CreateBinaryOperator('<',3,'less');
  opr.CreateOperation(typByte,@Oper_byte_less_byte);

  opr:=typByte.CreateBinaryOperator('>=',3,'gequ');
  opr.CreateOperation(typByte,@Oper_byte_gequ_byte);
  opr:=typByte.CreateBinaryOperator('<=',3,'lequ');
  opr.CreateOperation(typByte,@Oper_byte_lequ_byte);

  opr:=typByte.CreateBinaryOperator('>>',3,'shr');  { TODO : Definir bien la precedencia }
  opr.CreateOperation(typByte,@Oper_byte_shr_byte);
  opr:=typByte.CreateBinaryOperator('<<',3,'shl');
  opr.CreateOperation(typByte,@Oper_byte_shl_byte);
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
  //////// Operaciones con Char ////////////
  {Los operadores deben crearse con su precedencia correcta}
  typChar.OnLoadToReg:=@byte_LoadToReg;  //es lo mismo
  typChar.OnDefineRegister:=@byte_DefineRegisters;  //es lo mismo
  typChar.OnSaveToStk := @byte_SaveToStk;  //es lo mismo

  opr:=typChar.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typChar,@Oper_char_asig_char);
  opr:=typChar.CreateBinaryOperator('=',3,'equal');  //asignación
  opr.CreateOperation(typChar,@Oper_char_equal_char);
  opr:=typChar.CreateBinaryOperator('<>',3,'difer');  //asignación
  opr.CreateOperation(typChar,@Oper_char_difer_char);

  //////// Operaciones con Word ////////////
  {Los operadores deben crearse con su precedencia correcta}
  typWord.OnLoadToReg:=@word_LoadToReg;
  typWord.OnDefineRegister:=@word_DefineRegisters;
  typWord.OnSaveToStk := @word_SaveToStk;

  opr:=typWord.CreateBinaryOperator(':=',2,'asig');  //asignación
  opr.CreateOperation(typWord,@Oper_word_asig_word);
  opr.CreateOperation(typByte,@Oper_word_asig_byte);

  opr:=typWord.CreateBinaryOperator('=',3,'equal');  //asignación
  opr.CreateOperation(typWord,@Oper_word_equal_word);
  opr:=typWord.CreateBinaryOperator('<>',3,'difer');
  opr.CreateOperation(typWord,@Oper_word_difer_word);
  opr:=typWord.CreateBinaryOperator('>',3,'difer');
  opr.CreateOperation(typWord,@Oper_word_great_word);

  opr:=typWord.CreateBinaryOperator('+',4,'suma');  //suma
  opr.CreateOperation(typWord,@Oper_word_add_word);
  opr.CreateOperation(typByte,@Oper_word_add_byte);

  opr:=typWord.CreateBinaryOperator('-',4,'subs');  //suma
  opr.CreateOperation(typWord,@Oper_word_sub_word);

  typWord.CreateField('Low', @word_Low);
  typWord.CreateField('High', @word_High);
end;
procedure TGenCod.CreateSystemElements;
{Inicia los elementos del sistema. Se ejecuta cada vez que se compila.}
var
  f: TxpEleFun;  //índice para funciones
begin
  //////// Funciones del sistema ////////////
  {Notar que las funciones del sistema no crean espacios de nombres.}
  f := CreateSysFunction('delay_ms', nil, @fun_delay_ms);
  f.adrr:=$0;
  f.compile := @codif_delay_ms;  //rutina de compilación
  f.OnAddCaller := @AddCaller;  //Para que lleve la cuenta de las llamadas a subrutinas
  f := CreateSysFunction('exit'     , nil, @fun_Exit);
  f := CreateSysFunction('Inc'      , nil, @fun_Inc);
  f := CreateSysFunction('Dec'      , nil, @fun_Dec);
  f := CreateSysFunction('Ord'      , @callParam, @fun_Ord);
  f := CreateSysFunction('Chr'      , @callParam, @fun_Chr);
  f := CreateSysFunction('Bit'      , @callParam, @fun_Bit);
  f := CreateSysFunction('SetAsInput' ,nil, @fun_SetAsInput);
  f := CreateSysFunction('SetAsOutput',nil, @fun_SetAsOutput);
  f := CreateSysFunction('Word'     , @callParam, @fun_Word);
  f := CreateSysFunction('SetBank'  , nil, @fun_SetBank);
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
    dicSet('")" expected.', 'Se esperaba ")"');
    dicSet('Invalid parameter type: %s','Tipo de parámetro inválido: %s');
  end;
  //  ER_NOT_IMPLEM_ := trans('Cannot increase a constant.', 'No se puede incrementar una constante.','','');
  //  ER_NOT_IMPLEM_ := trans('Cannot increase an expression.','No se puede incrementar una expresión.','','');
  //  ER_NOT_IMPLEM_ := trans('Cannot decrease a constant.', 'No se puede disminuir una constante.','','');
  //  ER_NOT_IMPLEM_ := trans('Cannot decrease an expression.','No se puede disminuir una expresión.','','');
  end;
end;
end.
